{-# LANGUAGE OverloadedStrings #-}
-- | Session ownership of captured builds, settings, compiler discovery and consoles.
--
-- Provider retirement releases only its own terminal IDs. These services outlive
-- Conversation, and prepared builds still re-enter the full runtime authority
-- boundary before execution. Process acquisition/adoption and cancellation keep
-- their existing one-slot lifetimes; this module adds no dispatcher or queue.
module Hide.SessionServices
  ( SessionServices, withSessionServices, sessionDirectory, sessionConsoles
  , sessionBuildJobs, sessionEffects, tickSessionServices, tickBuildPreparation
  , withBuildAdmission, stopSessionBuild, buildTerminalLaunchPending, persist
  ) where

import Control.Exception (IOException, bracket, try, onException, mask, mask_, evaluate)
import Control.Concurrent.Async (Async, async, asyncWithUnmask, cancel, poll, wait, waitCatch)
import Control.Monad (foldM, forM, forM_, void, when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (findIndex)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import System.Directory (XdgDirectory(..), getXdgDirectory, createDirectoryIfMissing, canonicalizePath, renameFile, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.Info (os)
import System.IO (openBinaryTempFile, hClose)
import Text.Read (readMaybe)
import qualified Hide.Terminal as Terminal
import qualified Hide.Consoles as C
import qualified Hide.Compilers as Compilers
import qualified Hide.Build as B
import qualified Hide.BuildJobs as Jobs
import Hide.PackageSidebar (packageBuildManifestCurrent)
import Hide.MCPPermissions (AdmittedBuild, reserveAdmittedBuild, stepAdmittedBuild, cancelAdmittedBuild)
import Hide.Plugin.BufferHost (ContentVersion, captureVersion, versionCurrent)
import Hide.Files (filePath)
import Hide.Buffer
import Hide.Model
import Hide.Sidebar (treeRoot)

-- One caller-bound build intent, captured without retaining editable buffers or Undo.
data BuildReceipt = BuildReceipt !Int !FilePath !(Maybe FilePath) ![(Int,Maybe FilePath,ContentVersion)] !(Maybe AdmittedBuild) !(Maybe PackageBuildTarget)
data PreparedBuild = BuildOptions !Dialog
  | BuildCommands !B.BuildAction !FilePath ![(FilePath,[String])]
  | BuildConsole !C.PreparedConsole
  | BuildUnsaved !B.BuildAction
data BuildPreparation = BuildPreparing !BuildReceipt !Bool !(Async (Either Text PreparedBuild))
  | BuildReady !BuildReceipt !Bool !(Either Text PreparedBuild)
  | BuildRetiring !Bool !(Async ())

data ServiceState = ServiceState
  { compilerDiscovery :: Maybe (Async [Compilers.Compiler])
  , buildSettingsCache :: Maybe Toolchain, buildSettingsVersion :: Int
  , buildSettingsWorker :: Maybe (Int,Async Toolchain), buildSettingsChecked :: Maybe UTCTime
  , buildAdmission :: Maybe AdmittedBuild
  , buildPreparation :: Maybe BuildPreparation
  , shellLaunches :: [Async (Either Text C.PreparedConsole)]
  }

-- | One session owns these process handles independently of its ACP provider.
data SessionServices = SessionServices !FilePath !C.Consoles !Jobs.BuildJobs !(IORef ServiceState)

-- | Acquire services outside provider/frontend scopes. Teardown joins pending
-- acquisition cleanup before closing captured jobs and then adopted consoles.
withSessionServices :: (SessionServices -> IO a) -> IO a
withSessionServices action=C.withConsoles $ \consoles -> Jobs.withBuildJobs $ \jobs -> do
  directory<-getXdgDirectory XdgConfig "thc-edit"
  ref<-newIORef (ServiceState Nothing Nothing 0 Nothing Nothing Nothing Nothing [])
  bracket (pure (SessionServices directory consoles jobs ref)) closeSessionServices action

-- | The session's captured settings directory; no filesystem work.
sessionDirectory :: SessionServices -> FilePath
sessionDirectory (SessionServices directory _ _ _)=directory
-- | Shared console ownership, including terminals created by separate owners.
sessionConsoles :: SessionServices -> C.Consoles
sessionConsoles (SessionServices _ consoles _ _)=consoles
-- | The session's single captured job/output owner.
sessionBuildJobs :: SessionServices -> Jobs.BuildJobs
sessionBuildJobs (SessionServices _ _ jobs _)=jobs

closeSessionServices :: SessionServices -> IO ()
closeSessionServices (SessionServices _ _ _ ref)=do
  state<-readIORef ref
  mapM_ closeBuildPreparation (buildPreparation state)
  mapM_ (cancel . snd) (buildSettingsWorker state)
  mapM_ cancel (compilerDiscovery state)
  forM_ (shellLaunches state) $ \worker -> do
    cancel worker
    result<-waitCatch worker
    case result of Right (Right prepared)->C.closePreparedConsole prepared; _->pure ()

-- | Consume only session operations; the caller supplies the existing full
-- runtime interpreter for later prepared-build adoption.
sessionEffects :: SessionServices -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
sessionEffects runtime fallback=foldM apply . (False,)
  where
    apply state@(True,_) _=pure state
    apply (_,d) (ServiceAction action values)=(False,) <$> perform runtime action values d
    apply (_,d) (PackageBuildAction action target)=(False,) <$> startPackageBuildPreparation runtime action target d
    apply (_,d) (AdoptPreparedBuild target)=(False,) <$> adoptBuildPreparation runtime target d
    apply (_,d) effect=fallback d [effect]

-- | Poll shared presentation before provider events. Build adoption is a
-- separate phase so it can re-enter the caller's complete runtime gates.
tickSessionServices :: SessionServices -> Desktop -> IO Desktop
tickSessionServices runtime@(SessionServices directory consoles jobs ref) d=
  pollShellLaunches runtime d >>= pollBuildSettings directory ref >>= pollCompilerMenu runtime >>= C.tickConsoles consoles >>= Jobs.tickBuildJobs jobs

-- Temp files start private; never persist environment overrides to the project.
persist :: FilePath -> Value -> IO (Either Text ())
persist path value = do
  result<-try $ do
    createDirectoryIfMissing True (takeDirectory path)
    bracket (openBinaryTempFile (takeDirectory path) ".thc-settings-")
      (\(temporary,handle) -> ignore (hClose handle) >> ignore (removeFile temporary)) $ \(temporary,handle) -> do
        BL.hPut handle (encode value); hClose handle; renameFile temporary path
  pure (either (Left . T.pack . show) Right (result :: Either IOException ()))
  where ignore task=void (try task :: IO (Either IOException ()))

-- Keep backend settings independently. The flat selected record remains readable
-- by existing build/run clients; never save loadBuildConfig's root-filtered view
-- merely to switch backend. Legacy flat run.json becomes the first saved choice.
readRunSettings :: FilePath -> IO Value
readRunSettings directory = do
  result<-try (BS.readFile (directory </> "run.json")) :: IO (Either IOException BS.ByteString)
  pure (either (const (object [])) (fromMaybe (object []) . decodeStrict') result)

buildChoices :: Value -> M.Map Text Value
buildChoices saved = M.insert (fromMaybe "THC" (field "toolchain" saved)) (flat saved)
  (fromMaybe M.empty (field "toolchains" saved))
  where flat (Object fields')=Object (KM.delete "toolchains" fields'); flat _=object []

rememberBuildChoices :: Value -> Value -> Value
rememberBuildChoices saved selected = case combined of
  Object fields' -> Object (KM.insert "toolchains" (toJSON choices) fields')
  _ -> combined
  where
    name=fromMaybe "THC" (field "toolchain" selected)
    previous=M.findWithDefault (object []) name (buildChoices saved)
    combined=mergeSettings previous selected
    choices=M.insert name combined (buildChoices saved)

mergeSettings :: Value -> Value -> Value
mergeSettings (Object old) (Object new)=Object (KM.union new old)
mergeSettings _ new=new

-- One bounded discovery worker belongs to this session. Reopening
-- while it runs shares the catalogue query; results never reopen a closed menu.
openCompilerMenu :: SessionServices -> Desktop -> IO Desktop
openCompilerMenu (SessionServices directory _ _ ref) d
  | dialog d/=Nothing = pure d
  | otherwise = do
      mask $ \restore -> do
        current<-readIORef ref
        when (isNothing (compilerDiscovery current)) $ do
          worker<-async (restore Compilers.installedCompilers)
          modifyIORef' ref (\state -> state {compilerDiscovery=Just worker})
      saved<-readRunSettings directory
      pure (compilerMenu False saved [] d) {status="Finding installed GHC compilers..."}

pollCompilerMenu :: SessionServices -> Desktop -> IO Desktop
pollCompilerMenu (SessionServices directory _ _ ref) d = do
  current<-readIORef ref
  result<-maybe (pure Nothing) poll (compilerDiscovery current)
  case result of
    Nothing -> pure d
    Just outcome -> do
      modifyIORef' ref (\state -> state {compilerDiscovery=Nothing})
      case contextKind d of
        ToolchainContext _ | contextMenu d/=Nothing,dialog d==Nothing -> do
          saved<-readRunSettings directory
          let compilers=either (const []) id outcome
          pure (compilerMenu True saved compilers d) {status=if null compilers then "Automatic uses project settings; no GHCup compilers found." else "Select an installed GHC or Automatic."}
        _ -> pure d

compilerMenu :: Bool -> Value -> [Compilers.Compiler] -> Desktop -> Desktop
compilerMenu preserve saved compilers d = opened {contextMenu=fmap (\(r,_) -> (r,chosen)) (contextMenu opened)}
  where
    ghc=M.findWithDefault (object []) "GHC" (buildChoices saved)
    command=fromMaybe "ghc" (field "command" ghc)
    selected=if (field "toolchain" saved :: Maybe Text)==Just "GHC" then SelectCompiler command else SelectToolchain THC
    installed=[("GHC "<>Compilers.compilerVersion compiler,SelectCompiler (T.pack (Compilers.compilerPath compiler))) | compiler<-compilers]
    custom=[("GHC saved compiler",SelectCompiler command) | command/="ghc",SelectCompiler command `notElem` map snd installed]
    entries=[("THC",SelectToolchain THC),("GHC Automatic",SelectCompiler "ghc")]++installed++custom++[("Target settings...",RunOptions)]
    rows=[(if action==selected then "✓ "<>label else "  "<>label,action) | (label,action)<-entries]
    previous=case (preserve,contextMenu d) of
      (True,Just (_,index)) -> snd <$> listToMaybe (drop index (contextItemsFor d))
      _ -> Nothing
    chosen=fromMaybe 0 (findIndex ((==fromMaybe selected previous).snd) entries)
    Rect x y _ _=toolchainBadgeRect d
    opened=openContext (ToolchainContext rows) x y d

perform :: SessionServices -> Text -> [Text] -> Desktop -> IO Desktop
perform (SessionServices _ _ _ ref) "execute-shell-block" [bidText,startText,endText,dialect,body] d
  | Just bid<-readMaybe (T.unpack bidText), Just blockStart<-readMaybe (T.unpack startText), Just blockEnd<-readMaybe (T.unpack endText),
    Just doc<-M.lookup bid (buffers d), (blockStart,blockEnd,dialect,body) `elem` documentShellBlocks doc =
      if not Terminal.terminalAvailable then pure (message "Cannot execute shell block" ["Embedded terminals are unavailable in this build."] d)
      else if T.null (T.strip body) then pure (message "Cannot execute shell block" ["This shell block is empty."] d)
      else if dialect `notElem` ["sh","bash","zsh"] then pure (message "Cannot execute shell block" ["This shell dialect is unavailable."] d)
      else mask_ $ do
        worker<-async $ mask_ $ do
          root<-B.resolveBuildRoot d
          C.prepareConsole [] (Terminal.TerminalConfig (T.unpack dialect) ["-c",T.unpack body] [] root 80 24) (1024*1024)
        modifyIORef' ref (\state->state {shellLaunches=shellLaunches state++[worker]})
        pure d {status="Starting shell block in terminal..."}
perform _ "execute-shell-block" _ d = pure (message "Cannot execute shell block" ["The code block changed; open its context menu again."] d)

perform runtime@(SessionServices directory consoles _ ref) action values d=case (action,values) of
    ("terminal",_) -> do
      shell<-if os=="mingw32" then fromMaybe "cmd.exe" <$> lookupEnv "COMSPEC" else fromMaybe "/bin/sh" <$> lookupEnv "SHELL"
      root<-canonicalizePath (maybe (startingDirectory d) treeRoot (sideTree d))
      openConsole consoles (Terminal.TerminalConfig shell [] [] root 80 24) d
    ("run",_) -> startBuildPreparation runtime (Just B.Run) d
    ("compile",_) -> startBuildPreparation runtime (Just B.Compile) d
    ("make",_) -> startBuildPreparation runtime (Just B.Make) d
    ("build-stop",_) -> stopSessionBuild runtime d
    ("toolchain",[]) -> openCompilerMenu runtime d
    ("toolchain",choice:commands) | choice `elem` ["THC","GHC"],length commands<=1,
      all (\command -> not (T.null command) && T.length command<=4096 && not (T.any (== '\0') command)) commands -> do
      saved<-readRunSettings directory
      let selected=if choice=="GHC" then B.GHC else B.THC
          defaults=object ["toolchain" .= choice,"command" .= (if selected==B.GHC then "ghc" else "thc"::Text)]
          previousChoice=M.findWithDefault defaults choice (buildChoices saved)
          chosen=mergeSettings previousChoice (object (["toolchain" .= choice]++["command" .= command | command<-commands]))
      result<-persist (directory </> "run.json") (rememberBuildChoices saved chosen)
      when (result==Right ()) (buildSettingsChanged ref selected)
      pure $ either (\err -> d {status=err}) (const d {toolchain=Just selected,status=choice<>" selected. F9 builds; Ctrl+F9 runs."}) result
    ("run-options",_) -> startBuildPreparation runtime Nothing d
    ("run-config",_:settings) -> case B.parseBuildConfig settings of
      Left err -> pure (message "Build target" [err] d)
      Right config -> do
        root<-B.resolveBuildRoot d
        saved<-readRunSettings directory
        result<-persist (directory </> "run.json") (rememberBuildChoices saved (B.buildConfigValue root config))
        when (result==Right ()) (buildSettingsChanged ref (B.buildToolchain config))
        pure $ either (\err -> d {status=err}) (const d {toolchain=Just (B.buildToolchain config),status="Target saved. F9 builds; Ctrl+F9 runs."}) result
    ("terminal-input",[tid,text]) -> do
      result<-C.inputConsole consoles tid (TE.encodeUtf8 text)
      pure (either (\err -> d {status=err}) (const d) result)
    ("terminal-stop",_) -> case activeDocument d >>= documentLabel >>= T.stripPrefix "Terminal " of
      Just tid -> do result<-C.killConsole consoles tid; pure d {status=either id (const "Terminal command stopped.") result}
      Nothing -> pure d {status="Select a terminal window first."}
    _ -> pure d {status="Unknown session service action."}

-- The badge uses only the global toolchain field, so it need not resolve a
-- project or load its targets. One worker refreshes at most once per second.
-- Explicit saves advance the version so an older read cannot undo that choice.
pollBuildSettings :: FilePath -> IORef ServiceState -> Desktop -> IO Desktop
pollBuildSettings directory ref d = mask $ \restore -> do
  now<-getCurrentTime
  s<-readIORef ref
  forM_ (buildSettingsWorker s) $ \(version,worker) -> do
    result<-poll worker
    forM_ result $ \outcome -> modifyIORef' ref (\state -> state
      {buildSettingsWorker=Nothing,
       buildSettingsCache=if version==buildSettingsVersion state
         then either (const (buildSettingsCache state)) Just outcome else buildSettingsCache state})
  current<-readIORef ref
  when (isNothing (buildSettingsWorker current) && maybe True (\checked -> diffUTCTime now checked>=1) (buildSettingsChecked current)) $ do
    worker<-async (restore (B.buildToolchain <$> B.loadBuildConfig directory ""))
    modifyIORef' ref (\state -> state {buildSettingsWorker=Just (buildSettingsVersion state,worker),buildSettingsChecked=Just now})
  latest<-readIORef ref
  pure d {toolchain=Just (fromMaybe (fromMaybe THC (toolchain d)) (buildSettingsCache latest))}

-- Acquisition owns each child until the UI adopts it. Session teardown cancels
-- pending launches and closes any completed child that has not been adopted.
pollShellLaunches :: SessionServices -> Desktop -> IO Desktop
pollShellLaunches (SessionServices _ consoles _ ref) original = mask_ $ do
  state<-readIORef ref
  results<-mapM (\worker->(worker,) <$> poll worker) (shellLaunches state)
  modifyIORef' ref (\current->current {shellLaunches=[worker | (worker,Nothing)<-results]})
  foldM adopt original [result | (_,Just result)<-results]
  where
    adopt d (Right (Right prepared)) = snd <$> C.adoptConsole consoles prepared d `onException` C.closePreparedConsole prepared
    adopt d (Right (Left err)) = pure (message "Cannot execute shell block" (wrapMessage err) d)
    adopt d (Left _) = pure (message "Cannot execute shell block" ["Terminal launch failed."] d)

openConsole :: C.Consoles -> Terminal.TerminalConfig -> Desktop -> IO Desktop
openConsole consoles config d = do
  result<-C.startConsole consoles config (1024*1024) d
  pure (either (\err -> message "Cannot start terminal" (wrapMessage err) d) snd result)

-- | Bind only the serialized admitted guest-input invocation. Human commands
-- have no admission; asynchronous work captures the one-shot receipt explicitly.
withBuildAdmission :: SessionServices -> AdmittedBuild -> IO a -> IO a
withBuildAdmission (SessionServices _ _ _ ref) admission action=bracket
  (atomicModifyIORef' ref (\state->(state {buildAdmission=Just admission},buildAdmission state)))
  (\previous->modifyIORef' ref (\state->state {buildAdmission=previous}))
  (const action)

-- Capture paths and exact immutable source identities on the serialized owner.
-- Dirty representation comparisons and all filesystem/planning work run outside it.
startBuildPreparation :: SessionServices -> Maybe B.BuildAction -> Desktop -> IO Desktop
startBuildPreparation runtime action=startBuildPreparationAt runtime action Nothing

startPackageBuildPreparation :: SessionServices -> B.BuildAction -> PackageBuildTarget -> Desktop -> IO Desktop
startPackageBuildPreparation runtime action target=startBuildPreparationAt runtime (Just action) (Just target)

startBuildPreparationAt :: SessionServices -> Maybe B.BuildAction -> Maybe PackageBuildTarget -> Desktop -> IO Desktop
startBuildPreparationAt (SessionServices directory _ _ ref) action target d = mask_ $ do
  state<-readIORef ref
  case buildPreparation state of
    Just _ -> pure d {status="Build preparation is already pending."}
    Nothing -> do
      admitted<-maybe (pure True) reserveAdmittedBuild (buildAdmission state)
      if not admitted then pure d {status="Admitted input can start only one build intent."} else do
        captured<-forM (buildSourceDocuments d) $ \(bid,doc) -> do
          version<-captureVersion (documentBuffer doc)
          snapshot<-evaluate (captureDirty (documentBuffer doc))
          let path=filePath <$> documentFile doc
          mapM_ (evaluate . length) path
          pure ((bid,path,version),snapshot)
        let start=B.buildStartDirectory d
            source=B.buildSource d
            receipt=BuildReceipt (buildSettingsVersion state) start source (map fst captured) (buildAdmission state) target
            snapshots=map snd captured
        -- Evaluate pathname selectors here; the worker never captures Desktop
        -- or FileState through an unevaluated source/directory field.
        _<-evaluate (length start)
        mapM_ (evaluate . length) source
        worker<-asyncWithUnmask (\unmask -> unmask (prepareBuild directory action target start source snapshots))
        modifyIORef' ref (\current -> current {buildPreparation=Just (BuildPreparing receipt False worker)})
        pure d {status="Preparing build target…"}

buildSourceDocuments :: Desktop -> [(Int,Document)]
buildSourceDocuments d=[(bid,doc) | (bid,doc)<-M.toList (buffers d),documentLabel doc==Nothing]

prepareBuild :: FilePath -> Maybe B.BuildAction -> Maybe PackageBuildTarget -> FilePath -> Maybe FilePath -> [DirtySnapshot] -> IO (Either Text PreparedBuild)
prepareBuild directory action target start source snapshots = do
  result<-try $ do
    root<-maybe (B.resolveBuildRootFrom start) (pure . packageBuildRoot) target
    saved<-B.loadBuildConfig directory root
    let config=maybe saved (\component->saved {B.buildTarget=packageBuildName component}) target
    before<-maybe (pure True) packageBuildManifestCurrent target
    unsaved<-case action of Nothing->pure False; Just _->evaluate (any snapshotDirty snapshots)
    prepared<-if not before then pure (Left "Package build target changed during preparation.") else case action of
      Nothing -> do
        let options=buildOptions root config
        _<-evaluate (sum [T.length value | Input _ value _<-fields options])
        pure (Right (BuildOptions options))
      Just task | unsaved -> pure (Right (BuildUnsaved task))
      Just task -> fmap (BuildCommands task root) <$> B.buildPlan task config root (if isNothing target then source else Nothing)
    -- Strings/argv may otherwise retain planning thunks until execution on owner.
    _<-evaluate (length root+length (B.buildExecutable config)+T.length (B.buildTarget config)+
      T.length (B.buildTHCRoot config)+T.length (B.buildRuntime config)+sum (map length (B.buildArguments config)))
    after<-maybe (pure True) packageBuildManifestCurrent target
    _<-case prepared of
      Left err -> evaluate (T.length err) >> pure prepared
      Right (BuildCommands _ _ commands) -> evaluate (sum [length cmd+sum (map length args) | (cmd,args)<-commands]) >> pure prepared
      _ -> pure prepared
    pure (if after then prepared else Left "Package changed while preparing the build target.")
  pure (either (Left . T.take 512 . T.pack . show) id (result :: Either IOException (Either Text PreparedBuild)))

buildReceiptCurrent :: IORef ServiceState -> BuildReceipt -> Desktop -> IO Bool
buildReceiptCurrent ref (BuildReceipt version start source expected _ _) d = do
  state<-readIORef ref
  let current=buildSourceDocuments d
      names=[(bid,filePath <$> documentFile doc) | (bid,doc)<-current]
  if buildSettingsVersion state/=version || B.buildStartDirectory d/=start || B.buildSource d/=source ||
     names/=[(bid,path) | (bid,path,_)<-expected]
    then pure False
    else and <$> sequence [versionCurrent receipt (documentBuffer doc) | ((_,_,receipt),(_,doc))<-zip expected current]

-- | Poll the single build preparation lifetime. Completed work re-enters the
-- same runtime/Git boundary once only. Execution authority was admitted with
-- the original command; this re-entry grants no fresh MCP approval. A modal
-- defers adoption; stale settings/source receipts retire planning without
-- execution. After the checked launch gate starts a terminal process, ordinary
-- edits/settings saves do not cancel it; Stop and session teardown still do.
tickBuildPreparation :: SessionServices -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO Desktop
tickBuildPreparation runtime@(SessionServices _ _ _ ref) core d = mask_ $ do
  state<-readIORef ref
  case buildPreparation state of
    Nothing -> pure d
    Just (BuildRetiring _ worker) -> do
      done<-poll worker
      when (not (isNothing done)) (modifyIORef' ref (\current -> current {buildPreparation=Nothing}))
      pure d
    Just (BuildPreparing receipt launch worker) -> do
      current<-if launch then pure True else buildReceiptCurrent ref receipt d
      if not current then retireBuildPreparation ref >> pure d {status="Build preparation cancelled: source or settings changed."}
      else do
        result<-poll worker
        case result of
          Nothing -> pure d
          Just outcome -> do
            let prepared=either (const (Left "Build preparation failed.")) id outcome
            modifyIORef' ref (\s -> s {buildPreparation=Just (BuildReady receipt launch prepared)})
            tickBuildPreparation runtime core d
    Just (BuildReady receipt@(BuildReceipt _ _ _ _ admission target) launch _) -> do
      current<-if launch then pure True else buildReceiptCurrent ref receipt d
      if not current then retireBuildPreparation ref >> pure d {status="Build preparation cancelled: source or settings changed."}
      else do
        let blocked=not (isNothing (dialog d)) || questionActive d
        result<-case admission of
          Just receiptAdmission | not launch->stepAdmittedBuild receiptAdmission core d
          _ | blocked->pure Nothing
            | otherwise->Just . snd <$> core d [AdoptPreparedBuild (if launch then Nothing else target)]
        case result of
          Nothing->pure d
          Just updated->do
            remaining<-buildPreparation <$> readIORef ref
            -- A refusing gate does not see the slot. Consume that refused result;
            -- it must not launch later when the gate becomes permissive.
            case remaining of Just BuildReady{} -> retireBuildPreparation ref; _->pure ()
            pure updated

adoptBuildPreparation :: SessionServices -> Maybe PackageBuildTarget -> Desktop -> IO Desktop
adoptBuildPreparation (SessionServices _ consoles jobs ref) target d = mask_ $ do
  state<-readIORef ref
  case buildPreparation state of
    Just (BuildReady receipt launch result) -> do
      current<-if launch then pure True else buildReceiptCurrent ref receipt d
      if not current || target/= (if launch then Nothing else case receipt of BuildReceipt _ _ _ _ _ captured->captured) || not (isNothing (dialog d)) || questionActive d then retireBuildPreparation ref >> pure d
      else do
        modifyIORef' ref (\s -> s {buildPreparation=Nothing})
        case result of
          Left err -> pure (message "Build target" [err] d)
          Right (BuildOptions options) -> pure d {dialog=Just options}
          Right (BuildUnsaved task) -> pure (message (if task==B.Run then "Save before running" else "Save before building")
            ["Save modified source files before building the files on disk."] d)
          Right (BuildCommands task root [(command,args)]) | task==B.Run && Terminal.terminalAvailable -> do
            -- Keep prepare-to-publication masked, as for shell launch ownership:
            -- cancellation must not lose a successfully created console handle.
            worker<-async $ mask_ $
              fmap BuildConsole <$> C.prepareConsole [] (Terminal.TerminalConfig command args [] root 80 24) (1024*1024)
            modifyIORef' ref (\s -> s {buildPreparation=Just (BuildPreparing receipt True worker)})
            pure d {status="Starting terminal…"}
          Right (BuildCommands task root commands) -> Jobs.startBuildJob jobs (T.pack (show task)) root commands d
          Right (BuildConsole console) -> snd <$> C.adoptConsole consoles console d `onException` (do
            modifyIORef' ref (\s -> s {buildPreparation=Just (BuildReady receipt True (Right (BuildConsole console)))})
            retireBuildPreparation ref)
    _ -> pure d

-- Successful in-app run.json writes share this invalidation boundary. External
-- file edits retain the worker's captured settings snapshot for this intent.
buildSettingsChanged :: IORef ServiceState -> Toolchain -> IO ()
buildSettingsChanged ref selected=mask_ $ do
  modifyIORef' ref (\state -> state {buildSettingsCache=Just selected,
    buildSettingsVersion=buildSettingsVersion state+1,buildSettingsChecked=Nothing})
  slot<-buildPreparation <$> readIORef ref
  -- Once the checked gate starts Run, editing/saving is ordinary program use.
  -- Only planning still depends on the captured source/settings receipt.
  case slot of
    Just (BuildPreparing _ False _) -> retireBuildPreparation ref
    Just (BuildReady _ False _) -> retireBuildPreparation ref
    _ -> pure ()

-- | Request cancellation of pending command preparation and the shared captured
-- job. Both retain their owner until asynchronous cleanup; no UI join is required.
stopSessionBuild :: SessionServices -> Desktop -> IO Desktop
stopSessionBuild (SessionServices _ _ jobs ref) d=do
  pending<-not . isNothing . buildPreparation <$> readIORef ref
  retireBuildPreparation ref
  stopped<-Jobs.stopBuildJob jobs d
  pure (if pending then stopped {status="Stopping build preparation…"} else stopped)

-- | Cheap cross-owner reservation for terminal launch only. Prepared terminals
-- remain owned until adoption or cancellation cleanup; adopted consoles retain
-- the existing independent execution policy.
buildTerminalLaunchPending :: SessionServices -> IO Bool
buildTerminalLaunchPending (SessionServices _ _ _ ref) = do
  state<-readIORef ref
  pure $ case buildPreparation state of
    Just (BuildPreparing _ launch _) -> launch
    Just (BuildReady _ launch _) -> launch
    Just (BuildRetiring launch _) -> launch
    Nothing -> False

retireBuildPreparation :: IORef ServiceState -> IO ()
retireBuildPreparation ref = mask_ $ do
  state<-readIORef ref
  case buildPreparation state of
    Nothing -> pure ()
    Just BuildRetiring{} -> pure ()
    Just slot -> do
      let launch=case slot of BuildPreparing _ active _->active; BuildReady _ active _->active; _->False
      worker<-asyncWithUnmask (\unmask -> unmask (closeBuildPreparation slot))
      modifyIORef' ref (\s -> s {buildPreparation=Just (BuildRetiring launch worker)})

closeBuildPreparation :: BuildPreparation -> IO ()
closeBuildPreparation slot=do
  case slot of
    BuildPreparing receipt _ _->cancelReceipt receipt
    BuildReady receipt _ _->cancelReceipt receipt
    _->pure ()
  case slot of
    BuildPreparing _ _ worker -> do
      cancel worker
      result<-waitCatch worker
      case result of Right prepared->closeResult prepared; _->pure ()
    BuildReady _ _ prepared -> closeResult prepared
    BuildRetiring _ worker -> wait worker
  where
    cancelReceipt (BuildReceipt _ _ _ _ admission _)=mapM_ cancelAdmittedBuild admission
    closeResult (Right (BuildConsole console))=C.closePreparedConsole console
    closeResult _=pure ()

-- Docs: docs/site/screenshots/build-target.png (docs/running.md) shows the target dialog.
buildOptions :: FilePath -> B.BuildConfig -> Dialog
buildOptions root config=Dialog "Build target" (ServiceDialog "run-config")
  [setting "Compiler executable" (T.pack (B.buildExecutable config)),setting "Cabal target (optional)" (B.buildTarget config),
   setting "THC root (optional)" (B.buildTHCRoot config),setting "Runtime (THC only)" (B.buildRuntime config),
   ListBox "Toolchain" ["THC","GHC"] (if B.buildToolchain config==B.THC then 0 else 1),
   setting "Program arguments (JSON)" (jsonText (B.buildArguments config))]
  0 ["OK","Cancel"] ["F9 Make   Alt+F9 Compile   Ctrl+F9 Run",T.pack root]
  where setting label text=Input label text (T.length text)

field :: FromJSON a => Text -> Value -> Maybe a
field name=parseMaybe (withObject "object" (.: K.fromText name))
jsonText :: ToJSON a => a -> Text
jsonText=TE.decodeUtf8 . BL.toStrict . encode
