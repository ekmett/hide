{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
-- | DAP orchestration, editor inspection views and debugger terminal ownership.
--
-- Stopped-state generations scope stack and variable handles; late replies cannot
-- update a newer stop. Inspection distinguishes lazy references because expanding
-- them may execute target code. Replacements retire transport/consoles before
-- reusing endpoints. Consented hdb acquisition retains the exact launch context
-- and cannot revive a superseded launch after installation completes.
-- Output has one exact semantic window slot and one coalesced preparation worker.
-- Independent session/publication receipts reject late content without inspecting
-- unrelated Documents. Replacement freezes the prior snapshot until prepared
-- content adopts into the same display slot; closing never reopens from output.
module Hide.Debugger (Debugger, Core, withDebugger, withDebuggerConsoles, withDownloadsCommands, withDebuggerClock, withDebuggerHdb, hdbOfferDialog, debuggerEffects, tickDebugger, tickPreparedDebug, debuggerTool, debuggerSidebarEpoch, debuggerSidebarSession, debuggerSidebarRead, debuggerWatches, withDebuggerWatchProvider) where

import Hide.FileIO (withFileRead)

import qualified Hide.Plugin.Window as W
import qualified Hide.Plugin.Menu as Menu
import Hide.PluginWindowHost (adoptWindowUpdate,replaceWindowUpdate)
import Hide.Sidebar
import qualified Hide.Plugin.Tree as P
import Hide.DebuggerSidebarTypes
import Control.Concurrent (MVar, newEmptyMVar, tryPutMVar, tryReadMVar, threadDelay)
import Control.Exception (IOException, SomeException, SomeAsyncException, fromException, throwIO, catch, bracket, try, evaluate, mask, mask_, finally)
import Control.Concurrent.Async (Async, async, asyncWithUnmask, cancel, poll, race, waitCatch)
import Control.Concurrent.STM
import qualified Hide.Downloads as Downloads
import Hide.DownloadsWindowTypes
import Hide.MenuCommands (MenuHost,MenuContext(..),MenuReply(..),menuContributions)
import Hide.Plugin.Command
import qualified Hide.DownloadsWindow as DownloadsWindow
import qualified Hide.HdbAcquisition as Hdb
import Control.Monad (foldM, filterM, forM_, unless, when, void)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (XdgDirectory(XdgConfig), canonicalizePath, doesFileExist, getXdgDirectory, removeFile)
import System.IO (openTempFile, hClose)
import System.IO.Error (tryIOError)
import Hide.PackageSidebar (packageBuildManifestCurrent)
import System.FilePath (isAbsolute, takeFileName, takeExtension, takeDirectory, makeRelative, (</>))
import System.Timeout (timeout)
import System.Mem.StableName
import Text.Read (readMaybe)
import qualified Hide.Compilers as Compilers
import qualified Hide.Build as Build
import Hide.Build (resolveBuildRoot)
import Hide.Buffer
import qualified Hide.Consoles as C
import qualified Hide.Terminal as Terminal
import qualified Hide.DAP as D
import Hide.Files (FileState(..),loadFile)
import Hide.Plugin.BufferHost (ContentVersion,captureVersion,versionCurrent)
import Hide.GuestAccess (protectedBuffer,protectedPath,protectedFilePath)
import qualified Hide.LSP as L
import Hide.Model
import Hide.BufferView (BufferView(..))

-- One debugger-lifetime preparation worker; its desired snapshot and reply are
-- replaceable, not an event history. Session epochs differ from stopped handles.
data OutputOwner = OutputOwner !W.WindowScope !(IORef OutputSlot)
  !(TVar (Maybe OutputRequest)) !(TMVar OutputPublication) !(Async ())
data OutputSlot = OutputSlot !Int !Int !(Maybe W.WindowRef) !Bool !Bool !(Maybe W.WindowRef)
-- The two flags request a pending open or deferred explicit refocus.
-- Capping/concatenating output is deliberately lazy until the worker prepares
-- a copied bounded report; request admission forces only the small receipt.
-- Until that receipt returns, the existing bounded DAP inbox holds later events.
data OutputRequest = OutputRequest !Int !Int !(Maybe W.WindowRef) !Bool Text
data OutputPublication = OutputPublication !Int !Int !(Maybe W.WindowRef)
  !(Either Text Text) !(Maybe (Either Text W.WindowUpdate))

newOutputOwner :: W.WindowScope -> IO OutputOwner
newOutputOwner scope=mask_ $ do
  slot<-newIORef (OutputSlot 0 0 Nothing False False Nothing)
  desired<-newTVarIO Nothing
  latest<-newEmptyTMVarIO
  worker<-asyncWithUnmask (\unmask->unmask (loop desired latest))
  pure (OutputOwner scope slot desired latest worker)
  where
    loop desired latest=do
      OutputRequest epoch revision target prepareView value<-atomically $ do
        next<-readTVar desired
        maybe retry (\request->writeTVar desired Nothing >> pure request) next
      report<-(Right <$> evaluate (T.copy (T.takeEnd 16384 value))) `catch` synchronous "Debugger output preparation failed."
      result<-case report of
        Left _->pure Nothing
        Right _ | not prepareView->pure Nothing
        Right bounded->Just <$> ((do
          prepared<-W.prepareRecoverableTextWindow "hide.debug-output" 1 W.PrivateWindow "Debugger output" bounded
          case prepared of
            Left err->pure (Left err)
            Right snapshot->do
              update<-maybe (W.openWindow scope snapshot) (\reference->W.refreshWindow reference snapshot) target
              pure (maybe (Left "Debugger output view closed.") Right update))
          `catch` synchronous "Debugger output view preparation failed.")
      previous<-atomically $ do
        old<-tryTakeTMVar latest
        putTMVar latest (OutputPublication epoch revision target report result)
        pure old
      mapM_ retireOutputOpening previous
      loop desired latest
    synchronous detail (err::SomeException)=case fromException err :: Maybe SomeAsyncException of
      Just _->throwIO err
      Nothing->pure (Left detail)

closeOutputOwner :: OutputOwner -> IO ()
closeOutputOwner (OutputOwner _ slot _ _ worker)=do
  OutputSlot _ _ target _ _ frozen<-readIORef slot
  mapM_ W.retireWindowRef (maybeToList target++maybeToList frozen)
  cancel worker
  void (waitCatch worker)

retireOutputOpening :: OutputPublication -> IO ()
retireOutputOpening (OutputPublication _ _ Nothing _ (Just (Right update)))=W.retireWindowRef (W.updateWindowRef update)
retireOutputOpening _=pure ()

-- Replacement freezes the prior durable snapshot in its exact display slot. Late
-- preparation cannot cross this independent DAP transport/session epoch.
resetOutputOwner :: Debugger -> Desktop -> IO ()
resetOutputOwner (Debugger _ _ _ _ (OutputOwner _ slot desired latest _)) desktop=do
  OutputSlot epoch revision target _ _ frozen<-readIORef slot
  let previous=case target of Just reference->Just reference; _->frozen
      installed=previous >>= \reference->if M.member reference (pluginWindows desktop) then Just reference else Nothing
  -- Freeze rather than revoke an installed slot: replacement will atomically
  -- retire its capability. The session epoch already rejects every old reply.
  when (installed==Nothing) (mapM_ W.retireWindowRef previous)
  writeIORef slot (OutputSlot (epoch+1) (revision+1) Nothing False False installed)
  obsolete<-atomically (writeTVar desired Nothing >> tryTakeTMVar latest)
  mapM_ retireOutputOpening obsolete

queueOutput :: Debugger -> Bool -> Text -> IO ()
queueOutput (Debugger ref _ _ _ (OutputOwner _ slot desired _ _)) opening value=do
  OutputSlot epoch revision target requested focus frozen<-readIORef slot
  let next=revision+1
      prepareView=opening || requested || isJust target
      request=OutputRequest epoch next target prepareView value
  writeIORef slot (OutputSlot epoch next target (opening || requested) focus frozen)
  modifyIORef' ref (\state->state {outputPending=True})
  request `seq` atomically (writeTVar desired (Just request))

revealOutput :: Debugger -> Desktop -> IO Desktop
revealOutput runtime@(Debugger ref _ _ _ (OutputOwner _ slot _ _ _)) desktop=do
  current<-tickOutputOwner runtime desktop
  OutputSlot epoch revision target requested _ frozen<-readIORef slot
  let present=maybe False (\reference->M.member reference (pluginWindows current)) target
  if present then do
    writeIORef slot (OutputSlot epoch revision target requested True frozen)
    tickOutputOwner runtime current
  else do
    let old=case frozen of Just reference | M.member reference (pluginWindows current)->Just reference; _->Nothing
    when (old==Nothing) (mapM_ W.retireWindowRef frozen)
    writeIORef slot (OutputSlot epoch revision Nothing True True old)
    accepted<-readIORef ref
    unless (outputPending accepted) (queueOutput runtime True (output accepted))
    pure current

-- Idle ticks inspect only the exact owned slot and a publication receipt; no
-- source document labels, payloads or histories participate in invalidation.
tickOutputOwner :: Debugger -> Desktop -> IO Desktop
tickOutputOwner runtime@(Debugger ref _ _ _ (OutputOwner _ slot _ latest _)) desktop=do
  OutputSlot epoch revision target requestedOpen focus frozen<-readIORef slot
  next<-atomically (tryTakeTMVar latest)
  -- Report currentness is independent of the view: a closed or modal-deferred
  -- window must not discard the bounded report or stall the DAP inbox.
  forM_ next $ \(OutputPublication issued version _ report _)->when (issued==epoch && version==revision) $
    modifyIORef' ref (\state->state {output=either (const "") id report,outputPending=False})
  live<-maybe (pure True) W.windowRefCurrent target
  let present=maybe False (\reference->M.member reference (pluginWindows desktop)) target
      protected=dialog desktop/=Nothing || questionActive desktop || activeAutocomplete desktop
  if isJust target && (not live || not present) then do
    mapM_ W.retireWindowRef target
    -- The view closes independently; the outstanding report still owns this
    -- revision and will unblock further events once its receipt is ready.
    writeIORef slot (OutputSlot epoch revision Nothing False False Nothing)
    mapM_ retireOutputOpening next
    pure desktop
  else do
    let prepareRequested report=when (requestedOpen && target==Nothing) $
          forM_ (either (const Nothing) Just report) (queueOutput runtime False)
    updated<-case next of
      Nothing->pure desktop
      Just publication@(OutputPublication issued version captured report result)
        | issued/=epoch || version/=revision->retireOutputOpening publication >> pure desktop
        | Left err<-report->writeIORef slot (OutputSlot epoch revision target False False frozen) >> pure desktop {status=err}
        | captured/=target->retireOutputOpening publication >> prepareRequested report >> pure desktop
        | Nothing<-result->prepareRequested report >> pure desktop
        | captured==Nothing,Just old<-frozen,not (M.member old (pluginWindows desktop))->do
            W.retireWindowRef old
            retireOutputOpening publication
            writeIORef slot (OutputSlot epoch revision Nothing False False Nothing)
            pure desktop
        | captured==Nothing && frozen==Nothing && protected->do
            retained<-atomically (tryPutTMVar latest publication)
            unless retained (retireOutputOpening publication)
            pure desktop
        | otherwise->case fromMaybe (Left "Debugger output view preparation failed.") result of
          Left err->writeIORef slot (OutputSlot epoch revision target False False frozen) >> pure desktop {status=err}
          Right update->do
            adopted<-case (captured,frozen) of
              (Nothing,Just old)->replaceWindowUpdate Menu.HumanMenu old update desktop
              _->adoptWindowUpdate Menu.HumanMenu update desktop
            let reference=W.updateWindowRef update
                installed=M.member reference (pluginWindows adopted)
            writeIORef slot (OutputSlot epoch revision (if installed then Just reference else target) False focus (if installed then Nothing else frozen))
            unless installed (retireOutputOpening publication)
            pure adopted
    OutputSlot currentEpoch currentRevision currentTarget requested wantFocus previous<-readIORef slot
    if not wantFocus || protected then pure updated else
      case [windowId w | w<-windows updated,Just (windowContent w)==(PluginContent <$> currentTarget)] of
        ident:_->do
          writeIORef slot (OutputSlot currentEpoch currentRevision currentTarget requested False previous)
          pure (focusWindow ident updated)
        _->pure updated

type Core = Desktop -> [Effect] -> IO (Bool,Desktop)
data Debugger = Debugger (IORef State) (IO Integer) HdbRuntime SidebarMailbox OutputOwner
data SidebarMailbox = SidebarMailbox !(TVar (Maybe (Int,Int,FilePath))) !(TBQueue SidebarIngress) !(TVar (Int,Maybe WatchFrame,M.Map Int DebuggerWatch))
data DebugPageBody = AdapterPage !Value | CachedPage !Int !Value
data SidebarIngress = ReadDebugPage !DebugPageRequest !(TMVar (Either Text DebugPageBody)) | CacheDebugPage !DebugPageRequest !Value
  | ReadDebugSource !Int !Int !Int !(Maybe FilePath) !(MVar (Either Text Value))
data Pending = Init | Attach | Configure | Breaks Text [Int] | Exceptions | Threads Bool
  | Stack Bool Int Int | Scopes Int | Variables Int | ExceptionDetails | Source Bool Int Int Value | Control Bool | Detach
  | Inspection Text (MVar (Either Text Value)) | SourceInspection !Int !Int !(Maybe FilePath) (MVar (Either Text Value)) | SidebarRead DebugPageRequest (TMVar (Either Text DebugPageBody))
  | WatchRequest !WatchOperation !(Maybe Text) !FilePath ![FilePath]
  deriving (Eq)
data WatchMode = EvaluateWatch | ForceWatch !Int | ForceWatchChild !Int !Int !Int !Int deriving Eq
data WatchOperation = WatchOperation !Int !Int !WatchFrame !WatchMode deriving Eq
data PreparedWatch = PreparedWatch !Text !Int !Bool !(Maybe FilePath) !Bool !(Maybe Value)
  | PreparedWatchError !Text !(Maybe FilePath) !Bool
data WatchProvider = forall context reply. WatchProvider (P.TreeProvider context reply)
data WatchPreparation = WatchPreparation !WatchOperation !(Async PreparedWatch)
data ReferenceOwner = FrameReferences !Int !Int | WatchReferences !Int !Int !WatchFrame deriving (Eq,Ord)
data SourcePreparation = SourcePreparation !Int !Int !(Maybe (Int,Int)) !Bool !Value !(Maybe Int) !(Async (Either Text PreparedSource))
data PreparedSource = AdapterSource !(Maybe FilePath) !Buffer !Int | LocalSource !Bool !FilePath !LocalSourceTarget !Int
data LocalSourceTarget = ExistingSource !Int !Int !ContentVersion !Bool | NewSource !FileState !Buffer
data CapturedLocalSource = CapturedLocalSource !FilePath !Int !Int !ContentVersion !BufferContent !DirtySnapshot
data SourceObservation = SourceObservation !Int !(Maybe FilePath)
data Breakpoint = Breakpoint { bpLine :: Int, bpResult :: Value } deriving (Eq,Show)
data State = State
  { client :: Maybe D.Client, connected :: Bool, capabilities :: Value, ready :: Bool, configured :: Bool
  , pending :: M.Map Int (Pending,Int,Integer), generation :: Int, frameRevision :: Int
  , stopped :: Bool, thread :: Maybe Int, frame :: Maybe Value, frames :: [Value], followSource :: Bool
  , exceptionFilters :: [Text]
  , breakpoints :: M.Map Text (Value,[Breakpoint]), sources :: M.Map Int (Int,Int,Value)
  , root :: FilePath, endpoint :: (Text,Int), output :: Text
  , startRequest :: (Text,Value), managed :: Bool, adapterId :: Text
  , debugCradle :: Maybe FilePath
  , hdbLauncher :: Maybe FilePath, debugEnvironment :: [(String,String)], debugConsoles :: [Text], terminalLaunch :: Maybe (Int,Text,Async (),TMVar (Either Text C.PreparedConsole)), outputShown :: Bool, outputPending :: Bool
  , failure :: Maybe Text, disconnectAt :: Maybe Integer
  , endedAt :: Maybe Integer, programExitCode :: Maybe Int
  , choices :: M.Map Text [Value], choiceId :: Int, breakRequests :: M.Map Text Int
  , breakModified :: M.Map Text Bool, variableRefs :: M.Map Int Bool
  , sidebarVisible :: Bool, sidebarSession :: Int, sidebarPages :: M.Map DebugPageRequest Value
  , sidebarThreads :: M.Map Int (), sidebarFrames :: M.Map (Int,Int) Value
  , sidebarReferences :: M.Map (ReferenceOwner,Int) Bool
  , watchProvider :: Maybe WatchProvider, watchPreparing :: Maybe WatchPreparation
  , sourcePreparing :: Maybe SourcePreparation
  , sourceReferences :: M.Map Int SourceObservation, nextSourceObservation :: Int
  , watchExpressions :: M.Map Int DebuggerWatch, watchCatalogueRevision :: Int, nextWatch :: Int, watchDialog :: Maybe (Int,Maybe (Int,Int)), sourceWatchDialog :: Maybe (Int,DebugSourceRequest)
  }

emptyState :: State
emptyState = State {client=Nothing,connected=False,capabilities=Null,ready=False,configured=False,pending=M.empty,generation=0,frameRevision=0,
  stopped=False,thread=Nothing,frame=Nothing,frames=[],followSource=True,exceptionFilters=[],breakpoints=M.empty,sources=M.empty,
  root=".",endpoint=("127.0.0.1",4711),output="",failure=Nothing,disconnectAt=Nothing,endedAt=Nothing,programExitCode=Nothing,
  debugCradle=Nothing,hdbLauncher=Nothing,debugEnvironment=[],debugConsoles=[],terminalLaunch=Nothing,outputShown=False,outputPending=False,choices=M.empty,choiceId=0,breakRequests=M.empty,breakModified=M.empty,startRequest=("attach",object []),managed=False,adapterId="",variableRefs=M.empty,sidebarVisible=False,sidebarSession=0,sidebarPages=M.empty,sidebarThreads=M.empty,sidebarFrames=M.empty,sidebarReferences=M.empty,watchProvider=Nothing,watchPreparing=Nothing,sourcePreparing=Nothing,sourceReferences=M.empty,nextSourceObservation=1,watchExpressions=M.empty,watchCatalogueRevision=0,nextWatch=1,watchDialog=Nothing,sourceWatchDialog=Nothing}

withDebugger :: (Debugger -> IO a) -> IO a
withDebugger action = C.withConsoles (\consoles -> withDebuggerConsoles consoles action)

-- | Scope debugger state while borrowing the shared console service.
withDebuggerConsoles :: C.Consoles -> (Debugger -> IO a) -> IO a
withDebuggerConsoles consoles = withDebuggerHdbConsoles consoles (toInteger <$> getMonotonicTimeNSec) Hdb.prepareHdb Hdb.acquireHdb

-- | Provide the monotonic nanosecond clock used for debugger deadlines.
withDebuggerClock :: IO Integer -> (Debugger -> IO a) -> IO a
withDebuggerClock clock = withDebuggerHdb clock Hdb.prepareHdb Hdb.acquireHdb

-- Injectable acquisition boundary; production uses the pinned verified backend.
withDebuggerHdb :: IO Integer -> (Compilers.Compiler -> IO (Either Text Hdb.HdbPlan))
  -> (Hdb.HdbPlan -> (Downloads.DownloadProgress -> IO ()) -> IO (Either Text FilePath))
  -> (Debugger -> IO a) -> IO a
withDebuggerHdb clock prepare acquire action = C.withConsoles $ \consoles ->
  withDebuggerHdbConsoles consoles clock prepare acquire action

withDebuggerHdbConsoles :: C.Consoles -> IO Integer -> (Compilers.Compiler -> IO (Either Text Hdb.HdbPlan))
  -> (Hdb.HdbPlan -> (Downloads.DownloadProgress -> IO ()) -> IO (Either Text FilePath))
  -> (Debugger -> IO a) -> IO a
withDebuggerHdbConsoles consoles clock prepare acquire action = Downloads.withDownloads $ \downloads -> W.withWindowScope $ \scope -> bracket (newOutputOwner scope) closeOutputOwner $ \outputOwner -> DownloadsWindow.withOwner downloads $ \downloadView -> do
  jobs<-newIORef (HdbState 0 Nothing Nothing Nothing Nothing Nothing)
  retired<-newIORef []
  mailbox<-SidebarMailbox <$> newTVarIO Nothing <*> newTBQueueIO 32 <*> newTVarIO (0,Nothing,M.empty)
  let runtime=HdbRuntime downloads jobs prepare acquire consoles retired downloadView
  bracket ((\ref -> Debugger ref clock runtime mailbox outputOwner) <$> newIORef emptyState)
    (\debugger@(Debugger ref _ _ _ _) -> do
      invalidateHdb debugger
      h<-readIORef jobs
      mapM_ (\(_,_,_,task)->cancel task) (hdbPreparing h)
      atomically $ case mailbox of SidebarMailbox epoch _ _->writeTVar epoch Nothing
      readIORef ref >>= stopTransport debugger
      readIORef retired >>= mapM_ waitCatch) action

-- | Register the human-only exact-row Cancel action for this manager lifetime.
-- The existing menu worker prepares a closed request; only the Downloads owner
-- can perform its side effect after contribution/window/modal revalidation.
withDownloadsCommands :: MenuHost -> Debugger -> IO a -> IO a
withDownloadsCommands host (Debugger _ _ (HdbRuntime _ _ _ _ _ _ view) _ _) action=withRegistry $ \registry->do
  command<-either (fail . show) pure =<< registerCommand registry definition
  reference<-either (fail . show) pure =<< Menu.contributeMenu (menuContributions host)
    (Menu.MenuDef "hide.downloads.cancel" "context.window-rows" "downloads" 0 "Cancel transfer" "" False
      (Menu.menuAction registry command (const (Right ())) (\_ request->evaluate (PreparedDownloadCancel request))))
  DownloadsWindow.setMenuReference view (Just reference)
  action `finally` (DownloadsWindow.setMenuReference view Nothing >> void (Menu.retireMenu (menuContributions host) reference))
  where
    unit=Codec (object []) (const (Right ())) (const (object []))
    result=Codec (object []) (const (Left "Download cancellation is a host-owned request.")) (const (object []))
    definition=CommandDef "hide.downloads.cancel" "Cancel transfer" unit result $ \context ()->pure $ do
      if invocationOrigin context/=Menu.HumanMenu then Left (CommandRejected "Downloads actions require human input.") else Right ()
      (reference,node)<-maybe (Left (CommandRejected "No transfer row was captured.")) Right (invocationRow context)
      ident<-maybe (Left (CommandRejected "Invalid transfer ID.")) Right (readMaybe (T.unpack (P.nodeIdText node)))
      if ident<=0 then Left (CommandRejected "Invalid transfer ID.") else Right (DownloadCancelRequest reference ident)

debuggerEffects :: Debugger -> Core -> Core
debuggerEffects runtime fallback = foldM apply . (False,)
  where
    apply result@(True,_) _=pure result
    apply (_,d) (DebugAction action values) = do
      next<-perform runtime action values d
      publishSidebarEpoch runtime
      pure (False,next)
    apply (_,d) (PackageDebugAction target entry) = (False,) <$> queuePackageDebug runtime target entry d
    apply (_,d) (AdoptPreparedDebug target) = (False,) <$> adoptPackageDebug runtime target d
    apply (_,d) (DownloadCancelAction request) = (False,) <$> cancelDownloadRequest runtime request d
    apply (_,d) (DebugSourceAction request) = do
      next<-sourceAction runtime request d
      pure (False,next)
    apply (_,d) (DebugSidebarAction request) = do
      next<-sidebarAction runtime request d
      publishSidebarEpoch runtime
      pure (False,next)
    apply (_,d) effect@(ServiceAction action values)
      | action `elem` ["build-stop","run-config"] || action=="toolchain" && not (null values) =
          invalidateHdb runtime >> fallback d [effect]
    apply (_,d) effect=fallback d [effect]

-- | /O(1)/ immutable stopped projection. Provider workers borrow no mutable
-- debugger state; frame choice cannot expire sibling stopped handles.
debuggerSidebarEpoch :: Debugger -> IO (Maybe Int)
debuggerSidebarEpoch (Debugger _ _ _ (SidebarMailbox epoch _ _) _)=fmap (fmap (\(_,captured,_)->captured)) (readTVarIO epoch)

-- | Session identity for revealing Debug once; a later stop keeps the viewport.
debuggerSidebarSession :: Debugger -> IO (Maybe Int)
debuggerSidebarSession (Debugger _ _ _ (SidebarMailbox epoch _ _) _)=fmap (fmap (\(session,_,_)->session)) (readTVarIO epoch)

-- | /O(1)/ borrow of the bounded immutable expression catalogue. Sidebar workers
-- prepare presentation; this projection never reads mutable debugger state.
debuggerWatches :: Debugger -> IO (Int,Maybe WatchFrame,M.Map Int DebuggerWatch)
debuggerWatches (Debugger _ _ _ (SidebarMailbox _ _ watches) _)=readTVarIO watches

publishSidebarEpoch :: Debugger -> IO ()
publishSidebarEpoch (Debugger ref _ _ (SidebarMailbox epoch _ watches) _)=do
  currentState<-readIORef ref
  live<-watchProviderCurrent currentState
  let s=if live then currentState else currentState {watchProvider=Nothing}
  atomically (writeTVar epoch (if sidebarVisible s && stopped s && configured s && isJust (client s) && endedAt s==Nothing && disconnectAt s==Nothing then Just (sidebarSession s,generation s,root s) else Nothing))
  (_,previous,_)<-readTVarIO watches
  let selected=selectedWatchFrame s
      changed=previous/=selected
      expire entry=entry {watchValue=case watchValue entry of
        WatchResult receipt title _ _ origin | Just receipt/=selected->WatchStale title origin
        WatchLoading receipt | Just receipt/=selected->WatchPending
        WatchError receipt _ _ | Just receipt/=selected->WatchPending
        value->value}
      current=if changed then s {watchExpressions=M.map expire (watchExpressions s),watchCatalogueRevision=watchCatalogueRevision s+1} else s
  when changed (writeIORef ref current)
  atomically (writeTVar watches (watchCatalogueRevision current,selected,watchExpressions current))

selectedWatchFrame :: State -> Maybe WatchFrame
selectedWatchFrame s
  | sidebarVisible s && stopped s && configured s && ready s && isJust (client s) && endedAt s==Nothing && disconnectAt s==Nothing,
    Just (WatchProvider provider)<-watchProvider s,Just tid<-thread s,Just fid<-frame s >>= field "id",fid>0=Just (WatchFrame (P.treeReference provider) (generation s) (frameRevision s) tid fid)
  | otherwise=Nothing

-- | Borrow the existing provider lifetime. No handlers or metadata are invoked
-- by currentness checks; scope closure/retirement refuses late owner adoption.
withDebuggerWatchProvider :: Debugger -> P.TreeProvider context reply -> IO a -> IO a
withDebuggerWatchProvider (Debugger ref _ _ _ _) provider use=bracket
  (modifyIORef' ref (\s->s {watchProvider=Just (WatchProvider provider)}))
  (const (modifyIORef' ref (\s->s {watchProvider=Nothing}))) (const use)

watchProviderCurrent :: State -> IO Bool
watchProviderCurrent s=case watchProvider s of Nothing->pure False; Just (WatchProvider provider)->P.treeCurrent provider

-- | Wait on a sidebar worker. Only the debugger owner validates provenance and
-- enqueues its ordinary DAP request. Response sizing/cache preparation happen here,
-- outside the UI lock; owner cache admission retains at most 64 pages of 1 MiB.
debuggerSidebarRead :: Debugger -> DebugPageRequest -> IO (Either Text Value)
debuggerSidebarRead (Debugger _ _ _ (SidebarMailbox epoch queue _) _) request@(DebugPageRequest captured target offset)=do
  current<-readTVarIO epoch
  if fmap (\(_,value,_)->value) current/=Just captured then pure (Left "Debugger node expired.") else do
    reply<-newEmptyTMVarIO
    atomically (writeTBQueue queue (ReadDebugPage request reply))
    completed<-timeout 16000000 (atomically (takeTMVar reply))
    case completed of
      Nothing->pure (Left "Debugger sidebar request timed out.")
      Just (Left err)->pure (Left err)
      Just (Right result)->do
        -- Cache provenance comes from the host receipt, never adapter JSON.
        let (cachedOffset,body)=case result of AdapterPage value->(Nothing,value); CachedPage start value->(Just start,value)
            key=case target of DebugThreads->"threads"; DebugStack{}->"stackFrames"; DebugScopes{}->"scopes"; DebugVariables{}->"variables"; DebugWatchVariables{}->"variables"
            page=case target of
              DebugWatchVariables{}->variablePage offset cachedOffset body
              DebugVariables{}->variablePage offset cachedOffset body
              _->object [fromText key .= take 128 (items key body),"totalFrames" .= (field "totalFrames" body :: Maybe Int)]
        checked<-evaluate (boundedResult page)
        prepared<-case (checked,target,current) of
          (Right value,DebugStack{},Just (_,_,base))->prepareStackPaths base value
          _->pure checked
        case prepared of
          Left err->pure (Left err)
          Right value->do
            atomically (writeTBQueue queue (CacheDebugPage request value))
            pure (Right (case target of
              DebugWatchVariables{}->published value
              DebugVariables{}->published value
              _->value))
  where
    fromText=K.fromText
    published value=object ["variables" .= take 128 (items "variables" value),"hasMore" .= flag "hasMore" value]

-- Page zero owns the single bounded returned snapshot. Later entries retain only
-- their published rows; slicing/sizing happens on the waiting provider worker.
-- Adapters may ignore start/count, so continuation never repeats a DAP read.
variablePage :: Int -> Maybe Int -> Value -> Value
variablePage offset cachedOffset body=object ["variables" .= retained,"hasMore" .= more]
  where
    start=fromMaybe 0 cachedOffset
    rows=drop (offset-start) (items "variables" body)
    retained=if offset==0 then rows else take 128 rows
    more=if cachedOffset==Just offset then flag "hasMore" body else not (null (drop 128 rows))

-- At most four small mailbox messages per tick. Backpressure belongs to provider
-- workers; the session owner never waits for a producer or a DAP socket.
drainSidebarReads :: Debugger -> Desktop -> IO ()
drainSidebarReads runtime@(Debugger ref _ _ (SidebarMailbox _ queue _) _) d=forM_ [1..4::Int] $ \_->do
  next<-atomically (tryReadTBQueue queue)
  forM_ next $ \ingress->do
    s<-readIORef ref
    watchLive<-watchProviderCurrent s
    let current request@(DebugPageRequest _ target _)=validSidebarRequest s request && case target of DebugWatchVariables{}->watchLive; _->True
    case ingress of
      ReadDebugSource captured reference stamp origin reply
        | generation s/=captured || not (stopped s && ready s && configured s && isJust (client s) && endedAt s==Nothing && disconnectAt s==Nothing) || not (sourceStampCurrent s reference stamp) || maybe False (protectedPath d) origin ->void (tryPutMVar reply (Left "Debugger source is private or expired."))
        | length [() | (SourceInspection{},_,_)<-M.elems (pending s)]>=4 ->void (tryPutMVar reply (Left "Debugger source inspection is busy."))
        | otherwise->send runtime (SourceInspection reference stamp origin reply) "source" (object ["sourceReference" .= reference])
      CacheDebugPage request value | current request && (M.member request (sidebarPages s) || M.size (sidebarPages s)<64)->do
        recordSidebarResponse ref request value
        modifyIORef' ref (\state->state {sidebarPages=M.insert request value (sidebarPages state)})
      CacheDebugPage{}->pure ()
      ReadDebugPage request@(DebugPageRequest epoch target offset) reply
        | not (current request)->atomically (void (tryPutTMVar reply (Left "Debugger node expired or is lazy.")))
        | Just cached<-M.lookup request (sidebarPages s)->atomically (void (tryPutTMVar reply (Right (CachedPage offset cached))))
        | M.size (sidebarPages s)>=64->atomically (void (tryPutTMVar reply (Left "Debugger sidebar page budget reached; resume to refresh.")))
        | offset>0,(case target of DebugWatchVariables{}->True; DebugVariables{}->True; _->False)->atomically (void (tryPutTMVar reply
            (maybe (Left "Debugger variable snapshot expired.") (Right . CachedPage 0) (M.lookup (DebugPageRequest epoch target 0) (sidebarPages s)))))
        | otherwise->do
            let (command,args)=sidebarArguments request
            outcome<-try (send runtime (SidebarRead request reply) command args)
            case outcome of
              Left (err::IOException)->atomically (void (tryPutTMVar reply (Left ("DAP: "<>T.pack (show err)))))
              Right ()->pure ()

validSidebarRequest :: State -> DebugPageRequest -> Bool
validSidebarRequest s (DebugPageRequest epoch target offset)=generation s==epoch && sidebarVisible s && stopped s && configured s && isJust (client s) && disconnectAt s==Nothing && endedAt s==Nothing && offset>=0 && offset<=32768 && case target of
  DebugThreads->offset==0
  DebugStack tid->M.member tid (sidebarThreads s)
  DebugScopes tid fid->offset==0 && M.member (tid,fid) (sidebarFrames s)
  DebugVariables tid fid reference->offset `mod` 128==0 && M.lookup (FrameReferences tid fid,reference) (sidebarReferences s)==Just False
  DebugWatchVariables ident revision receipt reference->offset `mod` 128==0 && watchCurrent s ident revision receipt && M.lookup (WatchReferences ident revision receipt,reference) (sidebarReferences s)==Just False

sidebarArguments :: DebugPageRequest -> (Text,Value)
sidebarArguments (DebugPageRequest _ target offset)=case target of
  DebugThreads->("threads",object [])
  DebugStack tid->("stackTrace",object ["threadId" .= tid,"startFrame" .= offset,"levels" .= (128::Int)])
  DebugScopes _ fid->("scopes",object ["frameId" .= fid])
  DebugVariables _ _ reference->("variables",object ["variablesReference" .= reference])
  DebugWatchVariables _ _ _ reference->("variables",object ["variablesReference" .= reference])

-- Only IDs and shallow immutable DAP rows enter owner maps. Bounded raw response
-- fields are prepared/sized by the waiting worker before page cache admission.
recordSidebarResponse :: IORef State -> DebugPageRequest -> Value -> IO ()
recordSidebarResponse ref (DebugPageRequest _ target _) body=modifyIORef' ref $ \s->case target of
  DebugThreads->s {sidebarThreads=M.fromList [(ident,()) | row<-rows "threads",let ident=integer "id" row,ident>0]}
  DebugStack tid->s {sidebarFrames=boundedUnion (M.fromList [((tid,fid),row) | row<-rows "stackFrames",let fid=integer "id" row,fid>0]) (sidebarFrames s)}
  DebugScopes tid fid->s {sidebarReferences=boundedReferences (references tid fid "scopes") (sidebarReferences s)}
  DebugVariables tid fid _->s {sidebarReferences=boundedReferences (references tid fid "variables") (sidebarReferences s)}
  DebugWatchVariables ident revision receipt _->s {sidebarReferences=boundedReferences
    (M.fromListWith (||) [((WatchReferences ident revision receipt,reference),maybe False (flag "lazy") (field "presentationHint" row)) | row<-rows "variables",let reference=integer "variablesReference" row,reference>0]) (sidebarReferences s)}
  where
    -- Page zero may retain more rows, but admission never decodes its full array.
    rows key=case body of
      Object properties | Just (Array values)<-KM.lookup (K.fromText key) properties->V.toList (V.take 128 values)
      _->[]
    boundedUnion newer previous=fst (M.splitAt 32768 (M.union newer previous))
    boundedReferences newer previous=fst (M.splitAt 32768 (M.unionWith (||) newer previous))
    references tid fid key=M.fromListWith (||) [((FrameReferences tid fid,ident),key=="variables" && maybe False (flag "lazy") (field "presentationHint" row)) | row<-rows key,let ident=integer "variablesReference" row,ident>0]

sidebarAction :: Debugger -> DebugSidebarRequest -> Desktop -> IO Desktop
sidebarAction runtime@(Debugger ref _ _ _ _) request d
  | dialog d/=Nothing || questionActive d=pure d {status="Debugger action is unavailable while a dialog owns input."}
  | otherwise=case request of
      SelectDebugFrame epoch tid fid->selectSidebarFrame runtime epoch tid fid d
      AddDebugWatch->watchPrompt runtime Nothing d
      EditDebugWatch ident revision->do
        s<-readIORef ref
        case M.lookup ident (watchExpressions s) of
          Just entry | watchRevision entry==revision->watchPrompt runtime (Just (ident,entry)) d
          _->pure d {status="Watch expired."}
      EvaluateDebugWatch ident revision receipt->startWatch runtime ident revision receipt EvaluateWatch d
      ForceDebugWatch ident revision receipt reference->startWatch runtime ident revision receipt (ForceWatch reference) d
      ForceDebugWatchChild ident revision receipt parent offset position reference->startWatch runtime ident revision receipt (ForceWatchChild parent offset position reference) d
      RemoveDebugWatch ident revision->do
        s<-readIORef ref
        case M.lookup ident (watchExpressions s) of
          Just entry | watchRevision entry==revision->do
            modifyIORef' ref (\current->current {watchExpressions=M.delete ident (watchExpressions current),watchCatalogueRevision=watchCatalogueRevision current+1})
            pure d {status="Watch removed."}
          _->pure d {status="Watch expired."}

watchPrompt :: Debugger -> Maybe (Int,DebuggerWatch) -> Desktop -> IO Desktop
watchPrompt (Debugger ref _ _ _ _) chosen d=do
  s<-readIORef ref
  let ident=choiceId s+1
      expression=maybe "" (watchExpression.snd) chosen
      origins=maybe [] (watchOrigins.snd) chosen
      origin=listToMaybe (filter (protectedPath d) origins++origins)
      private=maybe False (watchPrivate.snd) chosen || any (protectedPath d) origins
      target=fmap (\(key,entry)->(key,watchRevision entry)) chosen
  modifyIORef' ref (\current->current {choiceId=ident,watchDialog=Just (ident,target)})
  pure d {dialog=Just (Dialog (if isJust chosen then "Edit watch" else "Add watch") (DebuggerWatchDialog ident origin private)
    [SelectedInput "Expression" expression (Selection 0 (T.length expression))] 0 ["Save","Cancel"]
    ["Evaluation is explicit and can execute program code."]),status="Enter a watch expression."}

watchOrigins :: DebuggerWatch -> [FilePath]
watchOrigins entry=maybeToList (watchOrigin entry)++case watchValue entry of
  WatchResult _ _ _ _ origin->maybeToList origin
  WatchStale _ origin->maybeToList origin
  WatchError _ _ origin->maybeToList origin
  _->[]

storeWatch :: Debugger -> Maybe (Int,Int) -> Text -> Maybe FilePath -> Bool -> Desktop -> IO Desktop
storeWatch (Debugger ref _ _ _ _) target expression origin private d=do
  s<-readIORef ref
  let chosen=target >>= \(ident,revision)->case M.lookup ident (watchExpressions s) of
        Just entry | watchRevision entry==revision->Just (ident,entry)
        _->Nothing
  if T.null (T.strip expression) || T.compareLength expression 4096==GT || T.any (=='\0') expression
    then pure d {status="Watch expression must contain 1–4096 characters without NUL."}
  else if isJust target && not (isJust chosen) then pure d {status="Watch changed; reopen its editor."}
  else if not (isJust target) && M.size (watchExpressions s)>=128 then pure d {status="Watch limit reached; remove a watch first."}
  else do
    let ident=maybe (nextWatch s) fst chosen
        entry=case chosen of
          Nothing->DebuggerWatch (T.copy expression) 0 origin private WatchPending
          Just (_,previous)->previous {watchExpression=T.copy expression,watchRevision=watchRevision previous+1,watchValue=WatchPending}
    modifyIORef' ref (\current->current {watchExpressions=M.insert ident entry (watchExpressions current),
      nextWatch=if isJust chosen then nextWatch current else nextWatch current+1,watchCatalogueRevision=watchCatalogueRevision current+1})
    pure d {status=if isJust chosen then "Watch updated." else "Watch added."}

watchCurrent :: State -> Int -> Int -> WatchFrame -> Bool
watchCurrent s ident revision receipt=selectedWatchFrame s==Just receipt && maybe False ((==revision).watchRevision) (M.lookup ident (watchExpressions s))

-- Docs: docs/site/screenshots/debug-watches.png (docs/running.md) shows explicit evaluation and cached children.
startWatch :: Debugger -> Int -> Int -> WatchFrame -> WatchMode -> Desktop -> IO Desktop
startWatch runtime@(Debugger ref _ _ _ _) ident revision receipt mode d=do
  s<-readIORef ref
  let busy=isJust (watchPreparing s) || any (\(kind,_,_)->case kind of WatchRequest{}->True; _->False) (M.elems (pending s))
      forceAllowed reference=case watchValue <$> M.lookup ident (watchExpressions s) of
        Just (WatchResult current _ rootReference True _)->current==receipt && rootReference==reference && M.lookup (WatchReferences ident revision receipt,reference) (sidebarReferences s)==Just True
        _->False
      childAllowed parent offset position reference=offset>=0 && offset<=32768 && offset `mod` 128==0 && position>=0 && position<128 && reference>0 &&
        M.lookup (WatchReferences ident revision receipt,parent) (sidebarReferences s)==Just False &&
        M.lookup (WatchReferences ident revision receipt,reference) (sidebarReferences s)==Just True &&
        case M.lookup (DebugPageRequest epoch (DebugWatchVariables ident revision receipt parent) offset) (sidebarPages s) >>= atRows position of
          Just row->integer "variablesReference" row==reference && maybe False (flag "lazy") (field "presentationHint" row)
          _->False
      epoch=case receipt of WatchFrame _ value _ _ _->value
      atRows position body=case body of
        Object properties | Just (Array values)<-KM.lookup "variables" properties->values V.!? position
        _->Nothing
  live<-watchProviderCurrent s
  if not live || not (watchCurrent s ident revision receipt) then pure d {status="Watch or stopped frame expired."}
  else if busy then pure d {status="Watch execution is busy."}
  else if case mode of
    ForceWatch reference->not (forceAllowed reference)
    ForceWatchChild parent offset position reference->not (childAllowed parent offset position reference)
    EvaluateWatch->False
    then pure d {status="Lazy watch reference expired."}
  else do
    -- Executing requests invalidate every retained value handle before dispatch.
    -- Keep the chosen frame, but never keep its old selection/stop receipt.
    let invalid=(invalidate s) {stopped=True,frame=frame s,frames=frames s}
        fresh=maybe (error "validated watch frame missing") id (selectedWatchFrame invalid)
        next=invalid {watchExpressions=M.adjust (\entry->entry {watchValue=WatchLoading fresh}) ident (watchExpressions invalid),watchCatalogueRevision=watchCatalogueRevision invalid+1}
        operation=WatchOperation ident revision fresh mode
        rawPath=frame s >>= field "source" >>= field "path"
    backing<-traverse evaluate rawPath
    base<-evaluate (root s)
    private<-evaluate (guestPrivatePaths d)
    writeIORef ref next
    let WatchFrame _ _ _ _ fid=fresh
        (command,args)=case mode of
          EvaluateWatch->("evaluate",object ["expression" .= maybe "" watchExpression (M.lookup ident (watchExpressions s)),"frameId" .= fid,"context" .= ("watch"::Text)])
          ForceWatch reference->("variables",object ["variablesReference" .= reference])
          ForceWatchChild _ _ _ reference->variables reference
        variables reference=("variables",object ["variablesReference" .= reference,"start" .= (0::Int),"count" .= (128::Int)])
    send runtime (WatchRequest operation backing base private) command args
    pure d {status=case mode of EvaluateWatch->"Evaluating watch…"; ForceWatch{}->"Forcing lazy watch…"; ForceWatchChild{}->"Forcing lazy child…"}

prepareWatch :: Debugger -> WatchOperation -> Maybe Text -> FilePath -> [FilePath] -> Either Text Value -> IO ()
prepareWatch (Debugger ref _ _ _ _) operation backing base private result=mask_ $ do
  worker<-asyncWithUnmask $ \unmask->unmask $ do
    origin<-canonicalSourcePath base (T.unpack <$> backing)
    let clean=T.copy . T.take 256 . T.map (\c->if c<' ' then ' ' else c)
        failed canonical privateOrigin err=do
          let detail=clean err
          _<-evaluate (T.length detail)
          pure (PreparedWatchError detail canonical privateOrigin)
    case origin of
      Left _->failed Nothing True "Watch source provenance could not be prepared."
      Right canonical->do
        let privateOrigin=maybe False (protectedFilePath private) canonical
        case result >>= boundedResult of
          Left err->failed canonical privateOrigin err
          Right body->do
            let WatchOperation _ _ _ mode=operation
                (title,reference,lazy,page)=case mode of
                  EvaluateWatch->(text "result" body,integer "variablesReference" body,maybe False (flag "lazy") (field "presentationHint" body),Nothing)
                  ForceWatch handle->("Forced; expand to inspect",handle,False,Just (variablePage 0 Nothing body))
                  -- A forcing reply may replace the value and immediately
                  -- invalidate all references. Refresh the expression explicitly;
                  -- neither the requested handle nor reply handles stay live.
                  ForceWatchChild{}->("Child forced; evaluate watch to refresh",0,False,Nothing)
                prepared=clean title
            _<-evaluate (T.length prepared)
            checked<-traverse (evaluate . boundedResult) page
            case checked of
              Just (Left err)->failed canonical privateOrigin err
              _->pure (PreparedWatch prepared reference lazy canonical privateOrigin (case checked of Just (Right value)->Just value; _->Nothing))
  modifyIORef' ref (\s->s {watchPreparing=Just (WatchPreparation operation worker)})

tickWatchPreparation :: Debugger -> Desktop -> IO Desktop
tickWatchPreparation (Debugger ref _ (HdbRuntime _ _ _ _ _ retired _) _ _) d=do
  s<-readIORef ref
  case watchPreparing s of
    Nothing->pure d
    Just (WatchPreparation (WatchOperation ident revision receipt mode) worker)->do
      live<-watchProviderCurrent s
      if not live || not (watchCurrent s ident revision receipt) || dialog d/=Nothing || questionActive d then mask_ $ do
          modifyIORef' ref (\state->state {watchPreparing=Nothing,watchExpressions=if watchCurrent state ident revision receipt then M.adjust (\entry->entry {watchValue=WatchPending}) ident (watchExpressions state) else watchExpressions state,watchCatalogueRevision=watchCatalogueRevision state+1})
          cleanup<-asyncWithUnmask (\unmask->unmask (cancel worker))
          modifyIORef' retired (cleanup:)
          pure d
      else do
          completed<-poll worker
          case completed of
            Nothing->pure d
            Just outcome->do
              modifyIORef' ref (\state->state {watchPreparing=Nothing})
              let result=case outcome of Left _->PreparedWatchError "Watch result preparation failed." Nothing True; Right value->value
              case result of
                PreparedWatchError err origin private->do
                  modifyIORef' ref (\state->state {watchExpressions=M.adjust (\entry->entry {watchValue=WatchError receipt err origin,watchPrivate=watchPrivate entry || private || maybe False (protectedPath d) origin}) ident (watchExpressions state),watchCatalogueRevision=watchCatalogueRevision state+1})
                  pure d {status="Watch evaluation failed."}
                PreparedWatch title reference lazy origin private page->do
                  let target=DebugPageRequest (case receipt of WatchFrame _ epoch _ _ _->epoch) (DebugWatchVariables ident revision receipt reference) 0
                      refs=if reference>0 then M.insert (WatchReferences ident revision receipt,reference) lazy (sidebarReferences s) else sidebarReferences s
                  modifyIORef' ref (\state->state {watchExpressions=M.adjust (\entry->entry {watchValue=case mode of ForceWatchChild{}->WatchStale title origin; _->WatchResult receipt title reference lazy origin,watchPrivate=watchPrivate entry || private || maybe False (protectedPath d) origin}) ident (watchExpressions state),watchCatalogueRevision=watchCatalogueRevision state+1,sidebarReferences=refs,
                    variableRefs=if reference>0 then M.insertWith (||) reference (lazy || case mode of ForceWatch{}->True; _->False) (variableRefs state) else variableRefs state,sidebarPages=maybe (sidebarPages state) (\value->M.insert target value (sidebarPages state)) page})
                  forM_ page (recordSidebarResponse ref target)
                  pure d {status="Watch result ready."}

selectSidebarFrame :: Debugger -> Int -> Int -> Int -> Desktop -> IO Desktop
selectSidebarFrame runtime@(Debugger ref _ _ _ _) epoch tid fid d=do
  s<-readIORef ref
  if not (validSidebarRequest s (DebugPageRequest epoch (DebugScopes tid fid) 0)) || dialog d/=Nothing
    then pure d {status="Debugger frame expired."}
    else case M.lookup (tid,fid) (sidebarFrames s) of
      Nothing->pure d {status="Debugger frame expired."}
      Just chosen->do
        modifyIORef' ref (\state->state {thread=Just tid,frame=Just chosen,frameRevision=frameRevision state+1,choices=M.empty})
        openFrame runtime True Nothing d chosen

-- | Initiate a tool under desktop serialization and return its outside-lock wait.
-- Inspection handles must belong to the current stopped generation.
debuggerTool :: Debugger -> Desktop -> Text -> Value -> IO (Desktop, IO (Either Text Value))
debuggerTool runtime@(Debugger ref _ _ _ _) d name arguments = do
  s<-readIORef ref
  case parseEither (parseTool s name) arguments of
    Left err -> pure (d,pure (Left (T.pack err)))
    Right request -> do
      result<-try (run request)
      publishSidebarEpoch runtime
      pure $ either (\(err::IOException) -> (d,pure (Left ("DAP: "<>T.pack (show err))))) id result
  where
    immediate desktop result=pure (desktop,pure (result >>= boundedResult))
    snapshot desktop=do
      current<-readIORef ref
      value<-debuggerStatus current
      pure (desktop,case failure current of
        Just err->pure (Left err)
        Nothing->publicDebuggerStatus (root current) (guestPrivatePaths desktop) (merge (object ["accepted" .= True]) value))
    run ToolStatus=do
      current<-readIORef ref
      value<-debuggerStatus current
      pure (d,publicDebuggerStatus (root current) (guestPrivatePaths d) value)
    run (ToolStart action values)=do
      desktop<-perform runtime action values d
      current<-readIORef ref
      starting<-hdbPending runtime
      if isJust (client current) || starting then snapshot desktop else immediate desktop (Left (status desktop))
    run (ToolControl command)=perform runtime command [] d >>= snapshot
    run (ToolPresent following view)=do
      when (isJust view) (modifyIORef' ref (\state->state {sidebarVisible=True}))
      forM_ following (\enabled->modifyIORef' ref (\state->state {followSource=enabled}))
      current<-readIORef ref
      shown<-case view of
        Nothing -> pure d
        Just "source" -> maybe (pure d) (openFrame runtime True (Just (guestPrivatePaths d)) d) (frame current)
        Just "stack" -> showChoices runtime "Call stack" "frame" (frames current) (map frameLabel (frames current)) d
        Just command -> perform runtime command [] d
      snapshot shown
    run (ToolBreakpoints bid rows)=case M.lookup bid (buffers d) of
      Nothing -> immediate d (Left "Unknown bufferId.")
      Just _ | protectedBuffer d bid -> immediate d (Left "Buffer is private.")
      Just doc | byteMode (documentBuffer doc) -> immediate d (Left "Breakpoints require a source text buffer.")
      Just doc -> do
        s<-readIORef ref
        source<-case documentFile doc of
          Just file -> Just . object . (:[]) . ("path" .=) <$> canonicalizePath (filePath file)
          Nothing -> pure (liveBufferSource s bid)
        case source of
          Nothing -> immediate d (Left "Buffer has no file or debugger source.")
          Just src | maybe False (protectedPath d . T.unpack) (field "path" src :: Maybe Text)->immediate d (Left "Debugger source is private.")
          Just src -> do
            let key=sourceKey src
                linesRequested=M.keys (M.fromList [(row,()) | row<-rows])
                old=maybe [] snd (M.lookup key (breakpoints s))
                modified=dirty (documentBuffer doc)
                unchanged=map bpLine old==linesRequested && M.findWithDefault False key (breakModified s)==modified
                points=if unchanged then old else [Breakpoint row Null | row<-linesRequested]
            unless unchanged $ do
              modifyIORef' ref (\state -> state {breakpoints=M.insert key (src,points) (breakpoints state),
                breakModified=M.insert key modified (breakModified state)})
              when (configured s) (sendBreakpoints runtime key src points)
            snapshot d
    run (ToolInspect "source" args)=do
      state<-readIORef ref
      let reference=integer "sourceReference" args
      case M.lookup reference (sourceReferences state) of
        Nothing->immediate d (Left "Unknown or expired source reference.")
        Just (SourceObservation stamp backing)->do
          captured<-evaluate (generation state)
          base<-evaluate (root state)
          private<-evaluate (guestPrivatePaths d)
          reply<-newEmptyMVar
          pure (d,do
            origin<-canonicalSourcePath base backing
            case origin of
              Left err->pure (Left err)
              Right canonical | maybe False (protectedFilePath private) canonical->pure (Left "Debugger source is private.")
              Right canonical->do
                let Debugger _ _ _ (SidebarMailbox _ queue _) _=runtime
                result<-timeout 16000000 $ do
                  atomically (writeTBQueue queue (ReadDebugSource captured reference stamp canonical reply))
                  awaitInspection ref captured reply
                pure $ maybe (Left "Debugger source inspection timed out.") (>>= \body->boundedResult (object ["generation" .= captured,"request" .= ("source"::Text),"body" .= body])) result)
    run (ToolInspect command args)=do
      s<-readIORef ref
      reply<-newEmptyMVar
      send runtime (Inspection command reply) command args
      pure (d,do
        result<-timeout 16000000 (awaitInspection ref (generation s) reply)
        prepared<-case result of
          Nothing->pure (Left "Debugger inspection timed out; refresh debug_status.")
          Just (Left err)->pure (Left err)
          Just (Right body) | command=="stackTrace"->publicStack (root s) (guestPrivatePaths d) body
                            | otherwise->pure (Right body)
        pure $ prepared >>= \body -> boundedResult (object
          ["generation" .= generation s,"request" .= command,"body" .= body]))

data ToolRequest = ToolStatus | ToolStart Text [Text] | ToolControl Text
  | ToolBreakpoints Int [Int] | ToolInspect Text Value | ToolPresent (Maybe Bool) (Maybe Text)

parseTool :: State -> Text -> Value -> Parser ToolRequest
parseTool s name = withObject "debugger tool arguments" $ \o -> do
  let fieldsAllowed names=unless (all ((`elem` names) . K.toText) (KM.keys o)) (fail "Unknown debugger argument")
      epoch=do
        expected<-o .: "generation"
        unless (expected==generation s) (fail "Debugger generation expired; refresh debug_status")
      live=unless (isJust (client s) && disconnectAt s==Nothing && endedAt s==Nothing) (fail "No active debugger session")
      idle=when (isJust (client s) && endedAt s==Nothing) (fail "Disconnect the existing debugger session first")
      portNumber=do
        port<-o .:? "port" .!= (4711::Int)
        unless (port>0 && port<=65535) (fail "port must be between 1 and 65535")
        pure port
      positive key=do
        value<-o .:? key
        forM_ value (\n -> unless (n>0) (fail (T.unpack (K.toText key)<>" must be positive")))
        pure (value :: Maybe Int)
      required key selected=positive key >>= maybe (maybe (fail (T.unpack (K.toText key)<>" is required")) pure selected) pure
  case name of
    "debug_status" -> fieldsAllowed [] >> pure ToolStatus
    "debug_present" -> do
      fieldsAllowed ["follow","view","generation"]
      following<-o .:? "follow"
      view<-o .:? "view"
      when (KM.member "generation" o || isJust view) epoch
      forM_ view $ \choice -> do
        unless (choice `elem` ["source","stack","scopes","output"]) (fail "view must be source, stack, scopes or output")
        when (choice/="output") $ do
          live
          unless (ready s && configured s && stopped s) (fail "Debugger must be ready and stopped to reveal this view")
          when (choice `elem` ["source","scopes"] && frame s==Nothing) (fail "No selected debugger frame; inspect debug_status after the stack arrives")
      pure (ToolPresent following view)
    "debug_launch" -> do
      fieldsAllowed ["adapterConfig","port"]
      idle
      port<-portNumber
      config<-o .:? "adapterConfig"
      case config of
        Just path | T.null path || T.any (=='\0') path -> fail "adapterConfig must be a nonempty path without NUL bytes"
        Just path -> pure (ToolStart "launch-config" ["1",path])
        Nothing -> pure (ToolStart "launch-config" ["0","",tshow port])
    "debug_attach" -> do
      fieldsAllowed ["host","port"]
      idle
      host<-o .:? "host" .!= "127.0.0.1"
      unless (host `elem` ["localhost","127.0.0.1","::1"]) (fail "host must be loopback")
      port<-portNumber
      pure (ToolStart "connect" ["0",host,tshow port])
    "debug_control" -> do
      fieldsAllowed ["generation","command"]
      epoch
      live
      command<-o .: "command"
      unless (command `elem` ["continue","next","stepIn","stepOut","pause","disconnect"]) (fail "Unsupported debugger control")
      unless (command=="disconnect" || (ready s && configured s && isJust (thread s) &&
        if command=="pause" then not (stopped s) else stopped s)) (fail "Debugger is not ready for this control")
      pure (ToolControl command)
    "debug_set_breakpoints" -> do
      fieldsAllowed ["generation","bufferId","lines"]
      epoch
      bid<-o .: "bufferId"
      rows<-o .: "lines"
      unless (bid>=0 && length rows<=1000 && all (>0) rows) (fail "bufferId must be nonnegative and lines must contain at most 1000 positive integers")
      pure (ToolBreakpoints bid rows)
    "debug_inspect" -> do
      fieldsAllowed ["generation","request","threadId","frameId","variablesReference","sourceReference","start","count"]
      epoch
      live
      unless (ready s && configured s) (fail "Debugger is not ready for inspection")
      command<-o .: "request"
      unless (command `elem` ["threads","stackTrace","scopes","variables","source","exceptionInfo"]) (fail "Unsupported debugger inspection")
      when (command `elem` ["stackTrace","scopes","variables","exceptionInfo"] && not (stopped s)) (fail "Debugger must be stopped for this inspection")
      -- Validate even unused optional fields: malformed handles are never ignored.
      mapM_ positive ["threadId","frameId","variablesReference","sourceReference"]
      start<-o .:? "start" .!= (0::Int)
      count<-o .:? "count" .!= (100::Int)
      unless (start>=0 && count>0 && count<=1000) (fail "start must be nonnegative and count must be between 1 and 1000")
      args<-case command of
        "threads" -> pure (object [])
        "stackTrace" -> do
          tid<-required "threadId" (thread s)
          pure (object ["threadId" .= tid,"startFrame" .= start,"levels" .= count])
        "exceptionInfo" -> do
          unless (flag "supportsExceptionInfoRequest" (capabilities s)) (fail "This debugger does not support exception details")
          tid<-required "threadId" (thread s)
          pure (object ["threadId" .= tid])
        "scopes" -> do
          ident<-required "frameId" (frame s >>= field "id")
          pure (object ["frameId" .= ident])
        "variables" -> do
          ident<-required "variablesReference" Nothing
          case M.lookup ident (variableRefs s) of
            Just False -> pure ()
            Just True -> fail "Lazy variable requires explicit evaluation; read-only inspection cannot force it"
            Nothing -> fail "Unknown or expired variable reference; request scopes/variables again"
          pure (object ["variablesReference" .= ident,"start" .= start,"count" .= count])
        _ -> do
          let selected=frame s >>= field "source"
              reference=selected >>= field "sourceReference" >>= \n -> if n>0 then Just n else Nothing
          unless (stopped s) (fail "Debugger must be stopped for source inspection")
          ident<-required "sourceReference" reference
          unless (M.member ident (sourceReferences s)) (fail "Unknown or expired source reference; inspect a stopped stack again")
          pure (object ["sourceReference" .= ident])
      pure (ToolInspect command args)
    _ -> fail "Unknown debugger tool"

debuggerStatus :: State -> IO Value
debuggerStatus s=do
  -- Observe completion without joining or forcing the prepared Buffer. A ready
  -- result stays owned by the next UI tick, which may defer it behind a modal.
  preparation<-case sourcePreparing s of
    Nothing->pure ("idle"::Text)
    Just (SourcePreparation _ _ _ _ _ _ worker)->do
      result<-poll worker
      pure (if isJust result then "ready" else "preparing")
  pure $ object
   ["sourcePreparation" .= preparation,"generation" .= generation s,"active" .= (isJust (client s) && endedAt s==Nothing),"terminated" .= isJust (endedAt s),
   "finishing" .= (isJust (client s) && isJust (endedAt s)),"exitCode" .= programExitCode s,"connected" .= connected s,
   "ready" .= ready s,"configured" .= configured s,"stopped" .= stopped s,"follow" .= followSource s,
   "threadId" .= thread s,"frame" .= frame s,"source" .= (frame s >>= (field "source" :: Value -> Maybe Value)),
   "watchCount" .= M.size (watchExpressions s),"capabilities" .= capabilities s,"breakpoints" .=
     [object ["source" .= src,"line" .= bpLine bp,"verified" .= flag "verified" (bpResult bp),
       "pending" .= (bpResult bp==Null),"result" .= bpResult bp,
       "sourceModified" .= M.findWithDefault False key (breakModified s)] | (key,src,bp)<-allBreakpoints s],
   "output" .= output s,"error" .= failure s]

-- Public metadata is prepared on the waiting tool worker. Entire private rows
-- are omitted: frame names and breakpoint messages can also reveal their source.
publicDebuggerStatus :: FilePath -> [FilePath] -> Value -> IO (Either Text Value)
publicDebuggerStatus base private value=case boundedResult value of
  Left err->pure (Left err)
  Right (Object objectValue)->do
    visibleFrame<-visibleRow base private (fromMaybe Null (field "frame" value))
    visibleSource<-visibleBacking base private (fromMaybe Null (field "source" value))
    points<-filterM (visibleRow base private) (items "breakpoints" value)
    pure (Right (Object (KM.insert "breakpoints" (toJSON points) $
      KM.insert "source" (if visibleSource then fromMaybe Null (field "source" value) else Null) $
      KM.insert "frame" (if visibleFrame then fromMaybe Null (field "frame" value) else Null) objectValue)))
  Right other->pure (Right other)

publicStack :: FilePath -> [FilePath] -> Value -> IO (Either Text Value)
publicStack base private value=do
  prepared<-prepareStackPaths base value
  case prepared of
    Right body@(Object objectValue)->do
      rows<-filterM (visibleRow base private) (items "stackFrames" body)
      pure (Right (Object (KM.insert "stackFrames" (toJSON rows) objectValue)))
    _->pure prepared

visibleRow :: FilePath -> [FilePath] -> Value -> IO Bool
visibleRow base private row=visibleBacking base private (fromMaybe Null (field "source" row))
visibleBacking :: FilePath -> [FilePath] -> Value -> IO Bool
visibleBacking base private source=do
  result<-canonicalSourcePath base (T.unpack <$> (field "path" source :: Maybe Text))
  pure (either (const False) (not . maybe False (protectedFilePath private)) result)

-- Sidebar page metadata carries canonical resource provenance to the ordinary
-- shared tree privacy projection. No filesystem work runs during row painting.
prepareStackPaths :: FilePath -> Value -> IO (Either Text Value)
prepareStackPaths base value=case boundedResult value of
  Left err->pure (Left err)
  Right (Object objectValue)->do
    prepared<-mapM prepare (take 1000 (items "stackFrames" value))
    pure $ do
      rows<-sequence prepared
      Right (Object (KM.insert "stackFrames" (toJSON rows) objectValue))
  Right other->pure (Right other)
  where
    prepare row@(Object rowValue)=case field "source" row of
      Just (Object sourceValue)->do
        canonical<-canonicalSourcePath base (T.unpack <$> (field "path" (Object sourceValue) :: Maybe Text))
        pure $ fmap (\origin->case origin of
          Nothing->row
          Just path->Object (KM.insert "source" (Object (KM.insert "path" (toJSON path) sourceValue)) rowValue)) canonical
      _->pure (Right row)
    prepare row=pure (Right row)

boundedResult :: Value -> Either Text Value
boundedResult value | BL.length (encode value)>=1024*1024 = Left "Debugger response exceeds 1 MiB; request a smaller page."
                    | otherwise = Right value

completeInspection :: Pending -> Either Text Value -> IO ()
completeInspection (SourceInspection _ _ _ reply) result=void (tryPutMVar reply result)
completeInspection (Inspection _ reply) result=tryPutMVar reply result >> pure ()
completeInspection (SidebarRead _ reply) result=atomically (void (tryPutTMVar reply (AdapterPage <$> result)))
completeInspection _ _=pure ()

awaitInspection :: IORef State -> Int -> MVar (Either Text Value) -> IO (Either Text Value)
awaitInspection ref epoch reply=do
  result<-tryReadMVar reply
  s<-readIORef ref
  if generation s/=epoch || not (isJust (client s)) || disconnectAt s/=Nothing
    then pure (Left "Debugger inspection expired; refresh debug_status.")
    else case failure s of
      Just err -> pure (Left err)
      Nothing -> maybe (threadDelay 10000 >> awaitInspection ref epoch reply) pure result

perform :: Debugger -> Text -> [Text] -> Desktop -> IO Desktop
perform runtime@(Debugger ref clock _ _ _) action values d = do
  when (action `elem` ["launch","launch-config","connect","attach","disconnect"]) (invalidateHdb runtime)
  s<-readIORef ref
  case (action,values) of
    ("downloads",[]) -> showDownloads runtime d
    _ | "hdb-accept:" `T.isPrefixOf` action -> acceptHdb runtime action values d
    ("output",_) -> modifyIORef' ref (\state -> state {outputShown=True}) >> revealOutput runtime d
    -- Docs: docs/site/screenshots/debug-launch.png (docs/running.md).
    ("launch",_) -> pure d {dialog=Just (Dialog "Launch debugger" (DebugDialog "launch-config")
      [Input "Adapter configuration" ".thc-debug.json" 15,Input "DAP port" "4711" 4] 0 ["Selected target","Adapter config","Cancel"]
      ["Selected target follows the THC/GHC status-bar choice.",
       "Adapter config reads a project-relative JSON file."])}
    ("launch-config","0":_:portText:_) -> case readMaybe (T.unpack portText) of
      Just port | port>0 && port<=65535 -> do
        result<-try $ launchTarget runtime port d
        pure $ either (\(err::IOException) -> d {status="Debugger: "<>T.pack (show err)}) id result
      _ -> pure d {status="Enter a DAP port between 1 and 65535."}
    ("launch-config","1":configPath:_) -> do
      result<-try $ do
        directory<-resolveBuildRoot d
        let path=if isAbsolute (T.unpack configPath) then T.unpack configPath else directory </> T.unpack configPath
        bytes<-withFileRead path (\h -> BS.hGet h (1024*1024+1))
        if BS.length bytes>1024*1024 then pure (Left "Debugger configuration exceeds 1 MiB.") else
          case eitherDecodeStrict' bytes >>= parseEither parseLaunch of
            Left err -> pure (Left (T.pack err))
            Right config -> Right <$> startSession runtime directory config d
      pure $ either (\(err::IOException) -> d {status="DAP: "<>T.pack (show err)})
        (either (\err -> d {status="DAP configuration: "<>err}) id) result
    ("attach",_) -> let (host,port)=endpoint s in pure d {dialog=Just (Dialog "Attach debugger" (DebugDialog "connect")
      [Input "Host" host (T.length host),Input "Port" (tshow port) (length (show port))] 0 ["Attach","Cancel"]
      ["Connect to a running loopback DAP server.","Use Launch / Adapter config for custom attach arguments."])}
    ("connect",_:host:portText:_) -> case readMaybe (T.unpack portText) of
      Just port | port>0 && port<=65535 -> do
        result<-try $ do
          directory<-resolveBuildRoot d
          startSession runtime directory (LaunchConfig (TCP host port) "attach" (object []) "thc") d
        pure $ either (\(err::IOException) -> d {status="DAP: "<>T.pack (show err)}) id result
      _ -> pure d {status="Enter a port between 1 and 65535."}
    ("disconnect",_) -> do
      now<-clock
      modifyIORef' ref (\state -> (invalidate state) {ready=False,configured=False,pending=M.empty,disconnectAt=Just now})
      send runtime Detach "disconnect" (object ["terminateDebuggee" .= (managed s || fst (startRequest s)=="launch")])
      pure (clearDialog d) {status="Disconnecting debugger..."}
    (watchAction,button:expression:_) | Just suffix<-T.stripPrefix "source-watch:" watchAction,Just ident<-readMaybe (T.unpack suffix),Just (current,captured)<-sourceWatchDialog s,ident==current->do
      modifyIORef' ref (\state->state {sourceWatchDialog=Nothing})
      valid<-sourceCurrent captured d
      if button/="0" then pure d else if not valid then pure d {status="Watch source changed; open its context menu again."}
        else storeWatch runtime Nothing expression
          (case debugSourceCanonical captured of Just path->Just path; Nothing->M.lookup (debugSourceBuffer captured) (buffers d) >>= documentOrigin)
          (protectedBuffer d (debugSourceBuffer captured)) d
    (watchAction,button:expression:_) | Just suffix<-T.stripPrefix "watch-edit:" watchAction,Just ident<-readMaybe (T.unpack suffix),Just (current,target)<-watchDialog s,ident==current->do
      modifyIORef' ref (\state->state {watchDialog=Nothing})
      if button/="0" then pure d else storeWatch runtime target expression Nothing False d
    ("breakpoint",_) -> toggleBreakpoint runtime d
    ("breakpoints",_) -> do
      let rows=[object ["key" .= key,"line" .= bpLine bp] | (key,_,bp)<-allBreakpoints s]
          labels=[sourceLabel src<>":"<>tshow (bpLine bp)<>if flag "verified" (bpResult bp) then " verified at "<>tshow (integer "line" (bpResult bp)) else " pending "<>text "message" (bpResult bp) | (_,src,bp)<-allBreakpoints s]
      shown<-showChoices runtime "Breakpoints" "remove-breakpoint" rows labels d
      pure shown {dialog=fmap (\dg -> dg {buttons=["Remove","Cancel"]}) (dialog shown)}
    ("exceptions",_) | ready s ->
      let filters=items "exceptionBreakpointFilters" (capabilities s) in
      pure $ if null filters then d {status="This debugger advertises no exception filters."} else
        d {dialog=Just (Dialog "Exception breakpoints" (DebugDialog (token s "exceptions"))
           [CheckBox (text "label" f) (text "filter" f `elem` exceptionFilters s) | f<-filters] 0 ["OK","Cancel"] [])}
    ("exception-info",_) | stopped s,Just tid<-thread s ->
      if flag "supportsExceptionInfoRequest" (capabilities s) then
        send runtime ExceptionDetails "exceptionInfo" (object ["threadId" .= tid]) >> pure d {status="Loading exception details..."}
      else pure d {status="This debugger does not support exception details."}
    ("threads",_) | configured s -> send runtime (Threads True) "threads" (object []) >> pure d {status="Loading threads..."}
    ("stack",_) | stopped s, Just tid<-thread s -> sendStack runtime True tid >> pure d {status="Loading call stack..."}
    ("scopes",_) | stopped s, Just selected<-frame s,Just ident<-(field "id" selected :: Maybe Int) ->
      send runtime (Scopes (frameRevision s)) "scopes" (object ["frameId" .= ident]) >> pure d {status="Loading scopes..."}
    (command,_) | command `elem` ["continue","next","stepIn","stepOut","pause"],ready s,Just tid<-thread s,
                  (command=="pause" && not (stopped s)) || (command/="pause" && stopped s) -> do
      when (command/="pause") (modifyIORef' ref invalidate)
      send runtime (Control (stopped s)) command (object ["threadId" .= tid])
      pure (clearDialog d) {status=if command=="pause" then "Pausing..." else "Running..."}
    _ | Just (epoch,choice)<-parseToken action,epoch==generation s -> select runtime action choice values d
      | "select:" `T.isPrefixOf` action -> pure (clearDialog d) {status="Debugger selection expired."}
      | otherwise -> pure d {status="Debugger is not ready for this command."}

data Transport = TCP Text Int | Stdio FilePath [String] | Server FilePath [String] Text Int
data LaunchConfig = LaunchConfig Transport Text Value Text

parseLaunch :: Value -> Parser LaunchConfig
parseLaunch = withObject "debugger configuration" $ \o -> do
  command<-o .:? "command"
  server<-o .:? "server"
  host<-o .:? "host" .!= "127.0.0.1"
  port<-o .:? "port" .!= 4711
  requestName<-o .:? "request" .!= "launch"
  arguments<-o .:? "arguments" .!= object []
  adapter<-o .:? "adapterId" .!= "hide"
  unless (requestName `elem` ["launch","attach"]) (fail "request must be launch or attach")
  case arguments of Object _ -> pure (); _ -> fail "arguments must be a JSON object"
  let argv label values=case values of
        exe:args | not (null exe),all (notElem '\0') values -> pure (exe,args)
        _ -> fail (label++" must be a nonempty argv array without NUL bytes")
      endpointValid=unless (host `elem` ["localhost","127.0.0.1","::1"] && port>0 && port<=65535)
        (fail "host/port must name a loopback DAP endpoint")
  transport<-case (command,server) of
    (Just _,Just _) -> fail "choose command or server, not both"
    (Just values,Nothing) -> do
      when (KM.member "host" o || KM.member "port" o) (fail "choose command or host/port, not both")
      uncurry Stdio <$> argv "command" values
    (Nothing,Just values) -> do
      endpointValid
      (exe,args)<-argv "server" values
      pure (Server exe args host port)
    (Nothing,Nothing) -> endpointValid >> pure (TCP host port)
  pure (LaunchConfig transport requestName arguments adapter)

startSession :: Debugger -> FilePath -> LaunchConfig -> Desktop -> IO Desktop
startSession runtime@(Debugger ref _ _ _ _) directory (LaunchConfig transport requestName arguments adapter) d = do
  -- Release an earlier owned listener before testing its port for the new session.
  readIORef ref >>= stopTransport runtime
  (c,address,owned)<-case transport of
    Stdio exe args -> (,,) <$> D.startAdapterAfter (awaitRetired runtime) exe args directory <*> pure ("127.0.0.1",4711) <*> pure False
    TCP host port -> (,,) <$> D.startClientAfter (awaitRetired runtime) host port <*> pure (host,port) <*> pure False
    Server exe args host port -> (,,) <$> D.startManagedWithAfter (awaitRetired runtime) (pure (exe,args,[])) directory host port <*> pure (host,port) <*> pure True
  started<-initializeSession runtime directory c address requestName arguments adapter owned d
  when (adapter=="hdb") $ modifyIORef' ref (\state -> state {hdbLauncher=case transport of
    Server executable _ _ _ -> Just executable; _ -> Nothing})
  pure started

launchTarget :: Debugger -> Int -> Desktop -> IO Desktop
launchTarget runtime@(Debugger ref _ _ _ _) port d
  | any (\doc -> documentLabel doc==Nothing && dirty (documentBuffer doc)) (M.elems (buffers d)) =
      pure d {status="Save modified source files before launching the disk build."}
  | otherwise = do
      directory<-resolveBuildRoot d
      settings<-getXdgDirectory XdgConfig "thc-edit"
      config<-Build.loadBuildConfig settings directory
      if Build.buildToolchain config==Build.GHC then launchGHC runtime directory config port d
      else do
        plan<-Build.buildPlan Build.Run config directory (filePath <$> (activeDocument d >>= documentFile))
        case plan of
          Right [(exe,args)] -> do
            let (compilerArgs,guestArgs)=break (=="--") args
                flags=["--dap-port",show port]
            -- Release an earlier managed session before testing its port again.
            readIORef ref >>= stopTransport runtime
            c<-D.startManagedWithAfter (awaitRetired runtime) (pure (exe,compilerArgs++flags++guestArgs,[])) directory "127.0.0.1" port
            started<-initializeSession runtime directory c ("127.0.0.1",port) "attach" (object []) "graalvm" True d
            pure started {status="Starting THC debugger; build output is in Debug / Output..."}
          Left err -> pure d {status=err}
          _ -> pure d {status="THC debugger requires a single runtime launch command."}

-- The entry file chooses the GHC cradle/component. Advanced adapter settings can
-- still use Adapter config; both paths feed the same debugger state machine.
launchGHC :: Debugger -> FilePath -> Build.BuildConfig -> Int -> Desktop -> IO Desktop
launchGHC _ _ config _ d | not (Compilers.recognizedCompiler (Build.buildExecutable config)) =
  pure d {status="hdb uses its own GHC build; a custom compiler requires an explicit Adapter config."}
launchGHC runtime directory config port d = case Build.buildSource d of
  Just path | takeExtension path `elem` [".hs",".lhs"] -> do
    file<-canonicalizePath path
    exists<-doesFileExist file
    if not exists then pure d {status="Save the Haskell entry file before debugging."} else do
      context<-hdbContext d
      queueHdb runtime (GhcLaunch directory config file port context) d
  _ -> pure d {status="Open the Haskell entry file for GHC debugging, or use Adapter config."}

initializeSession :: Debugger -> FilePath -> D.Client -> (Text,Int) -> Text -> Value -> Text -> Bool -> Desktop -> IO Desktop
initializeSession runtime@(Debugger ref _ _ _ _) directory c address requestName arguments adapter owned d = do
  s<-readIORef ref
  stopTransport runtime s
  resetOutputOwner runtime d
  writeIORef ref emptyState {client=Just c,generation=generation s+1,root=directory,endpoint=address,
    watchProvider=watchProvider s,watchExpressions=watchExpressions s,watchCatalogueRevision=watchCatalogueRevision s,nextWatch=nextWatch s,choiceId=choiceId s,followSource=followSource s,sidebarVisible=followSource s,sidebarSession=generation s+1,breakpoints=persistentBreakpoints s,breakModified=breakModified s,startRequest=(requestName,arguments),managed=owned,adapterId=adapter}
  -- Explicit session replacement retires the old dialog nonce.
  pure (if followSource s then clearDialog d else d) {status="Connecting debugger..."}

debuggerConsoles :: Debugger -> C.Consoles
debuggerConsoles (Debugger _ _ (HdbRuntime _ _ _ _ consoles _ _) _ _) = consoles

-- Session ownership survives frontend detach. Stop/replacement also retires a
-- terminal still being prepared, so it cannot appear in a newer session.
stopTransport :: Debugger -> State -> IO ()
stopTransport runtime@(Debugger ref _ (HdbRuntime _ _ _ _ _ retired _) _ _) s = mask $ \restore -> do
  -- No old worker can publish into the replacement session. Process cleanup is
  -- joined only at daemon teardown, outside the desktop lock.
  modifyIORef' ref (\state -> state {client=Nothing,terminalLaunch=Nothing,debugConsoles=[],sourcePreparing=Nothing,watchPreparing=Nothing,debugCradle=Nothing})
  cleanups<-mapM (C.retireConsole (debuggerConsoles runtime)) (debugConsoles s)
  when (isJust (client s) || isJust (terminalLaunch s) || isJust (sourcePreparing s) || isJust (watchPreparing s) || not (null cleanups) || isJust (debugCradle s)) $ do
    task<-async $ restore $ flip finally
      (mapM_ D.stopClient (client s) `finally` mapM_ (void . tryIOError . removeFile) (debugCradle s)) $ do
      forM_ (terminalLaunch s) $ \(_,_,worker,result) -> do
        cancel worker
        completed<-atomically (tryTakeTMVar result)
        forM_ completed (mapM_ C.closePreparedConsole)
      forM_ (sourcePreparing s) $ \(SourcePreparation _ _ _ _ _ _ worker)->cancel worker
      forM_ (watchPreparing s) $ \(WatchPreparation _ worker)->cancel worker
      mapM_ (either (const (pure ())) id) cleanups
    modifyIORef' retired (task:)

reapRetired :: Debugger -> IO ()
reapRetired (Debugger _ _ (HdbRuntime _ _ _ _ _ retired _) _ _) = do
  tasks<-readIORef retired
  running<-filterM (fmap (not . isJust) . poll) tasks
  writeIORef retired running

awaitRetired :: Debugger -> IO ()
awaitRetired (Debugger _ _ (HdbRuntime _ _ _ _ _ retired _) _ _) = readIORef retired >>= mapM_ waitCatch

replyReverse :: Debugger -> Int -> Text -> Either Text Value -> IO ()
replyReverse (Debugger ref _ _ _ _) ident command result = do
  s<-readIORef ref
  forM_ (client s) $ \connection -> do
    sent<-try (D.respond connection ident command result)
    case sent of
      Left (err::IOException) -> modifyIORef' ref (\state -> state {failure=Just (T.pack (show err))})
      Right () -> pure ()

terminalArguments :: State -> Value -> Parser ([String],Terminal.TerminalConfig)
terminalArguments s = withObject "runInTerminal" $ \o -> do
  kind<-o .:? "kind" .!= ("integrated"::Text)
  unless (kind=="integrated") (fail "Only integrated debugger terminals are supported")
  shell<-o .:? "argsCanBeInterpretedByShell" .!= False
  when shell (fail "Shell-interpreted debugger arguments are unsupported")
  argv<-o .: "args"
  (command,args)<-case argv of [] -> fail "Debugger terminal command is empty"; command:args -> pure (command,args)
  directory<-o .: "cwd"
  let cwd=if null directory then root s else if isAbsolute directory then directory else root s </> directory
  requested<-o .:? "env" .!= M.empty :: Parser (M.Map String (Maybe String))
  let environment=M.union requested (M.fromList [(key,Just value) | (key,value)<-debugEnvironment s])
  -- hdb's reverse request names its inner binary via getExecutablePath. The
  -- distributed launcher supplies GHC's shared-library paths and ABI checks;
  -- launching that inner binary directly loses those on macOS/Linux.
  let executable=case (hdbLauncher s,args) of
        (Just launcher,subcommand:_) | managed s && adapterId s=="hdb" && subcommand `elem` ["external-interpreter","proxy"] -> launcher
        _ -> command
  pure ([key | (key,Nothing)<-M.toList environment],Terminal.TerminalConfig executable args
    [(key,value) | (key,Just value)<-M.toList environment] cwd 80 24)

tickTerminalLaunch :: Debugger -> Desktop -> IO Desktop
tickTerminalLaunch runtime@(Debugger ref _ _ _ _) d = do
  s<-readIORef ref
  case terminalLaunch s of
    _ | isJust (endedAt s) || isJust (disconnectAt s) -> pure d
    Nothing -> pure d
    Just (ident,command,task,result) -> do
      finished<-poll task
      case finished of
        Nothing -> pure d
        Just outcome -> do
          modifyIORef' ref (\state -> state {terminalLaunch=Nothing})
          prepared<-atomically (tryTakeTMVar result)
          case prepared of
            Just (Right console) -> do
              (terminal,opened)<-C.adoptConsole (debuggerConsoles runtime) console d
              modifyIORef' ref (\state -> state {debugConsoles=terminal:debugConsoles state})
              pid<-C.consoleProcessId (debuggerConsoles runtime) terminal
              replyReverse runtime ident command (fmap (\value -> object ["processId" .= value]) pid)
              pure opened {status="Debugger program input and output: Terminal "<>terminal}
            _ -> do
              let detail=case prepared of Just (Left err) -> err; _ -> "Debugger terminal preparation failed: "<>T.pack (show outcome)
              replyReverse runtime ident command (Left detail)
              pure d {status=detail}

-- Frame and variable handles are scoped to a suspended execution state.
invalidate :: State -> State
invalidate s=s {generation=generation s+1,frameRevision=frameRevision s+1,stopped=False,frame=Nothing,frames=[],choices=M.empty,variableRefs=M.empty,sidebarPages=M.empty,sidebarThreads=M.empty,sidebarFrames=M.empty,sidebarReferences=M.empty,sourceReferences=M.empty}

send :: Debugger -> Pending -> Text -> Value -> IO ()
send (Debugger ref clock _ _ _) kind command arguments = do
  s<-readIORef ref
  forM_ (client s) $ \c -> do
    result<-try (D.request c command arguments)
    now<-clock
    case result of
      Left (err::IOException) -> modifyIORef' ref (\state -> state {failure=Just ("DAP: "<>T.pack (show err))})
      Right ident -> modifyIORef' ref (\state -> state {pending=M.insert ident (kind,generation state,now) (pending state),
        breakRequests=case kind of Breaks key _ -> M.insert key ident (breakRequests state); _ -> breakRequests state})

-- Capture presentation revision after a stop/thread transition. The stack
-- target remains explicit even when another thread is selected while it waits.
sendStack :: Debugger -> Bool -> Int -> IO ()
sendStack runtime@(Debugger ref _ _ _ _) showPicker tid=do
  current<-readIORef ref
  send runtime (Stack showPicker tid (frameRevision current)) "stackTrace" (stackArguments tid)

-- | Adopt DAP events, expire pending operations and advance resource retirement.
tickDebugger :: Debugger -> Desktop -> IO Desktop
tickDebugger runtime original = do
  updated<-tickDebuggerOwner runtime original
  publishSidebarEpoch runtime
  drainSidebarReads runtime updated
  pure updated

tickDebuggerOwner :: Debugger -> Desktop -> IO Desktop
tickDebuggerOwner runtime@(Debugger ref clock _ _ _) original = do
  reapRetired runtime
  prepared<-tickTerminalLaunch runtime original
  starting<-tickHdb runtime prepared >>= C.tickConsoles (debuggerConsoles runtime)
  reporting<-tickOutputOwner runtime starting
  s<-readIORef ref
  events<-if outputPending s then pure [] else maybe (pure []) D.pollEvents (client s)
  accepted<-evaluate (output s)
  -- The pending payload lives only in this already-bounded transport batch and
  -- then the output worker. Public State.output is always the accepted copy.
  (receivedEvents,batch,changed,opening)<-foldM receiveBatch (reporting,accepted,False,False) events
  when changed (queueOutput runtime opening batch)
  sourced<-tickSourcePreparation runtime receivedEvents
  received<-tickWatchPreparation runtime sourced
  updated<-tickOutputOwner runtime received
  now<-clock
  current<-readIORef ref
  let deadline kind=if kind==Attach && fst (startRequest current)=="launch" then 120000000000 else 15000000000
      expired=M.filter (\(kind,_,sent) -> now-sent>deadline kind) (pending current)
  let detachExpired=maybe False (\sent -> now-sent>1000000000) (disconnectAt current)
      timedOut=not (M.null expired)
      completionDue=isJust (client current) && maybe False
        (\sent -> now-sent>1000000000) (endedAt current)
  if completionDue then do
    stopTransport runtime current
    modifyIORef' ref (\state -> state {client=Nothing,connected=False,pending=M.empty})
    pure updated {status=completionStatus current}
  else if not timedOut && not detachExpired && failure current==Nothing then pure updated else do
    stopTransport runtime current
    modifyIORef' ref (\state -> (invalidate state) {client=Nothing,connected=False,pending=M.empty,ready=False,configured=False,disconnectAt=Nothing,failure=Nothing})
    pure (automaticDesktop current updated) {status=fromMaybe (if detachExpired then "Debugger disconnected." else "DAP request timed out; debugger disconnected.") (failure current)}
  where
    receiveBatch (desktop,batch,changed,opening) event=do
      current<-readIORef ref
      case event of
        D.Notification "output" body | isJust (client current)->do
          chunk<-evaluate (text "output" body)
          let first=not (outputShown current) && null (debugConsoles current) && not (T.null chunk)
          modifyIORef' ref (\state->state {outputShown=outputShown state || first})
          pure (desktop,T.takeEnd 16384 (batch<>chunk),True,opening || first)
        _->do
          updated<-receive runtime desktop event
          pure (updated,batch,changed,opening)

receive :: Debugger -> Desktop -> D.Event -> IO Desktop
receive runtime@(Debugger ref clock _ _ _) d event = do
  s<-readIORef ref
  case event of
    _ | Nothing<-client s -> pure d
    D.Connected -> do
      modifyIORef' ref (\state -> state {connected=True})
      -- Managed THC may still be compiling until the transport becomes ready.
      unless (disconnectAt s/=Nothing) $ do
        send runtime Init "initialize" (object
          ["clientID" .= ("hide"::Text),"clientName" .= ("Haskell"::Text),"adapterID" .= adapterId s,
           "pathFormat" .= ("path"::Text),"linesStartAt1" .= True,"columnsStartAt1" .= True,
           "supportsVariableType" .= True,"supportsRunInTerminalRequest" .= Terminal.terminalAvailable,
           "supportsVariablePaging" .= False,"supportsMemoryReferences" .= False,"supportsInvalidatedEvent" .= True])
      pure d
    D.ReverseRequest ident command arguments ->
      if command=="runInTerminal" && Terminal.terminalAvailable && not (isJust (endedAt s)) && disconnectAt s==Nothing
      then case parseEither (terminalArguments s) arguments of
        Left err -> replyReverse runtime ident command (Left (T.pack err)) >> pure d
        Right (unset,config) -> case terminalLaunch s of
          Just _ -> replyReverse runtime ident command (Left "A debugger terminal is already starting") >> pure d
          Nothing -> do
            result<-newEmptyTMVarIO
            task<-async $ mask_ $ do
              prepared<-C.prepareConsole unset config (1024*1024)
              atomically (putTMVar result prepared)
            modifyIORef' ref (\state -> state {terminalLaunch=Just (ident,command,task,result)})
            pure d
      else replyReverse runtime ident command (Left "Unsupported debugger reverse request") >> pure d
    D.Disconnected reason -> do
      stopTransport runtime s
      modifyIORef' ref (\state -> (invalidate state) {client=Nothing,connected=False,pending=M.empty,ready=False,configured=False,disconnectAt=Nothing,failure=Nothing})
      pure (automaticDesktop s d) {status=if isJust (endedAt s) then completionStatus s else "DAP: "<>reason}
    D.Notification name _ | isJust (endedAt s), name `notElem` ["terminated","exited","output"] -> pure d
    D.Notification "initialized" _ -> do
      modifyIORef' ref (\state -> state {ready=True})
      configure runtime
      pure d
    D.Notification "capabilities" body -> do
      modifyIORef' ref (\state -> state {capabilities=merge (capabilities state) (fromMaybe Null (field "capabilities" body))})
      pure d
    D.Notification "stopped" body -> do
      let tid=field "threadId" body
      modifyIORef' ref (\state -> (invalidate state) {stopped=True,thread=tid})
      when (configured s) $ do
        send runtime (Threads False) "threads" (object [])
        forM_ tid (\ident -> sendStack runtime False ident)
      pure (automaticDesktop s d) {status="Stopped: "<>text "reason" body}
    D.Notification "invalidated" body -> do
      let areas=fromMaybe [] (field "areas" body) :: [Text]
          allAreas=null areas || "all" `elem` areas
          threads=allAreas || "threads" `elem` areas
          stacks=threads || "stacks" `elem` areas
      if not (stacks || "variables" `elem` areas) then pure d else do
        modifyIORef' ref (\state -> state {generation=generation state+1,frameRevision=frameRevision state+1,choices=M.empty,variableRefs=M.empty,sidebarPages=M.empty,sidebarThreads=M.empty,sidebarFrames=M.empty,sidebarReferences=M.empty,sourceReferences=M.empty,
          thread=if threads then Nothing else thread state,
          frame=if stacks then Nothing else frame state,frames=if stacks then [] else frames state})
        if threads then send runtime (Threads False) "threads" (object [])
        else when (stacks && stopped s) $ forM_ (thread s) (\tid -> sendStack runtime False tid)
        pure (retireBackgroundDialog d) {status="Debugger values changed; request scopes again."}
    D.Notification "continued" _ -> modifyIORef' ref invalidate >> pure (automaticDesktop s d) {status="Running..."}
    D.Notification "thread" body -> do
      -- DAP thread changes can retire stack owners without a continued event.
      -- Conservatively expire the stop's references, retaining stopped state and
      -- selection when its thread survives. Source/picker replies use the same
      -- new epoch; no stale child page can revive an exited thread.
      when (stopped s) $ modifyIORef' ref (\state->state
        { generation=generation state+1,frameRevision=frameRevision state+1
        , choices=M.empty,variableRefs=M.empty,sidebarPages=M.empty
        , sidebarThreads=M.empty,sidebarFrames=M.empty,sidebarReferences=M.empty,sourceReferences=M.empty
        , thread=if exitedSelection then Nothing else thread state
        , frame=if exitedSelection then Nothing else frame state
        , frames=if exitedSelection then [] else frames state })
      when (configured s) (send runtime (Threads False) "threads" (object []))
      pure d
      where exitedSelection=text "reason" body=="exited" && field "threadId" body==thread s

    D.Notification "terminated" _ -> do
      now<-clock
      mapM_ (\(kind,_,_) -> completeInspection kind (Left "Debug session ended.")) (M.elems (pending s))
      -- Some adapters send exited (or a failed launch response) after terminated.
      -- Stop accepting controls now, but drain those final facts for at most 1s.
      modifyIORef' ref (\state -> (invalidate state) {endedAt=Just (fromMaybe now (endedAt state)),
        pending=M.filter (\(kind,_,_) -> kind==Init || kind==Attach || kind==Configure) (pending state),
        ready=False,configured=False,disconnectAt=Nothing,failure=Nothing})
      pure (automaticDesktop s d) {status="Debug session ended."}
    D.Notification "exited" body -> do
      modifyIORef' ref (\state -> state {programExitCode=field "exitCode" body})
      pure d
    D.Notification "breakpoint" body -> do
      let bp=fromMaybe Null (field "breakpoint" body)
      modifyIORef' ref (\state -> state {breakpoints=M.map (\(source,points) -> (source,map (updateBreakpoint bp) points)) (breakpoints state)})
      pure d
    D.Notification _ _ -> pure d
    D.Response ident result -> case M.lookup ident (pending s) of
      Nothing -> pure d
      Just (kind,epoch,_) -> do
        watchLive<-watchProviderCurrent s
        modifyIORef' ref (\state -> state {pending=M.delete ident (pending state)})
        if (case kind of WatchRequest{}->not watchLive; _->False) || (stale kind && epoch/=generation s) || selectionExpired s kind || sourceInspectionExpired s d kind || (case kind of Breaks key _ -> M.lookup key (breakRequests s)/=Just ident; _ -> False) then do
          completeInspection kind (Left "Debugger inspection expired; refresh debug_status.")
          pure d
        else case kind of
          WatchRequest operation backing base private->prepareWatch runtime operation backing base private result >> pure d
          SourceInspection _ _ _ reply->void (tryPutMVar reply result) >> pure d
          SidebarRead (DebugPageRequest _ target _) reply -> do
            case target of DebugStack{}->forM_ result (recordSourceReferences ref); _->pure ()
            atomically (void (tryPutTMVar reply (AdapterPage <$> result)))
            pure d
          Inspection command reply -> do
            forM_ result $ \body->do
              recordVariables ref command body
              when (command=="stackTrace") (recordSourceReferences ref body)
            _<-tryPutMVar reply (result >>= boundedResult)
            pure d
          _ -> case result of
           Left err -> do
             when (kind==Init || kind==Attach || kind==Configure) $ do
               stopTransport runtime s
               modifyIORef' ref (\state -> (invalidate state) {client=Nothing,connected=False,pending=M.empty,ready=False,configured=False,disconnectAt=Nothing,failure=Nothing})
             case kind of
               Control wasStopped | wasStopped -> do
                 modifyIORef' ref (\state -> state {stopped=True})
                 forM_ (thread s) (\tid -> sendStack runtime False tid)
               _ -> pure ()
             pure (automaticDesktop s d) {status="DAP: "<>err}
           Right body -> if isJust (endedAt s) then pure d else response runtime kind body d
  where
    stale kind=case kind of Threads{} -> True; Control{} -> True; Stack{} -> True; Scopes{} -> True; Variables{} -> True; ExceptionDetails -> True; Source{} -> True; Inspection{} -> True; SourceInspection{} -> True; SidebarRead{} -> True; WatchRequest{} -> True; _ -> False

-- Selection controls presentation/source follow only. Stopped handles remain
-- valid for sibling frames until the stop epoch itself expires.
selectionExpired :: State -> Pending -> Bool
selectionExpired s kind=case kind of
  Scopes revision -> revision/=frameRevision s
  Variables revision -> revision/=frameRevision s
  Source _ revision _ _ -> revision/=frameRevision s
  WatchRequest (WatchOperation ident revision receipt _) _ _ _->not (watchCurrent s ident revision receipt)
  _ -> False

-- Captured backing paths are resolved only on a waiting tool/source worker.
canonicalSourcePath :: FilePath -> Maybe FilePath -> IO (Either Text (Maybe FilePath))
canonicalSourcePath _ Nothing=pure (Right Nothing)
canonicalSourcePath _ (Just "")=pure (Right Nothing)
canonicalSourcePath _ (Just path) | length (take 4097 path)>4096 || '\0' `elem` path=pure (Left "Invalid debugger source path.")
canonicalSourcePath base (Just path)=do
  result<-try (canonicalizePath (if isAbsolute path then path else base </> path))
  pure $ either (\(_::IOException)->Left "Debugger source path is unavailable.") (Right . Just) result

sourceStampCurrent :: State -> Int -> Int -> Bool
sourceStampCurrent s reference stamp=case M.lookup reference (sourceReferences s) of
  Just (SourceObservation current _)->current==stamp
  _->False

-- A generated buffer retains the exact stop observation which produced it.
-- Re-observing the same numeric handle cannot authorize an older document.
liveBufferSource :: State -> Int -> Maybe Value
liveBufferSource s bid=do
  (epoch,stamp,source)<-M.lookup bid (sources s)
  if epoch==generation s && stopped s && sourceStampCurrent s (integer "sourceReference" source) stamp
    then Just source else Nothing

sourceInspectionExpired :: State -> Desktop -> Pending -> Bool
sourceInspectionExpired s d (SourceInspection reference stamp origin _)=not (stopped s && sourceStampCurrent s reference stamp) || maybe False (protectedPath d) origin
sourceInspectionExpired s _ (Source _ _ stamp selected)=not (stopped s && sourceStampCurrent s (integer "sourceReference" (fromMaybe Null (field "source" selected))) stamp)
sourceInspectionExpired _ _ _=False

-- Observe only bounded handles supplied by current stopped stack metadata.
-- A changed backing path rotates the scalar stamp; no adapter payload equality
-- participates in identity. Canonical path preparation is a worker operation.
recordSourceReferences :: IORef State -> Value -> IO ()
recordSourceReferences ref body=modifyIORef' ref $ \s->foldl' observe s (take 1000 (items "stackFrames" body))
  where
    observe s row=case field "source" row of
      Just source | reference>0,validPath, M.member reference (sourceReferences s) || M.size (sourceReferences s)<32768 ->
        case M.lookup reference (sourceReferences s) of
          Just (SourceObservation _ oldPath) | oldPath==path->s
          _->s {sourceReferences=M.insert reference (SourceObservation (nextSourceObservation s) path) (sourceReferences s),nextSourceObservation=nextSourceObservation s+1}
        where
          reference=integer "sourceReference" source
          rawPath=case (field "path" source :: Maybe Text) of Just ""->Nothing; value->value
          path=T.unpack <$> rawPath
          validPath=maybe True (\value->T.compareLength value 4096/=GT && not (T.any (=='\0') value)) rawPath
      _->s

-- DAP lazy handles are executable: hdb forces a thunk when its children are
-- requested. Track provenance for both UI and MCP; never guess a reference.
recordVariables :: IORef State -> Text -> Value -> IO ()
recordVariables ref command body
  | command `elem` ["scopes","variables"] = do
      let refs=M.fromListWith (||)
            [(ident,command=="variables" && maybe False (flag "lazy") (field "presentationHint" row))
            | row<-items command body,let ident=integer "variablesReference" row,ident>0]
      modifyIORef' ref (\state -> state {variableRefs=M.unionWith (||) refs (variableRefs state)})
  | otherwise = pure ()

configure :: Debugger -> IO ()
configure runtime@(Debugger ref _ _ _ _) = do
  s<-readIORef ref
  when (ready s && capabilities s/=Null && not (configured s)) $ do
    modifyIORef' ref (\state -> state {configured=True})
    forM_ (M.toList (breakpoints s)) $ \(key,(source,points)) -> sendBreakpoints runtime key source points
    when (not (null (items "exceptionBreakpointFilters" (capabilities s)))) $
      send runtime Exceptions "setExceptionBreakpoints" (object ["filters" .= exceptionFilters s])
    when (flag "supportsConfigurationDoneRequest" (capabilities s)) $
      send runtime Configure "configurationDone" (object [])

response :: Debugger -> Pending -> Value -> Desktop -> IO Desktop
response runtime@(Debugger ref _ _ _ _) kind body d = do
  s<-readIORef ref
  case kind of
    Init -> do
      let filters=[text "filter" f | f<-items "exceptionBreakpointFilters" body,flag "default" f]
      modifyIORef' ref (\state -> state {capabilities=body,exceptionFilters=filters})
      let (command,arguments)=startRequest s
      send runtime Attach command arguments
      configure runtime
      pure d {status=if fst (startRequest s)=="launch" then "Launching debugger..." else "Attaching debugger..."}
    Attach -> do
      send runtime (Threads False) "threads" (object [])
      when (stopped s) $ forM_ (thread s) (\tid -> sendStack runtime False tid)
      pure d
    Configure -> pure d
    Breaks key requested -> do
      modifyIORef' ref (\state -> state {breakpoints=M.adjust (\(source,points) -> (source,if map bpLine points==requested then zipWith (\p value -> p {bpResult=value}) points (items "breakpoints" body++repeat Null) else points)) key (breakpoints state)})
      pure d
    Exceptions -> pure d
    ExceptionDetails -> pure (addReadOnly "Debugger exception" (exceptionText body) d)
    Threads showPicker -> do
      recordSidebarResponse ref (DebugPageRequest (generation s) DebugThreads 0) body
      let rows=items "threads" body
          tid=case thread s of Just ident | any ((==Just ident).field "id") rows -> Just ident; _ -> listToMaybe rows >>= field "id"
      modifyIORef' ref (\state -> state {thread=tid})
      when (stopped s && thread s==Nothing) $ forM_ tid (\ident -> sendStack runtime False ident)
      if showPicker then showChoices runtime "Threads" "thread" rows (map (text "name") rows) d else pure d
    Stack _ tid selectedRevision | tid/=fromMaybe (-1) (thread s) || selectedRevision/=frameRevision s -> pure d
    Stack showPicker _ _ -> do
      recordSourceReferences ref body
      let rows=items "stackFrames" body
      modifyIORef' ref (\state -> state {frame=listToMaybe rows,frames=rows})
      if showPicker then showChoices runtime "Call stack" "frame" rows (map frameLabel rows) d
      else if not (followSource s) then pure d
      else maybe (pure d {status="Stopped; no source frame supplied."}) (openFrame runtime False Nothing d) (listToMaybe rows)
    Scopes _ -> do
      recordVariables ref "scopes" body
      let rows=items "scopes" body
      showChoices runtime "Scopes" "expand" rows (map (text "name") rows) d
    Variables _ -> do
      recordVariables ref "variables" body
      let rows=items "variables" body
      showChoices runtime "Variables" "expand" rows (map variableLabel rows) d
    Source explicit revision stamp selected
      | not explicit && not (followSource s) -> pure d
      | otherwise -> case (field "content" body,M.lookup reference (sourceReferences s)) of
          (Just content,Just (SourceObservation current path)) | stamp==current->do
            retireSourcePreparation runtime
            mask_ $ do
              worker<-asyncWithUnmask $ \unmask->unmask $ do
                origin<-canonicalSourcePath (root s) path
                case (origin,boundedResult body) of
                  (Left err,_)->pure (Left err)
                  (_,Left err)->pure (Left err)
                  (Right canonical,Right _)->do
                    let prepared=newBuffer (T.copy content)
                        row=max 0 (min (bufferLineCount prepared-1) (integer "line" selected-1))
                        offset=bufferLineOffset prepared row+L.positionOffset (bufferLineAt prepared row) (0,max 0 (integer "column" selected-1))
                    _<-evaluate (prepareBuffer prepared)
                    _<-evaluate offset
                    pure (Right (AdapterSource canonical prepared offset))
              modifyIORef' ref (\state->state {sourcePreparing=Just (SourcePreparation (generation s) revision (Just (reference,stamp)) explicit selected Nothing worker)})
              pure d
          _->pure d {status="DAP source response is unavailable or expired."}
      where reference=integer "sourceReference" (fromMaybe Null (field "source" selected))
    Control _ -> pure d
    WatchRequest{}->pure d
    SourceInspection _ _ _ reply->void (tryPutMVar reply (Right body)) >> pure d
    Inspection command reply -> do
      recordVariables ref command body
      when (command=="stackTrace") (recordSourceReferences ref body)
      _<-tryPutMVar reply (boundedResult body)
      pure d
    SidebarRead _ reply -> atomically (void (tryPutTMVar reply (Right (AdapterPage body)))) >> pure d
    Detach -> do
      stopTransport runtime s
      modifyIORef' ref (\state -> (invalidate state) {client=Nothing,connected=False,pending=M.empty,ready=False,configured=False,disconnectAt=Nothing})
      pure (clearDialog d) {status=if managed s || fst (startRequest s)=="launch" then "Debugger disconnected; launched session stopped." else "Debugger disconnected; attached program is not terminated."}

select :: Debugger -> Text -> Text -> [Text] -> Desktop -> IO Desktop
select runtime@(Debugger ref _ _ _ _) fullToken action values d = do
  s<-readIORef ref
  let rows=fromMaybe [] (M.lookup fullToken (choices s))
      selected=case values of _:index:_ -> readMaybe (T.unpack index); _ -> Nothing
  case action of
    "remove-breakpoint" | Just chosen<-selected >>= at rows,let key=text "key" chosen,Just (src,points)<-M.lookup key (breakpoints s) -> do
      let remaining=filter ((/=integer "line" chosen).bpLine) points
      modifyIORef' ref (\state -> state {breakpoints=M.insert key (src,remaining) (breakpoints state)})
      when (configured s) (sendBreakpoints runtime key src remaining)
      pure d {status="Breakpoint removed."}
    "thread" | Just chosen<-selected >>= at rows,Just tid<-field "id" chosen -> do
      modifyIORef' ref (\state -> state {frameRevision=frameRevision state+1,thread=Just tid,frame=Nothing,frames=[],choices=M.empty})
      when (stopped s) (sendStack runtime True tid)
      pure d
    "frame" | stopped s,Just chosen<-selected >>= at rows -> do
      modifyIORef' ref (\state -> state {frame=Just chosen,frameRevision=frameRevision state+1,choices=M.empty})
      openFrame runtime True Nothing d chosen
    "expand" | stopped s,Just chosen<-selected >>= at rows,let ident=integer "variablesReference" chosen,ident>0 ->
      case M.lookup ident (variableRefs s) of
        Just False -> send runtime (Variables (frameRevision s)) "variables" (object ["variablesReference" .= ident]) >> pure d {status="Loading variables..."}
        Just True -> pure d {status="Lazy variable requires explicit evaluation; expansion does not force it."}
        Nothing -> pure d {status="Debugger value expired; request scopes again."}
    "exceptions" -> do
      let filters=items "exceptionBreakpointFilters" (capabilities s)
          selectedFilters=[text "filter" f | (f,"true")<-zip filters (drop 1 values)]
      modifyIORef' ref (\state -> state {exceptionFilters=selectedFilters})
      send runtime Exceptions "setExceptionBreakpoints" (object ["filters" .= selectedFilters])
      pure d {status="Exception breakpoints updated."}
    _ -> pure d {status="No expandable debugger value selected."}

openFrame :: Debugger -> Bool -> Maybe [FilePath] -> Desktop -> Value -> IO Desktop
openFrame runtime@(Debugger ref _ _ _ _) explicit private d selected = do
  s<-readIORef ref
  let source=fromMaybe Null (field "source" selected)
      reference=integer "sourceReference" source
      path=text "path" source
      local=if isAbsolute (T.unpack path) then T.unpack path else root s </> T.unpack path
  if reference>0 then case M.lookup reference (sourceReferences s) of
    Just (SourceObservation stamp _) | stopped s->send runtime (Source explicit (frameRevision s) stamp selected) "source" (object ["source" .= source,"sourceReference" .= reference]) >> pure d
    _->pure d {status="Debugger source handle expired."}
  else if T.compareLength path 4096==GT || T.any (=='\0') path then pure d {status="Invalid debugger source path."}
  else if T.null path || not (stopped s) then pure d {status="Stopped: source is unavailable."} else do
    retireSourcePreparation runtime
    captured<-mapM capture [(window,doc,file) | window<-windows d,Just doc<-[windowDocument (buffers d) window],Just file<-[documentFile doc]]
    let owner=case find (\(CapturedLocalSource file _ _ _ _ _)->file==local) captured of
          Just (CapturedLocalSource _ wid _ _ _ _)->Just wid
          Nothing->windowId <$> activeWindow d
    mask_ $ do
      worker<-asyncWithUnmask (\unmask->unmask (prepareLocalSource (root s) path selected private captured))
      modifyIORef' ref (\state->state {sourcePreparing=Just (SourcePreparation (generation s) (frameRevision s) Nothing explicit selected owner worker)})
      pure d
  where
    capture (window,doc,file)=do
      version<-captureVersion (documentBuffer doc)
      image<-evaluate (bufferContent (documentBuffer doc))
      modified<-evaluate (captureDirty (documentBuffer doc))
      pure (CapturedLocalSource (filePath file) (windowId window) (fromMaybe (error "source window without buffer") (bufferId window)) version image modified)

-- The existing source worker owns canonical paths, disk decoding, dirty-state
-- evaluation and measured UTF-16 positioning. An open source needs no disk read.
prepareLocalSource :: FilePath -> Text -> Value -> Maybe [FilePath] -> [CapturedLocalSource] -> IO (Either Text PreparedSource)
prepareLocalSource base path selected private captured
  | T.compareLength path 4096==GT || T.any (=='\0') path=pure (Left "Invalid debugger source path.")
  | Just opened<-find (\(CapturedLocalSource file _ _ _ _ _)->file==local) captured=existing local opened
  | otherwise=do
      resolved<-canonicalSourcePath base (Just (T.unpack path))
      case resolved of
        Right (Just canonical) | denied canonical->pure (Left "Debugger source is private.")
        Right (Just canonical) -> case find (\(CapturedLocalSource file _ _ _ _ _)->file==canonical) captured of
          Just opened->existing canonical opened
          Nothing->do
            exists<-doesFileExist canonical
            if not exists then pure (Left "Debugger source file is unavailable.") else do
              result<-loadFile canonical
              case result of
                Right (file,buffer) | isJust (diskBytes file)->do
                  _<-evaluate (prepareBuffer buffer)
                  prepare (filePath file) (NewSource file buffer) (bufferContent buffer)
                _->pure (Left "Debugger source file is unavailable.")
        _->pure (Left "Debugger source file is unavailable.")
  where
    local=if isAbsolute (T.unpack path) then T.unpack path else base </> T.unpack path
    denied canonical=maybe False (\paths->protectedFilePath paths canonical) private
    existing canonical (CapturedLocalSource _ wid bid version image modified)
      | denied canonical=pure (Left "Debugger source is private.")
      | otherwise=do
          changed<-evaluate (snapshotDirty modified)
          prepare canonical (ExistingSource wid bid version changed) image
    prepare canonical target image=do
      let row=max 0 (min (contentLineCount image-1) (integer "line" selected-1))
          offset | integer "line" selected<=0 = -1
                 | otherwise=contentLineOffset image row+L.positionOffset (contentLineAt image row) (0,max 0 (integer "column" selected-1))
      _<-evaluate offset
      pure (Right (LocalSource (isJust private) canonical target offset))

-- Only this owner adopts a prepared source. Cancellation/join is retired outside
-- the desktop lock, including superseded selections and stopped generations.
retireSourcePreparation :: Debugger -> IO ()
retireSourcePreparation (Debugger ref _ (HdbRuntime _ _ _ _ _ retired _) _ _)=mask_ $ do
  current<-readIORef ref
  modifyIORef' ref (\state->state {sourcePreparing=Nothing})
  forM_ (sourcePreparing current) $ \(SourcePreparation _ _ _ _ _ _ worker)->do
    cleanup<-asyncWithUnmask (\unmask->unmask (cancel worker))
    modifyIORef' retired (cleanup:)

tickSourcePreparation :: Debugger -> Desktop -> IO Desktop
tickSourcePreparation runtime@(Debugger ref _ _ _ _) d=do
  s<-readIORef ref
  case sourcePreparing s of
    Nothing->pure d
    Just (SourcePreparation epoch selectedRevision observation explicit selected owner worker)
      | epoch/=generation s || selectedRevision/=frameRevision s || not (stopped s && maybe True (uncurry (sourceStampCurrent s)) observation)
        || not (isJust (client s)) || endedAt s/=Nothing || disconnectAt s/=Nothing || not explicit && not (followSource s)->
          retireSourcePreparation runtime >> pure d
      | maybe False (\wid->not (any ((==wid).windowId) (windows d))) owner->do
          retireSourcePreparation runtime
          pure d {status="Debugger source window closed; select the frame again."}
      | isJust (dialog d) || questionActive d->pure d
      | otherwise->do
          result<-poll worker
          case result of
            Nothing->pure d
            Just outcome->do
              modifyIORef' ref (\state->state {sourcePreparing=Nothing})
              case outcome of
                Left err->pure d {status=if isJust observation then "Debugger source preparation failed: "<>T.pack (show err) else "Debugger source preparation failed."}
                Right (Left err)->pure d {status=err}
                Right (Right (LocalSource agent path target offset))->adoptLocalSource agent path target offset selected d
                Right (Right (AdapterSource origin prepared offset))->do
                  let source=fromMaybe Null (field "source" selected)
                      reference=integer "sourceReference" source
                      stamp=maybe 0 snd observation
                      title="Source "<>sourceLabel source<>" ["<>tshow reference<>"]"
                      opened=addReadOnlyBuffer title prepared d
                      bid=fromMaybe (nextId d) (activeWindow opened >>= bufferId)
                      styled=opened {buffers=M.adjust (\doc->doc {documentOrigin=origin,documentSuggestedName=Just (T.unpack (sourceLabel source))}) bid (buffers opened)}
                      private=maybe False (protectedPath d) origin
                  modifyIORef' ref (\state->state {sources=M.insert bid (epoch,stamp,source) (sources state)})
                  pure (moveTo False offset styled) {status=if private then "Stopped in private debugger source." else "Stopped in "<>frameLabel selected}

-- Docs: docs/site/screenshots/debug-step.png (docs/running.md) shows the live stopped source.
-- Only a matching captured source may receive its prepared coordinates. A newly
-- opened target is never overwritten or positioned from a disk snapshot.
adoptLocalSource :: Bool -> FilePath -> LocalSourceTarget -> Int -> Value -> Desktop -> IO Desktop
adoptLocalSource agent path target offset selected d
  | agent && protectedPath d path=pure d {status="Debugger source is private."}
  | otherwise=case target of
    ExistingSource wid bid version changed->case (find ((==wid).windowId) (windows d),M.lookup bid (buffers d)) of
      (Just window,Just doc) | bufferId window==Just bid && fmap filePath (documentFile doc)==Just path->do
        current<-versionCurrent version (documentBuffer doc)
        pure $ if current then position changed (focusWindow wid d) else expired
      _->pure expired
    NewSource file prepared
      | any ((==Just path).fmap filePath.documentFile) (M.elems (buffers d))->pure expired
      | otherwise->pure (position False (addDocument (Just file) prepared d))
  where
    expired=d {status="Debugger source target changed; select the frame again."}
    position changed desktop=(if offset<0 then desktop else moveTo False offset (modifyActive (\window->window {bufferView=CurrentView,reviewSelection=Nothing}) desktop))
      {status=if protectedPath desktop path then "Stopped in private debugger source." else if changed
        then "Stopped; unsaved text may differ from the running source." else "Stopped in "<>frameLabel selected}

sourceCurrent :: DebugSourceRequest -> Desktop -> IO Bool
sourceCurrent request d=case (activeWindow d,activeDocument d) of
  (Just window,Just doc) | bufferView window/=MarkdownView, windowFocused d window, windowId window==debugSourceWindow request,
      bufferId window==Just (debugSourceBuffer request),selection window==debugSourceSelection request,
      (filePath <$> documentFile doc)==debugSourceFile request,not (byteMode (documentBuffer doc))->
    versionCurrent (debugSourceVersion request) (documentBuffer doc)
  _->pure False

sourceAction :: Debugger -> DebugSourceRequest -> Desktop -> IO Desktop
sourceAction runtime@(Debugger ref _ _ _ _) request d=do
  valid<-sourceCurrent request d
  state<-readIORef ref
  let source=case debugSourceCanonical request of
        Just path->Just (object ["path" .= path])
        Nothing->if stopped state && configured state && isJust (client state) && endedAt state==Nothing && disconnectAt state==Nothing
          then liveBufferSource state (debugSourceBuffer request) else Nothing
  if not valid || dialog d/=Nothing then pure d {status="Source action expired."} else case source of
    Nothing->pure d {status="Buffer has no live debugger source."}
    Just captured->case debugSourceOperation request of
      ToggleSourceBreakpoint->toggleBreakpointSource runtime captured (debugSourceRow request) (debugSourceModified request) d
      -- Docs: docs/site/screenshots/debug-add-watch.png (docs/running.md) shows the source-prefilled expression.
      AddSourceWatch->do
        let ident=choiceId state+1
            expression=fromMaybe "" (debugSourceExpression request)
        modifyIORef' ref (\current->current {choiceId=ident,sourceWatchDialog=Just (ident,request)})
        pure d {status="Enter a watch expression.",dialog=Just (Dialog "Add watch" (DebugSourceWatchDialog ident (debugSourceBuffer request)
          (case debugSourceCanonical request of Just path->Just path; Nothing->activeDocument d >>= documentOrigin)
          (protectedBuffer d (debugSourceBuffer request)))
          [SelectedInput "Expression" expression (Selection 0 (T.length expression))] 0 ["Add","Cancel"]
          ["Expression evaluation can execute program code."])}

toggleBreakpointSource :: Debugger -> Value -> Int -> Bool -> Desktop -> IO Desktop
toggleBreakpointSource runtime@(Debugger ref _ _ _ _) source row modified d=do
  state<-readIORef ref
  let key=sourceKey source
      old=maybe [] snd (M.lookup key (breakpoints state))
      removing=any ((==row).bpLine) old
      points=if removing then filter ((/=row).bpLine) old else old++[Breakpoint row Null]
  modifyIORef' ref (\current->current {breakpoints=M.insert key (source,points) (breakpoints current),breakModified=M.insert key modified (breakModified current)})
  when (configured state) (sendBreakpoints runtime key source points)
  pure d {status=if removing then "Breakpoint removed." else "Breakpoint requested at line "<>tshow row<>if modified then "; source has unsaved changes." else "."}

toggleBreakpoint :: Debugger -> Desktop -> IO Desktop
toggleBreakpoint runtime@(Debugger ref _ _ _ _) d = do
  s<-readIORef ref
  case (activeWindow d,activeDocument d) of
    (Just _,Just doc) | byteMode (documentBuffer doc) -> pure d {status="Breakpoints require source text; leave hex mode first."}
    (Just window,Just doc) -> do
      source<-case documentFile doc of
        Just file -> do path<-canonicalizePath (filePath file); pure (Just (object ["path" .= path]))
        Nothing -> pure (bufferId window >>= liveBufferSource s)
      case source of
        Nothing -> pure d {status="Choose a source file or debugger source first."}
        Just src ->toggleBreakpointSource runtime src
          (1+fst (bufferLineColumn (documentBuffer doc) (caret (selection window)))) (dirty (documentBuffer doc)) d
    _ -> pure d {status="Choose a source file first."}

sendBreakpoints :: Debugger -> Text -> Value -> [Breakpoint] -> IO ()
sendBreakpoints runtime@(Debugger ref _ _ _ _) key source points = do
  s<-readIORef ref
  send runtime (Breaks key (map bpLine points)) "setBreakpoints"
    (object ["source" .= source,"breakpoints" .= [object ["line" .= bpLine p] | p<-points],
      "sourceModified" .= M.findWithDefault False key (breakModified s)])

allBreakpoints :: State -> [(Text,Value,Breakpoint)]
allBreakpoints s=[(key,source,bp) | (key,(source,points))<-M.toList (breakpoints s),bp<-points]
updateBreakpoint :: Value -> Breakpoint -> Breakpoint
updateBreakpoint value bp | Just ident<-(field "id" value :: Maybe Int),field "id" (bpResult bp)==Just ident = bp {bpResult=value}
                          | otherwise = bp

persistentBreakpoints :: State -> M.Map Text (Value,[Breakpoint])
persistentBreakpoints = M.map (\(src,points) -> (src,map (\bp -> bp {bpResult=Null}) points)) .
  M.filter (\(src,_) -> integer "sourceReference" src==0 && not (T.null (text "path" src))) . breakpoints

-- Docs: docs/site/screenshots/debug-stack.png (docs/running.md) shows the live frame picker.
showChoices :: Debugger -> Text -> Text -> [Value] -> [Text] -> Desktop -> IO Desktop
showChoices (Debugger ref _ _ _ _) title action rows labels d = do
  s<-readIORef ref
  let key=token s action
      shown=chooser title key labels d
  when (dialog d==Nothing && not (null rows)) $
    modifyIORef' ref (\state -> state {choices=M.singleton key rows,choiceId=choiceId state+1})
  pure shown

chooser :: Text -> Text -> [Text] -> Desktop -> Desktop
chooser title action rows d
  | null rows = d {status=title<>" is empty."}
  | dialog d/=Nothing = d {status=title<>" ready; close the current dialog and request it again."}
  | otherwise = d {dialog=Just (Dialog title (DebugDialog action) [ListBox title rows 0] 0 ["Open","Cancel"] [])}
-- Background protocol events keep the editor's existing modal and focus.
-- Explicit view requests still use the normal source/picker presentation path.
automaticDesktop :: State -> Desktop -> Desktop
automaticDesktop s d=if followSource s then retireBackgroundDialog d else d

-- Persistent expression drafts are watch-revision owned, not stopped-handle
-- inspections. Explicit controls still use clearDialog to cancel their modal.
retireBackgroundDialog :: Desktop -> Desktop
retireBackgroundDialog d=case dialog d of
  Just dg | DebuggerWatchDialog{}<-purpose dg->d
  _->clearDialog d

clearDialog :: Desktop -> Desktop
clearDialog d = case dialog d of
  Just dg | DebugDialog{}<-purpose dg->d {dialog=Nothing}
  Just dg | DebugSourceWatchDialog{}<-purpose dg->d {dialog=Nothing}
  Just dg | DebuggerWatchDialog{}<-purpose dg->d {dialog=Nothing}
  _->d
sourceKey :: Value -> Text
sourceKey source = text "path" source<>"#"<>tshow (integer "sourceReference" source)
sourceLabel :: Value -> Text
sourceLabel source=fromMaybe (fromMaybe "Unavailable source" (field "name" source)) (field "path" source)
frameLabel :: Value -> Text
frameLabel value=text "name" value<>"  "<>maybe "" (T.pack . takeFileName . T.unpack . sourceLabel) (field "source" value)<>if integer "line" value>0 then ":"<>tshow (integer "line" value) else ""
variableLabel :: Value -> Text
variableLabel value=(if integer "variablesReference" value>0 then "+ " else "  ")<>text "name" value<>" = "<>text "value" value<>
  (if T.null (text "type" value) then "" else " : "<>text "type" value)
token :: State -> Text -> Text
token s action="select:"<>tshow (generation s)<>":"<>tshow (choiceId s)<>":"<>action
parseToken :: Text -> Maybe (Int,Text)
parseToken value=case T.splitOn ":" value of ["select",epoch,_,action] -> (,action) <$> readMaybe (T.unpack epoch); _ -> Nothing
stackArguments :: Int -> Value
stackArguments tid=object ["threadId" .= tid,"startFrame" .= (0::Int),"levels" .= (200::Int)]
field :: FromJSON a => Text -> Value -> Maybe a
field key=parseMaybe (withObject "object" (\o -> o .: K.fromText key))
text :: Text -> Value -> Text
text key=fromMaybe "" . field key
integer :: Text -> Value -> Int
integer key=fromMaybe 0 . field key
flag :: Text -> Value -> Bool
flag key=fromMaybe False . field key
items :: Text -> Value -> [Value]
items key=fromMaybe [] . field key
at :: [a] -> Int -> Maybe a
at values index | index<0=Nothing | otherwise=listToMaybe (drop index values)
tshow :: Show a => a -> Text
tshow=T.pack.show
merge :: Value -> Value -> Value
merge (Object old) (Object new)=Object (KM.union new old)
merge old _=old

completionStatus :: State -> Text
completionStatus s=case programExitCode s of
  Just code -> "Debug session ended (exit "<>tshow code<>")."
  Nothing -> "Debug session ended; exit status unavailable."

-- DAP exception details remain useful as selectable text, including nested causes.
exceptionText :: Value -> Text
exceptionText body=T.unlines (filter (not . T.null)
  ([text "exceptionId" body,text "description" body,"Break mode: "<>text "breakMode" body] ++
   maybe [] (details "") (field "details" body)))
  where
    details indent value=map (indent<>) (filter (not . T.null)
      [fromMaybe (text "typeName" value) (field "fullTypeName" value),text "message" value,text "stackTrace" value]) ++
      concatMap (details (indent<>"  ")) (items "innerException" value)

-- Download lifetime is independent of a DAP connection; each continuation keeps
-- the exact launch it was accepted for, never the current selection by accident.
type HdbContext=(Maybe FilePath,Maybe FilePath,Maybe FilePath,Toolchain,[(Int,Maybe FilePath,Int,StableName Buffer,Bool)])
data GhcLaunch=GhcLaunch FilePath Build.BuildConfig FilePath Int HdbContext
  | PackageLaunch !PackageBuildTarget !(Either Text FilePath) !Int !PackageDebugContext
      ![DirtySnapshot] !(Maybe Build.BuildConfig) !(IORef (Maybe FilePath))
-- Source identity, path and current toolchain only. No source/draft/history Eq.
type PackageDebugContext=(Maybe FilePath,Maybe FilePath,Toolchain,[(Int,Maybe FilePath,ContentVersion)])
data PackagePrepared=PackageGhc !FilePath ![(String,String)] !Value
  | PackageThc !FilePath ![String]
data HdbPrepared=HdbReady FilePath [(String,String)] | HdbOffer Hdb.HdbPlan
  | PackageReady !GhcLaunch !PackagePrepared | PackageOffer !GhcLaunch !Hdb.HdbPlan
data HdbRuntime=HdbRuntime Downloads.Downloads (IORef HdbState)
  (Compilers.Compiler -> IO (Either Text Hdb.HdbPlan))
  (Hdb.HdbPlan -> (Downloads.DownloadProgress -> IO ()) -> IO (Either Text FilePath)) C.Consoles (IORef [Async ()]) DownloadsWindow.Owner
data HdbState=HdbState
  { hdbSerial :: Int, hdbWanted :: Maybe (Int,GhcLaunch,Bool)
  , hdbPreparing :: Maybe (Int,GhcLaunch,TMVar (),Async (Either () (Either Text HdbPrepared)))
  , hdbOffer :: Maybe (Int,GhcLaunch,Hdb.HdbPlan)
  , hdbWaiting :: Maybe (Int,GhcLaunch,Int)
  , hdbReady :: Maybe (Int,GhcLaunch,PackagePrepared) }

hdbContext :: Desktop -> IO HdbContext
hdbContext d=do
  identities<-mapM identity [(bid,doc) | (bid,doc)<-M.toAscList (buffers d),documentLabel doc==Nothing]
  pure (Build.buildSource d,defaultDirectory d,treeRoot <$> sideTree d,fromMaybe GHC (toolchain d),identities)
  where identity (bid,doc)=do
          buffer<-evaluate (documentBuffer doc)
          stable<-makeStableName $! buffer
          pure (bid,filePath <$> documentFile doc,revision buffer,stable,dirty buffer)
hdbCurrent :: GhcLaunch -> HdbContext -> Bool
hdbCurrent PackageLaunch{} _=False
hdbCurrent (GhcLaunch _ _ _ _ context) current@(_,_,_,_,identities)=context==current &&
  not (any (\(_,_,_,_,modified)->modified) identities)
packageDebugContext :: Desktop -> IO (PackageDebugContext,[DirtySnapshot])
packageDebugContext d=do
  entries<-mapM capture [(bid,doc) | (bid,doc)<-M.toAscList (buffers d),documentLabel doc==Nothing]
  let directory=defaultDirectory d; tree=treeRoot <$> sideTree d
  mapM_ (evaluate . length) directory
  mapM_ (evaluate . length) tree
  pure ((directory,tree,fromMaybe GHC (toolchain d),map fst entries),map snd entries)
  where capture (bid,doc)=do
          version<-captureVersion (documentBuffer doc)
          snapshot<-evaluate (captureDirty (documentBuffer doc))
          path<-evaluate (filePath <$> documentFile doc)
          mapM_ (evaluate . length) path
          pure ((bid,path,version),snapshot)

launchCurrent :: GhcLaunch -> Desktop -> IO Bool
launchCurrent request@GhcLaunch{} d=hdbCurrent request <$> hdbContext d
launchCurrent (PackageLaunch _ _ _ (directory,tree,compiler,entries) _ _ _) d
  | directory/=defaultDirectory d || tree/=(treeRoot <$> sideTree d) || compiler/=fromMaybe GHC (toolchain d)=pure False
  | length entries/=length [() | doc<-M.elems (buffers d),documentLabel doc==Nothing]=pure False
  | otherwise=and <$> mapM current entries
  where current (bid,path,version)=case M.lookup bid (buffers d) of
          Just doc | documentLabel doc==Nothing,path==(filePath <$> documentFile doc)->versionCurrent version (documentBuffer doc)
          _->pure False

queuePackageDebug :: Debugger -> PackageBuildTarget -> Either Text FilePath -> Desktop -> IO Desktop
queuePackageDebug runtime target entry d=do
  (context,snapshots)<-packageDebugContext d
  owned<-newIORef Nothing
  queueHdb runtime (PackageLaunch target entry 4711 context snapshots Nothing owned) d

-- One request owns its temporary cradle until adopted into the DAP session.
-- Invalidation joins the preparation on the existing off-owner retired pool
-- before removing the file; a canceled worker cannot create after cleanup.
retirePackageLaunch :: Debugger -> Maybe (Async a) -> GhcLaunch -> IO ()
retirePackageLaunch (Debugger _ _ (HdbRuntime _ _ _ _ _ retired _) _ _) worker request=case request of
  PackageLaunch _ _ _ _ _ _ owned->mask_ $ do
    task<-async $ do
      mapM_ waitCatch worker
      file<-atomicModifyIORef' owned (\value->(Nothing,value))
      mapM_ (void . tryIOError . removeFile) file
    modifyIORef' retired (task:)
  _->pure ()

hdbPending :: Debugger -> IO Bool
hdbPending (Debugger _ _ (HdbRuntime _ ref _ _ _ _ _) _ _)=do
  h<-readIORef ref
  pure (any (==hdbSerial h) ([ident | (ident,_,_)<-maybeToList (hdbWanted h)]++
    [ident | (ident,_,_,_)<-maybeToList (hdbPreparing h)]++[ident | (ident,_,_)<-maybeToList (hdbOffer h)]++
    [ident | (ident,_,_)<-maybeToList (hdbWaiting h)]++[ident | (ident,_,_)<-maybeToList (hdbReady h)]))

invalidateHdb :: Debugger -> IO ()
invalidateHdb runtime@(Debugger _ _ (HdbRuntime _ ref _ _ _ _ _) _ _)=do
  h<-readIORef ref
  forM_ (hdbPreparing h) (\(_,_,stop,_)->atomically (void (tryPutTMVar stop ())))
  forM_ (hdbPreparing h) (\(_,request,_,task)->retirePackageLaunch runtime (Just task) request)
  forM_ ([request | (_,request,_)<-maybeToList (hdbWanted h)]++
         [request | (_,request,_)<-maybeToList (hdbOffer h)]++
         [request | (_,request,_)<-maybeToList (hdbWaiting h)]++
         [request | (_,request,_)<-maybeToList (hdbReady h)]) (retirePackageLaunch runtime Nothing)
  writeIORef ref h {hdbSerial=hdbSerial h+1,hdbWanted=Nothing,hdbOffer=Nothing,hdbReady=Nothing}

queueHdb :: Debugger -> GhcLaunch -> Desktop -> IO Desktop
queueHdb runtime@(Debugger _ _ (HdbRuntime _ ref _ _ _ _ _) _ _) request d=do
  invalidateHdb runtime
  h<-readIORef ref
  writeIORef ref h {hdbWanted=Just (hdbSerial h,request,True)}
  startHdbPreparation runtime
  pure d {status="Resolving the selected GHC and its debugger..."}

startHdbPreparation :: Debugger -> IO ()
startHdbPreparation (Debugger _ _ (HdbRuntime _ ref prepare _ _ _ _) _ _)=mask $ \restore->do
  h<-readIORef ref
  case (hdbPreparing h,hdbWanted h) of
    (Nothing,Just (ident,request,allowOffer))->do
      stop<-newEmptyTMVarIO
      task<-async $ restore $ race (atomically (readTMVar stop)) $ do
        result<-try $ do
          case request of
            PackageLaunch{}->preparePackageDebug prepare allowOffer request
            GhcLaunch directory config _ _ _->do
              settings<-getXdgDirectory XdgConfig "thc-edit"
              current<-Build.loadBuildConfig settings directory
              if current/=config then pure (Left "The build configuration changed; launch again.") else do
                project<-Build.isProject directory
                resolved<-Compilers.debuggerCompilerInfo directory project (Build.buildExecutable config)
                case resolved of
                  Left err->pure (Left err)
                  Right (_,Just (executable,environment))->pure (Right (HdbReady executable environment))
                  Right (compiler,Nothing) | allowOffer->fmap HdbOffer <$> prepare compiler
                                           | otherwise->pure (Left "The installed debugger is unavailable; launch again.")
        pure $ either (\(err::IOException)->Left (T.pack (show err))) id result
      writeIORef ref h {hdbWanted=Nothing,hdbPreparing=Just (ident,request,stop,task)}
    _->pure ()

-- Package-only preparation. The generic source/Adapter routes retain their
-- original authority and launch contract; these receipts are Human sidebar work.
preparePackageDebug :: (Compilers.Compiler -> IO (Either Text Hdb.HdbPlan)) -> Bool -> GhcLaunch -> IO (Either Text HdbPrepared)
preparePackageDebug prepare allowOffer (PackageLaunch target entry port context snapshots expected owned)=do
  settings<-getXdgDirectory XdgConfig "thc-edit"
  saved<-Build.loadBuildConfig settings (packageBuildRoot target)
  before<-packageBuildManifestCurrent target
  unsaved<-evaluate (any snapshotDirty snapshots)
  let config=saved {Build.buildTarget=packageBuildName target}
      captured=PackageLaunch target entry port context snapshots (Just saved) owned
  result<-if not before then pure (Left "Package debug target changed; refresh the tree.")
    else if maybe False (/=saved) expected then pure (Left "The build configuration changed; launch again.")
    else if unsaved then pure (Left "Save modified source files before debugging.")
    else case Build.buildToolchain config of
      THC->do
        planned<-Build.buildPlan Run config (packageBuildRoot target) Nothing
        pure $ case planned of
          Right [(executable,args)]->let (driver,guest)=break (=="--") args
            in Right (PackageReady captured (PackageThc executable (driver++["--dap-port",show port]++guest)))
          Left err->Left err
          _->Left "The selected THC target did not produce a debugger command."
      GHC->case entry of
        Left err->pure (Left err)
        Right file->do
          exists<-doesFileExist file
          governing<-governingCradle file
          if not exists then pure (Left "The captured main-is no longer exists; refresh the tree.")
            else if governing then pure (Left "This main-is has a custom hie.yaml or .hie-bios; use an explicit Adapter configuration.")
            else do
              resolved<-Compilers.debuggerCompilerInfo (packageBuildRoot target) True (Build.buildExecutable config)
              case resolved of
                Left err->pure (Left err)
                Right (_,Just (executable,environment))->do
                  cradle<-mask_ $ do
                    (path,handle)<-openTempFile (packageBuildRoot target) ".hide-debug-cradle.json"
                    writeIORef owned (Just path)
                    (BL.hPut handle (encode (object ["cradle" .= object ["cabal" .= object
                      ["component" .= packageBuildName target]]])) >> hClose handle)
                      `finally` void (tryIOError (hClose handle))
                    pure path
                  let arguments=object ["projectRoot" .= packageBuildRoot target,"entryFile" .= makeRelative (packageBuildRoot target) file,
                        "entryPoint" .= ("main"::Text),"entryArgs" .= Build.buildArguments config,
                        "extraGhcArgs" .= ([]::[String]),"cradleFile" .= cradle]
                  _<-evaluate (BL.length (encode arguments))
                  pure (Right (PackageReady captured (PackageGhc executable environment arguments)))
                Right (compiler,Nothing) | allowOffer->fmap (PackageOffer captured) <$> prepare compiler
                                         | otherwise->pure (Left "The installed debugger is unavailable; launch again.")
  -- Force only fixed launch metadata on this worker; final owner work is scalar.
  _<-evaluate (length (Build.buildExecutable config)+T.length (Build.buildTarget config)+
    T.length (Build.buildTHCRoot config)+T.length (Build.buildRuntime config)+sum (map length (Build.buildArguments config)))
  _<-case result of
    Left err->evaluate (T.length err) >> pure ()
    Right (PackageReady _ (PackageThc executable args))->evaluate (length executable+sum (map length args)) >> pure ()
    Right (PackageReady _ (PackageGhc executable environment _))->
      evaluate (length executable+sum [length key+length value | (key,value)<-environment]) >> pure ()
    _->pure ()
  after<-packageBuildManifestCurrent target
  current<-Build.loadBuildConfig settings (packageBuildRoot target)
  pure $ if not after || current/=saved then Left "Package or build configuration changed during debugger preparation." else result
preparePackageDebug _ _ _=pure (Left "Invalid package debugger request.")

-- The actual entry's ancestor chain governs hie-bios discovery. An unrelated
-- package-root file is not used as a proxy for the entry's cradle ownership.
governingCradle :: FilePath -> IO Bool
governingCradle file=go (takeDirectory file)
  where go directory=do
          yaml<-doesFileExist (directory </> "hie.yaml")
          bios<-doesFileExist (directory </> ".hie-bios")
          if yaml || bios then pure True else
            let parent=takeDirectory directory in if parent==directory then pure False else go parent

-- | Adopt a prepared Human package launch through the full host effect chain.
-- A modal defers it. Provider/Git refusal consumes the request once and retires
-- its cradle; ticks never invent an origin or retry a refused side effect.
tickPreparedDebug :: Debugger -> Core -> Desktop -> IO Desktop
tickPreparedDebug runtime@(Debugger _ _ (HdbRuntime _ ref _ _ _ _ _) _ _) core d=do
  h<-readIORef ref
  case hdbReady h of
    Just (ident,request@(PackageLaunch target _ _ _ _ _ _),_) | ident==hdbSerial h->do
      current<-launchCurrent request d
      if not current then invalidateHdb runtime >> pure d {status="Debug preparation cancelled: source or settings changed."}
        else if dialog d/=Nothing || questionActive d || activeAutocomplete d then pure d
        else do
          (_,next)<-core d [AdoptPreparedDebug target]
          -- Successful adoption cleared the slot. A rejected effect still owns
          -- the ready receipt here and must not be replayed on the next tick.
          remaining<-readIORef ref
          when (isJust (hdbReady remaining)) (invalidateHdb runtime)
          pure next
    _->pure d

adoptPackageDebug :: Debugger -> PackageBuildTarget -> Desktop -> IO Desktop
adoptPackageDebug runtime@(Debugger stateRef _ (HdbRuntime _ ref _ _ _ _ _) _ _) target d=mask_ $ do
  h<-readIORef ref
  case hdbReady h of
    Just (ident,request@(PackageLaunch captured _ port _ _ _ owned),launch)
      | ident==hdbSerial h,captured==target->do
          current<-launchCurrent request d
          if not current then invalidateHdb runtime >> pure d {status="Package debug source changed; launch again."}
            else do
              -- Keep request ownership until the handoff is protected, and
              -- track a new connection before initialization can fail.
              cradle<-readIORef owned
              created<-newIORef Nothing
              result<-try $ do
                writeIORef ref h {hdbReady=Nothing}
                atomicModifyIORef' owned (\_->(Nothing,()))
                readIORef stateRef >>= stopTransport runtime
                let directory=packageBuildRoot target
                next<-case launch of
                  PackageThc executable args->do
                    connection<-D.startManagedWithAfter (awaitRetired runtime) (pure (executable,args,[])) directory "127.0.0.1" port
                    writeIORef created (Just connection)
                    initializeSession runtime directory connection ("127.0.0.1",port) "attach" (object []) "graalvm" True d
                  PackageGhc executable environment arguments->do
                    connection<-D.startManagedWithAfter (awaitRetired runtime) (pure (executable,["server","--port",show port],environment)) directory "127.0.0.1" port
                    writeIORef created (Just connection)
                    next<-initializeSession runtime directory connection ("127.0.0.1",port) "launch" arguments "hdb" True d
                    modifyIORef' stateRef (\state->state {debugEnvironment=environment,hdbLauncher=Just executable})
                    pure next
                modifyIORef' stateRef (\state->state {debugCradle=cradle})
                pure next
              case result of
                Right next->pure next
                Left (err::SomeException)->do
                  connection<-readIORef created
                  state<-readIORef stateRef
                  stopTransport runtime state {client=connection,debugCradle=cradle}
                  case fromException err of
                    Just (io::IOException)->pure d {status="Debugger: "<>T.pack (show io)}
                    Nothing->throwIO err
    _->pure d {status="This prepared package Debug action expired."}

startPreparedGhc :: Debugger -> GhcLaunch -> FilePath -> [(String,String)] -> Desktop -> IO Desktop
startPreparedGhc runtime@(Debugger ref _ _ _ _) (GhcLaunch directory config file port _) executable environment d=do
  readIORef ref >>= stopTransport runtime
  connection<-D.startManagedWithAfter (awaitRetired runtime) (pure (executable,["server","--port",show port],environment)) directory "127.0.0.1" port
  let arguments=object ["projectRoot" .= directory,"entryFile" .= makeRelative directory file,
        "entryPoint" .= ("main"::Text),"entryArgs" .= Build.buildArguments config,"extraGhcArgs" .= ([]::[String])]
  started<-initializeSession runtime directory connection ("127.0.0.1",port) "launch" arguments "hdb" True d
  modifyIORef' ref (\state -> state {debugEnvironment=environment,hdbLauncher=Just executable})
  pure started {status="Starting the selected GHC debugger..."}
startPreparedGhc _ PackageLaunch{} _ _ d=pure d {status="Package debugger requires its captured launch gate."}

-- doc-artifact: tools/docs-screenshots.hs hdb-download -> docs/site/screenshots/hdb-download.png
hdbOfferDialog :: Int -> Hdb.HdbPlan -> Dialog
hdbOfferDialog ident plan=Dialog "Download Haskell debugger?" (DebugDialog ("hdb-accept:"<>tshow ident))
  [ReadOnly "GHC" (Compilers.compilerVersion (Hdb.hdbCompiler plan)),
   ReadOnly "Download" (tshow (Hdb.hdbAssetSize asset)<>" bytes; pinned SHA-256"),
   area "Compiler" (T.pack (Compilers.compilerPath (Hdb.hdbCompiler plan))),
   area "Source" (Hdb.hdbAssetURL asset),area "Install under" (T.pack (Hdb.hdbInstallRoot plan))]
  0 ["Download and launch","Not now"] ["Download the matching debugger to continue?"]
  where
    asset=Hdb.hdbAsset plan
    area name value=TextArea name False (newBuffer (T.intercalate "\n" (T.chunksOf 36 value))) (Selection 0 0) 0 0

acceptHdb :: Debugger -> Text -> [Text] -> Desktop -> IO Desktop
acceptHdb runtime@(Debugger _ _ (HdbRuntime downloads ref _ acquire _ _ _) _ _) action values d=do
  h<-readIORef ref
  case hdbOffer h of
    Just (ident,request,plan) | action=="hdb-accept:"<>tshow ident,ident==hdbSerial h->do
      writeIORef ref h {hdbOffer=Nothing}
      current<-launchCurrent request d
      if take 1 values/=["0"] || not current
        then invalidateHdb runtime >> pure d {status="Debugger download declined; no files downloaded."}
        else do
          result<-Downloads.startDownload downloads ("hdb for GHC "<>Compilers.compilerVersion (Hdb.hdbCompiler plan)) (acquire plan)
          case result of
            Left err->pure d {status=err}
            Right job->do
              modifyIORef' ref (\state->state {hdbWaiting=Just (ident,request,job)})
              showDownloads runtime d {status="Downloading debugger; launch will continue when ready."}
    _->pure d {status="This debugger download offer expired."}

cancelDownloadRequest :: Debugger -> DownloadCancelRequest -> Desktop -> IO Desktop
cancelDownloadRequest runtime@(Debugger _ _ (HdbRuntime downloads ref _ _ _ _ view) _ _) (DownloadCancelRequest target ident) d=do
  owned<-DownloadsWindow.cancelTarget view target d
  if not owned then pure d {status="This Downloads action expired."} else do
    cancelled<-Downloads.cancelDownload downloads ident
    h<-readIORef ref
    when (cancelled && maybe False (\(serial,_,job)->serial==hdbSerial h && job==ident) (hdbWaiting h)) (invalidateHdb runtime)
    pure d {status=if cancelled then "Cancelling download..." else "This download has already finished."}

showDownloads :: Debugger -> Desktop -> IO Desktop
showDownloads (Debugger _ _ (HdbRuntime _ _ _ _ _ _ view) _ _) = DownloadsWindow.open view

-- Every poll is nonblocking. Cancellation cleanup and GHC/Cabal queries stay on
-- the one preparation worker; a replacement waits for that worker to retire.
tickHdb :: Debugger -> Desktop -> IO Desktop
tickHdb runtime@(Debugger _ _ (HdbRuntime downloads ref _ _ _ _ view) _ _) original=do
  h<-readIORef ref
  let requests=[request | (ident,request,_)<-maybeToList (hdbWanted h),ident==hdbSerial h]++
        [request | (ident,request,_)<-maybeToList (hdbOffer h),ident==hdbSerial h]++
        [request | (ident,request,_)<-maybeToList (hdbWaiting h),ident==hdbSerial h]++
        [request | (ident,request,_,_)<-maybeToList (hdbPreparing h),ident==hdbSerial h]++
        [request | (ident,request,_)<-maybeToList (hdbReady h),ident==hdbSerial h]
  validity<-mapM (`launchCurrent` original) requests
  -- Constructors carry no Eq payloads; validate each continuation when needed.
  when (any not validity) (invalidateHdb runtime)
  current<-readIORef ref
  let finished ident=do
        modifyIORef' ref (\state->state {hdbWaiting=Nothing})
        when (ident==hdbSerial current) (invalidateHdb runtime)
  waited<-case hdbWaiting current of
    Just (ident,request,job)->do
      matches<-launchCurrent request original
      jobState<-Downloads.downloadStateFor downloads job
      case jobState of
        Just (Downloads.DownloadComplete _)->do
          modifyIORef' ref (\state->state {hdbWaiting=Nothing,hdbWanted=if ident==hdbSerial state && matches then Just (ident,request,False) else hdbWanted state})
          pure original {status=if ident==hdbSerial current && matches then "Debugger installed; validating the original launch..." else "Debugger installed; the original launch is no longer current."}
        Just (Downloads.DownloadFailed err)->finished ident >> pure original {status="Debugger download failed: "<>err}
        Just Downloads.DownloadCancelled->finished ident >> pure original {status="Debugger download cancelled."}
        _->pure original
    Nothing->pure original
  startHdbPreparation runtime
  state<-readIORef ref
  prepared<-case hdbPreparing state of
    Nothing->pure waited
    Just (ident,request,_,task)->do
      outcome<-poll task
      case outcome of
        Nothing->pure waited
        Just result->do
          matches<-launchCurrent request waited
          modifyIORef' ref (\latest->latest {hdbPreparing=Nothing})
          if ident/=hdbSerial state || not matches then retirePackageLaunch runtime Nothing request >> pure waited else case result of
            Right (Right (Right (HdbReady executable environment)))->startPreparedGhc runtime request executable environment waited
            Right (Right (Right (PackageReady captured launch)))->do
              modifyIORef' ref (\latest->latest {hdbReady=Just (ident,captured,launch)})
              pure waited {status="Component debugger prepared; waiting for launch admission."}
            Right (Right (Right (PackageOffer captured plan)))->do
              modifyIORef' ref (\latest->latest {hdbOffer=Just (ident,captured,plan)})
              pure waited {status="A matching debugger is available to download."}
            Right (Right (Right (HdbOffer plan)))->do
              modifyIORef' ref (\latest->latest {hdbOffer=Just (ident,request,plan)})
              pure waited {status="A matching debugger is available to download."}
            Right (Right (Left err))->retirePackageLaunch runtime Nothing request >> pure waited {status="Debugger: "<>err}
            Right (Left ())->retirePackageLaunch runtime Nothing request >> pure waited
            Left _->retirePackageLaunch runtime Nothing request >> pure waited {status="Debugger preparation failed."}
  latest<-readIORef ref
  shown<-DownloadsWindow.tick view prepared
  case dialog shown of
    Nothing | Just (ident,_,plan)<-hdbOffer latest->pure shown {dialog=Just (hdbOfferDialog ident plan)}
    _->pure shown
