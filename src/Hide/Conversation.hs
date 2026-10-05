{-# LANGUAGE CPP, OverloadedStrings #-}
-- | Primary ACP conversation ownership and child-agent transcript projection.
--
-- The session tick consumes protocol and hub mailbox events. Owned workers prepare
-- prompt context, file captures and consoles before adoption; capture itself does
-- not authorize an action. Prepared results must still match source identity and
-- privacy policy. Child cancellation/configuration/steering completes through ticks.
--
-- Conversation state also owns shared build and console services. Tool initiation
-- returns a desktop plus a continuation, so human questions can wait outside the
-- desktop lock while the rest of the session continues.
module Hide.Conversation (ConversationState, conversationServices, conversationAgents, withConversationAt, chatTools, chatToolNames, chatTool, QuestionCaller, captureQuestionCaller, chatToolAs, withConversation, conversationEffects, tickConversation, tickBuildPreparation, buildTerminalLaunchPending, stopConversationBuild, withBuildAdmission, parseLaunch, renderReply, pauseLabel, renderTimestamp) where

import Hide.Sidebar
import Prelude hiding (reads)
import Control.Exception (IOException, bracket, try, onException, mask, mask_, evaluate)
#ifdef WITH_WINDOW
import Control.Concurrent (forkIO)
import System.Process (createProcess, proc, waitForProcess)
import System.Environment (getExecutablePath)
#endif
import Hide.Session (SessionRecord(..))
import Control.Concurrent.Async (Async, async, asyncWithUnmask, cancel, poll, wait, waitCatch)
import Control.Concurrent.MVar (MVar, tryPutMVar, isEmptyMVar)
import Control.Monad (foldM, filterM, forM, forM_, void, when, unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe, parseEither)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Time (UTCTime, TimeZone, getCurrentTime, getCurrentTimeZone, diffUTCTime, utcToLocalTime, formatTime, defaultTimeLocale)
import Data.IORef
import Data.List (find, findIndex, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (XdgDirectory(..), getXdgDirectory, createDirectoryIfMissing, canonicalizePath, getCurrentDirectory, renameFile, removeFile)
import System.FilePath ((</>), takeDirectory, isAbsolute, makeRelative, splitDirectories)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Hide.Terminal as Terminal
import qualified Hide.Consoles as C
import qualified Hide.Compilers as Compilers
import qualified Hide.Build as B
import qualified Hide.BuildJobs as Jobs
import System.Info (os)
import System.Mem.StableName (StableName, makeStableName)
import System.IO (openBinaryTempFile, hClose)
import Text.Read (readMaybe)
import qualified Hide.ACP as A
import Hide.GuestAccess (sensitiveLabel, protectedPath, protectedBuffer)
import Hide.Files (filePath)
import Hide.MCPPermissions (AdmittedBuild, reserveAdmittedBuild, stepAdmittedBuild, cancelAdmittedBuild, permissionConfigPath, projectConfigPath, readAgentContextAt, writeAgentContextAt, readAgentContexts)
import qualified Hide.AgentRuntime as AR
import qualified Hide.AgentHub as AH
import qualified Hide.AgentACP as AP
import Hide.Session (checkpointPath)
import Hide.AgentFiles
import Hide.Buffer
import Hide.Plugin.BufferHost (ContentVersion,captureVersion,versionCurrent)
import Hide.Markdown (renderMarkdownWithShellBlocks)
import Hide.AgentSidebarTypes
import Hide.Model hiding (prompt)
import System.Environment (lookupEnv)
import Hide.Syntax (linkSpans,Style(..), bubbleTile)

-- One configured stdio provider; its protocol supplies models and tools.
data Phase = Initializing (Maybe Text) | Starting (Maybe Text) | Prompting | Steering Text (Maybe DraftReceipt) | Setting deriving Eq
-- A human submission can consume only its captured immutable draft. Stable
-- names in ContentVersion retain no Buffer/Undo; selection/focus are independent.
data DraftReceipt = DraftReceipt !Text !ContentVersion deriving Eq

data Record = Reply Text Text | Activity Text Value [Value] Bool | Pause Text deriving (Eq,Show)

activity :: Text -> Value -> Record
activity ident value=Activity ident value [value] False
data Approval = ChildPermission AH.AgentId AP.ACPPermission (MVar (Maybe Text)) | Permission Value [(Text,Text)] Value | Write Value Snapshot Text | Execute Value Terminal.TerminalConfig Int

data FileRequest = ReadFile Int (Maybe Int) | WriteFile Text
data FileCapture = FileCapture Value (Async (Either Text CapturedFile))
data CapturedFile = CapturedFile Snapshot (Maybe SourceIdentity) CapturedAction
data CapturedAction = CapturedRead A.PreparedResponse | CapturedWrite Text

-- Fixed answer delivery reuses the ordinary query queue. Its receipt identifies
-- the exact provider incarnation, not a reusable session label alone.
data ProviderReceipt = ProviderReceipt !(StableName A.Client) !Text
-- Host-minted before any permission wait; extension arguments cannot forge it.
data QuestionCaller = QuestionCaller !(StableName (IORef State)) !AH.AgentId !(Maybe ProviderReceipt)
data QueuedQuery = SubmittedQuery !Text | QuestionQuery !Int !AH.AgentId !ProviderReceipt !Text
data QuestionTicket = QuestionTicket !Int !AH.AgentId !(Maybe ProviderReceipt)
data QuestionResult = QuestionResult !AH.AgentId !(Maybe ProviderReceipt) !Value

-- One caller-bound build intent, captured without retaining editable buffers or Undo.
data BuildReceipt = BuildReceipt !Int !FilePath !(Maybe FilePath) ![(Int,Maybe FilePath,ContentVersion)] !(Maybe AdmittedBuild)
data PreparedBuild = BuildOptions !Dialog
  | BuildCommands !B.BuildAction !FilePath ![(FilePath,[String])]
  | BuildConsole !C.PreparedConsole
  | BuildUnsaved !B.BuildAction
data BuildPreparation = BuildPreparing !BuildReceipt !Bool !(Async (Either Text PreparedBuild))
  | BuildReady !BuildReceipt !Bool !(Either Text PreparedBuild)
  | BuildRetiring !Bool !(Async ())

data State = State
  { provider :: A.Launch, connection :: Maybe A.Client, session :: Maybe Text, project :: FilePath
  , pending :: M.Map Int Phase, queuedPrompt :: Maybe (Text,Maybe DraftReceipt), transcript :: [Record]
  , reads :: M.Map FilePath Snapshot, approvals :: [(Int,Approval)], presented :: Maybe Int, deferredApproval :: Bool, nextApproval :: Int
  , queuedQueries :: [QueuedQuery]
  , ownedTerminals :: S.Set Text
  , terminalWaiters :: M.Map Text [Value]
  , lastMessageAt :: Maybe UTCTime
  , lastRender :: Maybe (Int,Maybe Text,StableName [Record]), lastSession :: Maybe (A.Launch,FilePath,Text)
  , waitingQuestion :: Maybe QuestionTicket, questionResults :: M.Map Int QuestionResult, questionsClosed :: Bool, lastQuestion :: Maybe (StableName ChatQuestion)
  , deliveredContext :: Maybe Value
  , directoryAgents :: [AH.AgentId]
  , agentDelivery :: Maybe (AH.HubMessage,MVar (Either Text Value))
  , agentInitialized :: Value, agentConfig :: Value
  , streamTails :: M.Map Text Text
  , lastAgentSync :: Maybe (FilePath,Text,AH.Capabilities,Bool)
  , childRecords :: M.Map Text [Record], childRender :: Maybe (Text,Int,Value), childWidths :: M.Map Text Int
  , expandedToolRuns :: S.Set (Text,Text)
  , childControls :: M.Map Text (Maybe DraftReceipt,Async (Either Text ()))
  , childCancels :: M.Map Text (Async (Either Text ()))
  , compilerDiscovery :: Maybe (Async [Compilers.Compiler])
  , fileCaptures :: [FileCapture], retiringRequests :: [(Int,Async ())]
  , promptPreparation :: Maybe (Bool,Text,Maybe DraftReceipt,Async (Either Text ([Value],Value)))
  , buildSettingsCache :: Maybe Toolchain, buildSettingsVersion :: Int
  , buildSettingsWorker :: Maybe (Int,Async Toolchain), buildSettingsChecked :: Maybe UTCTime
  , buildAdmission :: Maybe AdmittedBuild
  , buildPreparation :: Maybe BuildPreparation
  , shellLaunches :: [Async (Either Text C.PreparedConsole)]
  , creatingAgent :: Maybe (Async (Either Text (AH.AgentId,Int)))
  , resumeRecordPath :: FilePath
  }
-- | Session-scoped provider, transcript and shared service ownership.
data ConversationState = ConversationState FilePath (IORef State) C.Consoles Jobs.BuildJobs AR.AgentRuntime

conversationAgents :: ConversationState -> AR.AgentRuntime
conversationAgents (ConversationState _ _ _ _ agents)=agents

defaultLaunch :: A.Launch
defaultLaunch = A.Launch "codex-acp" [] []

withConversation :: (ConversationState -> IO a) -> IO a
withConversation action = getCurrentDirectory >>= \root -> withConversationAt root action

-- | Load conversation configuration and scope provider, service and worker lifetimes.
withConversationAt :: FilePath -> (ConversationState -> IO a) -> IO a
withConversationAt root action = C.withConsoles $ \consoles -> Jobs.withBuildJobs $ \jobs -> do
  directory<-getXdgDirectory XdgConfig "thc-edit"
  loaded<-try (BS.readFile (directory </> "agents.json")) :: IO (Either IOException BS.ByteString)
  let launch=either (const defaultLaunch) (either (const defaultLaunch) id . decodeLaunch) loaded
  resumePath<-conversationSessionPath directory
  previous<-try (BS.readFile resumePath) :: IO (Either IOException BS.ByteString)
  let remembered=either (const Nothing) (\bytes -> decodeStrict' bytes >>= parseMaybe (withObject "session" $ \o -> do
        (raw::Value)<-o .: "provider"; config<-either fail pure (decodeLaunch (BL.toStrict (encode raw)))
        (,,) config <$> o .: "cwd" <*> o .: "sessionId")) previous
  ref<-newIORef State
    { provider=launch,connection=Nothing,session=Nothing,project=root
    , pending=M.empty,queuedPrompt=Nothing,transcript=[],reads=M.empty
    , approvals=[],presented=Nothing,deferredApproval=False,nextApproval=1,queuedQueries=[]
    , ownedTerminals=S.empty,terminalWaiters=M.empty,lastMessageAt=Nothing
    , lastRender=Nothing,lastSession=remembered,waitingQuestion=Nothing,questionResults=M.empty,questionsClosed=False,lastQuestion=Nothing
    , deliveredContext=Nothing,compilerDiscovery=Nothing,fileCaptures=[],retiringRequests=[],promptPreparation=Nothing,buildSettingsCache=Nothing,buildSettingsVersion=0,buildSettingsWorker=Nothing,buildSettingsChecked=Nothing,resumeRecordPath=resumePath,creatingAgent=Nothing,buildAdmission=Nothing,buildPreparation=Nothing,shellLaunches=[],directoryAgents=[],agentDelivery=Nothing
    , agentInitialized=Null,agentConfig=Null,streamTails=M.empty,lastAgentSync=Nothing,childRecords=M.empty,childRender=Nothing,childWidths=M.empty,childCancels=M.empty,childControls=M.empty,expandedToolRuns=S.empty }
  AR.withAgentRuntime root (provider <$> readIORef ref) $ \agents ->
    bracket (pure (ConversationState directory ref consoles jobs agents)) closeConversation action

-- Provider configuration remains global, but a recovered editor must resume
-- its own conversation. Standalone/legacy callers retain their existing file.
conversationSessionPath :: FilePath -> IO FilePath
conversationSessionPath directory=lookupEnv "THC_EDIT_SESSION" >>= maybe
  (pure (directory </> "agent-session.json"))
  (fmap (++".agent.json") . checkpointPath)

closeConversation :: ConversationState -> IO ()
closeConversation (ConversationState _ ref _ _ _) = do
  s<-readIORef ref
  writeIORef ref (abandonQuestion "Editor session closed." s) {questionsClosed=True}
  finishAgentDelivery ref (Left "Editor session closed.")
  mapM_ denyChild (map snd (approvals s))
  mapM_ closeBuildPreparation (buildPreparation s)
  mapM_ (cancel . snd) (buildSettingsWorker s)
  mapM_ (\(_,_,_,worker) -> cancel worker) (promptPreparation s)
  mapM_ cancel (creatingAgent s)
  mapM_ cancel (compilerDiscovery s)
  mapM_ (\(FileCapture _ worker) -> cancel worker) (fileCaptures s)
  mapM_ (wait . snd) (retiringRequests s)
  mapM_ cancel (childCancels s)
  mapM_ (cancel . snd) (childControls s)
  forM_ (shellLaunches s) $ \worker -> do
    cancel worker
    result<-waitCatch worker
    case result of Right (Right prepared)->C.closePreparedConsole prepared; _->pure ()
  mapM_ A.stopClient (connection s)

launchValue :: A.Launch -> Value
launchValue launch=object ["executable" .= A.executable launch,"arguments" .= A.arguments launch,"environment" .= M.fromList (A.environment launch)]

decodeLaunch :: BS.ByteString -> Either String A.Launch
decodeLaunch bytes=do
  value<-eitherDecodeStrict' bytes
  maybe (Left "Expected executable, arguments array and environment object.") Right (parseMaybe (withObject "agent" $ \o ->
    A.Launch <$> o .: "executable" <*> o .:? "arguments" .!= [] <*> (M.toList <$> (o .:? "environment" .!= M.empty))) value) >>= validateLaunch

validateLaunch :: A.Launch -> Either String A.Launch
validateLaunch launch
  | null (A.executable launch) = Left "Enter an executable."
  | any (elem '\0') (A.executable launch:A.arguments launch++concatMap (\(k,v)->[k,v]) (A.environment launch)) = Left "NUL bytes are not valid in process arguments."
  | any (\(key,_) -> null key || '=' `elem` key) (A.environment launch) = Left "Invalid environment variable name."
  | otherwise = Right launch

parseLaunch :: Text -> Text -> Text -> Either String A.Launch
parseLaunch command args env = do
  arguments<-eitherDecodeStrict' (TE.encodeUtf8 args)
  environment<-eitherDecodeStrict' (TE.encodeUtf8 env)
  validateLaunch (A.Launch (T.unpack (T.strip command)) arguments (M.toList environment))

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

-- One bounded discovery worker belongs to this conversation runtime. Reopening
-- while it runs shares the catalogue query; results never reopen a closed menu.
openCompilerMenu :: ConversationState -> Desktop -> IO Desktop
openCompilerMenu (ConversationState directory ref _ _ _) d
  | dialog d/=Nothing = pure d
  | otherwise = do
      mask $ \restore -> do
        current<-readIORef ref
        when (isNothing (compilerDiscovery current)) $ do
          worker<-async (restore Compilers.installedCompilers)
          modifyIORef' ref (\state -> state {compilerDiscovery=Just worker})
      saved<-readRunSettings directory
      pure (compilerMenu False saved [] d) {status="Finding installed GHC compilers..."}

pollCompilerMenu :: ConversationState -> Desktop -> IO Desktop
pollCompilerMenu (ConversationState directory ref _ _ _) d = do
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

-- | Consume conversation effects and delegate unrelated effects to the next interpreter.
conversationEffects :: ConversationState -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
conversationEffects runtime@(ConversationState _ ref _ _ _) fallback original effects = do
  (quit,updated)<-foldM apply (False,original) effects
  if quit then pure (True,updated) else (False,) <$> refreshConversationLayout runtime updated
  where
    apply state@(True,_) _=pure state
    apply (_,d) (AgentSidebarAction request) = (False,) <$> applyAgentSidebar runtime request d
    apply (_,d) (AgentAction "edit-context" ("0":scope:_)) = do
      state<-readIORef ref
      root<-if isNothing (connection state) then B.resolveBuildRoot d else pure (project state)
      path<-if scope=="0" then permissionConfigPath else projectConfigPath root
      -- The existing editor supplies multiline editing, save and undo.
      loaded<-readAgentContextAt path
      result<-case loaded of Left err->pure (Left err); Right text->writeAgentContextAt path text
      case result of
        Left err -> pure (False,message "Agent Context" [err] d)
        Right () -> do
          (quit,opened)<-fallback d {guestPrivatePaths=path:guestPrivatePaths d} [ReadPath path]
          pure (quit,opened {status="Edit [editor.agent] context; save to apply with the next query or steer."})
    apply (_,d) AdoptPreparedBuild = (False,) <$> adoptBuildPreparation runtime d
    apply (_,d) (AgentAction action values) = do
      updated<-perform runtime action values d
      syncConversationAgent runtime
      pure (False,updated)
    apply (_,d) effect = fallback d [effect]

-- Sidebar requests are fixed human operations, adopted after host hit/lifetime
-- validation. Dialog purposes carry the exact ID instead of a directory index.
applyAgentSidebar :: ConversationState -> AgentSidebarRequest -> Desktop -> IO Desktop
applyAgentSidebar runtime@(ConversationState _ ref _ _ agents) request d=case request of
  ShowAgent ident | ident==AR.primaryAgent agents->perform runtime "show" [] d
                  | otherwise->showAgentHistory runtime ident d
  ShowAgentConfiguration receipt option choices selected->do
    when (AH.agentConfigAgent receipt==AR.primaryAgent agents) (syncConversationAgent runtime)
    current<-AH.agentConfigurationCurrent hub receipt
    pure $ if not current then d {status="Agent choices expired."} else d {dialog=Just (Dialog "Agent setting"
      (AgentChoiceDialog receipt option choices) [ListBox "Provider choices" (map (T.take 256 . snd) choices) selected]
      0 ["Apply","Cancel"] []),contextMenu=Nothing,contextTarget=Nothing}
  ConfigureAgent receipt option value
    | AH.agentConfigAgent receipt==AR.primaryAgent agents->do
        syncConversationAgent runtime
        current<-AH.agentConfigurationCurrent hub receipt
        if current then performPrimary runtime "set-config" [option,value] d else pure d {status="Agent setting expired."}
    | otherwise->startChildControl runtime (AH.agentConfigAgent receipt) Nothing (AH.configureAgentAt hub receipt option value) d
  RenameAgentTo ident name->do
    result<-AH.renameAgent hub AH.Human ident name
    pure d {status=either id (const "Agent renamed.") result}
  NewAgent->pure d {dialog=Just (Dialog "New agent" AgentNewDialog
    [Input "Name" "" 0,Input "Task" "" 0] 0 ["Create","Cancel"]
    ["Start a fresh agent in this workspace and enqueue its task."]),contextMenu=Nothing,contextTarget=Nothing}
  CreateAgent name task->do
    state<-readIORef ref
    case creatingAgent state of
      Just _->pure d {status="An agent is already starting."}
      Nothing->mask $ \restore->do
        let spec=AH.SpawnSpec name task (startingDirectory d) AH.Shared AH.Fresh Nothing Nothing
        worker<-async (restore (AH.spawnAgentWithTask hub AH.Human spec))
        modifyIORef' ref (\current->current {creatingAgent=Just worker})
        pure d {status="Starting agent…"}
  _->pure d {status="Completion owner is unavailable."}
  where hub=AR.agentHub agents

pollAgentCreation :: ConversationState -> Desktop -> IO Desktop
pollAgentCreation (ConversationState _ ref _ _ _) d=do
  state<-readIORef ref
  case creatingAgent state of
    Nothing->pure d
    Just worker->poll worker >>= \completed->case completed of
      Nothing->pure d
      Just outcome->do
        modifyIORef' ref (\current->current {creatingAgent=Nothing})
        pure d {status=either (const "Agent creation interrupted.") (either id (const "Agent created; task queued.")) outcome}

perform :: ConversationState -> Text -> [Text] -> Desktop -> IO Desktop
perform (ConversationState _ ref _ _ _) "focus" [] d=do
  modifyIORef' ref (\state -> state {deferredApproval=False})
  pure d
perform (ConversationState _ ref _ _ _) "toggle-tool-run" [ident] d=do
  state<-readIORef ref
  let target=conversationTarget d
      key=(target,ident)
      expanded=expandedToolRuns state
      next=state {expandedToolRuns=if S.member key expanded then S.delete key expanded else S.insert key expanded}
      records=if T.null target then transcript next else M.findWithDefault [] target (childRecords next)
  writeIORef ref next
  keepConversationPosition d <$> paintView target False next {transcript=records} d
perform (ConversationState _ ref _ _ _) "execute-shell-block" [bidText,startText,endText,dialect,body] d
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

perform runtime action values d
  | action=="show" = performPrimary runtime action values (selectConversationView "" "Primary" d)
  | not (T.null (conversationTarget d)) && action `elem` ["send","send-draft","steer-draft","cancel","copy","toggle-activity","new","resume","load","set-config"] = performChild runtime action values d
  | otherwise = performPrimary runtime action values d

performPrimary :: ConversationState -> Text -> [Text] -> Desktop -> IO Desktop
performPrimary runtime@(ConversationState directory ref consoles jobs _) action values original = do
  d<-if action `elem` ["cancel","new","load","configure"] then cancelQuestion runtime "Question cancelled." original else pure original
  previous<-readIORef ref
  now<-getCurrentTime
  zone<-getCurrentTimeZone
  let s=if action `elem` ["send","send-draft","steer-draft"] then stampReply now zone previous else previous
  submitted<-if action/="send-draft" then pure False else case queuedPrompt s of
    Just (_,Just receipt)->draftCurrent receipt d
    _->pure False
  case (action,values) of
    ("directory",_) -> showAgentDirectory runtime d
    ("directory-select",button:index:_) | Just n<-readMaybe (T.unpack index), n>=0,
        ident:_<-drop n (directoryAgents s) -> case button of
      "0" | ident==AR.primaryAgent (conversationAgents runtime) -> perform runtime "show" [] d
          | otherwise -> showAgentHistory runtime ident d
      "1" -> do
        workspace<-AR.agentSession (conversationAgents runtime) ident
        currentSession<-lookupEnv "THC_EDIT_SESSION"
        case workspace of
          Nothing -> pure d {status="This agent uses the current editor workspace."}
          Just record | currentSession==Just (sessionId record) -> pure d {status="This agent uses the current editor workspace."}
          Just record -> do
            let command="hide --resume "<>T.pack (sessionId record)
                fallback= d {dialog=Just (Dialog "Agent workspace" (AgentDialog "workspace-command")
                  [Input "Session command" command (T.length command)] 0 ["Close"]
                  ["Run this command in a terminal on the editor host.","For SSH sessions, attach from your SSH client instead."])}
#ifdef WITH_WINDOW
            executable<-getExecutablePath
            launched<-try (createProcess (proc executable ["--window","--resume",sessionId record]))
            case launched of
              Left (_::IOException) -> pure fallback {status="Could not open a workspace window."}
              Right (_,_,_,process) -> do
                void (forkIO (void (waitForProcess process)))
                pure d {status="Opening workspace on the editor host; remote displays can attach through SSH."}
#else
            pure fallback
#endif
      "2" -> do
        result<-AR.requestAgentReconnect (conversationAgents runtime) ident
        pure d {status=either id (const "Reconnecting saved agent session...") result}
      "3" -> showAgentDirectory runtime d
      _ -> pure d
    ("toggle-activity",[index]) | Just ident<-readMaybe (T.unpack index) -> do
      let toggle (i,Activity title value history expanded) | i==ident=Activity title value history (not expanded)
          toggle (_,record)=record
          next=s {transcript=map toggle (zip [0::Int ..] (transcript s))}
      writeIORef ref next
      keepConversationPosition d <$> paint False next d
    ("question-choice",[token,index]) | Just ident<-readMaybe (T.unpack token),Just chosen<-readMaybe (T.unpack index),
        Just q<-chatQuestion d,questionToken q==ident,chosen>=0,chosen<length (questionChoices q) ->
      clearReplySelection <$> paint False s d {chatQuestion=Just q {questionChoice=Just chosen,questionFocused=True}}
    ("question-input",token:rest) | Just q<-chatQuestion d,token==T.pack (show (questionToken q)) ->
      let p=case rest of
            offset:_ | Just n<-readMaybe (T.unpack offset) -> min (bufferLength (questionBuffer q)) (questionInputStart (conversationWidth d) q+max 0 n)
            _ -> caret (questionSelection q)
      in clearReplySelection <$> paint False s d {chatQuestion=Just q {questionChoice=Nothing,questionSelection=Selection p p,questionFocused=True}}
    ("question-submit",[token]) | Just q<-chatQuestion d,token==T.pack (show (questionToken q)),
        Just (QuestionTicket ident actor receipt)<-waitingQuestion s,ident==questionToken q -> do
      let answer=case questionChoice q of
            Just index -> fromMaybe "" (case drop index (questionChoices q) of value:_->Just value; _->Nothing)
            Nothing -> contents (questionBuffer q)
      if T.null (T.strip answer) then pure d {status="Choose an option or enter an answer."}
      else if T.length answer>65536 then pure d {status="Answers may contain at most 65536 characters."} else do
        live<-case receipt of Nothing->pure True; Just target->providerCurrent target s
        active<-AH.statusAgent (AR.agentHub (conversationAgents runtime)) (AH.Agent actor) actor
        if not live || either (const True) (const False) active
          then cancelQuestion runtime "Question requester ended." d
          else do
            let value=object ["questionId" .= ident,"status" .= ("answered"::Text),"answer" .= answer,"choiceIndex" .= questionChoice q,"custom" .= isNothing (questionChoice q)]
                queued=case receipt of Nothing->queuedQueries s; Just target->queuedQueries s++[QuestionQuery ident actor target answer]
                next=(rememberQuestion ident actor receipt value s) {waitingQuestion=Nothing,queuedQueries=queued,
                  transcript=transcript s++[Reply "Agent" (questionText q),Reply "You" answer]}
            writeIORef ref next
            paint False next d {chatQuestion=Nothing,chatInputOffset=Nothing,status="Answer submitted.",agentQueued=length queued}
    ("question-cancel",[token]) | Just q<-chatQuestion d,token==T.pack (show (questionToken q)) ->
      cancelQuestion runtime "Question cancelled by user." d
    ("terminal",_) -> do
      shell<-if os=="mingw32" then fromMaybe "cmd.exe" <$> lookupEnv "COMSPEC" else fromMaybe "/bin/sh" <$> lookupEnv "SHELL"
      root<-canonicalizePath (maybe (startingDirectory d) treeRoot (sideTree d))
      openConsole consoles (Terminal.TerminalConfig shell [] [] root 80 24) d
    ("run",_) -> startBuildPreparation runtime (Just B.Run) d
    ("compile",_) -> startBuildPreparation runtime (Just B.Compile) d
    ("make",_) -> startBuildPreparation runtime (Just B.Make) d
    ("build-stop",_) -> stopConversationBuild runtime d
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
    ("context",_) -> pure d {dialog=Just (Dialog "Agent Context" (AgentDialog "edit-context")
      [Radio "Scope" ["Global","Project"] 1] 0 ["Edit","Cancel"]
      ["Edit [editor.agent] context in the selected TOML file.","Use triple quotes for multiple lines. Save before sending.","Project context follows global context; permissions do not change."])}
    ("options",_) -> pure d {dialog=Just (Dialog "Agents" (AgentDialog "configure")
      [input "Executable" (T.pack (A.executable (provider s))),input "Arguments (JSON array)" (jsonText (A.arguments (provider s))),
       input "Environment (JSON object)" (jsonText (M.fromList (A.environment (provider s))))] 0 ["OK","Cancel"]
      ["ACP stdio provider. Arguments are passed without a shell."])}
    ("configure",_:command:args:env:_) -> case parseLaunch command args env of
      Left err -> pure (message "Invalid agent configuration" (wrapMessage (T.pack err)) d)
      Right config -> do
        result<-persist (directory </> "agents.json") (launchValue config)
        case result of
          Left err -> pure (message "Cannot save configuration" (wrapMessage err) d)
          Right () -> do
            mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
            finishAgentDelivery ref (Left "Agent configuration changed.")
            mapM_ denyChild (map snd (approvals s))
            retired<-retireRequests ref
            mapM_ A.stopClient (connection s)
            writeIORef ref retired {provider=config,connection=Nothing,session=Nothing,agentDelivery=Nothing,streamTails=M.empty,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty}
            pure d {status="Agent configuration saved."}
    ("show",_) -> do
      modifyIORef' ref (\state -> state {deferredApproval=False})
      -- A recovered transcript belongs to the checkpoint until a provider
      -- connects. Opening its window must not repaint it from empty state.
      let recoveredWindow=do
            (bid,_)<-find ((==Just "Conversation") . documentLabel . snd) (M.toList (buffers d))
            find ((==Just bid) . bufferId) (windows d)
      case recoveredWindow of
        Just win | isNothing (connection s), null (transcript s), isNothing (chatQuestion d) ->
          pure (focusWindow (windowId win) d {composerFocused=True})
        _ -> paint True s d
    ("set-config",[ident,value])
      | busy s -> pure d {status="Wait for the current reply before changing its model."}
      | Just client<-connection s, Just sid<-session s,
        any (\option -> settingId option==ident && value `elem` map fst (settingChoices option)) (agentSettings d) -> do
          requestId<-A.request client "session/set_config_option" (object ["sessionId" .= sid,"configId" .= ident,"value" .= value])
          modifyIORef' ref (\state -> state {pending=M.insert requestId Setting (pending state)})
          pure d {status="Updating conversation settings...",agentReplying=True}
      | otherwise -> pure d {status="This conversation setting is unavailable."}
    ("copy",_) -> pure d {clipboard=rawTranscript (transcript s),clipboardCode=Nothing,status="Raw conversation copied."}
    ("send-draft",_) | steeringPending s -> pure d {status="Wait for the steering result before sending another message."}
    ("send-draft",_) | submitted -> pure d {status="Preparing the submitted draft..."}
    ("send-draft",_) | busy s, let text=contents (composerBuffer d), not (T.null (T.strip text)) -> do
      let next=s {queuedQueries=queuedQueries s++[SubmittedQuery text],transcript=transcript s++[Reply "You" (composerMarkdown text)]}
      writeIORef ref next
      painted<-paint True next d
      pure painted {composerBuffer=newBuffer "",composerSelection=Selection 0 0,composerFocused=True,agentQueued=length (queuedQueries next),status="Query queued."}
    ("send-draft",_) | not (T.null (T.strip (contents (composerBuffer d)))) -> do
      receipt<-captureDraft d
      next<-submitPrimaryPrompt runtime s (Just receipt) (contents (composerBuffer d)) (False,False,False) d
      latest<-readIORef ref
      pure next {agentReplying=busy latest,composerFocused=True}
    ("steer-draft",_) | not (agentSteering d) -> pure d {status="This provider does not advertise steering support."}
    ("steer-draft",_) | Prompting `notElem` M.elems (pending s) -> pure d {status="No active turn to steer; use Query to send the draft."}
    ("steer-draft",_) | steeringPending s -> pure d {status="A steering request is already pending."}
    ("steer-draft",_) | let text=contents (composerBuffer d), not (T.null (T.strip text)) -> do
      receipt<-captureDraft d
      beginPromptPreparation ref True text (Just receipt) d
    ("send",_:prompt:selectionFlag:fileFlag:diagnosticFlag:_) | not (T.null (T.strip prompt)),not (busy s) ->
      submitPrimaryPrompt runtime s Nothing prompt (selectionFlag=="true",fileFlag=="true",diagnosticFlag=="true") d
    ("cancel",_) -> do
      let child (_,ChildPermission{})=True
          child _=False
          retained=filter child (approvals s)
          keepDialog=maybe False (`elem` map fst retained) (presented s)
      mapM_ (C.killConsole consoles) (S.toList (ownedTerminals s))
      forM_ (connection s) $ \client -> do
        forM_ (session s) $ \sid -> A.notify client "session/cancel" (object ["sessionId" .= sid])
        mapM_ (cancelApproval client . snd) (filter (not . child) (approvals s))
      retired<-retireRequests ref
      writeIORef ref retired {queuedPrompt=Nothing,approvals=retained,presented=if keepDialog then presented s else Nothing,deferredApproval=False}
      pure (if keepDialog then d else dismissPermission d) {status="Cancellation requested."}
    ("new",_) | busy s -> pure d {status="Cancel the current reply before starting a new session."}
    ("new",_) -> do
      mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
      retired<-retireRequests ref
      mapM_ A.stopClient (connection s)
      writeIORef ref retired {connection=Nothing,session=Nothing,agentDelivery=Nothing,streamTails=M.empty,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],transcript=[],expandedToolRuns=S.filter ((/="").fst) (expandedToolRuns s),lastMessageAt=Nothing,reads=M.empty,approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty}
      start runtime Nothing d
    ("resume",_) | busy s -> pure d {status="Cancel the current reply before resuming a session."}
    ("resume",_) -> pure d {dialog=Just (Dialog "Resume conversation" (AgentDialog "load")
      [input "Session ID" (maybe "" (\(_,_,sid)->sid) (lastSession s))] 0 ["Resume","Cancel"]
      ["The provider must support loading or resuming sessions."])}
    ("load",_:sid:_) | not (T.null (T.strip sid)), not (busy s) -> do
      mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
      retired<-retireRequests ref
      mapM_ A.stopClient (connection s)
      writeIORef ref retired {connection=Nothing,session=Nothing,agentDelivery=Nothing,streamTails=M.empty,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],transcript=[],expandedToolRuns=S.filter ((/="").fst) (expandedToolRuns s),lastMessageAt=Nothing,reads=sourceSnapshots d,approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty}
      start runtime (Just (T.strip sid)) d
    _ | Just suffix<-T.stripPrefix "approval:" action, Just token<-readMaybe (T.unpack suffix) -> decide runtime token values d
    _ -> pure d
  where input label text=Input label text (T.length text)

-- Direct prompts and already-enqueued queries carry no draft consumption right.
submitPrimaryPrompt :: ConversationState -> State -> Maybe DraftReceipt -> Text -> (Bool,Bool,Bool) -> Desktop -> IO Desktop
submitPrimaryPrompt runtime@(ConversationState _ ref _ _ _) s receipt prompt (selectionFlag,fileFlag,diagnosticFlag) d=do
  let context=contextText selectionFlag fileFlag diagnosticFlag d
      full=prompt<>(if T.null context then "" else "\n\n"<>context)
      next=s {queuedPrompt=Just (full,receipt),reads=sourceSnapshots d,transcript=transcript s++[Reply "You" (composerMarkdown prompt)]}
  writeIORef ref next
  opened<-if isNothing (connection s) then start runtime Nothing d else sendQueued runtime d
  latest<-readIORef ref
  paint True latest opened

isSteering :: Phase -> Bool
isSteering Steering{}=True
isSteering _=False

steeringPending :: State -> Bool
steeringPending s=any isSteering (M.elems (pending s)) || maybe False (\(steering,_,_,_) -> steering) (promptPreparation s)

busy :: State -> Bool
busy s=not (M.null (pending s)) || queuedPrompt s/=Nothing || not (isNothing (promptPreparation s))

start :: ConversationState -> Maybe Text -> Desktop -> IO Desktop
start (ConversationState _ ref _ _ _) resume d = do
  s<-readIORef ref
  let (launch,directory)=case (resume,lastSession s) of
        (Just wanted,Just (savedProvider,savedDirectory,savedId)) | wanted==savedId -> (savedProvider,savedDirectory)
        _ -> (provider s,maybe (startingDirectory d) treeRoot (sideTree d))
  result<-try $ do
    root<-canonicalizePath directory
    client<-A.startClient launch root
    ident<-A.request client "initialize" (object ["protocolVersion" .= (1::Int),"clientInfo" .= object ["name" .= ("hide"::Text),"version" .= ("0.1.0.0"::Text)],
      "clientCapabilities" .= object ["fs" .= object ["readTextFile" .= True,"writeTextFile" .= True],"terminal" .= Terminal.terminalAvailable]]) `onException` A.stopClient client
    pure (root,client,ident)
  case result of
    Left (err::IOException) -> do
      writeIORef ref s {queuedPrompt=Nothing}
      pure (message "Cannot start agent" (wrapMessage (T.pack (show err))) d)
    Right (root,client,ident) -> do
      writeIORef ref s {provider=launch,connection=Just client,project=root,deliveredContext=Nothing,agentInitialized=Null,agentConfig=Null,streamTails=M.empty,lastAgentSync=Nothing,pending=M.singleton ident (Initializing resume)}
      pure d {status="Connecting to ACP provider...",agentSteering=False,agentReplying=True,agentContextUsage=Nothing,agentSettings=[]}

restorePrimaryDraft :: Text -> Desktop -> Desktop
restorePrimaryDraft text d
  | T.null (conversationTarget d) = d {composerBuffer=newBuffer text,composerSelection=selected}
  | otherwise = d {conversationViews=M.adjust (\view->view {conversationDraft=newBuffer text,conversationDraftSelection=selected}) "" (conversationViews d)}
  where selected=Selection (T.length text) (T.length text)

restoreEmptyPrimaryDraft :: Text -> Desktop -> Desktop
restoreEmptyPrimaryDraft text d
  | maybe False ((==0) . bufferLength) draft = restorePrimaryDraft text d
  | otherwise = d
  where draft=if T.null (conversationTarget d) then Just (composerBuffer d)
              else conversationDraft <$> M.lookup "" (conversationViews d)

sendQueued :: ConversationState -> Desktop -> IO Desktop
sendQueued (ConversationState _ ref _ _ _) d = do
  s<-readIORef ref
  case (connection s,session s,queuedPrompt s) of
    (Just _,Just _,Just (prompt,receipt)) -> beginPromptPreparation ref False prompt receipt d
    _ -> pure d

beginPromptPreparation :: IORef State -> Bool -> Text -> Maybe DraftReceipt -> Desktop -> IO Desktop
beginPromptPreparation ref steering text receipt d = mask $ \restore -> do
  s<-readIORef ref
  if not (isNothing (promptPreparation s)) || not (null (retiringRequests s))
    then pure d {status="Waiting for the previous agent request to stop."}
    else do
      worker<-async (restore (preparePrompt s text))
      modifyIORef' ref (\state -> state {promptPreparation=Just (steering,text,receipt,worker)})
      pure d {status="Preparing agent context...",agentReplying=True}

pollPromptPreparation :: ConversationState -> Desktop -> IO Desktop
pollPromptPreparation runtime@(ConversationState _ ref _ _ _) d = do
  s<-readIORef ref
  case promptPreparation s of
    Nothing -> sendQueued runtime d
    Just (steering,text,receipt,worker) -> do
      result<-poll worker
      case result of
        Nothing -> pure d
        Just outcome -> do
          modifyIORef' ref (\state -> state {promptPreparation=Nothing})
          let prepared=either (const (Left "Could not prepare agent context.")) id outcome
          case (connection s,session s,prepared) of
            (_,_,Left err) -> do
              unless steering $ do
                finishAgentDelivery ref (Left err)
                modifyIORef' ref (\state -> state {queuedPrompt=Nothing})
              pure (if steering then d else restoreEmptyPrimaryDraft text d) {status=err}
            (Just client,Just sid,Right (blocks,context))
              | steering && Prompting `notElem` M.elems (pending s) -> pure d {status="The turn ended while preparing steering; draft kept."}
              | otherwise -> do
                let method=if steering then "_session/steering" else "session/prompt"
                    meta=["_meta" .= object ["steering" .= object ["idleBehavior" .= ("promptRequired"::Text)]] | steering]
                ident<-A.request client method (object (["sessionId" .= sid,"prompt" .= blocks]++meta))
                modifyIORef' ref (\state -> state {queuedPrompt=if steering then queuedPrompt state else Nothing,
                  pending=M.insert ident (if steering then Steering text receipt else Prompting) (pending state),deliveredContext=Just context})
                cleared<-if steering then pure d else clearSubmittedDraft receipt d
                pure cleared {status=if steering then "Steering request sent; draft kept until accepted." else "Agent is replying...",agentReplying=True}
            _ -> pure d

-- Supply guidance once per connection and again when its saved value changes.
-- A separate text block preserves the user's query and the visible transcript.
preparePrompt :: State -> Text -> IO (Either Text ([Value],Value))
preparePrompt state query=do
  loaded<-readAgentContexts (project state)
  pure $ do
    context<-loaded
    let block text=object ["type" .= ("text"::Text),"text" .= text]
        textAt scope=fromMaybe "" (field scope context >>= field "text")
        section scope title=title<>"\n"<>(if T.null (textAt scope) then "(none)" else textAt scope)
        guidance="Current editor context replaces earlier editor context. It does not grant additional tool permissions.\n\n"<>
          section "global" "Global context:"<>"\n\n"<>section "project" "Project context:"
        catalog="Editor skills: explore projects; edit/review; HLS diagnosis/rename; build/test/run; DAP debugging; Git review; desktop/hex navigation; user questions; documentation/settings. Read docs/agent-skills.md with docs_read (corpus editor) for the relevant workflow and docs/agent-tools.md for operations. Discover exact schemas with tools/list. For missing executables or libraries, inspect environment_get and fix paths with environment_set; do not prescribe shell exports or an editor restart when a new job suffices. Prefer repository build configuration fixes for project dependencies."
        extra=[guidance | deliveredContext state/=Just context]++[catalog | deliveredContext state==Nothing]
    pure (map block (composerMarkdown query:extra),context)

-- The badge uses only the global toolchain field, so it need not resolve a
-- project or load its targets. One worker refreshes at most once per second.
-- Explicit saves advance the version so an older read cannot undo that choice.
pollBuildSettings :: FilePath -> IORef State -> Desktop -> IO Desktop
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

-- | Advance mailboxes, protocol replies, approvals and transcript views.
-- The caller serializes access to both desktop and conversation state.
tickConversation :: ConversationState -> Desktop -> IO Desktop
tickConversation runtime@(ConversationState directory ref consoles jobs _) original = do
  launched<-pollShellLaunches runtime original
  loaded<-pollBuildSettings directory ref launched
  menuReady<-pollCompilerMenu runtime loaded
  fresh<-pruneChildApprovals runtime menuReady
  initial<-drainConversationAgents runtime fresh
  currentQuestion<-readIORef ref
  ready<-case waitingQuestion currentQuestion of
    Just (QuestionTicket _ _ (Just receipt))->do
      live<-providerCurrent receipt currentQuestion
      if live then pure initial else cancelQuestion runtime "Question requester ended." initial
    _->pure initial
  d<-C.tickConsoles consoles ready >>= Jobs.tickBuildJobs jobs
  flushTerminalWaiters runtime
  s<-readIORef ref
  events<-maybe (pure []) A.pollEvents (connection s)
  received<-foldM (receive runtime) d events
  captured<-pollFileCaptures runtime received
  updated<-pollPromptPreparation runtime captured
  afterEvents<-readIORef ref
  advanced<-case queuedQueries afterEvents of
    query:rest | not (busy afterEvents), not (isNothing (connection afterEvents)), session afterEvents/=Nothing -> do
      text<-case query of
        SubmittedQuery value->pure (Just value)
        QuestionQuery ident actor receipt answer->do
          live<-providerCurrent receipt afterEvents
          active<-AH.statusAgent (AR.agentHub (conversationAgents runtime)) (AH.Agent actor) actor
          pure $ if live && either (const False) (const True) active
            then Just ("Human answer to ask_user question "<>T.pack (show ident)<>" (submitted explicitly):\n\n"<>answer) else Nothing
      writeIORef ref afterEvents {queuedQueries=rest,queuedPrompt=(\value->(value,Nothing)) <$> text,reads=sourceSnapshots updated}
      maybe (pure updated) (const (sendQueued runtime updated)) text
    _ -> pure updated
  current<-readIORef ref
  -- Esc/Cancel of a permission dialog denies it; it must never leave the peer waiting.
  case presented current of
    Just token | not (isApprovalDialog token advanced) -> do
      forM_ (lookup token (approvals current)) $ \approval -> denyChild approval >> mapM_ (\client -> cancelApproval client approval) (connection current)
      modifyIORef' ref (\state -> state {approvals=filter ((/=token).fst) (approvals state),presented=Nothing})
    _ -> pure ()
  laidOut<-refreshConversationLayout runtime advanced
  afterDismiss<-readIORef ref
  let rendered=laidOut {agentReplying=busy afterDismiss,agentQueued=length (queuedQueries afterDismiss)}
  syncConversationAgent runtime
  visible<-refreshChildConversation runtime rendered
  created<-pollAgentCreation runtime visible
  shown<-present runtime created
  notice<-AR.runtimeNotice (conversationAgents runtime)
  pure (maybe shown (\text -> shown {status=text}) notice)

-- Window changes also arrive in effect batches with no protocol actions. Reflow
-- before returning that desktop, so its bubbles and composer use the same bounds.
-- The immutable transcript key keeps mouse motion and idle ticks parse-free.
refreshConversationLayout :: ConversationState -> Desktop -> IO Desktop
refreshConversationLayout (ConversationState _ ref _ _ _) original = do
  state<-readIORef ref
  transcriptIdentity<-makeStableName =<< evaluate (transcript state)
  questionKey<-questionIdentity (chatQuestion original)
  let widthNow=conversationWidthFor "" original
      renderKey=(widthNow,session state,transcriptIdentity)
      -- A recovered view has no raw transcript owned by this runtime yet.
      ownsView=not (isNothing (connection state)) || not (null (transcript state)) || not (isNothing questionKey) || not (isNothing (lastQuestion state))
      redraw=ownsView && (lastRender state/=Just renderKey || lastQuestion state/=questionKey)
  primary<-if redraw then paint False state original else pure original
  when redraw (modifyIORef' ref (\current -> current {lastRender=Just renderKey,lastQuestion=questionKey}))
  -- Cached children can remain visible beside the selected conversation. Their
  -- geometry is independent, and reflow needs no provider/history round trip.
  foldM (reflowChild state) primary (M.toList (childRecords state))
  where
    reflowChild state desktop (target,records) =
      case conversationDocument target desktop of
        Just (bid,_) | any ((==Just bid) . bufferId) (windows desktop),
            let columns=conversationWidthFor target desktop,
            M.lookup target (childWidths state)/=Just columns -> do
          modifyIORef' ref (\current->current
            { childWidths=M.insert target columns (childWidths current)
            , childRender=case childRender current of
                Just (shown,_,entry) | shown==target -> Just (shown,columns,entry)
                other -> other })
          paintView target False state {transcript=records} desktop
        _ -> pure desktop

-- Acquisition owns each child until the UI adopts it. Session teardown cancels
-- pending launches and closes any completed child that has not been adopted.
pollShellLaunches :: ConversationState -> Desktop -> IO Desktop
pollShellLaunches (ConversationState _ ref consoles _ _) original = mask_ $ do
  state<-readIORef ref
  results<-mapM (\worker->(worker,) <$> poll worker) (shellLaunches state)
  modifyIORef' ref (\current->current {shellLaunches=[worker | (worker,Nothing)<-results]})
  foldM adopt original [result | (_,Just result)<-results]
  where
    adopt d (Right (Right prepared)) = snd <$> C.adoptConsole consoles prepared d `onException` C.closePreparedConsole prepared
    adopt d (Right (Left err)) = pure (message "Cannot execute shell block" (wrapMessage err) d)
    adopt d (Left _) = pure (message "Cannot execute shell block" ["Terminal launch failed."] d)

receive :: ConversationState -> Desktop -> A.Event -> IO Desktop
receive runtime@(ConversationState _ ref consoles _ _) d event = do
  s<-readIORef ref
  case event of
    A.Disconnected reason -> do
      cleared<-cancelQuestion runtime "Question requester disconnected." d
      current<-readIORef ref
      redact<-conversationRedactor runtime current
      finishAgentDelivery ref (Left "Agent disconnected.")
      AR.failPendingPrimary (conversationAgents runtime) "Agent disconnected."
      mapM_ denyChild (map snd (approvals s))
      mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
      retired<-retireRequests ref
      writeIORef ref retired {connection=Nothing,session=Nothing,agentDelivery=Nothing,streamTails=M.empty,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty,
        transcript=transcript current++[activity "Connection closed" (object ["message" .= redact reason])]}
      pure (dismissPermission cleared) {status="Agent disconnected.",agentSteering=False}
    A.Response ident result -> do
      writeIORef ref s {pending=M.delete ident (pending s)}
      when (M.lookup ident (pending s)==Just Prompting) (flushConversationChunks ref)
      case (M.lookup ident (pending s),result,connection s) of
        (Nothing,_,_) -> pure d
        (_,Left err,_) -> do
          redact<-conversationRedactor runtime s
          when (M.lookup ident (pending s)==Just Prompting) (completeConversationDelivery runtime (Left "Agent prompt failed."))
          modifyIORef' ref (\state -> state {queuedPrompt=Nothing,deliveredContext=Nothing,transcript=transcript state++[activity "Request failed" (redactValue redact err)]})
          let restored=case M.lookup ident (pending s) of Just (Steering text _) -> restoreEmptyPrimaryDraft text d; _ -> d
          pure restored {status="Agent request failed; see Conversation."}
        (Just (Initializing resume),Right value,Just client)
          | field "protocolVersion" value /= Just (1::Int) -> do
              A.stopClient client
              modifyIORef' ref (\state -> state {queuedPrompt=Nothing})
              pure d {status="Unsupported ACP protocol version."}
          | otherwise -> do
              let capabilities=fromMaybe Null (field "agentCapabilities" value)
                  loadSupported=field "loadSession" capabilities==Just True
                  resumeSupported=case field "sessionCapabilities" capabilities >>= field "resume" of Just (Object _) -> True; _ -> False
                  method=case resume of Nothing -> "session/new"; Just _ | loadSupported -> "session/load"; _ -> "session/resume"
              if resume/=Nothing && not loadSupported && not resumeSupported then pure d {status="This provider cannot resume sessions."}
              else do
                servers<-AR.primaryServers (conversationAgents runtime)
                requestId<-A.request client method (object (["cwd" .= project s,"mcpServers" .= servers]++maybe [] (\sid->["sessionId" .= sid]) resume))
                modifyIORef' ref (\state -> state {pending=M.insert requestId (Starting resume) (pending state),session=resume,agentInitialized=value})
                pure d {status="Opening agent session...",agentSteering=(field "_meta" value >>= field "steering" >>= field "supported")==Just True}
        (Just (Starting resumed),Right value,_) -> case field "sessionId" value <|> resumed of
          Nothing -> pure d {status="Agent returned no session ID."}
          Just sid -> do
            modifyIORef' ref (\state -> state {session=Just sid,agentConfig=value,lastSession=Just (provider state,project state,sid)})
            savedId<-persist (resumeRecordPath s) (object ["provider" .= launchValue (provider s),"cwd" .= project s,"sessionId" .= sid])
            safeSettings<-publicAgentSettings runtime value
            sendQueued runtime d {agentSettings=safeSettings,status=either ("Session opened; could not save ID: "<>) (const ("Session "<>sid)) savedId}
        (Just Setting,Right value,_) -> do
          modifyIORef' ref (\state -> state {agentConfig=value})
          safeSettings<-publicAgentSettings runtime value
          pure d {agentSettings=safeSettings,contextMenu=Nothing,status="Conversation settings updated."}
        (Just (Steering text receipt),Right value,_) -> case field "outcome" value :: Maybe Text of
          Just "injected" -> do
            modifyIORef' ref (\state->state {transcript=transcript state++[Reply "You" (composerMarkdown text)]})
            cleared<-clearSubmittedDraft receipt d
            pure cleared {status="Follow-up added to the active turn."}
          Just outcome | outcome `elem` ["promptRequired","failed"] -> do
            modifyIORef' ref (\state->state {deliveredContext=Nothing})
            pure d {status="Steering was not applied; the draft remains. Use Query to send it."}
          _ -> do
            mapM_ A.stopClient (connection s)
            stopped<-receive runtime d (A.Disconnected "Provider started an unowned steering turn or returned an unknown outcome.")
            pure stopped {status="Steering ownership was not confirmed; provider stopped. Draft kept; queued turns cancelled without replay."}
        (Just Prompting,Right value,_) -> do
          current<-readIORef ref
          let text=case reverse (transcript current) of Reply "Agent" body:_ -> body; _ -> ""
          redact<-conversationRedactor runtime current
          completeConversationDelivery runtime (Right (object ["text" .= redact text,"stopReason" .= fmap redact (field "stopReason" value :: Maybe Text)]))
          pure d {status="Agent: "<>redact (fromMaybe "finished" (field "stopReason" value))}
        _ -> pure d
    A.Notification "session/update" params
      | field "sessionId" params==session s || any isStarting (M.elems (pending s)) -> do
          let update=fromMaybe Null (field "update" params)
              kind=fromMaybe "" (field "sessionUpdate" update :: Maybe Text)
          case kind of
            "agent_message_chunk" -> appendReply "Agent" update
            "user_message_chunk" -> appendReply "You" update
            "tool_call" -> recordTool update
            "tool_call_update" -> recordTool update
            "plan" -> do
              redact<-conversationRedactor runtime s
              modifyIORef' ref (\state -> state {transcript=transcript state++[activity "Plan" (redactValue redact update)]})
            _ -> pure ()
          when (kind=="config_option_update") (modifyIORef' ref (\state -> state {agentConfig=update}))
          safeSettings<-if kind=="config_option_update" then publicAgentSettings runtime update else pure []
          pure $ if kind=="config_option_update" then d {agentSettings=safeSettings,contextMenu=Nothing}
            else if kind=="usage_update" then case (field "used" update,field "size" update) of
            (Just used,Just size) | used>=0 && size>0 -> d {agentContextUsage=Just (used,size)}
            _ -> d
            else d
      | otherwise -> pure d
    A.Request ident method params -> case connection s of
      Nothing -> pure d
      Just client | field "sessionId" params/=session s || session s==Nothing -> A.respond client ident (Left (failure "Unknown session.")) >> pure d
      Just client -> incoming runtime client ident method params d
    _ -> pure d
  where
    isStarting Starting{}=True; isStarting _=False
    appendReply role update = case field "content" update of
      Just content | field "type" content==Just ("text"::Text),Just text<-field "text" content, not (T.null text) -> do
        current<-readIORef ref
        keys<-conversationKeys runtime current
        now<-getCurrentTime
        zone<-getCurrentTimeZone
        modifyIORef' ref $ \state ->
          let combined=M.findWithDefault "" role (streamTails state)<>text
              redacted=redactText keys combined
              held=maximum (0:[n | key<-keys,n<-[1..T.length key-1],T.take n key `T.isSuffixOf` redacted])
              (safe,tailText)=T.splitAt (T.length redacted-held) redacted
              timed=if T.null safe then state else stampReply now zone state
          in timed {transcript=if T.null safe then transcript timed else appendChunk role safe (transcript timed),
            streamTails=M.insert role tailText (streamTails timed)}
      _ -> pure ()
    recordTool update=do
      current<-readIORef ref
      redact<-conversationRedactor runtime current
      now<-getCurrentTime
      modifyIORef' ref (\state -> state {lastMessageAt=Just now,transcript=mergeTool (redactValue redact update) (transcript state)})

pauseLabel :: Maybe UTCTime -> UTCTime -> TimeZone -> Maybe Text
pauseLabel previous now zone = case previous of
  Just before | diffUTCTime now before>=300 -> Just (T.pack (formatTime defaultTimeLocale "%b %-d, %H:%M" (utcToLocalTime zone now)))
  _ -> Nothing

stampReply :: UTCTime -> TimeZone -> State -> State
stampReply now zone s = s {lastMessageAt=Just now,
  transcript=transcript s++maybe [] (\label -> [Pause label]) (pauseLabel (lastMessageAt s) now zone)}

renderTimestamp :: Int -> Text -> [(Char,Style)]
renderTimestamp width label = map (,Comment) (T.unpack (T.replicate (max 0 ((width-T.length label) `div` 2)) " "<>T.take (max 0 width) label))

-- Tool records never pass through the Markdown parser.
appendChunk :: Text -> Text -> [Record] -> [Record]
appendChunk role text records = case reverse records of
  Reply previous body:rest | previous==role -> reverse rest++[Reply role (body<>text)]
  _ -> records++[Reply role text]

mergeTool :: Value -> [Record] -> [Record]
mergeTool update records = case field "toolCallId" update :: Maybe Text of
  Nothing -> records
  Just ident ->
    let merge (Activity old (Object previous) history expanded)
          | old==ident, Object new<-update = Activity old (Object (KM.union (KM.filter (/=Null) new) previous)) (history++[update]) expanded
        merge other=other
    in if any (\record -> case record of Activity old _ _ _ -> old==ident; _ -> False) records
       then map merge records else records++[activity ident update]

incoming :: ConversationState -> A.Client -> Value -> Text -> Value -> Desktop -> IO Desktop
incoming runtime@(ConversationState _ ref consoles _ _) client ident method params d = do
  s<-readIORef ref
  case method of
    "session/request_permission" -> case permissionOptions params of
      [] -> A.respond client ident (Right cancelled) >> pure d
      options -> do
        redact<-conversationRedactor runtime s
        enqueueApproval runtime (Permission ident [(key,redact name) | (key,name)<-options] (redactValue redact (fromMaybe Null (field "toolCall" params))))
        pure d
    "fs/read_text_file" -> case parseMaybe (withObject "read" $ \o -> (,,) <$> o .: "path" <*> o .:? "line" .!= (1::Int) <*> o .:? "limit") params of
      Nothing -> bad "Expected a path and optional integer line and limit."
      Just (path,line,limit)
        | line<1 || maybe False (<0) limit -> bad "Invalid line range."
        | otherwise -> queueFileCapture ref client ident (ReadFile line limit) (project s) path d
    "fs/write_text_file" -> case (field "path" params,field "content" params) of
      (Just path,Just content) -> queueFileCapture ref client ident (WriteFile content) (project s) path d
      _ -> bad "Expected path and content."
    "terminal/create" | Terminal.terminalAvailable -> case parseTerminal (project s) params of
      Left err -> bad err
      Right (config,limit) -> do
        checked<-try (canonicalizePath (Terminal.terminalDirectory config)) :: IO (Either IOException FilePath)
        case checked of
          Right root | let relative=makeRelative (project s) root, not (isAbsolute relative), ".." `notElem` splitDirectories relative ->
            enqueueApproval runtime (Execute ident config {Terminal.terminalDirectory=root} limit) >> pure d
          _ -> bad "Terminal directory is outside the session project."
    "terminal/output" -> terminalId $ \tid -> do
      result<-C.consoleOutput consoles tid
      A.respond client ident (either (Left . failure) (\(bytes,truncated,exited) -> Right (object
        (["output" .= TE.decodeUtf8With lenientDecode bytes,"truncated" .= truncated]++maybe [] (\code->["exitStatus" .= exitStatus code]) exited))) result)
      pure d
    "terminal/wait_for_exit" -> terminalId $ \tid -> do
      result<-C.consoleOutput consoles tid
      case result of
        Left err -> bad err
        Right (_,_,Just code) -> A.respond client ident (Right (exitStatus code)) >> pure d
        Right _ -> modifyIORef' ref (\state -> state {terminalWaiters=M.insertWith (++) tid [ident] (terminalWaiters state)}) >> pure d
    "terminal/kill" -> terminalId $ \tid -> C.killConsole consoles tid >>= replyUnit
    "terminal/release" -> terminalId $ \tid -> do
      result<-C.releaseConsole consoles tid
      modifyIORef' ref (\state -> state {ownedTerminals=S.delete tid (ownedTerminals state)})
      forM_ (M.findWithDefault [] tid (terminalWaiters s)) $ \waiter -> A.respond client waiter (Left (failure "Terminal released."))
      modifyIORef' ref (\state -> state {terminalWaiters=M.delete tid (terminalWaiters state)})
      replyUnit result
    _ -> A.respond client ident (Left (object ["code" .= (-32601::Int),"message" .= ("Unsupported client method: "<>method)])) >> pure d
  where
    bad err=A.respond client ident (Left (failure err)) >> pure d
    terminalId action=do
      state<-readIORef ref
      case field "terminalId" params of
        Just tid | S.member tid (ownedTerminals state) -> action tid
        _ -> bad "Unknown terminal ID for this session."
    replyUnit result=A.respond client ident (either (Left . failure) (const (Right (object []))) result) >> pure d

-- Bound retained desktop snapshots and filesystem workers together, including
-- cancellation still waiting on the OS. Completion stays in request order;
-- workers never modify conversation state or grant an approval.
queueFileCapture :: IORef State -> A.Client -> Value -> FileRequest -> FilePath -> FilePath -> Desktop -> IO Desktop
queueFileCapture ref client ident request root path d = mask $ \restore -> do
  s<-readIORef ref
  if length (fileCaptures s)+sum (map fst (retiringRequests s))>=4
    then A.respond client ident (Left (failure "Too many pending file requests."))
    else do
      worker<-async (restore (prepareFileCapture ident request root path d))
      modifyIORef' ref (\state -> state {fileCaptures=fileCaptures state++[FileCapture ident worker]})
  pure d

prepareFileCapture :: Value -> FileRequest -> FilePath -> FilePath -> Desktop -> IO (Either Text CapturedFile)
prepareFileCapture ident request root path before = do
  captured<-captureFile root path before
  case captured of
    Left err -> pure (Left err)
    Right snap -> do
      expected<-sourceIdentity (snapshotPath snap) before
      action<-case request of
        WriteFile content -> pure (CapturedWrite content)
        ReadFile line limit -> do
          let content=snapshotText snap
              buffer=newBuffer content
              startOffset=if line>bufferLineCount buffer then T.length content else bufferLineOffset buffer (line-1)
              endOffset=case limit of
                Nothing -> T.length content
                Just count | count>=bufferLineCount buffer-line+1 -> T.length content
                           | otherwise -> bufferLineOffset buffer (line-1+count)
              chosen=T.take (max 0 (endOffset-startOffset)) (T.drop startOffset content)
          CapturedRead <$> A.prepareResponse ident (Right (object ["content" .= chosen]))
      pure (Right (CapturedFile snap expected action))

retireRequests :: IORef State -> IO State
retireRequests ref = mask $ \restore -> do
  s<-readIORef ref
  let captures=fileCaptures s
      workers=[cancel worker | FileCapture _ worker<-captures]++[cancel worker | (_,_,_,worker)<-maybe [] pure (promptPreparation s)]
  if null workers then pure s else do
    forM_ (connection s) $ \client -> forM_ captures $ \(FileCapture ident _) ->
      A.respond client ident (Left (failure "File request cancelled."))
    reaper<-async (restore (sequence_ workers))
    let next=s {fileCaptures=[],promptPreparation=Nothing,retiringRequests=retiringRequests s++[(length workers,reaper)]}
    writeIORef ref next
    pure next

pollFileCaptures :: ConversationState -> Desktop -> IO Desktop
pollFileCaptures runtime@(ConversationState _ ref _ _ _) d = do
  s<-readIORef ref
  retiring<-filterM (fmap isNothing . poll . snd) (retiringRequests s)
  modifyIORef' ref (\state -> state {retiringRequests=retiring})
  drain
  pure d
  where
    drain=do
      s<-readIORef ref
      case (connection s,fileCaptures s) of
        (Just client,FileCapture ident worker:rest) -> do
          ready<-poll worker
          case ready of
            Nothing -> pure ()
            Just result -> do
              modifyIORef' ref (\state -> state {fileCaptures=rest})
              case either (const (Left "File request failed.")) id result of
                Left err -> A.respond client ident (Left (failure err))
                Right (CapturedFile snap expected action)
                  | protectedPath d (snapshotPath snap) || any (\(bid,doc) ->
                      fmap filePath (documentFile doc)==Just (snapshotPath snap) && (protectedBuffer d bid || not (textBuffer (documentBuffer doc)))) (M.toList (buffers d)) ->
                      A.respond client ident (Left (failure "Agent authority files require human input."))
                  | otherwise -> do
                      current<-sourceIdentity (snapshotPath snap) d
                      if current/=expected then A.respond client ident (Left (failure "File changed in the editor during capture; request a fresh read."))
                      else case action of
                        CapturedRead response -> do
                          modifyIORef' ref (\state -> state {reads=M.insert (snapshotPath snap) snap (reads state)})
                          A.respondPrepared client response
                        CapturedWrite content -> enqueueApproval runtime (Write ident (M.findWithDefault snap (snapshotPath snap) (reads s)) content)
              drain
        _ -> pure ()

permissionOptions :: Value -> [(Text,Text)]
permissionOptions params=map (\(_,ident,name)->(ident,name)) . sortOn (\(kind,_,_)->not ("reject" `T.isPrefixOf` kind)) $
  mapMaybe (parseMaybe (withObject "permission" $ \o -> (,,) <$> o .: "kind" <*> o .: "optionId" <*> o .: "name")) (fromMaybe [] (field "options" params))

enqueueApproval :: ConversationState -> Approval -> IO ()
enqueueApproval (ConversationState _ ref _ _ _) approval=modifyIORef' ref (\s -> s {approvals=approvals s++[(nextApproval s,approval)],nextApproval=nextApproval s+1})

present :: ConversationState -> Desktop -> IO Desktop
present (ConversationState _ ref _ _ _) d = do
  s<-readIORef ref
  case (dialog d,presented s,approvals s) of
    (Nothing,Nothing,(token,approval):_) | not (deferredApproval s) -> do
      modifyIORef' ref (\state -> state {presented=Just token})
      let action="approval:"<>T.pack (show token)
      pure $ case approval of
        ChildPermission ident request _ -> d {dialog=Just (Dialog ("Agent permission: "<>AH.agentIdText ident) (AgentDialog action)
          [ListBox "Action" [label | (_,label,_)<-AP.permissionOptions request] 0] 0 ["Choose","Reject"]
          (take 7 (wrapMessage (AP.permissionTitle request)++wrapMessage (AP.permissionDetails request))))}
        Permission _ options detail -> d {dialog=Just (Dialog "Agent permission" (AgentDialog action)
          [ListBox "Action" (map snd options) 0] 0 ["Choose","Review","Cancel"]
          (take 6 (wrapMessage (fromMaybe "Tool permission" (field "title" detail))++wrapMessage (jsonText detail))))}
        Execute _ config _ -> d {dialog=Just (Dialog "Run agent command" (AgentDialog action) [] 0 ["Run","Reject"]
          (take 7 (wrapMessage (T.pack (Terminal.terminalCommand config)) ++ wrapMessage (jsonText (Terminal.terminalArguments config)) ++ wrapMessage (T.pack (Terminal.terminalDirectory config)))))}
        Write _ snap content -> (addReadOnly "Proposed agent edit" ("CURRENT BUFFER\n"<>snapshotText snap<>"\n\nPROPOSED CONTENT\n"<>content) d)
          {dialog=Just (Dialog "Apply agent edit" (AgentDialog action) [] 0 ["Apply","Review","Reject"]
            ["The proposed edit is open behind this dialog.","Apply saves it and preserves the old buffer in Undo."])}
    _ -> pure d

decide :: ConversationState -> Int -> [Text] -> Desktop -> IO Desktop
decide (ConversationState _ ref consoles _ _) token values d = do
  s<-readIORef ref
  case (connection s,lookup token (approvals s)) of
    (_,Just (ChildPermission _ request reply)) -> do
      let selected=case values of
            "0":index:_ | Just n<-readMaybe (T.unpack index),n>=0,(chosen,_,_):_<-drop n (AP.permissionOptions request) -> Just chosen
            _ -> Nothing
      void (tryPutMVar reply selected)
      writeIORef ref s {approvals=filter ((/=token).fst) (approvals s),presented=Nothing}
      pure d
    (Just _,Just approval) | take 1 values==["1"],not (isExecute approval) -> do
      writeIORef ref s {presented=Nothing,deferredApproval=True}
      let detail=case approval of
            Permission _ _ value -> jsonText value
            Write _ snap text -> "FILE: "<>T.pack (snapshotPath snap)<>"\n\nCURRENT BUFFER\n"<>snapshotText snap<>"\n\nPROPOSED CONTENT\n"<>text
            _ -> ""
      pure (addReadOnly "Agent request" detail d) {status="Tools > Conversation returns to the pending approval."}
    (Just client,Just approval) -> do
      writeIORef ref s {approvals=filter ((/=token).fst) (approvals s),presented=Nothing}
      case approval of
        Permission ident options _ -> case values of
          "0":index:_ | Just n<-readMaybe (T.unpack index),n>=0, (chosen,_):_<-drop n options ->
            A.respond client ident (Right (object ["outcome" .= object ["outcome" .= ("selected"::Text),"optionId" .= chosen]])) >> pure d
          _ -> A.respond client ident (Right cancelled) >> pure d
        Write ident snap text | take 1 values==["0"] -> do
          result<-acceptWrite snap text d
          case result of
            Left err -> A.respond client ident (Left (failure err)) >> pure (message "Agent edit rejected" (wrapMessage err) d)
            Right changed -> do
              A.respond client ident (Right (object []))
              modifyIORef' ref (\state -> state {reads=maybe (reads state) (\fresh -> M.insert (snapshotPath fresh) fresh (reads state)) (M.lookup (snapshotPath snap) (sourceSnapshots changed))})
              pure changed
        Execute ident config limit | take 1 values==["0"] -> do
          result<-C.startConsole consoles config limit d
          case result of
            Left err -> A.respond client ident (Left (failure err)) >> pure d {status=err}
            Right (tid,changed) -> do
              modifyIORef' ref (\state -> state {ownedTerminals=S.insert tid (ownedTerminals state)})
              A.respond client ident (Right (object ["terminalId" .= tid]))
              pure changed
        _ -> cancelApproval client approval >> pure d
    _ -> pure d {status="Permission request expired."}
  where isExecute Execute{}=True; isExecute _=False

cancelApproval :: A.Client -> Approval -> IO ()
cancelApproval client approval=case approval of
  ChildPermission _ _ reply -> void (tryPutMVar reply Nothing)
  Permission ident _ _ -> A.respond client ident (Right cancelled)
  Execute ident _ _ -> A.respond client ident (Left (failure "User rejected the command."))
  Write ident _ _ -> A.respond client ident (Left (failure "User rejected the edit."))

isApprovalDialog :: Int -> Desktop -> Bool
isApprovalDialog token d = case dialog d of Just dg -> purpose dg==AgentDialog ("approval:"<>T.pack (show token)); _ -> False

dismissPermission :: Desktop -> Desktop
dismissPermission d=case dialog d of
  Just dg | AgentDialog action<-purpose dg,"approval:" `T.isPrefixOf` action -> d {dialog=Nothing}
  _ -> d

keepConversationPosition :: Desktop -> Desktop -> Desktop
keepConversationPosition before after=after {windows=map keep (windows after)}
  where
    keep w=case (find ((==windowId w).windowId) (windows before),windowDocument (buffers after) w) of
      (Just old,Just doc) | documentLabel doc==Just "Conversation" -> w {scrollRow=min (scrollRow old) (scrollbarLimit after True doc w),scrollColumn=0,selection=Selection 0 0}
      _ -> w

isToolRecord :: Record -> Bool
isToolRecord (Activity _ value _ _)=field "status" value `elem`
  [Just ("pending"::Text),Just "in_progress",Just "completed",Just "failed"]
isToolRecord _=False

-- The immutable question key detects replacement without comparing its answer
-- buffer or retaining separate baseline/Undo roots in the presentation cache.
questionIdentity :: Maybe ChatQuestion -> IO (Maybe (StableName ChatQuestion))
questionIdentity=traverse (\q->makeStableName =<< evaluate q)

paint :: Bool -> State -> Desktop -> IO Desktop
paint=paintView ""

paintView :: Text -> Bool -> State -> Desktop -> IO Desktop
paintView target force s original
  | not force && isNothing (conversationDocument target d) = pure original
  | otherwise = do
    questionKey<-questionIdentity (chatQuestion d)
    pure $ let
      width=conversationWidthFor target d
      header=if T.null target then "Session: "<>fromMaybe "not connected" (session s)<>"\n" else "No messages yet.\n"
      records=zip [0..] (transcript s)
      chunks=if null records then [(plain Comment header,Nothing,[]) | isNothing (chatQuestion d)] else renderRecords width records
      questionChunks=maybe [] (renderQuestion width (length records)) (chatQuestion d)
      allChunks=chunks++[ (plain Plain "\n\n",Nothing,[]) | not (null chunks) && not (null questionChunks)]++questionChunks
      styled=concatMap (\(cells,_,_)->cells) allChunks
      (_,shellBlocks)=foldl (\(offset,found) (cells,_,blocks)->(offset+length cells,found++[(offset+a,offset+b,dialect,body) | (a,b,dialect,body)<-blocks])) (0,[]) allChunks
      (_,actions)=foldl (\(offset,found) (cells,action,_)->(offset+length cells,found++maybe [] (\(name,values)->[(offset,offset+length cells,name,values)]) action)) (0,[]) allChunks
      inputOffset=case [a+7 | (a,_,action,_)<-actions,action=="question-input"] of offset:_->Just offset; _->Nothing
      text=T.pack (map fst styled)
      questionRow=do
        q<-chatQuestion d
        if not (questionFocused q) || lastQuestion s==questionKey then Nothing else do
          offset<-case questionChoice q of
            Nothing -> inputOffset
            Just index -> case [a | (a,_,action,values)<-actions,action=="question-choice",values==[T.pack (show (questionToken q)),T.pack (show index)]] of a:_->Just a; _->Nothing
          pure (fst (lineColumn text offset)+if isNothing (questionChoice q) then 1 else 0)
      existing=conversationDocument target d
      opened=case existing of
        Nothing -> let added=addConversationDocument d in added {buffers=M.adjust (\doc->restyle doc {documentBuffer=newBuffer text}) (nextId d) (buffers added)}
        Just (existingId,_) -> d {buffers=M.adjust (\doc->restyle doc {documentBuffer=(newBuffer text) {revision=revision (documentBuffer doc)+1}}) existingId (buffers d)}
      bid=maybe (nextId d) fst existing
      adjust w | bufferId w/=Just bid = w
               | otherwise =
                   let rows=max 1 (windowContentRows opened (fromMaybe (newDocument (newBuffer "") Nothing) (M.lookup bid (buffers opened))) w)
                       oldLines=maybe 0 (bufferLineCount . documentBuffer . snd) existing
                       newLines=length (T.splitOn "\n" text)
                       atEnd=scrollRow w>=max 0 (oldLines-rows)
                       bounded n=max 0 (min (T.length text) n)
                       previousRow=if atEnd then max 0 (newLines-rows) else min (max 0 (newLines-rows)) (scrollRow w)
                       visibleRow=maybe previousRow (\r->max 0 (if r<previousRow then r else if r>=previousRow+rows then r-rows+1 else previousRow)) questionRow
                   in w {scrollRow=visibleRow,
                         selection=Selection (bounded (anchor (selection w))) (bounded (caret (selection w)))}
      visible=target==conversationTarget original
      view=M.findWithDefault (ConversationView bid "Primary" (newBuffer "") (Selection 0 0) (0,0) (Selection 0 0)) target (conversationViews opened)
      -- Generated Markdown admission is checked by the presentation worker;
      -- never scan the styled transcript while adopting it on the UI owner.
      colored=opened {conversationViews=M.insert target view {conversationBufferId=bid,conversationReplySelection=let Selection a c=conversationReplySelection view in Selection (min (T.length text) a) (min (T.length text) c)} (conversationViews opened),chatQuestion=chatQuestion original,chatActions=if visible then actions else chatActions original,chatInputOffset=if visible then inputOffset else chatInputOffset original,buffers=M.adjust (\doc -> doc {documentHighlight=styled,documentHasLayoutMetadata=False,documentCursorVisible=False,documentLinks=linkSpans styled,documentMarkdownPath=Just (project s </> "conversation.md"),documentShellBlocks=shellBlocks}) bid (buffers opened),windows=map adjust (windows opened)}
      focused=case find ((==Just bid) . bufferId) (windows colored) of Just w | force && visible -> focusWindow (windowId w) colored {composerFocused=True}; _ -> colored
      in focused
  where
    d=if T.null target && T.null (conversationTarget original) then original else original {chatQuestion=Nothing}
    plain style=map (,style).T.unpack
    renderRecords _ []=[]
    renderRecords width rows@(record:rest)
      | (calls@(_:_:_),after)<-span (isToolRecord.snd) rows = renderRun width calls++continue (last calls) after
      | otherwise = renderRecord width record++continue record rest
      where
        continue _ []=[]
        continue previous remaining@(next:_)=
          [(plain Plain (if sameSpeaker (snd previous) (snd next) then "\n" else "\n\n"),Nothing,[])]++renderRecords width remaining
    renderRun width calls=case calls of
      (_,Activity ident _ _ _):_ ->
        let expanded=S.member (target,ident) (expandedToolRuns s)
            titles=[fromMaybe label (field "title" value) | (_,Activity label value _ _)<-calls]
            running=length [() | (_,Activity _ value _ _)<-calls,field "status" value `elem` [Just ("pending"::Text),Just "in_progress"]]
            failed=length [() | (_,Activity _ value _ _)<-calls,field "status" value==Just ("failed"::Text)]
            count n label=[T.pack (show n)<>label | n>0]
            summary=T.intercalate " · " (T.pack (show (length calls))<>" tool calls":count running " running"++count failed " failed"++[T.intercalate ", " (take 3 titles)])
            heading=clipCells width ((if expanded then "▾▾ " else "▸▸ ")<>T.unwords (T.words summary))
        in [(plain Pragma heading,Just ("toggle-tool-run",[ident]),[])]++
           (if expanded then concatMap (\call->(plain Plain "\n  ",Nothing,[]):renderRecord (max 1 (width-2)) call) calls else [])
      _ -> []
    sameSpeaker (Reply a _) (Reply b _)=a==b
    sameSpeaker _ _=False
    renderRecord width (recordId,record)=case record of
      Pause label -> [(renderTimestamp width label,Nothing,[])]
      Reply role text -> [replyChunk width recordId (role=="You") text]
      Activity ident value history expanded ->
        let title=T.unwords (T.words ("["<>fromMaybe "activity" (field "status" value)<>"] "<>fromMaybe ident (field "title" value)))
            heading=(if expanded then "▾ " else "▸ ")<>clipCells (max 1 (width-2)) title
        in [(plain Pragma heading,Just ("toggle-activity",[T.pack (show recordId)]),[])]++
          [(plain Plain ("\n"<>T.intercalate "\n" (map jsonText history)),Nothing,[]) | expanded]
    renderQuestion width recordId q=
      [replyChunk width recordId False (questionText q)]++
      concat [[(plain Plain "\n",Nothing,[]),(plain (if questionChoice q==Just index then Keyword else Plain)
        (choiceLines width (questionChoice q==Just index) text),Just ("question-choice",[token,T.pack (show index)]),[])] | (index,text)<-zip [0::Int ..] (questionChoices q)]++
      [(plain Plain "\n",Nothing,[]),(plain (if isNothing (questionChoice q) then Literal else Plain)
        ("Other: "<>questionVisibleInput width q<>" "),Just ("question-input",[token]),[]),
       (plain Plain "\n",Nothing,[]),(plain Keyword "[Submit answer]",Just ("question-submit",[token]),[]),
       (plain Plain "  ",Nothing,[]),(plain Comment "[Cancel]",Just ("question-cancel",[token]),[])]
      where token=T.pack (show (questionToken q))

    replyChunk width recordId outgoing text =
      let (cells,blocks)=renderReplyWithShellBlocks (videoMode d/=Nothing) width outgoing text
      in (map (\(c,style)->(c,case style of BubbleText _ sent base->BubbleText recordId sent base; _->style)) cells,Nothing,blocks)

choiceLines :: Int -> Bool -> Text -> Text
choiceLines width selected text=T.intercalate "\n" (zipWith (<>) ((if selected then "(*) " else "( ) "):repeat "    ") (wrap text))
  where
    wrap remaining
      | T.null remaining=[]
      | otherwise=let count=max 1 (columnOffset remaining (max 1 (width-4)))
                  in T.take count remaining:wrap (T.drop count remaining)

clipCells :: Int -> Text -> Text
clipCells count text=T.take (columnOffset text (max 0 count)) text


-- | Lay out Markdown as styled response-bubble characters for the chosen frontend.
renderReply :: Bool -> Int -> Bool -> Text -> [(Char,Style)]
renderReply graphical width outgoing = fst . renderReplyWithShellBlocks graphical width outgoing

renderReplyWithShellBlocks :: Bool -> Int -> Bool -> Text -> ([(Char,Style)],[(Int,Int,Text,Text)])
renderReplyWithShellBlocks graphical requested outgoing text
  | width<6 = (recolor markdown,blocks)
  | otherwise = (concat rendered,[(position blockStart,position blockEnd,dialect,body) | (blockStart,blockEnd,dialect,body)<-blocks])
  where
    width=max 1 requested
    (markdown,blocks)=renderMarkdownWithShellBlocks (if width<6 then width else width-5) text
    contentRows=splitRows (recolor markdown)
    rendered=zipWith renderLine [0::Int ..] rows
    -- Every source cell survives bubble decoration; only row prefixes change.
    position offset =
      let (row,column)=lineColumn (T.pack (map fst markdown)) offset
          renderedRow=row+if leadingCode then 1 else 0
          prefix=if outgoing then max 0 (width-bubbleWidth-3)+1 else 2
      in sum (map length (take renderedRow rendered)) + (if renderedRow>0 then 1 else 0) + prefix + column
    codeRow=any (\(_,style)->case style of BubbleText _ _ (CodeStyle _ _)->True; _->False)
    leadingCode=case contentRows of first:_->codeRow first; []->False
    trailingCode=case reverse contentRows of lastRow:_->codeRow lastRow; []->False
    rows=[[] | leadingCode]++contentRows++[[] | trailingCode]
    columns chars=let t=T.pack (map fst chars) in displayColumn t (T.length t)
    bubbleWidth=maximum (0:map columns rows)
    background=BubbleStyle outgoing Plain
    edge=TerminalStyle (if outgoing then 0x00aaaa else 0xaaaaaa) 0x0000aa 0
    recolor=map (\(c,style)->(c,BubbleText 0 outgoing style))
    spaces style n=replicate (max 0 n) (' ',style)
    tile n=(bubbleTile graphical n,edge)
    side first lastRow leftSide
      | first && lastRow = if leftSide then if outgoing then tile 4 else tile 2
                                            else if outgoing then tile 3 else tile 5
      | first = if leftSide then if outgoing then tile 0 else (' ',background)
                             else if outgoing then (' ',background) else tile 1
      | lastRow = tile (if leftSide then 2 else 3)
      | otherwise = (' ',background)
    lastIndex=length rows-1
    -- Margins belong to the bubble, never to copied text or shell-block spans.
    renderLine i chars =
      [('\n',if (leadingCode && i==1) || (trailingCode && i==lastIndex) then background else BubbleText 0 outgoing Plain) | i>0] ++ line i chars
    line i chars =
      let first=i==0; lastRow=i==lastIndex
          body=side first lastRow True:chars++spaces background (bubbleWidth-columns chars)++[side first lastRow False]
          tailCell=if first then tile (if outgoing then 7 else 6) else (' ',Plain)
      in if outgoing then spaces Plain (width-bubbleWidth-3)++body++[tailCell]
                     else tailCell:body
    splitRows chars=case break ((=='\n').fst) chars of
      (row,[]) -> [row]
      (row,_:rest) -> row:splitRows rest

publicAgentSettings :: ConversationState -> Value -> IO [AgentSetting]
publicAgentSettings runtime@(ConversationState _ ref _ _ _) value=do
  current<-readIORef ref
  keys<-conversationKeys runtime current
  let public option=not (any (\text->any (`T.isInfixOf` text) keys)
        ([settingId option,settingName option,settingCategory option,settingCurrent option]++concatMap (\(ident,label)->[ident,label]) (settingChoices option)))
  pure (filter public (parseAgentSettings value))

captureDraft :: Desktop -> IO DraftReceipt
captureDraft d=DraftReceipt (conversationTarget d) <$> captureVersion (composerBuffer d)

draftCurrent :: DraftReceipt -> Desktop -> IO Bool
draftCurrent (DraftReceipt target expected) d=case current of
  Nothing->pure False
  Just buffer->versionCurrent expected buffer
  where current=if target==conversationTarget d then Just (composerBuffer d)
                else conversationDraft <$> M.lookup target (conversationViews d)

clearSubmittedDraft :: Maybe DraftReceipt -> Desktop -> IO Desktop
clearSubmittedDraft Nothing d=pure d
clearSubmittedDraft (Just receipt@(DraftReceipt target _)) d=do
  current<-draftCurrent receipt d
  pure $ if not current then d else
    if target==conversationTarget d then d {composerBuffer=newBuffer "",composerSelection=Selection 0 0}
    else d {conversationViews=M.adjust (\view->view {conversationDraft=newBuffer "",conversationDraftSelection=Selection 0 0}) target (conversationViews d)}

parseAgentSettings :: Value -> [AgentSetting]
parseAgentSettings value=mapMaybe parseOption (fromMaybe [] (field "configOptions" value))
  where
    parseOption option=do
      ident<-field "id" option
      name<-field "name" option
      category<-field "category" option
      if category `notElem` ["model","thought_level"] then Nothing else do
        current<-field "currentValue" option
        choices<-field "options" option :: Maybe [Value]
        let values=concatMap choice choices
        if null values then Nothing else Just (AgentSetting ident name category current values)
    choice option=case (field "value" option,field "name" option) of
      (Just choiceId,Just name) -> [(choiceId,name)]
      _ -> concatMap choice (fromMaybe [] (field "options" option))

rawTranscript :: [Record] -> Text
rawTranscript=T.intercalate "\n\n" . mapMaybe (\record -> case record of Reply role text -> Just (role<>"\n"<>text); Activity ident _ history _ -> Just (ident<>"\n"<>T.intercalate "\n" (map jsonText history)); Pause _ -> Nothing)

field :: FromJSON a => Text -> Value -> Maybe a
field name=parseMaybe (withObject "object" (.: K.fromText name))
jsonText :: ToJSON a => a -> Text
jsonText=TE.decodeUtf8 . BL.toStrict . encode
failure :: Text -> Value
failure text=object ["code" .= (-32000::Int),"message" .= text]
cancelled :: Value
cancelled=object ["outcome" .= object ["outcome" .= ("cancelled"::Text)]]

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x <|> _=Just x
Nothing <|> y=y

parseTerminal :: FilePath -> Value -> Either Text (Terminal.TerminalConfig,Int)
parseTerminal root params = case parseMaybe parser params of
  Nothing -> Left "Invalid terminal command, arguments, environment or output limit."
  Just (command,args,env,cwd,limit)
    | not (isAbsolute cwd) || '\0' `elem` cwd -> Left "Expected an absolute terminal directory."
    | limit<0 || limit>16*1024*1024 -> Left "Terminal output limit must be between 0 and 16 MiB."
    | otherwise -> case validateLaunch (A.Launch command args env) of
        Left err -> Left (T.pack err)
        Right _ -> Right (Terminal.TerminalConfig command args env cwd 80 24,limit)
  where parser=withObject "terminal" $ \o -> (,,,,) <$> o .: "command" <*> o .:? "args" .!= []
          <*> (o .:? "env" .!= [] >>= mapM (withObject "environment" $ \v -> (,) <$> v .: "name" <*> v .: "value"))
          <*> o .:? "cwd" .!= root <*> o .:? "outputByteLimit" .!= (1024*1024)

exitStatus :: Int -> Value
exitStatus code=object ["exitCode" .= code,"signal" .= Null]

flushTerminalWaiters :: ConversationState -> IO ()
flushTerminalWaiters (ConversationState _ ref consoles _ _) = do
  s<-readIORef ref
  forM_ (connection s) $ \client -> forM_ (M.toList (terminalWaiters s)) $ \(tid,waiters) -> do
    result<-C.consoleOutput consoles tid
    let ready=case result of Left err -> Just (Left (failure err)); Right (_,_,Just code) -> Just (Right (exitStatus code)); _ -> Nothing
    forM_ ready $ \reply -> do
      mapM_ (\ident -> A.respond client ident reply) waiters
      modifyIORef' ref (\state -> state {terminalWaiters=M.delete tid (terminalWaiters state)})

openConsole :: C.Consoles -> Terminal.TerminalConfig -> Desktop -> IO Desktop
openConsole consoles config d = do
  result<-C.startConsole consoles config (1024*1024) d
  pure (either (\err -> message "Cannot start terminal" (wrapMessage err) d) snd result)

-- | Bind only the serialized admitted guest-input invocation. Human commands
-- have no admission; asynchronous work captures the one-shot receipt explicitly.
withBuildAdmission :: ConversationState -> AdmittedBuild -> IO a -> IO a
withBuildAdmission (ConversationState _ ref _ _ _) admission action=bracket
  (atomicModifyIORef' ref (\state->(state {buildAdmission=Just admission},buildAdmission state)))
  (\previous->modifyIORef' ref (\state->state {buildAdmission=previous}))
  (const action)

-- Capture paths and exact immutable source identities on the serialized owner.
-- Dirty representation comparisons and all filesystem/planning work run outside it.
startBuildPreparation :: ConversationState -> Maybe B.BuildAction -> Desktop -> IO Desktop
startBuildPreparation (ConversationState directory ref _ _ _) action d = mask_ $ do
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
            receipt=BuildReceipt (buildSettingsVersion state) start source (map fst captured) (buildAdmission state)
            snapshots=map snd captured
        -- Evaluate pathname selectors here; the worker never captures Desktop
        -- or FileState through an unevaluated source/directory field.
        _<-evaluate (length start)
        mapM_ (evaluate . length) source
        worker<-asyncWithUnmask (\unmask -> unmask (prepareBuild directory action start source snapshots))
        modifyIORef' ref (\current -> current {buildPreparation=Just (BuildPreparing receipt False worker)})
        pure d {status="Preparing build target…"}

buildSourceDocuments :: Desktop -> [(Int,Document)]
buildSourceDocuments d=[(bid,doc) | (bid,doc)<-M.toList (buffers d),documentLabel doc==Nothing]

prepareBuild :: FilePath -> Maybe B.BuildAction -> FilePath -> Maybe FilePath -> [DirtySnapshot] -> IO (Either Text PreparedBuild)
prepareBuild directory action start source snapshots = do
  result<-try $ do
    root<-B.resolveBuildRootFrom start
    config<-B.loadBuildConfig directory root
    unsaved<-case action of Nothing->pure False; Just _->evaluate (any snapshotDirty snapshots)
    prepared<-case action of
      Nothing -> do
        let options=buildOptions root config
        _<-evaluate (sum [T.length value | Input _ value _<-fields options])
        pure (Right (BuildOptions options))
      Just task | unsaved -> pure (Right (BuildUnsaved task))
      Just task -> fmap (BuildCommands task root) <$> B.buildPlan task config root source
    -- Strings/argv may otherwise retain planning thunks until execution on owner.
    _<-evaluate (length root+length (B.buildExecutable config)+T.length (B.buildTarget config)+
      T.length (B.buildTHCRoot config)+T.length (B.buildRuntime config)+sum (map length (B.buildArguments config)))
    case prepared of
      Left err -> evaluate (T.length err) >> pure prepared
      Right (BuildCommands _ _ commands) -> evaluate (sum [length cmd+sum (map length args) | (cmd,args)<-commands]) >> pure prepared
      _ -> pure prepared
  pure (either (Left . T.take 512 . T.pack . show) id (result :: Either IOException (Either Text PreparedBuild)))

buildReceiptCurrent :: IORef State -> BuildReceipt -> Desktop -> IO Bool
buildReceiptCurrent ref (BuildReceipt version start source expected _) d = do
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
tickBuildPreparation :: ConversationState -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO Desktop
tickBuildPreparation runtime@(ConversationState _ ref _ _ _) core d = mask_ $ do
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
    Just (BuildReady receipt@(BuildReceipt _ _ _ _ admission) launch _) -> do
      current<-if launch then pure True else buildReceiptCurrent ref receipt d
      if not current then retireBuildPreparation ref >> pure d {status="Build preparation cancelled: source or settings changed."}
      else do
        let blocked=not (isNothing (dialog d)) || questionActive d
        result<-case admission of
          Just receiptAdmission | not launch->stepAdmittedBuild receiptAdmission core d
          _ | blocked->pure Nothing
            | otherwise->Just . snd <$> core d [AdoptPreparedBuild]
        case result of
          Nothing->pure d
          Just updated->do
            remaining<-buildPreparation <$> readIORef ref
            -- A refusing gate does not see the slot. Consume that refused result;
            -- it must not launch later when the gate becomes permissive.
            case remaining of Just BuildReady{} -> retireBuildPreparation ref; _->pure ()
            pure updated

adoptBuildPreparation :: ConversationState -> Desktop -> IO Desktop
adoptBuildPreparation (ConversationState _ ref consoles jobs _) d = mask_ $ do
  state<-readIORef ref
  case buildPreparation state of
    Just (BuildReady receipt launch result) -> do
      current<-if launch then pure True else buildReceiptCurrent ref receipt d
      if not current || not (isNothing (dialog d)) || questionActive d then retireBuildPreparation ref >> pure d
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
buildSettingsChanged :: IORef State -> Toolchain -> IO ()
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
stopConversationBuild :: ConversationState -> Desktop -> IO Desktop
stopConversationBuild (ConversationState _ ref _ jobs _) d=do
  pending<-not . isNothing . buildPreparation <$> readIORef ref
  retireBuildPreparation ref
  stopped<-Jobs.stopBuildJob jobs d
  pure (if pending then stopped {status="Stopping build preparation…"} else stopped)

-- | Cheap cross-owner reservation for terminal launch only. Prepared terminals
-- remain owned until adoption or cancellation cleanup; adopted consoles retain
-- the existing independent execution policy.
buildTerminalLaunchPending :: ConversationState -> IO Bool
buildTerminalLaunchPending (ConversationState _ ref _ _ _) = do
  state<-readIORef ref
  pure $ case buildPreparation state of
    Just (BuildPreparing _ launch _) -> launch
    Just (BuildReady _ launch _) -> launch
    Just (BuildRetiring launch _) -> launch
    Nothing -> False

retireBuildPreparation :: IORef State -> IO ()
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
    cancelReceipt (BuildReceipt _ _ _ _ admission)=mapM_ cancelAdmittedBuild admission
    closeResult (Right (BuildConsole console))=C.closePreparedConsole console
    closeResult _=pure ()

-- Docs: docs/site/screenshots/build-target.png (docs/running.md) shows the target dialog.
buildOptions :: FilePath -> B.BuildConfig -> Dialog
buildOptions root config=Dialog "Build target" (AgentDialog "run-config")
  [setting "Compiler executable" (T.pack (B.buildExecutable config)),setting "Cabal target (optional)" (B.buildTarget config),
   setting "THC root (optional)" (B.buildTHCRoot config),setting "Runtime (THC only)" (B.buildRuntime config),
   ListBox "Toolchain" ["THC","GHC"] (if B.buildToolchain config==B.THC then 0 else 1),
   setting "Program arguments (JSON)" (jsonText (B.buildArguments config))]
  0 ["OK","Cancel"] ["F9 Make   Alt+F9 Compile   Ctrl+F9 Run",T.pack root]
  where setting label text=Input label text (T.length text)

conversationWidth :: Desktop -> Int
conversationWidth d = conversationWidthFor (conversationTarget d) d

conversationWidthFor :: Text -> Desktop -> Int
conversationWidthFor target d = max 1 $ case matching++available of
  w:_ -> width (bounds w)-2
  [] -> fst (screenSize d)-treeWidthOf d-4
  where
    matching=[w | Just (bid,_)<-[conversationDocument target d],w<-windows d,bufferId w==Just (bid)]
    available=[w | w<-windows d,Just doc<-[windowDocument (buffers d) w],documentLabel doc==Just "Conversation"]

conversationServices :: ConversationState -> (FilePath,C.Consoles,Jobs.BuildJobs)
conversationServices (ConversationState directory _ consoles jobs _)=(directory,consoles,jobs)


chatToolNames :: [Text]
chatToolNames=["ask_user","agent_settings"]

chatTools :: [Value]
chatTools=[object ["name" .= ("agent_settings"::Text),"description" .= ("Read provider executable, argument count, environment variable names, connection state, model/config choices and context usage. Secret-labelled values, argument values, environment values and session keys are omitted. Cannot change provider settings."::Text),
  "inputSchema" .= object ["type" .= ("object"::Text),"properties" .= object [],"additionalProperties" .= False],
  "annotations" .= object ["readOnlyHint" .= True,"destructiveHint" .= False,"openWorldHint" .= False]],
  object ["name" .= ("ask_user"::Text),"description" .= ("Create one inline human question and return questionId/status pending immediately. Continue independent work, then retrieve its status using questionId only. Answers require explicit human submission; pending replies never reveal the draft or selected choice. Only the authenticated requesting agent can retrieve results. No timeout supplies an answer or approval."::Text),
  "inputSchema" .= object ["oneOf" .=
    [object ["type" .= ("object"::Text),"required" .= ["question"::Text],"additionalProperties" .= False,
      "properties" .= object ["question" .= object ["type" .= ("string"::Text),"minLength" .= (1::Int),"maxLength" .= (4096::Int)],
        "choices" .= object ["type" .= ("array"::Text),"maxItems" .= (12::Int),"items" .= object ["type" .= ("string"::Text),"minLength" .= (1::Int),"maxLength" .= (256::Int)]],
        "allowMultiple" .= object ["type" .= ("boolean"::Text),"enum" .= [False]]]],
     object ["type" .= ("object"::Text),"required" .= ["questionId"::Text],"additionalProperties" .= False,
       "properties" .= object ["questionId" .= object ["type" .= ("integer"::Text),"minimum" .= (1::Int)]]]]],
  "annotations" .= object ["readOnlyHint" .= False,"destructiveHint" .= False,"openWorldHint" .= False]]]

-- | Initiate a conversation tool under desktop serialization. Run the returned
-- reply continuation outside the desktop lock. Anonymous callers cannot create
-- or retrieve human questions; the host must supply authenticated attribution.
chatTool :: ConversationState -> Desktop -> Text -> Value -> IO (Desktop,IO (Either Text Value))
chatTool runtime=chatToolAs runtime Nothing

-- | Capture authenticated primary attribution before queuing policy approval.
-- The transport resolves bearer credentials first; this operation checks the
-- owning actor and captures only the original provider incarnation metadata.
captureQuestionCaller :: ConversationState -> AH.AgentId -> IO (Either Text QuestionCaller)
captureQuestionCaller (ConversationState _ ref _ _ agents) actor
  | actor/=AR.primaryAgent agents=pure (Left "ask_user belongs to this editor's primary agent.")
  | otherwise=do
      active<-AH.statusAgent (AR.agentHub agents) (AH.Agent actor) actor
      s<-readIORef ref
      if questionsClosed s then pure (Left "Editor session closed.") else case active of
        Left err->pure (Left err)
        Right _->do
          receipt<-case (connection s,session s) of
            (Just client,Just sid)->Just . (`ProviderReceipt` sid) <$> (makeStableName =<< evaluate client)
            _->pure Nothing
          scope<-makeStableName =<< evaluate ref
          pure (Right (QuestionCaller scope actor receipt))

-- | Host-only authenticated caller binding. A question/result belongs to that
-- actor and runtime scope; the exact optional provider receipt also qualifies
-- polling/admission and answer delivery. This function never waits for a human.
chatToolAs :: ConversationState -> Maybe QuestionCaller -> Desktop -> Text -> Value -> IO (Desktop,IO (Either Text Value))
chatToolAs (ConversationState _ ref _ _ agents) caller d name args
  | name=="agent_settings" = if args/=object [] then pure (d,pure (Left "agent_settings accepts no arguments.")) else do
      s<-readIORef ref
      root<-if isNothing (connection s) then B.resolveBuildRoot d else pure (project s)
      context<-readAgentContexts root
      let launch=provider s
          setting option=let secret=sensitiveLabel (T.unwords [settingId option,settingName option,settingCategory option]) in object
                ["id" .= settingId option,"name" .= settingName option,"category" .= settingCategory option,
                 "current" .= (if secret then "[hidden]" else settingCurrent option),
                 "choices" .= [object ["value" .= value,"name" .= title] | (value,title)<-settingChoices option,not secret],"redacted" .= secret]
      pure (d,pure (Right (object ["executable" .= A.executable launch,"argumentCount" .= length (A.arguments launch),
        "environmentNames" .= map fst (A.environment launch),"connected" .= not (isNothing (connection s)),
        "scope" .= ("primary"::Text),"selectedAgent" .= (if T.null (conversationTarget d) then Nothing else Just (conversationTarget d)),
        "replying" .= busy s,"steering" .= agentSteering d,"contextUsage" .= agentContextUsage d,
        "settings" .= map setting (agentSettings d),"context" .= either (const Null) id context,
        "contextError" .= either Just (const (Nothing::Maybe Text)) context,"sessionKeysRedacted" .= True])))
  | name/="ask_user"=pure (d,pure (Left "Unknown chat tool."))
  | Just (QuestionCaller scope actor originalReceipt)<-caller,actor==AR.primaryAgent agents=do
      live<-AH.statusAgent (AR.agentHub agents) (AH.Agent actor) actor
      s<-readIORef ref
      owner<-(==scope) <$> (makeStableName =<< evaluate ref)
      same<-requesterCurrent originalReceipt s
      case live of
        Left err->pure (d,pure (Left err))
        Right _ | not (owner && same)->pure (d,pure (Left "Question requester session expired."))
        Right _ | questionsClosed s->pure (d,pure (Left "Editor session closed."))
        Right _->case parseEither parse args of
          Left err->pure (d,pure (Left (T.pack err)))
          Right (Left ident)->do
            result<-questionStatus actor ident s
            pure (d,pure result)
          Right (Right (question,choices))->case waitingQuestion s of
            Just _->pure (d,pure (Left "A question is already waiting for the user."))
            Nothing->do
              let token=nextApproval s
                  q=ChatQuestion token question choices Nothing (newBuffer "") (Selection 0 0) True
                  next=s {waitingQuestion=Just (QuestionTicket token actor originalReceipt),nextApproval=token+1}
                  pending=object ["questionId" .= token,"status" .= ("pending"::Text)]
              writeIORef ref next
              shown<-clearReplySelection <$> paint True next (selectConversationView "" "Primary" d) {chatQuestion=Just q,status="A question is waiting in Conversation."}
              pure (shown,pure (Right pending))
  | otherwise=pure (d,pure (Left "ask_user requires the authenticated requesting agent."))
  where
    parse=withObject "ask_user" $ \o->case KM.lookup "questionId" o of
      Just _->do
        unless (KM.keys o==["questionId"]) (fail "Retrieve a question with questionId only.")
        ident<-o .: "questionId"
        unless (ident>0) (fail "questionId must be positive.")
        pure (Left ident)
      Nothing->do
        unless (all (`elem` ["question","choices","allowMultiple"]) (KM.keys o)) (fail "Unknown question argument.")
        question<-o .: "question"
        choices<-o .:? "choices" .!= []
        multiple<-o .:? "allowMultiple" .!= False
        when multiple (fail "Only single-choice questions are supported; custom text is always available.")
        unless (not (T.null (T.strip question)) && T.length question<=4096 && not (T.any (\c->c<' ' && c `notElem` ['\n','\t']) question)) (fail "Question must contain 1..4096 characters.")
        unless (length choices<=12 && all (\text->not (T.null (T.strip text)) && T.length text<=256 && not (T.any (\c->c<' ' || c=='\DEL') text)) choices) (fail "Supply at most 12 nonempty single-line choices of at most 256 characters.")
        pure (Right (question,choices))

questionStatus :: AH.AgentId -> Int -> State -> IO (Either Text Value)
questionStatus actor ident s=case waitingQuestion s of
  Just (QuestionTicket current owner receipt) | ident==current->authorize owner receipt (object ["questionId" .= ident,"status" .= ("pending"::Text)])
  _->case M.lookup ident (questionResults s) of
    Just (QuestionResult owner receipt value)->authorize owner receipt value
    Nothing->pure (Left "Question is unknown or its retained result expired.")
  where
    authorize owner receipt value
      | actor/=owner=pure (Left "This question belongs to another requesting agent.")
      | otherwise=do
          same<-requesterCurrent receipt s
          pure $ if same then Right value else Left "Question requester session expired."

-- Keep at most 64 terminal answers, each capped at 65536 characters on submit.
-- Pending drafts remain exclusively in ChatQuestion, never in this result store.
rememberQuestion :: Int -> AH.AgentId -> Maybe ProviderReceipt -> Value -> State -> State
rememberQuestion ident actor receipt value s=s {questionResults=snd (M.splitAt (max 0 (M.size results-64)) results)}
  where results=M.insert ident (QuestionResult actor receipt value) (questionResults s)

abandonQuestion :: Text -> State -> State
abandonQuestion reason s=case waitingQuestion s of
  Nothing->s
  Just (QuestionTicket ident actor receipt)->(rememberQuestion ident actor receipt
    (object ["questionId" .= ident,"status" .= ("cancelled"::Text),"reason" .= reason]) s) {waitingQuestion=Nothing}

requesterCurrent :: Maybe ProviderReceipt -> State -> IO Bool
requesterCurrent receipt s=case receipt of
  Just target->providerCurrent target s
  Nothing->pure (isNothing (connection s) && session s==Nothing)

providerCurrent :: ProviderReceipt -> State -> IO Bool
providerCurrent (ProviderReceipt identity sid) s=case (connection s,session s) of
  (Just client,Just current) | current==sid->(==identity) <$> (makeStableName =<< evaluate client)
  _->pure False

cancelQuestion :: ConversationState -> Text -> Desktop -> IO Desktop
cancelQuestion (ConversationState _ ref _ _ _) reason d=do
  s<-readIORef ref
  case waitingQuestion s of
    Nothing->pure d
    Just _->do
      let next=abandonQuestion reason s
      writeIORef ref next
      paint False next d {chatQuestion=Nothing,chatInputOffset=Nothing,status=reason}

-- Provider workers exchange requests through the mailbox; only the editor tick
-- mutates conversation state or presents a permission dialog.
syncConversationAgent :: ConversationState -> IO ()
syncConversationAgent runtime@(ConversationState _ ref _ _ _) = do
  s<-readIORef ref
  keys<-conversationKeys runtime s
  let key=fromMaybe "" (session s)
      caps=AH.filterPrivateCapabilities keys (AH.parseCapabilities (agentInitialized s) (agentConfig s))
      externallyBusy=busy s && isNothing (agentDelivery s)
      signature=(project s,key,caps,externallyBusy)
  when (lastAgentSync s/=Just signature) $ do
    result<-AR.syncPrimary (conversationAgents runtime) (project s) key caps externallyBusy
    case result of
      Left _ -> pure () -- Invalid policy stays fail-closed and can be repaired live.
      Right () -> modifyIORef' ref (\state -> state {lastAgentSync=Just signature})

finishAgentDelivery :: IORef State -> Either Text Value -> IO ()
finishAgentDelivery ref result=do
  s<-readIORef ref
  forM_ (agentDelivery s) $ \(_,reply)->void (tryPutMVar reply result)
  modifyIORef' ref (\state -> state {agentDelivery=Nothing})

denyChild :: Approval -> IO ()
denyChild (ChildPermission _ _ reply)=void (tryPutMVar reply Nothing)
denyChild _=pure ()

drainConversationAgents :: ConversationState -> Desktop -> IO Desktop
drainConversationAgents runtime@(ConversationState _ ref _ _ agents) d=do
  requests<-AR.drainAgentRequests agents
  foldM apply d requests
  where
    apply desktop (AR.DeliverPrimary msg reply)=do
      waiting<-isEmptyMVar reply
      s<-readIORef ref
      current<-AH.statusAgent (AR.agentHub agents) AH.Human (AR.primaryAgent agents)
      let accepts=case current of Right value -> field "status" value==Just ("running"::Text); _ -> False
      if not waiting then pure desktop
      else if not accepts then void (tryPutMVar reply (Left "Agent prompt cancelled.")) >> pure desktop
      else if isNothing (connection s) || isNothing (session s) || busy s then do
        void (tryPutMVar reply (Left "The main conversation is not ready; connect it and retry."))
        pure desktop
      else do
        let author=case AH.messageAuthor msg of AH.Human -> "Human"; AH.Agent ident -> "Agent "<>AH.agentIdText ident
            attribution=if AH.messageIsUserSeat msg then "Human message" else author<>" sent a peer message, not the human user seat"
        writeIORef ref s {agentDelivery=Just (msg,reply),queuedPrompt=Just (attribution<>"\n\n"<>AH.messageText msg,Nothing),
          transcript=transcript s++[Reply author (AH.messageText msg)],reads=sourceSnapshots desktop}
        sendQueued runtime desktop
    apply desktop AR.CancelPrimary=performPrimary runtime "cancel" [] desktop
    apply desktop AR.EndPrimary=do
      cleared<-cancelQuestion runtime "Agent session ended." desktop
      s<-readIORef ref
      mapM_ denyChild (map snd (approvals s))
      _<-retireRequests ref
      mapM_ A.stopClient (connection s)
      finishAgentDelivery ref (Left "Agent session ended.")
      AR.failPendingPrimary agents "Agent session ended."
      modifyIORef' ref (\state -> state {connection=Nothing,session=Nothing,agentDelivery=Nothing,streamTails=M.empty,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],approvals=[],presented=Nothing})
      pure cleared {status="Agent session ended."}
    apply desktop (AR.AgentReconnected ident result)=do
      refreshed<-case dialog desktop of
        Just dg | purpose dg==AgentDialog "directory-select" -> showAgentDirectory runtime desktop
        _ -> pure desktop
      pure refreshed {status=either id (const ("Reconnected "<>AH.agentIdText ident<>"; no task was replayed.")) result}
    apply desktop (AR.ProviderPermission ident request reply)=do
      waiting<-isEmptyMVar reply
      when waiting (enqueueApproval runtime (ChildPermission ident request reply))
      pure desktop

showAgentDirectory :: ConversationState -> Desktop -> IO Desktop
showAgentDirectory (ConversationState _ ref _ _ agents) d=do
  result<-AH.listAgents (AR.agentHub agents) AH.Human
  case result of
    Left err -> pure d {status=err}
    Right value -> do
      let entries=fromMaybe [] (field "agents" value :: Maybe [Value])
          ids=[AH.AgentId ident | entry<-entries,Just ident<-[field "id" entry]]
          label entry=fromMaybe "Agent" (field "name" entry)<>"  "<>fromMaybe "" (field "status" entry)<>
            maybe "" ("  ← "<>) (field "parentName" entry)
      modifyIORef' ref (\s->s {directoryAgents=ids})
      pure d {dialog=Just (Dialog "Agents" (AgentDialog "directory-select")
        [ListBox "Sessions" (map label entries) 0] 0 ["Conversation","Workspace","Reconnect","Refresh","Close"]
        ["Conversation shows live messages and a human composer.","Reconnect explicitly loads a saved child without replaying work.","Workspace opens its files, terminals and debugger."])}

showAgentHistory :: ConversationState -> AH.AgentId -> Desktop -> IO Desktop
showAgentHistory runtime@(ConversationState _ ref _ _ agents) ident d=do
  selected<-AH.statusAgent (AR.agentHub agents) AH.Human ident
  case selected of
    Left err -> pure d {status=err}
    Right entry -> do
      state<-readIORef ref
      -- Remove transient question rendering before its document becomes hidden;
      -- the private answer remains in chatQuestion and returns with Primary.
      primary<-if isNothing (chatQuestion d) then pure d else paint False state d {chatQuestion=Nothing}
      withPrimary<-if isNothing (conversationDocument "" primary) then paint True state primary else pure primary
      let target=AH.agentIdText ident
          name=fromMaybe target (field "name" entry)
          selectedView=selectConversationView target name withPrimary {chatQuestion=chatQuestion d}
      modifyIORef' ref (\s->s {childRender=Nothing})
      refreshChildConversation runtime selectedView

performChild :: ConversationState -> Text -> [Text] -> Desktop -> IO Desktop
performChild runtime@(ConversationState _ ref _ _ agents) action values d=do
  state<-readIORef ref
  let target=conversationTarget d
      ident=AH.AgentId target
      hub=AR.agentHub agents
      records=M.findWithDefault [] target (childRecords state)
      text=if action=="send" then case values of _:body:_->body; _->"" else contents (composerBuffer d)
  case action of
    "send" | M.member target (childControls state) -> pure d {status="Wait for the child operation before sending."}
    "send" -> send hub ident text Nothing
    "send-draft" | M.member target (childControls state) -> pure d {status="Wait for the child operation before sending."}
    "send-draft" -> do
      receipt<-captureDraft d
      send hub ident text (Just receipt)
    "steer-draft" -> do
      receipt<-captureDraft d
      startControl (Just receipt) (fmap (fmap (const ())) (AH.steerAgent hub ident (composerMarkdown text)))
    "set-config" | [option,value]<-values -> startControl Nothing (AH.configureAgent hub ident option value)
    "cancel" -> case M.lookup target (childCancels state) of
      Just _ -> pure d {status="Cancellation requested."}
      Nothing -> do
        worker<-async (AH.cancelAgent hub AH.Human ident)
        modifyIORef' ref (\s->s {childCancels=M.insert target worker (childCancels s)})
        pure d {status="Cancellation requested."}
    "copy" -> pure d {clipboardCode=Nothing,clipboard=if M.member target (childRecords state) then rawTranscript records else maybe "" (contents.documentBuffer.snd) (conversationDocument target d),status="Conversation copied with sender attribution."}
    "toggle-activity" | [index]<-values,Just chosen<-readMaybe (T.unpack index) -> do
      let toggle (i,Activity title value history expanded) | i==chosen=Activity title value history (not expanded)
          toggle (_,record)=record
          changed=map toggle (zip [0::Int ..] records)
      modifyIORef' ref (\s->s {childRecords=M.insert target changed (childRecords s)})
      keepConversationPosition d <$> paintView target False state {transcript=changed} d
    _ -> pure d {status="Switch to Primary for provider settings or session controls; use Agents to reconnect a child."}
  where
    startControl submitted operation=startChildControl runtime (AH.AgentId (conversationTarget d)) submitted operation d
    send hub ident text receipt = do
      result<-AH.sendAgent hub AH.Human ident (composerMarkdown text)
      case result of
        Left err -> pure d {status=err}
        Right _ -> do
          cleared<-clearSubmittedDraft receipt d
          refreshChildConversation runtime cleared {status="Human message queued."}

-- Exact target is independent of the selected conversation. Worker ownership is
-- the same childControls map polled and retired by the existing conversation.
startChildControl :: ConversationState -> AH.AgentId -> Maybe DraftReceipt -> IO (Either Text ()) -> Desktop -> IO Desktop
startChildControl (ConversationState _ ref _ _ _) ident submitted operation d=mask $ \restore->do
  state<-readIORef ref
  let target=AH.agentIdText ident
  if M.member target (childControls state) then pure d {status="A child operation is already pending."} else do
    worker<-async (restore operation)
    modifyIORef' ref (\current->current {childControls=M.insert target (submitted,worker) (childControls current)})
    pure d {agentReplying=agentReplying d || target==conversationTarget d,contextMenu=Nothing,
      status=if submitted==Nothing then "Updating child settings..." else "Steering child; draft kept until accepted."}

refreshChildConversation :: ConversationState -> Desktop -> IO Desktop
refreshChildConversation (ConversationState _ ref _ _ agents) d=do
  state<-readIORef ref
  controls<-forM (M.toList (childControls state)) $ \(target,(submitted,worker))->do
    result<-poll worker
    pure (target,submitted,result)
  let controlsDone=[target | (target,_,Just _)<-controls]
      applyControl desktop (target,submitted,Just outcome)=do
        let result=either (const (Left "Child operation interrupted.")) id outcome
        cleared<-case result of Right ()->clearSubmittedDraft submitted desktop; _->pure desktop
        let notice=either id (const (if isNothing submitted then "Child settings updated." else "Follow-up added to child's active turn.")) result
        pure $ if target==conversationTarget desktop || isNothing submitted then cleared {status=notice} else cleared
      applyControl desktop _=pure desktop
  controlled<-foldM applyControl d controls
  modifyIORef' ref (\current->current {childControls=foldr M.delete (childControls current) controlsDone})
  completed<-forM (M.toList (childCancels state)) $ \(target,worker)->do
    result<-poll worker
    pure (target,result)
  let finished=[target | (target,Just _)<-completed]
      cancellation=[either (const "Child cancellation failed.") (either id (const "Child reply cancelled.")) result | (target,Just result)<-completed,target==conversationTarget d]
      original=case cancellation of text:_->controlled {status=text}; _->controlled
  modifyIORef' ref (\s->s {childCancels=foldr M.delete (childCancels s) finished})
  if T.null (conversationTarget original) then pure original else do
    let target=conversationTarget original
        hub=AR.agentHub agents
    selected<-AH.statusAgent hub AH.Human (AH.AgentId target)
    case selected of
      Left err -> pure original {status=err,agentReplying=False,agentQueued=0,childAgentSettings=[],childAgentSteering=False,childAgentContextUsage=Nothing}
      Right entry -> do
        let signature=(target,conversationWidth original,entry)
            name=fromMaybe target (field "name" entry)
            busyChild=field "status" entry `elem` [Just ("running"::Text),Just "cancelling",Just "starting",Just "configuring"]
            live=field "status" entry `elem` [Just ("idle"::Text),Just "running",Just "cancelling",Just "configuring"]
            capabilities=fromMaybe Null (field "capabilities" entry)
            usage=do value<-field "contextUsage" entry; (,) <$> field "used" value <*> field "size" value
            projected=original {childAgentSettings=if live then parseAgentSettings capabilities else [],
              childAgentSteering=live && field "steering" capabilities==Just True,childAgentContextUsage=if live then usage else Nothing,
              agentReplying=busyChild || M.member target (childControls state) && target `notElem` controlsDone,agentQueued=fromMaybe 0 (field "queued" entry),
              conversationViews=M.adjust (\v->v {conversationName=name}) target (conversationViews original)}
        current<-readIORef ref
        -- Desktop and Hub checkpoints are independent. Keep a recovered view
        -- until a live/reconnected child owns its transcript again.
        let frozen=M.notMember target (childRecords current) &&
              field "status" entry `elem` [Just ("recovered"::Text),Just "ended"] &&
              maybe False (not . T.null . contents . documentBuffer . snd) (conversationDocument target projected)
        if frozen || childRender current==Just signature then pure projected else do
          history<-recentChildHistory hub (AH.AgentId target) (fromMaybe 1 (field "nextEvent" entry))
          case history of
            Left err -> pure projected {status=err}
            Right (events,dropped) -> do
              let settings=fromMaybe [] (field "capabilities" entry >>= field "configOptions" :: Maybe [Value])
                  models=[category<>": "<>value | option<-settings,Just category<-[field "category" option],Just value<-[field "currentValue" option]]
                  trimmed=fromMaybe (0::Int) (field "nextEvent" entry)>101 || dropped
                  metadata=T.intercalate " · " ([fromMaybe "" (field "status" entry)]++
                    maybe [] (\parent->["parent: "<>parent]) (field "parentName" entry)++models++["recent history" | trimmed])
                  oldExpanded=S.fromList [ident | Activity ident _ _ True<-M.findWithDefault [] target (childRecords current)]
                  retain (Activity ident value updates _)=Activity ident value updates (S.member ident oldExpanded)
                  retain record=record
                  records=Pause metadata:map retain (foldl (childHistoryRecord name) [] events)
              modifyIORef' ref (\s->s {childRecords=M.insert target records (childRecords s),childRender=Just signature,childWidths=M.insert target (conversationWidthFor target projected) (childWidths s)})
              paintView target False current {transcript=records} projected

-- The Hub caps each page by bytes as well as count. Follow pages within the
-- captured event range so a large tool event cannot hide the newest reply.
recentChildHistory :: AH.AgentHub -> AH.AgentId -> Int -> IO (Either Text ([Value],Bool))
recentChildHistory hub ident next=go (max 0 (next-101)) [] False
  where
    go after accumulated dropped=do
      page<-AH.historyAgent hub AH.Human ident after (100-length accumulated)
      case page of
        Left err -> pure (Left err)
        Right value -> do
          let events=filter (\event->fromMaybe next (field "index" event)<next) (fromMaybe [] (field "events" value))
              combined=accumulated++events
              omitted=dropped || fromMaybe (0::Int) (field "dropped" value)>0
              cursor=fromMaybe after (field "nextAfter" value)
              more=field "hasMore" value==Just True && cursor<next-1 && length combined<100
          if more && cursor>after then go cursor combined omitted
          else pure (Right (combined,omitted || more))

childHistoryRecord :: Text -> [Record] -> Value -> [Record]
childHistoryRecord name records value=let detail=fromMaybe Null (field "detail" value) in case field "kind" value :: Maybe Text of
  Just kind | kind `elem` ["message_queued","steered"] ->
    let author=fromMaybe Null (field "author" value)
        human=field "kind" author==Just ("human"::Text)
        who=if human then "Human" else "Agent "<>fromMaybe "unknown" (field "id" author)
        seat=if field "userSeat" detail==Just True then if human then "human user seat" else "controlling parent" else "peer message"
    in records++[Reply (if human then "You" else "Peer") (who<>" ("<>seat<>")\n\n"<>fromMaybe "" (field "text" detail))]
  Just "output" -> appendChunk "Agent" (if lastRole records==Just "Agent" then chunk else name<>"\n\n"<>chunk) records
    where chunk=fromMaybe "" (field "text" detail)
  Just "thought" -> records -- Thoughts stay in the bounded history API.
  Just "tool" -> case field "toolCallId" detail :: Maybe Text of
    Just _ -> mergeTool detail records
    Nothing -> records++[Activity ("event-"<>T.pack (show (fromMaybe (length records) (field "index" value)::Int))) detail [detail] False]
  Just "message_finished" | field "status" detail/=Just ("completed"::Text) -> records++[Pause (fromMaybe "Stopped" (field "error" detail))]
  _ -> records
  where lastRole xs=case reverse xs of Reply role _:_->Just role; _->Nothing

-- Publish the next human turn before releasing the Hub ticket. Otherwise its
-- worker can dequeue another peer in the gap before the editor's next tick.
completeConversationDelivery :: ConversationState -> Either Text Value -> IO ()
completeConversationDelivery (ConversationState _ ref _ _ agents) result=do
  s<-readIORef ref
  AH.setExternalAgentBusy (AR.agentHub agents) (AR.primaryAgent agents) (busy s || not (null (queuedQueries s)))
  finishAgentDelivery ref result

pruneChildApprovals :: ConversationState -> Desktop -> IO Desktop
pruneChildApprovals (ConversationState _ ref _ _ _) d=do
  s<-readIORef ref
  live<-filterM (\(_,approval)->case approval of ChildPermission _ _ reply -> isEmptyMVar reply; _ -> pure True) (approvals s)
  let expired=maybe False (\ident -> ident `notElem` map fst live) (presented s)
  modifyIORef' ref (\state -> state {approvals=live,presented=if expired then Nothing else presented state})
  pure (if expired then dismissPermission d else d)

conversationRedactor :: ConversationState -> State -> IO (Text -> Text)
conversationRedactor runtime state=redactText <$> conversationKeys runtime state

conversationKeys :: ConversationState -> State -> IO [Text]
conversationKeys runtime state=do
  servers<-AR.primaryServers (conversationAgents runtime)
  let tokens=[value | server<-servers,entry<-fromMaybe [] (field "env" server :: Maybe [Value]),
        field "name" entry==Just ("THC_EDIT_MCP_TOKEN"::Text),Just value<-[field "value" entry]]
  pure (filter (not . T.null) (tokens++maybe [] pure (session state)))

redactText :: [Text] -> Text -> Text
redactText keys text=foldr (\key -> T.replace key "[private]") text keys

redactValue :: (Text -> Text) -> Value -> Value
redactValue redact value=case value of
  String text -> String (redact text)
  Array values -> Array (fmap (redactValue redact) values)
  Object fields -> Object (KM.fromList [(K.fromText (redact (K.toText key)),redactValue redact item) | (key,item)<-KM.toList fields])
  _ -> value

-- A matching prompt response is the only boundary that can release an
-- incomplete credential prefix. Unrelated RPC replies leave held text private.
flushConversationChunks :: IORef State -> IO ()
flushConversationChunks ref=modifyIORef' ref $ \state -> state
  {streamTails=M.empty,transcript=foldl (\records (role,text) -> if T.null text then records else appendChunk role text records)
    (transcript state) (M.toList (streamTails state))}
