{-# LANGUAGE CPP, OverloadedStrings #-}
module THC.Edit.Conversation (ConversationState, conversationServices, conversationAgents, withConversationAt, chatTools, chatToolNames, chatTool, withConversation, conversationEffects, tickConversation, parseLaunch, renderReply, pauseLabel, renderTimestamp) where

import Prelude hiding (reads)
import Control.Exception (IOException, bracket, try, onException)
#ifdef WITH_WINDOW
import Control.Concurrent (forkIO)
import System.Process (createProcess, proc, waitForProcess)
import System.Environment (getExecutablePath)
#endif
import THC.Edit.Session (SessionRecord(..))
import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryPutMVar, isEmptyMVar)
import Control.Monad (foldM, filterM, forM, forM_, void, when, unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe, parseEither)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Time (UTCTime, TimeZone, getCurrentTime, getCurrentTimeZone, diffUTCTime, utcToLocalTime, formatTime, defaultTimeLocale)
import Data.IORef
import Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (XdgDirectory(..), getXdgDirectory, createDirectoryIfMissing, canonicalizePath, getCurrentDirectory, renameFile, removeFile)
import System.FilePath ((</>), takeDirectory, isAbsolute, makeRelative, splitDirectories)
import Data.Text.Encoding.Error (lenientDecode)
import qualified THC.Edit.Terminal as Terminal
import qualified THC.Edit.Consoles as C
import qualified THC.Edit.Build as B
import qualified THC.Edit.BuildJobs as Jobs
import System.IO (openBinaryTempFile, hClose)
import Text.Read (readMaybe)
import qualified THC.Edit.ACP as A
import THC.Edit.GuestAccess (sensitiveLabel)
import THC.Edit.MCPPermissions (permissionConfigPath, projectConfigPath, readAgentContextAt, writeAgentContextAt, readAgentContexts)
import qualified THC.Edit.AgentRuntime as AR
import qualified THC.Edit.AgentHub as AH
import qualified THC.Edit.AgentACP as AP
import THC.Edit.Session (checkpointPath)
import THC.Edit.AgentFiles
import THC.Edit.Buffer
import THC.Edit.Markdown (renderMarkdown)
import THC.Edit.Model hiding (prompt)
import System.Environment (lookupEnv)
import THC.Edit.Syntax (Style(..), bubbleTile)

-- One configured stdio provider; its protocol supplies models and tools.
data Phase = Initializing (Maybe Text) | Starting (Maybe Text) | Prompting | Steering Text | Setting deriving Eq
data Record = Reply Text Text | Activity Text Value [Value] Bool | Pause Text deriving (Eq,Show)

activity :: Text -> Value -> Record
activity ident value=Activity ident value [value] False
data Approval = ChildPermission AH.AgentId AP.ACPPermission (MVar (Maybe Text)) | Permission Value [(Text,Text)] Value | Write Value Snapshot Text | Execute Value Terminal.TerminalConfig Int

data State = State
  { provider :: A.Launch, connection :: Maybe A.Client, session :: Maybe Text, project :: FilePath
  , pending :: M.Map Int Phase, queuedPrompt :: Maybe Text, transcript :: [Record]
  , reads :: M.Map FilePath Snapshot, approvals :: [(Int,Approval)], presented :: Maybe Int, deferredApproval :: Bool, nextApproval :: Int
  , queuedQueries :: [Text]
  , ownedTerminals :: S.Set Text
  , terminalWaiters :: M.Map Text [Value]
  , lastMessageAt :: Maybe UTCTime
  , lastRender :: (Int,Maybe Text,[Record]), lastSession :: Maybe (A.Launch,FilePath,Text)
  , waitingQuestion :: Maybe (Int,MVar (Either Text Value)), lastQuestion :: Maybe ChatQuestion
  , deliveredContext :: Maybe Value
  , directoryAgents :: [AH.AgentId]
  , agentDelivery :: Maybe (AH.HubMessage,MVar (Either Text Value))
  , agentInitialized :: Value, agentConfig :: Value
  , streamTails :: M.Map Text Text
  , lastAgentSync :: Maybe (FilePath,Text,AH.Capabilities,Bool)
  , childRecords :: M.Map Text [Record], childRender :: Maybe (Text,Int,Value)
  , childCancels :: M.Map Text (Async (Either Text ()))
  , resumeRecordPath :: FilePath
  }
data ConversationState = ConversationState FilePath (IORef State) C.Consoles Jobs.BuildJobs AR.AgentRuntime

conversationAgents :: ConversationState -> AR.AgentRuntime
conversationAgents (ConversationState _ _ _ _ agents)=agents

defaultLaunch :: A.Launch
defaultLaunch = A.Launch "codex-acp" [] []

withConversation :: (ConversationState -> IO a) -> IO a
withConversation action = getCurrentDirectory >>= \root -> withConversationAt root action

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
    , lastRender=(0,Nothing,[]),lastSession=remembered,waitingQuestion=Nothing,lastQuestion=Nothing
    , deliveredContext=Nothing,resumeRecordPath=resumePath,directoryAgents=[],agentDelivery=Nothing
    , agentInitialized=Null,agentConfig=Null,streamTails=M.empty,lastAgentSync=Nothing,childRecords=M.empty,childRender=Nothing,childCancels=M.empty }
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
  forM_ (waitingQuestion s) $ \(_,reply)->void (tryPutMVar reply (Left "Editor session closed."))
  finishAgentDelivery ref (Left "Editor session closed.")
  mapM_ denyChild (map snd (approvals s))
  mapM_ cancel (childCancels s)
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

conversationEffects :: ConversationState -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
conversationEffects runtime@(ConversationState _ ref _ _ _) fallback = foldM apply . (False,)
  where
    apply state@(True,_) _=pure state
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
    apply (_,d) (AgentAction action values) = do
      updated<-perform runtime action values d
      syncConversationAgent runtime
      pure (False,updated)
    apply (_,d) effect = fallback d [effect]

perform :: ConversationState -> Text -> [Text] -> Desktop -> IO Desktop
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
            let command="thc-edit --resume "<>T.pack (sessionId record)
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
      let painted=paint False next d
          keepPosition w=case (find ((==windowId w).windowId) (windows d),M.lookup (bufferId w) (buffers painted)) of
            (Just old,Just doc) | documentLabel doc==Just "Conversation" -> w {scrollRow=min (scrollRow old) (scrollbarLimit painted True doc w),scrollColumn=0,selection=Selection 0 0}
            _ -> w
      pure painted {windows=map keepPosition (windows painted)}
    ("question-choice",[token,index]) | Just ident<-readMaybe (T.unpack token),Just chosen<-readMaybe (T.unpack index),
        Just q<-chatQuestion d,questionToken q==ident,chosen>=0,chosen<length (questionChoices q) ->
      pure (clearReplySelection (paint False s d {chatQuestion=Just q {questionChoice=Just chosen,questionFocused=True}}))
    ("question-input",token:rest) | Just q<-chatQuestion d,token==T.pack (show (questionToken q)) ->
      let p=case rest of
            offset:_ | Just n<-readMaybe (T.unpack offset) -> min (bufferLength (questionBuffer q)) (questionInputStart (conversationWidth d) q+max 0 n)
            _ -> caret (questionSelection q)
      in pure (clearReplySelection (paint False s d {chatQuestion=Just q {questionChoice=Nothing,questionSelection=Selection p p,questionFocused=True}}))
    ("question-submit",[token]) | Just q<-chatQuestion d,token==T.pack (show (questionToken q)),
        Just (ident,reply)<-waitingQuestion s,ident==questionToken q -> do
      let answer=case questionChoice q of
            Just index -> fromMaybe "" (case drop index (questionChoices q) of value:_->Just value; _->Nothing)
            Nothing -> contents (questionBuffer q)
      if T.null (T.strip answer) then pure d {status="Choose an option or enter an answer."} else do
        accepted<-tryPutMVar reply (Right (object ["answer" .= answer,"choiceIndex" .= questionChoice q,"custom" .= isNothing (questionChoice q)]))
        let next=s {waitingQuestion=Nothing,transcript=transcript s++[record | accepted,record<-[Reply "Agent" (questionText q),Reply "You" answer]]}
        writeIORef ref next
        pure (paint False next d {chatQuestion=Nothing,chatInputOffset=Nothing,status=if accepted then "Answer sent." else "Question request ended; answer was not sent."})
    ("question-cancel",[token]) | Just q<-chatQuestion d,token==T.pack (show (questionToken q)) ->
      cancelQuestion runtime "Question cancelled by user." d
    ("terminal",_) -> do
      shell<-fromMaybe "/bin/sh" <$> lookupEnv "SHELL"
      root<-canonicalizePath (maybe (startingDirectory d) treeRoot (sideTree d))
      openConsole consoles (Terminal.TerminalConfig shell [] [] root 80 24) d
    ("run",_) -> runTarget directory consoles jobs (Just B.Run) d
    ("compile",_) -> runTarget directory consoles jobs (Just B.Compile) d
    ("make",_) -> runTarget directory consoles jobs (Just B.Make) d
    ("build-stop",_) -> Jobs.stopBuildJob jobs d
    ("run-options",_) -> runTarget directory consoles jobs Nothing d
    ("run-config",_:settings) -> case B.parseBuildConfig settings of
      Left err -> pure (message "Build target" [err] d)
      Right config -> do
        root<-B.resolveBuildRoot d
        result<-persist (directory </> "run.json") (B.buildConfigValue root config)
        pure d {status=either id (const "Target saved. F9 builds; Ctrl+F9 runs.") result}
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
            mapM_ A.stopClient (connection s)
            writeIORef ref s {provider=config,connection=Nothing,session=Nothing,agentDelivery=Nothing,streamTails=M.empty,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty}
            pure d {status="Agent configuration saved."}
    ("show",_) -> do
      modifyIORef' ref (\state -> state {deferredApproval=False})
      -- A recovered transcript belongs to the checkpoint until a provider
      -- connects. Opening its window must not repaint it from empty state.
      let recoveredWindow=do
            (bid,_)<-find ((==Just "Conversation") . documentLabel . snd) (M.toList (buffers d))
            find ((==bid) . bufferId) (windows d)
      pure $ case recoveredWindow of
        Just win | isNothing (connection s), null (transcript s), isNothing (chatQuestion d) ->
          focusWindow (windowId win) d {composerFocused=True}
        _ -> paint True s d
    ("set-config",[ident,value])
      | busy s -> pure d {status="Wait for the current reply before changing its model."}
      | Just client<-connection s, Just sid<-session s,
        any (\option -> settingId option==ident && value `elem` map fst (settingChoices option)) (agentSettings d) -> do
          requestId<-A.request client "session/set_config_option" (object ["sessionId" .= sid,"configId" .= ident,"value" .= value])
          modifyIORef' ref (\state -> state {pending=M.insert requestId Setting (pending state)})
          pure d {status="Updating conversation settings...",agentReplying=True}
      | otherwise -> pure d {status="This conversation setting is unavailable."}
    ("copy",_) -> pure d {clipboard=rawTranscript (transcript s),status="Raw conversation copied."}
    ("send-draft",_) | busy s, let text=contents (composerBuffer d), not (T.null (T.strip text)) -> do
      let next=s {queuedQueries=queuedQueries s++[text],transcript=transcript s++[Reply "You" text]}
      writeIORef ref next
      pure (paint True next d) {composerBuffer=newBuffer "",composerSelection=Selection 0 0,composerFocused=True,agentQueued=length (queuedQueries next),status="Query queued."}
    ("send-draft",_) | not (T.null (T.strip (contents (composerBuffer d)))) -> do
      next<-perform runtime "send" ["0",contents (composerBuffer d),"false","false","false"] d
      latest<-readIORef ref
      pure (if isNothing (connection latest) || not (busy latest) then next else next {agentReplying=busy latest,composerBuffer=newBuffer "",composerSelection=Selection 0 0,composerFocused=True})
    ("steer-draft",_) | not (agentSteering d) -> pure d {status="This provider does not advertise steering support."}
    ("steer-draft",_) | Just client<-connection s, Just sid<-session s, let text=contents (composerBuffer d), not (T.null (T.strip text)) -> do
      prepared<-preparePrompt s text
      case prepared of
        Left err -> pure d {status=err}
        Right (blocks,context) -> do
          ident<-A.request client "_session/steering" (object ["sessionId" .= sid,"prompt" .= blocks])
          writeIORef ref s {pending=M.insert ident (Steering text) (pending s),transcript=transcript s++[Reply "You" text],deliveredContext=Just context}
          pure d {composerBuffer=newBuffer "",composerSelection=Selection 0 0,composerFocused=True,status="Steering request sent."}
    ("send",_:prompt:selectionFlag:fileFlag:diagnosticFlag:_) | not (T.null (T.strip prompt)), not (busy s) -> do
      let context=contextText (selectionFlag=="true") (fileFlag=="true") (diagnosticFlag=="true") d
          full=prompt<>(if T.null context then "" else "\n\n"<>context)
          next=s {queuedPrompt=Just full,reads=sourceSnapshots d,transcript=transcript s++[Reply "You" prompt]}
      writeIORef ref next
      opened<-if isNothing (connection s) then start runtime Nothing d else sendQueued runtime d
      latest<-readIORef ref
      pure (paint True latest opened)
    ("cancel",_) -> do
      let child (_,ChildPermission{})=True
          child _=False
          retained=filter child (approvals s)
          keepDialog=maybe False (`elem` map fst retained) (presented s)
      mapM_ (C.killConsole consoles) (S.toList (ownedTerminals s))
      forM_ (connection s) $ \client -> do
        forM_ (session s) $ \sid -> A.notify client "session/cancel" (object ["sessionId" .= sid])
        mapM_ (cancelApproval client . snd) (filter (not . child) (approvals s))
      writeIORef ref s {queuedPrompt=Nothing,approvals=retained,presented=if keepDialog then presented s else Nothing,deferredApproval=False}
      pure (if keepDialog then d else dismissPermission d) {status="Cancellation requested."}
    ("new",_) | busy s -> pure d {status="Cancel the current reply before starting a new session."}
    ("new",_) -> do
      mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
      mapM_ A.stopClient (connection s)
      writeIORef ref s {connection=Nothing,session=Nothing,agentDelivery=Nothing,streamTails=M.empty,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],transcript=[],lastMessageAt=Nothing,reads=M.empty,approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty}
      start runtime Nothing d
    ("resume",_) | busy s -> pure d {status="Cancel the current reply before resuming a session."}
    ("resume",_) -> pure d {dialog=Just (Dialog "Resume conversation" (AgentDialog "load")
      [input "Session ID" (maybe "" (\(_,_,sid)->sid) (lastSession s))] 0 ["Resume","Cancel"]
      ["The provider must support loading or resuming sessions."])}
    ("load",_:sid:_) | not (T.null (T.strip sid)), not (busy s) -> do
      mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
      mapM_ A.stopClient (connection s)
      writeIORef ref s {connection=Nothing,session=Nothing,agentDelivery=Nothing,streamTails=M.empty,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],transcript=[],lastMessageAt=Nothing,reads=sourceSnapshots d,approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty}
      start runtime (Just (T.strip sid)) d
    _ | Just suffix<-T.stripPrefix "approval:" action, Just token<-readMaybe (T.unpack suffix) -> decide runtime token values d
    _ -> pure d
  where input label text=Input label text (T.length text)

busy :: State -> Bool
busy s=not (M.null (pending s)) || queuedPrompt s/=Nothing

start :: ConversationState -> Maybe Text -> Desktop -> IO Desktop
start (ConversationState _ ref _ _ _) resume d = do
  s<-readIORef ref
  let (launch,directory)=case (resume,lastSession s) of
        (Just wanted,Just (savedProvider,savedDirectory,savedId)) | wanted==savedId -> (savedProvider,savedDirectory)
        _ -> (provider s,maybe (startingDirectory d) treeRoot (sideTree d))
  result<-try $ do
    root<-canonicalizePath directory
    client<-A.startClient launch root
    ident<-A.request client "initialize" (object ["protocolVersion" .= (1::Int),"clientInfo" .= object ["name" .= ("thc-edit"::Text),"version" .= ("0.1.0.0"::Text)],
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
  | maybe False (T.null . contents) draft = restorePrimaryDraft text d
  | otherwise = d
  where draft=if T.null (conversationTarget d) then Just (composerBuffer d)
              else conversationDraft <$> M.lookup "" (conversationViews d)

sendQueued :: ConversationState -> Desktop -> IO Desktop
sendQueued (ConversationState _ ref _ _ _) d = do
  s<-readIORef ref
  case (connection s,session s,queuedPrompt s) of
    (Just client,Just sid,Just prompt) -> do
      prepared<-preparePrompt s prompt
      case prepared of
        Left err -> do
          finishAgentDelivery ref (Left err)
          modifyIORef' ref (\state -> state {queuedPrompt=Nothing})
          pure (restorePrimaryDraft prompt d) {status=err,agentReplying=False}
        Right (blocks,context) -> do
          ident<-A.request client "session/prompt" (object ["sessionId" .= sid,"prompt" .= blocks])
          writeIORef ref s {queuedPrompt=Nothing,pending=M.insert ident Prompting (pending s),deliveredContext=Just context}
          pure d {status="Agent is replying...",agentReplying=True}
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
        catalog="Editor skills: explore projects; edit/review; HLS diagnosis/rename; build/test/run; DAP debugging; Git review; desktop/hex navigation; user questions; documentation/settings. Read docs/agent-skills.md with docs_read (corpus editor) for the relevant workflow and docs/agent-tools.md for operations. Discover exact schemas with tools/list."
        extra=[guidance | deliveredContext state/=Just context]++[catalog | deliveredContext state==Nothing]
    pure (map block (query:extra),context)

tickConversation :: ConversationState -> Desktop -> IO Desktop
tickConversation runtime@(ConversationState _ ref consoles jobs _) original = do
  fresh<-pruneChildApprovals runtime original
  initial<-drainConversationAgents runtime fresh
  currentQuestion<-readIORef ref
  ready<-case waitingQuestion currentQuestion of
    Nothing -> pure initial
    Just (_,reply) -> do
      waiting<-isEmptyMVar reply
      if waiting then pure initial else cancelQuestion runtime "Question request ended." initial
  d<-C.tickConsoles consoles ready >>= Jobs.tickBuildJobs jobs
  flushTerminalWaiters runtime
  s<-readIORef ref
  events<-maybe (pure []) A.pollEvents (connection s)
  updated<-foldM (receive runtime) d events
  afterEvents<-readIORef ref
  advanced<-case queuedQueries afterEvents of
    text:rest | not (busy afterEvents), not (isNothing (connection afterEvents)), session afterEvents/=Nothing -> do
      writeIORef ref afterEvents {queuedQueries=rest,queuedPrompt=Just text,reads=sourceSnapshots updated}
      sendQueued runtime updated
    _ -> pure updated
  current<-readIORef ref
  -- Esc/Cancel of a permission dialog denies it; it must never leave the peer waiting.
  case presented current of
    Just token | not (isApprovalDialog token advanced) -> do
      forM_ (lookup token (approvals current)) $ \approval -> denyChild approval >> mapM_ (\client -> cancelApproval client approval) (connection current)
      modifyIORef' ref (\state -> state {approvals=filter ((/=token).fst) (approvals state),presented=Nothing})
    _ -> pure ()
  afterDismiss<-readIORef ref
  let widthNow=conversationWidth advanced
      -- A fresh runtime does not own the recovered transcript. Keep that view
      -- and its draft until a human connects, or a new question needs painting.
      ownsView=not (isNothing (connection afterDismiss)) || not (null (transcript afterDismiss)) || chatQuestion advanced/=Nothing || lastQuestion afterDismiss/=Nothing
      redraw=ownsView && (lastRender afterDismiss/=(widthNow,session afterDismiss,transcript afterDismiss) || lastQuestion afterDismiss/=chatQuestion advanced)
      rendered=(if redraw then paint False afterDismiss advanced else advanced) {agentReplying=busy afterDismiss,agentQueued=length (queuedQueries afterDismiss)}
  when redraw (modifyIORef' ref (\state -> state {lastRender=(widthNow,session state,transcript state),lastQuestion=chatQuestion rendered}))
  syncConversationAgent runtime
  visible<-refreshChildConversation runtime rendered
  shown<-present runtime visible
  notice<-AR.runtimeNotice (conversationAgents runtime)
  pure (maybe shown (\text -> shown {status=text}) notice)

receive :: ConversationState -> Desktop -> A.Event -> IO Desktop
receive runtime@(ConversationState _ ref consoles _ _) d event = do
  s<-readIORef ref
  case event of
    A.Disconnected reason -> do
      redact<-conversationRedactor runtime s
      finishAgentDelivery ref (Left "Agent disconnected.")
      AR.failPendingPrimary (conversationAgents runtime) "Agent disconnected."
      mapM_ denyChild (map snd (approvals s))
      mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
      writeIORef ref s {connection=Nothing,session=Nothing,agentDelivery=Nothing,streamTails=M.empty,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty,
        transcript=transcript s++[activity "Connection closed" (object ["message" .= redact reason])]}
      pure (dismissPermission d) {status="Agent disconnected.",agentSteering=False}
    A.Response ident result -> do
      writeIORef ref s {pending=M.delete ident (pending s)}
      when (M.lookup ident (pending s)==Just Prompting) (flushConversationChunks ref)
      case (M.lookup ident (pending s),result,connection s) of
        (Nothing,_,_) -> pure d
        (_,Left err,_) -> do
          redact<-conversationRedactor runtime s
          when (M.lookup ident (pending s)==Just Prompting) (completeConversationDelivery runtime (Left "Agent prompt failed."))
          modifyIORef' ref (\state -> state {queuedPrompt=Nothing,deliveredContext=Nothing,transcript=transcript state++[activity "Request failed" (redactValue redact err)]})
          let restored=case M.lookup ident (pending s) of Just (Steering text) -> restoreEmptyPrimaryDraft text d; _ -> d
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
            sendQueued runtime d {agentSettings=parseAgentSettings value,status=either ("Session opened; could not save ID: "<>) (const ("Session "<>sid)) savedId}
        (Just Setting,Right value,_) -> do
          modifyIORef' ref (\state -> state {agentConfig=value})
          pure d {agentSettings=parseAgentSettings value,contextMenu=Nothing,status="Conversation settings updated."}
        (Just (Steering text),Right value,_) -> case field "outcome" value :: Maybe Text of
          Just "injected" -> pure d {status="Follow-up added to the active turn."}
          Just "startedNewTurn" -> pure d {status="Follow-up started a new turn."}
          _ -> do
            modifyIORef' ref (\state -> state {deliveredContext=Nothing})
            pure (restoreEmptyPrimaryDraft text d) {status="Steering failed; the follow-up remains in Conversation."}
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
          pure $ if kind=="config_option_update" then d {agentSettings=parseAgentSettings update,contextMenu=Nothing}
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
      Just (path,line,limit) -> do
        captured<-captureFile (project s) path d
        case captured of
          Left err -> bad err
          Right snap -> do
            if line<1 || maybe False (<0) limit then bad "Invalid line range."
            else do
              let content=snapshotText snap
                  buffer=newBuffer content
                  startOffset=if line>bufferLineCount buffer then T.length content else bufferLineOffset buffer (line-1)
                  endOffset=case limit of
                    Nothing -> T.length content
                    Just count | count>=bufferLineCount buffer-line+1 -> T.length content
                               | otherwise -> bufferLineOffset buffer (line-1+count)
                  chosen=T.take (max 0 (endOffset-startOffset)) (T.drop startOffset content)
              modifyIORef' ref (\state -> state {reads=M.insert (snapshotPath snap) snap (reads state)})
              A.respond client ident (Right (object ["content" .= chosen])); pure d
    "fs/write_text_file" -> case (field "path" params,field "content" params) of
      (Just path,Just content) -> do
        captured<-captureFile (project s) path d
        case captured of
          Left err -> bad err
          Right current -> do
            let snapshot=M.findWithDefault current (snapshotPath current) (reads s)
            enqueueApproval runtime (Write ident snapshot content)
            pure d
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

paint :: Bool -> State -> Desktop -> Desktop
paint=paintView ""

paintView :: Text -> Bool -> State -> Desktop -> Desktop
paintView target force s original
  | not force && isNothing (conversationDocument target d) = original
  | otherwise = let
      width=conversationWidth d
      header=if T.null target then "Session: "<>fromMaybe "not connected" (session s)<>"\n" else "No messages yet.\n"
      records=zip [0..] (transcript s)
      chunks=if null records then [(plain Comment header,Nothing) | isNothing (chatQuestion d)] else renderRecords width records
      questionChunks=maybe [] (renderQuestion width (length records)) (chatQuestion d)
      allChunks=chunks++[ (plain Plain "\n\n",Nothing) | not (null chunks) && not (null questionChunks)]++questionChunks
      styled=concatMap fst allChunks
      (_,actions)=foldl (\(offset,found) (cells,action)->(offset+length cells,found++maybe [] (\(name,values)->[(offset,offset+length cells,name,values)]) action)) (0,[]) allChunks
      inputOffset=case [a+7 | (a,_,action,_)<-actions,action=="question-input"] of offset:_->Just offset; _->Nothing
      text=T.pack (map fst styled)
      questionRow=do
        q<-chatQuestion d
        if not (questionFocused q) || lastQuestion s==Just q then Nothing else do
          offset<-case questionChoice q of
            Nothing -> inputOffset
            Just index -> case [a | (a,_,action,values)<-actions,action=="question-choice",values==[T.pack (show (questionToken q)),T.pack (show index)]] of a:_->Just a; _->Nothing
          pure (fst (lineColumn text offset)+if isNothing (questionChoice q) then 1 else 0)
      existing=conversationDocument target d
      opened=case existing of
        Nothing -> let added=addConversationDocument d in added {buffers=M.adjust (\doc->restyle doc {documentBuffer=newBuffer text}) (nextId d) (buffers added)}
        Just (existingId,_) -> d {buffers=M.adjust (\doc->restyle doc {documentBuffer=newBuffer text}) existingId (buffers d)}
      bid=maybe (nextId d) fst existing
      adjust w | bufferId w/=bid = w
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
      colored=opened {conversationViews=M.insert target view {conversationBufferId=bid,conversationReplySelection=let Selection a c=conversationReplySelection view in Selection (min (T.length text) a) (min (T.length text) c)} (conversationViews opened),chatQuestion=chatQuestion original,chatActions=if visible then actions else chatActions original,chatInputOffset=if visible then inputOffset else chatInputOffset original,buffers=M.adjust (\doc -> doc {documentHighlight=styled,documentCursorVisible=False}) bid (buffers opened),windows=map adjust (windows opened)}
      focused=case find ((==bid).bufferId) (windows colored) of Just w | force && visible -> focusWindow (windowId w) colored {composerFocused=True}; _ -> colored
    in focused
  where
    d=if T.null target && T.null (conversationTarget original) then original else original {chatQuestion=Nothing}
    plain style=map (,style).T.unpack
    renderRecords _ []=[]
    renderRecords width (record:rest)=renderRecord width record++case rest of
      [] -> []
      next:_ -> [(plain Plain (if sameSpeaker (snd record) (snd next) then "\n" else "\n\n"),Nothing)]++renderRecords width rest
    sameSpeaker (Reply a _) (Reply b _)=a==b
    sameSpeaker _ _=False
    renderRecord width (recordId,record)=case record of
      Pause label -> [(renderTimestamp width label,Nothing)]
      Reply role text -> [(map (\(c,style) -> (c,case style of BubbleText _ outgoing base -> BubbleText recordId outgoing base; _ -> style)) (renderReply (videoMode d/=Nothing) width (role=="You") text),Nothing)]
      Activity ident value history expanded ->
        let title=T.unwords (T.words ("["<>fromMaybe "activity" (field "status" value)<>"] "<>fromMaybe ident (field "title" value)))
            heading=(if expanded then "▾ " else "▸ ")<>clipCells (max 1 (width-2)) title
        in [(plain Pragma heading,Just ("toggle-activity",[T.pack (show recordId)]))]++
          [(plain Plain ("\n"<>T.intercalate "\n" (map jsonText history)),Nothing) | expanded]
    renderQuestion width recordId q=
      [(map (\(c,style)->(c,case style of BubbleText _ outgoing base->BubbleText recordId outgoing base; _->style)) (renderReply (videoMode d/=Nothing) width False (questionText q)),Nothing)]++
      concat [[(plain Plain "\n",Nothing),(plain (if questionChoice q==Just index then Keyword else Plain)
        (choiceLines width (questionChoice q==Just index) text),Just ("question-choice",[token,T.pack (show index)]))] | (index,text)<-zip [0::Int ..] (questionChoices q)]++
      [(plain Plain "\n",Nothing),(plain (if isNothing (questionChoice q) then Literal else Plain)
        ("Other: "<>questionVisibleInput width q<>" "),Just ("question-input",[token])),
       (plain Plain "\n",Nothing),(plain Keyword "[Submit answer]",Just ("question-submit",[token])),
       (plain Plain "  ",Nothing),(plain Comment "[Cancel]",Just ("question-cancel",[token]))]
      where token=T.pack (show (questionToken q))

choiceLines :: Int -> Bool -> Text -> Text
choiceLines width selected text=T.intercalate "\n" (zipWith (<>) ((if selected then "(*) " else "( ) "):repeat "    ") (wrap text))
  where
    wrap remaining
      | T.null remaining=[]
      | otherwise=let count=max 1 (columnOffset remaining (max 1 (width-4)))
                  in T.take count remaining:wrap (T.drop count remaining)

clipCells :: Int -> Text -> Text
clipCells count text=T.take (columnOffset text (max 0 count)) text


-- Corner cells share the text rows. The spiked corner stays square, so the
-- top edge runs continuously into the tail. Terminal fonts use block fallbacks.
renderReply :: Bool -> Int -> Bool -> Text -> [(Char,Style)]
renderReply graphical requested outgoing text
  | width<6 = recolor (renderMarkdown width text)
  | otherwise = concat (zipWith renderLine [0::Int ..] rows)
  where
    width=max 1 requested
    contentRows=splitRows (recolor (renderMarkdown (width-5) text))
    leadingCode=case contentRows of
      first:_ -> any (\(_,style)->case style of BubbleText _ _ (CodeStyle _ _)->True; _->False) first
      [] -> False
    rows=if leadingCode then []:contentRows else contentRows
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
    -- The top margin belongs to the bubble, so copying still starts at its text.
    renderLine i chars =
      [('\n',if leadingCode && i==1 then background else BubbleText 0 outgoing Plain) | i>0] ++ line i chars
    line i chars =
      let first=i==0; lastRow=i==lastIndex
          body=side first lastRow True:chars++spaces background (bubbleWidth-columns chars)++[side first lastRow False]
          tailCell=if first then tile (if outgoing then 7 else 6) else (' ',Plain)
      in if outgoing then spaces Plain (width-bubbleWidth-3)++body++[tailCell]
                     else tailCell:body
    splitRows chars=case break ((=='\n').fst) chars of
      (row,[]) -> [row]
      (row,_:rest) -> row:splitRows rest

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

runTarget :: FilePath -> C.Consoles -> Jobs.BuildJobs -> Maybe B.BuildAction -> Desktop -> IO Desktop
runTarget directory consoles jobs action d = do
  root<-B.resolveBuildRoot d
  config<-B.loadBuildConfig directory root
  let setting label text=Input label text (T.length text)
      unsaved=any (\doc -> documentLabel doc==Nothing && dirty (documentBuffer doc)) (M.elems (buffers d))
      source=B.buildSource d
  case action of
    Nothing -> pure d {dialog=Just (Dialog "Build target" (AgentDialog "run-config")
      [setting "Compiler executable" (T.pack (B.buildExecutable config)),setting "Cabal target (optional)" (B.buildTarget config),
       setting "THC root (optional)" (B.buildTHCRoot config),setting "Runtime (THC only)" (B.buildRuntime config),
       ListBox "Toolchain" ["THC","GHC"] (if B.buildToolchain config==B.THC then 0 else 1),
       setting "Program arguments (JSON)" (jsonText (B.buildArguments config))]
      0 ["OK","Cancel"] ["F9 Make   Alt+F9 Compile   Ctrl+F9 Run",T.pack root])}
    Just task | unsaved -> pure (message (if task==B.Run then "Save before running" else "Save before building")
      ["Save modified source files before building the files on disk."] d)
    Just task -> do
      plan<-B.buildPlan task config root source
      case plan of
        Left err -> pure (message "Build target" [err] d)
        Right [(command,args)] | task==B.Run && Terminal.terminalAvailable ->
          openConsole consoles (Terminal.TerminalConfig command args [] root 80 24) d
        Right commands -> Jobs.startBuildJob jobs (T.pack (show task)) root commands d

conversationWidth :: Desktop -> Int
conversationWidth d = max 1 $ case [width (bounds w)-2 | w<-windows d,Just doc<-[M.lookup (bufferId w) (buffers d)],documentLabel doc==Just "Conversation"] of
  size:_ -> size
  [] -> fst (screenSize d)-treeWidthOf d-4

conversationServices :: ConversationState -> (FilePath,C.Consoles,Jobs.BuildJobs)
conversationServices (ConversationState directory _ consoles jobs _)=(directory,consoles,jobs)


chatToolNames :: [Text]
chatToolNames=["ask_user","agent_settings"]

chatTools :: [Value]
chatTools=[object ["name" .= ("agent_settings"::Text),"description" .= ("Read provider executable, argument count, environment variable names, connection state, model/config choices and context usage. Secret-labelled values, argument values, environment values and session keys are omitted. Cannot change provider settings."::Text),
  "inputSchema" .= object ["type" .= ("object"::Text),"properties" .= object [],"additionalProperties" .= False],
  "annotations" .= object ["readOnlyHint" .= True,"destructiveHint" .= False,"openWorldHint" .= False]],
  object ["name" .= ("ask_user"::Text),"description" .= ("Ask one inline question in the editor conversation. Supply optional single-choice answers; a custom text answer is always available. Waits for the human without a time limit. Only one question may be pending."::Text),
  "inputSchema" .= object ["type" .= ("object"::Text),"required" .= ["question"::Text],"additionalProperties" .= False,
    "properties" .= object ["question" .= object ["type" .= ("string"::Text),"minLength" .= (1::Int),"maxLength" .= (4096::Int)],
      "choices" .= object ["type" .= ("array"::Text),"maxItems" .= (12::Int),"items" .= object ["type" .= ("string"::Text),"minLength" .= (1::Int),"maxLength" .= (256::Int)]],
      "allowMultiple" .= object ["type" .= ("boolean"::Text),"enum" .= [False]]]],
  "annotations" .= object ["readOnlyHint" .= False,"destructiveHint" .= False,"openWorldHint" .= False]]]

chatTool :: ConversationState -> Desktop -> Text -> Value -> IO (Desktop,IO (Either Text Value))
chatTool (ConversationState _ ref _ _ _) d name args
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
  | otherwise=case parseEither parse args of
      Left err -> pure (d,pure (Left (T.pack err)))
      Right (question,choices) -> do
        s<-readIORef ref
        case waitingQuestion s of
          Just _ -> pure (d,pure (Left "A question is already waiting for the user."))
          Nothing -> do
            reply<-newEmptyMVar
            let token=nextApproval s
                q=ChatQuestion token question choices Nothing (newBuffer "") (Selection 0 0) True
                next=s {waitingQuestion=Just (token,reply),nextApproval=token+1}
            writeIORef ref next
            let shown=clearReplySelection (paint True next (selectConversationView "" "Primary" d) {chatQuestion=Just q,status="A question is waiting in Conversation."})
            pure (shown,readMVar reply `onException` void (tryPutMVar reply (Left "Question requester disconnected.")))
  where
    parse=withObject "ask_user" $ \o->do
      unless (all (`elem` ["question","choices","allowMultiple"]) (KM.keys o)) (fail "Unknown question argument.")
      question<-o .: "question"
      choices<-o .:? "choices" .!= []
      multiple<-o .:? "allowMultiple" .!= False
      when multiple (fail "Only single-choice questions are supported; custom text is always available.")
      unless (not (T.null (T.strip question)) && T.length question<=4096 && not (T.any (\c->c<' ' && c `notElem` ['\n','\t']) question)) (fail "Question must contain 1..4096 characters.")
      unless (length choices<=12 && all (\text->not (T.null (T.strip text)) && T.length text<=256 && not (T.any (\c->c<' ' || c=='\DEL') text)) choices) (fail "Supply at most 12 nonempty single-line choices of at most 256 characters.")
      pure (question,choices)

cancelQuestion :: ConversationState -> Text -> Desktop -> IO Desktop
cancelQuestion (ConversationState _ ref _ _ _) reason d=do
  s<-readIORef ref
  case waitingQuestion s of
    Nothing -> pure d
    Just (_,reply) -> do
      void (tryPutMVar reply (Left reason))
      let next=s {waitingQuestion=Nothing}
      writeIORef ref next
      pure (paint False next d {chatQuestion=Nothing,chatInputOffset=Nothing,status=reason})

-- Provider workers exchange requests through the mailbox; only the editor tick
-- mutates conversation state or presents a permission dialog.
syncConversationAgent :: ConversationState -> IO ()
syncConversationAgent runtime@(ConversationState _ ref _ _ _) = do
  s<-readIORef ref
  let key=fromMaybe "" (session s)
      caps=AH.parseCapabilities (agentInitialized s) (agentConfig s)
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
        writeIORef ref s {agentDelivery=Just (msg,reply),queuedPrompt=Just (attribution<>"\n\n"<>AH.messageText msg),
          transcript=transcript s++[Reply author (AH.messageText msg)],reads=sourceSnapshots desktop}
        sendQueued runtime desktop
    apply desktop AR.CancelPrimary=performPrimary runtime "cancel" [] desktop
    apply desktop AR.EndPrimary=do
      s<-readIORef ref
      mapM_ denyChild (map snd (approvals s))
      mapM_ A.stopClient (connection s)
      finishAgentDelivery ref (Left "Agent session ended.")
      AR.failPendingPrimary agents "Agent session ended."
      modifyIORef' ref (\state -> state {connection=Nothing,session=Nothing,agentDelivery=Nothing,streamTails=M.empty,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],approvals=[],presented=Nothing})
      pure desktop {status="Agent session ended."}
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
      let primary=if isNothing (chatQuestion d) then d else paint False state d {chatQuestion=Nothing}
          withPrimary=if isNothing (conversationDocument "" primary) then paint True state primary else primary
          target=AH.agentIdText ident
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
    "send" -> send hub ident text
    "send-draft" -> send hub ident text
    "steer-draft" -> pure d {status="Child steering is unavailable. Enter queues a human message."}
    "cancel" -> case M.lookup target (childCancels state) of
      Just _ -> pure d {status="Cancellation requested."}
      Nothing -> do
        worker<-async (AH.cancelAgent hub AH.Human ident)
        modifyIORef' ref (\s->s {childCancels=M.insert target worker (childCancels s)})
        pure d {status="Cancellation requested."}
    "copy" -> pure d {clipboard=if M.member target (childRecords state) then rawTranscript records else maybe "" (contents.documentBuffer.snd) (conversationDocument target d),status="Conversation copied with sender attribution."}
    "toggle-activity" | [index]<-values,Just chosen<-readMaybe (T.unpack index) -> do
      let toggle (i,Activity title value history expanded) | i==chosen=Activity title value history (not expanded)
          toggle (_,record)=record
          changed=map toggle (zip [0::Int ..] records)
      modifyIORef' ref (\s->s {childRecords=M.insert target changed (childRecords s)})
      pure (paintView target False state {transcript=changed} d)
    _ -> pure d {status="Switch to Primary for provider settings or session controls; use Agents to reconnect a child."}
  where
    send hub ident text = do
      result<-AH.sendAgent hub AH.Human ident text
      case result of
        Left err -> pure d {status=err}
        Right _ -> refreshChildConversation runtime d {composerBuffer=newBuffer "",composerSelection=Selection 0 0,status="Human message queued."}

refreshChildConversation :: ConversationState -> Desktop -> IO Desktop
refreshChildConversation (ConversationState _ ref _ _ agents) d=do
  state<-readIORef ref
  completed<-forM (M.toList (childCancels state)) $ \(target,worker)->do
    result<-poll worker
    pure (target,result)
  let finished=[target | (target,Just _)<-completed]
      cancellation=[either (const "Child cancellation failed.") (either id (const "Child reply cancelled.")) result | (target,Just result)<-completed,target==conversationTarget d]
      original=case cancellation of text:_->d {status=text}; _->d
  modifyIORef' ref (\s->s {childCancels=foldr M.delete (childCancels s) finished})
  if T.null (conversationTarget original) then pure original else do
    let target=conversationTarget original
        hub=AR.agentHub agents
    selected<-AH.statusAgent hub AH.Human (AH.AgentId target)
    case selected of
      Left err -> pure original {status=err,agentReplying=False,agentQueued=0}
      Right entry -> do
        let signature=(target,conversationWidth original,entry)
            name=fromMaybe target (field "name" entry)
            busyChild=field "status" entry `elem` [Just ("running"::Text),Just "cancelling",Just "starting"]
            projected=original {agentReplying=busyChild,agentQueued=fromMaybe 0 (field "queued" entry),
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
                  records=Pause metadata:foldl (childHistoryRecord name) [] events
              modifyIORef' ref (\s->s {childRecords=M.insert target records (childRecords s),childRender=Just signature})
              pure (paintView target False current {transcript=records} projected)

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
  Just "message_queued" ->
    let author=fromMaybe Null (field "author" value)
        human=field "kind" author==Just ("human"::Text)
        who=if human then "Human" else "Agent "<>fromMaybe "unknown" (field "id" author)
        seat=if field "userSeat" detail==Just True then if human then "human user seat" else "controlling parent" else "peer message"
    in records++[Reply (if human then "You" else "Peer") (who<>" ("<>seat<>")\n\n"<>fromMaybe "" (field "text" detail))]
  Just "output" -> appendChunk "Agent" (if lastRole records==Just "Agent" then chunk else name<>"\n\n"<>chunk) records
    where chunk=fromMaybe "" (field "text" detail)
  Just "thought" -> records -- Thoughts stay in the bounded history API.
  Just "tool" -> records++[Activity (fromMaybe "Tool call" (field "title" detail)) detail [detail] False]
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
