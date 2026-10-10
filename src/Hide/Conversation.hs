-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE CPP, OverloadedStrings #-}
-- | Module      : Hide.Conversation
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : CPP, OverloadedStrings
--
-- Primary ACP ownership and captured plugin transcript sources.
--
-- The session tick consumes protocol and hub mailbox events. Owned workers prepare
-- provider acquisition, prompt context, file captures and consoles before adoption; capture itself does
-- not authorize an action. Prepared results must still match source identity and
-- privacy policy. Child cancellation/configuration/steering completes through ticks.
--
-- Completed replies belong to their exact prompt, independently of transcript
-- layout or trailing tool activity. Safe chunks are retained only for that turn.
--
-- Shared consoles are injected; retirement closes only ACP-owned terminal IDs.
-- Tool initiation returns a desktop plus a continuation, so questions wait outside the
-- desktop lock while the rest of the session continues.
module Hide.Conversation (ConversationState, conversationAgents, captureAgentSettings, captureConversationSession, captureConversationChoices, withConversationAt, QuestionCaller, captureQuestionCaller, questionServices, applyQuestion, withConversation, conversationEffects, tickConversation, conversationBodyRequests, adoptConversationBodies, parseLaunch, renderReply, pauseLabel, renderTimestamp) where

import Hide.Sidebar
import Hide.ConversationBody
import Hide.SessionServices (persist)
import Prelude hiding (reads)
import Control.DeepSeq (force)
import qualified Hide.Plugin.AgentSettings as Settings
import Control.Exception (IOException, bracket, try, onException, mask, mask_, evaluate, finally)
#ifdef WITH_WINDOW
import Control.Concurrent (forkIO)
import System.Process (createProcess, proc, waitForProcess)
import System.Environment (getExecutablePath)
#endif
import Hide.Session (SessionRecord(..))
import Control.Concurrent.Async (Async, async, asyncWithUnmask, cancel, poll, wait)
import Control.Concurrent.STM (atomically)
import qualified Hide.Plugin.Menu as Plugin
import Control.Concurrent.MVar (MVar, tryPutMVar, isEmptyMVar, newMVar, withMVar, modifyMVar_)
import Control.Monad (foldM, filterM, forM, forM_, void, when, unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Time (UTCTime, TimeZone, getCurrentTime, getCurrentTimeZone, diffUTCTime, utcToLocalTime, formatTime, defaultTimeLocale)
import Data.IORef
import Data.Unique (Unique,newUnique)
import Hide.ConversationSessionTypes
import qualified Hide.Plugin.ConversationSession as Session
import Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (XdgDirectory(..), getXdgDirectory, canonicalizePath, getCurrentDirectory)
import System.FilePath ((</>), isAbsolute, makeRelative, splitDirectories)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Hide.Terminal as Terminal
import qualified Hide.Consoles as C
import qualified Hide.Build as B
import System.Mem.StableName (StableName, makeStableName)
import Text.Read (readMaybe)
import qualified Hide.ACP as A
import Hide.GuestAccess (sensitiveLabel, protectedPath, protectedBuffer)
import Hide.Files (filePath)
import Hide.MCPPermissions (permissionConfigPath, projectConfigPath, readAgentContextAt, writeAgentContextAt, readAgentContexts)
import qualified Hide.AgentRuntime as AR
import qualified Hide.AgentHub as AH
import qualified Hide.AgentACP as AP
import Hide.Plugin.Transcript (ConversationPresenter,AgentHistory(..),PrimaryContent(..),PrimarySpeaker(..),PrimaryUpdate(..))
import qualified Hide.Plugin.Transcript as Transcript
import Hide.Session (checkpointPath)
import Hide.AgentFiles
import Hide.Buffer
import Hide.Plugin.BufferHost (versionCurrent)
import Hide.AgentSidebarTypes
import Hide.Model hiding (prompt)
import qualified Hide.Plugin.EditorHost as Editor
import qualified Hide.Plugin.Command as Command
import Hide.Plugin.Input (InputDeclaration)
import qualified Hide.Plugin.Questions as Questions
import Hide.MCPPermissions (Permissions,requestPermission)
import Hide.Plugin.ConversationInput (PrimaryInputServices(..), ChildInputServices(..))
import Hide.PluginWindowHost (installEditorDraft,applyEditorUpdate,adoptWindowUpdate)
import qualified Hide.Plugin.Window as W
import qualified Data.Vector as V
import System.Environment (lookupEnv)
import Hide.Syntax (Style(..),styledText)

-- One configured stdio provider; its protocol supplies models and tools.
data Phase = Initializing (Maybe Text) | Starting (Maybe Text) | Prompting | CancellingPrompt | Steering Text AR.PrimaryControl | Setting AR.PrimaryControl deriving Eq
-- A human submission can consume only its captured immutable draft. Stable
-- names in ContentVersion retain no Buffer/Undo; selection/focus are independent.
type DraftReceipt = Editor.DraftSubmission
-- Immutable input context captured at the original human input turn.
data ChatEditorContext = ChatEditorContext !Text !(StableName A.Launch) !(Maybe ProviderReceipt) !(Maybe AH.AgentConfigRef)
-- Both input commands execute on the existing control worker. Their distinct
-- services preserve primary context/connect ownership and child Hub admission.
data ConversationEditor
  = PrimaryEditor !(Editor.PreparedEditor PrimaryInputServices Editor.EditorUpdate)
  | ChildEditor !(Editor.PreparedEditor ChildInputServices Editor.EditorUpdate)

conversationEditorMount :: ConversationEditor -> Editor.EditorMount
conversationEditorMount (PrimaryEditor editor)=Editor.editorMount editor
conversationEditorMount (ChildEditor editor)=Editor.editorMount editor

data AgentControlResult = AgentControlAccepted | ConversationInputAccepted !Editor.EditorUpdate
data PromptPreparation
  = StartingClient !(Maybe Text) !(Async (FilePath,A.Client,Int))
  | ContextPrompt !Bool !Text !(Maybe AR.PrimaryControl) !(Async (Either Text ([Value],Value)))
preparationCancel :: PromptPreparation -> IO ()
preparationCancel (StartingClient _ worker)=do
  cancel worker
  -- Cancellation can lose to successful acquisition. Until a tick adopts the
  -- result this owner must dispose it, including during session shutdown.
  completed<-poll worker
  case completed of
    Just (Right (_,client,_))->A.stopClient client
    _->pure ()
preparationCancel (ContextPrompt _ _ _ worker)=cancel worker


unavailableConversation :: Text
unavailableConversation="Agent conversation plugin is unavailable; draft kept."

toggleExpansion :: Text -> ToolExpansion -> State -> State
toggleExpansion target item state=state {toolExpansions=if S.member key expanded then S.delete key expanded else S.insert key expanded}
  where key=(target,item); expanded=toolExpansions state
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
  | EditorQuery !Editor.DraftSubmission !ChatEditorContext !ConversationEditor
data QuestionTicket = QuestionTicket !Int !AH.AgentId !(Maybe ProviderReceipt)
data QuestionResult = QuestionResult !AH.AgentId !(Maybe ProviderReceipt) !Value

-- Reversed safe chunks give O(1) append and one concatenation on completion.
-- The request ID scopes output to the provider prompt, never a displayed record.
data PromptReply = PromptReply !Int [Text]

data SettingsCapture = SettingsCapture !Bool !FilePath !Settings.SettingsSnapshot

data State = State
  { sessionIdentity :: !Unique
  , provider :: A.Launch, connection :: Maybe A.Client, session :: Maybe Text, project :: FilePath
  , pending :: M.Map Int Phase, queuedPrompt :: Maybe (Text,Maybe AR.PrimaryControl), transcript :: !Transcript.PrimaryTranscript, nextRecord :: !Int
  , reads :: M.Map FilePath Snapshot, approvals :: [(Int,Approval)], presented :: Maybe Int, deferredApproval :: Bool, nextApproval :: Int
  , queuedQueries :: [QueuedQuery]
  , ownedTerminals :: S.Set Text
  , terminalWaiters :: M.Map Text [Value]
  , lastMessageAt :: Maybe UTCTime
  , lastSession :: Maybe (A.Launch,FilePath,Text)
  , waitingQuestion :: Maybe QuestionTicket, questionResults :: M.Map Int QuestionResult, questionsClosed :: Bool, lastQuestion :: Maybe Int, lastQuestionInteraction :: Maybe (StableName ChatQuestion)
  , deliveredContext :: Maybe Value
  , directoryAgents :: [AH.AgentId]
  , agentInitialized :: Value, agentConfig :: Value
  , streamTails :: M.Map Text Text, promptReply :: Maybe PromptReply
  , lastAgentSync :: Maybe (FilePath,Maybe (StableName A.Client),Text,AH.Capabilities,Bool)
  , conversationPresenter :: Maybe ConversationPresenter, childRecords :: M.Map Text TranscriptSource, childRender :: Maybe (Text,Value)
  , toolExpansions :: S.Set (Text,ToolExpansion)
  , agentControls :: M.Map Text (Maybe DraftReceipt,Async (Either Text AgentControlResult))
  , childCancels :: M.Map Text (Async (Either Text ()))
  , fileCaptures :: [FileCapture], retiringRequests :: [(Int,Async ())]
  , promptPreparation :: Maybe PromptPreparation
  , primaryInput :: Maybe (Editor.DeclaredInput PrimaryInputServices Editor.EditorUpdate)
  , childInput :: Maybe (Editor.DeclaredInput ChildInputServices Editor.EditorUpdate)
  , conversationEditors :: IORef (M.Map Text ConversationEditor)
  , bodyScope :: !W.WindowScope, loadingBody :: !W.PreparedWindow
  , settingsCommands :: !(Command.Registry SettingsCapture, Command.Command SettingsCapture () Settings.SettingsSnapshot)
  , resumeRecordPath :: FilePath
  }
-- | Provider/transcript ownership with injected session consoles.
data ConversationState = ConversationState FilePath (IORef State) C.Consoles AR.AgentRuntime

conversationAgents :: ConversationState -> AR.AgentRuntime
conversationAgents (ConversationState _ _ _ agents)=agents

defaultLaunch :: A.Launch
defaultLaunch = A.Launch "codex-acp" [] []

withConversation :: Maybe ConversationPresenter -> Maybe (InputDeclaration PrimaryInputServices) -> Maybe (InputDeclaration ChildInputServices) -> C.Consoles -> (ConversationState -> IO a) -> IO a
withConversation presenter primary child consoles action = getCurrentDirectory >>= \root -> withConversationAt presenter primary child consoles root action

-- | Load conversation configuration and scope only provider and agent workers.
withConversationAt :: Maybe ConversationPresenter -> Maybe (InputDeclaration PrimaryInputServices) -> Maybe (InputDeclaration ChildInputServices) -> C.Consoles -> FilePath -> (ConversationState -> IO a) -> IO a
withConversationAt presenter primary child consoles root action = W.withWindowScope $ \scope->Command.withRegistry $ \registry->Command.withRegistry $ \childRegistry->Command.withRegistry $ \settingsRegistry->do
  settingsRead<-Command.registerCommand settingsRegistry (Command.CommandDef "hide.agent-settings.read" "Read public conversation settings"
    (Command.Codec Null (const (Right ())) (const Null))
    (Command.Codec Null (const (Left "Host-captured settings only.")) toJSON)
    (\(SettingsCapture connected directory snapshot) ()->do
      base<-if connected then pure directory else B.resolveBuildRootFrom directory
      context<-readAgentContexts base
      pure (Right snapshot {Settings.settingsContext=either (const Null) id context,
        Settings.settingsContextError=either Just (const Nothing) context}))) >>= either (ioError . userError . show) pure
  preparedPresenter<-traverse evaluate presenter
  registeredPrimaryInput<-traverse (\declaration->Editor.registerDeclaredInput registry declaration id >>= either (ioError . userError . show) pure) primary
  registeredChildInput<-traverse (\declaration->Editor.registerDeclaredInput childRegistry declaration id >>= either (ioError . userError . show) pure) child
  loading<-W.prepareSemanticTextWindow "Conversation" (styledText Comment "Preparing conversation…")
    (W.TextSemantics (W.CopyMessages W.UserBotAttribution) (Just root) V.empty V.empty W.ReadableWindow V.empty V.empty V.empty) >>= either (ioError . userError . T.unpack) pure
  directory<-getXdgDirectory XdgConfig "thc-edit"
  loaded<-try (BS.readFile (directory </> "agents.json")) :: IO (Either IOException BS.ByteString)
  let launch=either (const defaultLaunch) (either (const defaultLaunch) id . decodeLaunch) loaded
  resumePath<-conversationSessionPath directory
  previous<-try (BS.readFile resumePath) :: IO (Either IOException BS.ByteString)
  let remembered=either (const Nothing) (\bytes -> decodeStrict' bytes >>= parseMaybe (withObject "session" $ \o -> do
        (raw::Value)<-o .: "provider"; config<-either fail pure (decodeLaunch (BL.toStrict (encode raw)))
        (,,) config <$> o .: "cwd" <*> o .: "sessionId")) previous
  editors<-newIORef M.empty
  identity<-newUnique
  ref<-newIORef State
    { sessionIdentity=identity,provider=launch,connection=Nothing,session=Nothing,project=root
    , pending=M.empty,queuedPrompt=Nothing,transcript=Transcript.emptyPrimaryTranscript,nextRecord=0,reads=M.empty
    , approvals=[],presented=Nothing,deferredApproval=False,nextApproval=1,queuedQueries=[]
    , ownedTerminals=S.empty,terminalWaiters=M.empty,lastMessageAt=Nothing
    , lastSession=remembered,waitingQuestion=Nothing,questionResults=M.empty,questionsClosed=False,lastQuestion=Nothing,lastQuestionInteraction=Nothing
    , deliveredContext=Nothing,fileCaptures=[],retiringRequests=[],promptPreparation=Nothing,settingsCommands=(settingsRegistry,settingsRead),resumeRecordPath=resumePath,directoryAgents=[]
    , conversationPresenter=preparedPresenter,agentInitialized=Null,agentConfig=Null,streamTails=M.empty,promptReply=Nothing,lastAgentSync=Nothing,childRecords=M.empty,childRender=Nothing,childCancels=M.empty,agentControls=M.empty,toolExpansions=S.empty,primaryInput=registeredPrimaryInput,childInput=registeredChildInput,conversationEditors=editors,bodyScope=scope,loadingBody=loading }
  AR.withAgentRuntime root (provider <$> readIORef ref) $ \agents ->
    bracket (pure (ConversationState directory ref consoles agents)) closeConversation action

-- Provider configuration remains global, but a recovered editor must resume
-- its own conversation. Standalone/legacy callers retain their existing file.
conversationSessionPath :: FilePath -> IO FilePath
conversationSessionPath directory=lookupEnv "THC_EDIT_SESSION" >>= maybe
  (pure (directory </> "agent-session.json"))
  (fmap (++".agent.json") . checkpointPath)

closeConversation :: ConversationState -> IO ()
closeConversation (ConversationState _ ref consoles agents) = do
  s<-readIORef ref
  let (registry,command)=settingsCommands s
  void (Command.retireCommand registry (Command.commandRef command))
  writeIORef ref (abandonQuestion "Editor session closed." s) {questionsClosed=True,promptReply=Nothing}
  AR.failPendingPrimary agents "Editor session closed."
  mapM_ denyChild (map snd (approvals s))
  mapM_ preparationCancel (promptPreparation s)
  readIORef (conversationEditors s) >>= mapM_ (Editor.retireDraftRef . Editor.mountDraft . conversationEditorMount)
  mapM_ (\(FileCapture _ worker) -> cancel worker) (fileCaptures s)
  mapM_ (wait . snd) (retiringRequests s)
  mapM_ cancel (childCancels s)
  mapM_ (cancel . snd) (agentControls s)
  mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
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

-- | Consume conversation effects and delegate unrelated effects to the next interpreter.
conversationEffects :: ConversationState -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
conversationEffects runtime@(ConversationState _ ref _ _) fallback original effects = do
  (quit,updated)<-foldM apply (False,original) effects
  shown<-if quit then pure updated else revealQuestion runtime updated
  captured<-captureConversationSources runtime shown
  pure (quit,captured)
  where
    apply state@(True,_) _=pure state
    apply (_,d) effect@(SubmitEditor mount slot origin)=do
      state<-readIORef ref
      editors<-readIORef (conversationEditors state)
      let owned=any ((==mount).conversationEditorMount) (M.elems editors)
      if owned then (False,) <$> submitConversationEditor runtime mount slot origin d else fallback d [effect]
    apply (_,d) (AgentSidebarAction request) = (False,) <$> applyAgentSidebar runtime request d
    apply (_,d) (ConversationSessionAction request)=do
      updated<-applyConversationSession runtime request d
      syncConversationAgent runtime
      pure (False,updated)
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

-- Sidebar requests are fixed human operations, adopted after host hit/lifetime
-- validation. Dialog purposes carry the exact ID instead of a directory index.
applyAgentSidebar :: ConversationState -> AgentSidebarRequest -> Desktop -> IO Desktop
applyAgentSidebar runtime@(ConversationState _ _ _ agents) request d=case request of
  ShowAgent ident | ident==AR.primaryAgent agents->perform runtime "show" [] d
                  | otherwise->showAgentHistory runtime ident d
  ConfigureAgent receipt option value->
    startAgentControl runtime (AH.agentConfigAgent receipt) Nothing (fmap (fmap (const AgentControlAccepted)) (AH.configureAgentAt hub receipt option value)) d
  RenameAgentTo ident name->do
    result<-AH.renameAgent hub AH.Human ident name
    pure d {status=either id (const "Agent renamed.") result}
  CreateAgent workspace _ _ | workspace/=startingDirectory d->pure d {status="Agent workspace changed; reopen the form."}
  CreateAgent workspace name task->do
    let spec=AH.SpawnSpec name task workspace AH.Shared AH.Fresh Nothing Nothing
    accepted<-AR.requestAgentCreation agents spec
    pure d {status=either id (const "Starting agent…") accepted}
  _->pure d {status="Completion owner is unavailable."}
  where hub=AR.agentHub agents

perform :: ConversationState -> Text -> [Text] -> Desktop -> IO Desktop
perform (ConversationState _ ref _ _) "focus" [] d=do
  modifyIORef' ref (\state -> state {deferredApproval=False})
  pure d
perform (ConversationState _ ref _ _) "toggle-tool-run" [ident] d=do
  state<-readIORef ref
  let target=conversationTarget d
      next=toggleExpansion target (RunExpansion ident) state
  writeIORef ref next
  keepConversationPosition d <$> paintView target False next d
perform runtime@(ConversationState _ ref _ _) action values d=do
  state<-readIORef ref
  case () of
    _ | (isNothing (conversationPresenter state) || isNothing (primaryInput state)) &&
        (T.null (conversationTarget d) && action=="send")->
          pure d {status=unavailableConversation}
      | action=="show"->do
          prepared<-ensureConversationEditor runtime "" "Primary" d
          performPrimary runtime action values (selectConversationView "" "Primary" prepared)
      | not (T.null (conversationTarget d)) && action `elem` ["send","cancel","copy","toggle-activity"]->performChild runtime action values d
      | otherwise->performPrimary runtime action values d

performPrimary :: ConversationState -> Text -> [Text] -> Desktop -> IO Desktop
performPrimary runtime@(ConversationState directory ref consoles _) action values original = do
  selected<-if action=="show" then ensureConversationEditor runtime "" "Primary" original else pure original
  d<-if action `elem` ["cancel","configure"] then cancelQuestion runtime "Question cancelled." selected else pure selected
  previous<-readIORef ref
  now<-getCurrentTime
  zone<-getCurrentTimeZone
  let s=if action `elem` ["send"] then stampReply now zone previous else previous
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
    ("toggle-activity",[ident]) -> do
      let next=toggleExpansion "" (ActivityExpansion ident) s
      writeIORef ref next
      keepConversationPosition d <$> paint False next d
    ("question-choice",[token,index]) | Just ident<-readMaybe (T.unpack token),Just chosen<-readMaybe (T.unpack index),
        Just q<-chatQuestion d,questionToken q==ident,chosen>=0,chosen<length (questionChoices q) ->
      revealQuestion runtime (clearReplySelection d {chatQuestion=Just q {questionChoice=Just chosen,questionFocused=True}})
    ("question-input",token:rest) | Just q<-chatQuestion d,token==T.pack (show (questionToken q)) ->
      let p=case rest of
            offset:_ | Just n<-readMaybe (T.unpack offset) -> min (bufferLength (questionBuffer q)) (questionInputStart (conversationWidth d) q+max 0 n)
            _ -> caret (questionSelection q)
      in revealQuestion runtime (clearReplySelection d {chatQuestion=Just q {questionChoice=Nothing,questionSelection=Selection p p,questionFocused=True}})
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
                next=appendPrimary [PrimaryMessage AssistantSpeaker (questionText q),PrimaryMessage UserSpeaker answer]
                  (rememberQuestion ident actor receipt value s) {waitingQuestion=Nothing,queuedQueries=queued}
            writeIORef ref next
            paint False next d {chatQuestion=Nothing,status="Answer submitted.",agentQueued=queryCount "" queued}
    ("question-cancel",[token]) | Just q<-chatQuestion d,token==T.pack (show (questionToken q)) ->
      cancelQuestion runtime "Question cancelled by user." d
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
            AR.failPendingPrimary (conversationAgents runtime) "Agent configuration changed."
            mapM_ denyChild (map snd (approvals s))
            retired<-retireRequests ref
            mapM_ A.stopClient (connection s)
            writeIORef ref retired {provider=config,connection=Nothing,session=Nothing,streamTails=M.empty,promptReply=Nothing,pending=M.empty,queuedPrompt=Nothing,queuedQueries=childQueries (queuedQueries retired),approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty}
            pure d {status="Agent configuration saved."}
    ("show",_) -> do
      modifyIORef' ref (\state -> state {deferredApproval=False})
      -- A recovered transcript belongs to the checkpoint until a provider
      -- connects. Opening its window must not repaint it from empty state.
      ensureEditorWithState True s "" "Primary" d
    ("copy",_) -> pure d {status="Conversation copy requires its presentation owner."}
    ("send",_:prompt:selectionFlag:fileFlag:diagnosticFlag:_) | not (T.null (T.strip prompt)),not (primaryBusy s) ->
      submitPrimaryPrompt runtime s Nothing prompt (selectionFlag=="true",fileFlag=="true",diagnosticFlag=="true") d
    ("cancel",_) -> do
      let agents=conversationAgents runtime
      AH.cancelExternalAgentControls (AR.agentHub agents) (AR.primaryAgent agents)
      AR.failPrimaryControl (conversationAgents runtime) "Agent control cancelled."
      let child (_,ChildPermission{})=True
          child _=False
          retained=filter child (approvals s)
          keepDialog=maybe False (`elem` map fst retained) (presented s)
      mapM_ (C.killConsole consoles) (S.toList (ownedTerminals s))
      forM_ (connection s) $ \client -> do
        forM_ (session s) $ \sid -> A.notify client "session/cancel" (object ["sessionId" .= sid])
        mapM_ (cancelApproval client . snd) (filter (not . child) (approvals s))
      retired<-retireRequests ref
      writeIORef ref retired {pending=M.map (\phase->if phase==Prompting then CancellingPrompt else phase) (pending retired),queuedPrompt=Nothing,approvals=retained,presented=if keepDialog then presented s else Nothing,deferredApproval=False}
      -- Context preparation has no provider prompt whose response could release
      -- an admitted Hub receipt. Keep the barrier only after actual prompt IO.
      unless (any isPrompt (M.elems (pending retired))) $
        completeConversationDelivery runtime (Left "Agent prompt cancelled.")
      pure (if keepDialog then d else dismissPermission d) {status="Cancellation requested."}
    _ | Just suffix<-T.stripPrefix "approval:" action, Just token<-readMaybe (T.unpack suffix) -> decide runtime token values d
    _ -> pure d
  where input label text=Input label text (T.length text)

-- | Capture only small session/provider/configuration identities at human menu
-- dispatch. The returned private prefill is for a private form; capture starts
-- no process and retains no Desktop, transcript, buffer or Undo.
captureConversationSession :: ConversationState -> Desktop -> IO (Either Text (Session.ConversationTarget ConversationSessionReceipt))
captureConversationSession (ConversationState _ ref _ agents) d=do
  s<-readIORef ref
  if questionsClosed s || isNothing (conversationPresenter s) || isNothing (primaryInput s)
    then pure (Left unavailableConversation)
    else if primaryBusy s then pure (Left "Cancel the current reply before changing sessions.") else do
      launch<-makeStableName =<< evaluate (provider s)
      client<-traverse (\value->makeStableName =<< evaluate value) (connection s)
      config<-either (const Nothing) (Just . fst) <$> AH.agentConfiguration (AR.agentHub agents) (AR.primaryAgent agents) >>= traverse evaluate
      sid<-traverse evaluate (session s)
      receipt<-evaluate (ConversationSessionReceipt (sessionIdentity s) (AR.primaryAgent agents) launch client sid config)
      target<-evaluate (Session.ConversationTarget receipt (T.null (conversationTarget d)) (maybe "" (\(_,_,value)->value) (lastSession s)))
      pure (Right target)

-- | Capture the selected agent and geometry without retaining a desktop or
-- provider metadata. The original configuration governs opening and every
-- submenu/submit, including changes while the plugin worker reads choices.
captureConversationChoices :: ConversationState -> Desktop -> IO (Either Text ChoicePopupTarget)
captureConversationChoices (ConversationState _ _ _ agents) d=case activeWindow d of
  Just w | activeConversation d,windowFocused d w->do
    let view=T.copy (conversationTarget d)
        who=if T.null view then AR.primaryAgent agents else AH.AgentId view
    _<-evaluate (T.length view+T.length (AH.agentIdText who))
    captured<-AH.agentConfiguration (AR.agentHub agents) who
    case captured of
      Left err->pure (Left err)
      Right (receipt,_)->do
        ready<-evaluate receipt
        let Rect x y _ _=agentTitleRect d w
            frame=bounds w
        _<-evaluate (left frame+top frame+width frame+height frame)
        target<-evaluate (ChoicePopupTarget (windowId w) view ready frame x (y+1))
        pure (Right target)
  _->pure (Left "Select a conversation window.")

-- This is the owning lifecycle operation, not a text command adapter. Admission
-- is serialized with provider ticks; a stale form never cancels a question,
-- retires a request or starts a different provider incarnation.
applyConversationSession :: ConversationState -> Session.ConversationRequest ConversationSessionReceipt -> Desktop -> IO Desktop
applyConversationSession runtime@(ConversationState _ ref consoles agents) request original=do
  s<-readIORef ref
  let (receipt,resume)=case request of
        Session.NewConversation target->(target,Nothing)
        Session.ResumeConversation target sid->(target,Just sid)
      ConversationSessionReceipt owner primary expectedLaunch expectedClient expectedSession expectedConfig=receipt
  launch<-makeStableName =<< evaluate (provider s)
  client<-traverse (\value->makeStableName =<< evaluate value) (connection s)
  config<-either (const Nothing) (Just . fst) <$> AH.agentConfiguration (AR.agentHub agents) (AR.primaryAgent agents)
  let current=not (questionsClosed s) && owner==sessionIdentity s && primary==AR.primaryAgent agents &&
        launch==expectedLaunch && client==expectedClient && session s==expectedSession && config==expectedConfig &&
        not (primaryBusy s) && dialog original==Nothing &&
        maybe True (\sid->T.null (conversationTarget original) && not (T.null sid)) resume
  if isNothing (conversationPresenter s) || isNothing (primaryInput s) then pure original {status=unavailableConversation}
    else if not current then pure original {status="Conversation session changed or is busy; invoke it again."} else do
      selected<-case resume of
        Nothing->ensureConversationEditor runtime "" "Primary" original >>= pure . selectConversationView "" "Primary"
        Just _->pure original
      d<-cancelQuestion runtime "Question cancelled." selected
      AR.failPendingPrimary agents "Agent session changed."
      mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
      retired<-retireRequests ref
      mapM_ A.stopClient (connection s)
      writeIORef ref retired {connection=Nothing,session=Nothing,streamTails=M.empty,promptReply=Nothing,pending=M.empty,queuedPrompt=Nothing,queuedQueries=childQueries (queuedQueries retired),transcript=Transcript.emptyPrimaryTranscript,toolExpansions=S.filter ((/="").fst) (toolExpansions s),lastMessageAt=Nothing,reads=maybe M.empty (const (sourceSnapshots d)) resume,approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty}
      start runtime resume d

-- Direct prompts and already-enqueued queries carry no draft consumption right.
submitPrimaryPrompt :: ConversationState -> State -> Maybe AR.PrimaryControl -> Text -> (Bool,Bool,Bool) -> Desktop -> IO Desktop
submitPrimaryPrompt runtime@(ConversationState _ ref _ _) s control prompt (selectionFlag,fileFlag,diagnosticFlag) d=do
  let context=contextText selectionFlag fileFlag diagnosticFlag d
      full=prompt<>(if T.null context then "" else "\n\n"<>context)
      next=appendPrimary [PrimaryMessage UserSpeaker (composerMarkdown prompt)] s {queuedPrompt=Just (full,control),reads=sourceSnapshots d}
  writeIORef ref next
  opened<-if isNothing (connection s) then start runtime Nothing d else sendQueued runtime d
  latest<-readIORef ref
  paint True latest opened

isPrompt :: Phase -> Bool
isPrompt Prompting=True
isPrompt CancellingPrompt=True
isPrompt _=False

isSteering :: Phase -> Bool
isSteering Steering{}=True
isSteering _=False

steeringPending :: State -> Bool
steeringPending s=any isSteering (M.elems (pending s)) || maybe False preparingSteer (promptPreparation s)

busy :: State -> Bool
busy s=not (M.null (pending s)) || not (isNothing (queuedPrompt s)) || not (isNothing (promptPreparation s))

-- Public input stays pending across the worker-to-provider mailbox gap. Hub
-- admission uses protocol busy state so a control cannot block its own arrival.
primaryBusy :: State -> Bool
primaryBusy s=busy s || M.member "" (agentControls s)

start :: ConversationState -> Maybe Text -> Desktop -> IO Desktop
start (ConversationState _ ref _ _) resume d = mask_ $ do
  s<-readIORef ref
  if isNothing (conversationPresenter s) || isNothing (primaryInput s) then pure d {status=unavailableConversation}
  else do
    let (selectedLaunch,selectedDirectory)=case (resume,lastSession s) of
          (Just wanted,Just (savedProvider,savedDirectory,savedId)) | wanted==savedId -> (savedProvider,savedDirectory)
          _ -> (provider s,maybe (startingDirectory d) treeRoot (sideTree d))
    -- Project the captured fields before the worker escapes; it needs neither
    -- the source Desktop nor the rest of the conversation state.
    launch<-evaluate selectedLaunch
    directory<-evaluate selectedDirectory
    -- Inherit the mask through Async result publication: a successfully acquired
    -- client belongs either to the result or to its exception cleanup. Only the
    -- directory lookup is unmasked before a client exists. ACP owns cancellation
    -- safety within process acquisition; request failure closes the acquired peer.
    worker<-asyncWithUnmask $ \unmask->do
      root<-unmask (canonicalizePath directory)
      client<-A.startClient launch root
      (do
        ident<-A.request client "initialize" (object ["protocolVersion" .= (1::Int),"clientInfo" .= object ["name" .= ("hide"::Text),"version" .= ("0.1.0.0"::Text)],
          "clientCapabilities" .= object ["fs" .= object ["readTextFile" .= True,"writeTextFile" .= True],"terminal" .= Terminal.terminalAvailable]])
        pure (root,client,ident)) `onException` A.stopClient client
    -- Input captured while a saved provider starts belongs to that selected
    -- launch, not the previously configured provider.
    writeIORef ref s {provider=launch,promptPreparation=Just (StartingClient resume worker)}
    pure d {status="Connecting to ACP provider...",agentSteering=False,agentReplying=True,agentContextUsage=Nothing,agentSettings=[]}

restorePrimaryDraft :: Text -> Desktop -> Desktop
restorePrimaryDraft text d=case M.lookup "" (conversationViews d) of
  Nothing->d
  Just view->d {editorDrafts=M.adjust (\draft->draft {editorDraftBuffer=newBuffer text,
    editorDraftSelection=Selection (T.length text) (T.length text)}) (conversationDraftRef view) (editorDrafts d)}

restoreEmptyPrimaryDraft :: Text -> Desktop -> Desktop
restoreEmptyPrimaryDraft text d
  | maybe False ((==0).bufferLength.editorDraftBuffer) draft=restorePrimaryDraft text d
  | otherwise=d
  where draft=M.lookup "" (conversationViews d) >>= (\view->M.lookup (conversationDraftRef view) (editorDrafts d))

sendQueued :: ConversationState -> Desktop -> IO Desktop
sendQueued (ConversationState _ ref _ _) d = do
  s<-readIORef ref
  case (connection s,session s,queuedPrompt s) of
    (Just _,Just _,Just (prompt,control)) -> beginPromptPreparation ref False prompt control d
    _ -> pure d

beginPromptPreparation :: IORef State -> Bool -> Text -> Maybe AR.PrimaryControl -> Desktop -> IO Desktop
beginPromptPreparation ref steering text control d = mask $ \restore -> do
  s<-readIORef ref
  if not (isNothing (promptPreparation s)) || not (null (retiringRequests s))
    then do
      mapM_ (AR.rejectPrimaryControl "Waiting for the previous agent request to stop; draft kept.") control
      when (not steering && not (isNothing control)) (modifyIORef' ref (\state->state {queuedPrompt=Nothing}))
      pure d {status="Waiting for the previous agent request to stop."}
    else do
      worker<-async (restore (preparePrompt s text))
      modifyIORef' ref (\state -> state {promptPreparation=Just (ContextPrompt steering text control worker)})
      pure d {status="Preparing agent context...",agentReplying=True}

pollPromptPreparation :: ConversationState -> Desktop -> IO Desktop
pollPromptPreparation runtime@(ConversationState _ ref _ _) d = do
  s<-readIORef ref
  case promptPreparation s of
    Nothing -> if isNothing (queuedPrompt s) then pollQueuedPrimaryEditor runtime s d else sendQueued runtime d
    Just (StartingClient resume worker)->do
      result<-poll worker
      case result of
        Nothing->pure d
        Just outcome->mask_ $ case outcome of
          Left err->do
            mapM_ (AR.rejectPrimaryControl "Could not start the primary provider.") (snd =<< queuedPrompt s)
            writeIORef ref s {promptPreparation=Nothing,queuedPrompt=Nothing}
            pure (message "Cannot start agent" (wrapMessage (T.pack (show err))) d)
          Right (root,client,ident)->do
            -- Transfer ownership in one state update. Retirement no longer owns
            -- this completed result once the connection becomes current.
            writeIORef ref s {connection=Just client,project=root,promptPreparation=Nothing,
              deliveredContext=Nothing,agentInitialized=Null,agentConfig=Null,streamTails=M.empty,promptReply=Nothing,
              lastAgentSync=Nothing,pending=M.singleton ident (Initializing resume)}
            pure d
    Just (ContextPrompt steering text control worker) -> do
      result<-poll worker
      case result of
        Nothing -> pure d
        Just outcome -> do
          modifyIORef' ref (\state -> state {promptPreparation=Nothing})
          let prepared=either (const (Left "Could not prepare agent context.")) id outcome
          case (connection s,session s,prepared) of
            (_,_,Left err) -> do
              mapM_ (AR.rejectPrimaryControl err) control
              unless steering $ do
                modifyIORef' ref (\state -> state {queuedPrompt=Nothing})
                completeConversationDelivery runtime (Left err)
              pure (if steering || not (isNothing control) then d else restoreEmptyPrimaryDraft text d) {status=err}
            (Just client,Just sid,Right (blocks,context))
              | steering && Prompting `notElem` M.elems (pending s) -> do
                  mapM_ (AR.rejectPrimaryControl "The turn ended while preparing steering; draft kept.") control
                  pure d {status="The turn ended while preparing steering; draft kept."}
              | otherwise -> do
                current<-maybe (pure True) (\request->AR.primaryControlCurrent (conversationAgents runtime) request (connection s) (session s)) control
                if not current then do
                  mapM_ (AR.rejectPrimaryControl "Agent control expired; draft kept.") control
                  unless steering (modifyIORef' ref (\state->state {queuedPrompt=Nothing}))
                  pure d {status="Agent control expired; draft kept."}
                else do
                  let method=if steering then "_session/steering" else "session/prompt"
                      meta=["_meta" .= object ["steering" .= object ["idleBehavior" .= ("promptRequired"::Text)]] | steering]
                  ident<-A.request client method (object (["sessionId" .= sid,"prompt" .= blocks]++meta))
                  modifyIORef' ref (\state -> state {queuedPrompt=if steering then queuedPrompt state else Nothing,
                    pending=M.insert ident (if steering then maybe Prompting (Steering text) control else Prompting) (pending state),deliveredContext=Just context,
                    promptReply=if steering then promptReply state else Just (PromptReply ident [])})
                  unless steering $ forM_ control $ \request->case request of
                    AR.QueryPrimary _ _ _ _ _ _ reply->void (tryPutMVar reply (Right ()))
                    _->pure ()
                  pure d {status=if steering then "Steering request sent; draft kept until accepted." else "Agent is replying...",agentReplying=True}
            _ -> do
              mapM_ (AR.rejectPrimaryControl "Primary provider disconnected; draft kept.") control
              unless steering (modifyIORef' ref (\state->state {queuedPrompt=Nothing}))
              pure d

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

-- | Advance mailboxes, protocol replies, approvals and transcript views.
-- The caller serializes access to both desktop and conversation state.
tickConversation :: ConversationState -> Desktop -> IO Desktop
tickConversation runtime@(ConversationState _ ref _ _) original = do
  fresh<-pruneChildApprovals runtime original
  initial<-drainConversationAgents runtime fresh
  currentQuestion<-readIORef ref
  ready<-case waitingQuestion currentQuestion of
    Just (QuestionTicket _ _ (Just receipt))->do
      live<-providerCurrent receipt currentQuestion
      if live then pure initial else cancelQuestion runtime "Question requester ended." initial
    _->pure initial
  let d=ready
  flushTerminalWaiters runtime
  s<-readIORef ref
  events<-maybe (pure []) A.pollEvents (connection s)
  received<-foldM (receive runtime) d events
  captured<-pollFileCaptures runtime received
  childPrepared<-pollQueuedChildEditor runtime captured
  updated<-pollPromptPreparation runtime childPrepared
  afterEvents<-readIORef ref
  advanced<-case break ((=="").queryTarget) (queuedQueries afterEvents) of
    (_,EditorQuery{}:_) -> pure updated
    (before,query:rest) | not (primaryBusy afterEvents), not (isNothing (connection afterEvents)), session afterEvents/=Nothing -> do
      text<-case query of
        SubmittedQuery value->pure (Just value)
        QuestionQuery ident actor receipt answer->do
          live<-providerCurrent receipt afterEvents
          active<-AH.statusAgent (AR.agentHub (conversationAgents runtime)) (AH.Agent actor) actor
          pure $ if live && either (const False) (const True) active
            then Just ("Human answer to ask_user question "<>T.pack (show ident)<>" (submitted explicitly):\n\n"<>answer) else Nothing
      writeIORef ref afterEvents {queuedQueries=before++rest,queuedPrompt=(\value->(value,Nothing)) <$> text,reads=sourceSnapshots updated}
      maybe (pure updated) (const (sendQueued runtime updated)) text
    _ -> pure updated
  current<-readIORef ref
  -- Esc/Cancel of a permission dialog denies it; it must never leave the peer waiting.
  case presented current of
    Just token | not (isApprovalDialog token advanced) -> do
      forM_ (lookup token (approvals current)) $ \approval -> denyChild approval >> mapM_ (\client -> cancelApproval client approval) (connection current)
      modifyIORef' ref (\state -> state {approvals=filter ((/=token).fst) (approvals state),presented=Nothing})
    _ -> pure ()
  laidOut<-revealQuestion runtime advanced
  afterDismiss<-readIORef ref
  let rendered=laidOut {agentReplying=primaryBusy afterDismiss,agentQueued=queryCount "" (queuedQueries afterDismiss)}
  syncConversationAgent runtime
  visible<-refreshChildConversation runtime rendered
  shown<-present runtime visible
  notice<-AR.runtimeNotice (conversationAgents runtime)
  captureConversationSources runtime (maybe shown (\text -> shown {status=text}) notice)

-- Capture every received source root, including closed/inert views. Painting
-- and parser construction remain on the existing presentation/checkpoint workers.
captureConversationSources :: ConversationState -> Desktop -> IO Desktop
captureConversationSources (ConversationState _ ref _ agents) desktop=do
  state<-readIORef ref
  launch<-makeStableName =<< evaluate (provider state)
  sessionName<-traverse evaluate (session state)
  primary<-traverse (\client->do ident<-makeStableName =<< evaluate client; evaluate (ident,sessionName)) (connection state)
  views<-M.traverseWithKey (capture state launch primary) (conversationViews desktop)
  pure desktop {conversationViews=views}
  where
    capture state launch primary target view
      | T.null target,not (isNothing (conversationPresenter state)),not (isNothing (connection state)) || Transcript.primaryTranscriptStarted (transcript state) || not (isNothing (chatQuestion desktop)) || not (isNothing (conversationSource view))=do
          source<-captureConversationSource target (PrimaryBodyProvider launch primary) (TranscriptPrimary (transcript state)) (conversationSource view)
          pure view {conversationSource=Just source}
      | not (T.null target),Just records<-M.lookup target (childRecords state)=do
          receipt<-AH.agentConfiguration (AR.agentHub agents) (AH.AgentId target)
          case receipt of
            Right (owner,_)->do
              source<-captureConversationSource target (ChildBodyProvider owner) records (conversationSource view)
              pure view {conversationSource=Just source}
            Left _->pure view
      | otherwise=pure view

-- Capture only immutable roots and small presentation/lifetime receipts. Every
-- installed target owns one payload; closed inert snapshots are not scheduled.
conversationBodyRequests :: ConversationState -> Desktop -> IO [BodyRequest]
conversationBodyRequests (ConversationState _ ref _ agents) desktop=do
  state<-readIORef ref
  launch<-makeStableName =<< evaluate (provider state)
  sessionName<-traverse evaluate (session state)
  primary<-traverse (\client->do
    ident<-makeStableName =<< evaluate client
    evaluate (ident,sessionName)) (connection state)
  expansion<-makeStableName =<< evaluate (toolExpansions state)
  fmap concat $ forM (M.toList (conversationViews desktop)) $ \(target,view)->case conversationBodyRef view of
    Nothing->pure []
    Just reference->do
      live<-W.windowRefCurrent reference
      let question=if T.null target then chatQuestion desktop else Nothing
          records=if T.null target then TranscriptPrimary (transcript state) else M.findWithDefault (TranscriptRecords []) target (childRecords state)
          owns=if T.null target then not (isNothing (conversationPresenter state)) && (not (isNothing (connection state)) || Transcript.primaryTranscriptStarted (transcript state) || not (isNothing question) || not (isNothing (lastQuestion state))) else M.member target (childRecords state)
          visible=any ((==PluginContent reference).windowContent) (windows desktop)
          recovered=case conversationLogical view of
            Just logical | RecoveredBodyProvider{}<-logicalBodyProvider logical,not owns->Just logical
            _->Nothing
      captured<-if T.null target then pure (Just (PrimaryBodyProvider launch primary)) else fmap (either (const Nothing) (Just . ChildBodyProvider . fst)) (AH.agentConfiguration (AR.agentHub agents) (AH.AgentId target))
      schema<-traverse (\q->evaluate (QuestionSchema (questionToken q) (questionText q) (questionChoices q))) question
      -- Retain the pending question lifetime until a question-free body is
      -- adopted, even if cancellation races its first preparation.
      case schema of
        Just (QuestionSchema token _ _) | live->modifyIORef' ref (\current->current {lastQuestion=Just token})
        _->pure ()
      identity<-transcriptIdentity records
      pure $ case recovered of
        Just logical | visible->[BodyRequest
          (BodyKey reference target (logicalBodyProvider logical) (logicalBodyTranscriptIdentity logical) Nothing
            (conversationWidthFor target desktop) (videoMode desktop/=Nothing) (wideSectionTitles desktop)
            (BodyDemand (conversationAnchor view) (conversationRowShift view) (conversationHeightFor target desktop)) expansion)
          (BodyInput (if T.null target then "Conversation" else conversationName view) (project state) Nothing (TranscriptRecords []) Nothing S.empty (Just logical) (capturedSelection view))]
        _->case captured of
          Just owner | live && owns->
            let same=maybe True ((==owner).logicalBodyProvider) (conversationLogical view)
                retained=case conversationAnchor view of
                  At (QuestionPoint token _ _)->case schema of
                    Just (QuestionSchema current _ _) -> token==current
                    Nothing -> False
                  _->True
                keepAnchor=same && retained
            in [BodyRequest (BodyKey reference target owner identity (case schema of Just (QuestionSchema token _ _)->Just token; Nothing->Nothing)
              (conversationWidthFor target desktop) (videoMode desktop/=Nothing) (wideSectionTitles desktop)
              (BodyDemand (if keepAnchor then conversationAnchor view else FollowEnd) (if keepAnchor then conversationRowShift view else 0) (conversationHeightFor target desktop)) expansion)
              (BodyInput (if T.null target then "Conversation" else conversationName view) (project state) sessionName records
                schema (toolExpansions state) (conversationLogical view) (if same then capturedSelection view else Nothing))]
          _->[]

capturedSelection :: ConversationView -> Maybe BodySelection
capturedSelection view=case conversationReplySelection view of
  Just selected->selected `seq` Just selected
  Nothing->Nothing

-- Completed streaming text may trail the latest root while the single worker
-- prepares its successor. Owner/question/UI expansion identities remain exact.
adoptConversationBodies :: ConversationState -> [BodyResult] -> Desktop -> IO Desktop
adoptConversationBodies runtime results desktop=do
  desired<-conversationBodyRequests runtime desktop
  foldM (adopt desired) desktop results
  where
    adopt desired current (BodyResult key result)=case [wanted | BodyRequest wanted _<-desired,bodyOwnerMatches key wanted] of
      []->pure current
      _->case result of
        Left err->pure current {status=err}
        Right (PreparedBody body layout controls logical normalized)->do
          admitted<-case bodyProvider key of
            RecoveredBodyProvider identity | bodyWindow key `S.member` retiredPluginWindows current,
              Just retained<-conversationLogicalBody (bodyTarget key) current,
              logicalBodyIdentity retained==identity->pure (Just current {pluginWindows=M.insert (bodyWindow key) body (pluginWindows current)})
            _->do
              update<-W.refreshWindow (bodyWindow key) body
              traverse (\prepared->adoptWindowUpdate Plugin.HumanMenu prepared current) update
          case admitted of
            Nothing->pure current
            Just next->if M.lookup (bodyWindow key) (pluginWindows next)/=Just body then pure current else do
                let target=bodyTarget key
                    viewport=hostBodyViewport controls
                    adjust window | windowContent window/=PluginContent (bodyWindow key)=window
                                  | otherwise=let view=M.lookup target retainedViews
                                              in window {scrollRow=maybe 0 viewportScroll viewport,
                                                scrollColumn=maybe 0 conversationScrollColumn view,
                                                selection=maybe (Selection 0 0) (\v->projectConversationSelection v viewport) view}
                    frames=map adjust (windows next)
                    survives point=case point of
                      BodyPoint ident _ _->logicalBodyItemIndex ident logical/=Nothing
                      QuestionPoint token _ _->bodyQuestionToken key==Just token
                    retain view=let same=maybe True ((==bodyProvider key).logicalBodyProvider) (conversationLogical view)
                                    preparedView=view {conversationLogical=Just logical,
                      conversationAnchor=maybe (conversationAnchor view) viewportAnchor viewport,conversationRowShift=0,
                      conversationBody=InstalledBody (bodyWindow key) (Just (BodyControlReceipt body (bodyColumns key) (bodyWide key) layout controls)),
                      conversationReplySelection=case conversationReplySelection view of
                        Just chosen@(BodySelection a z) | same && survives a && survives z->case normalized of
                          Just (original,clamped) | chosen==original->clamped
                          _->Just chosen
                        _->Nothing,conversationCaretIntent=if same then conversationCaretIntent view else Nothing}
                                in settleConversationCaret layout viewport preparedView
                    retainedViews=M.adjust retain target (conversationViews next)
                    presented=next {conversationViews=retainedViews,windows=frames}
                let previousQuestion=do
                      view<-M.lookup target (conversationViews current)
                      InstalledBody _ (Just (BodyControlReceipt _ _ _ _ oldControls))<-pure (conversationBody view)
                      hostBodyQuestionToken oldControls
                when (T.null target) (modifyIORef' (case runtime of ConversationState _ owner _ _->owner) (\state->state {lastQuestion=bodyQuestionToken key}))
                pure (if previousQuestion/=bodyQuestionToken key then ensureQuestionVisible presented else presented)

receive :: ConversationState -> Desktop -> A.Event -> IO Desktop
receive runtime@(ConversationState _ ref consoles _) d event = do
  s<-readIORef ref
  case event of
    A.Disconnected reason -> do
      cleared<-cancelQuestion runtime "Question requester disconnected." d
      current<-readIORef ref
      redact<-conversationRedactor runtime current
      AR.failPendingPrimary (conversationAgents runtime) "Agent disconnected."
      mapM_ denyChild (map snd (approvals s))
      mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
      retired<-retireRequests ref
      writeIORef ref (appendPrimary [PrimaryDisconnect (redact reason)]
        retired {transcript=transcript current,connection=Nothing,session=Nothing,streamTails=M.empty,promptReply=Nothing,pending=M.empty,queuedPrompt=Nothing,queuedQueries=childQueries (queuedQueries retired),approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty})
      pure (dismissPermission cleared) {status="Agent disconnected.",agentSteering=False}
    A.Response ident result -> do
      writeIORef ref s {pending=M.delete ident (pending s)}
      when (maybe False isPrompt (M.lookup ident (pending s))) (flushConversationChunks runtime)
      case (M.lookup ident (pending s),result,connection s) of
        (Nothing,_,_) -> pure d
        (_,Left err,_) -> do
          mapM_ (AR.rejectPrimaryControl "Agent request failed; draft kept.") (snd =<< queuedPrompt s)
          redact<-conversationRedactor runtime s
          when (maybe False isPrompt (M.lookup ident (pending s))) (completeConversationDelivery runtime (Left "Agent prompt failed."))
          modifyIORef' ref (\state -> appendPrimary [PrimaryFailure (redactValue redact err)] state {queuedPrompt=Nothing,deliveredContext=Nothing})
          case M.lookup ident (pending s) of
            Just (Setting control)->AR.rejectPrimaryControl "Agent configuration failed." control
            Just (Steering _ control)->AR.rejectPrimaryControl "Agent request failed; see Conversation." control
            _->pure ()
          pure d {status="Agent request failed; see Conversation."}
        (Just (Initializing resume),Right value,Just client)
          | field "protocolVersion" value /= Just (1::Int) -> do
              A.stopClient client
              mapM_ (AR.rejectPrimaryControl "Unsupported ACP protocol version.") (snd =<< queuedPrompt s)
              modifyIORef' ref (\state -> state {queuedPrompt=Nothing})
              pure d {status="Unsupported ACP protocol version."}
          | otherwise -> do
              let capabilities=fromMaybe Null (field "agentCapabilities" value)
                  loadSupported=field "loadSession" capabilities==Just True
                  resumeSupported=case field "sessionCapabilities" capabilities >>= field "resume" of Just (Object _) -> True; _ -> False
                  method=case resume of Nothing -> "session/new"; Just _ | loadSupported -> "session/load"; _ -> "session/resume"
              if resume/=Nothing && not loadSupported && not resumeSupported then do
                A.stopClient client
                mapM_ (AR.rejectPrimaryControl "This provider cannot resume sessions.") (snd =<< queuedPrompt s)
                modifyIORef' ref (\state -> state {connection=Nothing,session=Nothing,queuedPrompt=Nothing})
                pure d {status="This provider cannot resume sessions.",agentReplying=False,agentSteering=False}
              else do
                servers<-AR.primaryServers (conversationAgents runtime)
                requestId<-A.request client method (object (["cwd" .= project s,"mcpServers" .= servers]++maybe [] (\sid->["sessionId" .= sid]) resume))
                modifyIORef' ref (\state -> state {pending=M.insert requestId (Starting resume) (pending state),session=resume,agentInitialized=value})
                pure d {status="Opening agent session...",agentSteering=(field "_meta" value >>= field "steering" >>= field "supported")==Just True}
        (Just (Starting resumed),Right value,_) -> case field "sessionId" value <|> resumed of
          Nothing -> do
            mapM_ (AR.rejectPrimaryControl "Agent returned no session ID.") (snd =<< queuedPrompt s)
            modifyIORef' ref (\state->state {queuedPrompt=Nothing})
            pure d {status="Agent returned no session ID."}
          Just sid -> do
            modifyIORef' ref (\state -> state {session=Just sid,agentConfig=value,lastSession=Just (provider state,project state,sid)})
            savedId<-persist (resumeRecordPath s) (object ["provider" .= launchValue (provider s),"cwd" .= project s,"sessionId" .= sid])
            safeSettings<-publicAgentSettings runtime value
            syncConversationAgent runtime
            sendQueued runtime d {agentSettings=safeSettings,status=either ("Session opened; could not save ID: "<>) (const ("Session "<>sid)) savedId}
        (Just (Setting control),Right value,_) -> do
          modifyIORef' ref (\state -> state {agentConfig=value})
          safeSettings<-publicAgentSettings runtime value
          keys<-conversationKeys runtime s
          case control of
            AR.ConfigurePrimary _ _ _ reply->void (tryPutMVar reply (Right (AP.filterPrivateCapabilities keys (AP.parseCapabilities (agentInitialized s) value))))
            _->pure ()
          pure d {agentSettings=safeSettings,contextMenu=Nothing,status=if M.member "" (agentControls s) then "Updating conversation settings..." else "Conversation settings updated."}
        (Just (Steering text control),Right value,_) -> case field "outcome" value :: Maybe Text of
          Just "injected" -> do
            modifyIORef' ref (appendPrimary [PrimaryMessage UserSpeaker (composerMarkdown text)])
            case control of
              AR.SteerPrimary _ _ _ reply->void (tryPutMVar reply (Right value))
              _->pure ()
            pure d {status="Follow-up added to the active turn."}
          Just outcome | outcome `elem` ["promptRequired","failed"] -> do
            AR.rejectPrimaryControl "Steering was not applied; the draft remains. Use Query to send it." control
            modifyIORef' ref (\state->state {deliveredContext=Nothing})
            pure d {status="Steering was not applied; the draft remains. Use Query to send it."}
          _ -> do
            AR.rejectPrimaryControl "Steering ownership was not confirmed; provider stopped. Draft kept; queued turns cancelled without replay." control
            mapM_ A.stopClient (connection s)
            stopped<-receive runtime d (A.Disconnected "Provider started an unowned steering turn or returned an unknown outcome.")
            pure stopped {status="Steering ownership was not confirmed; provider stopped. Draft kept; queued turns cancelled without replay."}
        (Just phase,Right value,_) | isPrompt phase -> do
          current<-readIORef ref
          let text=case promptReply current of Just (PromptReply request chunks) | request==ident->T.concat (reverse chunks); _->""
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
              let public=redactValue redact update
              modifyIORef' ref (appendPrimary [PrimaryPlan public])
              mapM_ (publishPrimaryEvent runtime) (AP.publicACPUpdate id public)
            "usage_update" -> mapM_ (publishPrimaryEvent runtime) (AP.publicACPUpdate id update)
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
        emitted<-atomicModifyIORef' ref $ \state ->
          let combined=M.findWithDefault "" role (streamTails state)<>text
              redacted=redactText keys combined
              held=maximum (0:[n | key<-keys,n<-[1..T.length key-1],T.take n key `T.isSuffixOf` redacted])
              (safe,tailText)=T.splitAt (T.length redacted-held) redacted
              timed=if T.null safe then state else stampReply now zone state
              next=(if T.null safe then timed else recordChunk role safe timed)
                {streamTails=M.insert role tailText (streamTails timed)}
          in (next,safe)
        when (role=="Agent") (publishPrimaryOutput runtime emitted)
      _ -> pure ()
    recordTool update=do
      current<-readIORef ref
      redact<-conversationRedactor runtime current
      now<-getCurrentTime
      let public=redactValue redact update
      modifyIORef' ref (\state -> recordToolUpdate public state {lastMessageAt=Just now})
      mapM_ (publishPrimaryEvent runtime) (AP.publicACPUpdate id public)

-- Provider publication runs on the daemon's existing serialized session tick
-- (including its suspend drain), not the renderer or ordinary input branch.
-- It uses the same exact connection receipt as controls, never captures human
-- prompts, and never copies transcript history during sync.
publishPrimaryEvent :: ConversationState -> AH.DriverEvent -> IO ()
publishPrimaryEvent (ConversationState _ ref _ agents) event=do
  state<-readIORef ref
  case (connection state,session state) of
    (Just client,Just sid)->AR.recordPrimaryEvent agents client sid event
    _->pure ()

publishPrimaryOutput :: ConversationState -> Text -> IO ()
publishPrimaryOutput runtime text=unless (T.null text) $
  publishPrimaryEvent runtime (AH.ProviderUpdate "output" (object ["text" .= T.take 8192 text,"truncated" .= (T.length text>8192)]))

pauseLabel :: Maybe UTCTime -> UTCTime -> TimeZone -> Maybe Text
pauseLabel previous now zone = case previous of
  Just before | diffUTCTime now before>=300 -> Just (T.pack (formatTime defaultTimeLocale "%b %-d, %H:%M" (utcToLocalTime zone now)))
  _ -> Nothing

stampReply :: UTCTime -> TimeZone -> State -> State
stampReply now zone state=appendPrimary (maybe [] (pure . PrimaryPause) (pauseLabel (lastMessageAt state) now zone))
  state {lastMessageAt=Just now}

-- Admission allocates update identities; plugin reduction stays lazy inside
-- each immutable source root. State forces only that wrapper at admission,
-- so later capture is constant-time and never evaluates records.
-- With no contribution the runtime is inert: new admission is rejected and
-- recovered sources remain owned by their checkpoint.
appendPrimary :: [PrimaryContent] -> State -> State
appendPrimary values state=case conversationPresenter state of
  Nothing->state
  Just presenter->foldl' (append (Transcript.primaryTranscript presenter)) state values
  where
    append presenter current value=let ident=nextRecord current in current
      {transcript=Transcript.appendPrimaryUpdate presenter (PrimaryUpdate (BodyItemId ident) ident value) (transcript current),nextRecord=ident+1}

recordChunk :: Text -> Text -> State -> State
recordChunk role text state=(appendPrimary [PrimaryChunk speaker text] state)
  {promptReply=case (role,promptReply state) of
      ("Agent",Just (PromptReply request chunks))->Just (PromptReply request (text:chunks))
      _->promptReply state}
  where speaker=if role=="Agent" then AssistantSpeaker else UserSpeaker

recordToolUpdate :: Value -> State -> State
recordToolUpdate update=appendPrimary [PrimaryTool update]

incoming :: ConversationState -> A.Client -> Value -> Text -> Value -> Desktop -> IO Desktop
incoming runtime@(ConversationState _ ref consoles _) client ident method params d = do
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
  forM_ (M.elems (pending s)) $ \phase->case phase of
    Setting control->AR.rejectPrimaryControl "Agent control cancelled." control
    Steering _ control->AR.rejectPrimaryControl "Agent control cancelled." control
    _->pure ()
  case promptPreparation s of
    Just (ContextPrompt _ _ control _)->mapM_ (AR.rejectPrimaryControl "Agent control cancelled.") control
    _->pure ()
  mapM_ (AR.rejectPrimaryControl "Agent control cancelled.") (snd =<< queuedPrompt s)
  let activeInput=M.lookup "" (agentControls s)
      queuedPrimary=[submitted | EditorQuery submitted (ChatEditorContext target _ _ _) _<-queuedQueries s,T.null target]
      keepQuery (EditorQuery _ (ChatEditorContext target _ _ _) _)=not (T.null target)
      keepQuery _=True
      retired=s {queuedQueries=filter keepQuery (queuedQueries s)}
  forM_ (fst =<< activeInput) (void . Editor.abortEditorSubmission)
  -- Invocation is not query admission. Retire all primary intents still waiting
  -- in this queue, even if their command already began or no worker exists yet.
  mapM_ (void . Editor.abortEditorSubmission) queuedPrimary
  let captures=fileCaptures s
      workers=[cancel worker | FileCapture _ worker<-captures]++map preparationCancel (maybe [] pure (promptPreparation s))++
        [cancel worker | Just (_,worker)<-[activeInput]]
  if null workers then writeIORef ref retired >> pure retired else do
    forM_ (connection s) $ \client -> forM_ captures $ \(FileCapture ident _) ->
      A.respond client ident (Left (failure "File request cancelled."))
    reaper<-async (restore (sequence_ workers))
    let next=retired {fileCaptures=[],promptPreparation=Nothing,agentControls=M.delete "" (agentControls s),
          retiringRequests=retiringRequests s++[(length workers,reaper)]}
    writeIORef ref next
    pure next

pollFileCaptures :: ConversationState -> Desktop -> IO Desktop
pollFileCaptures runtime@(ConversationState _ ref _ _) d = do
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
enqueueApproval (ConversationState _ ref _ _) approval=modifyIORef' ref (\s -> s {approvals=approvals s++[(nextApproval s,approval)],nextApproval=nextApproval s+1})

present :: ConversationState -> Desktop -> IO Desktop
present (ConversationState _ ref _ _) d = do
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
decide (ConversationState _ ref consoles _) token values d = do
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
    keep w=case find ((==windowId w).windowId) (windows before) of
      Just old | conversationTargetFor after w/=Nothing -> w {scrollRow=scrollRow old,scrollColumn=0,selection=Selection 0 0}
      _ -> w

-- Prompt and choices are immutable for the authenticated question token.
-- Only immutable question identity asks for a local reveal; idle ticks preserve
-- manual transcript scrolling and never inspect answer text or retained Undo.
questionInteraction :: Maybe ChatQuestion -> IO (Maybe (StableName ChatQuestion))
questionInteraction=traverse (\q->makeStableName =<< evaluate q)

revealQuestion :: ConversationState -> Desktop -> IO Desktop
revealQuestion (ConversationState _ ref _ _) d=do
  previous<-lastQuestionInteraction <$> readIORef ref
  current<-questionInteraction (chatQuestion d)
  modifyIORef' ref (\s->s {lastQuestionInteraction=current})
  pure (if current/=previous then ensureQuestionVisible d else d)

paint :: Bool -> State -> Desktop -> IO Desktop
paint=paintView ""

paintView :: Text -> Bool -> State -> Desktop -> IO Desktop
paintView target opening state desktop
  | not opening && M.notMember target (conversationViews desktop)=pure desktop
  | otherwise=do
      ready<-ensureEditorWithState opening state target (if T.null target then "Primary" else target) desktop
      pure $ if not (T.null target) then ready else ready {conversationViews=M.adjust (admitQuestion ready) target (conversationViews ready)}
  where
    admitQuestion ready view=case conversationBody view of
      InstalledBody reference receipt | reference `S.notMember` retiredPluginWindows ready,
        Just prepared<-M.lookup reference (pluginWindows ready)->
          let token=questionToken <$> chatQuestion ready
              columns=conversationWidthFor target ready
              empty=HostBodyControls token Nothing Nothing []
              current=case receipt of
                Just (BodyControlReceipt body _ _ layout controls) | body==prepared->
                  BodyControlReceipt prepared columns (wideSectionTitles ready) layout controls {hostBodyQuestionToken=token}
                _->BodyControlReceipt prepared columns (wideSectionTitles ready) Nothing empty
          in view {conversationBody=InstalledBody reference (Just current)}
      _->view

publicAgentSettings :: ConversationState -> Value -> IO [AgentSetting]
publicAgentSettings runtime@(ConversationState _ ref _ _) value=do
  current<-readIORef ref
  keys<-conversationKeys runtime current
  let public option=not (any (\text->any (`T.isInfixOf` text) keys)
        ([settingId option,settingName option,settingCategory option,settingCurrent option]++concatMap (\(ident,label)->[ident,label]) (settingChoices option)))
  pure (filter public (parseAgentSettings value))

draftCurrent :: DraftReceipt -> Desktop -> IO Bool
draftCurrent submitted d=maybe (pure False) (versionCurrent (Editor.submissionVersion submitted).editorDraftBuffer)
  (M.lookup (Editor.submissionDraft submitted) (editorDrafts d))

clearSubmittedDraft :: Maybe DraftReceipt -> Desktop -> IO Desktop
clearSubmittedDraft Nothing d=pure d
clearSubmittedDraft (Just submitted) d=applyEditorUpdate submitted (Editor.clearEditorDraft submitted) d

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
flushTerminalWaiters (ConversationState _ ref consoles _) = do
  s<-readIORef ref
  forM_ (connection s) $ \client -> forM_ (M.toList (terminalWaiters s)) $ \(tid,waiters) -> do
    result<-C.consoleOutput consoles tid
    let ready=case result of Left err -> Just (Left (failure err)); Right (_,_,Just code) -> Just (Right (exitStatus code)); _ -> Nothing
    forM_ ready $ \reply -> do
      mapM_ (\ident -> A.respond client ident reply) waiters
      modifyIORef' ref (\state -> state {terminalWaiters=M.delete tid (terminalWaiters state)})

conversationWidth :: Desktop -> Int
conversationWidth d = conversationWidthFor (conversationTarget d) d

conversationWidthFor :: Text -> Desktop -> Int
conversationWidthFor target d = max 1 $ case matching++available of
  w:_ -> width (bounds w)-2
  [] -> fst (screenSize d)-treeWidthOf d-4
  where
    matching=[w | w<-windows d,conversationTargetFor d w==Just target]
    available=[w | w<-windows d,maybe False (const True) (conversationTargetFor d w)]

conversationHeightFor :: Text -> Desktop -> Int
conversationHeightFor target d=case [window | window<-windows d,conversationTargetFor d window==Just target] of
  window:_->max 1 (pluginBodyRows d window)
  []->max 1 (snd (screenSize d)-6)

-- | Capture only public conversation metadata and path selection on the desktop
-- owner. Root discovery, context reads and reply encoding belong to the caller's
-- tool worker. The scoped command rejects deferred reads after session retirement;
-- an already admitted read may finish with its original immutable snapshot.
captureAgentSettings :: ConversationState -> Desktop -> IO Settings.AgentSettingsServices
captureAgentSettings (ConversationState _ ref _ _) desktop=do
  state<-readIORef ref
  connected<-evaluate (not (isNothing (connection state)))
  directory<-evaluate (if connected then project state else B.buildStartDirectory desktop)
  executable<-evaluate (A.executable (provider state))
  argumentCount<-evaluate (length (A.arguments (provider state)))
  environmentNames<-evaluate (force (map (T.pack . fst) (A.environment (provider state))))
  options<-evaluate (agentSettings desktop)
  target<-evaluate (conversationTarget desktop)
  snapshot<-evaluate Settings.SettingsSnapshot
    { Settings.settingsExecutable=executable,Settings.settingsArgumentCount=argumentCount
    , Settings.settingsEnvironmentNames=environmentNames,Settings.settingsConnected=connected
    , Settings.settingsSelectedAgent=if T.null target then Nothing else Just target
    , Settings.settingsReplying=primaryBusy state,Settings.settingsSteering=agentSteering desktop
    , Settings.settingsContextUsage=agentContextUsage desktop,Settings.settingsOptions=map setting options
    , Settings.settingsContext=Null,Settings.settingsContextError=Nothing }
  (registry,command)<-evaluate (settingsCommands state)
  let captured=SettingsCapture connected directory snapshot
  pure (Settings.AgentSettingsServices (fmap (either (Left . T.pack . show) Right)
    (Command.invoke registry command captured ())))
  where
    setting option=let secret=sensitiveLabel (T.unwords [settingId option,settingName option,settingCategory option]) in object
      ["id" .= settingId option,"name" .= settingName option,"category" .= settingCategory option,
       "current" .= (if secret then "[hidden]" else settingCurrent option),
       "choices" .= [object ["value" .= value,"name" .= title] | (value,title)<-settingChoices option,not secret],"redacted" .= secret]

-- | Capture authenticated primary attribution before queuing policy approval.
-- The transport resolves bearer credentials first; this operation checks the
-- owning actor and captures only the original provider incarnation metadata.
captureQuestionCaller :: ConversationState -> AH.AgentId -> IO (Either Text QuestionCaller)
captureQuestionCaller (ConversationState _ ref _ agents) actor
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

-- | Actor-bound worker capability. Fresh policy and the request claim use the
-- existing permission owner; host admission checks the original question caller
-- again. Neither this closure nor its public service retains a Desktop.
questionServices :: ConversationState -> QuestionCaller -> Permissions
  -> IO (Either Text ()) -> Questions.QuestionServices
questionServices runtime caller permissions live=Questions.QuestionServices $ \request->
  case Command.codecDecode Questions.questionInput (Command.codecEncode Questions.questionInput request) of
    Left err->pure (Left (Command.InvalidArguments err))
    Right checked->do
      arguments<-evaluate (force (Command.codecEncode Questions.questionInput checked))
      result<-requestPermission live permissions admit "ask_user" arguments
      pure (either (Left . Command.CommandRejected) Right result)
  where
    admit desktop _ args=case Command.codecDecode Questions.questionInput args of
      Left err->pure (desktop,pure (Left err))
      Right request->applyQuestion runtime (Just caller) desktop request

-- | The short host transition after policy admission. An identified request
-- belongs to this actor/runtime/provider incarnation. Only human input can
-- submit an answer; this operation never waits for one.
applyQuestion :: ConversationState -> Maybe QuestionCaller -> Desktop -> Questions.QuestionRequest
  -> IO (Desktop,IO (Either Text Value))
applyQuestion (ConversationState _ ref _ agents) caller d request
  | Just (QuestionCaller scope actor originalReceipt)<-caller,actor==AR.primaryAgent agents=do
      live<-AH.statusAgent (AR.agentHub agents) (AH.Agent actor) actor
      s<-readIORef ref
      owner<-(==scope) <$> (makeStableName =<< evaluate ref)
      same<-requesterCurrent originalReceipt s
      case live of
        Left err->pure (d,pure (Left err))
        Right _ | not (owner && same)->pure (d,pure (Left "Question requester session expired."))
        Right _ | questionsClosed s->pure (d,pure (Left "Editor session closed."))
        Right _->case request of
          Questions.ReadQuestion ident->do
            result<-questionStatus actor ident s
            pure (d,pure result)
          Questions.CreateQuestion{} | isNothing (conversationPresenter s)->pure (d,pure (Left unavailableConversation))
          Questions.CreateQuestion question choices->case waitingQuestion s of
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
cancelQuestion (ConversationState _ ref _ _) reason d=do
  s<-readIORef ref
  case waitingQuestion s of
    Nothing->pure d
    Just _->do
      let next=abandonQuestion reason s
      writeIORef ref next
      paint False next d {chatQuestion=Nothing,status=reason}

-- Provider workers exchange requests through the mailbox; only the editor tick
-- mutates conversation state or presents a permission dialog.
syncConversationAgent :: ConversationState -> IO ()
syncConversationAgent runtime@(ConversationState _ ref _ _) = do
  s<-readIORef ref
  keys<-conversationKeys runtime s
  owner<-traverse (\client->makeStableName =<< evaluate client) (connection s)
  deliveryActive<-AR.primaryDeliveryActive (conversationAgents runtime)
  let key=fromMaybe "" (session s)
      caps=AP.filterPrivateCapabilities keys (AP.parseCapabilities (agentInitialized s) (agentConfig s))
      externallyBusy=busy s && not deliveryActive
      signature=(project s,owner,key,caps,externallyBusy)
  when (lastAgentSync s/=Just signature) $ do
    result<-AR.syncPrimary (conversationAgents runtime) (project s) (connection s) key caps externallyBusy
    case result of
      Left _ -> pure () -- Invalid policy stays fail-closed and can be repaired live.
      Right () -> modifyIORef' ref (\state -> state {lastAgentSync=Just signature})

denyChild :: Approval -> IO ()
denyChild (ChildPermission _ _ reply)=void (tryPutMVar reply Nothing)
denyChild _=pure ()

drainConversationAgents :: ConversationState -> Desktop -> IO Desktop
drainConversationAgents runtime@(ConversationState _ ref _ agents) d=do
  requests<-AR.drainAgentRequests agents
  foldM apply d requests
  where
    apply desktop (AR.DeliverPrimary delivery)=do
      s<-readIORef ref
      case (connection s,session s) of
        (Just client,Just sid) | not (primaryBusy s)->do
          admitted<-AR.admitPrimaryDelivery agents delivery client sid
          case admitted of
            Nothing->pure desktop
            Just msg->do
              let author=case AH.messageAuthor msg of AH.Human -> "Human"; AH.Agent ident -> "Agent "<>AH.agentIdText ident
                  attribution=if AH.messageIsUserSeat msg then "Human message" else author<>" sent a peer message, not the human user seat"
              writeIORef ref (appendPrimary [PrimaryMessage (PeerSpeaker (AH.messageAuthor msg)) (AH.messageText msg)] s
                {queuedPrompt=Just (attribution<>"\n\n"<>AH.messageText msg,Nothing),reads=sourceSnapshots desktop})
              sendQueued runtime desktop
        _->AR.rejectPrimaryDelivery agents delivery "The main conversation is not ready; connect it and retry." >> pure desktop
    apply desktop (AR.ControlPrimary control)=do
      state<-readIORef ref
      current<-AR.primaryControlCurrent agents control (connection state) (session state)
      if not current then AR.rejectPrimaryControl "Primary provider changed or the control was cancelled." control >> pure desktop
      else case control of
        AR.QueryPrimary identity _ _ _ _ text reply
          | Just identity/= (Editor.submissionIdentity <$> (fst =<< M.lookup "" (agentControls state)))->AR.rejectPrimaryControl "Primary input owner expired." control >> pure desktop
          | Just submitted<-fst =<< M.lookup "" (agentControls state)->do
            now<-getCurrentTime
            zone<-getCurrentTimeZone
            let admitted=stampReply now zone state
                queuedInput=any (isQueuedEditor submitted) (queuedQueries state)
            if queuedInput || busy state then do
              let queries=if queuedInput then map (\query->if isQueuedEditor submitted query then SubmittedQuery text else query) (queuedQueries state)
                          else queuedQueries state++[SubmittedQuery text]
                  accepted=appendPrimary [PrimaryMessage UserSpeaker (composerMarkdown text)] admitted {queuedQueries=queries}
              writeIORef ref accepted
              void (tryPutMVar reply (Right ()))
              painted<-paint True accepted desktop
              pure painted {agentQueued=queryCount "" queries,status="Query queued."}
            else submitPrimaryPrompt runtime admitted (Just control) text (False,False,False) desktop
        AR.ConfigurePrimary _ sid [(option,value)] _
          | not (busy state),Just client<-connection state->do
              ident<-A.request client "session/set_config_option" (object ["sessionId" .= sid,"configId" .= option,"value" .= value])
              modifyIORef' ref (\s->s {pending=M.insert ident (Setting control) (pending s)})
              pure desktop {status="Updating conversation settings...",agentReplying=True}
        AR.SteerPrimary _ _ msg _
          | Prompting `elem` M.elems (pending state),not (steeringPending state)->
              beginPromptPreparation ref True (AH.messageText msg) (Just control) desktop
        _->AR.rejectPrimaryControl "The primary turn changed before control admission; draft kept." control >> pure desktop
    apply desktop AR.CancelPrimary=performPrimary runtime "cancel" [] desktop
    apply desktop AR.EndPrimary=do
      cleared<-cancelQuestion runtime "Agent session ended." desktop
      s<-readIORef ref
      mapM_ denyChild (map snd (approvals s))
      _<-retireRequests ref
      mapM_ A.stopClient (connection s)
      AR.failPendingPrimary agents "Agent session ended."
      modifyIORef' ref (\state -> state {connection=Nothing,session=Nothing,streamTails=M.empty,promptReply=Nothing,pending=M.empty,queuedPrompt=Nothing,queuedQueries=childQueries (queuedQueries state),approvals=[],presented=Nothing})
      pure cleared {status="Agent session ended."}
    apply desktop (AR.AgentCreated result)=pure desktop {status=either id (const "Agent created; task queued.") result}
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
showAgentDirectory (ConversationState _ ref _ agents) d=do
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
showAgentHistory runtime@(ConversationState _ ref _ agents) ident d=do
  selected<-AH.statusAgent (AR.agentHub agents) AH.Human ident
  case selected of
    Left err -> pure d {status=err}
    Right entry -> do
      state<-readIORef ref
      -- Remove transient question rendering before its document becomes hidden;
      -- the private answer remains in chatQuestion and returns with Primary.
      primary<-if isNothing (chatQuestion d) then pure d else paint False state d {chatQuestion=Nothing}
      withPrimary<-if M.notMember "" (conversationViews primary) then paint True state primary else pure primary
      let target=AH.agentIdText ident
          name=fromMaybe target (field "name" entry)
      prepared<-ensureConversationEditor runtime target name withPrimary
      let selectedView=selectConversationView target name prepared {chatQuestion=chatQuestion d}
      modifyIORef' ref (\s->s {childRender=Nothing})
      refreshChildConversation runtime selectedView

performChild :: ConversationState -> Text -> [Text] -> Desktop -> IO Desktop
performChild runtime@(ConversationState _ ref _ agents) action values d=do
  state<-readIORef ref
  let target=conversationTarget d
      ident=AH.AgentId target
      hub=AR.agentHub agents
      text=if action=="send" then case values of _:body:_->body; _->"" else contents (composerBuffer d)
  case action of
    "send" | M.member target (agentControls state) -> pure d {status="Wait for the child operation before sending."}
    "send" -> send hub ident text Nothing
    "cancel" -> case M.lookup target (childCancels state) of
      Just _ -> pure d {status="Cancellation requested."}
      Nothing -> do
        worker<-async (AH.cancelAgent hub AH.Human ident)
        modifyIORef' ref (\s->s {childCancels=M.insert target worker (childCancels s),queuedQueries=filter ((/=target).queryTarget) (queuedQueries s)})
        pure d {status="Cancellation requested."}
    "copy" -> pure d {status="Conversation copy requires its presentation owner."}
    "toggle-activity" | [activityId]<-values -> do
      let next=toggleExpansion target (ActivityExpansion activityId) state
      writeIORef ref next
      keepConversationPosition d <$> paintView target False next d
    _ -> pure d {status="Switch to Primary for provider settings or session controls; use Agents to reconnect a child."}
  where
    send hub ident text receipt = do
      result<-AH.sendAgent hub AH.Human ident (composerMarkdown text)
      case result of
        Left err -> pure d {status=err}
        Right _ -> do
          cleared<-clearSubmittedDraft receipt d
          refreshChildConversation runtime cleared {status="Human message queued."}

-- Exact target is independent of the selected conversation. Worker ownership is
-- the same agentControls map polled and retired by the existing conversation.
startAgentControl :: ConversationState -> AH.AgentId -> Maybe DraftReceipt -> IO (Either Text AgentControlResult) -> Desktop -> IO Desktop
startAgentControl (ConversationState _ ref _ agents) ident submitted operation d=mask_ $ do
  state<-readIORef ref
  let target=if ident==AR.primaryAgent agents then "" else AH.agentIdText ident
  if M.member target (agentControls state) then pure d {status="An agent operation is already pending."} else do
    worker<-asyncWithUnmask (\unmask->unmask operation)
    modifyIORef' ref (\current->current {agentControls=M.insert target (submitted,worker) (agentControls current)})
    pure d {agentReplying=agentReplying d || target==conversationTarget d,contextMenu=Nothing,
      status=case Editor.submissionSlot <$> submitted of Nothing->"Updating agent settings..."; Just Editor.DefaultEditor->"Preparing query; draft kept until accepted."; Just Editor.AlternateEditor->"Steering agent; draft kept until accepted."}

refreshChildConversation :: ConversationState -> Desktop -> IO Desktop
refreshChildConversation (ConversationState _ ref _ agents) d=do
  state<-readIORef ref
  controls<-forM (M.toList (agentControls state)) $ \(target,(submitted,worker))->do
    result<-poll worker
    pure (target,submitted,result)
  let controlsDone=[target | (target,_,Just _)<-controls]
      applyControl desktop (target,submitted,Just outcome)=do
        let result=either (const (Left "Agent operation interrupted.")) id outcome
        cleared<-case result of
          Right AgentControlAccepted->clearSubmittedDraft submitted desktop
          Right (ConversationInputAccepted update)->maybe (pure desktop) (\receipt->applyEditorUpdate receipt update desktop) submitted
          Left _->pure desktop
        let notice=either id (const (if isNothing submitted then if T.null target then "Conversation settings updated." else "Child settings updated." else if T.null target then "Follow-up added to the active turn." else "Follow-up added to child's active turn.")) result
        pure $ if target==conversationTarget desktop || isNothing submitted then cleared {status=notice} else cleared
      applyControl desktop _=pure desktop
  controlled<-foldM applyControl d controls
  modifyIORef' ref (\current->current {agentControls=foldr M.delete (agentControls current) controlsDone,
    queuedQueries=foldr (\submitted->filter (not.isQueuedEditor submitted)) (queuedQueries current)
      [submitted | (_,Just submitted,Just _)<-controls]})
  completed<-forM (M.toList (childCancels state)) $ \(target,worker)->do
    result<-poll worker
    pure (target,result)
  let finished=[target | (target,Just _)<-completed]
      cancellation=[either (const "Child cancellation failed.") (either id (const "Child reply cancelled.")) result | (target,Just result)<-completed,target==conversationTarget d]
      original=case cancellation of text:_->controlled {status=text}; _->controlled
  modifyIORef' ref (\s->s {childCancels=foldr M.delete (childCancels s) finished})
  if T.null (conversationTarget original) then do
    -- Controls can complete after the tick's initial projection. Publish their
    -- settled state together with draft adoption, not one UI tick afterward.
    current<-readIORef ref
    pure original {agentReplying=primaryBusy current,agentQueued=queryCount "" (queuedQueries current)}
  else do
    let target=conversationTarget original
        hub=AR.agentHub agents
    selected<-AH.statusAgent hub AH.Human (AH.AgentId target)
    case selected of
      Left err -> pure original {status=err,agentReplying=False,agentQueued=0,childAgentSettings=[],childAgentSteering=False,childAgentContextUsage=Nothing}
      Right entry -> do
        let signature=(target,entry)
            name=fromMaybe target (field "name" entry)
            busyChild=field "status" entry `elem` [Just ("running"::Text),Just "cancelling",Just "starting",Just "configuring"]
            live=field "status" entry `elem` [Just ("idle"::Text),Just "running",Just "cancelling",Just "configuring"]
            capabilities=fromMaybe Null (field "capabilities" entry)
            usage=do value<-field "contextUsage" entry; (,) <$> field "used" value <*> field "size" value
            projected=original {childAgentSettings=if live then parseAgentSettings capabilities else [],
              childAgentSteering=live && field "steering" capabilities==Just True,childAgentContextUsage=if live then usage else Nothing,
              agentReplying=busyChild || M.member target (agentControls state) && target `notElem` controlsDone,agentQueued=fromMaybe 0 (field "queued" entry)+queryCount target (queuedQueries state),
              conversationViews=M.adjust (\v->v {conversationName=name}) target (conversationViews original)}
        current<-readIORef ref
        -- Desktop and Hub checkpoints are independent. Keep a recovered view
        -- until a live/reconnected child owns its transcript again.
        let frozen=M.notMember target (childRecords current) &&
              field "status" entry `elem` [Just ("recovered"::Text),Just "ended"] &&
              maybe False (\logical->case logicalBodyProvider logical of
                RecoveredBodyProvider{}->True; _->False) (conversationLogicalBody target projected)
            missingPresenter=isNothing (conversationPresenter current) && maybe False
              (\view->not (isNothing (conversationLogical view)) || not (isNothing (conversationSource view)))
              (M.lookup target (conversationViews projected))
        if frozen || missingPresenter || childRender current==Just signature then pure projected else do
          history<-recentChildHistory hub (AH.AgentId target) (fromMaybe 1 (field "nextEvent" entry))
          case history of
            Left err -> pure projected {status=err}
            Right (events,dropped) -> do
              let settings=fromMaybe [] (field "capabilities" entry >>= field "configOptions" :: Maybe [Value])
                  models=[category<>": "<>value | option<-settings,Just category<-[field "category" option],Just value<-[field "currentValue" option]]
                  trimmed=fromMaybe (0::Int) (field "nextEvent" entry)>101 || dropped
                  metadata=T.intercalate " · " ([fromMaybe "" (field "status" entry)]++
                    maybe [] (\parent->["parent: "<>parent]) (field "parentName" entry)++models++["recent history" | trimmed])
                  capturedHistory=AgentHistory name metadata (fromMaybe 0 (field "nextEvent" entry)) events
                  records=case conversationPresenter current of
                    Just presenter->TranscriptHistory (Transcript.agentHistory presenter) capturedHistory
                    Nothing->TranscriptRecords [Record (BodyItemId (-1)) 0 (Pause "No agent history presenter is installed.")]
              modifyIORef' ref (\s->s {childRecords=M.insert target records (childRecords s),childRender=Just signature})
              paintView target False current projected

-- The Hub caps each page by bytes as well as count. Follow pages within the
-- captured event range so a large tool event cannot hide the newest reply.
recentChildHistory :: AH.AgentHub -> AH.AgentId -> Int -> IO (Either Text ([AH.HistoryEvent],Bool))
recentChildHistory hub ident next=go (max 0 (next-101)) [] False
  where
    go after accumulated dropped=do
      page<-AH.historyAgent hub AH.Human ident after (100-length accumulated)
      case page of
        Left err -> pure (Left err)
        Right value -> do
          let events=filter ((<next) . AH.historyIndex) (AH.historyEvents value)
              combined=accumulated++events
              omitted=dropped || AH.historyDropped value>0
              cursor=AH.historyNextAfter value
              more=AH.historyHasMore value && cursor<next-1 && length combined<100
          if more && cursor>after then go cursor combined omitted
          else pure (Right (combined,omitted || more))

-- Publish the next human turn before releasing the Hub ticket. Otherwise its
-- worker can dequeue another peer in the gap before the editor's next tick.
completeConversationDelivery :: ConversationState -> Either Text Value -> IO ()
completeConversationDelivery (ConversationState _ ref _ agents) result=do
  s<-atomicModifyIORef' ref (\state->(state {promptReply=Nothing},state))
  void (AR.completePrimaryDelivery agents (connection s) (session s) (busy s || queryCount "" (queuedQueries s)>0) result)

pruneChildApprovals :: ConversationState -> Desktop -> IO Desktop
pruneChildApprovals (ConversationState _ ref _ _) d=do
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
flushConversationChunks :: ConversationState -> IO ()
flushConversationChunks runtime@(ConversationState _ ref _ _)=do
  emitted<-atomicModifyIORef' ref $ \state ->
    ((foldl' (\current (role,text) -> if T.null text then current else recordChunk role text current)
      state (M.toList (streamTails state))) {streamTails=M.empty},M.findWithDefault "" "Agent" (streamTails state))
  publishPrimaryOutput runtime emitted


ensureConversationEditor :: ConversationState -> Text -> Text -> Desktop -> IO Desktop
ensureConversationEditor (ConversationState _ ref _ _) target name d=readIORef ref >>= \s->ensureEditorWithState True s target name d

-- Recovery transfers its existing root directly. Installation drops the seed
-- from the callable binding, leaving precisely one editable Buffer owner.
ensureEditorWithState :: Bool -> State -> Text -> Text -> Desktop -> IO Desktop
ensureEditorWithState opening state target name original=do
  let found=M.lookup target (conversationViews original)
  draftRef<-maybe Editor.newDraftRef (pure.conversationDraftRef) found
  let initial=fromMaybe (EditorDraft (newBuffer "") (Selection 0 0) True Nothing) (M.lookup draftRef (editorDrafts original))
      body=fromMaybe (loadingBody state) (conversationBodySnapshot target original)
      view=maybe (ConversationView (InertBody body) name draftRef Nothing Nothing FollowEnd 0 0 Nothing Nothing Nothing Nothing) (\old->old {conversationName=name}) found
      seeded=original {conversationViews=M.insert target view (conversationViews original),editorDrafts=M.insert draftRef initial (editorDrafts original)}
      -- Live output may resume an existing recovered frame; only explicit Show
      -- creates a frame for a closed or hidden inert body.
      activate=opening || maybe False (\reference->any ((==PluginContent reference).windowContent) (windows original)) (conversationBodyRef view)
      visible next=if opening then showConversationFrame target next else next
      ownsFrame window=maybe False ((==windowContent window).PluginContent) (conversationBodyRef view)
      installBody mount reference prepared desktop=desktop
        {pluginWindows=M.insert reference prepared (maybe (pluginWindows desktop) (`M.delete` pluginWindows desktop) (conversationBodyRef view))
        ,retiredPluginWindows=maybe (retiredPluginWindows desktop) (`S.delete` retiredPluginWindows desktop) (conversationBodyRef view)
        ,windows=map (\window->if ownsFrame window then window {windowContent=PluginContent reference,windowEditorMount=mount} else window) (windows desktop)
        ,conversationViews=M.adjust (\v->v {conversationBody=InstalledBody reference Nothing,conversationEditor=mount}) target (conversationViews desktop)}
  bodyLive<-maybe (pure False) W.windowRefCurrent (conversationBodyRef view)
  let readonly=do
        mapM_ Editor.retireEditorMount (conversationEditor view)
        let detached=seeded
              {conversationViews=M.adjust (\v->v {conversationEditor=Nothing}) target (conversationViews seeded)
              ,editorDrafts=M.adjust (\draft->draft {editorDraftMount=Nothing}) draftRef (editorDrafts seeded)
              ,windows=map (\window->if ownsFrame window then window {windowEditorMount=Nothing} else window) (windows seeded)}
        if bodyLive || not activate then pure (visible detached) else do
          update<-W.openWindow (bodyScope state) body
          admitted<-maybe (pure Nothing) (W.admitWindowUpdate False) update
          pure $ visible $ case admitted of
            Nothing->detached {status="Conversation window expired."}
            Just (reference,prepared)->installBody Nothing reference prepared detached
      attach wrap binding=do
        live<-Editor.mountCurrent (Editor.editorMount binding)
        when (activate && not bodyLive && live) (Editor.retireEditorMount (Editor.editorMount binding))
        nextBinding<-if activate && (not live || not bodyLive) then do
          current<-Editor.editorCurrent binding
          if current && bodyLive then pure binding else Editor.remountEditor binding
          else pure binding
        let mount=Editor.editorMount nextBinding
        opened<-case conversationBodyRef view of
          Just _ | bodyLive->do
            admitted<-if activate && not live then atomically (Editor.claimEditorMount mount) else pure live
            let mounted=if admitted then installEditorDraft mount Nothing seeded else seeded
            pure mounted {conversationViews=M.adjust (\v->v {conversationEditor=if admitted then Just mount else conversationEditor v}) target (conversationViews mounted)}
          _ | activate->do
            update<-W.openEditorWindow (bodyScope state) body nextBinding
            admitted<-maybe (pure Nothing) W.admitEditorWindowUpdate update
            pure $ case admitted of
              Nothing->seeded {status="Conversation window expired."}
              Just (reference,prepared,_)->installBody (Just mount) reference prepared (installEditorDraft mount Nothing seeded)
          _->pure seeded
        modifyIORef' (conversationEditors state) (M.insert target (wrap (Editor.installedEditor nextBinding)))
        pure (visible opened)
      attachBinding (PrimaryEditor editor)=attach PrimaryEditor editor
      attachBinding (ChildEditor editor)=attach ChildEditor editor
  retained<-readIORef (conversationEditors state)
  if (if T.null target then isNothing (primaryInput state) else isNothing (childInput state)) then readonly else case M.lookup target retained of
    Just editor | Editor.mountDraft (conversationEditorMount editor)==draftRef->attachBinding editor
    _ | T.null target,Just registered<-primaryInput state->Editor.attachDeclaredInput registered draftRef >>= attach PrimaryEditor
    _ | Just registered<-childInput state->Editor.attachDeclaredInput registered draftRef >>= attach ChildEditor
    _->readonly

-- Explicit Show is the only path that creates a frame; hidden snapshots remain
-- installed once and retain their own draft. Selection merely switches that frame.
showConversationFrame :: Text -> Desktop -> Desktop
showConversationFrame target desktop=case M.lookup target (conversationViews desktop) >>= conversationBodyRef of
  Nothing->desktop
  Just reference->
    let existing=any (maybe False (const True).conversationTargetFor desktop) (windows desktop)
        prepared=M.lookup reference (pluginWindows desktop)
        opened=if existing then desktop else maybe desktop (\body->addPluginWindow reference body desktop) prepared
    in selectConversationView target (maybe target conversationName (M.lookup target (conversationViews opened))) opened

preparingSteer :: PromptPreparation -> Bool
preparingSteer StartingClient{}=False
preparingSteer (ContextPrompt steering _ _ _)=steering

captureEditorContext :: ConversationState -> Text -> IO (Either Text ChatEditorContext)
captureEditorContext (ConversationState _ ref _ agents) target=do
  state<-readIORef ref
  launch<-makeStableName =<< evaluate (provider state)
  if T.null target then do
    receipt<-case (connection state,session state) of
      (Just client,Just sid)->Just . (`ProviderReceipt` sid) <$> (makeStableName =<< evaluate client)
      _->pure Nothing
    captured<-AH.agentConfiguration (AR.agentHub agents) (AR.primaryAgent agents)
    pure (Right (ChatEditorContext target launch receipt (either (const Nothing) (Just . fst) captured)))
  else do
    captured<-AH.agentConfiguration (AR.agentHub agents) (AH.AgentId target)
    pure $ case captured of
      Left err->Left err
      Right (receipt,_)->Right (ChatEditorContext target launch Nothing (Just receipt))

submitConversationEditor :: ConversationState -> Editor.EditorMount -> Editor.EditorSlot -> Plugin.MenuOrigin -> Desktop -> IO Desktop
submitConversationEditor runtime@(ConversationState _ ref _ _) mount slot origin d
  | origin/=Plugin.HumanMenu || activeEditorMount d/=Just mount || not (composerActive d)=pure d {status="Conversation input expired."}
  | otherwise=mask $ \_->do
      state<-readIORef ref
      editors<-readIORef (conversationEditors state)
      let target=conversationTarget d
      case M.lookup target editors of
        _ | T.null target,isNothing (conversationPresenter state)->pure d {status=unavailableConversation}
        Nothing->pure d {status="Conversation input expired."}
        Just editor | conversationEditorMount editor/=mount->pure d {status="Conversation input expired."}
        Just editor->do
          let pendingDrafts=[submitted | EditorQuery submitted _ _<-queuedQueries state]++
                [submitted | (Just submitted,_)<-M.elems (agentControls state)]
          previous<-or <$> mapM (\submitted->draftCurrent submitted d) (filter ((==Editor.mountDraft mount).Editor.submissionDraft) pendingDrafts)
          if previous then pure d {status="Preparing the submitted draft..."}
          else if M.member target (agentControls state) && (slot/=Editor.DefaultEditor || maybe True ((/=Editor.DefaultEditor).Editor.submissionSlot) (fst =<< M.lookup target (agentControls state)))
            then pure d {status="A conversation input operation is already pending."}
          else if slot==Editor.AlternateEditor && T.null target && (not (agentSteering d) || Prompting `notElem` M.elems (pending state) || steeringPending state)
            then pure d {status="No active turn is available for steering; use Query."}
          else do
            context<-captureEditorContext runtime target
            captured<-Editor.captureDraftSubmission mount slot (composerBuffer d)
            case (context,captured) of
              (Left err,_)->pure d {status=err}
              (_,Nothing)->pure d {status="Conversation input expired."}
              (Right context',Just submitted)
                | T.null target && slot==Editor.DefaultEditor && (primaryBusy state || queryCount "" (queuedQueries state)>0)->do
                    let queued=queuedQueries state++[EditorQuery submitted context' editor]
                    modifyIORef' ref (\s->s {queuedQueries=queued})
                    pure d {status="Query queued for preparation.",agentQueued=queryCount "" queued,agentReplying=True}
                | T.null target,PrimaryEditor primaryEditor<-editor->startPrimaryEditor runtime submitted context' primaryEditor d
                | slot==Editor.DefaultEditor && (M.member target (agentControls state) || queryCount target (queuedQueries state)>0)->do
                    let count=queryCount target (queuedQueries state)
                    if count>=32 then pure d {status="Agent message queue is full."} else do
                      modifyIORef' ref (\s->s {queuedQueries=queuedQueries s++[EditorQuery submitted context' editor]})
                      pure d {status="Human query queued for preparation.",agentQueued=count+1}
                | ChildEditor childEditor<-editor->startChildEditor runtime submitted context' childEditor d
                | otherwise->pure d {status="Conversation input expired."}

queryTarget :: QueuedQuery -> Text
queryTarget (EditorQuery _ (ChatEditorContext target _ _ _) _)=target
queryTarget _=""

queryCount :: Text -> [QueuedQuery] -> Int
queryCount target=length.filter ((==target).queryTarget)

childQueries :: [QueuedQuery] -> [QueuedQuery]
childQueries=filter (not.T.null.queryTarget)

startPrimaryEditor :: ConversationState -> DraftReceipt -> ChatEditorContext -> Editor.PreparedEditor PrimaryInputServices Editor.EditorUpdate -> Desktop -> IO Desktop
startPrimaryEditor runtime@(ConversationState _ _ _ agents) submitted (ChatEditorContext _ launch provider expected) editor=
  startAgentControl runtime (AR.primaryAgent agents) (Just submitted) $ do
    slot<-evaluate (Editor.submissionSlot submitted)
    identity<-evaluate (Editor.submissionIdentity submitted)
    liveCall<-newMVar True
    let services=PrimaryInputServices $ \text->withMVar liveCall $ \activeCall->
          if not activeCall then pure (Left "Primary input invocation expired.") else case slot of
            Editor.DefaultEditor->AR.requestPrimaryQuery agents identity launch (fmap (\(ProviderReceipt owner sid)->(owner,sid)) provider)
              (if isNothing provider then Nothing else expected) text
            Editor.AlternateEditor->case expected of
              Nothing->pure (Left "Primary target expired; draft kept.")
              Just receipt->fmap (fmap (const ())) (AH.steerAgentAt (AR.agentHub agents) receipt text)
    result<-Editor.invokeEditorAction editor services submitted
      `finally` modifyMVar_ liveCall (const (pure False))
    pure (either (Left . T.pack . show) (Right . ConversationInputAccepted) result)

startChildEditor :: ConversationState -> DraftReceipt -> ChatEditorContext -> Editor.PreparedEditor ChildInputServices Editor.EditorUpdate -> Desktop -> IO Desktop
startChildEditor runtime@(ConversationState _ _ _ agents) submitted (ChatEditorContext target _ _ expected) editor=
  startAgentControl runtime (AH.AgentId target) (Just submitted) $ do
    slot<-evaluate (Editor.submissionSlot submitted)
    liveCall<-newMVar True
    let services=ChildInputServices $ \text->withMVar liveCall $ \activeCall->
          if not activeCall then pure (Left "Child input invocation expired.") else case expected of
            Nothing->pure (Left "Child target expired.")
            Just receipt->case slot of
              Editor.DefaultEditor->fmap (fmap (const ())) (AH.sendAgentAt (AR.agentHub agents) AH.Human receipt (composerMarkdown text))
              Editor.AlternateEditor->fmap (fmap (const ())) (AH.steerAgentAt (AR.agentHub agents) receipt (composerMarkdown text))
    -- The captured Hub receipt performs atomic admission. Revocation shares the
    -- call gate, so an admitted provider call drains and escaped calls reject.
    result<-Editor.invokeEditorAction editor services submitted
      `finally` modifyMVar_ liveCall (const (pure False))
    pure (either (Left . T.pack . show) (Right . ConversationInputAccepted) result)

-- Transfer at most one queued child intent into its existing worker slot. It
-- does not wait for, or consume, the independent primary prompt preparation.
pollQueuedChildEditor :: ConversationState -> Desktop -> IO Desktop
pollQueuedChildEditor runtime@(ConversationState _ ref _ _) d=mask_ $ do
  state<-readIORef ref
  case [(submitted,context,editor) | EditorQuery submitted context@(ChatEditorContext target _ _ _) (ChildEditor editor)<-queuedQueries state,
        not (T.null target),M.notMember target (agentControls state),M.notMember target (childCancels state)] of
    (submitted,context,editor):_->do
      modifyIORef' ref (\s->s {queuedQueries=filter (not.isQueuedEditor submitted) (queuedQueries s)})
      startChildEditor runtime submitted context editor d
    []->pure d

-- Prepare queued primary submissions on their existing control worker even
-- while a turn runs. Admission replaces that exact queue entry in place; only
-- the protocol/preparation owner decides when its accepted text reaches ACP.
pollQueuedPrimaryEditor :: ConversationState -> State -> Desktop -> IO Desktop
pollQueuedPrimaryEditor runtime state d
  | M.member "" (agentControls state)=pure d
  | otherwise=case [(submitted,captured,editor) | EditorQuery submitted captured@(ChatEditorContext target _ _ _) (PrimaryEditor editor)<-queuedQueries state,T.null target] of
      (submitted,captured,editor):_->startPrimaryEditor runtime submitted captured editor d
      []->pure d

isQueuedEditor :: DraftReceipt -> QueuedQuery -> Bool
isQueuedEditor submitted (EditorQuery actual _ _)=submitted==actual
isQueuedEditor _ _=False
