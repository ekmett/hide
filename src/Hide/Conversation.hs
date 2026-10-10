-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE CPP, ExistentialQuantification, GADTs, ScopedTypeVariables, OverloadedStrings #-}
-- | Module      : Hide.Conversation
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : CPP, ExistentialQuantification, GADTs, ScopedTypeVariables, OverloadedStrings
--
-- Primary ACP ownership and captured plugin transcript sources.
--
-- The session tick consumes typed provider and hub mailbox events. Owned workers prepare
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
module Hide.Conversation (ConversationState, conversationAgents, captureAgentSettings, captureConversationSession, captureConversationOperation, captureConversationChoices, withConversationAt, QuestionCaller, captureQuestionCaller, questionServices, applyQuestion, withConversation, conversationEffects, tickConversation, conversationBodyRequests, adoptConversationBodies, renderReply, pauseLabel, renderTimestamp) where

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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Time (UTCTime, TimeZone, getCurrentTime, getCurrentTimeZone, diffUTCTime, utcToLocalTime, formatTime, defaultTimeLocale)
import Data.IORef
import Data.Unique (Unique,newUnique)
import Hide.ConversationSessionTypes
import qualified Hide.Plugin.ConversationSession as Session
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (XdgDirectory(..), getXdgDirectory, canonicalizePath, getCurrentDirectory)
import System.FilePath ((</>), isAbsolute, makeRelative, splitDirectories)
import qualified Hide.Terminal as Terminal
import qualified Hide.Consoles as C
import qualified Hide.Build as B
import System.Mem.StableName (StableName, makeStableName)
import Text.Read (readMaybe)
import Hide.Plugin.Provider
import Hide.Plugin.Agent (StartAgentProvider,ProviderKind(..),ProviderContent(..),ProviderPermission(..),ProviderTerminal(..),ProviderTerminalOutput(..))
import Hide.GuestAccess (sensitiveLabel, protectedPath, protectedBuffer)
import Hide.Files (filePath)
import qualified Hide.Files as Files
import Hide.MCPPermissions (permissionConfigPath, projectConfigPath, readAgentContextAt, writeAgentContextAt, readAgentContexts)
import qualified Hide.AgentRuntime as AR
import qualified Hide.AgentHub as AH
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
data Phase = Prompting | CancellingPrompt deriving Eq
-- A human submission can consume only its captured immutable draft. Stable
-- names in ContentVersion retain no Buffer/Undo; selection/focus are independent.
type DraftReceipt = Editor.DraftSubmission
-- Immutable input context captured at the original human input turn.
data ChatEditorContext = ChatEditorContext !Text !(StableName ProviderLaunch) !(Maybe ProviderReceipt) !(Maybe AH.AgentConfigRef)
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
  = StartingClient !(Maybe Text) !(Async (Either Text (FilePath,AH.AgentDriver)))
  | ContextPrompt !Bool !Text !(Maybe AR.PrimaryControl) !(Async (Either Text ([Text],Value)))
  | SendingPrompt !ProviderTurnId !ProviderSubmission !Text !Value !(Maybe AR.PrimaryControl) !(Async (Either Text ProviderTurn))
  | SteeringPrompt !ProviderSubmission !Text !Value !AR.PrimaryControl !(Async (Either Text Value))
  | PreparingHuman !ConversationSessionReceipt !(Async (Either Text HumanResult))
  | Configuring !AR.PrimaryControl !(Async (Either Text AH.Capabilities))
preparationCancel :: PromptPreparation -> IO ()
preparationCancel (StartingClient _ worker)=do
  cancel worker
  completed<-poll worker
  case completed of Just (Right (Right (_,driver)))->AH.driverStop driver; _->pure ()
preparationCancel (ContextPrompt _ _ _ worker)=cancel worker
preparationCancel (SendingPrompt _ submission _ _ _ worker)=do
  retireProviderSubmission submission
  cancel worker
  completed<-poll worker
  case completed of Just (Right (Right turn))->cancelProviderTurn turn; _->pure ()
preparationCancel (SteeringPrompt submission _ _ _ worker)=retireProviderSubmission submission >> cancel worker
preparationCancel (PreparingHuman _ worker)=cancel worker
preparationCancel (Configuring _ worker)=cancel worker


unavailableConversation :: Text
unavailableConversation="Agent conversation plugin is unavailable; draft kept."

toggleExpansion :: Text -> ToolExpansion -> State -> State
toggleExpansion target item state=state {toolExpansions=if S.member key expanded then S.delete key expanded else S.insert key expanded}
  where key=(target,item); expanded=toolExpansions state
data Approval
  = ChildPermission AH.AgentId ProviderPermission (MVar (Either Text (Maybe Text)))
  | Permission !ProviderIdentity !ProviderPermission (MVar (Either Text (Maybe Text)))
  | Write !ProviderIdentity (MVar (Either Text ())) !Snapshot !Text
  | Execute !ProviderIdentity (MVar (Either Text Text)) !Terminal.TerminalConfig !Int

data FileRequest
  = ReadFile !Int !(Maybe Int) (MVar (Either Text Text))
  | WriteFile !Text (MVar (Either Text ()))
data FileCapture
  = ResolvingFile !ProviderIdentity !FileRequest !(Maybe SourceIdentity) !(Async (Either Text ResolvedFile))
  | ReadingFile !ProviderIdentity !FileRequest !(Maybe SourceIdentity) !(Async (Either Text Snapshot))
  | SlicingFile !ProviderIdentity (MVar (Either Text Text)) !Snapshot !(Maybe SourceIdentity) !(Async (Either Text Text))
  | CheckingTerminal !ProviderIdentity (MVar (Either Text Text)) !Int !(Async (Either Text Terminal.TerminalConfig))
  | PreparingTerminal !ProviderIdentity (MVar (Either Text Text)) !(Async (Either Text C.PreparedConsole))
  | forall a. TerminalOperation !ProviderIdentity (MVar (Either Text a)) !(Async (Either Text a))

captureCancel :: FileCapture -> IO ()
captureCancel (ResolvingFile _ request _ worker)=finishFile (Left "File request cancelled.") request >> cancel worker
captureCancel (ReadingFile _ request _ worker)=finishFile (Left "File request cancelled.") request >> cancel worker
captureCancel (SlicingFile _ reply _ _ worker)=void (tryPutMVar reply (Left "File request cancelled.")) >> cancel worker
captureCancel (CheckingTerminal _ reply _ worker)=void (tryPutMVar reply (Left "Terminal request cancelled.")) >> cancel worker
captureCancel (PreparingTerminal _ reply worker)=do
  void (tryPutMVar reply (Left "Terminal request cancelled."))
  cancel worker
  ready<-poll worker
  case ready of Just (Right (Right console))->C.closePreparedConsole console; _->pure ()
captureCancel (TerminalOperation _ reply worker)=void (tryPutMVar reply (Left "Terminal request cancelled.")) >> cancel worker

fileWaiting :: FileRequest -> IO Bool
fileWaiting (ReadFile _ _ reply)=isEmptyMVar reply
fileWaiting (WriteFile _ reply)=isEmptyMVar reply
finishFile :: Either Text () -> FileRequest -> IO ()
finishFile (Left err) (ReadFile _ _ reply)=void (tryPutMVar reply (Left err))
finishFile (Left err) (WriteFile _ reply)=void (tryPutMVar reply (Left err))
finishFile _ _=pure ()

-- Fixed answer delivery reuses the ordinary query queue. Its receipt identifies
-- the exact provider incarnation, not a reusable session label alone.
data ProviderReceipt = ProviderReceipt !(ProviderIdentity) !Text
-- Host-minted before any permission wait; extension arguments cannot forge it.
data QuestionCaller = QuestionCaller !(StableName (IORef State)) !AH.AgentId !(Maybe ProviderReceipt)
data QueuedQuery = SubmittedQuery !Text | QuestionQuery !Int !AH.AgentId !ProviderReceipt !Text
  | EditorQuery !Editor.DraftSubmission !ChatEditorContext !ConversationEditor
data QuestionTicket = QuestionTicket !Int !AH.AgentId !(Maybe ProviderReceipt)
data QuestionResult = QuestionResult !AH.AgentId !(Maybe ProviderReceipt) !Value

-- Reversed safe chunks give O(1) append and one concatenation on completion.
-- The request ID scopes output to the provider prompt, never a displayed record.

data SettingsCapture = SettingsCapture !Bool !FilePath !Settings.SettingsSnapshot

data State = State
  { sessionIdentity :: !Unique
  , provider :: ProviderLaunch, connection :: Maybe AH.AgentDriver, connectionIdentity :: Maybe ProviderIdentity, providerFactory :: Maybe StartAgentProvider,
    currentTurn :: Maybe ProviderTurnId, completedTurn :: Maybe ProviderTurnId, activeTurn :: Maybe ProviderTurn, queuedDelivery :: Maybe AR.PrimaryDelivery, session :: Maybe Text, project :: FilePath
  , pending :: M.Map Int Phase, queuedPrompt :: Maybe (Text,Maybe AR.PrimaryControl), transcript :: !Transcript.PrimaryTranscript, nextRecord :: !Int
  , reads :: M.Map FilePath Snapshot, approvals :: [(Int,Approval)], presented :: Maybe Int, deferredApproval :: Bool, nextApproval :: Int
  , queuedQueries :: [QueuedQuery]
  , ownedTerminals :: S.Set Text
  , terminalWaiters :: M.Map Text [MVar (Either Text Int)]
  , terminalPoll :: Maybe (Async [(Text,Either Text (BS.ByteString,Bool,Maybe Int))])
  , lastMessageAt :: Maybe UTCTime
  , lastSession :: Maybe (ProviderLaunch,FilePath,Text)
  , waitingQuestion :: Maybe QuestionTicket, questionResults :: M.Map Int QuestionResult, questionsClosed :: Bool, lastQuestion :: Maybe Int, lastQuestionInteraction :: Maybe (StableName ChatQuestion)
  , deliveredContext :: Maybe Value
  , directoryAgents :: [AH.AgentId]
  , agentCapabilities :: AH.Capabilities
  , lastAgentSync :: Maybe (FilePath,Maybe (ProviderIdentity),Text,AH.Capabilities,Bool)
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

defaultLaunch :: ProviderLaunch
defaultLaunch = ProviderLaunch "codex-acp" [] []

withConversation :: Maybe StartAgentProvider -> Maybe ConversationPresenter -> Maybe (InputDeclaration PrimaryInputServices) -> Maybe (InputDeclaration ChildInputServices) -> C.Consoles -> (ConversationState -> IO a) -> IO a
withConversation factory presenter primary child consoles action = getCurrentDirectory >>= \root -> withConversationAt factory presenter primary child consoles root action

-- | Load conversation configuration and scope only provider and agent workers.
withConversationAt :: Maybe StartAgentProvider -> Maybe ConversationPresenter -> Maybe (InputDeclaration PrimaryInputServices) -> Maybe (InputDeclaration ChildInputServices) -> C.Consoles -> FilePath -> (ConversationState -> IO a) -> IO a
withConversationAt factory presenter primary child consoles root action = W.withWindowScope $ \scope->Command.withRegistry $ \registry->Command.withRegistry $ \childRegistry->Command.withRegistry $ \settingsRegistry->do
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
    { sessionIdentity=identity,provider=launch,connection=Nothing,connectionIdentity=Nothing,providerFactory=factory,currentTurn=Nothing,completedTurn=Nothing,activeTurn=Nothing,queuedDelivery=Nothing,session=Nothing,project=root
    , pending=M.empty,queuedPrompt=Nothing,transcript=Transcript.emptyPrimaryTranscript,nextRecord=0,reads=M.empty
    , approvals=[],presented=Nothing,deferredApproval=False,nextApproval=1,queuedQueries=[]
    , ownedTerminals=S.empty,terminalWaiters=M.empty,terminalPoll=Nothing,lastMessageAt=Nothing
    , lastSession=remembered,waitingQuestion=Nothing,questionResults=M.empty,questionsClosed=False,lastQuestion=Nothing,lastQuestionInteraction=Nothing
    , deliveredContext=Nothing,fileCaptures=[],retiringRequests=[],promptPreparation=Nothing,settingsCommands=(settingsRegistry,settingsRead),resumeRecordPath=resumePath,directoryAgents=[]
    , conversationPresenter=preparedPresenter,agentCapabilities=AH.Capabilities False False False [],lastAgentSync=Nothing,childRecords=M.empty,childRender=Nothing,childCancels=M.empty,agentControls=M.empty,toolExpansions=S.empty,primaryInput=registeredPrimaryInput,childInput=registeredChildInput,conversationEditors=editors,bodyScope=scope,loadingBody=loading }
  AR.withAgentRuntime factory root (provider <$> readIORef ref) $ \agents ->
    bracket (pure (ConversationState directory ref consoles agents)) closeConversation $ \runtime->do
      -- Publish the acquired initial owner before any menu can capture its
      -- configuration receipt; the first tick must not replace a placeholder.
      syncConversationAgent runtime
      action runtime

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
  writeIORef ref (abandonQuestion "Editor session closed." s) {questionsClosed=True}
  AR.failPendingPrimary agents "Editor session closed."
  mapM_ denyChild (map snd (approvals s))
  mapM_ preparationCancel (promptPreparation s)
  readIORef (conversationEditors s) >>= mapM_ (Editor.retireDraftRef . Editor.mountDraft . conversationEditorMount)
  mapM_ captureCancel (fileCaptures s)
  mapM_ (wait . snd) (retiringRequests s)
  mapM_ cancel (childCancels s)
  mapM_ (cancel . snd) (agentControls s)
  mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
  mapM_ cancel (terminalPoll s)
  mapM_ AH.driverStop (connection s)

launchValue :: ProviderLaunch -> Value
launchValue launch=object ["executable" .= executable launch,"arguments" .= arguments launch,"environment" .= M.fromList (environment launch)]

decodeLaunch :: BS.ByteString -> Either String ProviderLaunch
decodeLaunch bytes=do
  value<-eitherDecodeStrict' bytes
  maybe (Left "Expected executable, arguments array and environment object.") Right (parseMaybe (withObject "agent" $ \o ->
    ProviderLaunch <$> o .: "executable" <*> o .:? "arguments" .!= [] <*> (M.toList <$> (o .:? "environment" .!= M.empty))) value) >>= validateLaunch

validateLaunch :: ProviderLaunch -> Either String ProviderLaunch
validateLaunch launch
  | null (executable launch) = Left "Enter an executable."
  | any (elem '\0') (executable launch:arguments launch++concatMap (\(k,v)->[k,v]) (environment launch)) = Left "NUL bytes are not valid in process arguments."
  | any (\(key,_) -> null key || '=' `elem` key) (environment launch) = Left "Invalid environment variable name."
  | otherwise = Right launch

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
performPrimary runtime@(ConversationState _ ref consoles _) action values original = do
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
    ("show",_) -> do
      modifyIORef' ref (\state -> state {deferredApproval=False})
      -- A recovered transcript belongs to the checkpoint until a provider
      -- connects. Opening its window must not repaint it from empty state.
      ensureEditorWithState True s "" "Primary" d
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
      mapM_ (cancelApproval . snd) (filter (not . child) (approvals s))
      forM_ (promptPreparation s) $ \preparation->case preparation of
        SendingPrompt _ submission _ _ _ _->retireProviderSubmission submission
        SteeringPrompt submission _ _ _ _->retireProviderSubmission submission
        _->pure ()
      mapM_ (AR.retireProviderCalls (conversationAgents runtime)) (connectionIdentity s)
      case activeTurn s of
        Just turn->cancelProviderTurn turn
        Nothing->mapM_ AH.driverCancel (connection s)
      retired<-retireRequests ref
      writeIORef ref retired {pending=M.map (\phase->if phase==Prompting then CancellingPrompt else phase) (pending retired),queuedPrompt=Nothing,approvals=retained,presented=if keepDialog then presented s else Nothing,deferredApproval=False}
      -- Context preparation has no provider prompt whose response could release
      -- an admitted Hub receipt. Keep the barrier only after actual prompt IO.
      unless (any isPrompt (M.elems (pending retired))) $
        completeConversationDelivery runtime (Left "Agent prompt cancelled.")
      pure (if keepDialog then d else dismissPermission d) {status="Cancellation requested."}
    _ | Just suffix<-T.stripPrefix "approval:" action, Just token<-readMaybe (T.unpack suffix) -> decide runtime token values d
    _ -> pure d

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
      let client=connectionIdentity s
      config<-either (const Nothing) (Just . fst) <$> AH.agentConfiguration (AR.agentHub agents) (AR.primaryAgent agents) >>= traverse evaluate
      sid<-traverse evaluate (session s)
      receipt<-evaluate (ConversationSessionReceipt (sessionIdentity s) (AR.primaryAgent agents) launch client sid config)
      target<-evaluate (Session.ConversationTarget receipt (T.null (conversationTarget d)) (maybe "" (\(_,_,value)->value) (lastSession s)))
      pure (Right target)

-- | Human operation capture retains only small owner keys, private launch
-- prefill and an immutable transcript read handle for the selected view.
captureConversationOperation :: ConversationState -> Desktop -> IO (Either Text (Session.ConversationOperationTarget ConversationSessionReceipt))
captureConversationOperation (ConversationState _ ref _ _) d=do
  s<-readIORef ref
  if questionsClosed s then pure (Left unavailableConversation) else if maybe False humanPreparation (promptPreparation s) then pure (Left "A conversation options operation is pending.") else do
    launch<-evaluate (provider s)
    identity<-makeStableName launch
    let target=T.copy (conversationTarget d)
        root=if isNothing (connection s) then startingDirectory d else project s
        serial=fst (clipboardExport d)+1
        copy=do
          view<-M.lookup target (conversationViews d)
          reference<-conversationBodyRef view
          case conversationSource view of
            Just source->Just (ConversationTranscriptCopy reference target source serial)
            Nothing->(\logical->ConversationLogicalCopy reference target logical serial) <$> conversationLogical view
    _<-evaluate (T.length target+length root+length (executable launch)+sum (map length (arguments launch))+sum [length name+length value | (name,value)<-environment launch])
    copied<-traverse evaluate copy
    sid<-traverse evaluate (session s)
    receipt<-evaluate (ConversationOperationReceipt (sessionIdentity s) identity (connectionIdentity s) sid root target copied)
    captured<-evaluate (Session.ConversationOperationTarget receipt launch (not (primaryBusy s)))
    pure (Right captured)

humanPreparation :: PromptPreparation -> Bool
humanPreparation PreparingHuman{}=True
humanPreparation _=False

operationOwnerCurrent :: ConversationSessionReceipt -> State -> IO Bool
operationOwnerCurrent (ConversationOperationReceipt owner expectedLaunch expectedClient expectedSession _ _ _) s=do
  launch<-makeStableName =<< evaluate (provider s)
  pure (not (questionsClosed s) && owner==sessionIdentity s && expectedLaunch==launch && expectedClient==connectionIdentity s && expectedSession==session s)
operationOwnerCurrent _ _=pure False

operationCurrent :: ConversationSessionReceipt -> State -> Desktop -> IO Bool
operationCurrent receipt@(ConversationOperationReceipt _ _ _ _ _ target _) s d=do
  current<-operationOwnerCurrent receipt s
  pure (current && target==conversationTarget d && dialog d==Nothing)
operationCurrent _ _ _=pure False

data HumanResult = HumanProvider !ProviderLaunch | HumanContext !FilePath !Document

prepareHuman :: FilePath -> FilePath -> Session.ConversationRequest ConversationSessionReceipt -> IO (Either Text HumanResult)
prepareHuman directory root request=case request of
  Session.ConfigureConversation _ launch->case validateLaunch launch of
    Left err->pure (Left (T.pack err))
    Right checked->fmap (HumanProvider checked <$) (persist (directory </> "agents.json") (launchValue checked))
  Session.OpenConversationContext _ scope->do
    base<-B.resolveBuildRootFrom root
    path<-if scope==Session.GlobalContext then permissionConfigPath else projectConfigPath base
    loaded<-readAgentContextAt path
    saved<-case loaded of Left err->pure (Left err);Right text->writeAgentContextAt path text
    case saved of
      Left err->pure (Left err)
      Right ()->do
        opened<-Files.loadFile path
        case opened of
          Left err->pure (Left (T.pack err))
          Right (file,buffer)->do
            doc<-evaluate ((newDocument buffer (Just file)) {documentPrivate=True})
            pure (Right (HumanContext (filePath file) doc))
  _->pure (Left "Unsupported prepared human operation.")

applyConversationOperation :: ConversationState -> Session.ConversationRequest ConversationSessionReceipt -> Desktop -> IO Desktop
applyConversationOperation runtime@(ConversationState directory ref _ _) request d=do
  s<-readIORef ref
  let receipt=case request of Session.NewConversation r->r;Session.ResumeConversation r _->r;Session.OpenConversation r->r;Session.ConfigureConversation r _->r;Session.OpenConversationContext r _->r;Session.CopyRawConversation r->r
  current<-operationCurrent receipt s d
  if not current then pure d {status="Conversation operation expired; invoke it again."} else case request of
    Session.OpenConversation (ConversationOperationReceipt _ _ _ _ _ target _)
      | Just window<-find (\w->conversationTargetFor d w==Just target) (windows d)->do
          -- Opening an already installed view only restores its input focus;
          -- retain the original body and draft rather than refreshing history.
          modifyIORef' ref (\state->state {deferredApproval=False})
          let focused=focusWindow (windowId window) d
          pure (setComposerInput (composerBuffer focused) (composerSelection focused) True focused)
      | not (T.null target)->showAgentHistory runtime (AH.AgentId target) d
    Session.OpenConversation _->do
      shown<-ensureConversationEditor runtime "" "Primary" d
      ensureEditorWithState True s "" "Primary" (selectConversationView "" "Primary" shown)
    Session.CopyRawConversation _->pure d {status="Conversation copy requires its presentation owner."}
    Session.ConfigureConversation{} | primaryBusy s->pure d {status="Cancel the current reply before changing agent configuration."}
    _ | not (isNothing (promptPreparation s))->pure d {status="Wait for the current preparation before changing conversation options."}
      | ConversationOperationReceipt _ _ _ _ root _ _<-receipt->do
          -- This owner check admits the exact captured filesystem operation.
          -- The single preparation slot prevents another options operation from
          -- replacing it. Provider adoption follows this owner; only context
          -- presentation also follows the original focus.
          worker<-async (prepareHuman directory root request)
          modifyIORef' ref (\state->state {promptPreparation=Just (PreparingHuman receipt worker)})
          pure d {status="Preparing conversation options…"}
      | otherwise->pure d

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
applyConversationSession runtime request original=case request of
  Session.NewConversation receipt->changeConversationSession runtime receipt Nothing original
  Session.ResumeConversation receipt sid->changeConversationSession runtime receipt (Just sid) original
  _->applyConversationOperation runtime request original

changeConversationSession :: ConversationState -> ConversationSessionReceipt -> Maybe Text -> Desktop -> IO Desktop
changeConversationSession runtime@(ConversationState _ ref _ agents)
    (ConversationSessionReceipt owner primary expectedLaunch expectedClient expectedSession expectedConfig) resume original=do
  s<-readIORef ref
  launch<-makeStableName =<< evaluate (provider s)
  let client=connectionIdentity s
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
      retired<-retireProviderResources runtime "Agent session changed."
      writeIORef ref retired {connection=Nothing,connectionIdentity=Nothing,currentTurn=Nothing,completedTurn=Nothing,activeTurn=Nothing,queuedDelivery=Nothing,session=Nothing,pending=M.empty,queuedPrompt=Nothing,queuedQueries=childQueries (queuedQueries retired),transcript=Transcript.emptyPrimaryTranscript,toolExpansions=S.filter ((/="").fst) (toolExpansions s),lastMessageAt=Nothing,reads=maybe M.empty (const (sourceSnapshots d)) resume,approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty}
      start runtime resume d

changeConversationSession _ _ _ original=pure original {status="Invalid conversation session receipt."}

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

steeringPending :: State -> Bool
steeringPending s=maybe False preparingSteer (promptPreparation s)

busy :: State -> Bool
busy s=not (M.null (pending s)) || not (isNothing (queuedPrompt s)) || not (isNothing (promptPreparation s))

-- Public input stays pending across the worker-to-provider mailbox gap. Hub
-- admission uses protocol busy state so a control cannot block its own arrival.
primaryBusy :: State -> Bool
primaryBusy s=busy s || M.member "" (agentControls s)

start :: ConversationState -> Maybe Text -> Desktop -> IO Desktop
start (ConversationState _ ref _ agents) resume d=mask_ $ do
  s<-readIORef ref
  case providerFactory s of
    Nothing->pure d {status="Agent provider plugin is unavailable; draft kept."}
    Just _ | isNothing (conversationPresenter s) || isNothing (primaryInput s)->pure d {status=unavailableConversation}
    Just acquire->do
      let (selectedLaunch,selectedDirectory)=case (resume,lastSession s) of
            (Just wanted,Just (savedProvider,savedDirectory,savedId)) | wanted==savedId->(savedProvider,savedDirectory)
            _->(provider s,maybe (startingDirectory d) treeRoot (sideTree d))
      launch<-evaluate (forceLaunch selectedLaunch)
      directory<-evaluate (force selectedDirectory)
      identity<-newProviderIdentity
      worker<-asyncWithUnmask $ \unmask->do
        root<-unmask (canonicalizePath directory)
        endpoints<-AR.primaryServers agents
        let request=AH.StartRequest (AR.primaryAgent agents) AH.Human
              (AH.SpawnSpec "Primary" "Conversation" root AH.Shared AH.Fresh Nothing Nothing) Nothing resume
        acquired<-acquire PrimaryProvider identity launch endpoints "" (AR.primaryProviderHost agents identity root)
          request (AR.publishProviderEvent agents identity)
        pure ((\driver->(root,driver)) <$> acquired)
      writeIORef ref s {provider=launch,connectionIdentity=Just identity,promptPreparation=Just (StartingClient resume worker)}
      pure d {status="Connecting to agent provider...",agentSteering=False,agentReplying=True,agentContextUsage=Nothing,agentSettings=[]}
  where
    forceLaunch launch=force (executable launch,arguments launch,environment launch) `seq` launch

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
pollPromptPreparation runtime@(ConversationState _ ref _ agents) d=do
  s<-readIORef ref
  case promptPreparation s of
    Nothing->if isNothing (queuedPrompt s) then pollQueuedPrimaryEditor runtime s d else sendQueued runtime d
    Just (StartingClient _ worker)->poll worker >>= \ready->case ready of
      Nothing->pure d
      Just result->case either (const (Left "Could not start the primary provider.")) id result of
        Left err->do
          mapM_ (AR.rejectPrimaryControl err) (snd =<< queuedPrompt s)
          mapM_ (AR.retireProviderCalls agents) (connectionIdentity s)
          writeIORef ref s {promptPreparation=Nothing,queuedPrompt=Nothing,connectionIdentity=Nothing}
          completeConversationDelivery runtime (Left err)
          pure (message "Cannot start agent" (wrapMessage err) d {status=err,agentReplying=False})
        Right (root,driver)->do
          let sid=AH.driverSessionKey driver; caps=AH.driverCapabilities driver
          writeIORef ref s {connection=Just driver,session=Just sid,project=root,promptPreparation=Nothing,
            deliveredContext=Nothing,agentCapabilities=caps,lastAgentSync=Nothing,
            lastSession=Just (provider s,root,sid)}
          saved<-persist (resumeRecordPath s) (object ["provider" .= launchValue (provider s),"cwd" .= root,"sessionId" .= sid])
          syncConversationAgent runtime
          sendQueued runtime d {agentSettings=publicAgentSettings caps,agentSteering=AH.supportsSteering caps,
            status=either ("Session opened; could not save ID: "<>) (const "Conversation ready.") saved}
    Just (ContextPrompt steering text control worker)->poll worker >>= \ready->case ready of
      Nothing->pure d
      Just result->do
        modifyIORef' ref (\state->state {promptPreparation=Nothing})
        let prepared=either (const (Left "Could not prepare agent context.")) id result
        current<-maybe (pure True) (\request->AR.primaryControlCurrent agents request (connectionIdentity s) (session s)) control
        case (connection s,prepared) of
          (Just driver,Right (query:extra,context)) | current && (not steering || Prompting `elem` M.elems (pending s))->mask_ $ do
            submission<-case (steering,control,queuedDelivery s) of
              (True,Just (AR.SteerPrimary _ _ _ original _),_)->pure original
              (False,_,Just delivery)->pure (AR.deliverySubmission delivery)
              _->newProviderSubmission
            if steering then case control of
              Just request->do
                sending<-async (AH.driverSteer driver (AH.HubMessage 0 AH.Human query True) extra submission)
                modifyIORef' ref (\state->state {promptPreparation=Just (SteeringPrompt submission text context request sending)})
                pure d {status="Steering request sent; draft kept until accepted."}
              Nothing->retireProviderSubmission submission >> pure d
            else do
              turn<-maybe newProviderTurnId (pure . AR.deliveryTurn) (queuedDelivery s)
              sending<-async (AH.driverDeliver driver turn (AH.HubMessage 0 AH.Human query True) extra submission)
              modifyIORef' ref (\state->state {promptPreparation=Just (SendingPrompt turn submission text context control sending),
                currentTurn=Just turn,completedTurn=Nothing})
              pure d {status="Sending agent query...",agentReplying=True}
          _->do
            let err=either id (const "Agent control expired; draft kept.") prepared
            mapM_ (AR.rejectPrimaryControl err) control
            unless steering $ do
              modifyIORef' ref (\state->state {queuedPrompt=Nothing})
              completeConversationDelivery runtime (Left err)
            pure (if steering || not (isNothing control) then d else restoreEmptyPrimaryDraft text d) {status=err}
    Just (SendingPrompt turn submission text context control worker)->poll worker >>= \ready->case ready of
      Nothing->pure d
      Just result->do
        retireProviderSubmission submission
        case either (const (Left "Provider send interrupted; draft kept.")) id result of
          Left err->do
            mapM_ (AR.rejectPrimaryControl err) control
            modifyIORef' ref (\state->state {promptPreparation=Nothing,queuedPrompt=Nothing,currentTurn=Nothing,completedTurn=Nothing,activeTurn=Nothing})
            completeConversationDelivery runtime (Left err)
            pure (if isNothing control then restoreEmptyPrimaryDraft text d else d) {status=err}
          Right receipt | providerTurnId receipt==turn->do
            modifyIORef' ref (\state->state {promptPreparation=Nothing,queuedPrompt=Nothing,queuedDelivery=Nothing,
              activeTurn=Just receipt,pending=M.singleton 1 Prompting,deliveredContext=Just context})
            AR.acknowledgePrimaryDelivery agents
            forM_ control $ \request->case request of AR.QueryPrimary _ _ _ _ _ _ reply->void (tryPutMVar reply (Right ())); _->pure ()
            pure d {status="Agent is replying...",agentReplying=True}
          Right receipt->cancelProviderTurn receipt >> pure d
    Just (SteeringPrompt submission text context control worker)->poll worker >>= \ready->case ready of
      Nothing->pure d
      Just result->do
        retireProviderSubmission submission
        modifyIORef' ref (\state->state {promptPreparation=Nothing})
        case either (const (Left "Steering interrupted; draft kept.")) id result of
          Left err->AR.rejectPrimaryControl err control >> pure d {status=err}
          Right value->do
            modifyIORef' ref (\state->appendPrimary [PrimaryMessage UserSpeaker (composerMarkdown text)] state {deliveredContext=Just context})
            case control of AR.SteerPrimary _ _ _ _ reply->void (tryPutMVar reply (Right value)); _->pure ()
            pure d {status="Follow-up added to the active turn."}
    Just (PreparingHuman receipt worker)->poll worker >>= \ready->case ready of
      Nothing->pure d
      Just outcome->do
        modifyIORef' ref (\state->state {promptPreparation=Nothing})
        latest<-readIORef ref
        ownerCurrent<-operationOwnerCurrent receipt latest
        presentationCurrent<-operationCurrent receipt latest d
        if not ownerCurrent then pure d {status="Conversation options were prepared; provider ownership changed before adoption."} else case either (Left . T.pack . show) id outcome of
          Left err->pure d {status=err}
          Right (HumanContext _ _) | not presentationCurrent->pure d {status="Agent context file prepared; the current view was kept."}
          Right (HumanContext path doc)->do
            let protected=d {guestPrivatePaths=path:guestPrivatePaths d}
                opened=case find (\(_,document)->fmap filePath (documentFile document)==Just path) (M.toList (buffers d)) of
                  Just (bid,_)->case find ((==Just bid).bufferId) (windows d) of
                    Just window->focusWindow (windowId window) protected {buffers=M.adjust (\document->document {documentPrivate=True}) bid (buffers protected)}
                    Nothing->protected
                  Nothing->let added=addDocument (documentFile doc) (documentBuffer doc) protected in
                    case activeWindow added >>= bufferId of Just bid->added {buffers=M.insert bid doc (buffers added)};Nothing->added
            pure opened {status="Edit [editor.agent] context; save to apply with the next query or steer."}
          Right (HumanProvider launch)->do
            cancelled<-performPrimary runtime "cancel" [] d
            retired<-retireProviderResources runtime "Agent configuration changed."
            writeIORef ref retired {provider=launch,connection=Nothing,connectionIdentity=Nothing,currentTurn=Nothing,completedTurn=Nothing,activeTurn=Nothing,queuedDelivery=Nothing,session=Nothing,pending=M.empty,queuedPrompt=Nothing,queuedQueries=childQueries (queuedQueries retired),approvals=[],presented=Nothing,deferredApproval=False}
            pure cancelled {status="Agent configuration saved."}
    Just (Configuring control worker)->poll worker >>= \ready->case ready of
      Nothing->pure d
      Just result->do
        modifyIORef' ref (\state->state {promptPreparation=Nothing})
        case either (const (Left "Configuration interrupted.")) id result of
          Left err->AR.rejectPrimaryControl err control >> pure d {status=err}
          Right caps->do
            modifyIORef' ref (\state->state {agentCapabilities=caps})
            case control of AR.ConfigurePrimary _ _ _ reply->void (tryPutMVar reply (Right caps)); _->pure ()
            pure d {agentSettings=publicAgentSettings caps,contextMenu=Nothing,status="Conversation settings updated."}

-- Consume the supplying owner's terminal cell after draining its typed content.
pollProviderTurn :: ConversationState -> Desktop -> IO Desktop
pollProviderTurn runtime@(ConversationState _ ref _ _) d=do
  s<-readIORef ref
  case activeTurn s of
    Nothing->pure d
    Just turn->pollProviderReply (providerTurnReply turn) >>= \ready->case ready of
      Nothing->pure d
      Just result->do
        -- The adapter enqueues the boundary before filling an ordinary reply.
        -- Observing a failure may race the tick's earlier mailbox drain, so
        -- drain again after that receipt before classifying a failed pump.
        drained<-case result of
          Left _ | completedTurn s/=Just (providerTurnId turn)->drainConversationAgents runtime d
          _->pure d
        latest<-readIORef ref
        if currentTurn latest/=Just (providerTurnId turn) then pure drained
          else if completedTurn latest==Just (providerTurnId turn) then do
            modifyIORef' ref (\state->state {pending=M.empty,activeTurn=Nothing,currentTurn=Nothing})
            completeConversationDelivery runtime result
            pure drained {status=either id (\value->"Agent: "<>fromMaybe "finished" (field "stopReason" value)) result}
          else case result of
            Left err->do
              -- A failed pump cannot publish its successful content boundary.
              -- Retire this acquisition and its original native calls.
              completeConversationDelivery runtime (Left err)
              closed<-case connectionIdentity latest of
                Just owner->receiveProviderEvent runtime drained owner AH.ProviderClosed
                Nothing->pure drained
              pure closed {status=err}
            Right _->pure drained

-- Supply guidance once per connection and again when its saved value changes.
-- A separate text block preserves the user's query and the visible transcript.
preparePrompt :: State -> Text -> IO (Either Text ([Text],Value))
preparePrompt state query=do
  loaded<-readAgentContexts (project state)
  pure $ do
    context<-loaded
    let textAt scope=fromMaybe "" (field scope context >>= field "text")
        section scope title=title<>"\n"<>(if T.null (textAt scope) then "(none)" else textAt scope)
        guidance="Current editor context replaces earlier editor context. It does not grant additional tool permissions.\n\n"<>
          section "global" "Global context:"<>"\n\n"<>section "project" "Project context:"
        catalog="Editor skills: explore projects; edit/review; HLS diagnosis/rename; build/test/run; DAP debugging; Git review; desktop/hex navigation; user questions; documentation/settings. Read docs/agent-skills.md with docs_read (corpus editor) for the relevant workflow and docs/agent-tools.md for operations. Discover exact schemas with tools/list. For missing executables or libraries, inspect environment_get and fix paths with environment_set; do not prescribe shell exports or an editor restart when a new job suffices. Prefer repository build configuration fixes for project dependencies."
        extra=[guidance | deliveredContext state/=Just context]++[catalog | deliveredContext state==Nothing]
    pure (composerMarkdown query:extra,context)

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
  received<-pollProviderTurn runtime d
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
      forM_ (lookup token (approvals current)) $ \approval -> cancelApproval approval
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
  primary<-traverse (\ident->evaluate (ident,sessionName)) (connectionIdentity state)
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
  primary<-traverse (\ident->evaluate (ident,sessionName)) (connectionIdentity state)
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

-- The adapter owns protocol correlation. Host updates retain the original
-- acquisition/turn identity and enter only through the existing runtime mailbox.
receiveProviderEvent :: ConversationState -> Desktop -> ProviderIdentity -> AH.DriverEvent -> IO Desktop
receiveProviderEvent runtime@(ConversationState _ ref _ agents) d owner event=do
  s<-readIORef ref
  if connectionIdentity s/=Just owner then pure d else case event of
    AH.ProviderClosed->do
      cleared<-cancelQuestion runtime "Question requester disconnected." d
      AR.failPendingPrimary agents "Agent disconnected."
      retired<-retireProviderResources runtime "Agent disconnected."
      writeIORef ref (appendPrimary [PrimaryDisconnect "Agent disconnected."] retired
        {connection=Nothing,connectionIdentity=Nothing,session=Nothing,pending=M.empty,
         currentTurn=Nothing,completedTurn=Nothing,activeTurn=Nothing,queuedDelivery=Nothing,queuedPrompt=Nothing,
         queuedQueries=childQueries (queuedQueries retired),approvals=[],presented=Nothing,deferredApproval=False,
         ownedTerminals=S.empty,terminalWaiters=M.empty})
      pure (dismissPermission cleared) {status="Agent disconnected.",agentSteering=False}
    AH.ProviderCapabilities caps->do
      modifyIORef' ref (\state->state {agentCapabilities=caps})
      publishPrimaryEvent runtime event
      pure d {agentSettings=publicAgentSettings caps,agentSteering=AH.supportsSteering caps,contextMenu=Nothing}
    AH.ProviderUsage used size->publishPrimaryEvent runtime event >> pure d {agentContextUsage=Just (used,size)}
    _->publishPrimaryEvent runtime event >> pure d

receiveProviderContent :: ConversationState -> Desktop -> ProviderIdentity -> Maybe ProviderTurnId -> ProviderContent -> IO Desktop
receiveProviderContent (ConversationState _ ref _ _) d owner turn content=do
  s<-readIORef ref
  when (connectionIdentity s==Just owner) $ case content of
    ProviderMessage role text | not (T.null text),turn==currentTurn s,turn/=Nothing->do
      now<-getCurrentTime
      zone<-getCurrentTimeZone
      modifyIORef' ref (recordChunk role text . stampReply now zone)
    ProviderTool update->do
      now<-getCurrentTime
      modifyIORef' ref (\state->recordToolUpdate update state {lastMessageAt=Just now})
    ProviderPlan update->modifyIORef' ref (appendPrimary [PrimaryPlan update])
    ProviderTurnBoundary ident | Just ident==currentTurn s->modifyIORef' ref (\state->state {completedTurn=Just ident})
    _->pure ()
  pure d

-- Provider publication runs on the daemon's existing serialized session tick
-- (including its suspend drain), not the renderer or ordinary input branch.
-- It uses the same exact connection receipt as controls, never captures human
-- prompts, and never copies transcript history during sync.
publishPrimaryEvent :: ConversationState -> AH.DriverEvent -> IO ()
publishPrimaryEvent (ConversationState _ ref _ agents) event=do
  state<-readIORef ref
  case (connectionIdentity state,session state) of
    (Just identity,Just sid)->AR.recordPrimaryEvent agents identity sid event
    _->pure ()

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
recordChunk role text state=appendPrimary [PrimaryChunk speaker text] state
  where speaker=if role=="Agent" then AssistantSpeaker else UserSpeaker

recordToolUpdate :: Value -> State -> State
recordToolUpdate update=appendPrimary [PrimaryTool update]

incomingProvider :: forall a. ConversationState -> ProviderIdentity -> FilePath -> AR.ProviderCall a -> MVar (Either Text a) -> Desktop -> IO Desktop
incomingProvider runtime@(ConversationState _ ref consoles _) owner root operation reply d=do
  s<-readIORef ref
  waiting<-isEmptyMVar reply
  if connectionIdentity s/=Just owner || not waiting || questionsClosed s
    then void (tryPutMVar reply (Left "Provider request expired.")) >> pure d
    else case operation of
      AR.AskProviderPermission request->enqueueApproval runtime (Permission owner request reply) >> pure d
      AR.ReadProviderFile path line limit
        | line>=1,maybe True (>=0) limit->queueFileCapture ref owner (ReadFile line limit reply) root path d
        | otherwise->refuse "Invalid line range."
      AR.WriteProviderFile path text->queueFileCapture ref owner (WriteFile text reply) root path d
      AR.CreateProviderTerminal request | Terminal.terminalAvailable->queueBridge ref reply $ do
        worker<-async (checkTerminal root request)
        pure (CheckingTerminal owner reply (providerTerminalLimit request) worker)
      AR.CreateProviderTerminal _->refuse "Native terminals are unavailable."
      AR.ReadProviderTerminal tid->owned tid $ queueBridge ref reply $ do
        worker<-async $ do
          result<-C.consoleOutput consoles tid
          case result of
            Left err->pure (Left err)
            Right (bytes,truncated,exited)->do
              copied<-evaluate (BS.copy bytes)
              pure (Right (ProviderTerminalOutput copied truncated exited))
        pure (TerminalOperation owner reply worker)
      AR.WaitProviderTerminal tid->owned tid $ do
        modifyIORef' ref (\state->state {terminalWaiters=M.insertWith (++) tid [reply] (terminalWaiters state)})
        pure d
      AR.KillProviderTerminal tid->owned tid $ terminalJob (C.killConsole consoles tid)
      AR.ReleaseProviderTerminal tid->owned tid $ do
        modifyIORef' ref (\state->state {ownedTerminals=S.delete tid (ownedTerminals state),terminalWaiters=M.delete tid (terminalWaiters state)})
        forM_ (M.findWithDefault [] tid (terminalWaiters s)) (\waiter->void (tryPutMVar waiter (Left "Terminal released.")))
        terminalJob (C.releaseConsole consoles tid)
  where
    refuse :: Text -> IO Desktop
    refuse err=void (tryPutMVar reply (Left err)) >> pure d
    owned :: Text -> IO Desktop -> IO Desktop
    owned tid action=do
      current<-readIORef ref
      if S.member tid (ownedTerminals current) then action else refuse "Unknown terminal ID for this session."
    terminalJob :: IO (Either Text a) -> IO Desktop
    terminalJob action=queueBridge ref reply $ TerminalOperation owner reply <$> async action
    queueBridge :: IORef State -> MVar (Either Text b) -> IO FileCapture -> IO Desktop
    queueBridge state cell prepare=mask_ $ do
      current<-readIORef state
      if length (fileCaptures current)+sum (map fst (retiringRequests current))>=4
        then void (tryPutMVar cell (Left "Too many pending native requests.")) >> pure d
        else do
          capture<-prepare
          modifyIORef' state (\next->next {fileCaptures=fileCaptures next++[capture]})
          pure d

-- Resolve on the worker, capture the single immutable source on the owner,
-- then read/slice on the worker. Every stage occupies the same bounded slot.
queueFileCapture :: IORef State -> ProviderIdentity -> FileRequest -> FilePath -> FilePath -> Desktop -> IO Desktop
queueFileCapture ref owner request root path d=mask_ $ do
  s<-readIORef ref
  if length (fileCaptures s)+sum (map fst (retiringRequests s))>=4
    then finishFile (Left "Too many pending file requests.") request
    else do
      -- A known literal source binding belongs to this admission even while
      -- resolution waits. Aliases bind at canonical capture below; both retain
      -- the same final path/privacy/content-identity checks.
      expected<-sourceIdentity path d >>= traverse evaluate
      worker<-async (resolveFile root path)
      modifyIORef' ref (\state->state {fileCaptures=fileCaptures state++[ResolvingFile owner request expected worker]})
  pure d

checkTerminal :: FilePath -> ProviderTerminal -> IO (Either Text Terminal.TerminalConfig)
checkTerminal root request=case validateLaunch (providerTerminalLaunch request) of
  Left err->pure (Left (T.pack err))
  Right launch | length (arguments launch)>256 || length (environment launch)>256 ||
    length (executable launch)>32768 || any ((>65536).length) (arguments launch++concatMap (\(k,v)->[k,v]) (environment launch))->pure (Left "Terminal launch exceeds its bounds.")
  Right _ | not (isAbsolute cwd) || length cwd>32768 || '\0' `elem` cwd->pure (Left "Expected an absolute terminal directory.")
  Right _ | limit<0 || limit>16*1024*1024->pure (Left "Terminal output limit must be between 0 and 16 MiB.")
  Right launch->do
    checked<-try (canonicalizePath cwd) :: IO (Either IOException FilePath)
    pure $ case checked of
      Right directory | let relative=makeRelative root directory,not (isAbsolute relative),".." `notElem` splitDirectories relative->
        Right (Terminal.TerminalConfig (executable launch) (arguments launch) (environment launch) directory 80 24)
      _->Left "Terminal directory is outside the session project."
  where cwd=providerTerminalDirectory request; limit=providerTerminalLimit request

retireRequests :: IORef State -> IO State
retireRequests ref = mask $ \restore -> do
  s<-readIORef ref
  case promptPreparation s of
    Just (ContextPrompt _ _ control _)->mapM_ (AR.rejectPrimaryControl "Agent control cancelled.") control
    Just (SendingPrompt _ submission _ _ control _)->retireProviderSubmission submission >> mapM_ (AR.rejectPrimaryControl "Agent control cancelled.") control
    Just (SteeringPrompt submission _ _ control _)->retireProviderSubmission submission >> AR.rejectPrimaryControl "Agent control cancelled." control
    Just (Configuring control _)->AR.rejectPrimaryControl "Agent control cancelled." control
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
      workers=map captureCancel captures++map cancel (maybe [] pure (terminalPoll s))++map preparationCancel (maybe [] pure (promptPreparation s))++
        [cancel worker | Just (_,worker)<-[activeInput]]
  if null workers then writeIORef ref retired >> pure retired else do
    reaper<-async (restore (sequence_ workers))
    let next=retired {fileCaptures=[],terminalPoll=Nothing,promptPreparation=Nothing,agentControls=M.delete "" (agentControls s),
          retiringRequests=retiringRequests s++[(length workers,reaper)]}
    writeIORef ref next
    pure next

-- Retire host authority synchronously, then join only the detached provider and
-- console handles on the existing reaper. No State or desktop escapes to it.
retireProviderResources :: ConversationState -> Text -> IO State
retireProviderResources (ConversationState _ ref consoles agents) reason=do
  current<-readIORef ref
  mapM_ (cancelApproval . snd) (approvals current)
  forM_ (concat (M.elems (terminalWaiters current))) (\reply->void (tryPutMVar reply (Left reason)))
  mapM_ (AR.retireProviderCalls agents) (connectionIdentity current)
  retired<-retireRequests ref
  tids<-evaluate (force (S.toList (ownedTerminals retired)))
  driver<-traverse evaluate (connection retired)
  let detached=retired {approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty}
      count=length tids+maybe 0 (const 1) driver
  next<-if count==0 then pure detached else do
    reaper<-async (mapM_ (C.releaseConsole consoles) tids >> mapM_ AH.driverStop driver)
    pure detached {retiringRequests=retiringRequests detached++[(count,reaper)]}
  writeIORef ref next
  pure next

pollFileCaptures :: ConversationState -> Desktop -> IO Desktop
pollFileCaptures runtime@(ConversationState _ ref consoles _) original=do
  s<-readIORef ref
  retiring<-filterM (fmap isNothing . poll . snd) (retiringRequests s)
  modifyIORef' ref (\state->state {retiringRequests=retiring})
  drain original
  where
    drain d=do
      s<-readIORef ref
      case fileCaptures s of
        []->pure d
        capture:rest->do
          let replace next=modifyIORef' ref (\state->state {fileCaptures=next})
              alive owner waiting=(connectionIdentity s==Just owner && not (questionsClosed s) && waiting)
              failed result=either (const (Left "Native request failed.")) id result
          case capture of
            ResolvingFile owner request expected worker->poll worker >>= \ready->case ready of
              Nothing->pure d
              Just outcome->do
                waiting<-fileWaiting request
                case failed outcome of
                  Right resolved | alive owner waiting->do
                    current<-sourceIdentity (resolvedFilePath resolved) d
                    selected<-if maybe False (const (current/=expected)) expected
                      then pure (Left "File changed in the editor during path resolution; request a fresh read.")
                      else captureFileInput resolved d
                    case selected of
                      Left err->finishFile (Left err) request >> replace rest >> drain d
                      Right input->do
                        worker'<-async (readFileInput input)
                        replace (ReadingFile owner request (fileInputIdentity input) worker':rest)
                        pure d
                  result->finishFile (either Left (const (Left "File request expired.")) result) request >> replace rest >> drain d
            ReadingFile owner request expected worker->poll worker >>= \ready->case ready of
              Nothing->pure d
              Just outcome->do
                replace rest
                waiting<-fileWaiting request
                case failed outcome of
                  Right snap | alive owner waiting->do
                    current<-sourceIdentity (snapshotPath snap) d
                    if current/=expected || not (filePublic (snapshotPath snap) d) then finishFile (Left "File changed in the editor during capture; request a fresh read.") request
                    else case request of
                      ReadFile line limit reply->do
                        -- Slice the immutable read on the worker before publication.
                        worker'<-async (sliceFile snap line limit)
                        replace (SlicingFile owner reply snap expected worker':rest)
                      WriteFile text reply->enqueueApproval runtime (Write owner reply (M.findWithDefault snap (snapshotPath snap) (reads s)) text)
                  result->finishFile (either Left (const (Left "File request expired.")) result) request
                drain d
            SlicingFile owner reply snap expected worker->poll worker >>= \ready->case ready of
              Nothing->pure d
              Just outcome->do
                replace rest
                waiting<-isEmptyMVar reply
                current<-sourceIdentity (snapshotPath snap) d
                let admitted=alive owner waiting && current==expected && filePublic (snapshotPath snap) d
                    result=if admitted then failed outcome else Left "File changed during read preparation; request a fresh read."
                void (tryPutMVar reply result)
                case result of Right _->modifyIORef' ref (\state->state {reads=M.insert (snapshotPath snap) snap (reads state)}); _->pure ()
                drain d
            CheckingTerminal owner reply limit worker->poll worker >>= \ready->case ready of
              Nothing->pure d
              Just outcome->do
                replace rest
                waiting<-isEmptyMVar reply
                case failed outcome of
                  Right config | alive owner waiting->enqueueApproval runtime (Execute owner reply config limit)
                  result->void (tryPutMVar reply (either Left (const (Left "Terminal request expired.")) result))
                drain d
            PreparingTerminal owner reply worker->poll worker >>= \ready->case ready of
              Nothing->pure d
              Just (Right (Right _)) | dialog d/=Nothing || chatQuestion d/=Nothing->pure d
              Just outcome->do
                replace rest
                waiting<-isEmptyMVar reply
                case failed outcome of
                  Right prepared | alive owner waiting->do
                    (tid,next)<-C.adoptConsole consoles prepared d `onException` C.closePreparedConsole prepared
                    modifyIORef' ref (\state->state {ownedTerminals=S.insert tid (ownedTerminals state)})
                    void (tryPutMVar reply (Right tid))
                    drain next
                  Right prepared->C.closePreparedConsole prepared >> drain d
                  Left err->void (tryPutMVar reply (Left err)) >> drain d
            TerminalOperation owner reply worker->poll worker >>= \ready->case ready of
              Nothing->pure d
              Just outcome->do
                replace rest
                waiting<-isEmptyMVar reply
                void (tryPutMVar reply (if alive owner waiting then failed outcome else Left "Native request expired."))
                drain d

-- Only metadata for this path participates in final immutable source admission.
filePublic :: FilePath -> Desktop -> Bool
filePublic path d=not (protectedPath d path) && not (any privateMatching (M.toList (buffers d)))
  where privateMatching (bid,doc)=fmap filePath (documentFile doc)==Just path &&
          (protectedBuffer d bid || not (textBuffer (documentBuffer doc)))

sliceFile :: Snapshot -> Int -> Maybe Int -> IO (Either Text Text)
sliceFile snap line limit=do
  let content=snapshotText snap; buffer=newBuffer content
      startOffset=if line>bufferLineCount buffer then T.length content else bufferLineOffset buffer (line-1)
      endOffset=case limit of Nothing->T.length content; Just count | count>=bufferLineCount buffer-line+1->T.length content
                                                               | otherwise->bufferLineOffset buffer (line-1+count)
  chosen<-evaluate (T.copy (T.take (max 0 (endOffset-startOffset)) (T.drop startOffset content)))
  pure (Right chosen)

enqueueApproval :: ConversationState -> Approval -> IO ()
enqueueApproval (ConversationState _ ref _ _) approval=modifyIORef' ref (\s -> s {approvals=approvals s++[(nextApproval s,approval)],nextApproval=nextApproval s+1})

present :: ConversationState -> Desktop -> IO Desktop
present (ConversationState _ ref _ _) d=do
  s<-readIORef ref
  case (dialog d,presented s,approvals s) of
    (Nothing,Nothing,(token,approval):_) | not (deferredApproval s)->do
      modifyIORef' ref (\state->state {presented=Just token})
      let action="approval:"<>T.pack (show token)
          permissionDialog title request buttons=Just (Dialog title (AgentDialog action)
            [ListBox "Action" [label | (_,label,_)<-permissionOptions request] 0] 0 buttons
            (take 7 (wrapMessage (permissionTitle request)++wrapMessage (permissionDetails request))))
      pure $ case approval of
        ChildPermission ident request _->d {dialog=permissionDialog ("Agent permission: "<>AH.agentIdText ident) request ["Choose","Reject"]}
        Permission _ request _->d {dialog=permissionDialog "Agent permission" request ["Choose","Review","Cancel"]}
        Execute _ _ config _->d {dialog=Just (Dialog "Run agent command" (AgentDialog action) [] 0 ["Run","Reject"]
          (take 7 (wrapMessage (T.pack (Terminal.terminalCommand config))++wrapMessage (TE.decodeUtf8 (BL.toStrict (encode (Terminal.terminalArguments config))))++wrapMessage (T.pack (Terminal.terminalDirectory config)))))}
        Write _ _ snap text->(addReadOnly "Proposed agent edit" ("CURRENT BUFFER\n"<>snapshotText snap<>"\n\nPROPOSED CONTENT\n"<>text) d)
          {dialog=Just (Dialog "Apply agent edit" (AgentDialog action) [] 0 ["Apply","Review","Reject"]
            ["The proposed edit is open behind this dialog.","Apply saves it and preserves the old buffer in Undo."])}
    _->pure d

approvalCurrent :: State -> Approval -> IO Bool
approvalCurrent state approval=case approval of
  ChildPermission _ _ reply->isEmptyMVar reply
  Permission owner _ reply->(connectionIdentity state==Just owner &&) <$> isEmptyMVar reply
  Write owner reply _ _->(connectionIdentity state==Just owner &&) <$> isEmptyMVar reply
  Execute owner reply _ _->(connectionIdentity state==Just owner &&) <$> isEmptyMVar reply

decide :: ConversationState -> Int -> [Text] -> Desktop -> IO Desktop
decide (ConversationState _ ref _ _) token values d=do
  s<-readIORef ref
  case lookup token (approvals s) of
    Nothing->pure d {status="Permission request expired."}
    Just approval->do
      current<-approvalCurrent s approval
      if not current then cancelApproval approval >> pure d {status="Permission request expired."}
      else if take 1 values==["1"] && reviewable approval then do
        modifyIORef' ref (\state->state {presented=Nothing,deferredApproval=True})
        let detail=case approval of
              Permission _ request _->permissionDetails request
              Write _ _ snap text->"FILE: "<>T.pack (snapshotPath snap)<>"\n\nCURRENT BUFFER\n"<>snapshotText snap<>"\n\nPROPOSED CONTENT\n"<>text
              _->""
        pure (addReadOnly "Agent request" detail d) {status="Tools > Conversation returns to the pending approval."}
      else do
        modifyIORef' ref (\state->state {approvals=filter ((/=token).fst) (approvals state),presented=Nothing})
        case approval of
          ChildPermission _ request reply->selectPermission request reply >> pure d
          Permission _ request reply->selectPermission request reply >> pure d
          Write _ reply snap text | take 1 values==["0"]->do
            result<-acceptWrite snap text d
            case result of
              Left err->void (tryPutMVar reply (Left err)) >> pure (message "Agent edit rejected" (wrapMessage err) d)
              Right changed->do
                void (tryPutMVar reply (Right ()))
                modifyIORef' ref (\state->state {reads=maybe (reads state) (\fresh->M.insert (snapshotPath fresh) fresh (reads state)) (M.lookup (snapshotPath snap) (sourceSnapshots changed))})
                pure changed
          Execute owner reply config limit | take 1 values==["0"]->mask_ $ do
            worker<-async (C.prepareConsole [] config limit)
            modifyIORef' ref (\state->state {fileCaptures=fileCaptures state++[PreparingTerminal owner reply worker]})
            pure d {status="Starting approved agent command..."}
          _->cancelApproval approval >> pure d
  where
    reviewable Permission{}=True
    reviewable Write{}=True
    reviewable _=False
    selectPermission request reply=void (tryPutMVar reply (Right selected))
      where selected=case values of
              "0":index:_ | Just n<-readMaybe (T.unpack index),n>=0,(chosen,_,_):_<-drop n (permissionOptions request)->Just chosen
              _->Nothing

cancelApproval :: Approval -> IO ()
cancelApproval approval=case approval of
  ChildPermission _ _ reply->void (tryPutMVar reply (Right Nothing))
  Permission _ _ reply->void (tryPutMVar reply (Right Nothing))
  Execute _ reply _ _->void (tryPutMVar reply (Left "User rejected the command."))
  Write _ reply _ _->void (tryPutMVar reply (Left "User rejected the edit."))

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

publicAgentSettings :: AH.Capabilities -> [AgentSetting]
publicAgentSettings caps=[AgentSetting (AH.configId choice)
  (if AH.configCategory choice=="model" then "Model" else "Reasoning effort")
  (AH.configCategory choice) (AH.configCurrent choice) (AH.configValues choice) | choice<-AH.configChoices caps]

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
-- Waiters remain lightweight cells; one batch refresh uses the existing
-- console owner on a worker, rather than spawning one worker per waiter.
flushTerminalWaiters :: ConversationState -> IO ()
flushTerminalWaiters (ConversationState _ ref consoles _)=do
  s<-readIORef ref
  case terminalPoll s of
    Nothing | not (M.null (terminalWaiters s))->do
      let tids=M.keys (terminalWaiters s)
      worker<-async (mapM (\tid->(tid,) <$> C.consoleOutput consoles tid) tids)
      modifyIORef' ref (\state->state {terminalPoll=Just worker})
    Just worker->poll worker >>= \ready->case ready of
      Nothing->pure ()
      Just outcome->do
        modifyIORef' ref (\state->state {terminalPoll=Nothing})
        forM_ (either (const []) id outcome) $ \(tid,result)->do
          current<-readIORef ref
          let completed=case result of Left err->Just (Left err); Right (_,_,Just code)->Just (Right code); _->Nothing
          forM_ completed $ \reply->do
            mapM_ (\cell->void (tryPutMVar cell reply)) (M.findWithDefault [] tid (terminalWaiters current))
            modifyIORef' ref (\state->state {terminalWaiters=M.delete tid (terminalWaiters state)})
    _->pure ()

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
  executable<-evaluate (executable (provider state))
  argumentCount<-evaluate (length (arguments (provider state)))
  environmentNames<-evaluate (force (map (T.pack . fst) (environment (provider state))))
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
          receipt<-case (connectionIdentity s,session s) of
            (Just identity,Just sid)->pure (Just (ProviderReceipt identity sid))
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
providerCurrent (ProviderReceipt identity sid) s=pure (connectionIdentity s==Just identity && session s==Just sid && not (isNothing (connection s)))

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
  let owner=connectionIdentity s
  deliveryActive<-AR.primaryDeliveryActive (conversationAgents runtime)
  let key=fromMaybe "" (session s)
      caps=agentCapabilities s
      externallyBusy=busy s && not deliveryActive
      signature=(project s,owner,key,caps,externallyBusy)
  when (lastAgentSync s/=Just signature) $ do
    result<-AR.syncPrimary (conversationAgents runtime) (project s) (connectionIdentity s) key caps externallyBusy
    case result of
      Left _ -> pure () -- Invalid policy stays fail-closed and can be repaired live.
      Right () -> modifyIORef' ref (\state -> state {lastAgentSync=Just signature})

denyChild :: Approval -> IO ()
denyChild (ChildPermission _ _ reply)=void (tryPutMVar reply (Right Nothing))
denyChild _=pure ()

drainConversationAgents :: ConversationState -> Desktop -> IO Desktop
drainConversationAgents runtime@(ConversationState _ ref _ agents) d=do
  requests<-AR.drainAgentRequests agents
  foldM apply d requests
  where
    apply desktop (AR.DeliverPrimary delivery)=do
      s<-readIORef ref
      case (connectionIdentity s,session s) of
        (Just identity,Just sid) | not (primaryBusy s)->do
          admitted<-AR.admitPrimaryDelivery agents delivery identity sid
          case admitted of
            Nothing->pure desktop
            Just msg->do
              let author=case AH.messageAuthor msg of AH.Human -> "Human"; AH.Agent ident -> "Agent "<>AH.agentIdText ident
                  attribution=if AH.messageIsUserSeat msg then "Human message" else author<>" sent a peer message, not the human user seat"
              writeIORef ref (appendPrimary [PrimaryMessage (PeerSpeaker (AH.messageAuthor msg)) (AH.messageText msg)] s
                {queuedPrompt=Just (attribution<>"\n\n"<>AH.messageText msg,Nothing),queuedDelivery=Just delivery,reads=sourceSnapshots desktop})
              sendQueued runtime desktop
        _->AR.rejectPrimaryDelivery agents delivery "The main conversation is not ready; connect it and retry." >> pure desktop
    apply desktop (AR.ControlPrimary control)=do
      state<-readIORef ref
      current<-AR.primaryControlCurrent agents control (connectionIdentity state) (session state)
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
        AR.ConfigurePrimary _ _ settings _
          | not (busy state),Just driver<-connection state->mask_ $ do
              worker<-async (AH.driverConfigure driver settings)
              modifyIORef' ref (\s->s {promptPreparation=Just (Configuring control worker)})
              pure desktop {status="Updating conversation settings...",agentReplying=True}
        AR.SteerPrimary _ _ msg _ _
          | Prompting `elem` M.elems (pending state),not (steeringPending state)->
              beginPromptPreparation ref True (AH.messageText msg) (Just control) desktop
        _->AR.rejectPrimaryControl "The primary turn changed before control admission; draft kept." control >> pure desktop
    apply desktop AR.CancelPrimary=performPrimary runtime "cancel" [] desktop
    apply desktop AR.EndPrimary=do
      cleared<-cancelQuestion runtime "Agent session ended." desktop
      _<-retireProviderResources runtime "Agent session ended."
      AR.failPendingPrimary agents "Agent session ended."
      modifyIORef' ref (\state -> state {connection=Nothing,connectionIdentity=Nothing,currentTurn=Nothing,completedTurn=Nothing,activeTurn=Nothing,queuedDelivery=Nothing,session=Nothing,pending=M.empty,queuedPrompt=Nothing,queuedQueries=childQueries (queuedQueries state),approvals=[],presented=Nothing})
      pure cleared {status="Agent session ended."}
    apply desktop (AR.ProviderEvent identity event)=receiveProviderEvent runtime desktop identity event
    apply desktop (AR.ProviderContentEvent identity turn content)=receiveProviderContent runtime desktop identity turn content
    apply desktop (AR.NativeProviderRequest identity root operation reply)=incomingProvider runtime identity root operation reply desktop
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
  s<-readIORef ref
  void (AR.completePrimaryDelivery agents (connectionIdentity s) (session s) (busy s || queryCount "" (queuedQueries s)>0) result)

pruneChildApprovals :: ConversationState -> Desktop -> IO Desktop
pruneChildApprovals (ConversationState _ ref _ _) d=do
  s<-readIORef ref
  live<-filterM (approvalCurrent s . snd) (approvals s)
  let expired=maybe False (\ident -> ident `notElem` map fst live) (presented s)
  modifyIORef' ref (\state -> state {approvals=live,presented=if expired then Nothing else presented state})
  pure (if expired then dismissPermission d else d)

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
      attach :: (Editor.PreparedEditor services Editor.EditorUpdate -> ConversationEditor) -> Editor.PreparedEditor services Editor.EditorUpdate -> IO Desktop
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
preparingSteer SteeringPrompt{}=True
preparingSteer _=False

captureEditorContext :: ConversationState -> Text -> IO (Either Text ChatEditorContext)
captureEditorContext (ConversationState _ ref _ agents) target=do
  state<-readIORef ref
  launch<-makeStableName =<< evaluate (provider state)
  if T.null target then do
    receipt<-case (connectionIdentity state,session state) of
      (Just identity,Just sid)->pure (Just (ProviderReceipt identity sid))
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
