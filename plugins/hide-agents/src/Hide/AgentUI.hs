-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- | Module      : Hide.AgentUI
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Agents contributes a tree and scoped ID-bound commands through public plugin
-- capabilities. Its worker prepares metadata and queues invalidations; the host
-- adopts bounded deltas without calling plugin code on the UI owner.
module Hide.AgentUI (plugin) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forever,forM_)
import Data.Aeson (Value(Null))
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Hide.Plugin.Agent as A
import qualified Hide.Plugin.AgentDirectory as A
import Hide.Plugin.AgentDirectory (AgentDirectory(..),DirectoryRequest(..),CompletionEntry(..))
import qualified Hide.Plugin.Form as Form
import Hide.Plugin.Command
import Hide.Plugin.Session (Plugin(..),Session(..),PluginTool(..))
import Hide.Plugin.Request (requestQuestions)
import qualified Hide.QuestionTools as QuestionTools
import qualified Hide.AgentSettingsTools as AgentSettingsTools
import Hide.Plugin.Services (EditorServices(..))
import Hide.Plugin.Tool (mapToolContext)
import qualified Hide.AgentTools as AgentTools
import qualified Hide.BufferTools as BufferTools
import qualified Hide.DocsTools as DocsTools
import qualified Hide.EnvironmentTools as EnvironmentTools
import qualified Hide.AgentTranscript as AgentTranscript
import qualified Hide.CompletionInput as CompletionInput
import qualified Hide.ConversationInput as ConversationInput
import qualified Hide.ConversationMenus as ConversationMenus
import qualified Hide.ConversationChoices as ConversationChoices
import Hide.AgentConfigurationForms (captureChoices,prepareAgentChoice,choiceSpec)
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Tree as P
import qualified Hide.Plugin.Sidebar as Sidebar

data Snapshot completion = Snapshot !(M.Map A.AgentId A.AgentSummary) !(Maybe (CompletionEntry completion))

-- | Register the Agents directory for this session. Closing its view or this
-- registration never stops providers; the host retains their shared lifetime.
plugin :: Plugin
plugin=Plugin
  { withPlugin= \session->ConversationMenus.withConversationMenus (sessionSidebar session)
      (sessionMenus session) (sessionConversation session) (sessionConversationReply session)
      . ConversationChoices.withConversationChoices (sessionSidebar session) (sessionMenus session)
          (sessionAgents session) (sessionSelectedAgent session) (sessionAgentReply session)
      . withAgentSidebar (sessionSidebar session) (sessionAgentReply session) (sessionAgents session)
  , pluginTools=map CoordinationTool AgentTools.tools++map EditorTool (map (mapToolContext editorDocumentation) DocsTools.tools
      ++map (mapToolContext editorEnvironment) EnvironmentTools.tools
      ++map (mapToolContext editorAgentSettings) AgentSettingsTools.tools)
      ++map RequestTool (BufferTools.tools++map (mapToolContext requestQuestions) QuestionTools.tools)
  , pluginConversation=Just AgentTranscript.presentConversation
  , pluginPrimaryInput=Just ConversationInput.primaryInput
  , pluginChildInput=Just ConversationInput.childInput
  , pluginCompletionInput=Just CompletionInput.completionInput
  }

rootId :: P.NodeId
rootId=ident "agents"
ident :: Text -> P.NodeId
ident=either (error . T.unpack) id . P.nodeId
agentNode :: A.AgentId -> P.NodeId
agentNode=ident . A.agentIdText

-- | Scope commands and one metadata preparation worker. Closing the view never
-- closes agents; closing this registration rejects retained hits and commands.
withAgentSidebar :: Eq completion => Sidebar.Sidebar c r -> (DirectoryRequest settings completion -> r) -> AgentDirectory settings completion -> IO a -> IO a
withAgentSidebar host inject directory use=withRegistry $ \registry->do
  (initial,initialCompletion)<-prepare
  source<-newIORef (Snapshot initial initialCompletion)
  let command name title run=registerCommand registry (CommandDef name title hidden hidden run) >>= either (ioError . userError . show) pure
      human ctx value=if Sidebar.sidebarOrigin host ctx==Menu.HumanMenu then pure (Right (inject value)) else pure (Left (CommandRejected "Agent sidebar actions require the human."))
  open<-command "hide.sidebar.agents.open" "Conversation" (\ctx who->human ctx (ShowAgent who))
  renameTo<-command "hide.sidebar.agents.rename-to" "Rename agent" (\ctx (who,name)->do
    let copied=T.copy name
    _<-evaluate (T.length copied)
    human ctx (RenameAgentTo who copied))
  rename<-command "hide.sidebar.agents.rename" "Rename" (\ctx who->
    if Sidebar.sidebarOrigin host ctx/=Menu.HumanMenu then pure (Left (CommandRejected "Agent forms require the human.")) else do
      selected<-directoryAgent directory who
      case selected of
        Left err->pure (Left (CommandRejected err))
        Right entry->do
          let name=A.summaryName entry
          prepared<-Form.prepareForm Form.PrivateForm (Form.InputFormSpec "Rename agent" "Name" name "Rename")
            (Form.formAction registry renameTo (\text->(who,text)) (\_ reply->pure reply))
          pure (Sidebar.formReply host <$> prepared))
  configureAgent<-command "hide.sidebar.agents.configure" "Apply agent setting"
    (\ctx (receipt,option,value)->human ctx (ConfigureAgent receipt option value))
  configureCompletion<-command "hide.sidebar.completion.configure" "Apply completion setting"
    (\ctx (target,option,value)->human ctx (ConfigureCompletion target option value))
  let configurationCommand category ctx who
        | Sidebar.sidebarOrigin host ctx/=Menu.HumanMenu=pure (Left (CommandRejected "Agent forms require the human."))
        | otherwise=do
            -- The host owns advertised metadata and validates the captured
            -- receipt again on application. A refresh cannot retarget this form.
            captured<-captureChoices directory who
            case captured of
              Right (receipt,choices) | [choice]<-[choice | choice<-choices,A.configCategory choice==category]->do
                prepared<-prepareAgentChoice False registry configureAgent receipt choice
                pure (Sidebar.formReply host <$> prepared)
              _->pure (Left (CommandRejected "The agent has not advertised these choices."))
      completionCommand category ctx target
        | Sidebar.sidebarOrigin host ctx/=Menu.HumanMenu=pure (Left (CommandRejected "Completion forms require the human."))
        | otherwise=do
            captured<-directoryCompletionSettings directory target category
            case captured of
              Left err->pure (Left (CommandRejected err))
              Right (receipt,option,choices,current)->do
                prepared<-Form.prepareForm Form.ReadableForm (choiceSpec "Completion setting" choices current)
                  (Form.formAction registry configureCompletion (\value->(receipt,option,value)) (\_ reply->pure reply))
                pure (Sidebar.formReply host <$> prepared)
  model<-command "hide.sidebar.agents.model" "Model" (configurationCommand "model")
  effort<-command "hide.sidebar.agents.effort" "Effort" (configurationCommand "thought_level")
  completionOpen<-command "hide.sidebar.completion.open" "Completion conversation" (\ctx target->human ctx (ShowCompletion target))
  completionModel<-command "hide.sidebar.completion.model" "Completion model" (completionCommand "model")
  completionEffort<-command "hide.sidebar.completion.effort" "Completion effort" (completionCommand "thought_level")
  createTo<-command "hide.sidebar.agents.create" "Create agent" (\ctx (workspace,name,task)->human ctx (CreateAgent workspace name task))
  create<-command "hide.sidebar.agents.new" "New Agent" (\ctx ()->
    if Sidebar.sidebarOrigin host ctx/=Menu.HumanMenu then pure (Left (CommandRejected "Agent forms require the human.")) else do
      let workspace=Sidebar.sidebarWorkspace host ctx
      _<-evaluate (length workspace)
      prepared<-Form.prepareForm Form.PrivateForm
        (Form.InputsFormSpec "New agent" [Form.InputField "name" "Name" "",Form.InputField "task" "Task" ""] "Create")
        (Form.inputsFormAction registry createTo (\values->case (M.lookup "name" values,M.lookup "task" values) of
          (Just name,Just task)->Right (workspace,name,task)
          _->Left (InvalidArguments "Missing agent name or task.")) (\_ reply->pure reply))
      pure (Sidebar.formReply host <$> prepared))
  let root=P.NodeDef (P.NodeInfo rootId "Agents" "" True Nothing) Nothing [P.ActionMenu "New Agent" (P.treeAction registry create () (\_ value->pure value))]
      node values summary=
        let who=A.summaryId summary
            branch=any ((==Just who).A.summaryParent) (M.elems values)
            action=P.treeAction registry open who (\_ value->pure value)
        in P.NodeDef (P.NodeInfo (agentNode who) (A.summaryName summary<>"  "<>A.summaryStatus summary) "" branch Nothing)
             (if branch then Nothing else Just action)
             ([P.ActionMenu "Conversation" action | branch]++[P.ActionMenu "Rename" (P.treeAction registry rename who (\_ value->pure value))]++
              [P.ActionMenu title (P.treeAction registry setting who (\_ value->pure value)) | (title,setting)<-[(title,setting) | (title,setting,available)<-[("Model",model,A.summaryModel summary),("Effort",effort,A.summaryEffort summary)],available]])
      completionNode (CompletionEntry key target state)=P.NodeDef
        (P.NodeInfo (ident key) ("ACP completion  "<>state) "" False Nothing)
        (Just (P.treeAction registry completionOpen target (\_ value->pure value)))
        [P.ActionMenu title (P.treeAction registry setting target (\_ value->pure value)) | (title,setting)<-[("Model",completionModel),("Effort",completionEffort)]]
  provider<-P.registerTree registry "hide.sidebar.agents" root (\_ (P.ChildRequest parent cursor)->do
    Snapshot values completion<-readIORef source
    let children=filter (\summary->if parent==rootId then maybe True (not . (`M.member` values)) (A.summaryParent summary) else fmap agentNode (A.summaryParent summary)==Just parent) (M.elems values)
    case traverse (readMaybe . T.unpack) cursor of
      Nothing->pure (Left (InvalidArguments "Invalid agent page cursor."))
      Just offset | let start=maybe 0 id offset,start<0 || start>32768->pure (Left (InvalidArguments "Invalid agent page cursor."))
      Just offset->let start=maybe 0 id offset
                       nodes=[completionNode value | parent==rootId,Just value<-[completion]]++map (node values) children
                       page=take 128 (drop start nodes)
        in pure (Right (P.NodePage page (if length (drop (start+128) nodes)>0 then Just (T.pack (show (start+128))) else Nothing)))) >>= either (ioError . userError . show) pure
  Sidebar.publishTree host provider
  let worker=forever $ do
        threadDelay 250000
        (next,nextCompletion)<-prepare
        Snapshot previous oldCompletion<-readIORef source
        if next==previous && nextCompletion==oldCompletion then pure () else do
          affectedNodes<-prepareParents previous next
          writeIORef source (Snapshot next nextCompletion)
          forM_ affectedNodes (Sidebar.invalidateTree host (P.treeReference provider))
  withAsync worker (const use)
  where
    hidden=Codec Null (const (Left "Agent arguments are host-captured.")) (const Null)
    prepare=do
      summaries<-directoryAgents directory
      let bounded=take 1024 summaries
      _<-evaluate (force [(A.agentIdText (A.summaryId summary),A.summaryName summary,fmap A.agentIdText (A.summaryParent summary),A.summaryStatus summary) | summary<-bounded])
      values<-evaluate (M.fromList [(A.summaryId summary,summary) | summary<-bounded])
      completion<-directoryCompletion directory
      _<-evaluate (force [(completionKey entry,completionStatus entry) | Just entry<-[completion]])
      pure (values,completion)
    prepareParents previous next=do
      let affectedNodes=parents previous next
      _<-evaluate (force (map P.nodeIdText affectedNodes))
      pure affectedNodes
    parents previous next=rootId:map agentNode (S.toList (S.fromList (mapMaybe A.summaryParent (M.elems previous++M.elems next))))
