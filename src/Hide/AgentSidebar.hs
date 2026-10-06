{-# LANGUAGE OverloadedStrings #-}
-- | Module      : Hide.AgentSidebar
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Agents consumes the shared tree with scoped ID-bound commands. One worker
-- prepares only names, parents and states; owner ticks compare its small revision
-- and invalidate at most four nodes through the existing tree request machinery.
module Hide.AgentSidebar (AgentSidebar, withAgentSidebar, tickAgentSidebar) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forever)
import Data.Aeson (Value(Null),withObject,(.:))
import Data.Aeson.Types (parseMaybe)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe,listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Hide.AgentHub as A
import Hide.AgentRuntime (AgentRuntime,agentHub)
import Hide.Autocomplete (Autocomplete,completionSummary,completionChoices)
import Hide.AgentSidebarTypes
import qualified Hide.Plugin.Form as Form
import Hide.Plugin.Command
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Tree as P
import qualified Hide.Plugin.Sidebar as Sidebar

data Snapshot = Snapshot !Integer ![P.NodeId] !(M.Map A.AgentId A.AgentSummary) !(Maybe CompletionSummary)
data AgentSidebar c r = AgentSidebar !(Sidebar.Sidebar c r) !P.TreeRef !(IORef Snapshot) !(IORef (Integer,[P.NodeId]))
rootId :: P.NodeId
rootId=ident "agents"
ident :: Text -> P.NodeId
ident=either (error . T.unpack) id . P.nodeId
agentNode :: A.AgentId -> P.NodeId
agentNode=ident . A.agentIdText

completionNodeId :: CompletionTarget -> P.NodeId
completionNodeId (CompletionTarget epoch _)=ident ("acp-completion-"<>T.pack (show epoch))

-- | Scope commands and one metadata preparation worker. Closing the view never
-- closes agents; closing this registration rejects retained hits and commands.
withAgentSidebar :: Sidebar.Sidebar c r -> (AgentSidebarRequest -> r) -> AgentRuntime -> Autocomplete -> (AgentSidebar c r -> IO a) -> IO a
withAgentSidebar host inject runtime autocomplete use=withRegistry $ \registry->do
  (initial,initialCompletion)<-prepare
  affected<-prepareParents M.empty initial
  source<-newIORef (Snapshot 1 affected initial initialCompletion)
  pending<-newIORef (0,[])
  let command name title run=registerCommand registry (CommandDef name title hidden hidden run) >>= either (ioError . userError . show) pure
      human ctx value=if Sidebar.sidebarOrigin host ctx==Menu.HumanMenu then pure (Right (inject value)) else pure (Left (CommandRejected "Agent sidebar actions require the human."))
  open<-command "hide.sidebar.agents.open" "Conversation" (\ctx who->human ctx (ShowAgent who))
  renameTo<-command "hide.sidebar.agents.rename-to" "Rename agent" (\ctx (who,name)->do
    let copied=T.copy name
    _<-evaluate (T.length copied)
    human ctx (RenameAgentTo who copied))
  rename<-command "hide.sidebar.agents.rename" "Rename" (\ctx who->
    if Sidebar.sidebarOrigin host ctx/=Menu.HumanMenu then pure (Left (CommandRejected "Agent forms require the human.")) else do
      selected<-A.statusAgent (agentHub runtime) A.Human who
      case selected of
        Left err->pure (Left (CommandRejected err))
        Right entry->do
          let name=case parseMaybe (withObject "Agent" (.: "name")) entry of Just value->value; Nothing->A.agentIdText who
          prepared<-Form.prepareForm Form.PrivateForm (Form.InputFormSpec "Rename agent" "Name" name "Rename")
            (Form.formAction registry renameTo (\text->(who,text)) (\_ reply->pure reply))
          pure (Sidebar.formReply host <$> prepared))
  configureAgent<-command "hide.sidebar.agents.configure" "Apply agent setting"
    (\ctx (receipt,option,value)->human ctx (ConfigureAgent receipt option value))
  configureCompletion<-command "hide.sidebar.completion.configure" "Apply completion setting"
    (\ctx (target,option,value)->human ctx (ConfigureCompletion target option value))
  let choiceSpec title options current=Form.ChoiceFormSpec title "Provider choices"
        [(ident,T.take 256 (T.map (\c->if c<' ' || c=='\DEL' then ' ' else c) label)) | (ident,label)<-options]
        (if current `elem` map fst options then current else maybe "" fst (listToMaybe options)) "Apply"
      configurationCommand category ctx who
        | Sidebar.sidebarOrigin host ctx/=Menu.HumanMenu=pure (Left (CommandRejected "Agent forms require the human."))
        | otherwise=do
            -- tickConversation keeps the primary hub advertisement current.
            -- Opening may show old metadata; final ConfigureAgent sync/currentness
            -- remains the authority and cannot apply a replaced receipt.
            captured<-A.agentConfiguration (agentHub runtime) who
            case captured of
              Right (receipt,choices) | [choice]<-[choice | choice<-choices,A.configCategory choice==category]->do
                prepared<-Form.prepareForm Form.ReadableForm (choiceSpec "Agent setting" (A.configValues choice) (A.configCurrent choice))
                  (Form.formAction registry configureAgent (\value->(receipt,A.configId choice,value)) (\_ reply->pure reply))
                pure (Sidebar.formReply host <$> prepared)
              _->pure (Left (CommandRejected "The agent has not advertised these choices."))
      completionCommand category ctx target
        | Sidebar.sidebarOrigin host ctx/=Menu.HumanMenu=pure (Left (CommandRejected "Completion forms require the human."))
        | otherwise=do
            captured<-completionChoices autocomplete target category
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
      completionNode (CompletionSummary target state)=P.NodeDef
        (P.NodeInfo (completionNodeId target) ("ACP completion  "<>state) "" False Nothing)
        (Just (P.treeAction registry completionOpen target (\_ value->pure value)))
        [P.ActionMenu title (P.treeAction registry setting target (\_ value->pure value)) | (title,setting)<-[("Model",completionModel),("Effort",completionEffort)]]
  provider<-P.registerTree registry "hide.sidebar.agents" root (\_ (P.ChildRequest parent cursor)->do
    Snapshot _ _ values completion<-readIORef source
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
        Snapshot revision _ previous oldCompletion<-readIORef source
        if next==previous && nextCompletion==oldCompletion then pure () else do
          affectedNodes<-prepareParents previous next
          writeIORef source (Snapshot (revision+1) affectedNodes next nextCompletion)
  withAsync worker (const (use (AgentSidebar host (P.treeReference provider) source pending)))
  where
    hidden=Codec Null (const (Left "Agent arguments are host-captured.")) (const Null)
    prepare=do
      summaries<-A.agentSummaries (agentHub runtime)
      let bounded=take 1024 summaries
      _<-evaluate (force [(A.agentIdText (A.summaryId summary),A.summaryName summary,fmap A.agentIdText (A.summaryParent summary),A.summaryStatus summary) | summary<-bounded])
      values<-evaluate (M.fromList [(A.summaryId summary,summary) | summary<-bounded])
      completion<-completionSummary autocomplete
      pure (values,completion)
    prepareParents previous next=do
      let affectedNodes=parents previous next
      _<-evaluate (force (map P.nodeIdText affectedNodes))
      pure affectedNodes
    parents previous next=rootId:map agentNode (S.toList (S.fromList (mapMaybe A.summaryParent (M.elems previous++M.elems next))))

-- | /O(1)/ revision admission, then at most four scoped invalidations. Provider
-- callbacks and metadata/history scans never run on this owner tick.
tickAgentSidebar :: AgentSidebar c r -> IO ()
tickAgentSidebar (AgentSidebar host owner source ref)=do
  Snapshot revision changed _ _<-readIORef source
  (adopted,waiting)<-readIORef ref
  let requested=if revision==adopted then waiting else changed
  retained<-submit (4::Int) requested
  writeIORef ref (revision,retained)
  where
    submit _ []=pure []
    submit 0 pending=pure pending
    submit budget pending@(node:rest)=do
      accepted<-Sidebar.tryInvalidateTree host owner node
      if accepted then submit (budget-1) rest else pure pending
