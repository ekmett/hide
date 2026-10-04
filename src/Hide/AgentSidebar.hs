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
import Control.Monad (forever,foldM)
import Data.Aeson (Value(Null))
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Hide.AgentHub as A
import Hide.AgentRuntime (AgentRuntime,agentHub)
import Hide.Autocomplete (Autocomplete,completionSummary)
import Hide.AgentSidebarTypes
import Hide.Model (Desktop)
import Hide.Plugin.Command
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Tree as P
import Hide.SidebarCommands

data Snapshot = Snapshot !Integer ![P.NodeId] !(M.Map A.AgentId A.AgentSummary) !(Maybe CompletionSummary)
data AgentSidebar = AgentSidebar !P.TreeRef !(IORef Snapshot) !(IORef (Integer,[P.NodeId]))
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
withAgentSidebar :: SidebarHost -> AgentRuntime -> Autocomplete -> (AgentSidebar -> IO a) -> IO a
withAgentSidebar host runtime autocomplete use=withRegistry $ \registry->do
  (initial,initialCompletion)<-prepare
  affected<-prepareParents M.empty initial
  source<-newIORef (Snapshot 1 affected initial initialCompletion)
  pending<-newIORef (0,[])
  let command name title run=registerCommand registry (CommandDef name title hidden hidden run) >>= either (ioError . userError . show) pure
      human ctx value=if sidebarOrigin ctx==Menu.HumanMenu then pure (Right (SidebarAgent value)) else pure (Left (CommandRejected "Agent sidebar actions require the human."))
  open<-command "hide.sidebar.agents.open" "Conversation" (\ctx who->human ctx (ShowAgent who))
  rename<-command "hide.sidebar.agents.rename" "Rename" (\ctx who->human ctx (RenameAgent who))
  let configurationCommand category ctx who=do
        captured<-A.agentConfiguration (agentHub runtime) who
        case captured of
          Right (receipt,choices) | [choice]<-[choice | choice<-choices,A.configCategory choice==category]->do
            let options=A.configValues choice
                selected=length (takeWhile ((/=A.configCurrent choice).fst) options)
            _<-evaluate (force options)
            human ctx (ShowAgentConfiguration receipt (A.configId choice) options selected)
          _->pure (Left (CommandRejected "The agent has not advertised these choices."))
  model<-command "hide.sidebar.agents.model" "Model" (configurationCommand "model")
  effort<-command "hide.sidebar.agents.effort" "Effort" (configurationCommand "thought_level")
  completionOpen<-command "hide.sidebar.completion.open" "Completion conversation" (\ctx target->human ctx (ShowCompletion target))
  completionModel<-command "hide.sidebar.completion.model" "Completion model" (\ctx target->human ctx (ChooseCompletion target "model"))
  completionEffort<-command "hide.sidebar.completion.effort" "Completion effort" (\ctx target->human ctx (ChooseCompletion target "thought_level"))
  create<-command "hide.sidebar.agents.new" "New Agent" (\ctx ()->human ctx NewAgent)
  let root=P.NodeDef (P.NodeInfo rootId "Agents" "" True Nothing) Nothing [P.ActionMenu "New Agent" (P.treeAction registry create () (\_ value->pure value))]
      node values summary=
        let who=A.summaryId summary
            branch=any ((==Just who).A.summaryParent) (M.elems values)
            action=P.treeAction registry open who (\_ value->pure value)
        in P.NodeDef (P.NodeInfo (agentNode who) (A.summaryName summary<>"  "<>A.summaryStatus summary) "" branch Nothing)
             (if branch then Nothing else Just action)
             ([P.ActionMenu "Conversation" action | branch]++[P.ActionMenu "Rename" (P.treeAction registry rename who (\_ value->pure value))]++
              [P.ActionMenu title (P.treeAction registry setting who (\_ value->pure value)) | (title,setting)<-[("Model",model),("Effort",effort)]])
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
  publishTreeFromHost host provider
  let worker=forever $ do
        threadDelay 250000
        (next,nextCompletion)<-prepare
        Snapshot revision _ previous oldCompletion<-readIORef source
        if next==previous && nextCompletion==oldCompletion then pure () else do
          affectedNodes<-prepareParents previous next
          writeIORef source (Snapshot (revision+1) affectedNodes next nextCompletion)
  withAsync worker (const (use (AgentSidebar (P.treeReference provider) source pending)))
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
tickAgentSidebar :: AgentSidebar -> SidebarHost -> Desktop -> IO Desktop
tickAgentSidebar (AgentSidebar owner source ref) host d=do
  Snapshot revision changed _ _<-readIORef source
  (adopted,waiting)<-readIORef ref
  let requested=if revision==adopted then waiting else changed
      (begin,rest)=splitAt 4 requested
  writeIORef ref (revision,rest)
  foldM (\current node->refreshTreeFromHost host owner node current) d begin
