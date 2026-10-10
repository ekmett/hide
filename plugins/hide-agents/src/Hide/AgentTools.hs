{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.AgentTools
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- The linked agent plugin's strict orchestration tools. Parsing produces typed
-- requests; execution uses only public capabilities bound to the authenticated
-- caller and workspace by the host. Private hub/provider handles are absent.
module Hide.AgentTools (tools) where

import Control.Monad (unless)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Plugin.Agent
import Hide.Plugin.AgentServices
import Hide.Plugin.Command
import Hide.Plugin.Tool (Tool(..))

-- | Explicit schemas, policy metadata and codecs for the existing workflow.
-- No human-only configuration, steering or permission operation is registered.
tools :: [Tool AgentServices]
tools=[Tool name readonly (CommandDef ("hide.agents."<>T.drop 6 name) description
  (Codec (schema required properties) (decodeRequest name (map fst properties)) requestArguments)
  (Codec (object ["type" .= ("object"::Text)]) Right id)
  (\services request->fmap (either (Left . CommandRejected) Right) (perform services request)))
  | (name,description,readonly,required,properties)<-specs]

data Request = Directory | Spawn Text Text Context (Maybe Text) (Maybe Text) Workspace
  | Rename AgentId Text | Message AgentId Text | Wait AgentId Int Int
  | Cancel AgentId | End AgentId | History AgentId Int Int | Search AgentId Text Int Int

specs :: [(Text,Text,Bool,[Text],[(Text,Value)])]
specs=
  [("agent_directory","List session agents, parent/name, state, workspace, limits and provider-advertised model/effort choices. Private provider keys are omitted.",True,[],[])
  ,("agent_spawn","Create a child and enqueue its task, returning public agent metadata and a message ticket. Defaults to a separate worktree/editor from the caller's committed HEAD; requires Git. Explicit workspace mode shared opts into the caller's editor. No automatic display focus. Fork requires provider support and caller ownership.",False,["name","task"],
    [("name",string 80),("task",string 65536),("context",enum ["fresh","fork"]),("sourceAgentId",ident),("model",string 4096),("effort",string 4096),
     ("workspace",schema ["mode"] [("mode",enum ["shared","worktree"]),("ref",string 4096),("branch",string 4096),("name",string 4096)])])
  ,("agent_rename","Rename an agent; its stable ID and rename history remain. Names must be unique.",False,["agentId","name"],[("agentId",ident),("name",string 80)])
  ,("agent_message","Queue a message with authenticated sender attribution. The caller cannot impersonate a parent or the human. Returns a ticket for agent_wait.",False,["agentId","text"],[("agentId",ident),("text",string 65536)])
  ,("agent_wait","Wait at most 60 seconds for a message ticket; timeout returns its running state without cancelling it.",True,["agentId","ticket"],[("agentId",ident),("ticket",integer 1 2147483647),("timeoutMs",integer 0 60000)])
  ,("agent_cancel","Cancel an agent's current and queued messages, preserving the agent. Only the human or an ancestor may cancel it.",False,["agentId"],[("agentId",ident)])
  ,("agent_end","End an agent and descendants, retaining history and workspaces. Only the human or an ancestor may end it.",False,["agentId"],[("agentId",ident)])
  ,("agent_history","Read bounded retained history after an event index; nextAfter continues pagination and dropped reports lost older events.",True,["agentId"],historyProps)
  ,("agent_search","Search retained history for literal text, ignoring case. Results have the same bounded pagination as agent_history.",True,["agentId","query"],historyProps++[("query",string 4096)])]
  where
    ident=string 128
    historyProps=[("agentId",ident),("after",integer 0 2147483647),("limit",integer 1 100)]

schema :: [Text] -> [(Text,Value)] -> Value
schema required properties=object ["type" .= ("object"::Text),"required" .= required,
  "properties" .= object [K.fromText key .= value | (key,value)<-properties],"additionalProperties" .= False]
string :: Int -> Value
string maximumLength=object ["type" .= ("string"::Text),"minLength" .= (1::Int),"maxLength" .= maximumLength]
integer :: Int -> Int -> Value
integer lower upper=object ["type" .= ("integer"::Text),"minimum" .= lower,"maximum" .= upper]
enum :: [Text] -> Value
enum values=object ["type" .= ("string"::Text),"enum" .= values]

-- Parsing cannot capture an actor, working directory or provider handle.
decodeRequest :: Text -> [Text] -> Value -> Either Text Request
decodeRequest name allowed value=either (Left . T.pack) Right (parseEither (strictObject allowed parse) value)
  where
    parse o=case name of
      "agent_directory"->pure Directory
      "agent_spawn"->do
        childName<-text o "name" 80
        task<-text o "task" 65536
        context<-optionalText o "context" 5
        source<-fmap AgentId <$> optionalText o "sourceAgentId" 128
        origin<-case (context,source) of
          (Nothing,Nothing)->pure Fresh
          (Just "fresh",Nothing)->pure Fresh
          (Just "fork",Just ident)->pure (Fork ident)
          _->fail "Use fresh without sourceAgentId, or fork with sourceAgentId."
        model<-optionalText o "model" 4096
        effort<-optionalText o "effort" 4096
        workspace<-case KM.lookup "workspace" o of
          Nothing->pure (Worktree Nothing Nothing Nothing)
          Just entry->strictObject ["mode","ref","branch","name"] parseWorkspace entry
        pure (Spawn childName task origin model effort workspace)
      "agent_rename"->Rename <$> agentId o <*> text o "name" 80
      "agent_message"->Message <$> agentId o <*> text o "text" 65536
      "agent_wait"->Wait <$> agentId o <*> number o "ticket" Nothing 1 2147483647 <*> number o "timeoutMs" (Just 30000) 0 60000
      "agent_cancel"->Cancel <$> agentId o
      "agent_end"->End <$> agentId o
      "agent_history"->do (ident,after,count)<-history o; pure (History ident after count)
      "agent_search"->do (ident,after,count)<-history o; query<-text o "query" 4096; pure (Search ident query after count)
      _->fail "Unknown agent tool."
    agentId o=AgentId <$> text o "agentId" 128
    history o=(,,) <$> agentId o <*> number o "after" (Just 0) 0 2147483647 <*> number o "limit" (Just 50) 1 100
    parseWorkspace o=do
      mode<-text o "mode" 8
      ref<-optionalText o "ref" 4096
      branch<-optionalText o "branch" 4096
      feature<-optionalText o "name" 4096
      case mode of
        "shared" | all (==Nothing) [ref,branch,feature]->pure Shared
        "worktree"->pure (Worktree ref branch feature)
        _->fail "Use shared without worktree options, or worktree with optional ref, branch and name."

perform :: AgentServices -> Request -> IO (Either Text Value)
perform services request=case request of
  Directory->serviceAgents services
  Spawn name task origin model effort workspace->do
    created<-serviceSpawn services (SpawnSpec name task (serviceWorkspace services) workspace origin model effort)
    case created of
      Left err->pure (Left err)
      Right (ident,ticket)->fmap (\agent->object ["agent" .= agent,"ticket" .= ticket]) <$> serviceStatus services ident
  Rename ident name->accepted ident (serviceRename services ident name)
  Message ident body->fmap (\ticket->object ["agentId" .= agentIdText ident,"ticket" .= ticket]) <$> serviceMessage services ident body
  Wait ident ticket milliseconds->serviceWait services ident ticket milliseconds
  Cancel ident->accepted ident (serviceCancel services ident)
  End ident->accepted ident (serviceEnd services ident)
  History ident after count->fmap toJSON <$> serviceHistory services ident after count
  Search ident query after count->fmap toJSON <$> serviceSearch services ident query after count
  where accepted ident action=fmap (const (object ["agentId" .= agentIdText ident,"accepted" .= True])) <$> action

requestArguments :: Request -> Value
requestArguments request=object $ case request of
  Directory->[]
  Spawn name task origin model effort workspace->
    ["name" .= name,"task" .= task,"workspace" .= workspaceValue workspace]++
    ["model" .= value | Just value<-[model]]++["effort" .= value | Just value<-[effort]]++
    case origin of Fresh->["context" .= ("fresh"::Text)]; Fork ident->["context" .= ("fork"::Text),"sourceAgentId" .= agentIdText ident]
  Rename ident name->target ident++["name" .= name]
  Message ident body->target ident++["text" .= body]
  Wait ident ticket milliseconds->target ident++["ticket" .= ticket,"timeoutMs" .= milliseconds]
  Cancel ident->target ident
  End ident->target ident
  History ident after count->page ident after count
  Search ident query after count->page ident after count++["query" .= query]
  where
    target ident=["agentId" .= agentIdText ident]
    page ident after count=target ident++["after" .= after,"limit" .= count]
    workspaceValue Shared=object ["mode" .= ("shared"::Text)]
    workspaceValue (Worktree ref branch feature)=object (["mode" .= ("worktree"::Text)]++
      ["ref" .= value | Just value<-[ref]]++["branch" .= value | Just value<-[branch]]++["name" .= value | Just value<-[feature]])

strictObject :: [Text] -> (Object -> Parser a) -> Value -> Parser a
strictObject allowed parse=withObject "agent arguments" $ \o->do
  unless (all ((`elem` allowed) . K.toText) (KM.keys o)) (fail "Unsupported agent argument; caller identity and workspace path are host-controlled.")
  parse o
text :: Object -> Key -> Int -> Parser Text
text o key maximumLength=do
  value<-o .: key
  unless (not (T.null (T.strip value)) && T.length value<=maximumLength && not (T.any (=='\0') value))
    (fail (T.unpack (K.toText key)++" must be nonempty text within its declared length limit, without NUL."))
  pure value
optionalText :: Object -> Key -> Int -> Parser (Maybe Text)
optionalText o key maximumLength=if KM.member key o then Just <$> text o key maximumLength else pure Nothing
number :: Object -> Key -> Maybe Int -> Int -> Int -> Parser Int
number o key fallback lower upper=do
  value<-case KM.lookup key o of Nothing->maybe (fail ("Missing "++K.toString key)) pure fallback; Just entry->parseJSON entry
  unless (value>=lower && value<=upper) (fail (K.toString key++" is outside its declared range."))
  pure value
