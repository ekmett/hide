{-# LANGUAGE OverloadedStrings #-}
-- | Strict MCP schemas and dispatch for agent orchestration.
--
-- The host supplies the actor and working directory; tool arguments cannot replace
-- either. Creation combines hub reservation with explicit first-message enqueueing,
-- and failed/interrupted post-creation work ends the new child. Authority remains
-- with the hub rather than being inferred from JSON schema hints.
module Hide.AgentMCP (agentTools, agentToolNames, agentTool) where

import Control.Exception (mask, onException)
import Control.Monad (unless, void)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import Hide.AgentHub

agentToolNames :: [Text]
agentToolNames=[name | (name,_,_,_,_)<-specs]

-- | Published orchestration schemas; human-only configuration/steering hooks are omitted.
agentTools :: [Value]
agentTools=[object ["name" .= name,"description" .= description,"inputSchema" .= schema required properties,
  "annotations" .= object ["readOnlyHint" .= readonly,"destructiveHint" .= not readonly,"openWorldHint" .= not readonly]]
  | (name,description,readonly,required,properties)<-specs]

specs :: [(Text,Text,Bool,[Text],[(Text,Value)])]
specs=
  [("agent_directory","List session agents, parent/name, state, workspace, limits and provider-advertised model/effort choices. Private provider keys are omitted.",True,[],[])
  ,("agent_spawn","Create a child and enqueue its task, returning public agent metadata and a message ticket. Uses the caller's workspace; worktree starts from committed source. No automatic display focus. Fork requires provider support and caller ownership.",False,["name","task"],
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

-- | Validate names, fields and bounds, then dispatch as the host-supplied actor.
agentTool :: AgentHub -> Actor -> FilePath -> Text -> Value -> IO (Either Text Value)
agentTool hub actor directory name args=case [properties | (key,_,_,_,properties)<-specs,key==name] of
  []->pure (Left "Unknown agent tool.")
  properties:_->case parseEither (strictObject (map fst properties) $ \o -> dispatch o) args of
    Left err->pure (Left (T.pack err))
    Right action->action
  where
    dispatch o=case name of
      "agent_directory"->pure (listAgents hub actor)
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
          Nothing->pure Shared
          Just value->strictObject ["mode","ref","branch","name"] parseWorkspace value
        let spec=SpawnSpec childName task directory workspace origin model effort
        pure $ mask $ \restore->do
          created<-restore (spawnAgent hub actor spec)
          case created of
            Left err->pure (Left err)
            Right ident->do
              let cleanup=void (endAgent hub actor ident)
              (do
                queued<-sendAgent hub actor ident task
                case queued of
                  Left err->cleanup >> pure (Left err)
                  Right ticket->fmap (\agent->object ["agent" .= agent,"ticket" .= ticket]) <$> statusAgent hub actor ident)
                `onException` cleanup
      "agent_rename"->do
        ident<-agentId o
        newName<-text o "name" 80
        pure (accepted ident (renameAgent hub actor ident newName))
      "agent_message"->do
        ident<-agentId o
        body<-text o "text" 65536
        pure (fmap (\ticket->object ["agentId" .= agentIdText ident,"ticket" .= ticket]) <$> sendAgent hub actor ident body)
      "agent_wait"->do
        ident<-agentId o
        ticket<-number o "ticket" Nothing 1 2147483647
        milliseconds<-number o "timeoutMs" (Just 30000) 0 60000
        pure (waitAgent hub actor ident ticket milliseconds)
      "agent_cancel"->do ident<-agentId o; pure (accepted ident (cancelAgent hub actor ident))
      "agent_end"->do ident<-agentId o; pure (accepted ident (endAgent hub actor ident))
      "agent_history"->do
        (ident,after,count)<-history o
        pure (historyAgent hub actor ident after count)
      "agent_search"->do
        (ident,after,count)<-history o
        query<-text o "query" 4096
        pure (searchAgentHistory hub actor ident query after count)
      _->fail "Unknown agent tool."
    agentId o=AgentId <$> text o "agentId" 128
    accepted ident action=fmap (const (object ["agentId" .= agentIdText ident,"accepted" .= True])) <$> action
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
