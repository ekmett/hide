{-# LANGUAGE OverloadedStrings #-}
module AgentMCPCheck (checks) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (unless,forM_)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Text as T
import System.Directory (getTemporaryDirectory, canonicalizePath)
import Hide.AgentHub
import Hide.AgentMCP

checks :: IO ()
checks=do
  directory<-getTemporaryDirectory >>= canonicalizePath
  launched<-newIORef []
  configured<-newIORef []
  delivered<-newTVarIO []
  gate<-newTVarIO True
  let caps=Capabilities True True False [ConfigChoice "model-id" "model" "small" [("small","Small"),("large","Large")],ConfigChoice "effort-id" "thought_level" "low" [("low","Low"),("high","High")]]
      driver ident=AgentDriver directory ("private-test-key-"<>agentIdText ident) caps
        (\settings->modifyIORef' configured (++[settings]) >> pure (Right caps))
        (\message->do
          atomically (modifyTVar' delivered (++[message]))
          atomically (readTVar gate >>= check)
          pure (Right (String (messageText message))))
        (atomically (writeTVar gate True)) (atomically (writeTVar gate True)) (\_ ->pure (Left "unsupported"))
      launcher request _=modifyIORef' launched (++[request]) >> pure (Right (driver (startAgent request)))
      right=either (error . T.unpack) pure
      isLeft (Left _)=True; isLeft _=False
      identOf value=case field "agent" value >>= field "id" of Just ident->AgentId ident; _->error "Missing public agent ID"
      ticketOf value=maybe (error "Missing message ticket") id (field "ticket" value::Maybe Int)
      target ident=object ["agentId" .= agentIdText ident]
  bracket (newAgentHub (HubLimits 5 2) launcher) closeAgentHub $ \hub->do
    owner<-registerAgent hub "Main" directory (driver (AgentId "primary")) >>= right
    let call actor name args=agentTool hub actor directory name args
    listing<-call (Agent owner) "agent_directory" (object []) >>= right
    ensure "directory exposes actual provider model/effort options" (all (`BS.isInfixOf` BL.toStrict (encode listing)) ["model-id","large","effort-id"])
    ensure "directory omits private provider keys" (not ("private-test-key" `BS.isInfixOf` BL.toStrict (encode listing)))
    spawned<-call (Agent owner) "agent_spawn" (object ["name" .= ("Child"::T.Text),"task" .= ("Initial work"::T.Text),
      "model" .= ("large"::T.Text),"effort" .= ("high"::T.Text),"workspace" .= object ["mode" .= ("worktree"::T.Text),"ref" .= ("HEAD"::T.Text),"branch" .= ("feature/child"::T.Text),"name" .= ("child feature"::T.Text)]]) >>= right
    let child=identOf spawned
    completed<-call (Agent owner) "agent_wait" (object ["agentId" .= agentIdText child,"ticket" .= ticketOf spawned,"timeoutMs" .= (1000::Int)]) >>= right
    ensure "spawn queues initial task and returns a waitable ticket" (field "status" completed==Just ("completed"::T.Text) && field "result" completed==Just ("Initial work"::T.Text))
    requests<-readIORef launched
    ensure "host actor and cwd alone determine ownership and workspace root" (case requests of
      [request]->startOwner request==Agent owner && spawnDirectory (startSpec request)==directory &&
        spawnWorkspace (startSpec request)==Worktree (Just "HEAD") (Just "feature/child") (Just "child feature")
      _->False)
    choices<-readIORef configured
    ensure "provider-owned model and effort IDs are configured" ([("model-id","large"),("effort-id","high")] `elem` choices)
    _<-call (Agent child) "agent_rename" (object ["agentId" .= agentIdText owner,"name" .= ("Human seat"::T.Text)]) >>= right
    sent<-call (Agent child) "agent_message" (object ["agentId" .= agentIdText owner,"text" .= ("Result for parent"::T.Text)]) >>= right
    _<-call (Agent child) "agent_wait" (object ["agentId" .= agentIdText owner,"ticket" .= ticketOf sent,"timeoutMs" .= (1000::Int)]) >>= right
    messages<-readTVarIO delivered
    ensure "child messages cannot impersonate the human seat" (any (\m->messageText m=="Result for parent" && messageAuthor m==Agent child && not (messageIsUserSeat m)) messages)
    denied<-call (Agent child) "agent_end" (target owner)
    ensure "child cannot end its ancestor" (isLeft denied)
    before<-length <$> readIORef launched
    forM_ [object ["name" .= ("Spoof"::T.Text),"task" .= ("task"::T.Text),"parentId" .= agentIdText owner],
           object ["name" .= ("Spoof"::T.Text),"task" .= ("task"::T.Text),"actor" .= ("human"::T.Text)],
           object ["name" .= ("Spoof"::T.Text),"task" .= ("task"::T.Text),"cwd" .= directory],
           object ["name" .= ("Spoof"::T.Text),"task" .= ("task"::T.Text),"workspace" .= object ["mode" .= ("shared"::T.Text),"cwd" .= directory]]]
      (\args->call (Agent child) "agent_spawn" args >>= ensure "identity/path spoofing arguments are rejected" . isLeft)
    after<-length <$> readIORef launched
    ensure "spoofed calls never launch a provider" (before==after)
    forked<-call (Agent child) "agent_spawn" (object ["name" .= ("Fork"::T.Text),"task" .= ("Fork task"::T.Text),"context" .= ("fork"::T.Text),"sourceAgentId" .= agentIdText child]) >>= right
    forkRequests<-readIORef launched
    ensure "fork source passes through trusted core validation" (case reverse forkRequests of request:_->startSource request==Just (PrivateSource child ("private-test-key-"<>agentIdText child)); _->False)
    unsupported<-call Human "agent_spawn" (object ["name" .= ("Unknown model"::T.Text),"task" .= ("task"::T.Text),"model" .= ("invented"::T.Text)])
    ensure "unadvertised models return explicit failure" (isLeft unsupported)
    atomically (writeTVar gate False)
    pending<-call (Agent owner) "agent_message" (object ["agentId" .= agentIdText child,"text" .= ("Hold work"::T.Text)]) >>= right
    status<-call (Agent owner) "agent_wait" (object ["agentId" .= agentIdText child,"ticket" .= ticketOf pending,"timeoutMs" .= (0::Int)]) >>= right
    ensure "zero wait reports running without cancelling" (field "status" status==Just ("running"::T.Text))
    _<-call (Agent owner) "agent_cancel" (target child) >>= right
    cancelled<-call (Agent owner) "agent_wait" (object ["agentId" .= agentIdText child,"ticket" .= ticketOf pending,"timeoutMs" .= (1000::Int)]) >>= right
    ensure "cancel dispatch resolves the task ticket" (field "status" cancelled==Just ("cancelled"::T.Text))
    history<-call (Agent owner) "agent_history" (object ["agentId" .= agentIdText owner,"limit" .= (1::Int)]) >>= right
    ensure "history exposes bounded pagination" (field "hasMore" history==Just True && maybe False ((==1).length) (field "events" history::Maybe [Value]))
    search<-call (Agent owner) "agent_search" (object ["agentId" .= agentIdText owner,"query" .= ("result for parent"::T.Text)]) >>= right
    ensure "search dispatch finds literal text ignoring case" ("Result for parent" `BS.isInfixOf` BL.toStrict (encode search))
    forM_ [("agent_directory",Null),("agent_directory",object ["parent" .= ("human"::T.Text)]),
      ("agent_spawn",object ["name" .= True,"task" .= ("x"::T.Text)]),
      ("agent_spawn",object ["name" .= ("x"::T.Text),"task" .= ("x"::T.Text),"model" .= Null]),
      ("agent_spawn",object ["name" .= ("x"::T.Text),"task" .= ("x"::T.Text),"context" .= ("fork"::T.Text)]),
      ("agent_wait",object ["agentId" .= agentIdText child,"ticket" .= (1::Int),"timeoutMs" .= (60001::Int)]),
      ("agent_history",object ["agentId" .= agentIdText child,"limit" .= (101::Int)]),
      ("agent_search",object ["agentId" .= agentIdText child,"query" .= (""::T.Text)]),
      ("agent_unknown",object [])] $ \(name,args)->call (Agent owner) name args >>= ensure "invalid types/ranges/unknown tools fail explicitly" . isLeft
    _<-call (Agent owner) "agent_end" (target child) >>= right
    ended<-statusAgent hub Human (identOf forked) >>= right
    ensure "end dispatch terminates the descendant subtree" (field "status" ended==Just ("ended"::T.Text))
    inactive<-call (Agent child) "agent_directory" (object [])
    ensure "ended authenticated callers lose authority" (isLeft inactive)
  ensure "all public tool schemas are strict objects with annotations" (length agentTools==9 && length agentToolNames==9 && all strict agentTools)
  putStrLn "Agent MCP checks passed"
  where
    strict value=case (field "inputSchema" value,field "annotations" value::Maybe Value) of
      (Just schema,Just _)->field "additionalProperties" schema==Just False
      _->False

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "field" (.:key))
ensure :: String -> Bool -> IO ()
ensure label ok=unless ok (error label)
