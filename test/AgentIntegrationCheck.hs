{-# LANGUAGE OverloadedStrings #-}
module AgentIntegrationCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO (hClose,openTempFile)
import System.Timeout (timeout)
import THC.Edit.Conversation
import qualified THC.Edit.AgentRuntime as AR
import qualified THC.Edit.AgentHub as AH
import THC.Edit.GuestAccess (guestKeyboardAllowed,guestCommandAllowed,sanitizedBuffer)
import qualified Data.Map.Strict as M
import Data.List (findIndex)
import THC.Edit.Buffer (contents)
import qualified THC.Edit.Font as Font
import THC.Edit.ScreenCapture (capture)
import THC.Edit.Session
import THC.Edit.Model

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root ->
  environment "XDG_CONFIG_HOME" (Just (root </> "config")) $
  environment "XDG_DATA_HOME" (Just (root </> "data")) $
  environment "THC_EDIT_SESSION" Nothing $ do
    let config=root </> "config" </> "thc-edit"
        script=root </> "provider.py"
    createDirectoryIfMissing True config
    font<-Font.loadFont
    writeFile script fixture
    BL.writeFile (config </> "agents.json") (encode (object ["executable" .= ("python3"::T.Text),"arguments" .= [script]]))
    record<-newSessionRecord Nothing ["--",root]
    rememberSession record
    environment "THC_EDIT_SESSION" (Just (sessionId record)) $ withConversationAt root $ \conversation -> do
      let agents=conversationAgents conversation
          hub=AR.agentHub agents
          primary=AR.primaryAgent agents
          initial=(initialDesktop (80,25)) {defaultDirectory=Just root}
          send text d=snd <$> conversationEffects conversation (\x _->pure (False,x)) d [AgentAction "send" ["0",text,"false","false","false"]]
          tickUntil test d=do
            answer<-timeout 5000000 (loop d)
            maybe (error "Agent conversation integration timed out") pure answer
            where loop current=do next<-tickConversation conversation current; ok<-test next; if ok then pure next else threadDelay 10000 >> loop next
      connected<-send "Hello" initial >>= tickUntil (pure . (=="Agent: end_turn") . status)
      servers<-AR.primaryServers agents
      let tokens=[value | server<-servers, entry<-maybe [] id (field "env" server :: Maybe [Value]),Just value<-[field "value" entry :: Maybe T.Text]]
      let publicSafe desktop=do
            let readable=T.concat [body | ident<-M.keys (buffers desktop),Just body<-[sanitizedBuffer desktop ident]]
                private="private-main-key":tokens
            screen<-capture font desktop False
            ensure "primary transcript never exposes private keys through buffer reads" (all (not . (`T.isInfixOf` readable)) private)
            ensure "primary transcript never exposes private keys through screen capture" (all (not . (`T.isInfixOf` T.pack (show screen))) private)
      publicSafe connected
      split<-send "split-private" connected >>= tickUntil (\desktop->publicSafe desktop >> pure (status desktop=="Agent: end_turn"))
      publicSafe split
      nested<-send "nested-private" split >>= tickUntil (\desktop->publicSafe desktop >> pure (status desktop=="Agent: end_turn"))
      expanded<-snd <$> conversationEffects conversation (\x _->pure (False,x)) nested [AgentAction "copy" []]
      ensure "raw tool and plan details never retain bearer values" (all (not . (`T.isInfixOf` clipboard expanded)) ("private-main-key":tokens))
      peer<-AH.registerAgent hub "Peer" root (AH.AgentDriver root "private-peer-key" (AH.Capabilities False False [])
        (\_ ->pure (Right (AH.Capabilities False False []))) (\_ ->pure (Right Null)) (pure ()) (pure ())) >>= right
      ticket<-AH.sendAgent hub (AH.Agent peer) primary "Count files" >>= right
      finished<-tickUntil (\_ ->do result<-AH.waitAgent hub AH.Human primary ticket 0 >>= right; pure (field "status" result==Just ("completed"::T.Text))) connected
      result<-AH.waitAgent hub AH.Human primary ticket 0 >>= right
      ensure "peer message reaches the primary provider and returns text" (maybe False (T.isInfixOf "Count files") (field "result" result >>= field "text"))
      ensure "peer output cannot publish private provider references" (not ("private-main-key" `T.isInfixOf` T.pack (show result)))
      ensure "peer result cannot publish bearer tokens" (not (null tokens) && all (not . (`T.isInfixOf` T.pack (show result))) tokens)
      copied<-snd <$> conversationEffects conversation (\x _ ->pure (False,x)) finished [AgentAction "copy" []]
      ensure "peer transcript attributes the sender" (("Agent "<>AH.agentIdText peer) `T.isInfixOf` clipboard copied && "not the human user seat" `T.isInfixOf` clipboard copied)
      child<-AH.spawnAgent hub AH.Human (AH.SpawnSpec "Worker" "Permission" root AH.Shared AH.Fresh Nothing Nothing) >>= right
      _<-AH.sendAgent hub AH.Human child "permission" >>= right
      approval<-tickUntil (pure . maybe False (T.isPrefixOf "Agent permission:" . dialogTitle) . dialog) finished
      ensure "agents cannot approve their own UI" (not (guestKeyboardAllowed approval) && not (guestCommandAllowed AgentDirectory))
      _<-AH.cancelAgent hub AH.Human child >>= right
      cleared<-tickUntil (pure . maybe True (not . T.isPrefixOf "Agent permission:" . dialogTitle) . dialog) approval
      ensure "cancel removes stale human approval" (dialog cleared==Nothing)
      peerTicket<-AH.sendAgent hub (AH.Agent primary) peer "Peer attribution fixture" >>= right
      _<-AH.waitAgent hub AH.Human peer peerTicket 1000 >>= right
      directoryView<-snd <$> conversationEffects conversation (\x _->pure (False,x)) cleared [AgentAction "directory" []]
      directory<-AH.listAgents hub AH.Human >>= right
      let listed=maybe [] id (field "agents" directory :: Maybe [Value])
          index=maybe (error "Peer absent from directory") id (findIndex ((==Just (AH.agentIdText peer)) . field "id") listed)
      history<-snd <$> conversationEffects conversation (\x _->pure (False,x)) directoryView {dialog=Nothing} [AgentAction "directory-select" ["0",T.pack (show index)]]
      let historyText=T.concat [contents (documentBuffer doc) | doc<-M.elems (buffers history),documentLabel doc==Just "Agent: Peer"]
      ensure "history preserves peer identity instead of assigning the human seat" (("Agent "<>AH.agentIdText primary<>" (peer message)") `T.isInfixOf` historyText)
      _<-AH.endAgent hub AH.Human primary >>= right
      _<-tickConversation conversation finished
      invalid<-AH.statusAgent hub (AH.Agent primary) primary
      ensure "ended primary loses orchestration authority" (case invalid of Left _->True; _->False)
    corrupt<-newSessionRecord Nothing ["--",root]
    rememberSession corrupt
    checkpoint<-(++".agents.json") <$> checkpointPath (sessionId corrupt)
    writeFile checkpoint "{incomplete"
    environment "THC_EDIT_SESSION" (Just (sessionId corrupt)) $ withConversationAt root $ \conversation -> do
      noticed<-tickConversation conversation (initialDesktop (80,25))
      ensure "agent checkpoint failures reach the status line" ("checkpoint" `T.isInfixOf` status noticed && "retained" `T.isInfixOf` status noticed)
      consumed<-tickConversation conversation noticed {status="ordinary status"}
      ensure "agent runtime notices are shown only once" (status consumed=="ordinary status")
  where
    right=either (error . T.unpack) pure
    ensure label ok=unless ok (error label)

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "field" (.:key))
environment :: String -> Maybe String -> IO a -> IO a
environment key value action=bracket (lookupEnv key <* put value) put (const action)
  where put=maybe (unsetEnv key) (setEnv key)
temporary :: IO FilePath
temporary=do root<-getTemporaryDirectory; (path,h)<-openTempFile root "agent-integration"; hClose h; removeFile path; createDirectory path; canonicalizePath path
fixture :: String
fixture=unlines
  ["import sys,json,time"
  ,"tokens=[]; pending=None"
  ,"def send(x): print(json.dumps(x),flush=True)"
  ,"for line in sys.stdin:"
  ," r=json.loads(line); m=r.get('method'); p=r.get('params',{}); v={}"
  ," if m=='initialize': v={'protocolVersion':1,'agentCapabilities':{'loadSession':True}}"
  ," elif m in ('session/new','session/load'):"
  ,"  tokens=[e['value'] for s in p.get('mcpServers',[]) for e in s.get('env',[]) if e['name']=='THC_EDIT_MCP_TOKEN']"
  ,"  v={'sessionId':'private-main-key'}"
  ," elif m=='session/prompt':"
  ,"  text=\"\\n\".join(block['text'] for block in p['prompt'])+' private-main-key '+str(tokens)"
  ,"  if 'split-private' in text:"
  ,"   for kind in ['agent_message_chunk','user_message_chunk']:"
  ,"    for secret in ['private-main-key']+tokens:"
  ,"     for part in [secret[:8],secret[8:17],secret[17:]+' ']:"
  ,"      send({'jsonrpc':'2.0','method':'session/update','params':{'sessionId':'private-main-key','update':{'sessionUpdate':kind,'content':{'type':'text','text':part}}}}); send({'jsonrpc':'2.0','id':999,'result':{}}); time.sleep(0.03)"
  ,"   send({'jsonrpc':'2.0','id':r['id'],'result':{'stopReason':'end_turn'}}); continue"
  ,"  if 'nested-private' in text:"
  ,"   for kind in ['tool_call','plan']: send({'jsonrpc':'2.0','method':'session/update','params':{'sessionId':'private-main-key','update':{'sessionUpdate':kind,'toolCallId':'nested','title':str(tokens),'rawInput':{'nested':[{'private-main-key':tokens}]}}}})"
  ,"  if any(block['text'].strip().endswith('permission') for block in p['prompt']):"
  ,"   pending=r['id']; send({'jsonrpc':'2.0','id':'permit','method':'session/request_permission','params':{'sessionId':'private-main-key','toolCall':{'title':'Write file'},'options':[{'optionId':'yes','name':'Allow','kind':'allow_once'},{'optionId':'no','name':'Reject','kind':'reject_once'}]}}); continue"
  ,"  send({'jsonrpc':'2.0','method':'session/update','params':{'sessionId':'private-main-key','update':{'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':text}}}})"
  ,"  v={'stopReason':'end_turn'}"
  ," elif m=='session/cancel' and pending is not None:"
  ,"  send({'jsonrpc':'2.0','id':pending,'result':{'stopReason':'cancelled'}}); pending=None"
  ," if m and 'id' in r: send({'jsonrpc':'2.0','id':r['id'],'result':v})"]
