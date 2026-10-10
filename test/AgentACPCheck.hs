-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : AgentACPCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module AgentACPCheck (checks) where

import Control.Concurrent.Async (withAsync, poll, wait, cancel)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad (unless, forM_, when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import qualified Hide.ACP as A
import Hide.AgentACP
import Hide.AgentHub

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root -> do
  let script=root </> "provider.py"
      logPath=root </> "requests.jsonl"
      launch=A.ProviderLaunch "python3" [script] [("LOG",logPath)]
      spec=SpawnSpec "worker" "Inspect parser" root Shared Fresh Nothing Nothing
      request=StartRequest (AgentId "agent-1") Human spec Nothing Nothing
      bearer=T.replicate 12 "ab19"
      server=object ["name" .= ("editor"::T.Text),"command" .= ("/bridge"::T.Text),"args" .= (["--mcp-editor","session"]::[T.Text]),
        "env" .= [object ["name" .= ("THC_EDIT_MCP_TOKEN"::T.Text),"value" .= bearer]]]
      logs=mapMaybe decodeStrict' . BS.lines <$> BS.readFile logPath
      check label ok=unless ok (error label)
      right label result=either (error . ((label++": ")++) . T.unpack) pure result
      prompt text=HubMessage 1 (Agent (AgentId "sibling")) text False
  check "fork is never inferred from absent marker" (not (supportsFork (parseCapabilities (object []) (object []))))
  let initialized=object ["agentCapabilities" .= object ["sessionCapabilities" .= object ["fork" .= object []]]]
      advertisedOptions=object ["configOptions" .= [object ["id" .= ("model"::T.Text),"type" .= ("select"::T.Text),"category" .= ("model"::T.Text),"currentValue" .= ("m"::T.Text),"options" .= [object ["name" .= ("Group"::T.Text),"options" .= [object ["value" .= ("m"::T.Text),"name" .= ("Model"::T.Text)]]]]]]]
  check "actual advertised nested choices and fork supported" (supportsFork (parseCapabilities initialized advertisedOptions) && length (configChoices (parseCapabilities initialized advertisedOptions))==1)
  writeFile script fixture
  events<-newIORef ([]::[(T.Text,Value)])
  asked<-newEmptyMVar
  decision<-newEmptyMVar
  let permission value=putMVar asked value >> takeMVar decision
      emit (ProviderUpdate kind value)=modifyIORef' events (++[(kind,value)])
      emit (ProviderUsage used size)=modifyIORef' events (++[("usage",object ["used" .= used,"size" .= size])])
      emit (ProviderCapabilities caps)=modifyIORef' events (++[("capabilities",String (T.pack (show caps)))])
      emit ProviderClosed=pure ()
  let launcher = startACPDriver launch [] "" (const (pure Nothing))
  (currentChoices, releasesCapacity) <- bracket (newAgentHub (HubLimits 1 0) launcher) closeAgentHub $ \hub -> do
    ident <- spawnAgent hub Human spec {spawnModel=Just "model-b",spawnEffort=Just "high"} >>= right "configured child"
    current <- statusAgent hub Human ident >>= right "configured status"
    let options = field "capabilities" current >>= field "configOptions" :: Maybe [Value]
        actual = [value | option <- maybe [] id options, Just value <- [field "currentValue" option :: Maybe T.Text]]
    ticket <- sendAgent hub Human ident "disconnect" >>= right "disconnect prompt"
    _ <- waitAgent hub Human ident ticket 2000 >>= right "disconnect completion"
    replacement <- spawnAgent hub Human spec {spawnName="Replacement"}
    pure ("model-b" `elem` actual && "high" `elem` actual, either (const False) (const True) replacement)
  let failures = [label | (label,passed) <-
        [("directory reflects configured provider model and effort",currentChoices)
        ,("provider exit releases active capacity",releasesCapacity)], not passed]
  check (unlines failures) (null failures)
  writeFile logPath ""
  driver<-startACPDriver launch [server] "Initial context marker" permission request emit >>= right "start adapter"
  bracket (pure driver) driverStop $ \running -> do
    started<-logs
    check "startup never sends an automatic prompt" (all ((/=Just ("session/prompt"::T.Text)).field "method") started)
    check "private reference remains separate from public capabilities" (driverSessionKey running=="private-child-key" && not (null (configChoices (driverCapabilities running))))
    result<-driverConfigure running [("model-id","model-b"),("effort-id","high")]
    check "advertised model and effort selections apply" (case result of Right caps -> map configCurrent (configChoices caps)==["model-b","high"]; _ -> False)
    rejected<-driverConfigure running [("approval-mode","allow-all")]
    check "configuration never accepts unadvertised authority settings" (case rejected of Left _->True; _->False)
    completed<-driverDeliver running (prompt "ordinary")
    check "provider completion reaches driver caller" (case completed of Right value->field "stopReason" value==Just ("end_turn"::T.Text); _->False)
    observed<-readIORef events
    check "public updates include bounded text and tools without provider keys" (any ((=="output").fst) observed && any ((=="tool").fst) observed && not ("private-child-key" `T.isInfixOf` T.pack (show observed)))
    check "provider context usage reaches child updates" (any (\(kind,value)->kind=="usage" && field "used" value==Just (120::Integer) && field "size" value==Just (1000::Integer)) observed)
    check "child plan events retain public content without provider keys"
      (any (\(kind,value)->kind=="plan" && "Plan [private]" `T.isInfixOf` T.pack (show value) && not ("raw-plan-detail" `T.isInfixOf` T.pack (show value))) observed)
    let toolUpdates=[value | ("tool",value)<-observed]
    check "child tool updates retain call identity and omit absent titles"
      (length toolUpdates==2 && all ((==Just ("inspect-1"::T.Text)).field "toolCallId") toolUpdates &&
       field "title" (last toolUpdates)==(Nothing::Maybe T.Text) && field "status" (last toolUpdates)==Just ("completed"::T.Text))
    writeIORef events []
    split<-driverDeliver running (prompt "split") >>= right "split output"
    splitEvents<-readIORef events
    let joined=T.concat [text | ("output",value)<-splitEvents,Just text<-[field "text" value]]
    check "streamed session reference stays private across chunk boundaries" (not ("private-child-key" `T.isInfixOf` joined) && field "text" split==Just ("Output [private] suffix"::T.Text))
    forM_ ["whole","split"] $ \mode->do
      writeIORef events []
      credentialResult<-driverDeliver running (prompt ("credentials "<>mode)) >>= right "credential echo"
      echoed<-readIORef events
      let texts kind=T.concat [text | (label,value)<-echoed,label==kind,Just text<-[field "text" value]]
      check "MCP bearer is redacted from whole and split output and thought streams"
        (all (\kind->not (bearer `T.isInfixOf` texts kind) && "[private]" `T.isInfixOf` texts kind) ["output","thought"]
          && not (bearer `T.isInfixOf` T.pack (show credentialResult)))
      check "MCP bearer is redacted from echoed tool descriptors"
        (not (bearer `T.isInfixOf` T.pack (show echoed)) && any ((=="tool").fst) echoed)
    writeIORef events []
    _<-driverDeliver running (prompt "configuration") >>= right "configuration update"
    configEvents<-readIORef events
    check "dynamic capability labels and values cannot expose private references or MCP credentials"
      (any ((=="capabilities").fst) configEvents && not (bearer `T.isInfixOf` T.pack (show configEvents)) && not ("private-child-key" `T.isInfixOf` T.pack (show configEvents)))
    unsupportedSteer<-driverSteer running (prompt "not sent")
    check "unadvertised steering is rejected before RPC" (case unsupportedSteer of Left _->True; _->False)
    large<-driverDeliver running (prompt "large") >>= right "large output"
    check "large provider output is bounded" (maybe False ((<=131072).T.length) (field "text" large) && field "truncated" large==Just True)
    entries<-logs
    let initial=[p | entry<-entries,field "method" entry==Just ("initialize"::T.Text),Just p<-[field "params" entry::Maybe Value]]
        prompts=[p | entry<-entries,field "method" entry==Just ("session/prompt"::T.Text),Just p<-[field "params" entry::Maybe Value]]
    check "native filesystem and terminal capabilities are not advertised" (case initial of p:_->(field "clientCapabilities" p >>= field "terminal")==Just False && (field "clientCapabilities" p >>= field "fs" >>= field "readTextFile")==Just False; _->False)
    check "prompt distinguishes sibling message and supplies naming/context instructions" (all (`T.isInfixOf` T.pack (show prompts)) ["sibling","not the human","Initial context marker","agent_rename"])
    withAsync (driverDeliver running (prompt "permission")) $ \pending -> do
      shown<-timeout 2000000 (takeMVar asked)
      check "provider permission is forwarded to human callback" (maybe False ((=="Write file").permissionTitle) shown)
      check "human sees bounded tool details without private reference" (maybe False (\value->"/proposed.txt" `T.isInfixOf` permissionDetails value && not ("private-child-key" `T.isInfixOf` permissionDetails value) && not (bearer `T.isInfixOf` permissionDetails value) && T.length (permissionDetails value)<=8192) shown)
      check "provider cannot approve its own request" . maybe True (const False) =<< poll pending
      putMVar decision (Just "allow-once")
      check "approved callback releases the provider" . either (const False) (const True) =<< wait pending
    entriesAfter<-logs
    let native=[entry | entry<-entriesAfter,field "id" entry==Just ("native-read"::T.Text) || field "id" entry==Just ("native-terminal"::T.Text)]
    check "unsupported native requests get explicit errors" (length native==2 && all (maybe False (const True) . (field "error" :: Value -> Maybe Value)) native)
    withAsync (driverDeliver running (prompt "permission")) $ \pending -> do
      _<-timeout 2000000 (takeMVar asked) >>= maybe (error "permission callback missing") pure
      putMVar decision (Just "made-up-approval")
      _<-wait pending
      invalidLog<-logs
      let outcomes=[outcome | entry<-invalidLog,field "id" entry==Just ("permission"::T.Text),Just outcome<-[field "result" entry::Maybe Value]]
      check "unadvertised permission choice cancels request" (case reverse outcomes of outcome:_->(field "outcome" outcome >>= field "outcome")==Just ("cancelled"::T.Text); _->False)
    withAsync (driverDeliver running (prompt "permission")) $ \pending -> do
      _<-timeout 2000000 (takeMVar asked) >>= maybe (error "permission callback missing") pure
      driverCancel running
      ended<-timeout 4000000 (wait pending)
      check "cancel interrupts human approval wait without self-approval" (case ended of Just (Left _)->True; _->False)
    check "stopped provider rejects new work" . maybe False (const True) =<< timeout 2000000 (driverStop running)
    stopped<-driverDeliver running (prompt "ordinary")
    check "stopped provider does not hang on requests" (case stopped of Left _->True; _->False)
  let forked=request {startSpec=spec {spawnContext=Fork (AgentId "parent")},startSource=Just (PrivateSource (AgentId "parent") "private-parent-key")}
  beforeFork<-length <$> logs
  unsupported<-startACPDriver launch [] "" (const (pure Nothing)) forked emit
  check "unadvertised fork fails without starting a fresh session" (case unsupported of Left _->True; _->False)
  forkLog<-drop beforeFork <$> logs
  check "unsupported fork never sends session/new" (all ((/=Just ("session/new"::T.Text)).field "method") forkLog)
  forkDriver<-startACPDriver launch {A.environment=("FORK","yes"):A.environment launch} [] "" (const (pure Nothing)) forked emit >>= right "real fork"
  bracket (pure forkDriver) driverStop $ \running -> check "fork gets a distinct private provider session" (supportsFork (driverCapabilities running) && driverSessionKey running=="private-fork-key")
  reused<-startACPDriver launch {A.environment=[("FORK","yes"),("REUSE","yes")]++A.environment launch} [] "" (const (pure Nothing)) forked emit
  check "fork may not silently reuse source session" (case reused of Left _->True; _->False)
  let resumed=request {startResume=Just "private-saved-key"}
  beforeUnsupported<-length <$> logs
  unavailable<-startACPDriver launch [] "" (const (pure Nothing)) resumed emit
  check "unadvertised resume fails without new/fork/load" (case unavailable of Left _->True; _->False)
  unsupportedLog<-drop beforeUnsupported <$> logs
  check "unsupported resume performs only initialization" (map (field "method") unsupportedLog==[Just ("initialize"::T.Text)])
  forM_ ["load","resume"] $ \mode->do
    beforeLoad<-length <$> logs
    writeIORef events []
    loaded<-startACPDriver launch {A.environment=("RESUME",mode):A.environment launch} [server] "Do not replay context" (const (pure Nothing)) resumed emit >>= right "load saved session"
    bracket (pure loaded) driverStop $ \running->do
      opened<-drop beforeLoad <$> logs
      let methods=map (field "method") opened
      check "resume uses exactly the advertised load operation, without prompts"
        (methods==[Just ("initialize"::T.Text),Just ("session/"<>T.pack mode)] && driverSessionKey running=="private-saved-key")
      check "provider history replay does not duplicate local history" . null =<< readIORef events
      _<-driverDeliver running (prompt "after reconnect") >>= right "resumed prompt"
      delivered<-drop beforeLoad <$> logs
      check "first resumed prompt does not replay startup instructions or context"
        (not ("Do not replay context" `T.isInfixOf` T.pack (show delivered)) && not ("agent_rename" `T.isInfixOf` T.pack (show delivered)))
  changed<-startACPDriver launch {A.environment=[("RESUME","load"),("CHANGED","yes")]++A.environment launch} [] "" (const (pure Nothing)) resumed emit
  check "load rejects a replacement session identity" (case changed of Left _->True; _->False)
  stubborn<-startACPDriver launch {A.environment=("IGNORE_CANCEL","yes"):A.environment launch} [] "" (const (pure Nothing)) request emit >>= right "unresponsive provider"
  bracket (pure stubborn) driverStop $ \running -> withAsync (driverDeliver running (prompt "stall")) $ \pending -> do
    let awaitPrompt=do
          entries<-logs
          if any (T.isInfixOf "stall" . T.pack . show) entries then pure () else threadDelay 1000 >> awaitPrompt
    _<-timeout 2000000 awaitPrompt >>= maybe (error "unresponsive fixture prompt missing") pure
    driverCancel running
    stopped<-timeout 4000000 (wait pending)
    check "unresponsive cancellation closes driver within bound" (case stopped of Just (Left _)->True; _->False)
    closed<-driverDeliver running (prompt "ordinary")
    check "unresponsive provider cannot receive subsequent prompts" (case closed of Left _->True; _->False)
  forM_ ["accepted","idle-race","legacy"] $ \mode->do
    writeFile logPath ""
    steering<-startACPDriver launch {A.environment=("STEER","yes"):A.environment launch} [] "" (const (pure Nothing)) request emit >>= right "steering provider"
    bracket (pure steering) driverStop $ \running->withAsync (driverDeliver running (prompt "stall")) $ \turn->do
      let awaitPrompt=do entries<-logs; if any ((==Just ("session/prompt"::T.Text)).field "method") entries then pure () else threadDelay 1000 >> awaitPrompt
      _<-timeout 2000000 awaitPrompt >>= maybe (error "steer prompt missing") pure
      result<-driverSteer running (HubMessage 0 Human mode False)
      entries<-logs
      let requests=[value | entry<-entries,field "method" entry==Just ("_session/steering"::T.Text),Just value<-[field "params" entry::Maybe Value]]
      check "steer opts into host-owned idle handling and preserves human-peer attribution" (case requests of
        [value]->(field "_meta" value >>= field "steering" >>= field "idleBehavior")==Just ("promptRequired"::T.Text) && "not the human user seat" `T.isInfixOf` T.pack (show value)
        _->False)
      check "only injected steering is accepted" (either (const (mode/="accepted")) (const (mode=="accepted")) result)
      when (mode=="idle-race") (driverCancel running)
      _<-timeout 4000000 (wait turn) >>= maybe (error "steering fixture left a prompt alive") pure
      after<-logs
      check "rejected steering never automatically replays a prompt" (length [() | entry<-after,field "method" entry==Just ("session/prompt"::T.Text)]==1)
      when (mode=="legacy") $ do
        closed<-driverDeliver running (prompt "ordinary")
        check "legacy detached-turn response retires the provider" (case closed of Left _->True; _->False)
  writeFile logPath ""
  interrupted<-startACPDriver launch {A.environment=("STEER","yes"):A.environment launch} [] "" (const (pure Nothing)) request emit >>= right "cancelled steering provider"
  bracket (pure interrupted) driverStop $ \running->withAsync (driverDeliver running (prompt "stall")) $ \turn->do
    let awaitMethod method=do entries<-logs; if any ((==Just (method::T.Text)).field "method") entries then pure () else threadDelay 1000 >> awaitMethod method
    _<-timeout 2000000 (awaitMethod "session/prompt") >>= maybe (error "cancel-steer prompt missing") pure
    withAsync (driverSteer running (HubMessage 0 Human "withhold" False)) $ \pending->do
      _<-timeout 2000000 (awaitMethod "_session/steering") >>= maybe (error "cancel-steer request missing") pure
      cancel pending
    _<-timeout 4000000 (wait turn) >>= maybe (error "cancel-steer left provider alive") pure
    stopped<-driverDeliver running (prompt "must not replay")
    check "cancelled unknown steering retires its provider" (case stopped of Left _->True; _->False)
  let startup=startACPDriver launch {A.environment=("STARTUP_UPDATES","yes"):A.environment launch} [] "" (const (pure Nothing))
  bracket (newAgentHub (HubLimits 1 0) startup) closeAgentHub $ \hub->do
    ident<-spawnAgent hub Human spec >>= right "startup updates"
    let awaitUpdated=do
          current<-statusAgent hub Human ident >>= right "startup status"
          let options=maybe [] id (field "capabilities" current >>= field "configOptions"::Maybe [Value])
          if any ((==Just ("model-b"::T.Text)).field "currentValue") options && (field "contextUsage" current >>= field "used")==Just (42::Int)
            then pure () else threadDelay 1000 >> awaitUpdated
    result<-timeout 2000000 awaitUpdated
    check "adjacent opening capabilities and usage updates survive installation" (result==Just ())
  putStrLn "agent ACP checks passed"
  where
    field :: FromJSON a => Key -> Value -> Maybe a
    field key=parseMaybe (withObject "field" (.: key))
    temporary=do
      base<-getTemporaryDirectory
      (path,handle)<-openTempFile base "agent-acp-check"
      hClose handle
      removeFile path
      createDirectory path
      pure path

fixture :: String
fixture=unlines
  [ "import json,os,sys"
  , "log=open(os.environ['LOG'],'a',buffering=1); sid='private-child-key'; servers=[]; active=None; selected={'model-id':'model-a','effort-id':'low'}"
  , "def send(v): print(json.dumps(dict(jsonrpc='2.0',**v)),flush=True)"
  , "def reply(i,v): send({'id':i,'result':v})"
  , "def options(): return [{'id':'model-id','name':'Model','category':'model','type':'select','currentValue':selected['model-id'],'options':[{'value':'model-a','name':'A'},{'value':'model-b','name':'B'}]},{'id':'effort-id','name':'Effort','category':'thought_level','type':'select','currentValue':selected['effort-id'],'options':[{'value':'low','name':'Low'},{'value':'high','name':'High'}]}]"
  , "def update(v): send({'method':'session/update','params':{'sessionId':sid,'update':v}})"
  , "for line in sys.stdin:"
  , " m=json.loads(line); log.write(json.dumps(m)+'\\n'); method=m.get('method'); p=m.get('params',{}); i=m.get('id')"
  , " if method=='initialize': reply(i,{'protocolVersion':1,'_meta':{'steering':{'supported':os.environ.get('STEER')=='yes'}},'agentCapabilities':{'loadSession':os.environ.get('RESUME')=='load','sessionCapabilities':dict(([('fork',{})] if os.environ.get('FORK')=='yes' else [])+([('resume',{})] if os.environ.get('RESUME')=='resume' else []))}})"
  , " elif method=='session/new':"
  , "  servers=p['mcpServers']; reply(i,{'sessionId':sid,'configOptions':options()})"
  , "  if os.environ.get('STARTUP_UPDATES')=='yes': selected['model-id']='model-b'; update({'sessionUpdate':'config_option_update','configOptions':options()}); update({'sessionUpdate':'usage_update','used':42,'size':100})"
  , " elif method=='session/fork': sid=p['sessionId'] if os.environ.get('REUSE')=='yes' else 'private-fork-key'; reply(i,{'sessionId':sid,'configOptions':options()})"
  , " elif method in ['session/load','session/resume']: sid=p['sessionId']; servers=p['mcpServers']; update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'old provider history'}}); reply(i,{'sessionId':'replacement'} if os.environ.get('CHANGED')=='yes' else {'configOptions':options()})"
  , " elif method=='_session/steering':"
  , "  text=p['prompt'][-1]['text']"
  , "  if 'withhold' in text: continue"
  , "  outcome='promptRequired' if 'idle-race' in text else 'startedNewTurn' if 'legacy' in text else 'injected'; reply(i,{'outcome':outcome})"
  , "  if outcome=='injected': reply(active,{'stopReason':'end_turn'})"
  , " elif method=='session/set_config_option': selected[p['configId']]=p['value']; reply(i,{'configOptions':options()})"
  , " elif method=='session/prompt':"
  , "  active=i; text=p['prompt'][-1]['text']"
  , "  if 'disconnect' in text: sys.exit(0)"
  , "  elif 'stall' in text: pass"
  , "  elif 'permission' in text: send({'id':'permission','method':'session/request_permission','params':{'sessionId':sid,'toolCall':{'title':'Write file','rawInput':{'path':'/proposed.txt','sessionId':sid,'servers':servers}},'options':[{'optionId':'allow-once','name':'Allow once','kind':'allow_once'},{'optionId':'reject','name':'Reject','kind':'reject_once'}]}})"
  , "  elif 'configuration' in text:"
  , "   values=options(); values[0]['options'][0]['name']=sid+str(servers); update({'sessionUpdate':'config_option_update','configOptions':values}); reply(i,{'stopReason':'end_turn'})"
  , "  elif 'credentials' in text:"
  , "   tokens=[entry['value'] for server in servers for entry in server.get('env',[]) if entry.get('name')=='THC_EDIT_MCP_TOKEN']"
  , "   parts=[json.dumps(servers)] if 'whole' in text else [part for token in tokens for part in [token[:17],token[17:31],token[31:]]]"
  , "   for kind in ['agent_message_chunk','agent_thought_chunk']:"
  , "    for part in parts: update({'sessionUpdate':kind,'content':{'type':'text','text':part}}); reply(999,{})"
  , "   update({'sessionUpdate':'tool_call','toolCallId':json.dumps(servers),'title':json.dumps(servers),'status':'completed'}); reply(i,{'stopReason':'end_turn'})"
  , "  elif 'split' in text:"
  , "   for part in ['Output private-','child-','key suffix']: update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':part}}); reply(999,{})"
  , "   reply(i,{'stopReason':'end_turn'})"
  , "  elif 'large' in text:"
  , "   for n in range(24): update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'x'*9000}})"
  , "   reply(i,{'stopReason':'end_turn'})"
  , "  else:"
  , "   update({'sessionUpdate':'usage_update','used':120,'size':1000}); update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'Output '+sid}}); update({'sessionUpdate':'tool_call','toolCallId':'inspect-1','title':'Inspect source','status':'pending','rawInput':{'sessionId':sid}}); update({'sessionUpdate':'tool_call_update','toolCallId':'inspect-1','status':'completed'}); update({'sessionUpdate':'plan','entries':[{'content':'Plan '+sid,'priority':'high','status':'in_progress','raw':'raw-plan-detail'}]}); reply(i,{'stopReason':'end_turn'})"
  , " elif method=='session/cancel' and os.environ.get('IGNORE_CANCEL')!='yes': reply(active,{'stopReason':'cancelled'})"
  , " elif i=='permission':"
  , "  if m.get('result',{}).get('outcome',{}).get('outcome')=='selected': send({'id':'native-read','method':'fs/read_text_file','params':{'sessionId':sid,'path':'/secret'}})"
  , "  else: reply(active,{'stopReason':'cancelled'})"
  , " elif i=='native-read': send({'id':'native-terminal','method':'terminal/create','params':{'sessionId':sid,'command':'bad'}})"
  , " elif i=='native-terminal': reply(active,{'stopReason':'end_turn'})"
  ]
