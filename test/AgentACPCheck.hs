{-# LANGUAGE OverloadedStrings #-}
module AgentACPCheck (checks) where

import Control.Concurrent.Async (withAsync, poll, wait)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad (unless)
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
import qualified THC.Edit.ACP as A
import THC.Edit.AgentACP
import THC.Edit.AgentHub

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root -> do
  let script=root </> "provider.py"
      logPath=root </> "requests.jsonl"
      launch=A.Launch "python3" [script] [("LOG",logPath)]
      spec=SpawnSpec "worker" "Inspect parser" root Shared Fresh Nothing Nothing
      request=StartRequest (AgentId "agent-1") Human spec Nothing
      server=object ["name" .= ("editor"::T.Text),"command" .= ("/bridge"::T.Text),"args" .= ["private-actor-token"::T.Text],"env" .= ([]::[Value])]
      logs=mapMaybe decodeStrict' . BS.lines <$> BS.readFile logPath
      check label ok=unless ok (error label)
      right label result=either (error . ((label++": ")++) . T.unpack) pure result
      prompt text=HubMessage 1 (Agent (AgentId "sibling")) text False
  writeFile script fixture
  events<-newIORef ([]::[(T.Text,Value)])
  asked<-newEmptyMVar
  decision<-newEmptyMVar
  let permission value=putMVar asked value >> takeMVar decision
      emit (ProviderUpdate kind value)=modifyIORef' events (++[(kind,value)])
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
    writeIORef events []
    split<-driverDeliver running (prompt "split") >>= right "split output"
    splitEvents<-readIORef events
    let joined=T.concat [text | ("output",value)<-splitEvents,Just text<-[field "text" value]]
    check "streamed session reference stays private across chunk boundaries" (not ("private-child-key" `T.isInfixOf` joined) && field "text" split==Just ("Output [private] suffix"::T.Text))
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
      check "human sees bounded tool details without private reference" (maybe False (\value->"/proposed.txt" `T.isInfixOf` permissionDetails value && not ("private-child-key" `T.isInfixOf` permissionDetails value) && T.length (permissionDetails value)<=8192) shown)
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
  , "log=open(os.environ['LOG'],'a',buffering=1); sid='private-child-key'; active=None; selected={'model-id':'model-a','effort-id':'low'}"
  , "def send(v): print(json.dumps(dict(jsonrpc='2.0',**v)),flush=True)"
  , "def reply(i,v): send({'id':i,'result':v})"
  , "def options(): return [{'id':'model-id','name':'Model','category':'model','type':'select','currentValue':selected['model-id'],'options':[{'value':'model-a','name':'A'},{'value':'model-b','name':'B'}]},{'id':'effort-id','name':'Effort','category':'thought_level','type':'select','currentValue':selected['effort-id'],'options':[{'value':'low','name':'Low'},{'value':'high','name':'High'}]}]"
  , "def update(v): send({'method':'session/update','params':{'sessionId':sid,'update':v}})"
  , "for line in sys.stdin:"
  , " m=json.loads(line); log.write(json.dumps(m)+'\\n'); method=m.get('method'); p=m.get('params',{}); i=m.get('id')"
  , " if method=='initialize': reply(i,{'protocolVersion':1,'agentCapabilities':{'sessionCapabilities':{'fork':{}} if os.environ.get('FORK')=='yes' else {}}})"
  , " elif method=='session/new': reply(i,{'sessionId':sid,'configOptions':options()})"
  , " elif method=='session/fork': sid=p['sessionId'] if os.environ.get('REUSE')=='yes' else 'private-fork-key'; reply(i,{'sessionId':sid,'configOptions':options()})"
  , " elif method=='session/set_config_option': selected[p['configId']]=p['value']; reply(i,{'configOptions':options()})"
  , " elif method=='session/prompt':"
  , "  active=i; text=p['prompt'][-1]['text']"
  , "  if 'disconnect' in text: sys.exit(0)"
  , "  elif 'stall' in text: pass"
  , "  elif 'permission' in text: send({'id':'permission','method':'session/request_permission','params':{'sessionId':sid,'toolCall':{'title':'Write file','rawInput':{'path':'/proposed.txt','sessionId':sid}},'options':[{'optionId':'allow-once','name':'Allow once','kind':'allow_once'},{'optionId':'reject','name':'Reject','kind':'reject_once'}]}})"
  , "  elif 'split' in text:"
  , "   for part in ['Output private-','child-','key suffix']: update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':part}}); reply(999,{})"
  , "   reply(i,{'stopReason':'end_turn'})"
  , "  elif 'large' in text:"
  , "   for n in range(24): update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'x'*9000}})"
  , "   reply(i,{'stopReason':'end_turn'})"
  , "  else:"
  , "   update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'Output '+sid}}); update({'sessionUpdate':'tool_call','title':'Inspect source','status':'completed','rawInput':{'sessionId':sid}}); reply(i,{'stopReason':'end_turn'})"
  , " elif method=='session/cancel' and os.environ.get('IGNORE_CANCEL')!='yes': reply(active,{'stopReason':'cancelled'})"
  , " elif i=='permission':"
  , "  if m.get('result',{}).get('outcome',{}).get('outcome')=='selected': send({'id':'native-read','method':'fs/read_text_file','params':{'sessionId':sid,'path':'/secret'}})"
  , "  else: reply(active,{'stopReason':'cancelled'})"
  , " elif i=='native-read': send({'id':'native-terminal','method':'terminal/create','params':{'sessionId':sid,'command':'bad'}})"
  , " elif i=='native-terminal': reply(active,{'stopReason':'end_turn'})"
  ]
