{-# LANGUAGE OverloadedStrings #-}
module AgentRuntimeCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (callProcess)
import System.Timeout (timeout)
import qualified THC.Edit.ACP as ACP
import THC.Edit.AgentAccess
import THC.Edit.AgentHub
import THC.Edit.AgentRuntime
import THC.Edit.Session

checks :: IO ()
checks = bracket temporary removePathForcibly $ \root -> do
  let config = root </> "config"
      project = root </> "project"
      policy = config </> "thc" </> "config.toml"
      session = replicate 48 'a'
      script = root </> "provider.py"
      logPath = root </> "provider.jsonl"
      launch = pure (ACP.Launch "python3" [script] [("PROBE_LOG",logPath)])
  opened <- newIORef []
  let startEditor directory = do
        modifyIORef' opened (++[directory])
        record <- newSessionRecord Nothing [directory]
        pure record {sessionDirectory=directory}
  createDirectoryIfMissing True (config </> "thc")
  createDirectory project
  writeFile script fixture
  writeFile logPath ""
  writeFile policy "[broken\n"
  withEnvironment [("XDG_CONFIG_HOME",config),("XDG_DATA_HOME",root </> "data"),("THC_EDIT_SESSION",session)] $
    withAgentRuntimeUsing startEditor (const (error "Unexpected editor reconnect")) project launch $ \runtime -> do
      let hub = agentHub runtime
          primary = primaryAgent runtime
          spec = SpawnSpec "child" "Inspect code" project Shared Fresh Nothing Nothing
      token <- primaryServers runtime >>= serverToken
      assert "primary token resolves to primary identity" . (==Just primary) =<< resolveAgentAccess (agentAccess runtime) token
      rejected <- spawnAgent hub Human spec
      assert "malformed policy blocks spawning but not editor startup" (either (const True) (const False) rejected)
      writeFile policy "[editor.agents]\nmax_agents = 4\nmax_subagents = 2\n[editor.agent]\ncontext = 'global context marker'\n"
      _ <- syncPrimary runtime project "private-primary" (Capabilities True True False []) False >>= right
      ticket <- sendAgent hub Human primary "inspect" >>= right
      requests <- waitRequests runtime
      reply <- case [cell | DeliverPrimary _ cell <- requests] of
        [cell] -> pure cell
        _ -> error "Expected primary delivery"
      _ <- cancelAgent hub Human primary >>= right
      assert "active primary cancellation waits for real provider completion" =<< isEmptyMVar reply
      cancelled <- drainAgentRequests runtime
      assert "primary cancel reaches UI" (any isCancel cancelled)
      putMVar reply (Left "Cancelled")
      result <- waitAgent hub Human primary ticket 2000 >>= right
      assert "primary cancellation completes" (field "status" result==Just ("cancelled"::T.Text))
      await "primary cancellation barrier releases" $ do
        status <- statusAgent hub Human primary >>= right
        pure (field "status" status==Just ("idle"::T.Text))
      next <- sendAgent hub Human primary "another" >>= right
      _ <- waitRequests runtime
      failPendingPrimary runtime "Provider disconnected."
      ended <- waitAgent hub Human primary next 2000 >>= right
      assert "disconnect resolves outstanding primary delivery" (field "status" ended==Just ("failed"::T.Text))
      child <- spawnAgent hub (Agent primary) spec >>= right
      record <- agentSession runtime child >>= maybe (error "Missing shared editor") pure
      assert "shared child uses existing editor session" (sessionId record==session)
      assert "shared child starts no hidden editor" . null =<< readIORef opened
      ordinary <- sendAgent hub Human child "ordinary" >>= right
      done <- waitAgent hub Human child ordinary 2000 >>= right
      assert "child provider prompt completes through hub" (field "status" done==Just ("completed"::T.Text))
      entries <- mapM (either error pure . eitherDecodeStrict') . BS.lines =<< BS.readFile logPath
      let newSessions = [p | entry <- entries, field "method" entry==Just ("session/new"::T.Text), Just p <- [field "params" entry :: Maybe Value]]
          servers = concat [xs | value <- newSessions, Just xs <- [field "mcpServers" value :: Maybe [Value]]]
      assert "child separates editor and orchestration servers" (map (field "name") servers==[Just ("editor"::T.Text),Just "agents"])
      childToken <- serverToken servers
      assert "child bridge token belongs to child" . (==Just child) =<< resolveAgentAccess (agentAccess runtime) childToken
      assert "global context reaches child prompt" ("global context marker" `T.isInfixOf` T.pack (show entries))
      permissionTicket <- sendAgent hub Human child "permission" >>= right
      asked <- waitRequests runtime
      permissionReply <- case [cell | ProviderPermission who _ cell <- asked, who==child] of
        [cell] -> pure cell
        _ -> error "Expected human permission request"
      _ <- cancelAgent hub Human child >>= right
      assert "child cancellation releases human permission reply" . (==Just Nothing) =<< tryReadMVar permissionReply
      _ <- waitAgent hub Human child permissionTicket 2000 >>= right
      _ <- endAgent hub Human child >>= right
      assert "ending child revokes its bridge" . (==Nothing) =<< resolveAgentAccess (agentAccess runtime) childToken
      assert "ending child retains editor mapping" . maybe False ((==session).sessionId) =<< agentSession runtime child
      let otherProject = root </> "other-project"
      createDirectory otherProject
      _ <- syncPrimary runtime otherProject "private-primary" (Capabilities True True False []) False >>= right
      assert "primary editor mapping follows changed project" . maybe False ((==otherProject).sessionDirectory) =<< agentSession runtime primary
      moved <- spawnAgent hub (Agent primary) spec {spawnName="moved",spawnDirectory=otherProject} >>= right
      assert "shared child follows trusted current project" . maybe False (\r -> sessionDirectory r==otherProject && sessionId r==session) =<< agentSession runtime moved
      _ <- endAgent hub Human moved >>= right
      _ <- syncPrimary runtime project "private-primary" (Capabilities True True False []) False >>= right
      callProcess "git" ["-C",project,"init","-q","-b","main"]
      writeFile (project </> "source.txt") "committed\n"
      callProcess "git" ["-C",project,"add","source.txt"]
      callProcess "git" ["-C",project,"-c","user.name=Runtime Test","-c","user.email=test@example.invalid","-c","commit.gpgsign=false","commit","-qm","fixture"]
      writeFile (project </> "source.txt") "dirty\n"
      isolated <- spawnAgent hub Human spec {spawnName="worktree",spawnWorkspace=Worktree Nothing Nothing (Just "runtime-test")} >>= right
      workspace <- agentSession runtime isolated >>= maybe (error "Missing worktree editor") pure
      assert "worktree owns independent editor and directory" (sessionId workspace/=session && sessionDirectory workspace/=project)
      assert "worktree starts at committed source" . (=="committed\n") =<< readFile (sessionDirectory workspace </> "source.txt")
      assert "one hidden editor starts for worktree" . (==[sessionDirectory workspace]) =<< readIORef opened
      nested <- spawnAgent hub (Agent isolated) spec {spawnName="nested",spawnDirectory=sessionDirectory workspace} >>= right
      nestedSession <- agentSession runtime nested >>= maybe (error "Missing nested shared editor") pure
      assert "nested shared child uses its parent's editor" (sessionId nestedSession==sessionId workspace)
      _ <- endAgent hub Human isolated >>= right
      assert "ending worktree agent preserves checkout" =<< doesFileExist (sessionDirectory workspace </> "source.txt")
  persistenceChecks root
  where
    isCancel CancelPrimary = True
    isCancel _ = False

persistenceChecks :: FilePath -> IO ()
persistenceChecks root = do
  let project = root </> "project"
      config = root </> "config"
      logPath = root </> "recovery-provider.jsonl"
      sid = replicate 48 'b'
      launch = pure (ACP.Launch "python3" [root </> "provider.py"] [("PROBE_LOG",logPath)])
      startEditor directory = do
        record <- newSessionRecord Nothing [directory]
        pure record {sessionDirectory=directory}
      spec = SpawnSpec "recover-child" "Inspect code" project (Worktree Nothing Nothing (Just "recovery")) Fresh Nothing Nothing
  writeFile logPath ""
  withEnvironment [("XDG_CONFIG_HOME",config),("XDG_DATA_HOME",root </> "recovery-data"),("THC_EDIT_SESSION",sid)] $ do
    fresh <- newSessionRecord Nothing ["--",project]
    let record = fresh {sessionId=sid,sessionDirectory=project}
    rememberSession record
    path <- (++".agents.json") <$> checkpointPath sid
    (primary,child,workspace,oldToken,oldChildToken,oldAccess) <- withAgentRuntimeUsing startEditor (const (error "Unexpected editor reconnect")) project launch $ \runtime -> do
      activateAgentCheckpoint runtime
      let hub = agentHub runtime
      token <- primaryServers runtime >>= serverToken
      _ <- syncPrimary runtime project "primary-private" (Capabilities False True False []) False >>= right
      child <- spawnAgent hub Human spec >>= right
      workspace <- agentSession runtime child >>= maybe (error "Missing recoverable workspace") pure
      ticket <- sendAgent hub Human child "permission" >>= right
      _ <- waitRequests runtime
      _ <- sendAgent hub Human child "do-not-replay" >>= right
      pending <- waitAgent hub Human child ticket 0 >>= right
      assert "checkpoint captures busy child" (field "status" pending==Just ("running"::T.Text))
      _ <- checkpointAgents runtime >>= right
      bytes <- BS.readFile path
      assert "checkpoint omits primary bearer" (not (T.unpack token `isIn` BS.unpack bytes))
      logged <- mapM (either error pure . eitherDecodeStrict') . BS.lines =<< BS.readFile logPath
      let servers = concat [xs | entry <- logged, field "method" entry==Just ("session/new"::T.Text),
            Just params <- [field "params" entry :: Maybe Value], Just xs <- [field "mcpServers" params :: Maybe [Value]]]
      childToken <- serverToken servers
      assert "checkpoint omits child bearer" (not (T.unpack childToken `isIn` BS.unpack bytes))
      withAgentRuntimeUsing (const (error "Competing startup must not launch editors")) (const (error "Competing startup must not resume editors")) project launch $ \competing -> do
        recordAgentEvent (agentHub competing) (primaryAgent competing) "losing-startup" Null
        _ <- checkpointAgents competing >>= right
        pure ()
      assert "unactivated competing runtime cannot overwrite checkpoint on release" . (==bytes) =<< BS.readFile path
      pure (primaryAgent runtime,child,workspace,token,childToken,agentAccess runtime)
    assert "closing runtime invalidates old bearer" . (==Nothing) =<< resolveAgentAccess oldAccess oldToken
    before <- BS.readFile logPath
    writeFile (config </> "thc" </> "config.toml") "[broken\n"
    resumedEditors <- newIORef []
    editorAvailable <- newIORef False
    let resumeEditor savedEditor = do
          modifyIORef' resumedEditors (++[savedEditor])
          available <- readIORef editorAvailable
          unless available (ioError (userError "Saved editor is unavailable"))
    withAgentRuntimeUsing (const (error "Recovery must not start a new editor")) resumeEditor project launch $ \runtime -> do
      activateAgentCheckpoint runtime
      assert "restored primary identity is stable" (primaryAgent runtime==primary)
      token <- primaryServers runtime >>= serverToken
      assert "recovery issues a fresh bearer" (token/=oldToken)
      assert "old bearer is invalid in recovered runtime" . (==Nothing) =<< resolveAgentAccess (agentAccess runtime) oldToken
      restored <- statusAgent (agentHub runtime) Human child >>= right
      assert "child restores inert with no queued work" (field "status" restored==Just ("recovered"::T.Text) && field "queued" restored==Just (0::Int))
      assert "workspace mapping survives recovery" . (==Just workspace) =<< agentSession runtime child
      history <- historyAgent (agentHub runtime) Human child 0 100 >>= right
      assert "history survives recovery" ("do-not-replay" `T.isInfixOf` T.pack (show history))
      assert "recovery starts no provider or queued prompt" . (==before) =<< BS.readFile logPath
      let hub = agentHub runtime
          rejected label action = action >>= assert label . either (const True) (const False)
          logged = mapM (either error pure . eitherDecodeStrict') . BS.lines =<< BS.readFile logPath
      rejected "malformed current policy blocks explicit reconnect" (reconnectAgent hub child)
      assert "policy rejection starts no provider" . (==before) =<< BS.readFile logPath
      writeFile (config </> "thc" </> "config.toml") "[editor.agents]\nmax_agents = 1\nmax_subagents = 2\n"
      rejected "reconnect counts the existing primary toward current capacity" (reconnectAgent hub child)
      writeFile (config </> "thc" </> "config.toml") "[editor.agents]\nmax_agents = 4\nmax_subagents = 2\n"
      rejected "editor restore failure prevents provider reconnect" (reconnectAgent hub child)
      assert "failed editor restore starts no provider" . (==before) =<< BS.readFile logPath
      assert "editor restore keeps exact saved session and workspace" . (==[workspace]) =<< readIORef resumedEditors
      writeIORef editorAvailable True
      writeFile (logPath++".load") "unsupported"
      rejected "reconnect rechecks current provider capabilities" (reconnectAgent hub child)
      unsupported <- statusAgent hub Human child >>= right
      assert "unsupported provider leaves child recoverable" (field "status" unsupported==Just ("recovered"::T.Text))
      writeFile (logPath++".load") "reject"
      rejected "load failure remains retryable" (reconnectAgent hub child)
      failedLog <- logged
      failedToken <- serverToken (concat [servers | entry <- failedLog,field "method" entry==Just ("session/load"::T.Text),
        Just params <- [field "params" entry :: Maybe Value],Just servers <- [field "mcpServers" params :: Maybe [Value]]])
      assert "failed load revokes its fresh bearer" . (==Nothing) =<< resolveAgentAccess (agentAccess runtime) failedToken
      writeFile (logPath++".load") "ok"
      beforeLoad <- length <$> logged
      _ <- requestAgentReconnect runtime child >>= right
      reconnected <- waitRequests runtime
      assert "host reconnect completes asynchronously" (any (\request -> case request of AgentReconnected who (Right ()) -> who==child; _ -> False) reconnected)
      restoredEditors <- readIORef resumedEditors
      assert "each explicit retry restores only the saved editor identity" (length restoredEditors==4 && all (==workspace) restoredEditors)
      current <- statusAgent hub Human child >>= right
      assert "reconnected child keeps name and identity and starts idle"
        (field "id" current==Just (agentIdText child) && field "name" current==Just ("recover-child"::T.Text) && field "status" current==Just ("idle"::T.Text))
      assert "reconnect keeps owned editor and worktree" . (==Just workspace) =<< agentSession runtime child
      loaded <- drop beforeLoad <$> logged
      assert "reconnect sends only initialize/load; queued work is not replayed"
        (map (field "method") loaded==[Just ("initialize"::T.Text),Just "session/load"])
      freshToken <- serverToken (concat [servers | entry <- loaded,Just params <- [field "params" entry :: Maybe Value],Just servers <- [field "mcpServers" params :: Maybe [Value]]])
      assert "reconnected child gets a fresh scoped bearer"
        (freshToken/=oldChildToken && freshToken/=failedToken && freshToken/=token)
      assert "fresh reconnect bearer maps to original child" . (==Just child) =<< resolveAgentAccess (agentAccess runtime) freshToken
      assert "old child bearer remains invalid" . (==Nothing) =<< resolveAgentAccess (agentAccess runtime) oldChildToken
      afterHistory <- historyAgent hub Human child 0 100 >>= right
      assert "reconnect retains history without provider replay"
        ("do-not-replay" `T.isInfixOf` T.pack (show afterHistory) && not ("replayed-provider-history" `T.isInfixOf` T.pack (show afterHistory)))
      rejected "already active child cannot reconnect again" (reconnectAgent hub child)
      _ <- endAgent hub Human child >>= right
      assert "ending reconnected child revokes fresh bearer" . (==Nothing) =<< resolveAgentAccess (agentAccess runtime) freshToken
      rejected "ended child cannot be reconnected" (reconnectAgent hub child)
      forgetSession sid
      _ <- checkpointAgents runtime >>= right
      assert "explicit Exit removes agent sidecar" . not =<< doesFileExist path
    assert "runtime teardown does not recreate sidecar after Exit" . not =<< doesFileExist path
    rememberSession record
    writeFile path "broken-checkpoint"
    withAgentRuntimeUsing startEditor (const (error "Unexpected editor reconnect")) project launch $ \runtime -> do
      activateAgentCheckpoint runtime
      notice <- runtimeNotice runtime
      assert "invalid checkpoint is reported without preventing editor startup" (maybe False (T.isInfixOf "checkpoint") notice)
      assert "recovery notice is consumed once" . (==Nothing) =<< runtimeNotice runtime
      rejected <- spawnAgent (agentHub runtime) Human spec
      assert "invalid checkpoint fails closed for spawning" (either (const True) (const False) rejected)
      _ <- checkpointAgents runtime >>= right
      assert "invalid checkpoint is retained unchanged" . (=="broken-checkpoint") =<< readFile path
    forgetSession sid
    assert "forgetSession removes corrupt agent sidecar too" . not =<< doesFileExist path
  where isIn needle haystack = T.pack needle `T.isInfixOf` T.pack haystack

waitRequests :: AgentRuntime -> IO [AgentRequest]
waitRequests runtime = timeout 3000000 loop >>= maybe (error "Timed out waiting for runtime requests") pure
  where loop = drainAgentRequests runtime >>= \requests -> if null requests then threadDelay 1000 >> loop else pure requests

await :: String -> IO Bool -> IO ()
await label condition = timeout 3000000 loop >>= assert label . (==Just ())
  where loop = condition >>= \ready -> unless ready (threadDelay 1000 >> loop)

serverToken :: [Value] -> IO T.Text
serverToken servers = case [token | server <- servers, entry <- maybe [] id (field "env" server :: Maybe [Value]),
  field "name" entry==Just ("THC_EDIT_MCP_TOKEN"::T.Text), Just token <- [field "value" entry]] of
    [token] -> pure token
    _ -> error "Missing actor-bound editor server"

field :: FromJSON a => Key -> Value -> Maybe a
field name = parseMaybe (withObject "field" (.: name))
right :: Either T.Text a -> IO a
right = either (error . T.unpack) pure
assert :: String -> Bool -> IO ()
assert label = flip unless (error label)
temporary :: IO FilePath
temporary = do
  base <- getTemporaryDirectory
  (path,handle) <- openTempFile base "agent-runtime-check"
  hClose handle
  removeFile path
  createDirectory path
  canonicalizePath path
withEnvironment :: [(String,String)] -> IO a -> IO a
withEnvironment values action = bracket (mapM (lookupEnv . fst) values) restore $ \_ -> mapM_ (uncurry setEnv) values >> action
  where restore old = mapM_ (\((key,_),value) -> maybe (unsetEnv key) (setEnv key) value) (zip values old)

fixture :: String
fixture = unlines
  [ "import json,os,sys"
  , "active=None"
  , "sid='private-provider-'+str(os.getpid())"
  , "def send(v): print(json.dumps(v),flush=True)"
  , "def reply(i,r): send({'jsonrpc':'2.0','id':i,'result':r})"
  , "for line in sys.stdin:"
  , " q=json.loads(line)"
  , " with open(os.environ['PROBE_LOG'],'a') as f: f.write(json.dumps(q)+'\\n')"
  , " m=q.get('method'); i=q.get('id'); p=q.get('params',{})"
  , " if m=='initialize': reply(i,{'protocolVersion':1,'agentCapabilities':{'loadSession':not (os.path.exists(os.environ['PROBE_LOG']+'.load') and open(os.environ['PROBE_LOG']+'.load').read()=='unsupported')}})"
  , " elif m=='session/new': reply(i,{'sessionId':sid})"
  , " elif m=='session/load':"
  , "  sid=p['sessionId']"
  , "  if os.path.exists(os.environ['PROBE_LOG']+'.load') and open(os.environ['PROBE_LOG']+'.load').read()=='reject': send({'jsonrpc':'2.0','id':i,'error':{'code':-32000,'message':'Load rejected'}})"
  , "  else: send({'jsonrpc':'2.0','method':'session/update','params':{'sessionId':sid,'update':{'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'replayed-provider-history'}}}}); reply(i,{})"
  , " elif m=='session/prompt':"
  , "  active=i"
  , "  if 'permission' in json.dumps(p):"
  , "   send({'jsonrpc':'2.0','id':'approval','method':'session/request_permission','params':{'sessionId':sid,'toolCall':{'title':'Proposed tool'},'options':[{'optionId':'once','name':'Allow once','kind':'allow_once'}]}})"
  , "  else: reply(i,{'stopReason':'end_turn'}); active=None"
  , " elif m=='session/cancel' and active is not None: reply(active,{'stopReason':'cancelled'}); active=None"
  , " elif i=='approval' and active is not None: reply(active,{'stopReason':'cancelled'}); active=None"
  ]
