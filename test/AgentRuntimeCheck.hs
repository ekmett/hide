{-# LANGUAGE OverloadedStrings #-}
module AgentRuntimeCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync,wait)
import Control.Concurrent.MVar
import Control.Exception (bracket,finally)
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
import qualified Hide.ACP as ACP
import Hide.AgentAccess
import Hide.AgentHub
import qualified Hide.AgentUI
import Hide.AgentServicesHost (agentServices)
import qualified Hide.Plugin.Session as Plugin
import qualified Hide.Plugin.Tool as Tool
import Hide.AgentRuntime
import Hide.Session

checks :: IO ()
checks = Tool.withTools [] [tool | Plugin.CoordinationTool tool<-Plugin.pluginTools Hide.AgentUI.plugin] $ \tools -> bracket temporary removePathForcibly $ \root -> do
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
      primaryDeliveryChecks runtime project (ACP.Launch "python3" [script] [("PROBE_LOG",logPath)])
      let choices=Capabilities False False True [ConfigChoice "model" "model" "small" [("small","Small"),("large","Large")]]
      bracket (ACP.startClient (ACP.Launch "python3" [script] [("PROBE_LOG",logPath)]) project) ACP.stopClient $ \client->do
        _<-syncPrimary runtime project (Just client) "private-primary" choices False >>= right
        (captured,_)<-agentConfiguration hub primary >>= right
        withAsync (configureAgentAt hub captured "model" "large") $ \setting->do
          pending<-waitRequests runtime
          control<-case [request | ControlPrimary request<-pending] of
            [request]->pure request
            _->error "Expected primary configuration request"
          assert "primary control admits its exact connection" =<< primaryControlCurrent runtime control (Just client) (Just "private-primary")
          let refreshed=choices {configChoices=[ConfigChoice "model" "model" "large" [("small","Small"),("large","Large")]]}
          _<-syncPrimary runtime project (Just client) "private-primary" refreshed False >>= right
          assert "capability refresh preserves an admitted primary control" =<< primaryControlCurrent runtime control (Just client) (Just "private-primary")
          recordPrimaryEvent runtime client "private-primary" (ProviderUsage 12 100)
          bracket (ACP.startClient (ACP.Launch "python3" [script] [("PROBE_LOG",logPath)]) project) ACP.stopClient $ \replacement->do
            current<-primaryControlCurrent runtime control (Just replacement) (Just "private-primary")
            assert "same-key replacement cannot consume old primary control" (not current)
            _<-syncPrimary runtime project (Just replacement) "private-primary" choices False >>= right
            stale<-agentConfigurationCurrent hub captured
            assert "same-key replacement retires the hub control receipt" (not stale)
            recordPrimaryEvent runtime replacement "private-primary" (ProviderUsage 24 100)
            recordPrimaryEvent runtime client "private-primary" (ProviderUsage 99 100)
            currentStatus<-statusAgent hub Human primary >>= right
            assert "same-key old primary cannot publish into the replacement"
              ((field "contextUsage" currentStatus >>= field "used")==Just (24::Int))
            waiting<-case control of ConfigurePrimary _ _ _ reply->isEmptyMVar reply; _->pure True
            assert "connection replacement terminally resolves retained control" (not waiting)
          rejectPrimaryControl "Primary provider changed." control
          assert "retired primary configuration resolves without success" . either (const True) (const False) =<< wait setting
        _<-syncPrimary runtime project (Just client) "private-primary" choices False >>= right
        withAsync (configureAgent hub primary "model" "large") $ \setting->do
          pending<-waitRequests runtime
          control<-case [request | ControlPrimary request<-pending] of
            [request]->pure request
            _->error "Expected cancellable primary configuration"
          _<-cancelAgent hub Human primary >>= right
          assert "primary cancel resolves an admitted configuration" . either (const True) (const False) =<< wait setting
          current<-primaryControlCurrent runtime control (Just client) (Just "private-primary")
          assert "drained control remains retired after cancellation" (not current)
          lateReply<-newEmptyMVar
          let late=case control of
                ConfigurePrimary owner key options _->ConfigurePrimary owner key options lateReply
                _->error "Expected configuration control"
          admittedLate<-primaryControlCurrent runtime late (Just client) (Just "private-primary")
          assert "control published after cancellation cannot acquire a new admission" (not admittedLate)
        _<-drainAgentRequests runtime
        pure ()
      _<-syncPrimary runtime project Nothing "private-primary" (Capabilities True True False []) False >>= right
      noGit <- Tool.callTool tools (agentServices hub (Agent primary) project) "agent_spawn" (object ["name" .= ("No Git default"::T.Text),"task" .= ("Independent work"::T.Text)])
      assert "omitted workspace refuses non-Git without shared fallback" (either (const True) (const False) noGit)
      assert "failed isolated default starts no editor" . null =<< readIORef opened
      childResult <- Tool.callTool tools (agentServices hub (Agent primary) project) "agent_spawn" (object ["name" .= ("child"::T.Text),"task" .= ("Inspect code"::T.Text),"workspace" .= object ["mode" .= ("shared"::T.Text)]]) >>= right
      child <- spawnedId childResult
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
      _ <- syncPrimary runtime otherProject Nothing "private-primary" (Capabilities True True False []) False >>= right
      assert "primary editor mapping follows changed project" . maybe False ((==otherProject).sessionDirectory) =<< agentSession runtime primary
      moved <- spawnAgent hub (Agent primary) spec {spawnName="moved",spawnDirectory=otherProject} >>= right
      assert "shared child follows trusted current project" . maybe False (\r -> sessionDirectory r==otherProject && sessionId r==session) =<< agentSession runtime moved
      _ <- endAgent hub Human moved >>= right
      _ <- syncPrimary runtime project Nothing "private-primary" (Capabilities True True False []) False >>= right
      callProcess "git" ["-C",project,"init","-q","-b","main"]
      writeFile (project </> "source.txt") "committed\n"
      callProcess "git" ["-C",project,"add","source.txt"]
      callProcess "git" ["-C",project,"-c","user.name=Runtime Test","-c","user.email=test@example.invalid","-c","commit.gpgsign=false","commit","-qm","fixture"]
      writeFile (project </> "source.txt") "dirty\n"
      isolatedResult <- Tool.callTool tools (agentServices hub Human project) "agent_spawn" (object ["name" .= ("worktree"::T.Text),"task" .= ("Inspect committed source"::T.Text)]) >>= right
      isolated <- spawnedId isolatedResult
      workspace <- agentSession runtime isolated >>= maybe (error "Missing worktree editor") pure
      assert "worktree owns independent editor and directory" (sessionId workspace/=session && sessionDirectory workspace/=project)
      assert "worktree starts at committed source" . (=="committed\n") =<< readFile (sessionDirectory workspace </> "source.txt")
      assert "one hidden editor starts for worktree" . (==[sessionDirectory workspace]) =<< readIORef opened
      nested <- spawnAgent hub (Agent isolated) spec {spawnName="nested",spawnDirectory=sessionDirectory workspace} >>= right
      nestedSession <- agentSession runtime nested >>= maybe (error "Missing nested shared editor") pure
      assert "nested shared child uses its parent's editor" (sessionId nestedSession==sessionId workspace)
      _ <- endAgent hub Human isolated >>= right
      assert "ending worktree agent preserves checkout" =<< doesFileExist (sessionDirectory workspace </> "source.txt")
  primaryShutdownCheck root
  creationChecks root
  persistenceChecks root
  where
    spawnedId value=maybe (fail "Missing spawned agent ID") (pure . AgentId) (field "agent" value >>= field "id")

-- The public receipt owns terminal completion; removing a mailbox entry alone
-- grants no provider admission and cannot keep the cancellation barrier alive.
primaryDeliveryChecks :: AgentRuntime -> FilePath -> ACP.Launch -> IO ()
primaryDeliveryChecks runtime project launch=bracket (ACP.startClient launch project) ACP.stopClient $ \client->do
  let hub=agentHub runtime
      primary=primaryAgent runtime
      key="private-primary"
      caps=Capabilities True True False []
      complete owner busy result=completePrimaryDelivery runtime (Just owner) (Just key) busy result
      idle=await "primary reservation releases" $ do
        value<-statusAgent hub Human primary >>= right
        pure (field "status" value==Just ("idle"::T.Text))
      request body=do
        ticket<-sendAgent hub Human primary body >>= right
        entries<-waitRequests runtime
        delivery<-case [receipt | DeliverPrimary receipt<-entries] of
          [receipt]->pure receipt
          _->error "Expected opaque primary delivery"
        pure (ticket,delivery)
      isCancel CancelPrimary=True
      isCancel _=False
  _<-syncPrimary runtime project (Just client) key caps False >>= right
  (ticket,delivery)<-request "inspect"
  admitted<-admitPrimaryDelivery runtime delivery client key
  assert "admission preserves Hub ticket, author and user-seat attribution" $ case admitted of
    Just message->messageTicket message==ticket && messageAuthor message==Human && messageIsUserSeat message && messageText message=="inspect"
    _->False
  assert "runtime owns admitted delivery" =<< primaryDeliveryActive runtime
  withAgentRuntimeUsing (const (error "Unexpected editor startup")) (const (error "Unexpected editor reconnect")) project (pure launch) $ \other->do
    _<-syncPrimary other project (Just client) key caps False >>= right
    foreignAdmission<-admitPrimaryDelivery other delivery client key
    assert "foreign runtime cannot admit an owned receipt" (maybe True (const False) foreignAdmission)
    rejectPrimaryDelivery other delivery "Foreign rejection."
  foreignResult<-waitAgent hub Human primary ticket 0 >>= right
  assert "foreign admission/rejection cannot mutate owner reply" (field "status" foreignResult==Just ("running"::T.Text))
  _<-syncPrimary runtime project (Just client) key caps {supportsSteering=True} False >>= right
  duplicate<-admitPrimaryDelivery runtime delivery client key
  assert "capability refresh preserves exactly one admission" (maybe True (const False) duplicate)
  rejectPrimaryDelivery runtime delivery "Late duplicate rejection."
  pending<-waitAgent hub Human primary ticket 0 >>= right
  assert "late rejection cannot release admitted reply" (field "status" pending==Just ("running"::T.Text))
  _<-cancelAgent hub Human primary >>= right
  assert "admitted cancellation waits for provider terminal response" =<< primaryDeliveryActive runtime
  cancelling<-statusAgent hub Human primary >>= right
  assert "Hub retains reservation during admitted cancellation" (field "status" cancelling==Just ("cancelling"::T.Text))
  cancelled<-drainAgentRequests runtime
  assert "primary cancel reaches existing UI mailbox" (any isCancel cancelled)
  assert "real provider outcome settles cancelled receipt" =<< complete client False (Left "Cancelled")
  idle
  result<-waitAgent hub Human primary ticket 0 >>= right
  assert "primary cancellation completes" (field "status" result==Just ("cancelled"::T.Text))
  (unadmitted,drained)<-request "drained only"
  _<-cancelAgent hub Human primary >>= right
  idle
  stale<-admitPrimaryDelivery runtime drained client key
  assert "drain then cancel forbids late admission" (maybe True (const False) stale)
  notActive<-primaryDeliveryActive runtime
  assert "unadmitted cancellation retains no prompt owner" (not notActive)
  _<-waitAgent hub Human primary unadmitted 0 >>= right
  _<-drainAgentRequests runtime
  (first,owned)<-request "redacted result"
  _<-admitPrimaryDelivery runtime owned client key >>= maybe (error "Expected result admission") pure
  let redacted=Right (object ["text" .= ("[private session]"::T.Text)])
  assert "current owner completes first result" =<< complete client True redacted
  finished<-waitAgent hub Human primary first 2000 >>= right
  assert "runtime publishes the owner's already-redacted terminal value" (field "result" finished==Just (object ["text" .= ("[private session]"::T.Text)]))
  queued<-sendAgent hub Human primary "next turn" >>= right
  held<-statusAgent hub Human primary >>= right
  assert "external human busy state precedes reply release" (field "queued" held==Just (1::Int) && (field "currentTicket" held::Maybe (Maybe Int))==Just Nothing)
  _<-syncPrimary runtime project (Just client) key caps False >>= right
  queuedEntries<-waitRequests runtime
  nextDelivery<-case [receipt | DeliverPrimary receipt<-queuedEntries] of
    [receipt]->pure receipt
    _->error "Expected next primary turn"
  _<-admitPrimaryDelivery runtime nextDelivery client key >>= maybe (error "Expected next-turn admission") pure
  failPendingPrimary runtime "Provider disconnected."
  disconnected<-waitAgent hub Human primary queued 2000 >>= right
  assert "disconnect resolves runtime-owned primary delivery" (field "status" disconnected==Just ("failed"::T.Text))
  rejected<-complete client False (Right Null)
  assert "terminal delivery cannot complete a second time" (not rejected)
  idle
  (oldTicket,oldDelivery)<-request "old provider"
  _<-admitPrimaryDelivery runtime oldDelivery client key >>= maybe (error "Expected old-provider admission") pure
  bracket (ACP.startClient launch project) ACP.stopClient $ \replacement->do
    _<-syncPrimary runtime project (Just replacement) key caps False >>= right
    oldResult<-waitAgent hub Human primary oldTicket 2000 >>= right
    assert "same-key provider replacement resolves old delivery" (field "status" oldResult==Just ("failed"::T.Text))
    late<-admitPrimaryDelivery runtime oldDelivery replacement key
    assert "old driver receipt cannot adopt replacement client" (maybe True (const False) late)
    idle
    (newTicket,newDelivery)<-request "new provider"
    _<-admitPrimaryDelivery runtime newDelivery replacement key >>= maybe (error "Expected replacement admission") pure
    oldCompletion<-complete client True (Right Null)
    assert "old provider cannot complete or advertise busy for replacement" (not oldCompletion)
    live<-waitAgent hub Human primary newTicket 0 >>= right
    assert "replacement receipt stays pending after old completion" (field "status" live==Just ("running"::T.Text))
    assert "replacement owns completion" =<< complete replacement False (Right Null)
    _<-waitAgent hub Human primary newTicket 2000 >>= right
    idle
    (_,beforeCancel)<-request "cancel before replacement"
    _<-cancelAgent hub Human primary >>= right
    _<-syncPrimary runtime project (Just client) key caps False >>= right
    retired<-drainAgentRequests runtime
    assert "provider replacement clears old queued UI cancellation" (not (any isCancel retired))
    before<-admitPrimaryDelivery runtime beforeCancel client key
    assert "cancelled issuing-provider receipt cannot be re-admitted" (maybe True (const False) before)
    idle

primaryShutdownCheck :: FilePath -> IO ()
primaryShutdownCheck root=do
  let project=root </> "project"
      launch=ACP.Launch "python3" [root </> "provider.py"] [("PROBE_LOG",root </> "provider.jsonl")]
      key="private-shutdown"
  withEnvironment [("XDG_CONFIG_HOME",root </> "config"),("XDG_DATA_HOME",root </> "data"),("THC_EDIT_SESSION",replicate 48 'b')] $
    bracket (ACP.startClient launch project) ACP.stopClient $ \client->do
      (stopped,receipt,ticket)<-withAgentRuntimeUsing (const (error "Unexpected editor startup")) (const (error "Unexpected editor reconnect")) project (pure launch) $ \runtime->do
        _<-syncPrimary runtime project (Just client) key (Capabilities False False False []) False >>= right
        number<-sendAgent (agentHub runtime) Human (primaryAgent runtime) "scope exit" >>= right
        entries<-waitRequests runtime
        delivery<-case [request | DeliverPrimary request<-entries] of
          [request]->pure request
          _->error "Expected shutdown delivery"
        admitted<-admitPrimaryDelivery runtime delivery client key
        assert "scope-exit fixture admits primary prompt" (maybe False (const True) admitted)
        pure (runtime,delivery,number)
      active<-primaryDeliveryActive stopped
      assert "runtime teardown releases admitted delivery ownership" (not active)
      late<-admitPrimaryDelivery stopped receipt client key
      assert "closed runtime cannot re-admit retained receipt" (maybe True (const False) late)
      completed<-completePrimaryDelivery stopped (Just client) (Just key) False (Right Null)
      assert "closed runtime cannot publish a late completion" (not completed)
      value<-waitAgent (agentHub stopped) Human (primaryAgent stopped) ticket 0 >>= right
      assert "teardown resolves the Hub ticket" (field "status" value/=Just ("running"::T.Text))

-- The host launch lifetime belongs to the runtime, independently of a view.
creationChecks :: FilePath -> IO ()
creationChecks root=do
  let project=root </> "project"
      config=root </> "config"
      launch=ACP.Launch "python3" [root </> "provider.py"] [("PROBE_LOG",root </> "creation-provider.jsonl")]
      spec=SpawnSpec "Host child" "One initial task" project Shared Fresh Nothing Nothing
      noEditor _=error "Shared creation must not open another editor"
      environment sid=[("XDG_CONFIG_HOME",config),("XDG_DATA_HOME",root </> "creation-data"),("THC_EDIT_SESSION",replicate 48 sid)]
      failed=either (const True) (const False)
  writeFile (root </> "creation-provider.jsonl") ""
  started<-newEmptyMVar
  release<-newEmptyMVar
  withEnvironment (environment 'c') $ do
    closedRuntime<-withAgentRuntimeUsing noEditor noEditor project (putMVar started () >> readMVar release >> pure launch) $ \runtime->do
      _<-requestAgentCreation runtime spec >>= right
      await "host launch starts on its owned worker" (not <$> isEmptyMVar started)
      overlapping<-requestAgentCreation runtime spec {spawnName="Overlapping"}
      assert "one pending host launch refuses a second request" (failed overlapping)
      summaries<-agentSummaries (agentHub runtime)
      assert "pending host launch reserves only one child" (length summaries==2 && length (filter ((=="starting").summaryStatus) summaries)==1)
      putMVar release ()
      completed<-waitRequests runtime
      (child,ticket)<-case [result | AgentCreated result<-completed] of
        [Right value]->pure value
        _->error "Expected one completed host creation"
      result<-waitAgent (agentHub runtime) Human child ticket 2000 >>= right
      assert "host creation returns the actual initial task ticket" (field "status" result==Just ("completed"::T.Text))
      history<-historyAgent (agentHub runtime) Human child 0 100 >>= right
      assert "host creation queues its task exactly once" (length (filter ((=="message_queued").historyKind) (historyEvents history))==1)
      drained<-drainAgentRequests runtime
      assert "host creation completion drains once" (null [() | AgentCreated _<-drained])
      _<-requestAgentCreation runtime spec {spawnName=""} >>= right
      rejected<-waitRequests runtime
      assert "drained launch slot is reusable and retains Hub validation" (case [reply | AgentCreated reply<-rejected] of [Left _]->True; _->False)
      pure runtime
    refused<-requestAgentCreation closedRuntime spec
    assert "closed runtime refuses new provider acquisition" (failed refused)
  acquiring<-newEmptyMVar
  joined<-newIORef False
  gate<-newEmptyMVar
  withEnvironment (environment 'd') $ do
    record<-newSessionRecord Nothing [project]
    let saved=record {sessionId=replicate 48 'd',sessionDirectory=project}
    rememberSession saved
    path<-(++".agents.json") <$> checkpointPath (sessionId saved)
    runtime<-withAgentRuntimeUsing noEditor noEditor project
      ((putMVar acquiring () >> readMVar gate >> pure launch) `finally` writeIORef joined True) $ \owner->do
        activateAgentCheckpoint owner
        _<-requestAgentCreation owner spec >>= right
        await "shutdown fixture reaches provider acquisition" (not <$> isEmptyMVar acquiring)
        pure owner
    assert "runtime close joins pending provider acquisition" =<< readIORef joined
    summaries<-agentSummaries (agentHub runtime)
    assert "runtime close releases the startup reservation" (all ((=="ended").summaryStatus) summaries)
    checkpoint<-either error pure . eitherDecodeStrict' =<< BS.readFile path
    let entries=maybe [] id (field "hub" checkpoint >>= field "agents"::Maybe [Value])
        savedHistory=concat [events | entry<-entries,field "name" entry==Just ("Host child"::T.Text),Just events<-[field "history" entry::Maybe [Value]]]
    assert "final checkpoint follows cancelled startup cleanup" (any ((==Just ("failed"::T.Text)).field "kind") savedHistory)
    forgetSession (sessionId saved)

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
      _ <- syncPrimary runtime project Nothing "primary-private" (Capabilities False True False []) False >>= right
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
