{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
-- | Connect the agent hub to editor sessions, bridge capabilities and recovery.
--
-- The runtime owns child launch/reconnect workers and the primary conversation
-- mailbox. Private checkpoints are separate from public agent descriptions and
-- activate only after the session lifetime lock is held. Invalid recovery data is
-- retained and disables spawning rather than being silently replaced.
module Hide.AgentRuntime
  ( AgentRuntime, AgentRequest(..), PrimaryControl(..), primaryControlCurrent, rejectPrimaryControl, failPrimaryControl, withAgentRuntime, withAgentRuntimeUsing
  , agentHub, agentAccess, primaryAgent, primaryServers, drainAgentRequests
  , syncPrimary, recordPrimaryEvent, failPendingPrimary, agentSession, checkpointAgents, activateAgentCheckpoint, runtimeNotice, requestAgentReconnect
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Async (Async, async, cancel, poll, withAsync)
import Control.Exception (IOException, bracket, finally, mask, onException, try, evaluate)
import Control.Monad (filterM, forM_, forever, unless, void, when)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (pathIsSymbolicLink, removeFile, renameFile)
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, takeDirectory)
import System.IO (IOMode(ReadMode), hClose, hFlush, openBinaryTempFile, withBinaryFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Timeout (timeout)
import System.Mem.StableName (StableName,makeStableName)
#ifndef mingw32_HOST_OS
import System.Posix.Files (setFileMode)
#endif
import qualified Hide.ACP as ACP
import Hide.AgentACP (ACPPermission, startACPDriver)
import Hide.AgentAccess
import Hide.AgentHub
import qualified Hide.AgentWorkspace as Workspace
import Hide.EditorMCP (editorServersAt)
import Hide.MCPPermissions (readAgentLimitsFor, readAgentContexts)
import Hide.Protocol (WirePacket(..))
import Hide.Remote (RemotePeer(..), withLocalPeer)
import Hide.Session

-- | A host/UI mailbox request whose reply cell stays filled after completion.
-- Consumers must reject stale requests already resolved elsewhere.
data AgentRequest = DeliverPrimary HubMessage (MVar (Either Text Value))
  | ControlPrimary PrimaryControl
  | CancelPrimary | EndPrimary
  | ProviderPermission AgentId ACPPermission (MVar (Maybe Text))
  | AgentReconnected AgentId (Either Text ())
-- | One host-only control bound to the exact primary connection. Reply cells
-- stay filled after cancellation so a drained request cannot be admitted later.
data PrimaryControl
  = ConfigurePrimary !(StableName ACP.Client) !Text ![(Text,Text)] !(MVar (Either Text Capabilities))
  | SteerPrimary !(StableName ACP.Client) !Text !HubMessage !(MVar (Either Text Value))

-- Reply identity is the control lifetime; never compare prompt/config payloads.
instance Eq PrimaryControl where
  ConfigurePrimary _ _ _ a == ConfigurePrimary _ _ _ b=a==b
  SteerPrimary _ _ _ a == SteerPrimary _ _ _ b=a==b
  _ == _=False

data AgentRuntime = AgentRuntime
  { agentHub :: AgentHub, agentAccess :: AgentAccess, primaryAgent :: AgentId
  , runtimeState :: MVar RuntimeState, primaryToken :: Text
  , rootSession :: Maybe SessionRecord, configuredLaunch :: IO ACP.Launch
  , checkpointFile :: Maybe FilePath, checkpointWritable :: Bool
  , checkpointLock :: MVar (), reconnectWorkers :: MVar (M.Map AgentId (Async ())) }

-- Reply cells stay filled after consumption so a UI callback can reject a stale
-- request even when cancellation raced with draining the mailbox.
data RuntimeState = RuntimeState
  { requests :: [AgentRequest]
  , deliveries :: [MVar (Either Text Value)]
  , permissions :: M.Map AgentId [MVar (Maybe Text)]
  , sessions :: M.Map AgentId SessionRecord
  , launches :: M.Map AgentId ACP.Launch
  , primaryState :: Maybe (FilePath,Maybe (StableName ACP.Client),Text,Capabilities)
  , primaryControl :: Maybe PrimaryControl
  , primaryEvents :: Maybe (DriverEvent -> IO ())
  , closed :: Bool, notice :: Maybe Text, checkpointActive :: Bool }

-- | Scope providers, private bridge access, pending replies and checkpoint workers.
withAgentRuntime :: FilePath -> IO ACP.Launch -> (AgentRuntime -> IO a) -> IO a
withAgentRuntime = withAgentRuntimeUsing startEditor resumeEditor

-- Only editor acquisition is replaceable: tests use the real Hub, ACP process
-- and worktree code without recursively launching their own test executable.
withAgentRuntimeUsing :: (FilePath -> IO SessionRecord) -> (SessionRecord -> IO ()) -> FilePath -> IO ACP.Launch -> (AgentRuntime -> IO a) -> IO a
withAgentRuntimeUsing openEditor restoreEditor directory getLaunch action = bracket acquire release $ \runtime ->
  withAsync (forever (threadDelay 2000000 >> void (checkpointAgents runtime))) (const (action runtime))
  where
    acquire = mask $ \restore -> do
      access <- newAgentAccess
      state <- newMVar (RuntimeState [] [] M.empty M.empty M.empty Nothing Nothing Nothing False Nothing False)
      root <- lookupEnv "THC_EDIT_SESSION" >>= traverse (\sid -> do
        saved <- loadSession sid
        case saved of
          Just record -> pure record
          Nothing -> do
            record <- newSessionRecord Nothing ["--",directory]
            pure record {sessionId=sid,sessionDirectory=directory})
      path <- traverse (fmap (++".agents.json") . checkpointPath . sessionId) root
      saved <- maybe (pure (Right Nothing)) readRuntimeCheckpoint path
      initial <- newMVar True
      recoveryFault <- newMVar (either Just (const Nothing) saved)
      identity <- newEmptyMVar
      let limits workingDirectory = do
            result <- readLimits workingDirectory
            starting <- readMVar initial
            fault <- readMVar recoveryFault
            pure $ if starting then either (const (Right (HubLimits 8 4))) Right result else
              case fault of Just _ -> Left "Agent recovery checkpoint is invalid; spawning is disabled until it is repaired."; _ -> result
          starter = startChild openEditor restoreEditor getLaunch root access state
      restored <- case saved of
        Right (Just checkpoint) -> restoreHubWithLimits limits starter (savedHub checkpoint)
        _ -> Right <$> newAgentHubWithLimits limits starter
      (hub,recovered,fault) <- case restored of
        Right value -> pure (value,either (const Nothing) id saved,either Just (const Nothing) saved)
        Left err -> do
          fresh <- newAgentHubWithLimits limits starter
          pure (fresh,Nothing,Just err)
      let cleanup = closeAgentHub hub
          driver = primaryDriver state access (tryReadMVar identity) Nothing directory "" (Capabilities False False False [])
      ident <- (case recovered of
        Nothing -> restore (registerAgent hub "Primary" directory driver) >>= either (ioError . userError . T.unpack) pure
        Just checkpoint -> do
          let primary = savedPrimary checkpoint
          restore (updateExternalAgent hub primary driver) >>= either (ioError . userError . T.unpack) (const (pure ()))
          pure primary) `onException` cleanup
      putMVar identity ident
      modifyMVar_ recoveryFault (const (pure fault))
      modifyMVar_ initial (const (pure False))
      token <- grantAgentAccess access ident `onException` cleanup
      modifyMVar_ state $ \s -> pure s
        { sessions=maybe id (M.insert ident) root (maybe M.empty savedSessions recovered)
        , launches=maybe M.empty savedLaunches recovered
        , notice=(<>" The original checkpoint was retained; agent spawning is disabled.") <$> fault }
      lock <- newMVar ()
      workers <- newMVar M.empty
      pure (AgentRuntime hub access ident state token root getLaunch path (fault==Nothing) lock workers)
    release runtime =
      -- The periodic worker is already joined by withAsync. Capture live phases
      -- before driver shutdown; an explicit session Exit removed its catalog.
      void (checkpointAgents runtime) `finally` shutdown runtime
    shutdown runtime = do
      modifyMVar_ (runtimeState runtime) $ \s -> do
        mapM_ (rejectPrimaryControl "Editor closed.") (primaryControl s)
        forM_ (deliveries s) (\cell -> void (tryPutMVar cell (Left "Editor closed.")))
        forM_ (concat (M.elems (permissions s))) (\cell -> void (tryPutMVar cell Nothing))
        pure s {closed=True,requests=[]}
      withMVar (reconnectWorkers runtime) (mapM_ cancel)
      closeAgentHub (agentHub runtime) `finally` do
        ids <- M.keys . sessions <$> readMVar (runtimeState runtime)
        mapM_ (revokeAgentAccess (agentAccess runtime)) (primaryAgent runtime:ids)

primaryServers :: AgentRuntime -> IO [Value]
primaryServers runtime = maybe (pure []) (\record -> editorServersAt (sessionId record) (Just (primaryToken runtime))) (rootSession runtime)

-- | Drain unresolved mailbox requests for adoption by the owning UI tick.
drainAgentRequests :: AgentRuntime -> IO [AgentRequest]
drainAgentRequests runtime = modifyMVar (runtimeState runtime) $ \s -> do
  ready <- filterM unresolved (requests s)
  pure (s {requests=[]},ready)
  where
    unresolved (DeliverPrimary _ cell) = isEmptyMVar cell
    unresolved (ControlPrimary control) = primaryControlWaiting control
    unresolved (ProviderPermission _ _ cell) = isEmptyMVar cell
    unresolved _ = pure True

-- | Publish primary capabilities and exact connection identity. A replacement
-- client invalidates captured controls even if it reuses the same session key;
-- capability refresh retains the existing provider lifetime.
syncPrimary :: AgentRuntime -> FilePath -> Maybe ACP.Client -> Text -> Capabilities -> Bool -> IO (Either Text ())
syncPrimary runtime directory client key caps busy = do
  connection<-traverse (\value->makeStableName =<< evaluate value) client
  previous <- readMVar (runtimeState runtime)
  let signature=(directory,connection,key,caps)
      sameProvider (oldDirectory,oldConnection,oldKey,_)=
        (oldDirectory,oldConnection,oldKey)==(directory,connection,key)
  result <- if primaryState previous==Just signature then pure (Right ()) else case primaryEvents previous of
    Just emit | maybe False sameProvider (primaryState previous)->do
      emit (ProviderCapabilities caps)
      modifyMVar_ (runtimeState runtime) (\s->pure s {primaryState=Just signature})
      pure (Right ())
    _->do
      loaded <- try (configuredLaunch runtime)
      case loaded of
        Left (_::IOException) -> pure (Left "Could not read configured agent provider.")
        Right launch -> do
          failPrimaryControl runtime "Primary provider changed."
          updated <- updateExternalAgent (agentHub runtime) (primaryAgent runtime)
            (primaryDriver (runtimeState runtime) (agentAccess runtime) (pure (Just (primaryAgent runtime))) connection directory key caps)
          case updated of
            Left err -> pure (Left err)
            Right emit -> do
              modifyMVar_ (runtimeState runtime) $ \s -> pure s
                { primaryState=Just signature,primaryEvents=Just emit
                , launches=M.insert (primaryAgent runtime) launch (launches s)
                , sessions=M.adjust (\record -> record {sessionDirectory=directory}) (primaryAgent runtime) (sessions s) }
              pure (Right ())
  -- Busy state changes independently of session metadata, including while a
  -- malformed policy prevents a metadata update.
  setExternalAgentBusy (agentHub runtime) (primaryAgent runtime) busy
  pure result

-- | Publish an already-scrubbed update from the exact live primary connection.
-- The sink retains its Hub incarnation after this check, so concurrent provider
-- replacement also rejects publication. This never synchronizes or replays state.
recordPrimaryEvent :: AgentRuntime -> ACP.Client -> Text -> DriverEvent -> IO ()
recordPrimaryEvent runtime client key event=do
  owner<-makeStableName =<< evaluate client
  publish<-withMVar (runtimeState runtime) $ \state->pure $ case primaryState state of
    Just (_,Just current,sid,_) | not (closed state),owner==current,key==sid->primaryEvents state
    _->Nothing
  mapM_ ($ event) publish

failPendingPrimary :: AgentRuntime -> Text -> IO ()
failPendingPrimary runtime = failDeliveries (runtimeState runtime)

agentSession :: AgentRuntime -> AgentId -> IO (Maybe SessionRecord)
agentSession runtime ident = M.lookup ident . sessions <$> readMVar (runtimeState runtime)

-- | Schedule provider initialization off the UI thread; shutdown joins the worker.
requestAgentReconnect :: AgentRuntime -> AgentId -> IO (Either Text ())
requestAgentReconnect runtime ident = modifyMVar (reconnectWorkers runtime) $ \workers -> do
  liveWorkers <- M.filter isRunning <$> traverse (\worker -> (worker,) <$> poll worker) workers
  stopped <- closed <$> readMVar (runtimeState runtime)
  let retained = M.map fst liveWorkers
  if stopped then pure (retained,Left "Editor closed.")
  else if M.member ident retained then pure (retained,Left "This agent is already reconnecting.")
  else do
    worker <- async $ do
      result <- reconnectAgent (agentHub runtime) ident
      enqueue (runtimeState runtime) (AgentReconnected ident result)
    pure (M.insert ident worker retained,Right ())
  where isRunning (_,Nothing) = True
        isRunning _ = False

-- A one-shot host notice keeps checkpoint failures out of provider events.
runtimeNotice :: AgentRuntime -> IO (Maybe Text)
runtimeNotice runtime = modifyMVar (runtimeState runtime) (\s -> pure (s {notice=Nothing},notice s))

-- | Enable private sidecar persistence only after acquiring the session lifetime lock.
activateAgentCheckpoint :: AgentRuntime -> IO ()
activateAgentCheckpoint runtime = modifyMVar_ (runtimeState runtime) $ \s ->
  pure s {checkpointActive=not (closed s)}

readLimits :: FilePath -> IO (Either Text HubLimits)
readLimits directory = fmap (uncurry HubLimits) <$> readAgentLimitsFor directory

primaryDriver :: MVar RuntimeState -> AgentAccess -> IO (Maybe AgentId) -> Maybe (StableName ACP.Client) -> FilePath -> Text -> Capabilities -> AgentDriver
primaryDriver state access identity connection directory key caps = AgentDriver
  { driverDirectory=directory,driverSessionKey=key,driverCapabilities=caps
  , driverSteer= \message->case connection of
      Nothing->pure (Left "Primary provider is disconnected.")
      Just owner->do
        reply<-newEmptyMVar
        requestPrimaryControl state (SteerPrimary owner key message reply) reply
  , driverConfigure= \settings->case connection of
      Nothing->pure (Left "Primary provider is disconnected.")
      Just owner->do
        reply<-newEmptyMVar
        requestPrimaryControl state (ConfigurePrimary owner key settings reply) reply
  , driverDeliver= \message -> mask $ \restore -> do
      cell <- newEmptyMVar
      modifyMVar_ state $ \s -> if closed s
        then putMVar cell (Left "Editor closed.") >> pure s
        else pure s {requests=requests s++[DeliverPrimary message cell],deliveries=cell:deliveries s}
      restore (readMVar cell) `finally` do
        void (tryPutMVar cell (Left "Primary delivery interrupted."))
        modifyMVar_ state (\s -> pure s
          {deliveries=filter (/=cell) (deliveries s),requests=filter (not . matchingDelivery cell) (requests s)})
  , driverCancel=modifyMVar_ state $ \s -> do
      mapM_ (rejectPrimaryControl "Agent control cancelled.") (primaryControl s)
      -- A drained delivery may still be executing. Its UI owner releases the
      -- reply only after the underlying prompt settles, preserving Hub's barrier.
      forM_ [cell | DeliverPrimary _ cell <- requests s] (\cell -> void (tryPutMVar cell (Left "Cancelled.")))
      pure s {requests=filter (not . isDelivery) (requests s)++[CancelPrimary | not (closed s)]}
  , driverStop=do
      identity >>= mapM_ (revokeAgentAccess access)
      failDeliveries state "Primary agent ended."
      enqueue state EndPrimary }

-- | Admission checks the live provider object, not its reusable session key.
-- Cancellation before this check refuses the request without protocol IO.
primaryControlCurrent :: AgentRuntime -> PrimaryControl -> Maybe ACP.Client -> Maybe Text -> IO Bool
primaryControlCurrent runtime control client session=do
  reserved<-agentControlPending (agentHub runtime) (primaryAgent runtime)
  waiting<-primaryControlWaiting control
  case (client,session) of
    (Just current,Just key) | reserved && waiting->do
      owner<-makeStableName =<< evaluate current
      pure $ case control of
        ConfigurePrimary expected sid _ _->owner==expected && key==sid
        SteerPrimary expected sid _ _->owner==expected && key==sid
    _->pure False

primaryControlWaiting :: PrimaryControl -> IO Bool
primaryControlWaiting (ConfigurePrimary _ _ _ reply)=isEmptyMVar reply
primaryControlWaiting (SteerPrimary _ _ _ reply)=isEmptyMVar reply

-- | Terminally refuse one retained control. Repeated retirement preserves its
-- first result and cannot turn a cancelled reply into success.
rejectPrimaryControl :: Text -> PrimaryControl -> IO ()
rejectPrimaryControl reason (ConfigurePrimary _ _ _ reply)=void (tryPutMVar reply (Left reason))
rejectPrimaryControl reason (SteerPrimary _ _ _ reply)=void (tryPutMVar reply (Left reason))

-- | Resolve the primary's outstanding control without completing its running
-- prompt. Provider cancellation still waits for that prompt's real outcome.
failPrimaryControl :: AgentRuntime -> Text -> IO ()
failPrimaryControl runtime reason=withMVar (runtimeState runtime) $ \state->
  mapM_ (rejectPrimaryControl reason) (primaryControl state)

requestPrimaryControl :: MVar RuntimeState -> PrimaryControl -> MVar (Either Text a) -> IO (Either Text a)
requestPrimaryControl state control reply=mask $ \restore->do
  modifyMVar_ state $ \current->
    if closed current then rejectPrimaryControl "Editor closed." control >> pure current
    else if primaryControl current/=Nothing then rejectPrimaryControl "An agent operation is already pending." control >> pure current
    else pure current {primaryControl=Just control,requests=requests current++[ControlPrimary control]}
  restore (readMVar reply) `finally` do
    rejectPrimaryControl "Agent control interrupted." control
    modifyMVar_ state $ \current->pure current
      {primaryControl=if primaryControl current==Just control then Nothing else primaryControl current
      ,requests=filter (\request->case request of ControlPrimary queued->queued/=control; _->True) (requests current)}

isDelivery :: AgentRequest -> Bool
isDelivery DeliverPrimary{} = True
isDelivery _ = False
matchingDelivery :: MVar (Either Text Value) -> AgentRequest -> Bool
matchingDelivery cell (DeliverPrimary _ other) = cell==other
matchingDelivery _ _ = False

failDeliveries :: MVar RuntimeState -> Text -> IO ()
failDeliveries state reason = modifyMVar_ state $ \s -> do
  mapM_ (rejectPrimaryControl reason) (primaryControl s)
  forM_ (deliveries s) (\cell -> void (tryPutMVar cell (Left reason)))
  pure s {requests=filter (not . isDelivery) (requests s)}

enqueue :: MVar RuntimeState -> AgentRequest -> IO ()
enqueue state request = modifyMVar_ state $ \s -> pure s
  {requests=if closed s then requests s else requests s++[request]}

requestPermission :: MVar RuntimeState -> AgentId -> ACPPermission -> IO (Maybe Text)
requestPermission state ident permission = mask $ \restore -> do
  cell <- newEmptyMVar
  modifyMVar_ state $ \s -> if closed s then putMVar cell Nothing >> pure s else pure s
    {requests=requests s++[ProviderPermission ident permission cell],permissions=M.insertWith (++) ident [cell] (permissions s)}
  restore (readMVar cell) `finally` do
    void (tryPutMVar cell Nothing)
    modifyMVar_ state (\s -> pure s
      {permissions=M.update (nonempty . filter (/=cell)) ident (permissions s)
      ,requests=filter (\request -> case request of ProviderPermission _ _ other -> cell/=other; _ -> True) (requests s)})
  where nonempty [] = Nothing
        nonempty xs = Just xs

cancelPermissions :: MVar RuntimeState -> AgentId -> IO ()
cancelPermissions state ident = modifyMVar_ state $ \s -> do
  forM_ (M.findWithDefault [] ident (permissions s)) (\cell -> void (tryPutMVar cell Nothing))
  pure s {permissions=M.delete ident (permissions s),requests=filter keep (requests s)}
  where keep (ProviderPermission other _ _) = other/=ident
        keep _ = True

startChild :: (FilePath -> IO SessionRecord) -> (SessionRecord -> IO ()) -> IO ACP.Launch -> Maybe SessionRecord -> AgentAccess -> MVar RuntimeState -> StartProvider
startChild openEditor restoreEditor getLaunch root access state request emit = mask $ \restore -> do
  let ident = startAgent request
      spec = startSpec request
      retire = revokeAgentAccess access ident >> cancelPermissions state ident
  result <- restore (prepare spec) `onException` retire
  case result of
    Left err -> retire >> pure (Left err)
    Right (record,launch,context) -> do
      token <- grantAgentAccess access ident
      let run = do
            ownServers <- editorServersAt (sessionId record) Nothing
            orchestration <- maybe (pure []) (\r -> editorServersAt (sessionId r) (Just token)) root
            let servers = ownServers++map renameServer orchestration
                configured = launch {ACP.environment=("THC_EDIT_SESSION",sessionId record):filter ((/="THC_EDIT_SESSION").fst) (ACP.environment launch)}
                started = request {startSpec=spec {spawnDirectory=sessionDirectory record}}
                event value = do
                  case value of ProviderClosed -> retire; _ -> pure ()
                  emit value
            startACPDriver configured servers context (requestPermission state ident) started event
      started <- restore run `onException` retire
      case started of
        Left err -> retire >> pure (Left err)
        Right driver -> pure (Right driver
          {driverCancel=cancelPermissions state ident >> driverCancel driver
          ,driverStop=driverStop driver `finally` retire})
  where
    prepare spec | startResume request/=Nothing = do
      saved <- readMVar state
      case (M.lookup (startAgent request) (sessions saved),M.lookup (startAgent request) (launches saved)) of
        (Just record,Just launch) | sessionDirectory record==spawnDirectory spec -> do
          workspace <- Workspace.sharedAgentWorkspace (sessionDirectory record)
          case workspace of
            Left err -> pure (Left err)
            Right selected | Workspace.workspacePath selected/=sessionDirectory record ->
              pure (Left "The recovered workspace has changed location.")
            Right _ -> do
              contexts <- readAgentContexts (sessionDirectory record)
              case contexts of
                Left err -> pure (Left err)
                Right context -> do
                  -- Shared children already use this running editor. Other
                  -- saved workspaces may need their checkpoint restored first.
                  unless (Just (sessionId record)==fmap sessionId root) (restoreEditor record)
                  pure (Right (record,launch,contextText context))
        _ -> pure (Left "The saved provider or editor workspace is unavailable.")
    prepare spec = do
      owner <- case startOwner request of
        Human -> pure root
        Agent ident -> M.lookup ident . sessions <$> readMVar state
      case owner of
        Nothing -> pure (Left "Agent tools require a persistent editor session.")
        Just parent -> do
          workspace <- case spawnWorkspace spec of
            Shared -> Workspace.sharedAgentWorkspace (spawnDirectory spec)
            Worktree ref branch name -> Workspace.createAgentWorktree (spawnDirectory spec) ref branch (fromMaybe (spawnName spec) name)
          case workspace of
            Left err -> pure (Left err)
            Right selected -> do
              contexts <- readAgentContexts (Workspace.workspacePath selected)
              case contexts of
                Left err -> pure (Left err)
                Right context -> do
                  provider <- case startSource request of
                    Nothing -> Right <$> getLaunch
                    Just source -> maybe (Left "The source provider is not available for forking.") Right . M.lookup (sourceAgent source) . launches <$> readMVar state
                  case provider of
                    Left err -> pure (Left err)
                    Right launch -> do
                      record <- case spawnWorkspace spec of
                        Shared -> pure parent {sessionDirectory=Workspace.workspacePath selected}
                        Worktree{} -> openEditor (Workspace.workspacePath selected)
                      modifyMVar_ state $ \s -> pure s
                        {sessions=M.insert (startAgent request) record (sessions s),launches=M.insert (startAgent request) launch (launches s)}
                      pure (Right (record,launch,contextText context))
    renameServer (Object fields) = Object (KM.insert "name" (String "agents") fields)
    renameServer value = value

contextText :: Value -> Text
contextText value = T.intercalate "\n\n"
  [label<>"\n"<>body | (key,label) <- [("global","Global agent context:"),("project","Project agent context:")]
  , Just section <- [field key value :: Maybe Value], Just body <- [field "text" section], not (T.null body)]

field :: FromJSON a => Key -> Value -> Maybe a
field key = parseMaybe (withObject "field" (.: key))

startEditor :: FilePath -> IO SessionRecord
startEditor directory = do
  fresh <- newSessionRecord Nothing ["--",directory]
  let record = fresh {sessionDirectory=directory}
  rememberSession record
  attachEditor False record
  pure record

resumeEditor :: SessionRecord -> IO ()
resumeEditor record = do
  activity <- sessionActivity record
  -- A status probe does not take the display from an attached human.
  when (activity==Nothing) (attachEditor True record)

attachEditor :: Bool -> SessionRecord -> IO ()
attachEditor resume record = do
  withLocalPeer (sessionId record) resume (sessionArguments record) $ \peer -> do
    ready <- timeout 75000000 (awaitReady peer False)
    unless (ready==Just ()) (ioError (userError "Agent editor did not become ready within 75 seconds."))
  detached <- timeout 5000000 awaitDetached
  unless (detached==Just ()) (ioError (userError "Agent editor started but detach could not be confirmed."))
  where
    awaitReady peer assets = peerReceive peer >>= \packet -> case packet of
      Nothing -> ioError (userError "Agent editor ended before becoming ready.")
      Just (JsonPacket value)
        | field "type" value==Just ("assets"::Text) -> awaitReady peer True
        | field "type" value==Just ("connection"::Text), field "connected" value==Just True, assets -> pure ()
        | field "type" value `elem` [Just ("closed"::Text),Just "error"] -> ioError (userError "Agent editor startup failed.")
      _ -> awaitReady peer assets
    awaitDetached = do
      activity <- sessionActivity record
      if (activity >>= field "attached")==Just False then pure () else threadDelay 20000 >> awaitDetached

data RuntimeCheckpoint = RuntimeCheckpoint
  { savedHub :: Value, savedPrimary :: AgentId
  , savedSessions :: M.Map AgentId SessionRecord
  , savedLaunches :: M.Map AgentId ACP.Launch }

checkpointLimit :: Int
checkpointLimit = 256*1024*1024

readRuntimeCheckpoint :: FilePath -> IO (Either Text (Maybe RuntimeCheckpoint))
readRuntimeCheckpoint path = do
  loaded <- try $ do
    symbolic <- pathIsSymbolicLink path
    when symbolic (ioError (userError "Agent checkpoint cannot be a symlink"))
    withBinaryFile path ReadMode (\handle -> BS.hGet handle (checkpointLimit+1))
  pure $ case loaded of
    Left err | isDoesNotExistError err -> Right Nothing
             | otherwise -> Left "Could not read the private agent checkpoint."
    Right bytes
      | BS.length bytes>checkpointLimit -> Left "Agent checkpoint exceeds 256 MiB."
      | otherwise -> case eitherDecodeStrict' bytes >>= parseEither checkpointParser of
          Left _ -> Left "Invalid agent recovery checkpoint."
          Right checkpoint -> Right (Just checkpoint)

checkpointParser :: Value -> Parser RuntimeCheckpoint
checkpointParser = withObject "agent runtime" $ \o -> do
  version <- o .: "schemaVersion"
  unless (version==(1::Int)) (fail "Version")
  primary <- AgentId <$> o .: "primary"
  hub <- o .: "hub"
  entries <- maybe (fail "Directory") pure (field "agents" hub :: Maybe [Value])
  let identities = [AgentId ident | value <- entries, Just ident <- [field "id" value]]
      primaryEntries = [value | value <- entries, field "id" value==Just (agentIdText primary)]
  unless (length entries<=1024 && length identities==length entries &&
    case primaryEntries of
      [value] -> field "external" value==Just True && field "parent" value==Just (Nothing::Maybe Text)
      _ -> False) (fail "Primary")
  editorValues <- o .: "sessions"
  providerValues <- o .: "providers"
  unless (length editorValues<=1024 && length providerValues<=1024) (fail "Count")
  editors <- mapM (withObject "editor session" $ \entry -> do
    ident <- AgentId <$> entry .: "agent"
    record <- entry .: "session"
    let sid = sessionId record
    unless (ident `elem` identities && length sid==48 && all (`elem` ("0123456789abcdef"::String)) sid &&
      sessionHost record==Nothing && isAbsolute (sessionDirectory record) && length (sessionDirectory record)<=32768 &&
      length (sessionArguments record)<=256 && all (\arg -> length arg<=32768 && '\0' `notElem` arg) (sessionArguments record)) (fail "Session")
    pure (ident,record)) editorValues
  providers <- mapM (withObject "provider" $ \entry -> do
    ident <- AgentId <$> entry .: "agent"
    executable <- entry .: "executable"
    arguments <- entry .: "arguments"
    environment <- entry .: "environment"
    unless (ident `elem` identities && not (null executable) && valid executable && length arguments<=256 && all valid arguments &&
      length environment<=1024 && all (\(key,value) -> not (null key) && valid key && valid value && '=' `notElem` key && key/="THC_EDIT_MCP_TOKEN") environment) (fail "Provider")
    pure (ident,ACP.Launch executable arguments environment)) providerValues
  unless (M.size (M.fromList editors)==length editors && M.size (M.fromList providers)==length providers) (fail "Duplicate")
  -- Binding the human seat does not resume a provider, even when it had been
  -- explicitly ended before the editor crashed. Child ended states remain.
  let restoredEntries = [case value of
        Object entry | field "id" value==Just (agentIdText primary) -> Object (KM.insert "ended" (Bool False) entry)
        _ -> value | value <- entries]
      restoredHub = case hub of Object fields -> Object (KM.insert "agents" (toJSON restoredEntries) fields); _ -> hub
  pure (RuntimeCheckpoint restoredHub primary (M.fromList editors) (M.fromList providers))
  where valid value = length value<=65536 && '\0' `notElem` value

-- Atomic private sidecar publication is separate from the public directory.
-- Catalog checks on both sides of rename close the race with explicit Exit:
-- forgetSession deletes the catalog first, and this writer never recreates it.
checkpointAgents :: AgentRuntime -> IO (Either Text ())
checkpointAgents runtime = withMVar (checkpointLock runtime) $ \_ -> do
  enabled <- checkpointActive <$> readMVar (runtimeState runtime)
  if enabled then saveAgentCheckpoint runtime else pure (Right ())

saveAgentCheckpoint :: AgentRuntime -> IO (Either Text ())
saveAgentCheckpoint runtime = case (checkpointFile runtime,rootSession runtime) of
  (Just path,Just root) | checkpointWritable runtime -> do
    result <- try $ do
      present <- loadSession (sessionId root)
      case present of
        Nothing -> removeMissing path
        Just _ -> do
          hub <- snapshotHub (agentHub runtime)
          state <- readMVar (runtimeState runtime)
          let value = object
                ["schemaVersion" .= (1::Int),"primary" .= agentIdText (primaryAgent runtime),"hub" .= hub
                ,"sessions" .= [object ["agent" .= agentIdText ident,"session" .= record] | (ident,record) <- M.toList (sessions state)]
                ,"providers" .= [object ["agent" .= agentIdText ident,"executable" .= ACP.executable launch
                  ,"arguments" .= ACP.arguments launch,"environment" .= filter ((/="THC_EDIT_MCP_TOKEN").fst) (ACP.environment launch)]
                  | (ident,launch) <- M.toList (launches state)]]
              bytes = BL.take (fromIntegral checkpointLimit+1) (encode value)
          when (BL.length bytes>fromIntegral checkpointLimit) (ioError (userError "Agent checkpoint exceeds 256 MiB"))
          -- Reject an incomplete cross-thread snapshot; the periodic writer will
          -- retry instead of replacing a good checkpoint with invalid state.
          case parseEither checkpointParser value of Left _ -> ioError (userError "Agent checkpoint is inconsistent"); Right _ -> pure ()
          bracket (openBinaryTempFile (takeDirectory path) ".thc-agents-") cleanup $ \(temporary,handle) -> do
#ifndef mingw32_HOST_OS
            setFileMode temporary 0o600
#endif
            BL.hPut handle bytes
            hFlush handle
            hClose handle
            stillPresent <- loadSession (sessionId root)
            when (stillPresent/=Nothing) (renameFile temporary path)
          stillPresent <- loadSession (sessionId root)
          when (stillPresent==Nothing) (removeMissing path)
    case result of
      Left (_::IOException) -> do
        let message = "Could not save the private agent checkpoint; the previous checkpoint was retained."
        modifyMVar_ (runtimeState runtime) (\s -> pure s {notice=Just message})
        pure (Left message)
      Right () -> pure (Right ())
  _ -> pure (Right ())
  where
    cleanup (temporary,handle) = catchIOError (hClose handle) (const (pure ())) >> removeMissing temporary
    removeMissing path = catchIOError (removeFile path) (\err -> unless (isDoesNotExistError err) (ioError err))
