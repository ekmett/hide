-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE CPP, ExistentialQuantification, GADTs, OverloadedStrings, ScopedTypeVariables #-}
-- |
-- Module      : Hide.AgentRuntime
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : CPP, ExistentialQuantification, GADTs, OverloadedStrings, ScopedTypeVariables
--
-- Connect the agent hub to editor sessions, bridge capabilities and recovery.
--
-- The runtime owns child launch/reconnect workers and the primary conversation
-- mailbox. Private checkpoints are separate from public agent descriptions and
-- activate only after the session lifetime lock is held. Invalid recovery data is
-- retained and disables spawning rather than being silently replaced.
module Hide.AgentRuntime
  ( AgentRuntime, AgentRequest(..), ProviderCall(..), primaryProviderHost, publishProviderEvent, retireProviderCalls, PrimaryDelivery, admitPrimaryDelivery, rejectPrimaryDelivery, completePrimaryDelivery, primaryDeliveryActive, acknowledgePrimaryDelivery, deliveryTurn, deliverySubmission, PrimaryControl(..), requestPrimaryQuery, primaryControlCurrent, rejectPrimaryControl, failPrimaryControl, withAgentRuntime, withAgentRuntimeUsing
  , agentHub, agentAccess, primaryAgent, primaryServers, drainAgentRequests
  , syncPrimary, recordPrimaryEvent, failPendingPrimary, agentSession, checkpointAgents, activateAgentCheckpoint, runtimeNotice, requestAgentCreation, requestAgentReconnect
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Async (Async, async, cancel, poll, withAsync)
import Control.Exception (IOException, bracket, finally, mask_, mask, onException, try, evaluate)
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
import qualified Hide.Terminal as Terminal
import System.FilePath (isAbsolute, takeDirectory)
import System.IO (IOMode(ReadMode), hClose, hFlush, openBinaryTempFile, withBinaryFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import Data.Unique (Unique,newUnique)
import qualified Data.Text.Encoding as TE
import System.Timeout (timeout)
import System.Mem.StableName (StableName,makeStableName)
#ifndef mingw32_HOST_OS
import System.Posix.Files (setFileMode)
#endif
import Hide.Plugin.Agent (StartAgentProvider,ProviderKind(..),ProviderHost(..),ProviderPermission,permissionTitle,permissionDetails,permissionOptions,ProviderEndpoint(..),ProviderFiles(..),ProviderTerminals(..),ProviderTerminal(..),ProviderTerminalOutput(..),ProviderContent(..))
import Hide.Plugin.Provider
import Hide.AgentAccess
import Hide.Plugin.EditorHost (SubmissionIdentity)
import Hide.AgentHub
import qualified Hide.AgentWorkspace as Workspace
import Hide.EditorMCP (editorEndpointsAt)
import Hide.MCPPermissions (readAgentLimitsFor, readAgentContexts)
import Hide.Protocol (WirePacket(..))
import Hide.Remote (RemotePeer(..), withLocalPeer)
import Hide.Session

-- | A host/UI mailbox request whose reply cell stays filled after completion.
-- Consumers must reject stale requests already resolved elsewhere.
data AgentRequest = DeliverPrimary !PrimaryDelivery
  | ControlPrimary PrimaryControl
  | CancelPrimary | EndPrimary
  | ProviderPermission AgentId ProviderPermission (MVar (Either Text (Maybe Text)))
  | forall a. NativeProviderRequest !ProviderIdentity !FilePath !(ProviderCall a) !(MVar (Either Text a))
  | ProviderEvent !ProviderIdentity !DriverEvent
  | ProviderContentEvent !ProviderIdentity !(Maybe ProviderTurnId) !ProviderContent
  | AgentReconnected AgentId (Either Text ())
  | AgentCreated (Either Text (AgentId,Int))
-- Fixed host requests decoded only by the provider adapter. Their typed result
-- cells retain the original call lifetime through worker preparation/adoption.
data ProviderCall a where
  AskProviderPermission :: ProviderPermission -> ProviderCall (Maybe Text)
  ReadProviderFile :: FilePath -> Int -> Maybe Int -> ProviderCall Text
  WriteProviderFile :: FilePath -> Text -> ProviderCall ()
  CreateProviderTerminal :: ProviderTerminal -> ProviderCall Text
  ReadProviderTerminal :: Text -> ProviderCall ProviderTerminalOutput
  WaitProviderTerminal :: Text -> ProviderCall Int
  KillProviderTerminal :: Text -> ProviderCall ()
  ReleaseProviderTerminal :: Text -> ProviderCall ()

-- | One Hub-issued delivery. Its filled terminal reply remains its lifetime;
-- callers cannot mutate that reply or manufacture a fresh admission. Equality
-- observes only reply identity, never the attributed prompt payload.
data PrimaryDelivery = PrimaryDelivery
  { deliveryDirectory :: !FilePath, deliveryProvider :: !(Maybe ProviderIdentity), deliverySession :: !Text
  , deliveryMessage :: !HubMessage, deliveryTurn :: !ProviderTurnId, deliverySubmission :: !ProviderSubmission
  , deliveryAdmission :: MVar (Either Text ()), deliveryResult :: MVar (Either Text Value) }
instance Eq PrimaryDelivery where a==b=deliveryResult a==deliveryResult b


-- | One host-only control bound to the exact primary connection. Reply cells
-- stay filled after cancellation so a drained request cannot be admitted later.
data PrimaryControl
  = ConfigurePrimary !(ProviderIdentity) !Text ![(Text,Text)] !(MVar (Either Text Capabilities))
  | SteerPrimary !ProviderIdentity !Text !HubMessage !ProviderSubmission !(MVar (Either Text Value))
  | QueryPrimary !SubmissionIdentity !(StableName ProviderLaunch) !(Maybe (ProviderIdentity)) !(Maybe Text) !(Maybe AgentConfigRef) !Text !(MVar (Either Text ()))

-- Reply identity is the control lifetime; never compare prompt/config payloads.
instance Eq PrimaryControl where
  ConfigurePrimary _ _ _ a == ConfigurePrimary _ _ _ b=a==b
  SteerPrimary _ _ _ _ a == SteerPrimary _ _ _ _ b=a==b
  QueryPrimary _ _ _ _ _ _ a == QueryPrimary _ _ _ _ _ _ b=a==b
  _ == _=False

data AgentRuntime = AgentRuntime
  { agentHub :: AgentHub, agentAccess :: AgentAccess, primaryAgent :: AgentId
  , runtimeState :: MVar RuntimeState, primaryToken :: Text
  , rootSession :: Maybe SessionRecord, configuredLaunch :: IO ProviderLaunch
  , checkpointFile :: Maybe FilePath, checkpointWritable :: Bool
  , checkpointLock :: MVar (), reconnectWorkers :: MVar (M.Map AgentId (Async ()))
  , creationWorker :: MVar (Maybe (Async (Either Text (AgentId,Int)))) }

-- Reply cells stay filled after consumption so a UI callback can reject a stale
-- request even when cancellation raced with draining the mailbox.
data RuntimeState = RuntimeState
  { requests :: [AgentRequest], providerEventBytes :: !Int
  , providerReplies :: [(ProviderIdentity,Unique,IO ())]
  , deliveries :: [PrimaryDelivery], admittedPrimary :: Maybe PrimaryDelivery
  , permissions :: M.Map AgentId [MVar (Either Text (Maybe Text))]
  , sessions :: M.Map AgentId SessionRecord
  , launches :: M.Map AgentId ProviderLaunch
  , primaryState :: Maybe (FilePath,Maybe (ProviderIdentity),Text,Capabilities)
  , primaryControl :: Maybe PrimaryControl
  , primaryEvents :: Maybe (DriverEvent -> IO ())
  , closed :: Bool, notice :: Maybe Text, checkpointActive :: Bool }

-- | Scope providers, private bridge access, pending replies and checkpoint workers.
withAgentRuntime :: Maybe StartAgentProvider -> FilePath -> IO ProviderLaunch -> (AgentRuntime -> IO a) -> IO a
withAgentRuntime providerFactory = withAgentRuntimeUsing providerFactory startEditor resumeEditor

-- Only editor acquisition is replaceable: tests use the real Hub, ACP process
-- and worktree code without recursively launching their own test executable.
withAgentRuntimeUsing :: Maybe StartAgentProvider -> (FilePath -> IO SessionRecord) -> (SessionRecord -> IO ()) -> FilePath -> IO ProviderLaunch -> (AgentRuntime -> IO a) -> IO a
withAgentRuntimeUsing providerFactory openEditor restoreEditor directory getLaunch action = bracket acquire release $ \runtime ->
  withAsync (forever (threadDelay 2000000 >> void (checkpointAgents runtime))) (const (action runtime))
  where
    acquire = mask $ \restore -> do
      access <- newAgentAccess
      state <- newMVar (RuntimeState [] 0 [] [] Nothing M.empty M.empty M.empty Nothing Nothing Nothing False Nothing False)
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
          starter = startChild providerFactory openEditor restoreEditor getLaunch root access state
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
      creation <- newMVar Nothing
      pure (AgentRuntime hub access ident state token root getLaunch path (fault==Nothing) lock workers creation)
    release runtime =
      -- The periodic worker is already joined by withAsync. Join pending host
      -- acquisition before saving, preserving startup cancellation in the final
      -- checkpoint. Live providers are captured before their later shutdown.
      (do
        modifyMVar_ (runtimeState runtime) (\s->pure s {closed=True})
        modifyMVar_ (creationWorker runtime) (\worker->mapM_ cancel worker >> pure Nothing)
        void (checkpointAgents runtime)) `finally` shutdown runtime
    shutdown runtime = do
      modifyMVar_ (runtimeState runtime) $ \s -> do
        mapM_ (rejectPrimaryControl "Editor closed.") (primaryControl s)
        mapM_ (settleDelivery (Left "Editor closed.")) (deliveries s)
        forM_ (concat (M.elems (permissions s))) (\cell -> void (tryPutMVar cell (Right Nothing)))
        mapM_ (\(_,_,retire)->retire) (providerReplies s)
        pure s {closed=True,requests=[],providerReplies=[],providerEventBytes=0,admittedPrimary=Nothing}
      withMVar (reconnectWorkers runtime) (mapM_ cancel)
      closeAgentHub (agentHub runtime) `finally` do
        ids <- M.keys . sessions <$> readMVar (runtimeState runtime)
        mapM_ (revokeAgentAccess (agentAccess runtime)) (primaryAgent runtime:ids)

primaryServers :: AgentRuntime -> IO [ProviderEndpoint]
primaryServers runtime = maybe (pure []) (\record -> editorEndpointsAt (sessionId record) (Just (primaryToken runtime))) (rootSession runtime)

-- | Actual native services for one host-minted primary acquisition. The existing
-- runtime mailbox owns pending calls; retiring an acquisition fills all replies.
primaryProviderHost :: AgentRuntime -> ProviderIdentity -> FilePath -> ProviderHost
primaryProviderHost runtime identity root=ProviderHost
  (call . AskProviderPermission)
  (Just (ProviderFiles (\path line limit->call (ReadProviderFile path line limit))
    (\path text->call (WriteProviderFile path text))))
  (if Terminal.terminalAvailable then Just (ProviderTerminals (call . CreateProviderTerminal) (call . ReadProviderTerminal)
    (call . WaitProviderTerminal) (call . KillProviderTerminal) (call . ReleaseProviderTerminal)) else Nothing)
  (Just (\turn content->publishContent identity turn content))
  where
    state=runtimeState runtime
    call :: ProviderCall a -> IO (ProviderReply a)
    call operation=mask_ $ do
      ident<-newUnique
      cell<-newEmptyMVar
      let finish=void (tryPutMVar cell (Left "Provider request retired."))
          retire=do
            finish
            modifyMVar_ state (\s->pure s {providerReplies=filter (\(_,key,_)->key/=ident) (providerReplies s)})
      case validateProviderCall operation of
        Left err->void (tryPutMVar cell (Left err))
        Right ()->modifyMVar_ state $ \s->if closed s || length (providerReplies s)>=32
          then finish >> pure s
          else pure s {requests=requests s++[NativeProviderRequest identity root operation cell]
            ,providerReplies=(identity,ident,finish):providerReplies s}
      pure (ProviderReply (tryReadMVar cell) (readMVar cell) retire)
    publishContent owner turn content=do
      bytes<-case content of
        ProviderMessage _ text->pure (BS.length (TE.encodeUtf8 text))
        ProviderTool value->pure (fromIntegral (BL.length (encode value)))
        ProviderPlan value->pure (fromIntegral (BL.length (encode value)))
        ProviderTurnBoundary _->pure 64
      publishBounded state bytes (ProviderContentEvent owner turn content)

-- Called by the adapter worker before mailbox publication, including typed
-- callers. Payload traversal/UTF-8 sizing never migrates into the UI owner.
validateProviderCall :: ProviderCall a -> Either Text ()
validateProviderCall operation=case operation of
  AskProviderPermission request
    | T.length (permissionTitle request)>4096 || BS.length (TE.encodeUtf8 (permissionDetails request))>16*1024*1024->Left "Permission details exceed their bounds."
    | null choices || length choices>32 || M.size (M.fromList [(key,()) | (key,_,_)<-choices])/=length choices ||
      any (\(key,label,kind)->T.null key || T.length key>4096 || T.length label>4096 || kind `notElem` ["allow_once","allow_always","reject_once","reject_always"]) choices->Left "Invalid permission choices."
    | otherwise->Right ()
    where choices=permissionOptions request
  ReadProviderFile path line limit | validPath path && line>=1 && maybe True (>=0) limit->Right ()
                                  | otherwise->Left "Invalid file path or line range."
  WriteProviderFile path text | not (validPath path) || T.any (=='\0') text || BS.length (TE.encodeUtf8 text)>16*1024*1024->Left "File content must be bounded UTF-8 text without NUL."
                             | otherwise->Right ()
  CreateProviderTerminal _->Right () -- Checked/canonicalized on the launch worker before approval.
  ReadProviderTerminal tid->terminalId tid
  WaitProviderTerminal tid->terminalId tid
  KillProviderTerminal tid->terminalId tid
  ReleaseProviderTerminal tid->terminalId tid
  where
    validPath path=not (null path) && length path<=32768 && '\0' `notElem` path
    terminalId tid=if T.null tid || T.length tid>4096 || T.any (<' ') tid then Left "Invalid terminal ID." else Right ()

-- | Worker publication stays bounded by the original transport ingress budget.
-- Closing/overrun rejects the adapter rather than creating an unbounded host log.
publishProviderEvent :: AgentRuntime -> ProviderIdentity -> DriverEvent -> IO ()
publishProviderEvent runtime identity event=do
  bytes<-case event of
    ProviderUpdate _ value->pure (fromIntegral (BL.length (encode value)))
    ProviderCapabilities caps->evaluate (length (show caps))
    _->pure 64
  publishBounded (runtimeState runtime) bytes (ProviderEvent identity event)

publishBounded :: MVar RuntimeState -> Int -> AgentRequest -> IO ()
publishBounded state bytes event=modifyMVar_ state $ \s->
  if closed s then pure s else if bytes>16*1024*1024 || providerEventBytes s+bytes>32*1024*1024 || length (requests s)>=4096
    then ioError (userError "Provider event ingress exceeded its bound.")
    else pure s {requests=requests s++[event],providerEventBytes=providerEventBytes s+bytes}

-- | Synchronous monotone retirement; cleanup/joins belong to preparation owners.
retireProviderCalls :: AgentRuntime -> ProviderIdentity -> IO ()
retireProviderCalls runtime identity=modifyMVar_ (runtimeState runtime) $ \s->do
  forM_ (providerReplies s) $ \(owner,_,retire)->when (owner==identity) retire
  pure s {providerReplies=filter (\(owner,_,_)->owner/=identity) (providerReplies s)
    ,requests=filter keep (requests s)}
  where
    keep (NativeProviderRequest owner _ _ _)=owner/=identity
    keep _=True

-- | Drain unresolved mailbox requests and at most one host launch completion.
-- Polling never waits for provider startup. Draining a launch releases its single
-- pending slot and publishes the original Hub identity/ticket exactly once.
drainAgentRequests :: AgentRuntime -> IO [AgentRequest]
drainAgentRequests runtime = do
  created<-modifyMVar (creationWorker runtime) $ \worker->case worker of
    Nothing->pure (Nothing,[])
    Just running->poll running >>= \result->case result of
      Nothing->pure (worker,[])
      Just completed->pure (Nothing,[AgentCreated (either (const (Left "Agent creation interrupted.")) id completed)])
  modifyMVar (runtimeState runtime) $ \s -> do
    ready <- filterM unresolved (requests s)
    pure (s {requests=[],providerEventBytes=0},if closed s then [] else ready++created)
  where
    unresolved (DeliverPrimary delivery) = deliveryWaiting delivery
    unresolved (ControlPrimary control) = primaryControlWaiting control
    unresolved (ProviderPermission _ _ cell) = isEmptyMVar cell
    unresolved (NativeProviderRequest _ _ _ cell)=isEmptyMVar cell
    unresolved _ = pure True

-- | Publish primary capabilities and exact connection identity. A replacement
-- client invalidates captured controls even if it reuses the same session key;
-- capability refresh retains the existing provider lifetime and delivery receipt.
syncPrimary :: AgentRuntime -> FilePath -> Maybe ProviderIdentity -> Text -> Capabilities -> Bool -> IO (Either Text ())
syncPrimary runtime directory client key caps busy = do
  let connection=client
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
          launchIdentity<-makeStableName =<< evaluate launch
          modifyMVar_ (runtimeState runtime) $ \state->do
            -- A human query may own the original disconnected -> client ->
            -- session startup. Advance only missing pieces of that binding;
            -- an acquired client/session never rebinds to a replacement. All
            -- explicit reset/cancel paths reject its reply before this sync.
            let retained=primaryControl state >>= bindPrimaryQuery launchIdentity connection (if T.null key then Nothing else Just key)
            retired<-retireDeliveries retained "Primary provider changed." state
            pure retired {primaryState=Nothing,primaryEvents=Nothing}
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
recordPrimaryEvent :: AgentRuntime -> ProviderIdentity -> Text -> DriverEvent -> IO ()
recordPrimaryEvent runtime client key event=do
  let owner=client
  publish<-withMVar (runtimeState runtime) $ \state->pure $ case primaryState state of
    Just (_,Just current,sid,_) | not (closed state),owner==current,key==sid->primaryEvents state
    _->Nothing
  mapM_ ($ event) publish

-- | Claim one unresolved Hub delivery for the exact current provider object.
-- Claiming twice cannot fail an already running prompt. No new prompt queue is
-- created: the Hub worker still waits for this receipt's terminal result.
admitPrimaryDelivery :: AgentRuntime -> PrimaryDelivery -> ProviderIdentity -> Text -> IO (Maybe HubMessage)
admitPrimaryDelivery runtime delivery client key=do
  let owner=client
  current<-statusAgent (agentHub runtime) Human (primaryAgent runtime)
  let running=case current of Right value->field "status" value==Just ("running"::Text); _->False
  modifyMVar (runtimeState runtime) $ \state->do
    waiting<-deliveryWaiting delivery
    let directory=deliveryDirectory delivery; expected=deliveryProvider delivery; sid=deliverySession delivery; message=deliveryMessage delivery
    if delivery `notElem` deliveries state || not waiting || admittedPrimary state/=Nothing
      then pure (state,Nothing)
      else if closed state || not running || expected/=Just owner || sid/=key || not (driverBinding state directory expected sid)
        then settleDelivery (Left "Primary delivery expired.") delivery >> pure (state,Nothing)
        else pure (state {admittedPrimary=Just delivery},Just message)

-- | Refuse only an unadmitted delivery. A duplicate/late view callback cannot
-- reject an admitted provider prompt or overwrite its first terminal result.
-- Receipts from another runtime are refused without touching their owner.
rejectPrimaryDelivery :: AgentRuntime -> PrimaryDelivery -> Text -> IO ()
rejectPrimaryDelivery runtime delivery reason=withMVar (runtimeState runtime) $ \state->
  when (delivery `elem` deliveries state && admittedPrimary state/=Just delivery)
    (void (settleDelivery (Left reason) delivery))

-- | Complete the exact current binding with an already-redacted provider result.
-- Publish the host's next-turn busy state before releasing the Hub worker; stale
-- client objects cannot change that handoff even if their session key repeats.
-- The caller owns the provider response dispatcher: it must invoke completion
-- only for its current terminal prompt/preparation response, never a late response
-- from a previous prompt on this same client. A cancel request leaves the receipt
-- pending until that real provider outcome.
-- Returns whether this call settled an admitted delivery. Calls without one may
-- still update the matching external human turn's existing busy advertisement.
completePrimaryDelivery :: AgentRuntime -> Maybe ProviderIdentity -> Maybe Text -> Bool -> Either Text Value -> IO Bool
completePrimaryDelivery runtime client session busy result=case (client,session) of
  (Just current,Just key)->do
    let owner=current
    modifyMVar (runtimeState runtime) $ \state->
      if closed state || not (primaryBinding state owner key) then pure (state,False) else do
        -- This Hub operation is STM-only and cannot re-enter provider callbacks.
        setExternalAgentBusy (agentHub runtime) (primaryAgent runtime) busy
        case admittedPrimary state of
          Just delivery | Just owner==deliveryProvider delivery && key==deliverySession delivery->do
            settled<-settleDelivery result delivery
            pure (state {admittedPrimary=Nothing},settled)
          _->pure (state,False)
  _->pure False

-- | O(1). An admitted primary provider prompt, independent of window selection.
primaryDeliveryActive :: AgentRuntime -> IO Bool
primaryDeliveryActive runtime=maybe False (const True) . admittedPrimary <$> readMVar (runtimeState runtime)

primaryBinding :: RuntimeState -> ProviderIdentity -> Text -> Bool
primaryBinding state owner key=case primaryState state of
  Just (_,Just expected,sid,_)->owner==expected && key==sid
  _->False

-- A Hub worker may retain an old driver across replacement before its IO begins.
-- Preserve its issuance binding through admission instead of adopting a new client.
driverBinding :: RuntimeState -> FilePath -> Maybe (ProviderIdentity) -> Text -> Bool
driverBinding state directory owner key=case primaryState state of
  Just (current,expected,sid,_)->directory==current && owner==expected && key==sid
  _->False

deliveryWaiting :: PrimaryDelivery -> IO Bool
deliveryWaiting delivery=isEmptyMVar (deliveryResult delivery)
settleDelivery :: Either Text Value -> PrimaryDelivery -> IO Bool
settleDelivery result delivery=do
  retireProviderSubmission (deliverySubmission delivery)
  void (tryPutMVar (deliveryAdmission delivery) (either Left (const (Right ())) result))
  tryPutMVar (deliveryResult delivery) result

-- | Acknowledge only the original admitted delivery after its provider send committed.
acknowledgePrimaryDelivery :: AgentRuntime -> IO ()
acknowledgePrimaryDelivery runtime=withMVar (runtimeState runtime) $ \state->
  forM_ (admittedPrimary state) (\delivery->void (tryPutMVar (deliveryAdmission delivery) (Right ())))

failPendingPrimary :: AgentRuntime -> Text -> IO ()
failPendingPrimary runtime = failDeliveries (runtimeState runtime)

agentSession :: AgentRuntime -> AgentId -> IO (Maybe SessionRecord)
agentSession runtime ident = M.lookup ident . sessions <$> readMVar (runtimeState runtime)

-- | Admit one human host launch without waiting for provider startup. The caller
-- must first validate its human submission and captured workspace. Agent tools
-- use the Hub's authenticated actor path instead; this operation grants no tool
-- authority. The Hub still validates limits and queues the initial task once.
--
-- Until 'AgentCreated' is drained, another request is refused. Closing the
-- runtime refuses new requests and joins pending acquisition before checkpointing.
requestAgentCreation :: AgentRuntime -> SpawnSpec -> IO (Either Text ())
requestAgentCreation runtime spec=mask $ \restore->modifyMVar (creationWorker runtime) $ \worker->do
  stopped<-closed <$> readMVar (runtimeState runtime)
  if stopped then pure (worker,Left "Editor closed.") else case worker of
    Just _->pure (worker,Left "An agent is already starting.")
    Nothing->do
      started<-async (restore (spawnAgentWithTask (agentHub runtime) Human spec))
      pure (Just started,Right ())

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

primaryDriver :: MVar RuntimeState -> AgentAccess -> IO (Maybe AgentId) -> Maybe (ProviderIdentity) -> FilePath -> Text -> Capabilities -> AgentDriver
primaryDriver state access identity connection directory key caps = AgentDriver
  { driverDirectory=directory,driverSessionKey=key,driverCapabilities=caps
  , driverSteer= \message _extra submission->case connection of
      Nothing->pure (Left "Primary provider is disconnected.")
      Just owner->do
        reply<-newEmptyMVar
        requestPrimaryControl state (SteerPrimary owner key message submission reply) reply
  , driverConfigure= \settings->case connection of
      Nothing->pure (Left "Primary provider is disconnected.")
      Just owner->do
        reply<-newEmptyMVar
        requestPrimaryControl state (ConfigurePrimary owner key settings reply) reply
  , driverDeliver= \turn message _extra submission -> mask $ \restore -> do
      admitted<-newEmptyMVar
      cell<-newEmptyMVar
      let delivery=PrimaryDelivery directory connection key message turn submission admitted cell
          retire=do
            void (settleDelivery (Left "Primary delivery interrupted.") delivery)
            modifyMVar_ state (\s->pure s
              {deliveries=filter (/=delivery) (deliveries s),requests=filter (not . matchingDelivery delivery) (requests s)
              ,admittedPrimary=if admittedPrimary s==Just delivery then Nothing else admittedPrimary s})
      modifyMVar_ state $ \s -> if closed s
        then settleDelivery (Left "Editor closed.") delivery >> pure s
        else pure s {requests=requests s++[DeliverPrimary delivery],deliveries=delivery:deliveries s}
      admission<-restore (readMVar admitted) `onException` retire
      case admission of
        Left err->retire >> pure (Left err)
        Right ()->pure (Right (ProviderTurn turn
          (ProviderReply (tryReadMVar cell) (restore (readMVar cell) `finally` retire) retire)
          (modifyMVar_ state $ \current->pure current
            {requests=requests current++[CancelPrimary | admittedPrimary current==Just delivery && not (closed current)]})))
  , driverCancel=modifyMVar_ state $ \s -> do
      let current=driverBinding s directory connection key
          belongs delivery=deliveryDirectory delivery==directory && deliveryProvider delivery==connection && deliverySession delivery==key
      when current (mapM_ (rejectPrimaryControl "Agent control cancelled.") (primaryControl s))
      -- Draining alone is not admission. Only a claimed provider prompt retains
      -- the Hub reservation until its owner dispatches the terminal response.
      forM_ (filter belongs (deliveries s)) $ \delivery->unless
        (admittedPrimary s==Just delivery) (void (settleDelivery (Left "Cancelled.") delivery))
      -- Replacement retires old queued cancels under this same state lock and
      -- invalidates the binding before replacing the Hub driver. A callback
      -- captured by Hub before replacement cannot cancel the new UI client.
      let keep (DeliverPrimary delivery)=not (belongs delivery)
          keep _=True
      pure s {requests=filter keep (requests s)++[CancelPrimary | current && not (closed s)]}
  , driverStop=do
      identity >>= mapM_ (revokeAgentAccess access)
      failDeliveries state "Primary agent ended."
      enqueue state EndPrimary }

-- | Submit one already captured human query to the existing control mailbox.
-- The caller owns its declaration worker and original input lifetime. A query
-- acknowledges queue admission or prompt submission, never the terminal answer.
-- Initial missing client/session identities bind once; later replacements fail.
-- Connected submissions keep their captured configuration receipt. Only original
-- disconnected startup omits it, because acquiring that provider changes the Hub
-- incarnation itself. The submission identity retains no draft source payload.
requestPrimaryQuery :: AgentRuntime -> SubmissionIdentity -> StableName ProviderLaunch -> Maybe (ProviderIdentity,Text) -> Maybe AgentConfigRef -> Text -> IO (Either Text ())
requestPrimaryQuery runtime submitted launch provider config text=do
  reply<-newEmptyMVar
  requestPrimaryControl (runtimeState runtime)
    (QueryPrimary submitted launch (fst <$> provider) (snd <$> provider) config text reply) reply

-- Missing identity pieces describe only the original startup. Binding is
-- monotone: once present, a client/session can only compare equal, never change.
bindPrimaryQuery :: StableName ProviderLaunch -> Maybe (ProviderIdentity) -> Maybe Text -> PrimaryControl -> Maybe PrimaryControl
bindPrimaryQuery launch client session (QueryPrimary submitted expected original key config text reply)
  | launch==expected,maybe True (\value->Just value==client) original,maybe True (\value->Just value==session) key=
      Just (QueryPrimary submitted expected client session config text reply)
bindPrimaryQuery _ _ _ _=Nothing

-- | Admission checks the live provider object, not its reusable session key.
-- Cancellation before this check refuses the request without protocol IO.
-- Query's canonical binding lives in the existing control slot; retained copies
-- share only reply identity and cannot restore an earlier unbound startup phase.
primaryControlCurrent :: AgentRuntime -> PrimaryControl -> Maybe ProviderIdentity -> Maybe Text -> IO Bool
primaryControlCurrent runtime control client session=case control of
  QueryPrimary{}->do
    launch<-makeStableName =<< (configuredLaunch runtime >>= evaluate)
    let owner=client
    modifyMVar (runtimeState runtime) $ \state->do
      waiting<-primaryControlWaiting control
      case primaryControl state of
        Just current@(QueryPrimary _ _ _ _ config _ _) | current==control,not (closed state),waiting,
          Just bound<-bindPrimaryQuery launch owner session current->do
            configured<-maybe (pure True) (agentConfigurationCurrent (agentHub runtime)) config
            pure (if configured then state {primaryControl=Just bound} else state,configured)
        _->pure (state,False)
  _->do
    reserved<-agentControlPending (agentHub runtime) (primaryAgent runtime)
    waiting<-primaryControlWaiting control
    case (client,session) of
      (Just current,Just key) | reserved && waiting->do
        let owner=current
        pure $ case control of
          ConfigurePrimary expected sid _ _->owner==expected && key==sid
          SteerPrimary expected sid _ _ _->owner==expected && key==sid
      _->pure False

primaryControlWaiting :: PrimaryControl -> IO Bool
primaryControlWaiting (ConfigurePrimary _ _ _ reply)=isEmptyMVar reply
primaryControlWaiting (SteerPrimary _ _ _ _ reply)=isEmptyMVar reply
primaryControlWaiting (QueryPrimary _ _ _ _ _ _ reply)=isEmptyMVar reply

-- | Terminally refuse one retained control. Repeated retirement preserves its
-- first result and cannot turn a cancelled reply into success.
rejectPrimaryControl :: Text -> PrimaryControl -> IO ()
rejectPrimaryControl reason (ConfigurePrimary _ _ _ reply)=void (tryPutMVar reply (Left reason))
rejectPrimaryControl reason (SteerPrimary _ _ _ submission reply)=retireProviderSubmission submission >> void (tryPutMVar reply (Left reason))
rejectPrimaryControl reason (QueryPrimary _ _ _ _ _ _ reply)=void (tryPutMVar reply (Left reason))

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

matchingDelivery :: PrimaryDelivery -> AgentRequest -> Bool
matchingDelivery delivery (DeliverPrimary other) = delivery==other
matchingDelivery _ _ = False

failDeliveries :: MVar RuntimeState -> Text -> IO ()
failDeliveries state reason=modifyMVar_ state (retireDeliveries Nothing reason)

retireDeliveries :: Maybe PrimaryControl -> Text -> RuntimeState -> IO RuntimeState
retireDeliveries retained reason s=do
  forM_ (primaryControl s) $ \control->unless (Just control==retained) (rejectPrimaryControl reason control)
  mapM_ (settleDelivery (Left reason)) (deliveries s)
  pure s {requests=filter keep (requests s),admittedPrimary=Nothing,primaryControl=case retained of Just control->Just control; Nothing->primaryControl s}
  where keep DeliverPrimary{}=False
        keep CancelPrimary=False
        keep _=True

enqueue :: MVar RuntimeState -> AgentRequest -> IO ()
enqueue state request = modifyMVar_ state $ \s -> pure s
  {requests=if closed s then requests s else requests s++[request]}

requestPermission :: MVar RuntimeState -> AgentId -> ProviderPermission -> IO (ProviderReply (Maybe Text))
requestPermission state ident permission=mask_ $ do
  cell<-newEmptyMVar
  modifyMVar_ state $ \s->if closed s then putMVar cell (Left "Editor closed.") >> pure s else pure s
    {requests=requests s++[ProviderPermission ident permission cell],permissions=M.insertWith (++) ident [cell] (permissions s)}
  let retire=do
        void (tryPutMVar cell (Right Nothing))
        modifyMVar_ state (\s->pure s
          {permissions=M.update (nonempty . filter (/=cell)) ident (permissions s)
          ,requests=filter (\request->case request of ProviderPermission _ _ other->cell/=other; _->True) (requests s)})
  pure (ProviderReply (tryReadMVar cell) (readMVar cell) retire)
  where nonempty []=Nothing
        nonempty xs=Just xs

cancelPermissions :: MVar RuntimeState -> AgentId -> IO ()
cancelPermissions state ident = modifyMVar_ state $ \s -> do
  forM_ (M.findWithDefault [] ident (permissions s)) (\cell -> void (tryPutMVar cell (Right Nothing)))
  pure s {permissions=M.delete ident (permissions s),requests=filter keep (requests s)}
  where keep (ProviderPermission other _ _) = other/=ident
        keep _ = True

startChild :: Maybe StartAgentProvider -> (FilePath -> IO SessionRecord) -> (SessionRecord -> IO ()) -> IO ProviderLaunch -> Maybe SessionRecord -> AgentAccess -> MVar RuntimeState -> StartProvider
startChild providerFactory openEditor restoreEditor getLaunch root access state request emit = mask $ \restore -> do
  let ident = startAgent request
      spec = startSpec request
      retire = revokeAgentAccess access ident >> cancelPermissions state ident
  result <- case providerFactory of
    Nothing->pure (Left "Agent provider plugin is unavailable.")
    Just _->restore (prepare spec) `onException` retire
  case result of
    Left err -> retire >> pure (Left err)
    Right (record,launch,context) -> do
      token <- grantAgentAccess access ident
      let run = do
            ownServers <- editorEndpointsAt (sessionId record) Nothing
            orchestration <- maybe (pure []) (\r -> editorEndpointsAt (sessionId r) (Just token)) root
            let servers = ownServers++map renameServer orchestration
                configured = launch {environment=("THC_EDIT_SESSION",sessionId record):filter ((/="THC_EDIT_SESSION").fst) (environment launch)}
                started = request {startSpec=spec {spawnDirectory=sessionDirectory record}}
                event value = do
                  case value of ProviderClosed -> retire; _ -> pure ()
                  emit value
            identity<-newProviderIdentity
            case providerFactory of
              Nothing->pure (Left "Agent provider plugin is unavailable.")
              Just acquire->acquire ChildProvider identity configured servers context
                (ProviderHost (requestPermission state ident) Nothing Nothing Nothing) started event
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
    renameServer endpoint=endpoint {endpointName="agents"}

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
  , savedLaunches :: M.Map AgentId ProviderLaunch }

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
    pure (ident,ProviderLaunch executable arguments environment)) providerValues
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
                ,"providers" .= [object ["agent" .= agentIdText ident,"executable" .= executable launch
                  ,"arguments" .= arguments launch,"environment" .= filter ((/="THC_EDIT_MCP_TOKEN").fst) (environment launch)]
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
