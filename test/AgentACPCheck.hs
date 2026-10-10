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
import Control.Exception (bracket, finally)
import Control.Monad (unless, forM_, when, void)
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
import Hide.Plugin.Agent (ProviderKind(..), ProviderEndpoint(..), ProviderHost(..), ProviderPermission(..), ProviderContent(..))
import Hide.Plugin.Provider

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root -> do
  let script=root </> "provider.py"
      logPath=root </> "requests.jsonl"
      launch=A.ProviderLaunch "python3" [script] [("LOG",logPath)]
      spec=SpawnSpec "worker" "Inspect parser" root Shared Fresh Nothing Nothing
      request=StartRequest (AgentId "agent-1") Human spec Nothing Nothing
      bearer=T.replicate 12 "ab19"
      server=ProviderEndpoint "editor" (A.ProviderLaunch "/bridge" ["--mcp-editor","session"] [("THC_EDIT_MCP_TOKEN",T.unpack bearer)])
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
  permissionGate<-newIORef Nothing
  let permission value=do
        decision<-newEmptyMVar
        gate<-readIORef permissionGate
        putMVar asked (value,decision)
        forM_ gate (readMVar . fst)
        let retire=do
              void (tryPutMVar decision (Right Nothing))
              forM_ gate (\(_,retired)->void (tryPutMVar retired ()))
        pure (ProviderReply (tryReadMVar decision) (readMVar decision) retire)
      emit (ProviderUpdate kind value)=modifyIORef' events (++[(kind,value)])
      emit (ProviderUsage used size)=modifyIORef' events (++[("usage",object ["used" .= used,"size" .= size])])
      emit (ProviderCapabilities caps)=modifyIORef' events (++[("capabilities",String (T.pack (show caps)))])
      emit ProviderClosed=pure ()
  let launcher = startChild launch [] "" denyPermission
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
  driver<-startChild launch [server] "Initial context marker" permission request emit >>= right "start adapter"
  bracket (pure driver) driverStop $ \running -> do
    started<-logs
    check "startup never sends an automatic prompt" (all ((/=Just ("session/prompt"::T.Text)).field "method") started)
    check "private reference remains separate from public capabilities" (driverSessionKey running=="private-child-key" && not (null (configChoices (driverCapabilities running))))
    result<-driverConfigure running [("model-id","model-b"),("effort-id","high")]
    check "advertised model and effort selections apply" (case result of Right caps -> map configCurrent (configChoices caps)==["model-b","high"]; _ -> False)
    rejected<-driverConfigure running [("approval-mode","allow-all")]
    check "configuration never accepts unadvertised authority settings" (case rejected of Left _->True; _->False)
    tooLong<-deliver running (prompt (T.replicate 70000 "x"))
    check "child coordination keeps its bounded message size" (case tooLong of Left _->True; _->False)
    retired<-newProviderSubmission
    retireProviderSubmission retired
    unsentTurn<-newProviderTurnId
    unsent<-driverDeliver running unsentTurn (prompt "retired first prompt") [] retired
    check "retired first prompt never acquires the provider turn" (case unsent of Left _->True; _->False)
    completed<-deliver running (prompt "ordinary")
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
    split<-deliver running (prompt "split") >>= right "split output"
    splitEvents<-readIORef events
    let joined=T.concat [text | ("output",value)<-splitEvents,Just text<-[field "text" value]]
    check "streamed session reference stays private across chunk boundaries" (not ("private-child-key" `T.isInfixOf` joined) && field "text" split==Just ("Output [private] suffix"::T.Text))
    forM_ ["whole","split"] $ \mode->do
      writeIORef events []
      credentialResult<-deliver running (prompt ("credentials "<>mode)) >>= right "credential echo"
      echoed<-readIORef events
      let texts kind=T.concat [text | (label,value)<-echoed,label==kind,Just text<-[field "text" value]]
      check "MCP bearer is redacted from whole and split output and thought streams"
        (all (\kind->not (bearer `T.isInfixOf` texts kind) && "[private]" `T.isInfixOf` texts kind) ["output","thought"]
          && not (bearer `T.isInfixOf` T.pack (show credentialResult)))
      check "MCP bearer is redacted from echoed tool descriptors"
        (not (bearer `T.isInfixOf` T.pack (show echoed)) && any ((=="tool").fst) echoed)
    writeIORef events []
    _<-deliver running (prompt "configuration") >>= right "configuration update"
    configEvents<-readIORef events
    check "dynamic capability labels and values cannot expose private references or MCP credentials"
      (any ((=="capabilities").fst) configEvents && not (bearer `T.isInfixOf` T.pack (show configEvents)) && not ("private-child-key" `T.isInfixOf` T.pack (show configEvents)))
    unsupportedSteer<-steer running (prompt "not sent")
    check "unadvertised steering is rejected before RPC" (case unsupportedSteer of Left _->True; _->False)
    large<-deliver running (prompt "large") >>= right "large output"
    check "large provider output is bounded" (maybe False ((<=131072).T.length) (field "text" large) && field "truncated" large==Just True)
    entries<-logs
    let initial=[p | entry<-entries,field "method" entry==Just ("initialize"::T.Text),Just p<-[field "params" entry::Maybe Value]]
        prompts=[p | entry<-entries,field "method" entry==Just ("session/prompt"::T.Text),Just p<-[field "params" entry::Maybe Value]]
    check "native filesystem and terminal capabilities are not advertised" (case initial of p:_->(field "clientCapabilities" p >>= field "terminal")==Just False && (field "clientCapabilities" p >>= field "fs" >>= field "readTextFile")==Just False; _->False)
    check "prompt distinguishes sibling message and supplies naming/context instructions" (all (`T.isInfixOf` T.pack (show prompts)) ["sibling","not the human","Initial context marker","agent_rename"])
    withAsync (deliver running (prompt "permission")) $ \pending -> do
      (shown,decision)<-timeout 2000000 (takeMVar asked) >>= maybe (error "permission callback missing") pure
      check "provider permission is forwarded to human callback" (permissionTitle shown=="Write file")
      check "human sees bounded tool details without private reference"
        ("/proposed.txt" `T.isInfixOf` permissionDetails shown && not ("private-child-key" `T.isInfixOf` permissionDetails shown) && not (bearer `T.isInfixOf` permissionDetails shown) && T.length (permissionDetails shown)<=8192)
      check "provider cannot approve its own request" . maybe True (const False) =<< poll pending
      putMVar decision (Right (Just "allow-once"))
      check "approved callback releases the provider" . either (const False) (const True) =<< wait pending
    entriesAfter<-logs
    let native=[entry | entry<-entriesAfter,field "id" entry==Just ("native-read"::T.Text) || field "id" entry==Just ("native-terminal"::T.Text)]
    check "unsupported native requests get explicit errors" (length native==2 && all (maybe False (const True) . (field "error" :: Value -> Maybe Value)) native)
    withAsync (deliver running (prompt "permission")) $ \pending -> do
      (_,decision)<-timeout 2000000 (takeMVar asked) >>= maybe (error "permission callback missing") pure
      putMVar decision (Right (Just "made-up-approval"))
      _<-wait pending
      invalidLog<-logs
      let outcomes=[outcome | entry<-invalidLog,field "id" entry==Just ("permission"::T.Text),Just outcome<-[field "result" entry::Maybe Value]]
      check "unadvertised permission choice cancels request" (case reverse outcomes of outcome:_->(field "outcome" outcome >>= field "outcome")==Just ("cancelled"::T.Text); _->False)
    returned<-newEmptyMVar
    retiredPermission<-newEmptyMVar
    writeIORef permissionGate (Just (returned,retiredPermission))
    priorPermissionReplies<-length . filter ((==Just ("permission"::T.Text)).field "id") <$> logs
    withAsync (deliver running (prompt "permission")) $ \pending -> flip finally (void (tryPutMVar returned ())) $ do
      _<-timeout 2000000 (takeMVar asked) >>= maybe (error "permission callback missing") pure
      cancelled<-timeout 2000000 (driverCancel running) `finally` void (tryPutMVar returned ())
      check "cancel does not wait for a host callback to return" (cancelled==Just ())
      permissionRetired<-timeout 2000000 (readMVar retiredPermission)
      check "a permission receipt returned after cancellation is retired" (permissionRetired==Just ())
      ended<-timeout 4000000 (wait pending)
      check "cancel interrupts human approval wait without self-approval" (case ended of Just (Left _)->True; _->False)
      let originalReply=do
            permissionEntries<-drop priorPermissionReplies . filter ((==Just ("permission"::T.Text)).field "id") <$> logs
            case permissionEntries of
              permissionReply:_->pure permissionReply
              []->threadDelay 1000 >> originalReply
      permissionRejection<-timeout 2000000 originalReply
      check "late cancelled callback still answers its original RPC with failure"
        (case (permissionRejection >>= field "error")::Maybe Value of Just _->True; Nothing->False)
    check "stopped provider rejects new work" . maybe False (const True) =<< timeout 2000000 (driverStop running)
    stopped<-deliver running (prompt "ordinary")
    check "stopped provider does not hang on requests" (case stopped of Left _->True; _->False)
  -- Primary input retains the editor's domain; child coordination messages keep
  -- their smaller public bound. Use real admission/reply receipts here as well:
  -- cancelling an old completed turn must not touch the next active prompt.
  boundaryGate<-newIORef Nothing
  publicationFailure<-newIORef False
  let primaryEmit ProviderClosed=readIORef publicationFailure >>= \failed->when failed (ioError (userError "closed publication failed"))
      primaryEmit event=emit event
      content _ (ProviderTurnBoundary _)=do
        failed<-readIORef publicationFailure
        when failed (ioError (userError "content publication failed"))
        gate<-readIORef boundaryGate
        forM_ gate $ \(entered,released)->putMVar entered () >> readMVar released
      content _ _=pure ()
  identity<-newProviderIdentity
  primary<-startACPProvider PrimaryProvider identity
    launch {A.environment=("STEER","yes"):A.environment launch} [] ""
    (ProviderHost denyPermission Nothing Nothing (Just content)) request primaryEmit >>= right "primary provider"
  bracket (pure primary) driverStop $ \running->do
    earlier<-bracket newProviderSubmission retireProviderSubmission $ \submission->do
      turn<-newProviderTurnId
      driverDeliver running turn (HubMessage 2 Human (T.replicate 70000 "x") True) [] submission >>= right "large primary prompt admission"
    firstResult<-awaitProviderReply (providerTurnReply earlier) >>= right "large primary prompt completion"
    check "primary input exceeds the child message bound" (field "stopReason" firstResult==Just ("end_turn"::T.Text))
    active<-bracket newProviderSubmission retireProviderSubmission $ \submission->do
      turn<-newProviderTurnId
      admitted<-timeout 2000000 (driverDeliver running turn (HubMessage 3 Human "stall" True) [] submission)
      maybe (error "send admission waited for the reply") (right "pending prompt admission") admitted
    check "send admission does not imply a completed reply" . maybe True (const False) =<< pollProviderReply (providerTurnReply active)
    cancelProviderTurn earlier
    retiredSteer<-newProviderSubmission
    retireProviderSubmission retiredSteer
    refusedSteer<-driverSteer running (HubMessage 4 Human "not sent" True) [] retiredSteer
    check "retired steering is refused before send" (case refusedSteer of Left _->True; _->False)
    _<-steer running (HubMessage 5 Human "accepted" True) >>= right "next-turn steering after retired submission"
    finished<-timeout 2000000 (awaitProviderReply (providerTurnReply active)) >>= maybe (error "active turn lost after old cancellation") (right "active turn completion")
    check "old-turn cancellation and uncommitted steering leave the active prompt intact" (field "stopReason" finished==Just ("end_turn"::T.Text))
    entered<-newEmptyMVar
    released<-newEmptyMVar
    writeIORef boundaryGate (Just (entered,released))
    flip finally (void (tryPutMVar released ())) $ do
      finalizing<-bracket newProviderSubmission retireProviderSubmission $ \submission->do
        turn<-newProviderTurnId
        driverDeliver running turn (HubMessage 6 Human "ordinary" True) [] submission >>= right "finalizing prompt admission"
      _<-timeout 2000000 (readMVar entered) >>= maybe (error "terminal boundary callback missing") pure
      check "terminal reply waits for the content boundary" . maybe True (const False) =<< pollProviderReply (providerTurnReply finalizing)
      cancelled<-timeout 2000000 (cancelProviderTurn finalizing) `finally` void (tryPutMVar released ())
      check "exact-turn cancellation does not wait for terminal content adoption" (cancelled==Just ())
      ended<-timeout 2000000 (awaitProviderReply (providerTurnReply finalizing)) >>= maybe (error "terminal boundary never completed") (right "terminal boundary completion")
      check "terminal reply survives cancellation while content is being adopted" (field "stopReason" ended==Just ("end_turn"::T.Text))
    writeIORef boundaryGate Nothing
    writeIORef publicationFailure True
    broken<-bracket newProviderSubmission retireProviderSubmission $ \submission->do
      turn<-newProviderTurnId
      driverDeliver running turn (HubMessage 7 Human "ordinary" True) [] submission >>= right "publication failure prompt admission"
    failed<-timeout 2000000 (awaitProviderReply (providerTurnReply broken))
    check "failed content and closed-event publication still resolve the original turn"
      (case failed of Just (Left _)->True; _->False)
    writeIORef publicationFailure False
  -- Hold natural pump finalization where the host is publishing retirement.
  -- Explicit stop must complete that publication even if cancelling the pump
  -- interrupts its callback. Synchronize on the callback, not elapsed time.
  closingEntered<-newEmptyMVar
  closingRelease<-newEmptyMVar
  closedEvents<-newIORef (0::Int)
  let closingEvent ProviderClosed=do
        first<-tryPutMVar closingEntered ()
        when first (readMVar closingRelease)
        atomicModifyIORef' closedEvents (\n->(n+1,()))
      closingEvent _=pure ()
      brokenContent _ ProviderTurnBoundary{}=ioError (userError "held final publication")
      brokenContent _ _=pure ()
  closingIdentity<-newProviderIdentity
  closing<-startACPProvider PrimaryProvider closingIdentity launch [] ""
    (ProviderHost denyPermission Nothing Nothing (Just brokenContent)) request closingEvent >>= right "closing provider"
  flip finally (void (tryPutMVar closingRelease ()) >> driverStop closing) $ do
    pendingClose<-bracket newProviderSubmission retireProviderSubmission $ \submission->do
      turn<-newProviderTurnId
      driverDeliver closing turn (HubMessage 8 Human "ordinary" True) [] submission >>= right "closing prompt admission"
    _<-timeout 2000000 (readMVar closingEntered) >>= maybe (error "closing callback not reached") pure
    stopped<-timeout 2000000 (driverStop closing)
    check "stop joins interrupted retirement publication" (stopped==Just ())
    check "stop publishes retirement exactly once" . (==1) =<< readIORef closedEvents
    driverStop closing
    check "repeated stop does not repeat retirement" . (==1) =<< readIORef closedEvents
    settled<-pollProviderReply (providerTurnReply pendingClose)
    check "stop settles pending prompt" (case settled of Just (Left _)->True; _->False)
  let forked=request {startSpec=spec {spawnContext=Fork (AgentId "parent")},startSource=Just (PrivateSource (AgentId "parent") "private-parent-key")}
  beforeFork<-length <$> logs
  unsupported<-startChild launch [] "" denyPermission forked emit
  check "unadvertised fork fails without starting a fresh session" (case unsupported of Left _->True; _->False)
  forkLog<-drop beforeFork <$> logs
  check "unsupported fork never sends session/new" (all ((/=Just ("session/new"::T.Text)).field "method") forkLog)
  forkDriver<-startChild launch {A.environment=("FORK","yes"):A.environment launch} [] "" denyPermission forked emit >>= right "real fork"
  bracket (pure forkDriver) driverStop $ \running -> check "fork gets a distinct private provider session" (supportsFork (driverCapabilities running) && driverSessionKey running=="private-fork-key")
  reused<-startChild launch {A.environment=[("FORK","yes"),("REUSE","yes")]++A.environment launch} [] "" denyPermission forked emit
  check "fork may not silently reuse source session" (case reused of Left _->True; _->False)
  let resumed=request {startResume=Just "private-saved-key"}
  beforeUnsupported<-length <$> logs
  unavailable<-startChild launch [] "" denyPermission resumed emit
  check "unadvertised resume fails without new/fork/load" (case unavailable of Left _->True; _->False)
  unsupportedLog<-drop beforeUnsupported <$> logs
  check "unsupported resume performs only initialization" (map (field "method") unsupportedLog==[Just ("initialize"::T.Text)])
  forM_ ["load","resume"] $ \mode->do
    beforeLoad<-length <$> logs
    writeIORef events []
    loaded<-startChild launch {A.environment=("RESUME",mode):A.environment launch} [server] "Do not replay context" denyPermission resumed emit >>= right "load saved session"
    bracket (pure loaded) driverStop $ \running->do
      opened<-drop beforeLoad <$> logs
      let methods=map (field "method") opened
      check "resume uses exactly the advertised load operation, without prompts"
        (methods==[Just ("initialize"::T.Text),Just ("session/"<>T.pack mode)] && driverSessionKey running=="private-saved-key")
      check "provider history replay does not duplicate local history" . null =<< readIORef events
      _<-deliver running (prompt "after reconnect") >>= right "resumed prompt"
      delivered<-drop beforeLoad <$> logs
      check "first resumed prompt does not replay startup instructions or context"
        (not ("Do not replay context" `T.isInfixOf` T.pack (show delivered)) && not ("agent_rename" `T.isInfixOf` T.pack (show delivered)))
  changed<-startChild launch {A.environment=[("RESUME","load"),("CHANGED","yes")]++A.environment launch} [] "" denyPermission resumed emit
  check "load rejects a replacement session identity" (case changed of Left _->True; _->False)
  stubborn<-startChild launch {A.environment=("IGNORE_CANCEL","yes"):A.environment launch} [] "" denyPermission request emit >>= right "unresponsive provider"
  bracket (pure stubborn) driverStop $ \running -> bracket newProviderSubmission retireProviderSubmission $ \submission -> do
    -- Cancellation must follow this turn's send admission; shared logs can still
    -- contain an earlier provider's stalled prompt.
    turn<-newProviderTurnId
    pending<-timeout 2000000 (driverDeliver running turn (prompt "stall") [] submission)
      >>= maybe (error "unresponsive fixture prompt missing") (right "unresponsive prompt admission")
    driverCancel running
    stopped<-timeout 4000000 (awaitProviderReply (providerTurnReply pending))
    check "unresponsive cancellation closes driver within bound" (case stopped of Just (Left _)->True; _->False)
    closed<-deliver running (prompt "ordinary")
    check "unresponsive provider cannot receive subsequent prompts" (case closed of Left _->True; _->False)
  forM_ ["accepted","idle-race","legacy"] $ \mode->do
    writeFile logPath ""
    steering<-startChild launch {A.environment=("STEER","yes"):A.environment launch} [] "" denyPermission request emit >>= right "steering provider"
    bracket (pure steering) driverStop $ \running->withAsync (deliver running (prompt "stall")) $ \turn->do
      let awaitPrompt=do entries<-logs; if any ((==Just ("session/prompt"::T.Text)).field "method") entries then pure () else threadDelay 1000 >> awaitPrompt
      _<-timeout 2000000 awaitPrompt >>= maybe (error "steer prompt missing") pure
      result<-steer running (HubMessage 0 Human mode False)
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
        closed<-deliver running (prompt "ordinary")
        check "legacy detached-turn response retires the provider" (case closed of Left _->True; _->False)
  writeFile logPath ""
  interrupted<-startChild launch {A.environment=("STEER","yes"):A.environment launch} [] "" denyPermission request emit >>= right "cancelled steering provider"
  bracket (pure interrupted) driverStop $ \running->withAsync (deliver running (prompt "stall")) $ \turn->do
    let awaitMethod method=do entries<-logs; if any ((==Just (method::T.Text)).field "method") entries then pure () else threadDelay 1000 >> awaitMethod method
    _<-timeout 2000000 (awaitMethod "session/prompt") >>= maybe (error "cancel-steer prompt missing") pure
    withAsync (steer running (HubMessage 0 Human "withhold" False)) $ \pending->do
      _<-timeout 2000000 (awaitMethod "_session/steering") >>= maybe (error "cancel-steer request missing") pure
      cancel pending
    _<-timeout 4000000 (wait turn) >>= maybe (error "cancel-steer left provider alive") pure
    stopped<-deliver running (prompt "must not replay")
    check "cancelled unknown steering retires its provider" (case stopped of Left _->True; _->False)
  let startup=startChild launch {A.environment=("STARTUP_UPDATES","yes"):A.environment launch} [] "" denyPermission
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

-- The child has permission presentation but no native filesystem/terminal bridge.
startChild :: ProviderLaunch -> [ProviderEndpoint] -> T.Text
  -> (ProviderPermission -> IO (ProviderReply (Maybe T.Text))) -> StartProvider
startChild launch endpoints context permission request emit=do
  identity<-newProviderIdentity
  startACPProvider ChildProvider identity launch endpoints context
    (ProviderHost permission Nothing Nothing Nothing) request emit

denyPermission :: ProviderPermission -> IO (ProviderReply (Maybe T.Text))
denyPermission _=pure (ProviderReply (pure (Just (Right Nothing))) (pure (Right Nothing)) (pure ()))

-- Existing cases assert terminal outcomes; keep their submission lifetime until
-- that result. Tests below the driver also check prepared admission separately.
deliver :: AgentDriver -> HubMessage -> IO (Either T.Text Value)
deliver driver message=bracket newProviderSubmission retireProviderSubmission $ \submission->do
  turn<-newProviderTurnId
  admitted<-driverDeliver driver turn message [] submission
  either (pure . Left) (awaitProviderReply . providerTurnReply) admitted

steer :: AgentDriver -> HubMessage -> IO (Either T.Text Value)
steer driver message=bracket newProviderSubmission retireProviderSubmission $ \submission->
  driverSteer driver message [] submission

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
