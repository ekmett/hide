{-# LANGUAGE OverloadedStrings #-}
module AgentHubCheck (checks) where
import Control.Concurrent
import Control.Concurrent.Async (withAsync, cancel, wait, waitCatch)
import Control.Concurrent.STM
import Control.Exception (bracket, finally, SomeAsyncException, fromException)
import Control.Monad (unless,void,forM_)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Text as T
import System.Timeout (timeout)
import System.Directory (getTemporaryDirectory, canonicalizePath)
import System.FilePath ((</>))
import Hide.AgentHub

checks :: IO ()
checks=do
  directory <- getTemporaryDirectory >>= canonicalizePath
  deliveries<-newTVarIO []
  stops<-newTVarIO (0::Int)
  gate<-newTVarIO True
  let caps=Capabilities True True False [ConfigChoice "model-id" "model" "small" [("small","Small"),("large","Large")],ConfigChoice "effort-id" "thought_level" "low" [("low","Low"),("high","High")]]
      driver ident=AgentDriver directory ("private-provider-key-"<>agentIdText ident) caps
        (\_->pure (Right caps))
        (\message->do atomically (modifyTVar' deliveries (++[(ident,message)])); atomically (readTVar gate >>= check); pure (Right (String (messageText message))))
        (atomically (writeTVar gate True))
        (atomically (modifyTVar' stops (+1) >> writeTVar gate True))
        (\_ ->pure (Left "Unsupported steering"))
      launch request emit=emit (ProviderUpdate "provider" (object ["connected" .= True])) >> pure (Right (driver (startAgent request)))
      spec name=SpawnSpec name "Test task" directory Shared Fresh Nothing Nothing
      ensure label value=unless value (error label)
      right=either (error.T.unpack) pure
      isLeft (Left _)=True; isLeft _=False
      field key value=parseMaybe (withObject "field" (.:key)) value
  bracket (newAgentHub (HubLimits 4 2) launch) closeAgentHub $ \hub->do
    initial <- listAgents hub Human >>= right
    ensure "empty directory reports its configured limits"
      (field "totalActiveLimit" initial == Just (4::Int) && field "directSubagentLimit" initial == Just (2::Int))
    root<-registerAgent hub "Main" directory (driver (AgentId "primary")) >>= right
    first<-spawnAgent hub (Agent root) (spec "Parser") >>= right
    second<-spawnAgent hub (Agent root) (spec "Renderer") >>= right
    denied<-spawnAgent hub (Agent root) (spec "One too many")
    ensure "direct child capacity is atomic" (isLeft denied)
    grandchild<-spawnAgent hub (Agent first) (spec "Lexer") >>= right
    deniedTotal<-spawnAgent hub Human (spec "No capacity")
    ensure "primary counts toward total" (isLeft deniedTotal)
    deniedParent<-endAgent hub (Agent grandchild) root
    ensure "child cannot end parent" (isLeft deniedParent)
    renamed<-renameAgent hub (Agent second) first "Parse source"
    ensure "agents can ascribe names to others" (renamed==Right ())
    duplicate<-renameAgent hub Human first " renderer "
    ensure "names are unique ignoring case and outer spaces" (isLeft duplicate)
    badName<-renameAgent hub Human first "bad\nname"
    ensure "names reject control characters" (isLeft badName)
    transcript<-historyAgent hub Human first 0 100 >>= right
    ensure "renames retain actor audit" ("agent-3" `BS.isInfixOf` BL.toStrict (encode transcript) && "previousName" `BS.isInfixOf` BL.toStrict (encode transcript))
    atomically (writeTVar gate False)
    ticket<-sendAgent hub (Agent first) root "Message to the human seat" >>= right
    pending<-waitAgent hub Human root ticket 1 >>= right
    ensure "bounded waits report running" (field "status" pending==Just ("running"::T.Text))
    atomically (writeTVar gate True)
    completed<-waitAgent hub Human root ticket 1000 >>= right
    ensure "prompt completion is waitable" (field "status" completed==Just ("completed"::T.Text))
    sent<-readTVarIO deliveries
    ensure "agent to primary never masquerades as human user" (case [m | (_,m)<-sent,messageText m=="Message to the human seat"] of [m]->messageAuthor m==Agent first && not (messageIsUserSeat m); _->False)
    ownerTicket<-sendAgent hub (Agent root) first "Owner prompt" >>= right
    _<-waitAgent hub Human first ownerTicket 1000 >>= right
    sentAgain<-readTVarIO deliveries
    ensure "parent owns child user seat" (any (\(_,m)->messageText m=="Owner prompt" && messageIsUserSeat m) sentAgain)
    ended<-endAgent hub (Agent root) first
    ensure "ancestor can end child and descendant" (ended==Right ())
    grandStatus<-statusAgent hub Human grandchild >>= right
    ensure "ending subtree closes descendants" (field "status" grandStatus==Just ("ended"::T.Text))
    blockedActor<-sendAgent hub (Agent first) second "cannot act after ending"
    ensure "ended caller loses authority" (isLeft blockedActor)
    unknownModel<-spawnAgent hub Human ((spec "Bad model") {spawnModel=Just "invented"})
    ensure "only provider-advertised model choices are accepted" (isLeft unknownModel)
    chosen<-spawnAgent hub Human ((spec "Chosen") {spawnModel=Just "large",spawnEffort=Just "high"}) >>= right
    forked<-spawnAgent hub (Agent chosen) ((spec "Fork") {spawnContext=Fork chosen}) >>= right
    listing<-listAgents hub Human >>= right
    ensure "directory does not reveal private provider keys" (not ("private-provider-key" `BS.isInfixOf` BL.toStrict (encode listing)))
    snap<-snapshotHub hub
    restored<-restoreHub (HubLimits 4 2) (\_ _->error "recovery must never start a process") snap >>= right
    recovered<-statusAgent restored Human forked >>= right
    ensure "recovery is inert but can describe resume availability" (field "status" recovered==Just ("recovered"::T.Text) && field "reconnectable" recovered==Just True)
    endedRecovered<-statusAgent restored Human first >>= right
    ensure "explicitly ended agents remain ended on restore" (field "status" endedRecovered==Just ("ended"::T.Text) && field "reconnectable" endedRecovered==Just False)
    closeAgentHub restored
    reconnectLimits<-newIORef (HubLimits 2 0)
    resumed<-restoreHubWithLimits (const (Right <$> readIORef reconnectLimits)) launch snap >>= right
    deniedDirect<-reconnectAgent resumed forked
    ensure "reconnect honors current direct-child limit" (isLeft deniedDirect)
    _<-reconnectAgent resumed chosen >>= right
    deniedExternal<-reconnectAgent resumed root
    ensure "child reconnect cannot claim the external primary" (isLeft deniedExternal)
    writeIORef reconnectLimits (HubLimits 1 2)
    deniedCapacity<-reconnectAgent resumed forked
    ensure "reconnect honors current total active limit" (isLeft deniedCapacity)
    writeIORef reconnectLimits (HubLimits 2 2)
    beforeReconnect<-readTVarIO deliveries
    _<-reconnectAgent resumed forked >>= right
    ensure "reconnect does not replay saved task" . (==beforeReconnect) =<< readTVarIO deliveries
    duplicateReconnect<-reconnectAgent resumed forked
    ensure "active child cannot be loaded twice" (isLeft duplicateReconnect)
    closeAgentHub resumed
    reconnectStarted<-newEmptyMVar
    reconnectRelease<-newEmptyMVar
    let loading request _=putMVar reconnectStarted request >> readMVar reconnectRelease >> pure (Right (driver (startAgent request)))
    loadingHub<-restoreHub (HubLimits 1 2) loading snap >>= right
    withAsync (reconnectAgent loadingHub forked) $ \pendingReconnect->do
      request<-takeMVar reconnectStarted
      ensure "host reconnect supplies saved reference without fork source"
        (startResume request==Just ("private-provider-key-"<>agentIdText forked) && startSource request==Nothing)
      raced<-reconnectAgent loadingHub chosen
      ensure "pending reconnect atomically reserves capacity" (isLeft raced)
      _<-endAgent loadingHub Human forked >>= right
      putMVar reconnectRelease ()
      endedLoad<-wait pendingReconnect
      ensure "ending child during load prevents resurrection" (isLeft endedLoad)
    closeAgentHub loadingHub
    failedHub<-restoreHub (HubLimits 1 2) (\_ emit->emit ProviderClosed >> pure (Left "load failed")) snap >>= right
    loadFailed<-reconnectAgent failedHub forked
    failedStatus<-statusAgent failedHub Human forked >>= right
    ensure "provider failure while loading releases capacity and permits retry"
      (isLeft loadFailed && field "status" failedStatus==Just ("recovered"::T.Text) && field "reconnectable" failedStatus==Just True)
    closeAgentHub failedHub
    let wrongVersion=case snap of Object o->Object (KM.insert "schemaVersion" (Number 2) o); _->Null
    invalid<-restoreHub (HubLimits 4 2) launch wrongVersion
    ensure "unknown persistence schema rejected" (isLeft invalid)
    forM_ [1..1100::Int] (\n->recordAgentEvent hub second "stream" (toJSON n))
    bounded<-historyAgent hub Human second 0 100 >>= right
    ensure "bounded history reports dropped events" (maybe False (>0) (field "dropped" bounded::Maybe Int))
    searched<-searchAgentHistory hub Human second "1099" 0 100 >>= right
    ensure "literal history search" ("1099" `BS.isInfixOf` BL.toStrict (encode searched))
  -- Concurrent startup reservations must count before provider startup finishes.
  started<-newEmptyMVar
  release<-newEmptyMVar
  let slow request _=putMVar started () >> readMVar release >> pure (Right (driver (startAgent request)))
  bracket (newAgentHub (HubLimits 1 1) slow) closeAgentHub $ \hub->do
    result<-newEmptyMVar
    void (forkIO (spawnAgent hub Human (spec "Slow") >>= putMVar result))
    takeMVar started
    competing<-spawnAgent hub Human (spec "Concurrent")
    ensure "starting provider reserves capacity before IO" (isLeft competing)
    putMVar release ()
    _<-takeMVar result >>= right
    pure ()
  mutableLimits<-newIORef (HubLimits 2 2)
  bracket (newAgentHubWithLimits (const (Right <$> readIORef mutableLimits)) launch) closeAgentHub $ \hub->do
    _<-spawnAgent hub Human (spec "One") >>= right
    writeIORef mutableLimits (HubLimits 1 0)
    blocked<-spawnAgent hub Human (spec "Two")
    ensure "updated limits affect future spawn" (isLeft blocked)
  bracket (newAgentHub (HubLimits 3 2) launch) closeAgentHub $ \hub->do
    owner<-registerAgent hub "Unconnected" directory ((driver (AgentId "placeholder")) {driverSessionKey="",driverCapabilities=Capabilities False False False []}) >>= right
    placeholderSnap<-snapshotHub hub
    restoredPlaceholder<-restoreHub (HubLimits 3 2) launch placeholderSnap >>= right
    _<-updateExternalAgent restoredPlaceholder owner (driver owner) >>= right
    revived<-statusAgent restoredPlaceholder Human owner >>= right
    ensure "host can explicitly reconnect recovered primary with stable ID" (field "status" revived==Just ("idle"::T.Text) && field "name" revived==Just ("Unconnected"::T.Text))
    closeAgentHub restoredPlaceholder
    let actual=(driver owner) {driverDeliver= \message->pure (Right (object ["newDriver" .= True,"body" .= messageText message]))}
    _<-updateExternalAgent hub owner actual >>= right
    ticket<-sendAgent hub Human owner "After reconnect" >>= right
    done<-waitAgent hub Human owner ticket 1000 >>= right
    ensure "external reconnect retains ID and switches delivery callback" (case field "result" done >>= field "newDriver" of Just True->True; _->False)
    setExternalAgentBusy hub owner True
    queuedWhileBusy<-sendAgent hub Human owner "Wait for the human conversation" >>= right
    busyResult<-waitAgent hub Human owner queuedWhileBusy 30 >>= right
    ensure "external conversation retains the prompt seat while busy"
      (field "status" busyResult==Just ("running"::T.Text))
    setExternalAgentBusy hub owner False
    releasedResult<-waitAgent hub Human owner queuedWhileBusy 1000 >>= right
    ensure "queued message runs when external conversation releases the prompt seat"
      (field "status" releasedResult==Just ("completed"::T.Text))
    child<-spawnAgent hub (Agent owner) (spec "Cancelled") >>= right
    atomically (writeTVar gate False)
    firstTicket<-sendAgent hub (Agent owner) child "Blocked delivery" >>= right
    _<-waitAgent hub Human child firstTicket 1 >>= right
    secondTicket<-sendAgent hub (Agent owner) child "Queued delivery" >>= right
    _<-cancelAgent hub (Agent owner) child >>= right
    firstResult<-waitAgent hub Human child firstTicket 1000 >>= right
    secondResult<-waitAgent hub Human child secondTicket 1000 >>= right
    ensure "cancel resolves both active and queued tickets" (field "status" firstResult==Just ("cancelled"::T.Text) && field "status" secondResult==Just ("cancelled"::T.Text))
    seen<-readTVarIO deliveries
    ensure "cancelled queued prompt was not delivered" (not (any ((=="Queued delivery").messageText.snd) seen))
  -- A human prompt can start while the previous hub delivery is completing.
  -- Its independent busy state must survive completion and cancellation alike.
  forM_ [False,True] $ \cancelCurrent -> do
    deliveryStarted<-newEmptyMVar
    releaseDelivery<-newEmptyMVar
    delivered<-newTVarIO (0::Int)
    let externalDriver=(driver (AgentId "external-race"))
          {driverDeliver= \_->do
             atomically (modifyTVar' delivered (+1))
             void (tryPutMVar deliveryStarted ())
             readMVar releaseDelivery
             pure (Right Null)
          ,driverCancel=void (tryPutMVar releaseDelivery ())}
    bracket (newAgentHub (HubLimits 1 0) launch) closeAgentHub $ \hub->do
      ident<-registerAgent hub "Human prompt race" directory externalDriver >>= right
      current<-sendAgent hub Human ident "First delivery" >>= right
      takeMVar deliveryStarted
      setExternalAgentBusy hub ident True
      if cancelCurrent then void (cancelAgent hub Human ident >>= right) else putMVar releaseDelivery ()
      completed<-waitAgent hub Human ident current 1000 >>= right
      ensure "in-flight external delivery settles" (field "status" completed==Just (if cancelCurrent then "cancelled" else "completed"::T.Text))
      let awaitSettled=do
            currentState<-statusAgent hub Human ident >>= right
            if field "currentTicket" currentState==Just Null && field "status" currentState==Just ("running"::T.Text)
              then pure () else threadDelay 1000 >> awaitSettled
      settled<-timeout 1000000 awaitSettled
      ensure "settled external agent still reports its human prompt running" (settled==Just ())
      queued<-sendAgent hub Human ident "After human prompt began" >>= right
      pending<-waitAgent hub Human ident queued 30 >>= right
      ensure "human busy update during delivery survives finish and cancel" (field "status" pending==Just ("running"::T.Text))
      ensure "next external delivery cannot overlap the human prompt" . (==1) =<< readTVarIO delivered
      setExternalAgentBusy hub ident False
      released<-waitAgent hub Human ident queued 1000 >>= right
      ensure "external delivery resumes only after human busy clears" (field "status" released==Just ("completed"::T.Text))
  -- Provider cancellation is a barrier, including when no prompt was running.
  cancelStarted <- newEmptyMVar
  cancelRelease <- newEmptyMVar
  let cancellingDriver = (driver (AgentId "cancel-test"))
        { driverCancel = putMVar cancelStarted () >> readMVar cancelRelease }
  cancelBlocked <- bracket (newAgentHub (HubLimits 2 1) (\_ _ -> pure (Right cancellingDriver))) closeAgentHub $ \hub -> do
    ident <- spawnAgent hub Human (spec "Cancellation barrier") >>= right
    withAsync (cancelAgent hub Human ident) $ \operation -> do
      takeMVar cancelStarted
      incoming <- sendAgent hub Human ident "After cancel began"
        `finally` void (tryPutMVar cancelRelease ())
      _ <- wait operation >>= right
      pure (isLeft incoming)
  reconnectPath <- bracket (newAgentHub (HubLimits 2 1) launch) closeAgentHub $ \hub -> do
    ident <- registerAgent hub "Changing project" directory (driver (AgentId "primary")) >>= right
    _ <- updateExternalAgent hub ident ((driver ident) {driverDirectory=directory </> "different-project"}) >>= right
    current <- statusAgent hub Human ident >>= right
    pure (field "cwd" current == Just (T.pack (directory </> "different-project")))
  startupEntered <- newEmptyMVar
  startupHold <- newEmptyMVar
  let blockedLaunch _ _ = putMVar startupEntered () >> takeMVar startupHold
  interruptEscapes <- bracket (newAgentHub (HubLimits 2 1) blockedLaunch) closeAgentHub $ \hub ->
    withAsync (spawnAgent hub Human (spec "Interrupted startup")) $ \operation -> do
      takeMVar startupEntered
      cancel operation
      outcome <- waitCatch operation
      pure $ case outcome of
        Left err -> case fromException err :: Maybe SomeAsyncException of
          Just _ -> True
          Nothing -> False
        Right _ -> False
  let failures = [label | (label,passed) <-
        [("cancellation blocks new delivery until provider settles",cancelBlocked)
        ,("reconnect updates the authenticated workspace",reconnectPath)
        ,("async cancellation escapes provider error handling",interruptEscapes)], not passed]
  ensure (unlines failures) (null failures)
  ensure "fork is never inferred from absent marker" (not (supportsFork (parseCapabilities (object []) (object []))))
  let initialized=object ["agentCapabilities" .= object ["sessionCapabilities" .= object ["fork" .= object []]]]
      options=object ["configOptions" .= [object ["id" .= ("model"::T.Text),"type" .= ("select"::T.Text),"category" .= ("model"::T.Text),"currentValue" .= ("m"::T.Text),"options" .= [object ["name" .= ("Group"::T.Text),"options" .= [object ["value" .= ("m"::T.Text),"name" .= ("Model"::T.Text)]]]]]]]
  ensure "actual advertised nested choices and fork supported" (supportsFork (parseCapabilities initialized options) && length (configChoices (parseCapabilities initialized options))==1)
  finished<-timeout 1000000 (pure ())
  ensure "test completes" (finished==Just ())
  controlChecks directory
  putStrLn "agent hub checks passed"

controlChecks :: FilePath -> IO ()
controlChecks directory=do
  configGate<-newTVarIO False
  promptGate<-newTVarIO False
  configuring<-newEmptyMVar
  prompting<-newEmptyMVar
  callback<-newEmptyMVar
  steers<-newIORef []
  let initial=Capabilities False True True [ConfigChoice "model" "model" "a" [("a","A"),("b","B")]]
      updated=initial {configChoices=[ConfigChoice "model" "model" "b" [("a","A"),("b","B")]]}
      driver=AgentDriver directory "private-control-key" initial
        (\settings->if null settings then pure (Right initial) else do
          putMVar configuring (); atomically (readTVar configGate >>= check . not); pure (Right updated))
        (\_->do putMVar prompting (); atomically (readTVar promptGate >>= check . not); pure (Right Null))
        (atomically (writeTVar promptGate False)) (atomically (writeTVar promptGate False))
        (\message->modifyIORef' steers (++[message]) >> pure (Right (object ["outcome" .= ("injected"::T.Text)])))
      launch _ emit=putMVar callback emit >> pure (Right driver)
      right=either (error.T.unpack) pure
      ensure label condition=unless condition (error label)
      field key value=parseMaybe (withObject "field" (.:key)) value
      left (Left _)=True; left _=False
      waitSignal cell=timeout 2000000 (takeMVar cell) >>= maybe (error "Child control fixture timed out") pure
  bracket (newAgentHub (HubLimits 2 1) launch) closeAgentHub $ \hub->do
    parent<-registerAgent hub "Parent" directory driver >>= right
    child<-spawnAgent hub (Agent parent) (SpawnSpec "Child" "Test" directory Shared Fresh Nothing Nothing) >>= right
    emit<-takeMVar callback
    unknown<-configureAgent hub child "permission" "allow"
    ensure "host child configuration rejects authority/unadvertised choices" (left unknown)
    atomically (writeTVar configGate True)
    withAsync (configureAgent hub child "model" "b") $ \setting->do
      waitSignal configuring
      ticket<-sendAgent hub Human child "queued during configuration" >>= right
      blocked<-statusAgent hub Human child >>= right
      ensure "configuration reserves the child before queued delivery" (field "queued" blocked==Just (1::Int) && field "currentTicket" blocked==Just Null && field "status" blocked==Just ("configuring"::T.Text))
      atomically (writeTVar configGate False)
      ensure "configuration completes" . (==Right ()) =<< wait setting
      _<-waitAgent hub Human child ticket 2000 >>= right
      waitSignal prompting
    emit (ProviderUsage 42 100)
    emit (ProviderUsage (-1) 0)
    usage<-statusAgent hub Human child >>= right
    ensure "only valid live context usage is retained" ((field "contextUsage" usage >>= field "used")==Just (42::Integer))
    atomically (writeTVar promptGate True)
    ticket<-sendAgent hub (Agent parent) child "parent task" >>= right
    waitSignal prompting
    busy<-configureAgent hub child "model" "a"
    ensure "busy child rejects model changes" (left busy)
    _<-steerAgent hub child "human correction" >>= right
    observed<-readIORef steers
    ensure "human child steer preserves parent user-seat ownership" (case observed of [m]->messageAuthor m==Human && not (messageIsUserSeat m); _->False)
    atomically (writeTVar promptGate False)
    _<-waitAgent hub Human child ticket 2000 >>= right
    idleSteer<-steerAgent hub child "must remain a draft"
    ensure "idle steering never creates an unowned prompt" (left idleSteer)
    atomically (writeTVar configGate True)
    withAsync (configureAgent hub child "model" "a") $ \setting->do
      waitSignal configuring
      cancel setting
    recoveredSlot<-statusAgent hub Human child >>= right
    ensure "asynchronous cancellation releases the configuration reservation" (field "status" recoveredSlot==Just ("idle"::T.Text))
    withAsync (configureAgent hub child "model" "a") $ \setting->do
      waitSignal configuring
      _<-endAgent hub Human child >>= right
      atomically (writeTVar configGate False)
      outcome<-wait setting
      ensure "configuration completion cannot revive an ended driver" (left outcome)
    top<-spawnAgent hub Human (SpawnSpec "Human controlled" "Test" directory Shared Fresh Nothing Nothing) >>= right
    _<-takeMVar callback
    atomically (writeTVar promptGate True)
    topTicket<-sendAgent hub Human top "human task" >>= right
    waitSignal prompting
    _<-steerAgent hub top "human user-seat correction" >>= right
    allSteers<-readIORef steers
    ensure "top-level steering keeps the human user seat" (case reverse allSteers of m:_->messageAuthor m==Human && messageIsUserSeat m; _->False)
    atomically (writeTVar promptGate False)
    _<-waitAgent hub Human top topTicket 2000 >>= right
    emit (ProviderCapabilities initial)
    emit (ProviderUsage 99 100)
    ended<-statusAgent hub Human child >>= right
    ensure "late capabilities and usage from ended providers are ignored" (field "status" ended==Just ("ended"::T.Text) && field "contextUsage" ended==Just Null)
