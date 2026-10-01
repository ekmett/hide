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
import THC.Edit.AgentHub

checks :: IO ()
checks=do
  directory <- getTemporaryDirectory >>= canonicalizePath
  deliveries<-newTVarIO []
  stops<-newTVarIO (0::Int)
  gate<-newTVarIO True
  let caps=Capabilities True True [ConfigChoice "model-id" "model" "small" [("small","Small"),("large","Large")],ConfigChoice "effort-id" "thought_level" "low" [("low","Low"),("high","High")]]
      driver ident=AgentDriver directory ("private-provider-key-"<>agentIdText ident) caps
        (\_->pure (Right caps))
        (\message->do atomically (modifyTVar' deliveries (++[(ident,message)])); atomically (readTVar gate >>= check); pure (Right (String (messageText message))))
        (atomically (writeTVar gate True))
        (atomically (modifyTVar' stops (+1) >> writeTVar gate True))
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
    owner<-registerAgent hub "Unconnected" directory ((driver (AgentId "placeholder")) {driverSessionKey="",driverCapabilities=Capabilities False False []}) >>= right
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
  putStrLn "agent hub checks passed"
