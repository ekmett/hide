{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.WardenRuntime
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Trusted task provenance for the optional ACP Warden. The existing provider
-- owns delivery and cancellation; enforcing requests own their inference. A
-- single observation slot never delays actions or accumulates queued work.
-- This scope stores settings and attributed task revisions, never an action queue,
-- desktop, buffer or transcript. A judgment cannot outlive its task, provider,
-- settings or selected decision supplier. Checking these identities is cheap;
-- serialization, configuration reads and inference belong to request workers.
module Hide.WardenRuntime
  ( WardenRuntime, WardenBinding, WardenReceipt, withWarden
  , wardenProviderFactory, wardenAgent, wardenProvider, wardenAnonymous
  , runWarden, checkWarden, captureWardenBinding, wardenBindingCurrent, wardenEnforces
  , getWardenSettings, setWardenSettings, WardenSettingsRef, captureWardenSettings, chooseWardenMode, wardenReceiptResult
  ) where

import Control.Concurrent.STM
import Control.Concurrent.Async (Async,asyncWithUnmask,cancel,poll,race)
import Control.Concurrent.MVar (MVar,newMVar,tryTakeMVar,takeMVar,putMVar)
import Control.Exception (bracket,finally,mask,onException)
import Control.Monad (unless,void)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (Unique,newUnique,hashUnique)
import System.Environment (getEnvironment)
import Hide.GuestAccess (sensitiveLabel)
import Hide.Plugin.Agent
import Hide.Plugin.Provider
import Hide.Plugin.SystemOne
import Hide.Warden

data Entry = Entry
  { entryRevision :: !Unique, entryAdmitting :: !Bool, entryTurn :: !(Maybe ProviderTurnId)
  , entryTask :: !(Either Text Text), entryRules :: !(Either Text [Text])
  , entrySecrets :: ![Text], entryReport :: !(Value -> IO ()) }
data State = State
  { settings :: !WardenSettings, settingsRevision :: !Integer, closed :: !Bool
  , providers :: !(M.Map ProviderIdentity Entry), agents :: !(M.Map AgentId ProviderIdentity) }
data WardenRuntime = WardenRuntime !SystemOneServices
  !(FilePath -> IO (Either Text [Text])) !(TVar State) !(MVar (Maybe (Async ())))
data WardenSettingsRef = WardenSettingsRef !WardenRuntime !Integer
data Target = ByAgent !AgentId | ByProvider !ProviderIdentity | Anonymous
-- | A host-created caller binding. Tool JSON cannot supply task or rule authority.
data Snapshot = Snapshot !Integer !WardenSettings !Bool !(Maybe (ProviderIdentity,Entry))
data WardenBinding = WardenBinding !WardenRuntime !Target !(Maybe Snapshot)
-- | Exact judgment with small revocation keys. The owning request retains its
-- immutable arguments; this receipt does not retain their potentially large body.
data WardenReceipt = WardenReceipt !WardenRuntime !Integer
  !(Maybe (ProviderIdentity,Unique)) !WardenSettings !WardenResult

withWarden :: SystemOneServices -> WardenSettings
  -> (FilePath -> IO (Either Text [Text])) -> (WardenRuntime -> IO a) -> IO a
withWarden service config rules=bracket acquire release
  where
    acquire=case validateSettings config of
      Left err->ioError (userError (T.unpack err))
      Right ()->WardenRuntime service rules <$> newTVarIO (State config 0 False M.empty M.empty) <*> newMVar Nothing
    release (WardenRuntime _ _ cell observations)=do
      atomically $ modifyTVar' cell $ \s->
        s {closed=True,settingsRevision=settingsRevision s+1,providers=M.empty,agents=M.empty}
      worker<-takeMVar observations
      maybe (pure ()) cancel worker

getWardenSettings :: WardenRuntime -> IO WardenSettings
getWardenSettings (WardenRuntime _ _ cell _)=settings <$> readTVarIO cell

-- | Human form adoption only. Even selecting equal settings revokes old grants.
-- No model admission, filesystem write or thread join runs on the UI owner.
setWardenSettings :: WardenRuntime -> WardenSettings -> IO (Either Text ())
setWardenSettings (WardenRuntime _ _ cell _) config=case validateSettings config of
  Left err->pure (Left err)
  Right ()->atomically $ do
    s<-readTVar cell
    if closed s then pure (Left "Warden session is closed.") else do
      writeTVar cell s {settings=config,settingsRevision=settingsRevision s+1}
      pure (Right ())

-- | Capture only the scalar settings epoch for a submitted human form.
captureWardenSettings :: WardenRuntime -> IO (WardenSettingsRef,WardenSettings)
captureWardenSettings owner@(WardenRuntime _ _ cell _)=do
  s<-readTVarIO cell
  pure (WardenSettingsRef owner (settingsRevision s),settings s)

chooseWardenMode :: WardenSettingsRef -> WardenMode -> IO (Either Text ())
chooseWardenMode (WardenSettingsRef (WardenRuntime _ _ cell _) expected) mode=atomically $ do
  s<-readTVar cell
  if closed s || settingsRevision s/=expected then pure (Left "Warden settings changed; reopen the menu.") else do
    writeTVar cell s {settings=(settings s) {wardenMode=mode},settingsRevision=expected+1}
    pure (Right ())

validateSettings :: WardenSettings -> Either Text ()
validateSettings config=do
  unless (wardenBudgetMs config>=1 && wardenBudgetMs config<=30000) (Left "Warden budget must be 1..30000 ms.")
  let value=wardenThreshold config
  unless (not (isNaN value || isInfinite value) && value>=0 && value<=1) (Left "Warden threshold must be a finite probability.")

wardenAgent :: WardenRuntime -> AgentId -> WardenBinding
wardenAgent owner ident=WardenBinding owner (ByAgent ident) Nothing
wardenProvider :: WardenRuntime -> ProviderIdentity -> WardenBinding
wardenProvider owner ident=WardenBinding owner (ByProvider ident) Nothing
wardenAnonymous :: WardenRuntime -> WardenBinding
wardenAnonymous owner=WardenBinding owner Anonymous Nothing

resolve :: State -> Target -> Maybe (ProviderIdentity,Entry)
resolve s (ByAgent ident)=M.lookup ident (agents s) >>= resolve s . ByProvider
resolve s (ByProvider ident)=(ident,) <$> M.lookup ident (providers s)
resolve _ Anonymous=Nothing

-- | Freeze the task and settings at admission, before policy IO or queuing.
-- Capturing a frozen binding is idempotent: waiting cannot replace its task.
captureWardenBinding :: WardenBinding -> IO WardenBinding
captureWardenBinding binding@(WardenBinding owner target _)=do
  snap<-bindingSnapshot binding
  pure (WardenBinding owner target (Just snap))

bindingSnapshot :: WardenBinding -> IO Snapshot
bindingSnapshot (WardenBinding _ _ (Just snap))=pure snap
bindingSnapshot (WardenBinding (WardenRuntime _ _ cell _) target Nothing)=do
  s<-readTVarIO cell
  pure (Snapshot (settingsRevision s) (settings s) (closed s) (resolve s target))

-- | Only enforcing judgments require the original action owner to wait.
-- Off/Observe may call 'runWarden' directly; it only offers bounded observation.
wardenEnforces :: WardenBinding -> IO Bool
wardenEnforces binding=do
  Snapshot _ config _ _<-bindingSnapshot binding
  pure (wardenMode config==WardenEnforce)

wardenBindingCurrent :: WardenBinding -> IO Bool
wardenBindingCurrent binding@(WardenBinding (WardenRuntime _ _ cell _) _ _)=do
  Snapshot version config stopped found<-bindingSnapshot binding
  s<-readTVarIO cell
  pure (snapshotCurrent s (Snapshot version config stopped found))

snapshotCurrent :: State -> Snapshot -> Bool
snapshotCurrent s (Snapshot version config stopped found)=
  not (stopped || closed s) && settingsRevision s==version &&
    (wardenMode config==WardenOff || case found of
      Nothing->False
      Just (ident,entry)->maybe False (\current->entryAdmitting current && entryRevision current==entryRevision entry) (M.lookup ident (providers s)))

-- | Wrap acquisition once, at the trusted provider owner. Human and delivered
-- user-seat messages extend the task; peer messages cannot replace its authority.
-- Task history is bounded by refusal, never by dropping earlier constraints.
-- Failed delivery, cancellation and stop invalidate outstanding judgments.
wardenProviderFactory :: WardenRuntime -> StartAgentProvider -> StartAgentProvider
wardenProviderFactory (WardenRuntime _ loadRules cell _) acquire kind identity launch endpoints context host request emit=mask $ \restore->do
  stopped<-closed <$> readTVarIO cell
  revision<-newUnique
  initialRules<-if stopped then pure (Left "Warden session is closed.")
    else restore (loadRules (spawnDirectory (startSpec request)))
  let ident=startAgent request
      report value=case kind of
        PrimaryProvider->maybe (pure ()) (\send->send Nothing (ProviderTool value)) (providerContent host)
        ChildProvider->emit (ProviderUpdate "tool" value)
      secretLaunches=launch:map endpointLaunch endpoints
      secrets=filter (not . T.null) $
        [T.pack value | selected<-secretLaunches,(name,value)<-environment selected,sensitiveLabel (T.pack name)]
        ++maybe [] pure (startResume request)++maybe [] (pure . sourceSessionKey) (startSource request)
      task=case kind of
        PrimaryProvider->Right ""
        ChildProvider->Right (attributed (startOwner request) (spawnTask (startSpec request)))
      entry=Entry revision False Nothing task initialRules secrets report
      retire=atomically $ modifyTVar' cell $ \s->s
        {providers=M.delete identity (providers s),agents=case M.lookup ident (agents s) of
          Just owner | owner==identity->M.delete ident (agents s)
          _->agents s}
      invalidateAt expected=do
        fresh<-newUnique
        atomically $ modifyTVar' cell $ \s->s {providers=M.adjust
          (\e->if maybe True (==entryRevision e) expected
            then e {entryRevision=fresh,entryAdmitting=False} else e) identity (providers s)}
      invalidateTurn turn=do
        fresh<-newUnique
        atomically $ modifyTVar' cell $ \s->s {providers=M.adjust
          (\e->if entryTurn e==Just turn then e {entryRevision=fresh,entryAdmitting=False} else e) identity (providers s)}
      revise readRules activeTurn message=do
        fresh<-newUnique
        -- Reserve before interruptible IO. Old grants expire immediately, and
        -- an old turn's retained cancel cannot cancel this new turn's load.
        reserved<-atomically $ do
          s<-readTVar cell
          case M.lookup identity (providers s) of
            Just e | not (closed s)->do
              writeTVar cell s {providers=M.insert identity
                e {entryRevision=fresh,entryAdmitting=False,entryTurn=maybe (entryTurn e) Just activeTurn} (providers s)}
              pure True
            _->pure False
        if not reserved then pure (Left "Warden provider retired before task delivery.") else do
          rules<-readRules
          published<-atomically $ do
            s<-readTVar cell
            case M.lookup identity (providers s) of
              Just e | not (closed s),entryRevision e==fresh->do
                writeTVar cell s {providers=M.insert identity
                  e {entryAdmitting=True,entryRules=rules,entryTask=extend (entryTask e) message} (providers s)}
                pure True
              _->pure False
          pure $ if published then Right fresh else Left "Warden task delivery was cancelled."
      wrappedEmit event=case event of
        ProviderClosed->retire >> emit event
        _->emit event
  registered<-atomically $ do
    s<-readTVar cell
    if closed s then pure False else do
      writeTVar cell s {providers=M.insert identity entry (providers s),agents=M.insert ident identity (agents s)}
      pure True
  if not registered then pure (Left "Warden session is closed.") else do
    result<-restore (acquire kind identity launch endpoints context host request wrappedEmit) `onException` retire
    case result of
      Left err->retire >> pure (Left err)
      Right driver->do
        live<-atomically $ do
          s<-readTVar cell
          case M.lookup identity (providers s) of
            Just e | not (closed s)->do
              writeTVar cell s {providers=M.insert identity
                e {entrySecrets=driverSessionKey driver:entrySecrets e} (providers s)}
              pure True
            _->pure False
        if not live then (driverStop driver `finally` retire) >> pure (Left "Warden provider retired during acquisition.") else
          pure (Right driver
            { driverDeliver= \turn message extra submission->mask $ \unmask->do
                revised<-revise (unmask (loadRules (spawnDirectory (startSpec request)))) (Just turn) message
                case revised of
                  Left err->pure (Left err)
                  Right revisionSent->do
                    -- Publication above is masked through installing this cleanup.
                    sent<-unmask (driverDeliver driver turn message extra submission) `onException` invalidateAt (Just revisionSent)
                    case sent of
                      Left err->invalidateAt (Just revisionSent) >> pure (Left err)
                      Right active->pure (Right active {cancelProviderTurn=invalidateTurn turn >> cancelProviderTurn active})
            , driverSteer= \message extra submission->mask $ \unmask->do
                revised<-revise (unmask (loadRules (spawnDirectory (startSpec request)))) Nothing message
                case revised of
                  Left err->pure (Left err)
                  Right revisionSent->do
                    sent<-unmask (driverSteer driver message extra submission) `onException` invalidateAt (Just revisionSent)
                    case sent of Left err->invalidateAt (Just revisionSent) >> pure (Left err); Right value->pure (Right value)
            , driverCancel=invalidateAt Nothing >> driverCancel driver
            , driverStop=retire >> driverStop driver })
  where
    extend previous message
      | messageAuthor message/=Human && not (messageIsUserSeat message)=previous
      | otherwise=do
          old<-previous
          let body=old<>"\n"<>attributed (messageAuthor message) (messageText message)
          if T.length body>65536 then Left "Task history exceeds the Warden input bound; start a fresh conversation."
            else Right body
    attributed author text=(case author of Human->"Human task: "; Agent agent->"Parent agent "<>agentIdText agent<>" task: ")<>text

-- | Enforce runs on the owning request worker. Off returns immediately.
-- Observe offers one judgment to the scoped observation slot and returns; busy
-- observations are dropped rather than queued. Ordinary admission still belongs
-- to the caller. A later switch to Enforce cannot reuse an observational receipt.
runWarden :: WardenBinding -> Text -> Value -> IO WardenReceipt
runWarden binding name args=do
  frozen@(WardenBinding runtime _ _)<-captureWardenBinding binding
  Snapshot version config _ found<-bindingSnapshot frozen
  unique<-newUnique
  let actionId="warden-"<>T.pack (show (hashUnique unique))
      stamp=(\(ident,e)->(ident,entryRevision e)) <$> found
      base=WardenResult actionId name False [] Nothing Nothing Nothing
      receipt=WardenReceipt runtime version stamp config base
  case wardenMode config of
    WardenOff->pure receipt
    WardenEnforce->judgeAction frozen actionId name args
    WardenObserve->do
      observe frozen (void (judgeAction frozen actionId name args))
      pure receipt

-- One slot, no pending queue. Only a request worker can offer work; the UI never
-- waits on it. The slot is held only through inspection/spawn, not inference.
-- Provider/task/settings retirement interrupts the decision on this worker.
observe :: WardenBinding -> IO () -> IO ()
observe binding@(WardenBinding (WardenRuntime _ _ cell slot) _ _) action=mask $ \_->do
  captured<-bindingSnapshot binding
  prior<-tryTakeMVar slot
  case prior of
    Nothing->pure ()
    Just old->(do
      busy<-case old of Nothing->pure False; Just worker->maybe True (const False) <$> poll worker
      current<-(`snapshotCurrent` captured) <$> readTVarIO cell
      if busy || not current then putMVar slot old else do
        worker<-asyncWithUnmask $ \unmask->unmask $ void $ race
          (atomically (readTVar cell >>= check . not . (`snapshotCurrent` captured))) action
        putMVar slot (Just worker)) `onException` putMVar slot old

judgeAction :: WardenBinding -> Text -> Text -> Value -> IO WardenReceipt
judgeAction frozen@(WardenBinding runtime@(WardenRuntime service _ _ _) _ _) actionId name args=do
  Snapshot version config stopped found<-bindingSnapshot frozen
  environmentValues<-getEnvironment
  current<-wardenBindingCurrent frozen
  let stamp=(\(ident,e)->(ident,entryRevision e)) <$> found
      private=[T.pack value | (label,value)<-environmentValues,sensitiveLabel (T.pack label),not (null value)]
        ++maybe [] (entrySecrets . snd) found++actionSecrets name args
      prepared=do
        unless (not stopped && current) (Left "Warden request expired before judgment.")
        (_,entry)<-maybe (Left "No trusted task is bound to this caller.") Right found
        unless (entryAdmitting entry) (Left "No current task delivery is active.")
        task<-entryTask entry
        unless (not (T.null (T.strip task))) (Left "No user task has been submitted.")
        rules<-entryRules entry
        pure (WardenInput actionId task rules name args)
  result<-case prepared of
    Right input->judgeWarden service config private input
    Left _->pure (WardenResult actionId name False [] Nothing (Just (DecisionInvalid "No complete trusted task/rules are available.")) Nothing)
  let receipt=WardenReceipt runtime version stamp config result
  reportReceipt found receipt
  pure receipt

-- The two environment input schemas name their values explicitly. Apply the
-- same sensitive-label rule as the environment editor before supplier lookup,
-- including overrides that have never appeared in the process environment.
-- These are extra known-private values, not rewritten model input.
actionSecrets :: Text -> Value -> [Text]
actionSecrets "terminal/create" (Object args)
  | Just (Array entries)<-KM.lookup "env" args=
      [value | Object entry<-V.toList entries,Just (String name)<-[KM.lookup "name" entry]
        ,Just (String value)<-[KM.lookup "value" entry],sensitiveLabel name,not (T.null value)]
actionSecrets "environment_set" (Object args)
  | Just (Object entries)<-KM.lookup "values" args=
      [value | (name,String value)<-KM.toList entries,sensitiveLabel (K.toText name),not (T.null value)]
actionSecrets _ _=[]

reportReceipt :: Maybe (ProviderIdentity,Entry) -> WardenReceipt -> IO ()
reportReceipt found receipt@(WardenReceipt _ _ _ config result)=case found of
  Just (_,entry)->do
    current<-receiptCurrent receipt
    -- Expandable activity records criteria/provenance, not task or argument
    -- values. The existing tool review remains the owner of exact evidence.
    if not current then pure () else entryReport entry (object
      ["toolCallId" .= wardenResultStateId result,"title" .= ("Warden: "<>wardenResultActionName result),"status" .= ("completed"::Text)
      ,"rawOutput" .= object ["mode" .= modeName (wardenMode config),"judged" .= wardenJudged result
        ,"allowed" .= wardenAllows config result,"criteria" .=
          [object ["criterion" .= show criterion,"probability" .= score] | (criterion,score)<-wardenCriteria result]
        ,"failure" .= fmap failureName (wardenFailure result)
        ,"supplier" .= fmap supplierEvidence (wardenSupplier result)
        ,"scope" .= ("Editor-owned action admission. External tools retain provider permissions."::Text)]])
  Nothing->pure ()

-- Public supplier metadata; no task, model input or access key is retained.
supplierEvidence :: DecisionSupplier -> Value
supplierEvidence supplier=object
  ["label" .= supplierLabel description,"incarnation" .= decisionSupplierId supplier
  ,"location" .= show (supplierLocation description),"model" .= case supplierModel description of
    PinnedArtifact digest->object ["verifiedManifest" .= digest]
    ReportedModel name->object ["reportedName" .= name]]
  where description=decisionSupplierDescription supplier

wardenReceiptResult :: WardenReceipt -> WardenResult
wardenReceiptResult (WardenReceipt _ _ _ _ result)=result

receiptCurrent :: WardenReceipt -> IO Bool
receiptCurrent (WardenReceipt (WardenRuntime service _ cell _) version task _ result)=do
  s<-readTVarIO cell
  let owns=not (closed s) && settingsRevision s==version && case task of
        Nothing->True
        Just (ident,revision)->maybe False (\e->entryAdmitting e && entryRevision e==revision) (M.lookup ident (providers s))
  if not owns then pure False else case wardenSupplier result of
    Nothing->pure True
    Just expected->do
      current<-currentDecisionSupplier service
      pure (fmap decisionSupplierId current==Just (decisionSupplierId expected))

-- | Short admission check under the original request's claim. No filesystem,
-- inference, argument comparison or transcript scan occurs here.
checkWarden :: WardenReceipt -> IO (Either Text ())
checkWarden receipt@(WardenReceipt runtime _ _ config result)=do
  now<-getWardenSettings runtime
  if wardenMode now/=WardenEnforce then pure (Right ()) else do
    current<-receiptCurrent receipt
    pure $ if wardenMode config/=WardenEnforce || not current
      then Left "Warden judgment expired; submit the action again."
      else if wardenAllows config result then Right ()
      else Left $ "Warden held this action: "<>case wardenFailure result of
        Just reason->failureName reason
        Nothing->"the proposed action did not meet the configured judgment threshold."

modeName :: WardenMode -> Text
modeName WardenOff="off"
modeName WardenObserve="observe"
modeName WardenEnforce="enforce"
failureName :: DecisionFailure -> Text
failureName DecisionUnavailable="no decision supplier is selected"
failureName DecisionBusy="the decision supplier is busy"
failureName DecisionExpired="the decision supplier changed"
failureName DecisionCancelled="judgment was cancelled"
failureName DecisionDeadline="the judgment deadline expired"
failureName DecisionClosed="the decision service is closed"
failureName DecisionInvalid{}="the complete action could not be judged"
failureName DecisionProviderFailed{}="the decision supplier failed"
