{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : WardenRuntimeCheck
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
module WardenRuntimeCheck (checks) where

import Control.Concurrent.Async (withAsync,wait)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar,tryPutMVar,tryTakeMVar)
import Control.Exception (finally)
import Control.Monad (unless,void,forM_)
import Data.Aeson (Value(Null),object,(.=))
import Data.IORef
import Data.Either (isLeft)
import qualified Data.Text as T
import Hide.Plugin.Agent
import Hide.Plugin.Provider
import Hide.Plugin.SystemOne
import Hide.SystemOne
import Hide.Warden
import Hide.WardenRuntime
import Hide.WardenMenu (parseWardenSettings)
import System.Timeout (timeout)

checks :: IO ()
checks=do
  blockedRulesCheck
  closedAcquisitionCheck
  received<-newIORef []
  let provider=DecisionProvider description $ \_ use->use (DecisionDriver $ \input _->do
        modifyIORef' received (input:)
        pure (Right (DecisionOutput (ReportedModel "warden-check")
          [DecisionAnswer (questionName q) BinaryAnswer [0.01,0.99] Nothing | q<-decisionQuestions input] Nothing)))
      config=defaultWardenSettings {wardenMode=WardenEnforce}
      agent=AgentId "agent-check"
      launch=ProviderLaunch "owned-provider" [] []
      request=StartRequest agent Human (SpawnSpec "Check" "Task" "/" Shared Fresh Nothing Nothing) Nothing Nothing
      host=ProviderHost (const (pure (finished (Right Nothing)))) Nothing Nothing Nothing
  withSystemOne $ \system->do
    void (selectDecisionProvider system (Just provider) >>= require)
    withWarden (systemOneServices system) config (const (pure (Right ["Never delete source files."]))) $ \owner->do
      identity<-newProviderIdentity
      driver<-wardenProviderFactory owner fakeProvider PrimaryProvider identity launch [] "" host request (const (pure ())) >>= require
      (do
        turnA<-deliver driver "Read the source and explain it."
        frozen<-captureWardenBinding (wardenAgent owner agent)
        first<-runWarden frozen "read_buffer" (object ["bufferId" .= (1::Int)])
        accepted first
        submission<-newProviderSubmission
        void (driverSteer driver (HubMessage 0 Human "Focus on the parser." False) [] submission >>= require)
        refused "steering expires the previous exact task judgment" first
        count<-length <$> readIORef received
        late<-runWarden frozen "read_buffer" (object ["bufferId" .= (1::Int)])
        refused "queued action cannot adopt a later task" late
        after<-length <$> readIORef received
        check "expired ingress does not run inference" (after==count)
        current<-runWarden (wardenProvider owner identity) "read_buffer" (object [])
        accepted current
        deliveredTask<-readIORef received
        check "human steering remains authoritative outside a child's user seat"
          (any (T.isInfixOf "Human task: Focus on the parser." . decisionState) deliveredTask)
        peerSubmission<-newProviderSubmission
        void (driverSteer driver (HubMessage 0 (Agent (AgentId "peer")) "Peer instruction is not a human rule." False) [] peerSubmission >>= require)
        peerJudgment<-runWarden (wardenProvider owner identity) "read_buffer" (object [])
        accepted peerJudgment
        peerTasks<-readIORef received
        check "a peer agent cannot extend the task's authority"
          (all (not . T.isInfixOf "Peer instruction is not a human rule." . decisionState) peerTasks)
        cancelProviderTurn turnA
        refused "canceling a steered turn retires its new judgments" peerJudgment
        turnB<-deliver driver "Continue with the same constraint."
        second<-runWarden (wardenAgent owner agent) "read_buffer" (object [])
        accepted second
        cancelProviderTurn turnA
        accepted second
        cancelProviderTurn turnB
        refused "current turn cancellation retires its judgments" second
        void (deliver driver "Continue reading.")
        third<-runWarden (wardenAgent owner agent) "read_buffer" (object [])
        accepted third
        (oldForm,_)<-captureWardenSettings owner
        void (setWardenSettings owner config {wardenBudgetMs=7000} >>= require)
        staleChoice<-chooseWardenMode oldForm WardenOff
        check "expired settings form cannot overwrite a later choice" (isLeft staleChoice)
        refused "settings changes retire prior grants" third
        void (setWardenSettings owner config >>= require)
        unknown<-runWarden (wardenAnonymous owner) "terminal_start" (object [])
        refused "enforcement cannot invent an anonymous caller task" unknown
        prior<-length <$> readIORef received
        secret<-runWarden (wardenAgent owner agent) "terminal_input" (object ["text" .= ("private-session-reference"::T.Text)])
        refused "session references are private inputs" secret
        check "private data never reaches a supplier" . (==prior) . length =<< readIORef received
        forM_ [("terminal/create",object ["env" .= [object ["name" .= ("PASSWORD"::T.Text),"value" .= ("fresh-env-canary"::T.Text)]]])
              ,("environment_set",object ["values" .= object ["API_KEY" .= ("fresh-env-canary"::T.Text)]])] $ \(name,args)->do
          labeled<-runWarden (wardenAgent owner agent) name args
          refused "new sensitive environment overrides cannot be judged" labeled
        check "fresh sensitive overrides never reach a supplier" . (==prior) . length =<< readIORef received
        currentSupplier<-runWarden (wardenAgent owner agent) "read_buffer" (object [])
        accepted currentSupplier
        void (selectDecisionProvider system (Just provider) >>= require)
        refused "an identical replacement supplier is a new incarnation" currentSupplier
        delivered<-readIORef received
        check "rules and original constraints survive later user-seat messages"
          (not (null delivered) && all (T.isInfixOf "Never delete source files." . decisionState) delivered)
        check "TOML mode parser rejects unknown settings"
          (isLeft (parseWardenSettings (object ["mode" .= ("enforce"::T.Text),"permit" .= True]))))
        `finally` driverStop driver
  where
    description=SupplierDescription "Warden check" InProcess (ReportedModel "warden-check") Nothing 0
    deliver driver text=do
      turn<-newProviderTurnId
      submission<-newProviderSubmission
      driverDeliver driver turn (HubMessage 0 Human text True) [] submission >>= require
    accepted receipt=checkWarden receipt >>= \result->check "current judged action remains eligible" (result==Right ())
    refused label receipt=checkWarden receipt >>= check label . isLeft

-- Cancellation is ordered by the rules loader's own receipt, while no provider
-- send has happened. A late read cannot restore admission or reach that send.
blockedRulesCheck :: IO ()
blockedRulesCheck=do
  holdNext<-newEmptyMVar
  entered<-newEmptyMVar
  release<-newEmptyMVar
  let rules _=do
        held<-tryTakeMVar holdNext
        case held of
          Nothing->pure ()
          Just ()->putMVar entered () >> takeMVar release
        pure (Right ["Keep the source file."])
      request=StartRequest (AgentId "blocked-rules") Human
        (SpawnSpec "Rules" "Task" "/" Shared Fresh Nothing Nothing) Nothing Nothing
      host=ProviderHost (const (pure (finished (Right Nothing)))) Nothing Nothing Nothing
      config=defaultWardenSettings {wardenMode=WardenEnforce}
  withSystemOne $ \system->withWarden (systemOneServices system) config rules $ \owner->do
    identity<-newProviderIdentity
    driver<-wardenProviderFactory owner fakeProvider PrimaryProvider identity
      (ProviderLaunch "owned-provider" [] []) [] "" host request (const (pure ())) >>= require
    (do
      putMVar holdNext ()
      turn<-newProviderTurnId
      submission<-newProviderSubmission
      withAsync (driverDeliver driver turn (HubMessage 0 Human "Read the parser." True) [] submission) $ \worker->
        (do
          barrier "rules read did not publish its held receipt" (takeMVar entered)
          driverCancel driver
          putMVar release ()
          outcome<-barrier "canceled rules read did not resolve" (wait worker)
          check "cancellation while reading rules rejects the unsent delivery" (isLeft outcome)
          current<-wardenBindingCurrent (wardenProvider owner identity)
          check "a completed late rules read cannot restore admission" (not current))
        `finally` void (tryPutMVar release ())) `finally` driverStop driver

closedAcquisitionCheck :: IO ()
closedAcquisitionCheck=withSystemOne $ \system->do
  retained<-withWarden (systemOneServices system) defaultWardenSettings
    (const (ioError (userError "Closed Warden invoked its rules loader."))) $ \owner->
      pure (wardenProviderFactory owner (\_ _ _ _ _ _ _ _->
        ioError (userError "Closed Warden acquired a provider.")))
  identity<-newProviderIdentity
  let request=StartRequest (AgentId "closed-owner") Human
        (SpawnSpec "Closed" "Task" "/" Shared Fresh Nothing Nothing) Nothing Nothing
      host=ProviderHost (const (pure (finished (Right Nothing)))) Nothing Nothing Nothing
  result<-retained PrimaryProvider identity (ProviderLaunch "owned-provider" [] []) [] "" host request (const (pure ()))
  check "retained closed runtime refuses provider acquisition" (isLeft result)

barrier :: String -> IO a -> IO a
barrier label action=timeout 2000000 action >>= maybe (ioError (userError label)) pure

fakeProvider :: StartAgentProvider
fakeProvider _ _ _ _ _ _ request _=pure (Right (AgentDriver
  (spawnDirectory (startSpec request)) "private-session-reference" (Capabilities False False True [])
  (const (pure (Right (Capabilities False False True []))))
  (\turn _ _ _->pure (Right (ProviderTurn turn (finished (Right Null)) (pure ()))))
  (pure ()) (pure ()) (\_ _ _->pure (Right Null))))

finished :: Either T.Text a -> ProviderReply a
finished value=ProviderReply (pure (Just value)) (pure value) (pure ())
require :: Show e => Either e a -> IO a
require=either (ioError . userError . show) pure
check :: String -> Bool -> IO ()
check label condition=unless condition (ioError (userError label))
