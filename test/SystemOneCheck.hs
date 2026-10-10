-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : SystemOneCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module SystemOneCheck (checks) where

import Control.Concurrent.MVar
import Control.Concurrent.Async (withAsync,wait)
import Control.Concurrent.STM hiding (check)
import qualified Control.Concurrent.STM as STM
import Control.Exception (finally)
import Control.Monad (unless,void)
import Data.IORef
import qualified Data.Text as T
import System.Timeout (timeout)
import Hide.SystemOne
import Hide.Plugin.SystemOne

checks :: IO ()
checks=do
  successfulChecks
  cancellationChecks
  deadlineChecks
  closeChecks

successfulChecks :: IO ()
successfulChecks=do
  acquisitions<-newIORef (0::Int)
  invocations<-newIORef (0::Int)
  released<-newEmptyMVar
  let provider=DecisionProvider description $ \_ action->do
        modifyIORef' acquisitions (+1)
        action (DecisionDriver $ \input _->do
          modifyIORef' invocations (+1)
          pure (Right (response input))) `finally` putMVar released ()
  (retained,completed)<-withSystemOne $ \owner->do
    let services=systemOneServices owner
    empty<-currentDecisionSupplier services
    check "unselected service has no supplier" (empty==Nothing)
    _<-selectDecisionProvider owner (Just provider) >>= right >>= present
    selected<-currentDecisionSupplier services >>= present
    check "selection alone does not acquire a model" . (==0) =<< readIORef acquisitions
    let ident=decisionSupplierId selected
        reject label input deadline=do
          result<-requestDecision services ident input deadline
          check label (case result of Left (DecisionInvalid _)->True;_->False)
    reject "oversized state refuses instead of truncating" sample {decisionState=T.replicate 65537 "x"} 1000
    reject "state bound counts UTF-8 bytes rather than characters" sample {decisionState=T.replicate 32769 "é"} 1000
    reject "aggregate text bound covers question domains" sample {decisionQuestions=[DecisionQuestion "large" (T.replicate 65536 "x") (BinaryDecision (T.replicate 65536 "y") "")]} 1000
    reject "question count is bounded" sample {decisionQuestions=replicate 17 sampleQuestion} 1000
    reject "question identifiers must be distinct" sample {decisionQuestions=replicate 2 sampleQuestion} 1000
    reject "option count is bounded" sample {decisionQuestions=[DecisionQuestion "choice" "" (ChoiceDecision (replicate 256 (DecisionOption "x" "")))]} 1000
    reject "choice labels must be distinct" sample {decisionQuestions=[DecisionQuestion "choice" "" (ChoiceDecision [DecisionOption "x" "first",DecisionOption "x" "second"])]} 1000
    reject "deadline is bounded" sample 30001
    check "invalid inputs do not acquire a model" . (==0) =<< readIORef acquisitions
    stale<-requestDecision services "different-supplier" sample 1000
    check "request cannot substitute a supplier" (case stale of Left DecisionExpired->True;_->False)
    ticket<-requestDecision services ident sample 1000 >>= right
    first<-receipt ticket
    again<-receipt ticket
    check "repeat await preserves exact result provenance and state" (first==again && case first of
      Right result->resultSupplier result==selected && resultStateId result==decisionStateId sample && resultOutput result==response sample
      _->False)
    cancelled<-cancelDecision ticket
    check "cancel after accepted success leaves the receipt unchanged" (not cancelled)
    rounding<-requestDecision services ident sample {decisionStateId="rounding",decisionQuestions=[DecisionQuestion "choice" "" (ChoiceDecision [DecisionOption (T.pack (show n)) "" | n<-[1::Int ..255]])]} 1000 >>= right >>= receipt
    check "declared four-decimal error accepts a rounded distribution" (case rounding of Right _->True;_->False)
    mismatch<-requestDecision services ident sample {decisionStateId="wrong-model"} 1000 >>= right >>= receipt
    check "different model identity is refused" (case mismatch of Left (DecisionInvalid _)->True;_->False)
    malformed<-requestDecision services ident sample {decisionStateId="nonfinite"} 1000 >>= right >>= receipt
    check "nonfinite probabilities are refused" (case malformed of Left (DecisionInvalid _)->True;_->False)
    check "consecutive requests reuse the selected model scope" . (==1) =<< readIORef acquisitions
    check "only valid admitted requests invoke the driver" . (==4) =<< readIORef invocations
    pure (services,ticket)
  observed<-tryReadMVar released
  check "scope exit joins the selected resource" (observed==Just ())
  closed<-requestDecision retained "old" sample 1000
  check "retained services cannot admit after close" (case closed of Left DecisionClosed->True;_->False)
  check "close preserves an already accepted result" . either (const False) (const True) =<< receipt completed
  where
    response input
      | decisionStateId input=="wrong-model"=output {outputModel=ReportedModel "another"}
      | decisionStateId input=="nonfinite"=output {outputAnswers=[DecisionAnswer "binary" BinaryAnswer [0/0,1] Nothing]}
      | decisionStateId input=="rounding"=output {outputAnswers=[DecisionAnswer "choice" ChoiceAnswer (replicate 255 0.0039) (Just 0.5)]}
      | otherwise=output

cancellationChecks :: IO ()
cancellationChecks=do
  entered<-newEmptyMVar
  draining<-newEmptyMVar
  finish<-newEmptyMVar
  scopeDraining<-newEmptyMVar
  releaseScope<-newEmptyMVar
  let provider=DecisionProvider description $ \_ action->
        action (DecisionDriver $ \_ stop->do
          putMVar entered ()
          atomically (stop >>= checkSTM)
          putMVar draining ()
          takeMVar finish
          pure (Right output)) `finally` (putMVar scopeDraining () >> takeMVar releaseScope)
  withSystemOne $ \owner->(do
    let services=systemOneServices owner
    old<-selectDecisionProvider owner (Just provider) >>= right >>= present
    ticket<-requestDecision services (decisionSupplierId old) sample 1000 >>= right
    barrier "inference entered" (takeMVar entered)
    won<-cancelDecision ticket
    check "cancellation wins while inference is active" won
    barrier "cancelled inference is draining" (takeMVar draining)
    check "cancelled receipt resolves before inference drains" . (==Left DecisionCancelled) =<< receipt ticket
    replacement<-selectDecisionProvider owner (Just provider) >>= right >>= present
    check "even an equal supplier description gets a new incarnation" (decisionSupplierId replacement/=decisionSupplierId old)
    requestDecision services (decisionSupplierId replacement) sample 1000 >>= busy "cancel does not free an inference slot"
    putMVar finish ()
    barrier "retired model scope is draining" (takeMVar scopeDraining)
    requestDecision services (decisionSupplierId replacement) sample 1000 >>= busy "replacement waits for old scope release"
    check "late successful return cannot rewrite cancellation" . (==Left DecisionCancelled) =<< receipt ticket
    putMVar releaseScope ()) `finally` do
      void (tryPutMVar finish ())
      void (tryPutMVar releaseScope ())

deadlineChecks :: IO ()
deadlineChecks=do
  entered<-newEmptyMVar
  draining<-newEmptyMVar
  finish<-newEmptyMVar
  let provider=DecisionProvider description $ \_ action->action (DecisionDriver $ \_ stop->do
        putMVar entered ()
        atomically (stop >>= checkSTM)
        putMVar draining ()
        takeMVar finish
        pure (Right output))
  withSystemOne $ \owner->(do
    selected<-selectDecisionProvider owner (Just provider) >>= right >>= present
    let services=systemOneServices owner
    ticket<-requestDecision services (decisionSupplierId selected) sample 1000 >>= right
    barrier "deadline inference entered" (takeMVar entered)
    -- The driver observes the owner's stop publication before any ticket method:
    -- expiration must progress independently of consumer polling.
    barrier "deadline stops the exact inference" (takeMVar draining)
    polled<-pollDecision ticket
    check "deadline receipt is terminal while cleanup is still held" (polled==Just (Left DecisionDeadline))
    requestDecision services (decisionSupplierId selected) sample 1000 >>= busy "deadline retains the physical slot during cleanup"
    putMVar finish ()
    check "late output cannot rewrite deadline" . (==Left DecisionDeadline) =<< receipt ticket) `finally` void (tryPutMVar finish ())

closeChecks :: IO ()
closeChecks=do
  entered<-newEmptyMVar
  draining<-newEmptyMVar
  finish<-newEmptyMVar
  closing<-newEmptyMVar
  escape<-newEmptyMVar
  released<-newEmptyMVar
  let provider=DecisionProvider description $ \_ action->
        action (DecisionDriver $ \_ stop->do
          putMVar entered ()
          atomically (stop >>= checkSTM)
          putMVar draining ()
          takeMVar finish
          pure (Right output)) `finally` putMVar released ()
  withAsync (withSystemOne $ \owner->do
    selected<-selectDecisionProvider owner (Just provider) >>= right >>= present
    let services=systemOneServices owner
    ticket<-requestDecision services (decisionSupplierId selected) sample 1000 >>= right
    barrier "closing inference entered" (takeMVar entered)
    putMVar escape (services,ticket)
    takeMVar closing) $ \scope->(do
      (services,ticket)<-barrier "services captured" (takeMVar escape)
      putMVar closing ()
      barrier "close signals active inference" (takeMVar draining)
      check "shutdown resolves pending receipt before joining" . (==Left DecisionClosed) =<< receipt ticket
      requestDecision services "old" sample 1000 >>= \result->check "closed retained service refuses" (case result of Left DecisionClosed->True;_->False)
      current<-currentDecisionSupplier services
      check "closed service does not advertise its retired supplier" (current==Nothing)
      putMVar finish ()
      barrier "scope joins cleanup" (wait scope)
      check "provider resource is released before scope returns" . (==Just ()) =<< tryReadMVar released) `finally` do
        void (tryPutMVar finish ())
        void (tryPutMVar closing ())
  withSystemOne $ \owner->do
    let remote=DecisionProvider (description {supplierLocation=SystemOneEndpoint "http://127.0.0.1/systemone"}) (\_ _->error "local-only request acquired endpoint")
    selected<-selectDecisionProvider owner (Just remote) >>= right >>= present
    result<-requestDecision (systemOneServices owner) (decisionSupplierId selected) sample {decisionLocality=HostProcessOnly} 1000
    check "loopback endpoint is not host-process-local" (case result of Left DecisionUnavailable->True;_->False)

sample :: DecisionInput
sample=DecisionInput "state-1" "immutable authorized text" [sampleQuestion] SelectedSupplier

sampleQuestion :: DecisionQuestion
sampleQuestion=DecisionQuestion "binary" "choose" (BinaryDecision "false" "true")

description :: SupplierDescription
description=SupplierDescription "fixture" InProcess (ReportedModel "fixture-model") Nothing 0.00005

output :: DecisionOutput
output=DecisionOutput (supplierModel description) [DecisionAnswer "binary" BinaryAnswer [0.25,0.75] (Just 0.5)] (Just (DecisionUsage (Just 4) (Just 2)))

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)

checkSTM :: Bool -> STM ()
checkSTM=STM.check

right :: Show a => Either a b -> IO b
right=either (error.show) pure

present :: Maybe a -> IO a
present=maybe (error "expected selected supplier") pure

barrier :: String -> IO a -> IO a
barrier label action=timeout 2000000 action >>= maybe (error label) pure

receipt :: DecisionTicket -> IO (Either DecisionFailure DecisionResult)
receipt=barrier "decision receipt did not resolve" . awaitDecision

busy :: String -> Either DecisionFailure DecisionTicket -> IO ()
busy label result=check label (case result of Left DecisionBusy->True;_->False)
