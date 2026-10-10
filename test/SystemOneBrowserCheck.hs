{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : SystemOneBrowserCheck
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
module SystemOneBrowserCheck (checks) where

import Control.Concurrent.Async (withAsync,wait)
import Control.Concurrent.MVar
import Control.Concurrent.STM hiding (check)
import Control.Exception (finally)
import Control.Monad (unless,void)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)
import Hide.Plugin.SystemOne
import Hide.SystemOne
import Hide.SystemOneBrowser

checks :: IO ()
checks=do
  resultChecks
  cancellationChecks
  retirementChecks
  closureChecks
  refusalChecks

-- A browser offer alone cannot disclose context. Success requires both the
-- exact result and its resource release; public receipt identity is independent
-- of the private browser adapter incarnation.
resultChecks :: IO ()
resultChecks=withSystemOne $ \owner->withBrowser $ \registry attachment controls->do
  empty<-currentDecisionSupplier (systemOneServices owner)
  check "advertising browser does not select it" (empty==Nothing)
  check "advertising browser sends no inference context" . (==Nothing) =<< atomically (tryReadTQueue controls)
  offer<-captureBrowserOffer registry >>= present
  provider<-browserProvider offer >>= right
  selected<-selectDecisionProvider owner (Just provider) >>= right >>= present
  let services=systemOneServices owner
      supplier=decisionSupplierId selected
  local<-requestDecision services supplier sample {decisionLocality=HostProcessOnly} 30000
  check "local-only request never reaches browser" (case local of Left DecisionUnavailable->True;_->False)
  ticket<-requestDecision services supplier sample 30000 >>= right
  request<-next "system-one-request" controls
  check "request preserves immutable state and binary domain order" (field "input" request==inputValue)
  let wrong=put "request" (String (T.replicate 48 "f")) (resultControl request)
  stale<-receiveBrowserControl attachment wrong
  check "result cannot substitute request incarnation" (stale==Left DecisionExpired)
  malformed<-receiveBrowserControl attachment (put "unexpected" (String "not accepted") (resultControl request))
  check "unknown result fields refuse without disturbing pending operation" (case malformed of Left (DecisionInvalid _)->True;_->False)
  receiveBrowserControl attachment (resultControl request) >>= right
  check "result alone retains browser ownership" . (==Nothing) =<< pollDecision ticket
  receiveBrowserControl attachment (reply "system-one-released" request) >>= right
  completed<-receipt ticket
  check "common receipt attributes exact browser supplier and pinned output" (case completed of
    Right value->resultSupplier value==selected && resultStateId value==decisionStateId sample && resultOutput value==output
    _->False)
  check "accepted receipt is immutable" . (==completed) =<< receipt ticket
  duplicate<-receiveBrowserControl attachment (resultControl request)
  check "late result cannot reopen completed operation" (duplicate==Left DecisionExpired)
  again<-requestDecision services supplier sample {decisionStateId="second"} 30000 >>= right
  second<-next "system-one-request" controls
  check "scope reuses adapter but every request has a new identity" (field "supplier" request==field "supplier" second && field "request" request/=field "request" second)
  receiveBrowserControl attachment (resultControl second) >>= right
  receiveBrowserControl attachment (reply "system-one-released" second) >>= right
  check "same browser scope serves the next admitted request" . either (const False) (const True) =<< receipt again
  _<-selectDecisionProvider owner Nothing >>= right
  retire<-next "system-one-retire" controls
  check "idle scope retirement carries no state or request" (field "supplier" retire==field "supplier" second && not (has "input" retire || has "request" retire))
  receiveBrowserControl attachment (put "type" (String "system-one-retired") retire) >>= right

cancellationChecks :: IO ()
cancellationChecks=withSystemOne $ \owner->withBrowser $ \registry attachment controls->do
  provider<-captureBrowserOffer registry >>= present >>= browserProvider >>= right
  selected<-selectDecisionProvider owner (Just provider) >>= right >>= present
  let services=systemOneServices owner
  ticket<-requestDecision services (decisionSupplierId selected) sample 30000 >>= right
  request<-next "system-one-request" controls
  won<-cancelDecision ticket
  check "explicit cancellation wins pending receipt" won
  cancel<-next "system-one-cancel" controls
  check "cancel addresses exact request without replaying context" (cancel==reply "system-one-cancel" request)
  check "cancelled receipt resolves before physical release" . (==Left DecisionCancelled) =<< receipt ticket
  busy<-requestDecision services (decisionSupplierId selected) sample 30000
  check "cancel does not free browser inference slot" (case busy of Left DecisionBusy->True;_->False)
  late<-receiveBrowserControl attachment (resultControl request)
  check "late result cannot overwrite cancellation" (late==Left DecisionExpired)
  _<-selectDecisionProvider owner (Just provider) >>= right
  receiveBrowserControl attachment (reply "system-one-released" request) >>= right
  retire<-next "system-one-retire" controls
  current<-currentDecisionSupplier services >>= present
  blocked<-requestDecision services (decisionSupplierId current) sample 30000
  check "supplier replacement also waits for old idle scope release" (case blocked of Left DecisionBusy->True;_->False)
  receiveBrowserControl attachment (put "type" (String "system-one-retired") retire) >>= right
  check "release cannot rewrite cancelled receipt" . (==Left DecisionCancelled) =<< receipt ticket

retirementChecks :: IO ()
retirementChecks=withSystemOne $ \owner->withBrowser $ \registry attachment controls->do
  original<-captureBrowserOffer registry >>= present
  provider<-browserProvider original >>= right
  selected<-selectDecisionProvider owner (Just provider) >>= right >>= present
  ticket<-requestDecision (systemOneServices owner) (decisionSupplierId selected) sample 30000 >>= right
  request<-next "system-one-request" controls
  -- This result is prepared but has no resource release. Replacing the offer
  -- must expire it rather than adopt it or misclassify retirement as Cancel.
  receiveBrowserControl attachment (resultControl request) >>= right
  receiveBrowserControl attachment (offerControl attachment viewer 2) >>= right
  check "replacement expires pending exact offer" . (==Left DecisionExpired) =<< receipt ticket
  old<-browserProvider original
  check "escaped old offer cannot acquire replacement" (case old of Left DecisionExpired->True;_->False)
  late<-receiveBrowserControl attachment (resultControl request)
  check "old result cannot address replacement offer" (late==Left DecisionExpired)
  stale<-receiveBrowserControl attachment (offerControl attachment viewer 1)
  check "older offer cannot resurrect within viewer" (stale==Left DecisionExpired)
  setBrowserViewer attachment (Just nextViewer) >>= right
  oldViewer<-receiveBrowserControl attachment (offerControl attachment viewer 3)
  check "retired viewer cannot publish another offer" (oldViewer==Left DecisionExpired)
  receiveBrowserControl attachment (offerControl attachment nextViewer 1) >>= right
  newest<-captureBrowserOffer registry >>= present
  check "fresh viewer starts its own offer revision" (browserOfferViewer newest==nextViewer && browserOfferId newest==1)
  receiveBrowserControl attachment (object ["type" .= String "system-one-withdraw","connection" .= browserAttachmentId attachment,"viewer" .= nextViewer,"offer" .= (1::Int)]) >>= right
  check "withdraw retires selectable offer" . maybe True (const False) =<< captureBrowserOffer registry
  revived<-receiveBrowserControl attachment (offerControl attachment nextViewer 1)
  check "withdrawn revision cannot reoffer" (revived==Left DecisionExpired)
  retireBrowserAttachment attachment
  closed<-receiveBrowserControl attachment (error "closed receiver forced payload")
  check "closed attachment rejects without reading controls" (closed==Left DecisionClosed)

-- Close the browser registry while the common owner is still live. Its pending
-- receipt must retain the Closed cause even after a prepared result arrived.
closureChecks :: IO ()
closureChecks=withSystemOne $ \owner->do
  ticket<-withSystemOneBrowser $ \registry->do
    controls<-newTQueueIO
    attachment<-attachBrowser registry 1 (\value->atomically (writeTQueue controls value) >> pure True)
    setBrowserViewer attachment (Just viewer) >>= right
    receiveBrowserControl attachment (offerControl attachment viewer 1) >>= right
    offer<-captureBrowserOffer registry >>= present
    provider<-browserProvider offer >>= right
    selected<-selectDecisionProvider owner (Just provider) >>= right >>= present
    pending<-requestDecision (systemOneServices owner) (decisionSupplierId selected) sample 30000 >>= right
    request<-next "system-one-request" controls
    receiveBrowserControl attachment (resultControl request) >>= right
    pure pending
  check "registry close expires prepared result with Closed cause" . (==Left DecisionClosed) =<< receipt ticket

refusalChecks :: IO ()
refusalChecks=withSystemOneBrowser $ \registry->do
  attachment<-attachBrowser registry 1 (\_->pure False)
  setBrowserViewer attachment (Just viewer) >>= right
  receiveBrowserControl attachment (offerControl attachment viewer 1) >>= right
  offer<-captureBrowserOffer registry >>= present
  provider<-browserProvider offer >>= right
  escaped<-newEmptyMVar
  finish<-newEmptyMVar
  withAsync (withDecisionDriver provider (pure False) $ \driver->do
    putMVar escaped driver
    takeMVar finish) $ \scope->(do
      driver<-barrier "captured browser driver" (takeMVar escaped)
      refused<-runDecision driver sample (pure False)
      check "unadmitted transport request is explicitly unavailable" (refused==Left DecisionUnavailable)
      duplicate<-runDecision driver sample {decisionQuestions=[DecisionQuestion "choice" "" (ChoiceDecision [DecisionOption "same" "a",DecisionOption "same" "b"])]} (pure False)
      check "typed direct driver validates duplicate choice labels" (case duplicate of Left (DecisionInvalid _)->True;_->False)
      oversized<-runDecision driver sample {decisionState=T.replicate 65536 "\x01",decisionQuestions=[DecisionQuestion "binary" (T.replicate 65000 "\x01") (BinaryDecision "no" "yes")]} (pure False)
      check "escaped JSON refuses oversized control without truncation" (case oversized of Left (DecisionInvalid _)->True;_->False)
      putMVar finish ()
      barrier "unused browser driver scope closes" (wait scope)
      expired<-runDecision driver (error "retired driver forced input") (pure False)
      check "retained scoped driver refuses before payload preparation" (expired==Left DecisionExpired)) `finally` do
        void (tryPutMVar finish ())
        retireBrowserAttachment attachment

withBrowser :: (SystemOneBrowser -> BrowserAttachment -> TQueue Value -> IO a) -> IO a
withBrowser action=withSystemOneBrowser $ \registry->do
  controls<-newTQueueIO
  attachment<-attachBrowser registry 1 (\value->atomically (writeTQueue controls value) >> pure True)
  setBrowserViewer attachment (Just viewer) >>= right
  receiveBrowserControl attachment (offerControl attachment viewer 1) >>= right
  action registry attachment controls `finally` retireBrowserAttachment attachment

viewer,nextViewer,manifest :: Text
viewer=T.replicate 48 "a"
nextViewer=T.replicate 48 "b"
manifest=T.replicate 64 "c"

offerControl :: BrowserAttachment -> Text -> Int -> Value
offerControl attachment who revision=object
  ["type" .= String "system-one-offer","connection" .= browserAttachmentId attachment,"viewer" .= who,"offer" .= revision
  ,"label" .= String "Browser fixture","manifestSHA256" .= manifest,"allocationLimitBytes" .= (1048576::Int),"backend" .= String "webgpu"]

sample :: DecisionInput
sample=DecisionInput "browser-state" "authorized text" [DecisionQuestion "binary" "choose" (BinaryDecision "no" "yes")] SelectedSupplier

inputValue :: Value
inputValue=object ["stateId" .= decisionStateId sample,"state" .= decisionState sample,"questions" .=
  [object ["name" .= String "binary","instructions" .= String "choose","kind" .= String "binary","false" .= String "no","true" .= String "yes"]]]

output :: DecisionOutput
output=DecisionOutput (PinnedArtifact manifest) [DecisionAnswer "binary" BinaryAnswer [0.25,0.75] (Just 0.5)] (Just (DecisionUsage (Just 4) Nothing))

resultControl :: Value -> Value
resultControl request=put "output" (object
  ["manifestSHA256" .= manifest,"answers" .= [object ["question" .= String "binary","kind" .= String "binary","probabilities" .= ([0.25,0.75]::[Double]),"confidence" .= (0.5::Double)]]
  ,"usage" .= object ["inputTokens" .= (4::Int)]]) (reply "system-one-result" request)

reply :: Text -> Value -> Value
reply kind request=object (("type" .= kind):[K.fromText key .= field key request | key<-["connection","viewer","offer","supplier","request"]])

put :: Text -> Value -> Value -> Value
put key value (Object o)=Object (KM.insert (K.fromText key) value o)
put _ _ _=error "expected control object"

field :: Text -> Value -> Value
field key (Object o)=maybe (error "missing control field") id (KM.lookup (K.fromText key) o)
field _ _=error "expected control object"

has :: Text -> Value -> Bool
has key (Object o)=KM.member (K.fromText key) o
has _ _=False

next :: Text -> TQueue Value -> IO Value
next kind controls=do
  control<-barrier "browser control not delivered" (atomically (readTQueue controls))
  check "unexpected browser control kind" (field "type" control==String kind)
  pure control

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)

right :: Show a => Either a b -> IO b
right=either (error . show) pure

present :: Maybe a -> IO a
present=maybe (error "expected browser offer/supplier") pure

barrier :: String -> IO a -> IO a
barrier label action=timeout 2000000 action >>= maybe (error label) pure

receipt :: DecisionTicket -> IO (Either DecisionFailure DecisionResult)
receipt=barrier "browser decision did not resolve" . awaitDecision
