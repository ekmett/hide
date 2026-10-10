-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- | Module      : CompletionContextCheck
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Direct source slices and a scoped decision provider exercise selection at its
-- ownership boundary. No filesystem, external model or preparatory state.
module CompletionContextCheck (checks) where

import Control.Concurrent.Async (withAsync, cancel)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM as STM
import Control.Monad (unless)
import Data.IORef
import Data.List (sort)
import qualified Data.Text as T
import System.Timeout (timeout)
import Hide.Buffer
import Hide.CompletionContext
import Hide.Plugin.Completion
import Hide.Plugin.SystemOne
import Hide.SystemOne

checks :: IO ()
checks=do
  let rows=["module Example where","","import Data.Text (Text)","",
        "name :: Text","name = \"Alice\"",""]++replicate 95 ""++["greet :: Text -> Text","greet who = name <> who"]
      text=T.intercalate "\r\n" rows
      source=newBuffer text
      first=length rows-2
      input=CompletionInput "request" "propose" "Example.hs" text 3
        (bufferLineOffset source (first+1)+12) first (drop first rows) [CompletionEdit 4 "before" "after"] []
      candidates=contextCandidates source input [4,2]
      starts=map regionFirstLine candidates
      secret="test-private-value\ncontinuation-private"
      privateRegion=CompletionRegion 80 (T.lines secret)
      offered=candidates++[privateRegion]
      select services=selectContext services HostProcessOnly 1000 [secret] (first+1,12) input offered
  check "complete CRLF blocks retain source coordinates, order and contents"
    (starts==sort starts && any ((==["name :: Text","name = \"Alice\""]) . regionLines) candidates &&
      all (\r->regionLines r==take (length (regionLines r)) (drop (regionFirstLine r) rows)) candidates)
  check "optional regions exclude mandatory local context"
    (all (\r->regionFirstLine r+length (regionLines r)<=first) candidates)
  let huge=newBuffer (T.replicate 5000 "x"<>"\n\n"<>T.unlines rows)
  check "oversized blocks are omitted, not cut into invented source"
    (all ((>0) . regionFirstLine) (contextCandidates huge input [0]))
  withSystemOne $ \owner->do
    let services=systemOneServices owner
    missing<-select services
    check "no supplier leaves the ordinary input intact" (sameInput input missing)
    observed<-newIORef []
    let provider=DecisionProvider description $ \_ action->action (DecisionDriver $ \question _->do
          modifyIORef' observed (question:)
          let options=case decisionQuestions question of [DecisionQuestion _ _ (ChoiceDecision xs)]->xs;_->[]
              weights=[if "name :: Text" `T.isInfixOf` optionCriterion option then 0.8 else 0.2/fromIntegral (length options-1) | option<-options]
          pure (Right (DecisionOutput (supplierModel description) [DecisionAnswer "context" ChoiceAnswer weights Nothing] Nothing)))
    _<-selectDecisionProvider owner (Just provider) >>= right
    ranked<-select services
    questions<-readIORef observed
    check "ranking adds bounded whole source blocks without changing the editable input"
      (sameInput input ranked {inputRegions=[]} && length (inputRegions ranked)==2 &&
        all (`elem` candidates) (inputRegions ranked) &&
        any ((==4) . regionFirstLine) (inputRegions ranked) &&
        map regionFirstLine (inputRegions ranked)==sort (map regionFirstLine (inputRegions ranked)))
    check "known private values never enter the ranking request"
      (length questions==1 && all (\q->all (not . (`T.isInfixOf` T.pack (show q))) (T.lines secret)) questions)
    let protected=input {inputNearby=inputNearby input++["alpha-private-tail",secret]}
    skipped<-selectContext services HostProcessOnly 1000 ["alpha","alpha-private-tail",secret] (first+1,12) protected offered
    afterPrivate<-readIORef observed
    check "overlapping and multiline private local values skip inference without shifting context"
      (sameInput protected skipped && length afterPrivate==length questions)
  -- Generation changes cancel the selection task. Observe the exact driver's
  -- stop signal, rather than inferring retirement from time or UI status.
  entered<-newEmptyMVar
  retired<-newEmptyMVar
  let blocked=DecisionProvider description $ \_ action->action (DecisionDriver $ \_ stop->do
        putMVar entered ()
        atomically (stop >>= STM.check)
        putMVar retired ()
        pure (Left DecisionCancelled))
  withSystemOne $ \owner->do
    _<-selectDecisionProvider owner (Just blocked) >>= right
    withAsync (select (systemOneServices owner)) $ \worker->do
      barrier "ranking started" (takeMVar entered)
      cancel worker
      barrier "cancelled ranking releases its exact inference" (takeMVar retired)
  withSystemOne $ \owner->do
    _<-selectDecisionProvider owner (Just (DecisionProvider description $ \_ action->
      action (DecisionDriver $ \_ _->pure (Left DecisionUnavailable)))) >>= right
    fallback<-select (systemOneServices owner)
    check "provider failure preserves baseline context and identity" (sameInput input fallback)

sameInput :: CompletionInput -> CompletionInput -> Bool
sameInput a b=inputId a==inputId b && inputIntent a==inputIntent b && inputPath a==inputPath b &&
  inputText a==inputText b && inputVersion a==inputVersion b && inputOffset a==inputOffset b &&
  inputFirstLine a==inputFirstLine b && inputNearby a==inputNearby b && inputHistory a==inputHistory b && inputRegions a==inputRegions b

description :: SupplierDescription
description=SupplierDescription "context check" InProcess (ReportedModel "context-model") Nothing 0

right :: Show a => Either a b -> IO b
right=either (error . show) pure

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)

barrier :: String -> IO a -> IO a
barrier label action=timeout 2000000 action >>= maybe (error label) pure
