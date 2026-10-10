{-# LANGUAGE RankNTypes #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.SystemOne
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : RankNTypes
--
-- Small decisions over immutable, already-authorized text. A result is evidence,
-- never an editor action or permission. The host owns admission and one physical
-- inference slot; a linked provider owns its model, tokenizer and scratch space.
module Hide.Plugin.SystemOne
  ( DecisionInput(..)
  , DecisionLocality(..)
  , DecisionQuestion(..)
  , DecisionKind(..)
  , DecisionOption(..)
  , DecisionAnswer(..)
  , DecisionAnswerKind(..)
  , DecisionOutput(..)
  , DecisionUsage(..)
  , ModelIdentity(..)
  , SupplierLocation(..)
  , SupplierDescription(..)
  , DecisionSupplier(..)
  , DecisionFailure(..)
  , DecisionResult(..)
  , DecisionTicket(..)
  , SystemOneServices(..)
  , DecisionProvider(..)
  , DecisionDriver(..)
  ) where

import Control.Concurrent.STM (STM)
import Data.Text (Text)
import Data.Word (Word64)

-- | An immutable snapshot chosen and redacted by the consumer before dispatch.
-- The state identity is an opaque consumer revision, not a file or buffer read
-- capability. Neither state nor question text may be silently truncated.
data DecisionInput = DecisionInput
  { decisionStateId :: !Text
  , decisionState :: !Text
  , decisionQuestions :: ![DecisionQuestion]
  , decisionLocality :: !DecisionLocality
  } deriving (Eq,Show)

-- | A loopback endpoint and an attached browser are separate destinations too.
-- 'SelectedSupplier' permits only the exact supplier captured at submission;
-- there is no fallback to a different destination.
data DecisionLocality = HostProcessOnly | SelectedSupplier deriving (Eq,Show)

-- | A distinct name, instructions and ordered answer domain. Names identify
-- results; returned answers and distributions always follow submitted order.
data DecisionQuestion = DecisionQuestion
  { questionName :: !Text
  , questionInstructions :: !Text
  , questionKind :: !DecisionKind
  } deriving (Eq,Show)

-- | Binary probabilities are ordered @[false,true]@, with the supplied text
-- describing those two outcomes. Choice options and score levels retain their
-- submitted order. A score's expectation uses zero-based level indices.
data DecisionKind
  = BinaryDecision !Text !Text
  | ChoiceDecision ![DecisionOption]
  | ScoreDecision ![Text]
  deriving (Eq,Show)

-- | A distinct human-readable label and its criterion. Labels are model input,
-- not replaceable transport IDs. An endpoint may canonicalize JSON object order;
-- the adapter restores returned probabilities to the submitted label order.
data DecisionOption = DecisionOption
  { optionLabel :: !Text
  , optionCriterion :: !Text
  } deriving (Eq,Show)

-- | Question identity, kind and distribution, in submitted order. Confidence
-- is attributed to this provider; it is not calibrated across providers.
-- Probabilities are retained as returned, without hidden renormalization.
data DecisionAnswer = DecisionAnswer
  { answerQuestion :: !Text
  , answerKind :: !DecisionAnswerKind
  , answerProbabilities :: ![Double]
  , answerConfidence :: !(Maybe Double)
  } deriving (Eq,Show)

-- | The three answer domains. The host checks the kind as well as dimensions.
data DecisionAnswerKind = BinaryAnswer | ChoiceAnswer | ScoreAnswer deriving (Eq,Show)

-- | Data returned by a driver. The host validates it against the exact input
-- and supplier before accepting a terminal result. This is not a tool call.
data DecisionOutput = DecisionOutput
  { outputModel :: !ModelIdentity
  , outputAnswers :: ![DecisionAnswer]
  , outputUsage :: !(Maybe DecisionUsage)
  } deriving (Eq,Show)

-- | Optional provider accounting. Output tokens can describe serialized scores;
-- they do not imply that the model generated text. Counts must be nonnegative.
data DecisionUsage = DecisionUsage
  { decisionInputTokens :: !(Maybe Int)
  , decisionOutputTokens :: !(Maybe Int)
  } deriving (Eq,Show)

-- | 'PinnedArtifact' identifies the manifest verified by the loading adapter.
-- It is not remote execution attestation. 'ReportedModel' is a provider's name;
-- matching an endpoint's echo does not establish which weights executed.
data ModelIdentity = PinnedArtifact !Text | ReportedModel !Text deriving (Eq,Show)

-- | Where the selected supplier executes. Endpoint text is a public URL without
-- credentials; browser text is its connection label, not a session access key.
data SupplierLocation = InProcess | SystemOneEndpoint !Text | BrowserWorker !Text deriving (Eq,Show)

-- | Public provenance and declared allocation envelope. A native adapter checks
-- model/tensor allocations against its budget. This is not a total-process RSS
-- cap: runtime overhead is measured separately. The host cannot meter a remote
-- server, which reports no local allocation limit.
-- Probability error is the maximum absolute rounding error per serialized
-- probability (e.g. 0.00005 for Kev's four decimal places). It must be finite,
-- nonnegative and no greater than 0.00005. Sum validation accounts for the number
-- of options; it never changes the distribution to force the sum to one.
data SupplierDescription = SupplierDescription
  { supplierLabel :: !Text
  , supplierLocation :: !SupplierLocation
  , supplierModel :: !ModelIdentity
  , supplierAllocationLimit :: !(Maybe Word64)
  , supplierProbabilityError :: !Double
  } deriving (Eq,Show)

-- | Host-issued supplier incarnation. Retain this identity with an immutable
-- input: selecting another supplier expires it, even if labels/model names match.
data DecisionSupplier = DecisionSupplier
  { decisionSupplierId :: !Text
  , decisionSupplierDescription :: !SupplierDescription
  } deriving (Eq,Show)

-- | Failures are terminal data. Diagnostic text must not include submitted
-- state, credentials or raw transport exceptions.
data DecisionFailure
  = DecisionUnavailable
  | DecisionBusy
  | DecisionExpired
  | DecisionCancelled
  | DecisionDeadline
  | DecisionClosed
  | DecisionInvalid !Text
  | DecisionProviderFailed !Text
  deriving (Eq,Show)

-- | A successful result names its exact supplier and the consumer's state.
-- Consumers still own freshness, policy and any subsequent editor operation.
data DecisionResult = DecisionResult
  { resultSupplier :: !DecisionSupplier
  , resultStateId :: !Text
  , resultOutput :: !DecisionOutput
  } deriving (Eq,Show)

-- | One immutable terminal receipt. Repeated awaits/polls return the same value.
-- Cancellation that wins before completion publishes 'DecisionCancelled'; after
-- a terminal value it returns 'False' without rewriting that value. Completion,
-- timeout, replacement and shutdown compete for this same first transition.
-- Public cancellation does not release the physical slot until cleanup drains.
data DecisionTicket = DecisionTicket
  { awaitDecision :: IO (Either DecisionFailure DecisionResult)
  , pollDecision :: IO (Maybe (Either DecisionFailure DecisionResult))
  , cancelDecision :: IO Bool
  }

-- | One session's data-only service. Admission is bounded and does not wait for
-- inference or acquire a model. Busy calls are refused, not queued. The caller
-- supplies the previously captured supplier ID and a deadline in milliseconds.
-- Payload preparation/admission belongs on a consumer worker, never the UI lock.
-- The service does not select suppliers or expose human configuration authority.
data SystemOneServices = SystemOneServices
  { currentDecisionSupplier :: IO (Maybe DecisionSupplier)
  , requestDecision :: Text -> DecisionInput -> Int -> IO (Either DecisionFailure DecisionTicket)
  }

-- | A linked, scoped supplier. The host acquires it lazily on its sole inference
-- worker and retains the scope for consecutive requests to this supplier. The
-- STM flag becomes true on retirement/shutdown: acquisition and idle resource
-- ownership must respond to it. Scope exit releases all resources, including
-- partial acquisition, and joins owned work. A replacement is never acquired
-- before the old scope has drained. No editor callback crosses this interface.
data DecisionProvider = DecisionProvider
  { decisionProviderDescription :: !SupplierDescription
  , withDecisionDriver :: forall a. STM Bool -> (DecisionDriver -> IO a) -> IO a
  }

-- | One invocation on the owner worker. The cancellation flag also covers its
-- deadline and supplier retirement. Return only after inference and its cleanup
-- have stopped using request resources. Cancellation can retire local adoption
-- without proving that a remote endpoint stopped computing. Providers must check
-- tokenizer/context budgets before dispatch and explicitly refuse truncation.
newtype DecisionDriver = DecisionDriver
  { runDecision :: DecisionInput -> STM Bool -> IO (Either DecisionFailure DecisionOutput) }
