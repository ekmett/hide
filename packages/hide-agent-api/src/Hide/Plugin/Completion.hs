{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.Completion
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings, RankNTypes
--
-- Immutable completion snapshots and the scoped private-provider boundary.
-- No buffer, undo history, editor state or edit authority crosses this API.
module Hide.Plugin.Completion
  ( CompletionInput(..)
  , CompletionEdit(..)
  , CompletionContext(..)
  , completionContextValue
  , completionEditsValue
  , Proposal(..)
  , CompletionFeedback(..)
  , LineReplacement(..)
  , CompletionChunk(..)
  , CompletionServices(..)
  , CompletionStart(..)
  , CompletionProvider(..)
  , CompletionDriver(..)
  , HintServices(..)
  ) where

import Data.Aeson (Value,object,toJSON,(.=))
import Data.Text (Text)
import Hide.Plugin.Agent (ConfigChoice)
import Hide.Plugin.Provider (ProviderLaunch)

-- | Worker-prepared immutable file, caret, revision, local context and recent
-- edits. Offsets count Unicode characters, not UTF-16 or screen cells. Only the
-- host retains the source identity and decides whether a result remains current.
data CompletionInput = CompletionInput
  { inputId :: !Text, inputIntent :: !Text, inputPath :: !FilePath, inputText :: !Text
  , inputVersion :: !Int, inputOffset :: !Int
  , inputFirstLine :: !Int, inputNearby :: ![Text], inputHistory :: ![CompletionEdit]
  }

-- | One bounded recent change, prepared from the source owner's history on its
-- worker. It grants neither access to the undo tree nor authority to undo.
data CompletionEdit = CompletionEdit
  { editStartOffset :: !Int, editOldText :: !Text, editNewText :: !Text }
  deriving (Eq,Show)

-- | Local snapshot exposed by the private tool route. It contains no whole-file
-- source; explicit file reads are separately limited to 8192 characters.
data CompletionContext = CompletionContext
  { contextRequestId :: !Text, contextIntent :: !Text, contextPath :: !FilePath
  , contextRevision :: !Int, contextOffset :: !Int, contextLine :: !Int, contextColumn :: !Int
  , contextFirstLine :: !Int, contextLines :: ![Text], contextRecentEdits :: ![CompletionEdit]
  } deriving (Eq,Show)

-- | The same concrete snapshot encoding is used in the provider prompt and the
-- read-context tool reply, so the two views cannot disagree about coordinates.
completionContextValue :: CompletionContext -> Value
completionContextValue context=object
  ["requestId" .= contextRequestId context,"intent" .= contextIntent context,"path" .= contextPath context
  ,"revision" .= contextRevision context
  ,"caret" .= object ["offset" .= contextOffset context,"line" .= contextLine context,"column" .= contextColumn context]
  ,"firstLine" .= contextFirstLine context,"endLine" .= (contextFirstLine context+length (contextLines context))
  ,"lines" .= [object ["line" .= number,"text" .= text] | (number,text)<-zip [contextFirstLine context..] (contextLines context)]
  ,"recentEdits" .= completionEditsValue (contextRecentEdits context)]

-- | Encode the concrete recent-edit payload for its existing 32 KiB wire limit.
completionEditsValue :: [CompletionEdit] -> Value
completionEditsValue edits=toJSON [object ["startOffset" .= editStartOffset edit,"oldText" .= editOldText edit,"newText" .= editNewText edit] | edit<-edits]

-- | Half-open character replacement range, text and optional provider metadata.
-- Metadata is opaque to the host and returned only for provider feedback.
data Proposal = Proposal
  { proposalStart :: !Int, proposalEnd :: !Int, proposalText :: !Text
  , proposalData :: !(Maybe Value)
  } deriving (Eq,Show)

-- | Partial counts are cumulative in the normalized original insertion text,
-- including matching prefixes removed from the preview. Providers translate
-- their own wire units; feedback never grants permission to apply another edit.
data CompletionFeedback = Shown | Accepted | Ignored | PartiallyAccepted !Int
  deriving (Eq,Show)

-- | Whole-line proposal coordinates: zero-based, half-open, within the supplied
-- local context. Equal boundaries insert. The service checks all alternatives
-- together before consuming its single submission slot.
data LineReplacement = LineReplacement
  { replacementStartLine :: !Int, replacementEndLine :: !Int, replacementText :: !Text }
  deriving (Eq,Show)

-- | A bounded page of the immutable current file; no arbitrary path is accepted.
data CompletionChunk = CompletionChunk
  { chunkRequestId :: !Text, chunkStartOffset :: !Int, chunkText :: !Text
  , chunkNextOffset :: !Int, chunkEOF :: !Bool
  } deriving (Eq,Show)

-- | Self-admitting services exclusive to the authenticated completion endpoint.
-- Every operation checks the exact active request ID under the provider's
-- existing submission lock. Idle, hint, completed, cancelled and closed requests
-- reject access. Retention grants no authority over a later request or source.
-- Submission succeeds at most once, with at most eight alternatives and a total
-- of 128 KiB UTF-8 replacement text. Only the host can adopt or apply a proposal.
data CompletionServices = CompletionServices
  { readCompletionContext :: Text -> IO (Either Text CompletionContext)
  , readCompletionFile :: Text -> Int -> Int -> IO (Either Text CompletionChunk)
  , readCompletionSkill :: Text -> IO (Either Text Text)
  , submitCompletion :: Text -> [LineReplacement] -> IO (Either Text Int)
  }

-- | Host-owned acquisition inputs. The launch callback runs on the provider
-- worker for each actual lazy client acquisition, returning the complete frozen
-- environment and its private values. It must not capture Desktop or Buffer.
-- Explicit private values remain redacted even before/after a live connection.
-- MCP server values describe only the supplied private endpoint, never a grant
-- of general editor, filesystem, terminal or permission services.
data CompletionStart = CompletionStart
  { completionAcquireLaunch :: IO (ProviderLaunch,[Text])
  , completionPrivateValues :: ![Text]
  , completionDirectory :: !FilePath
  , completionServers :: ![Value]
  , completionModel :: !(Maybe Text)
  , completionEffort :: !(Maybe Text)
  }

-- | Scope one lazily acquired private provider on the existing owner worker.
-- Scope exit invalidates its tool slot, stops the client and joins outstanding
-- provider work before returning. Acquisition failure releases partial resources.
-- An absent contribution is represented by 'Nothing'; the host starts no fallback.
newtype CompletionProvider = CompletionProvider
  { withCompletionProvider :: forall a. CompletionStart -> (CompletionDriver -> IO a) -> IO a }

-- | Operations on one provider lifetime. The host serializes complete, hint,
-- discovery and configuration on its existing worker; snapshot tools and trace
-- draining may run concurrently. Configuration receipts identify the exact
-- client incarnation and version: stale receipts cannot acquire a replacement.
-- Only a matching submitted proposal followed by a completed provider turn may
-- become a completion result. Ordinary streamed text never becomes an edit.
data CompletionDriver = CompletionDriver
  { requestCompletion :: CompletionInput -> IO [Proposal]
  , reportCompletion :: CompletionFeedback -> Proposal -> IO ()
  , sendCompletionHint :: Text -> IO ()
  , completionConfiguration :: IO (Maybe ((Int,Int),[ConfigChoice]))
  , discoverCompletionConfiguration :: IO ()
  , configureCompletionAt :: (Int,Int) -> Text -> Text -> IO (Either Text ())
  , pollCompletionTranscript :: IO [Text]
  , completionServices :: !CompletionServices
  }

-- | Supplied to an admitted input command on the existing completion worker.
-- The host validates the original human input, mount and provider/configuration
-- receipt before supplying this context; it cannot be used to select a different
-- provider, expose source snapshots or submit a completion proposal.
-- A successful return acknowledges the hint turn. Failure leaves the input
-- available for correction. Each call rechecks the captured target and invocation
-- lifetime. Calls after command completion or provider/configuration replacement
-- are rejected; an admitted call may drain normally. This service starts no new
-- worker and does not bypass the completion owner's existing queue.
newtype HintServices = HintServices
  { sendHint :: Text -> IO (Either Text ()) }
