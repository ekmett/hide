{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Plugin.BufferDiff
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : DeriveGeneric, OverloadedStrings
--
-- Checked atomic diff batches and ordered replies. Exact content identity,
-- editable approval, source privacy and all-target adoption remain host-owned.
module Hide.Plugin.BufferDiff
  ( BufferDiffServices(..)
  , DiffEntry(..)
  , ApplyDiffArguments
  , applyDiffArguments
  , diffEntries
  , applyInput
  , entryInput
  , DiffReply(..)
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hide.Plugin.Command (Codec(..),CommandError)

-- | A host-bound exact-target request, supplied only after serialized preflight.
-- Invoke on a worker: the host owns fresh actor/policy admission, human-edited
-- approval and final version/privacy checks. Target count, order, IDs and
-- revisions cannot change; numeric revision alone is never an exact content
-- receipt. Retention grants no cached approval or human authority. Cancellation
-- and session shutdown resolve accepted requests.
--
-- Every target is adopted together or none is. Success adds ordinary Undo per
-- changed buffer and saves no file. Replies follow input order and report the
-- actual approved diffs, including any human corrections.
data BufferDiffServices = BufferDiffServices
  { applyDiff :: ApplyDiffArguments -> IO (Either CommandError [DiffReply])
  }

-- | One proposed strict patch. Selectors grant no authority: the host separately
-- captures every exact immutable target before worker dispatch. Construct a
-- checked batch with 'applyDiffArguments' before invoking the service.
data DiffEntry = DiffEntry
  { targetBuffer :: !Int -- ^ Session-local selector, not an editable capability.
  , expectedRevision :: !Int -- ^ Numeric revision, not a content identity.
  , diffText :: !Text -- ^ Proposed patch; approval may correct it.
  } deriving (Eq,Show)

-- | Nonempty, bounded batch with distinct targets in the caller's order.
newtype ApplyDiffArguments = ApplyDiffArguments [DiffEntry] deriving (Eq,Show)

-- | Validate 1–16 distinct targets and at most one MiB characters across all
-- patches. Target existence, revisions, editability, privacy and strict hunk
-- validation belong to the host. No source content is read here.
--
-- @diffEntries <$> applyDiffArguments entries = Right entries@
-- whenever the target and aggregate patch bounds hold.
applyDiffArguments :: [DiffEntry] -> Either Text ApplyDiffArguments
applyDiffArguments proposed
  | null entries || length entries>16=Left "Diff batch requires 1..16 targets"
  | length (nub (map targetBuffer entries))/=length entries=Left "Duplicate diff targets; no buffers changed"
  | sum [toInteger (T.length (diffText entry)) | entry<-entries]>1048576=Left "Diff batch exceeds 1 MiB characters"
  | otherwise=Right (ApplyDiffArguments entries)
  where entries=take 17 proposed

-- | /O(1)/. The checked entries in request order.
diffEntries :: ApplyDiffArguments -> [DiffEntry]
diffEntries (ApplyDiffArguments entries)=entries

-- | Canonical request shape, also used for a single target. Unknown fields fail;
-- typed and wire callers share the same batch validation.
--
-- @codecDecode applyInput (codecEncode applyInput arguments) = Right arguments@.
applyInput :: Codec ApplyDiffArguments
applyInput=Codec (object ["type" .= ("object"::Text),"additionalProperties" .= False,
  "required" .= (["buffers"]::[Text]),"properties" .= object
    ["buffers" .= object ["type" .= ("array"::Text),"minItems" .= (1::Int),
      "maxItems" .= (16::Int),"items" .= codecSchema entryInput]]])
  (either (Left . T.pack) Right . parseEither (withObject "buffer_apply_diff" $ \fields->do
    unless (KM.keys fields==["buffers"]) (fail "Expected only buffers")
    values<-fields .: "buffers"
    unless (not (null values) && length values<=16) (fail "Diff batch requires 1..16 targets")
    entries<-either (fail . T.unpack) pure (traverse (codecDecode entryInput) values)
    either (fail . T.unpack) pure (applyDiffArguments entries)))
  (\arguments->object ["buffers" .= map (codecEncode entryInput) (diffEntries arguments)])

-- | Strict per-target codec, shared by the batch wire format and host-owned
-- review attempts. Batch size, aggregate budget and uniqueness are checked by
-- 'applyInput'; this codec validates only one entry, without source IO.
--
-- @codecDecode entryInput (codecEncode entryInput entry) = Right entry@
-- whenever its patch fits the one-MiB-character bound.
entryInput :: Codec DiffEntry
entryInput=Codec (object ["type" .= ("object"::Text),"additionalProperties" .= False,
  "required" .= (["bufferId","revision","diff"]::[Text]),"properties" .= object
    ["bufferId" .= integer,"revision" .= integer,
     "diff" .= object ["type" .= ("string"::Text),"maxLength" .= (1048576::Int)]]])
  (either (Left . T.pack) Right . parseEither (withObject "buffer diff entry" $ \fields->do
    unless (all (`elem` ["bufferId","revision","diff"]) (KM.keys fields)) (fail "Unknown argument")
    target<-fields .: "bufferId"
    revision<-fields .: "revision"
    patch<-fields .: "diff"
    unless (T.length patch<=1048576) (fail "Diff exceeds 1 MiB characters")
    pure (DiffEntry target revision patch)))
  (\entry->object ["bufferId" .= targetBuffer entry,"revision" .= expectedRevision entry,"diff" .= diffText entry])
  where integer=object ["type" .= ("integer"::Text)]

-- | Host-issued outcome for one bound target. Revision is the committed revision;
-- appliedDiff is the exact approved patch and userModified records a human edit.
-- This bounded value is forced on the reply worker, never by the UI owner.
data DiffReply = DiffReply
  { editedBuffer :: !Int
  , editedRevision :: !Int
  , appliedDiff :: !Text
  , userModified :: !Bool
  } deriving (Eq,Show,Generic)

instance NFData DiffReply
