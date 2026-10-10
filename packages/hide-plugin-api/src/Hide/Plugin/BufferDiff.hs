{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Plugin.BufferDiff
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : DeriveGeneric, OverloadedStrings
--
-- Checked strict-diff requests and bounded replies. Exact content identity,
-- editable approval, source privacy and atomic adoption remain host-owned.
module Hide.Plugin.BufferDiff
  ( BufferDiffServices(..)
  , ApplyDiffArguments
  , applyDiffArguments
  , targetBuffer
  , expectedRevision
  , diffText
  , applyInput
  , DiffReply(..)
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hide.Plugin.Command (Codec(..),CommandError)

-- | A host-bound exact-target request, supplied only after serialized preflight.
-- Invoke on a worker: the host owns fresh actor/policy admission, human-edited
-- approval and final version/privacy checks. Different target or revision
-- arguments must be rejected rather than recaptured; numeric revision alone is
-- never an exact content receipt. Retention grants no cached approval or human
-- authority. Cancellation and session shutdown resolve accepted requests.
--
-- A successful edit uses ordinary Undo and saves no file. The
-- reply reports the actual approved diff, including any human corrections.
data BufferDiffServices = BufferDiffServices
  { applyDiff :: ApplyDiffArguments -> IO (Either CommandError DiffReply)
  }

-- | Checked wire request. The integer selector and revision grant no authority;
-- the host separately captures the exact immutable target before worker dispatch.
data ApplyDiffArguments = ApplyDiffArguments !Int !Int !Text deriving (Eq,Show)

-- | Validate the existing one-MiB-character patch bound. Target existence,
-- numeric revision, editability, privacy and strict hunk validation belong to
-- the host. No source content is read by this constructor.
--
-- @diffText <$> applyDiffArguments target revision patch = Right patch@
-- whenever the patch fits the bound.
applyDiffArguments :: Int -> Int -> Text -> Either Text ApplyDiffArguments
applyDiffArguments target revision patch
  | T.length patch<=1048576=Right (ApplyDiffArguments target revision patch)
  | otherwise=Left "Diff exceeds 1 MiB characters"

-- | /O(1)/. Session-local target selector, not an editable buffer capability.
targetBuffer :: ApplyDiffArguments -> Int
targetBuffer (ApplyDiffArguments target _ _)=target

-- | /O(1)/. Expected numeric revision, not an exact immutable-content identity.
expectedRevision :: ApplyDiffArguments -> Int
expectedRevision (ApplyDiffArguments _ revision _)=revision

-- | /O(1)/. Proposed patch. Approval may produce a different applied diff.
diffText :: ApplyDiffArguments -> Text
diffText (ApplyDiffArguments _ _ patch)=patch

-- | Strict existing request shape with all three fields required. Unknown
-- fields fail; the same checked constructor serves typed and wire callers.
--
-- @codecDecode applyInput (codecEncode applyInput arguments) = Right arguments@.
applyInput :: Codec ApplyDiffArguments
applyInput=Codec (object ["type" .= ("object"::Text),"additionalProperties" .= False,
  "required" .= (["bufferId","revision","diff"]::[Text]),"properties" .= object
    ["bufferId" .= integer,"revision" .= integer,
     "diff" .= object ["type" .= ("string"::Text),"maxLength" .= (1048576::Int)]]])
  (either (Left . T.pack) Right . parseEither (withObject "buffer_apply_diff" $ \fields->do
    unless (all (`elem` ["bufferId","revision","diff"]) (KM.keys fields)) (fail "Unknown argument")
    target<-fields .: "bufferId"
    revision<-fields .: "revision"
    patch<-fields .: "diff"
    either (fail . T.unpack) pure (applyDiffArguments target revision patch)))
  (\(ApplyDiffArguments target revision patch)->object
    ["bufferId" .= target,"revision" .= revision,"diff" .= patch])
  where integer=object ["type" .= ("integer"::Text)]

-- | Host-issued outcome for the bound target. Revision is the committed revision;
-- appliedDiff is the exact approved patch and userModified records a human edit.
-- This bounded value is forced on the reply worker, never by the UI owner.
data DiffReply = DiffReply
  { editedBuffer :: !Int
  , editedRevision :: !Int
  , appliedDiff :: !Text
  , userModified :: !Bool
  } deriving (Eq,Show,Generic)

instance NFData DiffReply
