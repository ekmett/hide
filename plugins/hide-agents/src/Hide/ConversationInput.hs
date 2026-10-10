{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.ConversationInput
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- The actual primary and child Query/Steer commands. The host binds each slot
-- and exact target; this plugin executes its acknowledged service and chooses
-- to clear the original submission only after success.
module Hide.ConversationInput (primaryInput, childInput) where

import Data.Aeson (Value(Null))
import qualified Data.Text as T
import Hide.Plugin.Command
import Hide.Plugin.ConversationInput (PrimaryInputServices(..), ChildInputServices(..))
import Hide.Plugin.Input

-- | Both slots invoke their host-captured operation. Blank/NUL input is refused;
-- failed enqueue or steering preserves the draft. Only acknowledgement clears.
-- Normalization removes at most four indentation characters per code row; each
-- such row retains a newline in the fenced result. Thus a normalized message of
-- at most 65536 characters needs no more than 5*65536 raw input characters.
-- The Hub still enforces its existing 65536-character normalized message limit.
childInput :: InputDeclaration ChildInputServices
childInput=InputDeclaration (EditorSpec True "Query" "Steer") (5*65536)
  (CommandDef "hide.conversation.child.submit" "Submit child conversation input" hidden hidden run)
  (\_ text->Right text) (const ClearInput)
  where
    hidden=Codec Null (const (Left "Conversation input is host-captured.")) (const Null)
    run services text
      | T.null (T.strip text) || T.any (=='\0') text=
          pure (Left (InvalidArguments "Enter a conversation message without NUL characters."))
      | otherwise=fmap (either (Left . CommandRejected) Right) (submitInput services text)

-- | Primary input preserves the host's existing Int-sized text domain and
-- blank-only validation. The host-bound operation owns queue/provider admission;
-- only its acknowledgement clears the original draft. No child message bound is
-- imposed on the primary composer.
primaryInput :: InputDeclaration PrimaryInputServices
primaryInput=InputDeclaration (EditorSpec True "Query" "Steer") maxBound
  (CommandDef "hide.conversation.primary.submit" "Submit primary conversation input" hidden hidden run)
  (\_ text->Right text) (const ClearInput)
  where
    hidden=Codec Null (const (Left "Conversation input is host-captured.")) (const Null)
    run services text
      | T.null (T.strip text)=pure (Left (InvalidArguments "Enter a conversation message."))
      | otherwise=fmap (either (Left . CommandRejected) Right) (submitPrimaryInput services text)
