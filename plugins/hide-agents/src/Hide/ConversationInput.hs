{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.ConversationInput
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- The actual child Query/Steer input command. The host binds the selected slot
-- and exact target; this plugin executes its acknowledged service and chooses
-- to clear the original submission only after success.
module Hide.ConversationInput (childInput) where

import Data.Aeson (Value(Null))
import qualified Data.Text as T
import Hide.Plugin.Command
import Hide.Plugin.ConversationInput (ChildInputServices(..))
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
