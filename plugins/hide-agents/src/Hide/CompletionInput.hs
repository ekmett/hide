{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.CompletionInput
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- The actual Send hint input for the persistent ACP completion conversation.
-- The host owns its draft and admitted provider; this plugin owns validation,
-- command execution and the decision to clear only after acknowledgement.
module Hide.CompletionInput (completionInput) where

import Data.Aeson (Value(Null))
import qualified Data.Text as T
import Hide.Plugin.Command
import Hide.Plugin.Completion (HintServices(..))
import Hide.Plugin.Input

-- | Both action slots send the same bounded hint. Rejection or failed delivery
-- preserves the draft; only successful provider acknowledgement requests Clear.
-- No source context or completion-proposal capability accompanies this input.
completionInput :: InputDeclaration HintServices
completionInput=InputDeclaration (EditorSpec False "Send hint" "Send hint") 16384
  (CommandDef "hide.autocomplete.hint" "Send autocomplete hint" hidden hidden run)
  (\_ text->Right text) (const ClearInput)
  where
    hidden=Codec Null (const (Left "Completion hints are host-captured.")) (const Null)
    run services text
      | T.null (T.strip text) || T.any (=='\0') text=
          pure (Left (InvalidArguments "Enter a hint without NUL characters."))
      | otherwise=fmap (either (Left . CommandRejected) Right) (sendHint services text)
