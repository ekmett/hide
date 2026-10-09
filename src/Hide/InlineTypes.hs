{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.InlineTypes
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Shared completion-provider inputs, proposals and feedback.
--
-- Replacement coordinates use zero-based Unicode-character offsets, not UTF-16
-- or screen cells. Adapters translate their wire coordinates; the background
-- owner prepares source/history before sending. Provider metadata remains opaque
-- so acceptance can be reported without interpreting service-specific tokens.
module Hide.InlineTypes where

import Data.Text (Text)
import Data.Aeson (Value)
-- | Immutable file, caret, revision, nearby context and recent-edit snapshot.
data CompletionInput = CompletionInput
  { inputId :: Text, inputIntent :: Text, inputPath :: FilePath, inputText :: Text
  , inputVersion :: Int, inputOffset :: Int
  , inputFirstLine :: Int, inputNearby :: [Text], inputHistory :: Value
  }
-- | Half-open character replacement range, new text and optional provider metadata.
data Proposal = Proposal
  { proposalStart :: Int, proposalEnd :: Int, proposalText :: Text
  , proposalData :: Maybe Value
  } deriving (Eq,Show)
-- | Acceptance outcome; partial counts are cumulative in the normalized original
-- insertion text, including matching prefixes removed from the visible proposal.
-- Copilot maps this count back through original line endings and UTF-16.
data CompletionFeedback = Shown | Accepted | Ignored | PartiallyAccepted Int
-- | Device-flow code and validated opaque command; credentials remain provider-owned.
data CopilotSignIn = CopilotSignIn
  { signInCode :: Text, signInCommand :: Value } deriving (Eq,Show)
