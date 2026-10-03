{-# LANGUAGE OverloadedStrings #-}
module Hide.InlineTypes where

import Data.Text (Text)
import Data.Aeson (Value)
-- Contract shared with backend implementers; all offsets/columns are Unicode
-- code points (Haskell Text API units), not UTF-16. Backends convert as needed.
-- CompletionInput values are forced on the background worker before sending.
data CompletionInput = CompletionInput
  { inputId :: Text, inputIntent :: Text, inputPath :: FilePath, inputText :: Text
  , inputVersion :: Int, inputOffset :: Int
  , inputFirstLine :: Int, inputNearby :: [Text], inputHistory :: Value
  }
data Proposal = Proposal
  { proposalStart :: Int, proposalEnd :: Int, proposalText :: Text
  , proposalData :: Maybe Value
  } deriving (Eq,Show)
data CompletionFeedback = Shown | Accepted | Ignored | PartiallyAccepted Int
-- PartiallyAccepted counts Unicode characters cumulatively in the original
-- insertText after editor newline normalization, including any matching prefix
-- trimmed from the displayed proposal. Copilot maps that count back to the
-- original wire text and UTF-16. proposalData retains the full opaque item.
data CopilotSignIn = CopilotSignIn
  { signInCode :: Text, signInCommand :: Value } deriving (Eq,Show)
