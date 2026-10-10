{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.WindowRead
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : DeriveGeneric, OverloadedStrings
--
-- Bounded reads of explicitly readable prepared windows. The host pins the
-- exact installed body and owns admission, privacy and logical text projection.
module Hide.Plugin.WindowRead
  ( -- * Captured request service
    WindowReadServices(..)
    -- * Checked requests
  , ReadArguments
  , readArguments
  , wantedWindow
  , startLine
  , lineCount
  , readInput
    -- * Immutable reply
  , WindowPage(..)
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Hide.Plugin.BufferRead as B
import Hide.Plugin.Command (Codec(..),CommandError)

-- | Read a host-captured prepared window on the invoking worker. Every call
-- requests fresh permission and rechecks caller, privacy, session and exact body
-- identity. Changing a numeric selector cannot redirect this capability to a
-- replacement body or another window; neither IDs nor retained services grant
-- cached approval, live input or human authority.
--
-- The host captures the active window for an omitted selector before dispatch.
-- Closing or replacing that body before admission rejects the request. An
-- already returned immutable page remains usable after close; tool registration
-- lifetime is separately owned by the plugin registry.
data WindowReadServices = WindowReadServices
  { readWindow :: ReadArguments -> IO (Either CommandError WindowPage)
    -- ^ One bounded logical text page; source windows use buffer reads instead.
  }

-- | Checked logical line coordinates and an optional session-local window ID.
-- This value contains no body identity or capability; the host binds those.
data ReadArguments = ReadArguments !(Maybe Int) !Int !Int deriving (Eq,Show)

-- | /O(1)/. Accept a one-based first line and 1--1000 requested rows. Window
-- existence, readable representation and identity remain host checks.
--
-- @wantedWindow <$> readArguments target start count = Right target@
-- whenever the page coordinates satisfy those bounds.
readArguments :: Maybe Int -> Int -> Int -> Either Text ReadArguments
readArguments target start count
  | start>=1 && count>=1 && count<=1000=Right (ReadArguments target start count)
  | otherwise=Left "Use startLine >= 1 and lineCount 1..1000"

-- | /O(1)/. Explicit ID, or the host-captured active window when omitted.
wantedWindow :: ReadArguments -> Maybe Int
wantedWindow (ReadArguments target _ _)=target

-- | /O(1)/. One-based first logical line, at least one.
startLine :: ReadArguments -> Int
startLine (ReadArguments _ start _)=start

-- | /O(1)/. Requested logical rows, from one through 1000.
lineCount :: ReadArguments -> Int
lineCount (ReadArguments _ _ count)=count

-- | Strict request codec. Omitted/null coordinates default to line 1 and 200
-- rows. Unknown fields, including byteOffset, fail; windows expose text only.
--
-- @codecDecode readInput (codecEncode readInput arguments) = Right arguments@.
readInput :: Codec ReadArguments
readInput=Codec (object ["type" .= ("object"::Text),"additionalProperties" .= False,
  "required" .= ([]::[Text]),"properties" .= object
    ["windowId" .= integer [],
     "startLine" .= integer ["minimum" .= (1::Int),"default" .= (1::Int)],
     "lineCount" .= integer ["minimum" .= (1::Int),"maximum" .= (1000::Int),"default" .= (200::Int)]]])
  (either (Left . T.pack) Right . parseEither (withObject "read_window" $ \fields->do
    unless (all (`elem` ["windowId","startLine","lineCount"]) (KM.keys fields)) (fail "Unknown argument")
    target<-fields .:? "windowId"
    start<-fields .:? "startLine" .!= 1
    count<-fields .:? "lineCount" .!= 200
    either (fail . T.unpack) pure (readArguments target start count)))
  (\(ReadArguments target start count)->object
    (["windowId" .= ident | Just ident<-[target]]++["startLine" .= start,"lineCount" .= count]))
  where integer fields=object (["type" .= ("integer"::Text)]++fields)

-- | Host-masked metadata and at most 131072 characters from the same admitted
-- logical body. Coordinates are window-text lines, independent of the viewport;
-- a window page never invents a source buffer ID or exposes an editable tree.
-- The host forces this ordinary bounded value on the worker after projection.
data WindowPage = WindowPage
  { windowIdentifier :: !Int
  , windowTitle :: !Text
  , windowPage :: !B.TextPage
  } deriving (Eq,Show,Generic)

instance NFData WindowPage
