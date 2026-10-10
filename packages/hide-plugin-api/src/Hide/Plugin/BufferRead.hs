{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Plugin.BufferRead
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : DeriveGeneric, OverloadedStrings
--
-- Bounded reads of open editor buffers. The host owns capture, measured paging,
-- privacy and admission; a page exposes neither an editable buffer nor its Undo.
module Hide.Plugin.BufferRead
  ( -- * Session services
    BufferReadServices(..)
    -- * Checked requests
  , ReadArguments
  , readArguments
  , wantedBuffer
  , startLine
  , lineCount
  , byteOffset
  , listInput
  , readInput
    -- * Immutable replies
  , BufferMetadata(..)
  , BufferPage(..)
  , TextPage(..)
  , BytePage(..)
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hide.Plugin.Command (Codec(..),CommandError)

-- | Session/actor-bound operations that request fresh host permission for every
-- invocation. Invoke on a worker: either operation can wait for human approval.
-- Unlike editor services supplied after admission, these capabilities self-admit;
-- wrapping them in another permission call would change approval ownership.
--
-- Retention grants no cached approval or human authority. The host rechecks
-- policy, caller liveness, session and privacy at capture; a closed session reader
-- cannot capture again. The plugin tool registry separately owns tool lifetime.
-- An already returned immutable page survives source close.
data BufferReadServices = BufferReadServices
  { bufferList :: IO (Either CommandError [BufferMetadata])
    -- ^ Masked discovery metadata. Listing never grants content-read authority.
  , bufferRead :: ReadArguments -> IO (Either CommandError BufferPage)
    -- ^ One bounded page. The host binds omitted selection to the active source
    -- at dispatch; explicit numeric IDs are resolved only in this service's session.
  }

-- | Checked page coordinates and an optional session-local buffer selector.
-- Buffer existence and identity are host checks, not properties of an integer.
data ReadArguments = ReadArguments !(Maybe Int) !Int !Int !Int deriving (Eq,Show)

-- | /O(1)/. Accept a one-based starting line, 1--1000 requested lines, and a
-- nonnegative byte offset. Byte buffers use the offset; text buffers use lines.
--
-- @wantedBuffer <$> readArguments target start count offset = Right target@
-- when the coordinates satisfy those bounds. The wire defaults are 1, 200 and 0.
readArguments :: Maybe Int -> Int -> Int -> Int -> Either Text ReadArguments
readArguments target start count offset
  | start>=1 && count>=1 && count<=1000 && offset>=0=Right (ReadArguments target start count offset)
  | otherwise=Left "Use startLine >= 1, lineCount 1..1000, and byteOffset >= 0"

-- | /O(1)/. Explicit buffer ID, or the host-bound active source when omitted.
wantedBuffer :: ReadArguments -> Maybe Int
wantedBuffer (ReadArguments target _ _ _)=target

-- | /O(1)/. One-based first line, at least one.
startLine :: ReadArguments -> Int
startLine (ReadArguments _ start _ _)=start

-- | /O(1)/. Requested text rows, from one through 1000.
lineCount :: ReadArguments -> Int
lineCount (ReadArguments _ _ count _)=count

-- | /O(1)/. Nonnegative requested offset for a byte buffer.
byteOffset :: ReadArguments -> Int
byteOffset (ReadArguments _ _ _ offset)=offset

-- | Strict empty discovery request. The service still performs its own admission.
listInput :: Codec ()
listInput=Codec (objectSchema [])
  (\value->case value of Object fields | KM.null fields->Right (); _->Left "Buffer listing accepts an empty object")
  (const (object []))

-- | Strict checked request codec, sharing 'readArguments' with typed callers.
-- Omitted/null coordinates retain the established defaults; unknown fields fail.
--
-- @codecDecode readInput (codecEncode readInput arguments) = Right arguments@.
readInput :: Codec ReadArguments
readInput=Codec (objectSchema
  [("bufferId",integer []),("startLine",integer ["minimum" .= (1::Int),"default" .= (1::Int)]),
   ("lineCount",integer ["minimum" .= (1::Int),"maximum" .= (1000::Int),"default" .= (200::Int)]),
   ("byteOffset",integer ["minimum" .= (0::Int),"default" .= (0::Int)])])
  (either (Left . T.pack) Right . parseEither (withObject "read_buffer" $ \fields->do
    unless (all (`elem` ["bufferId","startLine","lineCount","byteOffset"]) (KM.keys fields)) (fail "Unknown argument")
    target<-fields .:? "bufferId"
    start<-fields .:? "startLine" .!= 1
    count<-fields .:? "lineCount" .!= 200
    offset<-fields .:? "byteOffset" .!= 0
    either (fail . T.unpack) pure (readArguments target start count offset)))
  (\(ReadArguments target start count offset)->object
    (["bufferId" .= ident | Just ident<-[target]]++["startLine" .= start,"lineCount" .= count,"byteOffset" .= offset]))
  where integer fields=object (["type" .= ("integer"::Text)]++fields)

objectSchema :: [(Key,Value)] -> Value
objectSchema fields=object ["type" .= ("object"::Text),"required" .= ([]::[Text]),
  "additionalProperties" .= False,"properties" .= Object (KM.fromList fields)]

-- | Host-masked metadata from the same capture as a page, or a separate admitted
-- listing. A path is descriptive metadata, not filesystem access. Numeric revision
-- is not an edit receipt or an exact immutable-content identity.
--
-- The modified flag stays lazy because representation changes can require a
-- dirty comparison; the host captures its narrow inputs and evaluates on a worker.
data BufferMetadata = BufferMetadata
  { bufferIdentifier :: !Int
  , bufferTitle :: !Text
  , bufferPath :: !(Maybe FilePath)
  , bufferModified :: Bool
  , bufferBinary :: !Bool
  , bufferRevision :: !Int
  } deriving (Eq,Show,Generic)

instance NFData BufferMetadata

-- | An ordinary bounded reply, not a reusable source capture. The host constructs
-- the matching representation and metadata after admission and privacy checks.
data BufferPage
  = TextBufferPage !BufferMetadata !TextPage
  | ByteBufferPage !BufferMetadata !BytePage
  deriving (Eq,Show,Generic)

instance NFData BufferPage

-- | At most 131072 Unicode characters, with row endings normalized to LF. The
-- row count describes requested available rows (at most 1000), even when the
-- character cap truncates an earlier row. Masking is applied before disclosure.
data TextPage = TextPage
  { pageStartLine :: !Int
  , pageLineCount :: !Int
  , pageTotalLines :: !Int
  , pageText :: !Text
  , pageRedacted :: !Bool
  , pageTruncated :: !Bool
  } deriving (Eq,Show,Generic)

instance NFData TextPage

-- | At most 4096 original bytes. The requested offset is retained even past EOF;
-- the host clamps the actual read to its cached byte length and returns no bytes.
data BytePage = BytePage
  { pageByteOffset :: !Int
  , pageBytes :: !ByteString
  , pageTotalBytes :: !Int
  } deriving (Eq,Show,Generic)

instance NFData BytePage
