-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.Terminal
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Typed access to session-owned terminals. Every call admits its own fresh
-- permission and caller check; retaining this capability extends no lifetime.
-- A launch is approved before process preparation, then its same request owns
-- final admission and adoption or closes the unadopted process. Cancellation
-- cannot undo input or other side effects already admitted on their worker.
module Hide.Plugin.Terminal
  ( TerminalId(..)
  , TerminalLaunch(..)
  , TerminalOpened(..)
  , TerminalSummary(..)
  , TerminalListing(..)
  , TerminalPage(..)
  , TerminalServices(..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

-- | Session-local identity, not authority. Released or unknown IDs are refused.
newtype TerminalId = TerminalId { terminalIdText :: Text } deriving (Eq,Show)

-- | Direct executable and argv, with no implicit shell. An absent directory
-- uses the host's captured project context. Retention is bounded to 0..16 MiB;
-- launch strings and argument count together are bounded to 1 MiB.
data TerminalLaunch = TerminalLaunch
  { terminalCommand :: !Text
  , terminalArguments :: ![Text]
  , terminalDirectory :: !(Maybe FilePath)
  , terminalOutputByteLimit :: !Int
  } deriving (Eq,Show)

-- | A process adopted once into the existing console owner and editor view.
data TerminalOpened = TerminalOpened
  { openedTerminal :: !TerminalId, openedTerminalBuffer :: !Int } deriving (Eq,Show)

-- | Small facts for a live or retained terminal; closing a view keeps its ID.
data TerminalSummary = TerminalSummary
  { summaryTerminal :: !TerminalId, summaryTerminalBuffer :: !Int
  , summaryTerminalExitCode :: !(Maybe Int) } deriving (Eq,Show)

-- | Native backend availability and the session's shared console entries.
data TerminalListing = TerminalListing
  { terminalsAvailable :: !Bool, terminalEntries :: ![TerminalSummary] } deriving (Eq,Show)

-- | Bounded retained byte page. Offsets count bytes relative to the retained
-- tail, never Unicode characters. The plugin owns decoding and presentation.
data TerminalPage = TerminalPage
  { pageTerminal :: !TerminalId, terminalPageOffset :: !Int
  , terminalRetainedBytes :: !Int, terminalTruncated :: !Bool
  , terminalExitCode :: !(Maybe Int), terminalPageBytes :: !ByteString
  } deriving (Eq,Show)

-- | Self-admitting worker calls. There is one permission request per call:
-- initial launch approval precedes preparation; fresh final policy/caller
-- admission uses that request, without a second approval or reusable grant.
-- Each successful launch transfers process ownership once. A retired request
-- cannot adopt or reply successfully, and closes any unadopted prepared process.
-- Input is exact UTF-8 text bounded to 64 KiB; output offsets count bytes and
-- page limits must be 1..131072. IDs contain 1..128 characters with no NUL.
data TerminalServices = TerminalServices
  { terminalList :: IO (Either Text TerminalListing)
  , terminalStart :: TerminalLaunch -> IO (Either Text TerminalOpened)
  , terminalOutput :: TerminalId -> Int -> Int -> IO (Either Text TerminalPage)
  , terminalInput :: TerminalId -> Text -> IO (Either Text ())
  , terminalStop :: TerminalId -> IO (Either Text ())
  }
