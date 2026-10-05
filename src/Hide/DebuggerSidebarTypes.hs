-- | Module      : Hide.DebuggerSidebarTypes
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Captured debugger targets carry the stopped epoch and their DAP owner IDs.
-- Labels and filesystem paths cannot select a debugger operation.
module Hide.DebuggerSidebarTypes
  ( DebuggerWatch(..), WatchFrame(..), WatchValue(..)
  , DebugSidebarRequest(..)
  , DebugPageTarget(..)
  , DebugPageRequest(..)
  , DebugSourceOperation(..), DebugSourceRequest(..)
  ) where

import Data.Text (Text)
import Hide.Plugin.Tree (TreeRef)
import Hide.Buffer (Selection)
import Hide.Plugin.BufferHost (ContentVersion)

-- | Only explicit sidebar activation changes the selected frame/source.
-- Choosing a frame preserves other frames' stopped-state handles.
data DebugSidebarRequest = SelectDebugFrame !Int !Int !Int
  | AddDebugWatch | EditDebugWatch !Int !Int | RemoveDebugWatch !Int !Int
  | EvaluateDebugWatch !Int !Int !WatchFrame
  | ForceDebugWatch !Int !Int !WatchFrame !Int
  -- Watch ID/revision, stopped receipt, parent reference, page offset/position, child.
  | ForceDebugWatchChild !Int !Int !WatchFrame !Int !Int !Int !Int deriving (Eq,Show)

-- | Bounded immutable expression metadata published by the existing debugger
-- owner. IDs are monotonic; an edit advances its revision, removal never reuses it.
-- Canonical origin is privacy provenance and grants no source/file authority.
data DebuggerWatch = DebuggerWatch
  { watchExpression :: !Text, watchRevision :: !Int
  , watchOrigin :: !(Maybe FilePath), watchPrivate :: !Bool, watchValue :: !WatchValue }

-- | Exact stopped selection receipt. Every field is a scalar owner identity.
data WatchFrame = WatchFrame !TreeRef !Int !Int !Int !Int deriving (Eq,Ord,Show)

-- | Small prepared presentation; stale results retain text but no live reference.
-- Raw adapter values are bounded/prepared by a worker and never kept here.
data WatchValue = WatchPending | WatchLoading !WatchFrame | WatchStale !Text !(Maybe FilePath)
  | WatchError !WatchFrame !Text !(Maybe FilePath)
  | WatchResult !WatchFrame !Text !Int !Bool !(Maybe FilePath)

-- | The owner validates thread/frame/reference provenance before enqueuing DAP.
-- Variable targets retain both their thread and frame, not just an adapter ID.
data DebugPageTarget = DebugThreads | DebugStack !Int | DebugScopes !Int !Int
  | DebugVariables !Int !Int !Int
  | DebugWatchVariables !Int !Int !WatchFrame !Int deriving (Eq,Ord,Show)

-- | Stop epoch, target and zero-based bounded page offset. Stack requests at
-- most 128 frames. Locals and Watches page a bounded cached response without
-- another DAP read.
data DebugPageRequest = DebugPageRequest !Int !DebugPageTarget !Int deriving (Eq,Ord,Show)

-- | Host-captured source actions. A label or frontend argument cannot supply
-- buffer identity; the menu owner validates this version before interpretation.
data DebugSourceOperation = ToggleSourceBreakpoint | AddSourceWatch deriving (Eq,Show)
data DebugSourceRequest = DebugSourceRequest
  { debugSourceOperation :: !DebugSourceOperation
  , debugSourceWindow :: !Int, debugSourceBuffer :: !Int
  , debugSourceVersion :: !ContentVersion, debugSourceSelection :: !Selection
  , debugSourceFile :: !(Maybe FilePath), debugSourceCanonical :: !(Maybe FilePath)
  , debugSourceRow :: !Int, debugSourceExpression :: !(Maybe Text)
  , debugSourceModified :: !Bool } deriving Eq
instance Show DebugSourceRequest where
  show request="DebugSourceRequest "<>show (debugSourceOperation request,debugSourceWindow request,debugSourceBuffer request,debugSourceRow request)
