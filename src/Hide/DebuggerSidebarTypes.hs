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
  ( DebugSidebarRequest(..)
  , DebugPageTarget(..)
  , DebugPageRequest(..)
  ) where

-- | Only explicit sidebar activation changes the selected frame/source.
-- Choosing a frame preserves other frames' stopped-state handles.
data DebugSidebarRequest = SelectDebugFrame !Int !Int !Int deriving (Eq,Show)

-- | The owner validates thread/frame/reference provenance before enqueuing DAP.
-- Variable targets retain both their thread and frame, not just an adapter ID.
data DebugPageTarget = DebugThreads | DebugStack !Int | DebugScopes !Int !Int
  | DebugVariables !Int !Int !Int deriving (Eq,Ord,Show)

-- | Stop epoch, target and zero-based bounded page offset. Stack and variables
-- request at most 128 rows; non-paged adapters are capped at that same boundary.
data DebugPageRequest = DebugPageRequest !Int !DebugPageTarget !Int deriving (Eq,Ord,Show)
