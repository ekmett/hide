-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- |
-- Module      : Hide.Plugin.Sidebar
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : GHC2021
--
-- Host capabilities for scoped tree and form contributions. Registration,
-- callable definitions and prepared metadata reuse their existing owners;
-- no capability exposes a desktop or grants command authority.
module Hide.Plugin.Sidebar (Sidebar(..)) where

import Hide.Plugin.Form (PreparedForm,FormUpdate)
import Hide.Plugin.Menu (MenuOrigin)
import Hide.Plugin.Tree (TreeProvider,TreeRef,NodeId)

-- | The host supplies its captured origin and closed reply projection.
-- Publications, including invalidation, are ordered through its bounded queue
-- and may block a worker; never publish from the UI owner. Closing the host wakes
-- blocked publishers and rejects both waiting and later calls. Adopting an
-- invalidation reuses the node's last admitted origin, never an invented human.
-- Neither publication nor invalidation extends a retired registration lifetime.
data Sidebar c r = Sidebar
  { sidebarOrigin :: c -> MenuOrigin
  , sidebarWorkspace :: c -> FilePath
  , formReply :: PreparedForm c r -> r
  , popupFormReply :: PreparedForm c r -> r
    -- ^ Request popup presentation of finite choices through the same form
    -- owner. The host supplies geometry and checks its captured human target.
  , publishTree :: TreeProvider c r -> IO ()
  , publishFormRefresh :: FormUpdate -> IO ()
  , invalidateTree :: TreeRef -> NodeId -> IO ()
  }
