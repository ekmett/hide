-- | Module      : Hide.Plugin.Sidebar
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Host capabilities for scoped tree and form contributions. Registration,
-- callable definitions and prepared metadata reuse their existing owners;
-- no capability exposes a desktop or grants command authority.
module Hide.Plugin.Sidebar (Sidebar(..)) where

import Hide.Plugin.Editor (EditorUpdate)
import Hide.Plugin.Window (EditorWindowUpdate)
import Hide.Plugin.Form (PreparedForm,FormUpdate)
import Hide.Plugin.Menu (MenuOrigin)
import Hide.Plugin.Tree (TreeProvider,TreeRef,NodeId)

-- | The host supplies its captured origin and closed reply projection.
-- Publications are ordered through its bounded queue and may block a worker.
-- 'tryInvalidateTree' must never block: False means the caller retains that
-- node for a later tick; a closed host rejects explicitly. Adopting an
-- invalidation reuses the node's last admitted origin, never an invented human.
-- Neither publication nor invalidation extends a retired registration lifetime.
data Sidebar c r = Sidebar
  { sidebarOrigin :: c -> MenuOrigin
  , sidebarWorkspace :: c -> FilePath
  , formReply :: PreparedForm c r -> r
  , editorWindowReply :: EditorWindowUpdate c r -> r
  , editorUpdateReply :: EditorUpdate -> r
  , publishTree :: TreeProvider c r -> IO ()
  , publishFormRefresh :: FormUpdate -> IO ()
  , tryInvalidateTree :: TreeRef -> NodeId -> IO Bool
  }
