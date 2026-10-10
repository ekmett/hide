{-# LANGUAGE ExistentialQuantification #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.Input
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : ExistentialQuantification
--
-- Typed declarations for host-owned multiline input. The host compiles each
-- declaration into its existing command registration and editor attachment.
-- Drafts, selection, Undo, exact versions and submission claims stay with the
-- host; plugins receive bounded text only on the admitted command worker.
module Hide.Plugin.Input
  ( EditorSpec(..)
  , EditorSlot(..)
  , InputUpdate(..)
  , InputDeclaration(..)
  ) where

import Data.Text (Text)
import Hide.Plugin.Command (CommandDef,CommandError)

-- | Fixed input grammar and action labels. The host owns caret/selection
-- geometry. Enter and Ctrl+Enter select the two slots, subject to the host's
-- configured Enter action and newline grammar; labels never identify commands.
data EditorSpec = EditorSpec
  { editorCodeInput :: !Bool
  , editorDefaultLabel :: !Text
  , editorAlternateLabel :: !Text
  } deriving (Eq,Show)

-- | Exact action position captured before worker execution. Both slots use the
-- same registered command with independently prepared typed arguments.
data EditorSlot = DefaultEditor | AlternateEditor deriving (Eq,Show)

-- | Result of the acknowledged operation, bound by the host to its original
-- submission. No result can select another draft, version or visible window.
-- Keep preserves selection and Undo; Clear/Replace require that same immutable
-- version at adoption. Newer typing survives every older result. All three
-- consume the accepted submission at most once, including while its frame is
-- hidden. A command failure preserves the draft without applying an update.
data InputUpdate = KeepInput | ClearInput | ReplaceInput !Text deriving (Eq,Show)

-- | Declare metadata, a positive scalar-character bound, one ordinary typed
-- command, its slot/text argument adapter and its result adapter, in that order.
-- The host rejects malformed bounds/labels at activation. It checks cached input
-- length before materializing text; oversized input never reaches the command.
-- Both adapters execute on the existing command worker, outside desktop/registry
-- locks. Replacement text obeys the same bound and is prepared on that worker.
--
-- The host retains the original submission through the reply adapter, so
-- @ClearInput@ can clear only that submission's exact version. A declaration
-- grants neither human provenance nor provider authority; the supplying host
-- owns admission, service context, cancellation and registration lifetime.
data InputDeclaration c = forall a b. InputDeclaration
  !EditorSpec !Int !(CommandDef c a b)
  (EditorSlot -> Text -> Either CommandError a) (b -> InputUpdate)
