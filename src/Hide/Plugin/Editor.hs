{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- |
-- Module      : Hide.Plugin.Editor
-- Copyright   : (c) Edward Kmett
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Host-owned multiline input attached to prepared window content. Plugins
-- declare typed actions and initial text; the host owns the actual draft Buffer,
-- Undo, caret, selection and focus. Action adapters run on the existing worker.
-- Registry/publication lifetimes grant no human or MCP input authority.
module Hide.Plugin.Editor
  ( DraftRef, newDraftRef, draftRefCurrent, retireDraftRef
  , EditorMount, mountDraft, mountSpec, mountActions
  , EditorSpec(..), EditorSlot(..), EditorAction, editorAction
  , PreparedEditor, prepareEditor, remountEditor, editorMount
  , DraftSubmission, submissionDraft, submissionMount, submissionVersion
  , submissionContent, submissionAction, submissionSlot
  , EditorUpdate, clearEditorDraft, replacementEditorDraft
  ) where

import Data.Text (Text)
import Hide.Buffer (newBuffer)
import Hide.Plugin.Command (CommandError)
import Hide.Plugin.EditorHost

-- | Prepare initial text on the existing command/reply worker. The host transfers
-- the seed once; remounting or refreshing content never resets a retained draft.
-- Recovered Buffer roots use the distinct internal host operation directly.
prepareEditor :: DraftRef -> EditorSpec -> Text -> EditorAction c r -> EditorAction c r -> IO (Either CommandError (PreparedEditor c r))
prepareEditor draft spec text=prepareEditorBuffer draft spec (newBuffer text)
