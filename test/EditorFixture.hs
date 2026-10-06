{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Scoped real host editor ownership for model checks. The registry and draft
-- live for the callback; preparation and admission use the production host API.
module EditorFixture (withEditorFixture, sameBufferVersions) where

import Control.Monad (unless)
import Data.Aeson (Value(Null))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import Hide.Model
import qualified Hide.Plugin.BufferHost as B
import qualified Hide.Plugin.Command as C
import qualified Hide.Plugin.Editor as E
import qualified Hide.Plugin.Window as W
import qualified Hide.Plugin.Menu as P
import Hide.PluginWindowHost (adoptEditorWindowUpdate)

withEditorFixture :: Text -> Desktop -> (Desktop -> IO a) -> IO a
withEditorFixture target original run=C.withRegistry $ \registry->
  E.withDraftRef $ \ref->W.withWindowScope $ \scope->do
    let unit=C.Codec Null (const (Right ())) (const Null)
        definition=C.CommandDef "hide.test.editor" "Editor" unit unit (\() ()->pure (Right ()))
    command<-C.registerCommand registry definition >>= either (error . show) pure
    let action=E.editorAction registry command (const (Right ())) (\() ()->pure ())
        opened=case conversationDocument target original of
          Just _->original
          Nothing->addConversationDocument original
        bid=case conversationDocument target original of
          Just (ident,_)->ident
          Nothing->fromJust (activeWindow opened >>= bufferId)
        window=fromJust (activeWindow opened)
    prepared<-E.prepareEditor ref (E.EditorSpec True "Query" "Steer") "" action action >>= either (error . show) pure
    body<-W.prepareTextWindow "Editor fixture" ""
    update<-W.openEditorWindow scope body prepared >>= maybe (error "Editor fixture scope ended") pure
    (accepted,adopted)<-adoptEditorWindowUpdate P.HumanMenu Nothing update opened
    unless accepted (error "Editor fixture admission failed")
    let mount=E.editorMount prepared
    -- Public joint admission transfers the actual seed. These model fixtures
    -- attach its mount to their existing host Conversation source frame.
    run opened
      {conversationTarget=target,editingInput=MountedInput,
       conversationViews=M.insert target (ConversationView bid target ref (Just mount) (Just (windowId window)) (scrollRow window,scrollColumn window) (selection window)) (conversationViews opened),
       editorDrafts=editorDrafts adopted,
       windows=map (\w->if windowId w==windowId window then w {windowEditorMount=Just mount} else w) (windows opened)}

-- Compare immutable identities rather than document text, saved roots or Undo.
sameBufferVersions :: Desktop -> Desktop -> IO Bool
sameBufferVersions a b=do
  left<-traverse (B.captureVersion . documentBuffer) (buffers a)
  right<-traverse (B.captureVersion . documentBuffer) (buffers b)
  pure (left==right)
