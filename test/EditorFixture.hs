{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : EditorFixture
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Scoped real host editor ownership for model checks. The registry, body and
-- draft live for the callback; joint preparation/admission uses production APIs.
module EditorFixture (withEditorFixture, withEditorTextFixture, withEditorBodyFixture, installAutocompleteFixture, sameBufferVersions) where

import Control.Monad (unless)
import Data.Aeson (Value(Null))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Syntax (Style(..))
import qualified Data.Vector as V
import Hide.Model
import qualified Hide.Plugin.BufferHost as B
import qualified Hide.Plugin.Command as C
import qualified Hide.Plugin.Editor as E
import qualified Hide.Plugin.Window as W
import qualified Hide.Plugin.Menu as P
import Hide.PluginWindowHost (adoptEditorWindowUpdate,adoptWindowUpdate)

withEditorFixture :: Text -> Desktop -> (Desktop -> IO a) -> IO a
withEditorFixture target original run=case conversationBodySnapshot target original of
  Just prepared->withEditorBodyFixture target prepared original run
  Nothing->withEditorTextFixture target "" original run

-- A shared readable plain body for scalar/key checks. Styled or private-control
-- fixtures use withEditorBodyFixture with their actual immutable semantics.
withEditorTextFixture :: Text -> Text -> Desktop -> (Desktop -> IO a) -> IO a
withEditorTextFixture target text original run=do
  prepared<-W.prepareSemanticTextWindow "Conversation" [(text,Plain)]
    (W.TextSemantics W.CopyText Nothing V.empty V.empty W.ReadableWindow V.empty V.empty V.empty)
    >>= either (error . show) pure
  withEditorBodyFixture target prepared original run

withEditorBodyFixture :: Text -> W.PreparedWindow -> Desktop -> (Desktop -> IO a) -> IO a
withEditorBodyFixture target body original run=C.withRegistry $ \registry->
  E.withDraftRef $ \ref->W.withWindowScope $ \scope->do
    let unit=C.Codec Null (const (Right ())) (const Null)
        definition=C.CommandDef "hide.test.editor" "Editor" unit unit (\() ()->pure (Right ()))
    command<-C.registerCommand registry definition >>= either (error . show) pure
    let action=E.editorAction registry command (const (Right ())) (\() ()->pure ())
    prepared<-E.prepareEditor ref (E.EditorSpec True "Query" "Steer") "" action action >>= either (error . show) pure
    update<-W.openEditorWindow scope body prepared >>= maybe (error "Editor fixture scope ended") pure
    (accepted,adopted)<-adoptEditorWindowUpdate P.HumanMenu Nothing update original
    unless accepted (error "Editor fixture admission failed")
    let mount=E.editorMount prepared
        window=fromJust (activeWindow adopted)
        reference=case windowContent window of PluginContent actual->actual; _->error "Editor fixture is not a prepared frame"
    run adopted {conversationTarget=target,editingInput=MountedInput,
      conversationViews=M.insert target (ConversationView (InstalledBody reference Nothing) target ref (Just mount)
        (Just (windowId window)) FollowEnd 0 (scrollColumn window) Nothing Nothing Nothing Nothing) (conversationViews adopted)}

-- Compare immutable identities rather than document text, saved roots or Undo.
sameBufferVersions :: Desktop -> Desktop -> IO Bool
sameBufferVersions a b=do
  left<-traverse (B.captureVersion . documentBuffer) (buffers a)
  right<-traverse (B.captureVersion . documentBuffer) (buffers b)
  pure (left==right)

-- The completion view's host hint state is bound to an exact prepared frame;
-- ordinary documents with the same caption are not input owners.
installAutocompleteFixture :: W.WindowScope -> Text -> Desktop -> IO Desktop
installAutocompleteFixture scope text desktop=do
  body<-W.prepareRecoverableTextWindow "hide.autocomplete.transcript" 1 W.ReadableWindow "Autocomplete" text >>= either (error . T.unpack) pure
  publication<-W.openWindow scope body >>= maybe (error "Completion fixture scope ended") pure
  installed<-adoptWindowUpdate P.HumanMenu publication desktop
  let reference=W.updateWindowRef publication
  unless (M.member reference (pluginWindows installed)) (error "Completion fixture was not installed")
  pure installed {autocompleteWindow=Just reference,autocompleteACPEnabled=True}
