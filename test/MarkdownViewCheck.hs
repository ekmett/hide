-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : MarkdownViewCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module MarkdownViewCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless,forM_)
import Data.Aeson (object,Value(..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust,isNothing)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.Directory (getTemporaryDirectory,removeFile,removePathForcibly,createDirectory)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import Hide.App (applyEffects)
import System.IO (openTempFile,hClose)
import System.Timeout (timeout)
import Hide.Buffer
import Hide.BufferView
import Hide.Commands (configuredBindings)
import Hide.Files (FileState(..))
import Hide.EditorMCP (builtinTool)
import Hide.GuestAccess (readableAt)
import Hide.Model
import Hide.Recovery (writeCheckpoint,readCheckpoint)
import Hide.Render (snapshot,renderKey)
import Hide.TextLayout
import Hide.TextPresentation

checks :: IO ()
checks=do
  directory<-getTemporaryDirectory
  let sourcePath=directory </> "hide-preview-notes.md"
      check name ok=unless ok (fail name)
      source="# Heading\n\nA **bold** [link](next.md).\n\n```haskell\nmain = putStrLn \"hi\"\n```\n\n"<>T.replicate 30 "later paragraph\n\n"
      original=replaceSelection (Selection 0 0) source (newBuffer "")
      named=addDocument (Just (FileState sourcePath Nothing)) original (initialDesktop (60,20))
      current=modifyActive (\w->w {selection=Selection 3 8,scrollRow=1,scrollColumn=2}) named
        {buffers=M.map (\document->document {documentSuggestedName=Just "notes.md"}) (buffers named)}
      win d=fromJust (activeWindow d)
      doc d=fromJust (activeDocument d)
      mode view d=fst (runCommand (SetBufferView view) d)
      bindings=either (error . show) id (configuredBindings [] M.empty)
      pending=(mode MarkdownView current) {keyBindings=bindings}
      navigation=[CursorLeft False,CursorLeft True,CursorRight False,CursorRight True,CursorUp False,CursorUp True,CursorDown False,CursorDown True]
      navigationKeys=[V.EvKey key mods | key<-[V.KLeft,V.KRight,V.KUp,V.KDown],mods<-[[],[V.MShift]]]
      interaction d=(selection (win d),scrollRow (win d),scrollColumn (win d),markdownInteraction (win d),revision (documentBuffer (doc d)))
      navigationBlocked d=all (not . commandEnabled d) navigation &&
        all ((==interaction d) . interaction . (\command->fst (runCommand command d))) navigation &&
        all ((==interaction d) . interaction . (\event->fst (handleEvent event d))) navigationKeys &&
        all ((==interaction d) . interaction) [horizontalMove True False d,verticalMove 1 True d]
      sameSource d=contents (documentBuffer (doc d))==source && revision (documentBuffer (doc d))==revision original && selection (win d)==selection (win current) && scrollRow (win d)==1 && scrollColumn (win d)==2
  check "Markdown view is named and preserves existing review enum values" (parseBufferView "markdown"==Just MarkdownView && fromEnum SideBySideView==3)
  check "Markdown preview keeps source identity and Current interaction" (bufferId (win pending)==bufferId (win current) && sameSource pending && not (windowChangeView original (win pending)))
  check "Pending preview never paints raw Markdown or source caret" (not ("# Heading" `T.isInfixOf` snapshot pending) && not ("later paragraph" `T.isInfixOf` snapshot pending))
  forM_ [Copy,Cut,Paste,Undo,Redo,Replace,Definition,RenameSymbol,ExecuteShellBlock (SourceShell 1) (0,1,"sh","echo unsafe")] $ \command->do
    let after=fst (runCommand command pending {clipboard="poison",browserFrontend=True})
    check "Pending named commands cannot mutate source" (sameSource after)
  forM_ [V.EvKey (V.KChar 'x') [],V.EvKey V.KBS [],V.EvPaste "bad"] $ \event->check "Pending raw input cannot edit background source" (sameSource (fst (handleEvent event pending)))
  check "Pending compiled navigation refuses source fallback" (navigationBlocked pending)
  check "Source popup cannot remain current after switching to Markdown" (not (contextTargetCurrent pending {contextTarget=captureContextTarget SourceContext current}))
  check "Preview retains F10 and menu mnemonic ownership" (menu (fst (handleEvent (V.EvKey (V.KFun 10) []) pending))/=Nothing && menu (fst (handleEvent (V.EvKey (V.KChar 'w') [V.MAlt]) pending))/=Nothing)
  check "Pending MCP selection fails closed" (case builtinTool pending "read_selection" (object []) of Left _->True; _->False)
  ready<-prepareTextPresentations pending
  let (_,text,links)=fromJust (windowMarkdown ready (win ready))
      rendered=contentSlice text 0 (contentLength text)
      selected=fst (runCommand SelectAll ready)
      copied=fst (runCommand Copy selected)
      shown=displayWindow (win selected)
  check "Prepared view removes markup and retains code" ("Heading" `T.isInfixOf` rendered && not ("# Heading" `T.isInfixOf` rendered) && "main = putStrLn" `T.isInfixOf` rendered && not (null links))
  check "Preview copy uses rendered text without changing source interaction" (clipboard copied==rendered && caret (selection shown)==contentLength text && sameSource copied)
  let messageOwned=selected {problemsVisible=True,problemsFocused=True,diagnostics=[Diagnostic "/tmp/public.hs" Nothing 1 1 1 "message owner"]}
  check "Focused Messages Copy does not take background preview selection" ("message owner" `T.isInfixOf` clipboard (fst (runCommand Copy messageOwned)) && clipboard (fst (runCommand Copy messageOwned))/=rendered)
  check "MCP selection identifies rendered coordinate space" (case builtinTool selected "read_selection" (object []) of Right (Object value)->KM.lookup "coordinateSpace" value==Just (String "rendered-markdown") && KM.lookup "text" value==Just (String rendered); _->False)
  let (start,_,_)=case links of link:_->link; []->error "Markdown preview link missing"
      (row,col)=windowTextPosition ready (win ready) text start
      r=bounds (win ready)
      browsed=modifyActive (modifyDisplayedWindow (\w->w {scrollRow=row,scrollColumn=0})) ready
  check "Preview link hit uses rendered offset" (linkAt (left r+1+col) (top r+1) browsed==Just (OpenLink (SourceLink (Just sourcePath)) "next.md"))
  check "Preview shell hit cannot authorize execution" (isNothing (shellBlockAt (left r+2) (top r+2) ready))
  let navigated=fst (handleEvent (V.EvKey V.KRight []) ready)
      scrolled=changeScroll True 5 navigated
      restored=mode CurrentView scrolled
  check "Ready compiled navigation keeps rendered movement available" (commandEnabled ready (CursorRight False) && selection (displayWindow (win navigated))/=selection (displayWindow (win ready)) && sameSource navigated)
  check "Preview movement/scroll preserves exact Current state" (sameSource restored && markdownInteraction (win scrolled)/=markdownInteraction (win ready))
  let wideBase=selected {wideSectionTitles=True}
  wide<-prepareTextPresentations wideBase
  let (layout,_,_)=fromJust (windowMarkdown wide (win wide))
      heading=snd (layoutPosition layout 1)
  check "Wide heading has measured two-cell geometry and preserves rendered selection" (heading==2 && selection (displayWindow (win wide))==selection shown)
  let resized=modifyActive (\w->w {bounds=(bounds w) {width=28}}) selected
  check "Stale width navigation refuses source fallback" (isNothing (windowMarkdown resized (win resized)) && navigationBlocked resized)
  resizedReady<-prepareTextPresentations resized
  check "Reflow clears preview selection without changing Current" (selection (displayWindow (win resizedReady))==Selection 0 0 && sameSource resizedReady)
  let split=fst (runCommand SplitVertical ready)
      ids=map windowId (windows split)
      separate=mode CurrentView (focusWindow (case ids of first:_->first; []->error "split preview window missing") split)
      other=focusWindow (last ids) separate
      changed=insertText "EDIT" separate
  check "Split windows choose source and Markdown independently" (length ids==2 && bufferView (win separate)==CurrentView && bufferView (win other)==MarkdownView)
  let staleRevision=(focusWindow (last ids) changed) {keyBindings=bindings}
  check "Stale revision navigation refuses source fallback" (isNothing (windowMarkdown staleRevision (win staleRevision)) && navigationBlocked staleRevision)
  changedReady<-prepareTextPresentations staleRevision
  check "Sibling source edit retires old preview" (isNothing (windowMarkdown (focusWindow (last ids) changed) (win (focusWindow (last ids) changed))) && maybe False (\(_,content,_)->"EDIT" `T.isInfixOf` contentSlice content 0 (contentLength content)) (windowMarkdown changedReady (win changedReady)))
  _<-renderKey ready {buffers=M.map (\d->d {documentBuffer=(documentBuffer d) {undoStack=error "render forced source Undo"}}) (buffers ready)}
  let private=ready {guestPrivatePaths=[sourcePath]}
  check "Rendered preview retains source privacy" (not (readableAt private (left r+1) (top r+1)))
  let plain=addDocument Nothing (newBuffer "ordinary") (initialDesktop (60,20)) {defaultBufferView=MarkdownView}
  check "Markdown default does not preview arbitrary source" (bufferView (win plain)==CurrentView && not (commandEnabled plain (SetBufferView MarkdownView)))
  withTextPresentation $ \owner->do
    queued<-fmap fst (tickTextPresentation owner [] pending)
    let newer=mode MarkdownView (insertText "replacement" (mode CurrentView queued))
    completed<-timeout 5000000 (await owner newer)
    check "Live worker adopts only current source revision" (maybe False (\d->case windowMarkdown d (win d) of Just (_,content,_)->"replacement" `T.isInfixOf` contentSlice content 0 (contentLength content); _->False) completed)
  bracket temporary removeFile $ \path->do
    saved<-writeCheckpoint path selected
    check "Preview recovery writes source and scalar interaction" (saved==Right ())
    recovered<-readCheckpoint path (initialDesktop (60,20)) >>= either (fail . T.unpack) pure
    check "Recovery retains mode and source state without prepared payload" (bufferView (win recovered)==MarkdownView && isNothing (windowMarkdown recovered (win recovered)) && selection (win recovered)==selection (win current))
    recoveredReady<-prepareTextPresentations recovered
    check "Recovered preview prepares and retains rendered selection" (selection (displayWindow (win recoveredReady))==selection shown)
  saveAsChecks
  putStrLn "markdown view checks passed"
  where
    await owner d=do
      next<-fmap fst (tickTextPresentation owner [] d)
      case activeWindow next >>= windowMarkdown next of
        Just _->pure next
        Nothing->threadDelay 10000 >> await owner next
    temporary=do directory<-getTemporaryDirectory; (path,h)<-openTempFile directory "hide-markdown-recovery"; hClose h; pure path

-- Successful Save As adopts the path for every shared view, preserving Current's
-- source interaction while retiring previews that the new file cannot support.
saveAsChecks :: IO ()
saveAsChecks=bracket temporary removePathForcibly $ \dir->do
  let check name ok=unless ok (fail name)
      win=fromJust . activeWindow
      apply d request=snd <$> applyEffects d [request]
  opened<-apply (initialDesktop (80,25)) (ReadPath (dir </> "notes.md"))
  let edited=insertText "# Heading\n\nbody\n" opened
      bid=fromJust (bufferId (win edited))
  saved<-apply edited (SaveDocument bid Nothing Nothing)
  let positioned=modifyActive (\w->w {selection=Selection 3 8,scrollRow=1,scrollColumn=2}) saved
      previews=fst (runCommand SplitVertical (fst (runCommand (SetBufferView MarkdownView) positioned)))
  ready<-prepareTextPresentations previews
  eligible<-apply ready (SaveDocument bid (Just (dir </> "copy.markdown")) Nothing)
  check "Save As to Markdown retains per-window previews" (all ((==MarkdownView).bufferView) (windows eligible))
  eligibleReady<-prepareTextPresentations eligible
  adopted<-apply eligibleReady (SaveDocument bid (Just (dir </> "notes.txt")) Nothing)
  text<-TIO.readFile (dir </> "notes.txt")
  check "Save As retires all incompatible previews and preserves source interaction"
    (all (\w->bufferView w==CurrentView && markdownInteraction w==Nothing && selection w==Selection 3 8 && scrollRow w==1 && scrollColumn w==2) (windows adopted) && M.null (windowPresentations adopted) && text=="# Heading\n\nbody\n")
  check "Source remains editable after incompatible Save As" (activeText (insertText "EDIT" adopted)/=activeText adopted)
  where
    temporary=do directory<-getTemporaryDirectory; (path,h)<-openTempFile directory "hide-markdown-saveas-check"; hClose h; removeFile path; createDirectory path; pure path
