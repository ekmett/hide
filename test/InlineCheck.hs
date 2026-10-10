-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : InlineCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module InlineCheck (checks) where

import Control.Monad (forM_,unless)
import Data.Aeson (Value(..))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Hide.Buffer
import Hide.InlineState
import Hide.Files (FileState(..))
import qualified Hide.Model as Model
import qualified Graphics.Vty as Vty
import Hide.Plugin.Completion

checks :: IO ()
checks=do
  let source="foo old end"
      b=newBuffer source
      original=Proposal 0 (T.length source) "foo baz quux end" (Just Null)
  normalized<-prepare b original
  check "normalization preserves original insertion prefix for cumulative feedback"
    (optionPrefixLength normalized==4 && optionProposal normalized==Proposal 4 7 "baz quux" (Just Null))
  let (a,z,taken,next)=proposalWord b (optionProposal normalized)
  check "partial replacement takes one word without trailing whitespace"
    ((a,z,taken)==(4,7,"baz") && optionPrefixLength normalized+T.length taken==7)
  check "partial replacement rebases remaining insertion"
    (next==Just (Proposal 7 7 " quux" (Just Null)))
  let rebasingSource="foo x y z end"
  first<-prepare (newBuffer rebasingSource) (Proposal 0 (T.length rebasingSource) "foo a y b end" Nothing)
  let (ra,rz,rt,rp)=proposalWord (newBuffer rebasingSource) (optionProposal first)
      after=replaceSelection (Selection ra rz) rt (newBuffer rebasingSource)
  remainder<-maybe (error "Expected rebased remainder") (prepare after) rp
  check "re-normalized remainder contributes additional shared prefix to feedback"
    (optionPrefixLength first+T.length rt+optionPrefixLength remainder==8 && optionProposal remainder==Proposal 8 9 "b" Nothing)
  forM_ ["  next word","\n\tλambda rest","😀 rest","e\x0301lan rest","👩\x200d\&💻 next","(value)"] $ \text->do
    let (_,_,accepted,_)=proposalWord (newBuffer "") (Proposal 0 0 text Nothing)
        expected=case text of
          "  next word"->"  next"
          "\n\tλambda rest"->"\n\tλambda"
          "😀 rest"->"😀"
          "e\x0301lan rest"->"e\x0301lan"
          "👩\x200d\&💻 next"->"👩\x200d\&💻"
          _->"("
    check "next word includes leading whitespace and whole Unicode graphemes" (accepted==expected)
  forM_ [ ("",0,0,"one two three")
        , ("old several words",0,17,"new")
        , ("old",0,3,"new several words")
        , ("prefix old tail",7,10,"")
        , ("a😀b\nλ end",1,5,"😀 newer\nλ")
        , ("before\nold\nlast\nafter",7,15,"new\nblock")
        , ("foo x y z end",0,13,"foo a y b end")
        , ("abc",0,3,"  \n")
        ] $ \(text,start,end,replacement)->do
    let buffer=newBuffer text; p=Proposal start end replacement Nothing
        expected=T.take start text<>T.replace "\r\n" "\n" replacement<>T.drop end text
    option<-prepare buffer p
    final<-acceptAll 30 buffer (optionProposal option)
    check "repeated partial acceptance equals the independent full replacement" (contents final==expected)
    let edited=replaceSelection (Selection (proposalStart (optionProposal option)) (proposalEnd (optionProposal option))) (proposalText (optionProposal option)) buffer
    check "prefix/suffix normalization preserves the full edit" (contents edited==expected)
  deletion<-prepare (newBuffer "a\nb\nc") (Proposal 0 5 "" Nothing)
  check "full deletion is valid and has one empty replacement row"
    (optionFirstRow deletion==0 && optionLastRow deletion==2 && V.length (optionRows deletion)==1 && proposalWord (newBuffer "a\nb\nc") (optionProposal deletion)==(0,5,"",Nothing))
  unicode<-prepare (newBuffer "😀oldλ") (Proposal 1 4 "new" Nothing)
  check "proposal coordinates use code points rather than UTF16 units"
    (optionFirstRow unicode==0 && V.toList (optionRows unicode)==[[("😀",False),("new",True),("λ",False)]])
  crlf<-prepare (newBuffer "") (Proposal 0 0 "a\r\nb" Nothing)
  check "accepted text matches normalized preview newlines" (proposalText (optionProposal crlf)=="a\nb")
  forM_ [Proposal (-1) 0 "x" Nothing,Proposal 2 1 "x" Nothing,Proposal 0 4 "x" Nothing,Proposal 0 0 "\0" Nothing,Proposal 0 3 "abc" Nothing] $ \p->
    check "invalid ranges, NUL, and no-op proposals are rejected" (case prepareOption (newBuffer "abc") p of Left _->True; _->False)
  let view=InlineView 1 1 0 (Selection 0 0) 1 [unicode] (-1)
  check "invalid alternative index has no selection" (selectedOption view==Nothing && selectedOption (view {inlineIndex=1})==Nothing)
  let public=Model.addDocument Nothing b (Model.initialDesktop (80,25))
      window=maybe (error "Missing inline source window") id (Model.activeWindow public)
      bid=maybe (error "Missing inline source buffer") id (Model.bufferId window)
      sourceBuffer=maybe (error "Missing inline source document") Model.documentBuffer (Model.activeDocument public)
      captured=InlineView (Model.windowId window) bid (revision sourceBuffer) (Model.selection window) (Model.inlineEpoch public) [normalized] 0
      withDocument change=public {Model.buffers=M.adjust change bid (Model.buffers public)}
      propose=Vty.EvKey (Vty.KChar '\\') [Vty.MAlt]
      noInline event desktop=case Model.inlineEvent event desktop of Nothing->True; _->False
      privateSources=
        [("explicit private drafts",withDocument (\doc->doc {Model.documentPrivate=True}))
        ,("registered authority files",(withDocument (\doc->doc {Model.documentFile=Just (FileState "/authority/config.toml" Nothing)})) {Model.guestPrivatePaths=["/authority"]})
        ,("protected source origins",(withDocument (\doc->doc {Model.documentOrigin=Just "/authority/source.hs"})) {Model.guestPrivatePaths=["/authority"]})
        ,("project authority files",withDocument (\doc->doc {Model.documentFile=Just (FileState "/project/thc.toml" Nothing)}))]
  check "ordinary public sources remain eligible for autocomplete"
    (Model.inlineEligible public && Model.inlineMatches public captured && case Model.inlineEvent propose public of
      Just (_,effects)->effects==[Model.AutocompleteAction "propose" []]
      _->False)
  forM_ privateSources $ \(label,private)->do
    check (label++" refuse autocomplete requests") (not (Model.inlineEligible private) && noInline propose private)
    -- Only privacy/path metadata changes: the original buffer, revision, caret
    -- and view receipt are retained, so numeric source guards alone would pass.
    check (label++" revoke an existing inline proposal")
      (not (Model.inlineMatches private captured) && noInline (Vty.EvKey (Vty.KChar '\t') []) private {Model.inlinePreview=Just captured})
    let hidden=private {Model.buffers=M.adjust (\doc->doc {Model.documentBuffer=error "Inline privacy forced protected source content"}) bid (Model.buffers private)}
    check (label++" reject before inspecting protected source content") (not (Model.inlineEligible hidden) && not (Model.inlineMatches hidden captured))
  putStrLn "Inline checks passed"

prepare :: Buffer -> Proposal -> IO InlineOption
prepare b= either (error . T.unpack) pure . prepareOption b

acceptAll :: Int -> Buffer -> Proposal -> IO Buffer
acceptAll fuel b p
  | fuel<=0=error "Partial proposal failed to make progress"
  | otherwise=do
      let (a,z,text,next)=proposalWord b p
          after=replaceSelection (Selection a z) text b
      check "partial edit range remains within the current buffer" (a>=0 && a<=z && z<=bufferLength b)
      case next of
        Nothing->pure after
        Just remainder->case prepareOption after remainder of
          Left _->error "Rebased proposal unexpectedly invalid"
          Right option->acceptAll (fuel-1) after (optionProposal option)

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)
