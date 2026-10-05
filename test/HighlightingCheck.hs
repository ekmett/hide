{-# LANGUAGE OverloadedStrings #-}
module HighlightingCheck (checks) where

import Control.Concurrent
import Control.Exception (evaluate,finally)
import Control.Monad (unless,foldM)
import Data.IORef
import GHC.Conc (getAllocationCounter)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust,isJust)
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import qualified Data.Vector as V
import System.Timeout (timeout)
import System.Mem.StableName (makeStableName)
import Hide.Buffer
import Hide.Files
import Hide.Highlighting
import Hide.Model
import Hide.Render (snapshot,snapshotHtml)
import Hide.Syntax

checks :: IO ()
checks = do
  let exact="aé𝄞\t─\x301\r"
      row=prepareSourceRow exact (zip (T.unpack exact) (cycle [Keyword,Keyword,Plain]))
      pieces=[sourceRangeText row r | r<-V.toList (sourceRowRanges row)]
      ends=scanl (\(chars,bytes) text->(chars+T.length text,bytes+TU.lengthWord8 text)) (0,0) pieces
  check "source ranges preserve original UTF8 and character boundaries"
    (sourceRowText row==exact && T.concat pieces==exact &&
      [(sourceRangeCharEnd r,sourceRangeByteEnd r) | r<-V.toList (sourceRowRanges row)]==tail ends &&
      take (T.length exact) (sourceStylesAt row 0)==take (T.length exact) (cycle [Keyword,Keyword,Plain]))
  let sourceSigils row=let (_,_,sigils)=sourceSigilsWindow 0 maxBound row in sigils
      fragments Nil=[]
      fragments (ConsChars text _ rest)=text:fragments rest
      fragments (ConsSigil glyph _ _ rest)=graphemeText glyph:fragments rest
      display=sourceSigils row
  check "fused visible fragments borrow exact source runs and complete graphemes" (T.concat (fragments display)==exact)
  check "ordinary non-ASCII glyphs coalesce into a borrowed source run" (case sourceSigils (plainSourceRow "éδ─") of
    ConsChars "éδ─" Plain Nil->True
    _->False)
  let cluster=sourceSigils (prepareSourceRow "a\x301z" [('a',Keyword),('\x301',Plain),('z',Plain)])
  check "style boundary cannot split a combining source grapheme" (case cluster of
    ConsSigil glyph Keyword 1 (ConsChars "z" Plain Nil)->graphemeText glyph=="a\x301"
    _->False)
  check "exceptional width is independent of source character count" (case sourceSigils (plainSourceRow "🇯🇵") of
    ConsSigil glyph Plain 2 Nil->T.length (graphemeText glyph)==2
    _->False)
  check "zero-width source windows leave the row unforced" (case sourceSigilsWindow 4 0 (error "empty viewport forced source") of
    (0,0,Nil)->True
    _->False)
  check "clipped wide window retains complete source and original starts" (case sourceSigilsWindow 1 1 (plainSourceRow "界x") of
    (0,0,ConsSigil glyph Plain 2 Nil)->graphemeText glyph=="界"
    _->False)
  check "source window retains absolute tab stops" (case sourceSigilsWindow 6 2 (plainSourceRow "a\tb") of
    (1,1,ConsSigil glyph Plain 7 Nil)->graphemeText glyph=="\t"
    _->False)
  check "leading zero-width source preserves character selection coordinates" (case sourceSigilsWindow 0 2 (plainSourceRow "\rabc") of
    (1,0,ConsChars "ab" Plain Nil)->True
    _->False)
  let initial=addDocument (Just (FileState "example.py" Nothing)) (newBuffer "def old():\n    return 1\n") (initialDesktop (80,25))
      doc d=fromJust (activeDocument d)
      replace text d=d {buffers=M.adjust (\old->restyle old {documentBuffer=newBuffer text}) 1 (buffers d)}
      ready=isJust . documentSourceRows . doc
  check "new source has no lazy tokenizer to force on the display thread" (null (documentHighlight (doc initial)) && not (ready initial))
  let decorated=initial {buffers=M.map (\d->d {documentSourceRows=Just (V.singleton (prepareSourceRow "decorated" [(c,TerminalStyle 0x123456 0x654321 15) | c<-"decorated"]))}) (buffers initial)}
  check "HTML source capture keeps underline and strikethrough together" ("text-decoration:underline line-through" `T.isInfixOf` snapshotHtml decorated)
  let longText=T.replicate 100000 "界"
      longBase=addDocument Nothing (newBuffer longText) (initialDesktop (180,55))
      longView=longBase {buffers=M.map (\d->d {documentSourceRows=Just (V.singleton (plainSourceRow longText)),documentWidth=200000}) (buffers longBase)}
  _<-evaluate (T.length (snapshot longView))
  viewBefore<-getAllocationCounter
  viewCount<-evaluate (T.length (snapshot (modifyActive (\w->w {scrollColumn=1}) longView)))
  viewAfter<-getAllocationCounter
  check "a long source row prepares only the horizontally visible glyphs"
    (viewCount>0 && viewBefore-viewAfter<4000000)
  firstStarted<-newEmptyMVar
  nextStarted<-newEmptyMVar
  releaseFirst<-newEmptyMVar
  releaseNext<-newEmptyMVar
  calls<-newIORef ([]::[T.Text])
  -- Scheduling uses a deterministic tokenizer; cold grammar initialization is
  -- exercised separately through the production worker below.
  let tokenizer _ text=do
        index<-atomicModifyIORef' calls (\old->(old++[text],length old))
        if index==0 then putMVar firstStarted () >> takeMVar releaseFirst
                    else putMVar nextStarted () >> takeMVar releaseNext
        pure (zipWith (\i c->(c,if i<3 then Keyword else Plain)) [0::Int ..] (T.unpack text))
  withHighlightingUsing (pure ()) tokenizer $ \worker -> do
    first<-tickHighlighting worker initial
    bounded "first tokenizer starts" (takeMVar firstStarted)
    edited<-bounded "rapid edits never wait for the blocked tokenizer" $ foldM
      (\d n->tickHighlighting worker (replace ("def newest"<>T.pack (show n)<>"():\n    return 2\n") d)) first [1::Int ..20]
    check "replacement has same revision but new content" (revision (documentBuffer (doc edited))==revision (documentBuffer (doc initial)))
    check "pending view immediately renders current plain text" ("def newest20" `T.isInfixOf` snapshot edited && not (ready edited))
    putMVar releaseFirst ()
    bounded "only latest queued revision begins next" (takeMVar nextStarted)
    stale<-tickHighlighting worker edited
    check "completed old identity cannot color a replacement with the same revision" (not (ready stale))
    putMVar releaseNext ()
    colored<-await "latest running tokenizer result" worker ready stale
    requested<-readIORef calls
    check "rapid edits coalesce to first and newest requests" (length requested==2 && last requested==contents (documentBuffer (doc edited)))
    let rows=fromJust (documentSourceRows (doc colored))
    check "accepted rows contain current source and injected styles" (sourceRowText (rows V.! 0)=="def newest20():" && take 3 (sourceStylesAt (rows V.! 0) 0)==replicate 3 Keyword)
    before<-evaluate (buffers colored) >>= makeStableName
    unchanged<-tickHighlighting worker colored
    after<-evaluate (buffers unchanged) >>= makeStableName
    check "completed highlighting preserves unchanged document sharing" (before==after)
    let typed=insertText "x" colored
    check "editing invalidates accepted source rows immediately" (not (ready typed) && null (documentHighlight (doc typed)))
  initializing<-newEmptyMVar
  releaseInitialization<-newEmptyMVar
  initializedCalls<-newIORef ([]::[T.Text])
  withHighlightingUsing (putMVar initializing () >> takeMVar releaseInitialization)
    (\_ text->modifyIORef' initializedCalls (++[text]) >> pure (map (,Plain) (T.unpack text))) $ \worker -> do
      bounded "catalog initialization starts on its worker" (takeMVar initializing)
      queued<-bounded "source updates remain responsive during catalog initialization" $ foldM
        (\d n->tickHighlighting worker (replace ("newest "<>T.pack (show n)) d)) initial [1::Int ..20]
      waiting<-readIORef initializedCalls
      check "catalog initialization finishes before file tokenization starts" (null waiting)
      putMVar releaseInitialization ()
      colored<-await "newest request after catalog initialization" worker ready queued
      requested<-readIORef initializedCalls
      check "initialization coalesces all queued replacements to the newest source"
        (requested==["newest 20"] && sourceRowText (fromJust (documentSourceRows (doc colored)) V.! 0)=="newest 20")
  initializingStopped<-newEmptyMVar
  initializingEntered<-newEmptyMVar
  neverInitialized<-newEmptyMVar
  bounded "closing highlighting cancels catalog initialization" $ withHighlightingUsing
    ((putMVar initializingEntered () >> takeMVar neverInitialized) `finally` putMVar initializingStopped ())
    (\_ _->error "tokenizer ran before catalog initialization")
    (\_ ->takeMVar initializingEntered)
  bounded "catalog initialization cancellation cleanup completes" (takeMVar initializingStopped)
  withHighlighting $ \worker -> do
    colored<-await "cold real Python grammar result" worker ready initial
    let pythonRows=fromJust (documentSourceRows (doc colored))
    check "production worker applies real Python grammar on its cold first request"
      (sourceRowText (pythonRows V.! 0)=="def old():" && take 3 (sourceStylesAt (pythonRows V.! 0) 0)==replicate 3 Keyword)
    let renamed=colored {buffers=M.map (\d->restyle d {documentFile=Just (FileState "example.unknown" Nothing)}) (buffers colored)}
    plain<-await "unknown filename result" worker ready renamed
    check "filename changes invalidate syntax selection" (all ((==Plain).sourceRangeStyle) (V.concatMap sourceRowRanges (fromJust (documentSourceRows (doc plain)))))
  attempts<-newIORef (0::Int)
  timedOut<-newEmptyMVar
  withHighlightingUsing (pure ()) (\path text->do
    attempt<-atomicModifyIORef' attempts (\n->(n+1,n))
    if attempt==0 then (threadDelay 3000000 >> pure []) `finally` putMVar timedOut ()
      else pure (highlightFor path text)) $ \worker -> do
    started<-tickHighlighting worker initial
    expired<-timeout 4000000 (takeMVar timedOut)
    check "background tokenizer has a bounded runtime" (isJust expired)
    mapM_ (\_ -> tickHighlighting worker started >> threadDelay 1000) [1::Int ..30]
    count<-readIORef attempts
    check "timed out unchanged source is not rescheduled every tick" (count==1)
    retried<-await "edited source after tokenizer timeout" worker ready (replace "def retry(): return 3" started)
    retries<-readIORef attempts
    check "an edit retries highlighting after timeout" (ready retried && retries==2)
  entered<-newEmptyMVar
  stopped<-newEmptyMVar
  never<-newEmptyMVar
  bounded "closing highlighting cancels its blocked worker" $ withHighlightingUsing (pure ())
    (\_ _->(putMVar entered () >> takeMVar never) `finally` putMVar stopped ())
    (\worker->tickHighlighting worker initial >> takeMVar entered)
  bounded "worker cancellation cleanup completes" (takeMVar stopped)
  let lineCount=100000
      text=T.replicate lineCount "prefix\n"<>"TAIL"
      deep=addDocument Nothing (newBuffer text) (initialDesktop (80,25))
      indexed=deep {buffers=M.map (\d->d {documentHighlight=error "flat syntax traversed",documentWidth=8,
        documentSourceRows=Just (V.generate (lineCount+1) (\n->if n==lineCount then prepareSourceRow "TAIL" [('T',Keyword),('A',Keyword),('I',Keyword),('L',Keyword)] else error "offscreen syntax forced"))}) (buffers deep),
        windows=map (\w->w {scrollRow=lineCount}) (windows deep)}
  check "deep source scroll indexes only the visible highlighted row" ("TAIL" `T.isInfixOf` snapshot indexed)
  let pending=indexed {buffers=M.map (\d->d {documentSourceRows=Nothing}) (buffers indexed)}
  check "deep pending source scroll reads visible buffer rows without forcing syntax" ("TAIL" `T.isInfixOf` snapshot pending)
  let narrow=modifyActive (\w->w {bounds=Rect 0 1 20 10}) (addDocument Nothing (newBuffer (T.replicate 100 "x")) (initialDesktop (80,25)))
      end=moveTo False 100 narrow
  check "pending width still permits scrolling to the caret" (maybe False (\w->scrollColumn w>0 && windowDocumentWidth (doc end) w>=100) (activeWindow end))
  _<-evaluate (T.length (snapshot end))
  putStrLn "highlighting checks passed"

bounded :: String -> IO a -> IO a
bounded label action=timeout 1000000 action >>= maybe (error label) pure

await :: String -> Highlighting -> (Desktop -> Bool) -> Desktop -> IO Desktop
await label worker done start=timeout 4000000 (loop start) >>= maybe (error (label++": highlighting result timed out")) pure
  where loop d=do
          next<-tickHighlighting worker d
          if done next then pure next else threadDelay 10000 >> loop next

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)
