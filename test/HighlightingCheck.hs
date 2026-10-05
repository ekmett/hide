{-# LANGUAGE OverloadedStrings #-}
module HighlightingCheck (checks) where

import Control.Concurrent
import Control.Exception (evaluate,finally)
import Control.Monad (unless,foldM,forM_)
import Data.IORef
import GHC.Conc (getAllocationCounter)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust,isJust)
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import qualified Data.Vector as V
import qualified Graphics.Vty as VT
import System.Timeout (timeout)
import System.Mem.StableName (makeStableName)
import Hide.Buffer
import Hide.Files
import Hide.Highlighting
import Hide.Model
import Hide.Render (snapshot,snapshotHtml,renderCellRows)
import Hide.Unicode (CellSpan(..))
import Hide.Syntax

checks :: IO ()
checks = do
  sourceLineChecks
  composerWidthChecks
  plainSourceRowChecks
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
  forM_ [33,65] $ \count->do
    let original="a"<>T.replicate (count-1) "\x301"<>"Z"
        sigils=sourceSigils (plainSourceRow original)
        shown Nil=[]
        shown (ConsChars text _ rest)=text:shown rest
        shown (ConsSigil glyph _ advance rest)=if advance==1 then graphemeDisplayText glyph:shown rest else error "overflow source advance"
    check "single-scalar overflow tails retain source but emit visible replacement"
      (T.concat (fragments sigils)==original && T.concat (shown sigils)==T.replicate ((count+31) `div` 32) "�"<>"Z")
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
  deepBefore<-getAllocationCounter
  deepCount<-evaluate (T.length (snapshot (modifyActive (\w->w {scrollColumn=50000}) longView)))
  deepAfter<-getAllocationCounter
  check "far horizontal source scrolling skips prefix fragments without allocating them"
    (deepCount>0 && deepBefore-deepAfter<4000000)
  let viewportText=T.replicate 200 (T.replicate 20 "Haskell λ ⌘ "<>"\n")
      viewportSource=addDocument Nothing (newBuffer viewportText) (initialDesktop (180,55))
      preparedViewport=viewportSource {sideTree=Nothing,blinkCursor=False,buffers=M.map (\d->d
        {documentSourceRows=Just (V.fromList [prepareSourceRow line [(c,if c=='H' then Keyword else Plain) | c<-T.unpack line] | line<-T.lines viewportText])}) (buffers viewportSource)}
      occupied=V.foldl' (V.foldl' (\n span->case span of
        CellText paint text->paint `seq` n+T.length text
        CellGlyph paint text full start shown->paint `seq` n+T.length text+full+start+shown
        CellScript paint text natural script->paint `seq` script `seq` n+T.length text+natural)) 0
  _<-evaluate (occupied (renderCellRows preparedViewport))
  viewportBefore<-getAllocationCounter
  viewportCount<-evaluate (occupied (renderCellRows (modifyActive (\w->w {scrollRow=1,scrollColumn=1,selection=Selection 243 417}) preparedViewport)))
  viewportAfter<-getAllocationCounter
  check "ordinary source viewport avoids rebuilding prepared Unicode image rows"
    (viewportCount>0 && viewportBefore-viewportAfter<6000000)
  -- Chat and autocomplete share the visible-row renderer. Keep long draft
  -- lines borrowed even when moving the selection without changing their text.
  forM_ [False,True] $ \hint->do
    let draft=newBuffer (T.intercalate "\n" (replicate 12 (T.replicate 100 "words 界 e\x301 👩🏽\x200d\&💻 ")))
        base=addReadOnly (if hint then "Autocomplete" else "Conversation") "reply" (initialDesktop (100,35))
        chat=base {sideTree=Nothing,blinkCursor=False,appearance=LightMode,
          composerBuffer=draft,composerSelection=Selection 0 0,composerFocused=True,
          autocompleteACPEnabled=True,autocompleteDraft=draft,autocompleteSelection=Selection 0 0,autocompleteFocused=True}
    _<-evaluate (occupied (renderCellRows chat))
    before<-getAllocationCounter
    count<-evaluate (occupied (renderCellRows chat {composerSelection=Selection 1 1,autocompleteSelection=Selection 1 1}))
    after<-getAllocationCounter
    check "draft viewport avoids rebuilding offscreen character/style pairs" (count>0 && before-after<12000000)
  let mixedText="a界e\x301\t👩🏽\x200d\&💻z"
      mixedSource=addDocument (Just (FileState "Mixed.hs" Nothing)) (newBuffer mixedText) (initialDesktop (30,12))
      styledMixed=mixedSource {sideTree=Nothing,buffers=M.map (\d->d {documentLabel=Just "Source Mixed",documentSourceRows=Just
        (V.singleton (prepareSourceRow mixedText [(c,TerminalStyle 0x123456 0x654321 15) | c<-T.unpack mixedText]))}) (buffers mixedSource)}
      clipped=modifyActive (\w->w {bounds=Rect 1 1 14 7,scrollColumn=2,selection=Selection 1 2}) styledMixed
      glyphs=[(paint,text,full,start,shown) | CellGlyph paint text full start shown<-V.toList (renderCellRows clipped V.! 2)]
  check "source clipping keeps selected whole glyph and font traits"
    (any (\(paint,text,full,start,shown)->text=="界" && (full,start,shown)==(2,1,1) &&
      VT.attrForeColor paint==VT.SetTo (VT.RGBColor 0 0 170) && VT.attrBackColor paint==VT.SetTo (VT.RGBColor 170 170 170) &&
      VT.attrStyle paint==VT.SetTo (VT.bold+VT.italic+VT.underline+VT.strikethrough)) glyphs &&
      any (\(_,text,_,_,_)->text=="e\x301") glyphs && any (\(_,text,_,_,_)->text=="👩🏽\x200d\&💻") glyphs)
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

-- Bubble sizing stops at the available width, before rendering its visible rows.
composerWidthChecks :: IO ()
composerWidthChecks=do
  let size hint b columns=
        let d=(addReadOnly "Conversation" "" (initialDesktop (100,35))) {composerBuffer=b,autocompleteDraft=b}
            w=(fromJust (activeWindow d)) {bounds=Rect 0 1 (columns+6) 30}
        in width ((if hint then autocompleteComposerRect else composerRect) d w)
  forM_ [False,True] $ \hint->do
    let draft=(newBuffer (T.replicate 100000 "界"<>"\nshort"))
          {undoStack=error "composer width forced undo",saved=error "composer width forced baseline"}
    _<-evaluate (prepareBuffer draft)
    before<-getAllocationCounter
    columns<-evaluate (size hint draft 94)
    after<-getAllocationCounter
    check "bubble width stops measuring when its window is full" (columns==94 && before-after<100000)
    -- Avoid repeated root seeks when none of the short rows reaches the cap.
    let short=newBuffer (T.replicate 10000 "x\n")
    _<-evaluate (prepareBuffer short)
    shortBefore<-getAllocationCounter
    shortColumns<-evaluate (size hint short 94)
    shortAfter<-getAllocationCounter
    check ("bubble width visits short rows sequentially "++show (hint,shortColumns,shortBefore-shortAfter)) (shortColumns==12 && shortBefore-shortAfter<4000000)
    check "DEL placeholder occupies one source cell" (size hint (newBuffer "abcdefghijkl\DEL") 94==14)
    forM_ [0,8,12,13,20,94] $ \limit->
      forM_ ["","short","    x = 1\n","    "<>T.replicate 65 "\x301"<>"abcdefghijkl\n","    \x301"<>T.replicate 20 "界"<>"\n","\r\n","a\r","\t","👩🏽\x200d\&💻","a界e\x301\txyz\r\n","\x301","x\NULz",T.replicate 50 "界 e\x301 "] $ \text->do
        let lineWidth raw=let line=if not hint && "    " `T.isPrefixOf` raw then T.drop 4 raw else raw
                          in displayColumn line (T.length line)
            expected=min limit (max 12 (maximum (0:map lineWidth (textLines text))+1))
        check "bubble geometry keeps tabs, Unicode and code indentation" (size hint (newBuffer text) limit==expected)

-- Implicit plain rows have the same finite public projection and visible stream
-- as explicitly prepared Plain styling, including exceptional grapheme edges.
plainSourceRowChecks :: IO ()
plainSourceRowChecks=forM_ ["","abc","éδ─","a界e\x301\t👩🏽\x200d\&💻z","\rabc","a\r\n","\x301\&x","a\NUL\DELz"] $ \text->do
  let implicit=plainSourceRow text
      explicit=prepareSourceRow text [(c,Plain) | c<-T.unpack text]
      ranges=sourceRowRanges implicit
      pieces=[sourceRangeText implicit range | range<-V.toList ranges]
      fragments Nil=[]
      fragments (ConsChars run style rest)=Left (run,style):fragments rest
      fragments (ConsSigil glyph style advance rest)=Right (graphemeText glyph,style,advance):fragments rest
      window left width row=let (char,col,sigils)=sourceSigilsWindow left width row in (char,col,fragments sigils)
  check "implicit Plain range projection covers exact source characters and bytes"
    (sourceRowText implicit==text && T.concat pieces==text &&
      if T.null text then V.null ranges else case V.toList ranges of
        [range]->sourceRangeCharStart range==0 && sourceRangeCharEnd range==T.length text &&
          sourceRangeByteStart range==0 && sourceRangeByteEnd range==TU.lengthWord8 text && sourceRangeStyle range==Plain
        _->False)
  check "SourceRow equality is extensional across implicit and prepared Plain rows"
    (implicit==explicit && explicit==implicit && implicit==prepareSourceRow text [] &&
      (T.null text || implicit/=prepareSourceRow text [(c,Keyword) | c<-T.unpack text]))
  forM_ [0,1,2,6,8,12] $ \offset->do
    check "implicit Plain style projection preserves character offsets" (sourceStylesAt implicit offset==sourceStylesAt explicit offset)
    forM_ [0,1,2,8,32] $ \width->check "implicit Plain windows preserve complete fragments and original coordinates"
      (window offset width implicit==window offset width explicit)

-- The live source row consumes borrowed storage groups, preserving the same
-- complete-item paint/selection coordinates as its explicit flat projection.
sourceLineChecks :: IO ()
sourceLineChecks=do
  let samples=[T.replicate 1050 "a",T.replicate 260 "界a\t",T.replicate 540 "🇦",
        T.replicate 17 ("z"<>T.replicate 80 "\x301"),
        T.replicate 511 "a"<>"界"<>T.replicate 70 "\x301"<>"tail\r\n"]
      units Nil=[]
      units (ConsChars text style rest)=[(T.singleton c,style,1,T.singleton c) | c<-T.unpack text]++units rest
      units (ConsSigil glyph style advance rest)
        | advance==0=units rest
        | otherwise=(graphemeText glyph,style,advance,graphemeDisplayText glyph):units rest
      window left width row=let (char,col,sigils)=sourceSigilsWindow left width row in (char,col,units sigils)
  let capped=T.replicate 60 "a"<>"z"<>T.replicate 31 "\x301"<>T.replicate 42 "\x1d165"<>"X"
      recut=replaceSelection (Selection (T.length capped) (T.length capped+300)) "" (newBuffer (capped<>T.replicate 300 "b"))
  forM_ (map newBuffer samples++[recut]) $ \b->do
    let source=contents b
        line=contentSourceLineAt (bufferContent b) 0
        visible=lineAt source 0
        raw=head (T.splitOn "\n" source)
        prepared=prepareSourceRow raw (zip (T.unpack raw) (cycle [Plain,Keyword,Comment]))
        live=attachSourceLine line prepared
    check "live source row keeps exact public text and ranges" (live==prepared)
    check "implicit live source row keeps exact plain projection" (plainSourceLine line==plainSourceRow visible)
    forM_ [0,1,6,31,511,512,1024] $ \left->forM_ [0,1,2,80,180] $ \width->do
      check ("borrowed prepared storage preserves clipped complete-item styles and coordinates "++show (T.take 20 source,left,width))
        (window left width live==window left width prepared)
      check "borrowed plain storage preserves clipped complete-item coordinates"
        (window left width (plainSourceLine line)==window left width (plainSourceRow visible))
  let b=newBuffer (T.replicate (1024*1024) "界")
      row=plainSourceLine (contentSourceLineAt (bufferContent b) 0)
      forceWindow left=let (char,col,sigils)=sourceSigilsWindow left 180 row
                       in char+col+sum [T.length text+advance | (text,_,advance,_)<-units sigils]
  _<-evaluate (prepareBuffer b)
  _<-evaluate (forceWindow 0)
  -- Eager prepareBuffer previously excluded index construction from this
  -- viewport guard. Loaded rows now construct only the demanded prefix: measure
  -- that first linear seek independently, then guard a distinct nearby viewport.
  firstBefore<-getAllocationCounter
  firstCount<-evaluate (forceWindow 500000)
  firstAfter<-getAllocationCounter
  check "first far viewport prepares exact complete source glyphs and coordinates"
    (firstCount==750270 && window 500000 180 row==(250000,500000,replicate 90 ("界",Plain,2,"界")))
  check "first far viewport keeps preparation proportional to span receipts"
    (firstBefore-firstAfter<20*1024*1024)
  before<-getAllocationCounter
  count<-evaluate (forceWindow 500001)
  after<-getAllocationCounter
  check "live source viewport seeks borrowed long-row leaves without a flat projection"
    (count>0 && before-after<128*1024)
  check "prepared nearby viewport retains clipped wide source glyphs"
    (window 500001 180 row==(250000,500000,replicate 91 ("界",Plain,2,"界")))
