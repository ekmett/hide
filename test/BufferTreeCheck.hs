{-# LANGUAGE OverloadedStrings #-}
module BufferTreeCheck (checks) where

import Control.Monad (foldM, forM_, unless)
import Control.Exception (evaluate)
import GHC.Conc (getAllocationCounter)
import System.Mem.StableName (makeStableName)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import THC.Edit.Buffer
import qualified THC.Edit.BufferView as View

check :: String -> Bool -> IO ()
check name ok = unless ok (error ("buffer tree: " ++ name))

checkIndexed :: Buffer -> T.Text -> IO ()
checkIndexed b text = do
  check "cached character count" (bufferLength b == T.length text)
  check "cached NUL flag matches Text" (textBuffer b == (not (byteMode b) && not (T.any (=='\0') text)))
  check "cached newline style matches Text" (bufferNewline b == if "\r\n" `T.isInfixOf` text then "\r\n" else "\n")
  forM_ [-1..T.length text+1] $ \start -> forM_ [0,1,5,T.length text+1] $ \count ->
    check "tree range matches Text" (bufferSlice b start count == T.take count (T.drop (max 0 start) text))
  check "cached line count includes the trailing empty line" (bufferLineCount b == length (textLines text))
  forM_ [-1..T.length text+1] $ \p ->
    check "indexed position matches Text" (bufferLineColumn b p == lineColumn text p)
  forM_ [0..T.length text] $ \p -> do
    check "indexed next character matches Text" (bufferNextCharacter b p == nextCharacter text p)
    check "indexed previous character matches Text" (bufferPreviousCharacter b p == previousCharacter text p)
  forM_ [-1..length (textLines text)+1] $ \row -> do
    check "indexed line offset matches Text" (bufferLineOffset b row == lineOffset text row)
    check "indexed CRLF line content matches Text" (bufferLineAt b row == lineAt text row)

checks :: IO ()
checks = do
  batchChecks
  lineChangesChecks
  let clipped = replaceSelection (Selection (-3) 99) "界" (newBuffer "abc")
  check "change offsets describe the actual clipped edit"
    (contents clipped == "界" && lastChange clipped == Just (0,3,1))
  forM_ ["", "\0x\n", "\n", "\n\n", "a\r\nb\r\n", "λ😀e\x0301\n界", "last\r", "a\nb\nc\n"] $ \source -> do
    check "initial text roundtrip" (contents (newBuffer source) == source)
    checkIndexed (newBuffer source) source
    forM_ [0..T.length source] $ \a -> forM_ [a..T.length source] $ \z ->
      forM_ ["", "x", "\n", "\r\n", "😀\n界\n"] $ \inserted -> do
        let original=newBuffer source
            edited=replaceSelection (Selection z a) inserted original
            expected=T.take a source <> inserted <> T.drop z source
        check "boundary edit matches Text" (contents edited == expected)
        checkIndexed edited expected
        check "selection matches Text" (selectedText (Selection a z) original == T.take (z-a) (T.drop a source))
        check "undo keeps original tree" (contents (undo edited) == source && contents original == source)
        check "redo restores edited tree" (contents (redo (undo edited)) == expected)
  let initial=T.concat (replicate 40 "λ😀\r\nsecond line\n\n")
      seeds=take 500 (drop 1 (iterate (\n -> (1664525*n+1013904223) `mod` 4294967296) (42 :: Integer)))
  (final,history) <- foldM edit (newBuffer initial,[initial]) seeds
  let snapshots=take 101 history
      undone=take (length snapshots) (iterate undo final)
      oldest=last undone
      replayed=take (length snapshots) (iterate redo oldest)
  check "bounded undo retains the last 100 edits" (map contents undone == snapshots)
  check "undo snapshots are capped" (length (undoStack final) == 100)
  check "undo stops at history boundary" (contents (undo oldest) == last snapshots && lastChange (undo oldest) == Nothing)
  check "redo replays all retained snapshots" (map contents replayed == reverse snapshots)
  let branched=replaceSelection (Selection 0 0) "branch" (undo final)
  check "an edit after undo discards redo" (contents (redo branched) == contents branched && lastChange (redo branched) == Nothing)
  check "markSaved updates the baseline" (not (dirty (markSaved final)))
  where
    edit (_,[]) _ = error "buffer tree: missing reference history"
    edit (b,history@(old:_)) seed = do
      let size=T.length old
          rawA=fromInteger (seed `mod` toInteger (size+9))-4
          rawZ=fromInteger ((seed `div` 97) `mod` toInteger (size+9))-4
          a=max 0 (min size (min rawA rawZ))
          z=max 0 (min size (max rawA rawZ))
          inserted=["", "x", "\n", "界😀", "\r\n", "α\nb\n", "e\x0301", "\r"] !! fromInteger (seed `mod` 8)
          expected=T.take a old <> inserted <> T.drop z old
          changed=expected /= old
          next=replaceSelection (Selection rawA rawZ) inserted b
      check "deterministic edit sequence matches Text" (contents next == expected)
      checkIndexed next expected
      check "edit revision advances only on a change" (revision next == revision b + if changed then 1 else 0)
      check "edit change range" (lastChange next == if changed then Just (a,z,T.length inserted) else Nothing)
      checkReview next
      check "persistent old snapshots retain their text" (contents b == old)
      check "measured dirty matches saved contents" (dirty next == (contents next/=saved next))
      check "single undo and redo agree with Text" (not changed || (contents (undo next) == old && contents (redo (undo next)) == expected))
      check "undo and redo expose inverse change ranges" (not changed ||
        (lastChange (undo next) == Just (a,a+T.length inserted,z-a) && lastChange (redo (undo next)) == Just (a,z,T.length inserted)))
      pure (next,if changed then expected:history else history)


lineChangesChecks :: IO ()
lineChangesChecks=do
  let original=newBuffer "one\ntwo\nthree\n"
      changed=replaceSelection (Selection 4 7) "TWO" original
      reedited=replaceSelection (Selection 4 7) "again" changed
      restored=replaceSelection (Selection 4 9) "two" reedited
      inserted=replaceSelection (Selection 4 4) "fresh\n" original
      removedFresh=replaceSelection (Selection 4 10) "" inserted
      deleted=replaceSelection (Selection 4 8) "" original
      savedEdit=markSaved changed
      count label expected b=check label (bufferLineChanges b==expected)
  count "new buffer has no changed lines" (0,0) original
  count "modified original line is removed and inserted" (1,1) changed
  count "reediting a new line does not inflate counts" (1,1) reedited
  let duplicate=newBuffer "same\nsame\n"
      oneDeleted=replaceSelection (Selection 0 5) "" duplicate
      duplicateRestored=replaceSelection (Selection 5 5) "same\n" oneDeleted
  count "duplicate-line delete/reinsert restores exact saved contents" (0,0) duplicateRestored
  check "exact duplicate-line restoration is clean" (not (dirty duplicateRestored))
  count "typing original text restores its identity" (0,0) restored
  count "inserting a whole line preserves its neighbour" (1,0) inserted
  count "deleting a new line cancels its insertion" (0,0) removedFresh
  count "deleting an original line leaves a tombstone" (0,1) deleted
  checkIndexed deleted "one\nthree\n"
  count "undo restores line provenance" (0,0) (undo changed)
  count "redo restores changed line provenance" (1,1) (redo (undo changed))
  count "save establishes a fresh line baseline" (0,0) savedEdit
  count "undo across save compares with the saved baseline" (1,1) (undo savedEdit)
  count "redo across save returns to zero" (0,0) (redo (undo savedEdit))
  count "empty unnamed buffer gains its first line" (1,0) (replaceSelection (Selection 0 0) "hello" (newBuffer ""))
  count "deleting the final content leaves no added empty line" (0,1) (replaceSelection (Selection 0 3) "" (newBuffer "one"))
  count "trailing editor row is not a new file line" (1,0) (replaceSelection (Selection 4 4) "last\n" (newBuffer "one\n"))
  count "CRLF lines use the same provenance" (1,1) (replaceSelection (Selection 0 3) "ONE" (newBuffer "one\r\ntwo\r\n"))
  count "saving deleted lines resets their tombstones" (0,0) (markSaved deleted)
  count "undo saved deletion adds a line" (1,0) (undo (markSaved deleted))
  count "undo saved insertion deletes a line" (0,1) (undo (markSaved inserted))
  forM_ [original,changed,reedited,restored,inserted,removedFresh,deleted,savedEdit,undo savedEdit] $ \buffer -> do
    recovered<-either (error . T.unpack) pure (restoreBuffer (snapshotBuffer buffer))
    check "snapshot restores line counts" (bufferLineChanges recovered==bufferLineChanges buffer)
    check "snapshot restores undo line counts" (bufferLineChanges (undo recovered)==bufferLineChanges (undo buffer))
    check "snapshot restores redo line counts" (bufferLineChanges (redo recovered)==bufferLineChanges (redo buffer))

  let oldCheckpoint=(snapshotBuffer changed) {snapshotLineChanges=Nothing}
  oldRestored<-either (error . T.unpack) pure (restoreBuffer oldCheckpoint)
  count "old checkpoint restores conservative line counts" (1,1) oldRestored
  checkIndexed oldRestored (contents changed)
  let snapshot=snapshotBuffer changed
      invalid changes=snapshot {snapshotLineChanges=Just (changes,replicate (length (snapshotUndo snapshot)) [],[])}
      rejected candidate=case restoreBuffer candidate of Left _ -> True; Right _ -> False
  forM_ [[(-1,True,"")],[(maxBound,True,"")],[(0,True,"forged")],[(0,False,"forged\n")],[(0,False,"one\ntwo\n")],[(0,True,""),(0,True,"")]] $ \changes ->
    check "invalid recovery line metadata is rejected" (rejected (invalid changes))
  let forged=(snapshotBuffer (newBuffer "z")) {snapshotSaved="abc",snapshotLineChanges=Just ([(0,False,"a"),(0,False,"bc"),(0,True,"")],[],[])}
  check "recovery rejects forged baseline line boundaries" (rejected forged)
  check "recovery rejects insertions before deletions" (rejected forged {snapshotLineChanges=Just ([(0,True,""),(1,False,"abc")],[],[])})
  check "missing recovery line history is rejected" (rejected snapshot {snapshotLineChanges=Just ([],[],[])})
  let aged=foldl (\b n -> replaceSelection (Selection 0 1) (if even n then "X" else "Y") b) (newBuffer "original\n") [1::Int ..150]
  agedRestored<-either (error . T.unpack) pure (restoreBuffer (snapshotBuffer aged))
  count "provenance survives after baseline leaves undo history" (1,1) agedRestored
  count "oldest recovered undo still compares with original baseline" (1,1) (iterate undo agedRestored !! 100)
  let bytes=newByteBuffer (BS.pack [65,0,10,66])
      byteEdit=replaceSelection (Selection 0 2) "C" bytes
  count "byte buffers track changed lines" (1,1) byteEdit
  count "byte-buffer save resets counts" (0,0) (markSaved byteEdit)
  byteRestored<-either (error . T.unpack) pure (restoreBuffer (snapshotBuffer byteEdit))
  check "byte-buffer provenance preserves exact bytes" (bufferBytes byteRestored==bufferBytes byteEdit && bufferLineChanges byteRestored==(1,1))
  let modeOriginal=newBuffer "λ\n"
  modeBytes<-either (error . T.unpack) pure (toggleByteMode modeOriginal)
  check "representation-only toggle remains clean" (not (dirty modeBytes))
  modeRestored<-either (error . T.unpack) pure (restoreBuffer (snapshotBuffer modeBytes))
  check "representation changes restore unchanged bytes" (bufferBytes modeRestored==bufferBytes modeOriginal && not (dirty modeRestored))
  let large=newBuffer (T.replicate 200000 "unchanged\n")
  _<-evaluate (bufferLineCount large)
  let smallEdit=replaceSelection (Selection 900000 900001) "X" large
  _<-evaluate (fst (bufferLineChanges smallEdit))
  before<-getAllocationCounter
  let clean=markSaved smallEdit
  _<-evaluate (bufferLineCount clean+fst (bufferLineChanges clean)+snd (bufferLineChanges clean))
  _<-evaluate (dirty smallEdit)
  after<-getAllocationCounter
  check "save skips unchanged finger-tree subtrees and dirty does not flatten contents" (before-after<1024*1024)
  count "sparse save has zero changes" (0,0) clean
  count "sparse undo across save touches only the inverse edit" (1,1) (undo clean)

  let manyDeleted=replaceSelection (Selection 0 1800000) "" large
  _<-evaluate (snd (bufferLineChanges manyDeleted))
  firstKey<-getAllocationCounter
  let typing=replaceSelection (Selection 0 0) "a" manyDeleted
  _<-evaluate (fst (bufferLineChanges typing)+snd (bufferLineChanges typing))
  afterKey<-getAllocationCounter
  check "typing beside deleted lines skips the tombstone subtree" (firstKey-afterKey<1024*1024)

  let replacements=replaceSelection (Selection 4 7) "TWO" (replaceSelection (Selection 0 3) "ONE" original)
  check "adjacent replacements put deletions before insertions" (map (\(kind,_,_)->kind) (bufferChangeRows replacements 0 4)==[DeletedLine,DeletedLine,AddedLine,AddedLine])
  let secondRestored=replaceSelection (Selection 4 7) "two" replacements
  count "restoring an interior hunk line cancels its separated tombstone" (1,1) secondRestored
  checkReview secondRestored
  check "hunk lookup returns the complete contiguous run" (changeHunkAt replacements 3==Just (0,4))
  let reverted=revertChangeHunk 2 replacements
  check "hunk revert restores original lines as one undoable edit" (contents reverted==contents original && bufferLineChanges reverted==(0,0) && contents (undo reverted)==contents replacements)
  let separated=replaceSelection (Selection 8 13) "THREE" (replaceSelection (Selection 0 3) "ONE" original)
      justFirst=revertChangeHunk 0 separated
  check "hunk revert leaves unrelated changes" (contents justFirst=="one\ntwo\nTHREE\n" && bufferLineChanges justFirst==(1,1))
  check "next hunk skips unchanged subtrees" (nextChangeHunk separated 2==Just (3,2))
  forM_ [original,changed,replacements,separated,inserted,deleted,newBuffer "abc",replaceSelection (Selection 0 3) "xyz" (newBuffer "abc"),replaceSelection (Selection 0 3) "" (newBuffer "abc")] checkReview

checkReview :: Buffer -> IO ()
checkReview b=do
  let rows=bufferChangeRows b 0 (changeRowCount b)
      visible=T.concat [text | (_,Just _,text)<-rows]
      baseline=T.concat [text | (kind,_,text)<-rows,kind/=AddedLine]
      canonical []=True
      canonical ((AddedLine,_,_):(DeletedLine,_,_):_)=False
      canonical (_:rest)=canonical rest
      projected=T.concat [if index+1==length rows || "\n" `T.isSuffixOf` text then text else text<>"\n" | (index,(_,_,text))<-zip [0::Int ..] rows]
  check "review rows retain exactly the live contents" (visible==contents b)
  check "review rows retain original baseline line order" (baseline==saved b)
  check "review runs put removed lines before inserted lines" (canonical rows)
  check "review length and range projection agree" (changeLength b==T.length projected && changeSlice b 0 (changeLength b)==projected)
  forM_ [0..bufferLength b] $ \position ->
    check "live positions survive review coordinate roundtrip" (changeToLiveOffset b (liveToChangeOffset b position)==position)
  forM_ [0..changeLength b] $ \position ->
    check "review character coordinates match displayed lines" (changeLineColumn b position==lineColumn projected position)
  forM_ [0..changeRowCount b] $ \row ->
    check "review line offsets match displayed lines" (changeLineOffset b row==lineOffset projected row)

  let indexed=zip [0::Int ..] rows
      align []=[]
      align ((index,(OriginalLine,_,_)):rest)=View.ViewRow (Just index) (Just index) 0:align rest
      align remaining=let (changed,rest)=span (\(_, (kind,_,_))->kind/=OriginalLine) remaining
                          left=[index | (index,(DeletedLine,_,_))<-changed]
                          right=[index | (index,(AddedLine,_,_))<-changed]
                          width=max (length left) (length right)
                          pad values=take width (map Just values++repeat Nothing)
                      in zipWith (\a z->View.ViewRow a z 0) (pad left) (pad right)++align rest
      projection=bufferViewProjection b
      actual=[View.viewRowAt View.SideBySideView projection row | row<-[0..View.viewRowCount View.SideBySideView projection-1]]
  check "side-by-side projection aligns original/current hunks and EOF padding" (actual==align indexed)
  forM_ (zip [0::Int ..] actual) $ \(visual,row) -> do
    forM_ (View.viewLeftRow row) $ \source ->
      check "saved-side rows map back to their aligned row" (View.viewRowForChange View.SideBySideView projection View.OriginalSide source==visual)
    forM_ (View.viewRightRow row) $ \source ->
      check "current-side rows map back to their aligned row" (View.viewRowForChange View.SideBySideView projection View.CurrentSide source==visual)
  let changedRows=[index | (index,(kind,_,_))<-indexed,kind/=OriginalLine]
      visibleRows=[index | (index,_)<-indexed,any (\changed -> abs (index-changed)<=2) changedRows]
      contexts _ []=[]
      contexts position selected@(first:rest)
        | position<first=View.ViewRow Nothing Nothing (first-position):contexts first selected
        | otherwise=View.ViewRow (Just first) (Just first) 0:contexts (first+1) rest
      expectedContext=if null changedRows then [] else contexts 0 visibleRows++[View.ViewRow Nothing Nothing (length rows-last visibleRows-1) | last visibleRows+1<length rows]
      actualContext=[View.viewRowAt View.OnlyChangesView projection row | row<-[0..View.viewRowCount View.OnlyChangesView projection-1]]
  check "context projection merges overlaps and makes omitted gaps unselectable" (actualContext==expectedContext)

-- A flat oracle deliberately applies original-offset edits from right to left.
batchChecks :: IO ()
batchChecks = do
  forM_ ["ab", "a\r\nb", "😀\nxλ"] $ \source -> do
    let original=newBuffer source
        size=T.length source
    forM_ [(a,z,c,d) | a<-[0..size], z<-[a..size], c<-[max (a+1) z..size], d<-[c..size]] $ \(a,z,c,d) ->
      forM_ [("", "界"), ("x\n", ""), ("λ", "😀\r\n")] $ \(first,second) -> do
        let edits=[(a,z,first),(c,d,second)]
            expected=foldr (\(lo,hi,text) old -> T.take lo old<>text<>T.drop hi old) source edits
        result<-either (error . T.unpack) pure (replaceRanges edits original)
        check "batch agrees with independent flat oracle" (contents result==expected)
        check "batch advances one revision only when text changes" (revision result==if source==expected then 0 else 1)
        check "batch is one undo/redo step" (contents (undo result)==source && contents (redo (undo result))==expected)
        checkReview result
  let original=newBuffer "one\ntwo\nthree\n"
      prior=replaceSelection (Selection 0 3) "ONE" original
  priorName<-evaluate prior >>= makeStableName
  forM_ [[],[(0,3,"ONE")],[(0,1,""),(1,3,"ONE")]] $ \edits -> do
    result<-either (error . T.unpack) pure (replaceRanges edits prior)
    resultName<-evaluate result >>= makeStableName
    check "empty and net-no-op batches retain identity, history and lastChange" (resultName==priorName && result==prior)
  forM_ [[(-1,0,"")],[(0,99,"")],[(2,1,"")],[(4,4,"x"),(0,0,"x")],[(0,4,"x"),(3,3,"y")],[(0,0,"x"),(0,1,"y")]] $ \edits ->
    check "invalid batch is rejected atomically" (case replaceRanges edits prior of Left _ -> True; Right _ -> False)
  check "byte batch rejects non-byte text" (case replaceRanges [(0,1,"x"),(2,2,"😀")] (newByteBuffer (BS.pack [65,66])) of Left _ -> True; Right _ -> False)
  changed<-either (error . T.unpack) pure (replaceRanges [(0,3,"1"),(8,13,"THREE!")] original)
  check "disjoint batch retains unchanged-line provenance" (bufferLineChanges changed==(2,2))
  check "batch reports enclosing replacement and inverse" (lastChange changed==Just (0,13,12) && lastChange (undo changed)==Just (0,12,13))
  recovered<-either (error . T.unpack) pure (restoreBuffer (snapshotBuffer changed))
  check "batch snapshot retains undo and change counts" (contents (undo recovered)==contents original && bufferLineChanges recovered==(2,2))
  let savedBatch=markSaved changed
  check "batch undo across save uses enclosing inverse correctly" (contents (undo savedBatch)==contents original && contents (redo (undo savedBatch))==contents changed && bufferLineChanges (redo (undo savedBatch))==(0,0))
  let large=newBuffer (T.replicate 200000 "unchanged\n")
  _<-evaluate (prepareBuffer large)
  before<-getAllocationCounter
  sparse<-either (error . T.unpack) pure (replaceRanges [(0,1,"X"),(1799991,1799992,"Y")] large)
  _<-evaluate (prepareBuffer sparse)
  after<-getAllocationCounter
  check "preparing sparse batch shares unchanged tree and does not flatten text" (before-after<2*1024*1024)
  check "sparse batch counts only changed lines" (bufferLineChanges sparse==(2,2))
