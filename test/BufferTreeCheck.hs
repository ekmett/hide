{-# LANGUAGE OverloadedStrings #-}
module BufferTreeCheck (checks) where

import Control.Monad (foldM, forM_, unless)
import qualified Data.Text as T
import THC.Edit.Buffer

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
  check "saved remains independently writable" (not (dirty (final {saved=contents final})))
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
      check "persistent old snapshots retain their text" (contents b == old)
      check "single undo and redo agree with Text" (not changed || (contents (undo next) == old && contents (redo (undo next)) == expected))
      check "undo and redo expose inverse change ranges" (not changed ||
        (lastChange (undo next) == Just (a,a+T.length inserted,z-a) && lastChange (redo (undo next)) == Just (a,z,T.length inserted)))
      pure (next,if changed then expected:history else history)
