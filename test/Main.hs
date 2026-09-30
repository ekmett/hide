{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad (unless)
import qualified Data.Text as T
import THC.Edit.App (demoDesktop)
import qualified FilesCheck
import THC.Edit.Render
import THC.Edit.Buffer
import THC.Edit.Syntax
import THC.Edit.Model
import qualified Graphics.Vty as V
import qualified Data.Map.Strict as M

check :: String -> Bool -> IO ()
check name ok = unless ok (error name)

main :: IO ()
main = do
  let b = newBuffer "hello\nworld"
      edited = replaceSelection (Selection 0 5) "λ" b
  check "selection replacement" (contents edited == "λ\nworld")
  check "undo restores original" (contents (undo edited) == "hello\nworld")
  check "redo restores replacement" (contents (redo (undo edited)) == "λ\nworld")
  check "savepoint dirty tracking" (not (dirty (undo edited)) && dirty edited)
  check "paste is one undo action" (contents (undo (replaceSelection (Selection 5 5) "\na\nb" b)) == contents b)
  check "trailing newline position" (lineColumn "a\n" 2 == (1,0))
  check "tab and wide display columns" (displayColumn "\t界x" 2 == 10)
  check "click inside wide glyph" (columnOffset "\t界x" 9 == 1)
  check "click after wide glyph" (columnOffset "\t界x" 10 == 2)
  check "combining sequence moves together" (nextCharacter "e\x0301x" 0 == 2)
  check "nested comment stays a comment" (all ((== Comment) . snd) (highlight "{- x {- y -} z -}"))
  check "apostrophe identifiers" (map snd (highlight "foldl' x") == replicate 6 Plain ++ [Plain,Plain])
  check "highlight preserves all characters" (T.pack (map fst (highlight "x = \"hi\" -- ok\n")) == "x = \"hi\" -- ok\n")
  let d = initialDesktop (80,25)
      key k ms s = fst (handleEvent (V.EvKey k ms) s)
      n = fst (runCommand New d)
      modal = fst (runCommand About n)
      clicked = fst (handleEvent (V.EvMouseDown 20 10 V.BLeft []) modal)
      typed = key (V.KChar 'x') [] modal
  check "modal blocks text" (buffers typed == buffers modal)
  check "modal blocks underlying focus" (map windowId (windows clicked) == map windowId (windows modal))
  check "escape restores editor focus" (dialog (key V.KEsc [] modal) == Nothing)
  let changed = key (V.KChar 'x') [] n
      split = fst (runCommand SplitVertical changed)
  check "split shares buffer" (length (windows split) == 2 && M.size (buffers split) == 1)
  let shared = key (V.KChar 'y') [] split
  check "split edits same buffer" (maybe False ((== "xy") . contents . documentBuffer) (activeDocument shared))
  let quitting = fst (runCommand Quit shared)
  check "dirty quit asks" (dialog quitting /= Nothing)
  check "cancel quit retains text" (buffers (key V.KEsc [] quitting) == buffers shared)
  let menuState = key (V.KFun 10) [] d
  check "F10 activates menu" (menu menuState /= Nothing)
  let small = fst (handleEvent (V.EvResize 30 10) split)
  check "resize keeps frames inside desktop" (all (\w -> let Rect x y width height = bounds w in x >= 0 && y >= 1 && x+width <= 30 && y+height <= 9) (windows small))
  check "snapshot keeps 25 rows" (length (T.lines (snapshot d)) == 25)
  check "snapshot has menu" ("File" `T.isInfixOf` snapshot d)
  check "source text survives zero horizontal scroll" ("factorial" `T.isInfixOf` snapshot demoDesktop)
  check "CRLF moves as one newline" (nextCharacter "a\r\nb" 1 == 3 && previousCharacter "a\r\nb" 3 == 1)
  check "word-left crosses punctuation" (wordLeft "foo.bar" 4 == 3)
  let twoDirty = insertText "second" (fst (runCommand New shared))
      asked = fst (runCommand Quit twoDirty)
      discarded = case dialog asked of Just dg -> fst (submitDialog 1 dg asked); Nothing -> asked
      cancelled = key V.KEsc [] discarded
  check "discard then cancel cannot bless unsaved text" (all (dirty . documentBuffer) (M.elems (buffers cancelled)))
  let star = key (V.KChar 'b') [] (key (V.KChar 'k') [V.MCtrl] changed {wordStar=True})
      starMoved = key (V.KChar 's') [V.MCtrl] star
      starBlock = key (V.KChar 'k') [] (key (V.KChar 'k') [V.MCtrl] starMoved)
  check "WordStar block marker survives movement" (maybe False ((== (0,1)) . ordered . selection) (activeWindow starBlock))
  let repeated = addDocument Nothing (newBuffer "aaaa") d
      repSplit = fst (runCommand SplitVertical repeated)
      positioned = repSplit {windows=case windows repSplit of w:v:rest -> w:v {selection=Selection 2 2}:rest; ws -> ws}
      inserted = insertText "a" positioned
  check "repeated text rebase uses actual edit" (map (caret . selection) (windows inserted) == [1,3])
  let undone = fst (runCommand Undo inserted)
  check "undo rebases other split cursor" (map (caret . selection) (windows undone) == [0,2])
  let resizeClick = fst (handleEvent (V.EvMouseDown 78 23 V.BLeft []) n)
  check "visible resize grip captures drag" (case drag resizeClick of Just Resizing{} -> True; _ -> False)
  let abc = moveTo False 1 (addDocument Nothing (newBuffer "abc") d)
  check "wrapped search spans old cursor" (maybe False ((== (0,3)) . ordered . selection) (activeWindow (findText "abc" abc)))
  check "control placeholders use one column" (displayColumn "a\SOHb" 2 == 2 && columnOffset "a\SOHb" 2 == 2)
  let crowded = iterate (fst . runCommand New) d !! 6
      tiled = fst (runCommand Tile crowded)
  check "tile refuses unusably short windows" (map bounds (windows tiled) == map bounds (windows crowded))
  let gallery = fst (runCommand Gallery (initialDesktop (80,12)))
      scrolled = iterate (key (V.KChar '\t') []) gallery !! 3
      borderClick = case dialog scrolled of
        Just dg -> let Rect x y _ _ = dialogRect scrolled dg in fst (handleEvent (V.EvMouseDown (x+4) y V.BLeft []) scrolled)
        Nothing -> scrolled
  check "clipped dialog fields cannot receive border clicks" (dialog borderClick == dialog scrolled)
  FilesCheck.checks
  putStrLn "editor checks passed"
