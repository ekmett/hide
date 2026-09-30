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
  FilesCheck.checks
  putStrLn "editor checks passed"
