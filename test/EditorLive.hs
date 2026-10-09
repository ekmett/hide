{-# LANGUAGE OverloadedStrings #-}
-- Compile after cabal build: cabal exec -- ghc -threaded -package hide test/EditorLive.hs -o /tmp/hide-live
-- Run with an installed HLS and its supported GHC on PATH; fixtures are disposable.
-- |
-- Module      : Main
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.List (findIndex)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import Hide.App (applyEffects)
import Hide.Files (filePath)
import Hide.Buffer
import Hide.Model
import Hide.Tooling

main :: IO ()
main = bracket temporary removePathForcibly $ \root -> do
  let file = root </> "Live.hs"
      good = "module Live where\n\nanswer :: Int\nanswer = 42\n\nuse :: Int\nuse = answer\n\n-- unsaved marker\n"
      bad = good <> "\nbroken :: Int\nbroken = True\n"
      useOffset = lineOffset good 6
  writeFile (root </> "hie.yaml") "cradle:\n  direct:\n    arguments:\n      - Live.hs\n"
  let warning="{-# OPTIONS_GHC -Wtype-defaults #-}\nmodule Live where\nwarn :: String\nwarn = show (read \"1\" + 1)\n"
  T.writeFile file warning
  (_,existing)<-applyEffects (initialDesktop (120,40)) [ReadPath file]
  withTooling $ \tooling -> do
    warned<-await "warning in pre-existing file without saving" (tickTooling tooling applyEffects)
      (any ((==2).diagnosticSeverity) . diagnostics) existing
    check "initial warnings do not need an editor save" (not (dirty (buffer warned)) && revision (buffer warned)==0)
    check "diagnostic copy retains original line breaks" (any (T.isInfixOf "\n" . diagnosticMessage) (diagnostics warned))
    disk<-T.readFile file
    check "initial diagnostics leave disk unchanged" (disk==warning)
  T.writeFile file bad
  (_,loaded) <- applyEffects (initialDesktop (120,40)) [ReadPath file]
  check "fixture loaded through App" (activeText loaded == bad)
  withTooling $ \tooling -> do
    let tick = tickTooling tooling applyEffects
        effects = toolingEffects tooling applyEffects
        command cmd desktop = uncurry (\updated pending -> snd <$> effects updated pending) (runCommand cmd desktop)
    diagnosed <- await "diagnostics pane" tick (\d -> problemsVisible d && any ((==1) . diagnosticSeverity) (diagnostics d)) loaded
    check "diagnostics carry current revision" (all ((== Just 0) . diagnosticVersion) (diagnostics diagnosed))
    let fixed = insertText good (fst (runCommand SelectAll diagnosed))
    cleared <- tick fixed
    check "edits immediately hide stale diagnostics" (null (diagnostics cleared))
    check "fix remains unsaved" (dirty (buffer cleared))
    let positioned = moveTo False 0 cleared
        w = fromJust (activeWindow positioned)
        pointer = fst (hoverAt (left (bounds w)+1+8) (top (bounds w)+1+6) positioned)
    hovered <- await "pointer hover type" tick (T.isInfixOf "Int" . typeHint) pointer
    check "pointer hover does not move caret" (cursor hovered == 0)
    requested <- command Definition (moveTo False (useOffset+8) hovered {hoverTarget=Nothing,typeHint=""})
    defined <- await "definition navigation" tick (\d -> fst (lineColumn (activeText d) (cursor d)) == 3) requested
    check "definition navigation preserves unsaved source" (activeText defined == good && dirty (buffer defined))
    let partial = editActive (\_ -> replaceSelection (Selection (useOffset+6) (useOffset+12)) "ans") (Just (useOffset+9)) defined
    completing <- command Complete partial
    choices <- await "completion dialog" tick isCompletion completing
    dg <- maybe (error "Missing completion dialog") pure (dialog choices)
    index <- case purpose dg of
      Completing _ _ _ items -> maybe (error ("No answer completion: " ++ show items)) pure (findIndex (\(Completion label _) -> "answer" `T.isPrefixOf` label) items)
      _ -> error "Unexpected dialog"
    let selected = dg {fields=[case field of ListBox label labels _ -> ListBox label labels index; _ -> field | field <- fields dg]}
        (chosen,pending) = submitDialog 0 selected choices {dialog=Just selected}
    (_,completed) <- effects chosen pending
    check "selected completion inserts answer" (activeText completed == good)
    check "completion is one undo transaction" (length (undoStack (buffer completed)) == length (undoStack (buffer partial))+1 && contents (undo (buffer completed)) == activeText partial)
    (_,renaming) <- effects (moveTo False (useOffset+8) completed) [LanguageRequest (RenameAt "renamedAnswer")]
    renamed <- await "asynchronous symbol rename" tick (T.isInfixOf "renamedAnswer" . activeText) renaming
    check "rename changes every reference and preserves unsaved text" (activeText renamed == T.replace "answer" "renamedAnswer" good && dirty (buffer renamed))
    check "rename is one undo transaction" (contents (undo (buffer renamed)) == good)
    onDisk <- T.readFile file
    check "language actions never save over disk" (onDisk == bad)
  crossFileRename root
  putStrLn "Editor live HLS checks passed: diagnostics, pointer hover, definition, completion, undo, async rename, unsaved preservation, cross-file rename"
  where
    temporary = do
      base <- getTemporaryDirectory
      (path, handle) <- openTempFile base "hide-live"
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path

-- Index the importing module, close it, then ask HLS to rename the exported name.
crossFileRename :: FilePath -> IO ()
crossFileRename parent = do
  let root = parent </> "cross-file"
      provider = root </> "Provider.hs"
      consumer = root </> "Consumer.hs"
      source = "module Provider where\n\nanswer :: Int\nanswer = 42\n"
      usage = "module Consumer where\n\nimport Provider (answer)\n\nuse :: Int\nuse = answer\n"
      savedUsage = usage <> "\n-- saved consumer change\n"
      unsaved = source <> "\n-- keep unsaved provider text\n"
  createDirectory root
  writeFile (root </> "hie.yaml") "cradle:\n  direct:\n    arguments:\n      - Provider.hs\n      - Consumer.hs\n"
  T.writeFile provider source
  T.writeFile consumer usage
  (_,loaded) <- applyEffects (initialDesktop (120,40)) [ReadPath consumer]
  withTooling $ \tooling -> do
    let tick = tickTooling tooling applyEffects
        effects = toolingEffects tooling applyEffects
    indexed <- await "cross-file consumer indexing" tick (T.isInfixOf "Int" . typeHint) (moveTo False (lineOffset usage 5+8) loaded)
    let editedConsumer = insertText "\n-- saved consumer change\n" (moveTo False (T.length usage) indexed)
        (saving,saveEffects) = runCommand Save editedConsumer
    (_,savedConsumer) <- effects saving saveEffects
    check "consumer saved at version one before closing" (revision (buffer savedConsumer) == 1 && not (dirty (buffer savedConsumer)))
    reindexed <- await "saved consumer reindexing" tick (T.isInfixOf "Int" . typeHint) (moveTo False (lineOffset savedUsage 5+8) savedConsumer {typeHint=""})
    let (closed,pending) = runCommand Close reindexed
    (_,closedEffects) <- effects closed pending
    synced <- tick closedEffects
    check "consumer closed before rename" (M.null (buffers synced))
    (_,opened) <- effects synced [ReadPath provider]
    let edited = insertText "\n-- keep unsaved provider text\n" (moveTo False (T.length source) opened)
        positioned = moveTo False (lineOffset unsaved 3+2) edited {typeHint=""}
    ready <- await "cross-file provider type" tick (T.isInfixOf "Int" . typeHint) positioned
    (_,requested) <- effects ready [LanguageRequest (RenameAt "crossAnswer")]
    renamed <- await "cross-file rename application" tick (T.isPrefixOf "Rename applied" . status) requested
    let findBuffer path = case [documentBuffer doc | doc <- M.elems (buffers renamed), fmap filePath (documentFile doc) == Just path] of
          b:_ -> b
          [] -> error ("Rename did not introduce expected buffer: " ++ path)
        changedProvider = findBuffer provider
        changedConsumer = findBuffer consumer
    check "cross-file rename preserves dirty provider source" (contents changedProvider == T.replace "answer" "crossAnswer" unsaved && dirty changedProvider)
    check "closed consumer opens as changed buffer" (contents changedConsumer == T.replace "answer" "crossAnswer" savedUsage && dirty changedConsumer)
    check "cross-file rename has one undo per buffer" (contents (undo changedProvider) == unsaved && contents (undo changedConsumer) == savedUsage && length (undoStack changedConsumer) == 1)
    diskProvider <- T.readFile provider
    diskConsumer <- T.readFile consumer
    check "cross-file rename leaves both files untouched on disk" (diskProvider == source && diskConsumer == savedUsage)


check :: String -> Bool -> IO ()
check label ok = unless ok (error label)

buffer :: Desktop -> Buffer
buffer = documentBuffer . fromJust . activeDocument

cursor :: Desktop -> Int
cursor = caret . selection . fromJust . activeWindow

isCompletion :: Desktop -> Bool
isCompletion d = case purpose <$> dialog d of Just Completing{} -> True; _ -> False

await :: String -> (Desktop -> IO Desktop) -> (Desktop -> Bool) -> Desktop -> IO Desktop
await label tick ready initial = do
  result <- timeout 60000000 (loop initial)
  maybe (error ("Timed out: " ++ label)) pure result
  where
    loop desktop = do
      updated <- tick desktop
      whenError updated
      if ready updated then pure updated else threadDelay 20000 >> loop updated
    whenError d = do
      unless (not ("HLS:" `T.isPrefixOf` status d)) (error (T.unpack (status d)))
      case dialog d of
        Just dg | purpose dg == Information -> error (T.unpack (dialogTitle dg <> ": " <> T.unwords (body dg)))
        _ -> pure ()
