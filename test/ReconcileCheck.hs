-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : ReconcileCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module ReconcileCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally)
import Control.Monad (foldM, unless)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.Directory
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, openBinaryTempFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Timeout (timeout)
import Hide.App (applyEffects)
import Hide.Buffer
import Hide.BufferView
import Hide.TextPresentation (prepareTextPresentations)
import Hide.Files
import Hide.FileIO (replaceFile)
import Hide.Model
import Hide.Reconcile
import qualified Hide.AgentFiles as AgentFiles

checks :: IO ()
checks = bracket temporary removePathForcibly $ \dir -> withReconciliation $ \runtime -> do
  let path = dir </> "source.hs"
      tick = tickReconciliation runtime applyEffects
      effects = reconciliationEffects runtime applyEffects
      apply desktop requests = snd <$> effects desktop requests
      source desktop = maybe (error "missing source") id (M.lookup 1 (buffers desktop))
      sourceBuffer = documentBuffer . source
      disk desktop = documentFile (source desktop) >>= diskBytes
      conflict desktop = case purpose <$> dialog desktop of
        Just (DiskConflict value) -> value
        _ -> error "missing disk conflict"
      choose action desktop = apply desktop {dialog=Nothing} [ResolveConflict (conflict desktop) action]
      isConflict desktop = case purpose <$> dialog desktop of Just DiskConflict{} -> True; _ -> False
  externalWrite path "original\n"
  (file, buffer) <- loadFile path >>= either error pure
  let initial = fst (runCommand SplitVertical (addDocument (Just file) buffer (initialDesktop (100,30))))
      selected = initial {windows=map (\w -> w {selection=Selection 2 8,scrollRow=20,scrollColumn=20}) (windows initial)}
  registered <- tick selected
  externalWrite path "new\n"
  clean <- await tick ((== "new\n") . contents . sourceBuffer) registered
  check "clean buffer reloads with exact disk baseline" (disk clean==Just "new\n" && not (dirty (sourceBuffer clean)))
  check "reload preserves undo and increases revision" (contents (undo (sourceBuffer clean))=="original\n" && revision (sourceBuffer clean)>revision buffer)
  check "all split views have bounded selection and scroll" (all (\w -> caret (selection w)<=4 && scrollRow w<=1 && scrollColumn w<=3) (windows clean))
  check "reload invalidates syntax for background highlighting" (null (documentHighlight (source clean)) && documentSourceRows (source clean)==Nothing)
  let dirtyDesktop = insertText "local " (moveTo False 0 clean)
      local = contents (sourceBuffer dirtyDesktop)
  externalWrite path "disk version\n"
  prompted <- await tick isConflict dirtyDesktop
  check "dirty external change retains local text and baseline" (contents (sourceBuffer prompted)==local && disk prompted==Just "new\n")
  compared <- choose CompareDisk prompted
  check "compare contains base local and disk without modifying source" (all (`T.isInfixOf` activeText compared) ["BASE", "LOCAL", "DISK",local,"disk version\n"] && contents (sourceBuffer compared)==local && maybe False ((/=Nothing) . documentLabel) (activeDocument compared))
  afterCompare <- tick compared
  check "same disk version does not repeatedly prompt" (not (isConflict afterCompare))
  reviewed <- apply (focusWindow 1 afterCompare) [ReviewExternal]
  kept <- choose KeepBuffer reviewed
  check "keep retains save conflict protection" (disk kept==Just "new\n" && contents (sourceBuffer kept)==local)
  refused <- saveFile (maybe (error "no file") id (documentFile (source kept))) (sourceBuffer kept)
  check "ordinary save still refuses external overwrite" (either (const True) (const False) refused)
  reviewReload <- apply kept [ReviewExternal]
  reloaded <- choose ReloadDisk reviewReload
  check "explicit reload is undoable and clean" (contents (sourceBuffer reloaded)=="disk version\n" && contents (undo (sourceBuffer reloaded))==local && not (dirty (sourceBuffer reloaded)))
  let afterUndo = fst (runCommand Undo (focusWindow 1 reloaded))
  check "undo after reload keeps newer disk baseline" (disk afterUndo==Just "disk version\n" && dirty (sourceBuffer afterUndo))
  externalWrite path "second disk version\n"
  newConflict <- await tick isConflict afterUndo
  let captured = conflict newConflict
      edited = insertText "more " newConflict {dialog=Nothing}
  staleEdit <- apply edited [ResolveConflict captured ReloadDisk]
  check "stale action cannot discard a subsequent edit" (contents (sourceBuffer staleEdit)==contents (sourceBuffer edited))
  current <- apply staleEdit {dialog=Nothing} [ReviewExternal]
  externalWrite path "changed after prompt\n"
  staleDisk <- choose ReloadDisk current
  check "stale action cannot reload a different disk snapshot" (contents (sourceBuffer staleDisk)==contents (sourceBuffer edited))
  latest <- apply staleDisk {dialog=Nothing} [ReviewExternal]
  saveAs <- choose SaveConflictAs latest
  check "conflict save-as targets original source buffer" (case purpose <$> dialog saveAs of Just (Saving 1 Nothing) -> True; _ -> False)
  let copyPath = dir </> "copy.hs"
  savedCopy <- apply saveAs {dialog=Nothing} [SaveDocument 1 (Just copyPath) Nothing]
  originalBytes <- BS.readFile path
  copyBytes <- BS.readFile copyPath
  check "save-as preserves both local and external versions" (originalBytes=="changed after prompt\n" && copyBytes==BS.pack (map (fromIntegral . fromEnum) (T.unpack (contents (sourceBuffer savedCopy)))) && fmap filePath (documentFile (source savedCopy))==Just copyPath)
  removeFile copyPath
  deleted <- await tick isConflict savedCopy
  check "deleting a clean file prompts without erasing buffer" (contents (sourceBuffer deleted)==contents (sourceBuffer savedCopy))
  restored <- choose ReloadDisk deleted
  check "explicit reload of deletion preserves old content in undo" (contents (sourceBuffer restored)=="" && disk restored==Nothing && contents (undo (sourceBuffer restored))==contents (sourceBuffer savedCopy))
  let commands = [(AgentOptions,"options"),(Conversation,"show"),(AgentCancel,"cancel"),(AgentCopyRaw,"copy")]
  check "agent commands route generic effects" (all (\(command,action) -> snd (runCommand command restored)==[AgentAction action []]) commands)
  let agentDialog = Dialog "Agent" (AgentDialog "permission") [Input "Value" "x" 1,CheckBox "Allowed" True,ListBox "Choice" ["a","b"] 1] 3 ["Allow","Deny"] []
  check "agent dialog submits button and field values" (snd (handleEvent (V.EvKey V.KEnter []) restored {dialog=Just agentDialog})==[AgentAction "permission" ["0","x","true","1"]])
  binaryReload dir
  mapM_ (queuedSave dir) [False,True]
  putStrLn "external reconciliation checks passed"
  where
    check label condition = unless condition (error label)
    temporary = do
      base <- getTemporaryDirectory
      (path, handle) <- openBinaryTempFile base "hide-reconcile"
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path

-- External binary replacements use the same baseline and conflict protocol as text.
binaryReload :: FilePath -> IO ()
binaryReload dir = withReconciliation $ \runtime -> do
  let path=dir </> "binary.md"
      tick=tickReconciliation runtime applyEffects
      effects=reconciliationEffects runtime applyEffects
      check name ok=unless ok (error name)
      buffer=maybe (error "missing binary buffer") documentBuffer . activeDocument
      raw=BS.pack [0,255,65]
  externalWrite path "text"
  (file,b)<-loadFile path >>= either error pure
  preview<-prepareTextPresentations (fst (runCommand (SetBufferView MarkdownView) (addDocument (Just file) b (initialDesktop (80,25)))))
  opened<-tick preview
  externalWrite path raw
  reloaded<-await tick ((==raw) . bufferBytes . buffer) opened
  check "binary replacement retires Markdown preview" (maybe False ((==CurrentView).bufferView) (activeWindow reloaded) && M.null (windowPresentations reloaded))
  check "binary external reload is clean lossless and undoable" (byteMode (buffer reloaded) && not (dirty (buffer reloaded)) && bufferBytes (undo (buffer reloaded))=="text" && not (byteMode (undo (buffer reloaded))))
  let edited=fst (handleEvent (V.EvKey (V.KChar '1') []) reloaded)
      local=bufferBytes (buffer edited)
      changed=BS.pack [255,0,128]
  externalWrite path changed
  conflicted<-await tick (\d -> case purpose <$> dialog d of Just DiskConflict{} -> True; _ -> False) edited
  check "dirty binary reload retains local bytes" (bufferBytes (buffer conflicted)==local)
  case purpose <$> dialog conflicted of
    Just (DiskConflict conflict) -> do
      (_,resolved)<-effects conflicted {dialog=Nothing} [ResolveConflict conflict ReloadDisk]
      check "explicit binary reload updates baseline and undo bytes" (bufferBytes (buffer resolved)==changed && not (dirty (buffer resolved)) && bufferBytes (undo (buffer resolved))==local && (activeDocument resolved >>= documentFile >>= diskBytes)==Just changed)
    _ -> error "missing binary conflict"

-- Register without polling, leaving the worker's initial old-byte observation
-- queued while the save updates the baseline. ACP writes bypass the ordinary
-- save effect, so the next tick must invalidate their old token too.
queuedSave :: FilePath -> Bool -> IO ()
queuedSave dir agent = withReconciliation $ \runtime -> do
  let path=dir </> if agent then "queued-agent.hs" else "queued-save.hs"
      tick=tickReconciliation runtime applyEffects
      effects=reconciliationEffects runtime applyEffects
      check label condition=unless condition (error label)
  externalWrite path "old baseline\n"
  (file,buffer)<-loadFile path >>= either error pure
  let edited=insertText "new " (addDocument (Just file) buffer (initialDesktop (100,30)))
      version=maybe (error "missing buffer") (revision . documentBuffer) (activeDocument edited)
      staleChoice=Conflict 1 version file (Just "stale external bytes\n")
  (_,registered)<-effects edited [ReviewExternal]
  snapshot<-AgentFiles.captureFile dir path registered >>= either (error . T.unpack) pure
  threadDelay 100000
  savedDesktop<-if agent
    then AgentFiles.acceptWrite snapshot "new old baseline\n" registered >>= either (error . T.unpack) pure
    else snd <$> effects registered [SaveDocument 1 Nothing Nothing]
  let expected=activeText savedDesktop
      verify desktop=do
        next<-tick desktop
        check "queued old observation cannot undo a completed save"
          (activeText next==expected && (activeDocument next >>= documentFile >>= diskBytes)==Just "new old baseline\n" && dialog next==Nothing)
        threadDelay 10000
        pure next
  settled<-foldM (\desktop _ -> verify desktop) savedDesktop ([1..20] :: [Int])
  (_,stale)<-effects settled [ResolveConflict staleChoice ReloadDisk]
  check "captured pre-save baseline cannot resolve against saved buffer" (activeText stale==expected)

await :: (Desktop -> IO Desktop) -> (Desktop -> Bool) -> Desktop -> IO Desktop
await tick ready desktop = do
  result <- timeout 5000000 (go desktop)
  maybe (error "timed out waiting for external reconciliation") pure result
  where
    go current = do
      next <- tick current
      if ready next then pure next else threadDelay 10000 >> go next

-- A separate editor can replace a file while our watcher is reading it. Using
-- another handle in this process instead hits GHC's process-local file lock.
externalWrite :: FilePath -> BS.ByteString -> IO ()
externalWrite path bytes = bracket
  (openBinaryTempFile (takeDirectory path) ".reconcile-write-")
  (\(temporary, handle) -> hClose handle `finally`
    catchIOError (removeFile temporary) (\err -> unless (isDoesNotExistError err) (ioError err))) $
  \(temporary, handle) -> do
    BS.hPut handle bytes
    hClose handle
    replaceFile temporary path
