{-# LANGUAGE OverloadedStrings #-}
-- | Adopt background filesystem observations into buffers and the Files pane.
--
-- Observation tokens follow disk baselines rather than edit revisions: edits must
-- not hide disk changes, and old observations must not undo a save. Clean existing
-- files reload with Undo; dirty changes and deletions become conflicts. Explicit
-- conflict decisions recheck buffer revision, baseline and disk. Keeping a buffer
-- retains save-conflict checks rather than accepting an unseen disk overwrite.
module Hide.Reconcile
  ( Reconciliation, withReconciliation, reconciliationEffects, tickReconciliation ) where

import Hide.FileIO (readFileBytes)

import Hide.Sidebar
import Control.Monad (foldM, unless)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, pathIsSymbolicLink)
import System.IO.Error (catchIOError, isDoesNotExistError, tryIOError)
import Hide.Browser (Entry)
import Hide.Buffer
import Hide.External
import Hide.Files
import Hide.Model

-- | Session-owned watcher subscriptions, baseline tokens and conflict state.
data Reconciliation = Reconciliation Watcher (IORef Tracking)
data Tracking = Tracking
  { watched :: M.Map Int (FileState, Int), generation :: Int
  , changedDisk :: M.Map Int (Either T.Text (Maybe BS.ByteString))
  , announced :: M.Map Int (Maybe BS.ByteString)
  , directoryEntries :: M.Map FilePath [Entry]
  }

-- | Scope the filesystem watcher and reconciliation state.
withReconciliation :: (Reconciliation -> IO a) -> IO a
withReconciliation action = withWatcher $ \watcher -> do
  state <- newIORef (Tracking M.empty 0 M.empty M.empty M.empty)
  action (Reconciliation watcher state)

-- Tokens belong to a disk baseline, not an edit revision: local edits must not
-- hide a disk update, while a queued pre-save snapshot must never undo a save.
synchronize :: Reconciliation -> Desktop -> IO ()
synchronize (Reconciliation watcher ref) desktop = do
  old <- readIORef ref
  let documents = [(bid,file) | (bid,doc) <- M.toList (buffers desktop),
        documentLabel doc==Nothing, Just file <- [documentFile doc]]
      retain (previousCounter, result) (bid,file) = case M.lookup bid (watched old) of
        Just pair@(previous,_) | previous==file -> (previousCounter, M.insert bid pair result)
        _ -> (previousCounter+1, M.insert bid (file,previousCounter+1) result)
      (counter,current) = foldl retain (generation old,M.empty) documents
      unchanged bid _ = M.lookup bid current==M.lookup bid (watched old) && M.member bid current
      directories = case sideTree desktop of
        Nothing -> []
        Just tree -> treeRoot tree : treeWatchPaths tree
      desiredDirs = S.fromList directories
  writeIORef ref old {watched=current,generation=counter,
    changedDisk=M.filterWithKey unchanged (changedDisk old),announced=M.filterWithKey unchanged (announced old),
    directoryEntries=M.restrictKeys (directoryEntries old) desiredDirs}
  watchPaths watcher [(filePath file,token) | (file,token) <- M.elems current] directories

-- | Wrap core effects so baseline changes are synchronized before/after handling.
reconciliationEffects :: Reconciliation -> (Desktop -> [Effect] -> IO (Bool,Desktop))
  -> Desktop -> [Effect] -> IO (Bool,Desktop)
reconciliationEffects runtime core = foldM apply . (False,)
  where
    apply state@(True,_) _ = pure state
    apply (_,desktop) effect = do
      synchronize runtime desktop
      result <- case effect of
        ReviewExternal -> (False,) <$> reviewPending runtime desktop
        ResolveConflict conflict action -> (False,) <$> resolve runtime conflict action desktop
        _ -> core desktop [effect]
      synchronize runtime (snd result)
      pure result

-- | Consume coalesced observations, reload clean files and present conflicts.
-- Adoption may construct replacement buffers; explicit decisions reread the disk.
tickReconciliation :: Reconciliation -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO Desktop
tickReconciliation runtime@(Reconciliation watcher _) core desktop = do
  synchronize runtime desktop
  observations <- pollObservations watcher
  updated <- foldM (observe runtime core) desktop observations
  synchronize runtime updated
  promptPending runtime updated

observe :: Reconciliation -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> Observation -> IO Desktop
observe runtime@(Reconciliation _ ref) core desktop observation = do
  tracking <- readIORef ref
  case observation of
    FileObserved path token bytes -> case owner tracking path token of
      Nothing -> pure desktop
      Just bid -> recordDisk runtime bid bytes desktop
    FileUnavailable path token err -> case owner tracking path token of
      Nothing -> pure desktop
      Just bid -> do
        modifyIORef' ref (\s -> s {changedDisk=M.insert bid (Left err) (changedDisk s)})
        pure desktop {status="Cannot check "<>T.pack path<>": "<>err}
    DirectoryObserved path entries -> do
      let cache=M.insert path entries (directoryEntries tracking)
      modifyIORef' ref (\s -> s {directoryEntries=cache})
      snd <$> core desktop [RefreshTree path entries]
    DirectoryUnavailable path err -> pure desktop {status="Cannot refresh "<>T.pack path<>": "<>err}
  where
    owner tracking path token = fst <$> find (\(_, (file,version)) -> filePath file==path && version==token) (M.toList (watched tracking))

recordDisk :: Reconciliation -> Int -> Maybe BS.ByteString -> Desktop -> IO Desktop
recordDisk (Reconciliation _ ref) bid bytes desktop = case M.lookup bid (buffers desktop) of
  Just doc | Just file <- documentFile doc -> do
    let forget s = s {changedDisk=M.delete bid (changedDisk s),announced=M.delete bid (announced s)}
    if bytes==diskBytes file then modifyIORef' ref forget >> pure desktop else do
      modifyIORef' ref (\s -> s {changedDisk=M.insert bid (Right bytes) (changedDisk s)})
      case bytes of
        Just raw | not (dirty (documentBuffer doc)) -> do
          modifyIORef' ref forget
          pure (reload bid file bytes raw desktop)
        _ -> pure desktop
  _ -> pure desktop

decode :: BS.ByteString -> Either T.Text T.Text
decode bytes
  | BS.elem 0 bytes = Left "Binary text contains NUL bytes; the buffer was preserved."
  | otherwise = either (const (Left "The file is not valid UTF-8; the buffer was preserved.")) Right (TE.decodeUtf8' bytes)

reload :: Int -> FileState -> Maybe BS.ByteString -> BS.ByteString -> Desktop -> Desktop
reload bid file bytes raw desktop = case M.lookup bid (buffers desktop) of
  Nothing -> desktop
  Just doc ->
    let original=documentBuffer doc
        loaded=case decode raw of Right text | not (byteMode original) -> newBuffer text; _ -> newByteBuffer raw
        replaced=replaceBuffer (byteMode loaded) (contents loaded) original
        fresh=markSaved replaced {revision=max (revision original+1) (revision replaced)}
        updated=restyle doc {documentFile=Just file {diskBytes=bytes},documentBuffer=fresh}
        clamp n=max 0 (min (bufferLength fresh) n)
        adjust window | bufferId window/=Just bid = window
                      | otherwise = window {selection=let Selection a c=selection window in Selection (clamp a) (clamp c),
                          scrollRow=max 0 (min (documentRows updated window-1) (scrollRow window)),
                          scrollColumn=max 0 (min (windowDocumentWidth updated window) (scrollColumn window))}
    in ensureVisible (normalizeDocumentViews bid desktop {buffers=M.insert bid updated (buffers desktop),windows=map adjust (windows desktop),
      status="Reloaded "<>T.pack (filePath file)<>"; Undo restores the previous buffer.",hoverTarget=Nothing,typeHint=""})

pendingConflict :: Tracking -> Desktop -> Int -> Maybe Conflict
pendingConflict tracking desktop bid = do
  Right bytes <- M.lookup bid (changedDisk tracking)
  doc <- M.lookup bid (buffers desktop)
  file <- documentFile doc
  if bytes==diskBytes file then Nothing
    else Just (Conflict bid (revision (documentBuffer doc)) file bytes)

showConflict :: Conflict -> Desktop -> Desktop
showConflict conflict desktop = desktop {dialog=Just (Dialog "File changed on disk" (DiskConflict conflict) [] 0
  ["Compare","Reload","Keep","Save as"]
  [T.take 54 (T.pack (filePath (conflictBaseline conflict))),
   if conflictDisk conflict==Nothing then "The file was deleted. Your buffer is preserved." else "Your buffer and the disk version are both preserved.",
   "Reload is undoable. Keep retains save conflict checks.",
   "File > Disk changes reopens this review."]),menu=Nothing,contextMenu=Nothing,
   buttonHover=Nothing,buttonPressed=Nothing,drag=Nothing,dragOriginal=Nothing,dragTabs=Nothing,tabDropTarget=Nothing}

promptPending :: Reconciliation -> Desktop -> IO Desktop
promptPending (Reconciliation _ ref) desktop
  | dialog desktop/=Nothing || menu desktop/=Nothing || contextMenu desktop/=Nothing || drag desktop/=Nothing = pure desktop
  | otherwise = do
      tracking <- readIORef ref
      case listToMaybe [conflict | bid <- M.keys (changedDisk tracking), Just conflict <- [pendingConflict tracking desktop bid],
           M.lookup bid (announced tracking)/=Just (conflictDisk conflict)] of
        Nothing -> pure desktop
        Just conflict -> do
          modifyIORef' ref (\s -> s {announced=M.insert (conflictBuffer conflict) (conflictDisk conflict) (announced s)})
          pure (showConflict conflict desktop)

reviewPending :: Reconciliation -> Desktop -> IO Desktop
reviewPending (Reconciliation watcher ref) desktop = do
  tracking <- readIORef ref
  let choices=maybe [] pure (activeWindow desktop >>= bufferId) ++ M.keys (changedDisk tracking)
  case listToMaybe [conflict | bid <- choices, Just conflict <- [pendingConflict tracking desktop bid]] of
    Just conflict -> do
      modifyIORef' ref (\s -> s {announced=M.insert (conflictBuffer conflict) (conflictDisk conflict) (announced s)})
      pure (showConflict conflict desktop)
    Nothing -> forceCheck watcher >> pure desktop {status="No observed disk changes. Checking open files again."}

resolve :: Reconciliation -> Conflict -> ConflictAction -> Desktop -> IO Desktop
resolve runtime@(Reconciliation watcher ref) conflict action desktop = case M.lookup bid (buffers desktop) of
  Just doc | documentFile doc==Just file,revision (documentBuffer doc)==conflictRevision conflict -> do
    current <- readCurrent (filePath file)
    case current of
      Left err -> pure (message "Cannot check disk version" [T.take 54 err,"Your buffer and its save baseline are unchanged."] desktop)
      Right bytes | bytes/=conflictDisk conflict -> do
        modifyIORef' ref (\s -> s {announced=M.delete bid (announced s)})
        updated <- recordDisk runtime bid bytes desktop
        promptPending runtime updated {status="Disk changed again; review the latest version."}
      Right bytes -> case action of
        KeepBuffer -> pure desktop {status="Buffer kept. Save still checks the disk; Save as preserves both versions."}
        CompareDisk -> pure ((addReadOnly ("Disk changes: "<>T.pack (filePath file))
          (comparison file (documentBuffer doc) bytes) desktop) {status="File > Disk changes returns to the resolution choices."})
        SaveConflictAs -> pure (prompt "Save local buffer as" (Saving bid Nothing)
          [Input "Name" (T.pack (filePath file<>".local")) (length (filePath file)+6)] desktop)
        ReloadDisk -> pure (reload bid file bytes (fromMaybe BS.empty bytes) desktop)
  _ -> do
    modifyIORef' ref (\s -> s {announced=M.delete bid (announced s)})
    forceCheck watcher
    promptPending runtime desktop {status="Buffer changed since the prompt; review it again."}
  where
    bid=conflictBuffer conflict
    file=conflictBaseline conflict

-- User-requested decisions recheck the captured disk snapshot. Periodic ticks
-- never read files here; the external observer owns their filesystem work.
readCurrent :: FilePath -> IO (Either T.Text (Maybe BS.ByteString))
readCurrent path = either (Left . T.pack . show) Right <$> tryIOError (do
  checkPath
  bytes <- catchIOError (Just <$> readFileBytes path) (\err -> if isDoesNotExistError err then pure Nothing else ioError err)
  checkPath
  pure bytes)
  where
    checkPath = do
      resolved <- canonicalizePath path
      link <- catchIOError (pathIsSymbolicLink path) (\err -> if isDoesNotExistError err then pure False else ioError err)
      unless (resolved==path && not link) (ioError (userError "The path now points through a symbolic link; reopen it first."))

comparison :: FileState -> Buffer -> Maybe BS.ByteString -> T.Text
comparison file buffer disk = T.concat
  ["Disk change review: ",T.pack (filePath file),"\n\n",section "BASE — last loaded or saved" (render (diskBytes file)),
   section "LOCAL — current editor buffer" (render (Just (bufferBytes buffer))),section "DISK — observed external version" (render disk)]
  where
    section title text="===== "<>title<>" =====\n"<>text<>"\n===== END "<>title<>" =====\n\n"
    render Nothing="[File does not exist]"
    render (Just bytes)=either (const ("[Non-text bytes]\n"<>T.pack (show bytes))) id (decode bytes)
