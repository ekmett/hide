-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.Reconcile
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Adopt background filesystem observations into buffers and the Files pane.
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
import Control.Monad (foldM, unless, forever)
import Control.Concurrent.Async (withAsync, link)
import Control.Concurrent.STM
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, pathIsSymbolicLink)
import System.Mem.StableName (StableName, makeStableName)
import System.IO.Error (catchIOError, isDoesNotExistError, tryIOError)
import Hide.Browser (Entry)
import Hide.Buffer
import Hide.External
import Hide.Files
import Hide.Model

-- | Session-owned watcher subscriptions, baseline tokens and conflict state.
data Reconciliation = Reconciliation Watcher (IORef Tracking) (TVar Preparation)
-- Buffer, baseline token and request serial identify the exact captured input.
data ReloadRequest = ReloadRequest Int Int Int (StableName Buffer) Buffer FileState BS.ByteString
data ReloadResult = ReloadResult ReloadRequest Buffer
data Preparation = Preparation (M.Map Int ReloadRequest) (M.Map Int ReloadResult)
data Tracking = Tracking
  { watched :: M.Map Int (FilePath, StableName FileState, Int), generation :: Int
  , subscriptions :: M.Map FilePath ([Int],Int), scheduled :: M.Map Int Int
  , changedDisk :: M.Map Int (Either T.Text (Maybe BS.ByteString))
  , announced :: M.Map Int (Maybe BS.ByteString)
  , directoryEntries :: M.Map FilePath [Entry]
  }

-- | Scope the filesystem watcher and reconciliation state.
withReconciliation :: (Reconciliation -> IO a) -> IO a
withReconciliation action = withWatcher $ \watcher -> do
  state <- newIORef (Tracking M.empty 0 M.empty M.empty M.empty M.empty M.empty)
  work<-newTVarIO (Preparation M.empty M.empty)
  withAsync (prepareReloads work) $ \worker->do
    link worker
    action (Reconciliation watcher state work)

-- Tokens belong to a disk baseline, not an edit revision: local edits must not
-- hide a disk update, while a queued pre-save snapshot must never undo a save.
synchronize :: Reconciliation -> Desktop -> IO ()
synchronize (Reconciliation watcher ref work) desktop = do
  old <- readIORef ref
  documents <- mapM (\(bid,file)->do
    identity<-makeStableName =<< evaluate file
    pure (bid,filePath file,identity))
    [(bid,file) | (bid,doc) <- M.toList (buffers desktop),
      documentLabel doc==Nothing, Just file <- [documentFile doc]]
  let retain (previousCounter, result) (bid,path,identity) = case M.lookup bid (watched old) of
        Just pair@(previousPath,previous,_) | previousPath==path && previous==identity -> (previousCounter, M.insert bid pair result)
        _ -> (previousCounter+1, M.insert bid (path,identity,previousCounter+1) result)
      (baselineCounter,current) = foldl retain (generation old,M.empty) documents
      -- One disk read serves every buffer for a path. Any owner/baseline change
      -- retires the shared observation token before the next adoption.
      grouped=M.fromListWith (++) [(path,[token]) | (path,_,token)<-M.elems current]
      subscribe (previousCounter,result) (path,tokens)=case M.lookup path (subscriptions old) of
        Just pair@(previous,_) | previous==tokens->(previousCounter,M.insert path pair result)
        _->(previousCounter+1,M.insert path (tokens,previousCounter+1) result)
      (counter,currentSubscriptions)=foldl subscribe (baselineCounter,M.empty) (M.toList grouped)
      unchanged bid _ = M.lookup bid current==M.lookup bid (watched old) && M.member bid current
      directories = case sideTree desktop of
        Nothing -> []
        Just tree -> treeRoot tree : treeWatchPaths tree
      desiredDirs = S.fromList directories
  writeIORef ref old {watched=current,generation=counter,subscriptions=currentSubscriptions,
    changedDisk=M.filterWithKey unchanged (changedDisk old),announced=M.filterWithKey unchanged (announced old),
    scheduled=M.filterWithKey unchanged (scheduled old),
    directoryEntries=M.restrictKeys (directoryEntries old) desiredDirs}
  atomically $ do
    Preparation pending ready<-readTVar work
    let valid (ReloadRequest bid token serial _ _ _ _)=
          maybe False (\(_,_,currentToken)->currentToken==token) (M.lookup bid current) &&
          M.lookup bid (scheduled old)==Just serial
    writeTVar work $! Preparation (M.filter valid pending) (M.filter (\(ReloadResult request _)->valid request) ready)
  watchPaths watcher [(path,token) | (path,(_,token)) <- M.toList currentSubscriptions] directories

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
-- Automatic replacement buffers arrive prepared; explicit decisions reread disk.
tickReconciliation :: Reconciliation -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO Desktop
tickReconciliation runtime@(Reconciliation watcher _ _) core desktop = do
  synchronize runtime desktop
  observations <- pollObservations watcher
  observed <- foldM (observe runtime core) desktop observations
  updated <- adoptReloads runtime observed
  synchronize runtime updated
  promptPending runtime updated

observe :: Reconciliation -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> Observation -> IO Desktop
observe runtime@(Reconciliation _ ref _) core desktop observation = do
  tracking <- readIORef ref
  case observation of
    FileObserved path token bytes ->
      foldM (\d bid->recordObserved runtime bid bytes d) desktop (owners tracking path token)
    FileUnavailable path token err -> case owners tracking path token of
      [] -> pure desktop
      bids -> do
        mapM_ (retireReload runtime) bids
        modifyIORef' ref (\s -> s {changedDisk=foldr (\bid->M.insert bid (Left err)) (changedDisk s) bids})
        pure desktop {status="Cannot check "<>T.pack path<>": "<>err}
    DirectoryObserved path entries -> do
      let cache=M.insert path entries (directoryEntries tracking)
      modifyIORef' ref (\s -> s {directoryEntries=cache})
      snd <$> core desktop [RefreshTree path entries]
    DirectoryUnavailable path err -> pure desktop {status="Cannot refresh "<>T.pack path<>": "<>err}
  where
    owners tracking path token=case M.lookup path (subscriptions tracking) of
      Just (_,version) | version==token->[bid | (bid,(watchedPath,_,_))<-M.toList (watched tracking),watchedPath==path]
      _->[]

-- One session worker owns replacement decoding, line trees and Undo preparation.
-- Pending work coalesces by buffer; no prepared Desktop can overwrite live views.
prepareReloads :: TVar Preparation -> IO ()
prepareReloads work=forever $ do
  request@(ReloadRequest bid _ _ _ original _ raw)<-atomically $ do
    Preparation pending ready<-readTVar work
    case M.minViewWithKey pending of
      Nothing->retry
      Just ((_,request),rest)->writeTVar work (Preparation rest ready) >> pure request
  let fresh=prepareReload original raw
  _<-evaluate (prepareBuffer fresh)
  atomically $ do
    Preparation pending ready<-readTVar work
    writeTVar work $! Preparation pending (M.insert bid (ReloadResult request fresh) ready)

recordObserved :: Reconciliation -> Int -> Maybe BS.ByteString -> Desktop -> IO Desktop
recordObserved runtime@(Reconciliation _ ref work) bid bytes desktop=case M.lookup bid (buffers desktop) of
  Just doc | Just file<-documentFile doc,Just raw<-bytes,not (dirty (documentBuffer doc))->do
    if bytes==diskBytes file then recordDisk runtime bid bytes desktop else do
      tracking<-readIORef ref
      case M.lookup bid (watched tracking) of
        Nothing->pure desktop
        Just (_,_,token)->do
          original<-evaluate (documentBuffer doc)
          identity<-makeStableName original
          let serial=generation tracking+1
              request=ReloadRequest bid token serial identity original file raw
          writeIORef ref tracking {generation=serial,scheduled=M.insert bid serial (scheduled tracking),
            changedDisk=M.delete bid (changedDisk tracking),announced=M.delete bid (announced tracking)}
          atomically $ do
            Preparation pending ready<-readTVar work
            writeTVar work $! Preparation (M.insert bid request pending) (M.delete bid ready)
          pure desktop
  _->recordDisk runtime bid bytes desktop

adoptReloads :: Reconciliation -> Desktop -> IO Desktop
adoptReloads runtime@(Reconciliation _ ref work) desktop=do
  completed<-atomically $ do
    Preparation pending ready<-readTVar work
    writeTVar work $! Preparation pending M.empty
    pure (M.elems ready)
  foldM adopt desktop completed
  where
    adopt current (ReloadResult (ReloadRequest bid token serial captured _ file raw) fresh)=do
      tracking<-readIORef ref
      case (M.lookup bid (watched tracking),M.lookup bid (buffers current)) of
        (Just (_,_,currentToken),Just doc)
          | currentToken==token,M.lookup bid (scheduled tracking)==Just serial->do
              identity<-makeStableName =<< evaluate (documentBuffer doc)
              if identity==captured then do
                modifyIORef' ref (\s->s {scheduled=M.delete bid (scheduled s)})
                pure (adoptReload bid file (Just raw) fresh current)
              else recordObserved runtime bid (Just raw) current
        _->pure current

recordDisk :: Reconciliation -> Int -> Maybe BS.ByteString -> Desktop -> IO Desktop
recordDisk runtime@(Reconciliation _ ref _) bid bytes desktop = do
  retireReload runtime bid
  case M.lookup bid (buffers desktop) of
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

retireReload :: Reconciliation -> Int -> IO ()
retireReload (Reconciliation _ ref work) bid=do
  modifyIORef' ref (\s->s {scheduled=M.delete bid (scheduled s)})
  atomically $ do
    Preparation pending ready<-readTVar work
    writeTVar work $! Preparation (M.delete bid pending) (M.delete bid ready)

decode :: BS.ByteString -> Either T.Text T.Text
decode bytes
  | BS.elem 0 bytes = Left "Binary text contains NUL bytes; the buffer was preserved."
  | otherwise = either (const (Left "The file is not valid UTF-8; the buffer was preserved.")) Right (TE.decodeUtf8' bytes)

-- Explicit conflict resolution retains its checked, synchronous action contract.
reload :: Int -> FileState -> Maybe BS.ByteString -> BS.ByteString -> Desktop -> Desktop
reload bid file bytes raw desktop=case M.lookup bid (buffers desktop) of
  Nothing->desktop
  Just doc->adoptReload bid file bytes (prepareReload (documentBuffer doc) raw) desktop

prepareReload :: Buffer -> BS.ByteString -> Buffer
prepareReload original raw=
  let loaded=case decode raw of Right text | not (byteMode original)->newBuffer text; _->newByteBuffer raw
      replaced=replaceBuffer (byteMode loaded) (contents loaded) original
  in markSaved replaced {revision=max (revision original+1) (revision replaced)}

adoptReload :: Int -> FileState -> Maybe BS.ByteString -> Buffer -> Desktop -> Desktop
adoptReload bid file bytes fresh desktop=case M.lookup bid (buffers desktop) of
  Nothing->desktop
  Just doc->
    let updated=restyle doc {documentFile=Just file {diskBytes=bytes},documentBuffer=fresh}
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
promptPending (Reconciliation _ ref _) desktop
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
reviewPending (Reconciliation watcher ref _) desktop = do
  tracking <- readIORef ref
  let choices=maybe [] pure (activeWindow desktop >>= bufferId) ++ M.keys (changedDisk tracking)
  case listToMaybe [conflict | bid <- choices, Just conflict <- [pendingConflict tracking desktop bid]] of
    Just conflict -> do
      modifyIORef' ref (\s -> s {announced=M.insert (conflictBuffer conflict) (conflictDisk conflict) (announced s)})
      pure (showConflict conflict desktop)
    Nothing -> forceCheck watcher >> pure desktop {status="No observed disk changes. Checking open files again."}

resolve :: Reconciliation -> Conflict -> ConflictAction -> Desktop -> IO Desktop
resolve runtime@(Reconciliation watcher ref _) conflict action desktop = case M.lookup bid (buffers desktop) of
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
