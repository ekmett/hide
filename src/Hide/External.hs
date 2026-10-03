{-# LANGUAGE OverloadedStrings #-}
-- | Background filesystem polling with coalesced observations.
--
-- Callers subscribe canonical file paths with baseline tokens. Desired-state updates
-- do no file IO; the worker checks metadata plus a rotating overdue byte rescan.
-- Before/after stamps avoid publishing a raced read. Pending results coalesce by
-- path and are filtered against current subscriptions, so obsolete observations
-- cannot masquerade as evidence for a new baseline.
module Hide.External
  ( Watcher, Observation(..), withWatcher, watchPaths, pollObservations, forceCheck ) where

import Control.Concurrent (MVar, forkIO, killThread, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, takeMVar, tryPutMVar)
import Control.Exception (bracket)
import Control.Monad (foldM, unless, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (canonicalizePath, getFileSize, getModificationTime, listDirectory, pathIsSymbolicLink)
import System.IO.Error (catchIOError, isDoesNotExistError, tryIOError)
import System.Timeout (timeout)
import Hide.Browser (Entry, readDirectory)

data Observation
  = FileObserved FilePath Int (Maybe ByteString)
  | FileUnavailable FilePath Int Text
  | DirectoryObserved FilePath [Entry]
  | DirectoryUnavailable FilePath Text
  deriving (Eq, Show)

data Watcher = Watcher (MVar Desired) (MVar ())
data Desired = Desired (Map.Map FilePath Int) (Set.Set FilePath) (Map.Map (Bool, FilePath) Observation) Bool
type Stamp = (UTCTime, Integer)
data CachedFile = CachedFile (Maybe Stamp) Observation Word64

-- | Scope the watcher worker and its desired/published observation state.
withWatcher :: (Watcher -> IO a) -> IO a
withWatcher action = do
  watcher <- Watcher <$> newMVar (Desired Map.empty Set.empty Map.empty False) <*> newEmptyMVar
  bracket (forkIO (worker watcher)) killThread (const (action watcher))

-- | Replace subscriptions using canonical paths and caller-owned baseline tokens.
watchPaths :: Watcher -> [(FilePath, Int)] -> [FilePath] -> IO ()
watchPaths watcher@(Watcher state _) files directories = do
  changed <- modifyMVar state $ \(Desired oldFiles oldDirs pending forced) -> do
    let newFiles = Map.fromList files
        newDirs = Set.fromList directories
    pure (Desired newFiles newDirs (Map.filter (wanted newFiles newDirs) pending) forced, oldFiles /= newFiles || oldDirs /= newDirs)
  when changed (wake watcher)

-- | Drain coalesced results that still match current subscriptions/tokens.
pollObservations :: Watcher -> IO [Observation]
pollObservations (Watcher state _) = modifyMVar state $ \(Desired files dirs pending forced) ->
  pure (Desired files dirs Map.empty forced, Map.elems pending)

-- | Request an asynchronous byte check that bypasses metadata-cache reuse.
forceCheck :: Watcher -> IO ()
forceCheck watcher@(Watcher state _) = do
  modifyMVar_ state $ \(Desired files dirs pending _) -> pure (Desired files dirs pending True)
  wake watcher

wake :: Watcher -> IO ()
wake (Watcher _ signal) = void (tryPutMVar signal ())

worker :: Watcher -> IO ()
worker (Watcher state signal) = loop Map.empty Map.empty
  where
    loop files directories = do
      (wantedFiles, wantedDirs, forced) <- modifyMVar state $ \(Desired files' dirs pending forced') ->
        pure (Desired files' dirs pending False, (files', dirs, forced'))
      now <- getMonotonicTimeNSec
      (nextFiles, fileEvents, _) <- foldM (inspectFile now forced files)
        (Map.empty, [], True) (Map.toList wantedFiles)
      (nextDirs, dirEvents) <- foldM (inspectDirectory directories)
        (Map.empty, []) (Set.toList wantedDirs)
      modifyMVar_ state $ \(Desired currentFiles currentDirs pending forced') -> do
        let insert events isDirectory pendingEvents = foldr (\(path, event) acc ->
              if wanted currentFiles currentDirs event then Map.insert (isDirectory, path) event acc else acc) pendingEvents events
        pure (Desired currentFiles currentDirs
          (insert fileEvents False (insert dirEvents True pending)) forced')
      void (timeout 1000000 (takeMVar signal))
      loop nextFiles nextDirs

-- ponytail: metadata polling, with one 30-second-overdue byte rescan per pass;
-- use native filesystem notifications if very large watch sets need lower latency.
inspectFile :: Word64 -> Bool -> Map.Map FilePath CachedFile
  -> (Map.Map FilePath CachedFile, [(FilePath, Observation)], Bool) -> (FilePath, Int)
  -> IO (Map.Map FilePath CachedFile, [(FilePath, Observation)], Bool)
inspectFile now forced previous (next, events, budget) (path, token) = do
  let old = case Map.lookup path previous of
        Just cached@(CachedFile _ event _) | wanted (Map.singleton path token) Set.empty event -> Just cached
        _ -> Nothing
      overdue = case old of
        Just (CachedFile _ _ checked) -> now - checked >= 30000000000
        Nothing -> False
      rescan = forced || (budget && overdue)
  checked <- tryIOError $ do
    before <- stamp path
    case old of
      Just cached@(CachedFile oldStamp (FileObserved _ _ _) _) | before == oldStamp && not rescan -> pure (Just cached)
      _ -> do
        bytes <- case before of
          Nothing -> pure Nothing
          Just _ -> Just <$> BS.readFile path
        after <- stamp path
        pure $ if before == after then Just (CachedFile after (FileObserved path token bytes) now) else Nothing
  let result = case checked of
        Right value -> value
        Left err | isDoesNotExistError err -> Nothing -- Raced with deletion: retry next pass.
                 | otherwise -> Just (CachedFile Nothing (FileUnavailable path token (T.pack (show err))) now)
  pure $ case result of
    Nothing -> (maybe next (\cached -> Map.insert path cached next) old, events, budget)
    Just cached@(CachedFile _ event _) ->
      let changed = case old of
            Just (CachedFile _ oldEvent _) -> oldEvent /= event
            Nothing -> True
      in (Map.insert path cached next, if changed then (path, event) : events else events,
          budget && not overdue)

wanted :: Map.Map FilePath Int -> Set.Set FilePath -> Observation -> Bool
wanted files dirs event = case event of
  FileObserved path token _ -> Map.lookup path files == Just token
  FileUnavailable path token _ -> Map.lookup path files == Just token
  DirectoryObserved path _ -> Set.member path dirs
  DirectoryUnavailable path _ -> Set.member path dirs

stamp :: FilePath -> IO (Maybe Stamp)
stamp path = catchIOError inspect $ \err ->
  if isDoesNotExistError err then pure Nothing else ioError err
  where
    inspect = do
      resolved <- canonicalizePath path
      symlink <- catchIOError (pathIsSymbolicLink path) $ \err ->
        if isDoesNotExistError err then pure False else ioError err
      unless (not symlink && resolved == path)
        (ioError (userError "File path was replaced by a symbolic link; reopen it to follow the new target."))
      Just <$> ((,) <$> getModificationTime path <*> getFileSize path)

inspectDirectory :: Map.Map FilePath (Either Text [FilePath])
  -> (Map.Map FilePath (Either Text [FilePath]), [(FilePath, Observation)]) -> FilePath
  -> IO (Map.Map FilePath (Either Text [FilePath]), [(FilePath, Observation)])
inspectDirectory previous (next, events) path = do
  result <- either (Left . T.pack . show) (Right . sort) <$> tryIOError (listDirectory path)
  let changed = Map.lookup path previous /= Just result
  if not changed then pure (Map.insert path result next, events) else do
    listing <- readDirectory path "*"
    let event = either (DirectoryUnavailable path . T.pack) (DirectoryObserved path . snd) listing
        cached = either (Left . T.pack) (const result) listing
    pure (Map.insert path cached next, (path, event) : events)
