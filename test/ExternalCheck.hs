{-# LANGUAGE OverloadedStrings #-}
module ExternalCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.List (sort)
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)
import System.Timeout (timeout)
import THC.Edit.External
import THC.Edit.Browser (entryName)

checks :: IO ()
checks = bracket temporary removePathForcibly $ \dir -> withWatcher $ \watcher -> do
  let path = dir </> "source.txt"
      replacement = dir </> "replacement"
      expect label wanted = do
        forceCheck watcher
        result <- timeout 3000000 (await (== wanted))
        check label (result == Just ())
      expectDirectory label names = do
        forceCheck watcher
        result <- timeout 3000000 (await (\event -> case event of
          DirectoryObserved parent entries -> parent == dir && sort (map entryName entries) == sort names
          _ -> False))
        check label (result == Just ())
      await wanted = do
        observations <- pollObservations watcher
        unless (any wanted observations) (threadDelay 10000 >> await wanted)
  BS.writeFile path "already changed"
  watchPaths watcher [(path, 0)] [dir]
  expect "initial observation includes bytes" (FileObserved path 0 (Just "already changed"))
  BS.writeFile replacement "replacement"
  renameFile replacement path
  automatic <- timeout 3000000 (await (== FileObserved path 0 (Just "replacement")))
  check "automatic metadata polling finds atomic replacement" (automatic == Just ())
  stamp <- getModificationTime path
  BS.writeFile path "same length"
  setModificationTime path stamp
  expect "forced content check catches unchanged size and mtime" (FileObserved path 0 (Just "same length"))
  removeFile path
  expect "deleted file" (FileObserved path 0 Nothing)
  BS.writeFile path "recreated"
  expect "recreated file" (FileObserved path 0 (Just "recreated"))
  BS.writeFile replacement "new sibling"
  expectDirectory "directory child creation" ["..", "replacement", "source.txt"]
  removeFile replacement
  expectDirectory "directory child deletion" ["..", "source.txt"]
  forceCheck watcher
  threadDelay 1100000
  unchanged <- pollObservations watcher
  check "unchanged paths produce no repeated observations" (null unchanged)
  watchPaths watcher [(path, 1)] [dir]
  expect "token changes re-observe unchanged bytes" (FileObserved path 1 (Just "recreated"))
  BS.writeFile path "queued before save"
  forceCheck watcher
  threadDelay 100000
  watchPaths watcher [(path, 2)] [dir]
  pending <- pollObservations watcher
  check "new token discards queued old observations" (all (\event -> case event of
    FileObserved _ token _ -> token == 2
    FileUnavailable _ token _ -> token == 2
    _ -> True) pending)
  permissions <- getPermissions path
  bracket (setPermissions path (permissions { readable = False }))
          (const (setPermissions path permissions)) $ \_ -> do
    forceCheck watcher
    denied <- timeout 3000000 (awaitUnavailable watcher path)
    check "read errors are not mistaken for deletion" (denied == Just ())
  expect "readable file recovers from error" (FileObserved path 2 (Just "queued before save"))
  let subdirectory = dir </> "nested"
  createDirectory subdirectory
  watchPaths watcher [(path, 2)] [dir, subdirectory]
  forceCheck watcher
  nested <- timeout 3000000 (await (\event -> case event of
    DirectoryObserved parent _ -> parent == subdirectory
    _ -> False))
  check "new directory watch emits entries" (nested == Just ())
  removeDirectory subdirectory
  forceCheck watcher
  absent <- timeout 3000000 (await (\event -> case event of
    DirectoryUnavailable parent _ -> parent == subdirectory
    _ -> False))
  check "missing directory reports unavailable" (absent == Just ())
  createDirectory subdirectory
  forceCheck watcher
  recovered <- timeout 3000000 (await (\event -> case event of
    DirectoryObserved parent _ -> parent == subdirectory
    _ -> False))
  check "recreated directory emits entries" (recovered == Just ())
  removeFile path
  createFileLink replacement path
  forceCheck watcher
  unavailable <- timeout 3000000 (awaitUnavailable watcher path)
  check "symlink replacement is unavailable, not deletion" (unavailable == Just ())
  watchPaths watcher [] []
  removeFile path
  BS.writeFile path "unwatched"
  forceCheck watcher
  threadDelay 100000
  stale <- pollObservations watcher
  check "unwatched paths leave no queued observations" (null stale)
  putStrLn "external file checks passed"
  where
    check name ok = unless ok (error name)
    temporary = do
      base <- getTemporaryDirectory
      (path, handle) <- openBinaryTempFile base "thc-edit-external-check"
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path

awaitUnavailable :: Watcher -> FilePath -> IO ()
awaitUnavailable watcher path = do
  observations <- pollObservations watcher
  unless (any unavailable observations)
    (threadDelay 10000 >> awaitUnavailable watcher path)
  where
    unavailable (FileUnavailable file _ _) = file == path
    unavailable _ = False
