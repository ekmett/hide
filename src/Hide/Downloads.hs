-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.Downloads
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Session-owned serial download queue with progress and cancellation.
--
-- One worker handles a bounded pending queue. Cancellation is a signal; the action
-- owns resource cleanup and must bracket it. Async exceptions propagate rather
-- than becoming ordinary failures. Completed history is pruned as jobs arrive,
-- while active jobs remain visible. Tool-cache selection shares THC configuration.
module Hide.Downloads
  (Downloads, Download(..), DownloadState(..), DownloadProgress(..),
   withDownloads, startDownload, cancelDownload, downloadSnapshot, downloadStateFor, downloadRevision, downloadVersionedSnapshot, managedToolRoot) where

import Control.Concurrent.Async (race,withAsync)
import Control.Concurrent.STM
import Control.Exception (SomeException,SomeAsyncException,fromException,throwIO,try,finally)
import Control.Monad (forever,void,unless,when)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getHomeDirectory,getXdgDirectory,XdgDirectory(XdgCache))
import System.Environment (lookupEnv)
import System.FilePath ((</>),isAbsolute,normalise)
import System.Info (os)

-- | Reported completed bytes and optional total; an unknown total is not zero.
data DownloadProgress=DownloadProgress
  { downloadPhase :: Text, downloadBytes :: Integer, downloadTotal :: Maybe Integer }
  deriving (Eq,Show)
-- | Queued/running/cancelling states are distinct from terminal outcomes.
data DownloadState=DownloadQueued | DownloadRunning DownloadProgress | DownloadCancelling
  | DownloadComplete FilePath | DownloadFailed Text | DownloadCancelled deriving (Eq,Show)
data Download=Download {downloadId :: Int,downloadLabel :: Text,downloadState :: DownloadState} deriving (Eq,Show)
type Action=(DownloadProgress -> IO ()) -> IO (Either Text FilePath)
data Job=Job Download !Int (TMVar ()) Action
data Downloads=Downloads (TVar Int) (TVar (M.Map Int Job)) (TBQueue Int) (TVar Int) (TVar Bool)

-- | One bounded queue and worker, owned by the editor session. Closing joins
-- cancellation outside the desktop lock. Closing first rejects new admission and
-- terminalizes every unfinished job, preserving completed history. Retained
-- handles cannot restart the queue; progress callbacks cannot revive a terminal
-- job. Job actions must bracket their resources.
withDownloads :: (Downloads -> IO a) -> IO a
withDownloads action=do
  runtime<-Downloads <$> newTVarIO 1 <*> newTVarIO M.empty <*> newTBQueueIO 16 <*> newTVarIO 0 <*> newTVarIO False
  withAsync (forever (work runtime)) (const (action runtime `finally` closeDownloads runtime))

-- Admission and terminalization share one transaction. Signal before the outer
-- withAsync cancels/joins: cleanup may retain a reporter, but can no longer publish.
closeDownloads :: Downloads -> IO ()
closeDownloads (Downloads _ jobs queue serial closed)=atomically $ do
  stopped<-readTVar closed
  unless stopped $ do
    writeTVar closed True
    entries<-readTVar jobs
    let unfinished (Job row _ _ _)=not (terminal (downloadState row))
        cancelJob job@(Job row version stop action)
          | unfinished job=Job row {downloadState=DownloadCancelled} (version+1) stop action
          | otherwise=job
    mapM_ (\(Job _ _ stop _)->void (tryPutTMVar stop ())) (filter unfinished (M.elems entries))
    when (any unfinished entries) $ do
      writeTVar jobs (M.map cancelJob entries)
      modifyTVar' serial (+1)
    void (flushTBQueue queue)

-- | Call only after the user has accepted the concrete download offer. Admission
-- after scope closure is rejected; an accepted job always reaches a terminal state
-- by shutdown, even if it was still queued.
startDownload :: Downloads -> Text -> Action -> IO (Either Text Int)
startDownload (Downloads next jobs queue serial closed) label action
  | T.null (T.strip label) || T.length label>200 = pure (Left "Invalid download label.")
  | otherwise=atomically $ do
      stopped<-readTVar closed
      full<-isFullTBQueue queue
      if stopped then pure (Left "Download service is closed.")
      else if full then pure (Left "The download queue is full.") else do
        ident<-readTVar next
        cancel<-newEmptyTMVar
        modifyTVar' next (+1)
        modifyTVar' jobs (M.insert ident (Job (Download ident label DownloadQueued) 0 cancel action) . retain)
        modifyTVar' serial (+1)
        writeTBQueue queue ident
        pure (Right ident)
  where retain entries=let completed=[i | (i,Job row _ _ _)<-M.toAscList entries,terminal (downloadState row)]
                       in foldr M.delete entries (take (max 0 (length completed-47)) completed)

-- | Cancellation is a nonblocking signal; cleanup runs on the worker.
cancelDownload :: Downloads -> Int -> IO Bool
cancelDownload (Downloads _ jobs _ serial closed) ident=atomically $ do
  stopped<-readTVar closed
  entries<-readTVar jobs
  case M.lookup ident entries of
    Just (Job row version stop action) | not stopped,not (terminal (downloadState row))->do
      void (tryPutTMVar stop ())
      writeTVar jobs (M.insert ident (Job row {downloadState=DownloadCancelling} (version+1) stop action) entries)
      modifyTVar' serial (+1)
      pure True
    _->pure False

-- | Return retained jobs in ID order with their current progress and state.
downloadSnapshot :: Downloads -> IO [Download]
downloadSnapshot (Downloads _ jobs _ _ _)=map (\(Job row _ _ _)->row) . M.elems <$> readTVarIO jobs

-- | Look up the current retained state by exact job ID, without scanning other
-- jobs or preparing presentation. A pruned ID is absent, never another transfer.
downloadStateFor :: Downloads -> Int -> IO (Maybe DownloadState)
downloadStateFor (Downloads _ jobs _ _ _) ident=do
  entries<-readTVarIO jobs
  pure (case M.lookup ident entries of Just (Job row _ _ _)->Just (downloadState row); _->Nothing)

-- | /O(1)/. Revision of the retained job catalogue. Every accepted state write
-- advances this in the same transaction; reading it never inspects job payloads.
downloadRevision :: Downloads -> IO Int
downloadRevision (Downloads _ _ _ serial _)=readTVarIO serial

-- | Atomically capture catalogue revision and per-job revisions in ID order.
-- The immutable payloads are intentionally lazy; presentation belongs on a worker.
downloadVersionedSnapshot :: Downloads -> IO (Int,[(Int,Download)])
downloadVersionedSnapshot (Downloads _ jobs _ serial _)=atomically $ do
  revision<-readTVar serial
  rows<-readTVar jobs
  pure (revision,map (\(Job row version _ _)->(version,row)) (M.elems rows))

terminal :: DownloadState -> Bool
terminal DownloadComplete{}=True
terminal DownloadFailed{}=True
terminal DownloadCancelled=True
terminal _=False

work :: Downloads -> IO ()
work (Downloads _ jobs queue serial closed)=do
  (ident,Job _ _ stop action)<-atomically $ do
    stopped<-readTVar closed
    check (not stopped)
    ident<-readTBQueue queue
    entries<-readTVar jobs
    case M.lookup ident entries of Nothing->retry; Just job->pure (ident,job)
  let update state=atomically $ do
        stopped<-readTVar closed
        entries<-readTVar jobs
        cancelled<-not <$> isEmptyTMVar stop
        case M.lookup ident entries of
          Just (Job row version cancel body)
            | not stopped,not (terminal (downloadState row)),not cancelled || terminal state->do
                let next=if cancelled then DownloadCancelled else state
                writeTVar jobs (M.insert ident (Job row {downloadState=next} (version+1) cancel body) entries)
                modifyTVar' serial (+1)
          _->pure ()
      report progress=update (DownloadRunning progress)
  cancelled<-atomically (not <$> isEmptyTMVar stop)
  if cancelled then update DownloadCancelled else do
    report (DownloadProgress "Starting" 0 Nothing)
    result<-try (race (atomically (readTMVar stop)) (action report))
    case (result :: Either SomeException (Either () (Either Text FilePath))) of
      Left exception | Just async<-(fromException exception :: Maybe SomeAsyncException)->throwIO async
                     | otherwise->update (DownloadFailed "Download failed.")
      Right (Left ())->update DownloadCancelled
      Right (Right outcome)->update (either (DownloadFailed . T.take 1000) DownloadComplete outcome)

-- | Share the THC driver's CBD cache root and override.
managedToolRoot :: IO (Either Text FilePath)
managedToolRoot=do
  override<-lookupEnv "THC_CACHE_HOME"
  case override of
    Just path | isAbsolute path && not (null path) && all (`notElem` ['\0','\n','\r']) path->pure (Right (normalise path))
              | otherwise->pure (Left "THC_CACHE_HOME must be an absolute directory path.")
    Nothing | os=="darwin"->Right . (</> "Library/Caches/thc") <$> getHomeDirectory
            | otherwise->Right <$> getXdgDirectory XdgCache "thc"
