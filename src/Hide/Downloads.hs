{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
module Hide.Downloads
  (Downloads, Download(..), DownloadState(..), DownloadProgress(..),
   withDownloads, startDownload, cancelDownload, downloadSnapshot, managedToolRoot) where

import Control.Concurrent.Async (race,withAsync)
import Control.Concurrent.STM
import Control.Exception (SomeException,SomeAsyncException,fromException,throwIO,try)
import Control.Monad (forever,void)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getHomeDirectory,getXdgDirectory,XdgDirectory(XdgCache))
import System.Environment (lookupEnv)
import System.FilePath ((</>),isAbsolute,normalise)
import System.Info (os)

data DownloadProgress=DownloadProgress
  { downloadPhase :: Text, downloadBytes :: Integer, downloadTotal :: Maybe Integer }
  deriving (Eq,Show)
data DownloadState=DownloadQueued | DownloadRunning DownloadProgress | DownloadCancelling
  | DownloadComplete FilePath | DownloadFailed Text | DownloadCancelled deriving (Eq,Show)
data Download=Download {downloadId :: Int,downloadLabel :: Text,downloadState :: DownloadState} deriving (Eq,Show)
type Action=(DownloadProgress -> IO ()) -> IO (Either Text FilePath)
data Job=Job Download (TMVar ()) Action
data Downloads=Downloads (TVar Int) (TVar (M.Map Int Job)) (TBQueue Int)

-- | One bounded queue and worker, owned by the editor session. Closing joins
-- cancellation outside the desktop lock. Job actions must bracket their resources.
withDownloads :: (Downloads -> IO a) -> IO a
withDownloads action=do
  runtime<-Downloads <$> newTVarIO 1 <*> newTVarIO M.empty <*> newTBQueueIO 16
  withAsync (forever (work runtime)) (const (action runtime))

-- | Call only after the user has accepted the concrete download offer.
startDownload :: Downloads -> Text -> Action -> IO (Either Text Int)
startDownload (Downloads next jobs queue) label action
  | T.null (T.strip label) || T.length label>200 = pure (Left "Invalid download label.")
  | otherwise=atomically $ do
      full<-isFullTBQueue queue
      if full then pure (Left "The download queue is full.") else do
        ident<-readTVar next
        cancel<-newEmptyTMVar
        modifyTVar' next (+1)
        modifyTVar' jobs (M.insert ident (Job (Download ident label DownloadQueued) cancel action) . retain)
        writeTBQueue queue ident
        pure (Right ident)
  where retain entries=let completed=[i | (i,Job row _ _)<-M.toAscList entries,terminal (downloadState row)]
                       in foldr M.delete entries (take (max 0 (length completed-47)) completed)

-- | Cancellation is a nonblocking signal; cleanup runs on the worker.
cancelDownload :: Downloads -> Int -> IO Bool
cancelDownload (Downloads _ jobs _) ident=atomically $ do
  entries<-readTVar jobs
  case M.lookup ident entries of
    Just (Job row stop action) | not (terminal (downloadState row))->do
      void (tryPutTMVar stop ())
      writeTVar jobs (M.insert ident (Job row {downloadState=DownloadCancelling} stop action) entries)
      pure True
    _->pure False

downloadSnapshot :: Downloads -> IO [Download]
downloadSnapshot (Downloads _ jobs _)=map (\(Job row _ _)->row) . M.elems <$> readTVarIO jobs

terminal :: DownloadState -> Bool
terminal DownloadComplete{}=True
terminal DownloadFailed{}=True
terminal DownloadCancelled=True
terminal _=False

work :: Downloads -> IO ()
work (Downloads _ jobs queue)=do
  (ident,Job _ stop action)<-atomically $ do
    ident<-readTBQueue queue
    entries<-readTVar jobs
    case M.lookup ident entries of Nothing->retry; Just job->pure (ident,job)
  let update status=atomically $ modifyTVar' jobs (M.adjust (\(Job row cancel body)->Job row {downloadState=status} cancel body) ident)
      report progress=atomically $ do
        cancelled<-not <$> isEmptyTMVar stop
        if cancelled then pure () else modifyTVar' jobs (M.adjust (\(Job row cancel body)->Job row {downloadState=DownloadRunning progress} cancel body) ident)
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
