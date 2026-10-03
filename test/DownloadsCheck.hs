{-# LANGUAGE OverloadedStrings #-}
module DownloadsCheck (checks) where
import Control.Concurrent
import Control.Exception (finally)
import Control.Monad (unless)
import Data.Either (isLeft)
import System.Timeout (timeout)
import Hide.Downloads
checks :: IO ()
checks=withDownloads $ \downloads -> do
  entered<-newEmptyMVar
  stopped<-newEmptyMVar
  gate<-newEmptyMVar
  first<-startDownload downloads "one" (\report -> (report (DownloadProgress "Downloading" 4 (Just 10)) >> putMVar entered () >> takeMVar gate >> pure (Right "/tmp/one")) `finally` putMVar stopped ()) >>= right
  takeMVar entered
  pending<-startDownload downloads "two" (\_ -> error "cancelled queued action ran") >>= right
  _<-cancelDownload downloads pending
  rows<-downloadSnapshot downloads
  check "running progress and queued cancellation retained" (any (\d->downloadId d==first && downloadState d==DownloadRunning (DownloadProgress "Downloading" 4 (Just 10))) rows)
  _<-cancelDownload downloads first
  timeout 1000000 (takeMVar stopped) >>= check "cancel releases active job" . (/=Nothing)
  await downloads first (==DownloadCancelled)
  await downloads pending (==DownloadCancelled)
  complete<-startDownload downloads "complete" (\_ -> pure (Right "/tmp/tool")) >>= right
  await downloads complete (==DownloadComplete "/tmp/tool")
  failed<-startDownload downloads "failure" (\_ -> pure (Left "checksum mismatch")) >>= right
  await downloads failed (==DownloadFailed "checksum mismatch")
  check "empty labels rejected" . isLeft =<< startDownload downloads "" (\_ -> pure (Right "unused"))
  closed<-newEmptyMVar
  withDownloads $ \owned->do
    started<-newEmptyMVar
    blocked<-newEmptyMVar
    _<-startDownload owned "close" (\_->(putMVar started () >> takeMVar blocked >> pure (Right "unused")) `finally` putMVar closed ()) >>= right
    takeMVar started
  timeout 1000000 (takeMVar closed) >>= check "session close joins worker cleanup" . (/=Nothing)
  withDownloads $ \bounded->do
    started<-newEmptyMVar
    blocked<-newEmptyMVar
    _<-startDownload bounded "active" (\_->putMVar started () >> takeMVar blocked >> pure (Right "unused")) >>= right
    takeMVar started
    mapM_ (\_ -> startDownload bounded "queued" (\_->pure (Right "unused")) >>= right) [1::Int ..16]
    overflow<-startDownload bounded "overflow" (\_->pure (Right "unused"))
    check "pending download queue is bounded" (isLeft overflow)
  putStrLn "download lifecycle checks passed"
  where right=either (error.show) pure
        check label ok=unless ok (error label)
        await runtime ident done=do
          result<-timeout 1000000 (loop runtime ident done)
          check "job reaches terminal status" (result/=Nothing)
        loop runtime ident done=do
          rows<-downloadSnapshot runtime
          if any (\d->downloadId d==ident && done (downloadState d)) rows then pure () else threadDelay 1000 >> loop runtime ident done
