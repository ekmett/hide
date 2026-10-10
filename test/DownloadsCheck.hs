{-# LANGUAGE OverloadedStrings #-}
module DownloadsCheck (checks) where
import Control.Concurrent
import Control.Exception (finally)
import Control.Monad (unless)
import Data.Either (isLeft)
import System.Timeout (timeout)
import Hide.Downloads
checks :: IO ()
checks=do
  cleaned<-newEmptyMVar
  (retained,active,queued,completeId,failedId,lateReport)<-withDownloads $ \downloads -> do
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
    withDownloads $ \bounded->do
      started<-newEmptyMVar
      blocked<-newEmptyMVar
      _<-startDownload bounded "active" (\_->putMVar started () >> takeMVar blocked >> pure (Right "unused")) >>= right
      takeMVar started
      mapM_ (\_ -> startDownload bounded "queued" (\_->pure (Right "unused")) >>= right) [1::Int ..16]
      overflow<-startDownload bounded "overflow" (\_->pure (Right "unused"))
      check "pending download queue is bounded" (isLeft overflow)
    started<-newEmptyMVar
    blocked<-newEmptyMVar
    running<-startDownload downloads "close active" (\report ->
      (report (DownloadProgress "Downloading" 4 (Just 10)) >> putMVar started report >> takeMVar blocked >> pure (Right "unused")) `finally` do
        before<-downloadVersionedSnapshot downloads
        report (DownloadProgress "Closing" 10 (Just 10))
        after<-downloadVersionedSnapshot downloads
        putMVar cleaned (before==after)) >>= right
    report<-takeMVar started
    waiting<-startDownload downloads "close queued" (\_->error "closed queued action ran") >>= right
    pure (downloads,running,waiting,complete,failed,report)
  joined<-tryReadMVar cleaned
  check "close joins action cleanup" (joined/=Nothing)
  check "closing action cannot report progress from its finalizer" (joined==Just True)
  rejected<-startDownload retained "after close" (\_->error "post-close action ran")
  check "retained download handle rejects starts after close" (isLeft rejected)
  states<-mapM (downloadStateFor retained) [active,queued,completeId,failedId]
  check "close cancels active and queued transfers while preserving history" (states==
    [Just DownloadCancelled,Just DownloadCancelled,Just (DownloadComplete "/tmp/tool"),Just (DownloadFailed "checksum mismatch")])
  before<-downloadVersionedSnapshot retained
  lateReport (DownloadProgress "Late progress" 9 (Just 10))
  after<-downloadVersionedSnapshot retained
  revision<-downloadRevision retained
  check "retained progress cannot revive closed rows or advance revisions" (before==after && revision==fst before)
  putStrLn "download lifecycle checks passed"
  where right=either (error.show) pure
        check label ok=unless ok (error label)
        await runtime ident done=do
          result<-timeout 1000000 (loop runtime ident done)
          check "job reaches terminal status" (result/=Nothing)
        loop runtime ident done=do
          rows<-downloadSnapshot runtime
          if any (\d->downloadId d==ident && done (downloadState d)) rows then pure () else threadDelay 1000 >> loop runtime ident done
