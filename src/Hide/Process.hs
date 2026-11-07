-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
-- | Module      : Hide.Process
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : ScopedTypeVariables
--
-- UTF-8 capture and cancellable shutdown for owned subprocess groups.
--
-- Capture cleanup immediately after spawn, before another waiter can reap the
-- PID. The returned action serializes repeated cleanup, terminates the process
-- tree and bounds exit polling. Callers stop children before joining pipe workers;
-- suppressed cleanup IO failures mean return is not proof every descendant exited.
module Hide.Process (processCleanup,waitProcessExit,readProcessUtf8,readProcessBytesBounded) where

import Control.Concurrent (modifyMVar_, newMVar, threadDelay)
import Control.Exception (IOException, bracket, catch, mask, mask_, onException)
import Control.Concurrent.Async (withAsync,wait,link)
import Control.Monad (forM_, unless, void)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Info (os)
import System.IO (Handle,hClose,hSetBinaryMode)
import System.Exit (ExitCode)
import System.Process
import System.Timeout (timeout)

-- | Capture an idempotent cleanup action immediately after createProcess with
-- @create_group=True@, before another waiter can reap the PID.
-- Invoke it before joining readers that may be blocked on child pipes.
processCleanup :: ProcessHandle -> IO (IO ())
processCleanup process = do
  pid<-getPid process
  released<-newMVar False
  pure $ mask_ $ modifyMVar_ released $ \done -> do
    unless done $ do
      forM_ pid $ \ident -> ignore $ if os=="mingw32"
        then killTree 5000000 "taskkill" ["/PID",show ident,"/T","/F"]
        else killTree 1000000 "/bin/kill" ["-KILL","--","-"++show ident]
      ignore (terminateProcess process)
      void (timeout 1000000 (waitProcessExit process))
    pure True
  where
    -- On Windows a timed-out readProcessWithExitCode can still block while its
    -- pipe workers and waitForProcess unwind. Poll without creating those pipes.
    killTree grace command arguments = bracket
      (createProcess (proc command arguments) {std_in=NoStream,std_out=NoStream,std_err=NoStream})
      (\(_,_,_,helper) -> ignore (terminateProcess helper) >> void (timeout 1000000 (waitProcessExit helper)))
      (\(_,_,_,helper) -> void (timeout grace (waitProcessExit helper)))

-- | Wait for exit while retaining asynchronous cancellation. The Windows
-- process wait can defer a timeout until the child exits; a nonblocking status
-- probe keeps cancellation available to the owner which must stop that child.
-- POSIX retains its event-driven wait.
waitProcessExit :: ProcessHandle -> IO ExitCode
waitProcessExit process
  | os=="mingw32" = getProcessExitCode process >>= maybe (threadDelay 10000 >> waitProcessExit process) pure
  | otherwise = waitForProcess process

-- | Capture a UTF-8 subprocess without consulting the host locale. Invalid
-- UTF-8 is an IO error rather than a replacement character in a machine path.
-- The owned process group is stopped before joining pipe readers on failure.
readProcessUtf8 :: CreateProcess -> T.Text -> IO (ExitCode,T.Text,T.Text)
readProcessUtf8 = captureProcess BS.hGetContents $ \code out err->do
  stdout<-decode out
  stderr<-decode err
  pure (code,stdout,stderr)
  where
    decode bytes=either (const (ioError (userError "Subprocess returned invalid UTF-8"))) pure (TE.decodeUtf8' bytes)

-- | Retain at most the given number of bytes per pipe while draining both pipes
-- in fixed-size chunks. The flag reports discarded bytes. Cancellation and IO
-- failures use the same owned process-group cleanup as complete capture.
readProcessBytesBounded :: Int -> CreateProcess -> T.Text -> IO (ExitCode,(BS.ByteString,Bool),(BS.ByteString,Bool))
readProcessBytesBounded limit=captureProcess (readBounded (max 0 limit) [] False) (\code out err->pure (code,out,err))
  where
    readBounded !remaining !chunks !truncated pipe=do
      chunk<-BS.hGetSome pipe 32768
      if BS.null chunk then pure (BS.concat (reverse chunks),truncated)
      else let kept=BS.take remaining chunk
           in readBounded (remaining-BS.length kept) (if BS.null kept then chunks else kept:chunks)
                (truncated || BS.length chunk>remaining) pipe

captureProcess :: (Handle -> IO a) -> (ExitCode -> a -> a -> IO b) -> CreateProcess -> T.Text -> IO b
captureProcess readOutput finish command input =
  withCreateProcess command {std_in=CreatePipe,std_out=CreatePipe,std_err=CreatePipe,create_group=True} $ \sourcePipe outputPipe errorPipe child ->
    mask $ \restore -> do
      Just source<-pure sourcePipe
      Just output<-pure outputPipe
      Just errors<-pure errorPipe
      stop<-processCleanup child
      (do
        mapM_ (`hSetBinaryMode` True) [source,output,errors]
        withAsync (restore (readOutput output)) $ \out ->
          withAsync (restore (readOutput errors)) $ \err ->
            restore (do
              link out
              link err
              BS.hPut source (TE.encodeUtf8 input)
              hClose source
              code<-waitProcessExit child
              stdout<-wait out
              stderr<-wait err
              finish code stdout stderr) `onException` stop
        ) `onException` stop

ignore :: IO a -> IO ()
ignore operation=void operation `catch` (\(_::IOException) -> pure ())
