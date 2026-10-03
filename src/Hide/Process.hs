{-# LANGUAGE ScopedTypeVariables #-}
-- | Best-effort shutdown for owned subprocess groups.
--
-- Capture cleanup immediately after spawn, before another waiter can reap the
-- PID. The returned action serializes repeated cleanup, terminates the process
-- tree and bounds exit polling. Callers stop children before joining pipe workers;
-- suppressed cleanup IO failures mean return is not proof every descendant exited.
module Hide.Process (processCleanup) where

import Control.Concurrent (modifyMVar_, newMVar, threadDelay)
import Control.Exception (IOException, bracket, catch, mask_)
import Control.Monad (forM_, unless, void)
import System.Info (os)
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
      void (timeout 1000000 (pollExit process))
    pure True
  where
    -- On Windows a timed-out readProcessWithExitCode can still block while its
    -- pipe workers and waitForProcess unwind. Poll without creating those pipes.
    killTree grace command arguments = bracket
      (createProcess (proc command arguments) {std_in=NoStream,std_out=NoStream,std_err=NoStream})
      (\(_,_,_,helper) -> ignore (terminateProcess helper) >> void (timeout 1000000 (pollExit helper)))
      (\(_,_,_,helper) -> void (timeout grace (pollExit helper)))

pollExit :: ProcessHandle -> IO ()
pollExit process = getProcessExitCode process >>= maybe (threadDelay 10000 >> pollExit process) (const (pure ()))

ignore :: IO a -> IO ()
ignore operation=void operation `catch` (\(_::IOException) -> pure ())
