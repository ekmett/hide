{-# LANGUAGE ScopedTypeVariables #-}
module THC.Edit.Process (processCleanup) where

import Control.Concurrent (modifyMVar_, newMVar)
import Control.Exception (IOException, catch, mask_)
import Control.Monad (forM_, unless, void)
import System.Info (os)
import System.Process
import System.Timeout (timeout)

-- Call immediately after spawning with create_group=True. Capture the PID before
-- waitForProcess can reap it; callers must stop the tree before joining pipe IO.
processCleanup :: ProcessHandle -> IO (IO ())
processCleanup process = do
  pid<-getPid process
  released<-newMVar False
  pure $ mask_ $ modifyMVar_ released $ \done -> do
    unless done $ do
      forM_ pid $ \ident -> ignore $ timeout 1000000 $ if os=="mingw32"
        then readProcessWithExitCode "taskkill" ["/PID",show ident,"/T","/F"] ""
        else readProcessWithExitCode "/bin/kill" ["-KILL","--","-"++show ident] ""
      ignore (terminateProcess process)
      void (timeout 1000000 (waitForProcess process))
    pure True
  where
    ignore operation=void operation `catch` (\(_::IOException) -> pure ())
