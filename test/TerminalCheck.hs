{-# LANGUAGE CPP, OverloadedStrings #-}
module TerminalCheck (checks) where

import Control.Monad (unless)
import THC.Edit.Terminal
#ifdef WITH_TERMINAL
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try, IOException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.IORef
import Data.Either (isLeft)
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Timeout (timeout)
#endif

checks :: IO ()
#ifdef WITH_TERMINAL
checks = bracket temporary removePathForcibly $ \directory -> do
  check "real terminal backend enabled" terminalAvailable
  let config script = TerminalConfig "/bin/sh" ["-c",script] [("THC_TERMINAL_TEST","value with spaces")] directory 20 5
  result <- withTerminal (config "printf '\033[38;2;12;34;56mA\033[0mé'; printf '%s\\n' \"$THC_TERMINAL_TEST\"; pwd; printf 'stderr\\n' >&2; exit 7") $ \terminal -> do
    (snap,raw) <- waitFor terminal (maybe False (const True) . snapshotExitCode)
    check "exit status retained" (snapshotExitCode snap == Just 7)
    check "raw output includes env, cwd and stderr" (all (`BS.isInfixOf` raw) ["value with spaces",B8.pack directory,"stderr"])
    check "snapshot dimensions" (snapshotColumns snap == 20 && snapshotRows snap == 5 && length (snapshotCells snap) == 100)
  either (error . T.unpack) pure result
  result2 <- withTerminal (config "printf ready; read line; stty size; printf '%s' \"$line\"") $ \terminal -> do
    _ <- waitFor terminal (BS.isInfixOf "ready" . snapshotOutput)
    resizeTerminal terminal 31 9 >>= requireRight
    writeTerminal terminal "hello terminal\n" >>= requireRight
    (snap,raw) <- waitFor terminal (maybe False (const True) . snapshotExitCode)
    check "PTY resize and interactive input" ("9 31" `BS.isInfixOf` raw && "hello terminal" `BS.isInfixOf` raw && snapshotRows snap == 9)
  either (error . T.unpack) pure result2
  bulk <- withTerminal (config "head -c 300000 /dev/zero | tr '\\000' x") $ \terminal -> do
    (snap,raw) <- waitFor terminal (maybe False (const True) . snapshotExitCode)
    check "output survives bounded polling through final exit" (BS.length raw == 300000 && snapshotExitCode snap == Just 0)
  either (error . T.unpack) pure bulk
  query <- withTerminal (config "stty raw -echo; printf '\\033[6n'; dd bs=1 count=6 2>/dev/null") $ \terminal -> do
    (_,raw) <- waitFor terminal (maybe False (const True) . snapshotExitCode)
    check "VT query replies reach interactive process" ("\ESC[1;1R" `BS.isInfixOf` raw)
  either (error . T.unpack) pure query
  interrupt <- withTerminal (config "trap 'printf interrupted; exit 0' INT; printf ready; read line") $ \terminal -> do
    _ <- waitFor terminal (BS.isInfixOf "ready" . snapshotOutput)
    writeTerminal terminal "\ETX" >>= requireRight
    (_,raw) <- waitFor terminal (maybe False (const True) . snapshotExitCode)
    check "Ctrl-C interrupts the PTY foreground process" ("interrupted" `BS.isInfixOf` raw)
  either (error . T.unpack) pure interrupt
  rawControl <- withTerminal (config "stty raw -echo; printf ready; dd bs=1 count=1 2>/dev/null") $ \terminal -> do
    _ <- waitFor terminal (BS.isInfixOf "ready" . snapshotOutput)
    writeTerminal terminal "\ETX" >>= requireRight
    (_,raw) <- waitFor terminal (maybe False (const True) . snapshotExitCode)
    check "raw terminal receives Ctrl-C as a byte" ("\ETX" `BS.isInfixOf` raw)
  either (error . T.unpack) pure rawControl
  let foregroundFile=directory </> "foreground-pid"
      jobScript=directory </> "foreground.py"
      heartbeat=directory </> "heartbeat"
  writeFile jobScript $ unlines
    [ "import os,signal,time"
    , "signal.signal(signal.SIGHUP,signal.SIG_IGN)"
    , "with open('foreground-pid','w') as f: f.write(str(os.getpid())+' '+str(os.getpgrp())+' '+str(os.getppid()))"
    , "for tick in range(3000):"
    , "  with open('heartbeat.tmp','w') as f: f.write(str(tick))"
    , "  os.replace('heartbeat.tmp','heartbeat')"
    , "  time.sleep(0.01)"
    ]
  let interactive=(config "") {terminalArguments=["-i","-c","python3 foreground.py; printf finished"]}
  foregroundResult<-withTerminal interactive $ \jobTerminal -> do
    waitFile foregroundFile
    waitFile heartbeat
    details<-B8.words <$> B8.readFile foregroundFile
    (child,group,shell)<-case map B8.unpack details of
      [child,group,shell] -> pure (child,group,shell)
      _ -> error "Invalid foreground fixture process IDs"
    check "interactive fixture has a separate foreground process group" (child==group && group/=shell)
    bracket (newIORef True) (\cleanup -> do
      needed<-readIORef cleanup
      if needed then readProcessWithExitCode "/bin/kill" ["-KILL",child] "" >> pure () else pure ()) $ \cleanup -> do
      killTerminal jobTerminal
      threadDelay 100000
      before<-BS.readFile heartbeat
      threadDelay 150000
      after<-BS.readFile heartbeat
      if before==after then writeIORef cleanup False else pure ()
      check "Stop kills the interactive foreground job before closing the PTY" (before==after)
      killTerminal jobTerminal
  either (error . T.unpack) pure foregroundResult
  let pidFile = directory </> "pid"
  terminal <- startTerminal (config ("echo $$ > '" ++ pidFile ++ "'; sleep 30")) >>= requireRight
  waitFile pidFile
  pid <- readPid pidFile
  killTerminal terminal
  (killed,_) <- waitFor terminal (maybe False (const True) . snapshotExitCode)
  check "kill records signal exit" (snapshotExitCode killed == Just 137)
  closeTerminal terminal
  closeTerminal terminal
  gone <- readProcessWithExitCode "/bin/kill" ["-0",pid] ""
  check "release reaps child" (case gone of (ExitFailure _,_,_) -> True; _ -> False)
  pollTerminal terminal >>= check "released handle safely rejected" . isLeft
  bad <- startTerminal (config "true") {terminalDirectory=directory </> "does-not-exist"}
  check "bad working directory fails at creation" (isLeft bad)
  missing <- startTerminal (config "true") {terminalCommand="/does/not/exist"}
  check "missing executable fails at creation" (isLeft missing)
  writeFile pidFile ""
  let exceptionConfig = config ("echo $$ > '" ++ pidFile ++ "'; sleep 30")
  escaped <- try (withTerminal exceptionConfig $ \_ -> waitFile pidFile >> ioError (userError "fixture exception")) :: IO (Either IOException (Either T.Text ()))
  check "bracket preserves exceptions" (isLeft escaped)
  escapedPid <- readPid pidFile
  (code,_,_) <- readProcessWithExitCode "/bin/kill" ["-0",escapedPid] ""
  check "exception closes process" (code /= ExitSuccess)
  where
    readPid path = B8.readFile path >>= \value -> case B8.words value of
      pid:_ -> pure (B8.unpack pid)
      [] -> error "empty fixture pid"
    requireRight = either (error . T.unpack) pure
    waitFile path = do
      result <- timeout 5000000 loop
      check "fixture became ready" (result == Just ())
      where loop = do
              exists <- doesFileExist path
              ready <- if exists then not . BS.null <$> BS.readFile path else pure False
              if ready then pure () else threadDelay 10000 >> loop
    waitFor terminal predicate = do
      output <- newIORef BS.empty
      result <- timeout 5000000 $ let
        loop = do
          snap <- pollTerminal terminal >>= requireRight
          modifyIORef' output (<> snapshotOutput snap)
          if predicate snap then pure snap else threadDelay 10000 >> loop
        in loop
      snap <- maybe (error "terminal fixture timed out") pure result
      raw <- readIORef output
      pure (snap,raw)
    temporary = do
      root <- getTemporaryDirectory
      (path,h) <- openTempFile root "thc-terminal-check"
      hClose h
      removeFile path
      createDirectory path
      pure path
#else
checks = do
  check "terminal fallback honestly unavailable" (not terminalAvailable)
  result <- startTerminal (TerminalConfig "/bin/sh" [] [] "." 80 25)
  check "fallback rejects process creation" (case result of Left _ -> True; Right _ -> False)
#endif

check :: String -> Bool -> IO ()
check label success = unless success (error label)
