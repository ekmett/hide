-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- Windows qualification companion to native-terminal.c. Pass that executable.
-- |
-- Module      : Main
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import System.Directory
import System.Environment (getArgs, lookupEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import qualified TerminalCheck
import Hide.Terminal

main :: IO ()
main = do
  TerminalCheck.checks
  [fixture] <- getArgs
  bracket temporary removePathForcibly $ \directory -> do
    let executable = directory </> "terminal é界.exe"
    copyFile fixture executable
    result <- withTerminal (TerminalConfig executable
      ["--arguments", "", "two words", "a\"b", "trailing\\", "space trailing\\", "é界"]
      [("THC_EXPECTED_CWD",directory),("THC_TERMINAL_TEST","value é界")]
      directory 120 25) $ \terminal -> do
        (code, raw) <- waitExit terminal
        unless (code==0 && "ARGV-CWD-ENV-OK" `BS.isInfixOf` raw) $
          error ("Windows argv/cwd/environment mismatch: "++show (code,raw))
    require result
  directory <- getCurrentDirectory
  shell <- maybe "cmd.exe" id <$> lookupEnv "COMSPEC"
  forM_ [(shell,["/d","/q","/c","echo SHELL-READY & ping -n 30 127.0.0.1"],"SHELL-READY"),
         ("powershell.exe",["-NoProfile","-Command","Write-Output SHELL-READY; Start-Sleep -Seconds 30"],"SHELL-READY")] $ \(command,args,marker) -> do
    result <- withTerminal (TerminalConfig command args [] directory 120 25) $ \terminal -> do
      let ready bytes = do
            snap <- pollTerminal terminal >>= require
            let output=bytes<>snapshotOutput snap
            if marker `BS.isInfixOf` output then pure () else case snapshotExitCode snap of
              Just code -> error ("Premature shell exit: "++show (code,output))
              Nothing -> threadDelay 10000 >> ready output
      bounded (ready BS.empty)
      writeTerminal terminal "\ETX" >>= require
      (code,_) <- waitExit terminal
      putStrLn (command++" Ctrl-C exit="++show code)
    require result
  putStrLn "native Windows wrapper checks passed"
  where
    temporary = do
      parent <- getTemporaryDirectory
      (path,handle) <- openTempFile parent "thc-native-terminal"
      hClose handle
      removeFile path
      let directory=path++" é界"
      createDirectory directory
      pure directory

require :: Either T.Text a -> IO a
require = either (error . T.unpack) pure

bounded :: IO a -> IO a
bounded action = timeout 5000000 action >>= maybe (error "Native terminal fixture timed out") pure

waitExit :: Terminal -> IO (Int, BS.ByteString)
waitExit terminal = bounded (loop BS.empty)
  where
    loop bytes = do
      snap <- pollTerminal terminal >>= require
      let output=bytes<>snapshotOutput snap
      case snapshotExitCode snap of
        Just code -> pure (code,output)
        Nothing -> threadDelay 10000 >> loop output
