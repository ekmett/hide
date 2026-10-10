-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Main
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.List (isPrefixOf)
import Numeric (showHex)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (getArgs, getEnv, getExecutablePath, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (searchPathSeparator, takeBaseName, takeDirectory)
import System.IO (hSetEncoding, stdout, utf8)

-- A boot-library-only, uninstrumented native child used by CompilersCheck on Windows.
-- The copied executable name selects the tool, and each probe validates the
-- real argv/environment before replying to the host's discovery operation.
main :: IO ()
main=do
  hSetEncoding stdout utf8
  tool<-takeBaseName <$> getExecutablePath
  args<-getArgs
  case tool of
    "ghcup" -> do
      blocked<-lookupEnv "HIDE_COMPILER_FIXTURE_GHCUP"
      if blocked==Just "blocked"
        then getEnv "TEST_READY" >>= (`writeFile` "") >> threadDelay 60000000
        else if blocked==Just "menu" then case args of
          "--offline":"list":_ -> do
            getEnv "MENU_STARTED" >>= (`writeFile` "")
            release<-getEnv "MENU_RELEASE"
            let await=doesFileExist release >>= \ready->unless ready (threadDelay 1000 >> await)
            await
            putStrLn "ghc 9.8.2 installed"
          ["--offline","whereis","ghc",_] -> do
            getEnv "MENU_DONE" >>= (`writeFile` "")
            getEnv "MENU_COMPILER" >>= putStrLn
          _ -> exitFailure
        else case args of
          "--offline":"list":_ -> putStrLn "ghc 9.14.1 latest\nghc 9.8.2 old\nghc wasm32-wasi-9.12.2 cross"
          ["--offline","whereis","ghc",_] -> getEnv "TEST_COMPILER" >>= putStrLn
          _ -> exitFailure
    "cabal" -> do
      require ("--compiler-info" `elem` args)
      scenario<-getEnv "HIDE_COMPILER_FIXTURE_CABAL"
      expected<-getEnv "TEST_COMPILER"
      let selected=[drop (length "--with-compiler=") arg | arg<-args,"--with-compiler=" `isPrefixOf` arg]
      path<-case scenario of
        "automatic" -> require (null selected) >> pure expected
        "explicit" -> require (selected==[expected]) >> pure expected
        "selected" -> case selected of
          [path] -> pure path
          [] -> do
            compiler<-getEnv "GHC_BIN"
            require (compiler==expected)
            environmentPath<-getEnv "PATH"
            require (takeWhile (/=searchPathSeparator) environmentPath==takeDirectory compiler)
            project<-maybe "" id <$> lookupEnv "TEST_PROJECT_COMPILER"
            if null project then findExecutable "ghc" >>= maybe exitFailure pure else pure project
          _ -> exitFailure
        _ -> exitFailure
      identity<-if scenario=="explicit"
        then maybe "ghc-9.14.1" id <$> lookupEnv "TEST_COMPILER_ID"
        else pure "ghc-9.14.1"
      putStrLn ("{\"compiler\":{\"flavour\":\"ghc\",\"id\":"++jsonString identity++",\"path\":"++jsonString path++"}}")
    "ghc-9.14.1" -> case args of
      ["--numeric-version"] -> putStrLn "9.14.1"
      ["--print-libdir"] -> getEnv "TEST_LIBDIR" >>= putStrLn
      _ -> exitFailure
    "hdb-9.14.1" -> pure ()
    "hdb" -> pure ()
    _ -> exitFailure
  where
    require ok=unless ok exitFailure

jsonString :: String -> String
jsonString value='"':concatMap escape value++"\""
  where
    escape '"'="\\\""
    escape '\\'="\\\\"
    escape c | c<' '=let digits=showHex (fromEnum c) "" in "\\u"++replicate (4-length digits) '0'++digits
             | otherwise=[c]
