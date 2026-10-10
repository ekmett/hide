-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : FileExportCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module FileExportCheck (checks) where

import Control.Concurrent (newEmptyMVar,putMVar,takeMVar,threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.Environment (lookupEnv,setEnv,unsetEnv)
import System.FilePath ((</>),takeDirectory,takeFileName)
import System.Info (os)
import System.IO (hClose,openTempFile)
import System.Timeout (timeout)
import Hide.FileExport

checks :: IO ()
checks=do
  let check label ok=unless ok (error label)
      right=either (error . T.unpack) pure
      bytes=BS.pack [0,255,13,10,128,1]
  (owner,firstPath)<-withFileExports $ \exports->do
    mapM_ (\name->stageFileExport exports name bytes >>= check "invalid export basenames are refused" . isLeft)
      ["",".","..","../escape","dir/file","dir\\file","bad\0name","bad\nname","bad\DELname",T.replicate 256 "x"]
    path<-stageFileExport exports "λ.bin" bytes >>= right
    check "validated basename is preserved" (takeFileName path=="λ.bin")
    first<-BS.readFile path
    second<-stageFileExport exports "λ.bin" "second" >>= right
    reread<-BS.readFile path
    check "binary snapshots preserve bytes without overwriting prior offers" (first==bytes && reread==bytes && second/=path)
    _<-stageFileExport exports "three" bytes >>= right
    _<-stageFileExport exports "four" bytes >>= right
    full<-stageFileExport exports "five" bytes
    check "four retained offers bound frontend storage" (isLeft full)
    pure (exports,path)
  gone<-doesPathExist (takeDirectory (takeDirectory firstPath))
  check "frontend close removes its owned exports" (not gone)
  closed<-stageFileExport owner "closed" bytes
  check "retained owner cannot stage after scope close" (isLeft closed)
  withFileExports $ \exports->do
    large<-stageFileExport exports "large" (BS.replicate (16*1024*1024+1) 0)
    check "one export cannot exceed 16 MiB" (isLeft large)
  unless (os=="mingw32") $ bracket temporary removePathForcibly $ \root->
    bracket (mapM (\name->do value<-lookupEnv name; pure (name,value)) names) (mapM_ restore) $ \_->do
      let helper=root </> "helper"
          output=root </> "arguments"
          copied=root </> "bytes"
      writeFile helper "#!/bin/sh\nprintf '%s\\000' \"$@\" > \"$THC_FILE_EXPORT_CHECK_ARGS\"\n/bin/cp \"$2\" \"$THC_FILE_EXPORT_CHECK_BYTES\"\nexec /bin/sleep 60\n"
      permissions<-getPermissions helper
      setPermissions helper permissions {executable=True}
      setEnv "THC_EDIT_FILE_DRAG_HELPER" helper
      setEnv "THC_FILE_EXPORT_CHECK_ARGS" output
      setEnv "THC_FILE_EXPORT_CHECK_BYTES" copied
      exportedPath<-withFileExports $ \exports->do
        completion<-newEmptyMVar
        started<-startHelperFileExport exports "saved.bin" bytes (putMVar completion)
        check "helper export starts on its owned worker" (started==Right ())
        ready<-timeout 2000000 (awaitFile copied)
        check "helper receives staged snapshot" (ready/=Nothing)
        sent<-BS.readFile copied
        check "helper sees exact binary bytes" (sent==bytes)
        args<-BS.readFile output
        path<-case BS.split 0 args of
          ["--and-exit",staged,""] | takeFileName (T.unpack (TE.decodeUtf8 staged))=="saved.bin"->pure (T.unpack (TE.decodeUtf8 staged))
          _->error "helper did not get one staged named file via argv"
        busy<-startHelperFileExport exports "busy.bin" bytes (\_->pure ())
        check "one active helper refuses a second launch" (isLeft busy)
        pending<-timeout 10000 (takeMVar completion)
        check "helper leaves frontend unblocked while open" (pending==Nothing)
        pure path
      remains<-doesPathExist exportedPath
      check "helper cleanup finishes before staging scope is removed" (not remains)
  putStrLn "frontend file export checks passed"
  where
    names=["THC_EDIT_FILE_DRAG_HELPER","THC_FILE_EXPORT_CHECK_ARGS","THC_FILE_EXPORT_CHECK_BYTES"]
    restore (name,value)=maybe (unsetEnv name) (setEnv name) value
    awaitFile path=do exists<-doesFileExist path; unless exists (threadDelay 10000 >> awaitFile path)
    temporary=do
      base<-getTemporaryDirectory
      (path,handle)<-openTempFile base "hide-file-export-check"
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path
