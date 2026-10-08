{-# LANGUAGE OverloadedStrings #-}
module FileDragHelperCheck (checks) where

import Control.Concurrent (forkIO,killThread,newEmptyMVar,putMVar,takeMVar,tryTakeMVar,threadDelay)
import Control.Exception (bracket,finally)
import Control.Monad (unless,void)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.Environment (lookupEnv,setEnv,unsetEnv)
import System.FilePath ((</>))
import System.Info (os)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.IO (openTempFile,hClose)
import System.Timeout (timeout)
import Hide.FileDragHelper

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->
  bracket (mapM (\name->do value<-lookupEnv name; pure (name,value)) names) (mapM_ restore) $ \_->do
    let file=root </> (if os=="mingw32" then "bytes.bin" else "-λ ' \" $() ;\nfile.bin")
        output=root </> "arguments"
        copied=root </> "copy"
        helper=root </> "helper ' $() ; with spaces"
        bytes=BS.pack [0,255,13,10,128,1]
        check label ok=unless ok (error label)
    BS.writeFile file bytes
    setEnv "THC_EDIT_FILE_DRAG_HELPER" ""
    disabled<-runFileDragHelper file
    check "empty override disables optional helper" (isLeft disabled)
    setEnv "THC_EDIT_FILE_DRAG_HELPER" (root </> "not-installed")
    missing<-runFileDragHelper file
    check "unavailable configured helper reports failure" (isLeft missing)
    unless (os=="mingw32") $ do
      writeFile helper "#!/bin/sh\nprintf '%s\\000' \"$@\" > \"$THC_FILE_DRAG_CHECK_ARGS\"\n/bin/cp \"$2\" \"$THC_FILE_DRAG_CHECK_COPY\"\n"
      permissions<-getPermissions helper
      setPermissions helper permissions {executable=True}
      setEnv "THC_EDIT_FILE_DRAG_HELPER" helper
      setEnv "THC_FILE_DRAG_CHECK_ARGS" output
      setEnv "THC_FILE_DRAG_CHECK_COPY" copied
      result<-runFileDragHelper file
      check "literal configured executable starts" (result==Right ())
      arguments<-BS.readFile output
      check "path is exactly one literal argv element" (arguments==TE.encodeUtf8 ("--and-exit\0"<>T.pack file<>"\0"))
      exported<-BS.readFile copied
      source<-BS.readFile file
      check "binary export leaves source unchanged" (exported==bytes && source==bytes)
      setEnv "PATH" root
      unsetEnv "THC_EDIT_FILE_DRAG_HELPER"
      unavailable<-runFileDragHelper file
      check "missing discovered helpers report unavailable" (isLeft unavailable)
      copyFile helper (root </> "dragon-drop")
      discoveredDragon<-runFileDragHelper file
      check "dragon-drop is discovered on PATH" (discoveredDragon==Right ())
      copyFile helper (root </> "ripdrag")
      writeFile (root </> "dragon-drop") "#!/bin/sh\nexit 7\n"
      discoveredRipdrag<-runFileDragHelper file
      check "ripdrag takes discovery precedence" (discoveredRipdrag==Right ())
      setEnv "THC_EDIT_FILE_DRAG_HELPER" helper
      createFileLink file (root </> "symlink.bin")
      replaced<-runFileDragHelper (root </> "symlink.bin")
      check "unresolved symlinks cannot bypass canonical admission" (isLeft replaced)
      directory<-runFileDragHelper root
      absent<-runFileDragHelper (root </> "missing.bin")
      check "directories and missing files cannot be exported" (isLeft directory && isLeft absent)
      writeFile helper "#!/bin/sh\nexit 7\n"
      failed<-runFileDragHelper file
      check "nonzero helper exit is explicit" (case failed of Left err->"7" `T.isInfixOf` err; _->False)
      removeFile output
      writeFile helper "#!/bin/sh\nprintf '%s' \"$$\" > \"$THC_FILE_DRAG_CHECK_ARGS\"\nexec /bin/sleep 60\n"
      completed<-newEmptyMVar
      worker<-forkIO (void (runFileDragHelper file) `finally` putMVar completed ())
      started<-timeout 2000000 (awaitFile output)
      premature<-tryTakeMVar completed
      check "helper remains owned while its window is active" (started/=Nothing && premature==Nothing)
      killThread worker
      stopped<-timeout 3000000 (takeMVar completed)
      check "helper cancellation finishes its owned worker" (started/=Nothing && stopped/=Nothing)
      pid<-readFile output
      (alive,_,_)<-readProcessWithExitCode "/bin/kill" ["-0",pid] ""
      check "cancellation stops the helper process" (alive/=ExitSuccess)
    putStrLn "file drag helper checks passed"
  where
    names=["PATH","THC_EDIT_FILE_DRAG_HELPER","THC_FILE_DRAG_CHECK_ARGS","THC_FILE_DRAG_CHECK_COPY"]
    restore (name,value)=maybe (unsetEnv name) (setEnv name) value
    awaitFile path=do exists<-doesFileExist path; unless exists (threadDelay 10000 >> awaitFile path)
    temporary=do
      base<-getTemporaryDirectory
      (path,handle)<-openTempFile base "hide-file-drag-check"
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path
