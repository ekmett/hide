{-# LANGUAGE OverloadedStrings #-}
module HdbAcquisitionCheck (checks) where
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Timeout (timeout)
import Hide.Downloads
import Control.Exception (bracket,finally)
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO
import Hide.Compilers (Compiler(..))
import Hide.RemoteEndpoint (randomIdentity)
import Data.Either (isLeft,isRight)
import qualified Data.Text as T
import Hide.HdbAcquisition
checks :: IO ()
checks=do
  let asset=either (error.T.unpack) id (selectHdbAsset "aarch64-apple-darwin" "9.14.1")
      root="hdb-0.14.0.0-ghcup-aarch64-apple-darwin"
      listing=T.unlines ["drwxr-xr-x 0 user group 0 Sep 17 12:18 "<>root<>"/", "-rwxr-xr-x 0 user group 100 Sep 17 12:18 "<>root<>"/hdb", "-rwxr-xr-x 0 user group 200 Sep 17 12:18 "<>root<>"/bin/hdb"]
  check "official matching asset and checksum" (hdbAssetSize asset==13917010 && T.length (hdbAssetSHA256 asset)==64)
  check "version mismatch is unavailable" (isLeft (selectHdbAsset "aarch64-apple-darwin" "9.12.2"))
  check "Windows unavailable without hidden source build" (isLeft (selectHdbAsset "x86_64-windows" "9.14.1"))
  check "ordinary archive accepted" (isRight (validateArchiveListing asset listing))
  check "parent traversal refused" (isLeft (validateArchiveListing asset (listing<>"-rwxr-xr-x 0 user group 1 Sep 17 12:18 "<>root<>"/../escape\n")))
  check "symlink refused" (isLeft (validateArchiveListing asset (T.replace "-rwxr-xr-x" "lrwxr-xr-x" listing)))
  check "wrong root refused" (isLeft (validateArchiveListing asset (T.replace root "other" listing)))
  check "oversized entry refused" (isLeft (validateArchiveListing asset (T.replace "group 100" "group 9999999999" listing)))
  check "duplicate entries refused" (isLeft (validateArchiveListing asset (listing<>listing)))
  old<-lookupEnv "THC_CACHE_HOME"
  let restore=maybe (unsetEnv "THC_CACHE_HOME") (setEnv "THC_CACHE_HOME") old
  (do
    setEnv "THC_CACHE_HOME" "relative/cache"
    selected<-managedToolRoot
    check "relative cache root refused" (isLeft selected)
    host<-hostHdbPlatform
    temp<-getTemporaryDirectory
    token<-randomIdentity
    let directory=temp </> "hdb-acquisition-test-"++take 16 token
    bracket (createDirectory (directory++"-root") >> pure (directory++"-root")) removePathForcibly $ \scratch->do
      let invalidHome=scratch </> "not a home directory"
          explicit=scratch </> "explicit cache"
      writeFile invalidHome "regular file, not a directory"
      oldHome<-lookupEnv "HOME"
      oldXdg<-lookupEnv "XDG_CACHE_HOME"
      let restoreFallback=do
            maybe (unsetEnv "HOME") (setEnv "HOME") oldHome
            maybe (unsetEnv "XDG_CACHE_HOME") (setEnv "XDG_CACHE_HOME") oldXdg
      (do
        setEnv "HOME" invalidHome
        setEnv "XDG_CACHE_HOME" invalidHome
        setEnv "THC_CACHE_HOME" explicit
        chosen<-managedToolRoot
        created<-doesPathExist explicit
        check "absolute override ignores unusable HOME/XDG fallback and creates nothing" (chosen==Right explicit && not created)
        ) `finally` restoreFallback
    case selectHdbAsset host "9.14.1" of
      Left _->pure ()
      Right native->bracket (createDirectory directory >> pure directory) removePathForcibly $ \scratch->do
        let cache=scratch </> "cache with spaces"
            archive=scratch </> "corrupt.tar.gz"
            plan=HdbPlan (Compiler "9.14.1" "/missing/ghc") "/missing/lib" cache native
        setEnv "THC_CACHE_HOME" cache
        chosen<-managedToolRoot
        present<-doesPathExist cache
        check "root query honors override without creating directory" (chosen==Right cache && not present)
        withBinaryFile archive WriteMode (\handle->hSetFileSize handle (hdbAssetSize native))
        result<-installHdbArchive plan archive (const (pure ()))
        check "corrupt official-sized archive refused" (isLeft result)
        contents<-listDirectory cache
        check "failed verification removes private staging and publishes no tool" (null contents)
        let executables=scratch </> "commands"
            curl=executables </> "curl"
            marker=scratch </> "curl.pid"
        createDirectory executables
        writeFile curl "#!/bin/sh\nprintf '%s\\n' $$ > \"$HDB_CURL_MARKER\"\nexec /bin/sleep 60\n"
        permissions<-getPermissions curl
        setPermissions curl permissions {executable=True}
        oldPath<-lookupEnv "PATH"
        oldMarker<-lookupEnv "HDB_CURL_MARKER"
        let restoreEnvironment=do
              maybe (unsetEnv "PATH") (setEnv "PATH") oldPath
              maybe (unsetEnv "HDB_CURL_MARKER") (setEnv "HDB_CURL_MARKER") oldMarker
        (do
          setEnv "PATH" (executables++maybe "" (":"++) oldPath)
          setEnv "HDB_CURL_MARKER" marker
          withDownloads $ \downloads->do
            ident<-startDownload downloads "hdb" (acquireHdb plan) >>= either (error.T.unpack) pure
            let waitStarted=doesFileExist marker >>= \ready->if ready then pure () else threadDelay 1000 >> waitStarted
            timeout 2000000 waitStarted >>= check "curl starts on the worker" . (/=Nothing)
            pid<-takeWhile (/='\n') <$> readFile marker
            _<-cancelDownload downloads ident
            let waitCancelled=downloadSnapshot downloads >>= \rows->if any ((==DownloadCancelled).downloadState) rows then pure () else threadDelay 1000 >> waitCancelled
            timeout 3000000 waitCancelled >>= check "acquisition cancellation completes" . (/=Nothing)
            (code,_,_)<-readProcessWithExitCode "/bin/kill" ["-0",pid] ""
            check "cancelled curl process is reaped" (code/=ExitSuccess)
            leftovers<-listDirectory cache
            check "cancelled transfer removes only its owned staging" (null leftovers)
          ) `finally` restoreEnvironment
    ) `finally` restore
  putStrLn "hdb catalog/archive checks passed"
  where check label ok=unless ok (error label)
