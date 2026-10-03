{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
-- SPDX-License-Identifier: BSD-3-Clause
module Hide.HdbAcquisition
  (HdbAsset(..), HdbPlan(..), selectHdbAsset, hostHdbPlatform, managedToolRoot,
   prepareHdb, acquireHdb, installHdbArchive, validateArchiveListing) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException,bracket,finally,mask,onException,try)
import Control.Monad (forM_,unless,when)
import qualified Data.ByteString as BS
import Data.Char (isAscii,isAlphaNum)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath
import System.Info (arch,os)
import System.IO (IOMode(..),withBinaryFile)
import System.IO.Error (tryIOError)
import System.Process
import System.Timeout (timeout)
import Text.Read (readMaybe)
#ifndef mingw32_HOST_OS
import qualified System.Posix.Files as P
#endif
import Hide.Compilers (Compiler(..))
import Hide.Downloads (DownloadProgress(..),managedToolRoot)
import Hide.Process (processCleanup)
import Hide.RemoteEndpoint (privateDirectory,randomIdentity,withSessionLock)

data HdbAsset=HdbAsset
  {hdbAssetPlatform :: Text,hdbAssetURL :: Text,hdbAssetSHA256 :: Text,hdbAssetSize :: Integer}
  deriving (Eq,Show)
data HdbPlan=HdbPlan
  {hdbCompiler :: Compiler,hdbLibraryDirectory :: FilePath,hdbInstallRoot :: FilePath,hdbAsset :: HdbAsset}
  deriving (Eq,Show)

release :: Text
release="0.14.0.0"

-- Release v0.14.0.0's official bindists.yaml builds every asset with GHC 9.14.1.
-- Digests are the GitHub release-asset SHA256 values; no mutable latest URL.
selectHdbAsset :: Text -> Text -> Either Text HdbAsset
selectHdbAsset platform version
  | version/="9.14.1"=Left "No official hdb binary is available for this exact GHC version; configure an existing adapter."
  | otherwise=case lookup platform catalog of
      Nothing->Left "No official hdb binary is available for this operating system/platform; configure an existing adapter."
      Just (size,digest)->Right (HdbAsset platform ("https://github.com/well-typed/haskell-debugger/releases/download/v"<>release<>"/hdb-"<>release<>"-ghcup-"<>platform<>".tar.gz") digest size)

catalog :: [(Text,(Integer,Text))]
catalog=[
  ("aarch64-apple-darwin",(13917010,"7617e3b7722ed0d78753daa994550d17749c2a82409a4c30e32b1f04b79970ea")),
  ("aarch64-linux-ubuntu2404",(15553909,"a791d54cd830c8f98777b8f17da539051d79e394e4447ae8d0944cc3a3fe5173")),
  ("x86_64-apple-darwin",(14537979,"b47087d822ef2f4fa711c6a4ec10d1719373ca3cc2055b1b9becb8ac5f781e11")),
  ("x86_64-linux-deb10",(13940526,"06cb06acea4c8e6ef62da5bbfb9039eca9735a6c01c89be517bca80e506067fc")),
  ("x86_64-linux-deb11",(13930729,"ffe9ca3c08e6ce6073e601e8a9183727b4e062d5e9b7f35ecebafe6f12bb6f5e")),
  ("x86_64-linux-deb12",(13922354,"63b1a82149a294512c067dcf68d9844044f3d1ced773b967d8919ce54d0c58f4")),
  ("x86_64-linux-deb13",(13924726,"86c72fb1ab58fef443f48e2a4238ff5db41faabaaecb55f4cc0edb0010f6d888")),
  ("x86_64-linux-fedora33",(13944470,"3e54cabe387ea2e27f2293061e47d316dc81d6018ea272627b98cdbcce72568f")),
  ("x86_64-linux-fedora40",(13941285,"af2f509c7bd9aeb57539f6a54c15c3d837a754c0584173a84a692f641ae1221c")),
  ("x86_64-linux-mint202",(13936739,"a689cac8f69d54ad35623735618e8f971aa59d5cfcf589a4872f1643a99f393f")),
  ("x86_64-linux-mint213",(13921629,"24c66a478b1336d7a3021e1a3d89f6b068319f759b0f0bc267c7e099ef89a8c0")),
  ("x86_64-linux-mint222",(13918131,"439786f3a07ab5b0fe48164f972aabf791c6052658c1e8673c0792a5ea87ce9e")),
  ("x86_64-linux-ubuntu2004",(13935590,"84da0fb9ecc8d0e752b2a5e4251d077f3944683c12db2fd4788a3f33a2eb179b")),
  ("x86_64-linux-ubuntu2204",(13923393,"c3f74e0601c640f8a569d13ff2e161bd4a33511d53bc189bf034629a3ad24803")),
  ("x86_64-linux-ubuntu2404",(13918919,"3797c7361ecce2afe6feda36b04660a63add7a011c7ff604cd1961e01598dbe3")),
  ("x86_64-linux-unknown",(13947326,"f0d16b58ea1ca915029147189ebfd0635eb492458cace33ab4f5cd411f067929"))
  ]

hostHdbPlatform :: IO Text
hostHdbPlatform
  | os=="darwin"=pure (T.pack arch<>"-apple-darwin")
  | os=="linux"=do
      contents<-either (const "") id <$> tryIOError (TIO.readFile "/etc/os-release")
      let field key=fromMaybe "" (lookup key [(k,T.dropAround (=='"') (T.drop 1 v)) | line<-T.lines contents,let (k,v)=T.breakOn "=" line])
          distro=case field "ID" of "debian"->"deb"; "linuxmint"->"mint"; value->value
          version=T.filter (/='.') (field "VERSION_ID")
      pure (T.pack arch<>"-linux-"<>distro<>version)
  | otherwise=pure (T.pack arch<>"-"<>T.pack os)

-- | Prepare a concrete offer without fetching an asset or changing the install
-- directory. Compiler probes use private temporary files.
prepareHdb :: Compiler -> IO (Either Text HdbPlan)
prepareHdb compiler=do
  platform<-hostHdbPlatform
  root<-managedToolRoot
  case (selectHdbAsset platform (compilerVersion compiler),root) of
    (Left err,_)->pure (Left err)
    (_,Left err)->pure (Left err)
    (Right asset,Right directory)->asError $ do
      executable<-canonicalizePath (compilerPath compiler)
      temp<-getTemporaryDirectory
      bracket (freshDirectory temp) removePathForcibly $ \scratch->do
        actual<-capture scratch Nothing executable ["--numeric-version"]
        unless (T.strip actual==compilerVersion compiler) (failure "The selected GHC changed version; retry.")
        library<-T.unpack . T.strip <$> capture scratch Nothing executable ["--print-libdir"]
        exists<-doesDirectoryExist library
        unless (isAbsolute library && cleanPath library && exists) (failure "The selected GHC reported an invalid library directory.")
        pure (HdbPlan compiler {compilerPath=executable} library directory asset)

-- | Fetch only a previously accepted offer. The caller owns cancellation.
acquireHdb :: HdbPlan -> (DownloadProgress -> IO ()) -> IO (Either Text FilePath)
acquireHdb plan report=asError $ do
  validatePlan plan
  privateDirectory (hdbInstallRoot plan)
  bracket (freshDirectory (hdbInstallRoot plan)) removePathForcibly $ \stage->do
    let archive=stage </> "download.tar.gz"
        asset=hdbAsset plan
        progress=do
          exists<-doesFileExist archive
          size<-if exists then getFileSize archive else pure 0
          unless (size<=hdbAssetSize asset) (failure "Downloaded archive exceeds its expected size.")
          report (DownloadProgress "Downloading hdb" size (Just (hdbAssetSize asset)))
    report (DownloadProgress "Downloading hdb" 0 (Just (hdbAssetSize asset)))
    run stage Nothing "curl" ["--disable","--fail","--location","--silent","--show-error","--proto","=https","--proto-redir","=https",
      "--connect-timeout","20","--max-time","300","--max-filesize",show (hdbAssetSize asset),"--output",archive,T.unpack (hdbAssetURL asset)] progress
    installArchive plan archive stage report

-- | Verify and install an already downloaded official archive (also useful for
-- isolated qualification). The same pinned checksum and ABI checks apply.
installHdbArchive :: HdbPlan -> FilePath -> (DownloadProgress -> IO ()) -> IO (Either Text FilePath)
installHdbArchive plan archive report=asError $ do
  validatePlan plan
  privateDirectory (hdbInstallRoot plan)
  bracket (freshDirectory (hdbInstallRoot plan)) removePathForcibly $ \stage->do
    size<-getFileSize archive
    unless (size==hdbAssetSize (hdbAsset plan)) (failure "The hdb archive size does not match the official asset.")
    let privateArchive=stage </> "download.tar.gz"
    copyFile archive privateArchive
    installArchive plan privateArchive stage report

validatePlan :: HdbPlan -> IO ()
validatePlan plan=do
  platform<-hostHdbPlatform
  unless (platform==hdbAssetPlatform (hdbAsset plan)) (failure "The hdb offer belongs to another platform.")
  expected<-either failure pure (selectHdbAsset (hdbAssetPlatform (hdbAsset plan)) (compilerVersion (hdbCompiler plan)))
  unless (expected==hdbAsset plan && isAbsolute (hdbInstallRoot plan) && cleanPath (hdbInstallRoot plan)) (failure "Invalid hdb download offer.")
  unless (isAbsolute (compilerPath (hdbCompiler plan)) && cleanPath (compilerPath (hdbCompiler plan)) && isAbsolute (hdbLibraryDirectory plan) && cleanPath (hdbLibraryDirectory plan)) (failure "Invalid selected compiler paths.")

installArchive :: HdbPlan -> FilePath -> FilePath -> (DownloadProgress -> IO ()) -> IO FilePath
installArchive plan archive stage report=do
  let asset=hdbAsset plan
      root=hdbInstallRoot plan
      destination=root </> "tools" </> "hdb" </> T.unpack release </> T.unpack (hdbAssetPlatform asset)
      launcher=root </> "bin" </> "hdb-"++T.unpack (compilerVersion (hdbCompiler plan))
      launcherText="#!/bin/sh\n# hide managed hdb "<>hdbAssetSHA256 asset<>"\nexec "<>shellQuote (T.pack (destination </> "hdb"))<>" \"$@\"\n"
  report (DownloadProgress "Verifying SHA256" 0 Nothing)
  size<-getFileSize archive
  unless (size==hdbAssetSize asset) (failure "The hdb archive size does not match the official asset.")
  digest<-if os=="darwin" then capture stage Nothing "/usr/bin/shasum" ["-a","256",archive]
                        else capture stage Nothing "sha256sum" [archive]
  unless (take 1 (T.words digest)==[hdbAssetSHA256 asset]) (failure "The hdb archive SHA256 does not match the official asset.")
  listing<-capture stage Nothing "tar" ["-tzvf",archive]
  package<-either failure pure (validateArchiveListing asset listing)
  report (DownloadProgress "Extracting hdb" 0 Nothing)
  run stage Nothing "tar" ["-xzf",archive,"-C",stage,"--no-same-owner","--no-same-permissions"] (pure ())
  let unpacked=stage </> T.unpack package
  inherited<-getEnvironment
  let environment=Just ([("GHC_BIN",compilerPath (hdbCompiler plan)),("GHC_LIBDIR",hdbLibraryDirectory plan)]++filter (\(k,_)->k `notElem` ["GHC_BIN","GHC_LIBDIR"]) inherited)
      probe directory=do
        version<-capture stage environment (directory </> "hdb") ["--version"] `catchIO` (\_ -> failure "The downloaded hdb failed its selected GHC version/ABI or startup check.")
        unless (release `T.isInfixOf` version) (failure "The hdb wrapper did not report the expected release.")
  report (DownloadProgress "Checking selected GHC ABI" 0 Nothing)
  probe unpacked
  -- Secure each managed path component; never follow an existing symlink there.
  forM_ [root </> "bin",root </> "tools",root </> "tools" </> "hdb",takeDirectory destination] privateDirectory
  let lock=takeDirectory destination </> (T.unpack (hdbAssetPlatform asset)++".install-lock")
  withSessionLock lock $ do
    exists<-doesPathExist launcher
    symbolic<-pathIsSymbolicLink launcher `catchIO` const (pure False)
    when (exists || symbolic) $ do
      unless (not symbolic) (failure "The versioned hdb launcher already exists and will not be replaced.")
      launcherSize<-getFileSize launcher
      unless (launcherSize==fromIntegral (BS.length (TE.encodeUtf8 launcherText))) (failure "The versioned hdb launcher already exists and will not be replaced.")
      previous<-TIO.readFile launcher
      unless (previous==launcherText) (failure "The versioned hdb launcher already exists and will not be replaced.")
    bundleExists<-doesPathExist destination
    if bundleExists then do
      linked<-pathIsSymbolicLink destination
      unless (not linked) (failure "The hdb bundle destination is a symbolic link.")
      marker<-TIO.readFile (destination </> ".thc-sha256")
      unless (T.strip marker==hdbAssetSHA256 asset) (failure "An unmanaged hdb bundle already exists at the destination.")
      probe destination
    else do
      TIO.writeFile (unpacked </> ".thc-sha256") (hdbAssetSHA256 asset<>"\n")
      renameDirectory unpacked destination
    unless exists $ do
      let prepared=stage </> "launcher"
      TIO.writeFile prepared launcherText
      permissions<-getPermissions prepared
      setPermissions prepared permissions {executable=True}
#ifndef mingw32_HOST_OS
      -- Hard-link creation is exclusive: even a concurrent unmanaged writer is
      -- never overwritten. The intact bundle is usable after a failed publish.
      P.createLink prepared launcher
#else
      failure "No official Windows hdb binary is available."
#endif
    report (DownloadProgress "Ready" (hdbAssetSize asset) (Just (hdbAssetSize asset)))
    pure launcher

-- Accept only regular files/directories in the upstream wrapper/bin/lib layout.
-- Tar's verbose listing reports effective long/PAX names; unknown forms fail.
validateArchiveListing :: HdbAsset -> Text -> Either Text Text
validateArchiveListing asset listing=do
  rows<-mapM parse (T.lines listing)
  unless (not (null rows) && length rows<=512) (Left "Invalid hdb archive entry count.")
  let names=[name | (_,_,name)<-rows]
      roots=nub [T.takeWhile (/='/') name | name<-names]
      allowedRoots=["hdb-"<>release<>"-"<>hdbAssetPlatform asset,"hdb-"<>release<>"-ghcup-"<>hdbAssetPlatform asset]
  root<-case roots of [value] | value `elem` allowedRoots->Right value; _->Left "Unexpected hdb archive root."
  unless (length (nub names)==length names && sum [size | (_,size,_)<-rows]<=536870912) (Left "Duplicate or oversized hdb archive entries.")
  forM_ rows $ \(kind,size,name)->do
    let pieces=T.splitOn "/" (T.dropWhileEnd (=='/') name)
        relative=T.drop (T.length root+1) name
        safe=T.all (\c->isAscii c && (isAlphaNum c || c `elem` ("-._/"::String))) name && all (`notElem` ["",".",".."]) pieces
        allowed=if kind=='d' then name `elem` [root<>"/",root<>"/bin/",root<>"/lib/"]
          else relative `elem` ["hdb","bin/hdb"] || ("lib/lib" `T.isPrefixOf` relative && length pieces==3 && any (`T.isSuffixOf` relative) [".so",".dylib"])
    unless (safe && allowed && size<=134217728) (Left "Unsafe path, type or size in hdb archive.")
  unless (all (`elem` names) [root<>"/hdb",root<>"/bin/hdb"]) (Left "The hdb archive lacks its wrapper or executable.")
  pure root
  where
    parse :: Text -> Either Text (Char,Integer,Text)
    parse line=case T.words line of
      [mode,_,size,_,_,name]->entry mode size name
      [mode,_,_,_,size,_,_,_,name]->entry mode size name
      _->Left "Unrecognized hdb archive listing."
    entry mode size name=case (T.uncons mode,readMaybe (T.unpack size)) of
      (Just (kind,_),Just bytes) | kind `elem` ['d','-'] && bytes>=0->Right (kind,bytes,name)
      _->Left "Archive links, devices and unknown entries are not supported."

-- All subprocesses use files rather than inherited pipes, so cancellation can
-- stop and reap them before closing any IO handles (including native Windows).
run :: FilePath -> Maybe [(String,String)] -> FilePath -> [String] -> IO () -> IO ()
run stage environment command arguments progress=do
  result<-execute stage environment command arguments progress
  unless (result==ExitSuccess) (failure (T.pack (takeFileName command)<>" failed while preparing hdb."))

capture :: FilePath -> Maybe [(String,String)] -> FilePath -> [String] -> IO Text
capture stage environment command arguments=do
  run stage environment command arguments (pure ())
  bytes<-BS.readFile (stage </> "command-output")
  either (const (failure "Invalid UTF-8 in hdb tool output.")) pure (TE.decodeUtf8' bytes)

execute :: FilePath -> Maybe [(String,String)] -> FilePath -> [String] -> IO () -> IO ExitCode
execute stage environment command arguments progress=withBinaryFile output WriteMode $ \handle->mask $ \restore->do
  (_,_,_,process)<-createProcess (proc command arguments) {std_in=NoStream,std_out=UseHandle handle,std_err=NoStream,env=environment,create_group=True}
  stop<-processCleanup process `onException` terminateProcess process
  let loop=do
        size<-getFileSize output
        unless (size<=1048576) (failure "The hdb tool output exceeded its limit.")
        progress
        getProcessExitCode process >>= maybe (threadDelay 100000 >> loop) pure
  restore (timeout (if takeFileName command=="curl" then 310000000 else 30000000) loop >>= maybe (failure "The hdb acquisition step timed out.") pure) `finally` stop
  where output=stage </> "command-output"

freshDirectory :: FilePath -> IO FilePath
freshDirectory parent=do
  token<-randomIdentity
  let path=parent </> ("hdb-stage-"++take 24 token)
  privateDirectory path
  pure path

cleanPath :: FilePath -> Bool
cleanPath path=not (null path) && all (`notElem` ['\0','\n','\r']) path
shellQuote :: Text -> Text
shellQuote value="'"<>T.replace "'" "'\\''" value<>"'"
failure :: Text -> IO a
failure=ioError . userError . T.unpack
asError :: IO a -> IO (Either Text a)
asError action=do
  result<-try action
  pure $ case result of Left (err::IOException)->Left (T.take 1000 (T.pack (show err))); Right value->Right value
catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO action handler=try action >>= either handler pure
