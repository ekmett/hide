{-# LANGUAGE CPP, OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Frontend-owned saved-file snapshots. Receiver workers stage bounded bytes;
-- native UI work receives only the resulting local path. One managed helper runs
-- without blocking reception. Its cleanup precedes removal of this private root.
module Hide.FileExport
  ( FileExports,withFileExports,stageFileExport,startHelperFileExport ) where

import Control.Concurrent.MVar
import Control.Concurrent.Async (Async,asyncWithUnmask,cancel,poll)
import Control.Exception (bracket,bracketOnError,finally,onException)
import Data.Char (isControl)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import System.Directory (getTemporaryDirectory,canonicalizePath,createDirectory,removePathForcibly)
import System.FilePath ((</>),isValid)
import System.IO.Error (tryIOError)
import Hide.FileDragHelper (runFileDragHelper)
#ifdef mingw32_HOST_OS
import Hide.RemoteEndpoint (randomIdentity,privateDirectory)
#else
import System.Posix.Temp (mkdtemp)
#endif

data FileExports=FileExports !FilePath !(MVar State)
data State=State !Int !(Maybe (Async ())) !Bool

-- | Own at most four snapshots of at most 16 MiB each. Files retain their validated
-- basename and remain readable until frontend exit, including after helper close.
-- Closing refuses further operations, cancels/joins the helper, then removes only
-- this frontend's temporary root. Call staging and helper admission on workers.
withFileExports :: (FileExports -> IO a) -> IO a
withFileExports=bracket acquire close
  where
    acquire=do
      base<-getTemporaryDirectory
#ifdef mingw32_HOST_OS
      root<-((base </>) . ("hide-file-export-"++)) <$> randomIdentity
      privateDirectory root
#else
      root<-mkdtemp (base </> "hide-file-export-XXXXXX")
#endif
      resolved<-canonicalizePath root `onException` removePathForcibly root
      FileExports resolved <$> newMVar (State 0 Nothing False)
    close (FileExports root ref)=do
      helper<-modifyMVar ref (\(State count active _)->pure (State count Nothing True,active))
      maybe (pure ()) cancel helper `finally` removePathForcibly root

-- | Stage exact binary bytes without overwriting any previous offer. Invalid names,
-- exhausted budgets, closed scopes and filesystem errors return an explicit error.
-- No path component is silently removed or renamed at this trust boundary.
stageFileExport :: FileExports -> Text -> BS.ByteString -> IO (Either Text FilePath)
stageFileExport (FileExports root ref) name bytes=modifyMVar ref $ \state@(State count active closed)->
  if closed then pure (state,Left "File export frontend has closed.") else do
    staged<-stage root count name bytes
    pure (either (const state) (const (State (count+1) active False)) staged,staged)

-- | Admit one optional drag helper, refusing another while it is active. Staging
-- runs on the caller's receiver worker; the helper and completion callback run on
-- the owned child worker. The callback may publish to the existing frontend queue,
-- but must not write transport packets. Success means admission, not a drop receipt.
startHelperFileExport :: FileExports -> Text -> BS.ByteString -> (Either Text () -> IO ()) -> IO (Either Text ())
startHelperFileExport (FileExports root ref) name bytes complete=modifyMVar ref $ \state@(State count active closed)->do
  busy<-maybe (pure False) (fmap (maybe True (const False)) . poll) active
  if closed || busy then pure (state,Left (if closed then "File export frontend has closed." else "File drag helper is busy; close it before exporting another file.")) else do
    staged<-stage root count name bytes
    case staged of
      Left err->pure (state,Left err)
      Right path->do
        worker<-asyncWithUnmask (\unmask->unmask (runFileDragHelper path >>= complete))
        pure (State (count+1) (Just worker) False,Right ())

stage :: FilePath -> Int -> Text -> BS.ByteString -> IO (Either Text FilePath)
stage root count name bytes
  | T.null name || name `elem` [".",".."] || T.any (\c->isControl c || c=='/' || c=='\\') name ||
      not (isValid (T.unpack name)) || BS.length (TE.encodeUtf8 name)>255 = pure (Left "File export needs a valid basename without path components or controls.")
  | BS.length bytes>16*1024*1024=pure (Left "File export exceeds the 16 MiB limit.")
  | count>=4=pure (Left "File export retains four copies; close this frontend to release them.")
  | otherwise=do
      let directory=root </> show count
          path=directory </> T.unpack name
      result<-tryIOError $ bracketOnError (createDirectory directory) (const (removePathForcibly directory)) $ \_->
        BS.writeFile path bytes >> pure path
      pure (either (Left . T.pack . show) Right result)
