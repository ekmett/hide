{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module THC.Edit.Session
  (SessionRecord(..), newSessionRecord, rememberSession, forgetSession, listSessions, loadSession) where

import Control.Exception (IOException, bracket, bracketOnError, catch, finally)
import Control.Monad (filterM, unless)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Ord (Down(..))
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (getCurrentDirectory, listDirectory, removeFile, renameFile)
import System.FilePath (takeDirectory, takeExtension, dropExtension)
import System.IO (IOMode(ReadMode), hClose, hFileSize, openBinaryTempFile, withBinaryFile)
import System.IO.Error (isDoesNotExistError)
import System.Timeout (timeout)
import THC.Edit.RemoteEndpoint (sessionEndpoint, connectEndpoint, randomIdentity)

-- Each record describes the host which owns the editor state. Remote records
-- remain available while offline; local records are listed only while alive.
data SessionRecord = SessionRecord
  { sessionId :: String
  , sessionHost :: Maybe String
  , sessionDirectory :: FilePath
  , sessionArguments :: [String]
  , sessionCreated :: UTCTime
  } deriving (Eq, Show, Generic)
instance ToJSON SessionRecord
instance FromJSON SessionRecord

newSessionRecord :: Maybe String -> [String] -> IO SessionRecord
newSessionRecord host args = SessionRecord <$> randomIdentity <*> pure host <*> getCurrentDirectory <*> pure args <*> getCurrentTime

recordPath :: String -> IO FilePath
recordPath ident = (++".json") <$> sessionEndpoint ident

rememberSession :: SessionRecord -> IO ()
rememberSession record = do
  path <- recordPath (sessionId record)
  let bytes=BL.toStrict (encode record)
  unless (BS.length bytes<=1048576) (ioError (userError "Session metadata exceeds 1 MiB"))
  -- The parent directory is owner-only on both platforms; temporary files are
  -- exclusive and inherit its Windows ACL. Rename publishes a complete record.
  bracketOnError (openBinaryTempFile (takeDirectory path) ".session")
    (\(temporary,handle) -> hClose handle `finally` (removeFile temporary `catch` \(err::IOException) -> unless (isDoesNotExistError err) (ioError err))) $ \(temporary,handle) -> do
      BS.hPut handle bytes
      hClose handle
      renameFile temporary path

forgetSession :: String -> IO ()
forgetSession ident = recordPath ident >>= \path -> removeFile path `catch` \(err::IOException) ->
  unless (isDoesNotExistError err) (ioError err)

loadSession :: String -> IO (Maybe SessionRecord)
loadSession ident = do
  path <- recordPath ident
  (withBinaryFile path ReadMode $ \handle -> do
    size <- hFileSize handle
    if size>1048576 then pure Nothing else do
      bytes <- BS.hGet handle 1048576
      pure $ case eitherDecodeStrict' bytes of
        Right record | sessionId record==ident -> Just record
        _ -> Nothing) `catch` \(_::IOException) -> pure Nothing

listSessions :: IO [SessionRecord]
listSessions = do
  directory <- takeDirectory <$> sessionEndpoint (replicate 48 '0')
  names <- listDirectory directory
  records <- mapM loadSession [ident | name<-names, takeExtension name==".json", let ident=dropExtension name, length ident==48, all (`elem` ("0123456789abcdef"::String)) ident]
  sortOn (Down . sessionCreated) <$> filterM alive (catMaybes records)
  where
    alive record = case sessionHost record of
      Just _ -> pure True
      Nothing -> do
        path <- sessionEndpoint (sessionId record)
        result <- timeout 1000000 (bracket (connectEndpoint path) hClose (const (pure True)) `catch` \(_::IOException) -> pure False)
        pure (result==Just True)
