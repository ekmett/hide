{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module THC.Edit.Session
  (SessionRecord(..), newSessionRecord, rememberSession, forgetSession, listSessions, loadSession, sessionStoreDirectory, checkpointPath, sessionState, sessionActivity) where

import Control.Exception (IOException, bracket, bracketOnError, catch, finally)
import Control.Monad (filterM, unless)
import Data.Aeson (FromJSON, ToJSON, Value, object, (.=), eitherDecodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn, nub)
import Data.Maybe (catMaybes)
import Data.Ord (Down(..))
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (getCurrentDirectory, listDirectory, removeFile, renameFile, getXdgDirectory, XdgDirectory(XdgData), doesFileExist)
import System.FilePath (takeDirectory, takeExtension, dropExtension, (</>))
import System.IO (IOMode(ReadMode), hClose, hFileSize, openBinaryTempFile, withBinaryFile)
import System.IO.Error (isDoesNotExistError)
import System.Timeout (timeout)
import THC.Edit.Protocol (WirePacket(..), writePacket, readPacket)
import THC.Edit.RemoteEndpoint (sessionEndpoint, connectEndpoint, randomIdentity, privateDirectory)

-- Each record describes the host which owns the editor state. Remote records
-- remain available while offline; local records include recoverable checkpoints.
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
recordPath ident = do
  _ <- sessionEndpoint ident
  directory <- sessionStoreDirectory
  pure (directory </> ident ++ ".json")

sessionStoreDirectory :: IO FilePath
sessionStoreDirectory = do
  directory <- getXdgDirectory XdgData "thc-edit/sessions"
  privateDirectory directory
  pure directory

checkpointPath :: String -> IO FilePath
checkpointPath ident = (++".checkpoint") . dropExtension <$> recordPath ident

-- Discovery keeps crashed desktops visible when they have a recovery snapshot.
sessionState :: SessionRecord -> IO String
sessionState record = case sessionHost record of
  Just _ -> pure "remote"
  Nothing -> do
    path <- sessionEndpoint (sessionId record)
    result <- timeout 1000000 (bracket (connectEndpoint path) hClose (const (pure True)) `catch` \(_::IOException) -> pure False)
    if result==Just True then pure "running" else do
      checkpoint <- checkpointPath (sessionId record) >>= doesFileExist
      pure (if checkpoint then "recoverable" else "ended")

-- This handshake never claims the display; it is safe while another frontend is attached.
sessionActivity :: SessionRecord -> IO (Maybe Value)
sessionActivity record | sessionHost record/=Nothing=pure Nothing
sessionActivity record=do
  path<-sessionEndpoint (sessionId record)
  result<-timeout 1000000 $ (bracket (connectEndpoint path) hClose $ \handle->do
    writePacket handle (JsonPacket (object ["type" .= ("session-status"::String)]))
    packet<-readPacket handle
    pure $ case packet of Just (JsonPacket value)->Just value; _->Nothing)
      `catch` \(_::IOException)->pure Nothing
  pure (maybe Nothing id result)

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
forgetSession ident = do
  path <- recordPath ident
  checkpoint <- checkpointPath ident
  legacy <- (++".json") <$> sessionEndpoint ident
  mapM_ (\target -> removeFile target `catch` \(err::IOException) ->
    unless (isDoesNotExistError err) (ioError err)) [path,checkpoint,checkpoint++".agent.json",checkpoint++".agents.json",legacy]

loadSession :: String -> IO (Maybe SessionRecord)
loadSession ident = do
  path <- recordPath ident
  legacy <- (++".json") <$> sessionEndpoint ident
  current <- readRecord path
  maybe (readRecord legacy) (pure . Just) current
  where
    readRecord path=(withBinaryFile path ReadMode $ \handle -> do
      size <- hFileSize handle
      if size>1048576 then pure Nothing else do
        bytes <- BS.hGet handle 1048576
        pure $ case eitherDecodeStrict' bytes of
          Right record | sessionId record==ident -> Just record
          _ -> Nothing) `catch` \(_::IOException) -> pure Nothing

listSessions :: IO [SessionRecord]
listSessions = do
  directory <- sessionStoreDirectory
  legacy <- takeDirectory <$> sessionEndpoint (replicate 48 '0')
  names <- concat <$> mapM listDirectory [directory,legacy]
  records <- mapM loadSession (nub [ident | name<-names, takeExtension name==".json", let ident=dropExtension name, length ident==48, all (`elem` ("0123456789abcdef"::String)) ident])
  sortOn (Down . sessionCreated) <$> filterM (fmap (/="ended") . sessionState) (catMaybes records)
