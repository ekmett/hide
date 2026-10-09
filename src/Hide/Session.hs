{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
-- | Private discovery records for live, recoverable and remote desktops.
--
-- Catalog metadata is separate from endpoint ownership and checkpoint payloads.
-- Local status combines a bounded connection probe with recovery-file presence;
-- remote records remain discoverable while offline. Display prefixes can be short,
-- but endpoint operations require the complete session identity.
module Hide.Session
  (SessionRecord(..), newSessionRecord, rememberSession, forgetSession, deleteStoppedSession, withStoppedSession, listSessions, loadSession, sessionStoreDirectory, checkpointPath, sessionState, sessionActivity, shortSessionId) where

import Control.Exception (IOException, bracket, bracketOnError, catch, finally)
import Control.Monad (filterM, unless)
import Data.Aeson (FromJSON, ToJSON, Value, object, (.=), eitherDecodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn, nub, find, isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down(..))
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (getCurrentDirectory, listDirectory, removeFile, renameFile, getXdgDirectory, XdgDirectory(XdgData), doesFileExist)
import System.FilePath (takeDirectory, takeExtension, dropExtension, (</>))
import System.IO (IOMode(ReadMode), hClose, hFileSize, openBinaryTempFile, withBinaryFile)
import System.IO.Error (isDoesNotExistError)
import System.Timeout (timeout)
import Hide.Protocol (WirePacket(..), writePacket, readPacket)
import Hide.RemoteEndpoint (sessionEndpoint, connectEndpoint, randomIdentity, privateDirectory, withSessionLock)

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

-- | Classify a catalog entry using a bounded probe and checkpoint presence.
-- Recoverable status does not validate the checkpoint contents.
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

-- | Publish a bounded session record through a temporary-file rename.
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

-- | Remove discovery/checkpoint/sidecar artifacts; this does not stop a daemon.
forgetSession :: String -> IO ()
forgetSession ident = do
  path <- recordPath ident
  checkpoint <- checkpointPath ident
  legacy <- (++".json") <$> sessionEndpoint ident
  mapM_ (\target -> removeFile target `catch` \(err::IOException) ->
    unless (isDoesNotExistError err) (ioError err)) [path,checkpoint,checkpoint++".agent.json",checkpoint++".agents.json",legacy]

-- | Delete a captured, stopped local session from its owning sidebar worker.
-- 'withStoppedSession' checks ownership before 'forgetSession' removes saved data;
-- the endpoint and lifetime lock file remain in place.
deleteStoppedSession :: Maybe String -> SessionRecord -> IO ()
deleteStoppedSession current captured=withStoppedSession current captured (const (forgetSession (sessionId captured)))

-- | Hold the lifetime lock of the exact captured, stopped local session while
-- using its checkpoint path. Refuse current/remote records, a live or unresponsive
-- endpoint, an occupied lock, or changed metadata before invoking the callback.
-- No daemon is started/stopped and the lock file is never removed.
--
-- The owning sidebar worker serializes these operations. POSIX locks are
-- process-scoped: the current identity must be supplied and concurrent calls
-- for one session within a process are not supported. Failures are 'IOException'.
withStoppedSession :: Maybe String -> SessionRecord -> (FilePath -> IO a) -> IO a
withStoppedSession current captured use=do
  unless (current/=Just ident) (ioError (userError "Cannot use the current editor session as a stopped session."))
  unless (sessionHost captured==Nothing) (ioError (userError "Cannot use a remote saved session from this host."))
  endpoint<-sessionEndpoint ident
  stopped endpoint
  checkpoint<-checkpointPath ident
  withSessionLock (checkpoint++".lock") $ do
    stopped endpoint
    latest<-loadSession ident
    unless (latest==Just captured) (ioError (userError "The saved session changed or disappeared; refresh Sessions before continuing."))
    use checkpoint
  where
    ident=sessionId captured
    stopped endpoint=do
      result<-timeout 1000000 $ (bracket (connectEndpoint endpoint) hClose (const (pure True)))
        `catch` \(_::IOException)->pure False
      unless (result==Just False) (ioError (userError "The session is running or its endpoint did not respond; stop it before continuing."))

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

-- | Choose an unambiguous display prefix within the supplied session inventory.
shortSessionId :: String -> [String] -> String
shortSessionId ident others = fromMaybe ident (find unique [take n ident | n<-[12..length ident]])
  where unique prefix = not (any (isPrefixOf prefix) (filter (/=ident) others))
