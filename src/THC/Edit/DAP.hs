{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module THC.Edit.DAP
  ( Client, Event(..), startClient, stopClient, request, pollEvents
  ) where

import Control.Concurrent.Async (async, cancel, race_)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, unless, void, when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.IORef
import qualified Data.IntSet as Set
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket
import qualified Network.Socket.ByteString as Socket
import System.Timeout (timeout)
import Text.Read (readMaybe)

data Event = Response Int (Either Text Value) | Notification Text Value | Disconnected Text
  deriving (Eq, Show)

data Client = Client
  { outgoing :: TBQueue (Int, Text, Value)
  , incoming :: TBQueue Event
  , incomingBytes :: TVar Int
  , pending :: TVar Set.IntSet
  , nextId :: TVar Int
  , closed :: TVar (Maybe Text)
  , terminal :: TVar [Event]
  , closeClient :: IO ()
  }

-- The UI only enqueues requests. Connection, framing and socket IO run here.
startClient :: Text -> Int -> IO Client
startClient host port = mask $ \restore -> do
  out <- newTBQueueIO 64
  inbox <- newTBQueueIO 128
  bytes <- newTVarIO 0
  awaiting <- newTVarIO Set.empty
  counter <- newTVarIO 1
  unavailable <- newTVarIO Nothing
  finalEvents <- newTVarIO []
  let finish message = atomically $ do
        previous <- readTVar unavailable
        when (previous == Nothing) $ do
          outstanding <- readTVar awaiting
          writeTVar unavailable (Just message)
          writeTVar awaiting Set.empty
          void (flushTBQueue out)
          writeTVar finalEvents ([Response ident (Left message) | ident <- Set.toAscList outstanding] ++ [Disconnected message])
      emit size event = do
        failure <- readTVar unavailable
        when (failure == Nothing) $ do
          used <- readTVar bytes
          check (used + size <= 32*1024*1024)
          writeTBQueue inbox event
          writeTVar bytes (used + size)
      receive size value = case field "type" value :: Maybe Text of
        Just "response" -> case (field "request_seq" value, field "success" value) of
          (Just ident, Just success) -> atomically $ do
            outstanding <- readTVar awaiting
            when (Set.member ident outstanding) $ do
              let result = if success then Right (fromMaybe Null (field "body" value))
                    else Left (fromMaybe "DAP request failed" (field "message" value))
              emit size (Response ident result)
              modifyTVar' awaiting (Set.delete ident)
          _ -> ioError (userError "Malformed DAP response")
        Just "event" -> case field "event" value of
          Just name -> atomically (emit size (Notification name (fromMaybe Null (field "body" value))))
          Nothing -> ioError (userError "Malformed DAP event")
        -- Reverse requests require capabilities we do not advertise.
        _ -> ioError (userError "Unsupported DAP message type")
      run = do
        unless (host `elem` ["localhost", "127.0.0.1", "::1"] && port > 0 && port <= 65535)
          (ioError (userError "DAP endpoint must be localhost, 127.0.0.1 or ::1 with a port in 1..65535"))
        let ipv6 = host == "::1"
            address = if ipv6 then SockAddrInet6 (fromIntegral port) 0 (0,0,0,1) 0
              else SockAddrInet (fromIntegral port) (tupleToHostAddress (127,0,0,1))
        -- Never resolve hostnames: the actual destination is always loopback.
        bracket (socket (if ipv6 then AF_INET6 else AF_INET) Stream defaultProtocol) close $ \connection -> do
          bounded 5000000 "DAP connection timed out" (connect connection address)
          rest <- newIORef BS.empty
          race_
            (forever (readFrame connection rest >>= uncurry receive))
            (forever $ do
              (ident, command, arguments) <- atomically (readTBQueue out)
              let body = encode (object ["seq" .= ident, "type" .= ("request" :: Text), "command" .= command, "arguments" .= arguments])
                  size = BL.length (BL.take (fromIntegral maxFrame + 1) body)
              when (size > fromIntegral maxFrame) (ioError (userError "Oversized outgoing DAP frame"))
              bounded 5000000 "DAP write timed out" $ do
                Socket.sendAll connection (BC.pack ("Content-Length: " ++ show size ++ "\r\n\r\n"))
                Socket.sendAll connection (BL.toStrict body))
  thread <- async ((restore run `catch` (\(err :: SomeException) -> finish ("DAP: " <> T.pack (displayException err))))
    `finally` finish "DAP connection closed")
  pure (Client out inbox bytes awaiting counter unavailable finalEvents (mask_ (finish "DAP client stopped" >> cancel thread)))

-- Cancels connecting, reading or writing workers and closes the socket.
stopClient :: Client -> IO ()
stopClient = closeClient

-- Queue saturation throws IOException promptly, rather than blocking the UI.
-- Responses and events remain bounded even if the caller stops polling.
request :: Client -> Text -> Value -> IO Int
request client command arguments = atomically $ do
  ident <- readTVar (nextId client)
  failure <- readTVar (closed client)
  case failure of
    Just message -> do
      full <- isFullTBQueue (incoming client)
      when full (throwSTM (userError "DAP event queue is full"))
      writeTBQueue (incoming client) (Response ident (Left message))
    Nothing -> do
      outstanding <- readTVar (pending client)
      full <- isFullTBQueue (outgoing client)
      when (full || Set.size outstanding >= 64) (throwSTM (userError "DAP request queue is full"))
      writeTBQueue (outgoing client) (ident, command, arguments)
      writeTVar (pending client) (Set.insert ident outstanding)
  writeTVar (nextId client) (ident+1)
  pure ident

pollEvents :: Client -> IO [Event]
pollEvents client = atomically $ do
  events <- flushTBQueue (incoming client)
  finalEvents <- readTVar (terminal client)
  writeTVar (incomingBytes client) 0
  writeTVar (terminal client) []
  pure (events ++ finalEvents)

field :: FromJSON a => Key -> Value -> Maybe a
field key = parseMaybe (withObject "object" (.: key))

maxFrame :: Int
maxFrame = 16*1024*1024

bounded :: Int -> String -> IO a -> IO a
bounded micros message action = timeout micros action >>= maybe (ioError (userError message)) pure

-- Keep surplus bytes for coalesced frames, and limit the header before reading
-- another chunk. Idle connections may wait; a partial frame gets ten seconds.
readFrame :: Socket -> IORef BS.ByteString -> IO (Int, Value)
readFrame connection rest = do
  initial <- readIORef rest
  first <- if BS.null initial then chunk 4096 else pure initial
  bounded 10000000 "DAP frame timed out" $ do
    (header, prefix) <- headers first
    size <- contentLength header
    (body, surplus) <- bytes size prefix []
    writeIORef rest surplus
    value <- either (ioError . userError . ("Invalid DAP JSON: " ++)) pure (eitherDecodeStrict' body)
    pure (size, value)
  where
    chunk n = do
      part <- Socket.recv connection n
      when (BS.null part) (ioError (userError "DAP peer disconnected"))
      pure part
    headers buffer = case BS.breakSubstring "\r\n\r\n" buffer of
      (header, suffix) | not (BS.null suffix) -> do
        when (BS.length header + 4 > 8192) (ioError (userError "Oversized DAP header"))
        pure (header, BS.drop 4 suffix)
      _ -> do
        when (BS.length buffer >= 8192) (ioError (userError "Oversized DAP header"))
        more <- chunk (min 4096 (8192 - BS.length buffer))
        headers (buffer <> more)
    bytes remaining buffer chunks
      | BS.length buffer >= remaining =
          pure (BS.concat (reverse (BS.take remaining buffer : chunks)), BS.drop remaining buffer)
      | otherwise = do
          more <- chunk (min 65536 (remaining - BS.length buffer))
          bytes (remaining - BS.length buffer) more (buffer : chunks)

contentLength :: BS.ByteString -> IO Int
contentLength header = case [BC.dropWhile (== ' ') (BS.drop 1 value)
    | line <- BC.split '\n' header
    , let (name,value) = BC.break (== ':') (BC.takeWhile (/= '\r') line)
    , BC.map toLower name == "content-length"] of
  [value] | not (BS.null value), BC.all (\c -> c >= '0' && c <= '9') value
          , Just count <- readMaybe (BC.unpack value) :: Maybe Integer
          , count <= fromIntegral maxFrame -> pure (fromIntegral count)
  _ -> ioError (userError "Invalid DAP Content-Length")
