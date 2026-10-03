{-# LANGUAGE OverloadedStrings #-}
module Hide.ACP
  ( Launch(..), Client, Event(..), startClient, stopClient, request, notify, respond, PreparedResponse, prepareResponse, respondPrepared, pollEvents ) where

import Control.Concurrent
import Control.Exception
import Control.Monad (forever, unless, void, when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.Environment (getEnvironment)
import System.Info (os)
import System.IO
import System.Process hiding (cleanupProcess)
import System.Timeout (timeout)

data Launch = Launch { executable :: FilePath, arguments :: [String], environment :: [(String,String)] }
  deriving (Eq, Show)
data Event = Response Int (Either Value Value) | Notification Text Value | Request Value Text Value | Disconnected Text
  deriving (Eq, Show)
data State = State
  { outgoing :: Seq.Seq (Int,BL.ByteString), outgoingBytes :: Int
  , incoming :: Seq.Seq (Int,Event), incomingBytes :: Int
  , pending :: IS.IntSet, nextId :: Int, failure :: Maybe Text }
data Client = Client { state :: MVar State, wakeWriter :: MVar (), disconnect :: Text -> IO (), closeClient :: IO () }

-- Both queues and individual frames are bounded; a stalled peer cannot grow them indefinitely.
frameLimit, queueLimit :: Int
frameLimit = 16 * 1024 * 1024
queueLimit = 32 * 1024 * 1024

startClient :: Launch -> FilePath -> IO Client
startClient launch root = mask_ $ do
  inherited <- getEnvironment
  (Just input, Just output, Just errors, process) <- createProcess
    (proc (executable launch) (arguments launch))
      { cwd = Just root, env = Just (Map.toList (Map.union (Map.fromList (environment launch)) (Map.fromList inherited)))
      , std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, create_group = True }
  pid <- getPid process
  let terminateProvider = do
        -- Terminate the private process group, including adapter-owned children.
        -- Windows terminateProcess uses the process handle; taskkill covers its children.
        case pid of
          Just ident | os == "mingw32" -> ignore (readProcessWithExitCode "taskkill" ["/PID", show ident, "/T", "/F"] "")
                     | otherwise -> ignore (readProcessWithExitCode "/bin/kill" ["-KILL", "--", "-" ++ show ident] "")
          Nothing -> pure ()
        ignore (terminateProcess process)
      closeProcess = do
        mapM_ (ignore . hClose) [input,output,errors]
        void (timeout 1000000 (waitForProcess process))
      cleanupProcess = terminateProvider >> closeProcess
  (do
    mapM_ (`hSetBinaryMode` True) [input,output,errors]
    shared <- newMVar (State Seq.empty 0 Seq.empty 0 IS.empty 1 Nothing)
    wake <- newEmptyMVar
    stopped <- newEmptyMVar
    finished <- newEmptyMVar
    workers <- newEmptyMVar
    errorTail <- newMVar BS.empty
    let failClient reason = do
          tailBytes <- readMVar errorTail
          let message = reason <> if BS.null tailBytes then "" else "\n" <> TE.decodeUtf8With lenientDecode tailBytes
          first <- modifyMVar shared $ \s -> case failure s of
            Just _ -> pure (s,False)
            Nothing -> pure (s { failure = Just message, outgoing = Seq.empty, outgoingBytes = 0, pending = IS.empty
              , incoming = incoming s Seq.>< Seq.fromList [(0,event) | event<-Disconnected message : [Response ident (Left (rpcError message)) | ident <- IS.toList (pending s)]] },True)
          when first (void (tryPutMVar stopped ()))
        failed (e :: IOException) = failClient ("ACP: " <> T.pack (displayException e))
        receive size event = do
          accepted <- modifyMVar shared $ \s ->
            if failure s /= Nothing then pure (s,True)
            else if incomingBytes s + size > queueLimit || Seq.length (incoming s) >= 4096 then pure (s,False)
            else pure (s { incoming = incoming s Seq.|> (size,event), incomingBytes = incomingBytes s + size
                        , pending = case event of Response ident _ -> IS.delete ident (pending s); _ -> pending s },True)
          unless accepted (failClient "ACP: incoming event queue exceeded limit")
        writer = forever $ do
          takeMVar wake
          let drain = do
                item <- modifyMVar shared $ \s -> case Seq.viewl (outgoing s) of
                  Seq.EmptyL -> pure (s,Nothing)
                  (size,body) Seq.:< rest -> pure (s {outgoing = rest, outgoingBytes = outgoingBytes s - size},Just body)
                case item of
                  Nothing -> pure ()
                  Just body -> BL.hPutStr input body >> BS.hPut input "\n" >> hFlush input >> drain
          drain
        drainer = do
          chunk <- BS.hGetSome errors 4096
          unless (BS.null chunk) $ do
            modifyMVar_ errorTail (\old -> let bytes = old <> chunk in pure (BS.drop (max 0 (BS.length bytes - 4096)) bytes))
            drainer
    -- A separate supervisor owns cleanup, so no worker kills itself or waits on its own handle lock.
    void $ forkIO $ (do
      readMVar stopped
      tids <- readMVar workers
      -- Windows synchronous pipe reads can defer thread cancellation until EOF.
      -- Kill the provider tree first, including descendants holding those pipes.
      terminateProvider
      mapM_ killThread tids
      closeProcess) `finally` putMVar finished ()
    tids <- sequence
      [ forkIOWithUnmask (\unmask -> unmask (readFrames output receive) `catch` failed)
      , forkIOWithUnmask (\unmask -> unmask writer `catch` failed)
      , forkIOWithUnmask (\unmask -> unmask drainer `catch` (\(_ :: IOException) -> pure ())) ]
    putMVar workers tids
    pure (Client shared wake failClient (failClient "ACP: client stopped" >> readMVar finished))
    ) `onException` cleanupProcess

ignore :: IO a -> IO ()
ignore action = void action `catch` (\(_ :: IOException) -> pure ())

stopClient :: Client -> IO ()
stopClient = closeClient

rpcError :: Text -> Value
rpcError message = object ["code" .= (-32603 :: Int), "message" .= message]

request :: Client -> Text -> Value -> IO Int
request client method params = do
  ident <- modifyMVar (state client) $ \s -> do
    let ident = nextId s
        body = encode (object ["jsonrpc" .= ("2.0" :: Text), "id" .= ident, "method" .= method, "params" .= params])
        size = fromIntegral (BL.length body)
        problem = case failure s of
          Just reason -> Just reason
          Nothing | size > frameLimit || outgoingBytes s + size > queueLimit || IS.size (pending s) >= 1024 -> Just "ACP: outgoing request queue exceeded limit"
                  | otherwise -> Nothing
        updated = s { nextId = ident + 1 }
    pure (case problem of
      Just reason -> updated { incoming = incoming s Seq.|> (0,Response ident (Left (rpcError reason))) }
      Nothing -> updated { outgoing = outgoing s Seq.|> (size,body), outgoingBytes = outgoingBytes s + size, pending = IS.insert ident (pending s) },ident)
  void (tryPutMVar (wakeWriter client) ())
  pure ident

notify :: Client -> Text -> Value -> IO ()
notify client method params = enqueue client (object ["jsonrpc" .= ("2.0" :: Text), "method" .= method, "params" .= params])

-- Prepared frames contain no deferred JSON encoding or byte counting. File
-- capture workers may prepare one, but only the session may authorize sending.
data PreparedResponse = PreparedResponse !Int BL.ByteString

prepareResponse :: Value -> Either Value Value -> IO PreparedResponse
prepareResponse ident result = prepareMessage (object (["jsonrpc" .= ("2.0" :: Text), "id" .= ident] ++ either (\e -> ["error" .= e]) (\r -> ["result" .= r]) result))

respond :: Client -> Value -> Either Value Value -> IO ()
respond client ident result = prepareResponse ident result >>= respondPrepared client

prepareMessage :: Value -> IO PreparedResponse
prepareMessage value = do
  let body=encode value
  -- Escaping can expand a legal input beyond the frame limit. Stop encoding
  -- after that boundary and retain no rejected payload.
  size<-evaluate (fromIntegral (BL.length (BL.take (fromIntegral frameLimit+1) body)))
  pure (PreparedResponse size (if size>frameLimit then BL.empty else body))

enqueue :: Client -> Value -> IO ()
enqueue client value = prepareMessage value >>= respondPrepared client

respondPrepared :: Client -> PreparedResponse -> IO ()
respondPrepared client (PreparedResponse size body) = do
  accepted <- modifyMVar (state client) $ \s ->
    if failure s /= Nothing then pure (s,True)
    else if size > frameLimit || outgoingBytes s + size > queueLimit || Seq.length (outgoing s) >= 4096 then pure (s,False)
    else pure (s { outgoing = outgoing s Seq.|> (size,body), outgoingBytes = outgoingBytes s + size },True)
  if accepted then void (tryPutMVar (wakeWriter client) ()) else disconnect client "ACP: outgoing message queue exceeded limit"

pollEvents :: Client -> IO [Event]
pollEvents client = modifyMVar (state client) $ \s -> do
  -- The session consumes events while holding the desktop lock. Bound both
  -- count and bytes so a tool burst yields to keyboard/mouse between batches.
  -- A single legal frame may exceed this budget; it must still make progress.
  let takeBatch count bytes pending = case Seq.viewl pending of
        (size,event) Seq.:< rest | count<32 && (count==0 || bytes+size<=262144) ->
          let (batch,used,remaining)=takeBatch (count+1) (bytes+size) rest
          in (event:batch,used,remaining)
        _ -> ([],bytes,pending)
      (events,consumed,left)=takeBatch (0::Int) 0 (incoming s)
  pure (s {incoming=left,incomingBytes=incomingBytes s-consumed},events)

-- Chunked reading bounds memory even when a peer never writes a newline, and
-- delays UTF-8 decoding until a complete frame is available.
readFrames :: Handle -> (Int -> Event -> IO ()) -> IO ()
readFrames stream receive = loop [] 0 BS.empty
  where
    loop pieces size buffer = case BS.elemIndex 10 buffer of
      Just end -> do
        let (part,rest) = BS.splitAt end buffer
        checkSize (size + BS.length part)
        let body = BS.concat (reverse (part:pieces))
        value <- either (ioError . userError . ("Invalid ACP JSON: " ++)) pure (eitherDecodeStrict' body)
        event <- either (ioError . userError) pure (parseEvent value)
        receive (BS.length body) event
        loop [] 0 (BS.drop 1 rest)
      Nothing -> do
        let total = size + BS.length buffer
        checkSize total
        chunk <- BS.hGetSome stream 4096
        when (BS.null chunk) (ioError (userError (if total == 0 then "Provider closed stdout" else "Truncated ACP frame")))
        loop (if BS.null buffer then pieces else buffer:pieces) total chunk
    checkSize size = when (size > frameLimit) (ioError (userError "Oversized ACP frame"))

parseEvent :: Value -> Either String Event
parseEvent (Object o)
  | KM.lookup "jsonrpc" o == Just (String "2.0") = case KM.lookup "method" o of
      Just (String method) -> let params = fromMaybe Null (KM.lookup "params" o) in case KM.lookup "id" o of
        Nothing -> Right (Notification method params)
        Just ident@(String _) -> Right (Request ident method params)
        Just ident@(Number _) -> Right (Request ident method params)
        _ -> Left "Invalid ACP request id"
      Nothing -> case (KM.lookup "id" o >>= parseMaybe parseJSON, KM.lookup "result" o, KM.lookup "error" o) of
        (Just ident,Just result,Nothing) -> Right (Response ident (Right result))
        (Just ident,Nothing,Just err@(Object _)) -> Right (Response ident (Left err))
        _ -> Left "Invalid ACP response"
      _ -> Left "Invalid ACP method"
parseEvent _ = Left "Invalid ACP JSON-RPC message"
