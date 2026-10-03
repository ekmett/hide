{-# LANGUAGE OverloadedStrings #-}
-- | HLS transport, full-document synchronization and UTF-16 position conversion.
--
-- Initialization and encoding/writes run off the UI thread. Callers queue immutable
-- buffer references and coalesce unchanged identities; the writer performs any
-- whole-document comparison. An executeCommand reserves ownership of incoming
-- applyEdit requests at receipt time. Retirement can drain edit replies before
-- bounded asynchronous shutdown.
module Hide.LSP
  ( Client, Event(..), startClient, stopClient, syncDocuments, notifySaved, request, pollEvents, serverCapabilities
  , executeCommand, replyEdit, retireClient, retireClientAfterReplies
  , fileUri, uriFilePath, offsetPosition, positionOffset, positionValue
  , bufferOffsetPosition, bufferPositionOffset, bufferPositionValue
  ) where

import Control.Concurrent
import Control.Exception hiding (handle)
import Control.Monad (forever, forM_, unless, void, when)
import Data.Aeson hiding (decode)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char ( isHexDigit, ord, toLower)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Numeric (readHex, showHex)
import System.Environment (lookupEnv)
import System.IO
import System.Process
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Hide.Buffer (Buffer, bufferLineColumn, bufferLineOffset, bufferLineAt, bufferSlice)

data Event = Response Int Value | ApplyEdit Int Value Value | Diagnostics FilePath (Maybe Int) Value | ServerError Text
  deriving (Eq, Show)
data Command = Documents [(FilePath,Int,Text)] | Request Int Text Value | Saved FilePath | Reply Value | Barrier (MVar ())

-- | HLS process and protocol worker ownership. retireClientAfterReplies returns
-- a barrier for asynchronous retirement after queued edit replies drain.
data Client = Client
  { commands :: Chan Command, events :: MVar (Seq.Seq Event), nextId :: MVar Int
  , unavailable :: MVar (Maybe Text)
  , serverCapabilities :: IO Value, closeClient :: IO ()
  , commandOwner :: MVar (Maybe Int), replyEdit :: Value -> Bool -> Maybe Text -> IO (), retireClient :: IO (MVar ()), retireClientAfterReplies :: IO (MVar ())
  }

-- Initialization and all subsequent writes happen off the UI thread.
startClient :: FilePath -> IO Client
startClient root = mask $ \restore -> do
  executable <- fromMaybe "haskell-language-server-wrapper" <$> lookupEnv "THC_EDIT_HLS"
  (Just input, Just output, Just errors, process) <- createProcess
    (proc executable ["--lsp"]) { cwd = Just root, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  let ignore action = void action `catch` (\(_ :: IOException) -> pure ())
      cleanup = do
        ignore (terminateProcess process)
        ignore (hClose input)
        ignore (hClose output)
        ignore (hClose errors)
        void (timeout 1000000 (waitForProcess process))
  restore (do
    mapM_ (`hSetBinaryMode` True) [input, output, errors]
    queue <- newChan
    inbox <- newMVar Seq.empty
    counter <- newMVar 1
    writeLock <- newMVar ()
    initialized <- newEmptyMVar
    shutdown <- newEmptyMVar
    stopped <- newMVar False
    stopDone <- newEmptyMVar
    ownerState <- newMVar Nothing
    replyQueue <- newChan
    unavailableState <- newMVar Nothing
    capabilitiesState <- newMVar Null
    errorTail <- newMVar ""
    writerThread <- newEmptyMVar
    let emit event = modifyMVar_ inbox (pure . (Seq.|> event))
        send value = withMVar writeLock $ \_ -> do
          let body = encode value
          BC.hPutStr input (BC.pack ("Content-Length: " ++ show (BL.length body) ++ "\r\n\r\n"))
          BL.hPutStr input body
          hFlush input
        notify method params = send (object ["jsonrpc" .= ("2.0" :: Text), "method" .= method, "params" .= params])
        call ident method params = send (object ["jsonrpc" .= ("2.0" :: Text), "id" .= ident, "method" .= method, "params" .= params])
        failed (exception :: IOException) = do
          tailText <- readMVar errorTail
          let message = "HLS: " <> T.pack (displayException exception) <> if T.null tailText then "" else "\n" <> tailText
          first <- modifyMVar unavailableState (\previous -> pure (Just (fromMaybe message previous), previous == Nothing))
          isStopped <- readMVar stopped
          when (first && not isStopped) (emit (ServerError message))
          void (tryPutMVar initialized False)
          worker <- tryReadMVar writerThread
          self <- myThreadId
          forM_ worker (\thread -> when (thread /= self) (killThread thread))
        folders = [object ["uri" .= fileUri root, "name" .= T.pack root]]
        receive value = case field "method" value :: Maybe Text of
          Just method -> case field "id" value :: Maybe Value of
            Just ident -> do
              let params = fromMaybe Null (field "params" value)
                  respond response = writeChan replyQueue (object ["jsonrpc" .= ("2.0" :: Text), "id" .= ident, "result" .= response])
              if method == "workspace/applyEdit" then do
                owner <- readMVar ownerState
                case owner of
                  Just execution -> emit (ApplyEdit execution ident params)
                  Nothing -> respond (object ["applied" .= False,"failureReason" .= ("No active editor command owns this edit." :: Text)])
              else writeChan replyQueue $ object $ ["jsonrpc" .= ("2.0" :: Text), "id" .= ident] ++ case method of
                "workspace/configuration" -> ["result" .= toJSON (replicate (length (fromMaybe [] (field "items" params :: Maybe [Value]))) Null)]
                "workspace/workspaceFolders" -> ["result" .= folders]
                "window/workDoneProgress/create" -> ["result" .= Null]
                _ -> ["error" .= object ["code" .= (-32601 :: Int), "message" .= ("Unsupported client request: " <> method)]]
            Nothing -> when (method == "textDocument/publishDiagnostics") $ do
              let params = fromMaybe Null (field "params" value)
              case (field "uri" params >>= uriFilePath, field "diagnostics" params) of
                (Just path, Just diagnostics) -> emit (Diagnostics path (field "version" params) diagnostics)
                _ -> pure ()
          Nothing -> case field "id" value :: Maybe Int of
            Just 0 -> do
              let success = case field "error" value :: Maybe Value of Nothing -> True; Just _ -> False
              unless success (failed (userError ("Initialization failed: " ++ show value)))
              when success (modifyMVar_ capabilitiesState (const (pure (fromMaybe Null (field "result" value >>= field "capabilities")))))
              void (tryPutMVar initialized success)
            Just (-1) -> void (tryPutMVar shutdown ())
            Just ident -> do
              modifyMVar_ ownerState (pure . (\owner -> if owner == Just ident then Nothing else owner))
              emit (Response ident value)
            Nothing -> pure ()
    responder <- forkIO (forever (readChan replyQueue >>= send) `catch` failed)
    reader <- forkIO (forever (readFrame output >>= receive) `catch` failed)
    drainer <- forkIO ((let drain = BS.hGetSome errors 4096 >>= \chunk -> unless (BS.null chunk) (modifyMVar_ errorTail (pure . T.takeEnd 4096 . (<> TE.decodeUtf8With lenientDecode chunk)) >> drain) in drain) `catch` (\(_ :: IOException) -> pure ()))
    writer <- forkIO $ (do
      call (0 :: Int) ("initialize" :: Text) (object
        [ "processId" .= Null, "rootUri" .= fileUri root, "workspaceFolders" .= folders
        , "capabilities" .= object
            [ "general" .= object ["positionEncodings" .= ["utf-16" :: Text]]
            , "workspace" .= object ["applyEdit" .= True,"configuration" .= True, "workspaceFolders" .= True, "workspaceEdit" .= object ["documentChanges" .= True,"failureHandling" .= ("transactional" :: Text)]]
            , "textDocument" .= object
                [ "codeAction" .= object ["codeActionLiteralSupport" .= object ["codeActionKind" .= object ["valueSet" .= (["", "quickfix", "refactor", "refactor.extract", "refactor.inline", "refactor.rewrite", "source", "source.organizeImports"] :: [Text])]], "dataSupport" .= True, "disabledSupport" .= True, "isPreferredSupport" .= True, "resolveSupport" .= object ["properties" .= (["edit"] :: [Text])]]
                , "publishDiagnostics" .= object ["versionSupport" .= True]
                , "hover" .= object ["contentFormat" .= ["plaintext" :: Text, "markdown"]]
                , "completion" .= object ["completionItem" .= object ["snippetSupport" .= False]]
                ]
            ]
        ])
      ready <- timeout 20000000 (readMVar initialized)
      case ready of
        Just True -> notify ("initialized" :: Text) (object []) >> writeCommands notify call send queue Map.empty
        Just False -> pure ()
        Nothing -> failed (userError "Initialization timed out")
      ) `catch` failed
    putMVar writerThread writer
    let stop = mask_ $ do
          already <- modifyMVar stopped (\previous -> pure (True,previous))
          if already then readMVar stopDone else (do
            modifyMVar_ unavailableState (pure . Just . fromMaybe "HLS: client stopped")
            killThread writer
            ready <- tryReadMVar initialized
            when (ready == Just True) $ ignore $ do
              void $ timeout 250000 $ do
                call (-1 :: Int) ("shutdown" :: Text) Null
                takeMVar shutdown
              void $ timeout 250000 (notify ("exit" :: Text) Null)
            killThread reader
            killThread responder
            killThread drainer
            void (timeout 250000 (waitForProcess process))
            cleanup) `finally` putMVar stopDone ()
        reply ident applied reason = writeChan queue (Reply (object
          ["jsonrpc" .= ("2.0" :: Text),"id" .= ident,"result" .= object
            (["applied" .= applied] ++ maybe [] (\text -> ["failureReason" .= text]) reason)]))
        retire drainReplies = mask_ $ do
          modifyMVar_ unavailableState (const (pure (Just "HLS command transport retired; starting a fresh server.")))
          modifyMVar_ ownerState (const (pure Nothing))
          drained<-newEmptyMVar
          when drainReplies (writeChan queue (Barrier drained))
          -- A final executeCommand response can arrive before the server reads
          -- our preceding applyEdit replies. Flush that FIFO off the UI thread;
          -- a broken or blocked writer still has a bounded retirement lifetime.
          void (forkIO ((when drainReplies (void (timeout 250000 (readMVar drained)))) `finally` stop))
          pure stopDone
    pure (Client queue inbox counter unavailableState (readMVar capabilitiesState) stop ownerState reply (retire False) (retire True))
    ) `onException` cleanup

stopClient :: Client -> IO ()
stopClient = closeClient

-- | Queue immutable document references for writer-side comparison and encoding.
-- Callers should coalesce unchanged buffer/client identities.
syncDocuments :: Client -> [(FilePath,Int,Text)] -> IO ()
-- The caller only queues immutable references. Equality and full-text encoding
-- run in writeCommands; comparing here forces edited buffer text on the UI.
-- Tooling coalesces unchanged buffer identities before reaching this queue.
syncDocuments client docs = withMVar (unavailable client) $ \failure ->
  when (failure == Nothing) (writeChan (commands client) (Documents docs))

notifySaved :: Client -> FilePath -> IO ()
notifySaved client path = withMVar (unavailable client) $ \failure ->
  when (failure == Nothing) (writeChan (commands client) (Saved path))

request :: Client -> Text -> Value -> IO Int
request client method params = modifyMVar (nextId client) $ \ident -> do
  withMVar (unavailable client) $ \failure -> case failure of
    Nothing -> writeChan (commands client) (Request ident method params)
    Just message -> modifyMVar_ (events client) (pure . (Seq.|> Response ident (object ["id" .= ident, "error" .= object ["code" .= (-32603 :: Int), "message" .= message]])))
  pure (ident+1, ident)

-- | Reserve exclusive command edit ownership before transmitting the request.
executeCommand :: Client -> Text -> Value -> IO (Either Text Int)
executeCommand client name arguments = modifyMVar (nextId client) $ \ident -> do
  outcome <- modifyMVar (commandOwner client) $ \owner -> case owner of
    Just _ -> pure (owner,Left "An HLS command is already running.")
    Nothing -> withMVar (unavailable client) $ \failure -> case failure of
      Just err -> pure (Nothing,Left err)
      Nothing -> do
        writeChan (commands client) (Request ident "workspace/executeCommand"
          (object ["command" .= name,"arguments" .= arguments]))
        pure (Just ident,Right ident)
  pure (ident+1,outcome)

-- | Drain at most 32 protocol events in FIFO order to bound UI adoption work.
pollEvents :: Client -> IO [Event]
-- Bound work per desktop tick while retaining FIFO request/response order.
pollEvents client = modifyMVar (events client) $ \pending ->
  let (ready,rest)=Seq.splitAt 32 pending in pure (rest,toList ready)

writeCommands :: (Text -> Value -> IO ()) -> (Int -> Text -> Value -> IO ()) -> (Value -> IO ()) -> Chan Command -> Map.Map FilePath (Int, Text) -> IO ()
writeCommands notify call send queue previous = do
  command <- readChan queue
  case command of
    Barrier done -> putMVar done () >> writeCommands notify call send queue previous
    Reply value -> send value >> writeCommands notify call send queue previous
    Saved path -> notify "textDocument/didSave" (object ["textDocument" .= object ["uri" .= fileUri path]]) >> writeCommands notify call send queue previous
    Request ident method params -> call ident method params >> writeCommands notify call send queue previous
    Documents docs -> do
      let current = Map.fromList [(path,(version,contents)) | (path,version,contents) <- docs]
      forM_ (Map.keys (previous `Map.difference` current)) $ \path ->
        notify "textDocument/didClose" (object ["textDocument" .= object ["uri" .= fileUri path]])
      forM_ (Map.toList current) $ \(path,(version,contents)) -> case Map.lookup path previous of
        Nothing -> notify "textDocument/didOpen" (object ["textDocument" .= object
          ["uri" .= fileUri path, "languageId" .= ("haskell" :: Text), "version" .= version, "text" .= contents]])
        Just old -> when (old /= (version,contents)) $ notify "textDocument/didChange" (object
          [ "textDocument" .= object ["uri" .= fileUri path, "version" .= version]
          , "contentChanges" .= [object ["text" .= contents]]
          ])
      writeCommands notify call send queue current

field :: FromJSON a => Text -> Value -> Maybe a
field name = parseMaybe (withObject "object" (\o -> o .: fromStringKey name))
  where fromStringKey = Data.Aeson.Key.fromText

-- Content-Length counts UTF-8 bytes, not characters. Bound frames from the server.
readFrame :: Handle -> IO Value
readFrame handle = do
  size <- headers Nothing (0 :: Int)
  body <- bytes size []
  either (ioError . userError) pure (eitherDecodeStrict' body)
  where
    headers found total = do
      line <- BC.hGetLine handle
      let stripped = BC.filter (/= '\r') line
          count = total + BS.length line
      when (count > 8192) (ioError (userError "Oversized LSP header"))
      if BS.null stripped then case found of
        Just n | n >= 0 && n <= 16*1024*1024 -> pure n
        _ -> ioError (userError "Invalid LSP Content-Length")
      else case BC.break (== ':') stripped of
        (name,value) | BC.map toLower name == "content-length" ->
          case readMaybe (BC.unpack (BC.drop 1 value)) of
            Just n | found == Nothing -> headers (Just n) count
            _ -> ioError (userError "Invalid LSP Content-Length")
        _ -> headers found count
    bytes 0 chunks = pure (BS.concat (reverse chunks))
    bytes remaining chunks = do
      chunk <- BS.hGetSome handle remaining
      when (BS.null chunk) (ioError (userError "Haskell language server closed stdout"))
      bytes (remaining-BS.length chunk) (chunk:chunks)

fileUri :: FilePath -> Text
fileUri path = "file://" <> T.concatMap escape (TE.decodeLatin1 (TE.encodeUtf8 (T.pack path)))
  where
    escape c | c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c `elem` ("/-._~:" :: String) = T.singleton c
             | otherwise = let hex = showHex (ord c) "" in T.pack ('%' : (if length hex == 1 then '0':hex else hex))

uriFilePath :: Text -> Maybe FilePath
uriFilePath uri = do
  path <- T.stripPrefix "file://" uri
  local <- if T.isPrefixOf "/" path then Just path else ("/" <>) <$> T.stripPrefix "localhost/" path
  raw <- decode (T.unpack local)
  either (const Nothing) (Just . T.unpack) (TE.decodeUtf8' (BS.pack raw))
  where
    decode [] = Just []
    decode ('%':a:b:rest) | isHexDigit a && isHexDigit b = case readHex [a,b] of
      [(n,"")] -> (fromIntegral (n :: Int):) <$> decode rest
      _ -> Nothing
    decode ('%':_) = Nothing
    decode (c:rest) | c == '?' || c == '#' = Nothing
                    | otherwise = (BS.unpack (TE.encodeUtf8 (T.singleton c)) ++) <$> decode rest

-- Editor offsets count Unicode characters; LSP defaults to UTF-16 code units.
offsetPosition :: Text -> Int -> (Int,Int)
offsetPosition contents offset = T.foldl' step (0,0) (T.take (max 0 offset) contents)
  where step (line,column) c | c == '\n' = (line+1,0)
                            | otherwise = (line,column + if ord c > 0xffff then 2 else 1)

positionOffset :: Text -> (Int,Int) -> Int
positionOffset contents (line,column) = min (T.length contents) (prefix + units 0 (max 0 column) (T.unpack text))
  where
    rows = T.splitOn "\n" contents
    prefix = sum (map ((+1) . T.length) (take (max 0 line) rows))
    text = case drop (max 0 line) rows of row:_ -> T.dropWhileEnd (== '\r') row; [] -> ""
    units count _ [] = count
    units count remaining (c:cs)
      | remaining < width = count
      | otherwise = units (count+1) (remaining-width) cs
      where width = if ord c > 0xffff then 2 else 1

positionValue :: Text -> Int -> Value
positionValue contents offset = let (line,column) = offsetPosition contents offset
  in object ["line" .= line, "character" .= column]

-- | Convert a clamped character offset using measured line lookup, scanning only
-- the prefix of that line for UTF-16 units (including a CR before a newline).
bufferOffsetPosition :: Buffer -> Int -> (Int,Int)
bufferOffsetPosition buffer offset=(row,snd (offsetPosition prefix column))
  where
    (row,column)=bufferLineColumn buffer offset
    prefix=bufferSlice buffer (bufferLineOffset buffer row) column

-- | Convert a clamped UTF-16 position without traversing preceding lines.
-- Strict protocol ranges should round-trip through 'bufferOffsetPosition' to
-- reject out-of-range positions and positions inside a surrogate pair.
bufferPositionOffset :: Buffer -> (Int,Int) -> Int
bufferPositionOffset buffer (row,column)=bufferLineOffset buffer row+positionOffset (bufferLineAt buffer row) (0,column)

-- | The LSP JSON position for a buffer character offset.
bufferPositionValue :: Buffer -> Int -> Value
bufferPositionValue buffer offset=let (row,column)=bufferOffsetPosition buffer offset
  in object ["line" .= row,"character" .= column]
