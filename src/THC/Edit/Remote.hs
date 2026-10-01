{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
module THC.Edit.Remote
  ( RemotePeer(..), withSSHPeer, runRemoteRelay, runRemoteDaemon ) where

#ifndef WITH_REMOTE
import THC.Edit.Model (Desktop, Effect)
import THC.Edit.Protocol (WirePacket(..))
#endif

#ifdef WITH_REMOTE
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async (race_, withAsync, wait)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, unless, when, void, foldM, forM_)
import Data.Aeson
import Data.Aeson.Types (Parser, Pair, parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.Char (isHexDigit, isLower, isDigit, isSpace)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.Socket as N
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName)
import System.IO
import System.Process
import System.Timeout (timeout)
import THC.Edit.Buffer (bufferBytes)
import THC.Edit.Files (filePath)
import THC.Edit.Font (loadFont)
import THC.Edit.Frontend (modeSize)
import THC.Edit.Model hiding (Paste, message)
import THC.Edit.Protocol
import THC.Edit.RemoteEndpoint
import System.Directory (getCurrentDirectory)

#endif

data RemotePeer = RemotePeer
  { peerSend :: WirePacket -> IO ()
  , peerSendBatch :: [WirePacket] -> IO ()
  , peerReceive :: IO (Maybe WirePacket)
  }

#ifdef WITH_REMOTE
failure :: String -> IO a
failure = ioError . userError

json :: T.Text -> [Pair] -> WirePacket
json kind fields = JsonPacket (object ("type" .= kind : fields))

decodeValue :: (Value -> Parser a) -> Value -> IO a
decodeValue parser = either failure pure . parseEither parser

validIdentity :: String -> Bool
validIdentity s = length s==48 && all (\c -> isHexDigit c && (isDigit c || isLower c)) s

data Hello = Hello String String Int [String] Bool

helloParser :: Value -> Parser Hello
helloParser = withObject "remote hello" $ \o -> do
  kind <- o .: "type"; version <- o .: "version"
  unless (kind==("hello"::T.Text) && version==protocolVersion) (fail "Remote protocol version mismatch")
  session <- o .: "session"; client <- o .: "client"; ack <- o .:? "ack" .!= 0
  unless (validIdentity session && validIdentity client && ack>=0) (fail "Invalid remote session identity or acknowledgement")
  args <- o .:? "args" .!= []
  unless (length args<=128 && sum (map length args)<=65536 && all (all (/='\0')) args) (fail "Invalid remote startup arguments")
  let options=takeWhile (/="--") args
  unless (all (`notElem` ["--remote","--remote-daemon","--ssh","--remote-session","--snapshot","--snapshot-html","--help","-h"]) options &&
    all (\arg -> not (any (`T.isPrefixOf` T.pack arg) ["--remote-daemon=","--ssh=","--remote-session="])) options) (fail "Invalid remote startup mode")
  resume <- o .:? "resume" .!= False
  pure (Hello session client ack args resume)

readHello :: Handle -> IO (Hello, WirePacket)
readHello h = do
  packet <- timeout 15000000 (readPacket h)
  case packet of
    Just (Just p@(JsonPacket value)) -> (,p) <$> decodeValue helloParser value
    _ -> failure "Expected remote protocol hello within 15 seconds"

quietClose :: Handle -> IO ()
quietClose h = hClose h `catch` \(_::IOException) -> pure ()

-- The relay owns no editor state. Its death only closes one daemon attachment.
runRemoteRelay :: [String] -> IO ()
runRemoteRelay args = handle report $ do
  hSetBinaryMode stdin True; hSetBinaryMode stdout True
  hSetBuffering stdout NoBuffering
  (Hello session _ _ startup resume,hello) <- readHello stdin
  path <- sessionEndpoint session
  existing <- try (connectEndpoint path)
  h <- case existing of
    Right connection -> pure connection
    Left (_::IOException) -> do
      -- An existing socket must never be replaced: that could split one session.
      present <- endpointExists path
      when present (failure "Remote session endpoint is unavailable; session may be starting or has stopped unexpectedly")
      when resume (failure "Remote session ended or is unavailable; refusing to restart it automatically")
      executable <- getExecutablePath
      let (options,paths)=break (=="--") (if null startup then args else startup)
          daemonArgs=options++["--remote-daemon",session]++paths
      let logfile=path++".log"
      process <- spawnDetached executable daemonArgs logfile
      void (forkWait process)
      let startupFailure reason = do
            detail <- (withBinaryFile logfile ReadMode $ \logHandle -> do
              size <- hFileSize logHandle
              hSeek logHandle AbsoluteSeek (max 0 (size-8192))
              B8.unpack <$> BS.hGet logHandle 8192) `catch` \(_::IOException) -> pure ""
            failure (reason++"; remote log: "++logfile++"\n"++detail)
          awaitSocket remaining = connectEndpoint path `catch` \(_::IOException) -> do
            exited <- getProcessExitCode process
            case exited of
              Just code -> startupFailure ("Remote editor failed to start ("++show code++")")
              Nothing | remaining<=0 -> startupFailure "Remote editor did not become ready within 60 seconds"
                      | otherwise -> threadDelay 100000 >> awaitSocket (remaining-1)
      awaitSocket (600::Int)
  finally (writePacket h hello >> race_ (relay stdin h) (relay h stdout)) (quietClose h)
  where
    report (err::IOException) = do
      writePacket stdout (json "error" ["message" .= show err]) `catch` \(_::IOException) -> pure ()
      throwIO err
    relay source destination = readPacket source >>= maybe (pure ()) (\packet -> writePacket destination packet >> relay source destination)
    forkWait p = forkIO (void (waitForProcess p))

data Session = Session
  { desktop :: Desktop
  , owner :: Maybe String
  , acknowledged :: Int
  , savedReplies :: [(Int,[WirePacket])]
  , generation :: Int
  , stopped :: Bool
  }

runRemoteDaemon :: String -> Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> Desktop -> IO ()
runRemoteDaemon session scale effects tick initial = do
  path <- sessionEndpoint session
  epoch <- randomIdentity
  font <- loadFont
  state <- newMVar (Session initial {browserFrontend=True} Nothing 0 [] 0 False)
  writer <- newMVar ()
  done <- newEmptyMVar
  commands <- newTBQueueIO 128
  let commandLoop = forever $ do
        (serial,received,input,reply) <- atomically (readTBQueue commands)
        result <- try $ modifyMVar state $ \original -> do
          unless (serial>=0 && serial<=acknowledged original+1 && received>=0 && received<=acknowledged original) (failure "Remote input sequence gap")
          let s=if serial==0 then original else original {savedReplies=filter ((>received).fst) (savedReplies original)}
          if serial==0 then pure (s {desktop=fst (applyInput Blur (desktop s))},([],stopped s,acknowledged s,webDirty (desktop s)))
          else if serial<=acknowledged s then pure (s,(concatMap snd (savedReplies s),stopped s,acknowledged s,webDirty (desktop s))) else do
            let (next,requests)=applyInput input (desktop s)
                anticipated=concatMap (responsePackets next) requests
                retained=concatMap snd (savedReplies s)
                tooLarge=any ((>=maxPacketSize) . packetSize) anticipated
                full=not (null anticipated) && (length (savedReplies s)>=128 || sum (map packetSize (retained++anticipated))>33554432)
            (exited,updated,replies) <- if tooLarge || full
              then pure (False,(desktop s) {status=if tooLarge then "Clipboard or download exceeds 16 MiB; the command was not applied." else "Remote reply journal is full; reconnect before retrying this command."},[])
              else foldM effect (False,next,[]) requests
            -- Replaying a clipboard read could produce a second, distinct paste input.
            let retainedReplies=filter (\packet -> packetType packet/=Just "paste-request") replies
                saved=savedReplies s++[(serial,retainedReplies) | not (null retainedReplies)]
            pure (s {desktop=updated,acknowledged=serial,stopped=exited,savedReplies=saved},(replies,exited,serial,webDirty updated))
        case result of
          Left (_::IOException) -> modifyMVar_ state (\s -> pure s {generation=generation s+1})
          Right _ -> pure ()
        atomically (putTMVar reply (result :: Either IOException ([WirePacket],Bool,Int,Bool)))
      tickLoop = forever $ do
        threadDelay 50000
        modifyMVar_ state $ \s -> if stopped s then pure s else do
          d <- tick (desktop s)
          pure s {desktop=d}
      serve connection = flip finally (quietClose connection) $ handle (\(err::IOException) -> writePacket connection (json "error" ["message" .= show err])) $ do
        (Hello requested client clientAck _ _,_) <- readHello connection
        unless (requested==session) (failure "Wrong remote session")
        available <- tryTakeMVar writer
        case available of
          Nothing -> writePacket connection (json "error" ["message" .= ("Remote editor already has a writer"::T.Text)])
          Just () -> finally (attachment connection client clientAck) (do
            -- Wait behind accepted commands before another writer can attach.
            barrier <- newEmptyTMVarIO
            atomically (writeTBQueue commands (0,0,Blur,barrier))
            void (atomically (takeTMVar barrier))
            putMVar writer ())
      attachment connection client clientAck = do
        (ack,attachmentEpoch,replay) <- modifyMVar state $ \s -> do
          let switched=owner s/=Just client
              ack=if switched then 0 else acknowledged s
              gen=generation s+if switched then 1 else 0
              saved=if switched then [(1,concatMap snd (savedReplies s)) | not (null (savedReplies s))] else filter ((>clientAck).fst) (savedReplies s)
          unless (clientAck<=ack) (failure "Remote server lost acknowledged input")
          pure (s {owner=Just client,acknowledged=ack,generation=gen,savedReplies=saved},(ack,epoch++"-"++show gen,concatMap snd saved))
        writePacket connection (json "hello" ["version" .= protocolVersion,"session" .= session,"epoch" .= attachmentEpoch,"ack" .= ack,"replay" .= length replay])
        writePacket connection (JsonPacket (assetsPacket font scale))
        mapM_ (writePacket connection) replay
        outgoing <- newTBQueueIO 1
        let receive = forever $ do
              packet <- readPacket connection >>= maybe (failure "Remote client detached") pure
              value <- case packet of JsonPacket v -> pure v; _ -> failure "Unexpected remote binary input"
              (serial,received,input) <- decodeValue (\v -> (,,) <$> withObject "sequence" (\o -> o .: "seq") v <*> withObject "receipt" (\o -> o .:? "received" .!= 0) v <*> parseInput v) value
              unless (serial>0) (failure "Remote input sequence must be positive")
              complete <- case input of
                UploadFile name _ -> do
                  payload <- timeout 30000000 (readPacket connection)
                  case payload of Just (Just (BinaryPacket bytes)) -> pure (UploadFile name bytes); _ -> failure "Expected upload bytes"
                _ -> pure input
              reply <- newEmptyTMVarIO
              atomically (writeTBQueue commands (serial,received,complete,reply))
              (responses,exit,committed,isDirty) <- atomically (takeTMVar reply) >>= either throwIO pure
              -- State and sequence are committed before any fallible socket write.
              atomically $ writeTBQueue outgoing (responses++[json "ack" ["seq" .= committed,"dirty" .= isDirty]]++[json "closed" [] | exit])
            send previous = do
              s <- readMVar state
              cwd <- getCurrentDirectory
              let d=desktop s
                  oldRows=maybe [] (\(_,r,_)->r) previous
                  oldMeta=maybe [] (\(_,_,m)->m) previous
                  rows=if fmap (\(old,_,_)->old) previous==Just d then oldRows else frameRows d
                  metadata=frameMetadata cwd d
                  reset=maybe True (\(old,_,_)->screenSize old/=screenSize d || videoMode old/=videoMode d || pixelateUnicode old/=pixelateUnicode d) previous
              when (reset || rows/=oldRows || metadata/=oldMeta) $
                writePacket connection (BinaryPacket (BL.toStrict (framePacket reset oldRows rows (if reset then metadata else filter (`notElem` oldMeta) metadata))))
              next <- timeout 50000 (atomically (readTBQueue outgoing))
              case next of
                Just packets -> do
                  mapM_ (writePacket connection) packets
                  if any ((==Just "closed") . packetType) packets then void (tryPutMVar done ()) else send (Just (d,rows,metadata))
                Nothing -> send (Just (d,rows,metadata))
        race_ receive (send Nothing) `finally` do
          s <- readMVar state
          when (stopped s) (void (tryPutMVar done ()))
      responsePackets d request = case request of
        ReadBrowserClipboard -> [json "paste-request" []]
        WriteBrowserClipboard text -> [json "copy" ["text" .= text]]
        DownloadDocument bid -> case M.lookup bid (buffers d) of
          Nothing -> []
          Just doc -> [json "download" ["name" .= maybe (maybe "NONAME.HS" id (documentSuggestedName doc)) (takeFileName . filePath) (documentFile doc)],BinaryPacket (bufferBytes (documentBuffer doc))]
        _ -> []
      effect result@(True,_,_) _ = pure result
      effect (_,d,replies) request = case request of
        ReadBrowserClipboard -> pure (False,d,replies++[json "paste-request" []])
        WriteBrowserClipboard text -> pure (False,d,replies++[json "copy" ["text" .= text]])
        DownloadDocument bid -> case M.lookup bid (buffers d) of
          Nothing -> pure (False,d,replies)
          Just doc -> do
            let bytes=bufferBytes (documentBuffer doc)
                name=maybe (maybe "NONAME.HS" id (documentSuggestedName doc)) (takeFileName . filePath) (documentFile doc)
            if BS.length bytes>=maxPacketSize
              then pure (False,d {status="Download exceeds 16 MiB; save the file on the remote host."},replies)
              else pure (False,d,replies++[json "download" ["name" .= name],BinaryPacket bytes])
        SetScreenMode mode -> pure (False,(resizeScreenMode (modeSize mode) d) {videoMode=Just mode},replies)
        _ -> do (exited,updated) <- effects d [request]; pure (exited,updated,replies)
  withEndpointListener path $ \socket authenticate -> do
    let acceptLoop = forever $ do
          (sock,_) <- N.accept socket
          connection <- N.socketToHandle sock ReadWriteMode
          hSetBinaryMode connection True; hSetBuffering connection NoBuffering
          void (forkIO (finally
            ((authenticate connection >> serve connection) `catch` \(_::IOException) -> pure ())
            (quietClose connection)))
    withAsync commandLoop $ \inputs -> withAsync tickLoop $ \ticks -> withAsync acceptLoop $ \accepts ->
      race_ (takeMVar done) (race_ (wait inputs) (race_ (wait ticks) (wait accepts)))

packetSize :: WirePacket -> Int
packetSize (BinaryPacket bytes)=BS.length bytes
packetSize (JsonPacket value)=fromIntegral (BL.length (encode value))

packetType :: WirePacket -> Maybe T.Text
packetType (JsonPacket (Object o)) = case KM.lookup "type" o of Just (String t) -> Just t; _ -> Nothing
packetType _ = Nothing

data Journal = Journal
  { nextSequence :: Int
  , lastAck :: Int
  , pending :: [(Int,[WirePacket])]
  , aliases :: M.Map Int Value
  , pendingUpload :: Maybe Value
  , serverEpoch :: Maybe String
  , terminalError :: Maybe String
  }

withSSHPeer :: String -> [String] -> (RemotePeer -> IO ()) -> IO ()
withSSHPeer host args action = do
  unless ((case host of [] -> False; '-':_ -> False; _ -> True) && all (\c -> c>' ' && not (isSpace c)) host && all (all (/='\0')) args) (failure "Invalid SSH host or argument")
  let (sessionOptions,remoteArgs)=extractSession args
  session <- case sessionOptions of
    [] -> randomIdentity
    [value] | validIdentity value -> pure value
    _ -> failure "Specify one --remote-session followed by a 48-character session ID"
  client <- randomIdentity
  hPutStrLn stderr ("Remote session: "++session++" (resume with --remote-session "++session++")")
  journal <- newTVarIO (Journal 1 0 [] M.empty Nothing Nothing Nothing)
  incoming <- newTBQueueIO 8
  let emit packet=atomically (writeTBQueue incoming (Right (Just packet)))
      status connected message=emit (json "connection" ["connected" .= connected,"message" .= (message::T.Text)])
      send packet = sendBatch [packet]
      sendBatch packets = do
        let events=length [() | JsonPacket _<-packets]
        when (events>128 || sum (map packetSize packets)+256*events>33554432) (failure "Remote input batch exceeds the journal capacity")
        atomically $ do
          mapM_ enqueue packets
          j <- readTVar journal
          case pendingUpload j of
            Nothing -> pure ()
            Just _ -> throwSTM (userError "Upload metadata and bytes must be sent together with peerSendBatch")
      enqueue packet = do
        when (packetSize packet>16777216) (throwSTM (userError "Remote packet exceeds 16 MiB"))
        j <- readTVar journal
        maybe (pure ()) (throwSTM . userError) (terminalError j)
        let add value payload = do
              let serial=nextSequence j
                  tagged=case value of Object o -> JsonPacket (Object (KM.insert "seq" (toJSON serial) o)); _ -> JsonPacket value
                  entries=pending j++[(serial,tagged:payload)]
                  bytes=sum [sum (map packetSize packets) | (_,packets)<-entries]
              when (sum (map packetSize (tagged:payload))>33554432 || any ((>=maxPacketSize) . packetSize) (tagged:payload)) (throwSTM (userError "Remote event exceeds the journal or packet size limit"))
              when (serial==maxBound) (throwSTM (userError "Remote input sequence exhausted"))
              check (length entries<=128 && bytes<=33554432)
              let alias=case value of Object o -> KM.lookup "seq" o; _ -> Nothing
              writeTVar journal j {nextSequence=serial+1,pending=entries,pendingUpload=Nothing,
                aliases=maybe (aliases j) (\a -> M.insert serial a (aliases j)) alias}
        case (pendingUpload j,packet) of
          (Just value,BinaryPacket bytes) -> add value [BinaryPacket bytes]
          (Just _,_) -> throwSTM (userError "Expected remote upload bytes")
          (Nothing,JsonPacket value@(Object _)) -> do
            case parseEither parseInput value of Left err -> throwSTM (userError err); Right _ -> pure ()
            if packetType packet==Just "upload" then writeTVar journal j {pendingUpload=Just value} else add value []
          _ -> throwSTM (userError "Unexpected remote binary input")
      receive=atomically (readTBQueue incoming) >>= either failure pure
      peer=RemotePeer send sendBatch receive
      hello = do
        j <- readTVarIO journal
        pure (json "hello" ["version" .= protocolVersion,"session" .= session,"client" .= client,"ack" .= lastAck j,"args" .= remoteArgs,"resume" .= (serverEpoch j/=Nothing || not (null sessionOptions))])
      retire serial dirtyState = do
        forwarded <- atomically $ do
          j <- readTVar journal
          unless (serial>=lastAck j && serial<nextSequence j) (throwSTM (userError "Invalid remote acknowledgement"))
          let (finished,remaining)=M.partitionWithKey (\n _ -> n<=serial) (aliases j)
          writeTVar journal j {lastAck=serial,pending=filter ((>serial).fst) (pending j),aliases=remaining}
          pure (M.elems finished)
        mapM_ (\value -> emit (json "ack" (["seq" .= value]++maybe [] (\dirty -> ["dirty" .= (dirty::Bool)]) dirtyState))) forwarded
      fatal message=atomically (modifyTVar' journal (\j -> j {terminalError=Just message})) >> failure message
      connect handshook = do
        -- Dynamic paths travel in the framed hello, never through a login shell.
        let command="thc-edit --remote"
            sshArgs=["-T","-a","-x","-oForwardAgent=no","-oClearAllForwardings=yes","-oRequestTTY=no","-oServerAliveInterval=15","-oServerAliveCountMax=3","-oConnectTimeout=10","--",host,command]
        bracket (createProcess (proc "ssh" sshArgs) {std_in=CreatePipe,std_out=CreatePipe,std_err=Inherit,close_fds=True}) cleanup $ \(inputPipe,outputPipe,_,process) -> do
          input <- maybe (failure "SSH did not create its input pipe") pure inputPipe
          output <- maybe (failure "SSH did not create its output pipe") pure outputPipe
          hSetBinaryMode input True; hSetBinaryMode output True; hSetBuffering input NoBuffering
          responseResult <- try (hello >>= writePacket input >> timeout 20000000 (readPacket output))
          let response=either (const Nothing) id (responseResult :: Either IOException (Maybe (Maybe WirePacket)))
          value <- case response of
            Just (Just (JsonPacket v)) -> pure v
            _ -> do
              exited <- timeout 1000000 (waitForProcess process)
              j <- readTVarIO journal
              case exited of
                Just (ExitFailure 127) -> fatal "thc-edit is not installed or not on PATH on the remote host"
                Just code | serverEpoch j==Nothing -> fatal ("SSH remote startup failed ("++show code++"); check authentication and the remote thc-edit installation")
                _ -> failure "SSH remote handshake timed out or ended"
          (epoch,ack,replayCount) <- either fatal pure $ parseEither (withObject "remote hello" $ \o -> do
            kind <- o .: "type"
            when (kind==("error"::T.Text)) (o .: "message" >>= fail)
            version <- o .: "version"; returned <- o .: "session"
            unless (kind==("hello"::T.Text) && version==protocolVersion && returned==session) (fail "Remote protocol or session mismatch")
            count <- o .:? "replay" .!= 0
            unless (count>=0 && count<=1024) (fail "Invalid remote replay count")
            (,,) <$> o .: "epoch" <*> o .: "ack" <*> pure (count::Int)) value
          atomically $ do
            j <- readTVar journal
            unless (maybe True (==epoch) (serverEpoch j) && ack>=lastAck j && ack<nextSequence j) $ do
              writeTVar journal j {terminalError=Just "Remote session restarted or lost input; refusing unsafe replay"}
            j' <- readTVar journal
            case terminalError j' of
              Just _ -> pure ()
              Nothing -> writeTVar journal j' {serverEpoch=Just epoch}
          readTVarIO journal >>= maybe (pure ()) failure . terminalError
          assets <- readPacket output >>= maybe (failure "Remote assets missing") pure
          unless (packetType assets==Just "assets") (fatal "Expected remote assets after hello")
          emit assets
          let receiveReplay 0 = pure ()
              receiveReplay n = do
                packet <- readPacket output >>= maybe (failure "Remote reply replay interrupted") pure
                if packetType packet==Just "download" then do
                  unless (n>=2) (fatal "Incomplete remote download replay")
                  payload <- readPacket output
                  case payload of
                    Just binary@(BinaryPacket _) -> atomically (writeTBQueue incoming (Right (Just packet)) >> writeTBQueue incoming (Right (Just binary))) >> receiveReplay (n-2)
                    _ -> failure "Remote download replay interrupted"
                else emit packet >> receiveReplay (n-1)
          receiveReplay replayCount
          retire ack Nothing
          writeIORef handshook True
          status True "Connected"
          let sender sent = do
                entries <- atomically $ do
                  j <- readTVar journal
                  let entries=filter ((>sent).fst) (pending j)
                  check (not (null entries))
                  pure entries
                forM_ entries $ \(_,packets) -> do
                  j <- readTVarIO journal
                  let receipt (JsonPacket (Object fields))=JsonPacket (Object (KM.insert "received" (toJSON (lastAck j)) fields))
                      receipt packet=packet
                  mapM_ (writePacket input . receipt) packets
                sender (fst (last entries))
              receiver = do
                packet <- readPacket output >>= maybe (failure "SSH connection ended") pure
                case packetType packet of
                  Just "ack" -> case packet of
                    JsonPacket v -> do
                      serial <- decodeValue (withObject "ack" (\o -> o .: "seq")) v
                      dirtyState <- decodeValue (withObject "ack" (\o -> o .:? "dirty")) v
                      retire serial dirtyState
                    _ -> pure ()
                  Just "download" -> do
                    payload <- readPacket output
                    case payload of
                      Just binary@(BinaryPacket _) -> atomically $ do
                        writeTBQueue incoming (Right (Just packet))
                        writeTBQueue incoming (Right (Just binary))
                      _ -> failure "SSH disconnected during download; request the download again after reconnecting"
                  Just "error" -> case packet of
                    JsonPacket v -> decodeValue (withObject "error" (\o -> o .: "message")) v >>= fatal
                    _ -> failure "Remote protocol error"
                  _ -> emit packet
                if packetType packet==Just "closed" then atomically (writeTBQueue incoming (Right Nothing)) else receiver
          race_ (sender ack) receiver
      reconnect attempts = do
        handshook <- newIORef False
        result <- try (connect handshook)
        case result of
          Right () -> pure ()
          Left (err::IOException) -> do
            established <- readIORef handshook
            let retries=if established then 0 else attempts
            j <- readTVarIO journal
            case terminalError j of
              Just message -> failure message
              Nothing | retries>=8 -> failure ("SSH reconnect failed: "++show err)
                      | otherwise -> do
                          status False (T.pack ("Disconnected; reconnecting: "++show err))
                          threadDelay (min 5000000 (250000*2^retries))
                          reconnect (retries+1)
      worker = reconnect (0::Int) `catch` \(err::IOException) -> do
        atomically $ modifyTVar' journal (\j -> j {terminalError=Just (show err)})
        atomically (writeTBQueue incoming (Left (show err)))
  withAsync worker $ \_ -> action peer
  where
    extractSession []=([],[])
    extractSession allArgs@("--":_)=([],allArgs)
    extractSession ("--remote-session":value:rest)=let (ids,remaining)=extractSession rest in (value:ids,remaining)
    extractSession (value:rest)=let (ids,remaining)=extractSession rest in (ids,value:remaining)
    cleanup (input,output,_,process) = do
      mapM_ quietClose input; mapM_ quietClose output
      terminateProcess process `catch` \(_::IOException) -> pure ()
      void (waitForProcess process)
#else
withSSHPeer :: String -> [String] -> (RemotePeer -> IO ()) -> IO ()
withSSHPeer _ _ _ = ioError (userError "Remote support is not built. Rebuild with cabal build -fremote")
runRemoteRelay :: [String] -> IO ()
runRemoteRelay _ = ioError (userError "Remote support is not built. Rebuild with cabal build -fremote")
runRemoteDaemon :: String -> Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> Desktop -> IO ()
runRemoteDaemon _ _ _ _ _ = ioError (userError "Remote support is not built. Rebuild with cabal build -fremote")
#endif
