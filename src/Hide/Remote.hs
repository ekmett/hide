{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
module Hide.Remote
  ( RemotePeer(..), withLocalPeer, withSSHPeer, withSSHSession, runRemoteRelay, runRemoteDaemon, runRemoteDaemonWithStartup ) where

#ifndef WITH_REMOTE
import Hide.Model (Desktop, Effect)
import Hide.Protocol (WirePacket(..))
import qualified Data.Text as T
import Data.Aeson (Value)
#endif

#ifdef WITH_REMOTE
import Control.Concurrent (threadDelay, forkIO, myThreadId, killThread)
import Control.Concurrent.Async (race_, withAsync, wait, waitEither)
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
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.Socket as N
import System.Environment (getExecutablePath, getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, dropExtension)
import System.IO
import System.Process
import System.Timeout (timeout)
import Hide.Buffer (bufferBytes)
import Hide.Files (filePath)
import Hide.Font (loadFont)
import Hide.Frontend (modeSize)
import Hide.Links (prepareLink,applyLink)
import Paths_hide (getDataFileName)
import Hide.Model hiding (Paste, message)
import Hide.Protocol
import Hide.RemoteEndpoint
import Hide.Session
import System.Directory (getCurrentDirectory, doesFileExist, doesDirectoryExist, removeFile, listDirectory)
import Hide.Recovery (writeCheckpoint, readCheckpoint, checkpointKey)
import Hide.Render (renderKey)

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
  unless (all (`notElem` ["--remote","--remote-daemon","--ssh","--remote-session","--resume","--mcp-editor","--snapshot","--snapshot-html","--help","-h"]) options &&
    all (\arg -> not (any (`T.isPrefixOf` T.pack arg) ["--remote-daemon=","--ssh=","--remote-session=","--resume=","--mcp-editor="])) options) (fail "Invalid remote startup mode")
  resume <- o .:? "resume" .!= False
  pure (Hello session client ack args resume)

readFirstPacket :: Handle -> IO WirePacket
readFirstPacket h = do
  packet <- timeout 15000000 (readPacket h)
  case packet of
    Just (Just p) -> pure p
    _ -> failure "Expected protocol packet within 15 seconds"

parseHelloPacket :: WirePacket -> IO Hello
parseHelloPacket (JsonPacket value) = decodeValue helloParser value
parseHelloPacket _ = failure "Expected remote protocol hello"

readHello :: Handle -> IO (Hello, WirePacket)
readHello h = do
  packet <- readFirstPacket h
  greeting <- parseHelloPacket packet
  pure (greeting,packet)

quietClose :: Handle -> IO ()
quietClose h = hClose h `catch` \(_::IOException) -> pure ()

-- The relay owns no editor state. Its death only closes one daemon attachment.
runRemoteRelay :: [String] -> IO ()
runRemoteRelay args = handle report $ do
  hSetBinaryMode stdin True; hSetBinaryMode stdout True
  hSetBuffering stdout NoBuffering
  (greeting,hello) <- readHello stdin
  (h,shutdown) <- openSession args greeting
  finally (writePacket h hello >> raceWithShutdown shutdown (relay stdin h) (relay h stdout)) (quietClose h)
  where
    report (err::IOException) = do
      writePacket stdout (json "error" ["message" .= show err]) `catch` \(_::IOException) -> pure ()
      throwIO err
    relay source destination = readPacket source >>= maybe (pure ()) (\packet -> writePacket destination packet >> relay source destination)

-- GHC's Windows Handle readiness wait can remain inside a foreign call after
-- socket shutdown. Bound that wait so cancellation can run between polls.
awaitInspectionEOF :: Handle -> IO ()
#ifdef mingw32_HOST_OS
awaitInspectionEOF connection = do
  ready <- hWaitForInput connection 100
  if ready then void (BS.hGetSome connection 1) else awaitInspectionEOF connection
#else
awaitInspectionEOF connection = void (BS.hGetSome connection 1)
#endif

-- Run the wakeup before withAsync joins a blocked socket reader on Windows.
raceWithShutdown :: IO () -> IO a -> IO b -> IO ()
raceWithShutdown shutdown left right = mask $ \restore ->
  withAsync (restore left) $ \a -> withAsync (restore right) $ \b ->
    restore (void (waitEither a b)) `finally` shutdown

-- Opening a local peer and a stdio relay share daemon startup and diagnostics.
openSession :: [String] -> Hello -> IO (Handle,IO ())
openSession args (Hello session _ _ startup resume) = do
  path <- sessionEndpoint session
  existing <- try (connectEndpointWithShutdown path)
  case existing of
    Right connection -> pure connection
    Left (_::IOException) -> do
      recovery <- checkpointPath session >>= doesFileExist
      present <- endpointExists path
      unless recovery $ do
        when present (failure "Remote session endpoint is unavailable; session may be starting or has stopped unexpectedly")
        when resume (failure "Remote session ended or is unavailable; no recovery checkpoint exists")
      saved <- if recovery then loadSession session else pure Nothing
      executable <- getExecutablePath
      currentDirectory <- getCurrentDirectory
      directory <- case saved of
        Just record -> do
          exists <- doesDirectoryExist (sessionDirectory record)
          pure (if exists then sessionDirectory record else currentDirectory)
        Nothing -> pure currentDirectory
      let effective=maybe (if null startup then args else startup) sessionArguments saved
          (options,paths)=break (=="--") effective
          daemonArgs=options++["--remote-daemon",session]++paths
      let logfile=path++".log"
      process <- spawnDetached executable daemonArgs logfile directory
      void (forkIO (void (waitForProcess process)))
      let startupFailure reason = do
            detail <- (withBinaryFile logfile ReadMode $ \logHandle -> do
              size <- hFileSize logHandle
              hSeek logHandle AbsoluteSeek (max 0 (size-8192))
              B8.unpack <$> BS.hGet logHandle 8192) `catch` \(_::IOException) -> pure ""
            failure (reason++"; remote log: "++logfile++"\n"++detail)
          awaitSocket remaining = connectEndpointWithShutdown path `catch` \(_::IOException) -> do
            exited <- getProcessExitCode process
            case exited of
              Just code -> startupFailure ("Remote editor failed to start ("++show code++")")
              Nothing | remaining<=0 -> startupFailure "Remote editor did not become ready within 60 seconds"
                      | otherwise -> threadDelay 100000 >> awaitSocket (remaining-1)
      awaitSocket (600::Int)

data Session = Session
  { desktop :: Desktop
  , owner :: Maybe String
  , acknowledged :: Int
  , savedReplies :: [(Int,[WirePacket])]
  , generation :: Int
  , stopped :: Bool
  }

runRemoteDaemonWithStartup :: IO () -> String -> Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> (Desktop -> Maybe T.Text -> Value -> IO (Bool, Desktop, IO (Maybe Value))) -> Desktop -> IO ()
runRemoteDaemonWithStartup owned session scale effects tick inspect initial = do
  checkpoint <- checkpointPath session
  withSessionLock (checkpoint++".lock") $ runOwned checkpoint
 where
  runOwned checkpoint = do
    path <- sessionEndpoint session
    recoverable <- doesFileExist checkpoint
    recovered <- if recoverable then readCheckpoint checkpoint initial >>= either (failure . T.unpack) pure else pure initial
    -- Only the lock owner may reclaim a crashed daemon's stale endpoint.
    present <- endpointExists path
    when present $ do
      connected <- try (bracket (connectEndpoint path) hClose (const (pure ())))
      case connected of
        Right () -> failure "Editor session already has a listener"
        Left (_::IOException) -> if recoverable then removeFile path else failure "Refusing to replace an unknown session endpoint"
    owned
    runState checkpoint recovered path
  runState checkpoint recovered path = do
    names <- sessionStoreDirectory >>= listDirectory
    let label = T.pack (shortSessionId session (map dropExtension names))
    epoch <- randomIdentity
    font <- loadFont
    state <- newMVar (Session recovered {browserFrontend=True} Nothing 0 [] 0 False)
    writer <- newMVar ()
    done <- newEmptyMVar
    preserveCheckpoint <- newTVarIO False
    inspections <- newTVarIO M.empty
    inspectionClosing <- newTVarIO False
    activeDisplay <- newTVarIO False
    commands <- newTBQueueIO 128
    linkJobs <- newTBQueueIO 4
    linkReplies <- newTBQueueIO 4
    let commandLoop = forever $ do
          (serial,received,input,reply) <- atomically (readTBQueue commands)
          result <- try $ modifyMVar state $ \original -> do
            unless (serial>=0 && serial<=acknowledged original+1 && received>=0 && received<=acknowledged original) (failure "Remote input sequence gap")
            let s=if serial==0 then original else original {savedReplies=filter ((>received).fst) (savedReplies original)}
            if serial==0 then pure (s {desktop=fst (applyInput Blur (desktop s))},([],stopped s,acknowledged s,webDirty (desktop s)))
            else if stopped s then pure (s,([],True,acknowledged s,webDirty (desktop s)))
            else if input==SuspendSession && serial>acknowledged s then do
              -- A failed checkpoint must leave the daemon alive with its buffers.
              writeCheckpoint checkpoint (desktop s) >>= either (failure . T.unpack) pure
              atomically (writeTVar preserveCheckpoint True)
              pure (s {stopped=True,acknowledged=serial},([],True,serial,webDirty (desktop s)))
            else if serial<=acknowledged s then pure (s,(concatMap snd (savedReplies s),stopped s,acknowledged s,webDirty (desktop s))) else do
              let (next,requests)=applyInput input (desktop s)
                  anticipated=concatMap (responsePackets next) requests
                  retained=concatMap snd (savedReplies s)
                  tooLarge=any ((>=maxPacketSize) . packetSize) anticipated
                  full=not (null anticipated) && (length (savedReplies s)>=128 || sum (map packetSize (retained++anticipated))>33554432)
              (exited,updated,replies) <- if tooLarge || full
                then pure (False,(desktop s) {status=if tooLarge then "Clipboard or download exceeds 16 MiB; the command was not applied." else "Remote reply journal is full; reconnect before retrying this command."},[])
                else foldM (effect (owner s,generation s)) (False,next,[]) requests
              -- Replaying a clipboard read could produce a second, distinct paste input.
              let retainedReplies=filter (\packet -> packetType packet `notElem` [Just "paste-request",Just "open-resource"]) replies
                  saved=savedReplies s++[(serial,retainedReplies) | not (null retainedReplies)]
              pure (s {desktop=updated,acknowledged=serial,stopped=exited,savedReplies=saved},(replies,exited,serial,webDirty updated))
          case result of
            Left (_::IOException) -> modifyMVar_ state (\s -> pure s {generation=generation s+1})
            Right _ -> pure ()
          atomically (putTMVar reply (result :: Either IOException ([WirePacket],Bool,Int,Bool)))
        linkLoop = forever $ do
          (stamp,columns,directory,origin,target)<-atomically (readTBQueue linkJobs)
          prepared<-prepareLink True columns directory origin target
          modifyMVar_ state $ \s->do
            if stopped s || stamp/=(owner s,generation s) then pure s else do
              let (updated,packet)=applyLink prepared (desktop s)
              -- Bounded and live-only: the display drains these without replay.
              delivered<-case packet of
                Nothing->pure True
                Just value->atomically $ do
                  full<-isFullTBQueue linkReplies
                  if full then pure False else writeTBQueue linkReplies (stamp,JsonPacket value) >> pure True
              pure s {desktop=if delivered then updated else updated {status="Too many pending links; try again shortly."}}
        tickLoop = forever $ do
          threadDelay 50000
          modifyMVar_ state $ \s -> if stopped s then pure s else do
            d <- tick (desktop s)
            pure s {desktop=d}
        serve shutdown connection = flip finally (quietClose connection) $ handle (\(err::IOException) -> writePacket connection (json "error" ["message" .= show err])) $ do
          first <- readFirstPacket connection
          case first of
            JsonPacket _ | packetType first==Just "session-status" -> do
              current <- readMVar state
              attached <- readTVarIO activeDisplay
              let d=desktop current
              writePacket connection (json "session-status"
                ["attached" .= attached,"agentReplying" .= agentReplying d,"agentQueued" .= agentQueued d,
                 "waiting" .= (dialog d/=Nothing || chatQuestion d/=Nothing),"dirty" .= webDirty d])
            JsonPacket value | packetType first==Just "inspect" -> do
              (agentToken,request) <- decodeValue (withObject "editor inspection" (\o -> do
                token <- o .:? "agentToken"
                unless (maybe True (\credential -> not (T.null credential) && T.length credential<=256) token) (fail "Invalid editor MCP token")
                (token,) <$> o .: "request")) value
              mask $ \restore -> do
                thread<-myThreadId
                wake<-newMVar shutdown
                let stop=withMVar wake id >> killThread thread
                    release=do
                      -- Exclude a late shutdown from a closed/reused descriptor.
                      modifyMVar_ wake (const (pure (pure ())))
                      atomically (modifyTVar' inspections (M.delete thread))
                flip finally release $ do
                  (exited,finish) <- modifyMVar state $ \s -> do
                    when (stopped s) (failure "Editor session is closing")
                    -- Registration and initiation share the desktop lock with
                    -- approval/Exit, so accepted replies cannot miss the drain.
                    atomically (modifyTVar' inspections (M.insert thread stop))
                    (exited,updated,reply) <- inspect (desktop s) agentToken request
                    pure (s {desktop=updated,stopped=stopped s || exited},(exited,reply))
                  -- Start deferred work masked before accepting cancellation. Its
                  -- interruptible waits install their cleanup before EOF can stop it.
                  withAsync finish $ \response -> withAsync (restore (awaitInspectionEOF connection)) $ \eof ->
                    flip finally (do
                      shutdown
                      when exited $ do
                        attached<-atomically (writeTVar inspectionClosing True >> readTVar activeDisplay)
                        when attached $ do
                          flushed<-timeout 2000000 (readMVar done)
                          when (flushed==Nothing) (hPutStrLn stderr "Attached display did not finish its close notification.")
                        void (tryPutMVar done ())) $ do
                      completed <- restore (waitEither response eof)
                      case completed of
                        -- Send before waking the Windows reader: shutdown closes
                        -- both directions, and cancellation alone cannot wake it.
                        Left responseValue -> writePacket connection (JsonPacket (fromMaybe Null responseValue))
                        Right _ -> pure ()
            _ -> do
              Hello requested client clientAck _ _ <- parseHelloPacket first
              unless (requested==session) (failure "Wrong remote session")
              available <- tryTakeMVar writer
              case available of
                Nothing -> writePacket connection (json "error" ["message" .= ("Remote editor already has a writer"::T.Text)])
                Just () -> finally (attachment connection client clientAck) (do
                  closing<-atomically (writeTVar activeDisplay False >> readTVar inspectionClosing)
                  when closing (void (tryPutMVar done ()))
                  -- Wait behind accepted commands before another writer can attach.
                  barrier <- newEmptyTMVarIO
                  atomically (writeTBQueue commands (0,0,Blur,barrier))
                  void (atomically (takeTMVar barrier))
                  putMVar writer ())
        attachment connection client clientAck = do
          (ack,attachmentEpoch,replay) <- modifyMVar state $ \s -> do
            when (stopped s) (failure "Editor session is closing")
            let switched=owner s/=Just client
                ack=if switched then 0 else acknowledged s
                gen=generation s+if switched then 1 else 0
                saved=if switched then [(1,concatMap snd (savedReplies s)) | not (null (savedReplies s))] else filter ((>clientAck).fst) (savedReplies s)
            unless (clientAck<=ack) (failure "Remote server lost acknowledged input")
            atomically (writeTVar activeDisplay True)
            pure (s {owner=Just client,acknowledged=ack,generation=gen,savedReplies=saved},(ack,epoch++"-"++show gen,concatMap snd saved))
          writePacket connection (json "hello" ["version" .= protocolVersion,"session" .= session,"epoch" .= attachmentEpoch,"ack" .= ack,"replay" .= length replay])
          writePacket connection (JsonPacket (assetsPacket font scale))
          mapM_ (writePacket connection) replay
          -- Keep input consumption independent of frame generation. Drain replies
          -- in order as a bounded batch, then render the latest state once.
          outgoing <- newTBQueueIO 128
          pending <- newTBQueueIO 128
          inflight <- newTVarIO (0::Int)
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
                atomically $ do
                  readTVar inspectionClosing >>= check . not
                  writeTBQueue commands (serial,received,complete,reply)
                  writeTBQueue pending reply
                  modifyTVar' inflight (+1)
              respond = forever $ do
                reply <- atomically (readTBQueue pending)
                (responses,exit,committed,isDirty) <- atomically (takeTMVar reply) >>= either throwIO pure
                resumable <- readTVarIO preserveCheckpoint
                -- State and sequence are committed before any fallible socket write.
                atomically $ do
                  writeTBQueue outgoing (responses++[json "ack" ["seq" .= committed,"dirty" .= isDirty]]++[json "closed" ["resumable" .= resumable] | exit])
                  modifyTVar' inflight (subtract 1)
              send previous = do
                s <- readMVar state
                cwd <- getCurrentDirectory
                let d=desktop s
                stateKey<-renderKey d
                let key=(stateKey,cwd)
                    resetKey=(screenSize d,videoMode d,pixelateUnicode d)
                    oldRows=maybe [] (\(_,_,r,_)->r) previous
                    oldMeta=maybe [] (\(_,_,_,m)->m) previous
                    -- The same bounded key as the web/native frontends: never
                    -- compare desktops, file contents, or undo history.
                    sameFrame=maybe False (\(old,_,_,_)->old==key) previous
                    rows=if sameFrame then oldRows else frameRows d
                    metadata=if sameFrame then oldMeta else
                      [if name=="title" then (name,String (applicationTitle cwd d<>" ["<>label<>"]")) else (name,value) | (name,value)<-frameMetadata cwd d]
                    reset=maybe True (\(_,old,_,_)->old/=resetKey) previous
                case clipboardExport d of
                  (serial,Just text) -> do
                    writePacket connection (json "copy" ["text" .= text])
                    -- This is explicit clipboard_write output, never clipboard
                    -- capture. Preserve any newer export, even identical text.
                    modifyMVar_ state $ \current ->
                      let latest=desktop current
                      in pure $ if fst (clipboardExport latest)==serial
                        then current {desktop=latest {clipboardExport=(serial,Nothing)}} else current
                  _ -> pure ()
                when (reset || (not sameFrame && (rows/=oldRows || metadata/=oldMeta))) $
                  writePacket connection (BinaryPacket (BL.toStrict (framePacket reset oldRows rows (if reset then metadata else filter (`notElem` oldMeta) metadata))))
                next <- timeout 50000 $ atomically $
                  (do first<-readTBQueue outgoing
                      rest<-flushTBQueue outgoing
                      pure (Just (concat (first:rest)))) `orElse` (do
                    first<-readTBQueue linkReplies
                    rest<-flushTBQueue linkReplies
                    pure (Just (map snd (filter ((==(owner s,generation s)).fst) (first:rest))))) `orElse` (do
                    readTVar inspectionClosing >>= check
                    readTVar inflight >>= check . (==0)
                    pure Nothing)
                case next of
                  Just (Just packets) -> do
                    mapM_ (writePacket connection) packets
                    if any ((==Just "closed") . packetType) packets then void (tryPutMVar done ()) else send (Just (key,resetKey,rows,metadata))
                  Just Nothing -> do
                    -- An MCP Exit has no display input to carry its close. Flush
                    -- accepted input replies first, then acknowledge and close.
                    final<-readMVar state
                    writePacket connection (json "ack" ["seq" .= acknowledged final,"dirty" .= webDirty (desktop final)])
                    writePacket connection (json "closed" [])
                    void (tryPutMVar done ())
                  Nothing -> send (Just (key,resetKey,rows,metadata))
          race_ receive (race_ respond (send Nothing)) `finally` do
            s <- readMVar state
            when (stopped s) (void (tryPutMVar done ()))
        responsePackets d request = case request of
          ReadBrowserClipboard -> [json "paste-request" []]
          WriteBrowserClipboard text -> [json "copy" ["text" .= text]]
          DownloadDocument bid -> case M.lookup bid (buffers d) of
            Nothing -> []
            Just doc -> [json "download" ["name" .= maybe (maybe "NONAME.HS" id (documentSuggestedName doc)) (takeFileName . filePath) (documentFile doc)],BinaryPacket (bufferBytes (documentBuffer doc))]
          _ -> []
        queueLink stamp d replies origin target=do
          queued<-atomically $ do
            full<-isFullTBQueue linkJobs
            if full then pure False else do
              writeTBQueue linkJobs (stamp,max 20 (min 76 (fst (screenSize d)-treeWidthOf d-4)),startingDirectory d,origin,target)
              pure True
          pure (False,d {status=if queued then "Opening link…" else "Link loader is busy; try again shortly."},replies)
        effect _ result@(True,_,_) _ = pure result
        effect stamp (_,d,replies) request = case request of
          ReadHelp -> getDataFileName "README.md" >>= \helpPath -> queueLink stamp d replies (Just helpPath) ""
          FollowLink origin target -> queueLink stamp d replies origin target
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
    let checkpointNow d = do
          result <- writeCheckpoint checkpoint d
          case result of
            Left err -> do
              hPutStrLn stderr ("Recovery checkpoint: "++T.unpack err)
              modifyMVar_ state (\current -> pure current {desktop=(desktop current) {status="Recovery checkpoint failed: "<>err}})
            Right () -> pure ()
          pure result
        checkpointLoop previous = do
          threadDelay 1000000
          current <- readMVar state
          if stopped current then pure () else do
            let snapshot=desktop current
            key <- checkpointKey snapshot
            if Just key==previous then checkpointLoop previous else do
              result <- checkpointNow snapshot
              checkpointLoop (either (const previous) (const (Just key)) result)
        finishSession = do
          current <- readMVar state
          resumable <- readTVarIO preserveCheckpoint
          if stopped current && not resumable then forgetSession session
            else void (checkpointNow (desktop current))
    initialCheckpoint <- checkpointNow recovered
    initialKey <- either (const (pure Nothing)) (const (Just <$> checkpointKey recovered)) initialCheckpoint
    args <- getArgs
    oldRecord <- loadSession session
    fresh <- newSessionRecord Nothing (withoutDaemon args)
    let record=fromMaybe fresh oldRecord
    withEndpointListener path $ \socket authenticate -> flip finally finishSession $ do
      rememberSession record {sessionId=session,sessionHost=Nothing}
      let acceptLoop = forever $ do
            (sock,_) <- N.accept socket
            (connection,shutdown) <- socketToEndpoint sock
            void (forkIO (finally
              ((authenticate connection >> serve shutdown connection) `catch` \(_::IOException) -> pure ())
              (quietClose connection)))
          drainInspections=do
            let empty=atomically (readTVar inspections >>= check . M.null)
            -- Exit may have been approved through another connection. Wait for
            -- actual reply completion, then cancel unapproved/unresponsive calls.
            settled<-timeout 2000000 empty
            case settled of
              Just () -> pure ()
              Nothing -> do
                pendingStops<-M.elems <$> readTVarIO inspections
                cancelled<-timeout 2000000 (sequence_ pendingStops >> empty)
                when (cancelled==Nothing) (hPutStrLn stderr "Editor closed with an unfinished MCP reply.")
      withAsync (checkpointLoop initialKey) $ \_ -> withAsync commandLoop $ \inputs -> withAsync linkLoop $ \links -> withAsync tickLoop $ \ticks -> withAsync acceptLoop $ \accepts ->
        -- Windows accept is a blocking foreign call: close its socket before
        -- withAsync waits for cancellation, rather than in the outer bracket.
        race_ (readMVar done >> drainInspections) (race_ (wait inputs) (race_ (wait ticks) (race_ (wait links) (wait accepts)))) `finally` N.close socket

  withoutDaemon args@("--":_)=args
  withoutDaemon ("--remote-daemon":_:rest)=withoutDaemon rest
  withoutDaemon (arg:rest)=arg:withoutDaemon rest
  withoutDaemon []=[]

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
  let (sessionOptions,remoteArgs)=extractSession args
  session <- case sessionOptions of
    [] -> randomIdentity
    [value] | validIdentity value -> pure value
    _ -> failure "Specify one --remote-session followed by a 48-character session ID"
  hPutStrLn stderr ("Remote session: "++session++" (resume with --remote-session "++session++")")
  withSSHSession host session (not (null sessionOptions)) remoteArgs action
  where
    extractSession []=([],[])
    extractSession allArgs@("--":_)=([],allArgs)
    extractSession ("--remote-session":value:rest)=let (ids,remaining)=extractSession rest in (value:ids,remaining)
    extractSession (value:rest)=let (ids,remaining)=extractSession rest in (ids,value:remaining)

withSSHSession :: String -> String -> Bool -> [String] -> (RemotePeer -> IO ()) -> IO ()
withSSHSession host = withSessionPeer (Just host)

withLocalPeer :: String -> Bool -> [String] -> (RemotePeer -> IO ()) -> IO ()
withLocalPeer = withSessionPeer Nothing

withSessionPeer :: Maybe String -> String -> Bool -> [String] -> (RemotePeer -> IO ()) -> IO ()
withSessionPeer host session resume remoteArgs action = do
  unless (validIdentity session && all (all (/='\0')) remoteArgs) (failure "Invalid session identity or argument")
  forM_ host $ \name -> unless ((case name of [] -> False; '-':_ -> False; _ -> True) && all (\c -> c>' ' && not (isSpace c)) name) (failure "Invalid SSH host")
  record <- loadSession session >>= maybe (do
    fresh <- newSessionRecord host remoteArgs
    pure fresh {sessionId=session}) pure
  client <- randomIdentity
  journal <- newTVarIO (Journal 1 0 [] M.empty Nothing Nothing Nothing)
  incoming <- newTBQueueIO 8
  -- The callback borrows the Handle's socket descriptor. Clear it under this
  -- lock before closing handles, so it can never act on a reused descriptor.
  activeShutdown <- newMVar (pure ())
  let shutdown = withMVar activeShutdown id
      emit packet=atomically (writeTBQueue incoming (Right (Just packet)))
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
        pure (json "hello" ["version" .= protocolVersion,"session" .= session,"client" .= client,"ack" .= lastAck j,"args" .= remoteArgs,"resume" .= (serverEpoch j/=Nothing || resume)])
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
        greeting <- hello
        let open = do
              connection <- case host of
                Nothing -> do
                  parsed <- case greeting of JsonPacket value -> decodeValue helloParser value; _ -> failure "Invalid local hello"
                  (connectionHandle,stop) <- openSession remoteArgs parsed `catch` \(err::IOException) -> fatal (show err)
                  pure (Just connectionHandle,Just connectionHandle,Nothing,Nothing,stop)
                Just name -> do
                  -- Dynamic paths travel in the framed hello, never through a login shell.
                  let command="hide --remote"
                      sshArgs=["-T","-a","-x","-oForwardAgent=no","-oClearAllForwardings=yes","-oRequestTTY=no","-oServerAliveInterval=15","-oServerAliveCountMax=3","-oConnectTimeout=10","--",name,command]
                  (input,output,errors,process) <- createProcess (proc "ssh" sshArgs) {std_in=CreatePipe,std_out=CreatePipe,std_err=Inherit,close_fds=True}
                  pure (input,output,errors,Just process,pure ())
              let (_,_,_,_,stop)=connection
              modifyMVar_ activeShutdown (const (pure stop))
              pure connection
            close connection = do
              modifyMVar_ activeShutdown (const (pure (pure ())))
              cleanup connection
        bracket open close $ \(inputPipe,outputPipe,_,process,_) -> do
          input <- maybe (failure "Transport did not create its input pipe") pure inputPipe
          output <- maybe (failure "Transport did not create its output pipe") pure outputPipe
          hSetBinaryMode input True; hSetBinaryMode output True; hSetBuffering input NoBuffering
          responseResult <- try (writePacket input greeting >> timeout 20000000 (readPacket output))
          let response=either (const Nothing) id (responseResult :: Either IOException (Maybe (Maybe WirePacket)))
          value <- case response of
            Just (Just (JsonPacket v)) -> pure v
            _ -> do
              exited <- maybe (pure Nothing) (timeout 1000000 . waitForProcess) process
              j <- readTVarIO journal
              case exited of
                Just (ExitFailure 127) -> fatal "hide is not installed or not on PATH on the remote host"
                Just code | serverEpoch j==Nothing -> fatal ("SSH remote startup failed ("++show code++"); check authentication and the remote hide installation")
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
          rememberSession record {sessionHost=host}
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
                  Just "closed" -> do
                    resumable <- case packet of
                      JsonPacket v -> decodeValue (withObject "closed" (\o -> o .:? "resumable" .!= False)) v
                      _ -> pure False
                    unless resumable (forgetSession session)
                    emit packet
                  Just "error" -> case packet of
                    JsonPacket v -> decodeValue (withObject "error" (\o -> o .: "message")) v >>= fatal
                    _ -> failure "Remote protocol error"
                  _ -> emit packet
                if packetType packet==Just "closed" then atomically (writeTBQueue incoming (Right Nothing)) else receiver
          raceWithShutdown shutdown (sender ack) receiver
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
      drain = do
        void $ timeout 2000000 $ atomically $ do
          j <- readTVar journal
          check (null (pending j) || terminalError j/=Nothing)
        remaining <- pending <$> readTVarIO journal
        unless (null remaining) $ hPutStrLn stderr
          ("Detached with "++show (length remaining)++" unacknowledged input events; their application could not be confirmed.")
  mask $ \restore -> withAsync (restore worker) $ \_ ->
    restore (action peer) `finally` (drain `finally` shutdown)
  where
    cleanup (input,output,_,process,_) = do
      mapM_ quietClose input; mapM_ quietClose output
      forM_ process $ \child -> do
        terminateProcess child `catch` \(_::IOException) -> pure ()
        void (waitForProcess child)
#else
withLocalPeer :: String -> Bool -> [String] -> (RemotePeer -> IO ()) -> IO ()
withLocalPeer _ _ _ _ = ioError (userError "Persistent sessions are not built")
withSSHSession :: String -> String -> Bool -> [String] -> (RemotePeer -> IO ()) -> IO ()
withSSHSession _ _ _ _ _ = ioError (userError "Persistent sessions are not built")
withSSHPeer :: String -> [String] -> (RemotePeer -> IO ()) -> IO ()
withSSHPeer _ _ _ = ioError (userError "Remote support is not built. Rebuild with cabal build -fremote")
runRemoteRelay :: [String] -> IO ()
runRemoteRelay _ = ioError (userError "Remote support is not built. Rebuild with cabal build -fremote")
runRemoteDaemonWithStartup :: IO () -> String -> Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> (Desktop -> Maybe T.Text -> Value -> IO (Bool, Desktop, IO (Maybe Value))) -> Desktop -> IO ()
runRemoteDaemonWithStartup _ _ _ _ _ _ _ = ioError (userError "Remote support is not built. Rebuild with cabal build -fremote")
#endif

runRemoteDaemon :: String -> Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> (Desktop -> Maybe T.Text -> Value -> IO (Bool, Desktop, IO (Maybe Value))) -> Desktop -> IO ()
runRemoteDaemon=runRemoteDaemonWithStartup (pure ())
