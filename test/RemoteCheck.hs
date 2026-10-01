{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
module RemoteCheck (checks) where
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait, link)
import Control.Exception hiding (assert)
import Control.Monad (unless, void, replicateM, replicateM_)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Text as T
import System.IO
#ifndef mingw32_HOST_OS
import System.Directory (getTemporaryDirectory, createDirectory, removeFile, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)
import Data.List (isInfixOf)
#endif
import System.Timeout (timeout)
import THC.Edit.Buffer (newBuffer, markSaved)
import qualified Data.Map.Strict as M
import THC.Edit.Model
import THC.Edit.Protocol
import THC.Edit.Remote
import THC.Edit.RemoteEndpoint
import qualified THC.Edit.Session as S
checks :: IO ()
checks = do
  localPeerCheck
  sshFailureCheck
  let assert label ok=unless ok (error label)
  session <- randomIdentity
  path <- sessionEndpoint session
  let initial=addDocument Nothing (newBuffer "") (initialDesktop (80,25))
      client=replicate 48 'b'
  observed <- newIORef initial
  ticks <- newIORef (0::Int)
  replayed <- newIORef []
  let tick d=writeIORef observed d >> modifyIORef' ticks (+1) >> pure d
      effects d requests=pure (Exit `elem` requests,d {buffers=M.map (\doc -> doc {documentBuffer=markSaved (documentBuffer doc)}) (buffers d)})
      open = connectEndpoint path
      awaitOpen attempts=open `catch` \(err::IOException) -> if attempts<=0 then throwIO err else threadDelay 50000 >> awaitOpen (attempts-1)
      receive h=timeout 3000000 (readPacket h) >>= maybe (error "Remote test packet timeout") pure
      hello h who watermark=writePacket h (JsonPacket (object ["type" .= ("hello"::T.Text),"version" .= (1::Int),"session" .= session,"client" .= who,"ack" .= (watermark::Int)]))
      control h expected=do
        packet <- receive h
        case packet of
          Just (JsonPacket (Object o)) | KM.lookup "type" o==Just (String expected) -> pure o
          Just _ -> control h expected
          Nothing -> error ("Remote closed before "++T.unpack expected)
      attach h who watermark=do
        hello h who watermark
        greeting <- control h "hello"
        void (control h "assets")
        let count=maybe 0 id (parseMaybe (\o -> o .:? "replay" .!= 0) greeting)
        replies <- replicateM count (receive h)
        writeIORef replayed replies
        frame <- receive h
        assert "reattach starts with a reset frame" (case frame of Just (BinaryPacket bytes) -> not (BS.null bytes) && BS.head bytes==0; _ -> False)
        pure greeting
      input h serial=writePacket h (JsonPacket (object ["type" .= ("paste"::T.Text),"seq" .= (serial::Int),"text" .= ("λ"::T.Text)]))
      ack h serial=do
        value <- control h "ack"
        assert "remote acknowledgement tracks committed sequence" (KM.lookup "seq" value==Just (toJSON (serial::Int)))
  withAsync (runRemoteDaemon session 1 effects tick initial) $ \daemon -> do
    link daemon
    first <- awaitOpen (100::Int)
    writePacket first (JsonPacket (object ["type" .= ("hello"::T.Text),"version" .= (999::Int),"session" .= session,"client" .= client]))
    void (control first "error")
    hClose first
    connected <- open
    greeting <- attach connected client 0
    bracket open hClose $ \inspector -> do
      writePacket inspector (JsonPacket (object ["type" .= ("inspect"::T.Text),"request" .= object ["jsonrpc" .= ("2.0"::T.Text),"id" .= (41::Int),"method" .= ("initialize"::T.Text),"params" .= object ["protocolVersion" .= ("2025-11-25"::T.Text)]]]))
      inspected <- receive inspector
      assert "inspection works while an editor owns the writer slot" (case inspected of
        Just (JsonPacket (Object fields)) -> KM.lookup "id" fields==Just (toJSON (41::Int)) && KM.member "result" fields
        _ -> False)
    input connected 1
    ack connected 1
    bracket open hClose $ \other -> do
      hello other (replicate 48 'c') 0
      void (control other "error")
    hClose connected
    before <- readIORef ticks
    threadDelay 180000
    after <- readIORef ticks
    assert "remote tooling ticks continue while detached" (after>before)
    bracket open hClose $ \second -> do
      resumed <- attach second client 0
      assert "lost ACK reconnect retains committed sequence" (KM.lookup "ack" resumed==Just (toJSON (1::Int)))
      assert "same client retains server epoch" (KM.lookup "epoch" resumed==KM.lookup "epoch" greeting)
      input second 1
      ack second 1
      threadDelay 100000
      d <- readIORef observed
      assert "duplicate input is not applied twice" (activeText d=="λ")
      writePacket second (JsonPacket (object ["type" .= ("command"::T.Text),"seq" .= (2::Int),"command" .= ("selectAll"::T.Text)]))
      ack second 2
      writePacket second (JsonPacket (object ["type" .= ("command"::T.Text),"seq" .= (3::Int),"command" .= ("cut"::T.Text)]))
      -- Lose the reply after commitment, not the input before acceptance. An
      -- immediate full-duplex close may discard the write on some platforms.
      let cutCommitted = do
            current <- readIORef observed
            unless (T.null (activeText current)) (threadDelay 1000 >> cutCommitted)
      committed <- timeout 3000000 cutCommitted
      assert "Cut commits before its unread reply is dropped" (committed==Just ())
    threadDelay 100000
    bracket open hClose $ \recovered -> do
      resumed <- attach recovered client 0
      assert "unacknowledged Cut remains committed" (KM.lookup "ack" resumed==Just (toJSON (3::Int)))
      replies <- readIORef replayed
      assert "lost Cut clipboard reply is replayed" (any (\p -> case p of Just (JsonPacket (Object fields)) -> KM.lookup "type" fields==Just (String "copy") && KM.lookup "text" fields==Just (String "λ"); _ -> False) replies)
      threadDelay 100000
      d <- readIORef observed
      assert "Cut changes the buffer exactly once" (T.null (activeText d))
    threadDelay 100000
    bracket open hClose $ \third -> do
      replaced <- attach third (replicate 48 'c') 0
      assert "new launcher resumes with independent sequences" (KM.lookup "ack" replaced==Just (toJSON (0::Int)))
      assert "changing writers invalidates old reconnect journals" (KM.lookup "epoch" replaced/=KM.lookup "epoch" greeting)
      writePacket third (JsonPacket (object ["type" .= ("key"::T.Text),"seq" .= (1::Int),"key" .= ("F3"::T.Text)]))
      ack third 1
      writePacket third (JsonPacket (object ["type" .= ("key"::T.Text),"seq" .= (2::Int),"key" .= ("x"::T.Text),"mods" .= ["alt"::T.Text]]))
      ack third 2
      void (control third "closed")
    timeout 3000000 (wait daemon) >>= maybe (error "Explicit remote Exit did not stop daemon") pure
  putStrLn "remote transport checks passed"

-- Exercise the installed-ssh subprocess boundary without a network host.
sshFailureCheck :: IO ()
#ifdef mingw32_HOST_OS
sshFailureCheck = pure ()
#else
sshFailureCheck = do
  temporary <- getTemporaryDirectory
  bracket (do
    (name,h) <- openTempFile temporary "thc-ssh-test"
    hClose h; removeFile name; createDirectory name
    pure name) removePathForcibly $ \directory -> do
      let executable=directory </> "ssh"
          counter=directory </> "attempts"
          command=directory </> "command"
      writeFile executable ("#!/bin/sh\nfor argument do last=$argument; done\nprintf '%s' \"$last\" > \""++command++"\"\nprintf 'attempt\n' >> \""++counter++"\"\nexit 127\n")
      setFileMode executable 0o700
      oldPath <- lookupEnv "PATH"
      bracket_ (setEnv "PATH" directory) (maybe (unsetEnv "PATH") (setEnv "PATH") oldPath) $ do
        result <- timeout 3000000 (try (withSSHPeer "example.invalid" ["--","some'path; $(false)"] $ \peer ->
          let receive=peerReceive peer >>= maybe (pure ()) (const receive) in receive) :: IO (Either IOException ()))
        unless (case result of Just (Left err) -> "thc-edit is not installed" `isInfixOf` show err; _ -> False)
          (error "Missing remote thc-edit must fail clearly without retrying")
      launched <- readFile command
      unless (launched=="thc-edit --remote") (error "SSH must not interpolate remote paths into the login shell command")
      attempts <- readFile counter
      unless (lines attempts==["attempt"]) (error "Missing remote executable retried SSH")
      writeFile executable "#!/bin/sh\nexec /bin/sleep 30\n"
      bracket_ (setEnv "PATH" directory) (maybe (unsetEnv "PATH") (setEnv "PATH") oldPath) $
        withSSHPeer "example.invalid" [] $ \peer -> do
          let upload=JsonPacket (object ["type" .= ("upload"::T.Text),"name" .= ("sample.txt"::T.Text)])
              key=JsonPacket (object ["type" .= ("paste"::T.Text),"text" .= (""::T.Text)])
          incomplete <- try (peerSend peer upload) :: IO (Either IOException ())
          unless (case incomplete of Left _ -> True; _ -> False) (error "Incomplete upload must be rejected atomically")
          peerSendBatch peer [upload,BinaryPacket "abc"]
          replicateM_ 127 (peerSend peer key)
          blocked <- timeout 50000 (peerSend peer key)
          unless (blocked==Nothing) (error "Full input journal must apply backpressure without dropping events")


#endif

-- A local frontend uses the same journal and keeps its desktop when detached.
localPeerCheck :: IO ()
localPeerCheck = do
  let assert label ok=unless ok (error label)
  record <- S.newSessionRecord Nothing ["--demo"]
  offline <- S.newSessionRecord (Just "offline-test-host") ["--","project λ"]
  let session=S.sessionId record
  path <- sessionEndpoint session
  let initial=addDocument Nothing (newBuffer "") (initialDesktop (80,25))
  observed <- newIORef initial
  let tick d=writeIORef observed d >> pure d
      effects d requests=pure (Exit `elem` requests,d {buffers=M.map (\doc -> doc {documentBuffer=markSaved (documentBuffer doc)}) (buffers d)})
      awaitReady attempts=bracket (connectEndpoint path) hClose (const (pure ())) `catch` \(err::IOException) ->
        if attempts<=0 then throwIO err else threadDelay 50000 >> awaitReady (attempts-1)
      receive peer expected=do
        packet <- timeout 3000000 (peerReceive peer) >>= maybe (error ("Local peer timeout waiting for "++T.unpack expected)) pure
        case packet of
          Just (JsonPacket (Object fields)) | KM.lookup "type" fields==Just (String expected) -> pure fields
          Just _ -> receive peer expected
          Nothing -> error ("Local peer closed before "++T.unpack expected)
      send peer fields=peerSend peer (JsonPacket (object fields))
  flip finally (S.forgetSession session >> S.forgetSession (S.sessionId offline)) $ do
    S.rememberSession record
    S.rememberSession offline
    records <- S.listSessions
    assert "offline remote sessions remain discoverable" (offline `elem` records)
    assert "missing local endpoints are omitted" (not (record `elem` records))
    loaded <- S.loadSession (S.sessionId offline)
    assert "catalog round-trips Unicode arguments" (loaded==Just offline)
    withAsync (runRemoteDaemon session 1 effects tick initial) $ \daemon -> do
      link daemon
      awaitReady (100::Int)
      withLocalPeer session True [] $ \peer -> do
        void (receive peer "assets")
        records' <- S.listSessions
        assert "live local session listed while writer attached" (any ((==session).S.sessionId) records')
        send peer ["type" .= ("frontend"::T.Text),"mode" .= (Nothing::Maybe Int)]
        send peer ["type" .= ("paste"::T.Text),"text" .= ("persistent λ"::T.Text),"seq" .= (1::Int)]
      threadDelay 150000
      d <- readIORef observed
      assert "local peer detach retains unsaved desktop" (activeText d=="persistent λ")
      exists <- S.loadSession session
      assert "detach retains session catalog" (maybe False (const True) exists)
      withLocalPeer session True [] $ \peer -> do
        void (receive peer "assets")
        send peer ["type" .= ("command"::T.Text),"command" .= ("quit"::T.Text),"seq" .= (1::Int)]
        void (receive peer "ack")
        send peer ["type" .= ("key"::T.Text),"key" .= ("Tab"::T.Text),"seq" .= (2::Int)]
        void (receive peer "ack")
        send peer ["type" .= ("key"::T.Text),"key" .= ("Enter"::T.Text),"seq" .= (3::Int)]
        void (receive peer "closed")
      ended <- timeout 3000000 (wait daemon)
      assert "explicit Exit ends local daemon" (ended==Just ())
      exists' <- S.loadSession session
      assert "explicit Exit removes session catalog" (exists'==Nothing)
