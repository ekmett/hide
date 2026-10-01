{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
module RemoteCheck (checks) where
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Async (withAsync, wait, link)
import Control.Exception hiding (assert)
import Control.Monad (unless, void, replicateM, replicateM_)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Network.Socket as N
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
import THC.Edit.EditorMCP (editorResponse, runEditorMCPWithHandles, readMCPLine)
import THC.Edit.Model
import THC.Edit.Protocol
import THC.Edit.Remote
import THC.Edit.RemoteEndpoint
import qualified THC.Edit.Session as S
checks :: IO ()
checks = do
  localPeerCheck
  inspectionExitCheck
  inspectionViewerExitCheck
  promptedExitCheck
  sshFailureCheck
  let assert label ok=unless ok (error label)
  session <- randomIdentity
  path <- sessionEndpoint session
  let initial=addDocument Nothing (newBuffer "") (initialDesktop (80,25))
      client=replicate 48 'b'
  observed <- newIORef initial
  ticks <- newIORef (0::Int)
  replayed <- newIORef []
  inspectionStarted<-newEmptyMVar
  inspectionRelease<-newEmptyMVar
  inspectionFinished<-newEmptyMVar
  let tick d=writeIORef observed d >> modifyIORef' ticks (+1) >> pure d
      effects d requests=pure (Exit `elem` requests,d {buffers=M.map (\doc -> doc {documentBuffer=markSaved (documentBuffer doc)}) (buffers d)})
      deferred d=do
        putMVar inspectionStarted ()
        pure (False,d,(takeMVar inspectionRelease >> pure (Just (String "done"))) `finally` void (tryPutMVar inspectionFinished ()))
      inspectLive d (String "deferred")=deferred d
      inspectLive d (String "clipboard-export")=pure (False,d {clipboardExport=(fst (clipboardExport d)+1,Just "synthetic fixture λ")},pure (Just (String "queued")))
      inspectLive d (String "clipboard-status")=pure (False,d,pure (Just (toJSON (clipboardExport d))))
      inspectLive d (Object fields) | KM.lookup "method" fields==Just (String "test/deferred")=deferred d
      inspectLive d request=inspect d request
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
  withAsync (runRemoteDaemon session 1 effects tick inspectLive initial) $ \daemon -> do
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
    bracket open hClose $ \clipboardInspector -> do
      writePacket clipboardInspector (JsonPacket (object ["type" .= ("inspect"::T.Text),"request" .= ("clipboard-export"::T.Text)]))
      reply<-receive clipboardInspector
      assert "explicit clipboard output is queued" (case reply of Just (JsonPacket (String "queued")) -> True; _ -> False)
    copied<-control connected "copy"
    assert "explicit clipboard output reaches attached frontend" (KM.lookup "text" copied==Just (String "synthetic fixture λ"))
    let awaitClipboardClear=do
          cleared<-bracket open hClose $ \clipboardInspector -> do
            writePacket clipboardInspector (JsonPacket (object ["type" .= ("inspect"::T.Text),"request" .= ("clipboard-status"::T.Text)]))
            reply<-receive clipboardInspector
            pure (case reply of Just (JsonPacket value) -> value==toJSON ((1::Int),Nothing::Maybe T.Text); _ -> False)
          unless cleared (threadDelay 1000 >> awaitClipboardClear)
    cleared<-timeout 3000000 awaitClipboardClear
    assert "successful clipboard output clears its queued export" (cleared==Just ())
    inspector<-open
    writePacket inspector (JsonPacket (object ["type" .= ("inspect"::T.Text),"request" .= ("deferred"::T.Text)]))
    began<-timeout 3000000 (takeMVar inspectionStarted)
    assert "deferred tool starts" (began==Just ())
    ticksBefore<-readIORef ticks
    threadDelay 150000
    ticksAfter<-readIORef ticks
    assert "deferred MCP wait never holds desktop lock" (ticksAfter>ticksBefore)
    hClose inspector
    cancelled<-timeout 3000000 (takeMVar inspectionFinished)
    assert "closing MCP connection cancels deferred tool" (cancelled==Just ())
    -- Keep bridge input open across cancellation and a subsequent request.
    -- On Windows both socket readers require shutdown before cancellation.
    withBridgeHandles $ \bridgeHandle shutdownBridge clientHandle ->
      withAsync (runEditorMCPWithHandles session bridgeHandle bridgeHandle) $ \bridge ->
        flip finally shutdownBridge $ do
          let send value=BL.hPut clientHandle (encode value<>"\n") >> hFlush clientHandle
              rpc number method=object ["jsonrpc" .= ("2.0"::T.Text),"id" .= (number::Int),"method" .= (method::T.Text)]
              started=timeout 3000000 (takeMVar inspectionStarted) >>= assert "bridge deferred call starts" . (==Just ())
              finished=timeout 3000000 (takeMVar inspectionFinished) >>= assert "bridge cancellation reaches deferred call" . (==Just ())
          send (rpc 101 "test/deferred")
          started
          send (object ["jsonrpc" .= ("2.0"::T.Text),"method" .= ("notifications/cancelled"::T.Text),"params" .= object ["requestId" .= (101::Int)]])
          finished
          send (rpc 102 "ping")
          response<-timeout 3000000 (readMCPLine clientHandle BS.empty)
          assert "bridge responds after cancelling a request" (case response of
            Just (Just (line,_)) -> case eitherDecodeStrict' line of Right (Object fields) -> KM.lookup "id" fields==Just (toJSON (102::Int)); _ -> False
            _ -> False)
          send (rpc 103 "test/deferred")
          started
          hClose clientHandle
          finished
          exited<-timeout 3000000 (wait bridge)
          assert "bridge EOF cancels and joins pending calls" (exited==Just ())
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
    withAsync (runRemoteDaemon session 1 effects tick inspect initial) $ \daemon -> do
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

inspect :: Desktop -> Value -> IO (Bool, Desktop, IO (Maybe Value))
inspect d request=pure (False,d,pure (editorResponse d request))

-- Use a fresh loopback pair so the bridge regression runs on native Windows
-- without POSIX pipes or replacing the test process's standard handles.
withBridgeHandles :: (Handle -> IO () -> Handle -> IO a) -> IO a
withBridgeHandles action=bracket (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $ \listener -> do
  N.bind listener (N.SockAddrInet 0 (N.tupleToHostAddress (127,0,0,1)))
  N.listen listener 1
  address<-N.getSocketName listener
  bracketOnError (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $ \client -> do
    N.connect client address
    (server,_)<-N.accept listener
    (clientHandle,_)<-socketToEndpoint client
    (serverHandle,shutdown)<-socketToEndpoint server
    action serverHandle shutdown clientHandle `finally` do
      shutdown
      hClose serverHandle
      hClose clientHandle

inspectionExitCheck :: IO ()
inspectionExitCheck=do
  session<-randomIdentity
  path<-sessionEndpoint session
  let initial=initialDesktop (80,25)
      effects d _=pure (False,d)
      inspectExit d _=pure (True,d,pure (Just (String "exiting")))
      open attempts=connectEndpoint path `catch` \(err::IOException) ->
        if attempts<=0 then throwIO err else threadDelay 10000 >> open (attempts-1)
  withAsync (runRemoteDaemon session 1 effects pure inspectExit initial) $ \daemon -> do
    link daemon
    bracket (open (100::Int)) hClose $ \connection -> do
      writePacket connection (JsonPacket (object ["type" .= ("inspect"::T.Text),"request" .= Null]))
      reply<-timeout 3000000 (readPacket connection)
      unless (case reply of Just (Just (JsonPacket (String "exiting"))) -> True; _ -> False) (error "MCP Exit must deliver its reply before ending the daemon")
    ended<-timeout 3000000 (wait daemon)
    unless (ended==Just ()) (error "MCP Exit must end daemon")
    retained<-S.loadSession session
    unless (retained==Nothing) (error "MCP Exit must remove session catalog")

-- The approval connection can request Exit before the inspecting thread has
-- resumed its continuation. The daemon must join the accepted reply, not just
-- the connection that delivered approval.
promptedExitCheck :: IO ()
promptedExitCheck=do
  session<-randomIdentity
  path<-sessionEndpoint session
  started<-newEmptyMVar
  approved<-newEmptyMVar
  ready<-newEmptyMVar
  releaseReply<-newEmptyMVar
  pendingStarted<-newEmptyMVar
  pendingCancelled<-newEmptyMVar
  never<-newEmptyMVar
  let initial=initialDesktop (80,25)
      effects d requests=do
        whenExit requests (void (tryPutMVar approved ()))
        pure (Exit `elem` requests,d)
      whenExit requests action=if Exit `elem` requests then action else pure ()
      inspectPrompt d (String "exit-after-approval")=do
        putMVar started ()
        pure (False,d,readMVar approved >> putMVar ready () >> takeMVar releaseReply >> pure (Just (String "approved-exit")))
      inspectPrompt d (String "unapproved")=do
        putMVar pendingStarted ()
        pure (False,d,(takeMVar never >> pure Nothing) `finally` putMVar pendingCancelled ())
      inspectPrompt d value=inspect d value
      open attempts=connectEndpoint path `catch` \(err::IOException) ->
        if attempts<=0 then throwIO err else threadDelay 10000 >> open (attempts-1)
      receive h=timeout 3000000 (readPacket h) >>= maybe (error "Prompted Exit packet timed out") pure
      control h wanted=do
        packet<-receive h
        case packet of
          Just (JsonPacket (Object fields)) | KM.lookup "type" fields==Just (String wanted) -> pure ()
          Just _ -> control h wanted
          Nothing -> error "Prompted Exit connection ended early"
      await label barrier=timeout 3000000 (takeMVar barrier) >>= \result -> unless (result==Just ()) (error label)
  withAsync (runRemoteDaemon session 1 effects pure inspectPrompt initial) $ \daemon -> do
    link daemon
    bracket (open (100::Int)) hClose $ \inspector -> bracket (open (0::Int)) hClose $ \pending -> bracket (open (0::Int)) hClose $ \display -> do
      writePacket inspector (JsonPacket (object ["type" .= ("inspect"::T.Text),"request" .= ("exit-after-approval"::T.Text)]))
      await "Prompted Exit inspection did not start" started
      writePacket pending (JsonPacket (object ["type" .= ("inspect"::T.Text),"request" .= ("unapproved"::T.Text)]))
      await "Unapproved inspection did not start" pendingStarted
      writePacket display (JsonPacket (object ["type" .= ("hello"::T.Text),"version" .= (1::Int),"session" .= session,"client" .= replicate 48 'e',"ack" .= (0::Int)]))
      control display "hello"
      control display "assets"
      writePacket display (JsonPacket (object ["type" .= ("command"::T.Text),"seq" .= (1::Int),"command" .= ("quit"::T.Text)]))
      writePacket display (JsonPacket (object ["type" .= ("paste"::T.Text),"seq" .= (2::Int),"text" .= ("must not reopen session"::T.Text)]))
      control display "closed"
      bracket (open (0::Int)) hClose $ \late -> do
        writePacket late (JsonPacket (object ["type" .= ("inspect"::T.Text),"request" .= ("late request"::T.Text)]))
        denied<-receive late
        unless (case denied of Just (JsonPacket (Object fields)) -> KM.lookup "type" fields==Just (String "error"); _ -> False) (error "Closing session admitted a new inspection")
      await "Approved Exit continuation never became ready" ready
      premature<-timeout 50000 (wait daemon)
      unless (premature==Nothing) (error "Daemon exited before approved inspection reply was released")
      putMVar releaseReply ()
      reply<-receive inspector
      unless (case reply of Just (JsonPacket (String "approved-exit")) -> True; _ -> False) (error "Approved Exit response lost")
      await "Daemon did not cancel unapproved inspection" pendingCancelled
    ended<-timeout 3000000 (wait daemon)
    unless (ended==Just ()) (error "Prompted Exit daemon did not finish after draining replies")
    retained<-S.loadSession session
    unless (retained==Nothing) (error "Prompted Exit left session catalog")

inspectionViewerExitCheck :: IO ()
inspectionViewerExitCheck=do
  session<-randomIdentity
  path<-sessionEndpoint session
  committed<-newEmptyMVar
  let initial=addDocument Nothing (newBuffer "") (initialDesktop (80,25))
      effects d _=pure (False,d)
      tick d=do
        if activeText d=="accepted input" then void (tryPutMVar committed ()) else pure ()
        pure d
      inspectExit d _=pure (True,d,pure (Just (String "exiting")))
      open attempts=connectEndpoint path `catch` \(err::IOException) ->
        if attempts<=0 then throwIO err else threadDelay 10000 >> open (attempts-1)
      receive h=timeout 3000000 (readPacket h) >>= maybe (error "Inspect Exit viewer response timed out") pure
      control h wanted=do
        packet<-receive h
        case packet of
          Just (JsonPacket (Object fields)) | KM.lookup "type" fields==Just (String wanted) -> pure ()
          Just _ -> control h wanted
          Nothing -> error "Viewer received EOF instead of a close notification"
      closed h acknowledged=do
        packet<-receive h
        case packet of
          Just (JsonPacket (Object fields)) | KM.lookup "type" fields==Just (String "ack") -> closed h (acknowledged || KM.lookup "seq" fields==Just (toJSON (1::Int)))
          Just (JsonPacket (Object fields)) | KM.lookup "type" fields==Just (String "closed") -> unless acknowledged (error "MCP Exit lost the viewer's accepted input acknowledgement")
          Just _ -> closed h acknowledged
          Nothing -> error "MCP Exit looked like a reconnectable EOF to its viewer"
  withAsync (runRemoteDaemon session 1 effects tick inspectExit initial) $ \daemon -> do
    link daemon
    bracket (open (100::Int)) hClose $ \display -> bracket (open (0::Int)) hClose $ \inspector -> do
      writePacket display (JsonPacket (object ["type" .= ("hello"::T.Text),"version" .= (1::Int),"session" .= session,"client" .= replicate 48 'f',"ack" .= (0::Int)]))
      control display "hello"
      control display "assets"
      writePacket display (JsonPacket (object ["type" .= ("paste"::T.Text),"seq" .= (1::Int),"text" .= ("accepted input"::T.Text)]))
      accepted<-timeout 3000000 (takeMVar committed)
      unless (accepted==Just ()) (error "Viewer input did not commit before MCP Exit")
      writePacket inspector (JsonPacket (object ["type" .= ("inspect"::T.Text),"request" .= Null]))
      reply<-receive inspector
      unless (case reply of Just (JsonPacket (String "exiting")) -> True; _ -> False) (error "Attached MCP Exit lost its response")
      closed display False
    ended<-timeout 3000000 (wait daemon)
    unless (ended==Just ()) (error "MCP Exit with attached viewer did not finish")
