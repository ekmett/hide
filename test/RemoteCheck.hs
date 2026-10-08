{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
module RemoteCheck (checks) where
import Control.Concurrent.STM (retry)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Async (withAsync, wait, link)
import Control.Exception hiding (assert)
import Control.Monad (unless, void, replicateM, replicateM_, forM_)
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
import Hide.Buffer (Selection(..), newBuffer, markSaved)
import qualified Data.Map.Strict as M
import Hide.EditorMCP (editorResponse, runEditorMCPWithHandles, runEditorMCPWithToken, readMCPLine)
import Hide.Markdown (renderMarkdown)
import Hide.Model
import Hide.Protocol
import Hide.Remote
import Hide.RemoteEndpoint
import qualified Hide.Session as S
import Hide.Recovery (readCheckpoint)
isolatedStore :: IO a -> IO a
#ifndef mingw32_HOST_OS
isolatedStore action=do
  temp<-getTemporaryDirectory
  old<-lookupEnv "XDG_DATA_HOME"
  bracket (do (path,h)<-openTempFile temp "thc-session-tests"; hClose h; removeFile path; createDirectory path; pure path)
    removePathForcibly $ \path -> bracket_ (setEnv "XDG_DATA_HOME" path)
      (maybe (unsetEnv "XDG_DATA_HOME") (setEnv "XDG_DATA_HOME") old) action
#else
isolatedStore action=action
#endif

checks :: IO ()
checks = isolatedStore $ do
  let identity=replicate 12 'a'++"bcdef"
  unless (S.shortSessionId identity []==replicate 12 'a') (error "title session prefix starts at twelve characters")
  unless (S.shortSessionId identity [identity,replicate 12 'a'++"cdefg"]==replicate 12 'a'++"b") (error "title session prefix disambiguates saved sessions")

  requestedPasteReconnectCheck
  linkOpenCheck
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
      inspectLive d token (Object fields) | KM.lookup "method" fields==Just (String "test/token")=
        pure (False,d,pure (Just (object ["jsonrpc" .= ("2.0"::T.Text),"id" .= KM.lookup "id" fields,"result" .= token])))
      inspectLive d _ (String "deferred")=deferred d
      inspectLive d _ (String "clipboard-export")=pure (False,d {clipboardExport=(fst (clipboardExport d)+1,Just "synthetic fixture λ")},pure (Just (String "queued")))
      inspectLive d _ (String "clipboard-status")=pure (False,d,pure (Just (toJSON (clipboardExport d))))
      inspectLive d _ (Object fields) | KM.lookup "method" fields==Just (String "test/deferred")=deferred d
      inspectLive d token request=inspect d token request
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
        reset<-receive h
        assert "fresh canvas epoch follows all counted reply replay" (case reset of Just (JsonPacket (Object fields))->KM.lookup "type" fields==Just (String "canvas-reset"); _->False)
        marker<-control h "frame-ready"
        assert "reset frame records its exact committed input" (KM.lookup "seq" marker==KM.lookup "ack" greeting && KM.lookup "changed" marker==Just (Bool True))
        frame <- receive h
        assert "reattach starts with a reset frame" (case frame of Just (BinaryPacket bytes) -> not (BS.null bytes) && BS.head bytes==0; _ -> False)
        pure greeting
      input h serial=writePacket h (JsonPacket (object ["type" .= ("paste"::T.Text),"seq" .= (serial::Int),"text" .= ("λ"::T.Text)]))
      ack h serial=do
        value <- control h "ack"
        assert "remote acknowledgement tracks committed sequence" (KM.lookup "seq" value==Just (toJSON (serial::Int)))
  ownership<-newIORef False
  withAsync (runRemoteDaemonWithStartup (writeIORef ownership True) retry session 1 effects tick inspectLive initial) $ \daemon -> do
    link daemon
    first <- awaitOpen (100::Int)
    assert "session ownership hook runs before serving" =<< readIORef ownership
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
    let rpcToken number=object ["jsonrpc" .= ("2.0"::T.Text),"id" .= (number::Int),"method" .= ("test/token"::T.Text),
          "agentToken" .= ("spoofed-top-level"::T.Text),"params" .= object ["agentToken" .= ("spoofed-param"::T.Text)]]
        credential=T.replicate 48 "e"
        inspectToken :: Maybe T.Text -> IO (Maybe WirePacket)
        inspectToken outer=bracket open hClose $ \h -> do
          writePacket h (JsonPacket (object (["type" .= ("inspect"::T.Text),"request" .= rpcToken 51]++["agentToken" .= token | Just token<-[outer]])))
          receive h
    forwarded<-inspectToken (Just credential)
    assert "daemon forwards only the outer authenticated token" (case forwarded of
      Just (JsonPacket (Object fields))->KM.lookup "result" fields==Just (String credential); _->False)
    tokenless<-inspectToken Nothing
    assert "inner RPC fields cannot impersonate an outer agent identity" (case tokenless of
      Just (JsonPacket (Object fields))->KM.lookup "result" fields==Just Null; _->False)
    mapM_ (\invalid -> do
      rejected<-inspectToken (Just invalid)
      assert "daemon rejects empty or oversized bearer values before dispatch" (case rejected of
        Just (JsonPacket (Object fields))->KM.lookup "type" fields==Just (String "error"); _->False)) ["",T.replicate 257 "x"]
    let bridgeTokenCheck credential' expected=withBridgeHandles $ \bridgeHandle shutdownBridge clientHandle ->
          withAsync (maybe (runEditorMCPWithHandles session) (\token->runEditorMCPWithToken session (Just token)) credential' bridgeHandle bridgeHandle) $ \_ ->
            -- Finish the simulated stdio client before joining its input reader.
            flip finally (hClose clientHandle >> shutdownBridge) $ do
              BL.hPut clientHandle (encode (rpcToken 52)<>"\n") >> hFlush clientHandle
              response<-timeout 3000000 (readMCPLine clientHandle BS.empty)
              assert "bridge token binding ignores caller-supplied RPC identities" (case response of
                Just (Just (line,_))->case eitherDecodeStrict' line of Right (Object fields)->KM.lookup "result" fields==Just expected; _->False
                _->False)
    bridgeTokenCheck (Just credential) (String credential)
    bridgeTokenCheck Nothing Null
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
      writePacket second (JsonPacket (object ["type" .= ("command"::T.Text),"seq" .= (2::Int),"command" .= ("hide.edit.select-all"::T.Text)]))
      ack second 2
      writePacket second (JsonPacket (object ["type" .= ("command"::T.Text),"seq" .= (3::Int),"command" .= ("hide.edit.cut"::T.Text)]))
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
        unless (case result of Just (Left err) -> "hide is not installed" `isInfixOf` show err; _ -> False)
          (error "Missing remote hide must fail clearly without retrying")
      launched <- readFile command
      unless (launched=="hide --remote") (error "SSH must not interpolate remote paths into the login shell command")
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
        initialFrame<-receive peer "frame-ready"
        assert "initial frame has no input demand" (KM.lookup "seq" initialFrame==Just (toJSON (0::Int)) && KM.lookup "changed" initialFrame==Just (Bool True))
        send peer ["type" .= ("key"::T.Text),"key" .= ("ArrowLeft"::T.Text),"seq" .= (777::Int)]
        noFrame<-receive peer "frame-ready"
        assert "unchanged input retires its frontend demand without a fake frame" (KM.lookup "seq" noFrame==Just (toJSON (777::Int)) && KM.lookup "changed" noFrame==Just (Bool False))
        records' <- S.listSessions
        assert "live local session listed while writer attached" (any ((==session).S.sessionId) records')
        send peer ["type" .= ("frontend"::T.Text),"mode" .= (Nothing::Maybe Int)]
        send peer ["type" .= ("paste"::T.Text),"text" .= ("persistent λ"::T.Text),"seq" .= (1::Int)]
        -- Burst inputs must retain application and acknowledgement order while
        -- the display independently renders the newest available state.
        forM_ [1..32::Int] $ \i -> send peer ["type" .= ("paste"::T.Text),"text" .= T.pack (show i),"seq" .= (i+1)]
        forM_ [1..33::Int] $ \i -> do
          ack<-receive peer "ack"
          assert "burst acknowledgements retain input order" (KM.lookup "seq" ack==Just (toJSON i))
      threadDelay 150000
      d <- readIORef observed
      assert "local peer detach retains unsaved desktop" (activeText d=="persistent λ"<>T.concat (map (T.pack.show) [1..32::Int]))
      exists <- S.loadSession session
      assert "detach retains session catalog" (maybe False (const True) exists)
      withLocalPeer session True [] $ \peer -> do
        void (receive peer "assets")
        send peer ["type" .= ("command"::T.Text),"command" .= ("hide.app.quit"::T.Text),"seq" .= (1::Int)]
        void (receive peer "ack")
        send peer ["type" .= ("key"::T.Text),"key" .= ("Tab"::T.Text),"seq" .= (2::Int)]
        void (receive peer "ack")
        send peer ["type" .= ("key"::T.Text),"key" .= ("Enter"::T.Text),"seq" .= (3::Int)]
        void (receive peer "closed")
      ended <- timeout 3000000 (wait daemon)
      assert "explicit Exit ends local daemon" (ended==Just ())
      exists' <- S.loadSession session
      assert "explicit Exit removes session catalog" (exists'==Nothing)

inspect :: Desktop -> Maybe T.Text -> Value -> IO (Bool, Desktop, IO (Maybe Value))
inspect d _ request=pure (False,d,pure (editorResponse d request))

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
      inspectExit d _ _=pure (True,d,pure (Just (String "exiting")))
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
      inspectPrompt d _ (String "exit-after-approval")=do
        putMVar started ()
        pure (False,d,readMVar approved >> putMVar ready () >> takeMVar releaseReply >> pure (Just (String "approved-exit")))
      inspectPrompt d _ (String "unapproved")=do
        putMVar pendingStarted ()
        pure (False,d,(takeMVar never >> pure Nothing) `finally` putMVar pendingCancelled ())
      inspectPrompt d token value=inspect d token value
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
      writePacket display (JsonPacket (object ["type" .= ("command"::T.Text),"seq" .= (1::Int),"command" .= ("hide.app.quit"::T.Text)]))
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
      inspectExit d _ _=pure (True,d,pure (Just (String "exiting")))
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

-- A real click travels through the daemon and returns a client-side open once.
-- Retried input/reconnection must not launch the user's browser a second time.
linkOpenCheck :: IO ()
linkOpenCheck=do
  session<-randomIdentity
  path<-sessionEndpoint session
  let initial=addHelpStyled (renderMarkdown 60 "[Website](https://example.com/)") (initialDesktop (80,25))
      window=maybe (error "missing help") id (activeWindow initial)
      x=left (bounds window)+1; y=top (bounds window)+1
      effects d _=pure (False,d)
      awaitReady n=bracket (connectEndpoint path) hClose (const (pure ())) `catch` \(e::IOException)->
        if n<=0 then throwIO e else threadDelay 20000 >> awaitReady (n-1)
      receive h wanted=do
        result<-timeout 3000000 (readPacket h)
        case result of
          Just (Just (JsonPacket (Object fields))) | KM.lookup "type" fields==Just (String wanted)->pure fields
          Just (Just _)->receive h wanted
          _->error ("Link transport timed out waiting for "++T.unpack wanted)
      send h fields=writePacket h (JsonPacket (object fields))
      mouse h serial action=send h ["type" .= ("mouse"::T.Text),"seq" .= (serial::Int),"action" .= (action::T.Text),"x" .= x,"y" .= y,"button" .= (0::Int)]
  flip finally (S.forgetSession session) $
    withAsync (runRemoteDaemon session 1 effects pure inspect initial) $ \daemon->do
      link daemon
      awaitReady (100::Int)
      bracket (connectEndpoint path) hClose $ \h->do
        send h ["type" .= ("hello"::T.Text),"version" .= (1::Int),"session" .= session,"client" .= replicate 48 'e',"ack" .= (0::Int)]
        void (receive h "assets")
        mouse h 1 "down"; void (receive h "ack")
        mouse h 2 "up"
        let openedAndAck opened acknowledged
              | opened && acknowledged=pure ()
              | otherwise=do
                  result<-timeout 3000000 (readPacket h)
                  case result of
                    Just (Just (JsonPacket (Object fields)))
                      | KM.lookup "type" fields==Just (String "open-resource")->do
                          unless (not opened && KM.lookup "url" fields==Just (String "https://example.com/")) (error "Wrong or duplicate client URL")
                          openedAndAck True acknowledged
                      | KM.lookup "type" fields==Just (String "ack")->openedAndAck opened True
                    Just (Just _)->openedAndAck opened acknowledged
                    _->error "Missing asynchronous link response or acknowledgement"
        openedAndAck False False
        mouse h 2 "up"
        let onlyAck=do
              packet<-timeout 3000000 (readPacket h)
              case packet of
                Just (Just (JsonPacket (Object fields)))
                  | KM.lookup "type" fields==Just (String "open-resource")->error "Retried click reopened browser"
                  | KM.lookup "type" fields==Just (String "ack")->pure ()
                Just (Just _)->onlyAck
                _->error "Missing duplicate acknowledgement"
        onlyAck
  putStrLn "Remote link delivery and duplicate-input checks passed"

-- The attachment generation deliberately survives reconnecting the same client.
-- Clipboard receipts must nevertheless retire at that connection boundary.
requestedPasteReconnectCheck :: IO ()
requestedPasteReconnectCheck=do
  session<-randomIdentity
  path<-sessionEndpoint session
  let initial=(addDocument Nothing (newBuffer "source") (initialDesktop (80,25)))
        {dialog=Just (Dialog "Rename" Information [SelectedInput "Name" "old" (Selection 0 3)] 0 ["OK","Cancel"] [])}
      client=replicate 48 'f'
      open=connectEndpoint path
      awaitOpen attempts=open `catch` \(err::IOException)->if attempts<=0 then throwIO err else threadDelay 10000 >> awaitOpen (attempts-1)
      control h expected=do
        packet<-timeout 3000000 (readPacket h) >>= maybe (error "clipboard remote timeout") pure
        case packet of
          Just (JsonPacket (Object fields)) | KM.lookup "type" fields==Just (String expected)->pure fields
          Just _->control h expected
          _->error "clipboard remote connection ended"
      attach h watermark=do
        writePacket h (JsonPacket (object ["type" .= ("hello"::T.Text),"version" .= (1::Int),"session" .= session,"client" .= client,"ack" .= (watermark::Int)]))
        greeting<-control h "hello"
        void (control h "assets")
        pure greeting
      command h serial=writePacket h (JsonPacket (object ["type" .= ("command"::T.Text),"seq" .= (serial::Int),"command" .= ("hide.edit.paste"::T.Text)]))
      reply h serial token text=writePacket h (JsonPacket (object ["type" .= ("paste-reply"::T.Text),"seq" .= (serial::Int),"request" .= token,"text" .= (text::T.Text)]))
      receipt :: Handle -> IO T.Text
      receipt h=control h "paste-request" >>= maybe (error "missing requested paste identity") pure . parseMaybe (.:"request")
      check name ok=unless ok (error name)
  observed<-newIORef initial
  validNextId<-newIORef (nextId initial)
  let tick d=writeIORef observed d >> pure d
      core d _=pure (False,d)
      inspect d _ (String "invalidate-checkpoint")=do
        writeIORef validNextId (nextId d)
        pure (False,d {nextId=0},pure (Just (String "ready")))
      inspect d _ (String "repair-checkpoint")=do
        restored<-readIORef validNextId
        pure (False,d {nextId=restored},pure (Just (String "ready")))
      inspect d _ _=pure (False,d,pure Nothing)
      checkpointState request=bracket open hClose $ \h->do
        writePacket h (JsonPacket (object ["type" .= ("inspect"::T.Text),"request" .= (request::T.Text)]))
        response<-timeout 3000000 (readPacket h)
        check "Checkpoint fixture change reaches the actual inspection owner"
          (response==Just (Just (JsonPacket (String "ready"))))
      awaitText expected=do
        d<-readIORef observed
        if activeText d==expected then pure () else threadDelay 10000 >> awaitText expected
  withAsync (runRemoteDaemon session 1 core tick inspect initial) $ \daemon->do
    link daemon
    first<-awaitOpen (300::Int)
    greeting<-attach first 0
    command first 1
    closedReceipt<-receipt first
    void (control first "ack")
    writePacket first (JsonPacket (object ["type" .= ("key"::T.Text),"seq" .= (2::Int),"key" .= ("Escape"::T.Text)]))
    void (control first "ack")
    reply first 3 closedReceipt "WRONG-BUFFER"
    void (control first "ack")
    command first 4
    old<-receipt first
    void (control first "ack")
    hClose first
    threadDelay 100000
    bracket open hClose $ \second->do
      resumed<-attach second 4
      check "Same-client reconnect retains sequence epoch" (KM.lookup "epoch" greeting==KM.lookup "epoch" resumed)
      reply second 5 old "STALE"
      void (control second "ack")
      command second 6
      fresh<-receipt second
      check "Reattached clipboard read gets a fresh identity" (fresh/=old)
      void (control second "ack")
      reply second 7 old "OLD"
      void (control second "ack")
      reply second 8 fresh "accepted"
      void (control second "ack")
      reply second 9 fresh "DUPLICATE"
      void (control second "ack")
      done<-timeout 3000000 (awaitText "acceptedsource")
      check "Closed-dialog/reconnect/old/duplicate replies cannot edit wrong target or consume fresh receipt" (done==Just ())
      checkpointState "invalidate-checkpoint"
      writePacket second (JsonPacket (object ["type" .= ("suspend"::T.Text),"seq" .= (10::Int)]))
      void (control second "error")
    checkpointState "repair-checkpoint"
    bracket open hClose $ \third->do
      resumed<-attach third 9
      check "Failed suspension leaves the daemon alive without acknowledging its input"
        (KM.lookup "ack" resumed==Just (toJSON (9::Int)))
      writePacket third (JsonPacket (object ["type" .= ("suspend"::T.Text),"seq" .= (10::Int)]))
      committed<-control third "ack"
      check "Retrying suspension commits only its successful exact input"
        (KM.lookup "seq" committed==Just (toJSON (10::Int)))
      closed<-control third "closed"
      check "Successful suspension retains a resumable checkpoint"
        (KM.lookup "resumable" closed==Just (Bool True))
    ended<-timeout 3000000 (wait daemon)
    check "Suspended daemon joins after its close acknowledgement" (ended==Just ())
    checkpoint<-S.checkpointPath session
    recovered<-readCheckpoint checkpoint initial >>= either (error . T.unpack) pure
    check "Suspension preserves the accepted paste buffer exactly"
      (activeText recovered=="acceptedsource")
