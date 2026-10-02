{-# LANGUAGE OverloadedStrings #-}
module ACPCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless, forM_, replicateM)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory hiding (executable)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Info (os)
import System.Timeout (timeout)
import THC.Edit.ACP

checks :: IO ()
checks = bracket temporary removePathForcibly $ \root -> do
  -- The Windows venv redirector retains stdout while its interpreter runs,
  -- which would invalidate the fixture that closes stdout without exiting.
  python <- if os=="mingw32" then do
    (status,path,_) <- readProcessWithExitCode "python3" ["-c","import sys;print(sys._base_executable)"] ""
    check "Python interpreter available" (status==ExitSuccess)
    pure (T.unpack (T.strip (T.pack path)))
    else pure "python3"
  let server = root </> "fake.py"
      launch = Launch python [server] [("THC_ACP_CHECK", "λ")]
      start = startClient launch root
  BS.writeFile server (TE.encodeUtf8 (T.pack fakeServer))
  bracket start stopClient $ \client -> do
    ident <- request client "initialize" (object ["text" .= ("λ😀\nhello" :: T.Text)])
    events <- waitEvents client (any isRequest)
    check "inbound string id preserved" (Request (String "permission/λ") "session/request_permission" Null `elem` events)
    respond client (String "permission/λ") (Right (object ["approved" .= False]))
    result <- waitEvents client (hasResponse ident)
    check "fragmented UTF-8 round trip and env override" (Response ident (Right (String "λ😀\nhello")) `elem` result)
    numeric <- if any isRequest result then pure result else waitEvents client (any isRequest)
    check "inbound numeric id preserved" (Request (Number 9) "fs/write_text_file" Null `elem` numeric)
    respond client (Number 9) (Left (object ["code" .= (-32601 :: Int), "message" .= ("unsupported" :: T.Text)]))
    notify client "session/cancel" (object ["sessionId" .= ("s" :: T.Text)])
    cancelled <- waitEvents client (any (\e -> case e of Notification "cancelled" _ -> True; _ -> False))
    check "cancel is a notification" (Notification "cancelled" Null `elem` cancelled)
    closing <- request client "close" Null
    ended <- waitEvents client (any isDisconnect)
    check "pending request fails on EOF" (any (\e -> case e of Response n (Left _) -> n == closing; _ -> False) ended)
    after <- request client "after-close" Null
    check "request after EOF fails immediately" . hasResponse after =<< pollEvents client
    threadDelay 20000
    check "disconnect emitted only once" . not . any isDisconnect =<< pollEvents client
    check "stop is prompt" . (== Just ()) =<< timeout 2000000 (stopClient client >> stopClient client)
  forM_ ["sys.stdout.buffer.write(b'x'*(16*1024*1024+1));sys.stdout.flush();time.sleep(20)",
         "sys.stdout.buffer.write(b'{bad}\\n');sys.stdout.flush();time.sleep(20)",
         "sys.stdout.buffer.write(b'{\"jsonrpc\":\"2.0\"}');sys.stdout.flush()"] $ \body -> do
    writeFile server ("import sys,time\n" ++ body ++ "\n")
    bracket start stopClient $ \client -> do
      ended <- waitEvents client (any isDisconnect)
      check "invalid or oversized frame disconnects" (any isDisconnect ended)
      check "cleanup completes" . (== Just ()) =<< timeout 2000000 (stopClient client)
  writeFile server $ unlines
    [ "import os,signal,subprocess,sys,time"
    , "signal.signal(signal.SIGTERM,signal.SIG_IGN)"
    , "child=subprocess.Popen([sys.executable,'-c','import time;time.sleep(30)'],stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)"
    , "open('pids','w').write(str(os.getpid())+' '+str(child.pid))"
    , "sys.stderr.write('x'*10000+'\\nlast diagnostic\\n');sys.stderr.flush()"
    , "time.sleep(0.1)"
    , "sys.stdout.close();os.close(1)"
    , "time.sleep(30)"
    ]
  bracket start stopClient $ \client -> do
    ended <- waitEvents client (any isDisconnect)
    check "stderr is drained and bounded" (any (\e -> case e of Disconnected message -> "last diagnostic" `T.isInfixOf` message && T.length message < 4500; _ -> False) ended)
    check "uncooperative process stops" . (== Just ()) =<< timeout 2000000 (stopClient client)
  pids <- words <$> readFile (root </> "pids")
  forM_ pids $ \pid -> do
    (status,_,_) <- if os=="mingw32"
      then readProcessWithExitCode python ["-c",windowsProcessProbe,pid] ""
      else readProcessWithExitCode "kill" ["-0",pid] ""
    check "process tree reaped" (if os=="mingw32" then status==ExitFailure 1 else status/=ExitSuccess)
  writeFile server $ unlines
    [ "import json,subprocess,sys,time"
    , "child=subprocess.Popen([sys.executable,'-c','import time;time.sleep(30)'])"
    , "print(json.dumps({'jsonrpc':'2.0','method':'ready'}),flush=True)"
    , "time.sleep(30)"
    ]
  bracket start stopClient $ \client -> do
    _ <- waitEvents client (any (\event -> case event of Notification "ready" _ -> True; _ -> False))
    check "idle provider and inherited output pipes stop promptly" . (== Just ()) =<< timeout 2000000 (stopClient client)
  writeFile server "import time\ntime.sleep(30)\n"
  bracket start stopClient $ \client -> do
    queued <- timeout 2000000 (replicateM 64 (request client "blocked" (String (T.replicate (1024*1024) "x"))))
    check "blocked provider never blocks caller" (maybe False ((== 64) . length) queued)
    check "blocked writer stops promptly" . (== Just ()) =<< timeout 2000000 (stopClient client)
  -- Burst consumption yields to input without dropping or reordering frames.
  writeFile server $ unlines
    [ "import json,sys"
    , "for line in sys.stdin:"
    , " r=json.loads(line); size=r['params']"
    , " for n in range(80): print(json.dumps(dict(jsonrpc='2.0',method=str(n),params='x'*size)),flush=True)"
    , " print(json.dumps(dict(jsonrpc='2.0',id=r['id'],result=True)),flush=True)"
    ]
  bracket start stopClient $ \client -> forM_ [0,100000,300000,0] $ \size -> do
    ident<-request client "burst" (toJSON (size::Int))
    let drain accumulated=do
          batch<-pollEvents client
          check "ACP batch count is bounded" (length batch<=32)
          let payloads=[T.length body | Notification _ (String body)<-batch]
          check "ACP batch byte budget allows only one oversized frame" (sum payloads<=262144 || length batch==1)
          let combined=accumulated++batch
          if hasResponse ident combined then pure combined else threadDelay 1000 >> drain combined
    result<-timeout 10000000 (drain [])
    check "ACP batches preserve FIFO and byte accounting across bursts"
      (fmap (\events->[name | Notification name _<-events]) result==Just (map (T.pack.show) [0::Int ..79]))
  putStrLn "ACP checks passed"
  where
    check label ok = unless ok (error label)
    isRequest (Request _ _ _) = True
    isRequest _ = False
    temporary = do
      base <- getTemporaryDirectory
      (path,file) <- openTempFile base "thc-edit-acp-check"
      hClose file
      removeFile path
      createDirectory path
      pure path

isDisconnect :: Event -> Bool
isDisconnect (Disconnected _) = True
isDisconnect _ = False

hasResponse :: Int -> [Event] -> Bool
hasResponse ident = any (\e -> case e of Response n _ -> n == ident; _ -> False)

waitEvents :: Client -> ([Event] -> Bool) -> IO [Event]
waitEvents client ready = do
  result <- timeout 5000000 (loop [])
  maybe (error "Timed out waiting for ACP provider") pure result
  where
    loop accumulated = do
      events <- pollEvents client
      let combined = accumulated ++ events
      if ready combined then pure combined else threadDelay 10000 >> loop combined

fakeServer :: String
fakeServer = unlines
  [ "import json,os,sys,time"
  , "def send(value):"
  , "    raw=(json.dumps(dict(jsonrpc='2.0',**value),ensure_ascii=False)+'\\n').encode()"
  , "    for byte in raw:"
  , "        sys.stdout.buffer.write(bytes([byte]));sys.stdout.buffer.flush()"
  , "def recv(): return json.loads(sys.stdin.buffer.readline())"
  , "init=recv();assert init['method']=='initialize';assert os.environ['THC_ACP_CHECK']=='λ'"
  , "sys.stderr.write('log\\n'*12000);sys.stderr.flush()"
  , "send(dict(id='permission/λ',method='session/request_permission'))"
  , "answer=recv();assert answer['id']=='permission/λ' and answer['result']==dict(approved=False)"
  , "send(dict(id=init['id'],result=init['params']['text']))"
  , "send(dict(id=9,method='fs/write_text_file'))"
  , "answer=recv();assert answer['id']==9 and answer['error']['code']==-32601"
  , "cancel=recv();assert cancel['method']=='session/cancel' and 'id' not in cancel"
  , "send(dict(method='cancelled'))"
  , "assert recv()['method']=='close'"
  ]

-- Return 0 for a live process, 1 for absent/exited, and 2 for inspection errors.
windowsProcessProbe :: String
windowsProcessProbe = unlines
  [ "import ctypes,sys"
  , "k=ctypes.WinDLL('kernel32',use_last_error=True)"
  , "k.OpenProcess.restype=ctypes.c_void_p"
  , "k.WaitForSingleObject.argtypes=[ctypes.c_void_p,ctypes.c_ulong]"
  , "k.CloseHandle.argtypes=[ctypes.c_void_p]"
  , "handle=k.OpenProcess(0x100000,False,int(sys.argv[1]))"
  , "if not handle: sys.exit(1 if ctypes.get_last_error()==87 else 2)"
  , "state=k.WaitForSingleObject(handle,0);k.CloseHandle(handle)"
  , "sys.exit(1 if state==0 else (0 if state==258 else 2))"
  ]
