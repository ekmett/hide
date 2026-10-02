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
import System.Timeout (timeout)
import THC.Edit.ACP

checks :: IO ()
checks = bracket temporary removePathForcibly $ \root -> do
  let server = root </> "fake.py"
      launch = Launch "python3" [server] [("THC_ACP_CHECK", "λ")]
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
    (status,_,_) <- readProcessWithExitCode "kill" ["-0",pid] ""
    check "process tree reaped" (status /= ExitSuccess)
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
