{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DAPCheck (checks) where

import Control.Concurrent (threadDelay, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (concurrently_, withAsync, wait)
import Control.Exception hiding (handle)
import Control.Monad (forM_, unless, void)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Network.Socket
import System.IO
import System.Info (os)
import System.Process (CreateProcess(..), StdStream(..), proc, readProcessWithExitCode, withCreateProcess)
import Text.Read (readMaybe)
import THC.Edit.Process (processCleanup)
import System.Timeout (timeout)
import qualified THC.Edit.DAP as DAP

checks :: IO ()
checks = do
  processTreeCheck
  bracket (DAP.startAdapter "python3" ["-u", "-c", stdioPeer] ".") DAP.stopClient $ \client -> do
    ident <- DAP.request client "launch" (object ["program" .= ("λ spaced.hs" :: T.Text)])
    events <- await client (\es -> any (\e -> case e of DAP.Response _ _ -> True; _ -> False) es && any notification es)
    check "stdio launch response correlates" (DAP.Response ident (Right (object ["program" .= ("λ spaced.hs" :: T.Text)])) `elem` events)
    check "stdio stderr is drained and reported" (any (\e -> case e of DAP.Notification "output" body -> field "category" body==Just ("stderr" :: T.Text); _ -> False) events)
  bracket (DAP.startAdapter "python3" ["-u", "-c", "import sys,time; sys.stdin.buffer.readline(); sys.stderr.write('ready'); sys.stderr.flush(); time.sleep(60)"] ".") DAP.stopClient $ \client -> do
    ident <- DAP.request client "launch" Null
    _ <- await client (any notification)
    stopped <- timeout processStopTimeout (DAP.stopClient client)
    check "stalled stdio adapter stops promptly" (stopped == Just ())
    events <- DAP.pollEvents client
    check "stopping stdio adapter fails in-flight request" (any (failed ident) events)
  bracket (DAP.startAdapter "python3" ["-u", "-c", "import sys; sys.stderr.buffer.write(('λ😀'*4000).encode()); sys.stderr.flush()\n"++stdioPeer] ".") DAP.stopClient $ \client -> do
    _ <- DAP.request client "threads" Null
    let stderrText es=T.concat [value | DAP.Notification "output" body<-es,Just value<-[field "output" body]]
    events<-await client (T.isInfixOf "adapter diagnostic" . stderrText)
    check "stdio output preserves UTF8 across pipe read boundaries" (T.replicate 4000 "λ😀" `T.isPrefixOf` stderrText events)
  bracket (DAP.startAdapter "/nonexistent/thc-dap-adapter" [] ".") DAP.stopClient $ \client -> do
    ident <- DAP.request client "launch" Null
    events <- await client (any disconnected)
    check "failed adapter spawn fails launch promptly" (any (failed ident) events)
  managedPort <- bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
    bind listener (SockAddrInet 0 (tupleToHostAddress (127,0,0,1)))
    SockAddrInet number _ <- getSocketName listener
    pure (fromIntegral number)
  bracket (DAP.startManaged "python3" ["-u","-c",managedPeer,show managedPort] "." "127.0.0.1" managedPort) DAP.stopClient $ \client -> do
    ident <- DAP.request client "initialize" (object [])
    events <- await client (\es -> DAP.Response ident (Right (object [])) `elem` es)
    check "managed launch connects after process startup" (DAP.Response ident (Right (object [])) `elem` events)
  withServer drain $ \occupiedPort ->
    bracket (DAP.startManaged "/nonexistent/should-not-start" [] "." "127.0.0.1" occupiedPort) DAP.stopClient $ \client -> do
      events <- await client (any disconnected)
      check "managed launch rejects occupied port before spawning" (any (\e -> case e of DAP.Disconnected reason -> "already in use" `T.isInfixOf` reason; _ -> False) events)
  bracket (DAP.startManaged "python3" ["-c","raise SystemExit(23)"] "." "127.0.0.1" managedPort) DAP.stopClient $ \client -> do
    ident <- DAP.request client "initialize" Null
    events <- await client (any disconnected)
    check "failed THC startup fails queued initialize" (any (failed ident) events)
    check "failed THC startup reports exit status" (any (\e -> case e of DAP.Disconnected reason -> "23" `T.isInfixOf` reason; _ -> False) events)
  bracket (DAP.startManaged "python3" ["-u","-c","import time; print('waiting',flush=True); time.sleep(60)"] "." "127.0.0.1" managedPort) DAP.stopClient $ \client -> do
    ident<-DAP.request client "initialize" Null
    _<-await client (any notification)
    stopped<-timeout processStopTimeout (DAP.stopClient client)
    check "managed startup cancellation stops process before joining output readers" (stopped==Just ())
    events<-DAP.pollEvents client
    check "managed startup cancellation fails initialize" (any (failed ident) events)
  withServer protocol $ \port -> bracket (DAP.startClient "localhost" port) DAP.stopClient $ \client -> do
    first <- DAP.request client "threads" (object ["text" .= ("λ😀" :: T.Text)])
    second <- DAP.request client "stackTrace" Null
    third <- DAP.request client "evaluate" Null
    events <- await client (\es -> length [() | DAP.Response _ _ <- es] == 3 && any notification es)
    check "out-of-order correlation and UTF8 response body" (DAP.Response first (Right (String "λ😀")) `elem` events)
    check "missing success body becomes null" (DAP.Response second (Right Null) `elem` events)
    check "failed response message" (DAP.Response third (Left "bad expression") `elem` events)
    check "event body preserved" (DAP.Notification "stopped" (object ["threadId" .= (7 :: Int)]) `elem` events)
    check "responses retain wire order" ([ident | DAP.Response ident _ <- events] == [second,third,first])
  forM_ [ ("oversize body", "Content-Length: 16777217\r\n\r\n")
        , ("unterminated oversized header", BS.replicate 9000 120)
        , ("duplicate length", "Content-Length: 2\r\nContent-Length: 2\r\n\r\n{}")
        , ("negative length", "Content-Length: -1\r\n\r\n")
        , ("invalid JSON", "Content-Length: 1\r\n\r\n{")
        , ("truncated body", "Content-Length: 20\r\n\r\n{}")
        , ("disconnect", "")
        ] $ \(label, bytes) -> withServer (\handle -> receive handle >> BS.hPut handle bytes >> hFlush handle) $ \port ->
          bracket (DAP.startClient "127.0.0.1" port) DAP.stopClient $ \client -> do
            ident <- DAP.request client "threads" Null
            events <- await client (any disconnected)
            check (label ++ " fails outstanding request") (any (failed ident) events)
            check (label ++ " disconnects once") (length [() | DAP.Disconnected _ <- events] == 1)
            more <- DAP.pollEvents client
            check (label ++ " does not repeat disconnect") (not (any disconnected more))
  forM_ [("example.com", 4711), ("127.0.0.2", 4711), ("127.0.0.1", 0), ("::1", 65536)] $ \(host,port) -> do
    fast <- timeout 500000 (DAP.startClient host port)
    client <- maybe (error "startClient blocked") pure fast
    bracket (pure client) DAP.stopClient $ \c -> do
      events <- await c (any disconnected)
      check "invalid remote endpoint rejected" (any disconnected events)
  received <- newEmptyMVar
  withServer (\handle -> receive handle >> putMVar received () >> drain handle) $ \port ->
    bracket (DAP.startClient "127.0.0.1" port) DAP.stopClient $ \client -> do
      ident <- DAP.request client "continue" Null
      takeMVar received
      stopped <- timeout 500000 (DAP.stopClient client)
      check "stop cancels blocked worker promptly" (stopped == Just ())
      events <- DAP.pollEvents client
      check "stop fails queued/inflight request" (any (failed ident) events)
      DAP.stopClient client
      after <- DAP.request client "threads" Null
      afterEvents <- DAP.pollEvents client
      check "request after stop receives correlated error" (any (failed after) afterEvents)
  withServer (\handle -> receive handle >> putMVar received () >> drain handle) $ \port ->
    bracket (DAP.startClient "127.0.0.1" port) DAP.stopClient $ \client -> do
      results <- mapM (\_ -> try (DAP.request client "threads" Null) :: IO (Either IOException Int)) [1..200 :: Int]
      takeMVar received -- Ensure the server accepted before cancellation can stop the client.
      check "pending queue is bounded with prompt rejection" (any (either (const True) (const False)) results)
  withServer drain $ \port ->
    bracket (DAP.startClient "127.0.0.1" port) DAP.stopClient $ \client -> do
      ident <- DAP.request client "evaluate" (String (T.replicate (16*1024*1024) "x"))
      events <- await client (any disconnected)
      check "outgoing size limit fails correlated request" (any (failed ident) events)
  sent <- newEmptyMVar
  withServer (\handle -> do
      void (receive handle)
      BS.hPut handle (BS.concat (replicate 300 (frame (object ["seq" .= (1 :: Int), "type" .= ("event" :: T.Text), "event" .= ("output" :: T.Text)]))))
      hFlush handle
      putMVar sent ()
      drain handle) $ \port ->
    bracket (DAP.startClient "127.0.0.1" port) DAP.stopClient $ \client -> do
      void (DAP.request client "threads" Null)
      takeMVar sent
      events <- await client (\es -> length [() | DAP.Notification "output" Null <- es] == 300)
      check "event backpressure loses no messages" (length events == 300)
  putStrLn "DAP checks passed"
  where
    notification (DAP.Notification _ _) = True
    notification _ = False

drain :: Handle -> IO ()
drain handle = BS.hGetSome handle 4096 >>= \part -> unless (BS.null part) (drain handle)

check :: String -> Bool -> IO ()
check label ok = unless ok (error label)

disconnected :: DAP.Event -> Bool
disconnected (DAP.Disconnected _) = True
disconnected _ = False

failed :: Int -> DAP.Event -> Bool
failed ident (DAP.Response actual (Left _)) = ident == actual
failed _ _ = False

await :: DAP.Client -> ([DAP.Event] -> Bool) -> IO [DAP.Event]
await client done = timeout (if os=="mingw32" then 8000000 else 3000000) (loop []) >>= maybe (error "Timed out waiting for DAP") pure
  where
    loop previous = do
      events <- (previous ++) <$> DAP.pollEvents client
      if done events then pure events else threadDelay 1000 >> loop events

withServer :: (Handle -> IO ()) -> (Int -> IO a) -> IO a
withServer serve action = bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
  bind listener (SockAddrInet 0 (tupleToHostAddress (127,0,0,1)))
  listen listener 1
  SockAddrInet port _ <- getSocketName listener
  withAsync (bracket (accept listener >>= \(connection,_) -> socketToHandle connection ReadWriteMode) hClose $ \handle -> do
      hSetBinaryMode handle True
      hSetBuffering handle NoBuffering
      serve handle) $ \server -> do
    result <- action (fromIntegral port)
    timeout 3000000 (wait server) >>= maybe (error "DAP fixture did not close") pure
    pure result

protocol :: Handle -> IO ()
protocol handle = do
  one <- receive handle
  two <- receive handle
  three <- receive handle
  check "DAP request uses command, arguments and type" (field "type" one == Just (String "request") && field "command" one == Just (String "threads") && field "arguments" one == Just (object ["text" .= ("λ😀" :: T.Text)]))
  let ident value = fromJust (field "seq" value :: Maybe Int)
      response seqNum req fields = object (["seq" .= (seqNum :: Int), "type" .= ("response" :: T.Text), "request_seq" .= req, "command" .= ("threads" :: T.Text)] ++ fields)
      bytes = BS.concat
        [ frame (response 1 (ident two) ["success" .= True])
        , frame (object ["seq" .= (2 :: Int), "type" .= ("event" :: T.Text), "event" .= ("stopped" :: T.Text), "body" .= object ["threadId" .= (7 :: Int)]])
        , frame (response 3 (ident three) ["success" .= False, "message" .= ("bad expression" :: T.Text)])
        , frame (response 4 (ident one) ["success" .= True, "body" .= ("λ😀" :: T.Text)])
        ]
  mapM_ (BS.hPut handle) [BS.take 7 bytes, BS.take 21 (BS.drop 7 bytes), BS.drop 28 bytes]
  hFlush handle
  void (BS.hGetSome handle 1)

frame :: Value -> BS.ByteString
frame value = let body = BL.toStrict (encode value) in BC.pack ("Content-Length: " ++ show (BS.length body) ++ "\r\n\r\n") <> body

field :: FromJSON a => Key -> Value -> Maybe a
field key = parseMaybe (withObject "object" (.: key))

receive :: Handle -> IO Value
receive handle = do
  size <- headers
  body <- BS.hGet handle size
  either (error . ("invalid client frame: " ++)) pure (eitherDecodeStrict' body)
  where
    headers = do
      line <- BC.hGetLine handle
      if "Content-Length: " `BS.isPrefixOf` line then do
        let count = read (BC.unpack (BS.drop 16 line))
        blank <- BC.hGetLine handle
        check "client header terminator" (blank == "\r")
        pure count
      else error "Missing client Content-Length"

stdioPeer :: String
stdioPeer = unlines
  [ "import sys,json"
  , "sys.stderr.write('adapter diagnostic\\n'); sys.stderr.flush()"
  , "n=int(sys.stdin.buffer.readline().split(b':')[1]); sys.stdin.buffer.readline()"
  , "req=json.loads(sys.stdin.buffer.read(n))"
  , "body=json.dumps(dict(seq=1,type='response',request_seq=req['seq'],success=True,body=req['arguments'])).encode()"
  , "sys.stdout.buffer.write(('Content-Length: %d\\r\\n\\r\\n'%len(body)).encode()+body); sys.stdout.buffer.flush()"
  , "sys.stdin.buffer.read()"
  ]

managedPeer :: String
managedPeer = unlines
  [ "import socket,sys,types"
  , "print('managed startup output',flush=True)"
  , "server=socket.socket(); server.bind(('127.0.0.1',int(sys.argv[1]))); server.listen(1)"
  , "conn,_=server.accept(); stream=conn.makefile('rwb',buffering=0)"
  , "sys.stdin=types.SimpleNamespace(buffer=stream); sys.stdout=types.SimpleNamespace(buffer=stream)"
  ] ++ stdioPeer

-- The descendant ignores graceful signals and keeps the output pipe open. Merely
-- terminating its parent must fail this check on both POSIX and Windows.
processTreeCheck :: IO ()
processTreeCheck = do
  let child="import os,signal,time; signal.signal(signal.SIGINT,signal.SIG_IGN); signal.signal(signal.SIGTERM,signal.SIG_IGN); print(os.getpid(),flush=True); time.sleep(10)"
      parent="import subprocess,sys,time; subprocess.Popen([sys.executable,'-u','-c',sys.argv[1]]); time.sleep(10)"
      emergency pid=void $ timeout 2000000 $ readProcessWithExitCode "python3" ["-c",
        "import os,signal,sys\ntry: os.kill(int(sys.argv[1]),signal.SIGTERM if sys.platform=='win32' else signal.SIGKILL)\nexcept ProcessLookupError: pass",show pid] ""
  withCreateProcess (proc "python3" ["-u","-c",parent,child]) {create_group=True,std_out=CreatePipe} $ \_ outputHandle _ process -> do
    stop<-processCleanup process
    flip finally stop $ case outputHandle of
      Nothing -> error "process cleanup fixture output unavailable"
      Just stream -> do
        ready<-timeout 3000000 (hGetLine stream)
        pid<-maybe (error "process cleanup fixture did not start") pure (ready >>= readMaybe :: Maybe Int)
        flip onException (emergency pid) $ do
          ended<-timeout (if os=="mingw32" then processStopTimeout else 3000000) (concurrently_ stop stop)
          check "concurrent process-tree cleanup completes promptly" (ended==Just ())
          eof<-timeout 1000000 (hIsEOF stream)
          check "cleanup kills signal-resistant descendants retaining pipe handles" (eof==Just True)
          stop

-- taskkill /T takes 2-5 seconds on native Windows; cleanup grants it five seconds
-- plus bounded helper/child reaping. Socket-only cancellation keeps its 0.5s check.
processStopTimeout :: Int
processStopTimeout=if os=="mingw32" then 8000000 else 2000000
