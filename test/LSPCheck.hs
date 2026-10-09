{-# LANGUAGE OverloadedStrings #-}
module LSPCheck (checks) where
import AllocationProfile (AllocationProfile, withinBudget)

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, evaluate)
import GHC.Conc (getAllocationCounter)
import Control.Monad (forM_, unless)
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Info (os)
import System.Timeout (timeout)
import qualified Hide.Buffer as Buffer
import Hide.LSP

checks :: AllocationProfile -> IO ()
checks profile = do
  forM_ ["", "a😀b\r\nxλ", "\n\n", "a\r\r\nlast\r", "e\x0301\n界\0"] $ \source -> do
    let original=Buffer.newBuffer source
        edited=Buffer.replaceSelection (Buffer.Selection 0 0) "😀\n" original
    forM_ [original,edited] $ \buffer -> do
      let text=Buffer.contents buffer
      forM_ [-1..T.length text+2] $ \offset -> do
        check "measured UTF16 offset conversion matches flat text" (bufferOffsetPosition buffer offset==offsetPosition text offset)
        check "measured UTF16 position JSON matches flat text" (bufferPositionValue buffer offset==positionValue text offset)
      forM_ [-1..Buffer.bufferLineCount buffer+2] $ \row -> forM_ [-1..12] $ \column ->
        check "measured UTF16 position conversion matches flat text" (bufferPositionOffset buffer (row,column)==positionOffset text (row,column))
  -- A partial prefix ending in CR is still source content, not a terminator.
  let interior=Buffer.newBuffer "a\rb\r\nlast\r"
      astral=Buffer.newBuffer "a😀b\r\n"
  check "UTF16 prefix retains an interior CR" (bufferPositionOffset interior (0,2)==2)
  check "UTF16 prefix stops before the real CRLF" (bufferPositionOffset interior (0,99)==3)
  check "UTF16 buffer position clamps within an astral scalar" (bufferPositionOffset astral (0,2)==1 && bufferPositionOffset astral (0,3)==2)
  check "UTF16 buffer position clamps negative and EOF coordinates" (bufferPositionOffset astral (-1,-1)==0 && bufferPositionOffset astral (99,99)==5)
  let long=Buffer.replaceSelection (Buffer.Selection 6 6) "x"
        (Buffer.newBuffer ("head\n"<>T.replicate 200000 "a😀b"<>"\r\nlast"))
  -- Install the edited raw row before measuring the bounded endpoint lookup.
  -- This must not prepare the row's untouched display suffix.
  _<-evaluate (Buffer.prepareBuffer long)
  beforePrefix<-getAllocationCounter
  forM_ [3..12] $ \column->do
    actual<-evaluate (bufferPositionOffset long (1,column))
    check "edited long-row UTF16 prefix matches independent short source" (actual==5+positionOffset "ax😀ba😀ba😀ba😀b" (0,column))
  afterPrefix<-getAllocationCounter
  check "UTF16 prefix lookup does not flatten the selected edited row" (withinBudget profile (beforePrefix-afterPrefix) (1024*1024))
  let large=Buffer.newBuffer (T.replicate 200000 "a😀b\r\n")
  _<-evaluate (Buffer.prepareBuffer large)
  allocationBefore<-getAllocationCounter
  forM_ [199990..199999] $ \row -> do
    offset<-evaluate (bufferPositionOffset large (row,3))
    check "deep UTF16 position seeks directly to measured line" (offset==row*5+2)
    check "deep UTF16 offset seeks directly to measured line" (bufferOffsetPosition large offset==(row,3))
  allocationAfter<-getAllocationCounter
  check "UTF16 endpoints do not traverse or split preceding document text" (withinBudget profile (allocationBefore-allocationAfter) (1024*1024))
  let sample = "a😀b\r\nxλ"
      path = "/tmp/λ space/#%?.hs"
  check "URI Unicode and reserved characters round trip" (uriFilePath (fileUri path) == Just path)
  check "reject nonlocal and malformed URI" (all ((== Nothing) . uriFilePath) ["https://host/x", "file://host/x", "file:///tmp/%xz", "file:///tmp/%ff", "file:///tmp/#x"])
  check "localhost URI" (uriFilePath "file://localhost/tmp/a" == Just "/tmp/a")
  check "UTF16 counts astral characters" (offsetPosition sample 2 == (0,3))
  check "UTF16 newline round trip" (offsetPosition sample 6 == (1,1) && positionOffset sample (1,1) == 6)
  check "UTF16 clamps halfway through surrogate pair" (positionOffset sample (0,2) == 1)
  check "UTF16 clamps out of bounds" (positionOffset sample (90,90) == T.length sample && positionOffset sample (0,90) == 3)
  bracket temporary removePathForcibly $ \root -> do
    let server = root </> "fake-hls"
        source = root </> "space λ.hs"
    writeUtf8 (root </> "diagnostic-source.hs") "module X where\n"
    if os=="mingw32" then writeUtf8 source "module X where\n"
      else createFileLink (root </> "diagnostic-source.hs") source
    canonicalSource<-canonicalizePath source
    writeUtf8 server fakeServer
    bracket (startClientWith "python3" ["-X", "utf8", server, "--lsp"] root) stopClient $ \client -> do
      syncDocuments client [(source,0,"module X where\nx = \"😀\"\n")]
      notifySaved client source
      first <- request client "test/state" Null
      pending <- waitResponse client first
      check "initialize then open queued document" (result pending == Just (object ["opens" .= (1 :: Int), "changes" .= (0 :: Int), "closes" .= (0 :: Int), "text" .= ("module X where\nx = \"😀\"\n" :: T.Text)]))
      check "diagnostics resolve source aliases on the transport worker" (any (\event -> case event of Diagnostics file version _ -> file == canonicalSource && version == Just 0; _ -> False) pending)
      syncDocuments client [(source,0,"module X where\nx = \"😀\"\n")]
      syncDocuments client [(source,1,"x = 2\n")]
      second <- request client "test/state" Null
      updated <- waitResponse client second
      check "unchanged sync skipped and changed full text sent" (result updated == Just (object ["opens" .= (1 :: Int), "changes" .= (1 :: Int), "closes" .= (0 :: Int), "text" .= ("x = 2\n" :: T.Text)]))
      syncDocuments client []
      third <- request client "test/state" Null
      closed <- waitResponse client third
      check "removed document closes" (result closed == Just (object ["opens" .= (1 :: Int), "changes" .= (1 :: Int), "closes" .= (1 :: Int), "text" .= ("x = 2\n" :: T.Text)]))
      execution<-executeCommand client "fixture" (toJSON ([]::[Value])) >>= either (error . T.unpack) pure
      concurrent<-executeCommand client "fixture" (toJSON ([]::[Value]))
      check "command ownership is exclusive until response" (case concurrent of Left _->True; _->False)
      let awaitEdit=do
            batch<-pollEvents client
            case [(owner,ident) | ApplyEdit owner ident _<-batch] of
              value:_->pure value
              _->threadDelay 1000 >> awaitEdit
      owned<-timeout 3000000 awaitEdit >>= maybe (error "Missing owned applyEdit") pure
      check "server request carries receipt-time execution owner" (fst owned==execution)
      replyEdit client (snd owned) True Nothing
      _<-waitResponse client execution
      barrier<-request client "test/state" Null
      after<-waitResponse client barrier
      check "post-response applyEdit never acquires completed owner" (not (any (\event->case event of ApplyEdit{}->True; _->False) after))
    lifecycle <- readFile (root </> "lifecycle")
    check "shutdown response then exit" (lifecycle == "shutdown\nexit\n")
    -- Initialization is deliberately held: document projection/equality must
    -- belong to the writer, never to the caller holding the desktop lock.
    writeUtf8 server "#!/usr/bin/env python3\nimport time\ntime.sleep(30)\n"
    bracket (startClientWith "python3" ["-X", "utf8", server, "--lsp"] root) stopClient $ \client -> do
      let delayed = [(source,0,error "syncDocuments forced source text on caller")]
      syncDocuments client delayed
      syncDocuments client delayed
    writeUtf8 server "#!/usr/bin/env python3\nimport sys,time\nsys.stderr.write('x'*10000 + '\\ncompiler-version-unavailable\\n'); sys.stderr.flush()\ntime.sleep(0.1)\nsys.exit(1)\n"
    bracket (startClientWith "python3" ["-X", "utf8", server, "--lsp"] root) stopClient $ \client -> do
      failure <- timeout 3000000 (waitFailure client)
      check "failed server retains bounded stderr tail" (maybe False (\message -> "compiler-version-unavailable" `T.isInfixOf` message && T.length message < 4500) failure)
      syncDocuments client [(source,0,"x = 1")]
      syncDocuments client [(source,0,"x = 1")]
      ident <- request client "textDocument/hover" Null
      replies <- pollEvents client
      check "request after failure receives error response" (any (\event -> case event of Response actual value -> actual == ident && parseMaybe (withObject "response" (.: "error")) value /= (Nothing :: Maybe Value); _ -> False) replies)
      burst <- mapM (\_ -> request client "textDocument/hover" Null) [1..200::Int]
      batch <- pollEvents client
      check "HLS flood gives the UI a bounded batch" (length batch <= 32)
      rest <- waitResponse client (last burst)
      check "bounded HLS batches retain FIFO responses"
        ([actual | Response actual _ <- batch ++ rest] == burst)
      stopped <- timeout 2000000 (stopClient client)
      check "failed server stops promptly and idempotently" (stopped == Just ())
  putStrLn "LSP checks passed"
  where
    check label ok = unless ok (error label)
    result events = case [value | Response _ response <- events, Just value <- [parseMaybe (withObject "response" (.: "result")) response]] of value:_ -> Just value; [] -> Nothing
    temporary = do
      root <- getTemporaryDirectory
      (path, file) <- openTempFile root "hide-lsp-check"
      hClose file
      removeFile path
      createDirectory path
      pure path

waitFailure :: Client -> IO T.Text
waitFailure client = do
  pending <- pollEvents client
  case [message | ServerError message <- pending] of
    message:_ -> pure message
    [] -> threadDelay 10000 >> waitFailure client

waitResponse :: Client -> Int -> IO [Event]
waitResponse client ident = do
  answer <- timeout 5000000 (loop [])
  maybe (error "Timed out waiting for fake HLS") pure answer
  where
    loop accumulated = do
      pending <- pollEvents client
      case [message | ServerError message <- pending] of
        message:_ -> error (T.unpack message)
        [] -> pure ()
      let combined = accumulated ++ pending
      if any (\event -> case event of Response actual _ -> actual == ident; _ -> False) combined
        then pure combined
        else threadDelay 10000 >> loop combined

fakeServer :: String
fakeServer = unlines
  [ "#!/usr/bin/env python3"
  , "import json, sys"
  , "def recv():"
  , "    headers = {}"
  , "    while True:"
  , "        line = sys.stdin.buffer.readline()"
  , "        if not line: raise EOFError()"
  , "        if line == b'\\r\\n': break"
  , "        key, value = line.decode().split(':', 1)"
  , "        headers[key.lower()] = value.strip()"
  , "    return json.loads(sys.stdin.buffer.read(int(headers['content-length'])))"
  , "def send(value):"
  , "    body = json.dumps(dict(jsonrpc='2.0', **value), ensure_ascii=False).encode()"
  , "    frame = ('Content-Type: application/vscode-jsonrpc; charset=utf-8\\r\\nContent-Length: %d\\r\\n\\r\\n' % len(body)).encode() + body"
  , "    for part in [frame[:11], frame[11:47], frame[47:]]:"
  , "        sys.stdout.buffer.write(part); sys.stdout.buffer.flush()"
  , "init = recv()"
  , "assert init['method'] == 'initialize'"
  , "assert init['params']['capabilities']['textDocument']['codeAction']['resolveSupport']['properties']==['edit']"
  , "sys.stderr.write('server log\\n' * 12000); sys.stderr.flush()"
  , "send(dict(id='config', method='workspace/configuration', params=dict(items=[{}, {}])))"
  , "assert recv()['result'] == [None, None]"
  , "send(dict(id='folders', method='workspace/workspaceFolders', params={}))"
  , "assert recv()['result'] == init['params']['workspaceFolders']"
  , "send(dict(id='unsupported', method='workspace/applyEdit', params={}))"
  , "assert recv()['result']['applied'] == False"
  , "send(dict(id=init['id'], result=dict(capabilities={})))"
  , "assert recv()['method'] == 'initialized'"
  , "state = dict(opens=0, changes=0, closes=0, text='')"
  , "while True:"
  , "    message = recv(); method = message['method']; params = message.get('params')"
  , "    if method == 'textDocument/didOpen':"
  , "        doc = params['textDocument']; state['opens'] += 1; state['text'] = doc['text']"
  , "        send(dict(method='textDocument/publishDiagnostics', params=dict(uri=doc['uri'], version=doc['version'], diagnostics=[dict(message='λ diagnostic')])))"
  , "    elif method == 'textDocument/didChange':"
  , "        state['changes'] += 1; state['text'] = params['contentChanges'][0]['text']"
  , "    elif method == 'textDocument/didClose': state['closes'] += 1"
  , "    elif method == 'textDocument/didSave': pass"
  , "    elif method == 'test/state': send(dict(id=message['id'], result=state))"
  , "    elif method == 'workspace/executeCommand':"
  , "        send(dict(id='owned',method='workspace/applyEdit',params=dict(edit={})))"
  , "        assert recv()['result']['applied'] == True"
  , "        send(dict(id=message['id'],result=None))"
  , "        send(dict(id='late',method='workspace/applyEdit',params=dict(edit={})))"
  , "        late=recv()"
  , "        if late.get('method')=='test/state': send(dict(id=late['id'],result=state)); late=recv()"
  , "        assert late['result']['applied'] == False"
  , "    elif method == 'shutdown':"
  , "        open('lifecycle', 'a').write('shutdown\\n'); send(dict(id=message['id'], result=None))"
  , "    elif method == 'exit':"
  , "        open('lifecycle', 'a').write('exit\\n'); break"
  , "    else: raise RuntimeError(method)"
  ]

writeUtf8 :: FilePath -> String -> IO ()
writeUtf8 path = BS.writeFile path . TE.encodeUtf8 . T.pack
