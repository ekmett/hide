{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
module TypedBufferReadsCheck (checks) where

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Concurrent (threadDelay,newEmptyMVar,putMVar,takeMVar)
import Control.Concurrent.Async
import Control.Exception (bracket,try,evaluate,SomeException,IOException,catch,throwIO,finally)
import Control.Monad (unless,forM)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Conc (getAllocationCounter,threadStatus,ThreadStatus(..),BlockReason(..))
import System.Directory
import System.FilePath
import System.IO (hClose)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import Hide.Protocol
import Hide.Remote
import Hide.RemoteEndpoint
import qualified Hide.Session as S
import Hide.BufferReadCommand (withBufferReadCommands)
import Hide.BufferDiffCommand (withBufferDiffCommands,bufferDiffTool)
import Hide.WorkspaceFilesMCP (fileTools)
import qualified Data.Text.Encoding as TE
import System.Timeout (timeout)
import Hide.Buffer
import Hide.Model
import Hide.MCPPermissions
import qualified Hide.Plugin.Buffer as P
import Hide.Plugin.BufferHost (readerReference)
import Hide.EditorMCP (builtinTools,readBufferTool,editorResponseOnly)

ownerUntil owner desktop worker=do
  let loop current=do
        result<-poll worker
        case result of
          Just _->pure current
          Nothing->threadDelay 1000 >> tickPermissions owner current >>= loop
  timeout 3000000 (loop desktop) >>= maybe (error "typed read owner did not settle") pure

checks :: IO ()
checks=do
  temporary<-getTemporaryDirectory
  let root=temporary </> "hide-typed-buffer-read-check"
      path=root </> "config.toml"
      base=addDocument Nothing (newBuffer "original\n") (initialDesktop (80,25))
      ident=maybe (error "missing read target") sourceFixtureBuffer (activeWindow base)
      check label ok=unless ok (error label)
      wait worker=timeout 3000000 (Control.Concurrent.Async.wait worker) >>= maybe (error "typed read reply timed out") pure
      queued worker=do
        let observe=threadStatus (asyncThreadId worker) >>= \state->case state of
              ThreadBlocked BlockedOnMVar->pure ()
              ThreadFinished->error "typed reader finished before owner capture"
              ThreadDied->error "typed reader died before owner capture"
              _->threadDelay 1000 >> observe
        timeout 3000000 observe >>= maybe (error "typed read did not enqueue") pure
      text image=P.readText (P.capturedContent image) (P.TextRange (P.CharOffset 0) (P.CharOffset (P.readLength (P.capturedContent image))))
  bracket (createDirectoryIfMissing True root) (const (removePathForcibly root)) $ \_->do
    TIO.writeFile path "[editor.mcp.permissions]\nread_buffer = 'enable'\n"
    withPermissionsAt path builtinTools $ \owner->do
      let reader=bufferReader owner (pure (Right ()))
          reference=readerReference reader ident
      withAsync (P.captureBuffer reader reference) $ \worker->do
        queued worker
        pending<-poll worker
        check "typed capture waits outside the owner" (case pending of Nothing->True; _->False)
        let current=base {buffers=M.adjust (\doc->doc {documentBuffer=newBuffer "current λ\n"}) ident (buffers base)}
        _<-ownerUntil owner current worker
        captured<-wait worker >>= either (error . T.unpack) pure
        check "typed capture reads owner current state" (text captured==Right "current λ\n")
        check "granted snapshot retains measured source" (P.capturedRef captured==reference)
      let switched=base {buffers=M.adjust (\doc->doc {documentBuffer=(newBuffer "byte text\n") {byteMode=True,saved=error "dirty comparison reached saved text",undoStack=error "capture retained Undo evaluation"}}) ident (buffers base)}
      withAsync (P.captureBuffer reader reference) $ \worker->do
        queued worker
        _<-ownerUntil owner switched worker
        image<-wait worker >>= either (error . T.unpack) pure
        check "mode-switched capture admits without dirty text evaluation" (P.representation (P.capturedContent image)==P.ByteBuffer)
        result<-try (evaluate (P.modified (P.capturedMetadata image))) :: IO (Either SomeException Bool)
        check "mode-switched dirty comparison remains worker metadata" (case result of Left _->True; _->False)
      let large=(newBuffer (T.replicate 262144 "old line\n")) {byteMode=True}
          switchedLarge=base {buffers=M.adjust (\doc->doc {documentBuffer=large}) ident (buffers base)}
      _<-evaluate (bufferLength large)
      withAsync (P.captureBuffer reader reference) $ \worker->do
        queued worker
        before<-getAllocationCounter
        _<-tickPermissions owner switchedLarge
        after<-getAllocationCounter
        check "typed admission does not encode mode-switched full text" (before-after<2000000)
        _<-ownerUntil owner switchedLarge worker
        _<-wait worker >>= either (error . T.unpack) pure
        pure ()
      actor<-newIORef (Right ())
      let attributed=bufferReader owner (readIORef actor)
      withAsync (P.captureBuffer attributed reference) $ \worker->do
        queued worker
        writeIORef actor (Left "actor revoked")
        _<-ownerUntil owner base worker
        outcome<-wait worker
        check "queued typed read rechecks actor" (case outcome of Left "actor revoked"->True; _->False)
      withAsync (P.captureBuffer reader reference) $ \worker->do
        queued worker
        cancel worker
        _<-tickPermissions owner base
        check "typed capture cancellation reaches request" . either (const True) (const False) =<< waitCatch worker
      entered<-newEmptyMVar
      release<-newEmptyMVar
      let failing=bufferReader owner (putMVar entered () >> takeMVar release >> ioError (userError "admission failure"))
      withAsync (P.captureBuffer failing reference) $ \first->do
        queued first
        withAsync (P.captureBuffer reader reference) $ \remaining->do
          queued remaining
          withAsync (ownerUntil owner base remaining) $ \tick->do
            takeMVar entered
            putMVar release ()
            _<-wait tick
            rejected<-wait first
            check "synchronous admission failure resolves its request" (case rejected of Left _->True; _->False)
            accepted<-wait remaining
            check "admission failure continues accepted remainder" (case accepted of Right _->True; _->False)
      interrupted<-newEmptyMVar
      parked<-newEmptyMVar
      let gated=bufferReader owner (putMVar interrupted () >> takeMVar parked >> pure (Right ()))
      withAsync (P.captureBuffer gated reference) $ \first->do
        queued first
        withAsync (P.captureBuffer reader reference) $ \remaining->do
          queued remaining
          withAsync (ownerUntil owner base remaining) $ \tick->do
            takeMVar interrupted
            cancel tick
            check "interrupted admission resolves extracted current request" . either (const True) (const False) =<< wait first
            check "interrupted admission resolves accepted remainder" . either (const True) (const False) =<< wait remaining
    accepted<-withPermissionsAt path builtinTools $ \owner->do
      let reader=bufferReader owner (pure (Right ()))
          reference=readerReference reader ident
      workers<-forM [1..32::Int] $ \_->do
        worker<-async (P.captureBuffer reader reference)
        queued worker
        pure worker
      overflow<-timeout 3000000 (P.captureBuffer reader reference)
      check "full ingress refuses explicitly without blocking" (case overflow of Just (Left _)->True; _->False)
      pure workers
    outcomes<-mapM wait accepted
    check "all accepted ingress replies survive shutdown" (all (either (const True) (const False)) outcomes)
    TIO.writeFile path "[editor.mcp.permissions]\nread_buffer = 'enable'\nbuffer_apply_diff = 'enable'\n"
    withPermissionsAt path (builtinTools++fileTools) $ \owner->withBufferReadCommands $ \commands->withBufferDiffCommands $ \diffCommands->do
      session<-randomIdentity
      endpoint<-sessionEndpoint session
      let reader=bufferReader owner (pure (Right ()))
          inspect d _ request=do
            let dispatch current name args
                  | name=="buffer_apply_diff"=bufferDiffTool diffCommands (bufferEditor owner (pure (Right ()))) current name args
                  | otherwise=readBufferTool commands reader current name args
            (next,reply)<-editorResponseOnly (builtinTools++fileTools) dispatch d request
            pure (False,next,reply)
          effects d requests=pure (Exit `elem` requests,d)
          open attempts=connectEndpoint endpoint `catch` \(err::IOException)->
            if attempts<=0 then throwIO err else threadDelay 10000 >> open (attempts-1)
          receive h=timeout 3000000 (readPacket h) >>= maybe (error "typed daemon read timed out") pure
          callRemote name args=bracket (open (100::Int)) hClose $ \h->do
            writePacket h (JsonPacket (object ["type" .= ("inspect"::T.Text),"request" .= object
              ["jsonrpc" .= ("2.0"::T.Text),"id" .= (1::Int),"method" .= ("tools/call"::T.Text),
               "params" .= object ["name" .= (name::T.Text),"arguments" .= args]]]))
            receive h
          readRemote=callRemote "read_buffer" (object ["bufferId" .= ident])
          payload response=do
            JsonPacket value<-response
            content<-parseMaybe (withObject "rpc" (\o->o .: "result" >>= withObject "tool result" (.: "content"))) value :: Maybe [Value]
            row<-case content of first:_->Just first; _->Nothing
            text<-parseMaybe (withObject "content" (.: "text")) row
            decodeStrict' (TE.encodeUtf8 text)
          succeeded response=case response of
            Just (JsonPacket (Object fields))->case KM.lookup "result" fields of
              Just result->parseMaybe (withObject "tool result" (.: "isError")) result==Just False
              _->False
            _->False
      flip finally (S.forgetSession session) $ withAsync (runRemoteDaemonWithStartup (pure ()) (awaitPermissionWork owner) session 1 effects (tickPermissions owner) inspect base) $ \daemon->do
        link daemon
        _<-bracket (open (100::Int)) hClose (const (pure ()))
        withLocalPeer session True [] $ \peer->do
          assets<-timeout 3000000 (peerReceive peer)
          check "typed reader frontend receives assets" (case assets of Just (Just (JsonPacket (Object fields)))->KM.lookup "type" fields==Just (String "assets"); _->False)
          check "typed read_buffer progresses through daemon owner with attached frontend" . succeeded =<< readRemote
        check "same typed reader survives frontend detach" . succeeded =<< readRemote
        let patch="@@ -1 +1 @@\n-original\n+daemon edit\n"::T.Text
        edited<-callRemote "buffer_apply_diff" (object ["bufferId" .= ident,"revision" .= (0::Int),"diff" .= patch])
        check "actual typed MCP diff applies after frontend detach" (succeeded edited && (payload edited >>= parseMaybe (withObject "diff" (.: "appliedDiff")))==Just patch)
        current<-readRemote
        check "daemon read observes exact typed diff result" ((payload current >>= parseMaybe (withObject "read" (.: "text")))==Just ("daemon edit\n"::T.Text))
    saved<-newIORef Nothing
    worker<-withPermissionsAt path builtinTools $ \owner->do
      let reader=bufferReader owner (pure (Right ()))
      writeIORef saved (Just (reader,readerReference reader ident))
      pending<-async (P.captureBuffer reader (readerReference reader ident))
      queued pending
      pure pending
    closed<-wait worker
    check "shutdown resolves accepted typed capture" (case closed of Left _->True; _->False)
    Just (reader,reference)<-readIORef saved
    rejected<-P.captureBuffer reader reference
    check "closed service rejects new capture" (case rejected of Left _->True; _->False)
  putStrLn "typed buffer reader checks passed"
