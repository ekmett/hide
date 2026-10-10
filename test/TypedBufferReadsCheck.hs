{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
module TypedBufferReadsCheck (checks) where
import AllocationProfile (AllocationProfile, withinBudget)

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Concurrent (threadDelay,newEmptyMVar,putMVar,takeMVar)
import Control.Concurrent.Async hiding (wait)
import qualified Control.Concurrent.Async
import Control.Exception (bracket,try,evaluate,SomeException,IOException,catch,throwIO,finally)
import Control.Monad (unless,forM)
import Data.IORef
import Data.List (find)
import Hide.Files (FileState(..))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Conc (getAllocationCounter,threadStatus,ThreadStatus(..),BlockReason(..))
import System.Directory
import System.FilePath
import System.IO (hClose,openTempFile)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import Hide.Protocol
import Hide.Remote
import Hide.RemoteEndpoint
import qualified Hide.Session as S
import Hide.BufferReadServices (bufferReadServices,windowPage)
import qualified Hide.Plugin.WindowRead as WR
import Hide.BufferReads (windowReadTarget,capturedWindowPrepared)
import qualified Hide.Plugin.Window as W
import qualified Data.Vector as V
import Hide.Syntax (Style(..))
import Hide.BufferRequest (bufferRequestServices)
import Hide.WorkspaceFilesMCP (fileTools)
import qualified Data.Text.Encoding as TE
import System.Timeout (timeout)
import Hide.Buffer
import Hide.Model
import Hide.MCPPermissions
import qualified Hide.Plugin.Buffer as P
import Hide.Plugin.BufferHost (readerReference)
import Hide.EditorMCP (builtinTools,editorResponseOnly)
import qualified Hide.BufferTools as BufferTools
import qualified Hide.Plugin.BufferRead as R
import Hide.Plugin.Request (RequestServices(..))
import qualified Hide.Plugin.Tool as Tool
import Hide.Plugin.Command (codecDecode,codecEncode)

ownerUntil :: Hide.MCPPermissions.Permissions -> Desktop -> Async a -> IO Desktop
ownerUntil owner desktop worker=do
  let loop current=do
        result<-poll worker
        case result of
          Just _->pure current
          Nothing->threadDelay 1000 >> tickPermissions owner current >>= loop
  timeout 3000000 (loop desktop) >>= maybe (error "typed read owner did not settle") pure

checks :: AllocationProfile -> IO ()
checks profile=Tool.withTools [] BufferTools.tools $ \toolset->do
  let specs=builtinTools++Tool.toolDefinitions toolset
  let temporary=do
        directory<-getTemporaryDirectory
        (path,h)<-openTempFile directory "hide-typed-buffer-read-check"
        hClose h
        removeFile path
        createDirectory path
        pure path
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
  bracket temporary removePathForcibly $ \root->do
    let path=root </> "config.toml"
    listingChecks toolset specs path
    windowReadChecks toolset specs path
    TIO.writeFile path "[editor.mcp.permissions]\nread_buffer = 'enable'\n"
    withPermissionsAt path specs $ \owner->do
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
      context<-evaluate (RequestServices (bufferReadServices reader (activeWindow base >>= bufferId) (nextId base)) Nothing Nothing)
      let focusedElsewhere=addDocument Nothing (newBuffer "later source") base
      withAsync (Tool.callTool toolset context "read_buffer" (object [])) $ \worker->do
        queued worker
        _<-ownerUntil owner focusedElsewhere worker
        reply<-wait worker >>= either (error . T.unpack) pure
        check "public read keeps the source selected at dispatch"
          (parseMaybe (withObject "read" (.: "text")) reply==Just ("original\n"::T.Text))
      later<-Tool.callTool toolset context "read_buffer" (object ["bufferId" .= nextId base])
      check "public read cannot target a buffer allocated after dispatch"
        (case later of Left "Buffer not found"->True; _->False)
      invalid<-Tool.callTool toolset context "read_buffer" (object ["lineCount" .= (1001::Int)])
      check "public tool rejects invalid paging before enqueue" (case invalid of Left _->True; _->False)
      -- A saved document needs its mode-switched baseline comparison; an
      -- untitled seed already has a save obligation without comparing text.
      let switched=base {buffers=M.adjust (\doc->doc {documentFile=Just (FileState (root </> "existing.bin") Nothing),
            documentBuffer=(newBuffer "byte text\n") {byteMode=True,saved=error "dirty comparison reached saved text",undoStack=error "capture retained Undo evaluation"}}) ident (buffers base)}
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
        check "typed admission does not encode mode-switched full text" (withinBudget profile (before-after) (2000000))
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
    accepted<-withPermissionsAt path specs $ \owner->do
      let reader=bufferReader owner (pure (Right ()))
          reference=readerReference reader ident
      workers<-forM [1..32::Int] $ \index->do
        worker<-async (if even index then (() <$) <$> P.captureBuffer reader reference else (() <$) <$> P.listBuffers reader)
        queued worker
        pure worker
      overflow<-timeout 3000000 (P.captureBuffer reader reference)
      check "full ingress refuses explicitly without blocking" (case overflow of Just (Left _)->True; _->False)
      pure workers
    outcomes<-mapM wait accepted
    check "all accepted ingress replies survive shutdown" (all (either (const True) (const False)) outcomes)
    TIO.writeFile path "[editor.mcp.permissions]\nread_buffer = 'enable'\nbuffer_apply_diff = 'enable'\n"
    withPermissionsAt path (specs++fileTools) $ \owner->do
      session<-randomIdentity
      endpoint<-sessionEndpoint session
      let reader=bufferReader owner (pure (Right ()))
          editor=bufferEditor owner (pure (Right ()))
          inspect d _ request=do
            let dispatch current name args=do
                  captured<-bufferRequestServices reader editor (windowReader owner (pure (Right ()))) current name args
                  pure (current,either (pure . Left) (\context->Tool.callTool toolset context name args) captured)
            (next,reply)<-editorResponseOnly (specs++fileTools) dispatch d request
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
            encodedText<-parseMaybe (withObject "content" (.: "text")) row
            decodeStrict' (TE.encodeUtf8 encodedText)
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
        listing<-callRemote "list_buffers" (object [])
        check "actual typed list_buffers survives frontend detach with unchanged metadata"
          (succeeded listing && payload listing==Just (codecEncode BufferTools.listOutput [R.BufferMetadata ident "Untitled" Nothing True False 0]))
        let patch="@@ -1 +1 @@\n-original\n+daemon edit\n"::T.Text
        edited<-callRemote "buffer_apply_diff" (object ["bufferId" .= ident,"revision" .= (0::Int),"diff" .= patch])
        check "actual typed MCP diff applies after frontend detach" (succeeded edited && (payload edited >>= parseMaybe (withObject "diff" (.: "appliedDiff")))==Just patch)
        current<-readRemote
        check "daemon read observes exact typed diff result" ((payload current >>= parseMaybe (withObject "read" (.: "text")))==Just ("daemon edit\n"::T.Text))
    saved<-newIORef Nothing
    worker<-withPermissionsAt path specs $ \owner->do
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
    noListing<-P.listBuffers reader
    check "closed service rejects new listing" (case noListing of Left _->True; _->False)
  putStrLn "typed buffer reader checks passed"

-- Discovery preserves the established projection but returns usable scoped refs.
listingChecks :: Tool.Tools RequestServices -> [Value] -> FilePath -> IO ()
listingChecks toolset specs path=do
  let check label ok=unless ok (error label)
      base=addDocument Nothing (newBuffer "untitled source") (initialDesktop (80,25))
      ident=maybe (error "missing listing source") sourceFixtureBuffer (activeWindow base)
      public=addDocument (Just (FileState "/project/Public.hs" Nothing)) (newByteBuffer "binary") base
      privatePath=takeDirectory path </> "authority.json"
      protectedDocument=addDocument (Just (FileState privatePath Nothing)) (newBuffer "private source") public
      current=protectedDocument {guestPrivatePaths=[privatePath],buffers=M.adjust
        (\doc->doc {documentLabel=Just "Current name"}) ident (buffers protectedDocument)}
      queued worker=do
        let observe=threadStatus (asyncThreadId worker) >>= \state->case state of
              ThreadBlocked BlockedOnMVar->pure ()
              ThreadFinished->error "listing finished before owner admission"
              ThreadDied->error "listing died before owner admission"
              _->threadDelay 1000 >> observe
        timeout 3000000 observe >>= maybe (error "listing did not enqueue") pure
      wait worker=timeout 3000000 (Control.Concurrent.Async.wait worker) >>= maybe (error "listing reply timed out") pure
      right value=either (error . T.unpack) pure value
      rejected result=case result of Left _->True; _->False
  TIO.writeFile path "[editor.mcp.permissions]\nlist_buffers = 'enable'\nread_buffer = 'enable'\n"
  withPermissionsAt path specs $ \owner->do
    let reader=bufferReader owner (pure (Right ()))
        opaque=current {buffers=M.map (\doc->doc {documentBuffer=(documentBuffer doc)
          {saved=error "listing forced saved source",undoStack=error "listing forced Undo",redoStack=error "listing forced Redo"},
          documentHighlight=error "listing forced highlights"}) (buffers current)}
    withAsync (P.listBuffers reader) $ \worker->do
      queued worker
      _<-ownerUntil owner opaque worker
      entries<-wait worker >>= right
      let metadata=map P.listedMetadata entries
      check "listing follows current host ID order and labels"
        (map P.bufferIdentifier metadata==M.keys (buffers current) && any ((=="Current name") . P.displayName) metadata)
      check "listing masks private names and paths without omitting references"
        (length entries==3 && any (\info->P.displayName info=="[private]" && P.path info==Nothing) metadata
          && all ((/=Just privatePath) . P.path) metadata)
      check "listing metadata does not read saved text or history" (all (not . P.modified) metadata)
      reference<-maybe (error "listing omitted live source") (pure . P.listedRef)
        (find ((==ident) . P.bufferIdentifier . P.listedMetadata) entries)
      withAsync (P.captureBuffer reader reference) $ \capture->do
        queued capture
        _<-ownerUntil owner current capture
        image<-wait capture >>= right
        check "listed opaque refs work through separate capture admission"
          (P.readText (P.capturedContent image) (P.TextRange (P.CharOffset 0) (P.CharOffset 15))==Right "untitled source")
      withAsync (P.captureBuffer reader reference) $ \capture->do
        queued capture
        _<-ownerUntil owner current {buffers=M.delete ident (buffers current)} capture
        check "listing does not keep a closed target readable" . rejected =<< wait capture
      withPermissionsAt path specs $ \other->withAsync (P.captureBuffer (bufferReader other (pure (Right ()))) reference) $ \capture->do
        queued capture
        _<-ownerUntil other current capture
        check "listed refs cannot cross session namespaces" . rejected =<< wait capture
    let reply=Tool.callTool toolset (RequestServices (R.BufferReadServices
          (R.bufferList (bufferReadServices reader Nothing 0))
          (error "listing evaluated read-only context")) Nothing Nothing) "list_buffers" (object [])
    withAsync reply $ \worker->do
      queued worker
      _<-ownerUntil owner current worker
      result<-wait worker
      check ("actual typed listing matches public metadata: "++show result)
        (result==Right (codecEncode BufferTools.listOutput
          [R.BufferMetadata ident "Current name" Nothing False False 0,
           R.BufferMetadata (ident+1) "/project/Public.hs" (Just "/project/Public.hs") False True 0,
           R.BufferMetadata (ident+2) "[private]" Nothing False False 0]))
    actor<-newIORef (Right ())
    withAsync (P.listBuffers (bufferReader owner (readIORef actor))) $ \worker->do
      queued worker
      writeIORef actor (Left "listing actor revoked")
      _<-ownerUntil owner current worker
      check "listing rechecks the queued actor" . rejected =<< wait worker
    withAsync (P.listBuffers reader) $ \worker->do
      queued worker
      cancel worker
      _<-tickPermissions owner current
      check "listing cancellation withdraws acceptance" . either (const True) (const False) =<< waitCatch worker
    TIO.writeFile path "[editor.mcp.permissions]\nlist_buffers = 'disable'\nread_buffer = 'enable'\n"
    withAsync (P.listBuffers reader) $ \worker->do
      queued worker
      _<-ownerUntil owner current worker
      check "listing uses its own current permission policy" . rejected =<< wait worker
    TIO.writeFile path "[editor.mcp.permissions]\nlist_buffers = 'enable'\nread_buffer = 'enable'\n"
    let switched=current {buffers=M.adjust (\doc->doc {documentBuffer=(documentBuffer doc)
          {byteMode=True,saved=error "listing dirty comparison ran under owner"}}) ident (buffers current)}
    withAsync (P.listBuffers reader) $ \worker->do
      queued worker
      _<-ownerUntil owner switched worker
      entries<-wait worker >>= right
      info<-maybe (error "mode-switched listing missing") (pure . P.listedMetadata)
        (find ((==ident) . P.bufferIdentifier . P.listedMetadata) entries)
      outcome<-try (evaluate (P.modified info)) :: IO (Either SomeException Bool)
      check "listing defers exceptional dirty comparison to consumer worker" (case outcome of Left _->True; _->False)
    retired<-Tool.withTools [] BufferTools.tools pure
    check "closed tool registry refuses listing before enqueue" . rejected =<<
      Tool.callTool retired (RequestServices (bufferReadServices reader Nothing 0) Nothing Nothing) "list_buffers" (object [])

-- One actual prepared-window read workflow, sharing the existing admission pump.
windowReadChecks :: Tool.Tools RequestServices -> [Value] -> FilePath -> IO ()
windowReadChecks toolset specs path=W.withWindowScope $ \scope->do
  let check label ok=unless ok (error label)
      semantics=W.TextSemantics W.CopyText Nothing V.empty V.empty W.ReadableWindow
        (V.singleton (7,13)) V.empty V.empty
      prepare contentsText=W.prepareSemanticTextWindow "Transcript" [(contentsText,Plain)] semantics >>= either (error . T.unpack) pure
      rejected result=case result of Left _->True; _->False
      queued worker=do
        let observe=threadStatus (asyncThreadId worker) >>= \state->case state of
              ThreadBlocked BlockedOnMVar->pure ()
              ThreadFinished->error "window reader finished before owner capture"
              ThreadDied->error "window reader died before owner capture"
              _->threadDelay 1000 >> observe
        timeout 3000000 observe >>= maybe (error "window read did not enqueue") pure
      wait worker=timeout 3000000 (Control.Concurrent.Async.wait worker) >>= maybe (error "window read timed out") pure
      text result=result >>= parseMaybe (withObject "read" (.: "text"))
      call context arguments=Tool.callTool toolset context "read_window" arguments
  prepared<-prepare "public\nsecret\nvisible"
  update<-W.openWindow scope prepared >>= maybe (error "window read opening failed") pure
  (reference,_)<-W.admitWindowUpdate False update >>= maybe (error "window read admission failed") pure
  let base=addPluginWindow reference prepared (initialDesktop (80,25))
      ident=maybe (error "window read frame missing") windowId (activeWindow base)
      arguments=object ["windowId" .= ident]
      closed=base {windows=[],pluginWindows=M.empty}
  target<-either (error . T.unpack) pure (windowReadTarget base ident)
  pageArguments<-either (error . T.unpack) pure (WR.readArguments (Just ident) 1 200)
  check "window request codec preserves checked coordinates"
    (codecDecode WR.readInput (codecEncode WR.readInput pageArguments)==Right pageArguments)
  check "window request codec defaults the captured active target"
    (codecDecode WR.readInput (object [])==WR.readArguments Nothing 1 200)
  check "window request codec rejects unknown fields and unbounded coordinates"
    (all (rejected . codecDecode WR.readInput)
      [object ["byteOffset" .= (0::Int)],object ["startLine" .= (0::Int)],
       object ["lineCount" .= (1001::Int)],object ["windowId" .= (0.5::Double)]])
  privateBody<-W.prepareTextWindow "Private metadata" "private text"
  let private=base {pluginWindows=M.singleton reference privateBody}
  check "private prepared window refuses read selection" (rejected (windowReadTarget private ident))
  TIO.writeFile path "[editor.mcp.permissions]\nread_buffer = 'enable'\nread_window = 'enable'\n"
  withPermissionsAt path specs $ \owner->do
    let reader=windowReader owner (pure (Right ()))
        contextFor caller desktop args=bufferRequestServices (bufferReader owner caller)
          (bufferEditor owner caller) (windowReader owner caller) desktop "read_window" args
          >>= either (error . T.unpack) pure
    context<-contextFor (pure (Right ())) base arguments
    readOnlyContext<-bufferRequestServices (bufferReader owner (pure (Right ())))
      (bufferEditor owner (pure (Right ()))) reader base "read_buffer" (object [])
      >>= either (error . T.unpack) pure
    missing<-call readOnlyContext arguments
    check "public window tool requires an exact captured capability"
      (missing==Left "Window read requires an exact host-captured request.")
    substituted<-call context (object ["windowId" .= (ident+1)])
    check "captured window capability rejects target substitution"
      (substituted==Left "Window read changed its original target")
    denied<-bufferRequestServices (bufferReader owner (pure (Right ())))
      (bufferEditor owner (pure (Right ()))) reader private "read_window" arguments
    check "public window preflight retains private-body refusal" (rejected denied)
    withAsync (call context arguments) $ \worker->do
      queued worker
      _<-ownerUntil owner base worker
      result<-wait worker >>= either (error . T.unpack) pure
      check "typed window page uses central guest masking" (text (Just result)==Just ("public\n      \nvisible"::T.Text))
      check "typed window read has window coordinates without buffer identity" $ case result of
        Object fields->case KM.lookup "window" fields of
          Just (Object meta)->KM.lookup "windowId" meta==Just (toJSON ident) && KM.lookup "coordinateSpace" meta==Just (String "window-text") && not (KM.member "bufferId" meta)
          _->False
        _->False
      page<-either (error . T.unpack) pure (codecDecode BufferTools.windowOutput result)
      check "public window output codec round-trips the admitted page"
        (codecDecode BufferTools.windowOutput (codecEncode BufferTools.windowOutput page)==Right page)
      check "public window output codec enforces the text budget"
        (rejected (codecDecode BufferTools.windowOutput (codecEncode BufferTools.windowOutput
          (page {WR.windowPage=(WR.windowPage page) {R.pageText=T.replicate 131073 "x"}}))))
      check "public window output codec rejects extra fields" (case result of
        Object fields->rejected (codecDecode BufferTools.windowOutput (Object (KM.insert "bufferId" (toJSON ident) fields)))
        _->False)
    defaultContext<-contextFor (pure (Right ())) base (object [])
    let focusedElsewhere=addDocument Nothing (newBuffer "later source") base
    withAsync (call defaultContext (object [])) $ \worker->do
      queued worker
      _<-ownerUntil owner focusedElsewhere worker
      result<-wait worker >>= either (error . T.unpack) pure
      check "default window read keeps the target selected before dispatch"
        (text (Just result)==Just ("public\n      \nvisible"::T.Text))
    withAsync (reader target) $ \worker->do
      queued worker
      _<-ownerUntil owner base worker
      image<-wait worker >>= either (error . T.unpack) pure
      check "captured window retains exact prepared identity" (capturedWindowPrepared image==prepared)
      check "closed frame rejects a new target" (rejected (windowReadTarget closed ident))
      page<-windowPage image pageArguments >>= either (error . T.unpack) pure
      check "accepted immutable window snapshot formats after close"
        (text (Just (codecEncode BufferTools.windowOutput page))==Just ("public\n      \nvisible"::T.Text))
    refreshed<-prepare "public\nsecret\nnew body"
    refresh<-W.refreshWindow reference refreshed >>= maybe (error "window read refresh failed") pure
    _<-W.admitWindowUpdate True refresh >>= maybe (error "window read refresh admission failed") pure
    let current=base {pluginWindows=M.singleton reference refreshed}
    withAsync (call context arguments) $ \worker->do
      queued worker
      _<-ownerUntil owner current worker
      check "queued public window read refuses prepared refresh" . rejected =<< wait worker
    withAsync (call context arguments) $ \worker->do
      queued worker
      _<-ownerUntil owner closed worker
      check "queued public window read refuses closed frame" . rejected =<< wait worker
    actor<-newIORef (Right ())
    attributed<-contextFor (readIORef actor) base arguments
    withAsync (call attributed arguments) $ \worker->do
      queued worker
      writeIORef actor (Left "actor revoked")
      _<-ownerUntil owner base worker
      outcome<-wait worker
      check "public window read rechecks the captured caller" (outcome==Left "actor revoked")
    withAsync (call context arguments) $ \worker->do
      queued worker
      cancel worker
      _<-tickPermissions owner base
      check "public window cancellation resolves its capture claim" . either (const True) (const False) =<< waitCatch worker
    retired<-Tool.withTools [] BufferTools.tools pure
    check "retired plugin registry refuses window reads before enqueue" . rejected =<<
      Tool.callTool retired context "read_window" arguments
    W.retireWindowRef reference
    withAsync (call context arguments) $ \worker->do
      queued worker
      _<-ownerUntil owner base worker
      check "inert installed readable text survives action retirement" . either (const False) (const True) =<< wait worker
  (context,pending)<-withPermissionsAt path specs $ \owner->do
    context<-bufferRequestServices (bufferReader owner (pure (Right ())))
      (bufferEditor owner (pure (Right ()))) (windowReader owner (pure (Right ()))) base "read_window" arguments
      >>= either (error . T.unpack) pure
    pending<-async (call context arguments)
    queued pending
    pure (context,pending)
  check "shutdown resolves accepted public window read" . rejected =<< wait pending
  check "closed service refuses new public window read" . rejected =<< call context arguments
