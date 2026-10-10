{-# LANGUAGE OverloadedStrings #-}
module TypedBufferDiffsCheck (checks,startDiffCall) where

import SourceWindowFixture (sourceFixtureBuffer)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception (bracket,onException)
import Control.Monad (unless,forM)
import Data.Aeson
import Data.IORef
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import System.Directory hiding (Permissions)
import System.FilePath ((</>))
import System.IO (hClose,openTempFile)
import System.Timeout (timeout)
import GHC.Conc (threadStatus,ThreadStatus(..),BlockReason(..))
import Hide.BufferRequest (bufferRequestServices)
import Hide.Buffer
import Hide.Files (FileState(..))
import Hide.Model
import Hide.MCPPermissions
import Hide.WorkspaceFilesMCP (fileTools)
import Hide.EditorMCP (builtinTools)
import qualified Hide.BufferTools as BufferTools
import qualified Hide.Plugin.BufferDiff as D
import Hide.Plugin.Request (RequestServices)
import qualified Hide.Plugin.Tool as Tool
import Hide.Plugin.BufferHost (editorReference,captureVersion,versionCurrent)
import qualified Hide.Plugin.Buffer as P
import qualified Hide.Plugin.Command as C

checks :: IO ()
checks=Tool.withTools [] BufferTools.tools $ \toolset->bracket temporary removePathForcibly $ \directory->do
  let specs=builtinTools++Tool.toolDefinitions toolset++fileTools
      config=directory </> "config.toml"
      base=addDocument Nothing (newBuffer "old\n") (initialDesktop (80,25))
      ident=maybe (error "missing typed diff target") sourceFixtureBuffer (activeWindow base)
      patch="@@ -1 +1 @@\n-old\n+agent\n"
      arguments=object ["bufferId" .= ident,"revision" .= (0::Int),"diff" .= patch]
      check label ok=unless ok (error label)
      rejected result=case result of Left _->True; _->False
  TIO.writeFile config "[editor.mcp.permissions]\nbuffer_apply_diff = 'enable'\n"
  check "public diff codec rejects patches above its character bound"
    (rejected (C.codecDecode D.applyInput (object ["bufferId" .= ident,"revision" .= (0::Int),"diff" .= T.replicate 1048577 "x"])))
  withPermissionsAt config specs $ \owner->C.withRegistry $ \registry->do
    let linkedEditor=bufferEditor owner (pure (Right ()))
        linkedReference=editorReference linkedEditor ident
        codec=C.Codec Null (const (Left "typed only")) (const Null)
        reader=bufferReader owner (pure (Right ()))
        commandDef=C.CommandDef "test.diff" "Linked diff" codec codec $ \(sourceReader,ability,target) text->do
          image<-P.captureBuffer sourceReader target
          case image of
            Left err->pure (Left (C.CommandRejected err))
            Right source->fmap (either (Left . C.CommandRejected) Right) (P.applyBufferDiff ability target (P.capturedVersion source) text)
    command<-C.registerCommand registry commandDef >>= either (error . show) pure
    withAsync (C.invoke registry command (reader,linkedEditor,linkedReference) patch) $ \worker->do
      let await current=do
            done<-poll worker
            case done of
              Just (Right value)->pure (current,value)
              Just (Left err)->error (show err)
              Nothing->threadDelay 1000 >> tickPermissions owner current >>= await
      (updated,result)<-timeout 5000000 (await base) >>= maybe (error "typed diff reply timed out") pure
      response<-either (error . show) pure result
      check "internal typed handler applies exact diff without Desktop" (activeText updated=="agent\n" && P.appliedDiff response==patch && not (P.userModified response))
      check "internal typed diff has one ordinary Undo" (length (undoStack (documentBuffer (buffers updated M.! ident)))==1 && activeText (fst (runCommand Undo updated))=="old\n")
    readOnlyContext<-bufferRequestServices reader linkedEditor base "read_buffer" (object []) >>= either (error . T.unpack) pure
    missing<-timeout 5000000 (Tool.callTool toolset readOnlyContext "buffer_apply_diff" arguments)
    check "public diff tool refuses a request without captured diff capability"
      (missing==Just (Left "Diff requires an exact host-captured request."))
    let queued worker=do
          state<-threadStatus (asyncThreadId worker)
          case state of
            ThreadBlocked BlockedOnMVar->pure ()
            ThreadFinished->error "diff completed before admission"
            ThreadDied->error "diff died before admission"
            _->threadDelay 1000 >> queued worker
        editor=bufferEditor owner (pure (Right ()))
        reference=editorReference editor ident
    version<-captureVersion (documentBuffer (buffers base M.! ident))
    captured<-bufferRequestServices reader editor base "buffer_apply_diff" arguments >>= either (error . T.unpack) pure
    substituted<-timeout 5000000 (Tool.callTool toolset captured "buffer_apply_diff"
      (object ["bufferId" .= (ident+1),"revision" .= (0::Int),"diff" .= patch]))
    check "captured diff capability cannot be redirected to another target"
      (substituted==Just (Left "Diff request changed its original target or revision"))
    withAsync (Tool.callTool toolset captured "buffer_apply_diff" arguments) $ \worker->do
      _<-timeout 5000000 (queued worker) >>= maybe (error "diff did not queue") pure
      TIO.writeFile (directory </> "replacement.txt") "old\n"
      replaced<-TIO.readFile (directory </> "replacement.txt")
      let replacement=base {buffers=M.adjust (\doc->doc {documentBuffer=newBuffer replaced}) ident (buffers base)}
      same<-versionCurrent version (documentBuffer (buffers replacement M.! ident))
      check "queue-gap fixture replaces identity with equal numeric revision" (not same && revision (documentBuffer (buffers replacement M.! ident))==0)
      unchanged<-tickPermissions owner replacement
      result<-timeout 5000000 (wait worker) >>= maybe (error "stale diff reply timed out") pure
      check "equal-revision replacement in admission gap rejects public plugin diff" (activeText unchanged=="old\n" && case result of Left _->True; _->False)
    actor<-newIORef (Right ())
    let attributed=bufferEditor owner (readIORef actor)
    withAsync (P.applyBufferDiff attributed reference version patch) $ \worker->do
      _<-timeout 5000000 (queued worker) >>= maybe (error "attributed diff did not queue") pure
      writeIORef actor (Left "actor revoked")
      let await current=do
            done<-poll worker
            case done of
              Just (Right value)->pure (current,value)
              Just (Left err)->error (show err)
              Nothing->threadDelay 1000 >> tickPermissions owner current >>= await
      (unchanged,result)<-timeout 5000000 (await base) >>= maybe (error "revoked diff reply timed out") pure
      check "typed diff rechecks queued actor before source admission" (activeText unchanged=="old\n" && case result of Left "actor revoked"->True; _->False)
  (closed,closedReference,closedVersion)<-withPermissionsAt config specs $ \owner->do
    let editor=bufferEditor owner (pure (Right ()))
    closedVersion<-captureVersion (documentBuffer (buffers base M.! ident))
    pure (editor,editorReference editor ident,closedVersion)
  stopped<-P.applyBufferDiff closed closedReference closedVersion patch
  check "retained editor refuses requests after session shutdown" (case stopped of Left _->True; _->False)
  pending<-withPermissionsAt config specs $ \owner->do
    let editor=bufferEditor owner (pure (Right ()))
        reference=editorReference editor ident
    version<-captureVersion (documentBuffer (buffers base M.! ident))
    forM [1..32::Int] $ \_->do
      worker<-async (P.applyBufferDiff editor reference version patch)
      _<-timeout 5000000 (waitQueued worker) >>= maybe (cancel worker >> error "shutdown diff did not queue") pure
      pure worker
  outcomes<-mapM (timeout 5000000 . wait) pending
  check "shutdown resolves every accepted typed diff reply" (all (\result->case result of Just (Left _)->True; _->False) outcomes)
  retired<-Tool.withTools [] BufferTools.tools pure
  withPermissionsAt config specs $ \owner->do
    captured<-bufferRequestServices (bufferReader owner (pure (Right ())))
      (bufferEditor owner (pure (Right ()))) base "buffer_apply_diff" arguments >>= either (error . T.unpack) pure
    result<-timeout 5000000 (Tool.callTool retired captured "buffer_apply_diff" arguments)
    check "retired plugin tool cannot resurrect a diff request in live service"
      (case result of Just (Left "RegistryClosed")->True; _->False)
  batchChecks specs directory config base ident patch
  putStrLn "typed buffer diff checks passed"
  where
    waitQueued worker=do
      state<-threadStatus (asyncThreadId worker)
      case state of
        ThreadBlocked BlockedOnMVar->pure ()
        ThreadFinished->error "shutdown diff completed before admission"
        ThreadDied->error "shutdown diff died before admission"
        _->threadDelay 1000 >> waitQueued worker
    temporary=do root<-getTemporaryDirectory; (path,h)<-openTempFile root "hide-typed-diff"; hClose h; removeFile path; createDirectory path; pure path

-- The same internal owner handles singleton and multi-target edits. These checks
-- observe target text/history and terminal replies, never permission internals.
batchChecks :: [Value] -> FilePath -> FilePath -> Desktop -> Int -> T.Text -> IO ()
batchChecks specs directory config single first firstPatch=do
  let opened=addDocument (Just (FileState (directory </> "second.hs") Nothing)) (newBuffer "other\n") single
      second=maybe (error "missing second batch target") sourceFixtureBuffer (activeWindow opened)
      base=opened {windows=map (\w->w {windowHexLow=True}) (windows opened)}
      secondPatch="@@ -1 +1 @@\n-other\n+second\n"
      reviewed="@@ -1 +1 @@\n-old\n+human λ\n"
      buffer bid d=documentBuffer (buffers d M.! bid)
      original d=contents (buffer first d)=="old\n" && contents (buffer second d)=="other\n" &&
        all (\bid->revision (buffer bid d)==0 && null (undoStack (buffer bid d))) [first,second]
      check label ok=unless ok (error label)
      isLeft (Left _)=True
      isLeft _=False
      inputs editor=do
        a<-captureVersion (buffer first base)
        b<-captureVersion (buffer second base)
        pure [P.BufferDiff (editorReference editor second) b secondPatch,P.BufferDiff (editorReference editor first) a firstPatch]
      await owner worker start=timeout 5000000 (go start) >>= maybe (error "batch reply timed out") pure
        where go current=do
                done<-poll worker
                case done of
                  Just (Right result)->pure (current,result)
                  Just (Left err)->error (show err)
                  Nothing->threadDelay 1000 >> tickPermissions owner current >>= go
      untilDesktop owner predicate start=timeout 5000000 (go start) >>= maybe (error "batch review timed out") pure
        where go current=do
                next<-tickPermissions owner current
                if predicate next then pure next else threadDelay 1000 >> go next
      shown owner=untilDesktop owner (maybe False (const True) . dialog) base
      submit owner button d=case dialog d of
        Just dg->let (next,effects)=submitDialog button dg d in snd <$> policyEffects owner (\current _->pure (False,current)) next effects
        Nothing->error "missing batch approval"
      target bid d=case find ((==Just bid) . bufferId) (windows d) of
        Just w->focusWindow (windowId w) d
        Nothing->error "missing batch target window"
      enqueue worker=timeout 5000000 (waitQueued worker) >>= maybe (error "batch did not queue") pure
  withPermissionsAt config specs $ \owner->do
    let editor=bufferEditor owner (pure (Right ()))
    patches<-inputs editor
    withAsync (P.applyBufferDiffs editor patches) $ \worker->do
      (updated,result)<-await owner worker base
      results<-either (error . T.unpack) pure result
      check "batch results preserve input order" (map P.appliedDiff results==[secondPatch,firstPatch] && map P.diffRevision results==[1,1] && all (not . P.userModified) results)
      check "batch commits all targets and clears their hex nibble state" (contents (buffer first updated)=="agent\n" && contents (buffer second updated)=="second\n" && all (not . windowHexLow) (windows updated))
      let undoSecond=fst (runCommand Undo (target second updated))
          undoFirst=fst (runCommand Undo (target first undoSecond))
      check "batch gives each target one independent ordinary Undo" (all (\bid->length (undoStack (buffer bid updated))==1) [first,second] && contents (buffer first undoSecond)=="agent\n" && contents (buffer second undoSecond)=="other\n" && contents (buffer first undoFirst)=="old\n")
    case patches of
      a:_->do
        duplicate<-P.applyBufferDiffs editor [a,a]
        after<-tickPermissions owner base
        check "duplicate batch targets reject without edits" (isLeft duplicate && original after)
      _->error "empty batch fixture"
    foreignReference<-withPermissionsAt config specs $ \other->pure (editorReference (bufferEditor other (pure (Right ()))) first)
    version<-captureVersion (buffer first base)
    case patches of
      a:_->withAsync (P.applyBufferDiffs editor [a,P.BufferDiff foreignReference version firstPatch]) $ \worker->do
        (unchanged,result)<-await owner worker base
        check "one foreign-session target rejects the entire batch" (isLeft result && original unchanged)
      _->error "empty batch fixture"
    case patches of
      [a,P.BufferDiff reference expected _]->withAsync (P.applyBufferDiffs editor [a,P.BufferDiff reference expected "not a diff"]) $ \worker->do
        (unchanged,result)<-await owner worker base
        check "one invalid strict patch cannot partially commit a batch" (isLeft result && original unchanged)
      _->error "unexpected batch fixture"
  TIO.writeFile config "[editor.mcp.permissions]\nbuffer_apply_diff = 'prompt'\n"
  withPermissionsAt config specs $ \owner->do
    let editor=bufferEditor owner (pure (Right ()))
    patches<-inputs editor
    withAsync (P.applyBufferDiffs editor patches) $ \worker->do
      enqueue worker
      review<-shown owner
      let areas=[(i,contents b) | Just dg<-[dialog review],(i,TextArea _ True b _ _ _)<-zip [0..] (fields dg)]
      check "one batch ticket has a human editable diff per target" (map snd areas==[secondPatch,firstPatch] && original review)
      focused<-case (dialog review,areas) of
        (Just dg,[_,(i,_)])->pure (fst (moveDialogFocus (i-focus dg) review))
        _->error "missing second editable batch diff"
      let selected=fst (applyDialogCommand SelectAll focused)
          (edited,effects)=handleEvent (V.EvPaste (TE.encodeUtf8 reviewed)) selected
      draft<-snd <$> policyEffects owner (\current _->pure (False,current)) edited effects
      check "editing a batch approval retains both original target buffers" (original draft)
      started<-submit owner 0 draft
      (updated,result)<-await owner worker started
      results<-either (error . T.unpack) pure result
      check "one approval atomically applies exact edited diffs and ordered metadata" (contents (buffer first updated)=="human λ\n" && contents (buffer second updated)=="second\n" && map P.appliedDiff results==[secondPatch,reviewed] && map P.userModified results==[False,True] && dialog updated==Nothing)
    withAsync (P.applyBufferDiffs editor patches) $ \worker->do
      enqueue worker
      review<-shown owner
      let changed=review {buffers=M.adjust (\doc->doc {documentBuffer=replaceBuffer False "newer\n" (documentBuffer doc)}) second (buffers review)}
      started<-submit owner 0 changed
      refused<-untilDesktop owner (T.isPrefixOf "Diff not applied:" . status) started
      check "one stale target prevents every other batch edit" (contents (buffer first refused)=="old\n" && null (undoStack (buffer first refused)) && contents (buffer second refused)=="newer\n" && dialog refused/=Nothing)
      _<-submit owner 1 refused
      check "denial after stale batch resolves the same request" . isLeft =<< wait worker
    withAsync (P.applyBufferDiffs editor patches) $ \worker->do
      enqueue worker
      review<-shown owner
      started<-submit owner 0 review {guestPrivatePaths=[directory </> "second.hs"]}
      refused<-untilDesktop owner (T.isPrefixOf "Diff not applied:" . status) started
      check "one newly private target prevents every batch edit" (original refused && dialog refused/=Nothing)
      _<-submit owner 1 refused
      check "denial after private batch resolves the same request" . isLeft =<< wait worker
    withAsync (P.applyBufferDiffs editor patches) $ \worker->do
      enqueue worker
      review<-shown owner
      cancel worker
      unchanged<-tickPermissions owner review
      check "cancelled batch approval cannot commit on a later tick" (original unchanged)
    actor<-newIORef (Right ())
    let attributed=bufferEditor owner (readIORef actor)
    withAsync (P.applyBufferDiffs attributed patches) $ \worker->do
      enqueue worker
      review<-shown owner
      writeIORef actor (Left "actor revoked")
      started<-submit owner 0 review
      (unchanged,result)<-await owner worker started
      check "revocation after batch review rejects every target" (original unchanged && case result of Left "actor revoked"->True; _->False)
  pending<-withPermissionsAt config specs $ \owner->do
    let editor=bufferEditor owner (pure (Right ()))
    patches<-inputs editor
    worker<-async (P.applyBufferDiffs editor patches)
    enqueue worker `onException` cancel worker
    pure worker
  stopped<-timeout 5000000 (wait pending) >>= maybe (cancel pending >> error "batch shutdown reply timed out") pure
  check "shutdown resolves an accepted multi-target request" (isLeft stopped)
  where
    waitQueued worker=do
      state<-threadStatus (asyncThreadId worker)
      case state of
        ThreadBlocked BlockedOnMVar->pure ()
        ThreadFinished->error "batch completed before admission"
        ThreadDied->error "batch died before admission"
        _->threadDelay 1000 >> waitQueued worker

-- Existing owner checks also exercise the actual asynchronous MCP command route.
-- Start its one reply worker, then run one admission tick; later waits reuse that
-- same test worker rather than issuing a second diff request.
startDiffCall :: Tool.Tools RequestServices -> Permissions -> IO (Either T.Text ()) -> Desktop -> T.Text -> Value -> IO (Desktop,IO (Either T.Text Value))
startDiffCall toolset owner caller desktop name args=do
  captured<-bufferRequestServices (bufferReader owner caller) (bufferEditor owner caller) desktop name args
  let response=either (pure . Left) (\context->Tool.callTool toolset context name args) captured
  worker<-async response
  let queued=do
        state<-threadStatus (asyncThreadId worker)
        case state of
          ThreadBlocked BlockedOnMVar->pure ()
          ThreadFinished->pure ()
          ThreadDied->pure ()
          _->threadDelay 1000 >> queued
  _<-timeout 5000000 queued >>= maybe (cancel worker >> error "diff reply did not queue") pure
  let advance d=do
        completed<-race (STM.atomically (awaitPermissionWork owner)) (waitCatch worker)
        case completed of Left ()->tickPermissions owner d; Right _->pure d
  admitted<-timeout 5000000 (advance desktop >>= advance) >>= maybe (cancel worker >> error "diff policy admission did not settle") pure
  pure (admitted,(waitCatch worker >>= either (pure . Left . T.pack . show) pure) `onException` cancel worker)
