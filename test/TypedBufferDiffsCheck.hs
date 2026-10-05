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
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory hiding (Permissions)
import System.FilePath ((</>))
import System.IO (hClose,openTempFile)
import System.Timeout (timeout)
import GHC.Conc (threadStatus,ThreadStatus(..),BlockReason(..))
import Hide.BufferDiffCommand
import Hide.Buffer
import Hide.Model
import Hide.MCPPermissions
import Hide.WorkspaceFilesMCP (fileTools)
import Hide.EditorMCP (builtinTools)
import Hide.Plugin.BufferHost (editorReference,captureVersion,versionCurrent)
import qualified Hide.Plugin.Buffer as P
import qualified Hide.Plugin.Command as C

checks :: IO ()
checks=bracket temporary removePathForcibly $ \directory->do
  let config=directory </> "config.toml"
      base=addDocument Nothing (newBuffer "old\n") (initialDesktop (80,25))
      ident=maybe (error "missing typed diff target") sourceFixtureBuffer (activeWindow base)
      patch="@@ -1 +1 @@\n-old\n+agent\n"
      check label ok=unless ok (error label)
  TIO.writeFile config "[editor.mcp.permissions]\nbuffer_apply_diff = 'enable'\n"
  withPermissionsAt config (builtinTools++fileTools) $ \owner->C.withRegistry $ \registry->do
    let editor=bufferEditor owner (pure (Right ()))
        reference=editorReference editor ident
        codec=C.Codec Null (const (Left "typed only")) (const Null)
        reader=bufferReader owner (pure (Right ()))
        commandDef=C.CommandDef "test.diff" "Linked diff" codec codec $ \(reader,ability,target) text->do
          image<-P.captureBuffer reader target
          case image of
            Left err->pure (Left (C.CommandRejected err))
            Right source->fmap (either (Left . C.CommandRejected) Right) (P.applyBufferDiff ability target (P.capturedVersion source) text)
    command<-C.registerCommand registry commandDef >>= either (error . show) pure
    withAsync (C.invoke registry command (reader,editor,reference) patch) $ \worker->do
      let await current=do
            done<-poll worker
            case done of
              Just (Right value)->pure (current,value)
              Just (Left err)->error (show err)
              Nothing->threadDelay 1000 >> tickPermissions owner current >>= await
      (updated,result)<-timeout 5000000 (await base) >>= maybe (error "typed diff reply timed out") pure
      response<-either (error . show) pure result
      check "linked typed handler applies exact diff without Desktop" (activeText updated=="agent\n" && P.appliedDiff response==patch && not (P.userModified response))
      check "linked typed diff has one ordinary Undo" (length (undoStack (documentBuffer (buffers updated M.! ident)))==1 && activeText (fst (runCommand Undo updated))=="old\n")
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
    withAsync (P.applyBufferDiff editor reference version patch) $ \worker->do
      _<-timeout 5000000 (queued worker) >>= maybe (error "diff did not queue") pure
      TIO.writeFile (directory </> "replacement.txt") "old\n"
      replaced<-TIO.readFile (directory </> "replacement.txt")
      let replacement=base {buffers=M.adjust (\doc->doc {documentBuffer=newBuffer replaced}) ident (buffers base)}
      same<-versionCurrent version (documentBuffer (buffers replacement M.! ident))
      check "queue-gap fixture replaces identity with equal numeric revision" (not same && revision (documentBuffer (buffers replacement M.! ident))==0)
      unchanged<-tickPermissions owner replacement
      result<-timeout 5000000 (wait worker) >>= maybe (error "stale diff reply timed out") pure
      check "equal-revision replacement in admission gap rejects typed diff" (activeText unchanged=="old\n" && case result of Left _->True; _->False)
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
  (closed,reference,version)<-withPermissionsAt config fileTools $ \owner->do
    let editor=bufferEditor owner (pure (Right ()))
    version<-captureVersion (documentBuffer (buffers base M.! ident))
    pure (editor,editorReference editor ident,version)
  stopped<-P.applyBufferDiff closed reference version patch
  check "retained editor refuses requests after session shutdown" (case stopped of Left _->True; _->False)
  pending<-withPermissionsAt config fileTools $ \owner->do
    let editor=bufferEditor owner (pure (Right ()))
        reference=editorReference editor ident
    version<-captureVersion (documentBuffer (buffers base M.! ident))
    forM [1..32::Int] $ \_->do
      worker<-async (P.applyBufferDiff editor reference version patch)
      _<-timeout 5000000 (waitQueued worker) >>= maybe (cancel worker >> error "shutdown diff did not queue") pure
      pure worker
  outcomes<-mapM (timeout 5000000 . wait) pending
  check "shutdown resolves every accepted typed diff reply" (all (\result->case result of Just (Left _)->True; _->False) outcomes)
  retired<-withBufferDiffCommands pure
  withPermissionsAt config fileTools $ \owner->do
    let editor=bufferEditor owner (pure (Right ()))
        reference=editorReference editor ident
    version<-captureVersion (documentBuffer (buffers base M.! ident))
    rejected<-timeout 5000000 (bufferDiffCommand retired editor reference version patch)
    check "retired command cannot resurrect a diff request in live service" (case rejected of Just (Left _)->True; _->False)
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

-- Existing owner checks also exercise the actual asynchronous MCP command route.
-- Start its one reply worker, then run one admission tick; later waits reuse that
-- same test worker rather than issuing a second diff request.
startDiffCall :: BufferDiffCommands -> Permissions -> IO (Either T.Text ()) -> Desktop -> T.Text -> Value -> IO (Desktop,IO (Either T.Text Value))
startDiffCall commands owner caller desktop name args=do
  (current,response)<-bufferDiffTool commands (bufferEditor owner caller) desktop name args
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
  admitted<-timeout 5000000 (advance current >>= advance) >>= maybe (cancel worker >> error "diff policy admission did not settle") pure
  pure (admitted,(waitCatch worker >>= either (pure . Left . T.pack . show) pure) `onException` cancel worker)
