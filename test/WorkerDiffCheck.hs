{-# LANGUAGE OverloadedStrings #-}
module WorkerDiffCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Exception (bracket,evaluate)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Conc (getAllocationCounter,threadStatus,ThreadStatus(..),BlockReason(..))
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose,openTempFile)
import System.Timeout (timeout)
import Hide.Buffer
import Hide.AgentAccess
import qualified Hide.AgentHub as AH
import Hide.Files (FileState(..))
import Hide.MCPPermissions
import Hide.Model
import Hide.Plugin.BufferHost (captureVersion,versionCurrent)
import Hide.WorkspaceFilesMCP (fileTools)

checks :: IO ()
checks=bracket temporary removePathForcibly $ \directory -> do
  let path=directory </> "config.toml"
      enable=TIO.writeFile path "[editor.mcp.permissions]\nbuffer_apply_diff = 'enable'\n"
      prompt=TIO.writeFile path "[editor.mcp.permissions]\nbuffer_apply_diff = 'prompt'\n"
      base=addDocument Nothing (newBuffer "old\n") (initialDesktop (80,25))
      bid=maybe (error "missing diff target") bufferId (activeWindow base)
      patch="@@ -1 +1 @@\n-old\n+agent\n"::T.Text
      corrected="@@ -1 +1 @@\n-old\n+human λ\n"::T.Text
      args text=object ["bufferId" .= bid,"revision" .= (0::Int),"diff" .= text]
      call runtime=permissionDiffCall runtime (pure (Right ()))
      core d _=pure (False,d)
      submit runtime button d=case dialog d of
        Just dg->let (next,fx)=submitDialog button dg d in snd <$> policyEffects runtime core next fx
        _->error "missing diff review"
      waitUntil runtime predicate d=do
        next<-tickPermissions runtime d
        if predicate next then pure next else threadDelay 1000 >> waitUntil runtime predicate next
      untilResult runtime predicate d=timeout 5000000 (waitUntil runtime predicate d) >>= maybe (error "diff outcome timeout") pure
      awaitReply runtime d response=withAsync response $ \worker->do
        let await current=do
              completed<-poll worker
              case completed of
                Just (Right result)->pure (current,result)
                Just (Left err)->error (show err)
                Nothing->threadDelay 1000 >> tickPermissions runtime current >>= await
        timeout 5000000 (await d) >>= maybe (error "diff reply timeout") pure
      unchanged d=activeText d=="old\n" && maybe False (\doc->revision (documentBuffer doc)==0 && null (undoStack (documentBuffer doc))) (M.lookup bid (buffers d))
  enable
  withPermissionsAt path fileTools $ \runtime -> do
    (started,response)<-call runtime base "buffer_apply_diff" (args patch)
    check "enabled diff starts without synchronous adoption" (unchanged started)
    (applied,result)<-awaitReply runtime started response
    check "untitled diff uses ordinary atomic edit and exact response" (activeText applied=="agent\n" && field "appliedDiff" result==Just patch && field "userModified" result==Just False)
    check "untitled diff is unsaved and one ordinary Undo" (dirty (documentBuffer (buffers applied M.! bid)) && length (undoStack (documentBuffer (buffers applied M.! bid)))==1 && activeText (fst (runCommand Undo applied))=="old\n")
    (cancelStart,cancelResponse)<-call runtime base "buffer_apply_diff" (args patch)
    interrupted<-timeout 10000 cancelResponse
    check "cancel-first terminates the same promise" (interrupted==Nothing)
    cancelled<-tickPermissions runtime cancelStart
    check "cancel-first cannot edit on later tick" (unchanged cancelled)
    check "cancel-first reply stays an error" . isLeft =<< cancelResponse
    (policyStart,policyResponse)<-call runtime base "buffer_apply_diff" (args patch)
    prompt
    (policyResult,rejected)<-awaitReply runtime policyStart policyResponse
    check "Enable becoming Prompt before adoption requires approval" (unchanged policyResult && case rejected of Left err->"requires approval" `T.isInfixOf` err; _->False)
    enable
    (fileStart,fileResponse)<-call runtime base "buffer_apply_diff" (args patch)
    let renamed=fileStart {buffers=M.adjust (\doc->doc {documentFile=Just (FileState (directory </> "new.hs") Nothing)}) bid (buffers fileStart)}
    (fileResult,fileRejected)<-awaitReply runtime renamed fileResponse
    check "new file baseline invalidates originally untitled target" (unchanged fileResult)
    check "file identity failure returns error" (isLeft fileRejected)
  -- Hold the owning tick at the actual caller/adoption claim. Cancellation runs
  -- on another thread and cannot publish an error after that claim edits text.
  enable
  withPermissionsAt path fileTools $ \runtime -> do
    entered<-newEmptyMVar
    release<-newEmptyMVar
    called<-newIORef (0::Int)
    let caller=modifyIORef' called (+1) >> putMVar entered () >> readMVar release >> pure (Right ())
    (started,response)<-permissionDiffCall runtime caller base "buffer_apply_diff" (args patch)
    withAsync (awaitReply runtime started response) $ \owner->do
      _<-timeout 5000000 (readMVar entered) >>= maybe (error "adoption claim not reached") pure
      withAsync response $ \waiter->do
        let blocked=do s<-threadStatus (asyncThreadId waiter)
                       case s of ThreadBlocked BlockedOnMVar->pure (); _->threadDelay 1000 >> blocked
        _<-timeout 1000000 blocked >>= maybe (error "reply waiter not blocked") pure
        withAsync (cancel waiter) $ \canceller->do
          let reached=do s<-threadStatus (asyncThreadId canceller)
                         case s of ThreadBlocked BlockedOnSTM->pure (); _->threadDelay 1000 >> reached
          _<-timeout 1000000 reached >>= maybe (error "cancel path did not reach claim") pure
          stillClaimed<-poll canceller
          check "cancellation cannot finish while adoption owns the claim" (case stillClaimed of Nothing->True; _->False)
          putMVar release ()
          (adopted,_)<-wait owner
          wait canceller
          terminal<-response
          check "adoption-first owns exactly one success reply" (not (isLeft terminal) && activeText adopted=="agent\n")
          again<-tickPermissions runtime adopted
          count<-readIORef called
          check "adoption-first adds exactly one Undo and cannot repeat" (revision (documentBuffer (buffers again M.! bid))==1 && length (undoStack (documentBuffer (buffers again M.! bid)))==1 && count==1)
  prompt
  withPermissionsAt path fileTools $ \runtime -> do
    (shown,pending)<-call runtime base "buffer_apply_diff" (args patch)
    started<-submit runtime 0 shown
    let newer=started {dialog=fmap (\dg->dg {body=["new review body"],fields=map (\f->case f of TextArea "diff" True b sel sr sc->TextArea "diff" True (replaceSelection (Selection 0 (bufferLength b)) corrected b) sel sr sc; _->f) (fields dg)}) (dialog started)}
    rejected<-untilResult runtime (\d->status d=="Diff review changed; approve the current review.") newer
    check "old attempt never overwrites newer editable review/body" (unchanged rejected && maybe False ((==["new review body"]).body) (dialog rejected))
    correctedStart<-submit runtime 0 rejected
    (fixed,fixedResponse)<-awaitReply runtime correctedStart pending
    check "new attempt reuses ticket and applies exact corrected diff" (activeText fixed=="human λ\n" && field "appliedDiff" fixedResponse==Just corrected && field "userModified" fixedResponse==Just True)
    (replacement,replacementReply)<-call runtime base "buffer_apply_diff" (args patch)
    TIO.writeFile (directory </> "replacement.txt") "old\n"
    replacementText<-TIO.readFile (directory </> "replacement.txt")
    let sameRevision=replacement {buffers=M.adjust (\doc->doc {documentBuffer=newBuffer replacementText}) bid (buffers replacement)}
    originalVersion<-captureVersion (documentBuffer (buffers base M.! bid))
    identityCurrent<-versionCurrent originalVersion (documentBuffer (buffers sameRevision M.! bid))
    check "replacement fixture has distinct immutable identity with equal revision/content" (not identityCurrent && contents (documentBuffer (buffers sameRevision M.! bid))=="old\n" && revision (documentBuffer (buffers sameRevision M.! bid))==0)
    replaceStart<-submit runtime 0 sameRevision
    stale<-untilResult runtime (\d->maybe False (any (T.isPrefixOf "Diff not applied:") . body) (dialog d)) replaceStart
    check "same-revision same-content replacement cannot change original request source" (unchanged stale && dialog stale/=Nothing)
    _<-submit runtime 1 stale
    check "replacement failure keeps correction ticket until denied" . isLeft =<< replacementReply
  enable
  withPermissionsAt path fileTools $ \runtime -> do
    let caps=AH.Capabilities False False False []
        driver=AH.AgentDriver directory "test-diff-provider" caps (\_->pure (Right caps))
          (\_->pure (Right Null)) (pure ()) (pure ()) (\_->pure (Left "unsupported"))
    bracket (AH.newAgentHub (AH.HubLimits 1 0) (\_ _->pure (Right driver))) AH.closeAgentHub $ \hub->do
      ident<-AH.registerAgent hub "Diff actor" directory driver >>= either (error . T.unpack) pure
      access<-newAgentAccess
      secret<-grantAgentAccess access ident
      let caller=fmap (() <$) (resolveActiveAgentAccess access hub secret)
      (started,response)<-permissionDiffCall runtime caller base "buffer_apply_diff" (args patch)
      revokeAgentAccess access ident
      (rejected,result)<-awaitReply runtime started response
      check "actual token revocation before adoption rejects prepared diff" (unchanged rejected && isLeft result)
  -- The caller thread's allocation counter excludes the worker's full parsing.
  -- The source is already measured before observing admission allocation.
  enable
  withPermissionsAt path fileTools $ \runtime -> do
    let source=newBuffer (T.replicate 262144 "old line\n")
        large=base {buffers=M.adjust (\doc->doc {documentBuffer=source}) bid (buffers base)}
        largePatch="@@ -1 +1 @@\n-old line\n+new line\n"::T.Text
    _<-evaluate (bufferLength source)
    before<-getAllocationCounter
    (_,pending)<-call runtime large "buffer_apply_diff" (args largePatch)
    after<-getAllocationCounter
    check "large diff admission performs bounded caller allocation" (before-after<2000000)
    _<-timeout 10000 pending
    pure ()
  putStrLn "worker diff checks passed"
  where
    check label ok=unless ok (error label)
    isLeft (Left _)=True; isLeft _=False
    field key result=either (const Nothing) (parseMaybe (withObject "diff response" (.:key))) result
    temporary=do root<-getTemporaryDirectory; (path,h)<-openTempFile root "hide-worker-diff"; hClose h; removeFile path; createDirectory path; pure path
