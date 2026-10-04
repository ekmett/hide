{-# LANGUAGE OverloadedStrings #-}
module BufferReadsCheck (checks) where

import Control.Exception (bracket,onException)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import GHC.Conc (threadStatus,ThreadStatus(..),BlockReason(..))
import Hide.BufferReadCommand (withBufferReadCommands)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose,openTempFile)
import System.Timeout (timeout)
import Hide.Buffer
import Hide.AgentAccess
import qualified Hide.AgentHub as AH
import Hide.BufferReads
import Hide.EditorMCP (builtinTools,readBufferTool)
import Hide.MCPPermissions
import Hide.Model
import qualified Hide.Plugin.Buffer as P
import Hide.Plugin.BufferHost (versionCurrent)

checks :: IO ()
checks=bracket temporary removePathForcibly $ \directory -> do
  let path=directory </> "config.toml"
      base=addDocument Nothing (newBuffer "old λ\n") (initialDesktop (80,25))
      bid=maybe (error "missing initial buffer") bufferId (activeWindow base)
      args=object ["bufferId" .= bid]
      core d _=pure (False,d)
      submit runtime d=case dialog d of
        Just dg -> let (next,fx)=submitDialog 0 dg d in snd <$> policyEffects runtime core next fx
        _ -> error "missing read approval"
      text image=P.readText (capturedContent image) (P.TextRange (P.CharOffset 0) (P.CharOffset 6))
  saved<-newIORef Nothing
  withPermissionsAt path builtinTools $ \runtime -> withBufferReadCommands $ \commands->do
    let reader=bufferReader runtime (pure (Right ()))
        begin reader' d=do
          (_,finish)<-readBufferTool commands reader' d "read_buffer" args
          worker<-async finish
          let queued=threadStatus (asyncThreadId worker) >>= \state->case state of
                ThreadBlocked BlockedOnMVar->pure ()
                ThreadFinished->pure ()
                ThreadDied->pure ()
                _->threadDelay 1000 >> queued
          timeout 3000000 queued >>= maybe (error "read consumer did not enqueue") pure
          shown<-tickPermissions runtime d
          let reply=(waitCatch worker >>= pure . either (const (Left "Read worker cancelled")) id) `onException` cancel worker
          pure (shown,reply)
    let capture admission d _ _=do
          ref<-readReference admission bid >>= either (error . T.unpack) pure
          image<-captureBuffer admission d ref >>= either (error . T.unpack) pure
          writeIORef saved (Just (admission,ref,image))
          pure (d,pure (Right Null))
    (_,finish)<-permissionReadCall runtime capture base "read_buffer" args
    check "enabled read dispatch captures current immutable tree" . isRight =<< finish
    Just (expired,ref,image)<-readIORef saved
    check "captured read survives receipt expiry" (text image==Right "old λ\n")
    check "read receipt expires when admitted callback returns" . isLeft =<< captureBuffer expired base ref
    withPermissionsAt (directory </> "other.toml") builtinTools $ \other -> do
      (_,cross)<-permissionReadCall other (\admission d _ _->do
        result<-captureBuffer admission d ref
        pure (d,pure (either Left (const (Right Null)) result))) base "read_buffer" args
      check "same numeric buffer ID cannot cross session namespaces" . isLeft =<< cross
    let changed=base {buffers=M.adjust (\doc->doc {documentBuffer=newBuffer "new λ\n"}) bid (buffers base)}
    check "same revision replacement changes source content identity" . not =<< versionCurrent (capturedVersion image) (documentBuffer (buffers changed M.! bid))
    (_,next)<-permissionReadCall runtime (\admission d _ _->do
      live<-captureBuffer admission d ref
      check "reference remains logical document handle across replacement" (either (const False) ((==Right "new λ\n").text) live)
      closed<-captureBuffer admission (d {buffers=M.delete bid (buffers d)}) ref
      check "closed reference cannot capture" (isLeft closed)
      let reopened=addDocument Nothing (newBuffer "fresh\n") (d {buffers=M.delete bid (buffers d),windows=[]})
      check "reopen receives a different instance ID" (not (M.member bid (buffers reopened)))
      stale<-captureBuffer admission reopened ref
      check "reopen cannot resurrect closed reference" (isLeft stale)
      pure (d,pure (Right Null))) changed "read_buffer" args
    _<-next
    let private=base {buffers=M.adjust (\doc->doc {documentLabel=Just "Agent request"}) bid (buffers base)}
        conversation=base {buffers=M.adjust (\doc->doc {documentLabel=Just "Conversation",documentBuffer=newBuffer "Session: private-token\npublic λ\n"}) bid (buffers base)}
        poisoned=conversation {chatActions=error "worker mask evaluated"}
    (_,hidden)<-begin reader private
    check "admitted reads retain private-buffer refusal" . isLeft =<< hidden
    (_,masked)<-begin reader conversation
    maskedResult<-masked
    check "admitted conversation read applies existing mask" (either (const False) (\value->case parseMaybe (withObject "read result" (.: "text")) value of
      Just output->not ("private-token" `T.isInfixOf` output) && "public λ" `T.isInfixOf` output
      Nothing->False) maskedResult)
    (_,maskWorker)<-begin reader poisoned
    maskResult<-maskWorker
    check "conversation mask is forced only by returned worker, not admission" (isLeft maskResult)
    TIO.writeFile path "[editor.mcp.permissions]\nread_buffer = 'prompt'\n"
    (prompt,pending)<-begin reader base
    check "read policy Prompt uses owning approval queue" (dialog prompt/=Nothing)
    let current=changed {dialog=dialog prompt}
    _<-submit runtime current
    result<-pending
    check "queued read captures current buffer after approval" (either (const False) (\value->parseMaybe (withObject "read result" (.: "text")) value==Just ("new λ\n"::T.Text)) result)
    (again,denied)<-begin reader base
    check "allow-once read does not grant later calls" (dialog again/=Nothing)
    TIO.writeFile path "[editor.mcp.permissions]\nread_buffer = 'disable'\n"
    _<-submit runtime again
    check "policy tightened while read waits rejects queued capture" . isLeft =<< denied
    (_,blocked)<-begin reader base
    check "disabled read fails closed" . isLeft =<< blocked
    TIO.writeFile path "[editor.mcp.permissions]\nread_buffer = 'prompt'\n"
    (cancelPrompt,cancelled)<-begin reader base
    interrupted<-timeout 10000 cancelled
    check "cancelled read approval exits without capture" (interrupted==Nothing)
    _<-submit runtime cancelPrompt
    check "cancelled read cannot be resurrected by later approval" . isLeft =<< cancelled
    let caps=AH.Capabilities False False False []
        driver=AH.AgentDriver directory "test-read-provider" caps (\_->pure (Right caps))
          (\_->pure (Right Null)) (pure ()) (pure ()) (\_->pure (Left "unsupported"))
    bracket (AH.newAgentHub (AH.HubLimits 1 0) (\_ _->pure (Right driver))) AH.closeAgentHub $ \hub->do
      ident<-AH.registerAgent hub "Read actor" directory driver >>= either (error . T.unpack) pure
      access<-newAgentAccess
      secret<-grantAgentAccess access ident
      let attributed=bufferReader runtime (fmap (() <$) (resolveActiveAgentAccess access hub secret))
      (actorPrompt,actorPending)<-begin attributed base
      revokeAgentAccess access ident
      _<-submit runtime actorPrompt
      check "revoked token cannot capture after queued approval" . isLeft =<< actorPending
      liveToken<-grantAgentAccess access ident
      (endedPrompt,endedPending)<-begin (bufferReader runtime (fmap (() <$) (resolveActiveAgentAccess access hub liveToken))) base
      _<-AH.endAgent hub AH.Human ident
      _<-submit runtime endedPrompt
      check "ended actor cannot capture after queued approval" . isLeft =<< endedPending

  putStrLn "scoped buffer read checks passed"
  where
    isLeft (Left _)=True
    isLeft _=False
    isRight=not . isLeft
    check label ok=unless ok (error label)
    temporary=do
      root<-getTemporaryDirectory
      (path,h)<-openTempFile root "hide-buffer-reads"
      hClose h
      removeFile path
      createDirectory path
      pure path
