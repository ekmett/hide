{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.BufferRequest
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Capture the narrow context of self-admitting read and diff tools before worker
-- dispatch. The permission owner retains mutation, review and cancellation;
-- plugins receive neither the desktop nor an editable buffer tree.
module Hide.BufferRequest
  ( bufferRequestServices
  ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Aeson (Value)
import Data.Text (Text)
import Hide.BufferReadServices (bufferReadServices,windowPage)
import Hide.BufferReads (WindowReadTarget,CapturedWindowRead,windowReadTarget,windowReadIdentifier)
import Hide.Model (Desktop,activeWindow,bufferId,nextId,windowId)
import qualified Hide.Plugin.Buffer as P
import qualified Hide.Plugin.WindowRead as W
import qualified Hide.Plugin.BufferDiff as D
import Hide.Plugin.BufferHost (editorReference)
import Hide.Plugin.Command (Codec(..),CommandError(..))
import Hide.Plugin.Request (RequestServices(..))
import Hide.WorkspaceFilesMCP (capturePatchRequest)

-- | Capture under the desktop owner, invoke the resulting tools on a worker.
-- Read selection is fixed at dispatch. A diff additionally fixes its exact
-- immutable content identity before any permission wait; equal numeric revision
-- never permits a replacement buffer to inherit the request.
--
-- The context contains session-bound services and immutable target captures;
-- window captures retain only their prepared body or logical transcript.
-- A diff capability is present only for a validated diff request. It refuses
-- target/revision substitution and asks the existing owner for fresh admission
-- on every invocation, including retries with a changed patch. A window read
-- pins the original active/explicit window and exact body before approval; later
-- focus changes cannot retarget it and body replacement expires the capture.
bufferRequestServices :: P.BufferReader -> P.BufferEditor
  -> (WindowReadTarget -> IO (Either Text CapturedWindowRead)) -> Desktop -> Text -> Value
  -> IO (Either Text RequestServices)
bufferRequestServices reader editor captureWindow desktop name args=do
  reading<-evaluate (bufferReadServices reader (activeWindow desktop >>= bufferId) (nextId desktop))
  if name=="read_window" then case codecDecode W.readInput args of
    Left err->pure (Left err)
    Right request->case do
      ident<-maybe (maybe (Left "No active window") (Right . windowId) (activeWindow desktop)) Right (W.wantedWindow request)
      windowReadTarget desktop ident of
        Left err->pure (Left err)
        Right target->do
          captured<-evaluate target
          let ident=windowReadIdentifier captured
              readPage candidate
                | maybe False (/=ident) (W.wantedWindow candidate)=
                    pure (Left (CommandRejected "Window read changed its original target"))
                | otherwise=do
                    admitted<-captureWindow captured
                    result<-case admitted of
                      Left err->pure (Left err)
                      Right image->windowPage image candidate
                    pure (either (Left . CommandRejected) Right result)
          pure (Right (RequestServices reading Nothing (Just (W.WindowReadServices readPage))))
  else if name/="buffer_apply_diff" then pure (Right (RequestServices reading Nothing Nothing))
  else case codecDecode D.applyInput args of
    Left err->pure (Left err)
    Right request->do
      captured<-capturePatchRequest desktop request
      expected<-evaluate (D.expectedRevision request)
      pure $ case captured of
        Left err->Left err
        Right (ident,version)->
          let reference=editorReference editor ident
              apply candidate
                | D.targetBuffer candidate/=ident || D.expectedRevision candidate/=expected=
                    pure (Left (CommandRejected "Diff request changed its original target or revision"))
                | otherwise=do
                    outcome<-P.applyBufferDiff editor reference version (D.diffText candidate)
                    case outcome of
                      Left err->pure (Left (CommandRejected err))
                      Right result->Right <$> evaluate (force (D.DiffReply ident (P.diffRevision result)
                        (P.appliedDiff result) (P.userModified result)))
          in Right (RequestServices reading (Just (D.BufferDiffServices apply)) Nothing)
