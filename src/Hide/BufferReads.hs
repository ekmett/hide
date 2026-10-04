{-# LANGUAGE OverloadedStrings #-}
-- | Capture immutable reads during an owning session's admitted callback.
-- No plugin callbacks run here. The caller holds the existing session lock;
-- formatting and any conversation masking are evaluated by its reply worker.
module Hide.BufferReads (CapturedRead(..),captureBuffer) where

import Control.Exception (evaluate)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Hide.GuestAccess (sanitizedBufferContent)
import Hide.Model (Desktop(..),Document(..))
import Hide.BufferReadAdmission (ReadAdmission,resolveReadReference)
import Hide.Plugin.BufferHost (BufferRef,CapturedRead(..),BufferMetadata(..))
import Hide.Plugin.BufferHost (captureVersion)
import Hide.Buffer (captureDirty,snapshotDirty,revision)
import Hide.Files (filePath)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

captureBuffer :: ReadAdmission -> Desktop -> BufferRef -> IO (Either Text CapturedRead)
captureBuffer admission desktop reference=do
  resolved<-resolveReadReference admission reference
  case resolved of
    Left err->pure (Left err)
    Right ident->case M.lookup ident (buffers desktop) of
      Nothing->pure (Left "Buffer not found")
      Just doc->case sanitizedBufferContent desktop ident of
        Nothing->pure (Left "This buffer contains private user or approval content.")
        Just (redacted,content)->do
          -- Ordinary captures sever the Buffer/Undo thunk by evaluating only
          -- BufferContent's strict measured-tree/representation constructor.
          -- Masked Conversation content must be built by the reply worker.
          image<-if documentLabel doc==Just "Conversation" then pure content else evaluate content
          version<-captureVersion (documentBuffer doc)
          changed<-evaluate (captureDirty (documentBuffer doc))
          -- Resolve the selector without traversing the path. A labeled file
          -- otherwise leaves Just(filePath fileState) retaining unrelated state.
          sourcePath<-traverse (evaluate . filePath) (documentFile doc)
          let metadata=BufferMetadata ident
                (fromMaybe (maybe "Untitled" (T.pack . filePath) (documentFile doc)) (documentLabel doc))
                sourcePath (snapshotDirty changed) (revision (documentBuffer doc))
          bounded<-evaluate metadata
          pure (Right (CapturedRead reference version image redacted bounded))
