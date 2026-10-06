{-# LANGUAGE OverloadedStrings #-}
-- | Capture immutable reads during an owning session's admitted callback.
-- No plugin callbacks run here. The caller holds the existing session lock;
-- formatting and any conversation masking are evaluated by its reply worker.
module Hide.BufferReads
  ( CapturedRead(..),captureBuffer
  , WindowReadTarget,windowReadTarget,windowReadIdentifier
  , CapturedWindowRead(..),captureWindow
  ) where

import Control.Exception (evaluate)
import Data.List (find)
import qualified Hide.Plugin.Window as W
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Hide.GuestAccess (sanitizedBufferContent)
import Hide.Model (Desktop(..),Document(..),Window(..),WindowContent(..))
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

-- | An exact installed frame and immutable prepared body, captured without
-- granting a live action capability. Refresh/replacement requires a new request.
data WindowReadTarget = WindowReadTarget !Int !W.WindowRef !W.PreparedWindow

-- | /O(1)/. Wire selector retained for the existing policy request.
windowReadIdentifier :: WindowReadTarget -> Int
windowReadIdentifier (WindowReadTarget ident _ _)=ident

-- | Capture only immutable identity while serialized. Ordinary source windows
-- use read_buffer; rows/details need their own logical projection. Declaration
-- checks do not walk text or apply masks, and retired installed text is readable.
windowReadTarget :: Desktop -> Int -> Either Text WindowReadTarget
windowReadTarget desktop ident=do
  window<-maybe (Left "Window not found") Right (find ((==ident).windowId) (windows desktop))
  reference<-case windowContent window of
    PluginContent ref->Right ref
    SourceContent _->Left "Use read_buffer for source windows."
  prepared<-maybe (Left "Window body not found") Right (M.lookup reference (pluginWindows desktop))
  if W.preparedWindowDisclosure prepared/=W.ReadableWindow then Left "This window is private." else
    case W.preparedWindowRows prepared of
      W.RowsDetails{}->Left "Rows and Details are not a text body."
      _->Right (WindowReadTarget ident reference prepared)

-- | An admitted immutable snapshot. Masking and formatting use only this body
-- on the invoking worker; it retains no Desktop or editable Buffer/Undo root.
data CapturedWindowRead = CapturedWindowRead
  { capturedWindowIdentifier :: !Int, capturedWindowPrepared :: !W.PreparedWindow }

-- | Recheck the exact frame/ref/body under the request claim. Closing or any
-- prepared refresh rejects before capture. An accepted snapshot remains usable
-- after close, independently of retired input/action capability lifetimes.
captureWindow :: Desktop -> WindowReadTarget -> IO (Either Text CapturedWindowRead)
captureWindow desktop (WindowReadTarget ident reference prepared)=case windowReadTarget desktop ident of
  Left err->pure (Left err)
  Right (WindowReadTarget _ currentRef currentBody)
    | currentRef/=reference || currentBody/=prepared->pure (Left "Window body changed; read the window again.")
    | otherwise->Right <$> evaluate (CapturedWindowRead ident prepared)
