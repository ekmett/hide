{-# LANGUAGE OverloadedStrings #-}
-- | Capture immutable reads during an owning session's admitted callback.
-- No plugin callbacks run here. The caller holds the existing session lock;
-- formatting and any conversation masking are evaluated by its reply worker.
module Hide.BufferReads
  ( CapturedRead(..),captureBuffer,listBuffers
  , WindowReadTarget,windowReadTarget,windowReadIdentifier
  , CapturedWindowRead(..),captureWindow
  ) where

import Control.Exception (evaluate)
import Data.List (find)
import qualified Hide.Plugin.Window as W
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Hide.GuestAccess (sanitizedBufferContent,privateDocument)
import Hide.Model (Desktop(..),Document(..),Window(..),WindowContent(..),conversationTargetFor,conversationLogicalBody,captureDocumentModified,snapshotDocumentModified)
import Hide.ConversationBody (LogicalBody,logicalBodyIdentity)
import Hide.BufferReadAdmission (ReadAdmission,resolveReadReference,readReference)
import Hide.Plugin.BufferHost (BufferRef,CapturedRead(..),BufferMetadata(..),ListedBuffer(..))
import Hide.Plugin.BufferHost (captureVersion)
import Hide.Buffer (revision,byteMode)
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
          changed<-evaluate (captureDocumentModified doc)
          -- Resolve the selector without traversing the path. A labeled file
          -- otherwise leaves Just(filePath fileState) retaining unrelated state.
          sourcePath<-traverse (evaluate . filePath) (documentFile doc)
          let metadata=BufferMetadata ident
                (fromMaybe (maybe "Untitled" (T.pack . filePath) (documentFile doc)) (documentLabel doc))
                sourcePath (snapshotDocumentModified changed) (revision (documentBuffer doc))
          bounded<-evaluate metadata
          pure (Right (CapturedRead reference version image redacted bounded))

-- | /O(n)/. Capture open-buffer metadata at the admitted callback, using the
-- existing document privacy predicate. Dirty comparison remains worker work;
-- neither an immutable read image nor Undo is captured by this operation.
listBuffers :: ReadAdmission -> Desktop -> IO (Either Text [ListedBuffer])
listBuffers admission desktop=sequence <$> traverse capture (M.toAscList (buffers desktop))
  where
    capture (ident,doc)=do
      reference<-readReference admission ident
      case reference of
        Left err->pure (Left err)
        Right ref->do
          let private=privateDocument desktop doc
          sourcePath<-if private then pure Nothing else traverse (evaluate . filePath) (documentFile doc)
          changed<-evaluate (captureDocumentModified doc)
          let title=if private then "[private]" else fromMaybe (maybe "Untitled" T.pack sourcePath) (documentLabel doc)
              metadata=BufferMetadata ident title sourcePath (snapshotDocumentModified changed) (revision (documentBuffer doc))
          entry<-evaluate (ListedBuffer ref metadata (byteMode (documentBuffer doc)))
          pure (Right entry)

-- | An exact installed frame and immutable prepared body, captured without
-- granting a live action capability. Refresh/replacement requires a new request.
-- Conversation text belongs to its logical catalogue, independent of its viewport.
data WindowReadTarget = WindowReadTarget !Int !W.WindowRef !W.PreparedWindow !(Maybe LogicalBody)

-- | /O(1)/. Wire selector retained for the existing policy request.
windowReadIdentifier :: WindowReadTarget -> Int
windowReadIdentifier (WindowReadTarget ident _ _ _)=ident

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
  logical<-case conversationTargetFor desktop window of
    Nothing->Right Nothing
    Just target->maybe (Left "Conversation text is not ready.") (Right . Just)
      (conversationLogicalBody target desktop)
  if W.preparedWindowDisclosure prepared/=W.ReadableWindow then Left "This window is private." else
    case W.preparedWindowRows prepared of
      W.RowsDetails{}->Left "Rows and Details are not a text body."
      _->Right (WindowReadTarget ident reference prepared logical)

-- | An admitted immutable snapshot. Masking and formatting use only this body
-- on the invoking worker; it retains no Desktop or editable Buffer/Undo root.
data CapturedWindowRead = CapturedWindowRead
  { capturedWindowIdentifier :: !Int, capturedWindowPrepared :: !W.PreparedWindow
  , capturedWindowLogical :: !(Maybe LogicalBody) }

-- | Recheck the exact frame/ref/body under the request claim. Closing or any
-- logical replacement rejects before capture; conversation viewport-only changes
-- do not replace its logical body. An accepted snapshot remains usable
-- after close, independently of retired input/action capability lifetimes.
captureWindow :: Desktop -> WindowReadTarget -> IO (Either Text CapturedWindowRead)
captureWindow desktop (WindowReadTarget ident reference prepared logical)=case windowReadTarget desktop ident of
  Left err->pure (Left err)
  Right (WindowReadTarget _ currentRef currentBody currentLogical)
    | currentRef/=reference || not (sameBody currentBody currentLogical)->pure (Left "Window body changed; read the window again.")
    | otherwise->Right <$> evaluate (CapturedWindowRead ident currentBody currentLogical)
  where
    sameBody _ (Just current)=case logical of
      Just captured->logicalBodyIdentity current==logicalBodyIdentity captured
      Nothing->False
    sameBody current Nothing=case logical of Nothing->current==prepared; Just _->False
