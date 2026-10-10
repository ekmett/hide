{-# LANGUAGE OverloadedStrings #-}
-- | Bounded buffer services and exact prepared-window reads. Every buffer
-- operation uses the session's actor-bound admission queue; paging and metadata
-- evaluation stay on its invoking worker. Only bounded values cross the public
-- plugin API. Prepared windows retain their separate exact-body capture rule.
module Hide.BufferReadServices
  ( bufferReadServices, bufferPage, windowPage
  ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Hide.BufferReads (CapturedWindowRead(..))
import Hide.ConversationBody (logicalBodyRead)
import Hide.GuestAccess (sanitizedPreparedContent)
import qualified Hide.Plugin.Window as W
import qualified Data.Text as T
import qualified Hide.Plugin.Buffer as P
import qualified Hide.Plugin.BufferRead as R
import qualified Hide.Plugin.WindowRead as WR
import Hide.Plugin.BufferHost (readerReference)
import Hide.Plugin.Command

-- | Capture only the default source identity and allocation frontier under the
-- UI owner. IDs are allocated once per session: a later buffer cannot become an
-- earlier request's target. No Desktop, document, Undo or source tree is retained.
--
-- Both callbacks re-enter the reader's fresh policy/actor/privacy admission on
-- every call. Reader shutdown cancels accepted requests and rejects later calls;
-- the plugin tool registry separately owns each exposed tool's lifetime.
bufferReadServices :: P.BufferReader -> Maybe Int -> Int -> R.BufferReadServices
bufferReadServices reader active limit=maybe () (\ident->ident `seq` ()) active `seq` limit `seq` R.BufferReadServices listing reading
  where
    listing=do
      captured<-P.listBuffers reader
      case captured of
        Left err->pure (Left (CommandRejected err))
        Right entries->Right <$> evaluate (force [metadata (P.listedBinary entry) (P.listedMetadata entry) | entry<-entries])
    reading arguments=case maybe (maybe (Left "No active source buffer") Right active) Right (R.wantedBuffer arguments) of
      Left err->pure (Left (CommandRejected err))
      Right ident | ident<0 || ident>=limit->pure (Left (CommandRejected "Buffer not found"))
      Right ident->do
        captured<-P.captureBuffer reader (readerReference reader ident)
        case captured >>= \image->bufferPage (metadata (P.representation (P.capturedContent image)==P.ByteBuffer) (P.capturedMetadata image))
              arguments (P.capturedRedacted image) (P.capturedContent image) of
          Left err->pure (Left (CommandRejected err))
          Right page->Right <$> evaluate (force page)

metadata :: Bool -> P.BufferMetadata -> R.BufferMetadata
metadata binary info=R.BufferMetadata (P.bufferIdentifier info) (P.displayName info) (P.path info)
  (P.modified info) binary (P.editRevision info)

-- | Slice an admitted measured read before materializing text or bytes. Work is
-- bounded by the requested page, including when a single source row is huge.
-- Text counts Unicode characters; binary offsets/counts remain original bytes.
bufferPage :: R.BufferMetadata -> R.ReadArguments -> Bool -> P.BufferRead -> Either T.Text R.BufferPage
bufferPage info arguments redacted content
  | P.representation content==P.ByteBuffer=do
      let size=P.readLength content
          offset=R.byteOffset arguments
          start=min offset size
          end=start+min 4096 (size-start)
      bytes<-readResult (P.readBytes content (P.ByteRange (P.ByteOffset start) (P.ByteOffset end)))
      pure (R.ByteBufferPage info (R.BytePage offset bytes size))
  | otherwise=R.TextBufferPage info <$> textPage (R.startLine arguments) (R.lineCount arguments) redacted content

-- | Format an already admitted immutable window read on its invoking worker.
-- Conversation pages address the whole logical transcript, independently of
-- viewport wrapping. Masks and bounded measured reads precede wire encoding;
-- the public result contains neither a body handle nor editable source content.
windowPage :: CapturedWindowRead -> WR.ReadArguments -> IO (Either T.Text WR.WindowPage)
windowPage image arguments=do
  let prepared=capturedWindowPrepared image
  projection<-case capturedWindowLogical image of
    Nothing->pure (maybe (Left "This window is private.") Right (sanitizedPreparedContent prepared))
    Just body->Right <$> logicalBodyRead body
  case projection >>= \(redacted,content)->textPage (WR.startLine arguments) (WR.lineCount arguments) redacted content of
    Left err->pure (Left err)
    Right page->Right <$> evaluate (force (WR.WindowPage (capturedWindowIdentifier image) (W.preparedWindowTitle prepared) page))

-- Shared logical text paging; window responses never fabricate buffer metadata.
textPage :: Int -> Int -> Bool -> P.BufferRead -> Either T.Text R.TextPage
textPage start count redacted content=do
  total<-readResult (P.readLineCount content)
  let available=if start>total then 0 else min count (total-start+1)
  (parts,truncated)<-readRows 131072 (start-1) available
  pure (R.TextPage start available total (T.concat parts) redacted truncated)
  where
    readRows _ _ 0=Right ([],False)
    readRows remaining row countLeft=do
      P.TextRange (P.CharOffset a) (P.CharOffset z)<-readResult (P.lineRange content (P.LineNumber row))
      let size=min remaining (z-a)
      text<-readResult (P.readText content (P.TextRange (P.CharOffset a) (P.CharOffset (a+size))))
      if size<z-a then Right ([text],True)
      else if countLeft==1 then Right ([text],False)
      else if size==remaining then Right ([text],True)
      else do
        (rest,truncated)<-readRows (remaining-size-1) (row+1) (countLeft-1)
        Right (text:"\n":rest,truncated)

readResult :: Either P.RangeError a -> Either T.Text a
readResult=either (Left . T.pack . show) Right
