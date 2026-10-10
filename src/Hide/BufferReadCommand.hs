{-# LANGUAGE OverloadedStrings #-}
-- | Bounded buffer services and exact prepared-window reads. Every buffer
-- operation uses the session's actor-bound admission queue; paging and metadata
-- evaluation stay on its invoking worker. Only bounded values cross the public
-- plugin API. Prepared windows retain their separate exact-body capture rule.
module Hide.BufferReadCommand
  ( BufferReadCommands, withBufferReadCommands, ReadPage, readPage, readWindowCommand
  , bufferReadServices, bufferPage
  ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Hide.BufferReads (CapturedWindowRead(..))
import Hide.ConversationBody (logicalBodyRead)
import Hide.GuestAccess (sanitizedPreparedContent)
import qualified Hide.Plugin.Window as W
import qualified Data.Text as T
import qualified Hide.Plugin.Buffer as P
import qualified Hide.Plugin.BufferRead as R
import Hide.Plugin.BufferHost (readerReference)
import Hide.Plugin.Command

data ReadPage = ReadPage !Int !Int !Int

newtype ReadContext = WindowReadContext (IO (Either T.Text CapturedWindowRead))
data BufferReadCommands = BufferReadCommands (Registry ReadContext) (Command ReadContext ReadPage Value)

-- | Validate window page coordinates before capture.
readPage :: Int -> Int -> Int -> Either T.Text ReadPage
readPage start count offset
  | start>=1 && count>=1 && count<=1000 && offset>=0=Right (ReadPage start count offset)
  | otherwise=Left "Use startLine >= 1, lineCount 1..1000, and byteOffset >= 0"

withBufferReadCommands :: (BufferReadCommands -> IO a) -> IO a
withBufferReadCommands use=withRegistry $ \registry->do
  window<-registerCommand registry windowDefinition >>= either (ioError . userError . show) pure
  use (BufferReadCommands registry window)

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

-- | Run an exact window capture and privacy-aware formatting on the invoking
-- worker. The fixed host capture only queues/awaits; it retains no Desktop.
readWindowCommand :: BufferReadCommands -> IO (Either T.Text CapturedWindowRead) -> ReadPage -> IO (Either T.Text Value)
readWindowCommand (BufferReadCommands registry command) capture page=
  fmap (either (Left . message) Right) (invoke registry command (WindowReadContext capture) page)

message :: CommandError -> T.Text
message (CommandRejected err)=err
message (CommandFailed err)=err
message err=T.pack (show err)

windowDefinition :: CommandDef ReadContext ReadPage Value
windowDefinition=CommandDef "hide.window.read" "Read window page" input output $ \(WindowReadContext capture) (ReadPage start count _)->do
  captured<-capture
  result<-case captured of
    Left err->pure (Left err)
    Right image->do
      let prepared=capturedWindowPrepared image
          info=object ["windowId" .= capturedWindowIdentifier image,"title" .= W.preparedWindowTitle prepared,
            "coordinateSpace" .= ("window-text"::T.Text)]
      projection<-case capturedWindowLogical image of
        Nothing->pure (maybe (Left "This window is private.") Right (sanitizedPreparedContent prepared))
        Just body->Right <$> logicalBodyRead body
      pure $ do
        (redacted,content)<-projection
        page<-textPage start count redacted content
        pure (object ["window" .= info,"startLine" .= R.pageStartLine page,"lineCount" .= R.pageLineCount page,
          "totalLines" .= R.pageTotalLines page,"text" .= R.pageText page,
          "redacted" .= R.pageRedacted page,"truncated" .= R.pageTruncated page])
  case result of
    Left err->pure (Left (CommandRejected err))
    Right value->Right <$> evaluate (force value)
  where
    input=Codec (object ["type" .= ("object"::T.Text),"additionalProperties" .= False,
      "properties" .= object [property .= object ["type" .= ("integer"::T.Text)] | property<-["startLine","lineCount","byteOffset"]]])
      (\value->do
        (start,count,offset)<-either (Left . T.pack) Right (parseEither (withObject "read page" (\o->(,,) <$> o .:? "startLine" .!= 1 <*> o .:? "lineCount" .!= 200 <*> o .:? "byteOffset" .!= 0)) value)
        readPage start count offset)
      (\(ReadPage start count offset)->object ["startLine" .= start,"lineCount" .= count,"byteOffset" .= offset])
    output=Codec (object ["type" .= ("object"::T.Text)]) Right id

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
