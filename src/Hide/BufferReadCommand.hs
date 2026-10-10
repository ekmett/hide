{-# LANGUAGE OverloadedStrings #-}
-- | Typed measured read consumer. Its context is a freshly checked session
-- reader plus an opaque target, never Desktop. Capture waits and complete JSON
-- formatting run on the invoking worker; registration follows session lifetime.
module Hide.BufferReadCommand
  ( BufferReadCommands, withBufferReadCommands, ReadPage, readPage, listBufferCommand, readBufferCommand, readWindowCommand, formatBufferRead ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.Key as K
import Hide.BufferReads (CapturedWindowRead(..))
import Hide.ConversationBody (logicalBodyRead)
import Hide.GuestAccess (sanitizedPreparedContent)
import qualified Hide.Plugin.Window as W
import qualified Data.ByteString as BS
import Data.Char (intToDigit)
import qualified Data.Text as T
import qualified Hide.Plugin.Buffer as P
import Hide.Plugin.Command

data ReadPage = ReadPage !Int !Int !Int

data ReadContext = BufferReadContext P.BufferReader P.BufferRef
  | BufferListContext P.BufferReader
  | WindowReadContext (IO (Either T.Text CapturedWindowRead))
data BufferReadCommands = BufferReadCommands (Registry ReadContext)
  (Command ReadContext ReadPage Value) (Command ReadContext ReadPage Value) (Command ReadContext () Value)

-- | Validate page coordinates on either wire or typed routes.
readPage :: Int -> Int -> Int -> Either T.Text ReadPage
readPage start count offset
  | start>=1 && count>=1 && count<=1000 && offset>=0=Right (ReadPage start count offset)
  | otherwise=Left "Use startLine >= 1, lineCount 1..1000, and byteOffset >= 0"

withBufferReadCommands :: (BufferReadCommands -> IO a) -> IO a
withBufferReadCommands use=withRegistry $ \registry->do
  buffer<-registerCommand registry (definition "hide.buffer.read" "Read buffer page") >>= either (ioError . userError . show) pure
  window<-registerCommand registry (definition "hide.window.read" "Read window page") >>= either (ioError . userError . show) pure
  listing<-registerCommand registry listingDefinition >>= either (ioError . userError . show) pure
  use (BufferReadCommands registry buffer window listing)

readBufferCommand :: BufferReadCommands -> P.BufferReader -> P.BufferRef -> ReadPage -> IO (Either T.Text Value)
readBufferCommand (BufferReadCommands registry command _ _) reader reference page=
  fmap (either (Left . message) Right) (invoke registry command (BufferReadContext reader reference) page)

-- | Run an exact window capture and privacy-aware formatting on the invoking
-- worker. The fixed host capture only queues/awaits; it retains no Desktop.
readWindowCommand :: BufferReadCommands -> IO (Either T.Text CapturedWindowRead) -> ReadPage -> IO (Either T.Text Value)
readWindowCommand (BufferReadCommands registry _ command _) capture page=
  fmap (either (Left . message) Right) (invoke registry command (WindowReadContext capture) page)

-- | List through the same session-bound service; metadata evaluation/JSON
-- preparation stays on this invoking worker, outside the desktop lock.
listBufferCommand :: BufferReadCommands -> P.BufferReader -> IO (Either T.Text Value)
listBufferCommand (BufferReadCommands registry _ _ command) reader=
  fmap (either (Left . message) Right) (invoke registry command (BufferListContext reader) ())

listingDefinition :: CommandDef ReadContext () Value
listingDefinition=CommandDef "hide.buffer.list" "List buffers" input output $ \context ()->case context of
  BufferListContext reader->do
    listing<-P.listBuffers reader
    case listing of
      Left err->pure (Left (CommandRejected err))
      Right entries->Right <$> evaluate (force (object ["buffers" .= map metadata entries]))
  _->pure (Left (CommandRejected "Buffer listing requires a session reader"))
  where
    metadata entry=let info=P.listedMetadata entry in object
      ["bufferId" .= P.bufferIdentifier info,"title" .= P.displayName info,"path" .= P.path info,
       "modified" .= P.modified info,"binary" .= P.listedBinary entry,"revision" .= P.editRevision info]
    input=Codec (object ["type" .= ("object"::T.Text),"additionalProperties" .= False])
      (\value->case value of Object fields | null fields->Right (); _->Left "Buffer listing accepts an empty object")
      (const (object []))
    output=Codec (object ["type" .= ("object"::T.Text)]) Right id

message :: CommandError -> T.Text
message (CommandRejected err)=err
message (CommandFailed err)=err
message err=T.pack (show err)

definition :: T.Text -> T.Text -> CommandDef ReadContext ReadPage Value
definition name title=CommandDef name title input output $ \context (ReadPage start count offset)->do
  result<-case context of
    BufferListContext _->pure (Left "Buffer page requires a target reference")
    BufferReadContext reader reference->do
      captured<-P.captureBuffer reader reference
      pure $ do
        image<-captured
        let info=P.capturedMetadata image
            metadata=object ["bufferId" .= P.bufferIdentifier info,"title" .= P.displayName info,
              "path" .= P.path info,"modified" .= P.modified info,"binary" .= (P.representation (P.capturedContent image)==P.ByteBuffer),"revision" .= P.editRevision info]
        formatBufferRead metadata start count offset (P.capturedRedacted image) (P.capturedContent image)
    WindowReadContext capture->do
      captured<-capture
      case captured of
        Left err->pure (Left err)
        Right image->do
          let prepared=capturedWindowPrepared image
              metadata=object ["windowId" .= capturedWindowIdentifier image,"title" .= W.preparedWindowTitle prepared,
                "coordinateSpace" .= ("window-text"::T.Text)]
          projection<-case capturedWindowLogical image of
            Nothing->pure (maybe (Left "This window is private.") Right (sanitizedPreparedContent prepared))
            Just body->Right <$> logicalBodyRead body
          pure $ do
            (redacted,content)<-projection
            formatTextRead "window" metadata start count redacted content
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

formatBufferRead :: Value -> Int -> Int -> Int -> Bool -> P.BufferRead -> Either T.Text Value
formatBufferRead metadata start count offset redacted b
  | P.representation b==P.ByteBuffer = do
    let size=P.readLength b
        a=min offset size
        z=a+min 4096 (size-a)
    bytes<-readResult (P.readBytes b (P.ByteRange (P.ByteOffset a) (P.ByteOffset z)))
    Right (object ["buffer" .= metadata,"byteOffset" .= offset,"bytes" .= BS.length bytes,
      "hex" .= T.pack (drop 1 (BS.foldr hex [] bytes)),"totalBytes" .= size])
  | otherwise = formatTextRead "buffer" metadata start count redacted b
  where
    hex byte rest=let n=fromIntegral byte in ' ':intToDigit (n `div` 16):intToDigit (n `mod` 16):rest
    readResult :: Either P.RangeError a -> Either T.Text a
    readResult=either (Left . T.pack . show) Right

-- Shared logical text paging; window responses never fabricate buffer metadata.
formatTextRead :: K.Key -> Value -> Int -> Int -> Bool -> P.BufferRead -> Either T.Text Value
formatTextRead kind metadata start count redacted b=do
  total<-readResult (P.readLineCount b)
  let available=if start>total then 0 else min count (total-start+1)
  (parts,truncated)<-readRows 131072 (start-1) available
  Right (object [kind .= metadata,"startLine" .= start,"lineCount" .= available,
    "totalLines" .= total,"text" .= T.concat parts,"redacted" .= redacted,"truncated" .= truncated])
  where
    readRows _ _ 0=Right ([],False)
    readRows remaining row countLeft=do
      P.TextRange (P.CharOffset a) (P.CharOffset z)<-readResult (P.lineRange b (P.LineNumber row))
      let size=min remaining (z-a)
      text<-readResult (P.readText b (P.TextRange (P.CharOffset a) (P.CharOffset (a+size))))
      if size<z-a then Right ([text],True)
      else if countLeft==1 then Right ([text],False)
      else if size==remaining then Right ([text],True)
      else do
        (rest,truncated)<-readRows (remaining-size-1) (row+1) (countLeft-1)
        Right (text:"\n":rest,truncated)
    readResult :: Either P.RangeError a -> Either T.Text a
    readResult=either (Left . T.pack . show) Right
