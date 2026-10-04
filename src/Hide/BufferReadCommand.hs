{-# LANGUAGE OverloadedStrings #-}
-- | Typed measured read consumer. Its context is a freshly checked session
-- reader plus an opaque target, never Desktop. Capture waits and complete JSON
-- formatting run on the invoking worker; registration follows session lifetime.
module Hide.BufferReadCommand
  ( BufferReadCommands, withBufferReadCommands, ReadPage, readPage, readBufferCommand, formatBufferRead ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import Data.Char (intToDigit)
import qualified Data.Text as T
import qualified Hide.Plugin.Buffer as P
import Hide.Plugin.Command

data ReadPage = ReadPage !Int !Int !Int

data BufferReadCommands = BufferReadCommands (Registry (P.BufferReader,P.BufferRef)) (Command (P.BufferReader,P.BufferRef) ReadPage Value)

-- | Validate page coordinates on either wire or typed routes.
readPage :: Int -> Int -> Int -> Either T.Text ReadPage
readPage start count offset
  | start>=1 && count>=1 && count<=1000 && offset>=0=Right (ReadPage start count offset)
  | otherwise=Left "Use startLine >= 1, lineCount 1..1000, and byteOffset >= 0"

withBufferReadCommands :: (BufferReadCommands -> IO a) -> IO a
withBufferReadCommands use=withRegistry $ \registry->do
  command<-registerCommand registry definition >>= either (ioError . userError . show) pure
  use (BufferReadCommands registry command)

readBufferCommand :: BufferReadCommands -> P.BufferReader -> P.BufferRef -> ReadPage -> IO (Either T.Text Value)
readBufferCommand (BufferReadCommands registry command) reader reference page=fmap (either (Left . message) Right) (invoke registry command (reader,reference) page)
  where
    message (CommandRejected err)=err
    message (CommandFailed err)=err
    message err=T.pack (show err)

definition :: CommandDef (P.BufferReader,P.BufferRef) ReadPage Value
definition=CommandDef "hide.buffer.read" "Read buffer page" input output $ \(reader,reference) (ReadPage start count offset)->do
  captured<-P.captureBuffer reader reference
  case captured of
    Left err->pure (Left (CommandRejected err))
    Right image->do
      let info=P.capturedMetadata image
          metadata=object ["bufferId" .= P.bufferIdentifier info,"title" .= P.displayName info,
            "path" .= P.path info,"modified" .= P.modified info,"binary" .= (P.representation (P.capturedContent image)==P.ByteBuffer),"revision" .= P.editRevision info]
      case formatBufferRead metadata start count offset (P.capturedRedacted image) (P.capturedContent image) of
        Left err->pure (Left (CommandRejected err))
        Right result->Right <$> evaluate (force result)
  where
    input=Codec (object ["type" .= ("object"::T.Text),"additionalProperties" .= False,
      "properties" .= object [name .= object ["type" .= ("integer"::T.Text)] | name<-["startLine","lineCount","byteOffset"]]])
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
  | otherwise = do
    total<-readResult (P.readLineCount b)
    let available=if start>total then 0 else min count (total-start+1)
    rows<-mapM (readResult . P.readLine b . P.LineNumber . (start-1+)) [0..available-1]
    let text=T.intercalate "\n" rows
        limited=T.take 131072 text
    Right (object ["buffer" .= metadata,"startLine" .= start,"lineCount" .= available,
      "totalLines" .= total,"text" .= limited,"redacted" .= redacted,"truncated" .= (T.length limited<T.length text)])
  where
    hex byte rest=let n=fromIntegral byte in ' ':intToDigit (n `div` 16):intToDigit (n `mod` 16):rest
    readResult :: Either P.RangeError a -> Either T.Text a
    readResult=either (Left . T.pack . show) Right
