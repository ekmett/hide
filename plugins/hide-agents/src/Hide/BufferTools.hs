{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.BufferTools
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Agent discovery and bounded buffer reads through self-admitting host services.
-- The plugin owns wire presentation, never a buffer tree or permission decision.
module Hide.BufferTools
  ( tools
  , listOutput
  , readOutput
  ) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser,parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Char (intToDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Plugin.BufferRead
import Hide.Plugin.Command (Codec(..),CommandDef(..))
import Hide.Plugin.Tool (Tool(..))

-- | Explicit editor-visible reads. Every service call owns fresh host admission;
-- read-only metadata is not authority and must not add an outer approval call.
tools :: [Tool BufferReadServices]
tools=
  [Tool "list_buffers" True (CommandDef "hide.buffer.list"
    "List open buffers with paths and unsaved-change state, including untitled buffers."
    listInput listOutput (\services ()->bufferList services))
  ,Tool "read_buffer" True (CommandDef "hide.buffer.read"
    "Read live buffer contents including unsaved edits; private conversation fields are redacted and approval buffers are unavailable. Text is paged by 1-based lines (200 default, 1000 maximum), capped at 131072 characters; binary buffers return up to 4096 hex bytes from byteOffset."
    readInput readOutput bufferRead)]

-- | Concrete masked discovery reply. Encoding traverses metadata only, on the
-- caller's worker; listing grants no subsequent content-read authority.
--
-- @codecDecode listOutput (codecEncode listOutput entries) = Right entries@.
listOutput :: Codec [BufferMetadata]
listOutput=Codec (objectSchema [("buffers",object ["type" .= ("array"::Text),"items" .= metadataSchema])])
  (decodeValue (withObject "buffer listing" $ \fields->do
    only ["buffers"] fields
    entries<-fields .: "buffers"
    traverse parseMetadata entries))
  (\entries->object ["buffers" .= map encodeMetadata entries])

-- | Concrete text or binary reply, preserving the editor wire shape. Binary hex
-- is lower-case pairs separated by one space. Decoding checks the host's page
-- bounds; encoding uses the already bounded immutable payload without source IO.
--
-- @codecDecode readOutput (codecEncode readOutput page) = Right page@
-- for a host page satisfying the public representation and bound contracts.
readOutput :: Codec BufferPage
readOutput=Codec (object ["type" .= ("object"::Text),"oneOf" .= [textSchema,byteSchema]])
  (decodeValue (withObject "buffer page" $ \fields->do
    metadata<-fields .: "buffer" >>= parseMetadata
    if bufferBinary metadata then do
      only ["buffer","byteOffset","bytes","hex","totalBytes"] fields
      offset<-fields .: "byteOffset"
      count<-fields .: "bytes"
      total<-fields .: "totalBytes"
      encoded<-fields .: "hex"
      unless (offset>=0 && count>=0 && count<=4096 && total>=0
        && count<=total-min offset total) (fail "Invalid buffer byte page bounds")
      bytes<-parseHex encoded
      unless (BS.length bytes==count) (fail "Buffer byte count does not match hex payload")
      pure (ByteBufferPage metadata (BytePage offset bytes total))
    else do
      only ["buffer","startLine","lineCount","totalLines","text","redacted","truncated"] fields
      start<-fields .: "startLine"
      count<-fields .: "lineCount"
      total<-fields .: "totalLines"
      body<-fields .: "text"
      redacted<-fields .: "redacted"
      truncated<-fields .: "truncated"
      unless (start>=1 && count>=0 && count<=1000 && total>=0
        && count<=max 0 (total-start+1) && T.length body<=131072) (fail "Invalid buffer text page bounds")
      pure (TextBufferPage metadata (TextPage start count total body redacted truncated))))
  encodePage
  where
    textSchema=objectSchema [("buffer",metadataSchema),("startLine",integer 1 Nothing),
      ("lineCount",integer 0 (Just 1000)),("totalLines",integer 0 Nothing),
      ("text",object ["type" .= ("string"::Text),"maxLength" .= (131072::Int)]),
      ("redacted",boolean),("truncated",boolean)]
    byteSchema=objectSchema [("buffer",metadataSchema),("byteOffset",integer 0 Nothing),
      ("bytes",integer 0 (Just 4096)),("totalBytes",integer 0 Nothing),
      ("hex",object ["type" .= ("string"::Text),"maxLength" .= (12287::Int)])]

encodeMetadata :: BufferMetadata -> Value
encodeMetadata info=object
  ["bufferId" .= bufferIdentifier info,"title" .= bufferTitle info,"path" .= bufferPath info,
   "modified" .= bufferModified info,"binary" .= bufferBinary info,"revision" .= bufferRevision info]

parseMetadata :: Value -> Parser BufferMetadata
parseMetadata=withObject "buffer metadata" $ \fields->do
  only ["bufferId","title","path","modified","binary","revision"] fields
  BufferMetadata <$> fields .: "bufferId" <*> fields .: "title" <*> fields .: "path"
    <*> fields .: "modified" <*> fields .: "binary" <*> fields .: "revision"

metadataSchema :: Value
metadataSchema=objectSchema [("bufferId",object ["type" .= ("integer"::Text)]),("title",string),
  ("path",object ["anyOf" .= [string,object ["type" .= ("null"::Text)]]]),
  ("modified",boolean),("binary",boolean),("revision",object ["type" .= ("integer"::Text)])]

encodePage :: BufferPage -> Value
encodePage (TextBufferPage info page)=object
  ["buffer" .= encodeMetadata info,"startLine" .= pageStartLine page,"lineCount" .= pageLineCount page,
   "totalLines" .= pageTotalLines page,"text" .= pageText page,
   "redacted" .= pageRedacted page,"truncated" .= pageTruncated page]
encodePage (ByteBufferPage info page)=object
  ["buffer" .= encodeMetadata info,"byteOffset" .= pageByteOffset page,"bytes" .= BS.length (pageBytes page),
   "hex" .= T.pack (drop 1 (BS.foldr hex [] (pageBytes page))),"totalBytes" .= pageTotalBytes page]
  where hex byte rest=let n=fromIntegral byte in ' ':intToDigit (n `div` 16):intToDigit (n `mod` 16):rest

parseHex :: Text -> Parser BS.ByteString
parseHex value
  | T.null value=pure BS.empty
  | T.length value>12287=fail "Buffer hex payload exceeds 4096 bytes"
  | otherwise=BS.pack <$> traverse pair (T.splitOn " " value)
  where
    pair text=case T.unpack text of
      [a,b]->fromIntegral <$> ((+) <$> ((16*) <$> digit a) <*> digit b)
      _->fail "Expected lower-case hex byte pairs"
    digit c
      | c>='0' && c<='9'=pure (fromEnum c-fromEnum '0')
      | c>='a' && c<='f'=pure (fromEnum c-fromEnum 'a'+10)
      | otherwise=fail "Expected lower-case hex byte pairs"

decodeValue :: (Value -> Parser a) -> Value -> Either Text a
decodeValue parser=either (Left . T.pack) Right . parseEither parser

only :: [Key] -> Object -> Parser ()
only allowed fields=unless (all (`elem` allowed) (KM.keys fields)) (fail "Unknown reply field")

objectSchema :: [(Key,Value)] -> Value
objectSchema fields=object ["type" .= ("object"::Text),"required" .= map fst fields,
  "additionalProperties" .= False,"properties" .= Object (KM.fromList fields)]

integer :: Int -> Maybe Int -> Value
integer minimumValue maximumValue=object (["type" .= ("integer"::Text),"minimum" .= minimumValue]
  ++["maximum" .= value | Just value<-[maximumValue]])

string, boolean :: Value
string=object ["type" .= ("string"::Text)]
boolean=object ["type" .= ("boolean"::Text)]
