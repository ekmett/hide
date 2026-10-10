{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.BufferTools
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Agent discovery, bounded reads and exact diffs through self-admitting services.
-- The plugin owns wire presentation, never a buffer tree or permission decision.
module Hide.BufferTools
  ( tools
  , listOutput
  , readOutput
  , windowOutput
  , applyOutput
  ) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Pair,Parser,parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Char (intToDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Plugin.BufferRead
import qualified Hide.Plugin.BufferDiff as D
import qualified Hide.Plugin.WindowRead as W
import Hide.Plugin.Request (RequestServices(..))
import Hide.Plugin.Command (Codec(..),CommandDef(..),CommandError(..))
import Hide.Plugin.Tool (Tool(..),ToolHints(..))

-- | Explicit editor-visible requests. Every service call owns fresh host
-- admission; read-only metadata grants no authority and must not add an outer
-- approval call. A diff requires the exact request-bound capability from the host.
-- A prepared-window read similarly requires the captured immutable body service.
tools :: [Tool RequestServices]
tools=
  [Tool "list_buffers" (ToolHints True False False) (CommandDef "hide.buffer.list"
    "List open buffers with paths and unsaved-change state, including untitled buffers."
    listInput listOutput (\services ()->bufferList (requestBuffers services)))
  ,Tool "read_buffer" (ToolHints True False False) (CommandDef "hide.buffer.read"
    "Read live buffer contents including unsaved edits; private conversation fields are redacted and approval buffers are unavailable. Text is paged by 1-based lines (200 default, 1000 maximum), capped at 131072 characters; binary buffers return up to 4096 hex bytes from byteOffset."
    readInput readOutput (\services->bufferRead (requestBuffers services)))
  ,Tool "read_window" (ToolHints True False False) (CommandDef "hide.window.read"
    "Read an explicitly readable prepared text window by logical window-text lines; private regions are redacted. Defaults to the active window, 200 lines; maximum 1000 lines and 131072 characters. Source windows use read_buffer."
    W.readInput windowOutput (\services arguments->case requestWindows services of
      Nothing->pure (Left (CommandRejected "Window read requires an exact host-captured request."))
      Just reader->W.readWindow reader arguments))
  ,Tool "buffer_apply_diff" (ToolHints False True True) (CommandDef "hide.buffer.apply-diff"
    "Apply strict unified diffs atomically to 1–16 distinct live text buffers at their given revisions, with at most 1048576 patch characters in total. Context and hunk positions must match exactly. One approval covers the whole batch; every patch is applied together or none, with ordinary Undo per changed buffer. No file is saved. Results follow input order and report the exact approved patches. File headers identify only the target buffer, never disk paths."
    D.applyInput applyOutput (\services arguments->case requestDiff services of
      Nothing->pure (Left (CommandRejected "Diff requires an exact host-captured request."))
      Just editing->D.applyDiff editing arguments))]

-- | Ordered outcomes for an atomic batch, preserving exact human corrections.
-- A diff never saves a file; a true saved marker fails. Decode validates distinct
-- targets, 1–16 replies and the aggregate one-MiB-character applied-diff bound.
--
-- @codecDecode applyOutput (codecEncode applyOutput replies) = Right replies@
-- for host outcomes satisfying those bounds.
applyOutput :: Codec [D.DiffReply]
applyOutput=Codec (objectSchema [("buffers",object ["type" .= ("array"::Text),
    "minItems" .= (1::Int),"maxItems" .= (16::Int),"items" .= resultSchema])])
  (decodeValue (withObject "buffer diff replies" $ \fields->do
    only ["buffers"] fields
    values<-fields .: "buffers"
    unless (not (null values) && length values<=16) (fail "Diff batch requires 1..16 replies")
    replies<-traverse parseReply values
    _<-either (fail . T.unpack) pure (D.applyDiffArguments
      [D.DiffEntry (D.editedBuffer reply) (D.editedRevision reply) (D.appliedDiff reply) | reply<-replies])
    pure replies))
  (\replies->object ["buffers" .= map encodeReply replies])
  where
    number=object ["type" .= ("integer"::Text)]
    resultSchema=objectSchema [("bufferId",number),("revision",number),
      ("saved",object ["type" .= ("boolean"::Text),"const" .= False]),
      ("appliedDiff",object ["type" .= ("string"::Text),"maxLength" .= (1048576::Int)]),
      ("userModified",boolean)]
    parseReply=withObject "buffer diff reply" $ \fields->do
      only ["bufferId","revision","saved","appliedDiff","userModified"] fields
      target<-fields .: "bufferId"
      revision<-fields .: "revision"
      saved<-fields .: "saved"
      unless (not saved) (fail "Diff does not save files")
      D.DiffReply target revision <$> fields .: "appliedDiff" <*> fields .: "userModified"
    encodeReply reply=object ["bufferId" .= D.editedBuffer reply,"revision" .= D.editedRevision reply,
      "saved" .= False,"appliedDiff" .= D.appliedDiff reply,"userModified" .= D.userModified reply]

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
      TextBufferPage metadata <$> parseTextPage fields))
  encodePage
  where
    textSchema=objectSchema (("buffer",metadataSchema):textPageSchema)
    byteSchema=objectSchema [("buffer",metadataSchema),("byteOffset",integer 0 Nothing),
      ("bytes",integer 0 (Just 4096)),("totalBytes",integer 0 Nothing),
      ("hex",object ["type" .= ("string"::Text),"maxLength" .= (12287::Int)])]

-- | Concrete prepared-window reply with the existing window-text coordinate
-- marker. The body uses the same bounded text-page codec as buffer reads; there
-- is no source buffer ID, byte view or live action capability in this reply.
--
-- @codecDecode windowOutput (codecEncode windowOutput page) = Right page@
-- for a host page satisfying the public text-page bounds.
windowOutput :: Codec W.WindowPage
windowOutput=Codec (objectSchema (("window",metadata):textPageSchema))
  (decodeValue (withObject "window page" $ \fields->do
    only ["window","startLine","lineCount","totalLines","text","redacted","truncated"] fields
    (ident,title)<-fields .: "window" >>= withObject "window metadata" (\info->do
      only ["windowId","title","coordinateSpace"] info
      ident<-info .: "windowId"
      title<-info .: "title"
      space<-info .: "coordinateSpace"
      unless (space==("window-text"::Text)) (fail "Invalid window coordinate space")
      pure (ident,title))
    W.WindowPage ident title <$> parseTextPage fields))
  (\page->object (("window" .= object
    ["windowId" .= W.windowIdentifier page,"title" .= W.windowTitle page,
     "coordinateSpace" .= ("window-text"::Text)]):encodeTextPage (W.windowPage page)))
  where metadata=objectSchema [("windowId",object ["type" .= ("integer"::Text)]),("title",string),
          ("coordinateSpace",object ["type" .= ("string"::Text),"const" .= ("window-text"::Text)])]

parseTextPage :: Object -> Parser TextPage
parseTextPage fields=do
  start<-fields .: "startLine"
  count<-fields .: "lineCount"
  total<-fields .: "totalLines"
  body<-fields .: "text"
  redacted<-fields .: "redacted"
  truncated<-fields .: "truncated"
  unless (start>=1 && count>=0 && count<=1000 && total>=0
    && count<=max 0 (total-start+1) && T.length body<=131072) (fail "Invalid text page bounds")
  pure (TextPage start count total body redacted truncated)

textPageSchema :: [(Key,Value)]
textPageSchema=[("startLine",integer 1 Nothing),("lineCount",integer 0 (Just 1000)),
  ("totalLines",integer 0 Nothing),("text",object ["type" .= ("string"::Text),"maxLength" .= (131072::Int)]),
  ("redacted",boolean),("truncated",boolean)]

encodeTextPage :: TextPage -> [Pair]
encodeTextPage page=["startLine" .= pageStartLine page,"lineCount" .= pageLineCount page,
  "totalLines" .= pageTotalLines page,"text" .= pageText page,
  "redacted" .= pageRedacted page,"truncated" .= pageTruncated page]

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
encodePage (TextBufferPage info page)=object (("buffer" .= encodeMetadata info):encodeTextPage page)
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
