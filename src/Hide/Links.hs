-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.Links
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Resolve document links on the session host and external opens on the client.
--
-- Markdown loading, layout and indexing can be prepared by a worker and then
-- installed as an immutable document. External resources travel as HTTP(S) URLs
-- or bounded MIME-tagged bytes, never a remote filesystem path. Native openers
-- receive argument vectors; their process exit is reaped asynchronously.
module Hide.Links (followLink, prepareLink, prepareMarkdown, LinkResult, applyLink, openResource, validWebURL) where

import Hide.FileIO (withFileRead)

import Control.Concurrent (forkIO)
import Control.Exception (IOException, try, evaluate)
import Control.Monad (unless, void)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map.Strict as M
import Data.Char (isAlphaNum,toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.URI (parseURIReference,uriScheme,uriAuthority,uriRegName,uriPath,uriFragment,uriQuery)
import System.Directory (canonicalizePath,getTemporaryDirectory)
import System.FilePath ((</>),takeDirectory,takeExtension,isAbsolute,normalise)
import System.Info (os)
import System.IO (hFileSize,openBinaryTempFile,hClose)
import System.Process (createProcess,proc,waitForProcess,CreateProcess(..),StdStream(NoStream))
import Hide.Plugin.Canvas (imageContentFormat)
import Hide.Buffer (Buffer(revision),Selection(..),bufferLength)
import Hide.LSP (uriFilePath)
import Hide.Markdown (renderMarkdownWithShellBlocks)
import Hide.Model
import Hide.Syntax (styledContents,splitStyledText)

-- | Accept control-free absolute HTTP(S) URLs with a nonempty host.
validWebURL :: Text -> Bool
validWebURL text=not (T.any (<' ') text) && case parseURIReference (T.unpack text) of
  Just uri -> uriScheme uri `elem` ["http:","https:"] && maybe False (not . null . uriRegName) (uriAuthority uri)
  _ -> False

-- Local document links stay on the server. External opens are emitted to the
-- frontend: an HTTP(S) URL, or bounded, typed image/PDF bytes (never a host path).
data LinkResult = LinkDocument Document Int FilePath | LinkExternal Text (Maybe Value)

-- | Synchronous convenience composition of preparation and adoption.
-- Interactive callers should separate the worker phase with prepareLink/applyLink.
followLink :: Bool -> Desktop -> Maybe FilePath -> Text -> IO (Desktop,Maybe Value)
followLink stream d origin target=do
  prepared<-prepareLink stream (max 20 (min 76 (fst (screenSize d)-treeWidthOf d-4))) (startingDirectory d) origin target
  pure (applyLink prepared d)

-- | Resolve and prepare a link outside the desktop lock. With streaming enabled,
-- return external resources as client packets; otherwise invoke the local opener.
prepareLink :: Bool -> Int -> FilePath -> Maybe FilePath -> Text -> IO LinkResult
prepareLink stream columns directory origin target=do
  result<-try run
  pure $ case result of
    Left (_::IOException)->LinkExternal "Cannot open link: the target is missing, unreadable or unsupported." Nothing
    Right value->value
  where
    run | validWebURL target=external (object ["type" .= ("open-resource"::Text),"url" .= target])
        | otherwise=do
          (path,fragment)<-localTarget
          -- Match ordinary image opening even when a supported image has an
          -- unusual extension. Detection reads only this bounded prefix.
          prefix<-withFileRead path (\handle->BS.hGet handle 8)
          let detected=imageMime prefix
              extension=map toLower (takeExtension path)
          case detected of
            Just mime->openTyped path mime
            Nothing | extension `elem` [".md",".markdown"]->do
              text<-boundedRead path >>= either (const (ioError (userError "Invalid UTF-8 Markdown"))) pure . TE.decodeUtf8'
              prepareMarkdown columns path fragment text
            Nothing->case lookup extension formats of
              Just mime->openTyped path mime
              Nothing->pure (LinkExternal "Open link supports Markdown, web URLs, images and PDF files." Nothing)
    openTyped path mime
      | stream || lookup (map toLower (takeExtension path)) formats/=Just mime=do
          bytes<-boundedRead path
          let encoded=TE.decodeUtf8 (B64.encode bytes)
              actualMime=fromMaybe mime (imageMime bytes)
          _<-evaluate (T.length encoded)
          external (object ["type" .= ("open-resource"::Text),"mime" .= actualMime,"data" .= encoded])
      | otherwise=launch path >> pure (LinkExternal "Opened in the default application." Nothing)
    imageMime bytes=imageContentFormat bytes >>= (`lookup` [("PNG","image/png"),("JPEG","image/jpeg")])
    external packet | stream=pure (LinkExternal "Opening link on the client." (Just packet))
                    | otherwise=do result<-openResource packet; pure (LinkExternal (either id (const "Opened in the browser.") result) Nothing)
    localTarget=do
      let base=maybe directory takeDirectory origin
      case parseURIReference (T.unpack target) of
        Just uri | null (uriScheme uri), uriAuthority uri==Nothing, null (uriQuery uri)->do
          decoded<-maybe (ioError (userError "Invalid path")) (pure . drop 1) (uriFilePath ("file:///"<>T.pack (uriPath uri)))
          let path=if null decoded then fromMaybe base origin else if isAbsolute decoded then decoded else base </> decoded
          absolute<-canonicalizePath (normalise path)
          pure (absolute,T.pack (drop 1 (uriFragment uri)))
        _ | isAbsolute (T.unpack target)->do path<-canonicalizePath (T.unpack target); pure (path,"")
          | otherwise->ioError (userError "Unsupported link scheme")

-- | Prepare already-read Markdown on a worker, preserving the existing help
-- styling, shell blocks, relative-link base and fragment navigation.
prepareMarkdown :: Int -> FilePath -> Text -> Text -> IO LinkResult
prepareMarkdown columns path fragment text=do
  let (styled,blocks)=renderMarkdownWithShellBlocks columns text
      opened=addHelpStyled styled (initialDesktop (columns+4,25))
      rows=map (styledContents . fst) (splitStyledText styled)
      matching=[i | (i,line)<-zip [0..] rows, slug line==fragment]
      row=if T.null fragment then 0 else fromMaybe 0 (case matching of first:_ -> Just first; _ -> Nothing)
  case activeDocument opened of
    Nothing->ioError (userError "Missing help buffer")
    Just doc->do
      let prepared=doc {documentMarkdownPath=Just path,documentShellBlocks=blocks}
      _<-evaluate (bufferLength (documentBuffer prepared)+length (documentHighlight prepared)+sum [a+b+T.length url | (a,b,url)<-documentLinks prepared]+length blocks+row)
      pure (LinkDocument prepared row path)
  where
    slug=T.intercalate "-" . T.words . T.filter (\c->isAlphaNum c || c==' ' || c=='-' || c=='_') . T.toLower

-- | Install a prepared document or status notice, returning an optional client packet.
applyLink :: LinkResult -> Desktop -> (Desktop,Maybe Value)
applyLink (LinkExternal notice packet) d=(d {status=notice},packet)
applyLink (LinkDocument doc row path) d=
  let opened=addHelp "" d
      installed=case activeWindow opened of
        Just w | Just bid<-bufferId w,Just previous<-M.lookup bid (buffers opened)->opened {buffers=M.insert bid doc {documentBuffer=(documentBuffer doc) {revision=revision (documentBuffer previous)}} (buffers opened)}
        _->opened
      positioned=modifyActive (\w->w {scrollRow=row,scrollColumn=0,selection=Selection 0 0}) installed
  in (positioned {status=T.pack path},Nothing)

formats :: [(String,Text)]
formats=[(".png","image/png"),(".jpg","image/jpeg"),(".jpeg","image/jpeg"),(".gif","image/gif"),(".webp","image/webp"),(".bmp","image/bmp"),(".svg","image/svg+xml"),(".pdf","application/pdf")]

boundedRead :: FilePath -> IO BS.ByteString
boundedRead path=withFileRead path $ \h->do
  size<-hFileSize h
  unless (size<=8388608) (ioError (userError "Link exceeds 8 MiB"))
  bytes<-BS.hGet h 8388609
  unless (BS.length bytes<=8388608) (ioError (userError "Link exceeds 8 MiB"))
  pure bytes

-- OS invocation uses argument vectors, not shell interpolation of link text.
launch :: FilePath -> IO ()
launch target=do
  let command=case os of
        "darwin"->proc "open" [target]
        "mingw32"->proc "rundll32.exe" ["url.dll,FileProtocolHandler",target]
        _->proc "xdg-open" [target]
  (_,_,_,process)<-createProcess command {std_in=NoStream,std_out=NoStream,std_err=NoStream}
  void (forkIO (void (waitForProcess process)))

-- | Validate a client resource packet and launch the OS opener.
-- File bytes are bounded and written to a temporary file; success means launch,
-- not confirmation that another application displayed the resource.
openResource :: Value -> IO (Either Text ())
openResource value=case parseEither parse value of
  Left _->pure (Left "Invalid external link response.")
  Right resource->do
    result<-try $ case resource of
      Left url->launch (T.unpack url)
      Right (extension,bytes)->do
        temporary<-getTemporaryDirectory
        (path,h)<-openBinaryTempFile temporary ("thc-link"<>extension)
        BS.hPut h bytes; hClose h
        launch path
    pure $ case result of
      Left (_::IOException)->Left "Could not launch the default application."
      Right ()->Right ()
  where
    parse=withObject "resource" $ \o->do
      url<-o .:? "url"
      case url of
        Just text | validWebURL text->pure (Left text)
                  | otherwise->fail "Only HTTP(S) URLs can be opened"
        Nothing->do
          mime<-o .: "mime"
          extension<-maybe (fail "Unsupported file type") pure (lookup mime [(mimeType,ext) | (ext,mimeType)<-formats])
          encoded<-o .: "data"
          unless (T.length encoded<=11184812) (fail "File too large")
          bytes<-either fail pure (B64.decode (TE.encodeUtf8 encoded))
          unless (BS.length bytes<=8388608) (fail "File too large")
          pure (Right (extension,bytes))
