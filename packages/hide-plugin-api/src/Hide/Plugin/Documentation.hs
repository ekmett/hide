{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.Documentation
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Checked requests and session-scoped services for bounded offline documentation.
-- The host owns corpus selection, filesystem validation and read/search budgets.
-- These capabilities expose no arbitrary path reader or private editor state.
module Hide.Plugin.Documentation
  ( DocsServices(..)
  , ReadArguments
  , readArguments
  , readCorpus
  , readPath
  , readStartLine
  , readLineCount
  , ReadPage(..)
  , Heading(..)
  , readInput
  , readOutput
  , ListArguments
  , listArguments
  , listCorpus
  , listOffset
  , listLimit
  , listInput
  , listOutput
  , SearchArguments
  , searchArguments
  , searchCorpus
  , searchQuery
  , searchPath
  , searchOffset
  , searchLimit
  , searchCaseSensitive
  , searchInput
  , searchOutput
  ) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser,parseEither)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (isAbsolute,takeExtension)
import Hide.Plugin.Command (Codec(..),CommandError)

-- | All calls use the host's same typed documentation operations. Run on the
-- caller's worker after permission admission. Retaining this record does not
-- keep the session's registrations alive: calls after retirement are rejected.
-- Corpus roots come from the host; arguments cannot select another root.
data DocsServices = DocsServices
  { docsRead :: ReadArguments -> IO (Either CommandError ReadPage)
  , docsList :: ListArguments -> IO (Either CommandError Value)
  , docsSearch :: SearchArguments -> IO (Either CommandError Value)
  }

-- | Checked one-based line range. Construction and JSON decoding enforce the
-- same corpus, documentation path and 1..500 line-count rules.
data ReadArguments = ReadArguments Text FilePath Int Int

-- | Construct the same checked arguments without passing through JSON.
readArguments :: Text -> FilePath -> Int -> Int -> Either Text ReadArguments
readArguments corpus path start count=checked (checkedRead corpus path start count)

-- | Captured corpus name, either @editor@ or @thc@.
readCorpus :: ReadArguments -> Text
readCorpus (ReadArguments corpus _ _ _)=corpus
-- | Relative, validated documentation path.
readPath :: ReadArguments -> FilePath
readPath (ReadArguments _ path _ _)=path
-- | One-based first requested line.
readStartLine :: ReadArguments -> Int
readStartLine (ReadArguments _ _ start _)=start
-- | Requested line count, between 1 and 500.
readLineCount :: ReadArguments -> Int
readLineCount (ReadArguments _ _ _ count)=count

-- | Immutable bounded read result, shared by Help and plugin tools. The host
-- caps a file at 1 MiB, returned text at 128 Ki characters and headings at 64.
-- Truncation and continuation are reported independently of an empty range.
data ReadPage = ReadPage
  { pageCorpus :: Text, pagePath :: FilePath, pageBytes :: Int
  , pageStart :: Int, pageCount :: Int, pageTotal :: Int, pageText :: Text
  , pageTruncated :: Bool, pageHasMore :: Bool
  , pageHeadings :: [Heading], pageHeadingsTruncated :: Bool
  }

-- | A Markdown heading outside fenced code, with one-based source line.
data Heading = Heading { headingLine :: Int, headingLevel :: Int, headingText :: Text }

-- | Validated path/range codec. Omitted fields default to corpus @editor@,
-- @startLine = 1@ and @lineCount = 200@; unknown fields are rejected.
readInput :: Codec ReadArguments
readInput=Codec (documentationSchema ["path"] [("path",string),("startLine",integer),("lineCount",integer)])
  (decodeValue parseArguments) encodeArguments
  where
    parseArguments=withObject "documentation read" $ \o->do
      corpus<-parseCorpus ["corpus","path","startLine","lineCount"] o
      path<-o .: "path"
      start<-o .:? "startLine" .!= 1
      count<-o .:? "lineCount" .!= 200
      checkedRead corpus path start count
    encodeArguments (ReadArguments corpus path start count)=object
      ["corpus" .= corpus,"path" .= path,"startLine" .= start,"lineCount" .= count]

checkedRead :: Text -> FilePath -> Int -> Int -> Parser ReadArguments
checkedRead corpus path start count=do
  _<-checkedCorpus corpus
  validPath corpus path
  unless (start>=1 && count>=1 && count<=500) (fail "Use startLine >= 1 and lineCount 1..500.")
  pure (ReadArguments corpus path start count)

-- | Typed reply codec preserving the existing docs_read representation.
-- Encoding a host-produced page and decoding it preserves every field.
readOutput :: Codec ReadPage
readOutput=Codec (objectSchema fields) (decodeValue parsePage) encodePage
  where
    fields=[("corpus",string),("path",string),("bytes",integer),("startLine",integer),
      ("lineCount",integer),("totalLines",integer),("text",string),("truncated",boolean),
      ("hasMore",boolean),("headings",array headingSchema),("headingsTruncated",boolean)]
    parsePage=withObject "documentation page" $ \o->ReadPage <$> o .: "corpus" <*> o .: "path" <*> o .: "bytes"
      <*> o .: "startLine" <*> o .: "lineCount" <*> o .: "totalLines" <*> o .: "text"
      <*> o .: "truncated" <*> o .: "hasMore" <*> (o .: "headings" >>= mapM parseHeading) <*> o .: "headingsTruncated"
    encodePage page=object ["corpus" .= pageCorpus page,"path" .= pagePath page,"bytes" .= pageBytes page,
      "startLine" .= pageStart page,"lineCount" .= pageCount page,"totalLines" .= pageTotal page,"text" .= pageText page,
      "truncated" .= pageTruncated page,"hasMore" .= pageHasMore page,
      "headings" .= map headingValue (pageHeadings page),"headingsTruncated" .= pageHeadingsTruncated page]

-- | Checked listing page. Offset is 0..10000 and limit is 1..100.
data ListArguments = ListArguments Text Int Int

-- | Construct a checked listing request without JSON.
listArguments :: Text -> Int -> Int -> Either Text ListArguments
listArguments corpus offset limit=checked (checkedList corpus offset limit)
-- | Captured corpus name.
listCorpus :: ListArguments -> Text
listCorpus (ListArguments corpus _ _)=corpus
-- | Zero-based page offset.
listOffset :: ListArguments -> Int
listOffset (ListArguments _ offset _)=offset
-- | Requested page size.
listLimit :: ListArguments -> Int
listLimit (ListArguments _ _ limit)=limit

-- | Strict listing codec; defaults are corpus @editor@, offset 0 and limit 50.
listInput :: Codec ListArguments
listInput=Codec (documentationSchema [] [("offset",integer),("limit",integer)]) (decodeValue parseArguments) encodeArguments
  where
    parseArguments=withObject "documentation list" $ \o->do
      corpus<-parseCorpus ["corpus","offset","limit"] o
      offset<-o .:? "offset" .!= 0
      limit<-o .:? "limit" .!= 50
      checkedList corpus offset limit
    encodeArguments (ListArguments corpus offset limit)=object ["corpus" .= corpus,"offset" .= offset,"limit" .= limit]

checkedList :: Text -> Int -> Int -> Parser ListArguments
checkedList corpus offset limit=do
  _<-checkedCorpus corpus
  checkedPage offset limit
  pure (ListArguments corpus offset limit)

-- | Concrete listing reply codec. Document entries contain metadata or a
-- bounded read error. Index truncation remains explicit rather than implying
-- that the returned page covers the complete corpus.
listOutput :: Codec Value
listOutput=valueOutput (objectSchema [("corpus",string),("documents",array documentSchema),("offset",integer),
  ("indexedCount",integer),("indexTruncated",boolean),("hasMore",boolean)]) $ withObject "documentation list reply" $ \o->do
    stringField o "corpus"
    entries<-o .: "documents"
    mapM_ parseDocument (entries :: [Value])
    intField o "offset"
    intField o "indexedCount"
    boolField o "indexTruncated"
    boolField o "hasMore"
  where
    documentSchema=object ["oneOf" .= [objectSchema [("path",string),("error",string)],
      objectSchema [("path",string),("bytes",integer),("title",string),("lineCount",integer),
        ("headings",array headingSchema),("headingsTruncated",boolean)]]]
    parseDocument=withObject "document metadata" $ \o->do
      stringField o "path"
      if KM.member "error" o then stringField o "error" else do
        intField o "bytes"
        stringField o "title"
        intField o "lineCount"
        entries<-o .: "headings"
        _<-mapM parseHeading (entries :: [Value])
        boolField o "headingsTruncated"

-- | Checked literal search request. Queries have 1..256 characters on one line;
-- an optional path has the same validation as a read request.
data SearchArguments = SearchArguments Text Text (Maybe FilePath) Int Int Bool

-- | Construct a checked literal search request without JSON.
searchArguments :: Text -> Text -> Maybe FilePath -> Int -> Int -> Bool -> Either Text SearchArguments
searchArguments corpus query path offset limit sensitive=checked (checkedSearch corpus query path offset limit sensitive)
-- | Captured corpus name.
searchCorpus :: SearchArguments -> Text
searchCorpus (SearchArguments corpus _ _ _ _ _)=corpus
-- | Literal search query.
searchQuery :: SearchArguments -> Text
searchQuery (SearchArguments _ query _ _ _ _)=query
-- | Optional validated documentation path.
searchPath :: SearchArguments -> Maybe FilePath
searchPath (SearchArguments _ _ path _ _ _)=path
-- | Zero-based match offset.
searchOffset :: SearchArguments -> Int
searchOffset (SearchArguments _ _ _ offset _ _)=offset
-- | Requested match count.
searchLimit :: SearchArguments -> Int
searchLimit (SearchArguments _ _ _ _ limit _)=limit
-- | Whether matching preserves case instead of case folding.
searchCaseSensitive :: SearchArguments -> Bool
searchCaseSensitive (SearchArguments _ _ _ _ _ sensitive)=sensitive

-- | Strict literal-search codec. Defaults are corpus @editor@, offset 0,
-- limit 20 and caseSensitive false. No regex or filesystem root is accepted.
searchInput :: Codec SearchArguments
searchInput=Codec (documentationSchema ["query"] [("query",string),("path",string),("offset",integer),
  ("limit",integer),("caseSensitive",boolean)]) (decodeValue parseArguments) encodeArguments
  where
    parseArguments=withObject "documentation search" $ \o->do
      corpus<-parseCorpus ["corpus","query","path","offset","limit","caseSensitive"] o
      query<-o .: "query"
      path<-o .:? "path"
      offset<-o .:? "offset" .!= 0
      limit<-o .:? "limit" .!= 20
      sensitive<-o .:? "caseSensitive" .!= False
      checkedSearch corpus query path offset limit sensitive
    encodeArguments (SearchArguments corpus query path offset limit sensitive)=object
      (["corpus" .= corpus,"query" .= query,"offset" .= offset,"limit" .= limit,"caseSensitive" .= sensitive]++
       ["path" .= value | Just value<-[path]])

checkedSearch :: Text -> Text -> Maybe FilePath -> Int -> Int -> Bool -> Parser SearchArguments
checkedSearch corpus query path offset limit sensitive=do
  _<-checkedCorpus corpus
  unless (not (T.null query) && T.length query<=256 && not (T.any (`elem` ['\0','\r','\n']) query))
    (fail "query must be 1..256 characters on one line.")
  mapM_ (validPath corpus) path
  checkedPage offset limit
  pure (SearchArguments corpus query path offset limit sensitive)

-- | Concrete search reply codec. Match lines, enclosing headings and explicit
-- truncation use the existing representation; searching does not consume pages.
searchOutput :: Codec Value
searchOutput=valueOutput (objectSchema [("corpus",string),("query",string),("matches",array matchSchema),
  ("offset",integer),("hasMore",boolean),("scannedFiles",integer),("skippedFiles",integer),("searchTruncated",boolean)]) $
  withObject "documentation search reply" $ \o->do
    stringField o "corpus"
    stringField o "query"
    entries<-o .: "matches"
    mapM_ parseMatch (entries :: [Value])
    intField o "offset"
    boolField o "hasMore"
    intField o "scannedFiles"
    intField o "skippedFiles"
    boolField o "searchTruncated"
  where
    matchSchema=objectSchema [("path",string),("line",integer),("text",string),("truncated",boolean),
      ("heading",object ["anyOf" .= [headingSchema,object ["type" .= ("null"::Text)]]])]
    parseMatch=withObject "documentation match" $ \o->do
      stringField o "path"
      intField o "line"
      stringField o "text"
      boolField o "truncated"
      mark<-o .: "heading"
      case mark of Null->pure (); value->parseHeading value >> pure ()

checked :: Parser a -> Either Text a
checked parser=either (Left . T.pack) Right (parseEither (const parser) ())
decodeValue :: (Value -> Parser a) -> Value -> Either Text a
decodeValue parser=either (Left . T.pack) Right . parseEither parser

parseCorpus :: [Key] -> Object -> Parser Text
parseCorpus allowed o=do
  unless (all (`elem` allowed) (KM.keys o)) (fail "Unknown documentation argument.")
  o .:? "corpus" .!= "editor" >>= checkedCorpus
checkedCorpus :: Text -> Parser Text
checkedCorpus corpus=do
  unless (corpus `elem` ["editor","thc"]) (fail "corpus must be editor or thc.")
  pure corpus
checkedPage :: Int -> Int -> Parser ()
checkedPage offset limit=unless (offset>=0 && offset<=10000 && limit>=1 && limit<=100)
  (fail "Use offset 0..10000 and limit 1..100.")

validPath :: Text -> FilePath -> Parser ()
validPath corpus path=unless (safeRelative path && allowedPath corpus path)
  (fail "Use a listed documentation path without traversal, absolute paths, or backslashes.")
safeRelative :: FilePath -> Bool
safeRelative path=not (null path) && length path<=4096 && not (isAbsolute path) &&
  not (any (`elem` ['\0','\\',':']) path) && all (`notElem` ["",".",".."]) (T.splitOn "/" (T.pack path))
allowedPath :: Text -> FilePath -> Bool
allowedPath corpus path=path=="README.md" || documentation "docs/" ||
  (corpus=="thc" && (path=="compiler/README.md" || documentation "compiler/docs/"))
  where documentation prefix=prefix `T.isPrefixOf` T.pack path && takeExtension path `elem` [".md",".txt",".rst"]

documentationSchema :: [Text] -> [(Key,Value)] -> Value
documentationSchema required properties=object ["type" .= ("object"::Text),"required" .= required,"additionalProperties" .= False,
  "properties" .= Object (KM.fromList (("corpus",object ["type" .= ("string"::Text),"enum" .= (["editor","thc"]::[Text]),"default" .= ("editor"::Text)]):properties))]
objectSchema :: [(Key,Value)] -> Value
objectSchema fields=object ["type" .= ("object"::Text),"required" .= map fst fields,
  "additionalProperties" .= False,"properties" .= Object (KM.fromList fields)]
string, integer, boolean :: Value
string=object ["type" .= ("string"::Text)]
integer=object ["type" .= ("integer"::Text)]
boolean=object ["type" .= ("boolean"::Text)]
array :: Value -> Value
array entry=object ["type" .= ("array"::Text),"items" .= entry]
headingSchema :: Value
headingSchema=object ["type" .= ("object"::Text),"required" .= (["line","level","title"]::[Text]),
  "properties" .= object ["line" .= integer,"level" .= integer,"title" .= string]]
parseHeading :: Value -> Parser Heading
parseHeading=withObject "heading" $ \h->Heading <$> h .: "line" <*> h .: "level" <*> h .: "title"
headingValue :: Heading -> Value
headingValue mark=object ["line" .= headingLine mark,"level" .= headingLevel mark,"title" .= headingText mark]

valueOutput :: Value -> (Value -> Parser ()) -> Codec Value
valueOutput schema validate=Codec schema (\value->decodeValue validate value >> pure value) id
stringField :: Object -> Key -> Parser ()
stringField o key=(o .: key :: Parser Text) >> pure ()
intField :: Object -> Key -> Parser ()
intField o key=(o .: key :: Parser Int) >> pure ()
boolField :: Object -> Key -> Parser ()
boolField o key=(o .: key :: Parser Bool) >> pure ()
