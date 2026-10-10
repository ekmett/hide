{-# LANGUAGE CPP, OverloadedStrings #-}
-- | Bounded offline documentation access for editor and compiler corpora.
--
-- The documentation command depends only on a corpus-root resolver, not on
-- Desktop or an MCP dispatcher. File validation and read budgets live here;
-- callers run this IO on their owning worker. Truncated searches report limits.
module Hide.Documentation
  ( docsTools, docsToolNames, DocsContext, ReadArguments, readArguments, ReadPage(..), Heading(..)
  , readCommand, readInput, readOutput, prepareDocs
  ) where

import Hide.FileIO (withFileRead)

import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath
import System.IO.Error (tryIOError)
import System.Timeout (timeout)
#ifndef mingw32_HOST_OS
import qualified System.Posix.Files as Posix
#endif
import Hide.Plugin.Command

docsToolNames :: [Text]
docsToolNames=["docs_list","docs_search","docs_read"]

-- | Schemas for listing, literal searching and reading documentation by line range.
docsTools :: [Value]
docsTools=[descriptor "docs_list" "List offline documentation with titles and Markdown headings. Paths are relative to the selected corpus; default corpus is editor." []
    [("offset",integer),("limit",integer)],
  descriptor "docs_search" "Search literal text in offline documentation, with line numbers and enclosing headings. Searches at most 256 files and 16 MiB; partial results are marked. No regex or network access." ["query"]
    [("query",string),("path",string),("offset",integer),("limit",integer),("caseSensitive",object ["type" .= ("boolean"::Text)])],
  descriptor "docs_read" "Read a line range from an offline document. Files must be UTF-8 and at most 1 MiB. Responses are capped at 128 Ki characters. Lines start at 1." ["path"]
    [("path",string),("startLine",integer),("lineCount",integer)]]
  where
    integer=object ["type" .= ("integer"::Text)]
    string=object ["type" .= ("string"::Text)]
    descriptor name description required properties=object ["name" .= (name::Text),"description" .= (description::Text),
      "inputSchema" .= (if name=="docs_read" then codecSchema readInput else documentationSchema required properties),
      "annotations" .= object ["readOnlyHint" .= True,"destructiveHint" .= False,"openWorldHint" .= False]]

documentationSchema :: [Text] -> [(Key,Value)] -> Value
documentationSchema required properties=object ["type" .= ("object"::Text),"required" .= required,"additionalProperties" .= False,
  "properties" .= Object (KM.fromList (("corpus",object ["type" .= ("string"::Text),"enum" .= (["editor","thc"]::[Text]),"default" .= ("editor"::Text)]):properties))]

readSchema :: Value
readSchema=documentationSchema ["path"] [("path",object ["type" .= ("string"::Text)]),
  ("startLine",object ["type" .= ("integer"::Text)]),("lineCount",object ["type" .= ("integer"::Text)])]

-- | Host-granted capability for locating the selected documentation corpus.
-- Resolve on the calling worker; it may read project configuration.
type DocsContext = Text -> IO FilePath

data ReadArguments = ReadArguments Text FilePath Int Int

data ReadPage = ReadPage
  { pageCorpus :: Text, pagePath :: FilePath, pageBytes :: Int
  , pageStart :: Int, pageCount :: Int, pageTotal :: Int, pageText :: Text
  , pageTruncated :: Bool, pageHasMore :: Bool
  , pageHeadings :: [Heading], pageHeadingsTruncated :: Bool
  }

-- | Read a bounded UTF-8 document range using only the granted corpus resolver.
readCommand :: CommandDef DocsContext ReadArguments ReadPage
readCommand=CommandDef "hide.docs.read" "Read documentation" readInput readOutput $ \resolve (ReadArguments corpus path start count)->do
  root<-resolve corpus
  result<-readDocument root path
  pure (either (Left . CommandRejected) (Right . \(bytes,text)->readPage corpus path bytes text start count) result)

-- | Validated documentation path and one-based range, with explicit wire schema.
readInput :: Codec ReadArguments
readInput=Codec readSchema (either (Left . T.pack) Right . parseEither parseArguments) encodeArguments
  where
    parseArguments=withObject "documentation read" $ \o->do
      corpus<-parseCorpus ["corpus","path","startLine","lineCount"] o
      path<-o .: "path"
      start<-o .:? "startLine" .!= 1
      count<-o .:? "lineCount" .!= 200
      checkedArguments corpus path start count
    encodeArguments (ReadArguments corpus path start count)=object
      ["corpus" .= corpus,"path" .= path,"startLine" .= start,"lineCount" .= count]

-- | Construct the same checked arguments without passing through JSON.
readArguments :: Text -> FilePath -> Int -> Int -> Either Text ReadArguments
readArguments corpus path start count=either (Left . T.pack) Right
  (parseEither (const (checkedArguments corpus path start count)) ())

checkedArguments :: Text -> FilePath -> Int -> Int -> Parser ReadArguments
checkedArguments corpus path start count=do
  unless (corpus `elem` ["editor","thc"]) (fail "corpus must be editor or thc.")
  validPath corpus path
  unless (start>=1 && count>=1 && count<=500) (fail "Use startLine >= 1 and lineCount 1..500.")
  pure (ReadArguments corpus path start count)

-- | Typed reply shared by native callers and the MCP adapter.
readOutput :: Codec ReadPage
readOutput=Codec schema (either (Left . T.pack) Right . parseEither parsePage) encodePage
  where
    schema=object ["type" .= ("object"::Text),"required" .= map fst fields,
      "additionalProperties" .= False,"properties" .= Object (KM.fromList fields)]
    fields=[("corpus",string),("path",string),("bytes",integer),("startLine",integer),
      ("lineCount",integer),("totalLines",integer),("text",string),("truncated",boolean),
      ("hasMore",boolean),("headings",object ["type" .= ("array"::Text),"items" .= object
        ["type" .= ("object"::Text),"required" .= (["line","level","title"]::[Text]),
         "properties" .= object ["line" .= integer,"level" .= integer,"title" .= string]]]),
      ("headingsTruncated",boolean)]
    integer=object ["type" .= ("integer"::Text)]
    string=object ["type" .= ("string"::Text)]
    boolean=object ["type" .= ("boolean"::Text)]
    parsePage=withObject "documentation page" $ \o->ReadPage <$> o .: "corpus" <*> o .: "path" <*> o .: "bytes"
      <*> o .: "startLine" <*> o .: "lineCount" <*> o .: "totalLines" <*> o .: "text"
      <*> o .: "truncated" <*> o .: "hasMore" <*> (o .: "headings" >>= mapM (withObject "heading" $ \h->
        Heading <$> h .: "line" <*> h .: "level" <*> h .: "title")) <*> o .: "headingsTruncated"
    encodePage page=object ["corpus" .= pageCorpus page,"path" .= pagePath page,"bytes" .= pageBytes page,
      "startLine" .= pageStart page,"lineCount" .= pageCount page,"totalLines" .= pageTotal page,"text" .= pageText page,
      "truncated" .= pageTruncated page,"hasMore" .= pageHasMore page,
      "headings" .= map headingValue (pageHeadings page),"headingsTruncated" .= pageHeadingsTruncated page]

data Request = ListDocs Int Int | SearchDocs Text (Maybe FilePath) Int Int Bool

-- | Listing and search retain their bounded implementation while read goes
-- through the typed registry. Perform all resolution and IO outside UI locks.
prepareDocs :: DocsContext -> Text -> Value -> IO (Either Text Value)
prepareDocs resolve name arguments=case parseEither (parseRequest name) arguments of
  Left err -> pure (Left (T.pack err))
  Right (corpus,request) -> do
    result<-tryIOError (execute resolve corpus request)
    pure (either (Left . T.pack . show) id result)

parseCorpus :: [Key] -> Object -> Parser Text
parseCorpus allowed o=do
  unless (all (`elem` allowed) (KM.keys o)) (fail "Unknown documentation argument.")
  corpus<-o .:? "corpus" .!= "editor"
  unless (corpus `elem` ["editor","thc"]) (fail "corpus must be editor or thc.")
  pure corpus

parseRequest :: Text -> Value -> Parser (Text,Request)
parseRequest name=withObject "documentation arguments" $ \o->do
  let allowed=case name of
        "docs_list" -> ["corpus","offset","limit"]
        _ -> ["corpus","query","path","offset","limit","caseSensitive"]
  unless (name `elem` ["docs_list","docs_search"]) (fail "Unknown documentation tool.")
  corpus<-parseCorpus allowed o
  request<-case name of
    "docs_list" -> do
      offset<-o .:? "offset" .!= 0
      limit<-o .:? "limit" .!= 50
      page offset limit
      pure (ListDocs offset limit)
    _ -> do
      query<-o .: "query"
      unless (not (T.null query) && T.length query<=256 && not (T.any (`elem` ['\0','\r','\n']) query)) (fail "query must be 1..256 characters on one line.")
      path<-o .:? "path"
      mapM_ (validPath corpus) path
      offset<-o .:? "offset" .!= 0
      limit<-o .:? "limit" .!= 20
      page offset limit
      SearchDocs query path offset limit <$> o .:? "caseSensitive" .!= False
  pure (corpus,request)
  where
    page offset limit=unless (offset>=0 && offset<=10000 && limit>=1 && limit<=100)
      (fail "Use offset 0..10000 and limit 1..100.")

validPath :: Text -> FilePath -> Parser ()
validPath corpus path=unless (safeRelative path && allowedPath corpus path) (fail "Use a listed documentation path without traversal, absolute paths, or backslashes.")

safeRelative :: FilePath -> Bool
safeRelative path=not (null path) && length path<=4096 && not (isAbsolute path) &&
  not (any (`elem` ['\0','\\',':']) path) && all (`notElem` ["",".",".."]) (T.splitOn "/" (T.pack path))

allowedPath :: Text -> FilePath -> Bool
allowedPath corpus path=path=="README.md" || documentation "docs/" ||
  (corpus=="thc" && (path=="compiler/README.md" || documentation "compiler/docs/"))
  where documentation prefix=prefix `T.isPrefixOf` T.pack path && takeExtension path `elem` [".md",".txt",".rst"]

insideRoot :: FilePath -> FilePath -> Bool
insideRoot root path=let relative=makeRelative root path in not (isAbsolute relative) && ".." `notElem` splitDirectories relative

-- Reject every symlink component, including directory links, and verify the
-- canonical path before reading. Documentation mounts never include sources or
-- arbitrary files in the compiler checkout.
resolveDoc :: FilePath -> FilePath -> IO FilePath
resolveDoc root relative=do
  let components=map T.unpack (T.splitOn "/" (T.pack relative))
      prefixes=drop 1 (scanl (</>) root components)
  linked<-or <$> mapM pathIsSymbolicLink prefixes
  when linked (ioError (userError "Documentation symlinks are not followed."))
  absolute<-canonicalizePath (foldl (</>) root components)
  unless (insideRoot root absolute) (ioError (userError "Documentation path escapes its corpus."))
  pure absolute

maxFileBytes :: Int
maxFileBytes=1048576

readDocument :: FilePath -> FilePath -> IO (Either Text (Int,Text))
readDocument root path=do
  result<-tryIOError $ do
    absolute<-resolveDoc root path
#ifndef mingw32_HOST_OS
    regular<-Posix.isRegularFile <$> Posix.getFileStatus absolute
    unless regular (ioError (userError "Documentation must be a regular file."))
#endif
    size<-getFileSize absolute
    when (size>fromIntegral maxFileBytes) (ioError (userError "Document exceeds the 1 MiB file limit."))
    bytes<-timeout 5000000 (withFileRead absolute (\handle->BS.hGet handle (maxFileBytes+1)))
    case bytes of
      Nothing -> ioError (userError "Document read timed out.")
      Just content | BS.length content>maxFileBytes -> ioError (userError "Document exceeds the 1 MiB file limit.")
      Just content -> case TE.decodeUtf8' content of
        Left _ -> ioError (userError "Document is not valid UTF-8.")
        Right text -> pure (BS.length content,text)
  pure (either (Left . T.pack . show) Right result)

-- Limit directory visits, nesting and indexed files. A truncated index is
-- explicitly reported rather than pretending all documentation was searched.
indexDocs :: FilePath -> Text -> IO ([FilePath],Bool)
indexDocs root corpus=do
  let starts=["README.md","docs"]++(if corpus=="thc" then ["compiler/README.md","compiler/docs"] else [])
  walk 2048 [] False [(path,0::Int) | path<-starts]
  where
    walk _ found truncated []=pure (sort found,truncated)
    walk budget found _ _ | budget<=0 || length found>=1024=pure (sort found,True)
    walk budget found truncated ((path,depth):rest)=do
      checked<-tryIOError $ do
        absolute<-resolveDoc root path
        directory<-doesDirectoryExist absolute
        if directory then do
          names<-sort <$> listDirectory absolute
          pure (Left [path++"/"++name | name<-take (budget-1) names],length names>=budget)
        else do
          exists<-doesFileExist absolute
          pure (Right (exists && allowedPath corpus path),False)
      case checked of
        Left _ -> walk (budget-1) found truncated rest
        Right (Right eligible,_) -> walk (budget-1) ([path | eligible]++found) truncated rest
        Right (Left children,cut) | depth>=16 -> walk (budget-1) found (truncated || not (null children)) rest
                                  | otherwise -> walk (budget-1) found (truncated || cut) ([(child,depth+1) | child<-children]++rest)

execute :: DocsContext -> Text -> Request -> IO (Either Text Value)
execute resolve corpus request=do
  root<-resolve corpus
  case request of
    ListDocs offset limit -> do
      (paths,indexTruncated)<-indexDocs root corpus
      documents<-mapM (metadata root) (take limit (drop offset paths))
      pure (Right (object ["corpus" .= corpus,"documents" .= documents,"offset" .= offset,
        "indexedCount" .= length paths,"indexTruncated" .= indexTruncated,"hasMore" .= (offset+limit<length paths)]))
    SearchDocs query selected offset limit caseSensitive -> do
      (paths,indexTruncated)<-maybe (indexDocs root corpus) (\path->pure ([path],False)) selected
      (found,scanned,skipped,cut)<-searchFiles root query caseSensitive (offset+limit+1) (take 256 paths) (16*maxFileBytes) [] 0 0
      pure (Right (object ["corpus" .= corpus,"query" .= query,"matches" .= take limit (drop offset found),"offset" .= offset,
        "hasMore" .= (length found>offset+limit),"scannedFiles" .= scanned,"skippedFiles" .= skipped,
        "searchTruncated" .= (indexTruncated || length paths>256 || cut)]))
  where
    metadata root path=do
      loaded<-readDocument root path
      pure $ case loaded of
        Left err -> object ["path" .= path,"error" .= T.take 1024 err]
        Right (bytes,text) -> let marks=headings text in object ["path" .= path,"bytes" .= bytes,
          "title" .= maybe (T.pack (takeFileName path)) headingText (case marks of mark:_->Just mark; _->Nothing),
          "lineCount" .= length (docLines text),"headings" .= map headingValue (take 64 marks),"headingsTruncated" .= (length marks>64)]

readPage :: Text -> FilePath -> Int -> Text -> Int -> Int -> ReadPage
readPage corpus path bytes text start count=ReadPage corpus path bytes start
  (if cut then length (docLines excerpt) else length selected) (length lines') excerpt
  cut (cut || start-1+length selected<length lines') (take 64 marks) (length marks>64)
  where
    lines'=docLines text
    selected=take count (drop (start-1) lines')
    chosen=T.intercalate "\n" selected
    excerpt=T.take 131072 chosen
    cut=T.length chosen>131072
    marks=headings text

docLines :: Text -> [Text]
docLines text=if T.null text then [] else T.splitOn "\n" text

data Heading = Heading { headingLine :: Int, headingLevel :: Int, headingText :: Text }
headingValue :: Heading -> Value
headingValue mark=object ["line" .= headingLine mark,"level" .= headingLevel mark,"title" .= headingText mark]

headings :: Text -> [Heading]
headings text=reverse (snd (foldl step (Nothing,[]) (zip [1..] (docLines text))))
  where
    step (fence,found) (line,raw)=
      let stripped=T.strip raw
          delimiter=case T.uncons stripped of Just (c,_) | c `elem` ['`','~'],T.length (T.takeWhile (==c) stripped)>=3->Just c; _->Nothing
      in case (fence,delimiter) of
        (Just c,Just d) | c==d -> (Nothing,found)
        (Just _,_) -> (fence,found)
        (Nothing,Just c) -> (Just c,found)
        _ -> let level=T.length (T.takeWhile (=='#') stripped)
                 suffix=T.drop level stripped
             in if T.length (T.takeWhile (==' ') raw)<=3 && level>=1 && level<=6 && (T.null suffix || " " `T.isPrefixOf` suffix)
                then (Nothing,Heading line level (T.take 256 (T.strip (T.dropWhileEnd (=='#') (T.strip suffix)))):found)
                else (Nothing,found)

-- Reserve one file allowance before each read, including failed UTF-8 reads,
-- so malformed files cannot bypass the aggregate search budget.
searchFiles :: FilePath -> Text -> Bool -> Int -> [FilePath] -> Int -> [Value] -> Int -> Int -> IO ([Value],Int,Int,Bool)
searchFiles _ _ _ _ [] _ found scanned skipped=pure (found,scanned,skipped,False)
searchFiles _ _ _ maximumMatches _ budget found scanned skipped | budget<maxFileBytes || length found>=maximumMatches=pure (found,scanned,skipped,True)
searchFiles root query sensitive maximumMatches (path:rest) budget found scanned skipped=do
  loaded<-readDocument root path
  case loaded of
    Left _ -> searchFiles root query sensitive maximumMatches rest (budget-maxFileBytes) found scanned (skipped+1)
    Right (bytes,text) | bytes>budget -> pure (found,scanned,skipped,True)
                       | otherwise -> do
      let normalize=if sensitive then id else T.toCaseFold
          needle=normalize query
          marks=headings text
          matches=[object ["path" .= path,"line" .= line,"text" .= T.take 2048 (T.stripEnd content),
            "truncated" .= (T.length content>2048),"heading" .= fmap headingValue (lastHeading line marks)]
            | (line,content)<-zip [1::Int ..] (docLines text),needle `T.isInfixOf` normalize content]
          combined=take maximumMatches (found++matches)
      searchFiles root query sensitive maximumMatches rest (budget-bytes) combined (scanned+1) skipped
  where
    lastHeading line=foldl (\current mark->if headingLine mark<=line then Just mark else current) Nothing
