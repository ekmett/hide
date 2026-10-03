{-# LANGUAGE CPP, OverloadedStrings #-}
module Hide.DocsMCP (docsTools, docsToolNames, docsTool) where

import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Paths_hide (getDataFileName)
import System.Directory
import System.FilePath
import System.IO (IOMode(ReadMode), withBinaryFile)
import System.IO.Error (tryIOError)
import System.Timeout (timeout)
#ifndef mingw32_HOST_OS
import qualified System.Posix.Files as Posix
#endif
import qualified Hide.Build as Build
import Hide.Model (Desktop)

docsToolNames :: [Text]
docsToolNames=["docs_list","docs_search","docs_read"]

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
      "inputSchema" .= object ["type" .= ("object"::Text),"required" .= (required::[Text]),"additionalProperties" .= False,
        "properties" .= Object (KM.fromList (("corpus",object ["type" .= ("string"::Text),"enum" .= (["editor","thc"]::[Text]),"default" .= ("editor"::Text)]):properties))],
      "annotations" .= object ["readOnlyHint" .= True,"destructiveHint" .= False,"openWorldHint" .= False]]

data Request = ListDocs Int Int | SearchDocs Text (Maybe FilePath) Int Int Bool | ReadDoc FilePath Int Int

-- Capture the desktop under its lock, then perform all filesystem work outside.
docsTool :: Desktop -> Text -> Value -> IO (Desktop,IO (Either Text Value))
docsTool desktop name arguments=pure (desktop,case parseEither (parseRequest name) arguments of
  Left err -> pure (Left (T.pack err))
  Right (corpus,request) -> do
    result<-tryIOError (execute desktop corpus request)
    pure (either (Left . T.pack . show) id result))

parseRequest :: Text -> Value -> Parser (Text,Request)
parseRequest name=withObject "documentation arguments" $ \o->do
  let allowed=case name of
        "docs_list" -> ["corpus","offset","limit"]
        "docs_search" -> ["corpus","query","path","offset","limit","caseSensitive"]
        "docs_read" -> ["corpus","path","startLine","lineCount"]
        _ -> []
  unless (name `elem` docsToolNames) (fail "Unknown documentation tool.")
  unless (all (`elem` allowed) (KM.keys o)) (fail "Unknown documentation argument.")
  corpus<-o .:? "corpus" .!= "editor"
  unless (corpus `elem` ["editor","thc"]) (fail "corpus must be editor or thc.")
  request<-case name of
    "docs_list" -> do
      offset<-o .:? "offset" .!= 0
      limit<-o .:? "limit" .!= 50
      page offset limit 100
      pure (ListDocs offset limit)
    "docs_search" -> do
      query<-o .: "query"
      unless (not (T.null query) && T.length query<=256 && not (T.any (`elem` ['\0','\r','\n']) query)) (fail "query must be 1..256 characters on one line.")
      path<-o .:? "path"
      mapM_ (validPath corpus) path
      offset<-o .:? "offset" .!= 0
      limit<-o .:? "limit" .!= 20
      page offset limit 100
      SearchDocs query path offset limit <$> o .:? "caseSensitive" .!= False
    _ -> do
      path<-o .: "path"
      validPath corpus path
      start<-o .:? "startLine" .!= 1
      count<-o .:? "lineCount" .!= 200
      unless (start>=1 && count>=1 && count<=500) (fail "Use startLine >= 1 and lineCount 1..500.")
      pure (ReadDoc path start count)
  pure (corpus,request)
  where
    page offset limit maximumLimit=unless (offset>=0 && offset<=10000 && limit>=1 && limit<=maximumLimit)
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

corpusRoot :: Desktop -> Text -> IO FilePath
corpusRoot _ "editor"=getDataFileName "README.md" >>= canonicalizePath . takeDirectory
corpusRoot desktop _=do
  directory<-getXdgDirectory XdgConfig "thc-edit"
  project<-Build.resolveBuildRoot desktop
  config<-Build.loadBuildConfig directory project
  let configured=T.unpack (T.strip (Build.buildTHCRoot config))
  when (null configured) (ioError (userError "Compiler docs are unavailable: set THC_ROOT or the build target's THC root."))
  root<-canonicalizePath configured
  exists<-doesDirectoryExist root
  unless exists (ioError (userError "Configured THC root does not exist."))
  pure root

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
    bytes<-timeout 5000000 (withBinaryFile absolute ReadMode (\handle->BS.hGet handle (maxFileBytes+1)))
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

execute :: Desktop -> Text -> Request -> IO (Either Text Value)
execute desktop corpus request=do
  root<-corpusRoot desktop corpus
  case request of
    ReadDoc path start count -> fmap (fmap (\(bytes,text)->readValue corpus path bytes text start count)) (readDocument root path)
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

readValue :: Text -> FilePath -> Int -> Text -> Int -> Int -> Value
readValue corpus path bytes text start count=object ["corpus" .= corpus,"path" .= path,"bytes" .= bytes,
  "startLine" .= start,"lineCount" .= (if T.length chosen>131072 then length (docLines excerpt) else length selected),"totalLines" .= length lines',"text" .= excerpt,
  "truncated" .= (T.length chosen>131072),"hasMore" .= (T.length chosen>131072 || start-1+length selected<length lines'),
  "headings" .= map headingValue (take 64 (headings text)),"headingsTruncated" .= (length (headings text)>64)]
  where
    lines'=docLines text
    selected=take count (drop (start-1) lines')
    chosen=T.intercalate "\n" selected
    excerpt=T.take 131072 chosen

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
