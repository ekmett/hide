{-# LANGUAGE CPP, OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Documentation
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : CPP, OverloadedStrings
--
-- Bounded offline document operations for host-selected corpora.
-- The public API supplies checked arguments and codecs; this module owns path
-- confinement, native file reads and traversal budgets. All IO runs on the
-- invoking worker. Help and plugin tools use the same scoped operations.
module Hide.Documentation (DocsContext, readCommand, listCommand, searchCommand) where

import Hide.FileIO (withFileRead)

import Control.Monad (unless, when)
import Data.Aeson
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
import Hide.Plugin.Documentation

-- | Host-selected corpus roots. Resolve on the calling worker, never on input.
type DocsContext = Text -> IO FilePath

-- | The same scoped read is used by Help and plugin tools.
readCommand :: CommandDef DocsContext ReadArguments ReadPage
readCommand=CommandDef "hide.docs.read" "Read documentation" readInput readOutput $ \resolve arguments->do
  root<-resolve (readCorpus arguments)
  result<-readDocument root (readPath arguments)
  pure (either (Left . CommandRejected)
    (Right . \(bytes,text)->readPage (readCorpus arguments) (readPath arguments) bytes text
      (readStartLine arguments) (readLineCount arguments)) result)

-- | Listing has the same registration lifetime as reading. The index retains
-- its traversal and metadata budgets; a partial index is reported explicitly.
listCommand :: CommandDef DocsContext ListArguments Value
listCommand=CommandDef "hide.docs.list" "List documentation" listInput listOutput $ \resolve arguments->do
  let corpus=listCorpus arguments
      offset=listOffset arguments
      limit=listLimit arguments
  root<-resolve corpus
  (paths,indexTruncated)<-indexDocs root corpus
  documents<-mapM (metadata root) (take limit (drop offset paths))
  pure (Right (object ["corpus" .= corpus,"documents" .= documents,"offset" .= offset,
    "indexedCount" .= length paths,"indexTruncated" .= indexTruncated,"hasMore" .= (offset+limit<length paths)]))
  where
    metadata root path=do
      loaded<-readDocument root path
      pure $ case loaded of
        Left err -> object ["path" .= path,"error" .= T.take 1024 err]
        Right (bytes,text) -> let marks=headings text in object ["path" .= path,"bytes" .= bytes,
          "title" .= maybe (T.pack (takeFileName path)) headingText (case marks of mark:_->Just mark; _->Nothing),
          "lineCount" .= length (docLines text),"headings" .= map headingValue (take 64 marks),"headingsTruncated" .= (length marks>64)]

-- | Literal search with bounded files, bytes and returned matches. Retiring the
-- registration rejects a queued request before resolving or reading a corpus.
searchCommand :: CommandDef DocsContext SearchArguments Value
searchCommand=CommandDef "hide.docs.search" "Search documentation" searchInput searchOutput $ \resolve arguments->do
  let corpus=searchCorpus arguments
      query=searchQuery arguments
      offset=searchOffset arguments
      limit=searchLimit arguments
  root<-resolve corpus
  (paths,indexTruncated)<-maybe (indexDocs root corpus) (\path->pure ([path],False)) (searchPath arguments)
  (found,scanned,skipped,cut)<-searchFiles root query (searchCaseSensitive arguments) (offset+limit+1)
    (take 256 paths) (16*maxFileBytes) [] 0 0
  pure (Right (object ["corpus" .= corpus,"query" .= query,"matches" .= take limit (drop offset found),"offset" .= offset,
    "hasMore" .= (length found>offset+limit),"scannedFiles" .= scanned,"skippedFiles" .= skipped,
    "searchTruncated" .= (indexTruncated || length paths>256 || cut)]))

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
          pure (Right (exists && either (const False) (const True) (readArguments corpus path 1 1)),False)
      case checked of
        Left _ -> walk (budget-1) found truncated rest
        Right (Right eligible,_) -> walk (budget-1) ([path | eligible]++found) truncated rest
        Right (Left children,cut) | depth>=16 -> walk (budget-1) found (truncated || not (null children)) rest
                                  | otherwise -> walk (budget-1) found (truncated || cut) ([(child,depth+1) | child<-children]++rest)

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
