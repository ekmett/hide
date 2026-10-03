{-# LANGUAGE OverloadedStrings #-}
-- | Immutable file/context capture and checked ACP writes.
--
-- Eligible open source buffers override bounded UTF-8 disk reads. Stable buffer
-- and file identities let worker results be invalidated without comparing source
-- text during routine adoption. Explicit approved writes separately verify the
-- captured editor/disk baselines and use checked saving; they preserve Undo.
module Hide.AgentFiles (Snapshot, captureFile, snapshotPath, snapshotText, acceptWrite, SourceIdentity, sourceIdentity, sourceSnapshots, contextText) where

import Control.Exception (IOException, try, evaluate)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath)
import System.FilePath (isAbsolute, makeRelative, splitDirectories)
import System.IO (IOMode(ReadMode), withBinaryFile)
import System.Mem.StableName (StableName, makeStableName)
import System.IO.Error (catchIOError, isDoesNotExistError)
import Hide.Buffer
import Hide.Files
import Hide.GuestAccess (protectedPath, protectedBuffer)
import Hide.Model

-- | Captured text with its canonical path, disk baseline and optional live-buffer version.
data Snapshot = Snapshot FileState (Maybe (Int,Int)) Text deriving (Eq,Show)
snapshotPath :: Snapshot -> FilePath
snapshotPath (Snapshot file _ _) = filePath file

snapshotText :: Snapshot -> Text
snapshotText (Snapshot _ _ text) = text

fileLimit :: Int
fileLimit = 16*1024*1024

textTooLarge :: Text -> Bool
textTooLarge text = T.length text>fileLimit || BS.length (TE.encodeUtf8 text)>fileLimit

-- | Shallow immutable buffer/file identities plus revision for stale-work checks.
-- Replaced-but-equal values conservatively require a fresh capture.
data SourceIdentity = SourceIdentity !Int !Int (StableName Buffer) (StableName FileState) deriving Eq

sourceIdentity :: FilePath -> Desktop -> IO (Maybe SourceIdentity)
sourceIdentity path d = case find (\(_,file,_)->filePath file==path) (reverse (publicSources d)) of
  Nothing -> pure Nothing
  Just (bid,file,buffer) -> do
    current<-evaluate buffer >>= makeStableName
    baseline<-evaluate file >>= makeStableName
    pure (Just (SourceIdentity bid (revision buffer) current baseline))

sourceSnapshots :: Desktop -> M.Map FilePath Snapshot
sourceSnapshots d = M.fromList [(filePath file,Snapshot file (Just (bid,revision b)) (contents b)) |
  (bid,file,b)<-publicSources d]

-- Ascending IDs plus Map.fromList make the last eligible duplicate path win;
-- sourceIdentity uses the same selection without flattening any source text.
publicSources :: Desktop -> [(Int,FileState,Buffer)]
publicSources d = [(bid,file,b) | (bid,doc)<-M.toAscList (buffers d),not (protectedBuffer d bid),
  documentLabel doc==Nothing,textBuffer (documentBuffer doc),Just file<-[documentFile doc],let b=documentBuffer doc]

-- | Capture an absolute canonical project-contained file, excluding private paths
-- and byte buffers. A missing disk file is represented by empty text.
captureFile :: FilePath -> FilePath -> Desktop -> IO (Either Text Snapshot)
captureFile root path d = do
  result <- try $ do
    unless (isAbsolute path && '\0' `notElem` path) (ioError (userError "Expected an absolute file path."))
    resolved <- canonicalizePath path
    when (protectedPath d resolved) (ioError (userError "Agent authority files are private; use agent_settings for public context."))
    base <- canonicalizePath root
    let relative=makeRelative base resolved
    when (isAbsolute relative || ".." `elem` splitDirectories relative) (ioError (userError "File is outside this session's project."))
    when (any (\doc -> fmap filePath (documentFile doc)==Just resolved && not (textBuffer (documentBuffer doc))) (M.elems (buffers d)))
      (ioError (userError "Hex buffers are not available through ACP text file APIs."))
    captured <- case M.lookup resolved (sourceSnapshots d) of
      Just captured -> pure captured
      Nothing -> do
        -- Bound the read itself: checking size before an unbounded read races
        -- with a file growing between stat and read.
        bytes <- catchIOError (Just <$> withBinaryFile resolved ReadMode (\handle -> BS.hGet handle (fileLimit+1)))
          (\err -> if isDoesNotExistError err then pure Nothing else ioError err)
        let raw=maybe BS.empty id bytes
        when (BS.length raw>fileLimit) (ioError (userError "ACP text files are limited to 16 MiB."))
        when (BS.elem 0 raw) (ioError (userError "Binary text contains NUL bytes."))
        text <- either (const (ioError (userError "The file is not valid UTF-8."))) pure (TE.decodeUtf8' raw)
        pure (Snapshot (FileState resolved bytes) Nothing text)
    when (textTooLarge (snapshotText captured)) (ioError (userError "ACP text files are limited to 16 MiB."))
    current <- canonicalizePath path
    unless (current==resolved) (ioError (userError "File path changed while reading; request a fresh read."))
    pure captured
  pure (either (Left . T.pack . show) Right (result :: Either IOException Snapshot))

-- | Validate an approved write against captured editor and disk state, then save it.
-- This explicit write boundary can compare full contents; it is not a redraw check.
acceptWrite :: Snapshot -> Text -> Desktop -> IO (Either Text Desktop)
acceptWrite (Snapshot file expected oldText) text d
  | protectedPath d (filePath file) = pure (Left "Agent authority files require human input.")
  | T.any (=='\0') text = pure (Left "Text contains NUL bytes.")
  | textTooLarge text = pure (Left "ACP text files are limited to 16 MiB.")
  | otherwise = case current of
      Left err -> pure (Left err)
      Right (bid,b,opened) -> do
        let edited=replaceSelection (Selection 0 (bufferLength b)) text b
        savedFile <- saveFile file edited
        pure $ case savedFile of
          Left err -> Left (T.pack err)
          Right latest -> Right opened
            {buffers=M.adjust (\doc -> restyle doc {documentBuffer=markSaved edited,documentFile=Just latest}) bid (buffers opened)
            ,windows=map (clamp bid (bufferLength edited)) (windows opened)
            ,status="Agent edit saved; Undo restores the previous buffer."}
  where
    matching=find (\(_,doc) -> fmap filePath (documentFile doc)==Just (filePath file)) (M.toList (buffers d))
    current=case (expected,matching) of
      (Just (bid,version),Just (now,doc))
        | bid==now,documentLabel doc==Nothing,textBuffer (documentBuffer doc),revision (documentBuffer doc)==version,
          contents (documentBuffer doc)==oldText,documentFile doc==Just file -> Right (bid,documentBuffer doc,d)
      (Nothing,Nothing) -> let opened=addDocument (Just file) (newBuffer oldText) d
        in Right (nextId d,newBuffer oldText,opened)
      _ -> Left "File changed in the editor after the agent read it; request a fresh read."
    clamp bid size w | bufferId w==bid = w {selection=Selection (max 0 (min size (anchor (selection w)))) (max 0 (min size (caret (selection w)))),scrollRow=0,scrollColumn=0}
                     | otherwise = w

contextText :: Bool -> Bool -> Bool -> Desktop -> Text
contextText selectionOnly wholeFile includeDiagnostics d = T.intercalate "\n\n" (fileContext++problemContext)
  where
    fileContext=case (activeWindow d,activeDocument d) of
      (Just w,Just doc) | not (protectedBuffer d (bufferId w)), documentLabel doc==Nothing, textBuffer (documentBuffer doc) ->
        let b=documentBuffer doc; Selection a z=selection w
            path=maybe "Unsaved buffer" (T.pack . filePath) (documentFile doc)
            selected=T.take (abs (z-a)) (T.drop (min a z) (contents b))
        in ["File: "<>path<>"\n"<>contents b | wholeFile] ++
           ["Selection in "<>path<>"\n"<>selected | selectionOnly && not wholeFile && a/=z]
      _ -> []
    problemContext=["Diagnostics:\n"<>T.unlines [T.pack (diagnosticPath p)<>":"<>T.pack (show (diagnosticRow p+1))<>": "<>diagnosticMessage p | p<-diagnostics d] | includeDiagnostics]
