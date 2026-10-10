-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.AgentFiles
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Immutable file/context capture and checked ACP writes.
--
-- Eligible open source buffers override bounded UTF-8 disk reads. Stable buffer
-- and file identities let worker results be invalidated without comparing source
-- text during routine adoption. Explicit approved writes separately verify the
-- captured editor/disk baselines and use checked saving; they preserve Undo.
module Hide.AgentFiles
  ( Snapshot, snapshotPath, snapshotText, acceptWrite
  , ResolvedFile, resolveFile, resolvedFilePath
  , FileInput, captureFileInput, fileInputIdentity, readFileInput
  , SourceIdentity, sourceIdentity, sourceSnapshots, contextText
  ) where

import Hide.FileIO (withFileRead)

import Control.Exception (IOException, try, evaluate)
import Control.DeepSeq (force)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath)
import System.FilePath (isAbsolute, makeRelative, splitDirectories)
import System.Mem.StableName (StableName, makeStableName)
import System.IO.Error (catchIOError, isDoesNotExistError)
import Hide.Buffer
import Hide.Plugin.Buffer (ContentVersion)
import Hide.Plugin.BufferHost (captureVersion)
import Hide.Files
import Hide.GuestAccess (protectedPath, protectedBuffer, protectedWindow)
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
data SourceIdentity = SourceIdentity !Int !ContentVersion (StableName FileState) deriving Eq

sourceIdentity :: FilePath -> Desktop -> IO (Maybe SourceIdentity)
sourceIdentity path d = case find eligible (reverse (matchingSources path d)) of
  Nothing -> pure Nothing
  Just (bid,doc,file) -> do
    current<-captureVersion (documentBuffer doc)
    baseline<-evaluate file >>= makeStableName
    pure (Just (SourceIdentity bid current baseline))
  where eligible (bid,doc,_)=not (protectedBuffer d bid) && documentLabel doc==Nothing && textBuffer (documentBuffer doc)

-- Filter on path metadata before inspecting a buffer. Unrelated contents and
-- histories are not part of a single-file capture or its final identity check.
matchingSources :: FilePath -> Desktop -> [(Int,Document,FileState)]
matchingSources path d=[(bid,doc,file) | (bid,doc)<-M.toAscList (buffers d),
  Just file<-[documentFile doc],filePath file==path]

sourceSnapshots :: Desktop -> M.Map FilePath Snapshot
sourceSnapshots d = M.fromList [(filePath file,Snapshot file (Just (bid,revision b)) (contents b)) |
  (bid,file,b)<-publicSources d]

-- Ascending IDs plus Map.fromList make the last eligible duplicate path win;
-- sourceIdentity uses the same selection without flattening any source text.
publicSources :: Desktop -> [(Int,FileState,Buffer)]
publicSources d = [(bid,file,b) | (bid,doc)<-M.toAscList (buffers d),not (protectedBuffer d bid),
  documentLabel doc==Nothing,textBuffer (documentBuffer doc),Just file<-[documentFile doc],let b=documentBuffer doc]

-- | Worker-resolved project path. The original spelling is retained only to
-- detect a symlink changing while a read is in flight. No editor state is held.
data ResolvedFile = ResolvedFile !FilePath !FilePath

-- | /O(1)/. Canonical path used for the owner capture and later admission.
resolvedFilePath :: ResolvedFile -> FilePath
resolvedFilePath (ResolvedFile _ path)=path

-- | Resolve an absolute path and confine it to the canonical project root.
-- Filesystem work belongs on a worker. Privacy is checked against the current
-- editor by 'captureFileInput', after this result returns to the owner.
resolveFile :: FilePath -> FilePath -> IO (Either Text ResolvedFile)
resolveFile root path=ioResult $ do
  unless (isAbsolute path && '\0' `notElem` path) (ioError (userError "Expected an absolute file path."))
  resolved<-canonicalizePath path
  base<-canonicalizePath root
  let relative=makeRelative base resolved
  when (isAbsolute relative || ".." `elem` splitDirectories relative)
    (ioError (userError "File is outside this session's project."))
  (original,canonical)<-evaluate (force (path,resolved))
  pure (ResolvedFile original canonical)

-- | One shallow immutable read input. An open source retains live measured
-- content and the required disk baseline, never the editable Buffer, Undo or
-- the surrounding Desktop. A disk-only input retains just its resolved path.
data FileInput = FileInput !ResolvedFile !(Maybe OpenSource)
data OpenSource = OpenSource !FileState !Int !Int !BufferContent !SourceIdentity

-- | Capture the matching source and privacy facts on the editor owner. This
-- performs no filesystem IO and does not flatten text or compare baselines.
-- The last eligible duplicate path wins, as with 'sourceSnapshots'. Any matching
-- private/hex source refuses the request rather than falling back to disk.
captureFileInput :: ResolvedFile -> Desktop -> IO (Either Text FileInput)
captureFileInput resolved d
  | protectedPath d path || any (protectedBuffer d . first) matching =
      pure (Left "This file is private; use agent_settings for public context.")
  | any (not . textBuffer . documentBuffer . second) matching =
      pure (Left "Hex buffers are not available through ACP text file APIs.")
  | otherwise=do
      opened<-case find (\(_,doc,_)->documentLabel doc==Nothing) (reverse matching) of
        Nothing->pure Nothing
        Just (bid,doc,file)->do
          let buffer=documentBuffer doc
          version<-captureVersion buffer
          baseline<-evaluate file >>= makeStableName
          image<-evaluate (bufferContent buffer)
          Just <$> evaluate (OpenSource file bid (revision buffer) image (SourceIdentity bid version baseline))
      Right <$> evaluate (FileInput resolved opened)
  where
    path=resolvedFilePath resolved
    matching=matchingSources path d
    first (bid,_,_)=bid
    second (_,doc,_)=doc

-- | /O(1)/. Exact source receipt for final owner admission. A new open source
-- invalidates a disk-only input; replacement invalidates an open one even if
-- its numeric edit revision is unchanged.
fileInputIdentity :: FileInput -> Maybe SourceIdentity
fileInputIdentity (FileInput _ opened)=case opened of
  Nothing->Nothing
  Just (OpenSource _ _ _ _ identity)->Just identity

-- | Read only this captured input on a worker, enforcing the same 16 MiB UTF-8
-- and NUL limits for live and disk sources. Later edits cannot change the text
-- read from an open input. The caller must still recheck privacy and
-- 'fileInputIdentity' before publishing a result or admitting a write.
readFileInput :: FileInput -> IO (Either Text Snapshot)
readFileInput (FileInput (ResolvedFile original resolved) opened)=ioResult $ do
  captured<-case opened of
    Just (OpenSource file bid version image _)->do
      when (contentLength image>fileLimit) (ioError (userError "ACP text files are limited to 16 MiB."))
      pure (Snapshot file (Just (bid,version)) (contentSlice image 0 (contentLength image)))
    Nothing->do
      -- Bound the read itself; a preceding stat cannot bound a growing file.
      bytes<-catchIOError (Just <$> withFileRead resolved (\handle->BS.hGet handle (fileLimit+1)))
        (\err->if isDoesNotExistError err then pure Nothing else ioError err)
      let raw=maybe BS.empty id bytes
      when (BS.length raw>fileLimit) (ioError (userError "ACP text files are limited to 16 MiB."))
      text<-either (const (ioError (userError "The file is not valid UTF-8."))) pure (TE.decodeUtf8' raw)
      pure (Snapshot (FileState resolved bytes) Nothing text)
  let text=snapshotText captured
  when (textTooLarge text) (ioError (userError "ACP text files are limited to 16 MiB."))
  when (T.any (=='\0') text) (ioError (userError "Binary text contains NUL bytes."))
  current<-canonicalizePath original
  unless (current==resolved) (ioError (userError "File path changed while reading; request a fresh read."))
  pure captured

ioResult :: IO a -> IO (Either Text a)
ioResult action=do
  result<-try action
  pure $ case result of
    Left err->Left (T.pack (show (err :: IOException)))
    Right value->Right value

-- | Validate an approved write against captured editor and disk state, then save it.
-- This explicit write boundary can compare full contents; it is not a redraw check.
acceptWrite :: Snapshot -> Text -> Desktop -> IO (Either Text Desktop)
acceptWrite (Snapshot file expected oldText) text d
  | protectedPath d (filePath file) = pure (Left "Private files require human input.")
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
        | bid==now,not (protectedBuffer d bid),documentLabel doc==Nothing,textBuffer (documentBuffer doc),revision (documentBuffer doc)==version,
          contents (documentBuffer doc)==oldText,documentFile doc==Just file -> Right (bid,documentBuffer doc,d)
      (Nothing,Nothing) -> let opened=addDocument (Just file) (newBuffer oldText) d
        in Right (nextId d,newBuffer oldText,opened)
      _ -> Left "File changed in the editor after the agent read it; request a fresh read."
    clamp bid size w | bufferId w==Just (bid) = w {selection=Selection (max 0 (min size (anchor (selection w)))) (max 0 (min size (caret (selection w)))),scrollRow=0,scrollColumn=0}
                     | otherwise = w

contextText :: Bool -> Bool -> Bool -> Desktop -> Text
contextText selectionOnly wholeFile includeDiagnostics d = T.intercalate "\n\n" (fileContext++problemContext)
  where
    fileContext=case (activeWindow d,activeDocument d) of
      (Just w,Just doc) | not (protectedWindow d w), documentLabel doc==Nothing, textBuffer (documentBuffer doc) ->
        let b=documentBuffer doc; Selection a z=selection w
            path=maybe "Unsaved buffer" (T.pack . filePath) (documentFile doc)
            selected=T.take (abs (z-a)) (T.drop (min a z) (contents b))
        in ["File: "<>path<>"\n"<>contents b | wholeFile] ++
           ["Selection in "<>path<>"\n"<>selected | selectionOnly && not wholeFile && a/=z]
      _ -> []
    problemContext=["Diagnostics:\n"<>T.unlines [T.pack (diagnosticPath p)<>":"<>T.pack (show (diagnosticRow p+1))<>": "<>diagnosticMessage p | p<-diagnostics d] | includeDiagnostics]
