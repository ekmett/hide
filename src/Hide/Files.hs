-- | Byte-preserving file loading and baseline-checked replacement saves.
--
-- Loaded paths are canonicalized. Invalid UTF-8 or NUL-containing input selects
-- byte mode. Saving writes a sibling temporary file, preserves existing permissions
-- and rechecks path/symlink/baseline before rename. The final comparison/rename
-- still has a concurrent-writer race; atomic replacement is not power-loss durability.
module Hide.Files (FileState(..), FileRepresentation(..), loadFile, loadFileForDisplay, fileBuffer, saveFile) where

import Hide.FileIO (readFileBytes, withFileRead, openTemporaryFile, replaceFile)

import Control.Exception (bracket, evaluate, mask)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, copyPermissions, pathIsSymbolicLink, removeFile)
import System.FilePath (takeDirectory)
import System.IO (hClose, hFlush, hSeek, SeekMode(AbsoluteSeek))
import Hide.Plugin.Canvas (isImageContent)
import System.IO.Error (catchIOError, isDoesNotExistError, tryIOError)
import Hide.Buffer (Buffer, newBuffer, newByteBuffer, bufferByteStream, byteMode, textBuffer)

-- | Canonical path and last observed disk bytes; Nothing denotes a missing file.
data FileState = FileState { filePath :: FilePath, diskBytes :: Maybe ByteString }
  deriving (Eq, Show)

-- | Load a canonical file or a clean new buffer for a missing path.
-- Undecodable or NUL-containing data stays lossless in byte mode.
loadFile :: FilePath -> IO (Either String (FileState, Buffer))
loadFile path = fileResult path $ do
  resolved <- canonicalizePath path
  baseline <- readDisk resolved
  pure (FileState resolved baseline, fileBuffer (maybe BS.empty id baseline))

-- | File presentation chosen from bytes. Images carry a bounded encoded source;
-- decoding and window admission remain with the existing presentation owner.
data FileRepresentation = BufferedFile !FileState !Buffer | ImageFile !FilePath !ByteString

-- | Worker-only ordinary file opening. Recognized images are read with a 16 MiB
-- ceiling; all other contents retain 'loadFile' byte/text and missing-file rules.
-- Compiler and buffer services continue to use 'loadFile' directly.
loadFileForDisplay :: FilePath -> IO (Either String FileRepresentation)
loadFileForDisplay path=fileResult path $ do
  resolved<-canonicalizePath path
  catchIOError (withFileRead resolved $ \handle->do
    prefix<-BS.hGet handle 8
    hSeek handle AbsoluteSeek 0
    if isImageContent prefix then do
      bytes<-BS.hGet handle (16777216+1)
      if BS.length bytes>16777216 then ioError (userError "Image exceeds the 16 MiB file limit.")
        else pure (ImageFile resolved bytes)
    else do
      bytes<-BS.hGetContents handle
      pure (BufferedFile (FileState resolved (Just bytes)) (fileBuffer bytes)))
    (\err->if isDoesNotExistError err then pure (BufferedFile (FileState resolved Nothing) (fileBuffer BS.empty)) else ioError err)

-- | Lossless buffer representation for file and uploaded contents. Invalid
-- UTF-8 and NUL-containing input select byte mode without changing the payload.
fileBuffer :: ByteString -> Buffer
fileBuffer bytes=case TE.decodeUtf8' bytes of
  Right text | not (BS.elem 0 bytes)->newBuffer text
  _->newByteBuffer bytes

-- | Save only if the expected path/baseline still matches; adopt the returned
-- FileState and mark the buffer saved separately on success. Text-mode NUL data
-- is rejected. Raw pieces are batched into byte chunks and streamed without a
-- whole-text projection. The strict returned byte baseline is prepared before
-- the final disk check and replacement.
saveFile :: FileState -> Buffer -> IO (Either String FileState)
saveFile state buffer = fileResult path $ mask $ \restore -> do
  checkDisk
  bracket (openTemporaryFile (takeDirectory path))
          (\(temporary, handle) -> ignoreIO (hClose handle) >> ignoreIO (removeFile temporary)) $
    \(temporary, handle) -> do
      bytes <- restore $ do
        unless (byteMode buffer || textBuffer buffer)
          (ioError (userError "Text contains NUL bytes; switch to hex mode before saving."))
        BL.hPut handle stream
        hFlush handle
        hClose handle
        case diskBytes state of
          Nothing -> pure ()
          Just _ -> copyPermissions path temporary
        -- Complete encoding/allocation before replacing the destination.
        evaluate (BL.toStrict stream)
      -- ponytail: check immediately before rename; platform locking is needed for simultaneous writers.
      checkDisk
      replaceFile temporary path
      pure state { diskBytes = Just bytes }
  where
    path = filePath state
    stream = bufferByteStream buffer
    checkDisk = do
      resolved <- canonicalizePath path
      symlink <- catchIOError (pathIsSymbolicLink path)
        (\err -> if isDoesNotExistError err then pure False else ioError err)
      current <- readDisk path
      unless (resolved == path && not symlink && current == diskBytes state)
        (ioError (userError "File changed on disk; reopen it before saving."))

readDisk :: FilePath -> IO (Maybe ByteString)
readDisk path = catchIOError (Just <$> readFileBytes path)
  (\err -> if isDoesNotExistError err then pure Nothing else ioError err)

fileResult :: FilePath -> IO a -> IO (Either String a)
fileResult path action = either (Left . ((path ++ ": ") ++) . show) Right <$> tryIOError action

ignoreIO :: IO () -> IO ()
ignoreIO action = catchIOError action (const (pure ()))
