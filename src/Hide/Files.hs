-- | Byte-preserving file loading and baseline-checked replacement saves.
--
-- Loaded paths are canonicalized. Invalid UTF-8 or NUL-containing input selects
-- byte mode. Saving writes a sibling temporary file, preserves existing permissions
-- and rechecks path/symlink/baseline before rename. The final comparison/rename
-- still has a concurrent-writer race; atomic replacement is not power-loss durability.
module Hide.Files (FileState(..), loadFile, saveFile) where

import Control.Exception (bracket, evaluate, mask)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, copyPermissions, pathIsSymbolicLink, removeFile, renameFile)
import System.FilePath (takeDirectory)
import System.IO (hClose, hFlush, openBinaryTempFile)
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
  let bytes = maybe BS.empty id baseline
  let buffer = case TE.decodeUtf8' bytes of
        Right text | not (BS.elem 0 bytes) -> newBuffer text
        _ -> newByteBuffer bytes
  pure (FileState resolved baseline, buffer)

-- | Save only if the expected path/baseline still matches; adopt the returned
-- FileState and mark the buffer saved separately on success. Text-mode NUL data
-- is rejected. Raw pieces are batched into byte chunks and streamed without a
-- whole-text projection. The strict returned byte baseline is prepared before
-- the final disk check and replacement.
saveFile :: FileState -> Buffer -> IO (Either String FileState)
saveFile state buffer = fileResult path $ mask $ \restore -> do
  checkDisk
  bracket (openBinaryTempFile (takeDirectory path) ".hide-")
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
      renameFile temporary path
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
readDisk path = catchIOError (Just <$> BS.readFile path)
  (\err -> if isDoesNotExistError err then pure Nothing else ioError err)

fileResult :: FilePath -> IO a -> IO (Either String a)
fileResult path action = either (Left . ((path ++ ": ") ++) . show) Right <$> tryIOError action

ignoreIO :: IO () -> IO ()
ignoreIO action = catchIOError action (const (pure ()))
