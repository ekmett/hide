module THC.Edit.Files (FileState(..), loadFile, saveFile) where

import Control.Exception (bracket, mask)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, copyPermissions, pathIsSymbolicLink, removeFile, renameFile)
import System.FilePath (takeDirectory)
import System.IO (hClose, hFlush, openBinaryTempFile)
import System.IO.Error (catchIOError, isDoesNotExistError, tryIOError)
import THC.Edit.Buffer (Buffer, contents, newBuffer)

data FileState = FileState { filePath :: FilePath, diskBytes :: Maybe ByteString }
  deriving (Eq, Show)

loadFile :: FilePath -> IO (Either String (FileState, Buffer))
loadFile path = fileResult path $ do
  resolved <- canonicalizePath path
  baseline <- readDisk resolved
  let bytes = maybe BS.empty id baseline
  rejectBinary bytes
  text <- either (const (ioError (userError "Unsupported encoding; convert the file to UTF-8 before opening."))) pure
          (TE.decodeUtf8' bytes)
  pure (FileState resolved baseline, newBuffer text)

saveFile :: FileState -> Buffer -> IO (Either String FileState)
saveFile state buffer = fileResult path $ mask $ \restore -> do
  checkDisk
  bracket (openBinaryTempFile (takeDirectory path) ".thc-edit-")
          (\(temporary, handle) -> ignoreIO (hClose handle) >> ignoreIO (removeFile temporary)) $
    \(temporary, handle) -> do
      restore $ do
        rejectBinary bytes
        BS.hPut handle bytes
        hFlush handle
        hClose handle
        case diskBytes state of
          Nothing -> pure ()
          Just _ -> copyPermissions path temporary
      -- ponytail: check immediately before rename; platform locking is needed for simultaneous writers.
      checkDisk
      renameFile temporary path
      pure state { diskBytes = Just bytes }
  where
    path = filePath state
    bytes = TE.encodeUtf8 (contents buffer)
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

rejectBinary :: ByteString -> IO ()
rejectBinary bytes = when (BS.elem 0 bytes)
  (ioError (userError "Binary text contains NUL bytes; remove them or use a binary editor."))

fileResult :: FilePath -> IO a -> IO (Either String a)
fileResult path action = either (Left . ((path ++ ": ") ++) . show) Right <$> tryIOError action

ignoreIO :: IO () -> IO ()
ignoreIO action = catchIOError action (const (pure ()))
