{-# LANGUAGE OverloadedStrings #-}
module FilesCheck (checks) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless)
import Data.List (sort)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)
import THC.Edit.Buffer
import THC.Edit.Files

check :: String -> Bool -> IO ()
check name ok = unless ok (error name)

right :: String -> Either String a -> IO a
right name = either (error . ((name ++ ": ") ++)) pure

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

checks :: IO ()
checks = bracket makeDirectory removePathForcibly $ \dir -> do
  let path = dir </> "source.hs"
      original = "x = \206\187\r\nlast" :: BS.ByteString
  BS.writeFile path original
  permissions <- getPermissions path
  setPermissions path (permissions { executable = True })
  (state, buffer) <- loadFile path >>= right "load UTF-8"
  check "load preserves CRLF and absent final newline" (contents buffer == "x = λ\r\nlast")
  check "load keeps byte baseline" (diskBytes state == Just original)
  savedState <- saveFile state buffer >>= right "roundtrip save"
  bytes <- BS.readFile path
  check "save preserves exact bytes" (bytes == original && diskBytes savedState == Just original)
  savedPermissions <- getPermissions path
  check "save preserves executable permission" (executable savedPermissions)
  BS.writeFile path "changed outside editor"
  conflict <- saveFile savedState (newBuffer "overwrite")
  bytesAfterConflict <- BS.readFile path
  check "conflicting save keeps external text" (isLeft conflict && bytesAfterConflict == "changed outside editor")
  entries <- listDirectory dir
  check "conflict leaves no temporary sibling" (entries == ["source.hs"])

  let missing = dir </> "new.hs"
  (newState, emptyBuffer) <- loadFile missing >>= right "missing file load"
  check "missing file starts empty" (contents emptyBuffer == T.empty && diskBytes newState == Nothing)
  created <- saveFile newState (newBuffer "new\n") >>= right "create missing file"
  createdBytes <- BS.readFile missing
  check "new file save returns baseline" (createdBytes == "new\n" && diskBytes created == Just "new\n")
  staleMissing <- saveFile newState (newBuffer "overwrite")
  check "new file baseline refuses an existing file" (isLeft staleMissing)

  let link = dir </> "link.hs"
  createFileLink path link
  (linkedState, _) <- loadFile link >>= right "symlink load"
  _ <- saveFile linkedState (newBuffer "link target\n") >>= right "symlink save"
  remainsLink <- pathIsSymbolicLink link
  targetBytes <- BS.readFile path
  check "saving symlink preserves link and writes target" (remainsLink && targetBytes == "link target\n")

  let invalid = dir </> "invalid.hs"
  BS.writeFile invalid (BS.pack [0xff])
  badUTF8 <- loadFile invalid
  check "invalid UTF-8 is rejected" (isLeft badUTF8)
  BS.writeFile invalid (BS.pack [97,0,98])
  binary <- loadFile invalid
  check "NUL input is rejected" (isLeft binary)
  folder <- loadFile dir
  check "directory read is rejected" (isLeft folder)
  invalidPermissions <- getPermissions invalid
  bracket (setPermissions invalid (invalidPermissions { readable = False }))
          (const (setPermissions invalid invalidPermissions)) $ \_ -> do
    unreadable <- loadFile invalid
    check "unreadable input is rejected" (isLeft unreadable)

  (failureState, _) <- loadFile path >>= right "load before failure"
  before <- BS.readFile path
  beforeEntries <- sort <$> listDirectory dir
  dirPermissions <- getPermissions dir
  bracket (setPermissions dir (dirPermissions { writable = False }))
          (const (setPermissions dir dirPermissions)) $ \_ -> do
    failure <- saveFile failureState (newBuffer "should fail")
    check "unwritable directory save fails" (isLeft failure)
  after <- BS.readFile path
  afterEntries <- sort <$> listDirectory dir
  check "failed save keeps original bytes and cleans temporary files"
    (before == after && beforeEntries == afterEntries)
  binarySave <- saveFile failureState (newBuffer "a\NULb")
  binarySaveBytes <- BS.readFile path
  binarySaveEntries <- sort <$> listDirectory dir
  check "binary save fails without changing the original or leaving a temporary file"
    (isLeft binarySave && binarySaveBytes == before && binarySaveEntries == beforeEntries)
  -- The strict encoder fails after the sibling is opened, exercising bracket cleanup.
  encodingFailure <- try (saveFile failureState (newBuffer (error "encoding interrupted")))
    :: IO (Either SomeException (Either String FileState))
  finalBytes <- BS.readFile path
  finalEntries <- sort <$> listDirectory dir
  check "exception after opening temporary file cleans it and keeps original"
    (isLeft encodingFailure && finalBytes == before && finalEntries == beforeEntries)
  putStrLn "file checks passed"
  where
    makeDirectory = do
      base <- getTemporaryDirectory
      (path, handle) <- openBinaryTempFile base "thc-edit-files-check"
      hClose handle
      removeFile path
      createDirectory path
      pure path
