{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module FilesCheck (checks) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (foldM, unless)
import Data.List (sort)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, openBinaryTempFile)
import Hide.Buffer
import Hide.Files
import Hide.FileIO (withFileRead, replaceFile)

check :: String -> Bool -> IO ()
check name ok = unless ok (error name)

right :: String -> Either String a -> IO a
right name = either (error . ((name ++ ": ") ++) . show) pure

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

checks :: IO ()
checks = bracket makeDirectory removePathForcibly $ \dir -> do
  let path = dir </> "source.hs"
      original = "x = \206\187\r\nlast" :: BS.ByteString
  BS.writeFile path original
#ifndef mingw32_HOST_OS
  -- Windows infers executability from the filename rather than a mode bit.
  permissions <- getPermissions path
  setPermissions path (permissions { executable = True })
#endif
  (state, buffer) <- loadFile path >>= right "load UTF-8"
  check "load preserves CRLF and absent final newline" (contents buffer == "x = λ\r\nlast")
  check "load keeps byte baseline" (diskBytes state == Just original)
  savedState <- saveFile state buffer >>= right "roundtrip save"
  bytes <- BS.readFile path
  check "save preserves exact bytes" (bytes == original && diskBytes savedState == Just original)
#ifndef mingw32_HOST_OS
  savedPermissions <- getPermissions path
  check "save preserves executable permission" (executable savedPermissions)
#endif
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
  (_,badUTF8) <- loadFile invalid >>= right "load invalid UTF-8 as bytes"
  check "invalid UTF-8 opens in hex mode" (byteMode badUTF8 && bufferBytes badUTF8==BS.pack [255])
  BS.writeFile invalid (BS.pack [97,0,98])
  (_,binary) <- loadFile invalid >>= right "load NUL as bytes"
  check "NUL input opens in hex mode" (byteMode binary && bufferBytes binary==BS.pack [97,0,98])
  folder <- loadFile dir
  check "directory read is rejected" (isLeft folder)
#ifndef mingw32_HOST_OS
  -- The portable directory-read failure above remains checked on Windows.
  invalidPermissions <- getPermissions invalid
  bracket (setPermissions invalid (invalidPermissions { readable = False }))
          (const (setPermissions invalid invalidPermissions)) $ \_ -> do
    unreadable <- loadFile invalid
    check "unreadable input is rejected" (isLeft unreadable)
#endif

  (failureState, _) <- loadFile path >>= right "load before failure"
  before <- BS.readFile path
  beforeEntries <- sort <$> listDirectory dir
#ifndef mingw32_HOST_OS
  -- System.Directory cannot deny directory writes through Permissions on Windows.
  dirPermissions <- getPermissions dir
  bracket (setPermissions dir (dirPermissions { writable = False }))
          (const (setPermissions dir dirPermissions)) $ \_ -> do
    failure <- saveFile failureState (newBuffer "should fail")
    check "unwritable directory save fails" (isLeft failure)
  after <- BS.readFile path
  afterEntries <- sort <$> listDirectory dir
  check "failed save keeps original bytes and cleans temporary files"
    (before == after && beforeEntries == afterEntries)
#endif
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
  let piecesPath = dir </> "pieces.hs"
      row = T.replicate 500 "λx"
      originalText = "removed\r\n" <> row <> "\r\nlast"
  BS.writeFile piecesPath (TE.encodeUtf8 originalText)
  (piecesState, loadedPieces) <- loadFile piecesPath >>= right "load long raw row"
  loadedSaved <- saveFile piecesState loadedPieces >>= right "save loaded raw row"
  loadedBytes <- BS.readFile piecesPath
  check "loaded long row saves exact raw bytes and baseline"
    (loadedBytes == TE.encodeUtf8 originalText && diskBytes loadedSaved == Just loadedBytes)
  let withoutFirst = replaceSelection (Selection 0 9) T.empty loadedPieces
      edited = replaceSelection (Selection 400 402) "界é\r\n" withoutFirst
      expected = TE.encodeUtf8 (T.take 400 row <> "界é\r\n" <> T.drop 402 row <> "\r\nlast")
      independent = edited {saved=error "save forced saved text",undoStack=error "save forced Undo",redoStack=error "save forced Redo"}
  piecesSaved <- saveFile loadedSaved independent >>= right "save edited raw pieces"
  piecesBytes <- BS.readFile piecesPath
  check "edited spans save exact UTF-8 and CRLF, excluding deleted provenance"
    (piecesBytes == expected && diskBytes piecesSaved == Just expected)

  let switched = replaceBuffer True "\NUL\255\r\n" edited
      restored = either (error . T.unpack) id (restoreBufferStorage (fmap snd (snapshotBufferStorage switched)))
      transitions =
        [ ("Undo", undo edited, TE.encodeUtf8 (row <> "\r\nlast"))
        , ("Redo", redo (undo edited), expected)
        , ("explicit replacement", replaceBuffer False "replacement\r\nλ" edited, TE.encodeUtf8 "replacement\r\nλ")
        , ("byte mode replacement", switched, BS.pack [0,255,13,10])
        , ("mode Undo", undo switched, expected)
        , ("recovered representation", restored, BS.pack [0,255,13,10])
        ]
  _ <- foldM (\currentState (name, candidate, wanted) -> do
    result <- saveFile currentState candidate >>= right ("save " ++ name)
    actual <- BS.readFile piecesPath
    check (name ++ " saves exact bytes and strict baseline")
      (actual == wanted && diskBytes result == Just wanted)
    pure result) piecesSaved transitions

  let hexPath = dir </> "pieces.bin"
      raw = BS.pack [0,255,13,10,128,0,65]
      hex = replaceSelection (Selection 1 2) "\254\NUL" (newByteBuffer raw)
      hexExpected = BS.pack [0,254,0,13,10,128,0,65]
  (hexState, _) <- loadFile hexPath >>= right "prepare hex save"
  hexSaved <- saveFile hexState hex >>= right "save edited byte pieces"
  hexBytes <- BS.readFile hexPath
  check "hex pieces retain all bytes including NUL and invalid UTF-8"
    (hexBytes == hexExpected && diskBytes hexSaved == Just hexExpected)
  let sharedPath = dir </> "shared-λ.bin"
  BS.writeFile sharedPath "old bytes"
  (sharedState, _) <- loadFile sharedPath >>= right "load shared file"
  withFileRead sharedPath $ \reader -> do
    _ <- saveFile sharedState (newBuffer "new bytes") >>= right "save while reader is open"
    previous <- BS.hGetContents reader
    current <- BS.readFile sharedPath
    check "replacement preserves reader bytes and updates the path"
      (previous=="old bytes" && current=="new bytes")
  let longPath = foldl (</>) dir (replicate 6 ('\x1f4c1':replicate 47 'p')) </> "shared-λ.bin"
  createDirectoryIfMissing True (takeDirectory longPath)
  replaceFile sharedPath longPath
  withFileRead longPath $ \reader -> do
    (longState, _) <- loadFile longPath >>= right "load long-path file"
    entriesBefore <- sort <$> listDirectory (takeDirectory longPath)
    savedLong <- saveFile longState (newBuffer "latest bytes") >>= right "save long-path file"
    previous <- BS.hGetContents reader
    current <- BS.readFile longPath
    check "long-path save preserves overlapping readers and updates the baseline"
      (previous=="new bytes" && current=="latest bytes" && diskBytes savedLong==Just current)
    interrupted <- try (saveFile savedLong (newBuffer (error "long-path encoding interrupted")))
      :: IO (Either SomeException (Either String FileState))
    afterInterrupted <- BS.readFile longPath
    entriesAfter <- sort <$> listDirectory (takeDirectory longPath)
    check "long-path successful and interrupted saves leave no temporary sibling"
      (isLeft interrupted && afterInterrupted==current && entriesAfter==entriesBefore)
  -- A failed replacement must not unlink either side, including its source.
  let destinationDirectory = dir </> "occupied"
  createDirectory destinationDirectory
  failedReplacement <- try (replaceFile longPath destinationDirectory) :: IO (Either SomeException ())
  afterReplacement <- BS.readFile longPath
  directoryRemains <- doesDirectoryExist destinationDirectory
  check "failed replacement preserves both paths"
    (isLeft failedReplacement && afterReplacement=="latest bytes" && directoryRemains)
  putStrLn "file checks passed"
  where
    makeDirectory = do
      base <- getTemporaryDirectory
      (path, handle) <- openBinaryTempFile base "hide-files-check"
      hClose handle
      removeFile path
      createDirectory path
      pure path
