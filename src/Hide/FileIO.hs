{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.FileIO
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : CPP, ForeignFunctionInterface
--
-- Binary file access that permits concurrent atomic replacement.
--
-- Readers keep the opened file object when its path is replaced; new opens see
-- the replacement. Windows' POSIX I/O manager needs explicit deletion sharing,
-- and replacing an open destination needs POSIX rename semantics. GHC's native
-- Windows I/O manager already shares reads, writes and deletion.
--
-- These operations do not validate a workspace, symlink or save baseline. Those
-- checks belong to the caller. Replacement is atomic, not power-loss durability.
module Hide.FileIO (withFileRead, readFileBytes, openTemporaryFile, replaceFile) where

import qualified Data.ByteString as BS
import System.IO (Handle, IOMode(ReadMode), withBinaryFile, openBinaryTempFile)
#ifdef mingw32_HOST_OS
import Control.Exception (bracket, mask_, onException)
import Control.Monad (unless, void)
import Data.Bits ((.|.))
import Data.List (isPrefixOf)
import Data.Word (Word32)
import Foreign.C.String (CWString, withCWString, withCWStringLen, peekCWString)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek)
import GHC.IO.Handle.FD (fdToHandle')
import GHC.IO.SubSystem (IoSubSystem(IoNative), ioSubSystem)
import System.IO (hClose, IOMode(ReadWriteMode))
import qualified System.Win32.File as Windows
import qualified System.Win32.Types as Windows
import qualified System.Win32.Info as Windows
#else
import System.Directory (renameFile)
#endif

-- | Scope a binary read handle without preventing replacement of the path.
-- A replacement cannot change this handle's file identity; concurrent in-place
-- writes can still alter its bytes. The caller must not retain the handle.
withFileRead :: FilePath -> (Handle -> IO a) -> IO a
#ifdef mingw32_HOST_OS
withFileRead path action
  | ioSubSystem==IoNative = withBinaryFile path ReadMode action
  | otherwise = bracket open hClose action
  where
    open = do
      absolute <- windowsPath path
      raw <- Windows.createFile_NoRetry absolute Windows.gENERIC_READ
        (Windows.fILE_SHARE_READ .|. Windows.fILE_SHARE_WRITE .|. Windows.fILE_SHARE_DELETE)
        Nothing Windows.oPEN_EXISTING Windows.fILE_ATTRIBUTE_NORMAL Nothing
      fd <- throwErrnoIfMinus1 "Open binary read descriptor" (readDescriptor raw)
        `onException` Windows.closeHandle raw
      -- fdToHandle/hANDLEToHandle infer ReadWriteMode on Windows, taking a
      -- process-local writer lock even for this read-only OS handle.
      fdToHandle' fd Nothing False path ReadMode True
        `onException` void (closeDescriptor fd)
#else
withFileRead path = withBinaryFile path ReadMode
#endif

-- | Read strict bytes and close the handle before returning.
-- Use 'withFileRead' for bounded reads; this operation reads the whole file.
readFileBytes :: FilePath -> IO BS.ByteString
readFileBytes path = withFileRead path BS.hGetContents

-- | Create an exclusive binary temporary file in the supplied directory.
-- The caller owns the returned Handle and path, and must close and remove them.
-- Windows POSIX I/O uses an extended path and a random sibling name, avoiding
-- GHC's fixed 260-character temporary-file buffer. Native I/O retains GHC's
-- UUID-based helper and native Handle. Acquisition masks
-- asynchronous exceptions until ownership transfers; failed Handle creation
-- closes and removes only the file created by this call.
openTemporaryFile :: FilePath -> IO (FilePath, Handle)
#ifdef mingw32_HOST_OS
openTemporaryFile directory
  | ioSubSystem==IoNative = openBinaryTempFile directory ".hide-"
  | otherwise = mask_ $ do
      absolute <- windowsPath directory
      withCWStringLen absolute $ \(parent, count) -> allocaArray (count+40) $ \name ->
        alloca $ \descriptor -> do
          result <- createTemporaryFile parent (fromIntegral count) name descriptor
          unless (result==0) (Windows.failWith ("Create temporary file in "++directory) result)
          fd <- peek descriptor
          let cleanup = void (closeDescriptor fd) >> void (deleteTemporaryFile name)
          flip onException cleanup $ do
            path <- peekCWString name
            handle <- fdToHandle' fd Nothing False path ReadWriteMode True
            pure (path, handle)
#else
openTemporaryFile directory = openBinaryTempFile directory ".hide-"
#endif

-- | Atomically move a file over a destination on the same filesystem.
-- Existing read handles retain the old file; the source path disappears on
-- success. Unsupported filesystem semantics fail without a copy/delete fallback.
replaceFile :: FilePath -> FilePath -> IO ()
#ifdef mingw32_HOST_OS
replaceFile source destination = do
  old <- windowsPath source
  next <- windowsPath destination
  result <- withCWString old $ \oldPath -> withCWString next $ \nextPath ->
    replaceFileWindows oldPath nextPath
  unless (result==0) (Windows.failWith ("Replace "++destination) result)

-- GetFullPathName resolves relative components before the extended prefix,
-- which disables Win32 path normalization. Keep UNC and existing extended paths.
windowsPath :: FilePath -> IO FilePath
windowsPath path = do
  absolute <- Windows.getFullPathName path
  pure $ if "\\\\?\\" `isPrefixOf` absolute then absolute
    else if "\\\\" `isPrefixOf` absolute then "\\\\?\\UNC\\"++drop 2 absolute
    else "\\\\?\\"++absolute

-- Successful conversion transfers HANDLE ownership to the CRT descriptor;
-- failed Handle construction closes that descriptor, never the raw handle twice.
foreign import ccall unsafe "hide_read_descriptor"
  readDescriptor :: Ptr () -> IO CInt
foreign import ccall safe "_close"
  closeDescriptor :: CInt -> IO CInt

-- The output name has room for parent plus slash, .hide-, 32 hex digits and NUL.
foreign import ccall safe "hide_create_temporary_file"
  createTemporaryFile :: CWString -> CSize -> CWString -> Ptr CInt -> IO Word32
foreign import ccall safe "DeleteFileW"
  deleteTemporaryFile :: CWString -> IO CInt

foreign import ccall safe "hide_replace_file"
  replaceFileWindows :: CWString -> CWString -> IO Word32
#else
replaceFile = renameFile
#endif
