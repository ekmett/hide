{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Binary file access that permits concurrent atomic replacement.
--
-- Readers keep the opened file object when its path is replaced; new opens see
-- the replacement. Windows' POSIX I/O manager needs explicit deletion sharing,
-- and replacing an open destination needs POSIX rename semantics. GHC's native
-- Windows I/O manager already shares reads, writes and deletion.
--
-- These operations do not validate a workspace, symlink or save baseline. Those
-- checks belong to the caller. Replacement is atomic, not power-loss durability.
module Hide.FileIO (withFileRead, readFileBytes, replaceFile) where

import qualified Data.ByteString as BS
import System.IO (Handle, IOMode(ReadMode), withBinaryFile)
#ifdef mingw32_HOST_OS
import Control.Exception (bracket, onException)
import Control.Monad (unless, void)
import Data.Bits ((.|.))
import Data.List (isPrefixOf)
import Data.Word (Word32)
import Foreign.C.String (CWString, withCWString)
import Foreign.C.Types (CInt(..))
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.Ptr (Ptr)
import GHC.IO.Handle.FD (fdToHandle')
import GHC.IO.SubSystem (IoSubSystem(IoNative), ioSubSystem)
import System.IO (hClose)
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

foreign import ccall safe "hide_replace_file"
  replaceFileWindows :: CWString -> CWString -> IO Word32
#else
replaceFile = renameFile
#endif
