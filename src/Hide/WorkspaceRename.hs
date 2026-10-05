{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Checked workspace path rename shared by the human Files form and MCP owner.
-- Preparation owns canonicalization, dirty checks and immutable receipts.
-- Adoption preserves buffer/Undo identity and refuses a changed source or an
-- occupied destination. The platform rename never replaces a destination; the
-- final source metadata check still has a finite check-to-rename race.
module Hide.WorkspaceRename
  ( RenameFile, RenameSource, PreparedRename
  , captureRenameFiles, prepareRenameSource, prepareWorkspaceRename
  , prepareBasenameRename, commitWorkspaceRename, renameSourcePath
  , renamePaths, checkedPath, operationPath, within
  ) where

import Control.Exception (evaluate,mask_)
import Control.Monad (forM,forM_,unless,when)
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Foreign (Ptr,allocaArray,peekArray,withArray)
import Foreign.C (CString,CInt(..),throwErrnoIfMinus1_)
import System.Directory (canonicalizePath,doesPathExist,pathIsSymbolicLink)
import System.FilePath
import System.IO.Error (catchIOError,isDoesNotExistError,tryIOError)
import System.Mem.StableName
import System.Info (os)
import GHC.IO.Encoding (getFileSystemEncoding,utf8)
import qualified GHC.Foreign as Foreign
import Hide.Buffer (DirtySnapshot,captureDirty,snapshotDirty)
import Hide.Files (FileState(..))
import Hide.GuestAccess (protectedPathParent)
import Hide.Privacy (protectedFilePathParent)
import Hide.Sidebar (treeRoot)
import Hide.Model
import Hide.Plugin.BufferHost (ContentVersion,captureVersion,versionCurrent)

foreign import ccall safe "thc_file_stamp" c_stamp :: CString -> Ptr Word64 -> IO CInt
foreign import ccall safe "thc_rename_noreplace" c_rename :: CString -> CString -> Ptr Word64 -> IO CInt

-- No Buffer or Undo is retained; exceptional dirty comparison stays on worker.
data RenameFile = RenameFile !Int !FileState !(StableName FileState) !ContentVersion !DirtySnapshot
newtype FileStamp = FileStamp [Word64] deriving Eq
-- Original source is fixed while a form is open; text changes only destination.
data RenameSource = RenameSource !FilePath ![FilePath] !FilePath !FileStamp ![(RenameFile,FilePath)]
data PreparedRename = PreparedRename !FilePath !FilePath !BS.ByteString !BS.ByteString !FileStamp ![(RenameFile,FilePath)]

-- | Capture immutable file/dirty receipts without traversing text or history.
captureRenameFiles :: Desktop -> IO [RenameFile]
captureRenameFiles d=forM [(bid,doc,file) | (bid,doc)<-M.toList (buffers d),Just file<-[documentFile doc]] $ \(bid,doc,file)->do
  _<-evaluate (filePath file)
  identity<-makeStableName $! file
  version<-captureVersion (documentBuffer doc)
  dirtyImage<-evaluate (captureDirty (documentBuffer doc))
  pure (RenameFile bid file identity version dirtyImage)

nativePath :: FilePath -> IO BS.ByteString
nativePath path=do
  encoding<-if os=="mingw32" then pure utf8 else getFileSystemEncoding
  Foreign.withCStringLen encoding path BS.packCStringLen

fileStamp :: FilePath -> IO FileStamp
fileStamp path=do
  encoded<-nativePath path
  BS.useAsCString encoded $ \raw->allocaArray 8 $ \out->do
    throwErrnoIfMinus1_ "Inspect rename source" (c_stamp raw out)
    FileStamp <$> peekArray 8 out

-- | Strict containment after resolving existing ancestors, excluding metadata.
checkedPath :: FilePath -> FilePath -> IO FilePath
checkedPath root raw=do
  when (null raw || length raw>32768 || '\0' `elem` raw || ".." `elem` splitDirectories raw) (ioError (userError "Invalid workspace path"))
  let joined=if isAbsolute raw then raw else root </> raw
  resolved<-canonicalizePath joined
  unless (within root resolved && resolved/=root && not (metadata raw) && not (metadata (makeRelative root resolved)))
    (ioError (userError "Path must stay inside the workspace and outside repository metadata"))
  pure resolved
  where metadata=any ((`elem` [".git",".hg",".svn"]) . T.toCaseFold . T.pack) . splitDirectories

-- | Lexical containment of already canonical paths.
within :: FilePath -> FilePath -> Bool
within root path=let relative=makeRelative root path in not (isAbsolute relative) && ".." `notElem` splitDirectories relative

-- | Never follow a symlink endpoint, including dangling destinations.
operationPath :: FilePath -> FilePath -> IO FilePath
operationPath root raw=do
  path<-checkedPath root raw
  symlink<-catchIOError (pathIsSymbolicLink (if isAbsolute raw then raw else root </> raw))
    (\err->if isDoesNotExistError err then pure False else ioError err)
  when symlink (ioError (userError "File operations do not follow symlink endpoints"))
  pure path

-- | Prepare the original receipt on worker. Human forms require a regular file;
-- the existing MCP directory rename retains its supported path kind.
prepareRenameSource :: Bool -> FilePath -> [FilePath] -> FilePath -> [RenameFile] -> IO RenameSource
prepareRenameSource regular rawRoot private raw files=do
  root<-canonicalizePath rawRoot
  path<-operationPath root raw
  rejectPrivate private path
  stamp@(FileStamp fields)<-fileStamp path
  when (regular && fields!!2/=1) (ioError (userError "Rename requires a saved regular file"))
  canonical<-forM files $ \file@(RenameFile _ state _ _ _)->(file,) <$> canonicalizePath (filePath state)
  let affected=[(file,current) | (file,current)<-canonical,within path current]
  forM_ affected $ \(RenameFile _ state _ _ image,_)->do
    when (snapshotDirty image) (ioError (userError "Save or close dirty open buffers before changing their paths"))
    unless (not regular || isJust (diskBytes state)) (ioError (userError "Rename requires a saved file"))
  after<-fileStamp path
  unless (after==stamp) (ioError (userError "Rename source changed while preparing"))
  _<-evaluate (length root+length path+sum [length current | (_,current)<-canonical])
  pure (RenameSource root private path stamp canonical)

-- | Exact original path, suitable for basename presentation and scoped refresh.
renameSourcePath :: RenameSource -> FilePath
renameSourcePath (RenameSource _ _ path _ _)=path

-- | Prepare a full target for the existing workspace_files route.
prepareWorkspaceRename :: RenameSource -> FilePath -> IO PreparedRename
prepareWorkspaceRename (RenameSource root private source stamp files) raw=do
  current<-fileStamp source
  unless (current==stamp) (ioError (userError "Rename source changed; reopen Rename"))
  destination<-operationPath root raw
  rejectPrivate private destination
  exists<-doesPathExist destination
  when (exists || within source destination) (ioError (userError "Rename destination exists or is inside the source"))
  when (any (\(_,path)->within destination path) files) (ioError (userError "Close the open destination buffer before renaming"))
  let affected=[(file,if path==source then destination else destination </> makeRelative source path) | (file,path)<-files,within source path]
  _<-evaluate (length destination+sum [length path | (_,path)<-affected])
  oldBytes<-nativePath source
  newBytes<-nativePath destination
  pure (PreparedRename source destination oldBytes newBytes stamp affected)

-- | One basename, never a path traversal or another directory.
prepareBasenameRename :: RenameSource -> Text -> IO PreparedRename
prepareBasenameRename source name=do
  unless (not (T.null name) && T.length name<=255 && name/="." && name/=".." &&
    not (T.any (\c->c<' ' || c=='\DEL' || c `elem` ['/', '\\', '\0']) name))
    (ioError (userError "Enter a single filename"))
  prepareWorkspaceRename source (takeDirectory (renameSourcePath source) </> T.unpack name)

-- | Captured old/new paths for moved-subtree invalidation and refresh of both
-- distinct parents. No directory enumeration belongs to commit.
renamePaths :: PreparedRename -> (FilePath,FilePath)
renamePaths (PreparedRename source destination _ _ _ _)=(source,destination)

rejectPrivate :: [FilePath] -> FilePath -> IO ()
rejectPrivate private path=when (protectedFilePathParent private path)
  (ioError (userError "This path contains private editor configuration or session data"))

-- | Revalidate current scalar identities before one no-replace rename. Successful
-- adoption changes only paths/style metadata, preserving buffers, Undo and IDs.
commitWorkspaceRename :: PreparedRename -> Desktop -> IO (Either Text Desktop)
commitWorkspaceRename (PreparedRename source destination oldBytes newBytes (FileStamp stamp) affected) d=do
  let ids=[bid | (RenameFile bid _ _ _ _,_)<-affected]
      currentIds=[bid | (bid,doc)<-M.toList (buffers d),Just file<-[documentFile doc],within source (filePath file)]
      destinationOpen=any (\doc->maybe False (within destination . filePath) (documentFile doc)) (M.elems (buffers d))
  versions<-forM affected $ \(RenameFile bid expected identity version _,_)->case M.lookup bid (buffers d) of
    Just doc | Just file<-documentFile doc,filePath file==filePath expected->do
      current<-makeStableName $! file
      (&& (current==identity)) <$> versionCurrent version (documentBuffer doc)
    _->pure False
  if not (and versions) || sort currentIds/=sort ids || destinationOpen then pure (Left "Open rename targets changed; reopen Rename")
  else if protectedPathParent d source || protectedPathParent d destination then pure (Left "Rename target is now protected")
  else do
    result<-tryIOError $ mask_ $ BS.useAsCString oldBytes $ \old->BS.useAsCString newBytes $ \new->withArray stamp $ \expected->do
      throwErrnoIfMinus1_ "Rename file" (c_rename old new expected)
      let replacements=M.fromList [(bid,path) | (RenameFile bid _ _ _ _,path)<-affected]
          remap path | path==source=destination
                     | within source path=destination </> makeRelative source path
                     | otherwise=path
          updated=d {buffers=M.mapWithKey (\bid doc->case M.lookup bid replacements of
            Nothing->doc
            Just path->restyle doc {documentFile=fmap (\file->file {filePath=path}) (documentFile doc)}) (buffers d),
            defaultDirectory=fmap remap (defaultDirectory d),sideTree=fmap (\tree->tree {treeRoot=remap (treeRoot tree)}) (sideTree d)}
      evaluate (foldr normalizeDocumentViews updated ids)
    pure (either (Left . T.pack . show) Right result)
