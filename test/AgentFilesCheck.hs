{-# LANGUAGE OverloadedStrings #-}
module AgentFilesCheck (checks) where

import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)
import Hide.AgentFiles
import Hide.Buffer
import Hide.Files
import Hide.Model

checks :: IO ()
checks = bracket temporary removePathForcibly $ \base -> do
  let root=base </> "project"
      path=root </> "source.hs"
      outside=base </> "outside.hs"
      empty=initialDesktop (100,30)
      capture file desktop=captureFile root file desktop >>= right "capture file"
      document desktop=maybe (error "missing document") id (activeDocument desktop)
      bufferOf=documentBuffer . document
      open desktop file=loadFile file >>= right "load" >>= \(state,buffer) -> pure (addDocument (Just state) buffer desktop)
      assertRejected label snapshot text desktop=do
        result<-acceptWrite snapshot text desktop
        check label (isLeft result)
  createDirectory root
  BS.writeFile path (TE.encodeUtf8 "λ = 1\r\n")
  BS.writeFile outside "outside"
  original<-open empty path
  let dirtyDesktop=insertText "local " original
      oldText=contents (bufferOf dirtyDesktop)
      originalFile=maybe (error "missing FileState") id (documentFile (document dirtyDesktop))
  let protected=dirtyDesktop {guestPrivatePaths=[path]}
  hidden<-captureFile root path protected
  check "ACP cannot read authority files through disk or a live buffer" (isLeft hidden && M.null (sourceSnapshots protected) && T.null (contextText True True False protected))
  prior<-capture path dirtyDesktop
  assertRejected "ACP cannot write a snapshot that became protected" prior "changed authority" protected
  let privateInput=original {buffers=M.map (\doc->doc {documentPrivate=True,documentBuffer=error "ACP forced private input"}) (buffers original)}
  hiddenInput<-captureFile root path privateInput
  check "ACP cannot fall back to the disk copy of private input" (isLeft hiddenInput)
  let privateSaved=original {buffers=M.map (\doc->doc {documentPrivate=True}) (buffers original)}
  savedSnapshot<-capture path original
  assertRejected "ACP rechecks explicit document privacy after capture" savedSnapshot "changed input" privateSaved
  let duplicatePrivate=addDocument (Just originalFile) (newBuffer "duplicate") privateSaved
  check "new views of a saved private path retain privacy" (documentPrivate (document duplicatePrivate) && M.null (sourceSnapshots duplicatePrivate))
  let projectConfig=root </> "thc.toml"
  BS.writeFile projectConfig "[editor.agent]\ncontext = 'user guidance'\n"
  hiddenProject<-captureFile root projectConfig dirtyDesktop
  check "project authority config is protected before registration" (isLeft hiddenProject)
  local<-capture path dirtyDesktop
  check "agent reads current unsaved text" (snapshotText local==oldText)
  before<-BS.readFile path
  check "capturing unsaved text does not save it" (before==TE.encodeUtf8 "λ = 1\r\n")
  let selected=fst (runCommand SplitVertical dirtyDesktop)
      split=selected {windows=map (\w -> w {selection=Selection (-2) 100}) (windows selected)}
  written<-acceptWrite local "new\n" split >>= right "write"
  bytes<-BS.readFile path
  check "write saves and preserves prior unsaved text in undo" (bytes=="new\n" && contents (bufferOf written)=="new\n" && contents (undo (bufferOf written))==oldText && not (dirty (bufferOf written)))
  check "write updates revision and disk baseline" (revision (bufferOf written)>revision (bufferOf split) && (documentFile (document written) >>= diskBytes)==Just bytes)
  check "write clamps every split view" (all (\w -> caret (selection w)<=4 && anchor (selection w)>=0 && anchor (selection w)<=4) (windows written))
  check "write invalidates source styling for the background worker" (null (documentHighlight (document written)) && documentSourceRows (document written)==Nothing)
  fresh<-capture path written
  assertRejected "edited buffer rejects stale snapshot" fresh "overwritten" (insertText "later " written)
  let changedBaseline=written {buffers=M.adjust (\doc -> doc {documentFile=Just originalFile}) 1 (buffers written)}
  assertRejected "changed baseline rejects snapshot even with same revision" fresh "overwritten" changedBaseline
  identity<-sourceIdentity path written
  check "freshness identity survives window splits" . (==identity) =<< sourceIdentity path (fst (runCommand SplitVertical written))
  check "freshness identity catches changed baseline with equal revision" . (/=identity) =<< sourceIdentity path changedBaseline
  let replacement=written {buffers=M.adjust (\doc->doc {documentBuffer=(newBuffer "reloaded") {revision=revision (documentBuffer doc)}}) 1 (buffers written)}
  check "freshness identity catches replacement buffer with equal revision" . (/=identity) =<< sourceIdentity path replacement
  let duplicate=addDocument (Just originalFile) (newBuffer "last duplicate") written
      changedEarlier=duplicate {buffers=M.adjust (\doc->doc {documentBuffer=newBuffer "earlier ignored"}) 1 (buffers duplicate)}
  duplicateIdentity<-sourceIdentity path duplicate
  check "freshness identity uses same last duplicate as source snapshots" (fmap snapshotText (M.lookup path (sourceSnapshots duplicate))==Just "last duplicate")
  check "earlier duplicate does not replace the winning source identity" . (==duplicateIdentity) =<< sourceIdentity path changedEarlier
  check "editing the last duplicate invalidates its source identity" . (/=duplicateIdentity) =<< sourceIdentity path (insertText "changed " duplicate)
  let opaque=written {buffers=M.adjust (\doc->doc {documentFile=Just originalFile {diskBytes=error "freshness forced disk bytes"}}) 1 (buffers written)}
  opaqueIdentity<-sourceIdentity path opaque
  check "freshness identity does not compare disk bytes" (opaqueIdentity/=Nothing)
  check "protected sources have no public freshness identity" . (==Nothing) =<< sourceIdentity path written {guestPrivatePaths=[path]}
  BS.writeFile path "external change"
  assertRejected "unchanged buffer with changed disk rejects write" fresh "overwritten" written
  afterRejected<-BS.readFile path
  check "rejected write leaves external bytes intact" (afterRejected=="external change")
  BS.writeFile path "new\n"
  assertRejected "NUL write is rejected" fresh "bad\NULtext" written
  let closed=foldr (const closeActive) written (windows written)
  reopened<-open closed path
  assertRejected "closed and reopened buffer invalidates its snapshot" fresh "overwritten" reopened
  closedSnapshot<-capture path empty
  assertRejected "opening a previously closed file invalidates snapshot" closedSnapshot "overwritten" original
  closedWrite<-acceptWrite closedSnapshot "from closed file\n" empty >>= right "write closed file"
  check "writing closed file opens undoable saved document" (contents (undo (bufferOf closedWrite))=="new\n" && not (dirty (bufferOf closedWrite)))
  let newPath=root </> "new.hs"
  missing<-capture newPath empty
  check "new file snapshot is empty" (snapshotText missing=="")
  BS.writeFile newPath "appeared"
  assertRejected "new file appearing after read is not overwritten" missing "new" empty
  removeFile newPath
  created<-acceptWrite missing "created\n" empty >>= right "create file"
  createdBytes<-BS.readFile newPath
  check "missing file can be created with undo to empty" (createdBytes=="created\n" && contents (undo (bufferOf created))=="")
  let invalid=root </> "invalid.hs"
  BS.writeFile invalid (BS.pack [255])
  invalidUTF8<-captureFile root invalid empty
  check "closed invalid UTF-8 is rejected" (isLeft invalidUTF8)
  BS.writeFile invalid "a\NULb"
  binary<-captureFile root invalid empty
  check "closed binary text is rejected" (isLeft binary)
  BS.writeFile path (BS.pack [255])
  preserved<-capture path closedWrite
  check "open buffer text remains readable when disk becomes invalid" (snapshotText preserved==contents (bufferOf closedWrite))
  assertRejected "invalid external bytes still protect the disk baseline" preserved "overwritten" closedWrite
  BS.writeFile path "from closed file\n"
  outsideOpen<-open empty outside
  escaped<-captureFile root outside outsideOpen
  check "project boundary applies before open-buffer lookup" (isLeft escaped)
  traversal<-captureFile root (root </> ".." </> "outside.hs") empty
  check "parent traversal is rejected" (isLeft traversal)
  relative<-captureFile root "source.hs" empty
  check "relative ACP paths are rejected" (isLeft relative)
  let alias=root </> "alias.hs"
      escapeLink=root </> "escape.hs"
  createFileLink path alias
  aliasSnapshot<-capture alias closedWrite
  check "in-project alias has canonical cache key and reads open buffer" (snapshotPath aliasSnapshot==path && snapshotText aliasSnapshot==contents (bufferOf closedWrite))
  createFileLink outside escapeLink
  escapedLink<-captureFile root escapeLink outsideOpen
  check "outward symlink cannot reveal an open external buffer" (isLeft escapedLink)
  let directoryAlias=root </> "external-directory"
  createDirectoryLink base directoryAlias
  missingOutside<-captureFile root (directoryAlias </> "new-outside.hs") empty
  check "missing file through outward directory symlink is rejected" (isLeft missingOutside)
  captured<-capture path closedWrite
  removeFile path
  createFileLink outside path
  assertRejected "symlink replacement cannot redirect approved write" captured "overwritten" closedWrite
  outsideBytes<-BS.readFile outside
  check "symlink target remains unchanged" (outsideBytes=="outside")
  removeFile path
  BS.writeFile path "small"
  small<-open empty path
  let huge=T.replicate (16*1024*1024+1) "x"
      oversized=small {buffers=M.adjust (\doc -> doc {documentBuffer=newBuffer huge}) 1 (buffers small)}
  tooLarge<-captureFile root path oversized
  check "unsaved reads obey the same text size limit as disk reads" (isLeft tooLarge)
  smallSnapshot<-capture path small
  assertRejected "writes obey the ACP text size limit" smallSnapshot huge small
  putStrLn "agent file checks passed"
  where
    temporary=do
      base<-getTemporaryDirectory
      (path,handle)<-openBinaryTempFile base "hide-agent-files"
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path
    right label=either (error . ((label++": ")++) . show) pure
    check label condition=unless condition (error label)
    isLeft (Left _)=True
    isLeft _=False
