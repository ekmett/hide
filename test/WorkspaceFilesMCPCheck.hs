{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : WorkspaceFilesMCPCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module WorkspaceFilesMCPCheck (checks) where

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync,poll)
import System.Timeout (timeout)
import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Info (os)
import System.Process (callProcess)
import Hide.BufferDiffCommand (withBufferDiffCommands)
import TypedBufferDiffsCheck (startDiffCall)
import Hide.Buffer
import Hide.BufferView
import Hide.TextPresentation (prepareTextPresentations)
import Hide.Files (FileState(..), loadFile)
import Hide.MCPPermissions (withPermissionsAt,tickPermissions)
import Hide.Sidebar
import Hide.Model
import Hide.WorkspaceFilesMCP

checks :: IO ()
checks=withBufferDiffCommands $ \commands->do
  patchChecks
  bracket temporary removePathForcibly $ \directory -> do
    let policy=directory </> "policy.toml"
    TIO.writeFile policy "[editor.mcp.permissions]\nbuffer_apply_diff = 'enable'\n"
    withPermissionsAt policy fileTools $ \runtime -> do
      let root=directory </> "project"
          outside=directory </> "outside"
          core d _=pure (False,d)
          tool name args d=do
            (next,response)<-if name=="buffer_apply_diff" then startDiffCall commands runtime (pure (Right ())) d name (object args) else fileTool core d name (object args)
            withAsync response $ \worker->do
              let await current=do
                    completed<-poll worker
                    case completed of
                      Just (Right result)->pure (current,result)
                      Just (Left err)->error (show err)
                      Nothing->threadDelay 1000 >> tickPermissions runtime current >>= await
              timeout 5000000 (await next) >>= maybe (error "file tool timeout") pure
          success name args d=tool name args d >>= \(next,result)->either (error . T.unpack) (pure . (next,)) result
          rejected name args d=do
            (next,result)<-tool name args d
            check ("file tool rejects "<>T.unpack name) (next {status=status d}==d && case result of Left _->True; _->False)
          operation action path=["operation" .= (action::T.Text),"path" .= (path::FilePath)]
          file action path d=fst <$> success "workspace_files" (operation action path) d
          initial=(initialDesktop (80,25)) {defaultDirectory=Just root}
      createDirectory root
      createDirectory outside
      writeFile (root </> "test.cabal") "name: fixture\n"
      _<-file "mkdir" "new" initial
      _<-file "create_file" "new/empty.hs" initial
      bytes<-BS.readFile (root </> "new/empty.hs")
      check "create_file creates an empty file" (BS.null bytes)
      rejected "workspace_files" (operation "create_file" "new/empty.hs") initial
      rejected "workspace_files" (operation "delete" "new") initial
      mapM_ (\path->rejected "workspace_files" (operation "mkdir" path) initial) ["../escape",".",".git/objects",outside </> "escape"]
      when (os/="mingw32") $ do
        createDirectoryLink outside (root </> "escape")
        rejected "workspace_files" (operation "create_file" "escape/escaped.txt") initial
        createFileLink (root </> "missing") (root </> "dangling")
        rejected "workspace_files" (operation "rename" "new/empty.hs"++["to" .= ("dangling"::T.Text)]) initial
      TIO.writeFile (root </> "source.hs") "disk needle\n"
      loaded<-loadFile (root </> "source.hs") >>= either error pure
      let clean=uncurry (\state buffer->addDocument (Just state) buffer initial) loaded
          bid=maybe (error "missing buffer") sourceFixtureBuffer (activeWindow clean)
          patch::T.Text
          patch="--- a/source.hs\n+++ b/source.hs\n@@ -1 +1 @@\n-disk needle\n+live needle\n"
      (dirtyDesktop,patched)<-success "buffer_apply_diff" ["bufferId" .= bid,"revision" .= (0::Int),"diff" .= patch] clean
      check "diff edit updates live buffer revision and remains unsaved" (field "revision" patched==Just (1::Int) && activeText dirtyDesktop=="live needle\n" && maybe False (dirty.documentBuffer) (activeDocument dirtyDesktop))
      check "diff edit is undoable in one action" (activeText (fst (runCommand Undo dirtyDesktop))=="disk needle\n")
      disk<-TIO.readFile (root </> "source.hs")
      check "diff never saves the file" (disk=="disk needle\n")
      rejected "buffer_apply_diff" ["bufferId" .= bid,"revision" .= (0::Int),"diff" .= patch] dirtyDesktop
      rejected "buffer_apply_diff" ["bufferId" .= bid,"revision" .= (1::Int),"diff" .= patch] dirtyDesktop
      rejected "workspace_files" (operation "delete" "source.hs") dirtyDesktop
      rejected "workspace_files" (operation "rename" "source.hs"++["to" .= ("renamed.hs"::T.Text)]) dirtyDesktop
      (renamedDesktop,renamedResult)<-success "workspace_files" (operation "rename" "source.hs"++["to" .= ("renamed.hs"::T.Text)]) clean
      check "rename reports success and updates open buffer paths" (field "operation" renamedResult==Just ("rename"::T.Text) && fmap filePath (activeDocument renamedDesktop >>= documentFile)==Just (root </> "renamed.hs"))
      TIO.writeFile (root </> "notes.md") "# Heading\n"
      markdown<-loadFile (root </> "notes.md") >>= either error pure
      preview<-prepareTextPresentations (fst (runCommand SplitVertical (fst (runCommand (SetBufferView MarkdownView) (uncurry (\state buffer->addDocument (Just state) buffer initial) markdown)))))
      (renamedPreview,_)<-success "workspace_files" (operation "rename" "notes.md"++["to" .= ("notes.txt"::T.Text)]) preview
      check "workspace rename retires every incompatible Markdown preview"
        (all (\w->bufferView w==CurrentView && markdownInteraction w==Nothing) (windows renamedPreview) && M.null (windowPresentations renamedPreview) && activeText renamedPreview=="# Heading\n")
      -- Reload the renamed path, then prove deleting closes every clean shared view.
      renamed<-canonicalizePath (root </> "renamed.hs")
      state<-loadFile renamed >>= either error pure
      let shared=fst (runCommand SplitVertical (uncurry (\fileState buffer->addDocument (Just fileState) buffer initial) state))
      deleted<-file "delete" "renamed.hs" shared
      check "deleting a clean file removes its buffers and shared windows" (M.null (buffers deleted) && null (windows deleted))
      _<-file "delete" "new/empty.hs" initial
      _<-file "delete" "new" initial
      exists<-doesDirectoryExist (root </> "new")
      check "delete removes an empty directory" (not exists)
      _<-file "mkdir" "tree" initial
      let missingPath=root </> "tree/missing.hs"
          treeDesktop=(addDocument (Just (FileState missingPath Nothing)) (newBuffer "") initial)
            {defaultDirectory=Just (root </> "tree"),sideTree=Just (emptySidebar (root </> "tree") 24 False)}
      (movedTree,_)<-success "workspace_files" (operation "rename" "tree"++["to" .= ("moved"::T.Text)]) treeDesktop
      check "renaming a directory updates current directory and files tree root"
        (defaultDirectory movedTree==Just (root </> "moved") && fmap treeRoot (sideTree movedTree)==Just (root </> "moved") &&
          fmap filePath (activeDocument movedTree >>= documentFile)==Just (root </> "moved/missing.hs") &&
          maybe False ((==Nothing).diskBytes) (activeDocument movedTree >>= documentFile) &&
          map windowId (windows movedTree)==map windowId (windows treeDesktop))
      removedTree<-file "delete" "moved" movedTree
      check "deleting the selected subdirectory returns directory views to the project root"
        (defaultDirectory removedTree==Just root && fmap treeRoot (sideTree removedTree)==Just root)
      let binary=addDocument Nothing (newByteBuffer (BS.pack [0,255])) initial
          binaryId=maybe (error "missing binary buffer") sourceFixtureBuffer (activeWindow binary)
      rejected "buffer_apply_diff" ["bufferId" .= binaryId,"revision" .= (0::Int),"diff" .= patch] binary
      let multiBase=fst (runCommand SplitVertical (addDocument Nothing (newBuffer "a\nb\ncc\nd\n") initial))
          multi=multiBase {windows=map (\w->w {selection=Selection 5 5}) (windows multiBase)}
          multiId=maybe (error "missing split buffer") sourceFixtureBuffer (activeWindow multi)
          multiPatch::T.Text
          multiPatch="@@ -1 +1 @@\n-a\n+AAAAA\n@@ -3 +3 @@\n-cc\n+Z\n"
      (mapped,_)<-success "buffer_apply_diff" ["bufferId" .= multiId,"revision" .= (0::Int),"diff" .= multiPatch] multi
      check "multi-hunk diff rebases every shared selection through preceding insertions"
        (all ((==Selection 10 10).selection) (windows mapped) && map windowId (windows mapped)==map windowId (windows multi))
      rejected "workspace_search" ["query" .= ("needle"::T.Text),"unexpected" .= True] initial
      rejected "workspace_search" ["query" .= ("needle"::T.Text),"limit" .= (1001::Int)] initial
      callProcess "git" ["-C",root,"init","--quiet"]
      TIO.writeFile (root </> "tracked.hs") "disk needle\n"
      TIO.writeFile (root </> "untracked.hs") "untracked needle\n"
      TIO.writeFile (root </> ".gitignore") "ignored.hs\n"
      TIO.writeFile (root </> "ignored.hs") "ignored needle\n"
      callProcess "git" ["-C",root,"add","tracked.hs",".gitignore"]
      trackedFile<-canonicalizePath (root </> "tracked.hs")
      let live=addDocument Nothing (newBuffer "untitled needle\n") (addDocument (Just (FileState trackedFile (Just "disk needle\n"))) (replaceBuffer False "live needle\n" (newBuffer "disk needle\n")) initial)
      (_,found)<-success "workspace_search" ["query" .= ("needle"::T.Text)] live
      let texts value=[t | item<-fromMaybe [] (field "matches" value),Just t<-[field "text" item]] :: [T.Text]
      check "workspace search respects ignores and overlays unsaved and untitled buffers" (all (`elem` texts found) ["live needle","untracked needle","untitled needle"] && not (any (`elem` texts found) ["disk needle","ignored needle"]))
      (_,tracked)<-success "workspace_search" ["query" .= ("needle"::T.Text),"trackedOnly" .= True] live
      check "tracked search includes only Git tracked paths with live content" (texts tracked==["live needle"])
      (_,page)<-success "workspace_search" ["query" .= ("needle"::T.Text),"offset" .= (1::Int),"limit" .= (1::Int)] live
      check "workspace search pages deterministic results" (length (texts page)==1 && (field "total" page::Maybe Int)==field "total" found)
      createDirectory (root </> "authority")
      TIO.writeFile (root </> "authority/config.toml") "private needle\n"
      secretPath<-canonicalizePath (root </> "authority/config.toml")
      let protected=initial {guestPrivatePaths=[secretPath,root </> "future/session.json"]}
          privateLive=addDocument (Just (FileState secretPath (Just "private needle\n"))) (newBuffer "unsaved private needle\n") protected
          privateBid=maybe (error "missing private buffer") sourceFixtureBuffer (activeWindow privateLive)
      callProcess "git" ["-C",root,"add","authority/config.toml"]
      (_,privateSearch)<-success "workspace_search" ["query" .= ("private needle"::T.Text)] privateLive
      (_,privateTracked)<-success "workspace_search" ["query" .= ("private needle"::T.Text),"trackedOnly" .= True] privateLive
      check "workspace search excludes private disk and live contents" (null (texts privateSearch) && null (texts privateTracked))
      rejected "buffer_apply_diff" ["bufferId" .= privateBid,"revision" .= (0::Int),"diff" .= ("@@ -1 +1 @@\n-unsaved private needle\n+changed\n"::T.Text)] privateLive
      mapM_ (\path->rejected "workspace_files" (operation "delete" path) protected) ["authority/config.toml","authority"]
      rejected "workspace_files" (operation "rename" "authority"++["to" .= ("moved-authority"::T.Text)]) protected
      rejected "workspace_files" (operation "rename" "untracked.hs"++["to" .= ("future"::T.Text)]) protected
      rejected "workspace_files" (operation "mkdir" "future") protected
      rejected "workspace_files" (operation "create_file" "future/session.json") protected
      when (os/="mingw32") $ do
        createDirectoryLink (root </> "authority") (root </> "authority-alias")
        rejected "workspace_files" (operation "delete" "authority-alias/config.toml") protected
        createFileLink secretPath (root </> "key-alias.txt")
        callProcess "git" ["-C",root,"add","key-alias.txt"]
        (_,aliasSearch)<-success "workspace_search" ["query" .= ("private needle"::T.Text),"trackedOnly" .= True] protected
        check "search resolves private symlink aliases before reading" (null (texts aliasSearch))
      preservedSecret<-TIO.readFile secretPath
      check "rejected operations leave authority files unchanged" (preservedSecret=="private needle\n")
    putStrLn "workspace filesystem checks passed"

patchChecks :: IO ()
patchChecks=do
  let patched old diff=fmap fst (applyUnifiedDiff old diff)
  check "unified diff multiple hunks apply exact context atomically"
    (patched "a\nb\nc\nd\n" "@@ -1,2 +1,2 @@\n a\n-b\n+B\n@@ -4 +4,2 @@\n-d\n+D\n+E\n"==Right "a\nB\nc\nD\nE\n")
  check "unified diff creates initial lines in an empty buffer" (patched "" "@@ -0,0 +1 @@\n+hello\n"==Right "hello\n")
  check "unified diff deletes the entire buffer" (patched "hello\n" "@@ -1 +0,0 @@\n-hello\n"==Right "")
  check "unified diff preserves no-newline markers" (patched "old" "@@ -1 +1 @@\n-old\n\\ No newline at end of file\n+new\n\\ No newline at end of file\n"==Right "new")
  check "unified diff rejects insertion that silently joins an unterminated line"
    (case patched "a" "@@ -1,0 +2 @@\n+b\n" of Left _->True; _->False)
  check "unified diff rejects bad later context without changing earlier hunks"
    (case patched "a\nb\n" "@@ -1 +1 @@\n-a\n+A\n@@ -2 +2 @@\n-wrong\n+B\n" of Left _->True; _->False)
  check "unified diff rejects multiple files"
    (case patched "a\n" "--- a\n+++ b\n@@ -1 +1 @@\n-a\n+A\n--- c\n+++ d\n@@ -1 +1 @@\n-a\n+B\n" of Left _->True; _->False)
  check "unified diff rejects mismatched destination line offsets"
    (case patched "a\n" "@@ -1 +2 @@\n-a\n+A\n" of Left _->True; _->False)

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.:key))

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)

temporary :: IO FilePath
temporary=do
  parent<-getTemporaryDirectory
  (path,h)<-openTempFile parent "thc-workspace-files"
  hClose h
  removeFile path
  createDirectory path
  canonicalizePath path
