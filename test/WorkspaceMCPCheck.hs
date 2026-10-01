{-# LANGUAGE OverloadedStrings #-}
module WorkspaceMCPCheck (checks) where

import Control.Exception (bracket)
import Control.Monad (unless, foldM)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import System.Directory (canonicalizePath, getTemporaryDirectory, removeFile, createDirectory, removePathForcibly)
import System.IO (openBinaryTempFile, hClose)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import THC.Edit.Buffer
import THC.Edit.Files
import THC.Edit.Model
import THC.Edit.WorkspaceMCP

checks :: IO ()
checks=do
  let original=addDocument Nothing (newBuffer "λx\nnext") (initialDesktop (100,35))
      current=fromJust (activeWindow original)
      call d name fields=do
        (next,reply)<-workspaceTool effects d name (object fields)
        result<-reply
        pure (next,result)
      ok name predicate=unless predicate (error name)
      succeeded (Right _)=True
      succeeded _=False
      rejected (Left _)=True
      rejected _=False
      cursor=caret . selection . fromJust . activeWindow
      doc=fromJust . activeDocument
  ok "workspace schemas declare all tools" (length workspaceTools==length workspaceToolNames && "editor_file" `elem` workspaceToolNames)
  let privateReview=addReadOnly "Disk changes: /authority/private-session-key.json" "private review" original {guestPrivatePaths=["/authority/private-session-key.json"]}
  (_,reviewLayout)<-call privateReview "editor_layout" []
  ok "workspace layout hides protected filenames in disk review labels" (case reviewLayout of
    Right value->not ("private-session-key" `T.isInfixOf` T.pack (show value)) && "[private]" `T.isInfixOf` T.pack (show value)
    _->False)
  (navigated,result)<-call original "editor_navigate" ["line" .= (2::Int),"column" .= (3::Int)]
  ok "navigation uses live Unicode buffer offsets" (succeeded result && cursor navigated==5)
  (unchanged,outOfRange)<-call original "editor_navigate" ["line" .= (maxBound::Int)]
  ok "navigation rejects extreme positions without clamping" (rejected outOfRange && unchanged==original)
  (_,badTarget)<-call original "editor_navigate" ["windowId" .= windowId current,"bufferId" .= bufferId current]
  ok "navigation rejects ambiguous targets" (rejected badTarget)
  (_,typo)<-call original "editor_arrange" ["action" .= ("move"::T.Text),"heigth" .= (9::Int)]
  ok "workspace tools reject unknown arguments" (rejected typo)
  (hex,hexResult)<-call original "editor_mode" ["mode" .= ("hex"::T.Text)]
  ok "hex mode preserves UTF-8 bytes" (succeeded hexResult && byteMode (documentBuffer (doc hex)) && bufferBytes (documentBuffer (doc hex))==bufferBytes (documentBuffer (doc original)))
  (atByte,byteResult)<-call hex "editor_navigate" ["byteOffset" .= (2::Int)]
  ok "hex navigation counts bytes" (succeeded byteResult && cursor atByte==2)
  (_,textByte)<-call original "editor_navigate" ["byteOffset" .= (1::Int)]
  ok "byte navigation requires explicit hex mode" (rejected textByte)
  let binary=addDocument Nothing (newByteBuffer (BS.pack [255,0,1])) (initialDesktop (100,35))
  (binaryAfter,binaryResult)<-call binary "editor_mode" ["mode" .= ("text"::T.Text)]
  ok "invalid text conversion preserves bytes and state" (rejected binaryResult && binaryAfter==binary)
  (split,splitResult)<-call original "editor_arrange" ["action" .= ("split_vertical"::T.Text)]
  ok "split shares existing buffer through model" (succeeded splitResult && length (windows split)==2 && M.size (buffers split)==1 && map bounds (windows split)==map bounds (windows (fst (runCommand SplitVertical original))))
  (resized,resizeResult)<-call split "editor_arrange" ["action" .= ("resize"::T.Text),"width" .= (40::Int),"height" .= height (bounds (fromJust (activeWindow split)))]
  let win=fromJust (activeWindow split); expected=resizeWindowBounds (windowId win) ((bounds win) {width=40}) split
  ok "resize reuses shared-edge docking behavior" (succeeded resizeResult && map bounds (windows resized)==map bounds (windows expected))
  (shown,shownResult)<-call original "editor_panels" ["messages" .= True,"messagesHeight" .= (8::Int)]
  ok "messages tool resizes existing dock rules" (succeeded shownResult && problemsVisible shown && drag shown==Nothing && problemsHeight shown==8)
  (badPanel,badPanelResult)<-call shown "editor_panels" ["messages" .= False,"messagesHeight" .= (8::Int)]
  ok "invalid hidden-panel sizing is atomic" (rejected badPanelResult && badPanel==shown)
  (hidden,hiddenResult)<-call shown "editor_panels" ["messages" .= False]
  ok "messages panel hides without removing editor buffers" (succeeded hiddenResult && not (problemsVisible hidden) && buffers hidden==buffers shown)
  let dirtyDesktop=insertText "unsaved " original
      currentRevision=revision (documentBuffer (doc dirtyDesktop))
  (stale,staleResult)<-call dirtyDesktop "editor_file" ["action" .= ("close"::T.Text),"revision" .= (currentRevision-1),"dirtyAction" .= ("discard"::T.Text)]
  ok "stale revision cannot discard edits" (rejected staleResult && stale==dirtyDesktop)
  (refused,refusedResult)<-call dirtyDesktop "editor_file" ["action" .= ("close"::T.Text),"revision" .= currentRevision]
  ok "dirty close requires explicit decision" (rejected refusedResult && refused==dirtyDesktop)
  (closed,closedResult)<-call dirtyDesktop "editor_file" ["action" .= ("close"::T.Text),"revision" .= currentRevision,"dirtyAction" .= ("discard"::T.Text)]
  ok "explicit discard closes selected window" (succeeded closedResult && null (windows closed) && M.null (buffers closed))
  let dirtySplit=fst (runCommand SplitVertical dirtyDesktop)
  (viewClosed,viewResult)<-call dirtySplit "editor_file" ["action" .= ("close"::T.Text),"revision" .= currentRevision,"dirtyAction" .= ("discard"::T.Text)]
  ok "closing split retains unsaved shared buffer" (succeeded viewResult && length (windows viewClosed)==1 && buffers viewClosed==buffers dirtySplit)
  let diagnostic=Diagnostic "/live.hs" (Just 3) 1 2 1 (T.replicate 10000 "x")
      withDiagnostics=original {diagnostics=[diagnostic]}
  (_,diagResult)<-call withDiagnostics "workspace_diagnostics" ["limit" .= (1::Int)]
  ok "diagnostics output is bounded" (case diagResult of Right value->maybe False ((==8192).T.length) (parseMaybe (withObject "result" $ \o->do entries<-o .: "diagnostics"; case entries of entry:_->withObject "entry" (.: "message") entry; _->fail "missing diagnostic") value); _->False)
  (_,badLimit)<-call original "workspace_diagnostics" ["limit" .= (201::Int)]
  ok "diagnostics reject oversized pages" (rejected badLimit)
  temporary<-getTemporaryDirectory
  bracket (openBinaryTempFile temporary "workspace-mcp") (\(path,_)->removeFile path) $ \(path,handle)->do
    BS.hPut handle "disk"
    hClose handle
    absolute<-canonicalizePath path
    (opened,openResult)<-call original "editor_file" ["action" .= ("open"::T.Text),"path" .= absolute]
    ok "file open uses editor effects" (succeeded openResult && fmap filePath (documentFile (doc opened))==Just absolute)
    let private=original {guestPrivatePaths=[absolute]}
    (privateOpen,privateOpenResult)<-call private "editor_file" ["action" .= ("open"::T.Text),"path" .= absolute]
    ok "file open refuses authority files" (rejected privateOpenResult && privateOpen==private)
    (_,privateNavigate)<-call private "editor_navigate" ["path" .= absolute]
    ok "navigation cannot open authority files" (rejected privateNavigate)
    (privateSave,privateSaveResult)<-call private "editor_file" ["action" .= ("save"::T.Text),"path" .= absolute,"revision" .= revision (documentBuffer (doc private))]
    privateDisk<-BS.readFile path
    ok "save-as cannot overwrite authority files" (rejected privateSaveResult && privateSave==private && privateDisk=="disk")
    let privateOpened=opened {guestPrivatePaths=[absolute]}
    (_,privateCloseResult)<-call privateOpened "editor_file" ["action" .= ("close"::T.Text),"revision" .= revision (documentBuffer (doc privateOpened))]
    ok "human-open authority buffer cannot be closed by guest" (rejected privateCloseResult)
    (_,privateLayout)<-call privateOpened "editor_layout" []
    ok "private workspace layout suppresses secret-bearing file paths" (case privateLayout of
      Right value->case parseMaybe (withObject "layout" (.: "windows")) value of
        Just entries->all (\entry->parseMaybe (withObject "window" (.: "path")) entry/=Just (Just absolute)) (entries::[Value])
        _->False
      _->False)
    (_,privateProject)<-call privateOpened "workspace_project" []
    ok "private active file is not exposed as project source" (case privateProject of Right value->parseMaybe (withObject "project" (.: "source")) value==Just (Nothing::Maybe FilePath); _->False)
    let edited=insertText "live " opened
        editedRevision=revision (documentBuffer (doc edited))
    (reopened,reopenResult)<-call edited "editor_file" ["action" .= ("open"::T.Text),"path" .= absolute]
    ok "reopening file preserves unsaved buffer" (succeeded reopenResult && buffers reopened==buffers edited)
    (saved,savedResult)<-call edited "editor_file" ["action" .= ("save"::T.Text),"revision" .= editedRevision]
    disk<-BS.readFile path
    ok "save checks revision and commits live bytes" (succeeded savedResult && not (dirty (documentBuffer (doc saved))) && disk==bufferBytes (documentBuffer (doc edited)))
    let editedAgain=insertText "later " saved
    BS.writeFile path "external change"
    (conflict,conflictResult)<-call editedAgain "editor_file" ["action" .= ("close"::T.Text),"revision" .= revision (documentBuffer (doc editedAgain)),"dirtyAction" .= ("save"::T.Text)]
    preserved<-BS.readFile path
    ok "save conflicts retain buffer and disk data" (rejected conflictResult && buffers conflict==buffers editedAgain && preserved=="external change")
  let createProject=do
        (path,handle)<-openBinaryTempFile temporary "workspace-mcp-project"
        hClose handle
        removeFile path
        createDirectory path
        canonicalizePath path
  bracket createProject removePathForcibly $ \root->do
    BS.writeFile (root </> "sample.cabal") "name: sample\nversion: 0.1\nlibrary\n  exposed-modules: Sample\n"
    let project=dirtyDesktop {defaultDirectory=Just root}
    (_,projectResult)<-call project "workspace_project" []
    ok "project context reuses package root discovery" (case projectResult of
      Right value->parseMaybe (withObject "project" (.: "root")) value==Just root
      _->False)
    (code,_,_)<-readProcessWithExitCode "git" ["init","--quiet",root] ""
    ok "temporary Git fixture initializes" (code==ExitSuccess)
    (_,gitResult)<-call project "workspace_git" ["view" .= ("status"::T.Text)]
    ok "Git status distinguishes disk changes from unsaved buffers" (case gitResult of
      Right value->parseMaybe (withObject "git" (.: "diskDirty")) value==Just True &&
        maybe False (not . null) (parseMaybe (withObject "git" (.: "unsavedBuffers")) value :: Maybe [Value])
      _->False)
    (_,diffResult)<-call project "workspace_git" ["view" .= ("diff"::T.Text),"path" .= ("sample.cabal"::T.Text)]
    ok "Git diff reads disk without incorporating unsaved content" (case diffResult of
      Right value->parseMaybe (withObject "git" (.: "includesUnsaved")) value==Just False &&
        maybe False (T.isInfixOf "sample") (parseMaybe (withObject "git" (.: "diff")) value)
      _->False)
    (_,absentConfigDiff)<-call (project {guestPrivatePaths=[root </> "thc.toml",root </> "missing-global.toml"]}) "workspace_git" ["view" .= ("diff"::T.Text)]
    ok "default absent private configuration does not disable whole repository diff" (succeeded absentConfigDiff)
    BS.writeFile (root </> "authority.toml") "private-git-secret"
    let privateProject=project {guestPrivatePaths=[root </> "authority.toml"]}
    (_,privateWholeDiff)<-call privateProject "workspace_git" ["view" .= ("diff"::T.Text)]
    (_,privateFileDiff)<-call privateProject "workspace_git" ["view" .= ("diff"::T.Text),"path" .= ("authority.toml"::T.Text)]
    (_,wildcardDiff)<-call privateProject "workspace_git" ["view" .= ("diff"::T.Text),"path" .= ("*.toml"::T.Text)]
    (_,magicDiff)<-call privateProject "workspace_git" ["view" .= ("diff"::T.Text),"path" .= (":(glob)*"::T.Text)]
    (_,safeFileDiff)<-call privateProject "workspace_git" ["view" .= ("diff"::T.Text),"path" .= ("sample.cabal"::T.Text)]
    ok "Git diffs omit private files while whole and explicit safe diffs work" (succeeded privateWholeDiff && rejected privateFileDiff && succeeded wildcardDiff && succeeded magicDiff && succeeded safeFileDiff)
    ok "whole diff reports omission without private paths or contents" (case privateWholeDiff of
      Right value->maybe False (\text->"sample" `T.isInfixOf` text && not ("authority.toml" `T.isInfixOf` text) && not ("private-git-secret" `T.isInfixOf` text)) (parseMaybe (withObject "diff" (.: "diff")) value) && parseMaybe (withObject "diff" (.: "omittedFiles")) value==Just (1::Int)
      _->False)
    createDirectory (root </> "nested")
    BS.writeFile (root </> "nested/thc.toml") "private nested context"
    BS.writeFile (root </> "nested/public.hs") "ordinary nested source"
    (_,directoryDiff)<-call privateProject "workspace_git" ["view" .= ("diff"::T.Text),"path" .= ("nested"::T.Text)]
    ok "selected directories filter unregistered nested project configuration" (case directoryDiff of
      Right value->maybe False (\text->"ordinary nested source" `T.isInfixOf` text && not ("private nested context" `T.isInfixOf` text) && not ("thc.toml" `T.isInfixOf` text) && not ("sample.cabal" `T.isInfixOf` text)) (parseMaybe (withObject "diff" (.: "diff")) value)
      _->False)
    let fixtureGit args=do
          (gitCode,_,gitError)<-readProcessWithExitCode "git" (["-C",root,"-c","user.name=Git Test","-c","user.email=test@example.invalid","-c","commit.gpgsign=false","-c","core.hooksPath="<>root </> ".git/hooks"]++args) ""
          unless (gitCode==ExitSuccess) (error gitError)
    fixtureGit ["add","--","nested/thc.toml"]
    fixtureGit ["commit","--quiet","-m","private source fixture"]
    fixtureGit ["mv","--","nested/thc.toml","nested/ordinary-name.txt"]
    (_,renamedSelection)<-call privateProject "workspace_git" ["view" .= ("diff"::T.Text),"path" .= ("nested/ordinary-name.txt"::T.Text)]
    ok "selected rename destination cannot bypass private source filtering" (case renamedSelection of
      Right value->parseMaybe (withObject "diff" (.: "diff")) value==Just ("No changes.\n"::T.Text) && parseMaybe (withObject "diff" (.: "omittedFiles")) value==Just (1::Int)
      _->False)
    (_,missingDiff)<-call privateProject "workspace_git" ["view" .= ("diff"::T.Text),"path" .= ("absent/subdir"::T.Text)]
    ok "selected missing path is a safe empty diff" (case missingDiff of Right value->parseMaybe (withObject "diff" (.: "diff")) value==Just ("No changes.\n"::T.Text); _->False)
  putStrLn "workspace MCP checks passed"

-- Use the same file primitives as the application; unsupported effects fail
-- loudly so navigation cannot appear successful without opening its target.
effects :: Desktop -> [Effect] -> IO (Bool,Desktop)
effects desktop requests=(True,) <$> foldM apply desktop requests
  where
    apply d (ReadPath path)=do
      loaded<-loadFile path
      pure $ case loaded of
        Left err -> d {status=T.pack err}
        Right (file,buffer) -> addDocument (Just file) buffer d
    apply d (SaveDocument ident destination _)=case M.lookup ident (buffers d) of
      Nothing -> error "missing test buffer"
      Just document -> case (documentFile document,destination) of
        (Just file,Nothing) -> do
          savedFile<-saveFile file (documentBuffer document)
          pure $ case savedFile of
            Left err -> d {status=T.pack err}
            Right state -> d {buffers=M.insert ident document {documentFile=Just state,documentBuffer=markSaved (documentBuffer document)} (buffers d)}
        _ -> error "unsupported test save"
    apply _ request=error ("Unexpected workspace test effect: "++show request)
