{-# LANGUAGE OverloadedStrings #-}
module WorkspaceMCPCheck (checks) where

import Control.Exception (bracket)
import Control.Monad (unless, foldM, when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock (getCurrentTime, addUTCTime)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.List (find)
import System.Info (os)
import qualified Data.Text as T
import System.Directory (canonicalizePath, getTemporaryDirectory, removeFile, createDirectory, removePathForcibly, createDirectoryIfMissing, createFileLink, setModificationTime)
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
  let terminal=addReadOnly "Terminal fixture" "hello" original
      terminalId=windowId (fromJust (activeWindow terminal))
  (pinned,pinResult)<-call terminal "editor_arrange" ["action" .= ("pin"::T.Text)]
  ok "workspace arrange pins the same terminal and reports panel membership" (succeeded pinResult && maybe False (windowPinned pinned) (activeWindow pinned))
  mapM_ (\action->do
    (_,reply)<-call pinned "editor_arrange" (["action" .= (action::T.Text)]++case action of "move"->["x" .= (0::Int),"y" .= (1::Int)];"resize"->["width" .= (50::Int),"height" .= (10::Int)];_->[])
    ok "workspace rejects direct geometry changes on pinned terminals" (rejected reply)) ["move","resize","zoom","split_vertical","split_horizontal"]
  (messages,_)<-call pinned "editor_panels" ["messages" .= True]
  (focused,focusResult)<-call messages "editor_arrange" ["action" .= ("focus"::T.Text),"windowId" .= terminalId]
  (unpinned,unpinResult)<-call focused "editor_arrange" ["action" .= ("unpin"::T.Text)]
  ok "workspace focus selects hidden terminal tab and unpin restores its original window" (succeeded focusResult && bottomTerminal focused==Just terminalId && succeeded unpinResult && not (maybe False (windowPinned unpinned) (activeWindow unpinned)))
  (_,badPin)<-call original "editor_arrange" ["action" .= ("pin"::T.Text)]
  ok "workspace refuses pinning an ordinary source window" (rejected badPin)
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
  let rightWindow=last (windows split)
  (moved,moveResult)<-call split "editor_arrange" ["action" .= ("move"::T.Text),"windowId" .= windowId rightWindow,"x" .= (40::Int),"y" .= (1::Int)]
  ok "move carries the tiled neighbor through the same rule as title dragging"
    (succeeded moveResult && map bounds (windows moved)==[Rect 40 1 50 33,Rect 0 1 40 33] && buffers moved==buffers split)
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
    planChecks root project
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

-- These are Cabal-emitted flat and Custom Setup component shapes, rather than
-- a second Cabal package-description parser.
planChecks :: FilePath -> Desktop -> IO ()
planChecks root desktop=do
  let call d=do (_,reply)<-workspaceTool effects d "workspace_project" (object []); reply
      valueField key value=parseMaybe (withObject "value" (.: key)) value
      plan result=case result of Right value->valueField "cabalPlan" value; _->Nothing
      status result=plan result >>= valueField "status" :: Maybe T.Text
      ok label condition=unless condition (error label)
      file=root </> "dist-newstyle/cache/plan.json"
      unit ident name component dependencies=object
        ["id" .= (ident::T.Text),"pkg-name" .= (name::T.Text),"pkg-version" .= ("1.0"::T.Text),
         "type" .= ("configured"::T.Text),"style" .= ("local"::T.Text),"component-name" .= (component::T.Text),
         "depends" .= (dependencies::[T.Text]),"exe-depends" .= ["build-tool-unit"::T.Text],"pkg-src" .= object ["type" .= ("local"::T.Text),"path" .= root]]
      flat=unit "sample-unit" "sample" "exe:sample" ["base-unit"]
      nested=object ["id" .= ("custom-unit"::T.Text),"pkg-name" .= ("custom"::T.Text),"pkg-version" .= ("2"::T.Text),
        "style" .= ("local"::T.Text),"components" .= object
          ["lib" .= object ["depends" .= ["base-unit"::T.Text]],"setup" .= object ["depends" .= ["Cabal-unit"::T.Text]]]]
      base=object ["id" .= ("base-unit"::T.Text),"pkg-name" .= ("base"::T.Text),"pkg-version" .= ("4.22.0.0"::T.Text),"type" .= ("pre-existing"::T.Text)]
      fixture=object ["compiler-id" .= ("ghc-9.14.1"::T.Text),"cabal-version" .= ("3.16"::T.Text),
        "install-plan" .= [flat,nested,base],"repository-url" .= ("https://user:private-secret@invalid/"::T.Text)]
      write value=BL.writeFile file (encode value)
  missing<-call desktop
  ok "project reports a missing Cabal plan without generating one" (status missing==Just "missing")
  createDirectoryIfMissing True (root </> "dist-newstyle/cache")
  write fixture
  found<-call desktop
  let graph=plan found
      units=graph >>= valueField "units" :: Maybe [Value]
  ok "Cabal plan exposes emitted units and provenance" (status found==Just "available" && maybe False ((==3).length) units && (graph >>= valueField "compilerId") == Just ("ghc-9.14.1"::T.Text))
  let pick ident=units >>= find (\entry->valueField "id" entry==Just (ident::T.Text))
      nestedComponents=pick "custom-unit" >>= valueField "components" :: Maybe [Value]
      setup=nestedComponents >>= find (\entry->valueField "name" entry==Just ("setup"::T.Text))
  ok "Cabal graph retains exact unit/component dependency and build-tool edges" ((pick "sample-unit" >>= valueField "depends")==Just ["base-unit"::T.Text] &&
    (pick "sample-unit" >>= valueField "exeDepends")==Just ["build-tool-unit"::T.Text] && (setup >>= valueField "depends")==Just ["Cabal-unit"::T.Text])
  ok "local package roots are relative and absent dependency data remains unknown" ((pick "sample-unit" >>= valueField "sourceRoot")==Just (Just "."::Maybe FilePath) &&
    (pick "base-unit" >>= valueField "dependenciesKnown")==Just False)
  ok "plan raw fields and credential URLs are never returned" (not ("private-secret" `T.isInfixOf` T.pack (show found)))
  ok "plan existence alone never claims freshness" ((graph >>= valueField "freshness" >>= valueField "status") == Just ("unknown"::T.Text))
  now<-getCurrentTime
  setModificationTime file (addUTCTime (-60) now)
  stale<-call desktop
  ok "newer manifest marks the plan stale" ((plan stale >>= valueField "freshness" >>= valueField "status") == Just ("stale"::T.Text))
  write fixture
  let manifest=addDocument (Just (FileState (root </> "sample.cabal") (Just "disk"))) (replaceSelection (Selection 0 0) "live" (newBuffer "disk")) desktop
  unsaved<-call manifest
  ok "unsaved package manifest marks plan stale" ((plan unsaved >>= valueField "freshness" >>= valueField "status") == Just ("stale"::T.Text))
  let private=desktop {guestPrivatePaths=[root </> "sample.cabal",file]}
  hidden<-call private
  ok "protected plan and package paths stay hidden" (status hidden==Just "unavailable" && case hidden of Right value->valueField "packageFile" value==Just (Nothing::Maybe FilePath); _->False)
  let external=object ["id" .= ("outside"::T.Text),"style" .= ("local"::T.Text),"pkg-src" .= object ["type" .= ("local"::T.Text),"path" .= (root </> "../private-source-root")]]
      authority=object ["id" .= ("private"::T.Text),"style" .= ("local"::T.Text),"pkg-src" .= object ["type" .= ("local"::T.Text),"path" .= (root </> "authority-source")]]
      malformed=object ["id" .= ("https://private-credential@invalid/"::T.Text)]
  write (object ["install-plan" .= [flat,external,authority,malformed]])
  paths<-call desktop {guestPrivatePaths=[root </> "authority-source"]}
  ok "external and protected source roots and malformed identities are omitted" (case plan paths of
    Just value->valueField "omittedUnits" value==Just (1::Int) && valueField "graphComplete" value==Just False && not (any (`T.isInfixOf` T.pack (show value)) ["private-source-root","authority-source","private-credential"])
    _->False)
  write (object ["install-plan" .= [unit ("unit-"<>T.pack (show n)) "sample" "lib" ["outside-subset"] | n<-[1..4200::Int]]])
  truncated<-call desktop
  ok "large graphs expose truncation without dropping retained dependency edges" (case plan truncated of
    Just value->valueField "totalUnits" value==Just (4200::Int) && maybe False (>0) (valueField "truncatedUnits" value :: Maybe Int) && valueField "graphComplete" value==Just False && BL.length (encode value)<=524288 &&
      maybe False (all (\u->valueField "depends" u==Just ["outside-subset"::T.Text])) (valueField "units" value :: Maybe [Value])
    _->False)
  BS.writeFile file "not JSON private-secret"
  invalid<-call desktop
  ok "malformed plan errors omit raw contents" (status invalid==Just "invalid" && not ("private-secret" `T.isInfixOf` T.pack (show invalid)))
  BS.writeFile file (BS.replicate (8*1024*1024+1) 32)
  oversized<-call desktop
  ok "plan reads are bounded" (status oversized==Just "too-large")
  removeFile file
  when (os/="mingw32") $ do
    let secret=root </> "authority-plan.json"
    BL.writeFile secret (encode fixture)
    createFileLink secret file
    linked<-call desktop {guestPrivatePaths=[secret]}
    ok "plan symlink cannot bypass source protection" (status linked==Just "unavailable")
    removeFile file
