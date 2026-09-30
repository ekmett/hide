{-# LANGUAGE OverloadedStrings #-}
module ToolingCheck (checks) where
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (openTempFile, hClose)
import System.Timeout (timeout)
import THC.Edit.Buffer
import THC.Edit.Files (FileState(..))
import THC.Edit.Model
import THC.Edit.Tooling

checks :: IO ()
checks = do
  let check name ok=unless ok (error name)
      pos n=object ["line" .= (0::Int),"character" .= (n::Int)]
      range a z=object ["start" .= pos a,"end" .= pos z]
      edit a z text=object ["range" .= range a z,"newText" .= (text::String)]
      item=object ["label" .= ("foobar"::String),"textEdit" .= edit 0 3 "foobar"]
      ds=addDocument Nothing (newBuffer "foo x") (initialDesktop (80,25))
  check "completion parses replacement" (completionItems "foo x" 3 (toJSON [item])==[Completion "foobar" [(0,3,"foobar")]])
  check "completion rejects invalid UTF16 range" (null (completionItems "😀x" 1 (toJSON [object ["label" .= ("bad"::String),"textEdit" .= edit 1 2 "bad"]])))
  let applied=applyCompletion [(0,3,"foobar"),(5,5,"!")] ds
  check "completion applies simultaneous edits" (activeText applied=="foobar x!")
  check "completion is one undo" (activeText (fst (runCommand Undo applied))=="foo x")
  check "completion rejects overlap" (buffers (applyCompletion [(0,3,"a"),(1,2,"b")] ds)==buffers ds)
  check "completion rejects coincident insertion ambiguity" (buffers (applyCompletion [(0,0,"a"),(0,0,"b")] ds)==buffers ds)
  check "workspace edit rejects resource operations" (case workspaceEdits (object ["documentChanges" .= [object ["kind" .= ("delete"::String),"uri" .= ("file:///tmp/A.hs"::String)]]]) of Left _ -> True; _ -> False)
  check "workspace edit handles versioned document" (workspaceEdits (object ["documentChanges" .= [object ["textDocument" .= object ["uri" .= ("file:///tmp/A.hs"::String),"version" .= (7::Int)],"edits" .= [edit 0 1 "a"]]]])==Right [("/tmp/A.hs",Just 7,[edit 0 1 "a"])])
  check "hover markdown fits status" (hoverText (object ["contents" .= object ["kind" .= ("markdown"::String),"value" .= ("```haskell\nfoo :: Int\n```\n"::String)]])=="foo :: Int")
  let hover text=hoverText (object ["contents" .= (text::T.Text)])
  check "hover types use Unicode syntax" (hover "f :: forall a. Eq a => a->a"=="f :: ∀ a. Eq a ⇒ a→a")
  check "hover keeps identifiers and quoted arrows" (hover "forallValue :: Proxy \"->\" -> a"=="forallValue :: Proxy \"->\" → a")
  check "unversioned diagnostics cannot masquerade as current edits" (not (diagnosticsCurrent Nothing [1]) && diagnosticsCurrent Nothing [0] && not (diagnosticsCurrent (Just 0) [1]))
  check "split buffer count unaffected" (M.size (buffers applied)==1)
  bracket temporary removePathForcibly $ \root -> do
    let server=root </> "fake-hls"
        source=root </> "Main.hs"
        text="foo = 1\n"
        desktop=addDocument (Just (FileState source Nothing)) (newBuffer text) (initialDesktop (80,25))
        core d _=pure (False,d)
    writeFile server "#!/usr/bin/env python3\nimport time\ntime.sleep(30)\n"
    permissions<-getPermissions server
    setPermissions server permissions {executable=True}
    writeFile source (T.unpack text)
    writeFile (root </> "Bad.hs") "\NUL"
    bracket (lookupEnv "THC_EDIT_HLS") (maybe (unsetEnv "THC_EDIT_HLS") (setEnv "THC_EDIT_HLS")) $ \_ -> do
      setEnv "THC_EDIT_HLS" server
      withTooling $ \tooling -> do
        let binary=desktop {buffers=M.map (\doc -> restyle doc {documentBuffer=newByteBuffer "a\0b"}) (buffers desktop)}
        (_,blocked)<-toolingEffects tooling core binary [LanguageRequest TypeInfo]
        check "hex buffers do not reach HLS text requests" (buffers blocked==buffers binary && status blocked=="Open a saved Haskell source file first.")
        began<-timeout 1000000 (toolingEffects tooling core desktop [LanguageRequest (RenameAt "bar")])
        (_,preparing)<-maybe (error "rename preparation blocked dispatch") pure began
        check "rename preparation returns before disk result" (status preparing=="Preparing rename...")
        let await d = do
              next<-tickTooling tooling core d
              if "Cannot prepare rename:" `T.isPrefixOf` status next then pure next else threadDelay 10000 >> await next
        failed<-timeout 2000000 (await preparing)
        check "snapshot errors surface and preserve buffers" (maybe False (\d -> buffers d==buffers desktop) failed)
        (_,again)<-toolingEffects tooling core desktop [LanguageRequest (RenameAt "bar")]
        let changed=insertText "x" again
        stale<-tickTooling tooling core changed
        check "editing while preparing cancels rename" (status stale=="Rename target changed; request it again." && buffers stale==buffers changed)
        (_,beforeSaveAs)<-toolingEffects tooling core desktop [LanguageRequest (RenameAt "bar")]
        let moved=beforeSaveAs {buffers=M.map (\doc -> doc {documentFile=Just (FileState (root </> "Elsewhere.hs") Nothing)}) (buffers beforeSaveAs)}
        renamedPath<-tickTooling tooling core moved
        check "Save As while preparing cancels original target" (status renamedPath=="Rename target changed; request it again." && buffers renamedPath==buffers moved)
  putStrLn "tooling checks passed"
  where
    temporary = do
      base<-getTemporaryDirectory
      (path,h)<-openTempFile base "thc-tooling-check"
      hClose h
      removeFile path
      createDirectory path
      canonicalizePath path
