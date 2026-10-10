{-# LANGUAGE CPP, OverloadedStrings #-}
module DocsMCPCheck (checks) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
import Data.IORef
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (openBinaryTempFile, hClose)
import System.IO.Error (tryIOError)
#ifndef mingw32_HOST_OS
import qualified System.Posix.Files as Posix
#endif
import qualified Hide.Documentation as Host
import qualified Hide.Plugin.Documentation as Docs
import qualified Hide.Plugin.Tool as Tool
import Hide.Plugin.Services (EditorServices(..))
import qualified Hide.DocsTools as DocsTools
import qualified Hide.Plugin.Command as Commands
import Hide.DocumentationHost
import Hide.Environment (withEnvironmentCommands,environmentServices)
import Hide.Model

checks :: IO ()
checks=Tool.withTools [] DocsTools.tools $ \tools->withDocsCommands $ \commands->withEnvironmentCommands $ \environment->bracket temporary removePathForcibly $ \root->do
  let editor=root </> "editor"
      compiler=root </> "compiler"
      configuration=root </> "config"
      project=root </> "project"
      d=(initialDesktop (80,25)) {defaultDirectory=Just project}
      check label condition=unless condition (error label)
      run name arguments=do
        context<-captureDocsContext d
        Tool.callTool tools (EditorServices (docsServices commands context) (environmentServices environment root)) name (object arguments)
      value name reply=case reply of Right object'->parseMaybe (withObject "reply" (.: name)) object'; _->Nothing
      failed (Left _)=True
      failed _=False
      rendered=TE.decodeUtf8 . BL.toStrict . encode
  mapM_ (createDirectoryIfMissing True) [editor </> "docs/nested",compiler </> "docs",compiler </> "compiler/docs",configuration </> "thc-edit",project]
  BS.writeFile (project </> "sample.cabal") "name: sample\nversion: 0.1\n"
  BS.writeFile (editor </> "README.md") "# Editor manual\nStart here\n"
  BS.writeFile (editor </> "docs/guide.md") "# Editing\nNeedle first\n## Navigation\na.*b literal\n```haskell\n## Not a heading\n```\nneedle second\n"
  BS.writeFile (editor </> "docs/nested/extra.md") (TE.encodeUtf8 "# Unicode λ\nNeedle third\n")
  BS.writeFile (editor </> "docs/large.md") (BS.replicate 1048577 120)
  BS.writeFile (editor </> "docs/invalid.md") (BS.pack [255,254])
  BS.writeFile (editor </> "docs/long.md") (BS.replicate 200000 120)
  BS.writeFile (editor </> "docs/ignored.json") "{\"token\":\"private\"}"
  BS.writeFile (compiler </> "README.md") "# Compiler manual\nTHC compiler\n"
  BS.writeFile (compiler </> "docs/core.md") "# Core\nCompiler internals\n"
  BS.writeFile (compiler </> "compiler/README.md") "# Compiler component\n"
  BS.writeFile (compiler </> "compiler/docs/ir.md") "# IR\nLowering\n"
  bracket (saveEnv ["hide_datadir","THC_ROOT","XDG_CONFIG_HOME"]) restoreEnv $ \_->do
    setEnv "hide_datadir" editor
    setEnv "THC_ROOT" compiler
    setEnv "XDG_CONFIG_HOME" configuration
    check "editor documentation tools cannot expose agent coordination" (not (Tool.hasTool tools "agent_spawn"))
    check "documentation tool schemas match dispatch names" (length (Tool.toolDefinitions tools)==3 && all (Tool.hasTool tools) ["docs_list","docs_search","docs_read"])
    listed<-run "docs_list" []
    check "packaged docs include README and nested documents" (case listed of Right v->all (`T.isInfixOf` rendered v) ["README.md","docs/guide.md","docs/nested/extra.md","Unicode λ"] && not ("ignored.json" `T.isInfixOf` rendered v); _->False)
    check "fenced code is excluded from heading metadata" (case listed of Right v->not ("Not a heading" `T.isInfixOf` rendered v); _->False)
    readRange<-run "docs_read" ["path" .= ("docs/guide.md"::T.Text),"startLine" .= (2::Int),"lineCount" .= (3::Int)]
    check "read returns the requested one-based range" ((value "text" readRange::Maybe T.Text)==Just "Needle first\n## Navigation\na.*b literal" && (value "lineCount" readRange::Maybe Int)==Just 3)
    Commands.withRegistry $ \registry->do
      command<-either (error . show) id <$> Commands.registerCommand registry Host.readCommand
      let arguments=either (error . T.unpack) id (Docs.readArguments "editor" "docs/guide.md" 2 3)
      typed<-Commands.invoke registry command (\_->pure editor) arguments
      check "typed docs command reads the same bounded page as MCP" (case typed of
        Right page->Docs.pageText page=="Needle first\n## Navigation\na.*b literal" && Right (Commands.codecEncode Docs.readOutput page)==readRange
        _->False)
      check "typed docs arguments reject traversal before IO" (case Docs.readArguments "editor" "../README.md" 1 10 of Left _->True; _->False)
    resolutions<-newIORef (0::Int)
    deferred<-withDocsCommands $ \scoped->do
      let context _=modifyIORef' resolutions (+1) >> pure editor
          services=EditorServices (docsServices scoped context) (environmentServices environment root)
      pure [Tool.callTool tools services name (object arguments) | (name,arguments)<-
        [("docs_read",["path" .= ("README.md"::T.Text)]),("docs_list",[]),("docs_search",["query" .= ("Needle"::T.Text)])]]
    closed<-sequence deferred
    resolved<-readIORef resolutions
    check "deferred docs read/list/search cannot outlive their command scope" (all failed closed && resolved==0)
    pastEnd<-run "docs_read" ["path" .= ("docs/guide.md"::T.Text),"startLine" .= (maxBound::Int)]
    check "extreme read offsets cannot overflow" ((value "text" pastEnd::Maybe T.Text)==Just "" && (value "lineCount" pastEnd::Maybe Int)==Just 0)
    literal<-run "docs_search" ["query" .= ("a.*b"::T.Text)]
    check "search treats regex punctuation literally" (case value "matches" literal::Maybe [Value] of Just [entry]->parseMaybe (withObject "match" (.: "line")) entry==Just (4::Int); _->False)
    insensitive<-run "docs_search" ["query" .= ("NEEDLE"::T.Text),"offset" .= (1::Int),"limit" .= (1::Int)]
    check "search pages case-insensitive literal matches" (case value "matches" insensitive::Maybe [Value] of Just [entry]->"needle second" `T.isInfixOf` rendered entry && "Navigation" `T.isInfixOf` rendered entry; _->False)
    sensitive<-run "docs_search" ["query" .= ("NEEDLE"::T.Text),"caseSensitive" .= True]
    check "case-sensitive search honors letter case" ((value "matches" sensitive::Maybe [Value])==Just [])
    compilerList<-run "docs_list" ["corpus" .= ("thc"::T.Text)]
    check "THC_ROOT exposes compiler documentation sections" (case compilerList of Right v->all (`T.isInfixOf` rendered v) ["docs/core.md","compiler/README.md","compiler/docs/ir.md"]; _->False)
    unsetEnv "THC_ROOT"
    BL.writeFile (configuration </> "thc-edit/run.json") (encode (object ["thcRoot" .= compiler]))
    configured<-run "docs_read" ["corpus" .= ("thc"::T.Text),"path" .= ("docs/core.md"::T.Text)]
    check "existing build THC root configuration resolves docs" ((value "text" configured::Maybe T.Text)==Just "# Core\nCompiler internals\n")
    removeFile (configuration </> "thc-edit/run.json")
    missingRoot<-run "docs_list" ["corpus" .= ("thc"::T.Text)]
    check "missing compiler root is an explicit tool error" (failed missingRoot)
    mapM_ (\path->do result<-run "docs_read" ["path" .= (path::T.Text)]; check "unsafe or non-document paths are refused" (failed result))
      ["../README.md","docs/../README.md","/etc/passwd","docs\\guide.md","C:/secret.md","docs/ignored.json","src/Private.md"]
    tooLarge<-run "docs_read" ["path" .= ("docs/large.md"::T.Text)]
    invalid<-run "docs_read" ["path" .= ("docs/invalid.md"::T.Text)]
    check "oversized and invalid UTF-8 docs are rejected" (failed tooLarge && failed invalid)
    bounded<-run "docs_read" ["path" .= ("docs/long.md"::T.Text)]
    check "single long lines cannot exceed response limit" (maybe False ((==131072).T.length) (value "text" bounded) && (value "truncated" bounded::Maybe Bool)==Just True)
    unknown<-run "docs_list" ["commmand" .= ("oops"::T.Text)]
    hugePage<-run "docs_search" ["query" .= ("Needle"::T.Text),"offset" .= (maxBound::Int)]
    check "unknown arguments and extreme pages are rejected" (failed unknown && failed hugePage)
#ifndef mingw32_HOST_OS
    Posix.createNamedPipe (editor </> "docs/pipe.md") 0o600
    pipe<-run "docs_read" ["path" .= ("docs/pipe.md"::T.Text)]
    check "documentation readers reject FIFOs without blocking" (failed pipe)
    removeFile (editor </> "docs/pipe.md")
#endif
    BS.writeFile (root </> "outside.md") "private outside content"
    linked<-tryIOError (createFileLink (root </> "outside.md") (editor </> "docs/escape.md"))
    case linked of
      Left _ -> putStrLn "documentation symlink check unavailable on this filesystem"
      Right () -> do
        escaped<-run "docs_read" ["path" .= ("docs/escape.md"::T.Text)]
        hidden<-run "docs_list" []
        check "symlink files cannot escape the corpus" (failed escaped && case hidden of Right v->not ("escape.md" `T.isInfixOf` rendered v); _->False)
    directoryLinked<-tryIOError (createDirectoryLink root (editor </> "docs/loop"))
    case directoryLinked of
      Left _ -> pure ()
      Right () -> do
        escaped<-run "docs_read" ["path" .= ("docs/loop/outside.md"::T.Text)]
        index<-run "docs_list" []
        check "symlink directories are not traversed or looped" (failed escaped && (value "indexTruncated" index::Maybe Bool)==Just False)
  putStrLn "documentation MCP checks passed"
  where
    temporary=do
      root<-getTemporaryDirectory
      (path,handle)<-openBinaryTempFile root "thc-docs-mcp"
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path
    saveEnv names=mapM (\name->(name,) <$> lookupEnv name) names
    restoreEnv=mapM_ (\(name,old)->maybe (unsetEnv name) (setEnv name) old)
