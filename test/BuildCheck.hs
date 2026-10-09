{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : BuildCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module BuildCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, evaluate)
import Control.Monad (unless, forM_, when)
import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import GHC.Conc (getAllocationCounter)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.Directory
import System.FilePath ((</>))
import System.IO (openTempFile, hClose)
import System.Info (os)
import System.Timeout (timeout)
import qualified Hide.Build as B
import Hide.BuildJobs
import Hide.Files (FileState(..))
import Hide.Buffer
import Hide.Model
import qualified Hide.Plugin.Window as W
import Hide.Plugin.BufferHost (captureVersion)
import Hide.PluginWindowHost (adoptWindowUpdate)
import qualified Hide.Plugin.Menu as P

checks :: IO ()
checks = bracket temporary removePathForcibly $ \root -> do
  let initial=initialDesktop (80,25)
      ghc=B.BuildConfig B.GHC "ghc" "" "" "" []
      thc=B.BuildConfig B.THC "thc with spaces" "exe:hello world;literal" "compiler root" "runtime path" ["one two"]
      file=root </> "Main.hs"
      output d=maybe "" (\p->let text=W.preparedWindowText p in contentSlice text 0 (contentLength text)) (activePluginWindow d)
      await jobs desktop done=do
        answer<-timeout 15000000 (loop desktop)
        maybe (error "build worker did not finish") pure answer
        where loop d=do fresh<-tickBuildJobs jobs d; if done fresh then pure fresh else threadDelay 10000 >> loop fresh
      finished d=any (`T.isInfixOf` status d) ["completed.","failed (exit", "does not exist", "No such file"]
  check "F9 starts Make" (snd (handleEvent (V.EvKey (V.KFun 9) []) initial)==[ServiceAction "make" []])
  check "Alt-F9 starts Compile" (snd (handleEvent (V.EvKey (V.KFun 9) [V.MAlt]) initial)==[ServiceAction "compile" []])
  check "THC target retains literal argv" . (==Right [("thc with spaces",["run","exe:hello world;literal","--project-dir",root,"--thc-root","compiler root","--runtime","runtime path","--","one two"])]) =<< B.buildPlan B.Run thc root Nothing
  forM_ [B.Compile,B.Make] $ \action -> do
    check "THC builds selected components without runtime or program arguments" .
      (==Right [("thc with spaces",["build","exe:hello world;literal","--project-dir",root,"--thc-root","compiler root"])]) =<< B.buildPlan action thc root Nothing
    check "THC builds the current package when no target is selected" .
      (==Right [("thc with spaces",["build","--project-dir",root,"--thc-root","compiler root"])]) =<< B.buildPlan action (thc {B.buildTarget=""}) root Nothing
    check "THC accepts a library build target literally" .
      (==Right [("thc with spaces",["build","lib:example","--project-dir",root,"--thc-root","compiler root"])]) =<< B.buildPlan action (thc {B.buildTarget="lib:example"}) root Nothing
  check "reject option-like target" (case B.parseBuildConfig ["thc","--help","","", "[]", "0"] of Left _ -> True; _ -> False)
  check "reject invalid program arguments" (case B.parseBuildConfig ["ghc","","","", "not JSON", "1"] of Left _ -> True; _ -> False)
  check "switch default compiler with toolchain" (fmap B.buildExecutable (B.parseBuildConfig ["thc","","","", "[]", "1"])==Right "ghc")
  writeFile file "module Main where\nmain :: IO ()\nmain = putStrLn \"build-check\"\n"
  let sourceDesktop=addDocument (Just (FileState file Nothing)) ((newBuffer "main = pure ()") {undoStack=error "captured build touched source Undo"}) initial
  W.withWindowScope $ \scope->do
    prepared<-W.prepareTextWindow "Make output" "log"
    opening<-W.openWindow scope prepared >>= maybe (fail "prepare output focus") pure
    outputFocused<-adoptWindowUpdate P.HumanMenu opening sourceDesktop
    check "output focus keeps standalone source" (B.buildSource outputFocused==Just file)
    check "output focus keeps project directory without Files" . (==root) =<< B.resolveBuildRoot outputFocused

  installed<-findExecutable "ghc"
  forM_ installed $ \_ -> withBuildJobs $ \jobs -> do
    Right run<-B.buildPlan B.Run ghc root (Just file)
    running<-startBuildJob jobs "Run" root run initial
    check "captured job opens semantic output without source buffers"
      (M.null (buffers running) && maybe False ((==Nothing) . bufferId) (activeWindow running))
    ran<-await jobs running finished
    check "real GHC Run resolves the named compiler on PATH" (status ran=="Run completed." && "build-check" `T.isInfixOf` output ran)

  let relativeCompiler="compiler with spaces"++(if os=="mingw32" then ".exe" else "")
      selectedCompiler=root </> relativeCompiler
  writeFile selectedCompiler "selected compiler path fixture"
  permissions<-getPermissions selectedCompiler
  setPermissions selectedCompiler permissions {executable=True}
  resolvedCompiler<-canonicalizePath selectedCompiler
  check "Run resolves a selected relative compiler against its build root" .
    (==Right [("runghc",["-f",resolvedCompiler,file,"one two"])]) =<<
      B.buildPlan B.Run (ghc {B.buildExecutable="." </> relativeCompiler,B.buildArguments=["one two"]}) root (Just file)
  check "Run reports the missing selected compiler before launch" .
    (==Left "The selected GHC executable was not found or is not executable: ./absent-compiler") =<<
      B.buildPlan B.Run (ghc {B.buildExecutable="./absent-compiler"}) root (Just file)

  check "standalone compile checks source" . (==Right [("ghc",["--make","-fno-code","-fdiagnostics-color=never",file])]) =<< B.buildPlan B.Compile ghc root (Just file)
  writeFile (root </> "fixture.cabal") "name: fixture\n"
  check "Cabal project builds with selected compiler" . (==Right [("cabal",["build"])]) =<< B.buildPlan B.Make ghc root (Just file)
  check "explicit Cabal compiler is preserved literally" . (==Right [("cabal",["build","--with-compiler=compiler with spaces"])]) =<< B.buildPlan B.Make (ghc {B.buildExecutable="compiler with spaces"}) root (Just file)
  check "automatic Cabal tests defer to project compiler" . (==Right [("cabal",["test","--test-show-details=direct"])]) =<< B.testPlan ghc root
  forM_ [(B.Test,"test","sample:test:check",["--test-show-details=direct"]),(B.Benchmark,"bench","sample:bench:measure",[])] $ \(action,verb,target,details)->do
    let selected=ghc {B.buildExecutable="compiler with spaces",B.buildTarget=target,B.buildArguments=["Run argument"],B.buildRuntime="Run runtime"}
    check "captured runner uses the selected GHC and literal target" .
      (==Right [("cabal",[verb,"--with-compiler=compiler with spaces"]++details++[T.unpack target])]) =<< B.buildPlan action selected root (Just file)
    check "THC runner is explicitly refused" . (\result->case result of Left reason->"THC" `T.isPrefixOf` reason; _->False) =<< B.buildPlan action thc root Nothing
  removeFile (root </> "fixture.cabal")
  forM_ [B.Test,B.Benchmark] $ \action->
    check "runner requires a Cabal project" . (\result->case result of Left _->True; _->False) =<< B.buildPlan action ghc root (Just file)
  check "path spaces and warning location" (parseBuildDiagnostic root "src/My File.hs:12:3: warning: unused name" == Just (Diagnostic (root </> "src" </> "My File.hs") Nothing 11 2 2 "warning: unused name"))
  python<-findExecutable "python3" >>= maybe (findExecutable "python") (pure . Just)
  forM_ python $ \command -> withBuildJobs $ \jobs -> do
    sourceVersion<-captureVersion (documentBuffer (buffers sourceDesktop M.! 1))
    started<-startBuildJob jobs "Make" root [(command,["-u","-c","import sys,time; print('live λ',flush=True); time.sleep(.25); print('Main.hs:2:1: error: fixture',file=sys.stderr); sys.exit(3)"])] sourceDesktop
    streaming<-await jobs started (T.isInfixOf "live λ" . output)
    check "output arrives before completion" (not (finished streaming))
    let copied=fst (runCommand Copy (fst (runCommand SelectAll streaming)))
    check "captured output keeps semantic selection/copy" ("live λ" `T.isInfixOf` clipboard copied)
    let modal=prompt "Retained draft" Information [SelectedInput "Name" "draft" (Selection 0 5)] streaming
    completedModal<-await jobs modal finished
    check "captured output refresh preserves a focused modal" (dialog completedModal==dialog modal && fmap windowId (activeWindow completedModal)==fmap windowId (activeWindow modal))
    sourceAfter<-captureVersion (documentBuffer (buffers completedModal M.! 1))
    check "captured output preserves authoritative source and Undo identity" (sourceVersion==sourceAfter && M.size (buffers completedModal)==1)
    let completed=completedModal {dialog=Nothing}
    check "failed output and Messages" ("exit 3" `T.isInfixOf` status completed && length (buildDiagnostics completed)==1 && problemsVisible completed)
    repeated<-startBuildJob jobs "Make" root [(command,["-u","-c","print('second invocation')"])] completed
    rebuilt<-await jobs repeated finished
    check "new build reuses its host slot with a fresh content lifetime" ("second invocation" `T.isInfixOf` output rebuilt && length (windows rebuilt)==length (windows completed) && fmap (\w->(windowId w,windowNumber w,bounds w)) (activeWindow rebuilt)==fmap (\w->(windowId w,windowNumber w,bounds w)) (activeWindow completed) && fmap windowContent (activeWindow rebuilt)/=fmap windowContent (activeWindow completed))

    when (os/="mingw32") $ do
      createFileLink file (root </> "diagnostic-alias.hs")
      expectedSource<-canonicalizePath file
      aliased<-startBuildJob jobs "Compile" root [(command,["-c","print('diagnostic-alias.hs:1:1: warning: alias fixture')"])] completed
      canonical<-await jobs aliased finished
      check "build diagnostics resolve source aliases before UI publication"
        (map diagnosticPath (buildDiagnostics canonical)==[expectedSource])

    running<-startBuildJob jobs "Run" root [(command,["-u","-c","import time\nwhile True: print('busy',flush=True)"])] completed
    threadDelay 100000
    stopped<-timeout 100000 $ stopBuildJob jobs running
    stopping<-maybe (error "stop request blocked the UI") pure stopped
    check "stop returns a request before joining" ("Stopping" `T.isInfixOf` status stopping)
    active<-buildJobStatus jobs stopping
    check "stop retains worker ownership until completion" (parseMaybe (withObject "status" (.: "active")) active==Just True)
    _<-await jobs stopping (T.isInfixOf "Stopped." . status)
    -- POSIX process groups remain addressable after the group leader exits.
    when (os/="mingw32") $ do
      let sideEffect=root </> "must-not-run"
          parent="import subprocess,sys; subprocess.Popen([sys.executable,'-u','-c',sys.argv[1]])"
          child="import time; print('descendant-ready',flush=True); time.sleep(60)"
      chain<-startBuildJob jobs "Make" root
        [(command,["-u","-c",parent,child]),(command,["-c","import pathlib,sys; pathlib.Path(sys.argv[1]).write_text('ran')",sideEffect])] initial
      ready<-await jobs chain (T.isInfixOf "\ndescendant-ready\n" . output)
      stoppingChain<-stopBuildJob jobs ready
      _<-await jobs stoppingChain (T.isInfixOf "Stopped." . status)
      check "Stop does not advance to the next command after draining inherited output" . not =<< doesFileExist sideEffect
    -- Keep the process active after a large burst. The UI must be able to
    -- consume prepared output and navigate while the next burst is held.
    let release=root </> "release-output"
        burst="import os,sys,time\nprint('x\\n'*600000,end='',flush=True)\nprint('Main.hs:2:1: warning: fixture\\n'*1200,end='',flush=True)\nprint('stderr-only',file=sys.stderr,flush=True)\nprint('held-ready',flush=True)\nwhile not os.path.exists(sys.argv[1]): time.sleep(.01)\nprint('final-stdout',flush=True)"
        uiTick d=do
          before<-getAllocationCounter
          fresh<-tickBuildJobs jobs d
          -- Demand the same fields rendering and diagnostic navigation use.
          forM_ (M.elems (buffers fresh)) $ \doc -> evaluate (prepareBuffer (documentBuffer doc))
          let visible=sum [contentLineCount text+sum [T.length (contentLineAt text row) | row<-[0,contentLineCount text `div` 2,contentLineCount text-1]] | prepared<-M.elems (pluginWindows fresh),let text=W.preparedWindowText prepared]
              messages=sum [length (diagnosticPath p)+T.length (diagnosticMessage p) | p<-buildDiagnostics fresh]
          _<-evaluate (visible+messages+sum (map scrollRow (windows fresh))+T.length (status fresh))
          after<-getAllocationCounter
          check "UI tick does not construct output buffers or parse diagnostics" (before-after<2*1024*1024)
          pure fresh
        awaitPrepared d done=do
          answer<-timeout 15000000 (loop d)
          maybe (error "prepared build output did not arrive") pure answer
          where loop current=do fresh<-uiTick current; if done fresh then pure fresh else threadDelay 10000 >> loop fresh
    loud<-startBuildJob jobs "Make" root [(command,["-u","-c",burst,release])] initial
    held<-awaitPrepared loud (T.isInfixOf "held-ready\n" . output)
    check "sustained output remains capped" (T.length (output held)==1024*1024)
    check "diagnostics remain capped" (length (buildDiagnostics held)==1000)
    (machine,truncated)<-buildJobStdout jobs
    check "stdout report excludes stderr and retains truncation" (truncated && T.length machine==1024*1024 && not ("stderr-only" `T.isInfixOf` machine))
    check "output follows bottom" (maybe False ((>0) . scrollRow) (activeWindow held))
    let scrolled=held {windows=map (\w -> w {scrollRow=10}) (windows held)}
    writeFile release "continue"
    final<-awaitPrepared scrolled finished
    check "completion includes final output" ("final-stdout\n\nMake completed.\n" `T.isSuffixOf` output final)
    check "output preserves a user scroll away from bottom" (all ((==10) . scrollRow) (windows final))
    (finalStdout,finalTruncated)<-buildJobStdout jobs
    check "completion publishes final stdout too" (finalTruncated && "final-stdout\n" `T.isSuffixOf` finalStdout)
    removeFile release
    closing<-startBuildJob jobs "Run" root [(command,["-u","-c","import os,sys,time; print('close-ready',flush=True)\nwhile not os.path.exists(sys.argv[1]): time.sleep(.01)\nprint('closed-final')",release])] initial
    closeReady<-await jobs closing (T.isInfixOf "close-ready\n" . output)
    let closed=fst (runCommand Close closeReady)
    writeFile release "continue"
    closedFinal<-await jobs closed finished
    check "completion does not reopen a closed output window" (null (windows closedFinal) && M.null (buffers closedFinal) && M.null (pluginWindows closedFinal))
    missing<-startBuildJob jobs "Compile" root [(root </> "absent-compiler",[])] initial
    result<-await jobs missing finished
    check "missing compiler is reported" ("Compile:" `T.isPrefixOf` status result)
  forM_ installed $ \compiler -> withBuildJobs $ \jobs -> do
    let config=ghc {B.buildExecutable=compiler}
    Right commands<-B.buildPlan B.Make config root (Just file)
    started<-startBuildJob jobs "Make" root commands initial
    built<-await jobs started finished
    check "real GHC build" (status built=="Make completed.")
    let libraryFile=root </> "Example.hs"
    writeFile libraryFile "module Example where\nanswer :: Int\nanswer = 42\n"
    Right libraryBuild<-B.buildPlan B.Make config root (Just libraryFile)
    libraryStarted<-startBuildJob jobs "Make" root libraryBuild built
    libraryBuilt<-await jobs libraryStarted finished
    check "GHC Make supports non-Main modules" (status libraryBuilt=="Make completed.")

    writeFile file "module Main where\nmain :: IO ()\nmain = nonexistentName\n"
    Right compile<-B.buildPlan B.Compile config root (Just file)
    compiling<-startBuildJob jobs "Compile" root compile libraryBuilt
    bad<-await jobs compiling finished
    check "real GHC error reaches Messages" (not (null (buildDiagnostics bad)) && "failed" `T.isInfixOf` status bad)
    cabal<-findExecutable "cabal"
    forM_ cabal $ \_ -> do
      writeFile file "module Main where\nmain :: IO ()\nmain = putStrLn \"cabal-check\"\n"
      writeFile (root </> "fixture.cabal") (unlines ["cabal-version: 3.0","name: fixture","version: 0.1.0.0","build-type: Simple","executable fixture","  main-is: Main.hs","  build-depends: base","  default-language: Haskell2010"])
      writeFile (root </> "cabal.project") "packages: .\n"
      Right projectBuild<-B.buildPlan B.Make config root (Just file)
      projectStarted<-startBuildJob jobs "Make" root projectBuild initial
      projectBuilt<-await jobs projectStarted finished
      check "real Cabal project build" (status projectBuilt=="Make completed.")
      Right projectRun<-B.buildPlan B.Run config root (Just file)
      projectRunning<-startBuildJob jobs "Run" root projectRun projectBuilt
      projectRan<-await jobs projectRunning finished
      check "real Cabal project run" (status projectRan=="Run completed." && "cabal-check" `T.isInfixOf` output projectRan)


check :: String -> Bool -> IO ()
check label ok=unless ok (error label)
temporary :: IO FilePath
temporary=do
  tmp<-getTemporaryDirectory
  (path,handle)<-openTempFile tmp "thc-build-check"
  hClose handle
  removeFile path
  createDirectory path
  canonicalizePath path
