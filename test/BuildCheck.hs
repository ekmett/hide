{-# LANGUAGE OverloadedStrings #-}
module BuildCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless, forM_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.Directory
import System.FilePath ((</>))
import System.IO (openTempFile, hClose)
import System.Timeout (timeout)
import qualified THC.Edit.Build as B
import THC.Edit.BuildJobs
import THC.Edit.Files (FileState(..))
import THC.Edit.Buffer
import THC.Edit.Model

checks :: IO ()
checks = bracket temporary removePathForcibly $ \root -> do
  let initial=initialDesktop (80,25)
      ghc=B.BuildConfig B.GHC "ghc" "" "" "" []
      thc=B.BuildConfig B.THC "thc with spaces" "exe:hello world;literal" "compiler root" "runtime path" ["one two"]
      file=root </> "Main.hs"
      output d=T.concat [contents (documentBuffer doc) | doc<-M.elems (buffers d),documentLabel doc/=Nothing]
      await jobs desktop done=do
        answer<-timeout 15000000 (loop desktop)
        maybe (error "build worker did not finish") pure answer
        where loop d=do fresh<-tickBuildJobs jobs d; if done fresh then pure fresh else threadDelay 10000 >> loop fresh
      finished d=any (`T.isInfixOf` status d) ["completed.","failed (exit", "does not exist", "No such file"]
  check "F9 starts Make" (snd (handleEvent (V.EvKey (V.KFun 9) []) initial)==[AgentAction "make" []])
  check "Alt-F9 starts Compile" (snd (handleEvent (V.EvKey (V.KFun 9) [V.MAlt]) initial)==[AgentAction "compile" []])
  check "THC target retains literal argv" . (==Right [("thc with spaces",["run","exe:hello world;literal","--project-dir",root,"--thc-root","compiler root","--runtime","runtime path","--","one two"])]) =<< B.buildPlan B.Run thc root Nothing
  check "reject option-like target" (case B.parseBuildConfig ["thc","--help","","", "[]", "0"] of Left _ -> True; _ -> False)
  check "reject invalid program arguments" (case B.parseBuildConfig ["ghc","","","", "not JSON", "1"] of Left _ -> True; _ -> False)
  check "switch default compiler with toolchain" (fmap B.buildExecutable (B.parseBuildConfig ["thc","","","", "[]", "1"])==Right "ghc")
  writeFile file "module Main where\nmain :: IO ()\nmain = putStrLn \"build-check\"\n"
  let sourceDesktop=addDocument (Just (FileState file Nothing)) (newBuffer "main = pure ()") initial
      outputFocused=addReadOnly "Make output" "log" sourceDesktop
  check "output focus keeps standalone source" (B.buildSource outputFocused==Just file)
  check "output focus keeps project directory without Files" . (==root) =<< B.resolveBuildRoot outputFocused

  check "standalone compile checks source" . (==Right [("ghc",["--make","-fno-code","-fdiagnostics-color=never",file])]) =<< B.buildPlan B.Compile ghc root (Just file)
  writeFile (root </> "fixture.cabal") "name: fixture\n"
  check "Cabal project builds with selected compiler" . (==Right [("cabal",["build","--with-compiler=ghc"])]) =<< B.buildPlan B.Make ghc root (Just file)
  removeFile (root </> "fixture.cabal")
  check "path spaces and warning location" (parseBuildDiagnostic root "src/My File.hs:12:3: warning: unused name" == Just (Diagnostic (root </> "src/My File.hs") Nothing 11 2 2 "warning: unused name"))
  python<-findExecutable "python3" >>= maybe (findExecutable "python") (pure . Just)
  forM_ python $ \command -> withBuildJobs $ \jobs -> do
    started<-startBuildJob jobs "Make" root [(command,["-u","-c","import sys,time; print('live λ',flush=True); time.sleep(.25); print('Main.hs:2:1: error: fixture',file=sys.stderr); sys.exit(3)"])] initial
    streaming<-await jobs started (T.isInfixOf "live λ" . output)
    check "output arrives before completion" (not (finished streaming))
    completed<-await jobs streaming finished
    check "failed output and Messages" ("exit 3" `T.isInfixOf` status completed && length (buildDiagnostics completed)==1 && problemsVisible completed)
    repeated<-startBuildJob jobs "Make" root [(command,["-u","-c","print('second invocation')"])] completed
    rebuilt<-await jobs repeated finished
    check "repeated build refreshes existing output window" ("second invocation" `T.isInfixOf` output rebuilt)

    stopped<-timeout 3000000 $ do
      running<-startBuildJob jobs "Run" root [(command,["-u","-c","import time\nwhile True: print('busy',flush=True)"])] completed
      threadDelay 100000
      stopBuildJob jobs running
    check "stop cannot deadlock with full output queue" (maybe False (T.isInfixOf "Stopped." . status) stopped)
    missing<-startBuildJob jobs "Compile" root [(root </> "absent-compiler",[])] initial
    result<-await jobs missing finished
    check "missing compiler is reported" ("Compile:" `T.isPrefixOf` status result)
  installed<-findExecutable "ghc"
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

    Right run<-B.buildPlan B.Run config root (Just file)
    running<-startBuildJob jobs "Run" root run built
    ran<-await jobs running finished
    check "real GHC run" (status ran=="Run completed." && "build-check" `T.isInfixOf` output ran)
    writeFile file "module Main where\nmain :: IO ()\nmain = nonexistentName\n"
    Right compile<-B.buildPlan B.Compile config root (Just file)
    compiling<-startBuildJob jobs "Compile" root compile ran
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
