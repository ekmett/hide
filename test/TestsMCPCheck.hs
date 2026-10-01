{-# LANGUAGE OverloadedStrings #-}
module TestsMCPCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Info (os)
import System.IO (openTempFile, hClose)
import System.Timeout (timeout)
import THC.Edit.Buffer
import qualified THC.Edit.Build as B
import qualified THC.Edit.BuildJobs as Jobs
import THC.Edit.Conversation (withConversation, conversationServices)
import THC.Edit.Files (FileState(..))
import THC.Edit.Model
import THC.Edit.TestsMCP

checks :: IO ()
checks = bracket temporary removePathForcibly $ \root -> do
  let ghc=B.BuildConfig B.GHC "ghc with spaces" "test:unit" "" "" []
      initial=initialDesktop (80,25)
      check label ok=unless ok (error label)
      field key=parseMaybe (withObject "value" (.: key))
      report output active code failure=let d=addReadOnly "Test output" output initial
        in testResults (object ["action" .= ("Test"::T.Text),"active" .= active,"exitCode" .= (code::Maybe Int),"error" .= (failure::Maybe T.Text),"bufferId" .= maybe (-1) bufferId (activeWindow d)]) d
      state output active code failure=field "state" (report output active code failure) :: Maybe T.Text
  noProject<-B.testPlan ghc root
  check "tests reject standalone files" (either (const True) (const False) noProject)
  writeFile (root </> "fixture.cabal") "name: fixture\nversion: 0.1\n"
  planned<-B.testPlan ghc root
  check "Cabal tests retain selected compiler and target as argv" (planned==Right [("cabal",["test","--with-compiler=ghc with spaces","--test-show-details=direct","test:unit"])])
  noTHC<-B.testPlan ghc {B.buildToolchain=B.THC} root
  check "THC compilation is never misrepresented as tests" (either (T.isInfixOf "no configured test runner") (const False) noTHC)
  badTarget<-B.testPlan ghc {B.buildTarget="--evil"} root
  check "test targets cannot inject options" (either (const True) (const False) badTarget)
  check "suite parser follows explicit final outcomes" (parseTestSuites "Test suite unit: RUNNING...\nTest suite unit: PASS\nTest suite integration: FAIL\nnot a suite: PASS\n"==[("integration","failed"),("unit","passed")])
  check "successful explicit suites pass" (state "Test suite unit: PASS\n" False (Just 0) Nothing==Just "passed")
  check "nonzero exit wins over earlier suite successes" (state "Test suite unit: PASS\n" False (Just 1) Nothing==Just "failed")
  check "no diagnostics does not fabricate passed tests" (state "ordinary compiler output\n" False (Just 0) Nothing==Just "completed")
  check "active job remains running despite partial pass output" (state "Test suite unit: PASS\n" True Nothing Nothing==Just "running")
  check "stopped jobs cannot be reported as passed" (state "Test suite unit: PASS\n" False Nothing (Just "Stopped.")==Just "stopped")
  check "test status does not describe an unrelated build" (field "available" (testResults (object ["action" .= ("Make"::T.Text),"active" .= False]) initial)==Just False)
  check "suite results never claim individual cases" (field "individualTestsAvailable" (report "Test suite unit: PASS\n" False (Just 0) Nothing)==Just False)
  when (os/="mingw32") $ do
    let server=root </> "cabal"
        source=root </> "Main.hs"
        desktop=addDocument (Just (FileState source Nothing)) (newBuffer "main = pure ()\n") initial {defaultDirectory=Just root}
        args=object ["toolchain" .= ("GHC"::T.Text),"target" .= ("test:unit"::T.Text)]
        fake=unlines ["#!/usr/bin/env python3","import json,pathlib,sys,time","pathlib.Path('argv.json').write_text(json.dumps(sys.argv[1:]))","print('Test suite unit: RUNNING...',flush=True)","time.sleep(.15)","print('Main.hs:1:1: error: fixture failure',flush=True)","print('Test suite unit: FAIL',flush=True)","sys.exit(7)"]
    writeFile source "main = pure ()\n"
    writeFile server fake
    permissions<-getPermissions server
    setPermissions server permissions {executable=True}
    bracket (lookupEnv "PATH") (maybe (unsetEnv "PATH") (setEnv "PATH")) $ \oldPath -> do
      setEnv "PATH" (root++":"++maybe "" id oldPath)
      withConversation $ \runtime -> do
        let (_,_,jobs)=conversationServices runtime
            call d name fields=do (next,answer)<-testsTool runtime d name fields; result<-answer; pure (next,result)
        (_,dirty)<-call (insertText "x" desktop) "test_start" args
        check "tests reject unsaved source" (either (T.isInfixOf "Save modified") (const False) dirty)
        (started,began)<-call desktop "test_start" args
        check "test_start returns before completion" (either (const False) ((==Just ("running"::T.Text)).field "state") began)
        (_,duplicate)<-call started "test_start" args
        check "tests share the build exclusion gate" (either (T.isInfixOf "already active") (const False) duplicate)
        let pump d=do
              next<-Jobs.tickBuildJobs jobs d
              (_,reply)<-call next "test_status" (object [])
              if either (const False) ((==Just ("failed"::T.Text)).field "state") reply then pure (next,reply)
              else threadDelay 1000 >> pump next
        finished<-timeout 3000000 (pump started)
        (after,result)<-maybe (error "captured test did not finish") pure finished
        check "test failure is structured with exact exit code" (either (const False) (\value -> (field "job" value >>= field "exitCode")==Just (7::Int)) result)
        check "test compiler errors reach Messages" (not (null (buildDiagnostics after)))
        check "captured tests retain a readable output buffer" (any (\doc -> "Test suite unit: FAIL" `T.isInfixOf` contents (documentBuffer doc)) (M.elems (buffers after)))
  putStrLn "structured test tools checks passed"
  where
    temporary=do
      base<-getTemporaryDirectory
      (path,h)<-openTempFile base "thc-test-tools"
      hClose h; removeFile path; createDirectory path
      canonicalizePath path
