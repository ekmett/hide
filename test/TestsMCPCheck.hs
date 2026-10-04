{-# LANGUAGE OverloadedStrings #-}
module TestsMCPCheck (checks) where

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Info (os)
import System.IO (openTempFile, hClose)
import System.Timeout (timeout)
import Hide.Buffer
import qualified Hide.Build as B
import qualified Hide.BuildJobs as Jobs
import Hide.Conversation (withConversation, conversationServices)
import Hide.Files (FileState(..))
import Hide.Model
import Hide.TestsMCP

checks :: IO ()
checks = bracket temporary removePathForcibly $ \root -> do
  let ghc=B.BuildConfig B.GHC "ghc with spaces" "test:unit" "" "" []
      initial=initialDesktop (80,25)
      check label ok=unless ok (error label)
      field key=parseMaybe (withObject "value" (.: key))
      report output active code failure=let d=addReadOnly "Test output" output initial
        in testResults (object ["action" .= ("Test"::T.Text),"active" .= active,"exitCode" .= (code::Maybe Int),"error" .= (failure::Maybe T.Text),"bufferId" .= maybe (-1) sourceFixtureBuffer (activeWindow d)]) (output,False) d
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
  check "test status does not describe an unrelated build" (field "available" (testResults (object ["action" .= ("Make"::T.Text),"active" .= False]) ("",False) initial)==Just False)
  check "suite results never claim individual cases" (field "individualTestsAvailable" (report "Test suite unit: PASS\n" False (Just 0) Nothing)==Just False)
  let tap body=report ("Test suite unit: RUNNING...\nTAP version 13\n"<>body<>"Test suite unit: PASS\n") False (Just 0) Nothing
      cases value=fromMaybe [] (field "tests" value :: Maybe [Value])
      caseStates :: Value -> [T.Text]
      caseStates=mapMaybe (field "status") . cases
      streamStates value=mapMaybe (field "state") (fromMaybe [] (field "tapStreams" value :: Maybe [Value])) :: [T.Text]
  check "TAP yields explicit named individual cases" (caseStates (tap "1..2\nok 1 - addition\nnot ok 2 - subtraction\n")==["passed","failed"] && state "TAP version 13\n1..1\nnot ok 1\n" False (Just 0) Nothing==Just "failed")
  check "TAP skip, todo and unexpected success remain distinct" (caseStates (tap "1..3\nok 1 # SKIP unavailable\nnot ok 2 # TODO missing\nok 3 # TODO fixed\n")==["skipped","todo","unexpectedPass"])
  check "TAP validates trailing plan and inferred numbers" (streamStates (tap "ok - one\nok - two\n1..2\n")==["complete"])
  check "TAP truncated or missing plan cannot prove complete results" (streamStates (tap "1..2\nok 1\n")==["incomplete"] && streamStates (tap "ok 1\n")==["incomplete"])
  check "TAP duplicate numbers and misplaced plans are invalid" (streamStates (tap "1..2\nok 1\nok 1\n")==["invalid"] && streamStates (tap "ok 1\n1..2\nok 2\n")==["invalid"])
  check "TAP bailout remains failure despite successful process" (streamStates (tap "1..1\nBail out! unavailable\n")==["bailedOut"] && field "state" (tap "1..1\nBail out! unavailable\n")==Just ("failed"::T.Text))
  check "ordinary ok output is not guessed to be TAP" (null (cases (report "ok 1\nnot ok 2\n" False (Just 0) Nothing)))
  check "TAP child/YAML indentation is not a top-level case" (caseStates (tap "1..1\n    not ok 1 - child\nnot okay\nok 1 - parent\n  ---\n  note: not ok\n  ...\n")==["passed"])
  check "TAP requires exact point keyword boundary" (null (cases (tap "1..0 # SKIP none\nokay\n")))
  check "TAP consecutive streams retain identity" (mapMaybe (field "stream") (cases (tap "1..1\nok 1\nTAP version 13\n1..1\nok 1\n"))==([1,2]::[Int]))
  check "TAP reports bounded case retention" (length (cases (tap ("1..501\n"<>T.concat ["ok "<>T.pack (show n)<>"\n" | n<-[1::Int ..501]])))==500 && field "testResultsTruncated" (tap ("1..501\n"<>T.concat ["ok "<>T.pack (show n)<>"\n" | n<-[1::Int ..501]]))==Just True)
  check "TAP failures beyond retained cases still fail the run" (field "state" (tap ("1..501\n"<>T.concat ["ok "<>T.pack (show n)<>"\n" | n<-[1::Int ..500]]<>"not ok 501\n"))==Just ("failed"::T.Text))
  check "TAP handles CRLF and closes at Cabal suite boundary" (caseStates (report "TAP version 13\r\n1..1\r\nok 1\r\nTest suite first: PASS\r\nok 2 - not TAP\r\n" False (Just 0) Nothing)==["passed"])
  check "failed suite wins over passing TAP" (state "TAP version 13\n1..1\nok 1\nTest suite unit: FAIL\n" False (Just 0) Nothing==Just "failed")
  check "incomplete stream beyond retained streams cannot pass" (state (T.replicate 500 "TAP version 13\n1..1\nok 1\n"<>"TAP version 13\n1..1\nTest suite unit: PASS\n") False (Just 0) Nothing==Just "incomplete")
  check "indented suite-like YAML cannot close TAP" (state "TAP version 13\n1..1\nok 1\n  Test suite fake: PASS\nnot ok 2\n" False (Just 0) Nothing==Just "failed")
  check "TAP punctuation is a point boundary" (state "TAP version 13\n1..1\nok 1\nnot ok# failure\n" False (Just 0) Nothing==Just "failed")
  check "oversized TAP numbers cannot wrap" (state "TAP version 13\n1..18446744073709551617\nok 1\n" False (Just 0) Nothing==Just "failed" && state "TAP version 13\n1..1\nok 18446744073709551617\n" False (Just 0) Nothing==Just "failed")
  check "indented suite-like text is not suite evidence" (null (parseTestSuites "  Test suite fake: PASS\n"))
  check "partial streaming TAP line is not a completed case" (caseStates (report "TAP version 13\n1..1\nok 1 - unfinished" True Nothing Nothing)==[])
  let truncatedDesktop=addReadOnly "Test output" "TAP version 13\n1..1\nok 1\n" initial
      truncatedJob=object ["action" .= ("Test"::T.Text),"active" .= False,"exitCode" .= (0::Int)]
  check "truncated stdout never proves a complete passing run" (field "state" (testResults truncatedJob ("TAP version 13\n1..1\nok 1\n",True) truncatedDesktop)==Just ("incomplete"::T.Text))
  check "passing TAP cannot finish an unfinished Cabal suite" (state "Test suite pending: RUNNING...\nTAP version 13\n1..1\nok 1\n" False (Just 0) Nothing==Just "incomplete")
  check "passing TAP cannot hide capped suite results" (state ("TAP version 13\n1..1\nok 1\n"<>T.concat ["Test suite unit"<>T.pack (show n)<>": PASS\n" | n<-[1::Int ..501]]) False (Just 0) Nothing==Just "incomplete")
  when (os/="mingw32") $ do
    let server=root </> "cabal"
        source=root </> "Main.hs"
        desktop=addDocument (Just (FileState source Nothing)) (newBuffer "main = pure ()\n") initial {defaultDirectory=Just root}
        args=object ["toolchain" .= ("GHC"::T.Text),"target" .= ("test:unit"::T.Text)]
        fake=unlines ["#!/usr/bin/env python3","import json,pathlib,sys,time","pathlib.Path('argv.json').write_text(json.dumps(sys.argv[1:]))","print('Test suite unit: RUNNING...',flush=True)","print('TAP version 13\\n1..1\\nok 1 - actual stdout',flush=True)","print('TAP version 13\\n1..1\\nnot ok 1 - stderr is not TAP',file=sys.stderr,flush=True)","time.sleep(.15)","print('Main.hs:1:1: error: fixture failure',flush=True)","print('Test suite unit: FAIL',flush=True)","sys.exit(7)"]
    writeFile source "main = pure ()\n"
    writeFile server fake
    permissions<-getPermissions server
    setPermissions server permissions {executable=True}
    bracket (lookupEnv "PATH") (maybe (unsetEnv "PATH") (setEnv "PATH")) $ \oldPath -> do
      setEnv "PATH" (root++":"++maybe "" id oldPath)
      withConversation $ \runtime -> do
        let (_,_,jobs)=conversationServices runtime
            call d name fields=do (next,answer)<-testsTool runtime d name fields; result<-answer; pure (next,result)
        (_,dirtyResult)<-call (insertText "x" desktop) "test_start" args
        check "tests reject unsaved source" (either (T.isInfixOf "Save modified") (const False) dirtyResult)
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
        check "only stdout produces individual test points" (either (const False) (\value -> caseStates value==["passed"]) result)
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
