{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.TestsMCP (testsTools, testsToolNames, testsTool, testResults, parseTestSuites) where

import Data.Aeson
import Data.Aeson.Types (parseEither, parseMaybe)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import THC.Edit.Buffer
import qualified THC.Edit.Build as B
import qualified THC.Edit.BuildJobs as Jobs
import THC.Edit.Conversation (ConversationState, conversationServices)
import THC.Edit.Model

testsToolNames :: [T.Text]
testsToolNames=["test_start","test_status"]

testsTools :: [Value]
testsTools=
  [object ["name" .= ("test_start"::T.Text),"description" .= ("Run Cabal tests as a shared captured job; source buffers must be saved. Uses selected GHC settings; THC has no configured test runner. Optional target overrides the configured Cabal target, for example test:unit or all. Use build_stop to stop; test_status reports suites and process exit, not invented individual test results."::T.Text),
    "inputSchema" .= object ["type" .= ("object"::T.Text),"properties" .= object ["target" .= object ["type" .= ("string"::T.Text)],"toolchain" .= object ["type" .= ("string"::T.Text),"enum" .= (["GHC"]::[T.Text])]],"additionalProperties" .= False],
    "annotations" .= object ["readOnlyHint" .= False,"destructiveHint" .= True,"openWorldHint" .= True]],
   object ["name" .= ("test_status"::T.Text),"description" .= ("Read the latest shared job when it is a test run: Cabal suite statuses, captured output buffer ID, compiler diagnostic count and authoritative exit code. Starting another build replaces this report; prior output buffers remain readable. No individual test cases are inferred."::T.Text),
    "inputSchema" .= object ["type" .= ("object"::T.Text),"properties" .= object [],"additionalProperties" .= False],
    "annotations" .= object ["readOnlyHint" .= True,"destructiveHint" .= False,"openWorldHint" .= False]]]

testsTool :: ConversationState -> Desktop -> T.Text -> Value -> IO (Desktop,IO (Either T.Text Value))
testsTool runtime d name arguments = case parseEither (withObject "test arguments" pure) arguments of
  Left err -> done d (Left (T.pack err))
  Right fields -> case name of
    "test_status" -> Jobs.buildJobStatus jobs >>= done d . Right . (`testResults` d)
    "test_start" -> case parseEither (\_ -> (,) <$> fields .:? "target" <*> fields .:? "toolchain") arguments of
      Left err -> done d (Left (T.pack err))
      Right (target,toolchain) -> do
        current<-Jobs.buildJobStatus jobs
        if field "active" current==Just True then done d (Left "A build, run or test is already active; use build_stop first.")
        else if any (\doc -> documentLabel doc==Nothing && dirty (documentBuffer doc)) (M.elems (buffers d)) then done d (Left "Save modified source buffers before testing files on disk.")
        else do
          root<-B.resolveBuildRoot d
          config<-B.loadBuildConfig directory root
          case toolchain :: Maybe T.Text of
            Just value | value/="GHC" -> done d (Left "Tests support the GHC/Cabal runner only.")
            _ -> do
              let compiler=if toolchain==Just "GHC" then B.GHC else B.buildToolchain config
                  selected=config {B.buildToolchain=compiler,B.buildExecutable=if compiler==B.buildToolchain config then B.buildExecutable config else "ghc",B.buildTarget=fromMaybe (B.buildTarget config) target}
              plan<-B.testPlan selected root
              case plan of
                Left err -> done d (Left err)
                Right commands -> do
                  started<-Jobs.startBuildJob jobs "Test" root commands d
                  Jobs.buildJobStatus jobs >>= done started . Right . (`testResults` started)
    _ -> done d (Left "Unknown test tool")
  where
    (directory,_,jobs)=conversationServices runtime
    done desktop result=pure (desktop,pure result)

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "job" (.: key))

-- Cabal's suite outcomes are explicit evidence. Framework-specific case names
-- are intentionally not guessed from free-form console output.
parseTestSuites :: T.Text -> [(T.Text,T.Text)]
parseTestSuites text=take 500 (M.toList (M.fromList [entry | line<-T.lines text,Just entry<-[parse (T.strip line)]]))
  where
    parse line=do
      body<-T.stripPrefix "Test suite " line
      let (prefix,state)=T.breakOnEnd ": " body
          name=T.dropEnd 2 prefix
      if T.null name || T.length name>256 then Nothing else
        (\result -> (name,result)) <$> lookup state [("RUNNING...","running"),("PASS","passed"),("FAIL","failed")]

testResults :: Value -> Desktop -> Value
testResults job d
  | field "action" job/=Just ("Test"::T.Text) = object ["available" .= False,"job" .= job]
  | otherwise = object
      ["available" .= True,"job" .= job,"state" .= state,"resultGranularity" .= ("suite"::T.Text),
       "suites" .= [object ["name" .= name,"status" .= outcome] | (name,outcome)<-suites],
       "individualTestsAvailable" .= False,"outputAvailable" .= maybe False (const True) document,
       "outputTruncated" .= truncated,"suiteResultsTruncated" .= (length suites>=500),"compilerDiagnosticCount" .= length (buildDiagnostics d)]
  where
    document=field "bufferId" job >>= (`M.lookup` buffers d)
    output=maybe "" (contents . documentBuffer) document
    truncated=T.length output>=1024*1024
    suites=parseTestSuites output
    state :: T.Text
    state | field "active" job==Just True = "running"
          | field "error" job==Just ("Stopped."::T.Text) = "stopped"
          | Just _<-(field "error" job :: Maybe T.Text) = "error"
          | Just code<-field "exitCode" job, code/=(0::Int) = "failed"
          | field "exitCode" job==Just (0::Int), not (null suites), all ((=="passed").snd) suites, not truncated, length suites<500 = "passed"
          | field "exitCode" job==Just (0::Int) = "completed"
          | otherwise = "unknown"
