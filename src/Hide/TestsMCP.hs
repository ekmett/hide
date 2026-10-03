{-# LANGUAGE OverloadedStrings #-}
-- | Structured test status derived from the shared captured build job.
--
-- Cabal suite outcomes and explicit TAP 13 streams provide evidence; arbitrary
-- console prose is not treated as individual tests. Truncation and incomplete TAP
-- plans remain visible. The latest non-test job replaces the status report, while
-- its predecessor's output document can remain open.
module Hide.TestsMCP (testsTools, testsToolNames, testsTool, testResults, parseTestSuites) where

import Data.Aeson
import Data.Aeson.Types (parseEither, parseMaybe)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Char (isDigit, isSpace, isAlphaNum)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Hide.Buffer
import qualified Hide.Build as B
import qualified Hide.BuildJobs as Jobs
import Hide.Conversation (ConversationState, conversationServices)
import Hide.Model

testsToolNames :: [T.Text]
testsToolNames=["test_start","test_status"]

testsTools :: [Value]
testsTools=
  [object ["name" .= ("test_start"::T.Text),"description" .= ("Run Cabal tests as a shared captured job; source buffers must be saved. Uses selected GHC settings; THC has no configured test runner. Optional target overrides the configured Cabal target, for example test:unit or all. Use build_stop to stop; test_status reports suites, explicit TAP 13 test points, and process exit."::T.Text),
    "inputSchema" .= object ["type" .= ("object"::T.Text),"properties" .= object ["target" .= object ["type" .= ("string"::T.Text)],"toolchain" .= object ["type" .= ("string"::T.Text),"enum" .= (["GHC"]::[T.Text])]],"additionalProperties" .= False],
    "annotations" .= object ["readOnlyHint" .= False,"destructiveHint" .= True,"openWorldHint" .= True]],
   object ["name" .= ("test_status"::T.Text),"description" .= ("Read the latest shared job when it is a test run: Cabal suite statuses, captured output buffer ID, compiler diagnostic count and authoritative exit code. Starting another build replaces this report; prior output buffers remain readable. TAP 13 on stdout supplies individual test points; incomplete streams are identified."::T.Text),
    "inputSchema" .= object ["type" .= ("object"::T.Text),"properties" .= object [],"additionalProperties" .= False],
    "annotations" .= object ["readOnlyHint" .= True,"destructiveHint" .= False,"openWorldHint" .= False]]]

-- | Start a saved-source GHC/Cabal test job or read the latest shared test status.
testsTool :: ConversationState -> Desktop -> T.Text -> Value -> IO (Desktop,IO (Either T.Text Value))
testsTool runtime d name arguments = case parseEither (withObject "test arguments" pure) arguments of
  Left err -> done d (Left (T.pack err))
  Right fields -> case name of
    "test_status" -> results d
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
                  results started
    _ -> done d (Left "Unknown test tool")
  where
    (directory,_,jobs)=conversationServices runtime
    done desktop result=pure (desktop,pure result)
    results desktop=do
      job<-Jobs.buildJobStatus jobs
      output<-Jobs.buildJobStdout jobs
      done desktop (Right (testResults job output desktop))

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "job" (.: key))

-- | Extract up to 500 final-by-name Cabal suite states from explicit wrapper lines.
parseTestSuites :: T.Text -> [(T.Text,T.Text)]
parseTestSuites text=take 500 (M.toList (M.fromList [entry | line<-T.lines text,Just entry<-[parse (T.dropWhileEnd (=='\r') line)]]))
  where
    parse line=do
      body<-T.stripPrefix "Test suite " line
      let (prefix,state)=T.breakOnEnd ": " body
          name=T.dropEnd 2 prefix
      if T.null name || T.length name>256 then Nothing else
        (\result -> (name,result)) <$> lookup state [("RUNNING...","running"),("PASS","passed"),("FAIL","failed")]

-- | Combine job lifecycle, retained stdout and diagnostics into a bounded report.
-- Process exit and incomplete/truncated evidence are retained separately.
testResults :: Value -> (T.Text,Bool) -> Desktop -> Value
testResults job (stdoutText,stdoutTruncated) d
  | field "action" job/=Just ("Test"::T.Text) = object ["available" .= False,"job" .= job]
  | otherwise = object
      ["available" .= True,"job" .= job,"state" .= state,"resultGranularity" .= (if null streams then "suite" else "test"::T.Text),
       "suites" .= [object ["name" .= name,"status" .= outcome] | (name,outcome)<-suites],
       "individualTestsAvailable" .= not (null streams),
       "tests" .= take 500 [object ["stream" .= index,"number" .= number,"name" .= name,"status" .= outcome,"reason" .= reason]
           | (index,stream)<-zip [1::Int ..] streams,(number,name,outcome,reason)<-reverse (tapCases stream)],
       "tapStreams" .= [object ["stream" .= index,"planned" .= tapPlan stream,"observed" .= tapCount stream,
           "state" .= tapState stream,"reason" .= tapBailout stream] | (index,stream)<-zip [1::Int ..] streams],
       "testResultsTruncated" .= (stdoutTruncated || length allStreams>500 || sum (map tapCount allStreams)>500),"outputAvailable" .= maybe False (const True) document,
       "outputTruncated" .= truncated,"stdoutTruncated" .= stdoutTruncated,"suiteResultsTruncated" .= (length suites>=500),"compilerDiagnosticCount" .= length (buildDiagnostics d)]
  where
    document=field "bufferId" job >>= (`M.lookup` buffers d)
    output=maybe "" (contents . documentBuffer) document
    truncated=T.length output>=1024*1024
    suites=parseTestSuites stdoutText
    streams=take 500 allStreams
    allStreams=parseTAP (if field "active" job==Just True then fst (T.breakOnEnd "\n" stdoutText) else stdoutText)
    tapState stream
      | tapBailout stream/=Nothing = "bailedOut"
      | tapInvalid stream = "invalid"
      | stdoutTruncated || tapPlan stream/=Just (tapCount stream) = "incomplete"
      | otherwise = "complete" :: T.Text
    tapFailed stream=tapInvalid stream || tapBailout stream/=Nothing || tapFailure stream
    state :: T.Text
    state | field "active" job==Just True = "running"
          | field "error" job==Just ("Stopped."::T.Text) = "stopped"
          | Just _<-(field "error" job :: Maybe T.Text) = "error"
          | Just code<-field "exitCode" job, code/=(0::Int) = "failed"
          | any ((=="failed").snd) suites || any tapFailed allStreams = "failed"
          | not (null streams),any ((/="complete").tapState) allStreams = "incomplete"
          | length suites>=500 || any ((=="running").snd) suites = "incomplete"
          | field "exitCode" job==Just (0::Int),not (null streams),all ((=="complete").tapState) allStreams,not stdoutTruncated = "passed"
          | field "exitCode" job==Just (0::Int), not (null suites), all ((=="passed").snd) suites, not stdoutTruncated, length suites<500 = "passed"
          | field "exitCode" job==Just (0::Int) = "completed"
          | otherwise = "unknown"

-- Only a versioned stream admits test points. Cabal's wrapper lines and
-- indented subtests/YAML stay out of the top-level result set. Keep counts after
-- the retention limit so truncation cannot masquerade as a complete small run.
data TAP = TAP
  { tapCases :: [(Int,T.Text,T.Text,Maybe T.Text)], tapCount :: Int
  , tapPlan :: Maybe Int, tapTrailingPlan :: Bool, tapInvalid :: Bool
  , tapBailout :: Maybe T.Text, tapFailure :: Bool, tapClosed :: Bool }

parseTAP :: T.Text -> [TAP]
parseTAP text=reverse (foldl' step [] (map (T.dropWhileEnd (=='\r')) (T.lines text)))
  where
    step streams "TAP version 13"=TAP [] 0 Nothing False False Nothing False False:streams
    step [] _=[]
    step (stream:rest) raw=update stream raw:rest
    update stream line
      | tapClosed stream = stream
      | "Test suite " `T.isPrefixOf` line,not (null (parseTestSuites line)) = stream {tapClosed=True}
      | Just reason<-T.stripPrefix "Bail out!" line = stream {tapBailout=Just (T.take 512 (T.strip reason))}
      | tapBailout stream/=Nothing = stream
      | Just plan<-T.stripPrefix "1.." line =
          let (digits,suffix)=T.span isDigit plan
              count=natural digits
              valid=maybe False (>=0) count && (T.null (T.strip suffix) || "#" `T.isPrefixOf` T.stripStart suffix)
          in stream {tapPlan=count,tapTrailingPlan=tapCount stream>0,tapInvalid=tapInvalid stream || tapPlan stream/=Nothing || not valid}
      | Just (ok,body)<-point line =
          let (digits,afterNumber)=T.span isDigit (T.stripStart body)
              number=if T.null digits then Just (tapCount stream+1) else natural digits
              (name,annotation)=T.breakOn "#" (T.strip (if T.null digits then body else afterNumber))
              (directive,explanation)=T.break isSpace (T.stripStart (T.drop 1 annotation))
              tag=T.toUpper directive
              outcome | tag=="SKIP" = "skipped"
                      | tag=="TODO" = if ok then "unexpectedPass" else "todo"
                      | ok = "passed"
                      | otherwise = "failed"
              entry=(fromMaybe (tapCount stream+1) number,T.take 512 (T.strip (fromMaybe name (T.stripPrefix "- " name))),outcome,
                if tag `elem` ["SKIP","TODO"] then Just (T.take 512 (T.strip explanation)) else Nothing)
          in stream {tapCount=tapCount stream+1,tapCases=if tapCount stream<500 then entry:tapCases stream else tapCases stream,
               tapInvalid=tapInvalid stream || tapTrailingPlan stream || number/=Just (tapCount stream+1),tapFailure=tapFailure stream || outcome=="failed"}
      | otherwise=stream
    point line=case keyword "not ok" line of
      Just body->Just (False,body)
      Nothing->(True,) <$> keyword "ok" line
    keyword word line=do
      rest<-T.stripPrefix word line
      if T.null rest || maybe False ((\c -> not (isAlphaNum c || c=='_')).fst) (T.uncons rest) then Just rest else Nothing
    natural digits | T.length digits>20=Nothing
    natural digits=do
      value<-readMaybe (T.unpack digits) :: Maybe Integer
      if value>=0 && value<=toInteger (maxBound::Int) then Just (fromInteger value) else Nothing
