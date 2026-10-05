{-# LANGUAGE OverloadedStrings #-}
module RuntimeMCPCheck (checks) where
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Aeson.Key as K
import System.Directory (findExecutable,getTemporaryDirectory,removeFile)
import System.IO (openTempFile,hClose)
import Hide.Recovery (writeCheckpoint,readCheckpoint)
import Hide.Conversation
import Hide.RuntimeMCP
import Hide.Terminal (terminalAvailable)
import qualified Hide.BuildJobs as Jobs
import Hide.Model
import Hide.Buffer
import qualified Hide.Plugin.Window as W
checks :: IO ()
checks=withConversation $ \runtime -> do
  let d=initialDesktop (80,25)
      call desktop name args=do (updated,finish)<-runtimeTool runtime desktop name args; result<-finish; pure (updated,result)
      check label ok=unless ok (error label)
      field key value=parseMaybe (withObject "reply" (.: K.fromText key)) value
      core desktop _=pure (False,desktop)
  (_,listing)<-call d "terminal_list" (object [])
  check "terminal capability explicitly reported" (case listing of Right value -> field "available" value==Just terminalAvailable; _ -> False)
  (_,invalid)<-call d "terminal_output" (object ["terminalId" .= ("missing"::T.Text),"limit" .= (maxBound::Int)])
  check "terminal reads bounded" (either (const True) (const False) invalid)
  let dirty=insertText "x" (addDocument Nothing (newBuffer "") d)
  (unchanged,refused)<-call dirty "build_start" (object ["action" .= ("make"::T.Text)])
  check "build refuses unsaved source without UI dialog" (unchanged==dirty && either (const True) (const False) refused)
  python<-findExecutable "python3"
  case python of
    Nothing -> pure ()
    Just executable -> do
      root<-getTemporaryDirectory
      let (_,_,jobs)=conversationServices runtime
      started<-Jobs.startBuildJob jobs "Test job" root [(executable,["-c","import sys; print('mcp λ界'); print('compiler stderr',file=sys.stderr); raise SystemExit(7)"])] d
      done<-awaitJob jobs started (100::Int)
      status<-Jobs.buildJobStatus jobs done
      check "job retains exact nonzero exit status" (field "active" status==Just False && field "exitCode" status==Just (7::Int))
      check "job output is a semantic window" (M.null (buffers done) && maybe False (T.isInfixOf "mcp λ界" . (\p->let text=W.preparedWindowText p in contentSlice text 0 (contentLength text))) (activePluginWindow done))
      ident<-maybe (error "missing captured jobId") pure (field "jobId" status :: Maybe T.Text)
      check "status exposes semantic window and output facts without fake buffer ID"
        (case status of Object fields->not (KM.member "bufferId" fields) && field "windowId" status==fmap (Just . windowId) (activeWindow done) && field "outputAvailable" status==Just True; _->False)
      (_,outputReply)<-call done "build_output" (object ["jobId" .= ident])
      outputValue<-either (error . T.unpack) pure outputReply
      let combined=fromMaybe "" (field "text" outputValue :: Maybe T.Text)
          offset=T.length (fst (T.breakOn "\nmcp λ界\n" combined))+5
      check "combined-output reader retains compiler stderr" (all (`T.isInfixOf` combined) ["mcp λ界","compiler stderr"])
      (_,characters)<-call done "build_output" (object ["jobId" .= ident,"offset" .= offset,"limit" .= (1::Int)])
      check "build output offsets count Unicode characters" ((either (const Nothing) (field "text") characters :: Maybe T.Text)==Just "λ")
      let remembered=modifyActive (\w->w {bounds=Rect 3 4 45 12,selection=Selection 2 7,scrollRow=1}) done
      (checkpoint,handle)<-openTempFile root "hide-build-output-recovery.json"
      hClose handle
      saved<-writeCheckpoint checkpoint remembered
      check "captured output declares recoverable inert content" (saved==Right ())
      recovered<-readCheckpoint checkpoint d >>= either (error . T.unpack) pure
      removeFile checkpoint
      check "real build snapshot restores text, selection and slot geometry without a source buffer"
        (M.null (buffers recovered) && fmap (\w->(bounds w,selection w,scrollRow w)) (activeWindow recovered)==Just (Rect 3 4 45 12,Selection 2 7,1) &&
          maybe False (T.isInfixOf "compiler stderr" . (\p->let text=W.preparedWindowText p in contentSlice text 0 (contentLength text))) (activePluginWindow recovered))
      recoveredRef<-maybe (error "missing recovered output") (\w->case windowContent w of PluginContent ref->pure ref; _->error "recovered output became source") (activeWindow recovered)
      check "recovered build output has no live publication" . not =<< W.windowRefCurrent recoveredRef
      Jobs.withBuildJobs $ \freshJobs->do
        inertStatus<-Jobs.buildJobStatus freshJobs recovered
        check "recovered view cannot resurrect live job status" (field "active" inertStatus==Just False && (field "jobId" inertStatus :: Maybe T.Text)==Nothing)
      let closed=fst (runCommand Close done)
      closedStatus<-Jobs.buildJobStatus jobs closed
      (_,retained)<-call closed "build_output" (object ["jobId" .= ident])
      check "closing output retains last job read without a semantic view" (field "windowId" closedStatus==Just (Nothing::Maybe Int) && either (const False) ((==Just combined) . field "text") retained)
      (_,tooLarge)<-call closed "build_output" (object ["jobId" .= ident,"limit" .= (32769::Int)])
      check "build output enforces the character bound" (either (const True) (const False) tooLarge)
      -- Capture now; extraction deliberately occurs after a new job begins.
      (_,captured)<-runtimeTool runtime closed "build_output" (object ["jobId" .= ident,"limit" .= (16::Int)])
      replacement<-Jobs.startBuildJob jobs "New job" root [(executable,["-c","print('replacement')"])] closed
      oldResult<-captured
      check "deferred extraction preserves the admitted job identity" (either (const False) ((==Just ident) . field "jobId") oldResult)
      (_,expired)<-call replacement "build_output" (object ["jobId" .= ident])
      check "a new job refuses the old requested identity" (either (const True) (const False) expired)
      _<-awaitJob jobs replacement (100::Int)
      if not terminalAvailable then pure () else do
        (terminal,response)<-call d "terminal_start" (object ["command" .= executable,"args" .= (["-u","-c","print('ready',flush=True); print(input(),flush=True)"]::[String]),"cwd" .= root])
        ident<-case response of Right value -> maybe (error "missing terminalId") pure (field "terminalId" value :: Maybe T.Text); Left err -> error (T.unpack err)
        (_,written)<-call terminal "terminal_input" (object ["terminalId" .= ident,"text" .= ("mcp input\n"::T.Text)])
        check "terminal accepts input" (either (const False) (const True) written)
        output<-awaitTerminal call terminal ident (100::Int)
        check "shared terminal produces captured output and exit" (field "exitCode" output==Just (0::Int) && maybe False (T.isInfixOf "mcp input") (field "text" output))
  _<-tickConversation runtime d
  _<-conversationEffects runtime core d []
  putStrLn "runtime MCP checks passed"
  where
    awaitJob jobs d count | count<=0=error "MCP job timed out"
                         | otherwise=do
      updated<-Jobs.tickBuildJobs jobs d
      status<-Jobs.buildJobStatus jobs updated
      if parseMaybe (withObject "job" (.: "active")) status==Just False then pure updated else threadDelay 20000 >> awaitJob jobs updated (count-1)
    awaitTerminal call d ident count | count<=0=error "MCP terminal timed out"
                                    | otherwise=do
      (_,reply)<-call d "terminal_output" (object ["terminalId" .= ident])
      case reply of
        Right value | Just (Just (_::Int))<-parseMaybe (withObject "output" (.: "exitCode")) value -> pure value
        _ -> threadDelay 20000 >> awaitTerminal call d ident (count-1)
