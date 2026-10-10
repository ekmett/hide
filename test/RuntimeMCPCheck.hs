{-# LANGUAGE OverloadedStrings #-}
module RuntimeMCPCheck (checks) where
import Control.Concurrent (threadDelay,newEmptyMVar,tryPutMVar,tryReadMVar)
import Control.Concurrent.Async (Async,withAsync,poll,wait,cancel)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Either (isLeft)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Aeson.Key as K
import System.Directory (findExecutable,getTemporaryDirectory,removeFile,createDirectory,removePathForcibly,doesFileExist)
import System.FilePath ((</>))
import System.IO (openTempFile,hClose)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import System.Timeout (timeout)
import System.Info (os)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Hide.Recovery (writeCheckpoint,readCheckpoint)
import Hide.SessionServices
import Hide.RuntimeMCP
import Hide.Terminal (terminalAvailable)
import qualified Hide.Consoles as Consoles
import Hide.MCPPermissions
import Hide.BufferReadServices (bufferReadServices)
import Hide.Plugin.Request (RequestServices(..))
import Hide.Plugin.Session (Plugin(..),PluginTool(..))
import qualified Hide.Plugin.Terminal as Terminal
import qualified Hide.Plugin.Tool as Tool
import qualified Hide.AgentUI as AgentUI
import qualified Hide.BuildJobs as Jobs
import Hide.Model
import Hide.Buffer
import qualified Hide.Plugin.Window as W
checks :: IO ()
checks=withSessionServices $ \runtime -> do
  let d=initialDesktop (80,25)
      call desktop name args=do (updated,finish)<-runtimeTool runtime desktop name args; result<-finish; pure (updated,result)
      core desktop _=pure (False,desktop)
  let edited=insertText "x" (addDocument Nothing (newBuffer "") d)
  (unchanged,refused)<-call edited "build_start" (object ["action" .= ("make"::T.Text)])
  refusedFacts<-Jobs.buildJobStatus (sessionBuildJobs runtime) unchanged
  check "build refuses unsaved source without UI dialog"
    (fmap windowId (activeWindow unchanged)==fmap windowId (activeWindow edited) &&
     fmap (revision.documentBuffer) (activeDocument unchanged)==fmap (revision.documentBuffer) (activeDocument edited) &&
     dialog unchanged==Nothing && field "active" refusedFacts==Just False && either (const True) (const False) refused)
  python<-findExecutable "python3"
  case python of
    Nothing -> pure ()
    Just executable -> do
      root<-getTemporaryDirectory
      let jobs=sessionBuildJobs runtime
      started<-Jobs.startBuildJob jobs "Test job" root [(executable,["-X","utf8","-c","import sys; sys.stdout.reconfigure(newline='\\n'); print('mcp λ界'); print('compiler stderr',file=sys.stderr); raise SystemExit(7)"])] d
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
      pure ()
  terminalChecks runtime d
  _<-tickSessionServices runtime d
  _<-sessionEffects runtime core d []
  putStrLn "runtime MCP checks passed"
  where
    awaitJob jobs d count | count<=0=error "MCP job timed out"
                         | otherwise=do
      updated<-Jobs.tickBuildJobs jobs d
      status<-Jobs.buildJobStatus jobs updated
      if parseMaybe (withObject "job" (.: "active")) status==Just False then pure updated else threadDelay 20000 >> awaitJob jobs updated (count-1)

-- Exercise the first-party declarations through their real typed request
-- capability and existing permission owner. Build tools retain their own route.
terminalChecks :: SessionServices -> Desktop -> IO ()
terminalChecks runtime base=Tool.withTools runtimeToolNames [tool | RequestTool tool<-pluginTools AgentUI.plugin] $ \tools->
  bracket temporary removePathForcibly $ \root->do
    let path=root </> "config.toml"
        specs=runtimeTools++Tool.toolDefinitions tools
        core desktop _=pure (False,desktop)
        context owner services=RequestServices (bufferReadServices (bufferReader owner (pure (Right ()))) Nothing 0)
          Nothing Nothing Nothing (Just services)
        invoke owner services=Tool.callTool tools (context owner services)
        start executable script=object ["command" .= executable,"args" .= (["-u","-c",script]::[String]),"cwd" .= root]
        makeService owner caller=terminalServices owner (sessionConsoles runtime) caller root
    check "five terminal tools come from the linked public RequestTool contribution"
      (all (Tool.hasTool tools) ["terminal_list","terminal_start","terminal_output","terminal_input","terminal_stop"] &&
       all (`notElem` runtimeToolNames) ["terminal_list","terminal_start","terminal_output","terminal_input","terminal_stop"])
    -- Only read-only tools are enabled by default; launch/input/stop prompt.
    withPermissionsAt path specs $ \owner->do
      services<-makeService owner (pure (Right ()))
      let call desktop name args=withAsync (invoke owner services name args) $ \worker->do
            updated<-ownerUntil owner desktop worker
            result<-wait worker
            pure (updated,result)
          approved desktop name args=withAsync (invoke owner services name args) $ \worker->do
            shown<-ownerApproval owner desktop
            case dialog shown of
              Just dg | PermissionDialog action<-purpose dg->do
                check "terminal approval cannot edit its exact command or payload" (not (any editable (fields dg)))
                allowed<-snd <$> policyEffects owner core shown [PermissionAction action ["0"]]
                updated<-ownerUntil owner allowed worker
                result<-wait worker
                pure (updated,result)
              _->error "missing terminal permission receipt"
      (listed,listing)<-call base "terminal_list" (object [])
      check "terminal capability explicitly reported" (either (const False) ((==Just terminalAvailable) . field "available") listing)
      invalid<-invoke owner services "terminal_output" (object ["terminalId" .= ("missing"::T.Text),"limit" .= (maxBound::Int)])
      check "public terminal page codec rejects oversized reads" (isLeft invalid)
      unknown<-invoke owner services "terminal_list" (object ["unused" .= True])
      check "public terminal codecs reject unknown arguments" (isLeft unknown)
      typedLaunch<-Terminal.terminalStart services (Terminal.TerminalLaunch "true" (replicate 1048576 "") Nothing 1)
      typedInput<-Terminal.terminalInput services (Terminal.TerminalId "missing") (T.replicate 32769 "界")
      typedPage<-Terminal.terminalOutput services (Terminal.TerminalId "missing") 0 131073
      check "typed terminal requests enforce launch-count, UTF-8 input and page bounds before admission"
        (isLeft typedLaunch && isLeft typedInput && isLeft typedPage)
      python<-findExecutable "python3"
      case python of
        Just executable | terminalAvailable->do
          -- Prompt owns authorization before this invocation's process exists.
          let marker=root </> "before-approval.pid"
          afterCancelled<-withAsync (invoke owner services "terminal_start" (start executable (processScript marker))) $ \worker->do
            shown<-ownerApproval owner listed
            exists<-doesFileExist marker
            check "waiting for launch approval has not started a process" (not exists)
            cancel worker
            retired<-tickPermissions owner shown
            check "cancelled unapproved launch withdraws only its own review" (dialog retired==Nothing)
            pure retired
          (opened,reply)<-approved afterCancelled "terminal_start" (start executable "import sys; print('ready',flush=True); print(input(),flush=True); raise SystemExit(7)")
          value<-requireTerminal reply
          ident<-maybe (error "missing public terminalId") pure (field "terminalId" value :: Maybe T.Text)
          bid<-maybe (error "missing public terminal bufferId") pure (field "bufferId" value :: Maybe Int)
          check "public launch transfers into the shared console view" (activeTerminal opened==Just ident &&
            fmap bufferId (activeWindow opened)==Just (Just bid))
          (written,accepted)<-approved opened "terminal_input" (object ["terminalId" .= ident,"text" .= ("mcp λ界\n"::T.Text)])
          check "public terminal accepts exact UTF-8 input" (either (const False) ((==Just True) . field "accepted") accepted)
          (exited,output)<-awaitOutput call written ident (\value'->field "exitCode" value'==Just (Just (7::Int)))
          check "shared terminal produces captured input/output and exact exit status"
            (maybe False (T.isInfixOf "mcp λ界") (field "text" output :: Maybe T.Text))
          let combined=fromMaybe "" (field "text" output :: Maybe T.Text)
              offset=BS.length (TE.encodeUtf8 (fst (T.breakOn "λ" combined)))
          (paged,page)<-call exited "terminal_output" (object ["terminalId" .= ident,"offset" .= offset,"limit" .= (2::Int)])
          check "terminal pages count bytes rather than Unicode characters" (either (const False) ((==Just ("λ"::T.Text)) . field "text") page)
          -- Read replies finish below an unrelated modal; only launch adoption
          -- needs the visible window slot. Carry every resulting desktop on.
          let modal=message "Current question" ["keep this view"] paged
          (readBelow,below)<-call modal "terminal_list" (object [])
          check "terminal listing finishes beneath a modal without replacing it" (isRight below && fmap purpose (dialog readBelow)==fmap purpose (dialog modal))
          (stopped,stopReply)<-approved readBelow {dialog=Nothing} "terminal_stop" (object ["terminalId" .= ident])
          check "public terminal stop retains the shared terminal identity" (isRight stopReply && activeTerminal stopped==Just ident)
          (retained,postStop)<-call stopped "terminal_output" (object ["terminalId" .= ident])
          check "public terminal stop retains output/exit" (either (const False) ((==Just (Just (7::Int))) . field "exitCode") postStop)
          _<-call retained "terminal_list" (object [])
          -- Independent request lifetimes use their own console/permission
          -- scopes, actual caller receipts and invocation-owned PID markers.
          if os=="mingw32" then pure () else do
            lifecycleChecks tools specs root executable False
            lifecycleChecks tools specs root executable True
        _->pure ()
    -- Retaining the public service cannot keep its permission session alive.
    retired<-withPermissionsAt path specs $ \owner->makeService owner (pure (Right ()))
    check "retired terminal service refuses a fresh typed call" . isLeft =<< Terminal.terminalList retired
  where
    temporary=do
      directory<-getTemporaryDirectory
      (path,h)<-openTempFile directory "hide-public-terminal-check"
      hClose h
      removeFile path
      createDirectory path
      pure path
    editable (TextArea _ True _ _ _ _)=True
    editable _=False

-- Initial admission publishes a receipt without blocking the UI owner. Finish
-- that tick, then hold further owner ticks while the real process reports its
-- PID. Cancellation/revocation therefore acts on a live, unadopted invocation,
-- not a replayed Desktop or a worker whose launch has not happened yet.
lifecycleChecks :: Tool.Tools RequestServices -> [Value] -> FilePath -> String -> Bool -> IO ()
lifecycleChecks tools specs root executable revoke=Consoles.withConsoles $ \consoles->do
  let label=if revoke then "revoked" else "cancelled"
      path=root </> (label++".toml")
      marker=root </> (label++".pid")
      base=initialDesktop (80,25)
  TIO.writeFile path "[editor.mcp.permissions]\nterminal_start = 'enable'\n"
  pid<-withPermissionsAt path specs $ \owner->do
    admitted<-newEmptyMVar
    callerLive<-newIORef True
    let caller=do
          live<-readIORef callerLive
          _<-tryPutMVar admitted ()
          pure (if live then Right () else Left "Terminal caller retired")
    services<-terminalServices owner consoles caller root
    let context=RequestServices (bufferReadServices (bufferReader owner caller) Nothing 0) Nothing Nothing Nothing (Just services)
        args=object ["command" .= executable,"args" .= (["-u","-c",processScript marker]::[String]),"cwd" .= root]
    withAsync (Tool.callTool tools context "terminal_start" args) $ \worker->do
      let untilAdmitted current=do
            receipt<-tryReadMVar admitted
            case receipt of
              Just ()->pure current
              Nothing->threadDelay 1000 >> tickPermissions owner current >>= untilAdmitted
      pending<-bounded "initial terminal admission receipt" (untilAdmitted base)
      processId<-bounded "invocation-owned terminal PID" (awaitPid marker)
      result<-poll worker
      check "prepared terminal has not replied or adopted while owner is held" (case result of Nothing->True; _->False)
      entries<-Consoles.listConsoles consoles
      check "prepared process remains outside the shared console registry" (null entries)
      if revoke then do
        writeIORef callerLive False
        refused<-ownerUntil owner pending worker
        reply<-wait worker
        check "fresh final caller check refuses adoption" (reply==Left "Terminal caller retired" && nextId refused==nextId base)
      else do
        cancel worker
        refused<-tickPermissions owner pending
        check "cancelling a prepared launch creates no console view" (nextId refused==nextId base)
      check "retired preparation never enters the shared console registry" . null =<< Consoles.listConsoles consoles
      pure processId
  -- Permission scope joins its own retirement reaper; cleanup never acquires
  -- the shared Consoles lock or relies on another test's teardown.
  (alive,_,_)<-readProcessWithExitCode "/bin/kill" ["-0",pid] ""
  check "abandoned prepared terminal process is reaped" (alive/=ExitSuccess)

processScript :: FilePath -> String
processScript path="import os; open("++show path++",'w').write(str(os.getpid())); input()"

awaitPid :: FilePath -> IO String
awaitPid path=do
  exists<-doesFileExist path
  if not exists then threadDelay 1000 >> awaitPid path else do
    value<-TIO.readFile path
    if T.null value then threadDelay 1000 >> awaitPid path else pure (T.unpack value)

ownerApproval :: Permissions -> Desktop -> IO Desktop
ownerApproval owner=bounded "terminal approval" . loop
  where
    loop current=case dialog current of
      Just dg | PermissionDialog _<-purpose dg->pure current
      _->threadDelay 1000 >> tickPermissions owner current >>= loop

ownerUntil :: Permissions -> Desktop -> Async a -> IO Desktop
ownerUntil owner desktop worker=bounded "public terminal request" (loop originalReview desktop)
  where
    originalReview=case dialog desktop of Just dg | PermissionDialog action<-purpose dg->Just action; _->Nothing
    loop allowed current=do
      result<-poll worker
      case result of
        Just _->pure current
        Nothing->case dialog current of
          Just dg | PermissionDialog action<-purpose dg,Just action/=allowed->error "terminal request asked for a second approval"
          _->do
            let continuing=case dialog current of Nothing->Nothing; _->allowed
            threadDelay 1000
            tickPermissions owner current >>= loop continuing

awaitOutput :: (Desktop -> T.Text -> Value -> IO (Desktop,Either T.Text Value)) -> Desktop -> T.Text -> (Value -> Bool) -> IO (Desktop,Value)
awaitOutput call desktop ident done=bounded "public terminal output/exit" (loop desktop)
  where
    loop current=do
      (updated,result)<-call current "terminal_output" (object ["terminalId" .= ident])
      value<-requireTerminal result
      if done value then pure (updated,value) else threadDelay 10000 >> loop updated

bounded :: String -> IO a -> IO a
bounded label action=timeout 3000000 action >>= maybe (error ("Timed out waiting for "++label)) pure

field :: FromJSON a => T.Text -> Value -> Maybe a
field key=parseMaybe (withObject "reply" (.: K.fromText key))

requireTerminal :: Either T.Text a -> IO a
requireTerminal=either (error . T.unpack) pure

isRight :: Either a b -> Bool
isRight=either (const False) (const True)

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)
