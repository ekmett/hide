{-# LANGUAGE OverloadedStrings #-}
module DebuggerCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, poll, wait)
import Control.Exception (bracket)
import Control.Monad (unless, when, forM_)
import Data.Aeson
import Data.IORef
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.Socket as Socket
import System.Directory
import System.Exit (ExitCode(..))
import System.IO
import System.Info (os)
import System.Process
import System.Timeout (timeout)
import THC.Edit.Buffer
import THC.Edit.Debugger
import THC.Edit.Files (FileState(..))
import THC.Edit.Model
import THC.Edit.Render (snapshotHtml)

checks :: IO ()
checks = presentationCheck >> pendingPresentationCheck >> startupDeadlineCheck >> launchDeadlineCheck >> launchChecks >> mapM_ session ["basic", "frame", "choices", "breakpoints", "reconnect", "mcp", "lazy"] >> putStrLn "Debugger checks passed"
  where
    session mode = bracket (fixture mode) cleanup $ \(port,logPath,process) -> withDebugger $ \runtime -> do
      let core d _=pure (False,d)
          send action values d=snd <$> debuggerEffects runtime core d [DebugAction action values]
          tick=tickDebugger runtime core
          waitForIO label predicate d=do
            result<-timeout debuggerTimeout (loop d)
            maybe (error (label++": timed out")) pure result
            where loop state=do
                    updated<-tick state
                    done<-predicate updated
                    if done then pure updated else threadDelay 1000 >> loop updated
          waitFor label predicate=waitForIO label (pure . predicate)
          choose index d=case dialog d of
            Just dg -> let adjusted=dg {fields=map (\f -> case f of ListBox title values _ -> ListBox title values index; _ -> f) (fields dg)}
                           (next,effects)=submitDialog 0 adjusted d
                       in snd <$> debuggerEffects runtime core next effects
            _ -> error "missing debugger dialog"
          connect=send "connect" ["0","127.0.0.1",T.pack port]
          logEntries=map (fromMaybe (error "invalid fixture log") . decodeStrictText) . T.lines <$> TIO.readFile logPath
          commands=map (fromMaybe Null . field "request") <$> logEntries
          finish d=do
            disconnected<-send "disconnect" [] d >>= waitFor "disconnect response" (T.isInfixOf "Debugger disconnected" . status)
            check "disconnect clears transient inspection" (dialog disconnected==Nothing)
            ended<-timeout 2000000 (waitForProcess process)
            check "fixture exits after disconnect" (ended==Just ExitSuccess)
            requests<-commands
            check "disconnect request actually reaches adapter without terminating program"
              (any (\r -> field "command" r==Just ("disconnect"::T.Text) && (field "arguments" r >>= field "terminateDebuggee")==Just False) requests)
            check "inspection never evaluates or sets values"
              (all (\r -> (field "command" r :: Maybe T.Text) `notElem` [Just "evaluate",Just "setVariable"]) requests)
      attached<-connect (initialDesktop (80,25))
      if mode=="frame" then do
        stopped<-waitFor "initial stop" (T.isInfixOf "Stopped" . status) attached
        stack<-send "stack" [] stopped >>= waitFor "frame chooser" (hasDialog "Call stack")
        waiting<-send "scopes" [] stack
        chosen<-choose 1 waiting
        -- This response follows the current source and both delayed old replies.
        drained<-send "threads" [] chosen >>= waitFor "stale frame replies must not open scopes" (hasDialog "Threads")
        check "late source cannot replace selected frame" (activeText drained=="chosen frame source\n")
        check "late source does not create an obsolete buffer" (not (any (T.isInfixOf "STALE" . contents . documentBuffer) (M.elems (buffers drained))))
        finish drained
      else do
        stopped<-waitFor "initial source" (T.isInfixOf "value = λ" . activeText) attached
        check "sourceReference opens read-only at adapter line" (maybe False ((/=Nothing).documentLabel) (activeDocument stopped) && fmap (caret.selection) (activeWindow stopped)==Just (T.length "module Generated where\n"))
        check "embedded debugger source renders syntax colors"
          ("color:rgb(255,255,255);background:rgb(0,0,170)'>where" `T.isInfixOf` snapshotHtml stopped)
        initialRequests<-commands
        check "variables are not fetched automatically" (not (any ((==Just ("variables"::T.Text)) . field "command") initialRequests))
        final<-case mode of
          "basic" -> do
            scopes<-send "scopes" [] stopped >>= waitFor "scopes" (hasDialog "Scopes")
            variables<-choose 0 scopes >>= waitFor "variables" (hasDialog "Variables")
            check "thunk state is displayed without evaluation" (any (T.isInfixOf "<thunk>") (rows variables))
            expanding<-choose 0 variables
            before<-length . filter ((==Just ("threads"::T.Text)) . field "command") <$> commands
            resumed<-send "continue" [] expanding
            after<-waitForIO "resume barrier" (\_ -> (>before) . length . filter ((==Just ("threads"::T.Text)) . field "command") <$> commands) resumed
            check "late variables cannot reopen inspection after resume" (dialog after==Nothing)
            send "pause" [] after >>= waitFor "pause" (T.isInfixOf "Stopped" . status)
          "lazy" -> do
            scopes<-send "scopes" [] stopped >>= waitFor "scopes" (hasDialog "Scopes")
            variables<-choose 0 scopes >>= waitFor "variables" (hasDialog "Variables")
            blocked<-choose 0 variables
            check "ordinary UI expansion does not force a lazy variable"
              ("explicit" `T.isInfixOf` status blocked)
            let current d=do (_,result)<-debuggerTool runtime core d "debug_status" (object []); result >>= either (error . T.unpack) pure
                reject ref gen d=do
                  (_,result)<-debuggerTool runtime core d "debug_inspect" (object
                    ["generation" .= (gen::Int),"request" .= ("variables"::T.Text),"variablesReference" .= (ref::Int)])
                  answer<-result
                  check "read-only inspection rejects forcing or unobserved references" (either (const True) (const False) answer)
            before<-current blocked
            let gen=fromMaybe (error "missing generation") (field "generation" before)::Int
            reject 22 gen blocked
            reject 999 gen blocked
            -- Initial attach and stopped events each request threads. This third
            -- request causes the fixture to invalidate only variable handles.
            pending<-send "threads" [] blocked {dialog=Nothing}
            invalidated<-waitForIO "variables invalidation" (\d -> maybe False (>gen) . field "generation" <$> current d) pending
            now<-current invalidated
            let gen'=fromMaybe (error "missing generation") (field "generation" now)::Int
            check "variable invalidation preserves stopped source frame" (field "stopped" now==Just True && (field "frame" now::Maybe Value)==field "frame" before)
            reject 21 gen' invalidated
            pendingThreads<-send "threads" [] invalidated
            refreshed<-waitForIO "thread invalidation refreshes selected source" (\d -> do
              state<-current d
              pure (maybe False (>gen') (field "generation" state) && T.isPrefixOf "Stopped in " (status d))) pendingThreads
            refreshedState<-current refreshed
            check "thread invalidation reloads current stopped frame"
              (field "stopped" refreshedState==Just True && field "threadId" refreshedState==Just (7::Int) && (field "frame" refreshedState::Maybe Value)==field "frame" before)
            requests<-commands
            check "only non-forcing scope variables reached the adapter"
              ([field "variablesReference" args::Maybe Int | req<-requests,field "command" req==Just ("variables"::T.Text),Just args<-[field "arguments" req]]==[Just 21])
            pure refreshed
          "choices" -> do
            scopes<-send "scopes" [] stopped >>= waitFor "first scope picker" (hasDialog "Scopes")
            replaced<-send "scopes" [] scopes >>= waitFor "second scopes reply" (T.isInfixOf "Scopes ready" . status)
            check "existing picker keeps visible rows" (rows replaced==["Locals"])
            expanded<-choose 0 replaced >>= waitFor "original scope expansion" (hasDialog "Variables")
            check "visible scope selects its original reference" (any (T.isInfixOf "<thunk>") (rows expanded) && not (any (T.isInfixOf "WRONG_SCOPE") (rows expanded)))
            pure expanded
          "breakpoints" -> do
            first<-send "breakpoint" [] stopped
            second<-send "breakpoint" [] (moveTo False 0 first)
            third<-send "breakpoint" [] second
            drained<-send "threads" [] third >>= waitFor "breakpoint reply barrier" (hasDialog "Threads")
            shown<-send "breakpoints" [] drained {dialog=Nothing}
            check "latest breakpoint result wins even when an old requested list reappears"
              (rows shown==["Generated.hs:2 verified at 202"])
            requests<-commands
            let changes=[fromMaybe [] (field "breakpoints" args) | req<-requests, field "command" req==Just ("setBreakpoints"::T.Text),Just args<-[field "arguments" req]] :: [[Value]]
            check "fixture exercised different snapshots and repeated original list" (map (map (field "line")) changes==[[Just (2::Int)],[Just 2,Just 1],[Just 2]])
            pure shown
          "mcp" -> do
            let tool name args desktop=do
                  (updated,result)<-debuggerTool runtime core desktop name (object args)
                  value<-result
                  pure (updated,value)
                success name args desktop=do
                  (updated,result)<-tool name args desktop
                  either (error . T.unpack) (pure . (updated,)) result
                rejected name args desktop=do
                  (updated,result)<-tool name args desktop
                  check ("MCP rejects "<>T.unpack name) (either (const True) (const False) result && updated==desktop)
                current desktop=snd <$> success "debug_status" [] desktop
                epoch value=fromMaybe (error "missing debugger generation") (field "generation" value) :: Int
                inspect args desktop=do
                  (updated,result)<-debuggerTool runtime core desktop "debug_inspect" (object args)
                  withAsync result $ \pending -> do
                    drained<-waitForIO "MCP inspection reply" (\_ -> isJust <$> poll pending) updated
                    value<-wait pending >>= either (error . T.unpack) pure
                    check "MCP inspection leaves UI unchanged" (drained==desktop)
                    pure value
            snapshot<-current stopped
            let gen=epoch snapshot
                virtualId=maybe (error "missing debugger source buffer") bufferId (activeWindow stopped)
            check "MCP reports connected, stopped selection" (field "connected" snapshot==Just True && field "stopped" snapshot==Just True && field "threadId" snapshot==Just (7::Int))
            rejected "debug_attach" ["port" .= (read port::Int)] stopped
            rejected "debug_launch" [] stopped
            rejected "debug_control" ["generation" .= (gen-1),"command" .= ("continue"::T.Text)] stopped
            rejected "debug_control" ["generation" .= gen,"command" .= ("pause"::T.Text)] stopped
            rejected "debug_inspect" ["generation" .= gen,"request" .= ("evaluate"::T.Text)] stopped
            rejected "debug_inspect" ["generation" .= gen,"request" .= ("variables"::T.Text),"variablesReference" .= (0::Int)] stopped
            rejected "debug_inspect" ["generation" .= gen,"request" .= ("threads"::T.Text),"count" .= (1001::Int)] stopped
            rejected "debug_status" ["unknown" .= True] stopped
            rejected "debug_set_breakpoints" ["generation" .= gen,"bufferId" .= virtualId,"lines" .= ([0]::[Int])] stopped
            rejected "debug_set_breakpoints" ["generation" .= gen,"bufferId" .= virtualId,"lines" .= replicate 1001 (1::Int)] stopped
            rejected "debug_set_breakpoints" ["generation" .= gen,"bufferId" .= (999999::Int),"lines" .= ([]::[Int])] stopped
            scopesResult<-inspect ["generation" .= gen,"request" .= ("scopes"::T.Text)] stopped
            check "MCP receives scope body" ((field "body" scopesResult >>= field "scopes" :: Maybe [Value])/=Nothing)
            vars<-inspect ["generation" .= gen,"request" .= ("variables"::T.Text),"variablesReference" .= (21::Int),"start" .= (0::Int),"count" .= (3::Int)] stopped
            check "MCP returns non-evaluating variable inspection" ("<thunk>" `T.isInfixOf` T.pack (show vars))
            _<-inspect ["generation" .= gen,"request" .= ("stackTrace"::T.Text)] stopped
            sourceResult<-inspect ["generation" .= gen,"request" .= ("source"::T.Text)] stopped
            check "MCP source reply uses selected sourceReference" ((field "body" sourceResult >>= field "content" :: Maybe T.Text)==Just (activeText stopped))
            let local=addDocument (Just (FileState (logPath<>".hs") Nothing)) (replaceBuffer False "dirty = 2\n" (newBuffer "local = 1\n")) stopped
                localId=maybe (error "missing local buffer") bufferId (activeWindow local)
                points bid requested=["generation" .= gen,"bufferId" .= bid,"lines" .= (requested::[Int])]
            (virtual,_)<-success "debug_set_breakpoints" (points virtualId [2,1,2]) local
            check "MCP breakpoint changes do not select their buffer" (virtual==local)
            (dirtyLocal,_)<-success "debug_set_breakpoints" (points localId [1]) virtual
            (same,_)<-success "debug_set_breakpoints" (points localId [1]) dirtyLocal
            _<-inspect ["generation" .= gen,"request" .= ("threads"::T.Text)] same
            requests<-commands
            let changes=[args | req<-requests,field "command" req==Just ("setBreakpoints"::T.Text),Just args<-[field "arguments" req]]
            check "MCP breakpoint replacement sorts and deduplicates, remains idempotent, and marks dirty source"
              (case changes of [virtualChange,localChange] -> (field "breakpoints" virtualChange :: Maybe [Value])==Just [object ["line" .= (1::Int)],object ["line" .= (2::Int)]] && field "sourceModified" localChange==Just True; _ -> False)
            verified<-current same
            let bps=fromMaybe [] (field "breakpoints" verified) :: [Value]
            check "MCP status exposes verified breakpoints" (length bps==3 && all ((==Just True) . field "verified") bps)
            before<-length . filter ((==Just ("threads"::T.Text)) . field "command") <$> commands
            (waiting,pending)<-debuggerTool runtime core same "debug_inspect" (object ["generation" .= gen,"request" .= ("variables"::T.Text),"variablesReference" .= (22::Int)])
            (resumed,accepted)<-success "debug_control" ["generation" .= gen,"command" .= ("continue"::T.Text)] waiting
            check "MCP control advances generation" (epoch accepted>gen)
            expired<-timeout 1000000 pending
            check "MCP pending inspection expires promptly on resume without ticking" (case expired of Just (Left _) -> True; _ -> False)
            rejected "debug_inspect" ["generation" .= gen,"request" .= ("scopes"::T.Text)] resumed
            drained<-waitForIO "MCP stale reply barrier" (\_ -> (>before) . length . filter ((==Just ("threads"::T.Text)) . field "command") <$> commands) resumed
            check "MCP stale variables never open a picker" (dialog drained==Nothing)
            send "pause" [] drained >>= waitFor "MCP pause" (T.isInfixOf "Stopped" . status)
          "reconnect" -> do
            virtual<-send "breakpoint" [] stopped
            let local=addDocument (Just (FileState (logPath<>".hs") Nothing)) (newBuffer "local = 1\n") virtual
            both<-send "breakpoint" [] local
            drained<-send "threads" [] both >>= waitFor "breakpoints delivered before reconnect" (hasDialog "Threads")
            reattached<-connect drained >>= waitFor "second session source" (T.isInfixOf "session = 2" . activeText)
            entries<-logEntries
            let configured :: Int -> [Value]
                configured n=[args | entry<-entries,field "session" entry==Just (n::Int),Just req<-[field "request" entry],field "command" req==Just ("setBreakpoints"::T.Text),Just args<-[field "arguments" req]]
            check "first session installed adapter-owned source breakpoint" (any (\args -> (field "source" args >>= field "sourceReference")==Just (9::Int)) (configured 1))
            check "reconnect retains only file breakpoints" (length (configured 2)==1 && all (\args -> (field "source" args >>= field "path" :: Maybe T.Text)/=Nothing && (field "source" args >>= field "sourceReference" :: Maybe Int)==Nothing) (configured 2))
            pure reattached
          _ -> error "unknown fixture mode"
        finish final

fixture :: String -> IO (String,FilePath,ProcessHandle)
fixture mode=do
  dir<-getTemporaryDirectory
  (logPath,h)<-openTempFile dir "dap-session.log"
  hClose h
  writeFile (logPath<>".hs") "local = 1\n"
  (_,Just output,_,process)<-createProcess (proc "python3" ["test/dap-session.py",logPath,mode]) {std_out=CreatePipe}
  port<-hGetLine output
  hClose output
  pure (port,logPath,process)

cleanup :: (String,FilePath,ProcessHandle) -> IO ()
cleanup (_,path,process)=do
  terminateProcess process
  _<-waitForProcess process
  removeFile path
  removeFile (path<>".hs")

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.:key))

rows :: Desktop -> [T.Text]
rows d=case dialog d of Just dg -> concat [values | ListBox _ values _<-fields dg]; _ -> []

hasDialog :: T.Text -> Desktop -> Bool
hasDialog title=maybe False ((==title).dialogTitle) . dialog

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)

launchChecks :: IO ()
launchChecks = do
  directory <- getCurrentDirectory
  port <- bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \listener -> do
    Socket.bind listener (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127,0,0,1)))
    Socket.SockAddrInet number _ <- Socket.getSocketName listener
    pure (fromIntegral number :: Int)
  temp <- getTemporaryDirectory
  bracket (openTempFile temp "dap-launch.json") (\(path,h) -> hClose h >> removeFile path) $ \(path,h) -> do
    hClose h
    let logs=path<>".log"
        config transport mode requestName=object
          ([ (if transport=="server" then "server" else "command") .=
              (["python3",directory<>"/test/dap-session.py",logs,transport<>"-"<>mode] :: [String]),
            "request" .= (requestName :: T.Text),"arguments" .= object ["program" .= ("space λ.hs" :: T.Text)]] ++
            ["port" .= port | transport=="server"])
        core d _=pure (False,d)
        waitFor runtime predicate d=timeout debuggerTimeout (loop d) >>= maybe (error "launch fixture timed out") pure
          where loop state=do
                  updated<-tickDebugger runtime core state
                  if predicate updated then pure updated else if "DAP:" `T.isPrefixOf` status updated then do
                    (_,finish)<-debuggerTool runtime core updated "debug_status" (object [])
                    details<-finish
                    error (T.unpack (status updated)++" "++show details)
                  else threadDelay 1000 >> loop updated
    mapM_ (\(transport,mode,requestName) -> withDebugger $ \runtime -> do
      writeFile logs ""
      BL.writeFile path (encode (config transport mode requestName))
      let send action values d=snd <$> debuggerEffects runtime core d [DebugAction action values]
      started<-send "launch-config" ["1",T.pack path] (initialDesktop (80,25))
      if mode=="launch-fail" then do
        failed<-waitFor runtime (T.isInfixOf "fixture refused" . status) started
        after<-send "threads" [] failed
        check "failed launch closes session" (status after=="Debugger is not ready for this command.")
      else do
        stopped<-waitFor runtime (T.isInfixOf "value = λ" . activeText) started
        disconnected<-send "disconnect" [] stopped >>= waitFor runtime (T.isInfixOf "Debugger disconnected" . status)
        check "stdio launch disconnect clears dialog" (dialog disconnected==Nothing)
      entries<-map (fromMaybe (error "bad launch log") . decodeStrictText) . T.lines <$> TIO.readFile logs
      let requests=[r | e<-entries,Just r<-[field "request" e]]
      unless (mode=="launch-fail") $ check "disconnect terminates launches and preserves attached programs"
        (any (\r -> field "command" r==Just ("disconnect"::T.Text) && (field "arguments" r >>= field "terminateDebuggee")==Just (requestName=="launch" || transport=="server")) requests)
      check "launch forwards request and arguments" (any (\r -> field "command" r==Just requestName && (field "arguments" r >>= field "program")==Just ("space λ.hs" :: T.Text)) requests)
      ) [("stdio","basic","launch"),("stdio","basic","attach"),("stdio","launch-fail","launch"),("server","basic","launch"),("server","basic","attach")]
    withDebugger $ \runtime -> do
      TIO.writeFile path "{\"command\":[\"python3\"],\"request\":\"launch\",\"arguments\":[]}"
      (_,d)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "launch-config" ["1",T.pack path]]
      check "invalid launch arguments rejected before spawning" ("arguments" `T.isInfixOf` status d)
    forM_ [object ["command" .= (["python3"]::[String]),"server" .= (["python3"]::[String])],
           object ["server" .= ([]::[String])],
           object ["server" .= (["python3"]::[String]),"host" .= ("example.com"::T.Text)]] $ \bad ->
      withDebugger $ \runtime -> do
        BL.writeFile path (encode bad)
        (_,d)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "launch-config" ["1",T.pack path]]
        check "invalid server config rejected before spawning" ("DAP configuration:" `T.isPrefixOf` status d)
    removeFile logs

-- Launch can compile a cold project. Other DAP requests keep the short deadline.
launchDeadlineCheck :: IO ()
launchDeadlineCheck = do
  directory<-getCurrentDirectory
  temp<-getTemporaryDirectory
  bracket (openTempFile temp "dap-cold-launch.json")
    (\(path,h) -> hClose h >> mapM_ removeFile [path,path<>".log"]) $ \(path,h) -> do
    hClose h
    let logs=path<>".log"
        core d _=pure (False,d)
    writeFile logs ""
    BL.writeFile path (encode (object ["command" .= (["python3",directory<>"/test/dap-session.py",logs,"stdio-launch-wait"]::[String])]))
    clock<-newIORef 0
    withDebuggerClock (readIORef clock) $ \runtime -> do
      (_,started)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "launch-config" ["1",T.pack path]]
      let loop d=do
            updated<-tickDebugger runtime core d
            (_,finish)<-debuggerTool runtime core updated "debug_status" (object [])
            result<-finish
            if either (const False) (\v -> maybe False (T.isInfixOf "loading cradle") (field "output" v)) result
              then pure updated else threadDelay 1000 >> loop updated
      loading<-timeout debuggerTimeout (loop started) >>= maybe (error "cold launch fixture did not load") pure
      writeIORef clock 16000000000
      alive<-tickDebugger runtime core loading
      check "cold launch survives ordinary request deadline" (not ("timed out" `T.isInfixOf` status alive))
      writeIORef clock 121000000000
      expired<-tickDebugger runtime core alive
      check "cold launch retains bounded deadline" ("DAP request timed out" `T.isInfixOf` status expired)

-- Six minutes pass before the UI sees transport readiness. The request clock
-- must start at that event, and still expire an unresponsive initialize afterward.
startupDeadlineCheck :: IO ()
startupDeadlineCheck = do
  temp<-getTemporaryDirectory
  bracket (openTempFile temp "dap-deadline.json")
    (\(path,h) -> hClose h >> mapM_ (\file -> doesFileExist file >>= \exists -> when exists (removeFile file)) [path,path<>".ready",path<>".received"]) $ \(path,h) -> do
    hClose h
    clock<-newIORef 0
    let script="import pathlib,sys,time; p=sys.argv[1]; pathlib.Path(p+'.ready').touch(); sys.stdin.buffer.readline(); pathlib.Path(p+'.received').touch(); time.sleep(60)"
        core d _=pure (False,d)
        waitFor label action=timeout debuggerTimeout (loop action) >>= maybe (error label) pure
        loop action=action >>= \done -> unless done (threadDelay 1000 >> loop action)
    BL.writeFile path (encode (object ["command" .= (["python3","-u","-c",script,path] :: [String]),
      "request" .= ("launch" :: T.Text),"arguments" .= object []]))
    withDebuggerClock (readIORef clock) $ \runtime -> do
      (_,started)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "launch-config" ["1",T.pack path]]
      waitFor "deadline adapter did not start" (doesFileExist (path<>".ready"))
      sentEarly<-doesFileExist (path<>".received")
      check "initialize waits for transport readiness event" (not sentEarly)
      writeIORef clock 360000000000
      let awaitInitialize d=do
            updated<-tickDebugger runtime core d
            check "startup elapsed time does not consume initialize deadline" (not ("timed out" `T.isInfixOf` status updated))
            received<-doesFileExist (path<>".received")
            if received then pure updated else threadDelay 1000 >> awaitInitialize updated
      connected<-timeout debuggerTimeout (awaitInitialize started) >>= maybe (error "initialize was not sent after readiness") pure
      writeIORef clock 376000000000
      expired<-tickDebugger runtime core connected
      check "connected initialize retains a bounded request deadline" ("DAP request timed out" `T.isInfixOf` status expired)

-- A Windows-owned adapter can require the bounded taskkill /T grace on stop.
debuggerTimeout :: Int
debuggerTimeout=if os=="mingw32" then 10000000 else 5000000

presentationCheck :: IO ()
presentationCheck=bracket (fixture "basic") cleanup $ \(port,_,_) -> withDebugger $ \runtime -> do
  let core d _=pure (False,d)
      tool name args d=do
        (updated,finish)<-debuggerTool runtime core d name (object args)
        result<-finish >>= either (error . T.unpack) pure
        pure (updated,result)
      current d=snd <$> tool "debug_status" [] d
      epoch value=fromMaybe (error "missing debugger generation") (field "generation" value) :: Int
      waitFor label predicate d=timeout debuggerTimeout (loop d) >>= maybe (error (label<>" timed out")) pure
        where loop state=do
                updated<-tickDebugger runtime core state
                snapshot<-current updated
                if predicate updated snapshot then pure updated else threadDelay 1000 >> loop updated
      reveal view gen d=tool "debug_present" ["generation" .= gen,"view" .= (view::T.Text)] d
      base=(addDocument Nothing (newBuffer "my editor work") (initialDesktop (80,25)))
        {dialog=Just (Dialog "Existing debugger view" (DebugDialog "existing") [] 0 ["Close"] [])}
  (quiet,configured)<-tool "debug_present" ["follow" .= False] base
  check "background preference can be set before connecting without changing UI" (quiet==base && field "follow" configured==Just False)
  (attached,_)<-tool "debug_attach" ["port" .= (read port::Int)] quiet
  -- Preserve an existing modal while asynchronous adapter events arrive.
  let working=attached {dialog=dialog base}
  stopped<-waitFor "background frame" (\_ s -> field "stopped" s==Just True && (field "frame" s >>= field "id")==Just (11::Int)) working
  snapshot<-current stopped
  let gen=epoch snapshot
  check "background stop keeps windows, focus, buffers and modal"
    (windows stopped==windows working && buffers stopped==buffers working && dialog stopped==dialog working && field "follow" snapshot==Just False)
  (stale,staleReply)<-debuggerTool runtime core stopped "debug_present" (object ["generation" .= (gen-1),"follow" .= True,"view" .= ("source"::T.Text)])
  rejected<-staleReply
  afterRejected<-current stale
  check "stale reveal rejects preference changes atomically" (stale==stopped && either (const True) (const False) rejected && field "follow" afterRejected==Just False)
  forM_ [object ["view" .= ("source"::T.Text)],object ["generation" .= gen,"view" .= ("unknown"::T.Text)],object ["follow" .= ("no"::T.Text)]] $ \args -> do
    (unchanged,finish)<-debuggerTool runtime core stopped "debug_present" args
    result<-finish
    check "presentation arguments reject missing generation/unknown view/wrong type" (unchanged==stopped && either (const True) (const False) result)
  (requested,_)<-reveal "source" gen stopped
  shown<-waitFor "explicit source reveal" (\d _ -> "value = λ" `T.isInfixOf` activeText d) requested
  shownStatus<-current shown
  check "explicit reveal opens source without changing stopped generation or follow mode"
    (epoch shownStatus==gen && field "follow" shownStatus==Just False && dialog shown==dialog stopped)
  (stack,stackStatus)<-reveal "stack" gen shown {dialog=Nothing}
  check "cached stack is explicitly visible without resuming" (hasDialog "Call stack" stack && epoch stackStatus==gen)
  (loading,_)<-reveal "scopes" gen stack {dialog=Nothing}
  scopes<-waitFor "explicit scopes reveal" (\d _ -> hasDialog "Scopes" d) loading
  (output,_)<-reveal "output" gen scopes {dialog=Nothing}
  check "explicit output uses existing debugger session" (fmap documentLabel (activeDocument output)==Just (Just "Debugger output"))
  (following,_)<-tool "debug_present" ["follow" .= True] output
  (running,_)<-tool "debug_control" ["generation" .= gen,"command" .= ("continue"::T.Text)] following
  ready<-waitFor "resume processed" (\d _ -> status d=="Running...") running
  runningStatus<-current ready
  (pausing,_)<-tool "debug_control" ["generation" .= epoch runningStatus,"command" .= ("pause"::T.Text)] ready
  followed<-waitFor "follow restored" (\d s -> field "stopped" s==Just True && "value = λ" `T.isInfixOf` activeText d) pausing
  followedStatus<-current followed
  check "reenabling follow restores automatic source reveal on later stop" (field "follow" followedStatus==Just True && epoch followedStatus>gen)
  _<-tool "debug_control" ["generation" .= epoch followedStatus,"command" .= ("disconnect"::T.Text)] followed
  pure ()

-- The adapter's source reply is queued after the stack tick. Turning off follow
-- between those ticks must suppress an automatic reveal already in flight.
pendingPresentationCheck :: IO ()
pendingPresentationCheck=bracket (fixture "basic") cleanup $ \(port,_,_) -> withDebugger $ \runtime -> do
  let core d _=pure (False,d)
      tool name args d=do
        (updated,finish)<-debuggerTool runtime core d name (object args)
        result<-finish >>= either (error . T.unpack) pure
        pure (updated,result)
      current d=snd <$> tool "debug_status" [] d
      tick=tickDebugger runtime core
      base=addDocument Nothing (newBuffer "unchanged foreground") (initialDesktop (80,25))
      waitUntil label predicate d=timeout debuggerTimeout (loop d) >>= maybe (error (label<>" timed out")) pure
        where loop desktop=do
                updated<-tick desktop
                done<-predicate updated
                if done then pure updated else threadDelay 1000 >> loop updated
  (attached,_)<-tool "debug_attach" ["port" .= (read port::Int)] base
  awaitingSource<-waitUntil "automatic source request" (\d->do
    s<-current d
    pure ((field "frame" s >>= field "id")==Just (11::Int))) attached
  check "fixture exposes a source request before its response is processed" (activeText awaitingSource=="unchanged foreground")
  (quiet,_)<-tool "debug_present" ["follow" .= False] awaitingSource
  snapshot<-current quiet
  let gen=fromMaybe (error "missing generation") (field "generation" snapshot) :: Int
  (pending,finish)<-debuggerTool runtime core quiet "debug_inspect" (object ["generation" .= gen,"request" .= ("threads"::T.Text)])
  drained<-withAsync finish $ \reply -> do
    after<-waitUntil "source reply barrier" (\_ -> isJust <$> poll reply) pending
    result<-wait reply
    check "structured response remains available in background" (either (const False) (const True) result)
    pure after
  check "late automatic source reply cannot steal focus after follow is disabled"
    (windows drained==windows quiet && buffers drained==buffers quiet && dialog drained==dialog quiet)
  _<-tool "debug_control" ["generation" .= gen,"command" .= ("disconnect"::T.Text)] drained
  pure ()
