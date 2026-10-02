{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Debugger (Debugger, Core, withDebugger, withDebuggerClock, debuggerEffects, tickDebugger, debuggerTool) where

import Control.Concurrent (MVar, newEmptyMVar, tryPutMVar, tryReadMVar, threadDelay)
import Control.Exception (IOException, bracket, try)
import Control.Monad (foldM, forM_, unless, when)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (XdgDirectory(XdgConfig), canonicalizePath, doesFileExist, getXdgDirectory)
import System.IO (IOMode(ReadMode), withBinaryFile)
import System.FilePath (isAbsolute, takeFileName, (</>))
import System.Timeout (timeout)
import Text.Read (readMaybe)
import qualified THC.Edit.Build as Build
import THC.Edit.Build (resolveBuildRoot)
import THC.Edit.Buffer
import qualified THC.Edit.DAP as D
import THC.Edit.Files (filePath)
import qualified THC.Edit.LSP as L
import THC.Edit.Model
import THC.Edit.Syntax (highlightFor)

type Core = Desktop -> [Effect] -> IO (Bool,Desktop)
data Debugger = Debugger (IORef State) (IO Integer)
data Pending = Init | Attach | Configure | Breaks Text [Int] | Exceptions | Threads Bool
  | Stack Bool | Scopes | Variables | Source Bool Value | Control Bool | Detach
  | Inspection Text (MVar (Either Text Value))
  deriving (Eq)
data Breakpoint = Breakpoint { bpLine :: Int, bpResult :: Value } deriving (Eq,Show)
data State = State
  { client :: Maybe D.Client, connected :: Bool, capabilities :: Value, ready :: Bool, configured :: Bool
  , pending :: M.Map Int (Pending,Int,Integer), generation :: Int
  , stopped :: Bool, thread :: Maybe Int, frame :: Maybe Value, frames :: [Value], followSource :: Bool
  , exceptionFilters :: [Text]
  , breakpoints :: M.Map Text (Value,[Breakpoint]), sources :: M.Map Int Value
  , root :: FilePath, endpoint :: (Text,Int), output :: Text
  , startRequest :: (Text,Value), managed :: Bool, adapterId :: Text
  , failure :: Maybe Text, disconnectAt :: Maybe Integer
  , choices :: M.Map Text [Value], choiceId :: Int, breakRequests :: M.Map Text Int
  , breakModified :: M.Map Text Bool, variableRefs :: M.Map Int Bool
  }

emptyState :: State
emptyState = State {client=Nothing,connected=False,capabilities=Null,ready=False,configured=False,pending=M.empty,generation=0,
  stopped=False,thread=Nothing,frame=Nothing,frames=[],followSource=True,exceptionFilters=[],breakpoints=M.empty,sources=M.empty,
  root=".",endpoint=("127.0.0.1",4711),output="",failure=Nothing,disconnectAt=Nothing,
  choices=M.empty,choiceId=0,breakRequests=M.empty,breakModified=M.empty,startRequest=("attach",object []),managed=False,adapterId="",variableRefs=M.empty}

withDebugger :: (Debugger -> IO a) -> IO a
withDebugger = withDebuggerClock (toInteger <$> getMonotonicTimeNSec)

-- Clock values are monotonic nanoseconds, also allowing deterministic deadline checks.
withDebuggerClock :: IO Integer -> (Debugger -> IO a) -> IO a
withDebuggerClock clock = bracket ((\ref -> Debugger ref clock) <$> newIORef emptyState) $ \(Debugger ref _) ->
  readIORef ref >>= mapM_ D.stopClient . client

debuggerEffects :: Debugger -> Core -> Core
debuggerEffects runtime fallback = foldM apply . (False,)
  where
    apply result@(True,_) _=pure result
    apply (_,d) (DebugAction action values) = (False,) <$> perform runtime fallback action values d
    apply (_,d) effect=fallback d [effect]

-- The first action runs under the desktop lock; its continuation must run outside
-- that lock so the normal editor tick can receive the adapter's response.
debuggerTool :: Debugger -> Core -> Desktop -> Text -> Value -> IO (Desktop, IO (Either Text Value))
debuggerTool runtime@(Debugger ref _) core d name arguments = do
  s<-readIORef ref
  case parseEither (parseTool s name) arguments of
    Left err -> pure (d,pure (Left (T.pack err)))
    Right request -> do
      result<-try (run request)
      pure $ either (\(err::IOException) -> (d,pure (Left ("DAP: "<>T.pack (show err))))) id result
  where
    immediate desktop result=pure (desktop,pure (result >>= boundedResult))
    snapshot desktop=do
      current<-readIORef ref
      immediate desktop $ maybe (Right (merge (object ["accepted" .= True]) (debuggerStatus current))) Left (failure current)
    run ToolStatus=readIORef ref >>= immediate d . Right . debuggerStatus
    run (ToolStart action values)=do
      desktop<-perform runtime core action values d
      current<-readIORef ref
      if isJust (client current) then snapshot desktop else immediate desktop (Left (status desktop))
    run (ToolControl command)=perform runtime core command [] d >>= snapshot
    run (ToolPresent following view)=do
      forM_ following (\enabled->modifyIORef' ref (\state->state {followSource=enabled}))
      current<-readIORef ref
      shown<-case view of
        Nothing -> pure d
        Just "source" -> maybe (pure d) (openFrame runtime core True d) (frame current)
        Just "stack" -> showChoices runtime "Call stack" "frame" (frames current) (map frameLabel (frames current)) d
        Just command -> perform runtime core command [] d
      snapshot shown
    run (ToolBreakpoints bid rows)=case M.lookup bid (buffers d) of
      Nothing -> immediate d (Left "Unknown bufferId.")
      Just doc | byteMode (documentBuffer doc) -> immediate d (Left "Breakpoints require a source text buffer.")
      Just doc -> do
        s<-readIORef ref
        source<-case documentFile doc of
          Just file -> Just . object . (:[]) . ("path" .=) <$> canonicalizePath (filePath file)
          Nothing -> pure (M.lookup bid (sources s))
        case source of
          Nothing -> immediate d (Left "Buffer has no file or debugger source.")
          Just src -> do
            let key=sourceKey src
                linesRequested=M.keys (M.fromList [(row,()) | row<-rows])
                old=maybe [] snd (M.lookup key (breakpoints s))
                modified=dirty (documentBuffer doc)
                unchanged=map bpLine old==linesRequested && M.findWithDefault False key (breakModified s)==modified
                points=if unchanged then old else [Breakpoint row Null | row<-linesRequested]
            unless unchanged $ do
              modifyIORef' ref (\state -> state {breakpoints=M.insert key (src,points) (breakpoints state),
                breakModified=M.insert key modified (breakModified state)})
              when (configured s) (sendBreakpoints runtime key src points)
            snapshot d
    run (ToolInspect command args)=do
      s<-readIORef ref
      reply<-newEmptyMVar
      send runtime (Inspection command reply) command args
      pure (d,do
        result<-timeout 16000000 (awaitInspection ref (generation s) reply)
        pure $ case result of
          Nothing -> Left "Debugger inspection timed out; refresh debug_status."
          Just value -> value >>= \body -> boundedResult (object
            ["generation" .= generation s,"request" .= command,"body" .= body]))

data ToolRequest = ToolStatus | ToolStart Text [Text] | ToolControl Text
  | ToolBreakpoints Int [Int] | ToolInspect Text Value | ToolPresent (Maybe Bool) (Maybe Text)

parseTool :: State -> Text -> Value -> Parser ToolRequest
parseTool s name = withObject "debugger tool arguments" $ \o -> do
  let fieldsAllowed names=unless (all ((`elem` names) . K.toText) (KM.keys o)) (fail "Unknown debugger argument")
      epoch=do
        expected<-o .: "generation"
        unless (expected==generation s) (fail "Debugger generation expired; refresh debug_status")
      live=unless (isJust (client s) && disconnectAt s==Nothing) (fail "No active debugger session")
      idle=when (isJust (client s)) (fail "Disconnect the existing debugger session first")
      portNumber=do
        port<-o .:? "port" .!= (4711::Int)
        unless (port>0 && port<=65535) (fail "port must be between 1 and 65535")
        pure port
      positive key=do
        value<-o .:? key
        forM_ value (\n -> unless (n>0) (fail (T.unpack (K.toText key)<>" must be positive")))
        pure (value :: Maybe Int)
      required key selected=positive key >>= maybe (maybe (fail (T.unpack (K.toText key)<>" is required")) pure selected) pure
  case name of
    "debug_status" -> fieldsAllowed [] >> pure ToolStatus
    "debug_present" -> do
      fieldsAllowed ["follow","view","generation"]
      following<-o .:? "follow"
      view<-o .:? "view"
      when (KM.member "generation" o || isJust view) epoch
      forM_ view $ \choice -> do
        unless (choice `elem` ["source","stack","scopes","output"]) (fail "view must be source, stack, scopes or output")
        when (choice/="output") $ do
          live
          unless (ready s && configured s && stopped s) (fail "Debugger must be ready and stopped to reveal this view")
          when (choice `elem` ["source","scopes"] && frame s==Nothing) (fail "No selected debugger frame; inspect debug_status after the stack arrives")
      pure (ToolPresent following view)
    "debug_launch" -> do
      fieldsAllowed ["adapterConfig","port"]
      idle
      port<-portNumber
      config<-o .:? "adapterConfig"
      case config of
        Just path | T.null path || T.any (=='\0') path -> fail "adapterConfig must be a nonempty path without NUL bytes"
        Just path -> pure (ToolStart "launch-config" ["1",path])
        Nothing -> pure (ToolStart "launch-config" ["0","",tshow port])
    "debug_attach" -> do
      fieldsAllowed ["host","port"]
      idle
      host<-o .:? "host" .!= "127.0.0.1"
      unless (host `elem` ["localhost","127.0.0.1","::1"]) (fail "host must be loopback")
      port<-portNumber
      pure (ToolStart "connect" ["0",host,tshow port])
    "debug_control" -> do
      fieldsAllowed ["generation","command"]
      epoch
      live
      command<-o .: "command"
      unless (command `elem` ["continue","next","stepIn","stepOut","pause","disconnect"]) (fail "Unsupported debugger control")
      unless (command=="disconnect" || (ready s && configured s && isJust (thread s) &&
        if command=="pause" then not (stopped s) else stopped s)) (fail "Debugger is not ready for this control")
      pure (ToolControl command)
    "debug_set_breakpoints" -> do
      fieldsAllowed ["generation","bufferId","lines"]
      epoch
      bid<-o .: "bufferId"
      rows<-o .: "lines"
      unless (bid>=0 && length rows<=1000 && all (>0) rows) (fail "bufferId must be nonnegative and lines must contain at most 1000 positive integers")
      pure (ToolBreakpoints bid rows)
    "debug_inspect" -> do
      fieldsAllowed ["generation","request","threadId","frameId","variablesReference","sourceReference","start","count"]
      epoch
      live
      unless (ready s && configured s) (fail "Debugger is not ready for inspection")
      command<-o .: "request"
      unless (command `elem` ["threads","stackTrace","scopes","variables","source"]) (fail "Unsupported debugger inspection")
      when (command `elem` ["stackTrace","scopes","variables"] && not (stopped s)) (fail "Debugger must be stopped for this inspection")
      -- Validate even unused optional fields: malformed handles are never ignored.
      mapM_ positive ["threadId","frameId","variablesReference","sourceReference"]
      start<-o .:? "start" .!= (0::Int)
      count<-o .:? "count" .!= (100::Int)
      unless (start>=0 && count>0 && count<=1000) (fail "start must be nonnegative and count must be between 1 and 1000")
      args<-case command of
        "threads" -> pure (object [])
        "stackTrace" -> do
          tid<-required "threadId" (thread s)
          pure (object ["threadId" .= tid,"startFrame" .= start,"levels" .= count])
        "scopes" -> do
          ident<-required "frameId" (frame s >>= field "id")
          pure (object ["frameId" .= ident])
        "variables" -> do
          ident<-required "variablesReference" Nothing
          case M.lookup ident (variableRefs s) of
            Just False -> pure ()
            Just True -> fail "Lazy variable requires explicit evaluation; read-only inspection cannot force it"
            Nothing -> fail "Unknown or expired variable reference; request scopes/variables again"
          pure (object ["variablesReference" .= ident,"start" .= start,"count" .= count])
        _ -> do
          let selected=frame s >>= field "source"
              reference=selected >>= field "sourceReference" >>= \n -> if n>0 then Just n else Nothing
          ident<-required "sourceReference" reference
          pure (object ["sourceReference" .= ident])
      pure (ToolInspect command args)
    _ -> fail "Unknown debugger tool"

debuggerStatus :: State -> Value
debuggerStatus s=object
  ["generation" .= generation s,"active" .= isJust (client s),"connected" .= connected s,
   "ready" .= ready s,"configured" .= configured s,"stopped" .= stopped s,"follow" .= followSource s,
   "threadId" .= thread s,"frame" .= frame s,"source" .= (frame s >>= (field "source" :: Value -> Maybe Value)),
   "capabilities" .= capabilities s,"breakpoints" .=
     [object ["source" .= src,"line" .= bpLine bp,"verified" .= flag "verified" (bpResult bp),
       "pending" .= (bpResult bp==Null),"result" .= bpResult bp,
       "sourceModified" .= M.findWithDefault False key (breakModified s)] | (key,src,bp)<-allBreakpoints s],
   "output" .= output s,"error" .= failure s]

boundedResult :: Value -> Either Text Value
boundedResult value | BL.length (encode value)>=1024*1024 = Left "Debugger response exceeds 1 MiB; request a smaller page."
                    | otherwise = Right value

completeInspection :: Pending -> Either Text Value -> IO ()
completeInspection (Inspection _ reply) result=tryPutMVar reply result >> pure ()
completeInspection _ _=pure ()

awaitInspection :: IORef State -> Int -> MVar (Either Text Value) -> IO (Either Text Value)
awaitInspection ref epoch reply=do
  result<-tryReadMVar reply
  s<-readIORef ref
  if generation s/=epoch || not (isJust (client s)) || disconnectAt s/=Nothing
    then pure (Left "Debugger inspection expired; refresh debug_status.")
    else case failure s of
      Just err -> pure (Left err)
      Nothing -> maybe (threadDelay 10000 >> awaitInspection ref epoch reply) pure result

perform :: Debugger -> Core -> Text -> [Text] -> Desktop -> IO Desktop
perform runtime@(Debugger ref clock) core action values d = do
  s<-readIORef ref
  case (action,values) of
    ("output",_) -> pure (addReadOnly "Debugger output" (output s) d)
    -- Docs: docs/site/screenshots/debug-launch.png (docs/running.md).
    ("launch",_) -> pure d {dialog=Just (Dialog "Launch debugger" (DebugDialog "launch-config")
      [Input "Adapter configuration" ".thc-debug.json" 15,Input "THC DAP port" "4711" 4] 0 ["THC target","Adapter config","Cancel"]
      ["THC target uses your selected build settings.",
       "Adapter config reads a project-relative JSON file."])}
    ("launch-config","0":_:portText:_) -> case readMaybe (T.unpack portText) of
      Just port | port>0 && port<=65535 -> do
        result<-try $ launchTHC runtime port d
        pure $ either (\(err::IOException) -> d {status="THC debugger: "<>T.pack (show err)}) id result
      _ -> pure d {status="Enter a DAP port between 1 and 65535."}
    ("launch-config","1":configPath:_) -> do
      result<-try $ do
        directory<-resolveBuildRoot d
        let path=if isAbsolute (T.unpack configPath) then T.unpack configPath else directory </> T.unpack configPath
        bytes<-withBinaryFile path ReadMode (\h -> BS.hGet h (1024*1024+1))
        if BS.length bytes>1024*1024 then pure (Left "Debugger configuration exceeds 1 MiB.") else
          case eitherDecodeStrict' bytes >>= parseEither parseLaunch of
            Left err -> pure (Left (T.pack err))
            Right config -> Right <$> startSession runtime directory config d
      pure $ either (\(err::IOException) -> d {status="DAP: "<>T.pack (show err)})
        (either (\err -> d {status="DAP configuration: "<>err}) id) result
    ("attach",_) -> let (host,port)=endpoint s in pure d {dialog=Just (Dialog "Attach debugger" (DebugDialog "connect")
      [Input "Host" host (T.length host),Input "Port" (tshow port) (length (show port))] 0 ["Attach","Cancel"]
      ["Connect to a running loopback DAP server.","Use Launch / Adapter config for custom attach arguments."])}
    ("connect",_:host:portText:_) -> case readMaybe (T.unpack portText) of
      Just port | port>0 && port<=65535 -> do
        result<-try $ do
          directory<-resolveBuildRoot d
          startSession runtime directory (LaunchConfig Nothing host port "attach" (object []) "thc") d
        pure $ either (\(err::IOException) -> d {status="DAP: "<>T.pack (show err)}) id result
      _ -> pure d {status="Enter a port between 1 and 65535."}
    ("disconnect",_) -> do
      now<-clock
      modifyIORef' ref (\state -> (invalidate state) {ready=False,configured=False,pending=M.empty,disconnectAt=Just now})
      send runtime Detach "disconnect" (object ["terminateDebuggee" .= (managed s || fst (startRequest s)=="launch")])
      pure (clearDialog d) {status="Disconnecting debugger..."}
    ("breakpoint",_) -> toggleBreakpoint runtime d
    ("breakpoints",_) -> do
      let rows=[object ["key" .= key,"line" .= bpLine bp] | (key,_,bp)<-allBreakpoints s]
          labels=[sourceLabel src<>":"<>tshow (bpLine bp)<>if flag "verified" (bpResult bp) then " verified at "<>tshow (integer "line" (bpResult bp)) else " pending "<>text "message" (bpResult bp) | (_,src,bp)<-allBreakpoints s]
      shown<-showChoices runtime "Breakpoints" "remove-breakpoint" rows labels d
      pure shown {dialog=fmap (\dg -> dg {buttons=["Remove","Cancel"]}) (dialog shown)}
    ("exceptions",_) | ready s ->
      let filters=items "exceptionBreakpointFilters" (capabilities s) in
      pure $ if null filters then d {status="This debugger advertises no exception filters."} else
        d {dialog=Just (Dialog "Exception breakpoints" (DebugDialog (token s "exceptions"))
           [CheckBox (text "label" f) (text "filter" f `elem` exceptionFilters s) | f<-filters] 0 ["OK","Cancel"] [])}
    ("threads",_) | configured s -> send runtime (Threads True) "threads" (object []) >> pure d {status="Loading threads..."}
    ("stack",_) | stopped s, Just tid<-thread s -> send runtime (Stack True) "stackTrace" (stackArguments tid) >> pure d {status="Loading call stack..."}
    ("scopes",_) | stopped s, Just selected<-frame s,Just ident<-(field "id" selected :: Maybe Int) ->
      send runtime Scopes "scopes" (object ["frameId" .= ident]) >> pure d {status="Loading scopes..."}
    (command,_) | command `elem` ["continue","next","stepIn","stepOut","pause"],ready s,Just tid<-thread s,
                  (command=="pause" && not (stopped s)) || (command/="pause" && stopped s) -> do
      when (command/="pause") (modifyIORef' ref invalidate)
      send runtime (Control (stopped s)) command (object ["threadId" .= tid])
      pure (clearDialog d) {status=if command=="pause" then "Pausing..." else "Running..."}
    _ | Just (epoch,choice)<-parseToken action,epoch==generation s -> select runtime core action choice values d
      | "select:" `T.isPrefixOf` action -> pure (clearDialog d) {status="Debugger selection expired."}
      | otherwise -> pure d {status="Debugger is not ready for this command."}

data LaunchConfig = LaunchConfig (Maybe [String]) Text Int Text Value Text

parseLaunch :: Value -> Parser LaunchConfig
parseLaunch = withObject "debugger configuration" $ \o -> do
  command<-o .:? "command"
  host<-o .:? "host" .!= "127.0.0.1"
  port<-o .:? "port" .!= 4711
  requestName<-o .:? "request" .!= "launch"
  arguments<-o .:? "arguments" .!= object []
  adapter<-o .:? "adapterId" .!= "thc-edit"
  unless (requestName `elem` ["launch","attach"]) (fail "request must be launch or attach")
  case arguments of Object _ -> pure (); _ -> fail "arguments must be a JSON object"
  case command of
    Just (exe:args) | not (null exe),all (notElem '\0') (exe:args) ->
      when (KM.member "host" o || KM.member "port" o) (fail "choose command or host/port, not both")
    Just _ -> fail "command must be a nonempty argv array without NUL bytes"
    Nothing -> unless (host `elem` ["localhost","127.0.0.1","::1"] && port>0 && port<=65535)
      (fail "host/port must name a loopback DAP endpoint")
  pure (LaunchConfig command host port requestName arguments adapter)

startSession :: Debugger -> FilePath -> LaunchConfig -> Desktop -> IO Desktop
startSession runtime directory (LaunchConfig command host port requestName arguments adapter) d = do
  c<-case command of
    Just (exe:args) -> D.startAdapter exe args directory
    _ -> D.startClient host port
  initializeSession runtime directory c (host,port) requestName arguments adapter False d

launchTHC :: Debugger -> Int -> Desktop -> IO Desktop
launchTHC runtime@(Debugger ref _) port d
  | any (\doc -> documentLabel doc==Nothing && dirty (documentBuffer doc)) (M.elems (buffers d)) =
      pure d {status="Save modified source files before launching the disk build."}
  | otherwise = do
      directory<-resolveBuildRoot d
      settings<-getXdgDirectory XdgConfig "thc-edit"
      config<-Build.loadBuildConfig settings directory
      if Build.buildToolchain config/=Build.THC then pure d {status="Select the THC toolchain in Build target, or choose Adapter config for GHC debugging."}
      else do
        plan<-Build.buildPlan Build.Run config directory (filePath <$> (activeDocument d >>= documentFile))
        case plan of
          Right [(exe,args)] -> do
            let (compilerArgs,guestArgs)=break (=="--") args
                flags=["--dap-port",show port]
            -- Release an earlier managed session before testing its port again.
            readIORef ref >>= mapM_ D.stopClient . client
            c<-D.startManaged exe (compilerArgs++flags++guestArgs) directory "127.0.0.1" port
            started<-initializeSession runtime directory c ("127.0.0.1",port) "attach" (object []) "graalvm" True d
            pure started {status="Starting THC debugger; build output is in Debug / Output..."}
          Left err -> pure d {status=err}
          _ -> pure d {status="THC debugger requires a single runtime launch command."}

initializeSession :: Debugger -> FilePath -> D.Client -> (Text,Int) -> Text -> Value -> Text -> Bool -> Desktop -> IO Desktop
initializeSession (Debugger ref _) directory c address requestName arguments adapter owned d = do
  s<-readIORef ref
  mapM_ D.stopClient (client s)
  writeIORef ref emptyState {client=Just c,generation=generation s+1,root=directory,endpoint=address,
    followSource=followSource s,breakpoints=persistentBreakpoints s,breakModified=breakModified s,startRequest=(requestName,arguments),managed=owned,adapterId=adapter}
  pure (automaticDesktop s d) {status="Connecting debugger..."}

-- Frame and variable handles are scoped to a suspended execution state.
invalidate :: State -> State
invalidate s=s {generation=generation s+1,stopped=False,frame=Nothing,frames=[],choices=M.empty,variableRefs=M.empty}

send :: Debugger -> Pending -> Text -> Value -> IO ()
send (Debugger ref clock) kind command arguments = do
  s<-readIORef ref
  forM_ (client s) $ \c -> do
    result<-try (D.request c command arguments)
    now<-clock
    case result of
      Left (err::IOException) -> modifyIORef' ref (\state -> state {failure=Just ("DAP: "<>T.pack (show err))})
      Right ident -> modifyIORef' ref (\state -> state {pending=M.insert ident (kind,generation state,now) (pending state),
        breakRequests=case kind of Breaks key _ -> M.insert key ident (breakRequests state); _ -> breakRequests state})

tickDebugger :: Debugger -> Core -> Desktop -> IO Desktop
tickDebugger runtime@(Debugger ref clock) core original = do
  s<-readIORef ref
  events<-maybe (pure []) D.pollEvents (client s)
  updated<-foldM (receive runtime core) original events
  now<-clock
  current<-readIORef ref
  let expired=M.filter (\(_,_,sent) -> now-sent>15000000000) (pending current)
  let detachExpired=maybe False (\sent -> now-sent>1000000000) (disconnectAt current)
      timedOut=not (M.null expired)
  if not timedOut && not detachExpired && failure current==Nothing then pure updated else do
    mapM_ D.stopClient (client current)
    modifyIORef' ref (\state -> (invalidate state) {client=Nothing,connected=False,pending=M.empty,ready=False,configured=False,disconnectAt=Nothing,failure=Nothing})
    pure (automaticDesktop current updated) {status=fromMaybe (if detachExpired then "Debugger disconnected." else "DAP request timed out; debugger disconnected.") (failure current)}

receive :: Debugger -> Core -> Desktop -> D.Event -> IO Desktop
receive runtime@(Debugger ref _) core d event = do
  s<-readIORef ref
  case event of
    _ | Nothing<-client s -> pure d
    D.Connected -> do
      modifyIORef' ref (\state -> state {connected=True})
      -- Managed THC may still be compiling until the transport becomes ready.
      unless (disconnectAt s/=Nothing) $ do
        send runtime Init "initialize" (object
          ["clientID" .= ("thc-edit"::Text),"clientName" .= ("Turbo Haskell"::Text),"adapterID" .= adapterId s,
           "pathFormat" .= ("path"::Text),"linesStartAt1" .= True,"columnsStartAt1" .= True,
           "supportsVariableType" .= True,"supportsRunInTerminalRequest" .= False,
           "supportsVariablePaging" .= False,"supportsMemoryReferences" .= False,"supportsInvalidatedEvent" .= True])
      pure d
    D.Disconnected reason -> do
      mapM_ D.stopClient (client s)
      modifyIORef' ref (\state -> (invalidate state) {client=Nothing,connected=False,pending=M.empty,ready=False,configured=False,disconnectAt=Nothing,failure=Nothing})
      pure (automaticDesktop s d) {status="DAP: "<>reason}
    D.Notification "initialized" _ -> do
      modifyIORef' ref (\state -> state {ready=True})
      configure runtime
      pure d
    D.Notification "capabilities" body -> do
      modifyIORef' ref (\state -> state {capabilities=merge (capabilities state) (fromMaybe Null (field "capabilities" body))})
      pure d
    D.Notification "stopped" body -> do
      let tid=field "threadId" body
      modifyIORef' ref (\state -> (invalidate state) {stopped=True,thread=tid})
      when (configured s) $ do
        send runtime (Threads False) "threads" (object [])
        forM_ tid (\ident -> send runtime (Stack False) "stackTrace" (stackArguments ident))
      pure (automaticDesktop s d) {status="Stopped: "<>text "reason" body}
    D.Notification "invalidated" body -> do
      let areas=fromMaybe [] (field "areas" body) :: [Text]
          allAreas=null areas || "all" `elem` areas
          threads=allAreas || "threads" `elem` areas
          stacks=threads || "stacks" `elem` areas
      if not (stacks || "variables" `elem` areas) then pure d else do
        modifyIORef' ref (\state -> state {generation=generation state+1,choices=M.empty,variableRefs=M.empty,
          thread=if threads then Nothing else thread state,
          frame=if stacks then Nothing else frame state,frames=if stacks then [] else frames state})
        if threads then send runtime (Threads False) "threads" (object [])
        else when (stacks && stopped s) $ forM_ (thread s) (\tid -> send runtime (Stack False) "stackTrace" (stackArguments tid))
        pure (clearDialog d) {status="Debugger values changed; request scopes again."}
    D.Notification "continued" _ -> modifyIORef' ref invalidate >> pure (automaticDesktop s d) {status="Running..."}
    D.Notification "thread" _ -> when (configured s) (send runtime (Threads False) "threads" (object [])) >> pure d
    D.Notification "terminated" _ -> do
      mapM_ D.stopClient (client s)
      modifyIORef' ref (\state -> (invalidate state) {client=Nothing,connected=False,pending=M.empty,ready=False,configured=False,disconnectAt=Nothing,failure=Nothing})
      pure (automaticDesktop s d) {status="Debug session ended."}
    D.Notification "output" body -> do
      modifyIORef' ref (\state -> state {output=T.takeEnd 16384 (output state<>text "output" body)})
      pure d
    D.Notification "breakpoint" body -> do
      let bp=fromMaybe Null (field "breakpoint" body)
      modifyIORef' ref (\state -> state {breakpoints=M.map (\(source,points) -> (source,map (updateBreakpoint bp) points)) (breakpoints state)})
      pure d
    D.Notification _ _ -> pure d
    D.Response ident result -> case M.lookup ident (pending s) of
      Nothing -> pure d
      Just (kind,epoch,_) -> do
        modifyIORef' ref (\state -> state {pending=M.delete ident (pending state)})
        if (stale kind && epoch/=generation s) || (case kind of Breaks key _ -> M.lookup key (breakRequests s)/=Just ident; _ -> False) then do
          completeInspection kind (Left "Debugger inspection expired; refresh debug_status.")
          pure d
        else case kind of
          Inspection command reply -> do
            forM_ result (recordVariables ref command)
            _<-tryPutMVar reply (result >>= boundedResult)
            pure d
          _ -> case result of
           Left err -> do
             when (kind==Init || kind==Attach || kind==Configure) $ do
               mapM_ D.stopClient (client s)
               modifyIORef' ref (\state -> (invalidate state) {client=Nothing,connected=False,pending=M.empty,ready=False,configured=False,disconnectAt=Nothing,failure=Nothing})
             case kind of
               Control wasStopped | wasStopped -> do
                 modifyIORef' ref (\state -> state {stopped=True})
                 forM_ (thread s) (\tid -> send runtime (Stack False) "stackTrace" (stackArguments tid))
               _ -> pure ()
             pure (automaticDesktop s d) {status="DAP: "<>err}
           Right body -> response runtime core kind body d
  where
    stale kind=case kind of Threads{} -> True; Control{} -> True; Stack{} -> True; Scopes -> True; Variables -> True; Source{} -> True; Inspection{} -> True; _ -> False

-- DAP lazy handles are executable: hdb forces a thunk when its children are
-- requested. Track provenance for both UI and MCP; never guess a reference.
recordVariables :: IORef State -> Text -> Value -> IO ()
recordVariables ref command body
  | command `elem` ["scopes","variables"] = do
      let refs=M.fromListWith (||)
            [(ident,command=="variables" && maybe False (flag "lazy") (field "presentationHint" row))
            | row<-items command body,let ident=integer "variablesReference" row,ident>0]
      modifyIORef' ref (\state -> state {variableRefs=M.unionWith (||) refs (variableRefs state)})
  | otherwise = pure ()

configure :: Debugger -> IO ()
configure runtime@(Debugger ref _) = do
  s<-readIORef ref
  when (ready s && capabilities s/=Null && not (configured s)) $ do
    modifyIORef' ref (\state -> state {configured=True})
    forM_ (M.toList (breakpoints s)) $ \(key,(source,points)) -> sendBreakpoints runtime key source points
    when (not (null (items "exceptionBreakpointFilters" (capabilities s)))) $
      send runtime Exceptions "setExceptionBreakpoints" (object ["filters" .= exceptionFilters s])
    when (flag "supportsConfigurationDoneRequest" (capabilities s)) $
      send runtime Configure "configurationDone" (object [])

response :: Debugger -> Core -> Pending -> Value -> Desktop -> IO Desktop
response runtime@(Debugger ref _) core kind body d = do
  s<-readIORef ref
  case kind of
    Init -> do
      let filters=[text "filter" f | f<-items "exceptionBreakpointFilters" body,flag "default" f]
      modifyIORef' ref (\state -> state {capabilities=body,exceptionFilters=filters})
      let (command,arguments)=startRequest s
      send runtime Attach command arguments
      configure runtime
      pure d {status=if fst (startRequest s)=="launch" then "Launching debugger..." else "Attaching debugger..."}
    Attach -> do
      send runtime (Threads False) "threads" (object [])
      when (stopped s) $ forM_ (thread s) (\tid -> send runtime (Stack False) "stackTrace" (stackArguments tid))
      pure d
    Configure -> pure d
    Breaks key requested -> do
      modifyIORef' ref (\state -> state {breakpoints=M.adjust (\(source,points) -> (source,if map bpLine points==requested then zipWith (\p value -> p {bpResult=value}) points (items "breakpoints" body++repeat Null) else points)) key (breakpoints state)})
      pure d
    Exceptions -> pure d
    Threads showPicker -> do
      let rows=items "threads" body
          tid=case thread s of Just ident | any ((==Just ident).field "id") rows -> Just ident; _ -> listToMaybe rows >>= field "id"
      modifyIORef' ref (\state -> state {thread=tid})
      when (stopped s && thread s==Nothing) $ forM_ tid (\ident -> send runtime (Stack False) "stackTrace" (stackArguments ident))
      if showPicker then showChoices runtime "Threads" "thread" rows (map (text "name") rows) d else pure d
    Stack showPicker -> do
      let rows=items "stackFrames" body
      modifyIORef' ref (\state -> state {frame=listToMaybe rows,frames=rows})
      if showPicker then showChoices runtime "Call stack" "frame" rows (map frameLabel rows) d
      else if not (followSource s) then pure d
      else maybe (pure d {status="Stopped; no source frame supplied."}) (openFrame runtime core False d) (listToMaybe rows)
    Scopes -> do
      recordVariables ref "scopes" body
      let rows=items "scopes" body
      showChoices runtime "Scopes" "expand" rows (map (text "name") rows) d
    Variables -> do
      recordVariables ref "variables" body
      let rows=items "variables" body
      showChoices runtime "Variables" "expand" rows (map variableLabel rows) d
    Source explicit _ | not explicit && not (followSource s) -> pure d
    Source _ selected -> case field "content" body of
      Nothing -> pure d {status="DAP source response has no content."}
      Just content -> do
        let source=fromMaybe Null (field "source" selected)
            title="Source "<>sourceLabel source<>" ["<>tshow (integer "sourceReference" source)<>"]"
            opened=addReadOnly title content d
            bid=maybe (nextId d) bufferId (activeWindow opened)
            styled=opened {buffers=M.adjust (\doc -> doc {documentHighlight=highlightFor (T.unpack (sourceLabel source)) content}) bid (buffers opened)}
        modifyIORef' ref (\state -> state {sources=M.insert bid source (sources state)})
        pure (position selected styled) {status="Stopped in "<>frameLabel selected}
    Control _ -> pure d
    Inspection command reply -> recordVariables ref command body >> tryPutMVar reply (boundedResult body) >> pure d
    Detach -> do
      mapM_ D.stopClient (client s)
      modifyIORef' ref (\state -> (invalidate state) {client=Nothing,connected=False,pending=M.empty,ready=False,configured=False,disconnectAt=Nothing})
      pure (clearDialog d) {status=if managed s || fst (startRequest s)=="launch" then "Debugger disconnected; launched session stopped." else "Debugger disconnected; attached program is not terminated."}

select :: Debugger -> Core -> Text -> Text -> [Text] -> Desktop -> IO Desktop
select runtime@(Debugger ref _) core fullToken action values d = do
  s<-readIORef ref
  let rows=fromMaybe [] (M.lookup fullToken (choices s))
      selected=case values of _:index:_ -> readMaybe (T.unpack index); _ -> Nothing
  case action of
    "remove-breakpoint" | Just chosen<-selected >>= at rows,let key=text "key" chosen,Just (src,points)<-M.lookup key (breakpoints s) -> do
      let remaining=filter ((/=integer "line" chosen).bpLine) points
      modifyIORef' ref (\state -> state {breakpoints=M.insert key (src,remaining) (breakpoints state)})
      when (configured s) (sendBreakpoints runtime key src remaining)
      pure d {status="Breakpoint removed."}
    "thread" | Just chosen<-selected >>= at rows,Just tid<-field "id" chosen -> do
      modifyIORef' ref (\state -> (invalidate state) {stopped=stopped s,thread=Just tid})
      when (stopped s) (send runtime (Stack True) "stackTrace" (stackArguments tid))
      pure d
    "frame" | stopped s,Just chosen<-selected >>= at rows -> do
      modifyIORef' ref (\state -> state {frame=Just chosen,generation=generation state+1,choices=M.empty,variableRefs=M.empty})
      openFrame runtime core True d chosen
    "expand" | stopped s,Just chosen<-selected >>= at rows,let ident=integer "variablesReference" chosen,ident>0 ->
      case M.lookup ident (variableRefs s) of
        Just False -> send runtime Variables "variables" (object ["variablesReference" .= ident]) >> pure d {status="Loading variables..."}
        Just True -> pure d {status="Lazy variable requires explicit evaluation; expansion does not force it."}
        Nothing -> pure d {status="Debugger value expired; request scopes again."}
    "exceptions" -> do
      let filters=items "exceptionBreakpointFilters" (capabilities s)
          selectedFilters=[text "filter" f | (f,"true")<-zip filters (drop 1 values)]
      modifyIORef' ref (\state -> state {exceptionFilters=selectedFilters})
      send runtime Exceptions "setExceptionBreakpoints" (object ["filters" .= selectedFilters])
      pure d {status="Exception breakpoints updated."}
    _ -> pure d {status="No expandable debugger value selected."}

openFrame :: Debugger -> Core -> Bool -> Desktop -> Value -> IO Desktop
openFrame runtime@(Debugger ref _) core explicit d selected = do
  s<-readIORef ref
  let source=fromMaybe Null (field "source" selected)
      reference=integer "sourceReference" source
      path=text "path" source
      local=if isAbsolute (T.unpack path) then T.unpack path else root s </> T.unpack path
  if reference>0 then send runtime (Source explicit selected) "source" (object ["source" .= source,"sourceReference" .= reference]) >> pure d
  else do
    exists<-if T.null path then pure False else doesFileExist local
    if not exists then pure d {status="Stopped: source is unavailable ("<>sourceLabel source<>")."}
    else do
      canonical<-canonicalizePath local
      (_,opened)<-core d [ReadPath canonical]
      pure $ if fmap filePath (activeDocument opened >>= documentFile)==Just canonical
        then (position selected opened) {status=if maybe False (dirty.documentBuffer) (activeDocument opened)
              then "Stopped; unsaved text may differ from the running source." else "Stopped in "<>frameLabel selected}
        else opened

-- Docs: docs/site/screenshots/debug-step.png (docs/running.md) shows the live stopped source.
position :: Value -> Desktop -> Desktop
position selected d
  | row>0 = moveTo False (L.positionOffset (activeText d) (row-1,max 0 (integer "column" selected-1))) d
  | otherwise = d
  where row=integer "line" selected

toggleBreakpoint :: Debugger -> Desktop -> IO Desktop
toggleBreakpoint runtime@(Debugger ref _) d = do
  s<-readIORef ref
  case (activeWindow d,activeDocument d) of
    (Just _,Just doc) | byteMode (documentBuffer doc) -> pure d {status="Breakpoints require source text; leave hex mode first."}
    (Just window,Just doc) -> do
      source<-case documentFile doc of
        Just file -> do path<-canonicalizePath (filePath file); pure (Just (object ["path" .= path]))
        Nothing -> pure (M.lookup (bufferId window) (sources s))
      case source of
        Nothing -> pure d {status="Choose a source file or debugger source first."}
        Just src -> do
          let key=sourceKey src
              row=1+fst (lineColumn (contents (documentBuffer doc)) (caret (selection window)))
              old=maybe [] snd (M.lookup key (breakpoints s))
              removing=any ((==row).bpLine) old
              points=if removing then filter ((/=row).bpLine) old else old++[Breakpoint row Null]
          modifyIORef' ref (\state -> state {breakpoints=M.insert key (src,points) (breakpoints state),breakModified=M.insert key (dirty (documentBuffer doc)) (breakModified state)})
          when (configured s) (sendBreakpoints runtime key src points)
          pure d {status=if removing then "Breakpoint removed." else "Breakpoint requested at line "<>tshow row<>if dirty (documentBuffer doc) then "; source has unsaved changes." else "."}
    _ -> pure d {status="Choose a source file first."}

sendBreakpoints :: Debugger -> Text -> Value -> [Breakpoint] -> IO ()
sendBreakpoints runtime@(Debugger ref _) key source points = do
  s<-readIORef ref
  send runtime (Breaks key (map bpLine points)) "setBreakpoints"
    (object ["source" .= source,"breakpoints" .= [object ["line" .= bpLine p] | p<-points],
      "sourceModified" .= M.findWithDefault False key (breakModified s)])

allBreakpoints :: State -> [(Text,Value,Breakpoint)]
allBreakpoints s=[(key,source,bp) | (key,(source,points))<-M.toList (breakpoints s),bp<-points]
updateBreakpoint :: Value -> Breakpoint -> Breakpoint
updateBreakpoint value bp | Just ident<-(field "id" value :: Maybe Int),field "id" (bpResult bp)==Just ident = bp {bpResult=value}
                          | otherwise = bp

persistentBreakpoints :: State -> M.Map Text (Value,[Breakpoint])
persistentBreakpoints = M.map (\(src,points) -> (src,map (\bp -> bp {bpResult=Null}) points)) .
  M.filter (\(src,_) -> integer "sourceReference" src==0 && not (T.null (text "path" src))) . breakpoints

-- Docs: docs/site/screenshots/debug-stack.png (docs/running.md) shows the live frame picker.
showChoices :: Debugger -> Text -> Text -> [Value] -> [Text] -> Desktop -> IO Desktop
showChoices (Debugger ref _) title action rows labels d = do
  s<-readIORef ref
  let key=token s action
      shown=chooser title key labels d
  when (dialog d==Nothing && not (null rows)) $
    modifyIORef' ref (\state -> state {choices=M.singleton key rows,choiceId=choiceId state+1})
  pure shown

chooser :: Text -> Text -> [Text] -> Desktop -> Desktop
chooser title action rows d
  | null rows = d {status=title<>" is empty."}
  | dialog d/=Nothing = d {status=title<>" ready; close the current dialog and request it again."}
  | otherwise = d {dialog=Just (Dialog title (DebugDialog action) [ListBox title rows 0] 0 ["Open","Cancel"] [])}
-- Background protocol events keep the editor's existing modal and focus.
-- Explicit view requests still use the normal source/picker presentation path.
automaticDesktop :: State -> Desktop -> Desktop
automaticDesktop s d=if followSource s then clearDialog d else d

clearDialog :: Desktop -> Desktop
clearDialog d = case dialog d of Just dg | DebugDialog{}<-purpose dg -> d {dialog=Nothing}; _ -> d
sourceKey :: Value -> Text
sourceKey source = text "path" source<>"#"<>tshow (integer "sourceReference" source)
sourceLabel :: Value -> Text
sourceLabel source=fromMaybe (fromMaybe "Unavailable source" (field "name" source)) (field "path" source)
frameLabel :: Value -> Text
frameLabel value=text "name" value<>"  "<>maybe "" (T.pack . takeFileName . T.unpack . sourceLabel) (field "source" value)<>if integer "line" value>0 then ":"<>tshow (integer "line" value) else ""
variableLabel :: Value -> Text
variableLabel value=(if integer "variablesReference" value>0 then "+ " else "  ")<>text "name" value<>" = "<>text "value" value<>
  (if T.null (text "type" value) then "" else " : "<>text "type" value)
token :: State -> Text -> Text
token s action="select:"<>tshow (generation s)<>":"<>tshow (choiceId s)<>":"<>action
parseToken :: Text -> Maybe (Int,Text)
parseToken value=case T.splitOn ":" value of ["select",epoch,_,action] -> (,action) <$> readMaybe (T.unpack epoch); _ -> Nothing
stackArguments :: Int -> Value
stackArguments tid=object ["threadId" .= tid,"startFrame" .= (0::Int),"levels" .= (200::Int)]
field :: FromJSON a => Text -> Value -> Maybe a
field key=parseMaybe (withObject "object" (\o -> o .: K.fromText key))
text :: Text -> Value -> Text
text key=fromMaybe "" . field key
integer :: Text -> Value -> Int
integer key=fromMaybe 0 . field key
flag :: Text -> Value -> Bool
flag key=fromMaybe False . field key
items :: Text -> Value -> [Value]
items key=fromMaybe [] . field key
at :: [a] -> Int -> Maybe a
at values index | index<0=Nothing | otherwise=listToMaybe (drop index values)
tshow :: Show a => a -> Text
tshow=T.pack.show
merge :: Value -> Value -> Value
merge (Object old) (Object new)=Object (KM.union new old)
merge old _=old
