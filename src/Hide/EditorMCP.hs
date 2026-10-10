-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.EditorMCP
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- JSON-RPC/MCP bridge between agent subprocesses and the owning editor session.
--
-- A private endpoint exposes the current desktop without claiming the display.
-- Tool initiation and reply waiting are separate phases so HLS, DAP and human
-- approvals can continue while a request is pending. Actor-bound routes expose
-- only their supplied tools, with no fallback into ordinary desktop reads.
module Hide.EditorMCP (editorResponse, editorResponseWith, editorResponseOnly, rpcError, builtinTools, builtinTool, debugTools, editorServers, editorServersFor, editorServersAt, editorEndpointsAt, runEditorMCP, runEditorMCPWithHandles, runEditorMCPWithToken, openEditorFiles, readMCPLine) where

import Hide.Plugin.Agent (ProviderEndpoint(..))
import Hide.Plugin.Provider (ProviderLaunch(..))
import Hide.Sidebar
import Control.Exception (bracket, try, IOException, finally, catch, mask, throwIO)
import Control.Concurrent.Async (Async, async, cancel, wait, withAsync, AsyncCancelled(..))
import Control.Concurrent.MVar
import Control.Monad (unless, forM_)
import Data.Aeson
import qualified Data.Aeson.Key
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Paths_hide (getDataFileName)
import System.Environment (lookupEnv, getExecutablePath)
import System.Directory (makeAbsolute)
import System.IO (Handle, stdin, stdout, hClose, hFlush, hSetBinaryMode)
import Hide.Buffer
import qualified Hide.Plugin.Window as W
import Hide.Files (filePath)
import Hide.GuestAccess (protectedWindow)
import Hide.Model
import Hide.BufferView (BufferView(..))
import Hide.Protocol (WirePacket(..), readPacket, writePacket)
import Hide.RemoteEndpoint (sessionEndpoint, connectEndpointWithShutdown)

-- ACP starts this small stdio bridge beside its provider. The private session
-- endpoint supplies a current snapshot without taking over the display.
editorServers :: IO [Value]
editorServers = editorServersFor Nothing

editorServersFor :: Maybe T.Text -> IO [Value]
editorServersFor token = lookupEnv "THC_EDIT_SESSION" >>= maybe (pure []) (\ident -> editorServersAt ident token)

-- | Describe a stdio bridge using the current executable and a private session ID.
-- An optional capability travels in the child environment, not argv.
editorServersAt :: String -> Maybe T.Text -> IO [Value]
editorServersAt ident token = do
  endpoints<-editorEndpointsAt ident token
  pure [object ["name" .= endpointName endpoint,"command" .= executable launch,"args" .= arguments launch,
    "env" .= [object ["name" .= name,"value" .= value] | (name,value)<-environment launch]]
    | endpoint<-endpoints,let launch=endpointLaunch endpoint]

-- | The actual linked provider acquisition input for this private editor bridge.
editorEndpointsAt :: String -> Maybe T.Text -> IO [ProviderEndpoint]
editorEndpointsAt ident token=do
  executablePath<-getExecutablePath
  pure [ProviderEndpoint "editor" (ProviderLaunch executablePath ["--mcp-editor",ident]
    [("THC_EDIT_MCP_TOKEN",T.unpack value) | Just value<-[token]])]

runEditorMCP :: String -> IO ()
runEditorMCP ident = do
  token <- fmap T.pack <$> lookupEnv "THC_EDIT_MCP_TOKEN"
  runEditorMCPWithToken ident token stdin stdout

runEditorMCPWithHandles :: String -> Handle -> Handle -> IO ()
runEditorMCPWithHandles ident = runEditorMCPWithToken ident Nothing

-- | Serve bounded newline-delimited MCP over supplied handles and the session endpoint.
-- The token selects the host-authorized actor route; callers retain handle ownership.
runEditorMCPWithToken :: String -> Maybe T.Text -> Handle -> Handle -> IO ()
runEditorMCPWithToken ident token input outputHandle = do
  path <- editorEndpoint ident token
  hSetBinaryMode input True
  hSetBinaryMode outputHandle True
  outputLock<-newMVar ()
  pendingCalls<-newMVar M.empty
  let output value=withMVar outputLock $ \_ -> BL.hPut outputHandle (encode value<>"\n") >> hFlush outputHandle
      invoke active request=inspectEditor path token active request >>= mapM_ output
      dispatch request@(Object fields)
        | KM.lookup "method" fields==Just (String "notifications/cancelled") = do
            let wanted=KM.lookup "params" fields >>= \params -> case params of Object o -> KM.lookup "requestId" o; _ -> Nothing
            worker<-maybe (pure Nothing) (\key -> M.lookup (encode key) <$> readMVar pendingCalls) wanted
            mapM_ stopInspection worker
        | Just requestId<-KM.lookup "id" fields = do
            let key=encode requestId
            accepted<-modifyMVar pendingCalls $ \calls ->
              if M.member key calls || M.size calls>=32 then pure (calls,False) else do
                active<-newMVar (False,pure ())
                worker<-async $ (invoke active request `catch` (\(err::IOException) -> do
                    stopped<-fst <$> readMVar active
                    unless stopped (output (rpcError requestId (-32603) (T.pack (show err))))))
                  `finally` modifyMVar_ pendingCalls (pure . M.delete key)
                pure (M.insert key (worker,active) calls,True)
            unless accepted (output (rpcError requestId (-32600) "Duplicate request ID or more than 32 pending requests"))
        | KM.lookup "jsonrpc" fields==Just (String "2.0"), Just (String _)<-KM.lookup "method" fields = pure ()
      dispatch request=newMVar (False,pure ()) >>= \active -> invoke active request
      loop pending=do
        incoming<-readMCPLine input pending
        case incoming of
          Nothing -> pure ()
          Just (line,rest) -> do
            either (const (output (rpcError Null (-32700) "Invalid JSON"))) dispatch (eitherDecodeStrict' line)
            loop rest
      cleanup=readMVar pendingCalls >>= mapM_ stopInspection . M.elems
  loop BS.empty `finally` cleanup

-- | Open existing files in a running session without attaching a display.
-- Paths are resolved in the calling shell, and each operation waits for the
-- host's acknowledgement. Inherited actor identity and host permissions remain
-- in force. Refusal or disconnection raises an 'IOException'; there is no
-- fallback to a fresh editor. Interrupting the caller cancels its pending call.
openEditorFiles :: String -> [FilePath] -> IO ()
openEditorFiles ident files=do
  token<-fmap T.pack <$> lookupEnv "THC_EDIT_MCP_TOKEN"
  endpoint<-editorEndpoint ident token
  forM_ files $ \file->do
    absolute<-makeAbsolute file
    active<-newMVar (False,pure ())
    let request=object ["jsonrpc" .= ("2.0"::T.Text),"id" .= (1::Int),"method" .= ("tools/call"::T.Text),
          "params" .= object ["name" .= ("editor_file"::T.Text),"arguments" .=
            object ["action" .= ("open"::T.Text),"path" .= absolute]]]
    response<-withAsync (inspectEditor endpoint token active request) $ \worker->
      wait worker `finally` stopInspection (worker,active)
    let outcome=maybe (Left "Editor did not acknowledge the file open.") (parseEither opened) response
    either (ioError . userError . (("Cannot open "++file++": ")++)) pure outcome
  where
    opened=withObject "file-open reply" $ \reply->do
      version<-reply .: "jsonrpc"
      number<-reply .: "id"
      unless (version==("2.0"::T.Text) && number==(1::Int)) (fail "Unexpected editor reply.")
      err<-reply .:? "error"
      case err of
        Just value->withObject "editor error" (\fields->fields .: "message" >>= fail) value
        Nothing->do
          result<-reply .: "result"
          failed<-result .: "isError"
          if not failed then pure () else do
            content<-result .: "content"
            messages<-mapM (withObject "tool error" (.: "text")) content
            fail (T.unpack (T.intercalate "\n" messages))

editorEndpoint :: String -> Maybe T.Text -> IO FilePath
editorEndpoint ident token=do
  unless (maybe True (\value->not (T.null value) && T.length value<=256) token)
    (ioError (userError "Invalid editor MCP token"))
  sessionEndpoint ident

-- A cancellation can precede connection setup. The latch prevents a late
-- connection from starting work, and serializes socket wakeup with close.
inspectEditor :: FilePath -> Maybe T.Text -> MVar (Bool,IO ()) -> Value -> IO (Maybe Value)
inspectEditor path token active request=mask $ \restore->
  let close (connection,_)=do
        modifyMVar_ active (\(stopped,_)->pure (stopped,pure ()))
        hClose connection
  in bracket (connectEndpointWithShutdown path) close $ \(connection,shutdown)->do
    stopped<-modifyMVar active (\(stopped,_)->pure ((stopped,shutdown),stopped))
    if stopped then shutdown >> throwIO AsyncCancelled else restore $ do
      writePacket connection (JsonPacket (object (["type" .= ("inspect"::T.Text),"request" .= request]++["agentToken" .= value | Just value<-[token]])))
      reply<-readPacket connection
      case reply of
        Just (JsonPacket Null)->pure Nothing
        Just (JsonPacket response)->pure (Just response)
        _->ioError (userError "Editor connection ended")

stopInspection :: (Async a,MVar (Bool,IO ())) -> IO ()
stopInspection (worker,active)=do
  modifyMVar_ active (\(_,shutdown)->shutdown >> pure (True,pure ()))
  cancel worker

-- | Read a size-bounded JSON-RPC line, retaining bytes after its newline.
-- Enforce the limit even when a sender never terminates the line.
readMCPLine :: Handle -> BS.ByteString -> IO (Maybe (BS.ByteString,BS.ByteString))
readMCPLine handle = collect 0 []
  where
    bound size=unless (size<=1048576) (ioError (userError "MCP request exceeds 1 MiB"))
    collect size chunks bytes = case B8.elemIndex '\n' bytes of
      Just n -> do
        bound (size+n)
        pure (Just (BS.concat (reverse (BS.take n bytes:chunks)),BS.drop (n+1) bytes))
      Nothing -> do
        let total=size+BS.length bytes
        bound total
        more<-BS.hGetSome handle 8192
        if BS.null more then pure (if total==0 then Nothing else Just (BS.concat (reverse (bytes:chunks)),BS.empty))
        else collect total (bytes:chunks) more

rpcError :: Value -> Int -> T.Text -> Value
rpcError ident code text=object ["jsonrpc" .= ("2.0"::T.Text),"id" .= ident,"error" .= object ["code" .= code,"message" .= text]]

editorResponse :: Desktop -> Value -> Maybe Value
editorResponse = responseWithTools []

responseWithTools :: [Value] -> Desktop -> Value -> Maybe Value
responseWithTools = responseTools True

responseTools :: Bool -> [Value] -> Desktop -> Value -> Maybe Value
responseTools includeBuiltins extra desktop request = case request of
  Object fields | KM.lookup "jsonrpc" fields==Just (String "2.0"), Just (String method)<-KM.lookup "method" fields ->
    case KM.lookup "id" fields of
      Nothing -> Nothing
      Just ident -> Just $ case dispatch method (fromMaybe (object []) (KM.lookup "params" fields)) of
        Left (code,err) -> rpcError ident code err
        Right reply -> object ["jsonrpc" .= ("2.0"::T.Text),"id" .= ident,"result" .= reply]
  _ -> Just (rpcError Null (-32600) "Invalid JSON-RPC request")
  where
    dispatch "initialize" params = do
      requested <- parameters (withObject "initialize" (.: "protocolVersion")) params
      let version=if requested `elem` (["2024-11-05","2025-03-26","2025-06-18","2025-11-25"]::[T.Text]) then requested else "2025-11-25"
      pure (object ["protocolVersion" .= version,"capabilities" .= object ["tools" .= object [],"resources" .= object []],
        "serverInfo" .= object ["name" .= ("hide"::T.Text),"version" .= ("0.1.0"::T.Text)],
        "instructions" .= ("Work with the live editor session. Buffers include unsaved edits; IDs belong to this session. Lines and columns start at 1; byte offsets start at 0. Discover tool schemas before use. Load docs/agent-skills.md with docs_read for task workflows; docs/agent-tools.md lists operations by category. Debugging guidance is also available at hide://debugging. Use environment_get/environment_set to diagnose and fix subprocess paths inside the editor; restart only the affected process, not the editor. Execution and navigation tools change the live session; inspect their annotations."::T.Text)])
    dispatch "ping" _=Right (object [])
    dispatch "tools/list" _=Right (object ["tools" .= (filter (\entry -> includeBuiltins && not (any (sameTool entry) extra)) tools++extra)])
    dispatch "resources/list" _=Right (object ["resources" .= [object ["uri" .= skillURI,"name" .= ("Debugging with the editor"::T.Text),"mimeType" .= ("text/markdown"::T.Text),"description" .= ("A workflow for source breakpoints, stepping, stack and variable inspection."::T.Text)]]])
    dispatch "tools/call" params = do
      (name,args)<-parameters (withObject "tool call" $ \o -> (,) <$> o .: "name" <*> o .:? "arguments" .!= object []) params
      case if includeBuiltins then builtinTool desktop name args else Left "Unknown tool" of
        Left err -> pure (toolResult False (Left err))
        Right value -> pure (toolResult False (Right value))
    dispatch _ _=Left (-32601,"Method not found")
    parameters parser = either (Left . (-32602,) . T.pack) Right . parseEither parser
    sameTool (Object a) (Object b)=KM.lookup "name" a==KM.lookup "name" b
    sameTool _ _=False

builtinTools :: [Value]
builtinTools=tools

-- | Read public desktop metadata or bounded buffer content after privacy filtering.
builtinTool :: Desktop -> T.Text -> Value -> Either T.Text Value
builtinTool desktop=tool
  where
    tool :: T.Text -> Value -> Either T.Text Value
    tool "list_windows" _=Right (object ["windows" .= (map window (windows desktop)++panels)])
    tool "read_selection" args = parseArgs (withObject "read_selection" (.:? "windowId")) args >>= \wanted -> do
      w <- maybe (maybe (Left "No active window") Right (activeWindow desktop))
        (\ident -> maybe (Left "Window not found") Right (findWindow ident)) wanted
      doc <- maybe (Left "Buffer not found") Right (windowDocument (buffers desktop) w)
      unless (not (protectedWindow desktop w)) (Left "Selections from private conversation or approval buffers are unavailable.")
      (range,content,space)<-if bufferView w==MarkdownView then case windowMarkdown desktop w of
        Nothing->Left "Markdown view is still preparing."
        Just (_,rendered,_)->Right (selection (displayWindow w),rendered,"rendered-markdown"::T.Text)
        else Right (selection w,bufferContent (documentBuffer doc),"source")
      -- Clamp and cap using cached measures before materializing the selection.
      -- Truncation depends on its extent, never a full selected-text traversal.
      let clip=max 0 . min (contentLength content)
          (a,z)=ordered range
          start=clip a
          count=clip z-start
          text=contentSlice content start (min 131072 count)
      Right (object ["windowId" .= windowId w,"bufferId" .= bufferId w,"anchor" .= anchor range,
        "caret" .= caret range,"coordinateSpace" .= space,"text" .= text,"truncated" .= (count>131072)])
    tool _ _=Left "Unknown editor tool"
    parseArgs :: (Value -> Parser a) -> Value -> Either T.Text a
    parseArgs parser=either (Left . T.pack) Right . parseEither parser
    findWindow ident=case filter ((==ident).windowId) (windows desktop) of w:_->Just w; _->Nothing
    window w=object ["windowId" .= windowId w,"number" .= windowNumber w,"bufferId" .= bufferId w,
      "title" .= readableWindowTitle w,
      "kind" .= (case windowContent w of SourceContent _->"source"::T.Text; PluginContent _->"plugin"),
      "active" .= (fmap windowId (activeWindow desktop)==Just (windowId w)),"bounds" .= rect (bounds w)]
    readableWindowTitle w=case windowContent w of
      SourceContent _->maybe "[private]" title (windowDocument (buffers desktop) w)
      PluginContent reference->case M.lookup reference (pluginWindows desktop) of
        Just prepared | W.preparedWindowDisclosure prepared==W.ReadableWindow->W.preparedWindowTitle prepared
        _->"[private]"
    panels=[object ["kind" .= ("files"::T.Text),"title" .= ("Files"::T.Text),"path" .= treeRoot tree] | Just tree<-[sideTree desktop]]
      ++[object ["kind" .= ("messages"::T.Text),"title" .= ("Messages"::T.Text),"bounds" .= rect (problemsRect desktop)] | problemsVisible desktop]
    rect (Rect x y w h)=object ["x" .= x,"y" .= y,"width" .= w,"height" .= h]
    title doc | privateDocument desktop doc="[private]"
              | otherwise=fromMaybe (maybe "Untitled" (T.pack . filePath) (documentFile doc)) (documentLabel doc)
-- | Dispatch with the desktop locked and return a reply continuation.
-- Wait for that continuation only after releasing the desktop lock.
editorResponseWith :: [Value]
  -> (Desktop -> T.Text -> Value -> IO (Desktop, IO (Either T.Text Value)))
  -> Desktop -> Value -> IO (Desktop, IO (Maybe Value))
editorResponseWith = editorResponseUsing True

-- | Dispatch only explicitly supplied tools, with no built-in desktop-read fallback.
editorResponseOnly :: [Value]
  -> (Desktop -> T.Text -> Value -> IO (Desktop, IO (Either T.Text Value)))
  -> Desktop -> Value -> IO (Desktop, IO (Maybe Value))
editorResponseOnly = editorResponseUsing False

editorResponseUsing :: Bool -> [Value]
  -> (Desktop -> T.Text -> Value -> IO (Desktop, IO (Either T.Text Value)))
  -> Desktop -> Value -> IO (Desktop, IO (Maybe Value))
editorResponseUsing includeBuiltins extra execute desktop request = case request of
  Object fields | KM.lookup "jsonrpc" fields==Just (String "2.0")
    , Just ident<-KM.lookup "id" fields
    , Just (String method)<-KM.lookup "method" fields ->
      let params=fromMaybe (object []) (KM.lookup "params" fields)
          reply value=Just (object ["jsonrpc" .= ("2.0"::T.Text),"id" .= ident,"result" .= value])
          bad err=pure (desktop,pure (Just (rpcError ident (-32602) (T.pack err))))
      in case method of
        "tools/call" -> case parseEither (withObject "tool call" $ \o -> (,) <$> o .: "name" <*> o .:? "arguments" .!= object []) params of
          Left err -> bad err
          Right (name,args) | Just descriptor<-find (named name) extra
            , Left err<-validateArguments descriptor args -> pure (desktop,pure (reply (toolResult False (Left err))))
          Right (name,args) | Just descriptor<-find (named name) extra -> do
            started<-try (execute desktop name args)
            case started of
              Left (err::IOException) -> pure (desktop,pure (reply (toolResult False (Left (T.pack (show err))))))
              Right (updated,finish) -> pure (updated, do
                result<-try finish
                pure (reply (case either (Left . T.pack . show) id (result::Either IOException (Either T.Text Value)) of
                  Right value | name=="editor_screen" -> value
                  outcome -> toolResult (structured descriptor) outcome)))
          _ -> fallback
        "resources/read" -> case parseEither (withObject "resource" (.: "uri")) params of
          Left err -> bad err
          Right uri | uri==skillURI -> pure (desktop,do
            loaded<-try (getDataFileName "skills/debug-editor/SKILL.md" >>= TIO.readFile)
            pure $ case loaded of
              Left (err::IOException) -> Just (rpcError ident (-32603) (T.pack (show err)))
              Right content -> reply (object ["contents" .= [object ["uri" .= skillURI,"mimeType" .= ("text/markdown"::T.Text),"text" .= content]]]))
          Right _ -> pure (desktop,pure (Just (rpcError ident (-32002) "Resource not found")))
        _ -> fallback
  _ -> fallback
  where
    fallback=pure (desktop,pure (responseTools includeBuiltins extra desktop request))
    named name (Object fields)=KM.lookup "name" fields==Just (String name)
    named _ _=False
    structured (Object fields)=KM.member "outputSchema" fields
    structured _=False

-- The schema is also enforced at dispatch, so misspelled optional arguments
-- cannot silently turn a requested operation into a different one.
validateArguments :: Value -> Value -> Either T.Text ()
validateArguments (Object descriptor) (Object args)=case KM.lookup "inputSchema" descriptor of
  Just (Object schema) -> do
    case KM.lookup "properties" schema of
      Just (Object properties) -> unless (all (`KM.member` properties) (KM.keys args)) (Left "Unknown tool argument")
      _ -> pure ()
    case KM.lookup "required" schema of
      Just (Array required) -> unless (all (\item -> case item of String key -> KM.member (Data.Aeson.Key.fromText key) args; _ -> False) required) (Left "Missing required tool argument")
      _ -> pure ()
  _ -> Right ()
validateArguments _ _=Left "Tool arguments must be an object"

skillURI :: T.Text
skillURI="hide://debugging"

toolResult :: Bool -> Either T.Text Value -> Value
toolResult _ (Right value) | BL.length (BL.take 4194305 (encode value))>4194304=toolResult False (Left "Tool result exceeds 4 MiB; request a smaller page.")
toolResult structured outcome=object (["isError" .= either (const True) (const False) outcome,
  "content" .= [object ["type" .= ("text"::T.Text),"text" .= TE.decodeUtf8 (BL.toStrict (encode (either String id outcome)))]]]
  ++["structuredContent" .= value | structured,Right value@(Object _)<-[outcome]])

debugTools :: [Value]
debugTools =
  [ describe "debug_status" "Read connection, stop state, generation, selected frame, follow mode, capabilities, breakpoints and recent output." True [] []
  , describe "debug_present" "Set automatic source following (initially true), or reveal source/stack/scopes/output from the shared session. Omitted follow is unchanged; false keeps asynchronous stops in the background. Any view requires the current generation; source/stack/scopes also require a stopped target. follow=true resumes future automatic following; use view=source to reveal now. Presentation does not resume, relaunch or advance generation." False [] [("follow",object ["type" .= ("boolean"::T.Text)]),("view",enum ["source","stack","scopes","output"]),("generation",integer)]
  , describe "debug_launch" "Launch the configured THC target, or a general DAP adapter JSON configuration. Fails if a session is already active." False [] [("adapterConfig",str),("port",integer)]
  , describe "debug_attach" "Attach to a loopback DAP adapter (127.0.0.1:4711 by default)." False [] [("host",str),("port",integer)]
  , describe "debug_control" "Continue, step, pause or disconnect the current debug generation. Accepted does not mean the next stop has occurred; inspect status afterwards." False ["generation","command"] [("generation",integer),("command",enum ["continue","next","stepIn","stepOut","pause","disconnect"])]
  , describe "debug_set_breakpoints" "Replace source breakpoints for an open buffer, using 1-based lines. Reports pending/verified state; unsaved buffers are marked sourceModified." False ["generation","bufferId","lines"] [("generation",integer),("bufferId",integer),("lines",object ["type" .= ("array"::T.Text),"items" .= object ["type" .= ("integer"::T.Text),"minimum" .= (1::Int)],"maxItems" .= (1000::Int)])]
  , describe "debug_inspect" "Inspect threads, stackTrace, scopes, variables, source or exceptionInfo using generation-scoped handles. Stack and variables require a stopped target. Variable references must come from current scopes/variables responses; lazy handles require explicit evaluation and cannot be expanded here. start/count page results (100 default, 1000 max)." True ["generation","request"] ([("generation",integer),("request",enum ["threads","stackTrace","scopes","variables","source","exceptionInfo"]) ]++[(key,integer) | key<-["threadId","frameId","variablesReference","sourceReference","start","count"]])
  ]
  where
    str=object ["type" .= ("string"::T.Text)]
    integer=object ["type" .= ("integer"::T.Text)]
    enum values=object ["type" .= ("string"::T.Text),"enum" .= (values::[T.Text])]
    describe :: T.Text -> T.Text -> Bool -> [T.Text] -> [(T.Text,Value)] -> Value
    describe name description readOnly required properties=object ["name" .= name,"description" .= description,
      "inputSchema" .= object (["type" .= ("object"::T.Text),"properties" .= object [Data.Aeson.Key.fromText key .= value | (key,value)<-properties],"required" .= required,"additionalProperties" .= False]++
        ["dependentRequired" .= object ["view" .= ["generation"::T.Text]] | name=="debug_present"]),
      "annotations" .= object ["readOnlyHint" .= readOnly,"destructiveHint" .= not readOnly,"openWorldHint" .= not readOnly]]

tools :: [Value]
tools =
  [ describe "list_windows" "List editor window IDs, titles, buffer IDs, geometry, active window and side panels." []
  , describe "read_selection" "Read up to 131072 selected characters and cursor offsets in an editor window; truncated reports a larger selection. coordinateSpace distinguishes source from rendered-markdown offsets; a pending Markdown view refuses the read. Defaults to the active window." [("windowId","integer")]
  ]
  where
    describe :: T.Text -> T.Text -> [(T.Text,T.Text)] -> Value
    describe name description properties=object ["name" .= name,"description" .= description,
      "inputSchema" .= object ["type" .= ("object"::T.Text),"properties" .= object [fromString key .= object ["type" .= typ] | (key,typ)<-properties],"additionalProperties" .= False],
      "annotations" .= object ["readOnlyHint" .= True,"destructiveHint" .= False,"openWorldHint" .= False]]
    fromString=Data.Aeson.Key.fromText
