{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.EditorMCP (editorResponse, editorResponseWith, editorResponseOnly, rpcError, builtinTools, builtinTool, debugTools, editorServers, editorServersFor, editorServersAt, runEditorMCP, runEditorMCPWithHandles, runEditorMCPWithToken, readMCPLine) where

import Control.Exception (bracket, try, IOException, finally, catch, mask, throwIO)
import Control.Concurrent.Async (async, cancel, AsyncCancelled(..))
import Control.Concurrent.MVar
import Control.Monad (unless)
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
import Paths_thc_edit (getDataFileName)
import Numeric (showHex)
import System.Environment (lookupEnv, getExecutablePath)
import System.IO (Handle, stdin, stdout, hClose, hFlush, hSetBinaryMode)
import THC.Edit.Buffer
import THC.Edit.Files (filePath)
import THC.Edit.GuestAccess (protectedBuffer, privateDocument, sanitizedBuffer)
import THC.Edit.Model
import THC.Edit.Protocol (WirePacket(..), readPacket, writePacket)
import THC.Edit.RemoteEndpoint (sessionEndpoint, connectEndpointWithShutdown)

-- ACP starts this small stdio bridge beside its provider. The private session
-- endpoint supplies a current snapshot without taking over the display.
editorServers :: IO [Value]
editorServers = editorServersFor Nothing

editorServersFor :: Maybe T.Text -> IO [Value]
editorServersFor token = lookupEnv "THC_EDIT_SESSION" >>= maybe (pure []) (\ident -> editorServersAt ident token)

editorServersAt :: String -> Maybe T.Text -> IO [Value]
editorServersAt ident token = do
  executable <- getExecutablePath
  pure [object ["name" .= ("editor"::T.Text),"command" .= executable,
    "args" .= ["--mcp-editor",ident],"env" .=
      [object ["name" .= ("THC_EDIT_MCP_TOKEN"::T.Text),"value" .= value] | Just value <- [token]]]]

runEditorMCP :: String -> IO ()
runEditorMCP ident = do
  token <- fmap T.pack <$> lookupEnv "THC_EDIT_MCP_TOKEN"
  runEditorMCPWithToken ident token stdin stdout

runEditorMCPWithHandles :: String -> Handle -> Handle -> IO ()
runEditorMCPWithHandles ident = runEditorMCPWithToken ident Nothing

runEditorMCPWithToken :: String -> Maybe T.Text -> Handle -> Handle -> IO ()
runEditorMCPWithToken ident token input outputHandle = do
  unless (maybe True (\value -> not (T.null value) && T.length value<=256) token)
    (ioError (userError "Invalid editor MCP token"))
  path <- sessionEndpoint ident
  hSetBinaryMode input True
  hSetBinaryMode outputHandle True
  outputLock<-newMVar ()
  pendingCalls<-newMVar M.empty
  let output value=withMVar outputLock $ \_ -> BL.hPut outputHandle (encode value<>"\n") >> hFlush outputHandle
      -- A cancellation can precede connection setup. The latch prevents a
      -- late connection from starting work, and serializes wakeup with close.
      invoke active request=mask $ \restore ->
        let close (connection,_)=do
              modifyMVar_ active (\(stopped,_) -> pure (stopped,pure ()))
              hClose connection
        in bracket (connectEndpointWithShutdown path) close $ \(connection,shutdown) -> do
          stopped<-modifyMVar active (\(stopped,_) -> pure ((stopped,shutdown),stopped))
          if stopped then shutdown >> throwIO AsyncCancelled else restore $ do
            writePacket connection (JsonPacket (object (["type" .= ("inspect"::T.Text),"request" .= request]++["agentToken" .= value | Just value<-[token]])))
            reply <- readPacket connection
            case reply of
              Just (JsonPacket Null) -> pure ()
              Just (JsonPacket response) -> output response
              _ -> ioError (userError "Editor connection ended")
      stop (worker,active)=do
        modifyMVar_ active (\(_,shutdown) -> shutdown >> pure (True,pure ()))
        cancel worker
      dispatch request@(Object fields)
        | KM.lookup "method" fields==Just (String "notifications/cancelled") = do
            let wanted=KM.lookup "params" fields >>= \params -> case params of Object o -> KM.lookup "requestId" o; _ -> Nothing
            worker<-maybe (pure Nothing) (\key -> M.lookup (encode key) <$> readMVar pendingCalls) wanted
            mapM_ stop worker
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
      cleanup=readMVar pendingCalls >>= mapM_ stop . M.elems
  loop BS.empty `finally` cleanup

-- Enforce the limit while reading, including requests without a terminating
-- newline. Keep bytes after a newline for the next JSON-RPC message.
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
        "serverInfo" .= object ["name" .= ("thc-edit"::T.Text),"version" .= ("0.1.0"::T.Text)],
        "instructions" .= ("Work with the live editor session. Buffers include unsaved edits; IDs belong to this session. Lines and columns start at 1; byte offsets start at 0. Discover tool schemas before use. Load docs/agent-skills.md with docs_read for task workflows; docs/agent-tools.md lists operations by category. Debugging guidance is also available at thc-edit://debugging. Execution and navigation tools change the live session; inspect their annotations."::T.Text)])
    dispatch "ping" _=Right (object [])
    dispatch "tools/list" _=Right (object ["tools" .= (filter (\entry -> includeBuiltins && not (any (sameTool entry) extra)) tools++extra)])
    dispatch "resources/list" _=Right (object ["resources" .= [object ["uri" .= skillURI,"name" .= ("Debugging with the editor"::T.Text),"mimeType" .= ("text/markdown"::T.Text),"description" .= ("A workflow for source breakpoints, stepping, stack and variable inspection."::T.Text)]]])
    dispatch "tools/call" params = do
      (name,args)<-parameters (withObject "tool call" $ \o -> (,) <$> o .: "name" <*> o .:? "arguments" .!= object []) params
      case if includeBuiltins then builtinTool desktop name args else Left "Unknown tool" of
        Left err -> pure (result True (String err))
        Right value -> pure (result False value)
    dispatch _ _=Left (-32601,"Method not found")
    parameters parser = either (Left . (-32602,) . T.pack) Right . parseEither parser
    result failed value=object ["isError" .= failed,"content" .= [object ["type" .= ("text"::T.Text),"text" .= TE.decodeUtf8 (BL.toStrict (encode value))]]]
    sameTool (Object a) (Object b)=KM.lookup "name" a==KM.lookup "name" b
    sameTool _ _=False

builtinTools :: [Value]
builtinTools=tools

-- Exposed separately so session permission policy can wrap built-in reads just
-- like runtime tools, without decoding a JSON-RPC response back into a value.
builtinTool :: Desktop -> T.Text -> Value -> Either T.Text Value
builtinTool desktop=tool
  where
    tool :: T.Text -> Value -> Either T.Text Value
    tool "list_windows" _=Right (object ["windows" .= (map window (windows desktop)++panels)])
    tool "list_buffers" _=Right (object ["buffers" .= [bufferInfo ident doc | (ident,doc)<-M.toAscList (buffers desktop)]])
    tool "read_buffer" args = parseArgs (withObject "read_buffer" $ \o -> (,,,) <$> o .:? "bufferId" <*> o .:? "startLine" .!= 1 <*> o .:? "lineCount" .!= 200 <*> o .:? "byteOffset" .!= 0) args >>= \(wanted,start,count,offset) -> do
      ident <- maybe (maybe (Left "No active buffer") (Right . bufferId) (activeWindow desktop)) Right wanted
      doc <- maybe (Left "Buffer not found") Right (M.lookup ident (buffers desktop))
      unless (start>=1 && count>=1 && count<=1000 && offset>=0) (Left "Use startLine >= 1, lineCount 1..1000, and byteOffset >= 0")
      safeText<-maybe (Left "This buffer contains private user or approval content.") Right (sanitizedBuffer desktop ident)
      let original=documentBuffer doc
          redacted=safeText/=contents original
          b=if redacted then newBuffer safeText else original
      if byteMode b then
        let bytes=BS.take 4096 (BS.drop offset (bufferBytes b))
            hex n=let s=showHex n "" in T.pack (replicate (2-length s) '0'++s)
        in Right (object ["buffer" .= bufferInfo ident doc,"byteOffset" .= offset,"bytes" .= BS.length bytes,
          "hex" .= T.intercalate " " (map hex (BS.unpack bytes)),"totalBytes" .= BS.length (bufferBytes b)])
      else
        let available=if start>bufferLineCount b then 0 else min count (bufferLineCount b-start+1)
            text=T.intercalate "\n" [bufferLineAt b (start-1+row) | row<-[0..available-1]]
            limited=T.take 131072 text
        in Right (object ["buffer" .= bufferInfo ident doc,"startLine" .= start,"lineCount" .= available,
          "totalLines" .= bufferLineCount b,"text" .= limited,"redacted" .= redacted,"truncated" .= (T.length limited<T.length text)])
    tool "read_selection" args = parseArgs (withObject "read_selection" (.:? "windowId")) args >>= \wanted -> do
      w <- maybe (maybe (Left "No active window") Right (activeWindow desktop))
        (\ident -> maybe (Left "Window not found") Right (findWindow ident)) wanted
      doc <- maybe (Left "Buffer not found") Right (M.lookup (bufferId w) (buffers desktop))
      unless (not (protectedBuffer desktop (bufferId w))) (Left "Selections from private conversation or approval buffers are unavailable.")
      let b=documentBuffer doc
          text=selectedText (selection w) b
      Right (object ["windowId" .= windowId w,"bufferId" .= bufferId w,"anchor" .= anchor (selection w),
        "caret" .= caret (selection w),"text" .= T.take 131072 text,"truncated" .= (T.length text>131072)])
    tool _ _=Left "Unknown editor tool"
    parseArgs :: (Value -> Parser a) -> Value -> Either T.Text a
    parseArgs parser=either (Left . T.pack) Right . parseEither parser
    findWindow ident=case filter ((==ident).windowId) (windows desktop) of w:_->Just w; _->Nothing
    window w=object ["windowId" .= windowId w,"number" .= windowNumber w,"bufferId" .= bufferId w,
      "title" .= maybe "" title (M.lookup (bufferId w) (buffers desktop)),
      "active" .= (fmap windowId (activeWindow desktop)==Just (windowId w)),"bounds" .= rect (bounds w)]
    panels=[object ["kind" .= ("files"::T.Text),"title" .= ("Files"::T.Text),"path" .= treeRoot tree] | Just tree<-[sideTree desktop]]
      ++[object ["kind" .= ("messages"::T.Text),"title" .= ("Messages"::T.Text),"bounds" .= rect (problemsRect desktop)] | problemsVisible desktop]
    rect (Rect x y w h)=object ["x" .= x,"y" .= y,"width" .= w,"height" .= h]
    title doc | privateDocument desktop doc="[private]"
              | otherwise=fromMaybe (maybe "Untitled" (T.pack . filePath) (documentFile doc)) (documentLabel doc)
    bufferInfo ident doc=object ["bufferId" .= ident,"title" .= title doc,"path" .= (if privateDocument desktop doc then Nothing else fmap filePath (documentFile doc)),
      "modified" .= dirty (documentBuffer doc),"binary" .= byteMode (documentBuffer doc),"revision" .= revision (documentBuffer doc)]

-- Initiation runs with the desktop locked. Waiting for protocol replies must
-- happen afterwards so the same session can continue polling HLS and DAP.
editorResponseWith :: [Value]
  -> (Desktop -> T.Text -> Value -> IO (Desktop, IO (Either T.Text Value)))
  -> Desktop -> Value -> IO (Desktop, IO (Maybe Value))
editorResponseWith = editorResponseUsing True

-- Actor-bound orchestration bridges expose only explicitly supplied tools.
-- In particular, an unknown call must never reach the desktop read fallback.
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
            , Left err<-validateArguments descriptor args -> pure (desktop,pure (reply (toolResult (Left err))))
          Right (name,args) | any (named name) extra -> do
            started<-try (execute desktop name args)
            case started of
              Left (err::IOException) -> pure (desktop,pure (reply (toolResult (Left (T.pack (show err))))))
              Right (updated,finish) -> pure (updated, do
                result<-try finish
                pure (reply (case either (Left . T.pack . show) id (result::Either IOException (Either T.Text Value)) of
                  Right value | name=="editor_screen" -> value
                  outcome -> toolResult outcome)))
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
skillURI="thc-edit://debugging"

toolResult :: Either T.Text Value -> Value
toolResult (Right value) | BL.length (encode value)>4194304=toolResult (Left "Tool result exceeds 4 MiB; request a smaller page.")
toolResult outcome=object ["isError" .= either (const True) (const False) outcome,
  "content" .= [object ["type" .= ("text"::T.Text),"text" .= TE.decodeUtf8 (BL.toStrict (encode (either String id outcome)))]]]

debugTools :: [Value]
debugTools =
  [ describe "debug_status" "Read connection, stop state, generation, selected frame, follow mode, capabilities, breakpoints and recent output." True [] []
  , describe "debug_present" "Set automatic source following (initially true), or reveal source/stack/scopes/output from the shared session. Omitted follow is unchanged; false keeps asynchronous stops in the background. Any view requires the current generation; source/stack/scopes also require a stopped target. follow=true resumes future automatic following; use view=source to reveal now. Presentation does not resume, relaunch or advance generation." False [] [("follow",object ["type" .= ("boolean"::T.Text)]),("view",enum ["source","stack","scopes","output"]),("generation",integer)]
  , describe "debug_launch" "Launch the configured THC target, or a general DAP adapter JSON configuration. Fails if a session is already active." False [] [("adapterConfig",str),("port",integer)]
  , describe "debug_attach" "Attach to a loopback DAP adapter (127.0.0.1:4711 by default)." False [] [("host",str),("port",integer)]
  , describe "debug_control" "Continue, step, pause or disconnect the current debug generation. Accepted does not mean the next stop has occurred; inspect status afterwards." False ["generation","command"] [("generation",integer),("command",enum ["continue","next","stepIn","stepOut","pause","disconnect"])]
  , describe "debug_set_breakpoints" "Replace source breakpoints for an open buffer, using 1-based lines. Reports pending/verified state; unsaved buffers are marked sourceModified." False ["generation","bufferId","lines"] [("generation",integer),("bufferId",integer),("lines",object ["type" .= ("array"::T.Text),"items" .= object ["type" .= ("integer"::T.Text),"minimum" .= (1::Int)],"maxItems" .= (1000::Int)])]
  , describe "debug_inspect" "Inspect threads, stackTrace, scopes, variables or source using generation-scoped handles. Stack and variables require a stopped target. start/count page results (100 default, 1000 max)." True ["generation","request"] ([("generation",integer),("request",enum ["threads","stackTrace","scopes","variables","source"]) ]++[(key,integer) | key<-["threadId","frameId","variablesReference","sourceReference","start","count"]])
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
  , describe "list_buffers" "List open buffers with paths and unsaved-change state, including untitled buffers." []
  , describe "read_buffer" "Read live buffer contents including unsaved edits; private conversation fields are redacted and approval buffers are unavailable. Text is paged by 1-based lines (200 default, 1000 maximum); binary buffers return up to 4096 hex bytes from byteOffset." [("bufferId","integer"),("startLine","integer"),("lineCount","integer"),("byteOffset","integer")]
  , describe "read_selection" "Read selection and cursor offsets in an editor window (defaults to the active window)." [("windowId","integer")]
  ]
  where
    describe :: T.Text -> T.Text -> [(T.Text,T.Text)] -> Value
    describe name description properties=object ["name" .= name,"description" .= description,
      "inputSchema" .= object ["type" .= ("object"::T.Text),"properties" .= object [fromString key .= object ["type" .= typ] | (key,typ)<-properties],"additionalProperties" .= False],
      "annotations" .= object ["readOnlyHint" .= True,"destructiveHint" .= False,"openWorldHint" .= False]]
    fromString=Data.Aeson.Key.fromText
