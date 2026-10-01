{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.EditorMCP (editorResponse, editorServers, runEditorMCP, readMCPLine) where

import Control.Exception (bracket)
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showHex)
import System.Environment (lookupEnv, getExecutablePath)
import System.IO (Handle, stdin, stdout, hClose, hFlush, hSetBinaryMode)
import THC.Edit.Buffer
import THC.Edit.Files (filePath)
import THC.Edit.Model
import THC.Edit.Protocol (WirePacket(..), readPacket, writePacket)
import THC.Edit.RemoteEndpoint (sessionEndpoint, connectEndpoint)

-- ACP starts this small stdio bridge beside its provider. The private session
-- endpoint supplies a current snapshot without taking over the display.
editorServers :: IO [Value]
editorServers = do
  session <- lookupEnv "THC_EDIT_SESSION"
  executable <- getExecutablePath
  pure [object ["name" .= ("editor"::T.Text),"command" .= executable,
    "args" .= ["--mcp-editor",ident],"env" .= ([]::[Value])] | Just ident<- [session]]

runEditorMCP :: String -> IO ()
runEditorMCP ident = do
  path <- sessionEndpoint ident
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  let loop pending = do
        incoming <- readMCPLine stdin pending
        case incoming of
          Nothing -> pure ()
          Just (line,rest) -> do
            case eitherDecodeStrict' line of
              Left _ -> output (rpcError Null (-32700) "Invalid JSON")
              Right request -> bracket (connectEndpoint path) hClose $ \connection -> do
                writePacket connection (JsonPacket (object ["type" .= ("inspect"::T.Text),"request" .= (request::Value)]))
                reply <- readPacket connection
                case reply of
                  Just (JsonPacket Null) -> pure ()
                  Just (JsonPacket response) -> output response
                  _ -> ioError (userError "Editor introspection connection ended")
            loop rest
      output value=BL.hPut stdout (encode value<>"\n") >> hFlush stdout
  loop BS.empty

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
editorResponse desktop request = case request of
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
      pure (object ["protocolVersion" .= version,"capabilities" .= object ["tools" .= object []],
        "serverInfo" .= object ["name" .= ("thc-edit"::T.Text),"version" .= ("0.1.0"::T.Text)],
        "instructions" .= ("Read the editor's live windows and buffers. Buffer contents include unsaved edits; IDs are stable for this editor session. Line numbers start at 1. These tools do not modify files."::T.Text)])
    dispatch "ping" _=Right (object [])
    dispatch "tools/list" _=Right (object ["tools" .= tools])
    dispatch "tools/call" params = do
      (name,args)<-parameters (withObject "tool call" $ \o -> (,) <$> o .: "name" <*> o .:? "arguments" .!= object []) params
      case tool name args of
        Left err -> pure (result True (String err))
        Right value -> pure (result False value)
    dispatch _ _=Left (-32601,"Method not found")
    parameters parser = either (Left . (-32602,) . T.pack) Right . parseEither parser
    result failed value=object ["isError" .= failed,"content" .= [object ["type" .= ("text"::T.Text),"text" .= TE.decodeUtf8 (BL.toStrict (encode value))]]]
    tool :: T.Text -> Value -> Either T.Text Value
    tool "list_windows" _=Right (object ["windows" .= (map window (windows desktop)++panels)])
    tool "list_buffers" _=Right (object ["buffers" .= [bufferInfo ident doc | (ident,doc)<-M.toAscList (buffers desktop)]])
    tool "read_buffer" args = parseArgs (withObject "read_buffer" $ \o -> (,,,) <$> o .:? "bufferId" <*> o .:? "startLine" .!= 1 <*> o .:? "lineCount" .!= 200 <*> o .:? "byteOffset" .!= 0) args >>= \(wanted,start,count,offset) -> do
      ident <- maybe (maybe (Left "No active buffer") (Right . bufferId) (activeWindow desktop)) Right wanted
      doc <- maybe (Left "Buffer not found") Right (M.lookup ident (buffers desktop))
      unless (start>=1 && count>=1 && count<=1000 && offset>=0) (Left "Use startLine >= 1, lineCount 1..1000, and byteOffset >= 0")
      let b=documentBuffer doc
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
          "totalLines" .= bufferLineCount b,"text" .= limited,"truncated" .= (T.length limited<T.length text)])
    tool "read_selection" args = parseArgs (withObject "read_selection" (.:? "windowId")) args >>= \wanted -> do
      w <- maybe (maybe (Left "No active window") Right (activeWindow desktop))
        (\ident -> maybe (Left "Window not found") Right (findWindow ident)) wanted
      doc <- maybe (Left "Buffer not found") Right (M.lookup (bufferId w) (buffers desktop))
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
    title doc=fromMaybe (maybe "Untitled" (T.pack . filePath) (documentFile doc)) (documentLabel doc)
    bufferInfo ident doc=object ["bufferId" .= ident,"title" .= title doc,"path" .= fmap filePath (documentFile doc),
      "modified" .= dirty (documentBuffer doc),"binary" .= byteMode (documentBuffer doc),"revision" .= revision (documentBuffer doc)]

tools :: [Value]
tools =
  [ describe "list_windows" "List editor window IDs, titles, buffer IDs, geometry, active window and side panels." []
  , describe "list_buffers" "List open buffers with paths and unsaved-change state, including untitled buffers." []
  , describe "read_buffer" "Read live buffer contents including unsaved edits. Text is paged by 1-based lines (200 default, 1000 maximum); binary buffers return up to 4096 hex bytes from byteOffset." [("bufferId","integer"),("startLine","integer"),("lineCount","integer"),("byteOffset","integer")]
  , describe "read_selection" "Read selection and cursor offsets in an editor window (defaults to the active window)." [("windowId","integer")]
  ]
  where
    describe :: T.Text -> T.Text -> [(T.Text,T.Text)] -> Value
    describe name description properties=object ["name" .= name,"description" .= description,
      "inputSchema" .= object ["type" .= ("object"::T.Text),"properties" .= object [fromString key .= object ["type" .= typ] | (key,typ)<-properties],"additionalProperties" .= False],
      "annotations" .= object ["readOnlyHint" .= True,"destructiveHint" .= False,"openWorldHint" .= False]]
    fromString=Data.Aeson.Key.fromText
