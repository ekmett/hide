{-# LANGUAGE OverloadedStrings #-}
module EditorMCPCheck (checks) where
import AllocationProfile (AllocationProfile, withinBudget)
import SourceWindowFixture (sourceFixtureBuffer)
import Control.Monad (unless)
import Data.IORef
import Control.Exception (bracket, try, IOException, evaluate)
import GHC.Conc (getAllocationCounter)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (openBinaryTempFile, hClose, hSeek, SeekMode(AbsoluteSeek))
import qualified Data.ByteString as BS
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hide.EditorMCP
import Hide.BufferReads (captureWindow)
import Hide.BufferReadCommand (withBufferReadCommands,bufferPage)
import Hide.Plugin.Command (Codec(..))
import qualified Hide.Plugin.BufferRead as R
import qualified Hide.BufferTools as BufferTools
import qualified Hide.Plugin.Window as W
import qualified Data.Vector as V
import Hide.Syntax (Style(..))
import Hide.Files (FileState(..))
import Hide.Buffer
import Hide.Model

checks :: AllocationProfile -> IO ()
checks profile = do
  let check name ok=unless ok (error name)
      original=addDocument Nothing (newBuffer "old\nsecond") (initialDesktop (80,25))
      win=fromJust (activeWindow original)
      edited=original {buffers=M.adjust (\doc->doc {documentBuffer=replaceBuffer False "unsaved λ\nsecond" (documentBuffer doc)}) (sourceFixtureBuffer win) (buffers original)}
      rpc method params=object ["jsonrpc" .= ("2.0"::T.Text),"id" .= (7::Int),"method" .= (method::T.Text),"params" .= params]
      call name args=editorResponse edited (rpc "tools/call" (object ["name" .= (name::T.Text),"arguments" .= args]))
      text=TE.decodeUtf8 . BL.toStrict . encode
      names=editorResponse edited (rpc "tools/list" (object []))
      content=bufferContents edited (object ["bufferId" .= sourceFixtureBuffer win,"startLine" .= (1::Int),"lineCount" .= (1::Int)])
  check "MCP lists built-in live window tools" (all (\name->maybe False (T.isInfixOf name . text) names) ["list_windows","read_window","read_selection"])
  check "MCP returns unsaved Unicode text" (either (const False) (T.isInfixOf "unsaved λ" . text) content && not (either (const False) (T.isInfixOf "second" . text) content))
  check "MCP does not fabricate saved paths for untitled buffers" (maybe False (T.isInfixOf "Untitled" . text) (call "list_windows" (object [])))
  check "MCP missing buffer is a tool error" (case bufferContents edited (object ["bufferId" .= (999::Int)]) of Left "Buffer not found"->True; _->False)
  let pastEnd=bufferContents edited (object ["startLine" .= (maxBound::Int),"lineCount" .= (1000::Int)])
  check "MCP extreme line offsets cannot overflow" (case pastEnd of Right (Object fields)->KM.lookup "lineCount" fields==Just (toJSON (0::Int)); _->False)
  check "MCP bounds reads" (case bufferContents edited (object ["lineCount" .= (1001::Int)]) of Left _->True; _->False)
  check "MCP notifications receive no response" (editorResponse edited (object ["jsonrpc" .= ("2.0"::T.Text),"method" .= ("notifications/initialized"::T.Text)])==Nothing)
  let initialized=editorResponse edited (rpc "initialize" (object ["protocolVersion" .= ("2025-11-25"::T.Text)]))
  check "MCP initialize advertises tools" (maybe False (T.isInfixOf "capabilities" . text) initialized)
  check "MCP invalid request is protocol error" (case editorResponse edited Null of Just (Object fields)->KM.member "error" fields; _->False)
  let conversationText="Session: provider-secret\nPublic assistant response\nOther: unsent-secret"
      answerStart=T.length "Session: provider-secret\nPublic assistant response\nOther: "
      semantics=W.TextSemantics W.CopyText Nothing V.empty V.empty W.ReadableWindow
        (V.fromList [(0,T.length "Session: provider-secret"),(answerStart,T.length conversationText)]) V.empty V.empty
  conversationBody<-W.prepareSemanticTextWindow "Conversation" [(conversationText,Plain)] semantics
    >>= either (error . T.unpack) pure
  W.withWindowScope $ \scope->withBufferReadCommands $ \commands->do
    update<-W.openWindow scope conversationBody >>= maybe (error "Prepared read fixture scope ended") pure
    (reference,_)<-W.admitWindowUpdate False update >>= maybe (error "Prepared read fixture admission failed") pure
    let conversation=addPluginWindow reference conversationBody (initialDesktop (80,25))
    (_,finishRead)<-readWindowTool commands (captureWindow conversation) conversation "read_window" (object [])
    privateRead<-finishRead
    let encodedRead=either id text privateRead
    check "MCP prepared window reads redact private semantic ranges"
      (case privateRead of
        Right _->not ("provider-secret" `T.isInfixOf` encodedRead) && not ("unsent-secret" `T.isInfixOf` encodedRead)
          && "Public assistant response" `T.isInfixOf` encodedRead
        Left _->False)
    check "MCP prepared window selections cannot bypass private redaction"
      (case builtinTool (modifyActive (\w->w {selection=Selection 0 200}) conversation) "read_selection" (object []) of Left _->True; _->False)
    check "MCP prepared windows cannot fabricate a source buffer read"
      (M.null (buffers conversation) && case bufferContents conversation (object []) of Left _->True; _->False)
  let byteDocument=addDocument Nothing (newByteBuffer (BS.pack [0,127,128,255])) (initialDesktop (80,25))
  check "MCP byte formatting preserves original values and spacing" (case bufferContents byteDocument (object []) of
    Right (Object fields)->KM.lookup "hex" fields==Just (String "00 7f 80 ff") && KM.lookup "totalBytes" fields==Just (toJSON (4::Int))
    _->False)
  let largeBytes=addDocument Nothing (newByteBuffer (BS.replicate (2*1024*1024) 120)) (initialDesktop (80,25))
      largeBuffer=documentBuffer (fromJust (activeDocument largeBytes))
  _<-evaluate (prepareBuffer largeBuffer)
  allocatedBefore<-getAllocationCounter
  localBytes<-evaluate (case bufferContents largeBytes (object ["byteOffset" .= (1048576::Int)]) of
    Right value->BL.length (encode value)
    Left err->error (T.unpack err))
  allocatedAfter<-getAllocationCounter
  check "MCP bounded byte read does not flatten or encode the whole buffer" (localBytes>0 && withinBudget profile (allocatedBefore-allocatedAfter) 2000000)
  let pageLimit=131072
      -- A fragmented long row cannot borrow one already-flat source Text.
      longText=replaceSelection (Selection 1 2) "y" (newBuffer (T.replicate (8*1024*1024) "x"))
      longDocument=addDocument Nothing longText (initialDesktop (80,25))
  _<-evaluate (prepareBuffer longText)
  textAllocatedBefore<-getAllocationCounter
  boundedText<-evaluate (case bufferContents longDocument (object []) of
    Right (Object fields)->case KM.lookup "text" fields of
      Just (String body)->T.length body==pageLimit && KM.lookup "truncated" fields==Just (Bool True)
      _->False
    _->False)
  textAllocatedAfter<-getAllocationCounter
  check "MCP bounded text read does not flatten a complete oversized row"
    (boundedText && withinBudget profile (textAllocatedBefore-textAllocatedAfter) 2000000)
  selectionAllocatedBefore<-getAllocationCounter
  boundedSelection<-evaluate (case builtinTool (modifyActive (\w->w {selection=Selection maxBound 0}) longDocument) "read_selection" (object []) of
    Right (Object fields)->case KM.lookup "text" fields of
      Just (String body)->T.length body==pageLimit && KM.lookup "truncated" fields==Just (Bool True)
      _->False
    _->False)
  selectionAllocatedAfter<-getAllocationCounter
  check "MCP bounded selection does not materialize the complete selected text"
    (boundedSelection && withinBudget profile (selectionAllocatedBefore-selectionAllocatedAfter) 2000000)
  let pageCases=["", "λ😀\r\nsecond\r\n", T.replicate pageLimit "λ",
        T.replicate pageLimit "λ"<>"\r\n", T.replicate (pageLimit-1) "λ"<>"\r\n",
        T.replicate (pageLimit-1) "λ"<>"\r\n😀", T.replicate (pageLimit-2) "λ"<>"\n\n"]
  mapM_ (\source->do
    let pageDocument=addDocument Nothing (newBuffer source) (initialDesktop (80,25))
        rows=map (T.dropWhileEnd (=='\r')) (T.splitOn "\n" source)
        expected=T.intercalate "\n" rows
    check "MCP text paging preserves row metadata, normalized endings and exact character cap"
      (case bufferContents pageDocument (object []) of
        Right (Object fields)->KM.lookup "text" fields==Just (String (T.take pageLimit expected))
          && KM.lookup "truncated" fields==Just (Bool (T.length expected>pageLimit))
          && KM.lookup "lineCount" fields==Just (toJSON (length rows))
          && KM.lookup "totalLines" fields==Just (toJSON (length rows))
        _->False)
    check "MCP selection preserves direction, raw line endings and character bounds"
      (case builtinTool (modifyActive (\w->w {selection=Selection maxBound minBound}) pageDocument) "read_selection" (object []) of
        Right (Object fields)->KM.lookup "text" fields==Just (String (T.take pageLimit source))
          && KM.lookup "truncated" fields==Just (Bool (T.length source>pageLimit))
          && KM.lookup "anchor" fields==Just (toJSON (maxBound::Int))
          && KM.lookup "caret" fields==Just (toJSON (minBound::Int))
          && KM.lookup "coordinateSpace" fields==Just (String "source")
        _->False)) pageCases
  let partial=modifyActive (\w->w {selection=Selection 3 1}) (addDocument Nothing (newBuffer "λ😀\r\nsecond") (initialDesktop (80,25)))
  check "MCP selection slices Unicode character offsets rather than bytes"
    (case builtinTool partial "read_selection" (object []) of
      Right (Object fields)->KM.lookup "text" fields==Just (String "😀\r") && KM.lookup "truncated" fields==Just (Bool False)
      _->False)
  let authorityPath="/authority/private-session-key.json"
      authority=addDocument (Just (FileState authorityPath Nothing)) (newBuffer "secret config") edited {guestPrivatePaths=[authorityPath]}
  check "MCP private file metadata hides secret-bearing filenames" (all (\name->case builtinTool authority name (object []) of Right value->not ("private-session-key" `T.isInfixOf` text value) && "[private]" `T.isInfixOf` text value; _->False) ["list_windows"])
  let diskReview=addReadOnly ("Disk changes: "<>T.pack authorityPath) "private review" edited {guestPrivatePaths=[authorityPath]}
  check "MCP disk review metadata hides protected source filenames" (all (\name->case builtinTool diskReview name (object []) of Right value->not ("private-session-key" `T.isInfixOf` text value) && "[private]" `T.isInfixOf` text value; _->False) ["list_windows"])
  invoked<-newIORef (0::Int)
  completed<-newIORef False
  let execute d _ _=do
        modifyIORef' invoked (+1)
        pure (d {status="initiated"},writeIORef completed True >> pure (Right (object ["ready" .= True])))
      request name args=rpc "tools/call" (object ["name" .= (name::T.Text),"arguments" .= args])
  (updated,finish)<-editorResponseWith debugTools execute edited (request "debug_status" (object []))
  check "MCP initiation updates desktop before deferred completion" (status updated=="initiated")
  check "MCP does not wait while initializing tool" . not =<< readIORef completed
  reply<-finish
  check "MCP deferred result wraps JSON response" (maybe False (T.isInfixOf "ready" . text) reply)
  (_,ignored)<-editorResponseWith debugTools execute edited (request "not_a_tool" (object []))
  _<-ignored
  check "MCP unknown tool never dispatches controller" . (==1) =<< readIORef invoked
  (_,notification)<-editorResponseWith debugTools execute edited (object ["jsonrpc" .= ("2.0"::T.Text),"method" .= ("tools/call"::T.Text),"params" .= object ["name" .= ("debug_launch"::T.Text)]])
  check "MCP notifications never initiate a mutation" . (==Nothing) =<< notification
  check "MCP mutation notification leaves controller untouched" . (==1) =<< readIORef invoked
  check "built-in read operation can be called through permission wrapper" (case builtinTool edited "list_windows" (object []) of Right (Object fields) -> KM.member "windows" fields; _ -> False)
  (_,gatedRead)<-editorResponseWith builtinTools execute edited (request "list_windows" (object []))
  _<-gatedRead
  check "registered built-in reads use controller callback" . (==2) =<< readIORef invoked
  (_,registered)<-editorResponseWith builtinTools execute edited (rpc "tools/list" (object []))
  listed<-registered
  check "built-in descriptors registered for permissions are not duplicated" (case listed of
    Just (Object fields) -> KM.lookup "result" fields==Just (object ["tools" .= builtinTools])
    _ -> False)
  let agentSpec=object ["name" .= ("agents_list"::T.Text),"inputSchema" .= object ["type" .= ("object"::T.Text),"properties" .= object []],"outputSchema" .= object ["type" .= ("object"::T.Text)]]
      strict specs req=editorResponseOnly specs execute edited req >>= snd
  emptyList<-strict [] (rpc "tools/list" (object []))
  check "strict empty server lists no built-in tools" (case emptyList of
    Just (Object fields)->KM.lookup "result" fields==Just (object ["tools" .= ([]::[Value])]); _->False)
  agentList<-strict [agentSpec] (rpc "tools/list" (object []))
  check "strict server lists only actor tool descriptors" (case agentList of
    Just (Object fields)->KM.lookup "result" fields==Just (object ["tools" .= [agentSpec]]); _->False)
  mapM_ (\specs->mapM_ (\name->do
    denied<-strict specs (request name (object []))
    check "strict server cannot fall back to workspace reads" (maybe False (T.isInfixOf "Unknown tool" . text) denied
      && not (maybe False (T.isInfixOf "unsaved λ" . text) denied))) ["read_buffer","list_buffers","list_windows","read_selection"]) [[],[agentSpec]]
  check "strict rejected calls never reach controller" . (==2) =<< readIORef invoked
  allowed<-strict [agentSpec] (request "agents_list" (object []))
  check "strict registered tool reaches controller" (maybe False (T.isInfixOf "ready" . text) allowed)
  check "strict registered tool dispatched once" . (==3) =<< readIORef invoked
  check "advertised object output has matching structured and text results" (case allowed >>= parseMaybe (withObject "reply" (.: "result")) of
    Just (Object result)->case (KM.lookup "structuredContent" result,KM.lookup "content" result) of
      (Just value@(Object _),Just (Array blocks))->case V.toList blocks of
        [Object entry]->case KM.lookup "text" entry of Just (String encoded)->eitherDecodeStrict' (TE.encodeUtf8 encoded)==Right value; _->False
        _->False
      _->False
    _->False)
  (_,readSkill)<-editorResponseWith debugTools execute edited (rpc "resources/read" (object ["uri" .= ("hide://debugging"::T.Text)]))
  skill<-readSkill
  check "MCP packaged skill readable" (maybe False (T.isInfixOf "name: debug-editor" . text) skill)
  let token=T.replicate 48 "a"
      session=replicate 48 'b'
      envScoped name value action=bracket (lookupEnv name <* maybe (unsetEnv name) (setEnv name) value)
        (maybe (unsetEnv name) (setEnv name)) (const action)
  envScoped "THC_EDIT_SESSION" (Just session) $ envScoped "THC_EDIT_MCP_TOKEN" (Just "inherited-token") $ do
    legacy<-editorServers
    explicit<-editorServersFor (Just token)
    check "legacy MCP descriptors remain tokenless despite inherited credentials" (case legacy of
      [Object fields]->KM.lookup "env" fields==Just (toJSON ([]::[Value])); _->False)
    check "actor credential appears only in descriptor environment, never arguments" (case explicit of
      [Object fields]->KM.lookup "args" fields==Just (toJSON ["--mcp-editor",session]) &&
        KM.lookup "env" fields==Just (toJSON [object ["name" .= ("THC_EDIT_MCP_TOKEN"::T.Text),"value" .= token]])
      _->False)
  envScoped "THC_EDIT_SESSION" Nothing $ do
    absent<-editorServersFor (Just token)
    selected<-editorServersAt session (Just token)
    check "implicit MCP descriptor needs a host session" (null absent)
    check "explicit session descriptor does not depend on process session environment" (case selected of
      [Object fields]->KM.lookup "args" fields==Just (toJSON ["--mcp-editor",session]); _->False)
  temp<-getTemporaryDirectory
  bracket (openBinaryTempFile temp "thc-mcp-line") (\(path,h)->hClose h >> removeFile path) $ \(_,h) -> do
    BS.hPut h "first\nsecond\n"
    hSeek h AbsoluteSeek 0
    Just (first,rest)<-readMCPLine h BS.empty
    Just (second,_)<-readMCPLine h rest
    check "MCP reader preserves following messages" (first=="first" && second=="second")
    hSeek h AbsoluteSeek 0
    BS.hPut h (BS.replicate 1048577 120)
    hSeek h AbsoluteSeek 0
    tooLarge<-try (readMCPLine h BS.empty) :: IO (Either IOException (Maybe (BS.ByteString,BS.ByteString)))
    check "MCP rejects oversized unterminated input" (case tooLarge of Left _->True; _->False)
  putStrLn "editor MCP checks passed"

-- Paging and presentation are pure; authority is exercised through the actual
-- public tool/reader path in TypedBufferReadsCheck and BufferReadsCheck.
bufferContents :: Desktop -> Value -> Either T.Text Value
bufferContents desktop value=do
  arguments<-codecDecode R.readInput value
  ident<-maybe (maybe (Left "No active source buffer") Right (activeWindow desktop >>= bufferId)) Right (R.wantedBuffer arguments)
  doc<-maybe (Left "Buffer not found") Right (M.lookup ident (buffers desktop))
  let b=documentBuffer doc
      info=R.BufferMetadata ident "Untitled" Nothing (documentModified doc) (byteMode b) (revision b)
  codecEncode BufferTools.readOutput <$> bufferPage info arguments False (bufferContent b)
