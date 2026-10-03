{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Copilot language-server transport and completion adaptation.
--
-- The adapter keeps one synchronized document and converts editor character
-- positions to UTF-16. Synchronization batches and remembered document state are
-- committed together before requests proceed. Feedback retains opaque provider
-- items; authentication credentials remain with the language server, while the
-- editor displays only validated device-flow information. All operations except
-- status polling belong to one background owner, outside the desktop lock.
module Hide.Copilot
  ( Copilot, withCopilot, completeCopilot, signInCopilot, finishSignInCopilot
  , signOutCopilot, feedbackCopilot, pollCopilotMessages
  ) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Monad (forever, forM_, unless, void, when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower,ord)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe,mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.FilePath (takeExtension,takeFileName)
import System.IO
import System.Process
import System.Timeout (timeout)
import Text.Read (readMaybe)
import qualified Hide.ACP as ACP
import Hide.InlineTypes
import Hide.LSP (fileUri,offsetPosition,positionOffset,positionValue)
import Hide.Process (processCleanup)

data Copilot = Copilot
  { call :: Int -> Text -> Value -> IO Value
  , notify :: Text -> Value -> IO ()
  , prepareNotifications :: [(Text,Value)] -> IO (STM ())
  , document :: TVar (Maybe (FilePath,Int,Text))
  , messages :: TVar [Text]
  }

-- | Own the process and its three transport workers. Cancellation of an
-- individual call removes its waiter and sends $/cancelRequest; it never cancels
-- a writer halfway through a frame. Closing the scope kills the process tree
-- before joining pipe workers. No server stderr or raw error payload is logged.
withCopilot :: ACP.Launch -> FilePath -> (Copilot -> IO a) -> IO a
withCopilot launch root action = mask $ \restore -> do
  inherited<-getEnvironment
  let env'=M.toList (M.union (M.fromList (ACP.environment launch)) (M.fromList inherited))
  (Just input,Just output,Just errors,process)<-createProcess
    (proc (ACP.executable launch) (ACP.arguments launch))
      {cwd=Just root,env=Just env',std_in=CreatePipe,std_out=CreatePipe,std_err=CreatePipe,create_group=True}
  cleanup<-processCleanup process
  let close=cleanup >> mapM_ (\h->hClose h `catch` (\(_::IOException)->pure ())) [input,output,errors]
  flip finally close $ do
    mapM_ (`hSetBinaryMode` True) [input,output,errors]
    queue<-newTBQueueIO 64
    bytes<-newTVarIO (0::Int)
    pending<-newTVarIO M.empty
    next<-newTVarIO (1::Int)
    failed<-newTVarIO False
    status<-newTVarIO []
    docs<-newTVarIO Nothing
    let failTransport=atomically $ do
          writeTVar failed True
          waiting<-readTVar pending
          mapM_ (\reply->void (tryPutTMVar reply Nothing)) (M.elems waiting)
          writeTVar pending M.empty
          writeTVar status ["Copilot language server disconnected."]
        packet value=do
          let body=BL.toStrict (BL.take (fromIntegral frameLimit+1) (encode value))
          n<-evaluate (BS.length body)
          when (n>frameLimit) (protocolError "message exceeds size limit")
          pure (BC.pack ("Content-Length: "++show n++"\r\n\r\n")<>body)
        enqueue body=do
          dead<-readTVar failed
          when dead (throwSTM (userError "Copilot language server unavailable"))
          size<-readTVar bytes
          check (size+BS.length body<=frameLimit+8192)
          writeTBQueue queue body
          writeTVar bytes (size+BS.length body)
        send value=packet value >>= atomically . enqueue
        notification :: Text -> Value -> Value
        notification method params=object ["jsonrpc" .= ("2.0"::Text),"method" .= method,"params" .= params]
        tell method params=within 5000000 (send (notification method params))
        prepare notices=do
          bodies<-mapM (packet . uncurry notification) notices
          let body=BS.concat bodies
          when (BS.length body>frameLimit+8192) (protocolError "document synchronization exceeds size limit")
          pure (enqueue body)
        cancel ident=do
          body<-packet (notification ("$/cancelRequest"::Text) (object ["id" .= ident]))
          atomically ((enqueue body) `orElse` pure ()) `catch` (\(_::IOException)->pure ())
        request :: Int -> Text -> Value -> IO Value
        request micros method params=mask $ \unmask -> do
          ident<-atomically $ do n<-readTVar next; writeTVar next (n+1); pure n
          reply<-newEmptyTMVarIO
          let unregister=atomically (modifyTVar' pending (M.delete ident))
              run=do
                body<-packet (object ["jsonrpc" .= ("2.0"::Text),"id" .= ident,"method" .= method,"params" .= params])
                atomically $ do
                  waiting<-readTVar pending
                  check (M.size waiting<16)
                  enqueue body
                  modifyTVar' pending (M.insert ident reply)
                received<-atomically (takeTMVar reply)
                case received of
                  Nothing->protocolError "server disconnected"
                  Just value | Just (_::Value)<-field "error" value -> protocolError "request rejected"
                             | Just result<-field "result" value -> pure result
                             | otherwise -> protocolError "invalid response"
          unmask (within micros run) `onException` (unregister >> cancel ident) `finally` unregister
        respond ident fields=send (object (["jsonrpc" .= ("2.0"::Text),"id" .= ident]++fields))
        folders=[object ["uri" .= fileUri root,"name" .= T.pack (takeFileName root)]]
        receive value=case field "method" value :: Maybe Text of
          Just method | Just ident<-field "id" value -> do
            let params=fromMaybe Null (field "params" value)
            respond (ident::Value) $ case method of
              "workspace/configuration"->["result" .= replicate (min 64 (length (fromMaybe [] (field "items" params::Maybe [Value])))) Null]
              "workspace/workspaceFolders"->["result" .= folders]
              "window/workDoneProgress/create"->["result" .= Null]
              "window/showDocument"->["result" .= object ["success" .= False]]
              _->["error" .= object ["code" .= (-32601::Int),"message" .= ("Unsupported client request"::Text)]]
          Just "didChangeStatus"->do
            -- Only map known status kinds. Server text can include credentials,
            -- URLs or document content and must not enter the editor's logs.
            let kind=field "params" value >>= field "kind" :: Maybe Text
                message=case kind of
                  Just "Normal"->"Copilot ready."
                  Just "Error"->"Copilot requires authentication or authorization."
                  Just "Warning"->"Copilot temporarily unavailable."
                  Just "Inactive"->"Copilot inactive for this document."
                  _->"Copilot status changed."
            atomically (writeTVar status [message])
          Just _->pure ()
          Nothing | Just ident<-field "id" value -> atomically $ do
            waiting<-readTVar pending
            forM_ (M.lookup ident waiting) (\reply->void (tryPutTMVar reply (Just value)))
          _->pure ()
        guarded io=io `catch` (\(e::SomeException)->case fromException e::Maybe SomeAsyncException of
          Just _->throwIO e
          Nothing->failTransport)
        writer=forever $ do
          body<-atomically (readTBQueue queue)
          within 5000000 (BS.hPut input body >> hFlush input)
          atomically (modifyTVar' bytes (subtract (BS.length body)))
        reader=forever (readFrame output >>= receive)
        drain=BS.hGetSome errors 4096 >>= \chunk->unless (BS.null chunk) drain
        client=Copilot request tell prepare docs status
    withAsync (guarded writer) $ \_ -> withAsync (guarded reader) $ \_ -> withAsync drain $ \_ ->
      -- Kill before withAsync joins blocked pipe IO on both success and failure.
      restore (do
        _<-request 20000000 "initialize" (object
          ["processId" .= Null,"rootUri" .= fileUri root,"workspaceFolders" .= folders
          ,"capabilities" .= object ["workspace" .= object ["workspaceFolders" .= True,"configuration" .= True]
             ,"general" .= object ["positionEncodings" .= ["utf-16"::Text]]]
          ,"initializationOptions" .= object
             ["editorInfo" .= object ["name" .= ("hide"::Text),"version" .= ("0.1.0"::Text)]
             ,"editorPluginInfo" .= object ["name" .= ("hide Copilot"::Text),"version" .= ("0.1.0"::Text)]]])
        tell "initialized" (object [])
        tell "workspace/didChangeConfiguration" (object ["settings" .= object ["telemetry" .= object ["telemetryLevel" .= ("off"::Text)]]])
        action client) `finally` cleanup

-- | Drain a coalesced, credential-free status message; safe on the UI thread.
pollCopilotMessages :: Copilot -> IO [Text]
pollCopilotMessages client=atomically $ do result<-readTVar (messages client); writeTVar (messages client) []; pure result

-- | Synchronize the current file, then request all alternatives. Changes replace
-- the previous whole-document range rather than sending fine-grained edits.
completeCopilot :: Copilot -> CompletionInput -> IO [Proposal]
completeCopilot client input=do
  let path=inputPath input; text=inputText input; version=inputVersion input
      ident=object ["uri" .= fileUri path]
  previous<-readTVarIO (document client)
  let updates=case previous of
        Just (oldPath,oldVersion,oldText) | oldPath==path ->
          [("textDocument/didChange",object
            ["textDocument" .= object ["uri" .= fileUri path,"version" .= version]
            ,"contentChanges" .= [object ["range" .= object ["start" .= positionValue oldText 0,"end" .= positionValue oldText (T.length oldText)],"text" .= text]]])
            | oldVersion/=version || oldText/=text]
        _ -> [("textDocument/didClose",object ["textDocument" .= object ["uri" .= fileUri oldPath]]) | Just (oldPath,_,_)<-[previous]] ++
          [("textDocument/didOpen",object ["textDocument" .= object
            ["uri" .= fileUri path,"version" .= version,"languageId" .= language path,"text" .= text]])]
  enqueue<-prepareNotifications client (updates++[("textDocument/didFocus",object ["textDocument" .= ident])])
  -- Queue the complete sync batch and commit its remembered state together.
  -- Cancellation before enqueue leaves both unchanged; after enqueue the
  -- writer owns the whole batch even if its completion request is cancelled.
  within 5000000 (atomically (enqueue >> writeTVar (document client) (Just (path,version,text))))
  result<-call client 30000000 "textDocument/inlineCompletion" (object
    ["textDocument" .= object ["uri" .= fileUri path,"version" .= version]
    ,"position" .= positionValue text (inputOffset input)
    ,"context" .= object ["triggerKind" .= (if inputIntent input=="propose" then 2 else 1::Int)]
    ,"formattingOptions" .= object ["tabSize" .= (4::Int),"insertSpaces" .= True]])
  let proposals=take 32 (mapMaybe (proposal input) (fromMaybe [] (field "items" result)))
  -- Normalize and validate every published alternative on this worker.
  forM_ proposals $ \item->void (evaluate (proposalStart item+proposalEnd item+T.length (proposalText item)))
  pure proposals

-- | Begin device authentication. Only the user code and server command survive;
-- credentials remain the language server's responsibility.
signInCopilot :: Copilot -> IO CopilotSignIn
signInCopilot client=do
  result<-call client 30000000 "signIn" (object [])
  let code=fromMaybe "" (field "userCode" result)
      command=fromMaybe Null (field "command" result)
  if T.null code || field "command" command==Just ("github.copilot.finishDeviceFlow"::Text)
    then pure (CopilotSignIn code command)
    else protocolError "invalid authentication command"

-- | Finish only the explicit device-flow command, after the user chooses it.
finishSignInCopilot :: Copilot -> CopilotSignIn -> IO ()
finishSignInCopilot client signIn
  | T.null (signInCode signIn)=pure ()
  | otherwise=execute client 180000000 "github.copilot.finishDeviceFlow" (signInCommand signIn)

signOutCopilot :: Copilot -> IO ()
signOutCopilot client=void (call client 30000000 "signOut" (object []))

-- | Preserve the server's opaque item, including its acceptance command. Partial
-- acceptance counts from the start of insertText, in UTF-16 units on the wire.
feedbackCopilot :: Copilot -> CompletionFeedback -> Proposal -> IO ()
feedbackCopilot client feedback item=forM_ (proposalData item) $ \raw->case feedback of
  Ignored->pure ()
  Shown->notify client "textDocument/didShowCompletion" (object ["item" .= raw])
  Accepted->forM_ (field "command" raw) (execute client 10000000 "github.copilot.didAcceptCompletionItem")
  PartiallyAccepted count->notify client "textDocument/didPartiallyAcceptCompletion" (object
    ["item" .= raw,"acceptedLength" .= acceptedUnits count (fromMaybe (proposalText item) (field "insertText" raw))])

execute :: Copilot -> Int -> Text -> Value -> IO ()
execute client micros expected command
  | field "command" command==Just expected=void (call client micros "workspace/executeCommand" command)
  | otherwise=protocolError "unexpected command"

proposal :: CompletionInput -> Value -> Maybe Proposal
proposal input raw=do
  inserted<-field "insertText" raw :: Maybe Text
  (start,end)<-case field "range" raw :: Maybe Value of
    Nothing->pure (inputOffset input,inputOffset input)
    Just range->do
      start<-field "start" range >>= endpoint
      end<-field "end" range >>= endpoint
      pure (start,end)
  if start<0 || end<start || end>T.length (inputText input) || T.null inserted then Nothing
    else Just (Proposal start end (T.replace "\r\n" "\n" inserted) (Just raw))
  where
    endpoint value=do
      row<-field "line" value; column<-field "character" value
      let offset=positionOffset (inputText input) (row,column)
      if row<0 || column<0 || offsetPosition (inputText input) offset/=(row,column) then Nothing else Just offset

-- The editor normalizes CRLF. Map its cumulative accepted character count back
-- onto the original server insertText before counting UTF-16 units.
acceptedUnits :: Int -> Text -> Int
acceptedUnits count=go (max 0 count) . T.unpack
  where
    go 0 _=0
    go _ []=0
    go n ('\r':'\n':rest)=2+go (n-1) rest
    go n (c:rest)=(if ord c>0xffff then 2 else 1)+go (n-1) rest

language :: FilePath -> Text
language path=fromMaybe "" (lookup (map toLower (takeExtension path))
  [(".hs","haskell"),(".lhs","haskell"),(".py","python"),(".js","javascript"),(".jsx","javascriptreact")
  ,(".ts","typescript"),(".tsx","typescriptreact"),(".java","java"),(".rs","rust"),(".c","c"),(".h","c")
  ,(".cpp","cpp"),(".go","go"),(".sh","shellscript"),(".md","markdown"),(".json","json"),(".rb","ruby")])

field :: FromJSON a => Text -> Value -> Maybe a
field key=parseMaybe (withObject "Copilot message" (.: K.fromText key))

frameLimit :: Int
frameLimit=16*1024*1024

protocolError :: String -> IO a
protocolError message=ioError (userError ("Copilot: "++message))

within :: Int -> IO a -> IO a
within micros action=timeout micros action >>= maybe (protocolError "request timed out") pure

-- Header lines are read bytewise with a total bound, so a peer cannot allocate
-- an unbounded line before the header limit is checked.
readFrame :: Handle -> IO Value
readFrame handle=do
  size<-headers Nothing 0
  body<-bytes size []
  either (const (protocolError "invalid JSON response")) pure (eitherDecodeStrict' body)
  where
    headers found total=do
      line<-lineBytes (8192-total) []
      let count=total+BS.length line+1; stripped=BC.filter (/='\r') line
      if BS.null stripped then case found of
        Just n | n>=0 && n<=frameLimit->pure n
        _->protocolError "invalid Content-Length"
      else case BC.break (==':') stripped of
        (name,value) | BC.map toLower name=="content-length"->case readMaybe (BC.unpack (BC.drop 1 value)) of
          Just n | found==Nothing->headers (Just n) count
          _->protocolError "invalid Content-Length"
        _->headers found count
    lineBytes remaining acc
      | remaining<=0=protocolError "oversized header"
      | otherwise=do
        byte<-BS.hGet handle 1
        when (BS.null byte) (protocolError "server closed stdout")
        if byte=="\n" then pure (BS.concat (reverse acc)) else lineBytes (remaining-1) (byte:acc)
    bytes 0 chunks=pure (BS.concat (reverse chunks))
    bytes remaining chunks=do
      chunk<-BS.hGetSome handle remaining
      when (BS.null chunk) (protocolError "truncated response")
      bytes (remaining-BS.length chunk) (chunk:chunks)
