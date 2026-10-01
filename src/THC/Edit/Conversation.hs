{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Conversation (withConversation, conversationEffects, tickConversation, parseLaunch, renderReply, pauseLabel, renderTimestamp) where

import Prelude hiding (reads)
import Control.Exception (IOException, bracket, try, onException)
import Control.Monad (foldM, forM_, void, when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Time (UTCTime, TimeZone, getCurrentTime, getCurrentTimeZone, diffUTCTime, utcToLocalTime, formatTime, defaultTimeLocale)
import Data.IORef
import Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (XdgDirectory(..), getXdgDirectory, createDirectoryIfMissing, canonicalizePath, renameFile, removeFile)
import System.FilePath ((</>), takeDirectory, isAbsolute, makeRelative, splitDirectories)
import Data.Text.Encoding.Error (lenientDecode)
import qualified THC.Edit.Terminal as Terminal
import qualified THC.Edit.Consoles as C
import qualified THC.Edit.Build as B
import qualified THC.Edit.BuildJobs as Jobs
import System.IO (openBinaryTempFile, hClose)
import Text.Read (readMaybe)
import qualified THC.Edit.ACP as A
import THC.Edit.EditorMCP (editorServers)
import THC.Edit.AgentFiles
import THC.Edit.Buffer
import THC.Edit.Markdown (renderMarkdown)
import THC.Edit.Model hiding (prompt)
import System.Environment (lookupEnv)
import THC.Edit.Syntax (Style(..), bubbleTile)

-- One configured stdio provider; its protocol supplies models and tools.
data Phase = Initializing (Maybe Text) | Starting (Maybe Text) | Prompting | Steering Text | Setting deriving Eq
data Record = Reply Text Text | Activity Text Value | Pause Text deriving (Eq,Show)
data Approval = Permission Value [(Text,Text)] Value | Write Value Snapshot Text | Execute Value Terminal.TerminalConfig Int

data State = State
  { provider :: A.Launch, connection :: Maybe A.Client, session :: Maybe Text, project :: FilePath
  , pending :: M.Map Int Phase, queuedPrompt :: Maybe Text, transcript :: [Record]
  , reads :: M.Map FilePath Snapshot, approvals :: [(Int,Approval)], presented :: Maybe Int, deferredApproval :: Bool, nextApproval :: Int
  , queuedQueries :: [Text]
  , ownedTerminals :: S.Set Text
  , terminalWaiters :: M.Map Text [Value]
  , lastMessageAt :: Maybe UTCTime
  , lastRender :: (Int,Maybe Text,[Record]), lastSession :: Maybe (A.Launch,FilePath,Text)
  }
data ConversationState = ConversationState FilePath (IORef State) C.Consoles Jobs.BuildJobs

defaultLaunch :: A.Launch
defaultLaunch = A.Launch "codex-acp" [] []

withConversation :: (ConversationState -> IO a) -> IO a
withConversation action = C.withConsoles $ \consoles -> Jobs.withBuildJobs $ \jobs -> do
  directory<-getXdgDirectory XdgConfig "thc-edit"
  loaded<-try (BS.readFile (directory </> "agents.json")) :: IO (Either IOException BS.ByteString)
  let launch=either (const defaultLaunch) (either (const defaultLaunch) id . decodeLaunch) loaded
  previous<-try (BS.readFile (directory </> "agent-session.json")) :: IO (Either IOException BS.ByteString)
  let remembered=either (const Nothing) (\bytes -> decodeStrict' bytes >>= parseMaybe (withObject "session" $ \o -> do
        (raw::Value)<-o .: "provider"; config<-either fail pure (decodeLaunch (BL.toStrict (encode raw)))
        (,,) config <$> o .: "cwd" <*> o .: "sessionId")) previous
  bracket (do ref<-newIORef (State launch Nothing Nothing "." M.empty Nothing [] M.empty [] Nothing False 1 [] S.empty M.empty Nothing (0,Nothing,[]) remembered); pure (ConversationState directory ref consoles jobs)) closeConversation action

closeConversation :: ConversationState -> IO ()
closeConversation (ConversationState _ ref _ _) = readIORef ref >>= mapM_ A.stopClient . connection

launchValue :: A.Launch -> Value
launchValue launch=object ["executable" .= A.executable launch,"arguments" .= A.arguments launch,"environment" .= M.fromList (A.environment launch)]

decodeLaunch :: BS.ByteString -> Either String A.Launch
decodeLaunch bytes=do
  value<-eitherDecodeStrict' bytes
  maybe (Left "Expected executable, arguments array and environment object.") Right (parseMaybe (withObject "agent" $ \o ->
    A.Launch <$> o .: "executable" <*> o .:? "arguments" .!= [] <*> (M.toList <$> (o .:? "environment" .!= M.empty))) value) >>= validateLaunch

validateLaunch :: A.Launch -> Either String A.Launch
validateLaunch launch
  | null (A.executable launch) = Left "Enter an executable."
  | any (elem '\0') (A.executable launch:A.arguments launch++concatMap (\(k,v)->[k,v]) (A.environment launch)) = Left "NUL bytes are not valid in process arguments."
  | any (\(key,_) -> null key || '=' `elem` key) (A.environment launch) = Left "Invalid environment variable name."
  | otherwise = Right launch

parseLaunch :: Text -> Text -> Text -> Either String A.Launch
parseLaunch command args env = do
  arguments<-eitherDecodeStrict' (TE.encodeUtf8 args)
  environment<-eitherDecodeStrict' (TE.encodeUtf8 env)
  validateLaunch (A.Launch (T.unpack (T.strip command)) arguments (M.toList environment))

-- Temp files start private; never persist environment overrides to the project.
persist :: FilePath -> Value -> IO (Either Text ())
persist path value = do
  result<-try $ do
    createDirectoryIfMissing True (takeDirectory path)
    bracket (openBinaryTempFile (takeDirectory path) ".thc-settings-")
      (\(temporary,handle) -> ignore (hClose handle) >> ignore (removeFile temporary)) $ \(temporary,handle) -> do
        BL.hPut handle (encode value); hClose handle; renameFile temporary path
  pure (either (Left . T.pack . show) Right (result :: Either IOException ()))
  where ignore task=void (try task :: IO (Either IOException ()))

conversationEffects :: ConversationState -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
conversationEffects runtime fallback = foldM apply . (False,)
  where
    apply state@(True,_) _=pure state
    apply (_,d) (AgentAction action values) = (False,) <$> perform runtime action values d
    apply (_,d) effect = fallback d [effect]

perform :: ConversationState -> Text -> [Text] -> Desktop -> IO Desktop
perform runtime@(ConversationState directory ref consoles jobs) action values d = do
  previous<-readIORef ref
  now<-getCurrentTime
  zone<-getCurrentTimeZone
  let s=if action `elem` ["send","send-draft","steer-draft"] then stampReply now zone previous else previous
  case (action,values) of
    ("terminal",_) -> do
      shell<-fromMaybe "/bin/sh" <$> lookupEnv "SHELL"
      root<-canonicalizePath (maybe (startingDirectory d) treeRoot (sideTree d))
      openConsole consoles (Terminal.TerminalConfig shell [] [] root 80 24) d
    ("run",_) -> runTarget directory consoles jobs (Just B.Run) d
    ("compile",_) -> runTarget directory consoles jobs (Just B.Compile) d
    ("make",_) -> runTarget directory consoles jobs (Just B.Make) d
    ("build-stop",_) -> Jobs.stopBuildJob jobs d
    ("run-options",_) -> runTarget directory consoles jobs Nothing d
    ("run-config",_:settings) -> case B.parseBuildConfig settings of
      Left err -> pure (message "Build target" [err] d)
      Right config -> do
        root<-B.resolveBuildRoot d
        result<-persist (directory </> "run.json") (B.buildConfigValue root config)
        pure d {status=either id (const "Target saved. F9 builds; Ctrl+F9 runs.") result}
    ("terminal-input",[tid,text]) -> do
      result<-C.inputConsole consoles tid (TE.encodeUtf8 text)
      pure (either (\err -> d {status=err}) (const d) result)
    ("terminal-stop",_) -> case activeDocument d >>= documentLabel >>= T.stripPrefix "Terminal " of
      Just tid -> do result<-C.killConsole consoles tid; pure d {status=either id (const "Terminal command stopped.") result}
      Nothing -> pure d {status="Select a terminal window first."}
    ("options",_) -> pure d {dialog=Just (Dialog "Agents" (AgentDialog "configure")
      [input "Executable" (T.pack (A.executable (provider s))),input "Arguments (JSON array)" (jsonText (A.arguments (provider s))),
       input "Environment (JSON object)" (jsonText (M.fromList (A.environment (provider s))))] 0 ["OK","Cancel"]
      ["ACP stdio provider. Arguments are passed without a shell."])}
    ("configure",_:command:args:env:_) -> case parseLaunch command args env of
      Left err -> pure (message "Invalid agent configuration" (wrapMessage (T.pack err)) d)
      Right config -> do
        result<-persist (directory </> "agents.json") (launchValue config)
        case result of
          Left err -> pure (message "Cannot save configuration" (wrapMessage err) d)
          Right () -> do
            mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
            mapM_ A.stopClient (connection s)
            writeIORef ref s {provider=config,connection=Nothing,session=Nothing,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty}
            pure d {status="Agent configuration saved."}
    ("show",_) -> do
      modifyIORef' ref (\state -> state {deferredApproval=False})
      pure (paint True s d)
    ("set-config",[ident,value])
      | busy s -> pure d {status="Wait for the current reply before changing its model."}
      | Just client<-connection s, Just sid<-session s,
        any (\option -> settingId option==ident && value `elem` map fst (settingChoices option)) (agentSettings d) -> do
          requestId<-A.request client "session/set_config_option" (object ["sessionId" .= sid,"configId" .= ident,"value" .= value])
          modifyIORef' ref (\state -> state {pending=M.insert requestId Setting (pending state)})
          pure d {status="Updating conversation settings...",agentReplying=True}
      | otherwise -> pure d {status="This conversation setting is unavailable."}
    ("copy",_) -> pure d {clipboard=rawTranscript (transcript s),status="Raw conversation copied."}
    ("send-draft",_) | busy s, let text=contents (composerBuffer d), not (T.null (T.strip text)) -> do
      let next=s {queuedQueries=queuedQueries s++[text],transcript=transcript s++[Reply "You" text]}
      writeIORef ref next
      pure (paint True next d) {composerBuffer=newBuffer "",composerSelection=Selection 0 0,composerFocused=True,agentQueued=length (queuedQueries next),status="Query queued."}
    ("send-draft",_) | not (T.null (T.strip (contents (composerBuffer d)))) -> do
      next<-perform runtime "send" ["0",contents (composerBuffer d),"false","false","false"] d
      latest<-readIORef ref
      pure (if isNothing (connection latest) then next else next {agentReplying=busy latest,composerBuffer=newBuffer "",composerSelection=Selection 0 0,composerFocused=True})
    ("steer-draft",_) | not (agentSteering d) -> pure d {status="This provider does not advertise steering support."}
    ("steer-draft",_) | Just client<-connection s, Just sid<-session s, let text=contents (composerBuffer d), not (T.null (T.strip text)) -> do
      ident<-A.request client "_session/steering" (object ["sessionId" .= sid,"prompt" .= [object ["type" .= ("text"::Text),"text" .= text]]])
      writeIORef ref s {pending=M.insert ident (Steering text) (pending s),transcript=transcript s++[Reply "You" text]}
      pure d {composerBuffer=newBuffer "",composerSelection=Selection 0 0,composerFocused=True,status="Steering request sent."}
    ("send",_:prompt:selectionFlag:fileFlag:diagnosticFlag:_) | not (T.null (T.strip prompt)), not (busy s) -> do
      let context=contextText (selectionFlag=="true") (fileFlag=="true") (diagnosticFlag=="true") d
          full=prompt<>(if T.null context then "" else "\n\n"<>context)
          next=s {queuedPrompt=Just full,reads=sourceSnapshots d,transcript=transcript s++[Reply "You" prompt]}
      writeIORef ref next
      opened<-if isNothing (connection s) then start runtime Nothing d else sendQueued runtime d
      latest<-readIORef ref
      pure (paint True latest opened)
    ("cancel",_) -> do
      mapM_ (C.killConsole consoles) (S.toList (ownedTerminals s))
      forM_ (connection s) $ \client -> do
        forM_ (session s) $ \sid -> A.notify client "session/cancel" (object ["sessionId" .= sid])
        mapM_ (cancelApproval client . snd) (approvals s)
      writeIORef ref s {queuedPrompt=Nothing,approvals=[],presented=Nothing,deferredApproval=False}
      pure (dismissPermission d) {status="Cancellation requested."}
    ("new",_) | busy s -> pure d {status="Cancel the current reply before starting a new session."}
    ("new",_) -> do
      mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
      mapM_ A.stopClient (connection s)
      writeIORef ref s {connection=Nothing,session=Nothing,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],transcript=[],lastMessageAt=Nothing,reads=M.empty,approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty}
      start runtime Nothing d
    ("resume",_) | busy s -> pure d {status="Cancel the current reply before resuming a session."}
    ("resume",_) -> pure d {dialog=Just (Dialog "Resume conversation" (AgentDialog "load")
      [input "Session ID" (maybe "" (\(_,_,sid)->sid) (lastSession s))] 0 ["Resume","Cancel"]
      ["The provider must support loading or resuming sessions."])}
    ("load",_:sid:_) | not (T.null (T.strip sid)), not (busy s) -> do
      mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
      mapM_ A.stopClient (connection s)
      writeIORef ref s {connection=Nothing,session=Nothing,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],transcript=[],lastMessageAt=Nothing,reads=sourceSnapshots d,approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty}
      start runtime (Just (T.strip sid)) d
    _ | Just suffix<-T.stripPrefix "approval:" action, Just token<-readMaybe (T.unpack suffix) -> decide runtime token values d
    _ -> pure d
  where input label text=Input label text (T.length text)

busy :: State -> Bool
busy s=not (M.null (pending s)) || queuedPrompt s/=Nothing

start :: ConversationState -> Maybe Text -> Desktop -> IO Desktop
start (ConversationState _ ref _ _) resume d = do
  s<-readIORef ref
  result<-try $ do
    root<-canonicalizePath (maybe (startingDirectory d) treeRoot (sideTree d))
    client<-A.startClient (provider s) root
    ident<-A.request client "initialize" (object ["protocolVersion" .= (1::Int),"clientInfo" .= object ["name" .= ("thc-edit"::Text),"version" .= ("0.1.0.0"::Text)],
      "clientCapabilities" .= object ["fs" .= object ["readTextFile" .= True,"writeTextFile" .= True],"terminal" .= Terminal.terminalAvailable]]) `onException` A.stopClient client
    pure (root,client,ident)
  case result of
    Left (err::IOException) -> do
      writeIORef ref s {queuedPrompt=Nothing}
      pure (message "Cannot start agent" (wrapMessage (T.pack (show err))) d)
    Right (root,client,ident) -> do
      writeIORef ref s {connection=Just client,project=root,pending=M.singleton ident (Initializing resume)}
      pure d {status="Connecting to ACP provider...",agentSteering=False,agentReplying=True,agentContextUsage=Nothing,agentSettings=[]}

sendQueued :: ConversationState -> Desktop -> IO Desktop
sendQueued (ConversationState _ ref _ _) d = do
  s<-readIORef ref
  case (connection s,session s,queuedPrompt s) of
    (Just client,Just sid,Just prompt) -> do
      ident<-A.request client "session/prompt" (object ["sessionId" .= sid,"prompt" .= [object ["type" .= ("text"::Text),"text" .= prompt]]])
      writeIORef ref s {queuedPrompt=Nothing,pending=M.insert ident Prompting (pending s)}
      pure d {status="Agent is replying...",agentReplying=True}
    _ -> pure d

tickConversation :: ConversationState -> Desktop -> IO Desktop
tickConversation runtime@(ConversationState _ ref consoles jobs) initial = do
  d<-C.tickConsoles consoles initial >>= Jobs.tickBuildJobs jobs
  flushTerminalWaiters runtime
  s<-readIORef ref
  events<-maybe (pure []) A.pollEvents (connection s)
  updated<-foldM (receive runtime) d events
  afterEvents<-readIORef ref
  advanced<-case queuedQueries afterEvents of
    text:rest | not (busy afterEvents), not (isNothing (connection afterEvents)), session afterEvents/=Nothing -> do
      writeIORef ref afterEvents {queuedQueries=rest,queuedPrompt=Just text,reads=sourceSnapshots updated}
      sendQueued runtime updated
    _ -> pure updated
  current<-readIORef ref
  -- Esc/Cancel of a permission dialog denies it; it must never leave the peer waiting.
  case presented current of
    Just token | not (isApprovalDialog token advanced) -> do
      forM_ (lookup token (approvals current)) $ \approval -> mapM_ (\client -> cancelApproval client approval) (connection current)
      modifyIORef' ref (\state -> state {approvals=filter ((/=token).fst) (approvals state),presented=Nothing})
    _ -> pure ()
  afterDismiss<-readIORef ref
  let widthNow=conversationWidth advanced
      redraw=lastRender afterDismiss/=(widthNow,session afterDismiss,transcript afterDismiss)
      rendered=(if redraw then paint False afterDismiss advanced else advanced) {agentReplying=busy afterDismiss,agentQueued=length (queuedQueries afterDismiss)}
  when redraw (modifyIORef' ref (\state -> state {lastRender=(widthNow,session state,transcript state)}))
  present runtime rendered

receive :: ConversationState -> Desktop -> A.Event -> IO Desktop
receive runtime@(ConversationState directory ref consoles _) d event = do
  s<-readIORef ref
  case event of
    A.Disconnected reason -> do
      mapM_ (C.releaseConsole consoles) (S.toList (ownedTerminals s))
      writeIORef ref s {connection=Nothing,session=Nothing,pending=M.empty,queuedPrompt=Nothing,queuedQueries=[],approvals=[],presented=Nothing,deferredApproval=False,ownedTerminals=S.empty,terminalWaiters=M.empty,
        transcript=transcript s++[Activity "Connection closed" (object ["message" .= reason])]}
      pure (dismissPermission d) {status="Agent disconnected.",agentSteering=False}
    A.Response ident result -> do
      writeIORef ref s {pending=M.delete ident (pending s)}
      case (M.lookup ident (pending s),result,connection s) of
        (Nothing,_,_) -> pure d
        (_,Left err,_) -> do
          modifyIORef' ref (\state -> state {queuedPrompt=Nothing,transcript=transcript state++[Activity "Request failed" err]})
          pure d {status="Agent request failed; see Conversation.",composerBuffer=case M.lookup ident (pending s) of
            Just (Steering text) | T.null (contents (composerBuffer d)) -> newBuffer text
            _ -> composerBuffer d}
        (Just (Initializing resume),Right value,Just client)
          | field "protocolVersion" value /= Just (1::Int) -> do
              A.stopClient client
              modifyIORef' ref (\state -> state {queuedPrompt=Nothing})
              pure d {status="Unsupported ACP protocol version."}
          | otherwise -> do
              let capabilities=fromMaybe Null (field "agentCapabilities" value)
                  loadSupported=field "loadSession" capabilities==Just True
                  resumeSupported=case field "sessionCapabilities" capabilities >>= field "resume" of Just (Object _) -> True; _ -> False
                  method=case resume of Nothing -> "session/new"; Just _ | loadSupported -> "session/load"; _ -> "session/resume"
              if resume/=Nothing && not loadSupported && not resumeSupported then pure d {status="This provider cannot resume sessions."}
              else do
                servers<-editorServers
                requestId<-A.request client method (object (["cwd" .= project s,"mcpServers" .= servers]++maybe [] (\sid->["sessionId" .= sid]) resume))
                modifyIORef' ref (\state -> state {pending=M.insert requestId (Starting resume) (pending state),session=resume})
                pure d {status="Opening agent session...",agentSteering=(field "_meta" value >>= field "steering" >>= field "supported")==Just True}
        (Just (Starting resumed),Right value,_) -> case field "sessionId" value <|> resumed of
          Nothing -> pure d {status="Agent returned no session ID."}
          Just sid -> do
            modifyIORef' ref (\state -> state {session=Just sid,lastSession=Just (provider state,project state,sid)})
            savedId<-persist (directory </> "agent-session.json") (object ["provider" .= launchValue (provider s),"cwd" .= project s,"sessionId" .= sid])
            sendQueued runtime d {agentSettings=parseAgentSettings value,status=either ("Session opened; could not save ID: "<>) (const ("Session "<>sid)) savedId}
        (Just Setting,Right value,_) -> pure d {agentSettings=parseAgentSettings value,contextMenu=Nothing,status="Conversation settings updated."}
        (Just (Steering text),Right value,_) -> case field "outcome" value :: Maybe Text of
          Just "injected" -> pure d {status="Follow-up added to the active turn."}
          Just "startedNewTurn" -> pure d {status="Follow-up started a new turn."}
          _ -> pure d {status="Steering failed; the follow-up remains in Conversation.",composerBuffer=if T.null (contents (composerBuffer d)) then newBuffer text else composerBuffer d}
        (Just Prompting,Right value,_) -> pure d {status="Agent: "<>fromMaybe "finished" (field "stopReason" value)}
        _ -> pure d
    A.Notification "session/update" params
      | field "sessionId" params==session s || any isStarting (M.elems (pending s)) -> do
          let update=fromMaybe Null (field "update" params)
              kind=fromMaybe "" (field "sessionUpdate" update :: Maybe Text)
          case kind of
            "agent_message_chunk" -> appendReply "Agent" update
            "user_message_chunk" -> appendReply "You" update
            "tool_call" -> recordTool update
            "tool_call_update" -> recordTool update
            "plan" -> modifyIORef' ref (\state -> state {transcript=transcript state++[Activity "Plan" update]})
            _ -> pure ()
          pure $ if kind=="config_option_update" then d {agentSettings=parseAgentSettings update,contextMenu=Nothing}
            else if kind=="usage_update" then case (field "used" update,field "size" update) of
            (Just used,Just size) | used>=0 && size>0 -> d {agentContextUsage=Just (used,size)}
            _ -> d
            else d
      | otherwise -> pure d
    A.Request ident method params -> case connection s of
      Nothing -> pure d
      Just client | field "sessionId" params/=session s || session s==Nothing -> A.respond client ident (Left (failure "Unknown session.")) >> pure d
      Just client -> incoming runtime client ident method params d
    _ -> pure d
  where
    isStarting Starting{}=True; isStarting _=False
    appendReply role update = case field "content" update of
      Just content | field "type" content==Just ("text"::Text),Just text<-field "text" content, not (T.null text) -> do
        now<-getCurrentTime
        zone<-getCurrentTimeZone
        modifyIORef' ref (\state -> let timed=stampReply now zone state in timed {transcript=appendChunk role text (transcript timed)})
      _ -> pure ()
    recordTool update=do
      now<-getCurrentTime
      modifyIORef' ref (\state -> state {lastMessageAt=Just now,transcript=mergeTool update (transcript state)})

pauseLabel :: Maybe UTCTime -> UTCTime -> TimeZone -> Maybe Text
pauseLabel previous now zone = case previous of
  Just before | diffUTCTime now before>=300 -> Just (T.pack (formatTime defaultTimeLocale "%b %-d, %H:%M" (utcToLocalTime zone now)))
  _ -> Nothing

stampReply :: UTCTime -> TimeZone -> State -> State
stampReply now zone s = s {lastMessageAt=Just now,
  transcript=transcript s++maybe [] (\label -> [Pause label]) (pauseLabel (lastMessageAt s) now zone)}

renderTimestamp :: Int -> Text -> [(Char,Style)]
renderTimestamp width label = map (,Comment) (T.unpack (T.replicate (max 0 ((width-T.length label) `div` 2)) " "<>T.take (max 0 width) label))

-- Tool records never pass through the Markdown parser.
appendChunk :: Text -> Text -> [Record] -> [Record]
appendChunk role text records = case reverse records of
  Reply previous body:rest | previous==role -> reverse rest++[Reply role (body<>text)]
  _ -> records++[Reply role text]

mergeTool :: Value -> [Record] -> [Record]
mergeTool update records = case field "toolCallId" update :: Maybe Text of
  Nothing -> records
  Just ident ->
    let merge (Activity old (Object previous))
          | old==ident, Object new<-update = Activity old (Object (KM.union (KM.filter (/=Null) new) previous))
        merge other=other
    in if any (\record -> case record of Activity old _ -> old==ident; _ -> False) records
       then map merge records else records++[Activity ident update]

incoming :: ConversationState -> A.Client -> Value -> Text -> Value -> Desktop -> IO Desktop
incoming runtime@(ConversationState _ ref consoles _) client ident method params d = do
  s<-readIORef ref
  case method of
    "session/request_permission" -> case permissionOptions params of
      [] -> A.respond client ident (Right cancelled) >> pure d
      options -> enqueueApproval runtime (Permission ident options (fromMaybe Null (field "toolCall" params))) >> pure d
    "fs/read_text_file" -> case parseMaybe (withObject "read" $ \o -> (,,) <$> o .: "path" <*> o .:? "line" .!= (1::Int) <*> o .:? "limit") params of
      Nothing -> bad "Expected a path and optional integer line and limit."
      Just (path,line,limit) -> do
        captured<-captureFile (project s) path d
        case captured of
          Left err -> bad err
          Right snap -> do
            if line<1 || maybe False (<0) limit then bad "Invalid line range."
            else do
              let content=snapshotText snap
                  buffer=newBuffer content
                  startOffset=if line>bufferLineCount buffer then T.length content else bufferLineOffset buffer (line-1)
                  endOffset=case limit of
                    Nothing -> T.length content
                    Just count | count>=bufferLineCount buffer-line+1 -> T.length content
                               | otherwise -> bufferLineOffset buffer (line-1+count)
                  chosen=T.take (max 0 (endOffset-startOffset)) (T.drop startOffset content)
              modifyIORef' ref (\state -> state {reads=M.insert (snapshotPath snap) snap (reads state)})
              A.respond client ident (Right (object ["content" .= chosen])); pure d
    "fs/write_text_file" -> case (field "path" params,field "content" params) of
      (Just path,Just content) -> do
        captured<-captureFile (project s) path d
        case captured of
          Left err -> bad err
          Right current -> do
            let snapshot=M.findWithDefault current (snapshotPath current) (reads s)
            enqueueApproval runtime (Write ident snapshot content)
            pure d
      _ -> bad "Expected path and content."
    "terminal/create" | Terminal.terminalAvailable -> case parseTerminal (project s) params of
      Left err -> bad err
      Right (config,limit) -> do
        checked<-try (canonicalizePath (Terminal.terminalDirectory config)) :: IO (Either IOException FilePath)
        case checked of
          Right root | let relative=makeRelative (project s) root, not (isAbsolute relative), ".." `notElem` splitDirectories relative ->
            enqueueApproval runtime (Execute ident config {Terminal.terminalDirectory=root} limit) >> pure d
          _ -> bad "Terminal directory is outside the session project."
    "terminal/output" -> terminalId $ \tid -> do
      result<-C.consoleOutput consoles tid
      A.respond client ident (either (Left . failure) (\(bytes,truncated,exited) -> Right (object
        (["output" .= TE.decodeUtf8With lenientDecode bytes,"truncated" .= truncated]++maybe [] (\code->["exitStatus" .= exitStatus code]) exited))) result)
      pure d
    "terminal/wait_for_exit" -> terminalId $ \tid -> do
      result<-C.consoleOutput consoles tid
      case result of
        Left err -> bad err
        Right (_,_,Just code) -> A.respond client ident (Right (exitStatus code)) >> pure d
        Right _ -> modifyIORef' ref (\state -> state {terminalWaiters=M.insertWith (++) tid [ident] (terminalWaiters state)}) >> pure d
    "terminal/kill" -> terminalId $ \tid -> C.killConsole consoles tid >>= replyUnit
    "terminal/release" -> terminalId $ \tid -> do
      result<-C.releaseConsole consoles tid
      modifyIORef' ref (\state -> state {ownedTerminals=S.delete tid (ownedTerminals state)})
      forM_ (M.findWithDefault [] tid (terminalWaiters s)) $ \waiter -> A.respond client waiter (Left (failure "Terminal released."))
      modifyIORef' ref (\state -> state {terminalWaiters=M.delete tid (terminalWaiters state)})
      replyUnit result
    _ -> A.respond client ident (Left (object ["code" .= (-32601::Int),"message" .= ("Unsupported client method: "<>method)])) >> pure d
  where
    bad err=A.respond client ident (Left (failure err)) >> pure d
    terminalId action=do
      state<-readIORef ref
      case field "terminalId" params of
        Just tid | S.member tid (ownedTerminals state) -> action tid
        _ -> bad "Unknown terminal ID for this session."
    replyUnit result=A.respond client ident (either (Left . failure) (const (Right (object []))) result) >> pure d

permissionOptions :: Value -> [(Text,Text)]
permissionOptions params=map (\(_,ident,name)->(ident,name)) . sortOn (\(kind,_,_)->not ("reject" `T.isPrefixOf` kind)) $
  mapMaybe (parseMaybe (withObject "permission" $ \o -> (,,) <$> o .: "kind" <*> o .: "optionId" <*> o .: "name")) (fromMaybe [] (field "options" params))

enqueueApproval :: ConversationState -> Approval -> IO ()
enqueueApproval (ConversationState _ ref _ _) approval=modifyIORef' ref (\s -> s {approvals=approvals s++[(nextApproval s,approval)],nextApproval=nextApproval s+1})

present :: ConversationState -> Desktop -> IO Desktop
present (ConversationState _ ref _ _) d = do
  s<-readIORef ref
  case (dialog d,presented s,approvals s) of
    (Nothing,Nothing,(token,approval):_) | not (deferredApproval s) -> do
      modifyIORef' ref (\state -> state {presented=Just token})
      let action="approval:"<>T.pack (show token)
      pure $ case approval of
        Permission _ options detail -> d {dialog=Just (Dialog "Agent permission" (AgentDialog action)
          [ListBox "Action" (map snd options) 0] 0 ["Choose","Review","Cancel"]
          (take 6 (wrapMessage (fromMaybe "Tool permission" (field "title" detail))++wrapMessage (jsonText detail))))}
        Execute _ config _ -> d {dialog=Just (Dialog "Run agent command" (AgentDialog action) [] 0 ["Run","Reject"]
          (take 7 (wrapMessage (T.pack (Terminal.terminalCommand config)) ++ wrapMessage (jsonText (Terminal.terminalArguments config)) ++ wrapMessage (T.pack (Terminal.terminalDirectory config)))))}
        Write _ snap content -> (addReadOnly "Proposed agent edit" ("CURRENT BUFFER\n"<>snapshotText snap<>"\n\nPROPOSED CONTENT\n"<>content) d)
          {dialog=Just (Dialog "Apply agent edit" (AgentDialog action) [] 0 ["Apply","Review","Reject"]
            ["The proposed edit is open behind this dialog.","Apply saves it and preserves the old buffer in Undo."])}
    _ -> pure d

decide :: ConversationState -> Int -> [Text] -> Desktop -> IO Desktop
decide (ConversationState _ ref consoles _) token values d = do
  s<-readIORef ref
  case (connection s,lookup token (approvals s)) of
    (Just _,Just approval) | take 1 values==["1"],not (isExecute approval) -> do
      writeIORef ref s {presented=Nothing,deferredApproval=True}
      let detail=case approval of
            Permission _ _ value -> jsonText value
            Write _ snap text -> "FILE: "<>T.pack (snapshotPath snap)<>"\n\nCURRENT BUFFER\n"<>snapshotText snap<>"\n\nPROPOSED CONTENT\n"<>text
            _ -> ""
      pure (addReadOnly "Agent request" detail d) {status="Tools > Conversation returns to the pending approval."}
    (Just client,Just approval) -> do
      writeIORef ref s {approvals=filter ((/=token).fst) (approvals s),presented=Nothing}
      case approval of
        Permission ident options _ -> case values of
          "0":index:_ | Just n<-readMaybe (T.unpack index),n>=0, (chosen,_):_<-drop n options ->
            A.respond client ident (Right (object ["outcome" .= object ["outcome" .= ("selected"::Text),"optionId" .= chosen]])) >> pure d
          _ -> A.respond client ident (Right cancelled) >> pure d
        Write ident snap text | take 1 values==["0"] -> do
          result<-acceptWrite snap text d
          case result of
            Left err -> A.respond client ident (Left (failure err)) >> pure (message "Agent edit rejected" (wrapMessage err) d)
            Right changed -> do
              A.respond client ident (Right (object []))
              modifyIORef' ref (\state -> state {reads=maybe (reads state) (\fresh -> M.insert (snapshotPath fresh) fresh (reads state)) (M.lookup (snapshotPath snap) (sourceSnapshots changed))})
              pure changed
        Execute ident config limit | take 1 values==["0"] -> do
          result<-C.startConsole consoles config limit d
          case result of
            Left err -> A.respond client ident (Left (failure err)) >> pure d {status=err}
            Right (tid,changed) -> do
              modifyIORef' ref (\state -> state {ownedTerminals=S.insert tid (ownedTerminals state)})
              A.respond client ident (Right (object ["terminalId" .= tid]))
              pure changed
        _ -> cancelApproval client approval >> pure d
    _ -> pure d {status="Permission request expired."}
  where isExecute Execute{}=True; isExecute _=False

cancelApproval :: A.Client -> Approval -> IO ()
cancelApproval client approval=case approval of
  Permission ident _ _ -> A.respond client ident (Right cancelled)
  Execute ident _ _ -> A.respond client ident (Left (failure "User rejected the command."))
  Write ident _ _ -> A.respond client ident (Left (failure "User rejected the edit."))

isApprovalDialog :: Int -> Desktop -> Bool
isApprovalDialog token d = case dialog d of Just dg -> purpose dg==AgentDialog ("approval:"<>T.pack (show token)); _ -> False

dismissPermission :: Desktop -> Desktop
dismissPermission d=case dialog d of
  Just dg | AgentDialog action<-purpose dg,"approval:" `T.isPrefixOf` action -> d {dialog=Nothing}
  _ -> d

paint :: Bool -> State -> Desktop -> Desktop
paint force s d
  | not force && not (any ((==Just "Conversation").documentLabel) (M.elems (buffers d))) = d
  | otherwise = let
      width=conversationWidth d
      header="Session: "<>fromMaybe "not connected" (session s)<>"\n"
      styled=if null (transcript s) then plain Comment header else renderRecords width (zip [0..] (transcript s))
      text=T.pack (map fst styled)
      existing=find (\(_,doc)->documentLabel doc==Just "Conversation") (M.toList (buffers d))
      opened=case existing of
        Nothing -> addReadOnly "Conversation" text d
        Just (existingId,_) -> d {buffers=M.adjust (\doc->restyle doc {documentBuffer=newBuffer text}) existingId (buffers d)}
      bid=maybe (nextId d) fst existing
      adjust w | bufferId w/=bid = w
               | otherwise =
                   let rows=max 1 (windowContentRows opened (fromMaybe (newDocument (newBuffer "") Nothing) (M.lookup bid (buffers opened))) w)
                       oldLines=maybe 0 (bufferLineCount . documentBuffer . snd) existing
                       newLines=length (T.splitOn "\n" text)
                       atEnd=scrollRow w>=max 0 (oldLines-rows)
                       bounded n=max 0 (min (T.length text) n)
                   in w {scrollRow=if atEnd then max 0 (newLines-rows) else min (max 0 (newLines-rows)) (scrollRow w),
                         selection=Selection (bounded (anchor (selection w))) (bounded (caret (selection w)))}
      colored=opened {buffers=M.adjust (\doc -> doc {documentHighlight=styled,documentCursorVisible=False}) bid (buffers opened),windows=map adjust (windows opened)}
      focused=case find ((==bid).bufferId) (windows colored) of Just w | force -> focusWindow (windowId w) colored {composerFocused=True}; _ -> colored
    in focused
  where
    plain style=map (,style).T.unpack
    renderRecords _ []=[]
    renderRecords width (record:rest)=renderRecord width record++case rest of
      [] -> []
      next:_ -> plain Plain (if sameSpeaker (snd record) (snd next) then "\n" else "\n\n")++renderRecords width rest
    sameSpeaker (Reply a _) (Reply b _)=a==b
    sameSpeaker _ _=False
    renderRecord width (recordId,record)=case record of
      Pause label -> renderTimestamp width label
      Reply role text -> map (\(c,style) -> (c,case style of BubbleText _ outgoing base -> BubbleText recordId outgoing base; _ -> style)) (renderReply (videoMode d/=Nothing) width (role=="You") text)
      Activity ident value -> plain Pragma ("["<>fromMaybe "activity" (field "status" value)<>"] "<>fromMaybe ident (field "title" value))
        ++plain Plain (let detail=activityText value in if T.null detail then "" else "\n"<>detail)

-- Corner cells share the text rows. The spiked corner stays square, so the
-- top edge runs continuously into the tail. Terminal fonts use block fallbacks.
renderReply :: Bool -> Int -> Bool -> Text -> [(Char,Style)]
renderReply graphical requested outgoing text
  | width<6 = recolor (renderMarkdown width text)
  | otherwise = concat (zipWith renderLine [0::Int ..] rows)
  where
    width=max 1 requested
    contentRows=splitRows (recolor (renderMarkdown (width-5) text))
    leadingCode=case contentRows of
      first:_ -> any (\(_,style)->case style of BubbleText _ _ (CodeStyle _ _)->True; _->False) first
      [] -> False
    rows=if leadingCode then []:contentRows else contentRows
    columns chars=let t=T.pack (map fst chars) in displayColumn t (T.length t)
    bubbleWidth=maximum (0:map columns rows)
    background=BubbleStyle outgoing Plain
    edge=TerminalStyle (if outgoing then 0x00aaaa else 0xaaaaaa) 0x0000aa 0
    recolor=map (\(c,style)->(c,BubbleText 0 outgoing style))
    spaces style n=replicate (max 0 n) (' ',style)
    tile n=(bubbleTile graphical n,edge)
    side first lastRow leftSide
      | first && lastRow = if leftSide then if outgoing then tile 4 else tile 2
                                            else if outgoing then tile 3 else tile 5
      | first = if leftSide then if outgoing then tile 0 else (' ',background)
                             else if outgoing then (' ',background) else tile 1
      | lastRow = tile (if leftSide then 2 else 3)
      | otherwise = (' ',background)
    lastIndex=length rows-1
    -- The top margin belongs to the bubble, so copying still starts at its text.
    renderLine i chars =
      [('\n',if leadingCode && i==1 then background else BubbleText 0 outgoing Plain) | i>0] ++ line i chars
    line i chars =
      let first=i==0; lastRow=i==lastIndex
          body=side first lastRow True:chars++spaces background (bubbleWidth-columns chars)++[side first lastRow False]
          tailCell=if first then tile (if outgoing then 7 else 6) else (' ',Plain)
      in if outgoing then spaces Plain (width-bubbleWidth-3)++body++[tailCell]
                     else tailCell:body
    splitRows chars=case break ((=='\n').fst) chars of
      (row,[]) -> [row]
      (row,_:rest) -> row:splitRows rest

parseAgentSettings :: Value -> [AgentSetting]
parseAgentSettings value=mapMaybe parseOption (fromMaybe [] (field "configOptions" value))
  where
    parseOption option=do
      ident<-field "id" option
      name<-field "name" option
      category<-field "category" option
      if category `notElem` ["model","thought_level"] then Nothing else do
        current<-field "currentValue" option
        choices<-field "options" option :: Maybe [Value]
        let values=concatMap choice choices
        if null values then Nothing else Just (AgentSetting ident name category current values)
    choice option=case (field "value" option,field "name" option) of
      (Just value,Just name) -> [(value,name)]
      _ -> concatMap choice (fromMaybe [] (field "options" option))

rawTranscript :: [Record] -> Text
rawTranscript=T.intercalate "\n\n" . mapMaybe (\record -> case record of Reply role text -> Just (role<>"\n"<>text); Activity ident value -> Just (ident<>"\n"<>jsonText value); Pause _ -> Nothing)

field :: FromJSON a => Text -> Value -> Maybe a
field name=parseMaybe (withObject "object" (.: K.fromText name))
jsonText :: ToJSON a => a -> Text
jsonText=TE.decodeUtf8 . BL.toStrict . encode
failure :: Text -> Value
failure text=object ["code" .= (-32000::Int),"message" .= text]
cancelled :: Value
cancelled=object ["outcome" .= object ["outcome" .= ("cancelled"::Text)]]

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x <|> _=Just x
Nothing <|> y=y

parseTerminal :: FilePath -> Value -> Either Text (Terminal.TerminalConfig,Int)
parseTerminal root params = case parseMaybe parser params of
  Nothing -> Left "Invalid terminal command, arguments, environment or output limit."
  Just (command,args,env,cwd,limit)
    | not (isAbsolute cwd) || '\0' `elem` cwd -> Left "Expected an absolute terminal directory."
    | limit<0 || limit>16*1024*1024 -> Left "Terminal output limit must be between 0 and 16 MiB."
    | otherwise -> case validateLaunch (A.Launch command args env) of
        Left err -> Left (T.pack err)
        Right _ -> Right (Terminal.TerminalConfig command args env cwd 80 24,limit)
  where parser=withObject "terminal" $ \o -> (,,,,) <$> o .: "command" <*> o .:? "args" .!= []
          <*> (o .:? "env" .!= [] >>= mapM (withObject "environment" $ \v -> (,) <$> v .: "name" <*> v .: "value"))
          <*> o .:? "cwd" .!= root <*> o .:? "outputByteLimit" .!= (1024*1024)

exitStatus :: Int -> Value
exitStatus code=object ["exitCode" .= code,"signal" .= Null]

flushTerminalWaiters :: ConversationState -> IO ()
flushTerminalWaiters (ConversationState _ ref consoles _) = do
  s<-readIORef ref
  forM_ (connection s) $ \client -> forM_ (M.toList (terminalWaiters s)) $ \(tid,waiters) -> do
    result<-C.consoleOutput consoles tid
    let ready=case result of Left err -> Just (Left (failure err)); Right (_,_,Just code) -> Just (Right (exitStatus code)); _ -> Nothing
    forM_ ready $ \reply -> do
      mapM_ (\ident -> A.respond client ident reply) waiters
      modifyIORef' ref (\state -> state {terminalWaiters=M.delete tid (terminalWaiters state)})

openConsole :: C.Consoles -> Terminal.TerminalConfig -> Desktop -> IO Desktop
openConsole consoles config d = do
  result<-C.startConsole consoles config (1024*1024) d
  pure (either (\err -> message "Cannot start terminal" (wrapMessage err) d) snd result)

runTarget :: FilePath -> C.Consoles -> Jobs.BuildJobs -> Maybe B.BuildAction -> Desktop -> IO Desktop
runTarget directory consoles jobs action d = do
  root<-B.resolveBuildRoot d
  config<-B.loadBuildConfig directory root
  let setting label text=Input label text (T.length text)
      unsaved=any (\doc -> documentLabel doc==Nothing && dirty (documentBuffer doc)) (M.elems (buffers d))
      source=B.buildSource d
  case action of
    Nothing -> pure d {dialog=Just (Dialog "Build target" (AgentDialog "run-config")
      [setting "Compiler executable" (T.pack (B.buildExecutable config)),setting "Cabal target (optional)" (B.buildTarget config),
       setting "THC root (optional)" (B.buildTHCRoot config),setting "Runtime (THC only)" (B.buildRuntime config),
       ListBox "Toolchain" ["THC","GHC"] (if B.buildToolchain config==B.THC then 0 else 1),
       setting "Program arguments (JSON)" (jsonText (B.buildArguments config))]
      0 ["OK","Cancel"] ["F9 Make   Alt+F9 Compile   Ctrl+F9 Run",T.pack root])}
    Just task | unsaved -> pure (message (if task==B.Run then "Save before running" else "Save before building")
      ["Save modified source files before building the files on disk."] d)
    Just task -> do
      plan<-B.buildPlan task config root source
      case plan of
        Left err -> pure (message "Build target" [err] d)
        Right [(command,args)] | task==B.Run && Terminal.terminalAvailable ->
          openConsole consoles (Terminal.TerminalConfig command args [] root 80 24) d
        Right commands -> Jobs.startBuildJob jobs (T.pack (show task)) root commands d

conversationWidth :: Desktop -> Int
conversationWidth d = max 1 $ case [width (bounds w)-2 | w<-windows d,Just doc<-[M.lookup (bufferId w) (buffers d)],documentLabel doc==Just "Conversation"] of
  size:_ -> size
  [] -> fst (screenSize d)-treeWidthOf d-4

activityText :: Value -> Text
activityText value=T.intercalate "\n" (maybe [] (\input->["Input: "<>jsonText (input::Value)]) (field "rawInput" value)
  ++map renderContent (fromMaybe [] (field "content" value))
  ++maybe [] (\msg->[msg]) (field "message" value))
  where
    renderContent item=case field "type" item :: Maybe Text of
      Just "content" -> fromMaybe "[non-text content]" (field "content" item >>= field "text")
      Just "diff" -> "File: "<>fromMaybe "" (field "path" item)<>"\nPrevious:\n"<>fromMaybe "[new file]" (field "oldText" item)<>"\nProposed:\n"<>fromMaybe "" (field "newText" item)
      Just "terminal" -> "Terminal "<>fromMaybe "" (field "terminalId" item)
      _ -> jsonText item
