-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.AutocompleteACP
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Persistent ACP side-chat restricted to structured completion proposals.
--
-- A serialized prompt owner shares an immutable current-file snapshot through a
-- separate authenticated MCP submission slot. Only a matching structured proposal
-- after end_turn becomes a returned preview; only editor acceptance applies it.
-- Ordinary chat and hint turns are not parsed as edits. Native file,
-- terminal and permission requests are denied. Cancellation drains the prompt or
-- retires the connection before another prompt can use it.
module Hide.AutocompleteACP
  ( completionProvider ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (IOException, bracket, evaluate, mask, mask_, onException, try)
import Control.Monad (foldM, forM_, unless, void, when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Unique (newUnique,hashUnique)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Timeout (timeout)
import qualified Hide.ACP as ACP
import Hide.Plugin.Agent (ConfigChoice(..), Capabilities(..))
import Hide.AgentACP (parseCapabilities, filterPrivateCapabilities)
import Hide.Plugin.Completion hiding (completionConfiguration)
import qualified Hide.Plugin.Completion as C

data Session = Session ACP.Client (IORef (Maybe Text)) (IORef Bool) (IORef (Int,Value)) (IORef Value) !Int [Text]
data Pending = Pending Text CompletionContext Text [(Int,Int)] (Maybe [Proposal])
data State = State Bool (Maybe Session) (Maybe Pending)
data Transcript = Transcript (IORef [Text]) (IORef (M.Map Text Text))
data ACPCompletion = ACPCompletion CompletionStart Text (IORef (Maybe Text,Maybe Text)) (MVar ()) (MVar State) (IORef [Value]) Transcript

-- | Register the actual ACP completion implementation with the instruction text
-- owned by the first-party completion tools. Acquisition is lazy; this scope
-- owns the transport, pending snapshot and bounded feedback/trace state.
completionProvider :: Text -> CompletionProvider
completionProvider instructions=CompletionProvider $ \start use->
  withACPCompletion start instructions $ \completion->use CompletionDriver
    { requestCompletion=completeACP completion
    , reportCompletion=feedbackACP completion
    , sendCompletionHint=hintACP completion
    , C.completionConfiguration=completionConfiguration completion
    , discoverCompletionConfiguration=discoverACPConfiguration completion
    , configureCompletionAt=configureACPAt completion
    , pollCompletionTranscript=pollACPCompletionTranscript completion
    , completionServices=requestServices completion
    }

withACPCompletion :: CompletionStart -> Text -> (ACPCompletion -> IO a) -> IO a
withACPCompletion start instructions=bracket acquire close
  where
    acquire=ACPCompletion start instructions <$> newIORef (completionModel start,completionEffort start) <*> newMVar () <*> newMVar (State False Nothing Nothing) <*> newIORef [] <*> (Transcript <$> newIORef [] <*> newIORef M.empty)
    close completion@(ACPCompletion _ _ _ serial state _ _)=mask_ $ do
      modifyMVar_ state $ \(State _ session _) -> pure (State True session Nothing)
      retire completion
      -- Stopping the transport releases any in-flight RPC before scope exit.
      withMVar serial (const (pure ()))

retire :: ACPCompletion -> IO ()
retire completion@(ACPCompletion _ _ _ _ state _ _)=mask_ $ do
  flushTranscript completion
  session<-modifyMVar state $ \(State closed current _) -> pure (State closed Nothing Nothing,current)
  forM_ session $ \(Session client sid _ _ _ _ _) -> do
    readIORef sid >>= mapM_ (\ident -> ACP.notify client "session/cancel" (object ["sessionId" .= ident]))
    ACP.stopClient client

failure :: Text -> IO a
failure=ioError . userError . T.unpack

-- | Ask the private side chat for alternatives. Cancellation drains the prompt
-- before reuse, retiring only this connection if it cannot resynchronize.
-- Unstructured agent output is ignored, including text that looks like edits.
completeACP :: ACPCompletion -> CompletionInput -> IO [Proposal]
completeACP completion input=do
  pending@(Pending _ context _ _ _)<-either failure pure (prepareInput input)
  runPrompt completion (Just pending) (completionContextValue context)

-- | Send a human intent hint through the same warm side chat. Hints are ordinary
-- conversation, with no active source snapshot or completion submission slot.
hintACP :: ACPCompletion -> Text -> IO ()
hintACP completion text=do
  unless (not (T.null (T.strip text)) && T.length text<=16384 && not (T.any (=='\0') text))
    (failure "Autocomplete hint must contain at most 16384 characters and no NUL.")
  void (runPrompt completion Nothing (object ["intent" .= ("hint"::Text),"message" .= text]))

runPrompt :: ACPCompletion -> Maybe Pending -> Value -> IO [Proposal]
runPrompt completion@(ACPCompletion _ instructions _ serial state feedback _) pending context=withMVar serial $ \_ -> mask $ \restore -> do
  session@(Session _ sid first _ _ _ _) <- restore (getSession completion) `onException` retire completion
  ident<-readIORef sid >>= maybe (failure "Autocomplete session is unavailable.") pure
  firstUse<-readIORef first
  recent<-atomicModifyIORef' feedback (\old -> ([],old))
  let withFeedback=case context of Object value -> Object (KM.insert "feedback" (toJSON recent) value); _ -> context
      block text=object ["type" .= ("text"::Text),"text" .= text]
      prompt=[block instructions | firstUse]++[block (TE.decodeUtf8 (BL.toStrict (encode withFeedback)))]
      restoreFeedback=do
        modifyMVar_ state $ \(State closed current _) -> pure (State closed current Nothing)
        atomicModifyIORef' feedback (\current -> (take 16 (recent++current),()))
  (do
    modifyMVar_ state $ \(State closed current _) ->
      if closed then failure "Autocomplete is closed." else pure (State False current pending)
    record completion ("[prompt] "<>TE.decodeUtf8 (BL.toStrict (encode withFeedback)))
    outcome<-restore (rpc completion session "session/prompt" (object ["sessionId" .= ident,"prompt" .= prompt]))
    record completion ("[outcome] "<>fromMaybe "unknown" (field "stopReason" outcome))
    modifyMVar state $ \(State closed current active) ->
      let proposals=case active of
            Just (Pending requestId _ _ _ accepted) | Just requestId==pendingId pending,field "stopReason" outcome==Just ("end_turn"::Text) -> fromMaybe [] accepted
            _ -> []
      in pure (State closed current Nothing,proposals)
    ) `onException` restoreFeedback
  where
    pendingId (Just (Pending ident _ _ _ _))=Just ident
    pendingId Nothing=Nothing

-- | Queue bounded acceptance feedback for the next completion prompt. This
-- never starts a provider turn just to report that a suggestion was shown.
feedbackACP :: ACPCompletion -> CompletionFeedback -> Proposal -> IO ()
feedbackACP completion@(ACPCompletion _ _ _ _ _ feedback _) action proposal=do
  let status=case action of Shown -> "shown"; Accepted -> "accepted"; Ignored -> "ignored"; PartiallyAccepted _ -> "partially-accepted" :: Text
      partial=case action of PartiallyAccepted count -> Just (max 0 (min (T.length (proposalText proposal)) count)); _ -> Nothing
      entry=object ["status" .= status,"startOffset" .= proposalStart proposal,"endOffset" .= proposalEnd proposal
        ,"text" .= T.copy (T.take 2048 (proposalText proposal)),"truncated" .= (T.length (proposalText proposal)>2048),"acceptedCharacters" .= partial]
  atomicModifyIORef' feedback (\old -> (drop (max 0 (length old-15)) old++[entry],()))
  record completion ("[feedback] "<>status)

getSession :: ACPCompletion -> IO Session
getSession completion@(ACPCompletion start _ defaults _ state _ _)=mask $ \restore -> do
  State closed existing _<-readMVar state
  when closed (failure "Autocomplete is closed.")
  case existing of
    Just session -> pure session
    Nothing -> do
      -- Host policy freezes the effective environment and its redaction values
      -- at this actual lazy acquisition, including replacement after cancellation.
      (launch,credentials)<-completionAcquireLaunch start
      client<-ACP.startClientWithEnvironment launch (completionDirectory start)
      session@(Session _ sid _ configuration initializedRef _ _)<-Session client <$> newIORef Nothing <*> newIORef True <*> newIORef (0,Null) <*> newIORef Null <*> (hashUnique <$> newUnique) <*> pure credentials
      installed<-modifyMVar state $ \current@(State stopped _ active) ->
        if stopped then pure (current,False) else pure (State False (Just session) active,True)
      unless installed (ACP.stopClient client >> failure "Autocomplete is closed.")
      restore $ do
        initialized<-rpc completion session "initialize" (object
          ["protocolVersion" .= (1::Int),"clientInfo" .= object ["name" .= ("hide-autocomplete"::Text),"version" .= ("0.1.0.0"::Text)]
          ,"clientCapabilities" .= object ["fs" .= object ["readTextFile" .= False,"writeTextFile" .= False],"terminal" .= False]])
        unless (field "protocolVersion" initialized==Just (1::Int)) (failure "Unsupported autocomplete ACP protocol version.")
        writeIORef initializedRef initialized
        _<-rpc completion session "session/new" (object ["cwd" .= completionDirectory start,"mcpServers" .= completionServers start])
        ident<-readIORef sid >>= maybe (failure "Autocomplete provider returned no valid session.") pure
        (model,effort)<-readIORef defaults
        forM_ [("model",model),("thought_level",effort)] $ \(category,requested) -> forM_ requested $ \value -> do
          current<-snd <$> readIORef configuration
          let choices=[choice | choice<-configChoices (parseCapabilities initialized current),configCategory choice==category,value `elem` map fst (configValues choice)]
          case choices of
            [choice] -> when (configCurrent choice/=value) $ void $ rpc completion session "session/set_config_option"
              (object ["sessionId" .= ident,"configId" .= configId choice,"value" .= value])
            _ -> failure "Autocomplete model or effort is not advertised by this provider."
        pure session

-- | Read public, bounded choices without opening a connection. The receipt is
-- valid only for this exact live session and configuration version. Private
-- session/MCP credentials are filtered by the transcript's existing key owner.
completionConfiguration :: ACPCompletion -> IO (Maybe ((Int,Int),[ConfigChoice]))
completionConfiguration completion@(ACPCompletion _ _ _ _ state _ _)=do
  State closed current _<-readMVar state
  case current of
    Just (Session _ sid _ configuration initialized ident _) | not closed->do
      ready<-readIORef sid
      initial<-readIORef initialized
      (version,value)<-readIORef configuration
      keys<-privateKeys completion
      pure $ case ready of
        Nothing->Nothing
        Just _->Just ((ident,version),configChoices (filterPrivateCapabilities (maybe [] pure ready++keys) (parseCapabilities initial value)))
    _->pure Nothing

-- | Explicit choice discovery starts the same lazy connection. Call on the
-- completion worker; prompts, configuration and discovery share its serial lock.
discoverACPConfiguration :: ACPCompletion -> IO ()
discoverACPConfiguration completion@(ACPCompletion _ _ _ serial _ _ _)=
  withMVar serial (\_ -> void (getSession completion) `onException` retire completion)

-- | Change one currently advertised value on the exact captured session/version.
-- A retired receipt never initializes or configures its replacement. Native
-- permissions remain denied and the existing serialized RPC owner is reused.
configureACPAt :: ACPCompletion -> (Int,Int) -> Text -> Text -> IO (Either Text ())
configureACPAt completion@(ACPCompletion _ _ defaults serial state _ _) expected option value=withMVar serial $ \_->do
  snapshot<-completionConfiguration completion
  State closed current _<-readMVar state
  case (snapshot,current) of
    (Just (actual,choices),Just session@(Session _ sid _ _ _ _ _))
      | not closed,actual==expected,
        [choice]<-[choice | choice<-choices,configId choice==option,value `elem` map fst (configValues choice)]->do
          ident<-readIORef sid >>= maybe (failure "Autocomplete session is unavailable.") pure
          when (configCurrent choice/=value) $ void $ rpc completion session "session/set_config_option"
            (object ["sessionId" .= ident,"configId" .= option,"value" .= value])
          modifyIORef' defaults (\(model,effort)->if configCategory choice=="model" then (Just value,effort) else (model,Just value))
          pure (Right ())
    _->pure (Left "Completion setting expired or is no longer advertised.")

updateConfiguration :: IORef (Int,Value) -> Value -> IO ()
updateConfiguration ref value=atomicModifyIORef' ref (\(version,_)->let next=version+1 in next `seq` ((next,value),()))

-- One serialized caller consumes ACP replies. The authenticated MCP route can
-- fill the independent submission slot while this worker services the provider.
rpc :: ACPCompletion -> Session -> Text -> Value -> IO Value
rpc completion@(ACPCompletion _ _ _ _ state _ _) (Session client sid first configuration _ _ _) method params=mask $ \restore -> do
  requestId<-ACP.request client method params
  when (method=="session/prompt") (writeIORef first False)
  let interrupted
        | method/="session/prompt"=retire completion
        | otherwise=do
            modifyMVar_ state $ \(State closed current _) -> pure (State closed current Nothing)
            readIORef sid >>= mapM_ (\ident -> ACP.notify client "session/cancel" (object ["sessionId" .= ident]))
            record completion "[cancel] waiting for prompt acknowledgement"
            settled<-try (timeout 2000000 (loop requestId)) :: IO (Either IOException (Maybe Value))
            case settled of Right (Just _) -> flushTranscript completion; _ -> retire completion
  restore (timeout 30000000 (loop requestId) >>= maybe (failure "Autocomplete ACP request timed out.") pure)
    `onException` interrupted
  where
    loop requestId=do
      events<-ACP.pollEvents client
      answer<-foldM (handle requestId) Nothing events
      case answer of Just value -> pure value; Nothing -> threadDelay 1000 >> loop requestId
    handle requestId previous event=case event of
      ACP.Response ident result | ident==requestId -> case result of
        Left _ -> failure "Autocomplete ACP request failed."
        Right value -> do
          when (method=="session/prompt") (flushTranscript completion)
          when (method=="session/new") $ case field "sessionId" value of
            Just ident' | not (T.null ident'),T.length ident'<=4096,not (T.any (<' ') ident') -> writeIORef sid (Just ident')
            _ -> failure "Autocomplete provider returned an invalid session."
          when (method `elem` ["session/new","session/set_config_option"] && field "configOptions" value/=(Nothing::Maybe [Value]))
            (updateConfiguration configuration value)
          pure (Just value)
      ACP.Request ident "session/request_permission" _ -> do
        record completion "[denied] native permission request"
        ACP.respond client ident (Right (object ["outcome" .= object ["outcome" .= ("cancelled"::Text)]]))
        pure previous
      ACP.Request ident nativeMethod _ -> do
        record completion ("[denied] "<>nativeMethod)
        ACP.respond client ident (Left (object ["code" .= (-32601::Int),"message" .= ("Autocomplete only supports its supplied snapshot tools."::Text)]))
        pure previous
      ACP.Notification "session/update" value -> do
        expected<-readIORef sid
        when (expected/=Nothing && field "sessionId" value==expected) $ case field "update" value of
          Just update -> case field "sessionUpdate" update :: Maybe Text of
            Just "config_option_update" -> updateConfiguration configuration update
            Just kind | kind `elem` ["agent_message_chunk","agent_thought_chunk"] ->
              case field "content" update >>= field "text" of
                Just text -> streamTranscript completion (if kind=="agent_message_chunk" then "reply" else "thought") text
                Nothing -> pure ()
            Just kind | kind `elem` ["tool_call","tool_call_update"] ->
              record completion ("[tool] "<>fromMaybe "" (field "title" update)<>" "<>fromMaybe "" (field "status" update))
            _ -> pure ()
          _ -> pure ()
        pure previous
      ACP.Disconnected _ -> failure "Autocomplete ACP provider disconnected."
      _ -> pure previous

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "ACP field" (.: key))

-- Build offsets once from the supplied immutable source, retaining only the
-- permitted line boundaries. The source text never enters the tool context.
prepareInput :: CompletionInput -> Either Text Pending
prepareInput input=do
  unless (not (T.null (inputId input)) && T.length (inputId input)<=128 && not (T.any (<' ') (inputId input))) (Left "Invalid completion request identity.")
  unless (inputIntent input `elem` ["propose","alternate-next","alternate-previous"]) (Left "Invalid completion intent.")
  unless (inputFirstLine input>=0 && not (null nearby) && length (take 257 nearby)<=256 && sum (map T.length nearby)<=65536) (Left "Invalid completion context bounds.")
  unless (inputOffset input>=0 && inputOffset input<=size && inputVersion input>=0) (Left "Invalid completion caret or revision.")
  unless (BL.length (BL.take 32769 (encode (completionEditsValue (inputHistory input))))<=32768) (Left "Completion history exceeds its bound.")
  unless (length before==inputFirstLine input && length rows==length nearby && map (T.dropWhileEnd (=='\r')) rows==nearby) (Left "Completion context does not match its source snapshot.")
  let start=sum (map ((+1).T.length) before)
      offsets=scanl (\offset text -> min size (offset+T.length text+1)) start rows
      beforeCaret=T.take (inputOffset input) source
      row=T.count "\n" beforeCaret
      column=T.length (last (T.splitOn "\n" beforeCaret))
      context=CompletionContext (inputId input) (inputIntent input) (inputPath input) (inputVersion input)
        (inputOffset input) row column (inputFirstLine input) nearby (inputHistory input)
  pure (Pending (inputId input) context source (zip [inputFirstLine input..] offsets) Nothing)
  where
    source=inputText input
    size=T.length source
    nearby=inputNearby input
    (before,remaining)=splitAt (inputFirstLine input) (T.splitOn "\n" source)
    rows=take (length nearby) remaining

-- Typed services share the existing current-request slot. Wire codecs live in
-- hide-agents; direct in-process calls still receive the same bounds and exact
-- request admission here. No service captures a source outside this slot.
requestServices :: ACPCompletion -> CompletionServices
requestServices completion@(ACPCompletion _ instructions _ _ _ _ _)=CompletionServices
  { readCompletionContext= \ident->onRequest completion "read_completion_context" ident $ \pending@(Pending _ context _ _ _)->
      Right (pending,context)
  , readCompletionFile= \ident start count->onRequest completion "read_completion_file" ident $ \pending@(Pending _ _ source _ _)->do
      unless (start>=0 && start<=T.length source && count>=1 && count<=8192) (Left "Invalid current-file chunk range.")
      let text=T.take count (T.drop start source); next=start+T.length text
      pure (pending,CompletionChunk ident start text next (next==T.length source))
  , readCompletionSkill= \ident->onRequest completion "read_completion_skill" ident $ \pending->Right (pending,instructions)
  , submitCompletion= \ident entries->onRequest completion "submit_completion" ident $ \(Pending requestId context source offsets accepted)->do
      unless (accepted==Nothing) (Left "Completion was already submitted.")
      unless (length (take 9 entries)<=8) (Left "At most eight alternatives are allowed.")
      proposals<-mapM (\(LineReplacement start end text)->do
        unless (start<=end) (Left "Reversed completion line range.")
        a<-maybe (Left "Completion starts outside the supplied context.") Right (lookup start offsets)
        z<-maybe (Left "Completion ends outside the supplied context.") Right (lookup end offsets)
        unless (T.length text<=131072 && not (T.any (=='\0') text)) (Left "Invalid completion replacement text.")
        pure (Proposal a z text Nothing)) entries
      unless (sum (map (BS.length . TE.encodeUtf8 . proposalText) proposals)<=131072) (Left "Completion replacement exceeds 128 KiB.")
      pure (Pending requestId context source offsets (Just proposals),length proposals)
  }

-- The authenticated route grants access to this provider only. Request identity
-- and slot consumption are checked atomically, including retained service calls.
onRequest :: ACPCompletion -> Text -> Text -> (Pending -> Either Text (Pending,a)) -> IO (Either Text a)
onRequest completion@(ACPCompletion _ _ _ _ state _ _) name ident action=do
  result<-modifyMVar state $ \current@(State closed session active)->case active of
    Just pending@(Pending requestId _ _ _ _) | not closed->
      if ident/=requestId then pure (current,Left "Stale completion request.") else case action pending of
        Left problem->pure (current,Left problem)
        Right (next,value)->pure (State closed session (Just next),Right value)
    _->pure (current,Left "No current autocomplete request.")
  record completion ("[tool result] "<>name<>either (const " rejected") (const " accepted") result)
  pure result

-- | Drain the bounded debug transcript without issuing any ACP request.
pollACPCompletionTranscript :: ACPCompletion -> IO [Text]
pollACPCompletionTranscript (ACPCompletion _ _ _ _ _ _ (Transcript entries _))=
  atomicModifyIORef' entries (\old -> ([],old))

privateKeys :: ACPCompletion -> IO [Text]
privateKeys (ACPCompletion start _ _ _ state _ _)=do
  State _ current _<-readMVar state
  (sid,inherited)<-case current of Just (Session _ ref _ _ _ _ credentials) -> (,credentials) <$> readIORef ref; Nothing -> pure (Nothing,[])
  let headerKeys server=case field "headers" server of
        Just (Object headers) -> [value | String value<-KM.elems headers]
        _ -> [value | entry<-fromMaybe [] (field "headers" server),Just value<-[field "value" entry]]
      serverKeys=[value | server<-completionServers start,entry<-fromMaybe [] (field "env" server),Just value<-[field "value" entry]]++concatMap headerKeys (completionServers start)
      values=maybe [] pure sid++inherited++completionPrivateValues start++serverKeys
  pure (filter (not . T.null) (values++[token | value<-values,Just token<-[T.stripPrefix "Bearer " value]]))

record :: ACPCompletion -> Text -> IO ()
record completion text=do
  keys<-privateKeys completion
  appendTranscript completion (foldr (\key -> T.replace key "[private]") text keys)

appendTranscript :: ACPCompletion -> Text -> IO ()
appendTranscript (ACPCompletion _ _ _ _ _ _ (Transcript entries _)) text=do
  let bounded=T.copy (T.take 2048 text)
  _<-evaluate (T.length bounded)
  atomicModifyIORef' entries (\old -> (drop (max 0 (length old-63)) old++[bounded],()))

-- Do not publish a suffix that may be the first part of a split private key.
streamTranscript :: ACPCompletion -> Text -> Text -> IO ()
streamTranscript completion@(ACPCompletion _ _ _ _ _ _ (Transcript _ tails)) kind chunk=do
  keys<-privateKeys completion
  previous<-atomicModifyIORef' tails (\old -> (M.delete kind old,M.findWithDefault "" kind old))
  let scrubbed=foldr (\key -> T.replace key "[private]") (previous<>chunk) keys
      held=maximum (0:[n | key<-keys,n<-[1..min (T.length scrubbed) (T.length key-1)],T.take n key `T.isSuffixOf` scrubbed])
      shown=T.take (T.length scrubbed-held) scrubbed
  atomicModifyIORef' tails (\old -> (M.insert kind (T.copy (T.takeEnd held scrubbed)) old,()))
  unless (T.null shown) (appendTranscript completion ("["<>kind<>"] "<>shown))

flushTranscript :: ACPCompletion -> IO ()
flushTranscript completion@(ACPCompletion _ _ _ _ _ _ (Transcript _ tails))=do
  previous<-atomicModifyIORef' tails (\old -> (M.empty,old))
  forM_ (M.toList previous) $ \(kind,text) -> unless (T.null text) (appendTranscript completion ("["<>kind<>"] [private]"))
