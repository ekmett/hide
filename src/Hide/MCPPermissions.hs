{-# LANGUAGE OverloadedStrings #-}
-- | Human approval policy and bounded, layered TOML configuration.
--
-- Calls consult current policy under desktop serialization and return waits as
-- continuations. The oldest live approval is shown; cancellation withdraws it.
-- Diff tickets retain their original source and own separate worker attempts.
-- Adoption/reply and cancellation share one short request claim; invalid attempts
-- keep the same editable approval. Worker retirement joins run outside UI locks.
-- Configuration edits replace supported token spans, reparse the result and use
-- checked saving rather than reformatting unrelated tables and comments.
-- Project agent limits may tighten global ceilings but cannot raise them.
module Hide.MCPPermissions
  ( Permissions, withPermissions, withPermissionsAt, permissionCall, permissionCallAs, policyEffects, tickPermissions
  , ReadAdmission, permissionReadCall, bufferEditor, readReference, resolveReadReference, bufferReader
  , permissionConfigPath, readEditorDefaults, writeEditorDefaults, readEditorDefaultsAt, writeEditorDefaultsAt
  , projectConfigPath, readEditorDefaultsFor, readAgentContextAt, writeAgentContextAt, readAgentContexts
  , readEnvironmentAt, writeEnvironmentAt, readKeybindingsAt, readKeybindingsFor
  , readAgentLimitsFor, updateConfigTable, readAutocompleteFor, writeAutocomplete, writeAutocompleteFor
  ) where

import Control.Concurrent (MVar, newEmptyMVar, newMVar, readMVar, tryReadMVar, tryPutMVar, withMVar)
import Control.Exception (IOException, bracket, onException, try, finally, evaluate, mask_)
import Control.Concurrent (forkIOWithUnmask)
import qualified Control.Concurrent.STM as STM
import Control.Exception (mask,catch,SomeException,SomeAsyncException,fromException,throwIO)
import Hide.BufferReadAdmission (ReadAdmission,withReadAdmission,readReference,resolveReadReference)
import qualified Hide.BufferReads as Reads
import Control.Concurrent.Async (Async,asyncWithUnmask,cancel,poll,waitCatch)
import Control.Monad (foldM, unless, when)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.IORef
import Data.List (find, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, createDirectoryIfMissing, getHomeDirectory, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), isAbsolute, takeDirectory, takeExtension)
import System.IO (IOMode(ReadMode), withBinaryFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import qualified Toml
import qualified Toml.Syntax as TS
import Hide.Buffer (newBuffer, revision, Selection(..))
import Hide.Plugin.BufferHost (BufferRef,BufferNamespace,newBufferNamespace,referenceId,BufferReader,newBufferReader,CapturedRead,BufferEditor,newBufferEditor,DiffResult)
import Hide.WorkspaceFilesMCP (PatchSource,PreparedPatch,capturePatchSource,preparePatch,commitPatch)
import Hide.Plugin.BufferHost (ContentVersion,captureVersion,versionCurrent)
import Hide.Files (FileState(..), saveFile)
import Hide.Model

type Tool = Desktop -> Text -> Value -> IO (Desktop,IO (Either Text Value))
type Core = Desktop -> [Effect] -> IO (Bool,Desktop)
data Mode = Enable | Prompt | Disable deriving (Eq,Show)
data Waiting = Waiting
  { ticket :: Int, toolName :: Text, arguments :: Value, operation :: WaitingOperation
  , active :: IORef Bool
  , approvalRequired :: Bool, patchCaller :: IO (Either Text ()), patchAttempt :: IORef (Maybe DiffAttempt), patchSource :: Maybe PatchSource, requestClaim :: MVar () }
data DiffAttempt = DiffAttempt (Maybe ContentVersion) (Async (Either Text PreparedPatch))
data WaitingOperation = WireOperation Tool (MVar (IO (Either Text Value))) | CaptureOperation CaptureSubmission | DiffOperation DiffSubmission
data CaptureSubmission = CaptureSubmission BufferRef (IO (Either Text ())) (MVar (Either Text CapturedRead)) (IORef Bool) (MVar ())
data DiffSubmission = DiffSubmission BufferRef ContentVersion Text (IO (Either Text ())) (MVar (Either Text DiffResult)) (IORef Bool) (MVar ()) (IORef (Maybe DiffAttempt))
data BufferSubmission = ReadSubmission CaptureSubmission | EditSubmission DiffSubmission
data BufferIngress = BufferIngress (STM.TBQueue BufferSubmission) (STM.TVar Bool)
data PermissionState = PermissionState { waiting :: [Waiting], nextTicket :: Int, displayed :: Maybe Text }
data Permissions = Permissions FilePath (M.Map Text Bool) (IORef PermissionState) BufferNamespace (IORef [MVar ()]) BufferIngress

permissionConfigPath :: IO FilePath
permissionConfigPath=do
  configured<-lookupEnv "XDG_CONFIG_HOME"
  base<-case configured of
    Just path | not (null path),isAbsolute path -> pure path
              | not (null path) -> ioError (userError "XDG_CONFIG_HOME must be absolute")
    _ -> (</> ".config") <$> getHomeDirectory
  canonicalizePath (base </> "thc" </> "config.toml")

withPermissions :: [Value] -> (Permissions -> IO a) -> IO a
withPermissions specs action=permissionConfigPath >>= \path -> withPermissionsAt path specs action

withPermissionsAt :: FilePath -> [Value] -> (Permissions -> IO a) -> IO a
withPermissionsAt path specs=bracket acquire release
  where
    acquire=Permissions path registry <$> newIORef (PermissionState [] 1 Nothing) <*> newBufferNamespace <*> newIORef [] <*> (BufferIngress <$> STM.newTBQueueIO 32 <*> STM.newTVarIO False)
    registry=M.fromList [(name,fromMaybe False (field "annotations" spec >>= field "readOnlyHint")) | spec<-specs,Just name<-[field "name" spec]]
    release runtime@(Permissions _ _ ref _ retired (BufferIngress inbox closed))=do
      incoming<-STM.atomically $ STM.writeTVar closed True >> STM.flushTBQueue inbox
      mapM_ (\submission->finishBufferSubmission runtime submission "Editor session closed before admission") incoming
      requests<-waiting <$> readIORef ref
      mapM_ (\request->finish runtime request (Left "Editor session closed before approval")) requests
      readIORef retired >>= mapM_ readMVar

-- | Initiate an allowed operation or queue approval under the desktop lock.
-- Run its continuation after releasing that lock so approval can make progress.
permissionCall :: Permissions -> Tool -> Tool
permissionCall = permissionCallAs (pure (Right ()))

-- | Host-owned live caller validation for requests deferred by policy approval.
-- Extension data cannot provide this validator; use it only at authenticated
-- transport admission. Immediate calls already passed that transport check.
permissionCallAs :: IO (Either Text ()) -> Permissions -> Tool -> Tool
permissionCallAs caller runtime@(Permissions path registry ref _ _ _) callback desktop name args=do
  closed<-sessionClosed runtime
  loaded<-if closed then pure (Left "Editor session closed") else readPolicies path
  case M.lookup name registry of
    Nothing -> denied "Unknown MCP tool"
    Just readonly -> case loaded of
      Left err -> denied err
      Right policies -> case M.findWithDefault (if readonly then Enable else Prompt) name policies of
        Disable -> denied "This MCP tool is disabled in Options > Agent Permissions"
        Enable -> callback desktop name args
        Prompt | name=="ask_user",Object fields<-args,KM.keys fields==["questionId"] -> do
          actor<-caller
          case actor of Left err->denied err; Right ()->callback desktop name args
        Prompt -> enqueue True
  where
    enqueue needsApproval=do
      s<-readIORef ref
      live<-filterMActive (waiting s)
      if length live>=32 then denied "Too many MCP requests are awaiting permission" else
        do
          promise<-newEmptyMVar
          enabled<-newIORef True
          attempt<-newIORef Nothing
          claim<-newMVar ()
          let request=Waiting (nextTicket s) name args (WireOperation callback promise) enabled needsApproval caller attempt Nothing claim
          writeIORef ref s {waiting=live++[request],nextTicket=nextTicket s+1}
          shown<-tickPermissions runtime desktop
          pure (shown,(readMVar promise >>= id) `onException` finish runtime request (Left "MCP permission request cancelled"))
    denied reason=pure (desktop,pure (Left reason))

-- | Use the ordinary policy/approval owner for one read_buffer capture. The host
-- supplies its attributed callback; anonymous inspection remains guest input.
-- No receipt exists while a request waits for approval. Work returned by the
-- callback runs later and can consume its snapshot, but cannot capture again.
permissionReadCall :: Permissions -> (ReadAdmission -> Tool) -> Tool
permissionReadCall runtime@(Permissions _ _ _ namespace _ _) callback desktop name args
  | name/="read_buffer" = pure (desktop,pure (Left "Read admission requires read_buffer"))
  | otherwise = permissionCall runtime admitted desktop name args
  where admitted d tool parameters=withReadAdmission namespace (sessionClosed runtime)
          (\receipt->callback receipt d tool parameters)

sessionClosed :: Permissions -> IO Bool
sessionClosed (Permissions _ _ _ _ _ (BufferIngress _ closed))=STM.readTVarIO closed

-- | Host-bound session reader. It requests fresh policy for every capture and
-- grants no Human provenance or reusable approval. Linked handlers only enqueue
-- and await; the owning tick admits the fixed operation under serialization.
bufferReader :: Permissions -> IO (Either Text ()) -> BufferReader
bufferReader (Permissions _ _ _ namespace _ (BufferIngress inbox closed)) caller=newBufferReader namespace $ \reference->mask $ \restore->do
  promise<-newEmptyMVar
  enabled<-newIORef True
  claim<-newMVar ()
  let submission=CaptureSubmission reference caller promise enabled claim
  accepted<-STM.atomically $ do
    stopped<-STM.readTVar closed
    full<-STM.isFullTBQueue inbox
    if stopped then pure (Left "Editor session closed before capture")
    else if full then pure (Left "Too many buffer captures are awaiting admission")
    else STM.writeTBQueue inbox (ReadSubmission submission) >> pure (Right ())
  case accepted of
    Left err->pure (Left err)
    Right ()->restore (readMVar promise) `onException` finishSubmission submission (Left "Buffer capture cancelled")

finishSubmission :: CaptureSubmission -> Either Text CapturedRead -> IO ()
finishSubmission submission@(CaptureSubmission _ _ _ _ claim) result=withMVar claim (\()->finishSubmissionOwned submission result)

finishSubmissionOwned :: CaptureSubmission -> Either Text CapturedRead -> IO ()
finishSubmissionOwned (CaptureSubmission _ _ promise enabled _) result=mask_ $ do
  writeIORef enabled False
  _<-tryPutMVar promise result
  pure ()

-- | Host-only fixed actor binding. Public callers submit exact read versions;
-- this transport grants neither Human authority nor reusable approval.
bufferEditor :: Permissions -> IO (Either Text ()) -> BufferEditor
bufferEditor (Permissions _ _ _ namespace retired (BufferIngress inbox closed)) caller=newBufferEditor namespace $ \reference version patch->mask $ \restore->do
  if T.length patch>1048576 then pure (Left "Diff exceeds 1 MiB characters") else do
    promise<-newEmptyMVar
    enabled<-newIORef True
    claim<-newMVar ()
    attempt<-newIORef Nothing
    let submission=DiffSubmission reference version patch caller promise enabled claim attempt
    accepted<-STM.atomically $ do
      stopped<-STM.readTVar closed
      full<-STM.isFullTBQueue inbox
      if stopped then pure (Left "Editor session closed before diff admission")
      else if full then pure (Left "Too many buffer requests are awaiting admission")
      else STM.writeTBQueue inbox (EditSubmission submission) >> pure (Right ())
    case accepted of
      Left err->pure (Left err)
      Right ()->restore (readMVar promise) `onException` finishDiffSubmission retired submission (Left "Buffer diff cancelled")

finishDiffSubmission :: IORef [MVar ()] -> DiffSubmission -> Either Text DiffResult -> IO ()
finishDiffSubmission retired submission@(DiffSubmission _ _ _ _ _ _ claim attempt) result=withMVar claim $ \()->do
  stopAttempt retired attempt
  finishDiffSubmissionOwned submission result

finishDiffSubmissionOwned :: DiffSubmission -> Either Text DiffResult -> IO ()
finishDiffSubmissionOwned (DiffSubmission _ _ _ _ promise enabled _ _) result=mask_ $ do
  writeIORef enabled False
  _<-tryPutMVar promise result
  pure ()

finishBufferSubmission :: Permissions -> BufferSubmission -> Text -> IO ()
finishBufferSubmission _ (ReadSubmission submission) err=finishSubmission submission (Left err)
finishBufferSubmission (Permissions _ _ _ _ retired _) (EditSubmission submission) err=finishDiffSubmission retired submission (Left err)

-- Fixed bounded transport only; PermissionState still has one serialized owner.
-- An interrupted extracted batch resolves every accepted reply before unwinding.
drainBufferRequests :: Permissions -> Desktop -> IO Desktop
drainBufferRequests runtime@(Permissions path registry state namespace _ (BufferIngress inbox _)) desktop=mask_ $ do
  incoming<-STM.atomically (STM.flushTBQueue inbox)
  let admitBatch=if null incoming then pure desktop else do
        policies<-readPolicies path
        foldM (admitSafely policies) desktop incoming
  admitBatch `onException` mapM_ (\submission->finishBufferSubmission runtime submission "Buffer request owner interrupted") incoming
  where
    admitSafely policies current submission=admit policies current submission `catch` \(err::SomeException)->
      case fromException err :: Maybe SomeAsyncException of
        Just _->throwIO err
        Nothing->finishBufferSubmission runtime submission "Buffer request admission failed" >> pure current
    decision policies name reference caller=do
      stopped<-sessionClosed runtime
      actor<-caller
      pure $ do
        readonly<-maybe (Left "Unknown MCP tool") Right (M.lookup name registry)
        ident<-maybe (Left "Buffer reference belongs to another editor session") Right (referenceId namespace reference)
        when stopped (Left "Editor session closed before admission")
        modes<-policies
        actor
        pure (ident,M.findWithDefault (if readonly then Enable else Prompt) name modes)
    admit policies current (ReadSubmission submission@(CaptureSubmission reference caller _ enabled claim))=withMVar claim $ \()->mask_ $ do
      live<-readIORef enabled
      when live $ do
        selected<-decision policies "read_buffer" reference caller
        case selected of
          Left err->finishSubmissionOwned submission (Left err)
          Right (_,Disable)->finishSubmissionOwned submission (Left "This MCP tool is disabled in Options > Agent Permissions")
          Right (_,Enable)->captureSubmissionOwned runtime submission current
          Right (ident,Prompt)->do
            original<-readIORef state
            liveRequests<-filterMActive (waiting original)
            if length liveRequests>=32 then finishSubmissionOwned submission (Left "Too many MCP requests are awaiting permission") else do
              attempt<-newIORef Nothing
              let request=Waiting (nextTicket original) "read_buffer" (object ["bufferId" .= ident]) (CaptureOperation submission) enabled True caller attempt Nothing claim
              writeIORef state original {waiting=liveRequests++[request],nextTicket=nextTicket original+1}
      pure current
    admit policies current (EditSubmission submission@(DiffSubmission reference expected patch caller _ enabled claim attempt))=withMVar claim $ \()->mask_ $ do
      live<-readIORef enabled
      if not live then pure current else do
        selected<-decision policies "buffer_apply_diff" reference caller
        case selected of
          Left err->reject err
          Right (_,Disable)->reject "This MCP tool is disabled in Options > Agent Permissions"
          Right (ident,mode)->do
            doc<-pure (M.lookup ident (buffers current))
            matches<-maybe (pure False) (versionCurrent expected . documentBuffer) doc
            if not matches then reject "Buffer identity or revision changed; read the buffer again" else do
              let args=object ["bufferId" .= ident,"revision" .= maybe 0 (revision . documentBuffer) doc,"diff" .= patch]
              case capturePatchSource current args of
                Left err->reject err
                Right captured->do
                  source<-evaluate captured
                  original<-readIORef state
                  liveRequests<-filterMActive (waiting original)
                  if length liveRequests>=32 then reject "Too many MCP requests are awaiting permission" else do
                    let request=Waiting (nextTicket original) "buffer_apply_diff" args (DiffOperation submission) enabled (mode==Prompt) caller attempt (Just source) claim
                    writeIORef state original {waiting=liveRequests++[request],nextTicket=nextTicket original+1}
                    -- Transfer is complete. Starting an attempt reacquires the
                    -- same claim after this admission releases it, on the tick.
                    pure current
      where reject err=finishDiffSubmissionOwned submission (Left err) >> pure current

-- Caller holds the same short request claim used by cancellation. Only known
-- host capture/actor operations run here; no extension handler or reply wait.
captureSubmissionOwned :: Permissions -> CaptureSubmission -> Desktop -> IO ()
captureSubmissionOwned runtime@(Permissions _ _ _ namespace _ _) submission@(CaptureSubmission reference caller _ _ _) desktop=do
  actor<-caller
  outcome<-case actor of
    Left err->pure (Left err)
    Right ()->withReadAdmission namespace (sessionClosed runtime) (\receipt->Reads.captureBuffer receipt desktop reference)
  finishSubmissionOwned submission outcome

filterMActive :: [Waiting] -> IO [Waiting]
filterMActive requests=fmap (map fst . filter snd) (mapM (\request->(request,) <$> readIORef (active request)) requests)

finish :: Permissions -> Waiting -> Either Text Value -> IO ()
finish runtime request result=withMVar (requestClaim request) (\()->finishOwned runtime request result)

-- Caller holds requestClaim. Result publication and cancellation are linearized.
finishOwned :: Permissions -> Waiting -> Either Text Value -> IO ()
finishOwned runtime request result=mask_ $ do
  atomicModifyIORef' (active request) (const (False,()))
  stopDiffAttempt runtime request
  case operation request of
    WireOperation _ promise->tryPutMVar promise (pure result) >> pure ()
    CaptureOperation submission->finishSubmissionOwned submission (case result of Left err->Left err; Right _->Left "Invalid capture reply")
    DiffOperation submission->finishDiffSubmissionOwned submission (case result of Left err->Left err; Right _->Left "Invalid diff reply")

-- An attempt belongs to a ticket, but failure does not end that ticket. Retire
-- joins run on their own thread; neither tick nor dialog submission waits for a
-- worker's cancellation/finalizer while holding the desktop lock.
stopDiffAttempt :: Permissions -> Waiting -> IO ()
stopDiffAttempt (Permissions _ _ _ _ retired _) request=stopAttempt retired (patchAttempt request)

-- The transport and Waiting ticket share this single attempt cell from birth.
stopAttempt :: IORef [MVar ()] -> IORef (Maybe DiffAttempt) -> IO ()
stopAttempt retired attempt=mask_ $ do
  old<-readIORef attempt
  case old of
    Nothing->pure ()
    Just (DiffAttempt _ worker)->do
      done<-newEmptyMVar
      _<-forkIOWithUnmask (\unmask->unmask (cancel worker >> waitCatch worker >> pure ()) `finally` (tryPutMVar done () >> pure ()))
      atomicModifyIORef' retired (\doneList->(done:doneList,()))
      writeIORef attempt Nothing

reviewVersion :: Waiting -> Desktop -> IO (Maybe ContentVersion)
reviewVersion request desktop=case dialog desktop of
  Just dg | purpose dg==PermissionDialog (approvalAction request)->case [b | TextArea "diff" True b _ _ _<-fields dg] of
    b:_->Just <$> captureVersion b
    _->pure Nothing
  _->pure Nothing

startDiffAttempt :: Permissions -> Waiting -> Value -> Desktop -> IO Desktop
startDiffAttempt runtime request edited desktop=case patchSource request of
  Nothing->diffFailure runtime request "Missing original diff source" desktop
  Just source->withMVar (requestClaim request) $ \()->mask_ $ do
    live<-readIORef (active request)
    if not live then pure desktop else do
      stopDiffAttempt runtime request
      let prepare=preparePatch source (if approvalRequired request then field "diff" (arguments request) else Nothing) edited
      review<-reviewVersion request desktop
      worker<-asyncWithUnmask (\unmask->unmask prepare)
      writeIORef (patchAttempt request) (Just (DiffAttempt review worker))
      pure desktop {status="Preparing exact buffer diff...",dialog=fmap (\dg->if purpose dg==PermissionDialog (approvalAction request) then dg {body=[]} else dg) (dialog desktop)}

diffFailure :: Permissions -> Waiting -> Text -> Desktop -> IO Desktop
-- Preserve current fields/selection/review Undo and the live correction ticket.
diffFailure runtime request err desktop=withMVar (requestClaim request) (\()->diffFailureOwned runtime request err desktop)

diffFailureOwned :: Permissions -> Waiting -> Text -> Desktop -> IO Desktop
diffFailureOwned runtime request err desktop
  | approvalRequired request = pure desktop {status="Diff not applied: "<>err,
      dialog=fmap (\dg->if purpose dg==PermissionDialog (approvalAction request)
        then dg {body=T.chunksOf (max 1 (width (dialogRect desktop dg)-6)) ("Diff not applied: "<>err)} else dg) (dialog desktop)}
  | otherwise = finishOwned runtime request (Left err) >> pure desktop {status="Diff not applied: "<>err}

drainDiffAttempts :: Permissions -> Desktop -> IO Desktop
drainDiffAttempts runtime@(Permissions path _ ref _ retired _) initial=do
  s<-readIORef ref
  live<-filterMActive (waiting s)
  result<-foldM step initial live
  observed<-readIORef retired >>= mapM (\done->(done,) <$> tryReadMVar done)
  let completed=[done | (done,Just ())<-observed]
  atomicModifyIORef' retired (\current->(filter (`notElem` completed) current,()))
  pure result
  where
    step desktop request=do
      attempt<-readIORef (patchAttempt request)
      case attempt of
        Nothing->do
          enabled<-readIORef (active request)
          if enabled && not (approvalRequired request) && toolName request=="buffer_apply_diff"
            then startDiffAttempt runtime request (arguments request) desktop else pure desktop
        Just (DiffAttempt review worker)->do
          completed<-poll worker
          case completed of
            Nothing->pure desktop
            Just outcome->withMVar (requestClaim request) $ \()->mask_ $ do
              writeIORef (patchAttempt request) Nothing
              live<-readIORef (active request)
              currentReview<-case review of
                Nothing->pure (not (approvalRequired request))
                Just expected->case dialog desktop of
                  Just dg | purpose dg==PermissionDialog (approvalAction request)->case [b | TextArea "diff" True b _ _ _<-fields dg] of
                    b:_->versionCurrent expected b
                    _->pure False
                  _->pure False
              if not live then pure desktop else if not currentReview then
                pure desktop {status="Diff review changed; approve the current review."}
              else do
                policies<-readPolicies path
                caller<-patchCaller request
                let admitted=do
                      modes<-policies
                      unless (M.lookup "buffer_apply_diff" modes/=Just Disable) (Left "This MCP tool was disabled before adoption")
                      unless (approvalRequired request || M.lookup "buffer_apply_diff" modes==Just Enable) (Left "This MCP tool now requires approval; submit again")
                      caller
                case admitted of
                  Left err->finishOwned runtime request (Left err) >> pure (closeReview request desktop) {status="Diff not applied: "<>err}
                  Right ()->case outcome of
                    Left _->diffFailureOwned runtime request "Diff preparation failed" desktop
                    Right (Left err)->diffFailureOwned runtime request err desktop
                    Right (Right prepared)->do
                      adopted<-commitPatch prepared desktop
                      case adopted of
                        Left err->diffFailureOwned runtime request err desktop
                        Right (updated,response)->do
                          stopDiffAttempt runtime request
                          case operation request of
                            DiffOperation submission->finishDiffSubmissionOwned submission (Right response)
                            _->finishOwned runtime request (Left "Invalid diff operation")
                          pure (closeReview request updated) {status="Applied exact buffer diff."}
    closeReview request desktop=case dialog desktop of
      Just dg | purpose dg==PermissionDialog (approvalAction request)->desktop {dialog=Nothing}
      _->desktop

-- | Display the oldest live approval and withdraw stale or cancelled prompts.
tickPermissions :: Permissions -> Desktop -> IO Desktop
tickPermissions runtime@(Permissions _ _ ref _ _ _) original=do
  desktop<-drainBufferRequests runtime original >>= drainDiffAttempts runtime
  s<-readIORef ref
  live<-filterMActive (waiting s)
  let staleApproval=case dialog desktop of
        Just dg | PermissionDialog action<-purpose dg,"approve:" `T.isPrefixOf` action -> all ((/=action).approvalAction) live
        _ -> False
      cleared=if staleApproval then desktop {dialog=Nothing} else desktop
  writeIORef ref s {waiting=live,displayed=if staleApproval then Nothing else displayed s}
  case (dialog cleared,filter approvalRequired live) of
    (Nothing,request:_) -> do
      modifyIORef' ref (\state->state {displayed=Just (approvalAction request)})
      pure cleared {dialog=Just (approvalReview cleared request)}
    _ -> pure cleared

-- doc-artifact: tools/docs-screenshots.hs permission-diff -> docs/site/screenshots/permission-diff.png
approvalReview :: Desktop -> Waiting -> Dialog
approvalReview desktop request=Dialog "Agent permission" (PermissionDialog (approvalAction request)) reviewFields selected ["Allow once","Deny"] []
  where
    args=arguments request
    patch=if toolName request=="buffer_apply_diff" then field "diff" args else Nothing
    metadata=[ReadOnly "Tool" (toolName request)]++case patch of
      Just _ -> [ReadOnly "File" (fromMaybe "Unknown buffer" $ do
        bid<-field "bufferId" args
        doc<-M.lookup bid (buffers desktop)
        pure (maybe ("Untitled #"<>T.pack (show bid)) (T.pack.filePath) (documentFile doc)))]
      Nothing -> []
    members=case args of Object entries -> sortOn fst [(K.toText key,value) | (key,value)<-KM.toList entries]; _ -> [("Arguments",args)]
    rows=[view name value | (name,value)<-members,not (name=="diff" && patch/=Nothing)]
    reviewFields=metadata++rows++[TextArea "diff" True (newBuffer text) (Selection 0 0) 0 0 | Just text<-[patch]]
    selected=if patch/=Nothing then length reviewFields-1 else 0
    view name value=let text=case value of String t -> t; _ -> TE.decodeUtf8 (BL.toStrict (encode value))
                    in if T.any (=='\n') text || T.length text>48 then TextArea name False (newBuffer text) (Selection 0 0) 0 0 else ReadOnly name text

approvalAction :: Waiting -> Text
approvalAction request="approve:"<>T.pack (show (ticket request))

-- | Handle permission-dialog decisions and delegate other effects.
policyEffects :: Permissions -> Core -> Core
policyEffects runtime fallback desktop effects=foldM apply (False,desktop) effects
  where
    apply result@(True,_) _=pure result
    apply (_,d) (PermissionAction action values)=(False,) <$> permissionAction runtime action values d
    apply (_,d) effect=fallback d [effect]

permissionAction :: Permissions -> Text -> [Text] -> Desktop -> IO Desktop
permissionAction runtime@(Permissions path registry ref _ _ _) action values desktop=do
  s<-readIORef ref
  if action=="show" then showSettings runtime desktop else
    if displayed s/=Just action then pure desktop {status="Permission dialog expired."} else
      case action of
        "settings" -> case values of
          "0":index:_ | Just n<-readMaybe (T.unpack index),Just (name,readonly)<-at (M.toList registry) n -> do
            policies<-readPolicies path
            let mode=either (const (if readonly then Enable else Prompt)) (M.findWithDefault (if readonly then Enable else Prompt) name) policies
                token="set:"<>name
            modifyIORef' ref (\state->state {displayed=Just token})
            pure desktop {dialog=Just (Dialog "Agent permission setting" (PermissionDialog token)
              [Radio name ["Enable","Prompt","Disable"] (modeIndex mode)] 0 ["Save","Back"]
              ["Enable runs immediately; Prompt asks once per request; Disable rejects.","Saved globally in thc/config.toml."])}
          _ -> close
        _ | Just name<-T.stripPrefix "set:" action,Just _<-M.lookup name registry -> case values of
          "0":selected:_ | Just mode<-readMaybe (T.unpack selected) >>= at [Enable,Prompt,Disable] -> do
            saved<-writeTable path ["editor","mcp","permissions"] (object [K.fromText name .= modeText mode])
            case saved of
              Left err -> pure desktop {status=err,dialog=Nothing} >>= showSettings runtime
              Right () -> do
                when (mode==Disable) $ readIORef ref >>= mapM_ (\request->when (toolName request==name) (finish runtime request (Left "This MCP tool was disabled before approval"))) . waiting
                showSettings runtime desktop {status="Agent permission saved."}
          _ -> showSettings runtime desktop
        _ | "approve:" `T.isPrefixOf` action -> case find ((==action).approvalAction) (waiting s) of
          Nothing -> close
          Just request -> do
            policies<-readPolicies path
            enabled<-readIORef (active request)
            let edited=case (toolName request,arguments request,drop 1 values) of
                  ("buffer_apply_diff",Object args,text:_) -> Object (KM.insert "diff" (String text) args)
                  _ -> arguments request
                denied=case policies of
                  Left err -> Just err
                  Right modes | M.lookup (toolName request) modes==Just Disable -> Just "This MCP tool is disabled"
                  _ -> Nothing
            case operation request of
              CaptureOperation submission->do
                withMVar (requestClaim request) $ \()->mask_ $ do
                  live<-readIORef (active request)
                  if not live || take 1 values/=["0"] then finishOwned runtime request (Left "MCP request denied")
                  else case denied of
                    Just err->finishOwned runtime request (Left err)
                    Nothing->captureSubmissionOwned runtime submission desktop {dialog=Nothing}
                modifyIORef' ref (\state->state {waiting=filter ((/=ticket request).ticket) (waiting state),displayed=Nothing})
                tickPermissions runtime desktop {dialog=Nothing}
              DiffOperation _->if enabled && denied==Nothing && take 1 values==["0"]
                then startDiffAttempt runtime request edited desktop
                else finish runtime request (Left (fromMaybe "MCP request denied" denied)) >> tickPermissions runtime desktop {dialog=Nothing}
              WireOperation callback promise->do
                  claimed<-atomicModifyIORef' (active request) (\live->(False,live))
                  modifyIORef' ref (\state->state {waiting=filter ((/=ticket request).ticket) (waiting state),displayed=Nothing})
                  if not claimed || take 1 values/=["0"] then finish runtime request (Left "MCP request denied") >> tickPermissions runtime desktop {dialog=Nothing}
                  else case denied of
                    Just err -> finish runtime request (Left err) >> tickPermissions runtime desktop {dialog=Nothing}
                    Nothing -> do
                      actor<-patchCaller request
                      result<-try (case actor of
                        Left err->pure (desktop {dialog=Nothing},pure (Left err))
                        Right ()->callback desktop {dialog=Nothing} (toolName request) edited)
                      case result of
                        Left (_::IOException) -> finish runtime request (Left "MCP tool failed after approval") >> tickPermissions runtime desktop {dialog=Nothing}
                        Right (updated,continuation) -> do
                          _<-tryPutMVar promise continuation
                          tickPermissions runtime updated
        _ -> close
  where
    close=modifyIORef' ref (\state->state {displayed=Nothing}) >> tickPermissions runtime desktop {dialog=Nothing}

showSettings :: Permissions -> Desktop -> IO Desktop
showSettings (Permissions path registry ref _ _ _) desktop=do
  policies<-readPolicies path
  let rows=[name<>"  ["<>modeText (either (const (if readonly then Enable else Prompt)) (M.findWithDefault (if readonly then Enable else Prompt) name) policies)<>"]" | (name,readonly)<-M.toList registry]
      notes=either (:[]) (const ["Select a tool to set Enable, Prompt or Disable.","Policies apply to every request, including cached tool schemas."]) policies
  modifyIORef' ref (\state->state {displayed=Just "settings"})
  pure desktop {dialog=Just (Dialog "Agent Permissions" (PermissionDialog "settings") [ListBox "Tool" rows 0] 0 ["Edit","Close"] notes)}

modeText :: Mode -> Text
modeText Enable="enable"
modeText Prompt="prompt"
modeText Disable="disable"
modeIndex :: Mode -> Int
modeIndex Enable=0
modeIndex Prompt=1
modeIndex Disable=2

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.:key))
at :: [a] -> Int -> Maybe a
at rows n | n<0=Nothing | otherwise=case drop n rows of value:_->Just value; _->Nothing

readPolicies :: FilePath -> IO (Either Text (M.Map Text Mode))
readPolicies path=do
  config<-readConfig path
  pure $ do
    (_,_,table)<-config
    policies<-lookupTable ["editor","mcp","permissions"] table
    traverse parseMode (maybe M.empty (fmap snd . tableMap) policies)
  where
    parseMode (Toml.Text' _ value)=case value of "enable"->Right Enable; "prompt"->Right Prompt; "disable"->Right Disable; _->Left "Invalid MCP permission mode in thc/config.toml"
    parseMode _=Left "MCP permission modes in thc/config.toml must be strings"

readEditorDefaults :: IO (Either Text Value)
readEditorDefaults=permissionConfigPath >>= readEditorDefaultsAt

-- | Find project configuration upward, stopping at nearer repository/package boundaries.
projectConfigPath :: FilePath -> IO FilePath
projectConfigPath input=do
  absolute<-canonicalizePath input
  directory<-doesDirectoryExist absolute
  let start=if directory then absolute else takeDirectory absolute
  search start start
  where
    search fallback directory=do
      let config=directory </> "thc.toml"
      exists<-doesFileExist config
      if exists then canonicalizePath config else do
        entries<-listDirectory directory
        let boundary=".git" `elem` entries || "cabal.project" `elem` entries || any ((==".cabal").takeExtension) entries
            parent=takeDirectory directory
        if boundary then canonicalizePath config
          else if parent==directory then canonicalizePath (fallback </> "thc.toml")
          else search fallback parent

-- | Read terminal context tables. Empty arrays explicitly unbind a command;
-- all validation and compilation happens before publishing the prepared maps.
readKeybindingsAt :: FilePath -> IO (Either Text (M.Map Text (M.Map Text (M.Map Text [Text]))))
readKeybindingsAt path=configIO $ do
  config<-readConfig path
  pure $ do
    (_,_,table)<-config
    selected<-lookupTable ["editor","keybindings"] table
    traverse platform (maybe M.empty tableMap selected)
  where
    platform (_,Toml.Table' _ values)=traverse context (tableMap values)
    platform _=Left "Keybinding platforms must be tables"
    context (_,Toml.Table' _ values)=traverse (keys . snd) (tableMap values)
    context _=Left "Keybinding contexts must be tables"
    keys (Toml.List' _ values) | length values<=64 = traverse text values
    keys _=Left "Keybindings must be arrays of at most 64 chords"
    text (Toml.Text' _ value) | T.length value<=80 = Right value
    text _=Left "Keybinding chords must be strings of at most 80 characters"

readKeybindingsFor :: FilePath -> IO (Either Text (M.Map Text (M.Map Text (M.Map Text [Text]))))
readKeybindingsFor directory=configIO $ do
  global<-permissionConfigPath >>= readKeybindingsAt
  project<-projectConfigPath directory >>= readKeybindingsAt
  pure (M.unionWith (M.unionWith M.union) <$> project <*> global)

readEditorDefaultsFor :: FilePath -> IO (Either Text Value)
readEditorDefaultsFor directory=configIO $ do
  global<-readEditorDefaults
  project<-projectConfigPath directory >>= readEditorDefaultsAt
  pure $ do
    globalValue<-global
    projectValue<-project
    case (globalValue,projectValue) of
      (Object globalEntries,Object projectEntries)->Right (Object (KM.union projectEntries globalEntries))
      _->Left "Editor defaults must be a table"

-- Environment entries preserve other configuration and comments, like defaults.
readEnvironmentAt :: FilePath -> IO (Either Text Value)
readEnvironmentAt path=configIO $ do
  config<-readConfig path
  pure $ do
    (_,_,table)<-config
    selected<-lookupTable ["editor","environment"] table
    values<-traverse (primitive . snd) (maybe M.empty tableMap selected)
    pure (object [K.fromText key .= value | (key,value)<-M.toList values])

writeEnvironmentAt :: FilePath -> Value -> IO (Either Text ())
writeEnvironmentAt path=writeTable path ["editor","environment"]

-- Autocomplete provider settings are human-owned, separate from display defaults.
readAutocompleteFor :: FilePath -> IO (Either Text Value)
readAutocompleteFor directory=configIO $ do
  globalPath<-permissionConfigPath
  projectPath<-projectConfigPath directory
  global<-load globalPath
  project<-load projectPath
  pure $ do
    a<-global; b<-project
    pure (Object (KM.union b a))
  where
    load path=do
      config<-readConfig path
      pure $ do
        (_,_,table)<-config
        selected<-lookupTable ["editor","autocomplete"] table
        values<-traverse (primitive . snd) (maybe M.empty tableMap selected)
        pure (KM.fromList [(K.fromText key,value) | (key,value)<-M.toList values])

writeAutocomplete :: Value -> IO (Either Text ())
writeAutocomplete values=permissionConfigPath >>= \path->writeTable path ["editor","autocomplete"] values

-- Update a project's existing override instead of saving an ineffective global
-- value beneath it. Projects without this table continue to use global settings.
writeAutocompleteFor :: FilePath -> Value -> IO (Either Text ())
writeAutocompleteFor directory values=configIO $ do
  path<-projectConfigPath directory
  loaded<-readConfig path
  case loaded >>= (\(_,_,table)->lookupTable ["editor","autocomplete"] table) of
    Left err->pure (Left err)
    Right (Just _)->writeTable path ["editor","autocomplete"] values
    Right Nothing->writeAutocomplete values


-- A project may tighten the human's global ceilings, never raise them. Read
-- both files before each spawn; malformed limits must not restore permissive defaults.
readAgentLimitsFor :: FilePath -> IO (Either Text (Int,Int))
readAgentLimitsFor directory=configIO $ do
  globalPath<-permissionConfigPath
  projectPath<-projectConfigPath directory
  global<-readConfig globalPath
  project<-readConfig projectPath
  pure $ do
    (_,_,globalTable)<-global
    (_,_,projectTable)<-project
    globalLimits<-limits (8,4) globalTable
    projectLimits<-limits globalLimits projectTable
    pure (min (fst globalLimits) (fst projectLimits),min (snd globalLimits) (snd projectLimits))
  where
    limits (agents,subagents) table=do
      settings<-lookupTable ["editor","agents"] table
      let entries=maybe M.empty tableMap settings
          limit name lower fallback=case M.lookup name entries of
            Nothing->Right fallback
            Just (_,Toml.Integer' _ value) | value>=lower && value<=64->Right (fromInteger value)
            _->Left ("editor.agents."<>name<>" must be an integer from "<>T.pack (show lower)<>" to 64")
      (,) <$> limit "max_agents" 1 agents <*> limit "max_subagents" 0 subagents

readAgentContextAt :: FilePath -> IO (Either Text Text)
readAgentContextAt path=do
  config<-readConfig path
  pure $ do
    (_,_,table)<-config
    agent<-lookupTable ["editor","agent"] table
    case agent >>= M.lookup "context" . tableMap of
      Nothing->Right ""
      Just (_,Toml.Text' _ context) | T.length context<=16384->Right context
                                   | otherwise->Left "Agent context exceeds 16384 characters"
      _->Left "Agent context must be a TOML string"

writeAgentContextAt :: FilePath -> Text -> IO (Either Text ())
writeAgentContextAt path context
  | T.length context>16384=pure (Left "Agent context exceeds 16384 characters")
  | otherwise=writeTable path ["editor","agent"] (object ["context" .= context])

readAgentContexts :: FilePath -> IO (Either Text Value)
readAgentContexts directory=configIO $ do
  globalPath<-permissionConfigPath
  projectPath<-projectConfigPath directory
  global<-readAgentContextAt globalPath
  project<-readAgentContextAt projectPath
  pure $ do
    globalText<-global
    projectText<-project
    Right (object ["global" .= object ["path" .= globalPath,"text" .= globalText],
      "project" .= object ["path" .= projectPath,"text" .= projectText]])

configIO :: IO (Either Text a) -> IO (Either Text a)
configIO action=do
  result<-try action
  pure $ case result of
    Left (_::IOException)->Left "Could not locate editor configuration"
    Right value->value

writeEditorDefaults :: Value -> IO (Either Text ())
writeEditorDefaults values=permissionConfigPath >>= \path->writeEditorDefaultsAt path values
readEditorDefaultsAt :: FilePath -> IO (Either Text Value)
readEditorDefaultsAt path=do
  config<-readConfig path
  pure $ do
    (_,_,table)<-config
    defaults<-lookupTable ["editor","defaults"] table
    values<-traverse (primitive . snd) (maybe M.empty tableMap defaults)
    pure (object [K.fromText key .= value | (key,value)<-M.toList values])
writeEditorDefaultsAt :: FilePath -> Value -> IO (Either Text ())
writeEditorDefaultsAt path values=case values of
  Object entries | all (`elem` allowed) (KM.keys entries),all scalar (KM.elems entries) -> writeTable path ["editor","defaults"] values
  _ -> pure (Left "Editor defaults must contain only supported primitive settings")
  where
    allowed=["backend","scale","screenMode","columns","rows","appearance","wordStar","blinkCursor","crtFilter","pixelateUnicode","materialIcons","streamerMode","bufferView","chatSubmit","macKeySymbols"]
    scalar String{}=True; scalar Number{}=True; scalar Bool{}=True; scalar _=False

primitive :: Toml.Value' a -> Either Text Value
primitive value=case value of
  Toml.Text' _ text -> Right (String text)
  Toml.Bool' _ boolean -> Right (Bool boolean)
  Toml.Integer' _ number -> Right (toJSON number)
  Toml.Double' _ number | not (isNaN number || isInfinite number) -> Right (toJSON number)
  _ -> Left "Editor defaults must contain primitive strings, booleans or finite numbers"

tableMap :: Toml.Table' a -> M.Map Text (a,Toml.Value' a)
tableMap (Toml.MkTable table)=table
lookupTable :: [Text] -> Toml.Table' a -> Either Text (Maybe (Toml.Table' a))
lookupTable [] table=Right (Just table)
lookupTable (key:rest) table=case M.lookup key (tableMap table) of
  Nothing -> Right Nothing
  Just (_,Toml.Table' _ nested) -> lookupTable rest nested
  _ -> Left "Configuration namespace must be a TOML table"

readConfig :: FilePath -> IO (Either Text (Maybe BS.ByteString,Text,Toml.Table' Toml.Position))
readConfig path=do
  loaded<-try (catchIOError (Just <$> withBinaryFile path ReadMode (\h->BS.hGet h 1048577)) (\err->if isDoesNotExistError err then pure Nothing else ioError err))
  pure $ case loaded of
    Left (_::IOException) -> Left "Could not read thc/config.toml"
    Right bytes -> do
      unless (maybe True ((<=1048576).BS.length) bytes) (Left "thc/config.toml exceeds 1 MiB")
      text<-either (const (Left "thc/config.toml is not valid UTF-8")) Right (TE.decodeUtf8' (fromMaybe BS.empty bytes))
      table<-either (const (Left "Invalid TOML in thc/config.toml; existing configuration was not changed")) Right (Toml.parse text)
      pure (bytes,text,table)

-- Serialize configuration writers in this process. saveFile also checks the freshly
-- read disk baseline before its atomic rename, preserving other settings.
configWriteLock :: MVar ()
configWriteLock=unsafePerformIO (newMVar ())
{-# NOINLINE configWriteLock #-}
writeTable :: FilePath -> [Text] -> Value -> IO (Either Text ())
writeTable path namespace values=withMVar configWriteLock $ \_ -> do
  config<-readConfig path
  case config of
    Left err -> pure (Left err)
    Right (baseline,text,_) -> case updateConfigTable namespace values text of
      Left err -> pure (Left err)
      Right updated -> do
        saved<-try $ do
          createDirectoryIfMissing True (takeDirectory path)
          saveFile (FileState path baseline) (newBuffer updated)
        pure $ case saved of
          Left (_::IOException) -> Left "Could not save thc/config.toml"
          Right (Left _) -> Left "Configuration changed on disk or could not be saved; retry after reviewing it"
          Right (Right _) -> Right ()

-- | Update supported primitive value spans and validate the candidate TOML.
-- Unsupported inline/dotted insertions fail without rewriting unrelated content.
updateConfigTable :: [Text] -> Value -> Text -> Either Text Text
updateConfigTable namespace (Object values) original=do
  _<-either (const (Left "Invalid TOML configuration")) Right (Toml.parse original)
  foldM update original (KM.toList values)
  where
    update text (key,value)=do
      table<-either (const (Left "Invalid TOML configuration")) Right (Toml.parse text)
      target<-lookupTable namespace table
      rendered<-case value of String{}->Right (json value); Bool{}->Right (json value); Number{}->Right (json value); _->Left "Only primitive TOML settings can be changed"
      candidate<-case target >>= M.lookup (K.toText key) . tableMap of
        Just (_,old) -> do
          _<-primitive old
          let position=Toml.valueAnn old
              start=Toml.posIndex position
          (_,end)<-either (const (Left "Could not locate the TOML setting")) Right
            (TS.scanToken TS.ValueContext (TS.Located position (T.drop start text)))
          pure (T.take start text<>rendered<>T.drop (Toml.posIndex (TS.locPosition end)) text)
        Nothing -> do
          expressions<-either (const (Left "Invalid TOML configuration")) Right (TS.parseRawToml text)
          let after=dropWhile (\expression->case expression of TS.TableExpr parts->map snd (toList parts)/=namespace; _->True) expressions
              assignment=json (String (K.toText key))<>" = "<>rendered<>"\n"
              startLine position=let index=Toml.posIndex position in index-T.length (T.takeWhileEnd (/='\n') (T.take index text))
              nextTable expression=case expression of TS.TableExpr parts->Just (startLine (fst (NE.head parts))); TS.ArrayTableExpr parts->Just (startLine (fst (NE.head parts))); _->Nothing
          pure $ case after of
            _:rest -> let index=fromMaybe (T.length text) (firstJust (map nextTable rest)); prefix=T.take index text
                      in prefix<>newline prefix<>assignment<>T.drop index text
            [] -> text<>newline text<>"["<>T.intercalate "." (map (json.String) namespace)<>"]\n"<>assignment
      unless (BS.length (TE.encodeUtf8 candidate)<=1048576) (Left "Updated thc/config.toml would exceed 1 MiB")
      _<-either (const (Left "Cannot add a setting to this inline/dotted TOML table; use an explicit editor configuration table")) Right (Toml.parse candidate)
      pure candidate
    json=TE.decodeUtf8 . BL.toStrict . encode
    newline text=if T.null text || "\n" `T.isSuffixOf` text then "" else "\n"
    firstJust []=Nothing
    firstJust (Just x:_)=Just x
    firstJust (Nothing:xs)=firstJust xs
updateConfigTable _ _ _=Left "Settings must be a JSON object"
