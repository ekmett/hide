-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.MCPPermissions
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Human approval policy and bounded, layered TOML configuration.
--
-- Calls read fresh policy bytes on a bounded worker; admission stays serialized.
-- The owner never waits for policy IO. Fresh bytes may reuse a parsed decision
-- only when unchanged on the worker. Settings writes advance the owner epoch
-- before enqueueing and block adoption until the write completes. External edits
-- retain a finite read-to-adoption interval, not atomic filesystem revocation.
-- Accepted ingress and worker completion wake the session scheduler. Calls return
-- waits as continuations. The oldest live approval is shown; cancellation withdraws it.
-- Diff tickets retain their original source and own separate worker attempts.
-- Adoption/reply and cancellation share one short request claim; invalid attempts
-- keep the same editable approval. Worker retirement joins run outside UI locks.
-- Configuration edits replace supported token spans, reparse the result and use
-- checked saving rather than reformatting unrelated tables and comments.
-- Project agent limits may tighten global ceilings but cannot raise them.
module Hide.MCPPermissions
  ( Permissions, withPermissions, withPermissionsAt, guardPermissions, permissionCall, permissionCallAs, requestPermission, AdmittedBuild, permissionBuildInputAs, reserveAdmittedBuild, stepAdmittedBuild, cancelAdmittedBuild, policyEffects, tickPermissions, awaitPermissionWork
  , ReadAdmission, permissionReadCall, bufferEditor, readReference, resolveReadReference, bufferReader, windowReader
  , terminalServices
  , permissionConfigPath, readEditorDefaults, writeEditorDefaults, readEditorDefaultsAt, writeEditorDefaultsAt
  , projectConfigPath, readEditorDefaultsFor, readAgentContextAt, writeAgentContextAt, readAgentContexts
  , readEnvironmentAt, writeEnvironmentAt, readKeybindingsAt, readKeybindingsFor
  , readSystemOne, readWardenAt, writeWardenAt, readAgentLimitsFor, updateConfigTable, readAutocompleteFor, writeAutocomplete, writeAutocompleteFor
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
import Data.List (find, findIndex, nub, sortOn)
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
import Hide.Buffer (Buffer, newBuffer, prepareBuffer, revision, Selection(..))
import Hide.Plugin.BufferHost (BufferRef,BufferNamespace,newBufferNamespace,referenceId,BufferReader,newBufferReader,CapturedRead,ListedBuffer,BufferEditor,newBufferEditor,BufferDiff(..),DiffResult)
import Hide.WorkspaceFilesMCP (PatchSource,PreparedPatch,capturePatchSource,preparePatch,commitPatches)
import Hide.Plugin.BufferHost (ContentVersion,captureVersion,versionCurrent)
import Hide.Files (FileState(..), saveFile)
import qualified Hide.Plugin.Terminal as Terminal
import qualified Hide.Consoles as Consoles
import qualified Hide.Terminal as NativeTerminal
import qualified Hide.Build as Build
import Hide.Model
import Hide.WardenRuntime (WardenBinding,WardenReceipt,captureWardenBinding,wardenEnforces,runWarden,checkWarden)

type Tool = Desktop -> Text -> Value -> IO (Desktop,IO (Either Text Value))
type Core = Desktop -> [Effect] -> IO (Bool,Desktop)
data Mode = Enable | Prompt | Disable deriving (Eq,Show)
data Waiting = Waiting
  { ticket :: Int, toolName :: Text, arguments :: Value, operation :: WaitingOperation
  , active :: IORef Bool
  , approvalRequired :: Bool, patchCaller :: IO (Either Text ()), patchAttempt :: IORef (Maybe DiffAttempt), patchSources :: Maybe [PatchSource], requestClaim :: MVar (), policyStage :: IORef PolicyStage
  , wardenBinding :: !(Maybe WardenBinding), wardenReceipt :: IORef (Maybe WardenReceipt) }
type DiffReview = Maybe [(Text,ContentVersion)]
data DiffAttempt = DiffAttempt DiffReview (Async (Either Text [PreparedPatch]))
data WaitingOperation = WireOperation Tool (MVar (IO (Either Text Value)))
  | BuildInputOperation (AdmittedBuild -> Tool) (MVar (IO (Either Text Value)))
  | BuildAdoptionOperation AdmittedBuild
  | CaptureOperation CaptureSubmission | DiffOperation DiffSubmission | TerminalOperation TerminalSubmission
-- Minted only while the permission owner executes admitted editor input. The
-- original wire ticket may finish; this separate one-shot intent keeps its
-- approved policy and exact caller without retaining the original desktop.
data AdmittedBuild = AdmittedBuild Permissions Text Value Bool (IO (Either Text ())) (IORef BuildAdmissionState)
data BuildAdmissionState = BuildUnused | BuildReserved | BuildChecking Waiting
  | BuildAllowed Waiting Int | BuildRejected Text | BuildConsumed
data CaptureSubmission = CaptureSubmission BufferRef (IO (Either Text ())) (MVar (Either Text CapturedRead)) (IORef Bool) (MVar ())
  | ListingSubmission (IO (Either Text ())) (MVar (Either Text [ListedBuffer])) (IORef Bool) (MVar ())
  | WindowCaptureSubmission Reads.WindowReadTarget (IO (Either Text ())) (MVar (Either Text Reads.CapturedWindowRead)) (IORef Bool) (MVar ())
data DiffSubmission = DiffSubmission [BufferDiff] [Buffer] (IO (Either Text ())) (MVar (Either Text [DiffResult])) (IORef Bool) (MVar ()) (IORef (Maybe DiffAttempt))
-- Fixed host operations, never a plugin callback or JSON authority. The ingress
-- and Waiting owner share their claim and prepared resource from birth.
data TerminalRequest = ListTerminals | StartTerminal !Terminal.TerminalLaunch
  | OutputTerminal !Terminal.TerminalId !Int !Int
  | InputTerminal !Terminal.TerminalId !Text | StopTerminalRequest !Terminal.TerminalId
data TerminalResult = ListedTerminals !Terminal.TerminalListing | OpenedTerminal !Terminal.TerminalOpened
  | OutputTerminalPage !Terminal.TerminalPage | AcceptedTerminal
data PreparedTerminal = PreparedTerminalConsole !Consoles.PreparedConsole | PreparedTerminalReply !TerminalResult
data TerminalSubmission = TerminalSubmission !Consoles.Consoles !FilePath !TerminalRequest
  (IO (Either Text ())) (MVar (Either Text TerminalResult)) (IORef Bool) (MVar ())
  (IORef (Maybe (Async (Either Text PreparedTerminal))))
data RequestSubmission = ReadSubmission !(Maybe WardenBinding) CaptureSubmission | EditSubmission !(Maybe WardenBinding) DiffSubmission
  | SubmitTerminal !(Maybe WardenBinding) TerminalSubmission
  | WireSubmission !(Maybe WardenBinding) !Text !Value Tool (IO (Either Text ())) (MVar (IO (Either Text Value))) (IORef Bool) (MVar ())
data RequestIngress = RequestIngress (STM.TBQueue RequestSubmission) (STM.TVar Bool)
-- A request retains its policy phase while worker IO is pending. The queue is
-- transport only: Waiting remains the one request/cancellation owner.
type Policies = Either Text (M.Map Text Mode)
data PolicyUse = AdmitPolicy | BuildAdoptPolicy | TerminalAdoptPolicy | AllowPolicy Value DiffReview
  | AdoptPolicy DiffReview (Either Text [PreparedPatch])
data PolicyStage = PolicyReady | PolicyPending PolicyUse (Maybe (Int,STM.TMVar Policies))
  | TerminalPreparing | TerminalPrepared
  | WardenPreparing PolicyUse (Async ()) (STM.TMVar (Either SomeException WardenReceipt))
data PolicyTask = LoadPolicy (STM.TMVar Policies)
  | SavePolicy Text Mode (STM.TMVar Policies)
data SettingsUse = ListPolicies | EditPolicy Text Bool | SaveSetting Text Mode
data SettingsJob = SettingsJob Int SettingsUse (Maybe (STM.TMVar Policies))
data PolicyOwner = PolicyOwner
  { policyInbox :: STM.TBQueue PolicyTask, policyClosed :: STM.TVar Bool
  , policyWorker :: Async (), policyWake :: STM.TMVar (), policyEpoch :: IORef Int
  , settingsGeneration :: IORef Int, settingsJob :: IORef (Maybe SettingsJob) }
data PermissionState = PermissionState { waiting :: [Waiting], nextTicket :: Int, displayed :: Maybe Text }
data Permissions = Permissions FilePath (M.Map Text Bool) (IORef PermissionState) BufferNamespace (IORef [MVar ()]) RequestIngress PolicyOwner !(Maybe WardenBinding)

-- | /O(1)/. Bind subsequent calls to this host-issued Warden context without
-- changing the shared permission owner. Unguarded ticks retain each request's
-- original binding; the view grants no human approval or policy authority.
guardPermissions :: WardenBinding -> Permissions -> Permissions
guardPermissions binding (Permissions path registry state namespace retired ingress policy _)=
  Permissions path registry state namespace retired ingress policy (Just binding)

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
    acquire=do
      owner<-newPolicyOwner path registry
      Permissions path registry <$> newIORef (PermissionState [] 1 Nothing) <*> newBufferNamespace <*> newIORef [] <*> (RequestIngress <$> STM.newTBQueueIO 32 <*> STM.newTVarIO False) <*> pure owner <*> pure Nothing
    registry=M.fromList [(name,fromMaybe False (field "annotations" spec >>= field "readOnlyHint")) | spec<-specs,Just name<-[field "name" spec]]
    release runtime@(Permissions _ _ ref _ retired (RequestIngress inbox closed) policy _)=do
      incoming<-STM.atomically $ do
        STM.writeTVar closed True
        STM.writeTVar (policyClosed policy) True
        queued<-STM.flushTBQueue (policyInbox policy)
        mapM_ (resolvePolicy (Left "Editor session closed before policy decision")) queued
        STM.flushTBQueue inbox
      mapM_ (\submission->finishRequestSubmission runtime submission "Editor session closed before admission") incoming
      requests<-waiting <$> readIORef ref
      mapM_ (\request->finish runtime request (Left "Editor session closed before approval")) requests
      cancel (policyWorker policy)
      _<-waitCatch (policyWorker policy)
      readIORef retired >>= mapM_ readMVar

-- | Initiate an allowed operation or queue approval under the desktop lock.
-- Run its continuation after releasing that lock so approval can make progress.
permissionCall :: Permissions -> Tool -> Tool
permissionCall = permissionCallAs (pure (Right ()))

-- | Host-owned live caller validation after fresh policy IO and at final diff
-- adoption. Extension data cannot provide this validator; bind it at authenticated
-- transport admission so queued work cannot retain revoked caller authority.
permissionCallAs :: IO (Either Text ()) -> Permissions -> Tool -> Tool
permissionCallAs caller runtime callback=queueWirePermission caller runtime (WireOperation callback)

-- | Bind a one-shot build intent to actual admitted editor_input execution.
-- Anonymous input retains its existing session-only caller and agent policy;
-- no guest path is promoted to human authority by omitting an actor token.
permissionBuildInputAs :: IO (Either Text ()) -> Permissions -> (AdmittedBuild -> Tool) -> Tool
permissionBuildInputAs caller runtime callback desktop name args
  | name/="editor_input"=pure (desktop,pure (Left "Build input admission requires editor_input"))
  | otherwise=queueWirePermission caller runtime (BuildInputOperation callback) desktop name args

queueWirePermission :: IO (Either Text ()) -> Permissions -> (MVar (IO (Either Text Value)) -> WaitingOperation) -> Tool
queueWirePermission caller runtime@(Permissions _ registry ref _ _ _ _ binding) operationFor desktop name args=do
  closed<-sessionClosed runtime
  case M.lookup name registry of
    Nothing->denied "Unknown MCP tool"
    Just _ | closed->denied "Editor session closed"
    Just _->do
      s<-readIORef ref
      live<-filterMActive runtime (waiting s)
      if length live>=32 then denied "Too many MCP requests are awaiting permission" else do
        captured<-traverse captureWardenBinding binding
        promise<-newEmptyMVar
        enabled<-newIORef True
        attempt<-newIORef Nothing
        claim<-newMVar ()
        stage<-newIORef (PolicyPending AdmitPolicy Nothing)
        judgment<-newIORef Nothing
        let request=Waiting (nextTicket s) name args (operationFor promise) enabled False caller attempt Nothing claim stage captured judgment
        writeIORef ref s {waiting=live++[request],nextTicket=nextTicket s+1}
        shown<-tickPermissions runtime desktop
        pure (shown,(readMVar promise >>= id) `onException` finish runtime request (Left "MCP permission request cancelled"))
  where denied reason=pure (desktop,pure (Left reason))

-- | Worker entry to the same bounded permission ingress and Waiting owner.
-- The callback is private host code, executed only after policy/caller admission
-- under the owner; its returned continuation runs on this worker. No plugin
-- callback, Desktop or reusable approval crosses the public capability boundary.
requestPermission :: IO (Either Text ()) -> Permissions -> Tool -> Text -> Value -> IO (Either Text Value)
requestPermission caller runtime@(Permissions _ _ _ _ _ (RequestIngress inbox closed) owner binding) callback name args=mask $ \restore->do
  captured<-traverse captureWardenBinding binding
  promise<-newEmptyMVar
  enabled<-newIORef True
  claim<-newMVar ()
  let submission=WireSubmission captured name args callback caller promise enabled claim
  accepted<-STM.atomically $ do
    stopped<-STM.readTVar closed
    full<-STM.isFullTBQueue inbox
    if stopped then pure (Left "Editor session closed before admission")
    else if full then pure (Left "Too many requests are awaiting admission")
    else STM.writeTBQueue inbox submission >> STM.tryPutTMVar (policyWake owner) () >> pure (Right ())
  case accepted of
    Left err->pure (Left err)
    Right ()->restore (readMVar promise >>= id) `onException`
      (withMVar claim (\()->finishWireSubmissionOwned promise enabled "MCP permission request cancelled")
        `finally` signalPermissionWork runtime)

finishWireSubmissionOwned :: MVar (IO (Either Text Value)) -> IORef Bool -> Text -> IO ()
finishWireSubmissionOwned promise enabled reason=mask_ $ do
  writeIORef enabled False
  _<-tryPutMVar promise (pure (Left reason))
  pure ()

-- | Self-admitting terminal calls bound to a live host caller and session. The
-- captured launch directory is forced here; project resolution and process IO
-- run only on the admitted worker. No Desktop or reusable grant is retained.
terminalServices :: Permissions -> Consoles.Consoles -> IO (Either Text ()) -> FilePath -> IO Terminal.TerminalServices
terminalServices runtime consoles caller directory=do
  _<-evaluate (foldl' (\n char->n+fromEnum char) (0::Int) directory)
  pure Terminal.TerminalServices
    { Terminal.terminalList=receive ListTerminals (\result->case result of ListedTerminals value->Right value; _->invalid)
    , Terminal.terminalStart= \launch->receive (StartTerminal launch) (\result->case result of OpenedTerminal value->Right value; _->invalid)
    , Terminal.terminalOutput= \ident offset limit->receive (OutputTerminal ident offset limit) (\result->case result of OutputTerminalPage value->Right value; _->invalid)
    , Terminal.terminalInput= \ident text->receive (InputTerminal ident text) accepted
    , Terminal.terminalStop= \ident->receive (StopTerminalRequest ident) accepted }
  where
    invalid=Left "Invalid terminal result"
    accepted AcceptedTerminal=Right ()
    accepted _=invalid
    receive request project=do
      result<-requestTerminal runtime consoles directory caller request
      pure (result >>= project)

-- Typed callers have the same bounds as wire callers. Argument count consumes
-- budget even for empty strings; validation cannot traverse an unbounded argv.
validateTerminalRequest :: TerminalRequest -> Either Text ()
validateTerminalRequest request=case request of
  ListTerminals->Right ()
  StartTerminal launch
    | T.null (Terminal.terminalCommand launch)->Left "Expected a terminal executable"
    | Terminal.terminalOutputByteLimit launch<0 || Terminal.terminalOutputByteLimit launch>16777216->Left "Terminal output retention is limited to 0..16 MiB"
    | otherwise->strings 1048576 (Terminal.terminalCommand launch:maybe [] (pure . T.pack . take 1048577) (Terminal.terminalDirectory launch)++Terminal.terminalArguments launch)
  OutputTerminal ident offset limit->do
    identifier ident
    when (offset<0 || limit<1 || limit>131072) (Left "Use offset>=0 and limit 1..131072")
  InputTerminal ident text->do
    identifier ident
    when (T.length text>65536 || BS.length (TE.encodeUtf8 text)>65536) (Left "Terminal input is limited to 64 KiB")
  StopTerminalRequest ident->identifier ident
  where
    identifier (Terminal.TerminalId ident)=when (T.null ident || T.length ident>128 || T.any (=='\0') ident) (Left "Invalid terminal ID")
    strings _ []=Right ()
    strings remaining (value:rest)
      | remaining<=0 || T.length value>=remaining=Left "Terminal launch exceeds 1 MiB"
      | T.any (=='\0') value=Left "Terminal launch contains a NUL character"
      | otherwise=let cost=BS.length (TE.encodeUtf8 value)+1
          in if cost>remaining then Left "Terminal launch exceeds 1 MiB" else strings (remaining-cost) rest

terminalArguments :: TerminalRequest -> (Text,Value)
terminalArguments request=case request of
  ListTerminals->("terminal_list",object [])
  StartTerminal launch->("terminal_start",object (["command" .= Terminal.terminalCommand launch,
    "args" .= Terminal.terminalArguments launch,"outputByteLimit" .= Terminal.terminalOutputByteLimit launch]++
    ["cwd" .= directory | Just directory<-[Terminal.terminalDirectory launch]]))
  OutputTerminal ident offset limit->("terminal_output",object ["terminalId" .= Terminal.terminalIdText ident,"offset" .= offset,"limit" .= limit])
  InputTerminal ident text->("terminal_input",object ["terminalId" .= Terminal.terminalIdText ident,"text" .= text])
  StopTerminalRequest ident->("terminal_stop",object ["terminalId" .= Terminal.terminalIdText ident])

requestTerminal :: Permissions -> Consoles.Consoles -> FilePath -> IO (Either Text ()) -> TerminalRequest -> IO (Either Text TerminalResult)
requestTerminal runtime@(Permissions _ _ _ _ retired (RequestIngress inbox closed) owner binding) consoles directory caller request=case validateTerminalRequest request of
  Left err->pure (Left err)
  Right ()->mask $ \restore->do
    captured<-traverse captureWardenBinding binding
    promise<-newEmptyMVar
    enabled<-newIORef True
    claim<-newMVar ()
    attempt<-newIORef Nothing
    let submission=TerminalSubmission consoles directory request caller promise enabled claim attempt
    accepted<-STM.atomically $ do
      stopped<-STM.readTVar closed
      full<-STM.isFullTBQueue inbox
      if stopped then pure (Left "Editor session closed before terminal admission")
      else if full then pure (Left "Too many requests are awaiting admission")
      else STM.writeTBQueue inbox (SubmitTerminal captured submission) >> STM.tryPutTMVar (policyWake owner) () >> pure (Right ())
    case accepted of
      Left err->pure (Left err)
      Right ()->restore (readMVar promise) `onException`
        (withMVar claim (\()->finishTerminalSubmissionOwned retired submission (Left "Terminal request cancelled"))
          `finally` signalPermissionWork runtime)

terminalLifetime :: TerminalSubmission -> (IORef Bool,MVar (),IO (Either Text ()))
terminalLifetime (TerminalSubmission _ _ _ caller _ enabled claim _)=(enabled,claim,caller)

finishTerminalSubmissionOwned :: IORef [MVar ()] -> TerminalSubmission -> Either Text TerminalResult -> IO ()
finishTerminalSubmissionOwned retired (TerminalSubmission _ _ _ _ promise enabled _ attempt) result=mask_ $ do
  writeIORef enabled False
  stopTerminalAttempt retired attempt
  _<-tryPutMVar promise result
  pure ()

-- Retirement only schedules joining/cleanup. A completed, unadopted launch is
-- still owned here, including cancellation after preparation but before policy.
stopTerminalAttempt :: IORef [MVar ()] -> IORef (Maybe (Async (Either Text PreparedTerminal))) -> IO ()
stopTerminalAttempt retired attempt=mask_ $ do
  old<-atomicModifyIORef' attempt (\current->(Nothing,current))
  case old of
    Nothing->pure ()
    Just worker->do
      done<-newEmptyMVar
      _<-forkIOWithUnmask (\unmask->unmask (do
        cancel worker
        result<-waitCatch worker
        case result of
          Right (Right (PreparedTerminalConsole console))->Consoles.closePreparedConsole console
          _->pure ()) `finally` (tryPutMVar done () >> pure ()))
      atomicModifyIORef' retired (\current->(done:current,()))

-- | Reserve the admitted input once for a newly captured build intent.
reserveAdmittedBuild :: AdmittedBuild -> IO Bool
reserveAdmittedBuild (AdmittedBuild runtime _ _ _ _ state)=do
  closed<-sessionClosed runtime
  if closed then pure False else atomicModifyIORef' state $ \current->case current of
    BuildUnused->(BuildReserved,True)
    _->(current,False)

-- | Queue fresh policy through the existing owner, then invoke only the fixed
-- prepared-build effect under its request claim. Nothing is pending/deferred;
-- Just is consumed, including a refusal. Original Prompt approval is not repeated.
-- A modal deferral drops the fresh check and queues another after the hold. The
-- filesystem read-to-adoption interval is finite, not atomic external revocation.
stepAdmittedBuild :: AdmittedBuild -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO (Maybe Desktop)
stepAdmittedBuild admission@(AdmittedBuild runtime@(Permissions _ _ ref _ _ _ owner _) name args _ caller state) core desktop=do
  current<-readIORef state
  if dialog desktop/=Nothing || questionActive desktop then do
    -- A modal hold ends this fresh-check lifetime, not the original intent.
    -- No policy reads repeat while held; the next open-owner step queues anew.
    case current of
      BuildChecking request->defer request
      BuildAllowed request _->defer request
      _->pure ()
    pure Nothing
  else case current of
    BuildReserved->do
      closed<-sessionClosed runtime
      requests<-readIORef ref
      live<-filterMActive runtime (waiting requests)
      if closed || length live>=32 then do
        atomicModifyIORef' state (\fresh->((case fresh of BuildReserved->BuildRejected (if closed then "Editor session closed" else "Too many MCP requests are awaiting permission"); _->fresh),()))
        stepAdmittedBuild admission core desktop
      else do
        enabled<-newIORef True
        attempt<-newIORef Nothing
        claim<-newMVar ()
        stage<-newIORef (PolicyPending BuildAdoptPolicy Nothing)
        judgment<-newIORef Nothing
        let request=Waiting (nextTicket requests) name args (BuildAdoptionOperation admission) enabled False caller attempt Nothing claim stage Nothing judgment
        reserved<-atomicModifyIORef' state (\fresh->case fresh of BuildReserved->(BuildChecking request,True); _->(fresh,False))
        when reserved (writeIORef ref requests {waiting=live++[request],nextTicket=nextTicket requests+1})
        pure Nothing
    BuildChecking _->pure Nothing
    BuildAllowed request issued->withMVar (requestClaim request) $ \()->do
      fresh<-readIORef state
      live<-readIORef (active request)
      epoch<-readIORef (policyEpoch owner)
      writing<-policyWriting owner
      actor<-caller
      closed<-sessionClosed runtime
      case fresh of
        BuildAllowed owned checked | ticket owned==ticket request && checked==issued->case actor of
          Left err->finishOwned runtime request (Left err) >> stepAdmittedBuild admission core desktop
          Right () | not live || closed->finishOwned runtime request (Left "Build caller or session ended") >> stepAdmittedBuild admission core desktop
                   | writing || epoch/=issued->do
                       writeIORef state (BuildChecking request)
                       writeIORef (policyStage request) (PolicyPending BuildAdoptPolicy Nothing)
                       pure Nothing
                   | otherwise->do
                       writeIORef state BuildConsumed
                       writeIORef (active request) False
                       Just . snd <$> core desktop [AdoptPreparedBuild Nothing]
        BuildConsumed->pure (Just desktop)
        _->pure Nothing
    BuildRejected err->writeIORef state BuildConsumed >> pure (Just desktop {status=err})
    BuildConsumed->pure (Just desktop)
    BuildUnused->pure (Just desktop {status="Build intent was not reserved by admitted input"})
  where
    defer request=withMVar (requestClaim request) $ \()->do
      fresh<-readIORef state
      when (ownsBuildRequest request fresh) $ do
        writeIORef (active request) False
        writeIORef state BuildReserved

ownsBuildRequest :: Waiting -> BuildAdmissionState -> Bool
ownsBuildRequest request current=case current of
  BuildChecking owned->ticket owned==ticket request
  BuildAllowed owned _->ticket owned==ticket request
  _->False

-- | Retire pending adoption without launching or withdrawing another dialog.
cancelAdmittedBuild :: AdmittedBuild -> IO ()
cancelAdmittedBuild admission@(AdmittedBuild runtime _ _ _ _ state)=do
  current<-readIORef state
  case current of
    BuildChecking request->cancelRequest request
    BuildAllowed request _->cancelRequest request
    _->do
      again<-atomicModifyIORef' state (\fresh->case fresh of
        BuildChecking _->(fresh,True)
        BuildAllowed _ _->(fresh,True)
        _->(BuildConsumed,False))
      when again (cancelAdmittedBuild admission)
  where
    cancelRequest request=do
      again<-withMVar (requestClaim request) $ \()->do
        fresh<-readIORef state
        if ownsBuildRequest request fresh then do
          writeIORef state BuildConsumed
          finishOwned runtime request (Left "Build preparation cancelled")
          pure False
        else pure True
      when again (cancelAdmittedBuild admission)

-- | Use the ordinary policy/approval owner for one read_buffer capture. The host
-- supplies its attributed callback; anonymous inspection remains guest input.
-- No receipt exists while a request waits for approval. Work returned by the
-- callback runs later and can consume its snapshot, but cannot capture again.
permissionReadCall :: Permissions -> (ReadAdmission -> Tool) -> Tool
permissionReadCall runtime@(Permissions _ _ _ namespace _ _ _ _) callback desktop name args
  | name/="read_buffer" = pure (desktop,pure (Left "Read admission requires read_buffer"))
  | otherwise = permissionCall runtime admitted desktop name args
  where admitted d tool parameters=withReadAdmission namespace (sessionClosed runtime)
          (\receipt->callback receipt d tool parameters)

sessionClosed :: Permissions -> IO Bool
sessionClosed (Permissions _ _ _ _ _ (RequestIngress _ closed) _ _)=STM.readTVarIO closed

-- | Host-bound session reader. It requests fresh policy for every capture and
-- grants no Human provenance or reusable approval. Linked handlers only enqueue
-- and await; the owning tick admits the fixed operation under serialization.
bufferReader :: Permissions -> IO (Either Text ()) -> BufferReader
bufferReader runtime@(Permissions _ _ _ namespace _ ingress owner binding) caller=newBufferReader namespace
  (\reference->queueCapture runtime binding ingress owner (CaptureSubmission reference caller))
  (queueCapture runtime binding ingress owner (ListingSubmission caller))

-- | Same fixed ingress/claim/policy owner as buffer reads. The target retains
-- exact immutable body identity, never a Desktop or a live input capability.
windowReader :: Permissions -> IO (Either Text ()) -> Reads.WindowReadTarget -> IO (Either Text Reads.CapturedWindowRead)
windowReader runtime@(Permissions _ _ _ _ _ ingress owner binding) caller target=
  queueCapture runtime binding ingress owner (WindowCaptureSubmission target caller)

-- The existing fixed capture operation owns acceptance and reply cancellation;
-- the factory only binds one host submission to its new promise and claim.
queueCapture :: Permissions -> Maybe WardenBinding -> RequestIngress -> PolicyOwner
  -> (MVar (Either Text a) -> IORef Bool -> MVar () -> CaptureSubmission)
  -> IO (Either Text a)
queueCapture runtime binding (RequestIngress inbox closed) owner submissionFor=mask $ \restore->do
  captured<-traverse captureWardenBinding binding
  promise<-newEmptyMVar
  enabled<-newIORef True
  claim<-newMVar ()
  accepted<-STM.atomically $ do
    stopped<-STM.readTVar closed
    full<-STM.isFullTBQueue inbox
    if stopped then pure (Left "Editor session closed before capture")
    else if full then pure (Left "Too many captures are awaiting admission")
    else STM.writeTBQueue inbox (ReadSubmission captured (submissionFor promise enabled claim)) >> STM.tryPutTMVar (policyWake owner) () >> pure (Right ())
  case accepted of
    Left err->pure (Left err)
    Right ()->restore (readMVar promise) `onException`
      (finishCapture enabled claim promise (Left "Capture cancelled") `finally` signalPermissionWork runtime)

finishCapture :: IORef Bool -> MVar () -> MVar (Either Text a) -> Either Text a -> IO ()
finishCapture enabled claim promise result=withMVar claim (\()->finishCaptureOwned enabled promise result)

finishCaptureOwned :: IORef Bool -> MVar (Either Text a) -> Either Text a -> IO ()
finishCaptureOwned enabled promise result=mask_ $ do
  writeIORef enabled False
  _<-tryPutMVar promise result
  pure ()

captureLifetime :: CaptureSubmission -> (IORef Bool,MVar (),IO (Either Text ()))
captureLifetime (CaptureSubmission _ caller _ enabled claim)=(enabled,claim,caller)
captureLifetime (ListingSubmission caller _ enabled claim)=(enabled,claim,caller)
captureLifetime (WindowCaptureSubmission _ caller _ enabled claim)=(enabled,claim,caller)

rejectCaptureOwned :: CaptureSubmission -> Text -> IO ()
rejectCaptureOwned (CaptureSubmission _ _ promise enabled _) err=finishCaptureOwned enabled promise (Left err)
rejectCaptureOwned (ListingSubmission _ promise enabled _) err=finishCaptureOwned enabled promise (Left err)
rejectCaptureOwned (WindowCaptureSubmission _ _ promise enabled _) err=finishCaptureOwned enabled promise (Left err)

-- | Host-only fixed actor binding. Public callers submit exact read versions;
-- this transport grants neither Human authority nor reusable approval.
bufferEditor :: Permissions -> IO (Either Text ()) -> BufferEditor
bufferEditor runtime@(Permissions _ _ _ namespace retired (RequestIngress inbox closed) owner binding) caller=newBufferEditor namespace $ \patches->mask $ \restore->do
  let bounded=take 17 patches
      targets=[reference | BufferDiff reference _ _<-bounded]
      size=sum [toInteger (T.length patch) | BufferDiff _ _ patch<-bounded]
  if null bounded || length bounded>16 then pure (Left "Diff batch requires 1..16 targets")
  else if length (nub targets)/=length targets then pure (Left "Duplicate diff targets; no buffers changed")
  else if size>1048576 then pure (Left "Diff batch exceeds 1 MiB characters") else do
    captured<-traverse captureWardenBinding binding
    promise<-newEmptyMVar
    enabled<-newIORef True
    claim<-newMVar ()
    attempt<-newIORef Nothing
    -- Build the fixed review seeds on the requesting worker. Opening the
    -- approval transfers them; the UI never parses the batch or its text trees.
    seeds<-restore $ mapM (\(BufferDiff _ _ text)->do
      let seed=newBuffer text
      _<-evaluate (prepareBuffer seed)
      pure seed) bounded
    let submission=DiffSubmission bounded seeds caller promise enabled claim attempt
    accepted<-STM.atomically $ do
      stopped<-STM.readTVar closed
      full<-STM.isFullTBQueue inbox
      if stopped then pure (Left "Editor session closed before diff admission")
      else if full then pure (Left "Too many buffer requests are awaiting admission")
      else STM.writeTBQueue inbox (EditSubmission captured submission) >> STM.tryPutTMVar (policyWake owner) () >> pure (Right ())
    case accepted of
      Left err->pure (Left err)
      Right ()->restore (readMVar promise) `onException`
        (finishDiffSubmission retired submission (Left "Buffer diff cancelled") `finally` signalPermissionWork runtime)

finishDiffSubmission :: IORef [MVar ()] -> DiffSubmission -> Either Text [DiffResult] -> IO ()
finishDiffSubmission retired submission@(DiffSubmission _ _ _ _ _ claim attempt) result=withMVar claim $ \()->do
  stopAttempt retired attempt
  finishDiffSubmissionOwned submission result

finishDiffSubmissionOwned :: DiffSubmission -> Either Text [DiffResult] -> IO ()
finishDiffSubmissionOwned (DiffSubmission _ _ _ promise enabled _ _) result=mask_ $ do
  writeIORef enabled False
  _<-tryPutMVar promise result
  pure ()

-- A batch has the same policy and correction ticket as one strict diff. Its
-- target list is host-owned: human edits can replace patches, never references.
packDiffArguments :: [Value] -> Value
packDiffArguments batch=object ["buffers" .= batch]

diffArguments :: Value -> [Value]
diffArguments args=fromMaybe [] (field "buffers" args)

diffFields :: Waiting -> [(Text,Value)]
diffFields request=case operation request of
  DiffOperation _->zip labels patches
  _->[]
  where
    patches=diffArguments (arguments request)
    labels=if length patches==1 then ["diff"] else ["diff "<>T.pack (show n) | n<-[1::Int ..length patches]]

diffTargetsCurrent :: BufferNamespace -> [BufferDiff] -> Desktop -> IO Bool
diffTargetsCurrent namespace targets desktop=and <$> mapM current targets
  where current (BufferDiff reference expected _)=maybe (pure False) (versionCurrent expected . documentBuffer)
          (referenceId namespace reference >>= (`M.lookup` buffers desktop))

finishRequestSubmission :: Permissions -> RequestSubmission -> Text -> IO ()
finishRequestSubmission _ (ReadSubmission _ submission) err=let (_,claim,_)=captureLifetime submission
  in withMVar claim (\()->rejectCaptureOwned submission err)
finishRequestSubmission (Permissions _ _ _ _ retired _ _ _) (EditSubmission _ submission) err=finishDiffSubmission retired submission (Left err)
finishRequestSubmission (Permissions _ _ _ _ retired _ _ _) (SubmitTerminal _ submission) err=
  let (_,claim,_)=terminalLifetime submission
  in withMVar claim (\()->finishTerminalSubmissionOwned retired submission (Left err))
finishRequestSubmission _ (WireSubmission _ _ _ _ _ promise enabled claim) err=
  withMVar claim (\()->finishWireSubmissionOwned promise enabled err)

-- Fixed bounded transport only; PermissionState still has one serialized owner.
-- An interrupted extracted batch resolves every accepted reply before unwinding.
drainRequests :: Permissions -> Desktop -> IO Desktop
drainRequests runtime@(Permissions _ _ state namespace retired (RequestIngress inbox _) _ _) desktop=mask_ $ do
  incoming<-STM.atomically (STM.flushTBQueue inbox)
  foldM admitSafely desktop incoming `onException`
    mapM_ (\submission->finishRequestSubmission runtime submission "Request owner interrupted") incoming
  where
    admitSafely current submission=admit current submission `catch` \(err::SomeException)->
      case fromException err :: Maybe SomeAsyncException of
        Just _->throwIO err
        Nothing->finishRequestSubmission runtime submission "Request admission failed" >> pure current
    admit current submission=do
      let binding=case submission of
            ReadSubmission captured _->captured
            EditSubmission captured _->captured
            SubmitTerminal captured _->captured
            WireSubmission captured _ _ _ _ _ _ _->captured
          (enabled,claim,caller)=case submission of
            ReadSubmission _ capture->captureLifetime capture
            EditSubmission _ (DiffSubmission _ _ c _ e k _)->(e,k,c)
            SubmitTerminal _ terminal->terminalLifetime terminal
            WireSubmission _ _ _ _ c _ e k->(e,k,c)
          target=case submission of
            WireSubmission _ name args callback _ promise _ _->Right (name,args,WireOperation callback promise)
            SubmitTerminal _ terminal@(TerminalSubmission _ _ request _ _ _ _ _)->
              let (name,args)=terminalArguments request in Right (name,args,TerminalOperation terminal)
            ReadSubmission _ capture@ListingSubmission{}->Right ("list_buffers",object [],CaptureOperation capture)
            ReadSubmission _ capture@(CaptureSubmission reference _ _ _ _)->bufferTarget reference $ \ident->
              ("read_buffer",object ["bufferId" .= ident],CaptureOperation capture)
            ReadSubmission _ capture@(WindowCaptureSubmission reference _ _ _ _)->Right
              ("read_window",object ["windowId" .= Reads.windowReadIdentifier reference],CaptureOperation capture)
            EditSubmission _ diff@(DiffSubmission targets _ _ _ _ _ _)->do
              patches<-mapM (\(BufferDiff reference _ patch)->bufferTarget reference $ \ident->
                let version=maybe 0 (revision.documentBuffer) (M.lookup ident (buffers current))
                in version `seq` object ["bufferId" .= ident,"revision" .= version,"diff" .= patch]) targets
              pure ("buffer_apply_diff",packDiffArguments patches,DiffOperation diff)
          bufferTarget reference build=maybe (Left "Buffer reference belongs to another editor session") (Right . build) (referenceId namespace reference)
      withMVar claim $ \()->do
        live<-readIORef enabled
        when live $ case target of
          Left err->finishRequestSubmissionOwned submission err
          Right (name,args,op)->do
            original<-readIORef state
            liveRequests<-filterMActive runtime (waiting original)
            if length liveRequests>=32 then finishRequestSubmissionOwned submission "Too many MCP requests are awaiting permission" else do
              attempt<-case submission of EditSubmission _ (DiffSubmission _ _ _ _ _ _ a)->pure a; _->newIORef Nothing
              stage<-newIORef (PolicyPending AdmitPolicy Nothing)
              captured<-case submission of
                ReadSubmission _ _->pure (Right Nothing)
                WireSubmission{}->pure (Right Nothing)
                SubmitTerminal{}->pure (Right Nothing)
                EditSubmission _ (DiffSubmission targets _ _ _ _ _ _)->do
                  matches<-diffTargetsCurrent namespace targets current
                  if not matches then pure (Left "Buffer identity or revision changed; read the buffer again") else
                    case mapM (capturePatchSource current) (diffArguments args) of
                      Left err->pure (Left err)
                      Right sources->Right . Just <$> mapM evaluate sources
              case captured of
                Left err->finishRequestSubmissionOwned submission err
                Right source->do
                  judgment<-newIORef Nothing
                  let request=Waiting (nextTicket original) name args op enabled False caller attempt source claim stage binding judgment
                  writeIORef state original {waiting=liveRequests++[request],nextTicket=nextTicket original+1}
        pure current
    finishRequestSubmissionOwned (ReadSubmission _ submission) err=rejectCaptureOwned submission err
    finishRequestSubmissionOwned (EditSubmission _ submission) err=finishDiffSubmissionOwned submission (Left err)
    finishRequestSubmissionOwned (SubmitTerminal _ submission) err=finishTerminalSubmissionOwned retired submission (Left err)
    finishRequestSubmissionOwned (WireSubmission _ _ _ _ _ promise enabled _) err=finishWireSubmissionOwned promise enabled err

-- Caller holds the same short request claim used by cancellation. Only known
-- host capture/actor operations run here; no extension handler or reply wait.
captureSubmissionOwned :: Permissions -> CaptureSubmission -> Desktop -> IO ()
captureSubmissionOwned runtime@(Permissions _ _ _ namespace _ _ _ _) submission desktop=do
  let (_,_,caller)=captureLifetime submission
  actor<-caller
  case actor of
    Left err->rejectCaptureOwned submission err
    Right ()->case submission of
      ListingSubmission _ promise enabled _->do
        outcome<-withReadAdmission namespace (sessionClosed runtime) (`Reads.listBuffers` desktop)
        finishCaptureOwned enabled promise outcome
      CaptureSubmission reference _ promise enabled _->do
        outcome<-withReadAdmission namespace (sessionClosed runtime) (\receipt->Reads.captureBuffer receipt desktop reference)
        finishCaptureOwned enabled promise outcome
      WindowCaptureSubmission target _ promise enabled _->do
        closed<-sessionClosed runtime
        outcome<-if closed then pure (Left "Editor session closed before capture") else Reads.captureWindow desktop target
        finishCaptureOwned enabled promise outcome

filterMActive :: Permissions -> [Waiting] -> IO [Waiting]
filterMActive runtime requests=fmap concat $ mapM (\request->withMVar (requestClaim request) $ \()->do
  live<-readIORef (active request)
  if live then pure [request] else stopWardenAttempt runtime request >> pure []) requests

finish :: Permissions -> Waiting -> Either Text Value -> IO ()
finish runtime request result=withMVar (requestClaim request) (\()->finishOwned runtime request result)

-- A typed wait retires active through its original shared claim. Filtering
-- claims that same cell and retires its worker before removing the ticket.
-- Cancellation joins remain outside the desktop/request claim.
stopWardenAttempt :: Permissions -> Waiting -> IO ()
stopWardenAttempt (Permissions _ _ _ _ retired _ _ _) request=mask_ $ do
  stage<-readIORef (policyStage request)
  case stage of
    WardenPreparing _ worker _->do
      writeIORef (policyStage request) PolicyReady
      done<-newEmptyMVar
      _<-forkIOWithUnmask (\unmask->unmask (cancel worker >> waitCatch worker >> pure ())
        `finally` (tryPutMVar done () >> pure ()))
      atomicModifyIORef' retired (\current->(done:current,()))
    _->pure ()

-- Caller holds requestClaim. Result publication and cancellation are linearized.
finishOwned :: Permissions -> Waiting -> Either Text Value -> IO ()
finishOwned runtime@(Permissions _ _ _ _ retired _ _ _) request result=mask_ $ do
  atomicModifyIORef' (active request) (const (False,()))
  stopWardenAttempt runtime request
  stopDiffAttempt runtime request
  case operation request of
    WireOperation _ promise->tryPutMVar promise (pure result) >> pure ()
    BuildInputOperation _ promise->tryPutMVar promise (pure result) >> pure ()
    BuildAdoptionOperation (AdmittedBuild _ _ _ _ _ state)->atomicModifyIORef' state $ \current->
      (if ownsBuildRequest request current then BuildRejected (either id (const "Invalid build admission result") result) else current,())
    CaptureOperation submission->rejectCaptureOwned submission (either id (const "Invalid capture reply") result)
    DiffOperation submission->finishDiffSubmissionOwned submission (case result of Left err->Left err; Right _->Left "Invalid diff reply")
    TerminalOperation submission->finishTerminalSubmissionOwned retired submission (Left (either id (const "Invalid terminal reply") result))

-- An attempt belongs to a ticket, but failure does not end that ticket. Retire
-- joins run on their own thread; neither tick nor dialog submission waits for a
-- worker's cancellation/finalizer while holding the desktop lock.
stopDiffAttempt :: Permissions -> Waiting -> IO ()
stopDiffAttempt (Permissions _ _ _ _ retired _ _ _) request=stopAttempt retired (patchAttempt request)

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

-- An approval attempt belongs to every editable field in the same fixed order.
-- Comparing these small receipts never touches the review text or Undo trees.
reviewVersion :: Waiting -> Desktop -> IO DiffReview
reviewVersion request desktop=case dialog desktop of
  Just dg | purpose dg==PermissionDialog (approvalAction request)->do
    let drafts=[(label,b) | TextArea label True b _ _ _<-fields dg]
    if map fst drafts/=map fst (diffFields request) || null drafts then pure Nothing
    else Just <$> mapM (\(label,b)->(label,) <$> captureVersion b) drafts
  _->pure Nothing

startDiffAttempt :: Permissions -> Waiting -> Value -> Desktop -> IO Desktop
startDiffAttempt runtime request edited desktop=withMVar (requestClaim request) (\()->startDiffAttemptOwned runtime request edited desktop)

startDiffAttemptOwned :: Permissions -> Waiting -> Value -> Desktop -> IO Desktop
startDiffAttemptOwned runtime request edited desktop=case patchSources request of
  Nothing->diffFailureOwned runtime request "Missing original diff sources" desktop
  Just sources->mask_ $ do
    live<-readIORef (active request)
    if not live then pure desktop else do
      stopDiffAttempt runtime request
      let proposed=diffArguments edited
          original=diffArguments (arguments request)
          prepare
            | length sources/=length proposed || length sources/=length original=pure (Left "Diff attempt changed its target list")
            | sum [toInteger (T.length text) | patch<-proposed,Just text<-[field "diff" patch]]>1048576=pure (Left "Diff batch exceeds 1 MiB characters")
            | otherwise=sequence <$> sequence
                [preparePatch source (if approvalRequired request then field "diff" old else Nothing) patch
                | (source,old,patch)<-zip3 sources original proposed]
      review<-reviewVersion request desktop
      worker<-asyncWithUnmask (\unmask->unmask prepare `finally` signalPermissionWork runtime)
      writeIORef (patchAttempt request) (Just (DiffAttempt review worker))
      pure desktop {status="Preparing exact buffer diff...",dialog=fmap (\dg->if purpose dg==PermissionDialog (approvalAction request) then dg {body=[]} else dg) (dialog desktop)}

-- Preserve current fields/selection/review Undo and the live correction ticket.
diffFailureOwned :: Permissions -> Waiting -> Text -> Desktop -> IO Desktop
diffFailureOwned runtime request err desktop
  | approvalRequired request = pure desktop {status="Diff not applied: "<>err,
      dialog=fmap (\dg->if purpose dg==PermissionDialog (approvalAction request)
        then dg {body=T.chunksOf (max 1 (width (dialogRect desktop dg)-6)) ("Diff not applied: "<>err)} else dg) (dialog desktop)}
  | otherwise = finishOwned runtime request (Left err) >> pure desktop {status="Diff not applied: "<>err}

drainDiffAttempts :: Permissions -> Desktop -> IO Desktop
drainDiffAttempts runtime@(Permissions _ _ ref _ retired _ _ _) initial=do
  s<-readIORef ref
  live<-filterMActive runtime (waiting s)
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
          stage<-readIORef (policyStage request)
          if enabled && isPolicyReady stage && not (approvalRequired request) && not (null (diffFields request))
            then startDiffAttempt runtime request (arguments request) desktop else pure desktop
        Just (DiffAttempt review worker)->do
          completed<-poll worker
          case completed of
            Nothing->pure desktop
            Just outcome->withMVar (requestClaim request) $ \()->mask_ $ do
              writeIORef (patchAttempt request) Nothing
              live<-readIORef (active request)
              if not live then pure desktop else do
                let prepared=case outcome of
                      Left _->Left "Diff preparation failed"
                      Right value->value
                writeIORef (policyStage request) (PolicyPending (AdoptPolicy review prepared) Nothing)
                pure desktop

startTerminalAttemptOwned :: Permissions -> Waiting -> TerminalSubmission -> Desktop -> IO Desktop
startTerminalAttemptOwned runtime request (TerminalSubmission consoles directory terminal _ _ _ _ attempt) desktop=mask_ $ do
  receipt<-readIORef (wardenReceipt request)
  let admission=do
        live<-readIORef (active request)
        stopped<-sessionClosed runtime
        caller<-patchCaller request
        case caller of
          Left err->pure (Left err)
          Right () | not live || stopped->pure (Left "Terminal request expired before execution")
                   | otherwise->maybe (pure (Right ())) checkWarden receipt
  worker<-asyncWithUnmask (\_->mask_ (prepareTerminal admission consoles directory terminal `finally` signalPermissionWork runtime))
  writeIORef attempt (Just worker)
  writeIORef (policyStage request) TerminalPreparing
  pure (closeReview request desktop)

-- Preparation owns a launch until the masked Async publication hands it to the
-- ticket's attempt cell. All process, directory and exact-ID IO stays here.
prepareTerminal :: IO (Either Text ()) -> Consoles.Consoles -> FilePath -> TerminalRequest -> IO (Either Text PreparedTerminal)
prepareTerminal admission consoles directory request=case request of
  StartTerminal launch->do
    root<-maybe (Build.resolveBuildRootFrom directory) canonicalizePath (Terminal.terminalDirectory launch)
    let config=NativeTerminal.TerminalConfig (T.unpack (Terminal.terminalCommand launch))
          (map T.unpack (Terminal.terminalArguments launch)) [] root 80 24
    admitted (fmap PreparedTerminalConsole <$> Consoles.prepareConsole [] config (Terminal.terminalOutputByteLimit launch))
  ListTerminals->admitted $ do
    entries<-Consoles.listConsoles consoles
    summaries<-mapM (\(ident,bid,code)->do
      _<-evaluate (T.length ident)
      _<-traverse evaluate code
      evaluate (Terminal.TerminalSummary (Terminal.TerminalId ident) bid code)) entries
    reply<-evaluate (PreparedTerminalReply (ListedTerminals (Terminal.TerminalListing NativeTerminal.terminalAvailable summaries)))
    pure (Right reply)
  OutputTerminal ident offset limit->admitted $ do
    result<-Consoles.consoleOutput consoles (Terminal.terminalIdText ident)
    case result of
      Left err->pure (Left err)
      Right (bytes,truncated,code)->do
        _<-traverse evaluate code
        reply<-evaluate (PreparedTerminalReply (OutputTerminalPage (Terminal.TerminalPage ident offset
          (BS.length bytes) truncated code (BS.copy (BS.take limit (BS.drop offset bytes))))))
        pure (Right reply)
  InputTerminal ident text->admitted $ fmap (const (PreparedTerminalReply AcceptedTerminal)) <$>
    Consoles.inputConsole consoles (Terminal.terminalIdText ident) (TE.encodeUtf8 text)
  StopTerminalRequest ident->admitted $ fmap (const (PreparedTerminalReply AcceptedTerminal)) <$>
    Consoles.killConsole consoles (Terminal.terminalIdText ident)
  where
    admitted action=admission >>= either (pure . Left) (const action)

-- A modal hold ends a fresh policy lifetime. Prepared resources remain with the
-- same request, and only queue a new final check when that hold has ended.
drainTerminalAttempts :: Permissions -> Desktop -> IO Desktop
drainTerminalAttempts runtime@(Permissions _ _ ref _ _ _ _ _) desktop=do
  requests<-readIORef ref >>= filterMActive runtime . waiting
  foldM step desktop requests
  where
    step current request=case operation request of
      TerminalOperation (TerminalSubmission _ _ _ _ _ _ _ attempt)->do
        stage<-readIORef (policyStage request)
        case stage of
          TerminalPreparing->do
            worker<-readIORef attempt
            result<-maybe (pure Nothing) poll worker
            case result of
              Nothing->pure current
              Just (Left _)->finish runtime request (Left "Terminal preparation failed") >> pure current
              Just (Right (Left err))->finish runtime request (Left err) >> pure current
              Just (Right (Right _))->advance current request
          TerminalPrepared->advance current request
          _->pure current
      _->pure current
    advance current request=withMVar (requestClaim request) $ \()->do
      live<-readIORef (active request)
      when live (writeIORef (policyStage request) (if terminalWindowHeld request current
        then TerminalPrepared else PolicyPending TerminalAdoptPolicy Nothing))
      pure current

-- Only launch adoption changes the visible desktop. Other terminal replies
-- can finish beneath a modal while retaining the same final admission checks.
terminalWindowHeld :: Waiting -> Desktop -> Bool
terminalWindowHeld request desktop=case operation request of
  TerminalOperation (TerminalSubmission _ _ StartTerminal{} _ _ _ _ _)->dialog desktop/=Nothing || questionActive desktop
  _->False

-- The owner holds the same claim as cancellation throughout transfer/reply.
-- Removing the attempt after successful adoption prevents the retire reaper
-- from closing the transferred process. Exception paths still retain ownership.
adoptTerminalOwned :: Permissions -> Waiting -> Desktop -> IO Desktop
adoptTerminalOwned runtime@(Permissions _ _ _ _ retired _ _ _) request desktop=case operation request of
  TerminalOperation submission@(TerminalSubmission consoles _ _ _ _ _ _ attempt)->mask_ $ do
    worker<-readIORef attempt
    outcome<-maybe (pure Nothing) poll worker
    case outcome of
      Just (Right (Right prepared))->do
        (updated,response)<-case prepared of
          PreparedTerminalReply result->pure (desktop,result)
          PreparedTerminalConsole console->do
            let bid=nextId desktop
            (ident,opened)<-Consoles.adoptConsole consoles console desktop
            pure (opened,OpenedTerminal (Terminal.TerminalOpened (Terminal.TerminalId ident) bid))
        writeIORef attempt Nothing
        finishTerminalSubmissionOwned retired submission (Right response)
        pure updated
      _->finishOwned runtime request (Left "Terminal preparation is no longer available") >> pure desktop
  _->finishOwned runtime request (Left "Invalid terminal admission operation") >> pure desktop

-- | Display the oldest live approval and withdraw stale or cancelled prompts.
tickPermissions :: Permissions -> Desktop -> IO Desktop
tickPermissions runtime@(Permissions _ _ ref _ _ _ _ _) original=do
  desktop<-drainSettings runtime original >>= drainRequests runtime >>= drainDiffAttempts runtime >>= drainTerminalAttempts runtime >>= drainPolicies runtime
  s<-readIORef ref
  live<-filterMActive runtime (waiting s)
  let staleApproval=case dialog desktop of
        Just dg | PermissionDialog action<-purpose dg,"approve:" `T.isPrefixOf` action -> all ((/=action).approvalAction) live
        _ -> False
      cleared=if staleApproval then desktop {dialog=Nothing} else desktop
  writeIORef ref s {waiting=live,displayed=if staleApproval then Nothing else displayed s}
  ready<-filterMReady live
  case (dialog cleared,filter approvalRequired ready) of
    (Nothing,request:_) -> do
      modifyIORef' ref (\state->state {displayed=Just (approvalAction request)})
      pure cleared {dialog=Just (approvalReview cleared request)}
    _ -> pure cleared

-- doc-artifact: tools/docs-screenshots.hs permission-diff -> docs/site/screenshots/permission-diff.png
approvalReview :: Desktop -> Waiting -> Dialog
approvalReview desktop request=Dialog "Agent permission" (PermissionDialog (approvalAction request)) reviewFields selected ["Allow once","Deny"] []
  where
    args=arguments request
    patches=diffFields request
    seeds=case operation request of DiffOperation (DiffSubmission _ initial _ _ _ _ _)->initial; _->[]
    metadata=[ReadOnly "Tool" (toolName request)]++
      [ReadOnly "Changes" (T.pack (show (length patches))<>" buffers; apply together, without saving") | length patches>1]
    reviewFields=metadata++if null patches then [view name value | (name,value)<-members] else concatMap patchFields (zip patches seeds)
    selected=fromMaybe 0 (findIndex editable reviewFields)
    editable (TextArea _ True _ _ _ _)=True
    editable _=False
    members=case args of Object entries -> sortOn fst [(K.toText key,value) | (key,value)<-KM.toList entries]; _ -> [("Arguments",args)]
    patchFields ((label,patch),seed)=
      [ReadOnly "File" (fromMaybe "Unknown buffer" $ do
        bid<-field "bufferId" patch
        doc<-M.lookup bid (buffers desktop)
        pure (maybe ("Untitled #"<>T.pack (show bid)) (T.pack.filePath) (documentFile doc)))]++
      [view name value | Object entries<-[patch],(name,value)<-sortOn fst [(K.toText key,value) | (key,value)<-KM.toList entries],name/="diff"]++
      [TextArea label True seed (Selection 0 0) 0 0]
    view name value=let text=case value of String t -> t; _ -> TE.decodeUtf8 (BL.toStrict (encode value))
                    in if T.any (=='\n') text || T.length text>48 then TextArea name False (newBuffer text) (Selection 0 0) 0 0 else ReadOnly name text

-- Only patch text is editable; reconstruct the immutable host target order.
reviewArguments :: Waiting -> [Text] -> Maybe Value
reviewArguments request values
  | null patches=Just (arguments request)
  | length patches/=length values=Nothing
  | otherwise=packDiffArguments <$> sequence
      [case args of Object fields->Just (Object (KM.insert "diff" (String text) fields)); _->Nothing
      | ((_,args),text)<-zip patches values]
  where patches=diffFields request

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
permissionAction runtime@(Permissions _ registry ref _ _ _ owner _) action values desktop=do
  s<-readIORef ref
  generation<-readIORef (settingsGeneration owner)
  let settingsAction="settings:"<>T.pack (show generation)
      settingName=T.stripPrefix ("set:"<>T.pack (show generation)<>":") action
  if action=="show" then queueSettings ListPolicies desktop else
    if displayed s/=Just action then pure desktop {status="Permission dialog expired."} else
      if not ("approve:" `T.isPrefixOf` action) && values==["1"] && modalAbsent then close else
      case action of
        _ | action==settingsAction,not ownsModal->pure desktop {status="Permission dialog expired."}
          | action==settingsAction->case values of
          "0":index:_ | Just n<-readMaybe (T.unpack index),Just (name,readonly)<-at (M.toList registry) n ->queueSettings (EditPolicy name readonly) desktop
          _->close
        _ | Just name<-settingName,Just _<-M.lookup name registry,not ownsModal->pure desktop {status="Permission dialog expired."}
          | Just name<-settingName,Just _<-M.lookup name registry ->case values of
          "0":selected:_ | Just mode<-readMaybe (T.unpack selected) >>= at [Enable,Prompt,Disable] ->do
            modifyIORef' (policyEpoch owner) (+1)
            queueSettings (SaveSetting name mode) desktop
          _->queueSettings ListPolicies desktop
        _ | "approve:" `T.isPrefixOf` action ->case find ((==action).approvalAction) (waiting s) of
          Nothing->close
          Just request | take 1 values/=["0"]->finish runtime request (Left "MCP request denied") >> tickPermissions runtime (closeReview request desktop)
          Just request->case reviewArguments request (drop 1 values) of
            Nothing->pure desktop {status="Diff review changed; approve the current target list."}
            Just edited->do
              withMVar (requestClaim request) $ \()->do
                live<-readIORef (active request)
                when live $ do
                  -- A newly accepted review supersedes the old worker attempt.
                  -- Its completion must not overwrite this fresh Allow phase.
                  stopWardenAttempt runtime request
                  writeIORef (wardenReceipt request) Nothing
                  stopDiffAttempt runtime request
                  review<-reviewVersion request desktop
                  writeIORef (policyStage request) (PolicyPending (AllowPolicy edited review) Nothing)
              tickPermissions runtime desktop {status="Checking current permission policy...",dialog=fmap
                (\dg->if purpose dg==PermissionDialog action then dg {body=[]} else dg) (dialog desktop)}

        _->close
  where
    -- Escape already removed this modal in the pure input transition. It can
    -- retire its exact displayed settings receipt, but never starts a new job.
    modalAbsent=case dialog desktop of Nothing->True; _->False
    ownsModal=case dialog desktop of Just dg->purpose dg==PermissionDialog action; _->False
    queueSettings use d=do
      busy<-policyWriting owner
      if busy then pure d {status="Agent permission save is still pending."} else do
       generation<-atomicModifyIORef' (settingsGeneration owner) (\n->(n+1,n+1))
       writeIORef (settingsJob owner) (Just (SettingsJob generation use Nothing))
       let token="loading:"<>T.pack (show generation)
       modifyIORef' ref (\state->state {displayed=Just token})
       drainSettings runtime d {status="Loading agent permissions...",dialog=Just
         (Dialog "Agent Permissions" (PermissionDialog token) [ReadOnly "Status" "Loading current policy..."] 0 ["Close"] [])}
    close=do
      modifyIORef' (settingsGeneration owner) (+1)
      -- An accepted Save still completes on the worker; only its UI receipt retires.
      modifyIORef' ref (\state->state {displayed=Nothing})
      tickPermissions runtime (case dialog desktop of
        Just dg | purpose dg==PermissionDialog action->desktop {dialog=Nothing}
        _->desktop)

isPolicyReady :: PolicyStage -> Bool
isPolicyReady PolicyReady=True
isPolicyReady _=False
filterMReady :: [Waiting] -> IO [Waiting]
filterMReady requests=fmap (map fst . filter snd) (mapM (\request->(request,) . isPolicyReady <$> readIORef (policyStage request)) requests)

-- Save is a component-owned mutation barrier: epoch advances before submission,
-- and no policy decision adopts while the write is pending. Queue submission is
-- nonblocking; a full worker mailbox leaves this fixed phase for a later tick.
drainSettings :: Permissions -> Desktop -> IO Desktop
drainSettings runtime@(Permissions _ registry ref _ _ _ owner _) desktop=do
  job<-readIORef (settingsJob owner)
  case job of
    Nothing->pure desktop
    Just (SettingsJob generation use Nothing)->do
      pending<-submitPolicy owner (case use of SaveSetting name mode->SavePolicy name mode; _->LoadPolicy)
      writeIORef (settingsJob owner) (Just (SettingsJob generation use pending))
      pure desktop
    Just (SettingsJob generation use (Just promise))->do
      completed<-STM.atomically (STM.tryReadTMVar promise)
      case completed of
        Nothing->pure desktop
        Just policies->do
          writeIORef (settingsJob owner) Nothing
          case use of
            SaveSetting name Disable->when (either (const False) (const True) policies) $
              readIORef ref >>= mapM_ (\request->when (toolName request==name) (finish runtime request (Left "This MCP tool was disabled before approval"))) . waiting
            _->pure ()
          current<-readIORef (settingsGeneration owner)
          let ownsModal=case dialog desktop of
                Nothing->True
                Just dg->purpose dg==PermissionDialog ("loading:"<>T.pack (show generation))
          if current/=generation || not ownsModal then pure desktop else do
            let token=case use of EditPolicy name _->"set:"<>T.pack (show generation)<>":"<>name; _->"settings:"<>T.pack (show generation)
                mode name readonly=either (const (if readonly then Enable else Prompt)) (M.findWithDefault (if readonly then Enable else Prompt) name) policies
                notes=either (:[]) (const ["Select a tool to set Enable, Prompt or Disable.","Policies apply to every request, including cached tool schemas."]) policies
                dg=case use of
                  EditPolicy name readonly->Dialog "Agent permission setting" (PermissionDialog token)
                    [Radio name ["Enable","Prompt","Disable"] (modeIndex (mode name readonly))] 0 ["Save","Back"]
                    ["Enable runs immediately; Prompt asks once per request; Disable rejects.","Saved globally in thc/config.toml."]
                  _->Dialog "Agent Permissions" (PermissionDialog token)
                    [ListBox "Tool" [name<>"  ["<>modeText (mode name readonly)<>"]" | (name,readonly)<-M.toList registry] 0] 0 ["Edit","Close"] notes
            modifyIORef' ref (\state->state {displayed=Just token})
            pure desktop {dialog=Just dg,status=case use of SaveSetting _ _->either id (const "Agent permission saved.") policies; _->status desktop}

policyWriting :: PolicyOwner -> IO Bool
policyWriting owner=do
  job<-readIORef (settingsJob owner)
  pure (case job of Just (SettingsJob _ (SaveSetting _ _) _)->True; _->False)

drainPolicies :: Permissions -> Desktop -> IO Desktop
drainPolicies runtime@(Permissions _ registry ref namespace _ _ owner _) original=do
  requests<-readIORef ref >>= filterMActive runtime . waiting
  foldM stepSafely original requests `onException` mapM_ (\request->finish runtime request (Left "Permission request owner interrupted")) requests
  where
    stepSafely desktop request=step desktop request `catch` \(err::SomeException)->
      case fromException err :: Maybe SomeAsyncException of
        Just _->throwIO err
        Nothing->finish runtime request (Left "Permission request admission failed") >> pure (closeReview request desktop)
    step desktop request=do
      stage<-readIORef (policyStage request)
      epoch<-readIORef (policyEpoch owner)
      writing<-policyWriting owner
      case stage of
        PolicyReady->pure desktop
        TerminalPreparing->pure desktop
        TerminalPrepared->pure desktop
        WardenPreparing use _ result->STM.atomically (STM.tryReadTMVar result) >>= \ready->case ready of
          Nothing->pure desktop
          Just outcome->withMVar (requestClaim request) $ \()->do
            live<-readIORef (active request)
            if not live then stopWardenAttempt runtime request >> pure desktop else case outcome of
              Left _->finishOwned runtime request (Left "Warden judgment interrupted; action not admitted.") >> pure (closeReview request desktop)
              Right receipt->do
                writeIORef (wardenReceipt request) (Just receipt)
                writeIORef (policyStage request) (PolicyPending use Nothing)
                -- The judgment wake has been consumed. Queue its fresh policy
                -- read now so this phase owns the next completion wake.
                step desktop request
        PolicyPending use pending | writing->pure desktop
                                  | otherwise->case pending of
          Nothing->do
            queued<-submitPolicy owner LoadPolicy
            writeIORef (policyStage request) (PolicyPending use ((epoch,) <$> queued))
            pure desktop
          Just (issued,promise) | issued/=epoch->writeIORef (policyStage request) (PolicyPending use Nothing) >> step desktop request
                               | otherwise->do
            completed<-STM.atomically (STM.tryReadTMVar promise)
            case completed of
              Nothing->pure desktop
              Just policies->withMVar (requestClaim request) $ \()->do
                live<-readIORef (active request)
                if not live then pure desktop else do
                  writeIORef (policyStage request) PolicyReady
                  caller<-patchCaller request
                  stopped<-sessionClosed runtime
                  let selected=do
                        when stopped (Left "Editor session closed before policy admission")
                        readonly<-maybe (Left "Unknown MCP tool") Right (M.lookup (toolName request) registry)
                        modes<-policies
                        caller
                        let mode=M.findWithDefault (if readonly then Enable else Prompt) (toolName request) modes
                        when (mode==Disable) (Left "This MCP tool is disabled in Options > Agent Permissions")
                        pure mode
                  case selected of
                    Left err->finishOwned runtime request (Left err) >> pure (closeReview request desktop) {status=err}
                    Right mode->applyPolicy desktop request use mode
    applyPolicy desktop request AdmitPolicy mode=do
      prepared<-case operation request of
        DiffOperation (DiffSubmission targets _ _ _ _ _ _)->do
          current<-diffTargetsCurrent namespace targets desktop
          pure (if not current then Left "Buffer identity or revision changed; read the buffer again" else Right (patchSources request))
        _->pure (Right (patchSources request))
      case prepared of
        Left err->finishOwned runtime request (Left err) >> pure desktop {status=err}
        Right source->do
          let polling=toolName request=="ask_user" && case arguments request of Object fields->KM.keys fields==["questionId"]; _->False
              approved=request {approvalRequired=mode==Prompt && not polling,patchSources=source}
          replaceRequest approved
          if approvalRequired approved then pure desktop else execute desktop approved AdmitPolicy (arguments request)
    applyPolicy desktop request BuildAdoptPolicy mode=case operation request of
      BuildAdoptionOperation (AdmittedBuild _ _ _ approved _ state)
        | mode==Prompt && not approved->finishOwned runtime request (Left "This MCP tool now requires approval; submit again") >> pure desktop
        | otherwise->do
            epoch<-readIORef (policyEpoch owner)
            writeIORef state (BuildAllowed request epoch)
            pure desktop
      _->finishOwned runtime request (Left "Invalid build admission operation") >> pure desktop
    applyPolicy desktop request TerminalAdoptPolicy mode
      | mode==Prompt && not (approvalRequired request)=finishOwned runtime request (Left "This MCP tool now requires approval; submit again") >> pure desktop
      | terminalWindowHeld request desktop=writeIORef (policyStage request) TerminalPrepared >> pure desktop
      | otherwise=guarded desktop request (adoptTerminalOwned runtime request desktop)
    applyPolicy desktop request (AllowPolicy edited review) _=do
      current<-reviewCurrent request review desktop
      let owns=case dialog desktop of Just dg->purpose dg==PermissionDialog (approvalAction request); _->False
      if not current || not owns then pure desktop {status="Permission review changed; approve the current review."}
      else execute desktop request (AllowPolicy edited review) edited
    applyPolicy desktop request (AdoptPolicy review prepared) mode=do
      current<-reviewCurrent request review desktop
      if not current then pure desktop {status="Diff review changed; approve the current review."}
      else if not (approvalRequired request) && mode/=Enable then
        finishOwned runtime request (Left "This MCP tool now requires approval; submit again") >> pure desktop {status="Diff now requires approval."}
      else case prepared of
        Left err->diffFailureOwned runtime request err desktop
        Right patch->guarded desktop request $ do
          adopted<-commitPatches patch desktop
          case adopted of
            Left err->diffFailureOwned runtime request err desktop
            Right (updated,response)->do
              stopDiffAttempt runtime request
              case operation request of
                DiffOperation submission->finishDiffSubmissionOwned submission (Right response)
                _->finishOwned runtime request (Left "Invalid diff operation")
              pure (closeReview request updated) {status="Applied exact buffer diff."}
    execute desktop request use edited=case wardenBinding request of
      Nothing->executeOwned desktop request edited
      Just binding->do
        receipt<-readIORef (wardenReceipt request)
        case receipt of
          Just _->guarded desktop request (executeOwned desktop request edited)
          Nothing->do
            enforces<-wardenEnforces binding
            if not enforces then do
              -- Off/Observe do not add a worker round trip or policy reload.
              -- Only bounded observation admission runs here, never inference.
              issued<-runWarden binding (toolName request) edited
              writeIORef (wardenReceipt request) (Just issued)
              guarded desktop request (executeOwned desktop request edited)
            else mask_ $ do
              name<-evaluate (toolName request)
              result<-STM.newEmptyTMVarIO
              worker<-asyncWithUnmask $ \unmask->do
                outcome<-try (unmask (runWarden binding name edited))
                -- Publish the owned result before waking tick; Async's own
                -- completion is later and cannot be the wake readiness receipt.
                STM.atomically $ do
                  STM.putTMVar result outcome
                  _<-STM.tryPutTMVar (policyWake owner) ()
                  pure ()
              writeIORef (policyStage request) (WardenPreparing use worker result)
              pure desktop
    guarded desktop request action=do
      admitted<-checkRequestWarden request
      case admitted of
        Left err->finishOwned runtime request (Left err) >> pure (closeReview request desktop)
        Right ()->action
    executeOwned desktop request edited=case operation request of
      CaptureOperation submission->captureSubmissionOwned runtime submission desktop >> pure (closeReview request desktop)
      DiffOperation _->startDiffAttemptOwned runtime request edited desktop
      TerminalOperation submission->startTerminalAttemptOwned runtime request submission desktop
      BuildAdoptionOperation _->finishOwned runtime request (Left "Invalid build admission phase") >> pure desktop
      BuildInputOperation callback promise->do
        state<-newIORef BuildUnused
        receipt<-readIORef (wardenReceipt request)
        let caller=patchCaller request
            checkedCaller=caller >>= \live->case live of
              Left err->pure (Left err)
              Right ()->maybe (pure (Right ())) checkWarden receipt
            admission=AdmittedBuild runtime (toolName request) edited (approvalRequired request) checkedCaller state
        result<-try (callback admission (closeReview request desktop) (toolName request) edited
          `onException` cancelAdmittedBuild admission)
          `finally` atomicModifyIORef' state (\current->((case current of BuildUnused->BuildConsumed; _->current),()))
        completeWire desktop request promise result
      WireOperation callback promise->do
        result<-try (callback (closeReview request desktop) (toolName request) edited)
        completeWire desktop request promise result
    completeWire desktop request promise result=case result of
      Left (_::IOException)->finishOwned runtime request (Left "MCP tool failed after policy admission") >> pure (closeReview request desktop)
      Right (updated,continuation)->do
        writeIORef (active request) False
        _<-tryPutMVar promise continuation
        pure updated
    replaceRequest request=modifyIORef' ref (\state->state {waiting=map (\old->if ticket old==ticket request then request else old) (waiting state)})

-- Only a retained result for this exact proposal can cross final admission.
-- Human edits explicitly reset it; no large argument comparison runs on UI.
checkRequestWarden :: Waiting -> IO (Either Text ())
checkRequestWarden request=case wardenBinding request of
  Nothing->pure (Right ())
  Just _->readIORef (wardenReceipt request) >>= maybe
    (pure (Left "Warden judgment missing; action not admitted.")) checkWarden

reviewCurrent :: Waiting -> DiffReview -> Desktop -> IO Bool
reviewCurrent request Nothing _=pure (not (approvalRequired request) || null (diffFields request))
reviewCurrent request (Just expected) desktop=case dialog desktop of
  Just dg | purpose dg==PermissionDialog (approvalAction request)->do
    let drafts=[(label,b) | TextArea label True b _ _ _<-fields dg]
    if map fst drafts/=map fst expected || map fst drafts/=map fst (diffFields request) then pure False
    else and <$> sequence [versionCurrent version b | ((_,version),(_,b))<-zip expected drafts]
  _->pure False

closeReview :: Waiting -> Desktop -> Desktop
closeReview request desktop=case dialog desktop of
  Just dg | purpose dg==PermissionDialog (approvalAction request)->desktop {dialog=Nothing}
  _->desktop

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

-- Fresh bytes are read for every decision. Only unchanged bytes reuse parsing;
-- errors never fall back to an earlier Enable. The external read-to-adoption
-- interval is finite, not an atomic filesystem revocation guarantee.
newPolicyOwner :: FilePath -> M.Map Text Bool -> IO PolicyOwner
newPolicyOwner path registry=mask_ $ do
  inbox<-STM.newTBQueueIO 32
  closed<-STM.newTVarIO False
  epoch<-newIORef 0
  generation<-newIORef 0
  job<-newIORef Nothing
  wake<-STM.newEmptyTMVarIO
  worker<-asyncWithUnmask (\unmask->unmask (loop inbox wake Nothing))
  pure (PolicyOwner inbox closed worker wake epoch generation job)
  where
    loop inbox wake cached=do
      task<-STM.atomically (STM.readTBQueue inbox)
      let run=do
            saved<-case task of
              LoadPolicy _->pure (Right ())
              SavePolicy name mode _->writeTable path ["editor","mcp","permissions"] (object [K.fromText name .= modeText mode])
            bytes<-readConfigBytes path
            let parsed=case bytes of
                  Left err->Left err
                  Right source->case cached of
                    Just (old,modes) | old==source->modes
                    _->parsePolicies source
                policies=saved >> parsed
                bounded=fmap (\modes->M.mapMaybeWithKey (\name _->M.lookup name modes) registry) policies
                retained=fmap (\modes->M.mapMaybeWithKey (\name _->M.lookup name modes) registry) parsed
            forcePolicies bounded
            forcePolicies retained
            STM.atomically (resolvePolicy bounded task >> STM.tryPutTMVar wake () >> pure ())
            pure (case bytes of Right source->Just (source,retained); Left _->Nothing)
      next<-run `catch` \(err::SomeException)->do
        STM.atomically (resolvePolicy (Left "Permission policy worker interrupted") task >> STM.tryPutTMVar wake () >> pure ())
        case fromException err :: Maybe SomeAsyncException of
          Just _->throwIO err
          Nothing->pure Nothing
      loop inbox wake next

-- | Coalesced completion wake for the owning session scheduler. Waiting consumes
-- no desktop lock; a completion published before its waiter is retained. Taking
-- the signal consumes it once, so an idle worker cannot make the owner spin.
awaitPermissionWork :: Permissions -> STM.STM ()
awaitPermissionWork (Permissions _ _ _ _ _ _ owner _)=STM.takeTMVar (policyWake owner)

signalPermissionWork :: Permissions -> IO ()
signalPermissionWork (Permissions _ _ _ _ _ _ owner _)=STM.atomically (STM.tryPutTMVar (policyWake owner) () >> pure ())

resolvePolicy :: Policies -> PolicyTask -> STM.STM ()
resolvePolicy result task=do
  let promise=case task of LoadPolicy p->p; SavePolicy _ _ p->p
  _<-STM.tryPutTMVar promise result
  pure ()

forcePolicies :: Policies -> IO ()
forcePolicies result=evaluate (case result of
  Left err->T.length err
  Right modes->M.foldlWithKey' (\n name mode->mode `seq` n+T.length name) 0 modes) >> pure ()

submitPolicy :: PolicyOwner -> (STM.TMVar Policies -> PolicyTask) -> IO (Maybe (STM.TMVar Policies))
submitPolicy owner task=STM.atomically $ do
  closed<-STM.readTVar (policyClosed owner)
  full<-STM.isFullTBQueue (policyInbox owner)
  if closed || full then pure Nothing else do
    promise<-STM.newEmptyTMVar
    STM.writeTBQueue (policyInbox owner) (task promise)
    pure (Just promise)

readConfigBytes :: FilePath -> IO (Either Text (Maybe BS.ByteString))
readConfigBytes path=do
  loaded<-try (catchIOError (Just <$> withBinaryFile path ReadMode (\h->BS.hGet h 1048577)) (\err->if isDoesNotExistError err then pure Nothing else ioError err))
  pure $ case loaded of
    Left (_::IOException)->Left "Could not read thc/config.toml"
    Right bytes | maybe False ((>1048576).BS.length) bytes->Left "thc/config.toml exceeds 1 MiB"
                | otherwise->Right bytes

parsePolicies :: Maybe BS.ByteString -> Policies
parsePolicies bytes=do
  (_,table)<-parseConfigBytes bytes
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

-- | Read a human-owned Warden table at the host-selected global path. Project
-- rule text cannot select a supplier or replace this admission configuration.
readWardenAt :: FilePath -> IO (Either Text Value)
readWardenAt path=configIO $ do
  config<-readConfig path
  pure $ do
    (_,_,table)<-config
    selected<-lookupTable ["editor","warden"] table
    values<-traverse (primitive . snd) (maybe M.empty tableMap selected)
    pure (object [K.fromText key .= value | (key,value)<-M.toList values])

-- | Update only [editor.warden], preserving unrelated TOML tokens/comments with
-- the existing checked writer. Validation of supported settings is host-owned.
writeWardenAt :: FilePath -> Value -> IO (Either Text ())
writeWardenAt path=writeTable path ["editor","warden"]

-- | Read only the global human-owned inference destination. A project override
-- must not silently move admitted context to an endpoint or attached browser.
readSystemOne :: IO (Either Text Value)
readSystemOne=configIO $ do
  path<-permissionConfigPath
  config<-readConfig path
  pure $ do
    (_,_,table)<-config
    selected<-lookupTable ["editor","systemOne"] table
    values<-traverse (primitive . snd) (maybe M.empty tableMap selected)
    pure (object [K.fromText key .= value | (key,value)<-M.toList values])

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
    allowed=["backend","scale","screenMode","columns","rows","appearance","wordStar","blinkCursor","crtFilter","pixelateUnicode","materialIcons","streamerMode","bufferView","chatSubmit","macKeySymbols","hapticFeedback"]
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
  loaded<-readConfigBytes path
  pure $ do
    bytes<-loaded
    (text,table)<-parseConfigBytes bytes
    pure (bytes,text,table)

parseConfigBytes :: Maybe BS.ByteString -> Either Text (Text,Toml.Table' Toml.Position)
parseConfigBytes bytes=do
  text<-either (const (Left "thc/config.toml is not valid UTF-8")) Right (TE.decodeUtf8' (fromMaybe BS.empty bytes))
  table<-either (const (Left "Invalid TOML in thc/config.toml; existing configuration was not changed")) Right (Toml.parse text)
  pure (text,table)

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
