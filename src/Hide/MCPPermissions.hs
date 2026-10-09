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
  ( Permissions, withPermissions, withPermissionsAt, permissionCall, permissionCallAs, AdmittedBuild, permissionBuildInputAs, reserveAdmittedBuild, stepAdmittedBuild, cancelAdmittedBuild, policyEffects, tickPermissions, awaitPermissionWork
  , ReadAdmission, permissionReadCall, bufferEditor, readReference, resolveReadReference, bufferReader, windowReader
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
import Hide.Plugin.BufferHost (BufferRef,BufferNamespace,newBufferNamespace,referenceId,BufferReader,newBufferReader,CapturedRead,ListedBuffer,BufferEditor,newBufferEditor,DiffResult)
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
  , approvalRequired :: Bool, patchCaller :: IO (Either Text ()), patchAttempt :: IORef (Maybe DiffAttempt), patchSource :: Maybe PatchSource, requestClaim :: MVar (), policyStage :: IORef PolicyStage }
data DiffAttempt = DiffAttempt (Maybe ContentVersion) (Async (Either Text PreparedPatch))
data WaitingOperation = WireOperation Tool (MVar (IO (Either Text Value)))
  | BuildInputOperation (AdmittedBuild -> Tool) (MVar (IO (Either Text Value)))
  | BuildAdoptionOperation AdmittedBuild
  | CaptureOperation CaptureSubmission | DiffOperation DiffSubmission
-- Minted only while the permission owner executes admitted editor input. The
-- original wire ticket may finish; this separate one-shot intent keeps its
-- approved policy and exact caller without retaining the original desktop.
data AdmittedBuild = AdmittedBuild Permissions Text Value Bool (IO (Either Text ())) (IORef BuildAdmissionState)
data BuildAdmissionState = BuildUnused | BuildReserved | BuildChecking Waiting
  | BuildAllowed Waiting Int | BuildRejected Text | BuildConsumed
data CaptureSubmission = CaptureSubmission BufferRef (IO (Either Text ())) (MVar (Either Text CapturedRead)) (IORef Bool) (MVar ())
  | ListingSubmission (IO (Either Text ())) (MVar (Either Text [ListedBuffer])) (IORef Bool) (MVar ())
  | WindowCaptureSubmission Reads.WindowReadTarget (IO (Either Text ())) (MVar (Either Text Reads.CapturedWindowRead)) (IORef Bool) (MVar ())
data DiffSubmission = DiffSubmission BufferRef ContentVersion Text (IO (Either Text ())) (MVar (Either Text DiffResult)) (IORef Bool) (MVar ()) (IORef (Maybe DiffAttempt))
data BufferSubmission = ReadSubmission CaptureSubmission | EditSubmission DiffSubmission
data BufferIngress = BufferIngress (STM.TBQueue BufferSubmission) (STM.TVar Bool)
-- A request retains its policy phase while worker IO is pending. The queue is
-- transport only: Waiting remains the one request/cancellation owner.
type Policies = Either Text (M.Map Text Mode)
data PolicyUse = AdmitPolicy | BuildAdoptPolicy | AllowPolicy Value (Maybe ContentVersion)
  | AdoptPolicy (Maybe ContentVersion) (Either Text PreparedPatch)
data PolicyStage = PolicyReady | PolicyPending PolicyUse (Maybe (Int,STM.TMVar Policies))
data PolicyTask = LoadPolicy (STM.TMVar Policies)
  | SavePolicy Text Mode (STM.TMVar Policies)
data SettingsUse = ListPolicies | EditPolicy Text Bool | SaveSetting Text Mode
data SettingsJob = SettingsJob Int SettingsUse (Maybe (STM.TMVar Policies))
data PolicyOwner = PolicyOwner
  { policyInbox :: STM.TBQueue PolicyTask, policyClosed :: STM.TVar Bool
  , policyWorker :: Async (), policyWake :: STM.TMVar (), policyEpoch :: IORef Int
  , settingsGeneration :: IORef Int, settingsJob :: IORef (Maybe SettingsJob) }
data PermissionState = PermissionState { waiting :: [Waiting], nextTicket :: Int, displayed :: Maybe Text }
data Permissions = Permissions FilePath (M.Map Text Bool) (IORef PermissionState) BufferNamespace (IORef [MVar ()]) BufferIngress PolicyOwner

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
      Permissions path registry <$> newIORef (PermissionState [] 1 Nothing) <*> newBufferNamespace <*> newIORef [] <*> (BufferIngress <$> STM.newTBQueueIO 32 <*> STM.newTVarIO False) <*> pure owner
    registry=M.fromList [(name,fromMaybe False (field "annotations" spec >>= field "readOnlyHint")) | spec<-specs,Just name<-[field "name" spec]]
    release runtime@(Permissions _ _ ref _ retired (BufferIngress inbox closed) policy)=do
      incoming<-STM.atomically $ do
        STM.writeTVar closed True
        STM.writeTVar (policyClosed policy) True
        queued<-STM.flushTBQueue (policyInbox policy)
        mapM_ (resolvePolicy (Left "Editor session closed before policy decision")) queued
        STM.flushTBQueue inbox
      mapM_ (\submission->finishBufferSubmission runtime submission "Editor session closed before admission") incoming
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
queueWirePermission caller runtime@(Permissions _ registry ref _ _ _ _) operationFor desktop name args=do
  closed<-sessionClosed runtime
  case M.lookup name registry of
    Nothing->denied "Unknown MCP tool"
    Just _ | closed->denied "Editor session closed"
    Just _->do
      s<-readIORef ref
      live<-filterMActive (waiting s)
      if length live>=32 then denied "Too many MCP requests are awaiting permission" else do
        promise<-newEmptyMVar
        enabled<-newIORef True
        attempt<-newIORef Nothing
        claim<-newMVar ()
        stage<-newIORef (PolicyPending AdmitPolicy Nothing)
        let request=Waiting (nextTicket s) name args (operationFor promise) enabled False caller attempt Nothing claim stage
        writeIORef ref s {waiting=live++[request],nextTicket=nextTicket s+1}
        shown<-tickPermissions runtime desktop
        pure (shown,(readMVar promise >>= id) `onException` finish runtime request (Left "MCP permission request cancelled"))
  where denied reason=pure (desktop,pure (Left reason))

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
stepAdmittedBuild admission@(AdmittedBuild runtime@(Permissions _ _ ref _ _ _ owner) name args _ caller state) core desktop=do
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
      live<-filterMActive (waiting requests)
      if closed || length live>=32 then do
        atomicModifyIORef' state (\fresh->((case fresh of BuildReserved->BuildRejected (if closed then "Editor session closed" else "Too many MCP requests are awaiting permission"); _->fresh),()))
        stepAdmittedBuild admission core desktop
      else do
        enabled<-newIORef True
        attempt<-newIORef Nothing
        claim<-newMVar ()
        stage<-newIORef (PolicyPending BuildAdoptPolicy Nothing)
        let request=Waiting (nextTicket requests) name args (BuildAdoptionOperation admission) enabled False caller attempt Nothing claim stage
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
permissionReadCall runtime@(Permissions _ _ _ namespace _ _ _) callback desktop name args
  | name/="read_buffer" = pure (desktop,pure (Left "Read admission requires read_buffer"))
  | otherwise = permissionCall runtime admitted desktop name args
  where admitted d tool parameters=withReadAdmission namespace (sessionClosed runtime)
          (\receipt->callback receipt d tool parameters)

sessionClosed :: Permissions -> IO Bool
sessionClosed (Permissions _ _ _ _ _ (BufferIngress _ closed) _)=STM.readTVarIO closed

-- | Host-bound session reader. It requests fresh policy for every capture and
-- grants no Human provenance or reusable approval. Linked handlers only enqueue
-- and await; the owning tick admits the fixed operation under serialization.
bufferReader :: Permissions -> IO (Either Text ()) -> BufferReader
bufferReader (Permissions _ _ _ namespace _ ingress owner) caller=newBufferReader namespace
  (\reference->queueCapture ingress owner (CaptureSubmission reference caller))
  (queueCapture ingress owner (ListingSubmission caller))

-- | Same fixed ingress/claim/policy owner as buffer reads. The target retains
-- exact immutable body identity, never a Desktop or a live input capability.
windowReader :: Permissions -> IO (Either Text ()) -> Reads.WindowReadTarget -> IO (Either Text Reads.CapturedWindowRead)
windowReader (Permissions _ _ _ _ _ ingress owner) caller target=
  queueCapture ingress owner (WindowCaptureSubmission target caller)

-- The existing fixed capture operation owns acceptance and reply cancellation;
-- the factory only binds one host submission to its new promise and claim.
queueCapture :: BufferIngress -> PolicyOwner
  -> (MVar (Either Text a) -> IORef Bool -> MVar () -> CaptureSubmission)
  -> IO (Either Text a)
queueCapture (BufferIngress inbox closed) owner submissionFor=mask $ \restore->do
  promise<-newEmptyMVar
  enabled<-newIORef True
  claim<-newMVar ()
  accepted<-STM.atomically $ do
    stopped<-STM.readTVar closed
    full<-STM.isFullTBQueue inbox
    if stopped then pure (Left "Editor session closed before capture")
    else if full then pure (Left "Too many captures are awaiting admission")
    else STM.writeTBQueue inbox (ReadSubmission (submissionFor promise enabled claim)) >> STM.tryPutTMVar (policyWake owner) () >> pure (Right ())
  case accepted of
    Left err->pure (Left err)
    Right ()->restore (readMVar promise) `onException` finishCapture enabled claim promise (Left "Capture cancelled")

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
bufferEditor (Permissions _ _ _ namespace retired (BufferIngress inbox closed) owner) caller=newBufferEditor namespace $ \reference version patch->mask $ \restore->do
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
      else STM.writeTBQueue inbox (EditSubmission submission) >> STM.tryPutTMVar (policyWake owner) () >> pure (Right ())
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
finishBufferSubmission _ (ReadSubmission submission) err=let (_,claim,_)=captureLifetime submission
  in withMVar claim (\()->rejectCaptureOwned submission err)
finishBufferSubmission (Permissions _ _ _ _ retired _ _) (EditSubmission submission) err=finishDiffSubmission retired submission (Left err)

-- Fixed bounded transport only; PermissionState still has one serialized owner.
-- An interrupted extracted batch resolves every accepted reply before unwinding.
drainBufferRequests :: Permissions -> Desktop -> IO Desktop
drainBufferRequests runtime@(Permissions _ _ state namespace _ (BufferIngress inbox _) _) desktop=mask_ $ do
  incoming<-STM.atomically (STM.flushTBQueue inbox)
  foldM admitSafely desktop incoming `onException`
    mapM_ (\submission->finishBufferSubmission runtime submission "Buffer request owner interrupted") incoming
  where
    admitSafely current submission=admit current submission `catch` \(err::SomeException)->
      case fromException err :: Maybe SomeAsyncException of
        Just _->throwIO err
        Nothing->finishBufferSubmission runtime submission "Buffer request admission failed" >> pure current
    admit current submission=do
      let (enabled,claim,caller)=case submission of
            ReadSubmission capture->captureLifetime capture
            EditSubmission (DiffSubmission _ _ _ c _ e k _)->(e,k,c)
          target=case submission of
            ReadSubmission capture@ListingSubmission{}->Right ("list_buffers",object [],CaptureOperation capture)
            ReadSubmission capture@(CaptureSubmission reference _ _ _ _)->bufferTarget reference $ \ident->
              ("read_buffer",object ["bufferId" .= ident],CaptureOperation capture)
            ReadSubmission capture@(WindowCaptureSubmission reference _ _ _ _)->Right
              ("read_window",object ["windowId" .= Reads.windowReadIdentifier reference],CaptureOperation capture)
            EditSubmission diff@(DiffSubmission reference _ patch _ _ _ _ _)->bufferTarget reference $ \ident->
              ("buffer_apply_diff",object ["bufferId" .= ident,"revision" .= maybe 0 (revision.documentBuffer) (M.lookup ident (buffers current)),"diff" .= patch],DiffOperation diff)
          bufferTarget reference build=maybe (Left "Buffer reference belongs to another editor session") (Right . build) (referenceId namespace reference)
      withMVar claim $ \()->do
        live<-readIORef enabled
        when live $ case target of
          Left err->finishBufferSubmissionOwned submission err
          Right (name,args,op)->do
            original<-readIORef state
            liveRequests<-filterMActive (waiting original)
            if length liveRequests>=32 then finishBufferSubmissionOwned submission "Too many MCP requests are awaiting permission" else do
              attempt<-case submission of ReadSubmission _->newIORef Nothing; EditSubmission (DiffSubmission _ _ _ _ _ _ _ a)->pure a
              stage<-newIORef (PolicyPending AdmitPolicy Nothing)
              captured<-case submission of
                ReadSubmission _->pure (Right Nothing)
                EditSubmission (DiffSubmission reference expected _ _ _ _ _ _)->do
                  matches<-maybe (pure False) (versionCurrent expected . documentBuffer) (referenceId namespace reference >>= (`M.lookup` buffers current))
                  if not matches then pure (Left "Buffer identity or revision changed; read the buffer again") else
                    case capturePatchSource current args of
                      Left err->pure (Left err)
                      Right source->Right . Just <$> evaluate source
              case captured of
                Left err->finishBufferSubmissionOwned submission err
                Right source->do
                  let request=Waiting (nextTicket original) name args op enabled False caller attempt source claim stage
                  writeIORef state original {waiting=liveRequests++[request],nextTicket=nextTicket original+1}
        pure current
    finishBufferSubmissionOwned (ReadSubmission submission) err=rejectCaptureOwned submission err
    finishBufferSubmissionOwned (EditSubmission submission) err=finishDiffSubmissionOwned submission (Left err)

-- Caller holds the same short request claim used by cancellation. Only known
-- host capture/actor operations run here; no extension handler or reply wait.
captureSubmissionOwned :: Permissions -> CaptureSubmission -> Desktop -> IO ()
captureSubmissionOwned runtime@(Permissions _ _ _ namespace _ _ _) submission desktop=do
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
    BuildInputOperation _ promise->tryPutMVar promise (pure result) >> pure ()
    BuildAdoptionOperation (AdmittedBuild _ _ _ _ _ state)->atomicModifyIORef' state $ \current->
      (if ownsBuildRequest request current then BuildRejected (either id (const "Invalid build admission result") result) else current,())
    CaptureOperation submission->rejectCaptureOwned submission (either id (const "Invalid capture reply") result)
    DiffOperation submission->finishDiffSubmissionOwned submission (case result of Left err->Left err; Right _->Left "Invalid diff reply")

-- An attempt belongs to a ticket, but failure does not end that ticket. Retire
-- joins run on their own thread; neither tick nor dialog submission waits for a
-- worker's cancellation/finalizer while holding the desktop lock.
stopDiffAttempt :: Permissions -> Waiting -> IO ()
stopDiffAttempt (Permissions _ _ _ _ retired _ _) request=stopAttempt retired (patchAttempt request)

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
startDiffAttempt runtime request edited desktop=withMVar (requestClaim request) (\()->startDiffAttemptOwned runtime request edited desktop)

startDiffAttemptOwned :: Permissions -> Waiting -> Value -> Desktop -> IO Desktop
startDiffAttemptOwned runtime request edited desktop=case patchSource request of
  Nothing->diffFailureOwned runtime request "Missing original diff source" desktop
  Just source->mask_ $ do
    live<-readIORef (active request)
    if not live then pure desktop else do
      stopDiffAttempt runtime request
      let prepare=preparePatch source (if approvalRequired request then field "diff" (arguments request) else Nothing) edited
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
drainDiffAttempts runtime@(Permissions _ _ ref _ retired _ _) initial=do
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
          stage<-readIORef (policyStage request)
          if enabled && isPolicyReady stage && not (approvalRequired request) && toolName request=="buffer_apply_diff"
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

-- | Display the oldest live approval and withdraw stale or cancelled prompts.
tickPermissions :: Permissions -> Desktop -> IO Desktop
tickPermissions runtime@(Permissions _ _ ref _ _ _ _) original=do
  desktop<-drainSettings runtime original >>= drainBufferRequests runtime >>= drainDiffAttempts runtime >>= drainPolicies runtime
  s<-readIORef ref
  live<-filterMActive (waiting s)
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
permissionAction runtime@(Permissions _ registry ref _ _ _ owner) action values desktop=do
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
          Just request->do
            let edited=case (toolName request,arguments request,drop 1 values) of
                  ("buffer_apply_diff",Object args,text:_)->Object (KM.insert "diff" (String text) args)
                  _->arguments request
            withMVar (requestClaim request) $ \()->do
              live<-readIORef (active request)
              when live $ do
                -- A newly accepted review supersedes the old worker attempt.
                -- Its completion must not overwrite this fresh Allow phase.
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
drainSettings runtime@(Permissions _ registry ref _ _ _ owner) desktop=do
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
drainPolicies runtime@(Permissions _ registry ref _ _ _ owner) original=do
  requests<-readIORef ref >>= filterMActive . waiting
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
        DiffOperation (DiffSubmission _ expected _ _ _ _ _ _)->do
          let doc=field "bufferId" (arguments request) >>= (`M.lookup` buffers desktop)
          current<-maybe (pure False) (versionCurrent expected . documentBuffer) doc
          pure (if not current then Left "Buffer identity or revision changed; read the buffer again" else Right (patchSource request))
        _->pure (Right (patchSource request))
      case prepared of
        Left err->finishOwned runtime request (Left err) >> pure desktop {status=err}
        Right source->do
          let polling=toolName request=="ask_user" && case arguments request of Object fields->KM.keys fields==["questionId"]; _->False
              approved=request {approvalRequired=mode==Prompt && not polling,patchSource=source}
          replaceRequest approved
          if approvalRequired approved then pure desktop else execute desktop approved (arguments request)
    applyPolicy desktop request BuildAdoptPolicy mode=case operation request of
      BuildAdoptionOperation (AdmittedBuild _ _ _ approved _ state)
        | mode==Prompt && not approved->finishOwned runtime request (Left "This MCP tool now requires approval; submit again") >> pure desktop
        | otherwise->do
            epoch<-readIORef (policyEpoch owner)
            writeIORef state (BuildAllowed request epoch)
            pure desktop
      _->finishOwned runtime request (Left "Invalid build admission operation") >> pure desktop
    applyPolicy desktop request (AllowPolicy edited review) _=do
      current<-reviewCurrent request review desktop
      let owns=case dialog desktop of Just dg->purpose dg==PermissionDialog (approvalAction request); _->False
      if not current || not owns then pure desktop {status="Permission review changed; approve the current review."}
      else execute desktop request edited
    applyPolicy desktop request (AdoptPolicy review prepared) mode=do
      current<-reviewCurrent request review desktop
      if not current then pure desktop {status="Diff review changed; approve the current review."}
      else if not (approvalRequired request) && mode/=Enable then
        finishOwned runtime request (Left "This MCP tool now requires approval; submit again") >> pure desktop {status="Diff now requires approval."}
      else case prepared of
        Left err->diffFailureOwned runtime request err desktop
        Right patch->do
          adopted<-commitPatch patch desktop
          case adopted of
            Left err->diffFailureOwned runtime request err desktop
            Right (updated,response)->do
              stopDiffAttempt runtime request
              case operation request of
                DiffOperation submission->finishDiffSubmissionOwned submission (Right response)
                _->finishOwned runtime request (Left "Invalid diff operation")
              pure (closeReview request updated) {status="Applied exact buffer diff."}
    execute desktop request edited=case operation request of
      CaptureOperation submission->captureSubmissionOwned runtime submission desktop >> pure (closeReview request desktop)
      DiffOperation _->startDiffAttemptOwned runtime request edited desktop
      BuildAdoptionOperation _->finishOwned runtime request (Left "Invalid build admission phase") >> pure desktop
      BuildInputOperation callback promise->do
        state<-newIORef BuildUnused
        let admission=AdmittedBuild runtime (toolName request) edited (approvalRequired request) (patchCaller request) state
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

reviewCurrent :: Waiting -> Maybe ContentVersion -> Desktop -> IO Bool
reviewCurrent request Nothing _=pure (not (approvalRequired request) || toolName request/="buffer_apply_diff")
reviewCurrent request (Just expected) desktop=case dialog desktop of
  Just dg | purpose dg==PermissionDialog (approvalAction request)->case [b | TextArea "diff" True b _ _ _<-fields dg] of
    b:_->versionCurrent expected b
    _->pure False
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
awaitPermissionWork (Permissions _ _ _ _ _ _ owner)=STM.takeTMVar (policyWake owner)

signalPermissionWork :: Permissions -> IO ()
signalPermissionWork (Permissions _ _ _ _ _ _ owner)=STM.atomically (STM.tryPutTMVar (policyWake owner) () >> pure ())

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
