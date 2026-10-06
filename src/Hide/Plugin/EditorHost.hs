{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- |
-- Module      : Hide.Plugin.EditorHost
-- Copyright   : (c) Edward Kmett
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : ExistentialQuantification, OverloadedStrings
--
-- A host-owned multiline draft attached to a prepared window. The draft outlives
-- a frame mount; neither identity contains a mutable Buffer or a desktop callback.
-- Typed actions belong to their existing command registry and execute on its
-- owning worker. These host primitives grant no input or caller authority.
--
-- Laws: remount changes the mount, not the draft; body refresh changes neither;
-- accepted submission reads retain no Undo; applying a submission update requires
-- the same draft and exact immutable version, even while its frame is hidden.
module Hide.Plugin.EditorHost
  ( DraftRef, newDraftRef, draftRefCurrent, retireDraftRef
  , EditorMount, mountDraft, mountSpec, mountActions, mountCurrent, retireEditorMount
  , EditorSpec(..), EditorSlot(..), EditorAction, editorAction
  , PreparedEditor, prepareEditorBuffer, remountEditor, editorMount, editorInitialBuffer
  , installedEditor, editorCurrent, editorBindingCurrent, claimEditorMount
  , DraftSubmission, captureDraftSubmission, submissionDraft, submissionMount
  , submissionVersion, submissionContent, submissionAction, submissionSlot, sameDraftSubmission, submissionAccepted, abortEditorSubmission
  , invokeEditorAction, EditorUpdate, clearEditorDraft, replacementEditorDraft
  , updateSubmission, updateReplacement, consumeEditorUpdate
  ) where

import Control.Concurrent.STM
import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique
import Hide.Buffer (Buffer,BufferContent,newBuffer,bufferContent)
import Hide.Plugin.BufferHost (ContentVersion,captureVersion)
import Hide.Plugin.Command

-- | A draft's owning target/session lifetime. The host stores its sole editable
-- Buffer/selection separately, keyed by this reference. Closing a frame does not
-- retire it; target retirement does. Identity checks never inspect draft payloads.
data DraftRef = DraftRef !Unique !(TVar Bool)
instance Eq DraftRef where DraftRef a _==DraftRef b _=a==b
instance Ord DraftRef where compare (DraftRef a _) (DraftRef b _)=compare a b
instance Show DraftRef where show (DraftRef ident _)="DraftRef "++show (hashUnique ident)
-- | Allocate in the actual IO owner, never through a pure identity counter.
newDraftRef :: IO DraftRef
newDraftRef=DraftRef <$> newUnique <*> newTVarIO True
-- | /O(1)/. Target/session liveness, independent of visible frame geometry.
draftRefCurrent :: DraftRef -> IO Bool
draftRefCurrent (DraftRef _ live)=readTVarIO live
-- | Idempotent target retirement. Existing immutable submitted reads survive;
-- no future input or update is admitted. It performs no worker cancellation.
retireDraftRef :: DraftRef -> IO ()
retireDraftRef (DraftRef _ live)=atomically (writeTVar live False)

-- | Fixed input behavior and labels. The host owns all caret/selection geometry;
-- Code input enables its existing indentation/fenced-input grammar. Enter uses
-- the first action and Ctrl+Enter the second, except the host's newline grammar.
data EditorSpec = EditorSpec
  { editorCodeInput :: !Bool
  , editorDefaultLabel :: !Text
  , editorAlternateLabel :: !Text
  } deriving (Eq,Show)
-- | Fixed action position, independent of registration identity. Both slots may
-- use the same typed Command with different captured argument adapters.
data EditorSlot = DefaultEditor | AlternateEditor deriving (Eq,Show)
data MountPhase = MountPending | MountOpen | MountRetired deriving Eq
-- | Exact frame attachment. Reopening always obtains a fresh identity. Its
-- immutable actions cannot be changed by an ordinary body publication.
data EditorMount = EditorMount !Unique !DraftRef !EditorSpec !CommandRef !CommandRef !(TVar MountPhase)
instance Eq EditorMount where EditorMount a _ _ _ _ _==EditorMount b _ _ _ _ _=a==b
instance Ord EditorMount where compare (EditorMount a _ _ _ _ _) (EditorMount b _ _ _ _ _)=compare a b
instance Show EditorMount where show (EditorMount ident _ _ _ _ _)="EditorMount "++show (hashUnique ident)
-- | /O(1)/. The sole host draft selected by this attachment.
mountDraft :: EditorMount -> DraftRef
mountDraft (EditorMount _ draft _ _ _ _)=draft
-- | /O(1)/. Immutable presentation and input metadata, with no callable values.
mountSpec :: EditorMount -> EditorSpec
mountSpec (EditorMount _ _ spec _ _ _)=spec
-- | /O(1)/. Default and alternate exact registrations, independently of labels.
mountActions :: EditorMount -> (CommandRef,CommandRef)
mountActions (EditorMount _ _ _ normal alternate _)=(normal,alternate)
-- | /O(1)/. Scalar draft and frame liveness. Registration checks belong to the
-- typed owning binding, not to this metadata projection.
mountCurrent :: EditorMount -> IO Bool
mountCurrent (EditorMount _ (DraftRef _ live) _ _ _ state)=atomically $ do
  current<-readTVar live
  phase<-readTVar state
  pure (current && phase==MountOpen)
-- | Idempotent frame retirement. This cannot undo already committed provider
-- work, and does not retire a hidden draft or cancel unrelated owner jobs.
retireEditorMount :: EditorMount -> IO ()
retireEditorMount (EditorMount _ _ _ _ _ state)=atomically (writeTVar state MountRetired)

-- | Typed registered argument/reply adapters. Both execute only on the owning
-- action worker. No closure is placed in prepared readonly window content.
data EditorAction c r = forall a b. EditorAction !(Registry c) !(Command c a b)
  (DraftSubmission -> Either CommandError a) (c -> b -> IO r)
-- | Bind a typed command to immutable submitted input. Registration is not an
-- MCP exposure or a human grant; the host still validates input ownership.
editorAction :: Registry c -> Command c a b -> (DraftSubmission -> Either CommandError a) -> (c -> b -> IO r) -> EditorAction c r
editorAction=EditorAction
actionReference :: EditorAction c r -> CommandRef
actionReference (EditorAction _ command _ _)=commandRef command
actionCurrent :: EditorAction c r -> IO Bool
actionCurrent (EditorAction registry command _ _)=commandCurrent registry (commandRef command)
-- | Preparation-time input seed and actions. At adoption the host transfers the
-- seed Buffer once and retains 'installedEditor', which contains no second draft.
data PreparedEditor c r = PreparedEditor !EditorMount !(Maybe Buffer) !(EditorAction c r) !(EditorAction c r)
-- | Validate and detach small labels on the calling preparation worker. The
-- seed is transferred only if the host does not already own this DraftRef.
prepareEditorBuffer :: DraftRef -> EditorSpec -> Buffer -> EditorAction c r -> EditorAction c r -> IO (Either CommandError (PreparedEditor c r))
prepareEditorBuffer draft spec initial normal alternate
  | any invalid [editorDefaultLabel spec,editorAlternateLabel spec]=pure (Left (InvalidArguments "Invalid editor action label."))
  | otherwise=do
      _<-evaluate initial
      let metadata=spec {editorDefaultLabel=T.copy (editorDefaultLabel spec),editorAlternateLabel=T.copy (editorAlternateLabel spec)}
      _<-evaluate (T.length (editorDefaultLabel metadata)+T.length (editorAlternateLabel metadata))
      mount<-EditorMount <$> newUnique <*> pure draft <*> pure metadata <*> pure (actionReference normal) <*> pure (actionReference alternate) <*> newTVarIO MountPending
      pure (Right (PreparedEditor mount (Just initial) normal alternate))
  where invalid value=T.null value || T.length value>256 || T.any (\c->c<' ' || c=='\DEL') value
-- | Mint a fresh pending frame for the same draft/actions, without copying or
-- reseeding its editable state. A retired registration is refused at admission.
remountEditor :: PreparedEditor c r -> IO (PreparedEditor c r)
remountEditor (PreparedEditor old _ normal alternate)=do
  mount<-EditorMount <$> newUnique <*> pure (mountDraft old) <*> pure (mountSpec old) <*> pure (actionReference normal) <*> pure (actionReference alternate) <*> newTVarIO MountPending
  pure (PreparedEditor mount Nothing normal alternate)
-- | /O(1)/. Metadata carried by the host's frame; no action closures.
editorMount :: PreparedEditor c r -> EditorMount
editorMount (PreparedEditor mount _ _ _)=mount
-- | Preparation-time seed only. Existing host-owned drafts always take precedence.
editorInitialBuffer :: PreparedEditor c r -> Maybe Buffer
editorInitialBuffer (PreparedEditor _ seed _ _)=seed
-- | Drop the transferred seed from the retained action binding.
installedEditor :: PreparedEditor c r -> PreparedEditor c r
installedEditor (PreparedEditor mount _ normal alternate)=PreparedEditor mount Nothing normal alternate
-- | Check current draft and both exact registrations without reading input.
editorCurrent :: PreparedEditor c r -> IO Bool
editorCurrent (PreparedEditor mount _ normal alternate)=do
  draft<-draftRefCurrent (mountDraft mount)
  first<-actionCurrent normal
  second<-actionCurrent alternate
  let EditorMount _ _ _ _ _ state=mount
  phase<-readTVarIO state
  pure (draft && first && second && phase/=MountRetired)
-- | Hidden drafts retain their callable binding after a frame closes. Its
-- owner retires the draft when the publication scope or registration ends.
editorBindingCurrent :: PreparedEditor c r -> IO Bool
editorBindingCurrent (PreparedEditor mount _ normal alternate)=do
  draft<-draftRefCurrent (mountDraft mount)
  first<-actionCurrent normal
  second<-actionCurrent alternate
  pure (draft && first && second)

-- | Host-only joint-admission primitive after actor/registration checks. Use in
-- the same STM transaction as the readonly body opening; a failed pairing cannot
-- consume either lifetime. It never reopens a retired or already adopted mount.
claimEditorMount :: EditorMount -> STM Bool
claimEditorMount (EditorMount _ (DraftRef _ live) _ _ _ state)=do
  current<-readTVar live
  phase<-readTVar state
  if not current || phase/=MountPending then pure False
  else writeTVar state MountOpen >> pure True

-- | Immutable content/version captured by the host at the human input turn.
-- It retains the measured live tree only, with no separate baseline or Undo.
-- The one-shot result claim prevents duplicate completion from clearing twice.
data SubmissionPhase = Captured | Invoked | Consumed deriving Eq
data DraftSubmission = DraftSubmission !EditorMount !EditorSlot !CommandRef !ContentVersion !BufferContent !(TVar SubmissionPhase)
instance Eq DraftSubmission where
  DraftSubmission _ _ _ _ _ a==DraftSubmission _ _ _ _ _ b=a==b
instance Show DraftSubmission where
  show submitted="DraftSubmission "++show (submissionMount submitted)++" "++show (submissionSlot submitted)
-- | Capture after exact active/focused mount and human policy checks. Only an
-- action declared on this mount is admissible. This never encodes the draft.
captureDraftSubmission :: EditorMount -> EditorSlot -> Buffer -> IO (Maybe DraftSubmission)
captureDraftSubmission mount slot draft=do
  live<-mountCurrent mount
  if not live then pure Nothing else do
    version<-captureVersion draft
    Just . DraftSubmission mount slot (case slot of DefaultEditor->fst (mountActions mount); AlternateEditor->snd (mountActions mount)) version (bufferContent draft) <$> newTVarIO Captured
-- | /O(1)/. Exact target draft, independent of the currently shown frame.
submissionDraft :: DraftSubmission -> DraftRef
submissionDraft=mountDraft . submissionMount
-- | /O(1)/. Original frame lifetime, never a window number or title.
submissionMount :: DraftSubmission -> EditorMount
submissionMount (DraftSubmission mount _ _ _ _ _)=mount
-- | /O(1)/. Exact immutable identity; equal numeric revisions are insufficient.
submissionVersion :: DraftSubmission -> ContentVersion
submissionVersion (DraftSubmission _ _ _ version _ _)=version
-- | /O(1)/. Borrowed immutable read. Flattening/evaluation belongs to a worker.
submissionContent :: DraftSubmission -> BufferContent
submissionContent (DraftSubmission _ _ _ _ content _)=content
-- | /O(1)/. Exact registered action selected by the original input.
submissionAction :: DraftSubmission -> CommandRef
submissionAction (DraftSubmission _ _ action _ _ _)=action
-- | /O(1)/. Original argument-adapter slot, even when both use one Command.
submissionSlot :: DraftSubmission -> EditorSlot
submissionSlot (DraftSubmission _ slot _ _ _ _)=slot
-- | Small duplicate-pending receipt comparison; never compares input or Undo.
sameDraftSubmission :: DraftSubmission -> DraftSubmission -> Bool
sameDraftSubmission a b=submissionDraft a==submissionDraft b && submissionVersion a==submissionVersion b && submissionSlot a==submissionSlot b
-- | Accepted handoff is independent of later frame closure. This lets an
-- owner cancel unaccepted preparation without recalling committed work.
submissionAccepted :: DraftSubmission -> IO Bool
submissionAccepted (DraftSubmission _ _ _ _ _ state)=(==Invoked) <$> readTVarIO state

-- | Atomically retire only unaccepted input before scheduling off-owner
-- cancellation. The command handoff competes on this same phase; Invoked work
-- drains and can never be recalled by a later frame or scope closure.
abortEditorSubmission :: DraftSubmission -> IO Bool
abortEditorSubmission (DraftSubmission _ _ _ _ _ state)=atomically $ do
  phase<-readTVar state
  if phase/=Captured then pure False else writeTVar state Consumed >> pure True

-- | Invoke on the existing action worker. Recheck the original mount before
-- registry admission, after evaluating its argument adapter. This one-shot claim
-- is the host input handoff; registry admission still refuses retired commands.
-- Once the command accepts, its ordinary drain law applies;
-- closing a frame cannot recall committed work or an immutable granted read.
invokeEditorAction :: PreparedEditor c r -> c -> DraftSubmission -> IO (Either CommandError r)
invokeEditorAction (PreparedEditor mount@(EditorMount _ (DraftRef _ draftLive) _ _ _ mountState) _ normal alternate) context submitted@(DraftSubmission _ _ _ _ _ state)=
  case selected of
    EditorAction registry command arguments reply
      | commandRef command/=submissionAction submitted->pure (Left (CommandRejected "Editor action changed."))
      | otherwise->case arguments submitted of
          Left err->pure (Left err)
          Right value->do
            -- Force the argument adapter before the short input handoff. It may
            -- inspect the immutable read, so it belongs on this worker, never
            -- inside STM or after claiming a mount which could close meanwhile.
            captured<-evaluate value
            accepted<-atomically $ do
              live<-readTVar draftLive
              phase<-readTVar mountState
              claimed<-readTVar state
              if mount/=submissionMount submitted || not live || phase/=MountOpen || claimed/=Captured
                then pure False else writeTVar state Invoked >> pure True
            if not accepted then pure (Left (CommandRejected "Editor submission expired.")) else do
              result<-invoke registry command context captured
              case result of Left err->pure (Left err); Right resultValue->Right <$> (reply context resultValue >>= evaluate)
  where selected=case submissionSlot submitted of DefaultEditor->normal; AlternateEditor->alternate

-- | A result for one exact submitted draft. This cannot select another target,
-- reopen a frame or authorize an arbitrary edit of the current composer.
data EditorUpdate = EditorUpdate !DraftSubmission !Buffer
-- | Clear only the submitted immutable version after the real owning operation
-- accepts it. The host performs the final version check at serialized adoption.
clearEditorDraft :: DraftSubmission -> EditorUpdate
clearEditorDraft submitted=EditorUpdate submitted (newBuffer "")
-- | Prepare an exact-version replacement on the owning result worker.
replacementEditorDraft :: DraftSubmission -> Text -> IO EditorUpdate
replacementEditorDraft submitted text=evaluate (EditorUpdate submitted (newBuffer text))
-- | /O(1)/. Receipt checked against the host's sole draft state.
updateSubmission :: EditorUpdate -> DraftSubmission
updateSubmission (EditorUpdate submitted _)=submitted
-- | /O(1)/. Prepared replacement; transfer it only after exact version validation.
updateReplacement :: EditorUpdate -> Buffer
updateReplacement (EditorUpdate _ replacement)=replacement
-- | Consume once, after final draft/version/current-owner checks. A hidden mount
-- is allowed; target retirement refuses. No input or process authority is granted.
consumeEditorUpdate :: EditorUpdate -> IO Bool
consumeEditorUpdate (EditorUpdate (DraftSubmission mount _ _ _ _ consumed) _)=atomically $ do
  let DraftRef _ live=mountDraft mount
  current<-readTVar live
  used<-readTVar consumed
  if not current || used/=Invoked then pure False else writeTVar consumed Consumed >> pure True
