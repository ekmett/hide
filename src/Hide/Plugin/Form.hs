{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
-- | Module      : Hide.Plugin.Form
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : ExistentialQuantification, OverloadedStrings
--
-- Host-owned single-line modal input for trusted linked commands.
-- The registration owns the form lifetime; presentation refresh never changes
-- its action or resets the host's draft/selection. Adapters run on a worker.
--
-- Laws: refresh preserves the reference and host input; reopen mints a fresh
-- reference; each accepted submission/result is claimed/consumed at most once.
-- Host primitives grant no authority: admission/submission require the owning
-- host's current human/modal checks. This slice has no general widget reducer.
module Hide.Plugin.Form
  ( -- * Prepared single-line input
    FormRef
  , FormAction
  , InputFormSpec(..)
  , PreparedInputForm
  , formAction
  , prepareInputForm
  , formReference
  , formSpec
    -- * Metadata-only refresh
  , InputFormUpdate
  , updateFormReference
  , refreshInputForm
  , admitFormRefresh
    -- * Host lifetime and submission
  , formCurrent
  , admitInputForm
  , claimFormSubmission
  , submissionCurrent
  , invokeFormAction
  , finishFormSubmission
  , retireForm
  ) where

import Control.Concurrent.STM
import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique
import Hide.Plugin.Command

data Phase = Pending | Open | Submitted | Retired deriving Eq
-- | Exact opaque modal lifetime. Reopening always obtains a fresh identity.
data FormRef = FormRef !Unique !(TVar (Integer,Phase))
instance Eq FormRef where FormRef a _==FormRef b _=a==b
instance Show FormRef where show (FormRef a _)="FormRef "++show (hashUnique a)
-- | Bounded semantic labels and an initial selected value. Installed input is
-- host-owned: refresh only changes labels, never this initial value.
data InputFormSpec = InputFormSpec
  { inputFormTitle :: !Text, inputFormLabel :: !Text, inputFormInitial :: !Text
  , inputFormSubmit :: !Text } deriving (Eq,Show)
-- | Typed registered action; the registry owns its validity.
data FormAction c r = forall a b. FormAction !(Registry c) !(Command c a b) (Text -> a) (c -> b -> IO r)
-- | Capture typed arguments without exposing a desktop callback. The argument
-- builder and reply adapter execute only in 'invokeFormAction', on its worker.
formAction :: Registry c -> Command c a b -> (Text -> a) -> (c -> b -> IO r) -> FormAction c r
formAction=FormAction
-- | Worker-validated opening metadata with its captured typed action.
data PreparedInputForm c r = PreparedInputForm !FormRef !Integer !Bool !InputFormSpec !(FormAction c r)
-- | Exact immutable identity; equality never inspects form values.
formReference :: PreparedInputForm c r -> FormRef
formReference (PreparedInputForm ref _ _ _ _)=ref
-- | Worker-validated metadata. Initial value applies only at first open.
formSpec :: PreparedInputForm c r -> InputFormSpec
formSpec (PreparedInputForm _ _ _ spec _)=spec

checkedSpec :: InputFormSpec -> IO (Either CommandError InputFormSpec)
checkedSpec (InputFormSpec title label initial submit)
  | any (\s->T.null s || T.length s>256 || T.any control s) [title,label,submit] || T.length initial>8192 || T.any control initial=
      pure (Left (InvalidArguments "Invalid single-line form metadata."))
  | otherwise=do
      let spec=InputFormSpec (T.copy title) (T.copy label) (T.copy initial) (T.copy submit)
      _<-evaluate (sum (map T.length [inputFormTitle spec,inputFormLabel spec,inputFormInitial spec,inputFormSubmit spec]))
      pure (Right spec)
  where control c=c<' ' || c=='\DEL'
-- | Mint a fresh pending lifetime on the calling worker. Closing/reopening can
-- never reuse the old reference. Registration retirement prevents admission.
prepareInputForm :: InputFormSpec -> FormAction c r -> IO (Either CommandError (PreparedInputForm c r))
prepareInputForm spec action=do
  checked<-checkedSpec spec
  case checked of
    Left err->pure (Left err)
    Right value->do
      ref<-FormRef <$> newUnique <*> newTVarIO (1,Pending)
      pure (Right (PreparedInputForm ref 1 True value action))
-- | Metadata-only publication; never carries or changes the action.
data InputFormUpdate = InputFormUpdate !FormRef !Integer !InputFormSpec
-- | Exact lifetime of a metadata-only publication; no command or callback.
updateFormReference :: InputFormUpdate -> FormRef
updateFormReference (InputFormUpdate reference _ _)=reference
-- | Issue later metadata for the same action. Only latest metadata may adopt;
-- labels never revoke a live submission or overwrite installed widget state.
refreshInputForm :: FormRef -> InputFormSpec -> IO (Either CommandError (Maybe InputFormUpdate))
refreshInputForm ref@(FormRef _ state) spec=do
  checked<-checkedSpec spec
  case checked of
    Left err->pure (Left err)
    Right value->atomically $ do
      (revision,phase)<-readTVar state
      if phase/=Open then pure (Right Nothing) else do
        let next=revision+1
        writeTVar state (next,phase)
        pure (Right (Just (InputFormUpdate ref next value)))
-- | Host metadata adoption for an already installed exact live form. The stored
-- action is unchanged. Current host draft, selection and focus remain separate.
admitFormRefresh :: PreparedInputForm c r -> InputFormUpdate -> IO (Maybe (PreparedInputForm c r))
admitFormRefresh original@(PreparedInputForm ref@(FormRef _ state) _ _ _ action) (InputFormUpdate target version spec)=do
  live<-formCurrent original
  (revision,phase)<-readTVarIO state
  pure (if live && ref==target && phase==Open && revision==version then Just (PreparedInputForm ref version False spec action) else Nothing)
-- | Host scalar lifetime check, including the existing command registration.
formCurrent :: PreparedInputForm c r -> IO Bool
formCurrent (PreparedInputForm (FormRef _ state) _ _ _ (FormAction registry command _ _))=do
  live<-commandCurrent registry (commandRef command)
  (_,phase)<-readTVarIO state
  pure (live && phase/=Retired)
-- | Host admission after current actor/modal checks. A refresh cannot open a
-- closed form; an opening reply can be adopted once only.
admitInputForm :: Bool -> PreparedInputForm c r -> IO Bool
admitInputForm present form@(PreparedInputForm (FormRef _ state) version opening _ _)=do
  live<-formCurrent form
  if not live then pure False else atomically $ do
    (revision,phase)<-readTVar state
    if revision/=version || not (phase==Pending && opening && not present || phase==Open && present)
      then pure False else writeTVar state (revision,Open) >> pure True
-- | Claim exactly one human submission after host ownership validation. Metadata
-- revisions are deliberately irrelevant: the captured action is unchanged.
claimFormSubmission :: PreparedInputForm c r -> IO Bool
claimFormSubmission form=do
  live<-formCurrent form
  let FormRef _ state=formReference form
  if not live then pure False else atomically $ do
    (revision,phase)<-readTVar state
    if phase/=Open then pure False else writeTVar state (revision,Submitted) >> pure True
-- | An accepted submission survives closing its own modal, until explicitly
-- retired, superseded, or consumed. It never survives registration shutdown.
submissionCurrent :: PreparedInputForm c r -> IO Bool
submissionCurrent form=do
  live<-formCurrent form
  let FormRef _ state=formReference form
  (_,phase)<-readTVarIO state
  pure (live && phase==Submitted)
-- | Invoke the typed action on the existing reply worker. Results must be forced
-- by their owning adapter before publication; no adapter runs in a host commit.
invokeFormAction :: PreparedInputForm c r -> c -> Text -> IO (Either CommandError r)
invokeFormAction (PreparedInputForm _ _ _ _ (FormAction registry command arguments prepare)) context text
  | T.length text>8192 || T.any (\c->c<' ' || c=='\DEL') text=pure (Left (InvalidArguments "Invalid single-line form value."))
  | otherwise=do
      result<-invoke registry command context (arguments text)
      case result of Left err->pure (Left err); Right value->Right <$> prepare context value
-- | Atomically consume the submission once before applying its checked result.
finishFormSubmission :: FormRef -> IO Bool
finishFormSubmission (FormRef _ state)=atomically $ do
  (revision,phase)<-readTVar state
  if phase/=Submitted then pure False else writeTVar state (revision,Retired) >> pure True
-- | Idempotent close/reopen invalidation. No callbacks or worker joins occur.
retireForm :: FormRef -> IO ()
retireForm (FormRef _ state)=atomically (modifyTVar' state (\(revision,_)->(revision,Retired)))
