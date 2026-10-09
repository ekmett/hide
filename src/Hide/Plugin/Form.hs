{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
-- | Module      : Hide.Plugin.Form
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : ExistentialQuantification, OverloadedStrings
--
-- Host-owned single-line inputs and finite-choice modals for trusted linked commands.
-- The registration owns the form lifetime; presentation refresh never changes
-- its action or resets the host's draft/selection. Adapters run on a worker.
--
-- Laws: refresh preserves the reference and host input; reopen mints a fresh
-- reference; each accepted submission/result is claimed/consumed at most once.
-- Host primitives grant no authority: admission/submission require the owning
-- host's current human/modal checks. This slice has no general widget reducer.
module Hide.Plugin.Form
  ( -- * Prepared input and choices
    FormRef
  , FormDisclosure(..)
  , formDisclosure
  , FormAction
  , InputField(..)
  , FormValue(..)
  , FormSpec(..)
  , PreparedForm
  , formAction
  , inputsFormAction
  , prepareForm
  , formReference
  , formSpec
  , formRevision
  , formChoiceAt
  , formChoiceIndex
    -- * Metadata-only refresh
  , FormUpdate
  , updateFormReference
  , refreshForm
  , admitFormRefresh
    -- * Host lifetime and submission
  , formCurrent
  , admitForm
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
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Hide.Plugin.Command

data Phase = Pending | Open | Submitted | Retired deriving Eq
data Catalogue = ConfirmationCatalogue | InputCatalogue | InputsCatalogue ![Text] | ChoiceCatalogue !(S.Set Text) deriving Eq
-- | Immutable read/capture disclosure for trusted linked preparations. This
-- grants no input or source access: owners must sanitize public metadata first.
-- Refresh cannot change disclosure; both variants still require human submit.
data FormDisclosure = PrivateForm | ReadableForm deriving (Eq,Show)
-- | Exact opaque modal lifetime. Reopening always obtains a fresh identity.
data FormRef = FormRef !Unique !(TVar (Integer,Phase)) !Catalogue !FormDisclosure
instance Eq FormRef where FormRef a _ _ _==FormRef b _ _ _=a==b
instance Show FormRef where show (FormRef a _ _ _)="FormRef "++show (hashUnique a)
-- | /O(1)/. Preparation-time read/capture policy, independent of input authority.
formDisclosure :: FormRef -> FormDisclosure
formDisclosure (FormRef _ _ _ disclosure)=disclosure
-- | One named single-line field. IDs bind values, independently of labels.
-- A form has at most six fields with unique ordered IDs (1–128 scalars).
data InputField = InputField
  { inputId :: !Text, inputLabel :: !Text, inputInitial :: !Text } deriving (Eq,Show)
-- | A scalar input/choice or a complete named-input submission. The host owns
-- current drafts; the action worker validates bounds and the exact field keyset.
data FormValue = TextValue !Text | InputValues !(M.Map Text Text) deriving (Eq,Show)
-- | Fixed host widgets. A confirmation submits the empty scalar value.
-- Choice IDs are provider-owned values, never labels.
-- Choices are nonempty, unique and bounded to the existing ACP limits (512
-- entries, 4096 scalars per ID); visible labels are at most 256 scalars.
-- Initial input/choice applies only at opening, never on metadata refresh.
data FormSpec
  = ConfirmationFormSpec
    { formTitle :: !Text, formLabel :: !Text, formSubmit :: !Text }
  | InputFormSpec
    { formTitle :: !Text, formLabel :: !Text, inputFormInitial :: !Text
    , formSubmit :: !Text }
  | InputsFormSpec
    { formTitle :: !Text, inputsFormFields :: ![InputField], formSubmit :: !Text }
  | ChoiceFormSpec
    { formTitle :: !Text, formLabel :: !Text, choiceFormChoices :: ![(Text,Text)]
    , choiceFormInitial :: !Text, formSubmit :: !Text } deriving (Eq,Show)
-- | Typed registered action; the registry owns its validity.
data FormAction c r = forall a b. FormAction !Bool !(Registry c) !(Command c a b) (FormValue -> Either CommandError a) (c -> b -> IO r)
-- | Capture typed arguments without exposing a desktop callback. The argument
-- builder and reply adapter execute only in 'invokeFormAction', on its worker.
formAction :: Registry c -> Command c a b -> (Text -> a) -> (c -> b -> IO r) -> FormAction c r
formAction registry command arguments=FormAction False registry command (\value->case value of
  TextValue text->Right (arguments text)
  _->Left (InvalidArguments "Expected a scalar form value."))
-- | Bind immutable named fields to typed arguments on the action worker.
-- Preparation rejects a scalar widget paired with this named-input action.
inputsFormAction :: Registry c -> Command c a b -> (M.Map Text Text -> Either CommandError a) -> (c -> b -> IO r) -> FormAction c r
inputsFormAction registry command arguments=FormAction True registry command (\value->case value of
  InputValues fields->arguments fields
  _->Left (InvalidArguments "Expected named form values."))
-- | Worker-validated opening metadata with its captured typed action.
data PreparedForm c r = PreparedForm !FormRef !Integer !Bool !FormSpec !(V.Vector (Text,Text)) !(M.Map Text Int) !(FormAction c r)
-- | Exact immutable identity; equality never inspects form values.
formReference :: PreparedForm c r -> FormRef
formReference (PreparedForm ref _ _ _ _ _ _)=ref
-- | Worker-validated metadata. Initial value applies only at first open.
formSpec :: PreparedForm c r -> FormSpec
formSpec (PreparedForm _ _ _ spec _ _ _)=spec

-- | /O(1)/. Ordering receipt for index-based host submissions. A metadata
-- refresh preserves the FormRef but changes this scalar version.
formRevision :: PreparedForm c r -> Integer
formRevision (PreparedForm _ version _ _ _ _ _)=version

-- | /O(1)/. Resolve the installed ListBox index against the exact captured form.
formChoiceAt :: PreparedForm c r -> Int -> Maybe Text
formChoiceAt (PreparedForm _ _ _ _ choices _ _) index=fst <$> choices V.!? index
-- | /O(log n)/. Preserve the selected stable ID across a metadata-only reorder.
formChoiceIndex :: PreparedForm c r -> Text -> Maybe Int
formChoiceIndex (PreparedForm _ _ _ _ _ index _) value=M.lookup value index

checkedSpec :: FormSpec -> IO (Either CommandError FormSpec)
checkedSpec spec
  | any invalidLabel [formTitle spec,formSubmit spec]=invalid
  | otherwise=case spec of
      ConfirmationFormSpec title label submit
        | invalidLabel label->invalid
        | otherwise->checked (ConfirmationFormSpec (T.copy title) (T.copy label) (T.copy submit))
      InputFormSpec title label initial submit
        | invalidLabel label || invalidInput initial->invalid
        | otherwise->checked (InputFormSpec (T.copy title) (T.copy label) (T.copy initial) (T.copy submit))
      InputsFormSpec title fields submit
        | null fields || length (take 7 fields)>6 || any invalidField fields
          || S.size (S.fromList (map inputId fields))/=length fields->invalid
        | otherwise->checked (InputsFormSpec (T.copy title)
            [InputField (T.copy ident) (T.copy label) (T.copy initial) | InputField ident label initial<-fields] (T.copy submit))
      ChoiceFormSpec title label choices initial submit
        | invalidLabel label || null choices || length (take 513 choices)>512 || any invalidChoice choices
          || S.size (S.fromList (map fst choices))/=length choices || initial `notElem` map fst choices->invalid
        | otherwise->checked (ChoiceFormSpec (T.copy title) (T.copy label)
            [(T.copy ident,T.copy name) | (ident,name)<-choices] (T.copy initial) (T.copy submit))
  where
    invalid=pure (Left (InvalidArguments "Invalid form metadata."))
    control c=c<' ' || c=='\DEL'
    invalidLabel value=T.null value || T.length value>256 || T.any control value
    invalidInput value=T.length value>8192 || T.any control value
    invalidField (InputField ident label initial)=T.null ident || T.length ident>128 || T.any control ident || invalidLabel label || invalidInput initial
    invalidChoice (ident,label)=T.null ident || T.length ident>4096 || invalidLabel label
    checked value=do
      _<-evaluate (sum (map T.length ([formTitle value,formSubmit value]++case value of
        ConfirmationFormSpec _ label _->[label]
        InputFormSpec _ label initial _->[label,initial]
        InputsFormSpec _ fields _->concatMap (\(InputField ident label initial)->[ident,label,initial]) fields
        ChoiceFormSpec _ label choices initial _->label:initial:concatMap (\(ident,name)->[ident,name]) choices)))
      pure (Right value)

catalogue :: FormSpec -> Catalogue
catalogue ConfirmationFormSpec{}=ConfirmationCatalogue
catalogue InputFormSpec{}=InputCatalogue
catalogue (InputsFormSpec _ fields _)=InputsCatalogue (map inputId fields)
catalogue (ChoiceFormSpec _ _ choices _ _)=ChoiceCatalogue (S.fromList (map fst choices))
prepareChoices :: FormSpec -> IO (V.Vector (Text,Text),M.Map Text Int)
prepareChoices spec=do
  let entries=case spec of ChoiceFormSpec _ _ choices _ _->choices; _->[]
      vector=V.fromList entries
      index=M.fromList (zip (map fst entries) [0..])
  _<-evaluate (V.length vector+M.size index)
  pure (vector,index)
matchesAction :: FormSpec -> FormAction c r -> Bool
matchesAction spec (FormAction named _ _ _ _)=named==case spec of InputsFormSpec{}->True; _->False
-- | Mint a fresh pending lifetime with immutable disclosure and captured IDs on the calling worker. Closing/reopening can
-- never reuse the old reference. Registration retirement prevents admission.
prepareForm :: FormDisclosure -> FormSpec -> FormAction c r -> IO (Either CommandError (PreparedForm c r))
prepareForm disclosure spec action=do
  checked<-checkedSpec spec
  case checked of
    Left err->pure (Left err)
    Right value | not (matchesAction value action)->pure (Left (InvalidArguments "Form spec and action value shapes differ."))
    Right value->do
      let kind=catalogue value
      _<-evaluate kind
      ref<-FormRef <$> newUnique <*> newTVarIO (1,Pending) <*> pure kind <*> pure disclosure
      (choices,index)<-prepareChoices value
      pure (Right (PreparedForm ref 1 True value choices index action))
-- | Metadata-only publication; never carries or changes the action.
data FormUpdate = FormUpdate !FormRef !Integer !FormSpec !(V.Vector (Text,Text)) !(M.Map Text Int)
-- | Exact lifetime of a metadata-only publication; no command or callback.
updateFormReference :: FormUpdate -> FormRef
updateFormReference (FormUpdate reference _ _ _ _)=reference
-- | Issue later metadata for the same action. Only latest metadata may adopt;
-- labels never revoke a live submission or overwrite installed widget state.
refreshForm :: FormRef -> FormSpec -> IO (Either CommandError (Maybe FormUpdate))
refreshForm ref@(FormRef _ state kind _) spec=do
  checked<-checkedSpec spec
  case checked of
    Left err->pure (Left err)
    Right value | catalogue value/=kind->pure (Left (InvalidArguments "Form refresh must retain its widget and ordered input IDs or choice IDs."))
    Right value->do
      (choices,index)<-prepareChoices value
      atomically $ do
        (revision,phase)<-readTVar state
        if phase/=Open then pure (Right Nothing) else do
          let next=revision+1
          writeTVar state (next,phase)
          pure (Right (Just (FormUpdate ref next value choices index)))
-- | Host metadata adoption for an already installed exact live form. The stored
-- action is unchanged. Current host draft, selection and focus remain separate.
admitFormRefresh :: PreparedForm c r -> FormUpdate -> IO (Maybe (PreparedForm c r))
admitFormRefresh original@(PreparedForm ref@(FormRef _ state _ _) _ _ _ _ _ action) (FormUpdate target version spec choices index)=do
  live<-formCurrent original
  (revision,phase)<-readTVarIO state
  pure (if live && ref==target && phase==Open && revision==version then Just (PreparedForm ref version False spec choices index action) else Nothing)
-- | Host scalar lifetime check, including the existing command registration.
formCurrent :: PreparedForm c r -> IO Bool
formCurrent (PreparedForm (FormRef _ state _ _) _ _ _ _ _ (FormAction _ registry command _ _))=do
  live<-commandCurrent registry (commandRef command)
  (_,phase)<-readTVarIO state
  pure (live && phase/=Retired)
-- | Host admission after current actor/modal checks. A refresh cannot open a
-- closed form; an opening reply can be adopted once only.
admitForm :: Bool -> PreparedForm c r -> IO Bool
admitForm present form@(PreparedForm (FormRef _ state _ _) version opening _ _ _ _)=do
  live<-formCurrent form
  if not live then pure False else atomically $ do
    (revision,phase)<-readTVar state
    if revision/=version || not (phase==Pending && opening && not present || phase==Open && present)
      then pure False else writeTVar state (revision,Open) >> pure True
-- | Claim exactly one human submission after host ownership validation. Metadata
-- revisions are deliberately irrelevant: the captured action is unchanged.
claimFormSubmission :: PreparedForm c r -> IO Bool
claimFormSubmission form=do
  live<-formCurrent form
  let FormRef _ state _ _=formReference form
  if not live then pure False else atomically $ do
    (revision,phase)<-readTVar state
    if phase/=Open then pure False else writeTVar state (revision,Submitted) >> pure True
-- | An accepted submission survives closing its own modal, until explicitly
-- retired, superseded, or consumed. It never survives registration shutdown.
submissionCurrent :: PreparedForm c r -> IO Bool
submissionCurrent form=do
  live<-formCurrent form
  let FormRef _ state _ _=formReference form
  (_,phase)<-readTVarIO state
  pure (live && phase==Submitted)
-- | Invoke the typed action on the existing reply worker. Results must be forced
-- by their owning adapter before publication; no adapter runs in a host commit.
invokeFormAction :: PreparedForm c r -> c -> FormValue -> IO (Either CommandError r)
invokeFormAction (PreparedForm _ _ _ spec _ choices (FormAction _ registry command arguments prepare)) context value
  | not valid=pure (Left (InvalidArguments "Invalid form value."))
  | otherwise=case arguments value of
      Left err->pure (Left err)
      Right captured->do
        result<-invoke registry command context captured
        case result of Left err->pure (Left err); Right reply->Right <$> prepare context reply
  where
    input text=T.length text<=8192 && not (T.any (\c->c<' ' || c=='\DEL') text)
    valid=case (spec,value) of
      (ConfirmationFormSpec{},TextValue text)->T.null text
      (InputFormSpec{},TextValue text)->input text
      (ChoiceFormSpec{},TextValue text)->M.member text choices
      (InputsFormSpec _ fields _,InputValues values)->
        M.keysSet values==S.fromList (map inputId fields) && all input (M.elems values)
      _->False
-- | Atomically consume the submission once before applying its checked result.
finishFormSubmission :: FormRef -> IO Bool
finishFormSubmission (FormRef _ state _ _)=atomically $ do
  (revision,phase)<-readTVar state
  if phase/=Submitted then pure False else writeTVar state (revision,Retired) >> pure True
-- | Idempotent close/reopen invalidation. No callbacks or worker joins occur.
retireForm :: FormRef -> IO ()
retireForm (FormRef _ state _ _)=atomically (modifyTVar' state (\(revision,_)->(revision,Retired)))
