-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.SystemOne
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Session-owned, single-slot inference over immutable authorized text. One
-- worker retains the selected driver scope; a bounded expiry monitor publishes
-- deadlines independently of inference or receipt observation. Retirement wins
-- a terminal receipt immediately, but never releases a still-draining resource.
module Hide.SystemOne
  ( SystemOne
  , withSystemOne
  , systemOneServices
  , selectDecisionProvider
  ) where

import Control.Concurrent.Async (withAsync,wait)
import Control.Concurrent.STM
import Control.Exception (SomeException,SomeAsyncException,try,evaluate,finally,fromException,throwIO)
import Control.Monad (unless,when,void,foldM)
import qualified Data.ByteString as BS
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTimeNSec)
import Network.URI (parseURI,uriScheme,uriAuthority,uriUserInfo,uriFragment,uriQuery)
import System.Timeout (timeout)
import Hide.Plugin.SystemOne
import Hide.RemoteEndpoint (randomIdentity)

data Selection=Selection !DecisionSupplier !DecisionProvider !(TVar Bool)
data Work=Work
  { workSupplier :: !Text
  , workInput :: !DecisionInput
  , workDeadline :: !Integer
  , workStopped :: !(TVar Bool)
  , workResult :: !(TMVar (Either DecisionFailure DecisionResult))
  }
data Owner=Owner
  { ownerClosed :: !Bool
  , ownerSelected :: !(Maybe Selection)
  , ownerPhysical :: !(Maybe Text)
  , ownerWork :: !(Maybe Work)
  }

-- | Opaque session lifetime. It retains no editor state or permission capability.
newtype SystemOne=SystemOne (TVar Owner)

-- | Scope the sole inference worker and expiry monitor. Shutdown first closes
-- admission and terminalizes its pending receipt, then joins resource release.
-- Invoke the scope outside the UI owner; retained services cannot reopen it.
withSystemOne :: (SystemOne -> IO a) -> IO a
withSystemOne action=do
  runtime<-SystemOne <$> newTVarIO (Owner False Nothing Nothing Nothing)
  withAsync (inference runtime `finally` closeSystemOne runtime) $ \worker->
    withAsync (expiration runtime) $ \timer->
      action runtime `finally` (closeSystemOne runtime >> wait worker >> wait timer)

-- | Capture only this owner. Admission validates/forces bounded immutable inputs
-- on the calling worker. Each ticket observes one immutable terminal result:
-- @awaitDecision ticket ≡ awaitDecision ticket@ after its first transition.
systemOneServices :: SystemOne -> SystemOneServices
systemOneServices runtime@(SystemOne state)=SystemOneServices
  { currentDecisionSupplier=do
      current<-readTVarIO state
      if ownerClosed current then pure Nothing else traverse (evaluate . selectedSupplier) (ownerSelected current)
  , requestDecision=request runtime
  }

-- | Privileged host selection, never a service granted to decision consumers.
-- Publish a fresh incarnation even for an equal description; expire the original
-- ticket and signal retirement atomically. Return without joining the old scope.
-- The new supplier stays busy until old physical ownership has drained.
selectDecisionProvider :: SystemOne -> Maybe DecisionProvider -> IO (Either DecisionFailure (Maybe DecisionSupplier))
selectDecisionProvider (SystemOne state) provider=do
  prepared<-checked $ case provider of
    Nothing->Right ()
    Just value->validateDescription (decisionProviderDescription value)
  case prepared of
    Left failure->pure (Left failure)
    Right ()->do
      selected<-case provider of
        Nothing->pure Nothing
        Just value->do
          ident<-T.pack <$> randomIdentity
          stop<-newTVarIO False
          selection<-evaluate (Selection (DecisionSupplier ident (decisionProviderDescription value)) value stop)
          pure (Just selection)
      advertised<-traverse (evaluate . selectedSupplier) selected
      now<-clock
      atomically $ do
        current<-readTVar state
        if ownerClosed current then pure (Left DecisionClosed) else do
          mapM_ retireSelection (ownerSelected current)
          mapM_ (void . terminate now DecisionExpired) (ownerWork current)
          writeTVar state current {ownerSelected=selected}
          pure (Right advertised)

selectedSupplier :: Selection -> DecisionSupplier
selectedSupplier (Selection supplier _ _)=supplier

retireSelection :: Selection -> STM ()
retireSelection (Selection _ _ stop)=writeTVar stop True

clock :: IO Integer
clock=toInteger <$> getMonotonicTimeNSec

request :: SystemOne -> Text -> DecisionInput -> Int -> IO (Either DecisionFailure DecisionTicket)
request (SystemOne state) supplier input milliseconds=do
  prepared<-checked $ do
    _<-nameText 64 supplier
    unless (milliseconds>=1 && milliseconds<=30000) (Left (DecisionInvalid "Decision deadline must be 1..30000 milliseconds."))
    validateInput input
  case prepared of
    Left failure->pure (Left failure)
    Right ()->do
      frozen<-freezeInput input
      capturedSupplier<-evaluate (T.copy supplier)
      now<-clock
      stop<-newTVarIO False
      result<-newEmptyTMVarIO
      let work=Work capturedSupplier frozen (now+toInteger milliseconds*1000000) stop result
      accepted<-atomically $ do
        current<-readTVar state
        case ownerSelected current of
          _ | ownerClosed current->pure (Left DecisionClosed)
          Nothing->pure (Left DecisionUnavailable)
          Just selection
            | decisionSupplierId (selectedSupplier selection)/=supplier->pure (Left DecisionExpired)
            | decisionLocality frozen==HostProcessOnly && supplierLocation (decisionSupplierDescription (selectedSupplier selection))/=InProcess->pure (Left DecisionUnavailable)
            | Just _<-ownerWork current->pure (Left DecisionBusy)
            | Just physical<-ownerPhysical current,physical/=supplier->pure (Left DecisionBusy)
            | otherwise->writeTVar state current {ownerWork=Just work} >> pure (Right ())
      case accepted of
        Left failure->pure (Left failure)
        Right ()->Right <$> evaluate (ticket work)

ticket :: Work -> DecisionTicket
ticket Work {workDeadline=deadline,workStopped=stop,workResult=result}=DecisionTicket
  { awaitDecision=atomically (readTMVar result)
  , pollDecision=atomically (tryReadTMVar result)
  , cancelDecision=do
      now<-clock
      atomically (terminateReceipt now DecisionCancelled deadline stop result)
  }

-- Expiration is independent of provider progress and client receipt methods.
expiration :: SystemOne -> IO ()
expiration (SystemOne state)=loop
  where
    loop=do
      pending<-atomically $ do
        current<-readTVar state
        if ownerClosed current then pure Nothing else do
          Work {workDeadline=deadline,workStopped=stop,workResult=result}<-maybe retry pure (ownerWork current)
          isEmptyTMVar result >>= check
          pure (Just (deadline,stop,result))
      case pending of
        Nothing->pure ()
        Just (deadline,stop,result)->do
          now<-clock
          completed<-if now>=deadline then pure Nothing
            else timeout (fromInteger ((deadline-now+999) `div` 1000)) (atomically (readTMVar result))
          case completed of
            Nothing->atomically (void (publishReceipt stop result (Left DecisionDeadline)))
            Just _->pure ()
          loop

publish :: Work -> Either DecisionFailure DecisionResult -> STM Bool
publish work=publishReceipt (workStopped work) (workResult work)

publishReceipt :: TVar Bool -> TMVar (Either DecisionFailure DecisionResult) -> Either DecisionFailure DecisionResult -> STM Bool
publishReceipt stop receipt result=do
  won<-tryPutTMVar receipt result
  when won $ case result of Left _->writeTVar stop True; Right _->pure ()
  pure won

terminate :: Integer -> DecisionFailure -> Work -> STM Bool
terminate now failure work=terminateReceipt now failure (workDeadline work) (workStopped work) (workResult work)

terminateReceipt :: Integer -> DecisionFailure -> Integer -> TVar Bool -> TMVar (Either DecisionFailure DecisionResult) -> STM Bool
terminateReceipt now failure deadline stop receipt=do
  if now>=deadline
    then publishReceipt stop receipt (Left DecisionDeadline) >> pure False
    else publishReceipt stop receipt (Left failure)

closeSystemOne :: SystemOne -> IO ()
closeSystemOne (SystemOne state)=do
  now<-clock
  atomically $ do
    current<-readTVar state
    unless (ownerClosed current) $ do
      mapM_ retireSelection (ownerSelected current)
      mapM_ (void . terminate now DecisionClosed) (ownerWork current)
      writeTVar state current {ownerClosed=True,ownerSelected=Nothing}

inference :: SystemOne -> IO ()
inference runtime@(SystemOne state)=loop
  where
    loop=do
      next<-atomically $ do
        current<-readTVar state
        if ownerClosed current then pure Nothing else do
          work<-maybe retry pure (ownerWork current)
          live<-isEmptyTMVar (workResult work)
          case ownerSelected current of
            Just selection | live,decisionSupplierId (selectedSupplier selection)==workSupplier work->do
              writeTVar state current {ownerPhysical=Just (workSupplier work)}
              pure (Just (Right (selection,work)))
            _->writeTVar state current {ownerWork=Nothing} >> pure (Just (Left ()))
      case next of
        Nothing->pure ()
        Just (Left ())->loop
        Just (Right (selection@(Selection supplier provider retired),first))->do
          acquiring<-newTVarIO True
          initialStop<-evaluate (workStopped first)
          let stop=do
                closing<-readTVar retired
                initial<-readTVar acquiring
                cancelled<-readTVar initialStop
                pure (closing || initial && cancelled)
              run=withDecisionDriver provider stop $ \driver->do
                adopted<-atomically $ do
                  closing<-readTVar retired
                  cancelled<-readTVar initialStop
                  if closing || cancelled then pure False
                    else writeTVar acquiring False >> pure True
                when adopted (scoped runtime selection driver)
          outcome<-try run :: IO (Either SomeException ())
          now<-clock
          atomically $ do
            current<-readTVar state
            case ownerWork current of
              Just work | workSupplier work==decisionSupplierId supplier->do
                void (terminate now (DecisionProviderFailed "System-1 provider failed.") work)
                writeTVar state current {ownerWork=Nothing,ownerPhysical=Nothing}
              _->writeTVar state current {ownerPhysical=Nothing}
          case outcome of
            Left exception | Just async<-(fromException exception :: Maybe SomeAsyncException)->throwIO async
            _->loop

scoped :: SystemOne -> Selection -> DecisionDriver -> IO ()
scoped (SystemOne state) (Selection supplier _ retired) driver=loop
  where
    loop=do
      next<-atomically $ do
        current<-readTVar state
        stopped<-readTVar retired
        if ownerClosed current || stopped then pure Nothing else do
          work<-maybe retry pure (ownerWork current)
          if workSupplier work==decisionSupplierId supplier then pure (Just work) else pure Nothing
      case next of
        Nothing->pure ()
        Just work->do
          live<-atomically (isEmptyTMVar (workResult work))
          result<-if not live then pure Nothing else do
            outcome<-try (runDecision driver (workInput work) (readTVar (workStopped work))) :: IO (Either SomeException (Either DecisionFailure DecisionOutput))
            prepared<-case outcome of
              Left exception | Just async<-(fromException exception :: Maybe SomeAsyncException)->throwIO async
                             | otherwise->pure (Left (DecisionProviderFailed "System-1 inference failed."))
              Right (Left failure)->pure (Left (safeFailure failure))
              Right (Right output)->do
                validated<-checked (validateOutput (decisionSupplierDescription supplier) (workInput work) output)
                pure (DecisionResult supplier (decisionStateId (workInput work)) output <$ validated)
            pure (Just prepared)
          now<-clock
          atomically $ do
            current<-readTVar state
            closing<-readTVar retired
            mapM_ (\value->void $ publish work $ if now>=workDeadline work then Left DecisionDeadline
              else if ownerClosed current then Left DecisionClosed
              else if closing then Left DecisionExpired else value) result
            -- Keep the slot through provider-scope cleanup on replacement/close.
            unless (ownerClosed current || closing) $
              writeTVar state current {ownerWork=Nothing}
          loop

safeFailure :: DecisionFailure -> DecisionFailure
safeFailure (DecisionProviderFailed _)=DecisionProviderFailed "System-1 inference failed."
safeFailure (DecisionInvalid _)=DecisionInvalid "Selected supplier rejected the decision input."
safeFailure failure=failure

checked :: Either DecisionFailure () -> IO (Either DecisionFailure ())
checked value=do
  result<-try (evaluate value) :: IO (Either SomeException (Either DecisionFailure ()))
  case result of
    Left exception | Just async<-(fromException exception :: Maybe SomeAsyncException)->throwIO async
                   | otherwise->pure (Left (DecisionInvalid "Invalid decision data."))
    Right answer->pure answer

invalid :: Text -> Either DecisionFailure a
invalid=Left . DecisionInvalid

boundedText :: Int -> Text -> Either DecisionFailure Int
boundedText limit value
  | T.length (T.take (limit+1) value)>limit=invalid "Decision text exceeds its bound."
  | T.any (=='\0') value=invalid "Decision text contains NUL."
  | bytes>limit=invalid "Decision UTF-8 text exceeds its byte bound."
  | otherwise=Right bytes
  where bytes=BS.length (TE.encodeUtf8 value)

nameText :: Int -> Text -> Either DecisionFailure Int
nameText limit value=do
  count<-boundedText limit value
  unless (count>0 && T.all (>= ' ') value) (invalid "Invalid decision name.")
  pure count

boundedCount :: Int -> Int -> [a] -> Either DecisionFailure Int
boundedCount lower upper values=do
  let count=length (take (upper+1) values)
  unless (count>=lower && count<=upper) (invalid "Decision domain count exceeds its bound.")
  pure count

-- Copy only the validated logical text, so a small slice cannot keep an entire
-- source byte array alive. Every copy is forced before admission on this caller.
freezeInput :: DecisionInput -> IO DecisionInput
freezeInput input=do
  questions<-traverse freezeQuestion (decisionQuestions input)
  evaluate (DecisionInput (T.copy (decisionStateId input)) (T.copy (decisionState input)) questions (decisionLocality input))
  where
    freezeQuestion question=do
      kind<-case questionKind question of
        BinaryDecision no yes->evaluate (BinaryDecision (T.copy no) (T.copy yes))
        ChoiceDecision options->ChoiceDecision <$> traverse (\option->evaluate (DecisionOption (T.copy (optionLabel option)) (T.copy (optionCriterion option)))) options
        ScoreDecision levels->ScoreDecision <$> traverse (evaluate . T.copy) levels
      evaluate (DecisionQuestion (T.copy (questionName question)) (T.copy (questionInstructions question)) kind)

validateInput :: DecisionInput -> Either DecisionFailure ()
validateInput input=do
  stateName<-nameText 256 (decisionStateId input)
  stateSize<-boundedText 65536 (decisionState input)
  _<-boundedCount 1 16 (decisionQuestions input)
  (_,total)<-foldM question (S.empty,stateName+stateSize) (decisionQuestions input)
  unless (total<=131072) (invalid "Decision text exceeds 128 KiB UTF-8 bytes.")
  where
    question (names,total) value=do
      label<-nameText 128 (questionName value)
      unless (S.notMember (questionName value) names) (invalid "Duplicate decision question name.")
      let available=131072-total-label
      unless (available>=0) (invalid "Decision text exceeds 128 KiB UTF-8 bytes.")
      instructions<-boundedText available (questionInstructions value)
      let remaining=available-instructions
      domain<-case questionKind value of
        BinaryDecision no yes->do
          noSize<-boundedText remaining no
          yesSize<-boundedText (remaining-noSize) yes
          pure (noSize+yesSize)
        ChoiceDecision options->do
          _<-boundedCount 1 255 options
          (_,size)<-foldM (\(labels,size) option->do
            labelSize<-nameText 256 (optionLabel option)
            unless (S.notMember (optionLabel option) labels) (invalid "Duplicate decision option label.")
            unless (size+labelSize<=remaining) (invalid "Decision text exceeds 128 KiB UTF-8 bytes.")
            criterion<-boundedText (remaining-size-labelSize) (optionCriterion option)
            pure (S.insert (optionLabel option) labels,size+labelSize+criterion)) (S.empty,0) options
          pure size
        ScoreDecision levels->do
          _<-boundedCount 1 255 levels
          foldM (\size level->(size+) <$> boundedText (remaining-size) level) 0 levels
      let next=total+label+instructions+domain
      unless (next<=131072) (invalid "Decision text exceeds 128 KiB UTF-8 bytes.")
      pure (S.insert (questionName value) names,next)

validateDescription :: SupplierDescription -> Either DecisionFailure ()
validateDescription value=do
  _<-nameText 256 (supplierLabel value)
  _<-nameText 256 (case supplierModel value of PinnedArtifact model->model;ReportedModel model->model)
  case supplierLocation value of
    InProcess->pure ()
    BrowserWorker label->void (nameText 256 label)
    SystemOneEndpoint endpoint->do
      _<-nameText 4096 endpoint
      case parseURI (T.unpack endpoint) of
        Just uri | uriScheme uri `elem` ["http:","https:"],Just authority<-uriAuthority uri,null (uriUserInfo authority),null (uriQuery uri),null (uriFragment uri)->pure ()
        _->invalid "Invalid public System-1 endpoint URL."
  case supplierAllocationLimit value of Just 0->invalid "Invalid supplier allocation limit.";_->pure ()
  let allowance=supplierProbabilityError value
  unless (finite allowance && allowance>=0 && allowance<=0.00005) (invalid "Invalid supplier probability rounding allowance.")

validateOutput :: SupplierDescription -> DecisionInput -> DecisionOutput -> Either DecisionFailure ()
validateOutput description input output=do
  unless (outputModel output==supplierModel description) (invalid "Decision model identity does not match the selected supplier.")
  count<-boundedCount 1 16 (outputAnswers output)
  unless (count==length (decisionQuestions input)) (invalid "Decision answers do not match the submitted questions.")
  mapM_ answer (zip (decisionQuestions input) (outputAnswers output))
  case outputUsage output of
    Nothing->pure ()
    Just usage->unless (all (maybe True (>=0)) [decisionInputTokens usage,decisionOutputTokens usage]) (invalid "Invalid decision usage.")
  where
    answer (question,value)=do
      let (kind,dimension)=case questionKind question of
            BinaryDecision _ _->(BinaryAnswer,2)
            ChoiceDecision options->(ChoiceAnswer,length options)
            ScoreDecision levels->(ScoreAnswer,length levels)
      unless (answerQuestion value==questionName question && answerKind value==kind) (invalid "Decision answer identity or kind does not match.")
      count<-boundedCount dimension dimension (answerProbabilities value)
      unless (all probability (answerProbabilities value)) (invalid "Invalid decision probability.")
      let tolerance=0.000001+fromIntegral count*supplierProbabilityError description
      unless (abs (sum (answerProbabilities value)-1)<=tolerance) (invalid "Decision probabilities do not total one within the declared allowance.")
      unless (maybe True probability (answerConfidence value)) (invalid "Invalid decision confidence.")

finite :: Double -> Bool
finite value=not (isNaN value || isInfinite value)

probability :: Double -> Bool
probability value=finite value && value>=0 && value<=1
