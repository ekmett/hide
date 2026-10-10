{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.SystemOneBrowser
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Data-only inference over one explicitly selected, physical browser attachment.
-- This is a transport adapter, not a scheduler: 'Hide.SystemOne' owns admission,
-- deadlines and selection. A result does not release browser resource ownership;
-- the exact release acknowledgement or actual attachment retirement does.
module Hide.SystemOneBrowser
  ( SystemOneBrowser
  , BrowserAttachment
  , BrowserOffer
  , withSystemOneBrowser
  , attachBrowser
  , browserAttachmentId
  , setBrowserViewer
  , receiveBrowserControl
  , retireBrowserAttachment
  , captureBrowserOffer
  , browserOfferId
  , browserOfferViewer
  , browserOfferLabel
  , browserProvider
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException,SomeAsyncException,try,evaluate,finally,mask,onException,uninterruptibleMask_,fromException,throwIO)
import Control.Monad (unless,when,void,foldM)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser,Pair,parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Scientific (toBoundedInteger)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word64)
import Hide.Plugin.SystemOne
import Hide.RemoteEndpoint (randomIdentity)

data Registry=Registry !Bool !(Maybe BrowserAttachment)
newtype SystemOneBrowser=SystemOneBrowser (TVar Registry)

data BrowserAttachment=BrowserAttachment
  { attachmentIdentity :: !Text
  , attachmentSend :: Value -> IO Bool
  , attachmentState :: !(TVar Attachment)
  }
data Attachment=Attachment
  { attachmentClosed :: !Bool
  , attachmentViewer :: !(Maybe Text)
  , attachmentHighwater :: !Int
  , attachmentOffer :: !(Maybe BrowserOffer)
  , attachmentAdapter :: !(Maybe Adapter)
  , attachmentPending :: !(Maybe Pending)
  }

-- No model/context is retained in an offer, nor does advertising it select it.
data BrowserOffer=BrowserOffer !BrowserAttachment !Text !Int !Text !Text !Word64
data Adapter=Adapter !Text !(TVar Bool) !(TVar Bool) !(TVar Bool) !(TMVar ())
data RequestKey=RequestKey !Text !Text !Int !Text !Text deriving Eq
data Expected=Expected !Text !DecisionAnswerKind !Int
data Pending=Pending
  { pendingKey :: !RequestKey
  , pendingExpected :: ![Expected]
  , pendingModel :: !Text
  , pendingOutcome :: !(TMVar (Either DecisionFailure DecisionOutput))
  , pendingReleased :: !(TVar Bool)
  , pendingCancelled :: !(TVar Bool)
  , pendingCancelSent :: !(TVar Bool)
  , pendingDelivery :: !(TMVar ())
  }

-- | Scope the offer registry inside the common inference owner. Closing retires
-- attachments before that owner's join, so local waiters cannot outlive the
-- transport. This does not claim that a disconnected peer's GPU has stopped.
withSystemOneBrowser :: (SystemOneBrowser -> IO a) -> IO a
withSystemOneBrowser action=do
  state<-newTVarIO (Registry False Nothing)
  action (SystemOneBrowser state) `finally` atomically (do
    Registry _ current<-readTVar state
    mapM_ (retireAttachment DecisionClosed) current
    writeTVar state (Registry True Nothing))

-- | Create a fresh connection nonce for one physical transport stamp. The send
-- callback must never block; False means the control was never admitted/queued.
-- Its transport must recheck that stamp at delivery and discard controls on
-- reconnect/handoff. There is no replay. Attaching retires the prior attachment.
attachBrowser :: SystemOneBrowser -> Int -> (Value -> IO Bool) -> IO BrowserAttachment
attachBrowser (SystemOneBrowser registry) stamp send=do
  -- Force this scalar before the callback can escape with a lazy owner selector.
  transportStamp<-evaluate stamp
  ident<-identity
  state<-newTVarIO (Attachment False Nothing 0 Nothing Nothing Nothing)
  let attachment=BrowserAttachment ident (\value->transportStamp `seq` send value) state
  atomically $ do
    Registry closed previous<-readTVar registry
    mapM_ (retireAttachment DecisionExpired) previous
    if closed then retireAttachment DecisionClosed attachment
      else writeTVar registry (Registry False (Just attachment))
  pure attachment

-- | The non-secret, fresh physical connection identity sent in the connection
-- control. @browserAttachmentId a ≡ browserAttachmentId a@ throughout its scope.
browserAttachmentId :: BrowserAttachment -> Text
browserAttachmentId=attachmentIdentity

-- | Trusted gateway operation, never an incoming browser control. Bind a fresh
-- root-minted viewer for the actual browser generation; Nothing detaches it.
-- Same-current binding is idempotent. The gateway must never reuse a retired
-- viewer, and must filter its messages before forwarding them. Each viewer has
-- its own monotone offer revision; this operation retires all prior receipts.
setBrowserViewer :: BrowserAttachment -> Maybe Text -> IO (Either DecisionFailure ())
setBrowserViewer attachment viewer
  | maybe False (not . validIdentity) viewer=pure (invalid "Invalid browser viewer identity.")
  | otherwise=do
      copied<-traverse (evaluate . T.copy) viewer
      atomically $ do
        current<-readTVar (attachmentState attachment)
        if attachmentClosed current then pure (Left DecisionClosed)
        else if attachmentViewer current==copied then pure (Right ()) else do
          expireResources DecisionExpired current
          writeTVar (attachmentState attachment) current
            {attachmentViewer=copied,attachmentHighwater=0,attachmentOffer=Nothing,attachmentAdapter=Nothing,attachmentPending=Nothing}
          pure (Right ())

-- | Retire this exact physical attachment. Idempotent; retained offers, drivers
-- and controls refuse afterwards. Terminal local retirement never implies peer
-- compute completion, and no request can be replayed on a replacement transport.
retireBrowserAttachment :: BrowserAttachment -> IO ()
retireBrowserAttachment=atomically . retireAttachment DecisionExpired

retireAttachment :: DecisionFailure -> BrowserAttachment -> STM ()
retireAttachment failure attachment=do
  current<-readTVar (attachmentState attachment)
  unless (attachmentClosed current) $ do
    expireResources failure current
    writeTVar (attachmentState attachment) current
      {attachmentClosed=True,attachmentViewer=Nothing,attachmentOffer=Nothing,attachmentAdapter=Nothing,attachmentPending=Nothing}

expireResources :: DecisionFailure -> Attachment -> STM ()
expireResources failure current=do
  mapM_ (\(Adapter _ closed _ _ retired)->writeTVar closed True >> void (tryPutTMVar retired ())) (attachmentAdapter current)
  mapM_ (\pending->do
    released<-readTVar (pendingReleased pending)
    unless released $ do
      cancelled<-readTVar (pendingCancelled pending)
      -- A prepared browser result is provisional until its exact release.
      -- Retirement replaces it, but never an already terminal release/cancel.
      void (tryTakeTMVar (pendingOutcome pending))
      putTMVar (pendingOutcome pending) (Left (if cancelled then DecisionCancelled else failure))
    writeTVar (pendingReleased pending) True) (attachmentPending current)

-- | Capture the immutable current offer, without selecting or loading it.
captureBrowserOffer :: SystemOneBrowser -> IO (Maybe BrowserOffer)
captureBrowserOffer (SystemOneBrowser registry)=atomically $ do
  Registry closed current<-readTVar registry
  if closed then pure Nothing else case current of
    Nothing->pure Nothing
    Just attachment->attachmentOffer <$> readTVar (attachmentState attachment)

-- | Positive safe-integer revision, monotone within the captured viewer.
browserOfferId :: BrowserOffer -> Int
browserOfferId (BrowserOffer _ _ revision _ _ _)=revision

-- | Exact root-minted viewer generation, not editor authority.
browserOfferViewer :: BrowserOffer -> Text
browserOfferViewer (BrowserOffer _ viewer _ _ _ _)=viewer

-- | Bounded public display label.
browserOfferLabel :: BrowserOffer -> Text
browserOfferLabel (BrowserOffer _ _ _ label _ _)=label

offerCurrent :: BrowserOffer -> Attachment -> Bool
offerCurrent (BrowserOffer _ viewer revision _ _ _) current=
  not (attachmentClosed current) && attachmentViewer current==Just viewer &&
    maybe False (\offer->browserOfferViewer offer==viewer && browserOfferId offer==revision) (attachmentOffer current)

-- | Privileged human selection prepares an adapter incarnation, without sending
-- context or acquiring the model. Common owner selection still creates its own
-- public supplier receipt. Provenance describes the declared pinned browser
-- bundle, not remote attestation; its allocation limit meters owned GPU buffers.
browserProvider :: BrowserOffer -> IO (Either DecisionFailure DecisionProvider)
browserProvider offer@(BrowserOffer attachment _ _ label manifest allocation)=do
  live<-offerCurrent offer <$> readTVarIO (attachmentState attachment)
  if not live then pure (Left DecisionExpired) else
    pure (Right DecisionProvider
      { decisionProviderDescription=SupplierDescription label (BrowserWorker label) (PinnedArtifact manifest) (Just allocation) 0.00005
      , withDecisionDriver= \stopped action->mask $ \restore->do
          supplier<-identity
          closed<-newTVarIO False
          used<-newTVarIO False
          retiring<-newTVarIO False
          retired<-newEmptyTMVarIO
          let adapter=Adapter supplier closed used retiring retired
          claimed<-atomically $ do
            current<-readTVar (attachmentState attachment)
            stop<-stopped
            if stop || not (offerCurrent offer current) || maybe False (const True) (attachmentAdapter current)
              then pure False
              else writeTVar (attachmentState attachment) current {attachmentAdapter=Just adapter} >> pure True
          if not claimed then restore (action (DecisionDriver (\_ _->pure (Left DecisionExpired))))
            else restore (action (DecisionDriver (invoke offer adapter stopped)))
              `finally` releaseAdapter offer adapter
      })

adapterIdentity :: Adapter -> Text
adapterIdentity (Adapter ident _ _ _ _)=ident

sameAdapter :: Adapter -> Attachment -> Bool
sameAdapter adapter current=maybe False ((==adapterIdentity adapter) . adapterIdentity) (attachmentAdapter current)

releaseAdapter :: BrowserOffer -> Adapter -> IO ()
releaseAdapter offer@(BrowserOffer attachment viewer revision _ _ _) adapter@(Adapter supplier closed used retiring retired)=uninterruptibleMask_ $ do
  pending<-atomically $ do
    writeTVar closed True
    current<-readTVar (attachmentState attachment)
    pure (if sameAdapter adapter current then attachmentPending current else Nothing)
  mapM_ (\request->cancelPending attachment request >> atomically (readTVar (pendingReleased request) >>= check)) pending
  send<-atomically $ do
    current<-readTVar (attachmentState attachment)
    wasUsed<-readTVar used
    if not (offerCurrent offer current && sameAdapter adapter current) || not wasUsed
      then void (tryPutTMVar retired ()) >> pure False
      else writeTVar retiring True >> pure True
  when send $ void (safeSend attachment (object
    ["type" .= String "system-one-retire","connection" .= attachmentIdentity attachment,"viewer" .= viewer,"offer" .= revision,"supplier" .= supplier]))
  -- A failed enqueue while the browser is still live cannot certify release.
  atomically (readTMVar retired)
  atomically $ do
    current<-readTVar (attachmentState attachment)
    when (sameAdapter adapter current) $
      writeTVar (attachmentState attachment) current {attachmentAdapter=Nothing}

invoke :: BrowserOffer -> Adapter -> STM Bool -> DecisionInput -> STM Bool -> IO (Either DecisionFailure DecisionOutput)
invoke offer@(BrowserOffer attachment viewer revision _ manifest _) adapter@(Adapter supplier closed used _ _) retired input cancelled=mask $ \restore->do
  let stopped=(||) <$> ((||) <$> retired <*> cancelled) <*> readTVar closed
  live<-atomically $ do
    current<-readTVar (attachmentState attachment)
    expired<-readTVar closed
    stop<-stopped
    pure (if expired || not (offerCurrent offer current && sameAdapter adapter current) then Left DecisionExpired
      else if stop then Left DecisionCancelled else Right ())
  validation<-case live of Left failure->pure (Left failure);Right ()->evaluate (validateInput input)
  case validation of
    Left failure->pure (Left failure)
    Right preparedExpected->do
      expected<-traverse evaluate preparedExpected
      request<-identity
      let key=RequestKey (attachmentIdentity attachment) viewer revision supplier request
          control=requestControl key input
      size<-evaluate (BL.length (encode control))
      if size>262144 then pure (invalid "Browser decision control exceeds 256 KiB.") else do
        outcome<-newEmptyTMVarIO
        released<-newTVarIO False
        stoppedRequest<-newTVarIO False
        cancelSent<-newTVarIO False
        delivery<-newEmptyTMVarIO
        let pending=Pending key expected manifest outcome released stoppedRequest cancelSent delivery
        admitted<-atomically $ do
          current<-readTVar (attachmentState attachment)
          expired<-readTVar closed
          stop<-stopped
          if expired || not (offerCurrent offer current && sameAdapter adapter current) then pure (Left DecisionExpired)
          else if stop then pure (Left DecisionCancelled)
          else if maybe False (const True) (attachmentPending current) then pure (Left DecisionBusy)
          else writeTVar (attachmentState attachment) current {attachmentPending=Just pending} >> pure (Right ())
        case admitted of
          Left failure->pure (Left failure)
          Right ()->do
            sent<-safeSend attachment control
            atomically $ do
              case sent of
                Right False->do
                  void (tryPutTMVar outcome (Left DecisionUnavailable))
                  writeTVar released True
                _->writeTVar used True
              putTMVar delivery ()
            case sent of
              Right False->do
                result<-awaitPending attachment pending stopped
                atomically (clearPending attachment pending)
                pure result
              Left exception->do
                uninterruptibleMask_ $ do
                  cancelPending attachment pending
                  atomically (readTVar released >>= check)
                atomically (clearPending attachment pending)
                case fromException exception :: Maybe SomeAsyncException of
                  Just async->throwIO async
                  Nothing->pure (Left (DecisionProviderFailed "Browser control delivery failed."))
              Right True->do
                let drain=cancelPending attachment pending >> atomically (readTVar released >>= check)
                result<-restore (awaitPending attachment pending stopped)
                  `onException` uninterruptibleMask_ drain
                atomically (clearPending attachment pending)
                pure result

awaitPending :: BrowserAttachment -> Pending -> STM Bool -> IO (Either DecisionFailure DecisionOutput)
awaitPending attachment pending stopped=do
  let terminal=do
        readTVar (pendingReleased pending) >>= check
        wasCancelled<-readTVar (pendingCancelled pending)
        if wasCancelled then pure (Left DecisionCancelled) else readTMVar (pendingOutcome pending)
  next<-atomically $ do
    released<-readTVar (pendingReleased pending)
    -- The aggregate stop predicate also includes attachment retirement. Once
    -- released, only the recorded terminal result/cancel claim decides why.
    if released then Just <$> terminal
    else do
      cancel<-stopped
      if cancel then pure Nothing else retry
  case next of
    Just result->pure result
    Nothing->do
      cancelPending attachment pending
      atomically terminal

cancelPending :: BrowserAttachment -> Pending -> IO ()
cancelPending attachment pending=do
  send<-atomically $ do
    readTMVar (pendingDelivery pending)
    released<-readTVar (pendingReleased pending)
    sent<-readTVar (pendingCancelSent pending)
    if released then pure False else do
      writeTVar (pendingCancelled pending) True
      if sent then pure False else writeTVar (pendingCancelSent pending) True >> pure True
  when send (void (safeSend attachment (keyControl "system-one-cancel" (pendingKey pending))))

clearPending :: BrowserAttachment -> Pending -> STM ()
clearPending attachment pending=do
  current<-readTVar (attachmentState attachment)
  when (maybe False ((==pendingKey pending) . pendingKey) (attachmentPending current)) $
    writeTVar (attachmentState attachment) current {attachmentPending=Nothing}

safeSend :: BrowserAttachment -> Value -> IO (Either SomeException Bool)
safeSend attachment=try . attachmentSend attachment

identity :: IO Text
identity=T.pack <$> randomIdentity

invalid :: Text -> Either DecisionFailure a
invalid=Left . DecisionInvalid

-- Only known, bounded controls survive decoding. Error text is deliberately
-- static: parser diagnostics must never serialize submitted or peer text.
data Incoming
  = Offer !Text !Text !Int !Text !Text !Word64
  | Withdraw !Text !Text !Int
  | Result !RequestKey !(Either DecisionFailure DecisionOutput)
  | Released !RequestKey
  | Retired !Text !Text !Int !Text

-- | Nonblocking receiver operation. Decode/force bounded control data on the
-- transport receiver; the STM update is identity-only. No inference callback or
-- editor action occurs here. Stale controls cannot address a new request.
receiveBrowserControl :: BrowserAttachment -> Value -> IO (Either DecisionFailure ())
receiveBrowserControl attachment value=do
  closed<-attachmentClosed <$> readTVarIO (attachmentState attachment)
  if closed then pure (Left DecisionClosed) else case parseEither incoming value of
    Left _->pure (invalid "Invalid browser decision control.")
    Right message->do
      size<-evaluate (BL.length (encode value))
      if size>262144 then pure (invalid "Browser decision control exceeds 256 KiB.")
        else atomically (receive attachment message)

receive :: BrowserAttachment -> Incoming -> STM (Either DecisionFailure ())
receive attachment message=do
  current<-readTVar (attachmentState attachment)
  let state=attachmentState attachment
      exact connection viewer=not (attachmentClosed current) && connection==attachmentIdentity attachment && attachmentViewer current==Just viewer
      stale=pure (Left DecisionExpired)
      withPending key@(RequestKey connection viewer revision supplier _) operation=
        if not (exact connection viewer) || maybe True (\offer->browserOfferId offer/=revision) (attachmentOffer current)
          || maybe True ((/=supplier) . adapterIdentity) (attachmentAdapter current)
        then stale else case attachmentPending current of
          Just pending | pendingKey pending==key->operation pending
          _->stale
  if attachmentClosed current then pure (Left DecisionClosed) else case message of
    Offer connection viewer revision label manifest allocation
      | not (exact connection viewer)->stale
      | revision==attachmentHighwater current->case attachmentOffer current of
          Just (BrowserOffer _ _ _ oldLabel oldManifest oldAllocation)
            | (label,manifest,allocation)==(oldLabel,oldManifest,oldAllocation)->pure (Right ())
          _->stale
      | revision<attachmentHighwater current->stale
      | otherwise->do
          expireResources DecisionExpired current
          let offer=BrowserOffer attachment viewer revision label manifest allocation
          writeTVar state current {attachmentHighwater=revision,attachmentOffer=Just offer,attachmentAdapter=Nothing,attachmentPending=Nothing}
          pure (Right ())
    Withdraw connection viewer revision
      | exact connection viewer,Just offer<-attachmentOffer current,browserOfferId offer==revision->do
          expireResources DecisionExpired current
          writeTVar state current {attachmentOffer=Nothing,attachmentAdapter=Nothing,attachmentPending=Nothing}
          pure (Right ())
      | otherwise->stale
    Result key output->withPending key $ \pending->do
      released<-readTVar (pendingReleased pending)
      cancelled<-readTVar (pendingCancelled pending)
      if released || cancelled then stale else do
        let checked=output >>= validateOutput pending
        won<-tryPutTMVar (pendingOutcome pending) checked
        pure (if won then Right () else Left DecisionExpired)
    Released key->withPending key $ \pending->do
      wasReleased<-readTVar (pendingReleased pending)
      if wasReleased then stale else do
        void (tryPutTMVar (pendingOutcome pending) (Left (DecisionProviderFailed "Browser released without a result.")))
        writeTVar (pendingReleased pending) True
        pure (Right ())
    Retired connection viewer revision supplier
      | exact connection viewer,Just offer<-attachmentOffer current,browserOfferId offer==revision
      ,Just (Adapter actual _ _ retiring retired)<-attachmentAdapter current,actual==supplier->do
          requested<-readTVar retiring
          if requested then void (tryPutTMVar retired ()) >> pure (Right ()) else stale
      | otherwise->stale

incoming :: Value -> Parser Incoming
incoming=withObject "browser decision control" $ \o->do
  kind<-o .: "type"
  connection<-identityField o "connection"
  viewer<-identityField o "viewer"
  revision<-safeInteger o "offer" 1
  case (kind::Text) of
    "system-one-offer"->do
      fields o ["type","connection","viewer","offer","label","manifestSHA256","allocationLimitBytes","backend"]
      label<-textField o "label" 256 True
      manifest<-hashField o "manifestSHA256"
      allocation<-safeInteger o "allocationLimitBytes" 1
      backend<-o .: "backend"
      unless ((backend::Text)=="webgpu") (fail "backend")
      pure (Offer connection viewer revision label manifest (fromIntegral allocation))
    "system-one-withdraw"->fields o ["type","connection","viewer","offer"] >> pure (Withdraw connection viewer revision)
    "system-one-retired"->do
      fields o ["type","connection","viewer","offer","supplier"]
      Retired connection viewer revision <$> identityField o "supplier"
    "system-one-result"->do
      supplier<-identityField o "supplier"
      request<-identityField o "request"
      let key=RequestKey connection viewer revision supplier request
      output<-case (KM.lookup "output" o,KM.lookup "failure" o) of
        (Just value,Nothing)->fields o ["type","connection","viewer","offer","supplier","request","output"] >> (Right <$> parseOutput value)
        (Nothing,Just (String failure))->do
          fields o ["type","connection","viewer","offer","supplier","request","failure"]
          case failure of
            "invalid"->pure (Left (DecisionInvalid "Browser refused the decision input."))
            "failed"->pure (Left (DecisionProviderFailed "Browser inference failed."))
            _->fail "failure"
        _->fail "result"
      pure (Result key output)
    "system-one-released"->do
      fields o ["type","connection","viewer","offer","supplier","request"]
      supplier<-identityField o "supplier"
      Released . RequestKey connection viewer revision supplier <$> identityField o "request"
    _->fail "control"

fields :: Object -> [Text] -> Parser ()
fields o allowed=unless (all ((`elem` allowed) . K.toText) (KM.keys o)) (fail "fields")

validIdentity :: Text -> Bool
validIdentity value=T.length value==48 && T.all hexadecimal value

hexadecimal :: Char -> Bool
hexadecimal c=c>='0' && c<='9' || c>='a' && c<='f'

identityField :: Object -> K.Key -> Parser Text
identityField o key=do
  value<-o .: key
  unless (validIdentity value) (fail "identity")
  pure (T.copy value)

hashField :: Object -> K.Key -> Parser Text
hashField o key=do
  value<-o .: key
  unless (T.length value==64 && T.all hexadecimal value) (fail "hash")
  pure (T.copy value)

safeInteger :: Object -> K.Key -> Int -> Parser Int
safeInteger o key lower=do
  value<-o .: key
  case toBoundedInteger value of
    Just n | n>=lower && n<=9007199254740991->pure n
    _->fail "integer"

textField :: Object -> K.Key -> Int -> Bool -> Parser Text
textField o key bound name=do
  value<-o .: key
  case boundedText bound name value of
    Left _->fail "text"
    Right _->pure (T.copy value)

parseOutput :: Value -> Parser DecisionOutput
parseOutput=withObject "output" $ \o->do
  fields o ["manifestSHA256","answers","usage"]
  manifest<-hashField o "manifestSHA256"
  values<-o .: "answers"
  answers<-array 1 16 parseAnswer values
  usage<-traverse parseUsage (KM.lookup "usage" o)
  pure (DecisionOutput (PinnedArtifact manifest) answers usage)

parseAnswer :: Value -> Parser DecisionAnswer
parseAnswer=withObject "answer" $ \o->do
  fields o ["question","kind","probabilities","confidence"]
  name<-textField o "question" 128 True
  kindText<-o .: "kind"
  kind<-case (kindText::Text) of "binary"->pure BinaryAnswer;"choice"->pure ChoiceAnswer;"score"->pure ScoreAnswer;_->fail "kind"
  values<-o .: "probabilities"
  probabilities<-array 1 255 probability values
  confidence<-traverse probability (KM.lookup "confidence" o)
  pure (DecisionAnswer name kind probabilities confidence)

probability :: Value -> Parser Double
probability value=do
  number<-parseJSON value
  unless (not (isNaN number || isInfinite number) && number>=0 && number<=1) (fail "probability")
  pure number

parseUsage :: Value -> Parser DecisionUsage
parseUsage=withObject "usage" $ \o->do
  fields o ["inputTokens","outputTokens"]
  let count key=case KM.lookup key o of Nothing->pure Nothing;Just _->Just <$> safeInteger o key 0
  DecisionUsage <$> count "inputTokens" <*> count "outputTokens"

array :: Int -> Int -> (Value -> Parser a) -> Value -> Parser [a]
array lower upper parse=withArray "array" $ \values->do
  unless (V.length values>=lower && V.length values<=upper) (fail "count")
  traverse parse (V.toList values)

validateOutput :: Pending -> DecisionOutput -> Either DecisionFailure DecisionOutput
validateOutput pending output=do
  unless (outputModel output==PinnedArtifact (pendingModel pending) && length (outputAnswers output)==length (pendingExpected pending)) (invalid "Browser result identity differs from the request.")
  mapM_ (\(Expected name kind count,answer)->do
    unless (answerQuestion answer==name && answerKind answer==kind && length (answerProbabilities answer)==count) (invalid "Browser answer differs from the request.")
    unless (abs (sum (answerProbabilities answer)-1)<=0.000001+fromIntegral count*0.00005) (invalid "Browser probabilities do not total one.")) (zip (pendingExpected pending) (outputAnswers output))
  pure output

boundedText :: Int -> Bool -> Text -> Either DecisionFailure Int
boundedText bound name value
  | bound<0 || T.length (T.take (bound+1) value)>bound=invalid "Browser decision text exceeds its bound."
  | T.any (=='\0') value || name && (T.null value || T.any (<' ') value)=invalid "Invalid browser decision text."
  | bytes>bound=invalid "Browser decision UTF-8 text exceeds its bound."
  | otherwise=Right bytes
  where bytes=BS.length (TE.encodeUtf8 value)

validateInput :: DecisionInput -> Either DecisionFailure [Expected]
validateInput input=do
  when (decisionLocality input==HostProcessOnly) (Left DecisionUnavailable)
  ident<-boundedText 256 True (decisionStateId input)
  state<-boundedText 65536 False (decisionState input)
  count 16 (decisionQuestions input)
  (_,_,expected)<-foldM question (S.empty,ident+state,[]) (decisionQuestions input)
  pure (reverse expected)
  where
    count limit values=unless (not (null values) && length (take (limit+1) values)<=limit) (invalid "Browser decision domain exceeds its bound.")
    question (names,total,expected) q=do
      name<-boundedText 128 True (questionName q)
      unless (S.notMember (questionName q) names) (invalid "Duplicate browser question name.")
      instructions<-boundedText (131072-total-name) False (questionInstructions q)
      let remaining=131072-total-name-instructions
      (kind,dimension,domain)<-case questionKind q of
        BinaryDecision no yes->do
          n<-boundedText remaining False no
          y<-boundedText (remaining-n) False yes
          pure (BinaryAnswer,2,n+y)
        ChoiceDecision options->do
          count 255 options
          (_,size)<-foldM (\(labels,size) option->do
            label<-boundedText (min 256 (remaining-size)) True (optionLabel option)
            unless (S.notMember (optionLabel option) labels) (invalid "Duplicate browser option label.")
            criterion<-boundedText (remaining-size-label) False (optionCriterion option)
            pure (S.insert (optionLabel option) labels,size+label+criterion)) (S.empty,0) options
          pure (ChoiceAnswer,length options,size)
        ScoreDecision levels->do
          count 255 levels
          size<-foldM (\size level->(size+) <$> boundedText (remaining-size) False level) 0 levels
          pure (ScoreAnswer,length levels,size)
      let item=Expected (T.copy (questionName q)) kind dimension
      pure (S.insert (questionName q) names,total+name+instructions+domain,item:expected)

keyFields :: RequestKey -> [Pair]
keyFields (RequestKey connection viewer offer supplier request)=
  ["connection" .= connection,"viewer" .= viewer,"offer" .= offer,"supplier" .= supplier,"request" .= request]

keyControl :: Text -> RequestKey -> Value
keyControl kind key=object (("type" .= kind):keyFields key)

requestControl :: RequestKey -> DecisionInput -> Value
requestControl key input=object (("type" .= String "system-one-request"):("input" .= object
  ["stateId" .= decisionStateId input,"state" .= decisionState input,"questions" .= map question (decisionQuestions input)]):keyFields key)
  where
    question q=object (["name" .= questionName q,"instructions" .= questionInstructions q]++case questionKind q of
      BinaryDecision no yes->["kind" .= String "binary","false" .= no,"true" .= yes]
      ChoiceDecision options->["kind" .= String "choice","options" .= map (\option->object ["label" .= optionLabel option,"criterion" .= optionCriterion option]) options]
      ScoreDecision levels->["kind" .= String "score","levels" .= levels])
