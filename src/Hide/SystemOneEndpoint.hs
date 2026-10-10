{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.SystemOneEndpoint
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings, ScopedTypeVariables
--
-- System One HTTP transport and its ordered question codec. Connection policy
-- belongs to the selected supplier scope; only the inference worker performs
-- network IO. Redirects cannot move submitted context to another destination.
module Hide.SystemOneEndpoint
  ( EndpointConfig(..)
  , endpointProvider
  , encodeSystemOneInput
  , decodeSystemOneOutput
  ) where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM (atomically,check)
import Control.Exception (catch)
import Control.Monad (unless,when)
import Data.Aeson
import qualified Data.Aeson.Encoding as E
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser,parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Plugin.SystemOne
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Network.URI (parseURI,URI(..),URIAuth(..))

-- | Explicit human-selected URL, reported model name and optional bearer token.
-- No 'Show' instance: tokens must not become logs or transport diagnostics.
-- The URL names the complete /v1/systemone endpoint, without userinfo, a query
-- or a fragment. Credentials are carried only in the Authorization header.
data EndpointConfig = EndpointConfig
  { endpointURL :: !Text
  , endpointModel :: !Text
  , endpointToken :: !(Maybe BS.ByteString)
  }

-- | Validate configuration without connecting or loading a model. Acquisition
-- creates its manager on the inference worker. Idle connections are not retained:
-- http-client closes each response deterministically rather than relying on
-- garbage collection of the retired manager to release sockets. A remote response
-- reports a model name; it cannot attest to the checkpoint that produced it.
endpointProvider :: EndpointConfig -> Either Text DecisionProvider
endpointProvider config=do
  unless (T.length (endpointURL config)<=4096 && not (T.any (<' ') (endpointURL config))) (Left "Invalid System One endpoint URL")
  case parseURI (T.unpack (endpointURL config)) of
    Just uri | uriScheme uri `elem` ["http:","https:"]
      , Just authority<-uriAuthority uri
      , not (null (uriRegName authority)), null (uriUserInfo authority)
      , null (uriQuery uri), null (uriFragment uri) -> pure ()
    _->Left "System One requires an HTTP(S) URL without credentials, query or fragment"
  unless (not (T.null (endpointModel config)) && T.length (endpointModel config)<=256 && not (T.any (<' ') (endpointModel config))) (Left "Invalid System One model name")
  unless (maybe True (\token->not (BS.null token) && BS.length token<=4096 && BS.all (\c->c>=33 && c<=126) token) (endpointToken config)) (Left "Invalid System One bearer token")
  pure DecisionProvider
    { decisionProviderDescription=SupplierDescription
        "System One" (SystemOneEndpoint (endpointURL config))
        (ReportedModel (endpointModel config)) Nothing 0.00005
    , withDecisionDriver= \retired action->do
        manager<-HTTP.newManager (HTTP.managerSetProxy HTTP.noProxy tlsManagerSettings {HTTP.managerIdleConnectionCount=0})
        action (DecisionDriver (invoke manager retired))
    }
  where
    invoke manager retired input cancelled=do
      stopped<-atomically ((||) <$> retired <*> cancelled)
      if stopped then pure (Left DecisionCancelled) else do
        result<-race (atomically (((||) <$> retired <*> cancelled) >>= check))
          (send manager config input `catch` (\(_::HTTP.HttpException)->pure (Left (DecisionProviderFailed "System One request failed"))))
        pure (either (const (Left DecisionCancelled)) id result)

send :: HTTP.Manager -> EndpointConfig -> DecisionInput -> IO (Either DecisionFailure DecisionOutput)
send manager config input=do
  base<-HTTP.parseRequest (T.unpack (endpointURL config))
  let request=base
        { HTTP.method="POST"
        , HTTP.redirectCount=0
        , HTTP.requestBody=HTTP.RequestBodyLBS (encodeSystemOneInput (endpointModel config) input)
        , HTTP.requestHeaders=[("Content-Type","application/json"),("Accept","application/json")]
          ++maybe [] (\token->[("Authorization","Bearer "<>token)]) (endpointToken config)
        , HTTP.responseTimeout=HTTP.responseTimeoutMicro 30000000
        , HTTP.checkResponse= \_ _->pure ()
        }
  HTTP.withResponse request manager $ \response->
    if statusCode (HTTP.responseStatus response)/=200
    then pure (Left (DecisionProviderFailed ("System One returned HTTP "<>T.pack (show (statusCode (HTTP.responseStatus response))))))
    else do
      body<-readBounded (HTTP.responseBody response) 1048576 []
      pure (body >>= decodeSystemOneOutput (endpointModel config) input)

readBounded :: HTTP.BodyReader -> Int -> [BS.ByteString] -> IO (Either DecisionFailure BS.ByteString)
readBounded reader remaining chunks=do
  chunk<-HTTP.brRead reader
  if BS.null chunk then pure (Right (BS.concat (reverse chunks)))
  else if BS.length chunk>remaining then pure (Left (DecisionProviderFailed "System One response exceeds 1 MiB"))
  else readBounded reader (remaining-BS.length chunk) (chunk:chunks)

-- | Encode questions and options in submitted order. Object construction uses
-- explicit series rather than a key map. Preserve labels as model input; an
-- endpoint may canonicalize numeric JSON keys, but response decoding restores
-- probabilities to the caller's order without adding words to its criteria.
encodeSystemOneInput :: Text -> DecisionInput -> BL.ByteString
encodeSystemOneInput model input=E.encodingToLazyByteString $ E.pairs
  ("model" .= model <> "state" .= decisionState input <>
   E.pair "questions" (E.pairs (foldMap encodeQuestion (decisionQuestions input))))
  where
    encodeQuestion question=E.pair (K.fromText (questionName question)) $ E.pairs
      ("instructions" .= questionInstructions question <> case questionKind question of
        BinaryDecision no yes->"type" .= ("noul"::Text) <>
          E.pair "criteria" (E.pairs ("false" .= no <> "true" .= yes))
        ChoiceDecision options->"type" .= ("choice"::Text) <>
          E.pair "criteria" (E.pairs (mconcat
            [E.pair (K.fromText (optionLabel option)) (toEncoding (optionCriterion option))
            | option<-options]))
        ScoreDecision levels->"type" .= ("score"::Text) <> "criteria" .= levels)

-- | Decode only the submitted question/option identities. Unknown or missing
-- answers, type changes and reported truncation fail; input text and raw parser
-- exceptions are never included in the diagnostic. Scalar probability validation
-- is repeated at the common host boundary for every provider implementation.
decodeSystemOneOutput :: Text -> DecisionInput -> BS.ByteString -> Either DecisionFailure DecisionOutput
decodeSystemOneOutput model input bytes=case eitherDecodeStrict' bytes >>= parseEither parseOutput of
  Left _->Left (DecisionProviderFailed "Invalid or truncated System One response")
  Right value->Right value
  where
    parseOutput=withObject "System One response" $ \o->do
      actualModel<-o .: "model"
      unless (actualModel==model) (fail "Model identity mismatch")
      truncated<-o .:? "truncated" .!= False
      when truncated (fail "Truncated state")
      answers<-o .: "answers"
      exactKeys (map questionName (decisionQuestions input)) answers
      parsed<-mapM (parseAnswer answers) (decisionQuestions input)
      usage<-o .:? "usage" >>= traverse parseUsage
      pure (DecisionOutput (ReportedModel actualModel) parsed usage)
    parseAnswer answers question=do
      value<-answers .: K.fromText (questionName question)
      withObject "answer" (\o->do
        kind<-o .: "type" :: Parser Text
        confidence<-o .:? "confidence"
        mapM_ probability confidence
        (tag,ps)<-case questionKind question of
          BinaryDecision{}->do
            unless (kind=="noul") (fail "Wrong answer type")
            p<-o .: "noul" >>= probability
            pure (BinaryAnswer,[1-p,p])
          ChoiceDecision options->do
            unless (kind=="choice") (fail "Wrong answer type")
            let keys=map optionLabel options
            selected<-o .: "choice"
            unless (selected `elem` keys) (fail "Unknown selected option")
            ps<-distribution o keys
            pure (ChoiceAnswer,ps)
          ScoreDecision levels->do
            unless (kind=="score") (fail "Wrong answer type")
            score<-o .: "score" :: Parser Double
            unless (finite score && score>=0 && score<=fromIntegral (length levels-1)) (fail "Invalid score")
            let keys=map (T.pack.show) [0..length levels-1]
            legend<-o .: "legend"
            exactKeys keys legend
            descriptions<-mapM (\key->legend .: K.fromText key) keys
            unless (descriptions==levels) (fail "Changed score levels")
            ps<-distribution o keys
            pure (ScoreAnswer,ps)
        pure (DecisionAnswer (questionName question) tag ps confidence)) value
    distribution o keys=do
      ps<-o .: "probabilities"
      exactKeys keys ps
      values<-mapM (\key->ps .: K.fromText key >>= probability) keys
      unless (abs (sum values-1)<=0.00005*fromIntegral (length values)+0.000001) (fail "Invalid probability sum")
      pure values
    parseUsage=withObject "usage" $ \o->do
      inputCount<-o .:? "input_tokens" >>= traverse nonnegative
      outputCount<-o .:? "output_tokens" >>= traverse nonnegative
      stateCount<-o .:? "state_tokens" >>= traverse nonnegative
      usedCount<-o .:? "state_tokens_used" >>= traverse nonnegative
      unless (stateCount==usedCount) (fail "Truncated or inconsistent token usage")
      pure (DecisionUsage inputCount outputCount)
    nonnegative n | n>=0=pure (n::Int)
                  | otherwise=fail "Negative token count"
    probability p | finite p && p>=0 && p<=1=pure p
                  | otherwise=fail "Invalid probability"
    finite x=not (isNaN x || isInfinite x)
    exactKeys keys values=unless (S.fromList keys==S.fromList (map K.toText (KM.keys values))) (fail "Mismatched response keys")
