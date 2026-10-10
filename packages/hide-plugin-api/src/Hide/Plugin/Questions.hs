{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Plugin.Questions
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Nonblocking human questions. A request creates a pending question or polls an
-- identified result; neither operation submits an answer or reads a human draft.
-- The host binds the requesting actor and provider lifetime before dispatch.
module Hide.Plugin.Questions
  ( QuestionRequest(..), QuestionServices(..), questionInput, questionOutput ) where

import Control.Monad (unless,when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Plugin.Command

-- | Create one question, or retrieve only the same actor's identified result.
-- Constructing a value grants no authority; the service checks its bounds too.
data QuestionRequest = CreateQuestion !Text ![Text] | ReadQuestion !Int
  deriving (Eq,Show)

-- | Invoke on a worker. Every call uses the existing permission owner and checks
-- the captured requester again. It returns pending immediately after admission;
-- waiting for a human is never part of this call. Scope close and cancellation
-- wake unadmitted requests. Retaining a service cannot bind a replacement agent.
newtype QuestionServices = QuestionServices
  { requestQuestion :: QuestionRequest -> IO (Either CommandError Value) }

-- | Strict wire grammar and bounds, shared by the plugin and host admission.
questionInput :: Codec QuestionRequest
questionInput=Codec schema (either (Left . T.pack) Right . parseEither parse) encodeRequest
  where
    string limit=object ["type" .= ("string"::Text),"minLength" .= (1::Int),"maxLength" .= limit]
    schema=object ["type" .= ("object"::Text),"additionalProperties" .= False,"required" .= ([]::[Text]),
      "properties" .= object ["question" .= string (4096::Int),
        "choices" .= object ["type" .= ("array"::Text),"maxItems" .= (12::Int),"items" .= string (256::Int)],
        "allowMultiple" .= object ["type" .= ("boolean"::Text),"enum" .= [False]],
        "questionId" .= object ["type" .= ("integer"::Text),"minimum" .= (1::Int)]],
      "oneOf" .= [object ["required" .= ["question"::Text],"not" .= object ["required" .= ["questionId"::Text]]],
        object ["required" .= ["questionId"::Text],"maxProperties" .= (1::Int)]]]
    parse=withObject "ask_user" $ \o->case KM.lookup "questionId" o of
      Just _->do
        unless (KM.keys o==["questionId"]) (fail "Retrieve a question with questionId only.")
        ident<-o .: "questionId"
        unless (ident>0) (fail "questionId must be positive.")
        pure (ReadQuestion ident)
      Nothing->do
        unless (all (`elem` ["question","choices","allowMultiple"]) (KM.keys o)) (fail "Unknown question argument.")
        question<-o .: "question"
        choices<-o .:? "choices" .!= []
        multiple<-o .:? "allowMultiple" .!= False
        when multiple (fail "Only single-choice questions are supported; custom text is always available.")
        unless (not (T.null (T.strip question)) && T.length question<=4096 && not (T.any (\c->c<' ' && c `notElem` ['\n','\t']) question))
          (fail "Question must contain 1..4096 characters.")
        unless (length choices<=12 && all (\text->not (T.null (T.strip text)) && T.length text<=256 && not (T.any (\c->c<' ' || c=='\DEL') text)) choices)
          (fail "Supply at most 12 nonempty single-line choices of at most 256 characters.")
        pure (CreateQuestion question choices)
    encodeRequest (CreateQuestion question choices)=object ["question" .= question,"choices" .= choices]
    encodeRequest (ReadQuestion ident)=object ["questionId" .= ident]

-- | Results are host-owned pending/answered/cancelled projections. Pending
-- results contain only identity and status, never a selection or unsent answer.
questionOutput :: Codec Value
questionOutput=Codec (object ["type" .= ("object"::Text)])
  (\value->case value of Object _->Right value; _->Left "Expected a question result.") id
