{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.AgentTranscript
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- First-party presentation of primary updates and captured public agent history.
-- Provider lifetime, redaction and human input admission remain with their host
-- owners.
module Hide.AgentTranscript
  ( presentConversation
  , presentPrimary
  , presentHistory
  ) where

import Data.Aeson (Value,FromJSON,withObject,(.:),object,(.=))
import qualified Data.Aeson.Key as K
import Data.Aeson.Types (parseMaybe)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Hide.Plugin.Agent as A
import qualified Hide.Plugin.AgentServices as A
import Hide.Plugin.Transcript

-- | Supply both first-party transcript reducers. The host captures the public
-- source and evaluates these pure callbacks on its presentation workers.
presentConversation :: ConversationPresenter
presentConversation=ConversationPresenter presentPrimary presentHistory

-- | Fold one admitted primary update into immutable presentation records.
-- Messages always append; chunks merge only with the last matching role. Tools
-- retain their original call identity and update order through 'mergeTool'.
-- User-seat input displays as @You@, assistant output as @Agent@, and peer
-- deliveries retain @Human@ or @Agent <id>@ attribution. Roles confer no authority.
--
-- @presentPrimary (PrimaryUpdate ident revision (PrimaryChunk speaker text)) rs =
--   appendChunk ident revision (speakerRole speaker) text rs@
presentPrimary :: PrimaryPresenter
presentPrimary (PrimaryUpdate ident revision content) records=case content of
  PrimaryMessage speaker text->append (Reply (speakerRole speaker) text)
  PrimaryChunk speaker text->appendChunk ident revision (speakerRole speaker) text records
  PrimaryTool value->mergeTool ident revision value records
  PrimaryPlan value->append (Activity "Plan" value [value])
  PrimaryFailure value->append (Activity "Request failed" value [value])
  PrimaryDisconnect text->let value=object ["message" .= text] in append (Activity "Connection closed" value [value])
  PrimaryPause text->append (Pause text)
  where append value=records++[Record ident revision value]

speakerRole :: PrimarySpeaker -> Text
speakerRole UserSpeaker="You"
speakerRole AssistantSpeaker="Agent"
speakerRole (PeerSpeaker A.Human)="Human"
speakerRole (PeerSpeaker (A.Agent ident))="Agent "<>A.agentIdText ident

-- | Present metadata followed by the ordered public history. The metadata item
-- has reserved identity @-1@ and the captured metadata revision. Event items use
-- Hub ordinals; merged replies/tools retain their first item identity. Thoughts
-- remain available through public history but produce no transcript item.
--
-- @presentHistory (AgentHistory name metadata revision []) =
--   [Record (BodyItemId (-1)) revision (Pause metadata)]@
presentHistory :: HistoryPresenter
presentHistory (AgentHistory name metadata revision events)=
  Record (BodyItemId (-1)) revision (Pause metadata):List.foldl' (childHistoryRecord name) [] events

childHistoryRecord :: Text -> [Record] -> A.HistoryEvent -> [Record]
childHistoryRecord name records (A.HistoryEvent eventIndex kind author detail)=
  let eventRecord=Record (BodyItemId eventIndex) eventIndex
  in case kind of
    _ | kind `elem` ["message_queued","steered"] ->
      let human=author==A.Human
          who=case author of A.Human->"Human"; A.Agent ident->"Agent "<>A.agentIdText ident
          seat=if field "userSeat" detail==Just True then if human then "human user seat" else "controlling parent" else "peer message"
      in records++[eventRecord (Reply (if human then "You" else "Peer") (who<>" ("<>seat<>")\n\n"<>fromMaybe "" (field "text" detail)))]
    "output" -> appendChunk (BodyItemId eventIndex) eventIndex "Agent" (if lastRole records==Just "Agent" then chunk else name<>"\n\n"<>chunk) records
      where chunk=fromMaybe "" (field "text" detail)
    "thought" -> records -- Thoughts stay in the bounded history API.
    "tool" -> case field "toolCallId" detail :: Maybe Text of
      Just _ -> mergeTool (BodyItemId eventIndex) eventIndex detail records
      Nothing -> records++[eventRecord (Activity ("event-"<>T.pack (show eventIndex)) detail [detail])]
    "message_finished" | field "status" detail/=Just ("completed"::Text) -> records++[eventRecord (Pause (fromMaybe "Stopped" (field "error" detail)))]
    _ -> records
  where
    lastRole xs=case reverse xs of Record _ _ (Reply role _):_->Just role; _->Nothing

field :: FromJSON a => Text -> Value -> Maybe a
field name=parseMaybe (withObject "object" (.: K.fromText name))
