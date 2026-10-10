{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Plugin.Transcript
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Immutable transcript sources shared by plugins and host preparation workers.
-- Source identity grants no provider, draft or input authority; the host retains
-- admission, private credentials, interaction state and preparation lifetime.
module Hide.Plugin.Transcript
  ( BodyItemId(..)
  , Record(..)
  , RecordContent(..)
  , appendChunk
  , mergeTool
  , AgentHistory(..)
  , HistoryPresenter
  , PrimarySpeaker(..)
  , PrimaryContent(..)
  , PrimaryUpdate(..)
  , PrimaryPresenter
  , ConversationPresenter(..)
  , PrimaryTranscript
  , emptyPrimaryTranscript
  , primaryTranscriptStarted
  , appendPrimaryUpdate
  , primaryTranscriptRecords
  ) where

import Data.Aeson (Value(..),FromJSON,withObject,(.:))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import Hide.Plugin.Agent (Actor)
import Hide.Plugin.AgentServices (HistoryEvent)

-- | Identity allocated by the transcript/event owner, never rendering. It is
-- local to one transcript and survives chunk append and tool updates. Equality
-- identifies an item, not its current content or a callable provider lifetime.
newtype BodyItemId = BodyItemId Int deriving (Eq,Ord,Show)

-- | One immutable item. The owner advances its revision when content changes;
-- consumers retain source identity instead of comparing complete transcripts.
data Record = Record
  { recordId :: !BodyItemId, recordRevision :: !Int, recordContent :: !RecordContent
  } deriving (Eq,Show)

-- | Display data only. Reply roles control presentation and grouping; they do
-- not establish authorship or grant human input authority. Activity retains its
-- merged public value and ordered update history.
data RecordContent = Reply Text Text | Activity Text Value [Value] | Pause Text deriving (Eq,Show)

-- | Append one chunk, merging only with the last reply of the same role. The
-- caller supplies its own next item identity and revision; merging keeps the
-- first contributing item's identity and adopts the supplied revision.
--
-- @appendChunk candidate revision role text [] =
--   [Record candidate revision (Reply role text)]@
appendChunk :: BodyItemId -> Int -> Text -> Text -> [Record] -> [Record]
appendChunk candidate revision role text records = case reverse records of
  Record ident _ (Reply previous body):rest | previous==role -> reverse rest++[Record ident revision (Reply role (body<>text))]
  _ -> records++[Record candidate revision (Reply role text)]

-- | Merge a public tool update by its @toolCallId@, retaining item identity and
-- update order. Non-null fields replace earlier fields; null fields leave them
-- intact. A new tool uses the caller's identity and revision. An update without
-- a textual @toolCallId@ leaves the transcript unchanged.
--
-- @mergeTool candidate revision Null records = records@
mergeTool :: BodyItemId -> Int -> Value -> [Record] -> [Record]
mergeTool candidate revision update records = case field "toolCallId" update :: Maybe Text of
  Nothing -> records
  Just ident ->
    let merge (Record item _ (Activity old (Object previous) history))
          | old==ident, Object new<-update = Record item revision (Activity old (Object (KM.union (KM.filter (/=Null) new) previous)) (history++[update]))
        merge other=other
    in if any (\record -> case recordContent record of Activity old _ _ -> old==ident; _ -> False) records
       then map merge records else records++[Record candidate revision (activity ident update)]
  where
    activity ident value=Activity ident value [value]

-- | Captured public history for one agent. The host supplies a bounded ordered
-- event slice after attribution and redaction, plus display metadata and its
-- captured revision. No provider handle, private session key or draft is retained.
-- Presentation neither consumes these events nor changes their indices.
data AgentHistory = AgentHistory
  { historyName :: !Text
  , historyMetadata :: !Text
  , historyRevision :: !Int
  , historyEvents :: ![HistoryEvent]
  }

-- | Pure presentation of a closed source. The host selects a contribution at
-- session startup and evaluates it on its existing presentation/checkpoint
-- workers, never on an input or render owner. The returned records carry no
-- authority to submit, clear drafts or install host controls.
type HistoryPresenter = AgentHistory -> [Record]

-- | Host-attributed display speaker. A user-seat submission and a peer delivery
-- remain distinct even when that peer is human. These values
-- confer no input or provider authority.
data PrimarySpeaker = UserSpeaker | AssistantSpeaker | PeerSpeaker Actor

-- | One already-redacted primary conversation change. Messages create items;
-- chunks may extend the final matching reply. Tool updates retain their call ID
-- and ordered public update history. The host owns admission and redaction before
-- publication; presentation never reads credentials or submits provider requests.
data PrimaryContent
  = PrimaryMessage PrimarySpeaker Text
  | PrimaryChunk PrimarySpeaker Text
  | PrimaryTool Value
  | PrimaryPlan Value
  | PrimaryFailure Value
  | PrimaryDisconnect Text
  | PrimaryPause Text

-- | Identity and revision allocated by the host at receipt time. Presentation
-- keeps the first item's identity when merging chunks or tool updates and uses
-- this revision for the changed item. Payload fields remain lazy.
data PrimaryUpdate = PrimaryUpdate !BodyItemId !Int !PrimaryContent

-- | Pure incremental presentation of an admitted public update. The callback
-- runs only when a presentation/checkpoint worker resolves its captured source.
-- It must retain unaffected item identities and must not grant input authority.
type PrimaryPresenter = PrimaryUpdate -> [Record] -> [Record]

-- | Both transcript contributions selected together at session startup. Neither
-- callback owns provider, input, approval or presentation-worker lifetime.
data ConversationPresenter = ConversationPresenter
  { primaryTranscript :: PrimaryPresenter
  , agentHistory :: HistoryPresenter
  }

-- | Opaque immutable primary source. Each append shares one deferred callback
-- result; multiple consumers of that source share its records rather than
-- rerunning the presenter. Its payload is deliberately lazy, while the started
-- flag is strict and can be inspected without visiting the record history.
data PrimaryTranscript = PrimaryTranscript [Record] !Bool

-- | /O(1)/. An unstarted source with no records.
--
-- @primaryTranscriptRecords emptyPrimaryTranscript = []@
--
-- @primaryTranscriptStarted emptyPrimaryTranscript = False@
emptyPrimaryTranscript :: PrimaryTranscript
emptyPrimaryTranscript=PrimaryTranscript [] False

-- | /O(1)/. Whether the host has appended an update. This does not inspect the
-- payload or call the presenter, including when that update produces no item.
primaryTranscriptStarted :: PrimaryTranscript -> Bool
primaryTranscriptStarted (PrimaryTranscript _ started)=started

-- | /O(1)/. Capture an update without evaluating its presenter or prior records.
-- The immutable result shares one thunk; resolving any earlier source leaves it
-- unchanged. Successive updates resolve in their original publication order.
--
-- @primaryTranscriptRecords (appendPrimaryUpdate p u s) =
--   p u (primaryTranscriptRecords s)@
--
-- @primaryTranscriptStarted (appendPrimaryUpdate p u s) = True@
appendPrimaryUpdate :: PrimaryPresenter -> PrimaryUpdate -> PrimaryTranscript -> PrimaryTranscript
appendPrimaryUpdate presenter update (PrimaryTranscript records _)=
  PrimaryTranscript (presenter update records) True

-- | Access the shared record root on a presentation/checkpoint worker. Resolving
-- its lazy spine or payload may call the captured presenter and walk history;
-- input, tick and render owners must use source identity or the started flag.
primaryTranscriptRecords :: PrimaryTranscript -> [Record]
primaryTranscriptRecords (PrimaryTranscript records _)=records

field :: FromJSON a => Text -> Value -> Maybe a
field name=parseMaybe (withObject "object" (.: K.fromText name))
