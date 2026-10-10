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
  ) where

import Data.Aeson (Value(..),FromJSON,withObject,(.:))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
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

field :: FromJSON a => Text -> Value -> Maybe a
field name=parseMaybe (withObject "object" (.: K.fromText name))
