{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Plugin.AgentServices
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Orchestration capabilities bound by the host to one authenticated actor and
-- workspace. Tool arguments cannot replace either. The host owns ancestry,
-- limits, tickets, provider lifetime and private recovery; every operation
-- rechecks its captured actor rather than treating this record as a live lease.
module Hide.Plugin.AgentServices
  ( AgentServices(..), HistoryEvent(..), HistoryPage(..) ) where

import Data.Aeson (Value,ToJSON(..),object,(.=))
import Data.Text (Text)
import Hide.Plugin.Agent (AgentId,agentIdText,Actor(..),SpawnSpec)

-- | Capabilities for the existing directory/spawn/message/history workflow.
-- Run them on the tool worker after host permission admission. They do not
-- expose provider handles, human approvals or an unrestricted tool dispatcher.
-- A spawn must use 'serviceWorkspace'; a different directory is rejected before
-- reservation. Closing an agent revokes its captured capabilities. A successful
-- spawn includes the initial task ticket and uses the hub's rollback on failure.
-- Waits are bounded and do not cancel the task when their deadline expires.
data AgentServices = AgentServices
  { serviceWorkspace :: !FilePath
  , serviceAgents :: IO (Either Text Value)
  , serviceStatus :: AgentId -> IO (Either Text Value)
  , serviceSpawn :: SpawnSpec -> IO (Either Text (AgentId,Int))
  , serviceRename :: AgentId -> Text -> IO (Either Text ())
  , serviceMessage :: AgentId -> Text -> IO (Either Text Int)
  , serviceWait :: AgentId -> Int -> Int -> IO (Either Text Value)
  , serviceCancel :: AgentId -> IO (Either Text ())
  , serviceEnd :: AgentId -> IO (Either Text ())
  , serviceHistory :: AgentId -> Int -> Int -> IO (Either Text HistoryPage)
  , serviceSearch :: AgentId -> Text -> Int -> Int -> IO (Either Text HistoryPage)
  }

-- | One retained public event. The index increases within its agent and remains
-- unchanged across pagination and recovery. The author is host-attributed; detail
-- is bounded provider/service data whose publisher owns redaction. Private resume
-- state is kept separately from these public events.
-- Event kinds remain extensible for provider activity.
data HistoryEvent = HistoryEvent
  { historyIndex :: !Int, historyKind :: !Text, historyAuthor :: !Actor
  , historyDetail :: !Value } deriving (Eq,Show)

-- | An immutable page from the Hub's retained history, shared by presentation and
-- the MCP adapter. Events are in increasing index order and all exceed the
-- requested offset. The next offset is the last event index, or the requested
-- offset for an empty page. 'historyDropped' counts evicted events, independently
-- of filtering; 'historyHasMore' means matching retained events remain.
--
-- Pages contain at most the requested 1–100 events and 1 MiB of encoded events.
-- Resume with 'historyNextAfter'; reading neither consumes events nor changes
-- message tickets. The JSON instances preserve the MCP/checkpoint representation.
data HistoryPage = HistoryPage
  { historyEvents :: ![HistoryEvent], historyDropped :: !Int
  , historyHasMore :: !Bool, historyNextAfter :: !Int } deriving (Eq,Show)
actorValue :: Actor -> Value
actorValue Human=object ["kind" .= ("human"::Text)]
actorValue (Agent ident)=object ["kind" .= ("agent"::Text),"id" .= agentIdText ident]
instance ToJSON HistoryEvent where
  toJSON (HistoryEvent index kind author value)=object ["index" .= index,"kind" .= kind,"author" .= actorValue author,"detail" .= value]
instance ToJSON HistoryPage where
  toJSON page=object ["events" .= historyEvents page,"dropped" .= historyDropped page,
    "hasMore" .= historyHasMore page,"nextAfter" .= historyNextAfter page]
