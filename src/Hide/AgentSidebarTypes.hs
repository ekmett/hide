-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.AgentSidebarTypes
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Closed human navigation/dialog requests. IDs are captured by the host; these
-- requests carry no desktop, provider key, callback or agent authority.
module Hide.AgentSidebarTypes (AgentSidebarRequest,DirectoryRequest(..),CompletionTarget(..),CompletionSummary(..)) where

import Data.Text (Text)
import Hide.AgentHub (AgentConfigRef)
import Hide.Plugin.AgentDirectory (DirectoryRequest(..))

-- | Conversation owns application and rechecks the exact target. Dialog submit
-- retains the captured ID even if directory rows are renamed or reordered.
type AgentSidebarRequest = DirectoryRequest AgentConfigRef CompletionTarget

-- | Settings incarnation plus optional live ACP session/configuration receipt.
-- This contains no provider key. Only the completion owner can validate it.
data CompletionTarget = CompletionTarget !Int !(Maybe (Int,Int)) deriving (Eq,Show)
-- | Cheap prepared metadata. No transcript, source buffer or choice payload.
data CompletionSummary = CompletionSummary !CompletionTarget !Text deriving (Eq,Show)
