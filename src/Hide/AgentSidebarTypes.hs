{-# LANGUAGE OverloadedStrings #-}
-- | Module      : Hide.AgentSidebarTypes
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Closed human navigation/dialog requests. IDs are captured by the host; these
-- requests carry no desktop, provider key, callback or agent authority.
module Hide.AgentSidebarTypes (AgentSidebarRequest(..),CompletionTarget(..),CompletionSummary(..)) where

import Data.Text (Text)
import Hide.AgentHub (AgentId,AgentConfigRef)

-- | Conversation owns application and rechecks the exact target. Dialog submit
-- retains the captured ID even if directory rows are renamed or reordered.
data AgentSidebarRequest = ShowAgent !AgentId | NewAgent
  | RenameAgentTo !AgentId !Text | CreateAgent !Text !Text
  | ShowAgentConfiguration !AgentConfigRef !Text ![(Text,Text)] !Int | ConfigureAgent !AgentConfigRef !Text !Text
  | ShowCompletion !CompletionTarget | ChooseCompletion !CompletionTarget !Text
  | ConfigureCompletion !CompletionTarget !Text !Text deriving (Eq,Show)

-- | Settings incarnation plus optional live ACP session/configuration receipt.
-- This contains no provider key. Only the completion owner can validate it.
data CompletionTarget = CompletionTarget !Int !(Maybe (Int,Int)) deriving (Eq,Show)
-- | Cheap prepared metadata. No transcript, source buffer or choice payload.
data CompletionSummary = CompletionSummary !CompletionTarget !Text deriving (Eq,Show)
