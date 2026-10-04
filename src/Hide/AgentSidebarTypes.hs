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
module Hide.AgentSidebarTypes (AgentSidebarRequest(..)) where

import Data.Text (Text)
import Hide.AgentHub (AgentId)

-- | Conversation owns application and rechecks the exact target. Dialog submit
-- retains the captured ID even if directory rows are renamed or reordered.
data AgentSidebarRequest = ShowAgent !AgentId | NewAgent | RenameAgent !AgentId
  | RenameAgentTo !AgentId !Text | CreateAgent !Text !Text deriving (Eq,Show)
