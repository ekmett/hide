-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.ConversationSessionTypes
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Host-private session receipt. Small immutable identities bind the running
-- owner, provider launch, connection, session and advertised configuration;
-- no desktop, buffer or provider callback crosses the plugin boundary.
module Hide.ConversationSessionTypes (ConversationSessionReceipt(..)) where

import Data.Text (Text)
import Data.Unique (Unique)
import System.Mem.StableName (StableName)
import qualified Hide.ACP as A
import Hide.AgentHub (AgentId,AgentConfigRef)

data ConversationSessionReceipt = ConversationSessionReceipt
  !Unique !AgentId !(StableName A.Launch) !(Maybe (StableName A.Client))
  !(Maybe Text) !(Maybe AgentConfigRef) deriving Eq

-- Provider-private keys and launch configuration must never appear in status.
instance Show ConversationSessionReceipt where
  show _="ConversationSessionReceipt"
