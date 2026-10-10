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
module Hide.ConversationSessionTypes (ConversationSessionReceipt(..),ConversationAdviceTarget(..)) where

import Data.Text (Text)
import Data.Unique (Unique)
import System.Mem.StableName (StableName)
import Hide.Plugin.Provider (ProviderLaunch,ProviderIdentity)
import Hide.ConversationBody (ConversationCopy)
import Hide.AgentHub (AgentId,AgentConfigRef)
import Hide.Plugin.BufferHost (ContentVersion)
import qualified Hide.Plugin.EditorHost as Editor

data ConversationSessionReceipt = ConversationSessionReceipt
  !Unique !AgentId !(StableName ProviderLaunch) !(Maybe (ProviderIdentity))
  !(Maybe Text) !(Maybe AgentConfigRef)
  | ConversationOperationReceipt !Unique !(StableName ProviderLaunch) !(Maybe ProviderIdentity)
      !(Maybe Text) !FilePath !Text !(Maybe ConversationCopy)
  deriving Eq

-- | Exact empty composer selected by a human menu invocation. These immutable
-- keys retain neither its Buffer/Undo nor a Desktop. Advice preparation has no
-- input submission authority; adoption checks this version before insertion.
data ConversationAdviceTarget = ConversationAdviceTarget
  !Unique !Text !Int !Editor.EditorMount !ContentVersion !AgentConfigRef

-- Provider-private keys and launch configuration must never appear in status.
instance Show ConversationSessionReceipt where
  show _="ConversationSessionReceipt"
