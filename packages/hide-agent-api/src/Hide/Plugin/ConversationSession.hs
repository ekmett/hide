-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.ConversationSession
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Human New/Resume requests carry the host's opaque primary/provider receipt.
-- Captured resume input is private form data, never public menu metadata. A
-- request grants no authority: the host rechecks idle state and the original
-- provider/configuration before retiring or starting any provider resources.
module Hide.Plugin.ConversationSession
  ( ConversationTarget(..), ConversationOperationTarget(..), ContextScope(..)
  , ConversationRequest(..)
  ) where

import Data.Text (Text)
import Hide.Plugin.Provider (ProviderLaunch)

-- | Immutable admission input captured at the human menu invocation. New selects
-- Primary; Resume requires the originally selected view to be Primary. The
-- remembered ID seeds only the private form and does not automatically resume.
data ConversationTarget receipt = ConversationTarget
  { conversationReceipt :: !receipt
  , conversationPrimary :: !Bool
  , conversationResumeId :: !Text
  }

-- | The two existing mutable guidance scopes. Paths are resolved by the host.
data ContextScope = GlobalContext | ProjectContext deriving (Eq,Show)

-- | Original human operation target. Provider prefill is private form input,
-- never menu metadata; the opaque receipt binds the original host ownership.
-- The captured configurable flag controls presentation only; adoption rechecks
-- the original owner and idle state.
data ConversationOperationTarget receipt = ConversationOperationTarget
  { operationReceipt :: !receipt, operationProvider :: !ProviderLaunch
  , operationConfigurable :: !Bool }

-- | Closed lifecycle operations; the receipt cannot be supplied through JSON.
-- Resume carries a validated nonempty, trimmed ID. The host owns provider
-- support checks, saved provider/cwd selection, acquisition and recovery.
data ConversationRequest receipt
  = NewConversation !receipt
  | ResumeConversation !receipt !Text
  | OpenConversation !receipt
  | ConfigureConversation !receipt !ProviderLaunch
  | OpenConversationContext !receipt !ContextScope
  | CopyRawConversation !receipt
  deriving Eq

instance Show receipt => Show (ConversationRequest receipt) where
  show (NewConversation receipt)="NewConversation "++show receipt
  show (ResumeConversation receipt _)="ResumeConversation "++show receipt++" [private]"
  show (OpenConversation receipt)="OpenConversation "++show receipt
  show (ConfigureConversation receipt _)="ConfigureConversation "++show receipt++" [private]"
  show (OpenConversationContext receipt scope)="OpenConversationContext "++show receipt++" "++show scope
  show (CopyRawConversation receipt)="CopyRawConversation "++show receipt
