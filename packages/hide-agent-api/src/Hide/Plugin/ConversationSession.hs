-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Plugin.ConversationSession
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Human New/Resume requests carry the host's opaque primary/provider receipt.
-- Captured resume input is private form data, never public menu metadata. A
-- request grants no authority: the host rechecks idle state and the original
-- provider/configuration before retiring or starting any provider resources.
module Hide.Plugin.ConversationSession
  ( ConversationTarget(..)
  , ConversationRequest(..)
  ) where

import Data.Text (Text)

-- | Immutable admission input captured at the human menu invocation. New selects
-- Primary; Resume requires the originally selected view to be Primary. The
-- remembered ID seeds only the private form and does not automatically resume.
data ConversationTarget receipt = ConversationTarget
  { conversationReceipt :: !receipt
  , conversationPrimary :: !Bool
  , conversationResumeId :: !Text
  }

-- | Closed lifecycle operations; the receipt cannot be supplied through JSON.
-- Resume carries a validated nonempty, trimmed ID. The host owns provider
-- support checks, saved provider/cwd selection, acquisition and recovery.
data ConversationRequest receipt
  = NewConversation !receipt
  | ResumeConversation !receipt !Text
  deriving Eq

instance Show receipt => Show (ConversationRequest receipt) where
  show (NewConversation receipt)="NewConversation "++show receipt
  show (ResumeConversation receipt _)="ResumeConversation "++show receipt++" [private]"
