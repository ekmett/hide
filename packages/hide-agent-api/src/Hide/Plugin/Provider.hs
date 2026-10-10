-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.Provider
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Concrete process acquisition input shared by linked provider implementations.
module Hide.Plugin.Provider
  ( ProviderLaunch(..), ProviderIdentity, newProviderIdentity
  , ProviderTurnId, newProviderTurnId
  , ProviderSubmission, newProviderSubmission, retireProviderSubmission, claimProviderSubmission
  , ProviderReply(..), ProviderTurn(..)
  ) where

import Control.Concurrent.STM
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Unique (Unique,newUnique,hashUnique)

-- | Host-minted acquisition identity. A repeated private session key is never
-- an equal incarnation; capability changes preserve this identity.
newtype ProviderIdentity = ProviderIdentity Unique deriving (Eq,Ord)
instance Show ProviderIdentity where show (ProviderIdentity value)="ProviderIdentity "++show (hashUnique value)
-- | Mint before acquisition, binding callbacks even during startup.
newProviderIdentity :: IO ProviderIdentity
newProviderIdentity=ProviderIdentity <$> newUnique

-- | Exact prompt identity, independent of the provider's reusable session key.
newtype ProviderTurnId = ProviderTurnId Unique deriving (Eq,Ord)
instance Show ProviderTurnId where show (ProviderTurnId value)="ProviderTurnId "++show (hashUnique value)
-- | Mint before any context/send worker escapes.
newProviderTurnId :: IO ProviderTurnId
newProviderTurnId=ProviderTurnId <$> newUnique

-- | A one-shot send lifetime. Prepared transport admission reads this in the
-- same STM transaction as enqueue. Retirement is monotone: after retirement no
-- uncommitted request may send; retirement does not undo an admitted request.
newtype ProviderSubmission = ProviderSubmission (TVar Bool)
-- | Create a fresh one-shot submission lifetime.
newProviderSubmission :: IO ProviderSubmission
newProviderSubmission=ProviderSubmission <$> newTVarIO True
-- | Idempotent monotone retirement. It does not undo an already committed send.
retireProviderSubmission :: ProviderSubmission -> IO ()
retireProviderSubmission (ProviderSubmission cell)=atomically (writeTVar cell False)
-- | /O(1)/. Consume once, atomically with the bounded prepared enqueue.
-- @claim >> claim@ yields @True@ then @False@; retirement always yields @False@.
claimProviderSubmission :: ProviderSubmission -> STM Bool
claimProviderSubmission (ProviderSubmission cell)=do
  live<-readTVar cell
  whenLive live (writeTVar cell False)
  pure live
  where whenLive True action=action
        whenLive False _=pure ()

-- | The supplying owner retains the result cell. Polling never waits; first
-- result wins, and retirement is idempotent. No queue or worker is created here.
data ProviderReply a = ProviderReply
  { pollProviderReply :: IO (Maybe (Either Text a))
  , awaitProviderReply :: IO (Either Text a)
    -- ^ Wait only on the supplying owner's worker; never on the UI.
  , retireProviderReply :: IO ()
  }

-- | Successful send admission and the later terminal outcome are separate.
-- Cancel belongs to this exact turn; it never cancels whichever turn is newer.
data ProviderTurn = ProviderTurn
  { providerTurnId :: !ProviderTurnId
  , providerTurnReply :: !(ProviderReply Value)
  , cancelProviderTurn :: IO ()
  }

-- | Executable, argv and environment values, without shell interpretation.
-- The acquiring service specifies whether environment entries are overrides or
-- the complete frozen environment. This value carries no editor authority.
data ProviderLaunch = ProviderLaunch
  { executable :: !FilePath
  , arguments :: ![String]
  , environment :: ![(String,String)]
  } deriving (Eq,Show)
