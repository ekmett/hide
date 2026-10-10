-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.AgentAccess
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Ephemeral bearer capabilities for actor-bound editor bridges.
--
-- Tokens map authenticated bridge connections to hub identities within one host
-- runtime. They are not checkpointed or publicly rendered. Resolving a token
-- establishes identity only; live-agent and operation checks remain with the hub.
module Hide.AgentAccess
  (AgentAccess, newAgentAccess, grantAgentAccess, revokeAgentAccess, resolveAgentAccess, resolveActiveAgentAccess) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Hide.AgentHub (AgentId,AgentHub,Actor(..),statusAgent)
import Hide.RemoteEndpoint (randomIdentity)

-- Bearer capabilities belong only to this running host. No Show instance or
-- snapshot exposes them; restarting the host invalidates every old bridge.
newtype AgentAccess = AgentAccess (TVar (M.Map Text AgentId))

newAgentAccess :: IO AgentAccess
newAgentAccess = AgentAccess <$> newTVarIO M.empty

-- | Issue an additional capability for an agent identity.
grantAgentAccess :: AgentAccess -> AgentId -> IO Text
grantAgentAccess access@(AgentAccess registry) ident = do
  token <- T.pack <$> randomIdentity
  accepted <- atomically $ do
    grants <- readTVar registry
    if M.member token grants then pure False else do
      writeTVar registry (M.insert token ident grants)
      pure True
  if accepted then pure token else grantAgentAccess access ident

-- | Revoke every capability associated with this agent.
revokeAgentAccess :: AgentAccess -> AgentId -> IO ()
revokeAgentAccess (AgentAccess registry) ident =
  atomically (modifyTVar' registry (M.filter (/= ident)))

-- | Resolve a private capability without granting any additional operation authority.
resolveAgentAccess :: AgentAccess -> Text -> IO (Maybe AgentId)
resolveAgentAccess (AgentAccess registry) token
  | T.length token /= 48 = pure Nothing
  | otherwise = M.lookup token <$> readTVarIO registry

-- | Resolve again when an admitted deferred operation reaches its target. A
-- retained token/earlier lookup cannot resurrect a revoked or ended actor.
-- Policy and target privacy remain with their existing operation owners.
resolveActiveAgentAccess :: AgentAccess -> AgentHub -> Text -> IO (Either Text AgentId)
resolveActiveAgentAccess access hub token=do
  bound<-resolveAgentAccess access token
  case bound of
    Nothing->pure (Left "Invalid or inactive agent connection.")
    Just ident->fmap (ident <$) (statusAgent hub (Agent ident) ident)
