-- | Ephemeral bearer capabilities for actor-bound editor bridges.
--
-- Tokens map authenticated bridge connections to hub identities within one host
-- runtime. They are not checkpointed or publicly rendered. Resolving a token
-- establishes identity only; live-agent and operation checks remain with the hub.
module Hide.AgentAccess
  (AgentAccess, newAgentAccess, grantAgentAccess, revokeAgentAccess, resolveAgentAccess) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Hide.AgentHub (AgentId)
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
