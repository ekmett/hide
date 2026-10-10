{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.AgentServicesHost
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Bind public orchestration capabilities to host-authenticated identity and
-- workspace. The hub remains the operation owner, including revocation after
-- async waits, ancestry, provider startup rollback and bounded history.
module Hide.AgentServicesHost (agentServices) where

import Hide.AgentHub
import Hide.Plugin.AgentServices

-- | Capture the actor and workspace once, before queueing tool work or approval.
-- No tool argument can select another actor or working directory. Each closure
-- uses the hub's current authority checks; retaining it cannot keep an ended
-- caller alive. These capabilities do not expose human-only settings or steering.
agentServices :: AgentHub -> Actor -> FilePath -> AgentServices
agentServices hub actor directory=AgentServices
  { serviceWorkspace=directory
  , serviceAgents=listAgents hub actor
  , serviceStatus=statusAgent hub actor
  , serviceSpawn= \spec->if spawnDirectory spec/=directory
      then pure (Left "Agent spawn workspace differs from its captured authority.")
      else spawnAgentWithTask hub actor spec
  , serviceRename=renameAgent hub actor
  , serviceMessage=sendAgent hub actor
  , serviceWait=waitAgent hub actor
  , serviceCancel=cancelAgent hub actor
  , serviceEnd=endAgent hub actor
  , serviceHistory=historyAgent hub actor
  , serviceSearch=searchAgentHistory hub actor
  }
