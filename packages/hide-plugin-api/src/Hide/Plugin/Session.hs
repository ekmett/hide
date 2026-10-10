{-# LANGUAGE RankNTypes #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Plugin.Session
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : RankNTypes
--
-- Ordinary linked-plugin composition at session startup. The executable chooses
-- plugins; the host supplies typed capabilities with opaque invocation/reply and
-- receipt types. There is no desktop callback, UI tick hook or dynamic loader.
module Hide.Plugin.Session
  ( Plugin(..)
  , Session(..)
  , withPlugins
  ) where

import Hide.Plugin.AgentDirectory (AgentDirectory,DirectoryRequest)
import Hide.Plugin.Sidebar (Sidebar)
import Hide.Plugin.AgentServices (AgentServices)
import Hide.Plugin.Tool (Tool)

-- | Capabilities for the concrete first-party directory workflow. Providers and
-- resource owners outlive activation. Captured requests are admitted by the host;
-- the reply constructor does not grant human or agent authority.
data Session c r settings completion = Session
  { sessionSidebar :: Sidebar c r
  , sessionAgents :: AgentDirectory settings completion
  , sessionAgentReply :: DirectoryRequest settings completion -> r
  }

-- | Scope registrations and workers around the supplied session action.
-- Activation runs before the event loop, never beneath its desktop lock.
-- Teardown must cancel/join owned workers and retire registrations, on success
-- or exception; it must not stop session-owned agents or other shared services.
-- Plugins publish prepared results through host queues instead of UI callbacks.
-- Agent tools are declared explicitly; the host registers their policies and
-- supplies actor-bound services only after caller and permission admission.
data Plugin = Plugin
  { withPlugin :: forall c r settings completion a. Eq completion =>
      Session c r settings completion -> IO a -> IO a
  , pluginAgentTools :: [Tool AgentServices]
  }

-- | Nest resource scopes in declaration order and release them in reverse order.
-- Failure while activating a later plugin unwinds earlier scopes normally.
--
-- @withPlugins [] session action = action@
--
-- @withPlugins (p:ps) session action =
--   withPlugin p session (withPlugins ps session action)@
withPlugins :: Eq completion => [Plugin] -> Session c r settings completion -> IO a -> IO a
withPlugins [] _ action=action
withPlugins (plugin:plugins) session action=withPlugin plugin session (withPlugins plugins session action)
