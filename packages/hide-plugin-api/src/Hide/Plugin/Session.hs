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
  , PluginTool(..)
  , withPlugins
  ) where

import Hide.Plugin.AgentDirectory (AgentDirectory,DirectoryRequest)
import Hide.Plugin.Sidebar (Sidebar)
import Hide.Plugin.AgentServices (AgentServices)
import Hide.Plugin.Completion (HintServices)
import Hide.Plugin.ConversationInput (PrimaryInputServices, ChildInputServices)
import Hide.Plugin.ConversationSession (ConversationTarget,ConversationRequest)
import Hide.Plugin.Menu (MenuPublisher)
import Hide.Plugin.Agent (AgentId)
import Data.Text (Text)
import Hide.Plugin.Input (InputDeclaration)
import Hide.Plugin.Request (RequestServices)
import Hide.Plugin.Tool (Tool)
import Hide.Plugin.Services (EditorServices)
import Hide.Plugin.Transcript (ConversationPresenter)

-- | Capabilities for the concrete first-party directory workflow. Providers and
-- resource owners outlive activation. Captured requests are admitted by the host;
-- the reply constructor does not grant human or agent authority.
data Session c r settings completion receipt = Session
  { sessionSidebar :: Sidebar c r
  , sessionAgents :: AgentDirectory settings completion
  , sessionAgentReply :: DirectoryRequest settings completion -> r
  , sessionMenus :: MenuPublisher c r
  , sessionConversation :: c -> Either Text (ConversationTarget receipt)
  , sessionConversationReply :: ConversationRequest receipt -> r
  , sessionSelectedAgent :: c -> Either Text AgentId
    -- ^ Small selected-agent identity captured at menu admission. It grants no
    -- authority and must never be recomputed from later focus on a worker.
  }

-- | Endpoint visibility carries its actual service context. Editor tools are
-- available on anonymous and primary editor connections; coordination tools
-- require the host-attributed actor and are available to primary/child agents.
-- Visibility never replaces permission or caller checks.
--
-- Request tools share editor visibility but receive self-admitting services.
-- Each operation requests fresh permission through its host owner; exact diff
-- and window-read capabilities are present only for their host-captured request.
-- Question services require a host-authenticated actor/provider binding.
-- Do not wrap these tools
-- in a second permission call or supply general editor services before admission.
data PluginTool
  = EditorTool (Tool EditorServices)
  | RequestTool (Tool RequestServices)
  | CoordinationTool (Tool AgentServices)

-- | Scope registrations and workers around the supplied session action.
-- Activation runs before the event loop, never beneath its desktop lock.
-- Teardown must cancel/join owned workers and retire registrations, on success
-- or exception; it must not stop session-owned agents or other shared services.
-- Plugins publish prepared results through host queues instead of UI callbacks.
-- Tools are declared explicitly; the host registers their policies and
-- supplies actor-bound services with their declared admission contract: editor
-- services follow permission admission; request services own each admission.
data Plugin = Plugin
  { withPlugin :: forall c r settings completion receipt a. Eq completion =>
      Session c r settings completion receipt -> IO a -> IO a
  , pluginTools :: [PluginTool]
  , pluginConversation :: Maybe ConversationPresenter
    -- ^ Pure presentation selected at startup and evaluated only on the host
    -- preparation/checkpoint workers. This contribution owns no input lifetime.
  , pluginPrimaryInput :: Maybe (InputDeclaration PrimaryInputServices)
    -- ^ Primary Query/Steer executes on the host's existing control worker.
    -- Missing input leaves its transcript and recovered draft read-only.
  , pluginChildInput :: Maybe (InputDeclaration ChildInputServices)
    -- ^ One command registration shared by child draft attachments. The host
    -- supplies only the captured human slot/target service on its existing
    -- worker; missing input leaves child output read-only and preserves drafts.
  , pluginCompletionInput :: Maybe (InputDeclaration HintServices)
    -- ^ The host compiles this declaration before its event loop and invokes its
    -- command only on the admitted completion worker. Missing input preserves
    -- recovered user data and installs no callable hint attachment.
  }

-- | Nest resource scopes in declaration order and release them in reverse order.
-- Failure while activating a later plugin unwinds earlier scopes normally.
--
-- @withPlugins [] session action = action@
--
-- @withPlugins (p:ps) session action =
--   withPlugin p session (withPlugins ps session action)@
withPlugins :: Eq completion => [Plugin] -> Session c r settings completion receipt -> IO a -> IO a
withPlugins [] _ action=action
withPlugins (plugin:plugins) session action=withPlugin plugin session (withPlugins plugins session action)
