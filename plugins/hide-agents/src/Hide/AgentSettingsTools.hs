{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.AgentSettingsTools
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- The agent plugin's read-only settings tool. Permission, public metadata capture,
-- context-file access and retirement belong to the host's scoped capability.
module Hide.AgentSettingsTools (tools) where

import Data.Aeson
import Data.Text (Text)
import Hide.Plugin.AgentSettings
import Hide.Plugin.Command
import Hide.Plugin.Tool (Tool(..))

-- | Read the admitted primary snapshot. No argument can select another actor,
-- directory, provider or authority; anonymous editor access remains host policy.
tools :: [Tool AgentSettingsServices]
tools=[Tool "agent_settings" True (CommandDef "hide.agents.settings"
  "Read provider executable, argument count, environment variable names, connection state, model/config choices and context usage. Secret-labelled values, argument values, environment values and session keys are omitted. Cannot change provider settings."
  (Codec (object ["type" .= ("object"::Text),"properties" .= object [],"required" .= ([]::[Text]),"additionalProperties" .= False])
    (\value->if value==object [] then Right () else Left "agent_settings accepts no arguments.") (const (object [])))
  (Codec (object ["type" .= ("object"::Text)]) Right id)
  (\services ()->fmap (either (Left . CommandRejected) (Right . toJSON)) (readAgentSettings services)))]
