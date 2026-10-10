{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.AgentSettings
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Read-only primary conversation metadata. The host captures the selected
-- directory and public state after permission admission; context-file reads run
-- on the tool worker. No provider handle, secret value or mutation is exposed.
module Hide.Plugin.AgentSettings (AgentSettingsServices(..), SettingsSnapshot(..)) where

import Data.Aeson
import Data.Text (Text)

-- | A captured session capability. Deferred reads reject after its owner retires;
-- admitted reads may finish. Keeping this value does not retain a Desktop or keep
-- a provider running. The selected child is metadata, not a change of scope.
newtype AgentSettingsServices = AgentSettingsServices
  { readAgentSettings :: IO (Either Text SettingsSnapshot) }

-- | Public primary state captured together, with context read on execution.
-- Option values are redacted by the host before publication; context contains
-- only the explicitly configured agent guidance. Argument/environment values and
-- session keys have no fields. Scope and redaction markers are fixed in JSON.
data SettingsSnapshot = SettingsSnapshot
  { settingsExecutable :: !FilePath
  , settingsArgumentCount :: !Int
  , settingsEnvironmentNames :: ![Text]
  , settingsConnected :: !Bool
  , settingsSelectedAgent :: !(Maybe Text)
  , settingsReplying :: !Bool
  , settingsSteering :: !Bool
  , settingsContextUsage :: !(Maybe (Integer,Integer))
  , settingsOptions :: ![Value]
  , settingsContext :: !Value
  , settingsContextError :: !(Maybe Text)
  }

instance ToJSON SettingsSnapshot where
  toJSON s=object
    ["executable" .= settingsExecutable s,"argumentCount" .= settingsArgumentCount s
    ,"environmentNames" .= settingsEnvironmentNames s,"connected" .= settingsConnected s
    ,"scope" .= ("primary"::Text),"selectedAgent" .= settingsSelectedAgent s
    ,"replying" .= settingsReplying s,"steering" .= settingsSteering s
    ,"contextUsage" .= settingsContextUsage s,"settings" .= settingsOptions s
    ,"context" .= settingsContext s,"contextError" .= settingsContextError s
    ,"sessionKeysRedacted" .= True]
