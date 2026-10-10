{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.EnvironmentTools
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Agent environment tools use typed session services. The host retains workspace,
-- redaction, mutation policy and registration lifetime; this module declares no
-- raw process/configuration access or human-authority fallback.
module Hide.EnvironmentTools
  ( tools
  ) where

import Hide.Plugin.Command (CommandDef(..))
import Hide.Plugin.Environment
import Hide.Plugin.Tool (Tool(..))

-- | The editor endpoint's explicit environment tools. Read-only hints remain
-- policy metadata; every invocation still requires host permission admission.
tools :: [Tool EnvironmentServices]
tools=[Tool "environment_get" True (CommandDef "hide.environment.get"
    "Inspect the editor process environment used by newly launched builds, terminals, debuggers and agents. Optional names limits the result; absent names are null. Credential and editor authority values are always redacted. Use before prescribing shell exports or an editor restart."
    getInput getOutput environmentGet)
  ,Tool "environment_set" False (CommandDef "hide.environment.set"
    "Set or unset variables for newly launched editor subprocesses. values maps names to strings or null (unset); scope is session (default), project (thc.toml), or global (thc/config.toml). Project overrides global. Existing processes retain their environment: restart only the affected terminal/provider/job. Credential and editor authority variables cannot be changed through this tool. Prefer a project build configuration fix when it makes the dependency discoverable for everyone."
    setInput setOutput environmentSet)]
