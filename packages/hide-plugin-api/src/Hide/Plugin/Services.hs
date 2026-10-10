-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.Services
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Explicit services for editor-session plugin tools. Coordination uses its
-- separate actor-bound context; an anonymous editor call cannot fabricate it.
module Hide.Plugin.Services
  ( EditorServices(..)
  ) where

import Hide.Plugin.Documentation (DocsServices)
import Hide.Plugin.AgentSettings (AgentSettingsServices)
import Hide.Plugin.Environment (EnvironmentServices)

-- | Host-granted editor capabilities captured after permission admission.
-- Each service retains its own scoped operation owner; keeping this product
-- alive does not keep retired registrations or a replacement session alive.
data EditorServices = EditorServices
  { editorDocumentation :: !DocsServices
  , editorEnvironment :: !EnvironmentServices
  , editorAgentSettings :: !AgentSettingsServices
  }
