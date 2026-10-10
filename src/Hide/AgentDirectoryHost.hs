-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.AgentDirectoryHost
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Bind the public directory reads to their session owners. No new queue,
-- provider lifetime or permission interpreter is introduced: completion choices
-- use the existing worker, and captured requests return to the ordinary effects.
module Hide.AgentDirectoryHost (agentDirectory) where

import qualified Data.Text as T
import qualified Hide.AgentHub as A
import Hide.AgentSidebarTypes (CompletionTarget(..),CompletionSummary(..))
import Hide.Autocomplete (Autocomplete,completionSummary,completionChoices)
import Hide.Plugin.AgentDirectory

-- | The caller scopes this capability inside both owners. A sidebar retirement
-- only releases its own registration and metadata worker, leaving providers live.
agentDirectory :: A.AgentHub -> Autocomplete -> AgentDirectory A.AgentConfigRef CompletionTarget
agentDirectory hub autocomplete=AgentDirectory
  { directoryAgents=A.agentSummaries hub
  , directoryAgent=A.agentSummary hub
  , directorySettings=A.agentConfiguration hub
  , directoryCompletion=fmap (fmap completion) (completionSummary autocomplete)
  , directoryCompletionSettings=completionChoices autocomplete
  }
  where
    completion (CompletionSummary target@(CompletionTarget epoch _) state)=
      CompletionEntry (T.pack ("acp-completion-"++show epoch)) target state
