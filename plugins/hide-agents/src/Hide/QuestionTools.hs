{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.QuestionTools
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- The inline-question tool. Human authority and provider identity remain with
-- the host; this declaration receives only its captured request capability.
module Hide.QuestionTools (tools) where

import Hide.Plugin.Command
import Hide.Plugin.Questions
import Hide.Plugin.Tool

-- | Create or poll without blocking on a human. The host supplies fresh policy
-- admission per call; polling never fabricates an answer or grants approval.
tools :: [Tool (Maybe QuestionServices)]
tools=[Tool "ask_user" (ToolHints False False False) (CommandDef "hide.questions.ask" description
  questionInput questionOutput run)]
  where
    description="Create one inline human question and return questionId/status pending immediately. Continue independent work, then retrieve its status using questionId only. Answers require explicit human submission; pending replies never reveal the draft or selected choice. Only the authenticated requesting agent can retrieve results. No timeout supplies an answer or approval."
    run Nothing _=pure (Left (CommandRejected "ask_user requires the authenticated requesting agent."))
    run (Just services) request=requestQuestion services request
