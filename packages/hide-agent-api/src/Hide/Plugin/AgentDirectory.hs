-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Plugin.AgentDirectory
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Read capabilities and captured intents for an agent directory. Presentation
-- sees names, ancestry, state and advertised choices, never provider keys,
-- conversations or runtime handles. The host owns both receipt types and checks
-- them again when it admits a request; observing metadata grants no authority.
module Hide.Plugin.AgentDirectory
  ( AgentDirectory(..)
  , AgentSummary(..)
  , CompletionEntry(..)
  , DirectoryRequest(..)
  ) where

import Data.Text (Text)
import Hide.Plugin.Agent (AgentId,ConfigChoice)

-- | Cheap immutable directory projection. No task, transcript, source buffer,
-- capability payload or private provider identity is retained by a tree row.
data AgentSummary = AgentSummary
  { summaryId :: !AgentId, summaryName :: !Text, summaryParent :: !(Maybe AgentId)
  , summaryStatus :: !Text, summaryModel :: !Bool, summaryEffort :: !Bool } deriving (Eq,Show)

-- | An optional completion worker. The host supplies a stable public node key
-- and an opaque currentness receipt. Changing settings may replace the receipt
-- without changing the node key; presentation must retain the captured receipt
-- in an action rather than looking up whatever worker occupies the row later.
data CompletionEntry completion = CompletionEntry
  { completionKey :: !Text, completionTarget :: !completion
  , completionStatus :: !Text } deriving (Eq,Show)

-- | Trusted linked-plugin reads, valid within the supplying session scope.
-- Listing and lookup do not start providers, submit prompts or change settings.
-- Explicit completion-choice discovery may lazily connect its provider and
-- refresh its advertised metadata. Call it on a worker after input admission;
-- it can wait on the completion owner. Settings/completion receipts are small
-- host-owned identities and may only be passed back unchanged. Choice metadata has
-- already passed the host's provider-key redaction. This is not an MCP endpoint
-- and does not replace caller admission or output privacy checks.
data AgentDirectory settings completion = AgentDirectory
  { directoryAgents :: IO [AgentSummary]
  , directoryAgent :: AgentId -> IO (Either Text AgentSummary)
  , directorySettings :: AgentId -> IO (Either Text (settings,[ConfigChoice]))
  , directoryCompletion :: IO (Maybe (CompletionEntry completion))
  , directoryCompletionSettings :: completion -> Text -> IO (Either Text (completion,Text,[(Text,Text)],Text))
  }

-- | Closed navigation and settings intents. Construction does not execute them
-- or confer human authority. The host admits the originating command and checks
-- the captured identity/receipt at application, including after async waits.
-- Metadata refresh cannot retarget a request. Closing a directory view does not
-- stop an agent: provider lifetime belongs to the supplying session.
data DirectoryRequest settings completion
  = ShowAgent !AgentId
  | RenameAgentTo !AgentId !Text
  | CreateAgent !FilePath !Text !Text
  | ConfigureAgent !settings !Text !Text
  | ShowCompletion !completion
  | ConfigureCompletion !completion !Text !Text
  deriving (Eq,Show)
