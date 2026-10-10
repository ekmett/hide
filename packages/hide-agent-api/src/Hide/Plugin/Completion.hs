-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Plugin.Completion
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Worker-only service for human hints to the persistent completion conversation.
module Hide.Plugin.Completion (HintServices(..)) where

import Data.Text (Text)

-- | Supplied to an admitted input command on the existing completion worker.
-- The host validates the original human input, mount and provider/configuration
-- receipt before supplying this context; it cannot be used to select a different
-- provider, expose source snapshots or submit a completion proposal.
-- A successful return acknowledges the hint turn. Failure leaves the input
-- available for correction. Each call rechecks the captured target and invocation
-- lifetime. Calls after command completion or provider/configuration replacement
-- are rejected; an admitted call may drain normally. This service starts no new
-- worker and does not bypass the completion owner's existing queue.
newtype HintServices = HintServices
  { sendHint :: Text -> IO (Either Text ()) }
