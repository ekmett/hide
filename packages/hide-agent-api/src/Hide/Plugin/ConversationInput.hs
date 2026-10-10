-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Plugin.ConversationInput
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Acknowledged human input operations bound to a captured conversation slot.
-- Provider identities, drafts, configuration receipts and queues remain private
-- to the host; plugins cannot select a different target or operation.
module Hide.Plugin.ConversationInput (PrimaryInputServices(..), ChildInputServices(..)) where

import Data.Text (Text)

-- | Supplied only on the existing child input worker after human mount/slot
-- admission. The host normalizes the editor's code-input grammar before sending
-- and rechecks the original provider/configuration/cancellation receipt at Hub
-- admission. Query succeeds when that exact message is queued; Steer succeeds
-- only after provider acknowledgement. Neither waits for a query's final answer.
--
-- Calls after invocation completion are rejected. Admitted calls drain before
-- revocation; retaining this record cannot send to a replacement child/provider.
-- Failure grants no draft update. The service creates no additional worker or
-- queue and never exposes source files, private buffers or the Desktop.
newtype ChildInputServices = ChildInputServices
  { submitInput :: Text -> IO (Either Text ()) }

-- | The captured human primary slot. Query acknowledges admission to the
-- existing conversation queue or successful provider prompt submission; Steer
-- acknowledges the provider's acceptance. It never waits for a query's answer.
-- The original launch/provider/configuration lifetime remains authoritative, including an
-- initial connection acquired for this query. Cancellation/replacement and calls
-- after invocation completion reject. Admitted calls drain before revocation.
-- No draft, source snapshot, provider object or queue crosses this boundary.
newtype PrimaryInputServices = PrimaryInputServices
  { submitPrimaryInput :: Text -> IO (Either Text ()) }
