-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Plugin.Request
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Editor-visible services whose requests own fresh host permission admission.
-- Exact mutation targets are bound by host preflight, never by plugin callbacks.
module Hide.Plugin.Request
  ( RequestServices(..)
  ) where

import Hide.Plugin.BufferDiff (BufferDiffServices)
import Hide.Plugin.BufferRead (BufferReadServices)

-- | Request capabilities supplied before permission admission. Calls run on the
-- invoking worker and use their existing session/actor-bound host owner; adding
-- an outer permission call would duplicate approval. Keeping this product alive
-- cannot extend the reader, editable request or plugin registration lifetime.
--
-- A diff service is present only for an exact host-captured diff request. Other
-- request tools receive Nothing and must reject edits, never construct a new
-- target from a numeric ID/revision or inherit authority from a read operation.
data RequestServices = RequestServices
  { requestBuffers :: !BufferReadServices
  , requestDiff :: !(Maybe BufferDiffServices)
  }
