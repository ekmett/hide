-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Plugin.Identity
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Wire scopes for linked plugin registrations. A fresh 192-bit OS-random
-- identity distinguishes the scope across sessions; it is not an authority token.
module Hide.Plugin.Identity (randomIdentity) where

import qualified Data.ByteString as BS
import Numeric (showHex)
import System.Entropy (getEntropy)

-- | A fresh 48-character lowercase hexadecimal scope. OS entropy failures are
-- propagated; a counter or deterministic fallback cannot reuse a prior scope.
randomIdentity :: IO String
randomIdentity=do
  bytes<-getEntropy 24
  pure (concatMap (\n->let s=showHex n "" in replicate (2-length s) '0'++s) (BS.unpack bytes))
