-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.Provider
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Concrete process acquisition input shared by linked provider implementations.
module Hide.Plugin.Provider (ProviderLaunch(..)) where

-- | Executable, argv and environment values, without shell interpretation.
-- The acquiring service specifies whether environment entries are overrides or
-- the complete frozen environment. This value carries no editor authority.
data ProviderLaunch = ProviderLaunch
  { executable :: !FilePath
  , arguments :: ![String]
  , environment :: ![(String,String)]
  } deriving (Eq,Show)
