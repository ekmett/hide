-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : AllocationProfile
-- Copyright   : (c) Edward Kmett
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Optimized allocation limits belong to the normal test run. HPC counters
-- allocate independently; instrumented runs retain measurements and semantics.
module AllocationProfile (AllocationProfile(..), withinBudget) where

import Data.Int (Int64)
import Test.Tasty.Options (IsOption(..), flagCLParser, safeReadBool)

-- | Explicit test configuration; the default keeps every optimized limit active.
data AllocationProfile = Normal | Instrumented deriving (Eq,Show)

instance IsOption AllocationProfile where
  defaultValue=Normal
  parseValue value=(\enabled->if enabled then Instrumented else Normal) <$> safeReadBool value
  optionName=pure "instrumented"
  optionHelp=pure "Measure HPC allocations separately from optimized allocation limits"
  optionCLParser=flagCLParser Nothing Instrumented

-- | /O(1)/. Normal measurements retain their original strict upper bounds.
--
-- @withinBudget Normal measured limit ≡ measured < limit@
--
-- @withinBudget Instrumented measured limit ≡ measured `seq` True@
--
-- The instrumented profile still forces the counter delta; callers must keep
-- accompanying value and identity assertions outside this numeric predicate.
withinBudget :: AllocationProfile -> Int64 -> Int64 -> Bool
withinBudget Normal measured limit=measured<limit
withinBudget Instrumented measured _=measured `seq` True
