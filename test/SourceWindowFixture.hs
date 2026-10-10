-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- |
-- Module      : SourceWindowFixture
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : GHC2021
--
-- Source-only fixtures require a real source identity; plugin windows fail here.
module SourceWindowFixture (sourceFixtureBuffer) where
import Hide.Model (Window,bufferId)
sourceFixtureBuffer :: Window -> Int
sourceFixtureBuffer window=case bufferId window of
  Just ident->ident
  Nothing->error "Source fixture unexpectedly received a plugin window"
