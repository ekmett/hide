-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- |
-- Module      : Main
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : GHC2021

module Main (main) where
import qualified Hide.App
import qualified Hide.AgentUI
main :: IO ()
main = Hide.App.main [Hide.AgentUI.plugin]
