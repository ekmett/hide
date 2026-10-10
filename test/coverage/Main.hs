{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Main
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Fast coverage paths. Reuse the inexpensive, bounded contract checks;
-- process-lifetime fixtures, allocation budgets and large histories stay in CI.
module Main (main) where

import Control.Exception (bracket)
import Data.Aeson (object, (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (defaultMainWithIngredients, localOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Runners (NumThreads(..), consoleTestReporter)
import Test.Tasty.Ingredients.Basic (listingTests)
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Runners.AntXML (antXMLRunner)
import qualified Hide.Buffer as Buffer
import qualified Hide.Model as Model
import qualified Hide.Protocol as Protocol
import qualified Hide.Recovery as Recovery
import Hide.TextPresentation (prepareTextPresentations)
import qualified BufferViewCheck
import qualified BrowserCheck
import qualified DefaultsCheck
import qualified FilesCheck
import qualified HexCheck
import qualified MarkdownCheck
import qualified WindowCheck
import qualified PluginCommandCheck
import qualified PluginFormCheck
import qualified PluginMenuCheck
import qualified PluginTreeCheck
import qualified GuestAccessCheck
import qualified LinksCheck
import qualified HelpCheck
import qualified CompletionCheck
import qualified BufferEditsCheck
import qualified AgentHubCheck
import qualified TextStyleCheck
import qualified MarkdownViewCheck

main :: IO ()
main = defaultMainWithIngredients
  [listingTests, composeReporters consoleTestReporter antXMLRunner] $
  -- A few checks scope process-wide environment; none relies on a prior check.
  localOption (NumThreads 1) $ testGroup "coverage"
  [ testCase "BufferView" BufferViewCheck.checks
  , testCase "Browser" BrowserCheck.checks
  , testCase "Defaults" DefaultsCheck.checks
  , testCase "Files" FilesCheck.checks
  , testCase "Hex" HexCheck.checks
  , testCase "Markdown" MarkdownCheck.checks
  , testCase "Window" WindowCheck.checks
  , testCase "PluginCommand" PluginCommandCheck.checks
  , testCase "PluginForm" PluginFormCheck.checks
  , testCase "PluginMenu" PluginMenuCheck.checks
  , testCase "PluginTree" PluginTreeCheck.checks
  , testCase "GuestAccess" GuestAccessCheck.checks
  , testCase "Links" LinksCheck.checks
  , testCase "Help" HelpCheck.checks
  , testCase "Completion" CompletionCheck.checks
  , testCase "BufferEdits" BufferEditsCheck.checks
  , testCase "AgentHub" AgentHubCheck.checks
  , testCase "TextStyle" TextStyleCheck.checks
  , testCase "MarkdownView" MarkdownViewCheck.checks
  , testCase "source input, paint and display transport" $ do
      let opened = Model.addDocument Nothing (Buffer.newBuffer "-- λ 界 👩🏽\x200d\&💻\nmain = pure ()\n") (Model.initialDesktop (80,25))
      first <- prepareTextPresentations opened
      input <- right (parseEither Protocol.parseInput (object ["type" .= ("paste" :: Text.Text), "text" .= ("x" :: Text.Text)]))
      let (changed, _) = Protocol.applyInput input first
      second <- prepareTextPresentations changed
      let before = Protocol.frameRows first
          after = Protocol.frameRows second
      (_, decoded) <- Protocol.decodeFrame [] (BL.toStrict (Protocol.framePacket True [] before []))
      decoded @?= before
      (_, updated) <- Protocol.decodeFrame decoded (BL.toStrict (Protocol.framePacket False before after []))
      updated @?= after
  , testCase "checkpoint round trip" $ withFile $ \path -> do
      let edited = Buffer.replaceSelection (Buffer.Selection 0 0) "-- changed\n" (Buffer.newBuffer "main = pure ()\n")
          desktop = Model.addDocument Nothing edited (Model.initialDesktop (80,25))
      Recovery.writeCheckpoint path desktop >>= right
      restored <- Recovery.readCheckpoint path (Model.initialDesktop (80,25)) >>= right
      let contents = map (Buffer.contents . Model.documentBuffer) . Map.elems . Model.buffers
      contents restored @?= contents desktop
  ]

right :: Show e => Either e a -> IO a
right = either (fail . show) pure

withFile :: (FilePath -> IO a) -> IO a
withFile action = do
  directory <- getTemporaryDirectory
  bracket (do (path, handle) <- openBinaryTempFile directory "hide-coverage"; hClose handle; pure path)
    removeFile action
