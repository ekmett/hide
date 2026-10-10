-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.AutocompleteConfig
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Validated launch settings for the human-owned completion provider.
--
-- ACP and Copilot settings coexist, but the selected provider determines which
-- connection is opened. An omitted provider selection defaults to disabled.
-- Argument lists are JSON-array strings because the same values populate editable
-- configuration fields; they are not shell command strings.
module Hide.AutocompleteConfig where
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither, Parser)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Hide.Plugin.Provider as ACP

-- | Provider, executable, argument, model/effort and transcript-view selections.
data CompletionConfig = CompletionConfig
  { provider :: T.Text, acpLaunch :: ACP.ProviderLaunch, model :: Maybe T.Text, effort :: Maybe T.Text
  , copilotLaunch :: ACP.ProviderLaunch, debug :: Bool, contextRanking :: T.Text, contextRankingBudgetMs :: Int } deriving (Eq,Show)

-- | Reject unknown settings, provider names and invalid bounded argument arrays.
parseCompletionConfig :: Value -> Either T.Text CompletionConfig
parseCompletionConfig = either (Left . T.pack) Right . parseEither (withObject "autocomplete" $ \o->do
  unless (all (`elem` ["provider","executable","arguments","model","effort","copilotExecutable","copilotArguments","debug","contextRanking","contextRankingBudgetMs"]) (KM.keys o)) (fail "Unknown autocomplete setting")
  backend<-o .:? "provider" .!= "off"
  unless (backend `elem` ["off","acp","copilot"]) (fail "Autocomplete provider must be off, acp or copilot")
  exe<-o .:? "executable" .!= "codex-acp"
  args<-o .:? "arguments" .!= "[]" >>= argumentList
  selected<-o .:? "model" .!= ""
  reasoning<-o .:? "effort" .!= ""
  cpExe<-o .:? "copilotExecutable" .!= "copilot-language-server"
  cpArgs<-o .:? "copilotArguments" .!= "[\"--stdio\"]" >>= argumentList
  unless (all (\x->not (null x) && not (any (<' ') x)) [exe,cpExe]) (fail "Invalid autocomplete executable")
  showDebug<-o .:? "debug" .!= False
  ranking<-o .:? "contextRanking" .!= "off"
  unless (ranking `elem` ["off","local","selected"]) (fail "Context ranking must be off, local or selected")
  budget<-o .:? "contextRankingBudgetMs" .!= 6000
  unless (budget>=1 && budget<=10000) (fail "Context ranking budget must be 1 to 10000 milliseconds")
  pure (CompletionConfig backend (ACP.ProviderLaunch exe args []) (nonempty selected) (nonempty reasoning) (ACP.ProviderLaunch cpExe cpArgs []) showDebug ranking budget))
  where nonempty t=if T.null t then Nothing else Just t
        argumentList :: T.Text -> Parser [String]
        argumentList t=case eitherDecodeStrict' (TE.encodeUtf8 t) of
          Right args | length args<=64,all ((<=4096).length) args->pure args
          _->fail "Autocomplete arguments must be a JSON array of strings"
