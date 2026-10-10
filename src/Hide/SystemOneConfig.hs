{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.SystemOneConfig
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Explicit session supplier selection from human-owned global configuration.
-- A project cannot redirect inference by supplying a different endpoint. Secrets
-- are resolved during startup, never encoded into public supplier descriptions.
module Hide.SystemOneConfig (loadSystemOneProvider) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAsciiLower,isAsciiUpper,isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)
import Hide.Plugin.SystemOne (DecisionProvider)
import Hide.GuestAccess (sensitiveLabel)
import Hide.SystemOneEndpoint (EndpointConfig(..),endpointProvider)
import Hide.SystemOneNative (NativeConfig(..),nativeProvider)

data Selection = Off | Endpoint !Text !Text !Text | Native !NativeConfig

-- | Omission means disabled. Only global, human-selected settings are read;
-- no model loading or network connection occurs until a decision is requested.
-- Authentication uses a named environment variable rather than TOML plaintext.
loadSystemOneProvider :: Value -> IO (Either Text (Maybe DecisionProvider))
loadSystemOneProvider value=case parseEither parseConfig value of
  Left _->pure (Left "Invalid [editor.systemOne] settings")
  Right Off->pure (Right Nothing)
  Right (Native config)->pure (Just <$> nativeProvider config)
  Right (Endpoint url model tokenEnv)->do
    token<-if T.null tokenEnv then pure (Right Nothing) else do
      found<-lookupEnv (T.unpack tokenEnv)
      pure (maybe (Left "The System One token environment variable is not set") (Right . Just . TE.encodeUtf8 . T.pack) found)
    pure (token >>= \secret->Just <$> endpointProvider (EndpointConfig url model secret))
  where
    parseConfig=withObject "System One" $ \o->do
      unless (all (`elem` ["provider","endpoint","model","tokenEnv","bundle","manifestSHA256","allocationLimitBytes","threads"]) (KM.keys o)) (fail "Unknown System One setting")
      provider<-o .:? "provider" .!= ("off"::Text)
      case provider of
        "off"->pure Off
        "laya"->Native <$> (NativeConfig
          <$> o .: "bundle"
          <*> o .: "manifestSHA256"
          <*> o .:? "allocationLimitBytes" .!= 3221225472
          <*> o .:? "threads" .!= 4)
        "endpoint"->do
          url<-o .: "endpoint"
          model<-o .: "model"
          tokenEnv<-o .:? "tokenEnv" .!= ""
          unless (T.length tokenEnv<=128 && validEnv tokenEnv && (T.null tokenEnv || sensitiveLabel tokenEnv)) (fail "Token variable must use a protected secret name")
          pure (Endpoint url model tokenEnv)
        _->fail "Unknown System One provider"
    validEnv text=T.null text || case T.uncons text of
      Just (c,rest)->first c && T.all (\x->first x || isDigit x) rest
      _->False
    first c=c=='_' || isAsciiLower c || isAsciiUpper c
