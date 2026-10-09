{-# LANGUAGE OverloadedStrings #-}
-- A separately declared extension: no core command sum, Model or Render imports.
-- |
-- Module      : MenuExtension
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module MenuExtension (registerExtension,registerContextExtension) where
import Data.Aeson
import qualified Data.Text as T
import Hide.Plugin.Command
import Hide.Plugin.Menu

registerExtension :: Registry context -> Menus context reply
  -> (context -> T.Text -> IO reply) -> IO (Either MenuError MenuRef)
registerExtension registry menus prepare=do
  let codec=Codec (object ["type" .= ("string"::T.Text)])
        (\value->case value of String text->Right text; _->Left "Expected Markdown text.") String
      definition=CommandDef "example.manual" "Extension manual" codec codec (\_ text->pure (Right text))
  registered<-registerCommand registry definition
  case registered of
    Left err->pure (Left (MenuCommandError err))
    Right command->contributeMenu menus (MenuDef "example.manual" "help" "extensions" 10 "Extension manual" "" True
      (menuAction registry command (const (Right "# Independent extension\n\n[Installation](docs/install.md)\n")) prepare))

registerContextExtension :: T.Text -> Registry context -> Menus context reply
  -> (context -> T.Text -> IO reply) -> IO (Either MenuError MenuRef)
registerContextExtension ident registry menus prepare=do
  let codec=Codec Null (\value->case value of String text->Right text; _->Left "Expected text") String
      definition=CommandDef ident "Context extension" codec codec (\_ text->pure (Right text))
  registered<-registerCommand registry definition
  case registered of
    Left err->pure (Left (MenuCommandError err))
    Right command->contributeMenu menus (MenuDef ident "context.messages" "extensions" 10 "Context extension" "" True
      (menuAction registry command (const (Right "# Context extension\n")) prepare))
