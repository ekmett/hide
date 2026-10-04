{-# LANGUAGE OverloadedStrings #-}
-- A separately declared extension: no core command sum, Model or Render imports.
module MenuExtension (registerExtension) where
import Data.Aeson
import qualified Data.Text as T
import Hide.Plugin.Command
import Hide.Plugin.Menu

registerExtension :: Registry MenuContext -> Menus MenuContext reply
  -> (MenuContext -> T.Text -> IO reply) -> IO (Either MenuError MenuRef)
registerExtension registry menus prepare=do
  let codec=Codec (object ["type" .= ("string"::T.Text)])
        (\value->case value of String text->Right text; _->Left "Expected Markdown text.") String
      definition=CommandDef "example.manual" "Extension manual" codec codec (\_ text->pure (Right text))
  registered<-registerCommand registry definition
  case registered of
    Left err->pure (Left (MenuCommandError err))
    Right command->contributeMenu menus (MenuDef "example.manual" "help" "extensions" 10 "Extension manual" "" True
      (menuAction registry command "# Independent extension\n\n[Installation](docs/install.md)\n" prepare))
