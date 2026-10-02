{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.AutocompleteConfig where
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither, Parser)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified THC.Edit.ACP as ACP

data CompletionConfig = CompletionConfig
  { provider :: T.Text, acpLaunch :: ACP.Launch, model :: Maybe T.Text, effort :: Maybe T.Text
  , copilotLaunch :: ACP.Launch, debug :: Bool } deriving (Eq,Show)

parseCompletionConfig :: Value -> Either T.Text CompletionConfig
parseCompletionConfig = either (Left . T.pack) Right . parseEither (withObject "autocomplete" $ \o->do
  unless (all (`elem` ["provider","executable","arguments","model","effort","copilotExecutable","copilotArguments","debug"]) (KM.keys o)) (fail "Unknown autocomplete setting")
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
  pure (CompletionConfig backend (ACP.Launch exe args []) (nonempty selected) (nonempty reasoning) (ACP.Launch cpExe cpArgs []) showDebug))
  where nonempty t=if T.null t then Nothing else Just t
        argumentList :: T.Text -> Parser [String]
        argumentList t=case eitherDecodeStrict' (TE.encodeUtf8 t) of
          Right args | length args<=64,all ((<=4096).length) args->pure args
          _->fail "Autocomplete arguments must be a JSON array of strings"
