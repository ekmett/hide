{-# LANGUAGE OverloadedStrings #-}
-- A linked form consumer: public APIs only, no Model/Dialog/Render imports.
module FormExtension (prepareForm) where
import Data.Aeson (Value(Null))
import Data.Text (Text)
import Hide.Plugin.Command
import Hide.Plugin.Form

prepareForm :: Registry c -> InputFormSpec -> (c -> Text -> IO (Either CommandError r))
  -> IO (Either CommandError (PreparedInputForm c r))
prepareForm registry spec run=do
  let codec=Codec Null (const (Left "Host-owned typed form input.")) (const Null)
  registered<-registerCommand registry (CommandDef "example.form.submit" "Submit form" codec codec run)
  case registered of
    Left err->pure (Left err)
    Right command->prepareInputForm spec (formAction registry command id (\_ reply->pure reply))
