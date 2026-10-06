{-# LANGUAGE OverloadedStrings #-}
-- A linked form consumer: public APIs only, no Model/Dialog/Render imports.
module FormExtension (prepareForm,prepareInputsForm) where
import Data.Aeson (Value(Null))
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Hide.Plugin.Command
import Hide.Plugin.Form hiding (prepareForm)
import qualified Hide.Plugin.Form as Form

prepareForm :: Registry c -> FormSpec -> (c -> Text -> IO (Either CommandError r))
  -> IO (Either CommandError (PreparedForm c r))
prepareForm registry spec run=do
  let codec=Codec Null (const (Left "Host-owned typed form input.")) (const Null)
  registered<-registerCommand registry (CommandDef "example.form.submit" "Submit form" codec codec run)
  case registered of
    Left err->pure (Left err)
    Right command->Form.prepareForm PrivateForm spec (formAction registry command id (\_ reply->pure reply))

prepareInputsForm :: Registry c -> FormSpec -> (c -> M.Map Text Text -> IO (Either CommandError r))
  -> IO (Either CommandError (PreparedForm c r))
prepareInputsForm registry spec run=do
  let codec=Codec Null (const (Left "Host-owned named form inputs.")) (const Null)
  registered<-registerCommand registry (CommandDef "example.form.inputs" "Submit inputs" codec codec run)
  case registered of
    Left err->pure (Left err)
    Right command->Form.prepareForm PrivateForm spec (inputsFormAction registry command Right (\_ reply->pure reply))
