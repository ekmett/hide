{-# LANGUAGE OverloadedStrings #-}
-- Separately declared extension: imports public typed APIs, not Model or Render.
module WindowExtension (registerNotes) where
import Data.Aeson
import Hide.Plugin.Command
import Hide.Plugin.Menu
import Hide.Plugin.Window

registerNotes :: Registry context -> Menus context reply -> WindowScope -> IO ()
  -> (WindowUpdate -> reply) -> IO (Either MenuError MenuRef)
registerNotes registry menus scope before adapt=do
  let unit=Codec Null (const (Right ())) (const Null)
      result=Codec Null (const (Left "Prepared windows are host values."))
        (\prepared->object ["title" .= preparedWindowTitle prepared])
      command=CommandDef "example.notes" "Plugin notes" unit result
        (\_ ()->before >> fmap Right (prepareTextWindow "Plugin notes" "Independent text\nSecond row"))
  registered<-registerCommand registry command
  case registered of
    Left err->pure (Left (MenuCommandError err))
    Right action->contributeMenu menus (MenuDef "example.notes" "help" "extensions" 10 "Plugin notes" "" False
      (menuAction registry action (const (Right ())) (\_ prepared->maybe (fail "Window scope retired") (pure . adapt) =<< openTextWindow scope prepared)))
