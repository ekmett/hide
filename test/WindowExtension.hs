{-# LANGUAGE OverloadedStrings #-}
-- Separately declared extension: imports public typed APIs, not Model or Render.
module WindowExtension (registerNotes,registerNotesTree) where
import Data.Aeson
import Hide.Plugin.Command
import Hide.Plugin.Menu
import Hide.Plugin.Window
import qualified Hide.Plugin.Tree as P

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

registerNotesTree :: Registry context -> WindowScope -> (WindowUpdate -> reply)
  -> IO (Either CommandError (P.TreeProvider context reply))
registerNotesTree registry scope adapt=do
  let codec=Codec Null (const (Right ())) (const Null)
  registered<-registerCommand registry (CommandDef "example.notes.open" "Open plugin notes" codec codec (\_ ()->pure (Right ())))
  case registered of
    Left err->pure (Left err)
    Right action->do
      let ident=either (error . show) id (P.nodeId "notes")
          prepare _ ()=do
            prepared<-prepareTextWindow "Sidebar notes" "Typed sidebar window"
            maybe (fail "Window scope retired") (pure . adapt) =<< openTextWindow scope prepared
          root=P.NodeDef (P.NodeInfo ident "Plugin notes" "" False Nothing)
            (Just (P.treeAction registry action () prepare)) []
      P.registerTree registry "example.notes" root (\_ _->pure (Right (P.NodePage [] Nothing)))
