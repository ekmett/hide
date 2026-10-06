{-# LANGUAGE OverloadedStrings #-}
-- Separately declared extension: imports public typed APIs, not Model or Render.
module WindowExtension (registerNotes,registerNotesTree,registerEditorNotes) where
import Data.IORef
import Data.Aeson
import Hide.Plugin.Command
import Hide.Plugin.Menu
import Hide.Plugin.Window
import qualified Hide.Plugin.Editor as E
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

-- Both slots share one typed command; their adapters choose the operation. The
-- retained preparation loses its seed on the first opening, like a real owner.
registerEditorNotes :: Registry context -> Menus context reply -> WindowScope -> E.DraftRef
  -> (E.DraftSubmission -> Bool -> Either CommandError (E.DraftSubmission,Bool))
  -> (E.DraftSubmission -> Bool -> IO ())
  -> (EditorWindowUpdate context reply -> reply) -> (E.EditorUpdate -> reply)
  -> IO (Either MenuError MenuRef)
registerEditorNotes registry menus scope draft arguments submitted adaptWindow adaptUpdate=do
  let opaque=Codec Null (const (Left "Editor input is a host capability.")) (const Null)
      unit=Codec Null (const (Right ())) (const Null)
  registered<-registerCommand registry (CommandDef "example.editor.submit" "Submit notes" opaque (Codec Null (const (Left "Editor results are host capabilities.")) (const Null))
    (\_ (input,alternate)->submitted input alternate >> pure (Right input)))
  case registered of
    Left err->pure (Left (MenuCommandError err))
    Right action->do
      let slot alternate=E.editorAction registry action (\input->arguments input alternate)
            (\_ input->pure (adaptUpdate (E.clearEditorDraft input)))
      prepared<-E.prepareEditor draft (E.EditorSpec False "Apply" "Append") "    plain draft" (slot False) (slot True)
      case prepared of
        Left err->pure (Left (MenuCommandError err))
        Right initial->do
          next<-newIORef initial
          opening<-registerCommand registry (CommandDef "example.editor" "Editable notes" unit opaque
            (\_ ()->do
              current<-readIORef next
              E.remountEditor current >>= writeIORef next
              pure (Right current)))
          case opening of
            Left err->pure (Left (MenuCommandError err))
            Right open->contributeMenu menus (MenuDef "example.editor" "help" "extensions" 11 "Editable notes" "" False
              (menuAction registry open (const (Right ())) (\_ editor->do
                body<-prepareTextWindow "Editable notes" "Independent text\nSecond row"
                maybe (fail "Window scope retired") (pure . adaptWindow) =<< openEditorWindow scope body editor)))
