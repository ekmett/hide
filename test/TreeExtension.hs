{-# LANGUAGE OverloadedStrings #-}
-- Independent declaration: no Model command constructor or production row tag.
module TreeExtension (declare) where
import Data.Aeson (Value(Null))
import Hide.Plugin.Command
import Hide.Plugin.Tree

declare :: Registry context -> (context -> IO reply)
  -> (ChildRequest -> IO (Either CommandError (NodePage context reply)))
  -> IO (TreeProvider context reply,NodeDef context reply)
declare registry reply children=do
  let ident text=either (error.show) id (nodeId text)
      codec=Codec Null (const (Left "Host only")) (const Null)
  action<-registerCommand registry (CommandDef "extension.tools.inspect" "Inspect" codec codec (\_ ()->pure (Right ()))) >>= either (error.show) pure
  let leaf=NodeDef (NodeInfo (ident "inspect") "Inspect" "★" False Nothing)
        (Just (treeAction registry action () (\ctx ()->reply ctx))) []
      root=NodeDef (NodeInfo (ident "root") "Tools" "" True Nothing) Nothing []
  provider<-registerTree registry "extension.tools" root (\_ -> children) >>= either (error.show) pure
  pure (provider,leaf)
