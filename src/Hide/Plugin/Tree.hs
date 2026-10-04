{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
-- | Typed tree providers for a shared sidebar, scoped by command registration.
--
-- Providers prepare bounded pages on workers. Metadata has no callable handler;
-- node actions retain typed arguments and an exact command lifetime. The host
-- owns expansion/request generations and checks provider/action currentness again
-- before adopting delayed replies. Nothing here grants agent authority.
module Hide.Plugin.Tree
  ( TreeRef, NodeId, nodeId, nodeIdText, TreeHit(..), ChildRequest(..)
  , NodeInfo(..), NodeDef(..), NodePage(..), PreparedPage, pageNodes, pageNext
  , TreeProvider, TreeAction, treeAction, registerTree, treeReference, treeRoot
  , treeCurrent, retireTree, loadChildren, invokeTreeAction, actionReference
  ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (unless)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Hide.Plugin.Command

-- | Provider-local identity. It is distinct from labels and resource paths.
newtype NodeId = NodeId Text deriving (Eq,Ord,Show)
nodeId :: Text -> Either Text NodeId
nodeId text
  | T.null text || T.length text>128 || T.any (< ' ') text=Left "Invalid sidebar node ID."
  | otherwise=Right (NodeId text)
nodeIdText :: NodeId -> Text
nodeIdText (NodeId text)=text
newtype TreeRef = TreeRef CommandRef deriving (Eq,Ord,Show)
-- | Frozen hit identity, including the owning node publication generation.
data TreeHit = TreeHit !TreeRef !NodeId !Integer deriving (Eq,Ord,Show)
-- | Page cursors are opaque bounded provider tokens; the host never derives paths.
data ChildRequest = ChildRequest !NodeId !(Maybe Text) deriving (Eq,Show)
-- | Prepared small presentation fields. Optional resource annotations support
-- host-owned privacy and open-file badges; labels do not establish authority.
data NodeInfo = NodeInfo
  { infoId :: !NodeId, infoLabel :: !Text, infoIcon :: !Text
  , infoBranch :: !Bool, infoResource :: !(Maybe FilePath)
  } deriving (Eq,Show)
data TreeAction context reply = forall a b. TreeAction
  (Registry context) (Command context a b) a (context -> b -> IO reply)
treeAction :: Registry context -> Command context a b -> a -> (context -> b -> IO reply) -> TreeAction context reply
treeAction=TreeAction
actionReference :: TreeAction context reply -> CommandRef
actionReference (TreeAction _ command _ _)=commandRef command
data NodeDef context reply = NodeDef
  { nodeInfo :: !NodeInfo, nodeAction :: Maybe (TreeAction context reply) }
data NodePage context reply = NodePage [NodeDef context reply] (Maybe Text)
-- Constructor is private: force/validate metadata before owner publication.
data PreparedPage context reply = PreparedPage [NodeDef context reply] (Maybe Text)
pageNodes :: PreparedPage context reply -> [NodeDef context reply]
pageNodes (PreparedPage nodes _)=nodes
pageNext :: PreparedPage context reply -> Maybe Text
pageNext (PreparedPage _ token)=token
data TreeProvider context reply = TreeProvider
  (Registry context) (Command context ChildRequest (PreparedPage context reply)) (NodeDef context reply)

-- | Register one provider through an ordinary scoped children command. Root
-- metadata is strictly prepared here, outside the UI lock. Duplicate provider
-- IDs are the command registry's duplicate IDs, not a second identity namespace.
registerTree :: Registry context -> Text -> NodeDef context reply
  -> (context -> ChildRequest -> IO (Either CommandError (NodePage context reply)))
  -> IO (Either CommandError (TreeProvider context reply))
registerTree registry name root children=do
  prepared<-preparePage (NodePage [root] Nothing)
  case prepared of
    Left err->pure (Left err)
    Right _->fmap (fmap (\command->TreeProvider registry command root)) $ registerCommand registry
      (CommandDef (name<>".children") (infoLabel (nodeInfo root)) input output
        (\context request->children context request >>= either (pure . Left) preparePage))
  where
    input=Codec Null (const (Left "Tree loading is a host-owned typed operation.")) (const Null)
    output=Codec Null (const (Left "Tree pages are prepared host values.")) (const Null)

treeReference :: TreeProvider context reply -> TreeRef
treeReference (TreeProvider _ command _)=TreeRef (commandRef command)
treeRoot :: TreeProvider context reply -> NodeDef context reply
treeRoot (TreeProvider _ _ root)=root
treeCurrent :: TreeProvider context reply -> IO Bool
treeCurrent (TreeProvider registry command _)=commandCurrent registry (commandRef command)
retireTree :: TreeProvider context reply -> IO (Either CommandError ())
retireTree (TreeProvider registry command _)=retireCommand registry (commandRef command)
loadChildren :: TreeProvider context reply -> context -> ChildRequest -> IO (Either CommandError (PreparedPage context reply))
loadChildren (TreeProvider registry command _) context request@(ChildRequest _ token)
  | maybe False (\value->T.length value>256 || T.any (< ' ') value) token=pure (Left (InvalidArguments "Invalid sidebar page cursor."))
  | otherwise=invoke registry command context request
invokeTreeAction :: TreeAction context reply -> context -> IO (Either CommandError reply)
invokeTreeAction (TreeAction registry command arguments prepare) context=do
  result<-invoke registry command context arguments
  case result of Left err->pure (Left err); Right value->Right <$> prepare context value

preparePage :: NodePage context reply -> IO (Either CommandError (PreparedPage context reply))
preparePage (NodePage supplied token)
  | length bounded>128=pure (Left (CommandRejected "Sidebar pages contain at most 128 nodes."))
  | maybe False (\value->T.length value>256 || T.any (< ' ') value) token=pure (Left (CommandRejected "Invalid sidebar continuation."))
  | M.size unique/=length bounded=pure (Left (CommandRejected "Duplicate sidebar node IDs in a page."))
  | otherwise=do
      mapM_ prepare bounded
      pure (Right (PreparedPage bounded token))
  where
    bounded=take 129 supplied
    unique=M.fromList [(infoId (nodeInfo node),()) | node<-bounded]
    prepare node=do
      let info=nodeInfo node
      mapM_ (evaluate . actionReference) (nodeAction node)
      _<-evaluate (force (nodeIdText (infoId info),infoLabel info,infoIcon info,infoBranch info,infoResource info))
      unless (T.length (infoLabel info)<=256 && T.all (>= ' ') (infoLabel info) &&
        T.length (infoIcon info)<=4 && T.all (>= ' ') (infoIcon info) && maybe True ((<=32768) . length) (infoResource info))
        (ioError (userError "Invalid sidebar node presentation."))
