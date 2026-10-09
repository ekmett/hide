{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
-- |
-- Module      : Hide.Plugin.Tree
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : ExistentialQuantification, OverloadedStrings
--
-- Typed tree providers for a shared sidebar, scoped by command registration.
--
-- Providers prepare bounded pages on workers. Metadata has no callable handler;
-- node actions retain typed arguments and an exact command lifetime. The host
-- owns expansion/request generations and checks provider/action currentness again
-- before adopting delayed replies. Nothing here grants agent authority.
module Hide.Plugin.Tree
  ( TreeRef, NodeId, nodeId, nodeIdText, TreeHit(..), ChildRequest(..)
  , NodeInfo(..), NodeDef(..), NodeMenu(..), TreeMenuTarget(..), menuTarget, menuActions, NodePage(..), PreparedPage, pageNodes, pageNext
  , TreeProvider, TreeAction, treeAction, registerTree, treeReference, treeRoot
  , treeIdentity, treeCurrent, retireTree, loadChildren, invokeTreeAction, actionReference, actionCurrent
  ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (unless)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Hide.RemoteEndpoint (randomIdentity)
import Hide.Plugin.Command

-- | Provider-local identity. It is distinct from labels and resource paths.
newtype NodeId = NodeId Text deriving (Eq,Ord,Show)
nodeId :: Text -> Either Text NodeId
nodeId text
  | T.null text || T.length text>128 || T.any (< ' ') text=Left "Invalid sidebar node ID."
  | otherwise=Right (NodeId text)
nodeIdText :: NodeId -> Text
nodeIdText (NodeId text)=text
data TreeRef = TreeRef !CommandRef !Text deriving (Eq,Ord,Show)
-- | /O(1)/. Opaque wire scope for this exact provider registration. It survives
-- redraw, scrolling and child refresh; re-registration always creates a new scope.
-- Node IDs remain provider-owned and are meaningful only inside this scope.
treeIdentity :: TreeRef -> Text
treeIdentity (TreeRef _ identity)=identity
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
actionCurrent :: TreeAction context reply -> IO Bool
actionCurrent (TreeAction registry command _ _)=commandCurrent registry (commandRef command)
-- | Secondary actions are prepared declarations. Resource links reuse the
-- host's existing human link transport; registered actions retain typed args.
data TreeMenuTarget = RegisteredAction !CommandRef | ResourceLink !FilePath !Text deriving (Eq,Show)
data NodeMenu context reply = ActionMenu !Text (TreeAction context reply) | ResourceMenu !Text !FilePath !Text
menuTarget :: NodeMenu context reply -> (Text,TreeMenuTarget)
menuTarget (ActionMenu title action)=(title,RegisteredAction (actionReference action))
menuTarget (ResourceMenu title path target)=(title,ResourceLink path target)
menuActions :: NodeDef context reply -> [TreeAction context reply]
menuActions node=[action | ActionMenu _ action<-nodeMenus node]
data NodeDef context reply = NodeDef
  { nodeInfo :: !NodeInfo, nodeAction :: Maybe (TreeAction context reply)
  , nodeMenus :: [NodeMenu context reply] }

data NodePage context reply = NodePage [NodeDef context reply] (Maybe Text)
-- Constructor is private: force/validate metadata before owner publication.
data PreparedPage context reply = PreparedPage [NodeDef context reply] (Maybe Text)
pageNodes :: PreparedPage context reply -> [NodeDef context reply]
pageNodes (PreparedPage nodes _)=nodes
pageNext :: PreparedPage context reply -> Maybe Text
pageNext (PreparedPage _ token)=token
data TreeProvider context reply = TreeProvider
  (Registry context) (Command context ChildRequest (PreparedPage context reply)) !Text (NodeDef context reply)

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
    Right _->do
      identity<-T.pack <$> randomIdentity
      registered<-registerCommand registry
        (CommandDef (name<>".children") (infoLabel (nodeInfo root)) input output
          (\context request->children context request >>= either (pure . Left) preparePage))
      case registered of
        Left err->pure (Left err)
        Right command->pure (Right (TreeProvider registry command identity root))
  where
    input=Codec Null (const (Left "Tree loading is a host-owned typed operation.")) (const Null)
    output=Codec Null (const (Left "Tree pages are prepared host values.")) (const Null)

treeReference :: TreeProvider context reply -> TreeRef
treeReference (TreeProvider _ command identity _)=TreeRef (commandRef command) identity
treeRoot :: TreeProvider context reply -> NodeDef context reply
treeRoot (TreeProvider _ _ _ root)=root
treeCurrent :: TreeProvider context reply -> IO Bool
treeCurrent (TreeProvider registry command _ _)=commandCurrent registry (commandRef command)
retireTree :: TreeProvider context reply -> IO (Either CommandError ())
retireTree (TreeProvider registry command _ _)=retireCommand registry (commandRef command)
loadChildren :: TreeProvider context reply -> context -> ChildRequest -> IO (Either CommandError (PreparedPage context reply))
loadChildren (TreeProvider registry command _ _) context request@(ChildRequest _ token)
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
      let actions=nodeMenus node
          refs=maybe [] (pure . actionReference) (nodeAction node)++map actionReference (menuActions node)
      unless (length (take 9 actions)<=8 && all (\action->let (title,_)=menuTarget action in not (T.null title) && T.length title<=128 && T.all (>= ' ') title) actions && M.size (M.fromList [(ref,()) | ref<-refs])==length refs)
        (ioError (userError "Invalid sidebar node actions."))
      mapM_ (evaluate . actionReference) (nodeAction node)
      mapM_ (\action->case menuTarget action of
        (title,RegisteredAction reference)->evaluate (force title) >> evaluate reference >> pure ()
        (title,ResourceLink path target)->do
          _<-evaluate (force (title,path,target))
          unless (length path<=32768 && T.length target<=8192 && T.all (>= ' ') target)
            (ioError (userError "Invalid sidebar resource action."))) actions
      _<-evaluate (force (nodeIdText (infoId info),infoLabel info,infoIcon info,infoBranch info,infoResource info))
      unless (T.length (infoLabel info)<=256 && T.all (>= ' ') (infoLabel info) &&
        T.length (infoIcon info)<=4 && T.all (>= ' ') (infoIcon info) && maybe True ((<=32768) . length) (infoResource info))
        (ioError (userError "Invalid sidebar node presentation."))
