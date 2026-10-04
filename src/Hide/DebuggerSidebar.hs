{-# LANGUAGE OverloadedStrings #-}
-- | Module      : Hide.DebuggerSidebar
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Debug is an ordinary scoped provider. Its callbacks prepare small node pages
-- on sidebar workers and wait on the existing debugger owner, never issue DAP
-- requests themselves. Stop epochs are part of every retained child target.
module Hide.DebuggerSidebar
  ( DebuggerSidebar
  , withDebuggerSidebar
  , tickDebuggerSidebar
  ) where

import Control.Monad (forM)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Hide.Debugger (Debugger,debuggerSidebarEpoch,debuggerSidebarSession,debuggerSidebarRead,debuggerWatches)
import Hide.DebuggerSidebarTypes
import Hide.Model (Desktop(..),Effect(LoadTree))
import Hide.Sidebar
import Hide.Plugin.Command
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Tree as P
import Hide.SidebarCommands

data DebuggerSidebar = DebuggerSidebar !P.TreeRef !(IORef (Maybe Int)) !(IORef (Maybe Int)) !P.TreeRef !(IORef (Maybe Int))

rootId :: P.NodeId
rootId=ident "debug"
ident :: Text -> P.NodeId
ident=either (error . T.unpack) id . P.nodeId
node :: Text -> [Int] -> P.NodeId
node kind parts=ident (T.intercalate ":" (kind:map (T.pack . show) parts))

-- | Register metadata outside the UI owner. Closing these commands refuses
-- retained hits; debugger session lifetime remains with the existing owner.
withDebuggerSidebar :: SidebarHost -> Debugger -> (DebuggerSidebar -> IO a) -> IO a
withDebuggerSidebar host runtime use=withRegistry $ \registry->do
  selected<-newIORef Nothing
  shownSession<-newIORef Nothing
  watchRevisionSeen<-newIORef Nothing
  open<-registerCommand registry (CommandDef "hide.sidebar.debug.source" "Go to source" hidden hidden $ \ctx captured->
    pure $ if sidebarOrigin ctx==Menu.HumanMenu then Right (SidebarDebug captured)
      else Left (CommandRejected "Debug sidebar actions require human input.")) >>= either (ioError . userError . show) pure
  let frameAction epoch tid fid=P.treeAction registry open (SelectDebugFrame epoch tid fid) (\_ value->pure value)
      root=P.NodeDef (P.NodeInfo rootId "Debug" "" True Nothing) Nothing []
      children _ (P.ChildRequest parent cursor)=do
        epoch<-debuggerSidebarEpoch runtime
        case (epoch,pageOffset cursor) of
          (Nothing,_) | parent==rootId->pure (Right (P.NodePage [P.NodeDef (P.NodeInfo (ident "debug-unavailable") "Running or no revealed stopped session" "" False Nothing) Nothing []] Nothing))
          (Nothing,_)->pure (Left (CommandRejected "Debugger node expired."))
          (_,Nothing)->pure (Left (InvalidArguments "Invalid debugger page cursor."))
          (Just current,Just offset)->case target current parent of
            Nothing->pure (Left (CommandRejected "Debugger node expired."))
            Just request->do
              result<-debuggerSidebarRead runtime (DebugPageRequest current request offset)
              case result of
                Left err->pure (Left (CommandRejected err))
                Right body->do
                  rows<-forM (zip [offset..] (take 128 (items (rowField request) body))) $ \(index,row)->pure (makeNode frameAction current request index row)
                  pure (Right (P.NodePage rows (continuation request offset body (length rows))))
  provider<-P.registerTree registry "hide.sidebar.debug" root children >>= either (ioError . userError . show) pure
  publishTreeFromHost host provider
  let registerWatch name title=registerCommand registry (CommandDef name title hidden hidden $ \ctx captured->
        pure $ if sidebarOrigin ctx==Menu.HumanMenu then Right (SidebarDebug captured)
          else Left (CommandRejected "Watch changes require human input.")) >>= either (ioError . userError . show) pure
  add<-registerWatch "hide.sidebar.debug.watch-add" "Add watch"
  edit<-registerWatch "hide.sidebar.debug.watch-edit" "Edit watch"
  remove<-registerWatch "hide.sidebar.debug.watch-remove" "Remove watch"
  let watchAction command captured=P.treeAction registry command captured (\_ value->pure value)
      watchesRoot=P.NodeDef (P.NodeInfo watchesId "Watches" "" True Nothing) Nothing [P.ActionMenu "Add watch…" (watchAction add AddDebugWatch)]
      watchChildren _ (P.ChildRequest parent cursor)
        | parent/=watchesId || cursor/=Nothing=pure (Left (CommandRejected "Watch node expired."))
        | otherwise=do
            (_,entries)<-debuggerWatches runtime
            let rows=map (watchNode (watchAction edit) (watchAction remove)) (M.toList entries)
                emptyRow=P.NodeDef (P.NodeInfo (ident "watch-empty") "No watches; add an expression" "" False Nothing) Nothing []
            pure (Right (P.NodePage (if null rows then [emptyRow] else rows) Nothing))
  watches<-P.registerTree registry "hide.sidebar.watches" watchesRoot watchChildren >>= either (ioError . userError . show) pure
  publishTreeFromHost host watches
  use (DebuggerSidebar (P.treeReference provider) selected shownSession (P.treeReference watches) watchRevisionSeen)
  where hidden=Codec Null (const (Left "Debug sidebar arguments are host-captured.")) (const Null)

-- | /O(1)/ stopped projection comparison. Scoped refresh performs metadata
-- invalidation; cached indexed rows retain selection and viewport anchors.
tickDebuggerSidebar :: DebuggerSidebar -> SidebarHost -> Debugger -> Desktop -> IO Desktop
tickDebuggerSidebar (DebuggerSidebar owner ref shown watchOwner watchSeen) host runtime original=do
  (revision,_)<-debuggerWatches runtime
  oldRevision<-readIORef watchSeen
  d<-if oldRevision==Just revision then pure original else do
    writeIORef watchSeen (Just revision)
    refreshTreeFromHost host watchOwner watchesId original
  epoch<-debuggerSidebarEpoch runtime
  previous<-readIORef ref
  refreshed<-if previous==epoch then pure d else do
    writeIORef ref epoch
    refreshTreeFromHost host owner rootId d
  session<-debuggerSidebarSession runtime
  lastSession<-readIORef shown
  case (session,sideTree refreshed) of
    (Just identSession,Just tree) | Just identSession/=lastSession,Just root<-M.lookup (NodeKey owner rootId) (treeNodes tree)->do
      writeIORef shown session
      if stateExpanded root then pure refreshed else do
        let (opened,request)=requestChildren (nodeHit (NodeKey owner rootId) root) Nothing tree
        maybe (pure refreshed) (\value->snd <$> sidebarEffects host (\current _->pure (False,current)) refreshed {sideTree=Just opened} [LoadTree value Menu.HumanMenu]) request
    _->pure refreshed

watchesId :: P.NodeId
watchesId=ident "watches"

watchNode :: (DebugSidebarRequest -> P.TreeAction SidebarContext SidebarReply)
  -> (DebugSidebarRequest -> P.TreeAction SidebarContext SidebarReply)
  -> (Int,DebuggerWatch) -> P.NodeDef SidebarContext SidebarReply
watchNode editAction removeAction (key,entry)=P.NodeDef
  (P.NodeInfo (node "watch" [key]) title "" False (watchOrigin entry)) (Just edit)
  [P.ActionMenu "Remove watch" (removeAction (RemoveDebugWatch key (watchRevision entry)))]
  where
    edit=editAction (EditDebugWatch key (watchRevision entry))
    -- An originless private role is conservatively hidden, while canonical
    -- source origins use the ordinary shared row privacy projection.
    expression=if watchPrivate entry && watchOrigin entry==Nothing then "Private watch" else watchExpression entry
    title=bounded (expression<>" [pending; evaluate explicitly]")

pageOffset :: Maybe Text -> Maybe Int
pageOffset token=do
  offset<-maybe (Just 0) (readMaybe . T.unpack) token
  if offset>=0 && offset<=32768 then Just offset else Nothing

target :: Int -> P.NodeId -> Maybe DebugPageTarget
target epoch value
  | value==rootId=Just DebugThreads
  | otherwise=case T.splitOn ":" (P.nodeIdText value) of
      kind:parts | Just numbers<-traverse (readMaybe . T.unpack) parts->case (kind,numbers) of
        ("thread",[stop,tid]) | stop==epoch->Just (DebugStack tid)
        ("frame",[stop,tid,fid]) | stop==epoch->Just (DebugScopes tid fid)
        ("scope",[stop,tid,fid,reference,_]) | stop==epoch->Just (DebugVariables tid fid reference)
        ("value",[stop,tid,fid,_,reference,_]) | stop==epoch,reference>0->Just (DebugVariables tid fid reference)
        _->Nothing
      _->Nothing

makeNode :: (Int -> Int -> Int -> P.TreeAction SidebarContext SidebarReply)
  -> Int -> DebugPageTarget -> Int -> Value -> P.NodeDef SidebarContext SidebarReply
makeNode action epoch request offset row=case request of
  DebugThreads->plain (node "thread" [epoch,integer "id" row]) (label "name" row) True
  DebugStack tid->
    let fid=integer "id" row
        selected=action epoch tid fid
    in P.NodeDef (P.NodeInfo (node "frame" [epoch,tid,fid]) (bounded (label "name" row<>"  :"<>number (integer "line" row))) "" True (T.unpack <$> ((field "source" row :: Maybe Value) >>= field "path")))
      Nothing [P.ActionMenu "Go to source" selected]
  DebugScopes tid fid->plain (node "scope" [epoch,tid,fid,integer "variablesReference" row,offset]) (label "name" row) (integer "variablesReference" row>0)
  DebugVariables tid fid parent->
    let reference=integer "variablesReference" row
        lazy=maybe False (flag "lazy") (field "presentationHint" row)
        description=label "name" row<>" = "<>label "value" row<>if lazy then " [lazy; explicit evaluation required]" else ""
        -- DAP values have no row identity; position within this immutable page
        -- plus parent/reference distinguishes equal labels without payload equality.
        position=offset
    in plain (node "value" [epoch,tid,fid,parent,reference,position]) description (reference>0 && not lazy)
  where plain key title branch=P.NodeDef (P.NodeInfo key (bounded title) "" branch Nothing) Nothing []

continuation :: DebugPageTarget -> Int -> Value -> Int -> Maybe Text
continuation request offset body count
  | count<128=Nothing
  | DebugStack{}<-request,Just total<-(field "totalFrames" body :: Maybe Int),offset+count>=total=Nothing
  | DebugThreads<-request=Nothing
  | DebugScopes{}<-request=Nothing
  | DebugVariables{}<-request=Nothing -- Adapters without a paging contract expose the bounded first page.
  | otherwise=Just (number (offset+count))
rowField :: DebugPageTarget -> Key
rowField request=case request of DebugThreads->"threads"; DebugStack{}->"stackFrames"; DebugScopes{}->"scopes"; DebugVariables{}->"variables"
field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.:key))
items :: Key -> Value -> [Value]
items key body=maybe [] id (field key body)
integer :: Key -> Value -> Int
integer key=maybe 0 id . field key
flag :: Key -> Value -> Bool
flag key=maybe False id . field key
label :: Key -> Value -> Text
label key=maybe "" id . field key
bounded :: Text -> Text
bounded=T.take 256 . T.map (\c->if c<' ' then ' ' else c)
number :: Int -> Text
number=T.pack . show
