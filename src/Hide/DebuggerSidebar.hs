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
import Hide.Debugger (Debugger,debuggerSidebarEpoch,debuggerSidebarSession,debuggerSidebarRead,debuggerWatches,withDebuggerWatchProvider)
import Hide.DebuggerSidebarTypes
import Hide.GuestAccess (protectedFilePath)
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
  evaluate<-registerWatch "hide.sidebar.debug.watch-evaluate" "Evaluate watch"
  force<-registerWatch "hide.sidebar.debug.watch-force" "Force lazy watch"
  let watchAction command captured=P.treeAction registry command captured (\_ value->pure value)
      watchesRoot=P.NodeDef (P.NodeInfo watchesId "Watches" "" True Nothing) Nothing [P.ActionMenu "Add watch…" (watchAction add AddDebugWatch)]
      watchChildren ctx (P.ChildRequest parent cursor)=do
        (_,chosenWatch,entries)<-debuggerWatches runtime
        case (parent,pageOffset cursor) of
          (parentId,Just 0) | parentId==watchesId->do
            let rows=map (watchNode (watchAction edit) (watchAction remove) (watchAction evaluate) (watchAction force) chosenWatch (sidebarPrivatePaths ctx)) (M.toList entries)
                emptyRow=P.NodeDef (P.NodeInfo (ident "watch-empty") "No watches; add an expression" "" False Nothing) Nothing []
            pure (Right (P.NodePage (if null rows then [emptyRow] else rows) Nothing))
          (_,Just offset) | Just (key,revision,receipt,reference)<-watchTarget chosenWatch entries parent->do
            let epoch=case receipt of WatchFrame _ stop _ _ _->stop
            result<-debuggerSidebarRead runtime (DebugPageRequest epoch (DebugWatchVariables key revision receipt reference) offset)
            pure $ case result of
              Left err->Left (CommandRejected err)
              Right body->Right (P.NodePage [watchChild (watchAction force) key revision receipt reference offset index row (sidebarPrivatePaths ctx) entry | (index,row)<-zip [0..] (take 128 (items "variables" body)),Just entry<-[M.lookup key entries]] (if flag "hasMore" body then Just (number (offset+128)) else Nothing))
          _->pure (Left (CommandRejected "Watch node expired."))
  watches<-P.registerTree registry "hide.sidebar.watches" watchesRoot watchChildren >>= either (ioError . userError . show) pure
  publishTreeFromHost host watches
  withDebuggerWatchProvider runtime watches $ use (DebuggerSidebar (P.treeReference provider) selected shownSession (P.treeReference watches) watchRevisionSeen)
  where hidden=Codec Null (const (Left "Debug sidebar arguments are host-captured.")) (const Null)

-- | /O(1)/ stopped projection comparison. Scoped refresh performs metadata
-- invalidation; cached indexed rows retain selection and viewport anchors.
tickDebuggerSidebar :: DebuggerSidebar -> SidebarHost -> Debugger -> Desktop -> IO Desktop
tickDebuggerSidebar (DebuggerSidebar owner ref shown watchOwner watchSeen) host runtime original=do
  (revision,_,_)<-debuggerWatches runtime
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
  -> (DebugSidebarRequest -> P.TreeAction SidebarContext SidebarReply)
  -> (DebugSidebarRequest -> P.TreeAction SidebarContext SidebarReply)
  -> Maybe WatchFrame -> [FilePath] -> (Int,DebuggerWatch) -> P.NodeDef SidebarContext SidebarReply
watchNode editAction removeAction evaluateAction forceAction selected privatePaths (key,entry)=P.NodeDef
  (P.NodeInfo (node "watch" [key]) title "" branch origin) (if branch then Nothing else Just edit)
  ([P.ActionMenu "Edit watch…" edit | branch]++[P.ActionMenu "Remove watch" (removeAction (RemoveDebugWatch key revision))]++
    [P.ActionMenu "Evaluate watch" (evaluateAction (EvaluateDebugWatch key revision receipt)) | receipt<-maybeToList selected,not loading]++
    [P.ActionMenu "Force lazy watch" (forceAction (ForceDebugWatch key revision receipt reference)) | WatchResult receipt _ reference True _<-[watchValue entry],selected==Just receipt,reference>0])
  where
    revision=watchRevision entry
    edit=editAction (EditDebugWatch key revision)
    loading=case watchValue entry of WatchLoading{}->True; _->False
    branch=case watchValue entry of WatchResult receipt _ reference False _->selected==Just receipt && reference>0; _->False
    origin=watchResource privatePaths entry
    description=case watchValue entry of
      WatchPending->" [pending; evaluate explicitly]"
      WatchLoading{}->" [evaluating…]"
      WatchError _ err _->" [error: "<>err<>"]"
      WatchStale value _->" = "<>value<>" [stale; evaluate explicitly]"
      WatchResult _ value _ lazy _->" = "<>value<>if lazy then " [lazy; explicit Force]" else ""
    title=if watchPrivate entry then "Private watch" else bounded (watchExpression entry<>description)
    maybeToList Nothing=[]
    maybeToList (Just value)=[value]

watchTarget :: Maybe WatchFrame -> M.Map Int DebuggerWatch -> P.NodeId -> Maybe (Int,Int,WatchFrame,Int)
watchTarget selected entries parent=case T.splitOn ":" (P.nodeIdText parent) of
  ["watch",identText] | Just key<-readMaybe (T.unpack identText),Just entry<-M.lookup key entries,
      WatchResult receipt _ reference False _<-watchValue entry,selected==Just receipt,reference>0->Just (key,watchRevision entry,receipt,reference)
  "watchvalue":parts | Just [key,revision,epoch,selection,tid,fid,_,reference,_]<-traverse (readMaybe . T.unpack) parts,
      Just receipt@(WatchFrame _ epochNow selectionNow tidNow fidNow)<-selected,(epoch,selection,tid,fid)==(epochNow,selectionNow,tidNow,fidNow),Just entry<-M.lookup key entries,watchRevision entry==revision,reference>0->Just (key,revision,receipt,reference)
  _->Nothing

watchChild :: (DebugSidebarRequest -> P.TreeAction SidebarContext SidebarReply)
  -> Int -> Int -> WatchFrame -> Int -> Int -> Int -> Value -> [FilePath] -> DebuggerWatch -> P.NodeDef SidebarContext SidebarReply
watchChild forceAction key revision receipt@(WatchFrame _ epoch selection tid fid) parent offset index row privatePaths entry=P.NodeDef
  (P.NodeInfo (node "watchvalue" [key,revision,epoch,selection,tid,fid,parent,reference,offset+index]) title "" (reference>0 && not lazy) origin) Nothing
  [P.ActionMenu "Force lazy child" (forceAction (ForceDebugWatchChild key revision receipt parent offset index reference)) | lazy,reference>0]
  where
    reference=integer "variablesReference" row
    lazy=maybe False (flag "lazy") (field "presentationHint" row)
    title=if watchPrivate entry then "Private value" else bounded (label "name" row<>" = "<>label "value" row<>if lazy then " [lazy; explicit evaluation required]" else "")
    origin=watchResource privatePaths entry

watchResource :: [FilePath] -> DebuggerWatch -> Maybe FilePath
watchResource privatePaths entry=case filter (protectedFilePath privatePaths) origins++origins of
  path:_->Just path
  []->Nothing
  where
    origins=maybeToList (watchOrigin entry)++case watchValue entry of
      WatchResult _ _ _ _ origin->maybeToList origin
      WatchStale _ origin->maybeToList origin
      WatchError _ _ origin->maybeToList origin
      _->[]
    maybeToList Nothing=[]
    maybeToList (Just value)=[value]

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
  DebugWatchVariables{}->plain (node "invalid-watch-page" [epoch,offset]) "Watch page belongs to Watches" False
  where plain key title branch=P.NodeDef (P.NodeInfo key (bounded title) "" branch Nothing) Nothing []

continuation :: DebugPageTarget -> Int -> Value -> Int -> Maybe Text
continuation request offset body count
  | count<128=Nothing
  | DebugStack{}<-request,Just total<-(field "totalFrames" body :: Maybe Int),offset+count>=total=Nothing
  | DebugThreads<-request=Nothing
  | DebugScopes{}<-request=Nothing
  | DebugVariables{}<-request=if flag "hasMore" body then Just (number (offset+count)) else Nothing
  | otherwise=Just (number (offset+count))
rowField :: DebugPageTarget -> Key
rowField request=case request of DebugThreads->"threads"; DebugStack{}->"stackFrames"; DebugScopes{}->"scopes"; DebugVariables{}->"variables"; DebugWatchVariables{}->"variables"
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
