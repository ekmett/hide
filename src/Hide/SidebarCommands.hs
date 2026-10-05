{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
-- | Session owner for scoped sidebar providers. Pages, projections and prepared
-- file opens run on workers. Owner ticks adopt only exact current requests; the
-- cached viewport never evaluates a provider or inspects buffer payloads.
module Hide.SidebarCommands
  ( SidebarHost, SidebarContext(..), SidebarReply(..), withSidebarCommands
  , sidebarRegistry, publishTreeFromHost, retireTreeFromHost, sidebarEffects
  , tickSidebar, refreshTreeFromHost, initializeSidebar, prepareSidebarFile, publishFormRefreshFromHost
  ) where

import Control.Concurrent.Async (Async,async,cancel,poll)
import qualified Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq (force)
import Control.Exception (bracket,evaluate,displayException,mask,onException)
import Control.Monad (foldM,forever,forM,filterM,when)
import Data.Aeson (Value(Null))
import Data.IORef
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (Text)
import System.Directory (canonicalizePath)
import System.FilePath ((</>),takeExtension)
import Data.Char (toLower)
import System.Mem.StableName
import Text.Read (readMaybe)
import Hide.Browser
import Hide.Buffer (captureDirty,snapshotDirty,bufferLineChanges,prepareBuffer,Selection(..))
import Hide.Plugin.BufferHost (ContentVersion,captureVersion,versionCurrent)
import Hide.Files (FileState(..),loadFile)
import Hide.GuestAccess (protectedPath,protectedFilePath,protectedBuffer)
import Hide.Links (LinkResult,applyLink)
import Hide.PluginWindowHost (adoptWindowUpdate)
import qualified Hide.Plugin.Window as PluginWindow
import Hide.Model
import Hide.DebuggerSidebarTypes
import qualified Hide.AgentHub
import qualified Hide.Plugin.Form as Form
import Hide.AgentSidebarTypes
import Hide.SessionSidebarTypes
import Hide.Sidebar
import Hide.Plugin.Command
import qualified Hide.Plugin.Tree as P
import qualified Hide.Plugin.Menu as Menu

-- | Captured host policy; extension labels and paths grant no authority.
data SidebarContext = SidebarContext
  { sidebarOrigin :: !Menu.MenuOrigin, sidebarPrivatePaths :: ![FilePath]
  , sidebarColumns :: !Int, sidebarOpened :: !(Maybe (FilePath,Int,Int,ContentVersion)) }
data SidebarReply = SidebarForm !(Form.PreparedInputForm SidebarContext SidebarReply) | SidebarSession !SessionSidebarRequest | SidebarExisting !FilePath !Int !Int !ContentVersion | SidebarDocument !FilePath !Document | SidebarPrepared !LinkResult | SidebarAgent !AgentSidebarRequest | SidebarDebug !DebugSidebarRequest | SidebarWindow !PluginWindow.WindowUpdate

data ChildJob = ChildJob !TreeRequest !Menu.MenuOrigin !(Async (Either CommandError (P.PreparedPage SidebarContext SidebarReply))) !Bool
data ActionJob = ActionJob ![P.TreeHit] !CommandRef !Menu.MenuOrigin !Int !(Async (Either CommandError SidebarReply)) !Bool
  | FormJob !Form.FormRef !(Async (Either CommandError SidebarReply)) !Bool
data Publication = TreePublication !(P.TreeProvider SidebarContext SidebarReply) | FormRefresh !Form.InputFormUpdate
data FilesProvider = FilesProvider !(P.TreeProvider SidebarContext SidebarReply) !FilePath !CommandRef !(IORef (M.Map P.NodeId FilePath,M.Map FilePath P.NodeId,Int,M.Map FilePath [Entry]))
data State = State
  { providers :: !(M.Map P.TreeRef (P.TreeProvider SidebarContext SidebarReply))
  , definitions :: !(M.Map NodeKey (P.NodeDef SidebarContext SidebarReply))
  , jobs :: ![ChildJob], waiting :: ![(TreeRequest,Menu.MenuOrigin)]
  , projection :: !(Maybe (Integer,Async (Projection,M.Map NodeKey (P.NodeDef SidebarContext SidebarReply),RecoveryProjection)))
  , actionJob :: !(Maybe ActionJob), filesProvider :: !(Maybe FilesProvider)
  , badgeStamp :: !(Maybe (StableName (M.Map Int Document)))
  , badgeJob :: !(Maybe (StableName (M.Map Int Document),Async (M.Map FilePath (Bool,Int,Int))))
  , sidebarRevision :: !Integer, inputForm :: !(Maybe (Form.PreparedInputForm SidebarContext SidebarReply)) }
-- Prepared alongside visible rows; owner applies at most four branch changes.
data RecoveryProjection = RecoveryProjection !(Maybe SidebarHints)
  ![(P.TreeHit,[P.TreeHit],Bool)] !(Maybe RowKey) !(Maybe RowKey)
data Cancellation = forall a. Cancellation (Async a)
data SidebarHost = SidebarHost !(Registry SidebarContext) !(IORef State)
  !(TBQueue Publication) !(TBQueue Cancellation) !(Async ())

sidebarRegistry :: SidebarHost -> Registry SidebarContext
sidebarRegistry (SidebarHost registry _ _ _ _)=registry
withSidebarCommands :: (SidebarHost -> IO a) -> IO a
withSidebarCommands use=withRegistry $ \registry->bracket (acquire registry) close use
  where
    acquire registry=do
      state<-newIORef (State M.empty M.empty [] [] Nothing Nothing Nothing Nothing Nothing 0 Nothing)
      publications<-newTBQueueIO 32
      cancellation<-newTBQueueIO 32
      canceller<-async (forever (do Cancellation worker<-atomically (readTBQueue cancellation); cancel worker))
      pure (SidebarHost registry state publications cancellation canceller)
    close (SidebarHost _ ref _ _ canceller)=do
      state<-readIORef ref
      mapM_ (\(ChildJob _ _ worker _)->cancel worker) (jobs state)
      mapM_ (cancel . snd) (projection state)
      mapM_ (cancel . actionWorker) (actionJob state)
      mapM_ (Form.retireForm . Form.formReference) (inputForm state)
      mapM_ (cancel . snd) (badgeJob state)
      cancel canceller

-- | Register/prepare metadata outside the owner, then publish an ordered bounded
-- delta. Backpressure applies to the registration worker, never an input tick.
publishTreeFromHost :: SidebarHost -> P.TreeProvider SidebarContext SidebarReply -> IO ()
publishTreeFromHost (SidebarHost _ _ queue _ _) provider=atomically (writeTBQueue queue (TreePublication provider))
-- | Queue metadata for an already installed exact form. This cannot open a
-- modal or grant submission authority; ordered transport reuses the root queue.
publishFormRefreshFromHost :: SidebarHost -> Form.InputFormUpdate -> IO ()
publishFormRefreshFromHost (SidebarHost _ _ queue _ _) prepared=atomically (writeTBQueue queue (FormRefresh prepared))
actionWorker :: ActionJob -> Async (Either CommandError SidebarReply)
actionWorker (ActionJob _ _ _ _ worker _)=worker
actionWorker (FormJob _ worker _)=worker
-- | Withdrawal belongs to the session owner, so queued/late results cannot race
-- adoption. Cancellation is scheduled to a worker and never waits under UI lock.
retireTreeFromHost :: SidebarHost -> P.TreeRef -> Desktop -> IO Desktop
retireTreeFromHost (SidebarHost _ ref _ _ _) owner d=do
  state<-readIORef ref
  mapM_ P.retireTree (M.lookup owner (providers state))
  writeIORef ref state {providers=M.delete owner (providers state)}
  pure d {sideTree=fmap (removeRoot owner) (sideTree d),contextMenu=Nothing,contextTarget=Nothing}

context :: Menu.MenuOrigin -> Desktop -> SidebarContext
context origin d=SidebarContext origin (guestPrivatePaths d) (max 20 (min 76 (fst (screenSize d)-treeWidthOf d-4))) Nothing
metadata :: P.NodeDef c r -> (P.NodeInfo,Maybe CommandRef,[(Text,P.TreeMenuTarget)])
metadata node=(P.nodeInfo node,fmap P.actionReference (P.nodeAction node),map P.menuTarget (P.nodeMenus node))
addProvider :: P.TreeProvider SidebarContext SidebarReply -> Sidebar -> Sidebar
addProvider provider tree=let (info,action,actions)=metadata (P.treeRoot provider)
  in addRoot (P.treeReference provider) info action actions tree

-- | Prepare a captured file action on its worker. Reuse the existing buffer when
-- its captured identity is still current; adoption performs that check. New reads
-- recheck canonical privacy before preparing a document, never under the UI lock.
prepareSidebarFile :: SidebarContext -> FilePath -> IO (Either CommandError SidebarReply)
prepareSidebarFile ctx path=case sidebarOpened ctx of
    Just (captured,wid,bid,version) | captured==path->pure (Right (SidebarExisting path wid bid version))
    _->do
      resolved<-canonicalizePath path
      if sidebarOrigin ctx==Menu.AgentMenu && protectedFilePath (sidebarPrivatePaths ctx) resolved
        then pure (Left (CommandRejected "Agent file target is protected.")) else do
          result<-loadFile resolved
          case result of
            Left err->pure (Left (CommandRejected (T.pack err)))
            Right (file,buffer)->do
              _<-evaluate (prepareBuffer buffer)
              doc<-evaluate (newDocument buffer (Just file))
              pure (Right (SidebarDocument (filePath file) doc))

-- Files is declared through precisely the public provider/action route.
createFiles :: SidebarHost -> FilePath -> IO FilesProvider
createFiles host root=do
  let registry=sidebarRegistry host
  rootId<-either (ioError . userError . T.unpack) pure (P.nodeId "root")
  cache<-newIORef (M.singleton rootId root,M.singleton root rootId,1,M.empty)
  open<-either (ioError . userError . show) pure =<< registerCommand registry (CommandDef "hide.sidebar.files.open" "Open file" codec codec prepareSidebarFile)
  provider<-either (ioError . userError . show) pure =<< P.registerTree registry "hide.sidebar.files"
    (P.NodeDef (P.NodeInfo rootId "Files" "" True (Just root)) Nothing [])
    (\ctx (P.ChildRequest ident cursor)->do
      (paths,_,_,cached)<-readIORef cache
      case M.lookup ident paths of
        Nothing->pure (Left (CommandRejected "Files node is no longer known."))
        Just path->do
          resolved<-canonicalizePath path
          if sidebarOrigin ctx==Menu.AgentMenu && protectedFilePath (sidebarPrivatePaths ctx) resolved
            then pure (Left (CommandRejected "Agent directory target is protected.")) else case cursor of
              Just token | maybe True (\offset->offset<0 || offset>32768) (readMaybe (T.unpack token)::Maybe Int)->pure (Left (InvalidArguments "Invalid Files page cursor."))
              _->do
                listing<-case M.lookup resolved cached of
                  Just entries->pure (Right (resolved,entries))
                  Nothing->readDirectory resolved "*"
                case listing of
                  Left err->pure (Left (CommandRejected (T.pack err)))
                  Right (base,entries)->do
                    let visible=filter ((/="..").entryName) entries
                        offset=maybe 0 id (cursor >>= readMaybe . T.unpack)
                        page=take 128 (drop offset visible)
                    (_,known,_,_)<-readIORef cache
                    let missing=[() | entry<-page,not (M.member (base </> T.unpack (entryName entry)) known)]
                    if M.size known+length missing>32768 then pure (Left (CommandRejected "Files identity budget reached; remount Files to refresh its scope.")) else
                     if length (take 32769 visible)>32768 then pure (Left (CommandRejected "Directory exceeds the 32768-entry sidebar budget.")) else do
                      values<-forM page $ \entry->do
                        let resource=base </> T.unpack (entryName entry)
                        allocated<-atomicModifyIORef' cache $ \(byId,byPath,next,dirs)->case M.lookup resource byPath of
                          Just node->((byId,byPath,next,dirs),Right node)
                          Nothing | M.size byPath>=32768->((byId,byPath,next,dirs),Left "Files identity budget reached.")
                                  | otherwise->case P.nodeId ("file-"<>T.pack (show next)) of
                                      Left failure->((byId,byPath,next,dirs),Left failure)
                                      Right node->((M.insert node resource byId,M.insert resource node byPath,next+1,dirs),Right node)
                        node<-either (ioError . userError . T.unpack) pure allocated
                        pure (P.NodeDef (P.NodeInfo node (T.take 256 (T.filter (>= ' ') (entryName entry))) (if entryDirectory entry then "📁" else "📄")
                          (entryDirectory entry) (Just resource))
                          (if entryDirectory entry then Nothing else Just (P.treeAction registry open resource (\_ ->pure)))
                          [P.ResourceMenu "Open" resource "" | not (entryDirectory entry),map toLower (takeExtension resource) `elem` [".md",".markdown",".png",".jpg",".jpeg",".gif",".webp",".bmp",".svg",".pdf"]])
                      -- Cache bounded directory pages at the filesystem owner. Old
                      -- directories may be re-enumerated after their cache expires.
                      atomicModifyIORef' cache $ \(a,b,c,dirs)->((M.insert ident base a,M.insert base ident b,c,M.insert base visible (if M.size dirs>=32 then M.empty else dirs)),())
                      pure (Right (P.NodePage values (if length (drop (offset+128) visible)>0 then Just (T.pack (show (offset+128))) else Nothing))))
  pure (FilesProvider provider root (commandRef open) cache)
  where codec=Codec Null (const (Left "Files arguments are host-captured.")) (const Null)

-- | Mount initial Files and prepare its first projection outside the UI boundary.
-- Recovery restores hints only; registration identities are always fresh.
initializeSidebar :: SidebarHost -> Desktop -> IO Desktop
initializeSidebar host d=do
  (_,mounted)<-sidebarEffects host (\x _->pure (False,x)) d []
  await mounted
  where
    await current=do
      next<-tickSidebar host (\value _->pure (False,value)) current
      case sideTree next of
        Just tree | treeProjectionRevision tree==treeRevision tree && all ready (M.elems (treeNodes tree))->pure next
        Nothing->pure next
        _->do
          state<-readState host
          mapM_ (\(ChildJob _ _ worker _)->Control.Concurrent.Async.wait worker >> pure ()) (jobs state)
          mapM_ (\(_,worker)->Control.Concurrent.Async.wait worker >> pure ()) (projection state)
          await next
    ready node=case stateLoad node of Loading{}->False; _->True
readState :: SidebarHost -> IO State
readState (SidebarHost _ ref _ _ _)=readIORef ref

sidebarEffects :: SidebarHost -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
sidebarEffects host core d effects=do
  mounted<-mount host d
  foldM step (False,mounted) effects >>= \(quit,next)->(quit,) <$> (mount host next >>= rememberSidebar host)
  where
    step result@(True,_) _=pure result
    step (_,current) effect=case effect of
      LoadTree request origin->(False,) <$> enqueue host request origin current
      InvokeTree trace reference origin->(False,) <$> invokeAction host trace reference origin current
      SubmitInputForm reference text origin->(False,) <$> submitForm host reference text origin current
      RetireInputForm reference->do
        Form.retireForm reference
        pure (False,current)
      RefreshTree path entries->(False,) <$> refreshFiles host path entries current
      _->core current [effect]

mount :: SidebarHost -> Desktop -> IO Desktop
mount host@(SidebarHost _ ref _ _ _) d=case sideTree d of
  Nothing->pure d
  Just visible->do
    state<-readIORef ref
    live<-filterM P.treeCurrent (M.elems (providers state))
    let epoch=sidebarRevision state+1
        basis=if M.null (treeNodes visible) then visible {treeEpoch=epoch,treeRevision=epoch} else visible
        tree=foldl (flip addProvider) basis live
    case filesProvider state of
      Just (FilesProvider provider root _ _) | root==treeRoot tree->do
        let key=NodeKey (P.treeReference provider) (P.infoId (P.nodeInfo (P.treeRoot provider)))
            next=if M.member (P.treeReference provider) (providers state) then addProvider provider tree else tree
        if M.member key (treeNodes visible) then pure d {sideTree=Just next}
          else startRoot provider next
      _->do
        -- This path is startup/directory selection, which already belongs to the
        -- host's file effect owner. Registration and root validation are bounded.
        mapM_ (\(FilesProvider provider _ command _)->P.retireTree provider >> retireCommand (sidebarRegistry host) command) (filesProvider state)
        created@(FilesProvider provider _ _ _)<-createFiles host (treeRoot tree)
        let owner=P.treeReference provider
            withdrawn=maybe tree (\(FilesProvider old _ _ _)->removeRoot (P.treeReference old) tree) (filesProvider state)
            fresh=addProvider provider withdrawn {treeAgentRefs=[owner]}
            key=NodeKey owner (P.infoId (P.nodeInfo (P.treeRoot provider)))
            defs=M.insert key (P.treeRoot provider) (definitions state)
        writeIORef ref state {filesProvider=Just created,providers=M.insert owner provider (providers state),definitions=defs}
        startRoot provider fresh
  where
    startRoot provider tree=let key=NodeKey (P.treeReference provider) (P.infoId (P.nodeInfo (P.treeRoot provider)))
      in case M.lookup key (treeNodes tree) of
        Nothing->pure d {sideTree=Just tree,status="Sidebar provider/node budget reached; Files cannot be mounted."}
        Just node->let (opened,request)=requestChildren (nodeHit key node) Nothing tree
          in maybe (pure d {sideTree=Just opened}) (\value->enqueue host value Menu.HumanMenu d {sideTree=Just opened}) request

-- A fresh visible tree starts above every prior publication revision. Root and
-- request generations inherit that epoch, so reopening cannot alias old traces,
-- queued child tokens or projection results. Remembering is constant-time.
rememberSidebar :: SidebarHost -> Desktop -> IO Desktop
rememberSidebar (SidebarHost _ ref _ _ _) d=do
  mapM_ (\tree->modifyIORef' ref (\state->state {sidebarRevision=max (sidebarRevision state) (treeRevision tree)})) (sideTree d)
  pure d

enqueue :: SidebarHost -> TreeRequest -> Menu.MenuOrigin -> Desktop -> IO Desktop
enqueue (SidebarHost _ ref _ _ _) request origin d=case sideTree d of
  Just tree | requestCurrent request tree->do
    state<-readIORef ref
    let P.TreeHit owner _ _=requestHit request
        permitted=origin==Menu.HumanMenu || owner `elem` treeAgentRefs tree && maybe False (not . protectedPath d) (P.infoResource =<< fmap stateInfo (nodeAt (requestHit request) tree))
        duplicate=any (\(ChildJob active _ _ _)->active==request) (jobs state) || any ((==request).fst) (waiting state)
    if not permitted then pure d {sideTree=Just (failRequest request "Protected sidebar target." tree)}
    else if duplicate then pure d else if length (waiting state)>=64 then pure d {sideTree=Just (failRequest request "Sidebar loader is busy; retry." tree)} else do
      writeIORef ref state {waiting=waiting state++[(request,origin)]}
      pure d
  _->pure d

invokeAction :: SidebarHost -> [P.TreeHit] -> CommandRef -> Menu.MenuOrigin -> Desktop -> IO Desktop
invokeAction (SidebarHost _ ref _ _ _) trace reference origin d=case (trace,sideTree d) of
  (hit:_,Just tree) | hitCurrent trace tree && dialog d==Nothing->do
    state<-readIORef ref
    let P.TreeHit owner _ _=hit
        action=do
          node<-M.lookup (keyOf hit) (definitions state)
          find ((==reference).P.actionReference)
            (maybe [] pure (P.nodeAction node)++P.menuActions node)
        allowed=origin==Menu.HumanMenu || owner `elem` treeAgentRefs tree &&
          maybe False (not . protectedPath d) (P.infoResource . stateInfo =<< nodeAt hit tree)
    live<-maybe (pure False) P.treeCurrent (M.lookup owner (providers state))
    case (actionJob state,action) of
      (Nothing,Just command) | live && allowed->do
        ctx<-captureActionContext origin trace d
        worker<-async (P.invokeTreeAction command ctx)
        writeIORef ref state {actionJob=Just (ActionJob trace reference origin (sidebarColumns ctx) worker False)}
        pure d {status="Opening sidebar target…"}
      _->pure d {status="Sidebar action is stale, protected or busy."}
  _->pure d {status="Sidebar action expired."}

-- Capture just one existing file's immutable identity at admission. The worker
-- never reloads it from disk; version/path/window checks protect late adoption.
captureActionContext :: Menu.MenuOrigin -> [P.TreeHit] -> Desktop -> IO SidebarContext
captureActionContext origin trace d=do
  let target=case (trace,sideTree d) of
        (hit:_,Just tree)->P.infoResource . stateInfo =<< nodeAt hit tree
        _->Nothing
  opened<-case target >>= \path->(path,) <$> find (\(_,doc)->fmap filePath (documentFile doc)==Just path) (M.toList (buffers d)) of
    Just (path,(bid,doc)) | Just window<-find ((==Just bid) . bufferId) (windows d)->do
      version<-captureVersion (documentBuffer doc)
      pure (Just (path,windowId window,bid,version))
    _->pure Nothing
  pure (context origin d) {sidebarOpened=opened}

refreshFiles :: SidebarHost -> FilePath -> [Entry] -> Desktop -> IO Desktop
refreshFiles host@(SidebarHost _ ref _ _ _) path entries d=do
  state<-readIORef ref
  case (filesProvider state,sideTree d) of
    (Just (FilesProvider provider _ _ cache),Just tree)->do
      (_,paths,_,_)<-readIORef cache
      atomicModifyIORef' cache (\(a,b,c,dirs)->((a,b,c,M.insert path entries (if M.size dirs>=32 && not (M.member path dirs) then M.empty else dirs)),()))
      case M.lookup path paths >>= \ident->let key=NodeKey (P.treeReference provider) ident in (key,) <$> M.lookup key (treeNodes tree) of
        Just (NodeKey owner ident,_) -> refreshTreeFromHost host owner ident d
        _->pure d
    _->pure d

-- | Invalidate one expanded scoped node through the ordinary request owner.
-- Nothing runs a provider here; closed nodes load the latest metadata on expansion.
refreshTreeFromHost :: SidebarHost -> P.TreeRef -> P.NodeId -> Desktop -> IO Desktop
refreshTreeFromHost host@(SidebarHost _ ref _ _ _) owner ident d=do
  state<-readIORef ref
  live<-maybe (pure False) P.treeCurrent (M.lookup owner (providers state))
  case sideTree d of
    Just tree | live,Just node<-M.lookup key (treeNodes tree),stateExpanded node->do
      let invalid=collapseNode (nodeHit key node) tree
          current=treeNodes invalid M.! key
          (changed,request)=requestChildren (nodeHit key current) Nothing invalid
      maybe (pure d) (\value->enqueue host value Menu.HumanMenu d {sideTree=Just changed,contextMenu=Nothing,contextTarget=Nothing}) request
    _->pure d
  where key=NodeKey owner ident

-- At most four loads, one projection, one action and one badge computation.
-- Publication drains four deltas per tick. All queues and retained UI nodes have
-- explicit ceilings; slow providers cannot starve input with a recursive drain.
tickSidebar :: SidebarHost -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO Desktop
tickSidebar host@(SidebarHost _ ref publications cancellation _) core initial=do
  validForm<-tickForm host initial
  mounted<-mount host validForm
  published<-foldM (\d _->do
    supplied<-atomically (tryReadTBQueue publications)
    case supplied of
      Nothing->pure d
      Just (FormRefresh update)->refreshForm host update d
      Just (TreePublication provider)->do
        live<-P.treeCurrent provider
        registered<-readIORef ref
        let accepted=live && (M.member (P.treeReference provider) (providers registered) || M.size (providers registered)<32)
        if not accepted then pure d {status=if live then "Sidebar provider budget reached." else status d} else do
          let owner=P.treeReference provider; root=P.treeRoot provider; key=NodeKey owner (P.infoId (P.nodeInfo root))
          modifyIORef' ref (\s->s {providers=M.insert owner provider (providers s),definitions=M.insert key root (definitions s)})
          pure d {sideTree=fmap (addProvider provider) (sideTree d),contextMenu=Nothing,contextTarget=Nothing}) mounted [1..4::Int]
  registered<-readIORef ref
  withdrawn<-foldM (\d (reference,provider)->do
    live<-P.treeCurrent provider
    if live then pure d else retireTreeFromHost host reference d) published (M.toList (providers registered))
  state<-readIORef ref
  (loaded,retained)<-foldM (finishChild state) (withdrawn,[]) (jobs state)
  modifyIORef' ref (\s->s {jobs=reverse retained})
  adopted<-finishAction host core loaded
  projected<-finishProjection host adopted
  restarted<-startLoads host projected
  startProjection host restarted
  badges host restarted >>= rememberSidebar host
  where
    finishChild state (d,keep) job@(ChildJob request origin worker cancelled)=do
      live<-maybe (pure False) P.treeCurrent (M.lookup (owner request) (providers state))
      allowed<-if origin==Menu.HumanMenu then pure True else case filesProvider state of
        Just (FilesProvider provider _ _ cache) | P.treeReference provider==owner request->do
          (paths,_,_,_)<-readIORef cache
          let P.TreeHit _ ident _=requestHit request
          pure (maybe False (not . protectedPath d) (M.lookup ident paths))
        _->pure False
      let current=live && allowed && maybe False (requestCurrent request) (sideTree d) &&
            (origin==Menu.HumanMenu || maybe False (elem (owner request).treeAgentRefs) (sideTree d))
      recovered<-if current then pure d else releaseRequest host request origin d
      completed<-poll worker
      case completed of
        Nothing | not current && not cancelled->do
          queued<-atomically $ do full<-isFullTBQueue cancellation; if full then pure False else writeTBQueue cancellation (Cancellation worker) >> pure True
          pure (recovered,ChildJob request origin worker queued:keep)
        Nothing->pure (recovered,job:keep)
        Just _ | not current->pure (recovered,keep)
        Just result->case sideTree d of
          Nothing->pure (d,keep)
          Just tree->case result of
            Left err->pure (d {sideTree=Just (failRequest request (T.pack (displayException err)) tree)},keep)
            Right (Left err)->pure (d {sideTree=Just (failRequest request (T.pack (show err)) tree)},keep)
            Right (Right page)->case adoptPage request (map metadata (P.pageNodes page)) (P.pageNext page) tree of
              Left err->pure (d {sideTree=Just (failRequest request err tree)},keep)
              Right changed->do
                modifyIORef' ref (\s->s {definitions=foldr (\node->let key=NodeKey (owner request) (P.infoId (P.nodeInfo node))
                  in M.insert key node) (definitions s) (P.pageNodes page)})
                pure (d {sideTree=Just changed},keep)
    owner request=let P.TreeHit value _ _=requestHit request in value

-- Expired ancestry must not leave a row permanently Loading. Release only
-- this exact request token; a newer queued request is never reset. Expanded,
-- current ancestry may retry, while collapse/retirement only clears the state.
releaseRequest :: SidebarHost -> TreeRequest -> Menu.MenuOrigin -> Desktop -> IO Desktop
releaseRequest host request origin d=case sideTree d of
  Just tree | Just node<-M.lookup key (treeNodes tree),stateLoad node==Loading (requestGeneration request) (requestCursor request)->do
    let released=node {stateLoad=Unloaded,stateGeneration=stateGeneration node+1}
        changed=tree {treeNodes=M.insert key released (treeNodes tree),treeRevision=treeRevision tree+1}
        current=hitTrace key changed
    if stateExpanded released && hitCurrent current changed then do
      let (loading,pending)=requestChildren (nodeHit key released) (requestCursor request) changed
      maybe (pure d {sideTree=Just changed}) (\value->enqueue host value origin d {sideTree=Just loading}) pending
    else pure d {sideTree=Just changed}
  _->pure d
  where key=keyOf (requestHit request)

startLoads :: SidebarHost -> Desktop -> IO Desktop
startLoads host@(SidebarHost _ ref _ _ _) original=do
  state<-readIORef ref
  let (begin,rest)=splitAt (max 0 (4-length (jobs state))) (waiting state)
  modifyIORef' ref (\s->s {waiting=rest})
  (d,started)<-foldM (start state) (original,[]) begin
  modifyIORef' ref (\s->s {jobs=jobs s++reverse started})
  pure d
  where
    start state (d,started) (request,origin)=case sideTree d of
      Just tree | requestCurrent request tree->do
        let P.TreeHit owner ident _=requestHit request
        case M.lookup owner (providers state) of
          Just provider->do
            worker<-async (P.loadChildren provider (context origin d) (P.ChildRequest ident (requestCursor request)))
            pure (d,ChildJob request origin worker False:started)
          _->(,started) <$> releaseRequest host request origin d
      _->(,started) <$> releaseRequest host request origin d

finishProjection :: SidebarHost -> Desktop -> IO Desktop
finishProjection host@(SidebarHost _ ref _ _ _) d=do
  state<-readIORef ref
  case projection state of
    Nothing->pure d
    Just (_,worker)->do
      completed<-poll worker
      case completed of
        Nothing->pure d
        Just result->do
          modifyIORef' ref (\s->s {projection=Nothing})
          case (result,sideTree d) of
            (Right (prepared@(Projection revision _ _ _ _),defs,recovery),Just tree) | revision==treeRevision tree->do
              modifyIORef' ref (\s->s {definitions=defs})
              adoptRecovery host recovery d {sideTree=Just (adoptProjection prepared tree)}
            (Left err,_)->pure d {status="Sidebar projection failed: "<>T.pack (displayException err)}
            _->pure d
startProjection :: SidebarHost -> Desktop -> IO ()
startProjection (SidebarHost _ ref _ _ _) d=do
  state<-readIORef ref
  case (projection state,sideTree d) of
    (Nothing,Just tree) | treeProjectionRevision tree/=treeRevision tree->do
      worker<-async $ do
        prepared@(Projection _ _ _ retained _)<-prepareProjection tree
        defs<-evaluate (M.intersection (definitions state) retained)
        recovery<-prepareRecovery prepared tree
        pure (prepared,defs,recovery)
      modifyIORef' ref (\s->s {projection=Just (treeRevision tree,worker)})
    _->pure ()

-- Recovery shares the existing projection worker, never a tick/paint tree scan.
-- Only reachable nodes participate; opaque More cursors remain user-driven.
prepareRecovery :: Projection -> Sidebar -> IO RecoveryProjection
prepareRecovery (Projection _ rows _ retained _) tree=case treeHints tree of
  Nothing->pure (RecoveryProjection Nothing [] Nothing Nothing)
  Just (SidebarHints hints selected top)->do
    let resources=M.fromListWith (\_ first->first)
          [(path,row) | row<-M.elems rows,NodeRow{}<-[rowKey row],Just path<-[P.infoResource (rowInfo row)]]
        differs row expanded=P.infoBranch (rowInfo row) && (expanded || P.infoResource (rowInfo row)==Just (treeRoot tree)) && rowExpanded row/=expanded
        changes=take 4 [(path,row,expanded) | (path,expanded)<-M.toAscList hints,Just row<-[M.lookup path resources],differs row expanded]
        admitted=M.fromList [(path,()) | (path,_,_)<-changes]
        consumed=M.filterWithKey (\path expanded->case M.lookup path resources of
          Nothing->False; Just row->not (differs row expanded) || M.member path admitted) hints
        remaining=M.difference hints consumed
        locate path=rowKey <$> (path >>= (`M.lookup` resources))
        chosen=locate selected; scrolled=locate top
        pending=SidebarHints remaining (if chosen==Nothing then selected else Nothing) (if scrolled==Nothing then top else Nothing)
        next=case pending of SidebarHints values Nothing Nothing | M.null values->Nothing; _->Just pending
        preparedTree=tree {treeRows=rows,treeNodes=retained}
        transitions=[(rowHit row,hitTrace (keyOf (rowHit row)) preparedTree,expanded) | (_,row,expanded)<-changes]
    _<-evaluate (force (M.toAscList remaining,case pending of SidebarHints _ a b->(a,b)))
    mapM_ (\(_,trace,_)->evaluate (length trace)) transitions
    mapM_ evaluate [key | Just key<-[chosen,scrolled]]
    evaluate (RecoveryProjection next transitions chosen scrolled)

adoptRecovery :: SidebarHost -> RecoveryProjection -> Desktop -> IO Desktop
adoptRecovery host@(SidebarHost _ ref _ _ _) (RecoveryProjection hints transitions selected top) original=do
  changed<-foldM restore original {sideTree=fmap (\tree->tree {treeHints=hints}) (sideTree original)} transitions
  chosen<-position True selected changed
  position False top chosen
  where
    live hit=do
      state<-readIORef ref
      let P.TreeHit owner _ _=hit
      maybe (pure False) P.treeCurrent (M.lookup owner (providers state))
    restore d (hit,trace,expanded)=case sideTree d of
      Just tree | hitCurrent trace tree,Just node<-nodeAt hit tree->do
        current<-live hit
        if not current then pure d else
          if not expanded then pure d {sideTree=Just (case M.lookupIndex (stateAddress node) (treeRows tree) of
            Just index->collapseAt index tree; Nothing->tree)}
          else case stateLoad node of
            Loaded{}->pure d {sideTree=Just tree {treeNodes=M.adjust (\value->value {stateExpanded=True}) (keyOf hit) (treeNodes tree),treeRevision=treeRevision tree+1}}
            _->let (opened,request)=requestChildren hit Nothing tree
              in maybe (pure d {sideTree=Just opened}) (\value->enqueue host value Menu.HumanMenu d {sideTree=Just opened}) request
      _->pure d
    position _ Nothing d=pure d
    position choose (Just (NodeRow key)) d=case sideTree d of
      Just tree | Just node<-M.lookup key (treeNodes tree),Just index<-M.lookupIndex (stateAddress node) (treeRows tree)->do
        current<-live (nodeHit key node)
        pure $ if not current then d else d {sideTree=Just (if choose then tree {treeSelected=index} else tree {treeScroll=index})}
      _->pure d
    position _ _ d=pure d

finishAction :: SidebarHost -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO Desktop
finishAction host@(SidebarHost _ ref _ cancellation _) core d=do
  state<-readIORef ref
  case actionJob state of
    Nothing->pure d
    Just (FormJob reference worker cancelled)->finishFormJob host core d reference worker cancelled
    Just (ActionJob trace reference origin columns worker cancelled)->do
      let owner=case trace of P.TreeHit value _ _:_->Just value; _->Nothing
      live<-maybe (pure False) P.treeCurrent (owner >>= (`M.lookup` providers state))
      commandLive<-case trace of
        hit:_->case M.lookup (keyOf hit) (definitions state) of
          Just node->maybe (pure False) P.actionCurrent (find ((==reference).P.actionReference) (maybe [] pure (P.nodeAction node)++P.menuActions node))
          _->pure False
        _->pure False
      let current=not cancelled && live && commandLive && columns==sidebarColumns (context origin d) && dialog d==Nothing && maybe False (\tree->treeFocused tree && hitCurrent trace tree) (sideTree d) &&
            (origin==Menu.HumanMenu || maybe False (\tree->maybe False (`elem` treeAgentRefs tree) owner) (sideTree d))
      completed<-poll worker
      case completed of
        Nothing | not current && not cancelled->do
          queued<-atomically $ do full<-isFullTBQueue cancellation; if full then pure False else writeTBQueue cancellation (Cancellation worker) >> pure True
          modifyIORef' ref (\s->s {actionJob=Just (ActionJob trace reference origin columns worker queued)})
          pure d {status="Sidebar result expired."}
        Nothing->pure d
        Just result->do
          modifyIORef' ref (\s->s {actionJob=Nothing})
          case result of
            Right (Right (SidebarExisting path wid bid version)) | current->adoptExisting origin path wid bid version d
            Right (Right (SidebarDebug request)) | current && origin==Menu.HumanMenu->snd <$> core d [DebugSidebarAction request]
            Right (Right (SidebarSession request)) | current && origin==Menu.HumanMenu->snd <$> core d [SessionSidebarAction request]
            Right (Right (SidebarWindow request)) | current->adoptWindowUpdate origin request d
            Right (Right (SidebarForm prepared)) | current && origin==Menu.HumanMenu->adoptForm host True prepared d
            Right (Right (SidebarAgent request)) | current && origin==Menu.HumanMenu->snd <$> core d [AgentSidebarAction request]
            _->pure $ if not current then d {status="Sidebar result expired."} else case result of
              Left err->d {status="Sidebar action failed: "<>T.pack (displayException err)}
              Right (Left err)->d {status="Sidebar action failed: "<>T.pack (show err)}
              Right (Right SidebarExisting{})->d {status="Sidebar result expired."}
              Right (Right SidebarDebug{})->d {status="Sidebar result expired."}
              Right (Right SidebarSession{})->d {status="Sidebar result expired."}
              Right (Right SidebarWindow{})->d {status="Sidebar result expired."}
              Right (Right SidebarForm{})->d {status="Sidebar form expired."}
              Right (Right SidebarAgent{})->d {status="Sidebar result expired."}
              Right (Right (SidebarPrepared value))->fst (applyLink value d)
              Right (Right (SidebarDocument path doc))
                | origin==Menu.AgentMenu && protectedPath d path->d {status="Sidebar target is now protected."}
                | otherwise->case find (\(_,opened)->fmap filePath (documentFile opened)==Just path) (M.toList (buffers d)) of
                  Just (bid,_) | origin==Menu.AgentMenu && protectedBuffer d bid->d {status="Sidebar target is now private."}
                  Just (bid,_)->maybe d (\window->leave (focusWindow (windowId window) d)) (find ((==Just bid) . bufferId) (windows d))
                  Nothing->leave (addDocument (documentFile doc) (documentBuffer doc) d)
  where leave opened=opened {sideTree=fmap (\tree->tree {treeFocused=False}) (sideTree opened)}

adoptExisting :: Menu.MenuOrigin -> FilePath -> Int -> Int -> ContentVersion -> Desktop -> IO Desktop
adoptExisting origin path wid bid version d=case (find ((==wid).windowId) (windows d),M.lookup bid (buffers d)) of
  (Just window,Just doc) | bufferId window==Just (bid) && fmap filePath (documentFile doc)==Just path->do
    current<-versionCurrent version (documentBuffer doc)
    pure $ if not current || origin==Menu.AgentMenu && (protectedPath d path || protectedBuffer d bid)
      then d {status="Existing sidebar file changed or became private."}
      else let focused=focusWindow wid d in focused {sideTree=fmap (\tree->tree {treeFocused=False}) (sideTree focused)}
  _->pure d {status="Existing sidebar file expired."}

badges :: SidebarHost -> Desktop -> IO Desktop
badges (SidebarHost _ ref _ _ _) d=do
  stamp<-makeStableName $! buffers d
  state<-readIORef ref
  adopted<-case badgeJob state of
    Just (owned,worker)->do
      completed<-poll worker
      case completed of
        Nothing->pure d
        Just result->do
          modifyIORef' ref (\s->s {badgeJob=Nothing})
          pure $ case result of Right prepared | owned==stamp->d {sideTree=fmap (\tree->tree {treeBadges=prepared}) (sideTree d)}; _->d
    Nothing->pure d
  current<-readIORef ref
  when (maybe True (const False) (badgeJob current) && badgeStamp current/=Just stamp) $ do
    snapshots<-forM [(filePath file,documentBuffer doc) | doc<-M.elems (buffers d),Just file<-[documentFile doc]] $ \(path,buffer)->do
      snapshot<-evaluate (captureDirty buffer)
      counts<-evaluate (force (bufferLineChanges buffer))
      pure (path,snapshot,counts)
    worker<-async $ do
      values<-forM snapshots $ \(path,snapshot,(added,deleted))->do
        dirty<-evaluate (snapshotDirty snapshot)
        pure (path,(dirty,added,deleted))
      evaluate (M.fromList values)
    modifyIORef' ref (\s->s {badgeStamp=Just stamp,badgeJob=Just (stamp,worker)})
  pure adopted

-- Form ownership is independent of the source tree once the human accepted it.
formDialog :: Form.FormRef -> Desktop -> Bool
formDialog reference d=case dialog d of
  Just dg | PluginInputForm owned<-purpose dg->owned==reference
  _->False
adoptForm :: SidebarHost -> Bool -> Form.PreparedInputForm SidebarContext SidebarReply -> Desktop -> IO Desktop
adoptForm (SidebarHost _ ref _ _ _) opening prepared d=do
  state<-readIORef ref
  let reference=Form.formReference prepared
      present=maybe False ((==reference).Form.formReference) (inputForm state)
      owned=if opening then dialog d==Nothing && not (questionActive d) && not (activeAutocomplete d) else present && formDialog reference d
  accepted<-if owned then Form.admitInputForm present prepared else pure False
  if not accepted then pure d else do
    when opening (mapM_ (Form.retireForm . Form.formReference) (inputForm state))
    modifyIORef' ref (\s->s {inputForm=Just prepared})
    let spec=Form.formSpec prepared
        build=Dialog (Form.inputFormTitle spec) (PluginInputForm reference)
          [SelectedInput (Form.inputFormLabel spec) (Form.inputFormInitial spec) (Selection 0 (T.length (Form.inputFormInitial spec)))]
          0 [Form.inputFormSubmit spec,"Cancel"] []
        refresh dg=dg {dialogTitle=Form.inputFormTitle spec,buttons=[Form.inputFormSubmit spec,"Cancel"],
          fields=[case field of SelectedInput _ text selected->SelectedInput (Form.inputFormLabel spec) text selected; _->field | field<-fields dg]}
    pure d {dialog=if opening then Just build else refresh <$> dialog d,contextMenu=Nothing,contextTarget=Nothing}
-- Refresh transport is metadata-only and cannot create a modal by escaped ref.
refreshForm :: SidebarHost -> Form.InputFormUpdate -> Desktop -> IO Desktop
refreshForm host@(SidebarHost _ ref _ _ _) update d=do
  state<-readIORef ref
  case inputForm state of
    Just original | formDialog (Form.updateFormReference update) d->do
      merged<-Form.admitFormRefresh original update
      case merged of Nothing->pure d; Just prepared->adoptForm host False prepared d
    _->pure d
tickForm :: SidebarHost -> Desktop -> IO Desktop
tickForm (SidebarHost _ ref _ _ _) d=do
  state<-readIORef ref
  case inputForm state of
    Nothing->pure d
    Just prepared->do
      live<-Form.formCurrent prepared
      submitted<-Form.submissionCurrent prepared
      let owned=formDialog (Form.formReference prepared) d
      if live && (owned || submitted) then pure d else do
        Form.retireForm (Form.formReference prepared)
        modifyIORef' ref (\s->s {inputForm=Nothing})
        pure (if owned then d {dialog=Nothing,status="Input form expired."} else d)
submitForm :: SidebarHost -> Form.FormRef -> Text -> Menu.MenuOrigin -> Desktop -> IO Desktop
submitForm (SidebarHost _ ref _ _ _) reference text origin d=mask $ \restore->do
  state<-readIORef ref
  case inputForm state of
    Just prepared | origin==Menu.HumanMenu,Form.formReference prepared==reference,formDialog reference d->
      case actionJob state of
        Just _->pure d {status="Sidebar worker is busy; submit again."}
        Nothing->do
          accepted<-Form.claimFormSubmission prepared
          if not accepted then pure d {status="Input form expired."} else do
            worker<-async (restore (Form.invokeFormAction prepared (context origin d) text >>= traverse forceFormReply)) `onException` Form.retireForm reference
            modifyIORef' ref (\s->s {actionJob=Just (FormJob reference worker False)})
            pure d {dialog=Nothing,status="Submitting input form..."}
    _->pure d {status="Input form expired."}
-- This first host form consumer supports only the closed RenameAgentTo reply.
-- Other typed form handlers require their own checked host result route.
-- Fixed replies are forced at their owning worker before the UI receives them.
forceFormReply :: SidebarReply -> IO SidebarReply
forceFormReply reply@(SidebarAgent (RenameAgentTo who value))=evaluate (T.length (Hide.AgentHub.agentIdText who)+T.length value) >> evaluate reply
forceFormReply _=ioError (userError "Unsupported single-line form reply.")
finishFormJob :: SidebarHost -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> Form.FormRef
  -> Async (Either CommandError SidebarReply) -> Bool -> IO Desktop
finishFormJob (SidebarHost _ ref _ cancellation _) core d reference worker cancelled=do
  state<-readIORef ref
  live<-case inputForm state of
    Just prepared | Form.formReference prepared==reference->Form.submissionCurrent prepared
    _->pure False
  let current=live && not cancelled && dialog d==Nothing
  completed<-poll worker
  case completed of
    Nothing | not current && not cancelled->do
      Form.retireForm reference
      queued<-atomically $ do full<-isFullTBQueue cancellation; if full then pure False else writeTBQueue cancellation (Cancellation worker) >> pure True
      modifyIORef' ref (\s->s {actionJob=Just (FormJob reference worker queued)})
      pure d
    Nothing->pure d
    Just result->do
      modifyIORef' ref (\s->s {actionJob=Nothing})
      consumed<-if current then Form.finishFormSubmission reference else Form.retireForm reference >> pure False
      case result of
        Right (Right (SidebarAgent request@RenameAgentTo{})) | consumed->snd <$> core d [AgentSidebarAction request]
        _->pure d {status=if not consumed then "Input form expired." else "Input form submission failed."}
