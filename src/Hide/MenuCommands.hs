{-# LANGUAGE OverloadedStrings #-}
-- | Session-owned live menu worker and Help contribution.
--
-- Only immutable context and exact registrations cross into the worker. Typed
-- command handlers, docs IO and Markdown preparation run there. Tick checks the
-- contribution/command lifetime and host modal/layout state before installing a
-- prepared document. No extension callback executes during admission or adoption.
module Hide.MenuCommands
  ( MenuHost, withMenuCommands, withConversationMenuCommands, menuSidebarCapabilities, menuContributions, menuAgentReferences, publishMenuFromHost, requestMenuRetirement, retireMenuFromHost, MenuContext(..), MenuReply(..), menuEffects, tickMenus
  ) where

import Control.Concurrent.STM (TBQueue, atomically, newTBQueueIO, writeTBQueue, tryReadTBQueue, TVar, newTVarIO, readTVar, readTVarIO, writeTVar, flushTBQueue)
import Control.Concurrent.Async (Async, async, asyncWithUnmask, cancel, poll)
import Control.Exception (bracket, displayException, mask, evaluate)
import Control.Monad (filterM,when)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.IORef
import Data.List (find, sortOn)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Hide.Buffer (BufferContent, bufferContent, contentLength, contentLineAt, contentLineOffset, contentLineCount, contentByteMode, prepareBuffer, Selection(..),DirtySnapshot,captureDirty,snapshotDirty)
import Hide.GuestAccess (protectedPath,protectedFilePath,protectedBuffer)
import Hide.Files (FileState(..),loadFile)
import Hide.Plugin.BufferHost (ContentVersion,captureVersion,versionCurrent)
import Paths_hide (getDataFileName)
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory)
import Hide.DebuggerSidebarTypes
import Hide.DownloadsWindowTypes
import qualified Hide.Plugin.Tree as Tree
import Hide.DocumentationHost (DocsCommands, readDocs)
import qualified Hide.LSP as L
import Hide.Plugin.Documentation
import Hide.Links (LinkResult, applyLink, prepareMarkdown)
import Hide.BufferView (BufferView(..))
import qualified Hide.Plugin.EditorHost as Editor
import Hide.SidebarCommands (SidebarHost,SidebarContext(..),SidebarReply(..),sidebarInvocationContext,adoptForm)
import Hide.Conversation (ConversationState,captureConversationSession)
import Hide.Model hiding (menus)
import qualified Hide.Model as Model
import qualified Hide.Plugin.Window as PluginWindow
import Hide.PluginWindowHost (adoptWindowUpdate,adoptEditorWindowUpdate,applyEditorUpdate)
import Hide.Plugin.Command
import qualified Hide.Plugin.Menu as Plugin

-- | Immutable host input. No mutable Desktop or plugin availability callback
-- crosses into the worker; arguments are projected from this admitted snapshot.
data MenuContext = MenuContext
  { invocationColumns :: Int, invocationOrigin :: Plugin.MenuOrigin
  , invocationNavigation :: Maybe NavigationInput, invocationSource :: Maybe SourceInput
  , invocationRow :: Maybe (PluginWindow.WindowRef,Tree.NodeId)
  , invocationSidebar :: !SidebarContext
  }
data SourceInput = SourceInput ContextTarget ContentVersion DirtySnapshot
data NavigationInput = NavigationInput (FilePath,Int,Int) (Maybe OpenSource) (Maybe [FilePath])
data OpenSource = OpenSource Int Int ContentVersion BufferContent
data Navigation = Navigation FilePath Int Int (Maybe Document)
data MenuReply = PreparedSidebar !SidebarHost !SidebarReply | PreparedDownloadCancel !DownloadCancelRequest | PreparedDocument LinkResult | PreparedNavigation Navigation | PreparedDebugSource DebugSourceRequest | PreparedWindow PluginWindow.WindowUpdate | PreparedEditorWindow !(PluginWindow.EditorWindowUpdate MenuContext MenuReply) | PreparedEditorUpdate !Editor.EditorUpdate

data Pending = Pending Plugin.MenuRef (Maybe ContextTarget) MenuContext (Async (Either Plugin.MenuError MenuReply))
  | PendingEditor Editor.DraftSubmission (Async (Either CommandError MenuReply))
  | RetiringEditor (Async (Either CommandError MenuReply)) (Async ())
data MenuState = MenuState
  { menuPending :: Maybe Pending
  , menuEditors :: M.Map Editor.DraftRef (PluginWindow.WindowRef,Editor.PreparedEditor MenuContext MenuReply) }
data Publication = Publish Plugin.MenuItem | Withdraw Plugin.MenuRef
data MenuHost = MenuHost (Plugin.Menus MenuContext MenuReply) [Plugin.MenuRef] [Plugin.MenuRef] (TBQueue Publication) (IORef MenuState) !(TVar Bool) !(Maybe ConversationState)

-- | Keep the registry independent of frontend attachments. The host may add
-- linked extension declarations to menuContributions before taking its snapshot.
-- Once published, registrations/withdrawals belong to the session owner; use
-- retireMenuFromHost. Closing cancels and joins the worker before either registry
-- scope closes, so shutdown cannot race adoption or resurrect a document.
withMenuCommands :: DocsCommands -> (MenuHost -> IO a) -> IO a
withMenuCommands docs use=withRegistry $ \registry->Plugin.withMenus Model.menuContributionSlots $ \menus->do
  command<-either (ioError . userError . show) pure =<< registerCommand registry (helpCommand docs)
  reference<-either (ioError . userError . show) pure =<< Plugin.contributeMenu menus
    (Plugin.MenuDef "hide.help.contents" "help" "contents" 0 "Contents" "F1" True
      (Plugin.menuAction registry command (const (Right ())) (\context (path,text)->PreparedDocument <$> prepareMarkdown (invocationColumns context) path "" text)))
  navigation<-either (ioError . userError . show) pure =<< registerCommand registry navigationCommand
  navigationRef<-either (ioError . userError . show) pure =<< Plugin.contributeMenu menus
    (Plugin.MenuDef "hide.messages.go-to" "context.messages" "source" 0 "Go to source" "" True
      (Plugin.menuAction registry navigation navigationArguments (\_ -> pure . PreparedNavigation)))
  sourceReferences<-mapM (\(name,title,operation)->do
    source<-either (ioError . userError . show) pure =<< registerCommand registry (sourceCommand name title operation)
    item<-either (ioError . userError . show) pure =<< Plugin.contributeMenu menus
      (Plugin.MenuDef name "context.source" "debug" 0 title "" False
        (Plugin.menuAction registry source (const (Right ())) (\_ ->pure . PreparedDebugSource)))
    pure item) [("hide.debug.toggle-breakpoint","Toggle breakpoint",ToggleSourceBreakpoint),("hide.debug.add-watch","Add watch…",AddSourceWatch)]
  bracket (MenuHost menus [reference,navigationRef] sourceReferences <$> newTBQueueIO 256 <*> newIORef (MenuState Nothing M.empty) <*> newTVarIO False <*> pure Nothing) close use
  where close (MenuHost _ _ _ changes ref closed _)=do
          atomically (writeTVar closed True >> flushTBQueue changes >> pure ())
          state<-readIORef ref
          mapM_ cancelPending (menuPending state)
          mapM_ Editor.retireDraftRef (M.keys (menuEditors state))
        cancelPending (Pending _ _ _ worker)=cancel worker
        cancelPending (PendingEditor _ worker)=cancel worker
        cancelPending (RetiringEditor worker reaper)=cancel worker >> cancel reaper

-- | Compose the existing menu worker with Conversation's captured session owner.
-- The owner is fixed before activation and outlives all plugin registrations.
withConversationMenuCommands :: DocsCommands -> ConversationState -> (MenuHost -> IO a) -> IO a
withConversationMenuCommands docs conversation use=withMenuCommands docs $ \(MenuHost menus permitted sources queue state closed _)->
  use (MenuHost menus permitted sources queue state closed (Just conversation))

-- | Publish typed menus whose replies use this exact existing sidebar form owner.
-- Context/reply maps execute only on the menu worker. Menu and form registrations
-- retain their original lifetimes; no additional registry, queue or modal owner
-- is created, and no Desktop crosses into a declaration.
menuSidebarCapabilities :: MenuHost -> SidebarHost -> Plugin.MenuPublisher SidebarContext SidebarReply
menuSidebarCapabilities host sidebar=Plugin.MenuPublisher publish (requestMenuRetirement host)
  where
    publish definition=do
      registered<-Plugin.contributeMenu (menuContributions host)
        (Plugin.mapMenu invocationSidebar (\_ reply->evaluate (PreparedSidebar sidebar reply)) definition)
      case registered of
        Left err->pure (Left err)
        Right reference->do
          published<-publishMenuFromHost host reference
          pure (reference <$ published)

menuContributions :: MenuHost -> Plugin.Menus MenuContext MenuReply
menuContributions (MenuHost menus _ _ _ _ _ _)=menus

-- | Exact first-party refs allowed by host policy. Contribution metadata can
-- further restrict these; it cannot grant agent authority to new registrations.
menuAgentReferences :: MenuHost -> [Plugin.MenuRef]
menuAgentReferences (MenuHost _ permitted _ _ _ _ _)=permitted

-- | Prepare bounded metadata on the caller's registration worker, then queue an
-- exact delta. Never call this while holding the desktop/session lock. Deltas
-- cannot clobber newer publications, unlike delayed whole-catalogue snapshots.
-- Shutdown wakes blocked callers with MenusClosed and rejects later publication.
publishMenuFromHost :: MenuHost -> Plugin.MenuRef -> IO (Either Plugin.MenuError ())
publishMenuFromHost host@(MenuHost menus _ _ _ _ _ _) reference=do
  metadata<-Plugin.menuMetadata menus reference
  case metadata of
    Left err->pure (Left err)
    Right item->enqueuePublication host (Publish item)

-- | Registration workers request ordered retirement with bounded backpressure.
-- Never call this queueing operation while holding the session/UI lock.
-- Shutdown wakes blocked callers and rejects later retirement with an IOError.
requestMenuRetirement :: MenuHost -> Plugin.MenuRef -> IO ()
requestMenuRetirement host reference=enqueuePublication host (Withdraw reference) >>= either (ioError . userError . show) pure

-- Ordered transport admission and scope close share one STM boundary.
enqueuePublication :: MenuHost -> Publication -> IO (Either Plugin.MenuError ())
enqueuePublication (MenuHost _ _ _ changes _ closed _) publication=atomically $ do
  stopped<-readTVar closed
  if stopped then pure (Left Plugin.MenusClosed) else writeTBQueue changes publication >> pure (Right ())

requireOpen :: MenuHost -> IO ()
requireOpen (MenuHost _ _ _ _ _ closed _)=readTVarIO closed >>= \stopped->when stopped (ioError (userError "Menu host closed."))

-- | Withdraw directly at the session owner. UI callers do not enqueue or wait for
-- capacity, even if registration workers have filled the publication queue.
retireMenuFromHost :: MenuHost -> Plugin.MenuRef -> Desktop -> IO Desktop
retireMenuFromHost host@(MenuHost menus _ _ _ _ _ _) reference d=do
  requireOpen host
  _<-Plugin.retireMenu menus reference
  pure d {contributedMenus=filter ((/=reference) . Plugin.menuReference) (contributedMenus d),
    agentMenuRefs=filter (/=reference) (agentMenuRefs d),menu=Nothing,contextMenu=Nothing,contextTarget=Nothing}

-- Input remains schedulable under producer load: at most 16 bounded deltas are
-- admitted at one owner boundary. Metadata/handlers were prepared elsewhere.
adoptPublications :: MenuHost -> Desktop -> IO Desktop
adoptPublications host@(MenuHost menus _ _ changes _ _ _)=drain (16::Int)
  where
    drain 0 d=pure d
    drain remaining d=do
      pending<-atomically (tryReadTBQueue changes)
      case pending of
        Nothing->pure d
        Just change->do
          next<-case change of
            Withdraw reference->retireMenuFromHost host reference d
            Publish item->do
              live<-Plugin.menuCurrent menus (Plugin.menuReference item)
              pure $ if not live then d else d {contributedMenus=sortOn order
                (item:filter ((/=Plugin.menuName (Plugin.menuReference item)) . Plugin.menuName . Plugin.menuReference) (contributedMenus d))}
          -- Catalogue changes close positional popups before their next event.
          drain (remaining-1) next {menu=Nothing,contextMenu=Nothing,contextTarget=Nothing}
    order item=(Plugin.menuSlot item,Plugin.menuGroup item,Plugin.menuOrder item,Plugin.menuName (Plugin.menuReference item))

helpCommand :: DocsCommands -> CommandDef MenuContext () (FilePath,T.Text)
helpCommand docs=CommandDef "hide.help.contents" "Help contents" unit output $ \_ ()->do
  path<-getDataFileName "README.md" >>= canonicalizePath
  let resolve _=pure (takeDirectory path)
      readAll start pieces=case readArguments "editor" "README.md" start 500 of
        Left err->pure (Left (CommandRejected err))
        Right arguments->do
          loaded<-readDocs docs resolve arguments
          case loaded of
            Left err->pure (Left err)
            Right page
              | pageTruncated page->pure (Left (CommandRejected "Help page exceeds the documentation read budget."))
              | pageHasMore page->readAll (start+pageCount page) (pageText page:pieces)
              | otherwise->pure (Right (path,T.intercalate "\n" (reverse (pageText page:pieces))))
  readAll 1 []
  where
    unit=Codec (object ["type" .= ("object"::T.Text),"additionalProperties" .= False])
      (\value->if value==object [] then Right () else Left "Help contents takes no arguments.") (const (object []))
    output=Codec (object ["type" .= ("object"::T.Text),"required" .= (["path","text"]::[T.Text]),
        "properties" .= object ["path" .= object ["type" .= ("string"::T.Text)],"text" .= object ["type" .= ("string"::T.Text)]]])
      (either (Left . T.pack) Right . parseEither (withObject "help contents" $ \o->(,) <$> o .: "path" <*> o .: "text"))
      (\(path,text)->object ["path" .= path,"text" .= text])

sourceCommand :: T.Text -> T.Text -> DebugSourceOperation -> CommandDef MenuContext () DebugSourceRequest
sourceCommand name title operation=CommandDef name title hidden hidden $ \context ()->case invocationSource context of
  Just (SourceInput target@SourceTarget{} version modified) | invocationOrigin context==Plugin.HumanMenu->do
    canonical<-traverse canonicalizePath (sourceTargetFile target)
    changed<-evaluate (snapshotDirty modified)
    pure (Right (DebugSourceRequest operation (sourceTargetWindow target) (sourceTargetBuffer target) version
      (sourceTargetSelection target) (sourceTargetFile target) canonical (sourceTargetRow target) (sourceTargetExpression target) changed))
  _->pure (Left (CommandRejected "No admitted human source target."))
  where hidden=Codec Null (const (Left "Source arguments are host-captured.")) (const Null)

captureSource :: Maybe ContextTarget -> Desktop -> IO (Maybe SourceInput)
captureSource (Just target@SourceTarget{}) d=case M.lookup (sourceTargetBuffer target) (buffers d) of
  Just doc | not (contentByteMode (bufferContent (documentBuffer doc)))->do
    version<-captureVersion (documentBuffer doc)
    modified<-evaluate (captureDirty (documentBuffer doc))
    pure (Just (SourceInput target version modified))
  _->pure Nothing
captureSource _ _=pure Nothing

navigationArguments :: MenuContext -> Either T.Text (FilePath,Int,Int)
navigationArguments context=case invocationNavigation context of
  Just (NavigationInput location _ _)->Right location
  Nothing->Left "No captured diagnostic source location."

-- The source command is typed and host-admitted. Its location cannot come from
-- frontend JSON, and its immutable read/version never comes from later focus.
navigationCommand :: CommandDef MenuContext (FilePath,Int,Int) Navigation
navigationCommand=CommandDef "hide.messages.go-to" "Go to diagnostic source" location prepared $ \context target@(path,row,col)->
  case invocationNavigation context of
    Just (NavigationInput captured opened authority) | captured==target->do
      resolved<-canonicalizePath path
      if maybe False (`protectedFilePath` resolved) authority then pure forbidden else case opened of
        Just (OpenSource _ _ _ image)->prepare resolved row col image Nothing
        Nothing->do
          result<-loadFile resolved
          case result of
            Left err->pure (Left (CommandRejected (T.pack err)))
            Right (file,_) | maybe False (\paths -> protectedFilePath paths (filePath file)) authority->pure forbidden
            Right (file,buffer)->do
              _<-evaluate (prepareBuffer buffer)
              doc<-evaluate (newDocument buffer (Just file))
              prepare (filePath file) row col (bufferContent buffer) (Just doc)
    _->pure (Left (CommandRejected "Navigation arguments were not admitted by the host."))
  where
    forbidden=Left (CommandRejected "Agent navigation cannot open protected authority files.")
    location=Codec (object ["type" .= ("object"::T.Text)])
      (either (Left . T.pack) Right . parseEither (withObject "source location" $ \o->(,,) <$> o .: "path" <*> o .: "row" <*> o .: "column"))
      (\(path,row,col)->object ["path" .= path,"row" .= row,"column" .= col])
    -- The immutable prepared document is host-only; external output is metadata.
    prepared=Codec (object ["type" .= ("object"::T.Text)]) (const (Left "Prepared navigation is a host value."))
      (\(Navigation path row offset _)->object ["path" .= path,"row" .= row,"offset" .= offset])
    prepare path row col image doc=do
      let selected=max 0 (min row (contentLineCount image-1))
          line=contentLineAt image selected
          offset=min (contentLength image) (contentLineOffset image selected+L.positionOffset line (0,col))
      _<-evaluate offset
      pure (if contentByteMode image then Left (CommandRejected "Diagnostic source is not a text buffer.") else Right (Navigation path selected offset doc))

columns :: Desktop -> Int
columns d=max 20 (min 76 (fst (screenSize d)-treeWidthOf d-4))

captureNavigation :: Plugin.MenuOrigin -> Maybe ContextTarget -> Desktop -> IO (Maybe NavigationInput)
captureNavigation origin (Just (MessagesTarget _ _ (Just location@(path,_,_)))) d=do
  opened<-case find (\(_,doc)->fmap filePath (documentFile doc)==Just path && documentLabel doc==Nothing) (M.toList (buffers d)) of
    Just (bid,doc) | Just window<-find ((==Just bid) . bufferId) (windows d)->do
      version<-captureVersion (documentBuffer doc)
      image<-evaluate (bufferContent (documentBuffer doc))
      pure (Just (OpenSource (windowId window) bid version image))
    _->pure Nothing
  privatePaths<-evaluate (privateFilePaths d)
  pure (Just (NavigationInput location opened (if origin==Plugin.AgentMenu then Just privatePaths else Nothing)))
captureNavigation _ _ _=pure Nothing

-- | Admission checks only policy/lifetimes and captures immutable read handles.
-- Busy calls refuse instead of replacing another prepared result.
menuEffects :: MenuHost -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
menuEffects host@(MenuHost menus permitted _ _ ref _ conversation) _ original [InvokeMenu reference origin target]=mask $ \restore->do
  requireOpen host
  published<-adoptPublications host original
  d<-tickEditorBindings host published
  pending<-menuPending <$> readIORef ref
  live<-Plugin.menuCurrent menus reference
  let item=find ((==reference) . Plugin.menuReference) (contributedMenus d)
      allowed=dialog d==Nothing && maybe False (\entry->origin==Plugin.HumanMenu || reference `elem` permitted && Plugin.menuAgentAllowed entry) item &&
        maybe True (\captured->contextTargetCurrent d {contextTarget=Just captured}) target &&
        maybe True (\entry->Plugin.menuSlot entry/="context.window-rows" || reference `elem` windowRowMenuRefsFor target d) item
  if not live || not allowed then pure (False,d {status="Menu action is stale or unavailable."}) else case pending of
    Just _->pure (False,d {status="A menu action is already running."})
    Nothing->do
      navigation<-captureNavigation origin target d
      source<-captureSource target d
      sessionTarget<-maybe (pure (Left "No conversation session owner.")) (\runtime->captureConversationSession runtime d) conversation
      sidebar<-evaluate ((sidebarInvocationContext origin d) {sidebarConversation=sessionTarget})
      _<-evaluate (length (sidebarContextWorkspace sidebar))
      let row=case target of Just (WindowRowTarget windowRef ident)->Just (windowRef,ident); _->Nothing
          context=MenuContext (columns d) origin navigation source row sidebar
      worker<-async (restore (Plugin.invokeMenu menus reference context))
      modifyIORef' ref (\s->s {menuPending=Just (Pending reference target context worker)})
      pure (False,d {status="Running menu action…"})
menuEffects host@(MenuHost _ _ _ _ ref _ _) core d [effect@(SubmitEditor mount slot origin)]=do
  requireOpen host
  state<-readIORef ref
  if M.member (Editor.mountDraft mount) (menuEditors state)
    then (False,) <$> submitMenuEditor host mount slot origin d
    else core d [effect]
menuEffects host core d requests=requireOpen host >> core d requests

-- | Check captured identity before applying prepared geometry. Dirty open files
-- stay in memory; disk preparation can never replace an intervening open buffer.
adoptNavigation :: MenuContext -> Navigation -> Desktop -> IO Desktop
adoptNavigation context (Navigation path row offset loaded) d
  | invocationOrigin context==Plugin.AgentMenu && protectedPath d path=pure d {status="Agent navigation target is now protected."}
  | otherwise=case invocationNavigation context of
    Just (NavigationInput _ (Just (OpenSource wid bid version _)) _)->case (find ((==wid) . windowId) (windows d),M.lookup bid (buffers d)) of
      (Just window,Just doc) | bufferId window==Just (bid) && fmap filePath (documentFile doc)==Just path->do
        current<-versionCurrent version (documentBuffer doc)
        pure $ if current && (invocationOrigin context/=Plugin.AgentMenu || not (protectedBuffer d bid)) then position (focusWindow wid d) else expired
      _->pure expired
    _ | any ((==Just path) . fmap filePath . documentFile) (M.elems (buffers d))->pure expired
      | Just doc<-loaded->do
          let opened=addDocument (documentFile doc) (documentBuffer doc) d
              installed=case activeWindow opened of
                Just window | Just bid<-bufferId window->opened {buffers=M.insert bid doc (buffers opened)}
                _->opened
          pure (position installed)
      | otherwise->pure expired
  where
    expired=d {status="Navigation target changed; invoke it again."}
    position=ensureVisible . modifyActive (\window->window {selection=Selection offset offset,reviewSelection=Nothing,
      bufferView=CurrentView,scrollRow=max 0 (row-height (bounds window) `div` 2),scrollColumn=0})

-- | Drain ordered publication/retirement before late reply adoption. No handler
-- or lazy extension metadata executes here, and no file work runs under the lock.
tickMenus :: MenuHost -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO Desktop
tickMenus host@(MenuHost menus _ sourceRefs _ ref closed _) core original=
  readTVarIO closed >>= \stopped->if stopped then pure original else do
  published<-adoptPublications host original
  d<-tickEditorBindings host published
  pending<-menuPending <$> readIORef ref
  case pending of
    Nothing->pure d
    Just (PendingEditor submitted worker)->finishMenuEditor host d submitted worker
    Just (RetiringEditor _ reaper)->do
      completed<-poll reaper
      case completed of
        Nothing->pure d
        Just _->modifyIORef' ref (\s->s {menuPending=Nothing}) >> pure d
    Just (Pending reference target context worker)->do
      completed<-poll worker
      case completed of
        Nothing->pure d
        Just result->do
          modifyIORef' ref (\s->s {menuPending=Nothing})
          live<-Plugin.menuCurrent menus reference
          let exactRow=case result of Right (Right PreparedDownloadCancel{})->invocationOrigin context==Plugin.HumanMenu && invocationRow context/=Nothing; _->False
              current=dialog d==Nothing && (not exactRow || reference `elem` windowRowMenuRefsFor target d) && (exactRow || columns d==invocationColumns context) &&
                any ((==reference) . Plugin.menuReference) (contributedMenus d) &&
                maybe True (\captured->contextTargetCurrent d {contextTarget=Just captured}) target
          if not live || not current then pure d {status="Menu result expired; invoke it again."} else case result of
            Left err->pure d {status="Menu action failed: "<>T.pack (displayException err)}
            Right (Left err)->pure d {status="Menu action failed: "<>T.pack (show err)}
            Right (Right (PreparedSidebar sidebar (SidebarForm prepared))) | invocationOrigin context==Plugin.HumanMenu->adoptForm sidebar True prepared d
            Right (Right (PreparedSidebar _ (SidebarConversation request))) | invocationOrigin context==Plugin.HumanMenu->snd <$> core d [ConversationSessionAction request]
            Right (Right PreparedSidebar{})->pure d {status="Menu result requires its owning operation."}
            Right (Right (PreparedDownloadCancel request))->snd <$> core d [DownloadCancelAction request]
            Right (Right (PreparedWindow prepared))->adoptWindowUpdate (invocationOrigin context) prepared d
            Right (Right (PreparedEditorWindow prepared))->adoptMenuEditor host (invocationOrigin context) prepared d
            Right (Right PreparedEditorUpdate{})->pure d {status="Editor result has no submitted job."}
            Right (Right (PreparedDocument prepared))->pure (fst (applyLink prepared d))
            Right (Right (PreparedNavigation prepared))->adoptNavigation context prepared d
            Right (Right PreparedDebugSource{}) | reference `notElem` sourceRefs->pure d {status="Debugger source controls require a host-owned contribution."}
            Right (Right (PreparedDebugSource prepared))->case M.lookup (debugSourceBuffer prepared) (buffers d) of
              Nothing->pure d {status="Source action target closed."}
              Just doc->do
                valid<-versionCurrent (debugSourceVersion prepared) (documentBuffer doc)
                if valid then snd <$> core d [DebugSourceAction prepared] else pure d {status="Source action target changed."}

-- Retain callable ownership independently of a visible frame; a scope end
-- retires even a hidden draft. Unsent input moves to an ordinary private
-- document; callable bindings and prepared seeds retain no duplicate Undo.
tickEditorBindings :: MenuHost -> Desktop -> IO Desktop
tickEditorBindings (MenuHost _ _ _ _ ref _ _) d=do
  state<-readIORef ref
  expired<-filterM (\(_, (scope,editor))->do
    published<-PluginWindow.windowScopeCurrent scope
    live<-Editor.editorBindingCurrent editor
    pure (not (published && live))) (M.toList (menuEditors state))
  mapM_ (Editor.retireDraftRef . fst) expired
  let removed=map fst expired
  modifyIORef' ref (\s->s {menuEditors=foldr M.delete (menuEditors s) removed})
  pure (preserveEditorDrafts removed d)

adoptMenuEditor :: MenuHost -> Plugin.MenuOrigin -> PluginWindow.EditorWindowUpdate MenuContext MenuReply -> Desktop -> IO Desktop
adoptMenuEditor host@(MenuHost _ _ _ _ ref _ _) origin update original=do
  d<-tickEditorBindings host original
  state<-readIORef ref
  let editor=PluginWindow.editorWindowEditor update
      draft=Editor.mountDraft (Editor.editorMount editor)
      previous=Editor.editorMount . snd <$> M.lookup draft (menuEditors state)
  (accepted,next)<-if M.size (menuEditors state)>=256 && M.notMember draft (menuEditors state)
    then pure (False,d {status="Editor owner is full."})
    else adoptEditorWindowUpdate origin previous update d
  when accepted (modifyIORef' ref (\s->s {menuEditors=M.insert draft
    (PluginWindow.updateWindowRef (PluginWindow.editorWindowBody update),Editor.installedEditor editor) (menuEditors s)}))
  pure next

submitMenuEditor :: MenuHost -> Editor.EditorMount -> Editor.EditorSlot -> Plugin.MenuOrigin -> Desktop -> IO Desktop
submitMenuEditor (MenuHost _ _ _ _ ref _ _) mount slot origin d=mask $ \_->do
  state<-readIORef ref
  case M.lookup (Editor.mountDraft mount) (menuEditors state) of
    Just (_,editor) | origin==Plugin.HumanMenu,activeEditorMount d==Just mount,composerActive d,Editor.editorMount editor==mount->case menuPending state of
      Just _->pure d {status="A menu action is already running."}
      Nothing->do
        captured<-Editor.captureDraftSubmission mount slot (composerBuffer d)
        case captured of
          Nothing->pure d {status="Editor input expired."}
          Just submitted->do
            let context=MenuContext (columns d) origin Nothing Nothing Nothing (sidebarInvocationContext origin d)
            worker<-asyncWithUnmask (\unmask->unmask (Editor.invokeEditorAction editor context submitted >>= traverse evaluate))
            modifyIORef' ref (\s->s {menuPending=Just (PendingEditor submitted worker)})
            pure d {status="Submitting editor input..."}
    _->pure d {status="Editor input expired."}

-- Popup/menu refresh is unrelated to an already accepted editor invocation.
-- Closing its frame refuses pre-admission work but cannot undo committed work.
finishMenuEditor :: MenuHost -> Desktop -> Editor.DraftSubmission
  -> Async (Either CommandError MenuReply) -> IO Desktop
finishMenuEditor host@(MenuHost _ _ _ _ ref _ _) d submitted worker=do
  state<-readIORef ref
  owning<-case M.lookup (Editor.submissionDraft submitted) (menuEditors state) of
    Just (scope,editor) | Editor.mountActions (Editor.editorMount editor)==Editor.mountActions (Editor.submissionMount submitted)->do
      published<-PluginWindow.windowScopeCurrent scope
      registered<-Editor.editorBindingCurrent editor
      pure (published && registered)
    _->pure False
  completed<-poll worker
  case completed of
    Nothing->mask $ \_->do
      live<-Editor.mountCurrent (Editor.submissionMount submitted)
      aborted<-if live && owning then pure False else Editor.abortEditorSubmission submitted
      if not aborted then pure d else do
        reaper<-asyncWithUnmask (\unmask->unmask (cancel worker))
        modifyIORef' ref (\s->s {menuPending=Just (RetiringEditor worker reaper)})
        pure d
    Just result->do
      modifyIORef' ref (\s->s {menuPending=Nothing})
      if not owning then pure d {status="Editor owner expired."} else case result of
        Right (Right (PreparedEditorUpdate update))->applyEditorUpdate submitted update d
        Right (Right (PreparedEditorWindow update))->adoptMenuEditor host Plugin.HumanMenu update d
        Right (Right (PreparedWindow update))->adoptWindowUpdate Plugin.HumanMenu update d
        Right (Right (PreparedDocument prepared))->pure (fst (applyLink prepared d))
        Right (Left err)->pure d {status="Editor submission failed: "<>T.pack (show err)}
        Left err->pure d {status="Editor submission failed: "<>T.pack (displayException err)}
        _->pure d {status="Editor result requires its owning operation."}
