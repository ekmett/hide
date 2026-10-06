{-# LANGUAGE OverloadedStrings #-}
-- | Session-owned live menu worker and Help contribution.
--
-- Only immutable context and exact registrations cross into the worker. Typed
-- command handlers, docs IO and Markdown preparation run there. Tick checks the
-- contribution/command lifetime and host modal/layout state before installing a
-- prepared document. No extension callback executes during admission or adoption.
module Hide.MenuCommands
  ( MenuHost, withMenuCommands, menuContributions, menuAgentReferences, publishMenuFromHost, requestMenuRetirement, retireMenuFromHost, MenuContext(..), MenuReply(..), menuEffects, tickMenus
  ) where

import Control.Concurrent.STM (TBQueue, atomically, newTBQueueIO, writeTBQueue, tryReadTBQueue)
import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Exception (bracket, displayException, mask, evaluate)
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
import Hide.DocsMCP (DocsCommands, readDocs)
import qualified Hide.LSP as L
import Hide.Documentation
import Hide.Links (LinkResult, applyLink, prepareMarkdown)
import Hide.BufferView (BufferView(..))
import Hide.Model hiding (menus)
import qualified Hide.Model as Model
import qualified Hide.Plugin.Window as PluginWindow
import Hide.PluginWindowHost (adoptWindowUpdate)
import Hide.Plugin.Command
import qualified Hide.Plugin.Menu as Plugin

-- | Immutable host input. No mutable Desktop or plugin availability callback
-- crosses into the worker; arguments are projected from this admitted snapshot.
data MenuContext = MenuContext
  { invocationColumns :: Int, invocationOrigin :: Plugin.MenuOrigin
  , invocationNavigation :: Maybe NavigationInput, invocationSource :: Maybe SourceInput
  , invocationRow :: Maybe (PluginWindow.WindowRef,Tree.NodeId)
  }
data SourceInput = SourceInput ContextTarget ContentVersion DirtySnapshot
data NavigationInput = NavigationInput (FilePath,Int,Int) (Maybe OpenSource) (Maybe [FilePath])
data OpenSource = OpenSource Int Int ContentVersion BufferContent
data Navigation = Navigation FilePath Int Int (Maybe Document)
data MenuReply = PreparedDownloadCancel !DownloadCancelRequest | PreparedDocument LinkResult | PreparedNavigation Navigation | PreparedDebugSource DebugSourceRequest | PreparedWindow PluginWindow.WindowUpdate

data Pending = Pending Plugin.MenuRef (Maybe ContextTarget) MenuContext (Async (Either Plugin.MenuError MenuReply))
data Publication = Publish Plugin.MenuItem | Withdraw Plugin.MenuRef
data MenuHost = MenuHost (Plugin.Menus MenuContext MenuReply) [Plugin.MenuRef] [Plugin.MenuRef] (TBQueue Publication) (IORef (Maybe Pending))

-- | Keep the registry independent of frontend attachments. The host may add
-- linked extension declarations to menuContributions before taking its snapshot.
-- Once published, registrations/withdrawals belong to the session owner; use
-- retireMenuFromHost. Closing cancels and joins the worker before either registry
-- scope closes, so shutdown cannot race adoption or resurrect a document.
withMenuCommands :: DocsCommands -> (MenuHost -> IO a) -> IO a
withMenuCommands docs use=withRegistry $ \registry->Plugin.withMenus ("context.window-rows":"context.source":"context.messages":[T.toLower title | (title,_,_)<-Model.menus]) $ \menus->do
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
  bracket (MenuHost menus [reference,navigationRef] sourceReferences <$> newTBQueueIO 256 <*> newIORef Nothing) close use
  where close (MenuHost _ _ _ _ ref)=readIORef ref >>= mapM_ (\(Pending _ _ _ worker)->cancel worker)

menuContributions :: MenuHost -> Plugin.Menus MenuContext MenuReply
menuContributions (MenuHost menus _ _ _ _)=menus

-- | Exact first-party refs allowed by host policy. Contribution metadata can
-- further restrict these; it cannot grant agent authority to new registrations.
menuAgentReferences :: MenuHost -> [Plugin.MenuRef]
menuAgentReferences (MenuHost _ permitted _ _ _)=permitted

-- | Prepare bounded metadata on the caller's registration worker, then queue an
-- exact delta. Never call this while holding the desktop/session lock. Deltas
-- cannot clobber newer publications, unlike delayed whole-catalogue snapshots.
publishMenuFromHost :: MenuHost -> Plugin.MenuRef -> IO (Either Plugin.MenuError ())
publishMenuFromHost (MenuHost menus _ _ changes _) reference=do
  metadata<-Plugin.menuMetadata menus reference
  case metadata of
    Left err->pure (Left err)
    Right item->atomically (writeTBQueue changes (Publish item)) >> pure (Right ())

-- | Registration workers request ordered retirement with bounded backpressure.
-- Never call this queueing operation while holding the session/UI lock.
requestMenuRetirement :: MenuHost -> Plugin.MenuRef -> IO ()
requestMenuRetirement (MenuHost _ _ _ changes _)=atomically . writeTBQueue changes . Withdraw

-- | Withdraw directly at the session owner. UI callers do not enqueue or wait for
-- capacity, even if registration workers have filled the publication queue.
retireMenuFromHost :: MenuHost -> Plugin.MenuRef -> Desktop -> IO Desktop
retireMenuFromHost (MenuHost menus _ _ _ _) reference d=do
  _<-Plugin.retireMenu menus reference
  pure d {contributedMenus=filter ((/=reference) . Plugin.menuReference) (contributedMenus d),
    agentMenuRefs=filter (/=reference) (agentMenuRefs d),menu=Nothing,contextMenu=Nothing,contextTarget=Nothing}

-- Input remains schedulable under producer load: at most 16 bounded deltas are
-- admitted at one owner boundary. Metadata/handlers were prepared elsewhere.
adoptPublications :: MenuHost -> Desktop -> IO Desktop
adoptPublications host@(MenuHost menus _ _ changes _)=drain (16::Int)
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
  privatePaths<-evaluate (guestPrivatePaths d)
  pure (Just (NavigationInput location opened (if origin==Plugin.AgentMenu then Just privatePaths else Nothing)))
captureNavigation _ _ _=pure Nothing

-- | Admission checks only policy/lifetimes and captures immutable read handles.
-- Busy calls refuse instead of replacing another prepared result.
menuEffects :: MenuHost -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
menuEffects host@(MenuHost menus permitted _ _ ref) _ original [InvokeMenu reference origin target]=mask $ \restore->do
  d<-adoptPublications host original
  pending<-readIORef ref
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
      let row=case target of Just (WindowRowTarget reference ident)->Just (reference,ident); _->Nothing
          context=MenuContext (columns d) origin navigation source row
      worker<-async (restore (Plugin.invokeMenu menus reference context))
      writeIORef ref (Just (Pending reference target context worker))
      pure (False,d {status="Running menu action…"})
menuEffects _ core d requests=core d requests

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
tickMenus host@(MenuHost menus _ sourceRefs _ ref) core original=do
  d<-adoptPublications host original
  pending<-readIORef ref
  case pending of
    Nothing->pure d
    Just (Pending reference target context worker)->do
      completed<-poll worker
      case completed of
        Nothing->pure d
        Just result->do
          writeIORef ref Nothing
          live<-Plugin.menuCurrent menus reference
          let exactRow=case result of Right (Right PreparedDownloadCancel{})->invocationOrigin context==Plugin.HumanMenu && invocationRow context/=Nothing; _->False
              current=dialog d==Nothing && (not exactRow || reference `elem` windowRowMenuRefsFor target d) && (exactRow || columns d==invocationColumns context) &&
                any ((==reference) . Plugin.menuReference) (contributedMenus d) &&
                maybe True (\captured->contextTargetCurrent d {contextTarget=Just captured}) target
          if not live || not current then pure d {status="Menu result expired; invoke it again."} else case result of
            Left err->pure d {status="Menu action failed: "<>T.pack (displayException err)}
            Right (Left err)->pure d {status="Menu action failed: "<>T.pack (show err)}
            Right (Right (PreparedDownloadCancel request))->snd <$> core d [DownloadCancelAction request]
            Right (Right (PreparedWindow prepared))->adoptWindowUpdate (invocationOrigin context) prepared d
            Right (Right (PreparedDocument prepared))->pure (fst (applyLink prepared d))
            Right (Right (PreparedNavigation prepared))->adoptNavigation context prepared d
            Right (Right PreparedDebugSource{}) | reference `notElem` sourceRefs->pure d {status="Debugger source controls require a host-owned contribution."}
            Right (Right (PreparedDebugSource prepared))->case M.lookup (debugSourceBuffer prepared) (buffers d) of
              Nothing->pure d {status="Source action target closed."}
              Just doc->do
                valid<-versionCurrent (debugSourceVersion prepared) (documentBuffer doc)
                if valid then snd <$> core d [DebugSourceAction prepared] else pure d {status="Source action target changed."}
