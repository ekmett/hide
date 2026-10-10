-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
-- |
-- Module      : Hide.SidebarCommands
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : ExistentialQuantification, OverloadedStrings
--
-- Session owner for scoped sidebar providers. Pages, projections and prepared
-- file opens run on workers. Owner ticks adopt only exact current requests; the
-- cached viewport never evaluates a provider or inspects buffer payloads.
module Hide.SidebarCommands
  ( SidebarHost, SidebarContext(..), SidebarReply(..), withSidebarCommands
  , sidebarRegistry, sidebarCapabilities, publishTreeFromHost, retireTreeFromHost, sidebarEffects
  , tickSidebar, refreshTreeFromHost, initializeSidebar, awaitFileOpening, prepareSidebarFile, publishFormRefreshFromHost
  , sidebarInvocationContext, adoptForm, adoptPopupForm
  ) where

import Hide.FileIO (withFileRead)

import Control.Concurrent.Async (Async,async,asyncWithUnmask,cancel,poll)
import qualified Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq (force)
import Control.Exception (bracket,evaluate,displayException,mask,onException)
import Control.Monad (foldM,forever,forM,filterM,when)
import Data.Aeson (Value(Null))
import Data.IORef
import Data.List (find,nub)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (Text)
import System.Directory (canonicalizePath,doesFileExist,doesDirectoryExist)
import qualified Data.ByteString as BS
import System.FilePath ((</>),takeExtension,takeFileName,takeDirectory,isAbsolute)
import Data.Char (toLower)
import System.Mem.StableName
import Text.Read (readMaybe)
import Hide.Browser
import Hide.Buffer (Buffer,bufferLength,bufferBytes,byteMode,captureDirty,snapshotDirty,bufferLineChanges,prepareBuffer,Selection(..))
import Hide.Plugin.BufferHost (ContentVersion,captureVersion,versionCurrent)
import Hide.Files (FileState(..),FileRepresentation(..),loadFileForDisplay,fileBuffer)
import Hide.Plugin.Canvas (isImageContent)
import qualified Hide.WorkspaceRename as Rename
import System.IO.Error (tryIOError)
import Hide.GuestAccess (protectedPath,protectedFilePath,protectedBuffer)
import Hide.Links (LinkResult,applyLink)
import Hide.PluginWindowHost (adoptWindowUpdate,adoptEditorWindowUpdate,applyEditorUpdate)
import qualified Hide.Plugin.Window as PluginWindow
import Hide.Model
import Hide.DebuggerSidebarTypes
import qualified Hide.AgentHub
import qualified Hide.Plugin.Form as Form
import qualified Hide.Plugin.Sidebar as PluginSidebar
import Hide.AgentSidebarTypes
import Hide.ConversationSessionTypes
import qualified Hide.Plugin.ConversationSession as Conversation
import qualified Hide.Plugin.Provider as Provider
import Hide.SessionSidebarTypes
import qualified Hide.Recovery as Recovery
import Hide.Sidebar
import qualified Hide.Plugin.EditorHost as Editor
import Hide.Plugin.Command
import qualified Hide.Plugin.Tree as P
import qualified Hide.Plugin.Menu as Menu

-- | Captured host policy; extension labels and paths grant no authority.
data SidebarContext = SidebarContext
  { sidebarOrigin :: !Menu.MenuOrigin, sidebarContextDirectory :: !FilePath, sidebarContextWorkspace :: !FilePath, sidebarProvider :: !(Maybe P.TreeRef), sidebarPrivatePaths :: ![FilePath]
  , sidebarColumns :: !Int, sidebarOpenedImage :: !(Maybe (FilePath,Int,PluginWindow.WindowRef)), sidebarOpened :: !(Maybe (FilePath,Int,Int,ContentVersion)), sidebarRenameFiles :: ![Rename.RenameFile], sidebarExportEpoch :: !Int, sidebarAttachment :: !Int
  , sidebarConversation :: !(Either Text (Conversation.ConversationTarget ConversationSessionReceipt))
  , sidebarConversationOperation :: !(Either Text (Conversation.ConversationOperationTarget ConversationSessionReceipt))
  , sidebarSelectedAgent :: !(Either Text Hide.AgentHub.AgentId) }
data SidebarReply = SidebarPopupForm !(Form.PreparedForm SidebarContext SidebarReply) | SidebarExportFile !Int !Text !BS.ByteString | SidebarPackageDebug !PackageBuildTarget !(Either Text FilePath) | SidebarBuild !BuildAction !PackageBuildTarget | SidebarRename !P.TreeRef !Rename.PreparedRename | SidebarForm !(Form.PreparedForm SidebarContext SidebarReply) | SidebarSession !SessionSidebarRequest | SidebarConversation !(Conversation.ConversationRequest ConversationSessionReceipt) | SidebarRecoveredSources !Int !FilePath !Recovery.RecoveredSources | SidebarExisting !FilePath !Int !Int !ContentVersion | SidebarDocument !FilePath !Document | SidebarPrepared !LinkResult | SidebarAgent !AgentSidebarRequest | SidebarDebug !DebugSidebarRequest | SidebarExistingImage !FilePath !Int !PluginWindow.WindowRef | SidebarImage !(Maybe FilePath) !PluginWindow.PreparedWindow | SidebarUpload !Document | SidebarWindow !PluginWindow.WindowUpdate | SidebarEditorWindow !(PluginWindow.EditorWindowUpdate SidebarContext SidebarReply) | SidebarEditorUpdate !Editor.EditorUpdate

data ChildJob = ChildJob !TreeRequest !Menu.MenuOrigin !(Async (Either CommandError (P.PreparedPage SidebarContext SidebarReply))) !Bool
data ActionJob = ActionJob ![P.TreeHit] !CommandRef !Menu.MenuOrigin !Int !(Async (Either CommandError SidebarReply)) !Bool
  | FileJob !Menu.MenuOrigin !(Maybe Int) !Int !(Async (Either CommandError SidebarReply)) !Bool
  | ExportJob !Int !Int !ContentVersion ![Integer] !(Async (Either CommandError SidebarReply)) !Bool
  | FormJob !Form.FormRef !(Async (Either CommandError SidebarReply)) !Bool
  | EditorJob !Editor.DraftSubmission !(Async (Either CommandError SidebarReply)) !Bool
data FileRequest = FilePathRequest !Menu.MenuOrigin !FilePath | FileBytesRequest !Text !BS.ByteString
data PendingFile = PendingFile !(Maybe Int) !Int !FileRequest
data Publication = TreePublication !(P.TreeProvider SidebarContext SidebarReply) | FormRefresh !Form.FormUpdate | TreeInvalidation !P.TreeRef !P.NodeId
data FilesProvider = FilesProvider !(P.TreeProvider SidebarContext SidebarReply) !FilePath !CommandRef !CommandRef !CommandRef !CommandRef !(IORef (M.Map P.NodeId FilePath,M.Map FilePath P.NodeId,Int,M.Map FilePath [Entry]))
data State = State
  { imageWindowScope :: !PluginWindow.WindowScope
  , providers :: !(M.Map P.TreeRef (P.TreeProvider SidebarContext SidebarReply))
  , definitions :: !(M.Map NodeKey (P.NodeDef SidebarContext SidebarReply))
  , jobs :: ![ChildJob], waiting :: ![(TreeRequest,Menu.MenuOrigin)]
  , projection :: !(Maybe (Integer,Async (Projection,M.Map NodeKey (P.NodeDef SidebarContext SidebarReply),RecoveryProjection)))
  , actionJob :: !(Maybe ActionJob), pendingFiles :: ![PendingFile], filesProvider :: !(Maybe FilesProvider)
  , badgeStamp :: !(Maybe (StableName (M.Map Int Document)))
  , badgeJob :: !(Maybe (StableName (M.Map Int Document),Async (M.Map FilePath (Bool,Int,Int))))
  , sidebarRevision :: !Integer, inputForm :: !(Maybe (Form.PreparedForm SidebarContext SidebarReply))
  , inputPopup :: !(Maybe (Hide.AgentHub.AgentHub,ChoicePopupTarget))
  , editorBindings :: !(M.Map Editor.DraftRef (PluginWindow.WindowRef,Editor.PreparedEditor SidebarContext SidebarReply)) }
-- Prepared alongside visible rows; owner applies at most four branch changes.
data RecoveryProjection = RecoveryProjection !(Maybe SidebarHints)
  ![(P.TreeHit,[P.TreeHit],Bool)] !(Maybe RowKey) !(Maybe RowKey)
data Cancellation = forall a. Cancellation (Async a)
data SidebarHost = SidebarHost !(Registry SidebarContext) !(IORef State)
  !(TBQueue Publication) !(TBQueue Cancellation) !(Async ()) !(TVar Bool)

sidebarRegistry :: SidebarHost -> Registry SidebarContext
sidebarRegistry (SidebarHost registry _ _ _ _ _)=registry
-- | Public contribution capabilities reuse this host's ordered close-aware
-- queue. The host retains all currentness, privacy and presentation decisions.
sidebarCapabilities :: SidebarHost -> PluginSidebar.Sidebar SidebarContext SidebarReply
sidebarCapabilities host=PluginSidebar.Sidebar sidebarOrigin sidebarContextWorkspace SidebarForm SidebarPopupForm
  (publishTreeFromHost host) (publishFormRefreshFromHost host) (invalidateTree host)

-- A preparation worker publishes invalidation through the same bounded queue as
-- a tree or form. Shutdown wakes a blocked publisher and rejects its retained hit.
invalidateTree :: SidebarHost -> P.TreeRef -> P.NodeId -> IO ()
invalidateTree (SidebarHost _ _ queue _ _ closed) owner node=atomically $ do
  stopped<-readTVar closed
  if stopped then throwSTM (userError "Sidebar host closed.") else writeTBQueue queue (TreeInvalidation owner node)

withSidebarCommands :: (SidebarHost -> IO a) -> IO a
withSidebarCommands use=PluginWindow.withWindowScope $ \scope->withRegistry $ \registry->bracket (acquire scope registry) close use
  where
    acquire scope registry=do
      state<-newIORef (State scope M.empty M.empty [] [] Nothing Nothing [] Nothing Nothing Nothing 0 Nothing Nothing M.empty)
      publications<-newTBQueueIO 32
      cancellation<-newTBQueueIO 32
      closed<-newTVarIO False
      canceller<-async (forever (do Cancellation worker<-atomically (readTBQueue cancellation); cancel worker))
      pure (SidebarHost registry state publications cancellation canceller closed)
    close (SidebarHost _ ref publications _ canceller closed)=do
      atomically (writeTVar closed True >> flushTBQueue publications >> pure ())
      state<-readIORef ref
      mapM_ (\(ChildJob _ _ worker _)->cancel worker) (jobs state)
      mapM_ (cancel . snd) (projection state)
      mapM_ (cancel . actionWorker) (actionJob state)
      modifyIORef' ref (\current->current {actionJob=Nothing,pendingFiles=[]})
      mapM_ (Form.retireForm . Form.formReference) (inputForm state)
      mapM_ Editor.retireDraftRef (M.keys (editorBindings state))
      mapM_ (cancel . snd) (badgeJob state)
      cancel canceller

-- | Register/prepare metadata outside the owner, then publish an ordered bounded
-- delta. Backpressure applies to the registration worker, never an input tick.
-- Closing the host wakes blocked publishers and rejects them with an IOError.
publishTreeFromHost :: SidebarHost -> P.TreeProvider SidebarContext SidebarReply -> IO ()
publishTreeFromHost (SidebarHost _ _ queue _ _ closed) provider=atomically $ do
  stopped<-readTVar closed
  if stopped then throwSTM (userError "Sidebar host closed.") else writeTBQueue queue (TreePublication provider)
-- | Queue metadata for an already installed exact form. This cannot open a
-- modal or grant submission authority; ordered transport reuses the root queue.
-- Blocked and later refresh publishers receive an IOError when the host closes.
publishFormRefreshFromHost :: SidebarHost -> Form.FormUpdate -> IO ()
publishFormRefreshFromHost (SidebarHost _ _ queue _ _ closed) prepared=atomically $ do
  stopped<-readTVar closed
  if stopped then throwSTM (userError "Sidebar host closed.") else writeTBQueue queue (FormRefresh prepared)
actionWorker :: ActionJob -> Async (Either CommandError SidebarReply)
actionWorker (ActionJob _ _ _ _ worker _)=worker
actionWorker (FileJob _ _ _ worker _)=worker
actionWorker (ExportJob _ _ _ _ worker _)=worker
actionWorker (FormJob _ worker _)=worker
actionWorker (EditorJob _ worker _)=worker
-- | Withdrawal belongs to the session owner, so queued/late results cannot race
-- adoption. Cancellation is scheduled to a worker and never waits under UI lock.
retireTreeFromHost :: SidebarHost -> P.TreeRef -> Desktop -> IO Desktop
retireTreeFromHost (SidebarHost _ ref _ _ _ _) owner d=do
  state<-readIORef ref
  mapM_ P.retireTree (M.lookup owner (providers state))
  writeIORef ref state {providers=M.delete owner (providers state)}
  pure d {sideTree=fmap (removeRoot owner) (sideTree d),contextMenu=Nothing,contextTarget=Nothing}

context :: Menu.MenuOrigin -> Desktop -> SidebarContext
context origin d=SidebarContext origin (maybe (startingDirectory d) treeRoot (sideTree d)) (startingDirectory d) Nothing (privateFilePaths d) (max 20 (min 76 (fst (screenSize d)-treeWidthOf d-4))) Nothing Nothing [] (fst (pendingFileExport d)) (sessionAttachment d) (Left "No captured conversation session.") (Left "No captured conversation operation.") (Left "No captured conversation agent.")

-- | Shallow immutable menu input for this same form owner. It retains no desktop,
-- source contents or Undo. The menu owner supplies its captured session receipt.
sidebarInvocationContext :: Menu.MenuOrigin -> Desktop -> SidebarContext
sidebarInvocationContext origin d=SidebarContext origin workspace workspace Nothing []
  (max 20 (min 76 (fst (screenSize d)-treeWidthOf d-4))) Nothing Nothing [] 0 (sessionAttachment d)
  (Left "No captured conversation session.") (Left "No captured conversation operation.") (Left "No captured conversation agent.")
  where workspace=startingDirectory d
metadata :: P.NodeDef c r -> (P.NodeInfo,Maybe CommandRef,[(Text,P.TreeMenuTarget)])
metadata node=(P.nodeInfo node,fmap P.actionReference (P.nodeAction node),map P.menuTarget (P.nodeMenus node))
addProvider :: P.TreeProvider SidebarContext SidebarReply -> Sidebar -> Sidebar
addProvider provider tree=let (info,action,actions)=metadata (P.treeRoot provider)
  in addRoot (P.treeReference provider) info action actions tree

-- | Prepare a captured file action on its worker. Reuse the existing buffer when
-- its captured identity is still current; adoption performs that check. New reads
-- recheck canonical privacy before preparing a document, never under the UI lock.
prepareSidebarFile :: SidebarContext -> FilePath -> IO (Either CommandError SidebarReply)
prepareSidebarFile ctx path
  | Just (captured,wid,reference)<-sidebarOpenedImage ctx,captured==path=
      pure (if sidebarOrigin ctx==Menu.HumanMenu then Right (SidebarExistingImage path wid reference) else Left (CommandRejected "Opening an image requires the human."))
  | otherwise=case sidebarOpened ctx of
    Just (captured,wid,bid,version) | captured==path->pure (Right (SidebarExisting path wid bid version))
    _->do
      resolved<-canonicalizePath path
      if sidebarOrigin ctx==Menu.AgentMenu && protectedFilePath (sidebarPrivatePaths ctx) resolved
        then pure (Left (CommandRejected "Agent file target is protected.")) else do
          result<-loadFileForDisplay resolved
          case result of
            Left err->pure (Left (CommandRejected (T.pack err)))
            Right (ImageFile actual bytes)->prepareImageReply ctx (Just actual) (T.pack (takeFileName actual)) bytes
            Right (BufferedFile file buffer)
              | sidebarOrigin ctx==Menu.AgentMenu && protectedFilePath (sidebarPrivatePaths ctx) (filePath file)->pure (Left (CommandRejected "Agent file target is protected."))
              | otherwise->do
                  _<-evaluate (prepareBuffer buffer)
                  doc<-evaluate (newDocument buffer (Just file))
                  pure (Right (SidebarDocument (filePath file) doc))

-- Docs: tools/docs-screenshots.hs png-view exercises this through ordinary Files opening.
prepareImageReply :: SidebarContext -> Maybe FilePath -> Text -> BS.ByteString -> IO (Either CommandError SidebarReply)
prepareImageReply ctx path title bytes
  | sidebarOrigin ctx/=Menu.HumanMenu=pure (Left (CommandRejected "Opening an image requires the human."))
  | otherwise=do
      let disclosure=if maybe False (protectedFilePath (sidebarPrivatePaths ctx)) path then PluginWindow.PrivateWindow else PluginWindow.ReadableWindow
      prepared<-PluginWindow.prepareImageWindow title disclosure path bytes
      case prepared of
        Right image->pure (Right (SidebarImage path image))
        Left _->do
          let buffer=fileBuffer bytes
          _<-evaluate (prepareBuffer buffer)
          doc<-evaluate (restyle (newDocument buffer ((\name->FileState name (Just bytes)) <$> path)) {documentSuggestedName=if path==Nothing then Just (T.unpack title) else Nothing})
          pure (Right (maybe (SidebarUpload doc) (\name->SidebarDocument name doc) path))

prepareUpload :: SidebarContext -> Text -> BS.ByteString -> IO (Either CommandError SidebarReply)
prepareUpload ctx name bytes
  | BS.length bytes>16777216=pure (Left (CommandRejected "Dropped file exceeds the 16 MiB limit."))
  | isImageContent bytes=prepareImageReply ctx Nothing name bytes
  | otherwise=do
      let buffer=fileBuffer bytes
      _<-evaluate (prepareBuffer buffer)
      doc<-evaluate (restyle (newDocument buffer Nothing) {documentSuggestedName=Just (T.unpack name)})
      pure (Right (SidebarUpload doc))

-- Files is declared through precisely the public provider/action route.
createFiles :: SidebarHost -> FilePath -> IO FilesProvider
createFiles host root=do
  let registry=sidebarRegistry host
  rootId<-either (ioError . userError . T.unpack) pure (P.nodeId "root")
  cache<-newIORef (M.singleton rootId root,M.singleton root rootId,1,M.empty)
  owner<-newIORef Nothing
  open<-either (ioError . userError . show) pure =<< registerCommand registry (CommandDef "hide.sidebar.files.open" "Open file" codec codec prepareSidebarFile)
  export<-either (ioError . userError . show) pure =<< registerCommand registry
    (CommandDef "hide.sidebar.files.export" "Export saved copy" codec codec (\ctx path->
      if sidebarOrigin ctx/=Menu.HumanMenu then pure (Left (CommandRejected "File export requires the human.")) else do
        resolved<-canonicalizePath path
        exists<-doesFileExist resolved
        if not exists then pure (Left (CommandRejected "File export needs an existing saved file.")) else
          do
            bytes<-withFileRead resolved (\h->BS.hGet h (16*1024*1024))
            pure $ if BS.length bytes>=16*1024*1024 then Left (CommandRejected "File export exceeds the 16 MiB limit.")
              else Right (SidebarExportFile (sidebarExportEpoch ctx) (T.pack (takeFileName resolved)) bytes)))
  renameTo<-either (ioError . userError . show) pure =<< registerCommand registry
    (CommandDef "hide.sidebar.files.rename-to" "Rename file" codec codec (\ctx (source,name)->
      if sidebarOrigin ctx/=Menu.HumanMenu then pure (Left (CommandRejected "File rename requires the human.")) else do
        prepared<-tryIOError (Rename.prepareBasenameRename source name)
        current<-readIORef owner
        pure $ case (prepared,current) of
          (Right value,Just reference)->Right (SidebarRename reference value)
          (Left err,_)->Left (CommandRejected (T.pack (show err)))
          _->Left (CommandRejected "Files provider expired.")))
  rename<-either (ioError . userError . show) pure =<< registerCommand registry
    (CommandDef "hide.sidebar.files.rename" "Rename" codec codec (\ctx path->
      if sidebarOrigin ctx/=Menu.HumanMenu then pure (Left (CommandRejected "File rename requires the human.")) else do
        source<-tryIOError (Rename.prepareRenameSource True root (sidebarPrivatePaths ctx) path (sidebarRenameFiles ctx))
        case source of
          Left err->pure (Left (CommandRejected (T.pack (show err))))
          Right captured->do
            prepared<-Form.prepareForm Form.PrivateForm (Form.InputFormSpec "Rename file" "Name" (T.pack (takeFileName (Rename.renameSourcePath captured))) "Rename")
              (Form.formAction registry renameTo (\name->(captured,name)) (\_ value->pure value))
            pure (SidebarForm <$> prepared)))
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
                          ([P.ResourceMenu (if map toLower (takeExtension resource) `elem` [".md",".markdown"] then "Open" else "Open externally") resource "" | not (entryDirectory entry),map toLower (takeExtension resource) `elem` [".md",".markdown",".png",".jpg",".jpeg",".gif",".webp",".bmp",".svg",".pdf"]]++
                           [P.ActionMenu "Export saved copy…" (P.treeAction registry export resource (\_ value->pure value)) | not (entryDirectory entry)]++
                           [P.ActionMenu "Rename…" (P.treeAction registry rename resource (\_ value->pure value)) | not (entryDirectory entry)]))
                      -- Cache bounded directory pages at the filesystem owner. Old
                      -- directories may be re-enumerated after their cache expires.
                      atomicModifyIORef' cache $ \(a,b,c,dirs)->((M.insert ident base a,M.insert base ident b,c,M.insert base visible (if M.size dirs>=32 then M.empty else dirs)),())
                      pure (Right (P.NodePage values (if length (drop (offset+128) visible)>0 then Just (T.pack (show (offset+128))) else Nothing))))
  writeIORef owner (Just (P.treeReference provider))
  pure (FilesProvider provider root (commandRef open) (commandRef rename) (commandRef renameTo) (commandRef export) cache)
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
readState (SidebarHost _ ref _ _ _ _)=readIORef ref

sidebarEffects :: SidebarHost -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
sidebarEffects host@(SidebarHost _ ref _ _ _ closed) core d effects=do
  stopped<-readTVarIO closed
  when stopped (ioError (userError "Sidebar host closed."))
  mounted<-mount host d
  foldM step (False,mounted) effects >>= \(quit,next)->(quit,) <$> (mount host next >>= rememberSidebar host)
  where
    step result@(True,_) _=pure result
    step (_,current) effect=case effect of
      OpenFile origin path->do
        directory<-doesDirectoryExist path
        if directory then core current [ReadPath path] else do
          resolved<-tryIOError (canonicalizePath path)
          case resolved of
            Left err->pure (False,current {status="Cannot open file: "<>T.pack (show err)})
            Right actual->(False,) <$> queueFileOpen host (FilePathRequest origin actual) current
      ExportBufferDocument wid bid->(False,) <$> exportBuffer host wid bid current
      OpenFileBytes name bytes->(False,) <$> queueFileOpen host (FileBytesRequest name bytes) current
      OpenChoice origin base input pattern->do
        let chosen=if T.null input then pattern else input
            path=if isAbsolute (T.unpack chosen) then T.unpack chosen else base </> T.unpack chosen
        directory<-doesDirectoryExist path
        if directory then core current [BrowsePath path pattern]
        else if T.any (`elem` ("*?"::String)) chosen then core current [BrowsePath (takeDirectory path) (T.pack (takeFileName path))]
        else do
          exists<-doesFileExist path
          if exists then step (False,current {dialog=Nothing}) (OpenFile origin path)
            else pure (False,browserError "File not found." current)
      SubmitEditor editor slot origin->do
        state<-readIORef ref
        if M.member (Editor.mountDraft editor) (editorBindings state) then (False,) <$> submitEditor host editor slot origin current else core current [effect]
      LoadTree request origin->(False,) <$> enqueue host request origin current
      InvokeTree trace reference origin->(False,) <$> invokeAction host trace reference origin current
      SubmitChoiceForm reference version selected origin->(False,) <$> submitChoiceForm host reference version selected origin current
      SubmitPopupChoiceForm reference version selected origin->(False,) <$> submitPopupChoiceForm host reference version selected origin current
      SubmitInputForm reference text origin->(False,) <$> submitForm host reference text origin current
      RetireInputForm reference->do
        Form.retireForm reference
        pure (False,current)
      RefreshTree path entries->(False,) <$> refreshFiles host path entries current
      RefreshRenamedPath old new->(False,) <$> refreshRenamedPath host old new current
      _->core current [effect]

mount :: SidebarHost -> Desktop -> IO Desktop
mount host@(SidebarHost _ ref _ _ _ _) d=case sideTree d of
  Nothing->pure d
  Just visible->do
    state<-readIORef ref
    live<-filterM P.treeCurrent (M.elems (providers state))
    let epoch=sidebarRevision state+1
        basis=if M.null (treeNodes visible) then visible {treeEpoch=epoch,treeRevision=epoch} else visible
        tree=foldl (flip addProvider) basis live
    case filesProvider state of
      Just (FilesProvider provider root _ _ _ _ _) | root==treeRoot tree->do
        let key=NodeKey (P.treeReference provider) (P.infoId (P.nodeInfo (P.treeRoot provider)))
            next=if M.member (P.treeReference provider) (providers state) then addProvider provider tree else tree
        if M.member key (treeNodes visible) then pure d {sideTree=Just next}
          else startRoot provider next
      _->do
        -- This path is startup/directory selection, which already belongs to the
        -- host's file effect owner. Registration and root validation are bounded.
        mapM_ (\(FilesProvider provider _ open rename renameTo export _)->P.retireTree provider >> mapM_ (retireCommand (sidebarRegistry host)) [open,rename,renameTo,export]) (filesProvider state)
        created@(FilesProvider provider _ _ _ _ _ _)<-createFiles host (treeRoot tree)
        let owner=P.treeReference provider
            withdrawn=maybe tree (\(FilesProvider old _ _ _ _ _ _)->removeRoot (P.treeReference old) tree) (filesProvider state)
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
rememberSidebar (SidebarHost _ ref _ _ _ _) d=do
  mapM_ (\tree->modifyIORef' ref (\state->state {sidebarRevision=max (sidebarRevision state) (treeRevision tree)})) (sideTree d)
  pure d

enqueue :: SidebarHost -> TreeRequest -> Menu.MenuOrigin -> Desktop -> IO Desktop
enqueue (SidebarHost _ ref _ _ _ _) request origin d=case sideTree d of
  Just tree | requestCurrent request tree->do
    state<-readIORef ref
    let P.TreeHit owner _ _=requestHit request
        permitted=origin==Menu.HumanMenu || owner `elem` treeAgentRefs tree && maybe False (not . protectedPath d) (P.infoResource =<< fmap stateInfo (nodeAt (requestHit request) tree))
        duplicate=any (\(ChildJob active _ _ _)->active==request) (jobs state) || any ((==request).fst) (waiting state)
    if not permitted then pure d {sideTree=Just (failRequest request "Protected sidebar target." tree)}
    else if duplicate then pure d else if length (waiting state)>=64 then pure d {sideTree=Just (failRequest request "Sidebar loader is busy; retry." tree)} else do
      writeIORef ref state {waiting=waiting state++[(request,origin)]}
      -- Origin is acquired only by this permitted, nonduplicate admission.
      -- Projection snapshots contain nodes, so a changed receipt advances revision.
      let key=keyOf (requestHit request)
          node=treeNodes tree M.! key
          changed=tree {treeNodes=M.insert key node {stateLoadOrigin=Just origin} (treeNodes tree),
            treeRevision=treeRevision tree+if stateLoadOrigin node==Just origin then 0 else 1}
      pure d {sideTree=Just changed}
  _->pure d

invokeAction :: SidebarHost -> [P.TreeHit] -> CommandRef -> Menu.MenuOrigin -> Desktop -> IO Desktop
invokeAction (SidebarHost _ ref _ _ _ _) trace reference origin d=case (trace,sideTree d) of
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
        captured<-captureActionContext origin trace d
        files<-case filesProvider state of
          Just (FilesProvider _ _ _ rename _ _ _) | origin==Menu.HumanMenu,reference==rename->Rename.captureRenameFiles d
          _->pure []
        let ctx=captured {sidebarRenameFiles=files}
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
  opened<-case target >>= \path->(path,) <$> find (\(_,doc)->fmap filePath (documentFile doc)==Just path && not (maybe False isImageContent (documentFile doc >>= diskBytes))) (M.toList (buffers d)) of
    Just (path,(bid,doc)) | Just window<-find ((==Just bid) . bufferId) (windows d)->do
      version<-captureVersion (documentBuffer doc)
      pure (Just (path,windowId window,bid,version))
    _->pure Nothing
  pure (context origin d) {sidebarOpened=opened,sidebarOpenedImage=target >>= capturedImage d,sidebarProvider=case trace of P.TreeHit owner _ _:_->Just owner; _->Nothing}

-- Forget only moved-subtree identities/enumerations; unaffected sibling IDs
-- remain stable. Existing provider workers refresh both parent listings.
refreshRenamedPath :: SidebarHost -> FilePath -> FilePath -> Desktop -> IO Desktop
refreshRenamedPath host@(SidebarHost _ ref _ _ _ _) old new d=do
  state<-readIORef ref
  case filesProvider state of
    Just (FilesProvider provider _ _ _ _ _ cache)->do
      let parents=nub [takeDirectory old,takeDirectory new]
          moved path=Rename.within old path || Rename.within new path
      paths<-atomicModifyIORef' cache $ \(a,b,c,dirs)->
        ((M.filter (not . moved) a,M.filterWithKey (\path _->not (moved path)) b,c,
          M.filterWithKey (\path _->path `notElem` parents && not (moved path)) dirs),b)
      foldM (\current path->case M.lookup path paths of
        Just ident->refreshTreeFromHost host (P.treeReference provider) ident current
        Nothing->pure current) d parents
    _->pure d

refreshFiles :: SidebarHost -> FilePath -> [Entry] -> Desktop -> IO Desktop
refreshFiles host@(SidebarHost _ ref _ _ _ _) path entries d=do
  state<-readIORef ref
  case (filesProvider state,sideTree d) of
    (Just (FilesProvider provider _ _ _ _ _ cache),Just tree)->do
      (_,paths,_,_)<-readIORef cache
      atomicModifyIORef' cache (\(a,b,c,dirs)->((a,b,c,M.insert path entries (if M.size dirs>=32 && not (M.member path dirs) then M.empty else dirs)),()))
      case M.lookup path paths >>= \ident->let key=NodeKey (P.treeReference provider) ident in (key,) <$> M.lookup key (treeNodes tree) of
        Just (NodeKey owner ident,_) -> refreshTreeFromHost host owner ident d
        _->pure d
    _->pure d

-- | Invalidate one expanded scoped node through the ordinary request owner.
-- Nothing runs a provider here; closed nodes load the latest metadata on expansion.
-- Automatic reload keeps the admitted origin. With no receipt it only invalidates;
-- explicit expansion, startup and recovery perform their own fresh admissions.
refreshTreeFromHost :: SidebarHost -> P.TreeRef -> P.NodeId -> Desktop -> IO Desktop
refreshTreeFromHost host@(SidebarHost _ ref _ _ _ _) owner ident d=do
  state<-readIORef ref
  live<-maybe (pure False) P.treeCurrent (M.lookup owner (providers state))
  case sideTree d of
    Just tree | live,Just node<-M.lookup key (treeNodes tree),stateExpanded node->do
      let invalid=collapseNode (nodeHit key node) tree
          current=treeNodes invalid M.! key
          cleared=d {sideTree=Just invalid,contextMenu=Nothing,contextTarget=Nothing}
      case stateLoadOrigin node of
        Nothing->pure cleared
        Just origin->let (changed,request)=requestChildren (nodeHit key current) Nothing invalid
          in maybe (pure cleared) (\value->enqueue host value origin cleared {sideTree=Just changed}) request
    _->pure d
  where key=NodeKey owner ident

-- At most four loads, one projection, one action and one badge computation.
-- Publication drains four deltas per tick. All queues and retained UI nodes have
-- explicit ceilings; slow providers cannot starve input with a recursive drain.
tickSidebar :: SidebarHost -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO Desktop
tickSidebar host@(SidebarHost _ ref publications cancellation _ closed) core initial=
  readTVarIO closed >>= \stopped->if stopped then pure initial else do
  validEditors<-tickEditors host initial
  validForm<-tickForm host validEditors
  mounted<-mount host validForm
  published<-foldM (\d _->do
    supplied<-atomically (tryReadTBQueue publications)
    case supplied of
      Nothing->pure d
      Just (FormRefresh update)->refreshForm host update d
      Just (TreeInvalidation reference node)->refreshTreeFromHost host reference node d
      Just (TreePublication provider)->do
        live<-P.treeCurrent provider
        registered<-readIORef ref
        let accepted=live && (M.member (P.treeReference provider) (providers registered) || M.size (providers registered)<32)
        if not accepted then pure d {status=if live then "Sidebar provider budget reached." else status d} else do
          let reference=P.treeReference provider; root=P.treeRoot provider; key=NodeKey reference (P.infoId (P.nodeInfo root))
          modifyIORef' ref (\s->s {providers=M.insert reference provider (providers s),definitions=M.insert key root (definitions s)})
          pure d {sideTree=fmap (addProvider provider) (sideTree d),contextMenu=Nothing,contextTarget=Nothing}) mounted [1..4::Int]
  registered<-readIORef ref
  withdrawn<-foldM (\d (reference,provider)->do
    live<-P.treeCurrent provider
    if live then pure d else retireTreeFromHost host reference d) published (M.toList (providers registered))
  state<-readIORef ref
  (loaded,retained)<-foldM (finishChild state) (withdrawn,[]) (jobs state)
  modifyIORef' ref (\s->s {jobs=reverse retained})
  adopted<-finishAction host core loaded >>= startPendingFile host
  projected<-finishProjection host adopted
  restarted<-startLoads host projected
  startProjection host restarted
  badges host restarted >>= rememberSidebar host
  where
    finishChild state (d,keep) job@(ChildJob request origin worker cancelled)=do
      live<-maybe (pure False) P.treeCurrent (M.lookup (owner request) (providers state))
      allowed<-if origin==Menu.HumanMenu then pure True else case filesProvider state of
        Just (FilesProvider provider _ _ _ _ _ cache) | P.treeReference provider==owner request->do
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
startLoads host@(SidebarHost _ ref _ _ _ _) original=do
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
finishProjection host@(SidebarHost _ ref _ _ _ _) d=do
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
startProjection (SidebarHost _ ref _ _ _ _) d=do
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
adoptRecovery host@(SidebarHost _ ref _ _ _ _) (RecoveryProjection hints transitions selected top) original=do
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
finishAction host@(SidebarHost _ ref _ cancellation _ _) core d=do
  state<-readIORef ref
  case actionJob state of
    Nothing->pure d
    Just (FileJob origin active serial worker cancelled)->finishFileJob host origin active serial worker cancelled d
    Just (ExportJob wid bid version view worker cancelled)->finishExport host wid bid version view worker cancelled d
    Just (FormJob reference worker cancelled)->finishFormJob host core d reference worker cancelled
    Just (EditorJob submitted worker cancelled)->finishEditorJob host core d submitted worker cancelled
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
            Right (Right reply@SidebarExisting{}) | current->adoptFileReply host origin reply d
            Right (Right reply@SidebarDocument{}) | current->adoptFileReply host origin reply d
            Right (Right reply@SidebarExistingImage{}) | current->adoptFileReply host origin reply d
            Right (Right reply@SidebarImage{}) | current->adoptFileReply host origin reply d
            Right (Right (SidebarExportFile epoch name bytes)) | current && origin==Menu.HumanMenu && epoch==fst (pendingFileExport d)->
              case (trace,sideTree d) of
                (hit:_,Just tree) | Just index<-M.lookup (keyOf hit) (treeNodes tree) >>= (\node->M.lookupIndex (stateAddress node) (treeRows tree)),
                  index>=treeScroll tree,index<treeScroll tree+treeContentRows d ->
                    let row=Rect 1 (index-treeScroll tree+2) (max 1 (treeWidth tree-3)) 1
                    in pure d {pendingFileExport=(epoch+1,Just (ExportFileCopy name bytes row (fileExportView d {pendingFileExport=(epoch+1,Nothing)}))),status="Saved copy ready for export; drag the selected file on macOS."}
                _->pure d {status="Sidebar export expired."}
            Right (Right (SidebarPackageDebug target entry)) | current && origin==Menu.HumanMenu->snd <$> core d [PackageDebugAction target entry]
            Right (Right (SidebarBuild action target)) | current && origin==Menu.HumanMenu->snd <$> core d [PackageBuildAction action target]
            Right (Right (SidebarDebug request)) | current && origin==Menu.HumanMenu->snd <$> core d [DebugSidebarAction request]
            Right (Right (SidebarSession request)) | current && origin==Menu.HumanMenu->snd <$> core d [SessionSidebarAction request]
            Right (Right (SidebarRecoveredSources attachment workspace sources))
              | current && origin==Menu.HumanMenu && attachment==sessionAttachment d && workspace==sidebarContextDirectory (context origin d) &&
                not (questionActive d) && pendingSessionSwitch d==Nothing->pure (Recovery.adoptRecoveredSources sources d)
            Right (Right (SidebarWindow request)) | current->adoptWindowUpdate origin request d
            Right (Right (SidebarEditorWindow request)) | current->adoptEditor host origin request d
            Right (Right (SidebarForm prepared)) | current && origin==Menu.HumanMenu->adoptForm host True prepared d
            Right (Right (SidebarAgent request)) | current && origin==Menu.HumanMenu->snd <$> core d [AgentSidebarAction request]
            _->pure $ if not current then d {status="Sidebar result expired."} else case result of
              Left err->d {status="Sidebar action failed: "<>T.pack (displayException err)}
              Right (Left err)->d {status="Sidebar action failed: "<>T.pack (show err)}
              Right (Right SidebarExportFile{})->d {status="Sidebar export expired."}
              Right (Right SidebarExisting{})->d {status="Sidebar result expired."}
              Right (Right SidebarPackageDebug{})->d {status="Sidebar result expired."}
              Right (Right SidebarBuild{})->d {status="Sidebar result expired."}
              Right (Right SidebarDebug{})->d {status="Sidebar result expired."}
              Right (Right SidebarSession{})->d {status="Sidebar result expired."}
              Right (Right SidebarRecoveredSources{})->d {status="Recovery result expired."}
              Right (Right SidebarWindow{})->d {status="Sidebar result expired."}
              Right (Right SidebarEditorWindow{})->d {status="Sidebar result expired."}
              Right (Right SidebarEditorUpdate{})->d {status="Editor result has no submitted job."}
              Right (Right SidebarForm{})->d {status="Sidebar form expired."}
              Right (Right SidebarAgent{})->d {status="Sidebar result expired."}
              Right (Right SidebarPopupForm{})->d {status="Popup form requires its captured menu owner."}
              Right (Right SidebarConversation{})->d {status="Conversation session result requires its menu owner."}
              Right (Right SidebarRename{})->d {status="Sidebar result expired."}
              Right (Right (SidebarPrepared value))->fst (applyLink value d)
              Right (Right SidebarDocument{})->d {status="Sidebar result expired."}
              Right (Right SidebarExistingImage{})->d {status="Sidebar result expired."}
              Right (Right SidebarImage{})->d {status="Sidebar result expired."}
              Right (Right SidebarUpload{})->d {status="Dropped file has no opening request."}

-- Source export borrows an immutable buffer and scalar UI receipt. Preparation
-- is bounded on the existing action worker; adoption never walks its contents.
exportBuffer :: SidebarHost -> Int -> Int -> Desktop -> IO Desktop
exportBuffer (SidebarHost _ ref _ _ _ _) wid bid d=do
  state<-readIORef ref
  case (actionJob state,activeWindow d,M.lookup bid (buffers d)) of
    (Nothing,Just window,Just doc) | windowId window==wid,bufferId window==Just bid,
      commandEnabled d ExportBuffer->do
        version<-captureVersion (documentBuffer doc)
        let captured=documentBuffer doc
            name=T.pack (maybe (fromMaybeName captured (documentSuggestedName doc)) (takeFileName.filePath) (documentFile doc))
            serial=fst (pendingFileExport d)
        worker<-async $ do
          prepared<-evaluate (bufferExportBytes captured)
          pure (SidebarExportFile serial name <$> prepared)
        writeIORef ref state {actionJob=Just (ExportJob wid bid version (fileExportView d) worker False)}
        pure d {status="Preparing buffer copy…"}
    _->pure d {status="Buffer export is unavailable or busy."}
  where
    fromMaybeName buffer=maybe (if byteMode buffer then "NONAME.bin" else "NONAME.HS") takeFileName

bufferExportBytes :: Buffer -> Either CommandError BS.ByteString
bufferExportBytes buffer
  -- Scalar count rejects oversized buffers before flattening; UTF-8 encoding
  -- can require at most four bytes per remaining scalar. All work stays here.
  | bufferLength buffer>=limit=tooLarge
  | BS.length bytes>=limit=tooLarge
  | otherwise=Right bytes
  where
    limit=16*1024*1024
    bytes=bufferBytes buffer
    tooLarge=Left (CommandRejected "Buffer export exceeds 16 MiB; save the file instead.")

finishExport :: SidebarHost -> Int -> Int -> ContentVersion -> [Integer]
  -> Async (Either CommandError SidebarReply) -> Bool -> Desktop -> IO Desktop
finishExport (SidebarHost _ ref _ cancellation _ _) wid bid version view worker cancelled d=do
  same<-case (activeWindow d,M.lookup bid (buffers d)) of
    (Just window,Just doc) | windowId window==wid,bufferId window==Just bid,
      view==fileExportView d,commandEnabled d ExportBuffer,dialog d==Nothing,
      menu d==Nothing,contextMenu d==Nothing->versionCurrent version (documentBuffer doc)
    _->pure False
  let current=same && not cancelled
  completed<-poll worker
  case completed of
    Nothing | not current && not cancelled->do
      queued<-atomically $ do full<-isFullTBQueue cancellation; if full then pure False else writeTBQueue cancellation (Cancellation worker) >> pure True
      modifyIORef' ref (\state->state {actionJob=Just (ExportJob wid bid version view worker queued)})
      pure d {status="Buffer export expired."}
    Nothing->pure d
    Just result->do
      modifyIORef' ref (\state->state {actionJob=Nothing})
      pure $ case result of
        Right (Right (SidebarExportFile serial name bytes)) | current,Just window<-activeWindow d ->
          let r=bounds window
              row=Rect (left r+1) (top r) (max 1 (width r-2)) 1
              offered=d {pendingFileExport=(serial+1,Nothing)}
          in offered {pendingFileExport=(serial+1,Just (ExportFileCopy name bytes row (fileExportView offered))),
            status="Buffer copy ready; drag its title on macOS, or use the frontend's export control."}
        _ | not current->d {status="Buffer export expired."}
        Left err->d {status="Buffer export failed: "<>T.pack (displayException err)}
        Right (Left err)->d {status="Buffer export failed: "<>T.pack (show err)}
        _->d {status="Buffer export reply was invalid."}

-- Ordinary opens share the existing single action worker. A bounded FIFO keeps
-- drop bursts in order; only that worker's own successful openings advance the
-- captured target of its queued siblings. Unrelated focus/modal changes expire it.
queueFileOpen :: SidebarHost -> FileRequest -> Desktop -> IO Desktop
queueFileOpen host@(SidebarHost _ ref _ _ _ _) request d=do
  state<-readIORef ref
  let pending=PendingFile (windowId <$> activeWindow d) (nextId d) request
      bytes (PendingFile _ _ (FileBytesRequest _ value))=BS.length value
      bytes _=0
  if dialog d/=Nothing || questionActive d || activeAutocomplete d
    then pure d {status="File opening is protected."}
    else if length (pendingFiles state)>=16 || sum (map bytes (pending:pendingFiles state))>33554432
      then pure d {status="Pending file opening budget reached."}
      else do
        modifyIORef' ref (\current->current {pendingFiles=pendingFiles current++[pending]})
        startPendingFile host d

startPendingFile :: SidebarHost -> Desktop -> IO Desktop
startPendingFile host@(SidebarHost _ ref _ _ _ _) d=do
  state<-readIORef ref
  case (actionJob state,pendingFiles state) of
    (Nothing,PendingFile active serial request:rest)->do
      modifyIORef' ref (\current->current {pendingFiles=rest})
      if active/=(windowId <$> activeWindow d) || serial/=nextId d || dialog d/=Nothing || questionActive d || activeAutocomplete d
        then startPendingFile host d {status="Queued file opening expired."}
        else do
          let (origin,path)=case request of FilePathRequest who name->(who,Just name); FileBytesRequest{}->(Menu.HumanMenu,Nothing)
          opened<-case path >>= \name->(name,) <$> find (\(_,doc)->fmap filePath (documentFile doc)==Just name && not (maybe False isImageContent (documentFile doc >>= diskBytes))) (M.toList (buffers d)) of
            Just (name,(bid,doc)) | Just window<-find ((==Just bid).bufferId) (windows d)->do
              version<-captureVersion (documentBuffer doc)
              pure (Just (name,windowId window,bid,version))
            _->pure Nothing
          let ctx=(context origin d) {sidebarOpened=opened,sidebarOpenedImage=path >>= capturedImage d}
          worker<-async $ case request of
            FilePathRequest _ name->prepareSidebarFile ctx name
            FileBytesRequest name bytes->prepareUpload ctx name bytes
          modifyIORef' ref (\current->current {actionJob=Just (FileJob origin active serial worker False)})
          pure d {status="Opening file…"}
    _->pure d

capturedImage :: Desktop -> FilePath -> Maybe (FilePath,Int,PluginWindow.WindowRef)
capturedImage d path=do
  window<-find (\w->windowImage d w/=Nothing && (windowPluginText d w >>= PluginWindow.preparedWindowSemantics >>= PluginWindow.textLinkBase)==Just path) (windows d)
  case windowContent window of PluginContent reference->Just (path,windowId window,reference); _->Nothing

-- | Complete the current ordinary file opening before startup/snapshot proceeds.
-- Only startup calls this blocking operation, outside desktop serialization.
-- Runtime input queues the same worker and adopts it through 'tickSidebar'.
awaitFileOpening :: SidebarHost -> Desktop -> IO Desktop
awaitFileOpening host d=do
  state<-readState host
  case actionJob state of
    Just (FileJob origin active serial worker cancelled)->do
      _<-Control.Concurrent.Async.waitCatch worker
      finished<-finishFileJob host origin active serial worker cancelled d >>= startPendingFile host
      awaitFileOpening host finished
    _->pure d

finishFileJob :: SidebarHost -> Menu.MenuOrigin -> Maybe Int -> Int -> Async (Either CommandError SidebarReply) -> Bool -> Desktop -> IO Desktop
finishFileJob host@(SidebarHost _ ref _ cancellation _ _) origin active serial worker cancelled d=do
  let current=not cancelled && active==(windowId <$> activeWindow d) && serial==nextId d && dialog d==Nothing && not (questionActive d || activeAutocomplete d)
  completed<-poll worker
  case completed of
    Nothing | not current && not cancelled->do
      queued<-atomically $ do full<-isFullTBQueue cancellation; if full then pure False else writeTBQueue cancellation (Cancellation worker) >> pure True
      modifyIORef' ref (\state->state {actionJob=Just (FileJob origin active serial worker queued)})
      pure d {status="File opening expired."}
    Nothing->pure d
    Just result->do
      modifyIORef' ref (\state->state {actionJob=Nothing})
      if not current then pure d {status="File opening expired."} else case result of
        Left err->pure d {status="Cannot open file: "<>T.pack (displayException err)}
        Right (Left err)->pure d {status="Cannot open file: "<>T.pack (show err)}
        Right (Right reply)->adoptFileReply host origin reply d

adoptFileReply :: SidebarHost -> Menu.MenuOrigin -> SidebarReply -> Desktop -> IO Desktop
adoptFileReply host@(SidebarHost _ ref _ _ _ _) origin reply d=do
  result<-case reply of
    SidebarExisting path wid bid version->adoptExisting origin path wid bid version d
    SidebarExistingImage path wid reference | origin==Menu.HumanMenu,Just (same,ident,owned)<-capturedImage d path,same==path,ident==wid,owned==reference->do
      live<-PluginWindow.windowRefCurrent reference
      pure (if live then leave (focusWindow wid d) else d {status="Existing image expired."})
    SidebarDocument path doc
      | origin==Menu.AgentMenu && protectedPath d path->pure d {status="File target is now protected."}
      | otherwise->pure $ case find (\(_,opened)->fmap filePath (documentFile opened)==Just path) (M.toList (buffers d)) of
          Just (bid,_) | origin==Menu.AgentMenu && protectedBuffer d bid->d {status="File target is now private."}
          Just (bid,_)->maybe d (\window->leave (focusWindow (windowId window) d)) (find ((==Just bid).bufferId) (windows d))
          Nothing->leave (addDocument (documentFile doc) (documentBuffer doc) d)
    SidebarImage path prepared | origin==Menu.HumanMenu->case path >>= \name->find (sameImage name) (windows d) of
      Just window->pure (leave (focusWindow (windowId window) d))
      Nothing->do
        scope<-imageWindowScope <$> readState host
        request<-PluginWindow.openWindow scope prepared
        maybe (pure d {status="Image window scope closed."}) (fmap leave . (\update->adoptWindowUpdate origin update d)) request
    SidebarUpload doc | origin==Menu.HumanMenu->
      let opened=addDocument Nothing (documentBuffer doc) d
      in pure (leave opened {buffers=M.insert (nextId d) doc (buffers opened),status="Dropped file opened; Download exports changes."})
    _->pure d {status="File opening is protected."}
  let advance (PendingFile expected serial request)
        | expected==(windowId <$> activeWindow d) && serial==nextId d=PendingFile (windowId <$> activeWindow result) (nextId result) request
        | otherwise=PendingFile expected serial request
  modifyIORef' ref (\state->state {pendingFiles=map advance (pendingFiles state)})
  pure result
  where
    leave opened=opened {sideTree=fmap (\tree->tree {treeFocused=False}) (sideTree opened)}
    sameImage path window=windowImage d window/=Nothing &&
      (windowPluginText d window >>= PluginWindow.preparedWindowSemantics >>= PluginWindow.textLinkBase)==Just path

adoptExisting :: Menu.MenuOrigin -> FilePath -> Int -> Int -> ContentVersion -> Desktop -> IO Desktop
adoptExisting origin path wid bid version d=case (find ((==wid).windowId) (windows d),M.lookup bid (buffers d)) of
  (Just window,Just doc) | bufferId window==Just (bid) && fmap filePath (documentFile doc)==Just path->do
    current<-versionCurrent version (documentBuffer doc)
    pure $ if not current || origin==Menu.AgentMenu && (protectedPath d path || protectedBuffer d bid)
      then d {status="Existing sidebar file changed or became private."}
      else let focused=focusWindow wid d in focused {sideTree=fmap (\tree->tree {treeFocused=False}) (sideTree focused)}
  _->pure d {status="Existing sidebar file expired."}

badges :: SidebarHost -> Desktop -> IO Desktop
badges (SidebarHost _ ref _ _ _ _) d=do
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
  Just dg->case purpose dg of PluginInputForm owned->owned==reference; PluginInputsForm owned _->owned==reference; PluginChoiceForm owned _->owned==reference; _->False
  _->False
adoptForm :: SidebarHost -> Bool -> Form.PreparedForm SidebarContext SidebarReply -> Desktop -> IO Desktop
adoptForm (SidebarHost _ ref _ _ _ _) opening prepared d=do
  state<-readIORef ref
  let reference=Form.formReference prepared
      present=maybe False ((==reference).Form.formReference) (inputForm state)
      owned=if opening then dialog d==Nothing && not (questionActive d) && not (activeAutocomplete d) else present && formDialog reference d
  accepted<-if owned then Form.admitForm present prepared else pure False
  if not accepted then pure d else do
    when opening (mapM_ (Form.retireForm . Form.formReference) (inputForm state))
    modifyIORef' ref (\s->s {inputForm=Just prepared,inputPopup=Nothing})
    let spec=Form.formSpec prepared
        choiceIndex value=maybe 0 id (Form.formChoiceIndex prepared value)
        build=case spec of
          Form.ConfirmationFormSpec _ label _->Dialog (Form.formTitle spec) (PluginInputForm reference)
            [] 0 [Form.formSubmit spec,"Cancel"] [label]
          Form.InputFormSpec _ label initial _->Dialog (Form.formTitle spec) (PluginInputForm reference)
            [SelectedInput label initial (Selection 0 (T.length initial))] 0 [Form.formSubmit spec,"Cancel"] []
          Form.InputsFormSpec _ inputs _->Dialog (Form.formTitle spec) (PluginInputsForm reference (map Form.inputId inputs))
            [SelectedInput (Form.inputLabel field) (Form.inputInitial field) (Selection 0 (T.length (Form.inputInitial field))) | field<-inputs]
            0 [Form.formSubmit spec,"Cancel"] []
          Form.ChoiceFormSpec _ label choices initial _->Dialog (Form.formTitle spec) (PluginChoiceForm reference (Form.formRevision prepared))
            [ListBox label (map snd choices) (choiceIndex initial)] 0 [Form.formSubmit spec,"Cancel"] []
        refresh dg=dg {purpose=case spec of Form.ChoiceFormSpec{}->PluginChoiceForm reference (Form.formRevision prepared); _->purpose dg,dialogTitle=Form.formTitle spec,buttons=[Form.formSubmit spec,"Cancel"],body=case spec of Form.ConfirmationFormSpec _ label _->[label]; _->[],fields=case spec of
          Form.ConfirmationFormSpec{}->[]
          Form.InputFormSpec _ label _ _->[case field of SelectedInput _ text selected->SelectedInput label text selected; _->field | field<-fields dg]
          Form.InputsFormSpec _ inputs _->[case field of
            SelectedInput _ text selected->SelectedInput (Form.inputLabel input) text selected
            _->field | (input,field)<-zip inputs (fields dg)]
          Form.ChoiceFormSpec _ label choices _ _->[case field of
            ListBox _ _ selected->
              let ident=inputForm state >>= \old->Form.formChoiceAt old selected
                  retained=maybe 0 choiceIndex ident
              in ListBox label (map snd choices) retained
            _->field | field<-fields dg]}
    pure d {dialog=if opening then Just build else refresh <$> dialog d,contextMenu=Nothing,contextTarget=Nothing}
-- | Popup presentation shares the single installed form and action worker.
-- Only finite-choice metadata may be projected; the fixed hub/configuration and
-- small window/view/geometry receipt govern every adoption and submission.
adoptPopupForm :: SidebarHost -> Hide.AgentHub.AgentHub -> ChoicePopupTarget
  -> Form.PreparedForm SidebarContext SidebarReply -> Desktop -> IO Desktop
adoptPopupForm (SidebarHost _ ref _ _ _ _) hub target prepared d=do
  state<-readIORef ref
  config<-Hide.AgentHub.agentConfigurationCurrent hub (choicePopupConfig target)
  let finite=case Form.formSpec prepared of Form.ChoiceFormSpec{}->True; _->False
  accepted<-if finite && config && choicePopupTargetCurrent target d && contextMenu d==Nothing
    then Form.admitForm False prepared else pure False
  if not accepted then Form.retireForm (Form.formReference prepared) >> pure d {status="Conversation choices expired; invoke them again."} else do
    mapM_ (Form.retireForm . Form.formReference) (inputForm state)
    modifyIORef' ref (\current->current {inputForm=Just prepared,inputPopup=Just (hub,target)})
    pure (installChoicePopup target prepared 0 d)

installChoicePopup :: ChoicePopupTarget -> Form.PreparedForm c r -> Int -> Desktop -> Desktop
installChoicePopup target prepared selected d=opened {contextMenu=fmap (\(rect,_)->(rect,selected)) (contextMenu opened)}
  where
    reference=Form.formReference prepared
    version=Form.formRevision prepared
    opened=openChoicePopup target reference version rows d
    rows=case Form.formSpec prepared of
      Form.ChoiceFormSpec _ _ choices _ _->[(label,FormChoice reference version index) | (index,(_,label))<-zip [0..] choices]
      _->[]

popupInstalled :: Bool -> Form.FormRef -> Integer -> ChoicePopupTarget -> Desktop -> Bool
popupInstalled submitted reference version target d=choicePopupTargetCurrent target d &&
  contextTarget d==Just (ChoicePopupContextTarget target) && case contextKind d of
    FormChoicesContext owned revision _->not submitted && contextMenu d/=Nothing && owned==reference && revision==version
    SubmittedChoicesContext owned revision->submitted && contextMenu d==Nothing && owned==reference && revision==version
    _->False

popupCurrent :: Bool -> Form.PreparedForm c r -> (Hide.AgentHub.AgentHub,ChoicePopupTarget) -> Desktop -> IO Bool
popupCurrent submitted prepared (hub,target) d=do
  config<-Hide.AgentHub.agentConfigurationCurrent hub (choicePopupConfig target)
  pure (config && popupInstalled submitted (Form.formReference prepared) (Form.formRevision prepared) target d)

-- Refresh transport is metadata-only and cannot create a modal by escaped ref.
refreshForm :: SidebarHost -> Form.FormUpdate -> Desktop -> IO Desktop
refreshForm host@(SidebarHost _ ref _ _ _ _) update d=do
  state<-readIORef ref
  case inputForm state of
    Just original | Form.formReference original==Form.updateFormReference update->case inputPopup state of
      Nothing | formDialog (Form.formReference original) d->do
        merged<-Form.admitFormRefresh original update
        maybe (pure d) (\prepared->adoptForm host False prepared d) merged
      Just owner@(_,target)->do
        current<-popupCurrent False original owner d
        merged<-if current then Form.admitFormRefresh original update else pure Nothing
        case merged of
          Nothing->pure d
          Just prepared->do
            let selected=contextMenu d >>= (\(_,index)->Form.formChoiceAt original index) >>= Form.formChoiceIndex prepared
            modifyIORef' ref (\next->next {inputForm=Just prepared})
            pure (installChoicePopup target prepared (maybe 0 id selected) d)
      _->pure d
    _->pure d

tickForm :: SidebarHost -> Desktop -> IO Desktop
tickForm (SidebarHost _ ref _ _ _ _) d=do
  state<-readIORef ref
  case inputForm state of
    Nothing->pure d
    Just prepared->do
      live<-Form.formCurrent prepared
      submitted<-Form.submissionCurrent prepared
      owned<-case inputPopup state of
        Nothing->pure (formDialog (Form.formReference prepared) d || submitted)
        Just owner->popupCurrent submitted prepared owner d
      if live && owned then pure d else do
        Form.retireForm (Form.formReference prepared)
        modifyIORef' ref (\next->next {inputForm=Nothing,inputPopup=Nothing})
        let popupOwned=case contextKind d of
              FormChoicesContext reference _ _->reference==Form.formReference prepared
              SubmittedChoicesContext reference _->reference==Form.formReference prepared
              _->False
        pure (if popupOwned then d {contextMenu=Nothing,contextKind=SourceContext,contextTarget=Nothing,status="Conversation choices expired."}
              else if formDialog (Form.formReference prepared) d then d {dialog=Nothing,status="Input form expired."} else d)
-- Resolve indices only through the exact installed form; labels grant no action.
submitChoiceForm :: SidebarHost -> Form.FormRef -> Integer -> Int -> Menu.MenuOrigin -> Desktop -> IO Desktop
submitChoiceForm host@(SidebarHost _ ref _ _ _ _) reference version selected origin d=do
  state<-readIORef ref
  case inputForm state of
    Just prepared | Form.formReference prepared==reference,Form.formRevision prepared==version,Just value<-Form.formChoiceAt prepared selected->submitForm host reference (Form.TextValue value) origin d
    _->pure d {status="Choice form expired."}
submitPopupChoiceForm :: SidebarHost -> Form.FormRef -> Integer -> Int -> Menu.MenuOrigin -> Desktop -> IO Desktop
submitPopupChoiceForm host@(SidebarHost _ ref _ _ _ _) reference version selected origin d=do
  state<-readIORef ref
  case (inputForm state,inputPopup state) of
    (Just prepared,Just owner) | origin==Menu.HumanMenu,Form.formReference prepared==reference,Form.formRevision prepared==version,Just value<-Form.formChoiceAt prepared selected->do
      current<-popupCurrent True prepared owner d
      if current then startFormSubmission host prepared (Form.TextValue value) (sidebarInvocationContext origin d) d
        else pure d {status="Conversation choice expired."}
    _->pure d {status="Conversation choice expired."}

submitForm :: SidebarHost -> Form.FormRef -> Form.FormValue -> Menu.MenuOrigin -> Desktop -> IO Desktop
submitForm host@(SidebarHost _ ref _ _ _ _) reference value origin d=do
  state<-readIORef ref
  case inputForm state of
    Just prepared | origin==Menu.HumanMenu,Form.formReference prepared==reference,formDialog reference d->
      startFormSubmission host prepared value (sidebarInvocationContext origin d) d
    _->pure d {status="Input form expired."}

startFormSubmission :: SidebarHost -> Form.PreparedForm SidebarContext SidebarReply -> Form.FormValue -> SidebarContext -> Desktop -> IO Desktop
startFormSubmission (SidebarHost _ ref _ _ _ _) prepared value captured d=mask $ \restore->do
  state<-readIORef ref
  case actionJob state of
    Just _->pure d {status=case inputPopup state of Just _->"Sidebar worker is busy; reopen choices."; Nothing->"Sidebar worker is busy; submit again."}
    Nothing->do
      -- Form actions retain their own original target; submission supplies only
      -- shallow human context, detached before the action worker can retain it.
      _<-evaluate captured
      _<-evaluate (length (sidebarContextDirectory captured)+length (sidebarContextWorkspace captured))
      accepted<-Form.claimFormSubmission prepared
      let reference=Form.formReference prepared
      if not accepted then pure d {status="Input form expired."} else do
        worker<-async (restore (Form.invokeFormAction prepared captured value >>= traverse forceFormReply)) `onException` Form.retireForm reference
        modifyIORef' ref (\next->next {actionJob=Just (FormJob reference worker False)})
        pure d {dialog=Nothing,status="Submitting input form..."}
-- Fixed agent/file rename replies require their owning checked adoption routes.
-- Other typed form handlers require their own checked host result route.
-- Fixed replies are forced at their owning worker before the UI receives them.
forceFormReply :: SidebarReply -> IO SidebarReply
forceFormReply (SidebarAgent (RenameAgentTo who value))
  | T.length (Hide.AgentHub.agentIdText who)>128 || T.length value>8192=ioError (userError "Oversized single-line form result.")
  | otherwise=do
      let copied=T.copy value
      _<-evaluate (T.length copied)
      evaluate (SidebarAgent (RenameAgentTo who copied))
forceFormReply (SidebarAgent (CreateAgent workspace name task))
  | length workspace>32768 || T.length name>8192 || T.length task>8192=ioError (userError "Oversized named form result.")
  | otherwise=do
      let copiedName=T.copy name; copiedTask=T.copy task
      _<-evaluate (length workspace+T.length copiedName+T.length copiedTask)
      evaluate (SidebarAgent (CreateAgent workspace copiedName copiedTask))
forceFormReply reply@(SidebarAgent request)=case request of
  ConfigureAgent _ option value->checked option value
  ConfigureCompletion _ option value->checked option value
  _->ioError (userError "Unsupported form reply.")
  where checked option value
          | T.length option>4096 || T.length value>4096=ioError (userError "Oversized choice form result.")
          | otherwise=evaluate (T.length option+T.length value) >> evaluate reply
forceFormReply reply@(SidebarSession (SessionDeleted ident))
  | T.length ident==48=evaluate (T.length ident) >> evaluate reply
  | otherwise=ioError (userError "Invalid deleted session result.")
forceFormReply reply@SidebarRename{}=evaluate reply
forceFormReply reply@SidebarPopupForm{}=evaluate reply
forceFormReply reply@(SidebarConversation request)=do
  case request of
    Conversation.NewConversation{}->pure ()
    Conversation.ResumeConversation _ sid->evaluate (T.length sid) >> pure ()
    Conversation.OpenConversation{}->pure ()
    Conversation.ConfigureConversation _ launch->evaluate (force (Provider.executable launch,Provider.arguments launch,Provider.environment launch)) >> pure ()
    Conversation.OpenConversationContext _ scope->evaluate scope >> pure ()
    Conversation.CopyRawConversation{}->pure ()
  evaluate reply
forceFormReply _=ioError (userError "Unsupported single-line form reply.")
acceptedFormRequest :: AgentSidebarRequest -> Bool
acceptedFormRequest CreateAgent{}=True
acceptedFormRequest RenameAgentTo{}=True
acceptedFormRequest ConfigureAgent{}=True
acceptedFormRequest ConfigureCompletion{}=True
acceptedFormRequest _=False
finishFormJob :: SidebarHost -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> Form.FormRef
  -> Async (Either CommandError SidebarReply) -> Bool -> IO Desktop
finishFormJob host@(SidebarHost _ ref _ cancellation _ _) core d reference worker cancelled=do
  state<-readIORef ref
  live<-case inputForm state of
    Just prepared | Form.formReference prepared==reference->Form.submissionCurrent prepared
    _->pure False
  surface<-case (inputForm state,inputPopup state) of
    (Just prepared,Just owner)->popupCurrent True prepared owner d
    (_,Nothing)->pure True
    _->pure False
  let current=live && surface && not cancelled && dialog d==Nothing
      adopted=case inputPopup state of
        Just _->d {contextKind=SourceContext,contextTarget=Nothing}
        Nothing->d
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
        Right (Right (SidebarPopupForm prepared)) | consumed,Just (hub,target)<-inputPopup state->adoptPopupForm host hub target prepared adopted
        Right (Right (SidebarAgent request)) | consumed,acceptedFormRequest request->snd <$> core adopted [AgentSidebarAction request]
        Right (Right (SidebarSession request@SessionDeleted{})) | consumed->snd <$> core d [SessionSidebarAction request]
        Right (Right (SidebarConversation request)) | consumed->snd <$> core d [ConversationSessionAction request]
        Right (Right (SidebarRename owner prepared)) | consumed->do
          provider<-maybe (pure False) P.treeCurrent (M.lookup owner (providers state))
          if not provider then pure d {status="Files provider expired."} else do
            changed<-Rename.commitWorkspaceRename prepared d
            case changed of
              Left err->pure d {status=err}
              Right renamed->let (old,new)=Rename.renamePaths prepared in
                refreshRenamedPath host old new renamed {status="File renamed."}
        Right (Left err) | consumed->pure d {status="Input form submission failed: "<>T.pack (show err)}
        _->pure d {status=if not consumed then "Input form expired." else "Input form submission failed."}

-- A frame close retains one binding per draft. Scope/registration retirement
-- drops the binding and preserves unsent input as an ordinary private document,
-- even while the draft is hidden. Empty drafts release their host state.
tickEditors :: SidebarHost -> Desktop -> IO Desktop
tickEditors (SidebarHost _ ref _ _ _ _) d=do
  state<-readIORef ref
  expired<-filterM (\(_, (scope,editor))->do
    published<-PluginWindow.windowScopeCurrent scope
    live<-Editor.editorBindingCurrent editor
    pure (not (published && live))) (M.toList (editorBindings state))
  mapM_ (Editor.retireDraftRef . fst) expired
  let removed=map fst expired
  modifyIORef' ref (\s->s {editorBindings=foldr M.delete (editorBindings s) removed})
  pure (preserveEditorDrafts removed d)

adoptEditor :: SidebarHost -> Menu.MenuOrigin -> PluginWindow.EditorWindowUpdate SidebarContext SidebarReply -> Desktop -> IO Desktop
adoptEditor host@(SidebarHost _ ref _ _ _ _) origin update original=do
  d<-tickEditors host original
  state<-readIORef ref
  let editor=PluginWindow.editorWindowEditor update
      draft=Editor.mountDraft (Editor.editorMount editor)
      previous=Editor.editorMount . snd <$> M.lookup draft (editorBindings state)
  (accepted,next)<-if M.size (editorBindings state)>=256 && M.notMember draft (editorBindings state)
    then pure (False,d {status="Editor owner is full."})
    else adoptEditorWindowUpdate origin previous update d
  when accepted (modifyIORef' ref (\s->s {editorBindings=M.insert draft
    (PluginWindow.updateWindowRef (PluginWindow.editorWindowBody update),Editor.installedEditor editor) (editorBindings s)}))
  pure next

submitEditor :: SidebarHost -> Editor.EditorMount -> Editor.EditorSlot -> Menu.MenuOrigin -> Desktop -> IO Desktop
submitEditor (SidebarHost _ ref _ _ _ _) editorMount slot origin d=mask $ \_->do
  state<-readIORef ref
  case M.lookup (Editor.mountDraft editorMount) (editorBindings state) of
    Just (_,editor) | origin==Menu.HumanMenu,activeEditorMount d==Just editorMount,composerActive d,Editor.editorMount editor==editorMount->case actionJob state of
      Just _->pure d {status="Sidebar worker is busy; submit again."}
      Nothing->do
        captured<-Editor.captureDraftSubmission editorMount slot (composerBuffer d)
        case captured of
          Nothing->pure d {status="Editor input expired."}
          Just submitted->do
            worker<-asyncWithUnmask (\unmask->unmask (Editor.invokeEditorAction editor (context origin d) submitted >>= traverse evaluate))
            modifyIORef' ref (\s->s {actionJob=Just (EditorJob submitted worker False)})
            pure d {status="Submitting editor input..."}
    _->pure d {status="Editor input expired."}

-- Once accepted, an action uses its own captured context and command lifetime;
-- tree focus, popup/projection revisions and body refresh cannot revoke it.
finishEditorJob :: SidebarHost -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> Editor.DraftSubmission
  -> Async (Either CommandError SidebarReply) -> Bool -> IO Desktop
finishEditorJob host@(SidebarHost _ ref _ cancellation _ _) core d submitted worker cancelled=do
  state<-readIORef ref
  owning<-case M.lookup (Editor.submissionDraft submitted) (editorBindings state) of
    Just (scope,editor) | Editor.mountActions (Editor.editorMount editor)==Editor.mountActions (Editor.submissionMount submitted)->do
      published<-PluginWindow.windowScopeCurrent scope
      registered<-Editor.editorBindingCurrent editor
      pure (published && registered)
    _->pure False
  live<-Editor.mountCurrent (Editor.submissionMount submitted)
  completed<-poll worker
  case completed of
    Nothing | not live && not cancelled->do
      aborted<-Editor.abortEditorSubmission submitted
      accepted<-Editor.submissionAccepted submitted
      queued<-if not aborted && accepted then pure False else atomically $ do
        full<-isFullTBQueue cancellation
        if full then pure False else writeTBQueue cancellation (Cancellation worker) >> pure True
      modifyIORef' ref (\s->s {actionJob=Just (EditorJob submitted worker queued)})
      pure d
    Nothing->pure d
    Just result->do
      modifyIORef' ref (\s->s {actionJob=Nothing})
      if not owning then pure d {status="Editor owner expired."} else case result of
        Right (Right (SidebarEditorUpdate update))->applyEditorUpdate submitted update d
        Right (Right (SidebarEditorWindow update))->adoptEditor host Menu.HumanMenu update d
        Right (Right (SidebarWindow update))->adoptWindowUpdate Menu.HumanMenu update d
        Right (Right (SidebarForm prepared))->adoptForm host True prepared d
        Right (Right (SidebarAgent request))->snd <$> core d [AgentSidebarAction request]
        Right (Left err)->pure d {status="Editor submission failed: "<>T.pack (show err)}
        Left err->pure d {status="Editor submission failed: "<>T.pack (displayException err)}
        _->pure d {status="Editor result requires its owning operation."}
