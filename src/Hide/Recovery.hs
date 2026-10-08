{-# LANGUAGE CPP, OverloadedStrings, PackageImports #-}
-- | Versioned editor checkpoints and cheap persistence invalidation keys.
--
-- Checkpoints preserve buffers, history, views and preferences, not running
-- background processes. Transient approvals are excluded and private question
-- text is redacted where required; terminals recover as ended documents.
-- Metadata and stable immutable-payload identities decide whether to checkpoint.
-- Publishing uses flush and rename, without an explicit fsync durability promise.
module Hide.Recovery (writeCheckpoint, readCheckpoint, CheckpointKey, checkpointKey) where

import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Hide.Sidebar
import qualified Hide.Plugin.Tree as P
import Control.Exception (IOException, bracket, try, evaluate)
import Control.Monad (unless, when, foldM)
import Hide.Plugin.Command (validCommandName)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Control.Monad.ST (runST)
import Data.STRef
import Data.Word (Word64)
import Data.IORef
import System.Mem.StableName
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Hide.ConversationBody (ConversationBody(..), LogicalBody, BodyItemId(..), Record(..), RecordContent(..),
  BodyPoint(..), BodyAnchor(..), BodySelection(..), logicalBodyItems, logicalItemRecord, logicalBodyIdentity, logicalBodyItemIndex,
  restoreLogicalBody, restoreLogicalViewport, validateLogicalPoint, logicalBodyProvider, clampPoint,
  capturedSourceIdentity, prepareCapturedSource)
import Data.Unique (Unique)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (pathIsSymbolicLink, removeFile, renameFile)
import System.FilePath (isAbsolute, takeDirectory, takeFileName)
import System.IO (IOMode(ReadMode), hClose, hFlush, openBinaryTempFile, withBinaryFile)
import System.IO.Error (catchIOError)
#ifndef mingw32_HOST_OS
import System.Posix.Files (setFileMode)
#endif
import Hide.Buffer
import Hide.BufferView
import Hide.Files (FileState(..))
import Hide.Model
import qualified Hide.Plugin.Window as W
import qualified Hide.Plugin.Canvas as Canvas
import qualified Hide.Plugin.Editor as E

-- Histories are never truncated to fit. A rejected checkpoint leaves the last
-- complete checkpoint in place, and the caller must surface the returned error.
checkpointLimit :: Int
checkpointLimit=256*1024*1024

-- | Validate serialized state before publishing by rename. Oversized output
-- is refused without truncating histories or replacing the previous checkpoint.
writeCheckpoint :: FilePath -> Desktop -> IO (Either Text ())
writeCheckpoint path desktop=do
  current<-prepareRecoverySources desktop
  case validateConversationPoints current of
    Left _->pure (Left "Editor state cannot be represented by a valid recovery checkpoint.")
    Right ()->publish current
  where
    publish current=do
      let encoded=BL.take (fromIntegral checkpointLimit+1) (encode (desktopValue current))
      if BL.length encoded>fromIntegral checkpointLimit then pure (Left "Recovery checkpoint exceeds 256 MiB; no data was truncated.")
        else case eitherDecode encoded >>= parseEither (desktopParser current) of
          Left _->pure (Left "Editor state cannot be represented by a valid recovery checkpoint.")
          Right _->safeIO $ bracket (openBinaryTempFile (takeDirectory path) ".thc-recovery-") cleanup $ \(temporary,handle)->do
#ifndef mingw32_HOST_OS
            setFileMode temporary 0o600
#endif
            BL.hPut handle encoded
            hFlush handle
            hClose handle
            renameFile temporary path
    cleanup (temporary,handle)=ignore (hClose handle) >> ignore (removeFile temporary)

-- The received source may lead its displayed viewport or belong to a closed
-- target. Capture its catalogue on this worker without preparing painted rows.
prepareRecoverySources :: Desktop -> IO Desktop
prepareRecoverySources desktop=do
  views<-mapM prepare (conversationViews desktop)
  pure desktop {conversationViews=views}
  where
    prepare view=case conversationSource view of
      Nothing->pure view
      Just source->do
        latest<-prepareCapturedSource source (conversationLogical view)
        let same=maybe True ((==logicalBodyProvider latest).logicalBodyProvider) (conversationLogical view)
            anchored=case conversationAnchor view of
              At point->maybe FollowEnd At (clampPoint latest point)
              WithinItem ident fraction | logicalBodyItemIndex ident latest/=Nothing->WithinItem ident fraction
              _->FollowEnd
            selected=case conversationReplySelection view of
              Just (BodySelection a z)->BodySelection <$> clampPoint latest a <*> clampPoint latest z
              Nothing->Nothing
        pure view {conversationLogical=Just latest,conversationAnchor=if same then anchored else FollowEnd,
          conversationReplySelection=if same then selected else Nothing}

-- Canonical block/scalar validation may parse the named immutable item, so it
-- belongs to the checkpoint worker, never the small UI invalidation key.
validateConversationPoints :: Desktop -> Either Text ()
validateConversationPoints desktop=mapM_ validate (M.elems (conversationViews desktop))
  where
    validate view=case conversationLogical view of
      Nothing->Right ()
      Just logical->do
        case conversationAnchor view of
          WithinItem ident fraction->unless
            (fraction>=0 && fraction<=1000 && logicalBodyItemIndex ident logical/=Nothing)
            (Left "Invalid pending conversation anchor")
          _->Right ()
        mapM_ (validateLogicalPoint logical) (anchored++selected)
        where
          anchored=case conversationAnchor view of At point@BodyPoint{}->[point]; _->[]
          selected=case conversationReplySelection view of
            Just (BodySelection a@BodyPoint{} z@BodyPoint{})->[a,z]
            _->[]

-- | Bound and validate checkpoint input, then overlay it on a supplied baseline.
-- Reject symlink input and invalid references/ranges; clear transient interactions.
readCheckpoint :: FilePath -> Desktop -> IO (Either Text Desktop)
readCheckpoint path baseline=do
  loaded<-safeIO $ do
    symbolic<-pathIsSymbolicLink path
    when symbolic (ioError (userError "Recovery checkpoint must not be a symlink"))
    withBinaryFile path ReadMode (\handle->BS.hGet handle (checkpointLimit+1))
  case loaded >>= decode of
    Left err->pure (Left err)
    Right value->case parseEither (desktopParser baseline) value of
      Left _->pure (Left "Invalid or unsupported recovery checkpoint.")
      Right (recovered,frames,snapshots,seeds)->safeIO $ W.withWindowScope $ \scope->do
        generic<-mapM (restorePlugin scope) snapshots
        restored<-mapM (restoreConversation scope recovered frames) seeds
        let bodies=M.fromList [(target,body) | (target,_,_,_,body)<-restored]
            plugins=M.fromList [(ident,(reference,prepared)) | (ident,reference,prepared)<-generic]
            materialize (WindowSeed _ content _ window)=window $ case content of
              StoredSource ident->SourceContent ident
              StoredPlugin ident->PluginContent (fst (plugins M.! ident))
              StoredConversation target->case bodies M.! target of
                InstalledBody reference _->PluginContent reference
                InertBody _->error "Validated recovery frame has no installed body"
            installed=M.fromList ([(reference,prepared) | (_,reference,prepared)<-generic]++
              [(reference,prepared) | (_,_,_,prepared,InstalledBody reference _)<-restored])
            desktop=recovered {windows=map materialize frames,pluginWindows=installed,
              retiredPluginWindows=M.keysSet installed,
              conversationViews=M.fromList [(target,view) | (target,view,_,_,_)<-restored],
              editorDrafts=M.fromList [(ref,draft) | (_,_,(ref,draft),_,_)<-restored]}
        pure (layoutBottomWindows (normalizeBottom desktop))
  where
    decode bytes=do
      unless (BS.length bytes<=checkpointLimit) (Left "Recovery checkpoint exceeds 256 MiB.")
      either (const (Left "Invalid recovery checkpoint JSON.")) Right (eitherDecodeStrict' bytes)
    install scope prepared=do
      update<-W.openWindow scope prepared >>= maybe (ioError (userError "Recovery scope ended")) pure
      accepted<-W.admitWindowUpdate False update >>= maybe (ioError (userError "Recovery publication expired")) pure
      W.retireWindowRef (fst accepted)
      pure accepted
    restorePlugin scope (ident,kind,version,title,text)=do
      prepared<-W.prepareRecoverableTextWindow kind version W.PrivateWindow title text >>= either (ioError . userError . T.unpack) pure
      (reference,view)<-install scope prepared
      pure (ident,reference,view)
    restoreConversation scope desktop frames (target,ConversationSeed name records buffer selected focused anchored column reply)=do
      logical<-restoreLogicalBody target records
      let points=case anchored of At point->[point]; _->[]
          endpoints=case reply of Just (BodySelection a z)->[a,z]; Nothing->[]
      mapM_ (either (ioError . userError . T.unpack) pure . validateLogicalPoint logical) (points++endpoints)
      let rectangles=[rectangle | WindowSeed _ (StoredConversation owner) rectangle _<-frames,owner==target]
      prepared<-case rectangles of
        rectangle:_->restoreLogicalViewport (videoMode desktop/=Nothing) (wideSectionTitles desktop)
          (max 1 (width rectangle-2)) (max 1 (height rectangle-2)) anchored logical
            >>= either (ioError . userError . T.unpack) pure
        []->W.prepareSemanticTextWindow name []
          (W.TextSemantics (W.CopyMessages (if T.null target then W.UserBotAttribution else W.NoAttribution))
            Nothing V.empty V.empty W.ReadableWindow V.empty V.empty V.empty)
            >>= either (ioError . userError . T.unpack) pure
      body<-if not (null rectangles)
        then (\(reference,_)->InstalledBody reference Nothing) <$> install scope prepared
        else pure (InertBody prepared)
      ref<-E.newDraftRef
      pure (target,ConversationView body name ref Nothing Nothing anchored 0 column reply (Just logical) Nothing Nothing,
        (ref,EditorDraft buffer selected focused Nothing),prepared,body)

-- Validation is pure and never calls a plugin. All rendering preparation belongs
-- to readCheckpoint's calling recovery worker before layout adoption.
pluginSnapshots :: Object -> Parser [(Int,Text,Int,Text,Text)]
pluginSnapshots object'=do
  values<-object' .:? "pluginWindows" .!= []
  unless (length values<=256) (fail "Too many recovered plugin windows")
  snapshots<-mapM (withObject "plugin window" $ \entry->do
    ident<-entry .: "id" >>= positive
    kind<-entry .: "kind"
    version<-entry .: "version" >>= positive
    title<-entry .: "title"
    text<-entry .: "text"
    unless (T.length kind<=128 && validCommandName kind && T.length title<=8192 && T.all (\c->c>=' ' && c/='\DEL') title) (fail "Invalid plugin window metadata")
    pure (ident,kind,version,title,text)) values
  unless (S.size (S.fromList [ident | (ident,_,_,_,_)<-snapshots])==length snapshots) (fail "Duplicate plugin windows")
  pure snapshots

safeIO :: IO a -> IO (Either Text a)
safeIO action=do
  result<-try action
  pure $ case result of Left (_::IOException)->Left "Could not read or write the private recovery checkpoint."; Right value->Right value
ignore :: IO () -> IO ()
ignore action=catchIOError action (const (pure ()))

-- One checkpoint owns this table. Cached hashes select buckets, while exact
-- equality disambiguates collisions. IDs follow first encounter, not map order.
data StringTable = StringTable !Int !(M.Map Word64 [(Text,Int)]) [Text]

bufferValue :: BufferStorage Int -> Value
bufferValue s=object
  ["current" .= map line (storageCurrent s),"saved" .= map line (storageSaved s),
   "byteMode" .= storageByteMode s,"savedByteMode" .= storageSavedByteMode s,
   "revision" .= storageRevision s,"lastChange" .= storageLastChange s,
   "undo" .= map history (storageUndo s),"redo" .= map history (storageRedo s)]
  where
    line (StoredLine kind refs)=toJSON (fromEnumKind kind,refs)
    fromEnumKind OriginalLine=0::Int
    fromEnumKind AddedLine=1
    fromEnumKind DeletedLine=2
    history (StoredHistory lines' mode change)=object ["lines" .= map line lines',"byteMode" .= mode,"change" .= change]

bufferParser :: V.Vector Text -> Value -> Parser Buffer
bufferParser strings=withObject "buffer" $ \o->do
  stored<-BufferStorage <$> (o .: "current" >>= mapM line) <*> (o .: "saved" >>= mapM line)
    <*> (o .: "undo" >>= mapM history) <*> (o .: "redo" >>= mapM history)
    <*> o .: "revision" <*> o .: "lastChange" <*> o .: "byteMode" <*> o .: "savedByteMode"
  either (fail . T.unpack) pure (restoreBufferStorage stored)
  where
    line value=do
      (kind,refs)<-parseJSON value
      origin<-case kind::Int of 0->pure OriginalLine; 1->pure AddedLine; 2->pure DeletedLine; _->fail "Invalid line provenance"
      pieces<-mapM (\ident->maybe (fail "Invalid buffer string reference") pure (strings V.!? ident)) refs
      pure (StoredLine origin pieces)
    history=withObject "history" $ \o->StoredHistory <$> (o .: "lines" >>= mapM line) <*> o .: "byteMode" <*> o .: "change"

fileValueWith :: Monad m => (BS.ByteString -> m Value) -> FileState -> m Value
fileValueWith baseline file=do
  bytes<-traverse baseline (diskBytes file)
  pure (object ["path" .= filePath file,"diskBytes" .= bytes])
fileParser :: Value -> Parser FileState
fileParser=withObject "file baseline" $ \o->do
  path<-o .: "path" >>= checkedPath True
  encoded<-o .: "diskBytes"
  bytes<-traverse (either (const (fail "Invalid baseline bytes")) pure . B64.decode . TE.encodeUtf8) encoded
  pure (FileState path bytes)

-- Pending approval buffers are transient. Terminal output remains a read-only
-- ended view and has no terminal identifier that can route input to a process.
keptDocument :: Document -> Bool
keptDocument doc=documentLabel doc `notElem` [Just "Agent request",Just "Proposed agent edit",Just "Conversation"]
documentValueWith :: Monad m => (Buffer -> m Value) -> (BS.ByteString -> m Value) -> (Int,Document) -> m Value
documentValueWith buffer baseline (ident,doc)=do
  encoded<-buffer (documentBuffer doc)
  file<-traverse (fileValueWith baseline) (documentFile doc)
  pure (object ["id" .= ident,"buffer" .= encoded,"file" .= file,
    "label" .= recoveredLabel (documentLabel doc),"suggestedName" .= documentSuggestedName doc,"origin" .= documentOrigin doc])
  where
    recoveredLabel (Just label) | "Terminal " `T.isPrefixOf` label=Just ("Ended "<>label)
    recoveredLabel label=label

desktopValue :: Desktop -> Value
desktopValue desktop=runST $ do
  table<-newSTRef (StringTable 0 M.empty [])
  let intern (fingerprint,text)=do
        StringTable next buckets texts<-readSTRef table
        case lookup text (M.findWithDefault [] fingerprint buckets) of
          Just ident->pure ident
          Nothing->do
            writeSTRef table (StringTable (next+1) (M.insertWith (++) fingerprint [(text,next)] buckets) (text:texts))
            pure next
  encoded<-desktopValueWith
    (fmap bufferValue . traverse intern . snapshotBufferStorage)
    (pure . String . TE.decodeUtf8 . B64.encode)
    (\prepared->let text=W.preparedWindowText prepared in pure (String (contentSlice text 0 (contentLength text))))
    (pure . viewBodyValue) desktop
  StringTable _ _ texts<-readSTRef table
  pure (case encoded of Object fields->Object (KM.insert "strings" (toJSON (reverse texts)) fields); _->encoded)

-- | Durable scalar metadata and stable source/draft/baseline/prepared identities.
data CheckpointKey = CheckpointKey Value [StableName Buffer] [StableName BS.ByteString] [StableName W.PreparedWindow] [Unique] deriving Eq
-- | Capture persistence identity without walking buffer contents or Undo history.
-- Payload replacement is detected even when its revision number is unchanged.
checkpointKey :: Desktop -> IO CheckpointKey
checkpointKey desktop=do
  buffersRef<-newIORef []
  baselinesRef<-newIORef []
  pluginsRef<-newIORef []
  logicalRef<-newIORef []
  let buffer value=do
        ident<-evaluate value >>= makeStableName
        modifyIORef' buffersRef (ident:)
        pure Null
      baseline value=do
        ident<-evaluate value >>= makeStableName
        modifyIORef' baselinesRef (ident:)
        pure Null
      plugin value=do
        ident<-evaluate value >>= makeStableName
        modifyIORef' pluginsRef (ident:)
        pure Null
      logical view=case conversationSource view of
        Just source->remember (capturedSourceIdentity source)
        Nothing->case conversationLogical view of
          Just value->remember (logicalBodyIdentity value)
          Nothing->pure (object ["items" .= ([]::[Value])])
      remember value=do
        ident<-evaluate value
        modifyIORef' logicalRef (ident:)
        pure Null
  metadata<-desktopValueWith buffer baseline plugin logical desktop
  CheckpointKey metadata <$> readIORef buffersRef <*> readIORef baselinesRef <*> readIORef pluginsRef <*> readIORef logicalRef

desktopValueWith :: Monad m => (Buffer -> m Value) -> (BS.ByteString -> m Value) -> (W.PreparedWindow -> m Value) -> (ConversationView -> m Value) -> Desktop -> m Value
desktopValueWith buffer baseline plugin body desktop=do
  encodedDocuments<-mapM (documentValueWith buffer baseline) (M.toAscList documents)
  views<-mapM (conversationViewValueWith buffer body d) (M.toList (conversationViews d))
  plugins<-mapM (\(window,prepared,(kind,version))->do
    text<-plugin prepared
    pure (object ["id" .= windowId window,"kind" .= kind,"version" .= version,"title" .= W.preparedWindowTitle prepared,"text" .= text])) durable
  pure (object ["schemaVersion" .= (5::Int),"screen" .= screenSize d,"buffers" .= encodedDocuments,
    "dockedTerminals" .= [object ["windowId" .= ident,"bounds" .= rectValue rectangle,"restoredBounds" .= fmap rectValue saved] | (ident,(rectangle,saved))<-M.toList (dockedTerminals d),any ((==ident).windowId) (windows d)],
    "bottomTerminal" .= bottomTerminal d,
    "pluginWindows" .= plugins,
    "windows" .= map (windowValue d) [w | w<-windows d,maybe (S.member (windowId w) durableIds || conversationTargetFor d w/=Nothing) (`M.member` documents) (bufferId w)],"nextId" .= nextId d,
    "conversationTarget" .= conversationTarget d,"conversationViews" .= views,
    "directory" .= defaultDirectory d,"sidebar" .= fmap sidebarValue (sideTree d),"preferences" .= object
      ["wordStar" .= wordStar d,"wideSectionTitles" .= wideSectionTitles d,"blinkCursor" .= blinkCursor d,"crtFilter" .= crtFilter d,"pixelateUnicode" .= pixelateUnicode d,
       "defaultBufferView" .= fromEnum (defaultBufferView d),"chatSubmit" .= chatSubmitName (chatSubmit d),"macKeySymbols" .= macKeySymbols d,"materialIcons" .= materialIcons d,"streamerMode" .= streamerMode d,"appearance" .= fromEnum (appearance d),"videoMode" .= videoMode d,
       "problemsVisible" .= problemsVisible d,"problemsHeight" .= problemsPreferredHeight d,"messagesNumber" .= messagesNumber d]])
  where d=rememberConversationView desktop
        documents=M.filter keptDocument (buffers d)
        durableIds=S.fromList [windowId w | (w,_,_)<-durable]
        durable=[(w,prepared,recovery) | w<-windows d,conversationTargetFor d w==Nothing,PluginContent reference<-[windowContent w],Just prepared<-[M.lookup reference (pluginWindows d)],Just recovery<-[W.preparedWindowRecovery prepared]]

conversationViewValueWith :: Monad m => (Buffer -> m Value) -> (ConversationView -> m Value)
  -> Desktop -> (Text,ConversationView) -> m Value
conversationViewValueWith buffer body desktop (target,view)=case
  M.lookup (conversationDraftRef view) (editorDrafts desktop) of
  Just state->do
    draft<-buffer (editorDraftBuffer state)
    snapshot<-body view
    pure (object ["target" .= target,"name" .= conversationName view,"body" .= snapshot,
      "draft" .= draft,"selection" .= selectionValue (editorDraftSelection state),"focused" .= editorDraftFocused state,
      "anchor" .= anchorValue (conversationAnchor view),"column" .= conversationScrollColumn view,
      "replySelection" .= selectionBodyValue (conversationReplySelection view)])
  _->pure Null

-- Records contain the producers' redacted immutable sources. Provider/session
-- and pending question state are separate fields and never enter this codec.
viewBodyValue :: ConversationView -> Value
viewBodyValue view=case conversationLogical view of
  Just logical->bodyValue logical
  Nothing->object ["items" .= ([]::[Value])]

bodyValue :: LogicalBody -> Value
bodyValue body=object ["items" .= map (recordValue . logicalItemRecord) (V.toList (logicalBodyItems body))]

recordValue :: Record -> Value
recordValue (Record (BodyItemId ident) version content)=object
  ["id" .= ident,"revision" .= version,"content" .= case content of
    Reply role text->object ["kind" .= ("reply"::Text),"role" .= role,"markdown" .= text]
    Activity label value history->object ["kind" .= ("activity"::Text),"label" .= label,"value" .= value,"history" .= history]
    Pause label->object ["kind" .= ("pause"::Text),"text" .= label]]

pointValue :: BodyPoint -> Value
pointValue (BodyPoint (BodyItemId ident) block scalar)=toJSON (ident,block,scalar)
pointValue QuestionPoint{}=Null
anchorValue :: BodyAnchor -> Value
anchorValue (At point)=pointValue point
anchorValue FollowEnd=Null
anchorValue (WithinItem (BodyItemId ident) fraction)=object ["withinItem" .= ident,"fraction" .= fraction]
selectionBodyValue :: Maybe BodySelection -> Value
selectionBodyValue (Just (BodySelection a@BodyPoint{} z@BodyPoint{}))=toJSON (pointValue a,pointValue z)
selectionBodyValue _=Null

pointParser :: S.Set BodyItemId -> Value -> Parser BodyPoint
pointParser ids value=do
  (ident,block,scalar)<-parseJSON value
  unless (S.member (BodyItemId ident) ids && block>=0 && scalar>=0) (fail "Invalid logical conversation point")
  pure (BodyPoint (BodyItemId ident) block scalar)
anchorParser :: S.Set BodyItemId -> Value -> Parser BodyAnchor
anchorParser _ Null=pure FollowEnd
anchorParser ids value@Object{}=withObject "pending conversation anchor" (\o->do
  ident<-o .: "withinItem"
  fraction<-o .: "fraction" >>= boundedInt 0 1000
  unless (S.member (BodyItemId ident) ids) (fail "Pending anchor names a missing item")
  pure (WithinItem (BodyItemId ident) fraction)) value
anchorParser ids value=At <$> pointParser ids value
selectionBodyParser :: S.Set BodyItemId -> Value -> Parser (Maybe BodySelection)
selectionBodyParser _ Null=pure Nothing
selectionBodyParser ids value=do
  (a,z)<-parseJSON value
  Just <$> (BodySelection <$> pointParser ids a <*> pointParser ids z)

-- Validated temporary seeds are not another production content owner.
data ConversationSeed = ConversationSeed Text [Record] Buffer Selection Bool BodyAnchor Int (Maybe BodySelection)
data StoredContent = StoredSource Int | StoredPlugin Int | StoredConversation Text
data WindowSeed = WindowSeed Int StoredContent Rect (WindowContent -> Window)

bodyParser :: Text -> Value -> Parser [Record]
bodyParser target=withObject "logical conversation" $ \o->do
  encoded<-o .: "items"
  records<-mapM recordParser encoded
  _<-foldM (\previous record->do
    let BodyItemId ident=recordId record
    unless (ident>previous && (ident>=0 || ident==(-1) && not (T.null target) && case recordContent record of Pause{}->True; _->False))
      (fail "Invalid ordered conversation item identity")
    pure ident) (-2) records
  pure records
  where
    recordParser=withObject "conversation item" $ \entry->do
      ident<-entry .: "id"
      version<-entry .: "revision"
      unless (version>=0) (fail "Invalid conversation item revision")
      content<-entry .: "content" >>= withObject "conversation item content" (\value->do
        kind<-value .: "kind"
        case kind::Text of
          "reply"->Reply <$> value .: "role" <*> value .: "markdown"
          "activity"->Activity <$> value .: "label" <*> value .: "value" <*> value .: "history"
          "pause"->Pause <$> value .: "text"
          _->fail "Invalid conversation item kind")
      pure (Record (BodyItemId ident) version content)

conversationViewParser :: V.Vector Text -> Value -> Parser (Text,ConversationSeed)
conversationViewParser strings=withObject "conversation view" $ \o->do
  target<-o .: "target"
  unless (T.length target<=128 && not (T.any (<' ') target)) (fail "Invalid conversation target")
  name<-o .: "name"
  unless (T.length name<=256 && not (T.any (<' ') name)) (fail "Invalid conversation name")
  records<-o .: "body" >>= bodyParser target
  draft<-o .: "draft" >>= bufferParser strings
  selected<-o .: "selection" >>= selectionParser (bufferLength draft)
  focused<-o .: "focused"
  let ids=S.fromList (map recordId records)
  anchored<-o .: "anchor" >>= anchorParser ids
  column<-o .: "column" >>= boundedInt 0 1073741823
  reply<-o .: "replySelection" >>= selectionBodyParser ids
  pure (target,ConversationSeed name records draft selected focused anchored column reply)

windowValue :: Desktop -> Window -> Value
windowValue desktop original=object ["id" .= windowId w,"sourceId" .= bufferId w,"target" .= conversationTargetFor desktop w,
  "number" .= windowNumber w,"bounds" .= rectValue (bounds w),
  "selection" .= selectionValue (selection w),"scrollRow" .= scrollRow w,"scrollColumn" .= scrollColumn w,
  "restoredBounds" .= fmap rectValue (restoredBounds w),"hexLow" .= windowHexLow w,"hexAscii" .= windowHexAscii w,"bufferView" .= fromEnum (bufferView w),"reviewSplit" .= reviewSplit w,"markdownInteraction" .= fmap (\(MarkdownInteraction selected row column stamp)->(selectionValue selected,row,column,stamp)) (markdownInteraction w)]
  where w=case (rowsInteraction original,conversationTargetFor desktop original) of
          (Just _,_)->original {selection=Selection 0 0,scrollRow=0,scrollColumn=0}
          (_,Just _)->original {selection=Selection 0 0,scrollRow=0}
          _->original

rectValue :: Rect -> Value
rectValue (Rect x y w h)=toJSON (x,y,w,h)
selectionValue :: Selection -> Value
selectionValue (Selection a c)=toJSON (a,c)
sidebarValue :: Sidebar -> Value
sidebarValue tree=object ["root" .= treeRoot tree,"selected" .= selected,"scroll" .= scrolled,"width" .= treeWidth tree,
  "focused" .= treeFocused tree,"rows" .= rows]
  where
    prepared=[(P.infoLabel info,path,rowDepth row,P.infoBranch info,rowExpanded row) | row<-M.elems (treeRows tree),NodeRow{}<-[rowKey row],let info=rowInfo row,Just path<-[P.infoResource info]]
    current=M.fromList [(path,()) | (_,path,_,_,_)<-prepared]
    (pending,chosen,topPath)=case treeHints tree of
      Just (SidebarHints hints selected top)->(hints,selected,top)
      Nothing->(M.empty,Nothing,Nothing)
    savedExpansion path expanded=case M.lookup path pending of
      Just True->True
      Just False | path==treeRoot tree->False
      _->expanded
    values=[(name,path,depth,directory,savedExpansion path expanded) | (name,path,depth,directory,expanded)<-prepared]++[(T.pack (takeFileName path),path,0,expanded,expanded) | (path,expanded)<-M.toAscList (M.difference pending current)]
    locate wanted=maybe 0 (\path->fromMaybe 0 (findIndex (\(_,resource,_,_,_)->resource==path) values)) wanted
    resourceAt index=P.infoResource . rowInfo =<< rowAt index tree
    selected=locate (case chosen of Just path->Just path; Nothing->resourceAt (treeSelected tree))
    scrolled=locate (case topPath of Just path->Just path; Nothing->resourceAt (treeScroll tree))
    rows=[object ["name" .= name,"path" .= path,"depth" .= depth,"directory" .= directory,"expanded" .= expanded] | (name,path,depth,directory,expanded)<-values]

desktopParser :: Desktop -> Value -> Parser (Desktop,[WindowSeed],[(Int,Text,Int,Text,Text)],[(Text,ConversationSeed)])
desktopParser baseline=withObject "checkpoint" $ \o->do
  version<-o .: "schemaVersion"
  unless (version==(5::Int)) (fail "Unsupported checkpoint version")
  strings<-o .: "strings"
  size@(cols,rows)<-o .: "screen"
  unless (cols>0 && rows>0 && cols<=4096 && rows<=4096 && toInteger cols*toInteger rows<=1048576) (fail "Invalid desktop dimensions")
  encodedDocuments<-o .: "buffers"
  unless (length encodedDocuments<=2048) (fail "Too many recovered buffers")
  parsedDocuments<-mapM (documentParser strings) encodedDocuments
  let documents=M.fromList parsedDocuments
  unless (M.size documents==length parsedDocuments) (fail "Duplicate buffer IDs")
  selectedTarget<-o .: "conversationTarget"
  encodedViews<-o .: "conversationViews"
  unless (length encodedViews<=1024) (fail "Too many conversation views")
  parsedViews<-mapM (conversationViewParser strings) encodedViews
  let conversationViews'=M.fromList parsedViews
  unless (length parsedViews==M.size conversationViews') (fail "Duplicate conversation views")
  unless (T.null selectedTarget || M.member selectedTarget conversationViews') (fail "Unknown selected conversation")
  encodedWindows<-o .: "windows"
  unless (length encodedWindows<=4096) (fail "Too many recovered windows")
  snapshots<-pluginSnapshots o
  let pluginText=M.fromList [(ident,text) | (ident,_,_,_,text)<-snapshots]
  views<-mapM (windowParser documents pluginText conversationViews') encodedWindows
  let frameIds=[ident | WindowSeed ident _ _ _<-views]
      frameTargets=[target | WindowSeed _ (StoredConversation target) _ _<-views]
      pluginIds=[ident | WindowSeed _ (StoredPlugin ident) _ _<-views]
  unless (S.size (S.fromList frameIds)==length views) (fail "Duplicate window IDs")
  unless (S.size (S.fromList frameTargets)==length frameTargets) (fail "Duplicate conversation frames")
  unless (S.fromList pluginIds==M.keysSet pluginText) (fail "Unowned plugin snapshot")
  encodedDock<-o .:? "dockedTerminals" .!= []
  unless (length encodedDock<=length views) (fail "Too many docked terminals")
  pinned<-mapM (withObject "docked terminal" $ \entry->do
    wid<-entry .: "windowId"
    unless (any (\(WindowSeed ident content _ make)->ident==wid && case content of StoredSource bid->terminalWindow baseline {buffers=documents} (make (SourceContent bid)); _->False) views) (fail "Dock references missing terminal")
    rectangle<-entry .: "bounds" >>= rectParser
    restored<-entry .: "restoredBounds" >>= traverse rectParser
    pure (wid,(rectangle,restored))) encodedDock
  unless (M.size (M.fromList pinned)==length pinned) (fail "Duplicate docked terminal")
  selectedTerminal<-o .:? "bottomTerminal"
  unless (maybe True (`M.member` M.fromList pinned) selectedTerminal) (fail "Unknown selected terminal")
  ident<-o .: "nextId" >>= positive
  unless (all (<ident) (M.keys documents++frameIds)) (fail "Invalid next ID")
  directory<-o .: "directory" >>= traverse (checkedPath True)
  sidebar<-o .: "sidebar" >>= traverse sidebarParser
  prefs<-o .: "preferences"
  wideTitles<-prefs .:? "wideSectionTitles" .!= False
  wordStar'<-prefs .: "wordStar"; blink<-prefs .: "blinkCursor"; crt<-prefs .: "crtFilter"
  pixelate<-prefs .: "pixelateUnicode"; icons<-prefs .: "materialIcons"; streamer<-prefs .: "streamerMode"
  defaultView<-prefs .:? "defaultBufferView" .!= 0 >>= boundedInt 0 (fromEnum (maxBound :: BufferView))
  macSymbols<-prefs .:? "macKeySymbols" .!= macKeySymbols baseline
  submit<-prefs .:? "chatSubmit" .!= chatSubmitName (chatSubmit baseline) >>= maybe (fail "Invalid chat submit action") pure . parseChatSubmit
  look<-prefs .: "appearance"
  unless (look>=0 && look<=2) (fail "Invalid appearance")
  mode<-prefs .: "videoMode" >>= traverse (boundedInt 0 65535)
  problems<-prefs .: "problemsVisible"; preferred<-prefs .: "problemsHeight" >>= boundedInt 0 4096
  messages<-prefs .: "messagesNumber" >>= traverse positive
  pure (baseline {dockedTerminals=M.fromList pinned,bottomTerminal=selectedTerminal,screenSize=size,buffers=documents,windows=[],pluginWindows=M.empty,retiredPluginWindows=S.empty,nextId=ident,editorDrafts=M.empty,editingInput=MountedInput,
    conversationTarget=selectedTarget,conversationViews=M.empty,defaultDirectory=directory,sideTree=sidebar,wideSectionTitles=wideTitles,windowPresentations=M.empty,wordStar=wordStar',blinkCursor=blink,crtFilter=crt,pixelateUnicode=pixelate,materialIcons=icons,streamerMode=streamer,
    defaultBufferView=toEnum defaultView,chatSubmit=submit,macKeySymbols=macSymbols,appearance=toEnum look,videoMode=mode,problemsVisible=problems,problemsPreferredHeight=preferred,messagesNumber=messages,
    menu=Nothing,dialog=Nothing,drag=Nothing,dragOriginal=Nothing,clipboard="",clipboardCode=Nothing,clipboardExport=(0,Nothing),prefix=Nothing,blockStart=Nothing,
    status="Recovered session. Background processes ended; reconnect agents as needed.",lastFind="",branchStatus="",branchAdded=0,branchDeleted=0,branchRoot=Nothing,
    gitReview=Nothing,hoverTarget=Nothing,typeHint="",buttonHover=Nothing,buttonPressed=Nothing,contextMenu=Nothing,contextKind=SourceContext,contributedMenus=contributedMenus baseline,agentMenuRefs=agentMenuRefs baseline,menusActive=menusActive baseline,contextTarget=Nothing,
    diagnostics=[],diagnosticsGeneration=diagnosticsGeneration baseline+1,buildDiagnostics=[],problemsSelected=0,problemsScroll=0,problemsFocused=False,statusHover=Nothing,heldModifiers=[],
    autocompleteWindow=Nothing,autocompleteACPEnabled=False,autocompleteDraft=newBuffer "",autocompleteSelection=Selection 0 0,autocompleteFocused=True,
    childAgentSettings=[],childAgentSteering=False,childAgentContextUsage=Nothing,agentSteering=False,agentReplying=False,agentQueued=0,agentContextUsage=Nothing,chatQuestion=Nothing},views,snapshots,parsedViews)

documentParser :: V.Vector Text -> Value -> Parser (Int,Document)
documentParser strings=withObject "document" $ \o->do
  ident<-o .: "id" >>= positive
  buffer<-o .: "buffer" >>= bufferParser strings
  file<-o .: "file" >>= traverse fileParser
  label<-o .: "label" >>= traverse (boundedText 32768)
  suggested<-o .: "suggestedName" >>= traverse (checkedPath False)
  origin<-o .:? "origin" >>= traverse (checkedPath True)
  unless (label `notElem` [Just "Agent request",Just "Proposed agent edit",Just "Conversation"] && maybe True (not . T.isPrefixOf "Terminal ") label) (fail "Transient document")
  pure (ident,restyle (newDocument buffer file) {documentLabel=label,documentSuggestedName=suggested,documentOrigin=origin})
windowParser :: M.Map Int Document -> M.Map Int Text -> M.Map Text ConversationSeed -> Value -> Parser WindowSeed
windowParser documents plugins conversations=withObject "window" $ \o->do
  ident<-o .: "id" >>= positive
  bid<-o .: "sourceId"
  target<-o .: "target"
  (content,length',sourceView)<-case (bid,target) of
    (Just source,Nothing)->do
      unless (not (M.member ident plugins)) (fail "Source frame owns plugin state")
      doc<-maybe (fail "Window references missing buffer") pure (M.lookup source documents)
      pure (StoredSource source,bufferLength (documentBuffer doc),not (byteMode (documentBuffer doc)) && documentLabel doc==Nothing)
    (Nothing,Just owner)->do
      unless (not (M.member ident plugins)) (fail "Conversation frame owns generic plugin state")
      unless (M.member owner conversations) (fail "Window references missing conversation")
      pure (StoredConversation owner,0,False)
    (Nothing,Nothing)->case M.lookup ident plugins of
      Just text->pure (StoredPlugin ident,T.length text,False)
      Nothing->fail "Window references missing plugin state"
    _->fail "Window has two content owners"
  number<-o .: "number" >>= positive
  rectangle<-o .: "bounds" >>= rectParser
  selected<-o .: "selection" >>= selectionParser length'
  row<-o .: "scrollRow" >>= boundedInt 0 1073741823
  column<-o .: "scrollColumn" >>= boundedInt 0 1073741823
  restored<-o .: "restoredBounds" >>= traverse rectParser
  viewIndex<-o .:? "bufferView" .!= 0 >>= boundedInt 0 (fromEnum (maxBound :: BufferView))
  split<-o .:? "reviewSplit" .!= 50 >>= boundedInt 0 100
  preview<-o .:? "markdownInteraction" >>= traverse (\value->do
    (range,r,c,stamp)<-parseJSON value
    selectedPreview<-selectionParser 1073741823 range
    rowPreview<-boundedInt 0 1073741823 r
    colPreview<-boundedInt 0 1073741823 c
    checkedStamp<-traverse (\(version,columns)->(,) <$> boundedInt 0 1073741823 version <*> boundedInt 1 1048576 columns) stamp
    pure (MarkdownInteraction selectedPreview rowPreview colPreview checkedStamp))
  low<-o .: "hexLow"
  ascii<-o .: "hexAscii"
  let view=if sourceView && (toEnum viewIndex/=MarkdownView || maybe False markdownDocument (M.lookup (fromMaybe 0 bid) documents)) then toEnum viewIndex else CurrentView
  pure (WindowSeed ident content rectangle (\owner->Window ident owner rectangle selected row column restored low ascii number view Nothing split preview Nothing Nothing Nothing Canvas.fitCanvasView))

rectParser :: Value -> Parser Rect
rectParser value=do
  (x,y,w,h)<-parseJSON value
  _<-boundedInt (-1048576) 1048576 x; _<-boundedInt (-1048576) 1048576 y
  _<-boundedInt 1 1048576 w; _<-boundedInt 1 1048576 h
  pure (Rect x y w h)
selectionParser :: Int -> Value -> Parser Selection
selectionParser size value=do
  (a,c)<-parseJSON value
  _<-boundedInt 0 size a; _<-boundedInt 0 size c
  pure (Selection a c)
sidebarParser :: Value -> Parser Sidebar
sidebarParser=withObject "sidebar" $ \o->do
  root<-o .: "root" >>= checkedPath True
  rows<-o .: "rows"
  unless (length rows<=20000) (fail "Too many sidebar entries")
  entries<-mapM rowParser rows
  chosen<-o .: "selected" >>= boundedInt (-1) (max 0 (length entries-1))
  scroll<-o .: "scroll" >>= boundedInt 0 (length entries)
  width'<-o .: "width" >>= boundedInt 1 4096
  focused<-o .: "focused"
  let pathAt index=case drop index entries of (path,_):_->Just path; _->Nothing
  pure (emptySidebar root width' focused) {treeHints=Just (SidebarHints (M.fromListWith (\_ first->first) entries) (pathAt chosen) (pathAt scroll))}
  where rowParser=withObject "tree row" $ \o->do
          _<-o .: "name" >>= boundedText 32768
          path<-o .: "path" >>= checkedPath True
          _<-o .: "depth" >>= boundedInt 0 1024
          directory<-o .: "directory"
          expanded<-o .: "expanded"
          pure (path,directory && expanded)
positive :: Int -> Parser Int
positive=boundedInt 1 1073741823
boundedInt :: Int -> Int -> Int -> Parser Int
boundedInt low high value=if value>=low && value<=high then pure value else fail "Checkpoint integer out of range"
boundedText :: Int -> Text -> Parser Text
boundedText limit value=if T.length value<=limit then pure value else fail "Checkpoint text too long"
checkedPath :: Bool -> FilePath -> Parser FilePath
checkedPath absolute path=if not (null path) && length path<=32768 && '\0' `notElem` path && (not absolute || isAbsolute path) then pure path else fail "Invalid checkpoint path"
