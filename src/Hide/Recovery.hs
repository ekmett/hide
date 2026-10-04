{-# LANGUAGE CPP, OverloadedStrings, PackageImports #-}
-- | Versioned editor checkpoints and cheap persistence invalidation keys.
--
-- Checkpoints preserve buffers, history, views and preferences, not running
-- background processes. Transient approvals are excluded and private question
-- text is redacted where required; terminals recover as ended documents.
-- Metadata and stable immutable-payload identities decide whether to checkpoint.
-- Publishing uses flush and rename, without an explicit fsync durability promise.
module Hide.Recovery (writeCheckpoint, readCheckpoint, CheckpointKey, checkpointKey) where

import Control.Exception (IOException, bracket, try, evaluate)
import Control.Monad (unless, when)
import Data.Aeson
import Data.Functor.Identity (runIdentity)
import Data.IORef
import System.Mem.StableName
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (pathIsSymbolicLink, removeFile, renameFile)
import System.FilePath (isAbsolute, takeDirectory)
import System.IO (IOMode(ReadMode), hClose, hFlush, openBinaryTempFile, withBinaryFile)
import System.IO.Error (catchIOError)
#ifndef mingw32_HOST_OS
import System.Posix.Files (setFileMode)
#endif
import Hide.Buffer
import Hide.BufferView
import Hide.Files (FileState(..))
import Hide.Model

-- Histories are never truncated to fit. A rejected checkpoint leaves the last
-- complete checkpoint in place, and the caller must surface the returned error.
checkpointLimit :: Int
checkpointLimit=256*1024*1024

-- | Validate serialized state before publishing by rename. Oversized output
-- is refused without truncating histories or replacing the previous checkpoint.
writeCheckpoint :: FilePath -> Desktop -> IO (Either Text ())
writeCheckpoint path desktop=do
  let encoded=BL.take (fromIntegral checkpointLimit+1) (encode (desktopValue desktop))
  if BL.length encoded>fromIntegral checkpointLimit then pure (Left "Recovery checkpoint exceeds 256 MiB; no data was truncated.")
    else case eitherDecode encoded >>= parseEither (desktopParser desktop) of
      Left _->pure (Left "Editor state cannot be represented by a valid recovery checkpoint.")
      Right _->safeIO $ bracket (openBinaryTempFile (takeDirectory path) ".thc-recovery-") cleanup $ \(temporary,handle)->do
#ifndef mingw32_HOST_OS
        setFileMode temporary 0o600
#endif
        BL.hPut handle encoded
        hFlush handle
        hClose handle
        renameFile temporary path
  where cleanup (temporary,handle)=ignore (hClose handle) >> ignore (removeFile temporary)

-- | Bound and validate checkpoint input, then overlay it on a supplied baseline.
-- Reject symlink input and invalid references/ranges; clear transient interactions.
readCheckpoint :: FilePath -> Desktop -> IO (Either Text Desktop)
readCheckpoint path baseline=do
  loaded<-safeIO $ do
    symbolic<-pathIsSymbolicLink path
    when symbolic (ioError (userError "Recovery checkpoint must not be a symlink"))
    withBinaryFile path ReadMode (\handle->BS.hGet handle (checkpointLimit+1))
  pure $ do
    bytes<-loaded
    unless (BS.length bytes<=checkpointLimit) (Left "Recovery checkpoint exceeds 256 MiB.")
    value<-either (const (Left "Invalid recovery checkpoint JSON.")) Right (eitherDecodeStrict' bytes)
    either (const (Left "Invalid or unsupported recovery checkpoint.")) Right (parseEither (desktopParser baseline) value)

safeIO :: IO a -> IO (Either Text a)
safeIO action=do
  result<-try action
  pure $ case result of Left (_::IOException)->Left "Could not read or write the private recovery checkpoint."; Right value->Right value
ignore :: IO () -> IO ()
ignore action=catchIOError action (const (pure ()))

bufferValue :: Buffer -> Value
bufferValue buffer=let s=snapshotBuffer buffer in object
  ["contents" .= snapshotContents s,"saved" .= snapshotSaved s,"byteMode" .= snapshotByteMode s,"savedByteMode" .= snapshotSavedByteMode s,
   "revision" .= snapshotRevision s,"lastChange" .= snapshotLastChange s,"undo" .= map history (snapshotUndo s),"redo" .= map history (snapshotRedo s),"lineChanges" .= snapshotLineChanges s]
  where history (text,mode,change)=object ["contents" .= text,"byteMode" .= mode,"change" .= change]

bufferParser :: Value -> Parser Buffer
bufferParser=withObject "buffer" $ \o->do
  snapshot<-BufferSnapshot <$> o .: "contents" <*> o .: "saved" <*> (o .: "undo" >>= mapM history) <*> (o .: "redo" >>= mapM history)
    <*> o .: "revision" <*> o .: "lastChange" <*> o .: "byteMode" <*> o .: "savedByteMode" <*> o .:? "lineChanges"
  either (fail . T.unpack) pure (restoreBuffer snapshot)
  where history=withObject "history" $ \o->(,,) <$> o .: "contents" <*> o .: "byteMode" <*> o .: "change"

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
keptDocument doc=documentLabel doc `notElem` [Just "Agent request",Just "Proposed agent edit"]
documentValueWith :: Monad m => ([(Int,Int)] -> Buffer -> m Value) -> (BS.ByteString -> m Value) -> Desktop -> (Int,Document) -> m Value
documentValueWith buffer baseline desktop (ident,doc)=do
  encoded<-buffer privateSpans (documentBuffer doc)
  file<-traverse (fileValueWith baseline) (documentFile doc)
  pure (object ["id" .= ident,"buffer" .= encoded,"file" .= file,
    "label" .= recoveredLabel (documentLabel doc),"suggestedName" .= documentSuggestedName doc])
  where
    -- The key retains mask boundaries without constructing the redacted text.
    privateSpans
      | documentLabel doc==Just "Conversation",fmap fst (conversationDocument "" desktop)==Just ident,T.null (conversationTarget desktop)=
          [(a,z) | (a,z,action,_)<-chatActions desktop,"question-" `T.isPrefixOf` action]
      | otherwise=[]
    recoveredLabel (Just label) | "Terminal " `T.isPrefixOf` label=Just ("Ended "<>label)
    recoveredLabel label=label

redactPending :: [(Int,Int)] -> Buffer -> Buffer
redactPending [] buffer=buffer
redactPending spans buffer=newBuffer (T.pack
  [if c/='\n' && c/='\r' && any (\(a,z)->index>=a && index<z) spans then ' ' else c
  | (index,c)<-zip [0..] (T.unpack (contents buffer))])

desktopValue :: Desktop -> Value
desktopValue desktop=runIdentity (desktopValueWith
  (\spans -> pure . bufferValue . redactPending spans)
  (pure . String . TE.decodeUtf8 . B64.encode) desktop)

-- | Small recovery metadata, redaction boundaries and stable buffer/baseline identities.
data CheckpointKey = CheckpointKey Value [StableName Buffer] [StableName BS.ByteString] deriving Eq
-- | Capture persistence identity without walking buffer contents or Undo history.
-- Payload replacement is detected even when its revision number is unchanged.
checkpointKey :: Desktop -> IO CheckpointKey
checkpointKey desktop=do
  buffersRef<-newIORef []
  baselinesRef<-newIORef []
  let buffer spans value=do
        ident<-evaluate value >>= makeStableName
        modifyIORef' buffersRef (ident:)
        pure (toJSON spans)
      baseline value=do
        ident<-evaluate value >>= makeStableName
        modifyIORef' baselinesRef (ident:)
        pure Null
  metadata<-desktopValueWith buffer baseline desktop
  CheckpointKey metadata <$> readIORef buffersRef <*> readIORef baselinesRef

desktopValueWith :: Monad m => ([(Int,Int)] -> Buffer -> m Value) -> (BS.ByteString -> m Value) -> Desktop -> m Value
desktopValueWith buffer baseline desktop=do
  encodedDocuments<-mapM (documentValueWith buffer baseline d) (M.toAscList documents)
  views<-mapM (conversationViewValueWith buffer) (M.toList (conversationViews d))
  composer<-buffer [] (composerBuffer d)
  pure (object ["schemaVersion" .= (1::Int),"screen" .= screenSize d,"buffers" .= encodedDocuments,
    "dockedTerminals" .= [object ["windowId" .= ident,"bounds" .= rectValue rectangle,"restoredBounds" .= fmap rectValue saved] | (ident,(rectangle,saved))<-M.toList (dockedTerminals d),any ((==ident).windowId) (windows d)],
    "bottomTerminal" .= bottomTerminal d,
    "windows" .= map windowValue [w | w<-windows d,M.member (bufferId w) documents],"nextId" .= nextId d,
    "conversationTarget" .= conversationTarget d,"conversationViews" .= views,
    "composer" .= composer,"composerSelection" .= selectionValue (composerSelection d),"composerFocused" .= composerFocused d,
    "directory" .= defaultDirectory d,"sidebar" .= fmap sidebarValue (sideTree d),"preferences" .= object
      ["wordStar" .= wordStar d,"blinkCursor" .= blinkCursor d,"crtFilter" .= crtFilter d,"pixelateUnicode" .= pixelateUnicode d,
       "defaultBufferView" .= fromEnum (defaultBufferView d),"chatSubmit" .= chatSubmitName (chatSubmit d),"macKeySymbols" .= macKeySymbols d,"materialIcons" .= materialIcons d,"streamerMode" .= streamerMode d,"appearance" .= fromEnum (appearance d),"videoMode" .= videoMode d,
       "problemsVisible" .= problemsVisible d,"problemsHeight" .= problemsPreferredHeight d,"messagesNumber" .= messagesNumber d]])
  where d=rememberConversationView desktop
        documents=M.filter keptDocument (buffers d)

conversationViewValueWith :: Monad m => ([(Int,Int)] -> Buffer -> m Value) -> (Text,ConversationView) -> m Value
conversationViewValueWith buffer (target,view)=do
  draft<-buffer [] (conversationDraft view)
  pure (object ["target" .= target,"bufferId" .= conversationBufferId view,"name" .= conversationName view,
    "draft" .= draft,"selection" .= selectionValue (conversationDraftSelection view),
    "scroll" .= conversationScroll view,"replySelection" .= selectionValue (conversationReplySelection view)])

conversationViewParser :: M.Map Int Document -> Value -> Parser (Text,ConversationView)
conversationViewParser documents=withObject "conversation view" $ \o->do
  target<-o .: "target"
  unless (T.length target<=128 && not (T.any (<' ') target)) (fail "Invalid conversation target")
  bid<-o .: "bufferId"
  doc<-maybe (fail "Missing conversation document") pure (M.lookup bid documents)
  unless (documentLabel doc==Just "Conversation") (fail "Not a conversation document")
  name<-o .: "name"
  unless (T.length name<=256 && not (T.any (<' ') name)) (fail "Invalid conversation name")
  draft<-o .: "draft" >>= bufferParser
  selected<-o .: "selection" >>= selectionParser (bufferLength draft)
  (row,col)<-o .: "scroll"
  unless (row>=0 && col>=0 && row<=1000000000 && col<=1000000000) (fail "Invalid conversation scroll")
  replySelection<-o .: "replySelection" >>= selectionParser (bufferLength (documentBuffer doc))
  pure (target,ConversationView bid name draft selected (row,col) replySelection)

windowValue :: Window -> Value
windowValue w=object ["id" .= windowId w,"bufferId" .= bufferId w,"number" .= windowNumber w,"bounds" .= rectValue (bounds w),
  "selection" .= selectionValue (selection w),"scrollRow" .= scrollRow w,"scrollColumn" .= scrollColumn w,
  "restoredBounds" .= fmap rectValue (restoredBounds w),"hexLow" .= windowHexLow w,"hexAscii" .= windowHexAscii w,"bufferView" .= fromEnum (bufferView w),"reviewSplit" .= reviewSplit w]
rectValue :: Rect -> Value
rectValue (Rect x y w h)=toJSON (x,y,w,h)
selectionValue :: Selection -> Value
selectionValue (Selection a c)=toJSON (a,c)
sidebarValue :: Sidebar -> Value
sidebarValue tree=object ["root" .= treeRoot tree,"selected" .= treeSelected tree,"scroll" .= treeScroll tree,"width" .= treeWidth tree,
  "focused" .= treeFocused tree,"rows" .= [object ["name" .= nodeName row,"path" .= nodePath row,"depth" .= nodeDepth row,
    "directory" .= nodeDirectory row,"expanded" .= nodeExpanded row] | row<-treeRows tree]]

desktopParser :: Desktop -> Value -> Parser Desktop
desktopParser baseline=withObject "checkpoint" $ \o->do
  version<-o .: "schemaVersion"
  unless (version==(1::Int)) (fail "Unsupported checkpoint version")
  size@(cols,rows)<-o .: "screen"
  unless (cols>0 && rows>0 && cols<=4096 && rows<=4096 && toInteger cols*toInteger rows<=1048576) (fail "Invalid desktop dimensions")
  encodedDocuments<-o .: "buffers"
  unless (length encodedDocuments<=2048) (fail "Too many recovered buffers")
  parsedDocuments<-mapM documentParser encodedDocuments
  let documents=M.fromList parsedDocuments
  unless (M.size documents==length parsedDocuments) (fail "Duplicate buffer IDs")
  encodedWindows<-o .: "windows"
  unless (length encodedWindows<=4096) (fail "Too many recovered windows")
  views<-mapM (windowParser documents) encodedWindows
  unless (S.size (S.fromList (map windowId views))==length views) (fail "Duplicate window IDs")
  encodedDock<-o .:? "dockedTerminals" .!= []
  unless (length encodedDock<=length views) (fail "Too many docked terminals")
  pinned<-mapM (withObject "docked terminal" $ \entry->do
    wid<-entry .: "windowId"
    unless (any (\w->windowId w==wid && terminalWindow baseline {buffers=documents} w) views) (fail "Dock references missing terminal")
    rectangle<-entry .: "bounds" >>= rectParser
    restored<-entry .: "restoredBounds" >>= traverse rectParser
    pure (wid,(rectangle,restored))) encodedDock
  unless (M.size (M.fromList pinned)==length pinned) (fail "Duplicate docked terminal")
  selectedTerminal<-o .:? "bottomTerminal"
  unless (maybe True (`M.member` M.fromList pinned) selectedTerminal) (fail "Unknown selected terminal")
  ident<-o .: "nextId" >>= positive
  unless (all (<ident) (M.keys documents++map windowId views)) (fail "Invalid next ID")
  composer<-o .: "composer" >>= bufferParser
  composerSelection'<-o .: "composerSelection" >>= selectionParser (bufferLength composer)
  composerFocused'<-o .: "composerFocused"
  selectedTarget<-o .:? "conversationTarget" .!= ""
  encodedViews<-o .:? "conversationViews" .!= []
  unless (length encodedViews<=1024) (fail "Too many conversation views")
  parsedViews<-mapM (conversationViewParser documents) encodedViews
  let conversationViews'=M.fromList parsedViews
  unless (length parsedViews==M.size conversationViews' && S.size (S.fromList (map (conversationBufferId.snd) parsedViews))==length parsedViews) (fail "Duplicate conversation views")
  unless (T.null selectedTarget || M.member selectedTarget conversationViews') (fail "Unknown selected conversation")
  directory<-o .: "directory" >>= traverse (checkedPath True)
  sidebar<-o .: "sidebar" >>= traverse sidebarParser
  prefs<-o .: "preferences"
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
  pure (layoutBottomWindows (normalizeBottom baseline {dockedTerminals=M.fromList pinned,bottomTerminal=selectedTerminal,screenSize=size,buffers=documents,windows=views,nextId=ident,composerBuffer=composer,composerSelection=composerSelection',composerFocused=composerFocused',
    conversationTarget=selectedTarget,conversationViews=conversationViews',defaultDirectory=directory,sideTree=sidebar,wordStar=wordStar',blinkCursor=blink,crtFilter=crt,pixelateUnicode=pixelate,materialIcons=icons,streamerMode=streamer,
    defaultBufferView=toEnum defaultView,chatSubmit=submit,macKeySymbols=macSymbols,appearance=toEnum look,videoMode=mode,problemsVisible=problems,problemsPreferredHeight=preferred,messagesNumber=messages,
    menu=Nothing,dialog=Nothing,drag=Nothing,dragOriginal=Nothing,clipboard="",clipboardCode=Nothing,clipboardExport=(0,Nothing),prefix=Nothing,blockStart=Nothing,
    status="Recovered session. Background processes ended; reconnect agents as needed.",lastFind="",branchStatus="",branchAdded=0,branchDeleted=0,branchRoot=Nothing,
    gitReview=Nothing,hoverTarget=Nothing,typeHint="",buttonHover=Nothing,buttonPressed=Nothing,contextMenu=Nothing,contextKind=SourceContext,contextTarget=Nothing,
    diagnostics=[],buildDiagnostics=[],problemsSelected=0,problemsScroll=0,problemsFocused=False,statusHover=Nothing,heldModifiers=[],
    autocompleteACPEnabled=False,autocompleteDraft=newBuffer "",autocompleteSelection=Selection 0 0,autocompleteFocused=True,
    childAgentSettings=[],childAgentSteering=False,childAgentContextUsage=Nothing,agentSteering=False,agentReplying=False,agentQueued=0,agentContextUsage=Nothing,chatQuestion=Nothing,chatActions=[],chatInputOffset=Nothing}))

documentParser :: Value -> Parser (Int,Document)
documentParser=withObject "document" $ \o->do
  ident<-o .: "id" >>= positive
  buffer<-o .: "buffer" >>= bufferParser
  file<-o .: "file" >>= traverse fileParser
  label<-o .: "label" >>= traverse (boundedText 32768)
  suggested<-o .: "suggestedName" >>= traverse (checkedPath False)
  unless (label `notElem` [Just "Agent request",Just "Proposed agent edit"] && maybe True (not . T.isPrefixOf "Terminal ") label) (fail "Transient document")
  pure (ident,restyle (newDocument buffer file) {documentLabel=label,documentSuggestedName=suggested})
windowParser :: M.Map Int Document -> Value -> Parser Window
windowParser documents=withObject "window" $ \o->do
  ident<-o .: "id" >>= positive
  bid<-o .: "bufferId"
  doc<-maybe (fail "Window references missing buffer") pure (M.lookup bid documents)
  number<-o .: "number" >>= positive
  rectangle<-o .: "bounds" >>= rectParser
  selected<-o .: "selection" >>= selectionParser (bufferLength (documentBuffer doc))
  row<-o .: "scrollRow" >>= boundedInt 0 1073741823
  column<-o .: "scrollColumn" >>= boundedInt 0 1073741823
  restored<-o .: "restoredBounds" >>= traverse rectParser
  viewIndex<-o .:? "bufferView" .!= 0 >>= boundedInt 0 (fromEnum (maxBound :: BufferView))
  split<-o .:? "reviewSplit" .!= 50 >>= boundedInt 0 100
  Window ident bid rectangle selected row column restored <$> o .: "hexLow" <*> o .: "hexAscii" <*> pure number <*> pure (if byteMode (documentBuffer doc) || documentLabel doc/=Nothing then CurrentView else toEnum viewIndex) <*> pure Nothing <*> pure split
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
  Sidebar root entries chosen scroll width' <$> o .: "focused"
  where rowParser=withObject "tree row" $ \o->TreeRow <$> (o .: "name" >>= boundedText 32768) <*> (o .: "path" >>= checkedPath True)
          <*> (o .: "depth" >>= boundedInt 0 1024) <*> o .: "directory" <*> o .: "expanded"
positive :: Int -> Parser Int
positive=boundedInt 1 1073741823
boundedInt :: Int -> Int -> Int -> Parser Int
boundedInt low high value=if value>=low && value<=high then pure value else fail "Checkpoint integer out of range"
boundedText :: Int -> Text -> Parser Text
boundedText limit value=if T.length value<=limit then pure value else fail "Checkpoint text too long"
checkedPath :: Bool -> FilePath -> Parser FilePath
checkedPath absolute path=if not (null path) && length path<=32768 && '\0' `notElem` path && (not absolute || isAbsolute path) then pure path else fail "Invalid checkpoint path"
