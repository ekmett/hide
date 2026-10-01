{-# LANGUAGE CPP, OverloadedStrings, PackageImports #-}
module THC.Edit.Recovery (writeCheckpoint, readCheckpoint) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (unless, when)
import Data.Aeson
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
import THC.Edit.Buffer
import THC.Edit.Files (FileState(..))
import THC.Edit.Model

-- Histories are never truncated to fit. A rejected checkpoint leaves the last
-- complete checkpoint in place, and the caller must surface the returned error.
checkpointLimit :: Int
checkpointLimit=256*1024*1024

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
   "revision" .= snapshotRevision s,"lastChange" .= snapshotLastChange s,"undo" .= map history (snapshotUndo s),"redo" .= map history (snapshotRedo s)]
  where history (text,mode,change)=object ["contents" .= text,"byteMode" .= mode,"change" .= change]

bufferParser :: Value -> Parser Buffer
bufferParser=withObject "buffer" $ \o->do
  snapshot<-BufferSnapshot <$> o .: "contents" <*> o .: "saved" <*> (o .: "undo" >>= mapM history) <*> (o .: "redo" >>= mapM history)
    <*> o .: "revision" <*> o .: "lastChange" <*> o .: "byteMode" <*> o .: "savedByteMode"
  either (fail . T.unpack) pure (restoreBuffer snapshot)
  where history=withObject "history" $ \o->(,,) <$> o .: "contents" <*> o .: "byteMode" <*> o .: "change"

fileValue :: FileState -> Value
fileValue file=object ["path" .= filePath file,"diskBytes" .= fmap (TE.decodeUtf8 . B64.encode) (diskBytes file)]
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
documentValue :: Desktop -> (Int,Document) -> Value
documentValue desktop (ident,doc)=object ["id" .= ident,"buffer" .= bufferValue recoveredBuffer,"file" .= fmap fileValue (documentFile doc),
  "label" .= recoveredLabel (documentLabel doc),"suggestedName" .= documentSuggestedName doc]
  where
    -- Pending answers no longer have a waiter after a crash. Their rendered
    -- interaction spans must not become public transcript when actions reset.
    privateSpans=[(a,z) | (a,z,action,_)<-chatActions desktop,"question-" `T.isPrefixOf` action]
    recoveredBuffer
      | documentLabel doc==Just "Conversation",not (null privateSpans)=newBuffer (T.pack
          [if c/='\n' && c/='\r' && any (\(a,z)->index>=a && index<z) privateSpans then ' ' else c
          | (index,c)<-zip [0..] (T.unpack (contents (documentBuffer doc)))])
      | otherwise=documentBuffer doc
    recoveredLabel (Just label) | "Terminal " `T.isPrefixOf` label=Just ("Ended "<>label)
    recoveredLabel label=label

desktopValue :: Desktop -> Value
desktopValue d=object ["schemaVersion" .= (1::Int),"screen" .= screenSize d,"buffers" .= map (documentValue d) (M.toAscList documents),
  "windows" .= map windowValue [w | w<-windows d,M.member (bufferId w) documents],"nextId" .= nextId d,
  "composer" .= bufferValue (composerBuffer d),"composerSelection" .= selectionValue (composerSelection d),"composerFocused" .= composerFocused d,
  "directory" .= defaultDirectory d,"sidebar" .= fmap sidebarValue (sideTree d),"preferences" .= object
    ["wordStar" .= wordStar d,"blinkCursor" .= blinkCursor d,"crtFilter" .= crtFilter d,"pixelateUnicode" .= pixelateUnicode d,
     "materialIcons" .= materialIcons d,"streamerMode" .= streamerMode d,"appearance" .= fromEnum (appearance d),"videoMode" .= videoMode d,
     "problemsVisible" .= problemsVisible d,"problemsHeight" .= problemsPreferredHeight d,"messagesNumber" .= messagesNumber d]]
  where documents=M.filter keptDocument (buffers d)

windowValue :: Window -> Value
windowValue w=object ["id" .= windowId w,"bufferId" .= bufferId w,"number" .= windowNumber w,"bounds" .= rectValue (bounds w),
  "selection" .= selectionValue (selection w),"scrollRow" .= scrollRow w,"scrollColumn" .= scrollColumn w,
  "restoredBounds" .= fmap rectValue (restoredBounds w),"hexLow" .= windowHexLow w,"hexAscii" .= windowHexAscii w]
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
  ident<-o .: "nextId" >>= positive
  unless (all (<ident) (M.keys documents++map windowId views)) (fail "Invalid next ID")
  composer<-o .: "composer" >>= bufferParser
  composerSelection'<-o .: "composerSelection" >>= selectionParser (bufferLength composer)
  composerFocused'<-o .: "composerFocused"
  directory<-o .: "directory" >>= traverse (checkedPath True)
  sidebar<-o .: "sidebar" >>= traverse sidebarParser
  prefs<-o .: "preferences"
  wordStar'<-prefs .: "wordStar"; blink<-prefs .: "blinkCursor"; crt<-prefs .: "crtFilter"
  pixelate<-prefs .: "pixelateUnicode"; icons<-prefs .: "materialIcons"; streamer<-prefs .: "streamerMode"
  look<-prefs .: "appearance"
  unless (look>=0 && look<=2) (fail "Invalid appearance")
  mode<-prefs .: "videoMode" >>= traverse (boundedInt 0 65535)
  problems<-prefs .: "problemsVisible"; preferred<-prefs .: "problemsHeight" >>= boundedInt 0 4096
  messages<-prefs .: "messagesNumber" >>= traverse positive
  pure baseline {screenSize=size,buffers=documents,windows=views,nextId=ident,composerBuffer=composer,composerSelection=composerSelection',composerFocused=composerFocused',
    defaultDirectory=directory,sideTree=sidebar,wordStar=wordStar',blinkCursor=blink,crtFilter=crt,pixelateUnicode=pixelate,materialIcons=icons,streamerMode=streamer,
    appearance=toEnum look,videoMode=mode,problemsVisible=problems,problemsPreferredHeight=preferred,messagesNumber=messages,
    menu=Nothing,dialog=Nothing,drag=Nothing,dragOriginal=Nothing,clipboard="",clipboardExport=(0,Nothing),prefix=Nothing,blockStart=Nothing,
    status="Recovered session. Background processes ended; reconnect agents as needed.",lastFind="",branchStatus="",branchAdded=0,branchDeleted=0,branchRoot=Nothing,
    gitReview=Nothing,hoverTarget=Nothing,typeHint="",buttonHover=Nothing,buttonPressed=Nothing,contextMenu=Nothing,contextKind=SourceContext,
    diagnostics=[],buildDiagnostics=[],problemsSelected=0,problemsScroll=0,problemsFocused=False,statusHover=Nothing,heldModifiers=[],
    agentSteering=False,agentReplying=False,agentQueued=0,agentContextUsage=Nothing,chatQuestion=Nothing,chatActions=[],chatInputOffset=Nothing}

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
  Window ident bid rectangle selected row column restored <$> o .: "hexLow" <*> o .: "hexAscii" <*> pure number
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
