{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.WorkspaceMCP (workspaceTools, workspaceToolNames, workspaceTool) where

import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath ((</>), isAbsolute)
import System.IO.Error (tryIOError)
import THC.Edit.Buffer
import THC.Edit.Browser (packageFile)
import THC.Edit.Build (resolveBuildRoot, buildSource)
import THC.Edit.Files (filePath)
import THC.Edit.Git
import THC.Edit.GuestAccess (protectedBuffer, protectedPath, protectedPathParent, privateDocument, sanitizedStatus)
import THC.Edit.Model

workspaceToolNames :: [Text]
workspaceToolNames = [name | (name,_,_,_,_) <- definitions]

workspaceTools :: [Value]
workspaceTools = [object ["name" .= name,"description" .= description,
  "inputSchema" .= object ["type" .= ("object"::Text),"properties" .= Object (KM.fromList properties),
    "required" .= required,"additionalProperties" .= False],
  "annotations" .= object ["readOnlyHint" .= readonly,"destructiveHint" .= (name=="editor_file"),"openWorldHint" .= False]]
  | (name,description,properties,required,readonly)<-definitions]

field :: Text -> Text -> Value
field kind description=object ["type" .= kind,"description" .= description]
choice :: [Text] -> Value
choice values=object ["type" .= ("string"::Text),"enum" .= values]

definitions :: [(Text,Text,[(K.Key,Value)],[Text],Bool)]
definitions =
  [("editor_layout","Read screen resolution in character cells, window rectangles, cursor positions, modes and docked panels.",[],[],True)
  ,("editor_navigate","Focus an open window/buffer or open a file, then navigate. Lines/columns are 1-based Unicode code points; byteOffset is 0-based and requires hex mode. Never saves a file.",targets++[("path",field "string" "Existing file relative to the workspace directory."),("line",integer),("column",integer),("byteOffset",integer)],[],False)
  ,("editor_arrange","Arrange editor windows through normal docking rules. Geometry is in character cells, x/y start at 0. Result reports actual constrained geometry.",[("action",choice ["tile","cascade","split_vertical","split_horizontal","focus","move","resize","zoom"]),("windowId",integer),("x",integer),("y",integer),("width",integer),("height",integer)],["action"],False)
  ,("editor_panels","Show/hide the files tree or messages panel and resize their docks in character cells.",[("files",boolean),("messages",boolean),("filesWidth",integer),("messagesHeight",integer)],[],False)
  ,("editor_mode","Select text or hex mode for a buffer. Conversion preserves bytes and refuses invalid UTF-8 or NUL-containing text.",targets++[("mode",choice ["text","hex"])],["mode"],False)
  ,("editor_file","Open an existing file, save a buffer, or close one window. Save/close require the current buffer revision. Dirty close requires dirtyAction save or discard; discard only removes this view, retaining a buffer shared by other windows. Save uses the editor's disk-conflict checks; no implicit overwrite.",targets++[("action",choice ["open","save","close"]),("path",field "string" "Existing path to open, or destination for an untitled save."),("revision",integer),("dirtyAction",choice ["save","discard"])],["action"],False)
  ,("workspace_project","Read the enclosing project root, Cabal package file, active source, and live unsaved buffers. File/project metadata comes from disk.",[],[],True)
  ,("workspace_diagnostics","Read the live HLS/build/messages snapshot. Entries include reported versions and current buffer revisions, so stale diagnostics are distinguishable. Messages are truncated to 8192 characters.",[("offset",integer),("limit",integer)],[],True)
  ,("workspace_git","Read Git status or disk/worktree diff. Unsaved editor changes are listed separately and are not included in the Git diff. Diff response is capped at 128 Ki characters.",[("view",choice ["status","diff"]),("path",field "string" "Optional file filter for diff, relative to the workspace directory.")],["view"],True)]
  where
    integer=field "integer" "Integer in the documented coordinate or identifier range."
    boolean=field "boolean" "Desired visibility."
    targets=[("windowId",integer),("bufferId",integer)]

type Apply = Desktop -> [Effect] -> IO (Bool,Desktop)
type Reply = (Desktop,IO (Either Text Value))

-- The caller holds the desktop lock for this phase. Only read-only project/Git
-- work is returned as deferred IO; the reply captures the matching live snapshot.
workspaceTool :: Apply -> Desktop -> Text -> Value -> IO Reply
workspaceTool apply desktop name args = case parseEither parse args of
  Left err -> failure desktop (T.pack err)
  Right fields -> dispatch fields
  where
    parse=withObject "workspace arguments" $ \o -> do
      allowed<-case find (\(n,_,_,_,_)->n==name) definitions of
        Nothing -> fail "Unknown workspace tool"
        Just (_,_,properties,_,_) -> pure (map fst properties)
      unless (all (`elem` allowed) (KM.keys o)) (fail "Unknown argument")
      pure o
    dispatch o = case name of
      "editor_layout" -> success desktop
      "editor_navigate" -> parsed desktop (navigationArgs o) $ \(wid,bid,path,line,col,offset) -> do
        case validateLocation line col offset of
          Left err -> failure desktop err
          Right () -> target desktop wid bid path >>= \selected -> case selected of
            Left err -> failure desktop err
            Right d -> case navigate line col offset d of
              Left err -> failure d err
              Right next -> success next
      "editor_mode" -> parsed desktop ((,,) <$> o .:? "windowId" <*> o .:? "bufferId" <*> o .: "mode") $ \(wid,bid,mode) ->
        case selectTarget desktop wid bid of
          Left err -> failure desktop err
          Right d -> case setMode mode d of Left err -> failure desktop err; Right next -> success next
      "editor_arrange" -> parsed desktop ((,,,,,) <$> o .: "action" <*> o .:? "windowId" <*> o .:? "x" <*> o .:? "y" <*> o .:? "width" <*> o .:? "height") $ \(action,wid,x,y,w,h) ->
        case arrange action wid x y w h desktop of
          Left err -> failure desktop err
          Right (d,effects) -> apply d effects >>= success . snd
      "editor_panels" -> parsed desktop ((,,,) <$> o .:? "files" <*> o .:? "messages" <*> o .:? "filesWidth" <*> o .:? "messagesHeight") $ \(files,messages,fw,mh) ->
        if maybe False (\n->n<16 || n>fst (screenSize desktop)-20) fw || maybe False (\n->n<3 || n>snd (screenSize desktop)-7) mh
        then failure desktop "Panel sizes exceed the available screen; filesWidth >= 16 and messagesHeight >= 3."
        else if (isJust fw && not (fromMaybe (isJust (sideTree desktop)) files)) || (isJust mh && not (fromMaybe (problemsVisible desktop) messages))
        then failure desktop "Show a panel before setting its size."
        else do
          d<-case files of
            Just True | not (isJust (sideTree desktop)) -> snd <$> apply desktop [ReadTree (startingDirectory desktop)]
            Just False -> pure (setTree Nothing desktop)
            _ -> pure desktop
          let shown=maybe d (`setProblemsVisible` d) messages
              sized=maybe shown (\n->resizeTree (n-1) shown) fw
              next=maybe sized (\n->resizeProblems (snd (screenSize sized)-n-1) sized) mh
          if (isJust fw && not (isJust (sideTree next))) || (isJust mh && not (problemsVisible next))
            then failure d "Show a panel before setting its size."
            else success next {drag=Nothing,dragOriginal=Nothing}
      "editor_file" -> parsed desktop ((,,,,,) <$> o .: "action" <*> o .:? "windowId" <*> o .:? "bufferId" <*> o .:? "path" <*> o .:? "revision" <*> o .:? "dirtyAction") $ \(action,wid,bid,path,rev,decision) ->
        fileAction apply desktop action wid bid path rev decision
      "workspace_project" -> pure (desktop,ioResult $ do
        root<-resolveBuildRoot desktop
        package<-packageFile root
        pure (object ["root" .= root,"packageFile" .= package,"source" .= (if maybe False (protectedBuffer desktop . bufferId) (activeWindow desktop) then Nothing else buildSource desktop >>= \p->if protectedPath desktop p then Nothing else Just p),"unsavedBuffers" .= unsaved desktop]))
      "workspace_diagnostics" -> parsed desktop ((,) <$> o .:? "offset" .!= 0 <*> o .:? "limit" .!= 100) $ \(offset,limit) ->
        if offset<0 || limit<1 || limit>200 then failure desktop "Use offset >= 0 and limit 1..200."
        else pure (desktop,pure (Right (object ["total" .= length (diagnostics desktop),"offset" .= offset,
          "diagnostics" .= map (diagnosticValue desktop) (take limit (drop offset (diagnostics desktop))),
          "buildDiagnosticCount" .= length (buildDiagnostics desktop),"status" .= T.take 8192 (sanitizedStatus desktop)])))
      "workspace_git" -> parsed desktop ((,) <$> o .: "view" <*> o .:? "path") $ \(view,path) ->
        if view/=("status"::Text) && view/="diff" then failure desktop "Unknown Git view."
        else if maybe False invalidPath path then failure desktop "Invalid path."
        else if view=="status" && isJust path then failure desktop "path is only a diff filter."
        else pure (desktop,do
          result<-tryIOError $ if view=="status" then do
            repo<-repositoryStatus (startingDirectory desktop)
            pure $ case repo of
              Nothing -> Left "Git repository unavailable."
              Just r -> Right (object ["root" .= repoRoot r,"branch" .= repoBranch r,"diskDirty" .= repoDirty r,
                "added" .= repoAdded r,"deleted" .= repoDeleted r,"unsavedBuffers" .= unsaved desktop])
            else do
              let renderDiff (text,omitted)=object ["diff" .= T.take 131072 text,"truncated" .= (T.length text>131072),"includesUnsaved" .= False,
                    "unsavedBuffers" .= unsaved desktop,"omittedFiles" .= omitted]
              fmap (fmap renderDiff) (repositoryDiffFilteredAt (startingDirectory desktop) path (protectedPath desktop))
          pure (either (Left . T.pack . show) id result))
      _ -> failure desktop "Unknown workspace tool"
    target d wid bid Nothing=pure (selectTarget d wid bid)
    target d wid bid (Just path)
      | isJust wid || isJust bid=pure (Left "Choose one of path, windowId or bufferId.")
      | otherwise=openPath apply d path

parsed :: Desktop -> Parser a -> (a -> IO Reply) -> IO Reply
parsed d parser action=either (failure d . T.pack) action (parseEither (const parser) Null)
failure :: Desktop -> Text -> IO Reply
failure d err=pure (d,pure (Left err))
success :: Desktop -> IO Reply
success d=pure (d,pure (Right (layout d)))
ioResult :: IO Value -> IO (Either Text Value)
ioResult action=either (Left . T.pack . show) Right <$> tryIOError action

selectTarget :: Desktop -> Maybe Int -> Maybe Int -> Either Text Desktop
selectTarget d wid bid = do
  when (isJust wid && isJust bid) (Left "Choose either windowId or bufferId.")
  w<-case (wid,bid) of
    (Just ident,_) -> maybe (Left "Window not found.") Right (find ((==ident).windowId) (windows d))
    (_,Just ident) -> maybe (Left "Buffer has no open window.") Right (find ((==ident).bufferId) (windows d))
    _ -> maybe (Left "No active window.") Right (activeWindow d)
  when (protectedBuffer d (bufferId w)) (Left "This conversation or approval window is controlled by the user.")
  pure (focusWindow (windowId w) d)

invalidPath :: FilePath -> Bool
invalidPath path=null path || length path>32768 || '\0' `elem` path

openPath :: Apply -> Desktop -> FilePath -> IO (Either Text Desktop)
openPath apply d path
  | invalidPath path=pure (Left "Invalid path.")
  | otherwise=do
      checked<-tryIOError $ do
        absolute<-canonicalizePath (if isAbsolute path then path else startingDirectory d </> path)
        when (protectedPath d absolute) (ioError (userError "This path contains private editor configuration or session data."))
        exists<-doesFileExist absolute
        pure (absolute,exists)
      case checked of
        Left err -> pure (Left (T.pack (show err)))
        Right (absolute,exists) -> case find (\w->maybe False ((==Just absolute).fmap filePath.documentFile) (M.lookup (bufferId w) (buffers d))) (windows d) of
          Just w -> pure (Right (focusWindow (windowId w) d))
          Nothing | not exists -> pure (Left "File does not exist; opening does not create files.")
          Nothing -> do
            (_,next)<-apply d [ReadPath absolute]
            pure $ case find (\w->maybe False ((==Just absolute).fmap filePath.documentFile) (M.lookup (bufferId w) (buffers next))) (windows next) of
              Nothing -> Left ("Could not open file: "<>status next)
              Just w -> Right (focusWindow (windowId w) next)

navigationArgs :: Object -> Parser (Maybe Int,Maybe Int,Maybe FilePath,Maybe Int,Maybe Int,Maybe Int)
navigationArgs o=(,,,,,) <$> o .:? "windowId" <*> o .:? "bufferId" <*> o .:? "path" <*> o .:? "line" <*> o .:? "column" <*> o .:? "byteOffset"
validateLocation :: Maybe Int -> Maybe Int -> Maybe Int -> Either Text ()
validateLocation line col offset=do
  when (maybe False (<1) line || maybe False (<1) col || maybe False (<0) offset) (Left "Lines/columns start at 1; byte offsets start at 0.")
  when (isJust offset && (isJust line || isJust col)) (Left "Use byteOffset or line/column, not both.")
  when (isJust col && not (isJust line)) (Left "column requires line.")

navigate :: Maybe Int -> Maybe Int -> Maybe Int -> Desktop -> Either Text Desktop
navigate line col offset d=do
  doc<-maybe (Left "No active buffer.") Right (activeDocument d)
  let b=documentBuffer doc
  case offset of
    Just n -> do
      unless (byteMode b) (Left "Select hex mode before navigating by byteOffset.")
      unless (n<=bufferLength b) (Left "Byte offset is past the end of the buffer.")
      pure (moveTo False n d)
    Nothing -> case line of
      Nothing -> Right d
      Just n -> do
        when (byteMode b) (Left "Use byteOffset in hex mode.")
        unless (n<=bufferLineCount b) (Left "Line is past the end of the buffer.")
        let column=fromMaybe 1 col; text=T.dropWhileEnd (=='\n') (bufferLineAt b (n-1))
        unless (column-1<=T.length text) (Left "Column is past the end of the line.")
        pure (moveTo False (bufferLineOffset b (n-1)+column-1) d)

setMode :: Text -> Desktop -> Either Text Desktop
setMode mode d=do
  unless (mode `elem` ["text","hex"]) (Left "Mode must be text or hex.")
  doc<-maybe (Left "No active buffer.") Right (activeDocument d)
  let b=documentBuffer doc
  if byteMode b==(mode=="hex") then Right d else do
    when (isJust (documentLabel doc)) (Left "This buffer is read-only.")
    _<-toggleByteMode b
    pure (fst (runCommand ToggleHex d))

arrange :: Text -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Desktop -> Either Text (Desktop,[Effect])
arrange action wid x y w h original=do
  when (any (maybe False (\n->n<0 || n>1000000)) [x,y,w,h]) (Left "Geometry must be between 0 and 1000000 cells.")
  d<-if isJust wid then selectTarget original wid Nothing else Right original
  when (action/="move" && (isJust x || isJust y)) (Left "x/y require action move.")
  when (action/="resize" && (isJust w || isJust h)) (Left "width/height require action resize.")
  case action of
    "tile" -> Right (runCommand Tile d)
    "cascade" -> Right (runCommand Cascade d)
    "split_vertical" -> needWindow d >> Right (runCommand SplitVertical d)
    "split_horizontal" -> needWindow d >> Right (runCommand SplitHorizontal d)
    "focus" -> needWindow d >> Right (d,[])
    "zoom" -> needWindow d >> Right (runCommand Zoom d)
    "move" -> do
      v<-needWindow d
      a<-maybe (Left "move requires x and y.") Right x
      b<-maybe (Left "move requires x and y.") Right y
      pure (mapWindow (windowId v) (\window->window {bounds=fitMovingWindow d ((bounds window) {left=a,top=b}),restoredBounds=Nothing}) d,[])
    "resize" -> do
      v<-needWindow d
      a<-maybe (Left "resize requires width and height.") Right w
      b<-maybe (Left "resize requires width and height.") Right h
      unless (a>=16 && b>=5) (Left "Windows require width >= 16 and height >= 5.")
      pure (resizeWindowBounds (windowId v) ((bounds v) {width=a,height=b}) d,[])
    _ -> Left "Unknown arrangement action."
  where needWindow d=do
          window<-maybe (Left "No active window.") Right (activeWindow d)
          when (protectedBuffer d (bufferId window)) (Left "This conversation or approval window is controlled by the user.")
          pure window

fileAction :: Apply -> Desktop -> Text -> Maybe Int -> Maybe Int -> Maybe FilePath -> Maybe Int -> Maybe Text -> IO Reply
fileAction apply original action wid bid path rev decision
  | action=="open" = if isJust wid || isJust bid || isJust rev || isJust decision then failure original "open accepts only path."
    else maybe (failure original "open requires path.") (\p->openPath apply original p >>= either (failure original) success) path
  | action/="save" && action/="close" = failure original "Unknown file action."
  | otherwise=case selectTarget original wid bid of
      Left err -> failure original err
      Right d -> case (activeWindow d,activeDocument d) of
        (Just window,Just doc) -> let b=documentBuffer doc in
          if rev/=Just (revision b) then failure original "Current buffer revision is required; refresh list_buffers."
          else if isJust path && (action=="close" || isJust (documentFile doc)) then failure original "A destination path is only allowed when saving an untitled buffer."
          else if maybe False invalidPath path then failure original "Invalid destination path."
          else if maybe False (`notElem` ["save","discard"]) decision then failure original "dirtyAction must be save or discard."
          else if action=="save" && isJust decision then failure original "dirtyAction is only used by close."
          else if action=="close" && dirty b && decision==Nothing then failure original "Dirty buffer: specify dirtyAction save or discard."
          else if action=="close" && decision/=Just "save" then success (closeActive d)
          else if isJust (documentLabel doc) then failure original "This buffer is read-only."
          else if documentFile doc==Nothing && path==Nothing then failure original "Untitled buffer: save with a destination path first."
          else do
            destinationResult<-tryIOError $ traverse (\p->do
              absolute<-canonicalizePath (if isAbsolute p then p else startingDirectory d </> p)
              when (protectedPathParent d absolute) (ioError (userError "This path contains private editor configuration or session data."))
              pure absolute) path
            case destinationResult of
              Left err -> failure original (T.pack (show err))
              Right destination -> do
                (_,savedDesktop)<-apply d [SaveDocument (bufferId window) destination Nothing]
                case M.lookup (bufferId window) (buffers savedDesktop) of
                  Just savedDoc | not (dirty (documentBuffer savedDoc)),isJust (documentFile savedDoc),dialog savedDesktop==Nothing ->
                    success (if action=="close" then closeActive (focusWindow (windowId window) savedDesktop) else savedDesktop)
                  _ -> failure savedDesktop ("Save did not complete: "<>failureDetail savedDesktop)
        _ -> failure original "No active buffer."

failureDetail :: Desktop -> Text
failureDetail d=T.take 8192 (maybe (status d) (T.intercalate " " . body) (dialog d))

rectValue :: Rect -> Value
rectValue r=object ["x" .= left r,"y" .= top r,"width" .= width r,"height" .= height r]

layout :: Desktop -> Value
layout d=object ["screen" .= object ["columns" .= fst (screenSize d),"rows" .= snd (screenSize d),"videoMode" .= videoMode d],
  "windowCount" .= length (windows d),"windows" .= map windowValue (take 256 (windows d)),"windowsTruncated" .= (length (windows d)>256),
  "files" .= object ["visible" .= isJust (sideTree d),"width" .= maybe 0 treeWidth (sideTree d),"root" .= fmap treeRoot (sideTree d)],
  "messages" .= object ["visible" .= problemsVisible d,"height" .= problemsHeight d,"bounds" .= rectValue (problemsRect d)],
  "status" .= T.take 8192 (sanitizedStatus d)]
  where
    windowValue w=object (["windowId" .= windowId w,"bufferId" .= bufferId w,"focused" .= (fmap windowId (activeWindow d)==Just (windowId w)),"bounds" .= rectValue (bounds w)]++
      case M.lookup (bufferId w) (buffers d) of
        Nothing -> []
        Just doc -> let b=documentBuffer doc; (row,column)=bufferLineColumn b (caret (selection w)) in
          ["path" .= visiblePath d doc,"label" .= (if privateDocument d doc then Just "[private]" else documentLabel doc),"dirty" .= dirty b,"revision" .= revision b,
           "mode" .= (if byteMode b then "hex" else "text"::Text),"line" .= (row+1),"column" .= (column+1),
           "byteOffset" .= (if byteMode b then Just (caret (selection w)) else Nothing),"readOnly" .= isJust (documentLabel doc)])

visiblePath :: Desktop -> Document -> Maybe FilePath
visiblePath d doc=if privateDocument d doc then Nothing else fmap filePath (documentFile doc)

unsaved :: Desktop -> [Value]
unsaved d=take 256 [object ["bufferId" .= ident,"path" .= visiblePath d doc,"revision" .= revision b,"byteLength" .= BS.length (bufferBytes b)]
  | (ident,doc)<-M.toAscList (buffers d),let b=documentBuffer doc,dirty b]

diagnosticValue :: Desktop -> Diagnostic -> Value
diagnosticValue d entry=object ["path" .= diagnosticPath entry,"line" .= (diagnosticRow entry+1),"column" .= (diagnosticColumn entry+1),
  "severity" .= diagnosticSeverity entry,"message" .= T.take 8192 (diagnosticMessage entry),"truncated" .= (T.length (diagnosticMessage entry)>8192),
  "reportedVersion" .= diagnosticVersion entry,"liveRevisions" .= [revision (documentBuffer doc) | doc<-M.elems (buffers d),fmap filePath (documentFile doc)==Just (diagnosticPath entry)]]
