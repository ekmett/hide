{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Tooling (withTooling, tickTooling, toolingEffects, completionItems, workspaceEdits, hoverText, diagnosticsCurrent) where

import Control.Exception (bracket, try, IOException)
import Control.Concurrent (ThreadId, forkIO, killThread, MVar, newEmptyMVar, putMVar, tryReadMVar)
import Control.Monad (foldM, forM, forM_, unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe, Parser)
import Data.Char (isSpace)
import qualified Data.Aeson.KeyMap as K
import qualified Data.Aeson.Key as Key
import qualified Data.Map.Strict as M
import Data.IORef
import Data.List (find, sortOn)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, pathIsSymbolicLink)
import System.FilePath (takeDirectory, takeExtension, (</>))
import THC.Edit.Buffer
import THC.Edit.Files
import THC.Edit.Model
import qualified THC.Edit.LSP as L

type Target = (Int,Int,Int)
data Pending = Pending LanguageAction Target FilePath (M.Map FilePath (Int,T.Text))
data Session = Session L.Client (IORef (M.Map Int Pending))
data Preparing = Preparing Target FilePath T.Text (M.Map FilePath (Int,T.Text)) ThreadId (MVar (Either IOException (M.Map FilePath (Int,T.Text))))
data Tooling = Tooling
  { sessions :: IORef (M.Map FilePath (Either T.Text Session))
  , roots :: IORef (M.Map FilePath FilePath)
  , problems :: IORef (M.Map FilePath (Maybe Int,[Value]))
  , hovered :: IORef (Maybe Target, Integer, Bool)
  , preparing :: IORef (Maybe Preparing)
  }

withTooling :: (Tooling -> IO a) -> IO a
withTooling = bracket (Tooling <$> newIORef M.empty <*> newIORef M.empty <*> newIORef M.empty <*> newIORef (Nothing,0,False) <*> newIORef Nothing) closeTooling

closeTooling :: Tooling -> IO ()
closeTooling t = do
  cancelPreparation t
  readIORef (sessions t) >>= mapM_ (either (const (pure ())) (\(Session c _) -> L.stopClient c)) . M.elems

cancelPreparation :: Tooling -> IO ()
cancelPreparation t = do
  previous<-atomicModifyIORef' (preparing t) (\p -> (Nothing,p))
  forM_ previous $ \(Preparing _ _ _ _ worker _) -> killThread worker

sourceDocuments :: Desktop -> [(Int,FilePath,Int,T.Text)]
sourceDocuments d = [(bid,filePath f,revision b,contents b) | (bid,doc)<-M.toList (buffers d), documentLabel doc==Nothing, textBuffer (documentBuffer doc),
  Just f<-[documentFile doc], takeExtension (filePath f) `elem` [".hs",".lhs"], let b=documentBuffer doc]

projectRoot :: FilePath -> IO FilePath
projectRoot path = search (takeDirectory path)
  where
    fallback=takeDirectory path
    search dir = do
      entries <- either (const []) id <$> (try (listDirectory dir) :: IO (Either IOException [FilePath]))
      git <- doesDirectoryExist (dir </> ".git")
      if git || any (`elem` entries) ["hie.yaml","cabal.project","stack.yaml",".git"] || any ((==".cabal") . takeExtension) entries
        then pure dir else if takeDirectory dir==dir then pure fallback else search (takeDirectory dir)

rootFor :: Tooling -> FilePath -> IO FilePath
rootFor t path = do
  cached<-readIORef (roots t)
  case M.lookup path cached of
    Just root -> pure root
    Nothing -> do root<-projectRoot path; modifyIORef' (roots t) (M.insert path root); pure root

sessionFor :: Tooling -> FilePath -> IO (Either T.Text Session)
sessionFor t path = do
  root<-rootFor t path
  cached<-readIORef (sessions t)
  case M.lookup root cached of
    Just session -> pure session
    Nothing -> do
      started<-try (L.startClient root) :: IO (Either IOException L.Client)
      session<-case started of
        Left err -> pure (Left ("HLS: "<>T.pack (show err)))
        Right client -> Right . Session client <$> newIORef M.empty
      modifyIORef' (sessions t) (M.insert root session)
      pure session

sync :: Tooling -> Desktop -> IO ()
sync t d = do
  docs<-forM (sourceDocuments d) $ \(_,path,version,text) -> do
    root<-rootFor t path
    _<-sessionFor t path
    pure (root,[(path,version,text)])
  active<-readIORef (sessions t)
  let grouped=M.fromListWith (++) docs
  forM_ (M.toList active) $ \(root,session) -> case session of
    Right (Session client _) -> L.syncDocuments client (M.findWithDefault [] root grouped)
    Left _ -> pure ()

cursorTarget :: Desktop -> Maybe Target
cursorTarget d = do
  w<-activeWindow d
  doc<-activeDocument d
  _<-documentFile doc
  if not (textBuffer (documentBuffer doc)) || documentLabel doc/=Nothing || problemsFocused d || maybe False treeFocused (sideTree d) then Nothing
    else Just (bufferId w,revision (documentBuffer doc),caret (selection w))

currentTarget :: Desktop -> Maybe Target
currentTarget d | dialog d/=Nothing || menu d/=Nothing || contextMenu d/=Nothing || problemsFocused d = Nothing
currentTarget d = case hoverTarget d of Just target -> Just target; Nothing -> cursorTarget d

targetDocument :: Target -> Desktop -> Maybe (FilePath,T.Text)
targetDocument (bid,version,_) d = do
  doc<-M.lookup bid (buffers d)
  file<-documentFile doc
  if not (textBuffer (documentBuffer doc)) || documentLabel doc/=Nothing || revision (documentBuffer doc)/=version || takeExtension (filePath file) `notElem` [".hs",".lhs"] then Nothing
    else Just (filePath file,contents (documentBuffer doc))

sendRequest :: Tooling -> LanguageAction -> Target -> Desktop -> IO Desktop
sendRequest t action target d = case targetDocument target d of
  Nothing -> pure d {status="Save this Haskell source file before requesting language tools."}
  Just (path,text) -> do
    available<-sessionFor t path
    case available of
      Left err -> pure d {status=err}
      Right session -> case action of
        RenameAt name -> do
          cancelPreparation t
          root<-rootFor t path
          result<-newEmptyMVar
          let snapshot=M.fromList [(p,(v,src)) | (_,p,v,src)<-sourceDocuments d]
          worker<-forkIO (try (sourceSnapshot root) >>= putMVar result)
          writeIORef (preparing t) (Just (Preparing target path name snapshot worker result))
          pure d {status="Preparing rename..."}
        _ -> queueRequest session action target path text M.empty >> pure d

queueRequest :: Session -> LanguageAction -> Target -> FilePath -> T.Text -> M.Map FilePath (Int,T.Text) -> IO ()
queueRequest (Session client pending) action target@(_,_,pos) path text snapshot = do
  let (method,extra)=case action of
        TypeInfo -> ("textDocument/hover",[])
        FindDefinition -> ("textDocument/definition",[])
        Completions -> ("textDocument/completion",["context" .= object ["triggerKind" .= (1::Int)]])
        RenameAt name -> ("textDocument/rename",["newName" .= name])
        _ -> ("textDocument/hover",[])
  ident<-L.request client method (object (["textDocument" .= object ["uri" .= L.fileUri path],"position" .= L.positionValue text pos]++extra))
  modifyIORef' pending (M.insert ident (Pending action target path snapshot))

finishPreparation :: Tooling -> Desktop -> IO Desktop
finishPreparation t d = do
  pending<-readIORef (preparing t)
  case pending of
    Nothing -> pure d
    Just (Preparing target path name snapshot _ result)
      | cursorTarget d/=Just target || fmap fst (targetDocument target d)/=Just path ->
          cancelPreparation t >> pure d {status="Rename target changed; request it again."}
      | otherwise -> do
          completed<-tryReadMVar result
          case completed of
            Nothing -> pure d
            Just answer -> do
              writeIORef (preparing t) Nothing
              case answer of
                Left err -> pure d {status="Cannot prepare rename: "<>singleLine (T.pack (show err))}
                Right disk -> do
                  available<-sessionFor t path
                  case (available,targetDocument target d) of
                    (Left err,_) -> pure d {status=err}
                    (Right session,Just (_,text)) -> do
                      queueRequest session (RenameAt name) target path text (M.union snapshot disk)
                      pure d {status="Renaming symbol..."}
                    _ -> pure d

-- Idle hover is debounced; no request is issued for every mouse pixel or keystroke.
tickTooling :: Tooling -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO Desktop
tickTooling t core d = do
  sync t d
  prepared<-finishPreparation t d
  active<-readIORef (sessions t)
  received<-foldM collect prepared (M.elems active)
  updated<-refreshProblems t received
  now<-fromIntegral <$> getMonotonicTimeNSec
  (old,since,sent)<-readIORef (hovered t)
  let target=currentTarget updated
  if old/=target then writeIORef (hovered t) (target,now,False) >> pure updated {typeHint=""}
  else if not sent && now-since>=350000000 then do
    writeIORef (hovered t) (target,since,True)
    maybe (pure updated) (\p -> sendRequest t TypeInfo p updated) target
  else pure updated
  where
    collect desktop (Left _) = pure desktop
    collect desktop (Right (Session client pending)) = do
      events<-L.pollEvents client
      foldM (receive pending) desktop events
    receive pending desktop (L.ServerError err) = do
      writeIORef pending M.empty
      pure desktop {status="HLS: "<>singleLine err,typeHint=""}
    receive _ desktop (L.Diagnostics path version values) = do
      let diagnostics=case values of Array xs -> Vector.toList xs; _ -> []
      modifyIORef' (problems t) (M.insert path (version,diagnostics))
      pure desktop
    receive pending desktop (L.Response ident response) = do
      requests<-readIORef pending
      modifyIORef' pending (M.delete ident)
      case M.lookup ident requests of
        Nothing -> pure desktop
        Just (Pending action target path snapshot)
          | fmap fst (targetDocument target desktop)/=Just path -> pure desktop
          | action==TypeInfo && currentTarget desktop/=Just target -> pure desktop
          | action/=TypeInfo && (cursorTarget desktop/=Just target || dialog desktop/=Nothing) -> pure desktop
          | Just err<-member "error" response -> pure desktop {status="HLS: "<>singleLine (fromMaybe "Request failed" (member "message" err >>= stringValue))}
          | otherwise -> case member "result" response of
              Nothing -> pure desktop
              Just result -> applyResult core action target path snapshot result desktop

member :: T.Text -> Value -> Maybe Value
member key (Object obj)=K.lookup (Key.fromText key) obj
member _ _=Nothing
stringValue :: Value -> Maybe T.Text
stringValue (String text)=Just text
stringValue _=Nothing
singleLine :: T.Text -> T.Text
singleLine=T.unwords . T.words

hoverText :: Value -> T.Text
hoverText result = unicodeTypes (singleLine (T.unlines (filter useful (T.lines (flatten (fromMaybe Null (member "contents" result)))))))
  where
    flatten (String text)=text
    flatten (Array xs)=T.intercalate "\n" (map flatten (Vector.toList xs))
    flatten obj=fromMaybe "" (member "value" obj >>= stringValue)
    useful line=not ("```" `T.isPrefixOf` T.stripStart line) && not (T.null (T.strip line))

-- Display only: keep identifiers and quoted literals intact, and never alter edits.
unicodeTypes :: T.Text -> T.Text
unicodeTypes = T.pack . go . T.unpack
  where
    go [] = []
    go input = let (spaces,rest)=span isSpace input in spaces ++ case lex rest of
      [(token,remaining)] | not (null token) -> pretty token ++ go remaining
      _ -> rest
    pretty "forall" = "∀"
    pretty "->" = "→"
    pretty "=>" = "⇒"
    pretty token = token

applyResult :: (Desktop -> [Effect] -> IO (Bool,Desktop)) -> LanguageAction -> Target -> FilePath -> M.Map FilePath (Int,T.Text) -> Value -> Desktop -> IO Desktop
applyResult core action (bid,version,pos) _ snapshot result d = case action of
  TypeInfo -> pure d {typeHint=hoverText result}
  FindDefinition -> case locations result of
    [] -> pure d {status="No definition found."}
    [(path,row,col)] -> jump core path row col d
    places -> pure (locationDialog "Definitions" places (map locationLabel places) d)
  Completions -> do
    let choices=completionItems (activeText d) pos result
    pure $ if null choices then d {status="No completions available."} else
      (prompt "Complete identifier" (Completing bid version pos choices) [ListBox "Completion" [label | Completion label _<-choices] 0] d)
  RenameAt _ -> applyRename snapshot result d
  _ -> pure d

locationLabel :: (FilePath,Int,Int) -> T.Text
locationLabel (path,row,col)=T.pack path<>":"<>T.pack (show (row+1))<>":"<>T.pack (show (col+1))
locationDialog :: T.Text -> [(FilePath,Int,Int)] -> [T.Text] -> Desktop -> Desktop
locationDialog title places labels = prompt title (Locations places) [ListBox title labels 0]

locations :: Value -> [(FilePath,Int,Int)]
locations (Array xs)=concatMap locations (Vector.toList xs)
locations value=maybe [] pure $ parseMaybe (withObject "location" $ \o -> do
  uri<-o .:? "uri" >>= maybe (o .: "targetUri") pure
  range<-o .:? "range" >>= maybe (o .: "targetSelectionRange") pure
  (row,col)<-rangeStart range
  path<-maybe (fail "non-file URI") pure (L.uriFilePath uri)
  pure (path,row,col)) value

position :: Value -> Parser (Int,Int)
position=withObject "position" $ \o -> do
  row<-o .: "line"; col<-o .: "character"
  if row<0 || col<0 then fail "negative position" else pure (row,col)
rangeStart :: Value -> Parser (Int,Int)
rangeStart=withObject "range" $ \o -> o .: "start" >>= position
rangeOffsets :: T.Text -> Value -> Parser (Int,Int)
rangeOffsets text=withObject "range" $ \o -> do
  start<-o .: "start" >>= position; end<-o .: "end" >>= position
  let a=L.positionOffset text start; z=L.positionOffset text end
  if L.offsetPosition text a/=start || L.offsetPosition text z/=end || a>z then fail "invalid edit range" else pure (a,z)
textEdit :: T.Text -> Value -> Parser (Int,Int,T.Text)
textEdit text=withObject "edit" $ \o -> do
  range<-o .:? "range" >>= maybe (o .: "replace") pure
  (a,z)<-rangeOffsets text range
  inserted<-o .: "newText"
  pure (a,z,inserted)

completionItems :: T.Text -> Int -> Value -> [Completion]
completionItems text pos result = take 200 (mapMaybe parseItem sorted)
  where
    items=case result of Array xs -> Vector.toList xs; _ -> case member "items" result of Just (Array xs) -> Vector.toList xs; _ -> []
    sorted=sortOn (\v -> fromMaybe (fromMaybe "" (member "label" v >>= stringValue)) (member "sortText" v >>= stringValue)) items
    parseItem=parseMaybe $ withObject "completion" $ \o -> do
      label<-o .: "label"
      format<-o .:? "insertTextFormat" .!= (1::Int)
      unless (format==1) (fail "snippets not supported")
      command<-o .:? "command" :: Parser (Maybe Value)
      unless (command==Nothing) (fail "completion commands not supported")
      edit<-o .:? "textEdit"
      primary<-case edit of
        Just value -> textEdit text value
        Nothing -> do
          inserted<-o .:? "insertText" .!= label
          let start=pos-T.length (T.takeWhileEnd wordChar (T.take pos text))
          pure (start,pos,inserted)
      additional<-o .:? "additionalTextEdits" .!= [] >>= mapM (textEdit text)
      pure (Completion label (primary:additional))

-- Resource operations are refused: rename may edit text, never create/delete files.
workspaceEdits :: Value -> Either T.Text [(FilePath,Maybe Int,[Value])]
workspaceEdits result = maybe (Left "Unsupported workspace edit; no files changed.") Right (parseMaybe parse result)
  where
    parse=withObject "workspace edit" $ \o -> do
      changes<-o .:? "changes" .!= K.empty
      documents<-o .:? "documentChanges" .!= []
      direct<-forM (K.toList changes) $ \(uri,edits) -> do
        path<-maybe (fail "non-file URI") pure (L.uriFilePath (Key.toText uri))
        xs<-parseJSON edits
        pure (path,Nothing,xs)
      versioned<-forM documents $ withObject "document edit" $ \doc -> do
        td<-doc .: "textDocument"
        (uri,version)<-withObject "text document" (\x -> (,) <$> x .: "uri" <*> x .:? "version") td
        path<-maybe (fail "non-file URI") pure (L.uriFilePath uri)
        edits<-doc .: "edits"
        pure (path,version,edits)
      let paths=[path | (path,_,_)<-direct++versioned]
      unless (length paths==M.size (M.fromList [(p,()) | p<-paths])) (fail "duplicate document edits")
      pure (direct++versioned)

applyRename :: M.Map FilePath (Int,T.Text) -> Value -> Desktop -> IO Desktop
applyRename snapshot result d = case workspaceEdits result of
  Left err -> pure (message "Cannot rename" [err] d)
  Right [] -> pure d {status="No rename edits returned."}
  Right changes -> do
    prepared<-forM changes $ \(path,version,values) -> do
      let opened=find (\(_,doc) -> fmap filePath (documentFile doc)==Just path) (M.toList (buffers d))
      loaded<-case opened of
        Just (bid,doc) -> pure (Right (Just bid,fromMaybe (FileState path Nothing) (documentFile doc),documentBuffer doc))
        Nothing -> do
          exists<-doesFileExist path
          if not exists then pure (Left "Rename refers to a missing file.") else fmap (\(file,b) -> (Nothing,file,b)) <$> loadFile path
      pure $ do
        (bid,file,b)<-either (Left . T.pack) Right loaded
        unless (textBuffer b) (Left "Cannot apply text edits to a hex buffer.")
        case M.lookup path snapshot of
          Just (oldVersion,oldText) | (bid/=Nothing && oldVersion/=revision b) || oldText/=contents b -> Left "A buffer changed during rename; request it again."
          Nothing -> Left "Rename refers to a file outside the checked project; no files changed."
          _ -> Right ()
        -- HLS uses version 0 for closed files; the disk snapshot above is their guard.
        unless (version==Nothing || version==Just (revision b)) (Left "Rename document version no longer matches.")
        edits<-maybe (Left "Invalid rename edit range.") Right (mapM (parseMaybe (textEdit (contents b))) values)
        let sorted=sortOn (\(a,z,_) -> (a,z)) edits
        unless (and [z<=a' && a/=a' | ((a,z,_),(a',_,_))<-zip sorted (drop 1 sorted)]) (Left "Overlapping rename edits.")
        pure (bid,file,b,sorted)
    case sequence prepared of
      Left err -> pure (message "Cannot rename" [err] d)
      Right edits -> pure $ (foldl apply d edits) {status="Rename applied to buffers. Review and save the changed files."}
  where
    apply desktop (existing,file,b,edits) =
      let opened=case existing of Nothing -> addDocument (Just file) b desktop; Just _ -> desktop
          bid=fromMaybe (nextId desktop) existing
          changed=foldr (\(a,z,text) rest -> T.take a rest<>text<>T.drop z rest) (contents b) edits
          updated=replaceSelection (Selection 0 (T.length (contents b))) changed b
          rebase p=foldl (\q (a,z,text) -> if p<a then q else if p>=z then q+T.length text-(z-a) else a+T.length text) p edits
      in opened {buffers=M.adjust (\doc -> restyle doc {documentBuffer=updated}) bid (buffers opened),
        windows=map (\w -> if bufferId w==bid then w {selection=let Selection a c=selection w in Selection (rebase a) (rebase c)} else w) (windows opened)}

jump :: (Desktop -> [Effect] -> IO (Bool,Desktop)) -> FilePath -> Int -> Int -> Desktop -> IO Desktop
jump core path row col d = do
  (_,opened)<-core d [ReadPath path]
  pure $ if fmap filePath (activeDocument opened >>= documentFile)==Just path
    then moveTo False (L.positionOffset (activeText opened) (row,col)) opened else opened

toolingEffects :: Tooling -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
toolingEffects t core d effects = foldM apply (False,d) effects
  where
    apply state@(True,_) _=pure state
    apply (_,desktop) (JumpTo path row col) = (False,) <$> jump core path row col desktop
    apply (_,desktop) (LanguageRequest RestartLanguage) = do
      closeTooling t
      writeIORef (sessions t) M.empty
      writeIORef (problems t) M.empty
      writeIORef (hovered t) (Nothing,0,False)
      pure (False,desktop {status="Restarting HLS.",typeHint=""})
    apply (_,desktop) (LanguageRequest ShowProblems) = (False,) <$> refreshProblems t desktop
    apply (_,desktop) (LanguageRequest action) = do
      sync t desktop
      case cursorTarget desktop of
        Nothing -> pure (False,desktop {status="Open a saved Haskell source file first."})
        Just target -> (False,) <$> sendRequest t action target desktop
    apply (_,desktop) effect@(SaveDocument bid _ _) = do
      result@(_,updated)<-core desktop [effect]
      case M.lookup bid (buffers updated) of
        Just doc | textBuffer (documentBuffer doc), not (dirty (documentBuffer doc)), Just file<-documentFile doc -> do
          sync t updated
          session<-sessionFor t (filePath file)
          case session of Right (Session client _) -> L.notifySaved client (filePath file); Left _ -> pure ()
        _ -> pure ()
      pure result
    apply (_,desktop) effect = core desktop [effect]

-- Snapshot closed source files before rename so returned ranges cannot overwrite intervening disk edits.
sourceSnapshot :: FilePath -> IO (M.Map FilePath (Int,T.Text))
sourceSnapshot root = do
  entries<-listDirectory root
  M.unions <$> forM (filter (\name -> name `notElem` ["dist-newstyle","dist",".stack-work"] && not ("." `T.isPrefixOf` T.pack name)) entries) (\name -> do
    let path=root </> name
    linked<-pathIsSymbolicLink path
    directory<-doesDirectoryExist path
    if linked then pure M.empty else if directory then sourceSnapshot path
    else if takeExtension path `elem` [".hs",".lhs"] then do
      loaded<-loadFile path
      case loaded of
        Right (file,b) | textBuffer b -> pure (M.singleton (filePath file) (-1,contents b))
        Right _ -> ioError (userError "Cannot snapshot binary Haskell source for rename.")
        Left err -> ioError (userError err)
    else pure M.empty)

refreshProblems :: Tooling -> Desktop -> IO Desktop
refreshProblems t d = do
  entries<-readIORef (problems t)
  let shown=[Diagnostic path version row col severity msg | (path,(version,ds))<-M.toList entries,
        let current=[v | (_,p,v,_)<-sourceDocuments d,p==path],
        diagnosticsCurrent version current,
        value<-ds, Just (row,col,severity,msg)<-[parseMaybe parseDiagnostic value]]
      updated=chooseProblem (problemsSelected d) (d {diagnostics=sortOn (\p -> (diagnosticPath p,diagnosticRow p,diagnosticColumn p)) shown})
      newErrors=null [p | p<-diagnostics d,diagnosticSeverity p==1] && any ((==1) . diagnosticSeverity) shown
  pure (if newErrors && not (problemsVisible d) then setProblemsVisible True updated else updated)
  where
    parseDiagnostic=withObject "diagnostic" $ \o -> do
      (row,col)<-o .: "range" >>= rangeStart
      severity<-o .:? "severity" .!= (1::Int)
      msg<-o .: "message"
      pure (row,col,severity,msg)

-- A versionless notification cannot safely describe an edited open buffer.
diagnosticsCurrent :: Maybe Int -> [Int] -> Bool
diagnosticsCurrent Nothing current = all (==0) current
diagnosticsCurrent (Just version) current = version `elem` current
