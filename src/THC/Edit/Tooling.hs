{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Tooling (Tooling, toolingTool, toolingTools, toolingToolNames, withTooling, tickTooling, toolingEffects, completionItems, workspaceEdits, hoverText, diagnosticsCurrent) where

import Control.Exception (bracket, try, IOException, onException, mask_, evaluate)
import Control.Concurrent.STM
import System.Timeout (timeout)
import System.Mem.StableName (StableName, makeStableName)
import Control.Concurrent (ThreadId, forkIO, killThread, MVar, newEmptyMVar, putMVar, readMVar, tryReadMVar)
import Control.Monad (filterM, foldM, forM, forM_, unless, when, void)
import Data.Aeson
import Data.Aeson.Types (parseMaybe, parseEither, Parser)
import Data.Char (isSpace)
import qualified Data.Aeson.KeyMap as K
import qualified Data.Aeson.Key as Key
import qualified Data.Map.Strict as M
import Data.IORef
import Data.List (find, sortOn)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (canonicalizePath, doesFileExist, doesDirectoryExist, listDirectory, pathIsSymbolicLink)
import System.FilePath (takeDirectory, takeExtension, (</>))
import THC.Edit.Buffer
import THC.Edit.Files
import THC.Edit.GuestAccess (protectedBuffer, protectedPath)
import THC.Edit.Model
import qualified THC.Edit.LSP as L

type Target = (Int,Int,Int)
data Pending = Pending LanguageAction Target FilePath (M.Map FilePath (Int,T.Text)) | ToolPending ToolQuery | CommandPending ToolQuery (Maybe T.Text)
data ToolQuery = ToolQuery
  { queryName :: T.Text, queryTarget :: Target, queryPath :: FilePath, queryArguments :: Value
  , querySnapshot :: M.Map FilePath (Int,T.Text), queryDeadline :: Integer
  , queryReply :: TMVar (Either T.Text Value), queryHuman :: Bool
  , queryProgress :: TVar (Int,M.Map Int Int) }
data CachedAction = CachedAction ToolQuery Value Bool
data Session = Session L.Client (IORef (M.Map Int Pending))
data Preparing = Preparing Target FilePath T.Text (M.Map FilePath (Int,T.Text)) ThreadId (MVar (Either IOException (M.Map FilePath (Int,T.Text))))
  | ToolPreparing ToolQuery ThreadId (MVar (Either IOException (M.Map FilePath (Int,T.Text))))
data Tooling = Tooling
  { sessions :: IORef (M.Map FilePath (Either T.Text Session))
  , roots :: IORef (M.Map FilePath FilePath)
  , problems :: IORef (M.Map FilePath (Maybe Int,[Value]))
  , hovered :: IORef (Maybe Target, Integer, Bool)
  , actions :: IORef (M.Map T.Text CachedAction), nextAction :: IORef Int
  , retiring :: IORef [MVar ()]
  , preparing :: IORef (Maybe Preparing)
  , synchronized :: IORef (M.Map FilePath (StableName L.Client, [(Int,FilePath,Int,StableName Buffer)]))
  }

withTooling :: (Tooling -> IO a) -> IO a
withTooling = bracket (Tooling <$> newIORef M.empty <*> newIORef M.empty <*> newIORef M.empty <*> newIORef (Nothing,0,False) <*> newIORef M.empty <*> newIORef 1 <*> newIORef [] <*> newIORef Nothing <*> newIORef M.empty) closeTooling

closeTooling :: Tooling -> IO ()
closeTooling t = do
  cancelPreparation t
  readIORef (retiring t) >>= mapM_ readMVar
  writeIORef (retiring t) []
  writeIORef (actions t) M.empty
  readIORef (sessions t) >>= mapM_ (either (const (pure ())) (\(Session c pending) -> do
    readIORef pending >>= mapM_ (failPending "HLS stopped") . M.elems
    L.stopClient c)) . M.elems

cancelPreparation :: Tooling -> IO ()
cancelPreparation t = do
  previous<-atomicModifyIORef' (preparing t) (\p -> (Nothing,p))
  forM_ previous $ \job -> case job of
    Preparing _ _ _ _ worker _ -> killThread worker
    ToolPreparing query worker _ -> completeTool query (Left "Rename preparation cancelled") >> killThread worker

-- These calls share the session's HLS client and response pump. The returned
-- wait action must run outside the desktop lock, so tickTooling can resolve it.
toolingToolNames :: [T.Text]
toolingToolNames = ["lsp_hover","lsp_definition","lsp_type_definition","lsp_references","lsp_document_symbols","lsp_rename","lsp_code_actions","lsp_apply_code_action"]

toolingTools :: [Value]
toolingTools = map descriptor toolingToolNames
  where
    integer minimumValue=object ["type" .= ("integer"::T.Text),"minimum" .= (minimumValue::Int)]
    descriptor name=object
      ["name" .= name,"description" .= (description name<>" Uses live unsaved Haskell source. Input line/column are 1-based Unicode codepoints; raw LSP result coordinates are 0-based UTF-16."),
       "inputSchema" .= object ["type" .= ("object"::T.Text),"additionalProperties" .= False,
         "properties" .= object (["bufferId" .= integer 0,"revision" .= integer 0]++
           (if name `elem` ["lsp_document_symbols","lsp_apply_code_action"] then [] else ["line" .= integer 1,"column" .= integer 1])++
           ["actionId" .= object ["type" .= ("string"::T.Text)] | name=="lsp_apply_code_action"]++
           (if name=="lsp_code_actions" then ["endLine" .= integer 1,"endColumn" .= integer 1] else [])++
           ["newName" .= object ["type" .= ("string"::T.Text),"minLength" .= (1::Int),"maxLength" .= (256::Int)] | name=="lsp_rename"]++
           ["includeDeclaration" .= object ["type" .= ("boolean"::T.Text),"default" .= True] | name=="lsp_references"]),
         "required" .= (["bufferId"]++(if name `elem` ["lsp_document_symbols","lsp_apply_code_action"] then [] else ["line","column"])++["revision" | name `elem` ["lsp_rename","lsp_code_actions","lsp_apply_code_action"]]++["newName" | name=="lsp_rename"]++["actionId" | name=="lsp_apply_code_action"]::[T.Text])],
       "annotations" .= object ["readOnlyHint" .= (name `notElem` ["lsp_rename","lsp_apply_code_action"]),"destructiveHint" .= False,"idempotentHint" .= (name `notElem` ["lsp_rename","lsp_code_actions","lsp_apply_code_action"]),"openWorldHint" .= (name=="lsp_apply_code_action")]]
    description :: T.Text -> T.Text
    description name=case name of
      "lsp_hover" -> "Return HLS hover/type information."
      "lsp_definition" -> "Find HLS definitions."
      "lsp_type_definition" -> "Find HLS type definitions."
      "lsp_references" -> "Find HLS references."
      "lsp_document_symbols" -> "List HLS document symbols."
      "lsp_code_actions" -> "List HLS code actions with opaque action IDs; optional endLine/endColumn select a range. A new list expires previous IDs. Only advertised server commands can execute."
      "lsp_apply_code_action" -> "Apply a listed action once at its original revision. Checked text edits change buffers only; advertised commands run in HLS and may have server-side effects. Command results report succeeded, commandSucceeded, appliedBatches, partial and changed buffers. Earlier accepted edits remain after failure/cancellation. Resource operations are rejected."
      _ -> "Rename through HLS, requiring the current revision; edits change buffers, never saved files."

toolingTool :: Tooling -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> T.Text -> Value -> IO (Desktop, IO (Either T.Text Value))
toolingTool = startTool False

startTool :: Bool -> Tooling -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> T.Text -> Value -> IO (Desktop, IO (Either T.Text Value))
startTool human t _ d name arguments = case parseEither parameters arguments of
  Left err -> reject (T.pack err)
  Right (target,path,text) -> do
    active<-readIORef (sessions t)
    counts<-forM (M.elems active) $ \session -> case session of
      Left _ -> pure 0
      Right (Session _ pending) -> length . filter isTool . M.elems <$> readIORef pending
    preparation<-readIORef (preparing t)
    if sum counts+(case preparation of Just ToolPreparing{} -> 1; _ -> 0)>=32 then reject "Too many pending HLS tools"
    else if name `elem` ["lsp_rename","lsp_code_actions"] && maybe False (const True) preparation then reject "An HLS edit snapshot is already being prepared"
    else if name=="lsp_apply_code_action" then applyCodeAction human t d target path arguments
    else do
      sync t d
      available<-sessionFor t path
      case available of
        Left err -> reject err
        Right session -> do
          promise<-newEmptyTMVarIO
          now<-toInteger <$> getMonotonicTimeNSec
          snapshot<-openSnapshot d path
          progress<-newTVarIO (0,M.empty)
          let query=ToolQuery name target path arguments snapshot (now+30000000000) promise human progress
          if name `elem` ["lsp_rename","lsp_code_actions"] then do
            root<-projectRoot path
            result<-newEmptyMVar
            worker<-forkIO (try (sourceSnapshot d root) >>= putMVar result)
            writeIORef (preparing t) (Just (ToolPreparing query worker result))
          else queueTool t session query text
          pure (d,waitTool query)
  where
    reject err=pure (if human then d {status=err} else d,pure (Left err))
    isTool (ToolPending _) = True
    isTool CommandPending{} = True
    isTool _ = False
    parameters=withObject "HLS tool arguments" $ \o -> do
      unless (name `elem` toolingToolNames) (fail "Unknown HLS tool")
      bid<-o .: "bufferId"
      when (protectedBuffer d bid) (fail "This buffer is private to the user.")
      doc<-maybe (fail "Unknown bufferId") pure (M.lookup bid (buffers d))
      let b=documentBuffer doc; text=contents b
      unless (textBuffer b && documentLabel doc==Nothing) (fail "HLS requires a source text buffer")
      file<-maybe (fail "Save this buffer with a Haskell filename first") pure (documentFile doc)
      unless (takeExtension (filePath file) `elem` [".hs",".lhs"]) (fail "HLS requires a Haskell source file")
      expected<-if name `elem` ["lsp_rename","lsp_code_actions","lsp_apply_code_action"] then Just <$> o .: "revision" else o .:? "revision"
      unless (maybe True (==revision b) expected) (fail "Buffer revision changed")
      pos<-if name `elem` ["lsp_document_symbols","lsp_apply_code_action"] then pure 0 else do
        row<-o .: "line"; col<-o .: "column"
        unless (row>=1 && row<=bufferLineCount b) (fail "Line is outside the buffer")
        let line=T.dropWhileEnd (=='\r') (bufferLineAt b (row-1))
        unless (col>=1 && col<=T.length line+1) (fail "Column is outside the line")
        pure (bufferLineOffset b (row-1)+col-1)
      when (name=="lsp_code_actions") $ do
        endRow<-o .:? "endLine"; endCol<-o .:? "endColumn"
        case (endRow,endCol) of
          (Nothing,Nothing)->pure ()
          (Just row,Just col)->do
            unless (row>=1 && row<=bufferLineCount b && col>=1 && col<=T.length (T.dropWhileEnd (=='\r') (bufferLineAt b (row-1)))+1) (fail "Invalid code action range")
            unless (bufferLineOffset b (row-1)+col-1>=pos) (fail "Code action range ends before its start")
          _->fail "Supply both endLine and endColumn"
      when (name=="lsp_apply_code_action") $ do
        void (o .: "actionId" :: Parser T.Text)
        unless (all (`elem` ["bufferId","revision","actionId"]) (K.keys o)) (fail "Apply accepts only a returned actionId and current buffer revision")
      when (name=="lsp_rename") $ do
        newName<-o .: "newName"
        unless (not (T.null newName) && T.length newName<=256 && not (T.any (\c -> isSpace c || c<' ') newName)) (fail "Invalid rename identifier")
      when (name=="lsp_references") (void (o .:? "includeDeclaration" :: Parser (Maybe Bool)))
      pure ((bid,revision b,pos),filePath file,text)

completeTool :: ToolQuery -> Either T.Text Value -> IO ()
completeTool query result = atomically $ do
  progress@(count,_)<-readTVar (queryProgress query)
  let answer=case result of Left err | count>0 -> Right (commandResult query progress False (Just err)); _->result
  void (tryPutTMVar (queryReply query) answer)

commandResult :: ToolQuery -> (Int,M.Map Int Int) -> Bool -> Maybe T.Text -> Value
commandResult query (count,changed) success failure = object
  ["bufferId" .= (let (bid,_,_)=queryTarget query in bid),"applied" .= (count>0),
   "appliedBatches" .= count,"commandSucceeded" .= success,"succeeded" .= (success && failure==Nothing),"partial" .= (count>0 && (not success || failure/=Nothing)),
   "error" .= failure,"buffers" .= [object ["bufferId" .= bid,"revision" .= version] | (bid,version)<-M.toList changed]]

waitTool :: ToolQuery -> IO (Either T.Text Value)
waitTool query = (do
  now<-toInteger <$> getMonotonicTimeNSec
  result<-timeout (fromInteger (max 1 ((queryDeadline query-now) `div` 1000))) (atomically (readTMVar (queryReply query)))
  case result of
    Just answer -> pure answer
    Nothing -> completeTool query (Left "HLS request timed out") >> atomically (readTMVar (queryReply query)))
  `onException` completeTool query (Left "HLS request cancelled")

failPending :: T.Text -> Pending -> IO ()
failPending err (ToolPending query)=completeTool query (Left err)
failPending err (CommandPending query _)=completeTool query (Left err)
failPending _ _=pure ()

toolActive :: ToolQuery -> IO Bool
toolActive query = do
  now<-toInteger <$> getMonotonicTimeNSec
  when (now>=queryDeadline query) (completeTool query (Left "HLS request timed out"))
  atomically (isEmptyTMVar (queryReply query))

queueTool :: Tooling -> Session -> ToolQuery -> T.Text -> IO ()
queueTool t (Session client pending) query text = do
  let name=queryName query
      (_,_,pos)=queryTarget query
      method=fromMaybe "textDocument/hover" (lookup name [("lsp_definition","textDocument/definition"),("lsp_type_definition","textDocument/typeDefinition"),("lsp_references","textDocument/references"),("lsp_document_symbols","textDocument/documentSymbol"),("lsp_rename","textDocument/rename")])
      fields=["textDocument" .= object ["uri" .= L.fileUri (queryPath query)]]++
        ["position" .= L.positionValue text pos | name/="lsp_document_symbols"]++
        ["newName" .= fromMaybe Null (member "newName" (queryArguments query)) | name=="lsp_rename"]++
        ["context" .= object ["includeDeclaration" .= fromMaybe True (parseMaybe (withObject "references" (\o -> o .:? "includeDeclaration" .!= True)) (queryArguments query))] | name=="lsp_references"]
  diagnostics<-readIORef (problems t)
  let start=L.positionValue text pos
      endOffset=case (member "endLine" (queryArguments query),member "endColumn" (queryArguments query)) of
        (Just row,Just col) | Just r<-parseMaybe parseJSON row,Just c<-parseMaybe parseJSON col ->
          bufferLineOffset (newBuffer text) (r-1)+c-1
        _->pos
      end=L.positionValue text endOffset
      intersects value=case parseMaybe (withObject "diagnostic" (\o->o .: "range" >>= rangeOffsets text)) value of
        Just (a,z)->a<=endOffset && z>=pos
        _->False
      current=case M.lookup (queryPath query) diagnostics of
        Just (version,values) | diagnosticsCurrent version [let (_,v,_)=queryTarget query in v] -> take 256 (filter intersects values)
        _->[]
      (operation,params)=case name of
        "lsp_code_actions" -> ("textDocument/codeAction",object ["textDocument" .= object ["uri" .= L.fileUri (queryPath query)],"range" .= object ["start" .= start,"end" .= end],"context" .= object ["diagnostics" .= current]])
        "lsp_apply_code_action" -> ("codeAction/resolve",queryArguments query)
        _->(method,object fields)
  ident<-L.request client operation params
  modifyIORef' pending (M.insert ident (ToolPending query))

-- Open buffers from other projects and private authority files cannot enter an
-- edit snapshot, even when they are visible in the same desktop.
openSnapshot :: Desktop -> FilePath -> IO (M.Map FilePath (Int,T.Text))
openSnapshot d path = do
  root<-projectRoot path
  entries<-forM (sourceDocuments d) $ \(_,file,version,text)->do
    owner<-projectRoot file
    pure [(file,(version,text)) | owner==root]
  pure (M.fromList (concat entries))

advertisedCommands :: Value -> [T.Text]
advertisedCommands caps = fromMaybe [] (member "executeCommandProvider" caps >>= member "commands" >>= parseMaybe parseJSON)

-- Keep both LSP action shapes opaque to callers. Arguments only come from the
-- offered action (or its advertised resolver), never from tool input.
actionCommand :: Value -> Either T.Text (Maybe (T.Text,Value))
actionCommand value = case member "command" value of
  Nothing -> Right Nothing
  Just Null -> Right Nothing
  Just (String name) -> command name value
  Just commandValue@(Object _) -> case member "command" commandValue >>= stringValue of
    Just name -> command name commandValue
    Nothing -> Left "Invalid HLS command."
  _ -> Left "Invalid HLS command."
  where
    command name objectValue
      | T.null name || T.length name>1024 = Left "Invalid HLS command name."
      | otherwise = case fromMaybe (toJSON ([]::[Value])) (member "arguments" objectValue) of
          args@(Array _) -> Right (Just (name,args))
          _ -> Left "Invalid HLS command arguments."

actionDisabled :: Bool -> [T.Text] -> Value -> Maybe T.Text
actionDisabled canResolve commands value
  | Just disabled<-member "disabled" value,disabled/=Null = Just (T.take 512 (fromMaybe "Disabled by HLS" (member "reason" disabled >>= stringValue)))
  | Left err<-actionCommand value = Just err
  | Right (Just (command,_))<-actionCommand value,command `notElem` commands = Just "Command is not advertised by this HLS server."
  | Just edit<-member "edit" value,edit/=Null = either Just (const Nothing) (workspaceEdits edit)
  | Right (Just _)<-actionCommand value = Nothing
  | canResolve = Nothing
  | otherwise = Just "No text edit, command or advertised code-action resolver is available."

cacheCodeActions :: Tooling -> Session -> ToolQuery -> Value -> Desktop -> IO Desktop
cacheCodeActions t (Session client _) query result d = do
  caps<-L.serverCapabilities client
  let resolve=(member "codeActionProvider" caps >>= member "resolveProvider")==Just (Bool True)
      offered=case result of Array values->Vector.toList values; _->[]
      choices=take 128 [(value,T.take 512 title) | value<-offered,Just title<-[member "title" value >>= stringValue],not (T.null title)]
      (bid,version,_)=queryTarget query
  cached<-forM choices $ \(value,title)->do
    ident<-atomicModifyIORef' (nextAction t) (\n->(n+1,"action-"<>T.pack (show n)))
    let disabled=actionDisabled resolve (advertisedCommands caps) value
        description=object ["actionId" .= ident,"title" .= title,"kind" .= (T.take 128 <$> (member "kind" value >>= stringValue)),
          "preferred" .= (member "isPreferred" value==Just (Bool True)),"disabledReason" .= disabled]
        label=title<>maybe "" (" — "<>) disabled
    pure (ident,CachedAction query value resolve,description,label)
  active<-toolActive query
  accepted<-if active then atomically (tryPutTMVar (queryReply query) (Right (object
    ["bufferId" .= bid,"revision" .= version,"actions" .= [value | (_,_,value,_)<-cached],"truncated" .= (length offered>128)]))) else pure False
  if not accepted then pure d else do
    writeIORef (actions t) (M.fromList [(ident,entry) | (ident,entry,_,_)<-cached])
    pure $ if not (queryHuman query) then d else if null cached then d {status="No code actions available."}
      else prompt "HLS code actions" (CodeActionChoices bid version [ident | (ident,_,_,_)<-cached])
        [ListBox "Action" [label | (_,_,_,label)<-cached] 0] d

applyCodeAction :: Bool -> Tooling -> Desktop -> Target -> FilePath -> Value -> IO (Desktop,IO (Either T.Text Value))
applyCodeAction human t d target path arguments = do
  let ident=fromMaybe "" (member "actionId" arguments >>= stringValue)
      reject err=pure (if human then d {status=err} else d,pure (Left err))
  cached<-atomicModifyIORef' (actions t) (\entries->(M.delete ident entries,M.lookup ident entries))
  case cached of
    Nothing->reject "Code action expired or already used; list actions again."
    Just (CachedAction original value resolve)
      | let (bid,version,_)=target; (oldBid,oldVersion,_)=queryTarget original,
        bid/=oldBid || version/=oldVersion || path/=queryPath original -> reject "Code action source changed; list actions again."
      | otherwise->do
          available<-sessionFor t path
          case available of
            Left err->reject err
            Right session@(Session client _) -> do
              caps<-L.serverCapabilities client
              case actionDisabled resolve (advertisedCommands caps) value of
                Just reason->reject reason
                Nothing->do
                  now<-toInteger <$> getMonotonicTimeNSec
                  reply<-newEmptyTMVarIO
                  progress<-newTVarIO (0,M.empty)
                  let query=original {queryName="lsp_apply_code_action",queryArguments=value,queryDeadline=now+30000000000,queryReply=reply,queryHuman=human,queryProgress=progress}
                  if maybe False (/=Null) (member "edit" value) || either (const False) (/=Nothing) (actionCommand value)
                    then do
                      updated<-finishCodeAction t session query value d
                      pure (updated,waitTool query)
                    else do
                      queueTool t session query ""
                      pure (d {status=if human then "Resolving code action..." else status d},waitTool query)

finishCodeAction :: Tooling -> Session -> ToolQuery -> Value -> Desktop -> IO Desktop
finishCodeAction t (Session client pending) query value d = do
  caps<-L.serverCapabilities client
  case actionDisabled False (advertisedCommands caps) value of
    Just err->completeTool query (Left err) >> pure (if queryHuman query then d {status=err} else d)
    Nothing -> case actionCommand value of
      Right (Just (command,arguments))->do
        running<-any isCommand . M.elems <$> readIORef pending
        if running then completeTool query (Left "An HLS command is already running.") >> pure d else do
          prepared<-case member "edit" value of
            Just edit | edit/=Null -> applyCommandEdit query edit d
            _ -> pure (Right (query,d))
          case prepared of
            Left err->completeTool query (Left err) >> pure d
            Right (current,updated)->do
              active<-toolActive current
              if not active then pure updated else do
                sync t updated
                requested<-L.executeCommand client command arguments
                case requested of
                  Left err->completeTool current (Left err) >> pure updated {status=err}
                  Right ident->do
                    modifyIORef' pending (M.insert ident (CommandPending current Nothing))
                    pure updated {status="Executing HLS code action..."}
      _ -> commitToolEdit query (fromMaybe Null (member "edit" value)) d
  where isCommand CommandPending{}=True; isCommand _=False

-- Each server batch commits as one checked edit; successful earlier batches are
-- retained if a later batch or the command fails. Cancellation and this commit
-- compete in STM, so a canceled waiter cannot authorize a later mutation.
applyCommandEdit :: ToolQuery -> Value -> Desktop -> IO (Either T.Text (ToolQuery,Desktop))
applyCommandEdit query edit d = do
  progress<-readTVarIO (queryProgress query)
  if protectedPath d (queryPath query) || protectedBuffer d (let (bid,_,_)=queryTarget query in bid) then pure (Left "HLS command source is private.")
  else if fmap fst (targetDocument (queryTarget query) d)/=Just (queryPath query) then pure (Left "Code action source changed during execution.")
  else if fst progress>=128 then pure (Left "HLS command exceeded 128 edit batches.") else do
    changed<-renameBuffers (querySnapshot query) edit d
    case changed of
      Left err->pure (Left err)
      Right updated->do
        let edits=[(ident,doc) | (ident,doc)<-M.toList (buffers updated),M.lookup ident (buffers d)/=Just doc]
            replacements=M.fromList [(filePath file,(revision (documentBuffer doc),contents (documentBuffer doc))) | (_,doc)<-edits,Just file<-[documentFile doc]]
            (bid,version,pos)=queryTarget query
            current=query {querySnapshot=M.union replacements (querySnapshot query),queryTarget=(bid,maybe version (revision.documentBuffer) (M.lookup bid (buffers updated)),pos)}
        active<-toolActive query
        accepted<-if not active then pure False else atomically $ do
          cancelled<-not <$> isEmptyTMVar (queryReply query)
          if cancelled then pure False else do
            modifyTVar' (queryProgress query) (\(count,previous)->(count+1,M.union (M.fromList [(ident,revision (documentBuffer doc)) | (ident,doc)<-edits]) previous))
            pure True
        pure (if accepted then Right (current,updated) else Left "HLS command cancelled before applying this edit.")

commitToolEdit :: ToolQuery -> Value -> Desktop -> IO Desktop
commitToolEdit query edit d = do
  changed<-renameBuffers (querySnapshot query) edit d
  case changed of
    Left err->completeTool query (Left err) >> pure (if queryHuman query then d {status=err} else d)
    Right updated->do
      let (bid,_,_)=queryTarget query
          edited=[object ["bufferId" .= ident,"revision" .= revision (documentBuffer doc)] | (ident,doc)<-M.toList (buffers updated),M.lookup ident (buffers d)/=Just doc]
      active<-toolActive query
      accepted<-if active then atomically (tryPutTMVar (queryReply query) (Right (object ["bufferId" .= bid,"applied" .= True,"buffers" .= edited]))) else pure False
      pure $ if not accepted then d else if queryName query=="lsp_apply_code_action"
        then updated {status="Code action applied to buffers. Review and save the changed files."} else updated

sourceDocuments :: Desktop -> [(Int,FilePath,Int,T.Text)]
sourceDocuments d = [(bid,filePath f,revision b,contents b) | (bid,doc)<-M.toList (buffers d), documentLabel doc==Nothing, textBuffer (documentBuffer doc),
  not (protectedBuffer d bid), Just f<-[documentFile doc], takeExtension (filePath f) `elem` [".hs",".lhs"], let b=documentBuffer doc]

projectRoot :: FilePath -> IO FilePath
projectRoot path = search (takeDirectory path)
  where
    fallback=takeDirectory path
    search dir = do
      entries <- either (const []) id <$> (try (listDirectory dir) :: IO (Either IOException [FilePath]))
      if projectBoundary entries
        then pure dir else if takeDirectory dir==dir then pure fallback else search (takeDirectory dir)

projectBoundary :: [FilePath] -> Bool
projectBoundary entries = any (`elem` entries) ["hie.yaml","cabal.project","stack.yaml",".git"] || any ((==".cabal") . takeExtension) entries

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
  docs<-forM (sourceDocuments d) $ \(bid,path,version,text) -> do
    root<-rootFor t path
    _<-sessionFor t path
    identity<-makeStableName =<< evaluate (documentBuffer (buffers d M.! bid))
    pure (root,[((bid,path,version,identity),(path,version,text))])
  active<-readIORef (sessions t)
  previous<-readIORef (synchronized t)
  let grouped=M.fromListWith (++) docs
  current<-forM (M.toList active) $ \(root,session) -> case session of
    Right (Session client _) -> do
      identity<-makeStableName =<< evaluate client
      let entries=M.findWithDefault [] root grouped
          key=(identity,map fst entries)
      -- Buffer identity catches equal-revision reloads without flattening text.
      -- Client identity forces didOpen after a restarted language server.
      when (M.lookup root previous/=Just key) (L.syncDocuments client (map snd entries))
      pure (Just (root,key))
    Left _ -> pure Nothing
  writeIORef (synchronized t) (M.fromList (catMaybes current))

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
          root<-projectRoot path
          result<-newEmptyMVar
          snapshot<-openSnapshot d path
          worker<-forkIO (try (sourceSnapshot d root) >>= putMVar result)
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
  previous<-readIORef (preparing t)
  updated<-finishPreparationResult t d
  case previous of
    Just (ToolPreparing query _ _) | queryHuman query ->do
      result<-atomically (tryReadTMVar (queryReply query))
      pure $ case result of Just (Left err)->updated {status=err}; _->updated
    _->pure updated

finishPreparationResult :: Tooling -> Desktop -> IO Desktop
finishPreparationResult t d = do
  pending<-readIORef (preparing t)
  case pending of
    Nothing -> pure d
    Just (ToolPreparing query worker result) -> do
      active<-toolActive query
      if not active || fmap fst (targetDocument (queryTarget query) d)/=Just (queryPath query) then do
        completeTool query (Left "Buffer changed while preparing rename")
        killThread worker
        writeIORef (preparing t) Nothing
        pure d
      else do
        completed<-tryReadMVar result
        case completed of
          Nothing -> pure d
          Just answer -> do
            writeIORef (preparing t) Nothing
            case answer of
              Left err -> completeTool query (Left ("Cannot prepare rename: "<>T.pack (show err)))
              Right disk -> do
                available<-sessionFor t (queryPath query)
                case (available,targetDocument (queryTarget query) d) of
                  (Right session,Just (_,text)) -> queueTool t session query {querySnapshot=M.union (querySnapshot query) disk} text
                  (Left err,_) -> completeTool query (Left err)
                  _ -> completeTool query (Left "Rename target changed")
            pure d
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

-- applyEdit has no originating executeCommand ID. A client that executed one
-- command is never allowed to own another, including after successful replies.
-- Cleanup is asynchronous, but remains owned by closeTooling.
retireSession :: Tooling -> FilePath -> Session -> IO ()
retireSession t root (Session client pending) = mask_ $ do
  -- Register cleanup before removing the old session's ownership.
  stopped<-L.retireClient client
  modifyIORef' (retiring t) (stopped:)
  readIORef pending >>= mapM_ (failPending "HLS command transport retired; retry on the fresh server.") . M.elems
  writeIORef pending M.empty
  writeIORef (actions t) M.empty
  modifyIORef' (sessions t) (M.delete root)

-- Idle hover is debounced; no request is issued for every mouse pixel or keystroke.
tickTooling :: Tooling -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO Desktop
tickTooling t core d = do
  retired<-readIORef (retiring t)
  remaining<-filterM (fmap (==Nothing) . tryReadMVar) retired
  writeIORef (retiring t) remaining
  sync t d
  prepared<-finishPreparation t d
  active<-readIORef (sessions t)
  received<-foldM collect prepared (M.toList active)
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
    collect desktop (_,Left _) = pure desktop
    collect desktop (root,Right session@(Session client pending)) = do
      requests<-readIORef pending
      cancelled<-or <$> forM (M.elems requests) (\entry->case entry of CommandPending query _->not <$> toolActive query; _->pure False)
      if cancelled then do
        retireSession t root session
        applied<-or <$> forM (M.elems requests) (\entry->case entry of CommandPending query _->(>0) . fst <$> readTVarIO (queryProgress query); _->pure False)
        pure desktop {status=if applied then "HLS command cancelled. Earlier applied edits remain; review buffers." else "HLS command cancelled; server restarted."}
      else do
        (live,updated)<-foldM (\(kept,view) (ident,entry) -> do
          activeRequest<-case entry of ToolPending query -> toolActive query; _ -> pure True
          if activeRequest then pure (M.insert ident entry kept,view) else do
            result<-case entry of ToolPending query | queryHuman query -> atomically (tryReadTMVar (queryReply query)); _ -> pure Nothing
            pure (kept,case result of Just (Left err)->view {status=err}; _->view)) (M.empty,desktop) (M.toList requests)
        writeIORef pending live
        events<-L.pollEvents client
        foldM (receive session) updated events
    receive (Session client pending) desktop (L.ApplyEdit execution ident params) = do
      requests<-readIORef pending
      case M.lookup execution requests of
        Just (CommandPending query failure)->do
          activeRequest<-toolActive query
          answer<-if not activeRequest then pure (Left "HLS command cancelled.") else case failure of
            Just err->pure (Left err)
            Nothing->case member "edit" params of
              Nothing->pure (Left "HLS supplied no workspace edit.")
              Just edit->applyCommandEdit query edit desktop
          case answer of
            Left err->do
              L.replyEdit client ident False (Just err)
              modifyIORef' pending (M.insert execution (CommandPending query (Just err)))
              pure desktop
            Right (current,updated)->do
              sync t updated
              L.replyEdit client ident True Nothing
              modifyIORef' pending (M.insert execution (CommandPending current Nothing))
              pure updated
        _->L.replyEdit client ident False (Just "No active editor command owns this edit.") >> pure desktop
    receive session@(Session _ pending) desktop (L.ServerError err) = do
      requests<-readIORef pending
      mapM_ (failPending err) (M.elems requests)
      case [query | CommandPending query _<-M.elems requests] of
        query:_->rootFor t (queryPath query) >>= \root->retireSession t root session
        _->writeIORef pending M.empty
      pure desktop {status="HLS: "<>singleLine err,typeHint=""}
    receive _ desktop (L.Diagnostics path version values) = do
      let diagnostics=case values of Array xs -> Vector.toList xs; _ -> []
      modifyIORef' (problems t) (M.insert path (version,diagnostics))
      pure desktop
    receive session@(Session _ pending) desktop (L.Response ident response) = do
      requests<-readIORef pending
      modifyIORef' pending (M.delete ident)
      case M.lookup ident requests of
        Nothing -> pure desktop
        Just (CommandPending query failure) -> do
          activeRequest<-toolActive query
          let succeeded=activeRequest && member "error" response==Nothing && member "result" response/=Nothing
              err=if not activeRequest then Just "HLS command cancelled." else case member "error" response of
                Just detail->Just (T.take 512 (fromMaybe "HLS command failed" (member "message" detail >>= stringValue)))
                Nothing | member "result" response==Nothing -> Just "HLS returned no command result."
                        | otherwise -> failure
          progress<-readTVarIO (queryProgress query)
          completeTool query (Right (commandResult query progress succeeded err))
          rootFor t (queryPath query) >>= \root->retireSession t root session
          pure desktop {status=if err==Nothing then "HLS command completed. Review and save changed buffers." else "HLS command failed. Earlier applied edits remain; review buffers."}
        Just (ToolPending query) -> finishTool t session query response desktop
        Just (Pending action target path snapshot)
          | fmap fst (targetDocument target desktop)/=Just path -> pure desktop
          | action==TypeInfo && currentTarget desktop/=Just target -> pure desktop
          | action/=TypeInfo && (cursorTarget desktop/=Just target || dialog desktop/=Nothing) -> pure desktop
          | Just err<-member "error" response -> pure desktop {status="HLS: "<>singleLine (fromMaybe "Request failed" (member "message" err >>= stringValue))}
          | otherwise -> case member "result" response of
              Nothing -> pure desktop
              Just result -> applyResult core action target path snapshot result desktop

finishTool :: Tooling -> Session -> ToolQuery -> Value -> Desktop -> IO Desktop
finishTool t session query response d = do
  updated<-finishToolResult t session query response d
  result<-atomically (tryReadTMVar (queryReply query))
  pure $ case result of Just (Left err) | queryHuman query -> updated {status=err}; _ -> updated

finishToolResult :: Tooling -> Session -> ToolQuery -> Value -> Desktop -> IO Desktop
finishToolResult t session query response d = do
  active<-toolActive query
  if not active then pure d else
    if queryHuman query && queryName query=="lsp_code_actions" && (dialog d/=Nothing || fmap bufferId (activeWindow d)/=Just (let (bid,_,_)=queryTarget query in bid)) then completeTool query (Left "Code action selection changed") >> pure d else
    if fmap fst (targetDocument (queryTarget query) d)/=Just (queryPath query) then completeTool query (Left "Buffer changed while awaiting HLS") >> pure d
    else case member "error" response of
      Just err -> completeTool query (Left (fromMaybe "HLS request failed" (member "message" err >>= stringValue))) >> pure d
      Nothing -> case member "result" response of
        Nothing -> completeTool query (Left "HLS returned no result") >> pure d
        Just result -> do
          let (bid,version,_)=queryTarget query
          if queryName query=="lsp_code_actions" then cacheCodeActions t session query result d
          else if queryName query=="lsp_apply_code_action" then finishCodeAction t session query result d
          else if queryName query=="lsp_rename" then commitToolEdit query result d
          else do
            completeTool query (Right (object (["bufferId" .= bid,"revision" .= version,"result" .= result]++["text" .= hoverText result | queryName query=="lsp_hover"])))
            pure d

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
applyRename snapshot result d = either (\err -> message "Cannot rename" [err] d) id <$> renameBuffers snapshot result d

renameBuffers :: M.Map FilePath (Int,T.Text) -> Value -> Desktop -> IO (Either T.Text Desktop)
renameBuffers snapshot result d = case workspaceEdits result of
  Left err -> pure (Left err)
  Right [] -> pure (Right d {status="No rename edits returned."})
  Right changes -> do
    prepared<-forM changes $ \(path,version,values) -> do
      let opened=find (\(_,doc) -> fmap filePath (documentFile doc)==Just path) (M.toList (buffers d))
      canonical<-either (const Nothing) Just <$> (try (canonicalizePath path) :: IO (Either IOException FilePath))
      loaded<-if canonical==Nothing then pure (Left "Cannot inspect the HLS edit path.")
        else if protectedPath d path || maybe False (protectedPath d) canonical || maybe False (protectedBuffer d . fst) opened then pure (Left "HLS edit refers to a private file.")
        else if canonical/=Just path || M.notMember path snapshot then pure (Left "HLS edit refers to a file outside the checked project.")
        else case opened of
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
      Left err -> pure (Left err)
      Right edits -> pure $ Right (foldl apply d edits) {status="Rename applied to buffers. Review and save the changed files."}
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
    apply (_,desktop) (LanguageRequest RequestCodeActions) = case (activeWindow desktop,activeDocument desktop) of
      (Just window,Just doc)->do
        let b=documentBuffer doc
            Selection anchor cursor=selection window
            coordinate offset=let prefix=T.take offset (contents b) in (1+T.count "\n" prefix,1+T.length (T.takeWhileEnd (/='\n') prefix))
            (row,col)=coordinate (min anchor cursor)
            (endRow,endCol)=coordinate (max anchor cursor)
        (updated,_)<-startTool True t core desktop "lsp_code_actions" (object
          ["bufferId" .= bufferId window,"revision" .= revision b,"line" .= row,"column" .= col,"endLine" .= endRow,"endColumn" .= endCol])
        pure (False,updated)
      _->pure (False,desktop {status="Open a saved Haskell source file first."})
    apply (_,desktop) (LanguageRequest (ApplyCodeAction bid version ident)) = do
      (updated,_)<-startTool True t core desktop "lsp_apply_code_action" (object ["bufferId" .= bid,"revision" .= version,"actionId" .= ident])
      pure (False,updated)
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
sourceSnapshot :: Desktop -> FilePath -> IO (M.Map FilePath (Int,T.Text))
sourceSnapshot d root = walk root
  where
    walk directory = do
      entries<-listDirectory directory
      if directory/=root && projectBoundary entries then pure M.empty else
        M.unions <$> forM (filter (\name -> name `notElem` ["dist-newstyle","dist",".stack-work"] && not ("." `T.isPrefixOf` T.pack name)) entries) (\name -> do
          let path=directory </> name
          linked<-pathIsSymbolicLink path
          childDirectory<-doesDirectoryExist path
          if linked || protectedPath d path then pure M.empty else if childDirectory then walk path
          else if takeExtension path `elem` [".hs",".lhs"] then do
            owner<-projectRoot path
            if owner/=root then pure M.empty else do
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
      updated=chooseProblem (problemsSelected d) (d {diagnostics=sortOn (\p -> (diagnosticPath p,diagnosticRow p,diagnosticColumn p)) (shown++buildDiagnostics d)})
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
