{-# LANGUAGE OverloadedStrings #-}
module Hide.MCPPermissions
  ( Permissions, withPermissions, withPermissionsAt, permissionCall, policyEffects, tickPermissions
  , permissionConfigPath, readEditorDefaults, writeEditorDefaults, readEditorDefaultsAt, writeEditorDefaultsAt
  , projectConfigPath, readEditorDefaultsFor, readAgentContextAt, writeAgentContextAt, readAgentContexts
  , readEnvironmentAt, writeEnvironmentAt
  , readAgentLimitsFor, updateConfigTable, readAutocompleteFor, writeAutocomplete, writeAutocompleteFor
  ) where

import Control.Concurrent (MVar, newEmptyMVar, newMVar, readMVar, tryPutMVar, withMVar)
import Control.Exception (IOException, bracket, onException, try)
import Control.Monad (foldM, unless, when)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.IORef
import Data.List (find, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, createDirectoryIfMissing, getHomeDirectory, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), isAbsolute, takeDirectory, takeExtension)
import System.IO (IOMode(ReadMode), withBinaryFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import qualified Toml
import qualified Toml.Syntax as TS
import Hide.Buffer (newBuffer, Selection(..))
import Hide.WorkspaceFilesMCP (applyPatch)
import Hide.Files (FileState(..), saveFile)
import Hide.Model

type Tool = Desktop -> Text -> Value -> IO (Desktop,IO (Either Text Value))
type Core = Desktop -> [Effect] -> IO (Bool,Desktop)
data Mode = Enable | Prompt | Disable deriving (Eq,Show)
data Waiting = Waiting
  { ticket :: Int, toolName :: Text, arguments :: Value, execute :: Tool
  , reply :: MVar (IO (Either Text Value)), active :: IORef Bool }
data PermissionState = PermissionState { waiting :: [Waiting], nextTicket :: Int, displayed :: Maybe Text }
data Permissions = Permissions FilePath (M.Map Text Bool) (IORef PermissionState)

permissionConfigPath :: IO FilePath
permissionConfigPath=do
  configured<-lookupEnv "XDG_CONFIG_HOME"
  base<-case configured of
    Just path | not (null path),isAbsolute path -> pure path
              | not (null path) -> ioError (userError "XDG_CONFIG_HOME must be absolute")
    _ -> (</> ".config") <$> getHomeDirectory
  canonicalizePath (base </> "thc" </> "config.toml")

withPermissions :: [Value] -> (Permissions -> IO a) -> IO a
withPermissions specs action=permissionConfigPath >>= \path -> withPermissionsAt path specs action

withPermissionsAt :: FilePath -> [Value] -> (Permissions -> IO a) -> IO a
withPermissionsAt path specs=bracket acquire release
  where
    acquire=Permissions path registry <$> newIORef (PermissionState [] 1 Nothing)
    registry=M.fromList [(name,fromMaybe False (field "annotations" spec >>= field "readOnlyHint")) | spec<-specs,Just name<-[field "name" spec]]
    release (Permissions _ _ ref)=readIORef ref >>= mapM_ (\request->finish request (Left "Editor session closed before approval")) . waiting

-- All initial calls and UI effects run under the session desktop lock. The
-- returned continuation is the only wait, and must run outside that lock.
permissionCall :: Permissions -> Tool -> Tool
permissionCall runtime@(Permissions path registry ref) callback desktop name args=do
  loaded<-readPolicies path
  case M.lookup name registry of
    Nothing -> denied "Unknown MCP tool"
    Just readonly -> case loaded of
      Left err -> denied err
      Right policies -> case M.findWithDefault (if readonly then Enable else Prompt) name policies of
        Disable -> denied "This MCP tool is disabled in Options > Agent Permissions"
        Enable -> callback desktop name args
        Prompt -> do
          s<-readIORef ref
          live<-filterMActive (waiting s)
          if length live>=32 then denied "Too many MCP requests are awaiting permission" else do
            promise<-newEmptyMVar
            enabled<-newIORef True
            let request=Waiting (nextTicket s) name args callback promise enabled
            writeIORef ref s {waiting=live++[request],nextTicket=nextTicket s+1}
            shown<-tickPermissions runtime desktop
            pure (shown,(readMVar promise >>= id) `onException` finish request (Left "MCP permission request cancelled"))
  where denied reason=pure (desktop,pure (Left reason))

filterMActive :: [Waiting] -> IO [Waiting]
filterMActive requests=fmap (map fst . filter snd) (mapM (\request->(request,) <$> readIORef (active request)) requests)

finish :: Waiting -> Either Text Value -> IO ()
finish request result=do
  atomicModifyIORef' (active request) (const (False,()))
  _<-tryPutMVar (reply request) (pure result)
  pure ()

-- Display the oldest pending request when existing editor dialogs have closed.
-- A replaced modal is redisplayed, and cancellation removes actionable prompts.
tickPermissions :: Permissions -> Desktop -> IO Desktop
tickPermissions (Permissions _ _ ref) desktop=do
  s<-readIORef ref
  live<-filterMActive (waiting s)
  let staleApproval=case dialog desktop of
        Just dg | PermissionDialog action<-purpose dg,"approve:" `T.isPrefixOf` action -> all ((/=action).approvalAction) live
        _ -> False
      cleared=if staleApproval then desktop {dialog=Nothing} else desktop
  writeIORef ref s {waiting=live,displayed=if staleApproval then Nothing else displayed s}
  case (dialog cleared,live) of
    (Nothing,request:_) -> do
      modifyIORef' ref (\state->state {displayed=Just (approvalAction request)})
      pure cleared {dialog=Just (approvalReview cleared request)}
    _ -> pure cleared

-- doc-artifact: tools/docs-screenshots.hs permission-diff -> docs/site/screenshots/permission-diff.png
approvalReview :: Desktop -> Waiting -> Dialog
approvalReview desktop request=Dialog "Agent permission" (PermissionDialog (approvalAction request)) reviewFields selected ["Allow once","Deny"] []
  where
    args=arguments request
    patch=if toolName request=="buffer_apply_diff" then field "diff" args else Nothing
    metadata=[ReadOnly "Tool" (toolName request)]++case patch of
      Just _ -> [ReadOnly "File" (fromMaybe "Unknown buffer" $ do
        bid<-field "bufferId" args
        doc<-M.lookup bid (buffers desktop)
        pure (maybe ("Untitled #"<>T.pack (show bid)) (T.pack.filePath) (documentFile doc)))]
      Nothing -> []
    members=case args of Object entries -> sortOn fst [(K.toText key,value) | (key,value)<-KM.toList entries]; _ -> [("Arguments",args)]
    rows=[view name value | (name,value)<-members,not (name=="diff" && patch/=Nothing)]
    reviewFields=metadata++rows++[TextArea "diff" True (newBuffer text) (Selection 0 0) 0 0 | Just text<-[patch]]
    selected=if patch/=Nothing then length reviewFields-1 else 0
    view name value=let text=case value of String t -> t; _ -> TE.decodeUtf8 (BL.toStrict (encode value))
                    in if T.any (=='\n') text || T.length text>48 then TextArea name False (newBuffer text) (Selection 0 0) 0 0 else ReadOnly name text

approvalAction :: Waiting -> Text
approvalAction request="approve:"<>T.pack (show (ticket request))

policyEffects :: Permissions -> Core -> Core
policyEffects runtime fallback desktop effects=foldM apply (False,desktop) effects
  where
    apply result@(True,_) _=pure result
    apply (_,d) (PermissionAction action values)=(False,) <$> permissionAction runtime action values d
    apply (_,d) effect=fallback d [effect]

permissionAction :: Permissions -> Text -> [Text] -> Desktop -> IO Desktop
permissionAction runtime@(Permissions path registry ref) action values desktop=do
  s<-readIORef ref
  if action=="show" then showSettings runtime desktop else
    if displayed s/=Just action then pure desktop {status="Permission dialog expired."} else
      case action of
        "settings" -> case values of
          "0":index:_ | Just n<-readMaybe (T.unpack index),Just (name,readonly)<-at (M.toList registry) n -> do
            policies<-readPolicies path
            let mode=either (const (if readonly then Enable else Prompt)) (M.findWithDefault (if readonly then Enable else Prompt) name) policies
                token="set:"<>name
            modifyIORef' ref (\state->state {displayed=Just token})
            pure desktop {dialog=Just (Dialog "Agent permission setting" (PermissionDialog token)
              [Radio name ["Enable","Prompt","Disable"] (modeIndex mode)] 0 ["Save","Back"]
              ["Enable runs immediately; Prompt asks once per request; Disable rejects.","Saved globally in thc/config.toml."])}
          _ -> close
        _ | Just name<-T.stripPrefix "set:" action,Just _<-M.lookup name registry -> case values of
          "0":selected:_ | Just mode<-readMaybe (T.unpack selected) >>= at [Enable,Prompt,Disable] -> do
            saved<-writeTable path ["editor","mcp","permissions"] (object [K.fromText name .= modeText mode])
            case saved of
              Left err -> pure desktop {status=err,dialog=Nothing} >>= showSettings runtime
              Right () -> do
                when (mode==Disable) $ readIORef ref >>= mapM_ (\request->when (toolName request==name) (finish request (Left "This MCP tool was disabled before approval"))) . waiting
                showSettings runtime desktop {status="Agent permission saved."}
          _ -> showSettings runtime desktop
        _ | "approve:" `T.isPrefixOf` action -> case find ((==action).approvalAction) (waiting s) of
          Nothing -> close
          Just request -> do
            policies<-readPolicies path
            enabled<-readIORef (active request)
            let edited=case (toolName request,arguments request,drop 1 values) of
                  ("buffer_apply_diff",Object args,text:_) -> Object (KM.insert "diff" (String text) args)
                  _ -> arguments request
                validation=if toolName request=="buffer_apply_diff" && take 1 values==["0"] then do
                  bid<-maybe (Left "Missing bufferId") Right (field "bufferId" edited)
                  version<-maybe (Left "Missing revision") Right (field "revision" edited)
                  patch<-maybe (Left "Missing diff") Right (field "diff" edited)
                  _<-applyPatch desktop bid version patch
                  pure ()
                  else Right ()
                denied=case policies of
                  Left err -> Just err
                  Right modes | M.lookup (toolName request) modes==Just Disable -> Just "This MCP tool is disabled"
                  _ -> Nothing
            case validation of
              Left err | enabled,denied==Nothing -> pure desktop {status="Diff not applied: "<>err,
                dialog=fmap (\dg->dg {body=T.chunksOf (max 1 (width (dialogRect desktop dg)-6)) ("Diff not applied: "<>err)}) (dialog desktop)}
              _ -> do
                claimed<-atomicModifyIORef' (active request) (\live->(False,live))
                modifyIORef' ref (\state->state {waiting=filter ((/=ticket request).ticket) (waiting state),displayed=Nothing})
                if not claimed || take 1 values/=["0"] then finish request (Left "MCP request denied") >> tickPermissions runtime desktop {dialog=Nothing}
                else case denied of
                  Just err -> finish request (Left err) >> tickPermissions runtime desktop {dialog=Nothing}
                  Nothing -> do
                    result<-try (execute request desktop {dialog=Nothing} (toolName request) edited)
                    case result of
                      Left (_::IOException) -> finish request (Left "MCP tool failed after approval") >> tickPermissions runtime desktop {dialog=Nothing}
                      Right (updated,continuation) -> do
                        let report (Right (Object response)) | toolName request=="buffer_apply_diff",Just patch<-(field "diff" edited::Maybe Text) =
                              Right (Object (KM.insert "appliedDiff" (String patch) (KM.insert "userModified" (Bool (edited/=arguments request)) response)))
                            report value=value
                        _<-tryPutMVar (reply request) (report <$> continuation)
                        tickPermissions runtime updated
        _ -> close
  where
    close=modifyIORef' ref (\state->state {displayed=Nothing}) >> tickPermissions runtime desktop {dialog=Nothing}

showSettings :: Permissions -> Desktop -> IO Desktop
showSettings (Permissions path registry ref) desktop=do
  policies<-readPolicies path
  let rows=[name<>"  ["<>modeText (either (const (if readonly then Enable else Prompt)) (M.findWithDefault (if readonly then Enable else Prompt) name) policies)<>"]" | (name,readonly)<-M.toList registry]
      notes=either (:[]) (const ["Select a tool to set Enable, Prompt or Disable.","Policies apply to every request, including cached tool schemas."]) policies
  modifyIORef' ref (\state->state {displayed=Just "settings"})
  pure desktop {dialog=Just (Dialog "Agent Permissions" (PermissionDialog "settings") [ListBox "Tool" rows 0] 0 ["Edit","Close"] notes)}

modeText :: Mode -> Text
modeText Enable="enable"
modeText Prompt="prompt"
modeText Disable="disable"
modeIndex :: Mode -> Int
modeIndex Enable=0
modeIndex Prompt=1
modeIndex Disable=2

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.:key))
at :: [a] -> Int -> Maybe a
at rows n | n<0=Nothing | otherwise=case drop n rows of value:_->Just value; _->Nothing

readPolicies :: FilePath -> IO (Either Text (M.Map Text Mode))
readPolicies path=do
  config<-readConfig path
  pure $ do
    (_,_,table)<-config
    policies<-lookupTable ["editor","mcp","permissions"] table
    traverse parseMode (maybe M.empty (fmap snd . tableMap) policies)
  where
    parseMode (Toml.Text' _ value)=case value of "enable"->Right Enable; "prompt"->Right Prompt; "disable"->Right Disable; _->Left "Invalid MCP permission mode in thc/config.toml"
    parseMode _=Left "MCP permission modes in thc/config.toml must be strings"

readEditorDefaults :: IO (Either Text Value)
readEditorDefaults=permissionConfigPath >>= readEditorDefaultsAt

-- A nearer package/repository boundary keeps settings in a containing project
-- from silently configuring an independent nested project.
projectConfigPath :: FilePath -> IO FilePath
projectConfigPath input=do
  absolute<-canonicalizePath input
  directory<-doesDirectoryExist absolute
  let start=if directory then absolute else takeDirectory absolute
  search start start
  where
    search fallback directory=do
      let config=directory </> "thc.toml"
      exists<-doesFileExist config
      if exists then canonicalizePath config else do
        entries<-listDirectory directory
        let boundary=".git" `elem` entries || "cabal.project" `elem` entries || any ((==".cabal").takeExtension) entries
            parent=takeDirectory directory
        if boundary then canonicalizePath config
          else if parent==directory then canonicalizePath (fallback </> "thc.toml")
          else search fallback parent

readEditorDefaultsFor :: FilePath -> IO (Either Text Value)
readEditorDefaultsFor directory=configIO $ do
  global<-readEditorDefaults
  project<-projectConfigPath directory >>= readEditorDefaultsAt
  pure $ do
    globalValue<-global
    projectValue<-project
    case (globalValue,projectValue) of
      (Object globalEntries,Object projectEntries)->Right (Object (KM.union projectEntries globalEntries))
      _->Left "Editor defaults must be a table"

-- Environment entries preserve other configuration and comments, like defaults.
readEnvironmentAt :: FilePath -> IO (Either Text Value)
readEnvironmentAt path=configIO $ do
  config<-readConfig path
  pure $ do
    (_,_,table)<-config
    selected<-lookupTable ["editor","environment"] table
    values<-traverse (primitive . snd) (maybe M.empty tableMap selected)
    pure (object [K.fromText key .= value | (key,value)<-M.toList values])

writeEnvironmentAt :: FilePath -> Value -> IO (Either Text ())
writeEnvironmentAt path=writeTable path ["editor","environment"]

-- Autocomplete provider settings are human-owned, separate from display defaults.
readAutocompleteFor :: FilePath -> IO (Either Text Value)
readAutocompleteFor directory=configIO $ do
  globalPath<-permissionConfigPath
  projectPath<-projectConfigPath directory
  global<-load globalPath
  project<-load projectPath
  pure $ do
    a<-global; b<-project
    pure (Object (KM.union b a))
  where
    load path=do
      config<-readConfig path
      pure $ do
        (_,_,table)<-config
        selected<-lookupTable ["editor","autocomplete"] table
        values<-traverse (primitive . snd) (maybe M.empty tableMap selected)
        pure (KM.fromList [(K.fromText key,value) | (key,value)<-M.toList values])

writeAutocomplete :: Value -> IO (Either Text ())
writeAutocomplete values=permissionConfigPath >>= \path->writeTable path ["editor","autocomplete"] values

-- Update a project's existing override instead of saving an ineffective global
-- value beneath it. Projects without this table continue to use global settings.
writeAutocompleteFor :: FilePath -> Value -> IO (Either Text ())
writeAutocompleteFor directory values=configIO $ do
  path<-projectConfigPath directory
  loaded<-readConfig path
  case loaded >>= (\(_,_,table)->lookupTable ["editor","autocomplete"] table) of
    Left err->pure (Left err)
    Right (Just _)->writeTable path ["editor","autocomplete"] values
    Right Nothing->writeAutocomplete values


-- A project may tighten the human's global ceilings, never raise them. Read
-- both files before each spawn; malformed limits must not restore permissive defaults.
readAgentLimitsFor :: FilePath -> IO (Either Text (Int,Int))
readAgentLimitsFor directory=configIO $ do
  globalPath<-permissionConfigPath
  projectPath<-projectConfigPath directory
  global<-readConfig globalPath
  project<-readConfig projectPath
  pure $ do
    (_,_,globalTable)<-global
    (_,_,projectTable)<-project
    globalLimits<-limits (8,4) globalTable
    projectLimits<-limits globalLimits projectTable
    pure (min (fst globalLimits) (fst projectLimits),min (snd globalLimits) (snd projectLimits))
  where
    limits (agents,subagents) table=do
      settings<-lookupTable ["editor","agents"] table
      let entries=maybe M.empty tableMap settings
          limit name lower fallback=case M.lookup name entries of
            Nothing->Right fallback
            Just (_,Toml.Integer' _ value) | value>=lower && value<=64->Right (fromInteger value)
            _->Left ("editor.agents."<>name<>" must be an integer from "<>T.pack (show lower)<>" to 64")
      (,) <$> limit "max_agents" 1 agents <*> limit "max_subagents" 0 subagents

readAgentContextAt :: FilePath -> IO (Either Text Text)
readAgentContextAt path=do
  config<-readConfig path
  pure $ do
    (_,_,table)<-config
    agent<-lookupTable ["editor","agent"] table
    case agent >>= M.lookup "context" . tableMap of
      Nothing->Right ""
      Just (_,Toml.Text' _ context) | T.length context<=16384->Right context
                                   | otherwise->Left "Agent context exceeds 16384 characters"
      _->Left "Agent context must be a TOML string"

writeAgentContextAt :: FilePath -> Text -> IO (Either Text ())
writeAgentContextAt path context
  | T.length context>16384=pure (Left "Agent context exceeds 16384 characters")
  | otherwise=writeTable path ["editor","agent"] (object ["context" .= context])

readAgentContexts :: FilePath -> IO (Either Text Value)
readAgentContexts directory=configIO $ do
  globalPath<-permissionConfigPath
  projectPath<-projectConfigPath directory
  global<-readAgentContextAt globalPath
  project<-readAgentContextAt projectPath
  pure $ do
    globalText<-global
    projectText<-project
    Right (object ["global" .= object ["path" .= globalPath,"text" .= globalText],
      "project" .= object ["path" .= projectPath,"text" .= projectText]])

configIO :: IO (Either Text a) -> IO (Either Text a)
configIO action=do
  result<-try action
  pure $ case result of
    Left (_::IOException)->Left "Could not locate editor configuration"
    Right value->value

writeEditorDefaults :: Value -> IO (Either Text ())
writeEditorDefaults values=permissionConfigPath >>= \path->writeEditorDefaultsAt path values
readEditorDefaultsAt :: FilePath -> IO (Either Text Value)
readEditorDefaultsAt path=do
  config<-readConfig path
  pure $ do
    (_,_,table)<-config
    defaults<-lookupTable ["editor","defaults"] table
    values<-traverse (primitive . snd) (maybe M.empty tableMap defaults)
    pure (object [K.fromText key .= value | (key,value)<-M.toList values])
writeEditorDefaultsAt :: FilePath -> Value -> IO (Either Text ())
writeEditorDefaultsAt path values=case values of
  Object entries | all (`elem` allowed) (KM.keys entries),all scalar (KM.elems entries) -> writeTable path ["editor","defaults"] values
  _ -> pure (Left "Editor defaults must contain only supported primitive settings")
  where
    allowed=["backend","scale","screenMode","columns","rows","appearance","wordStar","blinkCursor","crtFilter","pixelateUnicode","materialIcons","streamerMode","bufferView","chatSubmit","macKeySymbols"]
    scalar String{}=True; scalar Number{}=True; scalar Bool{}=True; scalar _=False

primitive :: Toml.Value' a -> Either Text Value
primitive value=case value of
  Toml.Text' _ text -> Right (String text)
  Toml.Bool' _ boolean -> Right (Bool boolean)
  Toml.Integer' _ number -> Right (toJSON number)
  Toml.Double' _ number | not (isNaN number || isInfinite number) -> Right (toJSON number)
  _ -> Left "Editor defaults must contain primitive strings, booleans or finite numbers"

tableMap :: Toml.Table' a -> M.Map Text (a,Toml.Value' a)
tableMap (Toml.MkTable table)=table
lookupTable :: [Text] -> Toml.Table' a -> Either Text (Maybe (Toml.Table' a))
lookupTable [] table=Right (Just table)
lookupTable (key:rest) table=case M.lookup key (tableMap table) of
  Nothing -> Right Nothing
  Just (_,Toml.Table' _ nested) -> lookupTable rest nested
  _ -> Left "Configuration namespace must be a TOML table"

readConfig :: FilePath -> IO (Either Text (Maybe BS.ByteString,Text,Toml.Table' Toml.Position))
readConfig path=do
  loaded<-try (catchIOError (Just <$> withBinaryFile path ReadMode (\h->BS.hGet h 1048577)) (\err->if isDoesNotExistError err then pure Nothing else ioError err))
  pure $ case loaded of
    Left (_::IOException) -> Left "Could not read thc/config.toml"
    Right bytes -> do
      unless (maybe True ((<=1048576).BS.length) bytes) (Left "thc/config.toml exceeds 1 MiB")
      text<-either (const (Left "thc/config.toml is not valid UTF-8")) Right (TE.decodeUtf8' (fromMaybe BS.empty bytes))
      table<-either (const (Left "Invalid TOML in thc/config.toml; existing configuration was not changed")) Right (Toml.parse text)
      pure (bytes,text,table)

-- Serialize configuration writers in this process. saveFile also checks the freshly
-- read disk baseline before its atomic rename, preserving other settings.
configWriteLock :: MVar ()
configWriteLock=unsafePerformIO (newMVar ())
{-# NOINLINE configWriteLock #-}
writeTable :: FilePath -> [Text] -> Value -> IO (Either Text ())
writeTable path namespace values=withMVar configWriteLock $ \_ -> do
  config<-readConfig path
  case config of
    Left err -> pure (Left err)
    Right (baseline,text,_) -> case updateConfigTable namespace values text of
      Left err -> pure (Left err)
      Right updated -> do
        saved<-try $ do
          createDirectoryIfMissing True (takeDirectory path)
          saveFile (FileState path baseline) (newBuffer updated)
        pure $ case saved of
          Left (_::IOException) -> Left "Could not save thc/config.toml"
          Right (Left _) -> Left "Configuration changed on disk or could not be saved; retry after reviewing it"
          Right (Right _) -> Right ()

-- Change scalar token spans rather than reprinting the document. The real TOML
-- parser validates every candidate, so unrelated comments/keys remain verbatim.
updateConfigTable :: [Text] -> Value -> Text -> Either Text Text
updateConfigTable namespace (Object values) original=do
  _<-either (const (Left "Invalid TOML configuration")) Right (Toml.parse original)
  foldM update original (KM.toList values)
  where
    update text (key,value)=do
      table<-either (const (Left "Invalid TOML configuration")) Right (Toml.parse text)
      target<-lookupTable namespace table
      rendered<-case value of String{}->Right (json value); Bool{}->Right (json value); Number{}->Right (json value); _->Left "Only primitive TOML settings can be changed"
      candidate<-case target >>= M.lookup (K.toText key) . tableMap of
        Just (_,old) -> do
          _<-primitive old
          let position=Toml.valueAnn old
              start=Toml.posIndex position
          (_,end)<-either (const (Left "Could not locate the TOML setting")) Right
            (TS.scanToken TS.ValueContext (TS.Located position (T.drop start text)))
          pure (T.take start text<>rendered<>T.drop (Toml.posIndex (TS.locPosition end)) text)
        Nothing -> do
          expressions<-either (const (Left "Invalid TOML configuration")) Right (TS.parseRawToml text)
          let after=dropWhile (\expression->case expression of TS.TableExpr parts->map snd (toList parts)/=namespace; _->True) expressions
              assignment=json (String (K.toText key))<>" = "<>rendered<>"\n"
              startLine position=let index=Toml.posIndex position in index-T.length (T.takeWhileEnd (/='\n') (T.take index text))
              nextTable expression=case expression of TS.TableExpr parts->Just (startLine (fst (NE.head parts))); TS.ArrayTableExpr parts->Just (startLine (fst (NE.head parts))); _->Nothing
          pure $ case after of
            _:rest -> let index=fromMaybe (T.length text) (firstJust (map nextTable rest)); prefix=T.take index text
                      in prefix<>newline prefix<>assignment<>T.drop index text
            [] -> text<>newline text<>"["<>T.intercalate "." (map (json.String) namespace)<>"]\n"<>assignment
      unless (BS.length (TE.encodeUtf8 candidate)<=1048576) (Left "Updated thc/config.toml would exceed 1 MiB")
      _<-either (const (Left "Cannot add a setting to this inline/dotted TOML table; use an explicit editor configuration table")) Right (Toml.parse candidate)
      pure candidate
    json=TE.decodeUtf8 . BL.toStrict . encode
    newline text=if T.null text || "\n" `T.isSuffixOf` text then "" else "\n"
    firstJust []=Nothing
    firstJust (Just x:_)=Just x
    firstJust (Nothing:xs)=firstJust xs
updateConfigTable _ _ _=Left "Settings must be a JSON object"
