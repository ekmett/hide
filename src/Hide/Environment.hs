{-# LANGUAGE OverloadedStrings #-}
-- | Editor environment overlays for future subprocesses.
--
-- Project entries override global entries; session overrides need no persistence.
-- An entire request validates before mutation, and persisted changes reread merged
-- precedence before updating affected process variables. Authority/transport
-- variables are protected; agent inspection redacts sensitive values and agent
-- mutation also rejects sensitive names. Existing children are unaffected.
module Hide.Environment
  ( environmentTools, environmentToolNames, environmentTool
  , loadEnvironment, changeEnvironment, environmentAction
  ) where

import Control.Exception (IOException, try)
import Control.Monad (unless, forM_)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAsciiLower,isAsciiUpper,isDigit)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnvironment,lookupEnv,setEnv,unsetEnv)
import Hide.GuestAccess (sensitiveLabel)
import Hide.MCPPermissions (permissionConfigPath,projectConfigPath,readEnvironmentAt,writeEnvironmentAt)
import Hide.Model

-- Paths locating authority/configuration and editor transport credentials are
-- host-owned. Neither persisted project settings nor agent tools may replace them.
protected :: Text -> Bool
protected name=let n=T.toUpper name in
  any (`T.isPrefixOf` n) ["THC_EDIT_","CODEX_","XDG_","DYLD_","LD_","GHC_ENVIRONMENT"] ||
  n `elem` ["HOME","USERPROFILE","APPDATA","LOCALAPPDATA","TMPDIR","TMP","TEMP"]

validate :: Bool -> Value -> Either Text [(Text,Maybe Text)]
validate agent=withEntries
  where
    withEntries (Object entries)=traverse entry (KM.toList entries)
    withEntries _=Left "Environment must be an object of names with string values or null (unset)."
    entry (key,value)=do
      let name=K.toText key
          letter c=isAsciiLower c || isAsciiUpper c || c=='_'
      unless (not (T.null name) && letter (T.head name) && T.all (\c->letter c || isDigit c) name)
        (Left "Use environment names containing ASCII letters, digits and underscores, beginning with a letter or underscore.")
      unless (not (protected name) && not (agent && sensitiveLabel name))
        (Left ("Protected environment variable: "<>name))
      content<-case value of
        String text | not (T.any (=='\0') text)->Right (Just text)
        Null->Right Nothing
        Bool False->Right Nothing -- TOML spelling of an explicit unset
        _->Left "Use a string without NUL, or null/false to unset a variable."
      pure (name,content)

savedEnvironment :: FilePath -> IO (Either Text Value)
savedEnvironment directory=do
  global<-permissionConfigPath >>= readEnvironmentAt
  project<-projectConfigPath directory >>= readEnvironmentAt
  pure $ do
    a<-global; b<-project
    case (a,b) of
      (Object x,Object y)->Right (Object (KM.union y x))
      _->Left "Environment configuration must be a table."

apply :: [(Text,Maybe Text)] -> IO ()
apply entries=forM_ entries $ \(key,value)->maybe (unsetEnv (T.unpack key)) (setEnv (T.unpack key) . T.unpack) value

safeIO :: IO (Either Text a) -> IO (Either Text a)
safeIO action=do
  result<-try action
  pure $ case result of
    Left (_::IOException)->Left "Could not read or update the editor environment."
    Right value->value

-- | Apply merged global/project environment settings to this editor process.
loadEnvironment :: FilePath -> IO (Either Text ())
loadEnvironment directory=safeIO $ do
  loaded<-savedEnvironment directory
  case loaded >>= validate False of
    Left err->pure (Left err)
    Right entries->apply entries >> pure (Right ())

-- | Validate and apply a scoped environment change for future children.
-- The agent flag imposes additional sensitive-name restrictions.
changeEnvironment :: Bool -> FilePath -> Text -> Value -> IO (Either Text ())
changeEnvironment agent directory scope values=safeIO $ case validate agent values of
  Left err->pure (Left err)
  Right entries | scope=="session" -> apply entries >> pure (Right ())
  Right entries | scope `elem` ["project","global"] -> do
    loaded<-savedEnvironment directory
    case loaded >>= validate False of
      Left err->pure (Left err)
      Right _->do
        path<-if scope=="global" then permissionConfigPath else projectConfigPath directory
        written<-writeEnvironmentAt path (object [K.fromText k .= maybe (Bool False) String v | (k,v)<-entries])
        case written of
          Left err->pure (Left err)
          Right ()->do
            merged<-savedEnvironment directory
            case merged >>= validate False of
              Left err->pure (Left err)
              Right effective->apply (filter (\(key,_)->key `elem` map fst entries) effective) >> pure (Right ())
  _->pure (Left "Scope must be session, project or global.")

redacted :: Text -> Bool
redacted name=protected name || sensitiveLabel name

environmentToolNames :: [Text]
environmentToolNames=["environment_get","environment_set"]
environmentTools :: [Value]
environmentTools=
  [tool "environment_get" True "Inspect the editor process environment used by newly launched builds, terminals, debuggers and agents. Optional names limits the result; absent names are null. Credential and editor authority values are always redacted. Use before prescribing shell exports or an editor restart." (object ["names" .= object ["type" .= ("array"::Text),"items" .= object ["type" .= ("string"::Text)]]]) [],
   tool "environment_set" False "Set or unset variables for newly launched editor subprocesses. values maps names to strings or null (unset); scope is session (default), project (thc.toml), or global (thc/config.toml). Project overrides global. Existing processes retain their environment: restart only the affected terminal/provider/job. Credential and editor authority variables cannot be changed through this tool. Prefer a project build configuration fix when it makes the dependency discoverable for everyone." (object ["values" .= object ["type" .= ("object"::Text)],"scope" .= object ["type" .= ("string"::Text),"enum" .= (["session","project","global"]::[Text])]]) ["values"]]
  where
    tool name readonly description properties required=object
      ["name" .= (name::Text),"description" .= (description::Text),"inputSchema" .= object
        ["type" .= ("object"::Text),"properties" .= properties,"required" .= (required::[Text]),"additionalProperties" .= False],
       "annotations" .= object ["readOnlyHint" .= readonly,"destructiveHint" .= not readonly,"openWorldHint" .= not readonly]]

-- | Expose redacted environment inspection and restricted agent updates.
environmentTool :: FilePath -> Text -> Value -> IO (Either Text Value)
environmentTool directory name args=safeIO $ case parseEither parse args of
  Left err->pure (Left (T.pack err))
  Right (Left names)->do
    allEntries<-getEnvironment
    let keys=maybe (map (T.pack . fst) (sortOn fst allEntries)) id names
    entries<-mapM (\key->do value<-lookupEnv (T.unpack key); pure (K.fromText key,if redacted key then String "[redacted]" else maybe Null (String . T.pack) value)) keys
    pure (Right (object ["values" .= Object (KM.fromList entries),"appliesTo" .= ("new processes"::Text)]))
  Right (Right (scope,values))->do
    changed<-changeEnvironment True directory scope values
    pure (object ["scope" .= scope,"appliesTo" .= ("new processes; existing processes unchanged"::Text)] <$ changed)
  where
    parse=withObject "environment arguments" $ \o ->case name of
      "environment_get"->do
        unless (all (`elem` ["names"]) (KM.keys o)) (fail "Unknown argument")
        names<-o .:? "names"
        unless (maybe True (all (\n->not (T.null n) && not (T.any (`elem` ['\0','=']) n))) names) (fail "Invalid environment name")
        pure (Left names)
      "environment_set"->do
        unless (all (`elem` ["values","scope"]) (KM.keys o)) (fail "Unknown argument")
        scope<-o .:? "scope" .!= "session"
        values<-o .: "values"
        pure (Right (scope,values))
      _->fail "Unknown environment tool"

-- | Dispatch the human environment-dialog actions. The list contains names only;
-- the value editor participates in Streamer masking.
environmentAction :: Text -> [Text] -> Desktop -> IO Desktop
environmentAction action args d=case (action,args) of
  ("show",_)->showVariables
  ("choose","1":_)->pure (form "" "" False "session")
  ("choose","0":name:_)->do
    value<-lookupEnv (T.unpack name)
    pure (form name (maybe "" T.pack value) (value==Nothing) "session")
  ("edit","0":name:value:scope:unset:_)->do
    changed<-changeEnvironment False (startingDirectory d) scope (object [K.fromText name .= if unset=="true" then Null else String value])
    case changed of
      Left err->pure ((form name value (unset=="true") scope) {status=err})
      Right ()->do updated<-showVariables; pure updated {status="Environment updated for new processes."}
  _->pure d
  where
    showVariables=do
      entries<-sortOn fst <$> getEnvironment
      pure d {dialog=Just (Dialog "Environment" (EnvironmentDialog "choose")
        [ListBox "Variables" [T.pack k | (k,_)<-entries] 0]
        0 ["Edit","New","Cancel"] ["Changes affect new builds, terminals and agents."]),menu=Nothing,drag=Nothing,dragOriginal=Nothing,dragTabs=Nothing,tabDropTarget=Nothing}
    form :: Text -> Text -> Bool -> Text -> Desktop
    form name value unset scope=d {dialog=Just (Dialog "Environment variable" (EnvironmentDialog "edit")
      [Input "Name" name (T.length name),Input "Environment value" value (T.length value),ComboBox "Scope" ["session","project","global"] (if scope=="global" then 2 else if scope=="project" then 1 else 0) Nothing,CheckBox "Unset variable" unset]
      0 ["Apply","Cancel"] ["Project overrides global.","Only new processes receive changes."]),menu=Nothing,drag=Nothing,dragOriginal=Nothing,dragTabs=Nothing,tabDropTarget=Nothing}
