-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.Environment
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Editor environment overlays for future subprocesses.
--
-- Project entries override global entries; session overrides need no persistence.
-- An entire request validates before mutation, and persisted changes reread merged
-- precedence before updating affected process variables. Authority/transport
-- variables are protected; agent inspection redacts sensitive values and agent
-- mutation also rejects sensitive names. Existing children are unaffected.
module Hide.Environment
  ( EnvironmentCommands, withEnvironmentCommands, environmentServices
  , loadEnvironment, environmentAction
  ) where

import Control.Exception (IOException, try)
import Control.Monad (unless, forM_)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnvironment,lookupEnv,setEnv,unsetEnv)
import Hide.GuestAccess (sensitiveLabel)
import Hide.MCPPermissions (permissionConfigPath,projectConfigPath,readEnvironmentAt,writeEnvironmentAt)
import Hide.Model hiding (Command)
import Hide.Plugin.Command
import qualified Hide.Plugin.Environment as E

-- Paths locating authority/configuration and editor transport credentials are
-- host-owned. Neither persisted project settings nor agent tools may replace them.
protected :: Text -> Bool
protected name=let n=T.toUpper name in
  any (`T.isPrefixOf` n) ["THC_EDIT_","CODEX_","XDG_","DYLD_","LD_","GHC_ENVIRONMENT"] ||
  n `elem` ["HOME","USERPROFILE","APPDATA","LOCALAPPDATA","TMPDIR","TMP","TEMP"]

-- Syntax has one checked constructor for both saved overlays and plugin calls.
-- Policy remains here: no linked tool can select the human validation path.
validate :: Bool -> Value -> Either Text [(Text,Maybe Text)]
validate agent values=E.setArguments "session" values >>= validateEntries agent . E.setEntries

validateEntries :: Bool -> [(Text,Maybe Text)] -> Either Text [(Text,Maybe Text)]
validateEntries agent entries=do
  forM_ entries $ \(name,_)->unless (not (protected name) && not (agent && sensitiveLabel name))
    (Left ("Protected environment variable: "<>name))
  pure entries

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

-- Human dialogs and agent tools share the same overlay/mutation owner. Syntax
-- and the complete policy check finish before any configuration or process write.
changeEnvironment :: Bool -> FilePath -> Text -> Value -> IO (Either Text ())
changeEnvironment agent directory scope values=case E.setArguments scope values of
  Left err->pure (Left err)
  Right arguments->changePreparedEnvironment agent directory arguments

changePreparedEnvironment :: Bool -> FilePath -> E.SetArguments -> IO (Either Text ())
changePreparedEnvironment agent directory arguments=safeIO $ case validateEntries agent (E.setEntries arguments) of
  Left err->pure (Left err)
  Right entries | E.setScope arguments==E.SessionEnvironment -> apply entries >> pure (Right ())
  Right entries -> do
    loaded<-savedEnvironment directory
    case loaded >>= validate False of
      Left err->pure (Left err)
      Right _->do
        path<-if E.setScope arguments==E.GlobalEnvironment then permissionConfigPath else projectConfigPath directory
        written<-writeEnvironmentAt path (object [K.fromText k .= maybe (Bool False) String v | (k,v)<-entries])
        case written of
          Left err->pure (Left err)
          Right ()->do
            merged<-savedEnvironment directory
            case merged >>= validate False of
              Left err->pure (Left err)
              Right effective->apply (filter (\(key,_)->key `elem` map fst entries) effective) >> pure (Right ())

redacted :: Text -> Bool
redacted name=protected name || sensitiveLabel name

-- The command scope belongs to the editor session, not an individual plugin.
-- A captured capability cannot reach a replacement session after retirement.
data EnvironmentCommands = EnvironmentCommands (Registry FilePath)
  (Command FilePath E.GetArguments Value) (Command FilePath E.SetArguments Value)

-- | Scope typed environment operations. Calls already admitted may finish;
-- deferred calls after retirement fail before reading or changing the environment.
withEnvironmentCommands :: (EnvironmentCommands -> IO a) -> IO a
withEnvironmentCommands use=withRegistry $ \registry->do
  readRef<-registerCommand registry (CommandDef "hide.environment.get" "Read redacted environment"
    E.getInput E.getOutput (\_ arguments->fmap (either (Left . CommandRejected) Right) (readEnvironment arguments))) >>= registered
  setRef<-registerCommand registry (CommandDef "hide.environment.set" "Update environment for new processes"
    E.setInput E.setOutput setEnvironment) >>= registered
  use (EnvironmentCommands registry readRef setRef)
  where
    registered=either (ioError . userError . show) pure
    setEnvironment directory arguments=do
      result<-changePreparedEnvironment True directory arguments
      pure $ either (Left . CommandRejected) (const (Right (object
        ["scope" .= E.scopeText (E.setScope arguments),"appliesTo" .= ("new processes; existing processes unchanged"::Text)]))) result

-- | Grant the checked agent operations for a captured project directory. Invoke
-- only on the admitted caller's worker. No Desktop or human validation switch is
-- retained; the host owns redaction, protected names and overlay precedence.
environmentServices :: EnvironmentCommands -> FilePath -> E.EnvironmentServices
environmentServices (EnvironmentCommands registry readRef setRef) directory=E.EnvironmentServices
  { E.environmentGet=invoke registry readRef directory
  , E.environmentSet=invoke registry setRef directory
  }

readEnvironment :: E.GetArguments -> IO (Either Text Value)
readEnvironment arguments=safeIO $ do
  allEntries<-getEnvironment
  let keys=maybe (map (T.pack . fst) (sortOn fst allEntries)) id (E.getNames arguments)
  entries<-mapM (\key->do value<-lookupEnv (T.unpack key); pure (K.fromText key,if redacted key then String "[redacted]" else maybe Null (String . T.pack) value)) keys
  pure (Right (object ["values" .= Object (KM.fromList entries),"appliesTo" .= ("new processes"::Text)]))

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
