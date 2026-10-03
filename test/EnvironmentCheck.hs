{-# LANGUAGE OverloadedStrings #-}
module EnvironmentCheck (checks) where
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process (readProcess)
import THC.Edit.Environment
import THC.Edit.GuestAccess
import THC.Edit.MCPPermissions (permissionConfigPath)
import THC.Edit.Model

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->
  bracket (mapM (\name->(name,) <$> lookupEnv name) names) (mapM_ restore) $ \_->do
    setEnv "XDG_CONFIG_HOME" (root </> "config")
    let configFile=root </> "config" </> "thc" </> "config.toml"
    createDirectoryIfMissing True (takeDirectory configFile)
    actual<-permissionConfigPath
    expected<-canonicalizePath configFile
    unless (actual==expected) (error "Environment tests must use the temporary config only")
    writeFile (root </> "cabal.project") "packages: .\n"
    let check label ok=unless ok (error label)
        good=either (const False) (const True)
        bad=not.good
        set scope value=changeEnvironment True root scope (object ["THC_ENV_CHECK" .= (value::Value)])
    result<-set "session" (String "visible to child")
    check "session update succeeds" (good result)
    child<-readProcess "ghc" ["-e","System.Environment.getEnv \"THC_ENV_CHECK\" >>= putStrLn"] ""
    check "new child inherits updated environment" (child=="visible to child\n")
    rejected<-changeEnvironment True root "session" (object ["THC_ENV_CHECK" .= ("wrong"::T.Text),"BAD=NAME" .= ("bad"::T.Text)])
    current<-lookupEnv "THC_ENV_CHECK"
    check "invalid batch leaves environment unchanged" (bad rejected && current==Just "visible to child")
    nul<-set "session" (String "bad\0value")
    check "NUL rejected" (bad nul)
    mapM_ (\name->do
      denied<-changeEnvironment True root "session" (Object (KM.singleton name (String "private")))
      check "authority and credentials cannot be replaced by an agent" (bad denied))
      ["THC_EDIT_MCP_TOKEN","THC_EDIT_SESSION","HOME","XDG_CONFIG_HOME","API_TOKEN"]
    setEnv "THC_ENV_API_TOKEN" "synthetic-secret"
    readback<-environmentTool root "environment_get" (object ["names" .= (["THC_ENV_API_TOKEN","THC_ENV_CHECK","THC_ENV_MISSING"]::[T.Text])])
    let values=readback >>= \v->maybe (Left "missing values") Right (parseMaybe (withObject "reply" (.: "values")) v)
    check "tool reads normal values and redacts secrets" (case values of
      Right (Object v)->KM.lookup "THC_ENV_CHECK" v==Just (String "visible to child") && KM.lookup "THC_ENV_API_TOKEN" v==Just (String "[redacted]") && KM.lookup "THC_ENV_MISSING" v==Just Null
      _->False)
    global<-set "global" (String "global")
    project<-set "project" (String "project")
    globalAgain<-set "global" (String "global next")
    effective<-lookupEnv "THC_ENV_CHECK"
    check "project has precedence over global" (all good [global,project,globalAgain] && effective==Just "project")
    writeFile (root </> "thc.toml") "# retained comment\n[editor.environment]\nTHC_ENV_CHECK = false # explicitly unset\n[editor.defaults]\ncolumns = 100\n"
    loaded<-loadEnvironment root
    unset<-lookupEnv "THC_ENV_CHECK"
    check "saved unset overrides global and inherited environment" (good loaded && unset==Nothing)
    changed<-set "project" (String "saved")
    text<-readFile (root </> "thc.toml")
    check "saving preserves unrelated settings and comments" (good changed && "# retained comment" `T.isInfixOf` T.pack text && "columns = 100" `T.isInfixOf` T.pack text)
    unsetEnv "THC_ENV_CHECK"
    restarted<-loadEnvironment root
    restored<-lookupEnv "THC_ENV_CHECK"
    check "restart loads saved overrides" (good restarted && restored==Just "saved")
    writeFile configFile "[broken\n"
    failed<-set "project" (String "do not apply")
    unchanged<-lookupEnv "THC_ENV_CHECK"
    check "invalid configuration cannot partially apply changes" (bad failed && unchanged==Just "saved")
    let d=(initialDesktop (80,25)) {defaultDirectory=Just root}
        (_,effects)=runCommand EnvironmentOptions d
    check "menu dispatches and raw agent input cannot bypass tool permissions" (effects==[EnvironmentAction "show" []] && not (guestCommandAllowed EnvironmentOptions) && not (guestEffectsAllowed effects))
    form<-environmentAction "choose" ["1"] d
    check "environment dialog blocks agent input" (guestModalBlocked form && not (guestKeyAllowed form V.KEnter []))
    let filled=form {dialog=fmap (\dg->dg {fields=[Input "Name" "THC_ENV_CHECK" 13,Input "Environment value" "from form" 9,ComboBox "Scope" ["session","project","global"] 0 Nothing,CheckBox "Unset variable" False]}) (dialog form)}
        (_,submitted)=handleEvent (V.EvKey V.KEnter []) filled
    check "Apply sends selected scope, value and unset state" (submitted==[EnvironmentAction "edit" ["0","THC_ENV_CHECK","from form","session","false"]])
  where
    names=["XDG_CONFIG_HOME","THC_ENV_CHECK","THC_ENV_API_TOKEN"]
    restore (name,value)=maybe (unsetEnv name) (setEnv name) value
    temporary=do
      base<-getTemporaryDirectory
      (path,h)<-openTempFile base "thc-environment-check"
      hClose h; removeFile path; createDirectory path
      pure path
