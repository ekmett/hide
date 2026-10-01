{-# LANGUAGE OverloadedStrings #-}
module ControlMCPCheck (checks) where

import Control.Exception (bracket)
import Control.Monad (unless, forM_)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv,setEnv,unsetEnv)
import System.IO (openTempFile,hClose)
import THC.Edit.Buffer
import THC.Edit.ControlMCP
import THC.Edit.MCPPermissions (readEditorDefaults,permissionConfigPath)
import THC.Edit.Model

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root ->
  bracket (lookupEnv "XDG_CONFIG_HOME") (maybe (unsetEnv "XDG_CONFIG_HOME") (setEnv "XDG_CONFIG_HOME")) $ \_ -> do
    setEnv "XDG_CONFIG_HOME" root
    observed<-newIORef []
    let base=(addDocument Nothing (newBuffer "old") (initialDesktop (100,35))) {wordStar=False,browserFrontend=True}
        shaped=base {windows=map (\w -> w {bounds=Rect 3 4 51 17}) (windows base)}
        check label ok=unless ok (error label)
        apply d effects=modifyIORef' observed (++effects) >> pure (Exit `elem` effects,d)
        call d name args=do (updated,finish)<-controlTool apply d name args; result<-finish; pure (updated,result)
        settings d values=call d "editor_settings" (object ["settings" .= object values])
        input d events=call d "editor_input" (object ["events" .= events])
        key name mods=object ["type" .= ("key"::T.Text),"key" .= (name::T.Text),"mods" .= (mods::[T.Text])]
        paste text=object ["type" .= ("paste"::T.Text),"text" .= (text::T.Text)]
        field name=parseMaybe (withObject "reply" (.: name))
        success=either (const False) (const True)
        rejected=either (const True) (const False)
    (same,readSettings)<-call shaped "editor_settings" (object [])
    check "reading settings preserves window geometry and focus" (same==shaped && success readSettings)
    (stillSame,_)<-settings shaped []
    check "empty settings object is a geometry-preserving no-op" (stillSame==shaped)
    let small=shaped {screenSize=(30,8),videoMode=Nothing}
    (smallRead,smallReply)<-settings small []
    check "small terminal settings can be read without resize or validation failure" (smallRead==small && success smallReply)
    (themed,themeReply)<-settings small ["appearance" .= ("dark"::T.Text)]
    check "appearance-only update preserves small terminal geometry" (appearance themed==DarkMode && windows themed==windows small && screenSize themed==(30,8) && success themeReply)
    (mode,modeReply)<-settings shaped ["screenMode" .= (259::Int)]
    check "explicit screen mode selects its default grid" (screenSize mode==(80,50) && videoMode mode==Just 259 && success modeReply)
    (invalid,invalidReply)<-settings shaped ["appearance" .= ("dark"::T.Text),"rows" .= (999::Int)]
    check "invalid settings leave session untouched" (invalid==shaped && rejected invalidReply)
    (_,stored)<-call shaped "editor_settings" (object ["defaults" .= object ["wordStar" .= True,"columns" .= (101::Int)]])
    check "valid startup defaults can be stored" (success stored)
    (_,merged)<-call shaped "editor_settings" (object ["defaults" .= object ["appearance" .= ("light"::T.Text)]])
    persisted<-readEditorDefaults
    check "partial startup defaults preserve other keys" (success merged && case persisted of Right (Object values) -> KM.lookup "columns" values==Just (toJSON (101::Int)) && KM.lookup "wordStar" values==Just (Bool True) && KM.lookup "appearance" values==Just (String "light"); _ -> False)
    (invalidBoth,invalidBothReply)<-call shaped "editor_settings" (object ["settings" .= object ["wordStar" .= True],"defaults" .= object ["scale" .= (99::Int)]])
    afterInvalid<-readEditorDefaults
    check "invalid defaults prevent current settings and persistence changes" (invalidBoth==shaped && rejected invalidBothReply && afterInvalid==persisted)
    (invalidCurrent,invalidCurrentReply)<-call shaped "editor_settings" (object ["settings" .= object ["rows" .= (999::Int)],"defaults" .= object ["columns" .= (102::Int)]])
    afterInvalidCurrent<-readEditorDefaults
    check "invalid current settings prevent defaults writes" (invalidCurrent==shaped && rejected invalidCurrentReply && afterInvalidCurrent==persisted)
    path<-permissionConfigPath
    writeFile path "[broken\n"
    (badConfig,badConfigReply)<-settings shaped ["wordStar" .= True]
    check "defaults read failure cannot silently change current settings" (badConfig==shaped && rejected badConfigReply)
    removeFile path
    (typed,typedReply)<-input base [paste "λ",key "z" ["ctrl"]]
    check "raw input uses ordinary edit and undo behavior" (activeText typed=="old" && success typedReply && browserFrontend typed)
    (copied,copiedReply)<-input base [key "a" ["ctrl"],key "c" ["ctrl"],key "End" [],key "v" ["ctrl"]]
    effects<-readIORef observed
    check "raw clipboard uses editor clipboard in browser sessions" (activeText copied=="oldold" && clipboard copied=="old" && success copiedReply && browserFrontend copied && not (any browserClipboard effects))
    forM_ [PermissionDialog "approve:1",AgentDialog "approval:1"] $ \purposeValue -> do
      let protected=base {dialog=Just (Dialog "Agent permission" purposeValue [] 0 ["Allow once","Deny"] [])}
      forM_ [key "Enter" [],key "Escape" [],paste "yes",object ["type" .= ("blur"::T.Text)],object ["type" .= ("mouse"::T.Text),"action" .= ("down"::T.Text),"x" .= (4::Int),"y" .= (4::Int)]] $ \event -> do
        writeIORef observed []
        (blocked,blockedReply)<-input protected [event]
        effectsAfter<-readIORef observed
        check "raw input cannot approve, dismiss or edit MCP or ACP approval dialogs" (blocked==protected && null effectsAfter && either (const False) ((==Just (0::Int)).field "appliedEvents") blockedReply)
    let permissionMenu=case [(i,j) | (i,(_,_,items))<-zip [0..] menus,(j,MenuItem _ _ command)<-zip [0..] items,command==AgentPermissions] of
          entry:_ -> entry
          [] -> error "Agent Permissions menu missing"
        openMenu=base {menu=Just permissionMenu}
    writeIORef observed []
    (blockedMenu,blockedMenuReply)<-input openMenu [key "Enter" []]
    menuEffects<-readIORef observed
    check "raw input cannot open the permission configuration" (blockedMenu==openMenu && null menuEffects && either (const False) ((==Just (0::Int)).field "appliedEvents") blockedMenuReply)
    (unchanged,badEvents)<-input base [paste "must not happen",object ["type" .= ("menu"::T.Text),"command" .= ("AgentPermissions"::T.Text)]]
    check "all raw events validate before any event applies" (unchanged==base && rejected badEvents)
    (quit,quitReply)<-input base [key "q" ["ctrl"],paste "after quit"]
    check "Exit stops remaining events and is reported" (activeText quit=="old" && either (const False) (\v -> field "exitRequested" v==Just True && field "appliedEvents" v==Just (1::Int)) quitReply)
    let modified=insertText "x" base
    (confirm,confirmReply)<-input modified [key "q" ["ctrl"]]
    check "dirty Quit retains normal save confirmation" (dialog confirm/=Nothing && activeText confirm==activeText modified && either (const False) ((==Just False).field "exitRequested") confirmReply)
    putStrLn "editor control MCP checks passed"
  where
    browserClipboard ReadBrowserClipboard=True
    browserClipboard WriteBrowserClipboard{}=True
    browserClipboard _=False
    temporary=do
      base<-getTemporaryDirectory
      (path,h)<-openTempFile base "thc-control-mcp"
      hClose h; removeFile path; createDirectory path
      canonicalizePath path
