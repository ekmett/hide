{-# LANGUAGE OverloadedStrings #-}
module ControlMCPCheck (checks) where

import EditorFixture (withEditorTextFixture, sameBufferVersions)
import Control.Exception (bracket)
import Control.Monad (unless, forM_)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Graphics.Vty as V
import qualified Hide.Terminal as Terminal
import System.Directory
import System.Environment (lookupEnv,setEnv,unsetEnv)
import System.IO (openTempFile,hClose)
import qualified Hide.App as App
import Hide.Buffer
import Hide.ControlMCP
import Hide.MCPPermissions (readEditorDefaults,permissionConfigPath)
import Hide.Model

checks :: IO ()
checks=withEditorTextFixture "" "Public reply" (initialDesktop (100,35)) $ \conversation ->
  bracket temporary removePathForcibly $ \root ->
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
        ui d=(screenSize d,videoMode d,appearance d,wordStar d,pixelateUnicode d,materialIcons d,streamerMode d,chatSubmit d,
          [(windowId w,bounds w,selection w,scrollRow w,scrollColumn w) | w<-windows d],menu d,dialog d,clipboard d)
        checkUnchanged label a b extra=do
          sameVersions<-sameBufferVersions a b
          check label (sameVersions && ui a==ui b && extra)
    let terminalId=maybe (error "missing control fixture") id (activeWindow base >>= bufferId)
        terminal=base {buffers=M.adjust (\doc->doc {documentLabel=Just "Terminal batch"}) terminalId (buffers base),terminalMouseTracking=S.singleton terminalId}
        window=maybe (error "missing control fixture window") id (activeWindow terminal)
        mouse action=object ["type" .= ("mouse"::T.Text),"action" .= (action::T.Text),"x" .= (left (bounds window)+3),"y" .= (top (bounds window)+2),"button" .= (0::Int)]
        mouseActions effects=[Terminal.terminalMouseAction event | TerminalMouseInput "batch" event<-effects]
    writeIORef observed []
    (settled,_)<-input terminal [mouse "down"]
    batchEffects<-readIORef observed
    check "guest batch releases an unfinished terminal press" (drag settled==Nothing && mouseActions batchEffects==[Terminal.TerminalMousePress,Terminal.TerminalMouseRelease])
    writeIORef observed []
    (refused,_)<-input terminal [mouse "down",object ["type" .= ("mouse"::T.Text),"action" .= ("up"::T.Text),"x" .= (-1::Int),"y" .= (-1::Int)]]
    refusedEffects<-readIORef observed
    check "guest refusal still releases its terminal press" (drag refused==Nothing && mouseActions refusedEffects==[Terminal.TerminalMousePress,Terminal.TerminalMouseRelease])
    writeIORef observed []
    let human=terminal {drag=Just (TerminalDragging (windowId window) V.BLeft (left (bounds window)+3) (top (bounds window)+2))}
    (isolated,_)<-input human [key "F6" []]
    humanMouseEffects<-readIORef observed
    check "guest batch settles an existing human terminal capture" (drag isolated==Nothing && mouseActions humanMouseEffects==[Terminal.TerminalMouseRelease])
    writeIORef observed []
    (untouched,_)<-input human [object ["type" .= ("mouse"::T.Text),"action" .= ("up"::T.Text),"x" .= (-1::Int),"y" .= (-1::Int)]]
    deniedEffects<-readIORef observed
    check "fully refused guest batch preserves the human terminal capture" (drag untouched==drag human && null deniedEffects)
    writeIORef observed []
    (same,readSettings)<-call shaped "editor_settings" (object [])
    checkUnchanged "reading settings preserves window geometry and focus" same shaped (success readSettings)
    (stillSame,_)<-settings shaped []
    checkUnchanged "empty settings object is a geometry-preserving no-op" stillSame shaped True
    let small=shaped {screenSize=(30,8),videoMode=Nothing}
    (smallRead,smallReply)<-settings small []
    checkUnchanged "small terminal settings can be read without resize or validation failure" smallRead small (success smallReply)
    (themed,themeReply)<-settings small ["appearance" .= ("dark"::T.Text)]
    check "appearance-only update preserves small terminal geometry" (appearance themed==DarkMode && windows themed==windows small && screenSize themed==(30,8) && success themeReply)
    (mode,modeReply)<-settings shaped ["screenMode" .= (259::Int)]
    check "explicit screen mode selects its default grid" (screenSize mode==(80,50) && videoMode mode==Just 259 && success modeReply)
    (invalid,invalidReply)<-settings shaped ["appearance" .= ("dark"::T.Text),"rows" .= (999::Int)]
    checkUnchanged "invalid settings leave session untouched" invalid shaped (rejected invalidReply)
    (_,stored)<-call shaped "editor_settings" (object ["defaults" .= object ["wordStar" .= True,"columns" .= (101::Int)]])
    check "valid startup defaults can be stored" (success stored)
    (_,merged)<-call shaped "editor_settings" (object ["defaults" .= object ["appearance" .= ("light"::T.Text)]])
    persisted<-readEditorDefaults
    check "partial startup defaults preserve other keys" (success merged && case persisted of Right (Object values) -> KM.lookup "columns" values==Just (toJSON (101::Int)) && KM.lookup "wordStar" values==Just (Bool True) && KM.lookup "appearance" values==Just (String "light"); _ -> False)
    (invalidBoth,invalidBothReply)<-call shaped "editor_settings" (object ["settings" .= object ["wordStar" .= True],"defaults" .= object ["scale" .= (99::Int)]])
    afterInvalid<-readEditorDefaults
    checkUnchanged "invalid defaults prevent current settings and persistence changes" invalidBoth shaped (rejected invalidBothReply && afterInvalid==persisted)
    (invalidCurrent,invalidCurrentReply)<-call shaped "editor_settings" (object ["settings" .= object ["rows" .= (999::Int)],"defaults" .= object ["columns" .= (102::Int)]])
    afterInvalidCurrent<-readEditorDefaults
    checkUnchanged "invalid current settings prevent defaults writes" invalidCurrent shaped (rejected invalidCurrentReply && afterInvalidCurrent==persisted)
    path<-permissionConfigPath
    writeFile path "[broken\n"
    (badConfig,badConfigReply)<-settings shaped ["wordStar" .= True]
    checkUnchanged "defaults read failure cannot silently change current settings" badConfig shaped (rejected badConfigReply)
    removeFile path
    (typed,typedReply)<-input base [paste "λ",key "z" ["ctrl"]]
    check "raw input uses ordinary edit and undo behavior" (activeText typed=="old" && success typedReply && browserFrontend typed)
    (copied,copiedReply)<-input base [key "a" ["ctrl"],key "c" ["ctrl"],key "End" [],key "v" ["ctrl"]]
    effects<-readIORef observed
    check "raw clipboard uses editor clipboard in browser sessions" (activeText copied=="oldold" && clipboard copied==clipboard base && success copiedReply && browserFrontend copied && not (any browserClipboard effects))
    forM_ [PermissionDialog "approve:1",AgentDialog "approval:1"] $ \purposeValue -> do
      let protected=base {dialog=Just (Dialog "Agent permission" purposeValue [] 0 ["Allow once","Deny"] [])}
      forM_ [key "Enter" [],key "Escape" [],paste "yes",object ["type" .= ("blur"::T.Text)],object ["type" .= ("mouse"::T.Text),"action" .= ("down"::T.Text),"x" .= (4::Int),"y" .= (4::Int)]] $ \event -> do
        writeIORef observed []
        (blocked,blockedReply)<-input protected [event]
        effectsAfter<-readIORef observed
        checkUnchanged "raw input cannot approve, dismiss or edit MCP or ACP approval dialogs" blocked protected (null effectsAfter && either (const False) ((==Just (0::Int)).field "appliedEvents") blockedReply)
    let permissionMenu=case [(i,j) | (i,(_,_,items))<-zip [0..] menus,(j,MenuItem _ _ command)<-zip [0..] items,command==AgentPermissions] of
          entry:_ -> entry
          [] -> error "Agent Permissions menu missing"
        openMenu=base {menu=Just permissionMenu}
    writeIORef observed []
    (blockedMenu,blockedMenuReply)<-input openMenu [key "Enter" []]
    menuEffects<-readIORef observed
    checkUnchanged "raw input cannot open the permission configuration" blockedMenu openMenu (null menuEffects && either (const False) ((==Just (0::Int)).field "appliedEvents") blockedMenuReply)
    (unchanged,badEvents)<-input base [paste "must not happen",object ["type" .= ("menu"::T.Text),"command" .= ("AgentPermissions"::T.Text)]]
    checkUnchanged "all raw events validate before any event applies" unchanged base (rejected badEvents)
    (quit,quitReply)<-input base [key "q" ["ctrl"],paste "after quit"]
    check "Exit stops remaining events and is reported" (activeText quit=="old" && either (const False) (\v -> field "exitRequested" v==Just True && field "appliedEvents" v==Just (1::Int)) quitReply)
    let modified=insertText "x" base
    (confirm,confirmReply)<-input modified [key "q" ["ctrl"]]
    check "dirty Quit retains normal save confirmation" (dialog confirm/=Nothing && activeText confirm==activeText modified && either (const False) ((==Just False).field "exitRequested") confirmReply)
    let privateDraft=(setComposerInput (newBuffer "private draft") (Selection 0 0) True conversation) {clipboard="private clipboard"}
    (blockedDraft,draftReply)<-input privateDraft [paste "guest text",key "Enter" []]
    check "guest input cannot edit or submit the human conversation draft" (composerSelection blockedDraft==composerSelection privateDraft && revision (composerBuffer blockedDraft)==revision (composerBuffer privateDraft) && clipboard blockedDraft==clipboard privateDraft && either (const False) ((==Just (0::Int)).field "appliedEvents") draftReply)
    (noClipboard,clipboardReply)<-input base {clipboard="private clipboard"} [key "v" ["ctrl"]]
    check "guest paste cannot inherit or return the human clipboard" (activeText noClipboard=="old" && clipboard noClipboard=="private clipboard" && either (const False) ((==Just (""::T.Text)).field "clipboard") clipboardReply)
    (_,streamerWrite)<-settings base ["streamerMode" .= True]
    (_,streamerDefaults)<-call base "editor_settings" (object ["defaults" .= object ["streamerMode" .= True]])
    check "guest cannot change Streamer mode through session or startup settings" (rejected streamerWrite && rejected streamerDefaults)
    forM_ ["query","steer"] $ \action -> do
      (unchangedAction,actionReply)<-settings base ["wordStar" .= True,"chatSubmit" .= (action::T.Text)]
      (unchangedDefault,defaultReply)<-call base "editor_settings" (object ["settings" .= object ["wordStar" .= True],"defaults" .= object ["chatSubmit" .= action]])
      checkUnchanged "guest cannot change chat behavior via current settings" unchangedAction base (rejected actionReply)
      checkUnchanged "guest cannot change chat behavior via persisted defaults" unchangedDefault base (rejected defaultReply)
    let (humanOptions,_) = runCommand ChatInputOptions base
        humanDialog=maybe (error "missing chat input options") id (dialog humanOptions)
        (humanChoice,humanEffects)=submitDialog 0 (humanDialog {fields=[Radio "Enter action" ["Query","Steer"] 1]}) humanOptions
    (_,savedChoice)<-App.applyEffects humanChoice humanEffects
    persistedChoice<-readEditorDefaults
    check "human Options choice saves through the application effect" (chatSubmit savedChoice==SteerSubmit && case persistedChoice of Right value->field "chatSubmit" value==Just ("steer"::T.Text); _->False)
    (_,blockedChoice)<-call savedChoice "editor_settings" (object ["defaults" .= object ["chatSubmit" .= ("query"::T.Text)]])
    retainedChoice<-readEditorDefaults
    check "blocked guest default write preserves persisted human choice" (rejected blockedChoice && retainedChoice==persistedChoice)
    (_,chatRead)<-call base {chatSubmit=SteerSubmit} "editor_settings" (object [])
    check "chat submit behavior remains readable" (either (const False) ((==Just ("steer"::T.Text)).field "chatSubmit") chatRead)
    (_,privateRead)<-input base {clipboard="secret"} [object ["type" .= ("blur"::T.Text)]]
    check "no-op guest events do not expose the human clipboard" (either (const False) ((==Just (""::T.Text)).field "clipboard") privateRead)
    let humanPrefix=base {wordStar=True,prefix=Just 'k',heldModifiers=[]}
    (plainCharacter,_)<-input humanPrefix [key "y" []]
    check "guest characters do not complete a human WordStar prefix" (activeText plainCharacter=="yold" && prefix plainCharacter==Nothing)
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
