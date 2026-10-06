{-# LANGUAGE OverloadedStrings #-}
module BindingsCheck (checks) where
import SourceWindowFixture (sourceFixtureBuffer)
import Control.Monad (unless,forM_)
import Data.List (subsequences)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_)
import System.Directory (getTemporaryDirectory, removePathForcibly, removeFile, createDirectory, canonicalizePath)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import qualified Data.ByteString as BS
import qualified Hide.Plugin.Window as W
import qualified Hide.Plugin.Menu as P
import Hide.PluginWindowHost (adoptWindowUpdate)
import Hide.TextPresentation (prepareTextPresentations)
import Hide.BufferView (BufferView(..))
import Hide.Window (nativeMenuShortcut)
import Hide.Keybindings
import qualified Hide.Protocol as P
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Hide.Bindings
import Hide.Commands
import Hide.Sidebar
import Hide.Model
import Hide.Browser (Entry(..))
import Hide.GuestAccess (guestKeyAllowed, beginGuestInput, endGuestInput)
import Hide.Files (FileState(..))
import Hide.Buffer
import Hide.Render (renderKey)

checks :: IO ()
checks=do
  prefixChecks
  dialogInputChecks
  debuggerNavigationChecks
  let check name ok=unless ok (error name)
      prepare=either (error . show) id . platformBindings [] TerminalPlatform . M.singleton "source" . M.fromList
      bindings=prepare [("hide.file.save",["Ctrl+Shift+S"]),("hide.file.open",[])]
      base=addDocument (Just (FileState "/project/Main.hs" Nothing)) (newBuffer "hello") (initialDesktop (80,25))
      source=base {keyBindings=bindings}
      sourceKeys=bindings M.! (TerminalPlatform,SourceKeys)
      key k mods d=handleEvent (V.EvKey k mods) d
      noEffects (_,effects)=null effects
      rejected= either (const True) (const False) . platformBindings [] TerminalPlatform . M.singleton "source" . M.fromList
  check "Mac combined modifier display follows Control Option Shift Command order" (keyLabel base {nativeMac=True} "Ctrl+Cmd+Alt+Shift+S"=="⌃⌥⇧⌘S")
  check "Command is distinct from Control" (readChord "Cmd+Alt+Shift+S"==Right (V.KChar 's',[V.MMeta,V.MAlt,V.MShift]) && chordName (V.KChar 's') [V.MMeta,V.MShift,V.MAlt]==Just "Cmd+Alt+Shift+S" && chordName (V.KChar 's') [V.MCtrl]/=chordName (V.KChar 's') [V.MMeta])
  let platforms=either (error . show) id (configuredBindings [] (M.singleton "macos" (M.singleton "source" (M.fromList [("hide.file.save",["Cmd+Shift+S"]),("hide.file.save-as",[])]))))
      mac=base {nativeMac=True,videoMode=Just 3,keyBindings=platforms}
  check "macOS remap owns Command without changing Control" (boundKeyCommand (V.KChar 's') [V.MMeta,V.MShift] mac==Just Save && boundKeyCommand (V.KChar 's') [V.MMeta] mac==Nothing && boundKeyCommand (V.KChar 's') [V.MCtrl] mac==Nothing && boundKeyCommand (V.KChar 's') [V.MCtrl] base {keyBindings=platforms}==Just Save)
  check "macOS composed Option characters cannot bind commands" (either (const True) (const False) (platformBindings [] MacPlatform (M.singleton "source" (M.singleton "hide.file.save" ["Alt+J"]))))
  let replaceMaps=either (error . show) id (configuredBindings [] (M.singleton "macos" (M.singleton "source" (M.fromList [("hide.search.replace",["Cmd+Alt+F"]),("hide.edit.copy",[])]))))
      replaceMac=mac {keyBindings=replaceMaps}
  check "configured Command Option Replace dispatches through its owner" (maybe False (\dg->case purpose dg of Searching True _->True; _->False) (dialog (fst (key (V.KChar 'f') [V.MMeta,V.MAlt] replaceMac))))
  check "unbound Command copy cannot fall through to a fixed shortcut" (clipboard (fst (key (V.KChar 'c') [V.MMeta] (modifyActive (\w->w {selection=Selection 0 5}) replaceMac)))=="")
  let modalMaps=either (error . show) id (configuredBindings [] (M.singleton "macos" (M.singleton "source" (M.fromList [("hide.edit.copy",["Cmd+Left"]),("hide.cursor.left",[])]))))
      sourceModal=mac {keyBindings=modalMaps}
      editorModal=prompt "Edit" Information [TextArea "Text" True (newBuffer "draft") (Selection 0 5) 0 0] sourceModal
  check "native source accelerators cannot hijack modal movement" (nativeMenuShortcut sourceModal Copy==("\xf702",8) && nativeMenuShortcut editorModal Copy==("c",8) && clipboard (fst (key V.KLeft [V.MMeta] editorModal))=="")
  check "modal Command clipboard remains owned by the field" (clipboard (fst (key (V.KChar 'c') [V.MMeta] editorModal))=="draft")
  check "modal Command Option Replace retains search ownership" (maybe False (\dg->case purpose dg of Searching True _->True; _->False) (dialog (fst (key (V.KChar 'f') [V.MMeta,V.MAlt] (fst (runCommand Find mac))))))
  check "remapping replaces every old binding" (bindingAction sourceKeys (V.KFun 2) []==Nothing && bindingAction sourceKeys (V.KChar 's') [V.MCtrl]==Nothing)
  check "modifier order and letter case normalize" (bindingAction sourceKeys (V.KChar 'S') [V.MShift,V.MCtrl]==Just Save)
  check "empty binding lists remain unbound" (null (bindingKeys sourceKeys Open))
  check "unknown command and ambiguous key fail" (rejected [("missing",[])] && rejected [("hide.file.save",["Ctrl+O"])])
  check "unknown modifier and reserved keys fail" (all (\chord->rejected [("hide.file.save",[chord])]) ["Super+S","Ctrl+Ctrl+S","F10","Alt+F","Tab","S","Ctrl+]","Ctrl+\n"])
  check "source invokes the remapped action" (case snd (key (V.KChar 's') [V.MCtrl,V.MShift] source) of [SaveDocument{}]->True; _->False)
  check "unbound source keys cannot fall through to old defaults" (noEffects (key (V.KFun 2) [] source) && noEffects (key (V.KChar 's') [V.MCtrl] source) && noEffects (key (V.KFun 3) [] source))
  check "ordinary source typing remains local" (activeText (fst (key (V.KChar 'x') [] source))=="xhello")
  check "menu and status labels use the effective binding" (menuShortcut source (MenuItem "Save" "F2" Save)=="Ctrl+Shift+S" && any ((==" Ctrl+Shift+S Save").fst) (statusHints source))
  check "daemon clipboard transport does not disable terminal bindings" (boundKeyCommand (V.KChar 's') [V.MCtrl,V.MShift] source {browserFrontend=True}==Just Save)
  let messages=source {problemsVisible=True,problemsFocused=True}
  check "Messages uses its own defaults" (menuShortcut messages (MenuItem "Save" "F2" Save)=="F2")
  let modal=prompt "Question" Information [Input "Name" "" 0] source
  check "source bindings cannot invoke through a modal" (noEffects (key (V.KChar 's') [V.MCtrl,V.MShift] modal))
  let terminal=addReadOnly "Terminal test" "output" source
  check "PTY Ctrl+C still sends interrupt" (snd (key (V.KChar 'c') [V.MCtrl] terminal)==[AgentAction "terminal-input" ["test","\ETX"]])
  check "native frontend retains its own mapping in this stage" (not (noEffects (key (V.KFun 2) [] source {videoMode=Just 3})))
  let rebound=source {keyBindings=prepare [("hide.file.save",[])]}
  before<-renderKey source
  after<-renderKey rebound
  check "prepared binding replacement invalidates labels" (before/=after)
  let popup=openContext SourceContext 8 5 source
  captured<-renderKey popup
  invalidated<-renderKey popup {contextTarget=Just UnavailableSourceTarget}
  check "captured context target changes invalidate menu rendering" (captured/=invalidated)
  let maps=either (error . show) id (platformBindings [] TerminalPlatform (M.fromList
        [("global",M.singleton "hide.options.agent-permissions" ["Alt+P"])
        ,("sidebar",M.fromList [("hide.sidebar.expand",["Ctrl+E"]),("hide.sidebar.down",[]),("hide.options.agent-permissions",["Ctrl+Shift+P"])])
        ,("conversation",M.fromList [("hide.edit.copy",["Ctrl+Shift+J"]),("hide.agents.cancel",["Ctrl+Shift+K"])])
        ,("terminal",M.singleton "hide.terminal.stop" ["Alt+F11"])
        ,("debugger",M.singleton "hide.debug.continue" ["Ctrl+Shift+D"])]))
      contextBase=base {keyBindings=maps}
      tree=installSidebar (emptySidebar "/project" 24 True) contextBase
  check "sidebar expansion uses its own override" (boundKeyCommand (V.KChar 'e') [V.MCtrl] tree==Just SidebarExpand && noEffects (key V.KRight [] tree))
  check "removed sidebar movement stays removed" (maybe (-1) treeSelected (sideTree (fst (key V.KDown [] tree)))==0)
  check "context entry replaces global command chords" (boundKeyCommand (V.KChar 'p') [V.MCtrl,V.MShift] tree==Just AgentPermissions && boundKeyCommand (V.KChar 'p') [V.MAlt] tree==Nothing)
  check "rebound sidebar protected action retains guest policy" (not (guestKeyAllowed tree (V.KChar 'p') [V.MCtrl,V.MShift]))
  let chat=(addReadOnly "Conversation" "reply" contextBase) {composerBuffer=newBuffer "draft",composerSelection=Selection 0 5,composerFocused=True}
  check "conversation copy dispatches its effective chord" (clipboard (fst (key (V.KChar 'j') [V.MCtrl,V.MShift] chat))=="draft" && T.null (clipboard (fst (key (V.KChar 'c') [V.MCtrl] chat))))
  check "conversation commands remain protected after remapping" (not (guestKeyAllowed chat (V.KChar 'k') [V.MCtrl,V.MShift]))
  check "conversation Enter keeps draft submission ownership" (snd (key V.KEnter [] chat)==[AgentAction "send-draft" []])
  let pty=addReadOnly "Terminal test" "output" contextBase
  check "terminal editor actions can be rebound" (snd (key (V.KFun 11) [V.MAlt] pty)==[AgentAction "terminal-stop" []])
  check "terminal control characters retain process ownership" (all (\(c,text)->snd (key (V.KChar c) [V.MCtrl] pty)==[AgentAction "terminal-input" ["test",text]]) [('c',"\ETX"),('q',"\DC1"),('s',"\DC3")])
  check "terminal control chords cannot be assigned to commands" (either (const True) (const False) (platformBindings [] TerminalPlatform (M.singleton "terminal" (M.singleton "hide.terminal.stop" ["Ctrl+C"]))))
  let debug=addReadOnly "Debugger output" "stopped" contextBase
  check "debugger override drives dispatch and menu labels" (snd (key (V.KChar 'd') [V.MCtrl,V.MShift] debug)==[DebugAction "continue" []] && noEffects (key (V.KFun 4) [] debug) && menuShortcut debug (MenuItem "Continue" "F4" (DebugCommand "continue"))=="Ctrl+Shift+D")
  let global=either (error . show) id (platformBindings [] TerminalPlatform (M.fromList [("global",M.singleton "hide.file.save" ["Ctrl+Shift+S","Alt+F11"]),("wordstar",M.singleton "hide.cursor.left" []),("wordstar-quick",M.singleton "hide.cursor.row-start" [])]))
  check "global control overrides apply outside PTYs" (all (\context->bindingAction (global M.! (TerminalPlatform,context)) (V.KChar 's') [V.MCtrl,V.MShift]==Just Save) [SourceKeys,SidebarKeys,ConversationKeys,MessagesKeys,DebuggerKeys])
  check "PTY inherits transferable global chords and omits process controls" (bindingKeys (global M.! (TerminalPlatform,TerminalKeys)) Save==["Alt+F11"] && snd (key (V.KChar 'c') [V.MCtrl] (pty {keyBindings=global}))==[AgentAction "terminal-input" ["test","\ETX"]])
  check "unknown contexts fail instead of disappearing" (either (const True) (const False) (platformBindings [] TerminalPlatform (M.singleton "sidebaar" M.empty)))
  let defaults=either (error . show) id (platformBindings [] TerminalPlatform M.empty)
      compiled d=d {keyBindings=defaults}
      draft=chat {keyBindings=M.empty,composerSelection=Selection 5 5}
      chatState (d,effects)=(contents (composerBuffer d),composerSelection d,composerFocused d,clipboard d,effects)
  check "default conversation text and selection retain owner behavior" (all (\(k,m)->chatState (key k m draft)==chatState (key k m (compiled draft)))
    [(V.KLeft,[V.MShift]),(V.KChar 'a',[V.MCtrl]),(V.KEnter,[V.MShift]),(V.KChar 'x',[]),(V.KChar '\t',[]),(V.KEnter,[V.MCtrl])])
  let selectedDraft=draft {composerSelection=Selection 0 5}
  check "default conversation shifted clipboard keys retain owner behavior" (all (\c->chatState (key (V.KChar c) [V.MCtrl,V.MShift] selectedDraft)==chatState (key (V.KChar c) [V.MCtrl,V.MShift] (compiled selectedDraft))) ['c','x','v'])
  check "default debugger controls retain ownership" (all (\(k,m)->snd (key k m debug {keyBindings=M.empty})==snd (key k m (compiled debug))) [(V.KFun 4,[]),(V.KFun 7,[]),(V.KFun 8,[]),(V.KFun 7,[V.MCtrl])])
  let messagePane=base {problemsVisible=True,problemsFocused=True,diagnostics=[Diagnostic "/project/Main.hs" Nothing 0 0 1 "first",Diagnostic "/project/Main.hs" Nothing 1 0 1 "second"]}
      messageState (d,effects)=(problemsSelected d,problemsScroll d,problemsFocused d,clipboard d,effects)
  check "default Messages selection/copy/jump retain ownership" (all (\(k,m)->messageState (key k m messagePane)==messageState (key k m (compiled messagePane)))
    [(V.KDown,[]),(V.KPageDown,[]),(V.KEnter,[]),(V.KChar 'c',[V.MCtrl]),(V.KFun 4,[]),(V.KFun 6,[])])
  let messageKeys keys=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "messages" (M.singleton "hide.messages.go-to" keys)))
      messageRemap=messagePane {keyBindings=messageKeys ["Ctrl+Shift+J"]}
      messageUnbound=messagePane {keyBindings=messageKeys []}
      clickSourceHint d=case statusItemRects d of
        (Rect x y _ _,_,_):_ -> snd (handleEvent (V.EvMouseDown x y V.BLeft []) d)
        _ -> error "missing Messages source hint"
  check "Messages Source status shares its remapped command and clickable action"
    (take 1 (statusHints messageRemap)==[(" Ctrl+Shift+J Source",Just (Left GoToMessage))] &&
     snd (key (V.KChar 'j') [V.MCtrl,V.MShift] messageRemap)==[JumpTo "/project/Main.hs" 0 0] &&
     noEffects (key V.KEnter [] messageRemap) && clickSourceHint messageRemap==snd (runCommand GoToMessage messageRemap))
  check "unbound Messages Source status retains a semantic click without an old key"
    (take 1 (statusHints messageUnbound)==[(" Source",Just (Left GoToMessage))] &&
     noEffects (key V.KEnter [] messageUnbound) && noEffects (key (V.KChar 'j') [V.MCtrl,V.MShift] messageUnbound) &&
     clickSourceHint messageUnbound==[JumpTo "/project/Main.hs" 0 0])
  forM_ [[],["F13"]] $ \keys->do
    let copyMaps=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "messages" (M.singleton "hide.messages.copy-all" keys)))
        pane=messagePane {keyBindings=copyMaps}
        expected="Error /project/Main.hs:1:1 first\n\nError /project/Main.hs:2:1 second"
        clicked=case [r | (r,_,Left CopyAllMessages)<-statusItemRects pane] of
          r:_->fst (handleEvent (V.EvMouseDown (left r) (top r) V.BLeft []) pane)
          []->error "missing Copy all status action"
    check "Copy all status preserves its caption through remap and unbind"
      (any (\(label,action)->T.strip label==T.unwords (keys++["Copy all"]) && action==Just (Left CopyAllMessages)) (statusHints pane) &&
       clipboard clicked==expected && (null keys || clipboard (fst (key (V.KFun 13) [] pane))==expected))
  check "default PTY input and editor controls retain ownership" (all (\(k,m)->snd (key k m pty {keyBindings=M.empty})==snd (key k m (compiled pty)))
    [(V.KChar 'c',[V.MCtrl]),(V.KChar 'q',[V.MCtrl]),(V.KFun 1,[]),(V.KFun 4,[]),(V.KFun 7,[]),(V.KFun 9,[]),(V.KUp,[])])
  check "resolved reload retains guest origin policy" (not (guestKeyAllowed source {keyBindings=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "source" (M.singleton "hide.bindings.reload" ["Alt+F11"])))} (V.KFun 11) [V.MAlt]))
  let starMaps=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "wordstar" (M.fromList [("hide.file.save",["Ctrl+Shift+J"]),("hide.edit.undo",[])])))
      star=base {wordStar=True,keyBindings=starMaps}
  check "WordStar Save remap dispatches its named command" (case snd (key (V.KChar 'j') [V.MCtrl,V.MShift] star) of [SaveDocument{}]->True; _->False)
  check "WordStar unspecified Control keys retain their old inactive default" (clipboard (fst (key (V.KChar 'c') [V.MCtrl] (modifyActive (\w->w {selection=Selection 0 5}) star)))=="")
  check "WordStar old named command chords stay removed" (noEffects (key (V.KFun 2) [] star) && activeText (fst (key (V.KChar 'z') [V.MCtrl] (fst (key (V.KChar '!' ) [] star))))=="!hello")
  check "WordStar fixed movement retains its input owner" (maybe (-1) (caret . selection) (activeWindow (fst (key (V.KChar 'd') [V.MCtrl] star)))==1)
  check "WordStar prefix block grammar retains its input owner" (prefix (fst (key (V.KChar 'k') [V.MCtrl] star))==Just 'k' && blockStart (fst (key (V.KChar 'b') [] (fst (key (V.KChar 'k') [V.MCtrl] star))))==Just (maybe (-1) sourceFixtureBuffer (activeWindow star),0))
  check "WordStar remap uses effective menu and status labels" (menuShortcut star (MenuItem "Save" "F2" Save)=="Ctrl+Shift+J" && any ((==" Ctrl+Shift+J Save").fst) (statusHints star))
  check "WordStar starter chord conflicts require explicitly releasing its owner" (either (const True) (const False) (platformBindings [] TerminalPlatform (M.singleton "wordstar" (M.singleton "hide.file.save" ["Ctrl+K"]))))
  let dialogMaps=either (error . show) id (configuredBindings [] (M.singleton "macos" (M.singleton "dialog" (M.fromList [("hide.edit.copy",["Cmd+Shift+J"]),("hide.edit.paste",["Cmd+Shift+K"]),("hide.edit.undo",["Cmd+Shift+L"])]))))
      background=modifyActive (\w->w {selection=Selection 0 5}) base {nativeMac=True,videoMode=Just 3,keyBindings=dialogMaps}
      editing=prompt "Edit" Information [TextArea "Text" True (newBuffer "draft") (Selection 0 5) 0 0] background
      copiedDialog=fst (key (V.KChar 'j') [V.MMeta,V.MShift] editing)
      pastedDialog=fst (key (V.KChar 'k') [V.MMeta,V.MShift] editing {clipboard="replacement"})
      dialogText d=case dialog d of Just dg | TextArea _ _ b _ _ _:_<-fields dg -> contents b; _->error "missing text field"
  check "dialog remapped Copy reads the focused field" (clipboard copiedDialog=="draft")
  check "dialog remapped Paste changes only the focused field" (dialogText pastedDialog=="replacement" && activeText pastedDialog=="hello")
  check "dialog remapped Undo applies directly to field history" (dialogText (fst (key (V.KChar 'l') [V.MMeta,V.MShift] pastedDialog))=="draft")
  check "removed dialog clipboard keys do not replay defaults" (clipboard (fst (key (V.KChar 'c') [V.MMeta] editing))=="" && dialogText (fst (key (V.KChar 'v') [V.MMeta] editing {clipboard="replacement"}))=="draft")
  check "dialog native accelerator follows its field's effective remap" (nativeMenuShortcut editing Copy==("j",9) && nativeMenuShortcut editing Save==("",0))
  let guarded=prompt "Guard" Information [TextArea "Text" True (newBuffer "safe") (Selection 0 4) 0 0] background {buffers=error "modal clipboard forced background source"}
  check "dialog focused accelerator projection does not force background buffers" (nativeMenuShortcut guarded Copy==("j",9) && lookup "Cmd+Shift+J" (focusedBindingChords guarded)==Just "hide.edit.copy")
  check "dialog remapped clipboard does not force background buffers" (clipboard (fst (key (V.KChar 'j') [V.MMeta,V.MShift] guarded))=="safe" && dialogText (fst (key (V.KChar 'k') [V.MMeta,V.MShift] guarded {clipboard="replacement"}))=="replacement")
  check "dialog remap cannot admit an agent-owned sensitive control" (not (guestKeyAllowed editing {dialog=fmap (\dg->dg {purpose=AgentDialog "settings"}) (dialog editing)} (V.KChar 'j') [V.MMeta,V.MShift]))
  check "dialog labels and inspection use its effective context" (menuShortcut editing (MenuItem "Copy" "Cmd+C" Copy)=="⇧⌘J" && any ((==" ⇧⌘K Paste").fst) (statusHints editing) && case snd (runCommand InspectBindings editing) of [InspectKeyBindings (Just (MacPlatform,DialogKeys)) (Just table)]->bindingKeys table Copy==["Cmd+Shift+J"]; _->False)
  let buttonCommands=map snd (focusedBindingChords editing {dialog=fmap (\dg->dg {focus=1}) (dialog editing)})
  check "focused dialog buttons project controls without field or source editing"
    (all (`elem` buttonCommands) ["hide.dialog.accept","hide.dialog.cancel"] &&
     not (any (`elem` buttonCommands) ["hide.edit.copy","hide.edit.cut","hide.edit.paste","hide.file.save"]) &&
     not (any ((=="hide.file.save").snd) (focusedBindingChords editing)))
  check "dialog cannot bind a background source command" (either (const True) (const False) (platformBindings [] TerminalPlatform (M.singleton "dialog" (M.singleton "hide.file.save" ["Ctrl+Shift+J"]))))
  check "dialog field text and navigation retain their owner" (dialogText (fst (key (V.KChar 'ø') [] editing))=="ø" && dialog (fst (key V.KEsc [] editing))==Nothing)
  let dialogDefaults=either (error . show) id (configuredBindings [] M.empty)
      searchEditing=fst (runCommand Find background {keyBindings=dialogDefaults})
  check "default dialog clipboard matches field-owned behavior" (clipboard (fst (key (V.KChar 'c') [V.MCtrl] editing {nativeMac=False,keyBindings=dialogDefaults}))=="draft")
  check "unavailable dialog editing action retains Input button mnemonic ownership" (dialog (fst (key (V.KChar 'c') [V.MCtrl] searchEditing))==Nothing)
  check "default dialog search retains permitted replacement action" (maybe False (\dg->case purpose dg of Searching True _->True; _->False) (dialog (fst (key (V.KChar 'f') [V.MMeta,V.MAlt] searchEditing))))
  dialogControlChecks
  dialogFocusChecks
  horizontalChecks
  verticalChecks
  edgePageChecks
  wordChecks
  windowChecks
  reloadChecks
  putStrLn "keybinding checks passed"

windowChecks :: IO ()
windowChecks=do
  let check name ok=unless ok (error name)
      compile entries=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "global" (M.fromList entries)))
      defaults=compile []
      remapped=compile [("hide.window.next",["F13"]),("hide.window.previous",["F14"])]
      removed=compile [("hide.window.next",[]),("hide.window.previous",[])]
      first=addDocument Nothing (newBuffer "first") (initialDesktop (80,25))
      second=addDocument Nothing (newBuffer "second") first
      base=modifyActive (\w->w {selection=Selection 1 3}) (addDocument Nothing (newBuffer "third") second)
      current d=windowId <$> activeWindow d
      event k mods=handleEvent (V.EvKey k mods)
      step k mods=fst . event k mods
      state d=(current d,selection <$> activeWindow d,(\doc->(revision (documentBuffer doc),bufferLength (documentBuffer doc))) <$> activeDocument d)
      aliases=[(V.KChar '\t',[V.MCtrl]),(V.KChar '\t',[V.MCtrl,V.MShift]),(V.KBackTab,[V.MCtrl])]
      configured=base {keyBindings=remapped}
  check "window defaults cycle both directions through stable IDs"
    (current (step (V.KChar '\t') [V.MCtrl] base {keyBindings=defaults})==current second &&
     current (step (V.KChar '\t') [V.MCtrl,V.MShift] base {keyBindings=defaults})==current first &&
     current (step V.KBackTab [V.MCtrl] base {keyBindings=defaults})==current first)
  check "window remaps use the existing cycle owner"
    (current (step (V.KFun 13) [] configured)==current second && current (step (V.KFun 14) [] configured)==current first)
  forM_ [configured,base {keyBindings=removed},configured {wordStar=True}] $ \d->
    check "removed window aliases neither cycle nor edit source"
      (all (\(k,mods)->state (step k mods d)==state d && null (snd (event k mods d))) aliases)
  let terminal=(addReadOnly "Terminal test" "output" base) {keyBindings=removed}
      chat=(addReadOnly "Conversation" "reply" base) {keyBindings=removed,composerBuffer=newBuffer "draft",composerSelection=Selection 1 3,composerFocused=True}
  check "unbound PTY window aliases send no bytes"
    (all (\(k,mods)->current (step k mods terminal)==current terminal && null (snd (event k mods terminal))) aliases &&
     snd (event (V.KChar 'c') [V.MCtrl] terminal)==[AgentAction "terminal-input" ["test","\ETX"]])
  check "unbound chat window aliases preserve the draft selection"
    (all (\(k,mods)->let (d,effects)=event k mods chat in current d==current chat && composerSelection d==composerSelection chat && revision (composerBuffer d)==revision (composerBuffer chat) && bufferLength (composerBuffer d)==bufferLength (composerBuffer chat) && null effects) aliases)
  let tree=installSidebar (emptySidebar "/project" 24 True) base {keyBindings=defaults}
      messages=base {keyBindings=defaults,problemsVisible=True,problemsFocused=True}
      modal=fst (runCommand Find base {keyBindings=defaults})
  check "Files and Messages retain F6 focus with configurable window cycling"
    (all (\d->boundKeyCommand (V.KFun 6) [] d==Just FocusSource && current (step (V.KChar '\t') [V.MCtrl] d)==current second) [tree,messages])
  check "modal Control Tab retains search ownership"
    (current (step (V.KChar '\t') [V.MCtrl] modal)==current modal && maybe False (\dg->case purpose dg of Searching True _->True; _->False) (dialog (step (V.KChar '\t') [V.MCtrl] modal)))
  check "Alt and Meta Tab retain their earlier owners"
    (menu (step (V.KChar '\t') [V.MCtrl,V.MAlt] configured)==Just (0,0) && state (step (V.KChar '\t') [V.MMeta] configured)==state (step (V.KChar '\t') [V.MMeta] base))
  let private=base {keyBindings=remapped,guestPrivatePaths=["/authority"],buffers=M.map (\doc->doc {documentFile=Just (FileState "/authority/secret.hs" Nothing)}) (buffers base)}
      rebound=private {keyBindings=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "source" (M.fromList [("hide.window.next",[]),("hide.edit.copy",["Ctrl+Tab"])])))}
  navigation<-P.applyGuestInput (P.Key "F13" []) private
  copy<-P.applyGuestInput (P.Key "Tab" [V.MCtrl]) rebound
  check "protected-view navigation follows the resolved command"
    (case navigation of Right (d,_)->current d==current second; _->False)
  check "physical Control Tab cannot grant a protected non-navigation remap"
    (case copy of Left _->True; _->False)

reloadChecks :: IO ()
reloadChecks=bracket temporary removePathForcibly $ \directory->do
  old<-lookupEnv "XDG_CONFIG_HOME"
  let restore=maybe (unsetEnv "XDG_CONFIG_HOME") (setEnv "XDG_CONFIG_HOME") old
  bracket_ (setEnv "XDG_CONFIG_HOME" directory) restore $ withKeybindings M.empty [] $ \runtime->do
    let path=directory </> "thc.toml"
        defaults=either (error . show) id (platformBindings [] TerminalPlatform M.empty)
        base=(addDocument (Just (FileState (directory </> "Main.hs") Nothing)) (newBuffer "text") (initialDesktop (80,25))) {keyBindings=defaults}
        fallback d _=pure (False,d)
        start command d=let (requested,effects)=runCommand command d in snd <$> keybindingEffects runtime fallback requested effects
        settle done d=do
          updated<-tickKeybindings runtime d
          if done updated then pure updated else threadDelay 1000 >> settle done updated
        await done d=timeout 5000000 (settle done d) >>= maybe (error "keybinding worker did not finish") pure
        reloaded d=status d/="Reloading keybindings..."
    TIO.writeFile path "[editor.keybindings.terminal.source]\n\"hide.file.save\" = [\"Ctrl+Shift+S\"]\n\"hide.file.open\" = []\n"
    pending<-start ReloadBindings base
    unless (boundKeyCommand (V.KChar 's') [V.MCtrl] pending==Just Save) (error "old bindings remain active while reload is pending")
    loaded<-await reloaded pending
    unless (boundKeyCommand (V.KChar 's') [V.MCtrl,V.MShift] loaded==Just Save && boundKeyCommand (V.KFun 3) [] loaded==Nothing) (error "successful reload publishes the prepared context map")
    inspectedPending<-start InspectBindings loaded
    inspected<-await ((>nextId loaded).nextId) inspectedPending
    unless ("hide.file.save = Ctrl+Shift+S" `T.isInfixOf` activeText inspected && "hide.file.open = []" `T.isInfixOf` activeText inspected) (error "inspection reports effective chords and explicit unbinding")
    TIO.writeFile path "[editor.keybindings.terminal.source]\n\"hide.file.save\" = \"Ctrl+S\"\n"
    invalidPending<-start ReloadBindings loaded
    invalid<-await reloaded invalidPending
    unless ("Keybindings:" `T.isPrefixOf` status invalid && boundKeyCommand (V.KChar 's') [V.MCtrl,V.MShift] invalid==Just Save) (error "failed reload retains the old map and reports its error")
  where
    temporary=do
      parent<-getTemporaryDirectory
      (path,handle)<-openTempFile parent "hide-bindings"
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path

horizontalChecks :: IO ()
horizontalChecks=do
  let check label ok=unless ok (error label)
      compile entries=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "wordstar" (M.fromList entries)))
      base=modifyActive (\w->w {selection=Selection 2 2}) (addDocument Nothing (newBuffer "abc\ndef\n") (initialDesktop (80,25)))
      configured=base {wordStar=True,keyBindings=compile [("hide.cursor.left",["Ctrl+Shift+J"]),("hide.selection.left",["Ctrl+Shift+I"]),("hide.edit.delete-line",["Ctrl+Shift+U"])]}
      event key mods d=fst (handleEvent (V.EvKey key mods) d)
      range d=selection <$> activeWindow d
  check "horizontal remap invokes its semantic cursor owner" (range (event (V.KChar 'j') [V.MCtrl,V.MShift] configured)==Just (Selection 1 1))
  check "horizontal remap consumes the removed WordStar and arrow keys" (all (\(key,mods)->range (event key mods configured)==Just (Selection 2 2)) [(V.KLeft,[]),(V.KChar 's',[V.MCtrl]),(V.KChar 's',[V.MCtrl,V.MShift])])
  check "selection remap preserves its anchor" (range (event (V.KChar 'i') [V.MCtrl,V.MShift] configured)==Just (Selection 2 1))
  let deleted=event (V.KChar 'u') [V.MCtrl,V.MShift] configured
  check "remapped line deletion uses one existing undo operation" (activeText deleted=="def\n" && activeText (fst (runCommand Undo deleted))=="abc\ndef\n")
  let unbound=base {wordStar=True,keyBindings=compile [("hide.cursor.left",[]),("hide.selection.left",[]),("hide.edit.delete-backward",[]),("hide.edit.delete-line",[])]}
  check "unbound horizontal editing cannot replay physical defaults" (all (\(key,mods)->let d=event key mods unbound in range d==Just (Selection 2 2) && activeText d=="abc\ndef\n") [(V.KLeft,[]),(V.KLeft,[V.MShift]),(V.KBS,[]),(V.KChar 's',[V.MCtrl]),(V.KChar 'y',[V.MCtrl])])
  let defaults=either (error . show) id (configuredBindings [] M.empty)
      position text p=modifyActive (\w->w {selection=Selection p p}) (addDocument Nothing (newBuffer text) (initialDesktop (80,25)))
      unicode=position "a界e\x301\&z\r\n" 4
  check "horizontal commands preserve complete combining boundaries" (range (event V.KLeft [] unicode {keyBindings=defaults})==Just (Selection 2 2) && activeText (event V.KBS [] unicode {keyBindings=defaults})=="a界z\r\n")
  check "selection extension and reverse selections keep the original anchor" (range (event V.KRight [V.MShift] base {keyBindings=defaults})==Just (Selection 2 3) && range (event V.KLeft [] (modifyActive (\w->w {selection=Selection 3 1}) base {keyBindings=defaults}))==Just (Selection 0 0))
  check "empty and end-of-file editing stays bounded" (range (event V.KLeft [] (position "" 0) {keyBindings=defaults})==Just (Selection 0 0) && activeText (event V.KDel [] (position "a" 1) {keyBindings=defaults})=="a")
  let star=base {wordStar=True,keyBindings=defaults}
  check "default shifted WordStar movement retains its non-extending semantics" (all (\(c,expected)->range (event (V.KChar c) [V.MCtrl,V.MShift] star)==Just (Selection expected expected)) [('s',1),('d',3)])
  check "default shifted WordStar line deletion retains its owner" (activeText (event (V.KChar 'y') [V.MCtrl,V.MShift] star)=="def\n")
  let selected=modifyActive (\w->w {selection=Selection 1 3}) base {keyBindings=defaults}
  check "adjacent deletion removes a selection first and undo restores it" (all (\k->let edited=event k [] selected in activeText edited=="a\ndef\n" && activeText (fst (runCommand Undo edited))=="abc\ndef\n") [V.KBS,V.KDel])
  let sourceRemap=base {keyBindings=either (error . show) id (configuredBindings [] (M.singleton "macos" (M.singleton "source" (M.singleton "hide.cursor.left" ["Cmd+Shift+J"])))),nativeMac=True,videoMode=Just 3}
  check "source platform remap shares frontend projection and labels" (range (event (V.KChar 'j') [V.MMeta,V.MShift] sourceRemap)==Just (Selection 1 1) && lookup "Cmd+Shift+J" (focusedBindingChords sourceRemap)==Just "hide.cursor.left" && commandBindingKeys sourceRemap (CursorLeft False)==["Cmd+Shift+J"])
  let modal=prompt "Edit" Information [Input "Name" "draft" 0] configured
      private=configured {guestPrivatePaths=["/authority"],buffers=M.map (\doc->doc {documentFile=Just (FileState "/authority/secret.hs" Nothing)}) (buffers configured)}
      sidebar=installSidebar (emptySidebar "/project" 24 True) configured
  check "rebound source operations keep modal private and sidebar authority" (range (event (V.KChar 'j') [V.MCtrl,V.MShift] modal)==Just (Selection 2 2) && not (guestKeyAllowed private (V.KChar 'j') [V.MCtrl,V.MShift]) && not (commandEnabled sidebar (CursorLeft False)) && range (fst (runCommand DeleteLine sidebar))==Just (Selection 2 2))
  check "global overrides must explicitly release new WordStar chord owners" (either (const True) (const False) (platformBindings [] TerminalPlatform (M.singleton "global" (M.singleton "hide.file.save" ["Ctrl+Shift+S"]))))
  let viewMaps=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "source" (M.fromList [("hide.cursor.left",[]),("hide.selection.left",[]),("hide.cursor.right",["Ctrl+Shift+J"])])))
      hex=modifyActive (\w->w {selection=Selection 2 2}) (addDocument Nothing (newByteBuffer (BS.pack [65,13,10,66])) (initialDesktop (80,25))) {keyBindings=defaults}
  check "hex horizontal commands keep one-byte boundaries" (range (event V.KLeft [] hex)==Just (Selection 1 1) && range (event V.KRight [] hex)==Just (Selection 3 3) && (bufferBytes . documentBuffer <$> activeDocument (event V.KBS [] hex))==Just (BS.pack [65,10,66]))
  check "delete line is unavailable in hex" (not (commandEnabled hex DeleteLine) && activeText (fst (runCommand DeleteLine hex))==activeText hex)
  W.withWindowScope $ \scope->do
    prepared<-W.prepareTextWindow "Horizontal notes" "a界e\x301\&z"
    update<-W.openTextWindow scope prepared >>= maybe (fail "plugin open failed") pure
    opened<-adoptWindowUpdate P.HumanMenu update base
    let plugin=modifyActive (\w->w {selection=Selection 2 2}) opened {keyBindings=viewMaps}
    check "plugin unbinding consumes physical horizontal fallback" (range (event V.KLeft [] plugin)==Just (Selection 2 2) && range (event V.KLeft [V.MShift] plugin)==Just (Selection 2 2))
    check "plugin remap navigates prepared text and never its background source" (range (event (V.KChar 'j') [V.MCtrl,V.MShift] plugin)==Just (Selection 4 4) && not (commandEnabled plugin DeleteBackward))
  let mdSource=addDocument (Just (FileState "/tmp/horizontal.md" Nothing)) (newBuffer "*a界e\x301\&z*\n") (initialDesktop (80,25))
  ready<-prepareTextPresentations (fst (runCommand (SetBufferView MarkdownView) mdSource))
  let markdown=modifyActive (modifyDisplayedWindow (\w->w {selection=Selection 2 2})) ready {keyBindings=viewMaps}
      renderedRange d=selection . displayWindow <$> activeWindow d
  check "Markdown unbinding consumes physical horizontal fallback" (renderedRange (event V.KLeft [] markdown)==Just (Selection 2 2) && renderedRange (event V.KLeft [V.MShift] markdown)==Just (Selection 2 2))
  check "Markdown remap uses rendered coordinates without moving source selection" (renderedRange (event (V.KChar 'j') [V.MCtrl,V.MShift] markdown)==Just (Selection 3 3) && range (event (V.KChar 'j') [V.MCtrl,V.MShift] markdown)==range markdown && not (commandEnabled markdown DeleteBackward))

verticalChecks :: IO ()
verticalChecks=do
  let check name ok=unless ok (error name)
      event k mods=fst . handleEvent (V.EvKey k mods)
      range d=selection <$> activeWindow d
      defaults=either (error . show) id (configuredBindings [] M.empty)
      compile context entries=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton context (M.fromList entries)))
      base=modifyActive (\w->w {selection=Selection 8 8}) (addDocument Nothing (newBuffer "ab界e\x301\&z\nx\nab界e\x301\&z\n") (initialDesktop (80,25)))
      star=base {wordStar=True,keyBindings=defaults}
      configured=star {keyBindings=compile "wordstar" [("hide.cursor.up",["Ctrl+Shift+J"]),("hide.selection.up",["Ctrl+Shift+I"])]}
      unbound=star {keyBindings=compile "wordstar" [("hide.cursor.up",[]),("hide.selection.up",[])]}
  check "WordStar vertical remap executes direct movement and removes former chords"
    (range (event (V.KChar 'j') [V.MCtrl,V.MShift] configured)==Just (Selection 1 1) && all (\(k,mods)->range (event k mods configured)==range configured)
      [(V.KUp,[]),(V.KUp,[V.MCtrl]),(V.KChar 'e',[V.MCtrl]),(V.KChar 'e',[V.MCtrl,V.MShift])])
  check "vertical selection remap preserves the anchor" (range (event (V.KChar 'i') [V.MCtrl,V.MShift] configured)==Just (Selection 8 1))
  check "explicit vertical unbind consumes arrow and WordStar fallbacks" (all (\(k,mods)->range (event k mods unbound)==range unbound)
    [(V.KUp,[]),(V.KUp,[V.MShift]),(V.KUp,[V.MCtrl,V.MAlt]),(V.KChar 'e',[V.MCtrl]),(V.KChar 'e',[V.MCtrl,V.MShift])])
  check "shifted WordStar vertical aliases remain non-extending" (range (event (V.KChar 'e') [V.MCtrl,V.MShift] star)==Just (Selection 1 1) && range (event (V.KChar 'x') [V.MCtrl,V.MShift] star)==Just (Selection 10 10))
  check "WordStar earlier Alt owners retain Edit and Quit" (menu (event (V.KChar 'e') [V.MCtrl,V.MAlt] star)==Just (1,0) && snd (handleEvent (V.EvKey (V.KChar 'x') [V.MCtrl,V.MAlt]) star)==[Exit])
  let first=modifyActive (\w->w {selection=Selection 5 5}) base {keyBindings=defaults}
      down=event V.KDown [] first
  check "vertical motion retains short-row column reset and grapheme cells" (range down==Just (Selection 8 8) && range (event V.KDown [] down)==Just (Selection 10 10) && range (event V.KUp [V.MShift] down)==Just (Selection 8 1))
  let top=modifyActive (\w->w {selection=Selection 0 0}) first
      empty=addDocument Nothing (newBuffer "") (initialDesktop (80,25)) {keyBindings=defaults}
  check "vertical endpoints remain bounded without edits" (range (event V.KUp [] top)==Just (Selection 0 0) && range (event V.KDown [] empty)==Just (Selection 0 0) && (revision . documentBuffer <$> activeDocument down)==Just 0)
  let sourceMaps=compile "source" [("hide.cursor.up",[]),("hide.selection.up",[]),("hide.cursor.down",["Ctrl+Shift+J"])]
      hex=modifyActive (\w->w {selection=Selection 17 17}) (addDocument Nothing (newByteBuffer (BS.pack [0..63])) (initialDesktop (80,25))) {keyBindings=sourceMaps}
      stride=maybe 0 windowHexBytes (activeWindow hex)
  check "hex vertical remap keeps the existing byte-row stride" (range (event (V.KChar 'j') [V.MCtrl,V.MShift] hex)==Just (Selection (17+stride) (17+stride)) && range (event V.KUp [] hex)==range hex)
  let modal=prompt "Edit" Information [Input "Name" "draft" 0] configured
      private=configured {guestPrivatePaths=["/authority"],buffers=M.map (\doc->doc {documentFile=Just (FileState "/authority/secret.hs" Nothing)}) (buffers configured)}
      sidebar=installSidebar (emptySidebar "/project" 24 True) configured
      messages=configured {problemsFocused=True,problemsVisible=True}
  check "vertical remaps keep modal private sidebar and Messages owners" (range (event (V.KChar 'j') [V.MCtrl,V.MShift] modal)==range modal && not (guestKeyAllowed private (V.KChar 'j') [V.MCtrl,V.MShift]) && all (\d->not (commandEnabled d (CursorUp False)) && range (fst (runCommand (CursorDown False) d))==range d) [sidebar,messages])
  W.withWindowScope $ \scope->do
    prepared<-W.prepareTextWindow "Vertical notes" "ab界e\x301\&z\nx\nab界e\x301\&z"
    update<-W.openTextWindow scope prepared >>= maybe (fail "plugin open failed") pure
    opened<-adoptWindowUpdate P.HumanMenu update base
    let plugin=modifyActive (\w->w {selection=Selection 8 8}) opened {keyBindings=sourceMaps}
    check "plugin vertical remap and unbind use only prepared text" (range (event V.KUp [] plugin)==range plugin && range (event V.KUp [V.MShift] plugin)==range plugin && range (event (V.KChar 'j') [V.MCtrl,V.MShift] plugin)==Just (Selection 10 10) && activeText (event (V.KChar 'j') [V.MCtrl,V.MShift] plugin)==activeText plugin)
  let mdSource=addDocument (Just (FileState "/tmp/vertical.md" Nothing)) (newBuffer "abcd\n\nx\n\nabcdef\n") (initialDesktop (80,25))
  ready<-prepareTextPresentations (fst (runCommand (SetBufferView MarkdownView) mdSource))
  let markdown=modifyActive (modifyDisplayedWindow (\w->w {selection=Selection 0 0})) ready {keyBindings=sourceMaps}
      renderedRange d=selection . displayWindow <$> activeWindow d
      moved=event (V.KChar 'j') [V.MCtrl,V.MShift] markdown
  check "Markdown vertical remap owns rendered selection without source motion" (renderedRange moved/=renderedRange markdown && range moved==range markdown && renderedRange (event V.KUp [] moved)==renderedRange moved && renderedRange (event V.KUp [V.MShift] moved)==renderedRange moved)
  let mac=base {nativeMac=True,videoMode=Just 3,keyBindings=either (error . show) id (configuredBindings [] (M.singleton "macos" (M.singleton "source" (M.singleton "hide.cursor.up" ["Cmd+Shift+J"]))))}
  check "vertical platform remap projects the same identity and label" (range (event (V.KChar 'j') [V.MMeta,V.MShift] mac)==Just (Selection 1 1) && lookup "Cmd+Shift+J" (focusedBindingChords mac)==Just "hide.cursor.up" && commandBindingKeys mac (CursorUp False)==["Cmd+Shift+J"])

edgePageChecks :: IO ()
edgePageChecks=do
  let check name ok=unless ok (error name)
      event k mods=fst . handleEvent (V.EvKey k mods)
      range d=selection <$> activeWindow d
      renderedRange d=selection . displayWindow <$> activeWindow d
      narrow=modifyActive (\w->w {bounds=(bounds w) {height=12,width=34}})
      line="abcd界e\x301\&z\tmore"
      text=T.replicate 60 (line<>"\r\n")
      stride=T.length line+2
      pos=20*stride+2
      base=narrow (modifyActive (\w->w {selection=Selection (pos+4) pos}) (addDocument Nothing (newBuffer text) (initialDesktop (80,25))))
      operations=[("row-start",CursorRowStart,20*stride),("row-end",CursorRowEnd,20*stride+T.length line),
        ("document-start",CursorDocumentStart,0),("document-end",CursorDocumentEnd,T.length text),
        ("page-up",CursorPageUp,11*stride+2),("page-down",CursorPageDown,29*stride+2)]
      actions=[("hide."<>space<>"."<>name,command extend,target,extend) | (name,command,target)<-operations,(space,extend)<-[("cursor",False),("selection",True)]]
      overrides=[(name,["F"<>T.pack (show n)]) | ((name,_,_,_),n)<-zip actions [13::Int ..]]
      compile context entries=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton context (M.fromList entries)))
      maps=compile "source" overrides
      configured=base {keyBindings=maps}
      unbound=base {keyBindings=compile "source" [(name,[]) | (name,_,_,_)<-actions]}
      originalKeys=[(k,mods) | k<-[V.KHome,V.KEnd,V.KPageUp,V.KPageDown],mods<-subsequences [V.MCtrl,V.MAlt,V.MShift]]
      expected target extend=Selection (if extend then pos+4 else target) target
  forM_ (zip actions [13::Int ..]) $ \((name,command,target,extend),n)->do
    let moved=event (V.KFun n) [] configured
    check "edge/page remaps execute semantic source positions" (range moved==Just (expected target extend) && commandIdentifier command==Just name && (revision . documentBuffer <$> activeDocument moved)==Just 0)
    check "edge/page labels and frontend projection use the effective binding" (commandBindingKeys configured command==["F"<>T.pack (show n)] && menuShortcut configured (MenuItem "Edge" "Home" command)=="F"<>T.pack (show n) && lookup ("F"<>T.pack (show n)) (focusedBindingChords configured)==Just name)
  check "edge/page replacement consumes every former modifier alias" (all (\(k,mods)->range (event k mods configured)==range configured) originalKeys)
  check "explicit edge/page unbind consumes physical fallback" (all (\(k,mods)->range (event k mods unbound)==range unbound) originalKeys)
  let star=base {wordStar=True,keyBindings=compile "wordstar" [(name,[]) | (name,_,_,_)<-actions]}
      prefixStep c=event (V.KChar c) [] (event (V.KChar 'q') [V.MCtrl] star)
  check "WordStar quick defaults are independent non-extending semantic steps" (map (range . prefixStep) ['s','d','r','c']==map (Just . (\p->Selection p p)) [20*stride,20*stride+T.length line,0,T.length text])
  let hex=narrow (modifyActive (\w->w {selection=Selection 17 17}) (addDocument Nothing (newByteBuffer (BS.pack (take 1600 (cycle [0..255])))) (initialDesktop (80,25)))) {keyBindings=maps}
      count=maybe 0 windowHexBytes (activeWindow hex)
  check "hex remapped End keeps last byte and document End keeps insertion EOF" (range (event (V.KFun 15) [] hex)==Just (Selection (17-17 `mod` count+count-1) (17-17 `mod` count+count-1)) && range (event (V.KFun 19) [] hex)==Just (Selection 1600 1600))
  check "hex remapped page keeps measured byte-row stride" (range (event (V.KFun 23) [] hex)==Just (Selection (17+9*count) (17+9*count)))
  W.withWindowScope $ \scope->do
    prepared<-W.prepareTextWindow "Edge notes" text
    update<-W.openTextWindow scope prepared >>= maybe (fail "plugin edge open failed") pure
    opened<-adoptWindowUpdate P.HumanMenu update base
    let plugin=modifyActive (\w->w {selection=Selection (pos+4) pos}) (narrow opened) {keyBindings=maps}
        pluginUnbound=plugin {keyBindings=keyBindings unbound}
    check "plugin edge unbind consumes readonly fallback" (all (\(k,mods)->range (event k mods pluginUnbound)==range pluginUnbound) originalKeys)
    check "plugin page reserves its existing two chrome rows" (range (event (V.KFun 23) [] plugin)==Just (Selection (30*stride+2) (30*stride+2)))
    check "plugin document selection uses its prepared content" (range (event (V.KFun 20) [] plugin)==Just (Selection (pos+4) (T.length text)))
  let mdSource=narrow (addDocument (Just (FileState "/tmp/edge.md" Nothing)) (newBuffer (T.replicate 60 "abcd界e\x301\&z\n\n")) (initialDesktop (80,25)))
      pending=(fst (runCommand (SetBufferView MarkdownView) mdSource)) {keyBindings=maps}
      inert d=all (\(_,command,_,_)->not (commandEnabled d command) && renderedRange (fst (runCommand command d))==renderedRange d && range (fst (runCommand command d))==range d) actions
  check "pending Markdown edges and pages remain unavailable" (inert pending)
  ready<-prepareTextPresentations pending
  let Just (_,rendered,_)=activeWindow ready >>= windowMarkdown ready
      mdPos=contentLineOffset rendered 20+2
      markdown=modifyActive (modifyDisplayedWindow (\w->w {selection=Selection (mdPos+4) mdPos})) ready
      markdownUnbound=markdown {keyBindings=keyBindings unbound}
      down=event (V.KFun 23) [] markdown
      stale=modifyActive (\w->w {bounds=(bounds w) {width=28}}) markdown
  check "Markdown edge unbind consumes readonly fallback" (all (\(k,mods)->renderedRange (event k mods markdownUnbound)==renderedRange markdownUnbound) originalKeys)
  check "Markdown page keeps rendered ten-row geometry without source motion" (renderedRange down==Just (Selection (contentLineOffset rendered 30+2) (contentLineOffset rendered 30+2)) && range down==range markdown)
  check "stale Markdown edges and pages remain unavailable" (inert stale)
  let modal=prompt "Edit" Information [Input "Name" "draft" 0] configured
      private=configured {guestPrivatePaths=["/authority"],buffers=M.map (\doc->doc {documentFile=Just (FileState "/authority/secret.hs" Nothing)}) (buffers configured)}
      sidebar=installSidebar (emptySidebar "/project" 24 True) configured
      messages=configured {problemsFocused=True,problemsVisible=True}
      global=base {keyBindings=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "global" (M.singleton "hide.cursor.document-end" ["F13"])))}
      chat=(addReadOnly "Conversation" "reply" global) {composerBuffer=newBuffer "draft",composerSelection=Selection 2 2,composerFocused=True}
      pty=addReadOnly "Terminal test" "output" global
  check "edge remap respects private source policy" (not (guestKeyAllowed private (V.KFun 13) []))
  check "edge global remap cannot move behind other input owners" (all (\d->range (event (V.KFun 13) [] d)==range d && not (commandEnabled d (CursorDocumentEnd False))) [modal,sidebar {keyBindings=keyBindings global},messages {keyBindings=keyBindings global},chat,pty])
  check "global source remap cannot change the composer selection" (composerSelection (event (V.KFun 13) [] chat)==composerSelection chat)
  check "PTY edges and pages retain transport ownership" (map (\(k,mods)->terminalInput (V.EvKey k mods)) [(V.KHome,[]),(V.KEnd,[V.MCtrl]),(V.KPageUp,[]),(V.KPageDown,[])]==map Just ["\ESC[H","\ESC[1;5F","\ESC[5~","\ESC[6~"])
  let conflict=platformBindings [] TerminalPlatform (M.singleton "source" (M.singleton "hide.file.save" ["Home"]))
  check "edge chord reassignment must explicitly release its owner" (either (const True) (const False) conflict)

wordChecks :: IO ()
wordChecks=do
  let check name ok=unless ok (error name)
      event k mods=fst . handleEvent (V.EvKey k mods)
      range d=selection <$> activeWindow d
      renderedRange d=selection . displayWindow <$> activeWindow d
      text="one  two,three\r\n界 e\x301\& \tend\n"
      base=modifyActive (\w->w {selection=Selection 7 7}) (addDocument Nothing (newBuffer text) (initialDesktop (80,25)))
      actions=[("hide.cursor.word-left",CursorWordLeft False),("hide.cursor.word-right",CursorWordRight False),
        ("hide.selection.word-left",CursorWordLeft True),("hide.selection.word-right",CursorWordRight True),
        ("hide.edit.delete-word-backward",DeleteWordBackward),("hide.edit.delete-word-forward",DeleteWordForward)]
      compile context entries=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton context (M.fromList entries)))
      maps=compile "source" [(name,["F"<>T.pack (show n)]) | ((name,_),n)<-zip actions [13::Int ..]]
      configured=base {keyBindings=maps}
      unbound=base {keyBindings=compile "source" [(name,[]) | (name,_)<-actions]}
      state d=(range d,(\doc->(revision (documentBuffer doc),bufferLength (documentBuffer doc))) <$> activeDocument d)
      sourceWordKeys=[(k,mods) | k<-[V.KLeft,V.KRight,V.KBS,V.KDel],extra<-subsequences [V.MAlt,V.MShift],let mods=V.MCtrl:extra,not (terminalSourceReserved k mods)]
      docBuffer d=maybe (error "word source missing") documentBuffer (activeDocument d)
  check "word remaps execute measured scalar boundaries" (range (event (V.KFun 13) [] configured)==Just (Selection 5 5) && range (event (V.KFun 14) [] configured)==Just (Selection 8 8))
  check "word selection remaps retain reversed anchors" (map (\n->range (event (V.KFun n) [] (modifyActive (\w->w {selection=Selection 10 7}) configured))) [15,16]==map Just [Selection 10 5,Selection 10 8])
  forM_ (zip actions [13::Int ..]) $ \((name,command),n)->
    check "word labels and projections resolve effective identities" (commandIdentifier command==Just name && commandBindingKeys configured command==["F"<>T.pack (show n)] && menuShortcut configured (MenuItem "Word" "Ctrl+Left" command)=="F"<>T.pack (show n) && lookup ("F"<>T.pack (show n)) (focusedBindingChords configured)==Just name)
  check "word replacement consumes former physical aliases" (all (\(k,mods)->state (event k mods configured)==state configured) sourceWordKeys)
  check "word unbind consumes former physical aliases" (all (\(k,mods)->state (event k mods unbound)==state unbound) sourceWordKeys)
  let backward=event (V.KFun 17) [] configured
      forward=event (V.KFun 18) [] configured
      restored=fst (runCommand Undo backward)
      selected=event (V.KFun 18) [] (modifyActive (\w->w {selection=Selection 10 7}) configured)
  check "word deletion keeps independent local fragments" (bufferSlice (docBuffer backward) 3 5=="  o,t" && bufferLength (docBuffer backward)==T.length text-2 && bufferSlice (docBuffer forward) 3 5=="  tw," && bufferLength (docBuffer forward)==T.length text-1)
  check "word deletion makes one undoable edit" (revision (docBuffer backward)==1 && bufferLength (docBuffer restored)==T.length text && bufferSlice (docBuffer restored) 3 6=="  two,")
  check "word deletion prioritizes the selected range" (bufferLength (docBuffer selected)==T.length text-3 && range selected==Just (Selection 7 7))
  forM_ [(10,9,16),(14,9,15),(19,18,20),(20,19,21)] $ \(p,left,right)->do
    let placed=modifyActive (\w->w {selection=Selection p p}) configured
    check "word boundaries preserve punctuation CRLF and Unicode scalar policy" (range (event (V.KFun 13) [] placed)==Just (Selection left left) && range (event (V.KFun 14) [] placed)==Just (Selection right right))
  let starMaps=compile "wordstar" [(name,["F"<>T.pack (show n)]) | ((name,_),n)<-zip actions [13::Int ..]]
      star=base {wordStar=True,keyBindings=starMaps}
      aliases=[(V.KChar 'a',[V.MCtrl]),(V.KChar 'a',[V.MCtrl,V.MShift]),(V.KChar 'a',[V.MCtrl,V.MAlt]),(V.KChar 'a',[V.MCtrl,V.MAlt,V.MShift]),(V.KChar 'f',[V.MCtrl]),(V.KChar 'f',[V.MCtrl,V.MShift])]
      starUnbound=star {keyBindings=compile "wordstar" [(name,[]) | (name,_)<-actions]}
      starDefaults=base {wordStar=True,keyBindings=compile "wordstar" []}
  check "WordStar word remaps consume former A F aliases" (range (event (V.KFun 13) [] star)==Just (Selection 5 5) && range (event (V.KFun 14) [] star)==Just (Selection 8 8) && all (\(k,mods)->state (event k mods star)==state star) aliases)
  check "WordStar word unbinding consumes A F and arrow aliases" (all (\(k,mods)->state (event k mods starUnbound)==state starUnbound) (aliases++sourceWordKeys))
  check "WordStar default shifted A F aliases remain non-extending" (map (\c->range (event (V.KChar c) [V.MCtrl,V.MShift] starDefaults)) ['a','f']==map Just [Selection 5 5,Selection 8 8])
  check "WordStar A F can be assigned through the existing chord parser" (range (event (V.KChar 'f') [V.MCtrl] star {keyBindings=compile "wordstar" [("hide.cursor.word-left",["Ctrl+F"]),("hide.cursor.word-right",["Ctrl+A"])]})==Just (Selection 5 5))
  check "WordStar File menu and prefix owners survive word remapping" (menu (event (V.KChar 'f') [V.MCtrl,V.MAlt] star)==Just (0,0) && prefix (event (V.KChar 'k') [V.MCtrl] star)==Just 'k' && prefix (event (V.KChar 'q') [V.MCtrl] star)==Just 'q')
  check "ordinary source Control A retains SelectAll" (range (event (V.KChar 'a') [V.MCtrl] base {keyBindings=compile "source" []})==Just (Selection 0 (T.length text)))
  let hex=modifyActive (\w->w {selection=Selection 7 7}) (addDocument Nothing (newByteBuffer (BS.pack [0..31])) (initialDesktop (80,25))) {keyBindings=maps}
  check "word actions preserve hex byte steps" (range (event (V.KFun 13) [] hex)==Just (Selection 6 6) && bufferLength (docBuffer (event (V.KFun 17) [] hex))==31)
  let readonly=modifyActive (\w->w {selection=Selection 7 7}) (addHelp text (initialDesktop (80,25))) {keyBindings=maps}
  check "word deletion retains readonly source authority" (not (commandEnabled readonly DeleteWordForward) && state (event (V.KFun 18) [] readonly)==state readonly)
  W.withWindowScope $ \scope->do
    prepared<-W.prepareTextWindow "Word notes" text
    update<-W.openTextWindow scope prepared >>= maybe (fail "plugin word open failed") pure
    opened<-adoptWindowUpdate P.HumanMenu update base
    let plugin=modifyActive (\w->w {selection=Selection 7 7}) opened {keyBindings=maps}
    check "plugin word route preserves its complete-grapheme step" (range (event (V.KFun 13) [] plugin)==Just (Selection 6 6) && not (commandEnabled plugin DeleteWordBackward))
    check "plugin word unbind consumes readonly fallback" (range (event V.KLeft [V.MCtrl] plugin {keyBindings=keyBindings unbound})==range plugin)
  let mdSource=addDocument (Just (FileState "/tmp/word.md" Nothing)) (newBuffer "*one two*\n\nnext\n") (initialDesktop (80,25))
      pending=(fst (runCommand (SetBufferView MarkdownView) mdSource)) {keyBindings=maps}
      inert d=all (\(_,command)->not (commandEnabled d command) && range (fst (runCommand command d))==range d && renderedRange (fst (runCommand command d))==renderedRange d) actions
  check "pending Markdown word routes remain terminal" (inert pending)
  ready<-prepareTextPresentations pending
  let markdown=modifyActive (modifyDisplayedWindow (\w->w {selection=Selection 5 5})) ready
      moved=event (V.KFun 13) [] markdown
      stale=modifyActive (\w->w {bounds=(bounds w) {width=28}}) markdown
  check "Markdown word route preserves rendered grapheme and source coordinates" (renderedRange moved==Just (Selection 4 4) && range moved==range markdown && not (commandEnabled markdown DeleteWordForward))
  check "Markdown word unbind consumes readonly fallback" (renderedRange (event V.KLeft [V.MCtrl] markdown {keyBindings=keyBindings unbound})==renderedRange markdown)
  check "stale Markdown word routes remain terminal" (inert stale)
  let global=base {keyBindings=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "global" (M.fromList [("hide.cursor.word-left",["F13"]),("hide.edit.delete-word-forward",["F14"])])))}
      modal=prompt "Edit" Information [Input "Name" "draft" 0] global
      chat=(addReadOnly "Conversation" "reply" global) {composerBuffer=newBuffer "draft",composerSelection=Selection 2 2,composerFocused=True}
      pty=addReadOnly "Terminal test" "output" global
      sidebar=installSidebar (emptySidebar "/project" 24 True) global
      messages=global {problemsVisible=True,problemsFocused=True}
      private=global {guestPrivatePaths=["/authority"],buffers=M.map (\doc->doc {documentOrigin=Just "/authority/secret.hs"}) (buffers global)}
  check "global word remaps cannot acquire another input owner" (all (\d->all (\n->state (event (V.KFun n) [] d)==state d) [13,14] && not (commandEnabled d (CursorWordLeft False))) [modal,chat,pty,sidebar,messages])
  check "word remaps retain private origin policy" (all (\n->not (guestKeyAllowed private (V.KFun n) [])) [13,14])
  check "word remaps do not change composer drafts" (composerSelection (event (V.KFun 13) [] chat)==composerSelection chat)
  let poisoned=configured {buffers=M.map (\doc->doc {documentBuffer=(documentBuffer doc) {undoStack=error "word movement forced Undo"}}) (buffers configured)}
  check "word movement does not force source history" (range (event (V.KFun 13) [] poisoned)==Just (Selection 5 5))

dialogControlChecks :: IO ()
dialogControlChecks=do
  let check label ok=unless ok (error label)
      prepare entries=either (error . show) id (configuredBindings [] (M.singleton "terminal" (M.singleton "dialog" (M.fromList entries))))
      remapped=prepare [("hide.dialog.accept",["F13"]),("hide.dialog.cancel",["F14"])]
      unbound=prepare [("hide.dialog.accept",[]),("hide.dialog.cancel",[])]
      base=(initialDesktop (80,25)) {keyBindings=remapped}
      input=prompt "Name" Information [Input "Name" "draft" 5] base
      event key mods=handleEvent (V.EvKey key mods)
      closed=maybe True (const False) . dialog . fst
      table=remapped M.! (TerminalPlatform,DialogKeys)
      accept=maybe (error "missing dialog Accept") id (bindingAction table (V.KFun 13) [])
      cancel=maybe (error "missing dialog Cancel") id (bindingAction table (V.KFun 14) [])
  check "configured dialog controls use their focused semantic owner" (closed (event (V.KFun 13) [] input) && closed (event (V.KFun 14) [] input))
  check "removed Enter and Escape cannot replay dialog submission or cancellation"
    (all (\mods->not (closed (event V.KEnter mods input)) && not (closed (event V.KEsc mods input))) (subsequences [V.MCtrl,V.MShift,V.MAlt]))
  check "explicitly unbound dialog controls retain no physical defaults"
    (not (closed (event V.KEnter [] input {keyBindings=unbound})) && not (closed (event V.KEsc [] input {keyBindings=unbound})))
  check "dialog control labels and projection use effective commands"
    (commandIdentifier accept==Just "hide.dialog.accept" && commandIdentifier cancel==Just "hide.dialog.cancel" &&
     any ((==Just (Left accept)).snd) (statusHints input) && any ((==Just (Left cancel)).snd) (statusHints input) &&
     commandBindingKeys input accept==["F13"] && lookup "F14" (focusedBindingChords input)==Just "hide.dialog.cancel" &&
     null (commandBindingKeys input {keyBindings=unbound} accept))
  let defaults=either (error . show) id (configuredBindings [] M.empty)
      dialogs=[input,prompt "Choice" Information [ComboBox "Choice" ["old","new"] 0 Nothing] base,
        prompt "Choice" Information [ComboBox "Choice" ["old","new"] 0 (Just 1)] base,
        prompt "Draft" Information [TextArea "Draft" True (newBuffer "draft") (Selection 5 5) 0 0] base,
        prompt "Approve" (PermissionDialog "approve:test") [Input "Value" "" 0] base]
      facts (d,effects)=(fmap (\dg->(focus dg,[(case f of Input _ t pos->(t,Just pos,Nothing); ComboBox _ _ chosen preview->("",Just chosen,preview); TextArea _ _ b sel _ _->(contents b,Just (caret sel),Nothing); _->("",Nothing,Nothing)) | f<-fields dg])) (dialog d),effects)
  forM_ dialogs $ \current->forM_ [V.KEnter,V.KEsc] $ \key->forM_ (subsequences [V.MCtrl,V.MShift,V.MAlt,V.MMeta]) $ \mods->do
    let configured=current {keyBindings=defaults,nativeMac=V.MMeta `elem` mods,videoMode=if V.MMeta `elem` mods then Just 3 else Nothing}
    check "default dialog controls preserve the unconfigured focused owner"
      (facts (event key mods configured)==facts (event key mods configured {keyBindings=M.empty}))
  let popup=prompt "Choice" Information [ComboBox "Choice" ["old","new"] 0 (Just 1)] base
      choice current=case dialog (fst current) of Just dg | ComboBox _ _ value preview:_<-fields dg->(value,preview); _->(-1,Nothing)
  check "semantic Accept commits dropdown preview without submitting" (choice (event (V.KFun 13) [] popup)==(1,Nothing))
  check "semantic Cancel reverts dropdown preview without closing" (choice (event (V.KFun 14) [] popup)==(0,Nothing))
  check "unbound dropdown Escape cannot revert through fallback" (choice (event V.KEsc [] popup {keyBindings=unbound})==(0,Just 1))
  let area=prompt "Edit" Information [TextArea "Draft" True (newBuffer "draft") (Selection 5 5) 0 0] base
  check "semantic Accept preserves multiline control ownership" (case dialog (fst (event (V.KFun 13) [] area)) of
    Just dg | TextArea _ _ b _ _ _:_<-fields dg->contents b=="draft\n"; _->False)
  let protected=prompt "Approve" (PermissionDialog "approve:test") [Input "Value" "" 0] base
  check "remapped Accept cannot submit an approval from its input field" (null (snd (event (V.KFun 13) [] protected)) && dialog (fst (event (V.KFun 13) [] protected))/=Nothing)
  check "remapped Cancel keeps the permission denial owner" (snd (event (V.KFun 14) [] protected)==[PermissionAction "approve:test" ["1"]])
  let protectedButton=protected {dialog=fmap (\dg->dg {focus=length (fields dg)}) (dialog protected)}
  check "human semantic Accept still reaches the focused approval button" (case snd (event (V.KFun 13) [] protectedButton) of
    [PermissionAction _ ("0":_)]->True; _->False)
  refusedAccept<-P.applyGuestInput (P.Key "F13" []) protectedButton
  refusedCancel<-P.applyGuestInput (P.Key "F14" []) protected
  check "remapped dialog decisions remain human-only" (case (refusedAccept,refusedCancel) of (Left _,Left _)->True; _->False)
  let private=prompt "Private" Information [Input "API key" "secret" 6] base
  denied<-P.applyGuestInput (P.Key "F13" []) private
  safeCancel<-P.applyGuestInput (P.Key "F14" []) private
  check "private-field Cancel retains safe Escape authority without granting Accept" (case (denied,safeCancel) of (Left _,Right (d,[]))->dialog d==Nothing; _->False)
  let macBindings=either (error . show) id (platformBindings [] MacPlatform (M.singleton "dialog" (M.fromList [("hide.dialog.accept",["Cmd+Shift+J"]),("hide.dialog.cancel",["Cmd+Shift+K"])])))
      macInput=input {keyBindings=macBindings,nativeMac=True,videoMode=Just 3}
  check "native dialog controls share effective masks and host dispatch"
    (nativeMenuShortcut macInput accept==("j",9) && lookup "Cmd+Shift+K" (focusedBindingChords macInput)==Just "hide.dialog.cancel" && closed (event (V.KChar 'j') [V.MMeta,V.MShift] macInput))
  let guarded=input {buffers=error "dialog control forced background source"}
  check "dialog controls never inspect background source" (closed (event (V.KFun 13) [] guarded) && closed (event (V.KFun 14) [] guarded))
  check "dialog controls are unavailable outside a modal" (not (commandEnabled base accept) && not (commandEnabled base cancel) && null (snd (runCommand accept base)))

dialogFocusChecks :: IO ()
dialogFocusChecks=do
  let check label ok=unless ok (error label)
      prepared entries=either (error . show) id (configuredBindings [] (M.singleton "terminal" (M.singleton "dialog" (M.fromList entries))))
      defaults=either (error . show) id (configuredBindings [] M.empty)
      remapped=prepared [("hide.dialog.focus-next",["F13"]),("hide.dialog.focus-previous",["F14"])]
      unbound=prepared [("hide.dialog.focus-next",[]),("hide.dialog.focus-previous",[])]
      base=(initialDesktop (80,25)) {keyBindings=remapped}
      input=prompt "Names" Information [Input "First" "one" 3,Input "Second" "two" 1] base
      event key mods=fst . handleEvent (V.EvKey key mods)
      focusOf=maybe (-1) focus . dialog
      next=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "dialog" (M.singleton "hide.dialog.focus-next" ["F13"]))) M.! (TerminalPlatform,DialogKeys)
      nextCommand=maybe (error "missing focus command") id (bindingAction next (V.KFun 13) [])
      hintClick d=case statusItemRects d of (Rect x y _ _,_,_):_->eventClick x y d; _->error "missing dialog hint"
      eventClick x y=fst . handleEvent (V.EvMouseDown x y V.BLeft [])
  check "dialog focus IDs resolve without replaying keys" (commandIdentifier nextCommand==Just "hide.dialog.focus-next" && focusOf (fst (runCommand nextCommand input))==1)
  check "dialog focus remaps operate plain Input and wrap controls" (focusOf (event (V.KFun 13) [] input)==1 && focusOf (event (V.KFun 14) [] input)==3)
  check "removed dialog focus defaults cannot fall through" (all (\(key,mods)->focusOf (event key mods input)==0) [(V.KChar '\t',[]),(V.KChar '\t',[V.MShift]),(V.KBackTab,[]),(V.KBackTab,[V.MShift]),(V.KChar '\t',[V.MAlt]),(V.KBackTab,[V.MAlt])])
  check "unbound focus retains current map ownership" (bindingInputAvailable input {keyBindings=unbound} && focusOf (event (V.KChar '\t') [] input {keyBindings=unbound})==0 && focusOf (event V.KBackTab [] input {keyBindings=unbound})==0)
  check "focus status hint shares remap and direct click" (take 1 (statusHints input)==[(" F13 Next",Just (Left nextCommand))] && focusOf (hintClick input)==1)
  check "unbound focus status action remains clickable" (take 1 (statusHints input {keyBindings=unbound})==[(" Next",Just (Left nextCommand))] && focusOf (hintClick input {keyBindings=unbound})==1)
  check "plain Input frame advertises only permitted dialog actions" (lookup "F13" (focusedBindingChords input)==Just "hide.dialog.focus-next" && not (any ((=="hide.file.save").snd) (focusedBindingChords input)))
  let popup=prompt "Choice" Information [ComboBox "Choice" ["one","two"] 0 (Just 1),Input "Name" "" 0] base
      choice d=case dialog d of Just dg | ComboBox _ _ selected opened:_<-fields dg->(selected,opened,focus dg); _->error "missing combo"
  check "remapped next commits dropdown preview before advancing" (choice (event (V.KFun 13) [] popup)==(1,Nothing,1))
  check "remapped previous commits dropdown preview before wrapping" (choice (event (V.KFun 14) [] popup)==(1,Nothing,3))
  check "unbound dropdown Tab cannot commit preview" (choice (event (V.KChar '\t') [] popup {keyBindings=unbound})==(0,Just 1,0))
  check "dropdown Escape keeps its revert owner" (choice (event V.KEsc [] popup)==(0,Nothing,0))
  check "dropdown projection suppresses editing commands" (lookup "F13" (focusedBindingChords popup)==Just "hide.dialog.focus-next" && not (dialogCommandAllowed Copy popup) && all (\(_,name)->name `elem` ["hide.dialog.focus-next","hide.dialog.focus-previous","hide.dialog.accept","hide.dialog.cancel"]) (focusedBindingChords popup))
  let area=prompt "Edit" Information [TextArea "Text" True (newBuffer "draft") (Selection 5 5) 0 0] base
  check "TextArea Enter and focus retain distinct owners" ((case dialog (event V.KEnter [] area) of Just dg | TextArea _ _ b _ _ _:_<-fields dg->bufferLength b==6 && bufferLineCount b==2; _->False) && focusOf (event V.KEnter [] area)==0 && focusOf (event (V.KFun 13) [] area)==1)
  let guarded=input {buffers=error "dialog focus forced background buffers"}
  check "dialog focus never inspects background documents" (focusOf (event (V.KFun 13) [] guarded)==1 && lookup "F13" (focusedBindingChords guarded)==Just "hide.dialog.focus-next")
  check "no-table dialog Tab retains initialization behavior" (focusOf (event (V.KChar '\t') [] input {keyBindings=M.empty})==1 && focusOf (event V.KBackTab [] input {keyBindings=M.empty})==3)
  forM_ [(False,Nothing),(False,Just 3),(True,Just 3)] $ \(mac,video)->do
    let current=input {nativeMac=mac,videoMode=video,keyBindings=defaults}
    check "default focus canonicalizes terminal BackTab" (focusOf (event V.KBackTab [] current)==3 && focusOf (event (V.KChar '\t') [V.MShift] current)==3 && chordName V.KBackTab []==Just "Shift+Tab")
    check "default focus preserves forward and Alt routing" (focusOf (event (V.KChar '\t') [] current)==1 && focusOf (event V.KBackTab [V.MAlt] current)==3)
  let inherited=either (error . show) id (platformBindings [] TerminalPlatform (M.fromList [("global",M.singleton "hide.dialog.focus-next" ["F15"]),("dialog",M.singleton "hide.dialog.focus-next" ["F13"])]))
  check "dialog local focus override replaces global chords" (focusOf (event (V.KFun 13) [] input {keyBindings=inherited})==1 && focusOf (event (V.KFun 15) [] input {keyBindings=inherited})==0)
  check "dialog focus conflicts are rejected" (either (const True) (const False) (platformBindings [] TerminalPlatform (M.singleton "dialog" (M.fromList [("hide.dialog.focus-next",["F13"]),("hide.dialog.focus-previous",["F13"])]))))
  check "dialog focus cannot steal reserved field and platform controls" (all (\chord->either (const True) (const False) (platformBindings [] TerminalPlatform (M.singleton "dialog" (M.singleton "hide.dialog.focus-next" [chord])))) ["Ctrl+Tab","Cmd+Tab","Ctrl+U","Alt+A","F10"])
  let search=searchPrompt False base
  check "search Ctrl Tab retains page switching while focus is remapped" (maybe False (\dg->case purpose dg of Searching True _->True; _->False) (dialog (event (V.KChar '\t') [V.MCtrl] search)) && focusOf (event (V.KFun 13) [] search)==1)
  check "dialog focus is unavailable outside its modal owner" (not (commandEnabled base nextCommand) && null (snd (runCommand nextCommand base)))
  let private=prompt "Private" Information [Input "API key" "secret" 6,Input "Name" "" 0] base
      protected=prompt "Approve" (PermissionDialog "approve:test") [Input "Value" "" 0] base
  accepted<-P.applyGuestInput (P.Key "F13" []) private
  check "remapped focus preserves safe private-field navigation" (case accepted of Right (d,[])->focusOf d==1; _->False)
  refused<-P.applyGuestInput (P.Key "F13" []) protected
  check "remapped focus cannot authorize protected controls" (case refused of Left _->True; _->False)
  check "approval Enter and Escape keep existing decision owners" (null (snd (handleEvent (V.EvKey V.KEnter []) protected)) && snd (handleEvent (V.EvKey V.KEsc []) protected)==[PermissionAction "approve:test" ["1"]])
  let privateSelected=prompt "Private" Information [SelectedInput "API key" "secret" (Selection 0 6)] base {keyBindings=prepared [("hide.dialog.focus-next",[]),("hide.edit.copy",["Tab"])]}
  blockedCopy<-P.applyGuestInput (P.Key "Tab" []) privateSelected
  check "rebinding Tab to editing cannot borrow safe navigation authority" (case blockedCopy of Left _->True; _->False)
  let questionBase=(addReadOnly "Conversation" "Transcript" base) {chatQuestion=Just (ChatQuestion 42 "Question" ["Yes"] Nothing (newBuffer "") (Selection 0 0) True)}
  check "dialog commands do not replace inline question ownership" (not (commandEnabled questionBase nextCommand) && maybe False ((==Just 0).questionChoice) (chatQuestion (event (V.KChar '\t') [] questionBase)) && snd (handleEvent (V.EvKey V.KEnter []) questionBase)==[AgentAction "question-submit" ["42"]] && snd (handleEvent (V.EvKey V.KEsc []) questionBase)==[AgentAction "question-cancel" ["42"]])
  check "dialog focus chords are configurable on every platform" (all (\platform->case platformBindings [] platform (M.singleton "dialog" (M.fromList [("hide.dialog.focus-next",["Alt+Tab"]),("hide.dialog.focus-previous",["Shift+Tab"])])) of Right _->True; _->False) [TerminalPlatform,GraphicalPlatform,MacPlatform])


-- Debugger output and adapter sources share the same read-only navigation owner.
debuggerNavigationChecks :: IO ()
debuggerNavigationChecks=do
  let check name ok=unless ok (error name)
      prepare entries=either (error . show) id (configuredBindings [] (M.fromList
        [(platform,M.singleton "debugger" (M.fromList entries)) | platform<-["terminal","graphical","macos"]]))
      remapped=prepare [("hide.cursor.left",["F13"]),("hide.selection.left",["F14"]),("hide.edit.delete-backward",["F15"])]
      removed=prepare [("hide.cursor.left",[]),("hide.selection.left",[])]
      defaults=either (error . show) id (configuredBindings [] M.empty)
      event key mods d=handleEvent (V.EvKey key mods) d
      selected d=maybe (Selection (-1) (-1)) selection (activeWindow d)
      pane label=modifyActive (\w->w {selection=Selection 2 2})
        (addReadOnly label "a界e\x301\&z\nnext" (initialDesktop (80,25)))
      observations d=(selected (fst (event (V.KFun 13) [] d {keyBindings=remapped}))==Selection 1 1,
        selected (fst (event V.KLeft [] d {keyBindings=removed}))==Selection 2 2)
      observed=map (observations . pane) ["Debugger output","Source Generated.hs"]
  -- Report both actual-route failures in the original library, without stopping
  -- after the accepted remap and hiding the independent raw-fallback failure.
  check ("debugger remap executes and unbind consumes raw navigation: "++show observed) (all (\(remap,unbind)->remap && unbind) observed)
  forM_ ["Debugger output","Source Generated.hs"] $ \label->
    forM_ [(False,Nothing),(False,Just 3),(True,Just 3)] $ \(mac,video)->do
      let original=(pane label) {nativeMac=mac,videoMode=video}
          configured=original {keyBindings=remapped}
          result key mods=fst (event key mods configured)
          state (d,effects)=(selected d,effects)
          modal=prompt "Input" Information [Input "Value" "" 0] configured
          unfocused=configured {problemsVisible=True,problemsFocused=True}
          private=configured {guestPrivatePaths=["/authority"],buffers=M.map (\doc->doc {documentOrigin=Just "/authority/secret.hs"}) (buffers configured)}
      check "debugger remap is projected by exact command ID" (boundKeyCommand (V.KFun 13) [] configured==Just (CursorLeft False) && ("F13","hide.cursor.left") `elem` focusedBindingChords configured)
      check "debugger selection remap extends from its read-only caret" (selected (result (V.KFun 14) [])==Selection 2 1)
      check "debugger removed selection cannot fall through" (selected (fst (event V.KLeft [V.MShift] original {keyBindings=removed}))==Selection 2 2)
      check "debugger navigation defaults preserve existing pane geometry" (all (\(key,mods)->state (event key mods original)==state (event key mods original {keyBindings=defaults}))
        [(V.KLeft,[]),(V.KRight,[V.MShift]),(V.KUp,[]),(V.KDown,[V.MShift]),(V.KHome,[]),(V.KEnd,[]),(V.KHome,[V.MCtrl]),(V.KEnd,[V.MCtrl]),(V.KPageUp,[]),(V.KPageDown,[V.MShift]),(V.KLeft,[V.MCtrl]),(V.KRight,[V.MCtrl,V.MShift])])
      check "debugger navigation cannot acquire read-only mutation authority" (not (commandEnabled configured DeleteBackward) && activeText (result (V.KFun 15) [])==activeText configured && maybe (-1) (revision . documentBuffer) (activeDocument (result (V.KFun 15) []))==maybe (-2) (revision . documentBuffer) (activeDocument configured))
      check "debugger navigation remains behind modal and focus owners" (boundKeyCommand (V.KFun 13) [] modal==Nothing && selected (fst (event (V.KFun 13) [] modal))==Selection 2 2 && not (commandEnabled unfocused (CursorLeft False)) && selected (fst (event (V.KFun 13) [] unfocused))==Selection 2 2)
      denied<-P.applyGuestInput (P.Key "F13" []) private
      check "debugger remap retains generated-source privacy" (not (guestKeyAllowed private (V.KFun 13) []) && case denied of Left _->True; _->False)


-- Caret-only fields retain their exact field operations and control precedence.
dialogInputChecks :: IO ()
dialogInputChecks=do
  let check name ok=unless ok (error name)
      configured entries=either (error . show) id (configuredBindings [] (M.fromList
        [(platform,M.singleton "dialog" (M.fromList entries)) | platform<-["terminal","graphical","macos"]]))
      globals entries=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "global" (M.fromList entries)))
      event key mods d=handleEvent (V.EvKey key mods) d
      field d=case dialog d of Just dg | f:_<-fields dg -> f; _->error "missing Input"
      base=prompt "Name" Information [Input "Name" "a界e\x301\&z" 2,FileList [Entry "old.hs" False Nothing Nothing] 0] (initialDesktop (80,25))
      local=platformBindings [] TerminalPlatform (M.singleton "dialog" (M.singleton "hide.cursor.left" ["F13"]))
      observed=(either (const False) (const True) local,
        field (fst (event (V.KFun 13) [] base {keyBindings=globals [("hide.cursor.left",["F13"])]}))==Input "Name" "a界e\x301\&z" 1,
        field (fst (event V.KLeft [] base {keyBindings=globals [("hide.cursor.left",[])]}))==field base)
  check ("single-line Input configuration, remap and unbind: "++show observed) (case observed of (a,b,c)->a && b && c)
  let entries=[("hide.cursor.left",["F13"]),("hide.cursor.right",["F14"]),("hide.cursor.row-start",["F15"]),("hide.cursor.row-end",["F16"]),("hide.edit.delete-backward",["F17"]),("hide.edit.delete-forward",["F18"])]
      remapped=configured entries
      removed=configured [(name,[]) | (name,_)<-entries]
      altRemapped=configured [("hide.cursor.left",["Alt+Right"]),("hide.cursor.right",[])]
      defaults=either (error . show) id (configuredBindings [] M.empty)
      keys=[V.KLeft,V.KRight,V.KHome,V.KEnd,V.KBS,V.KDel]
  forM_ [(False,Nothing),(False,Just 3),(True,Just 3)] $ \(mac,video)->do
    let original=base {nativeMac=mac,videoMode=video}
        current=original {keyBindings=remapped,buffers=error "Input command touched background documents"}
        edited key=fst (event key [] current)
        outcome (d,effects)=(fmap (\dg->(fields dg,focus dg)) (dialog d),effects)
    check "Input command defaults preserve modifier and grapheme semantics" (all (\(key,mods,pos)->let d=original {dialog=fmap (\dg->dg {fields=replaceAt 0 (Input "Name" "a界e\x301\&z" pos) (fields dg)}) (dialog original)} in outcome (event key mods d)==outcome (event key mods d {keyBindings=defaults}))
      ([(key,[],pos) | key<-keys,pos<-[0,2,5]]++[(V.KLeft,[V.MShift],2),(V.KRight,[V.MAlt],2),(V.KHome,[V.MCtrl],4),(V.KEnd,[V.MMeta],1),(V.KBS,[V.MCtrl,V.MShift],4),(V.KDel,[V.MShift],2)]))
    check "Input remaps move by grapheme and row edges" (map (field . edited . V.KFun) [13,14,15,16]==[Input "Name" "a界e\x301\&z" 1,Input "Name" "a界e\x301\&z" 4,Input "Name" "a界e\x301\&z" 0,Input "Name" "a界e\x301\&z" 5])
    check "Input remapped deletion clears stale filename selection" (field (edited (V.KFun 17))==Input "Name" "ae\x301\&z" 1 && field (edited (V.KFun 18))==Input "Name" "a界z" 2 && all (\key->case dialog (edited key) of Just dg | [FileList _ (-1)]<-drop 1 (fields dg)->True; _->False) [V.KFun 17,V.KFun 18])
    check "Input explicit unbinding consumes old physical defaults" (all (\key->outcome (event key [] original {keyBindings=removed})==outcome (original,[])) keys && outcome (event V.KRight [V.MAlt] original {keyBindings=removed})==outcome (original,[]))
    check "Input reserved chord cannot be assigned to dialog acceptance" (either (const True) (const False) (platformBindings [] (bindingPlatform original) (M.singleton "dialog" (M.singleton "hide.dialog.accept" ["Alt+Right"]))))
    check "Input Alt Right can be remapped through its field owner" (field (fst (event V.KRight [V.MAlt] original {keyBindings=altRemapped}))==Input "Name" "a界e\x301\&z" 1)
    check "Input projection and labels expose its effective commands" (lookup "F13" (focusedBindingChords current)==Just "hide.cursor.left" && menuShortcut current (MenuItem "Left" "Left" (CursorLeft False))=="F13")
    let other f=current {buffers=M.empty,dialog=fmap (\dg->dg {fields=[f]}) (dialog current)}
        selected=other (SelectedInput "Expression" "abc" (Selection 1 1))
        area=other (TextArea "Text" True (newBuffer "abc") (Selection 1 1) 0 0)
        combo=other (ComboBox "Choice" ["one","two"] 0 (Just 1))
    check "Input bindings leave selected and multiline field owners fixed" (field (fst (event V.KLeft [V.MShift] selected))==SelectedInput "Expression" "abc" (Selection 1 0) && outcome (event (V.KFun 13) [] selected)==outcome (selected,[]) && case field (fst (event V.KLeft [] area)) of TextArea _ _ _ sel _ _->sel==Selection 0 0; _->False)
    check "Input bindings leave open dropdowns and buttons fixed" (field (fst (event V.KHome [] combo))==ComboBox "Choice" ["one","two"] 0 (Just 0) && outcome (event (V.KFun 13) [] combo)==outcome (combo,[]) && let buttons=current {dialog=fmap (\dg->dg {fields=[],focus=1}) (dialog current)} in maybe False ((==0).focus) (dialog (fst (event V.KLeft [] buttons))))
    let search=fst (runCommand Find original {keyBindings=remapped})
    check "Input bindings preserve search page and Ctrl U owners" (maybe False (\dg->case purpose dg of Searching True _->True; _->False) (dialog (fst (event (V.KChar '\t') [V.MCtrl] search))) && field (fst (event (V.KChar 'u') [V.MCtrl] current))==Input "Name" "" 0)
    forM_ [current {dialog=fmap (\dg->dg {fields=[Input "API key" "secret" 3]}) (dialog current)},current {dialog=fmap (\dg->dg {purpose=PermissionDialog "approve:test"}) (dialog current)}] $ \private->do
      denied<-P.applyGuestInput (P.Key "F17" []) private
      check "Input remapping cannot acquire private or approval authority" (case denied of Left _->True; _->False)

-- Finite prefix contexts share actual dispatch, labels and guest resolution.
prefixChecks :: IO ()
prefixChecks=do
  let check name ok=unless ok (error name)
      prepare platform=either (error . show) id . platformBindings [] platform
      configuration=M.fromList [("wordstar",M.fromList [("hide.wordstar.block-prefix",["Ctrl+J"]),("hide.edit.copy",[])])
        ,("wordstar-block",M.fromList [("hide.edit.copy",["Y"]),("hide.edit.delete-selection",[]),("hide.edit.undo",["Z"]),("hide.edit.paste",["P"]),("hide.cursor.right",[])])]
      base=modifyActive (\w->w {selection=Selection 0 5}) $ addDocument Nothing (newBuffer "hello") (initialDesktop (80,25))
      configured=base {wordStar=True,keyBindings=prepare TerminalPlatform configuration}
      event key mods=fst . handleEvent (V.EvKey key mods)
      pending=event (V.KChar 'j') [V.MCtrl] configured
      copied=event (V.KChar 'y') [] pending
      defaults=base {wordStar=True,keyBindings=prepare TerminalPlatform M.empty}
      started=event (V.KChar 'k') [V.MCtrl] defaults
      selected d=selection <$> activeWindow d
  check "remapped WordStar starter and continuation copy the selected source" (prefix pending==Just 'k' && clipboard copied=="hello" && prefix copied==Nothing && activeText copied=="hello")
  check "removed starter and continuation cannot reach old grammar" (prefix (event (V.KChar 'k') [V.MCtrl] configured)==Nothing && clipboard (event (V.KChar 'c') [] pending)=="" && activeText (event (V.KChar 'c') [] pending)=="hello")
  check "prefix labels are compound while frontend input stays a single stroke"
    (menuShortcut configured (MenuItem "Copy" "Ctrl+C" Copy)=="Ctrl+J Y" && commandBindingKeys pending Copy==["Ctrl+J Y"] &&
     lookup "Y" (focusedBindingChords pending)==Just "hide.edit.copy" && boundKeyCommand (V.KChar 'p') [] pending==Just Paste &&
     nativeMenuShortcut pending Copy==("",0) && any ((==" Ctrl+J … ").fst) (statusHints pending))
  let replaced=event (V.KChar '!') [] configured
      undoPending=event (V.KChar 'j') [V.MCtrl] replaced
  check "a configured ordinary command is eligible as a second stroke" (activeText replaced=="!" && activeText (event (V.KChar 'z') [] undoPending)=="hello")
  check "unknown removed noncharacter and cancelled steps consume prefix without editing"
    (all (\d->prefix d==Nothing && activeText d=="hello" && selected d==selected pending && clipboard d=="")
      [event (V.KChar 'c') [] pending,event V.KRight [] pending,event V.KEsc [] pending])
  forM_ [[],[V.MShift],[V.MCtrl],[V.MCtrl,V.MShift]] $ \mods->
    check "default bare Control and Shift block aliases resolve the same action" (clipboard (event (V.KChar 'C') mods started)=="hello")
  let marks=modifyActive (\w->w {selection=Selection 1 1}) defaults
      marked=event (V.KChar 'b') [] (event (V.KChar 'k') [V.MCtrl] marks)
      moved=modifyActive (\w->w {selection=Selection 4 4}) marked
      block=event (V.KChar 'k') [] (event (V.KChar 'k') [V.MCtrl] moved)
      removed=event (V.KChar 'y') [] (event (V.KChar 'k') [V.MCtrl] block)
  check "block markers and deletion retain source selection and one Undo"
    (selected block==Just (Selection 1 4) && activeText removed=="ho" && activeText (fst (runCommand Undo removed))=="hello")
  check "explicit global overrides apply inside the finite prefix table"
    (boundKeyCommand (V.KFun 13) [] started {keyBindings=prepare TerminalPlatform (M.singleton "global" (M.singleton "hide.edit.undo" ["F13"]))}==Just Undo)
  check "duplicate second strokes reject the complete configuration"
    (either (const True) (const False) (platformBindings [] TerminalPlatform (M.singleton "wordstar-block" (M.singleton "hide.edit.undo" ["C"]))))
  let two=addDocument Nothing (newBuffer "second") defaults
      cycling=event (V.KChar 'k') [V.MCtrl] two
      noCycle=cycling {keyBindings=prepare TerminalPlatform (M.singleton "wordstar-block" (M.singleton "hide.window.next" []))}
      ident d=windowId <$> activeWindow d
  check "pending prefix window cycling remains in its effective table"
    (ident (event (V.KChar '\t') [V.MCtrl] cycling)/=ident cycling && prefix (event (V.KChar '\t') [V.MCtrl] cycling)==Nothing)
  check "prefix window cycling unbinding cannot reach its old fallback"
    (ident (event (V.KChar '\t') [V.MCtrl] noCycle)==ident noCycle && prefix (event (V.KChar '\t') [V.MCtrl] noCycle)==Nothing)
  let pasted=fst (handleEvent (V.EvPaste "replacement") pending)
      requested=runCommand Paste pending {browserFrontend=True}
  check "native browser paste and requested paste consume the active prefix"
    (activeText pasted=="replacement" && prefix pasted==Nothing && prefix (fst requested)==Nothing && snd requested==[ReadBrowserClipboard])
  check "prefix cannot steal the earlier Ctrl Alt Exit owner"
    (prefix (event (V.KChar 'x') [V.MCtrl,V.MAlt] started)==Nothing && snd (handleEvent (V.EvKey (V.KChar 'x') [V.MCtrl,V.MAlt]) started)==[Exit])
  let noQuick=defaults {keyBindings=prepare TerminalPlatform (M.singleton "wordstar" (M.singleton "hide.wordstar.quick-prefix" []))}
      quick=event (V.KChar 'q') [V.MCtrl] defaults
  check "quick starter unbinding prevents the old alias and default quick edge remains semantic"
    (prefix (event (V.KChar 'q') [V.MCtrl] noQuick)==Nothing && selected (event (V.KChar 'c') [] quick)==Just (Selection 5 5))
  let modal=prompt "Edit" Information [Input "Name" "draft" 5] pending
      terminal=addReadOnly "Terminal test" "output" configured
      readonly=addReadOnly "Help" "protected" configured
      private=pending {guestPrivatePaths=["/authority"],buffers=M.map (\doc->doc {documentOrigin=Just "/authority/secret.hs"}) (buffers pending)}
  check "prefix state cannot move into modal terminal or readonly owners"
    (not (commandEnabled modal WordStarBlockPrefix) && not (commandEnabled readonly MarkBlockStart) &&
     snd (handleEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) terminal)==[AgentAction "terminal-input" ["test","\ETX"]] &&
     not (guestKeyAllowed private (V.KChar 'y') []))
  check "menu mnemonic priority does not become a prefix continuation" (menu (event (V.KChar 'f') [V.MAlt] started)/=Nothing && prefix (event (V.KChar 'f') [V.MAlt] started)==Nothing)
  let batch=event (V.KChar 'y') [] (event (V.KChar 'j') [V.MCtrl] (beginGuestInput configured))
  check "guest prefix lifetime remains one authorized batch"
    (clipboard batch=="hello" && prefix (endGuestInput configured pending)==Nothing && clipboard (endGuestInput configured batch)==clipboard configured)
  let mac=configured {nativeMac=True,videoMode=Just 3,keyBindings=prepare MacPlatform configuration}
      macPending=event (V.KChar 'j') [V.MCtrl] mac
  check "macOS prefix has no inherited Command clipboard default"
    (boundKeyCommand (V.KChar 'c') [V.MMeta] macPending==Nothing && menuShortcut mac (MenuItem "Copy" "Cmd+C" Copy)=="⌃J Y")
  where activeText d=maybe "" (contents . documentBuffer) (activeDocument d)
