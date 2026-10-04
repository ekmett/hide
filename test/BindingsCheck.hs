{-# LANGUAGE OverloadedStrings #-}
module BindingsCheck (checks) where
import SourceWindowFixture (sourceFixtureBuffer)
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_)
import System.Directory (getTemporaryDirectory, removePathForcibly, removeFile, createDirectory, canonicalizePath)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import Hide.Window (nativeMenuShortcut)
import Hide.Keybindings
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Hide.Bindings
import Hide.Commands
import Hide.Sidebar
import Hide.Model
import Hide.Browser (Entry(..))
import Hide.GuestAccess (guestKeyAllowed)
import Hide.Files (FileState(..))
import Hide.Buffer
import Hide.Render (renderKey)

checks :: IO ()
checks=do
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
  let modalMaps=either (error . show) id (configuredBindings [] (M.singleton "macos" (M.singleton "source" (M.singleton "hide.edit.copy" ["Cmd+Left"]))))
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
  let global=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "global" (M.singleton "hide.file.save" ["Ctrl+Shift+S","Alt+F11"])))
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
  check "WordStar owned grammar chords reject named overrides" (either (const True) (const False) (platformBindings [] TerminalPlatform (M.singleton "wordstar" (M.singleton "hide.file.save" ["Ctrl+K"]))))
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
  check "dialog projection omits actions unavailable to its focused control" (null (focusedBindingChords editing {dialog=fmap (\dg->dg {focus=1}) (dialog editing)}) && not (any ((=="hide.file.save").snd) (focusedBindingChords editing)))
  check "dialog cannot bind a background source command" (either (const True) (const False) (platformBindings [] TerminalPlatform (M.singleton "dialog" (M.singleton "hide.file.save" ["Ctrl+Shift+J"]))))
  check "dialog field text and navigation retain their owner" (dialogText (fst (key (V.KChar 'ø') [] editing))=="ø" && dialog (fst (key V.KEsc [] editing))==Nothing)
  let dialogDefaults=either (error . show) id (configuredBindings [] M.empty)
      searchEditing=fst (runCommand Find background {keyBindings=dialogDefaults})
  check "default dialog clipboard matches field-owned behavior" (clipboard (fst (key (V.KChar 'c') [V.MCtrl] editing {nativeMac=False,keyBindings=dialogDefaults}))=="draft")
  check "unavailable dialog editing action retains Input button mnemonic ownership" (dialog (fst (key (V.KChar 'c') [V.MCtrl] searchEditing))==Nothing)
  check "default dialog search retains permitted replacement action" (maybe False (\dg->case purpose dg of Searching True _->True; _->False) (dialog (fst (key (V.KChar 'f') [V.MMeta,V.MAlt] searchEditing))))
  reloadChecks
  putStrLn "keybinding checks passed"

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
