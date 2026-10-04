{-# LANGUAGE OverloadedStrings #-}
module BindingsCheck (checks) where
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_)
import System.Directory (getTemporaryDirectory, removePathForcibly, removeFile, createDirectory, canonicalizePath)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import Hide.Keybindings
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Hide.Bindings
import Hide.Commands
import Hide.Model
import Hide.Browser (Entry(..))
import Hide.GuestAccess (guestKeyAllowed)
import Hide.Files (FileState(..))
import Hide.Buffer
import Hide.Render (renderKey)

checks :: IO ()
checks=do
  let check name ok=unless ok (error name)
      prepare=either (error . show) id . terminalBindings . M.singleton "source" . M.fromList
      bindings=prepare [("hide.file.save",["Ctrl+Shift+S"]),("hide.file.open",[])]
      base=addDocument (Just (FileState "/project/Main.hs" Nothing)) (newBuffer "hello") (initialDesktop (80,25))
      source=base {keyBindings=bindings}
      sourceKeys=bindings M.! SourceKeys
      key k mods d=handleEvent (V.EvKey k mods) d
      noEffects (_,effects)=null effects
      rejected= either (const True) (const False) . terminalBindings . M.singleton "source" . M.fromList
  check "remapping replaces every old binding" (bindingAction sourceKeys (V.KFun 2) []==Nothing && bindingAction sourceKeys (V.KChar 's') [V.MCtrl]==Nothing)
  check "modifier order and letter case normalize" (bindingAction sourceKeys (V.KChar 'S') [V.MShift,V.MCtrl]==Just Save)
  check "empty binding lists remain unbound" (null (bindingKeys sourceKeys Open))
  check "unknown command and ambiguous key fail" (rejected [("missing",[])] && rejected [("hide.file.save",["Ctrl+O"])])
  check "unknown modifier and reserved keys fail" (all (\chord->rejected [("hide.file.save",[chord])]) ["Cmd+S","Ctrl+Ctrl+S","F10","Alt+F","Tab","S","Ctrl+]","Ctrl+\n"])
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
  let maps=either (error . show) id (terminalBindings (M.fromList
        [("global",M.singleton "hide.options.agent-permissions" ["Alt+P"])
        ,("sidebar",M.fromList [("hide.sidebar.expand",["Ctrl+E"]),("hide.sidebar.down",[]),("hide.options.agent-permissions",["Ctrl+Shift+P"])])
        ,("conversation",M.fromList [("hide.edit.copy",["Ctrl+Shift+J"]),("hide.agents.cancel",["Ctrl+Shift+K"])])
        ,("terminal",M.singleton "hide.terminal.stop" ["Alt+F11"])
        ,("debugger",M.singleton "hide.debug.continue" ["Ctrl+Shift+D"])]))
      contextBase=base {keyBindings=maps}
      tree=installTree "/project" [Entry "src" True Nothing Nothing,Entry "Main.hs" False Nothing Nothing] contextBase
  check "sidebar expansion uses its own override" (snd (key (V.KChar 'e') [V.MCtrl] tree)==[ExpandTree 0] && noEffects (key V.KRight [] tree))
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
  check "terminal control chords cannot be assigned to commands" (either (const True) (const False) (terminalBindings (M.singleton "terminal" (M.singleton "hide.terminal.stop" ["Ctrl+C"]))))
  let debug=addReadOnly "Debugger output" "stopped" contextBase
  check "debugger override drives dispatch and menu labels" (snd (key (V.KChar 'd') [V.MCtrl,V.MShift] debug)==[DebugAction "continue" []] && noEffects (key (V.KFun 4) [] debug) && menuShortcut debug (MenuItem "Continue" "F4" (DebugCommand "continue"))=="Ctrl+Shift+D")
  let global=either (error . show) id (terminalBindings (M.singleton "global" (M.singleton "hide.file.save" ["Ctrl+Shift+S","Alt+F11"])))
  check "global control overrides apply outside PTYs" (all (\context->bindingAction (global M.! context) (V.KChar 's') [V.MCtrl,V.MShift]==Just Save) [SourceKeys,SidebarKeys,ConversationKeys,MessagesKeys,DebuggerKeys])
  check "PTY inherits transferable global chords and omits process controls" (bindingKeys (global M.! TerminalKeys) Save==["Alt+F11"] && snd (key (V.KChar 'c') [V.MCtrl] (pty {keyBindings=global}))==[AgentAction "terminal-input" ["test","\ETX"]])
  check "unknown contexts fail instead of disappearing" (either (const True) (const False) (terminalBindings (M.singleton "sidebaar" M.empty)))
  let defaults=either (error . show) id (terminalBindings M.empty)
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
  check "resolved reload retains guest origin policy" (not (guestKeyAllowed source {keyBindings=either (error . show) id (terminalBindings (M.singleton "source" (M.singleton "hide.bindings.reload" ["Alt+F11"])))} (V.KFun 11) [V.MAlt]))
  reloadChecks
  putStrLn "keybinding checks passed"

reloadChecks :: IO ()
reloadChecks=bracket temporary removePathForcibly $ \directory->do
  old<-lookupEnv "XDG_CONFIG_HOME"
  let restore=maybe (unsetEnv "XDG_CONFIG_HOME") (setEnv "XDG_CONFIG_HOME") old
  bracket_ (setEnv "XDG_CONFIG_HOME" directory) restore $ withKeybindings $ \runtime->do
    let path=directory </> "thc.toml"
        defaults=either (error . show) id (terminalBindings M.empty)
        base=(addDocument (Just (FileState (directory </> "Main.hs") Nothing)) (newBuffer "text") (initialDesktop (80,25))) {keyBindings=defaults}
        fallback d _=pure (False,d)
        start command d=let (requested,effects)=runCommand command d in snd <$> keybindingEffects runtime fallback requested effects
        settle done d=do
          updated<-tickKeybindings runtime d
          if done updated then pure updated else threadDelay 1000 >> settle done updated
        await done d=timeout 5000000 (settle done d) >>= maybe (error "keybinding worker did not finish") pure
        reloaded d=status d/="Reloading terminal bindings..."
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
