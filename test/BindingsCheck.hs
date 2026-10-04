{-# LANGUAGE OverloadedStrings #-}
module BindingsCheck (checks) where
import Control.Monad (unless)
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
        ,("conversation",M.fromList [("hide.edit.copy",["Ctrl+Shift+Y"]),("hide.agents.cancel",["Ctrl+Shift+K"])])
        ,("terminal",M.singleton "hide.terminal.stop" ["Alt+T"])
        ,("debugger",M.singleton "hide.debug.continue" ["Ctrl+Shift+D"])]))
      contextBase=base {keyBindings=maps}
      tree=installTree "/project" [Entry "src" True,Entry "Main.hs" False] contextBase
  check "sidebar expansion uses its own override" (snd (key (V.KChar 'e') [V.MCtrl] tree)==[ExpandTree 0] && noEffects (key V.KRight [] tree))
  check "removed sidebar movement stays removed" (maybe (-1) treeSelected (sideTree (fst (key V.KDown [] tree)))==0)
  check "context entry replaces global command chords" (boundKeyCommand (V.KChar 'p') [V.MCtrl,V.MShift] tree==Just AgentPermissions && boundKeyCommand (V.KChar 'p') [V.MAlt] tree==Nothing)
  check "rebound sidebar protected action retains guest policy" (not (guestKeyAllowed tree (V.KChar 'p') [V.MCtrl,V.MShift]))
  let chat=(addReadOnly "Conversation" "reply" contextBase) {composerBuffer=newBuffer "draft",composerSelection=Selection 0 5,composerFocused=True}
  check "conversation copy dispatches its effective chord" (clipboard (fst (key (V.KChar 'y') [V.MCtrl,V.MShift] chat))=="draft" && T.null (clipboard (fst (key (V.KChar 'c') [V.MCtrl] chat))))
  check "conversation commands remain protected after remapping" (not (guestKeyAllowed chat (V.KChar 'k') [V.MCtrl,V.MShift]))
  check "conversation Enter keeps draft submission ownership" (snd (key V.KEnter [] chat)==[AgentAction "send-draft" []])
  let pty=addReadOnly "Terminal test" "output" contextBase
  check "terminal editor actions can be rebound" (snd (key (V.KChar 't') [V.MAlt] pty)==[AgentAction "terminal-stop" []])
  check "terminal control characters retain process ownership" (all (\(c,text)->snd (key (V.KChar c) [V.MCtrl] pty)==[AgentAction "terminal-input" ["test",text]]) [('c',"\ETX"),('q',"\DC1"),('s',"\DC3")])
  check "terminal control chords cannot be assigned to commands" (either (const True) (const False) (terminalBindings (M.singleton "terminal" (M.singleton "hide.terminal.stop" ["Ctrl+C"]))))
  let debug=addReadOnly "Debugger output" "stopped" contextBase
  check "debugger override drives dispatch and menu labels" (snd (key (V.KChar 'd') [V.MCtrl,V.MShift] debug)==[DebugAction "continue" []] && noEffects (key (V.KFun 4) [] debug) && menuShortcut debug (MenuItem "Continue" "F4" (DebugCommand "continue"))=="Ctrl+Shift+D")
  check "unknown contexts fail instead of disappearing" (either (const True) (const False) (terminalBindings (M.singleton "sidebaar" M.empty)))
  putStrLn "keybinding checks passed"
