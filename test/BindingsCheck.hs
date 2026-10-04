{-# LANGUAGE OverloadedStrings #-}
module BindingsCheck (checks) where
import Control.Monad (unless)
import qualified Data.Map.Strict as M
import qualified Graphics.Vty as V
import Hide.Bindings
import Hide.Commands
import Hide.Model
import Hide.Files (FileState(..))
import Hide.Buffer
import Hide.Render (renderKey)

checks :: IO ()
checks=do
  let check name ok=unless ok (error name)
      prepare=either (error . show) id . terminalSourceBindings . M.fromList
      bindings=prepare [("hide.file.save",["Ctrl+Shift+S"]),("hide.file.open",[])]
      base=addDocument (Just (FileState "/project/Main.hs" Nothing)) (newBuffer "hello") (initialDesktop (80,25))
      source=base {sourceBindings=Just bindings}
      key k mods d=handleEvent (V.EvKey k mods) d
      noEffects (_,effects)=null effects
      rejected= either (const True) (const False) . terminalSourceBindings . M.fromList
  check "remapping replaces every old binding" (bindingAction bindings (V.KFun 2) []==Nothing && bindingAction bindings (V.KChar 's') [V.MCtrl]==Nothing)
  check "modifier order and letter case normalize" (bindingAction bindings (V.KChar 'S') [V.MShift,V.MCtrl]==Just Save)
  check "empty binding lists remain unbound" (null (bindingKeys bindings Open))
  check "unknown command and ambiguous key fail" (rejected [("missing",[])] && rejected [("hide.file.save",["Ctrl+O"])])
  check "unknown modifier and reserved keys fail" (all (\chord->rejected [("hide.file.save",[chord])]) ["Cmd+S","Ctrl+Ctrl+S","F10","Alt+F","Tab","S","Ctrl+]","Ctrl+\n"])
  check "source invokes the remapped action" (case snd (key (V.KChar 's') [V.MCtrl,V.MShift] source) of [SaveDocument{}]->True; _->False)
  check "unbound source keys cannot fall through to old defaults" (noEffects (key (V.KFun 2) [] source) && noEffects (key (V.KChar 's') [V.MCtrl] source) && noEffects (key (V.KFun 3) [] source))
  check "ordinary source typing remains local" (activeText (fst (key (V.KChar 'x') [] source))=="xhello")
  check "menu and status labels use the effective binding" (menuShortcut source (MenuItem "Save" "F2" Save)=="Ctrl+Shift+S" && any ((==" Ctrl+Shift+S Save").fst) (statusHints source))
  check "daemon clipboard transport does not disable terminal bindings" (boundSourceCommand (V.KChar 's') [V.MCtrl,V.MShift] source {browserFrontend=True}==Just Save)
  let messages=source {problemsVisible=True,problemsFocused=True}
  check "Messages retains its own key labels" (menuShortcut messages (MenuItem "Save" "F2" Save)=="F2")
  let modal=prompt "Question" Information [Input "Name" "" 0] source
  check "source bindings cannot invoke through a modal" (noEffects (key (V.KChar 's') [V.MCtrl,V.MShift] modal))
  let terminal=addReadOnly "Terminal test" "output" source
  check "PTY Ctrl+C still sends interrupt" (snd (key (V.KChar 'c') [V.MCtrl] terminal)==[AgentAction "terminal-input" ["test","\ETX"]])
  check "native frontend retains its own mapping in this stage" (not (noEffects (key (V.KFun 2) [] source {videoMode=Just 3})))
  let rebound=source {sourceBindings=Just (prepare [("hide.file.save",[])])}
  before<-renderKey source
  after<-renderKey rebound
  check "prepared binding replacement invalidates labels" (before/=after)
  putStrLn "keybinding checks passed"
