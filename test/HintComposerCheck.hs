{-# LANGUAGE OverloadedStrings #-}
module HintComposerCheck (checks) where

import EditorFixture (withEditorFixture,withAutocompleteFixture)
import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import qualified Hide.Plugin.Editor as E
import qualified Hide.Plugin.Menu as Menu
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import System.Directory
import System.IO (openTempFile,hClose)
import Hide.Buffer
import Hide.Commands (platformBindings)
import qualified Hide.Bindings as Bindings
import Hide.GuestAccess
import Hide.Model
import Hide.Recovery
import qualified Hide.Plugin.Window as W

checks :: IO ()
checks=withEditorFixture "" (initialDesktop (90,30)) $ \chatBase->
  withAutocompleteFixture "completion trace\nresponse\n" (closeActive (setComposerInput (newBuffer "main chat draft") (Selection 2 5) True chatBase)) $ \trace->do
  let base=trace
        {autocompleteACPEnabled=True,
         agentReplying=True,agentQueued=2,agentSteering=True}
      paste text d=fst (handleEvent (V.EvPaste (TE.encodeUtf8 text)) d)
      key k mods d=handleEvent (V.EvKey k mods) d
      typed=paste "hint λ" base
      multiline=fst (key V.KEnter [V.MShift] typed)
      filled=paste "    preserve indentation" multiline
      (submitted,effects)=key V.KEnter [] filled
      w=fromJust (activeWindow filled)
      rect=composerRect filled w
      mount=fromJust (activeEditorMount filled)
      chatRef=fromJust (composerDraftRef chatBase)
      checkDraft d=case M.lookup chatRef (editorDrafts d) of
        Just draft->contents (editorDraftBuffer draft)=="main chat draft" && editorDraftSelection draft==Selection 2 5 && agentReplying d && agentQueued d==2 && agentSteering d
        Nothing->False
  check "ACP hint typing uses a dedicated draft" (contents (composerBuffer typed)=="hint λ" && checkDraft typed)
  check "Shift Enter inserts literal newline without chat submission" (contents (composerBuffer multiline)=="hint λ\n")
  check "Enter submits the exact mounted editor and retains its draft until acknowledgement"
    (effects==[SubmitEditor mount E.DefaultEditor Menu.HumanMenu] && contents (composerBuffer submitted)=="hint λ\n    preserve indentation" && composerSelection submitted==composerSelection filled && checkDraft submitted)
  check "Ctrl Enter selects the same mounted editor alternate slot"
    (snd (key V.KEnter [V.MCtrl] filled)==[SubmitEditor mount E.AlternateEditor Menu.HumanMenu])
  check "blank input remains an owning action validation decision"
    (snd (key V.KEnter [] (setComposerInput (newBuffer "") (Selection 0 0) True base))==[SubmitEditor mount E.DefaultEditor Menu.HumanMenu])
  check "hint editing never changes trace document" (maybe False ((=="completion trace\nresponse\n") . (\text->contentSlice text 0 (contentLength text)) . W.preparedWindowText) (activePluginWindow submitted))
  check "focused hint enables its edit commands without granting transcript mutation"
    (all (commandEnabled filled) [Undo,Redo,Cut,Paste] && not (commandEnabled filled Save) &&
      all (not . commandEnabled (setComposerInput (composerBuffer filled) (composerSelection filled) False filled)) [Undo,Redo,Cut,Paste])
  let allHint=fst (runCommand SelectAll filled)
      copied=fst (runCommand Copy allHint)
      cut=fst (runCommand Cut copied)
      undone=fst (runCommand Undo cut)
  check "hint select copy cut and undo remain isolated"
    (clipboard copied=="hint λ\n    preserve indentation" && bufferLength (composerBuffer cut)==0 && contents (composerBuffer undone)==contents (composerBuffer filled) && checkDraft undone)
  let (escaped,escapeEffects)=key V.KEsc [] filled
      (tabbed,tabEffects)=key (V.KChar '\t') [] filled
  check "hint Escape and Tab do not cancel or steer chat" (null escapeEffects && null tabEffects && not (composerFocused escaped) && not (composerFocused tabbed) && checkDraft escaped)
  let maps=either (error . T.unpack) id (platformBindings [] Bindings.TerminalPlatform M.empty)
      bound=filled {keyBindings=maps}
      transcript=fst (key (V.KChar '\t') [] bound)
      navigated=fst (key V.KRight [] transcript)
      resumed=fst (key (V.KChar '\t') [] navigated)
      remaps=either (error . T.unpack) id (platformBindings [("hide.test.submit",SubmitChat QuerySubmit)] Bindings.TerminalPlatform
        (M.singleton "conversation" (M.singleton "hide.test.submit" ["Ctrl+Shift+K"])))
      remapped=bound {keyBindings=remaps}
  check "loaded keymaps preserve Tab transcript navigation and return to the same hint"
    (not (composerFocused transcript) && maybe False ((==1) . caret . selection) (activeWindow navigated) &&
      composerFocused resumed && composerSelection resumed==composerSelection filled && checkDraft resumed &&
      snd (key V.KEnter [] resumed)==[SubmitEditor mount E.DefaultEditor Menu.HumanMenu])
  check "submission remaps still select this editor's exact mounted action"
    (snd (key (V.KChar 'k') [V.MCtrl,V.MShift] remapped)==[SubmitEditor mount E.DefaultEditor Menu.HumanMenu])
  let (clicked,clickEffects)=handleEvent (V.EvMouseDown (left rect+4) (top rect+1) V.BLeft []) filled
  check "hint clicks retain literal indentation and independent selection"
    (null clickEffects && caret (composerSelection clicked)==T.length "hint λ\n    " && checkDraft clicked)
  let pendingQuestion=ChatQuestion 17 "Choose a reply" ["Yes"] Nothing (newBuffer "private answer") (Selection 1 3) True
      (questionRetained,questionEffects)=handleEvent (V.EvMouseDown (left rect+4) (top rect+1) V.BLeft []) filled {chatQuestion=Just pendingQuestion}
  check "hint composer clicks preserve an unrelated conversation's pending question"
    (null questionEffects && chatQuestion questionRetained==Just pendingQuestion && checkDraft questionRetained &&
      caret (composerSelection questionRetained)==T.length "hint λ\n    ")
  check "hint composer reserves transcript rows only for ACP"
    (pluginBodyRows filled (w {windowEditorMount=Nothing})-pluginBodyRows filled w==height rect+1)
  check "agents cannot type or click hint composer"
    (not (guestKeyboardAllowed filled) && not (pointerAllowedAt filled (left rect) (top rect)) && not (readableAt filled (left rect) (top rect)))
  hintAllowed<-guestTransitionAllowed base typed []
  check "agent transition policy rejects hint edits and submission"
    (not hintAllowed && not (guestEffectsAllowed effects))
  check "agent can still read autocomplete trace above the private draft" (readableAt filled (left (bounds w)+2) (top (bounds w)+2))
  let fake=(addReadOnly "Autocomplete" "ordinary document" (initialDesktop (90,30))) {autocompleteACPEnabled=True}
  check "an ordinary document title grants no hint input" (not (activeAutocomplete fake) && bufferLength (composerBuffer (paste "not a hint" fake))==0)
  let disabled=paste "not a hint" (modifyActive (\frame->frame {windowEditorMount=Nothing}) base {autocompleteACPEnabled=False})
  check "disabled ACP does not expose a composer" (activeEditorMount disabled==Nothing && not (windowHasEditor disabled (fromJust (activeWindow disabled))))
  let settings=Dialog "Autocomplete" (AutocompleteDialog "save")
        (ComboBox "Provider" ["Off","ACP","Copilot"] 1 Nothing:[Input ("Field "<>T.pack (show n)) "" 0 | n<-[2::Int ..7]]++[CheckBox "Debug pane" True]) 0 ["Save","Cancel"] ["Choose a backend."]
      rectangles=fieldRects (initialDesktop (80,25)) settings
      outer=dialogRect (initialDesktop (80,25)) settings
  check "autocomplete settings fit standard screen in two columns"
    (length rectangles==8 && length (filter ((==left (rectangles !! 0)).left) rectangles)==5 &&
      all (\r->top r+height r<=top outer+height outer-3) rectangles)
  let settingsDesktop=(initialDesktop (80,25)) {dialog=Just settings}
      step k=fst . key k []
      opened=step V.KEnter settingsDesktop
      preview=step V.KDown opened
      cancelled=step V.KEsc preview
      committed=step V.KEnter preview
      provider d=case dialog d of
        Just dg | ComboBox _ _ selected popup:_<-fields dg -> Just (selected,popup)
        _ -> Nothing
  check "provider dropdown opens, previews and cancels without dismissing settings"
    (provider opened==Just (1,Just 1) && provider preview==Just (1,Just 2) && provider cancelled==Just (1,Nothing))
  check "provider dropdown commits without submitting settings"
    (provider committed==Just (2,Nothing) && null (snd (key V.KEnter [] preview)))
  check "provider does not accept arbitrary text or paste"
    (provider (step (V.KChar '$') settingsDesktop)==Just (1,Nothing) && provider (paste "garbage" settingsDesktop)==Just (1,Nothing))
  let fieldRect=rectangles !! 0
      mouseOpened=fst (handleEvent (V.EvMouseDown (left fieldRect+1) (top fieldRect+1) V.BLeft []) settingsDesktop)
      popup=comboBoxRect mouseOpened settings 0 ["Off","ACP","Copilot"]
      mouseChosen=fst (handleEvent (V.EvMouseDown (left popup+1) (top popup+3) V.BLeft []) mouseOpened)
      (_,saved)=submitDialog 0 (fromJust (dialog mouseChosen)) mouseChosen
  check "mouse provider selection reaches autocomplete save in field order"
    (provider mouseChosen==Just (2,Nothing) && saved==[AutocompleteAction "save" (["0","Copilot"]++replicate 6 ""++["true"])])
  let hovered=fst (hoverAt (left popup+1) (top popup+1) mouseOpened)
      dismissed=fst (handleEvent (V.EvMouseDown (left outer+1) (top outer+1) V.BLeft []) hovered)
  check "hover previews a choice; clicking outside preserves the original"
    (provider hovered==Just (1,Just 0) && provider dismissed==Just (1,Nothing))
  let tabbedProvider=step (V.KChar '\t') preview
  check "Tab commits provider and advances focus" (provider tabbedProvider==Just (2,Nothing) && maybe False ((==1).focus) (dialog tabbedProvider))
  let narrow=dialogFieldLayout 50 23 settings
  check "narrow autocomplete settings retain stacked scrolling layout" (all ((==3).left) narrow)
  bracket temporary removeFile $ \path->do
    let secret=setComposerInput (newBuffer "EPHEMERAL-HINT-SECRET") (Selection 0 0) True filled
    writeCheckpoint path secret >>= either (error . T.unpack) pure
    bytes<-BS.readFile path
    check "checkpoint does not serialize hint text" (not (TE.encodeUtf8 "EPHEMERAL-HINT-SECRET" `BS.isInfixOf` bytes))
    restored<-readCheckpoint path secret >>= either (error . T.unpack) pure
    check "recovery clears hint draft and runtime enablement"
      (autocompleteWindow restored==Nothing && not (autocompleteACPEnabled restored) && activeEditorMount restored==Nothing && all ((/=E.mountDraft mount) . fst) (M.toList (editorDrafts restored)))
    check "recovery retains completion output only as an inert plugin view"
      (any ((=="completion trace\nresponse\n") . (\text->contentSlice text 0 (contentLength text)) . W.preparedWindowText) (pluginWindows restored) && not (activeAutocomplete restored))
    before<-checkpointKey base
    after<-checkpointKey typed
    check "ephemeral hint edits do not invalidate the recovery checkpoint" (before==after)
  putStrLn "Hint composer checks passed"

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)

temporary :: IO FilePath
temporary=do
  root<-getTemporaryDirectory
  (path,h)<-openTempFile root "thc-hint-check"
  hClose h
  pure path
