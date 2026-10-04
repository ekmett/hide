{-# LANGUAGE OverloadedStrings #-}
module HintComposerCheck (checks) where

import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import System.Directory
import System.IO (openTempFile,hClose)
import Hide.Buffer
import Hide.GuestAccess
import Hide.Model
import Hide.Recovery

checks :: IO ()
checks=do
  let base=(addReadOnly "Autocomplete" "completion trace\nresponse\n" (initialDesktop (90,30)))
        {autocompleteACPEnabled=True,composerBuffer=newBuffer "main chat draft",composerSelection=Selection 2 5,
         agentReplying=True,agentQueued=2,agentSteering=True}
      paste text d=fst (handleEvent (V.EvPaste (TE.encodeUtf8 text)) d)
      key k mods d=handleEvent (V.EvKey k mods) d
      typed=paste "hint λ" base
      multiline=fst (key V.KEnter [V.MShift] typed)
      filled=paste "    preserve indentation" multiline
      (submitted,effects)=key V.KEnter [] filled
      w=fromJust (activeWindow filled)
      rect=autocompleteComposerRect filled w
      checkDraft d=contents (composerBuffer d)=="main chat draft" && composerSelection d==Selection 2 5 && agentReplying d && agentQueued d==2 && agentSteering d
  check "ACP hint typing uses a dedicated draft" (contents (autocompleteDraft typed)=="hint λ" && checkDraft typed)
  check "Shift Enter inserts literal newline without chat submission" (contents (autocompleteDraft multiline)=="hint λ\n")
  check "Enter emits exact hint and clears only its own draft"
    (effects==[AutocompleteAction "hint" ["hint λ\n    preserve indentation"]] && bufferLength (autocompleteDraft submitted)==0 && autocompleteSelection submitted==Selection 0 0 && checkDraft submitted)
  check "empty hint does not submit" (null (snd (key V.KEnter [] submitted)))
  check "hint editing never changes trace document" (maybe False ((=="completion trace\nresponse\n") . contents . documentBuffer) (activeDocument submitted))
  let allHint=fst (runCommand SelectAll filled)
      copied=fst (runCommand Copy allHint)
      cut=fst (runCommand Cut copied)
      undone=fst (runCommand Undo cut)
  check "hint select copy cut and undo remain isolated"
    (clipboard copied=="hint λ\n    preserve indentation" && bufferLength (autocompleteDraft cut)==0 && contents (autocompleteDraft undone)==contents (autocompleteDraft filled) && checkDraft undone)
  let (escaped,escapeEffects)=key V.KEsc [] filled
      (tabbed,tabEffects)=key (V.KChar '\t') [] filled
  check "hint Escape and Tab do not cancel or steer chat" (null escapeEffects && null tabEffects && not (autocompleteFocused escaped) && not (autocompleteFocused tabbed) && checkDraft escaped)
  let (clicked,clickEffects)=handleEvent (V.EvMouseDown (left rect+4) (top rect+1) V.BLeft []) filled
  check "hint clicks retain literal indentation and independent selection"
    (null clickEffects && caret (autocompleteSelection clicked)==T.length "hint λ\n    " && composerSelection clicked==Selection 2 5)
  let doc=fromJust (activeDocument filled)
  check "hint composer reserves transcript rows only for ACP"
    (windowContentRows (filled {autocompleteACPEnabled=False}) doc w-windowContentRows filled doc w==height rect+1)
  check "agents cannot type or click hint composer"
    (not (guestKeyboardAllowed filled) && not (pointerAllowedAt filled (left rect) (top rect)) && not (readableAt filled (left rect) (top rect)))
  hintAllowed<-guestTransitionAllowed base typed []
  check "agent transition policy rejects hint edits and submission"
    (not hintAllowed && not (guestEffectsAllowed effects))
  check "agent can still read autocomplete trace above the private draft" (readableAt filled (left (bounds w)+2) (top (bounds w)+2))
  let disabled=paste "not a hint" (base {autocompleteACPEnabled=False})
  check "disabled ACP does not expose a composer" (bufferLength (autocompleteDraft disabled)==0)
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
    let secret=filled {autocompleteDraft=newBuffer "EPHEMERAL-HINT-SECRET"}
    writeCheckpoint path secret >>= either (error . T.unpack) pure
    bytes<-BS.readFile path
    check "checkpoint does not serialize hint text" (not (TE.encodeUtf8 "EPHEMERAL-HINT-SECRET" `BS.isInfixOf` bytes))
    restored<-readCheckpoint path secret >>= either (error . T.unpack) pure
    check "recovery clears hint draft and runtime enablement"
      (not (autocompleteACPEnabled restored) && bufferLength (autocompleteDraft restored)==0 && autocompleteSelection restored==Selection 0 0)
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
