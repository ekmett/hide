{-# LANGUAGE OverloadedStrings #-}
module RequestedPasteCheck (checks) where

import EditorFixture (withEditorFixture,withAutocompleteFixture)
import Control.Monad (unless,forM_)
import Data.Aeson (object,(.=))
import Data.Aeson.Types (parseEither)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Hide.Buffer
import Hide.Model
import qualified Hide.Protocol as P
import Hide.RequestedPaste

checks :: IO ()
checks=do
  requests<-newRequestedPaste
  let source=addDocument Nothing (newBuffer "source") (initialDesktop (80,25)) {browserFrontend=True}
      dg=Dialog "Rename" Information [SelectedInput "Name" "old" (Selection 0 3)] 0 ["OK","Cancel"] []
      opened=source {dialog=Just dg}
      check name ok=unless ok (fail name)
      token d=do
        let (next,effects)=P.applyInput (P.BrowserCommand Paste) d
        check "Production named Paste requests a clipboard read" (ReadBrowserClipboard `elem` effects)
        fromJust <$> requestPaste requests next
      unchanged d=check "Expired requested paste cannot mutate source or dialog" (activeText d=="source" && dialog d==Nothing)
      stale d lost restored=do
        receipt<-token d
        refreshRequestedPaste requests lost
        (arrived,_)<-applyRequestedPaste requests receipt "CLIPBOARD" restored
        check "Input lifetime cannot revive after an intermediate transition" (activeText arrived==activeText restored && fmap fields (dialog arrived)==fmap fields (dialog restored))
  receipt<-token opened
  let closed=fst (handleEvent (V.EvKey V.KEsc []) opened)
  refreshRequestedPaste requests closed
  (arrived,_)<-applyRequestedPaste requests receipt "CLIPBOARD" closed
  unchanged arrived -- Original production RED: this previously wrote CLIPBOARDsource.
  stale opened closed opened -- Reuse the SAME immutable dialog value after close.
  let edited=fst (handleEvent (V.EvKey (V.KChar 'x') []) opened)
      selected=fst (handleEvent (V.EvKey V.KRight []) opened)
  stale opened edited opened
  stale opened selected opened
  let split=fst (runCommand SplitVertical source)
      first=windowId (case windows split of firstWindow:_->firstWindow; []->error "paste split window missing"); last'=windowId (last (windows split))
      focused=focusWindow first split
  stale focused (focusWindow last' split) focused
  let bid=fromJust (bufferId (fromJust (activeWindow source)))
      replacement=source {buffers=M.adjust (\doc->doc {documentBuffer=newBuffer "equal revision replacement"}) bid (buffers source)}
  stale source replacement source
  accepted<-token opened
  -- The actual pointer-move path leaves the immutable dialog untouched.
  let hover=fst (P.applyInput (P.Mouse "move" (-1) (-1) 0 0 []) opened)
  refreshRequestedPaste requests hover
  (pasted,_)<-applyRequestedPaste requests accepted "accepted" hover
  check "Matching requested paste reaches the selected field without touching source" (activeText pasted=="source" && case fields <$> dialog pasted of Just [SelectedInput _ "accepted" _]->True; _->False)
  (duplicate,_)<-applyRequestedPaste requests accepted "duplicate" pasted
  check "Accepted reply is consumed once" (fmap fields (dialog duplicate)==fmap fields (dialog pasted))
  old<-token source
  newer<-token source
  (oldReply,_)<-applyRequestedPaste requests old "old" source
  (newReply,_)<-applyRequestedPaste requests newer "new" oldReply
  check "Old reply cannot consume a newer pending receipt" (activeText newReply=="newsource")
  cancelled<-token source
  cancelRequestedPaste requests
  (reconnected,_)<-applyRequestedPaste requests cancelled "stale" source
  check "Disconnected receipt cannot survive a new attachment" (activeText reconnected=="source")
  let wire=object ["type" .= ("paste-reply"::T.Text),"request" .= newer,"text" .= ("requested"::T.Text)]
  check "Requested reply parses distinctly and pure input fails closed" (parseEither P.parseInput wire==Right (P.PasteReply newer "requested") && activeText (fst (P.applyInput (P.PasteReply newer "requested") source))=="source")
  denied<-P.applyGuestInput (P.PasteReply newer "spoof") opened {dialog=Just dg {purpose=PermissionDialog "fixture"}}
  check "Guest cannot use an observed receipt for a human form" (case denied of Left _->True; _->False)
  forM_ ["",T.replicate 48 "G",T.replicate 49 "a"] $ \bad->check "Malformed request identity is rejected" (case parseEither P.parseInput (object ["type" .= ("paste-reply"::T.Text),"request" .= bad,"text" .= ("x"::T.Text)]) of Left _->True; _->False)
  check "Native/TUI requested replies use the parsed stamped schema" (P.clipboardReplyInput newer "requested"==Just wire && P.clipboardReplyInput "bad" "requested"==Nothing)
  check "Ordinary paste stays ordinary" (activeText (fst (P.applyInput (P.Paste "direct") source))=="directsource")
  let poisoned=source {buffers=M.adjust (\doc->doc {documentBuffer=(documentBuffer doc) {undoStack=error "Paste snapshot forced Undo"}}) bid (buffers source)}
  _<-token poisoned
  refreshRequestedPaste requests poisoned
  -- PTY output replacement does not replace its paste recipient.
  let terminal=addReadOnly "Terminal test" "old output" (initialDesktop (80,25)) {browserFrontend=True}
  terminalToken<-token terminal
  let output=terminal {buffers=M.map (\doc->doc {documentBuffer=newBuffer "new output"}) (buffers terminal)}
  refreshRequestedPaste requests output
  (_,effects)<-applyRequestedPaste requests terminalToken "terminal input" output
  check "PTY output does not expire requested input" (effects==[ServiceAction "terminal-input" ["test","terminal input"]])
  withEditorFixture "" source $ \mounted->do
    let chat=setComposerInput (newBuffer "draft") (Selection 5 5) True mounted
    matching<-token chat
    refreshRequestedPaste requests chat
    (inserted,_)<-applyRequestedPaste requests matching " accepted" chat
    check "a matching requested reply edits only its actual host-owned draft"
      (contents (composerBuffer inserted)=="draft accepted" && activeText inserted==activeText chat)
    leaving<-token chat
    refreshRequestedPaste requests source
    (returned,_)<-applyRequestedPaste requests leaving "stale" chat
    check "returning to the same draft cannot revive a clipboard receipt"
      (revision (composerBuffer returned)==revision (composerBuffer chat) && composerSelection returned==composerSelection chat)
    editedDraft<-token chat
    refreshRequestedPaste requests (setComposerInput (replaceSelection (Selection 0 0) "x" (composerBuffer chat)) (Selection 5 5) True chat)
    (restoredDraft,_)<-applyRequestedPaste requests editedDraft "stale" chat
    check "restoring an old immutable draft after an edit cannot revive requested input"
      (revision (composerBuffer restoredDraft)==revision (composerBuffer chat))
    replacedFrame<-token chat
    -- A newly prepared frame for the same named target receives distinct opaque
    -- editor ownership, even when its input tree and selection are transferred.
    withEditorFixture "" chat $ \newFrame->do
      let replacementFrame=setComposerInput (composerBuffer chat) (composerSelection chat) True newFrame
      refreshRequestedPaste requests replacementFrame
      (obsolete,_)<-applyRequestedPaste requests replacedFrame "stale" chat
      check "a replacement editor attachment cannot preserve a prior clipboard receipt"
        (activeEditorMount replacementFrame/=activeEditorMount chat && revision (composerBuffer obsolete)==revision (composerBuffer chat))
    let poisonDraft=(composerBuffer chat) {undoStack=error "editor paste receipt forced Undo",saved=error "editor paste receipt forced saved contents"}
        retained=setComposerInput poisonDraft (composerSelection chat) True chat
    _<-token retained
    refreshRequestedPaste requests retained
  withAutocompleteFixture "readable completion trace" source $ \mounted->do
    let hint=setComposerInput (newBuffer "hint") (Selection 4 4) True mounted
        unchangedHint d=contents (composerBuffer d)=="hint" && composerSelection d==Selection 4 4
    matching<-token hint
    (inserted,_)<-applyRequestedPaste requests matching "\n    literal hint" hint
    check "hint requested paste uses the ordinary mounted plain-text editor"
      (contents (composerBuffer inserted)=="hint\n    literal hint" && activeText inserted==activeText hint)
    lostFocus<-token hint
    refreshRequestedPaste requests (setComposerInput (composerBuffer hint) (composerSelection hint) False hint)
    (focusedAgain,_)<-applyRequestedPaste requests lostFocus "stale" hint
    check "hint focus departure permanently expires a requested paste" (unchangedHint focusedAgain)
    changedVersion<-token hint
    refreshRequestedPaste requests (setComposerInput (newBuffer "replacement") (composerSelection hint) True hint)
    (restoredHint,_)<-applyRequestedPaste requests changedVersion "stale" hint
    check "hint immutable replacement expires requested paste despite matching numeric revision" (unchangedHint restoredHint)
    oldMount<-token hint
    let closedHint=closeActive hint
    refreshRequestedPaste requests closedHint
    withAutocompleteFixture "replacement completion trace" closedHint $ \replacementHint->do
      let next=setComposerInput (composerBuffer hint) (composerSelection hint) True replacementHint
      (reopened,_)<-applyRequestedPaste requests oldMount "stale" next
      check "hint frame replacement cannot revive requested paste"
        (activeEditorMount next/=activeEditorMount hint && unchangedHint reopened)
  putStrLn "requested paste checks passed"
