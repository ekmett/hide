{-# LANGUAGE OverloadedStrings #-}
module RequestedPasteCheck (checks) where

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
  reads<-newRequestedPaste
  let source=addDocument Nothing (newBuffer "source") (initialDesktop (80,25)) {browserFrontend=True}
      dg=Dialog "Rename" Information [SelectedInput "Name" "old" (Selection 0 3)] 0 ["OK","Cancel"] []
      opened=source {dialog=Just dg}
      check name ok=unless ok (fail name)
      token d=do
        let (next,effects)=P.applyInput (P.BrowserCommand Paste) d
        check "Production named Paste requests a clipboard read" (ReadBrowserClipboard `elem` effects)
        fromJust <$> requestPaste reads next
      unchanged d=check "Expired requested paste cannot mutate source or dialog" (activeText d=="source" && dialog d==Nothing)
      stale d lost restored=do
        receipt<-token d
        refreshRequestedPaste reads lost
        (arrived,_)<-applyRequestedPaste reads receipt "CLIPBOARD" restored
        check "Input lifetime cannot revive after an intermediate transition" (activeText arrived==activeText restored && fmap fields (dialog arrived)==fmap fields (dialog restored))
  receipt<-token opened
  let closed=fst (handleEvent (V.EvKey V.KEsc []) opened)
  refreshRequestedPaste reads closed
  (arrived,_)<-applyRequestedPaste reads receipt "CLIPBOARD" closed
  unchanged arrived -- Original production RED: this previously wrote CLIPBOARDsource.
  stale opened closed opened -- Reuse the SAME immutable dialog value after close.
  let edited=fst (handleEvent (V.EvKey (V.KChar 'x') []) opened)
      selected=fst (handleEvent (V.EvKey V.KRight []) opened)
  stale opened edited opened
  stale opened selected opened
  let split=fst (runCommand SplitVertical source)
      first=windowId (head (windows split)); last'=windowId (last (windows split))
      focused=focusWindow first split
  stale focused (focusWindow last' split) focused
  let bid=fromJust (bufferId (fromJust (activeWindow source)))
      replacement=source {buffers=M.adjust (\doc->doc {documentBuffer=newBuffer "equal revision replacement"}) bid (buffers source)}
  stale source replacement source
  accepted<-token opened
  -- The actual pointer-move path leaves the immutable dialog untouched.
  let hover=fst (P.applyInput (P.Mouse "move" (-1) (-1) 0 0 []) opened)
  refreshRequestedPaste reads hover
  (pasted,_)<-applyRequestedPaste reads accepted "accepted" hover
  check "Matching requested paste reaches the selected field without touching source" (activeText pasted=="source" && case fields <$> dialog pasted of Just [SelectedInput _ "accepted" _]->True; _->False)
  (duplicate,_)<-applyRequestedPaste reads accepted "duplicate" pasted
  check "Accepted reply is consumed once" (fmap fields (dialog duplicate)==fmap fields (dialog pasted))
  old<-token source
  newer<-token source
  (oldReply,_)<-applyRequestedPaste reads old "old" source
  (newReply,_)<-applyRequestedPaste reads newer "new" oldReply
  check "Old reply cannot consume a newer pending receipt" (activeText newReply=="newsource")
  cancelled<-token source
  cancelRequestedPaste reads
  (reconnected,_)<-applyRequestedPaste reads cancelled "stale" source
  check "Disconnected receipt cannot survive a new attachment" (activeText reconnected=="source")
  let wire=object ["type" .= ("paste-reply"::T.Text),"request" .= newer,"text" .= ("requested"::T.Text)]
  check "Requested reply parses distinctly and pure input fails closed" (parseEither P.parseInput wire==Right (P.PasteReply newer "requested") && activeText (fst (P.applyInput (P.PasteReply newer "requested") source))=="source")
  denied<-P.applyGuestInput (P.PasteReply newer "spoof") opened {dialog=Just dg {purpose=AgentNewDialog}}
  check "Guest cannot use an observed receipt for a human form" (case denied of Left _->True; _->False)
  forM_ ["",T.replicate 48 "G",T.replicate 49 "a"] $ \bad->check "Malformed request identity is rejected" (case parseEither P.parseInput (object ["type" .= ("paste-reply"::T.Text),"request" .= bad,"text" .= ("x"::T.Text)]) of Left _->True; _->False)
  check "Native/TUI requested replies use the parsed stamped schema" (P.clipboardReplyInput newer "requested"==Just wire && P.clipboardReplyInput "bad" "requested"==Nothing)
  check "Ordinary paste stays ordinary" (activeText (fst (P.applyInput (P.Paste "direct") source))=="directsource")
  let poisoned=source {buffers=M.adjust (\doc->doc {documentBuffer=(documentBuffer doc) {undoStack=error "Paste snapshot forced Undo"}}) bid (buffers source)}
  _<-token poisoned
  refreshRequestedPaste reads poisoned
  -- PTY output replacement does not replace its paste recipient.
  let terminal=addReadOnly "Terminal test" "old output" (initialDesktop (80,25)) {browserFrontend=True}
  terminalToken<-token terminal
  let output=terminal {buffers=M.map (\doc->doc {documentBuffer=newBuffer "new output"}) (buffers terminal)}
  refreshRequestedPaste reads output
  (_,effects)<-applyRequestedPaste reads terminalToken "terminal input" output
  check "PTY output does not expire requested input" (effects==[AgentAction "terminal-input" ["test","terminal input"]])
  putStrLn "requested paste checks passed"
