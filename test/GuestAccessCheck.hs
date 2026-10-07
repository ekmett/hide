{-# LANGUAGE OverloadedStrings #-}
module GuestAccessCheck (checks) where
import EditorFixture (withEditorBodyFixture)
import SourceWindowFixture (sourceFixtureBuffer)
import Control.Monad (unless,forM_)
import Data.Aeson (object,(.=),withObject,(.:))
import Data.Aeson.Types (parseMaybe)
import Hide.ControlMCP (controlTool)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Hide.Browser (Entry(..))
import Hide.Files (FileState(..))
import Hide.Buffer
import Hide.GuestAccess
import Hide.Commands (platformBindings, configuredBindings)
import Hide.Bindings (BindingPlatform(TerminalPlatform))
import SidebarFixture
import Hide.Sidebar
import Hide.Model
import qualified Hide.Protocol as P
import qualified Hide.Plugin.Editor as E
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Window as W
import qualified Data.Vector as Vec
import Hide.Syntax (Style(..))

checks :: IO ()
checks=do
  let text="Session: provider-secret\nPublic transcript\nOther: private answer"
      hidden=Vec.fromList [(0,T.length "Session: provider-secret"),(T.length "Session: provider-secret\nPublic transcript\nOther: ",T.length text)]
      semantics=W.TextSemantics W.CopyText Nothing Vec.empty Vec.empty W.ReadableWindow hidden
        (Vec.singleton (0,T.length "Session: provider-secret")) Vec.empty
  body<-W.prepareSemanticTextWindow "Conversation" [(c,Plain) | c<-T.unpack text] semantics
    >>= either (error . T.unpack) pure
  withEditorBodyFixture "" body (addDocument Nothing (newBuffer "file") (initialDesktop (100,35))) checksWithBody

checksWithBody :: Desktop -> IO ()
checksWithBody conversation=do
  let check label ok=unless ok (error label)
      base=(addDocument Nothing (newBuffer "file") (initialDesktop (100,35))) {wordStar=False}
      sourceId=maybe (-1) sourceFixtureBuffer (activeWindow base)
      firstRect d dg=case fieldRects d dg of r:_->r; _->error "missing field rectangle"
      chat=setComposerInput (newBuffer "private draft") (Selection 0 0) True conversation
      window=case activeWindow chat of Just w->w; _->error "no chat window"
      Rect x y _ _=bounds window
      draft=composerRect chat window
      denied d event=either (const True) (const False) <$> P.applyGuestInput event d
      checkDenied label d events=forM_ events $ \event->check label =<< denied d event
  let poison=(newBuffer "retained live tree") {saved=error "guest guard forced baseline",undoStack=error "guest guard forced Undo",redoStack=error "guest guard forced Redo"}
      hidden=(newDocument poison Nothing) {documentLabel=Just "Agent request",documentHighlight=error "guest guard forced highlighting",documentSourceRows=error "guest guard forced source rows"}
      q=ChatQuestion 42 "Human question" ["Yes"] Nothing poison (Selection 0 0) False
      conflict=Conflict 1 0 (FileState "/public/source.hs" (Just (error "guest guard forced disk baseline"))) (Just (error "guest guard forced disk conflict"))
      retained=(setComposerInput poison (Selection 0 0) False conversation) {windows=windows base,buffers=M.insert 99 hidden (buffers conversation),autocompleteDraft=poison,chatQuestion=Just q,
        dialog=Just (Dialog "Conflict" (DiskConflict conflict) [Input "Name" "source.hs" 0] 0 ["OK"] [])}
  (_,reply)<-controlTool (\d _->pure (False,d)) retained "editor_input"
    (object ["events" .= [object ["type" .= ("blur"::T.Text)]]])
  outcome<-reply
  check "actual guest input never forces hidden protected payloads"
    (case outcome of Right value->parseMaybe (withObject "reply" (.: "appliedEvents")) value==Just (1::Int); _->False)
  forM_ [retained {buffers=M.adjust (\doc->doc {documentBuffer=newBuffer "replacement"}) 99 (buffers retained)},
         retained {buffers=M.adjust (\doc->doc {documentLabel=Just "Public"}) 99 (buffers retained)},
         retained {buffers=M.delete 99 (buffers retained)},
         setComposerInput (newBuffer "replacement") (Selection 0 0) False retained,
         retained {autocompleteDraft=newBuffer "replacement"},
         retained {chatQuestion=Just q {questionBuffer=newBuffer "replacement"}},
         retained {editorDrafts=M.map (\draftState->draftState {editorDraftSelection=Selection 1 1}) (editorDrafts retained)},
         retained {editorDrafts=M.map (\draftState->draftState {editorDraftFocused=True}) (editorDrafts retained)},
         retained {editorDrafts=M.empty}] $ \changed->
    check "protected replacement rejects unchanged numeric revisions" . not =<< guestTransitionAllowed retained changed []
  let rewrapped=retained {editorDrafts=M.map (\draftState->draftState
        {editorDraftBuffer=editorDraftBuffer draftState,editorDraftSelection=editorDraftSelection draftState}) (editorDrafts retained)}
  check "unchanged hidden draft wrappers do not force saved contents or Undo" =<< guestTransitionAllowed retained rewrapped []
  let publicInput=retained {dialog=Nothing,chatQuestion=Nothing}
  (_,typedReply)<-controlTool (\d _->pure (False,d)) publicInput "editor_input"
    (object ["events" .= [object ["type" .= ("key"::T.Text),"key" .= ("x"::T.Text)]]])
  typedOutcome<-typedReply
  check "ordinary guest editing retains hidden protected identity"
    (case typedOutcome of Right value->parseMaybe (withObject "reply" (.: "appliedEvents")) value==Just (1::Int); _->False)
  let privateUndo=base {buffers=M.adjust (\doc->doc {documentBuffer=poison,
        documentFile=Just (FileState "/project/thc.toml" Nothing)}) sourceId (buffers base)}
  checkDenied "private undo and redo reject keyboard, browser and menu routes before reading history" privateUndo
    [P.Key "z" [V.MCtrl],P.Key "z" [V.MCtrl,V.MShift],P.Key "y" [V.MCtrl],
     P.MenuCommand Undo,P.MenuCommand Redo,P.BrowserCommand Undo,P.BrowserCommand Redo]
  let bindings=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "source" (M.singleton "hide.options.agent-permissions" ["Ctrl+Shift+P"])))
  checkDenied "rebound protected command retains agent policy" base {keyBindings=bindings} [P.Key "p" [V.MCtrl,V.MShift]]
  let macBindings=either (error . show) id (configuredBindings [] (M.singleton "macos" (M.singleton "source" (M.singleton "hide.options.agent-permissions" ["Cmd+Shift+P"]))))
  checkDenied "Command remap preserves protected host policy" base {keyBindings=macBindings,nativeMac=True,videoMode=Just 3} [P.Key "p" [V.MMeta,V.MShift]]
  let terminal=addReadOnly "Terminal hidden" "old output" base
      terminalId=maybe (error "terminal missing") windowId (activeWindow terminal)
      docked=setTerminalPinned True terminalId terminal
      withMessages=setProblemsVisible True docked
      protected=addReadOnly "Agent request" "private body" withMessages
      approvalId=maybe (error "approval missing") windowId (activeWindow protected)
      -- A stale hidden view must not participate in privacy hit testing even
      -- when its old rectangle overlaps a visible approval/source window.
      overlap=protected {windows=map (\w->if windowId w==terminalId then w {bounds=Rect 0 1 100 25} else w) (windows protected)}
      hiddenFirst=overlap {windows=filter ((==terminalId).windowId) (windows overlap)++filter ((/=terminalId).windowId) (windows overlap)}
      approvalWindow=maybe (error "approval missing") id (activeWindow hiddenFirst)
      approvalX=left (bounds approvalWindow)+2; approvalY=top (bounds approvalWindow)+2
  check "hidden pinned terminal cannot mask approval privacy or intercept its coordinates"
    (windowId approvalWindow==approvalId && not (readableAt hiddenFirst approvalX approvalY) && not (pointerAllowedAt hiddenFirst approvalX approvalY))
  checkDenied "hidden approval rejects pointer input" hiddenFirst [P.Mouse "down" approvalX approvalY 0 1 []]
  checkDenied "agents cannot stop the editor session" base [P.SuspendSession]
  check "guest Git review uses filtered workspace tool, not unrestricted human review"
    (not (guestCommandAllowed GitDiff) && not (guestCommandAllowed GitCommit) &&
     all (not . guestEffectsAllowed . pure) [ReadGitDiff,AskGitCommit,WriteGitCommit "message"])
  check "conversation transcript is public but draft and provider session ID are private"
    (readableAt chat (x+2) (y+2) && not (readableAt chat (x+2) (y+1)) && not (readableAt chat (left draft) (top draft)))
  check "conversation read sanitization preserves offsets and removes private spans"
    (case windowPluginText chat window >>= sanitizedPreparedContent of
      Just (True,content)->let text=contentSlice content 0 (contentLength content)
        in contentLength content==maybe (-1) (contentLength.W.preparedWindowText) (windowPluginText chat window) &&
           not ("provider-secret" `T.isInfixOf` text) && "Public transcript" `T.isInfixOf` text && not ("private answer" `T.isInfixOf` text)
      _->False)
  check "whole conversation window is not clickable, even blank composer space"
    (not (pointerAllowedAt chat (x+2) (y+2)) && not (pointerAllowedAt chat (x+2) (top draft)))
  forM_ [P.Key "Enter" [],P.Key "x" [],P.Paste "answer",P.Mouse "down" (left draft) (top draft) 0 1 [],P.Mouse "down" (x+2) (top draft) 0 1 []] $ \event ->
    checkDenied "guest cannot type or click into human draft/answer controls" chat [event]
  let genericText="Public generic body"
      publicSemantics=W.TextSemantics W.CopyText Nothing Vec.empty Vec.empty W.ReadableWindow Vec.empty Vec.empty Vec.empty
  publicBody<-W.prepareSemanticTextWindow "Plain text" [(c,Plain) | c<-T.unpack genericText] publicSemantics
    >>= either (error . T.unpack) pure
  withEditorBodyFixture "" publicBody base $ \mounted->do
    let generic=mounted {conversationViews=M.empty}
        genericWindow=maybe (error "generic editor missing") id (activeWindow generic)
        genericDraft=composerRect generic genericWindow
        Rect gx gy _ _=bounds genericWindow
    check "attached input is private regardless of body title while its body stays readable"
      (readableAt generic (gx+2) (gy+1) && not (readableAt generic (left genericDraft) (top genericDraft)) &&
       not (pointerAllowedAt generic (left genericDraft) (top genericDraft)) &&
       not (guestKeyboardAllowed generic) && not (guestCommandAllowedIn generic Copy) &&
       case sanitizedPreparedContent publicBody of
         Just (False,content)->contentSlice content 0 (contentLength content)==genericText
         _->False)
    checkDenied "generic attached editor rejects guest text, clipboard and pointer routes" generic
      [P.Paste "guest",P.Key "x" [],P.Key "c" [V.MCtrl],P.MenuCommand Copy,P.BrowserCommand Copy,
       P.Mouse "down" (left genericDraft) (top genericDraft) 0 1 []]
    check "human streamer presentation keeps the attached draft visible"
      (streamerReadableAt generic {streamerMode=True} (left genericDraft) (top genericDraft))
  let (human,_) = P.applyInput (P.Paste "human") chat
  check "human input still edits the conversation draft" (contents (composerBuffer human)/=contents (composerBuffer chat))
  let navigating=chat {keyBindings=either (error . T.unpack) id (configuredBindings [] M.empty)}
  check "guest window navigation resolves the configured conversation command"
    (boundKeyCommand (V.KFun 6) [] navigating==Just NextWindow)
  moved<-either (error . T.unpack) pure =<< P.applyGuestInput (P.Key "F6" []) navigating
  check "guest may focus the normal source window away from conversation"
    (maybe False (\w->bufferId w==Just sourceId && not (protectedWindow (fst moved) w)) (activeWindow (fst moved)))
  forM_ ["Agent request","Proposed agent edit","Git diff","Disk changes: /example/thc.toml"] $ \label -> do
    let review=addReadOnly label "private review" base
        bid=maybe (-1) sourceFixtureBuffer (activeWindow review)
    check "private review text is unavailable to generic buffer reads" (protectedBuffer review bid && sanitizedBuffer review bid==Nothing)
    checkDenied "private review rejects Copy" review [P.Key "c" [V.MCtrl]]
  forM_ [AgentDialog "configure",AgentDialog "load",AgentDialog "approval:2",PermissionDialog "approve:2",DiscardDraft] $ \p -> do
    let d=base {dialog=Just (Dialog "Human control" p [Input "Session ID" "secret" 6] 0 ["OK","Cancel"] [])}
    checkDenied "sensitive modal blocks every guest event, including Escape/blur" d [P.Key "Enter" [],P.Key "Escape" [],P.Paste "x",P.Blur,P.Mouse "down" 0 0 0 1 []]
    let dg=maybe (error "dialog") id (dialog d); r=firstRect d dg
    case p of
      PermissionDialog{} -> check "permission controls are entirely private" (not (readableAt d (left r) (top r)) && not (readableAt d (left r) (top r+1)))
      _ -> check "agent setting labels are public while sensitive values are hidden" (readableAt d (left r) (top r) && not (readableAt d (left r) (top r+1)))
  let dg=Dialog "Agents" (AgentDialog "configure") [Input "Executable" "claude" 0,Input "Environment (JSON object)" "TOKEN=secret" 0] 0 ["OK"] []
      settings=base {dialog=Just dg}
      public=firstRect settings dg
  check "nonsensitive agent settings can be read but not edited"
    (readableAt settings (left public) (top public+1) && not (pointerAllowedAt settings (left public) (top public+1)))
  let (chatPrefs,_)=runCommand ChatInputOptions base
      chatDialog=maybe (error "missing chat input dialog") id (dialog chatPrefs)
      chatRect=firstRect chatPrefs chatDialog
  check "chat input defaults are public but human-controlled"
    (readableAt chatPrefs (left chatRect) (top chatRect+1) && not (pointerAllowedAt chatPrefs (left chatRect) (top chatRect+1)) &&
     not (guestCommandAllowed ChatInputOptions) && not (guestEffectsAllowed [SaveChatSubmit SteerSubmit]))
  checkDenied "chat preferences reject guest input" chatPrefs [P.Key "Enter" [],P.Key "ArrowDown" [],P.Key "Escape" [],P.Blur]
  check "chat submit changes reject guest transitions" . not =<< guestTransitionAllowed base (base {chatSubmit=SteerSubmit}) []
  let prefs=Dialog "Preferences" Settings [CheckBox "Streamer mode" False] 0 ["OK","Cancel"] []
      pd=base {dialog=Just prefs}
      pr=firstRect pd prefs
  check "guest cannot alter Streamer checkbox by pointer or keyboard"
    (not (pointerAllowedAt pd (left pr) (top pr)))
  checkDenied "Streamer checkbox rejects keyboard" pd [P.Key " " []]
  forM_ [Input "API key" "human key" 0,SelectedInput "API key" "human key" (Selection 0 9)] $ \secretField -> do
    let secretDialog=Dialog "Service" Widgets [secretField,Input "Public name" "" 0] 0 ["OK"] []
        secretDesktop=base {dialog=Just secretDialog}
        secretRect=firstRect secretDesktop secretDialog
    check "secret fields in otherwise ordinary dialogs are readable-label only and immutable"
      (not (readableAt secretDesktop (left secretRect) (top secretRect+1)) && not (pointerAllowedAt secretDesktop (left secretRect) (top secretRect+1)))
    checkDenied "private value rejects text input" secretDesktop [P.Paste "guest key",P.Key "x" []]
    check "private value allows field navigation" . not =<< denied secretDesktop (P.Key "Tab" [])
  let option=AgentSetting "apiKey" "API key" "authentication" "private-token" []
      dropdown=base {agentSettings=[option],contextMenu=Just (Rect 2 2 40 4,0),contextKind=AgentContext [("API key  private-token",AgentChoose "apiKey")]}
  check "agent dropdown public labels stay readable but secret values do not"
    (readableAt dropdown 4 3 && not (readableAt dropdown 14 3) && not (pointerAllowedAt dropdown 4 3))
  check "ordinary preferences remain usable" . not =<< denied base {dialog=Just (Dialog "Find" (Searching False "") [Input "Text" "" 0] 0 ["Find"] [])} (P.Paste "needle")
  check "build dialogs remain usable" (guestEffectsAllowed [ServiceAction "make" [],ServiceAction "run-config" [],ServiceAction "terminal-input" ["1","ls\n"]])
  check "human editor, agent question and permission effects are rejected"
    (not (guestEffectsAllowed [SubmitEditor (maybe (error "missing fixture mount") id (activeWindow conversation >>= windowEditorMount)) E.DefaultEditor Menu.HumanMenu]) && not (guestEffectsAllowed [AgentAction "question-submit" []]) && not (guestEffectsAllowed [PermissionAction "show" []]))
  let sticky=base {prefix=Just 'k',heldModifiers=[V.MCtrl],drag=Just (Selecting 0),buttonPressed=Just 0,clipboard="human secret",clipboardCode=Just "human secret",blockStart=Just (0,0)}
      isolated=beginGuestInput sticky
  check "batch isolation removes human gestures and clipboard"
    (prefix isolated==Nothing && heldModifiers isolated==[] && drag isolated==Nothing && buttonPressed isolated==Nothing && clipboard isolated=="" && clipboardCode isolated==Nothing && blockStart isolated==Nothing)
  let finished=endGuestInput sticky isolated {clipboard="guest copy",prefix=Just 'q',drag=Just (Selecting 0)}
  check "guest gestures cannot carry into human input and human clipboard is retained"
    (clipboard finished=="human secret" && clipboardCode finished==Just "human secret" && prefix finished==Nothing && drag finished==Nothing)
  check "known Session status is hidden independent of Streamer mode"
    (sanitizedStatus base {status="Session provider-secret"}=="Session [redacted]" && not (streamerReadableAt base {status="Session provider-secret"} 5 34))
  check "ordinary source text that looks like a Session header stays public"
    (sanitizedBuffer (base {buffers=M.adjust (\doc->doc {documentBuffer=newBuffer "Session: ordinary source"}) sourceId (buffers base)}) sourceId==Just "Session: ordinary source")
  let privatePath="/authority/config.toml"
      privateSource=addDocument (Just (FileState privatePath Nothing)) (newBuffer "private settings") base {guestPrivatePaths=[privatePath]}
      privateWindow=maybe (error "missing private source") id (activeWindow privateSource)
      privateId=sourceFixtureBuffer privateWindow
      Rect px py _ _=bounds privateWindow
  check "host authority paths protect human-open buffers and input"
    (protectedBuffer privateSource privateId && sanitizedBuffer privateSource privateId==Nothing && not (readableAt privateSource (px+2) (py+2)) && not (streamerReadableAt privateSource (px+2) (py+2)))
  checkDenied "private source rejects pasted text" privateSource [P.Paste "replace"]
  let child=addDocument (Just (FileState "/authority/nested/secret.hs" Nothing)) (newBuffer "private") base {guestPrivatePaths=["/authority"],streamerMode=True}
      projectConfig=addDocument (Just (FileState "/project/THC.toml" Nothing)) (newBuffer "private") base {streamerMode=True}
  check "streamer titles share canonical descendant and project-authority rules"
    (applicationTitle "/" child=="th [private]" && applicationTitle "/" projectConfig=="th [private]")
  let messages=(setProblemsVisible True base {guestPrivatePaths=["/authority"],
        diagnostics=[Diagnostic "/authority/secret.hs" Nothing 0 0 1 "secret-diagnostic-payload"]}) {problemsFocused=True}
      Rect mx my _ _=problemsRect messages
  check "protected diagnostic rows are unavailable to agent input" (not (readableAt messages (mx+2) (my+1)))
  checkDenied "protected Messages reject copy actions" messages
    ([P.Key "c" [V.MCtrl],P.MenuCommand CopyAllMessages]++
     [P.Mouse "down" (left r) (top r) 0 1 [] | (r,_,Left c)<-statusItemRects messages,c `elem` [Copy,CopyAllMessages]])
  let saveAs=fst (runCommand SaveAs privateSource)
      saveDialog=maybe (error "missing Save As dialog") id (dialog saveAs)
      saveRect=firstRect saveAs saveDialog
      publicSave=fst (runCommand SaveAs base)
      publicDialog=maybe (error "missing public Save As dialog") id (dialog publicSave)
      publicRect=firstRect publicSave publicDialog
  check "Save As hides protected filenames from guest and Streamer views"
    (readableAt saveAs (left saveRect) (top saveRect) && not (readableAt saveAs (left saveRect+1) (top saveRect+1)) && not (streamerReadableAt saveAs (left saveRect+1) (top saveRect+1)))
  check "Save As keeps ordinary destination filenames readable"
    (readableAt publicSave (left publicRect+1) (top publicRect+1) && streamerReadableAt publicSave (left publicRect+1) (top publicRect+1))
  let privateDestination=publicSave {guestPrivatePaths=[privatePath],dialog=Just publicDialog {fields=[Input "Name" (T.pack privatePath) (length privatePath)]}}
  check "guest cannot edit a masked Save As destination to reveal its prefix"
    (not (pointerAllowedAt privateDestination (left publicRect+1) (top publicRect+1)))
  checkDenied "private save destination rejects editing" privateDestination [P.Key "Backspace" []]
  check "private path guards include ancestors without prefix sibling confusion"
    (protectedPath privateSource privatePath && protectedPathParent privateSource "/authority" && not (protectedPath privateSource "/authority/config.toml.example") && not (protectedPathParent privateSource "/authority-sibling"))
  privateTree<-sidebarFixture "/authority" [("config.toml",privatePath),("public.hs","/authority/public.hs")] base {guestPrivatePaths=[privatePath]}
  check "private sidebar filenames are hidden while ordinary filenames remain visible"
    (not (readableAt privateTree 5 3) && not (streamerReadableAt privateTree 5 3) && readableAt privateTree 5 4)
  let entries=[Entry "config.toml" False Nothing Nothing,Entry "public.hs" False Nothing Nothing]
      browser=openBrowser "/authority" "*" entries base {guestPrivatePaths=[privatePath]}
      browserDialog=maybe (error "missing browser") id (dialog browser)
      fileRect=case fieldRects browser browserDialog of _:r:_->r; _->error "missing browser list"
  check "private browser names and selected details share guest and Streamer masks"
    (not (readableAt browser (left fileRect+2) (top fileRect+2)) && not (streamerReadableAt browser (left fileRect+2) (top fileRect+12)) && readableAt browser (left fileRect+2) (top fileRect+3))
  forM_ [Opening "/authority" "*" [],ChangingDirectory "/authority" []] $ \browserPurpose ->
    forM_ [Input "Name" "/authority/secret.hs" 0,SelectedInput "Name" "/authority/secret.hs" (Selection 0 20)] $ \nameField -> do
      let dg=Dialog "Browser" browserPurpose [nameField] 0 ["OK"] []
          view=base {dialog=Just dg,guestPrivatePaths=["/authority"]}
          r=firstRect view dg
      check "browser text widgets share protected pathname masks"
        (not (readableAt view (left r) (top r+1)) && not (streamerReadableAt view (left r) (top r+1)))
  putStrLn "guest access checks passed"
