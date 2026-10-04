{-# LANGUAGE OverloadedStrings #-}
module GuestAccessCheck (checks) where
import SourceWindowFixture (sourceFixtureBuffer)
import Control.Monad (unless,forM_)
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

checks :: IO ()
checks=do
  let check label ok=unless ok (error label)
      base=(addDocument Nothing (newBuffer "file") (initialDesktop (100,35))) {wordStar=False}
      sourceId=maybe (-1) sourceFixtureBuffer (activeWindow base)
      firstRect d dg=case fieldRects d dg of r:_->r; _->error "missing field rectangle"
      conversation=addReadOnly "Conversation" "Session: provider-secret\nPublic transcript\nOther: private answer" base
      chat=conversation {composerBuffer=newBuffer "private draft",composerFocused=True,
        chatActions=[(43,64,"question-input",[])],chatInputOffset=Just 50}
      window=case activeWindow chat of Just w->w; _->error "no chat window"
      ident=sourceFixtureBuffer window
      Rect x y _ _=bounds window
      draft=composerRect chat window
      denied d event=case P.applyGuestInput event d of Left _->True; _->False
  let bindings=either (error . show) id (platformBindings [] TerminalPlatform (M.singleton "source" (M.singleton "hide.options.agent-permissions" ["Ctrl+Shift+P"])))
  check "rebound protected command retains agent policy" (denied base {keyBindings=bindings} (P.Key "p" [V.MCtrl,V.MShift]))
  let macBindings=either (error . show) id (configuredBindings [] (M.singleton "macos" (M.singleton "source" (M.singleton "hide.options.agent-permissions" ["Cmd+Shift+P"]))))
  check "Command remap preserves protected host policy" (denied base {keyBindings=macBindings,nativeMac=True,videoMode=Just 3} (P.Key "p" [V.MMeta,V.MShift]))
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
    (windowId approvalWindow==approvalId && not (readableAt hiddenFirst approvalX approvalY) && not (pointerAllowedAt hiddenFirst approvalX approvalY) &&
     denied hiddenFirst (P.Mouse "down" approvalX approvalY 0 1 []))
  check "agents cannot stop the editor session" (denied base P.SuspendSession)
  check "guest Git review uses filtered workspace tool, not unrestricted human review"
    (not (guestCommandAllowed GitDiff) && not (guestCommandAllowed GitCommit) &&
     all (not . guestEffectsAllowed . pure) [ReadGitDiff,AskGitCommit,WriteGitCommit "message"])
  check "conversation transcript is public but draft and provider session ID are private"
    (readableAt chat (x+2) (y+2) && not (readableAt chat (x+2) (y+1)) && not (readableAt chat (left draft) (top draft)))
  check "conversation read sanitization preserves offsets and removes private spans"
    (case sanitizedBuffer chat ident of Just text -> T.length text==T.length (activeText chat) && not ("provider-secret" `T.isInfixOf` text) && "Public transcript" `T.isInfixOf` text && not ("private answer" `T.isInfixOf` text); _->False)
  check "whole conversation window is not clickable, even blank composer space"
    (not (pointerAllowedAt chat (x+2) (y+2)) && not (pointerAllowedAt chat (x+2) (top draft)))
  forM_ [P.Key "Enter" [],P.Key "x" [],P.Paste "answer",P.Mouse "down" (left draft) (top draft) 0 1 [],P.Mouse "down" (x+2) (top draft) 0 1 []] $ \event ->
    check "guest cannot type or click into human draft/answer controls" (denied chat event)
  let (human,_) = P.applyInputFrom HumanInput (P.Paste "human") chat
  check "human input still edits the conversation draft" (contents (composerBuffer human)/=contents (composerBuffer chat))
  let moved=P.applyInputFrom GuestInput (P.Key "F6" []) chat
  check "guest may focus a normal window away from conversation" (maybe False (not . protectedBuffer (fst moved) . sourceFixtureBuffer) (activeWindow (fst moved)))
  forM_ ["Agent request","Proposed agent edit","Git diff","Disk changes: /example/thc.toml"] $ \label -> do
    let review=addReadOnly label "private review" base
        bid=maybe (-1) sourceFixtureBuffer (activeWindow review)
    check "private review text is unavailable to generic buffer reads" (protectedBuffer review bid && sanitizedBuffer review bid==Nothing && denied review (P.Key "c" [V.MCtrl]))
  forM_ [AgentDialog "configure",AgentDialog "load",AgentDialog "approval:2",PermissionDialog "approve:2",DiscardDraft] $ \p -> do
    let d=base {dialog=Just (Dialog "Human control" p [Input "Session ID" "secret" 6] 0 ["OK","Cancel"] [])}
    check "sensitive modal blocks every guest event, including Escape/blur" (all (denied d) [P.Key "Enter" [],P.Key "Escape" [],P.Paste "x",P.Blur,P.Mouse "down" 0 0 0 1 []])
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
     not (guestCommandAllowed ChatInputOptions) && not (guestEffectsAllowed [SaveChatSubmit SteerSubmit]) &&
     all (denied chatPrefs) [P.Key "Enter" [],P.Key "ArrowDown" [],P.Key "Escape" [],P.Blur] &&
     not (guestTransitionAllowed base (base {chatSubmit=SteerSubmit}) []))
  let prefs=Dialog "Preferences" Settings [CheckBox "Streamer mode" False] 0 ["OK","Cancel"] []
      pd=base {dialog=Just prefs}
      pr=firstRect pd prefs
  check "guest cannot alter Streamer checkbox by pointer or keyboard"
    (not (pointerAllowedAt pd (left pr) (top pr)) && denied pd (P.Key " " []))
  forM_ [Input "API key" "human key" 0,SelectedInput "API key" "human key" (Selection 0 9)] $ \secretField -> do
    let secretDialog=Dialog "Service" Widgets [secretField,Input "Public name" "" 0] 0 ["OK"] []
        secretDesktop=base {dialog=Just secretDialog}
        secretRect=firstRect secretDesktop secretDialog
    check "secret fields in otherwise ordinary dialogs are readable-label only and immutable"
      (not (readableAt secretDesktop (left secretRect) (top secretRect+1)) && not (pointerAllowedAt secretDesktop (left secretRect) (top secretRect+1)) && denied secretDesktop (P.Paste "guest key") && denied secretDesktop (P.Key "x" []) && not (denied secretDesktop (P.Key "Tab" [])))
  let option=AgentSetting "apiKey" "API key" "authentication" "private-token" []
      dropdown=base {agentSettings=[option],contextMenu=Just (Rect 2 2 40 4,0),contextKind=AgentContext [("API key  private-token",AgentChoose "apiKey")]}
  check "agent dropdown public labels stay readable but secret values do not"
    (readableAt dropdown 4 3 && not (readableAt dropdown 14 3) && not (pointerAllowedAt dropdown 4 3))
  check "ordinary preferences and build dialogs remain usable"
    (not (denied base {dialog=Just (Dialog "Find" (Searching False "") [Input "Text" "" 0] 0 ["Find"] [])} (P.Paste "needle")) && guestEffectsAllowed [AgentAction "make" [],AgentAction "run-config" [],AgentAction "terminal-input" ["1","ls\n"]])
  check "agent lifecycle and permission effects are rejected"
    (not (guestEffectsAllowed [AgentAction "send-draft" []]) && not (guestEffectsAllowed [AgentAction "question-submit" []]) && not (guestEffectsAllowed [PermissionAction "show" []]))
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
    (protectedBuffer privateSource privateId && sanitizedBuffer privateSource privateId==Nothing && denied privateSource (P.Paste "replace") && not (readableAt privateSource (px+2) (py+2)) && not (streamerReadableAt privateSource (px+2) (py+2)))
  let child=addDocument (Just (FileState "/authority/nested/secret.hs" Nothing)) (newBuffer "private") base {guestPrivatePaths=["/authority"],streamerMode=True}
      projectConfig=addDocument (Just (FileState "/project/THC.toml" Nothing)) (newBuffer "private") base {streamerMode=True}
  check "streamer titles share canonical descendant and project-authority rules"
    (applicationTitle "/" child=="th [private]" && applicationTitle "/" projectConfig=="th [private]")
  let messages=(setProblemsVisible True base {guestPrivatePaths=["/authority"],
        diagnostics=[Diagnostic "/authority/secret.hs" Nothing 0 0 1 "secret-diagnostic-payload"]}) {problemsFocused=True}
      Rect mx my _ _=problemsRect messages
  check "protected diagnostic rows and copy commands are unavailable to agent input"
    (not (readableAt messages (mx+2) (my+1)) && denied messages (P.Key "c" [V.MCtrl]) && denied messages (P.MenuCommand CopyAllMessages) &&
     all (\(r,_,_)->denied messages (P.Mouse "down" (left r) (top r) 0 1 []))
       [(r,i,a) | (r,i,a@(Left c))<-statusItemRects messages,c `elem` [Copy,CopyAllMessages]])
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
    (denied privateDestination (P.Key "Backspace" []) && not (pointerAllowedAt privateDestination (left publicRect+1) (top publicRect+1)))
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
