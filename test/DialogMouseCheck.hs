{-# LANGUAGE OverloadedStrings #-}
module DialogMouseCheck (checks) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import THC.Edit.Model
import THC.Edit.Render (snapshotHtml, snapshot, renderDesktop)
import THC.Edit.Buffer (newBuffer)
import THC.Edit.Browser (Entry(..))
import THC.Edit.Files (FileState(..))
import qualified Data.Text as T
import qualified Graphics.Vty as V

checks :: IO ()
checks = do
  let check name ok = unless ok (error name)
      at n xs = case drop n xs of value:_ -> value; _ -> error "missing test fixture"
      desktop = addDocument Nothing (newBuffer "hello world") (initialDesktop (80,25))
      modal = fst (runCommand About desktop)
      dg = fromMaybe (error "missing About dialog") (dialog modal)
      Rect bx by _ _ = at 0 (buttonRects modal dg)
      (pressed,requests) = handleEvent (V.EvMouseDown bx by V.BLeft []) modal
      (released,_) = handleEvent (V.EvMouseUp bx by (Just V.BLeft)) pressed
      (cancelled,_) = handleEvent (V.EvMouseUp 0 0 (Just V.BLeft)) pressed
  check "button press waits for release" (dialog pressed /= Nothing && null requests)
  check "button release activates" (dialog released == Nothing)
  check "release outside cancels button" (dialog cancelled /= Nothing)
  check "button press is visibly distinct" (snapshotHtml modal /= snapshotHtml pressed)
  check "pressed button loses its raised shadow" (not ("▀" `T.isInfixOf` snapshot pressed || "▄" `T.isInfixOf` snapshot pressed))
  check "pressed button moves its face without changing label padding" ("background:rgb(0,85,0)'>  O" `T.isInfixOf` snapshotHtml pressed)
  let hovered=fst (hoverAt bx by modal)
  check "button hover is visibly distinct" (snapshotHtml modal /= snapshotHtml hovered)
  check "hover leaving clears highlight" (buttonHover (fst (hoverAt 0 0 hovered))==Nothing)
  check "button hover preserves keyboard focus" (dialog hovered==dialog modal)
  let commitDialog=Dialog "Approve changes" Committing [Input "Message" "commit message" 14] 0 ["Commit","Cancel"] []
      committing=desktop {dialog=Just commitDialog}
      (_,committed)=handleEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) committing
      cancelledMnemonic=fst (handleEvent (V.EvKey (V.KChar 'a') [V.MAlt]) committing)
      focusedButton=committing {dialog=Just commitDialog {focus=1}}
  check "dialog mnemonics avoid duplicate initials" (buttonMnemonics commitDialog==[Just 'c',Just 'a'])
  check "Ctrl mnemonic accepts from input" (committed==[WriteGitCommit "commit message"])
  check "Alt mnemonic activates cancel" (dialog cancelledMnemonic==Nothing)
  check "focused dialog button has white text on green" ("color:rgb(255,255,255);background:rgb(0,170,0)'>  Commit" `T.isInfixOf` snapshotHtml focusedButton)
  check "dialog button shadows use half blocks on gray" (all (`T.isInfixOf` snapshotHtml committing) ["color:rgb(0,0,0);background:rgb(170,170,170)'>▄", "color:rgb(0,0,0);background:rgb(170,170,170)'>▀"])
  check "dialog frames are white on gray" ("color:rgb(255,255,255);background:rgb(170,170,170)'>╔" `T.isInfixOf` snapshotHtml committing)
  let browser=openBrowser "/project" "*" [Entry "folder" True Nothing Nothing,Entry "Main.hs" False Nothing Nothing] desktop
      fileDialog=fromMaybe (error "missing file dialog") (dialog browser)
      Rect fx fy _ _=at 1 (fieldRects browser fileDialog)
      (selected,singleEffects)=handleEvent (V.EvMouseDown (fx+2) (fy+3) V.BLeft []) browser
      (opened,fileEffects)=handleDoubleClick (fx+2) (fy+3) selected
      (_,directoryEffects)=handleDoubleClick (fx+2) (fy+2) browser
      (_,blankEffects)=handleDoubleClick (fx+2) (fy+7) browser
  check "single click selects without opening" (null singleEffects && dialog selected/=Nothing)
  check "double click opens file" (fileEffects==[ReadPath "/project/Main.hs"] && dialog opened==Nothing)
  check "double click enters directory" (directoryEffects==[BrowsePath "/project/folder" "*"])
  check "double click blank row does not open" (null blankEffects)
  let (popup,_) = handleEvent (V.EvMouseDown 3 2 V.BRight []) desktop
      (rename,_) = handleEvent (V.EvKey V.KEnter []) popup
      entered=foldl (\d c -> fst (handleEvent (V.EvKey (V.KChar c) []) d)) rename ("greeting" :: String)
      (_,renameEffects)=handleEvent (V.EvKey V.KEnter []) entered
      dismissed=fst (handleEvent (V.EvKey V.KEsc []) popup)
  check "right click opens source context menu" (contextMenu popup/=Nothing)
  check "context rename emits language request" (renameEffects==[LanguageRequest (RenameAt "greeting")])
  check "escape dismisses context" (contextMenu dismissed==Nothing)
  check "context menu draws" (snapshotHtml popup/=snapshotHtml desktop)
  let source=addDocument (Just (FileState "/project/Main.hs" Nothing)) (newBuffer "hello\nworld") (initialDesktop (80,25))
      problem=Diagnostic "/project/Main.hs" Nothing 1 2 1 "Not in scope"
      pane=setProblemsVisible True source {diagnostics=[problem]}
      Rect px py _ _=problemsRect pane
      (selectedPane,clickEffects)=handleEvent (V.EvMouseDown (px+3) (py+1) V.BLeft []) pane
      (_,jumpEffects)=handleDoubleClick (px+3) (py+1) pane
      focusedPane=fst (handleEvent (V.EvMouseDown (px+3) py V.BLeft []) pane)
      (_,keyJump)=handleEvent (V.EvKey V.KEnter []) focusedPane
      pasted=fst (handleEvent (V.EvPaste "overwrite") focusedPane)
  check "problems reserve editor area" (all (\w -> top (bounds w)+height (bounds w)<=py) (windows pane))
  check "problems preserve documents" (buffers pane==buffers source && not (problemsFocused pane))
  check "problem click selects; double click and Enter jump to diagnostic"
    (null clickEffects && problemsFocused selectedPane && jumpEffects==[JumpTo "/project/Main.hs" 1 2] && keyJump==jumpEffects)
  let copiedMessage=fst (handleEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) selectedPane)
      copiedInsert=fst (handleEvent (V.EvKey V.KIns [V.MCtrl]) selectedPane)
      copiedMenu=fst (runCommand Copy selectedPane)
      expected="Error /project/Main.hs:2:3 Not in scope"
  check "all Copy routes use the selected diagnostic rather than source text"
    (all ((==expected).clipboard) [copiedMessage,copiedInsert,copiedMenu] && buffers copiedMessage==buffers pane)
  let messagePopup=fst (handleEvent (V.EvMouseDown (px+3) (py+1) V.BRight []) pane)
      copyPopup=fst (handleEvent (V.EvKey V.KEnter []) (fst (handleEvent (V.EvKey V.KDown []) messagePopup)))
  check "Messages right click offers and invokes Copy message"
    (contextMenu messagePopup/=Nothing && "Copy all messages" `T.isInfixOf` snapshot messagePopup && clipboard copyPopup==expected && contextMenu copyPopup==Nothing)

  check "problems focus does not edit background" (buffers pasted==buffers pane)
  let another=problem {diagnosticPath="/project/Other.hs",diagnosticMessage="Long first line\n  full second line"}
      allPane=selectedPane {diagnostics=[problem,another]}
      allCopied=fst (runCommand CopyAllMessages allPane)
      emptyCopied=fst (runCommand Copy (selectedPane {diagnostics=[],clipboard="keep me"}))
      protected=fst (runCommand Cut selectedPane)
      noSource=selectedPane {windows=[],buffers=mempty}
  check "Copy all includes full multiline messages and source paths"
    ("/project/Other.hs:2:3 Long first line\n  full second line" `T.isInfixOf` clipboard allCopied && expected `T.isPrefixOf` clipboard allCopied)
  check "empty Messages preserves clipboard and disables copy"
    (clipboard emptyCopied=="keep me" && not (commandEnabled (selectedPane {diagnostics=[]}) CopyAllMessages))
  check "Messages copy works without an editor and editing commands preserve source"
    (clipboard (fst (runCommand Copy noSource))==expected && buffers protected==buffers selectedPane)
  let resizeMessagesTo y d=fst (handleEvent (V.EvMouseDown 10 y V.BLeft [])
        (fst (handleEvent (V.EvMouseDown 10 (top (problemsRect d)) V.BLeft []) d)))
      floatingMessage=modifyActive (\w -> w {bounds=Rect 0 6 80 10}) pane
      pushed=resizeMessagesTo 12 floatingMessage
      pinned=resizeMessagesTo 8 pushed
      pulled=resizeMessagesTo 14 pinned
      untouched=modifyActive (\w -> w {bounds=Rect 0 3 80 5}) pane
  check "Messages title drag pushes and pulls abutting windows without sharing borders"
    (top (problemsRect pushed)==12 && fmap bounds (activeWindow pushed)==Just (Rect 0 2 80 10) &&
     fmap bounds (activeWindow pinned)==Just (Rect 0 1 80 7) && fmap bounds (activeWindow pulled)==Just (Rect 0 1 80 13) &&
     fmap bounds (activeWindow (resizeMessagesTo 14 untouched))==Just (Rect 0 3 80 5) && buffers pulled==buffers pane)
  check "diagnostic chevron and message render" (all (`T.isInfixOf` snapshotHtml pane) ["▶","Not in scope"])
  check "closing pane preserves documents" (buffers (setProblemsVisible False pane)==buffers source)
  let scrolling=modifyActive (\w -> w {bounds=Rect 5 5 30 10,scrollRow=10,scrollColumn=4})
        (addDocument Nothing (newBuffer (T.unlines (replicate 100 (T.replicate 100 "x")))) (initialDesktop (80,25)))
      up=fst (handleEvent (V.EvMouseDown 34 6 V.BLeft []) scrolling)
  check "vertical scroll arrow moves one row" (fmap scrollRow (activeWindow up)==Just 9)
  let sw=fromMaybe (error "missing scroll window") (activeWindow scrolling)
      sd=fromMaybe (error "missing scroll document") (activeDocument scrolling)
      hr=scrollbarRect False sd sw
      vr=scrollbarRect True sd sw
      right=fst (handleEvent (V.EvMouseDown (left hr+width hr-1) (top hr) V.BLeft []) scrolling)
      page=fst (handleEvent (V.EvMouseDown (left vr) (top vr+height vr-2) V.BLeft []) scrolling)
      thumb=scrollbarThumb (height vr) (scrollbarLimit True sd sw) (scrollRow sw)
      grabbed=fst (handleEvent (V.EvMouseDown (left vr) (top vr+thumb) V.BLeft []) scrolling)
      bottom=fst (handleEvent (V.EvMouseDown (left vr) (top vr+height vr-2) V.BLeft []) grabbed)
  check "horizontal arrow moves one column" (fmap scrollColumn (activeWindow right)==Just 5)
  check "vertical track pages by viewport" (fmap scrollRow (activeWindow page)==Just 18)
  check "dragging thumb reaches final page" (fmap scrollRow (activeWindow bottom)==Just (scrollbarLimit True sd sw))
  check "horizontal scrollbar retains position readout" (all (`T.isInfixOf` snapshot scrolling) ["1:1","◄","►"])
  check "scrollbars use dark cyan" ("rgb(0,170,170)" `T.isInfixOf` snapshotHtml scrolling)
  let resizing=fst (handleEvent (V.EvMouseDown 33 14 V.BLeft []) scrolling)
      moved=fst (handleEvent (V.EvKey V.KRight []) resizing)
      taller=fst (handleEvent (V.EvKey V.KDown [V.MShift]) moved)
      ignored=fst (handleEvent (V.EvKey (V.KChar 'x') []) taller)
      restored=fst (handleEvent (V.EvKey V.KEsc []) taller)
      accepted=fst (handleEvent (V.EvKey V.KEnter []) taller)
  check "drag keys move and resize" (fmap bounds (activeWindow taller)==Just (Rect 6 5 30 11))
  check "drag typing cannot edit source" (buffers ignored==buffers scrolling)
  check "Escape restores original geometry" (fmap bounds (activeWindow restored)==Just (bounds sw) && dragOriginal restored==Nothing && drag restored==Nothing)
  check "Enter accepts geometry" (fmap bounds (activeWindow accepted)==fmap bounds (activeWindow taller) && drag accepted==Nothing)
  check "drag status explains keys" (all (`T.isInfixOf` snapshot resizing) ["↑↓→← Move","Shift+↑↓→← Resize","↵ Done","Esc Cancel"])
  check "window shadow preserves desktop dither" ("color:rgb(170,170,170);background:rgb(0,0,0)'>░" `T.isInfixOf` snapshotHtml scrolling)
  let overlapping=scrolling {windows=[sw,sw {windowId=windowId sw+1,bounds=Rect 0 1 70 22}]}
  check "window shadow preserves lower window text" ("color:rgb(170,170,170);background:rgb(0,0,0)'>xx" `T.isInfixOf` snapshotHtml overlapping)
  let behind=sw {windowId=windowId sw+1,windowNumber=2,bounds=Rect 40 2 30 18}
      separated=scrolling {windows=[sw,behind]}
      behindRect=bounds behind
      (focusedBehind,behindEffects)=handleEvent (V.EvMouseDown (left behindRect+width behindRect-1) (top behindRect+5) V.BLeft []) separated
      (focusedTitle,titleEffects)=handleEvent (V.EvMouseDown (left behindRect+3) (top behindRect) V.BLeft []) separated
  check "only foreground window has scrollbars and frame controls"
    (all (\glyph -> T.count glyph (snapshot separated {videoMode=Just 3})==1) ["▲","▼","◄","►","■","↑"])
  check "terminal close button uses ASCII x" ("[x]" `T.isInfixOf` snapshot desktop && not ("■" `T.isInfixOf` snapshot desktop))
  check "resize grip is an ordinary frame corner" (not ("◢" `T.isInfixOf` snapshot separated) && "═╝" `T.isInfixOf` snapshot separated)
  let compactTree=installTree "/project" [Entry "src" True Nothing Nothing,Entry "Main.hs" False Nothing Nothing] desktop
  check "dock buttons indicate collapse direction" ("[←]" `T.isInfixOf` snapshot compactTree && "[↓]" `T.isInfixOf` snapshot (setProblemsVisible True desktop) {problemsFocused=True})
  check "dock arrows match editor cyan" ("color:rgb(85,255,255);background:rgb(0,0,170)'>←" `T.isInfixOf` snapshotHtml compactTree && "color:rgb(85,255,255);background:rgb(0,170,170)'>↓" `T.isInfixOf` snapshotHtml (setProblemsVisible True desktop) {problemsFocused=True})
  check "Files has a window frame without a path row"
    (not ("/project" `T.isInfixOf` snapshot compactTree) && "╔" `T.isInfixOf` snapshot compactTree &&
     "▲" `T.isInfixOf` snapshot compactTree && "▼" `T.isInfixOf` snapshot compactTree &&
     snd (handleEvent (V.EvMouseDown 3 2 V.BLeft []) compactTree)==[ExpandTree 0])
  let longTree=installTree "/project" [Entry (T.pack (show i)<>".hs") False Nothing Nothing | i<-[1::Int ..100]] desktop
      scrolled=fst (handleEvent (V.EvMouseDown 4 4 V.BScrollDown []) longTree)
      barClicked=fst (handleEvent (V.EvMouseDown (treeWidthOf longTree) (snd (screenSize longTree)-3) V.BLeft []) longTree)
  let unfocusedTree=compactTree {sideTree=fmap (\t -> t {treeFocused=False}) (sideTree compactTree)}
      glyphAt d x y=T.index (T.lines (snapshot d) !! y) x
      shared=treeWidthOf unfocusedTree
      smallNeighbor=modifyActive (\w -> w {bounds=Rect shared 4 30 10}) unfocusedTree
  check "Files keeps its double outline when the editor is focused"
    (glyphAt unfocusedTree 0 1=='╔' && glyphAt unfocusedTree 0 2=='║' && glyphAt unfocusedTree 0 23=='╚')
  check "shared borders join neighboring top and bottom frames"
    (glyphAt unfocusedTree shared 1=='╦' && glyphAt unfocusedTree shared 23=='╩' &&
     glyphAt smallNeighbor shared 4=='╠' && glyphAt smallNeighbor shared 13=='╠' &&
     glyphAt smallNeighbor shared 1=='╗')
  let raisedFiles=setProblemsVisible True unfocusedTree
      bottomFiles=top (problemsRect raisedFiles)-1
      resizedFiles=resizeMessagesTo 10 raisedFiles
  check "Messages raises Files bottom frame and its scroll area"
    (glyphAt raisedFiles 0 bottomFiles=='╚' && treeContentRows raisedFiles==bottomFiles-2 &&
     glyphAt resizedFiles 0 9=='╚' && treeContentRows resizedFiles==7)
  check "Files scrollbar and wheel scroll rows" (fmap treeScroll (sideTree scrolled)==Just 3 && fmap treeScroll (sideTree barClicked)==Just 1)
  check "tree markers have a separating space" ("+ src" `T.isInfixOf` snapshot compactTree && not ("[+]" `T.isInfixOf` snapshot compactTree))
  check "inactive scrollbar region only focuses window"
    (fmap windowId (activeWindow focusedBehind)==Just (windowId behind) && drag focusedBehind==Nothing && null behindEffects && fmap scrollRow (activeWindow focusedBehind)==Just (scrollRow behind))
  check "inactive close region cannot close the window"
    (length (windows focusedTitle)==2 && fmap windowId (activeWindow focusedTitle)==Just (windowId behind) && null titleEffects)
  check "Messages focus removes editor scrollbars" (not ("▲" `T.isInfixOf` snapshot separated {problemsFocused=True}))
  let menuState=desktop {menu=Just (0,0),status="old status",typeHint="old type"}
      menuNext=fst (handleEvent (V.EvKey V.KDown []) menuState)
  check "menu status follows highlighted command" (commandDescription New `T.isInfixOf` snapshot menuState && commandDescription Open `T.isInfixOf` snapshot menuNext)
  check "context menu status explains highlighted action" (commandDescription RenameSymbol `T.isInfixOf` snapshot popup)
  let statusDesktop=desktop {status="",typeHint=""}
      statusY=snd (screenSize statusDesktop)-1
      openStatus=case [r | (r,_,Left Open)<-statusItemRects statusDesktop] of r:_ -> r; _ -> error "Missing Open status action"
      hoveredStatus=fst (hoverAt (left openStatus+3) statusY statusDesktop)
      clickedStatus=handleEvent (V.EvMouseDown (left openStatus+3) statusY V.BLeft []) statusDesktop
  check "status labels highlight green and invoke their menu command"
    ("background:rgb(0,170,0)" `T.isInfixOf` snapshotHtml hoveredStatus && snd clickedStatus==snd (runCommand Open statusDesktop))
  let statusModal=fst (runCommand EditorOptions statusDesktop)
      escapeRect=case [r | (r,_,Right (V.EvKey V.KEsc []))<-statusItemRects statusModal] of r:_ -> r; _ -> error "Missing modal Cancel status action"
  check "status bar Cancel works through modal input"
    (dialog (fst (handleEvent (V.EvMouseDown (left escapeRect+3) statusY V.BLeft []) statusModal))==Nothing)
  let withHint=statusDesktop {typeHint="a :: Int"}
  check "clicking type information does not invoke a hidden status shortcut"
    (handleEvent (V.EvMouseDown 3 statusY V.BLeft []) withHint==(withHint,[]))
  let narrow=statusDesktop {screenSize=(40,25),branchStatus="main",branchRoot=Just "/tmp"}
  check "status hit rectangles stop at Git badge"
    (all (\(r,_,_)->left r+width r<=left (gitBadgeRect narrow)) (statusItemRects narrow))
  let numbered=fst (runCommand New desktop)
      splitNumbered=fst (runCommand SplitVertical numbered)
      closedNumbered=fst (runCommand Close numbered)
      reused=fst (runCommand New closedNumbered)
      withMessages=setProblemsVisible True numbered
      numberedAgain=fst (runCommand New withMessages)
      activated=fst (handleEvent (V.EvKey (V.KChar '1') [V.MAlt]) numbered {menu=Just (0,0)})
      messagesActivated=fst (handleEvent (V.EvKey (V.KChar '3') [V.MAlt]) withMessages)
      sourceActivated=fst (handleEvent (V.EvKey (V.KChar '2') [V.MAlt]) messagesActivated)
  check "window numbers stay stable on focus" (map windowNumber (windows numbered)==[2,1] && fmap windowNumber (activeWindow activated)==Just 1 && menu activated==Nothing)
  check "split views get independent numbers" (map windowNumber (windows splitNumbered)==[3,2,1])
  check "closing releases the smallest number" (fmap windowNumber (activeWindow reused)==Just 2)
  check "Messages shares the window number pool" (messagesNumber withMessages==Just 3 && fmap windowNumber (activeWindow numberedAgain)==Just 4)
  check "Alt number activates Messages and then source" (problemsFocused messagesActivated && not (problemsFocused sourceActivated) && fmap windowNumber (activeWindow sourceActivated)==Just 2)
  check "hiding Messages releases its number" (messagesNumber (setProblemsVisible False withMessages)==Nothing && nextWindowNumber (setProblemsVisible False withMessages)==3)
  check "focused Messages uses white frame on cyan" ("color:rgb(255,255,255);background:rgb(0,170,170)'> Messages " `T.isInfixOf` snapshotHtml messagesActivated)
  let secondProblem=Diagnostic "/project/Other.hs" Nothing 0 0 1 "Other error"
      messages=source {diagnostics=[problem,secondProblem]}
      (_,firstMessage)=handleEvent (V.EvKey (V.KFun 8) [V.MAlt]) messages
      atFirst=moveTo False 6 messages
      (nextMessageState,nextMessage)=handleEvent (V.EvKey (V.KFun 8) [V.MAlt]) atFirst
      atSecond=addDocument (Just (FileState "/project/Other.hs" Nothing)) (newBuffer "other") nextMessageState
      (_,previousMessage)=handleEvent (V.EvKey (V.KFun 7) [V.MAlt]) atSecond
      (_,emptyNavigation)=runCommand NextMessage desktop
  check "first message navigation visits selected diagnostic" (firstMessage==[JumpTo "/project/Main.hs" 1 2])
  check "message navigation goes across files in both directions" (nextMessage==[JumpTo "/project/Other.hs" 0 0] && previousMessage==[JumpTo "/project/Main.hs" 1 2])
  check "empty message navigation is disabled and harmless" (not (commandEnabled desktop NextMessage) && not (commandEnabled desktop PreviousMessage) && null emptyNavigation)
  let preferences=fst (runCommand EditorOptions desktop)
      preferencesDialog=fromMaybe (error "missing preferences") (dialog preferences)
      checkboxIndex=length (fields preferencesDialog)-1
      checkboxRect=at checkboxIndex (fieldRects preferences preferencesDialog)
      mouseToggled=fst (handleEvent (V.EvMouseDown (left checkboxRect+1) (top checkboxRect) V.BLeft []) preferences)
      savedPreferences=fst (handleEvent (V.EvKey V.KEnter []) mouseToggled)
      keyboardFocused=preferences {dialog=Just preferencesDialog {focus=checkboxIndex}}
      keyboardToggled=fst (handleEvent (V.EvKey (V.KChar ' ') []) keyboardFocused)
      cancelledPreferences=fst (handleEvent (V.EvKey V.KEsc []) keyboardToggled)
      reopened=fst (runCommand EditorOptions savedPreferences)
  check "focused Messages hides source caret" (V.picCursor (renderDesktop desktop {problemsFocused=True})==V.NoCursor)
  check "cursor blinking defaults on" (blinkCursor desktop)
  check "cursor appearance checkbox uses shared mouse geometry" (not (blinkCursor savedPreferences) && buffers savedPreferences==buffers desktop)
  check "cursor appearance can be toggled by keyboard" (maybe False (elem (CheckBox "Blinking cursor" False) . fields) (dialog keyboardToggled))
  check "cancel preserves cursor appearance" (blinkCursor cancelledPreferences)
  check "appearance persists when preferences reopen" (maybe False (elem (CheckBox "Blinking cursor" False) . fields) (dialog reopened))
  mapM_ (\size -> let small=fst (handleEvent (uncurry V.EvResize size) scrolling)
                  in check "small window rendering remains bounded" (length (T.lines (snapshot small))==snd size)) [(1,3),(8,6),(16,8),(40,12)]
  putStrLn "dialog mouse checks passed"
