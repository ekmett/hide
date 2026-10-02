{-# LANGUAGE OverloadedStrings #-}
module DialogMouseCheck (checks) where

import Control.Monad (unless, forM_)
import Control.Exception (evaluate)
import System.Timeout (timeout)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import THC.Edit.BufferView
import THC.Edit.Model
import THC.Edit.Render (snapshotHtml, snapshot, renderDesktop)
import THC.Edit.Buffer (newBuffer, columnOffset, contents, markSaved, replaceSelection, Selection(..))
import THC.Edit.Window (nativeMenuShortcut)
import qualified Data.Text.Encoding as TE
import THC.Edit.Browser (Entry(..))
import THC.Edit.Files (FileState(..))
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Graphics.Vty as V
import THC.Edit.Unicode (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import Data.Foldable (toList)
import qualified Data.Text.Lazy as TL

checks :: IO ()
checks = do
  searchChecks
  previousSearchChecks
  let check name ok = unless ok (error name)
      at n xs = case drop n xs of value:_ -> value; _ -> error "missing test fixture"
      desktop = addDocument Nothing (newBuffer "hello world") (initialDesktop (80,25))
      modal = fst (runCommand About desktop)
      dg = fromMaybe (error "missing About dialog") (dialog modal)
      Rect bx by _ _ = at 0 (buttonRects modal dg)
      (pressed,requests) = handleEvent (V.EvMouseDown bx by V.BLeft []) modal
      (released,_) = handleEvent (V.EvMouseUp bx by (Just V.BLeft)) pressed
      (cancelled,_) = handleEvent (V.EvMouseUp 0 0 (Just V.BLeft)) pressed
  let terminal=addReadOnly "Terminal draw" "visible terminal content" desktop
      terminalId=maybe (error "missing terminal") windowId (activeWindow terminal)
      pinnedTerminal=setTerminalPinned True terminalId terminal
      shown=setProblemsVisible True pinnedTerminal
      showTerminal=focusWindow terminalId shown
      atTop d=T.lines (snapshot d) !! top (problemsRect d)
      clickedMessages=case [r | (r,Nothing,_)<-bottomTabs showTerminal] of
        r:_ -> fst (handleEvent (V.EvMouseDown (left r) (top r) V.BLeft []) showTerminal)
        _ -> error "missing messages tab"
      unpinned=fst (handleEvent (V.EvMouseDown (fst (screenSize pinnedTerminal)-8) (top (problemsRect pinnedTerminal)) V.BLeft []) pinnedTerminal)
  let withDiagnostic=showTerminal {diagnostics=[Diagnostic "/tmp/example.hs" Nothing 0 0 1 "problem"]}
      (doubleClicked,doubleEffects)=handleDoubleClick 2 (top (problemsRect withDiagnostic)+1) withDiagnostic
  check "double clicking a terminal tab never activates a hidden Messages diagnostic"
    (bottomTerminal doubleClicked==bottomTerminal withDiagnostic && null doubleEffects && not (problemsFocused doubleClicked))
  check "bottom panel renders tabs and cyan pin fallback without hidden terminal content"
    ("Messages" `T.isInfixOf` atTop showTerminal && "Terminal" `T.isInfixOf` atTop showTerminal &&
     "[P]" `T.isInfixOf` atTop showTerminal && "visible terminal content" `T.isInfixOf` snapshot showTerminal &&
     not ("visible terminal content" `T.isInfixOf` snapshot shown) && messagesDisplayed clickedMessages &&
     not (maybe False (windowPinned unpinned) (activeWindow unpinned)))
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
      floatingMessage=modifyActive (\w -> w {bounds=Rect 10 6 60 10}) pane
      pushed=resizeMessagesTo 12 floatingMessage
      pinned=resizeMessagesTo 8 pushed
      pulled=resizeMessagesTo 14 pinned
      untouched=modifyActive (\w -> w {bounds=Rect 0 3 80 5}) pane
  check "Messages title drag pushes and pulls abutting windows without sharing borders"
    (top (problemsRect pushed)==12 && fmap bounds (activeWindow pushed)==Just (Rect 10 2 60 10) &&
     fmap bounds (activeWindow pinned)==Just (Rect 10 1 60 7) && fmap bounds (activeWindow pulled)==Just (Rect 10 1 60 13) &&
     fmap bounds (activeWindow (resizeMessagesTo 14 untouched))==Just (Rect 0 3 80 5) && buffers pulled==buffers pane)
  check "diagnostic chevron and message render" (all (`T.isInfixOf` snapshotHtml pane) ["▶","Not in scope"])
  check "closing pane preserves documents" (buffers (setProblemsVisible False pane)==buffers source)
  let editedTitle=insertText "new" (addDocument Nothing (newBuffer "") (initialDesktop (80,25)))
      cleanTitle=editedTitle {buffers=M.map (\doc -> doc {documentBuffer=markSaved (documentBuffer doc)}) (buffers editedTitle)}
      narrowTitle=modifyActive (\w -> w {bounds=Rect 1 2 28 10}) editedTitle
      header desktop=T.lines (snapshot desktop) !! maybe 1 (top . bounds) (activeWindow desktop)
  check "edited unnamed buffer shows line changes in top title" ("+1 -0" `T.isInfixOf` header editedTitle)
  check "line counts retain green and red colors" (all (`T.isInfixOf` snapshotHtml editedTitle)
    ["color:rgb(85,255,85);background:rgb(0,0,170)'>+1", "color:rgb(255,85,85);background:rgb(0,0,170)'>-0"])
  check "saving clears title change counts" (not ("+1" `T.isInfixOf` header cleanTitle))
  check "narrow title retains complete change counts" ("+1 -0" `T.isInfixOf` header narrowTitle)
  let reviewBase=editActive (\_ -> replaceSelection (Selection 0 6) "after\nextra") Nothing
        (addDocument Nothing (newBuffer "before\nsame\n") (initialDesktop (80,25)))
      changes=setBufferView ChangesView reviewBase
      side=setBufferView SideBySideView reviewBase
      focusedChanges=setBufferView OnlyChangesView reviewBase
  check "current view hides deleted text" (not ("before" `T.isInfixOf` snapshot reviewBase))
  check "changes view shows red original and green replacement"
    (all (`T.isInfixOf` snapshot changes) ["before","after","extra"] && all (`T.isInfixOf` snapshotHtml changes)
      ["color:rgb(255,85,85);background:rgb(0,0,170)'>before","color:rgb(85,255,85);background:rgb(0,0,170)'>after"])
  check "side by side retains source and insert padding"
    (all (`T.isInfixOf` snapshot side) ["before","after","extra"] && "background:rgb(0,85,0)" `T.isInfixOf` snapshotHtml side)
  check "filtered view displays change body" ("before" `T.isInfixOf` snapshot focusedChanges)
  let scrolling=modifyActive (\w -> w {bounds=Rect 5 5 30 10,scrollRow=10,scrollColumn=4})
        (addDocument Nothing (newBuffer (T.unlines (replicate 100 (T.replicate 100 "x")))) (initialDesktop (80,25)))
      up=fst (handleEvent (V.EvMouseDown 34 6 V.BLeft []) scrolling)
  check "vertical scroll arrow moves one row" (fmap scrollRow (activeWindow up)==Just 9)
  let sw=fromMaybe (error "missing scroll window") (activeWindow scrolling)
      sd=fromMaybe (error "missing scroll document") (activeDocument scrolling)
      hr=scrollbarRect scrolling False sd sw
      vr=scrollbarRect scrolling True sd sw
      right=fst (handleEvent (V.EvMouseDown (left hr+width hr-1) (top hr) V.BLeft []) scrolling)
      page=fst (handleEvent (V.EvMouseDown (left vr) (top vr+height vr-2) V.BLeft []) scrolling)
      thumb=scrollbarThumb (height vr) (scrollbarLimit scrolling True sd sw) (scrollRow sw)
      grabbed=fst (handleEvent (V.EvMouseDown (left vr) (top vr+thumb) V.BLeft []) scrolling)
      bottom=fst (handleEvent (V.EvMouseDown (left vr) (top vr+height vr-2) V.BLeft []) grabbed)
  check "horizontal arrow moves one column" (fmap scrollColumn (activeWindow right)==Just 5)
  check "vertical track pages by viewport" (fmap scrollRow (activeWindow page)==Just 18)
  check "dragging thumb reaches final page" (fmap scrollRow (activeWindow bottom)==Just (scrollbarLimit scrolling True sd sw))
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
    (all (\glyph -> T.count glyph (T.unlines (take (snd (screenSize separated)-1) (T.lines (snapshot separated {videoMode=Just 3}))))==1) ["▲","▼","◄","►","■","↑"])
  check "terminal close button uses ASCII x" ("[x]" `T.isInfixOf` snapshot desktop && not ("■" `T.isInfixOf` snapshot desktop))
  check "resize grip is an ordinary frame corner" (not ("◢" `T.isInfixOf` snapshot separated) && "═╝" `T.isInfixOf` snapshot separated)
  let compactTree=installTree "/project" [Entry "src" True Nothing Nothing,Entry "Main.hs" False Nothing Nothing] desktop
  let dirtyTree=installTree "/project" [Entry "Main.hs" False Nothing Nothing] (insertText "x" source)
      cleanTree=installTree "/project" [Entry "Main.hs" False Nothing Nothing] (fst (runCommand Undo (insertText "x" source)))
      redName="color:rgb(255,85,85);background:rgb(0,0,170)'> Main.hs"
      unfocus d=d {sideTree=fmap (\t -> t {treeFocused=False}) (sideTree d)}
  check "unsaved filenames are red and undo restores their normal color"
    (redName `T.isInfixOf` snapshotHtml (unfocus dirtyTree) && not (redName `T.isInfixOf` snapshotHtml (unfocus cleanTree)))
  let treeLine d=T.lines (snapshot (unfocus d)) !! 2
      savedTree=dirtyTree {buffers=M.map (\doc -> doc {documentBuffer=markSaved (documentBuffer doc)}) (buffers dirtyTree)}
  check "Files shows the buffer's line counts after its dirty filename"
    ("Main.hs +1 -1" `T.isInfixOf` treeLine dirtyTree &&
     not ("+1" `T.isInfixOf` treeLine cleanTree) && not ("+1" `T.isInfixOf` treeLine savedTree))
  check "dock buttons indicate collapse direction" ("[←]" `T.isInfixOf` snapshot compactTree && "[↓]" `T.isInfixOf` snapshot (setProblemsVisible True desktop) {problemsFocused=True})
  check "dock arrows match editor cyan" ("color:rgb(85,255,255);background:rgb(0,0,170)'>←" `T.isInfixOf` snapshotHtml compactTree && "color:rgb(85,255,255);background:rgb(0,170,170)'>↓" `T.isInfixOf` snapshotHtml (setProblemsVisible True desktop) {problemsFocused=True})
  check "Files has a floating title and scrollbar without a path row"
    (not ("/project" `T.isInfixOf` snapshot compactTree) && " Files " `T.isInfixOf` snapshot compactTree &&
     "▲" `T.isInfixOf` snapshot compactTree && "▼" `T.isInfixOf` snapshot compactTree &&
     snd (handleEvent (V.EvMouseDown 3 2 V.BLeft []) compactTree)==[ExpandTree 0])
  let longTree=installTree "/project" [Entry (T.pack (show i)<>".hs") False Nothing Nothing | i<-[1::Int ..100]] desktop
      scrolled=fst (handleEvent (V.EvMouseDown 4 4 V.BScrollDown []) longTree)
      barClicked=fst (handleEvent (V.EvMouseDown (treeWidthOf longTree-1) (snd (screenSize longTree)-3) V.BLeft []) longTree)
  let unfocusedTree=compactTree {sideTree=fmap (\t -> t {treeFocused=False}) (sideTree compactTree)}
      glyphAt d x y=let line=T.lines (snapshot d) !! y in T.index line (columnOffset line x)
      shared=treeWidthOf unfocusedTree
      smallNeighbor=modifyActive (\w -> w {bounds=Rect shared 4 30 10}) unfocusedTree
  check "Files leaves its top, left and bottom edges unframed"
    (glyphAt unfocusedTree 0 1==' ' && glyphAt unfocusedTree 0 2==' ' && glyphAt unfocusedTree 0 23==' ')
  check "shared column yields to neighboring frames and stays single where exposed"
    (glyphAt unfocusedTree shared 1=='╔' && glyphAt unfocusedTree shared 23=='╚' &&
     glyphAt smallNeighbor shared 4=='╔' && glyphAt smallNeighbor shared 13=='╚' &&
     glyphAt smallNeighbor shared 1=='│')
  let raisedFiles=setProblemsVisible True unfocusedTree
      bottomFiles=top (problemsRect raisedFiles)-1
      resizedFiles=resizeMessagesTo 10 raisedFiles
  check "Messages raises Files background and its scroll area"
    (glyphAt raisedFiles 0 bottomFiles==' ' && treeContentRows raisedFiles==bottomFiles-2 &&
     glyphAt resizedFiles 0 9==' ' && treeContentRows resizedFiles==7)
  check "Files scrollbar and wheel scroll rows" (fmap treeScroll (sideTree scrolled)==Just 3 && fmap treeScroll (sideTree barClicked)==Just 1)
  check "tree markers have a separating space" ("📁 src" `T.isInfixOf` snapshot compactTree && not ("[+]" `T.isInfixOf` snapshot compactTree))
  check "Unicode folders are the default in terminals and windows"
    (all (T.isInfixOf "📁 src" . snapshot) [compactTree,compactTree {videoMode=Just 3}])
  check "Material folders remain an explicit option"
    ("\xf024b src" `T.isInfixOf` snapshot compactTree {materialIcons=True})
  let branched=expandTree 1 [Entry "C.hs" False Nothing Nothing]
        (expandTree 0 [Entry "A" True Nothing Nothing,Entry "B.hs" False Nothing Nothing] compactTree)
      branchLines=["├📂 src","│├📂 A","││└📄 C.hs","│└📄 B.hs","└📄 Main.hs"]
      scrolledBranches=branched {sideTree=fmap (\tree -> tree {treeScroll=2}) (sideTree branched)}
  check "Files draws connected branches with one-column depth steps"
    (all (`T.isInfixOf` snapshot branched) branchLines)
  check "tree branches preserve ancestry above the scrolled viewport"
    ("││└📄 C.hs" `T.isInfixOf` snapshot scrolledBranches)
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
  let selector=toolchainBadgeRect statusDesktop
      toolchainPopup=fst (handleEvent (V.EvMouseDown (left selector+2) statusY V.BLeft []) statusDesktop)
      ghc=fst (handleEvent (V.EvKey V.KDown []) toolchainPopup)
      (_,selectEffects)=handleEvent (V.EvKey V.KEnter []) ghc
  check "status toolchain dropdown selects GHC by keyboard"
    (contextMenu toolchainPopup/=Nothing && selectEffects==[AgentAction "toolchain" ["GHC","ghc"]])
  check "modal status cannot switch toolchain"
    (dialog (fst (handleEvent (V.EvMouseDown (left selector+2) statusY V.BLeft []) statusModal))==dialog statusModal)
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
  check "focused Messages uses white frame on cyan" (or [" Messages " `T.isInfixOf` TL.toStrict text && V.attrForeColor a==V.SetTo (V.RGBColor 255 255 255) && V.attrBackColor a==V.SetTo (V.RGBColor 0 170 170) | row<-toList (displayOpsForPic (renderDesktop messagesActivated) (screenSize messagesActivated)),TextSpan{textSpanAttr=a,textSpanText=text}<-toList row])
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
      checkboxIndex=fromMaybe (error "missing cursor checkbox") (findIndex (\field -> case field of CheckBox "Blinking cursor" _ -> True; _ -> False) (fields preferencesDialog))
      checkboxRect=at checkboxIndex (fieldRects preferences preferencesDialog)
      mouseToggled=fst (handleEvent (V.EvMouseDown (left checkboxRect+1) (top checkboxRect) V.BLeft []) preferences)
      savedPreferences=fst (handleEvent (V.EvKey V.KEnter []) mouseToggled)
      keyboardFocused=preferences {dialog=Just preferencesDialog {focus=checkboxIndex}}
      keyboardToggled=fst (handleEvent (V.EvKey (V.KChar ' ') []) keyboardFocused)
      cancelledPreferences=fst (handleEvent (V.EvKey V.KEsc []) keyboardToggled)
      reopened=fst (runCommand EditorOptions savedPreferences)
  let graphicalPreferences=fst (runCommand EditorOptions desktop {videoMode=Just 3})
      graphicalDialog=fromMaybe (error "missing graphical preferences") (dialog graphicalPreferences)
      pixelIndex=fromMaybe (error "missing pixelation checkbox") (findIndex (\field -> case field of CheckBox "Pixelate Unicode" _ -> True; _ -> False) (fields graphicalDialog))
      pixelRect=at pixelIndex (fieldRects graphicalPreferences graphicalDialog)
      toggledPixel=fst (handleEvent (V.EvMouseDown (left pixelRect+1) (top pixelRect) V.BLeft []) graphicalPreferences)
      savedPixel=fst (handleEvent (V.EvKey V.KEnter []) toggledPixel)
  check "80x25 Preferences shows every appearance option without scrolling"
    (all (`T.isInfixOf` snapshot graphicalPreferences) ["CRT filter","Pixelate Unicode","Streamer mode"] &&
     all (\r -> top r+height r<=minimum (map top (buttonRects graphicalPreferences graphicalDialog))) (fieldRects graphicalPreferences graphicalDialog))
  let preferenceRects=fieldRects graphicalPreferences graphicalDialog
      appearanceIndex=fromMaybe (error "missing appearance") (findIndex (\field -> case field of Radio "Appearance" _ _ -> True; _ -> False) (fields graphicalDialog))
      appearanceRect=at appearanceIndex preferenceRects
      darkChoice=fst (handleEvent (V.EvMouseDown (left appearanceRect+5) (top appearanceRect+2) V.BLeft []) graphicalPreferences)
      darkSaved=fst (handleEvent (V.EvKey V.KEnter []) darkChoice)
      narrow=graphicalPreferences {screenSize=(40,25)}
  check "Preferences uses compact columns with working right-column hit targets"
    (height (dialogRect graphicalPreferences graphicalDialog)<=15 &&
     left appearanceRect>left (at 0 preferenceRects) && top appearanceRect==top (at 0 preferenceRects) &&
     appearance darkSaved==DarkMode &&
     all (\i -> fieldRects graphicalPreferences graphicalDialog {focus=i}==preferenceRects) [0..length (fields graphicalDialog)-1])
  check "narrow Preferences falls back to one column"
    (all ((==3+left (dialogRect narrow graphicalDialog)) . left) (fieldRects narrow graphicalDialog))
  check "Pixelate Unicode can be clicked immediately at 80x25" (pixelateUnicode savedPixel)
  check "focused Messages hides source caret" (V.picCursor (renderDesktop desktop {problemsFocused=True})==V.NoCursor)
  check "cursor blinking defaults on" (blinkCursor desktop)
  check "cursor appearance checkbox uses shared mouse geometry" (not (blinkCursor savedPreferences) && buffers savedPreferences==buffers desktop)
  check "cursor appearance can be toggled by keyboard" (maybe False (elem (CheckBox "Blinking cursor" False) . fields) (dialog keyboardToggled))
  check "cancel preserves cursor appearance" (blinkCursor cancelledPreferences)
  check "appearance persists when preferences reopen" (maybe False (elem (CheckBox "Blinking cursor" False) . fields) (dialog reopened))
  mapM_ (\size -> let small=fst (handleEvent (uncurry V.EvResize size) scrolling)
                  in check "small window rendering remains bounded" (length (T.lines (snapshot small))==snd size)) [(1,3),(8,6),(16,8),(40,12)]
  putStrLn "dialog mouse checks passed"

searchChecks :: IO ()
searchChecks=do
  let check name ok=unless ok (error name)
      base=addDocument Nothing (newBuffer "hello world") (initialDesktop (80,25))
      key k mods=fst . handleEvent (V.EvKey k mods)
      paste text=fst . handleEvent (V.EvPaste (TE.encodeUtf8 text))
      view d=fromMaybe (error "search dialog missing") (dialog d)
      texts d=[value | Input _ value _<-fields (view d)]
      findOpen=key (V.KChar 'f') [V.MCtrl] base
      findTyped=paste "world" findOpen
      replaceOpen=key (V.KChar 'h') [V.MCtrl] findTyped
      replaceTyped=paste "planet" replaceOpen
      findAgain=key (V.KChar 'f') [V.MCtrl] replaceTyped
      tab=fromMaybe (error "replace tab missing") (lookup True [(mode,r) | (r,mode)<-searchTabRects findAgain (view findAgain)])
      clicked=fst (handleEvent (V.EvMouseDown (left tab+1) (top tab) V.BLeft []) findAgain)
      toggled=key (V.KChar '\t') [V.MCtrl] clicked
  check "Find and Replace share a stable dialog with text preserved across keyboard and mouse tabs"
    (dialogTitle (view findOpen)=="Find and Replace" && texts findAgain==["world"] &&
      purpose (view findAgain)==Searching False "planet" && texts clicked==["world","planet"] &&
      dialogRect findOpen (view findOpen)==dialogRect replaceTyped (view replaceTyped) &&
      purpose (view toggled)==Searching False "planet")
  let (replaced,_)=submitDialog 0 (view clicked) clicked
  check "tabbed Replace retains strict one-match editing behavior" (activeText replaced=="hello planet" && lastFind replaced=="world" && dialog replaced==Nothing)
  check "legacy Replace alias opens the Replace tab" (case purpose (view (key (V.KChar 'r') [V.MCtrl] base)) of Searching True _->True; _->False)
  let changed=key (V.KChar '!') [] base
      undone=key (V.KChar 'z') [V.MCtrl] changed
      redone=key (V.KChar 'z') [V.MCtrl,V.MShift] undone
      legacy=key (V.KChar 'y') [V.MCtrl] undone
      shortcut d cmd=fromMaybe "" (lookup cmd [(c,menuShortcut d item) | (_,_,items)<-menus,item@(MenuItem _ _ c)<-items])
  check "modern redo and legacy alias both restore the edit" (activeText redone==activeText changed && activeText legacy==activeText changed && activeText (key (V.KChar 'Z') [V.MCtrl,V.MShift] undone)==activeText changed)
  check "modern menu shortcuts match the native platform"
    (shortcut base Copy=="Ctrl+C" && shortcut base Cut=="Ctrl+X" && shortcut base Paste=="Ctrl+V" && shortcut base Replace=="Ctrl+H" &&
      shortcut base {nativeMac=True} Replace=="Cmd+Option+F" && shortcut base {nativeMac=True} Redo=="Cmd+Shift+Z" &&
      nativeMenuShortcut Replace=="~f" && nativeMenuShortcut FindPrevious=="G")
  let child=selectConversationView "child" "Worker" base
      drafted=child {composerBuffer=newBuffer "keep draft",composerSelection=Selection 10 10}
      (focused,focusEffects)=runCommand Conversation drafted
      (new,newEffects)=runCommand AgentNew drafted
  check "Conversation focuses the existing child view without repaint or replacing its draft"
    (buffers focused==buffers drafted && length (windows focused)==length (windows drafted) &&
      conversationTarget focused=="child" && contents (composerBuffer focused)=="keep draft" && focusEffects==[AgentAction "focus" []])
  check "Conversation opens through the existing runtime when absent" (snd (runCommand Conversation base)==[AgentAction "show" []])
  check "New conversation goes directly to the primary runtime session action" (conversationTarget new=="" && newEffects==[AgentAction "new" []])
  check "conversation shortcuts invoke explicit actions" (snd (handleEvent (V.EvKey (V.KChar 'c') [V.MCtrl,V.MShift]) base)==[AgentAction "show" []] && snd (handleEvent (V.EvKey (V.KChar 'n') [V.MCtrl,V.MShift]) base)==[AgentAction "new" []])
  let terminal=addReadOnly "Terminal 1" "" base
  check "modern shortcuts do not consume PTY control bytes" (and [snd (handleEvent (V.EvKey (V.KChar c) mods) terminal)==[AgentAction "terminal-input" ["1",text]] |
    (c,mods,text)<-[('h',[V.MCtrl],"\b"),('f',[V.MCtrl],"\x06"),('n',[V.MCtrl,V.MShift],"\x0e"),('c',[V.MCtrl,V.MShift],"\x03")]])

-- Independent exhaustive oracle includes overlapping matches and wraparound.
previousSearchChecks :: IO ()
previousSearchChecks=do
  let check name ok=unless ok (error name)
      run text needle p=fmap selection (activeWindow (findPrevious
        (moveTo False p (addDocument Nothing (newBuffer text) (initialDesktop (80,25)))) {lastFind=needle}))
  forM_ ["", "aaa", "ababa", "λ界λ界", "a\nb\na"] $ \text ->
    forM_ ["a", "aa", "aba", "λ界", "界λ", "\nb", "absent"] $ \needle ->
      forM_ [0..T.length text] $ \p -> do
        let matches=[i | i<-[0..T.length text-T.length needle],needle `T.isPrefixOf` T.drop i text]
            earlier=filter (<p) matches
            candidates=if null earlier then matches else earlier
            expected=case reverse candidates of i:_->Selection i (i+T.length needle); []->Selection p p
        check "previous search preserves overlapping matches, Unicode positions and wraparound" (run text needle p==Just expected)
  fast<-timeout 1000000 (evaluate (run (T.replicate 200000 "λ界") "λ界" 400000==Just (Selection 399998 400000)))
  check "previous search scans a large Unicode buffer without repeated prefix walks" (fast==Just True)
