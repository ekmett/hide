{-# LANGUAGE OverloadedStrings #-}
module WindowCheck (checks) where
import SourceWindowFixture (sourceFixtureBuffer)
import Control.Monad (unless, forM_, foldM)
import Control.Exception (evaluate)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Data.List (find)
import System.Directory (getTemporaryDirectory)
import System.FilePath (normalise, (</>))
import Hide.Frontend
import Hide.Sidebar
import Hide.Model
import Hide.Render (snapshot, renderKey)
import qualified Hide.Plugin.Editor as E
import qualified Hide.Plugin.Window as W
import qualified Data.Text as T
import Hide.Buffer (newBuffer, Selection(..))
import qualified Hide.Buffer as B
import Hide.BufferView
import Hide.Files (FileState(..))
import Hide.Syntax (Style(..),prepareSourceRow)
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as V
checks :: IO ()
checks = do
  windowTabChecks
  sourceCoordinateChecks
  sourceScrollChecks
  ref<-E.newDraftRef
  childBody<-W.prepareTextWindow "Child" "history"
  let check name ok = unless ok (error name)
  let original=newBuffer "same revision source"
      opaque=original {B.undoStack=error "redraw key forced Undo history",B.redoStack=error "redraw key forced Redo history"}
      base=addDocument (Just (FileState "Fixture.hs" Nothing)) opaque (initialDesktop (80,25))
      large=base {buffers=M.adjust (\doc->doc {documentHighlight=("x",Plain):error "redraw key forced highlight tail",documentLinks=(0,1,"target"):error "redraw key forced link tail"}) 1 (buffers base),
        diagnostics=Diagnostic "Fixture.hs" Nothing 0 0 1 "problem":error "redraw key forced diagnostics tail"}
      changeDoc f d=d {buffers=M.adjust f 1 (buffers d)}
  key<-renderKey large
  same<-renderKey large {systemDark=systemDark large,buffers=M.map id (buffers large)}
  check "redraw identity does not inspect histories, highlights or diagnostics" =<< evaluate (key==same)
  baseKey<-renderKey base
  exportKey<-renderKey base {pendingFileExport=(1,Just (error "redraw key forced exported bytes"))}
  drainedKey<-renderKey base {pendingFileExport=(1,Nothing)}
  check "redraw tracks export identity without inspecting bytes or repainting retirement" (exportKey/=baseKey && exportKey==drainedKey)
  forM_ [changeDoc (\doc->doc {documentBuffer=newBuffer "replacement with same revision"}) base,
         changeDoc (\doc->doc {documentHighlight=[("x",Keyword)]}) base,
         changeDoc (\doc->doc {documentLinks=[(0,1,"new target")]}) base,
         changeDoc (\doc->doc {documentSourceRows=Just (Seq.singleton (prepareSourceRow "x" [("x",Comment)]))}) base,
         changeDoc (\doc->doc {documentBuffer=B.markSaved original}) base,
         base {editorDrafts=M.singleton ref (EditorDraft (newBuffer "new draft") (Selection 0 0) True Nothing)},
         setDiagnostics (diagnostics base) base,
         base {diagnostics=[Diagnostic "Fixture.hs" Nothing 0 0 1 "new problem"]},
         base {systemDark=not (systemDark base)},
         modifyActive (\w->w {selection=Selection 0 1}) base,
         base {menu=Just (0,0)},
         base {guestPrivatePaths=["Fixture.hs"]}] $ \changed -> do
    changedKey<-renderKey changed
    check "redraw key notices content, colors, saved state, diagnostics, theme and UI changes" (changedKey/=baseKey)
  let textArea=base {dialog=Just (Dialog "Details" Information [TextArea "Text" True original (Selection 0 0) 0 0] 0 ["OK"] [])}
      areaChanged=textArea {dialog=fmap (\dg->dg {fields=[TextArea "Text" True (newBuffer "other") (Selection 0 0) 0 0]}) (dialog textArea)}
      areaWrapped=textArea {dialog=fmap (\dg->dg {fields=map id (fields dg)}) (dialog textArea)}
      view=ConversationView (InertBody childBody) "Child" ref Nothing Nothing FollowEnd 0 0 Nothing Nothing Nothing Nothing
      child=base {conversationViews=M.singleton "child" view,editorDrafts=M.singleton ref (EditorDraft opaque (Selection 0 0) True Nothing)}
      childChanged=child {editorDrafts=M.adjust (\draft->draft {editorDraftBuffer=newBuffer "other draft"}) ref (editorDrafts child)}
  areaKey<-renderKey textArea
  areaWrappedKey<-renderKey areaWrapped
  areaChangedKey<-renderKey areaChanged
  check "dialog wrappers retain redraw identity while TextArea replacement invalidates it" (areaKey==areaWrappedKey && areaKey/=areaChangedKey)
  childKey<-renderKey child
  childChangedKey<-renderKey childChanged
  check "hidden child draft changes invalidate native menus and rendering" (childKey/=childChangedKey)
  replacementBody<-W.prepareTextWindow "Child" "history"
  replacementKey<-renderKey child {conversationViews=M.adjust (\v->v {conversationBody=InertBody replacementBody}) "child" (conversationViews child)}
  check "replacing a retained body invalidates even with identical text" (childKey/=replacementKey)
  forM_ [child {editorDrafts=M.adjust (\draft->draft {editorDraftSelection=Selection 0 1}) ref (editorDrafts child)},
         child {editorDrafts=M.adjust (\draft->draft {editorDraftFocused=False}) ref (editorDrafts child)}] $ \changed->do
    next<-renderKey changed
    check "draft selection and focus invalidate without inspecting history" (next/=childKey)
  let idle=child {dialog=dialog textArea,
        chatQuestion=Just (ChatQuestion 7 "Question" ["One","Two"] Nothing original (Selection 0 0) True),
        sideTree=Just (emptySidebar "/project" 20 False)}
      idleWrapper d=d {systemDark=systemDark d,
        buffers=M.map (\doc->doc {documentFile=fmap (\f->f {filePath=filePath f}) (documentFile doc),documentSourceRows=fmap id (documentSourceRows doc)}) (buffers d),
        conversationViews=M.map (\v->v {conversationDraftRef=conversationDraftRef v}) (conversationViews d),
        editorDrafts=M.map (\draft->draft {editorDraftBuffer=editorDraftBuffer draft}) (editorDrafts d),
        chatQuestion=fmap (\q->q {questionFocused=questionFocused q}) (chatQuestion d),
        dialog=fmap (\dg->dg {fields=map wrapField (fields dg)}) (dialog d),
        sideTree=fmap (\tree->tree {treeSelected=treeSelected tree}) (sideTree d)}
      wrapField (TextArea label editable b selection row column)=TextArea label editable b selection row column
      wrapField field=field
  idleKey<-renderKey idle
  _<-foldM (\d _->do
    let wrapped=idleWrapper d
    next<-renderKey wrapped
    check "100 idle wrapper rebuilds never trigger a redraw" (next==idleKey)
    pure wrapped) idle [1..100::Int]
  let dirtyBuffer=B.replaceSelection (Selection 0 0) "changed " original
      dirtyDesktop=changeDoc (\doc->doc {documentBuffer=dirtyBuffer}) base
      cleanDesktop=changeDoc (\doc->doc {documentBuffer=B.markSaved dirtyBuffer}) dirtyDesktop
  dirtyKey<-renderKey dirtyDesktop
  cleanKey<-renderKey cleanDesktop
  check "markSaved invalidates redraw despite unchanged text and revision"
    (dirtyKey/=cleanKey && B.dirty dirtyBuffer && not (B.dirty (B.markSaved dirtyBuffer)) && B.revision dirtyBuffer==B.revision (B.markSaved dirtyBuffer))
  let reviewedBuffer=B.replaceSelection (Selection 0 3) "new" (newBuffer "old\nkeep\n")
      reviewBase=addDocument Nothing reviewedBuffer (initialDesktop (80,25))
      reviewFull=setBufferView ChangesView reviewBase
      reviewWindow=fromMaybe (error "review window missing") (activeWindow reviewFull)
      reviewRange'=Selection 0 (B.changeLineOffset reviewedBuffer 2)
      selectedReview=modifyActive (\w -> w {reviewSelection=Just (ReviewSelection (B.revision reviewedBuffer) (B.bufferLineChanges reviewedBuffer) UnifiedSide reviewRange'),selection=Selection 0 4}) reviewFull
      copiedReview=fst (runCommand Copy selectedReview)
      cutReview=fst (runCommand Cut selectedReview)
      revertedReview=fst (runCommand (RevertChange (sourceFixtureBuffer reviewWindow) (B.revision reviewedBuffer) (B.bufferLineChanges reviewedBuffer) 0) reviewFull)
      typedReview=insertText "x" selectedReview
      savedReview=selectedReview {buffers=M.adjust (\doc -> doc {documentBuffer=B.markSaved (documentBuffer doc)}) (sourceFixtureBuffer reviewWindow) (buffers selectedReview)}
  check "review view is per window and names all view modes"
    (bufferView reviewWindow==ChangesView && maybe False ((==CurrentView).bufferView) (activeWindow reviewBase) &&
     any (\(MenuItem _ _ c)->c==SetBufferView ChangesView) (menuItems 8))
  check "mixed review selection copies deleted and live rows but cut edits live text only"
    (clipboard copiedReview=="old\nnew\n" && activeText cutReview=="keep\n" && activeText (fst (runCommand Undo cutReview))=="new\nkeep\n")
  check "review hunk reversion is one undo step"
    (activeText revertedReview=="old\nkeep\n" && activeText (fst (runCommand Undo revertedReview))=="new\nkeep\n")
  check "typing clears review selection and save invalidates stale projected selection"
    (maybe False ((==Nothing).reviewSelection) (activeWindow typedReview) && windowReviewRange (B.markSaved reviewedBuffer) reviewWindow {reviewSelection=reviewSelection (fromMaybe reviewWindow (activeWindow selectedReview))}==Nothing &&
     clipboard (fst (runCommand Copy savedReview))/="old\nnew\n")
  check "split preserves independent review mode and divider"
    (all ((==ChangesView).bufferView) (windows (fst (runCommand SplitVertical reviewFull))) && reviewPaneWidths reviewWindow==(38,39))
  let sideBuffer=B.replaceSelection (Selection 0 3) "new" (newBuffer "old")
      sideDesktop=setBufferView SideBySideView (addDocument Nothing sideBuffer (initialDesktop (80,25)))
      leftSelected=fst (runCommand SelectAll (selectAt False 1 2 sideDesktop))
      leftCut=fst (runCommand Cut leftSelected)
      rightSelected=fst (runCommand SelectAll (selectAt False 41 2 sideDesktop))
      defaultsChanged=fst (runCommand (SetDefaultBufferView OnlyChangesView) reviewBase)
      newDefault=addDocument Nothing (newBuffer "fresh") defaultsChanged
      resizedDivider=fst (mouseEvent 65 3 V.BLeft [] sideDesktop {drag=Just (ReviewSizing 1)})
  check "side-by-side original selection copies exact unterminated saved text and never cuts live text"
    (clipboard leftCut=="old" && activeText leftCut=="new" && clipboard (fst (runCommand Copy rightSelected))=="new")
  check "default view affects only new buffers and emits preference persistence"
    (maybe False ((==CurrentView).bufferView) (activeWindow defaultsChanged) && maybe False ((==OnlyChangesView).bufferView) (activeWindow newDefault) &&
     snd (runCommand (SetDefaultBufferView ChangesView) reviewBase)==[SaveBufferViewDefault ChangesView])
  check "hex mode leaves review layout and clears projected selection"
    (maybe False ((==CurrentView).bufferView) (activeWindow (fst (runCommand ToggleHex sideDesktop))))
  check "side-by-side divider is independently draggable"
    (maybe False ((>50).reviewSplit) (activeWindow resizedDivider) && all ((>=4).snd.reviewPaneWidths) (windows resizedDivider))
  let originalContext=newBuffer (T.unlines (map (T.pack.show) [0..29::Int]))
      firstContext=B.replaceSelection (Selection (B.bufferLineOffset originalContext 5) (B.bufferLineOffset originalContext 5+1)) "FIVE" originalContext
      changedContext=B.replaceSelection (Selection (B.bufferLineOffset firstContext 25) (B.bufferLineOffset firstContext 25+2)) "TWENTYFIVE" firstContext
      contextual=setBufferView OnlyChangesView (addDocument Nothing changedContext (initialDesktop (80,25)))
      contextualAll=fst (runCommand SelectAll contextual)
      contextualCut=fst (runCommand Cut contextualAll)
      expectedHidden=T.unlines (map (T.pack.show) ([0..2]++[8..22]++[28..29::Int]))
      navigation=moveTo False (B.bufferLineOffset changedContext 8) (modifyActive (\w->w {selection=Selection (B.bufferLineOffset changedContext 7) (B.bufferLineOffset changedContext 7)}) contextual)
  check "context-only cut retains hidden originals and is one undo step"
    (activeText contextualCut==expectedHidden && not ("10\n" `T.isInfixOf` clipboard contextualCut) &&
     activeText (fst (runCommand Undo contextualCut))==B.contents changedContext)
  check "keyboard navigation skips hidden context gaps to a visible live row"
    (maybe False ((==B.bufferLineOffset changedContext 23).caret.selection) (activeWindow navigation))
  let staleRevert=fst (runCommand (RevertChange (sourceFixtureBuffer reviewWindow) (B.revision reviewedBuffer) (B.bufferLineChanges reviewedBuffer) 0) typedReview)
      contextOpened=fst (windowMouse 1 2 V.BRight [] reviewFull)
  check "hunk context action is available on red rows and rejects stale invocation"
    (case contextKind contextOpened of ChangeContext RevertChange{} -> activeText staleRevert==activeText typedReview; _ -> False)
  let shellRaw="printf \"a  b\\n\"\n"
      shellTuple=(0,10,"bash",shellRaw)
      shellDesktop=let opened=addReadOnly "Help" "printf abc" (initialDesktop (80,25))
                   in opened {buffers=M.adjust (\doc->doc {documentShellBlocks=[shellTuple]}) 1 (buffers opened)}
      shellMenu=fst (windowMouse 2 2 V.BRight [] shellDesktop)
      shellCommand=ExecuteShellBlock (SourceShell 1) shellTuple
      shellEffects=snd (runCommand shellCommand shellDesktop)
      expiredShell=shellDesktop {buffers=M.adjust restyle 1 (buffers shellDesktop)}
  check "shell code-block context forwards exact raw body and rejects stale renderer metadata"
    (contextKind shellMenu==ShellContext shellCommand &&
     shellEffects==[ExecuteShellBlockAction (SourceShell 1) shellTuple] &&
     null (snd (runCommand shellCommand expiredShell)))
  let terminal=modifyActive (\w->w {bounds=Rect 3 4 50 12,selection=Selection 1 4})
        (addReadOnly "Terminal 1" "terminal text" (addDocument Nothing (newBuffer "source") (initialDesktop (100,35))))
      terminalView=fromMaybe (error "terminal missing") (activeWindow terminal)
      ident=windowId terminalView
      pin=fst (runCommand ToggleTerminalPin terminal)
      unpinnedOriginal=fst (runCommand ToggleTerminalPin pin)
      get wid d=fromMaybe (error "window missing") (find ((==wid).windowId) (windows d))
      sameIdentity a b=windowId a==windowId b && sourceFixtureBuffer a==sourceFixtureBuffer b && windowNumber a==windowNumber b && selection a==selection b
  check "terminal pin retains window buffer number selection and restores floating bounds"
    (windowPinned pin (get ident pin) && bounds (get ident pin)==problemsRect pin &&
     sameIdentity terminalView (get ident pin) && sameIdentity terminalView (get ident unpinnedOriginal) &&
     bounds (get ident unpinnedOriginal)==bounds terminalView && buffers unpinnedOriginal==buffers terminal && M.null (dockedTerminals unpinnedOriginal))
  let border=T.lines (snapshot pin) !! top (problemsRect pin)
  check "bottom tab strip joins the panel sides with downward corners" (T.head border=='╔' && T.last border=='╗')
  let clicked=fst (handleEvent (V.EvMouseDown 9 4 V.BLeft []) terminal)
  check "terminal frame pin control docks the existing view" (windowPinned clicked (get ident clicked))
  let second=addReadOnly "Terminal 2" "second" pin
      secondView=fromMaybe (error "second missing") (activeWindow second)
      twoPins=setTerminalPinned True (windowId secondView) second
      withPanelMessages=setProblemsVisible True twoPins
      focused=activateWindowNumber (windowNumber terminalView) withPanelMessages
      cycled=cycleEditorWindow False focused
  check "one shared panel reserves one height and selecting a tab reveals only its terminal"
    (problemsHeight withPanelMessages==problemsHeight pin && messagesDisplayed withPanelMessages &&
     not (windowVisible withPanelMessages (get ident withPanelMessages)) && bottomTerminal focused==Just ident &&
     activeTerminal focused==Just "1" && activeTerminal cycled/=Just "1" &&
     length (filter (windowVisible focused) (filter (windowPinned focused) (windows focused)))==1)
  let dockFocused=activateEditorWindow ident withPanelMessages
      dockIds=[wid | (wid,_,_,_)<-editorWindowEntries withPanelMessages]
  check "Dock catalogue includes unselected terminal tabs and activates their existing view"
    (ident `elem` dockIds && windowId secondView `elem` dockIds &&
     bottomTerminal dockFocused==Just ident && fmap windowId (activeWindow dockFocused)==Just ident)
  let resized=fst (handleEvent (V.EvResize 120 45) focused)
      files=resizeTree 31 (installSidebar (emptySidebar "/tmp" 24 True) resized)
      panel=resizeProblems 30 files
      tiled=fst (runCommand Tile panel)
      cascaded=fst (runCommand Cascade tiled)
      resizedPins=[w | w<-windows cascaded,windowPinned cascaded w]
      undocked=setTerminalPinned False ident cascaded
      closed=closeActive (focusWindow ident twoPins)
  check "screen files panel tile and cascade keep pinned views in the same bottom rectangle"
    (all ((==problemsRect cascaded).bounds) resizedPins && all (\w->top (bounds w)+height (bounds w)<=top (problemsRect cascaded)) (floatingWindows cascaded) &&
     bounds (get ident undocked)==fitWindow undocked (bounds terminalView) && sameIdentity terminalView (get ident undocked))
  check "closing a pinned view removes only its tab and preserves the remaining terminal"
    (M.notMember ident (dockedTerminals closed) && M.member (windowId secondView) (dockedTerminals closed) &&
     bottomTerminal closed==Just (windowId secondView) && length (windows closed)==length (windows twoPins)-1)
  check "docked terminal geometry cannot be changed through normal zoom split or resize"
    (bounds (get ident (fst (runCommand Zoom focused)))==problemsRect focused &&
     length (windows (fst (runCommand SplitVertical focused)))==length (windows focused) &&
     resizeWindowBounds ident (Rect 0 1 40 10) focused==focused)
  let source=modifyActive (\w->w {bounds=Rect 4 3 42 12}) (addDocument Nothing (newBuffer "source") (initialDesktop (100,35)))
      zoomedSource=fst (runCommand Zoom source)
      closeOther=closeActive (addDocument Nothing (newBuffer "other") zoomedSource)
      sourceRestored=fst (runCommand Zoom closeOther)
      sourceWithPanel=setProblemsVisible True source
      zoomedWithPanel=fst (runCommand Zoom sourceWithPanel)
      terminalWithPanel=addReadOnly "Terminal zoom" "output" zoomedWithPanel
      pinWithPanel=fst (runCommand ToggleTerminalPin terminalWithPanel)
      sourceId=maybe (error "source missing") windowId (activeWindow source)
      restoredAfterPin=fst (runCommand Zoom (focusWindow sourceId pinWithPanel))
  check "unrelated close and pin in an existing panel preserve zoom restore bounds"
    (fmap bounds (activeWindow sourceRestored)==fmap bounds (activeWindow source) &&
     fmap bounds (activeWindow restoredAfterPin)==fmap bounds (activeWindow sourceWithPanel))
  let escapedMessages=fst (handleEvent (V.EvKey V.KEsc []) (setProblemsVisible True pin) {problemsFocused=True})
      nextAfterEscape=cycleEditorWindow False escapedMessages
  check "F6 cycles from the visible active window after leaving Messages"
    (fmap windowId (activeWindow nextAfterEscape)/=fmap windowId (activeWindow escapedMessages))
  let pinHover=fst (hoverAt 9 4 terminal)
      unpinHover=fst (hoverAt (fst (screenSize pin)-8) (top (problemsRect pin)) pin)
  check "terminal pin hints are exact and clear when leaving the control"
    (typeHint pinHover=="Dock window at bottom" && typeHint unpinHover=="Unpin window" &&
     typeHint (fst (hoverAt 50 0 unpinHover))=="")
  check "tile scale defaults, environment, overrides and validation"
    (chooseScale Nothing []==Right 0 && chooseScale (Just "3") []==Right 3 &&
     chooseScale (Just "bad") ["1"]==Right 1 && chooseScale (Just "") []==Right 0 &&
     all (either (const True) (const False)) [chooseScale (Just "0") [],chooseScale Nothing ["9"],chooseScale Nothing ["2","3"]])
  check "fractional scales work through flags and environment"
    (chooseScale (Just "1.125") []==Right 1.125 && chooseScale Nothing ["2.5"]==Right 2.5 &&
     chooseScale Nothing ["1.3"]==Right 1.25 &&
     all (either (const True) (const False)) [chooseScale Nothing ["NaN"],chooseScale Nothing ["Infinity"],chooseScale Nothing ["0.9"]])
  check "Control and Alt zoom reset use the same modifiers as zoom in and out"
    (all (\mods -> map (\character -> zoomDirection (fromEnum character) mods) "0+=-"==map Just [0,1,1,-1]) [2,4,3,5] &&
     zoomDirection (fromEnum '0') 0==Nothing && zoomDirection (fromEnum 'a') 2==Nothing)
  check "remote targets retain the exact remote path"
    (parseRemoteTarget "user@eak-pc.local:some-path"==Just ("user@eak-pc.local","some-path") &&
     parseRemoteTarget "host:/tmp/a b;$(x)"==Just ("host","/tmp/a b;$(x)") &&
     parseRemoteTarget "host:"==Just ("host",".") &&
     parseRemoteTarget "C:\\Users\\test"==Nothing && parseRemoteTarget "D:/src"==Nothing &&
     parseRemoteTarget "./local:name"==Nothing && parseRemoteTarget "/tmp/local:name"==Nothing)
  check "terminal remains default" (chooseBackend Nothing [] == Right Terminal)
  check "environment selects metal" (chooseBackend (Just "metal") [] == Right Metal)
  check "explicit terminal beats environment" (chooseBackend (Just "vulkan") [Terminal] == Right Terminal)
  check "explicit backend overrides invalid environment" (chooseBackend (Just "typo") [Metal] == Right Metal)
  check "invalid default rejected" (case chooseBackend (Just "typo") [] of Left _ -> True; _ -> False)
  check "conflicting backend flags rejected" (case chooseBackend Nothing [Metal,Vulkan] of Left _ -> True; _ -> False)
  check "shift-tab" (decodeKey (-9) 1 == Just (V.EvKey V.KBackTab [V.MShift]))
  check "command remains distinct from control" (decodeKey 115 8 == Just (V.EvKey (V.KChar 's') [V.MMeta]))
  check "unknown key ignored" (decodeKey (-999) 0 == Nothing)
  check "character dimensions" (parseWindowSize "100x32" == Right (100,32))
  check "reject tiny dimensions" (case parseWindowSize "1x2" of Left _ -> True; _ -> False)
  check "reject malformed dimensions" (case parseWindowSize "80.5x25" of Left _ -> True; _ -> False)
  check "numbered screen modes accept decimal and hexadecimal"
    (map parseScreenMode ["3","259","0x03","0x103","$103"] == map Right [3,259,3,259,259])
  check "unsupported screen modes rejected" (case parseScreenMode "257" of Left _ -> True; _ -> False)
  check "50 lines fit the same physical height"
    (modeSize 3 == (80,25) && modeSize 259 == (80,50) && modeHeight 3 == 16 && modeHeight 259 == 8)
  let desktop = addDocument Nothing (newBuffer "unsaved buffer") (initialDesktop (80,25)) {videoMode=Just 3}
      preferences = fst (runCommand EditorOptions desktop)
      choose50 = preferences {dialog=fmap (\dg -> dg {fields=[Radio "Key bindings" ["Modern","WordStar"] 0,Radio "Screen size" ["Mode 3 (80x25)","Mode 259 (80x50)"] 1]}) (dialog preferences)}
      (updated,requests) = handleEvent (V.EvKey V.KEnter []) choose50
  check "preferences expose classic screen modes"
    (maybe False (any (\f -> case f of Radio "Screen size" _ 0 -> True; _ -> False) . fields) (dialog preferences))
  check "mode changes preserve buffers and independent key bindings"
    (requests == [SetScreenMode 259] && not (wordStar updated) && buffers updated == buffers desktop)
  check "cancelled preferences do not change modes"
    (snd (handleEvent (V.EvKey V.KEsc []) choose50) == [])
  let taller = resizeScreenMode (80,50) desktop
      split = fst (runCommand SplitHorizontal desktop)
      tallerSplit = resizeScreenMode (80,50) split
  check "mode change fills the taller desktop without changing buffers"
    (map bounds (windows taller) == [Rect 0 1 80 48] && buffers taller == buffers desktop)
  check "mode change preserves tiled split layout"
    (sum (map (height . bounds) (windows tallerSplit)) == 48 &&
     all (\w -> width (bounds w) == 80) (windows tallerSplit) &&
     buffers tallerSplit == buffers split)
  let terminalPreferences = fst (runCommand EditorOptions (initialDesktop (80,25)))
  check "terminal preferences omit window modes"
    (maybe False (not . any (\field -> case field of Radio "Screen size" _ _ -> True; _ -> False) . fields) (dialog terminalPreferences))
  check "CRT defaults off and appears only in window preferences"
    (not (crtFilter desktop) && maybe False (elem (CheckBox "CRT filter" False) . fields) (dialog preferences) &&
     maybe False (not . any (\f -> case f of CheckBox "CRT filter" _ -> True; _ -> False) . fields) (dialog terminalPreferences))
  let toggle dg=dg {fields=map (\f -> case f of CheckBox "CRT filter" _ -> CheckBox "CRT filter" True; _ -> f) (fields dg)}
      changed=preferences {dialog=fmap toggle (dialog preferences)}
  check "CRT applies on OK and cancels without changing buffers"
    (crtFilter (fst (handleEvent (V.EvKey V.KEnter []) changed)) &&
     not (crtFilter (fst (handleEvent (V.EvKey V.KEsc []) changed))) && buffers changed==buffers desktop)
  check "File menu contains Terminal and Change dir"
    (all (`elem` [cmd | (name,_,items)<-menus,name=="File",MenuItem _ _ cmd<-items]) [OpenTerminal,ChangeDir] &&
     OpenTerminal `notElem` [cmd | (name,_,items)<-menus,name=="Run",MenuItem _ _ cmd<-items])
  let two=fst (runCommand New desktop)
      controlTab=fst (handleEvent (V.EvKey (V.KChar '\t') [V.MCtrl]) two)
      controlBack=fst (handleEvent (V.EvKey V.KBackTab [V.MCtrl,V.MShift]) controlTab)
      tree=installSidebar (emptySidebar "/tmp" 24 True) two
      altTab=fst . handleEvent (V.EvKey (V.KChar '\t') [V.MAlt])
      menuFocused=tree {menu=Just (0,0)}
      treeFocusedAgain=altTab menuFocused
      fileFocused=altTab treeFocusedAgain
  check "Ctrl Tab cycles windows and Shift reverses without editing"
    (fmap windowId (activeWindow controlTab)/=fmap windowId (activeWindow two) &&
     fmap windowId (activeWindow controlBack)==fmap windowId (activeWindow two) && buffers controlTab==buffers two)
  check "Alt Tab moves menu to Files to editor and arrows stay in the selected region"
    (maybe False treeFocused (sideTree treeFocusedAgain) && menu treeFocusedAgain==Nothing &&
     maybe False (windowFocused fileFocused) (activeWindow fileFocused) && buffers fileFocused==buffers tree)
  check "Alt Tab cycles dialog fields"
    (fmap focus (dialog (altTab preferences))==fmap ((+1).focus) (dialog preferences))
  let docked=installSidebar (emptySidebar "/tmp" 24 True) desktop
      edge=maybe (error "no Files") ((subtract 1).treeWidth) (sideTree docked)
      floating=modifyActive (\w -> w {bounds=Rect edge 3 30 10}) docked
      rect=fmap bounds . activeWindow
      leftward=resizeTree 19 floating
      rightward=resizeTree 35 floating
      touching=resizeTree 55 rightward
      stuck=resizeTree 35 touching
      movedByMouse=fst (handleEvent (V.EvMouseDown 35 23 V.BLeft [])
        (fst (handleEvent (V.EvMouseDown edge 23 V.BLeft []) floating)))
  check "Files and the adjacent editor share one border column"
    (edge==23 && fmap (left.bounds) (activeWindow docked)==Just edge)
  check "dock resizing carries a floating window without scaling it"
    (rect leftward==Just (Rect 19 3 30 10) && rect rightward==Just (Rect 35 3 30 10) && rect movedByMouse==rect rightward)
  check "editor right edge sticks to the screen once reached"
    (rect touching==Just (Rect 55 3 25 10) && rect stuck==Just (Rect 35 3 45 10) &&
     rect (resizeTree 19 docked)==Just (Rect 19 1 61 23) && buffers stuck==buffers floating)
  let resize size=fst . handleEvent (uncurry V.EvResize size)
      grow=resize (100,40)
      withRect r=modifyActive (\w -> w {bounds=r}) desktop
      bottomOnly=withRect (Rect 5 14 30 10)
      rightOnly=withRect (Rect 40 3 40 10)
      unattached=withRect (Rect 5 3 30 10)
      dockedMessages=setProblemsVisible True docked
      grownMessages=grow dockedMessages
      zoomed=fst (runCommand Zoom rightOnly)
      restored=fst (runCommand Zoom (grow zoomed))
  check "screen growth stretches windows touching both outer edges"
    (rect (grow desktop)==Just (Rect 0 1 100 38) && buffers (grow desktop)==buffers desktop)
  check "screen growth stretches only the attached axis"
    (rect (grow bottomOnly)==Just (Rect 5 14 30 25) && rect (grow rightOnly)==Just (Rect 40 3 60 10))
  check "screen growth leaves floating windows in place" (rect (grow unattached)==rect unattached)
  check "edge attachment survives shrinking and regrowing"
    (rect (grow (resize (80,25) (grow desktop)))==rect (grow desktop))
  check "screen growth respects Files and the bottom Messages dock"
    (rect grownMessages==Just (Rect 23 1 77 30) && problemsRect grownMessages==Rect 0 31 100 8 &&
     fmap treeWidth (sideTree grownMessages)==Just 24)
  check "zoom restores bounds with their original edge attachments"
    (rect (grow zoomed)==Just (Rect 0 1 100 38) && rect restored==rect (grow rightOnly))
  let selector=toolchainBadgeRect desktop
      sx=left selector
      sy=top selector
      mouseInput x y=V.EvMouseDown x y V.BLeft []
      beginSelection=fst (handleEvent (mouseInput 3 3) desktop)
      beginMove=fst (handleEvent (mouseInput 10 1) desktop)
  forM_ [beginSelection,beginMove] $ \captured -> do
    let (crossed,actions)=handleEvent (mouseInput sx sy) captured
        ended=fst (handleEvent (V.EvMouseUp sx sy (Just V.BLeft)) crossed)
        clickedSelector=fst (handleEvent (mouseInput sx sy) ended)
    check "dragging across compiler selector retains capture without invoking it"
      (drag captured/=Nothing && drag crossed==drag captured && contextMenu crossed==Nothing && null actions)
    check "compiler selector opens on a new click after release" (contextMenu clickedSelector/=Nothing)
  let mouse x y=fst . handleEvent (V.EvMouseDown x y V.BLeft [])
      grabbed=mouse 10 1 desktop
      dragged=mouse 20 6 grabbed
      minimumWindow=mouse 150 100 dragged
      released=fst (handleEvent (V.EvMouseUp 150 100 (Just V.BLeft)) minimumWindow)
      cancelled=fst (handleEvent (V.EvKey V.KEsc []) minimumWindow)
      floatingMove=mouse 20 6 (mouse 15 3 unattached)
      dockGrab=mouse 33 1 (dockedMessages {sideTree=fmap (\t->t {treeFocused=False}) (sideTree dockedMessages)})
      dockMove=mouse 43 6 dockGrab
      keyboardMove=fst (handleEvent (V.EvKey V.KRight []) grabbed)
  check "title dragging shrinks a full-size window against both edges"
    (rect dragged==Just (Rect 10 6 70 18) && buffers dragged==buffers desktop)
  check "title dragging stops at the window minimum size"
    (rect minimumWindow==Just (Rect 64 19 16 5) && rect released==rect minimumWindow && drag released==Nothing)
  check "Escape restores the window before movement and shrinkage" (rect cancelled==rect desktop)
  check "title dragging preserves size when there is room" (rect floatingMove==Just (Rect 10 6 30 10))
  check "title dragging shrinks within dock boundaries" (rect dockMove==Just (Rect 33 6 47 10))
  check "keyboard movement uses the same edge shrink behavior" (rect keyboardMove==Just (Rect 1 1 79 23))
  let arranged rs=let geometryBase=addDocument Nothing (newBuffer "geometry") (initialDesktop (100,40))
                      geometryWindow=fromMaybe (error "missing geometry window") (activeWindow geometryBase)
                  in geometryBase {windows=zipWith (\i r -> geometryWindow {windowId=i,windowNumber=i,bounds=r}) [1..] rs}
      rectangles=map bounds . windows
      corner dx dy state=let r=bounds (fromMaybe (error "missing geometry window") (activeWindow state)); x=left r+width r-2; y=top r+height r-1
                         in mouse (x+dx) (y+dy) (mouse x y state)
      release=fst . handleEvent (V.EvMouseUp 0 0 (Just V.BLeft))
      largeAndSmall=arranged [Rect 0 1 40 30,Rect 40 5 20 10]
      carried=corner 10 0 largeAndSmall
      atRight=corner 40 0 largeAndSmall
      pulledBack=corner (-30) 0 (release atRight)
      equalSides=arranged [Rect 0 1 40 30,Rect 40 1 60 30]
      rows=arranged [Rect 0 1 80 15,Rect 20 16 30 8]
      rowAtBottom=corner 0 15 rows
      rowPulled=corner 0 (-10) (release rowAtBottom)
  let stacked=arranged [Rect 0 20 100 19,Rect 0 1 100 19]
      titleGrab=mouse 15 20 stacked
      titlePull=mouse 15 15 titleGrab
  check "title dragging a tiled lower window shrinks its upper neighbor"
    (rectangles titlePull==[Rect 0 15 100 19,Rect 0 1 100 14])
  check "keyboard movement carries the same tiled divider"
    (rectangles (fst (handleEvent (V.EvKey V.KUp []) titleGrab))==
      [Rect 0 19 100 19,Rect 0 1 100 18])
  check "Escape restores both sides after title dragging a tiled divider"
    (rectangles (fst (handleEvent (V.EvKey V.KEsc []) titlePull))==rectangles stacked)
  let tiledRows=fst (runCommand Tile (arranged [Rect 5 5 30 10,Rect 8 8 30 10]))
      lower=focusWindow 2 tiledRows
      lowerGrab=mouse 15 20 lower
      columns=arranged [Rect 50 1 50 38,Rect 0 1 50 38]
      narrowAbove=arranged [Rect 10 20 80 15,Rect 20 10 30 10]
      diagonal=mouse 25 15 (mouse 15 20 narrowAbove)
  check "the actual Tile command produces contacts followed by title dragging"
    (rectangles (mouse 15 15 lowerGrab)==[Rect 0 15 100 19,Rect 0 1 100 14] &&
     buffers titlePull==buffers stacked)
  check "title dragging stops at the neighbor minimum and can return"
    (rectangles (mouse 15 (-50) titleGrab)==[Rect 0 6 100 19,Rect 0 1 100 5] &&
     rectangles (mouse 15 20 titlePull)==rectangles stacked)
  check "title dragging follows horizontal tiled contacts too"
    (rectangles (mouse 55 1 (mouse 65 1 columns))==[Rect 40 1 50 38,Rect 0 1 40 38])
  check "diagonal title dragging retains the original shorter-side contact"
    (rectangles diagonal==[Rect 20 15 80 15,Rect 20 5 30 10])
  check "title dragging leaves partially touching neighbors independent"
    (rectangles (mouse 15 15 (mouse 15 20 (arranged [Rect 10 20 50 15,Rect 40 10 40 10])))==
      [Rect 10 15 50 15,Rect 40 10 40 10])
  check "edge resize carries a fully touching shorter neighbor"
    (rectangles carried==[Rect 0 1 50 30,Rect 50 5 20 10] && buffers carried==buffers largeAndSmall)
  check "carried window sticks after reaching the right desktop edge"
    (rectangles atRight==[Rect 0 1 80 30,Rect 80 5 20 10] &&
     rectangles pulledBack==[Rect 0 1 50 30,Rect 50 5 50 10])
  check "equal touching sides move only their shared divider"
    (rectangles (corner 10 0 equalSides)==[Rect 0 1 50 30,Rect 50 1 50 30])
  check "horizontal contacts carry smaller windows and stick at the bottom"
    (rectangles (corner 0 5 rows)==[Rect 0 1 80 20,Rect 20 21 30 8] &&
     rectangles rowAtBottom==[Rect 0 1 80 30,Rect 20 31 30 8] &&
     rectangles rowPulled==[Rect 0 1 80 20,Rect 20 21 30 18])
  check "diagonal corner resize breaks contact instead of carrying neighbors"
    (rectangles (corner 10 (-3) largeAndSmall)==[Rect 0 1 50 27,Rect 40 5 20 10])
  check "partial side overlap does not attach a window"
    (rectangles (corner 10 0 (arranged [Rect 0 1 40 20,Rect 40 15 20 10]))==
     [Rect 0 1 50 20,Rect 40 15 20 10])
  check "edge propagation continues through a chain of smaller neighbors"
    (rectangles (corner 5 0 (arranged [Rect 0 1 30 30,Rect 30 5 25 20,Rect 55 8 20 10]))==
     [Rect 0 1 35 30,Rect 35 5 25 20,Rect 60 8 20 10])
  check "shared divider stops before either window crosses minimum size"
    (rectangles (corner 80 0 equalSides)==[Rect 0 1 84 30,Rect 84 1 16 30] &&
     rectangles (corner (-80) 0 equalSides)==[Rect 0 1 16 30,Rect 16 1 84 30])
  check "Escape restores all geometry affected by a sticky drag"
    (rectangles (fst (handleEvent (V.EvKey V.KEsc []) carried))==rectangles largeAndSmall)
  let leftSource=arranged [Rect 60 1 40 30,Rect 40 5 20 10]
      leftGrab=mouse 60 10 leftSource
      leftPull=mouse 50 10 leftGrab
      bottomGrab=mouse 6 15 rows
      bottomPull=mouse 6 20 bottomGrab
  check "plain left frame supports sticky edge dragging"
    (rectangles leftPull==[Rect 50 1 50 30,Rect 30 5 20 10])
  let topSource=arranged [Rect 0 20 80 19,Rect 20 10 30 10]
      topGrab=mouse 0 20 topSource
      topPull=mouse 0 15 topGrab
      atTop=mouse 0 6 topGrab
      fromTop=mouse 0 15 atTop
      atLeft=mouse 16 10 leftGrab
      fromLeft=mouse 40 10 atLeft
  check "plain top corner resizes the top edge and carries an upper neighbor"
    (rectangles topPull==[Rect 0 15 80 24,Rect 20 5 30 10])
  check "carried windows stick at the top and left desktop bounds"
    (rectangles atTop==[Rect 0 6 80 33,Rect 20 1 30 5] &&
     rectangles fromTop==[Rect 0 15 80 24,Rect 20 1 30 14] &&
     rectangles atLeft==[Rect 16 1 84 30,Rect 0 5 16 10] &&
     rectangles fromLeft==[Rect 40 1 60 30,Rect 0 5 40 10])
  check "a single large drag clips the carried far edge before shrinking"
    (rectangles (corner 50 0 largeAndSmall)==[Rect 0 1 84 30,Rect 84 5 16 10])
  check "plain bottom frame supports sticky edge dragging"
    (rectangles bottomPull==[Rect 0 1 80 20,Rect 20 21 30 8])
  check "Shift arrows resize with the same sticky edge rule"
    (rectangles (fst (handleEvent (V.EvKey V.KRight [V.MShift]) (mouse 38 30 largeAndSmall)))==
     [Rect 0 1 41 30,Rect 41 5 20 10])
  let withFiles rs=(arranged rs) {sideTree=Just (emptySidebar "/tmp" 24 False)}
      withMessages rs=(arranged rs) {problemsVisible=True,problemsPreferredHeight=8}
      equalFiles=withFiles [Rect 23 1 30 38]
      fileChain=withFiles [Rect 23 5 25 20,Rect 48 8 20 10]
      detachedFile=withFiles [Rect 50 5 25 10]
      equalMessages=withMessages [Rect 0 21 100 10]
      messageChain=withMessages [Rect 20 21 60 10,Rect 30 13 30 8]
      detachedMessage=withMessages [Rect 20 3 30 8]
  check "equal-height Files neighbor keeps its opposite edge stationary"
    (rectangles (resizeTree 30 equalFiles)==[Rect 30 1 23 38] &&
     rectangles (resizeTree 19 equalFiles)==[Rect 19 1 34 38])
  check "Files edge carries shorter contacts through a window chain"
    (rectangles (resizeTree 30 fileChain)==[Rect 30 5 25 20,Rect 55 8 20 10])
  check "Files drag leaves detached windows alone until the dock reaches them"
    (rectangles (resizeTree 30 detachedFile)==[Rect 50 5 25 10] &&
     rectangles (resizeTree 60 detachedFile)==[Rect 60 5 25 10])
  check "Files divider respects the minimum size of a chained neighbor"
    (treeWidthOf (resizeTree 79 fileChain)==59 &&
     rectangles (resizeTree 79 fileChain)==[Rect 59 5 25 20,Rect 84 8 16 10])
  check "equal-width Messages neighbor keeps its top stationary"
    (rectangles (resizeProblems 26 equalMessages)==[Rect 0 21 100 5] &&
     rectangles (resizeProblems 36 equalMessages)==[Rect 0 21 100 15])
  check "Messages edge carries shorter contacts through a window chain"
    (rectangles (resizeProblems 26 messageChain)==[Rect 20 16 60 10,Rect 30 8 30 8])
  check "Messages drag leaves detached windows alone and fits new overlap"
    (rectangles (resizeProblems 26 detachedMessage)==[Rect 20 3 30 8] &&
     rectangles (resizeProblems 23 (withMessages [Rect 20 21 30 5]))==[Rect 20 18 30 5])
  check "Messages divider respects the minimum size of a chained neighbor"
    (top (problemsRect (resizeProblems 7 messageChain))==16 &&
     rectangles (resizeProblems 7 messageChain)==[Rect 20 6 60 10,Rect 30 1 30 5])
  putStrLn "window input checks passed"

  temporary<-getTemporaryDirectory
  let project=temporary </> "project"
      named=addDocument (Just (FileState (project </> "src" </> "Main.hs") Nothing)) (newBuffer "") desktop
      other=addDocument (Just (FileState (project </> "test" </> "Spec.hs") Nothing)) (newBuffer "") named
  check "outer title includes the relative file path"
    (applicationTitle project named==("th "<>T.pack (normalise "src/Main.hs")))
  check "outer title follows the active file and project directory"
    (applicationTitle project other==("th "<>T.pack (normalise "test/Spec.hs")) &&
     applicationTitle (temporary </> "elsewhere") (installSidebar (emptySidebar project 24 True) named)==("th "<>T.pack (normalise "src/Main.hs")) &&
     applicationTitle project named {defaultDirectory=Just (project </> "test")}==("th "<>T.pack (normalise "../src/Main.hs")))
  check "outer title handles empty desktops and unnamed files"
    (applicationTitle project (initialDesktop (80,25))=="th" &&
     applicationTitle project desktop=="th NONAME1.HS")

-- Coordinate references intentionally project the small fixture's original
-- rows; production source windows must query measured rows instead.
sourceCoordinateChecks :: IO ()
sourceCoordinateChecks=do
  let check name ok=unless ok (error name)
      first=T.replicate 600 "ab"<>"\t界e\x301\&👩🏽\x200d\&💻q"<>T.replicate 70 "\x301"<>"Z"
      original=first<>"\r\nshort\t界\n\n"<>T.replicate 600 "x"<>"\r\ntail\r"
      source=newBuffer original
      removed=B.replaceSelection (Selection (T.length first+2) (T.length first+11)) "" source
      changed=T.take (T.length first+2) original<>T.drop (T.length first+11) original
      win d=fromMaybe (error "source coordinate window") (activeWindow d)
      doc d=fromMaybe (error "source coordinate document") (activeDocument d)
      pos d=caret (selection (win d))
  forM_ [(source,original),(removed,changed),(B.undo removed,original)] $ \(buffer,flat)->do
    _<-evaluate (B.prepareBuffer buffer)
    let rows=map (T.dropWhileEnd (=='\r')) (T.splitOn "\n" flat)
        starts=scanl (\start row->start+T.length row+1) 0 (T.splitOn "\n" flat)
        offset row=starts !! row
        reference p=let row=length (takeWhile (<=p) (take (length rows) starts))-1
                    in (row,B.displayColumn (rows !! row) (p-offset row))
        base=modifyActive (\w->w {bounds=Rect 2 2 42 8}) $
          addDocument (Just (FileState "Coordinates.hs" Nothing))
            buffer {B.undoStack=error "source coordinates forced undo",B.redoStack=error "source coordinates forced redo"}
            (initialDesktop (100,25))
        text=B.bufferContent buffer
        cases=[(r,c) | r<-[0..length rows-1],c<-let size=T.length (rows !! r) in
          [n | n<-[0,1,size `div` 2,size]++[1200..1220]++[size-40,size-8],n>=0,n<=size]]
    forM_ cases $ \(row,column)->do
      let p=offset row+column
          focused=modifyActive (\window->window {selection=Selection p p}) base
          w=win focused
          expected=reference p
          visible=ensureVisible focused
          (r,c)=expected
      check "source forward coordinates match original scalar/grapheme geometry"
        (contentPosition text p==expected && windowTextPosition focused w text p==expected &&
         windowCursorCell buffer w==expected && windowCaretCell focused (doc focused) w==expected)
      check "source caret visibility follows the measured coordinate"
        (r>=scrollRow (win visible) && r<scrollRow (win visible)+6 &&
         c>=scrollColumn (win visible) && c<scrollColumn (win visible)+40)
      let destination=min (length rows-1) (row+1)
          moved=verticalMove 1 False focused
          expectedNext=offset destination+B.columnOffset (rows !! destination) c
      check "vertical source movement retains the flat display column"
        (pos moved==expectedNext && B.revision (documentBuffer (doc moved))==B.revision buffer)
      check "source End excludes CRLF and preserves selection anchor"
        (selection (win (rowEdge True True focused))==Selection p (offset row+T.length (rows !! row)))
    forM_ (zip [0..] rows) $ \(row,line)->do
      let extent=B.displayColumn line (T.length line)
      forM_ ([0,1,7,8,max 0 (extent-1),extent,extent+1]++[1200..min 1220 extent]) $ \column->do
        let p=offset row+B.columnOffset line column
            pan=max 0 (column-3)
            located=modifyActive (\window->window {scrollRow=row,scrollColumn=pan}) base
            w=win located; x=left (bounds w)+1+column-pan; y=top (bounds w)+1
            hovered=fst (hoverAt x y located)
            target=if column>=extent then Nothing else Just (sourceFixtureBuffer w,B.revision buffer,p)
        check "source hit, link eligibility and hover retain flat scalar boundaries"
          (windowTextContainsColumn located w text row column==(column<extent) &&
           windowTextOffset located w text row column==p && pos (selectAt False x y located)==p && hoverTarget hovered==target)

-- Cold source geometry must stop at the displayed prefix, while an explicit
-- end seek refines and clamps immediately. The thumb receipt is only a hint:
-- replacing a buffer at the same revision cannot cap subsequent navigation.
sourceScrollChecks :: IO ()
sourceScrollChecks=do
  let check name ok=unless ok (error name)
      body=T.replicate 1200 "a\t界e\x301\&👩🏽\x200d\&💻"<>"tail"
      source=body<>"\r\nshort\r\n"
      base=modifyActive (\w->w {bounds=Rect 0 1 42 8}) $
        addDocument Nothing (newBuffer source) (initialDesktop (80,25))
      win d=fromMaybe (error "source scrollbar window") (activeWindow d)
      doc d=fromMaybe (error "source scrollbar document") (activeDocument d)
      limit d=scrollbarLimit d False (doc d) (win d)
      exact=B.displayColumn body (T.length body)-40+1
      estimate=limit base
      page=changeScroll False 40 base
      arrow=changeScroll False 1 page
      (bar,_)=fromMaybe (error "source scrollbar missing") (windowScrollbar base False (win base))
      ended=scrollTrack False (left bar+width bar-2) (top bar) arrow
      back=changeScroll False (-200) ended
      sourceState d=(selection (win d),B.revision (documentBuffer (doc d)))
  check "cold source scrollbar estimates the untouched suffix" (estimate>exact)
  check "source scrollbar page and arrow preserve ordinary movement"
    (scrollColumn (win page)==40 && scrollColumn (win arrow)==41)
  check "source scrollbar end drag discovers EOF and clamps immediately"
    (scrollColumn (win ended)==exact && limit ended==exact)
  check "moving left retains the discovered thumb extent"
    (scrollColumn (win back)==max 0 (exact-200) && limit back==exact)
  check "scrolling preserves source selection, revision and original bytes"
    (sourceState ended==sourceState base && B.contents (documentBuffer (doc ended))==source)
  let larger=body<>T.replicate 1200 "界\t"<>"end"
      replaced=back {buffers=M.adjust (\d->restyle d {documentBuffer=newBuffer larger})
        (sourceFixtureBuffer (win back)) (buffers back)}
      moved=changeScroll False 250 replaced
      (freshBar,_)=fromMaybe (error "replacement scrollbar missing") (windowScrollbar replaced False (win replaced))
      freshEnd=scrollTrack False (left freshBar+width freshBar-2) (top freshBar) replaced
      freshExact=B.displayColumn larger (T.length larger)-40+1
  check "equal-revision replacement cannot be clamped by a stale thumb hint"
    (scrollColumn (win moved)>exact && scrollColumn (win freshEnd)==freshExact && limit freshEnd==freshExact)
  check "layout changes preserve manual pan past a replaced source's old thumb hint"
    (scrollColumn (win (resizeScreenMode (100,25) moved))==scrollColumn (win moved))
  let shrunk=back {buffers=M.adjust (\d->restyle d {documentBuffer=newBuffer "short\t界\r\n"})
        (sourceFixtureBuffer (win back)) (buffers back)}
  check "a prospective seek clamps a shorter replacement immediately"
    (scrollColumn (win (changeScroll False 1 shrunk))==0)

-- Grouping is presentation only: the title drop must leave one visible frame.
windowTabChecks :: IO ()
windowTabChecks = do
  let base=addDocument (Just (FileState "Second.hs" Nothing)) (newBuffer "second")
          (addDocument (Just (FileState "First.hs" Nothing)) (newBuffer "first") (initialDesktop (100,40)))
      placed=base {windows=case windows base of
        source:target:rest->source {bounds=Rect 4 22 60 12}:target {bounds=Rect 8 3 64 15}:rest
        _->error "two source windows"}
      event e=fst . handleEvent e
      held=event (V.EvMouseDown 19 22 V.BLeft []) placed
      hovered=event (V.EvMouseDown 23 3 V.BLeft []) held
      grouped=event (V.EvMouseUp 23 3 (Just V.BLeft)) hovered
  unless (length (floatingWindows hovered)==2) (error "title hover must not group before release")
  unless (length (floatingWindows grouped)==1 && length (windows grouped)==2)
    (error "dropping title bars groups views without retiring either window")

  let ids=map windowId (windows placed)
      (sourceId,targetId)=case ids of [a,b]->(a,b); _->error "two source frames"
      geometry desktop=map (\w->(bounds w,restoredBounds w)) (windows desktop)
      sameFrame desktop=case geometry desktop of first:rest->all (==first) rest; []->False
      switched=focusWindow targetId grouped
      resized=resizeWindowBounds sourceId (Rect 8 3 45 14) switched
      third=addDocument Nothing (newBuffer "third") grouped
      tour=take 4 (iterate (cycleEditorWindow False) third)
  unless (sameFrame grouped && fmap windowId (activeWindow switched)==Just targetId &&
          sameFrame resized && all ((==Rect 8 3 45 14).bounds) (windows resized))
    (error "tab selection and hidden-member resizing share frame geometry")
  unless (length (M.fromList [(windowId w,()) | state<-tour,Just w<-[activeWindow state]])==3)
    (error "window cycling must escape the selected tab group")
  unless (length (floatingWindows (tileWindows False third))==2)
    (error "tiling places a tab group once")
  let visible=snapshot grouped
  unless ("second" `T.isInfixOf` visible && not ("first" `T.isInfixOf` visible))
    (error "only the selected tab body is rendered")
  let active=fromMaybe (error "selected tab") (activeWindow grouped)
      tabRect=case [r | (r,wid,_)<-windowTabStrip grouped active,wid==sourceId] of
        r:_->r; []->error "selected tab is visible"
      x=left tabRect; y=top tabRect
      captured=event (V.EvMouseDown x y V.BLeft []) grouped
      detached=event (V.EvMouseDown x (y+4) V.BLeft []) captured
      cancelled=event (V.EvKey V.KEsc []) detached
      closedDuringDrag=closeActive (focusWindow targetId detached)
      cancelledAfterClose=event (V.EvKey V.KEsc []) closedDuringDrag
  unless (length (floatingWindows detached)==2 && length (floatingWindows cancelled)==1 && sameFrame cancelled)
    (error "dragging a tab out detaches it and Escape restores the group")
  unless (null (windowTabs cancelledAfterClose) && length (windows cancelledAfterClose)==1)
    (error "Escape must not restore tab membership for a closed window")
  let closed=closeActive grouped
  unless (length (windows closed)==1 && null (windowTabs closed) && M.size (buffers closed)==1)
    (error "closing one tab retains its neighbor and dissolves the singleton group")

  let tiled=tileWindows False placed
      (topFrame,bottomFrame)=case floatingWindows tiled of [a,b]->(a,b); _->error "two tiled frames"
      tx=left (bounds topFrame)+8; ty=top (bounds topFrame)
      bottomTitle=top (bounds bottomFrame)
      tiledPress=event (V.EvMouseDown tx ty V.BLeft []) tiled
      tiledHover=event (V.EvMouseDown tx bottomTitle V.BLeft []) tiledPress
      tiledJoin=event (V.EvMouseUp tx bottomTitle (Just V.BLeft)) tiledHover
  unless (tabDropTarget tiledHover==Just (windowId bottomFrame) && length (floatingWindows tiledJoin)==1)
    (error "sticky neighbors must not move a title-drop target out of reach")
