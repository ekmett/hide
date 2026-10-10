{-# LANGUAGE CPP, OverloadedStrings #-}
module PluginWindowsCheck (checks,rowsChecks,imageChecks) where
import Control.Concurrent (threadDelay,yield)
import Control.Concurrent.MVar
import Control.Exception (evaluate,finally)
import Control.Monad (unless,foldM,forM_)
import Data.Aeson (Value(..))
import qualified Codec.Picture as Picture
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
import qualified Hide.Plugin.Canvas as Canvas
import Hide.Font (loadFont)
import Hide.ScreenCapture (capture)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import System.Directory (removeFile,getTemporaryDirectory)
import System.IO (openTempFile,hClose)
import Hide.Recovery (writeCheckpoint,readCheckpoint,checkpointKey)
import System.Timeout (timeout)
import System.IO.Unsafe (unsafePerformIO)
import Hide.App (applyEffects)
import Hide.Buffer (newBuffer,contents,contentSlice,contentLength,undo,redo,Selection(..))
import Hide.Commands (configuredBindings,contributedBindingCommands)
import Hide.DocumentationHost
import Hide.GuestAccess (guestKeyboardAllowed,pointerAllowedAt,readableAt,protectedBuffer)
import Hide.Debugger (withDebugger,withDownloadsCommands)
import Hide.MenuCommands
import qualified Hide.Plugin.Editor as E
import Hide.Plugin.Input (InputDeclaration(..),InputUpdate(..))
import Hide.Plugin.Completion (HintServices(..))
import qualified Hide.CompletionInput as CompletionInput
import Hide.Plugin.BufferHost (captureVersion,versionCurrent)
import qualified Hide.Plugin.Window as W
import Hide.PluginWindowHost (adoptWindowUpdate,replaceWindowUpdate,tickPluginWindows)
import Hide.Sidebar
import Hide.SidebarCommands
import Hide.Model
import Hide.Plugin.Command (withRegistry,CommandError(..),CommandDef(..),Codec(..),registerCommand,retireCommand,commandRef,invoke)
import qualified Hide.Plugin.Menu as P
import qualified Hide.Plugin.Tree as PTree
import Hide.Render (snapshot,renderKey,renderCellRows,renderCellRowsAndCanvas,renderCursor)
import Hide.Unicode (CellSpan(..),CellLayer(..),cellRowsAndOwnership)
import WindowExtension
#ifdef WITH_PROTOCOL
import Data.Aeson (object,(.:),withObject)
import qualified Data.Aeson.KeyMap
import Data.Aeson.Types (parseEither)
import Hide.Protocol hiding (Paste)
import Hide.RemoteWindow
import Hide.RemoteTerminal (terminalEventInput)
#endif

checks :: IO ()
checks=W.withWindowScope $ \scope->withDocsCommands $ \docs->withRegistry $ \registry->withMenuCommands docs $ \host->do
  imageChecks
  let check label condition=unless condition (fail label)
      core _ _=error "plugin menu missed typed owner"
  menuReference<-either (error . show) pure =<< registerNotes registry (menuContributions host) scope (pure ()) PreparedWindow
  catalogue<-P.menuSnapshot (menuContributions host)
  let source=addDocument Nothing (newBuffer "background source") (initialDesktop (80,25))
      initial=source {contributedMenus=catalogue,menusActive=True,agentMenuRefs=menuAgentReferences host}
      bindings=either (error . show) id (configuredBindings (contributedBindingCommands initial) (M.singleton "terminal" (M.singleton "source" (M.singleton "example.notes" ["Ctrl+Shift+J"]))))
      (chosen,requests)=handleEvent (V.EvKey (V.KChar 'j') [V.MCtrl,V.MShift]) initial {keyBindings=bindings}
      settle desktop=do
        next<-tickMenus host core desktop
        if activePluginWindow next/=Nothing then pure next else threadDelay 1000 >> settle next
  (_,queued)<-menuEffects host core chosen requests
  opened<-timeout 5000000 (settle queued) >>= maybe (fail "plugin window did not open") pure
  check "typed extension opens a window without a source document"
    (length (windows opened)==2 && M.size (buffers opened)==1 && activeDocument opened==Nothing && maybe False ((==Nothing) . bufferId) (activeWindow opened))
  check "plugin window cannot supply a debugger source target"
    (contextTarget (openContext SourceContext 8 5 opened)==Just UnavailableSourceTarget)
  check "shared frame renders host title and prepared plugin text"
    (all (`T.isInfixOf` snapshot opened) ["Plugin notes","Independent text","Second row"])
  let (selected,_)=handleEvent (V.EvKey (V.KChar 'a') [V.MCtrl]) opened
      (copied,_)=handleEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) selected
  check "effective source-context copy selects semantic plugin text"
    (clipboard copied=="Independent text\nSecond row")
  let attempted=foldl (\desktop action->fst (runCommand action desktop)) selected [Save,SaveAs,Cut,Paste,Undo,Redo,Replace,Complete]
  check "plugin commands cannot edit background source"
    (fmap (contents . documentBuffer) (M.lookup 1 (buffers attempted))==Just "background source" && M.size (buffers attempted)==1 && dialog attempted==Nothing)
  check "guest content/controls remain private without explicit semantics grant"
    (not (guestKeyboardAllowed opened) && not (pointerAllowedAt opened 3 3) && not (readableAt opened 3 3))
  let modal=prompt "Details" Information [TextArea "Draft" True (newBuffer "modal draft") (Selection 0 5) 0 0] opened
      (modalCopy,_)=runCommand Copy modal
  check "modal clipboard uses its field instead of background plugin text" (clipboard modalCopy=="modal")
#ifdef WITH_PROTOCOL
  let packet value=either error id (parseEither parseInput value)
      input event=maybe (error "missing terminal packet") packet (terminalEventInput event)
      native value=maybe (error "missing native packet") packet value
      wireSelected=fst (applyInput (input (V.EvKey (V.KChar 'a') [V.MCtrl])) opened)
      wireCopy=fst (applyInput (input (V.EvKey (V.KChar 'c') [V.MCtrl])) wireSelected)
      mac=opened {nativeMac=True}
      macSelected=fst (applyInput (native (nativeKeyInput (fromEnum 'a') 8)) mac)
      macCopy=fst (applyInput (native (nativeKeyInput (fromEnum 'c') 8)) macSelected)
      browserCopy=fst (applyInput (BrowserCommand Copy) selected {browserFrontend=True})
  frame<-either fail pure (parseRemoteFrame (object (frameMetadata "/tmp" opened)) (frameRows opened))
  check "actual remote frame includes plugin title/window and parsed cells" (remoteTitle frame=="th Plugin notes" && length (remoteWindows frame)==2)
  check "native, terminal and browser clipboard routes agree on plugin text"
    (all ((==clipboard copied) . clipboard) [wireCopy,macCopy,browserCopy])
#endif
  plain<-W.prepareTextWindow "Plain row" "a界e\x0301\t👩🏽\x200d\&💻z\r\nnext"
  plainOpening<-W.openWindow scope plain >>= maybe (fail "plain row opening refused") pure
  plainDesktop<-adoptWindowUpdate P.HumanMenu plainOpening (initialDesktop (30,12))
  let selectedPlain=modifyActive (\w->w {bounds=Rect 0 1 14 7,selection=Selection 1 2}) plainDesktop
      glyphs desktop=[(paint,text,full,start,shown) | CellGlyph paint text full start shown<-Vec.toList (renderCellRows desktop Vec.! 2)]
      clippedPlain=modifyActive (\w->w {scrollColumn=2}) selectedPlain
  check "plain prepared row retains whole selected glyph and borrowed grapheme text"
    (any (\(paint,text,full,start,shown)->text=="界" && (full,start,shown)==(2,0,2) &&
      V.attrForeColor paint==V.SetTo (V.RGBColor 0 0 170) && V.attrBackColor paint==V.SetTo (V.RGBColor 170 170 170)) (glyphs selectedPlain) &&
      any (\(_,text,_,_,_)->text=="e\x0301") (glyphs selectedPlain) &&
      any (\(_,text,_,_,_)->text=="👩🏽\x200d\&💻") (glyphs selectedPlain))
  check "plain row clipping preserves semantic glyph origin while text blanks the half"
    (any (\(_,text,full,start,shown)->text=="界" && (full,start,shown)==(2,1,1)) (glyphs clippedPlain) &&
      not ("界" `T.isInfixOf` snapshot clippedPlain))
  check "private plain row masks complete glyphs before export"
    (null (glyphs clippedPlain {streamerMode=True}))
  let selectedAll=fst (runCommand SelectAll selectedPlain)
  check "plain row display transformations leave copied source unchanged"
    (clipboard (fst (runCommand Copy selectedAll))=="a界e\x0301\t👩🏽\x200d\&💻z\r\nnext")
  let private=opened {streamerMode=True}
  check "private plugin title is masked in application and Dock metadata"
    (applicationTitle "/tmp" private=="th Private plugin window" && all (\(_,title,_,_)->title/="Plugin notes") (editorWindowEntries private))
#ifdef WITH_PROTOCOL
  privateFrame<-either fail pure (parseRemoteFrame (object (frameMetadata "/tmp" private)) (frameRows private))
  check "streamer frame metadata cannot leak private plugin title"
    (remoteTitle privateFrame=="th Private plugin window" && not ("Plugin notes" `T.isInfixOf` T.pack (show (remoteWindows privateFrame))))
#endif
  key<-renderKey opened
  wrapped<-renderKey opened {pluginWindows=M.map id (pluginWindows opened)}
  check "prepared payload wrapper retains shallow render identity" =<< evaluate (key==wrapped)
  let (closing,closeEffects)=runCommand Close opened
  let closed=closing
  check "closing plugin view preserves source and retires its content"
    (length (windows closed)==1 && M.null (pluginWindows closed) && activeDocument closed/=Nothing && M.size (buffers closed)==1)
  prepared<-W.prepareTextWindow "Updated notes" "a\x0301\nUpdated text and a longer selected text"
  let reference=case windowContent <$> activeWindow opened of Just (PluginContent ref)->ref; _->error "missing plugin ref"
  refresh<-W.refreshWindow reference prepared >>= maybe (fail "live refresh refused") pure
  refreshed<-adoptWindowUpdate P.HumanMenu refresh selected
  check "refresh preserves host geometry and semantic selection"
    (fmap (\w->(windowId w,bounds w,selection w)) (activeWindow refreshed)==fmap (\w->(windowId w,bounds w,selection w)) (activeWindow selected) && "Updated text" `T.isInfixOf` snapshot refreshed)
  modalPublication<-W.refreshWindow reference prepared >>= maybe (fail "prepare modal refresh") pure
  let selectedModal=prompt "Draft" Information [SelectedInput "Name" "keep draft" (Selection 2 7)] refreshed
  behindModal<-adoptWindowUpdate P.HumanMenu modalPublication selectedModal
  check "exact existing window refresh preserves modal and focused window"
    (dialog behindModal==dialog selectedModal && fmap windowId (activeWindow behindModal)==fmap windowId (activeWindow selectedModal) && M.lookup reference (pluginWindows behindModal)==Just prepared)
  longPrepared<-W.prepareTextWindow "Scrollable output" (T.unlines (replicate 100 (T.replicate 120 "x")))
  longUpdate<-W.openWindow scope longPrepared >>= maybe (fail "prepare scrollable output") pure
  longOpened<-adoptWindowUpdate P.HumanMenu longUpdate source
  let longView=modifyActive (\w->w {bounds=Rect 2 2 40 12}) longOpened
      click x y desktop=fst (handleEvent (V.EvMouseDown x y V.BLeft []) desktop)
      currentAxis vertical=maybe (-1) (if vertical then scrollRow else scrollColumn) . activeWindow
  mapM_ (\vertical->case activeWindow longView >>= windowScrollbar longView vertical of
    Nothing->fail "semantic output scrollbar missing"
    Just (Rect x y ww hh,limit)->do
      let bottom=click (x+ww-1) (y+hh-1) longView
          thumb=click (if vertical then x else x+1) (if vertical then y+1 else y) longView
          dragged=fst (handleEvent (V.EvMouseDown (if vertical then x else x+ww-2) (if vertical then y+hh-2 else y) V.BLeft []) thumb)
      check "semantic output scrollbar arrow uses live extent" (limit>0 && currentAxis vertical bottom==1)
      check "semantic output scrollbar thumb retains drag owner" (drag thumb==fmap (\w->Scrolling (windowId w) vertical) (activeWindow longView))
      check "semantic output scrollbar drag changes viewport" (currentAxis vertical dragged>1)) [True,False]
  check "semantic output frame paints both scrollbar affordances"
    (all (`T.isInfixOf` snapshot longView) ["▲","▼","◄","►"])
  let (start,_)=runCommand SelectAll refreshed
      caretStart=start {windows=map (\w->w {selection=Selection 0 0}) (windows start)}
      (advanced,_)=handleEvent (V.EvKey V.KRight []) caretStart
  check "plugin navigation respects combining character boundaries" (fmap (caret . selection) (activeWindow advanced)==Just 2)
  actorPreparedDummyForRevision<-W.prepareTextWindow "Older result" "stale text"
  older<-W.refreshWindow reference actorPreparedDummyForRevision >>= maybe (fail "prepare old revision") pure
  newer<-W.refreshWindow reference prepared >>= maybe (fail "prepare new revision") pure
  ignored<-adoptWindowUpdate P.HumanMenu older refreshed
  newest<-adoptWindowUpdate P.HumanMenu newer ignored
  check "late older revision cannot overwrite latest prepared publication"
    ("Updated text" `T.isInfixOf` snapshot newest && not ("Older result" `T.isInfixOf` snapshot newest))
  stale<-W.refreshWindow reference prepared >>= maybe (fail "prepare queued refresh") pure
  (_,retiredClosed)<-applyEffects closed closeEffects
  closeCurrent<-W.windowRefCurrent reference
  check "actual host close retires exact publication capability" (not closeCurrent)
  afterClosed<-adoptWindowUpdate P.HumanMenu stale retiredClosed
  check "queued refresh cannot resurrect a closed instance" (windows afterClosed==windows closed && M.null (pluginWindows afterClosed))
  actorPreparedDummy<-W.prepareTextWindow "Duplicate" "text"
  originalUpdate<-W.openWindow scope actorPreparedDummy >>= maybe (fail "prepare duplicate open") pure
  originalOpened<-adoptWindowUpdate P.HumanMenu originalUpdate source
  duplicate<-adoptWindowUpdate P.HumanMenu originalUpdate originalOpened
  check "duplicate open reply cannot create another window" (length (windows duplicate)==length (windows originalOpened))
  let slotRef=case windowContent <$> activeWindow originalOpened of Just (PluginContent ref)->ref; _->error "missing slot ref"
      slotModal=prompt "Keep draft" Information [SelectedInput "Name" "draft" (Selection 0 5)] originalOpened
  replacementPrepared<-W.prepareTextWindow "New lifetime" "fresh output"
  replacementOpening<-W.openWindow scope replacementPrepared >>= maybe (fail "prepare slot replacement") pure
  queuedOld<-W.refreshWindow slotRef prepared >>= maybe (fail "prepare old slot reply") pure
  invalidSelf<-replaceWindowUpdate P.HumanMenu slotRef queuedOld slotModal
  invalidInstalled<-replaceWindowUpdate P.HumanMenu slotRef longUpdate slotModal {pluginWindows=pluginWindows longOpened `M.union` pluginWindows slotModal}
  selfLive<-W.windowRefCurrent slotRef
  installedLive<-W.windowRefCurrent (W.updateWindowRef longUpdate)
  check "invalid replacement metadata never retires installed lifetimes"
    (selfLive && installedLive && M.lookup slotRef (pluginWindows invalidSelf)==M.lookup slotRef (pluginWindows slotModal) &&
      M.lookup (W.updateWindowRef longUpdate) (pluginWindows invalidInstalled)==Just longPrepared)
  replaced<-replaceWindowUpdate P.HumanMenu slotRef replacementOpening slotModal
  replacementRef<-maybe (fail "missing replacement ref") pure (case windowContent <$> activeWindow replaced of Just (PluginContent ref)->Just ref; _->Nothing)
  oldLate<-adoptWindowUpdate P.HumanMenu queuedOld replaced
  check "owned slot replacement preserves modal, numbering and geometry with a fresh lifetime"
    (dialog replaced==dialog slotModal && length (windows replaced)==length (windows slotModal) &&
      fmap (\w->(windowId w,windowNumber w,bounds w)) (activeWindow replaced)==fmap (\w->(windowId w,windowNumber w,bounds w)) (activeWindow slotModal) && replacementRef/=slotRef)
  check "old slot publication cannot alter replacement" (M.lookup replacementRef (pluginWindows oldLate)==Just replacementPrepared && M.notMember slotRef (pluginWindows oldLate))
  actorPrepared<-W.prepareTextWindow "Private notes" "private"
  actorUpdate<-W.openWindow scope actorPrepared >>= maybe (fail "prepare actor update") pure
  denied<-adoptWindowUpdate P.AgentMenu actorUpdate source
  modalDenied<-adoptWindowUpdate P.HumanMenu actorUpdate modal
  check "shared host adapter preserves agent and modal protection" (windows denied==windows source && windows modalDenied==windows modal)
  retired<-W.withWindowScope $ \short->W.openWindow short actorPrepared >>= maybe (fail "prepare retired update") pure
  retiredResult<-adoptWindowUpdate P.HumanMenu retired source
  retiredLive<-W.withWindowScope $ \short->do
    update<-W.openWindow short actorPrepared >>= maybe (fail "prepare retiring instance") pure
    adoptWindowUpdate P.HumanMenu update source
  placeholder<-tickPluginWindows retiredLive
  check "retirement retains host view and marks its read-only snapshot unavailable"
    (length (windows placeholder)==2 && "Unavailable: Private notes" `T.isInfixOf` snapshot placeholder && M.size (buffers placeholder)==1)
  check "retired scope refuses escaped publication" (windows retiredResult==windows source)
  durable<-W.prepareRecoverableTextWindow "example.notes" 1 W.PrivateWindow "Remembered notes" "durable content" >>= either (fail . T.unpack) pure
  durableUpdate<-W.openWindow scope durable >>= maybe (fail "prepare durable instance") pure
  retained<-adoptWindowUpdate P.HumanMenu durableUpdate source
  let custom=retained {windows=case windows retained of w:rest->w {bounds=Rect 2 3 40 10,selection=Selection 2 5}:rest; []->[]}
  directory<-getTemporaryDirectory
  (path,handle)<-openTempFile directory "hide-plugin-recovery.json"
  hClose handle
  stored<-writeCheckpoint path custom
  check "declared durable plugin state writes checkpoint" (stored==Right ())
  recovered<-readCheckpoint path (initialDesktop (80,25)) >>= either (fail . T.unpack) pure
  removeFile path
  check "missing plugin restores inert view without source document or callback"
    (length (windows recovered)==2 && M.size (buffers recovered)==1 && fmap (windowTitle recovered) (activeWindow recovered)==Just "Unavailable: Remembered notes" && "Unavailable:" `T.isInfixOf` snapshot recovered &&
      "durable content" `T.isInfixOf` snapshot recovered && fmap (\w->(bounds w,selection w)) (activeWindow recovered)==Just (Rect 2 3 40 10,Selection 2 5))
  recoveredCurrent<-maybe (fail "missing restored ref") (\w->case windowContent w of PluginContent ref->W.windowRefCurrent ref; _->fail "restored source instead") (activeWindow recovered)
  check "recovery never restores live registration identity" (not recoveredCurrent)
  persistenceKey<-checkpointKey custom
  wrappedPersistenceKey<-checkpointKey custom {pluginWindows=M.map id (pluginWindows custom)}
  check "plugin recovery invalidation is shallow" (persistenceKey==wrappedPersistenceKey)
  withSidebarCommands $ \sidebar->do
    provider<-registerNotesTree (sidebarRegistry sidebar) scope SidebarWindow >>= either (fail . show) pure
    publishTreeFromHost sidebar provider
    let treeInitial=installSidebar (emptySidebar "/tmp" 20 True) source
        await predicate current=do
          next<-tickSidebar sidebar (\_ _->fail "plugin sidebar escaped closed owner") current
          if predicate next then pure next else threadDelay 1000 >> await predicate next
    mounted<-timeout 5000000 (await (\d->maybe False (any ((=="Plugin notes") . PTree.infoLabel . rowInfo) . M.elems . treeRows) (sideTree d)) treeInitial) >>= maybe (fail "plugin sidebar publication timeout") pure
    let index=case [i | Just tree<-[sideTree mounted],(i,row)<-visibleRows 0 32768 tree,PTree.infoLabel (rowInfo row)=="Plugin notes"] of i:_->i; _->error "missing plugin row"
        focused=mounted {sideTree=fmap (\tree->tree {treeSelected=index,treeFocused=True}) (sideTree mounted)}
        (invoked,effects)=handleEvent (V.EvKey V.KEnter []) focused
    (_,queuedSidebar)<-sidebarEffects sidebar (\_ _->fail "plugin sidebar missed typed path") invoked effects
    sidebarOpened<-timeout 5000000 (await (\d->activePluginWindow d/=Nothing) queuedSidebar) >>= maybe (fail "typed sidebar window did not open") pure
    check "real typed sidebar action uses same prepared-window service" ("Typed sidebar window" `T.isInfixOf` snapshot sidebarOpened && M.size (buffers sidebarOpened)==1)
  withRegistry $ \blockedRegistry->do
    ready<-newEmptyMVar
    release<-newEmptyMVar
    withdrawn<-retireMenuFromHost host menuReference initial
    blockedRef<-registerNotes blockedRegistry (menuContributions host) scope (putMVar ready () >> takeMVar release) PreparedWindow >>= either (fail . show) pure
    metadata<-P.menuSnapshot (menuContributions host)
    let blockedStart=withdrawn {contributedMenus=metadata}
        (invoked,effects)=runCommand (RegisteredMenu blockedRef False) blockedStart
    (_,busy)<-menuEffects host core invoked effects
    entered<-timeout 5000000 (readMVar ready)
    check "typed window preparation entered its worker" (entered==Just ())
    let (selectedSource,_)=runCommand SelectAll busy
        (copiedSource,_)=runCommand Copy selectedSource
    responsive<-timeout 5000000 (tickMenus host core copiedSource) >>= maybe (fail "blocked plugin preparation held host tick") pure
    check "blocked preparation leaves existing copy/tick responsive" (clipboard responsive=="background source" && activePluginWindow responsive==Nothing)
    expired<-retireMenuFromHost host blockedRef responsive
    putMVar release ()
    let awaitExpired current=do
          next<-tickMenus host core current
          if "expired" `T.isInfixOf` status next then pure next else threadDelay 1000 >> awaitExpired next
    refused<-timeout 5000000 (awaitExpired expired) >>= maybe (fail "late retired menu result timeout") pure
    check "retired originating command cannot adopt late plugin window" (activePluginWindow refused==Nothing && length (windows refused)==1)
  editorChecks
  declaredInputChecks
  sidebarEditorRetirementChecks
  emptyEditorRetirementChecks
  putStrLn "plugin window checks passed"

-- Public declarations use the real published window and menu worker. Closing
-- then reopening supplies an owner barrier even when the result preserves input.
declaredInputChecks :: IO ()
declaredInputChecks=W.withWindowScope $ \scope->E.withDraftRef $ \draft->withDocsCommands $ \docs->
  withRegistry $ \registry->withMenuCommands docs $ \host->do
  entered<-newEmptyMVar
  finish<-newEmptyMVar
  let check label condition=unless condition (fail label)
      wait label action=timeout 5000000 action >>= maybe (fail label) pure
      right=either (fail . show) pure
      hidden=Codec Null (const (Left "host input")) (const Null)
      unit=Codec Null (const (Right ())) (const Null)
      definition=CommandDef "example.input" "Input" hidden hidden $ \_ value->
        putMVar entered value >> takeMVar finish
      declaration metadata limit=InputDeclaration metadata limit definition (\slot text->Right (slot,text)) id
      spec=E.EditorSpec False "Apply" "Alternate"
      refused value=case value of Left _->True; Right _->False
      core _ _=error "declared input escaped its menu owner"
      current desktop=editorDraftBuffer (editorDrafts desktop M.! draft)
      close desktop=let (closed,effects)=runCommand Close desktop in snd <$> applyEffects closed effects
      submit modifiers desktop=let (chosen,effects)=handleEvent (V.EvKey V.KEnter modifiers) desktop
        in snd <$> menuEffects host core chosen effects
  badBound<-E.prepareDeclaredEditor registry draft (declaration spec 0) PreparedEditorUpdate
  badLabel<-E.prepareDeclaredEditor registry draft (declaration (spec {E.editorDefaultLabel=""}) 16) PreparedEditorUpdate
  check "invalid input declarations reject at activation" (refused badBound && refused badLabel)
  editor<-E.prepareDeclaredEditor registry draft (declaration spec 16) PreparedEditorUpdate >>= right
  nextEditor<-newIORef editor
  opening<-registerCommand registry (CommandDef "example.input.open" "Input" unit hidden (\_ ()->do
    next<-readIORef nextEditor
    E.remountEditor next >>= writeIORef nextEditor
    pure (Right next))) >>= right
  reference<-P.contributeMenu (menuContributions host)
    (P.MenuDef "example.input.open" "help" "extensions" 11 "Input" "" False
      (P.menuAction registry opening (const (Right ())) (\_ prepared->do
        body<-W.prepareTextWindow "Declared input" "Bounded command input"
        W.openEditorWindow scope body prepared >>= maybe (fail "Input scope retired") (pure . PreparedEditorWindow))))
      >>= either (fail . show) pure
  catalogue<-P.menuSnapshot (menuContributions host)
  let base=(addDocument Nothing (newBuffer "background") (initialDesktop (80,25))) {contributedMenus=catalogue}
      open desktop=do
        next<-tickMenus host core desktop
        if activeEditorMount next/=Nothing then pure next else do
          let (chosen,effects)=runCommand (RegisteredMenu reference False) next
          (_,queued)<-menuEffects host core chosen effects
          yield
          open queued
      run reply modifiers change desktop=do
        busy<-submit modifiers desktop
        receipt<-wait "declared input command did not receive its exact input" (takeMVar entered)
        closed<-close (change busy)
        putMVar finish reply
        reopened<-wait "declared input result did not drain before reopening" (open closed)
        pure (receipt,reopened)
  opened<-wait "declared input did not open through menu worker" (open base)
  let original=newBuffer "original input"
      ready=setComposerInput original (Selection 2 5) True opened
  version<-captureVersion original
  (kept,unchanged)<-run (Right KeepInput) [] id ready
  check "KeepInput consumes its result without changing input or selection" =<<
    ((&& (composerSelection unchanged==Selection 2 5)) <$> versionCurrent version (current unchanged))
  (replaced,changed)<-run (Right (ReplaceInput "changed λ")) [V.MCtrl] id
    (setComposerInput (current unchanged) (Selection 2 5) True unchanged)
  check "ReplaceInput applies its exact worker-prepared text" (contents (current changed)=="changed λ")
  let newer=newBuffer "new typing"
  newerVersion<-captureVersion newer
  (cleared,preserved)<-run (Right ClearInput) [] (setComposerInput newer (Selection 2 5) True)
    (setComposerInput original (Selection 0 0) True changed)
  check "older public clear cannot consume newer typing" =<< versionCurrent newerVersion (current preserved)
  check "declared action slots use exact bounded text"
    ([kept,replaced,cleared]==[(E.DefaultEditor,"original input"),(E.AlternateEditor,"original input"),(E.DefaultEditor,"original input")])
  (_,bounded)<-run (Right (ReplaceInput "replacement exceeding sixteen")) [] id
    (setComposerInput newer (Selection 2 5) True preserved)
  check "oversized replacement preserves the exact input" =<< versionCurrent newerVersion (current bounded)
  (_,failed)<-run (Left (CommandRejected "delivery failed")) [] id
    (setComposerInput newer (Selection 2 5) True bounded)
  check "failed command preserves the exact input and selection" =<<
    ((&& (composerSelection failed==Selection 2 5)) <$> versionCurrent newerVersion (current failed))
  -- Exercise the real first-party command with its acknowledged service error.
  -- The runtime check separately supplies ACP and proves successful adoption.
  case CompletionInput.completionInput of
    InputDeclaration _ _ command arguments _->withRegistry $ \hintRegistry->do
      registered<-registerCommand hintRegistry command >>= right
      input<-right (arguments E.DefaultEditor "keep failed hint")
      calls<-newIORef []
      failure<-invoke hintRegistry registered
        (HintServices (\value->modifyIORef' calls (++[value]) >> pure (Left "provider refused"))) input
      check "actual plugin refuses a failed acknowledged hint" (refused failure)
      check "actual plugin calls the supplied service with the submitted text" . (==["keep failed hint"]) =<< readIORef calls

-- Block the actual pure worker adapter before input admission. This is test-only
-- scheduling control; no receipt or input authority is manufactured here.
{-# NOINLINE editorArguments #-}
editorArguments :: IORef Bool -> MVar () -> MVar () -> MVar () -> E.DraftSubmission -> Bool
  -> Either CommandError (E.DraftSubmission,Bool)
editorArguments gated entered release cancelled input alternate=unsafePerformIO $ do
  blocked<-readIORef gated
  if blocked then (putMVar entered () >> takeMVar release) `finally` putMVar cancelled () else pure ()
  pure (Right (input,alternate))

editorChecks :: IO ()
editorChecks=W.withWindowScope $ \scope->E.withDraftRef $ \draft->withDocsCommands $ \docs->
  withRegistry $ \registry->withMenuCommands docs $ \host->do
  gated<-newIORef False
  entered<-newEmptyMVar
  adapterRelease<-newEmptyMVar
  cancelled<-newEmptyMVar
  accepted<-newEmptyMVar
  finish<-newEmptyMVar
  let check label condition=unless condition (fail label)
      wait label action=timeout 5000000 action >>= maybe (fail label) pure
      core _ _=error "embedded editor escaped its menu owner"
      onSubmit input alternate=putMVar accepted (input,alternate) >> takeMVar finish
      draftText d=maybe (error "lost owned editor draft") (contents . editorDraftBuffer) (M.lookup draft (editorDrafts d))
      close d=let (closed,effects)=runCommand Close d in snd <$> applyEffects closed effects
      submit modifiers d=let (chosen,effects)=handleEvent (V.EvKey V.KEnter modifiers) d in snd <$> menuEffects host core chosen effects
  reference<-registerEditorNotes registry (menuContributions host) scope draft
    (editorArguments gated entered adapterRelease cancelled) onSubmit PreparedEditorWindow PreparedEditorUpdate >>= either (fail . show) pure
  catalogue<-P.menuSnapshot (menuContributions host)
  let source=(addDocument Nothing (newBuffer "background source") (initialDesktop (80,25))) {contributedMenus=catalogue,menusActive=True}
      -- Retrying the opening also proves the owner has drained the prior job.
      open current=do
        next<-tickMenus host core current
        if (activeWindow next >>= windowEditorMount)/=Nothing then pure next else do
          let (chosen,effects)=runCommand (RegisteredMenu reference False) next
          (_,queued)<-menuEffects host core chosen effects
          yield
          open queued
  opened<-wait "embedded editor did not open through menu worker" (open source)
  let window=maybe (error "missing editor window") id (activeWindow opened)
      rect=composerRect opened window
      click offset modifiers d=fst (handleEvent (V.EvMouseDown (left rect+offset) (top rect) V.BLeft modifiers) d)
      focused=click 4 [] opened
      selected=click 6 [V.MShift] focused
      copied=fst (runCommand Copy selected)
      plainRow=T.take 15 (T.drop (left rect) (T.lines (snapshot opened) !! top rect))
      bodySelected=modifyActive (\w->w {selection=Selection 2 7}) (setComposerInput (composerBuffer opened) (Selection 0 0) False opened)
      bodyCopy=fst (runCommand Copy bodySelected)
      escapeEffects=snd (handleEvent (V.EvKey V.KEsc []) focused {agentReplying=True})
  check "plain editor leading spaces use matching click, selection and cursor geometry"
    (composerSelection focused==Selection 4 4 && composerSelection selected==Selection 4 6 && clipboard copied=="pl" &&
      plainRow=="    plain draft" && renderCursor selected==V.Cursor (left rect+6) (top rect))
  check "plugin body copy uses its own nonzero semantic selection" (clipboard bodyCopy=="depen")
  check "generic editor Escape cannot cancel an unrelated chat" (AgentAction "cancel" [] `notElem` escapeEffects)
  let originalMount=maybe (error "missing original editor mount") id (windowEditorMount window)
  defaultBusy<-submit [] focused
  (defaultInput,defaultAlternate)<-wait "default editor handler was not admitted" (takeMVar accepted)
  check "default adapter enters registered command with immutable input"
    (not defaultAlternate && E.submissionSlot defaultInput==E.DefaultEditor && E.submissionMount defaultInput==originalMount &&
      contentSlice (E.submissionContent defaultInput) 0 (contentLength (E.submissionContent defaultInput))=="    plain draft")
  hidden<-close defaultBusy
  responsive<-wait "accepted hidden work held owner tick" (tickMenus host core hidden)
  check "accepted close retains hidden draft without reopening a frame"
    (draftText responsive=="    plain draft" && length (windows responsive)==1)
  putMVar finish ()
  remounted<-wait "hidden completion did not drain and remount" (open responsive)
  let nextMount=activeWindow remounted >>= windowEditorMount
  check "exact-version hidden completion clears retained input before fresh remount"
    (draftText remounted=="" && nextMount/=Just originalMount && fmap E.mountDraft nextMount==Just draft &&
      fmap E.mountActions nextMount==Just (E.mountActions originalMount) && M.size (buffers remounted)==1)
  let alternateDraft=setComposerInput (newBuffer "alternate draft") (Selection 0 0) True remounted
  alternateBusy<-submit [V.MCtrl] alternateDraft
  (alternateInput,alternate)<-wait "alternate editor handler was not admitted" (takeMVar accepted)
  check "alternate adapter uses the same command with its own operation"
    (alternate && E.submissionSlot alternateInput==E.AlternateEditor && E.submissionAction alternateInput==E.submissionAction defaultInput)
  changedHidden<-close (setComposerInput (newBuffer "newer draft") (Selection 5 5) True alternateBusy)
  putMVar finish ()
  staleRemounted<-wait "stale completion did not drain and remount" (open changedHidden)
  check "older accepted result cannot clear a changed hidden draft"
    (draftText staleRemounted=="newer draft" && composerSelection staleRemounted==Selection 5 5)
  writeIORef gated True
  preparing<-submit [] (setComposerInput (composerBuffer staleRemounted) (Selection 0 0) True staleRemounted)
  wait "editor argument adapter did not enter worker" (readMVar entered)
  abandoned<-close preparing
  retiring<-wait "unaccepted close held owner tick" (tickMenus host core abandoned)
  wait "unaccepted adapter was not cancelled" (readMVar cancelled)
  unexpected<-tryTakeMVar accepted
  check "close cancels preparation before command admission and preserves hidden input"
    (case unexpected of Nothing->draftText retiring=="newer draft" && length (windows retiring)==1; Just _->False)
  writeIORef gated False
  usable<-wait "cancelled worker left owner unavailable" (open retiring)
  finalBusy<-submit [] (setComposerInput (composerBuffer usable) (Selection 0 0) True usable)
  _<-wait "owner could not admit action after cancellation" (takeMVar accepted)
  putMVar finish ()
  let clear current=do
        next<-tickMenus host core current
        if draftText next=="" then pure next else yield >> clear next
  final<-wait "action after cancellation did not clear exact input" (clear finalBusy)
  check "embedded workflow leaves background source unchanged"
    (fmap (contents . documentBuffer) (M.lookup 1 (buffers final))==Just "background source")
  let pendingSeed=setComposerInput (newBuffer "private preserved seed") (Selection 3 8) True final
  pending<-submit [] pendingSeed
  _<-wait "retiring editor submission did not enter its worker" (takeMVar accepted)
  let mount=maybe (error "missing retiring editor mount") id (activeWindow pending >>= windowEditorMount)
  _<-retireCommand registry (fst (E.mountActions mount)) >>= either (fail . show) pure
  preserved<-tickMenus host core pending
  (preservedId,_,preservedWindow)<-checkPreservedDraft "visible menu seed" "private preserved seed" (Selection 3 8) pending preserved
  putMVar finish ()
  let drain current=do
        next<-tickMenus host core current
        if status next=="Editor owner expired." then pure next else yield >> drain next
  late<-wait "late retired menu result did not drain" (drain preserved)
  repeated<-tickMenus host core late
  check "late menu result cannot clear preserved input or duplicate its document"
    (M.size (buffers repeated)==2 && maybe False ((=="private preserved seed") . contents . documentBuffer) (M.lookup preservedId (buffers repeated)))
  let preservedFocus=focusWindow (windowId preservedWindow) repeated
      copiedSeed=fst (runCommand Copy (fst (runCommand SelectAll preservedFocus)))
  check "preserved input uses ordinary editable document copy"
    (clipboard copiedSeed=="private preserved seed" && activeDocument preservedFocus/=Nothing)

-- Withdrawal must preserve the user's input through the actual public owner,
-- including a closed frame whose callable binding is still retained.
sidebarEditorRetirementChecks :: IO ()
sidebarEditorRetirementChecks=W.withWindowScope $ \scope->E.withDraftRef $ \draft->withRegistry $ \registry->withSidebarCommands $ \host->do
  let check label condition=unless condition (fail label)
      wait label action=timeout 5000000 action >>= maybe (fail label) pure
      core _ _=error "sidebar editor escaped its typed owner"
      unit=Codec Null (const (Right ())) (const Null)
      opaque=Codec Null (const (Left "Editor input is host-owned.")) (const Null)
  submitCommand<-registerCommand registry (CommandDef "example.sidebar.draft.submit" "Submit draft" opaque opaque (\_ input->pure (Right input))) >>= either (fail . show) pure
  let action=E.editorAction registry submitCommand Right (\_ input->pure (SidebarEditorUpdate (E.clearEditorDraft input)))
  editor<-E.prepareEditor draft (E.EditorSpec False "Apply" "Apply") "private sidebar seed" action action >>= either (fail . show) pure
  opening<-registerCommand registry (CommandDef "example.sidebar.draft.open" "Draft" unit unit (\_ ()->pure (Right ()))) >>= either (fail . show) pure
  let node=either (error . T.unpack) id (PTree.nodeId "draft")
      prepare _ ()=do
        body<-W.prepareTextWindow "Sidebar draft" "Provider body"
        W.openEditorWindow scope body editor >>= maybe (fail "Editor scope expired") (pure . SidebarEditorWindow)
      root=PTree.NodeDef (PTree.NodeInfo node "Sidebar draft" "" False Nothing) (Just (PTree.treeAction registry opening () prepare)) []
  provider<-PTree.registerTree registry "example.sidebar.draft" root (\_ _->pure (Right (PTree.NodePage [] Nothing))) >>= either (fail . show) pure
  publishTreeFromHost host provider
  let source=installSidebar (emptySidebar "/tmp" 20 True) (addDocument Nothing (newBuffer "background source") (initialDesktop (90,30)))
      untilReady predicate current=do
        next<-tickSidebar host core current
        if predicate next then pure next else yield >> untilReady predicate next
  published<-wait "sidebar draft did not publish" (untilReady (maybe False (any ((=="Sidebar draft") . PTree.infoLabel . rowInfo) . M.elems . treeRows) . sideTree) source)
  let index=case [i | Just tree<-[sideTree published],(i,row)<-visibleRows 0 32768 tree,PTree.infoLabel (rowInfo row)=="Sidebar draft"] of
        i:_->i
        _->error "Published sidebar draft has no visible row"
      chosen=published {sideTree=fmap (\tree->tree {treeSelected=index,treeFocused=True}) (sideTree published)}
      (invoked,effects)=handleEvent (V.EvKey V.KEnter []) chosen
  (_,queued)<-sidebarEffects host core invoked effects
  opened<-wait "sidebar draft did not open through its action worker" (untilReady (maybe False ((/=Nothing) . windowEditorMount) . activeWindow) queued)
  let atEnd=setComposerInput (composerBuffer opened) (Selection 20 20) True opened
      edited=fst (runCommand Paste atEnd {clipboard=" + edit"})
      (hidden,closeEffects)=runCommand Close edited
  (_,closed)<-applyEffects hidden closeEffects
  check "frame close keeps the unsent sidebar draft and original source"
    (M.member draft (editorDrafts closed) && length (windows closed)==1 && M.size (buffers closed)==1)
  _<-retireCommand registry (commandRef submitCommand) >>= either (fail . show) pure
  let modal=prompt "Existing modal" Information [] closed
  preserved<-tickSidebar host core modal
  (_,document,_)<-checkPreservedDraft "hidden sidebar draft" "private sidebar seed + edit" (Selection 27 27) modal preserved
  check "preserved sidebar input retains ordinary Undo and Redo"
    (contents (undo (documentBuffer document))=="private sidebar seed" && contents (redo (undo (documentBuffer document)))=="private sidebar seed + edit")
  repeated<-tickSidebar host core preserved
  check "retired hidden binding preserves one document only" (M.size (buffers repeated)==2)

emptyEditorRetirementChecks :: IO ()
emptyEditorRetirementChecks=W.withWindowScope $ \scope->E.withDraftRef $ \draft->withDocsCommands $ \docs->withRegistry $ \registry->withMenuCommands docs $ \host->do
  let core _ _=error "empty editor escaped its menu owner"
      await current=do
        next<-tickMenus host core current
        if (activeWindow next >>= windowEditorMount)/=Nothing then pure next else yield >> await next
  reference<-registerEditorNotes registry (menuContributions host) scope draft (\input alternate->Right (input,alternate)) (\_ _->pure ()) PreparedEditorWindow PreparedEditorUpdate >>= either (fail . show) pure
  catalogue<-P.menuSnapshot (menuContributions host)
  let source=(addDocument Nothing (newBuffer "background source") (initialDesktop (80,25))) {contributedMenus=catalogue,menusActive=True}
      (chosen,effects)=runCommand (RegisteredMenu reference False) source
  (_,queued)<-menuEffects host core chosen effects
  opened<-timeout 5000000 (await queued) >>= maybe (fail "empty editor did not open") pure
  let empty=fst (runCommand Cut (fst (runCommand SelectAll opened)))
      mount=maybe (error "missing empty editor mount") id (activeWindow empty >>= windowEditorMount)
  _<-retireCommand registry (fst (E.mountActions mount)) >>= either (fail . show) pure
  retired<-tickMenus host core empty
  unless (M.size (buffers retired)==1 && M.null (editorDrafts retired) && all ((==Nothing) . windowEditorMount) (windows retired))
    (fail "empty retired editor must release without creating an unsaved document")

checkPreservedDraft :: String -> T.Text -> Selection -> Desktop -> Desktop -> IO (Int,Document,Window)
checkPreservedDraft label expected selected before after=do
  let check description condition=unless condition (fail (label++": "++description))
      additions=[(ident,document) | (ident,document)<-M.toList (buffers after),M.notMember ident (buffers before)]
  (ident,document)<-case additions of [value]->pure value; _->fail (label++": unsent input did not become one reachable document")
  window<-case [w | w<-windows after,bufferId w==Just ident] of [value]->pure value; _->fail (label++": preserved document has no unique frame")
  check "text and selection survive in an editable unsaved document"
    (contents (documentBuffer document)==expected && selection window==selected && documentLabel document==Nothing && documentFile document==Nothing && documentModified document && windowEditorMount window==Nothing)
  check "background preservation does not steal focus or modal ownership"
    (fmap windowId (activeWindow after)==fmap windowId (activeWindow before) && fmap dialogTitle (dialog after)==fmap dialogTitle (dialog before) && drag after==drag before)
  let focused=focusWindow (windowId window) after {dialog=Nothing}
      closePrompt=dialog (fst (runCommand Close focused))
      savePrompt=dialog (fst (runCommand Save focused))
  check "ordinary Close and Save protect a clean untitled seed"
    (maybe False ((==Confirm Close) . purpose) closePrompt && maybe False (\dg->case purpose dg of Saving bid _->bid==ident; _->False) savePrompt)
  check "composer privacy stays with the preserved document"
    (documentPrivate document && protectedBuffer after ident && not (guestKeyboardAllowed focused) && not (readableAt focused (left (bounds window)+1) (top (bounds window)+1)) && not (pointerAllowedAt focused (left (bounds window)+1) (top (bounds window)+1)) && not (expected `T.isInfixOf` snapshot focused {streamerMode=True}))
  pure (ident,document,window)

-- The real host route keeps job selection separate from changing Details text.
rowsChecks :: IO ()
rowsChecks=W.withWindowScope $ \scope->withDocsCommands $ \docs->withMenuCommands docs $ \host->withDebugger $ \runtime->withDownloadsCommands host runtime $ do
  catalogue<-P.menuSnapshot (menuContributions host)
  let check label condition=unless condition (fail label)
      ident=either (error . T.unpack) id . PTree.nodeId
      a=ident "a"; b=ident "b"
      source=(addDocument Nothing (newBuffer "source stays unchanged") (initialDesktop (80,25))) {contributedMenus=catalogue}
      sourceSelection=selection <$> activeWindow source
      install prepared desktop=do
        update<-W.openWindow scope prepared >>= maybe (fail "window scope closed") pure
        adoptWindowUpdate P.HumanMenu update desktop
      selected desktop=activeWindow desktop >>= rowsInteraction
      move event= fst . handleEvent event
#ifdef WITH_PROTOCOL
  let startup=(initialDesktop (80,25)) {contributedMenus=catalogue,menusActive=True}
  startupFrame<-either fail pure (parseRemoteFrame (object (frameMetadata "." startup)) (frameRows startup))
  check "default Downloads context contribution round trips through startup native metadata"
    ([(contributionSlot item,contributionEnabled item) | item<-remoteContributions startupFrame,contributionId item=="hide.downloads.cancel"]==[("context.window-rows",False)])
  check "context-only Downloads action does not become a top-level native menu"
    (all (\(_,entries)->all (\(title,_,_)->title/="Cancel transfer") entries) (remoteMenuLayout startupFrame))
  let invalid=startup {contributedMenus=[if P.menuName (P.menuReference item)=="hide.downloads.cancel" then item {P.menuSlot="context.unknown"} else item | item<-catalogue]}
  check "native receiver still rejects unknown host menu slots"
    (case parseRemoteFrame (object (frameMetadata "." invalid)) (frameRows invalid) of Left _->True; Right _->False)
#endif
  detailA<-W.prepareTextWindow "A" "AAA details\nnext line"
  detailB<-W.prepareTextWindow "B" "BBB details"
  prepared<-W.prepareRecoverableRowsWindow "hide.downloads" 1 "Transfers" []
    [W.WindowRow a "A" detailA,W.WindowRow b "B" detailB] >>= either (fail . T.unpack) pure
  opened<-install prepared source
  check "unrelated readonly rows do not advertise Downloads Cancel" (null (contextItemsFor opened {contextKind=WindowRowsContext}))
  check "rows open as a nonmodal plugin window" (dialog opened==Nothing && selected opened==Just (RowsInteraction a False))
  longDetails<-W.prepareTextWindow "Details" (T.intercalate "\n" (replicate 60 "line"))
  longPage<-W.prepareRowsWindow "Long details" [] [W.WindowRow a "A" longDetails] >>= either (fail . T.unpack) pure
  longWindow<-install longPage source
  let current=maybe (error "missing long rows") id (activeWindow longWindow)
      (bar,_)=maybe (error "missing Details scrollbar") id (windowScrollbar longWindow True current)
      paged=fst (handleEvent (V.EvMouseDown (left bar) (top bar+2) V.BLeft []) longWindow)
  check "Details track pages by its viewport rather than the full list window"
    (fmap scrollRow (activeWindow paged)==Just (height (snd (rowsWindowRects longWindow current))))
  let chosen=move (V.EvKey V.KDown []) opened
      focused=move (V.EvKey (V.KChar '\t') []) chosen
      allDetails=fst (runCommand SelectAll focused)
      copied=fst (runCommand Copy allDetails)
  check "list selection navigates by ID and Details copies only selected text"
    (selected copied==Just (RowsInteraction b True) && clipboard copied=="BBB details")
  changedA<-W.prepareTextWindow "A" "A longer progress details"
  changedB<-W.prepareTextWindow "B" "BBB details updated"
  changed<-W.prepareRecoverableRowsWindow "hide.downloads" 1 "Transfers" [] [W.WindowRow b "B progress" changedB,W.WindowRow a "A progress" changedA] >>= either (fail . T.unpack) pure
  let window=maybe (error "no window") id (activeWindow copied)
      reference=case windowContent window of PluginContent ref->ref; _->error "no plugin"
      resized=resizeWindowBounds (windowId window) ((bounds window) {width=60,height=19}) copied
  publication<-W.refreshWindow reference changed >>= maybe (fail "refresh refused") pure
  refreshed<-adoptWindowUpdate P.HumanMenu publication resized
  check "progress/reorder/resize retains stable selected ID and Details selection"
    (selected refreshed==Just (RowsInteraction b True) && fmap selection (activeWindow refreshed)==fmap selection (activeWindow copied))
  check "bounded painter displays list and selected Details" (all (`T.isInfixOf` snapshot refreshed) ["B progress","A progress","BBB details updated"])
  let win=maybe (error "no window") id (activeWindow refreshed)
      listRect=fst (rowsWindowRects refreshed win)
      wheeled=move (V.EvMouseDown (left listRect+1) (top listRect) V.BScrollDown []) refreshed
  check "wheel over list moves row selection" (selected wheeled==Just (RowsInteraction a False))
  check "source selection is untouched" (sourceSelection==fmap selection (findSource wheeled))
  temporary<-getTemporaryDirectory
  (path,handle)<-openTempFile temporary "hide-rows-recovery"
  hClose handle
  _<-writeCheckpoint path refreshed
  restored<-readCheckpoint path (initialDesktop (80,25)) >>= either (fail . T.unpack) pure
  removeFile path
  let inert=activePluginWindow restored
  check "rows recover only inert labels with no Details offsets or actions"
    (maybe False (\value->case W.preparedWindowRows value of W.PlainRows{}->True; _->False) inert &&
      fmap rowsInteraction (activeWindow restored)==Just Nothing && fmap selection (activeWindow restored)==Just (Selection 0 0))
  let popup=openContext WindowRowsContext (left listRect+2) (top listRect+1) refreshed
      popupRect=maybe (error "missing rows popup") fst (contextMenu popup)
  check "private rows popup cannot accept guest input or expose overlay cells"
    (not (guestKeyboardAllowed popup) && not (readableAt popup (left popupRect+1) (top popupRect+1)))
  duplicate<-W.prepareRowsWindow "bad" [] [W.WindowRow a "A" detailA,W.WindowRow a "duplicate" detailB]
  nested<-W.prepareRowsWindow "bad" [] [W.WindowRow a "nested" prepared]
  check "rows factory rejects duplicate IDs and nested Details" (either (const True) (const False) duplicate && either (const True) (const False) nested)
  where
    findSource desktop=case filter ((/=Nothing).bufferId) (windows desktop) of w:_->Just w; _->Nothing

-- A small asymmetric JPEG proves orientation independently of lossy encoding:
-- expected pixels come from the untagged reference and explicit source indices.
jpegChecks :: IO ()
jpegChecks=do
  let jpeg=BL.toStrict (Picture.encodeJpegAtQuality 100 (Picture.generateImage (\x y->Picture.PixelYCbCr8 (fromIntegral (30+60*x+20*y)) 128 128) 3 2))
      reference=either error Picture.convertRGBA8 (Picture.decodeJpeg jpeg)
      pixel n=case Picture.pixelAt reference (n `mod` 3) (n `div` 3) of Picture.PixelRGBA8 r g b a->BS.pack [r,g,b,a]
      check label condition=unless condition (fail label)
      prepare source=Canvas.prepareImage source >>= either (fail . T.unpack) pure
      number :: Bool -> Int -> Integer -> BS.ByteString
      number little count value=BS.pack [fromIntegral (value `div` (256^index) `mod` 256) | index<-if little then [0..count-1] else reverse [0..count-1]]
      exif little orientation extra=let
        word=number little 2
        long=number little 4
        entries=word 274<>word 3<>long 1<>word orientation<>word 0<>
          if extra then word 256<>word 4<>long 4294967295<>long 38 else BS.empty
        tiff=(if little then "II" else "MM")<>word 42<>long 8<>word (if extra then 2 else 1)<>entries<>long 0<>
          if extra then long 0 else BS.empty
        payload="Exif\0\0"<>tiff
        in BS.pack [255,225]<>wordLength (BS.length payload+2)<>payload
      wordLength :: Int -> BS.ByteString
      wordLength value=BS.pack [fromIntegral (value `div` 256),fromIntegral (value `mod` 256)]
      tagged tag=BS.take 2 jpeg<>tag<>BS.drop 2 jpeg
      cases=[(1,[0,1,2,3,4,5]),(2,[2,1,0,5,4,3]),(3,[5,4,3,2,1,0]),(4,[3,4,5,0,1,2]),
             (5,[0,3,1,4,2,5]),(6,[3,0,4,1,5,2]),(7,[5,2,4,1,3,0]),(8,[2,5,1,4,0,3])]
  plain<-prepare jpeg
  check "JPEG signature and resource preserve original source format and opaque RGBA"
    (Canvas.imageContentFormat (BS.take 8 jpeg)==Just "JPEG" && Canvas.isImageContent jpeg && Canvas.imageFormat plain=="JPEG" &&
     Canvas.imageEncoded plain==jpeg && Canvas.imageWidth plain==3 && Canvas.imageHeight plain==2 && Canvas.imageRGBA plain==BS.concat (map pixel [0..5]))
  forM_ cases $ \(orientation,indices)->do
    let source=tagged (exif (even orientation) orientation False)
        expectedSize=if orientation>=5 then (2,3) else (3,2)
    image<-prepare source
    check "all EXIF orientations use correct displayed dimensions and row-major RGBA"
      ((Canvas.imageWidth image,Canvas.imageHeight image)==expectedSize && Canvas.imageRGBA image==BS.concat (map pixel indices) && Canvas.imageEncoded image==source)
  safeMetadata<-prepare (tagged (exif True 6 True))
  check "JPEG decode does not allocate from unrelated EXIF vector counts"
    (Canvas.imageRGBA safeMetadata==BS.concat (map pixel [3,0,4,1,5,2]))
  invalidOrientation<-prepare (tagged (exif False 9 False))
  check "invalid EXIF orientation defaults to stored pixel order" (Canvas.imageRGBA invalidOrientation==Canvas.imageRGBA plain)
  let frame code width height=BS.pack [255,code]<>wordLength 11<>BS.singleton 8<>wordLength height<>wordLength width<>BS.pack [1,1,17,0]
      start=BS.pack [255,216]
      scan=BS.pack [255,218,0,8,1,1,0,0,63,0]
      end=BS.pack [255,217]
      firstScan=start<>frame 192 3 2<>scan<>BS.singleton 0
      refused label expected source=do
        result<-Canvas.prepareImage source
        check label (either (T.isInfixOf expected) (const False) result)
  refused "JPEG source limit precedes decode" "16 MiB" (BS.take 3 jpeg<>BS.replicate (16777216-2) 0)
  refused "JPEG oversized first frame is rejected before decode" "limit" (start<>frame 192 4097 1)
  refused "JPEG pixel area is bounded independently of side lengths" "limit" (start<>frame 192 2049 2048)
  refused "JPEG later SOF cannot escape predecode bounds" "limit" (firstScan<>frame 192 4097 1<>end)
  refused "JPEG DNL cannot introduce dimensions after preflight" "DNL" (firstScan<>BS.pack [255,220,0,4,0,2]<>end)
  refused "JPEG zero-height frame requires unsupported dynamic dimensions" "limit" (start<>frame 192 3 0)
  refused "JPEG multiple frames are rejected before decode" "multiple" (start<>frame 192 3 2<>frame 192 3 2<>scan<>end)
  refused "JPEG unsupported frame kind is rejected before decode" "Unsupported" (start<>frame 195 3 2)
  refused "JPEG truncated marker length is rejected before decode" "marker structure" (start<>BS.pack [255,224,255,255])

-- Real PNG preparation, composition and capture exercise the public image route.
imageChecks :: IO ()
imageChecks=do
  jpegChecks
  let check label condition=unless condition (fail label)
      bytes=BL.toStrict (Picture.encodePng (Picture.generateImage (\_ _->Picture.PixelRGBA8 200 80 40 128) 16 16))
      prepare disclosure=W.prepareImageWindow "sample.png" disclosure (Just "/images/sample.png") bytes >>= either (fail . T.unpack) pure
      scene=snd . renderCellRowsAndCanvas
      resource desktop=map (Canvas.imageResourceId . Canvas.canvasImage) (Canvas.canvasSurfaces (scene desktop))
      open scope body desktop=W.openWindow scope body >>= maybe (fail "image open expired") (\update->adoptWindowUpdate P.HumanMenu update desktop)
      imageOf body=maybe (error "prepared PNG missing") id (W.preparedWindowImage body)
      ready desktop=modifyActive (\w->w {bounds=Rect 2 2 24 10}) desktop
  invalid<-W.prepareImageWindow "broken" W.ReadableWindow Nothing "not a png"
  let oversized=BS.take 16 bytes<>BS.pack [0,0,32,0]<>BS.drop 20 bytes
  huge<-W.prepareImageWindow "huge" W.ReadableWindow Nothing oversized
  check "invalid and oversized PNGs are refused before image admission" (either (const True) (const False) invalid && either (const True) (const False) huge)
  public<-prepare W.ReadableWindow
  private<-prepare W.PrivateWindow
  check "PNG resource keeps exact originals and strict RGBA dimensions"
    (Canvas.imageFormat (imageOf public)=="PNG" && Canvas.imageEncoded (imageOf public)==bytes && BS.length (Canvas.imageRGBA (imageOf public))==16*16*4)
  W.withWindowScope $ \scope->do
    opened<-ready <$> open scope public (initialDesktop (40,18))
    let initial=scene opened
        surface=case Canvas.canvasSurfaces initial of value:_->value; _->error "missing canvas surface"
        (tx,ty,tw,th)=Canvas.canvasTarget surface
        owner x y=Canvas.canvasOwnerAt initial (y*40+x)
    let control=maybe (error "image control missing") id (Canvas.canvasControls (Canvas.canvasVisibleCells initial 40) surface)
        actual=applyImageAction control Canvas.ActualImageSize opened
        enlarged=applyImageAction control Canvas.ZoomImageIn actual
        reduced=applyImageAction control Canvas.ZoomImageOut enlarged
        views=map imageViewport . windows
        unchanged desktop target=views (applyImageAction target Canvas.ZoomImageIn desktop)==views desktop
    check "accessible image operations share keyboard fit and zoom behavior"
      (views actual==[Canvas.CanvasView (Just 1) 0 0] && views enlarged==[Canvas.CanvasView (Just 1.25) 0 0] &&
       views reduced==views actual && views (applyImageAction control Canvas.FitImage enlarged)==views opened)
    check "image actions reject stale instance, resource and hit geometry"
      (unchanged opened (control {Canvas.controlView="99999999"}) &&
       unchanged opened (control {Canvas.controlResource=T.replicate 48 "f"}) &&
       unchanged opened (control {Canvas.controlAnchor=(0,0)}) &&
       unchanged (message "Approval" ["Continue?"] actual) control)
    check "covered or modal images publish no semantic control"
      (Canvas.canvasControls (Canvas.canvasVisibleCells (initial {Canvas.canvasMask=BS.replicate (40*18*2) 0}) 40) surface==Nothing &&
       all (not . Canvas.canvasInteractive) (Canvas.canvasSurfaces (scene (message "Approval" ["Continue?"] actual))))
#ifdef WITH_PROTOCOL
    let request=Canvas.imageActionPacket control Canvas.ActualImageSize
        parsed=either error id (parseEither parseInput request)
    check "native/browser/remote image packet decodes to the same exact action"
      (parseEither Canvas.parseImageActionPacket request==Right (control,Canvas.ActualImageSize) && views (fst (applyInput parsed opened))==views actual)
    denied<-applyGuestInput parsed opened
    check "semantic image controls do not grant agent input authority" (case denied of Left _->True; _->False)
#endif
    check "image owns content cells but never host chrome" (owner 3 3==1 && owner 2 2==0 && BS.length (Canvas.canvasMask initial)==40*18*2)
    check "canvas preserves source aspect in logical pixels" (abs (tw*8-th*16)<0.00001)
    check "software image sample composites straight alpha onto black"
      (Canvas.canvasPixel initial 40 (tx+tw/2) (ty+th/2)==Just (100,40,20))
    let zoomed=fst (handleEvent (V.EvKey (V.KChar '+') []) opened)
        panned=fst (handleEvent (V.EvKey V.KRight []) zoomed)
        fitted=fst (handleEvent (V.EvKey (V.KChar 'f') []) panned)
    check "zoom/pan change placement and retain the immutable resource"
      (resource opened==resource panned && map Canvas.canvasTarget (Canvas.canvasSurfaces (scene panned))/=map Canvas.canvasTarget (Canvas.canvasSurfaces initial))
    let configured=either (error . T.unpack) id (configuredBindings [] M.empty)
        mapped=opened {keyBindings=configured}
        mappedZoom=fst (handleEvent (V.EvKey (V.KChar '+') [V.MShift]) mapped)
        mappedPan=fst (handleEvent (V.EvKey V.KRight []) mappedZoom)
        modal=message "Protected approval" ["Continue?"] mapped
        modalAfter=fst (handleEvent (V.EvKey (V.KChar '+') []) modal)
    check "compiled source bindings reach image pan and unbound image zoom"
      (map imageViewport (windows mappedPan)/=map imageViewport (windows mappedZoom) && map imageViewport (windows mappedZoom)/=map imageViewport (windows mapped))
    let findPrompt=prompt "Find" (Searching False "") [Input "Text" "needle" 6] mapped
    check "active image hides its own cursor while preserving the modal input cursor"
      (renderCursor mapped==V.NoCursor && case renderCursor findPrompt of V.Cursor{}->True; _->False)
    check "modal input cannot change the covered image" (map imageViewport (windows modalAfter)==map imageViewport (windows modal))
    check "fit restores the same transform" (map Canvas.canvasTarget (Canvas.canvasSurfaces (scene fitted))==map Canvas.canvasTarget (Canvas.canvasSurfaces initial))
    let (armed,_)=handleEvent (V.EvMouseDown 3 7 V.BLeft []) opened
        (_,external)=handleEvent (V.EvMouseUp 3 7 (Just V.BLeft)) armed
        graphic=opened {videoMode=Just 3}
        (dragging,_)=handleEvent (V.EvMouseDown 3 7 V.BLeft []) graphic
        (dragged,_)=handleEvent (V.EvMouseDown 5 8 V.BLeft []) dragging
    check "terminal fallback external-open link uses its captured host origin"
      (case external of [FollowLink origin ""]->linkOriginPath origin==Just "/images/sample.png"; _->False)
    check "graphical image contents pan instead of activating an invisible fallback link"
      (null (snd (handleEvent (V.EvMouseUp 3 7 (Just V.BLeft)) dragging)) && map imageViewport (windows dragged)/=map imageViewport (windows graphic))
    let covered=modifyActive (\w->w {bounds=Rect 1 1 15 9}) (addDocument Nothing (newBuffer "foreground") opened)
        coveredScene=scene covered
    check "ordinary front window clears image ownership and its exposed halo dims image cells"
      (Canvas.canvasOwnerAt coveredScene (3*40+3)==0 && Canvas.canvasOwnerAt coveredScene (3*40+16)==32769 && resource covered==resource opened)
    let nowPrivate=opened {guestPrivatePaths=["/images"]}
    check "current protected paths remove resource and semantic image surfaces before transport"
      (null (Canvas.canvasSurfaces (scene nowPrivate {streamerMode=True})) && not (readableAt nowPrivate 3 3))
    hidden<-ready <$> open scope private (initialDesktop (40,18))
    check "private pixels stay owner-visible and absent from streamer scenes"
      (not (null (resource hidden)) && null (resource hidden {streamerMode=True}))
    many<-foldM (\desktop _->open scope public desktop) (initialDesktop (40,18)) [1..65::Int]
    check "host caps image surfaces at 64 while sharing one decoded resource" (length (windows many)==64)
    font<-loadFont
    captured<-capture font opened True >>= either (fail . T.unpack) pure
    hiddenCapture<-capture font hidden True >>= either (fail . T.unpack) pure
#ifdef WITH_PROTOCOL
    let png value=do
          blocks<-parseEither (withObject "capture" (.: "content")) value
          encoded<-case [text | Object fields<-blocks,Just (String text)<-[Data.Aeson.KeyMap.lookup "data" fields]] of
            text:_->Right text; _->Left "missing captured PNG"
          encodedBytes<-B64.decode (TE.encodeUtf8 encoded)
          Picture.convertRGB8 <$> Picture.decodePng encodedBytes
    publicPixels<-either fail pure (png captured)
    privatePixels<-either fail pure (png hiddenCapture)
    check "agent PNG includes authorized image pixels and blacks private image content"
      (Picture.pixelAt publicPixels (floor ((tx+tw/2)*8)) (floor ((ty+th/2)*16))==Picture.PixelRGB8 100 40 20 &&
       Picture.pixelAt privatePixels (3*8+1) (3*16+1)==Picture.PixelRGB8 0 0 0)
#else
    captured `seq` hiddenCapture `seq` pure ()
#endif
    let reference=case windowContent <$> activeWindow dragging of Just (PluginContent value)->value; _->error "missing image reference"
        refresh body desktop=W.refreshWindow reference body >>= maybe (fail "image refresh expired") (\update->adoptWindowUpdate P.HumanMenu update desktop)
    sameImage<-refresh public dragging
    replacementImage<-prepare W.ReadableWindow
    changedImage<-refresh replacementImage sameImage
    check "same image refresh retains panning but new resource cancels the captured gesture"
      (drag sameImage==drag dragging && drag changedImage==Nothing)
    newInstance<-W.openWindow scope replacementImage >>= maybe (fail "image replacement expired") pure
    let (replacementDrag,_)=handleEvent (V.EvMouseDown 3 7 V.BLeft []) changedImage
    replacedImage<-replaceWindowUpdate P.HumanMenu reference newInstance replacementDrag
    let changedScene=scene changedImage
        changedSurface=case Canvas.canvasSurfaces changedScene of value:_->value; _->error "changed image control missing"
        changedControl=maybe (error "replacement control missing") id (Canvas.canvasControls (Canvas.canvasVisibleCells changedScene 40) changedSurface)
    check "queued image controls cannot affect a replacement view even with the same resource"
      (unchanged changedImage control && unchanged replacedImage changedControl)
    check "new instance cancels panning even when it shares the same image bytes"
      (drag replacementDrag/=Nothing && drag replacedImage==Nothing)
  retired<-W.withWindowScope $ \scope->open scope public (initialDesktop (40,18))
  inactive<-tickPluginWindows retired
  check "scope retirement drops retained pixel/PNG bytes and keeps the inert text"
    (all ((==Nothing).W.preparedWindowImage) (M.elems (pluginWindows inactive)) && null (resource inactive) && "PNG 16" `T.isInfixOf` snapshot inactive)
  let (_,mask)=cellRowsAndOwnership [CellImage (V.string V.defAttr "界"),CellHalo V.defAttr [(2,0,1,1)],CellCanvas 1 [CellImage (V.charFill V.defAttr ' ' (4::Int) 1)],CellMask V.defAttr [(3,0,1)]] (4,1)
      masked=Canvas.CanvasScene [] mask
  check "wide glyph halves remain ordinary while halo and privacy share canvas ownership"
    (map (Canvas.canvasOwnerAt masked) [0..3]==[0,0,32769,0])
