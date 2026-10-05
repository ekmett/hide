{-# LANGUAGE CPP, OverloadedStrings #-}
module PluginWindowsCheck (checks) where
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad (unless)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import System.Directory (removeFile,getTemporaryDirectory)
import System.IO (openTempFile,hClose)
import Hide.Recovery (writeCheckpoint,readCheckpoint,checkpointKey)
import System.Timeout (timeout)
import Hide.App (applyEffects)
import Hide.Buffer (newBuffer,contents,Selection(..))
import Hide.Commands (configuredBindings,contributedBindingCommands)
import Hide.DocsMCP
import Hide.GuestAccess (guestKeyboardAllowed,pointerAllowedAt,readableAt)
import Hide.MenuCommands
import qualified Hide.Plugin.Window as W
import Hide.PluginWindowHost (adoptWindowUpdate,replaceWindowUpdate,tickPluginWindows)
import Hide.Sidebar
import Hide.SidebarCommands
import Hide.Model
import Hide.Plugin.Command (withRegistry)
import qualified Hide.Plugin.Menu as P
import qualified Hide.Plugin.Tree as PTree
import Hide.Render (snapshot,renderKey,renderCellRows)
import Hide.Unicode (CellSpan(..))
import WindowExtension
#ifdef WITH_PROTOCOL
import Data.Aeson (object,(.=))
import Data.Aeson.Types (parseEither)
import Hide.Protocol hiding (Paste)
import Hide.RemoteWindow
import Hide.RemoteTerminal (terminalEventInput)
#endif

checks :: IO ()
checks=W.withWindowScope $ \scope->withDocsCommands $ \docs->withRegistry $ \registry->withMenuCommands docs $ \host->do
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
  plainOpening<-W.openTextWindow scope plain >>= maybe (fail "plain row opening refused") pure
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
  refresh<-W.refreshTextWindow reference prepared >>= maybe (fail "live refresh refused") pure
  refreshed<-adoptWindowUpdate P.HumanMenu refresh selected
  check "refresh preserves host geometry and semantic selection"
    (fmap (\w->(windowId w,bounds w,selection w)) (activeWindow refreshed)==fmap (\w->(windowId w,bounds w,selection w)) (activeWindow selected) && "Updated text" `T.isInfixOf` snapshot refreshed)
  modalPublication<-W.refreshTextWindow reference prepared >>= maybe (fail "prepare modal refresh") pure
  let selectedModal=prompt "Draft" Information [SelectedInput "Name" "keep draft" (Selection 2 7)] refreshed
  behindModal<-adoptWindowUpdate P.HumanMenu modalPublication selectedModal
  check "exact existing window refresh preserves modal and focused window"
    (dialog behindModal==dialog selectedModal && fmap windowId (activeWindow behindModal)==fmap windowId (activeWindow selectedModal) && M.lookup reference (pluginWindows behindModal)==Just prepared)
  longPrepared<-W.prepareTextWindow "Scrollable output" (T.unlines (replicate 100 (T.replicate 120 "x")))
  longUpdate<-W.openTextWindow scope longPrepared >>= maybe (fail "prepare scrollable output") pure
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
  older<-W.refreshTextWindow reference actorPreparedDummyForRevision >>= maybe (fail "prepare old revision") pure
  newer<-W.refreshTextWindow reference prepared >>= maybe (fail "prepare new revision") pure
  ignored<-adoptWindowUpdate P.HumanMenu older refreshed
  newest<-adoptWindowUpdate P.HumanMenu newer ignored
  check "late older revision cannot overwrite latest prepared publication"
    ("Updated text" `T.isInfixOf` snapshot newest && not ("Older result" `T.isInfixOf` snapshot newest))
  stale<-W.refreshTextWindow reference prepared >>= maybe (fail "prepare queued refresh") pure
  (_,retiredClosed)<-applyEffects closed closeEffects
  closeCurrent<-W.windowRefCurrent reference
  check "actual host close retires exact publication capability" (not closeCurrent)
  afterClosed<-adoptWindowUpdate P.HumanMenu stale retiredClosed
  check "queued refresh cannot resurrect a closed instance" (windows afterClosed==windows closed && M.null (pluginWindows afterClosed))
  actorPreparedDummy<-W.prepareTextWindow "Duplicate" "text"
  originalUpdate<-W.openTextWindow scope actorPreparedDummy >>= maybe (fail "prepare duplicate open") pure
  originalOpened<-adoptWindowUpdate P.HumanMenu originalUpdate source
  duplicate<-adoptWindowUpdate P.HumanMenu originalUpdate originalOpened
  check "duplicate open reply cannot create another window" (length (windows duplicate)==length (windows originalOpened))
  let slotRef=case windowContent <$> activeWindow originalOpened of Just (PluginContent ref)->ref; _->error "missing slot ref"
      slotModal=prompt "Keep draft" Information [SelectedInput "Name" "draft" (Selection 0 5)] originalOpened
  replacementPrepared<-W.prepareTextWindow "New lifetime" "fresh output"
  replacementOpening<-W.openTextWindow scope replacementPrepared >>= maybe (fail "prepare slot replacement") pure
  queuedOld<-W.refreshTextWindow slotRef prepared >>= maybe (fail "prepare old slot reply") pure
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
  actorUpdate<-W.openTextWindow scope actorPrepared >>= maybe (fail "prepare actor update") pure
  denied<-adoptWindowUpdate P.AgentMenu actorUpdate source
  modalDenied<-adoptWindowUpdate P.HumanMenu actorUpdate modal
  check "shared host adapter preserves agent and modal protection" (windows denied==windows source && windows modalDenied==windows modal)
  retired<-W.withWindowScope $ \short->W.openTextWindow short actorPrepared >>= maybe (fail "prepare retired update") pure
  retiredResult<-adoptWindowUpdate P.HumanMenu retired source
  retiredLive<-W.withWindowScope $ \short->do
    update<-W.openTextWindow short actorPrepared >>= maybe (fail "prepare retiring instance") pure
    adoptWindowUpdate P.HumanMenu update source
  placeholder<-tickPluginWindows retiredLive
  check "retirement retains host view and marks its read-only snapshot unavailable"
    (length (windows placeholder)==2 && "Unavailable: Private notes" `T.isInfixOf` snapshot placeholder && M.size (buffers placeholder)==1)
  check "retired scope refuses escaped publication" (windows retiredResult==windows source)
  durable<-W.prepareRecoverableTextWindow "example.notes" 1 "Remembered notes" "durable content" >>= either (fail . T.unpack) pure
  durableUpdate<-W.openTextWindow scope durable >>= maybe (fail "prepare durable instance") pure
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
  putStrLn "plugin window checks passed"
