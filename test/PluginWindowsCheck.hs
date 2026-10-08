{-# LANGUAGE CPP, OverloadedStrings #-}
module PluginWindowsCheck (checks,rowsChecks,imageChecks) where
import Control.Concurrent (threadDelay,yield)
import Control.Concurrent.MVar
import Control.Exception (evaluate,finally)
import Control.Monad (unless,foldM)
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
import Hide.Buffer (newBuffer,contents,contentSlice,contentLength,Selection(..))
import Hide.Commands (configuredBindings,contributedBindingCommands)
import Hide.DocsMCP
import Hide.GuestAccess (guestKeyboardAllowed,pointerAllowedAt,readableAt)
import Hide.Debugger (withDebugger,withDownloadsCommands)
import Hide.MenuCommands
import qualified Hide.Plugin.Editor as E
import qualified Hide.Plugin.Window as W
import Hide.PluginWindowHost (adoptWindowUpdate,replaceWindowUpdate,tickPluginWindows)
import Hide.Sidebar
import Hide.SidebarCommands
import Hide.Model
import Hide.Plugin.Command (withRegistry,CommandError)
import qualified Hide.Plugin.Menu as P
import qualified Hide.Plugin.Tree as PTree
import Hide.Render (snapshot,renderKey,renderCellRows,renderCellRowsAndCanvas,renderCursor)
import Hide.Unicode (CellSpan(..),CellLayer(..),cellRowsAndOwnership)
import WindowExtension
#ifdef WITH_PROTOCOL
import Data.Aeson (Value(..),object,(.=),(.:),withObject)
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
  durable<-W.prepareRecoverableTextWindow "example.notes" 1 "Remembered notes" "durable content" >>= either (fail . T.unpack) pure
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
  putStrLn "plugin window checks passed"

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
  writeCheckpoint path refreshed
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

-- Real PNG preparation, composition and capture exercise the public image route.
imageChecks :: IO ()
imageChecks=do
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
    (Canvas.imagePNG (imageOf public)==bytes && BS.length (Canvas.imageRGBA (imageOf public))==16*16*4)
  W.withWindowScope $ \scope->do
    opened<-ready <$> open scope public (initialDesktop (40,18))
    let initial=scene opened
        surface=case Canvas.canvasSurfaces initial of value:_->value; _->error "missing canvas surface"
        (tx,ty,tw,th)=Canvas.canvasTarget surface
        owner x y=Canvas.canvasOwnerAt initial (y*40+x)
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
    check "new instance cancels panning even when it shares the same image bytes"
      (drag replacementDrag/=Nothing && drag replacedImage==Nothing)
  retired<-W.withWindowScope $ \scope->open scope public (initialDesktop (40,18))
  inactive<-tickPluginWindows retired
  check "scope retirement drops retained pixel/PNG bytes and keeps the inert text"
    (all ((==Nothing).W.preparedWindowImage) (M.elems (pluginWindows inactive)) && null (resource inactive) && "PNG 16" `T.isInfixOf` snapshot inactive)
  let (_,mask)=cellRowsAndOwnership [CellImage (V.string V.defAttr "界"),CellHalo V.defAttr [(2,0,1,1)],CellCanvas 1 [CellImage (V.charFill V.defAttr ' ' 4 1)],CellMask V.defAttr [(3,0,1)]] (4,1)
      masked=Canvas.CanvasScene [] mask
  check "wide glyph halves remain ordinary while halo and privacy share canvas ownership"
    (map (Canvas.canvasOwnerAt masked) [0..3]==[0,0,32769,0])
