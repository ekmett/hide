{-# LANGUAGE CPP, OverloadedStrings #-}
module RecoveryCheck (checks) where

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory
import System.FilePath ((</>))
import System.IO (openTempFile,hClose)
#ifndef mingw32_HOST_OS
import Data.Bits ((.&.))
import System.Posix.Files (fileMode,getFileStatus)
#endif
import Hide.Buffer
import Hide.Files
import Hide.BufferView
import Hide.Sidebar
import Hide.Model
import Hide.Recovery
import qualified Hide.Plugin.Editor as E
import qualified Hide.Plugin.Window as W
import Hide.Syntax (Style(..))

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->W.withWindowScope $ \scope->do
  keyChecks
  primaryRef<-E.newDraftRef
  childRef<-E.newDraftRef
  let path=root </> "session.checkpoint"
      sourcePath=root </> "source.hs"
      original=newBuffer "abc λ\n"
      edited=undo (replaceSelection (Selection 0 0) "prefix " (replaceSelection (Selection 1 2) "中" original))
      bytes=BS.pack [0,255,10,128]
      hex=undo (replaceSelection (Selection 0 0) "B" (replaceSelection (Selection 1 2) "A" (newByteBuffer bytes)))
      fresh=(initialDesktop (80,25)) {guestPrivatePaths=[root </> "fresh-config"],nativeMac=True,browserFrontend=True}
      source=modifyActive (\w -> w {bufferView=SideBySideView,reviewSplit=63}) (addDocument (Just (FileState sourcePath (Just (bufferBytes original)))) edited (initialDesktop (100,35)))
      sourceId=sourceFixtureBuffer (fromJust (activeWindow source))
      split=fst (runCommand SplitVertical source)
      binary=addDocument Nothing hex split
      binaryId=sourceFixtureBuffer (fromJust (activeWindow binary))
      transcript="Session: old-provider-id\nPublic transcript\nOther: private pending answer"
      draft=replaceSelection (Selection 0 0) "private draft λ\n\n    main = 1\n      continuation\n" (newBuffer "")
  primaryBody<-semanticBody "Primary" (map (\c->(c,Plain)) (T.unpack transcript)) W.CopyText
    [(0,T.length "Session: old-provider-id"),(T.length "Session: old-provider-id\nPublic transcript\n",T.length transcript)]
  conversation<-showBody scope "" "Primary" (installDraft "" "Primary" primaryBody primaryRef draft (Selection 1 5) True binary)
  let terminal=addReadOnly "Terminal 7" "last terminal output" conversation
      terminalId=sourceFixtureBuffer (fromJust (activeWindow terminal))
      approval=addReadOnly "Agent request" "pending approval body" terminal
      approvalId=sourceFixtureBuffer (fromJust (activeWindow approval))
      desktop=approval {defaultDirectory=Just root,
        sideTree=Just ((emptySidebar root 23 True) {treeHints=Just (SidebarHints (M.singleton sourcePath False) (Just sourcePath) (Just sourcePath))}),problemsVisible=True,problemsPreferredHeight=9,
        wordStar=True,wideSectionTitles=True,blinkCursor=False,pixelateUnicode=True,materialIcons=True,appearance=DarkMode,streamerMode=True,chatSubmit=SteerSubmit,
        dialog=Just (Dialog "Pending permission" (PermissionDialog "approve:secret") [] 0 ["Allow"] []),
        menu=Just (0,0),drag=Just (Selecting sourceId),clipboard="transient clipboard",clipboardCode=Just "transient clipboard",clipboardExport=(3,Just "export"),
        agentReplying=True,agentQueued=3,agentSettings=[AgentSetting "token" "Token" "private" "secret" []],
        agentContextUsage=Just (1,2),diagnostics=[Diagnostic sourcePath Nothing 0 0 1 "old diagnostic"]}
      get d ident=documentBuffer (buffers d M.! ident)
      right=either (error . T.unpack) pure
      check label ok=unless ok (error label)
  BS.writeFile sourcePath (bufferBytes original)
  writeCheckpoint path desktop >>= right
#ifndef mingw32_HOST_OS
  permissions<-fileMode <$> getFileStatus path
  check "checkpoint is owner-private" (permissions .&. 0o077==0)
#endif
  recovered<-readCheckpoint path fresh {childAgentSettings=[AgentSetting "model" "Model" "model" "old" [("old","Old")]],childAgentSteering=True,childAgentContextUsage=Just (1,2)} >>= right
  check "recovery preserves wide preference but never cached presentation payload" (wideSectionTitles recovered && M.null (windowPresentations recovered))
  check "recovery preserves exact text state and both history stacks" (snapshotBuffer (get recovered sourceId)==snapshotBuffer edited)
  check "recovered undo and redo behave exactly like the original"
    (snapshotBuffer (undo (get recovered sourceId))==snapshotBuffer (undo edited) && snapshotBuffer (redo (get recovered sourceId))==snapshotBuffer (redo edited))
  check "hex bytes saved representation and history survive recovery" (snapshotBuffer (get recovered binaryId)==snapshotBuffer hex && bufferBytes (redo (get recovered binaryId))==bufferBytes (redo hex))
  check "split windows retain shared buffer IDs geometry and selection" (map windowState (windows recovered)==map windowState (filter ((/=Just approvalId).bufferId) (windows desktop)))
  check "conversation transcript and private draft history survive"
    ("Public transcript" `T.isInfixOf` bodyText "" recovered && not ("private pending answer" `T.isInfixOf` bodyText "" recovered) && not ("old-provider-id" `T.isInfixOf` bodyText "" recovered) && snapshotBuffer (composerBuffer recovered)==snapshotBuffer draft && composerSelection recovered==Selection 1 5)
  check "ended terminals are inert read-only views and approval buffers are omitted"
    (documentLabel (buffers recovered M.! terminalId)==Just "Ended Terminal 7" && M.notMember approvalId (buffers recovered))
  check "fresh runtime privacy/frontend values survive and transient controls reset"
    (guestPrivatePaths recovered==guestPrivatePaths fresh && nativeMac recovered && browserFrontend recovered && agentSettings recovered==agentSettings fresh &&
     dialog recovered==Nothing && menu recovered==Nothing && drag recovered==Nothing && clipboard recovered=="" && clipboardCode recovered==Nothing && clipboardExport recovered==(0,Nothing) &&
     null (childAgentSettings recovered) && not (childAgentSteering recovered) && childAgentContextUsage recovered==Nothing && not (agentReplying recovered) && agentQueued recovered==0 && agentContextUsage recovered==Nothing && null (diagnostics recovered))
  check "project sidebar dock geometry and display preferences survive"
    (defaultDirectory recovered==Just root && fmap (\tree->(treeRoot tree,treeWidth tree,treeFocused tree)) (sideTree recovered)==Just (root,23,True) && screenSize recovered==(100,35) && problemsVisible recovered && problemsPreferredHeight recovered==9 &&
     wordStar recovered && not (blinkCursor recovered) && pixelateUnicode recovered && materialIcons recovered && appearance recovered==DarkMode && streamerMode recovered && chatSubmit recovered==SteerSubmit)
  let pinnedPath=root </> "pinned.checkpoint"
      terminalWindowId=maybe (error "missing terminal") windowId (activeWindow terminal)
      pinned=setTerminalPinned True terminalWindowId terminal
  writeCheckpoint pinnedPath pinned >>= right
  recoveredPinned<-readCheckpoint pinnedPath fresh >>= right
  check "recovered terminal tabs preserve identity and layout without restarting or routing input"
    (dockedTerminals recoveredPinned==dockedTerminals pinned && bottomTerminal recoveredPinned==Just terminalWindowId &&
     bounds (fromJust (activeWindow recoveredPinned))==problemsRect recoveredPinned &&
     documentLabel (buffers recoveredPinned M.! terminalId)==Just "Ended Terminal 7" && activeTerminal recoveredPinned==Nothing)
  let viewsPath=root </> "views.checkpoint"
      hiddenSession="Session: old-hidden-provider-id\n"
      hiddenSessionLength=T.length hiddenSession
      primaryText=T.replicate (hiddenSessionLength-1) " "<>"\nhi\nx..ok"<>T.replicate 80 "primary transcript\n"
      primaryCells=[(c,BubbleText 1 True Plain) | c<-T.unpack hiddenSession]++[(c,BubbleText 1 True (LinkStyle "https://private.invalid" Plain)) | c<-"hi\nx"]++
        [(c,Plain) | c<-".."]++[(c,BubbleText 2 False Plain) | c<-"ok"]++
        [(c,Plain) | c<-T.unpack (T.replicate 80 "primary transcript\n")]
      primaryDraft=undo (replaceSelection (Selection 0 0) "redo " (replaceSelection (Selection 0 0) "primary draft" (newBuffer "")))
      childText=T.replicate 80 "child transcript\n"
      childDraft=replaceSelection (Selection 0 0) "child draft" (newBuffer "")
  primaryPrepared<-W.prepareSemanticTextWindow "Primary" primaryCells
    (W.TextSemantics (W.CopyMessages W.UserBotAttribution) (Just root)
      (V.singleton (0,2,"https://private.invalid")) (V.singleton (0,2,"sh","printf private-shell"))
      W.ReadableWindow V.empty V.empty (V.singleton (0,hiddenSessionLength))) >>= right
  childPrepared<-semanticBody "Child" [(c,Plain) | c<-T.unpack childText] W.CopyText []
  initialPrimary<-showBody scope "" "Primary" (installDraft "" "Primary" primaryPrepared primaryRef primaryDraft (Selection 2 5) False (initialDesktop (80,25)))
  let primaryChat=modifyActive (\w->w {scrollRow=12,selection=Selection 1 6})
        (setComposerInput (composerBuffer initialPrimary) (Selection 2 5) False initialPrimary)
  childChat<-showBody scope "agent-2" "Child" (installDraft "agent-2" "Child" childPrepared childRef childDraft (Selection 1 4) True (closeActive (rememberConversationView primaryChat)))
  let populated=modifyActive (\w->w {scrollRow=8,selection=Selection 2 7}) childChat
  writeCheckpoint viewsPath populated >>= right
  restoredViews<-readCheckpoint viewsPath fresh >>= right
  let primaryView=conversationViews restoredViews M.! ""
      childView=conversationViews restoredViews M.! "agent-2"
      primaryState=editorDrafts restoredViews M.! conversationDraftRef primaryView
      childState=editorDrafts restoredViews M.! conversationDraftRef childView
  check "closed hidden target recovers as one inert body outside the installed map"
    (conversationBodyRef primaryView==Nothing && M.size (pluginWindows restoredViews)==1 &&
      all (\w->conversationTargetFor restoredViews w==Just "agent-2") (windows restoredViews))
  let closedRecovered=closeActive restoredViews
  check "closing an inert recovered frame retains its one body without editor bindings"
    (null (windows closedRecovered) && M.null (pluginWindows closedRecovered) &&
      conversationBodyRef (conversationViews closedRecovered M.! "agent-2")==Nothing && bodyText "agent-2" closedRecovered==childText)
  restoredPrimary<-showBody scope "" "Primary" restoredViews
  restoredChild<-showBody scope "agent-2" "Child" restoredPrimary
  check "each conversation retains its prepared body draft selection and scroll"
    (activeText restoredPrimary==primaryText && contents (composerBuffer restoredPrimary)=="primary draft" &&
     composerSelection restoredPrimary==Selection 2 5 && scrollRow (fromJust (activeWindow restoredPrimary))==12 &&
     activeText restoredChild==childText && contents (composerBuffer restoredChild)=="child draft" &&
     composerSelection restoredChild==Selection 1 4 && scrollRow (fromJust (activeWindow restoredChild))==8)
  let recoveredBody=fromJust (conversationBodySnapshot "" restoredViews)
      copied=fst (runCommand Copy (modifyActive (\w->w {selection=Selection hiddenSessionLength (hiddenSessionLength+8)}) restoredPrimary))
      copyMetadata=fromJust (W.preparedWindowSemantics recoveredBody)
  check "inert recovery retains passive message attribution and decorated newline copy"
    (clipboard copied=="User: hi\nx\n\nBot: ok" &&
      W.copyPreparedSelection recoveredBody 0 (hiddenSessionLength+8)=="User: hi\nx\n\nBot: ok" && V.null (W.textLinks copyMetadata) && V.null (W.textShellBlocks copyMetadata) &&
      all (\w->maybe True (const False) (windowConversationControls restoredViews w)) (windows restoredViews))
  viewBytes<-BS.readFile viewsPath
  check "recovery discards actionable link and shell metadata"
    (not ("https://private.invalid" `BS.isInfixOf` viewBytes) && not ("private-shell" `BS.isInfixOf` viewBytes) && not ("old-hidden-provider-id" `BS.isInfixOf` viewBytes))
  viewJSON<-either error pure (eitherDecodeStrict' viewBytes)
  let field key=case viewJSON of Object fields->KM.lookup key fields; _->Nothing
  check "targets serialize one body each without synthetic documents or generic duplicates"
    (field "buffers"==Just (toJSON ([]::[Value])) && field "pluginWindows"==Just (toJSON ([]::[Value])) &&
      case field "conversationViews" of Just (Array entries)->V.length entries==2; _->False)
  check "hidden draft history and focus survive independently of active input"
    (not (editorDraftFocused primaryState) && editorDraftFocused childState &&
     contents (undo (editorDraftBuffer primaryState))=="" && contents (redo (editorDraftBuffer primaryState))=="redo primary draft" &&
     contents (undo (editorDraftBuffer childState))=="")
  check "recovery allocates fresh per-target draft identity without editor or frame bindings"
    (conversationDraftRef primaryView/=primaryRef && conversationDraftRef childView/=childRef &&
     conversationDraftRef primaryView/=conversationDraftRef childView &&
     all (\v->conversationEditor v==Nothing && conversationEditorFrame v==Nothing) (M.elems (conversationViews restoredViews)) &&
     all ((==Nothing).editorDraftMount) (M.elems (editorDrafts restoredViews)) &&
     all ((==Nothing).windowEditorMount) (windows restoredViews) && activeEditorMount restoredViews==Nothing)
  restoredAgain<-readCheckpoint viewsPath fresh >>= right
  check "another recovery lifetime cannot reuse a draft reference"
    (conversationDraftRef (conversationViews restoredAgain M.! "")/=conversationDraftRef primaryView)
  reopened<-showBody scope "" "Primary" (closeActive restoredChild)
  check "closing and reopening a conversation retains transcript with valid IDs" (activeText reopened==activeText restoredPrimary && all ((<nextId reopened).windowId) (windows reopened))
  writeCheckpoint viewsPath reopened >>= right
  check "recovered background drafts still require discard confirmation" (conversationHasDraft restoredViews && maybe False ((==DiscardDraft).purpose) (dialog (fst (runCommand Quit restoredViews))))
  BS.writeFile sourcePath "external disk edit"
  let baseline=fromJust (documentFile (buffers recovered M.! sourceId))
  check "source disk baseline survives independently of current disk" (diskBytes baseline==Just (bufferBytes original))
  conflict<-saveFile baseline (get recovered sourceId)
  disk<-BS.readFile sourcePath
  check "restored old baseline triggers existing save conflict checks" (case conflict of Left _->disk=="external disk edit"; _->False)
  encoded<-BS.readFile path
  check "recovery preserves per-window buffer view and divider"
    (all (\w -> bufferView w==SideBySideView && reviewSplit w==63) [w | w<-windows recovered,bufferId w==Just sourceId])
  check "checkpoint omits Session identifiers, private pending answers and approval tokens" (not ("old-provider-id" `BS.isInfixOf` encoded) && not ("private pending answer" `BS.isInfixOf` encoded) && not ("pending-action-token" `BS.isInfixOf` encoded) && not ("pending approval body" `BS.isInfixOf` encoded))
  missingDraft<-writeCheckpoint path desktop {editorDrafts=M.empty}
  check "a missing host-owned draft is rejected instead of discarded" (case missingDraft of Left _->True; _->False)
  invalidWrite<-writeCheckpoint path desktop {nextId=0}
  preserved<-BS.readFile path
  check "invalid write retains the previous complete checkpoint" (case invalidWrite of Left _->encoded==preserved; _->False)
  let mutate change=do
        value<-either error pure (eitherDecodeStrict' encoded)
        BL.writeFile path (encode (change value))
        result<-readCheckpoint path fresh
        check "malformed recovery state is rejected without leaking contents" (case result of Left err->T.length err<256 && not ("private draft" `T.isInfixOf` err); _->False)
      set key value (Object fields)=Object (KM.insert key value fields)
      set _ _ value=value
  mutate (set "conversationTarget" (toJSON ("unknown-agent"::T.Text)))
  mutate (set "schemaVersion" (toJSON (0::Int)))
  mutate (set "nextId" (toJSON (0::Int)))
  mutate (set "screen" (toJSON ((maxBound::Int),25::Int)))
  mutate (set "buffers" (toJSON ([]::[Value])))
  let alterFirst key change (Object fields)=case KM.lookup key fields of
        Just (Array entries) | not (V.null entries)->Object (KM.insert key (Array (entries V.// [(0,change (V.head entries))])) fields)
        _->Object fields
      alterFirst _ _ value=value
      alterFirstBody change (Object fields)=case KM.lookup "body" fields of
        Just body->Object (KM.insert "body" (change body) fields)
        _->Object fields
      alterFirstBody _ value=value
      duplicateBuffers (Object fields)=case KM.lookup "buffers" fields of
        Just (Array entries) | not (V.null entries)->Object (KM.insert "buffers" (Array (V.cons (V.head entries) entries)) fields)
        _->Object fields
      duplicateBuffers value=value
  mutate (alterFirst "conversationViews" (set "selection" (toJSON ((-1::Int),999999::Int))))
  mutate (alterFirst "conversationViews" (set "replySelection" (toJSON ((-1::Int),999999::Int))))
  mutate (alterFirst "conversationViews" (alterFirstBody (set "copy" (toJSON ("unknown"::T.Text)))))
  mutate (alterFirst "conversationViews" (alterFirstBody (set "messages" (toJSON [(-1::Int,2::Int,0::Int,True)]))))
  mutate duplicateBuffers
  mutate (alterFirst "windows" (set "bounds" (toJSON ((0::Int),(0::Int),(0::Int),(10::Int)))))
  mutate (alterFirst "windows" (set "selection" (toJSON ((-1::Int),(0::Int)))))
  mutate (alterFirst "windows" (set "sourceId" (toJSON (999999::Int))))
  BS.writeFile path "{not-json: secret}"
  corrupted<-readCheckpoint path fresh
  check "corrupt checkpoint returns an error" (case corrupted of Left _->True; _->False)
  switched<-right (toggleByteMode (newBuffer "λ中") >>= restoreBuffer . snapshotBuffer)
  check "mode-switch recovery keeps text saved baseline and reversible representation" (byteMode switched && not (savedByteMode switched) && contents (undo switched)=="λ中" && not (byteMode (undo switched)))
  let longPath=root </> "long-source.checkpoint"
      longText=T.replicate 400 "\t界e\x301"<>"\r\nlast"
      longDesktop=addDocument Nothing (newBuffer longText) (initialDesktop (80,25))
  writeCheckpoint longPath longDesktop >>= right
  longRecovered<-readCheckpoint longPath fresh >>= right
  let longWindow=fromJust (activeWindow longRecovered)
      longDocument=fromJust (activeDocument longRecovered)
      expectedWidth=displayColumn (lineAt longText 0) maxBound
      expectedLimit=max 0 (expectedWidth-(width (bounds longWindow)-2)+1)
      discovered=changeScroll False (8*T.length longText) longRecovered
      discoveredWindow=fromJust (activeWindow discovered)
      discoveredDocument=fromJust (activeDocument discovered)
  check "recovery preserves bytes and refines the estimated source extent on an end seek"
    (contents (documentBuffer longDocument)==longText &&
     scrollbarLimit longRecovered False longDocument longWindow>expectedLimit &&
     scrollColumn discoveredWindow==expectedLimit &&
     scrollbarLimit discovered False discoveredDocument discoveredWindow==expectedLimit)
  let badSnapshot=(snapshotBuffer edited) {snapshotByteMode=True,snapshotContents="中"}
  check "invalid byte representation cannot silently truncate on recovery" (case restoreBuffer badSnapshot of Left _->True; _->False)
#ifndef mingw32_HOST_OS
  let alias=root </> "checkpoint-alias"
  createFileLink path alias
  symbolic<-readCheckpoint alias fresh
  check "checkpoint reads refuse symlink endpoints" (case symbolic of Left _->True; _->False)
#endif
  putStrLn "recovery checks passed"

temporary :: IO FilePath
temporary=do
  parent<-getTemporaryDirectory
  (path,handle)<-openTempFile parent "thc-recovery-check"
  hClose handle
  removeFile path
  createDirectory path
  canonicalizePath path

-- Change detection must not inspect text, history or rendering caches.
keyChecks :: IO ()
keyChecks=W.withWindowScope $ \scope->do
  let original=replaceSelection (Selection 1 1) "x" (newBuffer "abc")
      desktop=addDocument (Just (FileState "/project/source.hs" (Just "abc"))) original (initialDesktop (80,25))
      update f d=d {buffers=M.map f (buffers d)}
      replace buffer=update (\doc->doc {documentBuffer=buffer}) desktop
      check label ok=unless ok (error label)
      changed label d=do a<-checkpointKey desktop; b<-checkpointKey d; check label (a/=b)
  initial<-checkpointKey desktop
  repeated<-checkpointKey desktop
  transient<-checkpointKey (update (\doc->doc {documentHighlight=error "render cache forced by checkpoint key",documentWidth=999})
    desktop {status="background status",agentReplying=True,clipboard="private transient",menu=Just (0,0)})
  check "unchanged checkpoint identity and transient UI changes stay cheap" (initial==repeated && initial==transient)
  changed "same-revision replacement changes checkpoint key" (replace (newBuffer "different") {revision=revision original})
  changed "markSaved changes checkpoint key without changing revision" (replace (markSaved original))
  changed "disk conflict baseline changes checkpoint key" (update (\doc->doc {documentFile=Just (FileState "/project/source.hs" (Just "new disk"))}) desktop)
  changed "saved source path changes checkpoint key" (update (\doc->doc {documentFile=Just (FileState "/project/renamed.hs" (Just "abc"))}) desktop)
  changed "window selection changes checkpoint key" (desktop {windows=map (\w->w {selection=Selection 1 2}) (windows desktop)})
  changed "recovered preferences change checkpoint key" (desktop {pixelateUnicode=not (pixelateUnicode desktop)})
  let poison=original {saved=error "saved contents forced",undoStack=error "undo history forced",redoStack=error "redo history forced"}
  lazyA<-checkpointKey (replace poison)
  lazyB<-checkpointKey (replace poison)
  check "checkpoint key never walks buffer snapshots" (lazyA==lazyB)
  ref<-E.newDraftRef
  prepared<-semanticBody "Primary" [(c,Plain) | c<-"Public prompt\nOther: private"] W.CopyText []
  maskedBody<-semanticBody "Primary" [(c,Plain) | c<-"Public prompt\nOther: private"] W.CopyText [(14,28)]
  let chat=installDraft "" "Primary" prepared ref (newBuffer "unsent draft") (Selection 0 0) True desktop
      masked=chat {conversationViews=M.adjust (\view->view {conversationBody=InertBody maskedBody}) "" (conversationViews chat)}
  chatKey<-checkpointKey chat
  wrappedBodyKey<-checkpointKey chat {conversationViews=M.map id (conversationViews chat),pluginWindows=M.map id (pluginWindows chat)}
  check "prepared body wrapper retains one cheap checkpoint identity" (chatKey==wrappedBodyKey)
  maskedKey<-checkpointKey masked
  opening<-W.openTextWindow scope maskedBody >>= maybe (fail "Key fixture scope ended") pure
  (reference,_)<-W.admitWindowUpdate False opening >>= maybe (fail "Key fixture admission failed") pure
  let controlled token=masked {pluginWindows=M.insert reference maskedBody (pluginWindows masked),
        conversationViews=M.adjust (\view->view {conversationBody=InstalledBody reference
          (Just (BodyControlReceipt maskedBody 80 False Nothing (HostBodyControls Nothing [(14,28,"question-input",[token])])))} "" (conversationViews masked)}
  tokenOnly<-checkpointKey (controlled "other-token")
  originalToken<-checkpointKey (controlled "ephemeral-token")
  remembered<-checkpointKey (rememberConversationView masked)
  check "fresh prepared privacy projection participates but host tokens do not" (chatKey/=maskedKey && maskedKey==tokenOnly && tokenOnly==originalToken)
  check "checkpoint key shares conversation-view normalization" (maskedKey==remembered)
  draftKey<-checkpointKey (setComposerInput (newBuffer "different draft") (Selection 0 0) True chat)
  check "unsent composer replacement changes checkpoint key" (chatKey/=draftKey)
  focusedKey<-checkpointKey (setComposerInput (composerBuffer chat) (Selection 0 0) False chat)
  selectedKey<-checkpointKey (setComposerInput (composerBuffer chat) (Selection 1 2) True chat)
  check "host-owned draft selection and focus participate in checkpoint invalidation" (chatKey/=focusedKey && chatKey/=selectedKey)
  let retained=(composerBuffer chat) {saved=error "draft saved contents forced",undoStack=error "draft undo history forced",redoStack=error "draft redo history forced"}
      cheap=setComposerInput retained (Selection 0 0) True chat
  cheapKey<-checkpointKey cheap
  bindingsKey<-checkpointKey cheap
    {conversationViews=M.map (\view->view {conversationEditor=error "editor authority forced",conversationEditorFrame=error "frame binding forced"}) (conversationViews cheap),
     editorDrafts=M.map (\state->state {editorDraftMount=error "draft mount forced"}) (editorDrafts cheap),
     windows=map (\w->w {windowEditorMount=error "window mount forced"}) (windows cheap)}
  check "checkpoint keys never force draft history or ephemeral editor bindings" (cheapKey==bindingsKey)

-- Public preparation/admission creates real snapshots and lifetime identities.
semanticBody :: T.Text -> [(Char,Style)] -> W.TextCopy -> [(Int,Int)] -> IO W.PreparedWindow
semanticBody title styled copy hidden=W.prepareSemanticTextWindow title styled
  (W.TextSemantics copy Nothing V.empty V.empty W.ReadableWindow V.empty V.empty (V.fromList hidden)) >>= either (fail . T.unpack) pure

installDraft :: T.Text -> T.Text -> W.PreparedWindow -> E.DraftRef -> Buffer -> Selection -> Bool -> Desktop -> Desktop
installDraft target name body ref buffer selected focused d=d
  {conversationViews=M.insert target (ConversationView (InertBody body) name ref Nothing Nothing (0,0) (Selection 0 0)) (conversationViews d),
   editorDrafts=M.insert ref (EditorDraft buffer selected focused Nothing) (editorDrafts d)}

showBody :: W.WindowScope -> T.Text -> T.Text -> Desktop -> IO Desktop
showBody scope target name original=case conversationBody view of
  InstalledBody _ _->pure (selectConversationView target name original)
  InertBody prepared->do
    opening<-W.openTextWindow scope prepared >>= maybe (fail "Recovery fixture scope ended") pure
    (reference,accepted)<-W.admitWindowUpdate False opening >>= maybe (fail "Recovery fixture admission failed") pure
    let attached=original {pluginWindows=M.insert reference accepted (pluginWindows original),
          conversationViews=M.insert target view {conversationBody=InstalledBody reference Nothing} (conversationViews original)}
        framed=if any (\w->conversationTargetFor original w/=Nothing) (windows original) then attached else addPluginWindow reference accepted attached
    pure (selectConversationView target name framed)
  where view=conversationViews original M.! target

bodyText :: T.Text -> Desktop -> T.Text
bodyText target d=case conversationBodySnapshot target d of
  Nothing->""
  Just prepared->let text=W.preparedWindowText prepared in contentSlice text 0 (contentLength text)

windowState :: Window -> (Int,Maybe Int,Rect,Selection,Int,Int,Maybe Rect,Int,BufferView,Int)
windowState w=(windowId w,bufferId w,bounds w,selection w,scrollRow w,scrollColumn w,restoredBounds w,windowNumber w,bufferView w,reviewSplit w)
