{-# LANGUAGE CPP, OverloadedStrings #-}
module RecoveryCheck (checks) where

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import System.Timeout (timeout)
import Hide.TextPresentation (withTextPresentation,textPresentationEffects,tickTextPresentation)
import Control.Monad (unless, forM_)
import Data.Char (chr, ord)
import Data.Word (Word64)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
import Hide.App (applyEffects)
import Hide.SidebarCommands (withSidebarCommands,sidebarEffects,awaitFileOpening,initializeSidebar)
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Editor as E
import qualified Hide.Plugin.Window as W
import Hide.Syntax (Style(..), StyledText, styledText)

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)

right :: Either T.Text a -> IO a
right=either (error . T.unpack) pure

-- Ordinary CLI directory opening must persist an absolute Files root, including
-- the README's relative "." argument. Recovery must retain that same directory.
relativeDirectoryChecks :: FilePath -> IO ()
relativeDirectoryChecks root=do
  let project=root </> "relative-project"
      checkpoint=root </> "relative-project.checkpoint"
      fresh=initialDesktop (80,25)
  createDirectory project
  writeFile (project </> "Main.hs") "main = pure ()\n"
  expected<-canonicalizePath project
  withCurrentDirectory project $ withSidebarCommands $ \host->do
    (_,opened)<-sidebarEffects host applyEffects fresh [OpenFile Menu.HumanMenu "."]
    prepared<-awaitFileOpening host opened >>= initializeSidebar host
    check "relative directory opening owns a canonical Files root"
      (fmap treeRoot (sideTree prepared)==Just expected)
    writeCheckpoint checkpoint prepared >>= right
    recovered<-readCheckpoint checkpoint fresh >>= right
    check "relative directory startup roundtrips through recovery"
      (fmap treeRoot (sideTree recovered)==Just expected)

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->W.withWindowScope $ \scope->do
  relativeDirectoryChecks root
  keyChecks root
  sharedTextChecks root
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
  primaryBody<-semanticBody "Primary" (styledText Plain transcript) W.CopyText
    [(0,T.length "Session: old-provider-id"),(T.length "Session: old-provider-id\nPublic transcript\n",T.length transcript)]
  primarySeed<-logicalFixture root "" "Primary" [replyRecord 0 "Agent" "Public transcript"] Null Null
  emptyBody<-W.prepareTextWindow "Primary" ""
  let unpublishedPath=root </> "unpublished-draft.checkpoint"
      unpublished=installDraft "" "Primary" primarySeed emptyBody primaryRef draft (Selection 1 5) True (initialDesktop (80,25))
  writeCheckpoint unpublishedPath unpublished {conversationViews=M.adjust (\view->view {conversationLogical=Nothing}) "" (conversationViews unpublished)} >>= right
  unpublishedDraft<-readCheckpoint unpublishedPath fresh >>= right
  check "a draft survives before the first asynchronous logical publication"
    (snapshotBuffer (composerBuffer unpublishedDraft)==snapshotBuffer draft && composerSelection unpublishedDraft==Selection 1 5 && bodyText "" unpublishedDraft=="")
  conversation<-showBody scope "" "Primary" (installDraft "" "Primary" primarySeed primaryBody primaryRef draft (Selection 1 5) True binary)
  let terminal=addReadOnly "Terminal 7" "last terminal output" conversation
      terminalId=sourceFixtureBuffer (fromJust (activeWindow terminal))
      approval=addReadOnly "Agent request" "pending approval body" terminal
      approvalId=sourceFixtureBuffer (fromJust (activeWindow approval))
      desktop=approval {defaultDirectory=Just root,
        sideTree=Just ((emptySidebar root 23 True) {treeHints=Just (SidebarHints (M.singleton sourcePath False) (Just sourcePath) (Just sourcePath))}),problemsVisible=True,problemsPreferredHeight=9,
        wordStar=True,wideSectionTitles=True,hapticFeedback=True,blinkCursor=False,pixelateUnicode=True,materialIcons=True,appearance=DarkMode,streamerMode=True,chatSubmit=SteerSubmit,
        dialog=Just (Dialog "Pending permission" (PermissionDialog "approve:secret") [] 0 ["Allow"] []),
        menu=Just (0,0),drag=Just (Selecting sourceId),clipboard="transient clipboard",clipboardCode=Just "transient clipboard",clipboardExport=(3,Just "export"),
        agentReplying=True,agentQueued=3,agentSettings=[AgentSetting "token" "Token" "private" "secret" []],
        agentContextUsage=Just (1,2),diagnostics=[Diagnostic sourcePath Nothing 0 0 1 "old diagnostic"]}
      get d ident=documentBuffer (buffers d M.! ident)
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
  check "split windows retain shared buffer IDs geometry and selection" (map windowState (filter ((/=Nothing).bufferId) (windows recovered))==map windowState (filter (\w->bufferId w/=Nothing && bufferId w/=Just approvalId) (windows desktop)))
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
     wordStar recovered && hapticFeedback recovered && not (blinkCursor recovered) && pixelateUnicode recovered && materialIcons recovered && appearance recovered==DarkMode && streamerMode recovered && chatSubmit recovered==SteerSubmit)
  let pinnedPath=root </> "pinned.checkpoint"
      terminalWindowId=maybe (error "missing terminal") windowId (activeWindow terminal)
      pinned=setTerminalPinned True terminalWindowId terminal
  writeCheckpoint pinnedPath pinned >>= right
  recoveredPinned<-readCheckpoint pinnedPath fresh >>= right
  check "recovered terminal tabs preserve identity and layout without restarting or routing input"
    (dockedTerminals recoveredPinned==dockedTerminals pinned && bottomTerminal recoveredPinned==Just terminalWindowId &&
     bounds (fromJust (activeWindow recoveredPinned))==problemsRect recoveredPinned &&
     documentLabel (buffers recoveredPinned M.! terminalId)==Just "Ended Terminal 7" && activeTerminal recoveredPinned==Nothing)
  importedSources<-readSourceCheckpoint pinnedPath >>= right
  let destinationBuffer=replaceSelection (Selection 0 0) "destination edit " original
      destinationFile=FileState sourcePath (Just (bufferBytes original))
      destination=addDocument (Just destinationFile) destinationBuffer unpublished
        {screenSize=(64,18),guestPrivatePaths=[sourcePath],wordStar=True,
         terminalMouseTracking=S.singleton 9000,agentSettings=[AgentSetting "model" "Model" "model" "current" []]}
      destinationId=sourceFixtureBuffer (fromJust (activeWindow destination))
      merged=adoptRecoveredSources importedSources destination
      importedDocuments=M.withoutKeys (buffers merged) (M.keysSet (buffers destination))
      copies=[(bid,doc) | (bid,doc)<-M.toList importedDocuments,documentSuggestedName doc==Just "source.hs"]
      (copyId,copyDoc)=case copies of [value]->value; _->error "missing distinct recovered source copy"
      copyViews=filter ((==Just copyId).bufferId) (windows merged)
      importedWindows=filter ((>=nextId destination).windowId) (windows merged)
      binaryCopies=[documentBuffer doc | doc<-M.elems importedDocuments,byteMode (documentBuffer doc)]
      terminalCopies=[w | w<-importedWindows,terminalWindow merged w]
  check "source import never overwrites the destination's dirty file or baseline"
    (snapshotBuffer (get merged destinationId)==snapshotBuffer destinationBuffer &&
     documentFile (buffers merged M.! destinationId)==Just destinationFile)
  check "duplicate source import preserves both history stacks and its original baseline"
    (snapshotBuffer (documentBuffer copyDoc)==snapshotBuffer edited &&
     snapshotBuffer (undo (documentBuffer copyDoc))==snapshotBuffer (undo edited) &&
     snapshotBuffer (redo (documentBuffer copyDoc))==snapshotBuffer (redo edited))
  check "duplicate import keeps privacy provenance and requests Save As"
    (documentFile copyDoc==Nothing && documentOrigin copyDoc==Just sourcePath && privateDocument merged copyDoc &&
     case dialog (fst (runCommand Save (focusWindow (windowId (case copyViews of w:_->w; _->error "missing split") ) merged))) of
       Just (Dialog _ (Saving bid Nothing) _ _ _ _)->bid==copyId
       _->False)
  check "imported text splits share one fresh document and retain source view state"
    (length copyViews==2 && all (\w->bufferView w==SideBySideView && reviewSplit w==63) copyViews &&
     length importedWindows==4 && M.size importedDocuments==3 &&
     all (\w->windowId w>=nextId destination && windowId w<nextId merged) importedWindows &&
     S.size (S.fromList (map windowNumber (windows merged)))==length (windows merged))
  check "source import keeps binary history and ended terminal views without live input"
    (case binaryCopies of [buffer]->snapshotBuffer buffer==snapshotBuffer hex; _->False)
  check "ended source terminals float with clamped saved bounds without changing destination dock"
    (case terminalCopies of
       [window]->not (windowPinned merged window) && bounds window==fitWindow destination (bounds (fromJust (activeWindow terminal))) &&
         activeTerminal (focusWindow (windowId window) merged)==Nothing
       _->False)
  check "source import retains destination runtime owners and preferences without importing conversation windows"
    (M.keys (conversationViews merged)==M.keys (conversationViews destination) &&
     M.keys (editorDrafts merged)==M.keys (editorDrafts destination) && M.keys (pluginWindows merged)==M.keys (pluginWindows destination) &&
     snapshotBuffer (composerBuffer merged)==snapshotBuffer (composerBuffer destination) &&
     terminalMouseTracking merged==terminalMouseTracking destination && dockedTerminals merged==dockedTerminals destination &&
     agentSettings merged==agentSettings destination && wordStar merged && screenSize merged==screenSize destination &&
     all ((/=Nothing).bufferId) importedWindows)
  let sameFrames a b=map windowState (windows a)==map windowState (windows b)
      full=destination {buffers=M.fromList [(i,buffers destination M.! destinationId) | i<-[1..2048]],nextId=2049}
      fullResult=adoptRecoveredSources importedSources full
      fullViews=destination {windows=[(fromJust (activeWindow destination)) {windowId=i,windowNumber=i} | i<-[1..4096]],nextId=4097}
      fullViewsResult=adoptRecoveredSources importedSources fullViews
      exhausted=destination {nextId=1073741823}
      exhaustedResult=adoptRecoveredSources importedSources exhausted
  check "source import refuses count and identity overflow before adding any windows"
    (M.keys (buffers fullResult)==M.keys (buffers full) && sameFrames fullResult full &&
     M.keys (buffers fullViewsResult)==M.keys (buffers fullViews) && sameFrames fullViewsResult fullViews &&
     nextId exhaustedResult==nextId exhausted && sameFrames exhaustedResult exhausted)
  let cleanPath=root </> "clean-copy.checkpoint"
      cleanSource=addDocument (Just destinationFile) original fresh
  writeCheckpoint cleanPath cleanSource >>= right
  cleanPrepared<-readSourceCheckpoint cleanPath >>= right
  let cleanMerged=adoptRecoveredSources cleanPrepared destination
      cleanCopy=fromJust (activeDocument cleanMerged)
  check "a clean duplicate requires Save As without falsifying its saved baseline"
    (documentFile cleanCopy==Nothing && not (dirty (documentBuffer cleanCopy)) && snapshotBuffer (documentBuffer cleanCopy)==snapshotBuffer original)
  let generatedPath=root </> "generated-copy.checkpoint"
      generated=cleanSource {buffers=M.map (\doc->doc {documentOrigin=Just (root </> "private-origin")}) (buffers cleanSource)}
  writeCheckpoint generatedPath generated >>= right
  generatedPrepared<-readSourceCheckpoint generatedPath >>= right
  let refused=adoptRecoveredSources generatedPrepared destination
  check "a duplicate with two privacy origins refuses the entire import"
    (M.keys (buffers refused)==M.keys (buffers destination) && sameFrames refused destination &&
     snapshotBuffer (get refused destinationId)==snapshotBuffer destinationBuffer && nextId refused==nextId destination)
#ifndef mingw32_HOST_OS
  let movedPath=root </> "moved-source"
      movedCheckpoint=root </> "moved-source.checkpoint"
  BS.writeFile movedPath (bufferBytes original)
  writeCheckpoint movedCheckpoint (addDocument (Just (FileState movedPath (Just (bufferBytes original)))) original fresh) >>= right
  removeFile movedPath
  createFileLink sourcePath movedPath
  moved<-readSourceCheckpoint movedCheckpoint
  check "source import refuses changed symlink authority rather than granting a new save target" (case moved of Left _->True; _->False)
#endif
  let viewsPath=root </> "views.checkpoint"
      hiddenSession="Session: old-hidden-provider-id\n"
      primaryRuns=[(hiddenSession,Plain),("hi\nx",BubbleText 0 True (LinkStyle "https://private.invalid" Plain)),
        ("..",Plain),("ok",BubbleText 1 False Plain)]
      primaryDraft=undo (replaceSelection (Selection 0 0) "redo " (replaceSelection (Selection 0 0) "primary draft" (newBuffer "")))
      childDraft=replaceSelection (Selection 0 0) "child draft" (newBuffer "")
      primaryRecords=[replyRecord 0 "You" "hi  \nx",replyRecord 1 "Agent" "ok",
        replyRecord 2 "Agent" (T.replicate 12 "original Markdown with `inline code` ")]++
        [replyRecord n "Agent" ("primary transcript "<>T.pack (show n)) | n<-[3..81]]
      childRecords=pauseRecord (-1) "Child status: idle":
        [replyRecord n "Agent" ("child transcript "<>T.pack (show n)) | n<-[0..79]]
      primaryAnchor=toJSON (0::Int,0::Int,0::Int)
      primaryReply=toJSON (toJSON (0::Int,0::Int,0::Int),toJSON (1::Int,0::Int,2::Int))
  primaryLogicalSeed<-logicalFixture root "" "Primary" primaryRecords primaryAnchor primaryReply
  let primaryLogical=primaryLogicalSeed {conversationViews=M.adjust (\view->view
        {conversationRowShift=7,conversationScrollColumn=3}) "" (conversationViews primaryLogicalSeed)}
  childLogical<-logicalFixture root "agent-2" "Child" childRecords Null Null
  primaryPrepared<-W.prepareSemanticTextWindow "Primary" primaryRuns
    (W.TextSemantics (W.CopyMessages W.UserBotAttribution) (Just root)
      (V.singleton (0,2,"https://private.invalid")) (V.singleton (0,2,"sh","printf private-shell"))
      W.ReadableWindow V.empty V.empty (V.singleton (0,T.length hiddenSession))) >>= right
  childPrepared<-semanticBody "Child" (styledText Plain "current child viewport") W.CopyText []
  initialPrimary<-showBody scope "" "Primary" (installDraft "" "Primary" primaryLogical primaryPrepared primaryRef primaryDraft (Selection 2 5) False (initialDesktop (80,25)))
  let primaryChat=setComposerInput (composerBuffer initialPrimary) (Selection 2 5) False initialPrimary
  childChat<-showBody scope "agent-2" "Child" (installDraft "agent-2" "Child" childLogical childPrepared childRef childDraft (Selection 1 4) True (closeActive (rememberConversationView primaryChat)))
  writeCheckpoint viewsPath childChat >>= right
  restoredViews<-readCheckpoint viewsPath fresh >>= right
  let primaryView=conversationViews restoredViews M.! ""
      childView=conversationViews restoredViews M.! "agent-2"
      primaryState=editorDrafts restoredViews M.! conversationDraftRef primaryView
      childState=editorDrafts restoredViews M.! conversationDraftRef childView
  check "hidden target retains logical ownership without laying out its history"
    (conversationBodyRef primaryView==Nothing && bodyText "" restoredViews=="" &&
      M.size (pluginWindows restoredViews)==1 &&
      all (\w->conversationTargetFor restoredViews w==Just "agent-2") (windows restoredViews))
  let hiddenBody=fromJust (conversationBodySnapshot "" restoredViews)
      hiddenMetadata=fromJust (W.preparedWindowSemantics hiddenBody)
  check "hidden recovery placeholder admits the readable passive transcript policy"
    (W.preparedWindowDisclosure hiddenBody==W.ReadableWindow &&
     V.null (W.textLinks hiddenMetadata) && V.null (W.textShellBlocks hiddenMetadata) &&
     W.textLinkBase hiddenMetadata==Nothing)
  let closedRecovered=closeActive restoredViews
  check "closing a recovered frame retains its catalogue without editor bindings"
    (null (windows closedRecovered) && M.null (pluginWindows closedRecovered) &&
      conversationBodyRef (conversationViews closedRecovered M.! "agent-2")==Nothing &&
      maybe False (const True) (conversationLogical (conversationViews closedRecovered M.! "agent-2")))
  -- Install the retained hidden frame through public admission, then acquire its
  -- real bounded projection through the same recovery worker used at restart.
  reopened<-showBody scope "" "Primary" closedRecovered
  writeCheckpoint viewsPath reopened >>= right
  restoredPrimary<-readCheckpoint viewsPath fresh >>= right
  let recoveredBody=fromJust (conversationBodySnapshot "" restoredPrimary)
      copyMetadata=fromJust (W.preparedWindowSemantics recoveredBody)
      restoredView=conversationViews restoredPrimary M.! ""
      expectedView=conversationViews primaryLogical M.! ""
  check "logical anchor and selection survive separately from viewport rows"
    (conversationAnchor restoredView==conversationAnchor expectedView &&
     conversationReplySelection restoredView==conversationReplySelection expectedView &&
     conversationRowShift restoredView==0 && conversationScrollColumn restoredView==3 &&
     contents (composerBuffer restoredPrimary)=="primary draft" && composerSelection restoredPrimary==Selection 2 5)
  check "recovered visible text is a bounded passive projection of original Markdown"
    ("hi" `T.isInfixOf` activeText restoredPrimary && "ok" `T.isInfixOf` activeText restoredPrimary &&
     not ("primary transcript 81" `T.isInfixOf` activeText restoredPrimary) &&
     V.null (W.textLinks copyMetadata) && V.null (W.textShellBlocks copyMetadata) &&
     all (\w->maybe True (const False) (windowConversationControls restoredPrimary w)) (windows restoredPrimary))
  allCopied<-withTextPresentation $ \presentation->do
    let (copying,effects)=runCommand Copy restoredPrimary
        awaitCopy current=do
          (next,_)<-tickTextPresentation presentation [] current
          if fst (clipboardExport next)==fst (clipboardExport copying) && snd (clipboardExport next)/=Nothing
            then pure (clipboard next) else threadDelay 10000 >> awaitCopy next
    (_,queued)<-textPresentationEffects presentation (\current _->pure (False,current)) copying effects
    timeout 8000000 (awaitCopy queued) >>= maybe (error "Recovered conversation copy timed out") pure
  check "passive copy retains hard breaks and outgoing attribution"
    ("User: hi\nx" `T.isInfixOf` allCopied && "Bot: ok" `T.isInfixOf` allCopied)
  viewBytes<-BS.readFile viewsPath
  check "recovery discards provider headers and actionable viewport metadata"
    (not ("https://private.invalid" `BS.isInfixOf` viewBytes) && not ("private-shell" `BS.isInfixOf` viewBytes) && not ("old-hidden-provider-id" `BS.isInfixOf` viewBytes))
  viewJSON<-either error pure (eitherDecodeStrict' viewBytes)
  let field key=case viewJSON of Object fields->KM.lookup key fields; _->Nothing
      catalogueCounts=case field "conversationViews" of
        Just (Array entries)->[V.length items | Object view<-V.toList entries,Just (Object body)<-[KM.lookup "body" view],Just (Array items)<-[KM.lookup "items" body]]
        _->[]
  check "targets serialize complete logical catalogues once without synthetic documents"
    (field "buffers"==Just (toJSON ([]::[Value])) && field "pluginWindows"==Just (toJSON ([]::[Value])) &&
     catalogueCounts==[82,81] && "primary transcript 81" `BS.isInfixOf` viewBytes && "child transcript 79" `BS.isInfixOf` viewBytes)
  check "hidden draft history and focus survive independently of active input"
    (not (editorDraftFocused primaryState) && editorDraftFocused childState &&
     contents (undo (editorDraftBuffer primaryState))=="" && contents (redo (editorDraftBuffer primaryState))=="redo primary draft" &&
     contents (undo (editorDraftBuffer childState))=="")
  check "recovery allocates fresh per-target draft identity without editor bindings"
    (conversationDraftRef primaryView/=primaryRef && conversationDraftRef childView/=childRef &&
     conversationDraftRef primaryView/=conversationDraftRef childView &&
     all (\v->conversationEditor v==Nothing && conversationEditorFrame v==Nothing && conversationCaretIntent v==Nothing) (M.elems (conversationViews restoredViews)) &&
     all ((==Nothing).editorDraftMount) (M.elems (editorDrafts restoredViews)) &&
     all ((==Nothing).windowEditorMount) (windows restoredViews) && activeEditorMount restoredViews==Nothing)
  restoredAgain<-readCheckpoint viewsPath fresh >>= right
  check "another recovery lifetime cannot reuse a draft reference"
    (conversationDraftRef (conversationViews restoredAgain M.! "")/=conversationDraftRef primaryView)
  let wider=restoredPrimary {screenSize=(120,35),windows=map (\w->w {bounds=Rect 0 0 120 35}) (windows restoredPrimary)}
  writeCheckpoint viewsPath wider >>= right
  resized<-readCheckpoint viewsPath fresh >>= right
  check "restart at another width preserves logical coordinates and original sources"
    (conversationAnchor (conversationViews resized M.! "")==conversationAnchor restoredView &&
     conversationReplySelection (conversationViews resized M.! "")==conversationReplySelection restoredView &&
     "hi" `T.isInfixOf` activeText resized && activeText resized/=activeText restoredPrimary)
  beforeInvalidPoint<-BS.readFile viewsPath
  let invalidAnchor=case conversationAnchor expectedView of
        At (BodyPoint ident _ _)->At (BodyPoint ident 99999 0)
        _->error "Expected fixture anchor"
  invalidPoint<-writeCheckpoint viewsPath resized {conversationViews=M.adjust (\view->view {conversationAnchor=invalidAnchor}) "" (conversationViews resized)}
  afterInvalidPoint<-BS.readFile viewsPath
  check "invalid canonical coordinates cannot replace a complete checkpoint"
    (case invalidPoint of Left _->beforeInvalidPoint==afterInvalidPoint; _->False)
  let questionPath=root </> "transient-question.checkpoint"
      questionPoint=QuestionPoint 17 0 0
  writeCheckpoint questionPath resized {conversationViews=M.map (\view->view
    {conversationAnchor=At questionPoint,conversationReplySelection=Just (BodySelection questionPoint questionPoint)}) (conversationViews resized)} >>= right
  withoutQuestion<-readCheckpoint questionPath fresh >>= right
  check "transient question coordinates restore as a cleared passive selection"
    (all (\view->conversationAnchor view==FollowEnd && conversationReplySelection view==Nothing)
      (M.elems (conversationViews withoutQuestion)))
  let pendingPath=root </> "pending-scroll.checkpoint"
      longReply="first paragraph\n\n"<>T.replicate 1000 "A long original reply with inline `code`.\n\n"<>"last paragraph"
  forM_ [0,500,1000::Int] $ \fraction->do
    pending<-logicalFixture root "" "Primary" [replyRecord 0 "Agent" longReply]
      (object ["withinItem" .= (0::Int),"fraction" .= fraction]) Null
    writeCheckpoint pendingPath pending >>= right
    restoredPending<-readCheckpoint pendingPath fresh >>= right
    check "a hidden item-relative scrollbar intent survives without history layout"
      (conversationAnchor (conversationViews restoredPending M.! "")==conversationAnchor (conversationViews pending M.! "") &&
       bodyText "" restoredPending=="" && null (windows restoredPending))
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
  invalidSource<-readSourceCheckpoint path
  check "source-only import retains checkpoint identity validation" (case invalidSource of Left _->True; _->False)
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
  mutate (alterFirst "conversationViews" (set "anchor" (toJSON (99999::Int,0::Int,0::Int))))
  mutate (alterFirst "conversationViews" (set "anchor" (toJSON (0::Int,99999::Int,0::Int))))
  mutate (alterFirst "conversationViews" (set "anchor" (toJSON (0::Int,0::Int,99999::Int))))
  mutate (alterFirst "conversationViews" (set "anchor" (object ["withinItem" .= (99999::Int),"fraction" .= (500::Int)])))
  mutate (alterFirst "conversationViews" (set "anchor" (object ["withinItem" .= (0::Int),"fraction" .= (-1::Int)])))
  mutate (alterFirst "conversationViews" (set "anchor" (object ["withinItem" .= (0::Int),"fraction" .= (1001::Int)])))
  mutate (alterFirst "conversationViews" (alterFirstBody (set "items" (toJSON [replyRecord 0 "Agent" "first",replyRecord 0 "Agent" "duplicate"]))))
  mutate (alterFirst "conversationViews" (alterFirstBody (set "items" (toJSON [pauseRecord (-1) "invalid primary metadata"]))))
  mutate (alterFirst "conversationViews" (alterFirstBody (alterFirst "items" (set "revision" (toJSON (-1::Int))))))
  mutate duplicateBuffers
  mutate (alterFirst "windows" (set "bounds" (toJSON ((0::Int),(0::Int),(0::Int),(10::Int)))))
  mutate (alterFirst "windows" (set "selection" (toJSON ((-1::Int),(0::Int)))))
  mutate (alterFirst "windows" (set "sourceId" (toJSON (999999::Int))))
  let alterBuffer change (Object fields)=case KM.lookup "buffer" fields of
        Just buffer->Object (KM.insert "buffer" (change buffer) fields)
        _->Object fields
      alterBuffer _ value=value
  mutate (set "strings" (toJSON ([]::[T.Text])))
  mutate (alterFirst "buffers" (alterBuffer (set "current" (toJSON [(0::Int,[-1::Int])]))))
  mutate (alterFirst "buffers" (alterBuffer (set "current" (toJSON [(0::Int,[maxBound::Int])]))))
  mutate (alterFirst "buffers" (alterBuffer (set "current" (toJSON [(7::Int,[0::Int])]))))
  BS.writeFile path "{not-json: secret}"
  corrupted<-readCheckpoint path fresh
  check "corrupt checkpoint returns an error" (case corrupted of Left _->True; _->False)
  switched<-right (toggleByteMode (newBuffer "λ中") >>= restoreBufferStorage . fmap snd . snapshotBufferStorage)
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
  let badStorage=(fmap snd (snapshotBufferStorage (newBuffer "中"))) {storageByteMode=True}
  check "invalid byte representation cannot silently truncate on recovery" (case restoreBufferStorage badStorage of Left _->True; _->False)
#ifndef mingw32_HOST_OS
  let alias=root </> "checkpoint-alias"
  createFileLink path alias
  symbolic<-readCheckpoint alias fresh
  check "checkpoint reads refuse symlink endpoints" (case symbolic of Left _->True; _->False)
  symbolicSource<-readSourceCheckpoint alias
  check "source-only import also refuses symlink endpoints" (case symbolicSource of Left _->True; _->False)
#endif
  putStrLn "recovery checks passed"

sharedTextChecks :: FilePath -> IO ()
sharedTextChecks root=do
  -- These distinct scalar sequences collide under the production polynomial
  -- fingerprint modulo 2^64. The table must resolve the bucket by exact text.
  let first=T.replicate 8 (T.singleton (chr 1000))
      second=T.pack (map (chr . (1000+)) [14,32,-86,-157,-11,-121,43,62])
      fingerprint=T.foldl' (\h c->h*16777619+fromIntegral (ord c)+1) (0::Word64)
      desktop=addDocument Nothing (newBuffer second) (addDocument Nothing (newBuffer first) (initialDesktop (80,25)))
      path=root </> "shared-text.checkpoint"
  check "collision fixture uses distinct raw text in one hash bucket" (first/=second && fingerprint first==fingerprint second)
  writeCheckpoint path desktop >>= right
  recovered<-readCheckpoint path (initialDesktop (80,25)) >>= right
  check "hash collisions preserve both exact string payloads"
    (map (contents . documentBuffer) (M.elems (buffers recovered))==[first,second])
  -- A file larger than three MiB with 100 localized edits used to retain over
  -- 300 MiB of flattened states. Repeated raw lines now have one shared payload.
  let line=T.replicate 1023 "a"<>"\n"
      source=T.replicate 3073 line
      edited=foldl (\b n->replaceSelection (Selection 5 6) (if even n then "x" else "y") b) (newBuffer source) [1..100::Int]
      large=addDocument Nothing edited (initialDesktop (80,25))
      largePath=root </> "large-history.checkpoint"
  writeCheckpoint largePath large >>= right
  encoded<-BS.readFile largePath
  restored<-readCheckpoint largePath (initialDesktop (80,25)) >>= right
  let buffer=documentBuffer (snd (M.findMin (buffers restored)))
  check "mostly unchanged histories share checkpoint text rather than snapshots" (BS.length encoded<T.length source && length (undoStack buffer)==100)
  forM_ [0,1,50,100] $ \steps->do
    let actual=iterate undo buffer!!steps
        expected=iterate undo edited!!steps
    check "shared history retains exact bytes and saved-line changes" (bufferBytes actual==bufferBytes expected && bufferLineChanges actual==bufferLineChanges expected)
  let oldest=iterate undo buffer!!100
  check "shared history replays every retained edit" (bufferBytes (iterate redo oldest!!100)==bufferBytes edited)

temporary :: IO FilePath
temporary=do
  parent<-getTemporaryDirectory
  (path,handle)<-openTempFile parent "thc-recovery-check"
  hClose handle
  removeFile path
  createDirectory path
  canonicalizePath path

-- Change detection must not inspect text, history or rendering caches.
keyChecks :: FilePath -> IO ()
keyChecks root=W.withWindowScope $ \scope->do
  let original=replaceSelection (Selection 1 1) "x" (newBuffer "abc")
      desktop=addDocument (Just (FileState "/project/source.hs" (Just "abc"))) original (initialDesktop (80,25))
      update f d=d {buffers=M.map f (buffers d)}
      replace buffer=update (\doc->doc {documentBuffer=buffer}) desktop
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
  changed "haptic preference changes checkpoint key" (desktop {hapticFeedback=not (hapticFeedback desktop)})
  let poison=original {saved=error "saved contents forced",undoStack=error "undo history forced",redoStack=error "redo history forced"}
  lazyA<-checkpointKey (replace poison)
  lazyB<-checkpointKey (replace poison)
  check "checkpoint key never walks buffer snapshots" (lazyA==lazyB)
  ref<-E.newDraftRef
  prepared<-semanticBody "Primary" (styledText Plain "Public prompt\nOther: private") W.CopyText []
  maskedBody<-semanticBody "Primary" (styledText Plain "Public prompt\nOther: private") W.CopyText [(14,28)]
  seed<-logicalFixture root "" "Primary" [replyRecord 0 "Agent" "Public prompt"] Null Null
  replacementSeed<-logicalFixture root "" "Primary" [replyRecord 0 "Agent" "Public prompt"] Null Null
  let chat=installDraft "" "Primary" seed prepared ref (newBuffer "unsent draft") (Selection 0 0) True desktop
      masked=chat {conversationViews=M.adjust (\view->view {conversationBody=InertBody maskedBody}) "" (conversationViews chat)}
  chatKey<-checkpointKey chat
  wrappedBodyKey<-checkpointKey chat {conversationViews=M.map id (conversationViews chat),pluginWindows=M.map id (pluginWindows chat)}
  check "logical body wrapper retains one cheap checkpoint identity" (chatKey==wrappedBodyKey)
  maskedKey<-checkpointKey masked
  opening<-W.openWindow scope maskedBody >>= maybe (fail "Key fixture scope ended") pure
  (reference,_)<-W.admitWindowUpdate False opening >>= maybe (fail "Key fixture admission failed") pure
  let receipt token=BodyControlReceipt maskedBody 80 False Nothing
        (HostBodyControls Nothing Nothing Nothing [(14,28,"question-input",[token])])
      controlled token=masked {pluginWindows=M.insert reference maskedBody (pluginWindows masked),
        conversationViews=M.adjust (\view->view {conversationBody=InstalledBody reference (Just (receipt token))})
          "" (conversationViews masked)}
  tokenOnly<-checkpointKey (controlled "other-token")
  originalToken<-checkpointKey (controlled "ephemeral-token")
  remembered<-checkpointKey (rememberConversationView masked)
  replacedKey<-checkpointKey chat {conversationViews=M.adjust (\view->view {conversationLogical=conversationLogical (conversationViews replacementSeed M.! "")}) "" (conversationViews chat)}
  check "logical replacement changes identity while viewport masks and host tokens do not" (chatKey/=replacedKey && chatKey==maskedKey && maskedKey==tokenOnly && tokenOnly==originalToken)
  check "checkpoint key shares conversation-view normalization" (maskedKey==remembered)
  draftKey<-checkpointKey (setComposerInput (newBuffer "different draft") (Selection 0 0) True chat)
  check "unsent composer replacement changes checkpoint key" (chatKey/=draftKey)
  let unpublished=chat {conversationViews=M.adjust (\view->view {conversationLogical=Nothing}) "" (conversationViews chat)}
  unpublishedKey<-checkpointKey unpublished
  unpublishedEditKey<-checkpointKey (setComposerInput (newBuffer "new chat draft") (Selection 0 0) True unpublished)
  check "draft edits invalidate checkpoints before logical publication" (unpublishedKey/=unpublishedEditKey)
  focusedKey<-checkpointKey (setComposerInput (composerBuffer chat) (Selection 0 0) False chat)
  selectedKey<-checkpointKey (setComposerInput (composerBuffer chat) (Selection 1 2) True chat)
  check "host-owned draft selection and focus participate in checkpoint invalidation" (chatKey/=focusedKey && chatKey/=selectedKey)
  let retained=(composerBuffer chat) {saved=error "draft saved contents forced",undoStack=error "draft undo history forced",redoStack=error "draft redo history forced"}
      cheap=setComposerInput retained (Selection 0 0) True chat
  cheapKey<-checkpointKey cheap
  bindingsKey<-checkpointKey cheap
    {conversationViews=M.map (\view->view {conversationEditor=error "editor authority forced",conversationEditorFrame=error "frame binding forced",conversationCaretIntent=error "pending caret intent forced"}) (conversationViews cheap),
     editorDrafts=M.map (\state->state {editorDraftMount=error "draft mount forced"}) (editorDrafts cheap),
     windows=map (\w->w {windowEditorMount=error "window mount forced"}) (windows cheap)}
  check "checkpoint keys never force draft history or ephemeral editor bindings" (cheapKey==bindingsKey)

-- Public preparation/admission creates real snapshots and lifetime identities.
semanticBody :: T.Text -> StyledText -> W.TextCopy -> [(Int,Int)] -> IO W.PreparedWindow
semanticBody title styled copy hidden=W.prepareSemanticTextWindow title styled
  (W.TextSemantics copy Nothing V.empty V.empty W.ReadableWindow V.empty V.empty (V.fromList hidden)) >>= either (fail . T.unpack) pure

installDraft :: T.Text -> T.Text -> Desktop -> W.PreparedWindow -> E.DraftRef -> Buffer -> Selection -> Bool -> Desktop -> Desktop
installDraft target name seed body ref buffer selected focused d=d
  {conversationViews=M.insert target view {conversationBody=InertBody body,conversationName=name,conversationDraftRef=ref,
      conversationEditor=Nothing,conversationEditorFrame=Nothing} (conversationViews d),
   editorDrafts=M.insert ref (EditorDraft buffer selected focused Nothing) (editorDrafts d)}
  where view=conversationViews seed M.! target

-- These inputs are independently written original sources, not render output or
-- private runtime constructors. readCheckpoint mints the actual logical owner.
replyRecord :: Int -> T.Text -> T.Text -> Value
replyRecord ident role markdown=object ["id" .= ident,"revision" .= (0::Int),
  "content" .= object ["kind" .= ("reply"::T.Text),"role" .= role,"markdown" .= markdown]]
pauseRecord :: Int -> T.Text -> Value
pauseRecord ident text=object ["id" .= ident,"revision" .= (0::Int),
  "content" .= object ["kind" .= ("pause"::T.Text),"text" .= text]]

logicalFixture :: FilePath -> T.Text -> T.Text -> [Value] -> Value -> Value -> IO Desktop
logicalFixture root target name records anchored selected=do
  let path=root </> "logical-input.checkpoint"
      baseline=initialDesktop (80,25)
  writeCheckpoint path baseline >>= right
  encoded<-BS.readFile path
  base<-either error pure (eitherDecodeStrict' encoded)
  let emptyBuffer=object ["current" .= [(0::Int,[0::Int])],"saved" .= [(0::Int,[0::Int])],
        "undo" .= ([]::[Value]),"redo" .= ([]::[Value]),"revision" .= (0::Int),
        "lastChange" .= Null,"byteMode" .= False,"savedByteMode" .= False]
      view=object ["target" .= target,"name" .= name,"body" .= object ["items" .= records],
        "draft" .= emptyBuffer,"selection" .= ((0::Int),(0::Int)),"focused" .= True,
        "anchor" .= anchored,"column" .= (0::Int),"replySelection" .= selected]
      set key value (Object fields)=Object (KM.insert key value fields)
      set _ _ value=value
      input=set "schemaVersion" (toJSON (5::Int)) . set "strings" (toJSON [""::T.Text]) .
        set "buffers" (toJSON ([]::[Value])) .
        set "conversationTarget" (toJSON target) . set "conversationViews" (toJSON [view]) $ base
  BL.writeFile path (encode input)
  readCheckpoint path baseline >>= right

showBody :: W.WindowScope -> T.Text -> T.Text -> Desktop -> IO Desktop
showBody scope target name original=case conversationBody view of
  InstalledBody _ _->pure (selectConversationView target name original)
  InertBody prepared->do
    opening<-W.openWindow scope prepared >>= maybe (fail "Recovery fixture scope ended") pure
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
