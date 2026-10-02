{-# LANGUAGE CPP, OverloadedStrings #-}
module RecoveryCheck (checks) where

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
import THC.Edit.Buffer
import THC.Edit.Files
import THC.Edit.BufferView
import THC.Edit.Model
import THC.Edit.Recovery

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->do
  keyChecks
  let path=root </> "session.checkpoint"
      sourcePath=root </> "source.hs"
      original=newBuffer "abc λ\n"
      edited=undo (replaceSelection (Selection 0 0) "prefix " (replaceSelection (Selection 1 2) "中" original))
      bytes=BS.pack [0,255,10,128]
      hex=undo (replaceSelection (Selection 0 0) "B" (replaceSelection (Selection 1 2) "A" (newByteBuffer bytes)))
      fresh=(initialDesktop (80,25)) {guestPrivatePaths=[root </> "fresh-config"],nativeMac=True,browserFrontend=True}
      source=modifyActive (\w -> w {bufferView=SideBySideView,reviewSplit=63}) (addDocument (Just (FileState sourcePath (Just (bufferBytes original)))) edited (initialDesktop (100,35)))
      sourceId=bufferId (fromJust (activeWindow source))
      split=fst (runCommand SplitVertical source)
      binary=addDocument Nothing hex split
      binaryId=bufferId (fromJust (activeWindow binary))
      transcript="Session: old-provider-id\nPublic transcript\nOther: private pending answer"
      conversation=addReadOnly "Conversation" transcript binary
      conversationId=bufferId (fromJust (activeWindow conversation))
      terminal=addReadOnly "Terminal 7" "last terminal output" conversation
      terminalId=bufferId (fromJust (activeWindow terminal))
      approval=addReadOnly "Agent request" "pending approval body" terminal
      approvalId=bufferId (fromJust (activeWindow approval))
      draft=replaceSelection (Selection 0 0) "private draft λ" (newBuffer "")
      desktop=approval {composerBuffer=draft,composerSelection=Selection 1 5,composerFocused=True,defaultDirectory=Just root,
        sideTree=Just (Sidebar root [TreeRow "source.hs" sourcePath 0 False False] 0 0 23 True),problemsVisible=True,problemsPreferredHeight=9,
        wordStar=True,blinkCursor=False,pixelateUnicode=True,materialIcons=True,appearance=DarkMode,streamerMode=True,
        dialog=Just (Dialog "Pending permission" (PermissionDialog "approve:secret") [] 0 ["Allow"] []),
        menu=Just (0,0),drag=Just (Selecting sourceId),clipboard="transient clipboard",clipboardExport=(3,Just "export"),
        chatActions=[(T.length "Session: old-provider-id\nPublic transcript\n",T.length transcript,"question-input",["pending-action-token"])],agentReplying=True,agentQueued=3,agentSettings=[AgentSetting "token" "Token" "private" "secret" []],
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
  check "recovery preserves exact text state and both history stacks" (snapshotBuffer (get recovered sourceId)==snapshotBuffer edited)
  check "recovered undo and redo behave exactly like the original"
    (snapshotBuffer (undo (get recovered sourceId))==snapshotBuffer (undo edited) && snapshotBuffer (redo (get recovered sourceId))==snapshotBuffer (redo edited))
  check "hex bytes saved representation and history survive recovery" (snapshotBuffer (get recovered binaryId)==snapshotBuffer hex && bufferBytes (redo (get recovered binaryId))==bufferBytes (redo hex))
  check "split windows retain shared buffer IDs geometry and selection" (windows recovered==filter ((/=approvalId).bufferId) (windows desktop))
  check "conversation transcript and private draft history survive"
    ("Public transcript" `T.isInfixOf` contents (get recovered conversationId) && not ("private pending answer" `T.isInfixOf` contents (get recovered conversationId)) && snapshotBuffer (composerBuffer recovered)==snapshotBuffer draft && composerSelection recovered==Selection 1 5)
  check "ended terminals are inert read-only views and approval buffers are omitted"
    (documentLabel (buffers recovered M.! terminalId)==Just "Ended Terminal 7" && M.notMember approvalId (buffers recovered))
  check "fresh runtime privacy/frontend values survive and transient controls reset"
    (guestPrivatePaths recovered==guestPrivatePaths fresh && nativeMac recovered && browserFrontend recovered && agentSettings recovered==agentSettings fresh &&
     dialog recovered==Nothing && menu recovered==Nothing && drag recovered==Nothing && clipboard recovered=="" && clipboardExport recovered==(0,Nothing) &&
     null (childAgentSettings recovered) && not (childAgentSteering recovered) && childAgentContextUsage recovered==Nothing && not (agentReplying recovered) && agentQueued recovered==0 && agentContextUsage recovered==Nothing && null (chatActions recovered) && null (diagnostics recovered))
  check "project sidebar dock geometry and display preferences survive"
    (defaultDirectory recovered==Just root && sideTree recovered==sideTree desktop && screenSize recovered==(100,35) && problemsVisible recovered && problemsPreferredHeight recovered==9 &&
     wordStar recovered && not (blinkCursor recovered) && pixelateUnicode recovered && materialIcons recovered && appearance recovered==DarkMode && streamerMode recovered)
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
      primaryChat=modifyActive (\w->w {scrollRow=12,selection=Selection 1 6})
        (addReadOnly "Conversation" (T.replicate 80 "primary transcript\n") (initialDesktop (80,25)))
        {composerBuffer=newBuffer "primary draft",composerSelection=Selection 2 5}
      childChat=selectConversationView "agent-2" "Child" primaryChat
      childBid=bufferId (fromJust (activeWindow childChat))
      populated=modifyActive (\w->w {scrollRow=8,selection=Selection 2 7}) childChat
        {buffers=M.adjust (\doc->restyle doc {documentBuffer=newBuffer (T.replicate 80 "child transcript\n")}) childBid (buffers childChat),
         composerBuffer=newBuffer "child draft",composerSelection=Selection 1 4}
  writeCheckpoint viewsPath populated >>= right
  restoredViews<-readCheckpoint viewsPath fresh >>= right
  let restoredPrimary=selectConversationView "" "Primary" restoredViews
      restoredChild=selectConversationView "agent-2" "Child" restoredPrimary
  check "each conversation retains its document draft selection and scroll"
    (activeText restoredPrimary==T.replicate 80 "primary transcript\n" && contents (composerBuffer restoredPrimary)=="primary draft" &&
     composerSelection restoredPrimary==Selection 2 5 && scrollRow (fromJust (activeWindow restoredPrimary))==12 &&
     activeText restoredChild==T.replicate 80 "child transcript\n" && contents (composerBuffer restoredChild)=="child draft" &&
     composerSelection restoredChild==Selection 1 4 && scrollRow (fromJust (activeWindow restoredChild))==8)
  let reopened=selectConversationView "" "Primary" (closeActive restoredChild)
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
    (all (\w -> bufferView w==SideBySideView && reviewSplit w==63) [w | w<-windows recovered,bufferId w==sourceId])
  check "checkpoint omits private pending answers and approval tokens" (not ("private pending answer" `BS.isInfixOf` encoded) && not ("pending-action-token" `BS.isInfixOf` encoded) && not ("pending approval body" `BS.isInfixOf` encoded))
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
  mutate (set "schemaVersion" (toJSON (2::Int)))
  mutate (set "nextId" (toJSON (0::Int)))
  mutate (set "screen" (toJSON ((maxBound::Int),25::Int)))
  mutate (set "composerSelection" (toJSON ((-1::Int),999999::Int)))
  mutate (set "buffers" (toJSON ([]::[Value])))
  let alterFirst key change (Object fields)=case KM.lookup key fields of
        Just (Array entries) | not (V.null entries)->Object (KM.insert key (Array (entries V.// [(0,change (V.head entries))])) fields)
        _->Object fields
      alterFirst _ _ value=value
      duplicateBuffers (Object fields)=case KM.lookup "buffers" fields of
        Just (Array entries) | not (V.null entries)->Object (KM.insert "buffers" (Array (V.cons (V.head entries) entries)) fields)
        _->Object fields
      duplicateBuffers value=value
  mutate duplicateBuffers
  mutate (alterFirst "windows" (set "bounds" (toJSON ((0::Int),(0::Int),(0::Int),(10::Int)))))
  mutate (alterFirst "windows" (set "selection" (toJSON ((-1::Int),(0::Int)))))
  mutate (alterFirst "windows" (set "bufferId" (toJSON (999999::Int))))
  BS.writeFile path "{not-json: secret}"
  corrupted<-readCheckpoint path fresh
  check "corrupt checkpoint returns an error" (case corrupted of Left _->True; _->False)
  switched<-right (toggleByteMode (newBuffer "λ中") >>= restoreBuffer . snapshotBuffer)
  check "mode-switch recovery keeps text saved baseline and reversible representation" (byteMode switched && not (savedByteMode switched) && contents (undo switched)=="λ中" && not (byteMode (undo switched)))
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
keyChecks=do
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
  let chat=(addReadOnly "Conversation" "Public prompt\nOther: private" desktop) {composerBuffer=newBuffer "unsent draft"}
      masked=chat {chatActions=[(14,28,"question-input",["ephemeral-token"])]}
  chatKey<-checkpointKey chat
  maskedKey<-checkpointKey masked
  tokenOnly<-checkpointKey masked {chatActions=[(14,28,"question-input",["other-token"])]}
  remembered<-checkpointKey (rememberConversationView masked)
  check "private pending-answer mask participates but tokens do not" (chatKey/=maskedKey && maskedKey==tokenOnly)
  check "checkpoint key shares conversation-view normalization" (maskedKey==remembered)
  draftKey<-checkpointKey chat {composerBuffer=newBuffer "different draft"}
  check "unsent composer replacement changes checkpoint key" (chatKey/=draftKey)
