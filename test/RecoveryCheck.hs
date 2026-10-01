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
import THC.Edit.Model
import THC.Edit.Recovery

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->do
  let path=root </> "session.checkpoint"
      sourcePath=root </> "source.hs"
      original=newBuffer "abc λ\n"
      edited=undo (replaceSelection (Selection 0 0) "prefix " (replaceSelection (Selection 1 2) "中" original))
      bytes=BS.pack [0,255,10,128]
      hex=undo (replaceSelection (Selection 0 0) "B" (replaceSelection (Selection 1 2) "A" (newByteBuffer bytes)))
      fresh=(initialDesktop (80,25)) {guestPrivatePaths=[root </> "fresh-config"],nativeMac=True,browserFrontend=True}
      source=addDocument (Just (FileState sourcePath (Just (bufferBytes original)))) edited (initialDesktop (100,35))
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
  recovered<-readCheckpoint path fresh >>= right
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
     not (agentReplying recovered) && agentQueued recovered==0 && agentContextUsage recovered==Nothing && null (chatActions recovered) && null (diagnostics recovered))
  check "project sidebar dock geometry and display preferences survive"
    (defaultDirectory recovered==Just root && sideTree recovered==sideTree desktop && screenSize recovered==(100,35) && problemsVisible recovered && problemsPreferredHeight recovered==9 &&
     wordStar recovered && not (blinkCursor recovered) && pixelateUnicode recovered && materialIcons recovered && appearance recovered==DarkMode && streamerMode recovered)
  BS.writeFile sourcePath "external disk edit"
  let baseline=fromJust (documentFile (buffers recovered M.! sourceId))
  check "source disk baseline survives independently of current disk" (diskBytes baseline==Just (bufferBytes original))
  conflict<-saveFile baseline (get recovered sourceId)
  disk<-BS.readFile sourcePath
  check "restored old baseline triggers existing save conflict checks" (case conflict of Left _->disk=="external disk edit"; _->False)
  encoded<-BS.readFile path
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
