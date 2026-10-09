{-# LANGUAGE OverloadedStrings #-}
module SessionSidebarCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.Async as Async
import Control.Exception (bracket,evaluate,finally,try,IOException)
import Control.Monad (foldM,unless)
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hide.Accessibility
import System.Directory
import System.Environment (lookupEnv,setEnv,unsetEnv)
import System.FilePath ((</>),takeFileName)
import System.IO (openTempFile,hClose)
import System.Timeout (timeout)
import Hide.Buffer (newBuffer)
import Hide.GuestAccess (guestEffectsAllowed,guestKeyboardAllowed)
import Hide.Model
import Hide.Files (loadFile)
import Hide.Session
import Hide.RemoteEndpoint (sessionEndpoint,withEndpointListener,socketToEndpoint)
import qualified Network.Socket as N
import qualified Graphics.Vty as V
import Hide.SessionSidebar
import Hide.SessionSidebarTypes
import Hide.Sidebar
import Hide.SidebarCommands
import Hide.PluginWindowHost (adoptWindowUpdate)
import qualified Hide.Plugin.Window as W
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Tree as P

checks :: IO ()
checks=W.withWindowScope $ \scope->bracket temporary removePathForcibly $ \root->
  environment "XDG_DATA_HOME" (Just (root </> "data")) $ do
    firstRecord<-newSessionRecord Nothing ["--private-startup-secret"]
    otherRecord<-newSessionRecord (Just "remote.example") ["--other-private-secret"]
    let current=firstRecord {sessionId=replicate 48 'a',sessionDirectory=root}
        other=otherRecord {sessionId=replicate 48 'b',sessionDirectory=root}
    rememberSession current
    rememberSession other
    checkpointPath (sessionId current) >>= \path->writeFile path "recoverable fixture"
    let first=addDocument Nothing (newBuffer "first\n") (initialDesktop (100,35))
        firstId=case windows first of window:_->windowId window; _->error "Missing fixture window"
        named=first {buffers=M.map (\doc->doc {documentLabel=Just "First"}) (buffers first)}
        added=addDocument Nothing (newBuffer "second\n") named
        sourceInitial=added {buffers=M.map (\doc->doc {documentLabel=Just (fromMaybe "Second" (documentLabel doc))}) (buffers added),sideTree=Just (emptySidebar root 28 False)}
    let authority=root </> "authority-name.json"
    writeFile authority "private-authority-value"
    (authorityFile,authorityBuffer)<-loadFile authority >>= either fail pure
    let publicInitial=(addDocument (Just authorityFile) authorityBuffer sourceInitial) {guestPrivatePaths=[authority],streamerMode=False}
    prepared<-W.prepareTextWindow "Plugin notes" "Private plugin text"
    update<-W.openWindow scope prepared >>= maybe (fail "Plugin scope unexpectedly retired") pure
    initial<-adoptWindowUpdate Menu.HumanMenu update publicInitial
    withSidebarCommands $ \host->do
      (service,after)<-withSessionSidebar host (Just (sessionId current)) initial $ \service->do
        let core=sessionSidebarEffects service (\d _->pure (False,d))
            tick d=tickSessionSidebar service host d >>= tickSidebar host core
            wait label predicate d=timeout 5000000 (loop d) >>= maybe (fail (label<>" timed out")) pure
              where loop value=do updated<-tick value; if predicate updated then pure updated else threadDelay 1000 >> loop updated
            rows d=maybe [] (M.elems . treeRows) (sideTree d)
            has name=any ((==name).P.infoLabel.rowInfo).rows
            hasId value=any ((==value).P.nodeIdText.P.infoId.rowInfo).rows
            activate predicate d=case [i | (i,row)<-zip [0..] (rows d),predicate (rowInfo row)] of
              i:_->let (requested,effects)=activateTree True i d in snd <$> sidebarEffects host core requested effects
              _->fail "Missing Sessions row"
            ready d=maybe False (\tree->treeProjectionRevision tree==treeRevision tree) (sideTree d)
            act (d,effects)=snd <$> sidebarEffects host core d effects
            popup node d=case [(i,row) | (i,row)<-maybe [] (visibleRows 0 32768) (sideTree d),P.nodeIdText (P.infoId (rowInfo row))==node] of
              (i,_):_->let scroll=maybe 0 treeScroll (sideTree d)
                       in fst (handleEvent (V.EvMouseDown 5 (2+i-scroll) V.BRight []) d)
              _->error "Missing context-menu target"
            menuLabels d=maybe [] (const (map fst (contextItemsFor d))) (contextMenu d)
            choose node title d=do
              let opened=popup node d
              case [i | (i,(name,_))<-zip [0..] (contextItemsFor opened),name==title] of
                i:_->act (handleEvent (V.EvKey V.KEnter []) opened {contextMenu=fmap (\(rect,_)->(rect,i)) (contextMenu opened)})
                _->fail "Missing session action"
            currentNode="session:"<>T.pack (sessionId current)
        started<-initializeSidebar host initial
        mounted<-wait "Sessions root" (has "Sessions") started
        expanded<-activate ((=="Sessions").P.infoLabel) mounted >>= wait "distinct catalog sessions"
          (\d->hasId currentNode d && hasId ("session:"<>T.pack (sessionId other)) d && ready d)
        unless ((windowId <$> activeWindow expanded)==(windowId <$> activeWindow initial))
          (fail "Background discovery stole window focus")
        let labels=map (P.infoLabel.rowInfo) (rows expanded)
        unless (length [value | value<-labels,T.pack (takeFileName root) `T.isPrefixOf` value]==2 &&
          all (not . T.isInfixOf "private") labels)
          (fail "Same-directory sessions are ambiguous or expose private arguments")
        views<-activate ((==currentNode).P.nodeIdText.P.infoId) expanded >>= wait "current session windows"
          (\d->has "First" d && has "Second" d && has "Private plugin window" d && has "Private buffer" d && ready d)
        let semanticFrames=[BL.toStrict (encode (sidebarSemantics audience frame)) |
              audience<-[OwnerSemantics,GuestSemantics],frame<-[views,views {streamerMode=True}]]
            fullKeys=map (TE.encodeUtf8 . T.pack . sessionId) [current,other]
        unless (all (\bytes->all (not . (`BS.isInfixOf` bytes)) fullKeys) semanticFrames)
          (fail "Session semantic metadata exposes a full private session identifier")
        let windowLabels=[P.infoLabel (rowInfo row) | row<-rows views,"window:" `T.isPrefixOf` P.nodeIdText (P.infoId (rowInfo row))]
        unless (not (any (`elem` windowLabels) ["Plugin notes","authority-name.json"]) && not (streamerMode views))
          (fail "Sessions exposes private window names with Streamer disabled")
        selected<-activate ((=="First").P.infoLabel) views >>= wait "captured window selection"
          (\d->(windowId <$> activeWindow d)==Just firstId)
        unless (guestEffectsAllowed [SessionSidebarAction (SelectSessionWindow (T.pack (sessionId current)) firstId)]==False)
          (fail "Guest can manufacture human session selection")
        (_,wrongSession)<-core views [SessionSidebarAction (SelectSessionWindow (T.pack (sessionId other)) firstId)]
        unless ((windowId <$> activeWindow wrongSession)==(windowId <$> activeWindow views))
          (fail "Another session selected a local numeric window")
        let modal=views {dialog=Just (Dialog "Hold" Information [] 0 ["OK"] [])}
        (_,blocked)<-core modal [SessionSidebarAction (SelectSessionWindow (T.pack (sessionId current)) firstId)]
        unless ((windowId <$> activeWindow blocked)==(windowId <$> activeWindow modal) && dialog blocked/=Nothing)
          (fail "Session window bypassed modal input")
        let removed=views {windows=filter ((/=firstId).windowId) (windows views)}
        (_,stale)<-core removed [SessionSidebarAction (SelectSessionWindow (T.pack (sessionId current)) firstId)]
        unless ((windowId <$> activeWindow stale)==(windowId <$> activeWindow removed))
          (fail "Removed window action selected a replacement")
        -- This owner path must never force a document's live payload/history.
        let poison=views {buffers=M.map (\doc->doc {documentBuffer=error "Sessions forced a Buffer",documentHighlight=error "Sessions forced highlights"}) (buffers views)}
        cheap<-timeout 1000000 (foldM (\d _->tickSessionSidebar service host d) poison [1..32::Int] >>= evaluate . length . windows)
        unless (cheap==Just (length (windows views))) (fail "Session metadata blocks the UI owner")
        -- Use the real sidebar context menu and host confirmation lifecycle.
        saved<-newSessionRecord Nothing []
        rememberSession saved
        savedPath<-checkpointPath (sessionId saved)
        writeFile savedPath "saved unsaved work"
        let savedNode="session:"<>T.pack (sessionId saved)
            confirmation d=maybe False (T.isPrefixOf "Delete session ".dialogTitle) (dialog d)
            openDelete d=choose savedNode "Delete..." d >>= wait "delete confirmation" confirmation
        catalogued<-wait "saved session" (\d->hasId savedNode d && ready d) selected
        unless ("Delete..." `elem` menuLabels (popup savedNode catalogued) &&
                null (menuLabels (popup currentNode catalogued)) &&
                null (menuLabels (popup ("session:"<>T.pack (sessionId other)) catalogued)))
          (fail "Session deletion menu does not distinguish stopped local/current/remote targets")
        switching<-choose savedNode "Recover" catalogued >>= wait "captured session recovery"
          ((==Just (T.pack (sessionId saved))).pendingSessionSwitch)
        unless ((windowId <$> activeWindow switching)==(windowId <$> activeWindow catalogued))
          (fail "Session request changed the old desktop before attachment")
        let request=SessionSidebarAction (SwitchSession (T.pack (sessionId saved)) (sessionAttachment catalogued))
        (_,expiredSwitch)<-core catalogued {sessionAttachment=sessionAttachment catalogued+1} [request]
        (_,modalSwitch)<-core modal [request]
        unless (pendingSessionSwitch expiredSwitch==Nothing && pendingSessionSwitch modalSwitch==Nothing && not (guestEffectsAllowed [request]))
          (fail "Session request crossed display lifetime, modal or input authority")
        opened<-openDelete switching {pendingSessionSwitch=Nothing}
        unless (not (guestKeyboardAllowed opened) && maybe False (\dg->null (fields dg) && buttons dg==["Delete","Cancel"]) (dialog opened))
          (fail "Deletion is not a human-only button confirmation")
        cancelled<-act (handleEvent (V.EvKey V.KEsc []) opened)
        retained<-doesFileExist savedPath
        unless retained (fail "Cancel deleted the saved session")
        reopened<-openDelete cancelled
        submitted<-act (handleEvent (V.EvKey V.KEnter []) reopened)
        deleted<-wait "saved-session deletion" ((=="Saved session deleted.").status) submitted
        checkpointRemains<-doesFileExist savedPath
        recordRemains<-loadSession (sessionId saved)
        unless (not checkpointRemains && recordRemains==Nothing) (fail "Confirmed deletion left saved session data")
        hidden<-wait "deleted-session row removal" (not . hasId savedNode) deleted
        pure (service,hidden)
      (_,expired)<-sessionSidebarEffects service (\d _->pure (False,d)) after
        [SessionSidebarAction (SelectSessionWindow (T.pack (sessionId current)) firstId)]
      unless (status expired=="Session window expired or is unavailable.") (fail "Retired Sessions registration accepted a selection")
    deletionChecks
    putStrLn "session sidebar checks passed"
  where
    temporary=do
      base<-getTemporaryDirectory
      (path,handle)<-openTempFile base "hide-session-sidebar"
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path
    environment key value action=bracket (lookupEnv key <* set value) set (const action)
      where set=maybe (unsetEnv key) (setEnv key)

-- Exercise the operation independently of the confirmation UI. A real listener
-- also proves that an endpoint with a different/missing lifetime lock is refused.
deletionChecks :: IO ()
deletionChecks=do
  captured<-newSessionRecord Nothing ["saved-session-argument"]
  let ident=sessionId captured
  endpoint<-sessionEndpoint ident
  checkpoint<-checkpointPath ident
  directory<-sessionStoreDirectory
  let metadata=directory </> ident++".json"
      artifacts=[metadata,checkpoint,checkpoint++".agent.json",checkpoint++".agents.json",endpoint++".json"]
      check label ok=unless ok (fail label)
      refused label current record=do
        before<-mapM BS.readFile artifacts
        result<-try (deleteStoppedSession current record) :: IO (Either IOException ())
        after<-mapM BS.readFile artifacts
        check label (case result of Left _->before==after;Right _->False)
  flip finally (forgetSession ident) $ do
    rememberSession captured
    mapM_ (\path->BS.writeFile path "saved private payload") (drop 1 artifacts)
    refused "Current-session deletion preserves every saved artifact" (Just ident) captured
    check "Current-session refusal never opens its lifetime lock" . not =<< doesFileExist (checkpoint++".lock")
    let remote=captured {sessionHost=Just "remote.example"}
    rememberSession remote
    refused "Remote-session deletion preserves every saved artifact" Nothing remote
    rememberSession captured {sessionArguments=["replacement-session-argument"]}
    refused "Captured-session deletion refuses changed records without deleting data" Nothing captured
    rememberSession captured
    withEndpointListener endpoint $ \listener authenticate->
      withAsync (do
        (socket,_)<-N.accept listener
        bracket (socketToEndpoint socket) (\(handle,shutdown)->shutdown `finally` hClose handle)
          (authenticate . fst)) $ \worker->do
        refused "Live endpoint deletion is refused even without its daemon lock" Nothing captured
        completed<-timeout 3000000 (Async.wait worker)
        check "Live endpoint probe reached the real listener" (completed==Just ())
    deleteStoppedSession Nothing captured
    absent<-mapM doesFileExist artifacts
    check "Stopped-session deletion removes record, checkpoint and sidecars" (not (or absent))
    check "Stopped-session deletion retains the lifetime lock file" =<< doesFileExist (checkpoint++".lock")
