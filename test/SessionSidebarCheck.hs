{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : SessionSidebarCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module SessionSidebarCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket,evaluate)
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
import Hide.GuestAccess (guestEffectsAllowed)
import Hide.Model
import Hide.Files (loadFile)
import Hide.Session
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
        pure (service,selected)
      (_,expired)<-sessionSidebarEffects service (\d _->pure (False,d)) after
        [SessionSidebarAction (SelectSessionWindow (T.pack (sessionId current)) firstId)]
      unless (status expired=="Session window expired or is unavailable.") (fail "Retired Sessions registration accepted a selection")
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
