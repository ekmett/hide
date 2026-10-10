{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | Module      : Hide.SessionSidebar
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings, ScopedTypeVariables
--
-- Sessions consumes the existing catalog and shared tree. Discovery/probes stay
-- on one scoped worker; ticks publish only scalar window metadata and invalidate
-- at most two nodes. Selection reuses the existing modal-aware window owner.
module Hide.SessionSidebar
  ( SessionSidebar, withSessionSidebar, tickSessionSidebar, sessionSidebarEffects ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.DeepSeq (force)
import Control.Exception (IOException,catch,evaluate,displayException)
import Control.Monad (foldM,forever)
import Data.Aeson (Value(Null))
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (canonicalizePath)
import System.FilePath (takeFileName,dropTrailingPathSeparator)
import System.Timeout (timeout)
import Hide.Model
import Hide.Sidebar (treeRoot)
import Hide.Plugin.Command
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Form as Form
import qualified Hide.Plugin.Tree as P
import qualified Hide.Session as S
import qualified Hide.Recovery as Recovery
import Hide.SessionSidebarTypes
import Hide.SidebarCommands
import Text.Read (readMaybe)

-- No SessionRecord, startup arguments, provider identities or payloads retained.
data Entry = Entry !Text !Text !Text !Text !Bool deriving (Eq)
data Snapshot = Snapshot !Integer ![Entry] !Integer ![(Int,Text)]
data SessionSidebar = SessionSidebar !(P.TreeProvider SidebarContext SidebarReply)
  !(Maybe Text) !(IORef Snapshot) !(IORef (Integer,Integer)) !(IORef FilePath)
rootId :: P.NodeId
rootId=ident "sessions"
ident :: Text -> P.NodeId
ident=either (error . T.unpack) id . P.nodeId
sessionNode :: Text -> P.NodeId
sessionNode=ident . ("session:"<>)
windowNode :: Int -> P.NodeId
windowNode=ident . ("window:"<>) . T.pack . show
label :: Text -> Text
label=T.take 256 . T.map (\c->if c<' ' || c=='\DEL' then '·' else c)

-- | Scope one background catalog worker and its typed tree actions. Discovery
-- has a ten-second whole-attempt limit, retaining the last successful inventory
-- on timeout/error. Pages contain at most 128 rows, inventories at most 1024.
-- The optional identity belongs to App's existing daemon; discovery never attaches.
withSessionSidebar :: SidebarHost -> Maybe String -> Desktop -> (SessionSidebar -> IO a) -> IO a
withSessionSidebar host current initial use=withRegistry $ \registry->do
  let currentId=T.pack <$> current
      directory=label (T.pack (takeFileName (dropTrailingPathSeparator (startingDirectory initial))))
      fallback=maybe [] (\sid->[Entry sid "local" directory "current" True]) currentId
  mapM_ evaluate fallback
  source<-newIORef (Snapshot 0 fallback 0 [])
  seen<-newIORef (-1,-1)
  workspace<-newIORef (maybe (startingDirectory initial) treeRoot (sideTree initial))
  command<-registerCommand registry (CommandDef "hide.sidebar.sessions.window" "Select window" hidden hidden $ \ctx request->
    pure $ if sidebarOrigin ctx==Menu.HumanMenu then Right (SidebarSession request)
      else Left (CommandRejected "Session selection requires the human.")) >>= required
  switchSaved<-registerCommand registry (CommandDef "hide.sidebar.sessions.switch" "Open saved session" hidden hidden $ \ctx sid->
    if sidebarOrigin ctx/=Menu.HumanMenu || Just sid==currentId
      then pure (Left (CommandRejected "Select another saved session.")) else do
        saved<-S.loadSession (T.unpack sid)
        case saved of
          Just record | S.sessionHost record==Nothing->do
            state<-S.sessionState record
            pure $ if state `elem` ["running","recoverable"]
              then Right (SidebarSession (SwitchSession sid (sidebarAttachment ctx)))
              else Left (CommandRejected "Saved session is no longer available.")
          _->pure (Left (CommandRejected "Select a session on the current host."))) >>= required
  recoverBuffers<-registerCommand registry (CommandDef "hide.sidebar.sessions.recover-buffers" "Recover buffers here" hidden hidden $ \ctx sid->
    if sidebarOrigin ctx/=Menu.HumanMenu || Just sid==currentId
      then pure (Left (CommandRejected "Select another stopped session.")) else
      (do
        saved<-S.loadSession (T.unpack sid)
        case saved of
          Just record | S.sessionHost record==Nothing->do
            same<-sameProject (sidebarContextDirectory ctx) record
            if not same then pure (Left (CommandRejected "Recover buffers from a session in this project.")) else do
              recovered<-S.withStoppedSession current record Recovery.readSourceCheckpoint
              pure $ case recovered of
                Left reason->Left (CommandRejected reason)
                Right sources->Right (SidebarRecoveredSources (sidebarAttachment ctx) (sidebarContextDirectory ctx) sources)
          _->pure (Left (CommandRejected "Select a session on the current host.")))
      `catch` (\(err::IOException)->pure (Left (CommandRejected (label (T.pack (displayException err))))))) >>= required
  deleteSaved<-registerCommand registry (CommandDef "hide.sidebar.sessions.delete-confirmed" "Delete saved session" hidden hidden $ \ctx captured->
    if sidebarOrigin ctx/=Menu.HumanMenu then pure (Left (CommandRejected "Session deletion requires the human.")) else
      (do
        S.deleteStoppedSession current captured
        let sid=T.pack (S.sessionId captured)
        atomicModifyIORef' source $ \(Snapshot revision entries serial views)->
          (Snapshot (revision+1) (filter (\(Entry value _ _ _ _)->value/=sid) entries) serial views,())
        pure (Right (SidebarSession (SessionDeleted sid))))
      `catch` (\(err::IOException)->pure (Left (CommandRejected (label (T.pack (displayException err))))))) >>= required
  deleteMenu<-registerCommand registry (CommandDef "hide.sidebar.sessions.delete" "Delete saved session" hidden hidden $ \ctx (sid,short)->
    if sidebarOrigin ctx/=Menu.HumanMenu || Just sid==currentId
      then pure (Left (CommandRejected "The current session cannot be deleted.")) else do
        captured<-S.loadSession (T.unpack sid)
        case captured of
          Just record | S.sessionHost record==Nothing->do
            state<-S.sessionState record
            if state/="recoverable" then pure (Left (CommandRejected "Only a stopped saved session can be deleted.")) else do
              prepared<-Form.prepareForm Form.ReadableForm
                (Form.ConfirmationFormSpec ("Delete session "<>short<>"?") "This removes its saved windows and unsaved edits." "Delete")
                (Form.formAction registry deleteSaved (const record) (\_ reply->pure reply))
              pure (SidebarForm <$> prepared)
          _->pure (Left (CommandRejected "Saved session is no longer available."))) >>= required
  let windowAction sid wid=P.treeAction registry command (SelectSessionWindow sid wid) (\_ value->pure value)
      root=P.NodeDef (P.NodeInfo rootId "Sessions" "" True Nothing) Nothing []
      entryNode allIds (Entry sid hostName project state same)=
        let here=Just sid==currentId
            short=T.pack (S.shortSessionId (T.unpack sid) (map T.unpack allIds))
            title=label (project<>"  "<>short<>"  "<>(if here then "current" else state)<>"  "<>hostName)
        in P.NodeDef (P.NodeInfo (sessionNode sid) title "" here Nothing) Nothing $
          [P.ActionMenu (if state=="recoverable" then "Recover" else "Switch to session")
             (P.treeAction registry switchSaved sid (\_ value->pure value))
          | not here,hostName=="local",state `elem` ["running","recoverable"]] ++
          [P.ActionMenu "Recover buffers here" (P.treeAction registry recoverBuffers sid (\_ value->pure value))
          | not here,hostName=="local",state=="recoverable",same] ++
          [P.ActionMenu "Delete..." (P.treeAction registry deleteMenu (sid,short) (\_ value->pure value))
          | not here,hostName=="local",state=="recoverable"]
      children _ (P.ChildRequest key cursor)=do
        Snapshot _ entries _ views<-readIORef source
        pure $ case offset cursor of
          Nothing->Left (InvalidArguments "Invalid session page cursor.")
          Just start->
            let nodes | key==rootId=map (entryNode [sid | Entry sid _ _ _ _<-entries]) entries
                      | Just sid<-currentId,key==sessionNode sid=
                          [P.NodeDef (P.NodeInfo (windowNode wid) title "" False Nothing)
                            (Just (windowAction sid wid)) [] | (wid,title)<-views]
                      | otherwise=[]
                page=take 128 (drop start nodes)
            in Right (P.NodePage page (if null (drop (start+128) nodes) then Nothing else Just (T.pack (show (start+128)))))
  provider<-P.registerTree registry "hide.sidebar.sessions" root children >>= required
  publishTreeFromHost host provider
  let service=SessionSidebar provider currentId source seen workspace
      discover=do
        result<-timeout 10000000 ((Just <$> (readIORef workspace >>= catalog)) `catch` \(_::IOException)->pure Nothing)
        case result of
          Just (Just entries)->do
            let present=maybe False (\sid->any (\(Entry value _ _ _ _)->value==sid) entries) currentId
                next=if present then entries else fallback++take (1024-length fallback) entries
            atomicModifyIORef' source $ \old@(Snapshot revision previous windowRevision views)->
              if previous==next then (old,()) else (Snapshot (revision+1) next windowRevision views,())
          _->pure ()
      worker=forever (discover >> threadDelay 1000000)
  withAsync worker (const (use service))
  where
    hidden=Codec Null (const (Left "Session targets are host-captured.")) (const Null)
    required=either (ioError . userError . show) pure
    sameProject directory record=(do
      here<-canonicalizePath directory
      there<-canonicalizePath (S.sessionDirectory record)
      pure (here==there)) `catch` (\(_::IOException)->pure False)
    catalog directory=do
      canonical<-canonicalizePath directory
      records<-take 1024 <$> S.listSessions
      mapM (\record->do
        state<-S.sessionState record
        same<-if S.sessionHost record==Nothing then sameProject canonical record else pure False
        let entry=Entry (T.pack (S.sessionId record)) (label (maybe "local" T.pack (S.sessionHost record)))
              (label (T.pack (takeFileName (dropTrailingPathSeparator (S.sessionDirectory record))))) (T.pack state) same
        evaluate entry) records
    offset Nothing=Just 0
    offset (Just value)=do
      n<-readMaybe (T.unpack value)
      if n>=0 && n<=1024 then Just n else Nothing

-- | Publish /O(windows)/ bounded titles/IDs; buffer contents and history are never
-- read or retained. Only catalog/current-window revisions trigger invalidation.
tickSessionSidebar :: SessionSidebar -> SidebarHost -> Desktop -> IO Desktop
tickSessionSidebar (SessionSidebar provider current source seen workspace) host d=do
  writeIORef workspace (maybe (startingDirectory d) treeRoot (sideTree d))
  let views=take 1024 [(wid,label title) | (wid,title,_,_)<-editorWindowEntries d {streamerMode=True}]
  _<-evaluate (force views)
  Snapshot catalogRevision _ windowRevision _<-atomicModifyIORef' source $ \old@(Snapshot revision entries serial previous)->
    let next=if previous==views then old else Snapshot revision entries (serial+1) views in (next,next)
  (oldCatalog,oldWindows)<-atomicModifyIORef' seen (\previous->((catalogRevision,windowRevision),previous))
  let changed=[rootId | catalogRevision/=oldCatalog]++[sessionNode sid | windowRevision/=oldWindows,Just sid<-[current]]
  foldM (\state node->refreshTreeFromHost host (P.treeReference provider) node state) d changed

-- | Adopt only current-session human view selection while this registration is
-- live. Missing windows, another session and modal controls refuse the selection;
-- all other effects go to the next existing interpreter.
sessionSidebarEffects :: SessionSidebar -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
sessionSidebarEffects (SessionSidebar provider current _ _ _) fallback=foldM step . (False,)
  where
    step result@(True,_) _=pure result
    step (_,d) (SessionSidebarAction (SelectSessionWindow sid wid))=do
      live<-P.treeCurrent provider
      pure (False,if live && Just sid==current && editorWindowAvailable d wid then activateEditorWindow wid d
        else d {status="Session window expired or is unavailable."})
    step (_,d) (SessionSidebarAction (SwitchSession sid attachment))=do
      live<-P.treeCurrent provider
      pure (False,if live && Just sid/=current && attachment==sessionAttachment d &&
          dialog d==Nothing && not (questionActive d) && pendingSessionSwitch d==Nothing
        then d {pendingSessionSwitch=Just sid,status="Opening saved session…"}
        else d {status="Session selection expired or is unavailable."})
    step (_,d) (SessionSidebarAction (SessionDeleted sid))=do
      live<-P.treeCurrent provider
      pure (False,if live && Just sid/=current then d {status="Saved session deleted."} else d)
    step (_,d) effect=fallback d [effect]
