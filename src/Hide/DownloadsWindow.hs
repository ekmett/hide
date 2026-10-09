{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.DownloadsWindow
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- One Downloads-owned coalesced preparation worker and exact window lifetime.
-- Closing never cancels a transfer. Progress refreshes installed snapshots only;
-- explicit reopening creates a fresh ref. Per-job revisions retain Details.
module Hide.DownloadsWindow (Owner,withOwner,setMenuReference,open,tick,cancelTarget) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Exception (SomeException,SomeAsyncException,fromException,throwIO,try,evaluate)
import Control.Monad (void)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Hide.Plugin.Tree as Tree
import qualified Hide.Plugin.Window as W
import qualified Hide.Plugin.Menu as Menu
import Hide.PluginWindowHost (adoptWindowUpdate)
import Hide.Model
import qualified Hide.Downloads as D

data Page=Page !Int !W.PreparedWindow
data Installed=Installed !(Maybe W.WindowRef) !Int !(Maybe Page) !Bool
data View=View !Int !(Maybe Installed) !(Maybe Menu.MenuRef)
data Owner=Owner !D.Downloads !W.WindowScope !(IORef View)
  !(TVar (Maybe (Int,Int,[(Int,D.Download)],Maybe Menu.MenuRef))) !(TMVar (Int,Int,Either Text Page))

-- | Preparation and joins are outside the desktop lock. Scope shutdown retires
-- every escaped ref after the worker has cancelled; accepted jobs are independent.
withOwner :: D.Downloads -> (Owner -> IO a) -> IO a
withOwner downloads action=W.withWindowScope $ \scope->do
  view<-newIORef (View 0 Nothing Nothing)
  desired<-newTVarIO Nothing
  latest<-newEmptyTMVarIO
  withAsync (loop desired latest M.empty) $ \_->action (Owner downloads scope view desired latest)
  where
    loop desired latest cache=do
      (epoch,revision,rows,menu)<-atomically $ do
        request<-readTVar desired
        maybe retry (\value->writeTVar desired Nothing >> pure value) request
      outcome<-try (prepare revision rows menu cache)
      (result,nextCache)<-case outcome of
        Right (page,next)->pure (Right page,next)
        Left (exception::SomeException) | Just async<-(fromException exception :: Maybe SomeAsyncException)->throwIO async
                       | otherwise->pure (Left "Download details could not be prepared.",cache)
      atomically $ void (tryTakeTMVar latest) >> putTMVar latest (epoch,revision,result)
      loop desired latest nextCache
    prepare revision rows menu cache=do
      prepared<-mapM (\(version,row)->do
        let ident=D.downloadId row
        detail<-case M.lookup ident cache of
          Just (old,value) | old==version->pure value
          _->W.prepareTextWindow "Details" (T.copy (T.take 16384 (downloadDetail row)))
        label<-evaluate (T.copy (T.take 240 (T.map (\c->if c<' ' || c=='\DEL' then ' ' else c)
          (D.downloadLabel row<>" — "<>stateLabel (D.downloadState row)))))
        key<-either (fail . T.unpack) pure (Tree.nodeId (T.pack (show ident)))
        pure (ident,version,detail,W.WindowRow key label detail)) rows
      view<-W.prepareRecoverableRowsWindow "hide.downloads" 1 "Downloads" (maybe [] pure menu) [row | (_,_,_,row)<-prepared] >>= either (fail . T.unpack) pure
      page<-evaluate (Page revision view)
      pure (page,M.fromList [(ident,(version,detail)) | (ident,version,detail,_)<-prepared])

-- | Registration startup/teardown binds the owner's one contribution. No
-- handler is stored here; workers publish only its immutable reference metadata.
setMenuReference :: Owner -> Maybe Menu.MenuRef -> IO ()
setMenuReference (Owner _ _ ref _ _) menu=modifyIORef' ref (\(View epoch view _)->View epoch view menu)

-- | Human open intent. A currently installed manager is revealed, never replaced
-- by a fresh lifetime. Missing/closed views request a new prepared opening.
open :: Owner -> Desktop -> IO Desktop
open owner@(Owner downloads _ ref desired _) d=do
  View epoch current menu<-readIORef ref
  case current of
    Just (Installed (Just target) wanted page _) | installed target d->do
      writeIORef ref (View epoch (Just (Installed (Just target) wanted page True)) menu)
      tick owner d
    _->do
      case current of Just (Installed old _ _ _)->mapM_ W.retireWindowRef old; _->pure ()
      (revision,rows)<-D.downloadVersionedSnapshot downloads
      let next=epoch+1
      writeIORef ref (View next (Just (Installed Nothing revision Nothing True)) menu)
      atomically (writeTVar desired (Just (next,revision,rows,menu)))
      pure d {status="Preparing Downloads; transfers continue while closed."}

-- | Exact window ownership precedes the live job lookup/cancellation. Progress,
-- selection and geometry are not authority. Closed/reopened refs cannot retarget.
cancelTarget :: Owner -> W.WindowRef -> Desktop -> IO Bool
cancelTarget (Owner _ _ ref _ _) target d=do
  View _ current _<-readIORef ref
  live<-W.windowRefCurrent target
  pure (live && dialog d==Nothing && not (questionActive d) && case current of
    Just (Installed (Just owned) _ _ _)->owned==target && installed target d
    _->False)

installed :: W.WindowRef -> Desktop -> Bool
installed target d=M.member target (pluginWindows d) && any ((==PluginContent target).windowContent) (windows d)

-- | Idle work is scalar catalogue/ref metadata only. A worker page retains the
-- chosen NodeId and clamps the sole Window Details selection through the host.
-- New openings wait for protected input; existing refreshes never steal focus.
tick :: Owner -> Desktop -> IO Desktop
tick (Owner downloads scope ref desired latest) d=do
  View epoch current menu<-readIORef ref
  case current of
    Nothing->pure d
    Just (Installed target _ _ _) | maybe False (\reference->not (installed reference d)) target->do
      mapM_ W.retireWindowRef target
      writeIORef ref (View epoch Nothing menu)
      atomically (writeTVar desired Nothing)
      pure d
    Just (Installed target wanted page focusWanted)->do
      revision<-D.downloadRevision downloads
      requested<-if revision/=wanted then do
        (actual,rows)<-D.downloadVersionedSnapshot downloads
        atomically (writeTVar desired (Just (epoch,actual,rows,menu)))
        pure actual
        else pure wanted
      publication<-atomically (tryTakeTMVar latest)
      let (nextPage,err)=case publication of
            Just (generation,published,Right value) | generation==epoch,published==requested->(Just value,Nothing)
            Just (generation,published,Left failure) | generation==epoch,published==requested->(page,Just failure)
            _->(page,Nothing)
          canOpen=dialog d==Nothing && not (questionActive d) && not (activeAutocomplete d)
          needsPage=case nextPage of Just (Page preparedRevision _)->preparedRevision==requested; _->False
          already=case (target,nextPage) of
            (Just reference,Just (Page _ prepared))->M.lookup reference (pluginWindows d)==Just prepared
            _->False
      (nextTarget,shown)<-case nextPage of
        Just (Page _ prepared) | needsPage && not already && (target/=Nothing || canOpen)->do
          update<-case target of Nothing->W.openWindow scope prepared; Just reference->W.refreshWindow reference prepared
          case update of
            Nothing->pure (target,d)
            Just value->do
              adopted<-adoptWindowUpdate Menu.HumanMenu value d
              let reference=W.updateWindowRef value
              pure (if installed reference adopted then Just reference else target,adopted)
        _->pure (target,d)
      let focusReady=focusWanted && canOpen && nextTarget/=Nothing
          focused=if focusReady then case [windowId w | w<-windows shown,Just (windowContent w)==fmap PluginContent nextTarget] of
            ident:_->focusWindow ident shown; _->shown else shown
      writeIORef ref (View epoch (Just (Installed nextTarget requested nextPage (focusWanted && not focusReady))) menu)
      pure focused {status=maybe (status focused) id err}

stateLabel :: D.DownloadState -> Text
stateLabel state=case state of
  D.DownloadQueued->"Queued"
  D.DownloadRunning progress->D.downloadPhase progress
  D.DownloadCancelling->"Cancelling"
  D.DownloadComplete _->"Installed"
  D.DownloadFailed _->"Failed"
  D.DownloadCancelled->"Cancelled"
downloadDetail :: D.Download -> Text
downloadDetail row=case D.downloadState row of
  D.DownloadRunning progress->D.downloadPhase progress<>"\n"<>T.pack (show (D.downloadBytes progress))<>
    maybe " bytes received" (\total->" / "<>T.pack (show total)<>" bytes received") (D.downloadTotal progress)
  D.DownloadComplete path->"Installed: "<>T.pack path
  D.DownloadFailed err->err
  state->stateLabel state
