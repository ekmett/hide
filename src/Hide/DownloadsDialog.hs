{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- |
-- Module      : Hide.DownloadsDialog
-- Copyright   : (c) Edward Kmett
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Downloads-owned prepared presentation. One worker formats bounded immutable
-- pages; the desktop owner touches only job IDs, revisions and modal receipts.
-- Closing retires presentation, never a transfer. Reopening has fresh identity.
module Hide.DownloadsDialog
  ( Owner, withOwner, open, tick, cancelTarget, close, downloadsDialog ) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Exception (SomeException,SomeAsyncException,fromException,throwIO,try,evaluate)
import Control.Monad (void)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Hide.Buffer (Buffer,Selection(..),newBuffer,bufferLength,bufferLineCount)
import Hide.Model
import qualified Hide.Downloads as D
import Text.Read (readMaybe)

-- The worker cache reuses a detail Buffer only by its owning job revision.
data Row=Row !Int !Int !Buffer !Int !Int
data Page=Page !Int [Text] !(V.Vector Row)
data View=View !Int !(Maybe Installed)
data Installed=Installed !Text !Int !Page !(Maybe (Int,Int))
data Owner=Owner !D.Downloads !(IORef View)
  !(TVar (Maybe (Int,Int,[(Int,D.Download)]))) !(TMVar (Int,Int,Either Text Page))

-- | Bracket one coalesced formatting worker. Shutdown cancels and joins outside
-- the desktop lock; requests and publications retain at most one bounded page.
withOwner :: D.Downloads -> (Owner -> IO a) -> IO a
withOwner downloads action=do
  view<-newIORef (View 0 Nothing)
  desired<-newTVarIO Nothing
  latest<-newEmptyTMVarIO
  withAsync (loop desired latest M.empty) $ \_->action (Owner downloads view desired latest)
  where
    loop desired latest cache=do
      (epoch,revision,rows)<-atomically $ do
        request<-readTVar desired
        maybe retry (\value->writeTVar desired Nothing >> pure value) request
      outcome<-try (prepare revision rows cache)
      (result,nextCache)<-case outcome of
        Right (page,next)->pure (Right page,next)
        Left (exception::SomeException) | Just async<-(fromException exception :: Maybe SomeAsyncException)->throwIO async
                       | otherwise->pure (Left "Download details could not be prepared.",cache)
      atomically $ void (tryTakeTMVar latest) >> putTMVar latest (epoch,revision,result)
      loop desired latest nextCache
    prepare revision rows cache=do
      prepared<-mapM (\(version,row)->do
        let ident=D.downloadId row
        buffer<-case M.lookup ident cache of
          Just (old,value) | old==version->pure value
          _->do
            let value=newBuffer (T.copy (T.take 16384 (downloadDetail row)))
            _<-evaluate (bufferLength value)
            pure value
        label<-evaluate (T.copy (T.take 240 (D.downloadLabel row<>" — "<>T.take 240 (stateLabel (D.downloadState row)))))
        size<-evaluate (bufferLength buffer)
        linesCount<-evaluate (bufferLineCount buffer)
        pure (label,Row ident version buffer size linesCount)) rows
      let page=Page revision (map fst prepared) (V.fromList (map snd prepared))
          next=M.fromList [(ident,(version,buffer)) | (_,Row ident version buffer _ _)<-prepared]
      _<-evaluate page
      pure (page,next)

-- | Explicit human view intent. A cheap loading modal receives a fresh token;
-- no background progress can reopen it or replace an unrelated newer modal.
open :: Owner -> Desktop -> IO Desktop
open (Owner downloads ref desired _) d=do
  View epoch _<-readIORef ref
  (revision,rows)<-D.downloadVersionedSnapshot downloads
  let next=epoch+1
      page=Page (-1) [] V.empty
      token=receipt next page
  writeIORef ref (View next (Just (Installed token revision page Nothing)))
  atomically (writeTVar desired (Just (next,revision,rows)))
  pure d {dialog=Just (Dialog "Downloads" (DebugDialog token)
    [ListBox "Transfers" [] 0,TextArea "Details" False emptyDetails (Selection 0 0) 0 0]
    0 ["Cancel selected","Close"] ["Preparing transfers; downloads continue while closed."])}

-- | Resolve a submitted index only in its exact installed immutable page.
-- The returned ID is captured before the caller signals cancellation.
cancelTarget :: Owner -> Text -> Int -> Desktop -> IO (Maybe Int)
cancelTarget (Owner _ ref _ _) token index d=do
  View epoch current<-readIORef ref
  pure $ do
    _<-current
    (submitted,ids)<-parseReceipt token
    if submitted/=epoch || not (submissionCurrent epoch d) then Nothing else
      case drop index ids of ident:_ | index>=0->Just ident; _->Nothing

-- | Retire a matching Close receipt; stale receipts cannot retire a reopened view.
close :: Owner -> Text -> IO ()
close (Owner _ ref desired _) token=do
  View epoch current<-readIORef ref
  case current of
    Just _ | maybe False ((==epoch).fst) (parseReceipt token)->do
      writeIORef ref (View epoch Nothing)
      atomically (writeTVar desired Nothing)
    _->pure ()

-- | Adopt only into the exact modal. Idle ticks preserve detail Buffer identity;
-- selection changes use an already-prepared row, with no formatting or text Eq.
tick :: Owner -> Desktop -> IO Desktop
tick owner@(Owner downloads ref desired latest) d=do
  View epoch current<-readIORef ref
  case (current,dialog d) of
    (Just (Installed token wanted page selected),Just dg) | purpose dg==DebugDialog token->do
      revision<-D.downloadRevision downloads
      requested<-if revision/=wanted then do
        (actual,rows)<-D.downloadVersionedSnapshot downloads
        atomically (writeTVar desired (Just (epoch,actual,rows)))
        pure actual
        else pure wanted
      publication<-atomically (tryTakeTMVar latest)
      let (nextPage,err)=case publication of
            Just (generation,published,Right value@(Page actual _ _)) | generation==epoch,published==requested,actual==requested->(value,Nothing)
            Just (generation,published,Left message) | generation==epoch,published==requested->(page,Just message)
            _->(page,Nothing)
          oldIndex=case fields dg of ListBox _ _ index:_->index; _->0
          oldId=case rowAt page oldIndex of Just (Row ident _ _ _ _)->Just ident; _->Nothing
          changed=pageRevision nextPage/=pageRevision page
          index=if changed then fromMaybe 0 (oldId >>= \ident->rowIndex ident nextPage) else oldIndex
          key=case rowAt nextPage index of Just (Row ident version _ _ _)->Just (ident,version); _->Nothing
          nextToken=if changed then receipt epoch nextPage else token
          details=if key==selected then case fields dg of _:area@TextArea{}:_->area; _->emptyField else
            case rowAt nextPage index of
              Just (Row ident _ buffer size linesCount)->case (selected,fields dg) of
                (Just (old,_),_:TextArea _ _ _ (Selection a z) top left:_) | old==ident->
                  TextArea "Details" False buffer (Selection (clamp size a) (clamp size z)) (clamp (linesCount-1) top) (clamp size left)
                _->TextArea "Details" False buffer (Selection 0 0) 0 0
              _->emptyField
          updated=if not changed && key==selected then dg else dg
            {purpose=DebugDialog nextToken,fields=[ListBox "Transfers" (pageLabels nextPage) index,details],body=["Transfers continue while this window is closed."]}
      writeIORef ref (View epoch (Just (Installed nextToken requested nextPage key)))
      pure d {dialog=Just updated,status=fromMaybe (status d) err}
    (Just (Installed token _ _ _),_)->close owner token >> pure d
    _->pure d
  where
    pageRevision (Page revision _ _)=revision
    pageLabels (Page _ labels _)=labels
    rowAt (Page _ _ rows) index=rows V.!? index
    rowIndex ident (Page _ _ rows)=V.findIndex (\(Row current _ _ _ _)->current==ident) rows

-- Captured ID order travels with the submitted effect. A newer progress page
-- cannot change the meaning of its selected index; close/reopen revokes epoch.
receipt :: Int -> Page -> Text
receipt epoch (Page _ _ rows)="downloads:"<>T.pack (show epoch)<>":"<>
  T.intercalate "," [T.pack (show ident) | Row ident _ _ _ _<-V.toList rows]
parseReceipt :: Text -> Maybe (Int,[Int])
parseReceipt token
  | T.length token>2048=Nothing
  | ["downloads",generation,order]<-T.splitOn ":" token=do
      epoch<-readMaybe (T.unpack generation)
      ids<-if T.null order then pure [] else mapM (readMaybe.T.unpack) (T.splitOn "," order)
      if epoch>0 && length ids<=64 && all (>0) ids && M.size (M.fromList [(ident,()) | ident<-ids])==length ids
        then Just (epoch,ids) else Nothing
  | otherwise=Nothing
submissionCurrent :: Int -> Desktop -> Bool
submissionCurrent epoch d=case dialog d of
  Nothing->True
  Just dg->case purpose dg of DebugDialog token->maybe False ((==epoch).fst) (parseReceipt token); _->False
clamp :: Int -> Int -> Int
clamp maximum value=max 0 (min maximum value)
emptyDetails :: Buffer
emptyDetails=newBuffer "No downloads."
emptyField :: Field
emptyField=TextArea "Details" False emptyDetails (Selection 0 0) 0 0

-- | Static documentation presentation. Construct/force this on a worker; the
-- live owner above never compares its detail text to a retained Buffer.
downloadsDialog :: [D.Download] -> Int -> Maybe Dialog -> Dialog
downloadsDialog rows index previous=Dialog "Downloads" (DebugDialog "downloads")
  [ListBox "Transfers" [D.downloadLabel row<>" — "<>stateLabel (D.downloadState row) | row<-rows] index,
   TextArea "Details" False (newBuffer detail) (Selection 0 0) 0 0]
  (maybe 0 focus previous) ["Cancel selected","Close"] ["Transfers continue while this window is closed."]
  where detail=case drop (max 0 index) rows of row:_->downloadDetail row; _->"No downloads."

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
