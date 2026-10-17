-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.InspectorWindow
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Host rendering of "Hide.Plugin.Inspect" documents on the existing prepared
-- rows window: a selectable list above one readonly Details pane.
--
-- The semantic document stays free of presentation. Here a 'List' becomes its
-- rows and a 'Text' document becomes one row per heading, with prose and facts
-- as that row's Details. Other applicable views of the same object are appended
-- as navigation rows, so view switching needs no new widget. Row actions are not
-- stored in the window: the owner keeps a small @NodeId -> RowAction@ table per
-- exact 'W.WindowRef', and a later action is admitted only through that table,
-- so a recycled row index or label cannot retarget it.
--
-- Everything runs on the caller's worker. Inspector windows are private by
-- default (no agent disclosure) and publication goes through the ordinary
-- scoped 'W.openWindow'/'adoptWindowUpdate' path.
module Hide.InspectorWindow
  ( Inspector, withInspector, inspectorViews, inspectorPublisher'
  , RowAction(..), Page(..), Followed(..), buildPage, inspectSubject, followInspectorRow, followInspectorAction, pageAction, setOpenMenu
  ) where

import Control.Monad (filterM)
import Control.Exception (finally)
import Data.IORef
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Hide.Plugin.Menu as Menu
import Hide.Plugin.Inspect
import qualified Hide.Plugin.Tree as Tree
import qualified Hide.Plugin.Window as W

-- | What a row does when activated. It is closed data, never a callback.
data RowAction = Navigate !Subject | PageRows !Subject !ViewId !Int | ShowView !Subject !ViewId | OpenPath !FilePath | OpenSite !FilePath !Int !Int !Text | GoBack
  deriving (Eq,Show)

-- | The outcome of an admitted row: another inspector page, or a source
-- location that the host must still verify against its current text.
data Followed = Opened !W.WindowUpdate | Jump !FilePath !Int !Int !Text

-- | A prepared window and the exact actions of its rows.
data Page = Page { pageWindow :: !W.PreparedWindow, pageActions :: !(M.Map Tree.NodeId RowAction) }

-- | The registry, window publication scope and live page tables of one session.
data Inspector = Inspector
  { inspectorViews :: !Inspectors, inspectorScope :: !W.WindowScope
  , inspectorPages :: !(IORef [(W.WindowRef,(M.Map Tree.NodeId RowAction,[(Subject,Maybe ViewId,Int)]))])
  , inspectorMenu :: !(IORef [Menu.MenuRef]) }

-- | Scope the registry and its window lifetimes. Closing retires every escaped
-- window reference and refuses later discovery or publication.
withInspector :: (Inspector -> IO a) -> IO a
withInspector use=withInspectors $ \views->W.withWindowScope $ \scope->do
  pages<-newIORef []
  menu<-newIORef []
  use (Inspector views scope pages menu) `finally` writeIORef pages []

-- | The plugin-facing publication capability for this inspector's registry.
inspectorPublisher' :: Inspector -> InspectPublisher
inspectorPublisher'=inspectPublisher . inspectorViews

-- | The menu contribution rows windows reference, set by its owner and cleared
-- on teardown. Rows windows opened while it is empty simply offer no actions.
setOpenMenu :: Inspector -> [Menu.MenuRef] -> IO ()
setOpenMenu inspector=writeIORef (inspectorMenu inspector)

-- | The action recorded for this exact window and row, if the window is still
-- remembered. At most 64 live windows are retained; further openings are refused.
pageAction :: Inspector -> W.WindowRef -> Tree.NodeId -> IO (Maybe RowAction)
pageAction inspector reference node=do
  live<-W.windowRefCurrent reference
  pages<-readIORef (inspectorPages inspector)
  pure (if live then lookup reference pages >>= M.lookup node . fst else Nothing)

-- | Build the rows window for one object and view. @views@ are the other
-- applicable views, offered as navigation rows after the document's own.
buildPage :: [Menu.MenuRef] -> Subject -> Text -> [ViewInfo] -> PreparedDoc -> IO (Either Text Page)
buildPage refs subject currentTitle others prepared=buildPageWithBack refs subject currentTitle others prepared False

buildPageWithBack :: [Menu.MenuRef] -> Subject -> Text -> [ViewInfo] -> PreparedDoc -> Bool -> IO (Either Text Page)
buildPageWithBack refs subject currentTitle others prepared hasBack=do
  let docRows=case preparedDoc prepared of
        List _ items->[(itemLabel item,itemDetail item,fmap action (itemTarget item)) | item<-items]
        Text blocks->[(heading,body,Nothing) | (heading,body)<-textRows blocks]
      viewRows=[("→ "<>T.take 200 (infoTitle view)<>" view","Show the "<>infoTitle view<>" view of this "<>typeName<>".",Just (ShowView subject (infoView view)))
               | view<-take 8 others]
      backRows=[("← Back","Return to the previous inspected object.",Just GoBack) | hasBack]
      room=64-length viewRows-length backRows
      kept=if length docRows<=room then docRows
            else take (room-1) docRows++[("… more rows","Some rows are not shown.",Nothing)]
      rows=(if null kept && null viewRows then [("No content","This view produced no rows.",Nothing)] else kept)++viewRows++backRows
      title=case preparedDoc prepared of
        List heading _ | not (T.null heading)->heading
        _->typeName<>" · "<>currentTitle
  built<-mapM detail (zip [1::Int ..] rows)
  case sequence [row | (row,_)<-built] of
    Left err->pure (Left err)
    Right windowRows->do
      window<-W.prepareRowsWindow (T.take 240 title) refs windowRows
      pure $ case window of
        Left err->Left err
        Right value->Right (Page value (M.fromList [(node,action') | (Right (W.WindowRow node _ _),Just action')<-built]))
  where
    typeName=T.pack (show (subjectType subject))
    action (Inspect target)=Navigate target
    action (PageAt target view offset)=PageRows target view offset
    action (OpenFile path)=OpenPath path
    action (OpenAt path row column expected)=OpenSite path row column expected
    detail (index,(label,body,act))=do
      node<-pure (Tree.nodeId (T.pack (maybe "r" (const "a") act<>show index)))
      text<-W.prepareTextWindow "Details" (T.take maxDetail body)
      pure (either Left (\ident->Right (W.WindowRow ident (T.take 240 label) text)) node,act)

-- Group a text document into captioned rows: a heading opens a row, prose and
-- facts extend the current row's Details. Content before any heading goes in an
-- "Overview" row.
textRows :: [Block] -> [(Text,Text)]
textRows=map finish . reverse . foldl accumulate []
  where
    accumulate rows (Heading text)=(text,[]):rows
    accumulate [] other=accumulate [("Overview",[])] other
    accumulate ((caption,parts):rows) (Prose text)=(caption,parts++[text]):rows
    accumulate ((caption,parts):rows) (Facts facts)=(caption,parts++[key<>": "<>value | (key,value)<-facts]):rows
    finish (caption,parts)=(caption,T.intercalate "\n" parts)

-- | Render one object in the requested view, or its first applicable view, as an
-- unadopted window. Every document is bounded and forced here;
-- caller-owned admission determines where it may be published.
inspectSubject :: Inspector -> Menu.MenuOrigin -> Subject -> Maybe ViewId -> IO (Either Text W.WindowUpdate)
inspectSubject inspector context subject requested=inspectWithHistory inspector context subject requested 0 []

-- History belongs to the exact page, never a session-global current object.
-- It retains at most 32 typed locations; revisiting always resolves live roots.
inspectWithHistory :: Inspector -> Menu.MenuOrigin -> Subject -> Maybe ViewId -> Int -> [(Subject,Maybe ViewId,Int)] -> IO (Either Text W.WindowUpdate)
inspectWithHistory inspector context subject requested offset history
  | context/=Menu.HumanMenu=pure (Left "Inspector windows require human input.")
  | otherwise=do
    available<-applicableViews (inspectorViews inspector) subject
    refs<-readIORef (inspectorMenu inspector)
    case (requested,available) of
      (Just wanted,_) | wanted `notElem` map infoView available->pure (Left "That view does not apply to this object.")
      (_,[])->publish refs "No views" [] (List "" [Item ("No views are attached to "<>T.pack (show (subjectType subject))) "" Nothing])
      (_,first:_)->do
        let current=maybe first id (find (\view->Just (infoView view)==requested) available)
        prepared<-openPage (inspectorViews inspector) subject (infoView current) offset
        either (pure . Left . T.pack . show) (publishReady refs (infoTitle current) [view | view<-available,infoView view/=infoView current]) prepared
  where
    publish refs title others value=do
      prepared<-prepareDoc value
      either (pure . Left . T.pack . show) (publishReady refs title others) prepared
    publishReady refs title others ready=do
      built<-buildPageWithBack refs subject title others ready (not (null history))
      case built of
        Left err->pure (Left err)
        Right (Page window actions)->do
          update<-W.openWindow (inspectorScope inspector) window
          case update of
            Nothing->pure (Left "Inspector scope closed.")
            Just opened->do
              kept<-remember (W.updateWindowRef opened) actions
              if kept then pure (Right opened) else do
                W.retireWindowRef (W.updateWindowRef opened)
                pure (Left "Inspector window budget reached; close an inspector first.")
    remember reference actions=do
      snapshot<-readIORef (inspectorPages inspector)
      dead<-map fst <$> filterM (fmap not . W.windowRefCurrent . fst) snapshot
      atomicModifyIORef' (inspectorPages inspector) $ \pages->
        let live=filter (\(ref,_)->ref `notElem` dead) pages
            trail=take 32 ((subject,requested,offset):history)
        in if length live>=64 then (live,False)
           else length trail `seq` ((reference,(actions,trail)):live,True)

-- | Follow an exact live row, preserving history across navigation and view
-- switches. Back consumes one location, so it cannot create a Back/Back loop.
followInspectorRow :: Inspector -> Menu.MenuOrigin -> W.WindowRef -> Tree.NodeId -> IO (Either Text W.WindowUpdate)
followInspectorRow inspector context reference node=do
  followed<-followInspectorAction inspector context reference node
  pure $ case followed of
    Right (Opened update)->Right update
    Right Jump{}->Left "That row opens a source location, not an inspector page."
    Left err->Left err

-- | As 'followInspectorRow', but a source-location row yields 'Jump' once its
-- page's own subject still resolves. The host verifies the location text.
followInspectorAction :: Inspector -> Menu.MenuOrigin -> W.WindowRef -> Tree.NodeId -> IO (Either Text Followed)
followInspectorAction inspector context reference node
  | context/=Menu.HumanMenu=pure (Left "Inspector navigation requires human input.")
  | otherwise=do
      action<-pageAction inspector reference node
      pages<-readIORef (inspectorPages inspector)
      let trail=maybe [] snd (lookup reference pages)
      case action of
        Just (Navigate subject)->enter subject Nothing 0 trail
        Just (ShowView subject view)->enter subject (Just view) 0 (drop 1 trail)
        Just (PageRows subject view offset)->do
          version<-currentRevision (inspectorViews inspector) subject
          if version/=Just (subjectRevision subject) then pure (Left "The source changed; reopen the inspector.")
            else enter subject (Just view) offset trail
        Just GoBack->case drop 1 trail of
          (subject,view,offset):earlier->enter subject view offset earlier
          []->pure (Left "No earlier inspector location.")
        Just (OpenPath _)->pure (Left "Open files from the Files or Package tree.")
        Just (OpenSite path row column expected)->case trail of
          (subject,_,_):_->do
            resolved<-resolve (inspectorViews inspector) subject
            pure $ case resolved of
              Fresh _->Right (Jump path row column expected)
              Gone->Left "That object no longer exists; reopen the inspector."
              RootRetired->Left "That object's source is no longer available."
          []->pure (Left "Inspector row expired; reopen it.")
        Nothing->pure (Left "Inspector row expired; reopen it.")
  where
    enter subject view offset earlier=do
      resolved<-resolve (inspectorViews inspector) subject
      case resolved of
        Gone->pure (Left "That object no longer exists; reopen the inspector.")
        RootRetired->pure (Left "That object's source is no longer available.")
        Fresh current->fmap Opened <$> inspectWithHistory inspector context current view offset earlier
