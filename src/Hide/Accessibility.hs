{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Accessibility
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Read-only sidebar semantics from the host's prepared, indexed tree. A complete
-- bounded projection accompanies a frame; it adds no action authority and never
-- reads document payloads. Resource annotations enter central privacy policy but
-- are never serialized.
module Hide.Accessibility
  ( SemanticAudience(..)
  , sidebarSemantics
  ) where

import Data.Aeson (Value, object, (.=))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Hide.GuestAccess (protectedPath)
import Hide.Model (Desktop(..), problemsHeight, treeContentRows)
import Hide.Plugin.Tree
import Hide.Sidebar

-- | Owner display metadata follows streamer mode. Agent screen metadata always
-- applies the same centralized protected-path policy, independently of that mode.
data SemanticAudience = OwnerSemantics | GuestSemantics deriving (Eq,Show)

-- | /O(v * h * log n)/ with at most 256 viewport rows, 65 ancestors per row and
-- 512 emitted nodes, including the host root. IDs survive viewport/selection
-- changes and expire with their actual provider registration/pane lifetime. Private ancestry
-- is omitted as a whole; covered sidebars publish an empty projection.
--
-- Revision tracks provider metadata, while layout also tracks selection, focus
-- and geometry. Consumers replace the complete field whenever it arrives: equal
-- revisions do not imply equal layout or privacy. Offscreen ancestors carry no
-- bounds; there is no offscreen-read API or callable semantic action.
sidebarSemantics :: SemanticAudience -> Desktop -> Value
sidebarSemantics audience d=case sideTree d of
  Nothing->snapshot 0 [cols,rows,0,0,0,0,bottom] 0 0 0 []
  Just tree
    | covered || listWidth<=0 || visible<=0->snapshot (revision tree) (layout tree) 0 0 0 []
    | otherwise->snapshot (revision tree) (layout tree) start visible (M.size (treeRows tree))
        (host tree:map (node tree) (M.toAscList accepted))
    where
      start=max 0 (treeScroll tree)
      visible=min 256 (min (max 0 (rows-2-bottom)) (treeContentRows d))
      listWidth=max 0 (min (cols-1) (treeWidth tree-if treeFocused tree then 3 else 2))
      listing=visibleRows start visible tree
      positions=M.fromList [(key,(index,2+index-start)) | (index,row)<-listing,NodeRow key<-[rowKey row]]
      -- Check only prepared ancestry for viewport rows. Whole-tree equality or
      -- scans would read unrelated payloads and lose the viewport bound.
      accepted=foldl' (include tree) M.empty (map (keyOf . rowHit . snd) listing)
      include current retained key=case reverse (hitTrace key current) of
        []->retained
        trace@(top:_)
          | not (maybe False ((==Nothing) . stateParent) (nodeAt top current))->retained
          | otherwise->case traverse (\hit->(,) (keyOf hit) <$> nodeAt hit current) trace of
              Just chain | all (\(_,state)->allowed (stateInfo state) && maybe True (allowed . rowInfo) (M.lookup (stateAddress state) (treeRows current))) chain,
                let combined=M.union retained (M.fromList chain),M.size combined<=511->combined
              _->retained
      allowed info=not maskPrivate || maybe True (not . protectedPath d) (infoResource info)
      maskPrivate=audience==GuestSemantics || streamerMode d
      host current=object
        ["id" .= (["sidebar"]::[Text]),"parent" .= (Nothing::Maybe [Text]),"role" .= ("tree"::Text),
         "name" .= ("Sidebar"::Text),"bounds" .= ([1,2,listWidth,visible]::[Int]),
         "selected" .= False,"focused" .= treeFocused current,"expanded" .= (Nothing::Maybe Bool),
         "loading" .= False,"childrenKnown" .= S.length (treeRoots current),"moreChildren" .= False,
         "index" .= (Nothing::Maybe Int),"generation" .= (0::Int),
         "level" .= (0::Int),"posInSet" .= (0::Int),"setSize" .= S.length (treeRoots current)]
      node current (key,state)=object
        ["id" .= identity current key state,"parent" .= Just (maybe ["sidebar"] parentIdentity (stateParent state)),
         "role" .= ("treeitem"::Text),"name" .= infoLabel info,
         "bounds" .= fmap (\(_,y)->[1,y,listWidth,1]) position,
         "selected" .= selected,"focused" .= (selected && treeFocused current),
         "expanded" .= (if infoBranch info then Just (maybe (stateExpanded state) rowExpanded cached) else Nothing),
         "loading" .= (case stateLoad state of Loading{}->True; _->False),
         "childrenKnown" .= S.length (stateChildren state),"moreChildren" .= incomplete state,
         "index" .= M.lookupIndex (stateAddress state) (treeRows current),
         "generation" .= safeInteger (stateGeneration state),
         "level" .= length address,"posInSet" .= (case reverse address of i:_->i+1; _->1),
         "setSize" .= siblingCount current state]
        where
          parentIdentity parent=case M.lookup parent (treeNodes current) of
            Just value->identity current parent value
            Nothing->["sidebar"]
          position=M.lookup key positions
          selected=maybe False ((==treeSelected current) . fst) position
          cached=M.lookup (stateAddress state) (treeRows current)
          info=maybe (stateInfo state) rowInfo cached
          address=take 65 (stateAddress state)
  where
    (cols,rows)=screenSize d
    bottom=problemsHeight d
    covered=isJust (dialog d) || isJust (menu d) || isJust (contextMenu d)
    revision=safeInteger . treeRevision
    layout tree=[cols,rows,treeWidth tree,treeScroll tree,treeSelected tree,if treeFocused tree then 1 else 0,bottom]
    snapshot :: Integer -> [Int] -> Int -> Int -> Int -> [Value] -> Value
    snapshot rev geometry start count total nodes=object
      ["revision" .= rev,"layout" .= geometry,"visibleStart" .= start,"visibleCount" .= count,
       "logicalRows" .= total,"readOnly" .= True,"nodes" .= nodes]
    identity tree (NodeKey ref _) state=["tree",treeIdentity ref,T.pack (show (treeEpoch tree))<>"."<>T.pack (show (stateWireId state))]
    incomplete state=case stateLoad state of Loaded Nothing->False; _->infoBranch (stateInfo state)
    siblingCount tree _ | treeProjectionRevision tree/=treeRevision tree= -1
    siblingCount tree state=case stateParent state of
      Nothing->S.length (treeRoots tree)
      Just parent->case M.lookup parent (treeNodes tree) of
        Just node | Loaded Nothing<-stateLoad node->S.length (stateChildren node)
        _-> -1
    safeInteger :: Integer -> Integer
    safeInteger value=max 0 (min 9007199254740991 value)
