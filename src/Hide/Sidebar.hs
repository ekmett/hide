{-# LANGUAGE OverloadedStrings #-}
-- | Shared sidebar state and its cached, indexed visible projection.
--
-- Input only changes bounded metadata and splices cached subtree spans. Full
-- projection preparation belongs to a host worker; painting and hit testing use
-- indexed rows. Small revisions and exact scoped hits protect delayed loads and
-- actions. Sidebar state contains no provider callback or document payload.
module Hide.Sidebar where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import System.FilePath (addTrailingPathSeparator)
import Hide.Plugin.Command (CommandRef)
import Hide.Plugin.Tree

-- | A key remains stable through row movement; the publication hit also carries
-- a generation, so retained actions cannot bind to later data at the same key.
data NodeKey = NodeKey !TreeRef !NodeId deriving (Eq,Ord,Show)
keyOf :: TreeHit -> NodeKey
keyOf (TreeHit ref ident _)=NodeKey ref ident
data RowKey = NodeRow !NodeKey | StateRow !NodeKey deriving (Eq,Ord,Show)
data LoadState = Unloaded | Loading !Integer !(Maybe Text) | Loaded !(Maybe Text) | Failed !Text deriving (Eq,Show)
data NodeState = NodeState
  { stateInfo :: !NodeInfo, stateGeneration :: !Integer, stateParent :: !(Maybe NodeKey)
  , stateChildren :: !(S.Seq NodeKey), stateChildIndex :: !(M.Map NodeKey ()), stateExpanded :: !Bool, stateLoad :: !LoadState
  , stateRequest :: !Integer, stateAddress :: ![Int], stateAction :: !(Maybe CommandRef), stateActions :: ![(Text,TreeMenuTarget)]
  } deriving (Eq,Show)
data RowAction = ActivateNode | WaitForLoad | RetryLoad | LoadNext !Text deriving (Eq,Show)
data TreeRow = TreeRow
  { rowKey :: !RowKey, rowAddress :: ![Int], rowHit :: !TreeHit, rowInfo :: !NodeInfo, rowDepth :: !Int
  , rowPrefix :: !Text, rowSpan :: !Int, rowExpanded :: !Bool, rowAction :: !RowAction
  , rowCommand :: !(Maybe CommandRef), rowActions :: ![(Text,TreeMenuTarget)]
  } deriving (Eq,Show)
data TreeRequest = TreeRequest
  { requestHit :: !TreeHit, requestGeneration :: !Integer, requestCursor :: !(Maybe Text)
  , requestAncestors :: ![TreeHit]
  } deriving (Eq,Show)
-- | Checkpoint hints deliberately contain no live command/provider reference.
data SidebarHints = SidebarHints !(M.Map FilePath Bool) !(Maybe FilePath) !(Maybe FilePath) deriving (Eq,Show)
data Sidebar = Sidebar
  { treeRoot :: FilePath, treeRows :: !(M.Map [Int] TreeRow), treeSelected :: !Int
  , treeScroll :: !Int, treeWidth :: !Int, treeFocused :: !Bool
  , treeNodes :: !(M.Map NodeKey NodeState), treeRoots :: !(S.Seq NodeKey)
  , treeEpoch :: !Integer, treeRevision :: !Integer, treeProjectionRevision :: !Integer
  , treeHints :: !(Maybe SidebarHints), treeAgentRefs :: ![TreeRef]
  , treeWatchPaths :: ![FilePath], treeBadges :: !(M.Map FilePath (Bool,Int,Int))
  } deriving (Eq,Show)
data Projection = Projection !Integer !(M.Map [Int] TreeRow) !(M.Map RowKey Int) !(M.Map NodeKey NodeState) ![FilePath]

emptySidebar :: FilePath -> Int -> Bool -> Sidebar
emptySidebar path width focused=Sidebar path M.empty 0 0 width focused M.empty S.empty 1 0 (-1) Nothing [] [] M.empty
-- | New selection replaces saved viewport intent, leaving unrelated expansions.
-- Advancing metadata revision also rejects any in-flight recovery projection.
dismissRecoveryAnchors :: Sidebar -> Sidebar
dismissRecoveryAnchors tree=case treeHints tree of
  Just (SidebarHints hints selected top) | selected/=Nothing || top/=Nothing->
    tree {treeHints=Just (SidebarHints hints Nothing Nothing),treeRevision=treeRevision tree+1}
  _->tree
-- | Scrolling supersedes only the saved top row.
dismissRecoveryScroll :: Sidebar -> Sidebar
dismissRecoveryScroll tree=case treeHints tree of
  Just (SidebarHints hints selected (Just _))->
    tree {treeHints=Just (SidebarHints hints selected Nothing),treeRevision=treeRevision tree+1}
  _->tree
-- | A deliberate collapse discards only that branch's deferred expansions.
-- Canonical resource paths let ordered-map splits remove the subtree without
-- filtering every pending path on the input owner.
dismissRecoveryBranch :: FilePath -> Sidebar -> Sidebar
dismissRecoveryBranch path tree=case treeHints tree of
  Just (SidebarHints hints selected top) | M.size remaining/=M.size hints->
    tree {treeHints=Just (SidebarHints remaining selected top),treeRevision=treeRevision tree+1}
    where prefix=addTrailingPathSeparator path
          upper=init prefix++[succ (last prefix)]
          (_,boundary,after)=M.splitLookup upper hints
          suffix=maybe after (\value->M.insert upper value after) boundary
          remaining=M.delete path (M.union (fst (M.split prefix hints)) suffix)
  _->tree

nodeHit :: NodeKey -> NodeState -> TreeHit
nodeHit (NodeKey ref ident) node=TreeHit ref ident (stateGeneration node)
rowAt :: Int -> Sidebar -> Maybe TreeRow
rowAt index tree
  | index<0 || index>=M.size (treeRows tree)=Nothing
  | otherwise=Just (snd (M.elemAt index (treeRows tree)))
-- | A viewport costs logarithmic splits plus its visible row count.
visibleRows :: Int -> Int -> Sidebar -> [(Int,TreeRow)]
visibleRows start count tree=zip [start..] (M.elems (fst (M.splitAt count (snd (M.splitAt start (treeRows tree))))))
nodeAt :: TreeHit -> Sidebar -> Maybe NodeState
nodeAt hit@(TreeHit _ _ generation) tree=do
  node<-M.lookup (keyOf hit) (treeNodes tree)
  if stateGeneration node==generation then Just node else Nothing
-- | Check the complete bounded ancestor trace, including expansion ownership.
hitCurrent :: [TreeHit] -> Sidebar -> Bool
hitCurrent [] _=False
hitCurrent (hit:ancestors) tree=case nodeAt hit tree of
  Nothing->False
  Just _->all (\parent->maybe False stateExpanded (nodeAt parent tree)) ancestors && linked (hit:ancestors)
  where
    linked (child:parent:rest)=case nodeAt parent tree of
      Just node->M.member (keyOf child) (stateChildIndex node) && linked (parent:rest)
      _->False
    linked [top]=maybe False ((==Nothing) . stateParent) (nodeAt top tree) && keyOf top `elem` treeRoots tree
    linked _=False
hitTrace :: NodeKey -> Sidebar -> [TreeHit]
hitTrace key tree=take 65 (go key)
  where go ident=case M.lookup ident (treeNodes tree) of
          Nothing->[]
          Just node->nodeHit ident node:maybe [] go (stateParent node)

-- | Publish prepared root metadata. Provider count and IDs are host-checked.
addRoot :: TreeRef -> NodeInfo -> Maybe CommandRef -> [(Text,TreeMenuTarget)] -> Sidebar -> Sidebar
addRoot ref info action actions tree
  | M.member key (treeNodes tree)=tree
  | S.length (treeRoots tree)>=32 || M.size (treeNodes tree)>=32768=tree
  | otherwise=tree {treeNodes=M.insert key node (treeNodes tree),treeRoots=treeRoots tree S.|> key,treeRevision=treeRevision tree+1}
  where key=NodeKey ref (infoId info)
        node=NodeState info (treeEpoch tree) Nothing S.empty M.empty False Unloaded (treeEpoch tree) [] action actions
removeRoot :: TreeRef -> Sidebar -> Sidebar
removeRoot ref tree
  | null roots=tree
  | otherwise=foldr withdraw marked roots
  where
    roots=[key | key@(NodeKey owner _)<-toList (treeRoots tree),owner==ref]
    marked=tree {treeRoots=S.filter (\(NodeKey owner _)->owner/=ref) (treeRoots tree),treeRevision=treeRevision tree+1}
    withdraw key current=case M.lookup key (treeNodes current) of
      Just node | address@(_:_)<-stateAddress node,Just index<-M.lookupIndex address (treeRows current)->
        let (before,_,rest)=M.splitLookup address (treeRows current)
            (descendants,after)=M.split (address++[maxBound]) rest
            removed=1+M.size descendants
            rows=M.union before after
            adjust position | position>=index && position<index+removed=max 0 (min index (M.size rows-1))
                            | position>=index+removed=position-removed
                            | otherwise=position
        in current {treeRows=rows,treeSelected=adjust (treeSelected current),treeScroll=adjust (treeScroll current)}
      _->current

-- | Expanding twice shares one request. Pages retain their opaque cursor.
requestChildren :: TreeHit -> Maybe Text -> Sidebar -> (Sidebar,Maybe TreeRequest)
requestChildren hit cursor tree=case nodeAt hit tree of
  Just node | infoBranch (stateInfo node), Loading{}<-stateLoad node->(tree,Nothing)
  Just node | infoBranch (stateInfo node)->
    let generation=stateRequest node+1
        opened=node {stateGeneration=stateGeneration node+1,stateExpanded=True,stateLoad=Loading generation cursor,stateRequest=generation}
        changed=tree {treeNodes=M.insert (keyOf hit) opened (treeNodes tree),treeRevision=treeRevision tree+1}
    in (changed,Just (TreeRequest (nodeHit (keyOf hit) opened) generation cursor (hitTrace (keyOf hit) changed)))
  _->(tree,Nothing)
collapseNode :: TreeHit -> Sidebar -> Sidebar
collapseNode hit tree=case nodeAt hit tree of
  Nothing->tree
  Just node->let key=keyOf hit
                 closed=node {stateExpanded=False,stateGeneration=stateGeneration node+1,
                   stateRequest=stateRequest node+1,stateLoad=case stateLoad node of Loading{}->Unloaded; value->value}
             in tree {treeNodes=M.insert key closed (treeNodes tree),treeRevision=treeRevision tree+1}
-- | Prepared ancestry addresses keep descendants contiguous. Two ordered-map
-- splits remove a subtree without scanning it or shifting later row indices.
collapseAt :: Int -> Sidebar -> Sidebar
collapseAt index tree=case rowAt index tree of
  Just row | NodeRow key<-rowKey row, Just node<-M.lookup key (treeNodes tree)->
    let address=rowAddress row
        (before,_,rest)=M.splitLookup address (treeRows tree)
        (descendants,after)=M.split (address++[maxBound]) rest
        removed=M.size descendants
        changed=collapseNode (nodeHit key node) tree
        closed=treeNodes changed M.! key
        adjust position | position>index && position<=index+removed=index
                        | position>index+removed=position-removed
                        | otherwise=position
        rows=M.union before (M.insert address row {rowHit=nodeHit key closed,rowExpanded=False,rowSpan=1} after)
    in changed {treeRows=rows,treeSelected=adjust (treeSelected tree),treeScroll=adjust (treeScroll tree)}
  _->tree

requestCurrent :: TreeRequest -> Sidebar -> Bool
requestCurrent request tree=hitCurrent (requestAncestors request) tree && case nodeAt (requestHit request) tree of
  Just node->stateExpanded node && stateLoad node==Loading (requestGeneration request) (requestCursor request)
  _->False

-- | Adopt at most one prepared bounded page. Structural projection is deferred
-- to its worker. A page may not steal another parent's/provider's node identity.
adoptPage :: TreeRequest -> [(NodeInfo,Maybe CommandRef,[(Text,TreeMenuTarget)])] -> Maybe Text -> Sidebar -> Either Text Sidebar
adoptPage request nodes next tree
  | not (requestCurrent request tree)=Left "Sidebar request expired."
  | length (requestAncestors request)>64=Left "Sidebar depth budget reached."
  | length nodes>128 || M.size (treeNodes tree)+length [() | (info,_,_)<-nodes,not (M.member (NodeKey ref (infoId info)) (treeNodes tree))]>32768=Left "Sidebar node budget reached."
  | any foreignNode nodes=Left "Sidebar node belongs to another parent or is an ancestor."
  | otherwise=Right tree {treeNodes=M.insert parent updated (foldr insert (treeNodes tree) nodes),treeRevision=treeRevision tree+1}
  where
    parent=keyOf (requestHit request)
    NodeKey ref _=parent
    original=treeNodes tree M.! parent
    keys=S.fromList [NodeKey ref (infoId info) | (info,_,_)<-nodes]
    updated=original {stateChildren=if requestCursor request==Nothing then keys else stateChildren original S.>< keys,
      stateChildIndex=if requestCursor request==Nothing then keyIndex else M.union keyIndex (stateChildIndex original),stateLoad=Loaded next}
    keyIndex=M.fromList [(key,()) | key<-toList keys]
    foreignNode (info,_,_)=let key=NodeKey ref (infoId info) in key==parent || case M.lookup key (treeNodes tree) of
      Just previous->stateParent previous/=Just parent || requestCursor request/=Nothing
      Nothing->False
    insert (info,action,actions) values=
      let key=NodeKey ref (infoId info)
          previous=M.lookup key values
          retained=previous >>= \old->if infoBranch info && infoBranch (stateInfo old) && infoResource info==infoResource (stateInfo old) then Just old else Nothing
          node=NodeState info (maybe (treeEpoch tree) ((+1).stateGeneration) previous) (Just parent)
            (maybe S.empty stateChildren retained) (maybe M.empty stateChildIndex retained) (maybe False stateExpanded retained) (maybe Unloaded stateLoad retained)
            (maybe (treeEpoch tree) stateRequest previous) (maybe [] stateAddress previous) action actions
      in M.insert key node values
failRequest :: TreeRequest -> Text -> Sidebar -> Sidebar
failRequest request failure tree
  | not (requestCurrent request tree)=tree
  | otherwise=tree {treeNodes=M.adjust (\node->node {stateLoad=Failed (T.take 512 failure)}) (keyOf (requestHit request)) (treeNodes tree),treeRevision=treeRevision tree+1}

-- | Build once per metadata/expansion revision on a worker. Prefixes and subtree
-- spans are prepared here, so a viewport slice needs no ancestor/sibling walk.
prepareProjection :: Sidebar -> IO Projection
prepareProjection tree=do
  let listing=concat [project [i] 0 "" True key | (i,key)<-zip [0..] (toList (treeRoots tree))]
      rows=M.fromList [(rowAddress row,row) | row<-listing]
      index=M.fromList (zip (map rowKey listing) [0..])
      addresses=M.fromList [(key,rowAddress row) | row<-listing,NodeRow key<-[rowKey row]]
      retained=M.mapWithKey (\key node->node {stateAddress=M.findWithDefault [] key addresses})
        (M.fromList (concatMap retain (toList (treeRoots tree))))
      retain key=case M.lookup key (treeNodes tree) of Nothing->[]; Just node->(key,node):concatMap retain (toList (stateChildren node))
  _<-evaluate (M.size rows)
  _<-evaluate (M.size index)
  _<-evaluate (M.size retained)
  mapM_ (\row->evaluate (force (rowPrefix row,rowSpan row,rowDepth row,infoLabel (rowInfo row)))) listing
  let paths=[path | node<-M.elems retained,stateExpanded node,infoBranch (stateInfo node),Just path<-[infoResource (stateInfo node)]]
  _<-evaluate (force paths)
  pure (Projection (treeRevision tree) rows index retained paths)
  where
    project address depth ancestry lastChild key=case M.lookup key (treeNodes tree) of
      Nothing->[]
      Just node->
        let hit=nodeHit key node
            prefix=if depth==0 then "" else ancestry<>(if lastChild then "└" else "├")
            below=ancestry<>(if lastChild then " " else "│")
            children=if stateExpanded node then concat [project (address++[i]) (depth+1) (if depth==0 then "" else below) (i==S.length (stateChildren node)-1) child | (i,child)<-zip [0..] (toList (stateChildren node))] else []
            placeholder=if stateExpanded node then case stateLoad node of
              Loading{}->[special (address++[S.length (stateChildren node)]) hit node (depth+1) "Loading…" WaitForLoad]
              Failed text->[special (address++[S.length (stateChildren node)]) hit node (depth+1) ("Retry: "<>text) RetryLoad]
              Loaded (Just token)->[special (address++[S.length (stateChildren node)]) hit node (depth+1) "More…" (LoadNext token)]
              Unloaded->[special (address++[S.length (stateChildren node)]) hit node (depth+1) "Load children" RetryLoad]
              _->[]
              else []
            descendants=children++placeholder
            row=TreeRow (NodeRow key) address hit (stateInfo node) depth prefix (1+length descendants) (stateExpanded node) ActivateNode (stateAction node) (stateActions node)
        in row:descendants
    special address hit node depth label action=TreeRow (StateRow (keyOf hit)) address hit ((stateInfo node) {infoLabel=label,infoIcon="",infoBranch=False}) depth (T.replicate (max 0 (depth-1)) " "<>"└") 1 False action Nothing []

-- | Preserve the current selected/top row identities, including scroll changes
-- made after preparation began. Lost descendants fall back to their ancestor.
adoptProjection :: Projection -> Sidebar -> Sidebar
adoptProjection (Projection revision rows index retained paths) tree
  | revision/=treeRevision tree=tree
  | otherwise=tree {treeRows=rows,treeSelected=locate (treeSelected tree),treeScroll=locate (treeScroll tree),treeProjectionRevision=revision,treeNodes=retained,treeWatchPaths=paths}
  where
    locate old=case rowAt old tree of
      Nothing->max 0 (min old (M.size rows-1))
      Just row->fromMaybe (max 0 (min old (M.size rows-1))) (findSurvivor (rowKey row))
    findSurvivor key=case M.lookup key index of
      Just position->Just position
      Nothing->case key of
        StateRow parent->findSurvivor (NodeRow parent)
        NodeRow node->M.lookup node (treeNodes tree) >>= stateParent >>= findSurvivor . NodeRow
