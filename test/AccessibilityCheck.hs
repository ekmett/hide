{-# LANGUAGE OverloadedStrings #-}
module AccessibilityCheck (checks) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as Lazy
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hide.Accessibility
import Hide.Model
import Hide.Plugin.Command (withRegistry)
import Hide.Plugin.Tree
import Hide.Sidebar
import SidebarFixture

checks :: IO ()
checks=do
  base<-sidebarFixture "/public" [("one.hs","/public/one.hs"),("secret.json","/authority/secret.json"),("three.hs","/public/three.hs")]
    (initialDesktop (80,25)) {guestPrivatePaths=["/authority"],streamerMode=False}
  let tree=fromMaybe (error "missing fixture tree") (sideTree base)
      project audience=sidebarSemantics audience
      owner=project OwnerSemantics base
      guest=project GuestSemantics base
      root=byName "Sidebar" owner
      byName name value=fromMaybe (error ("missing semantic node "++T.unpack name))
        (lookup name [(nameOf item,item) | item<-items value])
      one=byName "one.hs" owner
      files=byName "Files" owner
      treeId=field "id" files::Maybe [T.Text]
      oneId=field "id" one::Maybe [T.Text]
      shifted=base {sideTree=Just tree {treeScroll=2,treeSelected=2,treeFocused=True},screenSize=(20,7)}
      shiftedProjection=project OwnerSemantics shifted
  check "owner sees the prepared sidebar while guest independently masks protected paths"
    ("secret.json" `elem` names owner && "secret.json" `notElem` names guest && "one.hs" `elem` names guest)
  let secretKey=keyOf (rowHit (fromMaybe (error "missing private row") (rowAt 2 tree)))
      reannotated=base {sideTree=Just tree {treeNodes=M.adjust (\state->state {stateInfo=(stateInfo state) {infoResource=Just "/public/replacement"}}) secretKey (treeNodes tree)}}
  check "pending provider updates cannot unmask a protected cached painted row"
    ("secret.json" `notElem` names (project GuestSemantics reannotated))
  check "streamer display uses the same central private-path mask"
    (project OwnerSemantics base {streamerMode=True}==guest)
  check "semantic wire never serializes filesystem resource annotations"
    (all (\path->not (path `BS.isInfixOf` BL.toStrict (encode owner))) ["/public","/authority"])
  check "semantic projection is read-only and uses scoped provider/node identity"
    (field "readOnly" owner==Just True && field "id" root==Just (["sidebar"]::[T.Text]) &&
     case oneId of Just ["tree",scope,opaque]->T.length scope==48 && T.all (`elem` ("0123456789."::String)) opaque; _->False)
  check "offscreen ancestors retain identity and have no invented visible bounds"
    (field "id" (byName "Files" shiftedProjection)==treeId &&
     field "bounds" (byName "Files" shiftedProjection)==Just Null)
  check "scrolling and selection change the layout while provider revision stays stable"
    ((field "revision" shiftedProjection::Maybe Integer)==field "revision" owner &&
     (field "layout" shiftedProjection::Maybe [Int])/=field "layout" owner)
  check "selected/focused node and bounds describe the actual visible row"
    (let selected=byName "secret.json" shiftedProjection in field "selected" selected==Just True &&
     field "focused" selected==Just True && field "bounds" selected==Just ([1,2,19,1]::[Int]))
  check "partial trees carry exact prepared level and sibling ordinals"
    (field "level" one==Just (2::Int) && field "posInSet" one==Just (1::Int) && field "setSize" one==Just (3::Int))
  let renamed=base {sideTree=Just tree {treeNodes=M.adjust (\state->state {stateInfo=(stateInfo state) {infoLabel="renamed.hs"}})
        (keyOf (rowHit (fromMaybe (error "missing child") (rowAt 1 tree)))) (treeNodes tree)}}
  check "pending provider metadata does not rename the still-painted cached row"
    ("one.hs" `elem` names (project OwnerSemantics renamed) && "renamed.hs" `notElem` names (project OwnerSemantics renamed))
  let renamedTree=fromMaybe (error "missing renamed tree") (sideTree renamed)
  renamedReady<-prepareProjection renamedTree
  check "prepared label changes preserve exact node identity"
    (field "id" (byName "renamed.hs" (project OwnerSemantics renamed {sideTree=Just (adoptProjection renamedReady renamedTree)}))==oneId)
  let rootKey=keyOf (rowHit (fromMaybe (error "missing root") (rowAt 0 tree)))
      guarded=base {sideTree=Just tree {treeNodes=M.adjust (\state->state {stateInfo=(stateInfo state) {infoResource=Just "/authority/private-root"}}) rootKey (treeNodes tree)}}
  check "a protected ancestor removes its whole semantic subtree"
    (names (project GuestSemantics guarded)==["Sidebar"])
  mapM_ (\hidden->check "modal or menu ownership suppresses the covered sidebar"
    (null (items (project OwnerSemantics hidden))))
    [base {dialog=Just (Dialog "Question" Widgets [] 0 ["OK"] [])},base {menu=Just (0,0)},
     base {contextMenu=Just (Rect 1 2 4 3,0)},base {sideTree=Nothing}]
  let (replacing,maybeReplacement)=requestChildren (nodeHit rootKey (treeNodes tree M.! rootKey)) Nothing tree
      replacementRequest=fromMaybe (error "replacement did not start a request") maybeReplacement
      replacement=either (error . T.unpack) id (adoptPage replacementRequest [] Nothing replacing)
      pending=project OwnerSemantics base {sideTree=Just replacement}
  check "pending replacement keeps painted names but marks stale sibling counts unknown"
    ("one.hs" `elem` names pending && field "posInSet" (byName "one.hs" pending)==Just (1::Int) &&
     field "setSize" (byName "one.hs" pending)==Just (-1::Int))
  replacementReady<-prepareProjection replacement
  check "adopting the replacement retires removed painted nodes"
    ("one.hs" `notElem` names (project OwnerSemantics base {sideTree=Just (adoptProjection replacementReady replacement)}))
  let loadingTree=tree {treeNodes=M.adjust (\state->state {stateLoad=Loading 8 Nothing}) rootKey (treeNodes tree)}
      failedTree=loadingTree {treeNodes=M.adjust (\state->state {stateLoad=Failed "/authority/secret-failure"}) rootKey (treeNodes loadingTree)}
  loadingReady<-prepareProjection loadingTree
  failedReady<-prepareProjection failedTree
  let loading=base {sideTree=Just ((adoptProjection loadingReady loadingTree) {treeScroll=4})}
      failed=base {sideTree=Just ((adoptProjection failedReady failedTree) {treeScroll=4})}
  check "a viewport containing only a state row retains its parent loading semantics"
    (field "loading" (byName "Files" (project GuestSemantics loading))==Just True &&
     field "setSize" (byName "one.hs" (project GuestSemantics base {sideTree=Just loadingTree}))==Just (-1::Int))
  check "failure placeholders never expose provider error strings"
    (not ("secret-failure" `BS.isInfixOf` BL.toStrict (encode (project OwnerSemantics failed))))
  let unseen=keyOf (rowHit (fromMaybe (error "missing unseen row") (rowAt 3 tree)))
      poison=base {windows=error "semantics forced windows",buffers=error "semantics forced buffers",
        pluginWindows=error "semantics forced plugin windows",editorDrafts=error "semantics forced drafts",
        diagnostics=error "semantics forced diagnostics",sideTree=Just tree {treeScroll=0,
          treeRows=Lazy.adjust (const (error "semantics forced an offscreen row")) [0,2] (treeRows tree),
          treeNodes=Lazy.adjust (const (error "semantics forced an unrelated node")) unseen (treeNodes tree)},screenSize=(20,6)}
  check "bounded viewport projection leaves unrelated rows and editor payloads untouched"
    (BL.length (encode (project GuestSemantics poison))>0)
  bounded<-sidebarFixture "/public" [(T.pack (show i),"/public/item-"++show i) | i<-[1::Int ..128]] (initialDesktop (512,1000))
  let boundedProjection=project OwnerSemantics bounded
  check "projection has fixed row/node caps and clips bounds to the grid"
    (field "visibleCount" boundedProjection==Just (256::Int) && length (items boundedProjection)<=512 &&
     all (within (512,1000)) (items boundedProjection) && all (within (20,7)) (items shiftedProjection))
  privateIdentityChecks
  putStrLn "sidebar semantic checks passed"


-- Provider-local IDs are routing payloads, not publication identities. Sessions
-- uses a full secret session key; arbitrary plugins may use paths or other data.
privateIdentityChecks :: IO ()
privateIdentityChecks=withRegistry $ \registry->do
  let sessionKey=T.replicate 48 "a"
      pathKey="/authority/private-node-identity"
      ident=either (error . T.unpack) id . nodeId
      rootInfo=NodeInfo (ident pathKey) "Independent provider" "" True Nothing
      infos=[NodeInfo (ident ("session:"<>sessionKey)) "Saved session" "" False Nothing,
        NodeInfo (ident ("private:"<>pathKey)) "Provider-local path" "" False Nothing]
      root=NodeDef rootInfo Nothing []
      definitions=map (\info->NodeDef info Nothing []) infos
      right=either (error . show) id
  provider<-right <$> registerTree registry "semantic.private-identities" root (\() _->pure (Right (NodePage definitions Nothing)))
  let ref=treeReference provider
      rootKey=NodeKey ref (infoId rootInfo)
      mounted=addRoot ref rootInfo Nothing [] (emptySidebar "/public" 30 False)
      replace values tree=do
        let (loading,maybeRequest)=requestChildren (nodeHit rootKey (treeNodes tree M.! rootKey)) Nothing tree
            request=fromMaybe (error "identity fixture did not start a request") maybeRequest
        loaded<-either (error . T.unpack) pure (adoptPage request [(info,Nothing,[]) | info<-values] Nothing loading)
        prepared<-prepareProjection loaded
        pure (adoptProjection prepared loaded)
      project tree=sidebarSemantics GuestSemantics (installSidebar tree (initialDesktop (80,25)))
      nodeIdentity name tree=fromMaybe (error "missing private-ID semantic node") $ lookup name
        [(nameOf value,fromMaybe [] (field "id" value)::[T.Text]) | value<-items (project tree)]
  initial<-replace infos mounted
  let encoded=BL.toStrict (encode (project initial))
  check "full session keys and arbitrary private plugin IDs never enter semantic metadata"
    (all (\secret->not (secret `BS.isInfixOf` encoded)) [TE.encodeUtf8 sessionKey,TE.encodeUtf8 pathKey])
  reordered<-replace (reverse infos) initial
  check "node identities survive refresh and sibling movement"
    (all (\name->nodeIdentity name initial==nodeIdentity name reordered) ["Saved session","Provider-local path"] &&
     treeNextWireId initial==treeNextWireId reordered)
  removed<-replace [last infos] reordered
  returned<-replace infos removed
  check "a pruned and reintroduced node receives a new identity without rebinding its sibling"
    (nodeIdentity "Saved session" initial/=nodeIdentity "Saved session" returned &&
     nodeIdentity "Provider-local path" initial==nodeIdentity "Provider-local path" returned)
  check "a fresh sidebar epoch cannot alias previous node identities"
    (nodeIdentity "Saved session" returned/=nodeIdentity "Saved session" returned {treeEpoch=treeEpoch returned+1})

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)
field :: FromJSON a => T.Text -> Value -> Maybe a
field name=parseMaybe (withObject "semantic metadata" (.: fromStringKey name))
  where fromStringKey=Key.fromText
items :: Value -> [Value]
items value=fromMaybe [] (field "nodes" value)
nameOf :: Value -> T.Text
nameOf value=fromMaybe "" (field "name" value)
names :: Value -> [T.Text]
names=map nameOf . items
within :: (Int,Int) -> Value -> Bool
within (cols,rows) value=case field "bounds" value::Maybe (Maybe [Int]) of
  Just Nothing->True
  Just (Just [x,y,w,h])->x>=0 && y>=0 && w>0 && h>0 && x+w<=cols && y+h<=rows
  _->False
