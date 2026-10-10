{-# LANGUAGE OverloadedStrings #-}
module AccessibilityCheck (checks) where

import Control.Monad (unless,forM_)
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
import Hide.Buffer (newBuffer,Selection(..),Buffer(saved,undoStack,redoStack))
import Hide.BufferView (BufferView(..))
import Hide.Browser (Entry(..))
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
      -- Privacy may inspect document metadata, but never its contents/history.
      privateDoc=(newDocument (newBuffer "") Nothing)
        {documentPrivate=True,documentOrigin=Just "/private-draft.txt",documentBuffer=error "semantics forced buffer contents"}
      poison=base {windows=error "semantics forced windows",buffers=M.singleton 1 privateDoc,
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
  dialogChecks
  sourceChecks
  putStrLn "sidebar and dialog semantic checks passed"


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

-- Semantics are a bounded read of what the current modal paints, with the same
-- privacy as pixels and no action capability hidden in its identities or state.
dialogChecks :: IO ()
dialogChecks=do
  let base=initialDesktop (80,25)
      modal fs chosen=base {dialog=Just (Dialog "Current controls" Widgets fs chosen ["OK","Cancel"] ["Visible explanation"])}
      byId :: [T.Text] -> Value -> Value
      byId ident snapshot=fromMaybe (error ("missing dialog node "++show ident))
        (lookup ident [(fromMaybe [] (field "id" item),item) | item<-items snapshot])
      fieldId n=["dialog","field",T.pack (show (n::Int))]
      project=dialogSemantics OwnerSemantics
      visible=modal [Input "Name" "λ-current" 9,CheckBox "Enabled" True,Radio "Mode" ["Slow","Fast"] 1] 0
      value=project visible
      edited=visible {dialog=fmap (\dg->dg {fields=Input "Name" "updated" 7:drop 1 (fields dg)}) (dialog visible),screenSize=(92,31)}
  check "dialog dismissal is an explicit complete empty snapshot"
    (field "present" (project base)==Just False && null (items (project base)))
  check "modal exposes actual current field values, focus and control state read-only"
    (field "present" value==Just True && field "readOnly" value==Just True &&
     field "value" (byId (fieldId 0) value)==Just (Just ("λ-current"::T.Text)) &&
     field "focused" (byId (fieldId 0) value)==Just True &&
     field "checked" (byId (fieldId 1) value)==Just (Just True))
  check "dialog field identity survives typing and resizing without encoding contents"
    (field "id" (byId (fieldId 0) (project edited))==Just (fieldId 0) &&
     all (all (`notElem` ["λ-current","updated","Current controls"]) . (fromMaybe [] . field "id" :: Value -> [T.Text])) (items value))
  check "modal node bounds are positive and clipped to the screen"
    (all (within (80,25)) (items value) && all (within (12,7)) (items (project visible {screenSize=(12,7)})))
  let credentials=modal [Input "API token" "NEVER-PUBLISH" 13,CheckBox "Streamer mode" True] 0
      safe=dialogSemantics GuestSemantics credentials
  check "central value privacy masks secrets and hidden checkbox state but preserves allowed labels"
    (not ("NEVER-PUBLISH" `BS.isInfixOf` BL.toStrict (encode safe)) &&
     field "value" (byId (fieldId 0) safe)==Just Null &&
     field "checked" (byId (fieldId 1) safe)==Just Null &&
     "API token" `elem` names safe && safe==project credentials {streamerMode=True})
  let approval=base {dialog=Just (Dialog "PRIVATE TITLE" (PermissionDialog "approve:42")
        [ReadOnly "PRIVATE LABEL" "PRIVATE VALUE"] 0 ["PRIVATE BUTTON"] ["PRIVATE BODY"])}
      hidden=dialogSemantics GuestSemantics approval
  check "a guest-hidden approval emits only modal presence, never title/body/labels or state"
    (field "present" hidden==Just True && null (items hidden) &&
     not ("PRIVATE" `BS.isInfixOf` BL.toStrict (encode hidden)))
  let poison=(newBuffer "skip\nVISIBLE\t界\nlast")
        {saved=error "dialog forced saved text",undoStack=error "dialog forced Undo",redoStack=error "dialog forced Redo"}
      area=modal [TextArea "Source" True poison (Selection 0 0) 1 0] 0
      areaValue=byId (fieldId 0) (project area)
  check "text area borrows visible measured rows without saved text or Undo"
    (field "multiline" areaValue==Just True && maybe False (T.isPrefixOf "VISIBLE") (field "value" areaValue::Maybe T.Text) &&
     not ("skip" `BS.isInfixOf` BL.toStrict (encode (project area))))
  let horizontal=modal [TextArea "Source" True (newBuffer (T.replicate 100000 "x"<>"VISIBLE")) (Selection 0 0) 0 100000] 0
  check "long edited-row horizontal text is sought through the measured source window"
    (maybe False (T.isPrefixOf "VISIBLE") (field "value" (byId (fieldId 0) (project horizontal))::Maybe T.Text))
  let dropdown=modal [ComboBox "Compiler" ["One","Two","Three"] 0 (Just 1),Input "Covered" "COVERED-SECRET" 0] 0
      projected=project dropdown
      option=byId ["dialog","field","0","option","1"] projected
  check "combo preview exposes its actual visible choices without covered underlying text"
    (field "name" option==Just ("Two"::T.Text) && field "selected" option==Just (Just True) &&
     not ("COVERED-SECRET" `BS.isInfixOf` BL.toStrict (encode projected)))
  let files=base {guestPrivatePaths=["/authority"],dialog=Just (Dialog "Open" (Opening "/authority" "*" [])
        [FileList [Entry "secret.txt" False Nothing Nothing] 0] 0 ["Open"] [])}
  check "file chooser option names inherit canonical protected-path masking"
    (not ("secret.txt" `BS.isInfixOf` BL.toStrict (encode (dialogSemantics GuestSemantics files))))
  forM_ [(80,25),(160,50)] $ \size->do
    let entries=[Entry (T.pack (show n)<>".hs") False Nothing Nothing | n<-[0..99::Int]]
        chooser=openBrowser "/public" "*" entries (initialDesktop size)
        page=chooser {dialog=fmap (\picker->picker {fields=[Input "Name" "*" 1,FileList entries 45]}) (dialog chooser)}
        dg=fromMaybe (error "missing picker") (dialog page)
        options=[item | item<-items (project page),field "role" item==Just ("option"::T.Text)]
        agrees item=case field "bounds" item of
          Just [x,y,_,_] | Just (_,index)<-fileEntryAt x y page dg -> field "name" item==Just (entryName (entries !! index))
          _->False
    check "resized picker accessibility options match hit testing on later pages"
      (length options==2*fileListRows (fieldRects page dg !! 1) && all agrees options)
  let clipped=modal [Input "Offscreen" "OFFSCREEN-VALUE" 0,Input "Focused" "shown" 0] 1
      shifted=project clipped {screenSize=(30,8)}
  check "scrolled-off modal fields do not publish their values"
    (not ("OFFSCREEN-VALUE" `BS.isInfixOf` BL.toStrict (encode shifted)))
  let manyFields=modal [CheckBox ("Field "<>T.pack (show i)) False | i<-[0::Int ..199]] 199
      manyProjection=project manyFields
  check "field budgeting follows the viewport rather than discarding a late focused field"
    (field "focused" (byId (fieldId 199) manyProjection)==Just True && length (items manyProjection)<=256)
  let large=base {screenSize=(200,1000),dialog=Just (Dialog "Large" Widgets
        [Radio "Options" [T.replicate 120 "v" | _<-[1::Int ..1000]] 0] 0 ["Close"] [])}
      bounded=project large
      textSize=sum [T.length (nameOf n)+maybe 0 T.length (field "value" n::Maybe T.Text) | n<-items bounded]
  check "modal metadata stays bounded independently of oversized field catalogues"
    (field "truncated" bounded==Just True && length (items bounded)<=256 && textSize<=32768 && all (within (200,1000)) (items bounded))


sourceChecks :: IO ()
sourceChecks=do
  let original=newBuffer "alpha 中é🙂\nsecond\nthird"
      poisoned=original {saved=error "source excerpt forced saved text",undoStack=error "source excerpt forced Undo",redoStack=error "source excerpt forced Redo"}
      base=addDocument Nothing poisoned (initialDesktop (80,25))
      ready=base {windows=map (\w->w {bounds=Rect 2 2 28 7}) (windows base)}
      project=sourceSemantics OwnerSemantics
      shown=project ready
      text value=fromMaybe "" (field "value" value::Maybe T.Text)
      absent d=field "present" (project d)==Just False
      moved=ready {windows=map (\w->w {bounds=Rect 3 3 24 5,scrollRow=1}) (windows ready)}
  check "source semantics reads visible Unicode without saved text or histories"
    (text shown=="alpha 中é🙂\nsecond\nthird" && field "firstLine" shown==Just (1::Int) &&
      field "lineCount" shown==Just (3::Int) && field "readOnly" shown==Just True)
  check "source identity survives move and scrolling while the excerpt follows"
    ((field "id" shown::Maybe [T.Text])==field "id" (project moved) && text (project moved)=="second\nthird" &&
      field "bounds" (project moved)==Just ([4,4,22,3]::[Int]) && field "firstLine" (project moved)==Just (2::Int))
  let sibling=ready {windows=case windows ready of w:_->w {windowId=42}:windows ready; _->[]}
  check "split source views have distinct identity even for the same buffer"
    ((field "id" (project sibling)::Maybe [T.Text])/=field "id" shown)
  check "source excerpt clears under menus, modals, pane focus and alternate views"
    (all absent [ready {menu=Just (0,0)},ready {contextMenu=Just (Rect 0 0 1 1,0)},
      ready {dialog=Just (Dialog "Covered" Widgets [] 0 ["OK"] [])},ready {problemsFocused=True},
      ready {windows=map (\w->w {bufferView=ChangesView}) (windows ready)}])
  let hidden=ready {guestPrivatePaths=["/authority"],buffers=M.map (\doc->doc
        {documentOrigin=Just "/authority/source.hs",documentSuggestedName=Just (error "private title read"),
         documentBuffer=error "private source payload read"}) (buffers ready)}
  check "source privacy precedes both title and payload"
    (field "present" (sourceSemantics GuestSemantics hidden)==Just False && absent hidden {streamerMode=True})
  let publicPrivate=ready {guestPrivatePaths=["/authority"],buffers=M.map (\doc->doc {documentOrigin=Just "/authority/source.hs"}) (buffers ready)}
  check "owner may read private source with streamer mode off"
    (text (project publicPrivate)==text shown)
  let partial=addDocument Nothing (newBuffer "中x") (initialDesktop (80,25))
      partialView=partial {windows=map (\w->w {bounds=Rect 2 2 4 4,scrollColumn=1}) (windows partial)}
  check "source clipping never reveals a partial wide glyph"
    (text (project partialView)==" x" && field "firstColumn" (project partialView)==Just (1::Int))
  let long=addDocument Nothing (newBuffer (T.replicate 100000 "a"<>"TARGET\nnext")) (initialDesktop (80,25))
      sought=long {windows=map (\w->w {bounds=Rect 2 2 14 3,scrollColumn=100000}) (windows long)}
  check "source horizontal seek returns only its viewport"
    (text (project sought)=="TARGET")
  let crlf=addDocument Nothing (newBuffer (T.replicate 1000 "a"<>"END\r\nnext")) (initialDesktop (80,25))
      crlfView=crlf {windows=map (\w->w {bounds=Rect 2 2 14 3,scrollColumn=1000}) (windows crlf)}
  check "chunked source strips CRLF storage terminators" (text (project crlfView)=="END")
  let unusual=addDocument Nothing (newBuffer ("x"<>T.replicate 100000 "\r"<>"y")) (initialDesktop (80,25))
      bounded=project unusual
  check "zero-width source items cannot evade the per-row work budget"
    (text bounded=="x" && field "truncated" bounded==Just True)
  let tabs=addDocument Nothing (newBuffer "\ta\7b") (initialDesktop (80,25))
  check "source excerpt expands tabs and uses the source control placeholder"
    (text (project tabs)=="        a·b")
  let dense=addDocument Nothing (newBuffer (T.intercalate "\n" (replicate 300 (T.replicate 600 "a")))) (initialDesktop (800,400))
      full=dense {windows=map (\w->w {bounds=Rect 0 1 700 350}) (windows dense)}
      limited=project full
  check "source projection enforces combined text and row budgets"
    (T.length (text limited)<=32768 && maybe False (<=256) (field "lineCount" limited::Maybe Int) && field "truncated" limited==Just True)
  check "terminal output does not masquerade as source accessibility"
    (absent ready {buffers=M.map (\doc->doc {documentLabel=Just "Terminal 1"}) (buffers ready)})
