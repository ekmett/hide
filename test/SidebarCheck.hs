{-# LANGUAGE OverloadedStrings #-}
module SidebarCheck (checks) where
import Control.Concurrent (threadDelay)
import Data.IORef
import Control.Concurrent.MVar
import Control.Exception (bracket,evaluate)
import Control.Monad (unless,forM_,void)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Graphics.Vty as V
import System.Directory
import System.FilePath ((</>))
import System.IO (openTempFile,hClose)
import System.Timeout (timeout)
import GHC.Stack (HasCallStack)
import Hide.App (applyEffects)
import Hide.Buffer
import Hide.Browser
import Hide.Files
import Hide.Links (prepareMarkdown)
import Hide.Model
import Hide.Render
import Hide.Sidebar
import Hide.SidebarCommands
import Hide.Reconcile
import Hide.GuestAccess
import qualified Hide.Protocol as Wire
import Hide.Plugin.Command
import qualified Hide.Plugin.Tree as P
import qualified Hide.Plugin.Menu as Menu
import qualified TreeExtension

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)
right :: Show e => Either e a -> IO a
right=either (error.show) pure
treeOf :: Desktop -> Sidebar
treeOf=maybe (error "missing tree") id . sideTree
settle :: SidebarHost -> Desktop -> IO Desktop
settle host=await (tickSidebar host) (\d->let tree=treeOf d in treeProjectionRevision tree==treeRevision tree && all ready (M.elems (treeNodes tree)))
  where ready node=case stateLoad node of Loading{}->False; _->True
await :: HasCallStack => (Desktop -> IO Desktop) -> (Desktop -> Bool) -> Desktop -> IO Desktop
await tick done initial=do
  lastState<-newIORef initial
  result<-timeout 10000000 (goWith lastState initial)
  case result of
    Just value->pure value
    Nothing->do
      latest<-readIORef lastState
      print (status latest,treeRevision (treeOf latest),treeProjectionRevision (treeOf latest),treeWatchPaths (treeOf latest),[(P.infoLabel (stateInfo node),stateGeneration node,stateLoad node) | node<-M.elems (treeNodes (treeOf latest))])
      error "Sidebar timeout"
  where goWith ref value=do next<-tick value; writeIORef ref next; if done next then pure next else threadDelay 10000 >> goWith ref next

act :: SidebarHost -> (Desktop,[Effect]) -> IO Desktop
act host (d,effects)=snd <$> sidebarEffects host applyEffects d effects
atLabel :: T.Text -> Desktop -> Int
atLabel label d=case [i | (i,row)<-visibleRows 0 32768 (treeOf d),P.infoLabel (rowInfo row)==label] of i:_->i; _->error ("missing row "++T.unpack label)
select :: Int -> Desktop -> Desktop
select index d=d {sideTree=Just (treeOf d) {treeSelected=index,treeFocused=True}}

checks :: IO ()
checks=bracket temporary removePathForcibly $ \dir->withSidebarCommands $ \host->do
  createDirectory (dir </> "src")
  TIO.writeFile (dir </> "Main.hs") "main = 1\n"
  TIO.writeFile (dir </> "Readme.md") "# Documentation\n"
  TIO.writeFile (dir </> "thc.toml") "private = true\n"
  initial<-initializeSidebar host (installSidebar (emptySidebar dir 24 True) (initialDesktop (100,30)))
  check "Files is an ordinary first root in the shared tree" (P.infoLabel (rowInfo (maybe (error "root") id (rowAt 0 (treeOf initial))))=="Files")
  collapsed<-act host (handleEvent (V.EvKey V.KEnter []) initial)
  check "Enter collapses Files and removes cached descendants immediately" (M.size (treeRows (treeOf collapsed))==1 && not ("Main.hs" `T.isInfixOf` snapshot collapsed))
  expanded<-act host (handleEvent (V.EvKey V.KRight []) collapsed) >>= settle host
  check "Files root expansion restores children" ("Main.hs" `T.isInfixOf` snapshot expanded)
  check "Files icons and dock arrows survive shared projection" ("📁 src" `T.isInfixOf` snapshot expanded && "[←]" `T.isInfixOf` snapshot expanded && "\xf024b src" `T.isInfixOf` snapshot expanded {materialIcons=True})
  let dormant=expanded {buffers=error "painting inspected buffers",sideTree=Just (treeOf expanded) {treeNodes=M.map (\node->node {stateChildren=S.singleton (error "painting traversed tree")}) (treeNodes (treeOf expanded))}}
  void (evaluate (T.length (snapshot dormant)))
  opened<-act host (activateTree False (atLabel "Main.hs" expanded) expanded)
    >>= await (tickSidebar host) (\d->not (null (windows d)))
  check "Files primary action opens through a typed worker" (activeText opened=="main = 1\n")
  let changed=insertText "local " opened
  dirty<-await (tickSidebar host) (\d->maybe False (\(value,_,_)->value) (M.lookup (dir </> "Main.hs") (treeBadges (treeOf d)))) changed
  check "Files cached badge preserves dirty counts" ("Main.hs +1 -1" `T.isInfixOf` snapshot dirty)
  reopened<-act host (activateTree False (atLabel "Main.hs" dirty) dirty)
    >>= await (tickSidebar host) (\d->not (treeFocused (treeOf d)))
  check "Opening an existing dirty file preserves its live content" (activeText reopened=="local main = 1\n")
  let privateIndex=atLabel "thc.toml" reopened
      privateTree=select privateIndex reopened
  (agentState,agentEffects)<-right (Wire.applyGuestInput (Wire.Key "Enter" []) privateTree)
  check "agent input stamps the real Files action" (case agentEffects of [InvokeTree _ _ Menu.AgentMenu]->True; _->False)
  denied<-act host (agentState,agentEffects)
  check "agent protected Files action refuses before loading" ("protected" `T.isInfixOf` status denied)
  let docs=select (atLabel "Readme.md" expanded) expanded
      rowY=2+treeSelected (treeOf docs)-treeScroll (treeOf docs)
      (popup,_)=handleEvent (V.EvMouseDown 5 rowY V.BRight []) docs
      (_,links)=handleEvent (V.EvKey V.KEnter []) popup
  check "Files secondary Open retains its frozen provider target" (case links of [FollowTreeLink trace path]->path==dir </> "Readme.md" && hitCurrent trace (treeOf popup); _->False)
  let replaced=popup {sideTree=Just (collapseAt 0 (treeOf popup))}
  check "stale Files popup refuses after ancestor collapse" (null (snd (handleEvent (V.EvKey V.KEnter []) replaced)))
  independent host expanded
  refresh host dir expanded
  putStrLn "shared sidebar checks passed"

independent :: SidebarHost -> Desktop -> IO ()
independent host d=withRegistry $ \registry->do
  gate<-newEmptyMVar
  started<-newEmptyMVar
  calls<-newMVar (0::Int)
  leafRef<-newEmptyMVar
  (provider,leaf)<-TreeExtension.declare registry
    (\ctx->SidebarPrepared <$> prepareMarkdown (sidebarColumns ctx) "/extension/help.md" "" "# Independent action\n")
    (\(P.ChildRequest _ token)->do
      modifyMVar_ calls (pure . (+1))
      case token of
        Nothing->do void (tryPutMVar started ()); takeMVar gate; value<-readMVar leafRef; pure (Right (P.NodePage [value] (Just "next")))
        Just "next"->pure (Right (P.NodePage [P.NodeDef (P.NodeInfo (either (error.show) id (P.nodeId "other")) "Other" "" False Nothing) Nothing []] Nothing))
        _->pure (Left (CommandRejected "bad cursor")))
  putMVar leafRef leaf
  publishTreeFromHost host provider
  published<-settle host d
  let index=atLabel "Tools" published
  loading<-act host (activateTree True index published)
  running<-tickSidebar host loading
  takeMVar started
  projected<-await (tickSidebar host) (T.isInfixOf "Loading…" . snapshot) running
  check "independent provider displays actual loading row" ("Loading…" `T.isInfixOf` snapshot projected)
  repeated<-act host (activateTree True index projected)
  count<-readMVar calls
  check "repeated expansion shares one worker" (count==1 && treeRevision (treeOf repeated)==treeRevision (treeOf projected))
  closed<-act host (activateTree False index repeated)
  void (tryPutMVar gate ())
  refused<-settle host closed
  check "collapse refuses the delayed independent page" (not ("Inspect" `T.isInfixOf` snapshot refused))
  void (tryPutMVar gate ())
  loaded<-act host (activateTree True index refused) >>= settle host
  check "independent provider shares the visible Files tree" (all (`T.isInfixOf` snapshot loaded) ["Files","Tools","★ Inspect","More…"])
  let selected=select (atLabel "Inspect" loaded) loaded
      node=maybe (error "leaf") id (rowAt (treeSelected (treeOf selected)) (treeOf selected))
      trace=hitTrace (keyOf (rowHit node)) (treeOf selected)
      reference=maybe (error "action") id (rowCommand node)
  check "independent resource labels cannot grant agent authority" (guestEffectsAllowed [InvokeTree trace reference Menu.AgentMenu])
  protected<-act host (selected,[InvokeTree trace reference Menu.AgentMenu])
  check "host refuses independent agent action" ("protected" `T.isInfixOf` status protected)
  invoked<-act host (Wire.applyInput (Wire.Key "Enter" []) selected)
  displayed<-await (tickSidebar host) (T.isInfixOf "Independent action" . activeText) invoked
  check "independent action runs through actual frontend keyboard route" ("Independent action" `T.isInfixOf` activeText displayed)
  paged<-act host (activateTree False (atLabel "More…" loaded) loaded) >>= settle host
  check "bounded continuation uses the shared More row" ("Other" `T.isInfixOf` snapshot paged && not ("More…" `T.isInfixOf` snapshot paged))
  let late=(select (atLabel "Inspect" paged) paged,[InvokeTree trace reference Menu.HumanMenu])
  retired<-retireTreeFromHost host (P.treeReference provider) paged
  obsolete<-act host (retired,snd late) >>= settle host
  check "retired provider rejects captured invocation and visible rows" (not (any ((=="Tools").P.infoLabel.rowInfo.snd) (visibleRows 0 32768 (treeOf obsolete))) && "expired" `T.isInfixOf` T.toLower (status obsolete))

refresh :: SidebarHost -> FilePath -> Desktop -> IO ()
refresh host dir d=withReconciliation $ \watcher->do
  let nested=dir </> "src"
      tick value=tickReconciliation watcher (sidebarEffects host applyEffects) value >>= tickSidebar host
  TIO.writeFile (nested </> "a.hs") "a"
  opened<-act host (activateTree True (atLabel "src" d) d) >>= settle host
  let selected=select (atLabel "a.hs" opened) opened
      focused=selected {sideTree=Just (treeOf selected) {treeFocused=False}}
  subscribed<-tick focused
  TIO.writeFile (nested </> "b.hs") "b"
  refreshed<-await tick (T.isInfixOf "b.hs" . snapshot) subscribed
  check "directory refresh preserves expansion selection and input owner" (not (treeFocused (treeOf refreshed)) && P.infoLabel (rowInfo (maybe (error "selected") id (rowAt (treeSelected (treeOf refreshed)) (treeOf refreshed))))=="a.hs")

temporary :: IO FilePath
temporary=do root<-getTemporaryDirectory; (path,handle)<-openTempFile root "hide-sidebar"; hClose handle; removeFile path; createDirectory path; canonicalizePath path
