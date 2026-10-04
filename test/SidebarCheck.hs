{-# LANGUAGE OverloadedStrings #-}
module SidebarCheck (checks) where
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync,wait,poll)
import Data.IORef
import Control.Concurrent.MVar
import Control.Exception (bracket,evaluate)
import Control.Monad (unless,forM,forM_,void,replicateM_)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Graphics.Vty as V
import System.Directory
import System.FilePath ((</>))
import System.IO (openTempFile,hClose)
import Hide.Recovery (writeCheckpoint,readCheckpoint)
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
  removeFile (dir </> "Main.hs")
  createDirectory (dir </> "Main.hs")
  reopened<-act host (activateTree False (atLabel "Main.hs" dirty) dirty)
    >>= await (tickSidebar host) (\d->not (treeFocused (treeOf d)) || "failed" `T.isInfixOf` status d)
  check "Opening an existing dirty file survives an unreadable disk replacement and preserves its live content" (activeText reopened=="local main = 1\n" && not (treeFocused (treeOf reopened)))
  removeDirectory (dir </> "Main.hs")
  TIO.writeFile (dir </> "Main.hs") "main = 1\n"
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
  check "Files secondary Open retains its frozen provider target" (case links of [FollowTreeLink trace path ""]->path==dir </> "Readme.md" && hitCurrent trace (treeOf popup); _->False)
  let replaced=popup {sideTree=Just (collapseAt 0 (treeOf popup))}
  check "stale Files popup refuses after ancestor collapse" (null (snd (handleEvent (V.EvKey V.KEnter []) replaced)))
  independent host expanded
  refresh host dir expanded
  edgeChecks dir
  recoveryChecks dir
  budgetChecks dir
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
  let targetIndex=atLabel "Inspect" loaded
      (secondaryPopup,_)=Wire.applyInput (Wire.Mouse "down" 5 (2+targetIndex-treeScroll (treeOf loaded)) 2 1 []) loaded
  check "independent secondary actions join the actual shared popup" (map fst (contextItemsFor secondaryPopup)==["Inspect details","Read documentation"])
  let (_,resourceEffects)=handleEvent (V.EvKey V.KEnter []) (fst (handleEvent (V.EvKey V.KDown []) secondaryPopup))
  check "independent resource declaration reuses captured link transport" (case resourceEffects of [FollowTreeLink trace "/extension/help.md" "#details"]->hitCurrent trace (treeOf secondaryPopup); _->False)
  secondaryInvoked<-act host (Wire.applyInput (Wire.Key "Enter" []) secondaryPopup)
  _<-await (tickSidebar host) (T.isInfixOf "Independent action" . activeText) secondaryInvoked
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

-- Same live route for failure/retry, queued retirement and owning scope closure.
edgeChecks :: FilePath -> IO ()
edgeChecks dir=withSidebarCommands $ \host->do
  base<-initializeSidebar host (installSidebar (emptySidebar dir 24 True) (initialDesktop (100,30)))
  withRegistry $ \registry->do
    replyGate<-newEmptyMVar
    actionEntered<-newEmptyMVar
    leafRef<-newEmptyMVar
    count<-newMVar (0::Int)
    (provider,leaf)<-TreeExtension.declare registry
      (\ctx->void (tryPutMVar actionEntered ()) >> takeMVar replyGate >> SidebarPrepared <$> prepareMarkdown (sidebarColumns ctx) "/extension/help.md" "" "# Retired action\n")
      (\_ ->do
        next<-modifyMVar count (\value->pure (value+1,value+1))
        if next==1 then pure (Left (CommandRejected "fixture unavailable")) else do
          node<-readMVar leafRef
          pure (Right (P.NodePage [node] Nothing)))
    putMVar leafRef leaf
    publishTreeFromHost host provider
    published<-settle host base
    failed<-act host (activateTree True (atLabel "Tools" published) published) >>= settle host
    check "provider failures produce a real Retry row" ("Retry:" `T.isInfixOf` snapshot failed && any (T.isInfixOf "fixture unavailable".P.infoLabel.rowInfo.snd) (visibleRows 0 32768 (treeOf failed)))
    recovered<-act host (activateTree False (case [index | (index,row)<-visibleRows 0 32768 (treeOf failed),rowAction row==RetryLoad] of index:_->index; _->error "missing Retry row") failed) >>= settle host
    check "Retry invokes the same provider and replaces the failed row" ("Inspect" `T.isInfixOf` snapshot recovered && not ("Retry:" `T.isInfixOf` snapshot recovered))
    pendingResize<-act host (activateTree False (atLabel "Inspect" recovered) recovered)
    putMVar replyGate ()
    let resized=fst (handleEvent (V.EvResize 90 30) pendingResize)
    resizedResult<-await (tickSidebar host) (T.isInfixOf "expired" . status) resized
    check "prepared sidebar document refuses changed geometry" (not ("Retired action" `T.isInfixOf` activeText resizedResult))
    takeMVar actionEntered
    let startInspect current=do
          next<-tickSidebar host current
          act host (activateTree False (atLabel "Inspect" next) next)
    invoked<-await startInspect (T.isPrefixOf "Opening sidebar target" . status) recovered
    readMVar actionEntered
    withdrawn<-retireTreeFromHost host (P.treeReference provider) invoked
    check "withdrawal removes cached root rows before another projection" (not (any ((=="Tools").P.infoLabel.rowInfo.snd) (visibleRows 0 32768 (treeOf withdrawn))))
    let openFile current=do
          next<-tickSidebar host current
          act host (activateTree False (atLabel "Main.hs" next) next)
    refused<-await openFile (not . null . windows) withdrawn
    check "retired blocked action is cancelled and releases the Files action slot" (activeText refused=="main = 1\n")
  scoped<-withRegistry $ \registry->do
    (provider,_)<-TreeExtension.declare registry (const (error "not invoked")) (const (pure (Right (P.NodePage [] Nothing))))
    publishTreeFromHost host provider
    live<-settle host base
    check "scoped provider was published" (any ((=="Tools").P.infoLabel.rowInfo.snd) (visibleRows 0 32768 (treeOf live)))
    let toolRow=maybe (error "Tools row") id (rowAt (atLabel "Tools" live) (treeOf live))
        oldTrace=hitTrace (keyOf (rowHit toolRow)) (treeOf live)
    hidden<-act host (runCommand ToggleTree live)
    reshown<-act host (hidden,[ReadTree dir]) >>= settle host
    check "hide/show remounts still-live registered providers" (any ((=="Tools").P.infoLabel.rowInfo.snd) (visibleRows 0 32768 (treeOf reshown)))
    check "reopened provider refuses its pre-hide captured trace" (not (hitCurrent oldTrace (treeOf reshown)))
    -- Only registration workers backpressure; owner ticks drain four deltas.
    replicateM_ 32 (publishTreeFromHost host provider)
    advanced<-withAsync (publishTreeFromHost host provider) $ \writer->do
      threadDelay 20000
      blocked<-poll writer
      check "publication queue backpressures only its producer" (case blocked of Nothing->True; _->False)
      next<-tickSidebar host reshown
      resumed<-timeout 1000000 (wait writer)
      check "bounded owner drain releases publication producer" (case resumed of Just ()->True; _->False)
      pure next
    relocated<-act host (advanced,[ReadTree (dir </> "src")]) >>= settle host
    check "changing Files directory preserves independent sibling root" (any ((=="Tools").P.infoLabel.rowInfo.snd) (visibleRows 0 32768 (treeOf relocated)))
    pure relocated
  closed<-settle host scoped
  check "closing provider command scope withdraws its root" (not (any ((=="Tools").P.infoLabel.rowInfo.snd) (visibleRows 0 32768 (treeOf closed))))

recoveryChecks :: FilePath -> IO ()
recoveryChecks dir=withSidebarCommands $ \host->withRegistry $ \registry->do
  initial<-initializeSidebar host (installSidebar (emptySidebar dir 24 True) (initialDesktop (100,30)))
  (provider,_)<-TreeExtension.declare registry (const (error "not invoked")) (const (pure (Right (P.NodePage [] Nothing))))
  publishTreeFromHost host provider
  published<-settle host initial
  relocated<-act host (published,[ReadTree (dir </> "src")]) >>= settle host
  returned<-act host (relocated,[ReadTree dir]) >>= settle host
  check "nonresource provider precedes recovered Files rows" (P.infoLabel (rowInfo (maybe (error "provider root") id (rowAt 0 (treeOf returned))))=="Tools")
  opened<-act host (activateTree True (atLabel "src" returned) returned) >>= settle host
  let selected=select (atLabel "a.hs" opened) opened
      checkpoint=dir </> "sidebar.checkpoint"
      before=treeOf selected
      chosen=maybe (error "selected row") id (rowAt (treeSelected before) before)
      positioned=selected {sideTree=Just before {treeScroll=treeSelected before}}
  writeCheckpoint checkpoint positioned >>= right
  recovered<-readCheckpoint checkpoint (initialDesktop (100,30)) >>= right
  check "recovery retains hints rather than old scope handles" (M.null (treeNodes (treeOf recovered)) && null (treeRoots (treeOf recovered)))
  withSidebarCommands $ \freshHost->do
    restored<-initializeSidebar freshHost recovered
    let after=treeOf restored
        freshRow=maybe (error "restored row") id (rowAt (treeSelected after) after)
    check "fresh scope restores selected and top resource anchors" (P.infoResource (rowInfo freshRow)==P.infoResource (rowInfo chosen) && treeScroll after==treeSelected after && rowHit freshRow/=rowHit chosen)
    -- A prepared result must preserve input changes made after preparation began.
    prepared<-prepareProjection after
    let moved=after {treeSelected=0,treeScroll=0}
        adopted=adoptProjection prepared moved
    check "projection adoption preserves later viewport and selection input" (treeSelected adopted==0 && treeScroll adopted==0)

-- Valid scoped roots fill the public bound; Files mounting must refuse safely.
budgetChecks :: FilePath -> IO ()
budgetChecks dir=withSidebarCommands $ \host->withRegistry $ \registry->do
  ident<-right (P.nodeId "root")
  providers<-forM [1..32::Int] $ \index->right =<< P.registerTree registry
    ("extension.budget.provider"<>T.pack (show index))
    (P.NodeDef (P.NodeInfo ident "Other root" "" True Nothing) Nothing [])
    (\_ _->pure (Right (P.NodePage [] Nothing)))
  let full=foldl (\tree provider->addRoot (P.treeReference provider) (P.nodeInfo (P.treeRoot provider)) Nothing [] tree) (emptySidebar dir 24 True) providers
      desktop=installSidebar full (initialDesktop (100,30))
  refused<-act host (desktop,[])
  check "Files root refuses a full provider budget without crashing" ("budget" `T.isInfixOf` status refused && S.length (treeRoots (treeOf refused))==32)
  let available=refused {sideTree=Just (removeRoot (P.treeReference (head providers)) (treeOf refused))}
  accepted<-act host (available,[]) >>= settle host
  check "Files root loads once a provider slot becomes available" (any ((=="Main.hs").P.infoLabel.rowInfo.snd) (visibleRows 0 32768 (treeOf accepted)))
  let unchanged=removeRoot (P.treeReference (head providers)) (treeOf accepted)
  check "repeated withdrawal does not invalidate prepared projections" (treeRevision unchanged==treeRevision (treeOf accepted))
