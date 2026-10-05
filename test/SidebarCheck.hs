{-# LANGUAGE OverloadedStrings #-}
module SidebarCheck (checks) where
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync,wait,poll,waitCatch,asyncThreadId)
import Data.IORef
import Data.List (findIndex)
import Data.Aeson (object,(.=))
import Hide.WorkspaceFilesMCP (fileTool)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Hide.Plugin.BufferHost (captureVersion,versionCurrent)
import Control.Concurrent.MVar
import Control.Exception (bracket,evaluate,fromException)
import GHC.Conc (threadStatus,ThreadStatus(..),BlockReason(..))
import System.IO.Error (tryIOError,isUserError)
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
import qualified Hide.Plugin.Form as Form
import qualified FormExtension
import qualified TreeExtension

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)
right :: Show e => Either e a -> IO a
right=either (error.show) pure
treeOf :: Desktop -> Sidebar
treeOf=maybe (error "missing tree") id . sideTree
settle :: SidebarHost -> Desktop -> IO Desktop
settle host=await (tickSidebar host applyEffects) (\d->let tree=treeOf d in treeProjectionRevision tree==treeRevision tree && all ready (M.elems (treeNodes tree)))
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

-- One real Files popup/form workflow, including its filesystem refusal paths.
fileRenameChecks :: IO ()
fileRenameChecks=bracket temporary removePathForcibly $ \dir->withSidebarCommands $ \host->do
  let original=dir </> "Main.hs"
      renamed=dir </> "Renamedλ.hs"
      popupFor name d=
        let selected=select (atLabel name d) d
            y=2+treeSelected (treeOf selected)-treeScroll (treeOf selected)
        in fst (handleEvent (V.EvMouseDown 5 y V.BRight []) selected)
      chooseRename popup=case findIndex ((=="Rename…").fst) (contextItemsFor popup) of
        Nothing->error "Files has no Rename context action"
        Just index->handleEvent (V.EvKey V.KEnter []) (iterate (fst . handleEvent (V.EvKey V.KDown [])) popup!!index)
      open name d=act host (chooseRename (popupFor name d)) >>= await (tickSidebar host applyEffects) (\next->dialog next/=Nothing)
      submit name d=act host (handleEvent (V.EvKey V.KEnter []) (fst (handleEvent (V.EvPaste (TE.encodeUtf8 name)) d)))
      refused d=await (tickSidebar host applyEffects) (\next->any (`T.isInfixOf` status next) ["failed","changed","expired"]) d
      path d=filePath <$> (activeDocument d >>= documentFile)
      doc d=maybe (error "rename source missing") id (activeDocument d)
      sourceCommand command d=fst (runCommand command d {sideTree=fmap (\tree->tree {treeFocused=False}) (sideTree d)})
      ids d=map (\w->(windowId w,bufferId w,selection w,scrollRow w,scrollColumn w)) (windows d)
  createDirectory (dir </> "archive")
  TIO.writeFile original "main = 1\n"
  BS.writeFile (dir </> "bytes.bin") (BS.pack [0,255,13,10])
  (file,buffer)<-loadFile original >>= either error pure
  -- A clean buffer with retained redo proves path adoption does not replace Undo.
  let source=fst (runCommand Undo (insertText "x" (addDocument (Just file) buffer (initialDesktop (100,30)))))
  mounted<-initializeSidebar host (installSidebar (emptySidebar dir 24 True) source)
  version<-captureVersion (documentBuffer (doc mounted))
  form<-open "Main.hs" mounted
  check "Files Rename opens the basename preselected"
    (case dialog form of Just dg | PluginInputForm{}<-purpose dg,[SelectedInput label name selected]<-fields dg->label=="Name" && name=="Main.hs" && selected==Selection 0 7; _->False)
  pending<-submit "Renamedλ.hs" form
  result<-await (tickSidebar host applyEffects) ((==Just renamed).path) pending >>= settle host
  same<-versionCurrent version (documentBuffer (doc result))
  oldExists<-doesPathExist original
  bytes<-BS.readFile renamed
  check "Files Rename keeps open IDs, live buffer identity and exact bytes"
    (same && ids result==ids mounted && not oldExists && bytes=="main = 1\n" && not (dirty (documentBuffer (doc result))))
  check "Files Rename preserves Undo and Redo history" (activeText (sourceCommand Redo result)=="xmain = 1\n")
  collision<-open "Renamedλ.hs" result
  TIO.writeFile (dir </> "Taken.hs") "keep me"
  refusedCollision<-submit "Taken.hs" collision >>= refused
  untouched<-BS.readFile (dir </> "Taken.hs")
  check "Rename refuses an occupied destination" (path refusedCollision==Just renamed && untouched=="keep me")
  dirtyForm<-open "Renamedλ.hs" refusedCollision
  dirtyVersion<-captureVersion (documentBuffer (doc dirtyForm))
  let changedSource=insertText "local " dirtyForm {sideTree=fmap (\tree->tree {treeFocused=False}) (sideTree dirtyForm)}
  unchangedSource<-versionCurrent dirtyVersion (documentBuffer (doc changedSource))
  check "The source really changes while its rename form remains open" (not unchangedSource && dirty (documentBuffer (doc changedSource)))
  refusedDirty<-submit "Dirty.hs" changedSource >>= refused
  dirtyTarget<-doesPathExist (dir </> "Dirty.hs")
  check "Rename refuses a source edited after its form opened" (not dirtyTarget && path refusedDirty==Just renamed && dirty (documentBuffer (doc refusedDirty)))
  clean<-pure (sourceCommand Undo refusedDirty)
  staleForm<-open "Renamedλ.hs" clean
  TIO.writeFile renamed "external replacement\n"
  refusedStale<-submit "Stale.hs" staleForm >>= refused
  staleTarget<-doesPathExist (dir </> "Stale.hs")
  check "Rename refuses a changed filesystem source" (not staleTarget && path refusedStale==Just renamed)
  TIO.writeFile renamed "main = 1\n"
  let oldPopup=popupFor "Renamedλ.hs" refusedStale
      (captured,effects)=chooseRename oldPopup
      collapsed=captured {sideTree=Just (collapseAt 0 (treeOf captured))}
  staleNode<-act host (collapsed,effects)
  check "Rename refuses a stale captured tree node" (dialog staleNode==Nothing)
  expanded<-act host (activateTree True 0 staleNode) >>= settle host
  -- Ordinary binary files use the same basename form without decoding bytes.
  (binaryFile,binaryBuffer)<-loadFile (dir </> "bytes.bin") >>= either error pure
  let binarySource=addDocument (Just binaryFile) binaryBuffer expanded
  binaryForm<-open "bytes.bin" binarySource
  binaryPending<-submit "bytes.dat" binaryForm
  binaryResult<-await (tickSidebar host applyEffects) ((==Just (dir </> "bytes.dat")).path) binaryPending >>= settle host
  binaryBytes<-BS.readFile (dir </> "bytes.dat")
  check "Files Rename preserves byte-mode files" (byteMode (documentBuffer (doc binaryResult)) && binaryBytes==BS.pack [0,255,13,10])
  cachedDestination<-act host (activateTree True (atLabel "archive" binaryResult) binaryResult) >>= settle host
  (moved,answer)<-fileTool (sidebarEffects host applyEffects) cachedDestination "workspace_files"
    (object ["operation" .= ("rename"::T.Text),"path" .= (dir </> "bytes.dat"),"to" .= (dir </> "archive/bytes.dat")])
  _<-answer >>= right
  refreshedMove<-settle host moved
  check "Cross-directory MCP rename refreshes both cached parent listings"
    (path refreshedMove==Just (dir </> "archive/bytes.dat") &&
      length [() | (_,row)<-visibleRows 0 32768 (treeOf refreshedMove),P.infoLabel (rowInfo row)=="bytes.dat"]==1 &&
      any (\(_,row)->P.infoResource (rowInfo row)==Just (dir </> "archive/bytes.dat")) (visibleRows 0 32768 (treeOf refreshedMove)))
  lateForm<-open "Renamedλ.hs" refreshedMove
  late<-submit "Late.hs" lateForm
  let newer=sourceCommand Find (focusWindow (windowId (head (windows mounted))) late)
  check "The late rename fixture opens a newer source modal"
    (maybe False (\dg->case purpose dg of Searching{}->True; _->False) (dialog newer))
  retired<-tickSidebar host applyEffects newer
  lateTarget<-doesPathExist (dir </> "Late.hs")
  check "A newer modal retires pending Rename before its filesystem effect"
    (not lateTarget && maybe False (\dg->case purpose dg of Searching{}->True; _->False) (dialog retired))

checks :: IO ()
checks=bracket temporary removePathForcibly $ \dir->withSidebarCommands $ \host->do
  publicationLifetimeChecks
  fileRenameChecks
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
    >>= await (tickSidebar host applyEffects) (\d->not (null (windows d)))
  check "Files primary action opens through a typed worker" (activeText opened=="main = 1\n")
  let changed=insertText "local " opened
  dirty<-await (tickSidebar host applyEffects) (\d->maybe False (\(value,_,_)->value) (M.lookup (dir </> "Main.hs") (treeBadges (treeOf d)))) changed
  check "Files cached badge preserves dirty counts" ("Main.hs +1 -1" `T.isInfixOf` snapshot dirty)
  removeFile (dir </> "Main.hs")
  createDirectory (dir </> "Main.hs")
  reopened<-act host (activateTree False (atLabel "Main.hs" dirty) dirty)
    >>= await (tickSidebar host applyEffects) (\d->not (treeFocused (treeOf d)) || "failed" `T.isInfixOf` status d)
  check "Opening an existing dirty file survives an unreadable disk replacement and preserves its live content" (activeText reopened=="local main = 1\n" && not (treeFocused (treeOf reopened)))
  removeDirectory (dir </> "Main.hs")
  TIO.writeFile (dir </> "Main.hs") "main = 1\n"
  let privateIndex=atLabel "thc.toml" reopened
      privateTree=select privateIndex reopened
  (agentState,agentEffects)<-right =<< Wire.applyGuestInput (Wire.Key "Enter" []) privateTree
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
  recoveryPagingChecks
  budgetChecks dir
  putStrLn "shared sidebar checks passed"

-- A publisher belongs to its caller, not the host's owned preparation jobs.
-- Saturating the real queue must not strand that caller when the host closes.
publicationLifetimeChecks :: IO ()
publicationLifetimeChecks=withRegistry $ \registry->do
  (provider,_)<-TreeExtension.declare registry (const (error "not invoked")) (const (pure (Right (P.NodePage [] Nothing))))
  prepared<-FormExtension.prepareForm registry (Form.InputFormSpec "Rename" "Name" "Old" "Rename") (\_ _->pure (Right ())) >>= right
  admitted<-Form.admitInputForm False prepared
  check "publication fixture opens its exact form" admitted
  update<-Form.refreshInputForm (Form.formReference prepared) (Form.InputFormSpec "Refresh" "Name" "Ignored" "Rename") >>= right >>= maybe (error "missing refresh") pure
  hostReady<-newEmptyMVar
  withAsync (readMVar hostReady >>= \host->publishTreeFromHost host provider) $ \treeWriter->
    withAsync (readMVar hostReady >>= \host->publishFormRefreshFromHost host update) $ \formWriter->do
      escaped<-withSidebarCommands $ \host->do
        replicateM_ 32 (publishTreeFromHost host provider)
        putMVar hostReady host
        mapM_ blocked [treeWriter,formWriter]
        pure host
      mapM_ rejected [treeWriter,formWriter]
      treeLate<-tryIOError (publishTreeFromHost escaped provider)
      formLate<-tryIOError (publishFormRefreshFromHost escaped update)
      check "closed host explicitly rejects late tree and form publications" (closedError treeLate && closedError formLate)
      let initial=installSidebar (emptySidebar "/" 24 True) (initialDesktop (100,30))
      effectsLate<-tryIOError (sidebarEffects escaped (\_ _->error "closed host dispatched effects") initial [])
      check "closed host rejects effect dispatch before mounting" (case effectsLate of Left err->closedError (Left err); Right _->False)
      next<-tickSidebar escaped (\_ _->error "closed tick dispatched effects") initial
      check "late tick cannot mount or resurrect queued providers" (null (treeRoots (treeOf next)) && M.null (treeNodes (treeOf next)))
  where
    blocked worker=do
      ready<-timeout 1000000 (awaitBlocked worker)
      check "full publication queue blocks each producer in STM" (ready==Just ())
    awaitBlocked worker=do
      state<-threadStatus (asyncThreadId worker)
      case state of
        ThreadBlocked BlockedOnSTM->pure ()
        ThreadFinished->error "publisher finished before host close"
        ThreadDied->error "publisher failed before host close"
        _->threadDelay 1000 >> awaitBlocked worker
    rejected worker=do
      result<-timeout 1000000 (waitCatch worker)
      check "host close resolves a saturated publisher with an explicit failure" (case result of Just (Left err)->maybe False (closedError . Left) (fromException err); _->False)
    closedError result=case result of
      Left err->isUserError err
      Right ()->False

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
  running<-tickSidebar host applyEffects loading
  takeMVar started
  projected<-await (tickSidebar host applyEffects) (T.isInfixOf "Loading…" . snapshot) running
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
  _<-await (tickSidebar host applyEffects) (T.isInfixOf "Independent action" . activeText) secondaryInvoked
  let selected=select (atLabel "Inspect" loaded) loaded
      node=maybe (error "leaf") id (rowAt (treeSelected (treeOf selected)) (treeOf selected))
      trace=hitTrace (keyOf (rowHit node)) (treeOf selected)
      reference=maybe (error "action") id (rowCommand node)
  check "independent resource labels cannot grant agent authority" (guestEffectsAllowed [InvokeTree trace reference Menu.AgentMenu])
  protected<-act host (selected,[InvokeTree trace reference Menu.AgentMenu])
  check "host refuses independent agent action" ("protected" `T.isInfixOf` status protected)
  invoked<-act host (Wire.applyInput (Wire.Key "Enter" []) selected)
  displayed<-await (tickSidebar host applyEffects) (T.isInfixOf "Independent action" . activeText) invoked
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
      tick value=tickReconciliation watcher (sidebarEffects host applyEffects) value >>= tickSidebar host applyEffects
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
    resizedResult<-await (tickSidebar host applyEffects) (T.isInfixOf "expired" . status) resized
    check "prepared sidebar document refuses changed geometry" (not ("Retired action" `T.isInfixOf` activeText resizedResult))
    takeMVar actionEntered
    let startInspect current=do
          next<-tickSidebar host applyEffects current
          act host (activateTree False (atLabel "Inspect" next) next)
    invoked<-await startInspect (T.isPrefixOf "Opening sidebar target" . status) recovered
    readMVar actionEntered
    withdrawn<-retireTreeFromHost host (P.treeReference provider) invoked
    check "withdrawal removes cached root rows before another projection" (not (any ((=="Tools").P.infoLabel.rowInfo.snd) (visibleRows 0 32768 (treeOf withdrawn))))
    let openFile current=do
          next<-tickSidebar host applyEffects current
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
      next<-tickSidebar host applyEffects reshown
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

-- Recover across an actual Files continuation page and a second fresh scope.
recoveryPagingChecks :: IO ()
recoveryPagingChecks=bracket temporary removePathForcibly $ \base->do
  let dir=base </> "files"; checkpoint=base </> "checkpoint"
      name n="d"++replicate (3-length (show n)) '0'++show n
      target=dir </> "d129" </> "inner" </> "target.hs"
      blank=initialDesktop (100,30)
  createDirectory dir
  forM_ [0..129::Int] $ \n->createDirectory (dir </> name n)
  createDirectory (dir </> "d129" </> "inner")
  writeFile target "main = pure ()\n"
  recovered<-withSidebarCommands $ \host->do
    initial<-initializeSidebar host (installSidebar (emptySidebar dir 24 True) blank)
    paged<-act host (activateTree False (atLabel "More…" initial) initial) >>= settle host
    branch<-act host (activateTree True (atLabel "d129" paged) paged) >>= settle host
    nested<-act host (activateTree True (atLabel "inner" branch) branch) >>= settle host
    let selected=atLabel "target.hs" nested
        positioned=nested {sideTree=Just (treeOf nested) {treeSelected=selected,treeScroll=selected}}
    writeCheckpoint checkpoint positioned >>= right
    readCheckpoint checkpoint blank >>= right
  withSidebarCommands $ \host->do
    restored<-initializeSidebar host recovered
    check "recovery does not automatically chase Files pages" (not (any ((=="d129").P.infoLabel.rowInfo.snd) (visibleRows 0 32768 (treeOf restored))))
    -- A visible branch may still await its bounded recovery batch. Its saved
    -- expansion must survive checkpointing rather than losing to rowExpanded.
    let boundary=restored {sideTree=Just (treeOf restored) {treeHints=case treeHints (treeOf restored) of
          Just (SidebarHints hints selected top)->Just (SidebarHints (M.insert (dir </> "d000") True hints) selected top)
          Nothing->error "missing pending recovery"}}
    writeCheckpoint checkpoint boundary >>= right
    pending<-readCheckpoint checkpoint blank >>= right
    check "checkpoint merges current resources with unresolved hints" (case treeHints (treeOf pending) of
      Just (SidebarHints hints _ _)->M.member (dir </> "d001") hints && M.lookup (dir </> "d000") hints==Just True && M.lookup (dir </> "d129") hints==Just True
      _->False)
    withSidebarCommands $ \freshHost->do
      fresh<-initializeSidebar freshHost pending
      let (continuation,_) = handleEvent (V.EvKey V.KEnd []) fresh
      hydrated<-act freshHost (handleEvent (V.EvKey V.KEnter []) continuation) >>= settle freshHost
      let tree=treeOf hydrated
          resource index=P.infoResource . rowInfo =<< rowAt index tree
      check "paging recovers the saved nested resource selection" (resource (treeSelected tree)==Just target)
      check "paging recovers the saved top resource" (resource (treeScroll tree)==Just target)

  withSidebarCommands $ \host->do
    restored<-initializeSidebar host recovered
    loading<-act host (activateTree False (atLabel "More…" restored) restored)
    started<-tickSidebar host applyEffects loading
    let (selected,_) = selectTreeRow (atLabel "d000" started) (treeOf started) started
        scrolled=scrollTreeTo 5 (treeOf selected) selected
    hydrated<-settle host scrolled
    let tree=treeOf hydrated
        resource index=P.infoResource . rowInfo =<< rowAt index tree
    check "new selection wins over deferred recovery" (resource (treeSelected tree)==Just (dir </> "d000"))
    check "new scroll wins over deferred recovery" (treeScroll tree==5)
    check "new navigation retains unrelated saved expansion" (any ((==Just target).P.infoResource.rowInfo.snd) (visibleRows 0 32768 tree))
  withSidebarCommands $ \host->do
    restored<-initializeSidebar host recovered
    let adjacent=dismissRecoveryBranch (dir </> "d12") (treeOf recovered)
    check "collapse prefix preserves an adjacent branch" (case treeHints adjacent of Just (SidebarHints hints _ _)->M.member (dir </> "d120") hints && M.member (dir </> "d129") hints; _->False)
    loading<-act host (activateTree False (atLabel "More…" restored) restored)
    collapsed<-act host (activateTree False (atLabel "Files" loading) loading) >>= settle host
    reopened<-act host (activateTree True (atLabel "Files" collapsed) collapsed) >>= settle host
    paged<-act host (activateTree False (atLabel "More…" reopened) reopened) >>= settle host
    check "explicit collapse prevents late recovery from reopening its subtree" (not (rowExpanded (maybe (error "late branch") id (rowAt (atLabel "d129" paged) (treeOf paged)))))

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
