{-# LANGUAGE OverloadedStrings #-}
-- | Cabal package roots on the shared sidebar. A metadata worker observes the
-- selected directory and package files; lazy tree workers resolve source paths.
-- The UI owner only publishes scope changes and four invalidations per tick.
-- Source actions retain the package revision, then use the common file adopter.
module Hide.PackageSidebar (PackageSidebar, withPackageSidebar, tickPackageSidebar, packageBuildEffects, packageBuildManifestCurrent) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (evaluate,finally)
import Control.Monad (filterM,foldM,forM,forM_,unless)
import Data.Aeson (Value(Null))
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Distribution.PackageDescription (Condition(..),CondTree(..))
import Distribution.Types.Condition (cOr)
import System.Directory (canonicalizePath,doesFileExist,getFileSize,getModificationTime,listDirectory)
import System.FilePath ((</>),isAbsolute,makeRelative,splitDirectories,takeDirectory,takeExtension,takeFileName)
import System.IO (withBinaryFile,IOMode(ReadMode))
import System.IO.Error (tryIOError)
import Text.Read (readMaybe)
import Hide.GuestAccess (protectedFilePath)
import Hide.Model (Desktop(..),Effect(..),BuildAction(..),PackageBuildTarget(..),startingDirectory,privateFilePaths)
import Hide.PackagePaths
import Hide.PackageSources
import Hide.Plugin.Command
import qualified Hide.Plugin.Tree as P
import Hide.Sidebar (treeRoot)
import qualified Hide.Plugin.Menu as Menu
import Hide.SidebarCommands

type Scope = (FilePath,[FilePath])
type Node = P.NodeDef SidebarContext SidebarReply
type Stamp = Maybe (UTCTime,Integer)
data Snapshot = Snapshot !Int !Stamp !(Either Text PackageSources)
  !(M.Map P.NodeId [Node])
data Slot = Slot !FilePath !(P.TreeProvider SidebarContext SidebarReply)
  ![CommandRef] !(IORef Snapshot) !(IORef (M.Map (Text,Source,Text,FilePath) P.NodeId,Int))
data PackageSidebar = PackageSidebar !(IORef Scope) !(IORef (M.Map (P.TreeRef,P.NodeId) ())) !(IORef [Slot])

rootId :: P.NodeId
rootId=ident "package"
ident :: Text -> P.NodeId
ident=either (error . T.unpack) id . P.nodeId
componentId :: Int -> P.NodeId
componentId=ident . ("target:"<>) . T.pack . show
scope :: Desktop -> Scope
scope d=(maybe (startingDirectory d) treeRoot (sideTree d),privateFilePaths d)

-- | Observe only package files in the selected directory. Project-file globs and
-- configured multi-directory package discovery are separate from this service.
withPackageSidebar :: SidebarHost -> Desktop -> (PackageSidebar -> IO a) -> IO a
withPackageSidebar host initial use=withRegistry $ \registry->do
  desired<-newIORef (scope initial)
  dirty<-newIORef M.empty
  slots<-newIORef []
  let service=PackageSidebar desired dirty slots
      retire (Slot _ provider command _ _)=P.retireTree provider >> mapM_ (retireCommand registry) command >> pure ()
      clear=readIORef slots >>= mapM_ retire
      loop previous next=do
        requested@(directory,private)<-readIORef desired
        canonical<-tryIOError (canonicalizePath directory)
        case canonical of
          Left _->clear >> writeIORef slots [] >> pause requested next
          Right root->do
            whenChanged previous requested (clear >> writeIORef slots [])
            names<-either (const []) id <$> tryIOError (listDirectory root)
            let manifests=take 32 [root </> name | name<-sort names,takeExtension name==".cabal"]
            admitted<-filterPaths root private manifests
            current<-readIORef slots >>= filterM (\slot@(Slot _ provider _ _ _)->do live<-P.treeCurrent provider; unless live (retire slot); pure live)
            forM_ [slot | slot@(Slot file _ _ _ _)<-current,file `notElem` admitted] retire
            let retained=[slot | slot@(Slot file _ _ _ _)<-current,file `elem` admitted]
            created<-forM (zip [next..] [file | file<-admitted,all (\(Slot old _ _ _ _)->old/=file) retained]) $ \(serial,file)->do
              slot<-createSlot registry root private serial file
              let Slot _ provider _ _ _=slot
              publishTreeFromHost host provider
              pure slot
            let active=retained++created
            writeIORef slots active
            forM_ retained $ \(Slot file provider _ ref _)->do
              signature<-stamp file
              Snapshot _ old _ _<-readIORef ref
              unless (signature==old) $ do
                parsed<-readPackage file
                unless (packageTitle file parsed==P.infoLabel (P.nodeInfo (P.treeRoot provider))) (P.retireTree provider >> pure ())
                cached<-atomicModifyIORef' ref (\(Snapshot version _ _ cache)->(Snapshot (version+1) signature parsed M.empty,cache))
                let updates=(P.treeReference provider,rootId):[(P.treeReference provider,key) | key<-M.keys cached,key/=rootId]
                atomicModifyIORef' dirty (\pending->(M.union (M.fromList [(key,()) | key<-updates]) pending,()))
            pause requested (next+length created)
      pause requested next=threadDelay 500000 >> loop requested next
  withAsync (loop ("",[]) 0 `finally` clear) (const (use service))
  where whenChanged old new action=if old==new then pure () else action

-- | Only small scope metadata and prepared invalidation IDs cross the UI owner.
tickPackageSidebar :: PackageSidebar -> SidebarHost -> Desktop -> IO Desktop
tickPackageSidebar (PackageSidebar desired dirty _) host d=do
  old<-readIORef desired
  let current=scope d
  unless (old==current) (writeIORef desired current)
  changes<-atomicModifyIORef' dirty (\pending->let (now,later)=M.splitAt 4 pending in (later,M.keys now))
  foldM (\value (owner,node)->refreshTreeFromHost host owner node value) d changes

-- | Validate only bounded provider/snapshot/scope metadata at the execution gate.
-- Stat/canonical checks occur on the worker. External changes after that check
-- have a finite observation interval; this is not an atomic filesystem grant.
packageBuildEffects :: PackageSidebar -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
packageBuildEffects service core original=foldM step (False,original)
  where
    step state@(True,_) _=pure state
    step (_,d) effect=case effect of
      PackageDebugAction target _->checked d target effect
      AdoptPreparedDebug target->checked d target effect
      PackageBuildAction _ target->checked d target effect
      AdoptPreparedBuild (Just target)->checked d target effect
      _->core d [effect]
    checked d target effect=do
      live<-packageBuildCurrent service target d
      if live then core d [effect] else pure (False,d {status="Package build target expired; refresh the tree."})

packageBuildCurrent :: PackageSidebar -> PackageBuildTarget -> Desktop -> IO Bool
packageBuildCurrent (PackageSidebar _ _ slots) target d
  | packageBuildScope target/=scope d=pure False
  | otherwise=do
      active<-readIORef slots
      case [slot | slot@(Slot file provider _ _ _)<-active,
            file==packageBuildManifest target,P.treeReference provider==packageBuildProvider target] of
        Slot _ provider _ ref _:_->do
          live<-P.treeCurrent provider
          Snapshot version signature _ _<-readIORef ref
          pure (live && version==packageBuildVersion target && signature==packageBuildStamp target)
        _->pure False

-- | Worker-only path/stamp validation around captured component planning.
packageBuildManifestCurrent :: PackageBuildTarget -> IO Bool
packageBuildManifestCurrent target=do
  observed<-stamp (packageBuildManifest target)
  checked<-safePath (packageBuildRoot target) (snd (packageBuildScope target)) (packageBuildManifest target)
  pure (observed==packageBuildStamp target && observed/=Nothing && checked==Just (packageBuildManifest target))

stamp :: FilePath -> IO Stamp
stamp file=either (const Nothing) Just <$> tryIOError ((,) <$> getModificationTime file <*> getFileSize file)
readPackage :: FilePath -> IO (Either Text PackageSources)
readPackage file=do
  bytes<-tryIOError (withBinaryFile file ReadMode (\handle->BS.hGet handle (1048576+1)))
  pure (either (const (Left "Cannot read package description.")) parsePackageSources bytes)
filterPaths :: FilePath -> [FilePath] -> [FilePath] -> IO [FilePath]
filterPaths root private paths=fmap concat $ forM paths $ \path->do
  checked<-safePath root private path
  pure (maybe [] (:[]) checked)
safePath :: FilePath -> [FilePath] -> FilePath -> IO (Maybe FilePath)
safePath root private path
  | protectedFilePath private path=pure Nothing
  | otherwise=do
      resolved<-tryIOError (canonicalizePath path)
      case resolved of
        Right canonical | let relative=makeRelative root canonical
                        , not (isAbsolute relative),".." `notElem` splitDirectories relative
                        , not (protectedFilePath private canonical)->do
            exists<-doesFileExist canonical
            pure (if exists then Just canonical else Nothing)
        _->pure Nothing

packageTitle :: FilePath -> Either Text PackageSources -> Text
packageTitle file=T.take 256 . T.filter (>= ' ') . either (const (T.pack (takeFileName file))) sourcePackageName

createSlot :: Registry SidebarContext -> FilePath -> [FilePath] -> Int -> FilePath -> IO Slot
createSlot registry root _private serial file=do
  initialStamp<-stamp file
  parsed<-readPackage file
  ref<-newIORef (Snapshot 0 initialStamp parsed M.empty)
  ids<-newIORef (M.empty,0)
  let namespace="hide.sidebar.package.p"<>T.pack (show serial)
      codec=Codec Null (const (Left "Package actions are host-captured.")) (const Null)
      title=packageTitle file parsed
  open<-registerCommand registry (CommandDef (namespace<>".open") "Open source" codec codec $ \ctx (version,path)->do
    Snapshot current capturedStamp _ _<-readIORef ref
    observed<-stamp file
    checked<-safePath root (sidebarPrivatePaths ctx) path
    if maybe False (/=current) version || maybe False (const (observed/=capturedStamp)) version || checked/=Just path then pure (Left (CommandRejected "Package source changed; refresh the tree."))
      else do
        result<-prepareSidebarFile ctx path
        Snapshot latest _ _ _<-readIORef ref
        after<-stamp file
        pure $ if maybe False (/=latest) version || observed/=after
          then Left (CommandRejected "Package changed while opening source; refresh the tree.") else result) >>= required
  let eligible action component=
        let group=condTreeData (sourceTree component)
        in sourceBuildable group && case action of
          Make->sourceKind component `elem` [LibraryComponent,ExecutableComponent] ||
            sourceKind component==TestComponent && sourceRunKind group `elem` [ExecutableRun,DriverRun] ||
            sourceKind component==BenchmarkComponent && sourceRunKind group==ExecutableRun
          Run->debugEligible component
          Test->sourceKind component==TestComponent && sourceRunKind group `elem` [ExecutableRun,DriverRun]
          Benchmark->sourceKind component==BenchmarkComponent && sourceRunKind group==ExecutableRun
          Compile->False
      registerBuild action suffix label=registerCommand registry (CommandDef (namespace<>suffix) label codec codec $ \ctx (version,name)->do
        Snapshot current capturedStamp package _<-readIORef ref
        observed<-stamp file
        case (sidebarOrigin ctx,sidebarProvider ctx,package) of
          (Menu.HumanMenu,Just owner,Right parsedPackage)
            | current==version,observed==capturedStamp
            , component:_<-[item | item<-sourceComponents parsedPackage,sourceTarget item==name]
            , eligible action component->do
                let target=PackageBuildTarget owner version
                      (sidebarContextDirectory ctx,sidebarPrivatePaths ctx) root file observed
                      (sourcePackageName parsedPackage<>":"<>name)
                -- Construct and force fixed receipt metadata on the action worker.
                _<-evaluate (T.length (packageBuildName target)+length root+length file
                  +length (sidebarContextDirectory ctx)+sum (map length (sidebarPrivatePaths ctx)))
                reply<-evaluate (SidebarBuild action target)
                pure (Right reply)
          _->pure (Left (CommandRejected "Package build target changed or is not a human action."))) >>= required
  build<-registerBuild Make ".build" "Build component"
  run<-registerBuild Run ".run" "Run component"
  test<-registerBuild Test ".test" "Test component"
  benchmark<-registerBuild Benchmark ".benchmark" "Benchmark component"
  debug<-registerCommand registry (CommandDef (namespace<>".debug") "Debug component" codec codec $ \ctx (version,name)->do
    Snapshot current signature package _<-readIORef ref
    observed<-stamp file
    case (sidebarOrigin ctx,sidebarProvider ctx,package) of
      (Menu.HumanMenu,Just owner,Right value)
        | current==version,observed==signature
        , component:_<-[item | item<-sourceComponents value,sourceTarget item==name]
        , debugEligible component->do
            entry<-debugEntry root (sidebarPrivatePaths ctx) file component
            let target=PackageBuildTarget owner version (sidebarContextDirectory ctx,sidebarPrivatePaths ctx)
                  root file observed (sourcePackageName value<>":"<>name)
            _<-evaluate (T.length (packageBuildName target)+length root+length file
              +length (sidebarContextDirectory ctx)+sum (map length (sidebarPrivatePaths ctx))
              +either T.length length entry)
            Snapshot latest _ _ _<-readIORef ref
            after<-stamp file
            pure $ if latest/=version || after/=observed
              then Left (CommandRejected "Package changed while preparing Debug; refresh the tree.")
              else Right (SidebarPackageDebug target entry)
      _->pure (Left (CommandRejected "Package debug target changed or is not a human action."))) >>= required
  -- doc-artifact: tools/docs-screenshots.hs package-target-menu -> docs/site/screenshots/package-target-menu.png (docs/running.md)
  let targetActions version component=
        [P.ActionMenu label (P.treeAction registry command (version,sourceTarget component) (\_ result->pure result))
        | (label,buildAction,command)<-[("Build",Make,build),("Run",Run,run),("Test",Test,test),("Benchmark",Benchmark,benchmark)]
        , eligible buildAction component]++
        [P.ActionMenu "Debug" (P.treeAction registry debug (version,sourceTarget component) (\_ result->pure result))
        | debugEligible component]
      action version path=P.treeAction registry open (Just version,path) (\_ result->pure result)
      rootNode=P.NodeDef (P.NodeInfo rootId title "" True (Just file)) Nothing
        [P.ActionMenu "Open package file" (P.treeAction registry open (Nothing,file) (\_ result->pure result))]
      children ctx (P.ChildRequest key cursor)=do
        Snapshot version _ package cached<-readIORef ref
        case pageOffset cursor of
          Nothing->pure (Left (InvalidArguments "Invalid package page cursor."))
          Just offset->do
            result<-case M.lookup key cached of
              Just nodes->pure (Right nodes)
              Nothing | key==rootId->pure $ Right $ case package of
                Left _->[P.NodeDef (P.NodeInfo (ident "error") "Package description unavailable; reopen the package file" "" False Nothing) Nothing []]
                Right value->[P.NodeDef (P.NodeInfo (componentId index) (T.take 256 (sourceTarget component)) "" True Nothing) Nothing (targetActions version component) | (index,component)<-zip [0..] (take 4096 (sourceComponents value))]
              Nothing->case package of
                Right value | component:_<-[component | (index,component)<-zip [0..] (sourceComponents value),componentId index==key]->do
                  resolved<-resolveSources root (sidebarPrivatePaths ctx) (takeDirectory file) component
                  case resolved of
                    Left err->pure (Left (CommandRejected err))
                    Right values->do
                      nodes<-mapM (sourceNode ids action ref version (sourceTarget component)) values
                      pure (Right nodes)
                _->pure (Left (CommandRejected "Package node is no longer available."))
            Snapshot current _ _ _<-readIORef ref
            if current/=version then pure (Left (CommandRejected "Package description changed.")) else case result of
              Left err->pure (Left err)
              Right nodes->do
                storeCache ref version key nodes
                pure (Right (P.NodePage (take 128 (drop offset nodes)) (if null (drop (offset+128) nodes) then Nothing else Just (T.pack (show (offset+128))))))
  provider<-P.registerTree registry namespace rootNode children >>= required
  pure (Slot file provider [commandRef open,commandRef build,commandRef run,commandRef test,commandRef benchmark,commandRef debug] ref ids)
  where required=either (ioError . userError . show) pure

-- Entry selection is a worker operation. File existence cannot select a Cabal
-- conditional branch; an unresolved/ambiguous main remains a GHC refusal.
debugEligible :: ComponentSources -> Bool
debugEligible component=let group=condTreeData (sourceTree component)
  in sourceBuildable group && sourceRunKind group==ExecutableRun &&
     sourceKind component `elem` [ExecutableComponent,TestComponent,BenchmarkComponent]

debugEntry :: FilePath -> [FilePath] -> FilePath -> ComponentSources -> IO (Either Text FilePath)
debugEntry root private manifest component=do
  let mainsOnly group=group {sourceEntries=[entry | entry@MainSource{}<-sourceEntries group]}
      selected=component {sourceTree=fmap mainsOnly (sourceTree component)}
  resolved<-resolveSources root private (takeDirectory manifest) selected
  pure $ do
    candidates<-resolved
    let mains=[candidate | candidate<-candidates,MainSource _<-[candidateSource candidate]]
        paths=[entry | candidate<-mains,entry<-candidatePaths candidate]
        existing=M.keys (M.fromList [(path,()) | entry<-paths,Just path<-[existingPath entry]])
    if any ((/=Lit True).candidateCondition) mains || any ((/=Lit True).pathCondition) paths
      then Left "Debug needs an unconditional main-is and source directory. Use an explicit Adapter configuration for this component."
      else case existing of
        [path] | takeExtension path `elem` [".hs",".lhs"]->Right path
        _->Left "Debug needs one existing Haskell main-is for this component. Use an explicit Adapter configuration."

sourceNode :: IORef (M.Map (Text,Source,Text,FilePath) P.NodeId,Int)
  -> (Int -> FilePath -> P.TreeAction SidebarContext SidebarReply)
  -> IORef Snapshot -> Int -> Text -> SourceCandidate -> IO Node
sourceNode ids action snapshot version target candidate=do
  let source=candidateSource candidate
      guard=T.pack (show (candidateCondition candidate))
      paths=M.toList (M.fromListWith cOr [(path,pathCondition entry) | entry<-candidatePaths candidate,Just path<-[existingPath entry]])
      conditional=candidateCondition candidate/=Lit True || any ((/=Lit True).snd) paths
      suffix=case source of VirtualSource _->" (virtual)"; ModuleSource _ True->" (generated)"; IncludeSource _ True->" (generated)"; _ | null paths->" (missing)"; _->""
      name=case source of ModuleSource value _->value; SignatureSource value->value; VirtualSource value->value; DriverSource value->value; MainSource path->T.pack path; PackageFileSource path->T.pack path; IncludeSource path _->T.pack path
      label=T.take 256 (name<>suffix<>if conditional then " ?" else "")
  key<-allocate (target,source,guard,"")
  case paths of
    [(path,_)]->pure (P.NodeDef (P.NodeInfo key label "📄" False (Just path)) (Just (action version path)) [])
    []->pure (P.NodeDef (P.NodeInfo key label "" False Nothing) Nothing [])
    _->do
      nodes<-forM paths $ \(path,condition)->do
        child<-allocate (target,source,guard,path)
        pure (P.NodeDef (P.NodeInfo child (T.take 256 (T.pack path<>if condition==Lit True then "" else " ?")) "📄" False (Just path)) (Just (action version path)) [])
      storeCache snapshot version key nodes
      pure (P.NodeDef (P.NodeInfo key label "" True Nothing) Nothing [])
  where
    allocate value=do
      allocated<-atomicModifyIORef' ids $ \(known,next)->case M.lookup value known of
        Just key->((known,next),Right key)
        Nothing | next>=32768->((known,next),Left "Package node identity budget reached.")
                | otherwise->let key=ident ("source:"<>T.pack (show next)) in ((M.insert value key known,next+1),Right key)
      either (ioError . userError) pure allocated

pageOffset :: Maybe Text -> Maybe Int
pageOffset value=do
  offset<-maybe (Just 0) (readMaybe . T.unpack) value
  if offset>=0 && offset<=32768 then Just offset else Nothing

-- Budget rejection leaves the previous snapshot intact; never put a failing
-- cache thunk in a shared IORef. This executes only on provider workers.
storeCache :: IORef Snapshot -> Int -> P.NodeId -> [Node] -> IO ()
storeCache ref version key nodes=do
  result<-atomicModifyIORef' ref $ \current@(Snapshot v sig package cache)->
    let updated=M.insert key nodes cache
    in if v/=version then (current,Left "Package description changed.")
       else if not (M.member key cache) && M.size cache>=256 then (current,Left "Package child-page budget reached.")
       else if sum (map length (M.elems updated))>32768 then (current,Left "Package retained-node budget reached.")
       else (Snapshot v sig package updated,Right ())
  either (ioError . userError) pure result
