{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : DebuggerAcquisitionCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module DebuggerAcquisitionCheck (checks) where

import Control.Concurrent
import Control.Exception (bracket,finally,getMaskingState,MaskingState(Unmasked),evaluate)
import System.Mem.StableName (makeStableName)
import Control.Monad (unless,forM_,void,when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Vector as Vec
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding
import qualified Graphics.Vty as V
import qualified Network.Socket as S
import System.Directory
import System.Environment
import System.FilePath
import System.Info (os)
import System.Timeout
import Hide.Buffer
import Hide.Build
import Hide.Debugger
import Hide.Plugin.Command (withRegistry)
import qualified Hide.Plugin.Tree as P
import Data.List (isPrefixOf)
import Hide.MenuCommands
import Hide.DocsMCP (withDocsCommands)
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Window as W
import Hide.Downloads
import Hide.Files
import Hide.GuestAccess
import Hide.HdbAcquisition
import Hide.Model
import Hide.RemoteEndpoint (randomIdentity)

checks :: IO ()
checks | os=="mingw32"=pure () -- Official hdb bindists are currently POSIX only.
      | otherwise=do
  temp<-getTemporaryDirectory
  ident<-randomIdentity
  repository<-getCurrentDirectory
  python<-findExecutable "python3" >>= maybe (error "hdb UI fixture requires Python") pure
  let directory=temp </> "hdb-ui-"++take 12 ident
  bracket (createDirectory directory >> canonicalizePath directory) removePathForcibly $ \root->do
    let configRoot=root </> "config"; config=configRoot </> "thc-edit"
        commands=root </> "commands"; ghc=commands </> "ghc-9.14.1"
        library=root </> "lib"; cache=root </> "cache"; source=root </> "Main.hs"
        logFile=root </> "dap.log"; adapter=cache </> "bin/hdb-9.14.1"
        settings=BuildConfig GHC ghc "" "" "" ["original argument"]
    mapM_ (createDirectoryIfMissing True) [config,commands,library]
    TIO.writeFile source "main = putStrLn \"hello\"\n"
    executableFile ghc ("#!/bin/sh\ncase \"$1\" in\n--numeric-version) echo 9.14.1;;\n--print-libdir) printf '%s\\n' "<>quote library<>";;\nesac\n")
    BL.writeFile (config </> "run.json") (encode (buildConfigValue root settings))
    withEnvironment [("XDG_CONFIG_HOME",configRoot),("THC_CACHE_HOME",cache),("PATH",commands)] $ do
      let asset=either (error.T.unpack) id (selectHdbAsset "aarch64-apple-darwin" "9.14.1")
          plan compiler=HdbPlan compiler library cache asset
          base=(addDocument (Just (FileState source Nothing)) (newBuffer "main = putStrLn \"hello\"\n") (initialDesktop (80,25))) {defaultDirectory=Just root,toolchain=Just GHC}
          core d _=pure (False,d)
          offer d=case dialog d of
            Just dg | DebugDialog name<-purpose dg->"hdb-accept:" `T.isPrefixOf` name
            _->False
          send runtime action args d=snd <$> debuggerEffects runtime core d [DebugAction action args]
          submit runtime button d=case dialog d of
            Nothing->error "missing acquisition dialog"
            Just dg->let (next,effects)=submitDialog button dg d in snd <$> debuggerEffects runtime core next effects
          waitFor runtime label predicate d=timeout 5000000 (loop d) >>= maybe (error ("hdb UI timed out: "++label)) pure
            where loop state=do
                    next<-tickDebugger runtime state
                    done<-predicate next
                    if done then pure next else threadDelay 1000 >> loop next
          has text d=pure (any (\prepared->case W.preparedWindowRows prepared of
            W.RowsDetails rows _ _->any (\(W.WindowRow _ caption _)->text `T.isInfixOf` caption) rows
            _->False) (M.elems (pluginWindows d)))
          install report gate=do
            _<-report (DownloadProgress "Downloading" 7 (Just 20))
            _<-takeMVar gate
            createDirectoryIfMissing True (takeDirectory adapter)
            executableFile adapter ("#!/bin/sh\nexec "<>quote python<>" "<>quote (repository </> "test/dap-session.py")<>" "<>quote logFile<>" server-basic\n")
            pure (Right adapter)
          port=bracket (S.socket S.AF_INET S.Stream S.defaultProtocol) S.close $ \socket->do
            S.bind socket (S.SockAddrInet 0 (S.tupleToHostAddress (127,0,0,1)))
            S.SockAddrInet number _<-S.getSocketName socket
            pure (T.pack (show (fromIntegral number::Int)))
          start runtime=do p<-port; send runtime "launch-config" ["0","",p] base >>= waitFor runtime "offer" (pure.offer)
          reset=do
            exists<-doesPathExist cache
            if exists then removePathForcibly cache else pure ()
            TIO.writeFile logFile ""
      calls<-newIORef (0::Int)
      withDebuggerHdb (pure 0) (pure . Right . plan) (\_ _->modifyIORef' calls (+1)>>pure (Left "unexpected acquisition")) $ \runtime->do
        opened<-start runtime
        untouched<-readIORef calls
        exists<-doesPathExist cache
        check "offer performs no download or install" (untouched==0 && not exists)
        check "agent cannot read or activate download offer" (guestModalBlocked opened && not (guestKeyAllowed opened V.KEnter []) && not (cellReadable (cellAccess opened 20 10)))
        let (closed,effects)=handleEvent (V.EvKey V.KEsc []) opened
        declined<-snd <$> debuggerEffects runtime core closed effects
        check "escape declines and closes offer" (dialog declined==Nothing)
        _<-case dialog opened of Just dg->submit runtime 0 declined {dialog=Just dg}; _->error "missing offer"
        count<-readIORef calls
        check "old declined offer cannot acquire" (count==0)
      -- Fork ownership is registered while masked, but compiler probes remain
      -- interruptible and runtime shutdown joins their cleanup.
      preparationState<-newEmptyMVar
      preparationStopped<-newEmptyMVar
      preparationGate<-newEmptyMVar
      withDebuggerHdb (pure 0)
        (\compiler->(do state<-getMaskingState
                        putMVar preparationState state
                        takeMVar preparationGate
                        pure (Right (plan compiler))) `finally` putMVar preparationStopped ())
        (\_ _->pure (Left "unexpected acquisition")) $ \runtime->do
          p<-port
          _<-send runtime "launch-config" ["0","",p] base
          state<-timeout 1000000 (takeMVar preparationState) >>= maybe (error "preparation did not start") pure
          check "preparation body restores unmasked execution" (state==Unmasked)
      preparationCleanup<-tryTakeMVar preparationStopped
      check "runtime close joins pending preparation cleanup" (preparationCleanup==Just ())
      -- Replacing a still-running preparation retires its worker before the new
      -- request starts, without treating that old context as the new request.
      preparations<-newIORef (0::Int)
      entered<-newEmptyMVar
      preparationRelease<-newEmptyMVar
      let prepare compiler=do
            number<-atomicModifyIORef' preparations (\n->(n+1,n))
            if number==0 then putMVar entered () >> takeMVar preparationRelease else pure ()
            pure (Right (plan compiler))
      withDebuggerHdb (pure 0) prepare (\_ _->pure (Left "unexpected acquisition")) $ \runtime->do
        p<-port
        pending<-send runtime "launch-config" ["0","",p] base
        timeout 1000000 (readMVar entered) >>= maybe (error "preparation did not start") pure
        let replacement=addDocument (Just (FileState (root </> "Other.hs") Nothing)) (newBuffer "main = pure ()") pending
        TIO.writeFile (root </> "Other.hs") "main = pure ()"
        newer<-send runtime "launch-config" ["0","",p] replacement
        _<-waitFor runtime "replacement preparation" (pure.offer) newer
        count<-readIORef preparations
        check "retiring preparation cannot invalidate newer request" (count==2)
      forM_ ["cancel","two-jobs","retire","toolchain","project","edit","reload","settings","stop","newer","success"] $ \mode->do
        reset
        gate<-newEmptyMVar
        reportReady<-newEmptyMVar
        finished<-newEmptyMVar
        writeIORef calls 0
        withDebuggerHdb (pure 0) (pure . Right . plan)
          (\_ report->(modifyIORef' calls (+1)>>putMVar reportReady report>>install report gate) `finally` void (tryPutMVar finished ())) $ \runtime->withDocsCommands $ \docs->withMenuCommands docs $ \host->withDownloadsCommands host runtime $ do
            catalogue<-Menu.menuSnapshot (menuContributions host)
            let cancelRef=case [Menu.menuReference entry | entry<-catalogue,Menu.menuName (Menu.menuReference entry)=="hide.downloads.cancel"] of
                  reference:_->reference; _->error "missing Downloads Cancel contribution"
                captureCancel desktop=let (next,requests)=runCommand (RegisteredMenu cancelRef False) desktop in
                  snd <$> menuEffects host (debuggerEffects runtime core) next requests
                awaitMenu label predicate desktop=timeout 5000000 (loop desktop) >>= maybe (error ("menu timed out: "++label)) pure
                  where loop state=do
                          next<-tickMenus host (debuggerEffects runtime core) state
                          if predicate next then pure next else threadDelay 1000 >> loop next
            opened<-start runtime
            accepted<-submit runtime 0 opened {contributedMenus=catalogue}
            progress<-waitFor runtime "progress" (has "Downloading") accepted
            count<-readIORef calls
            check "accept starts exactly one background transfer" (count==1)
            responsive<-timeout 1000000 (tickDebugger runtime progress)
            check "held transfer never blocks editor tick" (maybe False (const True) responsive)
            changed<-case mode of
              "cancel"->do
                report<-takeMVar reportReady
                let details d=maybe (error "missing Details") id (activePluginWindow d)
                    detailState d=case activeWindow d of
                      Just w->(selection w,scrollRow w,scrollColumn w,rowsInteraction w)
                      _->error "missing Downloads window"
                    selected=modifyActive (\w->w {selection=Selection 0 5,scrollRow=1,scrollColumn=2,
                      rowsInteraction=fmap (\(RowsInteraction rowId _)->RowsInteraction rowId True) (rowsInteraction w)}) progress
                captured<-captureCancel selected
                before<-evaluate (details selected) >>= makeStableName
                idle<-tickDebugger runtime captured
                after<-evaluate (details idle) >>= makeStableName
                check "idle Downloads preserves exact prepared Details snapshot" (before==after)
                check "Downloads window remains private and human-only" (not (guestKeyboardAllowed idle) && not (cellReadable (cellAccess idle 20 10)))
                report (DownloadProgress "Downloading more" 12 (Just 20))
                refreshed<-waitFor runtime "progress refresh" (has "Downloading more") selected {screenSize=(96,31)}
                check "progress and resize retain Details selection/scroll/focus" (detailState refreshed==detailState selected)
                let (closed,closeEffects)=runCommand Close refreshed
                escaped<-snd <$> debuggerEffects runtime core closed closeEffects >>= tickDebugger runtime
                check "close leaves transfer alive and progress cannot reopen" (activePluginWindow escaped==Nothing)
                reopened<-send runtime "downloads" [] escaped >>= waitFor runtime "reopened Downloads" (has "Downloading more")
                rejected<-awaitMenu "old closed cancel expires" ((=="Menu result expired; invoke it again.").status) reopened
                check "old closed window cannot cancel or replace reopened view" (fmap windowContent (activeWindow rejected)==fmap windowContent (activeWindow reopened))
                held<-captureCancel rejected
                report (DownloadProgress "Download still running" 15 (Just 20))
                advanced<-waitFor runtime "progress after captured cancel" (has "Download still running") held
                awaitMenu "captured cancel survives progress" (\d->status d=="Cancelling download..." || status d=="This download has already finished.") advanced
              "two-jobs"->do
                report<-takeMVar reportReady
                let currentRows desktop=case activeWindow desktop >>= windowRows desktop of
                      Just (rows,index,selected,_)->(rows,index,selected)
                      _->error "missing Downloads rows"
                    (_,_,firstId)=currentRows progress
                    cancelled rowId desktop=let (rows,index,_)=currentRows desktop in case M.lookup rowId index >>= (rows Vec.!?) of
                      Just (W.WindowRow _ caption _)->"Cancelled" `T.isInfixOf` caption
                      _->False
                held<-captureCancel progress
                -- A second consent queues a real distinct job while A's action
                -- is waiting for adoption; this must not retarget it to B.
                p<-port
                offered<-send runtime "launch-config" ["0","",p] held >>= waitFor runtime "second offer" (pure.offer)
                second<-submit runtime 0 offered >>= waitFor runtime "second transfer row" (\d->pure (let (rows,_,_)=currentRows d in Vec.length rows==2))
                let selectedB=fst (handleEvent (V.EvKey V.KDown []) second)
                    (_,_,secondId)=currentRows selectedB
                check "list UI selected distinct second job" (firstId/=secondId)
                report (DownloadProgress "A still running" 15 (Just 20))
                advanced<-waitFor runtime "A progress with B selected" (has "A still running") selectedB
                signalled<-awaitMenu "cancel captured A while B selected" ((=="Cancelling download...").status) advanced
                stopped<-waitFor runtime "only A cancelled" (pure . cancelled firstId) signalled
                check "captured Cancel targets A and preserves B selection/job" (let (_,_,selected)=currentRows stopped in selected==secondId && not (cancelled secondId stopped))
                pure stopped
              "retire"->do
                queued<-captureCancel progress
                retired<-retireMenuFromHost host cancelRef queued
                rejected<-awaitMenu "retired contribution rejects pending Cancel" ((=="Menu result expired; invoke it again.").status) retired
                check "retired Cancel contribution leaves transfer running" =<< has "Downloading" rejected
                pure rejected
              "toolchain"->pure progress {toolchain=Just THC}
              "project"->pure progress {defaultDirectory=Just library}
              "edit"->pure (insertText "x" (focusWindow (maybe (error "missing source") windowId (activeWindow base)) progress))
              "reload"->pure progress {buffers=M.map (\doc->doc {documentBuffer=newBuffer "main = print (1::Int)\n"}) (buffers progress)}
              "settings"->snd <$> debuggerEffects runtime core progress [ServiceAction "toolchain" ["GHC","ghc-9.12.4"]]
              "stop"->send runtime "disconnect" [] progress
              "newer"->send runtime "launch" [] progress
              _->pure progress
            observed<-tickDebugger runtime changed
            if mode `elem` ["cancel","two-jobs"] then do
              _<-waitFor runtime "cancelled" (has "Cancelled") observed
              cleaned<-timeout 1000000 (readMVar finished)
              check "cancel joins transfer cleanup off UI" (cleaned==Just ())
            else do
              putMVar gate ()
              if mode `elem` ["success","retire"] then do
                _<-waitFor runtime "original launch" (\_->T.isInfixOf "\"command\": \"launch\"" <$> TIO.readFile logFile) observed
                entries<-mapM (either error pure.eitherDecodeStrict'.encodeText) . T.lines =<< TIO.readFile logFile
                let launches=[args | entry<-entries,Just request<-[field "request" entry],field "command" request==Just ("launch"::T.Text),Just args<-[field "arguments" request]]
                check "success continues captured launch arguments" (any (\args->field "projectRoot" args==Just root && field "entryFile" args==Just ("Main.hs"::String) && field "entryArgs" args==Just (["original argument"]::[String])) launches)
                completed<-send runtime "downloads" [] observed
                readyDownloads<-waitFor runtime "prepared completed downloads" (has "Installed") completed
                when (mode=="success") $ do
                  queuedCancel<-captureCancel readyDownloads
                  noCancel<-awaitMenu "finished transfer cancel" ((=="This download has already finished.").status) queuedCancel
                  check "finished transfer never claims to be cancelling" (status noCancel=="This download has already finished.")
              else do
                _<-timeout 1000000 (readMVar finished) >>= maybe (error "download did not finish") pure
                -- Observe both completion and possible queued continuation.
                final<-foldTicks runtime 20 observed
                logText<-TIO.readFile logFile
                check ("stale "++mode++" never launches") (T.null logText && status final=="Debugger installed; the original launch is no longer current.")
      -- A closed Human package request enters the same acquisition slot, but
      -- its final launch re-enters the host gate and uses the captured main.
      reset
      executableFile (commands </> "cabal") ("#!/bin/sh\nprintf '%s\\n' '{\"compiler\":{\"flavour\":\"ghc\",\"id\":\"ghc-9.14.1\",\"path\":\""<>T.pack ghc<>"\"}}'\n")
      let manifest=root </> "sample.cabal"
      TIO.writeFile manifest "cabal-version: 3.0\nname: sample\nversion: 0.1\nexecutable first\n  main-is: Main.hs\n"
      withRegistry $ \registry->do
        provider<-P.registerTree registry "package-debug.fixture"
          (P.NodeDef (P.NodeInfo (either (error.T.unpack) id (P.nodeId "root")) "Package" "" True Nothing) Nothing [])
          (\() _->pure (Right (P.NodePage [] Nothing))) >>= either (error.show) pure
        signature<-(,) <$> getModificationTime manifest <*> getFileSize manifest
        let target=PackageBuildTarget (P.treeReference provider) 0 (root,[]) root manifest (Just signature) "sample:exe:first"
            otherFile=root </> "Other.hs"
        TIO.writeFile otherFile "main = print (42::Int)\n"
        let other=addDocument (Just (FileState otherFile Nothing)) (newBuffer "main = print (42::Int)\n") base
            launchRequest runtime d=snd <$> debuggerEffects runtime core d [PackageDebugAction target (Right source)]
            cradleFiles=filter (".hide-debug-cradle" `isPrefixOf`) <$> listDirectory root
            packetRequests=do
              entries<-mapM (either error pure.eitherDecodeStrict'.encodeText) . T.lines =<< TIO.readFile logFile
              pure [args | entry<-entries,Just request<-[field "request" entry],field "command" request==Just ("launch"::T.Text),Just args<-[field "arguments" request]]
        gate<-newEmptyMVar
        withDebuggerHdb (pure 0) (pure . Right . plan) (\_ report->install report gate) $ \runtime->do
          pending<-launchRequest runtime other >>= waitFor runtime "package offer" (pure.offer)
          before<-cradleFiles
          check "package offer has no owned cradle before consent" (null before)
          accepted<-submit runtime 0 pending
          putMVar gate ()
          prepared<-waitFor runtime "component preparation" (\d->pure ("prepared" `T.isInfixOf` status d)) accepted
          files<-cradleFiles
          check "one owned project-root cradle prepared" (length files==1)
          let modal=Dialog "Unrelated" (Searching False "") [Input "Find" "name" 4] 0 ["Find"] []
          held<-tickPreparedDebug runtime (debuggerEffects runtime core) prepared {dialog=Just modal}
          launchesBefore<-packetRequests
          check "modal postpones final launch" (null launchesBefore && fmap dialogTitle (dialog held)==Just "Unrelated")
          admitted<-tickPreparedDebug runtime (debuggerEffects runtime core) held {dialog=Nothing}
          launched<-waitFor runtime "captured component launch" (\_->not.null <$> packetRequests) admitted
          launches<-packetRequests
          check "Debug uses captured component main instead of active Other.hs" (any (\args->field "entryFile" args==Just ("Main.hs"::String) && field "projectRoot" args==Just root) launches)
          cradle<-case [path | args<-launches,Just path<-[field "cradleFile" args]] of path:_->pure path; _->error "launch omitted exact cradle"
          document<-BL.readFile cradle
          check "owned cradle preserves qualified target" ("sample:exe:first" `T.isInfixOf` Data.Text.Encoding.decodeUtf8 (BL.toStrict document))
          stopped<-send runtime "disconnect" [] launched
          _<-waitFor runtime "owned cradle retirement" (\_->null <$> cradleFiles) stopped
          pure ()
        -- Installation may finish, but a source replacement during consented
        -- acquisition must not continue the old captured component request.
        reset
        staleGate<-newEmptyMVar
        staleFinished<-newEmptyMVar
        withDebuggerHdb (pure 0) (pure . Right . plan)
          (\_ report->install report staleGate `finally` void (tryPutMVar staleFinished ())) $ \runtime->do
            pending<-launchRequest runtime other >>= waitFor runtime "stale package offer" (pure.offer)
            accepted<-submit runtime 0 pending
            before<-cradleFiles
            check "acquisition waits without creating a cradle" (null before)
            let changed=accepted {buffers=M.map (\doc->doc {documentBuffer=newBuffer "replacement"}) (buffers accepted)}
            observed<-tickDebugger runtime changed
            putMVar staleGate ()
            timeout 1000000 (readMVar staleFinished) >>= maybe (error "stale package download did not complete") pure
            final<-foldTicks runtime 20 observed >>= tickPreparedDebug runtime (debuggerEffects runtime core)
            launches<-packetRequests
            after<-cradleFiles
            check "stale package acquisition cannot revive or create cradle" (null launches && null after && T.isInfixOf "no longer current" (status final))
        -- A refused final host gate consumes the receipt and cleans its file.
        TIO.writeFile logFile ""
        withDebuggerHdb (pure 0) (pure . Right . plan) (\_ _->pure (Left "unexpected acquisition")) $ \runtime->do
          pending<-launchRequest runtime other
          prepared<-waitFor runtime "installed component preparation" (\d->pure ("prepared" `T.isInfixOf` status d)) pending
          refused<-tickPreparedDebug runtime (\d _->pure (False,d {status="Git gate refused"})) prepared
          _<-waitFor runtime "refused cradle retirement" (\_->null <$> cradleFiles) refused
          _<-tickPreparedDebug runtime (debuggerEffects runtime core) refused
          launches<-packetRequests
          check "refused launch is not replayed" (null launches)
        -- Custom governing cradle is never overwritten by captured Debug.
        TIO.writeFile (root </> "hie.yaml") "custom sentinel\n"
        withDebuggerHdb (pure 0) (pure . Right . plan) (\_ _->pure (Left "unexpected acquisition")) $ \runtime->do
          pending<-launchRequest runtime other
          _<-waitFor runtime "governing custom cradle refusal" (pure . T.isInfixOf "custom hie.yaml" . status) pending
          sentinel<-TIO.readFile (root </> "hie.yaml")
          check "custom governing cradle stays unchanged" (sentinel=="custom sentinel\n")
        removeFile (root </> "hie.yaml")
        -- Same numeric revision replacements invalidate the captured source.
        TIO.writeFile logFile ""
        withDebuggerHdb (pure 0) (pure . Right . plan) (\_ _->pure (Left "unexpected acquisition")) $ \runtime->do
          pending<-launchRequest runtime other
          prepared<-waitFor runtime "stale component preparation" (\d->pure ("prepared" `T.isInfixOf` status d)) pending
          let replaced=prepared {buffers=M.map (\doc->doc {documentBuffer=newBuffer "replacement"}) (buffers prepared)}
          _<-tickPreparedDebug runtime (debuggerEffects runtime core) replaced
          _<-waitFor runtime "stale source cradle retirement" (\_->null <$> cradleFiles) replaced
          launches<-packetRequests
          check "equal-revision replacement cannot launch" (null launches)
  putStrLn "Debugger acquisition checks passed"
  where
    check label okay=unless okay (error label)
    field key=parseMaybe (withObject "field" (.: key))
    quote value="'"<>T.replace "'" "'\\''" (T.pack value)<>"'"
    executableFile path text=do TIO.writeFile path text; permissions<-getPermissions path;setPermissions path permissions {executable=True}
    encodeText=Data.Text.Encoding.encodeUtf8
    foldTicks :: Debugger -> Int -> Desktop -> IO Desktop
    foldTicks _ 0 d=pure d
    foldTicks runtime n d=tickDebugger runtime d >>= \next->threadDelay 1000>>foldTicks runtime (n-1) next

withEnvironment :: [(String,String)] -> IO a -> IO a
withEnvironment entries action=bracket (mapM (\(name,_)->(name,) <$> lookupEnv name) entries)
  (mapM_ (\(name,value)->maybe (unsetEnv name) (setEnv name) value))
  (\_->mapM_ (uncurry setEnv) entries>>action)
