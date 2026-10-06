{-# LANGUAGE OverloadedStrings #-}
module PackageSidebarCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless,when,foldM)
import Data.IORef
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import System.Environment (lookupEnv,setEnv,unsetEnv)
import Hide.Buffer
import qualified Hide.Build as B
import Hide.Conversation
import Hide.Plugin.BufferHost (captureVersion,versionCurrent)
import qualified Hide.Plugin.Menu as Menu
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO (openTempFile,hClose)
import System.Timeout (timeout)
import System.Info (os)
import qualified Hide.Files
import Hide.Files (filePath)
import Hide.Model
import Hide.PackageSidebar
import qualified Hide.Plugin.Tree as P
import qualified Hide.Plugin.Window as W
import Hide.Sidebar
import Hide.SidebarCommands

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->do
  let file=root </> "sample.cabal"
      mainFile=root </> "Main.hs"
  writeFile file (manifest "sample")
  writeFile mainFile "main = pure ()\n"
  withEnv "XDG_CONFIG_HOME" (Just (root </> "config")) $ withEnv "THC_EDIT_SESSION" Nothing $ withSidebarCommands $ \host->withConversationAt root $ \runtime->do
    let initial=(initialDesktop (100,35)) {sideTree=Just (emptySidebar root 28 False)}
    withPackageSidebar host initial $ \provider->do
      captured<-newIORef Nothing
      attempted<-newIORef False
      let core d effects=do
            when (any (\effect->case effect of AdoptPreparedBuild (Just _)->True; _->False) effects) (writeIORef attempted True)
            packageBuildEffects provider (\value pending->do
              mapM_ (\effect->case effect of PackageBuildAction _ target->writeIORef captured (Just target); _->pure ()) pending
              conversationEffects runtime (\state _->pure (False,state)) value pending) d effects
          sidebarTick d=tickPackageSidebar provider host d >>= tickSidebar host core
          tick d=tickConversation runtime d >>= tickBuildPreparation runtime core >>= sidebarTick
          wait label predicate current=timeout 5000000 (loop current) >>= maybe (fail (label<>" timed out")) pure
            where loop d=do next<-tick d; if predicate next then pure next else threadDelay 1000 >> loop next
          rows d=maybe [] (M.elems . treeRows) (sideTree d)
          has label=any ((==label).P.infoLabel.rowInfo).rows
          activate label d=case [i | (i,row)<-zip [0..] (rows d),P.infoLabel (rowInfo row)==label,(label/="Main.hs" || rowDepth row==2)] of
            i:_->let (next,effects)=activateTree True i d in snd <$> sidebarEffects host core next effects
            _->fail ("Missing package row: "<>T.unpack label)
          ready d=maybe False (\tree->treeProjectionRevision tree==treeRevision tree) (sideTree d)
      started<-initializeSidebar host initial
      mounted<-wait "package root" (has "sample") started
      targets<-activate "sample" mounted >>= wait "Cabal target" (has "exe:demo")
      let componentActions label=[title | row<-rows targets,P.infoLabel (rowInfo row)==label,(title,_)<-rowActions row]
      unless ("Build" `elem` componentActions "exe:demo" && "Run" `elem` componentActions "exe:demo" && "Build" `elem` componentActions "lib:sample" && "Run" `notElem` componentActions "lib:sample" && componentActions "test:check"==["Test"] && componentActions "test:driver"==["Test"] && componentActions "bench:measure"==["Benchmark"] && null (componentActions "exe:disabled"))
        (fail "Cabal components expose only their supported captured actions")
      sources<-activate "exe:demo" targets >>= wait "Cabal source" (\d->any (\row->P.infoLabel (rowInfo row)=="Main.hs" && rowDepth row==2) (rows d) && ready d)
      unless (has "Missing (missing)" sources && has "Paths_sample (generated)" sources)
        (fail ("Missing/generated sources stay visible without a build: "<>show (map (P.infoLabel.rowInfo) (rows sources))))
      sourceOpened<-activate "Main.hs" sources >>= wait "source opens through host" (\d->(filePath <$> (activeDocument d >>= documentFile))==Just mainFile)
      afterBuilds<-if os=="mingw32" then pure sourceOpened else do
        let configPath=root </> "config/thc-edit/run.json"
            compiler=root </> "chosen compiler"
            invocation=root </> "invocation"
            config=B.BuildConfig THC compiler "exe:not-selected" "compiler root" "runtime path" ["program argument"]
            bytes=A.encode (B.buildConfigValue root config)
            request label action d=case [row | row<-rows d,P.infoLabel (rowInfo row)==label] of
              row:_->case [ref | (title,P.RegisteredAction ref)<-rowActions row,title==action] of
                reference:_->do
                  writeIORef captured Nothing
                  let trace=maybe [] (hitTrace (keyOf (rowHit row))) (sideTree d)
                      (requested,effects)=runCommand (TreeCommand trace reference) d {sideTree=fmap (\tree->tree {treeFocused=True}) (sideTree d)}
                  snd <$> sidebarEffects host core requested effects
                _->fail "Component action missing"
              _->fail "Component row missing"
            admitted d=timeout 5000000 (loop d) >>= maybe (fail "Captured package action did not enter build owner") pure
              where loop current=do
                      next<-sidebarTick current
                      receipt<-readIORef captured
                      if receipt/=Nothing then pure next else threadDelay 1000 >> loop next
            waitInvocation d=timeout 5000000 (loop d) >>= maybe (fail "Captured target did not execute") pure
              where loop current=do
                      next<-tick current
                      exists<-doesFileExist invocation
                      if exists then pure next else threadDelay 1000 >> loop next
        writeFile compiler ("#!/bin/sh\nprintf '%s\n' \"$PWD\" \"$@\" > '"<>invocation<>".tmp'\nmv '"<>invocation<>".tmp' '"<>invocation<>"'\nprintf 'component output\n'\n")
        permissions<-getPermissions compiler
        setPermissions compiler permissions {executable=True}
        createDirectoryIfMissing True (takeDirectory configPath)
        BL.writeFile configPath bytes
        let otherFile=root </> "Other.hs"
            other=addDocument (Just (Hide.Files.FileState otherFile Nothing)) (newBuffer "other source") sourceOpened
        writeFile otherFile "other source"
        built<-request "lib:sample" "Build" other >>= admitted >>= waitInvocation
        arguments<-lines <$> readFile invocation
        unless (arguments==[root,"build","sample:lib:sample","--project-dir",root,"--thc-root","compiler root"])
          (fail "Library Build uses captured qualified target/root and selected compiler")
        saved<-BL.readFile configPath
        unless (saved==bytes) (fail "Component target override must not replace run settings")
        -- Retire the job, then request a different executable while Other.hs stays active.
        idle<-wait "captured build completes" (T.isInfixOf "completed." . status) built
        removeFile invocation
        running<-request "exe:second" "Run" idle >>= admitted >>= waitInvocation
        runArguments<-lines <$> readFile invocation
        unless (runArguments==[root,"run","sample:exe:second","--project-dir",root,"--thc-root","compiler root","--runtime","runtime path","--","program argument"])
          (fail "Executable Run uses captured target rather than focused source or saved target")
        stopped<-stopConversationBuild runtime running
        -- Dirty source is captured before preparation: no target process is launched.
        removeFile invocation
        readyToReuse<-timeout 5000000 (let loop current=do next<-tick current; pending<-buildTerminalLaunchPending runtime; if pending then threadDelay 1000 >> loop next else pure next in loop stopped) >>= maybe (fail "Run preparation did not retire") pure
        -- These component actions must refuse THC, rather than silently using
        -- Cabal/GHC or changing the saved toolchain.
        afterRefusals<-foldM (\current (label,action)->do
          refused<-request label action current {dialog=Nothing} >>= admitted >>= wait "THC runner refusal" (maybe False (const True) . dialog)
          spawned<-doesFileExist invocation
          savedConfig<-BL.readFile configPath
          unless (not spawned && savedConfig==bytes) (fail "Unsupported THC runner must not spawn or rewrite settings")
          pure refused {dialog=Nothing}) readyToReuse [("test:check","Test"),("bench:measure","Benchmark")]
        -- A Cabal fixture records the real package-action job boundary; it does
        -- not qualify Cabal itself. Other.hs remains the focused source.
        let cabalBin=root </> "bin"
            cabal=cabalBin </> "cabal"
            ghcConfig=B.BuildConfig GHC compiler "exe:not-selected" "compiler root" "runtime path" ["program argument"]
            ghcBytes=A.encode (B.buildConfigValue root ghcConfig)
        createDirectory cabalBin
        writeFile cabal ("#!/bin/sh\nprintf '%s\n' \"$PWD\" \"$@\" > '"<>invocation<>".tmp'\nmv '"<>invocation<>".tmp' '"<>invocation<>"'\nprintf 'captured runner output\n'\nif [ \"$1\" = bench ]; then while :; do sleep 1; done; fi\n")
        cabalPermissions<-getPermissions cabal
        setPermissions cabal cabalPermissions {executable=True}
        BL.writeFile configPath ghcBytes
        path<-lookupEnv "PATH"
        afterRunners<-withEnv "PATH" (Just (cabalBin<>[searchPathSeparator]<>maybe "" id path)) $ do
          tested<-request "test:check" "Test" afterRefusals >>= admitted >>= waitInvocation
          testArguments<-lines <$> readFile invocation
          unless (testArguments==[root,"test","--with-compiler="<>compiler,"--test-show-details=direct","sample:test:check"])
            (fail "Test uses captured qualified component/root and selected GHC")
          testDone<-wait "captured Test completes" (T.isInfixOf "completed." . status) tested
          removeFile invocation
          driver<-request "test:driver" "Test" testDone >>= admitted >>= waitInvocation
          driverArguments<-lines <$> readFile invocation
          unless (last driverArguments=="sample:test:driver") (fail "Detailed Test delegates its captured component to Cabal")
          driverDone<-wait "captured driver Test completes" (T.isInfixOf "completed." . status) driver
          removeFile invocation
          benchmarked<-request "bench:measure" "Benchmark" driverDone >>= admitted >>= waitInvocation
          benchArguments<-lines <$> readFile invocation
          unless (benchArguments==[root,"bench","--with-compiler="<>compiler,"sample:bench:measure"])
            (fail "Benchmark uses captured component without Run program/runtime arguments")
          visible<-wait "captured Benchmark output" (maybe False (\window->let text=W.preparedWindowText window in "captured runner output" `T.isInfixOf` contentSlice text 0 (contentLength text)) . activePluginWindow) benchmarked
          cancelled<-stopConversationBuild runtime visible
          wait "captured Benchmark Stop" (T.isInfixOf "Stopped." . status) cancelled
        unchangedConfig<-BL.readFile configPath
        unless (unchangedConfig==ghcBytes) (fail "Test/Benchmark must not replace the saved target")
        removeFile invocation
        let focused=maybe afterRunners (\window->focusWindow (windowId window) afterRunners) (activeWindow other)
            dirty=editActive (\sel->replaceSelection sel "changed") Nothing focused
        blocked<-request "test:check" "Test" dirty >>= admitted >>= wait "dirty target refusal" (maybe False (const True) . dialog)
        exists<-doesFileExist invocation
        unless (not exists) (fail "Dirty source cannot launch a captured package build")
        let reopened=blocked {dialog=Nothing}
        -- A component has no agent-readable resource: even an advertised tree is
        -- refused by the existing host boundary, before its Human-only handler.
        row<-case [row | row<-rows reopened,P.infoLabel (rowInfo row)=="exe:demo"] of
          row:_->pure row; _->fail "Executable row missing"
        reference<-case [ref | ("Build",P.RegisteredAction ref)<-rowActions row] of
          ref:_->pure ref; _->fail "Build action missing"
        let P.TreeHit owner _ _=rowHit row
            trace=maybe [] (hitTrace (keyOf (rowHit row))) (sideTree reopened)
            advertised=reopened {status="agent action pending",sideTree=fmap (\tree->tree {treeFocused=True,treeAgentRefs=[owner]}) (sideTree reopened)}
        writeIORef captured Nothing
        (_,agentQueued)<-sidebarEffects host core advertised [InvokeTree trace reference Menu.AgentMenu]
        agentRefused<-wait "agent target refusal" ((=="Sidebar action is stale, protected or busy.").status) agentQueued
        guestTarget<-readIORef captured
        unless (guestTarget==Nothing) (fail "Agent sidebar action must never acquire human build authority")
        -- Freeze a current intent, then advance its package snapshot before adoption.
        let clean=agentRefused {dialog=Nothing,buffers=buffers other}
            rootHit d=case [rowHit row | row<-rows d,P.infoLabel (rowInfo row)=="sample"] of hit:_->Just hit; _->Nothing
        pending<-request "bench:measure" "Benchmark" clean >>= admitted
        writeIORef attempted False
        writeFile file (manifest "sample"<>"-- new captured snapshot\n")
        refreshed<-timeout 5000000 (let loop d=do next<-sidebarTick d; if rootHit next/=rootHit pending && rootHit next/=Nothing then pure next else threadDelay 1000 >> loop next in loop pending) >>= maybe (fail "Package snapshot did not refresh") pure
        refused<-withEnv "PATH" (Just (cabalBin<>[searchPathSeparator]<>maybe "" id path)) $ timeout 5000000 (let loop d=do next<-tick d; reached<-readIORef attempted; if reached then pure next else threadDelay 1000 >> loop next in loop refreshed) >>= maybe (fail "Stale captured target did not reach its host gate") pure
        staleLaunch<-doesFileExist invocation
        unless (not staleLaunch) (fail "A changed package snapshot cannot execute a captured target")
        -- Keep the original source/navigation lifecycle checks on the fresh projection.
        restored<-wait "fresh component projection" (has "exe:demo") refused
        if any (\row->P.infoLabel (rowInfo row)=="Main.hs" && rowDepth row==2) (rows restored)
          then pure restored
          else activate "exe:demo" restored >>= wait "fresh source projection" (\d->any (\row->P.infoLabel (rowInfo row)=="Main.hs" && rowDepth row==2) (rows d))
      let opened=afterBuilds

      -- The root's manifest action must remain usable after a manifest refresh.
      writeFile file (manifest "sample"<>"-- changed\n")
      sourceRow<-case [row | row<-rows opened,rowDepth row==2,P.infoLabel (rowInfo row)=="Main.hs"] of
        row:_->pure row
        _->fail "Source row disappeared"
      sourceAction<-maybe (fail "Source action missing") pure (rowCommand sourceRow)
      (sourceId,sourceDocument)<-case [(bid,doc) | (bid,doc)<-M.toList (buffers opened),(filePath <$> documentFile doc)==Just mainFile] of
        entry:_->pure entry; _->fail "Opened source identity missing"
      sourceVersion<-captureVersion (documentBuffer sourceDocument)
      let sourceTrace=maybe [] (hitTrace (keyOf (rowHit sourceRow))) (sideTree opened)
          (staleRequest,staleEffects)=runCommand (TreeCommand sourceTrace sourceAction) opened {status="Checking stale source",sideTree=fmap (\tree->tree {treeFocused=True}) (sideTree opened)}
      (_,staleQueued)<-sidebarEffects host core staleRequest staleEffects
      -- Worker stamp rejection or an expired snapshot both refuse this intent.
      stale<-wait "changed manifest rejects old source action" (\d->T.isPrefixOf "Sidebar action failed:" (status d) || status d=="Sidebar result expired." || status d=="Sidebar action expired.") staleQueued
      unchanged<-maybe (pure False) (versionCurrent sourceVersion . documentBuffer) (M.lookup sourceId (buffers stale))
      unless (unchanged && (windowId <$> activeWindow stale)==(windowId <$> activeWindow opened))
        (fail "Refused source action cannot replace content or change focused window")
      threadDelay 600000
      refreshed<-wait "refreshed package projection" ready =<< tick stale
      packageRow<-case [row | row<-rows refreshed,P.infoLabel (rowInfo row)=="sample"] of
        row:_->pure row
        _->fail "Package root disappeared"
      reference<-case [ref | (_,P.RegisteredAction ref)<-rowActions packageRow] of
        ref:_->pure ref
        _->fail "Package manifest action missing"
      let trace=maybe [] (hitTrace (keyOf (rowHit packageRow))) (sideTree refreshed)
          (requested,effects)=runCommand (TreeCommand trace reference) refreshed {sideTree=fmap (\tree->tree {treeFocused=True}) (sideTree refreshed)}
      (_,queued)<-sidebarEffects host core requested effects
      manifestOpened<-wait "updated manifest remains openable" (\d->(filePath <$> (activeDocument d >>= documentFile))==Just file) queued
      writeFile file (manifest "renamed")
      renamed<-wait "renamed package root" (\d->has "renamed" d && not (has "sample" d)) manifestOpened
      createDirectory (root </> "empty")
      let moved=renamed {sideTree=fmap (\tree->tree {treeRoot=root </> "empty"}) (sideTree renamed)}
      _<-wait "old package retires after changing directory" (not . has "renamed") moved
      pure ()
  putStrLn "package sidebar checks passed"
  where
    manifest name=unlines ["cabal-version: 3.0","name: "<>name,"version: 0.1","library","  exposed-modules: Main","executable disabled","  main-is: Main.hs","  buildable: False","  if os(linux)","    buildable: True","executable second","  main-is: Other.hs","test-suite check","  type: exitcode-stdio-1.0","  main-is: Main.hs","test-suite driver","  type: detailed-0.9","  test-module: Main","benchmark measure","  type: exitcode-stdio-1.0","  main-is: Main.hs","executable demo","  main-is: Main.hs","  other-modules: Missing","  autogen-modules: Paths_sample"]
    temporary=do
      base<-getTemporaryDirectory
      (path,handle)<-openTempFile base "hide-package-sidebar"
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path

withEnv :: String -> Maybe String -> IO a -> IO a
withEnv key value action=bracket (lookupEnv key <* set value) set (const action)
  where set=maybe (unsetEnv key) (setEnv key)
