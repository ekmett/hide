{-# LANGUAGE OverloadedStrings #-}
module PackageSidebarCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO (openTempFile,hClose)
import System.Timeout (timeout)
import Hide.Files (filePath)
import Hide.Model
import Hide.PackageSidebar
import qualified Hide.Plugin.Tree as P
import Hide.Sidebar
import Hide.SidebarCommands

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->do
  let file=root </> "sample.cabal"
      mainFile=root </> "Main.hs"
  writeFile file (manifest "sample")
  writeFile mainFile "main = pure ()\n"
  withSidebarCommands $ \host->do
    let core d _=pure (False,d)
        initial=(initialDesktop (100,35)) {sideTree=Just (emptySidebar root 28 False)}
    withPackageSidebar host initial $ \provider->do
      let tick d=tickPackageSidebar provider host d >>= tickSidebar host core
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
      sources<-activate "exe:demo" targets >>= wait "Cabal source" (\d->any (\row->P.infoLabel (rowInfo row)=="Main.hs" && rowDepth row==2) (rows d) && ready d)
      unless (has "Missing (missing)" sources && has "Paths_sample (generated)" sources)
        (fail ("Missing/generated sources stay visible without a build: "<>show (map (P.infoLabel.rowInfo) (rows sources))))
      opened<-activate "Main.hs" sources >>= wait "source opens through host" (\d->(filePath <$> (activeDocument d >>= documentFile))==Just mainFile)
      -- The root's manifest action must remain usable after a manifest refresh.
      writeFile file (manifest "sample"<>"-- changed\n")
      sourceRow<-case [row | row<-rows opened,rowDepth row==2,P.infoLabel (rowInfo row)=="Main.hs"] of
        row:_->pure row
        _->fail "Source row disappeared"
      sourceAction<-maybe (fail "Source action missing") pure (rowCommand sourceRow)
      let sourceTrace=maybe [] (hitTrace (keyOf (rowHit sourceRow))) (sideTree opened)
          (staleRequest,staleEffects)=runCommand (TreeCommand sourceTrace sourceAction) opened {sideTree=fmap (\tree->tree {treeFocused=True}) (sideTree opened)}
      (_,staleQueued)<-sidebarEffects host core staleRequest staleEffects
      stale<-wait "changed manifest rejects old source action" (T.isInfixOf "Package source changed" . status) staleQueued
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
    manifest name=unlines ["cabal-version: 3.0","name: "<>name,"version: 0.1","executable demo","  main-is: Main.hs","  other-modules: Missing","  autogen-modules: Paths_sample"]
    temporary=do
      base<-getTemporaryDirectory
      (path,handle)<-openTempFile base "hide-package-sidebar"
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path
