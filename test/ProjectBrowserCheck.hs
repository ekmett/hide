{-# LANGUAGE OverloadedStrings #-}
module ProjectBrowserCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.Directory
import System.FilePath ((</>))
import System.IO (openTempFile,hClose)
import System.Timeout (timeout)
import THC.Edit.Model
import THC.Edit.ProjectBrowser
import THC.Edit.Render (snapshot)

checks :: IO ()
checks=bracket fixture removePathForcibly $ \root->withProjectBrowser $ \browser->do
  let desktop=(initialDesktop (80,25)) {defaultDirectory=Just root}
      perform d effects=snd <$> projectBrowserEffects browser (\s _->pure (False,s)) d effects
      launch d=uncurry perform (runCommand ProjectBrowser d)
      settle d=do
        result<-timeout 3000000 (loop d)
        maybe (error "Cabal browser read timed out") pure result
      loop d=do
        next<-tickProjectBrowser browser d
        case purpose <$> dialog next of
          Just ProjectLoading{}->threadDelay 1000 >> loop next
          _->pure next
      check label condition=unless condition (error label)
      press button d=uncurry perform (submitDialog button (fromJust (dialog d)) d)
      file=root </> "dist-newstyle/cache/plan.json"
      component n=object ["id" .= ("local-"<>T.pack (show n)),"pkg-name" .= ("sample"::T.Text),"pkg-version" .= ("1"::T.Text),"style" .= ("local"::T.Text),"component-name" .= ("exe:tool-"<>T.pack (show n)),"depends" .= ["base-unit"::T.Text],"pkg-src" .= object ["type" .= ("local"::T.Text),"path" .= root]]
      nested=object ["id" .= ("nested"::T.Text),"pkg-name" .= ("nested"::T.Text),"style" .= ("local"::T.Text),"components" .= object ["lib" .= object ["depends" .= ["missing-unit"::T.Text]],"setup" .= object ["depends" .= ["base-unit"::T.Text]]]]
      base=object ["id" .= ("base-unit"::T.Text),"pkg-name" .= ("base"::T.Text),"pkg-version" .= ("4"::T.Text),"type" .= ("pre-existing"::T.Text)]
      plan rows=object ["compiler-id" .= ("ghc-test"::T.Text),"install-plan" .= rows]
      write rows=BL.writeFile file (encode (plan rows))
  check "Project browser menu is enabled" (any (\(MenuItem _ _ command)->command==ProjectBrowser) [item | (_,_,items)<-menus,item<-items])
  missing<-launch desktop >>= settle
  check "Missing plan explains no build and canonical path" (all (`T.isInfixOf` snapshot missing) ["missing","No build was started.","dist-newstyle/cache/plan.json"])
  created<-doesPathExist file
  check "Browser does not create a Cabal plan" (not created)
  createDirectoryIfMissing True (root </> "dist-newstyle/cache")
  write (map component [1..34::Int]++[nested,base])
  loading<-launch desktop
  check "Browser read begins with a cancellable loading dialog" (case purpose <$> dialog loading of Just ProjectLoading{}->True; _->False)
  loaded<-settle loading
  check "80x25 chooser shows compiler, component count and buttons" (all (`T.isInfixOf` snapshot loaded) ["ghc-test","36 components","Details","Prev","Next","Refresh"])
  check "Chooser buttons fit inside the 80x25 dialog" (case dialog loaded of Just dg->let box=dialogRect loaded dg in all (\r->left r>left box && left r+width r+1<left box+width box-1) (buttonRects loaded dg); _->False)
  check "Chooser page contains at most32 entries" (case fields <$> dialog loaded of Just [ListBox _ values _]->length values==32; _->False)
  second<-press 2 loaded
  check "Next page lists remaining components" (case fields <$> dialog second of Just [ListBox _ values _]->length values==4 && last values=="nested / setup"; _->False)
  let nestedChosen=second {dialog=fmap (\dg->dg {fields=[ListBox "Components" ["a","b","lib","setup"] 2]}) (dialog second)}
  detailsView<-press 0 nestedChosen
  check "Nested component details preserve exact unresolved dependencies" (all (`T.isInfixOf` activeText detailsView) ["Component: lib","missing-unit [unresolved]","does not build"])
  check "Details are read only" (activeText (insertText "changed" detailsView)==activeText detailsView)
  pending<-launch desktop
  let dismissed=fst (handleEvent (V.EvKey V.KEsc []) pending)
  afterDismiss<-tickProjectBrowser browser dismissed
  check "Dismissed loading dialog never reappears" (afterDismiss==dismissed)
  createDirectory (root </> "other")
  stale<-launch desktop >>= \d->settle d {defaultDirectory=Just (root </> "other")}
  check "Changed project discards completed read" (dialog stale==Nothing && "Project changed" `T.isInfixOf` status stale)
  private<-launch desktop >>= \d->settle d {guestPrivatePaths=[file]}
  check "Changed privacy policy discards completed read" (dialog private==Nothing && "Project changed" `T.isInfixOf` status private)
  BL.writeFile file "not JSON"
  invalid<-launch desktop >>= settle
  check "Invalid plan shows a bounded error" ("invalid" `T.isInfixOf` snapshot invalid && not ("not JSON" `T.isInfixOf` snapshot invalid))
  write [component (1::Int),base]
  writeFile (root </> "sample.cabal") "name: sample\n"
  future<-addUTCTime 60 <$> getCurrentTime
  setModificationTime (root </> "sample.cabal") future
  oldPlan<-launch desktop >>= settle
  check "Newer manifests visibly mark the plan stale" ("stale" `T.isInfixOf` snapshot oldPlan)
  write (map component [1..4100::Int])
  truncated<-launch desktop >>= settle
  check "Bounded graph omissions are visible" ("Incomplete plan snapshot" `T.isInfixOf` snapshot truncated)
  where
    fixture=do
      temp<-getTemporaryDirectory
      (path,handle)<-openTempFile temp "thc-cabal-browser-"
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path
