-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- | Module      : InspectorWindowCheck
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- The inspector on real hide types: "Hide.PackageSources" declarations viewed
-- through "Hide.PackageInspect", rendered into the existing rows window by
-- "Hide.InspectorWindow", with row actions, view switching and stale handling.
module InspectorWindowCheck (checks) where

import Control.Monad (unless,forM_)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Hide.Buffer (contentLength,contentSlice)
import Hide.InspectorWindow
import Hide.PackageInspect
import Hide.Model (initialDesktop,windows,pluginWindows)
import Hide.PluginWindowHost (adoptWindowUpdate,replaceWindowUpdate)
import Hide.PackageSources
import Hide.Plugin.Inspect
import Hide.Plugin.Menu (MenuOrigin(..))
import qualified Hide.Plugin.Tree as Tree
import qualified Hide.Plugin.Window as W

manifest :: BS.ByteString
manifest=BS.unlines
  [ "cabal-version: 3.0", "name: demo", "version: 0.1"
  , "library", "  exposed-modules: Alpha Beta", "  other-modules: Gamma", "  hs-source-dirs: src"
  , "  if os(windows)", "    other-modules: Win"
  , "executable demo", "  main-is: Main.hs"
  , "test-suite tests", "  type: detailed-0.9", "  test-module: Tests" ]

packageOf :: BS.ByteString -> PackageSources
packageOf bytes=case parsePackageSources bytes of Right package->package; Left err->error (T.unpack err)

-- Adopt exactly as a host would: through the scoped publication primitive.
adopted :: Either Text W.WindowUpdate -> IO (W.WindowRef,W.PreparedWindow)
adopted (Left err)=ioError (userError ("inspect failed: "++T.unpack err))
adopted (Right update)=W.admitWindowUpdate False update >>= maybe (ioError (userError "window not admitted")) pure

rowsOf :: W.PreparedWindow -> [(Text,Text)]
rowsOf prepared=case W.preparedWindowRows prepared of
  W.RowsDetails rows _ _->[(caption,(let content=W.preparedWindowText detail' in contentSlice content 0 (contentLength content))) | W.WindowRow _ caption detail'<-V.toList rows]
  _->error "expected a rows window"

captions :: W.PreparedWindow -> [Text]
captions=map fst . rowsOf

nodeAt :: W.PreparedWindow -> Int -> Tree.NodeId
nodeAt prepared index=case W.preparedWindowRows prepared of
  W.RowsDetails rows _ _->case rows V.!? index of Just (W.WindowRow node _ _)->node; Nothing->error "no such row"
  _->error "expected a rows window"

checks :: IO ()
checks=do
  let check label ok=unless ok (error ("InspectorWindow: "++label))
      package=packageOf manifest
      human=HumanMenu
  -- The new edges satisfy the edge law on real data.
  check "component keys are unique" (checkEdge componentsEdge package)
  check "source keys are unique in every component" (all (checkEdge sourcesEdge) (sourceComponents package))
  check "package views are attachable without editing the type" (either (const False) (const True) packageContributions)
  revisions<-newIORef (Revision 1,package)
  withInspector $ \inspector->do
    let publisher=inspectorPublisher' inspector
        views=inspectorViews inspector
    _<-publishPackageViews publisher
    root<-either (error . show) id <$> publishRoot publisher (RootDef "hide.test.package" (Just <$> readIORef revisions))
    Just top<-rootSubject views root
    -- Package: default view lists the components, then offers the other views.
    (ref0,window0)<-inspectSubject inspector human top Nothing >>= adopted
    check "the default package view lists the components"
      (take 3 (captions window0)==["lib:demo","exe:demo","test:tests"])
    check "inspector windows are private by default" (W.preparedWindowDisclosure window0==W.PrivateWindow)
    check "a package without warnings offers no Warnings view" (not (any ("Warnings" `T.isInfixOf`) (captions window0)))
    check "the component list has its own row actions"
      (case W.preparedWindowRows window0 of W.RowsDetails rows _ _->V.length rows==3; _->False)
    action0<-pageAction inspector ref0 (nodeAt window0 1)
    case action0 of
      Just (Navigate component)->do
        check "a row's target is the typed component subject"
          (show (subjectType component)=="ComponentSources" && fmap (sourceTarget . hereValue) (subjectAs component)==Just "exe:demo")
        -- Navigate: sources of the library, with a switch to the other view.
        Just libraryTarget<-pure (componentSubject top "lib:demo")
        (ref1,window1)<-inspectSubject inspector human libraryTarget Nothing >>= adopted
        check "sources list unconditional declarations with their kind"
          (take 3 (captions window1)==["Alpha (module)","Beta (module)","Gamma (module)"])
        check "view switching is offered as a navigation row" ("→ Declaration view" `elem` captions window1)
        check "conditional branches are reported, never flattened"
          ("1 conditional branches omitted" `T.isInfixOf` W.preparedWindowTitle window1)
        let switch=length (captions window1)-1
        switchAction<-pageAction inspector ref1 (nodeAt window1 switch)
        case switchAction of
          Just (ShowView target view)->do
            (_,declaration)<-inspectSubject inspector human target (Just view) >>= adopted
            check "a text document becomes one row per heading, facts as Details"
              (take 1 (captions declaration)==["lib:demo"]
               && any (T.isInfixOf "Conditional branches: 1" . snd) (rowsOf declaration))
          _->error "InspectorWindow: the last row switches view"
        -- A source declaration is itself an inspectable object.
        sourceAction<-pageAction inspector ref1 (nodeAt window1 0)
        case sourceAction of
          Just (Navigate source)->do
            (_,sourceWindow)<-inspectSubject inspector human source Nothing >>= adopted
            check "a source's own view describes it" (any (T.isInfixOf "Generated: no" . snd) (rowsOf sourceWindow))
          _->error "InspectorWindow: sources navigate to their declarations"
        -- Exact row targeting: another window's node never resolves here.
        stranger<-pageAction inspector ref1 (nodeAt window0 0)
        check "the same row ID in another window resolves to that window's own target"
          (fmap actionType stranger==Just "Source" && fmap actionType action0==Just "ComponentSources")
        let reordered=package {sourceComponents=reverse (sourceComponents package)}
        writeIORef revisions (Revision 2,reordered)
        current<-currentRevision views component
        check "a changed root is detected by its scalar revision"
          (current==Just (Revision 2) && subjectRevision component==Revision 1)
        again<-resolve views component
        check "a reorder keeps the same component"
          (case again of Fresh s->s==component && fmap (sourceTarget . hereValue) (subjectAs s)==Just "exe:demo"; _->False)
        writeIORef revisions (Revision 3,package {sourceComponents=take 1 (sourceComponents package)})
        gone<-resolve views component
        check "a removed component is Gone, not another component" (case gone of Gone->True; _->False)
      _->error "InspectorWindow: component rows navigate"

    -- Warnings appear only when the parser reported some.
    let warned=package {sourceWarnings=["unknown field"]}
    warnedRoot<-either (error . show) id <$> publishRoot publisher (RootDef "hide.test.warned" (pure (Just (Revision 1,warned))))
    Just warnedTop<-rootSubject views warnedRoot
    (_,warnedWindow)<-inspectSubject inspector human warnedTop Nothing >>= adopted
    check "the Warnings view is offered once a warning exists" ("→ Warnings view" `elem` captions warnedWindow)

    -- Bounds: a view with hundreds of rows keeps the window within its limit.
    big<-either (error . T.unpack) pure (viewDef "hide.test.big" "Big" 98 (\_->(List "Big" [Item (T.pack (show i)) "d" Nothing | i<-[1..300::Int]])))
    _<-publishViews publisher (attach "hide.test.bigviews" [big :: ViewDef PackageSources])
    Just currentTop<-rootSubject views root
    (bigRef,bigWindow)<-inspectSubject inspector human currentTop (Just (viewIdOf "hide.test.big")) >>= adopted
    check "large documents stay within the rows-window bound"
      (case W.preparedWindowRows bigWindow of W.RowsDetails rows _ _->V.length rows<=64; _->False)
    let moreIndex=maybe (error "missing continuation") id (lookup "More…" (zip (captions bigWindow) [0..]))
    Right nextUpdate<-followInspectorRow inspector human bigRef (nodeAt bigWindow moreIndex)
    (_,nextWindow)<-adopted (Right nextUpdate)
    check "later rows are reachable" (take 1 (captions nextWindow)==[T.pack (show (maxItems+1))])
    writeIORef revisions (Revision 4,package)
    stalePage<-followInspectorRow inspector human bigRef (nodeAt bigWindow moreIndex)
    check "a continuation refuses a changed revision" (either (T.isInfixOf "changed") (const False) stalePage)

    -- Navigation reuses the host window while retiring the old action scope.
    Just freshTop<-rootSubject views root
    Right first<-inspectSubject inspector human freshTop Nothing
    desktop<-adoptWindowUpdate HumanMenu first (initialDesktop (100,30))
    let old=W.updateWindowRef first
        firstWindow=pluginWindows desktop M.! old
    Right replacement<-followInspectorRow inspector human old (nodeAt firstWindow 0)
    replaced<-replaceWindowUpdate HumanMenu old replacement desktop
    check "navigation keeps one visible window" (length (windows replaced)==length (windows desktop) && M.size (pluginWindows replaced)==1)
    staleAction<-pageAction inspector old (nodeAt firstWindow 0)
    check "replaced window actions cannot fire" (staleAction==Nothing)

    -- Row ids in a page are distinct.
    forM_ [window0] $ \w->check "row identities are unique"
      (case W.preparedWindowRows w of W.RowsDetails _ index _->M.size index==length (captions w); _->False)

  -- Closing the inspector retires its windows and tables.
  (escaped,reference,node)<-withInspector $ \inspector->do
    _<-publishPackageViews (inspectorPublisher' inspector)
    root<-either (error . show) id <$> publishRoot (inspectorPublisher' inspector) (RootDef "hide.test.closing" (pure (Just (Revision 1,package))))
    Just top<-rootSubject (inspectorViews inspector) root
    (ref,window)<-inspectSubject inspector human top Nothing >>= adopted
    live<-pageAction inspector ref (nodeAt window 0)
    check "a live window's rows resolve" (live/=Nothing)
    pure (inspector,ref,nodeAt window 0)
  afterClose<-pageAction escaped reference node
  check "a closed inspector admits no row action" (afterClose==Nothing)
  putStrLn "inspector window checks passed"

viewIdOf :: Text -> ViewId
viewIdOf name=case mkViewId name of Right ident->ident; Left err->error (T.unpack err)

actionType :: RowAction -> Text
actionType (Navigate subject)=T.pack (show (subjectType subject))
actionType (PageRows subject _ _)=T.pack (show (subjectType subject))
actionType (ShowView subject _)=T.pack (show (subjectType subject))
actionType (OpenPath _)="path"
actionType OpenSite{}="site"
actionType GoBack="back"
