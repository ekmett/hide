-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | Module      : PluginInspectCheck
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings, ScopedTypeVariables
--
-- The typed inspector core, through its public API only: pure observation and
-- navigation laws, exact type dispatch, scoped registration, stale references
-- and the bounded, contained document preparation.
module PluginInspectCheck (checks) where

import Control.Category ((>>>))
import qualified Control.Category as C
import Control.Monad (unless)
import Data.IORef
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Plugin.Inspect

-- A fixture type we attach views to without touching it.
data Pkg = Pkg { pkgName :: Text, pkgComps :: [Comp] }
data Comp = Comp { compName :: Text, compMods :: [Text] } deriving (Eq,Show)

-- A hand-written traversal; any getter/lens/traversal/fold fits 'Observe'.
compsO :: Observe Pkg Comp
compsO f (Pkg name cs)=Pkg name <$> traverse f cs
modsO :: Observe Comp Text
modsO f (Comp name ms)=Comp name <$> traverse f ms
-- An ordinary polymorphic lens, as another optics library would write it.
nameL :: Functor f => (Text -> f Text) -> Pkg -> f Pkg
nameL f (Pkg name cs)=(\n->Pkg n cs) <$> f name

fromRight' :: Show e => Either e a -> a
fromRight' (Right a)=a
fromRight' (Left e)=error (show e)

compEdge :: Edge Pkg Comp
compEdge=fromRight' (edge "example.pkg.components" "Components" compsO (Key . compName))
modEdge :: Edge Comp Text
modEdge=fromRight' (edge "example.comp.modules" "Modules" modsO Key)

checks :: IO ()
checks=do
  let check label ok=unless ok (error ("PluginInspect: "++label))
      pkg=Pkg "demo" [Comp "lib:demo" ["A","B"],Comp "exe:demo" ["Main"]]
      reordered=Pkg "demo" (reverse (pkgComps pkg))
  -- Pure observations and keyed navigation.
  check "a fold observes every element in order" (map compName (observe compsO pkg)==["lib:demo","exe:demo"])
  check "a polymorphic lens is an observation" (observe nameL pkg==["demo"])
  check "a unique key function is accepted" (checkEdge compEdge pkg)
  check "a colliding key function is rejected"
    (not (checkEdge (fromRight' (edge "example.bad.components" "Bad" compsO (const (Key (0::Int))))) pkg))
  check "invalid edge identifiers are refused" (isLeft (edge "NotNamespaced" "x" compsO (Key . compName)))
  let libPath=step compEdge (Key ("lib:demo"::Text))
      exePath=step compEdge (Key ("exe:demo"::Text))
  check "navigation resolves by key" (fmap compName (navResolve libPath pkg)==Just "lib:demo")
  check "reordering the source cannot retarget a path" (fmap compName (navResolve libPath reordered)==Just "lib:demo")
  check "removing the element yields no object, not another one"
    (fmap compName (navResolve exePath (Pkg "demo" (take 1 (pkgComps pkg))))==Nothing)
  let deep=libPath >>> step modEdge (Key ("B"::Text))
  check "navigation composes as a category" (navResolve deep pkg==Just "B" && length (navSteps deep)==2)
  check "category identity" (navSteps (C.id :: Nav Pkg Pkg)==[] && navResolve (C.id >>> libPath) pkg==navResolve libPath pkg
    && navResolve (libPath >>> C.id) pkg==navResolve libPath pkg)
  check "category associativity"
    (let a=libPath; b=step modEdge (Key ("A"::Text)) in navSteps ((a >>> b) >>> C.id)==navSteps (a >>> (b >>> C.id)))
  check "keys of different types never alias" (Key (1::Int)/=Key (1::Integer) && Key ("a"::Text)==Key ("a"::Text) && Key (1::Int)<Key (2::Int))

  -- Registration, discovery, retirement.
  withInspectors $ \inspectors->do
    let publisher=inspectPublisher inspectors

    let pkgView name title order=fromRight' (viewDef name title order (\here->(Text [Heading (pkgName (hereValue here))])))
        compView=(fromRight' (viewDef "example.comp.detail" "Detail" 0 (\here->(Text [Prose (compName (hereValue here))])))) :: ViewDef Comp
        warnView=(pkgView "example.pkg.empty" "Empty" 5) {viewApplies=null . pkgComps}
        nested=(pkgView "example.pkg.nested" "Nested" 1)
    root<-either (error . show) id <$> publishRoot publisher (RootDef "example.pkg" (pure (Just (Revision 7,pkg))))
    Just top<-rootSubject inspectors root
    check "the root subject carries its revision and type"
      (subjectRevision top==Revision 7 && show (subjectType top)=="Pkg")
    firstViews<-publishViews publisher (attach "example.pkg.views" [pkgView "example.pkg.overview" "Overview" 2,warnView,nested])
    ref<-either (error . show) id <$> pure firstViews
    publishViews publisher (attach "example.comp.views" [compView]) >>= either (error . show) (const (pure ()))
    listed<-viewsFor inspectors top
    check "views are ordered by (order, id)" (map (viewIdText . infoView) listed==["example.pkg.nested","example.pkg.overview","example.pkg.empty"])
    applicable<-applicableViews inspectors top
    check "inapplicable views are filtered on a worker" (map (viewIdText . infoView) applicable==["example.pkg.nested","example.pkg.overview"])
    dup<-publishViews publisher (attach "example.pkg.other" [pkgView "example.pkg.overview" "Again" 0])
    check "a view ID is unique across contributions" (case dup of Left DuplicateView{}->True; _->False)
    again<-publishViews publisher (attach "example.pkg.views" [pkgView "example.pkg.fresh" "Fresh" 0])
    check "a live contribution name is not replaced" (case again of Left DuplicateContribution{}->True; _->False)
    check "an invalid contribution name is refused" . (\r->case r of Left InvalidContribution{}->True; _->False)
      =<< publishViews publisher (attach "Bad Name" [pkgView "example.pkg.x" "X" 0])
    -- Exact type dispatch: Comp objects see only Comp views; a Maybe at another instance sees none.
    Just topHere<-pure (subjectAs top)
    [libHere]<-pure [h | h<-follow compEdge topHere, compName (hereValue h)=="lib:demo"]
    let libSubject=Subject libHere
    compViews<-viewsFor inspectors libSubject
    check "views attach to exactly the object's type" (map (viewIdText . infoView) compViews==["example.comp.detail"])
    check "an unrelated type has no views"
      . null =<< viewsFor inspectors (Subject (detached (Just (3::Int))))
    check "instances of a polymorphic type are distinct"
      . null =<< viewsFor inspectors (Subject (detached (Just ("x"::Text))))
    body<-openView inspectors top (fromRight' (mkViewId "example.pkg.overview"))
    check "a view body builds from the subject" (case body of Right ((Text [Heading "demo"]))->True; _->False)
    missing<-openView inspectors libSubject (fromRight' (mkViewId "example.pkg.overview"))
    check "a view of another type is unknown here" (case missing of Left UnknownView{}->True; _->False)
    -- Listing does not evaluate the payload.
    let poisoned=Subject (detached (error "payload forced" :: Pkg))
    listedPoisoned<-viewsFor inspectors poisoned
    check "discovery never forces the object" (length listedPoisoned==3)
    -- Retirement.
    withdrawViews publisher ref
    afterWithdraw<-viewsFor inspectors top
    check "withdrawn views vanish" (null afterWithdraw)
    reuse<-publishViews publisher (attach "example.pkg.views" [pkgView "example.pkg.overview" "Overview" 2])
    check "withdrawal frees names for a fresh contribution" (either (const False) (const True) reuse)

    -- Stale references.
    r1<-resolve inspectors libSubject
    check "a root-created subject resolves while its root is current"
      (case r1 of Fresh s->s==libSubject && subjectRevision s==Revision 7; _->False)
    revisions<-newIORef (Revision 1,pkg)
    changing<-either (error . show) id <$> publishRoot publisher (RootDef "example.changing" (Just <$> readIORef revisions))
    Just c0<-rootSubject inspectors changing
    Just c0here<-pure (subjectAs c0)
    let comps0=follow compEdge c0here
    check "the changing root starts at revision 1" (subjectRevision c0==Revision 1 && length comps0==2)
    target<-case [Subject h | h<-comps0, compName (hereValue h)=="exe:demo"] of t:_->pure t; []->error "fixture"
    writeIORef revisions (Revision 2,reordered)
    now<-currentRevision inspectors target
    check "a revision change is a scalar comparison" (now==Just (Revision 2) && subjectRevision target==Revision 1)
    reresolved<-resolve inspectors target
    check "re-resolution follows the key through a reorder" (case reresolved of
      Fresh s->s==target && subjectRevision s==Revision 2 && show (subjectType s)=="Comp"; _->False)
    writeIORef revisions (Revision 3,Pkg "demo" (take 1 (pkgComps pkg)))
    gone<-resolve inspectors target
    check "a deleted element is Gone, never another object" (case gone of Gone->True; _->False)
    withdrawRoot publisher changing
    retired<-resolve inspectors target
    check "a withdrawn root is retired" (case retired of RootRetired->True; _->False)
    none<-rootSubject inspectors changing
    check "a withdrawn root has no subject" (fmap subjectRevision none==Nothing)
    other<-either (error . show) id <$> publishRoot publisher (RootDef "example.changing" (pure (Just (Revision 1,pkg))))
    check "root names are reused only with fresh identities" (rootIdent other/=rootIdent changing)
    stillRetired<-resolve inspectors target
    check "a reused name cannot revive an old subject" (case stillRetired of RootRetired->True; _->False)

  -- Closing refuses everything.
  (escaped,topAfter)<-withInspectors $ \inspectors->do
    let publisher=inspectPublisher inspectors
    r<-either (error . show) id <$> publishRoot publisher (RootDef "example.closing" (pure (Just (Revision 1,pkg))))
    Just s<-rootSubject inspectors r
    pure (inspectors,s)
  closedViews<-viewsFor escaped topAfter
  closedPublish<-publishViews (inspectPublisher escaped) (attach "example.late" [fromRight' (viewDef "example.late.view" "Late" 0 (\_->(Text []))) :: ViewDef Pkg])
  closedResolve<-resolve escaped topAfter
  check "a closed registry has no views" (null closedViews)
  check "a closed registry refuses publication" (closedPublish==Left InspectorsClosed)
  check "a closed registry retires its roots" (case closedResolve of RootRetired->True; _->False)

  -- Document preparation bounds, sanitizes and contains failures.
  let rows n=List "Rows" [Item (T.pack ("row "++show i)) "detail" Nothing | i<-[1..n::Int]]
  Right many<-prepareDoc (rows 500)
  check "long lists are bounded and report continuation" (case preparedDoc many of List _ items->length items==maxItems && preparedHasMore many; _->False)
  Right exact<-prepareDoc (rows maxItems)
  check "a list at the bound has no continuation" (not (preparedHasMore exact))
  Right lazyPage<-prepareDoc (List "Lazy" (replicate (maxItems+1) (Item "row" "" Nothing)++error "forced discarded tail"))
  check "preparation only needs the page and lookahead" (preparedHasMore lazyPage)
  Right cleaned<-prepareDoc (List "T\nitle" [Item "a\nb\tc\DEL" (T.replicate 40000 "x") (Just (OpenFile "a\NULb"))])
  check "captions are single-line and Details bounded"
    (case preparedDoc cleaned of
      List t [Item l d (Just (OpenFile p))]->t=="T itle" && l=="a b c " && T.length d==maxDetail && p=="ab"
      _->False)
  failed<-prepareDoc (Text [Prose (error "lazy view failure")])
  check "a lazily failing document is contained" (case failed of Left ViewFailed{}->True; _->False)
  putStrLn "typed inspector core checks passed"
