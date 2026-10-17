-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.PackageInspect
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Inspector views for the Cabal package declarations in "Hide.PackageSources".
--
-- This is the first consumer of "Hide.Plugin.Inspect": the types stay as they
-- are, and views, edges and navigation are attached from outside. Everything
-- here is pure. A component's conditional branches are counted but not
-- flattened, matching the parser's contract: flattening would mix mutually
-- exclusive declarations and pretend that unknown flags were chosen.
module Hide.PackageInspect
  ( componentsEdge, sourcesEdge, packageContributions, publishPackageViews
  , componentSubject, kindLabel
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Distribution.PackageDescription (CondBranch(..),CondTree(..))
import Hide.PackageSources
import Hide.Plugin.Inspect

-- | Components of a package, keyed by Cabal's qualified selector (@lib:name@).
-- Observation is a traversal, so any optic consumer can rebuild the package.
componentsEdge :: Edge PackageSources ComponentSources
componentsEdge=required (edge "hide.package.edge.components" "Components" observed (Key . sourceTarget))
  where observed f package=(\components->package {sourceComponents=components}) <$> traverse f (sourceComponents package)

-- | The unconditional declarations of a component, keyed by the declaration.
-- 'Source' is 'Ord', and one component never declares the same entry twice.
sourcesEdge :: Edge ComponentSources Source
sourcesEdge=required (edge "hide.component.edge.sources" "Sources" observed Key)
  where
    observed f component=rebuild component <$> traverse f (sourceEntries (rootGroup component))
    rebuild component entries=component {sourceTree=(sourceTree component) {condTreeData=(rootGroup component) {sourceEntries=entries}}}

rootGroup :: ComponentSources -> SourceGroup
rootGroup=condTreeData . sourceTree

required :: Either Text a -> a
required=either (error . T.unpack) id

-- | The subject for one component of a package subject, by its Cabal selector.
-- 'Nothing' when the subject is not a package or has no such component.
componentSubject :: Subject -> Text -> Maybe Subject
componentSubject subject target=do
  here<-subjectAs subject
  case [Subject child | child<-follow componentsEdge here,sourceTarget (hereValue child)==target] of
    found:_->Just found
    []->Nothing

kindLabel :: ComponentKind -> Text
kindLabel kind=case kind of
  LibraryComponent->"library"
  ForeignLibraryComponent->"foreign library"
  ExecutableComponent->"executable"
  TestComponent->"test suite"
  BenchmarkComponent->"benchmark"

runLabel :: RunKind -> Text
runLabel run=case run of
  NoRun->"not runnable"
  ExecutableRun->"executable interface"
  DriverRun->"test driver"
  UnknownRun->"unknown interface"

yesNo :: Bool -> Text
yesNo True="yes"
yesNo False="no"

branches :: ComponentSources -> Int
branches component=count (sourceTree component)
  where count tree=sum [1+count yes+maybe 0 count no | CondBranch _ yes no<-condTreeComponents tree]

-- | Caption and description of one source declaration.
describe :: Source -> (Text,Text,[(Text,Text)])
describe source=case source of
  ModuleSource name generated->("Module",name,[("Generated",yesNo generated)])
  SignatureSource name->("Signature",name,[])
  VirtualSource name->("Virtual module",name,[("File","none; virtual modules have no source file")])
  IncludeSource path generated->("Include",T.pack path,[("Generated",yesNo generated)])
  MainSource path->("Main",T.pack path,[("Relative to","the component's source directories")])
  DriverSource name->("Test driver",name,[("Interface","detailed-0.9 module")])
  PackageFileSource path->("Package file",T.pack path,[("Relative to","the package directory")])

caption :: Source -> Text
caption source=let (kind,name,_)=describe source in name<>" ("<>T.toLower kind<>")"

-- Package views.
packageViewList :: Either Text [ViewDef PackageSources]
packageViewList=sequence
  [ viewDef "hide.package.components" "Components" 0 (\here->
      let package=hereValue here
          children=follow componentsEdge here
          item child=let component=hereValue child in Item (sourceTarget component) (componentSummary component) (Just (Inspect (Subject child)))
      in (List (sourcePackageName package<>" — components") (map item children)))
  , (\view->view {viewApplies=not . null . sourceWarnings}) <$> viewDef "hide.package.warnings" "Warnings" 10 (\here->
      (Text (Heading "Cabal parser warnings":map Prose (sourceWarnings (hereValue here)))))
  ]

componentSummary :: ComponentSources -> Text
componentSummary component=T.intercalate "\n"
  [ kindLabel (sourceKind component)<>", "<>(if sourceBuildable group then "buildable" else "not buildable")
  , runLabel (sourceRunKind group)
  , T.pack (show (length (sourceEntries group)))<>" unconditional declarations"
  , T.pack (show (branches component))<>" conditional branches not shown here" ]
  where group=rootGroup component

componentViewList :: Either Text [ViewDef ComponentSources]
componentViewList=sequence
  [ viewDef "hide.component.sources" "Sources" 0 (\here->
      let component=hereValue here
          children=follow sourcesEdge here
          item child=let source=hereValue child
                         (kind,name,extra)=describe source
                     in Item (caption source) (T.intercalate "\n" ((kind<>" "<>name):[k<>": "<>v | (k,v)<-extra])) (Just (Inspect (Subject child)))
      in (List (sourceTarget component<>" — unconditional sources"
                      <>(if branches component>0 then " ("<>T.pack (show (branches component))<>" conditional branches omitted)" else "")) (map item children)))
  , viewDef "hide.component.declaration" "Declaration" 10 (\here->
      let component=hereValue here
          group=rootGroup component
      in (Text [Heading (sourceTarget component)
        ,Facts [("Kind",kindLabel (sourceKind component)),("Buildable",yesNo (sourceBuildable group))
               ,("Interface",runLabel (sourceRunKind group))
               ,("Source directories",listing (map T.pack (sourceDirectories group)))
               ,("Include directories",listing (map T.pack (sourceIncludeDirectories group)))
               ,("Unconditional declarations",T.pack (show (length (sourceEntries group))))
               ,("Conditional branches",T.pack (show (branches component)))]]))
  ]
  where listing []="inherited or default"
        listing paths=T.intercalate ", " paths

sourceViewList :: Either Text [ViewDef Source]
sourceViewList=sequence
  [ viewDef "hide.source.declaration" "Declaration" 0 (\here->
      let (kind,name,extra)=describe (hereValue here)
      in (Text [Heading name,Facts (("Kind",kind):extra)]))
  ]

-- | The three contributions, each for one exact type.
packageContributions :: Either Text [Contribution]
packageContributions=do
  package<-packageViewList
  component<-componentViewList
  source<-sourceViewList
  pure [attach "hide.package.views" package,attach "hide.component.views" component,attach "hide.source.views" source]

-- | Publish the views through the session capability. A failure is a
-- programming error in the fixed declarations and is reported as an IOError.
publishPackageViews :: InspectPublisher -> IO [ContributionRef]
publishPackageViews publisher=case packageContributions of
  Left err->ioError (userError (T.unpack err))
  Right contributions->mapM (\contribution->publishViews publisher contribution >>= either (ioError . userError . show) pure) contributions
