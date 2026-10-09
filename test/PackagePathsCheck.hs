{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : PackagePathsCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module PackagePathsCheck (checks) where

import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import Distribution.PackageDescription (Condition(..))
import Hide.PackagePaths
import Hide.PackageSources
import System.Directory
import System.FilePath
import System.IO (openTempFile,hClose)

checks :: IO ()
checks=withSystemTempDirectory "hide-package-paths" $ \root->do
  let package=root </> "pkg"
  createDirectory package
  createDirectory (root </> "shared")
  writeFile (root </> "shared/Shared.hs") "module Shared where\n"
  mapM_ (createDirectory . (package </>)) ["src","windows","unix","cbits"]
  writeFile (package </> "src/Main.hs") "main = pure ()\n"
  writeFile (package </> "windows/Platform.hs") "module Platform where\n"
  writeFile (package </> "unix/Platform.hsc") "module Platform where\n"
  writeFile (package </> "cbits/helper.c") "void f(void) {}\n"
  let manifest=BS.unlines
        ["cabal-version: 3.0","name: paths","version: 0.1"
        ,"executable demo","  hs-source-dirs: src ../shared","  main-is: Main.hs"
        ,"  other-modules: Platform Missing Shared","  autogen-modules: Paths_paths"
        ,"  c-sources: cbits/helper.c"
        ,"  if os(windows)","    hs-source-dirs: windows"
        ,"  else","    hs-source-dirs: unix"]
  component<-case parsePackageSources manifest of
    Right parsed | [value]<-sourceComponents parsed->pure value
    _->fail "Expected one Cabal executable"
  resolved<-resolveSources root [] package component
  values<-either (fail . show) pure resolved
  let files source=[path | candidate<-values,candidateSource candidate==source,path<-candidatePaths candidate]
      present source=[path | row<-files source,Just path<-[existingPath row]]
  canonical<-canonicalizePath package
  unless (present (MainSource "Main.hs")==[canonical </> "src/Main.hs"])
    (fail "main-is uses package source directories, not the workspace root")
  unless (present (ModuleSource "Shared" False)==[root </> "shared/Shared.hs"])
    (fail "Shared sources outside a package but inside the workspace remain available")
  unless (present (PackageFileSource "cbits/helper.c")==[canonical </> "cbits/helper.c"])
    (fail "Foreign sources use the package root")
  unless (all ((/=Lit True).pathCondition) [row | row<-files (ModuleSource "Platform" False),existingPath row/=Nothing]
       && length (present (ModuleSource "Platform" False))==2)
    (fail "Conditional source directories remain unknown candidates, including preprocessor sources")
  unless (any ((==ModuleSource "Missing" False).candidateSource) values
       && any ((==ModuleSource "Paths_paths" True).candidateSource) values)
    (fail "Missing and generated sources remain in the package tree")
  private<-resolveSources root [canonical </> "src"] package component
  unless (case private of Right rows->all (all ((/=Just (canonical </> "src/Main.hs")).existingPath).candidatePaths) rows; _->False)
    (fail "Private sources are never openable from the public package projection")
  withSystemTempDirectory "hide-package-outside" $ \outside->do
    writeFile (outside </> "Main.hs") "private\n"
    bracket (renameDirectory (package </> "src") (package </> "original") >> createDirectoryLink outside (package </> "src"))
      (\_ -> removeDirectoryLink (package </> "src") >> renameDirectory (package </> "original") (package </> "src")) $ \_ ->do
        escaped<-resolveSources root [] package component
        unless (case escaped of Right rows->all (all ((==Nothing).existingPath).candidatePaths) [row | row<-rows,candidateSource row==MainSource "Main.hs"]; _->False)
          (fail "Source directory symlinks cannot escape the workspace")
  putStrLn "package path checks passed"

withSystemTempDirectory :: String -> (FilePath -> IO a) -> IO a
withSystemTempDirectory name=bracket acquire removePathForcibly
  where
    acquire=do
      base<-getTemporaryDirectory
      (path,handle)<-openTempFile base name
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path
