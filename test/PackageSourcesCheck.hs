{-# LANGUAGE OverloadedStrings #-}
module PackageSourcesCheck (checks) where
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import Distribution.PackageDescription (CondTree(..), CondBranch(..))
import Hide.PackageSources

checks :: IO ()
checks = do
  let source = BS.unlines
        [ "cabal-version: 3.0", "name: source-fixture", "version: 0.1"
        , "common shared", "  hs-source-dirs: src", "  other-modules: Shared"
        , "library", "  import: shared", "  exposed-modules: Library Virtual", "  virtual-modules: Virtual"
        , "executable demo", "  import: shared", "  main-is: Main.hs"
        , "  if os(windows)", "    hs-source-dirs: windows", "    other-modules: Platform"
        , "  else", "    hs-source-dirs: unix", "    other-modules: Platform"
        , "test-suite tests", "  type: detailed-0.9", "  test-module: Tests"
        , "benchmark bench", "  type: exitcode-stdio-1.0", "  main-is: Bench.hs"
        , "  autogen-modules: Paths_source_fixture", "  c-sources: cbits/helper.c"
        , "  include-dirs: include", "  includes: helper.h generated.h", "  autogen-includes: generated.h"
        ]
  let parsed=parsePackageSources source
  unless (fmap (map sourceTarget . sourceComponents) parsed == Right ["lib:source-fixture", "exe:demo", "test:tests", "bench:bench"])
    (fail "Cabal source index exposes targets before a plan exists")
  case parsed of
    Right package' | [library,exe,tests,bench]<-sourceComponents package' -> do
      unless (ModuleSource "Shared" False `elem` sourceEntries (condTreeData (sourceTree library)))
        (fail "Cabal common stanza sources are retained")
      unless (sourceRunKind (condTreeData (sourceTree tests))==DriverRun
           && DriverSource "Tests" `elem` sourceEntries (condTreeData (sourceTree tests))
           && sourceRunKind (condTreeData (sourceTree bench))==ExecutableRun)
        (fail "Test and benchmark interfaces preserve their different run routes")
      unless (ModuleSource "Paths_source_fixture" True `elem` sourceEntries (condTreeData (sourceTree bench))
           && PackageFileSource "cbits/helper.c" `elem` sourceEntries (condTreeData (sourceTree bench)))
        (fail "Generated and foreign source declarations are retained")
      unless (VirtualSource "Virtual" `elem` sourceEntries (condTreeData (sourceTree library))
           && ModuleSource "Virtual" False `notElem` sourceEntries (condTreeData (sourceTree library)))
        (fail "Virtual modules must not resolve as ordinary source files")
      unless (sourceIncludeDirectories (condTreeData (sourceTree bench))==["include"]
           && IncludeSource "helper.h" False `elem` sourceEntries (condTreeData (sourceTree bench))
           && IncludeSource "generated.h" True `elem` sourceEntries (condTreeData (sourceTree bench)))
        (fail "Include search paths and generated headers retain their distinct origin")
      let tree=sourceTree exe
      unless (MainSource "Main.hs" `elem` sourceEntries (condTreeData tree))
        (fail "Executable entry point is retained")
      case condTreeComponents tree of
        [CondBranch _ yes (Just no)] -> unless
          (sourceDirectories (condTreeData yes)==["windows"] && sourceDirectories (condTreeData no)==["unix"])
          (fail "Unknown condition alternatives remain separate")
        _ -> fail "Cabal condition tree is preserved"
    _ -> fail "Expected parsed package components"
  unless (either (const True) (const False) (parsePackageSources "not a Cabal file"))
    (fail "Invalid package descriptions are rejected")
  putStrLn "package source checks passed"
