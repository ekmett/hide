{-# LANGUAGE OverloadedStrings #-}
-- | Source declarations from Cabal's package parser, without configuring a build.
--
-- Each conditional node contains declarations added by that branch. Keep the
-- condition tree: flattening it would mix mutually exclusive source directories
-- and pretend that unknown flags have been selected. Paths are declarations,
-- not checked workspace access; filesystem resolution belongs to the worker.
module Hide.PackageSources
  ( PackageSources(..), ComponentSources(..), ComponentKind(..), SourceGroup(..)
  , Source(..), RunKind(..), parsePackageSources
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.List (nub)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Fields.ParseResult (runParseResult)
import Distribution.Parsec.Error (showPError)
import Distribution.Parsec.Warning (showPWarning)
import Distribution.Pretty (prettyShow)
import Distribution.Utils.Path (getSymbolicPath)

-- | Package identity and public parser diagnostics, prepared on a worker.
data PackageSources = PackageSources
  { sourcePackageName :: !Text, sourceComponents :: [ComponentSources]
  , sourceWarnings :: [Text] }
data ComponentKind = LibraryComponent | ForeignLibraryComponent | ExecutableComponent
  | TestComponent | BenchmarkComponent deriving (Eq,Show)
-- | The target is Cabal's qualified component selector, never a display label.
data ComponentSources = ComponentSources
  { sourceTarget :: !Text, sourceKind :: !ComponentKind
  , sourceTree :: CondTree ConfVar [Dependency] SourceGroup }
-- | Local declarations at one conditional node, including imported common stanzas.
-- An empty source-directory list inherits surrounding declarations; only a fully
-- resolved component with no directories uses Cabal's default current directory.
data SourceGroup = SourceGroup
  { sourceDirectories :: [FilePath], sourceEntries :: [Source]
  , sourceBuildable :: !Bool, sourceRunKind :: !RunKind }
data Source = ModuleSource !Text !Bool | SignatureSource !Text | FileSource !FilePath
  deriving (Eq,Show)
-- | Unknown means this branch has no interface declaration. Executable interfaces
-- may support direct Run/Debug; library-style tests require their test driver.
data RunKind = NoRun | ExecutableRun | DriverRun | UnknownRun deriving (Eq,Show)

-- | Parse at most 1 MiB of package description using Cabal-syntax. No filesystem,
-- process, plan.json or flag evaluation occurs. Malformed input remains an error.
parsePackageSources :: ByteString -> Either Text PackageSources
parsePackageSources bytes
  | BS.length bytes>1048576 = Left "Package description exceeds 1 MiB."
  | otherwise = case runParseResult (parseGenericPackageDescription bytes) of
      (_,Left (_,errors)) -> Left (T.intercalate "\n" (map (T.pack . showPError "package.cabal") (toList errors)))
      (warnings,Right package') -> Right (describe package')
        {sourceWarnings=map (T.pack . showPWarning "package.cabal") warnings}
  where
    describe package'=PackageSources name components []
      where
        name=T.pack (prettyShow (pkgName (package (packageDescription package'))))
        components=maybe [] (\tree->[component LibraryComponent ("lib:"<>name) librarySources tree]) (condLibrary package')
          ++ named LibraryComponent "lib:" librarySources (condSubLibraries package')
          ++ named ForeignLibraryComponent "flib:" (group NoRun [] . foreignLibBuildInfo) (condForeignLibs package')
          ++ named ExecutableComponent "exe:" executableSources (condExecutables package')
          ++ named TestComponent "test:" testSources (condTestSuites package')
          ++ named BenchmarkComponent "bench:" benchmarkSources (condBenchmarks package')
    named kind prefix project=map (\(name,tree)->component kind (prefix<>T.pack (prettyShow name)) project tree)
    component kind target project tree=ComponentSources target kind (fmap project tree)
    librarySources value=group NoRun
      (map (\name->moduleSource (libBuildInfo value) name) (exposedModules value)
       ++map (SignatureSource . T.pack . prettyShow) (signatures value)) (libBuildInfo value)
    executableSources value=group ExecutableRun (file (getSymbolicPath (modulePath value))) (buildInfo value)
    testSources value=let (run,entries)=case testInterface value of
                           TestSuiteExeV10 _ path->(ExecutableRun,file (getSymbolicPath path))
                           TestSuiteLibV09 _ name->(DriverRun,[moduleSource (testBuildInfo value) name])
                           TestSuiteUnsupported _->(UnknownRun,[])
                      in group run entries (testBuildInfo value)
    benchmarkSources value=let (run,entries)=case benchmarkInterface value of
                                BenchmarkExeV10 _ path->(ExecutableRun,file (getSymbolicPath path))
                                BenchmarkUnsupported _->(UnknownRun,[])
                           in group run entries (benchmarkBuildInfo value)
    file path=[FileSource path | not (null path)]
    moduleSource info name=ModuleSource (T.pack (prettyShow name)) (name `elem` autogenModules info)
    group run entries info=SourceGroup (map getSymbolicPath (hsSourceDirs info))
      (nub (entries++map (moduleSource info) (otherModules info++autogenModules info)
        ++map (FileSource . getSymbolicPath) (cSources info++cxxSources info++asmSources info++cmmSources info++jsSources info)))
      (buildable info) run
