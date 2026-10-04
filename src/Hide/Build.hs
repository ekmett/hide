{-# LANGUAGE OverloadedStrings #-}
-- | Resolve build context and plan THC/GHC commands without executing them.
--
-- Source selection ignores labeled output windows. Saved targets are scoped to
-- their working directory so changing projects does not reuse an unrelated target.
-- Project GHC work goes through Cabal; loose-file work uses GHC/runghc. Process
-- ownership and output collection belong to "Hide.BuildJobs" or consoles.
module Hide.Build
  (Toolchain(..), BuildAction(..), BuildConfig(..), loadBuildConfig, isProject, resolveBuildRoot, buildSource, buildPlan, testPlan, buildConfigValue, parseBuildConfig) where

import Hide.Sidebar
import Control.Exception (IOException, try)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import System.Directory (canonicalizePath, doesFileExist, findExecutable, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), isAbsolute, takeFileName, takeExtension, takeDirectory)
import Hide.Model (Desktop(..), Window(..), Document(..), Toolchain(..), startingDirectory)
import Hide.Files (filePath)

data BuildAction = Compile | Make | Run deriving (Eq,Show)
data BuildConfig = BuildConfig
  { buildToolchain :: Toolchain, buildExecutable :: FilePath, buildTarget :: Text
  , buildTHCRoot :: Text, buildRuntime :: Text, buildArguments :: [String]
  } deriving (Eq,Show)

-- Output and debugger windows do not replace the most recently used source.
buildSource :: Desktop -> Maybe FilePath
buildSource desktop = listToMaybe [filePath file | window<-windows desktop,
  Just doc<-[M.lookup (bufferId window) (buffers desktop)], documentLabel doc==Nothing,
  Just file<-[documentFile doc]]

-- | Resolve the selected sidebar/default/source context to an enclosing build root.
resolveBuildRoot :: Desktop -> IO FilePath
resolveBuildRoot desktop = do
  let fallback=fromMaybe (maybe (startingDirectory desktop) takeDirectory (buildSource desktop)) (defaultDirectory desktop)
  start<-canonicalizePath (maybe fallback treeRoot (sideTree desktop))
  search start start
  where
    search fallback dir = do
      found<-isProject dir
      if found then pure dir else if takeDirectory dir==dir then pure fallback else search fallback (takeDirectory dir)

isProject :: FilePath -> IO Bool
isProject root = do
  entries<-either (const []) id <$> (try (listDirectory root) :: IO (Either IOException [FilePath]))
  pure ("cabal.project" `elem` entries || any ((==".cabal").takeExtension) entries)

-- | Load toolchain settings, reusing the saved target only for its matching cwd.
loadBuildConfig :: FilePath -> FilePath -> IO BuildConfig
loadBuildConfig directory root = do
  loaded<-try (BS.readFile (directory </> "run.json")) :: IO (Either IOException BS.ByteString)
  thcRoot<-T.pack . fromMaybe "" <$> lookupEnv "THC_ROOT"
  let fallback=BuildConfig THC "thc" "" thcRoot "" []
      parse=withObject "build target" $ \o -> do
        name<-o .:? "toolchain" .!= ("THC"::Text)
        let tool=if name=="GHC" then GHC else THC
        command<-o .:? "command" .!= (if tool==THC then "thc" else "ghc")
        savedRoot<-o .:? "cwd"
        target<-o .:? "target" .!= ""
        BuildConfig tool command (if savedRoot==Just root then target else "")
          <$> o .:? "thcRoot" .!= thcRoot <*> o .:? "runtime" .!= "" <*> o .:? "arguments" .!= []
  pure (fromMaybe fallback (either (const Nothing) (\bytes -> decodeStrict' bytes >>= parseMaybe parse) loaded))

buildConfigValue :: FilePath -> BuildConfig -> Value
buildConfigValue root config=object
  ["cwd" .= root,"toolchain" .= show (buildToolchain config),"command" .= buildExecutable config
  ,"target" .= buildTarget config,"thcRoot" .= buildTHCRoot config,"runtime" .= buildRuntime config,"arguments" .= buildArguments config]

parseBuildConfig :: [Text] -> Either Text BuildConfig
parseBuildConfig (command:target:thcRoot:runtime:rest) = do
  let tool=case rest of _:"1":_ -> GHC; _ -> THC
      executable=case (tool,T.strip command) of (GHC,"thc") -> "ghc"; (THC,"ghc") -> "thc"; (_,value) -> T.unpack value
  args<-case rest of
    raw:_ -> either (Left . T.pack) Right (eitherDecodeStrict' (Data.Text.Encoding.encodeUtf8 raw))
    _ -> Right []
  if null executable || any (elem '\0') (executable:T.unpack target:T.unpack thcRoot:T.unpack runtime:args)
    then Left "Enter an executable and arguments without NUL characters."
    else if "-" `T.isPrefixOf` target then Left "A target cannot start with '-'."
    else Right (BuildConfig tool executable target thcRoot runtime args)
parseBuildConfig _ = Left "Incomplete build target settings."

-- | Construct executable/argument steps for compile, make or run; do not execute them.
buildPlan :: BuildAction -> BuildConfig -> FilePath -> Maybe FilePath -> IO (Either Text [(FilePath,[String])])
buildPlan _ config _ _
  | null (buildExecutable config) || any (elem '\0') (buildExecutable config:T.unpack (buildTarget config):T.unpack (buildTHCRoot config):T.unpack (buildRuntime config):buildArguments config) =
      pure (Left "Invalid executable or NUL character in build settings.")
  | "-" `T.isPrefixOf` buildTarget config = pure (Left "A target cannot start with '-'.")
buildPlan action config root source = do
  project<-isProject root
  let target=buildTarget config
      chosen=if T.null target then [] else [T.unpack target]
      exe=buildExecutable config
      optional flag value=[part | not (T.null value),part<-[flag,T.unpack value]]
      args=buildArguments config
  case buildToolchain config of
    THC -> pure (Right [(exe,[if action==Run then "run" else "build"]++chosen++
      ["--project-dir",root]++optional "--thc-root" (buildTHCRoot config)++
      (if action==Run then optional "--runtime" (buildRuntime config)++["--" | not (null args)]++args else []))])
    GHC | project -> pure (Right [("cabal",[if action==Run then "run" else "build"]++["--with-compiler="++exe | exe/="ghc"]++chosen++
      (if action==Run then ["--" | not (null args)]++args else []))])
    GHC -> case source of
      Just file | takeExtension file `elem` [".hs",".lhs"] -> do
        exists<-doesFileExist file
        if not exists then pure (Left "Save the source file before building.") else case action of
          Compile -> pure (Right [(exe,["--make","-fno-code","-fdiagnostics-color=never",file])])
          Make -> pure (Right [(exe,["--make","-fdiagnostics-color=never",file])])
          Run -> do
            -- runghc's -f executes a path directly rather than searching PATH.
            compiler<-findExecutable (if isAbsolute exe then exe else if takeFileName exe/=exe then root </> exe else exe)
            case compiler of
              Nothing -> pure (Left ("The selected GHC executable was not found or is not executable: "<>T.pack exe))
              Just path -> do
                resolved<-canonicalizePath path
                pure (Right [("runghc",["-f",resolved,file]++args)])
      _ -> pure (Left "Choose a saved Haskell source file or a Cabal project.")

-- | Plan a GHC/Cabal test run. THC has no configured test-runner plan.
testPlan :: BuildConfig -> FilePath -> IO (Either Text [(FilePath,[String])])
testPlan config root
  | buildToolchain config/=GHC = pure (Left "THC has no configured test runner. Select GHC to run Cabal tests.")
  | null (buildExecutable config) || any (elem '\0') [buildExecutable config,T.unpack (buildTarget config)] = pure (Left "Invalid test compiler or target.")
  | "-" `T.isPrefixOf` buildTarget config = pure (Left "A test target cannot start with '-'.")
  | otherwise = do
      project<-isProject root
      pure $ if not project then Left "Tests require a Cabal project." else
        Right [("cabal",["test"]++["--with-compiler="++buildExecutable config | buildExecutable config/="ghc"]++["--test-show-details=direct"]++[T.unpack (buildTarget config) | not (T.null (buildTarget config))])]
