{-# LANGUAGE OverloadedStrings #-}
-- | Discover GHC installations and prepare exact debugger/compiler pairings.
--
-- GHCup discovery is offline and time bounded. Project compiler selection asks
-- Cabal instead of parsing its configuration. Debug preparation canonicalizes the
-- compiler, rechecks version/libdir and supplies matching environment variables.
-- A missing adapter is distinct from a failed compiler probe so acquisition can
-- be offered for the selected compiler.
module Hide.Compilers
  (Compiler(..), installedCompilers, compilerInfo, nativeVersions, recognizedCompiler, debuggerCompiler, debuggerCompilerInfo) where

import Control.Concurrent.Async (withAsync, wait)
import Control.Exception (IOException, bracket, finally, try, catch)
import Control.Monad (forM, void)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.List (nub, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Ord (Down(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (findExecutable, getHomeDirectory, doesFileExist, doesDirectoryExist, canonicalizePath)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), isAbsolute, takeDirectory, takeFileName, searchPathSeparator)
import System.Info (os)
import System.IO (hClose)
import System.Process
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Hide.Process (processCleanup)
import Hide.Downloads (managedToolRoot)

data Compiler = Compiler { compilerVersion :: T.Text, compilerPath :: FilePath } deriving (Eq,Show)

-- GHCup is the source of installed versions; reject cross-compilers and tags.
nativeVersions :: String -> [T.Text]
nativeVersions input=sortOn (Down . numbers) (nub [T.pack version | line<-lines input,
  "ghc":version:_<-[words line], Just _<-[numericVersion version]])
  where numbers value=fromMaybe [] (numericVersion (T.unpack value))

numericVersion :: String -> Maybe [Int]
numericVersion value=case T.splitOn "." (T.pack value) of
  parts | length parts>=3 && all (\part -> not (T.null part) && T.all isDigit part) parts -> mapM (readMaybe . T.unpack) parts
  _ -> Nothing

-- | Recognize compiler executable naming, not ABI compatibility.
recognizedCompiler :: FilePath -> Bool
recognizedCompiler path=name=="ghc" || maybe False (isJust . numericVersion . T.unpack) (T.stripPrefix "ghc-" name)
  where file=T.pack (takeFileName path)
        name=fromMaybe file (T.stripSuffix ".exe" file)

-- | Probe installed compilers with bounded subprocesses; call from a cancellable worker.
installedCompilers :: IO [Compiler]
installedCompilers = fromMaybe [] <$> timeout 15000000 discoverInstalled

discoverInstalled :: IO [Compiler]
discoverInstalled = do
  onPath<-findExecutable "ghcup"
  home<-getHomeDirectory
  base<-fromMaybe home <$> lookupEnv "GHCUP_INSTALL_BASE_PREFIX"
  let fallback=base </> ".ghcup" </> "bin" </> (if os=="mingw32" then "ghcup.exe" else "ghcup")
  exists<-doesFileExist fallback
  case onPath of
    Just executable -> discover executable
    Nothing | exists -> discover fallback
    _ -> pure []
  where
    discover executable=do
      listed<-query "." executable ["--offline","list","-t","ghc","-c","installed","-r"]
      case listed of
        Left _ -> pure []
        Right rows -> fmap catMaybes $ forM (take 64 (nativeVersions rows)) $ \version -> do
          found<-query "." executable ["--offline","whereis","ghc",T.unpack version]
          case found of
            Right output | let path=T.unpack (T.strip (T.pack output)), isAbsolute path, cleanPath path -> do
              exists<-doesFileExist path
              pure (if exists then Just (Compiler version path) else Nothing)
            _ -> pure Nothing

-- | Resolve compiler information using Cabal for a project or a standalone executable.
compilerInfo :: FilePath -> Bool -> FilePath -> IO (Either T.Text Compiler)
compilerInfo root project command
  | not (recognizedCompiler command) = pure (Left "hdb uses its own GHC build; a custom compiler requires an explicit Adapter config.")
  | project = do
      result<-queryWithin 30000000 root "cabal" (["path","--output-format=json","--compiler-info","-v0"]++
        ["--with-compiler="++command | command/="ghc"])
      case result >>= parseInformation of
        Left err -> pure (Left err)
        Right compiler -> validate compiler
  | otherwise = do
      path<-findExecutable (if isAbsolute command then command else if takeFileName command/=command then root </> command else command)
      case path of
        Nothing -> pure (Left "The selected GHC executable was not found.")
        Just executable -> do
          result<-query root executable ["--numeric-version"]
          pure $ result >>= \output -> let version=T.strip (T.pack output) in
            if isJust (numericVersion (T.unpack version)) then Right (Compiler version executable)
            else Left "The selected executable did not report a GHC version."
  where
    parseInformation output=case decodeStrict' (TE.encodeUtf8 (T.pack output)) >>= parseMaybe parser of
      Just compiler -> Right compiler
      Nothing -> Left "Cabal did not report a GHC compiler; use cabal-install 3.12 or newer."
    parser=withObject "compiler information" $ \o -> do
      compiler<-o .: "compiler"
      flavour<-compiler .: "flavour"
      if flavour/=("ghc"::T.Text) then fail "Not GHC" else do
        ident<-compiler .: "id"
        case T.stripPrefix "ghc-" ident of
          Just version | isJust (numericVersion (T.unpack version)) -> Compiler version <$> compiler .: "path"
          _ -> fail "Missing GHC version"
    validate compiler=do
      let path=compilerPath compiler
      exists<-if isAbsolute path && cleanPath path then doesFileExist path else pure False
      pure (if exists then Right compiler else Left "Cabal reported a missing or invalid compiler path.")

-- Resolve before spawning, in the DAP worker. The selected compiler's real bin
-- directory prevents a GHCup 'ghc' symlink from selecting a different version.
-- hdb wrappers validate their own GHC ABI; hdb --version reports its package only.
debuggerCompiler :: FilePath -> Bool -> FilePath -> IO (Either T.Text (FilePath,[(String,String)]))
debuggerCompiler root project command = do
  result<-debuggerCompilerInfo root project command
  pure $ result >>= \(compiler,adapter)->case adapter of
    Just found->Right found
    Nothing->Left ("No hdb-"<>compilerVersion compiler<>" or hdb executable found; install a matching debugger or use Adapter config.")

-- | Prepare verified compiler details and discover an optional hdb executable.
-- Its wrapper checks compiler ABI when launched.
-- A missing adapter is a successful result with Nothing, not a compiler failure.
debuggerCompilerInfo :: FilePath -> Bool -> FilePath -> IO (Either T.Text (Compiler,Maybe (FilePath,[(String,String)])))
debuggerCompilerInfo root project command = do
  resolved<-compilerInfo root project command
  case resolved of
    Left err -> pure (Left err)
    Right compiler -> do
      executable<-canonicalizePath (compilerPath compiler)
      actual<-query root executable ["--numeric-version"]
      libdir<-query root executable ["--print-libdir"]
      case (actual,libdir) of
        (Right version,Right library) | T.strip (T.pack version)==compilerVersion compiler -> do
          let directory=T.unpack (T.strip (T.pack library))
          exists<-if isAbsolute directory && cleanPath directory then doesDirectoryExist directory else pure False
          versioned<-findExecutable ("hdb-"++T.unpack (compilerVersion compiler))
          external<-maybe (findExecutable "hdb") (pure . Just) versioned
          managed<-managedToolRoot
          adapter<-case (external,managed) of
            (Nothing,Right cache)->findExecutable (cache </> "bin" </> ("hdb-"++T.unpack (compilerVersion compiler)))
            _->pure external
          path<-fromMaybe "" <$> lookupEnv "PATH"
          pure $ if not exists then Left "The selected GHC reported an invalid library directory." else
            Right (compiler {compilerPath=executable},fmap (\binary->(binary,[("GHC_BIN",executable),("GHC_LIBDIR",directory),
                ("PATH",takeDirectory executable++[searchPathSeparator]++path)])) adapter)
        (Left err,_) -> pure (Left err)
        (_,Left err) -> pure (Left err)
        _ -> pure (Left "The selected compiler changed version during debugger preparation; retry.")

cleanPath :: FilePath -> Bool
cleanPath value=not (null value) && all (`notElem` ['\0','\r','\n']) value

-- Own the process group so cancellation/timeouts also release inherited pipes.
-- Both captured streams are bounded; a noisy or stalled probe cannot retain UI.
query :: FilePath -> FilePath -> [String] -> IO (Either T.Text String)
query=queryWithin 5000000

queryWithin :: Int -> FilePath -> FilePath -> [String] -> IO (Either T.Text String)
queryWithin micros root command args=do
  result<-try $ bracket acquire release $ \(output,errors,process,stop) ->
    withAsync (capture (1024*1024) output) $ \out -> withAsync (capture 65536 errors) $ \err ->
      timeout micros ((,,) <$> waitForProcess process <*> wait out <*> wait err) `finally` stop
  pure $ case (result :: Either IOException (Maybe (ExitCode,BS.ByteString,BS.ByteString))) of
    Right (Just (ExitSuccess,output,_)) -> Right (T.unpack (TE.decodeUtf8With lenientDecode output))
    Right (Just (_,_,err)) -> Left (T.take 1000 (T.strip (TE.decodeUtf8With lenientDecode err)))
    Right Nothing -> Left "Compiler discovery timed out."
    Left err -> Left (T.take 1000 (T.pack (show err)))
  where
    acquire=do
      (_,Just output,Just errors,process)<-createProcess (proc command args)
        {cwd=Just root,std_in=NoStream,std_out=CreatePipe,std_err=CreatePipe,create_group=True}
      stop<-processCleanup process
      pure (output,errors,process,stop)
    release (output,errors,_,stop)=stop >> mapM_ (ignore . hClose) [output,errors]
    capture limit handle=do
      bytes<-BS.hGet handle (limit+1)
      if BS.length bytes>limit then ioError (userError "Compiler discovery output exceeds its limit") else pure bytes
    ignore operation=void operation `catch` (\(_::IOException) -> pure ())
