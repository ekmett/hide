{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Git
  ( RepoStatus(..), repositoryStatus, repositoryDiff
  , GitReview(..), reviewRepository, commitReview
  ) where

import Control.Monad (forM, unless, when)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO.Error (ioeGetErrorString, tryIOError)
import System.Process (proc, readCreateProcessWithExitCode, cwd, env)

data RepoStatus = RepoStatus
  { repoRoot :: FilePath, repoBranch :: Text, repoDirty :: Bool }
  deriving (Eq, Show)

data GitReview = GitReview
  { reviewRoot :: FilePath, reviewText :: Text, reviewToken :: Text }
  deriving (Eq, Show)

repositoryStatus :: FilePath -> IO (Maybe RepoStatus)
repositoryStatus path = either (const Nothing) Just <$> result (do
  root <- repositoryRoot path
  (code, name, _) <- runGit root ["symbolic-ref", "--short", "HEAD"]
  branch <- if code == ExitSuccess then pure (T.strip name)
    else ("detached@" <>) . T.strip <$> git root ["rev-parse", "--short", "HEAD"]
  dirty <- not . T.null <$> git root ["status", "--porcelain", "-z", "--untracked-files=all"]
  pure (RepoStatus root branch dirty))

repositoryDiff :: FilePath -> Maybe FilePath -> IO (Either Text Text)
repositoryDiff path selected = result $ do
  dir <- pathDirectory path
  root <- repositoryRoot dir
  paths <- case selected of
    Nothing -> pure []
    Just file -> do
      absolute <- canonicalizePath (if isAbsolute file then file else dir </> file)
      let relative = makeRelative root absolute
      when (isAbsolute relative || ".." `elem` splitDirectories relative)
        (failGit "Selected file is outside this repository.")
      owner <- repositoryRoot absolute
      unless (owner == root) (failGit "Selected file belongs to another repository.")
      pure [relative]
  diffText root paths

reviewRepository :: FilePath -> IO (Either Text GitReview)
reviewRepository path = result $ do
  root <- repositoryRoot path
  before <- snapshot root
  rendered <- diffText root []
  after <- snapshot root
  unless (before == after) (failGit "Repository changed while preparing the review; refresh the diff.")
  pure (GitReview root rendered before)

commitReview :: GitReview -> Text -> IO (Either Text Text)
commitReview review message = result $ do
  when (T.null (T.strip message)) (failGit "Enter a nonempty commit message.")
  let root = reviewRoot review
  current <- snapshot root
  unless (current == reviewToken review)
    (failGit "Repository changed since this review; refresh the diff before approving.")
  _ <- git root ["add", "-A", "--", "."]
  staged <- snapshot root
  unless (fst (T.breakOn indexSeparator current) == fst (T.breakOn indexSeparator staged))
    (failGit "Repository changed while staging; review again. Changes remain on disk and in the index.")
  expectedTree <- T.strip <$> git root ["write-tree"]
  summary <- git root ["commit", "-m", T.unpack message]
  committedTree <- T.strip <$> git root ["rev-parse", "HEAD^{tree}"]
  pure (if committedTree == expectedTree then summary
        else "Committed; Git hooks changed the reviewed tree. Inspect HEAD.\n" <> summary)

indexSeparator :: Text
indexSeparator = "\NULINDEX\NUL"

-- Fingerprint file bytes independently of staging, but retain the original index
-- as well so another actor cannot alter a reviewed staged change unnoticed.
snapshot :: FilePath -> IO Text
snapshot root = do
  conflicts <- git root ["ls-files", "--unmerged", "-z"]
  unless (T.null conflicts) (failGit "Resolve merge conflicts before approving a commit.")
  (code, headId, _) <- runGit root ["rev-parse", "--verify", "HEAD"]
  files <- fileNames <$> git root ["ls-files", "--cached", "--others", "--exclude-standard", "-z"]
  hashes <- fmap catMaybes $ forM files $ \file -> do
    let path = root </> file
    symbolic <- pathIsSymbolicLink path `orMissing` False
    exists <- doesFileExist path
    value <- if symbolic then T.pack . ("link:" ++) <$> getSymbolicLinkTarget path
      else if exists then do
        digest <- git root ["hash-object", "--no-filters", "--", file]
        executableBit <- executable <$> getPermissions path
        pure (T.pack (show executableBit) <> ":" <> digest)
      else do
        directory <- doesDirectoryExist path
        when directory (failGit "Reviewing submodule commits is not supported; commit them with Git.")
        pure "missing"
    pure (if symbolic || exists then Just (file, value) else Nothing)
  index <- git root (diffArgs ++ ["--cached", "--binary", "--"])
  pure ((if code == ExitSuccess then headId else "unborn\n") <> T.pack (show hashes) <> indexSeparator <> index)

-- Keep staged and unstaged changes distinct, including on an unborn branch.
-- New files get a no-index diff so the approval screen shows their contents.
diffText :: FilePath -> [FilePath] -> IO Text
diffText root paths = do
  staged <- git root (diffArgs ++ ["--cached", "--"] ++ paths)
  working <- git root (diffArgs ++ ["--"] ++ paths)
  untracked <- fileNames <$> git root (["ls-files", "--others", "--exclude-standard", "-z", "--"] ++ paths)
  newFiles <- forM untracked $ \file -> do
    (code, out, err) <- runGit root (diffArgs ++ ["--no-index", "--", "/dev/null", file])
    unless (code == ExitSuccess || code == ExitFailure 1) (failGit err)
    symbolic <- pathIsSymbolicLink (root </> file)
    digest <- if symbolic then do
      target <- getSymbolicLinkTarget (root </> file)
      (hashCode, hashText, hashError) <- runGitInput root ["hash-object", "--stdin"] target
      if hashCode == ExitSuccess then pure hashText else failGit hashError
      else git root ["hash-object", "--no-filters", "--", file]
    pure (out <> "Content hash: " <> digest)
  let sections = [("Staged changes", staged), ("Unstaged changes", working), ("Untracked files", T.concat newFiles)]
      text = T.concat [title <> "\n\n" <> body <> "\n" | (title, body) <- sections, not (T.null body)]
  pure (if T.null text then "No changes.\n" else text)

diffArgs :: [String]
diffArgs = ["diff", "--no-ext-diff", "--no-textconv", "--color=never"]

fileNames :: Text -> [FilePath]
fileNames = Set.toAscList . Set.fromList . map T.unpack . filter (not . T.null) . T.splitOn "\NUL"

pathDirectory :: FilePath -> IO FilePath
pathDirectory path = do
  absolute <- canonicalizePath path
  directory <- doesDirectoryExist absolute
  pure (if directory then absolute else takeDirectory absolute)

repositoryRoot :: FilePath -> IO FilePath
repositoryRoot path = do
  dir <- pathDirectory path
  root <- git dir ["rev-parse", "--show-toplevel"]
  canonicalizePath (T.unpack (T.dropWhileEnd (== '\n') root))

-- Disable optional index writes and configured status/diff commands. Explicit
-- arguments and literal pathspecs keep filenames out of shell and Git syntax.
runGit :: FilePath -> [String] -> IO (ExitCode, Text, Text)
runGit dir args = runGitInput dir args ""

runGitInput :: FilePath -> [String] -> String -> IO (ExitCode, Text, Text)
runGitInput dir args input = do
  inherited <- getEnvironment
  let environment = ("GIT_OPTIONAL_LOCKS", "0") : filter (\(name, _) -> name `notElem`
        ["GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR", "GIT_OPTIONAL_LOCKS"]) inherited
  (code, out, err) <- readCreateProcessWithExitCode
    ((proc "git" (["--no-pager", "--literal-pathspecs", "-c", "core.fsmonitor=false"] ++ args))
      { cwd = Just dir, env = Just environment }) input
  pure (code, T.pack out, T.pack err)

git :: FilePath -> [String] -> IO Text
git root args = do
  (code, out, err) <- runGit root args
  if code == ExitSuccess then pure out else failGit err

failGit :: Text -> IO a
failGit = ioError . userError . T.unpack

result :: IO a -> IO (Either Text a)
result action = either (Left . T.pack . ioeGetErrorString) Right <$> tryIOError action

orMissing :: IO a -> a -> IO a
orMissing action fallback = either (const fallback) id <$> tryIOError action
