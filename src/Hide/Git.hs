{-# LANGUAGE OverloadedStrings #-}
-- | Repository inspection, exact review snapshots and checked commits.
--
-- Git receives literal paths with routing environment overrides removed; external
-- diff/textconv helpers are disabled. Privacy filtering classifies rename/copy
-- lineage before selection. Reviews retain HEAD, index and working-file identity
-- so a commit can reject changed evidence. Normal hooks still run and their tree
-- changes are reported separately from the reviewed staged tree.
module Hide.Git
  ( RepoStatus(..), repositoryStatus, repositoryDiff, repositoryDiffFiltered, repositoryDiffFilteredAt
  , GitReview(..), reviewRepository, commitReview
  , GitCommit(..), reviewRepositoryChecked, commitReviewChecked
  , checkIntegrationState, checkIncomingChanges
  ) where

import Control.Monad (forM, forM_, unless, when)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO.Error (ioeGetErrorString, tryIOError)
import System.Process (proc, readCreateProcessWithExitCode, cwd, env)
import Text.Read (readMaybe)

data RepoStatus = RepoStatus
  { repoRoot :: FilePath, repoBranch :: Text, repoDirty :: Bool, repoAdded :: Int, repoDeleted :: Int }
  deriving (Eq, Show)

data GitReview = GitReview
  { reviewRoot :: FilePath, reviewText :: Text, reviewToken :: Text }
  deriving (Eq, Show)

-- Structured facts are separate from human-facing Git/hook output.
data GitCommit = GitCommit
  { commitExitCode :: ExitCode, commitTreeMatched :: Maybe Bool, commitOutput :: Text }
  deriving (Eq, Show)

repositoryStatus :: FilePath -> IO (Maybe RepoStatus)
repositoryStatus path = either (const Nothing) Just <$> result (do
  root <- repositoryRoot path
  (code, name, _) <- runGit root ["symbolic-ref", "--short", "HEAD"]
  branch <- if code == ExitSuccess then pure (T.strip name)
    else ("detached@" <>) . T.strip <$> git root ["rev-parse", "--short", "HEAD"]
  dirty <- not . T.null <$> git root ["status", "--porcelain", "-z", "--untracked-files=all"]
  -- A failed count must not hide an otherwise valid branch and dirty indicator.
  (added,deleted) <- if dirty then either (const (-1,-1)) id <$> result (lineCounts root) else pure (0,0)
  pure (RepoStatus root branch dirty added deleted))

lineCounts :: FilePath -> IO (Int,Int)
lineCounts root = do
  (headCode,headId,_)<-runGit root ["rev-parse","--verify","HEAD"]
  base<-if headCode==ExitSuccess then pure (T.strip headId) else do
    (code,emptyTree,err)<-runGitInput root ["hash-object","-t","tree","--stdin"] ""
    if code==ExitSuccess then pure (T.strip emptyTree) else failGit err
  tracked<-git root (diffArgs++["--numstat",T.unpack base,"--"])
  untracked<-fileNames <$> git root ["ls-files","--others","--exclude-standard","-z"]
  additions<-forM untracked $ \file -> do
    (code,out,err)<-runGit root (diffArgs++["--numstat","--no-index","--","/dev/null",file])
    unless (code==ExitSuccess || code==ExitFailure 1) (failGit err)
    pure out
  pure (foldl' count (0,0) (concatMap T.lines (tracked:additions)))
  where
    -- Git quotes newlines/tabs in names without -z; only the first two fields matter.
    count (added,deleted) line = case T.splitOn "\t" line of
      a:z:_ | Just plus<-readMaybe (T.unpack a),Just minus<-readMaybe (T.unpack z) -> (added+plus,deleted+minus)
      _ -> (added,deleted) -- Binary entries use '-' for both counts.

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

-- The predicate receives both lexical and canonical absolute paths. Include
-- staged deletion names: they no longer appear in ls-files, but their old bytes
-- still belong to the diff. Never interpret an empty filtered list as all files.
repositoryDiffFiltered :: FilePath -> (FilePath -> Bool) -> IO (Either Text (Text,Int))
repositoryDiffFiltered path=repositoryDiffFilteredAt path Nothing

-- | Filter whole-repository rename/copy lineage before selecting paths for a diff.
repositoryDiffFilteredAt :: FilePath -> Maybe FilePath -> (FilePath -> Bool) -> IO (Either Text (Text,Int))
repositoryDiffFilteredAt path selected excluded=do
  prepared<-result $ do
    directory<-pathDirectory path
    root<-repositoryRoot directory
    selection<-traverse (\file->do
      let absolute=normalise (if isAbsolute file then file else directory </> file)
      canonical<-canonicalizePath absolute
      unless (within root canonical && not (excluded absolute || excluded canonical)) (failGit "Selected diff path is private or outside this repository.")
      pure canonical) selected
    (checked,before,_)<-classifyChanges root excluded
    let selectedRows=[row | row@(file,_,_)<-checked,maybe True (\base->within base (normalise (root </> file))) selection]
        allowed=[file | (file,_,False)<-selectedRows]
    text<-if null allowed then pure "No changes.\n" else diffTextWith ["--no-renames","--submodule=short"] root allowed
    after<-renameChanges root
    unless (before==after) (failGit "Repository changed while preparing the filtered diff; retry.")
    forM_ [row | row@(_,_,False)<-selectedRows] $ \(file,canonical,_)->do
      current<-canonicalizePath (root </> file)
      unless (current==canonical && not (excluded current)) (failGit "A filtered diff path changed; retry.")
    pure (text,length [() | (_,_,True)<-selectedRows])
  -- Git failures may mention filenames; a private path must not escape in errors.
  pure (either (const (Left "Could not prepare a stable filtered Git diff; retry or select a safe file.")) Right prepared)

within :: FilePath -> FilePath -> Bool
within root file=let relative=makeRelative root file in not (isAbsolute relative) && ".." `notElem` splitDirectories relative

-- Include unchanged sources when classifying rename/copy lineage, but keep the
-- changed set separate: an unchanged private configuration does not veto review.
classifyChanges :: FilePath -> (FilePath -> Bool) -> IO ([(FilePath,FilePath,Bool)],(Text,Text),Set.Set FilePath)
classifyChanges root excluded=do
  listed<-fileNames <$> git root ["ls-files","--cached","--others","--exclude-standard","-z"]
  untracked<-fileNames <$> git root ["ls-files","--others","--exclude-standard","-z"]
  before<-renameChanges root
  (changed,renames)<-either failGit pure (parseChanges (fst before<>snd before))
  let files=Set.toAscList (Set.fromList (listed++changed))
  when (length files>10000) (failGit "Git review exceeds 10000 files.")
  checked<-forM files $ \file->do
    let absolute=normalise (root </> file)
    canonical<-canonicalizePath absolute
    pure (file,canonical,excluded absolute || excluded canonical || not (within root canonical))
  let links=Map.fromListWith (++) [(source,[destination]) | (source,destination)<-renames]
      initial=Set.fromList [file | (file,_,True)<-checked]
      omitted=closeRenames links initial (Set.toList initial)
  pure ([(file,canonical,Set.member file omitted) | (file,canonical,_)<-checked],before,Set.fromList (changed++untracked))
  where
    closeRenames _ omitted []=omitted
    closeRenames links omitted (source:rest)=
      let fresh=filter (`Set.notMember` omitted) (Map.findWithDefault [] source links)
      in closeRenames links (foldr Set.insert omitted fresh) (fresh++rest)

-- Bound exhaustive similarity matching, including copies from unchanged sources.
renameChanges :: FilePath -> IO (Text,Text)
renameChanges root=(,) <$> git root (diffArgs++["--cached","--name-status","-z","-M","-C","--find-copies-harder","-l256","--"])
                      <*> git root (diffArgs++["--name-status","-z","-M","-C","--find-copies-harder","-l256","--"])

parseChanges :: Text -> Either Text ([FilePath],[(FilePath,FilePath)])
parseChanges input=go (filter (not . T.null) (T.splitOn "\NUL" input))
  where
    go []=Right ([],[])
    go (status:rest) | T.take 1 status `elem` ["R","C"]=case rest of
      source:destination:remaining->do
        (files,renames)<-go remaining
        pure (T.unpack source:T.unpack destination:files,(T.unpack source,T.unpack destination):renames)
      _->Left "Invalid Git rename listing"
    go (_:file:remaining)=do
      (files,renames)<-go remaining
      pure (T.unpack file:files,renames)
    go _=Left "Invalid Git change listing"

checkChanges :: (FilePath -> Bool) -> FilePath -> IO ()
checkChanges excluded root=do
  (files,_,changed)<-classifyChanges root excluded
  when (any (\(file,_,private)->private && Set.member file changed) files)
    (failGit "Protected changes cannot be approved through an agent review.")

-- Only saved, ordinary clean working trees can be changed through an agent.
-- Git itself still owns checkout locking and protection against concurrent edits.
checkIntegrationState :: FilePath -> Text -> IO (Either Text ())
checkIntegrationState root expected=result $ do
  current<-T.strip <$> git root ["rev-parse","--verify","HEAD"]
  unless (current==expected) (failGit "Repository HEAD changed.")
  status<-git root ["status","--porcelain","-z","--untracked-files=all"]
  unless (T.null status) (failGit "Repository has saved changes or conflicts.")
  flags<-git root ["ls-files","-v","-z"]
  unless (all (T.isPrefixOf "H ") (filter (not . T.null) (T.splitOn "\NUL" flags)))
    (failGit "Repository contains hidden or sparse index entries.")
  forM_ ["MERGE_HEAD","CHERRY_PICK_HEAD","REVERT_HEAD","rebase-apply","rebase-merge","sequencer","BISECT_LOG"] $ \name->do
    path<-T.stripEnd <$> git root ["rev-parse","--git-path",name]
    active<-doesPathExist (root </> T.unpack path)
    when active (failGit "Finish the current Git operation first.")

-- | Preflight incoming integration for protected/unsupported paths and index state.
-- This check does not itself mutate the repository.
checkIncomingChanges :: FilePath -> Text -> Text -> (FilePath -> Bool) -> IO (Either Text ())
checkIncomingChanges root expected target excluded=result $ do
  checkIntegrationState root expected >>= either failGit pure
  base<-T.strip <$> git root ["merge-base",T.unpack expected,T.unpack target]
  raw<-git root ["diff","--raw","--no-ext-diff","--no-renames","-z",T.unpack base,T.unpack target,"--"]
  rows<-either failGit pure (pairs (filter (not . T.null) (T.splitOn "\NUL" raw)))
  let names from=git root ["diff","--name-status","--no-ext-diff","-z","-M","-C","--find-copies-harder","-l256",T.unpack from,T.unpack target,"--"]
  lineage<-names base
  -- HEAD-side renames can redirect incoming edits into protected destinations.
  -- Refuse protected divergence instead of trying to predict Git's merge tree.
  divergence<-names expected
  (files,_)<-either failGit pure (parseChanges (lineage<>divergence))
  when (length files>10000) (failGit "Incoming changes exceed 10000 paths.")
  forM_ files $ \file->do
    let absolute=normalise (root </> file)
    canonical<-canonicalizePath absolute
    unless (within root absolute && within root canonical && not (excluded absolute || excluded canonical))
      (failGit "Incoming changes touch a protected path.")
  forM_ rows $ \(header,_)->
    when (any (`elem` ["120000","160000"]) (take 2 (T.words (T.drop 1 header))))
      (failGit "Incoming symbolic links and submodules require human Git integration.")
  checkIntegrationState root expected >>= either failGit pure
  where
    pairs []=Right []
    pairs (header:file:rest) | ":" `T.isPrefixOf` header=( (header,file):) <$> pairs rest
    pairs _=Left "Invalid incoming Git change listing."

reviewRepository :: FilePath -> IO (Either Text GitReview)
reviewRepository=reviewWith (const (pure ()))

-- | Capture a bounded complete review with HEAD, index and working-file evidence.
reviewRepositoryChecked :: FilePath -> (FilePath -> Bool) -> IO (Either Text GitReview)
reviewRepositoryChecked path excluded=do
  reviewed<-reviewWith (checkChanges excluded) path
  pure $ reviewed >>= \review ->
    if T.length (reviewText review)>131072 || BS.length (TE.encodeUtf8 (reviewToken review))>1024*1024
      then Left "Complete Git review exceeds the review limit."
      else Right review

reviewWith :: (FilePath -> IO ()) -> FilePath -> IO (Either Text GitReview)
reviewWith check path = result $ do
  root <- repositoryRoot path
  before <- snapshot root
  check root
  rendered <- diffText root []
  check root
  after <- snapshot root
  unless (before == after) (failGit "Repository changed while preparing the review; refresh the diff.")
  pure (GitReview root rendered before)

commitReview :: GitReview -> Text -> IO (Either Text Text)
commitReview review message=do
  committed<-commitWith (const (pure ())) review message
  pure $ committed >>= \outcome ->
    if commitExitCode outcome/=ExitSuccess then Left (commitOutput outcome)
    else Right ((case commitTreeMatched outcome of
      Just True -> ""
      Just False -> "Committed; Git hooks changed the reviewed tree. Inspect HEAD.\n"
      Nothing -> "Committed; the reviewed tree could not be verified. Inspect HEAD.\n")<>commitOutput outcome)

-- | Revalidate the review and stage saved changes repository-wide before committing.
-- Normal hooks run; report whether the final committed tree matches staged evidence.
commitReviewChecked :: GitReview -> Text -> (FilePath -> Bool) -> IO (Either Text GitCommit)
commitReviewChecked review message excluded=commitWith (checkChanges excluded) review message

-- Keep the ordinary whole-repository staging and hook semantics for both callers.
commitWith :: (FilePath -> IO ()) -> GitReview -> Text -> IO (Either Text GitCommit)
commitWith check review message = result $ do
  when (T.null (T.strip message)) (failGit "Enter a nonempty commit message.")
  let root = reviewRoot review
  check root
  current <- snapshot root
  unless (current == reviewToken review)
    (failGit "Repository changed since this review; refresh the diff before approving.")
  _ <- git root ["add", "-A", "--", "."]
  check root
  staged <- snapshot root
  unless (fst (T.breakOn indexSeparator current) == fst (T.breakOn indexSeparator staged))
    (failGit "Repository changed while staging; review again. Changes remain on disk and in the index.")
  expectedTree <- T.strip <$> git root ["write-tree"]
  (code,out,err) <- runGit root ["commit", "-m", T.unpack message]
  tree <- tryIOError (runGit root ["rev-parse", "HEAD^{tree}"])
  let matched=case tree of Right (ExitSuccess,committedTree,_) | code==ExitSuccess -> Just (T.strip committedTree==expectedTree); _ -> Nothing
  pure (GitCommit code matched (out<>err))

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
diffText=diffTextWith []

diffTextWith :: [String] -> FilePath -> [FilePath] -> IO Text
diffTextWith options root paths = do
  let arguments=diffArgs++options
  staged <- git root (arguments ++ ["--cached", "--"] ++ paths)
  working <- git root (arguments ++ ["--"] ++ paths)
  untracked <- fileNames <$> git root (["ls-files", "--others", "--exclude-standard", "-z", "--"] ++ paths)
  newFiles <- forM untracked $ \file -> do
    (code, out, err) <- runGit root (arguments ++ ["--no-index", "--", "/dev/null", file])
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
