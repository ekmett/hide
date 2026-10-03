{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | Canonical shared directories and persistent Git worktrees for agents.
--
-- A worktree starts from a resolved commit, not the parent's dirty or unsaved
-- contents. It may use a new named branch or detached HEAD. Successful worktrees
-- outlive the agent; failure cleanup removes only an empty directory allocated
-- by that call. Git subprocesses discard inherited repository/index overrides.
module Hide.AgentWorkspace
  (AgentWorkspace(..), createAgentWorktree, sharedAgentWorkspace) where

import Control.Exception (IOException, catch, onException, try)
import Control.Monad (unless)
import Data.Aeson
import Data.Char (isAsciiLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (proc, cwd, env, readCreateProcessWithExitCode)
import Hide.RemoteEndpoint (privateDirectory, randomIdentity)

-- | Workspace location and provenance, not a lease that deletes the checkout.
data AgentWorkspace = AgentWorkspace
  { workspacePath :: FilePath
  , workspaceSourceRepo :: Maybe FilePath
  , workspaceBaseCommit :: Maybe Text
  , workspaceBranch :: Maybe Text
  , workspaceMode :: Text
  } deriving (Eq,Show)

instance ToJSON AgentWorkspace where
  toJSON workspace=object
    ["path" .= workspacePath workspace,"sourceRepo" .= workspaceSourceRepo workspace,
     "baseCommit" .= workspaceBaseCommit workspace,"branch" .= workspaceBranch workspace,
     "mode" .= workspaceMode workspace]
instance FromJSON AgentWorkspace where
  parseJSON=withObject "agent workspace" $ \o -> AgentWorkspace <$> o .: "path"
    <*> o .:? "sourceRepo" <*> o .:? "baseCommit" <*> o .:? "branch" <*> o .: "mode"

-- | Create a persistent worktree from a resolved commit. Nonempty partial
-- checkouts and created branches remain available after failure for inspection.
createAgentWorktree :: FilePath -> Maybe Text -> Maybe Text -> Text -> IO (Either Text AgentWorkspace)
createAgentWorktree parent ref branch feature=workspaceIO $ do
  directory<-existingDirectory parent
  inside<-git directory ["rev-parse","--is-inside-work-tree"]
  unless (inside=="true") (ioError (userError "Agent worktrees require a non-bare Git repository"))
  repository<-git directory ["rev-parse","--show-toplevel"] >>= canonicalizePath . T.unpack
  let revision=fromMaybe "HEAD" ref
  unless (not (T.null revision) && not (T.any (=='\0') revision))
    (ioError (userError "Invalid Git worktree ref"))
  commit<-git repository ["rev-parse","--verify","--end-of-options",T.unpack revision++"^{commit}"]
  case branch of
    Nothing->pure ()
    Just name->do
      unless (not (T.null name) && not ("-" `T.isPrefixOf` name) && not (T.any (=='\0') name))
        (ioError (userError "Invalid Git worktree branch"))
      _<-git repository ["check-ref-format","refs/heads/"++T.unpack name]
      pure ()
  store<-getXdgDirectory XdgData "thc-edit/workspaces"
  privateDirectory store
  unique<-randomIdentity
  let allocated=store </> slug feature++"-"++take 16 unique
      cleanup=removeDirectory allocated `catch` (\(_::IOException)->pure ())
      arguments=["worktree","add"]++maybe ["--detach"] (\name->["-b",T.unpack name]) branch++
        ["--",allocated,T.unpack commit]
  createDirectory allocated
  -- Only an empty directory allocated here can be removed. A failed checkout's
  -- nonempty files and any created branch remain available for inspection.
  (do
      _<-git repository arguments
      path<-canonicalizePath allocated
      pure (AgentWorkspace path (Just repository) (Just commit) branch "worktree"))
    `onException` cleanup
    `catch` (\(err::IOException)->ioError (userError
      ("Worktree creation failed at "++allocated++"; any nonempty checkout was retained. "++show err)))

-- | Validate and canonicalize an existing directory; Git metadata is optional.
sharedAgentWorkspace :: FilePath -> IO (Either Text AgentWorkspace)
sharedAgentWorkspace parent=workspaceIO $ do
  directory<-existingDirectory parent
  metadata<-try $ do
    repository<-git directory ["rev-parse","--show-toplevel"] >>= canonicalizePath . T.unpack
    commit<-git directory ["rev-parse","--verify","HEAD^{commit}"]
    branch<-try (git directory ["symbolic-ref","--quiet","--short","HEAD"])
    pure (Just repository,Just commit,either (const Nothing) Just (branch::Either IOException Text))
  let (repository,commit,branch)=either (const (Nothing,Nothing,Nothing)) id
        (metadata::Either IOException (Maybe FilePath,Maybe Text,Maybe Text))
  pure (AgentWorkspace directory repository commit branch "shared")

existingDirectory :: FilePath -> IO FilePath
existingDirectory path=do
  exists<-doesDirectoryExist path
  unless exists (ioError (userError "Agent workspace must be an existing directory"))
  canonicalizePath path

slug :: Text -> String
slug label=case take 48 (unwordsHyphens (T.unpack (T.toLower label))) of
  ""->"feature"
  value->value
  where
    unwordsHyphens=concat . zipWith (++) ("":repeat "-") . words . map safe
    safe c | isAsciiLower c || c>='0' && c<='9'=c
           | otherwise=' '

git :: FilePath -> [String] -> IO Text
git directory arguments=do
  environment<-filter (\(key,_)->key `notElem` ["GIT_DIR","GIT_WORK_TREE","GIT_INDEX_FILE","GIT_COMMON_DIR"]) <$> getEnvironment
  (code,output,errors)<-readCreateProcessWithExitCode
    ((proc "git" arguments) {cwd=Just directory,env=Just environment}) ""
  case code of
    ExitSuccess->pure (fromMaybe (T.pack output) (T.stripSuffix "\n" (T.pack output)))
    _->ioError (userError ("Git workspace operation failed: "++take 2048 errors))

workspaceIO :: IO a -> IO (Either Text a)
workspaceIO action=do
  result<-try action
  pure $ case result of
    Left (err::IOException)->Left (T.take 4096 (T.pack (show err)))
    Right value->Right value
