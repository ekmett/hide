{-# LANGUAGE OverloadedStrings #-}
module GitCheck (checks) where

import Control.Exception (bracket)
import Control.Monad (unless, void)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (proc, readCreateProcessWithExitCode, cwd)
import THC.Edit.Git

checks :: IO ()
checks = bracket temporary removePathForcibly $ \base -> do
  let dir = base </> "repo"
      file = dir </> "file with spaces.txt"
      git args = do
        (code, out, err) <- readCreateProcessWithExitCode
          ((proc "git" (["-c", "user.name=Git Test", "-c", "user.email=test@example.invalid", "-c", "commit.gpgsign=false"] ++ args)) { cwd = Just dir }) ""
        unless (code == ExitSuccess) (error err)
        pure out
  createDirectory dir
  absent <- repositoryStatus dir
  check "nonrepository has no status" (absent == Nothing)
  void (git ["init", "-b", "main"])
  void (git ["config", "core.hooksPath", dir </> ".git" </> "hooks"])
  initial <- repositoryStatus dir
  check "unborn branch has clean status" (fmap repoBranch initial == Just "main" && fmap repoDirty initial == Just False)
  writeFile file "original\n"
  void (git ["add", "--", "file with spaces.txt"])
  unborn <- repositoryDiff dir Nothing >>= right
  check "unborn diff includes staged file" ("+original" `T.isInfixOf` unborn)
  void (git ["commit", "-m", "initial"])
  clean <- repositoryStatus file
  check "file locates clean repository" (fmap repoDirty clean == Just False)
  writeFile file "staged\n"
  void (git ["add", "--", "file with spaces.txt"])
  writeFile file "working\n"
  writeFile (dir </> "new file.txt") "untracked content\n"
  dirty <- repositoryStatus dir
  check "modified and untracked repository is dirty" (fmap repoDirty dirty == Just True)
  changes <- repositoryDiff dir Nothing >>= right
  check "diff includes staged unstaged and untracked" (all (`T.isInfixOf` changes) ["+staged", "+working", "new file.txt"])
  one <- repositoryDiff dir (Just "file with spaces.txt") >>= right
  check "literal file filter retains spaces" ("+working" `T.isInfixOf` one && not ("new file.txt" `T.isInfixOf` one))
  outside <- repositoryDiff dir (Just (base </> "outside"))
  check "outside path rejected" (isLeft outside)
  indexReview <- reviewRepository dir >>= right
  void (git ["add", "--", "file with spaces.txt"])
  indexStale <- commitReview indexReview "must not commit changed index"
  check "index changes invalidate review" (isLeft indexStale)
  BS.writeFile (dir </> "binary.dat") (BS.pack [0, 255, 1, 2])
  createFileLink "missing-target" (dir </> "new-link")
  review <- reviewRepository dir >>= right
  check "review shows untracked contents" ("+untracked content" `T.isInfixOf` reviewText review)
  check "review labels binary untracked content with hash" (all (`T.isInfixOf` reviewText review) ["binary.dat", "Binary files", "Content hash:"])
  emptyMessage <- commitReview review "   "
  check "blank commit message rejected" (isLeft emptyMessage)
  writeFile file "changed after review\n"
  stale <- commitReview review "must not commit"
  check "stale review rejected" (isLeft stale)
  -- Repository-local identity belongs only to this disposable test repository.
  void (git ["config", "user.name", "Git Test"])
  void (git ["config", "user.email", "test@example.invalid"])
  void (git ["config", "commit.gpgsign", "false"])
  fresh <- reviewRepository dir >>= right
  _ <- commitReview fresh "approved changes" >>= right
  committed <- repositoryStatus dir
  check "approved review committed all reviewed changes" (fmap repoDirty committed == Just False)
  message <- git ["log", "-1", "--format=%s"]
  check "commit uses approved message" (message == "approved changes\n")
  removeFile file
  deletion <- reviewRepository dir >>= right
  _ <- commitReview deletion "delete reviewed file" >>= right
  deleted <- repositoryStatus dir
  check "reviewed deletion commits successfully" (fmap repoDirty deleted == Just False)
  writeFile (dir </> "untracked only.txt") "new\n"
  untrackedOnly <- repositoryStatus dir
  check "untracked file alone makes repository dirty" (fmap repoDirty untrackedOnly == Just True)
  writeFile (dir </> ":(glob)*") "literal path\n"
  literal <- repositoryDiff dir (Just ":(glob)*") >>= right
  check "pathspec metacharacters are literal" ("literal path" `T.isInfixOf` literal && not ("untracked only.txt" `T.isInfixOf` literal))
  let hook = dir </> ".git" </> "hooks" </> "pre-commit"
  writeFile hook "#!/bin/sh\necho hook-refused >&2\nexit 1\n"
  permissions <- getPermissions hook
  setPermissions hook (permissions { executable = True })
  beforeFailure <- git ["rev-parse", "HEAD"]
  failingReview <- reviewRepository dir >>= right
  failure <- commitReview failingReview "hook must reject"
  afterFailure <- git ["rev-parse", "HEAD"]
  preserved <- readFile (dir </> "untracked only.txt")
  check "failed commit preserves HEAD and file contents" (isLeft failure && beforeFailure == afterFailure && preserved == "new\n")
  check "failed commit reports hook error" (either (T.isInfixOf "hook-refused") (const False) failure)
  writeFile hook "#!/bin/sh\nprintf 'hook transformed\\n' > hook-output.txt\ngit add -- hook-output.txt\n"
  hookReview <- reviewRepository dir >>= right
  hookResult <- commitReview hookReview "normal transforming hook" >>= right
  check "transforming hook is reported after successful commit" ("Git hooks changed" `T.isInfixOf` hookResult)
  removeFile hook
  void (git ["checkout", "--detach", "HEAD"])
  detached <- repositoryStatus dir
  check "detached head is identified" (maybe False (T.isPrefixOf "detached@" . repoBranch) detached)
  bracket (lookupEnv "PATH") (maybe (unsetEnv "PATH") (setEnv "PATH")) $ \_ -> do
    setEnv "PATH" (base </> "no-executables")
    missing <- repositoryStatus dir
    failed <- repositoryDiff dir Nothing
    check "missing git returns errors without crashing" (missing == Nothing && isLeft failed)
  putStrLn "git checks passed"
  where
    temporary = do
      base <- getTemporaryDirectory
      (path, handle) <- openTempFile base "thc-edit-git-check"
      hClose handle
      removeFile path
      createDirectory path
      pure path
    check name ok = unless ok (error name)
    right = either (error . T.unpack) pure
    isLeft (Left _) = True
    isLeft _ = False
