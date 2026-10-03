{-# LANGUAGE OverloadedStrings #-}
module AgentWorkspaceCheck (checks) where

import Control.Exception (bracket)
import Control.Monad (unless, void, forM_)
import Data.Aeson (encode, eitherDecode)
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.Info (os)
import System.IO (hClose, openTempFile)
import System.Process (proc, cwd, readCreateProcessWithExitCode)
import Hide.AgentWorkspace

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root -> do
  let repository=root </> "repo spaces ' ; $literal"
      store=root </> "private data"
      tracked="source.txt"
      gitAt path args=do
        (code,out,err)<-readCreateProcessWithExitCode
          ((proc "git" (["-c","user.name=Workspace Test","-c","user.email=test@example.invalid",
            "-c","commit.gpgsign=false"]++args)) {cwd=Just path}) ""
        unless (code==ExitSuccess) (error err)
        pure (T.strip (T.pack out))
      git=gitAt repository
      right=either (error . T.unpack) pure
      rejected result=check "invalid worktree request is rejected" (case result of Left _->True; _->False)
  createDirectory repository
  void (git ["init","-b","main"])
  void (git ["config","core.hooksPath",repository </> ".git/hooks"])
  writeFile (repository </> tracked) "committed\n"
  void (git ["add","--",tracked])
  void (git ["commit","-m","base"])
  base<-git ["rev-parse","HEAD"]
  writeFile (repository </> tracked) "uncommitted parent\n"
  writeFile (repository </> "untracked.txt") "private working contents\n"
  bracket (lookupEnv "XDG_DATA_HOME" <* setEnv "XDG_DATA_HOME" store)
    (maybe (unsetEnv "XDG_DATA_HOME") (setEnv "XDG_DATA_HOME")) $ \_ -> do
      detached<-createAgentWorktree repository Nothing Nothing "feature / spaces λ" >>= right
      let path=workspacePath detached
      canonical<-canonicalizePath path
      check "detached metadata identifies canonical path and exact committed base"
        (path==canonical && workspaceSourceRepo detached==Just repository && workspaceBaseCommit detached==Just base &&
         workspaceBranch detached==Nothing && workspaceMode detached=="worktree" &&
         takeDirectory path==store </> "thc-edit/workspaces")
      contents<-readFile (path </> tracked)
      copied<-doesFileExist (path </> "untracked.txt")
      check "worktree excludes uncommitted and untracked parent contents" (contents=="committed\n" && not copied)
      childCwd<-gitAt path ["rev-parse","--show-toplevel"]
      check "commands run in the child checkout" (T.unpack childCwd==path)
      writeFile (path </> tracked) "child edit\n"
      writeFile (path </> "build-output") "isolated build\n"
      parentContents<-readFile (repository </> tracked)
      parentBuild<-doesFileExist (repository </> "build-output")
      check "child edits and build outputs do not modify the parent checkout" (parentContents=="uncommitted parent\n" && not parentBuild)
      named<-createAgentWorktree repository (Just "HEAD") (Just "feature/child-work") "named" >>= right
      actualBranch<-gitAt (workspacePath named) ["symbolic-ref","--short","HEAD"]
      check "named worktree creates the requested new branch" (actualBranch=="feature/child-work" && workspaceBranch named==Just actualBranch)
      check "workspace metadata survives JSON roundtrip" (eitherDecode (encode named)==Right named)
      shared<-sharedAgentWorkspace repository >>= right
      check "shared workspace keeps the parent path and reports committed metadata"
        (workspacePath shared==repository && workspaceMode shared=="shared" && workspaceBaseCommit shared==Just base && workspaceBranch shared==Just "main")
      plain<-sharedAgentWorkspace root >>= right
      check "shared directories need not be Git repositories" (workspacePath plain==root && workspaceSourceRepo plain==Nothing)
      before<-listDirectory (takeDirectory path)
      forM_ ["--help","HEAD; touch injected","missing-ref"] $ \ref ->
        createAgentWorktree repository (Just ref) Nothing "bad ref" >>= rejected
      forM_ ["--force","../invalid","branch with spaces","feature/child-work"] $ \branch ->
        createAgentWorktree repository Nothing (Just branch) "bad branch" >>= rejected
      createAgentWorktree root Nothing Nothing "not repo" >>= rejected
      createAgentWorktree (repository </> tracked) Nothing Nothing "not directory" >>= rejected
      createAgentWorktree (root </> "missing") Nothing Nothing "missing" >>= rejected
      after<-listDirectory (takeDirectory path)
      check "rejected requests leave no allocation or branch overwrite" (length before==length after)
      unchanged<-readFile (path </> tracked)
      check "failures preserve existing child changes" (unchanged=="child edit\n")
      -- A post-checkout failure can leave a valid checkout. Never remove it.
      unless (os=="mingw32") $ do
        let hook=repository </> ".git/hooks/post-checkout"
        writeFile hook "#!/bin/sh\nprintf 'retained work' > checkout-evidence\nexit 1\n"
        permissions<-getPermissions hook
        setPermissions hook permissions {executable=True}
        createAgentWorktree repository Nothing Nothing "failed-checkout" >>= rejected
        allocations<-listDirectory (takeDirectory path)
        evidence<-filterMFile [takeDirectory path </> name </> "checkout-evidence" | name<-allocations]
        check "failed checkout keeps nonempty work for human inspection" (length evidence==1)
  putStrLn "Agent workspace checks passed"
  where
    filterMFile []=pure []
    filterMFile (path:paths)=do
      found<-doesFileExist path
      rest<-filterMFile paths
      pure (if found then path:rest else rest)

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)

temporary :: IO FilePath
temporary=do
  base<-getTemporaryDirectory
  (path,handle)<-openTempFile base "thc-workspace-check"
  hClose handle
  removeFile path
  createDirectory path
  canonicalizePath path
