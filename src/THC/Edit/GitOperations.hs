{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.GitOperations (withGitOperations, gitOperationEffects, tickGitOperations) where

import Control.Concurrent
import Control.Exception (IOException, SomeException, bracket, displayException, try)
import Control.Monad (foldM, forM, unless, void)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)
import System.Process (proc, readCreateProcessWithExitCode, cwd, env)
import THC.Edit.Buffer
import THC.Edit.Files
import THC.Edit.Model

data Result = Branches FilePath [T.Text] | Finished FilePath T.Text [(Int,FileState,Either T.Text (FileState,Buffer))]
data Worker = Worker Bool ThreadId (MVar (Either SomeException Result))
data GitOperations = GitOperations (IORef (Maybe Worker)) (IORef (Maybe FilePath))

withGitOperations :: (GitOperations -> IO a) -> IO a
withGitOperations = bracket (GitOperations <$> newIORef Nothing <*> newIORef Nothing) close
  where
    close (GitOperations ref _) = readIORef ref >>= mapM_ (\(Worker _ thread done) -> killThread thread >> void (readMVar done))

-- readCreateProcessWithExitCode owns and closes its pipes and terminates its child
-- on cancellation; a normal Quit is deferred until the Git operation completes.
runGit :: FilePath -> [String] -> IO (ExitCode,T.Text,T.Text)
runGit root args = do
  inherited <- getEnvironment
  let cleared = ["GIT_DIR","GIT_WORK_TREE","GIT_INDEX_FILE","GIT_COMMON_DIR","GIT_TERMINAL_PROMPT","GIT_MERGE_AUTOEDIT"]
      environment = [("GIT_TERMINAL_PROMPT","0"),("GIT_MERGE_AUTOEDIT","no")] ++ filter ((`notElem` cleared) . fst) inherited
  (code,out,err) <- readCreateProcessWithExitCode
    ((proc "git" (["--no-pager","--literal-pathspecs","-c","core.fsmonitor=false","-C",root] ++ args)) {cwd=Just root,env=Just environment}) ""
  pure (code,T.pack out,T.pack err)

checkedGit :: FilePath -> [String] -> IO T.Text
checkedGit root args = do
  (code,out,err) <- runGit root args
  if code==ExitSuccess then pure out else ioError (userError (T.unpack (out<>err)))

owner :: FilePath -> IO (Maybe FilePath)
owner path = do
  result <- try (checkedGit (takeDirectory path) ["rev-parse","--show-toplevel"] >>= canonicalizePath . T.unpack . T.stripEnd) :: IO (Either IOException FilePath)
  pure (either (const Nothing) Just result)

label :: GitAction -> T.Text
label FetchRemote = "Fetch"
label PullRemote = "Pull (fast-forward only)"
label (MergeBranch name) = "Merge "<>name

runOperation :: FilePath -> GitAction -> Desktop -> IO Result
runOperation root action desktop = do
  documents <- fmap catMaybes $ forM (M.toList (buffers desktop)) $ \(bid,doc) -> case documentFile doc of
    Nothing -> pure Nothing
    Just file -> do
      repository <- owner (filePath file)
      pure (if repository==Just root && documentLabel doc==Nothing then Just (bid,file,documentBuffer doc) else Nothing)
  let mutates = action /= FetchRemote
      unnamedDirty = any (\doc -> documentFile doc==Nothing && documentLabel doc==Nothing && dirty (documentBuffer doc)) (M.elems (buffers desktop))
  unless (not mutates || (not unnamedDirty && all (not . dirty . third) documents))
    (ioError (userError "Save or discard unsaved buffers in this repository before pulling or merging."))
  let args = case action of FetchRemote -> ["fetch"]; PullRemote -> ["pull","--ff-only"]; MergeBranch branch -> ["merge","--no-edit","--",T.unpack branch]
  (code,out,err) <- runGit root args
  conflicts <- if mutates then checkedGit root ["diff","--name-only","--diff-filter=U","--"] else pure ""
  refreshed <- if not mutates then pure [] else forM documents $ \(bid,file,_) -> do
    exists <- doesFileExist (filePath file)
    loaded <- if not exists then pure (Left "File was removed on disk; its buffer was preserved.")
      else either (Left . T.pack) Right <$> loadFile (filePath file)
    pure (bid,file,loaded)
  let outcome = if code==ExitSuccess then " completed.\n" else " failed ("<>T.pack (show code)<>").\n"
      conflictText = if T.null conflicts then "" else "\nMerge conflicts remain in these files; resolve them before committing:\n"<>conflicts
  pure (Finished root (label action<>outcome<>out<>err<>conflictText) refreshed)
  where third (_,_,b)=b

readBranches :: FilePath -> IO Result
readBranches root = do
  current <- T.strip <$> checkedGit root ["branch","--show-current"]
  refs <- checkedGit root ["for-each-ref","--format=%(refname:short)%09%(symref)","refs/heads","refs/remotes"]
  pure (Branches root [name | line<-T.lines refs, let (name,symbolic)=T.breakOn "\t" line, T.null (T.drop 1 symbolic), name/=current, not (T.null name)])

gitOperationEffects :: GitOperations -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
gitOperationEffects (GitOperations ref _) core = foldM apply . (False,)
  where
    apply state@(True,_) _ = pure state
    apply (_,desktop) effect = do
      worker <- readIORef ref
      let busy=maybe False (const True) worker
          mutating=case worker of Just (Worker changes _ _) -> changes; Nothing -> False
      case effect of
        Exit | busy -> pure (False,desktop {status="Wait for the Git operation to finish before quitting."})
        SaveDocument{} | mutating -> pure (False,desktop {status="Wait for the Git operation to finish before saving."})
        AgentAction action _ | mutating, action `elem` ["run","compile","make"] || "approval:" `T.isPrefixOf` action ->
          pure (False,desktop {status="Wait for the Git operation to finish before approving agent actions or running commands."})
        WriteGitCommit{} | busy -> pure (False,desktop {status="Wait for the Git operation to finish before committing."})
        RunGit action -> start busy (action/=FetchRemote) desktop (label action) (\root -> runOperation root action desktop)
        ReadMergeBranches -> start busy False desktop "Reading merge branches" readBranches
        _ -> do
          (exited,updated) <- core desktop [effect]
          pure $ if busy && exited
            then (False,updated {status="Wait for the Git operation to finish before quitting."})
            else (exited,updated)
    start True _ desktop _ _ = pure (False,desktop {status="A Git operation is already running."})
    start False mutates desktop title action = case branchRoot desktop of
      Nothing -> pure (False,desktop {status="No Git repository is selected."})
      Just root -> do
        done <- newEmptyMVar
        thread <- forkFinally (action root) (putMVar done)
        writeIORef ref (Just (Worker mutates thread done))
        pure (False,desktop {status=title<>"…",gitReview=Nothing})

-- Applying results happens on the UI thread. Any buffer edited while Git ran is
-- kept with its old disk baseline, so a later Save detects the disk conflict.
tickGitOperations :: GitOperations -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO Desktop
tickGitOperations (GitOperations ref focused) core initial = do
  previous <- readIORef focused
  desktop <- case activeDocument initial of
    Just doc | documentLabel doc==Nothing, Just file<-documentFile doc, let directory=takeDirectory (filePath file), previous/=Just directory -> do
      writeIORef focused (Just directory)
      snd <$> core initial [RefreshGit directory]
    _ -> pure initial
  worker <- readIORef ref
  case worker of
    Nothing -> pure desktop
    Just (Worker _ _ done) -> do
      result <- tryTakeMVar done
      case result of
        Nothing -> pure desktop
        Just finished -> do
          writeIORef ref Nothing
          case finished of
            Left err -> pure ((addReadOnly "Git operations" (T.pack (displayException err)) desktop) {status="Git operation failed; see Git operations.",gitReview=Nothing})
            Right (Branches root _) | branchRoot desktop/=Just root -> pure desktop {status="Repository changed; request the merge branches again."}
            Right (Branches _ []) -> pure desktop {status="No other branches are available to merge."}
            Right (Branches _ branches) -> pure (prompt "Merge branch" (Merging branches) [ListBox "Branch" branches 0] desktop)
            Right (Finished root logText reloads) -> do
              let (reloaded,notes) = foldl reload (desktop,[]) reloads
                  shown = addReadOnly "Git operations" (logText<>T.concat (reverse notes)) reloaded
              (_,updated) <- core shown {gitReview=Nothing} [RefreshGit root]
              writeIORef focused Nothing
              pure updated {status=T.takeWhile (/='\n') logText}
  where
    reload state@(d,notes) (bid,oldFile,result) = case M.lookup bid (buffers d) of
      Just doc | documentFile doc==Just oldFile && documentLabel doc==Nothing ->
        let original=documentBuffer doc
            note text=(d,("\n"<>T.pack (filePath oldFile)<>" — "<>text<>"\n"):notes)
        in case result of
          Left err -> note err
          Right (file,b)
            | filePath file/=filePath oldFile -> note "File path was replaced by a symbolic link; reopen it explicitly. Buffer preserved."
            | diskBytes file==diskBytes oldFile -> state
            | dirty original -> note "Changed on disk while you edited; unsaved buffer preserved."
            | otherwise ->
                let loaded=if byteMode original then newByteBuffer (bufferBytes b) else b
                    fresh=markSaved (replaceBuffer (byteMode loaded) (contents loaded) original)
                    clamp n=max 0 (min (bufferLength fresh) n)
                    adjust w=if bufferId w==bid then w {selection=let Selection a c=selection w in Selection (clamp a) (clamp c)} else w
                in (ensureVisible d {buffers=M.insert bid (restyle doc {documentFile=Just file,documentBuffer=fresh}) (buffers d),windows=map adjust (windows d)},notes)
      _ -> state
