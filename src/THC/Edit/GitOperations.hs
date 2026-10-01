{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.GitOperations (withGitOperations, gitOperationEffects, tickGitOperations, gitTools, gitToolNames, gitTool) where

import Control.Concurrent
import Control.Exception (IOException, SomeException, bracket, displayException, mask_, try)
import Control.Monad (foldM, forM, forM_, unless, void)
import Data.Aeson hiding (Result)
import Data.Aeson.Types (Pair,parseEither)
import qualified Data.Aeson.KeyMap as KM
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

data Result = Branches FilePath [T.Text] | Finished ExitCode FilePath T.Text [(Int,FileState,Either T.Text (FileState,Buffer))]
data Worker = Worker Bool (Maybe Integer) ThreadId (MVar (Either SomeException Result))
data GitOperations = GitOperations (IORef (Maybe Worker)) (IORef (Maybe FilePath)) (IORef (Integer,M.Map Integer Value))

withGitOperations :: (GitOperations -> IO a) -> IO a
withGitOperations = bracket (GitOperations <$> newIORef Nothing <*> newIORef Nothing <*> newIORef (0,M.empty)) close
  where
    close (GitOperations ref _ _) = readIORef ref >>= mapM_ (\(Worker _ _ thread done) -> killThread thread >> void (readMVar done))

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

runOperation :: Bool -> FilePath -> GitAction -> Desktop -> IO Result
runOperation privateOutput root action desktop = do
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
  pure (Finished code root (label action<>outcome<>(if privateOutput then "" else out<>err<>conflictText)) refreshed)
  where third (_,_,b)=b

readBranches :: FilePath -> IO Result
readBranches root = do
  current <- T.strip <$> checkedGit root ["branch","--show-current"]
  refs <- checkedGit root ["for-each-ref","--format=%(refname:short)%09%(symref)","refs/heads","refs/remotes"]
  pure (Branches root [name | line<-T.lines refs, let (name,symbolic)=T.breakOn "\t" line, T.null (T.drop 1 symbolic), name/=current, not (T.null name)])

gitOperationEffects :: GitOperations -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
gitOperationEffects runtime@(GitOperations ref _ _) core = foldM apply . (False,)
  where
    apply state@(True,_) _ = pure state
    apply (_,desktop) effect = do
      worker <- readIORef ref
      let busy=maybe False (const True) worker
          mutating=case worker of Just (Worker changes _ _ _) -> changes; Nothing -> False
      case effect of
        Exit | busy -> pure (False,desktop {status="Wait for the Git operation to finish before quitting."})
        SaveDocument{} | mutating -> pure (False,desktop {status="Wait for the Git operation to finish before saving."})
        AgentAction action _ | mutating, action `elem` ["run","compile","make"] || "approval:" `T.isPrefixOf` action ->
          pure (False,desktop {status="Wait for the Git operation to finish before approving agent actions or running commands."})
        WriteGitCommit{} | busy -> pure (False,desktop {status="Wait for the Git operation to finish before committing."})
        RunGit action -> start busy (action/=FetchRemote) desktop (label action) (\root -> runOperation False root action desktop)
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
        startWorker runtime mutates Nothing (action root)
        pure (False,desktop {status=title<>"…",gitReview=Nothing})

-- Applying results happens on the UI thread. Any buffer edited while Git ran is
-- kept with its old disk baseline, so a later Save detects the disk conflict.
tickGitOperations :: GitOperations -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO Desktop
tickGitOperations (GitOperations ref focused jobs) core initial = do
  previous <- readIORef focused
  desktop <- case activeDocument initial of
    Just doc | documentLabel doc==Nothing, Just file<-documentFile doc, let directory=takeDirectory (filePath file), previous/=Just directory -> do
      writeIORef focused (Just directory)
      snd <$> core initial [RefreshGit directory]
    _ -> pure initial
  worker <- readIORef ref
  case worker of
    Nothing -> pure desktop
    Just (Worker _ jobId _ done) -> do
      result <- tryReadMVar done
      case result of
        Nothing -> pure desktop
        Just finished -> do
          writeIORef ref Nothing
          forM_ jobId $ \ident -> modifyIORef' jobs $ \(latest,history) ->
            (latest,M.insert ident (jobValue ident False (case finished of Right (Finished code _ _ _) -> Just code; _ -> Nothing)) history)
          case finished of
            Left _ | Just _<-jobId -> pure desktop {status="Fetch failed; see git_operation_status.",gitReview=Nothing}
            Left err -> pure ((addReadOnly "Git operations" (T.pack (displayException err)) desktop) {status="Git operation failed; see Git operations.",gitReview=Nothing})
            Right (Branches root _) | branchRoot desktop/=Just root -> pure desktop {status="Repository changed; request the merge branches again."}
            Right (Branches _ []) -> pure desktop {status="No other branches are available to merge."}
            Right (Branches _ branches) -> pure (prompt "Merge branch" (Merging branches) [ListBox "Branch" branches 0] desktop)
            Right (Finished _ root logText reloads) -> do
              let (reloaded,notes) = foldl reload (desktop,[]) reloads
                  shown = if jobId/=Nothing then reloaded else addReadOnly "Git operations" (logText<>T.concat (reverse notes)) reloaded
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

-- Both entry points run under the desktop lock and reserve the same worker slot.
-- Mask registration so session shutdown always owns the started subprocess.
startWorker :: GitOperations -> Bool -> Maybe Integer -> IO Result -> IO ()
startWorker (GitOperations ref _ _) mutates ident action=mask_ $ do
  done<-newEmptyMVar
  thread<-forkIOWithUnmask $ \unmask -> try (unmask action) >>= putMVar done
  writeIORef ref (Just (Worker mutates ident thread done))

gitToolNames :: [T.Text]
gitToolNames=["git_fetch","git_operation_status"]

gitTools :: [Value]
gitTools=[spec "git_fetch" "Fetch the selected repository's configured default remote. Returns accepted and jobId, not completion. Poll git_operation_status. Does not change working files or expose transport output." False [],
  spec "git_operation_status" "Read a Git fetch job's actual completion and exit code. Omit jobId for the latest job. Retains the latest 16 agent jobs; busy includes human Git operations." True [("jobId",object ["type" .= ("integer"::T.Text),"minimum" .= (1::Int)])]]
  where
    spec :: T.Text -> T.Text -> Bool -> [Pair] -> Value
    spec name description readOnly props=object ["name" .= name,"description" .= description,
      "inputSchema" .= object ["type" .= ("object"::T.Text),"properties" .= object props,"required" .= ([]::[T.Text]),"additionalProperties" .= False],
      "annotations" .= object ["readOnlyHint" .= readOnly,"destructiveHint" .= False,"openWorldHint" .= not readOnly]]

jobValue :: Integer -> Bool -> Maybe ExitCode -> Value
jobValue ident active code=object ["jobId" .= ident,"active" .= active,
  "state" .= (if active then "running" else case code of Just ExitSuccess -> "succeeded"; _ -> "failed" :: T.Text),
  "exitCode" .= fmap (\value -> case value of ExitSuccess -> 0; ExitFailure n -> n) code]

gitTool :: GitOperations -> Desktop -> T.Text -> Value -> IO (Desktop,IO (Either T.Text Value))
gitTool runtime@(GitOperations ref _ jobs) desktop name args=case parseEither (withObject "arguments" pure) args of
  Left _ -> reply desktop (Left "Expected an arguments object.")
  Right fields | any (`notElem` allowed) (KM.keys fields) -> reply desktop (Left "Unexpected Git tool argument.")
  Right fields -> case name of
    "git_fetch" -> do
      worker<-readIORef ref
      case worker of
        Just _ -> reply desktop (Left "A Git operation is already running.")
        Nothing -> mask_ $ do
          (previous,history)<-readIORef jobs
          let ident=previous+1
              kept=M.filterWithKey (\key _ -> key>ident-16) history
              directory=maybe (startingDirectory desktop) id (branchRoot desktop)
              fetch=do
                root<-checkedGit directory ["rev-parse","--show-toplevel"] >>= canonicalizePath . T.unpack . T.stripEnd
                runOperation True root FetchRemote desktop
          startWorker runtime False (Just ident) fetch
          writeIORef jobs (ident,M.insert ident (jobValue ident True Nothing) kept)
          reply desktop {status="Fetch…",gitReview=Nothing} (Right (object ["accepted" .= True,"jobId" .= ident]))
    "git_operation_status" -> case parseEither (\_ -> fields .:? "jobId") args of
      Left _ -> reply desktop (Left "jobId must be a positive integer.")
      Right wanted -> do
        (latest,history)<-readIORef jobs
        worker<-readIORef ref
        let ident=maybe latest id wanted
        if maybe False (<1) wanted then reply desktop (Left "jobId must be a positive integer.")
        else case M.lookup ident history of
          Nothing | wanted/=Nothing -> reply desktop (Left "Git job is unknown or no longer retained.")
          result -> reply desktop (Right (object ["busy" .= maybe False (const True) worker,"operation" .= result]))
    _ -> reply desktop (Left "Unknown Git tool.")
  where
    allowed=if name=="git_operation_status" then ["jobId"] else []
    reply updated result=pure (updated,pure result)
