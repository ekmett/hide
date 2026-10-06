{-# LANGUAGE OverloadedStrings #-}
-- | Serialize UI and MCP Git work through one owned background worker.
--
-- Both entry paths reserve the same worker under desktop serialization. Mutations
-- interlock with conflicting saves/commands and ordinary quit waits. Agent pull
-- separates fetch/preparation from integration under current policy. One-time review
-- IDs bind commits to evidence. Reload adopts only matching clean buffers, leaving
-- intervening edits and their save-conflict baselines intact.
module Hide.GitOperations (withGitOperations, gitOperationEffects, tickGitOperations, gitTools, gitToolNames, gitTool) where

import Control.Concurrent
import Control.Exception (IOException, SomeException, bracket, displayException, mask_, try)
import Control.Monad (foldM, forM, forM_, unless, void)
import Data.Aeson hiding (Result)
import Data.Aeson.Types (Pair,parseEither)
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isHexDigit)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)
import System.Process (proc, readCreateProcessWithExitCode, cwd, env)
import Hide.Buffer
import Hide.Files
import Hide.Model
import Hide.Git (GitReview(..),GitCommit(..),reviewRepositoryChecked,commitReviewChecked,checkIntegrationState,checkIncomingChanges)
import Hide.GuestAccess (protectedPathParent)
import Hide.RemoteEndpoint (randomIdentity)

data Result = Branches FilePath [T.Text]
  | Finished ExitCode FilePath T.Text [(Int,FileState,Either T.Text (FileState,Buffer))] Value
  | Prepared FilePath GitAction T.Text T.Text T.Text (Maybe ExitCode)
  | IntegrationFailed FilePath (Maybe ExitCode) Value
  | Reviewed T.Text [FilePath] (Either T.Text GitReview)
  | Committed FilePath (Either T.Text GitCommit) (Maybe T.Text)
data Worker = Worker Bool (Maybe Integer) ThreadId (MVar (Either SomeException Result))
data GitOperations = GitOperations (IORef (Maybe Worker)) (IORef (Maybe FilePath)) (IORef (Integer,M.Map Integer Value)) (IORef (Maybe (T.Text,GitReview))) (IO Bool)

-- | Scope the Git worker and pending operation state. The serialized owner
-- reserves terminal launch before spawning its worker; mutating Git operations
-- cannot start until that unadopted/cancelling lifetime releases its reservation.
-- Already adopted consoles retain their existing independent execution policy.
withGitOperations :: IO Bool -> (GitOperations -> IO a) -> IO a
withGitOperations terminalLaunch = bracket (GitOperations <$> newIORef Nothing <*> newIORef Nothing <*> newIORef (0,M.empty) <*> newIORef Nothing <*> pure terminalLaunch) close
  where
    close (GitOperations ref _ _ _ _) = readIORef ref >>= mapM_ (\(Worker _ _ thread done) -> killThread thread >> void (readMVar done))

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
runOperation privateOutput root action = runOperationArgs privateOutput root action arguments
  where arguments=case action of FetchRemote -> ["fetch"]; PullRemote -> ["pull","--ff-only"]; MergeBranch branch -> ["merge","--no-edit","--",T.unpack branch]

runOperationArgs :: Bool -> FilePath -> GitAction -> [String] -> Desktop -> IO Result
runOperationArgs privateOutput root action args desktop = do
  documents <- fmap catMaybes $ forM (M.toList (buffers desktop)) $ \(bid,doc) -> case documentFile doc of
    Nothing -> pure Nothing
    Just file -> do
      repository <- owner (filePath file)
      pure (if repository==Just root && documentLabel doc==Nothing then Just (bid,file,documentBuffer doc) else Nothing)
  let mutates = action /= FetchRemote
      unnamedDirty = any (\doc -> documentFile doc==Nothing && documentLabel doc==Nothing && dirty (documentBuffer doc)) (M.elems (buffers desktop))
  unless (not mutates || (not unnamedDirty && all (not . dirty . third) documents))
    (ioError (userError "Save or discard unsaved buffers in this repository before pulling or merging."))
  (code,out,err) <- runGit root args
  conflicts <- if mutates && not privateOutput then checkedGit root ["diff","--name-only","--diff-filter=U","--"] else pure ""
  refreshed <- if not mutates then pure [] else forM documents $ \(bid,file,_) -> do
    inspected <- try $ do
      exists <- doesFileExist (filePath file)
      if not exists then pure (Left "File was removed on disk; its buffer was preserved.")
        else either (Left . T.pack) Right <$> loadFile (filePath file)
    let loaded=either (const (Left "Could not reload the changed file; its buffer was preserved.")) id (inspected :: Either IOException (Either T.Text (FileState,Buffer)))
    pure (bid,file,loaded)
  let outcome = if code==ExitSuccess then " completed.\n" else " failed ("<>T.pack (show code)<>").\n"
      conflictText = if T.null conflicts then "" else "\nMerge conflicts remain in these files; resolve them before committing:\n"<>conflicts
  pure (Finished code root (label action<>outcome<>(if privateOutput then "" else out<>err<>conflictText)) refreshed (object []))
  where third (_,_,b)=b

observedHead :: FilePath -> IO (Maybe T.Text)
observedHead root=do
  result<-try (runGit root ["rev-parse","--verify","HEAD"]) :: IO (Either IOException (ExitCode,T.Text,T.Text))
  pure $ case result of
    Right (ExitSuccess,text,_) | let value=T.strip text,T.length value `elem` [40,64],T.all isHexDigit value -> Just value
    _ -> Nothing

integrationFacts :: FilePath -> T.Text -> Maybe ExitCode -> IO Value
integrationFacts root phase fetched=do
  headId<-observedHead root
  conflicts<-try (checkedGit root ["diff","--name-only","--diff-filter=U","-z","--"]) :: IO (Either IOException T.Text)
  pure (object ["phase" .= phase,"head" .= headId,"fetchExitCode" .= fmap exitNumber fetched,
    "conflicts" .= either (const Nothing) (Just . length . filter (not . T.null) . T.splitOn "\NUL") conflicts])
  where exitNumber ExitSuccess=0::Int; exitNumber (ExitFailure n)=n

integrationFailure :: FilePath -> T.Text -> Maybe ExitCode -> Maybe ExitCode -> IO Result
integrationFailure root phase fetched code=do
  facts<-integrationFacts root phase fetched
  pure (IntegrationFailed root code (extend facts (object ["error" .= ("Integration refused or failed; check clean saved files, configured upstream, named branch, and protected incoming changes. No automatic rollback was performed."::T.Text)])))

-- Fetch never changes working files. Return to the desktop lock afterward so
-- mutation uses current buffers and authority policy, not the pre-fetch copy.
prepareIntegration :: FilePath -> GitAction -> IO Result
prepareIntegration root action=do
  branch<-T.strip <$> checkedGit root ["rev-parse","--symbolic-full-name","HEAD"]
  headId<-observedHead root
  case headId of
    Nothing->integrationFailure root "preflight" Nothing Nothing
    Just expected->do
      clean<-checkIntegrationState root expected
      case clean of
        Left _->integrationFailure root "preflight" Nothing Nothing
        Right ()->do
          fetched<-if action==PullRemote then Just . (\(code,_,_)->code) <$> runGit root ["fetch"] else pure Nothing
          if maybe False (/=ExitSuccess) fetched then integrationFailure root "fetch" fetched fetched else do
            resolved<-try $ case action of
              PullRemote->T.strip <$> checkedGit root ["rev-parse","--verify","@{upstream}^{commit}"]
              MergeBranch wanted->do
                listed<-checkedGit root ["for-each-ref","--format=%(refname)%09%(objectname)%09%(symref)","refs/heads","refs/remotes"]
                let matches=[sha | line<-T.lines listed,[ref,sha,symbolic]<-[T.splitOn "\t" line],T.null symbolic,
                      wanted `elem` ([ref]++[T.drop 11 ref | "refs/heads/" `T.isPrefixOf` ref])++[T.drop 13 ref | "refs/remotes/" `T.isPrefixOf` ref]]
                case matches of [sha]->T.strip <$> checkedGit root ["rev-parse","--verify",T.unpack sha<>"^{commit}"]; _->ioError (userError "Unknown or ambiguous merge branch")
              _->ioError (userError "Unsupported integration")
            case (resolved :: Either IOException T.Text) of
              Left _->integrationFailure root "preflight" fetched Nothing
              Right target->pure (Prepared root action expected branch target fetched)

integrate :: FilePath -> GitAction -> T.Text -> T.Text -> T.Text -> Maybe ExitCode -> Desktop -> IO Result
integrate root action expected branch target fetched desktop=do
  currentBranch<-T.strip <$> checkedGit root ["rev-parse","--symbolic-full-name","HEAD"]
  safe<-if currentBranch/=branch then pure (Left "Selected branch changed.") else checkIncomingChanges root expected target (protectedPathParent desktop)
  case safe of
    Left _->integrationFailure root "preflight" fetched Nothing
    Right ()->do
      result<-runOperationArgs True root action (["-c","merge.directoryRenames=false","merge","--no-edit","--no-autostash","--no-overwrite-ignore"]++["--ff-only" | action==PullRemote]++["--",T.unpack target]) desktop
      facts<-integrationFacts root "merge" fetched
      pure $ case result of Finished code repo text reloads _->Finished code repo text reloads facts; other->other

readBranches :: FilePath -> IO Result
readBranches root = do
  current <- T.strip <$> checkedGit root ["branch","--show-current"]
  refs <- checkedGit root ["for-each-ref","--format=%(refname:short)%09%(symref)","refs/heads","refs/remotes"]
  pure (Branches root [name | line<-T.lines refs, let (name,symbolic)=T.breakOn "\t" line, T.null (T.drop 1 symbolic), name/=current, not (T.null name)])

-- | Apply Git serialization/interlocks and delegate other effects.
gitOperationEffects :: GitOperations -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
gitOperationEffects runtime@(GitOperations ref _ _ _ terminalLaunch) core = foldM apply . (False,)
  where
    apply state@(True,_) _ = pure state
    apply (_,desktop) effect = do
      worker <- readIORef ref
      launching<-terminalLaunch
      let busy=maybe False (const True) worker
          mutating=case worker of Just (Worker changes _ _ _) -> changes; Nothing -> False
      case effect of
        Exit | busy -> pure (False,desktop {status="Wait for the Git operation to finish before quitting."})
        SaveDocument{} | mutating -> pure (False,desktop {status="Wait for the Git operation to finish before saving."})
        AgentAction action _ | mutating, action `elem` ["run","compile","make"] || "approval:" `T.isPrefixOf` action ->
          pure (False,desktop {status="Wait for the Git operation to finish before approving agent actions or running commands."})
        AdoptPreparedDebug _ | mutating -> pure (False,desktop {status="Wait for the Git operation to finish before debugging."})
        AdoptPreparedBuild _ | mutating -> pure (False,desktop {status="Wait for the Git operation to finish before running commands."})
        RunGit action | launching, action/=FetchRemote -> pure (False,desktop {status="Wait for terminal launch to finish before changing the repository."})
        WriteGitCommit{} | launching -> pure (False,desktop {status="Wait for terminal launch to finish before changing the repository."})
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

-- | Adopt completed Git results while preserving intervening edits.
tickGitOperations :: GitOperations -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO Desktop
tickGitOperations (GitOperations ref focused jobs reviews terminalLaunch) core initial = do
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
        Just (Right (Prepared root action expected branch target fetched)) -> do
          selected<-try (selectedRepository desktop) :: IO (Either IOException FilePath)
          let next=if unsaved desktop || either (const True) (/=root) selected
                then integrationFailure root "preflight" fetched Nothing
                else integrate root action expected branch target fetched desktop
          startWorker (GitOperations ref focused jobs reviews terminalLaunch) True jobId next
          pure desktop {status="Checking Git integration…"}
        Just finished -> do
          writeIORef ref Nothing
          let usable=case finished of
                Right (Reviewed token paths outcome)
                  | paths/=guestPrivatePaths desktop || unsaved desktop -> Right (Reviewed token paths (Left "Review context changed."))
                  | otherwise -> Right (Reviewed token paths outcome)
                _ -> finished
          forM_ jobId $ \ident -> modifyIORef' jobs $ \(latest,history) ->
            (latest,M.insert ident (extend (M.findWithDefault Null ident history) (completion ident usable)) history)
          case usable of
            Right (Reviewed token _ (Right review)) -> do
              writeIORef reviews (if reviewText review=="No changes.\n" then Nothing else Just (token,review))
              pure desktop {status="Git review completed; read git_operation_status."}
            Right (Reviewed _ _ (Left _)) -> pure desktop {status="Git review refused; see git_operation_status."}
            Right (Committed root outcome _) -> do
              (_,updated)<-core desktop {gitReview=Nothing} [RefreshGit root]
              pure updated {status=case outcome of Right commit | commitExitCode commit==ExitSuccess -> "Git commit completed; see git_operation_status."; _ -> "Git commit failed; see git_operation_status."}
            Left _ | Just _<-jobId -> pure desktop {status="Git operation failed; see git_operation_status.",gitReview=Nothing}
            Left err -> pure ((addReadOnly "Git operations" (T.pack (displayException err)) desktop) {status="Git operation failed; see Git operations.",gitReview=Nothing})
            Right (Branches root _) | branchRoot desktop/=Just root -> pure desktop {status="Repository changed; request the merge branches again."}
            Right (Branches _ []) -> pure desktop {status="No other branches are available to merge."}
            Right (Branches _ branches) -> pure (prompt "Merge branch" (Merging branches) [ListBox "Branch" branches 0] desktop)
            Right (IntegrationFailed _ _ _) -> pure desktop {status="Git integration failed; see git_operation_status.",gitReview=Nothing}
            Right Prepared{} -> pure desktop
            Right (Finished _ root logText reloads _) -> do
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
                    adjust w=if bufferId w==Just (bid) then w {selection=let Selection a c=selection w in Selection (clamp a) (clamp c)} else w
                in (ensureVisible (normalizeDocumentViews bid d {buffers=M.insert bid (restyle doc {documentFile=Just file,documentBuffer=fresh}) (buffers d),windows=map adjust (windows d)}),notes)
      _ -> state

-- Both entry points run under the desktop lock and reserve the same worker slot.
-- Mask registration so session shutdown always owns the started subprocess.
startWorker :: GitOperations -> Bool -> Maybe Integer -> IO Result -> IO ()
startWorker (GitOperations ref _ _ _ _) mutates ident action=mask_ $ do
  done<-newEmptyMVar
  thread<-forkIOWithUnmask $ \unmask -> try (unmask action) >>= putMVar done
  writeIORef ref (Just (Worker mutates ident thread done))

gitToolNames :: [T.Text]
gitToolNames=["git_fetch","git_pull","git_merge","git_review","git_commit","git_operation_status"]

gitTools :: [Value]
gitTools=
  [spec "git_fetch" "Fetch the selected repository's configured default remote. Returns accepted and jobId, not completion. Poll git_operation_status. Does not change working files or expose transport output." False [] [],
   spec "git_pull" "Fetch configured upstream, inspect incoming changes, then fast-forward only. Requires clean saved buffers/worktree. Returns acceptance; poll actual exit, HEAD and conflicts. Protected changes, changed symlinks and submodules are refused. Ordinary Git hooks/drivers execute." False [] [],
   spec "git_merge" "Merge one named local or remote-tracking branch after checking incoming changes and clean saved files. No raw revision syntax, URL or options. Conflicts remain for resolution; no automatic abort. Poll actual exit, HEAD and conflict count." False ["ref"] [("ref",string 256)],
   spec "git_review" "Review all saved staged, unstaged and untracked changes. Poll git_operation_status for the complete diff and one-time reviewId. Protected or incomplete reviews cannot be committed. Requires no dirty buffers." True [] [],
   spec "git_commit" "Commit ALL saved changes from a complete git_review using ordinary Git staging and hooks. Requires unchanged HEAD, index and files, and no dirty buffers. Consumes reviewId; poll git_operation_status for actual HEAD, exit code and reviewedTreeMatched. Failure may leave reviewed changes staged." False ["reviewId","message"] [("reviewId",string 128),("message",string 8192)],
   spec "git_operation_status" "Read a Git job's completion, review, commit or integration result. Integrations report phase, HEAD, fetchExitCode and conflict count. Fetch/commit/merge report actual process exit codes; review exitCode=0 denotes a completed composite review. Omit jobId for the latest. Retains the latest 16 agent jobs; busy includes human Git operations. Only the latest complete review ID can be committed." True [] [("jobId",object ["type" .= ("integer"::T.Text),"minimum" .= (1::Int)])]]
  where
    string :: Int -> Value
    string limit=object ["type" .= ("string"::T.Text),"minLength" .= (1::Int),"maxLength" .= limit]
    spec :: T.Text -> T.Text -> Bool -> [T.Text] -> [Pair] -> Value
    spec name description readOnly required props=object ["name" .= name,"description" .= description,
      "inputSchema" .= object ["type" .= ("object"::T.Text),"properties" .= object props,"required" .= required,"additionalProperties" .= False],
      "annotations" .= object ["readOnlyHint" .= readOnly,"destructiveHint" .= (name `elem` ["git_commit","git_pull","git_merge"]),"openWorldHint" .= not readOnly]]

jobValue :: Integer -> Bool -> Maybe ExitCode -> Value
jobValue ident active code=object ["jobId" .= ident,"active" .= active,
  "state" .= (if active then "running" else case code of Just ExitSuccess -> "succeeded"; _ -> "failed" :: T.Text),
  "exitCode" .= fmap (\value -> case value of ExitSuccess -> 0; ExitFailure n -> n) code]

extend :: Value -> Value -> Value
extend (Object old) (Object new)=Object (KM.union new old)
extend _ new=new

completion :: Integer -> Either SomeException Result -> Value
completion ident result=case result of
  Right (Finished code _ _ _ facts) -> extend (jobValue ident False (Just code)) facts
  Right (IntegrationFailed _ code facts) -> extend (jobValue ident False code) facts
  Right (Reviewed token _ (Right review)) -> extend (jobValue ident False (Just ExitSuccess)) (object
    ["complete" .= True,"diff" .= reviewText review,"reviewId" .= (if reviewText review=="No changes.\n" then Nothing else Just token)])
  Right (Reviewed _ _ (Left _)) -> extend failed (object ["complete" .= False,"reviewId" .= Null,
    "error" .= ("Complete review unavailable: check for protected changes, unsaved buffers, conflicts, stale files, or the review size limit."::T.Text)])
  Right (Committed _ outcome headId) -> extend (jobValue ident False (either (const Nothing) (Just . commitExitCode) outcome)) (object
    ["head" .= headId,"reviewedTreeMatched" .= either (const Nothing) commitTreeMatched outcome,
     "message" .= (case outcome of
       Right commit | commitExitCode commit==ExitSuccess -> if commitTreeMatched commit==Just True then "Committed the reviewed tree." else "Committed; Git hooks changed the reviewed tree or it could not be verified. Inspect HEAD."
       _ -> "Commit refused or failed. Review again; changes may remain staged." :: T.Text)])
  _ -> extend failed (object ["error" .= ("Git operation failed; raw process output is private."::T.Text)])
  where failed=jobValue ident False Nothing

unsaved :: Desktop -> Bool
unsaved=any (dirty . documentBuffer) . M.elems . buffers

-- | Accept an asynchronous Git job or inspect status/review. A returned job ID
-- means acceptance, not successful completion; review IDs are consumed once.
gitTool :: GitOperations -> Desktop -> T.Text -> Value -> IO (Desktop,IO (Either T.Text Value))
gitTool runtime@(GitOperations ref _ jobs reviews terminalLaunch) desktop name args=case parseEither (withObject "arguments" pure) args of
  Left _ -> reply desktop (Left "Expected an arguments object.")
  Right fields | any (`notElem` allowed) (KM.keys fields) -> reply desktop (Left "Unexpected Git tool argument.")
  Right fields -> case name of
    "git_fetch" -> start False (pure ()) $ do
      root<-selectedRoot
      runOperation True root FetchRemote desktop
    "git_pull" | unsaved desktop -> reply desktop (Left "Save changed buffers before pulling or merging.")
               | otherwise -> start True (writeIORef reviews Nothing) (selectedRoot >>= \root->prepareIntegration root PullRemote)
    "git_merge" -> case parseEither (\_ -> fields .: "ref") args of
      Left _->reply desktop (Left "ref must name a local or remote-tracking branch.")
      Right refName | T.null refName || T.length refName>256 || T.isPrefixOf "-" refName || T.any (<' ') refName -> reply desktop (Left "Invalid branch name.")
                    | unsaved desktop -> reply desktop (Left "Save changed buffers before pulling or merging.")
                    | otherwise -> start True (writeIORef reviews Nothing) (selectedRoot >>= \root->prepareIntegration root (MergeBranch refName))
    "git_review" | unsaved desktop -> reply desktop (Left "Save changed buffers before reviewing a commit.")
                 | otherwise -> start False (writeIORef reviews Nothing) $ do
                     root<-selectedRoot
                     token<-T.pack <$> randomIdentity
                     Reviewed token (guestPrivatePaths desktop) <$> reviewRepositoryChecked root (protectedPathParent desktop)
    "git_commit" -> case parseEither (\_ -> (,) <$> fields .: "reviewId" <*> fields .: "message") args of
      Left _ -> reply desktop (Left "Expected reviewId and message strings.")
      Right (token,commitMessage)
        | T.null token || T.length token>128 || T.null (T.strip commitMessage) || T.length commitMessage>8192 || T.any (=='\0') commitMessage -> reply desktop (Left "Use a valid reviewId and a nonempty commit message of at most 8192 characters.")
        | unsaved desktop -> reply desktop (Left "Save changed buffers before approving a commit.")
        | otherwise -> do
            reviewed<-readIORef reviews
            case reviewed of
              Just (expected,review) | token==expected -> start True (writeIORef reviews Nothing) $ do
                outcome<-commitReviewChecked review commitMessage (protectedPathParent desktop)
                headResult<-try (runGit (reviewRoot review) ["rev-parse","--verify","HEAD"]) :: IO (Either IOException (ExitCode,T.Text,T.Text))
                let headId=case headResult of
                      Right (ExitSuccess,text,_) | let value=T.strip text,T.length value `elem` [40,64],T.all isHexDigit value -> Just value
                      _ -> Nothing
                pure (Committed (reviewRoot review) outcome headId)
              _ -> reply desktop (Left "Review ID is unknown, replaced or already consumed; request git_review.")
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
    allowed=case name of "git_operation_status" -> ["jobId"]; "git_commit" -> ["reviewId","message"]; "git_merge" -> ["ref"]; _ -> []
    reply updated result=pure (updated,pure result)
    selectedRoot=selectedRepository desktop
    start mutates prepare action=do
      worker<-readIORef ref
      launching<-terminalLaunch
      case worker of
        _ | mutates && launching -> reply desktop (Left "Wait for terminal launch to finish before changing the repository.")
        Just _ -> reply desktop (Left "A Git operation is already running.")
        Nothing -> mask_ $ do
          (previous,history)<-readIORef jobs
          let ident=previous+1
              kept=M.filterWithKey (\key _ -> key>ident-16) history
          _<-prepare
          startWorker runtime mutates (Just ident) action
          writeIORef jobs (ident,M.insert ident (extend (jobValue ident True Nothing) (object ["kind" .= name])) kept)
          reply desktop {status="Git operation started…",gitReview=Nothing} (Right (object ["accepted" .= True,"jobId" .= ident]))

selectedRepository :: Desktop -> IO FilePath
selectedRepository desktop=checkedGit (maybe (startingDirectory desktop) id (branchRoot desktop)) ["rev-parse","--show-toplevel"] >>= canonicalizePath . T.unpack . T.stripEnd
