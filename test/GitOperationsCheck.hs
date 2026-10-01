{-# LANGUAGE OverloadedStrings #-}
module GitOperationsCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless, void)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Exit (ExitCode(..))
import System.Environment (lookupEnv,setEnv,unsetEnv)
import System.Info (os)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)
import qualified THC.Edit.App as App
import THC.Edit.Buffer
import THC.Edit.Files
import THC.Edit.Git
import THC.Edit.GitOperations
import THC.Edit.Model

checks :: IO ()
checks = bracket temporary removePathForcibly $ \base -> do
  let upstream=base </> "upstream"
      work=base </> "work"
      source=work </> "Main.hs"
      upstreamSource=upstream </> "Main.hs"
      git dir args=do
        (code,out,err)<-readCreateProcessWithExitCode (proc "git" (["-C",dir,"-c","user.name=Git Test","-c","user.email=test@example.invalid","-c","commit.gpgsign=false"]++args)) ""
        unless (code==ExitSuccess) (error err)
        pure out
      commit dir name=void (git dir ["add","-A"]) >> void (git dir ["commit","-m",name])
      open desktop=loadFile source >>= either error (\(file,b)->pure (addDocument (Just file) b desktop))
      doc desktop=case [d | d<-M.elems (buffers desktop),fmap filePath (documentFile d)==Just source] of d:_ -> d; [] -> error "missing source buffer"
      select desktop=case [windowId w | w<-windows desktop,Just d<-[M.lookup (bufferId w) (buffers desktop)],fmap filePath (documentFile d)==Just source] of i:_ -> focusWindow i desktop; [] -> error "missing source window"
  createDirectory upstream
  void (git upstream ["init","-b","main"])
  T.writeFile upstreamSource "original\n"
  commit upstream "initial"
  void (git base ["clone",upstream,work])
  initial<-open (initialDesktop (100,30))
  (_,configured)<-core initial [RefreshGit work]
  withGitOperations $ \runtime -> do
    let effects=gitOperationEffects runtime core
        tick=tickGitOperations runtime core
        run action desktop=effects desktop [RunGit action] >>= await tick (\d -> " completed." `T.isInfixOf` status d || " failed" `T.isInfixOf` status d) . snd
    fetched<-run FetchRemote configured
    check "fetch reports success" (status fetched=="Fetch completed.")
    let call d name args=do
          (updated,pending)<-gitTool runtime d name args
          result<-pending
          pure (updated,result)
        stateOf value=parseMaybe (withObject "status" (\o -> o .: "operation" >>= withObject "job" (.: "state"))) value :: Maybe T.Text
        codeOf value=parseMaybe (withObject "status" (\o -> o .: "operation" >>= withObject "job" (.: "exitCode"))) value :: Maybe Int
        identOf value=parseMaybe (withObject "job" (.: "jobId")) value :: Maybe Integer
    (_,emptyStatus)<-call fetched "git_operation_status" (object [])
    check "no invented job before fetch" (either (const False) ((==Just Null) . parseMaybe (withObject "status" (.: "operation"))) emptyStatus)
    (accepted,receipt)<-call fetched "git_fetch" (object [])
    check "fetch returns acceptance before completion" (either (const False) ((==Just True) . parseMaybe (withObject "receipt" (.: "accepted"))) receipt)
    let firstId=either (const Nothing) identOf receipt
    (_,runningStatus)<-call accepted "git_operation_status" (object ["jobId" .= firstId])
    check "accepted job starts running" (either (const False) ((==Just "running") . stateOf) runningStatus)
    completed<-await tick (T.isInfixOf "completed." . status) accepted
    (_,completedStatus)<-call completed "git_operation_status" (object ["jobId" .= firstId])
    check "fetch completion reports actual zero exit" (either (const False) (\v -> stateOf v==Just "succeeded" && codeOf v==Just 0) completedStatus)
    (_,badArgs)<-call completed "git_fetch" (object ["remote" .= ("https://private.invalid/secret"::T.Text)])
    check "fetch refuses arbitrary remote arguments" (either (const True) (const False) badArgs)
    (_,badId)<-call completed "git_operation_status" (object ["jobId" .= ("1"::T.Text)])
    check "status rejects mistyped job identifier" (either (const True) (const False) badId)
    let secret="credential-secret-marker"
    void (git work ["config","remote.origin.url",base </> secret])
    (failedStart,_)<-call completed "git_fetch" (object [])
    failed<-await tick (T.isInfixOf "failed" . status) failedStart
    (_,failedStatus)<-call failed "git_operation_status" (object [])
    check "fetch failure retains actual nonzero exit" (either (const False) (\v -> stateOf v==Just "failed" && maybe False (/=0) (codeOf v)) failedStatus)
    check "fetch output cannot leak through status or editor buffers"
      (not (T.pack secret `T.isInfixOf` T.pack (BL.unpack (encode failedStatus))) && all (not . T.isInfixOf (T.pack secret) . contents . documentBuffer) (M.elems (buffers failed)))
    (_,retainedStatus)<-call failed "git_operation_status" (object ["jobId" .= firstId])
    check "earlier completion remains queryable" (either (const False) ((==Just "succeeded") . stateOf) retainedStatus)
    void (git work ["config","remote.origin.url",upstream])
    T.writeFile upstreamSource "pulled\n"
    commit upstream "upstream change"
    let dirtyDesktop=insertText "unsaved " (select fetched)
    refused<-run PullRemote dirtyDesktop
    check "pull refuses existing dirty buffers" ("Save or discard" `T.isInfixOf` activeText refused && contents (documentBuffer (doc refused))=="unsaved original\n")
    diskBefore<-T.readFile source
    check "refused pull does not change disk" (diskBefore=="original\n")
    cleanPull<-run PullRemote (fst (runCommand Undo (select refused)))
    let reloaded=documentBuffer (doc cleanPull)
    check "pull reloads clean buffer with increasing revision" (contents reloaded=="pulled\n" && not (dirty reloaded) && revision reloaded>0 && contents (undo reloaded)=="original\n")
    -- Pause the local upload-pack so an edit deterministically occurs during pull.
    let upload=base </> "upload-pack"
        marker=base </> "fetch-started"
        gate=base </> "fetch-continue"
    writeFile upload (unlines ["#!/bin/sh","touch '"++marker++"'","n=0","while [ ! -e '"++gate++"' ] && [ $n -lt 200 ]; do sleep 0.01; n=$((n+1)); done","exec git-upload-pack \"$@\""])
    permissions<-getPermissions upload
    setPermissions upload (permissions {executable=True})
    void (git work ["config","remote.origin.uploadpack",upload])
    T.writeFile upstreamSource "changed during operation\n"
    commit upstream "another change"
    (_,running)<-effects (select cleanPull) [RunGit PullRemote]
    started<-timeout 5000000 (waitFile marker)
    check "local Git operation runs asynchronously" (started==Just ())
    (_,busyTool)<-call running "git_fetch" (object [])
    check "agent fetch shares human operation serialization" (either (T.isInfixOf "already running") (const False) busyTool)
    (_,humanBusy)<-call running "git_operation_status" (object [])
    check "status reports human operation busy" (either (const False) ((==Just True) . parseMaybe (withObject "status" (.: "busy"))) humanBusy)
    (exited,waiting)<-effects running [Exit]
    check "quit waits for active operation" (not exited && "Wait for" `T.isPrefixOf` status waiting)
    (_,blockedApproval)<-effects waiting [AgentAction "approval:1" ["0"]]
    check "mutating Git blocks agent approvals before they reach the writer" ("Wait for" `T.isPrefixOf` status blockedApproval)
    (_,blockedRun)<-effects waiting [AgentAction "run" []]
    check "mutating Git blocks starting a terminal run" ("Wait for" `T.isPrefixOf` status blockedRun)
    (_,cancelledAgent)<-effects waiting [AgentAction "cancel" []]
    check "mutating Git still delegates agent cancellation" (status cancelledAgent=="agent action delegated")
    (_,duplicate)<-effects waiting [RunGit FetchRemote]
    check "concurrent Git operation refused" (status duplicate=="A Git operation is already running.")
    let edited=insertText "keep " (select duplicate)
    writeFile gate "continue"
    preserved<-await tick (T.isInfixOf "completed." . status) edited
    let retained=doc preserved
    diskAfter<-T.readFile source
    check "edits during pull survive disk updates" (contents (documentBuffer retained)=="keep pulled\n" && dirty (documentBuffer retained) && diskAfter=="changed during operation\n")
    staleSave<-saveFile (maybe (error "no file") id (documentFile retained)) (documentBuffer retained)
    check "preserved buffer retains disk conflict protection" (either (const True) (const False) staleSave)
    removeFile marker
    removeFile gate
    (_,fetching)<-effects preserved [RunGit FetchRemote]
    _<-timeout 5000000 (waitFile marker)
    (_,savedDuringFetch)<-effects fetching [SaveDocument 0 Nothing Nothing]
    check "fetch permits ordinary save effects" (status savedDuringFetch=="save delegated")
    (_,approvedDuringFetch)<-effects fetching [AgentAction "approval:2" ["0"]]
    check "read-only fetch permits agent approvals" (status approvedDuringFetch=="agent action delegated")
    let savePath=work </> "save-then-quit.txt"
    T.writeFile savePath "before\n"
    (saveFileState,saveBuffer)<-loadFile savePath >>= either error pure
    let saveDesktop=addDocument (Just saveFileState) (replaceSelection (Selection 0 (bufferLength saveBuffer)) "saved\n" saveBuffer) (initialDesktop (100,30))
        saveBid=maybe (error "missing save window") bufferId (activeWindow saveDesktop)
    (quitAfterSave,afterSave)<-gitOperationEffects runtime App.applyEffects saveDesktop [SaveDocument saveBid Nothing (Just Quit)]
    savedText<-T.readFile savePath
    check "real save-and-quit continuation cannot exit during fetch" (not quitAfterSave && savedText=="saved\n" && "Wait for" `T.isPrefixOf` status afterSave)
    (otherFile,otherBuffer)<-loadFile upstreamSource >>= either error pure
    switched<-tick (addDocument (Just otherFile) otherBuffer (initialDesktop (100,30)))
    check "focused repository changes during fetch" (branchRoot switched==Just upstream)
    writeFile gate "continue"
    logged<-await tick (T.isInfixOf "completed." . status) switched
    check "operation log identifies completed repository" (branchRoot logged==Just work)
    returned<-tick (closeActive logged)
    check "closing operation log restores focused repository badge" (branchRoot returned==Just upstream)
    removeFile marker
    removeFile gate
    (agentRunning,_)<-call returned {branchRoot=Just work} "git_fetch" (object [])
    agentStarted<-timeout 5000000 (waitFile marker)
    check "agent fetch uses configured transport" (agentStarted==Just ())
    (_,busyHuman)<-effects agentRunning [RunGit FetchRemote]
    check "human action shares agent operation serialization" (status busyHuman=="A Git operation is already running.")
    writeFile gate "continue"
    _<-await tick (T.isInfixOf "completed." . status) busyHuman
    void (git work ["config","--unset","remote.origin.uploadpack"])
    void (git work ["checkout","-b","topic"])
    T.writeFile source "topic change\n"
    commit work "topic change"
    void (git work ["checkout","main"])
    T.writeFile source "main change\n"
    commit work "main change"
    conflictBase<-open (initialDesktop (100,30))
    (_,conflictRoot)<-core conflictBase [RefreshGit work]
    (_,staleReading)<-effects conflictRoot [ReadMergeBranches]
    staleBranches<-await tick (T.isPrefixOf "Repository changed" . status) (initialDesktop (100,30)) {branchRoot=Just upstream,status=status staleReading}
    check "branch picker result cannot target a different repository" (dialog staleBranches==Nothing)
    (_,reading)<-effects conflictRoot [ReadMergeBranches]
    choices<-await tick (\d -> case purpose <$> dialog d of Just Merging{} -> True; _ -> False) reading
    check "merge lists another branch" (case purpose <$> dialog choices of Just (Merging branches) -> "topic" `elem` branches && "main" `notElem` branches; _ -> False)
    conflict<-run (MergeBranch "topic") choices {dialog=Nothing}
    check "conflicting merge is reported and retained" ("Merge conflicts remain" `T.isInfixOf` activeText conflict && "<<<<<<<" `T.isInfixOf` contents (documentBuffer (doc conflict)))
    merging<-doesFileExist (work </> ".git" </> "MERGE_HEAD")
    check "conflicting merge remains available for resolution" merging
    void (git work ["reset","--hard","HEAD"])
    writeFile (upstream </> "remote.txt") "remote commit\n"
    commit upstream "remote diverged"
    diverged<-run PullRemote (select conflict)
    check "pull refuses divergence instead of creating a merge" ("failed" `T.isInfixOf` status diverged)
    noMerge<-not <$> doesFileExist (work </> ".git" </> "MERGE_HEAD")
    check "fast-forward-only pull leaves no merge state" noMerge
    void (git work ["reset","--hard","origin/main"])
    deleteBase<-open (initialDesktop (100,30))
    (_,deleteRoot)<-core deleteBase [RefreshGit work]
    removeFile upstreamSource
    commit upstream "delete source"
    deleted<-run PullRemote deleteRoot
    check "pull deletion preserves the previous buffer" (contents (documentBuffer (doc deleted))=="changed during operation\n" && "File was removed" `T.isInfixOf` activeText deleted)
    missing<-not <$> doesFileExist source
    check "deleted source stays deleted on disk" missing
    let target=base </> "symlink-target.hs"
    T.writeFile target "outside symlink target\n"
    createFileLink target upstreamSource
    commit upstream "replace source with symbolic link"
    linked<-run PullRemote (select deleted)
    check "pull symlink replacement preserves original path and buffer"
      (contents (documentBuffer (doc linked))=="changed during operation\n" &&
       documentFile (doc linked)==documentFile (doc deleted) && "reopen" `T.isInfixOf` activeText linked)
    targetBytes<-T.readFile target
    check "pull symlink target remains unchanged" (targetBytes=="outside symlink target\n")
  reviewCommitChecks base
  unless (os=="mingw32") (cancellationCheck base work)
  putStrLn "Git operation checks passed"
  where
    check label ok=unless ok (error label)
    waitFile path=doesFileExist path >>= \exists -> unless exists (threadDelay 10000 >> waitFile path)
    temporary=do
      base<-getTemporaryDirectory
      (path,file)<-openTempFile base "thc-git-operations"
      hClose file
      removeFile path
      createDirectory path
      canonicalizePath path

core :: Desktop -> [Effect] -> IO (Bool,Desktop)
core desktop [RefreshGit path]=do
  repo<-repositoryStatus path
  pure (False,desktop {branchRoot=repoRoot <$> repo,branchStatus=maybe "" repoBranch repo})
core desktop [Exit]=pure (True,desktop)
core desktop [SaveDocument{}]=pure (False,desktop {status="save delegated"})
core desktop [AgentAction{}]=pure (False,desktop {status="agent action delegated"})
core desktop _=pure (False,desktop)

await :: (Desktop -> IO Desktop) -> (Desktop -> Bool) -> Desktop -> IO Desktop
await tick ready initial=do
  result<-timeout 10000000 (loop initial)
  maybe (error ("Git operation timed out: "++T.unpack (status initial))) pure result
  where loop desktop=do
          updated<-tick desktop
          if ready updated then pure updated else threadDelay 10000 >> loop updated

-- The wrapper becomes the owned Git process, rather than leaving a sleeping
-- grandchild behind. Closing the controller must terminate and reap it.
cancellationCheck :: FilePath -> FilePath -> IO ()
cancellationCheck base work=do
  realGit<-findExecutable "git" >>= maybe (error "git unavailable") pure
  let directory=base </> "cancellation-bin"
      wrapper=directory </> "git"
      marker=base </> "cancelled-git.pid"
      quote value="'"++concatMap (\c -> if c=='\'' then "'\\''" else [c]) value++"'"
  createDirectory directory
  writeFile wrapper (unlines ["#!/bin/sh","for arg do", "if [ \"$arg\" = fetch ]; then", "echo $$ > "++quote marker,"exec /bin/sleep 60","fi","done","exec "++quote realGit++" \"$@\""])
  permissions<-getPermissions wrapper
  setPermissions wrapper (permissions {executable=True})
  bracket (lookupEnv "PATH") (maybe (unsetEnv "PATH") (setEnv "PATH")) $ \oldPath -> do
    setEnv "PATH" (directory++maybe "" (":"++) oldPath)
    ended<-timeout 5000000 $ withGitOperations $ \runtime -> do
      (_,receipt)<-gitTool runtime (initialDesktop (80,25)) {defaultDirectory=Just work} "git_fetch" (object [])
      accepted<-receipt
      unless (either (const False) (const True) accepted) (error "cancellation fixture fetch rejected")
      let wait=doesFileExist marker >>= \exists -> unless exists (threadDelay 10000 >> wait)
      wait
    unless (ended==Just ()) (error "closing Git controller did not reap fetch promptly")
  pid<-T.unpack . T.strip <$> T.readFile marker
  (code,_,_)<-readCreateProcessWithExitCode (proc "/bin/kill" ["-0",pid]) ""
  unless (code/=ExitSuccess) (error "Git subprocess survived controller close")

reviewCommitChecks :: FilePath -> IO ()
reviewCommitChecks base=do
  let root=base </> "review-commit"
      source=root </> "safe.txt"
      private=root </> "thc.toml"
      hook=root </> ".git" </> "hooks" </> "pre-commit"
      git args=do
        (code,out,err)<-readCreateProcessWithExitCode (proc "git" (["-C",root]++args)) ""
        unless (code==ExitSuccess) (error err)
        pure (T.pack out)
      check label ok=unless ok (error label)
      field key value=parseMaybe (withObject "object" (.: key)) value
      textField key value=field key value :: Maybe T.Text
      left (Left _)=True
      left _=False
      script body=do
        writeFile hook ("#!/bin/sh\n"++body)
        permissions<-getPermissions hook
        setPermissions hook (permissions {executable=True})
  createDirectory root
  _<-git ["init","-b","main"]
  _<-git ["config","user.name","Git Test"]
  _<-git ["config","user.email","test@example.invalid"]
  _<-git ["config","commit.gpgsign","false"]
  _<-git ["config","core.hooksPath",root </> ".git" </> "hooks"]
  T.writeFile source "before\n"
  T.writeFile private "authority-secret-marker\n"
  _<-git ["add","-A"]
  _<-git ["commit","-m","initial"]
  withGitOperations $ \runtime -> do
    let initial=(initialDesktop (80,25)) {defaultDirectory=Just root,branchRoot=Just root}
        call d name args=do
          (next,pending)<-gitTool runtime d name args
          response<-pending
          pure (next,response)
        run d name args=do
          (next,accepted)<-call d name args
          receipt<-either (error . T.unpack) pure accepted
          check "Git request explicitly accepted" (field "accepted" receipt==Just True)
          waited<-timeout 10000000 (poll next)
          maybe (error "review/commit operation timed out") pure waited
        poll d=do
          next<-tickGitOperations runtime core d
          (_,response)<-call next "git_operation_status" (object [])
          value<-either (error . T.unpack) pure response
          case field "operation" value of
            Just operation | field "active" operation==Just False -> pure (next,operation)
            _ -> threadDelay 10000 >> poll next
        review d=do
          (next,result)<-run d "git_review" (object [])
          check "complete safe review succeeded" (textField "state" result==Just "succeeded" && field "complete" result==Just True && field "exitCode" result==Just (0::Int))
          ident<-maybe (error "missing opaque review id") pure (textField "reviewId" result)
          check "review id contains no raw snapshot" (T.length ident<=128 && not ("INDEX" `T.isInfixOf` ident) && not (T.any (=='\0') ident))
          pure (next,ident,result)
        commit d ident=run d "git_commit" (object ["reviewId" .= ident,"message" .= ("approved saved changes"::T.Text)])
        failedReview d=do
          (next,result)<-run d "git_review" (object [])
          check "incomplete private review cannot grant commit id" (textField "state" result==Just "failed" && field "complete" result==Just False && field "reviewId" result==Just Null)
          check "refused review never returns partial diff text" ((field "diff" result :: Maybe Value)==Nothing)
          check "private review failure does not expose contents" (not ("authority-secret-marker" `T.isInfixOf` T.pack (BL.unpack (encode result))))
          pure next
    T.writeFile source "working edit\n"
    (reviewed,token,description)<-review initial
    check "unchanged private configuration permits full public review"
      (maybe False (T.isInfixOf "+working edit") (textField "diff" description) && not ("authority-secret-marker" `T.isInfixOf` T.pack (BL.unpack (encode description))))
    let dirtyDesktop=insertText "unsaved" reviewed
    (_,dirtyRefused)<-call dirtyDesktop "git_commit" (object ["reviewId" .= token,"message" .= ("must refuse"::T.Text)])
    check "commit refuses unsaved buffers" (left dirtyRefused)
    (_,dirtyReview)<-call dirtyDesktop "git_review" (object [])
    check "review refuses unsaved buffers" (left dirtyReview)
    T.writeFile source "later edit\n"
    (stale,result)<-commit reviewed token
    check "changed file invalidates reviewed commit" (textField "state" result==Just "failed")
    (_,reused)<-call stale "git_commit" (object ["reviewId" .= token,"message" .= ("must refuse"::T.Text)])
    check "failed commit consumes review id" (left reused)
    (policyReview,policyToken,_)<-review stale
    (_,policyResult)<-commit policyReview {guestPrivatePaths=[source]} policyToken
    check "commit rechecks current private-path policy" (textField "state" policyResult==Just "failed")
    (again,stagingToken,_)<-review stale
    _<-git ["add","safe.txt"]
    (staged,staleIndex)<-commit again stagingToken
    check "changed index invalidates reviewed commit" (textField "state" staleIndex==Just "failed")
    (headReview,headToken,_)<-review staged
    _<-git ["commit","--allow-empty","-m","external commit"]
    (headChanged,staleHead)<-commit headReview headToken
    check "changed HEAD invalidates reviewed commit" (textField "state" staleHead==Just "failed")
    T.writeFile source "approved edit\n"
    T.writeFile (root </> "new.txt") "untracked addition\n"
    (ready,commitToken,_)<-review headChanged
    (committed,success)<-commit ready commitToken
    actualHead<-T.strip <$> git ["rev-parse","HEAD"]
    clean<-git ["status","--porcelain"]
    check "whole working-tree review commits tracked and untracked changes"
      (textField "state" success==Just "succeeded" && field "exitCode" success==Just (0::Int) && field "reviewedTreeMatched" success==Just True && textField "head" success==Just actualHead && T.null clean)
    createDirectory (root </> "nested")
    T.writeFile (root </> "nested" </> "thc.toml") "untracked authority-secret-marker\n"
    privateUntracked<-failedReview committed
    removeFile (root </> "nested" </> "thc.toml")
    removeDirectory (root </> "nested")
    T.writeFile (root </> "authority-parent") "authority-secret-marker\n"
    ancestor<-failedReview privateUntracked {guestPrivatePaths=[root </> "authority-parent" </> "session.key"]}
    removeFile (root </> "authority-parent")
    T.writeFile private "changed authority-secret-marker\n"
    privateChanged<-failedReview ancestor {guestPrivatePaths=[]}
    _<-git ["add","thc.toml"]
    T.writeFile private "authority-secret-marker\n"
    privateStaged<-failedReview privateChanged
    _<-git ["reset","--","thc.toml"]
    _<-git ["mv","thc.toml","apparently-public.txt"]
    renamed<-failedReview privateStaged
    _<-git ["reset","--hard","HEAD"]
    copyFile private (root </> "copied-public.txt")
    _<-git ["add","copied-public.txt"]
    copied<-failedReview renamed
    _<-git ["reset","--","copied-public.txt"]
    removeFile (root </> "copied-public.txt")
    createFileLink private (root </> "private-alias")
    linked<-failedReview copied
    removeFile (root </> "private-alias")
    T.writeFile source (T.replicate 131073 "x")
    oversized<-failedReview linked
    T.writeFile source "hook change\n"
    (beforeHook,hookToken,_)<-review oversized
    script "echo authority-secret-marker >&2\nexit 7\n"
    (hookFailed,hookFailure)<-commit beforeHook hookToken
    preserved<-T.readFile source
    stagedNames<-git ["diff","--cached","--name-only"]
    check "failed normal hook preserves bytes and staged changes"
      (textField "state" hookFailure==Just "failed" && maybe False (/=0) (field "exitCode" hookFailure :: Maybe Int) && textField "head" hookFailure==Just actualHead && preserved=="hook change\n" && "safe.txt" `T.isInfixOf` stagedNames)
    check "raw hook output is not returned or opened"
      (not ("authority-secret-marker" `T.isInfixOf` T.pack (BL.unpack (encode hookFailure))) && all (not . T.isInfixOf "authority-secret-marker" . contents . documentBuffer) (M.elems (buffers hookFailed)))
    script "printf 'hook output\\n' > hook-output.txt\ngit add -- hook-output.txt\n"
    (transformReview,transformToken,_)<-review hookFailed
    (_,transformed)<-commit transformReview transformToken
    check "ordinary transforming hook reports changed committed tree"
      (textField "state" transformed==Just "succeeded" && field "reviewedTreeMatched" transformed==Just False && maybe False (T.isInfixOf "hooks changed") (textField "message" transformed))
    removeFile hook
