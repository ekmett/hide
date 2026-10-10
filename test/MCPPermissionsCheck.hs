{-# LANGUAGE CPP, OverloadedStrings #-}
module MCPPermissionsCheck (checks,policyResponsivenessChecks,policyWakeChecks,settledTool,settleDialog) where

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM
import GHC.Conc (threadStatus,ThreadStatus(..),BlockReason(..))
import Control.Concurrent.Async (concurrently, withAsync, poll, wait, async, cancel)
import qualified Control.Concurrent.Async
import Control.Exception (bracket, onException, SomeAsyncException,fromException,throwIO)
import Control.Monad (foldM, unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import System.Directory
import System.FilePath ((</>))
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose,openTempFile)
#ifndef mingw32_HOST_OS
import Control.Concurrent.MVar
import Control.Exception (catch,IOException)
import System.Posix.Files (createNamedPipe)
import System.Posix.IO (openFd,closeFd,fdWrite,OpenMode(ReadWrite),OpenFileFlags(nonBlock),defaultFileFlags)
#endif
import System.Timeout (timeout)
import qualified Hide.Plugin.Buffer as P
import Hide.Plugin.BufferHost (readerReference)
import qualified Hide.BufferTools as BufferTools
import qualified Hide.Plugin.Tool as Tool
import TypedBufferDiffsCheck (startDiffCall)
import Hide.Buffer
import Hide.MCPPermissions
import Hide.Model
import Hide.Render (snapshot, snapshotHtml)
import Hide.GuestAccess (readableAt, guestKeyboardAllowed, guestEffectsAllowed)
import Hide.RuntimeMCP (runtimeTools)
import Hide.WorkspaceFilesMCP (fileTools)

checks :: IO ()
checks=do
  policyResponsivenessChecks
  policyWakeChecks
  reviewChecks
  configChecks
  keybindingConfigChecks
  projectConfigChecks
  agentLimitChecks
  bracket temporary removePathForcibly $ \directory -> do
    let path=directory </> "config.toml"
        specs=[object ["name" .= name,"annotations" .= object ["readOnlyHint" .= readonly]] | (name,readonly)<-[("mutate"::T.Text,False),("read",True)]]
        core desktop _=pure (False,desktop)
        base=addDocument Nothing (newBuffer "before") (initialDesktop (80,25))
        bid=fromMaybe (error "missing initial buffer") (sourceFixtureBuffer <$> activeWindow base)
        changed=base {buffers=M.adjust (\doc->doc {documentBuffer=replaceBuffer False "current" (documentBuffer doc)}) bid (buffers base)}
        submit runtime button desktop=case dialog desktop of
          Just dg -> let (next,effects)=submitDialog button dg desktop in snd <$> policyEffects runtime core next effects >>= settleDialog runtime dg
          _ -> error ("missing permission dialog: "++T.unpack (status desktop))
        showSettings runtime desktop=snd <$> policyEffects runtime core desktop [PermissionAction "show" []] >>= awaitPermissionUI runtime
        isLeft (Left _)=True
        isLeft _=False
    seen<-newIORef []
    let execute desktop name _=do
          modifyIORef' seen (++[(name,activeText desktop)])
          pure (desktop {status="executed"},pure (Right (object ["text" .= activeText desktop])))
    withPermissionsAt path specs $ \runtime -> do
      (_,readonly)<-settledCall runtime execute base "read" (object [])
      first<-readonly
      check "read-only tools default to Enable" (not (isLeft first))
      (prompted,pending)<-settledCall runtime execute base "mutate" (object ["revision" .= (0::Int),"command" .= T.unlines ["line "<>T.pack (show n) | n<-[1..20::Int]]])
      let readOnlyReview=prompted {dialog=fmap (\dg->dg {focus=1}) (dialog prompted)}
          afterTyping=fst (handleEvent (V.EvKey (V.KChar 'x') []) readOnlyReview)
          (_,selectEffects)=handleEvent (V.EvKey (V.KChar 'a') [V.MCtrl]) prompted
          scrolled=fst (handleEvent (V.EvKey V.KEnd []) readOnlyReview)
      check "Ctrl+A on read-only metadata cannot approve" (null selectEffects)
      check "generic arguments are labeled read-only multiline fields" (case dialog afterTyping of
        Just dg -> any (\f->case f of TextArea "command" False b _ _ _ -> "line 1\n" `T.isPrefixOf` contents b; _ -> False) (fields dg) &&
          fmap fields (dialog afterTyping)==fmap fields (dialog readOnlyReview) && "line 20" `T.isInfixOf` snapshot scrolled
        _ -> False)
      calls<-readIORef seen
      check "mutations default to Prompt without executing" (length calls==1 && maybe False ((=="Agent permission").dialogTitle) (dialog prompted))
      executed<-withAsync pending $ \worker -> do
        blocked<-poll worker
        check "approval wait is deferred outside the initial call" (case blocked of Nothing->True; _->False)
        let current=changed {dialog=dialog prompted}
        next<-submit runtime 0 current
        result<-wait worker
        check "approval executes against CURRENT desktop and delivers deferred result"
          ((either (const Nothing) (field "text") result::Maybe T.Text)==Just "current")
        pure next
      check "approved effect returns the updated desktop" (status executed=="executed")
      (again,denied)<-settledCall runtime execute base "mutate" (object [])
      let (escaped,effects)=handleEvent (V.EvKey V.KEsc []) again
      _<-policyEffects runtime core escaped effects
      check "Escape denies a pending request" . isLeft =<< denied
      countBefore<-length <$> readIORef seen
      check "Allow once did not enable subsequent requests" (countBefore==2)
      (cancelPrompt,cancelled)<-settledCall runtime execute base "mutate" (object [])
      interrupted<-timeout 10000 cancelled
      check "cancelled deferred permission wait exits" (interrupted==Nothing)
      _<-submit runtime 0 cancelPrompt
      cancelledResult<-cancelled
      afterCancel<-length <$> readIORef seen
      check "cancelled request cannot execute after later approval" (isLeft cancelledResult && afterCancel==countBefore)
      settings<-showSettings runtime base
      firstEdit<-submit runtime 0 settings
      backed<-submit runtime 1 firstEdit
      check "human Back returns to current permission settings" (case dialog backed of Just dg->dialogTitle dg=="Agent Permissions"; _->False)
      escapedEdit<-submit runtime 0 backed
      let (closedEdit,closeEffects)=handleEvent (V.EvKey V.KEsc []) escapedEdit
      (_,retiredEdit)<-policyEffects runtime core closedEdit closeEffects
      check "human settings Escape retires receipt without expiry error" (case dialog retiredEdit of Nothing->not ("expired" `T.isInfixOf` status retiredEdit); _->False)
      settingsAgain<-showSettings runtime retiredEdit
      editing<-submit runtime 0 settingsAgain
      let disabled=editing {dialog=fmap (\dg->dg {fields=[Radio "mutate" ["Enable","Prompt","Disable"] 2]}) (dialog editing)}
      _<-submit runtime 0 disabled
      (_,blocked)<-settledCall runtime execute base "mutate" (object [])
      check "saved Disable rejects tools even with a cached registry" . isLeft =<< blocked
      persisted<-TIO.readFile path
      check "permission policy persists in its TOML namespace" ("disable" `T.isInfixOf` persisted && "permissions" `T.isInfixOf` persisted)
      (_,unknown)<-settledCall runtime execute base "not_registered" (object [])
      check "unknown tools cannot bypass permission checks" . isLeft =<< unknown
    -- Changing the shared file is observed on every dispatch and approval.
    TIO.writeFile path "[editor.mcp.permissions]\nmutate = 'prompt'\n"
    withPermissionsAt path specs $ \runtime -> do
      before<-length <$> readIORef seen
      (prompted,pending)<-settledCall runtime execute base "mutate" (object [])
      TIO.writeFile path "[editor.mcp.permissions]\nmutate = 'disable'\n"
      _<-submit runtime 0 prompted
      check "Disable written while queued is enforced before execution" . isLeft =<< pending
      after<-length <$> readIORef seen
      check "disabled pending callback never runs" (after==before)
      TIO.writeFile path "[broken\nsecret = 'do not leak'\n"
      (_,invalid)<-settledCall runtime execute base "read" (object [])
      invalidResult<-invalid
      check "invalid config fails closed without exposing its contents" (case invalidResult of Left err->not ("secret" `T.isInfixOf` err); _->False)
    TIO.writeFile path "[editor.mcp.permissions]\nbuild_output = 'disable'\n"
    withPermissionsAt path runtimeTools $ \runtime -> do
      (_,disabledOutput)<-settledCall runtime execute base "build_output" (object ["jobId" .= ("fixture"::T.Text)])
      check "build output honors its ordinary configured permission" . isLeft =<< disabledOutput
      settings<-showSettings runtime base
      check "build output is listed by Agent Permissions" (case dialog settings of Just dg->any (\f->case f of ListBox _ rows _->any (T.isPrefixOf "build_output") rows; _->False) (fields dg); _->False)
      TIO.writeFile path "[editor.mcp.permissions]\n"
      (_,enabledOutput)<-settledCall runtime execute base "build_output" (object ["jobId" .= ("fixture"::T.Text)])
      check "build output defaults to permitted read-only routing" . not . isLeft =<< enabledOutput
    removeFile path
    pendingAfterClose<-withPermissionsAt path specs $ \runtime -> do
      (desktop,requests)<-foldM (\(d,results) _->do
        (next,pending)<-settledCall runtime execute d "mutate" (object [])
        pure (next,results++[pending])) (base,[]) [1..32::Int]
      (_,overflow)<-settledCall runtime execute desktop "mutate" (object [])
      check "permission queue is capped at 32 requests" . isLeft =<< overflow
      let other=desktop {dialog=Just (Dialog "Other editor work" (Searching False "") [] 0 ["Close"] [])}
      preserved<-tickPermissions runtime other
      check "queued approvals preserve unrelated editor dialogs" (dialog preserved==dialog other)
      pure requests
    closed<-mapM (timeout 1000000) pendingAfterClose
    check "session shutdown completes every pending waiter" (all (maybe False isLeft) closed)
  putStrLn "MCP permission checks passed"

-- A live writer withholds data from the actual configuration read. The first
-- owner tick must return while that read waits, then the same request completes.
-- Exercise the real asynchronous owner until a reply or human approval appears.
settledCall :: Hide.MCPPermissions.Permissions -> (Desktop -> T.Text -> Value -> IO (Desktop,IO (Either T.Text Value))) -> Desktop -> T.Text -> Value -> IO (Desktop,IO (Either T.Text Value))
settledCall runtime callback=settledTool runtime (permissionCall runtime callback)
settledTool :: Hide.MCPPermissions.Permissions -> (desktop -> name -> args -> IO (Desktop,IO result)) -> desktop -> name -> args -> IO (Desktop,IO result)
settledTool runtime initiate desktop name args=do
  (initial,continuation)<-initiate desktop name args
  worker<-async continuation
  let reply=(Control.Concurrent.Async.waitCatch worker >>= \result->case result of
        Right value->pure value
        Left err->case fromException err :: Maybe SomeAsyncException of
          -- Cancellation retires the actual request promise. Re-reading that
          -- terminal promise preserves the original reusable-reply assertion.
          Just _->continuation
          Nothing->throwIO err) `onException` cancel worker
      loop current=do
        outcome<-poll worker
        case outcome of
          Just _->pure (current,reply)
          Nothing | Just dg<-dialog current,PermissionDialog action<-purpose dg,"approve:" `T.isPrefixOf` action->pure (current,reply)
                  | otherwise->threadDelay 1000 >> tickPermissions runtime current >>= loop
  timeout 3000000 (loop initial) >>= maybe (cancel worker >> error "policy dispatch did not settle") pure

awaitPermissionUI :: Hide.MCPPermissions.Permissions -> Desktop -> IO Desktop
awaitPermissionUI runtime desktop=do
  let loop current=case dialog current of
        Just dg | PermissionDialog action<-purpose dg,not ("loading:" `T.isPrefixOf` action)->pure current
        _->threadDelay 1000 >> tickPermissions runtime current >>= loop
  timeout 3000000 (loop desktop) >>= maybe (error "permission UI did not settle") pure

settleDialog :: Hide.MCPPermissions.Permissions -> Dialog -> Desktop -> IO Desktop
settleDialog runtime original desktop
  | any (\widget->case widget of TextArea "diff" True _ _ _ _->True; _->False) (fields original)=pure desktop
  | otherwise=do
      let loop current=case dialog current of
            Just dg | purpose dg==purpose original || (case purpose dg of PermissionDialog action->"loading:" `T.isPrefixOf` action; _->False)->threadDelay 1000 >> tickPermissions runtime current >>= loop
            _->pure current
      timeout 3000000 (loop desktop) >>= maybe (error "permission dialog action did not settle") pure

policyResponsivenessChecks :: IO ()
#ifdef mingw32_HOST_OS
policyResponsivenessChecks=pure ()
#else
policyResponsivenessChecks=bracket temporary removePathForcibly $ \directory->do
  let path=directory </> "held-policy.toml"
      specs=[object ["name" .= ("read_buffer"::T.Text),"annotations" .= object ["readOnlyHint" .= True]]]
      base=addDocument Nothing (newBuffer "source") (initialDesktop (80,25))
      bid=fromMaybe (error "Missing source") (sourceFixtureBuffer <$> activeWindow base)
  createNamedPipe path 0o600
  -- Keep a kernel writer alive before admission. A capped oversized release
  -- ends hGet without EOF, so closing the writer cannot race reader startup.
  bracket (openFd path ReadWrite defaultFileFlags {nonBlock=True}) closeFd $ \gate->withPermissionsAt path specs $ \owner->do
    let reader=bufferReader owner (pure (Right ()))
    withAsync (P.captureBuffer reader (readerReference reader bid)) $ \capture->do
      let queued=do
            observed<-threadStatus (Control.Concurrent.Async.asyncThreadId capture)
            case observed of ThreadBlocked BlockedOnMVar->pure (); _->threadDelay 1000 >> queued
      timeout 1000000 queued >>= maybe (fail "Capture did not reach admission") pure
      state<-newMVar base
      withAsync (modifyMVar_ state (tickPermissions owner)) $ \tick->do
        returned<-timeout 100000 (wait tick)
        available<-tryReadMVar state
        pending<-poll capture
        let writeAll []=pure ()
            writeAll bytes=do
              written<-fdWrite gate bytes `catch` \(_::IOException)->pure 0
              threadDelay 1000
              writeAll (drop (fromIntegral written) bytes)
        -- Release before asserting so the RED fixture tears down cleanly too.
        timeout 3000000 (writeAll (replicate 1048577 'x')) >>= maybe (fail "Held policy read never consumed release") pure
        _<-timeout 1000000 (wait tick) >>= maybe (fail "Released read did not finish") pure
        check "permission owner returns while actual policy read is held"
          (case (returned,available,pending) of (Just (),Just _,Nothing)->True; _->False)
        let await=do
              outcome<-poll capture
              case outcome of
                Just (Right (Left "thc/config.toml exceeds 1 MiB"))->pure ()
                Just _->fail "Held policy release did not fail closed"
                Nothing->modifyMVar_ state (tickPermissions owner) >> threadDelay 1000 >> await
        timeout 1000000 await >>= maybe (fail "Released policy never admitted capture") pure
#endif

-- Completion signals retain publication before a waiter and wake an existing
-- waiter once. The same real owner route rechecks callers, modal receipts and a
-- settings Save barrier before executing a formerly enabled wire operation.
policyWakeChecks :: IO ()
policyWakeChecks=bracket temporary removePathForcibly $ \directory->do
  let path=directory </> "wake.toml"
      specs=[object ["name" .= name,"annotations" .= object ["readOnlyHint" .= readonly]] | (name,readonly)<-[("read_buffer"::T.Text,True),("read",True),("mutate",False)]]
      base=addDocument Nothing (newBuffer "source") (initialDesktop (80,25))
      ident=fromMaybe (error "Missing source") (sourceFixtureBuffer <$> activeWindow base)
      core d _=pure (False,d)
      action owner actionToken values d=snd <$> policyEffects owner core d [PermissionAction actionToken values]
      advance owner d=do
        timeout 3000000 (STM.atomically (awaitPermissionWork owner)) >>= maybe (error ("Permission completion did not wake owner: "++T.unpack (status d))) pure
        tickPermissions owner d
      token d=case dialog d of Just dg | PermissionDialog value<-purpose dg->value; _->error "Missing permission modal"
      replacement=Dialog "Replacement modal" (Searching False "unrelated") [] 0 ["Close"] []
      retained d=case dialog d of Just dg->dialogTitle dg=="Replacement modal"; _->False
  withPermissionsAt path specs $ \owner->do
    let reader=bufferReader owner (pure (Right ()))
    withAsync (P.captureBuffer reader (readerReference reader ident)) $ \capture->do
      -- Accepted ingress publishes before this waiter is installed.
      let queued=do
            state<-threadStatus (Control.Concurrent.Async.asyncThreadId capture)
            case state of ThreadBlocked BlockedOnMVar->pure (); _->threadDelay 1000 >> queued
      timeout 3000000 queued >>= maybe (error "Capture did not enqueue") pure
      timeout 3000000 (STM.atomically (awaitPermissionWork owner)) >>= maybe (error "Accepted capture did not wake owner") pure
      noSpin<-timeout 10000 (STM.atomically (awaitPermissionWork owner))
      check "consumed permission wake does not spin" (noSpin==Nothing)
      capturing<-tickPermissions owner base
      captured<-advance owner capturing
      image<-timeout 3000000 (wait capture)
      check "policy completion admits typed capture without periodic polling" (case image of Just (Right _)->True; _->False)
      check "typed capture preserves source selection" (fmap selection (activeWindow captured)==fmap selection (activeWindow base))
    withAsync (STM.atomically (awaitPermissionWork owner)) $ \wake->do
      let parked=do
            state<-threadStatus (Control.Concurrent.Async.asyncThreadId wake)
            case state of ThreadBlocked BlockedOnSTM->pure (); _->threadDelay 1000 >> parked
      timeout 3000000 parked >>= maybe (error "Permission wake waiter did not park") pure
      withAsync (P.captureBuffer reader (readerReference reader ident)) $ \capture->do
        timeout 3000000 (wait wake) >>= maybe (error "Ingress failed to wake existing waiter") pure
        admitted<-tickPermissions owner base
        _<-advance owner admitted
        timeout 3000000 (wait capture) >>= maybe (error "Awakened capture did not finish") (either (error . T.unpack) (const (pure ())))
    actor<-newIORef (Right ())
    seen<-newIORef (0::Int)
    let execute d _ _=modifyIORef' seen (+1) >> pure (d,pure (Right Null))
    (queued,reply)<-permissionCallAs (readIORef actor) owner execute base "read" (object [])
    timeout 3000000 (STM.atomically (awaitPermissionWork owner)) >>= maybe (error "Enabled wire policy did not finish") pure
    writeIORef actor (Left "actor revoked")
    _<-tickPermissions owner queued
    check "enabled wire operation rechecks revoked caller after policy IO" . (==Left "actor revoked") =<< reply
    check "revoked wire callback never runs" . (==0) =<< readIORef seen
    (admission,pending)<-permissionCall owner execute base "mutate" (object [])
    prompted<-advance owner admission
    denied<-action owner (token prompted) ["1"] prompted {dialog=Just replacement}
    check "stale Deny preserves newer modal" (retained denied)
    check "stale Deny still resolves original request" . either (const True) (const False) =<< pending
    loading<-action owner "show" [] base
    closed<-action owner (token loading) [] loading {dialog=Just replacement}
    late<-advance owner closed
    check "closed loading receipt cannot replace newer modal" (retained late)
    loadingSettings<-action owner "show" [] base
    settings<-advance owner loadingSettings
    staleEdit<-action owner (token settings) ["0","0"] settings {dialog=Just replacement}
    check "stale settings Edit preserves newer modal" (retained staleEdit)
    loadingEdit<-action owner (token settings) ["0","1"] settings
    editing<-advance owner loadingEdit
    staleSave<-action owner (token editing) ["0","2"] editing {dialog=Just replacement}
    check "stale settings Save preserves newer modal" (retained staleSave)
    exists<-doesFileExist path
    check "stale settings Save does not write policy" (not exists)
    reopenedLoading<-action owner "show" [] editing
    reopenedSettings<-advance owner reopenedLoading
    reopenedEditLoading<-action owner (token reopenedSettings) ["0","1"] reopenedSettings
    reopenedEdit<-advance owner reopenedEditLoading
    staleReopen<-action owner (token editing) ["0","2"] reopenedEdit
    check "reopened same setting rejects old Save receipt" (token staleReopen==token reopenedEdit && token editing/=token reopenedEdit)
    existsAgain<-doesFileExist path
    check "old same-purpose Save cannot write through reopen" (not existsAgain)
    -- Its old Enable reply is ready, but has
    -- not yet been adopted when the human accepts Disable through actual UI.
    (oldPolicy,oldReply)<-permissionCall owner execute reopenedEdit "read" (object [])
    timeout 3000000 (STM.atomically (awaitPermissionWork owner)) >>= maybe (error "Old policy did not complete") pure
    saving<-action owner (token reopenedEdit) ["0","2"] oldPolicy
    whileSaving<-tickPermissions owner saving
    check "Save barrier blocks old enabled policy reply" . (==0) =<< readIORef seen
    (revoked,result)<-withAsync oldReply $ \worker->do
      let await current=do
            completed<-Control.Concurrent.Async.race (wait worker) (STM.atomically (awaitPermissionWork owner))
            case completed of Left value->pure (current,value); Right ()->tickPermissions owner current >>= await
      timeout 3000000 (await whileSaving) >>= maybe (error "Saved policy did not resolve queued request") pure
    check "fresh Disable replaces pre-save policy result" (either (const True) (const False) result)
    check "old policy never executes after accepted Disable" . (==0) =<< readIORef seen
    check "policy settings remain current after save" (case dialog revoked of Just dg->dialogTitle dg=="Agent Permissions"; _->False)
  -- Shutdown terminally resolves a request whose initial admission is queued.
  pending<-withPermissionsAt path specs $ \owner->do
    (_,reply)<-permissionCall owner (\d _ _->pure (d,pure (Right Null))) base "mutate" (object [])
    pure reply
  check "shutdown resolves pending policy admission once" . either (const True) (const False) =<< pending

reviewChecks :: IO ()
reviewChecks=Tool.withTools [] BufferTools.tools $ \toolset->bracket temporary removePathForcibly $ \directory ->
  withPermissionsAt (directory </> "review.toml") (fileTools++Tool.toolDefinitions toolset) $ \runtime -> do
    let core d _=pure (False,d)
        base=addDocument Nothing (newBuffer "old\n") (initialDesktop (100,32))
        bid=fromMaybe (error "missing buffer") (sourceFixtureBuffer <$> activeWindow base)
        patch="@@ -1 +1 @@\n-old\n+agent\n"
        revised="@@ -1 +1 @@\n-old\n+human λ\n"
        args=object ["buffers" .= [object ["bufferId" .= bid,"revision" .= (0::Int),"diff" .= (patch::T.Text)]]]
        request d=startDiffCall toolset runtime (pure (Right ())) d "buffer_apply_diff" args
        input event d=let (next,fx)=handleEvent event d in snd <$> policyEffects runtime core next fx
        key k mods=input (V.EvKey k mods)
        replace text d=key (V.KChar 'a') [V.MCtrl] d >>= input (V.EvPaste (TE.encodeUtf8 text))
        settle d=do
          next<-tickPermissions runtime d
          if maybe True (any (T.isPrefixOf "Diff not applied:") . body) (dialog next)
            then pure next else threadDelay 1000 >> settle next
        allow d=key (V.KChar 'a') [V.MAlt] d >>= \started->timeout 5000000 (settle started) >>= maybe (error "diff preparation timeout") pure
        deny=key V.KEsc []
        area d=case dialog d of Just dg -> [(b,sel,sr,sc) | TextArea _ True b sel sr sc<-fields dg]; _ -> []
        areaText d=case area d of (b,_,_,_):_->contents b; _->""
        isLeft (Left _)=True; isLeft _=False
        diffField name value=do
          [entry]<-field "buffers" value :: Maybe [Value]
          field name entry
    (shown,pending)<-request base
    let image=snapshot shown
        colors=snapshotHtml shown
        dg=fromMaybe (error "missing review") (dialog shown)
        rect=dialogRect shown dg
    check "approval shows object fields and unescaped diff lines" (all (`T.isInfixOf` image) ["bufferId:","revision:","File:","-old","+agent","Allow once","Deny"])
    check "diff review colors added and removed lines" ("rgb(85,255,85)" `T.isInfixOf` colors && "rgb(255,85,85)" `T.isInfixOf` colors)
    check "approval controls and edited text are private to the human" (not (guestKeyboardAllowed shown) && not (readableAt shown 10 (snd (screenSize shown)-1)) && not (guestEffectsAllowed [PermissionAction "approve:1" ["0",revised]]) && and [not (readableAt shown x y) | x<-[left rect..left rect+width rect-1],y<-[top rect..top rect+height rect-1]])
    edited<-replace revised shown
    check "human edits private diff without changing target buffer" (areaText edited==revised && activeText edited=="old\n")
    undone<-key (V.KChar 'z') [V.MCtrl] edited
    check "diff editor undo is independent of target history" (areaText undone==patch && activeText undone=="old\n")
    restored<-key (V.KChar 'y') [V.MCtrl] undone
    applied<-allow restored
    result<-pending
    check "Allow applies human patch and reports exact applied diff" (activeText applied=="human λ\n" && dialog applied==Nothing &&
      (either (const Nothing) (diffField "appliedDiff") result::Maybe T.Text)==Just revised &&
      (either (const Nothing) (diffField "userModified") result::Maybe Bool)==Just True &&
      (either (const Nothing) (diffField "revision") result::Maybe Int)==Just 1)
    (bad,badPending)<-request base
    invalid<-replace "not a diff" bad >>= allow
    check "invalid human diff retains review and does not edit target" (dialog invalid/=Nothing && areaText invalid=="not a diff" && activeText invalid=="old\n" && "Diff not applied:" `T.isInfixOf` snapshot invalid)
    _<-deny invalid
    _<-badPending
    (retry,retryPending)<-request base
    invalidAgain<-replace "bad" retry >>= allow
    fixed<-replace revised invalidAgain >>= allow
    fixedResult<-retryPending
    check "correcting invalid diff can approve same pending request" (activeText fixed=="human λ\n" && not (isLeft fixedResult))
    (partial,partialPending)<-request base
    atomic<-replace "@@ -1 +1 @@\n-old\n+first\n@@ -9 +9 @@\n-missing\n+second\n" partial >>= allow
    check "a later invalid hunk cannot partially apply the human patch" (activeText atomic=="old\n" && dialog atomic/=Nothing)
    _<-deny atomic
    _<-partialPending
    (stale,stalePending)<-request base
    let changed=stale {buffers=M.adjust (\doc->doc {documentBuffer=replaceBuffer False "newer\n" (documentBuffer doc)}) bid (buffers stale)}
    rejected<-allow changed
    check "stale target retains review without applying any hunk" (dialog rejected/=Nothing && activeText rejected=="newer\n" && "changed" `T.isInfixOf` snapshot rejected)
    _<-deny rejected
    check "Deny after stale rejection resolves request" . isLeft =<< stalePending
    (old,oldPending)<-request base
    _<-timeout 10000 oldPending
    (replacement,replacementPending)<-request base
    let oldDialog=fromMaybe (error "missing cancelled dialog") (dialog old)
        (obsolete,oldEffects)=submitDialog 0 oldDialog replacement
    (_,stillWaiting)<-policyEffects runtime core obsolete oldEffects
    check "cancelled dialog cannot approve its replacement ticket" (dialog stillWaiting==dialog replacement && activeText stillWaiting=="old\n")
    replacementApplied<-allow stillWaiting
    replacementResult<-replacementPending
    check "replacement retains its own decision and original diff" (activeText replacementApplied=="agent\n" && (either (const Nothing) (diffField "userModified") replacementResult::Maybe Bool)==Just False)
    (cancelled,cancelPending)<-request base
    _<-key (V.KFun 3) [V.MAlt] cancelled
    check "close shortcut denies pending patch" . isLeft =<< cancelPending
    (closing,closePending)<-request base
    let closeRect=maybe (error "missing approval") (dialogCloseRect closing) (dialog closing)
    _<-input (V.EvMouseDown (left closeRect+1) (top closeRect) V.BLeft []) closing
    check "review close button denies without applying" . isLeft =<< closePending
    (entered,enterPending)<-request base
    newline<-key V.KEnter [] entered
    check "Enter edits diff rather than implicitly approving" (dialog newline/=Nothing && T.length (areaText newline)==T.length patch+1 && activeText newline=="old\n")
    _<-deny newline
    _<-enterPending
    (protected,protectedPending)<-request base
    let private=protected {buffers=M.adjust (\doc->doc {documentLabel=Just "Conversation"}) bid (buffers protected)}
    privateResult<-allow private
    check "approval does not bypass protected-buffer guards" (activeText privateResult=="old\n" && dialog privateResult/=Nothing)
    _<-deny privateResult
    _<-protectedPending
    let long=T.unlines ["+line "<>T.pack (show n) | n<-[1..70::Int]]
        large=shown {dialog=fmap (\view->view {fields=[TextArea "diff" True (newBuffer long) (Selection 0 0) 0 0],focus=0}) (dialog shown)}
    bottom<-key V.KEnd [V.MCtrl] large
    check "long diff scrolls to final lines and retains bottom actions" ("+line 70" `T.isInfixOf` snapshot bottom && all (`T.isInfixOf` snapshot bottom) ["Allow once","Deny"])
    let (narrow,_)=handleEvent (V.EvResize 60 18) bottom
    check "small screen keeps approval buttons in bounds" (case dialog narrow of Just view -> all (\r->top r>=1 && top r<snd (screenSize narrow)-1) (buttonRects narrow view); _->False)

configChecks :: IO ()
configChecks=bracket temporary removePathForcibly $ \directory -> do
  let path=directory </> "thc" </> "config.toml"
      namespace=["editor","mcp","permissions"]
      original="# Keep me\n[compiler]\noptimization = 3 # tuning\nnotes = '''literal\n[editor.mcp.permissions]\ninside string'''\n\n[editor.mcp.permissions]\nmutate = 'prompt' # keep this comment\nfuture_tool = 'disable'\n\n[editor.defaults]\nscale = 1.25 # exact\n\n[compiler.future]\nflag = true\n"
      edit values text=either (error . T.unpack) id (updateConfigTable namespace values text)
      updated=edit (object ["mutate" .= ("disable"::T.Text)]) original
  check "TOML scalar replacement preserves unrelated bytes and comments"
    (updated==T.replace "mutate = 'prompt'" "mutate = \"disable\"" original)
  let inserted=edit (object ["new_tool" .= ("enable"::T.Text)]) updated
  check "new policy keys preserve later tables and unknown tool settings"
    ("future_tool = 'disable'" `T.isInfixOf` inserted && "[compiler.future]\nflag = true\n" `T.isSuffixOf` inserted)
  let inline="editor = { mcp = { permissions = { mutate = 'prompt' } } }\n"
  check "existing inline-table scalar policies update safely"
    (edit (object ["mutate" .= ("disable"::T.Text)]) inline==T.replace "'prompt'" "\"disable\"" inline)
  check "unsafe new inline-table keys are refused without reprinting config"
    (case updateConfigTable namespace (object ["new_tool" .= ("enable"::T.Text)]) inline of Left _->True; _->False)
  absent<-readEditorDefaultsAt path
  check "absent config has empty editor defaults" (absent==Right (object []))
  saved<-writeEditorDefaultsAt path (object ["backend" .= ("terminal"::T.Text),"scale" .= (1.5::Double),"wordStar" .= True])
  check "editor defaults create global-style parent directories" (saved==Right ())
  loaded<-readEditorDefaultsAt path
  check "editor defaults roundtrip primitive TOML values" (loaded==Right (object ["backend" .= ("terminal"::T.Text),"scale" .= (1.5::Double),"wordStar" .= True]))
  submitSaved<-writeEditorDefaultsAt path (object ["chatSubmit" .= ("steer"::T.Text)])
  submitLoaded<-readEditorDefaultsAt path
  check "chat input default persists through the ordinary TOML writer" (submitSaved==Right () && case submitLoaded of Right value->field "chatSubmit" value==Just ("steer"::T.Text) && field "wordStar" value==Just True; _->False)
  TIO.writeFile path original
  _<-writeEditorDefaultsAt path (object ["scale" .= (2::Int),"blinkCursor" .= False])
  preserved<-TIO.readFile path
  check "defaults writes preserve permission and compiler namespaces" ("mutate = 'prompt' # keep this comment" `T.isInfixOf` preserved && "optimization = 3 # tuning" `T.isInfixOf` preserved)
  (a,b)<-concurrently (writeEditorDefaultsAt path (object ["columns" .= (100::Int)])) (writeEditorDefaultsAt path (object ["rows" .= (40::Int)]))
  combined<-readEditorDefaultsAt path
  check "concurrent config writers reload and retain both changes" (a==Right () && b==Right () && case combined of Right value->field "columns" value==Just (100::Int) && field "rows" value==Just (40::Int); _->False)
  let malformed="[compiler\npassword = 'hidden'\n"
  TIO.writeFile path malformed
  refused<-writeEditorDefaultsAt path (object ["scale" .= (2::Int)])
  unchanged<-TIO.readFile path
  check "invalid TOML is not overwritten or leaked" (unchanged==malformed && case refused of Left err->not ("hidden" `T.isInfixOf` err); _->False)
  bad<-writeEditorDefaultsAt path (object ["notASetting" .= True])
  check "unknown editor defaults are rejected" (case bad of Left _->True; _->False)

projectConfigChecks :: IO ()
projectConfigChecks=bracket temporary removePathForcibly $ \directory -> do
  let root=directory </> "project"
      nested=root </> "nested"
      deep=nested </> "src"
      source=deep </> "Main.hs"
      projectPath=root </> "thc.toml"
      localPath=nested </> "thc.toml"
      globalDirectory=directory </> "global"
      globalPath=globalDirectory </> "thc" </> "config.toml"
      absentPath=directory </> "absent.toml"
      isLeft (Left _)=True
      isLeft _=False
  createDirectoryIfMissing True deep
  createDirectory (root </> ".git")
  TIO.writeFile source "main = pure ()\n"
  TIO.writeFile (directory </> "thc.toml") "[editor.defaults]\nscale = 99\n"
  fallback<-projectConfigPath source
  check "project discovery stops at a Git boundary before outer settings" (fallback==projectPath)
  TIO.writeFile projectPath "[editor.defaults]\nscale = 2\nrows = 40\n"
  inherited<-projectConfigPath source
  check "nested file paths discover enclosing project settings" (inherited==projectPath)
  TIO.writeFile localPath "[editor.defaults]\ncolumns = 100\n"
  nearest<-projectConfigPath deep
  check "nearest existing local config wins within a project" (nearest==localPath)
  removeFile localPath
  TIO.writeFile (nested </> "package.cabal") "name: nested\n"
  packageBoundary<-projectConfigPath source
  check "nested Cabal package stops inherited settings at its own boundary" (packageBoundary==localPath)
  removeFile (nested </> "package.cabal")
  TIO.writeFile (nested </> "cabal.project") "packages: .\n"
  cabalBoundary<-projectConfigPath source
  check "cabal.project also defines the local settings boundary" (cabalBoundary==localPath)
  removeFile (nested </> "cabal.project")
  removeDirectory (root </> ".git")
  TIO.writeFile (root </> ".git") "gitdir: elsewhere\n"
  removeFile projectPath
  worktreeBoundary<-projectConfigPath source
  check "Git worktree marker files define settings boundaries" (worktreeBoundary==projectPath)
  removeFile (directory </> "thc.toml")
  let orphan=directory </> "orphan"
  createDirectory orphan
  orphanPath<-projectConfigPath orphan
  check "unmarked directories fall back to their original directory" (orphanPath==orphan </> "thc.toml")
  absentContext<-readAgentContextAt absentPath
  check "missing agent context is empty" (absentContext==Right "")
  let context="Use local conventions.\nKeep \"quoted\" names and \\ paths.\nλ documentation\n"
      original="# preserve context comments\n[compiler]\noptimization = 3 # unchanged\n\n[editor.agent]\ncontext = \"\"\"old\nmultiline\"\"\" # context note\nother = 'keep'\n\n[editor.mcp.permissions]\nmutate = 'disable'\n"
  TIO.writeFile absentPath original
  savedContext<-writeAgentContextAt absentPath context
  rereadContext<-readAgentContextAt absentPath
  preserved<-TIO.readFile absentPath
  check "multiline context updates roundtrip valid TOML and preserve unrelated comments/tables"
    (savedContext==Right () && rereadContext==Right context && "optimization = 3 # unchanged" `T.isInfixOf` preserved && "# context note\nother = 'keep'" `T.isInfixOf` preserved && "mutate = 'disable'" `T.isInfixOf` preserved)
  oversized<-writeAgentContextAt absentPath (T.replicate 16385 "x")
  afterOversize<-TIO.readFile absentPath
  check "oversized context writes are rejected without changing configuration" (isLeft oversized && afterOversize==preserved)
  TIO.writeFile absentPath ("[editor.agent]\ncontext = '"<>T.replicate 16385 "x"<>"'\n")
  oversizedRead<-readAgentContextAt absentPath
  check "oversized contexts on disk are rejected" (isLeft oversizedRead)
  TIO.writeFile absentPath "[editor.agent]\ncontext = 12\n"
  wrongType<-readAgentContextAt absentPath
  check "agent context requires a string" (isLeft wrongType)
  TIO.writeFile absentPath "[broken\ncontext = 'private-context-value'\n"
  malformedRead<-readAgentContextAt absentPath
  malformedWrite<-writeAgentContextAt absentPath "replacement"
  malformedDisk<-TIO.readFile absentPath
  check "malformed agent config is neither exposed nor overwritten" (all (\result->case result of Left err->T.length err<256 && not ("private-context-value" `T.isInfixOf` err); _->False) [fmap (const ()) malformedRead,malformedWrite] && "private-context-value" `T.isInfixOf` malformedDisk)
  bracket (lookupEnv "XDG_CONFIG_HOME" <* setEnv "XDG_CONFIG_HOME" globalDirectory)
    (maybe (unsetEnv "XDG_CONFIG_HOME") (setEnv "XDG_CONFIG_HOME")) $ \_ -> do
      _<-writeEditorDefaults (object ["backend" .= ("terminal"::T.Text),"scale" .= (1::Int),"wordStar" .= True])
      _<-writeAgentContextAt globalPath "Global guidance"
      TIO.writeFile projectPath "[editor.defaults]\nscale = 2\nrows = 40\n[editor.agent]\ncontext = 'Project guidance'\n[editor.mcp.permissions]\nmutate = 'enable'\n"
      merged<-readEditorDefaultsFor source
      globalOnly<-readEditorDefaults
      check "project defaults override global keys while inheriting omitted keys"
        (merged==Right (object ["backend" .= ("terminal"::T.Text),"scale" .= (2::Int),"rows" .= (40::Int),"wordStar" .= True]) && globalOnly==Right (object ["backend" .= ("terminal"::T.Text),"scale" .= (1::Int),"wordStar" .= True]))
      TIO.appendFile globalPath "\n[editor.keybindings.terminal.source]\n\"hide.file.save\" = [\"Ctrl+Shift+S\"]\n\"hide.file.open\" = []\n"
      TIO.appendFile projectPath "\n[editor.keybindings.terminal.source]\n\"hide.file.save\" = []\n"
      mergedKeys<-readKeybindingsFor source
      check "project keybindings replace global chords and preserve explicit removal" (mergedKeys==Right (M.singleton "terminal" (M.singleton "source" (M.fromList [("hide.file.save",[]),("hide.file.open",[])]))))
      contexts<-readAgentContexts source
      check "agent contexts return separate global/project text and resolved paths"
        (contexts==Right (object ["global" .= object ["path" .= globalPath,"text" .= ("Global guidance"::T.Text)],"project" .= object ["path" .= projectPath,"text" .= ("Project guidance"::T.Text)]]))
      calls<-newIORef (0::Int)
      let execute d _ _=modifyIORef' calls (+1) >> pure (d,pure (Right Null))
          spec=object ["name" .= ("mutate"::T.Text),"annotations" .= object ["readOnlyHint" .= False]]
          base=(initialDesktop (80,25)) {defaultDirectory=Just root}
      TIO.appendFile globalPath "\n[editor.mcp.permissions]\nmutate = 'disable'\n"
      withPermissions [spec] $ \runtime -> do
        (_,reply)<-settledCall runtime execute base "mutate" (object [])
        result<-reply
        count<-readIORef calls
        check "project permissions never override global permissions" (isLeft result && count==0)
      TIO.writeFile projectPath "[editor.defaults]\n[editor.agent]\ncontext = ''\n"
      emptyDefaults<-readEditorDefaultsFor source
      emptyContexts<-readAgentContexts source
      check "empty project defaults inherit global settings while empty context remains explicit"
        (emptyDefaults==globalOnly && case emptyContexts of Right value->(field "project" value >>= field "text"::Maybe T.Text)==Just ""; _->False)
      _<-writeEditorDefaults (object ["chatSubmit" .= ("steer"::T.Text)])
      inheritedSubmit<-readEditorDefaultsFor source
      TIO.writeFile projectPath "[editor.defaults]\nchatSubmit = 'query'\n"
      overriddenSubmit<-readEditorDefaultsFor source
      check "project chat defaults override the global default and otherwise inherit it"
        ((inheritedSubmit >>= maybe (Left "missing") Right . field "chatSubmit")==Right ("steer"::T.Text) &&
         (overriddenSubmit >>= maybe (Left "missing") Right . field "chatSubmit")==Right ("query"::T.Text))
      TIO.writeFile projectPath "[invalid\nsecret = 'private-project-value'\n"
      malformedDefaults<-readEditorDefaultsFor source
      malformedContexts<-readAgentContexts source
      check "malformed project errors are bounded and do not reveal configuration contents"
        (all (\result->case result of Left err->T.length err<256 && not ("private-project-value" `T.isInfixOf` err); _->False) [malformedDefaults,malformedContexts])

agentLimitChecks :: IO ()
agentLimitChecks=bracket temporary removePathForcibly $ \directory -> do
  let root=directory </> "project"
      globalDirectory=directory </> "global"
      globalPath=globalDirectory </> "thc" </> "config.toml"
      projectPath=root </> "thc.toml"
      expect label expected=readAgentLimitsFor root >>= check label . (==Right expected)
      rejected label=readAgentLimitsFor root >>= check label . either (const True) (const False)
      limits="[editor.agents]\n"
  createDirectory root
  createDirectory (root </> ".git")
  createDirectoryIfMissing True (globalDirectory </> "thc")
  bracket (lookupEnv "XDG_CONFIG_HOME" <* setEnv "XDG_CONFIG_HOME" globalDirectory)
    (maybe (unsetEnv "XDG_CONFIG_HOME") (setEnv "XDG_CONFIG_HOME")) $ \_ -> do
      expect "agent limits default to eight total and four direct children" (8,4)
      TIO.writeFile globalPath (limits<>"max_agents = 12\nmax_subagents = 6\nfuture = 'preserve'\n")
      expect "global agent limits can replace built-in defaults" (12,6)
      TIO.writeFile projectPath (limits<>"max_agents = 3\n")
      expect "project limits lower supplied fields and inherit omitted global fields" (3,6)
      TIO.writeFile projectPath (limits<>"max_agents = 64\nmax_subagents = 64\n")
      expect "project limits cannot raise global ceilings" (12,6)
      TIO.writeFile projectPath (limits<>"max_subagents = 0\n")
      expect "zero direct subagents disables further delegation" (12,0)
      _<-writeEditorDefaultsAt globalPath (object ["scale" .= (2::Int)])
      _<-writeAgentContextAt globalPath "Human guidance"
      preserved<-TIO.readFile globalPath
      check "existing config writers preserve limits and unknown agent fields"
        ("max_agents = 12\nmax_subagents = 6\nfuture = 'preserve'\n" `T.isInfixOf` preserved)
      mapM_ (\bad->TIO.writeFile projectPath (limits<>bad<>"\n") >> rejected "invalid project agent limits fail closed")
        ["max_agents = 0","max_agents = 65","max_subagents = -1","max_subagents = 65",
         "max_agents = '3'","max_subagents = 2.0","max_agents = true","max_subagents = []"]
      TIO.writeFile projectPath "[editor]\nagents = false\n"
      rejected "agent limit namespace must be a table"
      TIO.writeFile projectPath (limits<>"max_agents = 1\nmax_subagents = 0\n")
      TIO.writeFile globalPath (limits<>"max_agents = 999999999999999999\n")
      rejected "a restrictive project cannot hide an invalid global ceiling"
      TIO.writeFile globalPath "[editor.agents\nprivate = 'hidden-value'\n"
      malformed<-readAgentLimitsFor root
      check "malformed agent config errors do not expose source values"
        (case malformed of Left err->not ("hidden-value" `T.isInfixOf` err); _->False)
      removeFile globalPath
      TIO.writeFile projectPath (limits<>"max_agents = 64\nmax_subagents = 64\n")
      expect "absent global config still imposes built-in ceilings on projects" (8,4)

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.:key))

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)

temporary :: IO FilePath
temporary=do
  parent<-getTemporaryDirectory
  (path,h)<-openTempFile parent "thc-permissions"
  hClose h
  removeFile path
  createDirectory path
  canonicalizePath path

keybindingConfigChecks :: IO ()
keybindingConfigChecks=bracket temporary removePathForcibly $ \directory->do
  let path=directory </> "thc.toml"
  TIO.writeFile path "[editor.keybindings.terminal.source]\n\"hide.file.save\" = [\"Ctrl+Shift+S\"]\n\"hide.file.open\" = []\n"
  loaded<-readKeybindingsAt path
  unless (loaded==Right (M.singleton "terminal" (M.singleton "source" (M.fromList [("hide.file.save",["Ctrl+Shift+S"]),("hide.file.open",[])])))) (error "keybinding arrays and explicit unbind load from TOML")
  TIO.writeFile path "[editor.keybindings.terminal.source]\n\"hide.file.save\" = \"Ctrl+S\"\n"
  invalid<-readKeybindingsAt path
  unless (either (const True) (const False) invalid) (error "scalar keybindings must fail")
