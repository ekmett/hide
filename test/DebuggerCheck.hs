-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE MagicHash, OverloadedStrings #-}
-- |
-- Module      : DebuggerCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : MagicHash, OverloadedStrings

module DebuggerCheck (checks,fixture,cleanup,outputOwnerChecks,outputLifecycleCheck,richBreakpointChecks) where

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, poll, wait)
import Control.Exception (bracket,evaluate)
import Control.Monad (unless, when, forM_)
import Data.Aeson
import Data.IORef
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Array.Byte (ByteArray(..))
import GHC.Exts (Int(..),sizeofByteArray#)
import qualified Data.Text.Internal as TI
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.Socket as Socket
import System.Directory
import System.Exit (ExitCode(..))
import System.IO
import System.Info (os)
import System.Process
import System.Timeout (timeout)
import qualified Hide.Consoles as C
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Hide.Terminal as Terminal
import Hide.Buffer
import qualified Hide.Plugin.Window as W
import Hide.PluginWindowHost (tickPluginWindows)
import Hide.Debugger
import Hide.DebuggerSidebarTypes (DebugSidebarRequest(..),DebuggerWatch(..))
import Hide.Files (FileState(..))
import Hide.GuestAccess (guestEffectsAllowed,guestCommandAllowed,privateDialogField,protectedBuffer)
import qualified Hide.Protocol as P
import Hide.EditorMCP (debugTools)
import Hide.Model
import Hide.SystemOne (withSystemOne,systemOneServices)
import Hide.Highlighting (withHighlighting,tickHighlighting)
import Hide.Render (snapshotHtml)

-- Exercise the real idle owner, not a content-version helper. A matching title
-- grants no debugger ownership of an unrelated source document.
outputOwnerChecks :: IO ()
outputOwnerChecks=withDebugger $ \runtime->do
  let opened=addReadOnly "Debugger output" "unrelated snapshot" (initialDesktop (80,25))
      trapped=opened {buffers=M.map (\doc->doc {documentBuffer=error "idle debugger forced unrelated output-labelled Buffer"}) (buffers opened)}
  after<-tickDebugger runtime trapped
  count<-evaluate (sum [maybe (0::Int) (const 1) (documentLabel doc) | doc<-M.elems (buffers after)])
  check "idle debugger leaves unrelated output-labelled buffers untouched" (count==1)
-- Real DAP messages exercise automatic opening, exact refresh, closure and a
-- replacement transport. No callback or prepared-window test seam is exposed.
outputLifecycleCheck :: IO ()
outputLifecycleCheck=bracket (fixture "output-owner") cleanup $ \(port,_,_)->do
  escaped<-withDebugger $ \runtime->do
    let core d _=pure (False,d)
        tick=tickDebugger runtime
        tool name arguments d=do
          (next,finish)<-debuggerTool runtime d name (object arguments)
          result<-finish >>= either (error . T.unpack) pure
          pure (next,result)
        state d=snd <$> tool "debug_status" [] d
        epoch value=fromMaybe (error "missing generation") (field "generation" value) :: Int
        await label predicate d=timeout debuggerTimeout (loop d) >>= maybe (error label) pure
          where loop current=do
                  next<-tick current
                  yes<-predicate next
                  if yes then pure next else threadDelay 1000 >> loop next
        report expected d=state d >>= pure . maybe False (T.isInfixOf expected) . field "output"
        text reference d=maybe "" (\prepared->let value=W.preparedWindowText prepared in contentSlice value 0 (contentLength value)) (M.lookup reference (pluginWindows d))
        opening d=case [reference | w<-windows d,PluginContent reference<-[windowContent w]] of reference:_->Just reference; _->Nothing
        installed d=case opening d of Just reference->pure reference; _->error "missing semantic output"
        emit expected reveal d=do
          statusValue<-state d
          (queued,finish)<-debuggerTool runtime d "debug_inspect" (object ["generation" .= epoch statusValue,"request" .= ("threads"::T.Text)])
          withAsync finish $ \reply->do
            showing<-if not reveal then pure queued else do
              -- The control response follows the burst on the real DAP stream.
              -- Reveal may arrive before its copied report publication; it must
              -- retain that pending payload rather than queue the old report.
              seen<-await "burst control response before reveal" (\_->isJust <$> poll reply) queued
              captured<-epoch <$> state seen
              fst <$> tool "debug_present" ["generation" .= captured,"view" .= ("output"::T.Text)] seen
            delivered<-await "fixture output request" (\current->do
              value<-state current
              check "interleaved status retains only accepted copied report" (maybe False ((<=4*16384).backingBytes) (field "output" value))
              let observed=maybe False (T.isInfixOf expected) (field "output" value)
              done<-poll reply
              pure (observed && isJust done)) showing
            _<-wait reply >>= either (error . T.unpack) pure
            pure delivered
        connect d=snd <$> debuggerEffects runtime core d [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
        modal=Dialog "Human draft" (Searching False "draft") [SelectedInput "Name" "draft" (Selection 0 5)] 0 ["OK"] []
        base=modifyActive (\w->w {selection=Selection 2 7}) (addReadOnly "Debugger output" "unrelated snapshot" (initialDesktop (80,25)))
        sourceId=maybe (error "missing source") sourceFixtureBuffer (activeWindow base)
        sourceWindow=fromMaybe (error "missing source window") (activeWindow base)
        sourceKept d=fmap (contents . documentBuffer) (M.lookup sourceId (buffers d))==Just "unrelated snapshot" &&
          maybe False ((==selection sourceWindow).selection) (findWindow (windowId sourceWindow) d)
    (quiet,_)<-tool "debug_present" ["follow" .= False] base
    attached<-connect quiet {dialog=Just modal}
    deferred<-await "automatic output while modal" (report "session=1 output=1") attached
    check "automatic output preserves protected modal and unrelated same-title source" (dialog deferred==Just modal && opening deferred==Nothing && sourceKept deferred)
    shown<-await "deferred semantic opening" (pure . isJust . opening) deferred {dialog=Nothing}
    first<-installed shown
    check "debugger output has explicit durable semantic identity" (maybe False ((==Just ("hide.debug-output",1)).W.preparedWindowRecovery) (M.lookup first (pluginWindows shown)))
    let background=focusWindow (windowId sourceWindow) shown {dialog=Just modal}
    updatedReport<-emit "session=1 output=2" False background
    refreshed<-await "owned output refresh" (pure . T.isInfixOf "output=2" . text first) updatedReport
    check "refresh retains source focus, selection and modal" (fmap windowId (activeWindow refreshed)==Just (windowId sourceWindow) && dialog refreshed==Just modal && sourceKept refreshed)
    let closing=closeActive (focusWindow (fromMaybe (error "missing output window") (windowId <$> (case [w | w<-windows refreshed,windowContent w==PluginContent first] of w:_->Just w; _->Nothing))) refreshed {dialog=Nothing})
    closed<-tick closing
    -- The command reply may arrive before the final copied report is adopted.
    -- Earlier chunks already satisfy the size cap; wait for this burst's last one.
    afterClose<-emit "session=1 output=3 part=7" False closed
    check "closed output stays closed while MCP retains capped current report" (opening afterClose==Nothing)
    copied<-await "closed output report detaches oversized backing" (\d->do
      value<-state d
      pure (maybe False (\reportText->T.length reportText==16384 && backingBytes reportText<=4*16384) (field "output" value))) afterClose
    capped<-state copied
    check "closed burst advances the capped report through its final chunk" (maybe False (T.isInfixOf "output=3 part=7") (field "output" capped))
    gen<-epoch <$> state copied
    (requested,_)<-tool "debug_present" ["generation" .= gen,"view" .= ("output"::T.Text)] copied
    reopened<-await "explicit output reopen" (pure . isJust . opening) requested
    second<-installed reopened
    oldLive<-W.windowRefCurrent first
    check "explicit reopen has fresh identity and current retained text" (second/=first && not oldLive && "output=3" `T.isInfixOf` text second reopened && sourceKept reopened)
    queuedOld<-emit "session=1 output=4" False reopened {dialog=Just modal}
    let oldSnapshot=M.lookup second (pluginWindows queuedOld)
    replacement<-connect queuedOld
    oldCurrent<-W.windowRefCurrent second
    check "session replacement freezes exact output slot during preparation" (oldCurrent && M.lookup second (pluginWindows replacement)==oldSnapshot)
    secondGeneration<-epoch <$> state replacement
    (revealed,_)<-tool "debug_present" ["generation" .= secondGeneration,"view" .= ("output"::T.Text)] (focusWindow (windowId sourceWindow) replacement {dialog=Nothing})
    focusedReplacement<-await "explicit output focuses prepared replacement slot" (\d->pure (maybe False (\w->case windowContent w of PluginContent reference->reference/=second; _->False) (activeWindow d))) revealed
    newReport<-await "new session report" (report "session=2 output=1") focusedReplacement {dialog=Just modal}
    check "session preparation preserves unrelated human modal" (dialog newReport==Just modal)
    newShown<-await "new session semantic output" (\d->pure (maybe False (\reference->reference/=second && "session=2" `T.isInfixOf` text reference d) (opening d))) newReport {dialog=Nothing}
    third<-installed newShown
    retired<-tickPluginWindows newShown
    let oldWindow=case [w | w<-windows queuedOld,windowContent w==PluginContent second] of w:_->w; _->error "missing old output slot"
        newWindow=case [w | w<-windows retired,windowContent w==PluginContent third] of w:_->w; _->error "missing replacement output slot"
    revoked<-W.windowRefCurrent second
    check "new session replaces same display slot with fresh content lifetime" (third/=second && not revoked && not (M.member second (pluginWindows retired)) && windowId newWindow==windowId oldWindow && bounds newWindow==bounds oldWindow && length (windows retired)==length (windows queuedOld) && "session=2" `T.isInfixOf` text third retired)
    preparingAgain<-connect retired
    let closingFrozen=closeActive (focusWindow (windowId newWindow) preparingAgain)
    finalReport<-await "third session output after frozen slot closes" (report "session=3 output=1") closingFrozen
    frozenClosed<-await "closed frozen capability retired" (\_->not <$> W.windowRefCurrent third) finalReport
    check "closing frozen slot during replacement never resurrects it" (opening frozenClosed==Nothing && sourceKept frozenClosed)
    secondReport<-emit "session=3 output=2" False frozenClosed
    explicitAgain<-emit "session=3 output=3 part=7" True secondReport
    finalShown<-await "deliberate reopen retains pending burst" (\d->pure (maybe False (\reference->"output=3 part=7" `T.isInfixOf` text reference d) (opening d))) explicitAgain
    fourth<-installed finalShown
    check "deliberate reopen after frozen close cannot reuse old capability" (fourth/=third && "session=3" `T.isInfixOf` text fourth finalShown)
    pure fourth
  live<-W.windowRefCurrent escaped
  check "debugger scope shutdown retires output publication lifetime" (not live)
  where
    backingBytes (TI.Text (ByteArray bytes) _ _)=I# (sizeofByteArray# bytes)
    findWindow ident d=case [w | w<-windows d,windowId w==ident] of w:_->Just w; _->Nothing

checks :: IO ()
checks = outputOwnerChecks >> outputLifecycleCheck >> terminalLauncherCheck >> terminalCheck >> completionChecks >> presentationCheck >> pendingPresentationCheck >> startupDeadlineCheck >> launchDeadlineCheck >> launchChecks >> mapM_ session ["basic", "frame", "choices", "breakpoints", "reconnect", "mcp", "lazy", "exception"] >> putStrLn "Debugger checks passed"
  where
    session mode = bracket (fixture mode) cleanup $ \(port,logPath,process) -> withDebugger $ \runtime -> do
      let core d _=pure (False,d)
          send action values d=snd <$> debuggerEffects runtime core d [DebugAction action values]
          tick=tickDebugger runtime
          waitForIO label predicate d=do
            result<-timeout debuggerTimeout (loop d)
            maybe (error (label++": timed out")) pure result
            where loop state=do
                    updated<-tick state
                    done<-predicate updated
                    if done then pure updated else threadDelay 1000 >> loop updated
          waitFor label predicate=waitForIO label (pure . predicate)
          choose index d=case dialog d of
            Just dg -> let adjusted=dg {fields=map (\f -> case f of ListBox title values _ -> ListBox title values index; _ -> f) (fields dg)}
                           (next,effects)=submitDialog 0 adjusted d
                       in snd <$> debuggerEffects runtime core next effects
            _ -> error "missing debugger dialog"
          connect=send "connect" ["0","127.0.0.1",T.pack port]
          logEntries=map (fromMaybe (error "invalid fixture log") . decodeStrictText) . T.lines <$> TIO.readFile logPath
          commands=map (fromMaybe Null . field "request") <$> logEntries
          finish d=do
            disconnected<-send "disconnect" [] d >>= waitFor "disconnect response" (T.isInfixOf "Debugger disconnected" . status)
            check "disconnect clears transient inspection" (dialog disconnected==Nothing)
            ended<-timeout 2000000 (waitForProcess process)
            check "fixture exits after disconnect" (ended==Just ExitSuccess)
            requests<-commands
            check "disconnect request actually reaches adapter without terminating program"
              (any (\r -> field "command" r==Just ("disconnect"::T.Text) && (field "arguments" r >>= field "terminateDebuggee")==Just False) requests)
            check "inspection never evaluates or sets values"
              (all (\r -> (field "command" r :: Maybe T.Text) `notElem` [Just "evaluate",Just "setVariable"]) requests)
      attached<-connect (initialDesktop (80,25))
      if mode=="frame" then do
        stopped<-waitFor "initial stop" (T.isInfixOf "Stopped" . status) attached
        stack<-send "stack" [] stopped >>= waitFor "frame chooser" (hasDialog "Call stack")
        (_,beforeReply)<-debuggerTool runtime stack "debug_status" (object [])
        before<-beforeReply
        waiting<-send "scopes" [] stack
        chosen<-choose 1 waiting
        (_,afterReply)<-debuggerTool runtime chosen "debug_status" (object [])
        after<-afterReply
        check "frame selection preserves stopped handles"
          ((before >>= maybe (Left "missing epoch") Right . (field "generation" :: Value -> Maybe Int))==
           (after >>= maybe (Left "missing epoch") Right . (field "generation" :: Value -> Maybe Int)))
        -- This response follows the current source and both delayed old replies.
        drained<-send "threads" [] chosen >>= waitFor "stale frame replies must not open scopes" (hasDialog "Threads")
        check "selected source waits while the ordered threads modal owns input" (hasDialog "Threads" drained)
        shown<-waitFor "selected frame source after modal dismissal" ((=="chosen frame source\n").activeText) drained {dialog=Nothing}
        check "late source cannot replace selected frame" (activeText shown=="chosen frame source\n")
        check "late source does not create an obsolete buffer" (not (any (T.isInfixOf "STALE" . contents . documentBuffer) (M.elems (buffers shown))))
        finish shown
      else do
        stopped<-waitFor "initial source" (T.isInfixOf "value = λ" . activeText) attached
        check "sourceReference opens read-only at adapter line" (maybe False ((/=Nothing).documentLabel) (activeDocument stopped) && fmap (caret.selection) (activeWindow stopped)==Just (T.length "module Generated where\n"))
        colored<-withHighlighting $ \highlighting -> do
          let awaitColors d=do
                next<-tickHighlighting highlighting d
                if maybe False (isJust.documentSourceRows) (activeDocument next) then pure next
                  else threadDelay 10000 >> awaitColors next
          timeout 2000000 (awaitColors stopped) >>= maybe (error "embedded source highlighting timed out") pure
        check "embedded debugger source renders syntax colors"
          ("color:rgb(255,255,255);background:rgb(0,0,170)'>where" `T.isInfixOf` snapshotHtml colored)
        initialRequests<-commands
        check "variables are not fetched automatically" (not (any ((==Just ("variables"::T.Text)) . field "command") initialRequests))
        final<-case mode of
          "basic" -> do
            scopes<-send "scopes" [] stopped >>= waitFor "scopes" (hasDialog "Scopes")
            variables<-choose 0 scopes >>= waitFor "variables" (hasDialog "Variables")
            check "thunk state is displayed without evaluation" (any (T.isInfixOf "<thunk>") (rows variables))
            expanding<-choose 0 variables
            before<-length . filter ((==Just ("threads"::T.Text)) . field "command") <$> commands
            resumed<-send "continue" [] expanding
            after<-waitForIO "resume barrier" (\_ -> (>before) . length . filter ((==Just ("threads"::T.Text)) . field "command") <$> commands) resumed
            check "late variables cannot reopen inspection after resume" (dialog after==Nothing)
            send "pause" [] after >>= waitFor "pause" (T.isInfixOf "Stopped" . status)
          "lazy" -> do
            scopes<-send "scopes" [] stopped >>= waitFor "scopes" (hasDialog "Scopes")
            variables<-choose 0 scopes >>= waitFor "variables" (hasDialog "Variables")
            blocked<-choose 0 variables
            check "ordinary UI expansion does not force a lazy variable"
              ("explicit" `T.isInfixOf` status blocked)
            let current d=do (_,result)<-debuggerTool runtime d "debug_status" (object []); result >>= either (error . T.unpack) pure
                reject ref gen d=do
                  (_,result)<-debuggerTool runtime d "debug_inspect" (object
                    ["generation" .= (gen::Int),"request" .= ("variables"::T.Text),"variablesReference" .= (ref::Int)])
                  answer<-result
                  check "read-only inspection rejects forcing or unobserved references" (either (const True) (const False) answer)
            before<-current blocked
            let gen=fromMaybe (error "missing generation") (field "generation" before)::Int
            reject 22 gen blocked
            reject 999 gen blocked
            let watchAction action d=snd <$> debuggerEffects runtime core d [DebugSidebarAction action]
                fill expression d=case dialog d of
                  Just dg->d {dialog=Just dg {fields=[SelectedInput "Expression" expression (Selection 2 5)]}}
                  _->error "missing persistent watch editor"
                draft d=case dialog d of
                  Just dg | DebuggerWatchDialog ident origin private<-purpose dg,
                    [SelectedInput _ expression selection]<-fields dg->Just (ident,origin,private,expression,selection)
                  _->Nothing
                saveOnce expected revision d=case dialog d of
                  Just dg->do
                    let (next,outbox)=submitDialog 0 dg d
                    saved<-snd <$> debuggerEffects runtime core next outbox
                    replayed<-snd <$> debuggerEffects runtime core saved outbox
                    (_,_,values)<-debuggerWatches runtime
                    check "watch draft saves once; replay cannot duplicate or revise it"
                      (case M.toList values of
                        [(_,entry)]->watchExpression entry==expected && watchRevision entry==revision
                        _->False)
                    pure replayed
                  _->error "watch invalidation discarded the draft"
            -- Initial attach and stopped events each request threads. This third
            -- request invalidates variable handles while a real Add Watch owns input.
            adding<-watchAction AddDebugWatch blocked {dialog=Nothing}
            let addDraft=fill "counter + λ" adding
            pending<-send "threads" [] addDraft
            invalidated<-waitForIO "variables invalidation" (\d -> maybe False (>gen) . field "generation" <$> current d) pending
            check "variables invalidation preserves Add Watch draft, selection and authority"
              (draft invalidated==draft addDraft && draft invalidated/=Nothing)
            now<-current invalidated
            let gen'=fromMaybe (error "missing generation") (field "generation" now)::Int
            check "variable invalidation preserves stopped source frame" (field "stopped" now==Just True && (field "frame" now::Maybe Value)==field "frame" before)
            reject 21 gen' invalidated
            saved<-saveOnce "counter + λ" 0 invalidated
            (_,_,values)<-debuggerWatches runtime
            let (watchId,entry)=case M.toList values of [watch]->watch; _->error "expected one saved debugger watch"
            editing<-watchAction (EditDebugWatch watchId (watchRevision entry)) saved
            let editDraft=fill "counter + 2" editing
            pendingThreads<-send "threads" [] editDraft
            retired<-waitForIO "thread invalidation" (\d -> maybe False (>gen') . field "generation" <$> current d) pendingThreads
            check "thread invalidation preserves Edit Watch draft, selection and authority"
              (draft retired==draft editDraft && draft retired/=Nothing)
            committed<-saveOnce "counter + 2" 1 retired
            refreshed<-waitFor "thread invalidation refreshes selected source" (T.isPrefixOf "Stopped in " . status) committed
            refreshedState<-current refreshed
            check "thread invalidation reloads current stopped frame"
              (field "stopped" refreshedState==Just True && field "threadId" refreshedState==Just (7::Int) && (field "frame" refreshedState::Maybe Value)==field "frame" before)
            requests<-commands
            check "only non-forcing scope variables reached the adapter"
              ([field "variablesReference" args::Maybe Int | req<-requests,field "command" req==Just ("variables"::T.Text),Just args<-[field "arguments" req]]==[Just 21])
            pure refreshed
          "choices" -> do
            scopes<-send "scopes" [] stopped >>= waitFor "first scope picker" (hasDialog "Scopes")
            replaced<-send "scopes" [] scopes >>= waitFor "second scopes reply" (T.isInfixOf "Scopes ready" . status)
            check "existing picker keeps visible rows" (rows replaced==["Locals"])
            expanded<-choose 0 replaced >>= waitFor "original scope expansion" (hasDialog "Variables")
            check "visible scope selects its original reference" (any (T.isInfixOf "<thunk>") (rows expanded) && not (any (T.isInfixOf "WRONG_SCOPE") (rows expanded)))
            pure expanded
          "breakpoints" -> do
            first<-send "breakpoint" [] stopped
            second<-send "breakpoint" [] (moveTo False 0 first)
            third<-send "breakpoint" [] second
            drained<-send "threads" [] third >>= waitFor "breakpoint reply barrier" (hasDialog "Threads")
            shown<-send "breakpoints" [] drained {dialog=Nothing}
            check "latest breakpoint result wins even when an old requested list reappears"
              (rows shown==["Generated.hs:2 verified at 202"])
            requests<-commands
            let changes=[fromMaybe [] (field "breakpoints" args) | req<-requests, field "command" req==Just ("setBreakpoints"::T.Text),Just args<-[field "arguments" req]] :: [[Value]]
            check "fixture exercised different snapshots and repeated original list" (map (map (field "line")) changes==[[Just (2::Int)],[Just 2,Just 1],[Just 2]])
            pure shown
          "exception" -> do
            requested<-send "exception-info" [] stopped
            shown<-waitFor "exception details" (T.isInfixOf "nested cause" . activeText) requested
            check "exception view includes identity, message and source stack"
              (all (`T.isInfixOf` activeText shown) ["IOException","cannot open λ.hs","permission denied","Main.hs:6","always"])
            pure shown
          "mcp" -> do
            let tool name args desktop=do
                  (updated,result)<-debuggerTool runtime desktop name (object args)
                  value<-result
                  pure (updated,value)
                success name args desktop=do
                  (updated,result)<-tool name args desktop
                  either (error . T.unpack) (pure . (updated,)) result
                rejected name args desktop=do
                  (updated,result)<-tool name args desktop
                  check ("MCP rejects "<>T.unpack name) (either (const True) (const False) result && updated==desktop)
                current desktop=snd <$> success "debug_status" [] desktop
                epoch value=fromMaybe (error "missing debugger generation") (field "generation" value) :: Int
                inspect args desktop=do
                  (updated,result)<-debuggerTool runtime desktop "debug_inspect" (object args)
                  withAsync result $ \pending -> do
                    drained<-waitForIO "MCP inspection reply" (\_ -> isJust <$> poll pending) updated
                    value<-wait pending >>= either (error . T.unpack) pure
                    check "MCP inspection leaves UI unchanged" (drained==desktop)
                    pure value
            snapshot<-current stopped
            let gen=epoch snapshot
                virtualId=maybe (error "missing debugger source buffer") sourceFixtureBuffer (activeWindow stopped)
            check "MCP reports connected, stopped selection" (field "connected" snapshot==Just True && field "stopped" snapshot==Just True && field "threadId" snapshot==Just (7::Int))
            rejected "debug_attach" ["port" .= (read port::Int)] stopped
            rejected "debug_launch" [] stopped
            rejected "debug_control" ["generation" .= (gen-1),"command" .= ("continue"::T.Text)] stopped
            rejected "debug_control" ["generation" .= gen,"command" .= ("pause"::T.Text)] stopped
            rejected "debug_inspect" ["generation" .= gen,"request" .= ("evaluate"::T.Text)] stopped
            rejected "debug_inspect" ["generation" .= gen,"request" .= ("variables"::T.Text),"variablesReference" .= (0::Int)] stopped
            rejected "debug_inspect" ["generation" .= gen,"request" .= ("threads"::T.Text),"count" .= (1001::Int)] stopped
            rejected "debug_status" ["unknown" .= True] stopped
            rejected "debug_set_breakpoints" ["generation" .= gen,"bufferId" .= virtualId,"lines" .= ([0]::[Int])] stopped
            rejected "debug_set_breakpoints" ["generation" .= gen,"bufferId" .= virtualId,"lines" .= replicate 1001 (1::Int)] stopped
            rejected "debug_set_breakpoints" ["generation" .= gen,"bufferId" .= (999999::Int),"lines" .= ([]::[Int])] stopped
            scopesResult<-inspect ["generation" .= gen,"request" .= ("scopes"::T.Text)] stopped
            check "MCP receives scope body" ((field "body" scopesResult >>= field "scopes" :: Maybe [Value])/=Nothing)
            vars<-inspect ["generation" .= gen,"request" .= ("variables"::T.Text),"variablesReference" .= (21::Int),"start" .= (0::Int),"count" .= (3::Int)] stopped
            check "MCP returns non-evaluating variable inspection" ("<thunk>" `T.isInfixOf` T.pack (show vars))
            _<-inspect ["generation" .= gen,"request" .= ("stackTrace"::T.Text)] stopped
            exceptionResult<-inspect ["generation" .= gen,"request" .= ("exceptionInfo"::T.Text)] stopped
            check "MCP preserves structured exception details"
              ((field "body" exceptionResult >>= field "exceptionId" :: Maybe T.Text)==Just "IOException")
            sourceResult<-inspect ["generation" .= gen,"request" .= ("source"::T.Text)] stopped
            check "MCP source reply uses selected sourceReference" ((field "body" sourceResult >>= field "content" :: Maybe T.Text)==Just (activeText stopped))
            let local=addDocument (Just (FileState (logPath<>".hs") Nothing)) (replaceBuffer False "dirty = 2\n" (newBuffer "local = 1\n")) stopped
                localId=maybe (error "missing local buffer") sourceFixtureBuffer (activeWindow local)
                points bid requested=["generation" .= gen,"bufferId" .= bid,"lines" .= (requested::[Int])]
            (virtual,_)<-success "debug_set_breakpoints" (points virtualId [2,1,2]) local
            check "MCP breakpoint changes do not select their buffer" (virtual==local)
            (dirtyLocal,_)<-success "debug_set_breakpoints" (points localId [1]) virtual
            (same,_)<-success "debug_set_breakpoints" (points localId [1]) dirtyLocal
            _<-inspect ["generation" .= gen,"request" .= ("threads"::T.Text)] same
            requests<-commands
            let changes=[args | req<-requests,field "command" req==Just ("setBreakpoints"::T.Text),Just args<-[field "arguments" req]]
            check "MCP breakpoint replacement sorts and deduplicates, remains idempotent, and marks dirty source"
              (case changes of [virtualChange,localChange] -> (field "breakpoints" virtualChange :: Maybe [Value])==Just [object ["line" .= (1::Int)],object ["line" .= (2::Int)]] && field "sourceModified" localChange==Just True; _ -> False)
            verified<-current same
            let bps=fromMaybe [] (field "breakpoints" verified) :: [Value]
            check "MCP status exposes verified breakpoints" (length bps==3 && all ((==Just True) . field "verified") bps)
            before<-length . filter ((==Just ("threads"::T.Text)) . field "command") <$> commands
            (waiting,pending)<-debuggerTool runtime same "debug_inspect" (object ["generation" .= gen,"request" .= ("variables"::T.Text),"variablesReference" .= (22::Int)])
            (resumed,accepted)<-success "debug_control" ["generation" .= gen,"command" .= ("continue"::T.Text)] waiting
            check "MCP control advances generation" (epoch accepted>gen)
            expired<-timeout 1000000 pending
            check "MCP pending inspection expires promptly on resume without ticking" (case expired of Just (Left _) -> True; _ -> False)
            rejected "debug_inspect" ["generation" .= gen,"request" .= ("scopes"::T.Text)] resumed
            drained<-waitForIO "MCP stale reply barrier" (\_ -> (>before) . length . filter ((==Just ("threads"::T.Text)) . field "command") <$> commands) resumed
            check "MCP stale variables never open a picker" (dialog drained==Nothing)
            send "pause" [] drained >>= waitFor "MCP pause" (T.isInfixOf "Stopped" . status)
          "reconnect" -> do
            virtual<-send "breakpoint" [] stopped
            let local=addDocument (Just (FileState (logPath<>".hs") Nothing)) (newBuffer "local = 1\n") virtual
            both<-send "breakpoint" [] local
            drained<-send "threads" [] both >>= waitFor "breakpoints delivered before reconnect" (hasDialog "Threads")
            reattached<-connect drained >>= waitFor "second session source" (T.isInfixOf "session = 2" . activeText)
            entries<-logEntries
            let configured :: Int -> [Value]
                configured n=[args | entry<-entries,field "session" entry==Just (n::Int),Just req<-[field "request" entry],field "command" req==Just ("setBreakpoints"::T.Text),Just args<-[field "arguments" req]]
            check "first session installed adapter-owned source breakpoint" (any (\args -> (field "source" args >>= field "sourceReference")==Just (9::Int)) (configured 1))
            check "reconnect retains only file breakpoints" (length (configured 2)==1 && all (\args -> (field "source" args >>= field "path" :: Maybe T.Text)/=Nothing && (field "source" args >>= field "sourceReference" :: Maybe Int)==Nothing) (configured 2))
            pure reattached
          _ -> error "unknown fixture mode"
        finish final

richBreakpointChecks :: IO ()
richBreakpointChecks=duplicateBreakpointProvenanceCheck >> richBreakpointFixtureChecks

richBreakpointFixtureChecks :: IO ()
richBreakpointFixtureChecks=bracket (fixture "rich-breakpoints") cleanup $ \(port,path,_) ->
  bracket (BL.writeFile (path<>".rich.json") (encode (object ["host" .= ("127.0.0.1"::T.Text),"port" .= (read port::Int),"adapterId" .= ("graalvm"::T.Text),"request" .= ("attach"::T.Text)])))
    (const (removeFile (path<>".rich.json"))) $ \_->withDebugger $ \runtime->do
  let core desktop _=pure (False,desktop)
      send action values desktop=snd <$> debuggerEffects runtime core desktop [DebugAction action values]
      tick=tickDebugger runtime
      await label predicate desktop=timeout debuggerTimeout (loop desktop) >>= maybe (error label) pure
        where loop current=do
                next<-tick current
                ready<-predicate next
                if ready then pure next else threadDelay 1000 >> loop next
      tool name args desktop=do
        (next,finish)<-debuggerTool runtime desktop name (object args)
        value<-finish >>= either (error . T.unpack) pure
        pure (next,value)
      current desktop=snd <$> tool "debug_status" [] desktop
      generationOf value=fromMaybe (error "missing generation") (field "generation" value)::Int
      points value=fromMaybe [] (field "breakpoints" value)::[Value]
      row :: Int -> Value -> Value
      row n value=case filter ((==Just n).field "line") (points value) of p:_->p; _->error "missing breakpoint"
      check label ok=unless ok (error label)
      withheld answer=case answer of
        Right value->field "sourceMetadataUnavailable" value==Just ("Source privacy context changed; refresh status."::T.Text) &&
          all (\key->(field key value::Maybe Value)==Nothing) ["breakpoints","frame","source","output","assistance","error","capabilities"]
        _->False
      barrier desktop=do
        epoch<-generationOf <$> current desktop
        (queued,finish)<-debuggerTool runtime desktop "debug_inspect" (object ["generation" .= epoch,"request" .= ("threads"::T.Text)])
        withAsync finish $ \reply->do
          done<-await "breakpoint stream receipt" (\_->isJust <$> poll reply) queued
          _<-wait reply >>= either (error . T.unpack) pure
          pure done
      connect=send "connect" ["0","127.0.0.1",T.pack port]
      rich condition=object ["line" .= (1::Int),"enabled" .= True,"condition" .= (condition::T.Text),"hitCondition" .= ("3"::T.Text),"logMessage" .= ("counter={counter}"::T.Text)]
      edit desktop=do
        listed<-send "breakpoints" [] desktop {dialog=Nothing}
        case dialog listed of
          Nothing->error "missing breakpoint list"
          Just picker->let chosen=picker {fields=map (\field->case field of ListBox title choices _->ListBox title choices (max 0 (length choices-1)); _->field) (fields picker)}
                           (next,effects)=submitDialog 0 chosen listed in snd <$> debuggerEffects runtime core next effects
      save dg desktop=let (next,effects)=submitDialog 0 dg desktop in snd <$> debuggerEffects runtime core next effects
  attached<-send "launch-config" ["1",T.pack (path<>".rich.json")] (initialDesktop (80,25))
  stopped<-await "rich initial source" (pure . T.isInfixOf "value = λ" . activeText) attached
  let virtualId=maybe (error "missing virtual source") sourceFixtureBuffer (activeWindow stopped)
      hidden=stopped {buffers=M.adjust (\doc->doc {documentPrivate=True}) virtualId (buffers stopped)}
  virtualPoint<-send "breakpoint" [] hidden
  virtualEditor<-edit virtualPoint
  virtualDialog<-maybe (error "missing private breakpoint editor") pure (dialog virtualEditor)
  let secretDialog=virtualDialog {fields=map (\field->case field of Input "Condition" _ _->Input "Condition" "PRIVATE_FIXTURE_CONDITION" 0; _->field) (fields virtualDialog)}
  privateStored<-save secretDialog virtualEditor >>= barrier
  check "adapter breakpoint rejection never echoes private expressions" (status privateStored=="Debugger rejected source breakpoints; review their settings.")
  let sourceWindow=maybe (error "missing private source window") windowId (activeWindow stopped)
      (closed,_) = Hide.Model.runCommand Close (focusWindow sourceWindow privateStored)
  closedStatus<-current closed
  check "closed private adapter-only source never reveals stored expressions"
    (M.notMember virtualId (buffers closed) && null (points closedStatus))
  canonical<-canonicalizePath (path<>".hs")
  let local=addDocument (Just (FileState canonical Nothing)) (newBuffer "one\ntwo\n") closed
      bid=maybe (error "missing source") sourceFixtureBuffer (activeWindow local)
  epoch<-generationOf <$> current local
  (unchanged,malformed)<-debuggerTool runtime local "debug_configure_breakpoints" (object ["generation" .= epoch,"bufferId" .= bid,"breakpoints" .= [object ["line" .= (1::Int),"condition" .= Null]]])
  malformedResult<-malformed
  malformedStatus<-current unchanged
  check "malformed rich fields cannot silently become unconditional" (unchanged==local && null (points malformedStatus) && either (const True) (const False) malformedResult)
  (_,rejectedLog)<-debuggerTool runtime local "debug_configure_breakpoints" (object ["generation" .= epoch,"bufferId" .= bid,"breakpoints" .= [object ["line" .= (1::Int),"format" .= ("not-supported"::T.Text)]]])
  logResult<-rejectedLog
  check "unknown breakpoint fields are rejected before mutation" (either (const True) (const False) logResult)
  (stored,_)<-tool "debug_configure_breakpoints" ["generation" .= epoch,"bufferId" .= bid,"breakpoints" .= [rich "counter > 1"]] local
  (invalid,invalidStatus)<-tool "debug_configure_breakpoints" ["generation" .= epoch,"bufferId" .= bid,"breakpoints" .= [object ["line" .= (1::Int),"hitCondition" .= ("INVALID_FIXTURE_HIT"::T.Text)]]] stored
  invalidDrained<-barrier invalid
  invalidLines<-map (fromMaybe (error "invalid breakpoint log") . decodeStrictText) . T.lines <$> TIO.readFile path
  let invalidSets=[args | entry<-invalidLines,Just req<-[field "request" entry],field "command" req==Just ("setBreakpoints"::T.Text),Just args<-[field "arguments" req],(field "source" args >>= field "path"::Maybe T.Text)==Just (T.pack canonical)]
  check "invalid known-backend hit condition stays unavailable and never installs unconditional"
    (maybe False (T.isInfixOf "positive count") (field "unavailable" (row 1 invalidStatus)) &&
     not ("INVALID_FIXTURE_HIT" `T.isInfixOf` fromMaybe "" (field "unavailable" (row 1 invalidStatus))) &&
     case reverse invalidSets of args:_->(field "breakpoints" args::Maybe[Value])==Just []; _->False)
  (orderedOld,_)<-tool "debug_configure_breakpoints" ["generation" .= epoch,"bufferId" .= bid,"breakpoints" .= [rich "ORDER_OLD"]] invalidDrained
  (orderedNew,_)<-tool "debug_configure_breakpoints" ["generation" .= epoch,"bufferId" .= bid,"breakpoints" .= [rich "ORDER_NEW"]] orderedOld
  ordered<-barrier orderedNew
  orderedStatus<-current ordered
  check "new expression on the same line owns reversed source-set replies"
    (field "condition" (row 1 orderedStatus)==Just ("ORDER_NEW"::T.Text) &&
     (field "result" (row 1 orderedStatus) >>= field "line")==Just (202::Int))
  (restored,_)<-tool "debug_configure_breakpoints" ["generation" .= epoch,"bufferId" .= bid,"breakpoints" .= [rich "counter > 1"]] ordered
  (replaced,_)<-tool "debug_set_breakpoints" ["generation" .= epoch,"bufferId" .= bid,"lines" .= ([1,2]::[Int])] restored
  changed<-send "breakpoint" [] (moveTo False 4 replaced)
  ordinary<-current changed
  check "integer replacement and ordinary toggle retain rich metadata"
    (length (points ordinary)==1 && field "condition" (row 1 ordinary)==Just ("counter > 1"::T.Text))
  disabled<-send "breakpoint" [] (moveTo False 0 changed)
  disabledStatus<-current disabled
  check "rich toggle disables without erasing metadata" (field "enabled" (row 1 disabledStatus)==Just False && field "pending" (row 1 disabledStatus)==Just True && field "logMessage" (row 1 disabledStatus)==Just ("counter={counter}"::T.Text))
  (_,oldGrant)<-debuggerTool runtime disabled "debug_set_breakpoints" (object ["generation" .= epoch,"bufferId" .= bid,"breakpoints" .= [rich "counter > 99"]])
  oldRejected<-oldGrant
  check "old line-tool grant cannot introduce executable expressions" (either (const True) (const False) oldRejected)
  (stillDisabled,_)<-tool "debug_set_breakpoints" ["generation" .= epoch,"bufferId" .= bid,"lines" .= ([1]::[Int])] disabled
  preserved<-current stillDisabled
  check "old line-tool grant cannot enable a disabled executable breakpoint" (field "enabled" (row 1 preserved)==Just False)
  cleared<-barrier stillDisabled
  clearedStatus<-current cleared
  check "disabled source-set pending state ends only after actual adapter reply"
    (field "pending" (row 1 clearedStatus)==Just False && field "result" (row 1 clearedStatus)==Just Null)
  enabled<-send "breakpoint" [] cleared >>= barrier
  enabledStatus<-current enabled
  check "rich toggle re-enables and receives actual verification" (field "enabled" (row 1 enabledStatus)==Just True && field "verified" (row 1 enabledStatus)==Just True)
  editing<-edit enabled
  dg<-maybe (error "missing breakpoint editor") pure (dialog editing)
  deniedInput<-P.applyGuestInput (P.Key "Enter" []) editing
  check "breakpoint editing is human-only and private fields follow source policy"
    (either (const True) (const False) deniedInput && not (guestEffectsAllowed [DebugAction "breakpoint-edit:1" []]) && not (guestEffectsAllowed [DebugAction "breakpoint" []]) && not (guestCommandAllowed (DebugCommand "breakpoint")) && all (privateDialogField editing {guestPrivatePaths=[canonical]} dg) [f | f@Input{}<-fields dg] &&
     all (privateDialogField editing {buffers=M.adjust (\doc->doc {documentPrivate=True}) bid (buffers editing)} dg) [f | f@Input{}<-fields dg])
  let oversized=dg {fields=map (\field->case field of Input "Condition" _ _->Input "Condition" (T.replicate 4097 "x") 0; _->field) (fields dg)}
  retained<-save oversized editing
  retry<-maybe (error "invalid breakpoint edit discarded its draft") pure (dialog retained)
  check "invalid breakpoint editor retains entered text"
    (any (\field->case field of Input "Condition" value _->T.length value==4097; _->False) (fields retry))
  let updated=retry {fields=map (\f->case f of Input "Condition" _ _->Input "Condition" "counter > 2" 11; _->f) (fields retry)}
  edited<-save updated retained >>= barrier
  editedStatus<-current edited
  check "human editor updates the shared DAP record" (field "condition" (row 1 editedStatus)==Just ("counter > 2"::T.Text))
  stale<-edit edited
  oldDialog<-maybe (error "missing stale editor") pure (dialog stale)
  (newer,_)<-tool "debug_configure_breakpoints" ["generation" .= epoch,"bufferId" .= bid,"breakpoints" .= [rich "counter > 3"]] stale
  refused<-save oldDialog newer
  refusedStatus<-current refused
  retryStale<-maybe (error "stale breakpoint editor discarded its draft") pure (dialog refused)
  check "late human save preserves newer state and entered draft" (field "condition" (row 1 refusedStatus)==Just ("counter > 3"::T.Text) && "Breakpoint changed" `T.isPrefixOf` status refused &&
    any (\field->case field of Input "Condition" value _->value=="counter > 2"; _->False) (fields retryStale))
  (_,alreadyTainted)<-debuggerTool runtime refused "debug_status" (object [])
  (_,privateStatus)<-tool "debug_status" [] refused {guestPrivatePaths=[canonical]}
  secondPrivacy<-alreadyTainted
  check "already-tainted output cannot release another newly private source snapshot"
    (withheld secondPrivacy)
  check "private breakpoint expressions do not appear in MCP readback" (null (points privateStatus))
  let dismissed=fst (submitDialog 1 retryStale refused)
  disabledRestart<-send "breakpoint" [] (moveTo False 0 dismissed) >>= barrier
  reconnected<-connect disabledRestart >>= await "rich second session" (pure . T.isInfixOf "session = 2" . activeText)
  unavailable<-current reconnected
  check "restart preserves disabled metadata and exposes unsupported capability" (field "enabled" (row 1 unavailable)==Just False && field "condition" (row 1 unavailable)==Just ("counter > 3"::T.Text) && maybe False (T.isInfixOf "does not support") (field "unavailable" (row 1 unavailable)))
  entries<-map (fromMaybe (error "invalid rich fixture log") . decodeStrictText) . T.lines <$> TIO.readFile path
  let installed=[args | entry<-entries,field "session" entry==Just (2::Int),Just req<-[field "request" entry],field "command" req==Just ("setBreakpoints"::T.Text),Just args<-[field "arguments" req]]
      requested=[point | entry<-entries,Just req<-[field "request" entry],field "command" req==Just ("setBreakpoints"::T.Text),Just args<-[field "arguments" req],point<-fromMaybe [] (field "breakpoints" args)]::[Value]
  check "rich fields reach actual DAP and unsupported breakpoint never becomes unconditional"
    (any (\p->field "condition" p==Just ("counter > 2"::T.Text) && field "hitCondition" p==Just ("3"::T.Text) && field "logMessage" p==Just ("counter={counter}"::T.Text)) requested && not (null installed) && all ((==Just ([]::[Value])).field "breakpoints") installed)
  currentEpoch<-generationOf <$> current reconnected
  (genericDraft,_)<-tool "debug_configure_breakpoints" ["generation" .= currentEpoch,"bufferId" .= bid,"breakpoints" .= [object ["line" .= (1::Int),"hitCondition" .= ("adapter-defined-count"::T.Text)]]] reconnected
  BL.writeFile (path<>".rich.json") (encode (object ["host" .= ("127.0.0.1"::T.Text),"port" .= (read port::Int),"adapterId" .= ("custom"::T.Text),"request" .= ("attach"::T.Text)]))
  (_,retiringOutput)<-debuggerTool runtime genericDraft "debug_status" (object [])
  generic<-send "launch-config" ["1",T.pack (path<>".rich.json")] genericDraft >>= await "generic third session" (pure . T.isInfixOf "session = 3" . activeText) >>= barrier
  oldOutput<-retiringOutput
  check "replacement retires already-admitted old output" (withheld oldOutput)
  genericStatus<-current generic
  genericEntries<-map (fromMaybe (error "invalid generic breakpoint log") . decodeStrictText) . T.lines <$> TIO.readFile path
  let genericPoints=[point | entry<-genericEntries,field "session" entry==Just (3::Int),Just req<-[field "request" entry],field "command" req==Just ("setBreakpoints"::T.Text),Just args<-[field "arguments" req],point<-fromMaybe [] (field "breakpoints" args)]::[Value]
  check "generic adapters retain their own hit-condition grammar"
    (field "unavailable" (row 1 genericStatus)==Just Null && any ((==Just ("adapter-defined-count"::T.Text)).field "hitCondition") genericPoints)
  idle<-tick generic {buffers=M.adjust (\doc->doc {documentBuffer=error "breakpoint privacy forced immutable source payload"}) bid (buffers generic)}
  idleStatus<-current idle
  check "rich-source idle privacy checks inspect metadata without forcing payloads" (generationOf idleStatus==generationOf genericStatus)
  outputReady<-await "rich output on actual DAP stream" (\desktop->current desktop >>= pure . maybe False (T.isInfixOf "RICH_OUTPUT session=3") . field "output") generic
  (_,publicReceipt)<-debuggerTool runtime outputReady "debug_set_breakpoints" (object ["generation" .= generationOf genericStatus,"bufferId" .= bid,"lines" .= ([1]::[Int])])
  publicAnswer<-publicReceipt >>= either (error . T.unpack) pure
  check "ordinary public mutation keeps its accepted receipt and full metadata"
    (field "accepted" publicAnswer==Just True && (field "sourceMetadataUnavailable" publicAnswer::Maybe T.Text)==Nothing && not (null (points publicAnswer)))
  (_,mutationReceipt)<-debuggerTool runtime outputReady "debug_set_breakpoints" (object ["generation" .= generationOf genericStatus,"bufferId" .= bid,"lines" .= ([1]::[Int])])
  (_,inFlight)<-debuggerTool runtime outputReady "debug_status" (object [])
  (removed,_)<-tool "debug_set_breakpoints" ["generation" .= generationOf genericStatus,"bufferId" .= bid,"lines" .= ([]::[Int])] outputReady
  drained<-barrier removed
  privateOutput<-tick drained {guestPrivatePaths=[canonical]}
  late<-inFlight
  mutationAnswer<-mutationReceipt
  check "privacy withholding preserves actual mutation acceptance and generation"
    (withheld mutationAnswer && (mutationAnswer >>= maybe (Left "missing acceptance") Right . field "accepted")==Right True &&
     (mutationAnswer >>= maybe (Left "missing generation") Right . field "generation")==Right (generationOf genericStatus))
  privateRead<-current privateOutput
  check "retained output tracks removed source privacy and protects in-flight status"
    (withheld late && field "output" privateRead==Just (""::T.Text) &&
     field "outputUnavailable" privateRead==Just ("Debugger output is private or retired."::T.Text) &&
     generationOf privateRead==generationOf genericStatus && field "active" privateRead==Just True)
  publicAgain<-current privateOutput {guestPrivatePaths=[]}
  check "source becoming public cannot downgrade retained output privacy" (field "output" publicAgain==Just (""::T.Text))
  withSystemOne $ \owner->withDebuggerSystemOne (systemOneServices owner) runtime $ do
    (assisting,admittedAssist)<-tool "debug_assist" ["command" .= ("start"::T.Text),"generation" .= generationOf genericStatus,"goal" .= ("Inspect the public stopped source."::T.Text)] privateOutput
    refusedAssist<-await "private retained output refuses assistance admission" (\desktop->do
      value<-current desktop
      pure ((field "assistance" value >>= field "phase")==Just ("finished"::T.Text) &&
        (field "assistance" value >>= field "runId"::Maybe T.Text)==(field "assistance" admittedAssist >>= field "runId"))) assisting
    assistStatus<-current refusedAssist
    check "private output never becomes a supplier observation"
      ((field "assistance" assistStatus >>= field "reason")==Just ("observation-expired-or-private"::T.Text) &&
       (field "assistance" assistStatus >>= field "decisions")==Just (0::Int))
  let spec=case filter ((==Just ("debug_configure_breakpoints"::T.Text)).field "name") debugTools of value:_->value; _->error "missing breakpoint tool"
  check "rich breakpoint mutations retain execution permission admission" ((field "annotations" spec >>= field "readOnlyHint")==Just False)
  _<-send "disconnect" [] privateOutput >>= await "rich disconnect" (pure . T.isInfixOf "Debugger disconnected" . status)
  putStrLn "rich source breakpoint checks passed"

-- Same-path views can have different private roles. Preserving an expression
-- through another view must preserve every original host-owned source identity.
duplicateBreakpointProvenanceCheck :: IO ()
duplicateBreakpointProvenanceCheck=bracket (fixture "rich-breakpoints") cleanup $ \(port,path,_)->withDebugger $ \runtime->do
  let core desktop _=pure (False,desktop)
      send action values desktop=snd <$> debuggerEffects runtime core desktop [DebugAction action values]
      tool name args desktop=do
        (next,finish)<-debuggerTool runtime desktop name (object args)
        value<-finish >>= either (error . T.unpack) pure
        pure (next,value)
      state desktop=snd <$> tool "debug_status" [] desktop
      await desktop=do
        next<-tickDebugger runtime desktop
        value<-state next
        if field "ready" value==Just True && field "configured" value==Just True then pure next
          else threadDelay 1000 >> await next
      edit desktop=do
        listed<-send "breakpoints" [] desktop {dialog=Nothing}
        dg<-maybe (error "missing duplicate breakpoint list") pure (dialog listed)
        let (next,effects)=submitDialog 0 dg listed
        snd <$> debuggerEffects runtime core next effects
      condition dg=case [value|Input "Condition" value _<-fields dg] of value:_->value;_->""
  attached<-send "connect" ["0","127.0.0.1",T.pack port] (initialDesktop (80,25))
  ready<-timeout debuggerTimeout (await attached) >>= maybe (error "duplicate breakpoint adapter readiness") pure
  canonical<-canonicalizePath (path<>".hs")
  let first=addDocument (Just (FileState canonical Nothing)) (newBuffer "one\ntwo\n") ready
      original=maybe (error "missing original source") sourceFixtureBuffer (activeWindow first)
      originalWindow=maybe (error "missing original source window") windowId (activeWindow first)
  initial<-state first
  let epoch=fromMaybe (error "missing duplicate generation") (field "generation" initial)::Int
  (configured,_)<-tool "debug_configure_breakpoints" ["generation" .= epoch,"bufferId" .= original,"breakpoints" .= [object ["line" .= (1::Int),"condition" .= ("ORIGINAL_OWNER_EXPRESSION"::T.Text)]]] first
  let duplicate=addDocument (Just (FileState canonical Nothing)) (newBuffer "one\ntwo\n") configured
      second=maybe (error "missing duplicate source") sourceFixtureBuffer (activeWindow duplicate)
  (preserved,_)<-tool "debug_set_breakpoints" ["generation" .= epoch,"bufferId" .= second,"lines" .= ([1,2]::[Int])] duplicate
  editing<-edit preserved
  dg<-maybe (error "missing duplicate expression form") pure (dialog editing)
  let private=editing {buffers=M.adjust (\doc->doc {documentLabel=Just "Git diff"}) original (buffers editing)}
  check "duplicate fixture changes only original source role"
    (original/=second && protectedBuffer private original && not (protectedBuffer private second) && null (privateFilePaths private))
  (observedPrivate,hidden)<-tool "debug_status" [] private
  check "line-only duplicate cannot replace retained expression provenance"
    (null (fromMaybe [] (field "breakpoints" hidden)::[Value]))
  check "open duplicate expression form follows original source privacy"
    (condition dg=="ORIGINAL_OWNER_EXPRESSION" && all (privateDialogField private dg) [f|f@Input{}<-fields dg])
  (_,attempt)<-debuggerTool runtime observedPrivate "debug_configure_breakpoints" (object ["generation" .= epoch,"bufferId" .= second,"breakpoints" .= [object ["line" .= (1::Int),"condition" .= ("PUBLIC_DUPLICATE_REPLACEMENT"::T.Text)]]])
  refused<-attempt
  check "public duplicate cannot admit edits to retained private expressions" (refused==Left "Debugger source is private.")
  let (closedAfterRead,_)=Hide.Model.runCommand Close (focusWindow originalWindow observedPrivate)
  closedStatus<-state closedAfterRead
  closedForm<-maybe (error "source closure discarded the retained expression form") pure (dialog closedAfterRead)
  check "observed private source closure cannot publish retained expressions"
    (M.notMember original (buffers closedAfterRead) && null (fromMaybe [] (field "breakpoints" closedStatus)::[Value]))
  check "retained form keeps observed privacy after its original source closes"
    (all (privateDialogField closedAfterRead closedForm) [f|f@Input{}<-fields closedForm])
  -- Ordinary membership edits and human disable/re-enable retain the same owner.
  toggled<-send "breakpoint" [] (moveTo False 4 closedAfterRead {dialog=Nothing}) >>= send "breakpoint" [] . moveTo False 0 >>= send "breakpoint" []
  again<-edit toggled
  kept<-maybe (error "missing retained private expression form") pure (dialog again)
  after<-state again
  check "human toggles preserve private expression provenance and contents"
    (condition kept=="ORIGINAL_OWNER_EXPRESSION" && all (privateDialogField again kept) [f|f@Input{}<-fields kept] &&
     null (fromMaybe [] (field "breakpoints" after)::[Value]))

fixture :: String -> IO (String,FilePath,ProcessHandle)
fixture mode=do
  dir<-getTemporaryDirectory
  (logPath,h)<-openTempFile dir "dap-session.log"
  hClose h
  writeFile (logPath<>".hs") "local = 1\n"
  (_,Just output,_,process)<-createProcess (proc "python3" ["test/dap-session.py",logPath,mode]) {std_out=CreatePipe}
  port<-hGetLine output
  hClose output
  pure (port,logPath,process)

cleanup :: (String,FilePath,ProcessHandle) -> IO ()
cleanup (_,path,process)=do
  terminateProcess process
  _<-waitForProcess process
  removeFile path
  removeFile (path<>".hs")

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.:key))

rows :: Desktop -> [T.Text]
rows d=case dialog d of Just dg -> concat [values | ListBox _ values _<-fields dg]; _ -> []

hasDialog :: T.Text -> Desktop -> Bool
hasDialog title=maybe False ((==title).dialogTitle) . dialog

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)

launchChecks :: IO ()
launchChecks = do
  directory <- getCurrentDirectory
  port <- bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \listener -> do
    Socket.bind listener (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127,0,0,1)))
    Socket.SockAddrInet number _ <- Socket.getSocketName listener
    pure (fromIntegral number :: Int)
  temp <- getTemporaryDirectory
  bracket (openTempFile temp "dap-launch.json") (\(path,h) -> hClose h >> removeFile path) $ \(path,h) -> do
    hClose h
    let logs=path<>".log"
        config transport mode requestName=object
          ([ (if transport=="server" then "server" else "command") .=
              (["python3",directory<>"/test/dap-session.py",logs,transport<>"-"<>mode] :: [String]),
            "request" .= (requestName :: T.Text),"arguments" .= object ["program" .= ("space λ.hs" :: T.Text)]] ++
            ["port" .= port | transport=="server"])
        core d _=pure (False,d)
        waitFor runtime predicate d=timeout debuggerTimeout (loop d) >>= maybe (error "launch fixture timed out") pure
          where loop state=do
                  updated<-tickDebugger runtime state
                  if predicate updated then pure updated else if "DAP:" `T.isPrefixOf` status updated then do
                    (_,finish)<-debuggerTool runtime updated "debug_status" (object [])
                    details<-finish
                    error (T.unpack (status updated)++" "++show details)
                  else threadDelay 1000 >> loop updated
    mapM_ (\(transport,mode,requestName) -> withDebugger $ \runtime -> do
      writeFile logs ""
      BL.writeFile path (encode (config transport mode requestName))
      let send action values d=snd <$> debuggerEffects runtime core d [DebugAction action values]
      started<-send "launch-config" ["1",T.pack path] (initialDesktop (80,25))
      if mode `elem` ["launch-fail","launch-fail-late"] then do
        failed<-waitFor runtime (T.isInfixOf "fixture refused" . status) started
        after<-send "threads" [] failed
        check "failed launch closes session" (status after=="Debugger is not ready for this command.")
      else do
        stopped<-waitFor runtime (T.isInfixOf "value = λ" . activeText) started
        disconnected<-send "disconnect" [] stopped >>= waitFor runtime (T.isInfixOf "Debugger disconnected" . status)
        check "stdio launch disconnect clears dialog" (dialog disconnected==Nothing)
      entries<-map (fromMaybe (error "bad launch log") . decodeStrictText) . T.lines <$> TIO.readFile logs
      let requests=[r | e<-entries,Just r<-[field "request" e]]
      unless (mode `elem` ["launch-fail","launch-fail-late"]) $ check "disconnect terminates launches and preserves attached programs"
        (any (\r -> field "command" r==Just ("disconnect"::T.Text) && (field "arguments" r >>= field "terminateDebuggee")==Just (requestName=="launch" || transport=="server")) requests)
      check "launch forwards request and arguments" (any (\r -> field "command" r==Just requestName && (field "arguments" r >>= field "program")==Just ("space λ.hs" :: T.Text)) requests)
      ) [("stdio","basic","launch"),("stdio","basic","attach"),("stdio","launch-fail","launch"),("stdio","launch-fail-late","launch"),("server","basic","launch"),("server","basic","attach")]
    withDebugger $ \runtime -> do
      TIO.writeFile path "{\"command\":[\"python3\"],\"request\":\"launch\",\"arguments\":[]}"
      (_,d)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "launch-config" ["1",T.pack path]]
      check "invalid launch arguments rejected before spawning" ("arguments" `T.isInfixOf` status d)
    forM_ [object ["command" .= (["python3"]::[String]),"server" .= (["python3"]::[String])],
           object ["server" .= ([]::[String])],
           object ["server" .= (["python3"]::[String]),"host" .= ("example.com"::T.Text)]] $ \bad ->
      withDebugger $ \runtime -> do
        BL.writeFile path (encode bad)
        (_,d)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "launch-config" ["1",T.pack path]]
        check "invalid server config rejected before spawning" ("DAP configuration:" `T.isPrefixOf` status d)
    removeFile logs

-- Launch can compile a cold project. Other DAP requests keep the short deadline.
launchDeadlineCheck :: IO ()
launchDeadlineCheck = do
  directory<-getCurrentDirectory
  temp<-getTemporaryDirectory
  bracket (openTempFile temp "dap-cold-launch.json")
    (\(path,h) -> hClose h >> mapM_ removeFile [path,path<>".log"]) $ \(path,h) -> do
    hClose h
    let logs=path<>".log"
        core d _=pure (False,d)
    writeFile logs ""
    BL.writeFile path (encode (object ["command" .= (["python3",directory<>"/test/dap-session.py",logs,"stdio-launch-wait"]::[String])]))
    clock<-newIORef 0
    withDebuggerClock (readIORef clock) $ \runtime -> do
      (_,started)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "launch-config" ["1",T.pack path]]
      let loop d=do
            updated<-tickDebugger runtime d
            (_,finish)<-debuggerTool runtime updated "debug_status" (object [])
            result<-finish
            if either (const False) (\v -> maybe False (T.isInfixOf "loading cradle") (field "output" v)) result
              then pure updated else threadDelay 1000 >> loop updated
      loading<-timeout debuggerTimeout (loop started) >>= maybe (error "cold launch fixture did not load") pure
      writeIORef clock 16000000000
      alive<-tickDebugger runtime loading
      check "cold launch survives ordinary request deadline" (not ("timed out" `T.isInfixOf` status alive))
      writeIORef clock 121000000000
      expired<-tickDebugger runtime alive
      check "cold launch retains bounded deadline" ("DAP request timed out" `T.isInfixOf` status expired)

-- Six minutes pass before the UI sees transport readiness. The request clock
-- must start at that event, and still expire an unresponsive initialize afterward.
startupDeadlineCheck :: IO ()
startupDeadlineCheck = do
  temp<-getTemporaryDirectory
  bracket (openTempFile temp "dap-deadline.json")
    (\(path,h) -> hClose h >> mapM_ (\file -> doesFileExist file >>= \exists -> when exists (removeFile file)) [path,path<>".ready",path<>".received"]) $ \(path,h) -> do
    hClose h
    clock<-newIORef 0
    let script="import pathlib,sys,time; p=sys.argv[1]; pathlib.Path(p+'.ready').touch(); sys.stdin.buffer.readline(); pathlib.Path(p+'.received').touch(); time.sleep(60)"
        core d _=pure (False,d)
        waitFor label action=timeout debuggerTimeout (loop action) >>= maybe (error label) pure
        loop action=action >>= \done -> unless done (threadDelay 1000 >> loop action)
    BL.writeFile path (encode (object ["command" .= (["python3","-u","-c",script,path] :: [String]),
      "request" .= ("launch" :: T.Text),"arguments" .= object []]))
    withDebuggerClock (readIORef clock) $ \runtime -> do
      (_,started)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "launch-config" ["1",T.pack path]]
      waitFor "deadline adapter did not start" (doesFileExist (path<>".ready"))
      sentEarly<-doesFileExist (path<>".received")
      check "initialize waits for transport readiness event" (not sentEarly)
      writeIORef clock 360000000000
      let awaitInitialize d=do
            updated<-tickDebugger runtime d
            check "startup elapsed time does not consume initialize deadline" (not ("timed out" `T.isInfixOf` status updated))
            received<-doesFileExist (path<>".received")
            if received then pure updated else threadDelay 1000 >> awaitInitialize updated
      connected<-timeout debuggerTimeout (awaitInitialize started) >>= maybe (error "initialize was not sent after readiness") pure
      writeIORef clock 376000000000
      expired<-tickDebugger runtime connected
      check "connected initialize retains a bounded request deadline" ("DAP request timed out" `T.isInfixOf` status expired)

-- A Windows-owned adapter can require the bounded taskkill /T grace on stop.
debuggerTimeout :: Int
debuggerTimeout=if os=="mingw32" then 10000000 else 5000000

presentationCheck :: IO ()
presentationCheck=bracket (fixture "basic") cleanup $ \(port,_,_) -> withDebugger $ \runtime -> do
  let tool name args d=do
        (updated,finish)<-debuggerTool runtime d name (object args)
        result<-finish >>= either (error . T.unpack) pure
        pure (updated,result)
      current d=snd <$> tool "debug_status" [] d
      epoch value=fromMaybe (error "missing debugger generation") (field "generation" value) :: Int
      waitFor label predicate d=timeout debuggerTimeout (loop d) >>= maybe (error (label<>" timed out")) pure
        where loop state=do
                updated<-tickDebugger runtime state
                snapshot<-current updated
                if predicate updated snapshot then pure updated else threadDelay 1000 >> loop updated
      reveal view gen d | view/="scopes"=tool "debug_present" ["generation" .= gen,"view" .= (view::T.Text)] d
      reveal view gen d=do
        (queued,finish)<-debuggerTool runtime d "debug_present" (object ["generation" .= gen,"view" .= (view::T.Text)])
        withAsync finish $ \reply->do
          let loop desktop=do
                next<-tickDebugger runtime desktop
                complete<-poll reply
                if isJust complete then pure next else threadDelay 1000 >> loop next
          settled<-timeout debuggerTimeout (loop queued) >>= maybe (error "presentation reply timed out") pure
          result<-wait reply >>= either (error . T.unpack) pure
          pure (settled,result)
      base=(addDocument Nothing (newBuffer "my editor work") (initialDesktop (80,25)))
        {dialog=Just (Dialog "Existing debugger view" (DebugDialog "existing") [] 0 ["Close"] [])}
  (quiet,configured)<-tool "debug_present" ["follow" .= False] base
  check "background preference can be set before connecting without changing UI" (quiet==base && field "follow" configured==Just False)
  (attached,_)<-tool "debug_attach" ["port" .= (read port::Int)] quiet
  -- Preserve an existing modal while asynchronous adapter events arrive.
  let working=attached {dialog=dialog base}
  stopped<-waitFor "background frame" (\_ s -> field "stopped" s==Just True && (field "frame" s >>= field "id")==Just (11::Int)) working
  snapshot<-current stopped
  let gen=epoch snapshot
  check "background stop keeps windows, focus, buffers and modal"
    (windows stopped==windows working && buffers stopped==buffers working && dialog stopped==dialog working && field "follow" snapshot==Just False)
  (stale,staleReply)<-debuggerTool runtime stopped "debug_present" (object ["generation" .= (gen-1),"follow" .= True,"view" .= ("source"::T.Text)])
  rejected<-staleReply
  afterRejected<-current stale
  check "stale reveal rejects preference changes atomically" (stale==stopped && either (const True) (const False) rejected && field "follow" afterRejected==Just False)
  forM_ [object ["view" .= ("source"::T.Text)],object ["generation" .= gen,"view" .= ("unknown"::T.Text)],object ["follow" .= ("no"::T.Text)]] $ \args -> do
    (unchanged,finish)<-debuggerTool runtime stopped "debug_present" args
    result<-finish
    check "presentation arguments reject missing generation/unknown view/wrong type" (unchanged==stopped && either (const True) (const False) result)
  (requested,_)<-reveal "source" gen stopped
  -- The ordered DAP control response follows the source response. It proves
  -- preparation was admitted while the human modal still owns input.
  (queued,barrier)<-debuggerTool runtime requested "debug_inspect" (object ["generation" .= gen,"request" .= ("threads"::T.Text)])
  held<-withAsync barrier $ \reply->do
    let loop d=do
          next<-tickDebugger runtime d
          done<-poll reply
          if isJust done then pure next else threadDelay 1000 >> loop next
    waiting<-timeout debuggerTimeout (loop queued) >>= maybe (error "source/modal response barrier timed out") pure
    _<-wait reply >>= either (error . T.unpack) pure
    pure waiting
  check "prepared adapter source waits without replacing a human modal or source focus"
    (activeText held=="my editor work" && fmap dialogTitle (dialog held)==Just "Existing debugger view" && length (windows held)==1)
  shown<-waitFor "explicit source reveal after dismissal" (\d _ -> "value = λ" `T.isInfixOf` activeText d) held {dialog=Nothing}
  shownStatus<-current shown
  check "explicit reveal opens source without changing stopped generation or follow mode"
    (epoch shownStatus==gen && field "follow" shownStatus==Just False && dialog shown==Nothing)
  (stack,stackStatus)<-reveal "stack" gen shown {dialog=Nothing}
  check "cached stack is explicitly visible without resuming" (hasDialog "Call stack" stack && epoch stackStatus==gen)
  (loading,_)<-reveal "scopes" gen stack {dialog=Nothing}
  scopes<-waitFor "explicit scopes reveal" (\d _ -> hasDialog "Scopes" d) loading
  (preparingOutput,_)<-reveal "output" gen scopes {dialog=Nothing}
  output<-waitFor "explicit semantic output" (\d _->case activeWindow d >>= (\w->case windowContent w of PluginContent reference->Just reference; _->Nothing) of Just _->True; _->False) preparingOutput
  check "explicit output uses existing debugger session" (case activeWindow output >>= (\w->case windowContent w of PluginContent reference->M.lookup reference (pluginWindows output); _->Nothing) of Just prepared->W.preparedWindowTitle prepared=="Debugger output"; _->False)
  (following,_)<-tool "debug_present" ["follow" .= True] output
  (running,_)<-tool "debug_control" ["generation" .= gen,"command" .= ("continue"::T.Text)] following
  ready<-waitFor "resume processed" (\d _ -> status d=="Running...") running
  runningStatus<-current ready
  (pausing,_)<-tool "debug_control" ["generation" .= epoch runningStatus,"command" .= ("pause"::T.Text)] ready
  followed<-waitFor "follow restored" (\d s -> field "stopped" s==Just True && "value = λ" `T.isInfixOf` activeText d) pausing
  followedStatus<-current followed
  check "reenabling follow restores automatic source reveal on later stop" (field "follow" followedStatus==Just True && epoch followedStatus>gen)
  _<-tool "debug_control" ["generation" .= epoch followedStatus,"command" .= ("disconnect"::T.Text)] followed
  pure ()

-- The adapter's source reply is queued after the stack tick. Turning off follow
-- between those ticks must suppress an automatic reveal already in flight.
pendingPresentationCheck :: IO ()
pendingPresentationCheck=bracket (fixture "basic") cleanup $ \(port,_,_) -> withDebugger $ \runtime -> do
  let tool name args d=do
        (updated,finish)<-debuggerTool runtime d name (object args)
        result<-finish >>= either (error . T.unpack) pure
        pure (updated,result)
      current d=snd <$> tool "debug_status" [] d
      tick=tickDebugger runtime
      base=addDocument Nothing (newBuffer "unchanged foreground") (initialDesktop (80,25))
      waitUntil label predicate d=timeout debuggerTimeout (loop d) >>= maybe (error (label<>" timed out")) pure
        where loop desktop=do
                updated<-tick desktop
                done<-predicate updated
                if done then pure updated else threadDelay 1000 >> loop updated
  (attached,_)<-tool "debug_attach" ["port" .= (read port::Int)] base
  awaitingSource<-waitUntil "automatic source request" (\d->do
    s<-current d
    pure ((field "frame" s >>= field "id")==Just (11::Int))) attached
  check "fixture exposes a source request before its response is processed" (activeText awaitingSource=="unchanged foreground")
  (quiet,_)<-tool "debug_present" ["follow" .= False] awaitingSource
  snapshot<-current quiet
  let gen=fromMaybe (error "missing generation") (field "generation" snapshot) :: Int
  (pending,finish)<-debuggerTool runtime quiet "debug_inspect" (object ["generation" .= gen,"request" .= ("threads"::T.Text)])
  drained<-withAsync finish $ \reply -> do
    after<-waitUntil "source reply barrier" (\_ -> isJust <$> poll reply) pending
    result<-wait reply
    check "structured response remains available in background" (either (const False) (const True) result)
    pure after
  check "late automatic source reply cannot steal focus after follow is disabled"
    (windows drained==windows quiet && buffers drained==buffers quiet && dialog drained==dialog quiet)
  _<-tool "debug_control" ["generation" .= gen,"command" .= ("disconnect"::T.Text)] drained
  pure ()

-- Completion and exit are distinct DAP events, and may arrive in either order.
completionChecks :: IO ()
completionChecks=forM_ ["exit-first","exit-last","exit-missing"] $ \mode ->
  bracket (fixture mode) cleanup $ \(port,path,_) -> do
    clock<-newIORef 0
    withDebuggerClock (readIORef clock) $ \runtime -> do
      let core d _=pure (False,d)
          send action values d=snd <$> debuggerEffects runtime core d [DebugAction action values]
          state d=do (_,finish)<-debuggerTool runtime d "debug_status" (object []); finish >>= either (error . T.unpack) pure
          awaitState label predicate d=timeout debuggerTimeout (loop d) >>= maybe (error label) pure
            where loop current=do
                    next<-tickDebugger runtime current
                    value<-state next
                    if predicate value then pure next else threadDelay 1000 >> loop next
      connected<-send "connect" ["0","127.0.0.1",T.pack port] (initialDesktop (80,25))
      stopped<-awaitState "completion fixture stop" ((==Just True).field "stopped") connected
      running<-send "continue" [] stopped
      ended<-awaitState "termination event" ((==Just True).field "terminated") running
      endedState<-state ended
      check "terminated programs cannot be inspected as active" (field "active" endedState==Just False && field "frame" endedState==Just Null)
      check "exit is not an end-of-stream marker" (field "finishing" endedState==Just True)
      drained<-if mode=="exit-missing" then pure ended else do
        writeFile (path<>".release") "release"
        awaitState "output after termination and exit" (\value ->
          maybe False (T.isInfixOf "final output") (field "output" value) && field "exitCode" value==Just (42::Int)) ended
      writeIORef clock 2000000000
      finished<-awaitState "bounded exit collection" ((==Just False).field "finishing") drained
      result<-state finished
      check "exit code survives both event orders; missing remains unknown"
        ((field "exitCode" result :: Maybe Value)==Just (if mode=="exit-missing" then Null else toJSON (42::Int)))
      when (mode/="exit-missing") $ do
        check "final program output is drained" (maybe False (T.isInfixOf "final output") (field "output" result))
        removeFile (path<>".release")

terminalCheck :: IO ()
terminalCheck = when Terminal.terminalAvailable $ mapM_ session ["terminal","terminal-stop","terminal-functions","terminal-shell","terminal-invalid","terminal-missing"]
  where
    session mode = bracket (fixture mode) cleanup $ \(port,logPath,_) -> C.withConsoles $ \consoles -> withDebuggerConsoles consoles $ \runtime -> do
      let core d _=pure (False,d)
          tick=tickDebugger runtime
          send action d=snd <$> debuggerEffects runtime core d [DebugAction action []]
          terminalText d=T.concat [contents (documentBuffer doc) | doc<-M.elems (buffers d),maybe False (T.isPrefixOf "Terminal ") (documentLabel doc)]
          outputText d=T.concat [let text=W.preparedWindowText prepared in contentSlice text 0 (contentLength text) | prepared<-M.elems (pluginWindows d),W.preparedWindowTitle prepared=="Debugger output"]
          await label predicate d=timeout debuggerTimeout (loop d) >>= maybe (error label) pure
            where loop state=do n<-tick state; done<-predicate n; if done then pure n else threadDelay 1000 >> loop n
          raw ident predicate _=C.consoleOutput consoles ident >>= pure . either (const False) predicate
      (_,connected)<-debuggerEffects runtime core (initialDesktop (100,32)) [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
      if mode `elem` ["terminal-shell","terminal-invalid","terminal-missing"] then do
        rejected<-await "reverse request failure was not returned" (pure . T.isInfixOf "reverse terminal rejected" . outputText) connected
        entries<-C.listConsoles consoles
        check "failed reverse terminal creates no terminal" (null entries)
        _<-send "disconnect" rejected >>= await "failed-terminal disconnect" (pure . T.isInfixOf "Debugger disconnected" . status)
        pure ()
      else do
        opened<-await "reverse terminal request opens live shared terminal" (pure . T.isInfixOf "terminal ready λ" . terminalText) connected
        entries<-C.listConsoles consoles
        (ident,bid)<-case entries of [(ident,bid,_)]->pure (ident,bid); _->error "expected one debugger terminal"
        ready<-await "argv/env/cwd terminal output" (raw ident (\(bytes,_,_)->"unset=True" `BS.isInfixOf` bytes)) opened
        captured<-C.consoleOutput consoles ident
        check "terminal gets literal Unicode argv, cwd and environment overrides/unsets" (case captured of
          Right (bytes,_,_)->all (`BS.isInfixOf` bytes) [TE.encodeUtf8 "argv=['literal ; λ']","env=value with spaces","cwd=","unset=True"]
          _->False)
        pid<-C.consoleProcessId consoles ident
        check "terminal reports actual child process id" (either (const False) (>0) pid)
        ready<-if mode/="terminal-functions" then pure ready else do
          listed<-send "function-breakpoints" ready {dialog=Nothing}
          picker<-maybe (error "missing function catalogue") pure (dialog listed)
          let (chosen,effects)=submitDialog 0 picker listed
          editing<-snd <$> debuggerEffects runtime core chosen effects
          editor<-maybe (error "missing function name editor") pure (dialog editing)
          let form=editor {fields=[Input "Function name" "Main.visit" 10,CheckBox "Enabled" True]}
              (stored,actions)=submitDialog 0 form editing
          queued<-snd <$> debuggerEffects runtime core stored actions
          adopted<-await "literal function request delivery" (\_->do
            rows<-map (fromMaybe Null . decodeStrictText) . T.lines <$> TIO.readFile logPath
            pure (any (\entry->case field "request" entry of
              Just request | field "command" request==Just ("setFunctionBreakpoints"::T.Text)->
                (field "arguments" request >>= field "breakpoints"::Maybe[Value])==Just [object ["name" .= ("Main.visit"::T.Text)]]
              _->False) rows)) queued
          (_,privateReport)<-debuggerTool runtime adopted "debug_status" (object [])
          report<-privateReport >>= either (error . T.unpack) pure
          check "function names retain their DAP report privacy policy" (field "output" report==Just (""::T.Text) && isJust (field "outputUnavailable" report::Maybe T.Text))
          visible<-C.consoleOutput consoles ident
          check "function report taint cannot shut off an unrelated public program terminal"
            (case visible of Right (bytes,_,_)->TE.encodeUtf8 "terminal ready λ" `BS.isInfixOf` bytes;_->False)
          pure adopted
        requests<-map (fromMaybe Null . decodeStrictText) . T.lines <$> TIO.readFile logPath
        check "initialize advertises implemented terminal support" (any (\entry->
          (field "request" entry >>= field "arguments" >>= field "supportsRunInTerminalRequest")==Just True) requests)
        -- Frontend absence does not affect ownership. No tick consumes the output
        -- until a resumed display polls the same console below.
        C.inputConsole consoles ident (TE.encodeUtf8 "typed λ\n") >>= either (error . T.unpack) pure
        echoed<-await "stdin and stderr appear after frontend resumes" (raw ident (\(bytes,_,_)->all (`BS.isInfixOf` bytes) [TE.encodeUtf8 "stdin=typed λ","stderr visible"])) ready
        if mode=="terminal" && os/="mingw32" then do
          C.inputConsole consoles ident (BS.singleton 3) >>= either (error . T.unpack) pure
          ended<-await "Ctrl+C reaches the debugger terminal child" (raw ident (\(_,_,code)->code==Just 23)) echoed
          let before=fmap windowId (activeWindow ended)
          painted<-tick ended
          check "live terminal ticks preserve focus" (fmap windowId (activeWindow painted)==before)
          _<-send "disconnect" painted >>= await "terminal disconnect" (pure . T.isInfixOf "Debugger disconnected" . status)
          pure ()
        else do
          ended<-send "disconnect" echoed >>= await "terminal stop disconnect" (pure . T.isInfixOf "Debugger disconnected" . status)
          _<-await "disconnect kills owned terminal" (raw ident (\(_,_,code)->isJust code)) ended
          check "completed terminal screen is retained" (M.member bid (buffers ended))

-- The official bindist's reverse request names its inner binary, bypassing
-- the launcher's loader environment. Only its known interpreter command uses
-- the owned launcher; an ordinary reverse command must remain unchanged.
terminalLauncherCheck :: IO ()
terminalLauncherCheck = when (Terminal.terminalAvailable && os/="mingw32") $ do
  directory<-getCurrentDirectory
  temp<-getTemporaryDirectory
  bracket (openTempFile temp "dap-terminal-wrapper") (\(path,h)->hClose h >> removeFile path) $ \(wrapper,h)->do
    hPutStr h (unlines ["#!/usr/bin/env python3","import os, sys, runpy",
      "if sys.argv[1] == 'external-interpreter':",
      "    os.execv(sys.executable, [sys.executable] + sys.argv[2:])",
      "else:","    sys.argv.pop(0)","    runpy.run_path(sys.argv[0], run_name='__main__')"])
    hClose h
    permissions<-getPermissions wrapper
    setPermissions wrapper permissions {executable=True}
    forM_ ["terminal-wrapper","terminal"] $ \mode ->
      bracket (openTempFile temp "dap-wrapper-launch.json") (\(path,handle)->hClose handle >> removeFile path >> removeFile (path<>".log")) $ \(config,handle)->do
        hClose handle
        writeFile (config<>".log") ""
        port<-bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \listener -> do
          Socket.bind listener (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127,0,0,1)))
          Socket.SockAddrInet number _<-Socket.getSocketName listener
          pure (fromIntegral number::Int)
        BL.writeFile config (encode (object ["host" .= ("127.0.0.1"::T.Text),"port" .= port,"adapterId" .= ("hdb"::T.Text),
          "request" .= ("launch"::T.Text),"arguments" .= object [],
          "server" .= [wrapper,directory<>"/test/dap-session.py",config<>".log","server-"<>mode]]))
        C.withConsoles $ \consoles -> withDebuggerConsoles consoles $ \runtime -> do
          let core d _=pure (False,d)
              tick=tickDebugger runtime
              await d=do
                n<-tick d
                entries<-C.listConsoles consoles
                outputs<-mapM (\(ident,_,_)->C.consoleOutput consoles ident) entries
                if any (either (const False) (\(bytes,_,_)->TE.encodeUtf8 "argv=['literal ; λ']" `BS.isInfixOf` bytes)) outputs
                  then pure () else threadDelay 1000 >> await n
          (_,started)<-debuggerEffects runtime core (initialDesktop (100,32)) [DebugAction "launch-config" ["1",T.pack config]]
          result<-timeout debuggerTimeout (await started)
          check ("owned hdb terminal launcher preserves argv: "<>mode) (result==Just ())
