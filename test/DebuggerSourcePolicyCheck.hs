-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE CPP, OverloadedStrings #-}
-- |
-- Module      : DebuggerSourcePolicyCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : CPP, OverloadedStrings

module DebuggerSourcePolicyCheck (checks) where

import Control.Concurrent (threadDelay,myThreadId,newEmptyMVar,putMVar,readMVar,tryPutMVar,tryReadMVar)
import Control.Concurrent.Async (withAsync,poll,wait)
import Control.Exception (bracket,finally)
import Control.Monad (unless,when)
import Data.Aeson
import Data.IORef
import Data.Aeson.Types (parseMaybe,Parser)
import qualified Data.Text as T
import System.Timeout (timeout)
import qualified DebuggerCheck as Fixture
import Hide.Debugger
import Hide.DebuggerSidebarTypes
import Hide.Plugin.BufferHost (captureVersion)
import Hide.Model
import qualified Data.Map.Strict as M
import System.Directory (getTemporaryDirectory,removeFile,canonicalizePath)
import System.IO (openTempFile,hClose)
import System.FilePath ((</>),takeDirectory)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Hide.Files (loadFile)
#ifndef mingw32_HOST_OS
import qualified GHC.IO.Device as Device
import qualified GHC.IO.FD as FD
import Text.Read (readMaybe)
import Control.Monad (void)
import Hide.Files (FileState(..))
import System.Posix.Files (createNamedPipe,getFileStatus,getFdStatus,fileID,deviceID,isNamedPipe)
import qualified System.Posix.IO.ByteString as PosixBytes
import System.Posix.IO (openFd,closeFd,fdWrite,dup,queryFdOption,FdOption(NonBlockingRead),OpenMode(ReadWrite),defaultFileFlags,nonBlock,cloexec)
import System.Posix.Types (Fd(..))
import System.Posix.Process (getProcessID)
import System.IO.Error (tryIOError,isFullError)
import System.Process (getPid,readProcessWithExitCode)
#endif
import Hide.Buffer
import Hide.GuestAccess
import Hide.Recovery

checks :: IO ()
checks=inspectionPrivacyCheck >> locationsCheck >> localSourceCheck >> missingLocalSourceCheck >> delayedLocalSourceCheck >> sourceHandleCheck >> generatedOriginCheck >> delayedPolicyCheck >> sourceStampCheck >> originCheck >> putStrLn "Debugger source policy checks passed"

-- Public inspection must retain the selected frame's source authority through
-- scopes and descendants; a numeric adapter handle alone grants no access.
inspectionPrivacyCheck :: IO ()
inspectionPrivacyCheck=bracket (Fixture.fixture "inspection-policy") Fixture.cleanup $ \(port,path,_)->withDebugger $ \runtime->do
  canonical<-canonicalizePath (path<>".hs")
  let core d _=pure (False,d)
      await predicate d=timeout 5000000 (loop d) >>= maybe (fail "inspection privacy fixture timed out") pure
        where loop current=do next<-tickDebugger runtime current; yes<-predicate next; if yes then pure next else threadDelay 1000 >> loop next
      inspect epoch request extra desktop=do
        (queued,reply)<-debuggerTool runtime desktop "debug_inspect" (object (["generation" .= epoch,"request" .= (request::T.Text)]++extra))
        withAsync reply $ \worker->do
          _<-await (\_->maybe False (const True) <$> poll worker) queued
          wait worker
      reject label result=unless (either (const True) (const False) result) (fail label)
  (_,connected)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
  stopped<-await (pure . T.isPrefixOf "Stopped in " . status) connected
  (_,stateReply)<-debuggerTool runtime stopped "debug_status" (object [])
  snapshot<-stateReply >>= either (fail . T.unpack) pure
  epoch<-maybe (fail "missing generation") pure (parseMaybe (withObject "status" (.:"generation")) snapshot :: Maybe Int)
  let private=stopped {guestPrivatePaths=[canonical]}
  inspect epoch "scopes" [] private >>= reject "Private selected frame leaked ordinary scopes"
  (privateReveal,privateRevealReply)<-debuggerTool runtime private "debug_present" (object ["generation" .= epoch,"view" .= ("scopes"::T.Text)])
  withAsync privateRevealReply $ \worker->do
    unchanged<-await (\_->maybe False (const True) <$> poll worker) privateReveal
    wait worker >>= reject "Agent scheduled private scope presentation"
    unless (dialog unchanged==dialog private) (fail "Private scope reveal changed the visible inspector")
  inspect epoch "scopes" ["frameId" .= (999::Int)] stopped >>= reject "Unobserved frame granted scope authority"
  (_,humanRequested)<-debuggerEffects runtime core stopped [DebugAction "scopes" []]
  humanScopes<-await (pure . maybe False ((=="Scopes").dialogTitle) . dialog) humanRequested
  scopeDialog<-maybe (fail "Missing human scopes") pure (dialog humanScopes)
  let retained=humanScopes {guestPrivatePaths=[canonical]}
      bounds=dialogRect retained scopeDialog
      x=left bounds+2; y=top bounds+2
  token<-case purpose scopeDialog of DebugDialog action->pure action;_->fail "Missing scope action"
  unless (guestModalBlocked retained && not (readableAt retained x y) && not (streamerReadableAt retained x y) &&
    not (guestEffectsAllowed [DebugAction token ["0","0"]])) (fail "Retained scope chooser exposed private labels or expansion authority")
  (_,humanExpanded)<-debuggerEffects runtime core retained {dialog=Nothing} [DebugAction token ["0","0"]]
  _<-await (pure . maybe False ((=="Variables").dialogTitle) . dialog) humanExpanded
  (do
    BS.writeFile (path<>".inspection-fail") ""
    (_,failedRequest)<-debuggerEffects runtime core private [DebugAction "scopes" []]
    -- The sequential peer's following threads reply proves the scope error was
    -- consumed; HUD text is the assertion, never the completion signal.
    (queued,reply)<-debuggerTool runtime failedRequest "debug_inspect"
      (object ["generation" .= epoch,"request" .= ("threads"::T.Text)])
    failure<-withAsync reply $ \worker->do
      consumed<-await (\_->maybe False (const True) <$> poll worker) queued
      _<-wait worker >>= either (fail . T.unpack) pure
      pure consumed
    unless (status failure=="Debugger inspection failed; request scopes again.") (fail "Private inspection error escaped into the public HUD"))
      `finally` removeFile (path<>".inspection-fail")
  _<-inspect epoch "scopes" [] stopped >>= either (fail . T.unpack) pure
  _<-inspect epoch "variables" ["variablesReference" .= (21::Int)] stopped >>= either (fail . T.unpack) pure
  inspect epoch "variables" ["variablesReference" .= (21::Int)] private >>= reject "Private scope owner leaked variables"
  inspect epoch "variables" ["variablesReference" .= (22::Int)] private >>= reject "Private frame provenance was lost by child variables"
  pathless<-inspect epoch "scopes" ["frameId" .= (0::Int)] stopped >>= either (fail . T.unpack) pure
  absent<-inspect epoch "scopes" ["frameId" .= (1::Int)] stopped >>= either (fail . T.unpack) pure
  unless ("Pathless locals" `T.isInfixOf` T.pack (show pathless) && "Absent source locals" `T.isInfixOf` T.pack (show absent))
    (fail "Public pathless frames lost their scope rows")
  _<-inspect epoch "variables" ["variablesReference" .= (41::Int)] stopped >>= either (fail . T.unpack) pure
  _<-inspect epoch "variables" ["variablesReference" .= (42::Int)] stopped >>= either (fail . T.unpack) pure
  _<-inspect epoch "variables" ["variablesReference" .= (44::Int)] stopped >>= either (fail . T.unpack) pure
  scoped<-inspect epoch "scopes" ["frameId" .= (0::Int)] private >>= either (fail . T.unpack) pure
  unless ("Pathless locals" `T.isInfixOf` T.pack (show scoped) && not ("PRIVATE_" `T.isInfixOf` T.pack (show scoped))) (fail "Public frame disclosed a separately private scope source")
  inspect epoch "variables" ["variablesReference" .= (42::Int)] private >>= reject "Private scope source was replaced by public frame authority"
  inspect epoch "variables" ["variablesReference" .= (43::Int)] private >>= reject "Private scope source was lost by child expansion"
  inspect epoch "variables" ["variablesReference" .= (44::Int)] private >>= reject "Known private scope source reference lost its backing path"
  inspect epoch "variables" ["variablesReference" .= (45::Int)] private >>= reject "Known private scope source reference lost child provenance"
  let variableArgs=object ["generation" .= epoch,"request" .= ("variables"::T.Text),"variablesReference" .= (22::Int)]
      childRequests=length . filter (T.isInfixOf "\"variablesReference\": 22") . T.lines <$> TIO.readFile path
      barrier desktop=debuggerTool runtime desktop "debug_inspect" (object ["generation" .= epoch,"request" .= ("threads"::T.Text)])
  live<-newIORef True
  let caller=readIORef live >>= \current->pure (if current then Right () else Left "retired")
  (_,retired)<-debuggerToolWithCaller caller runtime stopped "debug_inspect" variableArgs
  writeIORef live False
  retired >>= reject "Retired caller admitted variable inspection"
  writeIORef live True
  -- Actual adapter receipt, rather than a sleep, places the privacy change and
  -- caller retirement between request admission and the matching response.
  mapM_ (\retireCaller->do
    before<-childRequests
    (queued,reply)<-debuggerToolWithCaller caller runtime stopped "debug_inspect" variableArgs
    withAsync reply $ \worker->do
      held<-await (\_->(>before) <$> childRequests) queued
      when retireCaller (writeIORef live False)
      (released,finished)<-barrier (if retireCaller then held else held {guestPrivatePaths=[canonical]})
      _<-await (\_->maybe False (const True) <$> poll worker) released
      wait worker >>= reject "Late private source or retired caller leaked child variables"
      _<-finished
      pure ()
    writeIORef live True) [False,True]
  -- Hold the existing caller capability's worker-side publication check after
  -- the response is owned. A policy change must still retire that prepared reply.
  ownerThread<-myThreadId
  publication<-newIORef False
  reached<-newEmptyMVar
  release<-newEmptyMVar
  let publicationCaller=do
        currentThread<-myThreadId
        held<-readIORef publication
        when (held && currentThread/=ownerThread) $ do
          _<-tryPutMVar reached ()
          readMVar release
        pure (Right ())
  beforePublication<-childRequests
  (publicationQueued,publicationReply)<-debuggerToolWithCaller publicationCaller runtime stopped "debug_inspect" variableArgs
  withAsync publicationReply $ \worker->do
    held<-await (\_->(>beforePublication) <$> childRequests) publicationQueued
    writeIORef publication True
    (released,finished)<-barrier held
    published<-await (\_->maybe False (const True) <$> tryReadMVar reached) released
    _<-tickDebugger runtime published {guestPrivatePaths=[canonical]}
    putMVar release ()
    _<-await (\_->maybe False (const True) <$> poll worker) published {guestPrivatePaths=[canonical]}
    wait worker >>= reject "Prepared variables survived a later privacy change"
    _<-finished
    pure ()
  -- The adapter reuses the same numeric frame after changing its source. That
  -- new stack response precedes the old child response on the ordered channel.
  before<-childRequests
  (queued,reply)<-debuggerTool runtime stopped "debug_inspect" variableArgs
  withAsync reply $ \worker->do
    held<-await (\_->(>before) <$> childRequests) queued
    (changed,stackReply)<-debuggerTool runtime held "debug_inspect" (object ["generation" .= epoch,"request" .= ("stackTrace"::T.Text)])
    _<-await (\_->maybe False (const True) <$> poll worker) changed
    wait worker >>= reject "Changed frame metadata revived an old child handle"
    _<-stackReply >>= either (fail . T.unpack) pure
    pure ()
  inspect epoch "variables" ["variablesReference" .= (21::Int)] stopped >>= reject "Reused frame metadata revived old scope handles"
  inspect epoch "variables" ["variablesReference" .= (45::Int)] stopped >>= reject "Reused source reference revived an old scope child"

-- Use the existing DAP peer and real file owner, preserving regular file/save
-- authority and already-open dirty text even when its disk path disappears.
localSourceCheck :: IO ()
localSourceCheck=bracket (Fixture.fixture "local-source") Fixture.cleanup $ \(port,path,_)->do
  canonical<-canonicalizePath (path<>".hs")
  let text="disk\nα界🐈Z\n"
  BS.writeFile canonical (TE.encodeUtf8 text)
  Right (file,_)<-loadFile canonical
  withDebugger $ \runtime->do
    let core d [ReadPath target]=do
          loaded<-loadFile target
          pure (False,either (const d) (\(state,image)->addDocument (Just state) image d) loaded)
        core d _=pure (False,d)
        tick=tickDebugger runtime
        await label predicate d=timeout 5000000 (loop d) >>= maybe (fail label) pure
          where loop current=do next<-tick current; if predicate next then pure next else threadDelay 1000 >> loop next
    (_,attached)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
    opened<-await "local file did not open" (T.isPrefixOf "Stopped in " . status) attached
    unless ((activeDocument opened >>= documentFile)==Just file && (activeDocument opened >>= documentLabel)==Nothing && fmap (caret.selection) (activeWindow opened)==Just 8)
      (fail "Local source must retain regular FileState and UTF-16 caret coordinates")
    let changed=insertText "!" (moveTo False 0 opened)
        present d=do
          (_,stateReply)<-debuggerTool runtime d "debug_status" (object [])
          state<-stateReply >>= either (fail . T.unpack) pure
          generation<-maybe (fail "Missing stopped generation") pure (parseMaybe (withObject "status" (.:"generation")) state :: Maybe Int)
          (queued,reply)<-debuggerTool runtime d "debug_present" (object ["generation" .= generation,"view" .= ("source"::T.Text)])
          _<-reply >>= either (fail . T.unpack) pure
          pure queued
    removeFile canonical
    (do
      queued<-present changed
      retained<-await "dirty open source did not navigate without disk" (T.isPrefixOf "Stopped; unsaved" . status) queued
      unless (activeText retained=="!"<>text && fmap (caret.selection) (activeWindow retained)==Just 9 && (activeDocument retained >>= documentFile)==Just file)
        (fail "Local source follow must preserve dirty open text and file authority")
      pending<-present retained
      let replacement=insertText "?" (moveTo False 0 pending)
      refused<-await "changed local source was not refused" (T.isInfixOf "target changed" . status) replacement
      unless (activeText refused=="?!"<>text && fmap (caret.selection) (activeWindow refused)==Just 1)
        (fail "Late source coordinates must not jump in a changed buffer")
      publicQueued<-present refused
      latePrivate<-await "late agent source lost current private path policy" ((=="Debugger source is private.").status) publicQueued {guestPrivatePaths=[canonical]}
      unless (fmap (caret.selection) (activeWindow latePrivate)==Just 1)
        (fail "Late private policy change must prevent source navigation")
      privateQueued<-present latePrivate {status=""}
      privateRefused<-await "agent local source lost captured private path policy" ((=="Debugger source is private.").status) privateQueued
      unless (fmap (caret.selection) (activeWindow privateRefused)==Just 1 && maybe False (privateDocument privateRefused) (activeDocument privateRefused))
        (fail "Agent source presentation must preserve private document authority")
      -- The human frame chooser retains normal source navigation and privacy.
      (_,selecting)<-debuggerEffects runtime core privateRefused [DebugAction "stack" []]
      chooser<-await "human frame chooser missing" (maybe False ((=="Call stack").dialogTitle).dialog) selecting
      action<-case purpose <$> dialog chooser of Just (DebugDialog token)->pure token; _->fail "Missing frame choice token"
      (_,humanQueued)<-debuggerEffects runtime core chooser {dialog=Nothing} [DebugAction action ["0","0"]]
      humanShown<-await "human private source did not navigate" ((=="Stopped in private debugger source.").status) humanQueued
      unless (fmap (caret.selection) (activeWindow humanShown)==Just 10 && maybe False (privateDocument humanShown) (activeDocument humanShown))
        (fail "Human local source must retain ordinary document privacy"))
      `finally` BS.writeFile canonical (TE.encodeUtf8 text)
  putStrLn "local debugger source owner checks passed"

-- Missing local paths are unavailable, unlike the normal new-file owner. A
-- clean new buffer must not replace the source location after a disk race.
missingLocalSourceCheck :: IO ()
missingLocalSourceCheck=bracket (Fixture.fixture "local-source") Fixture.cleanup $ \(port,path,_)->do
  let source=path<>".hs"
  removeFile source
  (withDebugger $ \runtime->do
    let core d _=pure (False,d)
        base=addDocument Nothing (newBuffer "foreground") (initialDesktop (80,25))
        loop d=do
          next<-tickDebugger runtime d
          if T.isInfixOf "unavailable" (status next) then pure next else threadDelay 1000 >> loop next
    (_,attached)<-debuggerEffects runtime core base [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
    refused<-timeout 5000000 (loop attached) >>= maybe (fail "Missing debugger source was not refused") pure
    unless (activeText refused=="foreground" && length (windows refused)==1)
      (fail "Missing debugger source opened an empty document")) `finally` TIO.writeFile source "local=1\n"

-- A live read-write FIFO keeps the real file read held without racing reader
-- startup. Always close its gate before waiting for debugger cleanup, including
-- the RED UI-owner case. No callback or timer substitutes for the file read.
delayedLocalSourceCheck :: IO ()
#ifdef mingw32_HOST_OS
delayedLocalSourceCheck=pure ()
#else
delayedLocalSourceCheck=mapM_ scenario ["continue","close","disconnect","opened","modal","modal-continue"]
  where
    scenario action=bracket (Fixture.fixture "local-source") Fixture.cleanup $ \(port,path,peer)->do
      let rawSource=path<>".hs"
      removeFile rawSource
      createNamedPipe rawSource 0o600
      source<-canonicalizePath rawSource
      withDebugger $ \runtime->do
        let core d [ReadPath target]=do
              loaded<-loadFile target
              pure (False,either (const d) (\(state,image)->addDocument (Just state) image d) loaded)
            core d _=pure (False,d)
            tick=tickDebugger runtime
            frameReady d=do
              (_,reply)<-debuggerTool runtime d "debug_status" (object [])
              value<-reply >>= either (fail . T.unpack) pure
              pure (parseMaybe (withObject "status" (\o->o .: "frame" >>= withObject "frame" (.:"id"))) value==Just (11::Int))
            awaitFrame d=do next<-tick d; ready<-frameReady next; if ready then pure next else threadDelay 1000 >> awaitFrame next
            awaitPreparation expected=awaitPreparationWhen expected (const True)
            awaitPreparationWhen expected workerReady d=do
              observed<-newIORef ("not polled"::T.Text,""::T.Text,Null)
              let loop current=do
                    next<-tick current
                    (_,reply)<-debuggerTool runtime next "debug_status" (object [])
                    value<-reply >>= either (fail . T.unpack) pure
                    phase<-maybe (fail "Missing source preparation phase") pure
                      (parseMaybe (withObject "status" (.:"sourcePreparation")) value :: Maybe T.Text)
                    let detail=maybe Null id (parseMaybe (withObject "status" (.:"sourcePreparationDetail")) value)
                    writeIORef observed (phase,status next,detail)
                    if phase==expected && workerReady detail then pure next else threadDelay 1000 >> loop next
              timeout 5000000 (loop d) >>= maybe (do
                actual<-readIORef observed
                -- Inspect this invocation's FIFO before debugger/peer teardown
                -- removes the worker and descriptors that could explain missing EOF.
                ownerPid<-getProcessID
                peerPid<-getPid peer
                let owners=show ownerPid++maybe "" ((","++).show) peerPid
                    matching ownerRows descriptorRows
                      | ("n"++source) `elem` descriptorRows=ownerRows++descriptorRows
                      | otherwise=[]
                    fifoRecords ownerRows descriptorRows []=matching ownerRows descriptorRows
                    fifoRecords ownerRows descriptorRows (row:rest)
                      | take 1 row=="p"=matching ownerRows descriptorRows++fifoRecords [row] [] rest
                      | take 1 row=="c"=fifoRecords (ownerRows++[row]) descriptorRows rest
                      | take 1 row=="f"=matching ownerRows descriptorRows++fifoRecords ownerRows [row] rest
                      | otherwise=fifoRecords ownerRows (descriptorRows++[row]) rest
                -- macOS can omit a FIFO from file-path selection; inspect the
                -- owned processes and retain only this FIFO's descriptor records.
                descriptors<-timeout 1000000 (tryIOError (readProcessWithExitCode "lsof" ["-nP","-p",owners,"-Fpcfatn"] ""))
                let attribution=case descriptors of
                      Nothing->"lsof diagnostic timed out"
                      Just (Left err)->show err
                      Just (Right (code,out,err))->show code++" "++T.unpack (T.take 8192 (T.pack (unlines (fifoRecords [] [] (lines out))++err)))
                readiness<-case descriptors of
                  Just (Right (_,out,_))->mapM (fifoReadiness source)
                    (take 16 (ownedFifoReaders (show ownerPid) source (lines out)))
                  _->pure []
                fail ("Source preparation did not reach "++T.unpack expected++" in "++T.unpack action++": "++show actual++"; FIFO descriptors for owned PIDs "++owners++" (omitted/inaccessible descriptors are inconclusive): "++attribution++"; kernel read readiness (fd, (nonblocking, ready)): "++show readiness)) pure
#ifdef darwin_HOST_OS
            drainBeforeClose=action=="modal"
#else
            drainBeforeClose=False
#endif
            readerParked detail=parseMaybe (withObject "worker" (\o->do
              stage<-o .: "stage"
              activity<-o .: "threadStatus"
              pure (stage==("reading-file"::T.Text) && "ThreadBlocked " `T.isPrefixOf` activity))) detail==Just True
            base=addDocument (Just (FileState (takeDirectory source </> "Foreground.hs") (Just "foreground")))
              (newBuffer "foreground") (initialDesktop (80,25))
        (_,attached)<-debuggerEffects runtime core base [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
        writerReady<-newEmptyMVar
        withAsync (readMVar writerReady >> awaitFrame attached) $ \owner->do
          -- Close the gate before waiting/cancelling the owner even when the
          -- baseline owner is blocked in its read. One-byte consumption proves
          -- a real filesystem reader entered, rather than an early load error.
          -- Release the writer even when an assertion fails. Only this bracket
          -- owns it; child processes must not keep it alive.
          drained<-newIORef False
          returned<-bracket (openFd source ReadWrite defaultFileFlags {nonBlock=True,cloexec=True})
            (\gate->(readIORef drained >>= \done->unless done (void (fdWrite gate "y"))) `finally` closeFd gate) $ \gate->do
            _<-fdWrite gate "x"
            -- Do not let the reader observe a FIFO with no writer. Keep the
            -- existing cleanup order: close the gate before joining the owner.
            putMVar writerReady ()
            let awaitConsumed marker=do
                  result<-tryIOError (PosixBytes.fdRead gate 1)
                  case result of
                    Left err | isFullError err->pure ()
                             | otherwise->ioError err
                    Right bytes | BS.length bytes==1->fdWrite gate marker >> threadDelay 1000 >> awaitConsumed marker
                    _->fail "Local source gate lost its writer"
            entered<-timeout 1000000 (awaitConsumed "x")
            unless (maybe False (const True) entered) (fail "Local source worker did not enter its file read")
            queued<-timeout 1000000 (wait owner)
            interruption<-case queued of
              Nothing->pure Nothing
              Just current->do
                loading<-awaitPreparation "preparing" current
                interrupted<-if action=="close" then pure (fst (runCommand Close loading))
                  else if action=="opened" then pure (insertText "!" (addDocument (Just (FileState source (Just "disk"))) (newBuffer "new open") loading))
                  else if action=="modal" || action=="modal-continue" then pure loading {dialog=Just (Dialog "Human draft" (Searching False "draft") [Input "Query" "draft" 5] 0 ["OK"] [])}
                  else snd <$> debuggerEffects runtime core loading [DebugAction action []]
                responsive<-timeout 1000000 (tick interrupted) >>= maybe
                  (fail "Debugger source retirement blocked the UI owner") pure
                if drainBeforeClose then do
                  -- Exercise EOF with an empty FIFO and the exact file reader
                  -- waiting, so a final readable byte cannot wake it.
                  _<-fdWrite gate "y"
                  consumed<-timeout 1000000 (awaitConsumed "y")
                  unless (maybe False (const True) consumed) (fail "Local source worker did not consume the final FIFO byte")
                  parked<-awaitPreparationWhen "preparing" readerParked responsive
                  writeIORef drained True
                  pure (Just parked)
                else pure (Just responsive)
            pure interruption
          _<-timeout 1000000 (wait owner)
          case returned of
            Nothing->fail "Debugger UI owner blocked on local source read"
            Just interrupted->do
              settled<-awaitPreparation (if action=="modal" || action=="modal-continue" then "ready" else "idle") interrupted
              let modalHeld=maybe False (\modal->dialogTitle modal=="Human draft" && focus modal==0 &&
                    case fields modal of [Input "Query" "draft" 5]->True; _->False) (dialog settled) &&
                    activeText settled=="foreground" && length (windows settled)==1
              unless (action/="modal" && action/="modal-continue" || modalHeld)
                (fail "Prepared source replaced a human modal")
              if action=="modal" then do
                resumed<-awaitPreparation "idle" settled {dialog=Nothing}
                unless (activeText resumed=="xy" && (activeDocument resumed >>= fmap filePath.documentFile)==Just source)
                  (fail "Dismissed modal did not adopt its prepared file source")
              else do
                cancelled<-if action=="modal-continue" then snd <$> debuggerEffects runtime core settled [DebugAction "continue" []] else pure settled
                released<-awaitPreparation "idle" cancelled {dialog=Nothing}
                if action=="opened" then do
                  unless (T.isInfixOf "target changed" (status released) && activeText released=="!new open" && fmap (caret.selection) (activeWindow released)==Just 1)
                    (fail "Late source read changed a newly opened dirty target")
                else unless (not (any ((==Just source).fmap filePath.documentFile) (M.elems (buffers released))) &&
                  (action=="close" || activeText released=="foreground"))
                  (fail "Released stale local source read reopened or jumped")
        putStrLn ("delayed local source "++T.unpack action++" checks passed")

-- Failure-only inspection of this process's exact FIFO readers. Own a duplicate
-- before checking inode identity so the original can close without fd-reuse races.
-- Never read bytes, close the reader's descriptor or borrow its Handle.
-- The zero-time fdReady syscall bypasses the GHC I/O-manager/Handle wait, letting
-- a captured failure distinguish kernel readiness from a missed runtime wakeup.
ownedFifoReaders :: String -> FilePath -> [String] -> [Int]
ownedFifoReaders owner source=go "" Nothing False
  where
    go _ _ _ []=[]
    go pid fd readable (row:rest)=case row of
      'p':value->go value Nothing False rest
      'f':value->go pid (readMaybe value) False rest
      'a':value->go pid fd (value=="r") rest
      'n':path | pid==owner,readable,path==source,Just number<-fd->number:go pid fd readable rest
      _->go pid fd readable rest

fifoReadiness :: FilePath -> Int -> IO (Int,Either String (Bool,Bool))
fifoReadiness source number=do
  result<-tryIOError $ bracket (dup (Fd (fromIntegral number))) closeFd $ \owned@(Fd raw)->do
    expected<-getFileStatus source
    actual<-getFdStatus owned
    unless (isNamedPipe actual && fileID actual==fileID expected && deviceID actual==deviceID expected)
      (fail "FIFO descriptor changed before readiness observation")
    nonblocking<-queryFdOption owned NonBlockingRead
    ready<-Device.ready (FD.FD raw (if nonblocking then 1 else 0)) False 0
    pure (nonblocking,ready)
  pure (number,either (Left . show) Right result)
#endif

sourceHandleCheck :: IO ()
sourceHandleCheck=bracket (Fixture.fixture "mcp") Fixture.cleanup $ \(port,_,_)->withDebugger $ \runtime->do
  let core d _=pure (False,d)
      tick=tickDebugger runtime
      await predicate d=timeout 5000000 (loop d) >>= maybe (fail "debugger policy fixture timed out") pure
        where loop current=do next<-tick current; yes<-predicate next; if yes then pure next else threadDelay 1000 >> loop next
  (_,connected)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
  stopped<-await (pure . T.isPrefixOf "Stopped in " . status) connected
  (_,statusReply)<-debuggerTool runtime stopped "debug_status" (object [])
  snapshot<-statusReply >>= either (fail . T.unpack) pure
  generation<-maybe (fail "missing generation") pure (parseMaybe (withObject "status" (.:"generation")) snapshot :: Maybe Int)
  (admitted,reply)<-debuggerTool runtime stopped "debug_inspect" (object ["generation" .= generation,"request" .= ("source"::T.Text),"sourceReference" .= (999::Int)])
  result<-withAsync reply $ \worker->do
    _<-await (\_->maybe False (const True) <$> poll worker) admitted
    wait worker
  unless (either (const True) (const False) result) (fail "Guessed sourceReference must not read adapter source content")
  (known,knownReply)<-debuggerTool runtime stopped "debug_inspect" (object ["generation" .= generation,"request" .= ("source"::T.Text),"sourceReference" .= (9::Int)])
  public<-withAsync knownReply $ \worker->do
    _<-await (\_->maybe False (const True) <$> poll worker) known
    wait worker
  unless (either (const False) (T.isInfixOf "module Generated where" . T.pack . show) public) (fail "Observed human stack source remains inspectable")
  (_,running)<-debuggerEffects runtime core stopped [DebugAction "continue" []]
  resumed<-await (pure . T.isPrefixOf "Running" . status) running
  (_,expired)<-debuggerTool runtime resumed "debug_inspect" (object ["generation" .= generation,"request" .= ("source"::T.Text),"sourceReference" .= (9::Int)])
  denied<-expired
  unless (either (const True) (const False) denied) (fail "Previous stop source handle must expire")

originCheck :: IO ()
originCheck=bracket temporary removeFile $ \path->do
  let authority=takeDirectory path </> "hide-authority"
      origin=authority </> "thc.toml"
      base=(addReadOnly "Source private [9]" "unique private source" (initialDesktop (80,25))) {guestPrivatePaths=[authority]}
      guarded=base {buffers=M.map (\doc->doc {documentOrigin=Just origin}) (buffers base)}
      bid=maybe (error "missing source buffer") id (activeWindow guarded >>= bufferId)
      check label valid=unless valid (fail label)
  check "canonical generated-source origin protects immutable read" (protectedBuffer guarded bid && sanitizedBuffer guarded bid==Nothing)
  let view=maybe (error "missing origin source view") id (activeWindow guarded)
  check "shared model origin policy protects public window metadata" (lookup (windowId view) [(ident,title) | (ident,title,_,_)<-editorWindowEntries guarded {streamerMode=True}]==Just "Private buffer")
  check "shared origin policy protects Streamer application title" (applicationTitle "/tmp" guarded {streamerMode=True}=="th [private]")
  check "origin does not grant file/save authority" ((activeDocument guarded >>= documentFile)==Nothing)
  saved<-writeCheckpoint path guarded
  either (fail . show) pure saved
  restored<-readCheckpoint path (initialDesktop (80,25)) {guestPrivatePaths=[authority]} >>= either (fail . show) pure
  check "recovery preserves generated-source privacy association" ((activeDocument restored >>= documentOrigin)==Just origin && protectedBuffer restored bid && sanitizedBuffer restored bid==Nothing)
  putStrLn "generated source origin and recovery checks passed"
  where temporary=do root<-getTemporaryDirectory; (path,h)<-openTempFile root "hide-debug-origin"; hClose h; pure path

-- The human source route must attach provenance before any public read can see it.
generatedOriginCheck :: IO ()
generatedOriginCheck=bracket (Fixture.fixture "source-origin") Fixture.cleanup $ \(port,path,_)->withDebugger $ \runtime->do
  canonical<-canonicalizePath (path<>".hs")
  let core d _=pure (False,d)
      tick=tickDebugger runtime
      await predicate d=timeout 5000000 (loop d) >>= maybe (fail "generated source fixture timed out") pure
        where loop current=do next<-tick current; yes<-predicate next; if yes then pure next else threadDelay 1000 >> loop next
      base=(initialDesktop (80,25)) {guestPrivatePaths=[canonical]}
  (_,connected)<-debuggerEffects runtime core base [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
  stopped<-await (pure . T.isPrefixOf "Stopped in " . status) connected
  let bid=maybe (error "missing generated source buffer") id (activeWindow stopped >>= bufferId)
  unless ((activeDocument stopped >>= documentOrigin)==Just canonical) (fail "Generated DAP source must retain canonical backing origin before publication")
  unless ((activeDocument stopped >>= documentFile)==Nothing && protectedBuffer stopped bid && sanitizedBuffer stopped bid==Nothing) (fail "Generated source origin protects reads without granting file authority")
  (_,statusReply)<-debuggerTool runtime stopped "debug_status" (object [])
  snapshot<-statusReply >>= either (fail . T.unpack) pure
  unless (parseMaybe (withObject "status" (.:"frame")) snapshot==Just Null && parseMaybe (withObject "status" (.:"source")) snapshot==Just Null) (fail "Private frame/source metadata must be omitted as complete records")
  let doc=maybe (error "missing generated source") id (activeDocument stopped)
      window=maybe (error "missing generated view") id (activeWindow stopped)
  version<-captureVersion (documentBuffer doc)
  let captured=DebugSourceRequest AddSourceWatch (windowId window) bid version (selection window) Nothing Nothing 1 (Just "unique private expression") False
  (_,prompted)<-debuggerEffects runtime core stopped [DebugSourceAction captured]
  watchPrompt<-maybe (fail "private source watch prompt missing") pure (dialog prompted)
  let r=case fieldRects prompted watchPrompt of value:_->value; _->error "missing watch field geometry"
      x=left r+3; y=top r+1
  unless (not (streamerReadableAt prompted x y) && not (readableAt prompted x y) && guestModalBlocked prompted) (fail "Frozen private source expression must be masked and protected in watch prompt")
  unless (not (streamerReadableAt prompted {guestPrivatePaths=[]} x y)) (fail "Captured watch privacy must survive later policy removal")
  epoch<-maybe (fail "missing generation") pure (parseMaybe (withObject "status" (.:"generation")) snapshot :: Maybe Int)
  (_,reply)<-debuggerTool runtime stopped "debug_inspect" (object ["generation" .= epoch,"request" .= ("source"::T.Text),"sourceReference" .= (9::Int)])
  result<-reply
  unless (either (const True) (const False) result) (fail "Observed private source handle must refuse adapter read")
  (_,breakReply)<-debuggerTool runtime stopped "debug_set_breakpoints" (object ["generation" .= epoch,"bufferId" .= bid,"lines" .= ([1]::[Int])])
  breakResult<-breakReply
  unless (either (T.isInfixOf "private") (const False) breakResult) (fail "Private generated source must refuse agent breakpoint mutation")
  (stack,stackReply)<-debuggerTool runtime stopped "debug_inspect" (object ["generation" .= epoch,"request" .= ("stackTrace"::T.Text)])
  withAsync stackReply $ \worker->do
    _<-await (\_->maybe False (const True) <$> poll worker) stack
    resultValue<-wait worker >>= either (fail . T.unpack) pure
    let frames=parseMaybe (withObject "reply" (\o->o .: "body" >>= withObject "body" (.:"stackFrames"))) resultValue :: Maybe [Value]
    unless (frames==Just []) (fail "Private stack frames must be omitted as complete records")
  withAsync (debuggerSidebarRead runtime (DebugPageRequest epoch (DebugStack 7) 0)) $ \worker->do
    _<-await (\_->maybe False (const True) <$> poll worker) stopped
    page<-wait worker >>= either (fail . T.unpack) pure
    let paths=parseMaybe (withObject "page" (\o->do
          rows<-o .: "stackFrames" :: Parser [Value]
          traverse (withObject "frame" (\row->row .: "source" >>= withObject "source" (.:"path"))) rows)) page :: Maybe [FilePath]
    unless (paths==Just [canonical]) (fail "Shared Debug provider page must retain canonical source resource provenance")
  (_,requestedChooser)<-debuggerEffects runtime core stopped [DebugAction "stack" []]
  chooser<-await (pure . maybe False ((=="Call stack").dialogTitle) . dialog) requestedChooser
  frameDialog<-maybe (fail "human frame chooser missing") pure (dialog chooser)
  let frameRect=dialogRect chooser frameDialog; fx=left frameRect+2; fy=top frameRect+2
  unless (not (readableAt chooser fx fy) && not (streamerReadableAt chooser fx fy) && guestModalBlocked chooser) (fail "Human frame chooser metadata must remain private until canonical row projection exists")
  putStrLn "actual generated source origin checks passed"

-- Current path policy is checked at owner admission and again at late publication.
delayedPolicyCheck :: IO ()
delayedPolicyCheck=bracket (Fixture.fixture "source-policy-delay") Fixture.cleanup $ \(port,path,_)->withDebugger $ \runtime->do
  canonical<-canonicalizePath (path<>".hs")
  let core d _=pure (False,d)
      tick=tickDebugger runtime
      await predicate d=timeout 5000000 (loop d) >>= maybe (fail "delayed source fixture timed out") pure
        where loop current=do next<-tick current; yes<-predicate next; if yes then pure next else threadDelay 1000 >> loop next
      requests=length . filter (T.isInfixOf "\"command\": \"source\"") . T.lines . T.pack <$> readFile path
  (_,connected)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
  stopped<-await (pure . T.isPrefixOf "Stopped in " . status) connected
  (_,statusReply)<-debuggerTool runtime stopped "debug_status" (object [])
  snapshot<-statusReply >>= either (fail . T.unpack) pure
  epoch<-maybe (fail "missing generation") pure (parseMaybe (withObject "status" (.:"generation")) snapshot :: Maybe Int)
  let args=object ["generation" .= epoch,"request" .= ("source"::T.Text),"sourceReference" .= (9::Int)]
  (queued,reply)<-debuggerTool runtime stopped "debug_inspect" args
  withAsync reply $ \worker->do
    _<-await (\_->maybe False (const True) <$> poll worker) queued {guestPrivatePaths=[canonical]}
    result<-wait worker
    unless (either (const True) (const False) result) (fail "Policy change before owner admission must refuse source read")
  count<-requests
  unless (count==1) (fail "Refused source admission must not issue DAP source")
  (late,lateReply)<-debuggerTool runtime stopped "debug_inspect" args
  withAsync lateReply $ \worker->do
    pending<-await (\_->(>=2) <$> requests) late
    writeFile (path<>".release") "release"
    _<-await (\_->maybe False (const True) <$> poll worker) pending {guestPrivatePaths=[canonical]}
    result<-wait worker
    unless (either (const True) (const False) result) (fail "Policy change while adapter source is pending must refuse late body")
  putStrLn "delayed debugger source policy checks passed"

sourceStampCheck :: IO ()
sourceStampCheck=bracket (Fixture.fixture "source-stamp") Fixture.cleanup $ \(port,path,_)->withDebugger $ \runtime->do
  let core d _=pure (False,d)
      tick=tickDebugger runtime
      await predicate d=timeout 5000000 (loop d) >>= maybe (fail "source stamp fixture timed out") pure
        where loop current=do next<-tick current; yes<-predicate next; if yes then pure next else threadDelay 1000 >> loop next
      requests=length . filter (T.isInfixOf "\"command\": \"source\"") . T.lines . T.pack <$> readFile path
  (_,connected)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
  stopped<-await (pure . T.isPrefixOf "Stopped in " . status) connected
  (_,statusReply)<-debuggerTool runtime stopped "debug_status" (object [])
  snapshot<-statusReply >>= either (fail . T.unpack) pure
  epoch<-maybe (fail "missing generation") pure (parseMaybe (withObject "status" (.:"generation")) snapshot :: Maybe Int)
  (queued,reply)<-debuggerTool runtime stopped "debug_inspect" (object ["generation" .= epoch,"request" .= ("source"::T.Text),"sourceReference" .= (9::Int)])
  withAsync reply $ \worker->do
    pending<-await (\_->(>=2) <$> requests) queued
    (stack,stackReply)<-debuggerTool runtime pending "debug_inspect" (object ["generation" .= epoch,"request" .= ("stackTrace"::T.Text)])
    withAsync stackReply $ \stackWorker->do
      _<-await (\_->maybe False (const True) <$> poll worker) stack
      result<-wait worker
      unless (either (const True) (const False) result) (fail "Reused reference with changed source provenance must refuse old body")
      _<-wait stackWorker
      let doc=maybe (error "missing retained source") id (activeDocument stopped)
          window=maybe (error "missing retained source window") id (activeWindow stopped)
          bid=maybe (error "missing source id") id (bufferId window)
      version<-captureVersion (documentBuffer doc)
      (_,refused)<-debuggerEffects runtime core stopped [DebugSourceAction (DebugSourceRequest AddSourceWatch (windowId window) bid version (selection window) Nothing Nothing 1 (Just "old buffer expression") False)]
      unless ("no live debugger source" `T.isInfixOf` status refused && dialog refused==Nothing) (fail "Reobserved numeric handle must not authorize the previous generated buffer")
  putStrLn "source observation stamp check passed"

-- The ordinary transport owns admission and late replies. A scalar supplementary
-- character distinguishes DAP UTF-16 coordinates from editor scalar offsets.
locationsCheck :: IO ()
locationsCheck=bracket (Fixture.fixture "locations") Fixture.cleanup $ \(port,path,_)->do
  canonical<-canonicalizePath(path<>".hs")
  BS.writeFile canonical (TE.encodeUtf8 "first\nα界🐈Z\n")
  Right(file,image)<-loadFile canonical
  withDebugger $ \runtime->do
    let core d _=pure(False,d)
        tick=tickDebugger runtime
        await label predicate start=timeout 5000000 (loop start) >>= maybe(fail label) pure
          where loop d=do next<-tick d; done<-predicate next; if done then pure next else threadDelay 1000 >> loop next
        facts d=do (_,reply)<-debuggerTool runtime d "debug_status" (object[]);reply >>= either(fail.T.unpack) pure
        runReply d reply=withAsync reply $ \pending->do
          completed<-await "locations reply timeout" (\_->maybe False (const True) <$> poll pending) d
          result<-wait pending
          pure(completed,result)
        request d row column=do
          state<-facts d
          pure(object["generation" .= (locationField "generation" state::Maybe Int),"request" .= ("breakpointLocations"::T.Text),"bufferId" .= (activeWindow d >>= bufferId),"line" .= (row::Int),"column" .= (column::Int)])
        query d row column=do
          args<-request d row column
          (queued,reply)<-debuggerTool runtime d "debug_inspect" args
          runReply queued reply
        reject label outcome=unless(either(const True)(const False)outcome)(fail label)
        locationRequests=do
          bytes<-BS.readFile path
          pure [value|line<-BS.split 10 bytes,Just value<-[decodeStrict' line::Maybe Value],(locationField "request" value >>= locationField "command")==Just("breakpointLocations"::T.Text)]
        source=moveTo False 8 (addDocument (Just file) image (initialDesktop(80,25)))
    (background,_)<-debuggerTool runtime source "debug_present" (object["follow" .= False])
    (_,attached)<-debuggerEffects runtime core background [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
    ready<-await "locations adapter did not configure" (\d->do state<-facts d;pure(locationField "configured" state==Just True && locationField "stopped" state==Just True)) attached
    (queried,result)<-query ready 2 3
    body<-either(fail.T.unpack)(pure . locationField "body")result
    unless ((body >>= locationField "breakpoints"::Maybe[Value])==Just[object["line" .= (2::Int),"column" .= (3::Int),"endLine" .= (2::Int),"endColumn" .= (5::Int)]]) (fail "locations tool lost one-based span coordinates")
    unless (fmap selection(activeWindow queried)==Just(Selection 8 8) && dialog queried==Nothing) (fail "read-only locations query moved source or opened UI")
    let human d=do
          window<-maybe(fail "missing source window") pure(activeWindow d)
          doc<-maybe(fail "missing source document") pure(activeDocument d)
          version<-captureVersion(documentBuffer doc)
          let captured=DebugSourceRequest SourceBreakpointLocations (windowId window) (maybe(-1)id(bufferId window)) version (selection window) (Just canonical) (Just canonical) 2 Nothing False
          (_,loading)<-debuggerEffects runtime core d [DebugSourceAction captured]
          await "human locations chooser absent" (pure . maybe False ((=="Executable locations (1-based)").dialogTitle) . dialog) loading
        selectSpan d=case dialog d of
          Nothing->fail "missing executable span chooser"
          Just dg->do let (submitted,effects)=submitDialog 0 dg d
                      snd <$> debuggerEffects runtime core submitted effects
    chooser<-human queried
    unless (case dialog chooser of Just dg | DebugLocationsDialog key<-purpose dg->not(guestCommandAllowed(DebugCommand key));_->False) (fail "location chooser must retain human-only authority")
    unless (case dialog chooser of Just dg->[values|ListBox _ values _<-fields dg]==[["2:3–2:5"]];_->False) (fail "human locations did not label DAP one-based coordinates")
    selected<-selectSpan chooser
    unless (fmap selection(activeWindow selected)==Just(Selection 8 9)) (fail "executable span must select one supplementary scalar using UTF-16 coordinates")
    cancelledChooser<-human (moveTo False 8 selected)
    cancelledDialog<-maybe(fail "cancel chooser absent")pure(dialog cancelledChooser)
    let (cancelledInput,cancelEffects)=submitDialog 1 cancelledDialog cancelledChooser
        (_,replayEffects)=submitDialog 0 cancelledDialog cancelledChooser
    (_,cancelled)<-debuggerEffects runtime core cancelledInput cancelEffects
    (_,replayed)<-debuggerEffects runtime core cancelled replayEffects
    unless (fmap selection(activeWindow replayed)==Just(Selection 8 8)) (fail "cancelled location chooser or replay moved source")
    staleChooser<-human (moveTo False 8 selected)
    let edited=insertText "!" staleChooser
        editedSelection=selection <$> activeWindow edited
    staleSelection<-selectSpan edited
    unless (fmap selection(activeWindow staleSelection)==editedSelection && "expired" `T.isInfixOf` status staleSelection) (fail "stale chooser selected coordinates in changed source")
    (_,emptyResult)<-query queried 2 6
    unless ((emptyResult >>= \value->maybe(Left "missing body")Right(locationField "body" value) >>= \emptyBody->maybe(Left "missing spans")Right(locationField "breakpoints" emptyBody::Maybe[Value]))==Right[]) (fail "empty executable locations must remain a successful empty result")
    mapM_ (\(row,column)->query queried row column >>= reject "malformed or oversized locations accepted" . snd) [(1,2),(1,3)]
    countBefore<-length <$> locationRequests
    mapM_ (\(row,column)->query queried row column >>= reject "invalid query position admitted" . snd) [(0,1),(2,0),(2,4),(4,1),(2,999)]
    (_,privateReply)<-query queried {guestPrivatePaths=[canonical]} 2 3
    reject "private source locations admitted" privateReply
    (_,unownedReply)<-query (addReadOnly "Unowned source [9]" "first\nα界🐈Z\n" queried) 2 3
    reject "unobserved source label granted adapter authority" unownedReply
    (_,dirtyReply)<-query (insertText "!" queried) 2 3
    reject "unsaved source locations admitted" dirtyReply
    countAfter<-length <$> locationRequests
    unless(countAfter==countBefore)(fail "refused private/dirty queries reached adapter")
    -- The peer holds line 1/column 1 until a subsequent threads request. Its
    -- logged request, rather than a timer, proves exact query admission.
    mapM_ (\mode->do
      alive<-newIORef True
      args<-request queried 1 1
      before<-length <$> locationRequests
      let caller=readIORef alive >>= \current->pure(if current then Right() else Left "Attachment retired.")
      (queued,reply)<-debuggerToolWithCaller caller runtime queried "debug_inspect" args
      withAsync reply $ \pending->do
        admitted<-await "held locations request not admitted" (\_->(>before).length <$> locationRequests) queued
        let current=case mode of 0->insertText "!" admitted;1->admitted {guestPrivatePaths=[canonical]};_->admitted
        when(mode==2)(writeIORef alive False)
        state<-facts current
        (barrier,finish)<-debuggerTool runtime current "debug_inspect" (object["generation" .= (locationField "generation" state::Maybe Int),"request" .= ("threads"::T.Text)])
        (released,_)<-runReply barrier finish
        _<-await "held query did not finish" (\_->maybe False(const True) <$> poll pending) released
        wait pending >>= reject "late source/private/attachment change leaked executable spans") [0::Int,1,2]
    putStrLn "Executable location identity, privacy, UTF-16 and bounded-result checks passed"

locationField :: FromJSON a => Key -> Value -> Maybe a
locationField key=parseMaybe(withObject "locations response" (.:key))
