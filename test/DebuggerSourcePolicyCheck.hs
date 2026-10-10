{-# LANGUAGE CPP, OverloadedStrings #-}
module DebuggerSourcePolicyCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync,poll,wait)
import Control.Exception (bracket,finally)
import Control.Monad (unless)
import Data.Aeson
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
import Data.IORef
import Control.Monad (void)
import Hide.Files (FileState(..))
import System.Posix.Files (createNamedPipe)
import qualified System.Posix.IO.ByteString as PosixBytes
import System.Posix.IO (openFd,closeFd,fdWrite,OpenMode(ReadWrite),defaultFileFlags,nonBlock,cloexec)
import System.IO.Error (tryIOError,isFullError)
#endif
import Hide.Buffer
import Hide.GuestAccess
import Hide.Recovery

checks :: IO ()
checks=localSourceCheck >> missingLocalSourceCheck >> delayedLocalSourceCheck >> sourceHandleCheck >> generatedOriginCheck >> delayedPolicyCheck >> sourceStampCheck >> originCheck >> putStrLn "Debugger source policy checks passed"

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
    scenario action=bracket (Fixture.fixture "local-source") Fixture.cleanup $ \(port,path,_)->do
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
            awaitPreparation expected d=do
              observed<-newIORef ("not polled"::T.Text,""::T.Text)
              let loop current=do
                    next<-tick current
                    (_,reply)<-debuggerTool runtime next "debug_status" (object [])
                    value<-reply >>= either (fail . T.unpack) pure
                    phase<-maybe (fail "Missing source preparation phase") pure
                      (parseMaybe (withObject "status" (.:"sourcePreparation")) value :: Maybe T.Text)
                    writeIORef observed (phase,status next)
                    if phase==expected then pure next else threadDelay 1000 >> loop next
              timeout 5000000 (loop d) >>= maybe (do
                actual<-readIORef observed
                fail ("Source preparation did not reach "++T.unpack expected++": "++show actual)) pure
            base=addDocument (Just (FileState (takeDirectory source </> "Foreground.hs") (Just "foreground")))
              (newBuffer "foreground") (initialDesktop (80,25))
        (_,attached)<-debuggerEffects runtime core base [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
        withAsync (awaitFrame attached) $ \owner->do
          -- Close the gate before waiting/cancelling the owner even when the
          -- baseline owner is blocked in its read. One-byte consumption proves
          -- a real filesystem reader entered, rather than an early load error.
          -- Final data plus close wakes EOF readiness, also on exception cleanup.
          -- Only this bracket owns the writer; child processes must not keep it alive.
          returned<-bracket (openFd source ReadWrite defaultFileFlags {nonBlock=True,cloexec=True})
            (\gate->void (fdWrite gate "y") `finally` closeFd gate) $ \gate->do
            _<-fdWrite gate "x"
            let readStarted=do
                  result<-tryIOError (PosixBytes.fdRead gate 1)
                  case result of
                    Left err | isFullError err->pure ()
                             | otherwise->ioError err
                    Right bytes | BS.length bytes==1->fdWrite gate "x" >> threadDelay 1000 >> readStarted
                    _->fail "Local source gate lost its writer"
            entered<-timeout 1000000 readStarted
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
                pure (Just responsive)
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
