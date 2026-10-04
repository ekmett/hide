{-# LANGUAGE OverloadedStrings #-}
module DebuggerSourcePolicyCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync,poll,wait)
import Control.Exception (bracket)
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
import Hide.Buffer
import Hide.GuestAccess
import Hide.Recovery

checks :: IO ()
checks=sourceHandleCheck >> generatedOriginCheck >> delayedPolicyCheck >> sourceStampCheck >> originCheck >> putStrLn "Debugger source policy checks passed"

sourceHandleCheck :: IO ()
sourceHandleCheck=bracket (Fixture.fixture "mcp") Fixture.cleanup $ \(port,_,_)->withDebugger $ \runtime->do
  let core d _=pure (False,d)
      tick=tickDebugger runtime core
      await predicate d=timeout 5000000 (loop d) >>= maybe (fail "debugger policy fixture timed out") pure
        where loop current=do next<-tick current; yes<-predicate next; if yes then pure next else threadDelay 1000 >> loop next
  (_,connected)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
  stopped<-await (pure . T.isPrefixOf "Stopped in " . status) connected
  (_,statusReply)<-debuggerTool runtime core stopped "debug_status" (object [])
  snapshot<-statusReply >>= either (fail . T.unpack) pure
  generation<-maybe (fail "missing generation") pure (parseMaybe (withObject "status" (.:"generation")) snapshot :: Maybe Int)
  (admitted,reply)<-debuggerTool runtime core stopped "debug_inspect" (object ["generation" .= generation,"request" .= ("source"::T.Text),"sourceReference" .= (999::Int)])
  result<-withAsync reply $ \worker->do
    _<-await (\_->maybe False (const True) <$> poll worker) admitted
    wait worker
  unless (either (const True) (const False) result) (fail "Guessed sourceReference must not read adapter source content")
  (known,knownReply)<-debuggerTool runtime core stopped "debug_inspect" (object ["generation" .= generation,"request" .= ("source"::T.Text),"sourceReference" .= (9::Int)])
  public<-withAsync knownReply $ \worker->do
    _<-await (\_->maybe False (const True) <$> poll worker) known
    wait worker
  unless (either (const False) (T.isInfixOf "module Generated where" . T.pack . show) public) (fail "Observed human stack source remains inspectable")
  (_,running)<-debuggerEffects runtime core stopped [DebugAction "continue" []]
  resumed<-await (pure . T.isPrefixOf "Running" . status) running
  (_,expired)<-debuggerTool runtime core resumed "debug_inspect" (object ["generation" .= generation,"request" .= ("source"::T.Text),"sourceReference" .= (9::Int)])
  denied<-expired
  unless (either (const True) (const False) denied) (fail "Previous stop source handle must expire")

originCheck :: IO ()
originCheck=bracket temporary removeFile $ \path->do
  let origin="/tmp/hide-authority/thc.toml"
      base=(addReadOnly "Source private [9]" "unique private source" (initialDesktop (80,25))) {guestPrivatePaths=["/tmp/hide-authority"]}
      guarded=base {buffers=M.map (\doc->doc {documentOrigin=Just origin}) (buffers base)}
      bid=maybe (error "missing source buffer") id (activeWindow guarded >>= bufferId)
      check label valid=unless valid (fail label)
  check "canonical generated-source origin protects immutable read" (protectedBuffer guarded bid && sanitizedBuffer guarded bid==Nothing)
  check "origin does not grant file/save authority" ((activeDocument guarded >>= documentFile)==Nothing)
  saved<-writeCheckpoint path guarded
  either (fail . show) pure saved
  restored<-readCheckpoint path (initialDesktop (80,25)) {guestPrivatePaths=["/tmp/hide-authority"]} >>= either (fail . show) pure
  check "recovery preserves generated-source privacy association" ((activeDocument restored >>= documentOrigin)==Just origin && protectedBuffer restored bid && sanitizedBuffer restored bid==Nothing)
  putStrLn "generated source origin and recovery checks passed"
  where temporary=do root<-getTemporaryDirectory; (path,h)<-openTempFile root "hide-debug-origin"; hClose h; pure path

-- The human source route must attach provenance before any public read can see it.
generatedOriginCheck :: IO ()
generatedOriginCheck=bracket (Fixture.fixture "source-origin") Fixture.cleanup $ \(port,path,_)->withDebugger $ \runtime->do
  canonical<-canonicalizePath (path<>".hs")
  let core d _=pure (False,d)
      tick=tickDebugger runtime core
      await predicate d=timeout 5000000 (loop d) >>= maybe (fail "generated source fixture timed out") pure
        where loop current=do next<-tick current; yes<-predicate next; if yes then pure next else threadDelay 1000 >> loop next
      base=(initialDesktop (80,25)) {guestPrivatePaths=[canonical]}
  (_,connected)<-debuggerEffects runtime core base [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
  stopped<-await (pure . T.isPrefixOf "Stopped in " . status) connected
  let bid=maybe (error "missing generated source buffer") id (activeWindow stopped >>= bufferId)
  unless ((activeDocument stopped >>= documentOrigin)==Just canonical) (fail "Generated DAP source must retain canonical backing origin before publication")
  unless ((activeDocument stopped >>= documentFile)==Nothing && protectedBuffer stopped bid && sanitizedBuffer stopped bid==Nothing) (fail "Generated source origin protects reads without granting file authority")
  (_,statusReply)<-debuggerTool runtime core stopped "debug_status" (object [])
  snapshot<-statusReply >>= either (fail . T.unpack) pure
  unless (parseMaybe (withObject "status" (.:"frame")) snapshot==Just Null && parseMaybe (withObject "status" (.:"source")) snapshot==Just Null) (fail "Private frame/source metadata must be omitted as complete records")
  let doc=maybe (error "missing generated source") id (activeDocument stopped)
      window=maybe (error "missing generated view") id (activeWindow stopped)
  version<-captureVersion (documentBuffer doc)
  let captured=DebugSourceRequest AddSourceWatch (windowId window) bid version (selection window) Nothing Nothing 1 (Just "unique private expression") False
  (_,prompted)<-debuggerEffects runtime core stopped [DebugSourceAction captured]
  prompt<-maybe (fail "private source watch prompt missing") pure (dialog prompted)
  let r=case fieldRects prompted prompt of value:_->value; _->error "missing watch field geometry"
      x=left r+3; y=top r+1
  unless (not (streamerReadableAt prompted x y) && not (readableAt prompted x y) && guestModalBlocked prompted) (fail "Frozen private source expression must be masked and protected in watch prompt")
  unless (not (streamerReadableAt prompted {guestPrivatePaths=[]} x y)) (fail "Captured watch privacy must survive later policy removal")
  epoch<-maybe (fail "missing generation") pure (parseMaybe (withObject "status" (.:"generation")) snapshot :: Maybe Int)
  (_,reply)<-debuggerTool runtime core stopped "debug_inspect" (object ["generation" .= epoch,"request" .= ("source"::T.Text),"sourceReference" .= (9::Int)])
  result<-reply
  unless (either (const True) (const False) result) (fail "Observed private source handle must refuse adapter read")
  (_,breakReply)<-debuggerTool runtime core stopped "debug_set_breakpoints" (object ["generation" .= epoch,"bufferId" .= bid,"lines" .= ([1]::[Int])])
  breakResult<-breakReply
  unless (either (T.isInfixOf "private") (const False) breakResult) (fail "Private generated source must refuse agent breakpoint mutation")
  (stack,stackReply)<-debuggerTool runtime core stopped "debug_inspect" (object ["generation" .= epoch,"request" .= ("stackTrace"::T.Text)])
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
      tick=tickDebugger runtime core
      await predicate d=timeout 5000000 (loop d) >>= maybe (fail "delayed source fixture timed out") pure
        where loop current=do next<-tick current; yes<-predicate next; if yes then pure next else threadDelay 1000 >> loop next
      requests=length . filter (T.isInfixOf "\"command\": \"source\"") . T.lines . T.pack <$> readFile path
  (_,connected)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
  stopped<-await (pure . T.isPrefixOf "Stopped in " . status) connected
  (_,statusReply)<-debuggerTool runtime core stopped "debug_status" (object [])
  snapshot<-statusReply >>= either (fail . T.unpack) pure
  epoch<-maybe (fail "missing generation") pure (parseMaybe (withObject "status" (.:"generation")) snapshot :: Maybe Int)
  let args=object ["generation" .= epoch,"request" .= ("source"::T.Text),"sourceReference" .= (9::Int)]
  (queued,reply)<-debuggerTool runtime core stopped "debug_inspect" args
  withAsync reply $ \worker->do
    _<-await (\_->maybe False (const True) <$> poll worker) queued {guestPrivatePaths=[canonical]}
    result<-wait worker
    unless (either (const True) (const False) result) (fail "Policy change before owner admission must refuse source read")
  count<-requests
  unless (count==1) (fail "Refused source admission must not issue DAP source")
  (late,lateReply)<-debuggerTool runtime core stopped "debug_inspect" args
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
      tick=tickDebugger runtime core
      await predicate d=timeout 5000000 (loop d) >>= maybe (fail "source stamp fixture timed out") pure
        where loop current=do next<-tick current; yes<-predicate next; if yes then pure next else threadDelay 1000 >> loop next
      requests=length . filter (T.isInfixOf "\"command\": \"source\"") . T.lines . T.pack <$> readFile path
  (_,connected)<-debuggerEffects runtime core (initialDesktop (80,25)) [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
  stopped<-await (pure . T.isPrefixOf "Stopped in " . status) connected
  (_,statusReply)<-debuggerTool runtime core stopped "debug_status" (object [])
  snapshot<-statusReply >>= either (fail . T.unpack) pure
  epoch<-maybe (fail "missing generation") pure (parseMaybe (withObject "status" (.:"generation")) snapshot :: Maybe Int)
  (queued,reply)<-debuggerTool runtime core stopped "debug_inspect" (object ["generation" .= epoch,"request" .= ("source"::T.Text),"sourceReference" .= (9::Int)])
  withAsync reply $ \worker->do
    pending<-await (\_->(>=2) <$> requests) queued
    (stack,stackReply)<-debuggerTool runtime core pending "debug_inspect" (object ["generation" .= epoch,"request" .= ("stackTrace"::T.Text)])
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
