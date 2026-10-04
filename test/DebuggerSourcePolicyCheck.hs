{-# LANGUAGE OverloadedStrings #-}
module DebuggerSourcePolicyCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync,poll,wait)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import System.Timeout (timeout)
import qualified DebuggerCheck as Fixture
import Hide.Debugger
import Hide.Model
import qualified Data.Map.Strict as M
import System.Directory (getTemporaryDirectory,removeFile)
import System.IO (openTempFile,hClose)
import Hide.Buffer
import Hide.GuestAccess
import Hide.Recovery

checks :: IO ()
checks=sourceHandleCheck >> originCheck >> putStrLn "Debugger source policy checks passed"

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
