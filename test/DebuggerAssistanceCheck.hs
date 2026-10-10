{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : DebuggerAssistanceCheck
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Real DAP stops and exact System One tickets own completion. A supplier's
-- delayed answer cannot retain control after a human debugger operation.
module DebuggerAssistanceCheck (checks) where

import qualified DebuggerCheck as Fixture
import Control.Concurrent (yield)
import Control.Concurrent.Async (withAsync,poll,wait)
import Control.Concurrent.MVar
import qualified Control.Concurrent.STM as STM
import Control.Exception (bracket,finally)
import Control.Monad (forM_,unless,void)
import Data.IORef
import Data.Aeson
import Data.Aeson.Types (parseMaybe,Pair)
import Data.List (findIndex)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import System.Timeout (timeout)
import System.Directory (canonicalizePath)
import qualified Hide.DebugAssistance as A
import Hide.Buffer (newBuffer,contentSlice,contentLength)
import qualified Hide.Plugin.Window as W
import qualified Data.Map.Strict as M
import Hide.Debugger
import Hide.Model
import Hide.Plugin.SystemOne
import Hide.SystemOne

checks :: IO ()
checks=projectionPrivacyCheck >> evidencePrivacyCheck >> stepBudgetCheck >> takeoverCheck >> stallRevealCheck >> timeBudgetCheck >> callerRetirementCheck >> historyPrivacyCheck

-- Crop boundaries must not turn a complete known credential into a leaked
-- prefix or suffix. Test the actual buffer/value projections used by the worker.
projectionPrivacyCheck :: IO ()
projectionPrivacyCheck=do
  let secret="provider-secret-0123456789"
      source=A.sourceExcerpt [secret] 1 (newBuffer (T.replicate 250 "x"<>secret<>"\n"))
      raw=object ["name" .= ("value"::T.Text),"value" .= (T.replicate 250 "x"<>secret),"variablesReference" .= (0::Int)]
      output=secret<>T.replicate 2040 "y"
  check "source credential crossing the 256-character boundary is refused" (case source of Left _->True;_->False)
  check "local credential crossing its value crop is omitted" (A.compactVariable [secret] raw==Nothing)
  check "complete output privacy precedes the last-2048-character crop" (A.hasPrivateText [secret] output)
  check "CRLF private text follows ordinary normalized admission"
    (A.hasPrivateText ["private\r\nvalue"] "prefix private\nvalue suffix")
  check "a lazy local remains explicit without target evaluation" (case A.compactVariable []
    (object ["name" .= ("pending"::T.Text),"value" .= ("do not expose"::T.Text),"presentationHint" .= object ["lazy" .= True]]) of
      Just value->field "value" value==Just ("<unevaluated>"::T.Text) && field "unevaluated" value==Just True
      _->False)

-- Host control metadata does not become adapter evidence merely because its
-- action name equals a credential. Adapter values retain privacy at every depth.
evidencePrivacyCheck :: IO ()
evidencePrivacyCheck=withSystemOne $ \owner->do
  calls<-newIORef (0::Int)
  let provider=DecisionProvider description $ \_ use->use (DecisionDriver $ \input _->do
        atomicModifyIORef' calls (\n->(n+1,()))
        pure (Right (choose "next" input)))
      neutral=object ["source" .= object ["lines" .= (["value = 1"]::[T.Text])]]
      historical=object ["generation" .= (1::Int),"action" .= ("next"::T.Text)]
      decide identity evidence recent=do
        cancelled<-STM.newTVarIO False
        ticket<-STM.newTVarIO Nothing
        A.decideAssistance (systemOneServices owner) identity "Investigate the calculation."
          ["root","next"] ["inspect","next"] evidence recent Nothing 1000 cancelled ticket
  void (selectDecisionProvider owner (Just provider) >>= right)
  accepted<-decide "public-history" neutral [historical]
  invoked<-readIORef calls
  check "a retained host action matching a credential still reaches the selected supplier"
    (invoked==1 && case accepted of Right result->A.assistedAction result=="next";_->False)
  forM_ [
    ("source",object ["source" .= object ["lines" .= (["root"]::[T.Text])]],[]),
    ("locals",object ["locals" .= [object ["value" .= ("next"::T.Text)]]],[]),
    ("recent-location",neutral,[object ["location" .= object ["name" .= ("root"::T.Text)]]]),
    ("recent-locals",neutral,[object ["locals" .= [object ["value" .= ("next"::T.Text)]]]]),
    ("adapter-action",object ["locals" .= [object ["action" .= ("root"::T.Text)]]],[]),
    ("recent-adapter-action",neutral,[object ["locals" .= [object ["action" .= ("next"::T.Text)]]]])] $ \(label,evidence,recent)->do
      refused<-decide label evidence recent
      after<-readIORef calls
      check (T.unpack label<>" private adapter evidence is refused before supplier invocation")
        (after==invoked && case refused of Left "private-observation"->True;_->False)

-- A sent step is only an admission: the run must observe this command's new
-- stopped generation before reporting its step budget as completed.
stepBudgetCheck :: IO ()
stepBudgetCheck=withSystemOne $ \owner->do
  entered<-newEmptyMVar
  let provider=DecisionProvider description $ \_ use->use (DecisionDriver $ \input _->do
        void (tryPutMVar entered input)
        pure (Right (choose "next" input)))
  void (selectDecisionProvider owner (Just provider) >>= right)
  session (systemOneServices owner) $ \runtime path initial->do
    before<-state runtime initial
    (started,accepted)<-tool runtime initial "debug_assist"
      ["command" .= ("start"::T.Text),"generation" .= generationOf before,
       "goal" .= ("Find the next source transition."::T.Text),"maxSteps" .= (1::Int)]
    run<-runIdOf accepted
    (finished,value)<-awaitState "exact assisted step budget" runtime
      (\s->pure (sameRun run s && (assistance s >>= field "phase")==Just ("finished"::T.Text))) started
    let outcome=object ["runId" .= (assistance value >>= field "runId" :: Maybe T.Text),
          "phase" .= (assistance value >>= field "phase" :: Maybe T.Text),
          "reason" .= (assistance value >>= field "reason" :: Maybe T.Text),
          "steps" .= (assistance value >>= field "steps" :: Maybe Int),
          "decisions" .= (assistance value >>= field "decisions" :: Maybe Int),
          "generation" .= generationOf value,"stopped" .= (field "stopped" value :: Maybe Bool)]
    check ("step budget waits for a new real DAP stop; terminal assistance: "<>take 2048 (show outcome))
      (field "stopped" value==Just True && generationOf value>generationOf before &&
       (assistance value >>= field "steps")==Just (1::Int) &&
       (assistance value >>= field "reason")==Just ("step-budget"::T.Text))
    input<-barrier "selected supplier was not invoked" (takeMVar entered)
    check "debug assistance sends the explicit goal to the selected supplier"
      ("Find the next source transition." `T.isInfixOf` decisionState input)
    commands<-requests path
    let sources=[arguments | request<-commands,commandOf request==Just "source",Just arguments<-[field "arguments" request :: Maybe Value]]
        selectedSource=field "frame" before >>= field "source" :: Maybe Value
    check "assisted source retrieval carries the exact observed stack source object"
      (not (null sources) && all (\arguments->field "source" arguments==selectedSource) sources)
    check "one admitted next command exhausts maxSteps=1" (map commandOf (filter isStep commands)==[Just "next"])
    check "assistance never forces or mutates values" (all (\r->commandOf r `notElem` [Just "evaluate",Just "setVariable"]) commands)
    _<-tool runtime finished "debug_control" ["generation" .= generationOf value,"command" .= ("disconnect"::T.Text)]
    pure ()

-- The original ticket is canceled while its supplier still holds a successful
-- answer. After an actual manual continue/pause stop, release that answer and
-- drain one correlated DAP inspection response: no stale step may be sent.
takeoverCheck :: IO ()
takeoverCheck=withSystemOne $ \owner->do
  entered<-newEmptyMVar
  stoppedSupplier<-newEmptyMVar
  release<-newEmptyMVar
  returned<-newEmptyMVar
  ticketCell<-newEmptyMVar
  let provider=DecisionProvider description $ \_ use->use (DecisionDriver $ \input stop->do
        putMVar entered input
        STM.atomically (stop >>= STM.check)
        putMVar stoppedSupplier ()
        takeMVar release
        pure (Right (choose "stepIn" input)))
      base=systemOneServices owner
      services=base {requestDecision= \supplier input budget->do
        result<-requestDecision base supplier input budget
        case result of Right ticket->putMVar ticketCell ticket;Left _->pure ()
        pure result}
  -- Provider return is signaled inside its actual call, not by a UI/status tick.
  let observed=provider {withDecisionDriver= \stop use->withDecisionDriver provider stop
        (\driver->use (DecisionDriver $ \input requestStop->
          runDecision driver input requestStop `finally` void (tryPutMVar returned ())))}
  void (selectDecisionProvider owner (Just observed) >>= right)
  (session services $ \runtime path initial->do
    before<-state runtime initial
    (started,accepted)<-tool runtime initial "debug_assist"
      ["command" .= ("start"::T.Text),"generation" .= generationOf before,
       "goal" .= ("Inspect before stepping."::T.Text)]
    run<-runIdOf accepted
    (deciding,_)<-awaitState "assistance supplier entered" runtime
      (const (not <$> isEmptyMVar entered)) started
    ticket<-barrier "assistance ticket was not published" (takeMVar ticketCell)
    (continued,_)<-tool runtime deciding "debug_control"
      ["generation" .= generationOf before,"command" .= ("continue"::T.Text)]
    (running,current)<-awaitState "manual continue receipt" runtime
      (\s->pure (field "stopped" s==Just False && generationOf s>generationOf before)) continued
    cancelled<-barrier "manual takeover did not cancel its exact ticket" (awaitDecision ticket)
    check "manual control cancels the original decision" (cancelled==Left DecisionCancelled)
    barrier "supplier did not observe cancellation" (takeMVar stoppedSupplier)
    (pausing,_)<-tool runtime running "debug_control"
      ["generation" .= generationOf current,"command" .= ("pause"::T.Text)]
    (paused,pausedValue)<-awaitState "manual pause stop" runtime
      (\s->pure (field "stopped" s==Just True && generationOf s>generationOf current && isJust (field "frame" s >>= field "id" :: Maybe Int))) pausing
    putMVar release ()
    barrier "late supplier did not return" (takeMVar returned)
    (inspecting,finish)<-debuggerTool runtime paused "debug_inspect"
      (object ["generation" .= generationOf pausedValue,"request" .= ("threads"::T.Text)])
    withAsync finish $ \reply->do
      (settled,value)<-awaitState "owned DAP stream barrier after late answer" runtime
        (const (isJust <$> poll reply)) inspecting
      void (wait reply >>= right)
      check "manual takeover retains the exact run without executing its stale answer"
        (sameRun run value && (assistance value >>= field "phase")==Just ("paused"::T.Text) &&
         (assistance value >>= field "steps")==Just (0::Int))
      commands<-requests path
      check "a late selected next-action cannot send a DAP step" (null (filter isStep commands))
      _<-tool runtime settled "debug_control" ["generation" .= generationOf value,"command" .= ("disconnect"::T.Text)]
      pure ()) `finally` void (tryPutMVar release ())

-- UI and MCP share one admitted operation. A hidden inspection-only run ends
-- at its repeated-location limit; reveal prepares existing views without an
-- execution command or a second stepping run.
stallRevealCheck :: IO ()
stallRevealCheck=withSystemOne $ \owner->do
  entered<-newEmptyMVar
  let provider=DecisionProvider description $ \_ use->use (DecisionDriver $ \input _->do
        void (tryPutMVar entered input)
        pure (Right (choose "inspect" input)))
  void (selectDecisionProvider owner (Just provider) >>= right)
  session (systemOneServices owner) $ \runtime path initial->do
    (hidden,_)<-tool runtime initial "debug_present" ["follow" .= False]
    (_,form)<-debuggerEffects runtime (\d _->pure (False,d)) hidden [DebugAction "assist" []]
    action<-case dialog form of
      Just dg | DebugDialog captured<-purpose dg->pure captured
      _->fail "Assisted debugger goal dialog was not presented"
    (_,started)<-debuggerEffects runtime (\d _->pure (False,d)) (form {dialog=Nothing})
      [DebugAction action ["0","Find useful runtime evidence.","20","60000"]]
    admitted<-state runtime started
    run<-runIdOf admitted
    (finished,value)<-awaitState "assisted inspection stall limit" runtime
      (\s->pure (sameRun run s && (assistance s >>= field "phase")==Just ("finished"::T.Text))) started
    input<-barrier "UI start did not reach the shared provider operation" (takeMVar entered)
    check "UI supplied the goal through the shared operation" ("Find useful runtime evidence." `T.isInfixOf` decisionState input)
    check "inspect-only run stops after three repeated-location decisions"
      ((assistance value >>= field "reason")==Just ("stall"::T.Text) &&
       (assistance value >>= field "steps")==Just (0::Int) &&
       (assistance value >>= field "decisions")==Just (4::Int))
    let observations=maybe [] id (assistance value >>= field "observations" :: Maybe [Value])
    check "retained evidence names its exact stop and source location"
      (not (null observations) && all (\observation->field "generation" observation==Just (generationOf value) &&
        field "threadId" observation==Just (7::Int) && isJust (field "location" observation :: Maybe Value) &&
        isJust (field "source" observation :: Maybe Value)) observations)
    (revealing,_)<-tool runtime finished "debug_assist" ["command" .= ("reveal"::T.Text)]
    let awaitReport current=do
          next<-tickDebugger runtime current
          let reports=[W.preparedWindowText prepared | prepared<-M.elems (pluginWindows next),
                W.preparedWindowRecovery prepared==Just ("hide.debug-output",1)]
          if any (\content->"Assisted debugger observation" `T.isInfixOf` contentSlice content 0 (min 512 (contentLength content))) reports
            then pure next else yield >> awaitReport next
    shown<-barrier "revealed evidence output-window receipt" (awaitReport revealing)
    public<-state runtime shown
    check "revealing private evidence never rewrites the program's raw output"
      (not ("Assisted debugger observation" `T.isInfixOf` maybe "" id (field "output" public)))
    commands<-requests path
    check "inspect and reveal never send a step, continue, evaluate or mutation"
      (all (\r->commandOf r `notElem` map Just ["next","stepIn","stepOut","continue","evaluate","setVariable"]) commands)
    _<-tool runtime shown "debug_control" ["generation" .= generationOf value,"command" .= ("disconnect"::T.Text)]
    pure ()

-- The injected monotonic deadline expires while the actual supplier is holding
-- an answer. This is a real cancellation receipt, not a sleep/status heuristic.
timeBudgetCheck :: IO ()
timeBudgetCheck=withSystemOne $ \owner->do
  clock<-newIORef (0::Integer)
  entered<-newEmptyMVar
  returned<-newEmptyMVar
  ticketCell<-newEmptyMVar
  let base=systemOneServices owner
      services=base {requestDecision= \supplier input budget->do
        result<-requestDecision base supplier input budget
        case result of Right ticket->putMVar ticketCell ticket;_->pure ()
        pure result}
      provider=DecisionProvider description $ \_ use->use (DecisionDriver $ \input stop->do
        putMVar entered ()
        STM.atomically (stop >>= STM.check)
        putMVar returned ()
        pure (Right (choose "next" input)))
  void (selectDecisionProvider owner (Just provider) >>= right)
  sessionWith (withDebuggerClock (readIORef clock)) services $ \runtime path initial->do
    before<-state runtime initial
    (started,accepted)<-tool runtime initial "debug_assist"
      ["command" .= ("start"::T.Text),"generation" .= generationOf before,
       "goal" .= ("Inspect until the deadline."::T.Text),"budgetMs" .= (1000::Int)]
    run<-runIdOf accepted
    (deciding,_)<-awaitState "supplier entered before run deadline" runtime (const (not <$> isEmptyMVar entered)) started
    ticket<-barrier "run deadline ticket absent" (takeMVar ticketCell)
    writeIORef clock 1000000001
    (finished,value)<-awaitState "exact assisted time budget" runtime
      (\s->pure (sameRun run s && (assistance s >>= field "phase")==Just ("finished"::T.Text))) deciding
    receipt<-barrier "run deadline did not cancel its decision" (awaitDecision ticket)
    check "time budget owns exact ticket cancellation" (receipt==Left DecisionCancelled &&
      (assistance value >>= field "reason")==Just ("time-budget"::T.Text))
    barrier "supplier did not receive deadline cancellation" (takeMVar returned)
    commands<-requests path
    check "a decision held beyond the total deadline cannot step" (null (filter isStep commands))
    _<-tool runtime finished "debug_control" ["generation" .= generationOf value,"command" .= ("disconnect"::T.Text)]
    pure ()

callerRetirementCheck :: IO ()
callerRetirementCheck=withSystemOne $ \owner->do
  active<-newIORef True
  entered<-newEmptyMVar
  ticketCell<-newEmptyMVar
  let base=systemOneServices owner
      services=base {requestDecision= \supplier input budget->do
        result<-requestDecision base supplier input budget
        case result of Right ticket->putMVar ticketCell ticket;_->pure ()
        pure result}
      caller=readIORef active >>= \live->pure (if live then Right () else Left "retired")
      provider=DecisionProvider description $ \_ use->use (DecisionDriver $ \input stop->do
        putMVar entered ()
        STM.atomically (stop >>= STM.check)
        pure (Right (choose "next" input)))
  void (selectDecisionProvider owner (Just provider) >>= right)
  session services $ \runtime path initial->do
    before<-state runtime initial
    (started,reply)<-debuggerToolWithCaller caller runtime initial "debug_assist" (object
      ["command" .= ("start"::T.Text),"generation" .= generationOf before,"goal" .= ("Inspect this connection's stop."::T.Text)])
    accepted<-reply >>= right
    run<-runIdOf accepted
    (deciding,_)<-awaitState "caller-bound supplier entered" runtime (const (not <$> isEmptyMVar entered)) started
    ticket<-barrier "caller-bound ticket absent" (takeMVar ticketCell)
    writeIORef active False
    (finished,value)<-awaitState "caller retirement receipt" runtime
      (\s->pure (sameRun run s && (assistance s >>= field "phase")==Just ("finished"::T.Text))) deciding
    receipt<-barrier "retired caller did not cancel its exact ticket" (awaitDecision ticket)
    check "retirement retains the original run and cancels it" (receipt==Left DecisionCancelled &&
      (assistance value >>= field "reason")==Just ("caller-retired"::T.Text))
    commands<-requests path
    check "retired caller cannot execute a held action" (null (filter isStep commands))
    _<-tool runtime finished "debug_control" ["generation" .= generationOf value,"command" .= ("disconnect"::T.Text)]
    pure ()

-- Historical evidence retains its original source authority. Marking source A
-- private after stepping to B must prevent A's locals/location entering another
-- supplier request, even though B itself remains public.
historyPrivacyCheck :: IO ()
historyPrivacyCheck=withSystemOne $ \owner->do
  decisions<-newIORef (0::Int)
  let provider=DecisionProvider description $ \_ use->use (DecisionDriver $ \input _->do
        modifyIORef' decisions (+1)
        pure (Right (choose "next" input)))
  void (selectDecisionProvider owner (Just provider) >>= right)
  sessionWithMode "assist-history" withDebugger (systemOneServices owner) $ \runtime path initial->do
    privateSource<-canonicalizePath (path<>".hs")
    before<-state runtime initial
    (started,accepted)<-tool runtime initial "debug_assist"
      ["command" .= ("start"::T.Text),"generation" .= generationOf before,
       "goal" .= ("Inspect the next source."::T.Text),"maxSteps" .= (2::Int)]
    run<-runIdOf accepted
    (atSecond,_)<-awaitState "second public source stop" runtime
      (\s->pure ((assistance s >>= field "steps")==Just (1::Int) &&
        (field "frame" s >>= field "source" >>= field "path")==Just path)) started
    (finished,value)<-awaitState "historical source privacy admission" runtime
      (\s->pure (sameRun run s && (assistance s >>= field "phase")==Just ("finished"::T.Text)))
      (atSecond {guestPrivatePaths=privateSource:guestPrivatePaths atSecond})
    calls<-readIORef decisions
    check "a newly private historical source cannot reach a second decision" (calls==1 &&
      (assistance value >>= field "reason")==Just ("observation-expired-or-private"::T.Text))
    let retained=maybe [] id (assistance value >>= field "observations" :: Maybe [Value])
    check "public status omits the now-private prior observation" (null retained)
    commands<-requests path
    check "historical privacy retirement sends only the original step" (length (filter isStep commands)==1)
    _<-tool runtime finished "debug_control" ["generation" .= generationOf value,"command" .= ("disconnect"::T.Text)]
    pure ()

session :: SystemOneServices -> (Debugger -> FilePath -> Desktop -> IO a) -> IO a
session=sessionWith withDebugger

sessionWith :: ((Debugger -> IO a) -> IO a) -> SystemOneServices -> (Debugger -> FilePath -> Desktop -> IO a) -> IO a
sessionWith=sessionWithMode "assist"

sessionWithMode :: String -> ((Debugger -> IO a) -> IO a) -> SystemOneServices -> (Debugger -> FilePath -> Desktop -> IO a) -> IO a
sessionWithMode mode scope services use=bracket (Fixture.fixture mode) Fixture.cleanup $ \(port,path,_)->scope $ \runtime->
  withDebuggerSystemOne services runtime $ do
    (background,_)<-tool runtime (initialDesktop (100,35)) "debug_present" ["follow" .= False]
    (connecting,_)<-tool runtime background "debug_attach" ["port" .= (read port::Int)]
    (ready,_)<-awaitState "ready paused DAP frame" runtime
      (\s->pure (field "stopped" s==Just True && field "ready" s==Just True && field "configured" s==Just True &&
        isJust (field "frame" s >>= field "id" :: Maybe Int))) connecting
    use runtime path ready

tool :: Debugger -> Desktop -> T.Text -> [Pair] -> IO (Desktop,Value)
tool runtime desktop name arguments=do
  (next,reply)<-debuggerTool runtime desktop name (object arguments)
  value<-reply >>= right
  pure (next,value)

state :: Debugger -> Desktop -> IO Value
state runtime desktop=snd <$> tool runtime desktop "debug_status" []

awaitState :: String -> Debugger -> (Value -> IO Bool) -> Desktop -> IO (Desktop,Value)
awaitState label runtime ready initial=barrier label (go initial)
  where
    go desktop=do
      next<-tickDebugger runtime desktop
      value<-state runtime next
      done<-ready value
      if done then pure (next,value) else yield >> go next

barrier :: String -> IO a -> IO a
barrier label action=timeout 10000000 action >>= maybe (fail (label<>" did not complete")) pure

assistance :: Value -> Maybe Value
assistance=field "assistance"

runIdOf :: Value -> IO T.Text
runIdOf value=maybe (fail "Missing admitted assistance runId") pure (assistance value >>= field "runId")

sameRun :: T.Text -> Value -> Bool
sameRun run value=(assistance value >>= field "runId")==Just run

generationOf :: Value -> Int
generationOf value=maybe (error "Missing debugger generation") id (field "generation" value)

requests :: FilePath -> IO [Value]
requests path=do
  rows<-T.lines <$> TIO.readFile path
  traverse (\row->either fail (maybe (fail "Missing fixture request") pure . field "request") (eitherDecodeStrict' (TE.encodeUtf8 row))) rows

commandOf :: Value -> Maybe T.Text
commandOf=field "command"

isStep :: Value -> Bool
isStep request=commandOf request `elem` map Just ["next","stepIn","stepOut"]

choose :: T.Text -> DecisionInput -> DecisionOutput
choose label input=DecisionOutput (ReportedModel "assistance-check") (map answer (decisionQuestions input)) Nothing
  where
    answer question=case questionKind question of
      ChoiceDecision options->case findIndex ((==label).optionLabel) options of
        Just selected->DecisionAnswer (questionName question) ChoiceAnswer
          [if index==selected then 1 else 0 | index<-[0..length options-1]] Nothing
        Nothing->error "Requested test decision is not admissible"
      _->error "Assisted debugger must ask for an admissible action choice"

description :: SupplierDescription
description=SupplierDescription "Assistance check" InProcess (ReportedModel "assistance-check") Nothing 0

field :: FromJSON a => Key -> Value -> Maybe a
field name=parseMaybe (withObject "field" (.:name))

right :: Show e => Either e a -> IO a
right=either (fail . show) pure

check :: String -> Bool -> IO ()
check label condition=unless condition (fail label)
