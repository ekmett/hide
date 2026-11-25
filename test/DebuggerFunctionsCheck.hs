-- SPDX-FileCopyrightText: 2026 Edward Kmett
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
module DebuggerFunctionsCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket,finally)
import Control.Monad (unless,when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified DebuggerCheck as Fixture
import Hide.Debugger
import Hide.Files (loadFile)
import Hide.GuestAccess
import Hide.Model
import System.Directory (doesFileExist,removeFile)
import System.Timeout (timeout)

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe(withObject "function fixture" (.:key))
check :: String -> Bool -> IO ()
check label ok=unless ok(fail label)
checks :: IO ()
checks=bracket(Fixture.fixture "functions") Fixture.cleanup $ \(port,path,_)->flip finally (doesFileExist(path<>".json") >>= \exists->when exists(removeFile(path<>".json"))) $ do
  Right(file,image)<-loadFile(path<>".hs")
  withDebugger $ \runtime->do
    let core d _=pure(False,d)
        tick=tickDebugger runtime
        await label predicate initial=timeout 5000000(loop initial) >>= maybe(fail label)pure
          where loop d=do next<-tick d;done<-predicate next;if done then pure next else threadDelay 1000 >> loop next
        facts d=do (_,reply)<-debuggerTool runtime d "debug_status"(object[]);reply >>= either(fail.T.unpack)pure
        send name values d=snd <$> debuggerEffects runtime core d [DebugAction name values]
        form d=maybe(error "function dialog absent")id(dialog d)
        submit button d=let (next,effects)=submitDialog button(form d)d in snd <$> debuggerEffects runtime core next effects
        list d=send "function-breakpoints" [] d {dialog=Nothing}
        add name d=do
          opened<-list d >>= submit 0
          let dg=form opened
          submit 0 opened {dialog=Just dg {fields=[Input "Function name" name(T.length name),CheckBox "Enabled" True]}}
        edit name d=do
          opened<-list d >>= submit 1
          let dg=form opened
          submit 0 opened {dialog=Just dg {fields=[Input "Function name" name(T.length name),CheckBox "Enabled" True]}}
        labels d=concat[rows|ListBox _ rows _<-fields(form d)]
        kind value d=do
          opened<-list d >>= submit 5
          let dg=form opened
          submit 0 opened {dialog=Just dg {fields=[Radio "Kind" ["Source","Function"] value]}}
        requests=do
          bytes<-BS.readFile path
          pure[value|line<-BS.split 10 bytes,Just value<-[decodeStrict' line::Maybe Value]]
        calls session command=do
          rows<-requests
          pure[req|row<-rows,field "session" row==Just(session::Int),Just req<-[field "request" row],field "command" req==Just(command::T.Text)]
        names req=maybe[]id(field "arguments" req >>= field "breakpoints"::Maybe[Value])
        launch adapter d=do
          putStrLn("Function fixture adapter launch: "++T.unpack adapter)
          BL.writeFile(path<>".json")(encode(object["host" .= ("127.0.0.1"::T.Text),"port" .= (read port::Int),"adapterId" .= (adapter::T.Text),"request" .= ("attach"::T.Text)]))
          started<-send "launch-config" ["1",T.pack(path<>".json")] d {dialog=Nothing}
          await "function adapter did not stop" (\next->do s<-facts next;pure(field "configured" s==Just True && field "stopped" s==Just True)) started
        disconnect d=do
          closing<-send "disconnect" [] d {dialog=Nothing}
          await "function adapter did not fully disconnect" (\next->do s<-facts next;pure(field "active" s==Just False && field "connected" s==Just False)) closing
        replyList d=do
          opened<-list d >>= submit 4
          pure(opened,labels opened)
        awaitReply expected d=await "function set reply did not become current" (\next->do
          (_,rows)<-replyList next
          pure(any(T.isInfixOf expected)rows)) d
        source=addDocument(Just file)image(initialDesktop(100,35))
        sourceWindow=maybe(error "source window absent")windowId(activeWindow source)
    (quiet,_)<-debuggerTool runtime source "debug_present"(object["follow" .= False])
    withName<-add "Alpha" quiet
    check "human catalogue stores literal name" (labels withName==["Alpha"])
    empty<-add "   " withName
    check "empty literal is refused" ("1–512" `T.isInfixOf` status empty)
    duplicate<-add " Alpha " empty
    check "duplicate trimmed literal is refused" ("already" `T.isInfixOf` status duplicate)
    oversized<-add (T.replicate 513 "x") duplicate
    check "oversized literal is refused" ("1–512" `T.isInfixOf` status oversized)
    multiline<-add "Main.foo\nMain.bar" oversized
    check "multiline literal is refused" ("1–512" `T.isInfixOf` status multiline)

    let Rect dialogX dialogY _ _=dialogRect withName(form withName)
    check "function form is protected from guests" (not(guestCommandAllowed(DebugCommand "function-breakpoints")) && not(readableAt withName (dialogX+2)(dialogY+2)))
    -- Retain an editor draft when a newer catalogue invalidates its receipt.
    oldEdit<-list withName >>= submit 1
    newer<-add "Other" oldEdit
    let oldForm=form oldEdit
        (replayedInput,replayedEffects)=submitDialog 0 oldForm newer {dialog=Just oldForm}
    (_,refused)<-debuggerEffects runtime core replayedInput replayedEffects
    check "stale function editor refuses overwrite" ("changed" `T.isInfixOf` status refused)
    cleaned<-list refused >>= submit 3
    check "remove preserves remaining item" (labels cleaned==["Other"])
    renamed<-edit "Alpha" cleaned
    version<-facts renamed
    bid<-maybe(fail "source buffer missing")pure(activeWindow renamed >>= bufferId)
    (sourceStored,sourceReply)<-debuggerTool runtime renamed {dialog=Nothing} "debug_set_breakpoints"(object["generation" .= (field "generation" version::Maybe Int),"bufferId" .= bid,"lines" .= [1::Int]])
    _<-sourceReply >>= either(fail.T.unpack)pure
    first<-launch "hdb" sourceStored
    sourceCalls<-calls 1 "setBreakpoints"
    functionCalls<-calls 1 "setFunctionBreakpoints"
    check "hdb defaults to sources and never sends an empty function set afterward" (not(null sourceCalls) && null functionCalls)
    blocked<-kind 1 first
    check "paused hdb cannot change launch kind" ("still connected" `T.isInfixOf` status blocked)
    inactive<-list blocked >>= submit 2 >>= submit 2
    functionCallsAfter<-calls 1 "setFunctionBreakpoints"
    check "inactive function edits issue no DAP" (null functionCallsAfter)
    closed<-disconnect inactive
    chosen<-kind 1 closed
    second<-launch "graalvm" chosen
    _<-awaitReply "Alpha.hs" second
    bothSources<-calls 2 "setBreakpoints"
    bothFunctions<-calls 2 "setFunctionBreakpoints"
    check "other capable adapter preserves and applies both lists" (not(null bothSources) && any((==[object["name" .= ("Alpha"::T.Text)]]).names)bothFunctions)
    hidden<-facts second
    check "function diagnostics stay private in agent status" (field "output" hidden==Just(""::T.Text) && field "outputUnavailable" hidden/=(Nothing::Maybe T.Text))
    beta<-edit "Beta" second
    admitted<-await "pending Beta request absent" (\_->any((==[object["name" .= ("Beta"::T.Text)]]).names) <$> calls 2 "setFunctionBreakpoints") beta
    gamma<-edit "Gamma" admitted
    beforeControl<-calls 2 "continue"
    waiting<-send "continue" [] gamma
    afterControl<-calls 2 "continue"
    check "rapid edits cannot resume before latest function set settles" (length beforeControl==length afterControl && "Wait" `T.isPrefixOf` status waiting)
    beforeRelease<-calls 2 "setFunctionBreakpoints"
    check "function updates coalesce while one request is pending" (not(any((==[object["name" .= ("Gamma"::T.Text)]]).names)beforeRelease))
    current<-facts gamma
    (barrier,finish)<-debuggerTool runtime gamma {dialog=Nothing} "debug_inspect"(object["generation" .= (field "generation" current::Maybe Int),"request" .= ("threads"::T.Text)])
    _<-pure finish
    gammaReady<-awaitReply "Gamma.hs" barrier
    (_,currentRows)<-replyList gammaReady
    check "late duplicate old reply cannot replace current function results" (not(any(T.isInfixOf "STALE_FUNCTION")currentRows))
    missing<-edit "missing" gammaReady >>= awaitReply "unverified"
    ambiguous<-edit "ambiguous" missing >>= awaitReply "unverified"
    multi<-edit "Main.multi" ambiguous
    multiReady<-awaitReply "Multi.hs:13" multi
    (multiList,_)<-replyList multiReady
    catalogue<-list multiList
    check "multiple locations are not assigned to individual requested names" (labels catalogue==["Main.multi"])
    renamedAgain<-edit "Gamma" catalogue >>= awaitReply "Gamma.hs"
    secondClosed<-disconnect renamedAgain
    third<-launch "hdb" secondClosed
    thirdReady<-awaitReply "Gamma.hs" third
    exclusiveSources<-calls 3 "setBreakpoints"
    check "mixed restart applies only selected hdb function kind without deleting source preferences" (null exclusiveSources)
    toggledSource<-send "breakpoint" [] (focusWindow sourceWindow thirdReady {dialog=Nothing})
    restoredSource<-send "breakpoint" [] toggledSource
    afterSourceEdits<-calls 3 "setBreakpoints"
    check ("source edits remain inactive and never clear hdb function sites: "++show(length afterSourceEdits,status restoredSource)) (null afterSourceEdits && "inactive" `T.isInfixOf` status restoredSource)
    disabled<-list restoredSource >>= submit 2
    _<-await "disabling function did not send empty replacement" (\_->any(null.names) <$> calls 3 "setFunctionBreakpoints") disabled
    enabled<-list disabled >>= submit 2
    enabledReady<-awaitReply "Gamma.hs" enabled
    rejected<-edit "refused" enabledReady
    failed<-await "refused function replacement did not retire its transport" (\next->do s<-facts next;pure(field "active" s==Just False && field "connected" s==Just False)) rejected
    failedCatalogue<-list failed
    check "failed replacement preserves requested names" (labels failedCatalogue==["refused"])
    failureState<-facts failedCatalogue
    check "failed replacement preserves source records" (maybe False (not.null)(field "breakpoints" failureState::Maybe[Value]))
    offlineRestored<-edit "Gamma" failedCatalogue
    thirdClosed<-disconnect offlineRestored
    sourceKind<-kind 0 thirdClosed
    fourth<-launch "hdb" sourceKind
    fourthSources<-calls 4 "setBreakpoints"
    fourthFunctions<-calls 4 "setFunctionBreakpoints"
    check "source-kind restart restores source list and retains inactive function names" (not(null fourthSources) && null fourthFunctions)
    finalCatalogue<-list fourth
    check "function names survive both adapter switches and kind restarts" (labels finalCatalogue==["Gamma"])
    fourthClosed<-disconnect finalCatalogue
    unsupported<-launch "graalvm" fourthClosed
    unsupportedList<-list unsupported
    unsupportedCalls<-calls 5 "setFunctionBreakpoints"
    check "unsupported adapter retains names without issuing function requests"
      (labels unsupportedList==["Gamma"] && null unsupportedCalls && any(T.isInfixOf "does not support")(body(form unsupportedList)))
    _<-disconnect unsupportedList
    putStrLn "Function catalogue, hdb exclusivity, adapter restart, privacy and reply-lifetime checks passed"
