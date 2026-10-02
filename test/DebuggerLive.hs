{-# LANGUAGE OverloadedStrings #-}
-- Real THC/DAP qualification; see docs/contributing.md#live-debugger-checks.
-- This catches broken source identity, stale stops, and failed termination.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import THC.Edit.App (applyEffects)
import THC.Edit.Debugger
import THC.Edit.Model
import qualified EditorDriver as Driver

main :: IO ()
main = do
  args <- getArgs
  (mode,port,root,line) <- case args of
    [mode,raw,root,row] | mode `elem` ["attach","launch"], Just port <- readMaybe raw,
                         port > 0 && port <= (65535::Int), Just line <- readMaybe row, line > (0::Int) -> pure (mode,port,root,line)
    _ -> fail "Usage: debugger-live (attach|launch) PORT PROJECT BREAKPOINT_LINE; choose a later executable line in a disposable THC toy."
  withDebugger $ \runtime -> do
    let effects = debuggerEffects runtime applyEffects
        tool name values d = do
          (next,finish) <- debuggerTool runtime effects d name (object values)
          result <- finish >>= either (fail . T.unpack) pure
          pure (next,result)
        current d = snd <$> tool "debug_status" [] d
        report label d = do
          state <- current d
          BL.putStrLn (encode (object ["stage" .= (label::String),"status" .= status d,"debug" .= state]))
          pure state
        tick d = do
          next <- tickDebugger runtime effects d
          state <- current next
          unless (flag "active" state || status next == "Debug session ended.") $ do
            _ <- report "failure" next
            fail (T.unpack (status next))
          pure next
        waitFor label predicate d = do
          result <- timeout 180000000 (loop d)
          maybe (report "timeout" d >> fail (label ++ " timed out")) pure result
          where loop state = do
                  next <- tick state
                  value <- current next
                  if predicate next value then pure next else threadDelay 20000 >> loop next
        stopped = waitFor "source stop" (\d s -> not (flag "active" s) ||
          (flag "stopped" s && T.isPrefixOf "Stopped in " (status d)))
        advance name allowExit d = do
          before <- current d
          check "control requires a stopped program" (flag "stopped" before)
          next <- Driver.command effects (DebugCommand name) d >>= stopped
          after <- report (T.unpack name) next
          check "control must advance the suspension generation" (epoch after > epoch before)
          check "program terminated before required step" (allowExit || flag "stopped" after)
          pure next
    (_,initial) <- applyEffects (initialDesktop (100,32)) [ReadPath root]
    (pending,_) <- tool (if mode=="launch" then "debug_launch" else "debug_attach") ["port" .= port] initial
    entry <- stopped pending
    entryState <- report "entry" entry
    check "program stops at entry" (flag "stopped" entryState)
    window <- maybe (fail "No source window") pure (activeWindow entry)
    frame <- required "frame" entryState
    source <- required "source" frame
    reference <- required "sourceReference" source :: IO Int
    check "THC embedded source is retrieved" (reference > 0 && not (T.null (activeText entry)))
    let setBreakpoints rows d = do
          state <- current d
          fst <$> tool "debug_set_breakpoints"
            ["generation" .= epoch state,"bufferId" .= bufferId window,"lines" .= rows] d
        points s = fromMaybe [] (field "breakpoints" s) :: [Value]
    requested <- setBreakpoints [line] entry
    verified <- waitFor "verified source breakpoint" (\_ s -> not (null (points s)) && all (not . flag "pending") (points s)) requested
    verification <- report "breakpoint response" verified
    check "adapter verifies the requested executable breakpoint" (all (flag "verified") (points verification))
    hit <- advance "continue" False verified
    hitState <- current hit
    hitFrame <- required "frame" hitState
    check "continue hits the selected source" (field "source" hitFrame == Just (source::Value))
    let locations = [row | bp <- points hitState, Just result <- [field "result" bp], Just row <- [field "line" result]] :: [Int]
    check "continue stops on the verified breakpoint line" (field "line" hitFrame `elem` map Just locations)
    cleared <- setBreakpoints ([]::[Int]) hit >>= waitFor "cleared breakpoints" (\_ s -> null (points s))
    into <- advance "stepIn" False cleared
    over <- advance "next" True into
    overState <- current over
    out <- if flag "active" overState then advance "stepOut" True over else do
      hPutStrLn stderr "Step-out not exercised: step-over ran the program to completion."
      pure over
    outState <- current out
    done <- if flag "active" outState then advance "continue" True out else pure out
    final <- report "terminated" done
    check "program terminates and releases inspection state"
      (not (flag "active" final) && not (flag "stopped" final) && field "frame" final == Just Null && status done=="Debug session ended.")
    hPutStrLn stderr "Live debugger checks passed: embedded source, breakpoint, step-in/over, termination (see trace for step-out)."

field :: FromJSON a => Key -> Value -> Maybe a
field key = parseMaybe (withObject "object" (.: key))

required :: FromJSON a => Key -> Value -> IO a
required key value = maybe (fail ("Missing debugger field: " ++ show key)) pure (field key value)

flag :: Key -> Value -> Bool
flag key value = field key value == Just True

epoch :: Value -> Int
epoch value = fromMaybe (error "Missing debugger generation") (field "generation" value)

check :: String -> Bool -> IO ()
check label ok = unless ok (fail label)
