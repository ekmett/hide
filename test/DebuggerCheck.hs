{-# LANGUAGE OverloadedStrings #-}
module DebuggerCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.Exit (ExitCode(..))
import System.IO
import System.Process
import System.Timeout (timeout)
import THC.Edit.Buffer
import THC.Edit.Debugger
import THC.Edit.Files (FileState(..))
import THC.Edit.Model

checks :: IO ()
checks = mapM_ session ["basic", "frame", "choices", "breakpoints", "reconnect"] >> putStrLn "Debugger checks passed"
  where
    session mode = bracket (fixture mode) cleanup $ \(port,logPath,process) -> withDebugger $ \runtime -> do
      let core d _=pure (False,d)
          send action values d=snd <$> debuggerEffects runtime core d [DebugAction action values]
          tick=tickDebugger runtime core
          waitForIO label predicate d=do
            result<-timeout 5000000 (loop d)
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
        waiting<-send "scopes" [] stack
        chosen<-choose 1 waiting
        -- This response follows the current source and both delayed old replies.
        drained<-send "threads" [] chosen >>= waitFor "stale frame replies must not open scopes" (hasDialog "Threads")
        check "late source cannot replace selected frame" (activeText drained=="chosen frame source\n")
        check "late source does not create an obsolete buffer" (not (any (T.isInfixOf "STALE" . contents . documentBuffer) (M.elems (buffers drained))))
        finish drained
      else do
        stopped<-waitFor "initial source" (T.isInfixOf "value = λ" . activeText) attached
        check "sourceReference opens read-only at adapter line" (maybe False ((/=Nothing).documentLabel) (activeDocument stopped) && fmap (caret.selection) (activeWindow stopped)==Just (T.length "module Generated where\n"))
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
