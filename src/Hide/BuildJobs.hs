{-# LANGUAGE OverloadedStrings #-}
-- | Own a captured build/run and publish prepared output snapshots.
--
-- A supervisor executes commands sequentially until failure. Bounded output events
-- feed an aggregation worker, which prepares buffer measures and diagnostics before
-- publishing one replaceable snapshot. The tick adopts at most one snapshot.
-- Stop signals the supervisor; it retains ownership through process-tree cleanup
-- and reader joins. Stdout retention is separate from stderr and command echoes.
module Hide.BuildJobs (BuildJobs, withBuildJobs, startBuildJob, tickBuildJobs, stopBuildJob, buildJobStatus, buildJobStdout, parseBuildDiagnostic) where

import Data.Aeson
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_, void, when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), isAbsolute, normalise)
import System.IO (Handle)
import System.Process
import Text.Read (readMaybe)
import Hide.Buffer
import Hide.Process (processCleanup)
import Hide.Model

-- The session, rather than its attached display, owns this worker.
data Event = Output Bool Text | Finished (Either Text ExitCode)
-- Only the worker builds these values. The UI takes at most one complete,
-- already evaluated snapshot, even when output arrives faster than it renders.
data Snapshot = Snapshot Buffer [Diagnostic] (Text,Bool) (Maybe (Either Text ExitCode))
data Job = Job Int Text FilePath (IO ()) (Async ()) (TMVar Snapshot)
data BuildJobs = BuildJobs (IORef (Maybe Job)) (IORef (Maybe (Int,Text,FilePath,Maybe (Either Text ExitCode)))) (IORef (Text,Bool))

-- | Scope the captured-job service and its supervisor/aggregation workers.
withBuildJobs :: (BuildJobs -> IO a) -> IO a
withBuildJobs = bracket (BuildJobs <$> newIORef Nothing <*> newIORef Nothing <*> newIORef ("",False)) close
  where close (BuildJobs ref _ _)=readIORef ref >>= mapM_ (\(Job _ _ _ stop worker _) -> stop >> void (waitCatch worker))

-- | Start a command sequence in its explicit working directory, using the single job slot.
startBuildJob :: BuildJobs -> Text -> FilePath -> [(FilePath,[String])] -> Desktop -> IO Desktop
startBuildJob (BuildJobs ref report stdoutReport) label root commands desktop = mask $ \restore -> do
  current<-readIORef ref
  case current of
    Just _ -> pure desktop {status="A build or run is already active; stop it before starting another."}
    Nothing -> do
      queue<-newTBQueueIO 128
      latest<-newEmptyTMVarIO
      stopping<-newEmptyTMVarIO
      -- Nothing records a stop requested before or during process creation.
      cleanup<-newTVarIO (Just (pure ()))
      let emit=atomically . writeTBQueue queue
          run []=pure ExitSuccess
          run ((command,args):rest)=do
            pending<-readTVarIO cleanup
            case pending of Nothing -> throwIO ThreadKilled; Just _ -> pure ()
            emit (Output False ("$ "<>T.replace "\n" "\\n" (T.pack (showCommandForUser command args))<>"\n"))
            result<-capture cleanup root command args (\isStdout -> emit . Output isStdout)
            if result==ExitSuccess then run rest else pure result
          stop=atomically $ do
            pending<-readTVar cleanup
            writeTVar cleanup Nothing
            void (tryPutTMVar stopping (maybe (pure ()) id pending))
      worker<-async $ restore $ withAsync (aggregate label root queue latest) $ \aggregator -> do
        link aggregator
        outcome<-withAsync (run commands) $ \producer -> do
          done<-atomically ((Left <$> readTMVar stopping) `orElse` (Right <$> waitCatchSTM producer))
          case done of
            Left release -> release >> cancel producer >> pure (Left "Stopped.")
            Right result -> pure (either (Left . T.pack . displayException) Right result)
        emit (Finished outcome)
        wait aggregator
      let opened=addReadOnly (label<>" output") "" desktop
          bid=maybe (nextId desktop) bufferId (activeWindow opened)
          old=buildDiagnostics desktop
      writeIORef ref (Just (Job bid label root stop worker latest))
      writeIORef report (Just (bid,label,root,Nothing))
      writeIORef stdoutReport ("",False)
      pure opened {status=label<>"…",buildDiagnostics=[],diagnostics=filter (`notElem` old) (diagnostics opened)}

aggregate :: Text -> FilePath -> TBQueue Event -> TMVar Snapshot -> IO ()
aggregate label root queue latest=loop "" "" False
  where
    loop previous previousStdout wasTruncated=do
      first<-atomically (readTBQueue queue)
      -- Coalesce bursts without making the UI wait for the producer or parser.
      threadDelay 20000
      events<-(first:) <$> atomically (flushTBQueue queue)
      let outcomes=[final | Finished final<-events]
          outcome=case outcomes of [] -> Nothing; final:_ -> Just final
          ending=maybe "" (\final -> "\n"<>summary label final<>"\n") outcome
          output=T.takeEnd (1024*1024) (previous<>T.concat [text | Output _ text<-events]<>ending)
          stdout=previousStdout<>T.concat [text | Output True text<-events]
          retained=T.takeEnd (1024*1024) stdout
          truncated=wasTruncated || T.length stdout>1024*1024
          problems=take 1000 (mapMaybe (parseBuildDiagnostic root) (T.lines output))
          buffer=newBuffer output
      -- Strict measures prepare the tree once; seeking every row separately
      -- turns a burst of short lines into O(lines * log lines) work.
      evaluate (prepareBuffer buffer)
      forM_ problems $ \problem -> do
        void (evaluate (length (diagnosticPath problem)))
        void (evaluate (diagnosticRow problem+diagnosticColumn problem+diagnosticSeverity problem+T.length (diagnosticMessage problem)))
      void (evaluate (T.length retained))
      void (evaluate truncated)
      let snapshot=Snapshot buffer problems (retained,truncated) outcome
      atomically (tryTakeTMVar latest >> putTMVar latest snapshot)
      case outcome of Nothing -> loop output retained truncated; Just _ -> pure ()

summary :: Text -> Either Text ExitCode -> Text
summary label (Left err)=label<>": "<>err
summary label (Right ExitSuccess)=label<>" completed."
summary label (Right (ExitFailure code))=label<>" failed (exit "<>T.pack (show code)<>")."

capture :: TVar (Maybe (IO ())) -> FilePath -> FilePath -> [String] -> (Bool -> Text -> IO ()) -> IO ExitCode
capture cleanup root command args emit = do
  -- The command echo may have blocked on a full queue after the loop check.
  available<-readTVarIO cleanup
  case available of Nothing -> throwIO ThreadKilled; Just _ -> pure ()
  withCreateProcess ((proc command args) {cwd=Just root,std_in=NoStream,std_out=CreatePipe,std_err=CreatePipe,create_group=True}) $ \_ out err child ->
    mask $ \restore -> do
      stop<-processCleanup child
      release<-atomically $ do
        pending<-readTVar cleanup
        case pending of
          Nothing -> pure stop
          Just _ -> writeTVar cleanup (Just stop) >> pure (pure ())
      release
      let unregister=atomically (modifyTVar' cleanup (fmap (const (pure ()))))
      flip finally unregister $ withAsync (restore (maybe (pure ()) (pump (emit True)) out)) $ \reader ->
        withAsync (restore (maybe (pure ()) (pump (emit False)) err)) $ \errors ->
          restore (do code<-waitForProcess child; wait reader; wait errors; pure code)
            `onException` stop

pump :: (Text -> IO ()) -> Handle -> IO ()
pump emit stream=loop (TE.streamDecodeUtf8With lenientDecode) BS.empty
  where
    loop decoder tailBytes=do
      bytes<-BS.hGetSome stream 4096
      if BS.null bytes then when (not (BS.null tailBytes)) (emit (TE.decodeUtf8With lenientDecode tailBytes))
      else case decoder bytes of
        TE.Some text remaining next -> emit text >> loop next remaining

stopBuildJob :: BuildJobs -> Desktop -> IO Desktop
stopBuildJob (BuildJobs ref _ _) desktop = do
  current<-readIORef ref
  case current of
    Nothing -> pure desktop {status="No build or captured run is active."}
    Just (Job _ label _ stop _ _) -> do
      -- This only records the request. The owned supervisor releases the process
      -- tree, joins pipe readers and publishes completion after the final output.
      stop
      pure desktop {status=label<>": Stopping…"}

-- | Adopt the latest prepared output and diagnostics without replaying output history.
tickBuildJobs :: BuildJobs -> Desktop -> IO Desktop
tickBuildJobs (BuildJobs ref report stdoutReport) desktop = do
  current<-readIORef ref
  case current of
    Nothing -> pure desktop
    Just (Job bid label root _ worker latest) -> do
      prepared<-atomically (tryTakeTMVar latest)
      case prepared of
        Nothing -> pure desktop
        Just (Snapshot buffer problems stdout outcome) -> do
          let old=buildDiagnostics desktop
              update doc=doc {documentBuffer=buffer,documentHighlight=[]}
              oldLines=maybe 0 (bufferLineCount . documentBuffer) (M.lookup bid (buffers desktop))
              newLines=bufferLineCount buffer
              follow window
                | bufferId window==bid, Just previous<-find ((==windowId window) . windowId) (windows desktop) =
                    window {scrollRow=if scrollRow previous+height (bounds previous)-2>=oldLines
                      then max 0 (newLines-height (bounds window)+2) else scrollRow previous}
                | otherwise = window
              shown=desktop {buffers=M.adjust update bid (buffers desktop),buildDiagnostics=problems
                ,diagnostics=filter (`notElem` old) (diagnostics desktop)++problems}
              result=case outcome of
                Nothing -> shown
                Just final -> (if null problems then shown else setProblemsVisible True shown) {status=summary label final}
          writeIORef stdoutReport stdout
          completed<-case outcome of
            Just final -> do
              -- Publication is the aggregator's last action. Keep ownership until
              -- its supervisor has also exited; a later tick can apply completion.
              exited<-poll worker
              case exited of
                Nothing -> atomically (putTMVar latest (Snapshot buffer problems stdout outcome)) >> pure False
                Just _ -> do
                  writeIORef ref Nothing
                  writeIORef report (Just (bid,label,root,Just final))
                  pure True
            Nothing -> pure False
          let updated=if completed then result else shown
          pure updated {windows=map follow (windows updated)}

-- Split from the first numeric line/column pair, leaving drive letters and
-- colons in file names intact. Both GHC and THC's compiler use these locations.
parseBuildDiagnostic :: FilePath -> Text -> Maybe Diagnostic
parseBuildDiagnostic _ input | "$ " `T.isPrefixOf` input = Nothing
parseBuildDiagnostic root input = scan [] (T.splitOn ":" input)
  where
    scan path (row:col:rest) | not (null path), Just line<-number row, Just column<-number col, line>0,column>0 =
      let detail=T.strip (T.intercalate ":" rest)
          file=T.unpack (T.intercalate ":" (reverse path))
          lower=T.toLower detail
      in if "error" `T.isPrefixOf` lower || "warning" `T.isPrefixOf` lower
        then Just (Diagnostic (normalise (if isAbsolute file then file else root </> file)) Nothing (line-1) (column-1)
          (if "warning" `T.isPrefixOf` lower then 2 else 1) detail)
        else Nothing
    scan path (part:rest)=scan (part:path) rest
    scan _ []=Nothing
    number text=readMaybe (T.unpack (T.takeWhile (\c -> c>='0' && c<='9') text))

-- Completion metadata survives worker exit; output remains an editor buffer.
buildJobStatus :: BuildJobs -> IO Value
buildJobStatus (BuildJobs ref report _)=do
  active<-maybe False (const True) <$> readIORef ref
  recent<-readIORef report
  pure (object (["active" .= active]++case recent of
    Nothing -> []
    Just (bid,label,root,outcome) -> ["bufferId" .= bid,"action" .= label,"root" .= root,
      "exitCode" .= (case outcome of Just (Right ExitSuccess) -> Just (0::Int); Just (Right (ExitFailure code)) -> Just code; _ -> Nothing),
      "error" .= (case outcome of Just (Left err) -> Just err; _ -> Nothing)]))

-- | Read the retained stdout-only tail and its truncation flag.
buildJobStdout :: BuildJobs -> IO (Text,Bool)
buildJobStdout (BuildJobs _ _ output)=readIORef output
