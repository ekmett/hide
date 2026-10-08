{-# LANGUAGE OverloadedStrings #-}
-- | Own a captured build/run and publish prepared output snapshots.
--
-- A supervisor executes commands sequentially until failure. Bounded output events
-- feed an aggregation worker, which prepares buffer measures and diagnostics before
-- publishing one replaceable snapshot. The tick adopts at most one snapshot.
-- Stop signals the supervisor; it retains ownership through process-tree cleanup
-- and reader joins. Stdout retention is separate from stderr and command echoes.
module Hide.BuildJobs (BuildJobs, withBuildJobs, startBuildJob, tickBuildJobs, stopBuildJob, buildJobStatus, buildJobOutput, buildJobStdout, parseBuildDiagnostic) where

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
import System.Directory (canonicalizePath)
import System.IO.Error (tryIOError)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), isAbsolute, normalise)
import System.IO (Handle)
import System.Process
import Text.Read (readMaybe)
import Hide.Buffer
import Hide.Process (processCleanup)
import Hide.Model
import qualified Hide.Plugin.Window as W
import qualified Hide.Plugin.Menu as P
import Hide.PluginWindowHost (adoptWindowUpdate,replaceWindowUpdate)
import Hide.RemoteEndpoint (randomIdentity)

-- The session, rather than its attached display, owns this worker.
data Event = Output Bool Text | Finished (Either Text ExitCode)
-- Only the worker builds these values. The UI takes at most one complete,
-- already evaluated snapshot, even when output arrives faster than it renders.
data Snapshot = Snapshot !W.PreparedWindow !(Maybe W.WindowUpdate) [Diagnostic] (Text,Bool) (Maybe (Either Text ExitCode)) !Bool
data Job = Job !Text !W.WindowRef Text FilePath (IO ()) (Async ()) (TMVar Snapshot)
-- The report retains the same prepared snapshot as the visible window. Closing
-- the view does not discard captured output or fabricate a source buffer.
data Report = Report !Text !W.WindowRef Text FilePath (Maybe (Either Text ExitCode)) !W.PreparedWindow !Bool
data BuildJobs = BuildJobs (IORef (Maybe Job)) (IORef (Maybe Report)) (IORef (Text,Bool)) !W.WindowScope (IORef (Maybe W.WindowRef))

-- | Scope the captured-job service and its supervisor/aggregation workers.
withBuildJobs :: (BuildJobs -> IO a) -> IO a
withBuildJobs use=W.withWindowScope $ \scope->bracket (BuildJobs <$> newIORef Nothing <*> newIORef Nothing <*> newIORef ("",False) <*> pure scope <*> newIORef Nothing) close use
  where close (BuildJobs ref _ _ _ _)=readIORef ref >>= mapM_ (\(Job _ _ _ _ stop worker _) -> stop >> void (waitCatch worker))

-- | Start a command sequence in its explicit working directory, using the single job slot.
startBuildJob :: BuildJobs -> Text -> FilePath -> [(FilePath,[String])] -> Desktop -> IO Desktop
startBuildJob (BuildJobs ref report stdoutReport scope slot) label root commands desktop = mask $ \restore -> do
  current<-readIORef ref
  case current of
    Just _ -> pure desktop {status="A build or run is already active; stop it before starting another."}
    Nothing -> do
      ident<-T.pack <$> randomIdentity
      empty<-either (ioError . userError . T.unpack) pure =<< W.prepareRecoverableTextWindow "hide.build-output" 1 (label<>" output") ""
      opening<-W.openWindow scope empty >>= maybe (ioError (userError "Build output scope is closed.")) pure
      previous<-readIORef slot
      opened<-maybe (adoptWindowUpdate P.HumanMenu opening desktop) (\old->replaceWindowUpdate P.HumanMenu old opening desktop) previous
      let reference=W.updateWindowRef opening
      writeIORef slot (Just reference)
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
      worker<-async $ restore $ withAsync (aggregate reference label root queue latest) $ \aggregator -> do
        link aggregator
        outcome<-withAsync (run commands) $ \producer -> do
          done<-atomically ((Left <$> readTMVar stopping) `orElse` (Right <$> waitCatchSTM producer))
          case done of
            Left release -> release >> cancel producer >> pure (Left "Stopped.")
            Right result -> pure (either (Left . T.pack . displayException) Right result)
        emit (Finished outcome)
        wait aggregator
      let old=buildDiagnostics desktop
      writeIORef ref (Just (Job ident reference label root stop worker latest))
      writeIORef report (Just (Report ident reference label root Nothing empty False))
      writeIORef stdoutReport ("",False)
      pure (setDiagnostics (filter (`notElem` old) (diagnostics opened)) opened) {status=label<>"…",buildDiagnostics=[]}

aggregate :: W.WindowRef -> Text -> FilePath -> TBQueue Event -> TMVar Snapshot -> IO ()
aggregate reference label root queue latest=loop "" "" False False
  where
    loop previous previousStdout outputWasTruncated stdoutWasTruncated=do
      first<-atomically (readTBQueue queue)
      -- Coalesce bursts without making the UI wait for the producer or parser.
      threadDelay 20000
      events<-(first:) <$> atomically (flushTBQueue queue)
      let outcomes=[final | Finished final<-events]
          outcome=case outcomes of [] -> Nothing; final:_ -> Just final
          ending=maybe "" (\final -> "\n"<>summary label final<>"\n") outcome
          combined=previous<>T.concat [text | Output _ text<-events]<>ending
          output=T.takeEnd (1024*1024) combined
          outputTruncated=outputWasTruncated || T.length combined>1024*1024
          stdout=previousStdout<>T.concat [text | Output True text<-events]
          retained=T.takeEnd (1024*1024) stdout
          truncated=stdoutWasTruncated || T.length stdout>1024*1024
          problems=take 1000 (mapMaybe (parseBuildDiagnostic root) (T.lines output))
      -- Deduplicate path resolution within a batch; canonical provenance belongs
      -- to this worker, not diagnostic rendering or agent read admission.
      resolved<-traverse (tryIOError . canonicalizePath) (M.fromList [(diagnosticPath p,diagnosticPath p) | p<-problems])
      let canonicalProblems=[p {diagnosticPath=path} | p<-problems,Just (Right path)<-[M.lookup (diagnosticPath p) resolved]]
      -- All text/style/measure preparation belongs to this aggregation worker.
      prepared<-either (ioError . userError . T.unpack) pure =<< W.prepareRecoverableTextWindow "hide.build-output" 1 (label<>" output") output
      publication<-W.refreshWindow reference prepared
      forM_ canonicalProblems $ \problem -> do
        void (evaluate (length (diagnosticPath problem)))
        void (evaluate (diagnosticRow problem+diagnosticColumn problem+diagnosticSeverity problem+T.length (diagnosticMessage problem)))
      void (evaluate (T.length retained))
      void (evaluate truncated)
      let snapshot=Snapshot prepared publication canonicalProblems (retained,truncated) outcome outputTruncated
      atomically (tryTakeTMVar latest >> putTMVar latest snapshot)
      case outcome of Nothing -> loop output retained outputTruncated truncated; Just _ -> pure ()

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
stopBuildJob (BuildJobs ref _ _ _ _) desktop = do
  current<-readIORef ref
  case current of
    Nothing -> pure desktop {status="No build or captured run is active."}
    Just (Job _ _ label _ stop _ _) -> do
      -- This only records the request. The owned supervisor releases the process
      -- tree, joins pipe readers and publishes completion after the final output.
      stop
      pure desktop {status=label<>": Stopping…"}

-- | Adopt the latest prepared output and diagnostics without replaying output history.
tickBuildJobs :: BuildJobs -> Desktop -> IO Desktop
tickBuildJobs (BuildJobs ref report stdoutReport _ _) desktop = do
  current<-readIORef ref
  case current of
    Nothing -> pure desktop
    Just (Job ident reference label root _ worker latest) -> do
      snapshot<-atomically (tryTakeTMVar latest)
      case snapshot of
        Nothing -> pure desktop
        Just preparedSnapshot@(Snapshot prepared publication problems stdout outcome truncated) -> do
          shown<-maybe (pure desktop) (\update->adoptWindowUpdate P.HumanMenu update desktop) publication
          let old=buildDiagnostics desktop
              oldLines=maybe 0 (contentLineCount . W.preparedWindowText) (M.lookup reference (pluginWindows desktop))
              newLines=contentLineCount (W.preparedWindowText prepared)
              adopted=M.lookup reference (pluginWindows shown)==Just prepared
              follow window
                | adopted, windowContent window==PluginContent reference,
                  Just previous<-find ((==windowId window) . windowId) (windows desktop) =
                    window {scrollRow=if scrollRow previous+height (bounds previous)-2>=oldLines
                      then max 0 (newLines-height (bounds window)+2) else scrollRow previous}
                | otherwise = window
              diagnosed=(setDiagnostics (filter (`notElem` old) (diagnostics desktop)++problems) shown) {buildDiagnostics=problems}
              result=case outcome of
                Nothing -> diagnosed
                Just final -> (if null problems then diagnosed else setProblemsVisible True diagnosed) {status=summary label final}
          writeIORef stdoutReport stdout
          completed<-case outcome of
            Just _ -> do
              -- Publication is the aggregator's last action. Keep ownership until
              -- its supervisor has also exited; a later tick can apply completion.
              exited<-poll worker
              case exited of
                Nothing -> atomically (putTMVar latest preparedSnapshot) >> pure False
                Just _ -> writeIORef ref Nothing >> pure True
            Nothing -> pure False
          writeIORef report (Just (Report ident reference label root (if completed then outcome else Nothing) prepared truncated))
          let updated=if completed then result else diagnosed
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

-- | Small completion/output facts, including the actual semantic view ID when
-- still open. This observes cached measures and never scans or flattens text.
buildJobStatus :: BuildJobs -> Desktop -> IO Value
buildJobStatus (BuildJobs ref report _ _ _) desktop=do
  active<-maybe False (const True) <$> readIORef ref
  recent<-readIORef report
  pure (object (["active" .= active]++case recent of
    Nothing -> []
    Just (Report ident reference label root outcome prepared truncated) ->
      ["jobId" .= ident,"windowId" .= (windowId <$> find ((==PluginContent reference) . windowContent) (windows desktop)),
       "action" .= label,"root" .= root,"outputAvailable" .= True,"outputTruncated" .= truncated,
       "retainedCharacters" .= contentLength (W.preparedWindowText prepared),
       "exitCode" .= (case outcome of Just (Right ExitSuccess) -> Just (0::Int); Just (Right (ExitFailure code)) -> Just code; _ -> Nothing),
       "error" .= (case outcome of Just (Left err) -> Just err; _ -> Nothing)]))

-- | Capture the exact latest job snapshot without projecting text. The deferred
-- result slices measured Unicode character offsets on the tool worker. A new job
-- cannot substitute output for a stale requested identity; an admitted snapshot
-- remains attributed to its captured job if another job starts during extraction.
-- Only captured job text is exposed: no general plugin visibility grant is made.
buildJobOutput :: BuildJobs -> Text -> Int -> Int -> IO (IO (Either Text Value))
buildJobOutput (BuildJobs _ report _ _ _) ident offset limit
  | offset<0 || limit<1 || limit>32768=pure (pure (Left "Use character offset>=0 and limit 1..32768."))
  | otherwise=do
      current<-readIORef report
      pure $ case current of
        Just (Report captured _ _ _ _ prepared truncated) | ident==captured->do
          let text=W.preparedWindowText prepared
              extracted=contentSlice text offset limit
          _<-evaluate (T.length extracted)
          pure (Right (object ["jobId" .= captured,"offset" .= offset,"retainedCharacters" .= contentLength text,
            "truncated" .= truncated,"text" .= extracted]))
        _->pure (Left "Build output job expired; read build_status for the current jobId.")

-- | Read the retained stdout-only tail and its truncation flag.
buildJobStdout :: BuildJobs -> IO (Text,Bool)
buildJobStdout (BuildJobs _ _ output _ _)=readIORef output
