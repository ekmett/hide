{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.BuildJobs (BuildJobs, withBuildJobs, startBuildJob, tickBuildJobs, stopBuildJob, parseBuildDiagnostic) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef
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
import THC.Edit.Buffer
import THC.Edit.Process (processCleanup)
import THC.Edit.Model

-- The session, rather than its attached display, owns this worker.
data Event = Output Text | Finished (Either Text ExitCode)
data Job = Job Int Text FilePath (Async ()) (TBQueue Event) Text
newtype BuildJobs = BuildJobs (IORef (Maybe Job))

withBuildJobs :: (BuildJobs -> IO a) -> IO a
withBuildJobs = bracket (BuildJobs <$> newIORef Nothing) close
  where close (BuildJobs ref)=readIORef ref >>= mapM_ (\(Job _ _ _ worker _ _) -> cancel worker)

startBuildJob :: BuildJobs -> Text -> FilePath -> [(FilePath,[String])] -> Desktop -> IO Desktop
startBuildJob (BuildJobs ref) label root commands desktop = do
  current<-readIORef ref
  case current of
    Just _ -> pure desktop {status="A build or run is already active; stop it before starting another."}
    Nothing -> do
      queue<-newTBQueueIO 128
      let emit=atomically . writeTBQueue queue
          run []=pure ExitSuccess
          run ((command,args):rest)=do
            emit (Output ("$ "<>T.replace "\n" "\\n" (T.pack (showCommandForUser command args))<>"\n"))
            result<-capture root command args (emit . Output)
            if result==ExitSuccess then run rest else pure result
      worker<-async $ do
        result<-try (run commands)
        case result of
          Left (err::SomeException) | Just (_::SomeAsyncException)<-fromException err -> throwIO err
          _ -> emit (Finished (either (Left . T.pack . displayException) Right result))
      let opened=addReadOnly (label<>" output") "" desktop
          bid=maybe (nextId desktop) bufferId (activeWindow opened)
          old=buildDiagnostics desktop
      writeIORef ref (Just (Job bid label root worker queue ""))
      pure opened {status=label<>"…",buildDiagnostics=[],diagnostics=filter (`notElem` old) (diagnostics opened)}

capture :: FilePath -> FilePath -> [String] -> (Text -> IO ()) -> IO ExitCode
capture root command args emit =
  withCreateProcess ((proc command args) {cwd=Just root,std_in=NoStream,std_out=CreatePipe,std_err=CreatePipe,create_group=True}) $ \_ out err child ->
    mask $ \restore -> do
      stop<-processCleanup child
      withAsync (restore (maybe (pure ()) (pump emit) out)) $ \reader ->
        withAsync (restore (maybe (pure ()) (pump emit) err)) $ \errors ->
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
stopBuildJob runtime@(BuildJobs ref) desktop = do
  current<-readIORef ref
  case current of
    Nothing -> pure desktop {status="No build or captured run is active."}
    Just (Job _ _ _ worker queue _) -> do
      cancel worker
      -- Drain pending output before inserting the completion marker.
      updated<-tickBuildJobs runtime desktop
      atomically (writeTBQueue queue (Finished (Left "Stopped.")))
      tickBuildJobs runtime updated

tickBuildJobs :: BuildJobs -> Desktop -> IO Desktop
tickBuildJobs (BuildJobs ref) desktop = do
  current<-readIORef ref
  case current of
    Nothing -> pure desktop
    Just (Job bid label root worker queue previous) -> do
      events<-atomically (flushTBQueue queue)
      if null events then pure desktop else do
        let appended=T.concat [text | Output text<-events]
            outcomes=[outcome | Finished outcome<-events]
            ending=case outcomes of [] -> ""; outcome:_ -> "\n"<>summary outcome<>"\n"
            output=T.takeEnd (1024*1024) (previous<>appended<>ending)
            problems=take 1000 (mapMaybe (parseBuildDiagnostic root) (T.lines output))
            old=buildDiagnostics desktop
            update doc=doc {documentBuffer=newBuffer output,documentHighlight=[]}
            oldLines=maybe 0 (bufferLineCount . documentBuffer) (M.lookup bid (buffers desktop))
            newLines=bufferLineCount (newBuffer output)
            follow window
              | bufferId window==bid, scrollRow window+height (bounds window)-2>=oldLines =
                  window {scrollRow=max 0 (newLines-height (bounds window)+2)}
              | otherwise = window
            shown=desktop {buffers=M.adjust update bid (buffers desktop),windows=map follow (windows desktop),buildDiagnostics=problems
              ,diagnostics=filter (`notElem` old) (diagnostics desktop)++problems}
            result=case outcomes of
              [] -> shown
              outcome:_ -> (if null problems then shown else setProblemsVisible True shown) {status=summary outcome}
        writeIORef ref (if null outcomes then Just (Job bid label root worker queue output) else Nothing)
        pure result
      where
        summary (Left err)=label<>": "<>err
        summary (Right ExitSuccess)=label<>" completed."
        summary (Right (ExitFailure code))=label<>" failed (exit "<>T.pack (show code)<>")."

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
