{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Consoles
  ( Consoles, withConsoles, startConsole, tickConsoles, consoleOutput
  , inputConsole, killConsole, releaseConsole
  ) where

import Control.Concurrent.MVar
import Control.Exception (bracket, mask_, onException)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.List (find, intercalate)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import THC.Edit.Buffer
import THC.Edit.Model hiding (message)
import THC.Edit.Syntax (Style(..))
import THC.Edit.Terminal

newtype Consoles = Consoles (MVar (Integer,M.Map Text Console))
data Console = Console
  { consoleTerminal :: Terminal, consoleBuffer :: Int, outputLimit :: Int
  , retainedOutput :: ByteString, outputTruncated :: Bool
  , latestSnapshot :: TerminalSnapshot, consoleError :: Maybe Text }

withConsoles :: (Consoles -> IO a) -> IO a
withConsoles = bracket (Consoles <$> newMVar (1,M.empty)) closeAll
  where
    closeAll (Consoles state) = modifyMVar_ state $ \(counter,consoles) -> do
      mapM_ (closeTerminal . consoleTerminal) consoles
      pure (counter,M.empty)

startConsole :: Consoles -> TerminalConfig -> Int -> Desktop -> IO (Either Text (Text,Desktop))
startConsole (Consoles state) config limit desktop = mask_ $ do
  started <- startTerminal config
  case started of
    Left message -> pure (Left message)
    Right terminal -> (do
      polled <- pollTerminal terminal
      case polled of
        Left message -> closeTerminal terminal >> pure (Left message)
        Right snapshot -> modifyMVar state $ \(counter,consoles) -> do
          let ident = T.pack (show counter)
              bid = nextId desktop
              console = recordOutput snapshot (Console terminal bid (max 0 (min (16*1024*1024) limit)) BS.empty False snapshot Nothing)
              opened = addDocument Nothing (newBuffer "") desktop
              labeled = opened {buffers=M.adjust (\doc -> doc {documentLabel=Just ("Terminal " <> ident)}) bid (buffers opened)}
          pure ((counter+1,M.insert ident console consoles),Right (ident,showConsole console labeled))
      ) `onException` closeTerminal terminal

tickConsoles :: Consoles -> Desktop -> IO Desktop
tickConsoles (Consoles state) desktop = modifyMVar state $ \(counter,consoles) -> do
  updated <- mapM (\console -> resizeFor desktop console >>= refresh) consoles
  let shown = foldr showConsole desktop updated
      errors = [message | (ident,console) <- M.toList updated, Just message <- [consoleError console]
                        , maybe True ((== Nothing) . consoleError) (M.lookup ident consoles)]
  pure ((counter,updated),case errors of message:_ -> shown {status=message}; [] -> shown)

consoleOutput :: Consoles -> Text -> IO (Either Text (ByteString,Bool,Maybe Int))
consoleOutput consoles ident = updateConsole consoles ident $ \console -> do
  updated <- refresh console
  pure (updated,case consoleError updated of
    Just message -> Left message
    Nothing -> Right (retainedOutput updated,outputTruncated updated,snapshotExitCode (latestSnapshot updated)))

inputConsole :: Consoles -> Text -> ByteString -> IO (Either Text ())
inputConsole consoles ident bytes = updateConsole consoles ident $ \console -> do
  result <- maybe (writeTerminal (consoleTerminal console) bytes) (pure . Left) (consoleError console)
  pure (console,result)

killConsole :: Consoles -> Text -> IO (Either Text ())
killConsole consoles ident = updateConsole consoles ident $ \console -> do
  killTerminal (consoleTerminal console)
  updated <- refresh console
  pure (updated,Right ())

releaseConsole :: Consoles -> Text -> IO (Either Text ())
releaseConsole (Consoles state) ident = modifyMVar state $ \(counter,consoles) -> case M.lookup ident consoles of
  Nothing -> pure ((counter,consoles),Left "Unknown or released terminal")
  Just console -> do
    closeTerminal (consoleTerminal console)
    pure ((counter,M.delete ident consoles),Right ())

updateConsole :: Consoles -> Text -> (Console -> IO (Console,Either Text a)) -> IO (Either Text a)
updateConsole (Consoles state) ident action = modifyMVar state $ \(counter,consoles) -> case M.lookup ident consoles of
  Nothing -> pure ((counter,consoles),Left "Unknown or released terminal")
  Just console -> do
    (updated,result) <- action console
    pure ((counter,M.insert ident updated consoles),result)

resizeFor :: Desktop -> Console -> IO Console
resizeFor desktop console
  | consoleError console /= Nothing = pure console
  | Just window <- find ((== consoleBuffer console) . bufferId) (windows desktop)
  , let columns = max 1 (min 1000 (width (bounds window)-2))
        rows = max 1 (min 1000 (height (bounds window)-2))
  , (columns,rows) /= (snapshotColumns snapshot,snapshotRows snapshot) = do
      result <- resizeTerminal (consoleTerminal console) columns rows
      case result of
        Right () -> pure console
        Left message -> do
          killTerminal (consoleTerminal console)
          pure console {consoleError=Just message}
  | otherwise = pure console
  where snapshot = latestSnapshot console

refresh :: Console -> IO Console
refresh console
  | consoleError console /= Nothing = pure console
  | otherwise = do
      result <- pollTerminal (consoleTerminal console)
      case result of
        Right snapshot -> pure (recordOutput snapshot console)
        Left message -> do
          killTerminal (consoleTerminal console)
          pure console {consoleError=Just message}

recordOutput :: TerminalSnapshot -> Console -> Console
recordOutput snapshot console
  | BS.null (snapshotOutput snapshot) = console {latestSnapshot=snapshot}
  | otherwise = console
  { retainedOutput=retained, outputTruncated=truncated, latestSnapshot=snapshot }
  where
    bytes = retainedOutput console <> snapshotOutput snapshot
    dropped = max 0 (BS.length bytes-outputLimit console)
    truncated = outputTruncated console || dropped > 0
    suffix = BS.drop dropped bytes
    -- Keep incomplete UTF-8 tails until a later poll supplies the remaining bytes.
    -- Once truncation starts, discard only leading continuation bytes.
    retained = if truncated then BS.copy (BS.dropWhile (\byte -> byte >= 0x80 && byte < 0xc0) suffix) else bytes

showConsole :: Console -> Desktop -> Desktop
showConsole console desktop = case M.lookup bid (buffers desktop) of
  Nothing -> desktop -- Closing a window must not reopen it on the next tick.
  Just original ->
    let styled = snapshotStyles snapshot
        text = T.pack (map fst styled)
        rendered = if documentHighlight original == styled then original else original
          {documentBuffer=newBuffer text,documentHighlight=styled}
        document = rendered {documentWidth=snapshotColumns snapshot,documentCursorVisible=snapshotCursor snapshot /= Nothing}
        buffer = documentBuffer document
        count = bufferLength buffer
        cursor = fmap (\(col,row) -> bufferLineOffset buffer row + columnOffset (bufferLineAt buffer row) col) (snapshotCursor snapshot)
        clamp n = max 0 (min count n)
        adjust window
          | bufferId window /= bid = window
          | otherwise = window
              { selection=let Selection a c = selection window in case cursor of
                  Just pos | a == c -> Selection (clamp pos) (clamp pos)
                  _ -> Selection (clamp a) (clamp c)
              , scrollRow=max 0 (min (scrollbarLimit desktop True document window) (scrollRow window))
              , scrollColumn=max 0 (min (scrollbarLimit desktop False document window) (scrollColumn window)) }
    in desktop {buffers=M.insert bid document (buffers desktop),windows=map adjust (windows desktop)}
  where
    bid = consoleBuffer console
    snapshot = latestSnapshot console

snapshotStyles :: TerminalSnapshot -> [(Char,Style)]
snapshotStyles snapshot = intercalate [('\n',Plain)] (map (concatMap cell) (rows (snapshotCells snapshot)))
  where
    columns = max 1 (snapshotColumns snapshot)
    rows [] = []
    rows cells = let (row,rest) = splitAt columns cells in row : rows rest
    cell value
      | cellWidth value == 0 = []
      | otherwise = [(char,TerminalStyle (cellForeground value) (cellBackground value) (cellAttributes value))
                    | char <- T.unpack (if T.null (cellText value) then " " else cellText value)]
