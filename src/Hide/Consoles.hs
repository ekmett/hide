{-# LANGUAGE OverloadedStrings #-}
-- | Own named terminals and their corresponding editor documents.
--
-- Process preparation can run off the UI worker; adoption transfers an already
-- started terminal into the console service. Screen polling updates documents,
-- while a separate bounded byte suffix serves output tools. Closing a view does
-- not implicitly reopen it on the next terminal update.
module Hide.Consoles
  ( Consoles, withConsoles, startConsole, tickConsoles, consoleOutput
  , PreparedConsole, prepareConsole, adoptConsole, closePreparedConsole, consoleProcessId
  , listConsoles, inputConsole, killConsole, retireConsole, releaseConsole
  ) where

import Control.Concurrent.MVar
import Control.Exception (bracket, mask_, onException)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.List (find, intercalate)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Buffer
import Hide.Model hiding (message)
import Hide.Syntax (Style(..))
import Hide.Terminal

newtype Consoles = Consoles (MVar (Integer,M.Map Text Console))
data Console = Console
  { consoleTerminal :: Terminal, consoleBuffer :: Int, outputLimit :: Int
  , retainedOutput :: ByteString, outputTruncated :: Bool
  , latestSnapshot :: TerminalSnapshot, consoleError :: Maybe Text, consoleRetiring :: Bool }

withConsoles :: (Consoles -> IO a) -> IO a
withConsoles = bracket (Consoles <$> newMVar (1,M.empty)) closeAll
  where
    closeAll (Consoles state) = modifyMVar_ state $ \(counter,consoles) -> do
      mapM_ (closeTerminal . consoleTerminal) consoles
      pure (counter,M.empty)

-- | Caller-owned started terminal until adoption; close the returned value if
-- adoption is abandoned. Cancellation inside preparation already has cleanup.
-- The ownership transfer is conventional, not enforced by a linear type.
newtype PreparedConsole = PreparedConsole Console

prepareConsole :: [String] -> TerminalConfig -> Int -> IO (Either Text PreparedConsole)
prepareConsole unset config limit = mask_ $ do
  started <- startTerminalUnsetting unset config
  case started of
    Left message -> pure (Left message)
    Right terminal -> (do
      polled <- pollTerminal terminal
      case polled of
        Left message -> closeTerminal terminal >> pure (Left message)
        Right snapshot -> pure (Right (PreparedConsole (recordOutput snapshot
          (Console terminal 0 (max 0 (min (16*1024*1024) limit)) BS.empty False snapshot Nothing False)))))
      `onException` closeTerminal terminal

closePreparedConsole :: PreparedConsole -> IO ()
closePreparedConsole (PreparedConsole console) = closeTerminal (consoleTerminal console)

adoptConsole :: Consoles -> PreparedConsole -> Desktop -> IO (Text,Desktop)
adoptConsole (Consoles state) (PreparedConsole prepared) desktop = modifyMVar state $ \(counter,consoles) -> do
  let ident = T.pack (show counter)
      bid = nextId desktop
      console = prepared {consoleBuffer=bid}
      opened = addDocument Nothing (newBuffer "") desktop
      labeled = opened {buffers=M.adjust (\doc -> doc {documentLabel=Just ("Terminal " <> ident)}) bid (buffers opened)}
  pure ((counter+1,M.insert ident console consoles),(ident,showConsole console labeled))

startConsole :: Consoles -> TerminalConfig -> Int -> Desktop -> IO (Either Text (Text,Desktop))
startConsole consoles config limit desktop = mask_ $ do
  prepared <- prepareConsole [] config limit
  case prepared of
    Left message -> pure (Left message)
    Right console -> (Right <$> adoptConsole consoles console desktop) `onException` closePreparedConsole console

consoleProcessId :: Consoles -> Text -> IO (Either Text Int)
consoleProcessId consoles ident = updateConsole consoles ident $ \console -> do
  result <- terminalProcessId (consoleTerminal console)
  pure (console,result)

-- | Apply appearance/size changes, poll terminals and update existing views.
tickConsoles :: Consoles -> Desktop -> IO Desktop
tickConsoles (Consoles state) desktop = modifyMVar state $ \(counter,consoles) -> do
  updated <- mapM (\console -> if consoleRetiring console then pure console else do
    result<-setTerminalAppearance (consoleTerminal console) (darkAppearance desktop)
    case result of
      Left message -> pure console {consoleError=Just message}
      Right () -> resizeFor desktop console >>= refresh) consoles
  let shown = foldr showConsole desktop updated
      errors = [message | (ident,console) <- M.toList updated, Just message <- [consoleError console]
                        , maybe True ((== Nothing) . consoleError) (M.lookup ident consoles)]
  pure ((counter,updated),case errors of message:_ -> shown {status=message}; [] -> shown)

-- | Refresh and return retained bytes, sticky truncation state and process exit status.
consoleOutput :: Consoles -> Text -> IO (Either Text (ByteString,Bool,Maybe Int))
consoleOutput consoles ident = updateConsole consoles ident $ \console -> do
  updated <- refresh console
  pure (updated,case consoleError updated of
    Just message -> Left message
    Nothing -> Right (retainedOutput updated,outputTruncated updated,snapshotExitCode (latestSnapshot updated)))

inputConsole :: Consoles -> Text -> ByteString -> IO (Either Text ())
inputConsole consoles ident bytes = updateConsole consoles ident $ \console -> do
  result <- if consoleRetiring console then pure (Left "Terminal has stopped") else maybe (writeTerminal (consoleTerminal console) bytes) (pure . Left) (consoleError console)
  pure (console,result)

-- | Stop normal polling and return cleanup to run outside the shared service lock.
-- The returned cleanup still has to be executed.
retireConsole :: Consoles -> Text -> IO (Either Text (IO ()))
retireConsole (Consoles state) ident = modifyMVar state $ \(counter,consoles) -> case M.lookup ident consoles of
  Nothing -> pure ((counter,consoles),Left "Unknown or released terminal")
  Just console | consoleRetiring console -> pure ((counter,consoles),Right (pure ()))
  Just console -> do
    let cleanup=do
          killTerminal (consoleTerminal console)
          updated<-refresh console
          closeTerminal (consoleTerminal console)
          modifyMVar_ state $ \(next,current) -> pure (next,M.adjust (const updated {consoleRetiring=True}) ident current)
    pure ((counter,M.insert ident console {consoleRetiring=True} consoles),Right cleanup)

killConsole :: Consoles -> Text -> IO (Either Text ())
killConsole consoles ident = retireConsole consoles ident >>= either (pure . Left) (fmap Right)

-- | Close the terminal and remove its console service entry.
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
  | Just window <- find ((== Just (consoleBuffer console)) . bufferId) (filter (windowVisible desktop) (windows desktop)++windows desktop)
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
  | consoleRetiring console = pure console
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
          | bufferId window/=Just bid = window
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

listConsoles :: Consoles -> IO [(Text,Int,Maybe Int)]
listConsoles (Consoles state)=withMVar state $ \(_,consoles) -> pure [(ident,consoleBuffer c,snapshotExitCode (latestSnapshot c)) | (ident,c)<-M.toAscList consoles]
