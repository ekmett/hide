-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings, StrictData #-}
-- |
-- Module      : Hide.Consoles
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings, StrictData
--
-- Own named terminals and their corresponding editor documents.
--
-- Process preparation can run off the UI worker; adoption transfers an already
-- started terminal into the console service. Screen polling prepares immutable
-- document payloads once per native revision; adoption only installs references.
-- A separate bounded byte suffix serves output tools. Closing a view does
-- not implicitly reopen it on the next terminal update.
module Hide.Consoles
  ( Consoles, withConsoles, startConsole, tickConsoles, consoleOutput
  , PreparedConsole, prepareConsole, adoptConsole, closePreparedConsole, consoleProcessId
  , listConsoles, inputConsole, inputHumanConsole, mouseConsole, killConsole, retireConsole, releaseConsole
  , retainConsoleSources, refreshConsolePrivacy, publicConsoleAt
  ) where

import Control.Concurrent.MVar
import Control.Exception (bracket, evaluate, mask_, onException)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.List (find, intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Data.Word (Word64)
import System.Mem.StableName
import Hide.GuestAccess (protectedBufferWith,protectedFilePath)
import qualified Data.Text as T
import Hide.Buffer
import Hide.Model hiding (message)
import Hide.Syntax (Style(..),StyledText,styledContents)
import Hide.Terminal

-- Output bounds apply at polling, not when an agent eventually reads the tail.
newtype Consoles = Consoles (MVar (Integer,M.Map Text Console))
data Console = Console
  { consoleTerminal :: ~Terminal -- Opaque capability; the disabled backend has no inhabitants.
  , consoleBuffer :: Int, outputLimit :: Int
  , retainedOutput :: ByteString, outputTruncated :: Bool
  , latestSnapshot :: TerminalSnapshot, latestScreen :: ConsoleScreen
  , consoleError :: Maybe Text, consoleRetiring :: Bool
  , consoleMouse :: Maybe TerminalMouseEvent, consoleDisclosure :: !ConsoleDisclosure }

-- Disclosure belongs to retained bytes, independent of the DAP transport and
-- process lifetime. Private/overflowed provenance stays restricted until release.
data ConsoleDisclosure = ConsoleDisclosure !Bool !(M.Map (Maybe Int,Maybe FilePath) ()) !(Maybe ConsolePrivacySnapshot)
data ConsolePrivacySnapshot = ConsolePrivacySnapshot !(StableName (M.Map Int Document)) !(StableName [FilePath])

privateConsole :: Console -> Bool
privateConsole console=case consoleDisclosure console of ConsoleDisclosure private _ _->private

retainConsoleSources :: Consoles -> Text -> [(Maybe Int,Maybe FilePath)] -> Bool -> IO ()
retainConsoleSources (Consoles state) ident sources captured=modifyMVar_ state $ \(counter,consoles)->pure (counter,M.adjust retain ident consoles)
  where
    retain console | privateConsole console=console
                   | otherwise=case consoleDisclosure console of
      ConsoleDisclosure _ previous _->
        let next=M.union previous (M.fromList [(source,()) | source<-sources])
            private=captured || M.size next>1024
        in console {consoleDisclosure=ConsoleDisclosure private (if private then M.empty else next) Nothing}

refreshDisclosure :: [FilePath] -> Desktop -> Console -> IO Console
refreshDisclosure paths desktop console
  | privateConsole console=pure console
  | protectedBufferWith paths desktop (consoleBuffer console)=pure console {consoleDisclosure=ConsoleDisclosure True M.empty Nothing}
  | otherwise=case consoleDisclosure console of
    ConsoleDisclosure _ owners old | not (M.null owners)->do
      documents<-evaluate (buffers desktop) >>= makeStableName
      configured<-evaluate (guestPrivatePaths desktop) >>= makeStableName
      let unchanged=case old of Just (ConsolePrivacySnapshot before pathsBefore)->eqStableName documents before && eqStableName configured pathsBefore;_->False
      if unchanged then pure console else do
        let private=any (\(bid,path)->maybe False (protectedBufferWith paths desktop) bid || maybe False (protectedFilePath paths) path) (M.keys owners)
        pure console {consoleDisclosure=ConsoleDisclosure private (if private then M.empty else owners) (Just (ConsolePrivacySnapshot documents configured))}
    _->pure console

protectConsoleDocument :: Console -> Desktop -> Desktop
protectConsoleDocument console desktop
  | privateConsole console,Just doc<-M.lookup (consoleBuffer console) (buffers desktop),not (documentPrivate doc)=
      desktop {buffers=M.insert (consoleBuffer console) doc {documentPrivate=True} (buffers desktop)}
  | otherwise=desktop

-- Metadata-only owner step, also used after effect/policy transitions. Retired
-- consoles participate because their retained output remains readable otherwise.
refreshConsolePrivacy :: Consoles -> Desktop -> IO Desktop
refreshConsolePrivacy (Consoles state) desktop=modifyMVar state $ \(counter,consoles)->do
  let paths=privateFilePaths desktop
  updated<-mapM (refreshDisclosure paths desktop) consoles
  pure ((counter,updated),foldr protectConsoleDocument desktop updated)

-- Current policy is checked at final publication, even if preparation captured
-- a public page before its expression source became private or its view closed.
publicConsoleAt :: Consoles -> Text -> Desktop -> IO (Desktop,Either Text ())
publicConsoleAt (Consoles state) ident desktop=modifyMVar state $ \(counter,consoles)->case M.lookup ident consoles of
  Nothing->pure ((counter,consoles),(desktop,Left "Unknown or released terminal"))
  Just console->do
    updated<-refreshDisclosure (privateFilePaths desktop) desktop console
    pure ((counter,M.insert ident updated consoles),(protectConsoleDocument updated desktop,
      if privateConsole updated then Left "Terminal output is private." else Right ()))

-- The revision belongs to this Console's Terminal. Keep preparation at the
-- polling owner, including polls made by output readers before desktop adoption.
data ConsoleScreen = ConsoleScreen
  { screenRevision :: Word64, screenBuffer :: Buffer, screenStyles :: StyledText }

prepareScreen :: TerminalSnapshot -> ConsoleScreen
prepareScreen snapshot = ConsoleScreen (snapshotScreenRevision snapshot) (newBuffer (styledContents styled)) styled
  where styled = snapshotStyles snapshot

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
        Right snapshot -> do
          console<-evaluate (recordOutput snapshot
            (Console terminal 0 (max 0 (min (16*1024*1024) limit)) BS.empty False snapshot (prepareScreen snapshot) Nothing False Nothing (ConsoleDisclosure False M.empty Nothing)))
          pure (Right (PreparedConsole console)))
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
  let paths=privateFilePaths desktop
  updated <- mapM (\original -> do
    console<-refreshDisclosure paths desktop original
    if consoleRetiring console then pure console else do
      result<-setTerminalAppearance (consoleTerminal console) (darkAppearance desktop)
      case result of
        Left message -> pure console {consoleError=Just message}
        Right () -> releaseLostCapture desktop console >>= resizeFor desktop >>= refresh) consoles
  let projected=foldr showConsole (desktop {terminalMouseTracking=S.empty}) updated
      shown=case drag projected of
        Just (TerminalDragging wid _ _ _) | dialog projected/=Nothing || menu projected/=Nothing || contextMenu projected/=Nothing ||
          not (any (\w->windowId w==wid && windowVisible projected w && maybe False (`S.member` terminalMouseTracking projected) (bufferId w)) (windows projected)) -> projected {drag=Nothing,dragOriginal=Nothing,dragTabs=Nothing,tabDropTarget=Nothing}
        _->projected
      errors = [message | (ident,console) <- M.toList updated, Just message <- [consoleError console]
                        , maybe True ((== Nothing) . consoleError) (M.lookup ident consoles)]
  pure ((counter,updated),case errors of message:_ -> shown {status=message}; [] -> shown)

-- | Refresh and return retained bytes, sticky truncation state and process exit status.
consoleOutput :: Consoles -> Text -> IO (Either Text (ByteString,Bool,Maybe Int))
consoleOutput consoles ident = updateConsole consoles ident $ \console -> if privateConsole console then pure (console,Left "Terminal output is private.") else do
  updated <- refresh console
  pure (updated,case consoleError updated of
    Just message -> Left message
    Nothing -> Right (retainedOutput updated,outputTruncated updated,snapshotExitCode (latestSnapshot updated)))

inputConsole :: Consoles -> Text -> ByteString -> IO (Either Text ())
inputConsole=inputConsoleWith True

-- Native host keyboard input is distinct from the permissioned public service.
-- Guest effects validate their exact existing public view before this host path.
inputHumanConsole :: Consoles -> Text -> ByteString -> IO (Either Text ())
inputHumanConsole=inputConsoleWith False

inputConsoleWith :: Bool -> Consoles -> Text -> ByteString -> IO (Either Text ())
inputConsoleWith public consoles ident bytes = updateConsole consoles ident $ \console -> if public && privateConsole console then pure (console,Left "Terminal output is private.") else do
  result <- if consoleRetiring console then pure (Left "Terminal has stopped") else maybe (writeTerminal (consoleTerminal console) bytes) (pure . Left) (consoleError console)
  pure (console,result)

-- | Deliver through the same live process boundary as keyboard input.
mouseConsole :: Consoles -> Text -> TerminalMouseEvent -> IO (Either Text ())
mouseConsole consoles ident event = updateConsole consoles ident $ \console -> do
  -- A new press can arrive before a UI-cancellation tick. Settle the previous
  -- delivered gesture first, even if its frame has already disappeared.
  case (consoleMouse console,terminalMouseAction event,terminalMouseButton event) of
    (Just held,TerminalMousePress,Just button) | button `elem` [TerminalMouseLeft,TerminalMouseMiddle,TerminalMouseRight]->
      sendTerminalMouse (consoleTerminal console) held {terminalMouseAction=TerminalMouseRelease} >> pure ()
    _->pure ()
  result <- if consoleRetiring console then pure (Left "Terminal has stopped") else
    maybe (sendTerminalMouse (consoleTerminal console) event) (pure . Left) (consoleError console)
  let held=case (result,terminalMouseAction event,terminalMouseButton event) of
        (Right (),TerminalMousePress,Just button) | button `elem` [TerminalMouseLeft,TerminalMouseMiddle,TerminalMouseRight]->Just event
        (Right (),TerminalMouseMotion,Just button) | (terminalMouseButton =<< consoleMouse console)==Just button->Just event
        (Right (),TerminalMouseRelease,_)->Nothing
        _->consoleMouse console
  pure (console {consoleMouse=held},result)

-- Pure UI transitions can dismiss or replace a capture without effects. The
-- process owner settles its last delivered press before resizing or polling.
releaseLostCapture :: Desktop -> Console -> IO Console
releaseLostCapture desktop console=case consoleMouse console of
  Just event | not captured -> do
    _<-sendTerminalMouse (consoleTerminal console) event {terminalMouseAction=TerminalMouseRelease}
    pure console {consoleMouse=Nothing}
  _->pure console
  where
    captured=dialog desktop==Nothing && menu desktop==Nothing && contextMenu desktop==Nothing &&
      snapshotMouseTracking (latestSnapshot console) && case drag desktop of
        Just (TerminalDragging wid button _ _)->any (\w->windowId w==wid && windowVisible desktop w && bufferId w==Just (consoleBuffer console)) (windows desktop) &&
          (terminalMouseButton =<< consoleMouse console)==terminalButton button
        _->False

-- | Stop normal polling and return cleanup to run outside the shared service lock.
-- The returned cleanup still has to be executed.
retireConsole :: Consoles -> Text -> IO (Either Text (IO ()))
retireConsole=retireConsoleWith False

retireConsoleWith :: Bool -> Consoles -> Text -> IO (Either Text (IO ()))
retireConsoleWith public (Consoles state) ident = modifyMVar state $ \(counter,consoles) -> case M.lookup ident consoles of
  Nothing -> pure ((counter,consoles),Left "Unknown or released terminal")
  Just console | public && privateConsole console -> pure ((counter,consoles),Left "Terminal output is private.")
  Just console | consoleRetiring console -> pure ((counter,consoles),Right (pure ()))
  Just console -> do
    let cleanup=do
          killTerminal (consoleTerminal console)
          updated<-refresh console
          closeTerminal (consoleTerminal console)
          modifyMVar_ state $ \(next,current) -> pure (next,M.adjust (\latest->updated {consoleRetiring=True,consoleDisclosure=consoleDisclosure latest}) ident current)
    pure ((counter,M.insert ident console {consoleRetiring=True} consoles),Right cleanup)

killConsole :: Consoles -> Text -> IO (Either Text ())
killConsole consoles ident = retireConsoleWith True consoles ident >>= either (pure . Left) (fmap Right)

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
        Right snapshot -> evaluate (recordOutput snapshot console)
        Left message -> do
          killTerminal (consoleTerminal console)
          pure console {consoleError=Just message}

recordOutput :: TerminalSnapshot -> Console -> Console
recordOutput snapshot original
  | BS.null (snapshotOutput snapshot) = console
  | otherwise = console
  { retainedOutput=retained, outputTruncated=truncated }
  where
    previous = latestScreen original
    console = original {latestSnapshot=snapshot,latestScreen=
      if screenRevision previous == snapshotScreenRevision snapshot then previous else prepareScreen snapshot}
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
    let prepared = latestScreen console
        -- Reinstall the same prepared references, without inspecting the old
        -- document. This also handles stale desktops and intervening output
        -- polls; no consumer can consume another view's pending screen update.
        document = original {documentBuffer=screenBuffer prepared,documentHighlight=screenStyles prepared,
          documentHasLayoutMetadata=False,documentWidth=snapshotColumns snapshot,
          documentCursorVisible=snapshotCursor snapshot /= Nothing,documentPrivate=documentPrivate original || privateConsole console}
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
              -- A terminal already owns exact grid extents. Generic source
              -- scrollbar discovery would scan visible text on every poll.
              , scrollRow=max 0 (min (max 0 (snapshotRows snapshot-max 1 (windowContentRows desktop document window))) (scrollRow window))
              , scrollColumn=max 0 (min (max 0 (snapshotColumns snapshot-max 1 (width (bounds window)-2)+1)) (scrollColumn window)) }
    in desktop {buffers=M.insert bid document (buffers desktop),windows=map adjust (windows desktop),
      terminalMouseTracking=if snapshotMouseTracking snapshot && snapshotExitCode snapshot==Nothing && consoleError console==Nothing && not (consoleRetiring console)
        then S.insert bid (terminalMouseTracking desktop) else S.delete bid (terminalMouseTracking desktop)}
  where
    bid = consoleBuffer console
    snapshot = latestSnapshot console

snapshotStyles :: TerminalSnapshot -> StyledText
snapshotStyles snapshot = intercalate [("\n",Plain)] (map styleRow (rows (snapshotCells snapshot)))
  where
    columns = max 1 (snapshotColumns snapshot)
    rows [] = []
    rows cells = let (row,rest) = splitAt columns cells in row : rows rest
    -- Prepare contiguous paint runs once, rather than making the compositor
    -- handle a separate styled slice for every terminal cell on every frame.
    -- Accumulate fragments before concatenating to keep each row linear.
    styleRow = map (\(texts,style)->(T.concat texts,style)) . foldr join [] . concatMap cell
    join (text,style) ((texts,previous):rest) | style==previous = (text:texts,previous):rest
    join (text,style) rest = ([text],style):rest
    cell value
      | cellWidth value == 0 = []
      | otherwise = [(if T.null (cellText value) then " " else cellText value,TerminalStyle (cellForeground value) (cellBackground value) (cellAttributes value))]

listConsoles :: Consoles -> IO [(Text,Int,Maybe Int)]
listConsoles (Consoles state)=withMVar state $ \(_,consoles) -> pure [(ident,consoleBuffer c,snapshotExitCode (latestSnapshot c)) | (ident,c)<-M.toAscList consoles,not (privateConsole c)]
