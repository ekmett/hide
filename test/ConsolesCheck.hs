{-# LANGUAGE OverloadedStrings #-}
module ConsolesCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Timeout (timeout)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import THC.Edit.Buffer
import THC.Edit.Consoles
import THC.Edit.Model
import THC.Edit.Syntax (Style(..))
import THC.Edit.Terminal

checks :: IO ()
checks = withConsoles $ \consoles -> do
  let desktop = initialDesktop (80,25)
  check "unknown output is rejected" . isLeft =<< consoleOutput consoles "missing"
  check "unknown input is rejected" . isLeft =<< inputConsole consoles "missing" "x"
  check "unknown kill is rejected" . isLeft =<< killConsole consoles "missing"
  check "unknown release is rejected" . isLeft =<< releaseConsole consoles "missing"
  unchanged <- tickConsoles consoles desktop
  check "empty collection leaves desktop unchanged" (unchanged == desktop)
  if terminalAvailable then nativeChecks consoles desktop else do
    unavailable <- startConsole consoles (config "true") 4096 desktop
    check "unavailable backend does not create terminal" (isLeft unavailable)
  putStrLn "Consoles checks passed"

nativeChecks :: Consoles -> Desktop -> IO ()
nativeChecks consoles desktop = do
  (ident,opened) <- startConsole consoles (config "printf '\\033[1;38;2;12;34;56;48;2;65;43;21mA\\033[0m界'; exit 7") 4096 desktop >>= requireRight
  let bid = maybe (error "missing terminal window") bufferId (activeWindow opened)
  check "numbered read-only terminal window" (maybe False (\doc -> documentLabel doc == Just ("Terminal " <> ident)) (activeDocument opened) && maybe False ((>0) . windowNumber) (activeWindow opened))
  (raw,truncated,exitCode) <- waitOutput consoles ident (\(_,_,code) -> code /= Nothing)
  check "exit and raw bytes retained" (exitCode == Just 7 && not truncated && TE.encodeUtf8 "界" `BS.isInfixOf` raw)
  let other = addDocument Nothing (newBuffer "focus stays here") opened
      selected = other {windows=map (\w -> if bufferId w == bid then w {bounds=Rect 0 1 22 6,selection=Selection 999999 999999,scrollRow=999999,scrollColumn=999999} else w) (windows other)}
  painted <- tickConsoles consoles selected
  check "tick never steals focus" (fmap windowId (activeWindow painted) == fmap windowId (activeWindow other))
  let doc = buffers painted M.! bid
      screen = contents (documentBuffer doc)
      terminalView = case [w | w <- windows painted,bufferId w == bid] of w:_ -> w; [] -> error "missing terminal window"
  check "truecolor and attributes reach document" (('A',TerminalStyle 0x0c2238 0x412b15 1) `elem` documentHighlight doc)
  check "wide continuation omitted and blank cells preserved" ("A界" `T.isPrefixOf` screen && T.length (T.takeWhile (/= '\n') screen) == 19 && length (T.lines screen) == 4)
  check "terminal selection and scrolling clamped" (caret (selection terminalView) <= T.length screen && scrollRow terminalView <= scrollbarLimit painted True doc terminalView && scrollColumn terminalView <= scrollbarLimit painted False doc terminalView)
  check "collapsed selection follows terminal cursor across wide glyph" (caret (selection terminalView) == 2)
  selecting <- tickConsoles consoles painted {windows=map (\w -> if bufferId w == bid then w {selection=Selection 0 2} else w) (windows painted)}
  check "tick preserves selected terminal text" (all ((== Selection 0 2) . selection) [w | w <- windows selecting,bufferId w == bid])
  let closed = painted {windows=filter ((/=bid) . bufferId) (windows painted),buffers=M.delete bid (buffers painted)}
  afterClose <- tickConsoles consoles closed
  check "closing window does not reopen it" (windows afterClose == windows closed && M.notMember bid (buffers afterClose))
  check "closed window retains raw output" . (== Right (raw,truncated,exitCode)) =<< consoleOutput consoles ident
  releaseConsole consoles ident >>= requireRight
  check "released terminal id becomes invalid" . isLeft =<< consoleOutput consoles ident
  (second,interactive) <- startConsole consoles (config "printf ready; read value; printf '%s' \"$value\"") 4096 painted >>= requireRight
  check "ids are never reused" (second /= ident)
  _ <- waitOutput consoles second (\(bytes,_,_) -> "ready" `BS.isInfixOf` bytes)
  inputConsole consoles second "typed\n" >>= requireRight
  (echoed,_,_) <- waitOutput consoles second (\(_,_,code) -> code /= Nothing)
  check "input reaches same terminal process" ("typed" `BS.isInfixOf` echoed)
  releaseConsole consoles second >>= requireRight
  afterRelease <- tickConsoles consoles interactive
  check "release keeps terminal screen document" (M.member (nextId painted) (buffers afterRelease))
  (third,_) <- startConsole consoles (config "printf '\\360\\237'; sleep 0.05; printf '\\230\\200xyz'") 5 desktop >>= requireRight
  (tailBytes,dropped,_) <- waitOutput consoles third (\(_,_,code) -> code /= Nothing)
  check "truncation drops leading UTF-8 continuation bytes" (dropped && tailBytes == "xyz")
  releaseConsole consoles third >>= requireRight
  (fourth,_) <- startConsole consoles (config "printf '\\360\\237'; sleep 0.05; printf '\\230\\200'") 8 desktop >>= requireRight
  (whole,notDropped,_) <- waitOutput consoles fourth (\(_,_,code) -> code /= Nothing)
  check "fragmented UTF-8 retained as raw bytes" (not notDropped && whole == TE.encodeUtf8 "😀")
  releaseConsole consoles fourth >>= requireRight
  (fifth,_) <- startConsole consoles (config "sleep 30") 0 desktop >>= requireRight
  killConsole consoles fifth >>= requireRight
  (_,_,killed) <- waitOutput consoles fifth (\(_,_,code) -> code /= Nothing)
  check "kill preserves exit status" (killed == Just 137)
  releaseConsole consoles fifth >>= requireRight
  (sixth,_) <- startConsole consoles (config "printf '\\360\\237\\230'; sleep 0.05; printf '\\200'") 2 desktop >>= requireRight
  (discarded,wasTruncated,_) <- waitOutput consoles sixth (\(_,_,code) -> code /= Nothing)
  check "late continuation never revives a truncated UTF-8 character" (wasTruncated && BS.null discarded)
  releaseConsole consoles sixth >>= requireRight
  (docking,dockDesktop) <- startConsole consoles (config "stty -echo; printf '%s\\n' $$; while IFS= read -r line; do printf '%s:%s\\n' $$ \"$line\"; done") 4096 desktop >>= requireRight
  (started,_,_) <- waitOutput consoles docking (\(bytes,_,_) -> "\n" `BS.isInfixOf` bytes)
  let dockView=case activeWindow dockDesktop of Just w->w; Nothing->error "missing dock terminal"
      dockId=windowId dockView
      dockBuffer=bufferId dockView
      processId=B8.takeWhile (\c->c/='\r' && c/='\n') started
      pinned=setTerminalPinned True dockId dockDesktop
      concealed=setProblemsVisible True pinned
  inputConsole consoles docking "hidden\n" >>= requireRight
  _ <- waitOutput consoles docking (\(bytes,_,_) -> (processId<>":hidden") `BS.isInfixOf` bytes)
  hiddenTick <- tickConsoles consoles concealed
  check "hidden terminal tab keeps polling without replacing its console window or buffer"
    (not (windowVisible hiddenTick dockView) && any ((==dockId).windowId) (windows hiddenTick) &&
     maybe False (T.isInfixOf ":hidden" . contents . documentBuffer) (M.lookup dockBuffer (buffers hiddenTick)))
  let floated=setTerminalPinned False dockId (focusWindow dockId hiddenTick)
  inputConsole consoles docking "floating\n" >>= requireRight
  (afterDock,_,_) <- waitOutput consoles docking (\(bytes,_,_) -> (processId<>":floating") `BS.isInfixOf` bytes)
  floatedTick <- tickConsoles consoles floated
  check "docking and undocking retain the same live process and input route"
    (activeTerminal floatedTick==Just docking && not (windowPinned floatedTick dockView) &&
     (processId<>":hidden") `BS.isInfixOf` afterDock && (processId<>":floating") `BS.isInfixOf` afterDock &&
     fmap bufferId (activeWindow floatedTick)==Just dockBuffer && fmap windowNumber (activeWindow floatedTick)==Just (windowNumber dockView))
  releaseConsole consoles docking >>= requireRight
  (resizing,resizeDesktop) <- startConsole consoles (config "stty -echo; printf ready; read line; printf '\\033[?25l'; read line; stty size; printf '\\033[?25h'; read line") 4096 desktop >>= requireRight
  _ <- waitOutput consoles resizing (\(bytes,_,_) -> "ready" `BS.isInfixOf` bytes)
  beforeHide <- tickConsoles consoles resizeDesktop
  inputConsole consoles resizing "hide\n" >>= requireRight
  _ <- waitOutput consoles resizing (\(bytes,_,_) -> "\ESC[?25l" `BS.isInfixOf` bytes)
  hidden <- tickConsoles consoles beforeHide
  check "cursor-only update hides cursor without changing screen" (fmap documentHighlight (activeDocument hidden) == fmap documentHighlight (activeDocument beforeHide) && maybe False (not . documentCursorVisible) (activeDocument hidden))
  let resized = case windows hidden of
        window:rest -> hidden {windows=window {bounds=Rect 0 1 42 12} : window {windowId=999,bounds=Rect 0 1 22 6} : rest}
        [] -> error "missing resize window"
  resizedScreen <- tickConsoles consoles resized
  inputConsole consoles resizing "size\n" >>= requireRight
  _ <- waitOutput consoles resizing (\(bytes,_,_) -> "10 40" `BS.isInfixOf` bytes)
  visible <- tickConsoles consoles resizedScreen
  check "frontmost view resizes the PTY and snapshot" (maybe False (\resizedDoc -> documentWidth resizedDoc == 40 && bufferLineCount (documentBuffer resizedDoc) == 10 && documentCursorVisible resizedDoc) (activeDocument visible))
  releaseConsole consoles resizing >>= requireRight
  pid <- withConsoles $ \owned -> do
    (running,_) <- startConsole owned (config "printf '%s' $$; sleep 30") 4096 desktop >>= requireRight
    (bytes,_,_) <- waitOutput owned running (\(bytes,_,_) -> not (BS.null bytes))
    pure (B8.unpack bytes)
  (gone,_,_) <- readProcessWithExitCode "/bin/kill" ["-0",pid] ""
  check "console scope shutdown reaps running processes" (gone /= ExitSuccess)

config :: String -> TerminalConfig
config script = TerminalConfig "/bin/sh" ["-c",script] [] "/tmp" 20 4

waitOutput :: Consoles -> T.Text -> ((BS.ByteString,Bool,Maybe Int) -> Bool) -> IO (BS.ByteString,Bool,Maybe Int)
waitOutput consoles ident done = do
  result <- timeout 5000000 loop
  maybe (error "Timed out waiting for console") pure result
  where
    loop = do
      output <- consoleOutput consoles ident >>= requireRight
      if done output then pure output else threadDelay 10000 >> loop

requireRight :: Either T.Text a -> IO a
requireRight = either (error . T.unpack) pure

check :: String -> Bool -> IO ()
check label ok = unless ok (error label)
