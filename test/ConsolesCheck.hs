{-# LANGUAGE OverloadedStrings #-}
module ConsolesCheck (checks) where

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Concurrent (threadDelay)
import Control.Monad (unless,forM_)
import qualified Graphics.Vty as V
import qualified Data.Set as S
import qualified Hide.Protocol as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Timeout (timeout)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Info (os)
import System.Environment (lookupEnv)
import Hide.Buffer
import Hide.Consoles
import Hide.Model
import Hide.Syntax (Style(..))
import Hide.Terminal

checks :: IO ()
checks = withConsoles $ \consoles -> do
  routingChecks
  let desktop = initialDesktop (80,25)
  check "unknown output is rejected" . isLeft =<< consoleOutput consoles "missing"
  check "unknown input is rejected" . isLeft =<< inputConsole consoles "missing" "x"
  check "unknown kill is rejected" . isLeft =<< killConsole consoles "missing"
  check "unknown release is rejected" . isLeft =<< releaseConsole consoles "missing"
  unchanged <- tickConsoles consoles desktop
  check "empty collection leaves desktop unchanged" (unchanged == desktop)
  if terminalAvailable then (if os=="mingw32" then windowsChecks else nativeChecks) consoles desktop else do
    unavailable <- startConsole consoles (config "true") 4096 desktop
    check "unavailable backend does not create terminal" (isLeft unavailable)
  putStrLn "Consoles checks passed"

windowsChecks :: Consoles -> Desktop -> IO ()
windowsChecks consoles desktop = do
  shell<-maybe "cmd.exe" id <$> lookupEnv "COMSPEC"
  let windowsConfig=TerminalConfig shell ["/d","/c","echo retained-output & exit /b 7"] [] "." 40 5
  (ident,opened)<-startConsole consoles windowsConfig 4096 desktop >>= requireRight
  (raw,truncated,code)<-waitOutput consoles ident (\(_,_,exit)->exit/=Nothing)
  check "ConPTY console output and exit retained" ("retained-output" `BS.isInfixOf` raw && not truncated && code==Just 7)
  let bid=maybe (error "missing ConPTY window") sourceFixtureBuffer (activeWindow opened)
      closed=opened {windows=filter ((/=bid).sourceFixtureBuffer) (windows opened),buffers=M.delete bid (buffers opened)}
  afterClose<-tickConsoles consoles closed
  check "ConPTY closed window stays closed" (M.notMember bid (buffers afterClose))
  check "ConPTY closed window retains captured output" . (==Right (raw,truncated,code)) =<< consoleOutput consoles ident
  releaseConsole consoles ident >>= requireRight
  check "released ConPTY console ID is rejected" . isLeft =<< consoleOutput consoles ident
  (second,_)<-startConsole consoles windowsConfig 8 desktop >>= requireRight
  (_,limited,_)<-waitOutput consoles second (\(_,_,exit)->exit/=Nothing)
  check "ConPTY console output bound is explicit" (second/=ident && limited)
  releaseConsole consoles second >>= requireRight

nativeChecks :: Consoles -> Desktop -> IO ()
nativeChecks consoles desktop = do
  forM_ [ ("resize",fst . handleEvent (V.EvResize 90 30))
        , ("modal",message "Question" ["Current modal"])
        , ("close",closeActive)] $ \(label,cancelCapture)->do
    (ident,opened)<-startConsole consoles (config "stty raw -echo; printf '\\033[?1000h\\033[?1006hready'; dd bs=1 count=18 2>/dev/null") 4096 desktop >>= requireRight
    _<-waitOutput consoles ident (\(bytes,_,_)->"ready" `BS.isInfixOf` bytes)
    ready<-tickConsoles consoles opened
    let w=maybe (error "missing mouse terminal") id (activeWindow ready)
        (pressed,effects)=P.applyInput (P.Mouse "down" (left (bounds w)+3) (top (bounds w)+2) 0 1 []) ready
    forM_ effects $ \effect->case effect of
      TerminalMouseInput target event->mouseConsole consoles target event >>= requireRight
      _->error "unexpected mouse effect"
    _<-tickConsoles consoles (cancelCapture pressed)
    (bytes,_,_)<-waitOutput consoles ident (\(_,_,code)->code/=Nothing)
    check ("lost "++label++" capture releases the actual PTY press") ("\ESC[<0;3;2M\ESC[<0;3;2m" `BS.isInfixOf` bytes)
    releaseConsole consoles ident >>= requireRight
  (ident,opened) <- startConsole consoles (config "printf '\\033[1;38;2;12;34;56;48;2;65;43;21mA\\033[0m界'; exit 7") 4096 desktop >>= requireRight
  let bid = maybe (error "missing terminal window") sourceFixtureBuffer (activeWindow opened)
  check "numbered read-only terminal window" (maybe False (\doc -> documentLabel doc == Just ("Terminal " <> ident)) (activeDocument opened) && maybe False ((>0) . windowNumber) (activeWindow opened))
  (raw,truncated,exitCode) <- waitOutput consoles ident (\(_,_,code) -> code /= Nothing)
  check "exit and raw bytes retained" (exitCode == Just 7 && not truncated && TE.encodeUtf8 "界" `BS.isInfixOf` raw)
  let other = addDocument Nothing (newBuffer "focus stays here") opened
      selected = other {windows=map (\w -> if sourceFixtureBuffer w == bid then w {bounds=Rect 0 1 22 6,selection=Selection 999999 999999,scrollRow=999999,scrollColumn=999999} else w) (windows other)}
  painted <- tickConsoles consoles selected
  check "tick never steals focus" (fmap windowId (activeWindow painted) == fmap windowId (activeWindow other))
  let doc = buffers painted M.! bid
      screen = contents (documentBuffer doc)
      terminalView = case [w | w <- windows painted,sourceFixtureBuffer w == bid] of w:_ -> w; [] -> error "missing terminal window"
  check "truecolor and attributes reach document" (any (\(text,style)->"A" `T.isInfixOf` text && style==TerminalStyle 0x0c2238 0x412b15 1) (documentHighlight doc))
  check "wide continuation omitted and blank cells preserved" ("A界" `T.isPrefixOf` screen && T.length (T.takeWhile (/= '\n') screen) == 19 && length (T.lines screen) == 4)
  check "terminal selection and scrolling clamped" (caret (selection terminalView) <= T.length screen && scrollRow terminalView <= scrollbarLimit painted True doc terminalView && scrollColumn terminalView <= scrollbarLimit painted False doc terminalView)
  check "collapsed selection follows terminal cursor across wide glyph" (caret (selection terminalView) == 2)
  selecting <- tickConsoles consoles painted {windows=map (\w -> if sourceFixtureBuffer w == bid then w {selection=Selection 0 2} else w) (windows painted)}
  check "tick preserves selected terminal text" (all ((== Selection 0 2) . selection) [w | w <- windows selecting,sourceFixtureBuffer w == bid])
  let closed = painted {windows=filter ((/=bid) . sourceFixtureBuffer) (windows painted),buffers=M.delete bid (buffers painted)}
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
      dockBuffer=sourceFixtureBuffer dockView
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
     fmap sourceFixtureBuffer (activeWindow floatedTick)==Just dockBuffer && fmap windowNumber (activeWindow floatedTick)==Just (windowNumber dockView))
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
  -- stty output can arrive before the shell writes the following cursor-show.
  _ <- waitOutput consoles resizing (\(bytes,_,_) -> "10 40" `BS.isInfixOf` bytes && "\ESC[?25h" `BS.isInfixOf` bytes)
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

-- Exercise the ordinary wire path, using only small capture/effect observations.
routingChecks :: IO ()
routingChecks = do
  let opened=addDocument Nothing (newBuffer "screen") (initialDesktop (80,25))
      w=maybe (error "missing mouse fixture") id (activeWindow opened)
      bid=sourceFixtureBuffer w
      d=opened {buffers=M.adjust (\doc->doc {documentLabel=Just "Terminal live"}) bid (buffers opened),
        terminalMouseTracking=S.singleton bid}
      x=left (bounds w)+3; y=top (bounds w)+2
      apply action button clicks mods=P.applyInput (P.Mouse action x y button clicks mods)
      (pressed,press)=apply "down" 0 1 [] d
      (otherUp,ignored)=apply "up" 2 1 [] pressed
      (moved,motion)=P.applyInput (P.Mouse "move" (-1) (-1) 0 0 []) otherUp
      (released,release)=P.applyInput (P.Mouse "up" (-1) (-1) (-1) 1 []) moved
      (_,wheel)=P.applyInput (P.Wheel x y 2 []) pressed
      (_,double)=apply "down" 0 2 [] d
      (shifted,local)=apply "down" 0 1 [V.MShift] d
      (_,frame)=P.applyInput (P.Mouse "down" (left (bounds w)) (top (bounds w)) 0 1 []) d
      effect action button requests=case requests of
        [TerminalMouseInput "live" event]->terminalMouseAction event==action && terminalMouseButton event==button
        _->False
      (blurred,blur)=P.applyInput P.Blur pressed
      (_,noMode)=apply "down" 0 1 [] d {terminalMouseTracking=S.empty}
      (_,hover)=P.applyInput (P.Mouse "move" x y 0 0 []) d
  check "terminal mouse uses client coordinates" (case press of
    [TerminalMouseInput "live" event]->terminalMouseColumn event==2 && terminalMouseRow event==1
    _->False)
  check "unrelated release preserves captured terminal button" (drag otherUp==drag pressed && null ignored)
  check "captured motion and unknown release keep their original owner" (effect TerminalMouseMotion (Just TerminalMouseLeft) motion && effect TerminalMouseRelease (Just TerminalMouseLeft) release && drag released==Nothing)
  check "wheel remains wheel while terminal drag is captured" (length wheel==2 && all (effect TerminalMousePress (Just TerminalMouseWheelUp) . pure) wheel)
  check "terminal double click stays application input" (effect TerminalMousePress (Just TerminalMouseLeft) double)
  check "Shift bypasses application tracking for local selection" (null local && case drag shifted of Just Selecting{}->True; _->False)
  check "terminal frame and disabled tracking stay local" (null frame && null noMode)
  check "blur releases the captured application button" (drag blurred==Nothing && effect TerminalMouseRelease (Just TerminalMouseLeft) blur)
  check "unbuttoned terminal hover is an application motion" (effect TerminalMouseMotion Nothing hover)
