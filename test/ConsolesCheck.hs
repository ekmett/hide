-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : ConsolesCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module ConsolesCheck (checks,privacyChecks) where

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate,try,IOException)
import Control.DeepSeq (force)
import Control.Monad (unless,forM_,foldM)
import GHC.Conc (getAllocationCounter)
import AllocationProfile (AllocationProfile,withinBudget)
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
import Hide.Render (renderKey,snapshot)
import Hide.GuestAccess (protectedBuffer,sanitizedBuffer,validateGuestEffects)
import Hide.Unicode (graphemes)

checks :: AllocationProfile -> IO ()
checks profile = withConsoles $ \consoles -> do
  routingChecks
  let desktop = initialDesktop (80,25)
  check "unknown output is rejected" . isLeft =<< consoleOutput consoles "missing"
  check "unknown input is rejected" . isLeft =<< inputConsole consoles "missing" "x"
  check "unknown kill is rejected" . isLeft =<< killConsole consoles "missing"
  check "unknown release is rejected" . isLeft =<< releaseConsole consoles "missing"
  unchanged <- tickConsoles consoles desktop
  check "empty collection leaves desktop unchanged" (unchanged == desktop)
  if terminalAvailable then do
    privacyChecks
    idleChecks profile consoles
    (if os=="mingw32" then windowsChecks else nativeChecks) consoles desktop
  else do
    unavailable <- startConsole consoles (config "true") 4096 desktop
    check "unavailable backend does not create terminal" (isLeft unavailable)
  putStrLn "Consoles checks passed"

-- Retained source ownership is independent of DAP rows and process cleanup.
privacyChecks :: IO ()
privacyChecks=if not terminalAvailable then pure () else withConsoles $ \consoles->do
  shell<-maybe "cmd.exe" id <$> lookupEnv "COMSPEC"
  let launch script windows=if os=="mingw32" then TerminalConfig shell ["/d","/c",windows] [] "." 80 10
        else TerminalConfig "/bin/sh" ["-c",script] [] "." 80 10
      first=addDocument Nothing (newBuffer "source A\n") (initialDesktop (82,25))
      sourceA=maybe (error "missing source A") sourceFixtureBuffer (activeWindow first)
      base=addDocument Nothing (newBuffer "source B\n") first
      sourceB=maybe (error "missing source B") sourceFixtureBuffer (activeWindow base)
      awaitHost label marker desktop=timeout 5000000 (loop desktop) >>= maybe (error label) pure
        where loop current=do
                next<-tickConsoles consoles current
                if maybe False (T.isInfixOf marker . contents . documentBuffer) (activeDocument next) then pure next
                  else threadDelay 1000 >> loop next
  (ident,opened)<-startConsole consoles (launch "printf 'PUBLIC_LOG_RECEIPT\n'; IFS= read -r value; printf 'HOST_INPUT_OK:%s\n' $value" "echo PUBLIC_LOG_RECEIPT & set /p value= & echo HOST_INPUT_OK") 4096 base >>= requireRight
  _<-waitOutput consoles ident (\(bytes,_,_)->"PUBLIC_LOG_RECEIPT" `BS.isInfixOf` bytes)
  public<-tickConsoles consoles opened
  let bid=maybe (error "missing private terminal") sourceFixtureBuffer (activeWindow public)
  retainConsoleSources consoles ident [(Just sourceA,Nothing)] False
  retainConsoleSources consoles ident [(Just sourceB,Nothing)] False
  retainConsoleSources consoles ident [] False
  (private,allowed)<-publicConsoleAt consoles ident public {buffers=M.adjust (\doc->doc {documentPrivate=True}) sourceA (buffers public)}
  check "accumulated original source makes retained terminal private at publication" (isLeft allowed && protectedBuffer private bid && sanitizedBuffer private bid==Nothing)
  check "human terminal rendering remains available while streamer output hides it"
    ("PUBLIC_LOG_RECEIPT" `T.isInfixOf` snapshot private && not ("PUBLIC_LOG_RECEIPT" `T.isInfixOf` snapshot private {streamerMode=True}))
  check "private terminal output is unavailable to shared readers" . isLeft =<< consoleOutput consoles ident
  check "private terminal input is unavailable to shared callers" . isLeft =<< inputConsole consoles ident "agent\n"
  check "private terminal is omitted from shared terminal listing" . all (\(tid,_,_)->tid/=ident) =<< listConsoles consoles
  blocked<-try (validateGuestEffects private [ServiceAction "terminal-input" [ident,"agent\n"]]) :: IO (Either IOException ())
  check "guest input cannot forge the native host terminal effect" (isLeft blocked)
  inputHumanConsole consoles ident "host-private\n" >>= requireRight
  human<-awaitHost "private host input receipt" "HOST_INPUT_OK" private
  let marker=if os=="mingw32" then "HOST_INPUT_OK" else "HOST_INPUT_OK:host-private"
  check "host input keeps its exact terminal authority" (maybe False (T.isInfixOf marker . contents . documentBuffer) (activeDocument human))
  -- Retirement captures a public Console. A later privacy transition must not
  -- be overwritten when that exact deferred cleanup adopts its final snapshot.
  (retained,second)<-startConsole consoles (launch "printf RETIRED_LOG_RECEIPT" "echo RETIRED_LOG_RECEIPT") 4096 human >>= requireRight
  _<-waitOutput consoles retained (\(_,_,code)->code/=Nothing)
  retainConsoleSources consoles retained [(Just sourceB,Nothing)] False
  cleanup<-retireConsole consoles retained >>= requireRight
  marked<-refreshConsolePrivacy consoles second {buffers=M.adjust (\doc->doc {documentPrivate=True,documentBuffer=error "terminal privacy forced source payload"}) sourceB (buffers second)}
  cleanup
  check "cleanup preserves privacy acquired after transport retirement" . isLeft =<< consoleOutput consoles retained
  (sticky,stillPrivate)<-publicConsoleAt consoles retained marked {buffers=M.adjust (\doc->doc {documentPrivate=False}) sourceB (buffers marked)}
  check "retained terminal privacy cannot be downgraded by later public source policy" (isLeft stillPrivate)
  let (closed,_)=runCommand Close sticky
  check "closed retained terminal still refuses shared output" . isLeft =<< consoleOutput consoles retained
  check "closing private terminal does not retain its view" (activeTerminal closed/=Just retained)
  let spoofed=addReadOnly ("Terminal "<>retained) "unrelated generated source" closed
  (_,spoofAccess)<-publicConsoleAt consoles retained spoofed
  check "an unrelated public label cannot claim retained private terminal authority" (isLeft spoofAccess)

-- Polling output tools and polling the desktop share one terminal owner. An
-- intervening read must neither consume a screen update nor rebuild idle text.
idleChecks :: AllocationProfile -> Consoles -> IO ()
idleChecks profile consoles = do
  shell<-maybe "cmd.exe" id <$> lookupEnv "COMSPEC"
  let launch=if os=="mingw32" then TerminalConfig shell ["/d","/c","echo prepared-screen & exit /b 0"] [] "." 160 50
        else TerminalConfig "/bin/sh" ["-c","printf prepared-screen"] [] "." 160 50
  (ident,opened)<-startConsole consoles launch 4096 (initialDesktop (162,54)) >>= requireRight
  _<-waitOutput consoles ident (\(_,_,code)->code/=Nothing)
  ready<-tickConsoles consoles opened {windows=map (\w->w {bounds=Rect 0 1 162 52}) (windows opened)}
  let bid=maybe (error "missing idle terminal") sourceFixtureBuffer (activeWindow ready)
  check "output read preserves the screen for subsequent desktop adoption"
    (maybe False (T.isPrefixOf "prepared-screen" . contents . documentBuffer) (activeDocument ready))
  key<-renderKey ready
  before<-getAllocationCounter
  let idleLoop start=foldM (\d _->do
        _<-consoleOutput consoles ident >>= requireRight
        next<-tickConsoles consoles d
        nextKey<-renderKey next
        check "idle console polling does not request a redraw" (key==nextKey)
        pure next) start [1..20::Int]
  idle<-idleLoop ready
  after<-getAllocationCounter
  check ("idle terminal polling avoids screen-sized allocation: "++show (before-after))
    (withinBudget profile (before-after) 1000000)
  retainConsoleSources consoles ident [(Just bid,Nothing)] False
  trackedBefore<-getAllocationCounter
  tracked<-idleLoop idle
  trackedAfter<-getAllocationCounter
  let baseline=before-after
      provenance=trackedBefore-trackedAfter
  check ("public source provenance preserves idle allocation baseline: "++show (baseline,provenance)) (provenance<=2*max 1 baseline)
  trackedKey<-renderKey tracked
  check "public provenance alone cannot invalidate terminal rendering" (trackedKey==key)
  _<-evaluate (force (P.frameRows idle))
  frameBefore<-getAllocationCounter
  _<-evaluate (force (P.frameRows idle {status="Terminal redraw"}))
  frameAfter<-getAllocationCounter
  check ("prepared terminal frame avoids per-cell style runs: "++show (frameBefore-frameAfter))
    (withinBudget profile (frameBefore-frameAfter) 8000000)
  let hidden=idle {buffers=M.adjust (\doc->doc {documentHighlight=error "idle adoption inspected the old highlighted screen"}) bid (buffers idle)}
  adopted<-tickConsoles consoles hidden
  _<-evaluate (length (documentHighlight (buffers adopted M.! bid)))
  repeated<-tickConsoles consoles ready
  repeatedKey<-renderKey repeated
  check "repeated adoption after intervening polls retains prepared payload identity" (key==repeatedKey)
  releaseConsole consoles ident >>= requireRight

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
  (ident,opened) <- startConsole consoles (config "printf '\\033[1;38;2;12;34;56;48;2;65;43;21mA\\033[0m界é'; exit 7") 4096 desktop >>= requireRight
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
  check "wide continuation omitted and combining clusters and blank cells preserved" ("A界é" `T.isPrefixOf` screen && T.length (T.takeWhile (/= '\n') screen) == 20 && length (T.lines screen) == 4)
  let perCell (text,style@TerminalStyle{})=[(cluster,style) | cluster<-graphemes text]
      perCell styled=[styled]
      expanded d=d {buffers=M.adjust (\value->value {documentHighlight=concatMap perCell (documentHighlight value)}) bid (buffers d)}
  check "coalesced terminal styles preserve exact rendered cells across wide and combining glyphs"
    (P.frameRows painted==P.frameRows (expanded painted))
  check "terminal selection and scrolling clamped" (caret (selection terminalView) <= T.length screen && scrollRow terminalView <= scrollbarLimit painted True doc terminalView && scrollColumn terminalView <= scrollbarLimit painted False doc terminalView)
  check "collapsed selection follows terminal cursor across wide and combining glyphs" (caret (selection terminalView) == 4)
  selecting <- tickConsoles consoles painted {windows=map (\w -> if sourceFixtureBuffer w == bid then w {selection=Selection 0 2} else w) (windows painted)}
  check "tick preserves selected terminal text" (all ((== Selection 0 2) . selection) [w | w <- windows selecting,sourceFixtureBuffer w == bid])
  check "coalesced terminal runs preserve rendered selection" (P.frameRows selecting==P.frameRows (expanded selecting))
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
