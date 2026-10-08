{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
-- | Vty frontend for validated remote cell frames.
--
-- Bounded sender/receiver queues keep transport and frame decoding outside input
-- handling. Disconnects retain the last picture with a notice and suppress remote
-- input; local detach remains available. Clipboard export uses OSC 52 and does
-- not read the terminal host clipboard.
module Hide.RemoteTerminal
  (runRemoteTerminal, terminalEventInput, remoteTerminalDisplay, terminalClipboard
#ifdef WITH_REMOTE
  , Incoming(..), receiveTerminalFrames
#endif
  ) where

import Data.Aeson
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as Vec
import Data.Char (isPrint)
import Graphics.Vty.Span (DisplayOps)
import Hide.Links (openResource)
import Hide.Remote (RemotePeer)
import Hide.TextStyle
import Hide.RemoteWindow (RemoteFrame(..), RemoteCell(..), remoteBindingInput)
import Hide.Unicode (CellSpan(..),cellDisplayOps,displayItems,itemDisplayText,itemWidth)
#ifdef WITH_REMOTE
import Control.Concurrent.Async (withAsync, poll)
import Control.Concurrent.STM
import Control.Exception (bracket, finally, throwIO)
import Control.Monad (forever, forM_, unless, when)
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Graphics.Vty.CrossPlatform (mkVty)
import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (stdout, stderr, hPutStrLn, hFlush, openBinaryTempFile, hClose)
import System.Timeout (timeout)
import Hide.Protocol (WirePacket(..), decodeFrame,parseClipboardRequest,clipboardReplyInput)
import Hide.FileExport (FileExports,withFileExports,startHelperFileExport)
import Hide.Remote (peerReceive, peerSend)
import Hide.RemoteWindow (parseRemoteFrame, sanitizeDownloadName)
import Hide.Unicode (updateDisplayOps)
#endif

-- | Translate supported Vty events with bounded coordinates, dimensions and paste sizes.
terminalEventInput :: V.Event -> Maybe Value
terminalEventInput event = case event of
  V.EvKey key modifiers -> do
    name <- case key of
      V.KChar '\t' -> Just "Tab"
      V.KChar c | c>=' ' && c/='\DEL' -> Just (T.singleton c)
      V.KFun n | n>=1 && n<=24 -> Just ("F"<>T.pack (show n))
      _ -> lookup key [(V.KUp,"ArrowUp"),(V.KDown,"ArrowDown"),(V.KLeft,"ArrowLeft"),(V.KRight,"ArrowRight"),
        (V.KHome,"Home"),(V.KEnd,"End"),(V.KPageUp,"PageUp"),(V.KPageDown,"PageDown"),(V.KBackTab,"Tab"),
        (V.KEnter,"Enter"),(V.KEsc,"Escape"),(V.KBS,"Backspace"),(V.KDel,"Delete"),(V.KIns,"Insert")]
    pure (object ["type" .= ("key"::T.Text),"key" .= name,"mods" .= mods (if key==V.KBackTab then V.MShift:modifiers else modifiers)])
  V.EvPaste bytes | BS.length bytes<=4194304, Right text <- TE.decodeUtf8' bytes, T.length text<=1048576 ->
    Just (object ["type" .= ("paste"::T.Text),"text" .= text])
  V.EvResize w h -> Just (object ["type" .= ("resize"::T.Text),"width" .= max 40 (min 512 w),"height" .= max 12 (min 256 h)])
  V.EvMouseDown x y button modifiers -> mouse x y (case button of V.BScrollUp -> "wheel-up"; V.BScrollDown -> "wheel-down"; _ -> "down") button modifiers
  V.EvMouseUp x y button -> mouse x y "up" (maybe V.BLeft id button) []
  V.EvLostFocus -> Just (object ["type" .= ("blur"::T.Text)])
  _ -> Nothing
  where
    mods ms = [name | (modifier,name)<-[(V.MShift,"shift"::T.Text),(V.MCtrl,"ctrl"),(V.MAlt,"alt"),(V.MMeta,"alt")],modifier `elem` ms]
    mouse x y action button modifiers = Just (object ["type" .= ("mouse"::T.Text),"action" .= (action::T.Text),
      "x" .= max (-1) (min 511 x),"y" .= max (-1) (min 255 y),"button" .= (case button of V.BRight -> 2; V.BMiddle -> 1; _ -> 0::Int),"clicks" .= (1::Int),"mods" .= mods modifiers])

-- | Project validated remote cells directly at the actual terminal bounds.
-- Sparse gaps and partial glyphs occupy blanks. A notice covers only its bottom
-- row prefix, preserving the frame suffix and suppressing a partly covered glyph.
remoteTerminalDisplay :: (Int,Int) -> Maybe RemoteFrame -> T.Text -> (V.Cursor,DisplayOps)
remoteTerminalDisplay (columns,linesCount) frame message=(cursor,cellDisplayOps (Vec.generate height row))
  where
    width=max 0 columns; height=max 0 linesCount
    cells=maybe [] remoteCells frame
    rows=IM.fromListWith (++) [(y,[cell]) | cell<-cells,let y=case cell of RemoteText _ row _ _ _->row; RemoteGlyph _ row _ _ _ _ _->row; RemoteScript _ row _ _ _ _->row,y<height]
    blanks n=[CellText V.defAttr (T.replicate n " ") | n>0]
    spans at []=blanks (width-at)
    spans at (cell:rest)
      | x>=width=blanks (width-at)
      | otherwise=blanks (x-at)++glyph:spans (x+visible) rest
      where
        (x,shown,glyph)=case cell of
          RemoteText left _ paint text n->(left,n,CellText (textStyleAttr paint) (T.take (min n (width-left)) text))
          RemoteGlyph left _ paint text full start n->(left,n,CellGlyph (textStyleAttr paint) text full start (min n (width-left)))
          RemoteScript left _ paint text natural script->(left,1,CellScript (textStyleAttr paint) text natural script)
        visible=min shown (width-x)
    bannerPaint=V.defAttr `V.withForeColor` V.white `V.withBackColor` V.blue
    banner=[if n==1 && T.length glyph==1 then CellText bannerPaint glyph else CellGlyph bannerPaint glyph n 0 n
      | item<-displayItems (T.filter isPrint message),let glyph=itemDisplayText item,let n=itemWidth item,n>0]
    bannerWidth=min width (sum (map spanWidth banner))
    spanWidth (CellText _ text)=T.length text
    spanWidth (CellGlyph _ _ _ _ shown)=shown
    spanWidth CellScript{}=1
    row y=Vec.fromList (compact (if not (T.null message) && y==height-1
      then clip 0 width banner++clip bannerWidth width base else base))
      where base=spans 0 (reverse (IM.findWithDefault [] y rows))
    cursor | not (T.null message)=V.NoCursor
           | otherwise=maybe V.NoCursor (maybe V.NoCursor (uncurry V.Cursor) . remoteCursor) frame
    compact (CellText attr text:rest)=let (same,after)=span (\value->case value of CellText paint _->paint==attr; _->False) rest
                                    in CellText attr (if null same then text else T.concat (text:[value | CellText _ value<-same])):compact after
    compact (value:rest)=value:compact rest
    compact []=[]
    clip lo hi=go 0
      where
        go _ []=[]
        go at (value:rest)
          | at>=hi=[]
          | right<=left=go (at+n) rest
          | otherwise=part:go (at+n) rest
          where
            n=spanWidth value
            left=max lo at; right=min hi (at+n)
            part=case value of
              CellText paint text->CellText paint (T.take (right-left) (T.drop (left-at) text))
              CellGlyph paint text full start _->CellGlyph paint text full (start+left-at) (right-left)
              CellScript{}->value

-- | Construct a base64 OSC 52 clipboard-write sequence; no clipboard read is performed.
terminalClipboard :: T.Text -> BS.ByteString
terminalClipboard text="\ESC]52;c;"<>BS.pack (base64 (BS.unpack (TE.encodeUtf8 text)))<>"\BEL"
  where
    alphabet=BS.unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    at n=alphabet !! fromIntegral n
    base64 (a:b:c:rest)=[at (a `shiftR` 2),at ((a .&. 3)*16+(b `shiftR` 4)),at ((b .&. 15)*4+(c `shiftR` 6)),at (c .&. 63)]++base64 rest
    base64 [a,b]=[at (a `shiftR` 2),at ((a .&. 3)*16+(b `shiftR` 4)),at ((b .&. 15)*4),61]
    base64 [a]=[at (a `shiftR` 2),at ((a .&. 3)*16),61,61]
    base64 []=[]

#ifdef WITH_REMOTE
data Incoming = Frame RemoteFrame | Control Value

-- | Scope terminal setup, remote display/input handling and cursor restoration.
-- Detach performs a bounded outbound handoff wait.
runRemoteTerminal :: RemotePeer -> IO ()
runRemoteTerminal peer = withFileExports $ \exports -> bracket (mkVty V.defaultConfig) (\vty -> V.shutdown vty `finally` cursorStyle Nothing) $ \vty -> do
  forM_ [V.Mouse,V.BracketedPaste,V.Focus] $ \mode ->
    when (V.supportsMode (V.outputIface vty) mode) (V.setMode (V.outputIface vty) mode True)
  incoming <- newTBQueueIO 8
  outgoing <- newTBQueueIO 64
  queued <- newTVarIO (0::Int)
  let send value = do
        let size=fromIntegral (BL.length (encode value))
        accepted <- atomically $ do
          full <- isFullTBQueue outgoing
          bytes <- readTVar queued
          if full || bytes+size>33554432 then pure False else do
            writeTBQueue outgoing (size,value)
            writeTVar queued (bytes+size)
            pure True
        unless accepted (ioError (userError "Remote terminal input queue is full"))
      drain = do
        sent <- timeout 2000000 (atomically (readTVar queued >>= check . (==0)))
        when (sent==Nothing) (hPutStrLn stderr "Some terminal input could not be handed to the session before detaching.")
      resize = V.displayBounds (V.outputIface vty) >>= \(w,h) -> forM_ (terminalEventInput (V.EvResize w h)) send
      render frame message = do
        size <- V.displayBounds (V.outputIface vty)
        let (cursor,ops)=remoteTerminalDisplay size frame message
        updateDisplayOps (V.outputIface vty) size cursor ops
      loop receiver sender frame connected clipboard notice = do
        forM_ [receiver,sender] $ \worker -> poll worker >>= \result -> case result of Just (Left err) -> throwIO err; _ -> pure ()
        message <- atomically (tryReadTBQueue incoming)
        case message of
          Just (Frame next) -> do
            when (maybe True ((/=remoteBlink next).remoteBlink) frame) (cursorStyle (Just (remoteBlink next)))
            render (Just next) (if connected then notice else "Connecting; Ctrl+] detaches")
            loop receiver sender (Just next) connected clipboard notice
          Just (Control value) -> do
            kind <- parseIO (withObject "control" (.: "type")) value :: IO T.Text
            case kind of
              "closed" -> pure ()
              "connection" -> do
                live <- parseIO (withObject "connection" (.: "connected")) value
                when live resize
                render frame (if live then "" else "Reconnecting; Ctrl+] detaches")
                loop receiver sender frame live clipboard ""
              "copy" -> do
                copied <- parseIO (withObject "copy" (.: "text")) value
                when (T.length copied>1048576) (ioError (userError "Remote clipboard exceeds 1 MiB"))
                BS.hPut stdout (terminalClipboard copied) >> hFlush stdout
                loop receiver sender frame connected (Just copied) notice
              "notice" -> do
                notification <- parseIO (withObject "notice" (.: "message")) value
                render frame notification
                loop receiver sender frame connected clipboard notification
              "open-resource" -> do
                result<-openResource value
                let notification=either id (const "Opened link") result
                render frame notification
                loop receiver sender frame connected clipboard notification
              "paste-request" -> do
                request<-parseIO parseClipboardRequest value
                forM_ (clipboard >>= clipboardReplyInput request) send
                let hint=if clipboard==Nothing then "Paste with your terminal's paste shortcut; Ctrl+] detaches" else ""
                render frame hint
                loop receiver sender frame connected clipboard hint
              _ -> loop receiver sender frame connected clipboard notice
          Nothing -> do
            event <- timeout 30000 (V.nextEvent vty)
            case event of
              Just (V.EvKey (V.KChar ']') [V.MCtrl]) -> pure ()
              _ -> do
                when connected (forM_ (event >>= \input->maybe (terminalEventInput input) (\value->remoteBindingInput value input (terminalEventInput input)) frame) send)
                when (connected && event/=Nothing && not (T.null notice)) (render frame "")
                loop receiver sender frame connected clipboard (if event==Nothing then notice else "")
  resize
  render Nothing "Connecting; Ctrl+] detaches"
  withAsync (receiveTerminalFrames exports peer incoming) $ \receiver ->
    withAsync (forever $ do
      (size,value) <- atomically (readTBQueue outgoing)
      peerSend peer (JsonPacket value)
      atomically (modifyTVar' queued (subtract size))) $ \sender ->
        loop receiver sender Nothing False Nothing "" `finally` drain
  where
    cursorStyle blinking = putStr (case blinking of Just True -> "\ESC[3 q"; Just False -> "\ESC[4 q"; Nothing -> "\ESC[0 q") >> hFlush stdout

parseIO :: (Value -> Parser a) -> Value -> IO a
parseIO parser = either (ioError . userError) pure . parseEither parser

-- | Receive validated frames and paired binary downloads on the existing worker.
-- Saved-file exports use the frontend's owned staging/helper lifetime; no helper
-- runs on the session host and no transport sends occur from this receiver.
receiveTerminalFrames :: FileExports -> RemotePeer -> TBQueue Incoming -> IO ()
receiveTerminalFrames exports peer queue = go [] (object []) Nothing
  where
    emit = atomically . writeTBQueue queue
    notice message=emit (Control (object ["type" .= ("notice"::T.Text),"message" .= (message::T.Text)]))
    go rows metadata download = peerReceive peer >>= \packet -> case packet of
      Nothing -> emit (Control (object ["type" .= ("closed"::T.Text)]))
      Just (JsonPacket value) -> do
        kind <- parseIO (withObject "control" (.: "type")) value :: IO T.Text
        case kind of
          "assets" -> go [] (object []) Nothing
          "canvas-chunk" -> do
            lengthBytes<-parseIO (withObject "canvas chunk" $ \o->do
              n<-o .: "length"
              unless (n>0 && n<=262144) (fail "Invalid image chunk length")
              pure (n::Int)) value
            following<-peerReceive peer
            case following of
              Just (BinaryPacket bytes) | BS.length bytes==lengthBytes->go rows metadata download
              _->ioError (userError "Expected image chunk bytes")
          "canvas-reset" -> go rows metadata download
          "canvas-resource" -> go rows metadata download
          "canvas-release" -> go rows metadata download
          "download" -> parseIO (withObject "download" $ \o->(,) <$> o .: "name" <*> o .:? "purpose") value >>= go rows metadata . Just
          "connection" -> emit (Control value) >> go rows metadata Nothing
          _ -> emit (Control value) >> go rows metadata download
      Just (BinaryPacket bytes) -> case download of
        Just (name,purpose) -> do
          case purpose :: Maybe T.Text of
            Just "file-export" -> do
              notice "Opening a file drag helper; drag the saved copy from its window, or close it to cancel."
              started<-startHelperFileExport exports name bytes (notice . either id (const "File drag helper closed."))
              either notice (const (pure ())) started
            Nothing -> do
              directory <- (</> "Downloads") <$> getHomeDirectory
              createDirectoryIfMissing True directory
              path <- bracket (openBinaryTempFile directory (T.unpack (sanitizeDownloadName name))) (hClose . snd) $ \(path,output) -> BS.hPut output bytes >> pure path
              notice ("Downloaded "<>T.pack path)
            Just _ -> notice "Unsupported download purpose."
          go rows metadata Nothing
        Nothing -> do
          (delta,newRows) <- decodeFrame rows bytes
          let merged=case (delta,metadata) of (Object new,Object old) -> Object (KM.union new old); _ -> delta
          frame <- either (ioError . userError) pure (parseRemoteFrame merged newRows)
          emit (Frame frame)
          go newRows merged Nothing
#else
runRemoteTerminal :: RemotePeer -> IO ()
runRemoteTerminal _ = ioError (userError "Remote support is not built; rebuild with -fremote.")
#endif
