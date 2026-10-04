{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
-- | Vty frontend for validated remote cell frames.
--
-- Bounded sender/receiver queues keep transport and frame decoding outside input
-- handling. Disconnects retain the last picture with a notice and suppress remote
-- input; local detach remains available. Clipboard export uses OSC 52 and does
-- not read the terminal host clipboard.
module Hide.RemoteTerminal (runRemoteTerminal, terminalEventInput, remoteTerminalPicture, terminalClipboard) where

import Data.Aeson
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import qualified Data.IntMap.Strict as IM
import Hide.Links (openResource)
import Hide.Remote (RemotePeer)
import Hide.TextStyle
import Hide.RemoteWindow (RemoteFrame(..), RemoteCell(..), remoteBindingInput)
import Hide.Unicode (textImage)
#ifdef WITH_REMOTE
import Control.Concurrent.Async (withAsync, poll)
import Control.Concurrent.STM
import Control.Exception (bracket, finally, throwIO)
import Control.Monad (forever, forM_, unless, when)
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (isPrint)
import Graphics.Vty.CrossPlatform (mkVty)
import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (stdout, stderr, hPutStrLn, hFlush, openBinaryTempFile, hClose)
import System.Timeout (timeout)
import Hide.Protocol (WirePacket(..), decodeFrame)
import Hide.Remote (peerReceive, peerSend)
import Hide.RemoteWindow (parseRemoteFrame, sanitizeDownloadName)
import Hide.Unicode (updatePicture)
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

-- | Build a Vty picture from previously validated remote frame geometry.
remoteTerminalPicture :: RemoteFrame -> V.Picture
remoteTerminalPicture frame = (V.picForImage (V.vertCat [row (IM.findWithDefault [] y rows) | y<-[0..height-1]]))
  {V.picCursor=maybe V.NoCursor (uncurry V.Cursor) (remoteCursor frame)}
  where
    (width,height)=remoteSize frame
    rows=IM.fromListWith (++) [(y,[cell]) | cell@(RemoteCell _ y _ _ _)<-remoteCells frame]
    row = V.horizCat . spans 0 . reverse
    spaces n=V.charFill V.defAttr ' ' n 1
    spans at []=[spaces (width-at)]
    spans at (RemoteCell x _ paint text w:rest)=spaces (x-at):textImage (textStyleAttr paint) text:spans (x+w) rest

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
runRemoteTerminal peer = bracket (mkVty V.defaultConfig) (\vty -> V.shutdown vty `finally` cursorStyle Nothing) $ \vty -> do
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
        (_,height) <- V.displayBounds (V.outputIface vty)
        let picture=maybe (V.picForImage V.emptyImage) remoteTerminalPicture frame
            banner=V.translate 0 (max 0 (height-1)) (textImage (V.defAttr `V.withForeColor` V.white `V.withBackColor` V.blue) (T.filter isPrint message))
        updatePicture vty (if T.null message then picture else picture {V.picLayers=banner:V.picLayers picture,V.picCursor=V.NoCursor})
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
                forM_ clipboard $ \text -> send (object ["type" .= ("paste"::T.Text),"text" .= text])
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
  withAsync (receiveFrames peer incoming) $ \receiver ->
    withAsync (forever $ do
      (size,value) <- atomically (readTBQueue outgoing)
      peerSend peer (JsonPacket value)
      atomically (modifyTVar' queued (subtract size))) $ \sender ->
        loop receiver sender Nothing False Nothing "" `finally` drain
  where
    cursorStyle blinking = putStr (case blinking of Just True -> "\ESC[3 q"; Just False -> "\ESC[4 q"; Nothing -> "\ESC[0 q") >> hFlush stdout

parseIO :: (Value -> Parser a) -> Value -> IO a
parseIO parser = either (ioError . userError) pure . parseEither parser

receiveFrames :: RemotePeer -> TBQueue Incoming -> IO ()
receiveFrames peer queue = go [] (object []) Nothing
  where
    emit = atomically . writeTBQueue queue
    go rows metadata download = peerReceive peer >>= \packet -> case packet of
      Nothing -> emit (Control (object ["type" .= ("closed"::T.Text)]))
      Just (JsonPacket value) -> do
        kind <- parseIO (withObject "control" (.: "type")) value :: IO T.Text
        case kind of
          "assets" -> go [] (object []) Nothing
          "download" -> parseIO (withObject "download" (.: "name")) value >>= go rows metadata . Just
          "connection" -> emit (Control value) >> go rows metadata Nothing
          _ -> emit (Control value) >> go rows metadata download
      Just (BinaryPacket bytes) -> case download of
        Just name -> do
          directory <- (</> "Downloads") <$> getHomeDirectory
          createDirectoryIfMissing True directory
          path <- bracket (openBinaryTempFile directory (T.unpack (sanitizeDownloadName name))) (hClose . snd) $ \(path,output) -> BS.hPut output bytes >> pure path
          emit (Control (object ["type" .= ("notice"::T.Text),"message" .= ("Downloaded "<>T.pack path)]))
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
