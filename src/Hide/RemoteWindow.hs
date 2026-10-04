{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
-- | Native SDL frontend for remotely owned editor sessions.
--
-- A receiver worker validates/decodes frames and downloads; SDL events and drawing
-- stay on the window thread. Draining pending messages allows one presentation of
-- the latest frame. Local pointer feedback and remote content updates have distinct
-- repaint rules, so drag/wheel rendering can await the resulting remote frame.
module Hide.RemoteWindow
  (runRemoteWindow, RemoteFrame(..), RemoteCell(..), parseRemoteFrame
  , nativeKeyInput, nativeEventInput, remoteMenuInput, pasteShortcut, sanitizeDownloadName
  , remoteInputAllowed, remoteCloseDetaches, remoteDetachShortcut, nativeRepaint) where
import Control.Monad (unless)
import Hide.Commands (commandIdentifier)
import Data.Aeson hiding (withArray)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Hide.Frontend
import Hide.Model (Command)
import Hide.Window (nativeCommands, nativeMenuEvent)
import Hide.Remote (RemotePeer)
import Hide.Unicode (clusterWidth, graphemes)
#if defined(WITH_WINDOW) && defined(WITH_REMOTE)
import Control.Concurrent.Async (withAsync, poll)
import Control.Concurrent.STM hiding (check)
import Control.Exception (bracket, bracket_, throwIO, IOException, catch, finally)
import Control.Monad (forM_, forever, when, foldM)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Foreign (alloca, allocaArray, peek, peekArray, withArray)
import Foreign.C
import Data.IORef
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf (printf)
import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import System.Info (os)
import System.Timeout (timeout)
import System.IO (withBinaryFile, IOMode(ReadMode), hFileSize, openBinaryTempFile, hClose, hPutStrLn, stderr)
import Hide.Font
import Hide.Model (Command(Paste))
import Hide.Protocol (WirePacket(..), decodeFrame)
import Hide.Links (openResource)
import Hide.Remote (peerSendBatch, peerReceive)
import Hide.Window hiding (nativeCommands, nativeMenuEvent)
#endif

-- | A positioned grapheme with explicit cell width and foreground/background RGB.
data RemoteCell = RemoteCell Int Int Int Int T.Text Int deriving (Eq,Show)
data RemoteFrame = RemoteFrame
  { remoteSize :: (Int,Int), remoteMode :: Maybe Int, remoteTitle :: T.Text
  , remoteCursor :: Maybe (Int,Int), remoteBlink :: Bool, remoteCRT :: Bool
  , remotePixelated :: Bool, remoteTerminal :: Bool, remoteWordStar :: Bool
  , remoteMenus :: [Bool], remoteCells :: [RemoteCell]
  } deriving (Eq,Show)

-- | Validate dimensions, ordered nonoverlapping spans, colors, cursor and widths
-- before any remote cells reach native drawing.
parseRemoteFrame :: Value -> [Value] -> Either String RemoteFrame
parseRemoteFrame metadata rows = parseEither (withObject "frame metadata" $ \o -> do
  size@(cols,lines') <- o .: "size"
  unless (cols>=40 && cols<=512 && lines'>=12 && lines'<=256 && length rows==lines') (fail "Invalid frame dimensions")
  mode <- o .:? "mode"
  unless (maybe True (`elem` [3,259]) mode) (fail "Invalid screen mode")
  title <- o .:? "title" .!= "Haskell"
  unless (T.length title<=8192 && not (T.any (=='\0') title)) (fail "Invalid title")
  cursor <- o .:? "cursor"
  unless (maybe True (\(x,y) -> x>=0 && x<cols && y>=0 && y<lines') cursor) (fail "Invalid cursor")
  blink <- o .:? "blink" .!= True
  crt <- o .:? "crt" .!= False
  pixelated <- o .:? "pixelated" .!= False
  terminal <- o .:? "terminal" .!= False
  wordstar <- o .:? "wordstar" .!= False
  supported <- o .:? "menuCommands" .!= [] :: Parser [T.Text]
  states <- o .:? "menuState" .!= [] :: Parser [(T.Text,Bool)]
  unless (length supported<=256 && length states<=256 && all ((<=256).T.length) (supported++map fst states)) (fail "Invalid menu state")
  let enabled=[maybe False (\name -> name `elem` supported && lookup name states==Just True) (commandIdentifier cmd) | cmd<-menuActions]
  cells <- concat <$> sequence [parseRow cols y row | (y,row) <- zip [0..] rows]
  pure (RemoteFrame size mode title cursor blink crt pixelated terminal wordstar enabled cells)) metadata
  where
    parseRow cols y value = do
      spans <- parseJSON value :: Parser [(Int,Int,Int,[Value])]
      unless (length spans<=cols+1) (fail "Too many spans")
      snd <$> foldRow cols y (0,[]) spans
    foldRow _ _ acc [] = pure acc
    foldRow cols y (previous,acc) ((x,fg,bg,runs):rest) = do
      unless (x>=previous && x<=cols && all (\c -> c>=0 && c<=0xffffff) [fg,bg] && length runs<=cols+1) (fail "Invalid span")
      clusters <- concat <$> traverse parseRun runs
      let width = sum (map snd clusters)
      unless (width<=cols-x && length clusters<=cols*4) (fail "Span exceeds row")
      let positions = scanl (+) x (map snd clusters)
          cells = [RemoteCell at y fg bg text w | (at,(text,w)) <- zip positions clusters, w>0]
      foldRow cols y (x+width,acc++cells) rest
    parseRun (String text) = do
      unless (T.length text<=512 && T.all (\c -> c>=' ' && c/='\DEL' && clusterWidth (T.singleton c)==1) text) (fail "Invalid character run")
      pure [(T.singleton c,1) | c<-T.unpack text]
    parseRun value = do
      (text,w) <- parseJSON value :: Parser (T.Text,Int)
      unless (not (T.null text) && T.length text<=4096 && not (T.any (\c -> c<' ' || c=='\DEL') text) && graphemes text==[text] && w>=0 && w<=2 && clusterWidth text==w) (fail "Invalid grapheme")
      pure [(text,w)]

modifierNames :: Int -> [T.Text]
modifierNames mask = ["shift" | mask .&. 1/=0] ++ ["ctrl" | mask .&. 10/=0] ++ ["alt" | mask .&. 4/=0]
nativeKeyInput :: Int -> Int -> Maybe Value
nativeKeyInput key mask = do
  V.EvKey k decodedMods <- decodeKey key mask
  name <- case k of
    V.KChar '\t' -> Just "Tab"
    V.KChar c -> Just (T.singleton c)
    V.KFun n -> Just ("F"<>T.pack (show n))
    _ -> lookup k [(V.KUp,"ArrowUp"),(V.KDown,"ArrowDown"),(V.KLeft,"ArrowLeft"),(V.KRight,"ArrowRight"),
      (V.KHome,"Home"),(V.KEnd,"End"),(V.KPageUp,"PageUp"),(V.KPageDown,"PageDown"),
      (V.KBackTab,"Tab"),(V.KEnter,"Enter"),(V.KEsc,"Escape"),(V.KBS,"Backspace"),(V.KDel,"Delete"),(V.KIns,"Insert")]
  pure (object ["type" .= ("key"::T.Text),"key" .= name,"mods" .= [label | (modifier,label)<-[(V.MShift,"shift"::T.Text),(V.MCtrl,"ctrl"),(V.MAlt,"alt")],modifier `elem` decodedMods]])
pasteShortcut :: Bool -> Bool -> Int -> Int -> Bool
pasteShortcut terminal wordstar key mask = key==fromEnum 'v' && mask .&. 10/=0
  && (not wordstar || mask .&. 8/=0 || terminal && mask .&. 1/=0) && not (terminal && mask .&. 15==2)
nativeEventInput :: [Int] -> Maybe Value
nativeEventInput event = case event of
  1:key:mods:_ | not (remoteDetachShortcut event) -> nativeKeyInput key mods
  3:x:y:clicks:mods:button:_ -> mouse (if clicks==0 then "move" else "down") x y button clicks mods
  4:x:y:_ -> mouse "up" x y 1 1 0
  5:w:h:_ -> Just (object ["type" .= ("resize"::T.Text),"width" .= max 40 (min 512 w),"height" .= max 12 (min 256 h)])
  6:_ -> Just (object ["type" .= ("command"::T.Text),"command" .= ("hide.app.quit"::T.Text)])
  7:_ -> Just (object ["type" .= ("blur"::T.Text)])
  9:x:y:direction:mods:_ -> case mouse (if direction>0 then "wheel-up" else "wheel-down") x y 1 1 mods of
    Just (Object fields) -> Just (Object (KM.insert "steps" (toJSON (max 1 (min 256 (abs direction)))) fields))
    result -> result
  12:x:y:_ -> mouse "move" x y 1 0 0
  13:mods:_ -> Just (object ["type" .= ("modifiers"::T.Text),"mods" .= modifierNames mods])
  _ -> Nothing
  where
    mouse :: T.Text -> Int -> Int -> Int -> Int -> Int -> Maybe Value
    mouse action x y button clicks mods = Just (object ["type" .= ("mouse"::T.Text),"action" .= (action::T.Text),
      "x" .= max (-1) (min 511 x),"y" .= max (-1) (min 255 y),"button" .= (if button==3 then 2 else 0::Int),
      "clicks" .= max 0 (min 3 clicks),"mods" .= modifierNames mods])
menuActions :: [Command]
menuActions = nativeCommands

-- | Resolve a local menu slot against the server's named command state.
remoteMenuInput :: RemoteFrame -> Int -> Maybe Value
remoteMenuInput frame index
  | index>=0, True:_<-drop index (remoteMenus frame),
    Just name:_<-drop index (map commandIdentifier menuActions) =
      Just (object ["type" .= ("menu"::T.Text),"command" .= name])
  | otherwise = Nothing

-- Drag/wheel updates are painted when their resulting frame arrives. Painting
-- the previous frame first spends an extra vblank on obsolete selection/layout.
-- Press/release still repaint locally to hide/restore the cell pointer.
nativeRepaint :: [Int] -> Bool
nativeRepaint event = case event of
  3:_:_:clicks:_ -> clicks>0
  n:_ -> n `elem` [4,5,7,8,12]
  _ -> False

remoteDetachShortcut :: [Int] -> Bool
remoteDetachShortcut event = case event of
  1:key:mods:_ -> key==fromEnum ']' && mods .&. 15==2
  _ -> False

-- | Gate disconnected input while retaining local zoom controls.
remoteInputAllowed :: Bool -> [Int] -> Bool
remoteInputAllowed connected event = connected || case event of
  1:key:mods:_ -> maybe False (const True) (zoomDirection key mods)
  _ -> False
-- | Detach locally on a disconnected close, without queuing a Quit that
-- could unexpectedly execute after reconnection.
remoteCloseDetaches :: Bool -> [Int] -> Bool
remoteCloseDetaches connected event = not connected && case event of 6:_ -> True; _ -> False

-- | Strip path components and unsuitable characters, bound UTF-8 length and
-- supply a fallback name. File creation still requires exclusive temporary output.
sanitizeDownloadName :: T.Text -> T.Text
sanitizeDownloadName input = case limit (T.map clean (last (T.splitOn "/" (T.replace "\\" "/" input)))) of
  "" -> "download"; "." -> "download"; ".." -> "download"; name -> name
  where limit = TE.decodeUtf8With (\_ _ -> Nothing) . BS.take 180 . TE.encodeUtf8
        clean c | c<' ' || c=='\DEL' || c==':' = '_'
                | otherwise = c

#if defined(WITH_WINDOW) && defined(WITH_REMOTE)
data Incoming = Frame RemoteFrame | Assets (M.Map T.Text Glyph) | Control Value
-- Compression and file transfers stay off the SDL thread.
receiveFrames :: RemotePeer -> TBQueue Incoming -> IO ()
receiveFrames peer queue = go [] (object []) Nothing
  where
    emit item = atomically (writeTBQueue queue item) >> c_wake
    go rows metadata download = peerReceive peer >>= \packet -> case packet of
      Nothing -> emit (Control (object ["type" .= ("closed"::T.Text)]))
      Just (JsonPacket value) -> do
        kind <- parseIO (withObject "control" (.: "type")) value :: IO T.Text
        case kind of
          "assets" -> do
            atlas <- parseIO (withObject "assets" $ \o -> do
              glyphs <- o .: "glyphs" :: Parser [(T.Text,Int,[Int])]
              unless (length glyphs<=65536) (fail "Oversized glyph atlas")
              forM_ glyphs $ \(text,w,bits) -> unless (T.length text==1 && w `elem` [8,16] && length bits==16 && all (\n -> n>=0 && n<=65535) bits) (fail "Invalid glyph")
              pure (M.fromList [(text,Glyph w (map fromIntegral bits)) | (text,w,bits)<-glyphs])) value
            supported <- parseIO (withObject "assets" (\o -> o .:? "menuCommands" .!= [])) value :: IO [T.Text]
            unless (length supported<=256 && all ((<=256).T.length) supported) (ioError (userError "Invalid menu commands"))
            emit (Assets atlas)
            go [] (object ["menuCommands" .= supported]) Nothing
          "download" -> do
            name <- parseIO (withObject "download" (.: "name")) value
            go rows metadata (Just name)
          "connection" -> emit (Control value) >> go rows metadata Nothing
          _ -> emit (Control value) >> go rows metadata download
      Just (BinaryPacket bytes) -> case download of
        Just name -> do
          saveDownload name bytes `catch` \(e::IOException) -> hPutStrLn stderr ("Download failed: "++show e)
          go rows metadata Nothing
        Nothing -> do
          (delta,newRows) <- decodeFrame rows bytes
          let merged = case (delta,metadata) of (Object new,Object old) -> Object (KM.union new old); _ -> delta
          frame <- either (ioError . userError) pure (parseRemoteFrame merged newRows)
          emit (Frame frame)
          go newRows merged Nothing
    saveDownload name bytes = do
      directory <- (</> "Downloads") <$> getHomeDirectory
      createDirectoryIfMissing True directory
      path <- bracket (openBinaryTempFile directory (T.unpack (sanitizeDownloadName name))) (hClose . snd) $ \(path,handle) -> BS.hPut handle bytes >> pure path
      hPutStrLn stderr ("Downloaded "++path)
parseIO :: (Value -> Parser a) -> Value -> IO a
parseIO parser = either (ioError . userError) pure . parseEither parser

drawRemote :: Font -> M.Map T.Text Glyph -> RemoteFrame -> IO ()
drawRemote font atlas frame = do
  c_cursor_blink (flag (remoteBlink frame))
  c_crt_filter (flag (remoteCRT frame))
  c_pixelate_unicode (flag (remotePixelated frame))
  check "Allocate remote frame" c_begin
  forM_ (remoteCells frame) $ \(RemoteCell x y fg bg text w) -> do
    let bitmap = case M.lookup text atlas of
          Just tile -> Just tile
          Nothing -> case T.unpack text of [c] | bitmapGlyph font c -> Just (glyph font c); _ -> Nothing
    case bitmap of
      Just (Glyph width bits) -> withArray bits $ \p -> c_glyph (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral width) p (fromIntegral fg) (fromIntegral bg)
      Nothing -> utf8 text $ \p -> check "Draw remote Unicode" (c_unicode (fromIntegral x) (fromIntegral y) (fromIntegral w) p (fromIntegral fg) (fromIntegral bg))
  forM_ (remoteCursor frame) $ \(x,y) -> c_cursor (fromIntegral x) (fromIntegral y)
  check "Present remote frame" c_present
  where flag value = if value then 1 else 0

runRemoteWindow :: Backend -> Double -> (Int,Int) -> Int -> String -> RemotePeer -> IO ()
runRemoteWindow backend scale (cols,rows) mode host peer = do
  font <- loadFont
  drawTimes <- newIORef ([]::[Double])
  titleTiming <- newIORef (0::Double,""::T.Text)
  incoming <- newTBQueueIO 8
  outgoing <- newTBQueueIO 256
  queuedBytes <- newTVarIO (0::Int)
  let driver = case backend of Metal -> "metal"; Vulkan -> "vulkan"; _ -> if os=="darwin" then "metal" else "vulkan"
      send packets = do
        let size = sum [case packet of JsonPacket value -> fromIntegral (BL.length (encode value)); BinaryPacket bytes -> BS.length bytes | packet<-packets]
        accepted <- atomically $ do
          full <- isFullTBQueue outgoing
          used <- readTVar queuedBytes
          if full || used+size>33554432 then pure False
          else writeTBQueue outgoing (size,packets) >> writeTVar queuedBytes (used+size) >> pure True
        unless accepted (hPutStrLn stderr "Remote input queue full; input was not sent.")
      sendJSON value = send [JsonPacket value]
      sendEvent = maybe (pure ()) sendJSON . nativeEventInput
      paste = do
        bytes <- c_clipboard >>= BS.packCString
        case TE.decodeUtf8' bytes of
          Right text | T.length text<=1048576 -> sendJSON (object ["type" .= ("paste"::T.Text),"text" .= text])
          _ -> hPutStrLn stderr "Clipboard text is invalid or exceeds 1 MiB."
      resize = alloca $ \wp -> alloca $ \hp -> do
        c_size wp hp
        w <- fromIntegral <$> peek wp; h <- fromIntegral <$> peek hp
        sendEvent [5,w,h]
      dispatch connected frame event = case event of
        1:key:mods:_ | not (maybe False remoteTerminal frame && mods .&. 15==2), Just direction <- zoomDirection key mods -> check "Change window scale" (c_scale (fromIntegral direction)) >> when connected resize
                    | pasteShortcut (maybe False remoteTerminal frame) (maybe False remoteWordStar frame) key mods -> paste
        2:_ -> do
          bytes <- c_text >>= BS.packCString
          case TE.decodeUtf8' bytes of
            Right text -> forM_ (T.unpack text) $ \c -> sendEvent [1,fromEnum c,0]
            Left _ -> pure ()
        11:i:_ -> do
#ifdef darwin_HOST_OS
          generation<-fromIntegral <$> c_menu_generation
          case nativeMenuEvent generation event of
            Just command | Just value<-frame, Just packet<-remoteMenuInput value i ->
              if command==Paste then paste else sendJSON packet
            _ -> pure ()
#else
          pure ()
#endif
        14:_ | null host -> do
          bytes <- c_text >>= BS.packCString
          case TE.decodeUtf8' bytes of
            Right path -> sendJSON (object ["type" .= ("open"::T.Text),"path" .= path])
            Left _ -> pure ()
        14:_ -> do
          bytes <- c_text >>= BS.packCString
          case TE.decodeUtf8' bytes of
            Left _ -> pure ()
            Right path -> do
              let name = sanitizeDownloadName (T.pack (takeFileName (T.unpack path)))
              (withBinaryFile (T.unpack path) ReadMode $ \handle -> do
                size <- hFileSize handle
                if size>16777216 then hPutStrLn stderr "Dropped file exceeds 16 MiB."
                else do
                  payload <- BS.hGet handle 16777217
                  when (BS.length payload<=16777216) (send [JsonPacket (object ["type" .= ("upload"::T.Text),"name" .= name]),BinaryPacket payload]))
                `catch` \(e::IOException) -> hPutStrLn stderr ("Cannot upload dropped file: "++show e)
        _ -> when connected (sendEvent event)
      title connection frame = do
        (_,timing)<-readIORef titleTiming
        utf8 ((maybe "Haskell" remoteTitle frame)<>(if null host then "" else " — "<>T.pack host)<>connection<>timing) c_title
      updateTiming connection frame = do
        now<-((/1000000000).fromIntegral) <$> getMonotonicTimeNSec
        (previous,_)<-readIORef titleTiming
        when (now-previous>=1) $ do
          samples<-readIORef drawTimes
          unless (null samples) $ do
            let timing=T.pack (printf " | %.1f ms/frame" (sum samples/fromIntegral (length samples)))
            writeIORef titleTiming (now,timing)
            title connection frame
      controls (frame,atlas,connection,changed,closed) item = case item of
        Assets glyphs -> pure (frame,glyphs,connection,True,closed)
        Frame value -> do
          when (maybe (Just mode) remoteMode frame /= remoteMode value) $ do
            let (w,h) = remoteSize value
            check "Change remote screen mode" (c_mode (fromIntegral (modeHeight (maybe mode id (remoteMode value)))) (fromIntegral w) (fromIntegral h))
            resize
#ifdef darwin_HOST_OS
          forM_ (zip [0..] nativeCommands) $ \(i,_) ->
            c_menu_enabled (fromIntegral (i::Int)) (if maybe False id (atMay (remoteMenus value) i) then 1 else 0)
#endif
          pure (Just value,atlas,connection,True,closed)
        Control value -> do
          kind <- parseIO (withObject "control" (.: "type")) value :: IO T.Text
          case kind of
            "closed" -> pure (frame,atlas,connection,changed,True)
            "copy" -> do
              text <- parseIO (withObject "copy" (.: "text")) value
              utf8 text c_set_clipboard
              pure (frame,atlas,connection,changed,closed)
            "open-resource" -> do
              result<-openResource value
              pure (frame,atlas,either id (const connection) result,changed,closed)
            "paste-request" -> paste >> pure (frame,atlas,connection,changed,closed)
            "connection" -> do
              connected <- parseIO (withObject "connection" (.: "connected")) value
              when connected resize
              pure (frame,atlas,if connected then "" else " (reconnecting)",True,closed)
            _ -> pure (frame,atlas,connection,changed,closed)
      loop receiver sender frame atlas connection previousTheme repaint = do
        forM_ [receiver,sender] $ \worker -> do
          result <- poll worker
          case result of Just (Left e) -> throwIO e; _ -> pure ()
        messages <- atomically (drain incoming)
        (current,glyphs,status,changed,closed) <- foldM controls (frame,atlas,connection,repaint,False) messages
        unless closed $ do
          let connected = T.null status
          when changed $ do
            title status current
            forM_ current $ \value -> do
              start<-getMonotonicTimeNSec
              drawRemote font glyphs value
              end<-getMonotonicTimeNSec
              modifyIORef' drawTimes (take 60 . (fromIntegral (end-start)/1000000:))
#ifdef darwin_HOST_OS
            unless connected $ forM_ (zip [0::Int ..] nativeCommands) $ \(i,_) -> c_menu_enabled (fromIntegral i) 0
#endif
          updateTiming status current
          dark <- (/=0) <$> c_system_dark
          when (previousTheme/=Just dark && (connected || previousTheme==Nothing)) (sendJSON (object ["type" .= ("theme"::T.Text),"dark" .= dark]))
          event <- allocaArray 6 $ \p -> check "Read remote window event" (c_wait p) >> map fromIntegral <$> peekArray 6 p
          unless (remoteDetachShortcut event || remoteCloseDetaches connected event) $ do
            when (remoteInputAllowed connected event) (dispatch connected current event)
            loop receiver sender current glyphs status (if connected || previousTheme==Nothing then Just dark else previousTheme) (nativeRepaint event)
  bracket_ (pure ()) c_close $ do
#ifdef darwin_HOST_OS
    c_menu_prepare
#endif
    withCString driver $ \name -> check "Open remote window" (c_open name (realToFrac scale) (fromIntegral cols) (fromIntegral rows) (fromIntegral (modeHeight mode)))
    nativeMenus
    sendJSON (object ["type" .= ("frontend"::T.Text),"mode" .= mode,"mac" .= (os=="darwin")])
#ifdef darwin_HOST_OS
    forM_ (zip [0::Int ..] nativeCommands) $ \(i,_) -> c_menu_enabled (fromIntegral i) 0
#endif
    title " (connecting)" Nothing
    resize
    withAsync (receiveFrames peer incoming) $ \receiver ->
      withAsync (forever $ do
        (size,packets) <- atomically (readTBQueue outgoing)
        peerSendBatch peer packets
        atomically (modifyTVar' queuedBytes (subtract size))) $ \sender ->
        loop receiver sender Nothing M.empty " (connecting)" Nothing True `finally` do
          sent <- timeout 2000000 (atomically (readTVar queuedBytes >>= \bytes -> when (bytes/=0) retry))
          when (sent==Nothing) (hPutStrLn stderr "Some window input could not be handed to the session before detaching.")
  where
    drain queue = do
      item <- tryReadTBQueue queue
      case item of Nothing -> pure []; Just value -> (value:) <$> drain queue
#ifdef darwin_HOST_OS
    atMay xs i = case drop i xs of x:_ -> Just x; _ -> Nothing
#endif
#else
runRemoteWindow :: Backend -> Double -> (Int,Int) -> Int -> String -> RemotePeer -> IO ()
runRemoteWindow _ _ _ _ _ _ = ioError (userError "Graphical support is not built; rebuild with -fwindow -fremote.")
#endif
