{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
module THC.Edit.Web (runWeb
#ifdef WITH_WEB
  , WebInput(..), parseInput, applyInput, frameRows, framePacket, frameCandidates, frameDictionary, allowedOrigin, webDirty
#endif
  ) where
import THC.Edit.Model hiding (Paste)
#ifdef WITH_WEB
import qualified Codec.Compression.Zlib.Raw as Z
import Control.Concurrent.Async (withAsync, wait, race_)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (bracket, finally, catch, IOException)
import Control.Monad (forever, unless, when, void, foldM)
import Data.Aeson
import Data.Aeson.Types (Parser, Pair, parseEither)
import Data.Bits ((.|.), shiftL)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.IORef
import Data.List (groupBy)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Graphics.Vty as V
import Graphics.Vty.Span (SpanOp(..))
import qualified Network.Socket as N
import Network.HTTP.Types
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Numeric (showHex)
import Paths_thc_edit (getDataFileName)
import System.Info (os)
import System.Environment (lookupEnv)
import System.Directory (getCurrentDirectory)
import System.IO (withBinaryFile, IOMode(ReadMode), hPutStrLn, stderr)
import System.Process (callProcess)
import System.Timeout (timeout)
import THC.Edit.Buffer (dirty, contents, bufferBytes, newBuffer, newByteBuffer)
import System.FilePath (takeFileName)
import THC.Edit.Files (filePath)
import THC.Edit.Font
import THC.Edit.Frontend (modeSize)
import THC.Edit.Render (renderDesktop)
import THC.Edit.Unicode (displayOpsForPic, graphemes, clusterWidth)

data WebInput = Key T.Text [V.Modifier] | Paste T.Text | Mouse T.Text Int Int Int Int [V.Modifier]
              | SystemTheme Bool | BrowserCommand Command | UploadFile T.Text BS.ByteString | Resize Int Int | Blur | Modifiers [V.Modifier] deriving (Eq,Show)

parseInput :: Value -> Parser WebInput
parseInput = withObject "browser event" $ \o -> do
  kind <- o .: "type" :: Parser T.Text
  let mods = do
        values <- o .:? "mods" .!= [] :: Parser [T.Text]
        traverse (\v -> case v of "shift" -> pure V.MShift; "ctrl" -> pure V.MCtrl; "alt" -> pure V.MAlt; _ -> fail "Unknown modifier") values
  case kind of
    "theme" -> SystemTheme <$> o .: "dark"
    "command" -> do
      name <- o .: "command"
      maybe (fail "Unknown browser command") (pure . BrowserCommand) (lookup (name::T.Text)
        [("copy",Copy),("cut",Cut),("selectAll",SelectAll),("undo",Undo),("redo",Redo),("find",Find),("findNext",FindNext),("findPrevious",FindPrevious)])
    "upload" -> do
      name <- o .: "name"
      unless (not (T.null name) && T.length name<=255 && T.all (\c -> c>=' ' && c/='/' && c/='\\') name && name/="." && name/="..") (fail "Invalid filename")
      pure (UploadFile name BS.empty)
    "key" -> do
      key <- o .: "key"
      unless (T.length key<=24) (fail "Invalid key")
      Key key <$> mods
    "paste" -> do
      text <- o .: "text"
      unless (T.length text<=1048576) (fail "Paste too large")
      pure (Paste text)
    "resize" -> do
      w <- o .: "width"; h <- o .: "height"
      unless (w>=40 && w<=512 && h>=12 && h<=256) (fail "Invalid dimensions")
      pure (Resize w h)
    "mouse" -> do
      action <- o .: "action"; x <- o .: "x"; y <- o .: "y"
      button <- o .:? "button" .!= 0; clicks <- o .:? "clicks" .!= 1
      unless (action `elem` ["down","up","move","wheel-up","wheel-down"] && x>=(-1) && x<512 && y>=(-1) && y<256 && button>=0 && button<=2 && clicks>=0 && clicks<=3) (fail "Invalid mouse event")
      Mouse action x y button clicks <$> mods
    "blur" -> pure Blur
    "modifiers" -> Modifiers <$> mods
    _ -> fail "Unknown event"

applyInput :: WebInput -> Desktop -> (Desktop,[Effect])
applyInput input d = case input of
  SystemTheme value -> (d {systemDark=value},[])
  BrowserCommand cmd | dialog d/=Nothing -> (d,[WriteBrowserClipboard "" | cmd `elem` [Copy,Cut]])
                     | otherwise -> runCommand cmd d
  UploadFile name bytes ->
    let b=case TE.decodeUtf8' bytes of
          Right text | not (BS.elem 0 bytes) -> newBuffer text
          _ -> newByteBuffer bytes
        opened=addDocument Nothing b d
    in (opened {buffers=M.adjust (\doc -> restyle doc {documentSuggestedName=Just (T.unpack name)}) (nextId d) (buffers opened),status="Dropped file opened; Download exports changes."},[])
  Key name mods -> maybe (d,[]) (\key -> handleEvent (V.EvKey key mods) d) (keyName name)
  Paste text -> handleEvent (V.EvPaste (TE.encodeUtf8 text)) d
  Resize w h -> handleEvent (V.EvResize w h) d
  Blur -> hoverAt (-1) (-1) d {drag=Nothing,dragOriginal=Nothing,prefix=Nothing,buttonPressed=Nothing,heldModifiers=[]}
  Modifiers mods -> (d {heldModifiers=mods},[])
  Mouse action x y button clicks mods -> case action of
    "move" | dialog d/=Nothing || drag d==Nothing -> hoverAt x y d
           | otherwise -> handleEvent (V.EvMouseDown x y V.BLeft mods) d
    "down" | button==0 && clicks>=2 -> handleDoubleClick x y d
           | otherwise -> handleEvent (V.EvMouseDown x y (if button==2 then V.BRight else V.BLeft) mods) d
    "up" -> handleEvent (V.EvMouseUp x y (Just (if button==2 then V.BRight else V.BLeft))) d
    "wheel-up" -> handleEvent (V.EvMouseDown x y V.BScrollUp mods) d
    "wheel-down" -> handleEvent (V.EvMouseDown x y V.BScrollDown mods) d
    _ -> (d,[])
  where
    keyName name = case T.unpack name of
      [ch] -> Just (V.KChar (if V.MCtrl `elem` keyMods || V.MAlt `elem` keyMods then toLower ch else ch))
      _ -> lookup name ([ ("F"<>T.pack (show n),V.KFun n) | n<-[1..24]] ++
        [("ArrowUp",V.KUp),("ArrowDown",V.KDown),("ArrowLeft",V.KLeft),("ArrowRight",V.KRight),
         ("Home",V.KHome),("End",V.KEnd),("PageUp",V.KPageUp),("PageDown",V.KPageDown),
         ("Enter",V.KEnter),("Escape",V.KEsc),("Backspace",V.KBS),("Delete",V.KDel),
         ("Insert",V.KIns),("Tab",if V.MShift `elem` keyMods then V.KBackTab else V.KChar '\t')])
    keyMods=case input of Key _ ms -> ms; _ -> []

-- Complete row-major snapshots feed DEFLATE in a stable order, refreshing its
-- history with unchanged cells as well as edits. No application-level move search.
frameRows :: Desktop -> [Value]
frameRows d = map (toJSON . spans 0 . toList) (toList (displayOpsForPic (renderDesktop d) (screenSize d)))
  where
    spans _ [] = []
    spans x (op:rest) = case op of
      TextSpan{textSpanAttr=a,textSpanText=t} ->
        let clusters=[(cluster,clusterWidth cluster) | cluster<-graphemes (TL.toStrict t)]
        in toJSON (x,rgb (V.attrForeColor a),rgb (V.attrBackColor a),packClusters clusters):spans (x+sum (map snd clusters)) rest
      Skip n -> toJSON (x,0xffffff::Int,0x0000aa::Int,[String (T.replicate n " ")]):spans (x+n) rest
      RowEnd n -> toJSON (x,0xffffff::Int,0x0000aa::Int,[String (T.replicate n " ")]):spans (x+n) rest
    packClusters = concatMap pack . groupBy (\a b -> ordinary a==ordinary b)
    ordinary (cluster,w)=w==1 && T.length cluster==1
    pack []=[]
    pack xs@(first:_)
      | ordinary first = [String (T.concat (map fst xs))]
      | otherwise = map toJSON xs
    rgb :: V.MaybeDefault V.Color -> Int
    rgb (V.SetTo (V.RGBColor r g b)) = fromIntegral r `shiftL` 16 .|. fromIntegral g `shiftL` 8 .|. fromIntegral b
    rgb (V.SetTo (V.ISOColor n)) = [0,0xaa0000,0x00aa00,0xaa5500,0x0000aa,0xaa00aa,0x00aaaa,0xaaaaaa,0x555555,0xff5555,0x55ff55,0xffff55,0x5555ff,0xff55ff,0x55ffff,0xffffff] !! (fromIntegral n `mod` 16)
    rgb _ = 0

-- Both encodings use the reconstructed previous screen, never the previous
-- packet, as their dictionary. Screen rows contain only arrays, bounded integer
-- coordinates/colors, and Unicode strings: JSON.stringify reproduces this encoding.
frameDictionary :: [Value] -> BS.ByteString
frameDictionary rows = let bytes=BL.toStrict (encode rows)
                       in BS.drop (max 0 (BS.length bytes-32768)) bytes

frameCandidates :: Bool -> [Value] -> [Value] -> [Pair] -> [BL.ByteString]
frameCandidates reset old rows metadata =
  [packet (if reset then 0 else 1) allRows] ++ [packet 2 changed | not reset]
  where
    allRows=zip [0::Int ..] rows
    changed=[(y,r) | ((y,r),previous)<-zip allRows (map Just old++repeat Nothing), Just r/=previous]
    params=Z.defaultCompressParams {Z.compressLevel=Z.compressionLevel 8,
      Z.compressDictionary=if reset then Nothing else Just (frameDictionary old)}
    packet tag selected=BL.cons tag (Z.compressWith params (encode (object
      (["type" .= ("frame"::T.Text),"reset" .= reset,
        "rows" .= selected]++metadata))))

framePacket :: Bool -> [Value] -> [Value] -> [Pair] -> BL.ByteString
framePacket reset old rows metadata = foldl1 smaller (frameCandidates reset old rows metadata)
  where smaller a b=if BL.length b<BL.length a then b else a

webDirty :: Desktop -> Bool
webDirty d = any (dirty . documentBuffer) (M.elems (buffers d)) || not (T.null (contents (composerBuffer d)))

allowedOrigin :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString -> Bool
allowedOrigin expectedHost host origin = host==expectedHost && origin==Just ("http://"<>expectedHost)

runWeb :: Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> Desktop -> IO ()
runWeb scale effects tick initial = do
  font <- loadFont
  html <- getDataFileName "assets/web/index.html" >>= BL.readFile
  script <- getDataFileName "assets/web/editor.js" >>= BL.readFile
  token <- withBinaryFile "/dev/urandom" ReadMode $ \h -> do
    bytes<-BS.hGet h 24
    unless (BS.length bytes==24) (ioError (userError "Cannot create browser session token"))
    pure (B8.pack (concatMap (\n -> let s=showHex n "" in replicate (2-length s) '0'++s) (BS.unpack bytes)))
  state <- newIORef initial {browserFrontend=True}
  slot <- newMVar ()
  done <- newEmptyMVar
  bracket (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $ \socket -> do
    N.bind socket (N.SockAddrInet 0 (N.tupleToHostAddress (127,0,0,1)))
    N.listen socket 8
    N.SockAddrInet port _ <- N.getSocketName socket
    let host=B8.pack ("127.0.0.1:"++show port)
        prefix="/"<>token<>"/"
        url="http://"<>host<>prefix
        headers=[("Cache-Control","no-store"),("X-Content-Type-Options","nosniff"),("Referrer-Policy","no-referrer"),("Content-Security-Policy","default-src 'self'; script-src 'self'; style-src 'unsafe-inline'; connect-src 'self'; frame-ancestors 'none'")]
        static req respond
          | W.requestHeaderHost req/=Just host = respond (W.responseLBS status403 headers "Forbidden")
          | W.requestMethod req/="GET" = respond (W.responseLBS status405 headers "GET required")
          | W.rawPathInfo req==prefix = respond (W.responseLBS status200 (("Content-Type","text/html; charset=utf-8"):headers) html)
          | W.rawPathInfo req==prefix<>"editor.js" = respond (W.responseLBS status200 (("Content-Type","text/javascript; charset=utf-8"):headers) script)
          | otherwise = respond (W.responseLBS status404 headers "Not found")
        websocket pending = do
          let req=WS.pendingRequest pending
          if WS.requestPath req/=prefix<>"socket" || not (allowedOrigin host (maybe "" id (lookup "Host" (WS.requestHeaders req))) (lookup "Origin" (WS.requestHeaders req)))
            then WS.rejectRequest pending "Forbidden"
            else do
              available<-tryTakeMVar slot
              case available of
                Nothing -> WS.rejectRequest pending "Editor already connected"
                Just () -> finally (session pending `catch` \(err :: WS.ConnectionException) -> hPutStrLn stderr ("Browser connection: "++show err)) (putMVar slot ())
        session pending = do
          conn<-WS.acceptRequest pending
          queue<-newTBQueueIO 128
          disconnected<-newEmptyTMVarIO
          let send=WS.sendTextData conn . encode
              receive=forever $ do
                bytes<-WS.receiveData conn :: IO BL.ByteString
                case eitherDecode bytes >>= parseEither (\value -> (,) <$> withObject "event sequence" (\o -> o .:? "seq" .!= (0::Int)) value <*> parseInput value) of
                  Left err -> hPutStrLn stderr ("Browser input: "++err) >> WS.sendCloseCode conn 1003 ("Invalid input" :: T.Text) >> ioError (userError "Invalid browser input")
                  Right (serial,UploadFile name _) -> do
                    payload <- timeout 30000000 (WS.receiveDataMessage conn)
                    case payload of
                      Just (WS.Binary payloadBytes) | BL.length payloadBytes<=16777216 -> atomically (writeTBQueue queue (Just (serial,UploadFile name (BL.toStrict payloadBytes))))
                      _ -> WS.sendCloseCode conn 1003 ("Expected file bytes (maximum 16 MiB)"::T.Text) >> ioError (userError "Invalid file upload")
                  Right event -> atomically (writeTBQueue queue (Just event))
          send (object ["type" .= ("assets"::T.Text),"scale" .= scale,
            "glyphs" .= [(T.singleton c,glyphWidth g,glyphRows g) | (c,g)<-bitmapAtlas font]])
          WS.withPingThread conn 15 (pure ()) $ withAsync (finally receive (atomically (void (tryPutTMVar disconnected ())))) $ \_ ->
            readIORef state >>= loop conn send queue disconnected Nothing
        loop conn send queue disconnected previous d = do
          current<-tick d
          cwd<-getCurrentDirectory
          writeIORef state current
          let oldDesktop=fmap (\(old,_,_) -> old) previous
              oldRows=maybe [] (\(_,cached,_) -> cached) previous
              oldMetadata=maybe [] (\(_,_,meta) -> meta) previous
              rows=if oldDesktop==Just current then oldRows else frameRows current
              metadata=["title" .= applicationTitle cwd current,"size" .= screenSize current,"mode" .= videoMode current,
                "dirty" .= webDirty current,"cursor" .= cursor,"blink" .= blinkCursor current,
                "crt" .= crtFilter current,"pixelated" .= pixelateUnicode current,
                "selection" .= (if dialog current/=Nothing then "" else clipboard (fst (runCommand THC.Edit.Model.Copy current {browserFrontend=False}))),
                "terminal" .= (activeTerminal current/=Nothing && dialog current==Nothing && menu current==Nothing),"wordstar" .= wordStar current]
              cursor=case V.picCursor (renderDesktop current) of V.Cursor x y -> Just (x,y); _ -> Nothing
          when (oldDesktop/=Just current) $ do
            let
                reset=maybe True (\old -> screenSize old/=screenSize current || videoMode old/=videoMode current || pixelateUnicode old/=pixelateUnicode current) oldDesktop
            when (reset || rows/=oldRows || metadata/=oldMetadata) $
              WS.sendBinaryData conn (framePacket reset oldRows rows (if reset then metadata else filter (`notElem` oldMetadata) metadata))
          event<-timeout 50000 (atomically ((readTMVar disconnected >> pure Nothing) `orElse` readTBQueue queue))
          case event of
            Just Nothing -> pure ()
            _ -> do
              let (next,requests)=maybe (current,[]) (\(_,inputEvent) -> applyInput inputEvent current) (event >>= id)
              (exit,updated)<-foldM (effect conn send) (False,next) requests
              writeIORef state updated
              case event >>= id of
                Just (serial,_) -> send (object ["type" .= ("ack"::T.Text),"seq" .= serial,"dirty" .= webDirty updated])
                Nothing -> pure ()
              if exit then send (object ["type" .= ("closed"::T.Text)]) >> void (tryPutMVar done ())
                else loop conn send queue disconnected (Just (current,rows,metadata)) updated
        effect _ _ result@(True,_) _ = pure result
        effect _ send (_,d) ReadBrowserClipboard = send (object ["type" .= ("paste-request"::T.Text)]) >> pure (False,d)
        effect _ send (_,d) (WriteBrowserClipboard text) = send (object ["type" .= ("copy"::T.Text),"text" .= text]) >> pure (False,d)
        effect conn send (_,d) (DownloadDocument bid) = do
          case M.lookup bid (buffers d) of
            Nothing -> pure ()
            Just doc -> do
              let name=maybe (maybe "NONAME.HS" id (documentSuggestedName doc)) (takeFileName . filePath) (documentFile doc)
              _ <- send (object ["type" .= ("download"::T.Text),"name" .= name])
              WS.sendBinaryData conn (bufferBytes (documentBuffer doc))
          pure (False,d)
        effect _ _ (_,d) (SetScreenMode mode) = pure (False,(resizeScreenMode (modeSize mode) d) {videoMode=Just mode})
        effect _ _ (_,d) request = effects d [request]
        connectionOptions=WS.defaultConnectionOptions {WS.connectionCompressionOptions=WS.NoCompression,WS.connectionFramePayloadSizeLimit=WS.SizeLimit 16777216,WS.connectionMessageDataSizeLimit=WS.SizeLimit 16777216}
        app=websocketsOr connectionOptions websocket static
    hPutStrLn stderr ("Turbo Haskell browser: "++B8.unpack url)
    withAsync (Warp.runSettingsSocket Warp.defaultSettings socket app) $ \server -> do
      autoOpen <- (/=Just "0") <$> lookupEnv "THC_EDIT_WEB_OPEN"
      when autoOpen $ callProcess (if os=="darwin" then "open" else "xdg-open") [B8.unpack url]
        `catch` \(_ :: IOException) -> hPutStrLn stderr "Open the browser URL above to connect."
      race_ (wait server) (takeMVar done)
#else
runWeb :: Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> Desktop -> IO ()
runWeb _ _ _ _ = ioError (userError "Browser support is not built. Rebuild with cabal build -fweb")
#endif
