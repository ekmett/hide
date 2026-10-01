{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
module THC.Edit.Web (runWeb
#ifdef WITH_WEB
  , WebInput(..), parseInput, applyInput, frameRows, framePacket, frameCandidates, frameDictionary, allowedOrigin, webDirty
#endif
  ) where
import THC.Edit.Model hiding (Paste)
#ifdef WITH_WEB
import THC.Edit.Protocol
#endif
#ifdef WITH_WEB
import Control.Concurrent.Async (withAsync, wait, race_)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (bracket, finally, catch, IOException)
import Control.Monad (forever, unless, when, void, foldM)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
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
import THC.Edit.Buffer (bufferBytes)
import System.FilePath (takeFileName)
import THC.Edit.Files (filePath)
import THC.Edit.Font
import THC.Edit.Frontend (modeSize)

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
          send (assetsPacket font scale)
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
              metadata=frameMetadata cwd current
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
