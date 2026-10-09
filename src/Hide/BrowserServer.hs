{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | Loopback HTTP/WebSocket host shared by browser frontends.
--
-- An unpredictable URL path, exact Host/Origin checks and a single-viewer slot
-- restrict attachment. This is local capability-based access, not user-account
-- authentication. The server scopes listener and viewer ownership and applies
-- bounded WebSocket messages and restrictive asset response headers.
module Hide.BrowserServer (serveBrowser, allowedOrigin) where
import Control.Concurrent.Async (withAsync, wait, race_)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally, catch, IOException)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Network.Socket as N
import Network.HTTP.Types
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Hide.RemoteEndpoint (randomIdentity)
import Paths_hide (getDataFileName)
import System.Environment (lookupEnv)
import System.Info (os)
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)

-- | Require exact expected Host and HTTP Origin; missing Origin is rejected.
allowedOrigin :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString -> Bool
allowedOrigin expectedHost host origin = host==expectedHost && origin==Just ("http://"<>expectedHost)

-- | Own the loopback listener and bracket the single viewer slot.
-- Return when the completion signal is consumed or the server terminates.
serveBrowser :: MVar () -> (WS.Connection -> IO ()) -> IO ()
serveBrowser done session = do
  html<-getDataFileName "assets/web/index.html" >>= BL.readFile
  script<-getDataFileName "assets/web/editor.js" >>= BL.readFile
  cellShader<-getDataFileName "assets/web/cell-shader.js" >>= BL.readFile
  canvasShader<-getDataFileName "assets/web/canvas-shader.js" >>= BL.readFile
  canvasImages<-getDataFileName "assets/web/canvas-images.js" >>= BL.readFile
  token<-B8.pack <$> randomIdentity
  slot<-newMVar ()
  bracket (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $ \socket->do
    N.bind socket (N.SockAddrInet 0 (N.tupleToHostAddress (127,0,0,1)))
    N.listen socket 8
    N.SockAddrInet port _<-N.getSocketName socket
    let host=B8.pack ("127.0.0.1:"++show port)
        prefix="/"<>token<>"/"
        url="http://"<>host<>prefix
        headers=[("Cache-Control","no-store"),("X-Content-Type-Options","nosniff"),("Referrer-Policy","no-referrer"),("Content-Security-Policy","default-src 'self'; script-src 'self'; style-src 'unsafe-inline'; connect-src 'self'; frame-ancestors 'none'")]
        static req respond
          | W.requestHeaderHost req/=Just host = respond (W.responseLBS status403 headers "Forbidden")
          | W.requestMethod req/="GET" = respond (W.responseLBS status405 headers "GET required")
          | W.rawPathInfo req==prefix = respond (W.responseLBS status200 (("Content-Type","text/html; charset=utf-8"):headers) html)
          | W.rawPathInfo req==prefix<>"editor.js" = respond (W.responseLBS status200 (("Content-Type","text/javascript; charset=utf-8"):headers) script)
          | W.rawPathInfo req==prefix<>"cell-shader.js" = respond (W.responseLBS status200 (("Content-Type","text/javascript; charset=utf-8"):headers) cellShader)
          | W.rawPathInfo req==prefix<>"canvas-shader.js" = respond (W.responseLBS status200 (("Content-Type","text/javascript; charset=utf-8"):headers) canvasShader)
          | W.rawPathInfo req==prefix<>"canvas-images.js" = respond (W.responseLBS status200 (("Content-Type","text/javascript; charset=utf-8"):headers) canvasImages)
          | otherwise = respond (W.responseLBS status404 headers "Not found")
        websocket pending = do
          let req=WS.pendingRequest pending
          if WS.requestPath req/=prefix<>"socket" || not (allowedOrigin host (maybe "" id (lookup "Host" (WS.requestHeaders req))) (lookup "Origin" (WS.requestHeaders req)))
            then WS.rejectRequest pending "Forbidden"
            else do
              available<-tryTakeMVar slot
              case available of
                Nothing -> WS.rejectRequest pending "Editor already connected"
                Just () -> finally
                  ((WS.acceptRequest pending >>= session) `catch` \(err::WS.ConnectionException)->hPutStrLn stderr ("Browser connection: "++show err))
                  (putMVar slot ())
        connectionOptions=WS.defaultConnectionOptions {WS.connectionCompressionOptions=WS.NoCompression,WS.connectionFramePayloadSizeLimit=WS.SizeLimit 16777216,WS.connectionMessageDataSizeLimit=WS.SizeLimit 16777216}
        app=websocketsOr connectionOptions websocket static
    hPutStrLn stderr ("Haskell browser: "++B8.unpack url)
    withAsync (Warp.runSettingsSocket Warp.defaultSettings socket app) $ \server->do
      autoOpen<-( /=Just "0") <$> lookupEnv "THC_EDIT_WEB_OPEN"
      when autoOpen $ callProcess (if os=="darwin" then "open" else "xdg-open") [B8.unpack url]
        `catch` \(_::IOException)->hPutStrLn stderr "Open the browser URL above to connect."
      race_ (wait server) (takeMVar done)
