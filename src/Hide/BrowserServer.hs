-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- |
-- Module      : Hide.BrowserServer
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings, ScopedTypeVariables
--
-- Loopback HTTP/WebSocket host shared by browser frontends.
--
-- An unpredictable URL path, exact Host/Origin checks and a single-viewer slot
-- restrict attachment. This is local capability-based access, not user-account
-- authentication. The server scopes listener and viewer ownership and applies
-- bounded WebSocket messages and restrictive asset response headers.
module Hide.BrowserServer (serveBrowser, allowedOrigin) where
import Control.Concurrent.Async (withAsync, wait, race_)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally, catch, IOException)
import Control.Monad (when, unless)
import Data.Aeson (Value(..), object, (.=), encode)
import qualified Data.Text as T
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
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), takeExtension)
import Text.Read (readMaybe)
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
  inferenceClient<-getDataFileName "assets/web/system-one-client.js" >>= BL.readFile
  inferenceAssets<-loadInferenceAssets
  token<-B8.pack <$> randomIdentity
  slot<-newMVar ()
  bracket (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $ \socket->do
    N.bind socket (N.SockAddrInet 0 (N.tupleToHostAddress (127,0,0,1)))
    N.listen socket 8
    N.SockAddrInet port _<-N.getSocketName socket
    let host=B8.pack ("127.0.0.1:"++show port)
        prefix="/"<>token<>"/"
        url="http://"<>host<>prefix
        inferenceFiles=maybe [] (inferenceRoutes prefix) inferenceAssets
        headers=[("Cross-Origin-Opener-Policy","same-origin"),("Cross-Origin-Embedder-Policy","require-corp"),("Cross-Origin-Resource-Policy","same-origin"),("Cache-Control","no-store"),("X-Content-Type-Options","nosniff"),("Referrer-Policy","no-referrer"),("Content-Security-Policy","default-src 'self'; script-src 'self' 'wasm-unsafe-eval'; worker-src 'self' blob:; style-src 'unsafe-inline'; connect-src 'self'; frame-ancestors 'none'")]
        static req respond
          | W.requestHeaderHost req/=Just host = respond (W.responseLBS status403 headers "Forbidden")
          | W.requestMethod req/="GET" = respond (W.responseLBS status405 headers "GET required")
          | W.rawPathInfo req==prefix = respond (W.responseLBS status200 (("Content-Type","text/html; charset=utf-8"):headers) html)
          | W.rawPathInfo req==prefix<>"editor.js" = respond (W.responseLBS status200 (("Content-Type","text/javascript; charset=utf-8"):headers) script)
          | W.rawPathInfo req==prefix<>"cell-shader.js" = respond (W.responseLBS status200 (("Content-Type","text/javascript; charset=utf-8"):headers) cellShader)
          | W.rawPathInfo req==prefix<>"canvas-shader.js" = respond (W.responseLBS status200 (("Content-Type","text/javascript; charset=utf-8"):headers) canvasShader)
          | W.rawPathInfo req==prefix<>"canvas-images.js" = respond (W.responseLBS status200 (("Content-Type","text/javascript; charset=utf-8"):headers) canvasImages)
          | W.rawPathInfo req==prefix<>"system-one-client.js" = respond (W.responseLBS status200 (("Content-Type","text/javascript; charset=utf-8"):headers) inferenceClient)
          | W.rawPathInfo req==prefix<>"system-one-config.json" = respond (W.responseLBS status200 (("Content-Type","application/json"):headers) (encode (inferenceConfiguration inferenceAssets)))
          | Just path<-lookup (W.rawPathInfo req) inferenceFiles = respond (W.responseFile status200 (("Content-Type",assetType path):headers) path Nothing)
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


-- These explicit frontend-process paths are never accepted from a daemon or
-- browser request. Only the listed immutable artifact names become HTTP routes.
-- Warp streams responseFile bodies; the 1.6 GiB checkpoint is not a lazy body
-- retained in this server or copied into the package's data directory.
data InferenceAssets=InferenceAssets !FilePath !FilePath !T.Text !Integer

runtimeFiles, modelFiles :: [FilePath]
runtimeFiles=["system-one-runtime.js","ort-wasm-simd-threaded.asyncify.mjs","ort-wasm-simd-threaded.asyncify.wasm"]
modelFiles=["manifest.json","LICENSE","MODEL_CARD.md","encoder.onnx","encoder.onnx.data","encoder_config.json","head.onnx","head.onnx.data","rl_agent_config.json","tokenizer.json","tokenizer_config.json"]

loadInferenceAssets :: IO (Maybe InferenceAssets)
loadInferenceAssets=do
  runtime<-lookupEnv "HIDE_SYSTEM_ONE_WEB_RUNTIME_DIR"
  model<-lookupEnv "HIDE_SYSTEM_ONE_WEB_MODEL_DIR"
  manifest<-lookupEnv "HIDE_SYSTEM_ONE_WEB_MANIFEST_SHA256"
  allocation<-lookupEnv "HIDE_SYSTEM_ONE_WEB_GPU_BYTES"
  case (runtime,model,manifest,allocation) of
    (Nothing,Nothing,Nothing,Nothing)->pure Nothing
    (Just runtimePath,Just modelPath,Just expected,limit)->do
      unless (length expected==64 && all (`elem` ("0123456789abcdef"::String)) expected) (invalid "manifest SHA256")
      bytes<-case maybe (Just 3221225472) readMaybe limit of
        Just value | value>0 && value<=9007199254740991->pure value
        _->invalid "WebGPU allocation limit"
      runtimeRoot<-directory runtimePath runtimeFiles
      modelRoot<-directory modelPath modelFiles
      pure (Just (InferenceAssets runtimeRoot modelRoot (T.pack expected) bytes))
    _->invalid "incomplete runtime/model/manifest configuration"
  where
    invalid field=ioError (userError ("System-1 browser assets: "++field))
    directory path files=do
      present<-doesDirectoryExist path
      unless present (invalid "configured directory does not exist")
      root<-canonicalizePath path
      mapM_ (\name->do found<-doesFileExist (root </> name); unless found (invalid ("missing "++name))) files
      pure root

inferenceConfiguration :: Maybe InferenceAssets -> Value
inferenceConfiguration Nothing=Null
inferenceConfiguration (Just (InferenceAssets _ _ manifest allocation))=object
  ["workerURL" .= ("system-one-runtime/system-one-runtime.js"::T.Text)
  ,"runtimeBaseURL" .= ("system-one-runtime/"::T.Text)
  ,"modelBaseURL" .= ("system-one-model/"::T.Text)
  ,"manifestSHA256" .= manifest,"allocationLimitBytes" .= allocation
  ,"label" .= ("Local Laya (WebGPU)"::T.Text)]

inferenceRoutes :: BS.ByteString -> InferenceAssets -> [(BS.ByteString,FilePath)]
inferenceRoutes prefix (InferenceAssets runtime model _ _)=
  [(prefix<>"system-one-runtime/"<>B8.pack name,runtime </> name) | name<-runtimeFiles]++
  [(prefix<>"system-one-model/"<>B8.pack name,model </> name) | name<-modelFiles]

assetType :: FilePath -> BS.ByteString
assetType path=case takeExtension path of
  ".js"->"text/javascript; charset=utf-8"
  ".mjs"->"text/javascript; charset=utf-8"
  ".wasm"->"application/wasm"
  ".json"->"application/json"
  _->"application/octet-stream"
