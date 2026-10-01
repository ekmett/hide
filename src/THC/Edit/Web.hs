{-# LANGUAGE CPP, OverloadedStrings #-}
module THC.Edit.Web (runWeb
#ifdef WITH_WEB
  , WebInput(..), parseInput, applyInput, frameRows, framePacket, frameCandidates, frameDictionary, allowedOrigin, webDirty
#endif
  ) where
import THC.Edit.Model hiding (Paste)
#ifdef WITH_WEB
import THC.Edit.Protocol
import THC.Edit.BrowserServer
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, when, void, foldM)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.IORef
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Directory (getCurrentDirectory)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)
import THC.Edit.Buffer (bufferBytes)
import System.FilePath (takeFileName)
import THC.Edit.Files (filePath)
import THC.Edit.Font
import THC.Edit.Frontend (modeSize)

runWeb :: Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> Desktop -> IO ()
runWeb scale effects tick initial = do
  font<-loadFont
  state<-newIORef initial {browserFrontend=True}
  done<-newEmptyMVar
  let
      session conn = do
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
  serveBrowser done session
#else
runWeb :: Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> Desktop -> IO ()
runWeb _ _ _ _ = ioError (userError "Browser support is not built. Rebuild with cabal build -fweb")
#endif
