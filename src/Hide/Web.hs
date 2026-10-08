{-# LANGUAGE CPP, OverloadedStrings #-}
-- | In-process browser display for the shared desktop model.
--
-- An IORef retains the desktop across browser reconnects. Each attached browser
-- loop runs the supplied tick, applies serialized input/effects and emits compressed
-- row frames; there is no tick loop during browser absence here. Explicit render
-- keys avoid payload equality merely to detect an idle frame. Grid/mode/pixelation
-- changes reset frame history.
module Hide.Web (runWeb
#ifdef WITH_WEB
  , WebInput(..), parseInput, applyInput, frameRows, framePacket, frameCandidates, frameDictionary, allowedOrigin, webDirty
#endif
  ) where
import Hide.Model hiding (Paste)
#ifdef WITH_WEB
import Hide.Links (followLink)
import Hide.Protocol
import Hide.BrowserServer
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
import Hide.Buffer (bufferBytes)
import System.FilePath (takeFileName)
import Hide.Files (filePath)
import Hide.Font
import Hide.Frontend (modeSize)
import Hide.Render (renderKey)
import Hide.RequestedPaste

-- | Serve a browser frontend, intercepting clipboard, download, link and mode
-- effects. Acknowledge input after its effects have been applied.
runWeb :: Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> Desktop -> IO ()
runWeb scale effects tick initial = do
  font<-loadFont
  state<-newIORef initial {browserFrontend=True}
  done<-newEmptyMVar
  let
      session conn = do
        pasteReads<-newRequestedPaste
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
          readIORef state >>= loop pasteReads conn send queue disconnected Nothing
      loop pasteReads conn send queue disconnected previous d = do
        pending<-tick d
        -- Retain the pending intent if socket delivery fails before retirement.
        writeIORef state pending
        exported<-case pendingFileExport pending of
          (serial,Just offer@(ExportFileCopy _ bytes _ _))->do
            send (fileExportHeader offer)
            WS.sendBinaryData conn bytes
            pure pending {pendingFileExport=(serial,Nothing)}
          _->pure pending
        writeIORef state exported
        current<-case clipboardExport exported of
          (serial,Just text)->do
            send (object ["type" .= ("copy"::T.Text),"text" .= text])
            pure exported {clipboardExport=(serial,Nothing)}
          _->pure exported
        refreshRequestedPaste pasteReads current
        cwd<-getCurrentDirectory
        writeIORef state current
        -- Never compare desktops here: even idle equality would walk buffers
        -- and undo history. Share the native frontend's bounded render key.
        stateKey<-renderKey current
        let key=(stateKey,cwd)
            resetKey=(screenSize current,videoMode current,pixelateUnicode current)
            oldRows=maybe [] (\(_,_,cached,_) -> cached) previous
            oldMetadata=maybe [] (\(_,_,_,meta) -> meta) previous
            sameFrame=maybe False (\(old,_,_,_)->old==key) previous
            rows=if sameFrame then oldRows else frameRows current
            metadata=if sameFrame then oldMetadata else frameMetadata cwd current
            reset=maybe True (\(_,old,_,_)->old/=resetKey) previous
        when (reset || (not sameFrame && (rows/=oldRows || metadata/=oldMetadata))) $
          WS.sendBinaryData conn (framePacket reset oldRows rows (if reset then metadata else filter (`notElem` oldMetadata) metadata))
        event<-timeout 50000 (atomically ((readTMVar disconnected >> pure Nothing) `orElse` readTBQueue queue))
        case event of
          Just Nothing -> pure ()
          _ -> do
            (next,requests)<-case event >>= id of
              Just (_,PasteReply token text)->applyRequestedPaste pasteReads token text current
              Just (_,inputEvent)->pure (applyInput inputEvent current)
              Nothing->pure (current,[])
            refreshRequestedPaste pasteReads next
            (exit,updated)<-foldM (effect pasteReads conn send) (False,next) requests
            refreshRequestedPaste pasteReads updated
            writeIORef state updated
            case event >>= id of
              Just (serial,_) -> send (object ["type" .= ("ack"::T.Text),"seq" .= serial,"dirty" .= webDirty updated])
              Nothing -> pure ()
            if exit then send (object ["type" .= ("closed"::T.Text)]) >> void (tryPutMVar done ())
              else loop pasteReads conn send queue disconnected (Just (key,resetKey,rows,metadata)) updated
      effect _ _ _ result@(True,_) _ = pure result
      effect pasteReads _ send (_,d) (FollowLink origin target) | not (linkOriginCurrent d origin)=pure (False,d {status="Link body expired."})
      effect pasteReads _ send (_,d) (FollowLink origin target) = do
        (opened,packet)<-followLink True d (linkOriginPath origin) target
        mapM_ send packet
        refreshRequestedPaste pasteReads opened
        pure (False,opened)
      effect pasteReads _ send (_,d) ReadBrowserClipboard = do
        token<-requestPaste pasteReads d
        mapM_ (\value->send (object ["type" .= ("paste-request"::T.Text),"request" .= value])) token
        pure (False,d)
      effect _ _ send (_,d) (WriteBrowserClipboard text) = send (object ["type" .= ("copy"::T.Text),"text" .= text]) >> pure (False,d)
      effect _ conn send (_,d) (DownloadDocument bid) = do
        case M.lookup bid (buffers d) of
          Nothing -> pure ()
          Just doc -> do
            let name=maybe (maybe "NONAME.HS" id (documentSuggestedName doc)) (takeFileName . filePath) (documentFile doc)
            _ <- send (object ["type" .= ("download"::T.Text),"name" .= name])
            WS.sendBinaryData conn (bufferBytes (documentBuffer doc))
        pure (False,d)
      effect _ _ _ (_,d) (SetScreenMode mode) = pure (False,(resizeScreenMode (modeSize mode) d) {videoMode=Just mode})
      effect pasteReads _ _ (_,d) request = do
        result@(_,updated)<-effects d [request]
        refreshRequestedPaste pasteReads updated
        pure result
  serveBrowser done session
#else
runWeb :: Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> Desktop -> IO ()
runWeb _ _ _ _ = ioError (userError "Browser support is not built. Rebuild with cabal build -fweb")
#endif
