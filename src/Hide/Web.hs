-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE CPP, OverloadedStrings #-}
-- |
-- Module      : Hide.Web
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : CPP, OverloadedStrings
--
-- In-process browser display for the shared desktop model.
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
import Hide.SystemOneBrowser (SystemOneBrowser)
#ifdef WITH_WEB
import Hide.Links (followLink)
import Hide.Protocol
import Hide.BrowserServer
import qualified Hide.SystemOneBrowser as DecisionBrowser
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (finally, bracket, try)
import Control.Monad (forever, when, void, foldM)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither)
import Data.IORef
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Directory (getCurrentDirectory)
import System.Timeout (timeout)
import Hide.Buffer (bufferBytes)
import System.FilePath (takeFileName)
import Hide.Files (filePath)
import Hide.Font
import Hide.Frontend (modeSize)
import Hide.Render (renderKey)
import Hide.RemoteEndpoint (randomIdentity)
import Hide.RequestedPaste

-- | Serve a browser frontend, intercepting clipboard, download, link and mode
-- effects. Acknowledge input after its effects have been applied.
runWeb :: Maybe SystemOneBrowser -> Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> Desktop -> IO ()
runWeb inference scale effects tick initial = do
  font<-loadFont
  state<-newIORef initial {browserFrontend=True}
  done<-newEmptyMVar
  let
      session conn = do
        pasteReads<-newRequestedPaste
        queue<-newTBQueueIO 128
        -- One admitted request plus its cancellation/retirement control.
        -- Editor traffic cannot consume this reserved FIFO or reorder it.
        inferenceQueue<-newTBQueueIO 2
        disconnected<-newEmptyTMVarIO
        let send=WS.sendTextData conn . encode
            enqueue control=atomically $ do
              live<-isEmptyTMVar disconnected
              full<-isFullTBQueue inferenceQueue
              if live && not full then writeTBQueue inferenceQueue control >> pure True
              else do
                let release=case control of
                      Object fields->KM.lookup "type" fields `elem` map (Just . String) ["system-one-cancel","system-one-retire"]
                      _->False
                when (live && release) (void (tryPutTMVar disconnected True))
                pure False
            acquire=traverse (\owner->DecisionBrowser.attachBrowser owner 0 enqueue) inference
            retire attachment=do
              forced<-atomically $ do
                void (tryPutTMVar disconnected False)
                readTMVar disconnected
              (when forced $ void (try (WS.sendCloseCode conn 1011 ("Inference transport unavailable"::T.Text)) :: IO (Either WS.ConnectionException ())))
                `finally` mapM_ DecisionBrowser.retireBrowserAttachment attachment
            invalid=WS.sendCloseCode conn 1003 ("Invalid input"::T.Text) >> ioError (userError "Invalid browser input")
        bracket acquire retire $ \attachment->do
          let receive=forever $ do
                bytes<-WS.receiveData conn :: IO BL.ByteString
                case eitherDecode bytes of
                  Left _->invalid
                  Right value@(Object fields) | Just (String kind)<-KM.lookup "type" fields, "system-one-" `T.isPrefixOf` kind->
                    -- This receiver accepts only private reply/offer controls;
                    -- viewer identity is minted here, never supplied by input.
                    mapM_ (\owner->void (DecisionBrowser.receiveBrowserControl owner value)) attachment
                  Right value->case parseEither (\event->(,) <$> withObject "event sequence" (\o->o .:? "seq" .!= (0::Int)) event <*> parseInput event) value of
                    Left _->invalid
                    Right (serial,UploadFile name _)->do
                      payload<-timeout 30000000 (WS.receiveDataMessage conn)
                      case payload of
                        Just (WS.Binary payloadBytes) | BL.length payloadBytes<=16777216->atomically (writeTBQueue queue (Just (Right (serial,UploadFile name (BL.toStrict payloadBytes)))))
                        _->invalid
                    Right event->atomically (writeTBQueue queue (Just (Right event)))
          mapM_ (\owner->do
            viewer<-T.pack <$> randomIdentity
            _<-DecisionBrowser.setBrowserViewer owner (Just viewer)
            send (object ["type" .= ("system-one-connection"::T.Text),"connection" .= DecisionBrowser.browserAttachmentId owner,"viewer" .= viewer])) attachment
          send (assetsPacket font scale)
          canvasEpoch<-T.pack <$> randomIdentity
          send (canvasReset canvasEpoch)
          WS.withPingThread conn 15 (pure ()) $ withAsync (finally receive (atomically (void (tryPutTMVar disconnected False)))) $ \_ ->
            readIORef state >>= loop canvasEpoch (CanvasSender canvasEpoch M.empty) pasteReads conn send queue inferenceQueue disconnected Nothing
      loop canvasEpoch transfers pasteReads conn send queue inferenceQueue disconnected previous d = do
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
            oldRows=maybe [] (\(_,_,cached,_,_) -> cached) previous
            oldMetadata=maybe [] (\(_,_,_,meta,_) -> meta) previous
            sameFrame=maybe False (\(old,_,_,_,_)->old==key) previous
            (freshRows,freshScene)=frameRowsAndCanvas current
            scene=case previous of Just (_,_,_,_,cached) | sameFrame->cached; _->freshScene
            rows=if sameFrame then oldRows else freshRows
            metadata=if sameFrame then oldMetadata else canvasMetadata canvasEpoch (fst (screenSize current)) scene : frameMetadata cwd current
            reset=maybe True (\(_,old,_,_,_)->old/=resetKey) previous
        when (reset || (not sameFrame && (rows/=oldRows || metadata/=oldMetadata))) $
          WS.sendBinaryData conn (framePacket reset oldRows rows (if reset then metadata else filter (`notElem` oldMetadata) metadata))
        let (nextTransfers,canvasPackets,moreCanvas)=canvasTransfer transfers scene
        mapM_ (\packet->case packet of JsonPacket value->send value; BinaryPacket bytes->WS.sendBinaryData conn bytes) canvasPackets
        let nextEvent=(Just . Left <$> readTBQueue inferenceQueue) `orElse` readTBQueue queue
        event<-if moreCanvas then atomically ((readTMVar disconnected >> pure (Just Nothing)) `orElse` (Just <$> nextEvent) `orElse` pure Nothing)
          else timeout 50000 (atomically ((readTMVar disconnected >> pure Nothing) `orElse` nextEvent))
        case event of
          Just Nothing -> pure ()
          _ -> do
            -- Inference controls share this sole socket writer. They cannot
            -- split a file/canvas header from its binary payload.
            mapM_ (either send (const (pure ()))) (event >>= id)
            let input=event >>= id >>= either (const Nothing) Just
            (next,requests)<-case input of
              Just (_,PasteReply token text)->applyRequestedPaste pasteReads token text current
              Just (_,inputEvent)->pure (applyInput inputEvent current)
              Nothing->pure (current,[])
            refreshRequestedPaste pasteReads next
            (exit,updated)<-foldM (effect pasteReads conn send) (False,next) requests
            refreshRequestedPaste pasteReads updated
            writeIORef state updated
            case input of
              Just (serial,_) -> send (object ["type" .= ("ack"::T.Text),"seq" .= serial,"dirty" .= webDirty updated])
              Nothing -> pure ()
            if exit then send (object ["type" .= ("closed"::T.Text)]) >> void (tryPutMVar done ())
              else loop canvasEpoch nextTransfers pasteReads conn send queue inferenceQueue disconnected (Just (key,resetKey,rows,metadata,scene)) updated
      effect _ _ _ result@(True,_) _ = pure result
      effect _ _ _ (_,d) (FollowLink origin _) | not (linkOriginCurrent d origin)=pure (False,d {status="Link body expired."})
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
runWeb :: Maybe SystemOneBrowser -> Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> Desktop -> IO ()
runWeb _ _ _ _ _ = ioError (userError "Browser support is not built. Rebuild with cabal build -fweb")
#endif
