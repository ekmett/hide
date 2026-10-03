{-# LANGUAGE CPP, OverloadedStrings #-}
module THC.Edit.RemoteWeb (runRemoteWeb) where
import THC.Edit.Remote (RemotePeer(..))
#if defined(WITH_REMOTE) && defined(WITH_WEB)
import Control.Concurrent.Async (race_)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, unless, when, void)
import Data.Aeson
import Data.Aeson.Types (parseEither, parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Timeout (timeout)
import THC.Edit.Protocol
import THC.Edit.BrowserServer

-- Keep a complete reconstructed screen for browser reconnects. The SSH peer
-- remains attached while the browser is absent, so tooling continues running.
data Cache = Cache
  { cachedAssets :: Maybe Value, cachedRows :: [Value], cachedMeta :: Object
  , cachedConnection :: Maybe Value, downloading :: Bool, sessionClosed :: Bool }

runRemoteWeb :: Double -> String -> RemotePeer -> IO ()
runRemoteWeb scale host peer = do
  cache<-newTVarIO (Cache Nothing [] KM.empty Nothing False False)
  subscriber<-newTVarIO Nothing
  generation<-newTVarIO (0::Integer)
  nextSerial<-newTVarIO (0::Integer)
  aliases<-newTVarIO M.empty
  -- Command results survive a browser disconnect, including a Cut whose edit
  -- has already committed remotely. Downloads are retained as complete pairs.
  replies<-newTVarIO Seq.empty
  replyBytes<-newTVarIO (0::Int)
  downloadHeader<-newTVarIO Nothing
  done<-newEmptyMVar
  let emit packet=do
        current<-readTVar subscriber
        case current of Nothing->pure (); Just queue->writeTBQueue queue packet
      retain packets=do
        let bytes=sum [case p of JsonPacket v->fromIntegral (BL.length (encode v)); BinaryPacket b->BS.length b | p<-packets]
        pending<-readTVar replies
        size<-readTVar replyBytes
        check (Seq.length pending<128 && size+bytes<=33554432)
        writeTVar replies (pending Seq.|> (packets,bytes))
        writeTVar replyBytes (size+bytes)
      receive=forever $ do
        incoming<-peerReceive peer
        packet<-case incoming of
          Just value -> pure value
          Nothing -> do
            closed<-sessionClosed <$> readTVarIO cache
            if closed then atomically retry else ioError (userError "SSH connection ended")
        before<-readTVarIO cache
        case packet of
          BinaryPacket bytes | not (downloading before) -> do
            (value,rows)<-decodeFrame (cachedRows before) bytes
            fields<-case value of Object fields->pure fields; _->ioError (userError "Invalid remote display metadata")
            atomically $ do
              modifyTVar' cache (\c->c {cachedRows=rows,cachedMeta=KM.union (foldr KM.delete fields ["type","reset","rows"]) (cachedMeta c)})
              emit packet
          BinaryPacket _ -> atomically $ do
            header<-readTVar downloadHeader
            mapM_ (\h->retain [h,packet]) header
            writeTVar downloadHeader Nothing
            modifyTVar' cache (\c->c {downloading=False})
          JsonPacket value -> case messageType value of
            Just "assets" -> do
              let adjusted=case value of Object fields->Object (KM.insert "scale" (toJSON scale) fields); _->value
              atomically $ modifyTVar' cache (\c->c {cachedAssets=Just adjusted}) >> emit (JsonPacket adjusted)
            Just "connection" -> atomically $ modifyTVar' cache (\c->c {cachedConnection=Just value,downloading=False}) >> writeTVar downloadHeader Nothing >> emit packet
            Just "open-resource" -> atomically (emit packet)
            Just "download" -> atomically $ modifyTVar' cache (\c->c {downloading=True}) >> writeTVar downloadHeader (Just packet)
            Just "copy" -> atomically (retain [packet])
            Just "ack" -> atomically $ do
              table<-readTVar aliases
              case parseMaybe (withObject "ack" (\o->o .: "seq")) value >>= (`M.lookup` table) of
                Nothing->pure ()
                Just (gen,serial)->do
                  current<-readTVar generation
                  when (gen==current) $ case value of
                    Object fields->emit (JsonPacket (Object (KM.insert "seq" (toJSON serial) fields)))
                    _->pure ()
              case parseMaybe (withObject "ack" (\o->o .: "seq")) value of
                Just key->modifyTVar' aliases (M.delete (key::Integer))
                Nothing->pure ()
            Just "closed" -> do
              attached<-atomically $ do
                modifyTVar' cache (\c->c {sessionClosed=True})
                emit packet
                maybe False (const True) <$> readTVar subscriber
              unless attached (void (tryPutMVar done ()))
            Just "hello" -> pure ()
            _ -> atomically (emit packet)
      sendPacket conn packet=case packet of
        JsonPacket value->WS.sendTextData conn (encode value)
        BinaryPacket bytes->WS.sendBinaryData conn bytes
      session conn = do
        queue<-newTBQueueIO 128
        (gen,snapshot)<-atomically $ do
          current<-readTVar cache
          case cachedAssets current of Nothing->retry; Just _->pure ()
          -- A binary download is one indivisible browser message pair.
          when (downloading current) retry
          modifyTVar' generation (+1)
          gen<-readTVar generation
          writeTVar aliases M.empty
          writeTVar subscriber (Just queue)
          pure (gen,current)
        let cleanup=atomically (writeTVar subscriber Nothing)
            number value = do
              serial<-either (ioError . userError) pure (parseEither (withObject "event" (\o->o .:? "seq" .!= 0)) value :: Either String Integer)
              atomically $ do
                modifyTVar' nextSerial (+1)
                key<-readTVar nextSerial
                modifyTVar' aliases (M.insert key (gen,serial))
                pure (case value of Object fields->Object (KM.insert "seq" (toJSON key) fields); _->value)
            incoming=forever $ do
              bytes<-WS.receiveData conn :: IO BL.ByteString
              value<-either (ioError . userError) pure (eitherDecode bytes)
              when (messageType value==Just "detach") $ do
                atomically (writeTBQueue queue (JsonPacket (object ["type" .= ("detached"::T.Text)])))
                atomically retry
              input<-either (ioError . userError) pure (parseEither parseInput value)
              case input of
                UploadFile _ _ -> do
                  blob<-timeout 30000000 (WS.receiveDataMessage conn)
                  payload<-case blob of
                    Just (WS.Binary payload) | BL.length payload<=16777216 -> pure (BL.toStrict payload)
                    _->ioError (userError "Expected upload bytes (maximum 16 MiB)")
                  numbered<-number value
                  peerSendBatch peer [JsonPacket numbered,BinaryPacket payload]
                _ -> number value >>= peerSend peer . JsonPacket
            outgoing=forever $ do
              work<-atomically $ (do
                pending<-readTVar replies
                case Seq.viewl pending of
                  Seq.EmptyL->retry
                  (packets,bytes) Seq.:< _->pure (Left (packets,bytes)))
                `orElse` (Right <$> readTBQueue queue)
              packet<-case work of
                Left (packets,bytes)->do
                  mapM_ (sendPacket conn) packets
                  atomically $ modifyTVar' replies (Seq.drop 1) >> modifyTVar' replyBytes (subtract bytes)
                  pure (JsonPacket Null)
                Right p->sendPacket conn p >> pure p
              case packet of
                JsonPacket value | messageType value `elem` [Just "closed",Just "detached"] -> void (tryPutMVar done ())
                _->pure ()
        finally (do
          sendPacket conn (JsonPacket (object ["type" .= ("remote"::T.Text),"host" .= host]))
          mapM_ (sendPacket conn . JsonPacket) (cachedAssets snapshot)
          unless (null (cachedRows snapshot)) $ sendPacket conn (BinaryPacket (BL.toStrict (framePacket True [] (cachedRows snapshot) (KM.toList (cachedMeta snapshot)))))
          mapM_ (sendPacket conn . JsonPacket) (cachedConnection snapshot)
          if sessionClosed snapshot then sendPacket conn (JsonPacket (object ["type" .= ("closed"::T.Text)])) >> void (tryPutMVar done ())
          else WS.withPingThread conn 15 (pure ()) (race_ incoming outgoing)) cleanup
  race_ receive (serveBrowser done session)

messageType :: Value -> Maybe T.Text
messageType = parseMaybe (withObject "message" (\o->o .: "type"))
#else
runRemoteWeb :: Double -> String -> RemotePeer -> IO ()
runRemoteWeb _ _ _ = ioError (userError "Remote browser support is not built. Rebuild with cabal build -fremote -fweb")
#endif
