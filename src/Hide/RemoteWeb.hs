{-# LANGUAGE BangPatterns, CPP, OverloadedStrings #-}
-- | Local browser bridge for a persistent remote peer.
--
-- The peer remains attached while browsers disconnect. Reconstructed rows and
-- metadata provide a reset frame on the next browser connection; clipboard and
-- complete download results have separate bounded retention. Connection
-- generations keep acknowledgements for an old browser from reaching its
-- replacement. Interrupted delivery can replay retained results.
module Hide.RemoteWeb (runRemoteWeb) where
import Hide.Remote (RemotePeer(..))
#if defined(WITH_REMOTE) && defined(WITH_WEB)
import Control.Concurrent.Async (race_)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (finally, evaluate)
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
import Hide.Protocol
import Hide.BrowserServer

-- Keep a complete reconstructed screen for browser reconnects. The SSH peer
-- remains attached while the browser is absent, so tooling continues running.
data Cache = Cache
  { cachedAssets :: Maybe Value, cachedRows :: [Value], cachedMeta :: Object
  , cachedConnection :: Maybe Value, pendingBinary :: Maybe BinaryPurpose
  , cachedCanvas :: CanvasCache, sessionClosed :: Bool }

-- One current cursor and immutable completed resources. Byte admission includes
-- the entire unfinished resource; replay uses its retained contiguous prefix.
data CanvasResource = CanvasResource !Value !BS.ByteString
data CanvasUpload = CanvasUpload !Value !T.Text !Int !Int ![BS.ByteString]
data CanvasCache = CanvasCache
  { canvasEpoch :: !(Maybe T.Text), canvasResources :: !(M.Map T.Text CanvasResource)
  , canvasUpload :: !(Maybe CanvasUpload), canvasBytes :: !Int }
data BinaryPurpose = DownloadBytes !WirePacket | CanvasBytes !Value !T.Text !Int !Int

emptyCanvas :: CanvasCache
emptyCanvas = CanvasCache Nothing M.empty Nothing 0

canvasControl :: CanvasCache -> Value -> Either String (CanvasCache,Maybe BinaryPurpose)
canvasControl current value = parseEither (withObject "canvas control" $ \o->do
  kind<-o .: "type";epoch<-o .: "epoch"
  let validId t=T.length t==48 && T.all (\c->c>='0'&&c<='9'||c>='a'&&c<='f') t
      require condition=unless condition (fail "Invalid canvas resource")
  require (validId epoch)
  if kind==("canvas-reset"::T.Text) then pure (emptyCanvas {canvasEpoch=Just epoch},Nothing) else do
    ident<-o .: "id";require (validId ident && canvasEpoch current==Just epoch)
    case kind of
      "canvas-resource"->do
        width<-o .: "width";height<-o .: "height";bytes<-o .: "bytes"
        require (width>0 && width<=4096 && height>0 && height<=4096 && width*height<=(4194304::Int) && bytes==width*height*4 && canvasBytes current+bytes<=67108864 && M.size (canvasResources current)<64 && M.notMember ident (canvasResources current))
        case canvasUpload current of Just _->fail "Canvas upload already active";Nothing->pure ()
        pure (current {canvasUpload=Just (CanvasUpload value ident bytes 0 []),canvasBytes=canvasBytes current+bytes},Nothing)
      "canvas-chunk"->do
        offset<-o .: "offset";bytes<-o .: "length"
        case canvasUpload current of
          Just (CanvasUpload _ active total received _) -> require (ident==active && offset==received && bytes>0 && bytes<=262144 && offset+bytes<=total)
          Nothing->fail "Canvas chunk without resource"
        pure (current,Just (CanvasBytes value ident offset bytes))
      "canvas-release"->do
        let retired=case M.lookup ident (canvasResources current) of Just (CanvasResource _ bytes)->BS.length bytes;Nothing->0
            (upload,released)=case canvasUpload current of Just (CanvasUpload _ active bytes _ _) | active==ident->(Nothing,bytes);_->(canvasUpload current,0)
        pure (current {canvasResources=M.delete ident (canvasResources current),canvasUpload=upload,canvasBytes=canvasBytes current-retired-released},Nothing)
      _->fail "Unknown canvas control") value

canvasChunk :: CanvasCache -> T.Text -> Int -> Int -> BS.ByteString -> Either String CanvasCache
canvasChunk current ident offset count bytes = case canvasUpload current of
  Just (CanvasUpload header active total received chunks)
    | active==ident && received==offset && BS.length bytes==count ->
      let next=received+count in
      if next==total then
        let !payload=BS.concat (reverse (bytes:chunks)) in
        Right current {canvasResources=M.insert ident (CanvasResource header payload) (canvasResources current),canvasUpload=Nothing}
      else Right current {canvasUpload=Just (CanvasUpload header active total next (bytes:chunks))}
  _->Left "Invalid canvas chunk bytes"

canvasReplay :: CanvasCache -> [WirePacket]
canvasReplay current =
  [JsonPacket (object ["type" .= ("canvas-reset"::T.Text),"epoch" .= epoch]) | Just epoch<-[canvasEpoch current]] ++
  concat [JsonPacket header:chunks header 0 (split bytes) | CanvasResource header bytes<-M.elems (canvasResources current)] ++
  case canvasUpload current of
    Nothing->[]
    Just (CanvasUpload header _ _ _ prefix)->JsonPacket header:chunks header 0 (reverse prefix)
  where
    split bytes | BS.null bytes=[] | otherwise=let (front,rest)=BS.splitAt 262144 bytes in front:split rest
    chunks _ _ []=[]
    chunks header offset (bytes:rest)=
      let fields=case header of Object o->o;_->KM.empty
          chunk=object ["type" .= ("canvas-chunk"::T.Text),"epoch" .= KM.lookup "epoch" fields,"id" .= KM.lookup "id" fields,"offset" .= offset,"length" .= BS.length bytes]
      in JsonPacket chunk:BinaryPacket bytes:chunks header (offset+BS.length bytes) rest

-- | Run peer reception alongside the single-viewer browser server.
-- Uploads enter the peer as atomic metadata/payload batches.
runRemoteWeb :: Double -> String -> RemotePeer -> IO ()
runRemoteWeb scale host peer = do
  cache<-newTVarIO (Cache Nothing [] KM.empty Nothing Nothing emptyCanvas False)
  subscriber<-newTVarIO Nothing
  generation<-newTVarIO (0::Integer)
  nextSerial<-newTVarIO (0::Integer)
  aliases<-newTVarIO M.empty
  -- Command results survive a browser disconnect, including a Cut whose edit
  -- has already committed remotely. Downloads are retained as complete pairs.
  replies<-newTVarIO Seq.empty
  replyBytes<-newTVarIO (0::Int)
  done<-newEmptyMVar
  let emitBatch packets=do
        current<-readTVar subscriber
        case current of Nothing->pure (); Just queue->writeTBQueue queue packets
      emit packet=emitBatch [packet]
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
          BinaryPacket bytes -> case pendingBinary before of
            Nothing -> do
              (value,rows)<-decodeFrame (cachedRows before) bytes
              fields<-case value of Object fields->pure fields; _->ioError (userError "Invalid remote display metadata")
              atomically $ do
                modifyTVar' cache (\c->c {cachedRows=rows,cachedMeta=KM.union (foldr KM.delete fields ["type","reset","rows"]) (cachedMeta c)})
                emit packet
            Just (DownloadBytes header) -> atomically $ do
              retain [header,packet]
              modifyTVar' cache (\c->c {pendingBinary=Nothing})
            Just (CanvasBytes header ident offset count) -> do
              updated<-either (ioError . userError) evaluate (canvasChunk (cachedCanvas before) ident offset count bytes)
              atomically $ do
                modifyTVar' cache (\c->c {cachedCanvas=updated,pendingBinary=Nothing})
                emitBatch [JsonPacket header,packet]
          JsonPacket value -> do
            case pendingBinary before of Nothing->pure ();Just _->ioError (userError "Expected remote binary payload")
            case messageType value of
              Just "assets" -> do
                let adjusted=case value of Object fields->Object (KM.insert "scale" (toJSON scale) fields); _->value
                atomically $ modifyTVar' cache (\c->c {cachedAssets=Just adjusted,cachedRows=[],cachedMeta=KM.empty,cachedCanvas=emptyCanvas}) >> emit (JsonPacket adjusted)
              Just "connection" -> atomically $ do
                let disconnected=parseMaybe (withObject "connection" (\o->o .: "connected")) value==Just False
                modifyTVar' cache (\c->if disconnected then c {cachedConnection=Just value,cachedCanvas=emptyCanvas,cachedMeta=KM.delete "canvas" (cachedMeta c)} else c {cachedConnection=Just value})
                emit packet
              Just kind | "canvas-" `T.isPrefixOf` kind -> do
                (updated,purpose)<-either (ioError . userError) pure (canvasControl (cachedCanvas before) value)
                atomically $ do
                  modifyTVar' cache (\c->c {cachedCanvas=updated,pendingBinary=purpose,cachedMeta=if kind=="canvas-reset" then KM.delete "canvas" (cachedMeta c) else cachedMeta c})
                  case purpose of Nothing->emit packet;Just _->pure ()
              Just "open-resource" -> atomically (emit packet)
              Just "download" -> atomically $ modifyTVar' cache (\c->c {pendingBinary=Just (DownloadBytes packet)})
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
                  modifyTVar' cache (\c->c {sessionClosed=True,cachedCanvas=emptyCanvas,cachedMeta=KM.delete "canvas" (cachedMeta c)})
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
          -- Subscribe between binary pairs; a current PNG prefix is replayed
          -- before the queue receives its remaining tail.
          case pendingBinary current of Nothing->pure ();Just _->retry
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
                atomically (writeTBQueue queue [JsonPacket (object ["type" .= ("detached"::T.Text)])])
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
                Right packets->mapM_ (sendPacket conn) packets >> pure (last packets)
              case packet of
                JsonPacket value | messageType value `elem` [Just "closed",Just "detached"] -> void (tryPutMVar done ())
                _->pure ()
        finally (do
          sendPacket conn (JsonPacket (object ["type" .= ("remote"::T.Text),"host" .= host]))
          mapM_ (sendPacket conn . JsonPacket) (cachedAssets snapshot)
          mapM_ (sendPacket conn) (canvasReplay (cachedCanvas snapshot))
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
