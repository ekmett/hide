{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : RemoteWebCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module RemoteWebCheck (checks) where
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait, waitCatch)
import Control.Concurrent.STM (newTChanIO, atomically, readTChan, writeTChan)
import Control.Exception (bracket, bracket_)
import Control.Monad (unless, void)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (isInfixOf, stripPrefix)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import qualified Network.WebSockets as WS
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (stderr, openTempFile, hClose, hFlush, hFileSize, hSeek, SeekMode(AbsoluteSeek), hGetLine)
import System.Timeout (timeout)
import Hide.Protocol
import Hide.Remote (RemotePeer(..))
import Hide.RemoteWeb (runRemoteWeb)

checks :: IO ()
checks = do
  oldOpen<-lookupEnv "THC_EDIT_WEB_OPEN"
  bracket_ (setEnv "THC_EDIT_WEB_OPEN" "0") (maybe (unsetEnv "THC_EDIT_WEB_OPEN") (setEnv "THC_EDIT_WEB_OPEN") oldOpen) $ do
    temp<-getTemporaryDirectory
    bracket (openTempFile temp "hide-canvas-relay-check") (\(path,h)->hClose h >> removeFile path) $ \(_,logHandle)->
      bracket (hDuplicate stderr) hClose $ \saved->bracket_ (hDuplicateTo logHandle stderr) (hFlush stderr >> hDuplicateTo saved stderr) $ do
        queue<-newTChanIO
        let feed packets=atomically (mapM_ (writeTChan queue . Just) packets)
            peer=RemotePeer (const (pure ())) (const (pure ())) (atomically (readTChan queue))
            epoch=T.replicate 48 "a";ident=T.replicate 48 "b";other=T.replicate 48 "c"
            control kind fields=JsonPacket (object (["type" .= (kind::T.Text),"epoch" .= epoch]++fields))
            reset=control "canvas-reset" []
            resource key=control "canvas-resource" ["id" .= key,"width" .= (2::Int),"height" .= (2::Int),"bytes" .= (16::Int)]
            chunk key offset bytes=[control "canvas-chunk" ["id" .= key,"offset" .= (offset::Int),"length" .= BS.length bytes],BinaryPacket bytes]
            release key=control "canvas-release" ["id" .= key]
            rows=replicate 12 (toJSON ([]::[Value]))
            scene target=object ["epoch" .= epoch,"surfaces" .= [object ["id" .= (1::Int),"resource" .= ident,"slot" .= (1::Int),"rect" .= ([0,0,2,2]::[Int]),"target" .= (target::[Double]),"name" .= ("safe.png"::T.Text),"description" .= ("2 by 2"::T.Text)]],"mask" .= ("AQAA"<>T.replicate 1276 "A")]
            frame first canvas=BinaryPacket (BL.toStrict (framePacket first (if first then [] else rows) rows ["size" .= ([40,12]::[Int]),"canvas" .= canvas]))
            payload=BS.pack [0..15]
        withAsync (runRemoteWeb 1 "safe-host" peer) $ \server->do
          feed ([JsonPacket (object ["type" .= ("assets"::T.Text)]),reset,resource ident]++chunk ident 0 (BS.take 4 payload)++[frame True (scene [0,0,2,2])])
          url<-bounded "browser URL" (awaitURL logHandle)
          let address=drop 7 url;(host,portPath)=break (==':') address;(portText,pathText)=break (=='/') (drop 1 portPath)
              client action=WS.runClientWith host (read portText) (pathText++"socket") WS.defaultConnectionOptions [("Origin",B8.pack ("http://"++host++":"++portText))] $ action
              attach action=client action >> threadDelay 50000
          attach $ \conn->do
            reader<-newReader
            (_,resources)<-readFrame conn reader
            check "first client retains the exact unfinished prefix" (M.lookup ident resources==Just (BS.take 4 payload))
          attach $ \conn->do
            reader<-newReader
            (_,resources)<-readFrame conn reader
            check "reconnect replays the active resource header and prefix" (M.lookup ident resources==Just (BS.take 4 payload))
            feed ([JsonPacket (object ["type" .= ("copy"::T.Text),"text" .= ("safe copy"::T.Text)])]++chunk ident 4 (BS.drop 4 payload)++[frame False (scene [1,0,4,4])])
            (_,complete)<-readFrame conn reader
            check "tail follows replay prefix without result interleaving" (M.lookup ident complete==Just payload)
          attach $ \conn->do
            reader<-newReader
            (_,resources)<-readFrame conn reader
            check "completed replay preserves exact immutable bytes" (M.lookup ident resources==Just payload)
            feed [frame False (scene [-1,0,4,4])]
            (_,panned)<-readFrame conn reader
            begins<-readIORef (third reader)
            check "pan-only frame does not begin or retransmit a resource" (panned==resources && begins==1)
            feed ([resource other]++chunk other 0 (BS.take 4 payload)++[release other,release ident,frame False (object ["epoch" .= epoch,"surfaces" .= ([]::[Value]),"mask" .= T.replicate 1280 "A"])])
            (_,released)<-readFrame conn reader
            check "release cancels active and completed resources" (M.null released)
          client $ \conn->do
            reader<-newReader
            (_,resources)<-readFrame conn reader
            check "reconnect cannot replay retired resources" (M.null resources)
            feed [JsonPacket (object ["type" .= ("closed"::T.Text)])]
            void (WS.receiveDataMessage conn)
          bounded "relay completion" (wait server)
        let reject packets=do
              badQueue<-newTChanIO
              let badPeer=RemotePeer (const (pure ())) (const (pure ())) (atomically (readTChan badQueue))
              withAsync (runRemoteWeb 1 "safe-host" badPeer) $ \server->do
                atomically (mapM_ (writeTChan badQueue . Just) packets)
                result<-bounded "malformed relay refusal" (waitCatch server)
                check "late or old-epoch chunk cannot resurrect" (case result of Left err->"canvas" `isInfixOf` show err||"Canvas" `isInfixOf` show err;Right _->False)
        reject ([reset,resource ident]++chunk ident 0 (BS.take 4 payload)++[release ident,control "canvas-chunk" ["id" .= ident,"offset" .= (4::Int),"length" .= (12::Int)]])
        reject [reset,JsonPacket (object ["type" .= ("canvas-resource"::T.Text),"epoch" .= T.replicate 48 "d","id" .= ident,"width" .= (2::Int),"height" .= (2::Int),"bytes" .= (16::Int)])]
  putStrLn "remote canvas prefix/completed replay, binary pairs, pan retention and cancellation checks passed"
  where
    check name good=unless good (error name)
    bounded name action=timeout 10000000 action >>= maybe (error (name++" timed out")) pure
    awaitURL handle=do
      hFlush stderr
      size<-hFileSize handle
      if size==0 then threadDelay 10000 >> awaitURL handle else do
        hSeek handle AbsoluteSeek 0
        line<-hGetLine handle
        maybe (error ("Missing browser URL: "++line)) pure (stripPrefix "Haskell browser: " line)
    third (_,_,value)=value
    newReader :: IO (IORef (M.Map T.Text BS.ByteString),IORef [Value],IORef Int)
    newReader=(,,) <$> newIORef M.empty <*> newIORef [] <*> newIORef (0::Int)
    readFrame conn reader@(resources,rows,begins)=do
      packet<-bounded "browser packet" (WS.receiveDataMessage conn)
      case packet of
        WS.Text bytes _->do
          value<-either error pure (eitherDecode bytes)
          kind<-field "type" value
          case (kind::T.Text) of
            "canvas-reset"->writeIORef resources M.empty
            "canvas-resource"->do
              modifyIORef' begins (+1)
              field "id" value >>= \ident->modifyIORef' resources (M.insert ident BS.empty)
            "canvas-release"->field "id" value >>= \ident->modifyIORef' resources (M.delete ident)
            "canvas-chunk"->do
              ident<-field "id" value;offset<-field "offset" value;count<-field "length" value
              binary<-bounded "contiguous canvas binary" (WS.receiveDataMessage conn)
              payload<-case binary of WS.Binary payload->pure (BL.toStrict payload);_->error "Result interleaved within a canvas pair"
              previous<-M.findWithDefault BS.empty ident <$> readIORef resources
              check "contiguous exact chunk" (BS.length previous==offset && BS.length payload==count)
              modifyIORef' resources (M.insert ident (previous<>payload))
            _->pure ()
          readFrame conn reader
        WS.Binary bytes->do
          previous<-readIORef rows
          (value,current)<-decodeFrame previous (BL.toStrict bytes)
          writeIORef rows current
          retained<-readIORef resources
          pure (value,retained)
    field :: FromJSON a => Key -> Value -> IO a
    field key value=either error pure (parseEither (withObject "metadata" (\o->o .: key)) value)
