{-# LANGUAGE OverloadedStrings #-}
module ProtocolCheck (checks) where
import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, forM_)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO
import THC.Edit.Protocol
import THC.Edit.Model
import THC.Edit.Buffer

checks :: IO ()
checks = do
  let check name ok=unless ok (error name)
      rejects name action=do
        result<-try action :: IO (Either SomeException ())
        check name (either (const True) (const False) result)
  dir<-getTemporaryDirectory
  bracket (openBinaryTempFile dir "thc-wire") (\(path,h)->hClose h >> removeFile path) $ \(_,h)->do
    let packets=[JsonPacket (object ["text" .= ("λ 👩🏽\x200d\&💻"::T.Text)]),BinaryPacket (BS.pack [0,1,2,255])]
    mapM_ (writePacket h) packets
    hSeek h AbsoluteSeek 0
    actual<-sequence [readPacket h,readPacket h,readPacket h]
    check "binary and Unicode packets round trip with clean EOF" (actual==map Just packets++[Nothing])
    forM_ [BS.pack [0],BS.pack [0,0,0,3,0,123],BS.pack [255,255,255,255],BS.pack [0,0,0,1,9]] $ \bad->do
      hSetFileSize h 0; hSeek h AbsoluteSeek 0; BS.hPut h bad; hSeek h AbsoluteSeek 0
      rejects "truncated, oversized and unknown-kind packets fail" (readPacket h >> pure ())
  let d=addDocument Nothing (newBuffer "λ\nhello") (initialDesktop (80,25))
      screens=map frameRows [d,insertText "world " d,d {screenSize=(100,30)}]
  _<-foldFrames check [] screens
  rejects "unknown display encoding rejected" (decodeFrame [] (BS.pack [9]) >> pure ())
  rejects "bad compressed stream rejected" (decodeFrame [] (BS.pack [0,255,255]) >> pure ())
  putStrLn "protocol checks passed"
  where
    foldFrames _ old []=pure old
    foldFrames check old (rows:rest)=do
      let reset=null old || length old/=length rows
      (_,actual)<-decodeFrame old (BL.toStrict (framePacket reset old rows ["size" .= (80::Int,length rows)]))
      _<-check "display reset and dictionary patches reconstruct identical cells" (actual==rows)
      foldFrames check actual rest
