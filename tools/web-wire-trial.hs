{-# LANGUAGE OverloadedStrings #-}
-- Convert web-bandwidth.hs snapshots into actual adaptive wire packets.
-- cabal exec -- runghc -package=thc-edit tools/web-wire-trial.hs < frames.jsonl > packets.jsonl
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as K
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy as Bytes
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import THC.Edit.Web

main :: IO ()
main = BL.getContents >>= go M.empty . BL.lines
  where
    go _ []=pure ()
    go previous (line:rest)=case eitherDecode line >>= parseEither parse of
      Left _ -> go previous rest
      Right (size,phase,rows,meta) -> do
        let (old,oldMeta)=M.findWithDefault ([],K.empty) size previous
            reset=null old
            changed=if reset then K.toList meta else filter (`notElem` K.toList oldMeta) (K.toList meta)
            candidates=frameCandidates reset old rows changed
            packet=framePacket reset old rows changed
        if rows==old && meta==oldMeta then pure () else BL.putStrLn (encode (object ["screen" .= size,"phase" .= phase,"rows" .= rows,
          "candidates" .= map Bytes.unpack candidates,"packet" .= Bytes.unpack packet]))
        go (M.insert size (rows,meta) previous) rest
    parse = withObject "snapshot" $ \o -> do
      size<-o .: "screen"; phase<-o .: "phase"; frame<-o .: "frame"
      pairs<-frame .: "rows"
      pure (size::(Int,Int),phase::T.Text,map snd (pairs::[(Int,Value)]),foldr K.delete frame ["rows","type","reset"])
