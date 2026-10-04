{-# LANGUAGE OverloadedStrings #-}
module WideTextCheck (checks) where
import Control.Monad (unless,forM_)
import Data.Aeson (Value(..),object,toJSON,(.=))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Graphics.Vty.Span (SpanOp(..))
import Blaze.ByteString.Builder (writeToByteString)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Hide.Unicode
import Hide.RemoteWindow
import Hide.RemoteTerminal (remoteTerminalPicture)

checks :: IO ()
checks=do
 let check name ok=unless ok (fail name)
     ops image=Vec.toList (displayOpsForPic (V.picForImage image) (8,1) Vec.! 0)
     glyphs image=[(TL.toStrict text,width) | TextSpan _ width _ text<-ops image]
     original=wideTextImage V.defAttr "A"
 check ("compositor retains original narrow text and two-cell advance: "++show (glyphs original)) (take 1 (glyphs original)==[("A",2)])
 check "flattening preserves explicit advances" (displayOpsForPic (flattenPicture (8,1) (V.picForImage original)) (8,1)==displayOpsForPic (V.picForImage original) (8,1))
 check "partial wide crop becomes blank" (all (not . T.isInfixOf "A" . fst) (glyphs (V.cropRight 1 original)))
 let covered=V.picForLayers [V.translateX 1 (textImage V.defAttr "X"),original]
 check "covering either wide half clears the full glyph" (all (not . T.isInfixOf "A") [TL.toStrict text | row<-Vec.toList (displayOpsForPic covered (8,1)),TextSpan _ _ _ text<-Vec.toList row])
 forM_ [("A","Ａ"),(" ","　"),("é","ｅ́"),("é","é "),("界","界"),("👩🏽\x200d\&💻","👩🏽\x200d\&💻")] $ \(semantic,projected)->do
   check "all title graphemes occupy two cells" (V.imageWidth (wideTextImage V.defAttr semantic)==2)
   let (bytes,end)=terminalSpan (const mempty) 0 2 semantic
   check "terminal projection reserves two cells and keeps semantic text separate" (end==2 && TE.encodeUtf8 projected `BS.isInfixOf` writeToByteString bytes)
 let metadata=object ["size" .= ([40,12]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)])]
     row=toJSON [(0::Int,0xffffff::Int,0::Int,3::Int,[toJSON ("A"::T.Text,2::Int,True)])]
 frame<-either fail pure (parseRemoteFrame metadata (row:replicate 11 (toJSON ([]::[Value]))))
 check "native receiver keeps stretched semantic glyph width" (case remoteCells frame of [RemoteCell 0 0 _ "A" 2]->True; _->False)
 check "remote TUI retains the same original glyph/advance before output projection" (case [ (TL.toStrict text,width) | rowOps<-Vec.toList (displayOpsForPic (remoteTerminalPicture frame) (40,12)),TextSpan _ width _ text<-Vec.toList rowOps,text=="A"] of [("A",2)]->True; _->False)
 let bad flag advance text=toJSON [(0::Int,0::Int,0::Int,0::Int,[toJSON (text::T.Text,advance::Int,flag::Bool)])]
 check "invalid stretched wire glyphs refuse instead of changing geometry" (all (either (const True) (const False) . parseRemoteFrame metadata . (:replicate 11 (toJSON ([]::[Value])))) [bad True 1 "A",bad True 2 "界",bad True 2 "ab",bad False 2 "A"])
 putStrLn "wide semantic glyph compositor and frontend checks passed"
