{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.ScreenCapture (capture, screenTool) where

import Codec.Picture (PixelRGB8(..), encodePng, generateImage)
import Data.Aeson
import Data.Bits (testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Graphics.Vty.Span (SpanOp(..))
import THC.Edit.Font
import THC.Edit.Frontend (modeHeight)
import THC.Edit.Model (Desktop(..))
import THC.Edit.Render (renderDesktop)
import THC.Edit.Unicode (clusterWidth, displayOpsForPic, graphemes)

screenTool :: Value
screenTool=object
  ["name" .= ("editor_screen"::Text),
   "description" .= ("Capture the complete editor frame as colorless text, with an optional PNG bitmap preview from that same frame. Includes cursor and grid geometry; excludes OS chrome, CRT effects and native Unicode shaping."::Text),
   "inputSchema" .= object ["type" .= ("object"::Text),"properties" .= object
     ["image" .= object ["type" .= ("boolean"::Text),"default" .= False]],"additionalProperties" .= False],
   "annotations" .= object ["readOnlyHint" .= True,"destructiveHint" .= False,"openWorldHint" .= False]]

-- Both representations use one flattened picture. This is a logical editor
-- framebuffer, independent of a window server or the currently visible frontend.
capture :: Font -> Desktop -> Bool -> IO (Either Text Value)
capture font desktop includeImage
  | cols<=0 || rows<=0 || toInteger cols*toInteger rows>32768 = pure (Left "Editor screen exceeds the 32768-cell capture limit.")
  | includeImage && toInteger cols*8*toInteger rows*toInteger cellHeight>4194304 = pure (Left "Editor image exceeds the 4-megapixel capture limit.")
  | BS.length (TE.encodeUtf8 plain)>1048576 = pure (Left "Editor screen text exceeds 1 MiB.")
  | includeImage && BL.length png>2097152 = pure (Left "Editor PNG exceeds 2 MiB; request text only or reduce the editor size.")
  | BL.length (encode result)>4194304 = pure (Left "Editor screen result exceeds 4 MiB.")
  | otherwise = pure (Right result)
  where
    (cols,rows)=screenSize desktop
    cellHeight=modeHeight (fromMaybe 3 (videoMode desktop))
    picture=renderDesktop desktop
    spans=map toList (toList (displayOpsForPic picture (cols,rows)))
    plain=T.unlines [T.concat (map plainSpan line) | line<-spans]
    plainSpan TextSpan{textSpanText=t}=TL.toStrict t
    plainSpan (Skip n)=T.replicate n " "
    plainSpan (RowEnd n)=T.replicate n " "
    cursor=case V.picCursor picture of
      V.Cursor x y | x>=0 && x<cols && y>=0 && y<rows -> Just (x,y)
      _ -> Nothing
    metadata=object ["cols" .= cols,"rows" .= rows,"text" .= plain,
      "cursor" .= fmap (\(x,y) -> object ["x" .= x,"y" .= y]) cursor,
      "cursorPolicy" .= ("visible underline; blink phase fixed on"::Text),
      "cellWidth" .= (8::Int),"cellHeight" .= cellHeight,"pixelWidth" .= (cols*8),"pixelHeight" .= (rows*cellHeight),
      "videoMode" .= fromMaybe 3 (videoMode desktop),"imageIncluded" .= includeImage,
      "renderer" .= ("canonical IBM/Unicode bitmap approximation; multi-codepoint graphemes use their first code point"::Text),
      "excluded" .= (["OS chrome","CRT effects","native font shaping","mouse pointer"]::[Text])]
    result=object ["isError" .= False,"content" .=
      (object ["type" .= ("text"::Text),"text" .= TE.decodeUtf8 (BL.toStrict (encode metadata))] :
        [object ["type" .= ("image"::Text),"mimeType" .= ("image/png"::Text),
          "data" .= TE.decodeUtf8 (B64.encode (BL.toStrict png))] | includeImage])]
    blank=(glyph font ' ',PixelRGB8 0 0 0,PixelRGB8 0 0 0,0)
    cells=Vec.fromList (concatMap (take cols . (++repeat blank) . concatMap tiles) spans)
    tiles TextSpan{textSpanAttr=attr,textSpanText=t}=concatMap (cluster attr) (graphemes (TL.toStrict t))
    tiles (Skip n)=replicate n blank
    tiles (RowEnd n)=replicate n blank
    cluster attr text=case T.uncons text of
      Nothing -> []
      Just (c,_) -> let tile=glyph font c
                   in [(tile,color (V.attrForeColor attr),color (V.attrBackColor attr),offset*8) | offset<-[0..clusterWidth text-1]]
    png=encodePng (generateImage pixel (cols*8) (rows*cellHeight))
    pixel x y=let (tile,fg,bg,offset)=cells Vec.! ((y `div` cellHeight)*cols+x `div` 8)
                  gx=offset+x `mod` 8
                  gy=(y `mod` cellHeight)*16 `div` cellHeight
                  ink=gx<glyphWidth tile && gx<16 && testBit (glyphRows tile !! gy) (15-gx)
                  base=if ink then fg else bg
              in if cursor==Just (x `div` 8,y `div` cellHeight) && y `mod` cellHeight>=cellHeight*14 `div` 16
                   then invert base else base
    invert (PixelRGB8 r g b)=PixelRGB8 (255-r) (255-g) (255-b)

color :: V.MaybeDefault V.Color -> PixelRGB8
color (V.SetTo (V.RGBColor r g b))=PixelRGB8 r g b
color (V.SetTo (V.ISOColor n))=palette !! (fromIntegral n `mod` 16)
  where palette=[PixelRGB8 0 0 0,PixelRGB8 170 0 0,PixelRGB8 0 170 0,PixelRGB8 170 85 0,
          PixelRGB8 0 0 170,PixelRGB8 170 0 170,PixelRGB8 0 170 170,PixelRGB8 170 170 170,
          PixelRGB8 85 85 85,PixelRGB8 255 85 85,PixelRGB8 85 255 85,PixelRGB8 255 255 85,
          PixelRGB8 85 85 255,PixelRGB8 255 85 255,PixelRGB8 85 255 255,PixelRGB8 255 255 255]
color _=PixelRGB8 0 0 0
