-- | Foreground/background and the font traits shared by display frontends.
-- For 24-bit colors, @textStyleFromAttr (textStyleAttr s) == s@. Conversion
-- projects bold/italic only; unrelated terminal attributes remain owned by Vty.
module Hide.TextStyle
  ( TextStyle(..), textFlags, textStyleFromAttr, textStyleAttr ) where

import Data.Bits ((.&.), (.|.), shiftL)
import qualified Graphics.Vty as V

-- | Small immutable paint metadata. No text, layout or backend handles.
data TextStyle = TextStyle
  { textForeground :: !Int, textBackground :: !Int
  , textBold :: !Bool, textItalic :: !Bool } deriving (Eq,Show)

-- | Stable transport/FFI flags; independent of Vty's internal bit allocation.
textFlags :: TextStyle -> Int
textFlags style=(if textBold style then 1 else 0)+(if textItalic style then 2 else 0)

-- | Project colors and existing font attributes without changing geometry.
textStyleFromAttr :: V.Attr -> TextStyle
textStyleFromAttr attr=TextStyle (rgb (V.attrForeColor attr)) (rgb (V.attrBackColor attr))
  (V.styleMask attr .&. V.bold/=0) (V.styleMask attr .&. V.italic/=0)
  where
    rgb (V.SetTo (V.RGBColor r g b))=fromIntegral r `shiftL` 16 .|. fromIntegral g `shiftL` 8 .|. fromIntegral b
    rgb (V.SetTo (V.ISOColor n))=[0,0xaa0000,0x00aa00,0xaa5500,0x0000aa,0xaa00aa,0x00aaaa,0xaaaaaa,0x555555,0xff5555,0x55ff55,0xffff55,0x5555ff,0xff55ff,0x55ffff,0xffffff] !! (fromIntegral n `mod` 16)
    rgb _=0

-- | Reconstruct ordinary terminal attributes from validated paint metadata.
textStyleAttr :: TextStyle -> V.Attr
textStyleAttr style=foldl V.withStyle colored
  ([V.bold | textBold style]++[V.italic | textItalic style])
  where
    rgb n=V.RGBColor (fromIntegral (n `div` 65536)) (fromIntegral (n `div` 256)) (fromIntegral n)
    colored=V.defAttr `V.withForeColor` rgb (textForeground style) `V.withBackColor` rgb (textBackground style)
