-- |
-- Module      : Hide.TextStyle
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : GHC2021
--
-- Packed colors and four font traits shared by display frontends.
-- For 24-bit colors and flags drawn from bold1/italic2/underline8/strike16,
-- @textStyleFromAttr (textStyleAttr s) == s@. Bit4 belongs to explicit glyph
-- width, not paint. Other terminal attributes remain owned by Vty.
module Hide.TextStyle
  ( TextStyle(..), textBold, textItalic, textUnderline, textStrikethrough
  , textStyleFromAttr, textStyleAttr ) where

import Data.Bits ((.&.), (.|.), shiftL)
import qualified Graphics.Vty as V

-- | Three unpacked scalar fields; no text, layout or backend handles.
-- Flags use transport/FFI bits, independent of Vty's internal allocation.
data TextStyle = TextStyle
  { textForeground :: {-# UNPACK #-} !Int
  , textBackground :: {-# UNPACK #-} !Int
  , textFlags :: {-# UNPACK #-} !Int } deriving (Eq,Show)

-- | Whether the packed paint requests bold text.
textBold :: TextStyle -> Bool
textBold style=textFlags style .&. 1/=0

-- | Whether the packed paint requests italic text.
textItalic :: TextStyle -> Bool
textItalic style=textFlags style .&. 2/=0

-- | Whether the packed paint requests an underline.
textUnderline :: TextStyle -> Bool
textUnderline style=textFlags style .&. 8/=0

-- | Whether the packed paint requests strikethrough text.
textStrikethrough :: TextStyle -> Bool
textStrikethrough style=textFlags style .&. 16/=0

-- | Project colors and all four supported traits without changing geometry.
textStyleFromAttr :: V.Attr -> TextStyle
textStyleFromAttr attr=TextStyle (rgb (V.attrForeColor attr)) (rgb (V.attrBackColor attr))
  (flag V.bold 1 .|. flag V.italic 2 .|. flag V.underline 8 .|. flag V.strikethrough 16)
  where
    flag trait bit=if V.styleMask attr .&. trait/=0 then bit else 0
    rgb (V.SetTo (V.RGBColor r g b))=fromIntegral r `shiftL` 16 .|. fromIntegral g `shiftL` 8 .|. fromIntegral b
    rgb (V.SetTo (V.ISOColor n))=[0,0xaa0000,0x00aa00,0xaa5500,0x0000aa,0xaa00aa,0x00aaaa,0xaaaaaa,0x555555,0xff5555,0x55ff55,0xffff55,0x5555ff,0xff55ff,0x55ffff,0xffffff] !! (fromIntegral n `mod` 16)
    rgb _=0

-- | Restore validated paint to terminal attributes. Explicit glyph width is
-- handled by the span, so transport bit4 never becomes a Vty underline.
textStyleAttr :: TextStyle -> V.Attr
textStyleAttr style=if traits==0 then colored else V.withStyle colored traits
  where
    traits=flag (textBold style) V.bold .|. flag (textItalic style) V.italic
      .|. flag (textUnderline style) V.underline .|. flag (textStrikethrough style) V.strikethrough
    flag enabled trait=if enabled then trait else 0
    rgb n=V.RGBColor (fromIntegral (n `div` 65536)) (fromIntegral (n `div` 256)) (fromIntegral n)
    colored=V.defAttr `V.withForeColor` rgb (textForeground style) `V.withBackColor` rgb (textBackground style)
