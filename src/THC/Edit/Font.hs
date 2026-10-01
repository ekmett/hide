module THC.Edit.Font (Font, Glyph(..), loadFont, glyph, bitmapGlyph) where

import Data.Word (Word16)
import Control.Monad (unless)
import Data.Bits (shiftL)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isHexDigit, ord)
import qualified Data.IntMap.Strict as IM
import Numeric (readHex)
import Paths_thc_edit (getDataFileName)

data Font = Font (IM.IntMap Glyph) (IM.IntMap Glyph)
data Glyph = Glyph { glyphWidth :: Int, glyphRows :: [Word16] }
  deriving (Eq, Show)

loadFont :: IO Font
loadFont = do
  ibm <- load "assets/fonts/ibm-vga-8x16.hex"
  unicode <- load "assets/fonts/unifont-18.0.01.hex"
  pure (Font (IM.union ibm unicode) ibm)
  where
    load name = do
      path <- getDataFileName name
      bytes <- BS.readFile path
      pairs <- mapM (parse path) (BS.lines bytes)
      unless (not (null pairs) && strictlyAscending (map fst pairs)) $
        ioError (userError ("Invalid bitmap font order: " ++ path))
      pure (IM.fromDistinctAscList pairs)
    strictlyAscending xs = and (zipWith (<) xs (drop 1 xs))
    parse path line = case BS.split ':' line of
      [code, bits]
        | BS.length code >= 4 && BS.length code <= 6
        , BS.length bits `elem` [32,64]
        , BS.all isHexDigit code && BS.all isHexDigit bits
        , Just point <- hex (BS.unpack code)
        , point <= 0x10ffff ->
            let width = BS.length bits `div` 4
                digits = width `div` 4
                rows = traverse (hex . BS.unpack . BS.take digits . (`BS.drop` bits)) [0,digits .. BS.length bits - digits]
            in case rows of
              Just rs -> pure (point, Glyph width (map (fromIntegral . (`shiftL` (16 - width))) rs))
              Nothing -> invalid path
      _ -> invalid path
    invalid path = ioError (userError ("Invalid bitmap font data: " ++ path))
    hex s = case readHex s :: [(Int, String)] of
      [(n, "")] -> Just n
      _ -> Nothing

glyph :: Font -> Char -> Glyph
glyph (Font font _) c
  | ord c>=0xe000 && ord c<=0xe007 = Glyph 8 (map (*256) (corners !! (ord c-0xe000)))
  | otherwise = IM.findWithDefault missing (ord c) font
  where
    -- Rounded half-cell corners, mirrored vertically for the bottom edges.
    tl=[7,31,63,127,127,255,255,255]++replicate 8 255
    tr=[224,248,252,254,254,255,255,255]++replicate 8 255
    bl=reverse tl; br=reverse tr
    corners=[tl,tr,bl,br,zipWith min tl bl,zipWith min tr br,
             [255,127,63,31,15,7,3,1]++replicate 8 0,
             [255,254,252,248,240,224,192,128]++replicate 8 0]
    missing = IM.findWithDefault (Glyph 8 (replicate 16 0xff00)) 0xfffd font

-- Interface geometry and the original IBM repertoire retain their exact pixels.
bitmapGlyph :: Font -> Char -> Bool
bitmapGlyph (Font _ ibm) c = IM.member (ord c) ibm || ord c>=0x2500 && ord c<=0x259f || ord c>=0xe000 && ord c<=0xe007
