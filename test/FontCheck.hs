module FontCheck (checks) where

import Control.Monad (forM_, unless)
import Data.Bits (testBit)
import Data.Maybe (listToMaybe)
import THC.Edit.Font

checks :: IO ()
checks = do
  font <- loadFont
  let check name ok = unless ok (error name)
      rows c = glyphRows (glyph font c)
      left c = map (`testBit` 15) (rows c)
      right c = map (`testBit` 8) (rows c)
      missing = glyph font '\x10ffff'
  check "ASCII uses original IBM pixels, aligned to bit 15"
    (rows 'A' == map (* 256) [0,0,16,56,108,198,198,254,198,198,198,198,0,0,0,0])
  check "lambda is a distinct, visible IBM glyph"
    (rows 'λ' /= rows 'A' && glyph font 'λ' /= missing && any (/= 0) (rows 'λ'))
  check "space remains blank" (all (== 0) (rows ' '))
  check "CJK fallback occupies two cells"
    (glyphWidth (glyph font '中') == 16 && any (/= 0) (rows '中') && glyph font '中' /= missing)
  check "missing code point is visibly marked" (any (/= 0) (glyphRows missing))
  forM_ "Aλ中 ─│═║┌┐└┘╔╗╚╝\x10ffff" $ \c -> do
    let g = glyph font c
    check "glyph dimensions are cell sized" (length (glyphRows g) == 16 && glyphWidth g `elem` [8,16])
  forM_ [('┌','─','┐','│','└','┘'), ('╔','═','╗','║','╚','╝')] $
    \(tl,h,tr,v,bl,br) -> do
      check "horizontal border fills its stroke through both cell edges"
        (left h == right h && any id (left h))
      check "top corners meet horizontal border" (right tl == left h && right h == left tr)
      check "bottom corners meet horizontal border" (right bl == left h && right h == left br)
      check "vertical border reaches top and bottom" (listToMaybe (rows v) == Just (last (rows v)) && last (rows v) /= 0)
      check "left corners meet vertical border" (Just (last (rows tl)) == listToMaybe (rows v) && Just (last (rows v)) == listToMaybe (rows bl))
      check "right corners meet vertical border" (Just (last (rows tr)) == listToMaybe (rows v) && Just (last (rows v)) == listToMaybe (rows br))
  putStrLn "font checks passed"
