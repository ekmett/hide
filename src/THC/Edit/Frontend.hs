module THC.Edit.Frontend (Backend(..), chooseBackend, parseWindowSize, decodeKey) where
import Data.Bits ((.&.))
import Data.Char (chr, isDigit)
import Text.Read (readMaybe)
import Data.List (nub)
import qualified Graphics.Vty as V

data Backend = Terminal | Auto | Metal | Vulkan deriving (Eq,Show)

chooseBackend :: Maybe String -> [Backend] -> Either String Backend
chooseBackend env explicit = case nub explicit of
  [backend] -> Right backend
  [] -> case env of
    Nothing -> Right Terminal
    Just "" -> Right Terminal
    Just "terminal" -> Right Terminal
    Just "auto" -> Right Auto
    Just "metal" -> Right Metal
    Just "vulkan" -> Right Vulkan
    Just _ -> Left "THC_EDIT_BACKEND must be terminal, auto, metal or vulkan."
  _ -> Left "Choose only one of --terminal, --window, --metal or --vulkan."

-- Stable, small ABI shared with cbits/window.c; no SDL structure layout in Haskell.
decodeKey :: Int -> Int -> Maybe V.Event
decodeKey key mask = fmap (`V.EvKey` mods) decoded
  where
    mods = [V.MShift | mask .&. 1 /= 0] ++ [V.MCtrl | mask .&. 10 /= 0] ++ [V.MAlt | mask .&. 4 /= 0]
    decoded
      | key >= 0 && key <= 0x10ffff && not (key >= 0xd800 && key <= 0xdfff) = Just (V.KChar (chr key))
      | key <= -101 && key >= -124 = Just (V.KFun (-key-100))
      | otherwise = lookup key [(-1,V.KUp),(-2,V.KDown),(-3,V.KLeft),(-4,V.KRight),(-5,V.KHome),(-6,V.KEnd),(-7,V.KPageUp),(-8,V.KPageDown),(-9,if mask .&. 1 /= 0 then V.KBackTab else V.KChar '\t'),(-10,V.KEnter),(-11,V.KEsc),(-12,V.KBS),(-13,V.KDel),(-14,V.KIns)]

parseWindowSize :: String -> Either String (Int,Int)
parseWindowSize value = case break (== 'x') value of
  (w,'x':h) | all isDigit w && all isDigit h,
              Just cols <- readMaybe w, Just rows <- readMaybe h,
              cols >= 40, cols <= 512, rows >= 12, rows <= 256 -> Right (cols,rows)
  _ -> Left "--size needs COLSxROWS (40..512 columns, 12..256 rows), e.g. 80x25."
