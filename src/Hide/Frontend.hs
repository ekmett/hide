-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- |
-- Module      : Hide.Frontend
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : GHC2021
--
-- Pure launch-option selection and the shared native event encoding.
--
-- Explicit options override environment values. Native key codes and modifier bits
-- map to the editor input model, including selected macOS Command bindings. SSH
-- path recognition distinguishes scp-style targets from drive-letter and explicitly
-- relative local paths; it is not a shell command parser.
module Hide.Frontend (Backend(..), chooseBackend, chooseScale, parseWindowSize, parseScreenMode, modeSize, modeHeight, parseRemoteTarget, decodeKey, zoomDirection) where
import Data.Bits ((.&.))
import Data.Char (chr, isDigit)
import Text.Read (readMaybe)
import Data.List (nub)
import qualified Graphics.Vty as V

data Backend = Terminal | Auto | Metal | Vulkan | Web | Remote deriving (Eq,Show)

-- Borland TextMode constants: C80 (3), C80 + Font8x8 (259).
parseScreenMode :: String -> Either String Int
parseScreenMode value = case readMaybe (case value of '$':xs -> "0x" ++ xs; _ -> value) of
  Just n | n `elem` [3,259] -> Right n
  _ -> Left "--mode needs 3 (80x25) or 259 (80x50); hexadecimal 0x03/0x103 also works."

modeSize :: Int -> (Int,Int)
modeSize mode = (80, if mode == 259 then 50 else 25)

modeHeight :: Int -> Int
modeHeight mode = if mode == 259 then 8 else 16

-- | Choose the explicit frontend before considering an environment/default value.
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
    Just "web" -> Right Web
    Just "remote" -> Right Remote
    Just _ -> Left "THC_EDIT_BACKEND must be terminal, auto, metal, vulkan, web or remote."
  _ -> Left "Choose only one of --terminal, --window, --metal, --vulkan, --web or --remote."

-- | Decode native scalar/special-key codes and modifier bits into editor input.
decodeKey :: Int -> Int -> Maybe V.Event
decodeKey key mask = fmap (`V.EvKey` mods) decoded
  where
    mods = [V.MShift | mask .&. 1 /= 0] ++ [V.MCtrl | mask .&. 2 /= 0] ++ [V.MAlt | mask .&. 4 /= 0] ++ [V.MMeta | mask .&. 8 /= 0]
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

-- | Choose explicit or environment scale; round valid numeric scales to eighths.
-- Zero denotes automatic selection when no scale is supplied.
chooseScale :: Maybe String -> [String] -> Either String Double
chooseScale env explicit = case explicit of
  [] -> maybe (Right 0) parse (env >>= \s -> if null s then Nothing else Just s)
  [value] -> parse value
  _ -> Left "Specify --scale only once."
  where
    parse value = case readMaybe value :: Maybe Double of
      Just n | n>=1 && n<=8 -> Right (fromIntegral (round (n*8) :: Int)/8)
      _ -> Left "--scale or THC_EDIT_SCALE needs a number from 1 to 8 (rounded to 1/8 steps)."

-- Raw SDL modifier bits: Control or Alt, matching zoom in/out and reset.
zoomDirection :: Int -> Int -> Maybe Int
zoomDirection key mask
  | mask .&. 6 == 0 = Nothing
  | key==fromEnum '0' = Just 0
  | key `elem` map fromEnum ['+','='] = Just 1
  | key==fromEnum '-' = Just (-1)
  | otherwise = Nothing

-- | Recognize host:path while keeping drive-letter and explicitly relative paths local.
parseRemoteTarget :: FilePath -> Maybe (String,FilePath)
parseRemoteTarget (_:':':slash:_) | slash `elem` "/\\" = Nothing
parseRemoteTarget target = case break (==':') target of
  (host,':':path) | not (null host) && all (`notElem` "/\\") host -> Just (host,if null path then "." else path)
  _ -> Nothing
