{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.Hex
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Byte-grid layout shared by drawing and hit testing.
--
-- The grid chooses sixteen bytes per row only when the entire layout fits,
-- otherwise eight. Both hexadecimal digits and the printable view carry the same
-- absolute byte offset, so selection has one source of truth. Input text uses the
-- buffer's Latin-1 byte representation; this module does not decode UTF-8.
module Hide.Hex (hexNumber, hexBytesPerRow, hexWidth, hexAsciiColumn, hexColumn, hexDividers, hexRow, hexRowChunk, hexHit, parseHex) where

import Data.Char (ord, chr, isHexDigit, digitToInt, isSpace, toUpper)
import qualified Data.Text as T
import Data.Text (Text)
import Numeric (showHex)

hexNumber :: Int -> Int -> Text
hexNumber width value = T.justifyRight width '0' (T.pack (map toUpper (showHex value "")))

-- | Choose sixteen or eight bytes from the available character-cell width.
hexBytesPerRow :: Int -> Int
hexBytesPerRow available = if available>=hexWidth 16 then 16 else 8

hexAsciiColumn :: Int -> Int
hexAsciiColumn count = 9+3*count+if count>8 then 1 else 0

hexWidth :: Int -> Int
hexWidth count = hexAsciiColumn count+count

hexColumn :: Int -> Int
hexColumn index = 9+3*index+if index>=8 then 1 else 0

hexDividers :: Int -> [Int]
hexDividers count = [8,hexAsciiColumn count-1]

-- Each cell carries its byte offset, so both panes highlight the same selection.
hexRow :: Int -> Int -> Text -> [(Char,Maybe Int)]
hexRow count row bytes = hexRowChunk count (row*count) (T.take count (T.drop (row*count) bytes))

-- | Render a visible byte slice starting at an absolute offset.
-- Each content cell carries its byte address; borders and padding carry Nothing.
hexRowChunk :: Int -> Int -> Text -> [(Char,Maybe Int)]
hexRowChunk count start chunk = plain (hexNumber 8 start<>"│") ++ concatMap cell [0..count-1] ++ plain "│" ++ concatMap ascii [0..count-1]
  where
    value i = if i<T.length chunk then Just (T.index chunk i) else Nothing
    plain = map (\c -> (c,Nothing)) . T.unpack
    tagged i = map (\c -> (c,Just (start+i))) . T.unpack
    cell i = maybe (plain "  ") (tagged i . hexNumber 2 . ord) (value i) ++ plain (if i==count-1 then "" else if i==7 && count>8 then "  " else " ")
    ascii i = maybe (plain " ") (\c -> tagged i (T.singleton (if c>=' ' && c<='~' then c else '.'))) (value i)

-- | Map a cell column to (byte index, ASCII pane, low hex nibble).
-- The caller supplies the row and checks whether the byte exists.
hexHit :: Int -> Int -> (Int,Bool,Bool)
hexHit count column
  | column>=hexAsciiColumn count = (min (count-1) (column-hexAsciiColumn count),True,False)
  | otherwise = let i=max 0 (min (count-1) ((column-9-if count>8 && column>=34 then 1 else 0) `div` 3))
                in (i,False,column==hexColumn i+1)

-- | Parse whitespace-separated hexadecimal pairs into Latin-1 byte text.
parseHex :: Text -> Either Text Text
parseHex input
  | odd (length digits) || any (not . isHexDigit) digits = Left "Paste hexadecimal byte pairs (for example: 00 FF 41)."
  | otherwise = Right (T.pack (pairs digits))
  where
    digits=filter (not . isSpace) (T.unpack input)
    pairs (a:b:rest)=chr (16*digitToInt a+digitToInt b):pairs rest
    pairs _=[]
