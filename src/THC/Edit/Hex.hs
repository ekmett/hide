{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Hex (hexNumber, hexBytesPerRow, hexWidth, hexAsciiColumn, hexColumn, hexDividers, hexRow, hexHit, parseHex) where

import Data.Char (ord, chr, isHexDigit, digitToInt, isSpace, toUpper)
import qualified Data.Text as T
import Data.Text (Text)
import Numeric (showHex)

hexNumber :: Int -> Int -> Text
hexNumber width value = T.justifyRight width '0' (T.pack (map toUpper (showHex value "")))

hexBytesPerRow :: Int -> Int
hexBytesPerRow available = if available>=hexWidth 16 then 16 else 8

hexAsciiColumn :: Int -> Int
hexAsciiColumn count = 12+3*count+if count>8 then 1 else 0

hexWidth :: Int -> Int
hexWidth count = hexAsciiColumn count+count

hexColumn :: Int -> Int
hexColumn index = 10+3*index+if index>=8 then 1 else 0

hexDividers :: Int -> [Int]
hexDividers count = [8,hexAsciiColumn count-1]

-- Each cell carries its byte offset, so both panes highlight the same selection.
hexRow :: Int -> Int -> Text -> [(Char,Maybe Int)]
hexRow count row bytes = plain (hexNumber 8 start<>"│ ") ++ concatMap cell [0..count-1] ++ plain " │" ++ concatMap ascii [0..count-1]
  where
    start=row*count
    chunk=T.take count (T.drop start bytes)
    value i = if i<T.length chunk then Just (T.index chunk i) else Nothing
    plain = map (\c -> (c,Nothing)) . T.unpack
    tagged i = map (\c -> (c,Just (start+i))) . T.unpack
    cell i = maybe (plain "  ") (tagged i . hexNumber 2 . ord) (value i) ++ plain (if i==7 && count>8 then "  " else " ")
    ascii i = maybe (plain " ") (\c -> tagged i (T.singleton (if c>=' ' && c<='~' then c else '.'))) (value i)

hexHit :: Int -> Int -> (Int,Bool,Bool)
hexHit count column
  | column>=hexAsciiColumn count = (min (count-1) (column-hexAsciiColumn count),True,False)
  | otherwise = let i=max 0 (min (count-1) ((column-10-if count>8 && column>=35 then 1 else 0) `div` 3))
                in (i,False,column==hexColumn i+1)

parseHex :: Text -> Either Text Text
parseHex input
  | odd (length digits) || any (not . isHexDigit) digits = Left "Paste hexadecimal byte pairs (for example: 00 FF 41)."
  | otherwise = Right (T.pack (pairs digits))
  where
    digits=filter (not . isSpace) (T.unpack input)
    pairs (a:b:rest)=chr (16*digitToInt a+digitToInt b):pairs rest
    pairs _=[]
