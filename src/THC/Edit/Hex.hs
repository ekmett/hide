{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Hex (hexNumber, hexColumn, hexDividers, hexRow, hexHit, parseHex) where

import Data.Char (ord, chr, isHexDigit, digitToInt, isSpace, toUpper)
import qualified Data.Text as T
import Data.Text (Text)
import Numeric (showHex)

hexNumber :: Int -> Int -> Text
hexNumber width value = T.justifyRight width '0' (T.pack (map toUpper (showHex value "")))

hexColumn :: Int -> Int
hexColumn index = 10+3*index+if index>=8 then 1 else 0

hexDividers :: [Int]
hexDividers = [60,77]

-- Each cell carries its byte offset, so both panes highlight the same selection.
hexRow :: Int -> Text -> [(Char,Maybe Int)]
hexRow row bytes = plain (hexNumber 8 start<>"  ") ++ concatMap cell [0..15] ++ plain " │" ++ concatMap ascii [0..15] ++ plain "│"
  where
    start=row*16
    chunk=T.take 16 (T.drop start bytes)
    value i = if i<T.length chunk then Just (T.index chunk i) else Nothing
    plain = map (\c -> (c,Nothing)) . T.unpack
    tagged i = map (\c -> (c,Just (start+i))) . T.unpack
    cell i = maybe (plain "  ") (tagged i . hexNumber 2 . ord) (value i) ++ plain (if i==7 then "  " else " ")
    ascii i = maybe (plain " ") (\c -> tagged i (T.singleton (if c>=' ' && c<='~' then c else '.'))) (value i)

hexHit :: Int -> (Int,Bool,Bool)
hexHit column
  | column>=61 = (min 15 (column-61),True,False)
  | otherwise = let i=max 0 (min 15 ((column-10-if column>=35 then 1 else 0) `div` 3))
                in (i,False,column==hexColumn i+1)

parseHex :: Text -> Either Text Text
parseHex input
  | odd (length digits) || any (not . isHexDigit) digits = Left "Paste hexadecimal byte pairs (for example: 00 FF 41)."
  | otherwise = Right (T.pack (pairs digits))
  where
    digits=filter (not . isSpace) (T.unpack input)
    pairs (a:b:rest)=chr (16*digitToInt a+digitToInt b):pairs rest
    pairs _=[]
