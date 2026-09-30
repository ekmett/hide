{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Buffer where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (GeneralCategory(..), generalCategory, isAlphaNum, isSpace)
import Graphics.Vty (safeWcwidth)

-- ponytail: whole-text undo snapshots, capped at 100; use a piece table if large files need it.
data Buffer = Buffer
  { contents :: Text, saved :: Text, undoStack :: [Text], redoStack :: [Text]
  , revision :: Int
  } deriving (Eq, Show)
data Selection = Selection { anchor :: Int, caret :: Int } deriving (Eq, Show)

newBuffer :: Text -> Buffer
newBuffer t = Buffer t t [] [] 0

dirty :: Buffer -> Bool
dirty b = contents b /= saved b

ordered :: Selection -> (Int,Int)
ordered (Selection a c) = (min a c, max a c)

replaceSelection :: Selection -> Text -> Buffer -> Buffer
replaceSelection sel inserted b
  | updated == contents b = b
  | otherwise = b { contents = updated, undoStack = take 100 (contents b : undoStack b)
                  , redoStack = [], revision = revision b + 1 }
  where
    (a,z) = ordered sel
    updated = T.take (max 0 a) (contents b) <> inserted <> T.drop (max a z) (contents b)

undo, redo :: Buffer -> Buffer
undo b = case undoStack b of
  [] -> b
  t:ts -> b { contents = t, undoStack = ts, redoStack = contents b : redoStack b, revision = revision b + 1 }
redo b = case redoStack b of
  [] -> b
  t:ts -> b { contents = t, redoStack = ts, undoStack = contents b : undoStack b, revision = revision b + 1 }

lineColumn :: Text -> Int -> (Int,Int)
lineColumn t p = (T.count "\n" before, T.length (last (T.splitOn "\n" before)))
  where before = T.take (max 0 p) t

textLines :: Text -> [Text]
textLines = T.splitOn "\n"

lineOffset :: Text -> Int -> Int
lineOffset t row = sum (map ((+1) . T.length) (take (max 0 row) (textLines t))) `min` T.length t

lineAt :: Text -> Int -> Text
lineAt t row = case drop (max 0 row) (textLines t) of x:_ -> x; [] -> ""

displayColumn :: Text -> Int -> Int
displayColumn t p = T.foldl' advance 0 (T.take p t)
  where advance n '\t' = n + 8 - n `mod` 8
        advance n c = n + safeWcwidth c

columnOffset :: Text -> Int -> Int
columnOffset t goal = go 0 0 (T.unpack t)
  where
    go i _ [] = i
    go i col (c:cs)
      | col >= goal && safeWcwidth c > 0 = i
      | col + width > goal = i
      | otherwise = go (i+1) (col+width) cs
      where width = if c == '\t' then 8-col `mod` 8 else safeWcwidth c

combining :: Char -> Bool
combining c = generalCategory c `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark]

nextCharacter, previousCharacter :: Text -> Int -> Int
nextCharacter t p = min (T.length t) (p + 1 + T.length (T.takeWhile combining (T.drop (p+1) t)))
previousCharacter t p = max 0 (p - 1 - T.length (T.takeWhile combining (T.reverse (T.take p t))))

wordLeft, wordRight :: Text -> Int -> Int
wordLeft t p = T.length (T.dropWhileEnd wordChar (T.dropWhileEnd isSpace (T.take p t)))
wordRight t p = p + T.length word + T.length spaces
  where
    (word, rest) = T.span wordChar (T.drop p t)
    spaces = if T.null word then T.take 1 rest else T.takeWhile isSpace rest

wordChar :: Char -> Bool
wordChar c = isAlphaNum c || c `elem` ("_'" :: String)

selectedText :: Selection -> Buffer -> Text
selectedText sel b = let (a,z) = ordered sel in T.take (z-a) (T.drop a (contents b))
