{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Buffer where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (GeneralCategory(..), generalCategory, isAlphaNum, isSpace)
import Graphics.Vty (safeWcwidth)

-- ponytail: whole-text undo snapshots, capped at 100; use a piece table if large files need it.
data Buffer = Buffer
  { contents :: Text, saved :: Text, undoStack :: [(Text,(Int,Int,Int))], redoStack :: [(Text,(Int,Int,Int))]
  , revision :: Int, lastChange :: Maybe (Int,Int,Int)
  } deriving (Eq, Show)
data Selection = Selection { anchor :: Int, caret :: Int } deriving (Eq, Show)

newBuffer :: Text -> Buffer
newBuffer t = Buffer t t [] [] 0 Nothing

dirty :: Buffer -> Bool
dirty b = contents b /= saved b

ordered :: Selection -> (Int,Int)
ordered (Selection a c) = (min a c, max a c)

replaceSelection :: Selection -> Text -> Buffer -> Buffer
replaceSelection sel inserted b
  | updated == contents b = b {lastChange=Nothing}
  | otherwise = b { contents = updated, undoStack = take 100 ((contents b,(a,a+T.length inserted,z-a)) : undoStack b)
                  , redoStack = [], revision = revision b + 1, lastChange = Just (a,z,T.length inserted) }
  where
    (a,z) = ordered sel
    updated = T.take (max 0 a) (contents b) <> inserted <> T.drop (max a z) (contents b)

undo, redo :: Buffer -> Buffer
undo b = case undoStack b of
  [] -> b {lastChange=Nothing}
  (t,change@(a,z,n)):ts -> b { contents = t, undoStack = ts, redoStack = (contents b,(a,a+n,z-a)) : redoStack b, revision = revision b + 1, lastChange = Just change }
redo b = case redoStack b of
  [] -> b {lastChange=Nothing}
  (t,change@(a,z,n)):ts -> b { contents = t, redoStack = ts, undoStack = (contents b,(a,a+n,z-a)) : undoStack b, revision = revision b + 1, lastChange = Just change }

lineColumn :: Text -> Int -> (Int,Int)
lineColumn t p = (T.count "\n" before, T.length (last (T.splitOn "\n" before)))
  where before = T.take (max 0 p) t

textLines :: Text -> [Text]
textLines = T.splitOn "\n"

lineOffset :: Text -> Int -> Int
lineOffset t row = sum (map ((+1) . T.length) (take (max 0 row) (textLines t))) `min` T.length t

lineAt :: Text -> Int -> Text
lineAt t row = case drop (max 0 row) (textLines t) of x:_ -> T.dropWhileEnd (== '\r') x; [] -> ""

displayColumn :: Text -> Int -> Int
displayColumn t p = T.foldl' advance 0 (T.take p t)
  where advance n '\t' = n + 8 - n `mod` 8
        advance n c = n + characterWidth c

columnOffset :: Text -> Int -> Int
columnOffset t goal = go 0 0 (T.unpack t)
  where
    go i _ [] = i
    go i col (c:cs)
      | col >= goal && characterWidth c > 0 = i
      | col + width > goal = i
      | otherwise = go (i+1) (col+width) cs
      where width = if c == '\t' then 8-col `mod` 8 else characterWidth c

combining :: Char -> Bool
combining c = generalCategory c `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark]

nextCharacter, previousCharacter :: Text -> Int -> Int
nextCharacter t p | T.take 2 (T.drop p t) == "\r\n" = p+2
nextCharacter t p = min (T.length t) (p + 1 + T.length (T.takeWhile combining (T.drop (p+1) t)))
previousCharacter t p | T.take 2 (T.drop (p-2) t) == "\r\n" && p>=2 = p-2
previousCharacter t p = max 0 (p - 1 - T.length (T.takeWhile combining (T.reverse (T.take p t))))

wordLeft, wordRight :: Text -> Int -> Int
wordLeft t p = case T.unsnoc before of
  Nothing -> 0
  Just (_,c) -> T.length (T.dropWhileEnd (if wordChar c then wordChar else (\x -> not (wordChar x) && not (isSpace x))) before)
  where before = T.dropWhileEnd isSpace (T.take p t)
wordRight t p = p + T.length word + T.length spaces
  where
    (word, rest) = T.span wordChar (T.drop p t)
    spaces = if T.null word then T.take 1 rest else T.takeWhile isSpace rest

wordChar :: Char -> Bool
wordChar c = isAlphaNum c || c `elem` ("_'" :: String)

selectedText :: Selection -> Buffer -> Text
selectedText sel b = let (a,z) = ordered sel in T.take (z-a) (T.drop a (contents b))

-- C0/DEL bytes are displayed as visible placeholders; CR belongs to CRLF.
characterWidth :: Char -> Int
characterWidth '\r' = 0
characterWidth c | c < ' ' || c == '\DEL' = 1
characterWidth c = safeWcwidth c
