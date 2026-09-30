{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module THC.Edit.Buffer
  ( Buffer(saved,undoStack,redoStack,revision,lastChange), Selection(..)
  , newBuffer, contents, dirty, ordered, replaceSelection, undo, redo, selectedText
  , bufferLength, bufferLineCount, bufferLineColumn, bufferLineOffset, bufferLineAt
  , lineColumn, textLines, lineOffset, lineAt, displayColumn, columnOffset
  , combining, nextCharacter, previousCharacter, wordLeft, wordRight, wordChar, characterWidth
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (GeneralCategory(..), generalCategory, isAlphaNum, isSpace)
import Data.Foldable (toList)
import qualified Data.FingerTree as FT
import Graphics.Vty (safeWcwidth)

data LineMeasure = LineMeasure { characterCount :: !Int, lineCount :: !Int } deriving (Eq,Show)
instance Semigroup LineMeasure where
  LineMeasure a b <> LineMeasure c d = LineMeasure (a+c) (b+d)
instance Monoid LineMeasure where
  mempty = LineMeasure 0 0

-- Each leaf includes its newline; the final leaf has none (and may be empty).
data Line = Line !Int Text deriving (Eq,Show)
instance FT.Measured LineMeasure Line where
  measure (Line n _) = LineMeasure n 1
type LineTree = FT.FingerTree LineMeasure Line

data Buffer = Buffer
  { bufferLines :: !LineTree, cachedContents :: Text, saved :: Text
  , undoStack :: [(LineTree,(Int,Int,Int))], redoStack :: [(LineTree,(Int,Int,Int))]
  , revision :: !Int, lastChange :: Maybe (Int,Int,Int)
  } deriving (Eq, Show)
data Selection = Selection { anchor :: Int, caret :: Int } deriving (Eq, Show)

newBuffer :: Text -> Buffer
newBuffer t = Buffer (linesFromText t) t t [] [] 0 Nothing

-- The lazy projection is shared by rendering, highlighting and language tooling.
-- Undo retains only trees, so old flattened documents are not retained by history.
contents :: Buffer -> Text
contents = cachedContents

lineText :: Line -> Text
lineText (Line _ t) = t

treeText :: LineTree -> Text
treeText = T.concat . map lineText . toList

linesFromText :: Text -> LineTree
linesFromText = FT.fromList . go . T.splitOn "\n"
  where
    line t = Line (T.length t) t
    go [] = []
    go [t] = [line t]
    go (t:ts) = line (t <> "\n") : go ts

bufferLength, bufferLineCount :: Buffer -> Int
bufferLength = characterCount . FT.measure . bufferLines
bufferLineCount = lineCount . FT.measure . bufferLines

-- Locate one line with a measured split, including the final empty line at EOF.
splitLine :: Int -> LineTree -> (LineTree,Text,Int,LineTree)
splitLine position tree = case FT.viewl right of
  line FT.:< rest -> (left,lineText line,p-characterCount (FT.measure left),rest)
  FT.EmptyL -> case FT.viewr left of
    rest FT.:> line -> (rest,lineText line,p-characterCount (FT.measure rest),FT.empty)
    FT.EmptyR -> (FT.empty,"",0,FT.empty)
  where
    p=max 0 (min position (characterCount (FT.measure tree)))
    (left,right)=FT.split ((>p) . characterCount) tree

bufferLineColumn :: Buffer -> Int -> (Int,Int)
bufferLineColumn b p = let (before,_,column,_) = splitLine p (bufferLines b)
                      in (lineCount (FT.measure before),column)

bufferLineOffset :: Buffer -> Int -> Int
bufferLineOffset b row = characterCount (FT.measure before)
  where (before,_) = FT.split ((>max 0 row) . lineCount) (bufferLines b)

bufferLineAt :: Buffer -> Int -> Text
bufferLineAt b row = case FT.viewl remaining of
  FT.EmptyL -> ""
  line FT.:< _ -> T.dropWhileEnd (=='\r') (T.dropWhileEnd (=='\n') (lineText line))
  where remaining = FT.dropUntil ((>max 0 row) . lineCount) (bufferLines b)

rangeText :: Int -> Int -> LineTree -> Text
rangeText a z tree
  | z <= a = ""
  | otherwise = T.take (z-a) (T.drop (a-start) (treeText selected))
  where
    (before,remaining)=FT.split ((>a) . characterCount) tree
    start=characterCount (FT.measure before)
    (middle,after)=FT.split ((>z-start) . characterCount) remaining
    selected=case FT.viewl after of
      line FT.:< _ | characterCount (FT.measure middle) < z-start -> middle FT.|> line
      _ -> middle

dirty :: Buffer -> Bool
dirty b = contents b /= saved b

ordered :: Selection -> (Int,Int)
ordered (Selection a c) = (min a c, max a c)

replaceSelection :: Selection -> Text -> Buffer -> Buffer
replaceSelection sel inserted b@Buffer{bufferLines=tree,undoStack=history,revision=version}
  | insertedLength == z-a && rangeText a z tree == inserted = b {lastChange=Nothing}
  | otherwise = b { bufferLines = updated, cachedContents = treeText updated
                  , undoStack = take 100 ((tree,(a,a+insertedLength,z-a)) : history)
                  , redoStack = [], revision = version + 1, lastChange = Just (a,z,insertedLength) }
  where
    (rawA,rawZ) = ordered sel
    a=max 0 (min (bufferLength b) rawA)
    z=max 0 (min (bufferLength b) rawZ)
    insertedLength=T.length inserted
    (before,first,start,_) = splitLine a tree
    (_,lastLine,end,after) = splitLine z tree
    joined=linesFromText (T.take start first <> inserted <> T.drop end lastLine)
    -- A following leaf already represents the line after the boundary newline.
    middle=if FT.null after then joined else case FT.viewr joined of rest FT.:> _ -> rest; FT.EmptyR -> FT.empty
    updated=before FT.>< middle FT.>< after

undo, redo :: Buffer -> Buffer
undo b@Buffer{bufferLines=current,undoStack=history,redoStack=future,revision=version} = case history of
  [] -> b {lastChange=Nothing}
  (t,change@(a,z,n)):ts -> b { bufferLines = t, cachedContents = treeText t, undoStack = ts, redoStack = (current,(a,a+n,z-a)) : future, revision = version + 1, lastChange = Just change }
redo b@Buffer{bufferLines=current,undoStack=history,redoStack=future,revision=version} = case future of
  [] -> b {lastChange=Nothing}
  (t,change@(a,z,n)):ts -> b { bufferLines = t, cachedContents = treeText t, redoStack = ts, undoStack = (current,(a,a+n,z-a)) : history, revision = version + 1, lastChange = Just change }

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
selectedText sel b = let (a,z) = ordered sel; clip = max 0 . min (bufferLength b)
                    in rangeText (clip a) (clip z) (bufferLines b)

-- C0/DEL bytes are displayed as visible placeholders; CR belongs to CRLF.
characterWidth :: Char -> Int
characterWidth '\r' = 0
characterWidth c | c < ' ' || c == '\DEL' = 1
characterWidth c = safeWcwidth c
