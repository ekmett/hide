{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module THC.Edit.Buffer
  ( Buffer(saved,undoStack,redoStack,revision,lastChange,byteMode,savedByteMode), Selection(..)
  , BufferSnapshot(..), snapshotBuffer, restoreBuffer
  , newBuffer, newByteBuffer, bufferBytes, markSaved, toggleByteMode, replaceBuffer, textBuffer
  , contents, dirty, ordered, replaceSelection, undo, redo, selectedText
  , bufferLength, bufferLineCount, bufferLineColumn, bufferLineOffset, bufferLineAt
  , bufferNextCharacter, bufferPreviousCharacter, bufferNewline, bufferSlice
  , lineColumn, textLines, lineOffset, lineAt, displayColumn, columnOffset
  , combining, nextCharacter, previousCharacter, wordLeft, wordRight, wordChar, characterWidth
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Char (GeneralCategory(..), generalCategory, isAlphaNum, isSpace)
import Data.Foldable (toList)
import qualified Data.FingerTree as FT
import Graphics.Vty (safeWcwidth)
import THC.Edit.Unicode (graphemes, clusterWidth)

data LineMeasure = LineMeasure { characterCount :: !Int, lineCount :: !Int, containsNul :: !Bool, containsCRLF :: !Bool } deriving (Eq,Show)
instance Semigroup LineMeasure where
  LineMeasure a b n r <> LineMeasure c d m s = LineMeasure (a+c) (b+d) (n || m) (r || s)
instance Monoid LineMeasure where
  mempty = LineMeasure 0 0 False False

-- Each leaf includes its newline; the final leaf has none (and may be empty).
data Line = Line !Int !Bool !Bool Text deriving (Eq,Show)
instance FT.Measured LineMeasure Line where
  measure (Line n nul crlf _) = LineMeasure n 1 nul crlf
type LineTree = FT.FingerTree LineMeasure Line

data Buffer = Buffer
  { bufferLines :: !LineTree, cachedContents :: Text, saved :: Text
  , undoStack :: [(LineTree,Bool,(Int,Int,Int))], redoStack :: [(LineTree,Bool,(Int,Int,Int))]
  , revision :: !Int, lastChange :: Maybe (Int,Int,Int), byteMode :: Bool, savedByteMode :: Bool
  } deriving (Eq, Show)
-- Recovery flattens each persistent history tree explicitly; its edit metadata
-- and representation mode must travel with it for lossless undo and redo.
data BufferSnapshot = BufferSnapshot
  { snapshotContents :: Text, snapshotSaved :: Text
  , snapshotUndo :: [(Text,Bool,(Int,Int,Int))], snapshotRedo :: [(Text,Bool,(Int,Int,Int))]
  , snapshotRevision :: Int, snapshotLastChange :: Maybe (Int,Int,Int)
  , snapshotByteMode :: Bool, snapshotSavedByteMode :: Bool
  } deriving (Eq,Show)

snapshotBuffer :: Buffer -> BufferSnapshot
snapshotBuffer b=BufferSnapshot (contents b) (saved b) (map flatten (undoStack b)) (map flatten (redoStack b))
  (revision b) (lastChange b) (byteMode b) (savedByteMode b)
  where flatten (tree,mode,change)=(treeText tree,mode,change)

restoreBuffer :: BufferSnapshot -> Either Text Buffer
restoreBuffer s
  | snapshotRevision s<0 || snapshotRevision s>1073741823=Left "Invalid buffer revision"
  | not (validText (snapshotByteMode s) (snapshotContents s) && validText (snapshotSavedByteMode s) (snapshotSaved s))=Left "Invalid byte buffer representation"
  | any ((>100).length) [snapshotUndo s,snapshotRedo s]=Left "Invalid buffer history length"
  | not (validHistory (T.length (snapshotContents s)) (snapshotUndo s) && validHistory (T.length (snapshotContents s)) (snapshotRedo s))=Left "Invalid buffer history"
  | maybe False (not . validChange (T.length (snapshotContents s))) (snapshotLastChange s)=Left "Invalid last buffer change"
  | otherwise=Right (Buffer (linesFromText (snapshotContents s)) (snapshotContents s) (snapshotSaved s)
      (map inflate (snapshotUndo s)) (map inflate (snapshotRedo s)) (snapshotRevision s) (snapshotLastChange s)
      (snapshotByteMode s) (snapshotSavedByteMode s))
  where
    validText mode text=not mode || T.all ((<=255).ord) text
    validChange size (a,z,n)=a>=0 && z>=a && n>=0 && toInteger a+toInteger n<=toInteger size
    validHistory _ []=True
    validHistory size ((text,mode,change@(a,z,n)):rest)=validText mode text && validChange (T.length text) change && z<=size &&
      toInteger size-toInteger (z-a)+toInteger n==toInteger (T.length text) && validHistory (T.length text) rest
    inflate (text,mode,change)=(linesFromText text,mode,change)

data Selection = Selection { anchor :: Int, caret :: Int } deriving (Eq, Show)

newBuffer :: Text -> Buffer
newBuffer t = Buffer (linesFromText t) t t [] [] 0 Nothing False False

-- Byte buffers use one Latin-1 code point per byte; text never passes through a lossy decoder.
newByteBuffer :: BS.ByteString -> Buffer
newByteBuffer bytes = (newBuffer (TE.decodeLatin1 bytes)) {byteMode=True,savedByteMode=True}

encodeContents :: Bool -> Text -> BS.ByteString
encodeContents False = TE.encodeUtf8
encodeContents True = BS.pack . map (fromIntegral . ord) . T.unpack

bufferBytes :: Buffer -> BS.ByteString
bufferBytes b = encodeContents (byteMode b) (contents b)

markSaved :: Buffer -> Buffer
markSaved b = b {saved=contents b,savedByteMode=byteMode b}

textBuffer :: Buffer -> Bool
textBuffer b = not (byteMode b) && not (containsNul (FT.measure (bufferLines b)))

toggleByteMode :: Buffer -> Either Text Buffer
toggleByteMode b
  | not (byteMode b) = Right (replaceBuffer True (TE.decodeLatin1 (bufferBytes b)) b)
  | BS.elem 0 bytes = Left "This file contains NUL bytes; keep using hex mode."
  | otherwise = either (const (Left "These bytes are not valid UTF-8; keep using hex mode."))
      (\text -> Right (replaceBuffer False text b)) (TE.decodeUtf8' bytes)
  where bytes=bufferBytes b

-- Reload and mode changes retain both the representation and contents in undo.
replaceBuffer :: Bool -> Text -> Buffer -> Buffer
replaceBuffer mode text b
  | mode && T.any ((>255) . ord) text = b {lastChange=Nothing}
  | mode==byteMode b = replaceSelection (Selection 0 (bufferLength b)) text b
  | otherwise = b {bufferLines=linesFromText text,cachedContents=text,byteMode=mode,
      undoStack=take 100 ((bufferLines b,byteMode b,(0,T.length text,bufferLength b)):undoStack b),
      redoStack=[],revision=revision b+1,lastChange=Just (0,bufferLength b,T.length text)}

-- The lazy projection is shared by rendering, highlighting and language tooling.
-- Undo retains only trees, so old flattened documents are not retained by history.
contents :: Buffer -> Text
contents = cachedContents

lineText :: Line -> Text
lineText (Line _ _ _ t) = t

treeText :: LineTree -> Text
treeText = T.concat . map lineText . toList

linesFromText :: Text -> LineTree
linesFromText = FT.fromList . go . T.splitOn "\n"
  where
    line t = Line (T.length t) (T.any (=='\0') t) ("\r\n" `T.isSuffixOf` t) t
    go [] = []
    go [t] = [line t]
    go (t:ts) = line (t <> "\n") : go ts

bufferLength, bufferLineCount :: Buffer -> Int
bufferLength = characterCount . FT.measure . bufferLines
bufferLineCount = lineCount . FT.measure . bufferLines

-- These flags are recomputed only for changed leaves.
bufferNewline :: Buffer -> Text
bufferNewline b = if containsCRLF (FT.measure (bufferLines b)) then "\r\n" else "\n"

bufferSlice :: Buffer -> Int -> Int -> Text
bufferSlice b start count = rangeText a (a+min (max 0 count) (bufferLength b-a)) (bufferLines b)
  where a=max 0 (min (bufferLength b) start)

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

-- Character motion inspects the containing line, not a flattened document.
-- The previous line is needed only at column zero, to preserve CRLF as a unit.
bufferNextCharacter, bufferPreviousCharacter :: Buffer -> Int -> Int
bufferNextCharacter b position = p-column+nextCharacter line column
  where
    p=max 0 (min (bufferLength b) position)
    (_,line,column,_)=splitLine p (bufferLines b)
bufferPreviousCharacter b position
  | column>0 = p-column+previousCharacter line column
  | otherwise = case FT.viewr before of
      _ FT.:> previous -> p-if "\r\n" `T.isSuffixOf` lineText previous then 2 else 1
      FT.EmptyR -> 0
  where
    p=max 0 (min (bufferLength b) position)
    (before,line,column,_)=splitLine p (bufferLines b)

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
dirty b | byteMode b==savedByteMode b = contents b /= saved b
        | otherwise = bufferBytes b /= encodeContents (savedByteMode b) (saved b)

ordered :: Selection -> (Int,Int)
ordered (Selection a c) = (min a c, max a c)

replaceSelection :: Selection -> Text -> Buffer -> Buffer
replaceSelection sel inserted b@Buffer{bufferLines=tree,undoStack=history,revision=version}
  | byteMode b && T.any ((>255) . ord) inserted = b {lastChange=Nothing}
  | insertedLength == z-a && rangeText a z tree == inserted = b {lastChange=Nothing}
  | otherwise = b { bufferLines = updated, cachedContents = treeText updated
                  , undoStack = take 100 ((tree,byteMode b,(a,a+insertedLength,z-a)) : history)
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
  (t,mode,change@(a,z,n)):ts -> b { bufferLines = t, byteMode=mode, cachedContents = treeText t, undoStack = ts, redoStack = (current,byteMode b,(a,a+n,z-a)) : future, revision = version + 1, lastChange = Just change }
redo b@Buffer{bufferLines=current,undoStack=history,redoStack=future,revision=version} = case future of
  [] -> b {lastChange=Nothing}
  (t,mode,change@(a,z,n)):ts -> b { bufferLines = t, byteMode=mode, cachedContents = treeText t, redoStack = ts, undoStack = (current,byteMode b,(a,a+n,z-a)) : history, revision = version + 1, lastChange = Just change }

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
displayColumn t p = go 0 0 (graphemes t)
  where
    go _ col [] = col
    go offset col (g:gs)
      | offset+T.length g>p = col
      | otherwise = go (offset+T.length g) (col+width col g) gs
    width col "\t"=8-col `mod` 8
    width _ "\r"=0
    width _ g | T.any (<' ') g=1
              | otherwise=clusterWidth g

columnOffset :: Text -> Int -> Int
columnOffset t goal = go 0 0 (graphemes t)
  where
    go i _ [] = i
    go i col (g:gs)
      | col>=goal && width>0 = i
      | col+width>goal = i
      | otherwise = go (i+T.length g) (col+width) gs
      where width | g=="\t"=8-col `mod` 8
                  | g=="\r"=0
                  | T.any (<' ') g=1
                  | otherwise=clusterWidth g

combining :: Char -> Bool
combining c = generalCategory c `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark]

nextCharacter, previousCharacter :: Text -> Int -> Int
nextCharacter t p
  | "\r\n" `T.isPrefixOf` rest = p+2
  | "\n" `T.isPrefixOf` rest = p+1
  | otherwise = p + case graphemes (T.takeWhile (/='\n') rest) of []->0; g:_->T.length g
  where rest=T.drop p t
previousCharacter t p
  | "\r\n" `T.isSuffixOf` before = p-2
  | "\n" `T.isSuffixOf` before = p-1
  | otherwise = p - maybe 0 T.length (case reverse (graphemes (T.takeWhileEnd (/='\n') before)) of []->Nothing; g:_->Just g)
  where before=T.take p t

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
