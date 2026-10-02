{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module THC.Edit.Buffer
  ( Buffer(saved,undoStack,redoStack,revision,lastChange,byteMode,savedByteMode), Selection(..)
  , BufferSnapshot(..), snapshotBuffer, restoreBuffer
  , newBuffer, newByteBuffer, bufferBytes, markSaved, toggleByteMode, replaceBuffer, textBuffer
  , contents, dirty, ordered, replaceSelection, undo, redo, selectedText
  , bufferLineChanges, bufferViewProjection, bufferLength, bufferLineCount,
    ChangeKind(..), changeRowCount, bufferChangeRows, changeLength, changeSlice
  , changeLineColumn, changeLineOffset, changeLineAt, liveToChangeOffset, changeToLiveOffset
  , changeHunkAt, nextChangeHunk, revertChangeHunk, bufferLineColumn, bufferLineOffset, bufferLineAt
  , bufferNextCharacter, bufferPreviousCharacter, bufferNewline, bufferSlice
  , lineColumn, textLines, lineOffset, lineAt, displayColumn, columnOffset
  , combining, nextCharacter, previousCharacter, wordLeft, wordRight, wordChar, characterWidth
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Word (Word64)
import Data.Char (GeneralCategory(..), generalCategory, isAlphaNum, isSpace)
import Data.Foldable (toList)
import Control.Monad (unless)
import qualified Data.FingerTree as FT
import Graphics.Vty (safeWcwidth)
import THC.Edit.BufferView (ViewProjection, buildViewProjection)
import THC.Edit.Unicode (graphemes, clusterWidth)

data LineMeasure = LineMeasure
  { characterCount :: !Int, lineCount :: !Int, newLineCount :: !Int, deletedLineCount :: !Int
  , containsNul :: !Bool, containsCRLF :: !Bool, contentHash :: !Word64, hashFactor :: !Word64, reviewCharacterCount :: !Int } deriving (Eq,Show)
instance Semigroup LineMeasure where
  LineMeasure a b c d n r h p u <> LineMeasure e f g i m s j q v = LineMeasure (a+e) (b+f) (c+g) (d+i) (n || m) (r || s) (h*q+j) (p*q) (u+v)
instance Monoid LineMeasure where
  mempty = LineMeasure 0 0 0 0 False False 0 1 0

data LineOrigin = Original | Added | Deleted deriving (Eq,Show)
-- Deleted leaves retain baseline text, but occupy no character or visible line.
-- The final empty editor row is visible and contributes no file-line change.
data Line = Line !Int !Bool !Bool !LineOrigin !Word64 !Word64 Text deriving (Eq,Show)
instance FT.Measured LineMeasure Line where
  measure (Line n nul crlf origin fingerprint factor text) = case origin of
    Deleted -> LineMeasure 0 0 0 1 False False 0 1 reviewSize
    _ -> LineMeasure n 1 (if origin==Added && n>0 then 1 else 0) 0 nul crlf fingerprint factor reviewSize
    where reviewSize=n+if "\n" `T.isSuffixOf` text then 0 else 1
type LineTree = FT.FingerTree LineMeasure Line

data Buffer = Buffer
  { bufferLines :: !LineTree, cachedContents :: Text, saved :: Text
  , undoStack :: [(LineTree,Bool,(Int,Int,Int))], redoStack :: [(LineTree,Bool,(Int,Int,Int))]
  , revision :: !Int, lastChange :: Maybe (Int,Int,Int), byteMode :: Bool, savedByteMode :: Bool
  , baselineLines :: !LineTree, viewProjection :: ViewProjection
  } deriving (Eq, Show)
-- Recovery flattens each persistent history tree explicitly; its edit metadata
-- and representation mode must travel with it for lossless undo and redo.
data BufferSnapshot = BufferSnapshot
  { snapshotContents :: Text, snapshotSaved :: Text
  , snapshotUndo :: [(Text,Bool,(Int,Int,Int))], snapshotRedo :: [(Text,Bool,(Int,Int,Int))]
  , snapshotRevision :: Int, snapshotLastChange :: Maybe (Int,Int,Int)
  , snapshotByteMode :: Bool, snapshotSavedByteMode :: Bool
  , snapshotLineChanges :: Maybe (LineChangesSnapshot,[LineChangesSnapshot],[LineChangesSnapshot])
  } deriving (Eq,Show)

-- Visible row, inserted flag, and deleted baseline text. Unchanged lines are
-- omitted; all coordinates and baseline text are checked when restoring.
type LineChangesSnapshot = [(Int,Bool,Text)]

snapshotBuffer :: Buffer -> BufferSnapshot
snapshotBuffer b=BufferSnapshot (contents b) (saved b) (map flatten (undoStack b)) (map flatten (redoStack b))
  (revision b) (lastChange b) (byteMode b) (savedByteMode b)
  (Just (snapshotLines (bufferLines b),map (snapshotLines . first) (undoStack b),map (snapshotLines . first) (redoStack b)))
  where
    flatten (tree,mode,change)=(treeText tree,mode,change)
    first (tree,_,_)=tree

restoreBuffer :: BufferSnapshot -> Either Text Buffer
restoreBuffer s
  | snapshotRevision s<0 || snapshotRevision s>1073741823=Left "Invalid buffer revision"
  | not (validText (snapshotByteMode s) (snapshotContents s) && validText (snapshotSavedByteMode s) (snapshotSaved s))=Left "Invalid byte buffer representation"
  | any ((>100).length) [snapshotUndo s,snapshotRedo s]=Left "Invalid buffer history length"
  | not (validHistory (T.length (snapshotContents s)) (snapshotUndo s) && validHistory (T.length (snapshotContents s)) (snapshotRedo s))=Left "Invalid buffer history"
  | maybe False (not . validChange (T.length (snapshotContents s))) (snapshotLastChange s)=Left "Invalid last buffer change"
  | otherwise=do
      (current,history,future)<-case snapshotLineChanges s of
        Nothing -> pure (fromSaved (snapshotContents s),map inflate (snapshotUndo s),map inflate (snapshotRedo s))
        Just (currentChanges,historyChanges,futureChanges) -> do
          unless (length historyChanges==length (snapshotUndo s) && length futureChanges==length (snapshotRedo s))
            (Left "Invalid buffer line-change history")
          current<-restoreLines (snapshotSaved s) (snapshotContents s) currentChanges
          history<-sequence (zipWith restoreEntry (snapshotUndo s) historyChanges)
          future<-sequence (zipWith restoreEntry (snapshotRedo s) futureChanges)
          pure (current,history,future)
      pure (Buffer current (snapshotContents s) (snapshotSaved s) history future (snapshotRevision s) (snapshotLastChange s)
        (snapshotByteMode s) (snapshotSavedByteMode s) (linesFromText (snapshotSaved s)) (projectionFor current))
  where
    validText mode text=not mode || T.all ((<=255).ord) text
    validChange size (a,z,n)=a>=0 && z>=a && n>=0 && toInteger a+toInteger n<=toInteger size
    validHistory _ []=True
    validHistory size ((text,mode,change@(a,z,n)):rest)=validText mode text && validChange (T.length text) change && z<=size &&
      toInteger size-toInteger (z-a)+toInteger n==toInteger (T.length text) && validHistory (T.length text) rest
    -- Older checkpoints have no provenance. Reconcile their saved/current line
    -- regions conservatively; all new checkpoints preserve exact edit identity.
    fromSaved text=normalizeHunks (reconcileTree (linesFromText (snapshotSaved s)) (linesFromText text))
    inflate (text,mode,change)=(fromSaved text,mode,change)
    restoreEntry (text,mode,change) changes=do
      tree<-restoreLines (snapshotSaved s) text changes
      pure (tree,mode,change)

data Selection = Selection { anchor :: Int, caret :: Int } deriving (Eq, Show)

newBuffer :: Text -> Buffer
newBuffer t = let tree=linesFromText t in Buffer tree t t [] [] 0 Nothing False False tree (projectionFor tree)

-- Byte buffers use one Latin-1 code point per byte; text never passes through a lossy decoder.
newByteBuffer :: BS.ByteString -> Buffer
newByteBuffer bytes = (newBuffer (TE.decodeLatin1 bytes)) {byteMode=True,savedByteMode=True}

encodeContents :: Bool -> Text -> BS.ByteString
encodeContents False = TE.encodeUtf8
encodeContents True = BS.pack . map (fromIntegral . ord) . T.unpack

bufferBytes :: Buffer -> BS.ByteString
bufferBytes b = encodeContents (byteMode b) (contents b)

markSaved :: Buffer -> Buffer
-- Reset the baseline through this operation, not a direct update of saved.
-- Measured searches visit only changed leaves. History rebasing stays lazy and
-- replays each stored inverse edit only if that history entry is used.
markSaved b = b {bufferLines=clean,baselineLines=clean,viewProjection=projectionFor clean,saved=contents b,savedByteMode=byteMode b,
  undoStack=rebaseHistory clean clean (undoStack b),redoStack=rebaseHistory clean clean (redoStack b)}
  where clean=normalizeLines (bufferLines b)

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
  | otherwise = b {bufferLines=updated,viewProjection=projectionFor updated,cachedContents=text,byteMode=mode,
      undoStack=take 100 ((bufferLines b,byteMode b,(0,T.length text,bufferLength b)):undoStack b),
      redoStack=[],revision=revision b+1,lastChange=Just (0,bufferLength b,T.length text)}
  where updated=restoreBaseline (baselineLines b) (editTree 0 (bufferLength b) text (bufferLines b))

-- The lazy projection is shared by rendering, highlighting and language tooling.
-- Undo retains only trees, so old flattened documents are not retained by history.
contents :: Buffer -> Text
contents = cachedContents

lineText :: Line -> Text
lineText (Line _ _ _ _ _ _ t) = t

lineOrigin :: Line -> LineOrigin
lineOrigin (Line _ _ _ origin _ _ _) = origin

withOrigin :: LineOrigin -> Line -> Line
withOrigin origin (Line n nul crlf _ fingerprint factor t) = Line n nul crlf origin fingerprint factor t

treeText :: LineTree -> Text
treeText = T.concat . map lineText . filter ((/=Deleted) . lineOrigin) . toList

linesFromText :: Text -> LineTree
linesFromText = FT.fromList . go . T.splitOn "\n"
  where
    line t = let n=T.length t in Line n (T.any (=='\0') t) ("\r\n" `T.isSuffixOf` t) Original
      (T.foldl' (\hash c -> hash*16777619+fromIntegral (ord c)+1) 0 t) (16777619^n) t
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
splitLine position tree=let (before,line,column,after)=splitLeaf position tree in (before,lineText line,column,after)

splitLeaf :: Int -> LineTree -> (LineTree,Line,Int,LineTree)
splitLeaf position tree = case FT.viewl right of
  line FT.:< rest -> (left,line,p-characterCount (FT.measure left),rest)
  FT.EmptyL -> case FT.viewl final of
    line FT.:< rest -> (prefix,line,p-characterCount (FT.measure prefix),rest)
    FT.EmptyL -> (FT.empty,Line 0 False False Original 0 1 "",0,FT.empty)
  where
    p=max 0 (min position (characterCount (FT.measure tree)))
    (left,right)=FT.split ((>p) . characterCount) tree
    (prefix,final)=FT.split ((>lineCount (FT.measure tree)-1) . lineCount) tree

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
  | p==0=0
  | column>0 = p-column+previousCharacter line column
  | otherwise = case FT.viewl previousLine of
      previous FT.:< _ -> p-if "\r\n" `T.isSuffixOf` lineText previous then 2 else 1
      FT.EmptyL -> 0
  where
    p=max 0 (min (bufferLength b) position)
    (before,line,column,_)=splitLine p (bufferLines b)
    previousLine=FT.dropUntil ((>lineCount (FT.measure before)-1) . lineCount) before

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
dirty b | byteMode b==savedByteMode b = bufferLineChanges b/=(0,0)
        | otherwise = bufferBytes b /= encodeContents (savedByteMode b) (saved b)

ordered :: Selection -> (Int,Int)
ordered (Selection a c) = (min a c, max a c)

replaceSelection :: Selection -> Text -> Buffer -> Buffer
replaceSelection sel inserted b@Buffer{bufferLines=tree,undoStack=history,revision=version}
  | byteMode b && T.any ((>255) . ord) inserted = b {lastChange=Nothing}
  | insertedLength == z-a && rangeText a z tree == inserted = b {lastChange=Nothing}
  | otherwise = b { bufferLines = updated, viewProjection=projectionFor updated, cachedContents = treeText updated
                  , undoStack = take 100 ((tree,byteMode b,(a,a+insertedLength,z-a)) : history)
                  , redoStack = [], revision = version + 1, lastChange = Just (a,z,insertedLength) }
  where
    (rawA,rawZ) = ordered sel
    a=max 0 (min (bufferLength b) rawA)
    z=max 0 (min (bufferLength b) rawZ)
    insertedLength=T.length inserted
    updated=restoreBaseline (baselineLines b) (editTree a z inserted tree)

-- Only the edited line region is rebuilt. Include adjacent tombstones so
-- restoring a replaced/deleted original line can recover its baseline identity.
editTree :: Int -> Int -> Text -> LineTree -> LineTree
editTree a z inserted tree=foldl' (flip cancelRestoredLine) updated [firstRow..lastRow]
  where
    updated=keptBefore FT.>< normalizeHunks (changedBefore FT.>< reconcileTree affected middle FT.>< changedAfter) FT.>< keptAfter
    rowAt position=let (prefix,_,_,_)=splitLeaf position updated in lineCount (FT.measure prefix)
    firstRow=rowAt a
    lastRow=rowAt (a+T.length inserted)
    (rawBefore,first,start,_) = splitLeaf a tree
    (_,lastLine,end,rawAfter) = splitLeaf z tree
    (before,_)=stripDeletedEnd rawBefore
    (_,after)=FT.split ((>0) . lineCount) rawAfter
    (keptBefore,changedBefore)=splitChangedEnd before
    (changedAfter,keptAfter)=FT.split ((>0) . originalCount) after
    entries m=lineCount m+deletedLineCount m
    (_,rest)=FT.split ((>entries (FT.measure before)) . entries) tree
    (affected,_)=FT.split ((>entries (FT.measure tree)-entries (FT.measure before)-entries (FT.measure after)) . entries) rest
    joined=linesFromText (T.take start (lineText first) <> inserted <> T.drop end (lineText lastLine))
    middle=if lineCount (FT.measure after)==0 then joined else case FT.viewr joined of rest' FT.:> _ -> rest'; FT.EmptyR -> FT.empty
    stripDeletedEnd input
      | lineCount (FT.measure input)==0=(FT.empty,input)
      | otherwise=let (prefix,lastAndDeleted)=FT.split ((>lineCount (FT.measure input)-1) . lineCount) input
        in case FT.viewl lastAndDeleted of
          line FT.:< deleted -> (prefix FT.|> line,deleted)
          FT.EmptyL -> (input,FT.empty)

-- Canonical runs can separate a modified line from its own tombstone. Check
-- only the edited added row against its corresponding boundary-aligned original
-- slots; an equal pair splits the run around a restored unchanged line.
cancelRestoredLine :: Int -> LineTree -> LineTree
cancelRestoredLine row tree=case FT.viewl remaining of
  line FT.:< rest | lineOrigin line==Added ->
    let (prefix,leading)=splitChangedEnd before
        (trailing,after)=FT.split ((>0) . originalCount) rest
        hunk=leading FT.>< (line FT.<| trailing)
        removed=deletedLineCount (FT.measure hunk)
        added=newLineCount (FT.measure hunk)
        index=newLineCount (FT.measure leading)
        candidates=if removed==added then [index] else [index,removed-added+index]
        restore []=tree
        restore (candidate:others)
          | candidate<0 || candidate>=removed=restore others
          | otherwise=let (oldBefore,oldRest)=FT.split ((>candidate) . deletedLineCount) hunk
            in case FT.viewl oldRest of
              old FT.:< oldTail | lineText old==lineText line ->
                let (oldAfter,addedLines)=FT.split ((>0) . newLineCount) oldTail
                    (newBefore,newRest)=FT.split ((>index) . newLineCount) addedLines
                in case FT.viewl newRest of
                  _ FT.:< newAfter -> prefix FT.>< oldBefore FT.>< newBefore FT.><
                    (withOrigin Original old FT.<| (oldAfter FT.>< newAfter FT.>< after))
                  FT.EmptyL -> tree
              _ -> restore others
    in restore candidates
  _ -> tree
  where (before,remaining)=FT.split ((>row) . lineCount) tree

-- Equal boundary lines keep their identity when a whole line is inserted or
-- deleted at a boundary. Equal interior pairs also survive bulk replacements;
-- this is local edit reconciliation, not a render-time whole-buffer diff.
reconcileTree :: LineTree -> LineTree -> LineTree
reconcileTree previous current=prefix FT.>< pair oldMiddle newMiddle FT.>< suffix
  where
    originals=withoutAdded previous
    (prefix,oldRest,newRest)=matchingLeft originals current
    (oldMiddle,newMiddle,suffix)=matchingRight oldRest newRest
    matchingLeft old new=case (FT.viewl old,FT.viewl new) of
      (line FT.:< olds,replacement FT.:< news) | lineText line==lineText replacement ->
        let (same,remainingOld,remainingNew)=matchingLeft olds news
        in (withOrigin Original line FT.<| same,remainingOld,remainingNew)
      _ -> (FT.empty,old,new)
    matchingRight old new=case (FT.viewr old,FT.viewr new) of
      (olds FT.:> line,news FT.:> replacement) | lineText line==lineText replacement ->
        let (remainingOld,remainingNew,same)=matchingRight olds news
        in (remainingOld,remainingNew,same FT.|> withOrigin Original line)
      _ -> (old,new,FT.empty)
    pair old new=case (FT.viewl old,FT.viewl new) of
      (line FT.:< olds,replacement FT.:< news)
        | T.null (lineText replacement),FT.null news -> markDeleted old FT.|> withOrigin Original replacement
        | lineText line==lineText replacement -> withOrigin Original line FT.<| pair olds news
        | otherwise -> deleted line FT.>< (asAdded replacement FT.<| pair olds news)
      (_,FT.EmptyL) -> markDeleted old
      (FT.EmptyL,_) -> FT.fromList (map asAdded (toList new))
    deleted line | T.null (lineText line)=FT.empty
                 | otherwise=FT.singleton (withOrigin Deleted line)
    -- Reediting beside a large removed region must retain that subtree rather
    -- than repeatedly flattening or relabelling every old tombstone.
    withoutAdded tree
      | newLineCount (FT.measure tree)==0=tree
      | otherwise=let (before,changed)=FT.split ((>0) . newLineCount) tree
        in case FT.viewl changed of
          _ FT.:< rest -> before FT.>< withoutAdded rest
          FT.EmptyL -> before
    markDeleted tree
      | lineCount (FT.measure tree)==0=tree
      | otherwise=let (before,live)=FT.split ((>0) . lineCount) tree
        in case FT.viewl live of
          line FT.:< rest -> before FT.>< deleted line FT.>< markDeleted rest
          FT.EmptyL -> before

-- Changed runs are canonical: all deleted originals precede their inserted
-- replacements. Split and concatenate whole same-kind subtrees when possible.
originalCount :: LineMeasure -> Int
originalCount measure=lineCount measure-newLineCount measure

asAdded :: Line -> Line
asAdded line=withOrigin (if T.null (lineText line) then Original else Added) line

splitChangedEnd :: LineTree -> (LineTree,LineTree)
splitChangedEnd tree
  | originalCount (FT.measure tree)==0=(FT.empty,tree)
  | otherwise=let (before,lastOriginal)=FT.split ((>originalCount (FT.measure tree)-1) . originalCount) tree
    in case FT.viewl lastOriginal of
      line FT.:< changed -> (before FT.|> line,changed)
      FT.EmptyL -> (tree,FT.empty)

normalizeHunks :: LineTree -> LineTree
normalizeHunks tree
  | newLineCount measure+deletedLineCount measure==0=tree
  | otherwise=before FT.>< deleted FT.>< added FT.>< normalizeHunks after
  where
    measure=FT.measure tree
    (before,changed)=FT.split (\m -> newLineCount m+deletedLineCount m>0) tree
    (hunk,after)=FT.split ((>0) . originalCount) changed
    (deleted,added)=partitionHunk hunk
    partitionHunk run
      | newLineCount (FT.measure run)==0=(run,FT.empty)
      | deletedLineCount (FT.measure run)==0=(FT.empty,run)
      | otherwise=let (old,rest)=FT.split ((>0) . newLineCount) run
                      (new,more)=FT.split ((>0) . deletedLineCount) rest
                      (olds,news)=partitionHunk more
                  in (old FT.>< olds,new FT.>< news)

normalizeLines :: LineTree -> LineTree
normalizeLines tree
  | changes (FT.measure tree)==0=tree
  | otherwise=case FT.viewl changed of
      line FT.:< rest -> unchanged FT.>< (if lineOrigin line==Deleted then normalizeLines rest else withOrigin Original line FT.<| normalizeLines rest)
      FT.EmptyL -> unchanged
  where
    changes m=newLineCount m+deletedLineCount m
    (unchanged,changed)=FT.split ((>0) . changes) tree

-- Fingerprints reject unequal contents in O(1). Equality is never inferred
-- from a hash: only a possible return to the saved text needs exact comparison.
-- This also cancels edit provenance for duplicate-line delete/reinsert cycles.
restoreBaseline :: LineTree -> LineTree -> LineTree
restoreBaseline baseline current
  | newLineCount measure==0 && deletedLineCount measure==0=current
  | characterCount measure==characterCount savedMeasure && contentHash measure==contentHash savedMeasure
  , treeText current==treeText baseline=baseline
  | otherwise=current
  where
    measure=FT.measure current
    savedMeasure=FT.measure baseline

rebaseHistory :: LineTree -> LineTree -> [(LineTree,Bool,(Int,Int,Int))] -> [(LineTree,Bool,(Int,Int,Int))]
rebaseHistory _ _ []=[]
rebaseHistory baseline current ((old,mode,change@(a,z,n)):rest)=
  let next=restoreBaseline baseline (editTree a z (rangeText a (a+n) old) current)
  in (next,mode,change):rebaseHistory baseline next rest

snapshotLines :: LineTree -> LineChangesSnapshot
snapshotLines=go 0 . toList
  where
    go _ []=[]
    go row (line:rest)=case lineOrigin line of
      Deleted -> (row,False,lineText line):go row rest
      Added | not (T.null (lineText line)) -> (row,True,""):go (row+1) rest
      _ -> go (row+1) rest

restoreLines :: Text -> Text -> LineChangesSnapshot -> Either Text LineTree
restoreLines baseline text changes=do
  lines'<-go 0 (toList (linesFromText text)) changes
  let original=[lineText line | line<-lines',lineOrigin line/=Added,not (T.null (lineText line))]
      savedLines=filter (not . T.null) (map lineText (toList (linesFromText baseline)))
  unless (original==savedLines) (Left "Buffer line changes do not match saved lines")
  unless (canonical False lines') (Left "Invalid buffer change-run ordering")
  pure (FT.fromList lines')
  where
    go _ [] []=Right []
    go row live ((index,False,deleted):rest) | index==row && validDeleted deleted =
      case toList (linesFromText deleted) of
        line:_ -> (withOrigin Deleted line:) <$> go row live rest
        [] -> Left "Invalid deleted buffer line"
    go row (line:live) ((index,True,empty):rest) | index==row && T.null empty && not (T.null (lineText line)) =
      (withOrigin Added line:) <$> go (row+1) live rest
    go row (line:live) []=(line:) <$> go (row+1) live []
    go row (line:live) pending@((index,_,_):_)
      | index>row = (line:) <$> go (row+1) live pending
    go _ _ _=Left "Invalid buffer line-change coordinates"
    validDeleted deleted=not (T.null deleted) && not (T.any (=='\n') (T.dropEnd 1 deleted))
    canonical _ []=True
    canonical added (line:rest)=case lineOrigin line of
      Original -> canonical False rest
      Added -> canonical True rest
      Deleted -> not added && canonical False rest

undo, redo :: Buffer -> Buffer
undo b@Buffer{bufferLines=current,undoStack=history,redoStack=future,revision=version} = case history of
  [] -> b {lastChange=Nothing}
  (t,mode,change@(a,z,n)):ts -> b { bufferLines = t, viewProjection=projectionFor t, byteMode=mode, cachedContents = treeText t, undoStack = ts, redoStack = (current,byteMode b,(a,a+n,z-a)) : future, revision = version + 1, lastChange = Just change }
redo b@Buffer{bufferLines=current,undoStack=history,redoStack=future,revision=version} = case future of
  [] -> b {lastChange=Nothing}
  (t,mode,change@(a,z,n)):ts -> b { bufferLines = t, viewProjection=projectionFor t, byteMode=mode, cachedContents = treeText t, redoStack = ts, undoStack = (current,byteMode b,(a,a+n,z-a)) : history, revision = version + 1, lastChange = Just change }

-- The review projection walks stored leaves, with synthetic line separators
-- where a deleted unterminated final line precedes its replacement.
data ChangeKind = OriginalLine | AddedLine | DeletedLine deriving (Eq,Ord,Show)

reviewRows :: LineMeasure -> Int
reviewRows measure=lineCount measure+deletedLineCount measure

changeRowCount :: Buffer -> Int
changeRowCount=reviewRows . FT.measure . bufferLines

bufferChangeRows :: Buffer -> Int -> Int -> [(ChangeKind,Maybe Int,Text)]
bufferChangeRows b start count=go (lineCount (FT.measure before)) (take (max 0 count) (toList rest))
  where
    (before,rest)=FT.split ((>max 0 start) . reviewRows) (bufferLines b)
    go _ []=[]
    go row (line:lines')=case lineOrigin line of
      Deleted -> (DeletedLine,Nothing,lineText line):go row lines'
      origin -> (if origin==Added then AddedLine else OriginalLine,Just row,lineText line):go (row+1) lines'

changeLength :: Buffer -> Int
changeLength b=reviewCharacterCount (FT.measure tree)-case FT.viewr tree of
  _ FT.:> line | not ("\n" `T.isSuffixOf` lineText line) -> 1
  _ -> 0
  where tree=bufferLines b

reviewText :: Line -> Text
reviewText line=let text=lineText line in if "\n" `T.isSuffixOf` text then text else text<>"\n"

changeSlice :: Buffer -> Int -> Int -> Text
changeSlice b start count
  | z<=a=""
  | otherwise=T.take (z-a) (T.drop (a-reviewCharacterCount (FT.measure before)) (T.concat (map reviewText (toList selected))))
  where
    tree=bufferLines b
    a=max 0 (min (changeLength b) start)
    z=a+max 0 (min (changeLength b-a) count)
    (before,rest)=FT.split ((>a) . reviewCharacterCount) tree
    (middle,after)=FT.split ((>z-reviewCharacterCount (FT.measure before)) . reviewCharacterCount) rest
    selected=case FT.viewl after of
      line FT.:< _ | reviewCharacterCount (FT.measure middle)<z-reviewCharacterCount (FT.measure before) -> middle FT.|> line
      _ -> middle

splitReviewLine :: Buffer -> Int -> (LineTree,Line,Int)
splitReviewLine b position=case FT.viewl remaining of
  line FT.:< _ -> (before,line,p-reviewCharacterCount (FT.measure before))
  FT.EmptyL -> case FT.viewr tree of
    prefix FT.:> line -> (prefix,line,p-reviewCharacterCount (FT.measure prefix))
    FT.EmptyR -> (FT.empty,Line 0 False False Original 0 1 "",0)
  where
    tree=bufferLines b
    p=max 0 (min (changeLength b) position)
    (before,remaining)=FT.split ((>p) . reviewCharacterCount) tree

changeLineColumn :: Buffer -> Int -> (Int,Int)
changeLineColumn b position=let (before,_,column)=splitReviewLine b position in (reviewRows (FT.measure before),column)

changeLineOffset :: Buffer -> Int -> Int
changeLineOffset b row=min (changeLength b) (reviewCharacterCount (FT.measure before))
  where (before,_)=FT.split ((>max 0 row) . reviewRows) (bufferLines b)

changeLineAt :: Buffer -> Int -> Text
changeLineAt b row=case bufferChangeRows b row 1 of
  (_,_,text):_ -> T.dropWhileEnd (=='\r') (T.dropWhileEnd (=='\n') text)
  [] -> ""

liveToChangeOffset :: Buffer -> Int -> Int
liveToChangeOffset b position=let (before,_,column,_)=splitLeaf position (bufferLines b) in reviewCharacterCount (FT.measure before)+column

changeToLiveOffset :: Buffer -> Int -> Int
changeToLiveOffset b position=let (before,line,column)=splitReviewLine b position
  in characterCount (FT.measure before)+if lineOrigin line==Deleted then 0 else min column (T.length (lineText line))

-- Hunk coordinates are (first projected row, number of projected rows).
changeHunkAt :: Buffer -> Int -> Maybe (Int,Int)
changeHunkAt b row
  | row<0 || row>=changeRowCount b=Nothing
  | otherwise=case FT.viewl remaining of
      line FT.:< _ | lineOrigin line/=Original -> Just (reviewRows (FT.measure prefix),reviewRows (FT.measure leading)+reviewRows (FT.measure changed))
      _ -> Nothing
  where
    (before,remaining)=FT.split ((>row) . reviewRows) (bufferLines b)
    (prefix,leading)=splitChangedEnd before
    (changed,_)=FT.split ((>0) . originalCount) remaining

nextChangeHunk :: Buffer -> Int -> Maybe (Int,Int)
nextChangeHunk b row=case FT.viewl changed of
  FT.EmptyL -> Nothing
  _ -> changeHunkAt b (reviewRows (FT.measure before)+reviewRows (FT.measure unchanged))
  where
    (before,rest)=FT.split ((>max 0 row) . reviewRows) (bufferLines b)
    (unchanged,changed)=FT.split (\m -> newLineCount m+deletedLineCount m>0) rest

-- Revert one contiguous run as a single ordinary undoable replacement.
revertChangeHunk :: Int -> Buffer -> Buffer
revertChangeHunk row b=case changeHunkAt b row of
  Nothing -> b {lastChange=Nothing}
  Just (first,count) ->
    let start=changeToLiveOffset b (changeLineOffset b first)
        end=changeToLiveOffset b (changeLineOffset b (first+count))
        original=T.concat [text | (DeletedLine,_,text)<-bufferChangeRows b first count]
    in replaceSelection (Selection start end) original b

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

-- Counts of inserted and removed lines relative to the last load/save.
bufferLineChanges :: Buffer -> (Int,Int)
bufferLineChanges b=let measure=FT.measure (bufferLines b) in (newLineCount measure,deletedLineCount measure)


-- Shared by all views of this buffer and invalidated only when its tree changes.
bufferViewProjection :: Buffer -> ViewProjection
bufferViewProjection=viewProjection

projectionFor :: LineTree -> ViewProjection
projectionFor tree=buildViewProjection (reviewRows (FT.measure tree)) (runs 0 tree)
  where
    runs offset remaining=case FT.viewl changed of
      FT.EmptyL -> []
      _ -> (start,deletedLineCount measure,newLineCount measure):runs (start+reviewRows measure) after
      where
        (before,changed)=FT.split (\m -> newLineCount m+deletedLineCount m>0) remaining
        (hunk,after)=FT.split ((>0) . originalCount) changed
        start=offset+reviewRows (FT.measure before)
        measure=FT.measure hunk
