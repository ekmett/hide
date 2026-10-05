{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
-- | Persistent editable text, byte-preserving buffers and change provenance.
--
-- A finger tree stores newline-inclusive lines. Its measure counts live text,
-- review text, inserted/deleted lines and encoding flags. Deleted baseline lines
-- remain as zero-live-width tombstones; change runs place deletions before
-- insertions. Edits share untouched subtrees and undo retains those trees.
--
-- Offsets count Unicode characters, not UTF-8 bytes or display cells. Byte mode
-- instead uses one Latin-1 character per byte. Whole-text projections are lazy;
-- line navigation uses measured splits. Fingerprints reject unequal content but
-- never establish equality without an exact check. Derived Eq is not a redraw key.
module Hide.Buffer
  ( Buffer(saved,undoStack,redoStack,revision,lastChange,byteMode,savedByteMode), Selection(..)
  , BufferSnapshot(..), snapshotBuffer, restoreBuffer
  , BufferContent, bufferContent, contentLength, contentLineCount, contentByteMode
  , contentSlice, contentByteSlice, contentLineOffset, contentLineAt
  , newBuffer, newByteBuffer, bufferBytes, markSaved, toggleByteMode, replaceBuffer, textBuffer
  , DirtySnapshot, captureDirty, snapshotDirty
  , contents, dirty, ordered, replaceSelection, replaceRanges, prepareBuffer, undo, redo, selectedText
  , bufferLineChanges, bufferViewProjection, bufferLength, bufferLineCount,
    ChangeKind(..), changeRowCount, bufferChangeRows, changeLength, changeSlice
  , changeLineColumn, changeLineOffset, changeLineAt, liveToChangeOffset, changeToLiveOffset
  , changeHunkAt, nextChangeHunk, revertChangeHunk, bufferLineColumn, bufferLineOffset, bufferLineAt, bufferRowsFrom
  , bufferNextCharacter, bufferPreviousCharacter, bufferWordLeft, bufferWordRight, bufferNewline, bufferSlice
  , lineColumn, textLines, lineOffset, lineAt, displayColumn, columnOffset
  , combining, nextCharacter, previousCharacter, wordLeft, wordRight, wordChar, characterWidth
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Word (Word64)
import Data.Bits ((.|.), (.&.))
import Data.Char (GeneralCategory(..), generalCategory, isAlphaNum, isSpace, ord)
import Data.Foldable (toList)
import Control.Monad (unless)
import qualified Data.FingerTree as FT
import Graphics.Vty (safeWcwidth)
import Hide.BufferView (ViewProjection, buildViewProjection, forceViewProjection)
import Hide.Unicode (graphemes, clusterWidth)

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
-- Encoding flags include the terminator, so a tree measure never inspects text.
-- Packing the existing NUL/CRLF flags also leaves the ordinary Text leaf compact.
data Line = Line !Int !Int !LineOrigin !Word64 !Word64 Text deriving (Eq,Show)
instance FT.Measured LineMeasure Line where
  measure (Line n flags origin fingerprint factor _) = case origin of
    Deleted -> LineMeasure 0 0 0 1 False False 0 1 reviewSize
    _ -> LineMeasure n 1 (if origin==Added && n>0 then 1 else 0) 0 (flags .&. 1/=0) (flags .&. 2/=0) fingerprint factor reviewSize
    where reviewSize=n+if flags .&. 4/=0 then 0 else 1
type LineTree = FT.FingerTree LineMeasure Line

-- | An editable revision with a saved baseline and at most 100 undo states.
-- Use buffer operations to preserve line provenance and cached projections.
data Buffer = Buffer
  { bufferLines :: !LineTree, cachedContents :: Text, saved :: Text
  , undoStack :: [(LineTree,Bool,(Int,Int,Int))], redoStack :: [(LineTree,Bool,(Int,Int,Int))]
  , revision :: !Int, lastChange :: Maybe (Int,Int,Int), byteMode :: Bool, savedByteMode :: Bool
  , baselineLines :: !LineTree, viewProjection :: ViewProjection
  } deriving (Eq, Show)
-- | Immutable tree and representation; excludes separate baseline/Undo roots.
-- Deleted provenance leaves remain retained but occupy no live range or row.
-- Capture is shallow. Local reads use the same measured algorithms as editing.
data BufferContent = BufferContent !LineTree !Bool

-- | Retain live content without keeping the editable buffer or its Undo alive.
bufferContent :: Buffer -> BufferContent
bufferContent b=BufferContent (bufferLines b) (byteMode b)

contentLength, contentLineCount :: BufferContent -> Int
contentLength (BufferContent tree _)=characterCount (FT.measure tree)
contentLineCount (BufferContent tree _)=lineCount (FT.measure tree)
contentByteMode :: BufferContent -> Bool
contentByteMode (BufferContent _ mode)=mode

-- | Clamped character range; byte representation has one character per byte.
contentSlice :: BufferContent -> Int -> Int -> Text
contentSlice (BufferContent tree _) = treeSlice tree

-- | Clamped original-byte range, for byte content only; callers check mode.
contentByteSlice :: BufferContent -> Int -> Int -> BS.ByteString
contentByteSlice content start count=encodeContents True (contentSlice content start count)

-- | Start of a zero-based row, clamped at EOF, using the cached tree measure.
contentLineOffset :: BufferContent -> Int -> Int
contentLineOffset (BufferContent tree _) = treeLineOffset tree

-- | Editor row without its terminator, using a measured split.
contentLineAt :: BufferContent -> Int -> Text
contentLineAt (BufferContent tree _) = treeLineAt tree

-- | Explicit recovery representation, including flattened histories and provenance.
-- Constructing or encoding this value may traverse all retained buffer text.
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

-- | Project a buffer and its history for persistence. Keep this out of input/render work.
snapshotBuffer :: Buffer -> BufferSnapshot
snapshotBuffer b=BufferSnapshot (contents b) (saved b) (map flatten (undoStack b)) (map flatten (redoStack b))
  (revision b) (lastChange b) (byteMode b) (savedByteMode b)
  (Just (snapshotLines (bufferLines b),map (snapshotLines . first) (undoStack b),map (snapshotLines . first) (redoStack b)))
  where
    flatten (tree,mode,change)=(treeText tree,mode,change)
    first (tree,_,_)=tree

-- | Validate recovery coordinates, byte representation and history before rebuilding trees.
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

-- | Anchor and caret in zero-based character offsets; either end may come first.
data Selection = Selection { anchor :: Int, caret :: Int } deriving (Eq, Show)

-- | Create a clean text buffer with an empty undo history and one final editor row.
newBuffer :: Text -> Buffer
newBuffer t = let tree=linesFromText t in Buffer tree t t [] [] 0 Nothing False False tree (projectionFor tree)

-- | Create a clean byte buffer using one Latin-1 character per byte, without lossy decoding.
newByteBuffer :: BS.ByteString -> Buffer
newByteBuffer bytes = (newBuffer (TE.decodeLatin1 bytes)) {byteMode=True,savedByteMode=True}

encodeContents :: Bool -> Text -> BS.ByteString
encodeContents False = TE.encodeUtf8
encodeContents True = BS.pack . map (fromIntegral . ord) . T.unpack

-- | Encode the current representation for file output: UTF-8 text or original byte values.
bufferBytes :: Buffer -> BS.ByteString
bufferBytes b = encodeContents (byteMode b) (contents b)

-- | Establish the current contents as baseline through measured changed-leaf traversal.
-- Undo/redo provenance is rebased lazily, replaying stored inverse edits only
-- when a history entry is used. Do not update saved directly.
markSaved :: Buffer -> Buffer
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

-- | The shared lazy whole-text projection. Use line/slice accessors for local navigation.
contents :: Buffer -> Text
contents = cachedContents

lineText :: Line -> Text
lineText (Line _ _ _ _ _ t) = t

lineCharacters :: Line -> Int
lineCharacters (Line n _ _ _ _ _) = n

-- Cached fingerprints only reject unequal lines. Restore/provenance decisions
-- still check the exact text when the length and fingerprint both match.
sameLineText :: Line -> Line -> Bool
sameLineText a@(Line n _ _ hash _ _) b@(Line m _ _ other _ _) =
  n==m && hash==other && lineText a==lineText b

lineOrigin :: Line -> LineOrigin
lineOrigin (Line _ _ origin _ _ _) = origin

withOrigin :: LineOrigin -> Line -> Line
withOrigin origin (Line n flags _ fingerprint factor t) = Line n flags origin fingerprint factor t

lineTerminated :: Line -> Bool
lineTerminated (Line _ flags _ _ _ _) = flags .&. 4/=0

lineCRLF :: Line -> Bool
lineCRLF (Line _ flags _ _ _ _) = flags .&. 2/=0

treeText :: LineTree -> Text
treeText = T.concat . map lineText . filter ((/=Deleted) . lineOrigin) . toList

linesFromText :: Text -> LineTree
linesFromText = FT.fromList . go . T.splitOn "\n"
  where
    line t = let n=T.length t
                 flags=(if T.any (=='\0') t then 1 else 0) .|.
                       (if "\r\n" `T.isSuffixOf` t then 2 else 0) .|.
                       (if "\n" `T.isSuffixOf` t then 4 else 0)
             in Line n flags Original
      (T.foldl' (\hash c -> hash*16777619+fromIntegral (ord c)+1) 0 t) (16777619^n) t
    go [] = []
    go [t] = [line t]
    go (t:ts) = line (t <> "\n") : go ts

-- | Live character and editor-row totals from the root measure; tombstones do not count.
bufferLength, bufferLineCount :: Buffer -> Int
bufferLength = characterCount . FT.measure . bufferLines
bufferLineCount = lineCount . FT.measure . bufferLines

-- These flags are recomputed only for changed leaves.
bufferNewline :: Buffer -> Text
bufferNewline b = if containsCRLF (FT.measure (bufferLines b)) then "\r\n" else "\n"

-- | Read a clamped start/count range of live text using measured splits.
bufferSlice :: Buffer -> Int -> Int -> Text
bufferSlice b = treeSlice (bufferLines b)

treeSlice :: LineTree -> Int -> Int -> Text
treeSlice tree start count = rangeText a (a+min (max 0 count) (size-a)) tree
  where
    size=characterCount (FT.measure tree)
    a=max 0 (min size start)

-- Locate one line with a measured split, including the final empty line at EOF.
splitLine :: Int -> LineTree -> (LineTree,Text,Int,LineTree)
splitLine position tree=let (before,line,column,after)=splitLeaf position tree in (before,lineText line,column,after)

splitLeaf :: Int -> LineTree -> (LineTree,Line,Int,LineTree)
splitLeaf position tree = case FT.viewl right of
  line FT.:< rest -> (left,line,p-characterCount (FT.measure left),rest)
  FT.EmptyL -> case FT.viewl final of
    line FT.:< rest -> (prefix,line,p-characterCount (FT.measure prefix),rest)
    FT.EmptyL -> (FT.empty,Line 0 0 Original 0 1 "",0,FT.empty)
  where
    p=max 0 (min position (characterCount (FT.measure tree)))
    (left,right)=FT.split ((>p) . characterCount) tree
    (prefix,final)=FT.split ((>lineCount (FT.measure tree)-1) . lineCount) tree

-- | Locate a character offset as a zero-based row and character column.
bufferLineColumn :: Buffer -> Int -> (Int,Int)
bufferLineColumn b p = let (before,_,column,_) = splitLeaf p (bufferLines b)
                      in (lineCount (FT.measure before),column)

-- | Find the start of a zero-based live row using the tree measure.
bufferLineOffset :: Buffer -> Int -> Int
bufferLineOffset b = treeLineOffset (bufferLines b)

treeLineOffset :: LineTree -> Int -> Int
treeLineOffset tree row = characterCount (FT.measure before)
  where (before,_) = FT.split ((>max 0 row) . lineCount) tree

-- | Read one live row without its line terminator; out-of-range rows return empty text.
bufferLineAt :: Buffer -> Int -> Text
bufferLineAt b = treeLineAt (bufferLines b)

-- | Borrow live rows from one measured seek, without flattening the buffer.
-- Negative starts clamp to zero; past EOF yields no rows. Terminators are
-- stripped as in 'bufferLineAt', including the final empty editor row.
--
-- @bufferRowsFrom b n == map (bufferLineAt b) [max 0 n .. bufferLineCount b - 1]@
--
-- Seeking is /O(log n)/; consuming rows visits their physical leaves, including
-- deleted tombstones, and strips their terminators. Text storage remains shared.
bufferRowsFrom :: Buffer -> Int -> [Text]
bufferRowsFrom b row =
  [T.dropWhileEnd (=='\r') (T.dropWhileEnd (=='\n') (lineText line))
  | line<-toList (FT.dropUntil ((>max 0 row) . lineCount) (bufferLines b))
  , lineOrigin line/=Deleted]

treeLineAt :: LineTree -> Int -> Text
treeLineAt tree row = case FT.viewl remaining of
  FT.EmptyL -> ""
  line FT.:< _ -> T.dropWhileEnd (=='\r') (T.dropWhileEnd (=='\n') (lineText line))
  where remaining = FT.dropUntil ((>max 0 row) . lineCount) tree

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
      previous FT.:< _ -> p-if lineCRLF previous then 2 else 1
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

-- | Narrow immutable inputs for the modified flag. Matching representations
-- need only the measured changes. A representation switch needs exact encoded
-- text comparison; retain those fields, not Buffer/Undo, and evaluate on a worker.
data DirtySnapshot = MeasuredDirty !Bool | EncodedDirty !Bool Text !Bool Text

-- | Evaluate this constructor shallowly during capture. Text projections remain
-- lazy, so the exceptional representation comparison does not run under UI locks.
captureDirty :: Buffer -> DirtySnapshot
captureDirty Buffer{bufferLines=tree,cachedContents=current,saved=baseline,byteMode=mode,savedByteMode=savedMode}
  | mode==savedMode=let measure=FT.measure tree in MeasuredDirty ((newLineCount measure,deletedLineCount measure)/=(0,0))
  | otherwise=EncodedDirty mode current savedMode baseline

snapshotDirty :: DirtySnapshot -> Bool
snapshotDirty (MeasuredDirty changed)=changed
snapshotDirty (EncodedDirty mode current savedMode baseline)=encodeContents mode current/=encodeContents savedMode baseline

dirty :: Buffer -> Bool
dirty = snapshotDirty . captureDirty

ordered :: Selection -> (Int,Int)
ordered (Selection a c) = (min a c, max a c)

-- | Replace a clamped half-open selection and retain the previous tree for Undo.
-- Invalid byte-mode characters are ignored; text-preserving edits add no history.
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

-- | Apply ascending, disjoint character ranges in the original buffer as one
-- undo step. Starts must be distinct; invalid ranges or byte text reject the
-- entire batch. Empty and text-preserving batches return the original buffer.
replaceRanges :: [(Int,Int,Text)] -> Buffer -> Either Text Buffer
replaceRanges edits b@Buffer{bufferLines=tree,undoStack=history,revision=version}=do
  validate Nothing edits
  case filter changed edits of
    [] -> pure b
    changes@((a,_,_):_) ->
      let (_,z,_)=last changes
          updated=restoreBaseline (baselineLines b) (foldl' (\current (lo,hi,text) -> editTree lo hi text current) tree (reverse changes))
          n=z-a+characterCount (FT.measure updated)-characterCount (FT.measure tree)
      in pure $ if sameText tree updated then b else
        b {bufferLines=updated,viewProjection=projectionFor updated,cachedContents=treeText updated
          ,undoStack=take 100 ((tree,byteMode b,(a,a+n,z-a)):history),redoStack=[]
          ,revision=version+1,lastChange=Just (a,z,n)}
  where
    validate _ []=Right ()
    validate previous ((a,z,text):rest)
      | a<0 || z<a || z>bufferLength b=Left "Invalid buffer edit range"
      | maybe False (\(start,end) -> a<=start || a<end) previous=Left "Buffer edit ranges must be ascending and disjoint"
      | byteMode b && T.any ((>255).ord) text=Left "Invalid byte buffer edit"
      | otherwise=validate (Just (a,z)) rest
    changed (a,z,text)=T.length text/=z-a || rangeText a z tree/=text

-- | Prepare measured edits and compact review indexes on their owning worker.
-- Shared text, flattened contents, saved baselines and undo history stay lazy.
prepareBuffer :: Buffer -> ()
prepareBuffer b=FT.measure (bufferLines b) `seq` forceViewProjection (viewProjection b)

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
              old FT.:< oldTail | sameLineText old line ->
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
      (line FT.:< olds,replacement FT.:< news) | sameLineText line replacement ->
        let (same,remainingOld,remainingNew)=matchingLeft olds news
        in (withOrigin Original line FT.<| same,remainingOld,remainingNew)
      _ -> (FT.empty,old,new)
    matchingRight old new=case (FT.viewr old,FT.viewr new) of
      (olds FT.:> line,news FT.:> replacement) | sameLineText line replacement ->
        let (remainingOld,remainingNew,same)=matchingRight olds news
        in (remainingOld,remainingNew,same FT.|> withOrigin Original line)
      _ -> (old,new,FT.empty)
    pair old new=case (FT.viewl old,FT.viewl new) of
      (line FT.:< olds,replacement FT.:< news)
        | lineCharacters replacement==0,FT.null news -> markDeleted old FT.|> withOrigin Original replacement
        | sameLineText line replacement -> withOrigin Original line FT.<| pair olds news
        | otherwise -> deleted line FT.>< (asAdded replacement FT.<| pair olds news)
      (_,FT.EmptyL) -> markDeleted old
      (FT.EmptyL,_) -> FT.fromList (map asAdded (toList new))
    deleted line | lineCharacters line==0=FT.empty
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
asAdded line=withOrigin (if lineCharacters line==0 then Original else Added) line

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
  | sameText current baseline=baseline
  | otherwise=current
  where
    measure=FT.measure current

-- Fingerprints only reject mismatches; equality still checks exact live text.
sameText :: LineTree -> LineTree -> Bool
sameText left right=characterCount a==characterCount b && contentHash a==contentHash b && treeText left==treeText right
  where a=FT.measure left; b=FT.measure right

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
      Added | lineCharacters line/=0 -> (row,True,""):go (row+1) rest
      _ -> go (row+1) rest

restoreLines :: Text -> Text -> LineChangesSnapshot -> Either Text LineTree
restoreLines baseline text changes=do
  lines'<-go 0 (toList (linesFromText text)) changes
  let original=[lineText line | line<-lines',lineOrigin line/=Added,lineCharacters line/=0]
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
    go row (line:live) ((index,True,empty):rest) | index==row && T.null empty && lineCharacters line/=0 =
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
  _ FT.:> line | not (lineTerminated line) -> 1
  _ -> 0
  where tree=bufferLines b

reviewText :: Line -> Text
reviewText line=let text=lineText line in if lineTerminated line then text else text<>"\n"

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
    FT.EmptyR -> (FT.empty,Line 0 0 Original 0 1 "",0)
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
  in characterCount (FT.measure before)+if lineOrigin line==Deleted then 0 else min column (lineCharacters line)

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

-- | Move left by the existing scalar word policy, with a clamped live offset.
-- @bufferWordLeft b p == wordLeft (contents b) (clamp p)@. Skips preceding
-- whitespace, then a word or punctuation run. Only the containing row and
-- crossed live leaves are read; deleted provenance never contributes text.
bufferWordLeft :: Buffer -> Int -> Int
bufferWordLeft b position=case T.unsnoc remaining of
  Nothing->0
  Just (_,c)->let (result,_,_)=spanLeft (if wordChar c then wordChar else punctuation) q remaining earlier in result
  where
    p=max 0 (min (bufferLength b) position)
    (before,line,column,_)=splitLine p (bufferLines b)
    (q,remaining,earlier)=spanLeft isSpace p (T.take column line) before
    punctuation c=not (wordChar c) && not (isSpace c)
    spanLeft predicate offset text previous=
      let rest=T.dropWhileEnd predicate text
          next=offset-T.length (T.takeWhileEnd predicate text)
      in next `seq` if not (T.null rest) then (next,rest,previous) else case FT.viewr previous of
        prefix FT.:> leaf | lineOrigin leaf/=Deleted->spanLeft predicate next (lineText leaf) prefix
        _ | lineCount (FT.measure previous)<=0->(next,"",FT.empty)
          | otherwise->let (prefix,trailing)=FT.split ((>lineCount (FT.measure previous)-1) . lineCount) previous
                       in case FT.viewl trailing of
                         leaf FT.:< _->spanLeft predicate next (lineText leaf) prefix
                         FT.EmptyL->(next,"",FT.empty)

-- | Move right by the existing asymmetric scalar word policy.
-- @bufferWordRight b p == wordRight (contents b) (clamp p)@. A current word
-- consumes its following whitespace; any other current scalar advances once.
-- Measured lookup and live neighbors avoid the lazy whole-text projection.
bufferWordRight :: Buffer -> Int -> Int
bufferWordRight b position=case T.uncons current of
  Nothing->p
  Just (c,_) | not (wordChar c)->p+1
             | otherwise->let (q,rest,later)=spanRight wordChar p current after
                              (result,_,_)=spanRight isSpace q rest later
                          in result
  where
    p=max 0 (min (bufferLength b) position)
    (_,line,column,after)=splitLine p (bufferLines b)
    current=T.drop column line
    spanRight predicate offset text following=
      let (consumed,rest)=T.span predicate text
          next=offset+T.length consumed
      in next `seq` if not (T.null rest) then (next,rest,following) else case FT.viewl following of
        leaf FT.:< suffix | lineOrigin leaf==Deleted->spanRight predicate next "" (FT.dropUntil ((>0) . lineCount) following)
                          | otherwise->spanRight predicate next (lineText leaf) suffix
        FT.EmptyL->(next,"",FT.empty)

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

-- | Return inserted and deleted baseline-line counts from the root measure.
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
