{-# LANGUAGE BangPatterns, MagicHash, MultiParamTypeClasses, UnboxedTuples #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Persistent borrowed storage inside a long physical source line.
--
-- Loaded text shares a lazy stream of complete bounded-item span receipts.
-- Exact seeks reuse those receipts; first editing promotes them to a measured
-- tree for persistent local repair. Measures compose source scalar/byte counts,
-- tab-dependent advance and exact-content rejection fingerprints.
-- Whole text and exact loaded width remain independent of the receipt stream.
module Hide.LineChunks
  ( Chunks, ChunkMeasure(..), ColumnAdvance(..), applyAdvance
  , chunksFromText, chunksEdit, chunksMeasure, chunksWidth, chunksText, chunksSlice, chunksFragments
  , chunksWindow, chunksDisplayColumn, chunksColumnOffset
  , chunksPreviousCharacter, chunksSpanLeft, chunksSpanRight, chunksSuffixWidth
  ) where

import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Word (Word64)
import qualified Data.FingerTree as FT
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as TI
import GHC.Exts (isTrue#, sameByteArray#, sizeofByteArray#, Int(I#),Int#)
import Hide.Unicode (DisplayItem, SourceCursor, initialSourceCursor, sourceItemStep, sourceSpanStep, sourceLeafFrom, sourceItemsFromCursor, sourceGraphemesFrom, sourceScalarColumn)

-- | Concatenated source column transforms. A tab erases the incoming residue.
-- @applyAdvance (a <> b) c == applyAdvance b (applyAdvance a c)@.
data ColumnAdvance = Add {-# UNPACK #-} !Int | Tab {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq,Show)

applyAdvance :: ColumnAdvance -> Int -> Int
applyAdvance (Add width) col=col+width
applyAdvance (Tab prefix suffix) col=nextTab (col+prefix)+suffix

nextTab :: Int -> Int
nextTab col=8*(col `div` 8+1)

instance Semigroup ColumnAdvance where
  Add a <> Add b=Add (a+b)
  Add a <> Tab p s=Tab (a+p) s
  Tab p s <> Add a=Tab p (s+a)
  Tab p s <> Tab q t=Tab p (nextTab (s+q)+t)
instance Monoid ColumnAdvance where mempty=Add 0

-- | Byte/scalar/item counts remain distinct. Low flags are NUL, CRLF and LF;
-- upper bits cache the trailing terminator scalar count.
-- the content fingerprint only rejects unequal content, never proves equality.
data ChunkMeasure = ChunkMeasure
  { chunkBytes :: !Int, chunkCharacters :: !Int, chunkItems :: !Int
  , chunkAdvance :: !ColumnAdvance, chunkHash :: !Word64, chunkFactor :: !Word64
  , chunkFlags :: !Int
  } deriving (Eq,Show)
instance Semigroup ChunkMeasure where
  ChunkMeasure b c i w h p f <> ChunkMeasure d e j v g q k=
    ChunkMeasure (b+d) (c+e) (i+j) (w<>v) (h*q+g) (p*q)
      (((f .|. k) .&. 7) .|. (trailing `shiftL` 3))
    where
      tailLeft=f `shiftR` 3
      tailRight=k `shiftR` 3
      trailing=if e==tailRight then tailLeft+tailRight else tailRight
instance Monoid ChunkMeasure where mempty=ChunkMeasure 0 0 0 mempty 0 1 0

-- The outgoing overflow receipt was established with real source lookahead.
-- A borrowed storage leaf's apparent EOF must never override that receipt.
data Chunk = Chunk !ChunkMeasure !T.Text !SourceCursor !SourceCursor !Bool
instance Show Chunk where
  showsPrec p (Chunk m text _ _ overflow)=showsPrec p (m,text,overflow)
instance FT.Measured ChunkMeasure Chunk where measure (Chunk m _ _ _ _)=m

type ChunkTree = FT.FingerTree ChunkMeasure Chunk

-- A loaded line memoizes only demanded span receipts. The Text and exact width
-- remain independent of that stream; exports/outer metadata must not force it.
-- First editing promotes this line once; no display adoption or Buffer mutation.
data Chunks = Loaded !T.Text [Chunk] Int | Edited !ChunkTree
instance Show Chunks where
  showsPrec p (Loaded text _ _)=showsPrec p text
  showsPrec p (Edited tree)=showsPrec p tree

chunksTree :: Chunks -> ChunkTree
chunksTree (Loaded _ spans _)=FT.fromList spans
chunksTree (Edited tree)=tree

chunksMeasure :: Chunks -> ChunkMeasure
chunksMeasure=FT.measure . chunksTree

-- | Exact width is memoized independently of loaded receipts. Its first use
-- scans the original row; it does not dice an undemanded row into spans.
chunksWidth :: Chunks -> Int
chunksWidth (Loaded _ _ width)=width
chunksWidth (Edited tree)=applyAdvance (chunkAdvance (FT.measure tree)) 0

-- | Explicit whole-line read, for serialization and worker-owned consumers.
chunksText :: Chunks -> T.Text
chunksText (Loaded text _ _)=text
chunksText (Edited tree)=T.concat [text | Chunk _ text _ _ _<-toList tree]

-- | Borrow only the leaves overlapping a clamped scalar range. Both boundary
-- leaves inspect bounded local byte spans; middle leaves remain whole.
chunksFragments :: Chunks -> Int -> Int -> [T.Text]
chunksFragments (Loaded text spans _) requested count
  | count<=0=[]
  | requested<=0=[T.take count text]
  | otherwise=case seekLoadedScalar (max 0 requested) spans of
      (# base,_,suffix #)->fragmentsFrom (max 0 requested-I# base) count suffix
chunksFragments (Edited tree) requested count=fragmentsFrom offset (max 0 count) (toList suffix)
  where
    start=max 0 (min (chunkCharacters (FT.measure tree)) requested)
    (prefix,suffix)=FT.split ((>start).chunkCharacters) tree
    offset=start-chunkCharacters (FT.measure prefix)
fragmentsFrom :: Int -> Int -> [Chunk] -> [T.Text]
fragmentsFrom _ remaining _ | remaining<=0=[]
fragmentsFrom _ _ []=[]
fragmentsFrom skip remaining (Chunk m text _ _ _:rest)=
  let size=min remaining (chunkCharacters m-skip)
  in T.take size (T.drop skip text):fragmentsFrom 0 (remaining-size) rest

-- | Exact bounded scalar read; source offsets never refer to UTF8 bytes.
chunksSlice :: Chunks -> Int -> Int -> T.Text
chunksSlice tree start count=T.concat (chunksFragments tree start count)

-- | Seek once by cached display transforms, then borrow only the overlapping
-- leaf suffix and its successors. Offsets count scalars and absolute cells;
-- each group owns its source array, so ordinary style runs cannot cross leaves.
chunksWindow :: Chunks -> Int -> (Int,Int,[(T.Text,[DisplayItem])])
chunksWindow (Loaded _ spans _) requested=case seekLoadedColumn (max 0 requested) spans of
  (# char,col,[] #)->(I# char,I# col,[])
  (# char,col,chunk:rest #)->windowSuffix (max 0 requested) (I# char) (I# col) chunk rest
chunksWindow (Edited tree) requested=case FT.viewl suffix of
  FT.EmptyL->(chunkCharacters measure,applyAdvance (chunkAdvance measure) 0,[])
  chunk FT.:< rest->windowSuffix goal (chunkCharacters measure) initialColumn chunk (toList rest)
  where
    goal=max 0 requested
    (prefix,suffix)=FT.split ((>goal).(`applyAdvance` 0).chunkAdvance) tree
    measure=FT.measure prefix
    initialColumn=applyAdvance (chunkAdvance measure) 0

-- Primitive endpoint receipts keep the cached-span walk numeric. Box the
-- public coordinates only at its selected edge, never once per skipped span.
seekLoadedColumn :: Int -> [Chunk] -> (# Int#,Int#,[Chunk] #)
seekLoadedColumn goal=go 0 0
  where
    go !char !col []=finish char col []
    go !char !col suffix@(Chunk m _ _ _ _:rest)
      | next<=goal=go (char+chunkCharacters m) next rest
      | otherwise=finish char col suffix
      where next=applyAdvance (chunkAdvance m) col
    finish (I# char) (I# col) suffix=(# char,col,suffix #)

seekLoadedScalar :: Int -> [Chunk] -> (# Int#,Int#,[Chunk] #)
seekLoadedScalar goal=go 0 0
  where
    go !char !col []=finish char col []
    go !char !col suffix@(Chunk m _ _ _ _:rest)
      | char+chunkCharacters m<=goal=go (char+chunkCharacters m) (applyAdvance (chunkAdvance m) col) rest
      | otherwise=finish char col suffix
    finish (I# char) (I# col) suffix=(# char,col,suffix #)

windowSuffix :: Int -> Int -> Int -> Chunk -> [Chunk] -> (Int,Int,[(T.Text,[DisplayItem])])
windowSuffix goal base col (Chunk _ text incoming _ overflow) rest=
  let (char,byte,column,items)=sourceLeafFrom goal col incoming overflow text
  in (base+char,column,(TU.dropWord8 byte text,items):
    [(source,sourceItemsFromCursor cursor receipt source) | Chunk _ source cursor _ receipt<-rest])

-- | Bounded width after deliberately removing a scalar prefix, as in a code
-- bubble's indentation. Segmentation restarts at that normalized suffix, exactly
-- like Text.drop; storage edges still supply real lookahead through borrowed
-- fragments. No DisplayItem or whole-suffix Text is constructed.
chunksSuffixWidth :: Chunks -> Int -> Int -> Int -> Int
chunksSuffixWidth (Loaded original _ _) start count bound=go 0 0 initialSourceCursor 0
  where
    text=T.drop start original
    go !byte !chars !cursor !col
      | col>=bound=max 0 bound
      | chars>=count || byte>=TU.lengthWord8 text=col
      | otherwise=case sourceItemStep text byte cursor of
          (# end,n,width,tab,_,next #)->
            if chars+n>count then col else go end (chars+n) next (if tab then nextTab col else col+width)
chunksSuffixWidth loaded@(Edited tree) 0 _ bound=
  min (max 0 bound) (chunksWidth loaded-if chunkFlags (FT.measure tree) .&. 4/=0 then 1 else 0)
chunksSuffixWidth tree start count bound=go first 0 initialSourceCursor 0 rest
  where
    (first,rest)=sourceSpan T.empty (chunksFragments tree start count)
    go text !byte !cursor !col following
      | col>=bound=max 0 bound
      | TU.lengthWord8 text-byte<132 && not (null following)=
          let (next,more)=sourceSpan (TU.dropWord8 byte text) following
          in go next 0 cursor col more
      | byte>=TU.lengthWord8 text=col
      | otherwise=case sourceItemStep text byte cursor of
          (# end,_,width,tab,_,next #)->
            go text end next (if tab then nextTab col else col+width) following

-- | Map a scalar position to the start of its complete display item. Interior
-- scalars never create a partial grapheme; tabs retain the absolute column.
chunksDisplayColumn :: Chunks -> Int -> Int
chunksDisplayColumn (Loaded _ spans _) requested=case seekLoadedScalar (max 0 requested) spans of
  (# _,col,[] #)->I# col
  (# char,col,chunk:_ #)->chunkDisplayColumn (max 0 requested-I# char) (I# col) chunk
chunksDisplayColumn (Edited tree) requested=case FT.viewl suffix of
  FT.EmptyL->initialColumn
  chunk FT.:< _->chunkDisplayColumn local initialColumn chunk
  where
    goal=max 0 (min requested (chunkCharacters (FT.measure tree)))
    (prefix,suffix)=FT.split ((>goal).chunkCharacters) tree
    initialColumn=applyAdvance (chunkAdvance (FT.measure prefix)) 0
    local=goal-chunkCharacters (FT.measure prefix)

chunkDisplayColumn :: Int -> Int -> Chunk -> Int
chunkDisplayColumn local initialColumn (Chunk _ text cursor _ receipt)=
  sourceScalarColumn local initialColumn cursor receipt text

-- | Display hits return an original scalar boundary, using the same seek as
-- visible emission. Zero-width items before the hit are consumed identically.
chunksColumnOffset :: Chunks -> Int -> Int
chunksColumnOffset tree goal=let (char,_,_)=chunksWindow tree goal in char

-- | Previous complete-item boundary, including interior scalar positions.
chunksPreviousCharacter :: Chunks -> Int -> Int
chunksPreviousCharacter (Loaded _ spans _) requested
  | requested<=0=0
  | otherwise=case seekLoadedScalar (requested-1) spans of
      (# base,_,[] #)->I# base
      (# base,_,chunk:_ #)->I# base+chunkPreviousCharacter (requested-I# base) chunk
chunksPreviousCharacter (Edited tree) requested
  | goal<=0=0
  | otherwise=case FT.viewl suffix of
      chunk FT.:< _->base+chunkPreviousCharacter local chunk
      FT.EmptyL->goal
  where
    goal=min requested (chunkCharacters (FT.measure tree))
    (prefix,suffix)=FT.split ((>=goal).chunkCharacters) tree
    base=chunkCharacters (FT.measure prefix)
    local=goal-base
chunkPreviousCharacter :: Int -> Chunk -> Int
chunkPreviousCharacter local (Chunk _ text cursor _ _)
  | local<=0=0
  | otherwise=go 0 0 cursor
  where
    go !byte !char !state=case sourceItemStep text byte state of
      (# end,count,_,_,_,next #)->if char+count>=local then char else go end (char+count) next

-- | Scalar-class traversal borrows only visited storage leaves. Predicates do
-- not require grapheme segmentation; offsets remain original source scalars.
chunksSpanLeft :: (Char->Bool) -> Chunks -> Int -> Int
chunksSpanLeft predicate (Loaded _ spans _) requested=seek 0 [] spans
  where
    goal=max 0 requested
    seek !base previous []=go base T.empty previous
    seek !base previous (chunk@(Chunk m text _ _ _):rest)
      | base+chunkCharacters m<goal=seek (base+chunkCharacters m) (chunk:previous) rest
      | otherwise=go goal (T.take (goal-base) text) previous
    go !offset text previous=
      let consumed=T.takeWhileEnd predicate text; next=offset-T.length consumed
      in if TU.lengthWord8 consumed<TU.lengthWord8 text then next else case previous of
        Chunk _ source _ _ _:earlier->go next source earlier
        []->next
chunksSpanLeft predicate (Edited tree) requested=case FT.viewl suffix of
  Chunk _ text _ _ _ FT.:< _->go goal (T.take (goal-base) text) prefix
  FT.EmptyL->go goal T.empty prefix
  where
    goal=max 0 (min requested (chunkCharacters (FT.measure tree)))
    (prefix,suffix)=FT.split ((>=goal).chunkCharacters) tree
    base=chunkCharacters (FT.measure prefix)
    go !offset text previous=
      let consumed=T.takeWhileEnd predicate text; next=offset-T.length consumed
      in if TU.lengthWord8 consumed<TU.lengthWord8 text then next else case FT.viewr previous of
        earlier FT.:> Chunk _ source _ _ _->go next source earlier
        FT.EmptyR->next

-- | Forward scalar-class traversal, sharing the same physical storage owner.
chunksSpanRight :: (Char->Bool) -> Chunks -> Int -> Int
chunksSpanRight predicate (Loaded _ spans _) requested=case seekLoadedScalar (max 0 requested) spans of
  (# base,_,[] #)->I# base
  (# base,_,Chunk _ text _ _ _:rest #)->go (max 0 requested) (T.drop (max 0 requested-I# base) text) rest
  where
    go !offset text following=
      let consumed=T.takeWhile predicate text; next=offset+T.length consumed
      in if TU.lengthWord8 consumed<TU.lengthWord8 text then next else case following of
        Chunk _ source _ _ _:later->go next source later
        []->next
chunksSpanRight predicate (Edited tree) requested=case FT.viewl suffix of
  Chunk _ text _ _ _ FT.:< rest->go goal (T.drop (goal-base) text) rest
  FT.EmptyL->goal
  where
    goal=max 0 (min requested (chunkCharacters (FT.measure tree)))
    (prefix,suffix)=FT.split ((>goal).chunkCharacters) tree
    base=chunkCharacters (FT.measure prefix)
    go !offset text following=
      let consumed=T.takeWhile predicate text; next=offset+T.length consumed
      in if TU.lengthWord8 consumed<TU.lengthWord8 text then next else case FT.viewl following of
        Chunk _ source _ _ _ FT.:< later->go next source later
        FT.EmptyL->next

-- Text 2.x stores immutable UTF8 arrays plus byte offsets/lengths. Adjacent
-- validated slices of the same array can be recut without copying source bytes.
-- Array identity only permits borrowing; it never proves content equality.
joinAdjacent :: T.Text -> T.Text -> Maybe T.Text
joinAdjacent (TI.Text a@(TA.ByteArray array) start size) (TI.Text (TA.ByteArray other) next count)
  | start>=0 && size>=0 && count>=0 && next>=0
  , size<=capacity-start && count<=capacity-next
  , start+size==next && isTrue# (sameByteArray# array other)=
      Just (TI.Text a start (size+count))
  | otherwise=Nothing
  where capacity=I# (sizeofByteArray# array)

-- | Prepare regular borrowed byte spans in one owning pass. A candidate lies
-- about 128 UTF8 bytes after the preceding cut; move to the next scalar start
-- and complete bounded-item boundary. A span therefore ends by byte 255, apart
-- from bounded local neighbor merging. Cuts are storage choices, not canonical
-- source identity. Loading retains a shared lazy receipt stream; exact queries
-- force only the prefix they visit. The first edit promotes it to a measured tree.
chunksFromText :: T.Text -> Chunks
chunksFromText text=Loaded text (loadedSpans text) width
  where (_,_,width,_)=sourceGraphemesFrom maxBound text

-- Each tail is shared and remains lazy. The scanner sees the original full
-- source, so its outgoing cursor/overflow includes real lookahead at every cut.
loadedSpans :: T.Text -> [Chunk]
loadedSpans text=go 0 initialSourceCursor
  where
    go !byte !cursor
      | byte>=TU.lengthWord8 text=[]
      | otherwise=case sourceSpanStep text byte cursor (byte+128) maxBound False of
          (# end,chars,count,prefix,suffix,overflow,next #)->
            let source=TU.takeWord8 (end-byte) (TU.dropWord8 byte text)
                advance=if prefix<0 then Add suffix else Tab prefix suffix
            in Chunk (leafMeasure source chars count advance) source cursor next overflow:go end next

-- | Repair one physical row. Prefix/suffix trees remain shared. Restart before
-- the edit, where the saved cursor still names unchanged source; reuse the old
-- suffix only at an unchanged item boundary with an equal numeric checkpoint.
-- Valid cuts need not reproduce a canonical global cut sequence. Regional
-- indicator parity can require linear suffix repair, without flattening it.
chunksEdit :: Chunks -> Int -> Int -> T.Text -> Chunks
chunksEdit loaded requestedStart requestedEnd inserted=Edited (prefix FT.>< repaired)
  where
    original=chunksTree loaded
    size=chunkCharacters (FT.measure original)
    start=max 0 (min size requestedStart)
    end=max start (min size requestedEnd)
    (before,fromStart)=FT.split ((>start).chunkCharacters) original
    (prefix,restart)=case FT.viewr before of
      rest FT.:> previous->(rest,previous FT.<| fromStart)
      FT.EmptyR->(FT.empty,fromStart)
    offset=chunkCharacters (FT.measure prefix)
    incoming=case FT.viewl restart of Chunk _ _ cursor _ _ FT.:< _->cursor; FT.EmptyL->initialSourceCursor
    parts=chunksFragments (Edited restart) 0 (start-offset)++[inserted]++chunksFragments (Edited original) end (size-end)
    (oldBefore,oldAfter)=FT.split ((>end).chunkCharacters) original
    (boundary,candidates)=if chunkCharacters (FT.measure oldBefore)==end
      then (end,oldAfter)
      else case FT.viewl oldAfter of
        chunk FT.:< rest->(chunkCharacters (FT.measure oldBefore)+chunkCharacters (FT.measure chunk),rest)
        FT.EmptyL->(size,FT.empty)
    repaired=buildChunks incoming parts (Just (boundary-offset+T.length inserted-(end-start),candidates))

-- Coalescing never allocates source bytes. A changed chunk spanning independent
-- arrays is copied once, bounded by its complete-item storage limit.
borrowedConcat :: [T.Text] -> T.Text
borrowedConcat input=case filter (not.T.null) input of
  []->T.empty
  first:rest->case foldl' (\joined next->joined >>= (`joinAdjacent` next)) (Just first) rest of
    Just joined->joined
    Nothing->T.concat (first:rest)

-- Every nonfinal input span supplies the maximum 128-byte item plus a complete
-- four-byte lookahead scalar. Artificial storage EOF must never finalize an item.
-- Only cross-array edges create a small temporary bridge; ordinary suffix bytes
-- remain slices of their original immutable array.
sourceSpan :: T.Text -> [T.Text] -> (T.Text,[T.Text])
sourceSpan text rest
  | T.null text, next:more<-rest=sourceSpan next more
  | TU.lengthWord8 text>=132 || null rest=(text,rest)
  | otherwise=case rest of
      []->(text,[])
      next:more | T.null next->sourceSpan text more
                | otherwise->case joinAdjacent text next of
                    Just joined->sourceSpan joined more
                    Nothing->let (headText,tailText)=T.splitAt 33 next
                             in sourceSpan (text<>headText) (tailText:more)

-- A repaired final fragment is not an EOF tail when it precedes reused
-- storage. Merge or redistribute only the last two repaired chunks; recutting
-- the whole periodic suffix would defeat persistent sharing.
prependBalanced :: Chunk -> ChunkTree -> ChunkTree
prependBalanced first rest=case FT.viewl rest of
  second@(Chunk m _ _ _ _) FT.:< suffix | chunkBytes m<64->
    balancePair first second FT.>< suffix
  _->first FT.<| rest

balancePair :: Chunk -> Chunk -> ChunkTree
balancePair (Chunk a left incoming _ _) (Chunk b right _ outgoing receipt)
  | bytes<=256=FT.singleton (Chunk (a<>b) source incoming outgoing receipt)
  | otherwise=case regularSplit source incoming of
      (# byte,chars,count,advance,middle,prefixOverflow #)->
        case measureItems source byte middle (total-count) receipt of
          (# end,restChars,restAdvance,_,_ #)->FT.fromList
            [Chunk (leafMeasure prefix chars count advance) prefix incoming middle prefixOverflow
            ,Chunk (leafMeasure suffix restChars (total-count) restAdvance) suffix middle outgoing receipt]
            where
              prefix=TU.takeWord8 byte source
              suffix=TU.takeWord8 (end-byte) (TU.dropWord8 byte source)
  where
    source=borrowedConcat [left,right]
    total=chunkItems a+chunkItems b
    bytes=chunkBytes a+chunkBytes b

-- Redistribute only a bounded pair. The item containing the midpoint can be
-- up to 128 bytes; choose its preceding edge if its ending edge leaves <64 bytes.
-- For a pair larger than 256 bytes a legal edge exists with both sides >=64.
regularSplit :: T.Text -> SourceCursor -> (# Int,Int,Int,ColumnAdvance,SourceCursor,Bool #)
regularSplit text incoming=go 0 0 0 mempty incoming False
  where
    size=TU.lengthWord8 text
    target=size `div` 2
    go !byte !chars !count !advance !cursor receipt=case sourceItemStep text byte cursor of
      (# end,n,width,tab,overflow,next #)->
        let part=if tab then Tab 0 0 else Add width
        in if end<target then go end (chars+n) (count+1) (advance<>part) next overflow
           else if size-end<64 then (# byte,chars,count,advance,cursor,receipt #)
           else (# end,chars+n,count+1,advance<>part,next,overflow #)

-- Measure bounded recut storage against the complete combined source. Its last
-- overflow flag comes from real source lookahead, not the combined span's EOF.
measureItems :: T.Text -> Int -> SourceCursor -> Int -> Bool -> (# Int,Int,ColumnAdvance,SourceCursor,Bool #)
measureItems text start initial count lastOverflow=go start 0 mempty initial count False
  where
    go !byte !chars !advance !cursor remaining receipt
      | remaining<=0=(# byte,chars,advance,cursor,receipt #)
      | otherwise=case sourceItemStep text byte cursor of
          (# end,n,width,tab,overflow,next #)->
            let final=remaining==1 && lastOverflow
                part=if final || overflow then Add 1 else if tab then Tab 0 0 else Add width
            in go end (chars+n) (advance<>part) next (remaining-1) (final || overflow)

leafMeasure :: T.Text -> Int -> Int -> ColumnAdvance -> ChunkMeasure
leafMeasure text chars count advance=ChunkMeasure (TU.lengthWord8 text) chars count advance hash (16777619^chars) flags
  where
    hash=T.foldl' (\h c->h*16777619+fromIntegral (ord c)+1) 0 text
    flags=(if T.any (=='\0') text then 1 else 0) .|. (if T.isInfixOf (T.pack "\r\n") text then 2 else 0) .|.
      (if T.isSuffixOf (T.singleton '\n') text then 4 else 0) .|.
      ((TU.lengthWord8 text-TU.lengthWord8 (T.dropWhileEnd (\c->c=='\r'||c=='\n') text)) `shiftL` 3)

-- The same streaming owner prepares initial storage and repairs edited source.
-- Only chunk cuts retain cursors and Text fragments; the item/scalar loops keep
-- numeric state. An unchanged suffix receipt is checked before consuming it.
buildChunks :: SourceCursor -> [T.Text] -> Maybe (Int,ChunkTree) -> ChunkTree
buildChunks initial source candidates=items first 0 0 [] 0 0 0 (-1) 0 initial initial False 0 False rest candidates
  where
    slice text start end=TU.takeWord8 (end-start) (TU.dropWord8 start text)
    advanceCandidates position current=case current of
      Just (boundary,tree) | boundary<position->case FT.viewl tree of
        chunk FT.:< later->advanceCandidates position (Just (boundary+chunkCharacters (FT.measure chunk),later))
        FT.EmptyL->Nothing
      _->current
    reusable position cursor bytes haveCut current=case current of
      Just (boundary,tree) | boundary==position && (bytes==0 || bytes>=64 || haveCut)->case FT.viewl tree of
        Chunk _ _ incoming _ _ FT.:< _ | cursor==incoming->Just tree
        _->Nothing
      _->Nothing
    pack text byte start pieces n count prefix suffix incoming outgoing receipt=
      let source=borrowedConcat (reverse (slice text start byte:pieces))
          advance=if prefix<0 then Add suffix else Tab prefix suffix
      in Chunk (leafMeasure source n count advance) source incoming outgoing receipt
    items !text !byte !start !pieces !stored !n !count !tabPrefix !tabSuffix !cursor !incoming !receipt !offset !haveCut following pending=
      let position=offset+n; pending'=advanceCandidates position pending
          scalarLimit=case pending' of Just (boundary,_)->max 1 (boundary-position); Nothing->maxBound
      in case reusable position cursor (stored+byte-start) haveCut pending' of
        Just suffix->if count==0 then suffix else pack text byte start pieces n count tabPrefix tabSuffix incoming cursor receipt FT.<| suffix
        Nothing
          | byte>=TU.lengthWord8 text && null following->if count==0 then FT.empty else FT.singleton (pack text byte start pieces n count tabPrefix tabSuffix incoming cursor receipt)
          | TU.lengthWord8 text-byte<132 && not (null following)->
              let consumed=slice text start byte
                  (next,more)=sourceSpan (TU.dropWord8 byte text) following
              in items next 0 0 (consumed:pieces) (stored+byte-start) n count tabPrefix tabSuffix cursor incoming receipt offset haveCut more pending'
          | otherwise->case sourceSpanStep text byte cursor (start+128-stored) scalarLimit (not (null following)) of
              (# end,scalars,itemsAdded,prefix,suffix,overflow,next #)->
                let n'=n+scalars; count'=count+itemsAdded
                    prefix'=if prefix<0 then tabPrefix else if tabPrefix<0 then tabSuffix+prefix else tabPrefix
                    suffix'=if prefix<0 then tabSuffix+suffix else if tabPrefix<0 then suffix else nextTab (tabSuffix+prefix)+suffix
                in if stored+end-start>=128 then
                    prependBalanced (pack text end start pieces n' count' prefix' suffix' incoming next overflow)
                      (items text end end [] 0 0 0 (-1) 0 next next overflow (offset+n') True following pending')
                   else items text end start pieces stored n' count' prefix' suffix' next incoming overflow offset haveCut following pending'
    (first,rest)=sourceSpan T.empty source
