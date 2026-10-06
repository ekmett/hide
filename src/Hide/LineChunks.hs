{-# LANGUAGE BangPatterns, MagicHash, MultiParamTypeClasses, UnboxedTuples #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Persistent borrowed storage inside a long physical source line.
--
-- Loaded text shares lazy geometric vector blocks of complete bounded-item receipts.
-- Exact seeks reuse those receipts; editing shares raw ranges of immutable
-- owners in a measured tree with persistent local repair. Measures compose raw
-- tab-dependent advance and exact-content rejection fingerprints.
-- Whole text and exact loaded width remain independent of the receipt stream.
module Hide.LineChunks
  ( Chunks, RawMeasure(..), ColumnAdvance(..), applyAdvance
  , chunksFromText, chunksEdit, chunksRawMeasure, chunksFlags, chunksWidth, chunksText, chunksPieces, chunksSlice, chunksFragments
  , chunksWindow, chunksExtentThrough, chunksDisplayColumn, chunksColumnOffset
  , chunksPreviousCharacter, chunksSpanLeft, chunksSpanRight, chunksSuffixWidth
  ) where

import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Word (Word64)
import qualified Data.FingerTree as FT
import qualified Data.Vector as V
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

-- A loaded line memoizes only demanded receipt blocks. The Text and exact width
-- remain independent of that index; exports/outer metadata must not force it.
-- Editing retains direct ranges of this owner; no display adoption or Buffer mutation.
-- Disjoint geometric blocks hold 1,2,4,... complete receipts. The tail MUST
-- remain lazy: a small query must not construct the next checkpoint. There is
-- no separately retained receipt list. Each entry owns its cached prefix once;
-- immutable binary searches never construct split trees or cumulative measures.
data LoadedEntry = LoadedEntry {-# UNPACK #-} !Int {-# UNPACK #-} !Int !ColumnAdvance !Word64 {-# UNPACK #-} !Int !Chunk
data LoadedBlocks = LoadedBlock {-# UNPACK #-} !Int {-# UNPACK #-} !Int !ColumnAdvance !Word64 {-# UNPACK #-} !Int !(V.Vector LoadedEntry) LoadedBlocks | LoadedEnd

-- Raw storage measures never demand display preparation. The final field MUST
-- remain lazy: scalar/byte/hash tree splits must not inspect a suffix advance.
-- @rawHash (a <> b) == rawHash a * rawFactor b + rawHash b@ (Word64 arithmetic).
data RawMeasure = RawMeasure
  { rawBytes :: !Int, rawCharacters :: !Int, rawHash :: !Word64, rawFactor :: !Word64
  , rawAdvance :: ColumnAdvance
  }
instance Semigroup RawMeasure where
  RawMeasure a b h p w <> RawMeasure c d g q v=RawMeasure (a+c) (b+d) (h*q+g) (p*q) (w<>v)
instance Monoid RawMeasure where mempty=RawMeasure 0 0 0 1 mempty

-- An immutable receipt owner, never a previous Edited root or pending recipe.
-- Its complete advance closes over this index independently of range queries;
-- reusing the raw seed cannot recursively demand that same seed's advance.
data SourceOwner = SourceOwner !T.Text !RawMeasure !Int !SourceCursor !Bool LoadedBlocks

-- Edited storage refers directly to immutable owners, never a prior edit root.
-- Piece edges are proven contextual receipt edges; raw measures stay strict
-- except for the exact advance. The unchanged suffix may be a single raw piece.
data Piece = Piece !RawMeasure !SourceOwner !Int !Int
instance FT.Measured RawMeasure Piece where measure (Piece measure _ _ _)=measure
type PieceTree = FT.FingerTree RawMeasure Piece

data Chunks = Loaded !SourceOwner Int | Edited !PieceTree !Int
instance Show Chunks where showsPrec p chunks=showsPrec p (chunksText chunks)

pieceText :: Piece -> T.Text
pieceText (Piece measure (SourceOwner text _ _ _ _ _) _ byte)=
  TU.takeWord8 (rawBytes measure) (TU.dropWord8 byte text)

pieceRange :: SourceOwner -> ReceiptEdge -> ReceiptEdge -> Piece
pieceRange owner a@(ReceiptEdge scalar byte _ _ _ _ _) b=
  Piece (ownerRangeMeasure owner a b) owner scalar byte

-- Promotion installs the already-owned raw seed, without visiting receipts.
piecesTree :: Chunks -> PieceTree
piecesTree (Loaded owner@(SourceOwner _ seed _ _ _ _) _)=FT.singleton (Piece seed owner 0 0)
piecesTree (Edited tree _)=tree

-- Start at one receipt. Cumulative checkpoint sizes are 1,3,7,... receipts,
-- strictly less than twice the first demanded receipt count at each expansion.
-- The temporary reverse list is consumed into an exactly sized vector, leaving
-- only the unconsumed source tail in the next lazy checkpoint.
loadedBlocks :: [Chunk] -> LoadedBlocks
loadedBlocks=build (1::Int)
  where
    build count source=case collect count 0 0 mempty 0 0 [] source of
      (_,_,_,_,_,[],_)->LoadedEnd
      (chars,bytes,advance,hash,tabs,entries,rest)->
        LoadedBlock chars bytes advance hash tabs (V.fromListN (length entries) (reverse entries)) (build next rest)
      where next=if count>maxBound `div` 2 then maxBound else count*2
    collect 0 !chars !bytes !advance !hash !tabs entries rest=(chars,bytes,advance,hash,tabs,entries,rest)
    collect _ !chars !bytes !advance !hash !tabs entries []=(chars,bytes,advance,hash,tabs,entries,[])
    collect count !chars !bytes !advance !hash !tabs entries (chunk:rest)=
      let m=FT.measure chunk
          entry=LoadedEntry chars bytes advance hash tabs chunk
          hasTab=case chunkAdvance m of Add _->0; Tab _ _->1
      in entry `seq` collect (count-1) (chars+chunkCharacters m) (bytes+chunkBytes m)
        (advance<>chunkAdvance m) (hash*chunkFactor m+chunkHash m) (tabs+hasTab) (entry:entries) rest

loadedChunks :: LoadedBlocks -> [Chunk]
loadedChunks LoadedEnd=[]
loadedChunks (LoadedBlock _ _ _ _ _ entries rest)=loadedSuffix 0 entries rest

loadedSuffix :: Int -> V.Vector LoadedEntry -> LoadedBlocks -> [Chunk]
loadedSuffix start entries rest=go start
  where
    go index | index>=V.length entries=loadedChunks rest
             | otherwise=case V.unsafeIndex entries index of
                 LoadedEntry _ _ _ _ _ chunk->chunk:go (index+1)

-- The endpoint of an entry is the following cached prefix, or the block total.
-- Binary predicates inspect those scalars directly; no per-probe transform is
-- composed and no source receipt is replayed.
loadedIndex :: (Int->Bool) -> V.Vector LoadedEntry -> Int
loadedIndex predicate entries=go 0 (V.length entries)
  where
    go !lo !hi | lo>=hi=lo
               | predicate mid=go lo mid
               | otherwise=go (mid+1) hi
      where mid=lo+(hi-lo) `div` 2

-- | Owning raw metadata. Loaded storage returns its original seed without
-- preparing receipts; byte/scalar/hash fields never demand 'rawAdvance'.
chunksRawMeasure :: Chunks -> RawMeasure
chunksRawMeasure (Loaded (SourceOwner _ seed _ _ _ _) _)=seed
chunksRawMeasure (Edited tree _)=FT.measure tree

-- | Exact line-owned NUL/CRLF/LF/trailing-terminator flags, independent of the
-- loaded display index. Trailing CR/LF count may be arbitrarily long.
chunksFlags :: Chunks -> Int
chunksFlags (Loaded (SourceOwner _ _ flags _ _ _) _)=flags
chunksFlags (Edited _ flags)=flags

-- A proven stored receipt edge in one immutable owner. Raw coordinates/hash
-- are strict; display facts MUST stay lazy. In particular constructing EOF for
-- a suffix raw measure must use the seed, not prepare the whole display index.
-- The final Boolean is the preceding item's proven overflow at this edge,
-- including interior artificial EOF. Edges belong to that same owner;
-- fingerprints never prove it.
data ReceiptEdge = ReceiptEdge !Int !Int !Word64 Int Int SourceCursor Bool

-- Normalize to the preceding stored receipt edge. An interior scalar is not an
-- installable Piece edge: later splice repair must recut its bounded receipt.
ownerReceiptEdge :: SourceOwner -> Int -> ReceiptEdge
ownerReceiptEdge (SourceOwner _ seed _ incoming finalOverflow blocks) requested
  | goal<=0=ReceiptEdge 0 0 0 0 0 incoming False
  | goal>=rawCharacters seed=ReceiptEdge (rawCharacters seed) (rawBytes seed) (rawHash seed)
      (applyAdvance (loadedAdvance blocks) 0) (endTabs blocks) (endCursor incoming blocks) (endOverflow finalOverflow blocks)
  | otherwise=find 0 0 0 0 0 False blocks
  where
    goal=max 0 requested
    find !char !byte !hash !tabs !col previousOverflow (LoadedBlock chars bytes advance blockHash blockTabs entries rest)
      | char+chars<=goal=find (char+chars) (byte+bytes) (hash*16777619^chars+blockHash)
          (tabs+blockTabs) (applyAdvance advance col) (entryOverflow entries (V.length entries-1)) rest
      | otherwise=case V.unsafeIndex entries index of
          LoadedEntry scalar offset prefix localHash localTabs (Chunk _ _ cursor _ _)->
            ReceiptEdge (char+scalar) (byte+offset) (hash*16777619^scalar+localHash)
              (applyAdvance prefix col) (tabs+localTabs) cursor
              (if index==0 then previousOverflow else entryOverflow entries (index-1))
      where
        index=loadedIndex (\i->char+endpoint i>goal) entries
        endpoint i | i+1==V.length entries=chars
                   | otherwise=case V.unsafeIndex entries (i+1) of LoadedEntry scalar _ _ _ _ _->scalar
    find _ _ _ _ _ _ LoadedEnd=error "Source receipt ended before its raw extent."
    entryOverflow entries index=case V.unsafeIndex entries index of
      LoadedEntry _ _ _ _ _ (Chunk _ _ _ _ overflow)->overflow
    endTabs LoadedEnd=0
    endTabs (LoadedBlock _ _ _ _ tabs _ rest)=tabs+endTabs rest
    endCursor fallback LoadedEnd=fallback
    endCursor _ (LoadedBlock _ _ _ _ _ entries rest)=case V.last entries of
      LoadedEntry _ _ _ _ _ (Chunk _ _ _ outgoing _)->endCursor outgoing rest
    endOverflow fallback LoadedEnd=fallback
    endOverflow _ (LoadedBlock _ _ _ _ _ entries rest)=case V.last entries of
      LoadedEntry _ _ _ _ _ (Chunk _ _ _ _ overflow)->endOverflow (overflow || finalOverflow) rest

-- Exact raw range at stored receipt endpoints. Its advance closes over only the
-- immutable owner/edges, never a prior Piece or Edited root. Whole-owner reuse
-- returns the seed; that seed's advance is the independent loadedAdvance fold.
-- H[a,b) = H(b) - H(a)*B^(b-a); factors count scalars, never UTF8 bytes.
ownerRangeMeasure :: SourceOwner -> ReceiptEdge -> ReceiptEdge -> RawMeasure
ownerRangeMeasure owner@(SourceOwner _ seed _ _ _ _) a@(ReceiptEdge start firstByte firstHash _ _ _ _) b@(ReceiptEdge end lastByte lastHash _ _ _ _)
  | end<=start=mempty
  | start==0 && end==rawCharacters seed=seed
  | otherwise=RawMeasure (lastByte-firstByte) count (lastHash-firstHash*factor) factor (ownerRangeAdvance owner a b)
  where
    count=end-start
    factor=16777619^count

-- Prefix Tab transforms cannot be inverted. Locate the first tab-containing
-- receipt using monotone cached counts; its local Tab prefix and endpoint
-- columns yield an exact transform for every incoming column residue.
ownerRangeAdvance :: SourceOwner -> ReceiptEdge -> ReceiptEdge -> ColumnAdvance
ownerRangeAdvance (SourceOwner _ _ _ _ _ blocks) (ReceiptEdge _ _ _ firstColumn firstTabs _ _) (ReceiptEdge _ _ _ lastColumn lastTabs _ _)
  | firstTabs==lastTabs=Add (lastColumn-firstColumn)
  | otherwise=case firstTab 0 0 blocks of
      (column,prefix)->Tab (column-firstColumn+prefix) (lastColumn-nextTab (column+prefix))
  where
    firstTab !tabs !col (LoadedBlock _ _ advance _ count entries rest)
      | tabs+count<=firstTabs=firstTab (tabs+count) (applyAdvance advance col) rest
      | otherwise=case V.unsafeIndex entries index of
          LoadedEntry _ _ before _ _ (Chunk m _ _ _ _)->case chunkAdvance m of
            Tab prefix _->(applyAdvance before col,prefix)
            Add _->error "Tab receipt count selected an additive receipt."
      where
        index=loadedIndex (\i->tabs+endpoint i>firstTabs) entries
        endpoint i | i+1==V.length entries=count
                   | otherwise=case V.unsafeIndex entries (i+1) of LoadedEntry _ _ _ _ n _->n
    firstTab _ _ LoadedEnd=error "Missing tab receipt in source range."

-- | Exact width is memoized independently of loaded receipts. Its first use
-- scans the original row; it does not dice an undemanded row into spans.
chunksWidth :: Chunks -> Int
chunksWidth (Loaded _ width)=width
chunksWidth (Edited tree _)=applyAdvance (rawAdvance (FT.measure tree)) 0

-- | Explicit whole-line read, for serialization and worker-owned consumers.
chunksText :: Chunks -> T.Text
chunksText (Loaded (SourceOwner text _ _ _ _ _) _)=text
chunksText (Edited tree _)=T.concat (map pieceText (toList tree))

-- | Raw stored payloads in source order, without scalar slicing or display
-- preparation. A loaded row yields its original text without forcing receipts;
-- an edited row joins adjacent leaves of the same immutable array without
-- copying. Independent arrays remain separate.
chunksPieces :: Chunks -> [T.Text]
chunksPieces (Loaded (SourceOwner text _ _ _ _ _) _)=[text]
chunksPieces (Edited tree _)=case map pieceText (toList tree) of
  []->[]
  text:rest->gather text rest
  where
    gather text []=[text]
    gather text (next:rest)=case joinAdjacent text next of
      Just joined->gather joined rest
      Nothing->text:gather next rest

-- Raw byte endpoints are known directly at owner ends. Interior reads prepare
-- only the selected stored receipt, then count its bounded scalar prefix.
ownerByteAt :: SourceOwner -> Int -> Int
ownerByteAt owner@(SourceOwner text seed _ _ _ blocks) scalar
  | scalar<=0=0
  | scalar>=rawCharacters seed=rawBytes seed
  | otherwise=case ownerReceiptEdge owner scalar of
      ReceiptEdge base byte _ _ _ _ _->case seekLoadedScalar base blocks of
        (# _,_,Chunk _ source _ _ _:_ #)->byte+TU.lengthWord8 (T.take (scalar-base) source)
        _->TU.lengthWord8 text

pieceFragments :: Piece -> Int -> Int -> T.Text
pieceFragments piece@(Piece measure owner scalar byte) skip count
  | skip<=0 && count>=rawCharacters measure=pieceText piece
  | otherwise=TU.takeWord8 (end-start) (TU.dropWord8 (start-byte) (pieceText piece))
  where
    start=ownerByteAt owner (scalar+skip)
    end=ownerByteAt owner (scalar+skip+count)

-- | Borrow only the leaves overlapping a clamped scalar range. Both boundary
-- leaves inspect bounded local byte spans; middle leaves remain whole.
chunksFragments :: Chunks -> Int -> Int -> [T.Text]
chunksFragments (Loaded (SourceOwner text _ _ _ _ spans) _) requested count
  | count<=0=[]
  | requested<=0=[T.take count text]
  | otherwise=case seekLoadedScalar (max 0 requested) spans of
      (# base,_,suffix #)->fragmentsFrom (max 0 requested-I# base) count suffix
chunksFragments (Edited tree _) requested count=go offset (max 0 count) (toList suffix)
  where
    start=max 0 (min (rawCharacters (FT.measure tree)) requested)
    (prefix,suffix)=FT.split ((>start).rawCharacters) tree
    offset=start-rawCharacters (FT.measure prefix)
    go _ remaining _ | remaining<=0=[]
    go _ _ []=[]
    go skip remaining (piece:rest)=
      let size=min remaining (rawCharacters (FT.measure piece)-skip)
      in pieceFragments piece skip size:go 0 (remaining-size) rest
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
chunksWindow (Loaded (SourceOwner _ _ _ _ _ spans) _) requested=case seekLoadedColumn (max 0 requested) spans of
  (# char,col,_,[] #)->(I# char,I# col,[])
  (# char,col,_,chunk:rest #)->windowSuffix (max 0 requested) (I# char) (I# col) chunk rest
chunksWindow (Edited tree _) requested=case seekEditedColumn (max 0 requested) tree of
  (# char,col,_,[] #)->(I# char,I# col,[])
  (# char,col,_,chunk:rest #)->windowSuffix (max 0 requested) (I# char) (I# col) chunk rest

-- Receipt enumeration is only a viewport/repair tail. It never builds a
-- display tree or a retained second stream for an edited row.
pieceChunks :: Piece -> [Chunk]
pieceChunks (Piece measure (SourceOwner _ _ _ _ _ blocks) scalar _)=
  case seekLoadedScalar scalar blocks of
    (# _,_,suffix #)->takeChunks (rawCharacters measure) suffix

takeChunks :: Int -> [Chunk] -> [Chunk]
takeChunks remaining _ | remaining<=0=[]
takeChunks _ []=[]
takeChunks remaining (chunk:rest)=chunk:takeChunks (remaining-chunkCharacters (FT.measure chunk)) rest

treeChunks :: PieceTree -> [Chunk]
treeChunks=concatMap pieceChunks . toList

-- A column query probes bounded scalar prefixes, then normalizes once through
-- the selected receipt. The selected piece's full advance is never demanded.
seekEditedColumn :: Int -> PieceTree -> (# Int#,Int#,Int#,[Chunk] #)
seekEditedColumn goal tree
  | point>=size=finish size (prefixColumn size) (rawBytes (FT.measure tree)) []
  | otherwise=case FT.viewl suffix of
      Piece measure owner@(SourceOwner _ _ _ _ _ blocks) start byte FT.:< rest->
        case ownerReceiptEdge owner (start+local) of
          edge@(ReceiptEdge scalar offset _ _ _ _ _)->case seekLoadedScalar scalar blocks of
            (# _,_,selected #)->finish (base+scalar-start)
              (applyAdvance (ownerRangeAdvance owner (ownerReceiptEdge owner start) edge) initial)
              (rawBytes before+offset-byte) (takeChunks (start+rawCharacters measure-scalar) selected++treeChunks rest)
            _->finish size (prefixColumn size) (rawBytes (FT.measure tree)) []
      FT.EmptyL->finish size (prefixColumn size) (rawBytes (FT.measure tree)) []
  where
    size=rawCharacters (FT.measure tree)
    prefixColumn=editedDisplayColumn tree
    point=gallop 0 (min 1 size)
    gallop !lo !hi
      | prefixColumn hi>goal=binary lo hi
      | hi>=size=size
      | otherwise=gallop hi (if hi>size `div` 2 then size else hi*2)
    binary !lo !hi
      | hi-lo<=1=editedPreviousCharacter tree hi
      | prefixColumn mid>goal=binary lo mid
      | otherwise=binary mid hi
      where mid=lo+(hi-lo) `div` 2
    (prefix,suffix)=FT.split ((>point).rawCharacters) tree
    before=FT.measure prefix
    base=rawCharacters before
    local=point-base
    initial=applyAdvance (rawAdvance before) 0
    finish (I# char) (I# col) (I# byte) chunks=(# char,col,byte,chunks #)

-- Primitive endpoint receipts keep the cached-span walk numeric. Box the
-- public coordinates only at its selected edge, never once per skipped block.
seekLoadedColumn :: Int -> LoadedBlocks -> (# Int#,Int#,Int#,[Chunk] #)
seekLoadedColumn goal=go 0 0 0
  where
    go !char !col !byte LoadedEnd=finish char col byte []
    go !char !col !byte (LoadedBlock chars bytes advance _ _ entries rest)
      | next<=goal=go (char+chars) next (byte+bytes) rest
      | otherwise=case V.unsafeIndex entries index of
          LoadedEntry scalar offset prefix _ _ _->finish (char+scalar) (applyAdvance prefix col)
            (byte+offset) (loadedSuffix index entries rest)
      where
        next=applyAdvance advance col
        index=loadedIndex (\i->applyAdvance (endpoint i) col>goal) entries
        endpoint i | i+1==V.length entries=advance
                   | otherwise=case V.unsafeIndex entries (i+1) of LoadedEntry _ _ prefix _ _ _->prefix
    finish (I# char) (I# col) (I# byte) suffix=(# char,col,byte,suffix #)

-- | Seek only through the requested column. Return reached scalar/column and
-- an upper extent: known prefix plus at most eight cells per remaining byte.
-- Tabs are the largest source advance. This never demands an exact full-row
-- width, whether the row is loaded or edited.
chunksExtentThrough :: Chunks -> Int -> (Int,Int,Int)
chunksExtentThrough (Loaded (SourceOwner text _ _ _ _ spans) _) requested=case seekLoadedColumn goal spans of
  (# char,col,_,[] #)->(I# char,I# col,I# col)
  (# char,col,byte,Chunk _ leaf incoming _ overflow:_ #)->
    let (local,used,column,_)=sourceLeafFrom goal (I# col) incoming overflow leaf
        remaining=TU.lengthWord8 text-I# byte-used
        upper=if remaining>(maxBound-column) `div` 8 then maxBound else column+remaining*8
    in (I# char+local,column,upper)
  where goal=max 0 requested
chunksExtentThrough (Edited tree _) requested=case seekEditedColumn (max 0 requested) tree of
  (# char,col,_,[] #)->(I# char,I# col,I# col)
  (# char,col,byte,Chunk _ leaf incoming _ overflow:_ #)->
    let (local,used,column,_)=sourceLeafFrom (max 0 requested) (I# col) incoming overflow leaf
        remaining=rawBytes (FT.measure tree)-I# byte-used
        upper=if remaining>(maxBound-column) `div` 8 then maxBound else column+remaining*8
    in (I# char+local,column,upper)

seekLoadedScalar :: Int -> LoadedBlocks -> (# Int#,Int#,[Chunk] #)
seekLoadedScalar goal=go 0 0
  where
    go !char !col LoadedEnd=finish char col []
    go !char !col (LoadedBlock chars _ advance _ _ entries rest)
      | char+chars<=goal=go (char+chars) (applyAdvance advance col) rest
      | otherwise=case V.unsafeIndex entries index of
          LoadedEntry scalar _ prefix _ _ _->finish (char+scalar) (applyAdvance prefix col)
            (loadedSuffix index entries rest)
      where
        index=loadedIndex (\i->char+endpoint i>goal) entries
        endpoint i | i+1==V.length entries=chars
                   | otherwise=case V.unsafeIndex entries (i+1) of LoadedEntry scalar _ _ _ _ _->scalar
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
chunksSuffixWidth (Loaded (SourceOwner original _ _ _ _ _) _) start count bound=go 0 0 initialSourceCursor 0
  where
    text=T.drop start original
    go !byte !chars !cursor !col
      | col>=bound=max 0 bound
      | chars>=count || byte>=TU.lengthWord8 text=col
      | otherwise=case sourceItemStep text byte cursor of
          (# end,n,width,tab,_,next #)->
            if chars+n>count then col else go end (chars+n) next (if tab then nextTab col else col+width)
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
chunksDisplayColumn (Loaded (SourceOwner _ _ _ _ _ spans) _) requested=case seekLoadedScalar (max 0 requested) spans of
  (# _,col,[] #)->I# col
  (# char,col,chunk:_ #)->chunkDisplayColumn (max 0 requested-I# char) (I# col) chunk
chunksDisplayColumn (Edited tree _) requested=editedDisplayColumn tree requested

editedDisplayColumn :: PieceTree -> Int -> Int
editedDisplayColumn tree requested=case FT.viewl suffix of
  FT.EmptyL->initialColumn
  Piece _ owner@(SourceOwner _ _ _ _ _ blocks) scalar _ FT.:< _->
    case ownerReceiptEdge owner (scalar+local) of
      edge@(ReceiptEdge base _ _ _ _ _ _)->
        let column=applyAdvance (ownerRangeAdvance owner (ownerReceiptEdge owner scalar) edge) initialColumn
        in if base==scalar+local then column else case seekLoadedScalar base blocks of
          (# _,_,chunk:_ #)->chunkDisplayColumn (scalar+local-base) column chunk
          _->column
  where
    goal=max 0 (min requested (rawCharacters (FT.measure tree)))
    (prefix,suffix)=FT.split ((>goal).rawCharacters) tree
    initialColumn=applyAdvance (rawAdvance (FT.measure prefix)) 0
    local=goal-rawCharacters (FT.measure prefix)

chunkDisplayColumn :: Int -> Int -> Chunk -> Int
chunkDisplayColumn local initialColumn (Chunk _ text cursor _ receipt)=
  sourceScalarColumn local initialColumn cursor receipt text

-- | Display hits return an original scalar boundary, using the same seek as
-- visible emission. Zero-width items before the hit are consumed identically.
chunksColumnOffset :: Chunks -> Int -> Int
chunksColumnOffset tree goal=let (char,_,_)=chunksWindow tree goal in char

-- | Previous complete-item boundary, including interior scalar positions.
chunksPreviousCharacter :: Chunks -> Int -> Int
chunksPreviousCharacter (Loaded (SourceOwner _ _ _ _ _ spans) _) requested
  | requested<=0=0
  | otherwise=case seekLoadedScalar (requested-1) spans of
      (# base,_,[] #)->I# base
      (# base,_,chunk:_ #)->I# base+chunkPreviousCharacter (requested-I# base) chunk
chunksPreviousCharacter (Edited tree _) requested=editedPreviousCharacter tree requested

editedPreviousCharacter :: PieceTree -> Int -> Int
editedPreviousCharacter tree requested
  | goal<=0=0
  | otherwise=case FT.viewl suffix of
      Piece _ owner@(SourceOwner _ _ _ _ _ blocks) scalar _ FT.:< _->
        case ownerReceiptEdge owner (scalar+local-1) of
          ReceiptEdge start _ _ _ _ _ _->case seekLoadedScalar start blocks of
            (# _,_,chunk:_ #)->base+start-scalar+chunkPreviousCharacter (scalar+local-start) chunk
            _->goal
      FT.EmptyL->goal
  where
    goal=min requested (rawCharacters (FT.measure tree))
    (prefix,suffix)=FT.split ((>=goal).rawCharacters) tree
    base=rawCharacters (FT.measure prefix)
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
chunksSpanLeft predicate (Loaded (SourceOwner _ _ _ _ _ blocks) _) requested=find 0 [] blocks
  where
    goal=max 0 requested
    find !base previous LoadedEnd=go base T.empty previous
    find !base previous (LoadedBlock chars _ _ _ _ entries rest)
      | base+chars<goal=find (base+chars) ((entries,V.length entries-1):previous) rest
      | otherwise=case V.unsafeIndex entries index of
          LoadedEntry scalar _ _ _ _ (Chunk _ text _ _ _)->
            go goal (T.take (goal-base-scalar) text) ((entries,index-1):previous)
      where
        index=loadedIndex (\i->base+endpoint i>=goal) entries
        endpoint i | i+1==V.length entries=chars
                   | otherwise=case V.unsafeIndex entries (i+1) of LoadedEntry scalar _ _ _ _ _->scalar
    go !offset text previous=
      let consumed=T.takeWhileEnd predicate text; next=offset-T.length consumed
      in if TU.lengthWord8 consumed<TU.lengthWord8 text then next else earlier next previous
    earlier !offset []=offset
    earlier !offset ((entries,index):rest)
      | index<0=earlier offset rest
      | otherwise=case V.unsafeIndex entries index of
          LoadedEntry _ _ _ _ _ (Chunk _ source _ _ _)->go offset source ((entries,index-1):rest)
chunksSpanLeft predicate (Edited tree _) requested=case FT.viewl suffix of
  piece FT.:< _->go goal (pieceFragments piece 0 (goal-base)) prefix
  FT.EmptyL->go goal T.empty prefix
  where
    goal=max 0 (min requested (rawCharacters (FT.measure tree)))
    (prefix,suffix)=FT.split ((>=goal).rawCharacters) tree
    base=rawCharacters (FT.measure prefix)
    go !offset text previous=
      let consumed=T.takeWhileEnd predicate text; next=offset-T.length consumed
      in if TU.lengthWord8 consumed<TU.lengthWord8 text then next else case FT.viewr previous of
        earlier FT.:> piece->go next (pieceText piece) earlier
        FT.EmptyR->next

-- | Forward scalar-class traversal, sharing the same physical storage owner.
chunksSpanRight :: (Char->Bool) -> Chunks -> Int -> Int
chunksSpanRight predicate (Loaded (SourceOwner _ _ _ _ _ spans) _) requested=case seekLoadedScalar (max 0 requested) spans of
  (# base,_,[] #)->I# base
  (# base,_,Chunk _ text _ _ _:rest #)->go (max 0 requested) (T.drop (max 0 requested-I# base) text) rest
  where
    go !offset text following=
      let consumed=T.takeWhile predicate text; next=offset+T.length consumed
      in if TU.lengthWord8 consumed<TU.lengthWord8 text then next else case following of
        Chunk _ source _ _ _:later->go next source later
        []->next
chunksSpanRight predicate (Edited tree _) requested=case FT.viewl suffix of
  piece FT.:< rest->go goal (pieceFragments piece (goal-base) (rawCharacters (FT.measure piece)-(goal-base))) rest
  FT.EmptyL->goal
  where
    goal=max 0 (min requested (rawCharacters (FT.measure tree)))
    (prefix,suffix)=FT.split ((>goal).rawCharacters) tree
    base=rawCharacters (FT.measure prefix)
    go !offset text following=
      let consumed=T.takeWhile predicate text; next=offset+T.length consumed
      in if TU.lengthWord8 consumed<TU.lengthWord8 text then next else case FT.viewl following of
        piece FT.:< later->go next (pieceText piece) later
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
-- prepare a covering checkpoint with <2x receipt-count overscan. Editing shares
-- that immutable index through raw ranges; raw saves never prepare it.
chunksFromText :: T.Text -> Chunks
chunksFromText text=Loaded (SourceOwner text seed flags initialSourceCursor False blocks) width
  where
    blocks=loadedBlocks (loadedSpans text initialSourceCursor)
    (seed,flags)=rawSeed text (loadedAdvance blocks)
    (_,_,width,_)=sourceGraphemesFrom maxBound text

-- Independent complete-owner transform, demanded only by an explicit full
-- advance. Never implemented through ownerRangeMeasure's whole-seed shortcut.
loadedAdvance :: LoadedBlocks -> ColumnAdvance
loadedAdvance LoadedEnd=mempty
loadedAdvance (LoadedBlock _ _ advance _ _ _ rest)=advance<>loadedAdvance rest

-- One raw numeric pass supplies the outer Buffer long-line leaf as well as the
-- immutable owner. No Unicode segmentation, source copies or per-scalar records.
rawSeed :: T.Text -> ColumnAdvance -> (RawMeasure,Int)
rawSeed text advance=go 0 0 0 1 0 False 0 False
  where
    size=TU.lengthWord8 text
    go !byte !chars !hash !factor !flags !previousCR !trailing !lastLF
      | byte>=size=(RawMeasure size chars hash factor advance,
          flags .|. (if lastLF then 4 else 0) .|. (trailing `shiftL` 3))
      | otherwise=case TU.iter text byte of
          TU.Iter c delta->go (byte+delta) (chars+1) (hash*16777619+fromIntegral (ord c)+1)
            (factor*16777619) (flags .|. (if c=='\0' then 1 else 0) .|. (if previousCR && c=='\n' then 2 else 0))
            (c=='\r') (if c=='\r' || c=='\n' then trailing+1 else 0) (c=='\n')

-- Each tail is shared and remains lazy. The scanner sees the original full
-- source, so its outgoing cursor/overflow includes real lookahead at every cut.
loadedSpans :: T.Text -> SourceCursor -> [Chunk]
loadedSpans text=go 0
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
chunksEdit loaded requestedStart requestedEnd inserted=
  -- Force the raw result measure here: splice repair cannot escape as a recipe
  -- closing over a previous Edited tree. Its display advance stays lazy.
  let !measure=FT.measure result in rawCharacters measure `seq` Edited result flags
  where
    original=piecesTree loaded
    size=rawCharacters (FT.measure original)
    start=max 0 (min size requestedStart)
    end=max start (min size requestedEnd)
    restart=receiptBefore original (max 0 (start-1))
    (prefix,fromStart)=splitPieces restart original
    incoming=case FT.viewl fromStart of
      Piece _ owner scalar _ FT.:< _->edgeCursor (ownerReceiptEdge owner scalar)
      FT.EmptyL->initialSourceCursor
    boundary=receiptAfter original end
    (_,candidates)=splitPieces boundary original
    parts=chunksFragments loaded restart (start-restart)++[inserted]++chunksFragments loaded end (size-end)
    repaired=buildPieces incoming parts (Just (boundary-restart+T.length inserted-(end-start),candidates))
    result=prefix FT.>< repaired
    flags=editedFlags loaded start end inserted result

edgeCursor :: ReceiptEdge -> SourceCursor
edgeCursor (ReceiptEdge _ _ _ _ _ cursor _)=cursor

-- These splits install only proven stored edges. Raw EOF/hash comes from the
-- owner seed and does not demand the endpoint's cursor/column.
splitPieces :: Int -> PieceTree -> (PieceTree,PieceTree)
splitPieces goal tree=case FT.viewl after of
  FT.EmptyL->(before,after)
  piece@(Piece m owner start _) FT.:< rest
    | local<=0->(before,after)
    | local>=rawCharacters m->(before FT.|> piece,rest)
    | otherwise->let a=ownerReceiptEdge owner start
                     b=ownerReceiptEdge owner (start+local)
                     c=ownerReceiptEdge owner (start+rawCharacters m)
                 in (before FT.|> pieceRange owner a b,pieceRange owner b c FT.<| rest)
  where
    (before,after)=FT.split ((>goal).rawCharacters) tree
    local=goal-rawCharacters (FT.measure before)

receiptBefore :: PieceTree -> Int -> Int
receiptBefore tree goal=case FT.viewl rest of
  Piece _ owner start _ FT.:< _->case ownerReceiptEdge owner (start+goal-base) of
    ReceiptEdge scalar _ _ _ _ _ _->base+scalar-start
  FT.EmptyL->goal
  where
    (prefix,rest)=FT.split ((>goal).rawCharacters) tree
    base=rawCharacters (FT.measure prefix)

receiptAfter :: PieceTree -> Int -> Int
receiptAfter tree goal=case FT.viewl rest of
  Piece _ owner@(SourceOwner _ _ _ _ _ blocks) start _ FT.:< _->case ownerReceiptEdge owner (start+goal-base) of
    ReceiptEdge scalar _ _ _ _ _ _ | scalar==start+goal-base->goal
    ReceiptEdge scalar _ _ _ _ _ _->case seekLoadedScalar scalar blocks of
      (# _,_,chunk:_ #)->base+scalar-start+chunkCharacters (FT.measure chunk)
      _->goal
  FT.EmptyL->goal
  where
    (prefix,rest)=FT.split ((>goal).rawCharacters) tree
    base=rawCharacters (FT.measure prefix)

-- A bounded repair owner uses the actual prepared Chunk, preserving incoming
-- state and proven final overflow instead of segmenting artificial EOF again.
chunkPiece :: Chunk -> Piece
chunkPiece chunk@(Chunk m text incoming _ overflow)=Piece seed owner 0 0
  where
    seed=RawMeasure (chunkBytes m) (chunkCharacters m) (chunkHash m) (chunkFactor m) (chunkAdvance m)
    entry=LoadedEntry 0 0 mempty 0 0 chunk
    tabs=case chunkAdvance m of Add _->0; Tab _ _->1
    blocks=LoadedBlock (chunkCharacters m) (chunkBytes m) (chunkAdvance m) (chunkHash m) tabs (V.singleton entry) LoadedEnd
    owner=SourceOwner text seed (chunkFlags m) incoming overflow blocks

-- Remove one unchanged receipt from a transient candidate suffix, retaining a
-- direct range of the same immutable owner. Never build its remaining FT index.
dropReceipt :: PieceTree -> (Int,PieceTree)
dropReceipt tree=case FT.viewl tree of
  FT.EmptyL->(0,FT.empty)
  Piece m owner@(SourceOwner _ _ _ _ _ blocks) start _ FT.:< rest->case seekLoadedScalar start blocks of
    (# _,_,chunk:_ #)->let count=chunkCharacters (FT.measure chunk) in
      (count,if count>=rawCharacters m then rest else
        pieceRange owner (ownerReceiptEdge owner (start+count)) (ownerReceiptEdge owner (start+rawCharacters m)) FT.<| rest)
    _->(0,rest)

-- Same-row edits cannot insert LF. NUL-free ordinary edits need no remaining
-- source scan. Removing NUL may require an explicit raw scan; deleting into a
-- previously unknown trailing CR run may likewise inspect that raw run.
editedFlags :: Chunks -> Int -> Int -> T.Text -> PieceTree -> Int
editedFlags original start end inserted tree=nul .|. terminal .|. (trailing `shiftL` 3)
  where
    old=chunksFlags original
    size=rawCharacters (chunksRawMeasure original)
    oldTrailing=old `shiftR` 3
    texts=map pieceText (toList tree)
    nul | T.any (=='\0') inserted=1
        | old .&. 1==0=0
        | not (any (T.any (=='\0')) (chunksFragments original start (end-start)))=1
        | any (T.any (=='\0')) texts=1
        | otherwise=0
    tailStart=size-oldTrailing
    insertedLength=T.length inserted
    insertedTrailing=T.length (T.takeWhileEnd terminator inserted)
    terminator c=c=='\r'||c=='\n'
    trailing | end<tailStart=oldTrailing
             | insertedTrailing<insertedLength=size-end+insertedTrailing
             | start>=tailStart=oldTrailing-(end-start)+insertedLength
             | otherwise=size-end+insertedLength+prefixTrailing start (piecesTree original)
    -- Only a newly exposed, previously unknown prefix tail is inspected.
    -- The unchanged terminator suffix has an exact cached scalar count already.
    prefixTrailing point source=case FT.viewl suffix of
      FT.EmptyL->tailCount 0 T.empty prefix
      piece FT.:< _->tailCount 0 (pieceFragments piece 0 (point-rawCharacters (FT.measure prefix))) prefix
      where (prefix,suffix)=FT.split ((>=point).rawCharacters) source
    tailCount !n text previous=
      let tailText=T.takeWhileEnd terminator text; total=n+T.length tailText in
      if TU.lengthWord8 tailText<TU.lengthWord8 text then total else case FT.viewr previous of
        FT.EmptyR->total
        rest FT.:> piece->tailCount total (pieceText piece) rest
    lastTwo=takeEnd 2 tree
    takeEnd n remaining=case FT.viewr remaining of
      FT.EmptyR->T.empty
      rest FT.:> piece->let suffix=T.takeEnd n (pieceText piece) in
        if T.length suffix>=n then suffix else takeEnd (n-T.length suffix) rest<>suffix
    terminal | end<size-2=old .&. 6
             | otherwise=(if T.isSuffixOf (T.singleton '\n') lastTwo then 4 else 0) .|. (if T.isSuffixOf (T.pack "\r\n") lastTwo then 2 else 0)

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
prependPiece :: Chunk -> PieceTree -> PieceTree
prependPiece first rest=case FT.viewl rest of
  second FT.:< suffix | rawBytes (FT.measure second)<64->case pieceChunks second of
    [chunk]->FT.fromList (map chunkPiece (toList (balancePair first chunk))) FT.>< suffix
    _->chunkPiece first FT.<| rest
  _->chunkPiece first FT.<| rest

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
buildPieces :: SourceCursor -> [T.Text] -> Maybe (Int,PieceTree) -> PieceTree
buildPieces initial source candidates=items first 0 0 [] 0 0 0 (-1) 0 initial initial False 0 False rest candidates
  where
    slice text start end=TU.takeWord8 (end-start) (TU.dropWord8 start text)
    advanceCandidates position current=case current of
      Just (boundary,tree) | boundary<position->case FT.viewl tree of
        _ FT.:< _->let (count,later)=dropReceipt tree in advanceCandidates position (Just (boundary+count,later))
        FT.EmptyL->Nothing
      _->current
    reusable position cursor bytes haveCut current=case current of
      Just (boundary,tree) | boundary==position && (bytes==0 || bytes>=64 || haveCut)->case FT.viewl tree of
        Piece _ owner scalar _ FT.:< _ | cursor==edgeCursor (ownerReceiptEdge owner scalar)->Just tree
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
        Just suffix->if count==0 then suffix else chunkPiece (pack text byte start pieces n count tabPrefix tabSuffix incoming cursor receipt) FT.<| suffix
        Nothing
          | byte>=TU.lengthWord8 text && null following->if count==0 then FT.empty else FT.singleton (chunkPiece (pack text byte start pieces n count tabPrefix tabSuffix incoming cursor receipt))
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
                    prependPiece (pack text end start pieces n' count' prefix' suffix' incoming next overflow)
                      (items text end end [] 0 0 0 (-1) 0 next next overflow (offset+n') True following pending')
                   else items text end start pieces stored n' count' prefix' suffix' next incoming overflow offset haveCut following pending'
    (first,rest)=sourceSpan T.empty source
