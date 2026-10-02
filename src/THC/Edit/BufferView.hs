-- | Compact row mappings for change views. Text remains in the buffer tree.
module THC.Edit.BufferView
  ( BufferView(..), ReviewSide(..), ViewProjection, ViewRow(..)
  , bufferViewName, parseBufferView, buildViewProjection, viewRowCount, viewRowAt, viewRowForChange, viewChangeRanges
  ) where

import qualified Data.Vector as V

data BufferView = CurrentView | ChangesView | OnlyChangesView | SideBySideView
  deriving (Eq,Show,Enum,Bounded)
data ReviewSide = UnifiedSide | OriginalSide | CurrentSide
  deriving (Eq,Show,Enum,Bounded)

-- Full change-row indices, not editable offsets. A gap has no selectable text.
data ViewRow = ViewRow
  { viewLeftRow :: Maybe Int, viewRightRow :: Maybe Int, viewOmittedRows :: Int
  } deriving (Eq,Show)

data Piece = Lines Int Int | Hunk Int Int Int | Gap Int Int deriving (Eq,Show)
data Segment = Segment Int Piece deriving (Eq,Show)
data ViewProjection = ViewProjection Int (V.Vector Segment) (V.Vector Segment) deriving (Eq,Show)

-- | Build once per buffer tree, using only changed-run metadata. The triples
-- are (first change row, deleted rows, added rows), in source order.
buildViewProjection :: Int -> [(Int,Int,Int)] -> ViewProjection
buildViewProjection total hunks = ViewProjection total (index contextual) (index aligned)
  where
    spans=merge [(max 0 (start-2),min total (start+removed+added+2)) | (start,removed,added)<-hunks]
    contextual=if null hunks then [] else contexts 0 spans
    contexts pos []=[Gap pos (total-pos) | pos<total]
    contexts pos ((lo,hi):rest)=[Gap pos (lo-pos) | lo>pos]++[Lines lo (hi-lo) | hi>lo]++contexts hi rest
    aligned=align 0 hunks
    align pos []=[Lines pos (total-pos) | pos<total]
    align pos ((start,removed,added):rest)=
      [Lines pos (start-pos) | start>pos]++[Hunk start removed added]++align (start+removed+added) rest
    merge []=[]
    merge ((lo,hi):rest)=collect lo hi rest
    collect lo hi ((next,end):rest) | next<=hi=collect lo (max hi end) rest
    collect lo hi rest=(lo,hi):merge rest
    index=V.fromList . go 0
    go _ []=[]
    go row (piece:rest)=Segment row piece:go (row+pieceRows piece) rest

pieceRows :: Piece -> Int
pieceRows (Lines _ n)=n
pieceRows (Gap _ _)=1
pieceRows (Hunk _ removed added)=max removed added

sourceStart :: Piece -> Int
sourceStart (Lines start _)=start
sourceStart (Gap start _)=start
sourceStart (Hunk start _ _)=start

segments :: BufferView -> ViewProjection -> V.Vector Segment
segments OnlyChangesView (ViewProjection _ context _)=context
segments SideBySideView (ViewProjection _ _ aligned)=aligned
segments _ _=V.empty

viewRowCount :: BufferView -> ViewProjection -> Int
viewRowCount mode p@(ViewProjection total _ _)
  | mode==CurrentView || mode==ChangesView=total
  | otherwise=case V.unsnoc (segments mode p) of
      Nothing -> 0
      Just (_,Segment start piece) -> start+pieceRows piece

-- Binary search over a compact index of changed runs/context intervals.
findSegment :: (Segment -> Int) -> Int -> V.Vector Segment -> Maybe Segment
findSegment key target rows
  | V.null rows=Nothing
  | otherwise=Just (rows V.! search 0 (V.length rows))
  where
    search lo hi
      | lo+1>=hi=lo
      | key (rows V.! mid)<=target=search mid hi
      | otherwise=search lo mid
      where mid=(lo+hi) `div` 2

viewRowAt :: BufferView -> ViewProjection -> Int -> ViewRow
viewRowAt mode p row
  | row<0 || row>=viewRowCount mode p=ViewRow Nothing Nothing 0
  | mode==CurrentView || mode==ChangesView=ViewRow (Just row) (Just row) 0
  | otherwise=case findSegment (\(Segment start _)->start) row (segments mode p) of
      Nothing -> ViewRow Nothing Nothing 0
      Just (Segment start piece) -> let offset=row-start in case piece of
        Lines source _ -> ViewRow (Just (source+offset)) (Just (source+offset)) 0
        Gap _ count -> ViewRow Nothing Nothing count
        Hunk source removed added -> ViewRow
          (if offset<removed then Just (source+offset) else Nothing)
          (if offset<added then Just (source+removed+offset) else Nothing) 0

-- | Map a full-change row to its displayed row. Hidden context maps to its gap;
-- callers keep the caret on visible live rows when navigating a filtered view.
viewRowForChange :: BufferView -> ViewProjection -> ReviewSide -> Int -> Int
viewRowForChange mode p _ row
  | mode==CurrentView || mode==ChangesView=max 0 (min (viewRowCount mode p-1) row)
  | otherwise=case findSegment (\(Segment _ piece)->sourceStart piece) row (segments mode p) of
      Nothing -> 0
      Just (Segment start piece) -> start+case piece of
        Lines source count -> max 0 (min (count-1) (row-source))
        Gap _ _ -> 0
        Hunk source removed added -> max 0 (min (max removed added-1)
          (if row<source+removed then row-source else row-source-removed))

-- | Visible full-change row ranges for copy/cut. Gap text and hidden source are
-- never part of a selection. This traversal runs on selection actions, not paint.
viewChangeRanges :: BufferView -> ViewProjection -> ReviewSide -> [(Int,Int)]
viewChangeRanges mode p@(ViewProjection total _ _) side
  | mode==CurrentView || mode==ChangesView=[(0,total)]
  | otherwise=concatMap ranges (V.toList (segments mode p))
  where
    ranges (Segment _ (Gap _ _))=[]
    ranges (Segment _ (Lines start count))=[(start,start+count)]
    ranges (Segment _ (Hunk start removed added))=case side of
      OriginalSide -> [(start,start+removed) | removed>0]
      _ -> [(start+removed,start+removed+added) | added>0]

bufferViewName :: BufferView -> String
bufferViewName CurrentView="current"
bufferViewName ChangesView="changes"
bufferViewName OnlyChangesView="only-changes"
bufferViewName SideBySideView="side-by-side"

parseBufferView :: String -> Maybe BufferView
parseBufferView value=lookup value [(bufferViewName mode,mode) | mode<-[minBound..maxBound]]
