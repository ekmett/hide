-- | Worker-prepared visual rows with original semantic character ranges.
-- Widening never inserts padding or wrapping newlines into the source. Render,
-- hit testing and navigation consume one measured snapshot. Layout equality
-- observes its fresh immutable identity, never text or glyph vectors.
module Hide.TextLayout
  ( TextLayout, LayoutRow(..), LayoutGlyph(..), prepareTextLayout, prepareMappedTextLayout
  , layoutRows, layoutWidth, layoutPosition, layoutOffset, layoutVisibleGlyphs ) where

import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Unique (Unique,newUnique,hashUnique)
import Hide.Buffer (BufferContent,contentLineOffset)
import Hide.Syntax (Style(..),StyledRow(..),MappedStyledRow(..),Sigils(..),fontTraits,sectionTitle,styleScript,sigilsLength,graphemeText,graphemeDisplayText,graphemeWidth,graphemeOverflow)
import Hide.Unicode (Script)

data LayoutGlyph = LayoutGlyph
  { layoutText :: !Text, layoutDisplayText :: !Text, layoutStart :: !Int, layoutEnd :: !Int
  , layoutColumn :: !Int, layoutAdvance :: !Int, layoutStyle :: !Style
  , layoutNatural :: !Int, layoutScript :: !(Maybe Script), layoutRunStep :: !Int
  } deriving Show

data LayoutRow = LayoutRow
  { layoutRowStart :: !Int, layoutRowEnd :: !Int, layoutRowWidth :: !Int
  , layoutGlyphs :: !(V.Vector LayoutGlyph) } deriving Show

data TextLayout = TextLayout !Unique !(V.Vector LayoutRow) !Int
instance Eq TextLayout where TextLayout a _ _==TextLayout b _ _=a==b
instance Show TextLayout where show (TextLayout identity _ _)="TextLayout "++show (hashUnique identity)
layoutRows :: TextLayout -> V.Vector LayoutRow
layoutRows (TextLayout _ rows _)=rows
layoutWidth :: TextLayout -> Int
layoutWidth (TextLayout _ _ width)=width

-- | Prepare complete heading wrapping and cell/source maps outside input. Only
-- semantic section titles widen; already-wide graphemes remain two cells.
-- Widened titles use stretch instead of bold, retaining italic and annotations.
-- Ordinary rows retain their original line geometry. A one-cell viewport still
-- consumes a whole two-cell title grapheme, which clipping safely blanks.
prepareTextLayout :: Bool -> Int -> BufferContent -> V.Vector StyledRow -> IO TextLayout
prepareTextLayout wide requested text styled=prepareMappedTextLayout wide requested (V.imap row styled)
  where
    row number styledRow@(StyledRow sigils _ _)=
      let start=contentLineOffset text number; end=start+sigilsLength sigils
      in MappedStyledRow styledRow start end (V.singleton (0,end-start,start,end))

-- | Prepare only supplied rows, resolving their compact logical mappings once.
-- No width-created furniture acquires a source extent. Each complete glyph maps
-- to the union of its intersecting logical spans, preserving whole-glyph masks.
-- Coordinates may be local to one logical block; the owning viewport retains
-- that block identity alongside these ordinary layout receipts.
prepareMappedTextLayout :: Bool -> Int -> V.Vector MappedStyledRow -> IO TextLayout
prepareMappedTextLayout wide requested styled=do
  identity<-newUnique
  let rows=V.fromList (concatMap line (V.toList styled))
      width=V.foldl' (\n row->max n (layoutRowWidth row)) 0 rows
      forced=V.foldl' (\total row->total+layoutRowStart row+layoutRowEnd row+layoutRowWidth row+
        V.foldl' (\n glyph->n+T.length (layoutText glyph)+layoutStart glyph+layoutEnd glyph+layoutColumn glyph+layoutAdvance glyph) 0 (layoutGlyphs row)) 0 rows
  _<-evaluate forced
  pure (TextLayout identity rows width)
  where
    columns=max 1 requested
    line mapped=case mappedStyledRow mapped of
      StyledRow sigils _ _->finishEnd (mappedRowEnd mapped) (wrap [] 0 (mappedRowStart mapped)
        (glyphs 0 (mappedRowStart mapped) (V.toList (mappedSourceRanges mapped)) sigils))
    finishEnd _ []=[]
    finishEnd end [row]=[row {layoutRowEnd=max end (layoutRowEnd row)}]
    finishEnd end (row:rest)=row:finishEnd end rest
    -- A monotone map cursor visits each source span once. Ordinary text splits
    -- only at map/style/wrap edges; no character tuple/vector is retained.
    glyphs _ _ _ Nil=[]
    glyphs offset boundary spans (ConsChars text style rest)=ordinary offset text boundary spans
      where
        ordinary position remaining edge pending
          | T.null remaining=glyphs position edge pending rest
          | otherwise=case advanceMap position edge pending of
              (nearest,current@((lo,hi,a,z):more))
                | lo<=position->
                  let (part,after)=T.splitAt (hi-position) remaining
                      count=T.length part
                      first=a+(position-lo)*(z-a) `div` (hi-lo)
                      lastOffset=a+((position+count-lo)*(z-a)+hi-lo-1) `div` (hi-lo)
                  in (part,part,False,count,first,lastOffset,style,True):
                    ordinary (position+count) after nearest current
                | otherwise->gap nearest lo current
              (nearest,[])->gap nearest (position+T.length remaining) []
          where
            gap nearest limit current=
              let (part,after)=T.splitAt (limit-position) remaining; count=T.length part
              in (part,part,False,count,nearest,nearest,style,True):ordinary (position+count) after nearest current
    glyphs offset boundary spans (ConsSigil item style _ rest)=
      let g=graphemeText item; end=offset+T.length g
          (nearest,current)=advanceMap offset boundary spans
          covered=[(a+(max offset lo-lo)*(z-a) `div` (hi-lo),
            a+((min end hi-lo)*(z-a)+hi-lo-1) `div` (hi-lo))
            | (lo,hi,a,z)<-takeWhile (\(lo,_,_,_)->lo<end) current,hi>lo,offset<hi]
          (a,z)=case covered of []->(nearest,nearest); _->(minimum (map fst covered),maximum (map snd covered))
      in (g,graphemeDisplayText item,graphemeOverflow item,graphemeWidth item,a,z,style,False):
        glyphs end nearest current rest
    advanceMap offset _ ((lo,hi,_,z):rest) | hi<=offset=advanceMap offset z rest
    advanceMap _ boundary spans=(boundary,spans)
    wrap current col start []=[finish current col start]
    wrap current col start remaining@((g,displayed,overflow,naturalWidth,a,z,style,isRun):rest)
      | not (null current) && widened && col+firstAdvance>columns=finish current col start:wrap [] 0 a remaining
      | isRun=
          let n=naturalWidth
              count=if widened then max 1 ((columns-col) `div` 2) else n
              used=min n count
              (part,after)=T.splitAt used g
              end=a+(used*(z-a)+n-1) `div` n
              advance=used*step
              run=LayoutGlyph part part a end col advance shownStyle (if script/=Nothing then 1 else used) script step
              next=if T.null after then rest else (after,after,False,n-used,a+used*(z-a) `div` n,z,style,True):rest
          in wrap (run:current) (col+advance) start next
      | otherwise=wrap (LayoutGlyph g drawn a z col advance shownStyle natural script 0:current) (col+advance) start rest
      where
        widened=wide && sectionTitle style && requestedScript==Nothing
        step=if widened then 2 else 1
        firstAdvance=if isRun then step else advance
        shownStyle
          | widened=let (base,_,italic)=fontTraits style in if italic then ItalicStyle base else base
          | otherwise=style
        natural | g=="\r"=0
                | g=="\t"=if widened then 1 else 8-col `mod` 8
                | T.any (\c->c<' ' || c=='\DEL') g=1
                | otherwise=naturalWidth
        requestedScript=styleScript style
        script=case requestedScript of
          Just mode | (isRun || natural `elem` [1,2]),not (T.any (\c->c<' ' || c=='\DEL') g)->Just mode
          _->Nothing
        advance | overflow=1
                | Just _<-script=1
                | requestedScript/=Nothing && natural==0=0
                | widened && not (T.null drawn)=2
                | otherwise=natural
        drawn | g=="\r"=T.empty
              | g=="\t"=T.replicate natural " "
              | overflow=displayed
              | T.any (\c->c<' ' || c=='\DEL') g=T.map (\c->if c<' ' || c=='\DEL' then '·' else c) g
              | otherwise=displayed
    finish current col start=LayoutRow start (case current of glyph:_->layoutEnd glyph; _->start) col (V.fromList (reverse current))

-- | Borrow only glyphs overlapping a horizontal cell window. Cached columns
-- locate both ends logarithmically; clipping never slices a semantic grapheme.
-- A zero-width window inspects no glyphs. The vector view shares prepared data.
layoutVisibleGlyphs :: Int -> Int -> LayoutRow -> V.Vector LayoutGlyph
layoutVisibleGlyphs requested width row
  | width<=0 || V.null glyphs=V.empty
  | otherwise=V.map clip (V.slice start (max 0 (end-start)) glyphs)
  where
    glyphs=layoutGlyphs row
    count=V.length glyphs
    left=max 0 requested
    right=left+min (maxBound-left) width
    candidate=lastBefore count (\i->layoutColumn (glyphs V.! i)<=left)
    first=glyphs V.! candidate
    start=if layoutColumn first+layoutAdvance first<=left then candidate+1 else candidate
    end=lastBefore count (\i->layoutColumn (glyphs V.! i)<right)+1

    clip glyph
      | layoutRunStep glyph<=0=glyph
      | otherwise=
          let step=layoutRunStep glyph; n=layoutAdvance glyph `div` step
              first=max 0 (min n ((left-layoutColumn glyph) `div` step))
              lastOffset=max first (min n ((right-layoutColumn glyph+step-1) `div` step))
              text=T.take (lastOffset-first) (T.drop first (layoutText glyph))
              source=layoutEnd glyph-layoutStart glyph
          in glyph {layoutText=text,layoutDisplayText=text,
            layoutStart=layoutStart glyph+first*source `div` n,
            layoutEnd=layoutStart glyph+(lastOffset*source+n-1) `div` n,
            layoutColumn=layoutColumn glyph+first*step,layoutAdvance=(lastOffset-first)*step}

-- | Locate an original character offset in prepared visual rows. Both cells of
-- a wide glyph and its combining characters share the same semantic position.
layoutPosition :: TextLayout -> Int -> (Int,Int)
layoutPosition layout offset
  | V.null rows=(0,0)
  | otherwise=(rowNumber,column)
  where
    rows=layoutRows layout
    rowNumber=lastBefore (V.length rows) (\i->layoutRowStart (rows V.! i)<=offset)
    row=rows V.! rowNumber
    glyphs=layoutGlyphs row
    column | V.null glyphs=0
           | offset>=layoutRowEnd row=layoutRowWidth row
           | otherwise=let glyph=glyphs V.! lastBefore (V.length glyphs) (\i->layoutStart (glyphs V.! i)<=offset)
                       in layoutColumn glyph+if layoutRunStep glyph<=0 || layoutEnd glyph<=layoutStart glyph then 0
                         else (offset-layoutStart glyph)*(layoutAdvance glyph `div` layoutRunStep glyph) `div` (layoutEnd glyph-layoutStart glyph)*layoutRunStep glyph

-- | Hit/navigation map back to original text. Right padding maps to row end;
-- both halves of a two-cell grapheme map to its starting character offset.
layoutOffset :: TextLayout -> Int -> Int -> Int
layoutOffset layout requested column
  | V.null rows=0
  | V.null glyphs || column>=layoutRowWidth row=layoutRowEnd row
  | otherwise=let glyph=glyphs V.! lastBefore (V.length glyphs) (\i->layoutColumn (glyphs V.! i)<=column)
              in layoutStart glyph+if layoutRunStep glyph<=0 then 0 else
                ((column-layoutColumn glyph) `div` layoutRunStep glyph)*(layoutEnd glyph-layoutStart glyph) `div` (layoutAdvance glyph `div` layoutRunStep glyph)
  where
    rows=layoutRows layout
    row=rows V.! max 0 (min (V.length rows-1) requested)
    glyphs=layoutGlyphs row

lastBefore :: Int -> (Int -> Bool) -> Int
lastBefore count predicate=search 0 (max 0 (count-1))
  where
    search low high | low>=high=low
                    | predicate middle=search middle high
                    | otherwise=search low (middle-1)
      where middle=(low+high+1) `div` 2
