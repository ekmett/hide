-- | Worker-prepared visual rows with original semantic character ranges.
-- Widening never inserts padding or wrapping newlines into the source. Render,
-- hit testing and navigation consume one measured snapshot. Layout equality
-- observes its fresh immutable identity, never text or glyph vectors.
module Hide.TextLayout
  ( TextLayout, LayoutRow(..), LayoutGlyph(..), prepareTextLayout
  , layoutRows, layoutWidth, layoutPosition, layoutOffset ) where

import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Unique (Unique,newUnique,hashUnique)
import Hide.Buffer (BufferContent,contentLineOffset)
import Hide.Syntax (Style(..),sectionTitle)
import Hide.Unicode (graphemes,clusterWidth)

data LayoutGlyph = LayoutGlyph
  { layoutText :: !Text, layoutDisplayText :: !Text, layoutStart :: !Int, layoutEnd :: !Int
  , layoutColumn :: !Int, layoutAdvance :: !Int, layoutStyle :: !Style
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
-- Ordinary rows retain their original line geometry. A one-cell viewport still
-- consumes a whole two-cell title grapheme, which clipping safely blanks.
prepareTextLayout :: Bool -> Int -> BufferContent -> V.Vector [(Char,Style)] -> IO TextLayout
prepareTextLayout wide requested text styled=do
  identity<-newUnique
  let rows=V.fromList (concat [line number chars | (number,chars)<-zip [0..] (V.toList styled)])
      width=V.foldl' (\n row->max n (layoutRowWidth row)) 0 rows
      forced=V.foldl' (\total row->total+layoutRowStart row+layoutRowEnd row+layoutRowWidth row+
        V.foldl' (\n glyph->n+T.length (layoutText glyph)+layoutStart glyph+layoutEnd glyph+layoutColumn glyph+layoutAdvance glyph) 0 (layoutGlyphs row)) 0 rows
  _<-evaluate forced
  pure (TextLayout identity rows width)
  where
    columns=max 1 requested
    line number chars=wrap [] 0 (contentLineOffset text number) (glyphs (contentLineOffset text number) (graphemes (T.pack (map fst chars))) chars)
    glyphs _ [] _=[]
    glyphs offset (g:rest) chars=
      let style=case chars of (_,value):_->value; _->Plain
          next=offset+T.length g
      in (g,offset,next,style):glyphs next rest (drop (T.length g) chars)
    wrap current col start []=[finish current col start]
    wrap current col start remaining@((g,a,z,style):rest)
      | not (null current) && wide && sectionTitle style && col+advance>columns = finish current col start:wrap [] 0 a remaining
      | otherwise=wrap (LayoutGlyph g drawn a z col advance style:current) (col+advance) start rest
      where
        natural=if g==T.singleton '\t' then 8-col `mod` 8 else clusterWidth g
        advance=if wide && sectionTitle style then 2 else natural
        drawn=if g==T.singleton '\t' then T.replicate (if wide && sectionTitle style then 1 else natural) (T.singleton ' ') else g
    finish current col start=LayoutRow start (case current of glyph:_->layoutEnd glyph; _->start) col (V.fromList (reverse current))

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
           | otherwise=layoutColumn (glyphs V.! lastBefore (V.length glyphs) (\i->layoutStart (glyphs V.! i)<=offset))

-- | Hit/navigation map back to original text. Right padding maps to row end;
-- both halves of a two-cell grapheme map to its starting character offset.
layoutOffset :: TextLayout -> Int -> Int -> Int
layoutOffset layout requested column
  | V.null rows=0
  | V.null glyphs || column>=layoutRowWidth row=layoutRowEnd row
  | otherwise=layoutStart (glyphs V.! lastBefore (V.length glyphs) (\i->layoutColumn (glyphs V.! i)<=column))
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
