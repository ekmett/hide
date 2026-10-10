-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings, BangPatterns, UnboxedTuples #-}
-- |
-- Module      : Hide.Syntax
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings, BangPatterns, UnboxedTuples
--
-- Source token styles and presentation annotations shared by renderers.
--
-- Skylighting supplies language grammars; this module maps token classes to editor
-- styles. A tokenizer result is accepted only when it preserves the original
-- characters exactly. Link and bubble annotations remain in the styled stream
-- so later layout can retain interaction metadata without reparsing text.
module Hide.Syntax (Style(..), StyledText, styledText, styledContents, compactStyled, styledLength, splitStyledAt, splitStyledText, StyledRow(..), MappedStyledRow(..), styledRows, sigilsText, sigilsLength, sigilsColumn, styledColumn, styleRunStep, styleGlyphAdvance, sigilsStyles, mapSigilsStyle, Grapheme, graphemeText, graphemeDisplayText, graphemeWidth, graphemeOverflow, Sigils(..), sourceSigilsWindow, SourceRow, SourceRange, prepareSourceRow, rebaseSourceRows, plainSourceRow, plainSourceLine, attachSourceLine, sourceRowText, sourceRowRanges, sourceRangeCharStart, sourceRangeCharEnd, sourceRangeByteStart, sourceRangeByteEnd, sourceRangeStyle, sourceRangeText, sourceStylesAt, presentationItems, styleOverflowExtent, styleLayoutMetadata, styleScript, fontTraits, sectionTitle, highlight, highlightFor, bubbleTile, linkSpans) where

import Data.List (intercalate)
import qualified Data.List as List
import Data.Char (chr)
import Data.Word (Word32)
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import Hide.Unicode (DisplayItem,itemScalarCount,itemOverflow,itemSourceText,itemDisplayText,itemWidth,sourceGraphemesFrom,sourceItemAdvance,sourceGlyphAdvance,displayItems,scalarWidth,initialSourceCursor,sourceItemStep,sourceSpanStep,sourceItemsFromCursor,Script)
import Hide.LineChunks (joinAdjacent)
import Hide.Buffer (Buffer,lastChange,bufferLineColumn,bufferContent,contentSourceLineAt,sourceLineSlice,SourceLine,sourceLineText,sourceLineRawText,sourceLineLength,sourceLineRawLength,sourceLineHasChunks,sourceLineWindow)
import qualified Skylighting as S
import System.FilePath (takeFileName)

-- | Token color intent plus nested prose, link, bubble or terminal annotations.
data Style = OverflowFragment !Int Style | ScriptStyle !Script Style | SectionStyle Int Style | BoldStyle Style | ItalicStyle Style | LinkStyle T.Text Style | Plain | Heading Int | CodeStyle Bool Style | ProseStyle Style | Keyword | Comment | Literal | Number | Constructor | Pragma | BubbleStyle Bool Style | BubbleText Int Bool Style | TerminalStyle Word32 Word32 Word32 deriving (Eq,Show)

-- | Borrowed lexical runs, before grapheme segmentation. Concatenation and
-- decoration preserve every scalar; a style boundary is not a glyph boundary.
-- The parser uses Seq internally for append, then passes these runs to its row
-- worker. Runs are never expanded into character/style tuples.
type StyledText = [(T.Text,Style)]

-- | /O(1)/. Borrow one lexical run; empty input contributes no source extent.
styledText :: Style -> T.Text -> StyledText
styledText style text=[(text,style) | not (T.null text)]

-- | Project exact lexical text. A single run retains its existing Text array.
-- This projection belongs to parsing/preparation, never an interaction owner.
styledContents :: StyledText -> T.Text
styledContents=T.concat . map fst

-- | Coalesce adjacent equal-style slices only when their immutable arrays
-- prove they are contiguous. This borrows bytes without concatenation. Captured
-- overflow starts remain separate even when two fragments share a style.
compactStyled :: StyledText -> StyledText
compactStyled []=[]
compactStyled ((text,style):rest)
  | T.null text=compactStyled rest
  | otherwise=gather text rest
  where
    gather current ((next,other):more)
      | T.null next=gather current more
      | style==other,styleOverflowExtent style==Nothing,Just joined<-joinAdjacent current next=gather joined more
    gather current remaining=(current,style):compactStyled remaining

-- | Scalar extent of lexical runs, independent of display-cell width.
styledLength :: StyledText -> Int
styledLength=List.foldl' (\n (text,_)->n+T.length text) 0

-- | Split exact scalar coordinates by borrowing Text slices. No glyph decision
-- is made here; wrapping calls this only at its captured complete item edges.
splitStyledAt :: Int -> StyledText -> (StyledText,StyledText)
splitStyledAt requested=go (max 0 requested) []
  where
    go _ done []=(reverse done,[])
    go n done remaining | n<=0=(reverse done,remaining)
    go n done ((text,style):rest)
      | T.null after=go (n-T.length before) ((before,style):done) rest
      | otherwise=(reverse ((before,style):done),(after,style):rest)
      where (before,after)=T.splitAt n text

-- | Split LF on borrowed run boundaries. CR remains source text. The optional
-- style names the removed newline, including its passive message identity.
-- Concatenating rows and their breaks recovers the original lexical text.
splitStyledText :: StyledText -> [(StyledText,Maybe Style)]
splitStyledText=go []
  where
    go current []=[(reverse current,Nothing)]
    go current ((text,style):rest)
      | T.null text=go current rest
      | T.null after=go ((text,style):current) rest
      | otherwise=(reverse ([(before,style) | not (T.null before)]++current),Just style):
          go [] ((T.drop 1 after,style):rest)
      where (before,after)=T.breakOn "\n" text

-- | One finalized physical row. Strict Sigils remain bounded by the demanded
-- row; the enclosing row list stays lazy. Newline metadata is not painted.
-- Passive message ranges preserve original scalar attribution independently of
-- the first-scalar style used to paint a cross-run grapheme.
data StyledRow = StyledRow !Sigils !(Maybe Style) !(V.Vector (Int,Int,Int,Bool))

-- | One demanded row's paint-to-logical source map. Furniture has no range;
-- its hit boundary comes from the nearest logical edge. Expanded tabs can map
-- several painted scalars to one logical scalar. Layout consumes these ranges
-- into its glyph receipts rather than retaining another mapping owner.
data MappedStyledRow = MappedStyledRow
  { mappedStyledRow :: !StyledRow, mappedRowStart :: !Int, mappedRowEnd :: !Int
  , mappedSourceRanges :: !(V.Vector (Int,Int,Int,Int))
  }

-- | Segment before assigning styles, preserving cross-run graphemes and exact
-- overflow markers. Only the first scalar's style paints a complete item.
styledRows :: StyledText -> [StyledRow]
styledRows=map row . splitStyledText
  where
    row (runs,newline)=StyledRow (styledSigils (compactStyled runs)) newline
      (V.fromList (concat (snd (List.mapAccumL passive 0 runs))))
    passive offset (text,style)=let end=offset+T.length text in
      (end,case style of BubbleText ident outgoing _->[(offset,end,ident,outgoing)]; _->[])


-- | Original bytes and scalar count of a finalized row, without display text.
sigilsText :: Sigils -> T.Text
sigilsText=T.concat . pieces
  where
    pieces Nil=[]
    pieces (ConsChars text _ rest)=text:pieces rest
    pieces (ConsSigil glyph _ _ rest)=graphemeText glyph:pieces rest
sigilsLength :: Sigils -> Int
sigilsLength=go 0
  where
    go !n Nil=n
    go !n (ConsChars text _ rest)=go (n+T.length text) rest
    go !n (ConsSigil glyph _ _ rest)=go (n+T.length (graphemeText glyph)) rest

-- | Displayed column shared by wrapping, bubble furniture and layout. Ordinary
-- one-cell scalar runs widen only for unscripted section titles; exceptional
-- items remain whole, including overflow and private combining continuations.
sigilsColumn :: Bool -> Int -> Sigils -> Int
sigilsColumn wide=go
  where
    go !col Nil=col
    go !col (ConsChars text style rest)=go (col+T.length text*styleRunStep wide style) rest
    go !col (ConsSigil glyph style _ rest)=go
      (col+styleGlyphAdvance wide col style (graphemeText glyph) (graphemeOverflow glyph) (graphemeWidth glyph)) rest

-- | Resolve borrowed lexical text through the same grapheme-first row policy.
-- No width or style boundary can split a complete display item.
styledColumn :: Bool -> Int -> StyledText -> Int
styledColumn wide column runs=go 0 column (presentationItems text runs) (positions 0 runs)
  where
    text=styledContents runs
    positions _ []=[]
    positions offset ((run,style):rest)=let end=offset+T.length run in (end,style):positions end rest
    go !_ !col [] _=col
    go !offset !col ((glyph,overflow):rest) pending=
      let current=dropStyles offset pending
          style=case current of (_,value):_->value; _->Plain
          advance=styleGlyphAdvance wide col style glyph overflow (sourceGlyphAdvance 0 glyph)
      in go (offset+T.length glyph) (col+advance) rest current
    dropStyles offset ((end,_):rest) | end<=offset=dropStyles offset rest
    dropStyles _ pending=pending

-- | Cell step of an ordinary one-cell scalar run. Scripted text owns one cell
-- per scalar; a heading's natural two-cell items are handled separately.
styleRunStep :: Bool -> Style -> Int
styleRunStep wide style=if wide && sectionTitle style && styleScript style==Nothing then 2 else 1

-- | Exact exceptional-item advance at the incoming displayed column. This is
-- the common numeric policy used before and during physical row preparation.
styleGlyphAdvance :: Bool -> Int -> Style -> T.Text -> Bool -> Int -> Int
styleGlyphAdvance wide col style text overflow naturalWidth
  | overflow=1
  | scripted=1
  | requestedScript/=Nothing && natural==0=0
  | widened && text/="\r" && not (T.null text)=2
  | otherwise=natural
  where
    requestedScript=styleScript style
    widened=styleRunStep wide style==2
    control=T.any (\c->c<' ' || c=='\DEL') text
    natural | text=="\r"=0
            | text=="\t"=if widened then 1 else 8-col `mod` 8
            | control=1
            | otherwise=naturalWidth
    scripted=requestedScript/=Nothing && natural `elem` [1,2] && not control


-- | Run-level metadata fold. Ordinary one-cell text is never expanded.
sigilsStyles :: Sigils -> [Style]
sigilsStyles Nil=[]
sigilsStyles (ConsChars _ style rest)=style:sigilsStyles rest
sigilsStyles (ConsSigil _ style _ rest)=style:sigilsStyles rest

-- | Decorate a finalized row without changing source extents or geometry.
mapSigilsStyle :: (Style -> Style) -> Sigils -> Sigils
mapSigilsStyle _ Nil=Nil
mapSigilsStyle f (ConsChars text style rest)=ConsChars text (f style) (mapSigilsStyle f rest)
mapSigilsStyle f (ConsSigil glyph style advance rest)=ConsSigil glyph (f style) advance (mapSigilsStyle f rest)

-- | Original source row and worker-prepared style ranges. Styling never changes
-- character positions; range boundaries also name complete UTF8 codepoints.
-- Concatenating 'sourceRangeText' over 'sourceRowRanges' recovers 'sourceRowText'.
data SourceRow = SourceRow !T.Text !(V.Vector SourceRange) | PlainSourceRow !T.Text | LiveSourceRow !SourceLine !(Maybe (V.Vector SourceRange)) deriving Show

-- Equality is the public text/range projection, independent of representation.
-- It is not an identity or an invalidation key.
instance Eq SourceRow where
  a==b=sourceRowText a==sourceRowText b && sourceRowRanges a==sourceRowRanges b

-- | Original source text, borrowed by either representation.
sourceRowText :: SourceRow -> T.Text
sourceRowText (SourceRow text _)=text
sourceRowText (PlainSourceRow text)=text
sourceRowText (LiveSourceRow line Nothing)=sourceLineText line
sourceRowText (LiveSourceRow line (Just _))=sourceLineRawText line

-- | Exact ordered ranges covering the source. Requesting ranges for an implicit
-- plain row counts its characters; visible rendering does not need that count.
sourceRowRanges :: SourceRow -> V.Vector SourceRange
sourceRowRanges (SourceRow _ ranges)=ranges
sourceRowRanges (PlainSourceRow text)
  | T.null text=V.empty
  | otherwise=V.singleton (SourceRange 0 (T.length text) 0 (TU.lengthWord8 text) Plain)

sourceRowRanges (LiveSourceRow _ (Just ranges))=ranges
sourceRowRanges row@(LiveSourceRow line Nothing)
  | sourceLineLength line==0=V.empty
  | otherwise=V.singleton (SourceRange 0 (sourceLineLength line) 0 (TU.lengthWord8 (sourceRowText row)) Plain)

-- | Half-open character and UTF8 byte coordinates in one original source row.
-- The constructor stays private: ranges are ordered and cover the exact row.
data SourceRange = SourceRange
  { sourceRangeCharStart :: {-# UNPACK #-} !Int
  , sourceRangeCharEnd :: {-# UNPACK #-} !Int
  , sourceRangeByteStart :: Int
  , sourceRangeByteEnd :: Int
  , sourceRangeStyle :: !Style
  } deriving (Eq,Show)

-- | Keep existing colors aligned through an edit, including line joins/splits.
-- New text borrows its left-hand style until the worker supplies lexical colors.
-- Byte coordinates are lazy: painting uses character ranges and the live line's
-- measured seek, so an edit does not flatten an otherwise untouched long line.
rebaseSourceRows :: Buffer -> Buffer -> Seq.Seq SourceRow -> Seq.Seq SourceRow
rebaseSourceRows before after rows=case lastChange after of
  Nothing->rows
  Just (start,end,inserted)->
    let (first,a)=bufferLineColumn before start
        (lastOld,z)=bufferLineColumn before end
        (lastNew,b)=bufferLineColumn after (start+inserted)
        prefix=pieces first 0 a 0
        suffix=pieces lastOld z maxBound (b-z)
        inherited=case reverse prefix of (_,_,style):_->style; _->Plain
        rebuild n=
          let line=contentSourceLineAt (bufferContent after) n
              len=sourceLineLength line
              lo=if n==first then a else 0
              hi=if n==lastNew then b else sourceLineRawLength line
              paints=(if n==first then prefix else [])++[(lo,hi,inherited) | hi>lo]++
                (if n==lastNew then suffix else [])
              ranges _ []=[]
              ranges byte ((x,y,style):rest)=
                let next=byte+TU.lengthWord8 (sourceLineSlice line x (y-x))+max 0 (y-max x len)
                in SourceRange x y byte next style:ranges next rest
          in LiveSourceRow line (Just (V.fromList (ranges 0 (foldr join [] paints))))
        changed=Seq.fromFunction (lastNew-first+1) (rebuild . (+first))
    in Seq.take first rows Seq.>< changed Seq.>< Seq.drop (lastOld+1) rows
  where
    join (x,y,style) ((a,z,other):rest) | y==a && style==other=(x,z,style):rest
    join part rest=part:rest
    pieces n lo hi shift=case Seq.lookup n rows of
      Nothing->[]
      Just row->[(max lo (sourceRangeCharStart r)+shift,min hi (sourceRangeCharEnd r)+shift,sourceRangeStyle r)
        | r<-V.toList (sourceRowRanges row),sourceRangeCharEnd r>lo,sourceRangeCharStart r<hi]

-- | Prepare compact ranges for one physical row (LF already split by the
-- owning worker; a trailing CR may remain). The source remains
-- authoritative; missing style positions are Plain, and no token character is
-- copied into the retained representation.
prepareSourceRow :: T.Text -> StyledText -> SourceRow
prepareSourceRow text tokens=SourceRow text (V.fromList (ranges 0 0 text tokens))
  where
    ranges !_ !_ remaining _ | T.null remaining=[]
    ranges !char !byte remaining []=
      [SourceRange char (char+T.length remaining) byte (byte+TU.lengthWord8 remaining) Plain]
    ranges !char !byte remaining ((token,style):rest)
      | T.null token=ranges char byte remaining rest
      | otherwise=let (part,after)=T.splitAt (T.length token) remaining
                      endChar=char+T.length part
                      endByte=byte+TU.lengthWord8 part
                  in addRange (SourceRange char endChar byte endByte style) (ranges endChar endByte after rest)
    -- Ranges describe the authoritative source, not tokenizer array ownership.
    -- Equal adjacent styles therefore coalesce even when lexical slices came
    -- from distinct arrays; no Text concatenation is required.
    addRange (SourceRange a _ b _ style) (SourceRange _ z _ end other:rest)
      | style==other=SourceRange a z b end style:rest
    addRange range rest=range:rest

-- | /O(1)/. Plain visible rows borrow their original Text without counting or
-- rebuilding characters. Exact ranges remain available through 'sourceRowRanges'.
plainSourceRow :: T.Text -> SourceRow
plainSourceRow=PlainSourceRow

-- | Borrow the current physical source owner without projecting its full Text.
plainSourceLine :: SourceLine -> SourceRow
plainSourceLine line
  | sourceLineHasChunks line=LiveSourceRow line Nothing
  | otherwise=plainSourceRow (sourceLineText line)

-- | Attach live storage to prepared ranges from this exact source revision.
-- Public projections remain the original worker row, including a trailing CR.
attachSourceLine :: SourceLine -> SourceRow -> SourceRow
attachSourceLine line row | not (sourceLineHasChunks line)=case row of
  SourceRow {}->row
  PlainSourceRow {}->plainSourceLine line
  LiveSourceRow _ Nothing->plainSourceLine line
  LiveSourceRow _ (Just ranges)->SourceRow (sourceLineRawText line) ranges
attachSourceLine line (SourceRow _ ranges)=LiveSourceRow line (Just ranges)
attachSourceLine line (LiveSourceRow _ ranges)=LiveSourceRow line ranges
attachSourceLine line (PlainSourceRow _)=plainSourceLine line

-- | Slice a range belonging to this row. Both endpoints were established from
-- the original UTF8 iterator on the preparation worker.
sourceRangeText :: SourceRow -> SourceRange -> T.Text
sourceRangeText row range=TU.takeWord8 (sourceRangeByteEnd range-sourceRangeByteStart range)
  (TU.dropWord8 (sourceRangeByteStart range) (sourceRowText row))

-- | Stream styles starting at a character position. Inline preview consumes
-- only its requested chunk; ordinary rendering consumes the ranges directly.
sourceStylesAt :: SourceRow -> Int -> [Style]
sourceStylesAt row offset=concat
  [replicate (sourceRangeCharEnd range-max offset (sourceRangeCharStart range)) (sourceRangeStyle range)
  | range<-V.toList (sourceRowRanges row),sourceRangeCharEnd range>offset]

-- | One bounded source display item, borrowed from the original row.
-- Normal graphemes stay complete; capped fragments keep separate display text.
data Grapheme = Grapheme !DisplayItem | OverflowGrapheme !T.Text deriving (Eq,Show)

graphemeText :: Grapheme -> T.Text
graphemeText (Grapheme item)=itemSourceText item
graphemeText (OverflowGrapheme text)=text
graphemeDisplayText :: Grapheme -> T.Text
graphemeDisplayText (Grapheme item)=itemDisplayText item
graphemeDisplayText OverflowGrapheme{}="�"

-- | Natural displayed cells and captured overflow, independent of source size.
graphemeWidth :: Grapheme -> Int
graphemeWidth (Grapheme item)=itemWidth item
graphemeWidth OverflowGrapheme{}=1
graphemeOverflow :: Grapheme -> Bool
graphemeOverflow (Grapheme item)=itemOverflow item
graphemeOverflow OverflowGrapheme{}=True

-- | Visible source display stream. Each character in ConsChars independently
-- occupies one cell; ConsSigil retains one complete exceptional grapheme. Both
-- borrow source bytes. Strict tails avoid a separate pair/list payload layer.
-- Window fragments retain their exact source bytes and original coordinates;
-- segmentation always precedes styling, regardless of range boundaries.
data Sigils
  = ConsChars {-# UNPACK #-} !T.Text {-# UNPACK #-} !Style !Sigils
  | ConsSigil {-# UNPACK #-} !Grapheme {-# UNPACK #-} !Style {-# UNPACK #-} !Int !Sigils
  | Nil

-- | Prepare bounded display items overlapping a display-column window.
-- Returns the original character and display-column start of the first fragment.
-- Translation/cropping may hide a half glyph; its source fragment stays complete.
-- Tab advances use absolute columns. Styling is assigned after segmentation.
-- A zero-width window never inspects source metadata or text.
sourceSigilsWindow :: Int -> Int -> SourceRow -> (Int,Int,Sigils)
sourceSigilsWindow requested width row
  | width<=0=(0,0,Nil)
  | otherwise=let (char,col,groups)=window
              in (char,col,nextGroup col char (rangeIndex char) groups)
  where
    left=max 0 requested
    right=left+min (maxBound-left) width
    limit=case row of LiveSourceRow line _->sourceLineLength line; _->maxBound
    window=case row of
      LiveSourceRow line prepared->
        let (char,col,groups)=sourceLineWindow line left
            -- Prepared worker rows retain trailing CR, while editor geometry
            -- omits it. Preserve the exact source EOF coordinate without paint.
            end=case prepared >>= (V.!? (maybe 0 V.length prepared-1)) of
              Just range | null groups->sourceRangeCharEnd range
              _->char
        in (end,col,groups)
      _->let text=sourceRowText row; (char,byte,col,items)=sourceGraphemesFrom left text
         in (char,col,[(TU.dropWord8 byte text,items)])
    ranges=case row of
      SourceRow _ prepared->prepared
      LiveSourceRow _ (Just prepared)->prepared
      _->V.empty
    -- Binary seek keeps a far-horizontal viewport independent of earlier styles.
    rangeIndex char=seek 0 (V.length ranges)
      where seek lo hi | lo>=hi=lo
                       | otherwise=let mid=(lo+hi) `div` 2
                                   in if sourceRangeCharEnd (ranges V.! mid)<=char then seek (mid+1) hi else seek lo mid
    activeIndex char index
      | Just range<-ranges V.!? index,sourceRangeCharEnd range<=char=activeIndex char (index+1)
      | otherwise=index
    nextGroup col char index groups
      | col>=right || char>=limit=Nil
      | otherwise=case groups of
          []->Nil
          (text,pending):more->build text col char 0 index pending more
    build text !col !char !byte index pending more
      | col>=right || char>=limit=Nil
      | otherwise=case pending of
          []->nextGroup col char index more
          glyph:rest->emit text col char byte index glyph (itemScalarCount glyph) (sourceItemAdvance col glyph) rest more
    emit text col char byte index glyph n advance rest more=
      let current=activeIndex char index
          range=ranges V.!? current
          style=maybe Plain sourceRangeStyle range
          endByte=byte+TU.lengthWord8 (itemSourceText glyph)
          endChar=char+n
          slice a b=TU.takeWord8 (b-a) (TU.dropWord8 a text)
      in if not (itemOverflow glyph) && n==1 && advance==1 && T.all (\c->c>=' ' && c/='\DEL') (itemSourceText glyph) then
           let rangeLimit=min limit (maybe maxBound sourceRangeCharEnd range)
               (finishChar,finishByte,after)=gather rangeLimit (col+1) endChar endByte rest
           in ConsChars (slice byte finishByte) style (build text (col+finishChar-char) finishChar finishByte current after more)
         else ConsSigil (Grapheme glyph) style advance (build text (col+advance) endChar endByte current rest more)
    -- Borrowed ordinary runs stop at both the style and the storage-leaf edge.
    gather rangeLimit !col !char !byte pending
      | char>=rangeLimit || col>=right=(char,byte,pending)
      | otherwise=case pending of
          glyph:rest | not (itemOverflow glyph),itemScalarCount glyph==1,itemWidth glyph==1,T.all (\c->c>=' ' && c/='\DEL') (itemSourceText glyph)->
            gather rangeLimit (col+1) (char+1) (byte+TU.lengthWord8 (itemSourceText glyph)) rest
          _->(char,byte,pending)

highlight :: T.Text -> StyledText
highlight = highlightFor "Main.hs"

-- | Choose a grammar by filename, tokenize, and preserve exact source positions.
-- Unknown grammars, tokenizer failure or normalized output fall back to plain text.
highlightFor :: FilePath -> T.Text -> StyledText
highlightFor path source = case S.syntaxesByFilename S.defaultSyntaxMap (takeFileName path) of
  syntax:_ -> case S.tokenize (S.TokenizerConfig S.defaultSyntaxMap False) syntax source of
    Right lines' ->
      let styled = intercalate [("\n",Plain)] (map (concatMap paint) lines')
                   ++ [("\n",Plain) | "\n" `T.isSuffixOf` source]
      -- Never change buffer positions if a tokenizer normalizes its input.
      in if styledContents styled == source then styled else plain
    Left _ -> plain
  [] -> plain
  where
    plain = styledText Plain source
    paint (token,text) = styledText (if markdown && token==S.FunctionTok then Heading (max 1 (min 6 (T.length (T.takeWhile (=='#') (T.stripStart text))))) else style token) text
    markdown = any ((=="Markdown") . S.sName) (S.syntaxesByFilename S.defaultSyntaxMap (takeFileName path))
    style token = case token of
      S.KeywordTok -> Keyword; S.ControlFlowTok -> Keyword; S.ImportTok -> Keyword
      S.CommentTok -> Comment; S.DocumentationTok -> Comment; S.AnnotationTok -> Comment; S.CommentVarTok -> Comment
      S.CharTok -> Literal; S.SpecialCharTok -> Literal; S.StringTok -> Literal
      S.VerbatimStringTok -> Literal; S.SpecialStringTok -> Literal
      S.DecValTok -> Number; S.BaseNTok -> Number; S.FloatTok -> Number
      S.DataTypeTok -> Constructor; S.ConstantTok -> Constructor
      S.PreprocessorTok -> Pragma; S.ExtensionTok -> Pragma
      _ -> Plain

-- | Choose a private graphical tile or terminal block-character fallback by tile index.
bubbleTile :: Bool -> Int -> Char
bubbleTile graphical n
  | n<0 || n>7 = ' '
  | graphical = chr (0xe000+n)
  | otherwise = "▟▙▜▛▐▌◥◤" !! n

-- | Collect half-open character-offset spans from nested link annotations.
-- Compute during layout, not on each paint.
linkSpans :: StyledText -> [(Int,Int,T.Text)]
linkSpans = reverse . snd . List.foldl' collect (0,[])
  where
    target (OverflowFragment _ s)=target s
    target (SectionStyle _ s)=target s
    target (BoldStyle s)=target s
    target (ItalicStyle s)=target s
    target (ScriptStyle _ s)=target s
    target (LinkStyle url _)=Just url
    target (ProseStyle s)=target s
    target (BubbleText _ _ s)=target s
    target (BubbleStyle _ s)=target s
    target _=Nothing
    collect (offset,found) (text,style)=(offset+T.length text,case target style of
      Nothing->found
      Just url->case found of
        (start,end,old):rest | end==offset && url==old -> (start,offset+T.length text,url):rest
        _->(offset,offset+T.length text,url):found)

-- | Captured scalar extent of a capped presentation fragment, at its first
-- character. Wrapping retains the marker and original characters together.
styleOverflowExtent :: Style -> Maybe Int
styleOverflowExtent (OverflowFragment n _)=Just n
styleOverflowExtent (ScriptStyle _ s)=styleOverflowExtent s
styleOverflowExtent (SectionStyle _ s)=styleOverflowExtent s
styleOverflowExtent (BoldStyle s)=styleOverflowExtent s
styleOverflowExtent (ItalicStyle s)=styleOverflowExtent s
styleOverflowExtent (LinkStyle _ s)=styleOverflowExtent s
styleOverflowExtent (CodeStyle _ s)=styleOverflowExtent s
styleOverflowExtent (ProseStyle s)=styleOverflowExtent s
styleOverflowExtent (BubbleStyle _ s)=styleOverflowExtent s
styleOverflowExtent (BubbleText _ _ s)=styleOverflowExtent s
styleOverflowExtent _=Nothing

-- | Cached admission for geometry metadata; inspecting styled payloads belongs
-- to preparation, never the render/input owner.
styleLayoutMetadata :: Style -> Bool
styleLayoutMetadata s=styleScript s/=Nothing || styleOverflowExtent s/=Nothing

-- | Borrow bounded presentation items, respecting captured overflow extents
-- after wrapping. Fresh Unicode segmentation cannot recover a fragment's GB11
-- context, so annotated fragments consume their exact original scalar range.
-- Ordinary items retain the shared stateful cursor; no source bytes are changed.
presentationItems :: T.Text -> StyledText -> [(T.Text,Bool)]
presentationItems text styled=go 0 text (map (\i->(itemSourceText i,itemOverflow i)) (displayItems text)) ranges
  where
    ranges=positions 0 styled
    positions _ []=[]
    positions !offset ((run,style):rest)
      | T.null run=positions offset rest
      | otherwise=let end=offset+T.length run in (offset,end,style):positions end rest
    go _ _ [] _=[]
    go offset remaining pending@((glyph,overflow):rest) styles=
      case styles of
        (start,_,style):_ | start==offset,Just n<-styleOverflowExtent style,n>0,n<=32,
                           let original=T.take n remaining,T.length original==n->
          (original,True):go (offset+n) (T.drop n remaining) (skip n pending) (dropRanges (offset+n) styles)
        _->case markerBefore (offset+T.length glyph) styles of
          Just start->let n=start-offset in (T.take n glyph,False):
            go start (T.drop n remaining) (skip n pending) (dropRanges start styles)
          Nothing->let n=T.length glyph; end=offset+n
                   in (glyph,overflow):go end (T.drop n remaining) rest (dropRanges end styles)
      where
        markerBefore limit ((start,_,style):more)
          | start>=limit=Nothing
          | start>offset && styleOverflowExtent style/=Nothing=Just start
          | otherwise=markerBefore limit more
        markerBefore _ []=Nothing
    dropRanges offset remaining@((_,end,_):rest)
      | end<=offset=dropRanges offset rest
      | otherwise=remaining
    dropRanges _ []=[]
    skip _ []=[]
    skip n pending@((glyph,overflow):rest)
      | n<=0=pending
      | n>=size=skip (n-size) rest
      | otherwise=(T.drop n glyph,overflow):rest
      where size=T.length glyph

styledSigils :: StyledText -> Sigils
styledSigils runs
  | any ((/=Nothing) . styleOverflowExtent . snd) runs=marked
  | otherwise=borrowed 0 initialSourceCursor runs
  where
    -- Numeric span receipts consume ordinary ASCII without per-scalar Text,
    -- DisplayItem or tuple objects. Leave its last scalar for real lookahead;
    -- a following run may begin with a combining/ZWJ continuation.
    borrowed _ _ []=Nil
    borrowed col cursor remaining@((text,style):rest)
      | T.null text=borrowed col cursor rest
      | count>1=case sourceSpanStep text 0 cursor maxBound (count-1) False of
          (# byte,chars,_,_,_,_,next #)->
            let original=TU.takeWord8 byte text
                following=TU.dropWord8 byte text
            in ordinaryRun original style (borrowed (col+chars) next ((following,style):rest))
      | otherwise=
          let bridge=styledContents (fst (splitStyledAt 33 remaining))
          in case sourceItemStep bridge 0 cursor of
            (# byte,chars,natural,tab,overflow,next #)->
              let original=TU.takeWord8 byte bridge
                  glyph=case sourceItemsFromCursor cursor overflow original of
                    item:_->Grapheme item
                    _->OverflowGrapheme original
                  advance=if tab then 8-col `mod` 8 else natural
                  following=snd (splitStyledAt chars remaining)
                  ordinary=not overflow && chars==1 && natural==1 &&
                    T.all (\c->c>=' ' && c/='\DEL') original
              in if ordinary then ordinaryRun original style (borrowed (col+advance) next following)
                else ConsSigil glyph style advance (borrowed (col+advance) next following)
      where count=T.length (T.takeWhile (\c->c>=' ' && c<='~') text)
    ordinaryRun text style (ConsChars next other rest)
      | style==other,Just joined<-joinAdjacent text next=ConsChars joined style rest
    ordinaryRun text style rest=ConsChars text style rest
    -- Captured overflow fragments own exact original extents, including a
    -- fragment beginning inside a freshly segmented padding/ZWJ item.
    marked=build 0 0 fullText (presentationItems fullText runs) ranges
    fullText=styledContents runs
    ranges=V.toList (sourceRowRanges (prepareSourceRow fullText runs))
    build !_ !_ _ [] _=Nil
    build !offset !col raw ((glyph,overflow):rest) styles=
      let current=dropRanges offset styles
          style=case current of range:_->sourceRangeStyle range; _->Plain
          count=T.length glyph
          ordinary=not overflow && count==1 && T.all (\c->c>=' ' && c/='\DEL') glyph &&
            T.all ((==1) . scalarWidth) glyph
      in if ordinary then
           let limit=case current of range:_->sourceRangeCharEnd range; _->maxBound
               (end,after)=gather limit (offset+1) rest
               (original,following)=T.splitAt (end-offset) raw
           in ConsChars original style (build end (col+end-offset) following after current)
         else let item=if overflow then OverflowGrapheme glyph else case displayItems glyph of
                          [value]->Grapheme value
                          _->OverflowGrapheme glyph
                  advance=if overflow then 1 else case displayItems glyph of
                    [value]->sourceItemAdvance col value
                    _->1
              in ConsSigil item style advance (build (offset+count) (col+advance) (T.drop count raw) rest current)
    gather limit !offset pending
      | offset>=limit=(offset,pending)
      | otherwise=case pending of
          (glyph,False):rest | T.length glyph==1,T.all (\c->c>=' ' && c/='\DEL') glyph,
            T.all ((==1) . scalarWidth) glyph->gather limit (offset+1) rest
          _->(offset,pending)
    dropRanges offset remaining@(range:rest)
      | sourceRangeCharEnd range<=offset=dropRanges offset rest
      | otherwise=remaining
    dropRanges _ []=[]

-- | Outermost explicit script annotation, preserved through existing color,
-- font, link and bubble wrappers. A script hint never changes source characters.
styleScript :: Style -> Maybe Script
styleScript (OverflowFragment _ style)=styleScript style
styleScript (ScriptStyle script _)=Just script
styleScript (SectionStyle _ style)=styleScript style
styleScript (BoldStyle style)=styleScript style
styleScript (ItalicStyle style)=styleScript style
styleScript (LinkStyle _ style)=styleScript style
styleScript (CodeStyle _ style)=styleScript style
styleScript (ProseStyle style)=styleScript style
styleScript (BubbleStyle _ style)=styleScript style
styleScript (BubbleText _ _ style)=styleScript style
styleScript _=Nothing

-- | Separate composable font traits while retaining color/link/bubble semantics.
-- Combining bold and italic is idempotent; wrapper order does not affect traits.
-- Script geometry is extracted separately by styleScript, not a paint trait.
fontTraits :: Style -> (Style,Bool,Bool)
fontTraits (OverflowFragment _ style)=fontTraits style
fontTraits (ScriptStyle _ style)=fontTraits style
fontTraits (SectionStyle level style)=wrap (SectionStyle level) style
fontTraits (BoldStyle style)=let (base,_,italic)=fontTraits style in (base,True,italic)
fontTraits (ItalicStyle style)=let (base,bold,_)=fontTraits style in (base,bold,True)
fontTraits (LinkStyle target style)=wrap (LinkStyle target) style
fontTraits (CodeStyle shell style)=wrap (CodeStyle shell) style
fontTraits (ProseStyle style)=wrap ProseStyle style
fontTraits (BubbleStyle outgoing style)=wrap (BubbleStyle outgoing) style
fontTraits (BubbleText offset outgoing style)=wrap (BubbleText offset outgoing) style
fontTraits style=(style,False,False)
wrap :: (Style -> Style) -> Style -> (Style,Bool,Bool)
wrap constructor style=let (base,bold,italic)=fontTraits style in (constructor base,bold,italic)

-- | A CommonMark section-heading annotation, distinct from token colors and
-- table headers. Nested font/link/bubble wrappers preserve section ownership.
sectionTitle :: Style -> Bool
sectionTitle SectionStyle{}=True
sectionTitle (OverflowFragment _ style)=sectionTitle style
sectionTitle (ScriptStyle _ style)=sectionTitle style
sectionTitle (BoldStyle style)=sectionTitle style
sectionTitle (ItalicStyle style)=sectionTitle style
sectionTitle (LinkStyle _ style)=sectionTitle style
sectionTitle (ProseStyle style)=sectionTitle style
sectionTitle (BubbleStyle _ style)=sectionTitle style
sectionTitle (BubbleText _ _ style)=sectionTitle style
sectionTitle _=False
