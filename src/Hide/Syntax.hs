{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- | Source token styles and presentation annotations shared by renderers.
--
-- Skylighting supplies language grammars; this module maps token classes to editor
-- styles. A tokenizer result is accepted only when it preserves the original
-- characters exactly. Link and bubble annotations remain in the styled stream
-- so later layout can retain interaction metadata without reparsing text.
module Hide.Syntax (Style(..), Grapheme, graphemeText, graphemeDisplayText, Sigils(..), sourceSigilsWindow, SourceRow, SourceRange, prepareSourceRow, plainSourceRow, plainSourceLine, attachSourceLine, sourceRowText, sourceRowRanges, sourceRangeCharStart, sourceRangeCharEnd, sourceRangeByteStart, sourceRangeByteEnd, sourceRangeStyle, sourceRangeText, sourceStylesAt, presentationItems, styleOverflowExtent, styleLayoutMetadata, styleScript, fontTraits, sectionTitle, highlight, highlightFor, bubbleTile, linkSpans) where

import Data.List (intercalate)
import qualified Data.List as List
import Data.Char (chr)
import Data.Word (Word32)
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import qualified Data.Vector as V
import Hide.Unicode (DisplayItem,itemScalarCount,itemOverflow,itemSourceText,itemDisplayText,itemWidth,sourceGraphemesFrom,sourceItemAdvance,displayItems,Script)
import Hide.Buffer (SourceLine,sourceLineText,sourceLineRawText,sourceLineLength,sourceLineHasChunks,sourceLineWindow)
import qualified Skylighting as S
import System.FilePath (takeFileName)

-- | Token color intent plus nested prose, link, bubble or terminal annotations.
data Style = OverflowFragment !Int Style | ScriptStyle !Script Style | SectionStyle Int Style | BoldStyle Style | ItalicStyle Style | LinkStyle T.Text Style | Plain | Heading Int | CodeStyle Bool Style | ProseStyle Style | Keyword | Comment | Literal | Number | Constructor | Pragma | BubbleStyle Bool Style | BubbleText Int Bool Style | TerminalStyle Word32 Word32 Word32 deriving (Eq,Show)

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
  , sourceRangeByteStart :: {-# UNPACK #-} !Int
  , sourceRangeByteEnd :: {-# UNPACK #-} !Int
  , sourceRangeStyle :: !Style
  } deriving (Eq,Show)

-- | Prepare compact ranges for one physical row (LF already split by the
-- owning worker; a trailing CR may remain). The source remains
-- authoritative; missing style positions are Plain, and no token character is
-- copied into the retained representation.
prepareSourceRow :: T.Text -> [(Char,Style)] -> SourceRow
prepareSourceRow text tokens=SourceRow text (V.fromList (ranges 0 0 tokens))
  where
    size=TU.lengthWord8 text
    ranges !char !byte rest
      | byte>=size=[]
      | otherwise=let style=case rest of (_,s):_->s; _->Plain
                      (endChar,endByte,after)=consume style char byte rest
                  in SourceRange char endChar byte endByte style:ranges endChar endByte after
    consume style !char !byte rest
      | byte>=size=(char,byte,rest)
      | otherwise=case rest of
          (_,s):more | s/=style->(char,byte,rest)
                     | otherwise->step more
          [] | style/=Plain->(char,byte,rest)
             | otherwise->step []
      where step more=case TU.iter text byte of TU.Iter _ bytes->consume style (char+1) (byte+bytes) more

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
newtype Grapheme = Grapheme DisplayItem deriving (Eq,Show)

graphemeText :: Grapheme -> T.Text
graphemeText (Grapheme item)=itemSourceText item
graphemeDisplayText :: Grapheme -> T.Text
graphemeDisplayText (Grapheme item)=itemDisplayText item

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

highlight :: T.Text -> [(Char,Style)]
highlight = highlightFor "Main.hs"

-- | Choose a grammar by filename, tokenize, and preserve exact source positions.
-- Unknown grammars, tokenizer failure or normalized output fall back to plain text.
highlightFor :: FilePath -> T.Text -> [(Char,Style)]
highlightFor path source = case S.syntaxesByFilename S.defaultSyntaxMap (takeFileName path) of
  syntax:_ -> case S.tokenize (S.TokenizerConfig S.defaultSyntaxMap False) syntax source of
    Right lines' ->
      let styled = intercalate [('\n',Plain)] (map (concatMap paint) lines')
                   ++ [('\n',Plain) | "\n" `T.isSuffixOf` source]
      -- Never change buffer positions if a tokenizer normalizes its input.
      in if T.pack (map fst styled) == source then styled else plain
    Left _ -> plain
  [] -> plain
  where
    plain = map (,Plain) (T.unpack source)
    paint (token,text) = map (,if markdown && token==S.FunctionTok then Heading (max 1 (min 6 (T.length (T.takeWhile (=='#') (T.stripStart text))))) else style token) (T.unpack text)
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
linkSpans :: [(Char,Style)] -> [(Int,Int,T.Text)]
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
    collect (offset,found) (_,style)=(offset+1,case target style of
      Nothing->found
      Just url->case found of
        (start,end,old):rest | end==offset && url==old -> (start,offset+1,url):rest
        _->(offset,offset+1,url):found)

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
presentationItems :: T.Text -> [(Char,Style)] -> [(T.Text,Bool)]
presentationItems text=go text (map (\i->(itemSourceText i,itemOverflow i)) (displayItems text))
  where
    go _ [] _=[]
    go remaining pending@((glyph,overflow):rest) styles=
      case styles of
        (_,style):_ | Just n<-styleOverflowExtent style,n>0,n<=32,
                      let original=T.take n remaining,T.length original==n ->
          (original,True):go (T.drop n remaining) (skip n pending) (drop n styles)
        _->case markerBefore (T.length glyph) 1 (drop 1 styles) of
          Just n->(T.take n glyph,False):go (T.drop n remaining) (skip n pending) (drop n styles)
          Nothing->let n=T.length glyph in (glyph,overflow):go (T.drop n remaining) rest (drop n styles)
    -- Inserted padding can join a leading mark/ZWJ under GB9. Captured fragment
    -- boundaries take precedence; that borrowed prefix is ordinary presentation.
    markerBefore limit n styles
      | n>=limit=Nothing
      | otherwise=case styles of
          (_,style):more | styleOverflowExtent style/=Nothing->Just n
                         | otherwise->markerBefore limit (n+1) more
          []->Nothing
    skip _ []=[]
    skip n pending@((glyph,overflow):rest)
      | n<=0=pending
      | n>=size=skip (n-size) rest
      | otherwise=(T.drop n glyph,overflow):rest
      where size=T.length glyph

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
