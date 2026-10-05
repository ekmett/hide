{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- | Source token styles and presentation annotations shared by renderers.
--
-- Skylighting supplies language grammars; this module maps token classes to editor
-- styles. A tokenizer result is accepted only when it preserves the original
-- characters exactly. Link and bubble annotations remain in the styled stream
-- so later layout can retain interaction metadata without reparsing text.
module Hide.Syntax (Style(..), Grapheme, graphemeText, Sigils(..), sourceSigilsWindow, SourceRow, SourceRange, prepareSourceRow, plainSourceRow, sourceRowText, sourceRowRanges, sourceRangeCharStart, sourceRangeCharEnd, sourceRangeByteStart, sourceRangeByteEnd, sourceRangeStyle, sourceRangeText, sourceStylesAt, styleScript, fontTraits, sectionTitle, highlight, highlightFor, bubbleTile, linkSpans) where

import Data.List (intercalate)
import qualified Data.List as List
import Data.Char (chr)
import Data.Word (Word32)
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import qualified Data.Vector as V
import Hide.Unicode (graphemes,clusterWidth,sourceGraphemesFrom,sourceGlyphAdvance,Script)
import qualified Skylighting as S
import System.FilePath (takeFileName)

-- | Token color intent plus nested prose, link, bubble or terminal annotations.
data Style = ScriptStyle !Script Style | SectionStyle Int Style | BoldStyle Style | ItalicStyle Style | LinkStyle T.Text Style | Plain | Heading Int | CodeStyle Bool Style | ProseStyle Style | Keyword | Comment | Literal | Number | Constructor | Pragma | BubbleStyle Bool Style | BubbleText Int Bool Style | TerminalStyle Word32 Word32 Word32 deriving (Eq,Show)

-- | Original source row and worker-prepared style ranges. Styling never changes
-- character positions; range boundaries also name complete UTF8 codepoints.
-- Concatenating 'sourceRangeText' over 'sourceRowRanges' recovers 'sourceRowText'.
data SourceRow = SourceRow !T.Text !(V.Vector SourceRange) | PlainSourceRow !T.Text deriving Show

-- Equality is the public text/range projection, independent of representation.
-- It is not an identity or an invalidation key.
instance Eq SourceRow where
  a==b=sourceRowText a==sourceRowText b && sourceRowRanges a==sourceRowRanges b

-- | Original source text, borrowed by either representation.
sourceRowText :: SourceRow -> T.Text
sourceRowText (SourceRow text _)=text
sourceRowText (PlainSourceRow text)=text

-- | Exact ordered ranges covering the source. Requesting ranges for an implicit
-- plain row counts its characters; visible rendering does not need that count.
sourceRowRanges :: SourceRow -> V.Vector SourceRange
sourceRowRanges (SourceRow _ ranges)=ranges
sourceRowRanges (PlainSourceRow text)
  | T.null text=V.empty
  | otherwise=V.singleton (SourceRange 0 (T.length text) 0 (TU.lengthWord8 text) Plain)

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

-- | One complete source grapheme, borrowed directly from the original row.
newtype Grapheme = Grapheme { graphemeText :: T.Text } deriving (Eq,Show)

-- | Visible source display stream. Each character in ConsChars independently
-- occupies one cell; ConsSigil retains one complete exceptional grapheme. Both
-- borrow source bytes. Strict tails avoid a separate pair/list payload layer.
-- Window fragments retain their exact source bytes and original coordinates;
-- segmentation always precedes styling, regardless of range boundaries.
data Sigils
  = ConsChars {-# UNPACK #-} !T.Text {-# UNPACK #-} !Style !Sigils
  | ConsSigil {-# UNPACK #-} !Grapheme {-# UNPACK #-} !Style {-# UNPACK #-} !Int !Sigils
  | Nil

-- | Prepare only complete graphemes overlapping a display-column window.
-- Returns the original character and display-column start of the first fragment.
-- Translation/cropping may hide a half glyph; its source fragment stays complete.
-- Tab advances use absolute columns. Styling is assigned after segmentation.
-- A zero-width window never inspects source metadata or text.
sourceSigilsWindow :: Int -> Int -> SourceRow -> (Int,Int,Sigils)
sourceSigilsWindow requested width row
  | width<=0=(0,0,Nil)
  | otherwise=let (char,byte,col,pending)=sourceGraphemesFrom left text
              in (char,col,build col char byte ranges pending)
  where
    text=sourceRowText row
    left=max 0 requested
    right=left+min (maxBound-left) width
    ranges=case row of SourceRow _ prepared->V.toList prepared; PlainSourceRow _->[]
    slice a b=TU.takeWord8 (b-a) (TU.dropWord8 a text)
    build !col !char !byte current pending
      | col>=right=Nil
      | otherwise=case pending of
          []->Nil
          glyph:rest->emit col char byte current glyph (T.length glyph) (sourceGlyphAdvance col glyph) rest
    emit col char byte current glyph n advance rest=
      let active=dropWhile ((<=char).sourceRangeCharEnd) current
          style=case active of range:_->sourceRangeStyle range; _->Plain
          endByte=byte+TU.lengthWord8 glyph
          endChar=char+n
      in if n==1 && advance==1 && T.all (\c->c>=' ' && c/='\DEL') glyph then
           let limit=case active of range:_->sourceRangeCharEnd range; _->maxBound
               (finishChar,finishByte,after)=gather limit (col+1) endChar endByte rest
           in ConsChars (slice byte finishByte) style (build (col+finishChar-char) finishChar finishByte active after)
         else ConsSigil (Grapheme (slice byte endByte)) style advance (build (col+advance) endChar endByte active rest)
    gather limit !col !char !byte pending
      | char>=limit || col>=right=(char,byte,pending)
      | otherwise=case pending of
          glyph:rest | T.length glyph==1,clusterWidth glyph==1,T.all (\c->c>=' ' && c/='\DEL') glyph->
            gather limit (col+1) (char+1) (byte+TU.lengthWord8 glyph) rest
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

-- | Outermost explicit script annotation, preserved through existing color,
-- font, link and bubble wrappers. A script hint never changes source characters.
styleScript :: Style -> Maybe Script
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
sectionTitle (ScriptStyle _ style)=sectionTitle style
sectionTitle (BoldStyle style)=sectionTitle style
sectionTitle (ItalicStyle style)=sectionTitle style
sectionTitle (LinkStyle _ style)=sectionTitle style
sectionTitle (ProseStyle style)=sectionTitle style
sectionTitle (BubbleStyle _ style)=sectionTitle style
sectionTitle (BubbleText _ _ style)=sectionTitle style
sectionTitle _=False
