-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- |
-- Module      : Hide.Markdown
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : BangPatterns, OverloadedStrings
--
-- CommonMark layout into borrowed styled rows for help and conversations.
--
-- Parsing builds a small block representation, then wrapping and table layout
-- use display-cell widths. Code panels retain their original source separately
-- from padded, highlighted display text. Link targets remain nested style data;
-- shell spans identify executable source without reconstructing it from the grid.
module Hide.Markdown (Markdown, MarkdownBlock, parseMarkdown, markdownBlocks, markdownBlockText, markdownBlockLinks, markdownBlockShell, markdownIntrinsicWidth, renderMarkdownBlock, renderMarkdown, renderMarkdownRows, renderMarkdownWithShellBlocks) where

import qualified Commonmark as C
import Commonmark.Extensions.PipeTable
import Commonmark.Entity (lookupEntity)
import Data.Functor.Identity (runIdentity)
import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import Hide.LineChunks (joinAdjacent)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Skylighting as S
import Hide.Unicode (displayItems, itemSourceText, itemWidth,sourceTextWidth)
import Hide.Syntax (Style(..), Sigils(..), StyledText, StyledRow(..), MappedStyledRow(..), styledContents, compactStyled, styledLength, styledRows, styledColumn, presentationItems, styleOverflowExtent, highlightFor, linkSpans)

type Styled = [(T.Text,Style,Maybe (Int,Int,Int))]
newtype Inline = Inline (Seq.Seq (T.Text,Style,Maybe (Int,Int,Int))) deriving (Show, Semigroup, Monoid)
newtype Blocks = Blocks [Block] deriving (Show, Semigroup, Monoid)
data Block = Table [ColAlignment] [Styled] [[Styled]] | Code T.Text T.Text Styled | Flow Styled | Pre Styled | Indent T.Text Blocks | Gap deriving Show

instance C.Rangeable Inline where ranged _ = id
instance C.HasAttributes Inline where addAttributes _ = id
instance C.Rangeable Blocks where ranged _ = id
instance C.HasAttributes Blocks where addAttributes _ = id

instance C.IsInline Inline where
  lineBreak = Inline (Seq.singleton ("\n",Plain,Nothing))
  softBreak = C.str " "
  str = Inline . Seq.fromList . paint Plain
  entity text = C.str (fromMaybe text (lookupEntity (T.drop 1 text)))
  escapedChar = C.str . T.singleton
  emph = decorate ItalicStyle . tint Constructor
  strong = decorate BoldStyle . tint Keyword
  link url _ (Inline chars) = Inline (fmap (\(c,s,sourceSpan)->(c,LinkStyle url (if s==Plain then Literal else s),sourceSpan)) chars)
  image url title label = C.str "[image: " <> C.link url title label <> C.str "]"
  code = Inline . Seq.fromList . paint Literal
  rawInline _ = C.str

instance C.IsBlock Inline Blocks where
  paragraph (Inline chars) | Seq.null chars = mempty
  paragraph (Inline chars) = Blocks [Flow (toList chars), Gap]
  plain (Inline chars) | Seq.null chars = mempty
  plain (Inline chars) = Blocks [Flow (toList chars)]
  thematicBreak = Blocks [Flow (paint Comment "───"), Gap]
  blockQuote blocks = Blocks [Indent "> " (trim blocks), Gap]
  codeBlock info source = Blocks [Code info source (codeStyles info source), Gap]
  heading level content = let Inline chars = decorate (SectionStyle level) (decorate BoldStyle (tint (Heading level) content)) in Blocks [Flow (toList chars),Gap]
  rawBlock _ source = Blocks [Pre (paint Plain source),Gap]
  referenceLinkDefinition _ _ = mempty
  list kind spacing items = Blocks (concat (zipWith item [first..] items) ++ [Gap])
    where
      first = case kind of C.OrderedList n _ _ -> n; _ -> 1
      item n blocks = [Indent (prefix n) (trim blocks)] ++ [Gap | spacing == C.LooseList]
      prefix n = case kind of
        C.BulletList _ -> "• "
        C.OrderedList _ _ delimiter -> T.pack (show n) <> if delimiter == C.Period then ". " else ") "

instance HasPipeTable Inline Blocks where
  pipeTable aligns header body = Blocks [Table aligns (map unInline header) (map (map unInline) body),Gap]
    where unInline (Inline chars)=toList chars

-- | Render accumulated Markdown at a cell width, including incomplete streaming input.
renderMarkdown :: Int -> T.Text -> StyledText
renderMarkdown width = fst . renderMarkdownWithShellBlocks width

-- | Finalize one physical row at a time. The enclosing list is lazy; no strict
-- Sigils chain spans the complete Markdown document.
renderMarkdownRows :: Int -> T.Text -> [StyledRow]
renderMarkdownRows width=styledRows . renderMarkdown width

-- | Parse once into existing logical blocks. This immutable owner is independent
-- of width; wrapping never reparses or changes its lexical source.
data Markdown = Markdown [MarkdownBlock]
data MarkdownBlock = MarkdownBlock !Block !T.Text !Bool

parseMarkdown :: T.Text -> Markdown
parseMarkdown source=Markdown (normalize (trim (either (const (Blocks [Pre (paint Plain source)])) id
  (runIdentity (C.parseCommonmarkWith (pipeTableSpec <> C.defaultSyntaxSpec) tokens)))))
  where
    -- Tables need an EOF line-end token. Its empty contents keep exact code
    -- and raw-block payloads unchanged when the source has no final newline.
    complete=T.null source || T.isSuffixOf "\n" source || T.isSuffixOf "\r" source
    original=C.tokenize "" (if complete then source else T.snoc source '\n')
    tokens=if complete then original else endLine original
    endLine []=[]
    endLine [token]=[token {C.tokContents=""}]
    endLine (token:rest)=token:endLine rest

-- | Borrow ordered logical parser blocks. Separators are owned here, never
-- inferred from wrapped rows. The final block has no synthetic trailing break.
markdownBlocks :: Markdown -> [MarkdownBlock]
markdownBlocks (Markdown blocks)=blocks

-- | Canonical copy text before width decorations: normalized prose, original
-- code/tabs, table cells separated by tabs/rows by LF, and explicit block gaps.
-- List markers, padding and soft-wrap newlines contribute no source characters.
markdownBlockText :: MarkdownBlock -> T.Text
markdownBlockText (MarkdownBlock _ text _)=text

-- | Link offsets share this block's canonical scalar coordinates.
markdownBlockLinks :: MarkdownBlock -> [(Int,Int,T.Text)]
markdownBlockLinks (MarkdownBlock block _ _)=concatMap links (runs block)
  where
    links (text,style,Just (a,z,_))=[(a,z,url) | (_,_,url)<-linkSpans [(text,style)]]
    links _=[]
    runs (Flow chars)=chars
    runs (Pre chars)=chars
    runs (Code _ _ chars)=chars
    runs (Table _ header body)=concat (header++concat body)
    runs (Indent _ (Blocks children))=concatMap runs children
    runs Gap=[]

-- | Executable shell source remains the original parser body, not padded text.
markdownBlockShell :: MarkdownBlock -> Maybe (T.Text,T.Text)
markdownBlockShell (MarkdownBlock (Code info source _) _ _)=(,source) <$> executableShell info
markdownBlockShell _=Nothing

-- | A lazy intrinsic cap for the demanded parsed owner. This numeric source
-- walk does not prepare decorated rows or other transcript items.
markdownIntrinsicWidth :: Markdown -> Int
markdownIntrinsicWidth (Markdown blocks)=maximum (1:[sourceTextWidth line
  | block<-blocks,line<-T.splitOn "\n" (markdownBlockText block)])

-- | Demand mapped physical rows from one immutable logical block. The map is
-- consumed by TextLayout; no global source-map owner or transcript is created.
renderMarkdownBlock :: Bool -> Int -> MarkdownBlock -> [MappedStyledRow]
renderMarkdownBlock wide width (MarkdownBlock block canonical gap)=mapped 0 (render wide (max 1 width) (Blocks ([block]++[Gap | gap])))
  where
    mapped _ []=[]
    mapped boundary ((runs,_):rest)=
      let compact=compactRuns runs
          ranges=V.fromList (snd (foldl collect (0,[]) compact))
          starts=[a | (_,_,a,_)<-V.toList ranges]
          ends=[z | (_,_,_,z)<-V.toList ranges]
          start=case starts of []->boundary; _->minimum starts
          end=if null rest then T.length canonical else case ends of []->boundary; _->maximum ends
          row=case styledRows (plainRuns compact) of first:_->first; []->StyledRow Nil Nothing V.empty
      in MappedStyledRow row start end ranges:mapped end rest
    collect (offset,found) (text,_,source)=
      let end=offset+T.length text
      in (end,found++maybe [] (\(a,z,_)->[(offset,end,a,z)]) source)

normalize :: Blocks -> [MarkdownBlock]
normalize (Blocks blocks)=go blocks
  where
    go []=[]
    go (Gap:rest)=go rest
    go (block:rest)=
      let (prepared,text)=canonicalBlock block
          gap=case rest of Gap:_->True; _->False
          remaining=case rest of Gap:after->after; _->rest
          separator=if gap then "\n\n" else if null remaining then "" else "\n"
      in MarkdownBlock prepared (text<>separator) gap:go remaining
    canonicalBlock (Flow runs)=let chars=normalizeProse runs in (Flow (assign 0 chars),textOf chars)
    canonicalBlock (Pre runs)=(Pre (assign 0 runs),textOf runs)
    canonicalBlock (Code info source runs)=(Code info source (assign 0 runs),source)
    canonicalBlock (Table aligns header body)=
      let (_,preparedRows)=mapAccum 0 (header:body)
          prepareRow start cells=mapAccumCells start cells
          mapAccum offset []=(offset,[])
          mapAccum offset (row:rest)=
            let (end,prepared)=prepareRow offset row
                (finish,more)=mapAccum (end+1) rest
            in (finish,prepared:more)
          mapAccumCells offset []=(offset,[])
          mapAccumCells offset (cell:rest)=
            let normalized=normalizeProse cell
                count=lengthOf normalized
                (finish,more)=mapAccumCells (offset+count+1) rest
            in (if null rest then offset+count else finish,assign offset normalized:more)
          text=T.intercalate "\n" [T.intercalate "\t" (map (textOf . normalizeProse) row) | row<-header:body]
      in case preparedRows of first:rest->(Table aligns first rest,text); []->(Table aligns [] [],text)
    canonicalBlock (Indent prefix children)=
      let normalized=normalize children
          (_,prepared)=foldl (\(offset,found) (MarkdownBlock block text gap)->
            (offset+T.length text,found++[shift offset block]++[Gap | gap])) (0,[]) normalized
      in (Indent prefix (Blocks prepared),T.concat (map markdownBlockText normalized))
    canonicalBlock Gap=(Gap,"")

-- Assign once before wrapping. Every split then adjusts its explicit source
-- interval; padding never gains an interval merely by sharing a Text array.
assign :: Int -> Styled -> Styled
assign _ []=[]
assign offset ((text,style,_):rest)=let end=offset+T.length text
  in (text,style,Just (offset,end,end-offset)):assign end rest

shift :: Int -> Block -> Block
shift offset block=case block of
  Flow runs->Flow (change runs); Pre runs->Pre (change runs)
  Code info source runs->Code info source (change runs)
  Table aligns header body->Table aligns (map change header) (map (map change) body)
  Indent prefix (Blocks children)->Indent prefix (Blocks (map (shift offset) children))
  Gap->Gap
  where change=map (\(text,style,source)->(text,style,fmap (\(a,z,n)->(a+offset,z+offset,n)) source))

normalizeProse :: Styled -> Styled
normalizeProse=intercalate [("\n",Plain,Nothing)] . map normalizeLine . rows
  where
    normalizeLine=compactRuns . intercalate [(" ",Plain,Nothing)] . wordsOf . boundedParts
    wordsOf pending=case dropWhile whitespace pending of
      []->[]
      remaining->let (word,rest)=break whitespace remaining in concat word:wordsOf rest
    whitespace part=all (\(text,_,_)->T.all isSpace text) part && all (\(_,style,_)->styleOverflowExtent style==Nothing) part

-- | Return borrowed styled runs and half-open scalar-offset shell spans.
-- Each span carries dialect and original parser code body, before wrapping, tab
-- expansion and padding. Offsets count characters, not terminal cells.
renderMarkdownWithShellBlocks :: Int -> T.Text -> (StyledText,[(Int,Int,T.Text,T.Text)])
renderMarkdownWithShellBlocks requested source =
  (compactStyled (plainRuns (intercalate [("\n",Plain,Nothing)] (map fst rendered))), reverse (snd (foldl collect (0,[]) rendered)))
  where
    Markdown parsed=parseMarkdown source
    rendered=concatMap (\(MarkdownBlock block _ gap)->render False (max 1 requested) (Blocks ([block]++[Gap | gap]))) parsed
    collect (offset,found) (chars,payload)=
      let end=offset+lengthOf chars
          next=case payload of
            Nothing -> found
            Just (dialect,body) -> case found of
              (start,previous,oldDialect,oldBody):rest
                | previous+1==offset && oldDialect==dialect && oldBody==body -> (start,end,dialect,body):rest
              _ -> (offset,end,dialect,body):found
      in (end+1,next)

executableShell :: T.Text -> Maybe T.Text
executableShell info = case T.words (T.toLower info) of
  language:_ | language `elem` ["sh","bash","zsh"] -> Just language
  "shell":_ -> Just "sh"
  _ -> Nothing

paint :: Style -> T.Text -> Styled
paint style text=[(text,style,Nothing) | not (T.null text)]

tint :: Style -> Inline -> Inline
tint style (Inline chars) = Inline (fmap (\(c,old,sourceSpan)->(c,if old==Plain then style else old,sourceSpan)) chars)

decorate :: (Style -> Style) -> Inline -> Inline
decorate style (Inline chars)=Inline (fmap (\(c,old,sourceSpan)->(c,style old,sourceSpan)) chars)

textOf :: Styled -> T.Text
textOf = styledContents . plainRuns

columns :: Bool -> Styled -> Int
columns wide=styledColumn wide 0 . plainRuns

trim :: Blocks -> Blocks
trim (Blocks blocks) = Blocks (reverse (dropWhile gap (reverse blocks)))
  where gap Gap = True; gap _ = False

render :: Bool -> Int -> Blocks -> [(Styled,Maybe (T.Text,T.Text))]
render wide width (Blocks blocks) = concatMap block blocks
  where
    block (Flow chars) = plainRows (concatMap (wrapWords wide width) (rows chars))
    block (Code info source chars) =
      let margin=if width>=8 then 2 else 0
          panelWidth=width-margin
          padding=if panelWidth>=3 then 1 else 0
          inner=max 1 (panelWidth-2*padding)
          shell=shellBlock info
          base=CodeStyle shell Plain
          line xs=paint Plain (T.replicate margin " ") ++ paint base (T.replicate padding " ") ++
            [(c,CodeStyle shell style,sourceSpan) | (c,style,sourceSpan)<-xs] ++ paint base (T.replicate (max 0 (panelWidth-padding-columns wide xs)) " ")
          content=concatMap (wrapExact wide inner) (rows (stripFinalNewline (expandTabs chars)))
      in [(line xs, (,source) <$> executableShell info) | xs<-[]:content++[[]]]
    block (Table aligns header body)=plainRows (renderTable wide width aligns header body)
    block (Pre chars) = plainRows (concatMap (wrapExact wide width) (rows (stripFinalNewline (expandTabs chars))))
    block Gap = [([],Nothing)]
    block (Indent prefix content)
      | indent >= width = concatMap (\(chars,payload)->map (,payload) (wrapExact wide width chars)) (attach (render wide width content))
      | otherwise = attach (render wide (width-indent) content)
      where
        indent = T.length prefix
        attach [] = [(paint Comment (T.stripEnd prefix),Nothing)]
        attach ((first,payload):rest) = (paint Comment prefix ++ first,payload) : map (\(chars,tag)->(paint Plain (T.replicate indent " ") ++ chars,tag)) rest
    plainRows=map (,Nothing)

renderTable :: Bool -> Int -> [ColAlignment] -> [Styled] -> [[Styled]] -> [Styled]
renderTable wide width aligns header body
  | count==0 = []
  | width < count*4+1 = concat [concat [wrapWords wide width (h++paint Comment ": "++value) | (h,value)<-zip header row] ++ [[]] | row<-body]
  | otherwise = [rule '┌' '┬' '┐'] ++ rowLines True header ++ [rule '├' '┼' '┤'] ++ concatMap (rowLines False) body ++ [rule '└' '┴' '┘']
  where
    count=length header
    budget=width-3*count-1
    natural=[maximum (1:[columns wide cell | row<-header:body,cell<-take 1 (drop i row)]) | i<-[0..count-1]]
    shrink widths | sum widths<=budget = widths
                  | otherwise = let biggest=maximum widths; (before,after)=break (==biggest) widths
                               in shrink (before++[biggest-1]++drop 1 after)
    sizes=shrink natural
    rule a b c=paint Comment (T.singleton a<>T.intercalate (T.singleton b) [T.replicate (n+2) "─" | n<-sizes]<>T.singleton c)
    rowLines isHeader cells=
      let wrapped=zipWith (wrapWords wide) sizes (take count (cells++repeat []))
          height=maximum (1:map length wrapped)
          line j=paint Comment "│"++concat [paint Plain " "++pad alignment n (if isHeader then [(c,headerStyle s,sourceSpan) | (c,s,sourceSpan)<-part] else part)++paint Comment " │"
            | (n,alignment,parts)<-zip3 sizes (aligns++repeat DefaultAlignedCol) wrapped, let part=case drop j parts of x:_->x; _->[]]
      in map line [0..height-1]
    headerStyle (OverflowFragment n style)=OverflowFragment n (headerStyle style)
    headerStyle (LinkStyle url _)=LinkStyle url (Heading 2)
    headerStyle _=Heading 2
    pad alignment n chars=paint Plain (T.replicate left " ")++chars++paint Plain (T.replicate (extra-left) " ")
      where extra=max 0 (n-columns wide chars)
            left=case alignment of RightAlignedCol->extra; CenterAlignedCol->extra `div` 2; _->0

rows :: Styled -> [Styled]
rows=splitRows

stripFinalNewline :: Styled -> Styled
stripFinalNewline chars=case reverse chars of
  (text,style,sourceSpan):rest | "\n" `T.isSuffixOf` text->
    reverse ([(T.dropEnd 1 text,style,fmap (\(a,z,n)->(a,z-1,n-1)) sourceSpan) | T.length text>1]++rest)
  _->chars

wrapWords :: Bool -> Int -> Styled -> [Styled]
wrapWords wide width = go 0 [] . map (\word->(word,columns wide word)) . wordsOf . boundedParts
  where
    -- Word splitting removes tabs; advances compose with the inserted one-cell
    -- blank. Carry the accepted width instead of segmenting the row again.
    go !_ current [] = [current]
    go !_ [] ((word,advance):rest)
      | advance > width =
          let parts=wrapExact wide width word; final=last parts
          in init parts ++ go (columns wide final) final rest
      | otherwise = go advance word rest
    go !col current remaining@((word,advance):rest)
      | col + 1 + advance <= width = go (col+1+advance) (current ++ [(" ",Plain,between current word)] ++ word) rest
      | otherwise = current : go 0 [] remaining
    wordsOf pending=case dropWhile whitespace pending of
      []->[]
      remaining->let (word,rest)=break whitespace remaining in compactRuns (concat word):wordsOf rest
    whitespace part=all (\(text,_,_)->T.all isSpace text) part && all (\(_,style,_)->styleOverflowExtent style==Nothing) part

-- Canonical prose has one space between words. Its rendered blank retains that
-- source interval; the same interval is merely absent at a soft row boundary.
between :: Styled -> Styled -> Maybe (Int,Int,Int)
between left right=case (extent left,extent right) of
  (Just (_,a),Just (z,_)) | a<z->Just (a,z,1)
  _->Nothing

-- Capture overflow boundaries before any operation introduces row breaks or
-- trims whitespace. Each part remains atomic, including space + combining runs.
boundedParts :: Styled -> [Styled]
boundedParts chars=go (presentationItems (textOf chars) (plainRuns chars)) chars
  where
    go [] _=[]
    go ((text,overflow):rest) remaining=
      let (part,after)=splitRuns (T.length text) remaining
      in mark overflow part:go rest after
    mark True part@((_,style,_):_)
      | styleOverflowExtent style==Nothing=
        let (first,after)=splitRuns 1 part
        in [(t,OverflowFragment (lengthOf part) s,sourceSpan) | (t,s,sourceSpan)<-first]++after
    mark _ part=part

wrapExact :: Bool -> Int -> Styled -> [Styled]
wrapExact wide width chars=go [] 0 (boundedParts chars)
  where
    go current _ []=[compactRuns (concat (reverse current))]
    go current col pending@(part:rest)
      | not (null current) && col+advance>width=compactRuns (concat (reverse current)):go [] 0 pending
      | otherwise=go (part:current) (col+advance) rest
      where advance=columns wide part

expandTabs :: Styled -> Styled
expandTabs chars
  | not (any (\(text,_,_)->T.any (=='\t') text) chars)=chars
  | otherwise=go 0 (displayItems (textOf chars)) chars
  where
    go _ [] _=[]
    go col (item:gs) remaining = expanded ++ go next gs rest
      where
        g=itemSourceText item
        (part,rest)=splitRuns (T.length g) remaining
        style=case part of (_,s,_):_->s; _->Plain
        count=8-col `mod` 8
        expanded=if g=="\t" then [(T.replicate count " ",style,fmap (\(a,z)->(a,z,count)) (extent part))] else part
        next | g=="\n"=0
             | g=="\t"=col+count
             | otherwise=col+itemWidth item

shellBlock :: T.Text -> Bool
shellBlock info = case T.words (T.toLower info) of
  language:_ -> language `elem` ["sh","bash","shell","shellsession","console","terminal","zsh","fish","powershell","pwsh","ps1","cmd","bat","batch","dos"]
  [] -> False

codeStyles :: T.Text -> T.Text -> Styled
codeStyles info source = case T.words info of
  language:_ -> case S.lookupSyntax language S.defaultSyntaxMap of
    Just syntax -> case listToMaybe (S.sExtensions syntax) of
      Just pattern' -> lexical (highlightFor (T.unpack (T.replace "*" "code" (T.pack pattern'))) source)
      Nothing -> plain
    Nothing -> lexical (highlightFor ("code." ++ T.unpack language) source)
  [] -> plain
  where plain = paint Plain source

-- The parser's additional run field records its logical range and cached paint
-- scalar count. Splitting a bounded prefix never counts its remaining suffix.
-- It stays local to a demanded block/row, not an extensible layout/event tree.
plainRuns :: Styled -> StyledText
plainRuns=map (\(text,style,_)->(text,style))
lexical :: StyledText -> Styled
lexical=map (\(text,style)->(text,style,Nothing))
lengthOf :: Styled -> Int
lengthOf=styledLength . plainRuns
extent :: Styled -> Maybe (Int,Int)
extent runs=case [(a,z) | (_,_,Just (a,z,_))<-runs] of
  []->Nothing
  ranges->Just (minimum (map fst ranges),maximum (map snd ranges))

compactRuns :: Styled -> Styled
compactRuns []=[]
compactRuns ((text,style,source):rest)
  | T.null text=compactRuns rest
  | otherwise=gather text source rest
  where
    gather current sourceSpan ((next,other,following):more)
      | T.null next=gather current sourceSpan more
      | style==other,styleOverflowExtent style==Nothing,Just joined<-joinAdjacent current next,
        Just combined<-merge sourceSpan following=gather joined combined more
    gather current sourceSpan remaining=(current,style,sourceSpan):compactRuns remaining
    merge Nothing Nothing=Just Nothing
    merge (Just (a,z,n)) (Just (b,end,m)) | z==b=Just (Just (a,end,n+m))
                                       | a==b && z==end=Just (Just (a,z,n+m))
    merge _ _=Nothing

splitRuns :: Int -> Styled -> (Styled,Styled)
splitRuns requested=go (max 0 requested) []
  where
    go _ done []=(reverse done,[])
    go n done remaining | n<=0=(reverse done,remaining)
    go n done ((text,style,source):rest)
      | T.null after=go (n-T.length before) ((before,style,source):done) rest
      | otherwise=let count=T.length before
                      first=fmap (\(a,z,size)->(a,a+(count*(z-a)+size-1) `div` size,count)) source
                      lastRange=fmap (\(a,z,size)->(a+count*(z-a) `div` size,z,size-count)) source
                  in (reverse ((before,style,first):done),(after,style,lastRange):rest)
      where (before,after)=T.splitAt n text

splitRows :: Styled -> [Styled]
splitRows=go []
  where
    go current []=[reverse current]
    go current (run@(text,_,_):rest)
      | T.null after=go (run:current) rest
      | otherwise=let (beforeRuns,remaining)=splitRuns (T.length before) [run]
                      (_,afterBreak)=splitRuns 1 remaining
                  in (reverse current++beforeRuns):go [] (afterBreak++rest)
      where (before,after)=T.breakOn "\n" text
