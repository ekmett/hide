{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Markdown (renderMarkdown) where

import qualified Commonmark as C
import Commonmark.Extensions.PipeTable
import Commonmark.Entity (lookupEntity)
import Data.Functor.Identity (runIdentity)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Skylighting as S
import THC.Edit.Unicode (graphemes, clusterWidth)
import THC.Edit.Buffer (columnOffset, displayColumn, nextCharacter)
import THC.Edit.Syntax (Style(..), highlightFor)

type Styled = [(Char,Style)]
newtype Inline = Inline Styled deriving (Show, Semigroup, Monoid)
newtype Blocks = Blocks [Block] deriving (Show, Semigroup, Monoid)
data Block = Table [ColAlignment] [Styled] [[Styled]] | Code Bool Styled | Flow Styled | Pre Styled | Indent T.Text Blocks | Gap deriving Show

instance C.Rangeable Inline where ranged _ = id
instance C.HasAttributes Inline where addAttributes _ = id
instance C.Rangeable Blocks where ranged _ = id
instance C.HasAttributes Blocks where addAttributes _ = id

instance C.IsInline Inline where
  lineBreak = Inline [('\n',Plain)]
  softBreak = C.str " "
  str = Inline . paint Plain
  entity text = C.str (fromMaybe text (lookupEntity (T.drop 1 text)))
  escapedChar = C.str . T.singleton
  emph = tint Constructor
  strong = tint Keyword
  link url _ label@(Inline chars)
    | textOf chars == url = tint Literal label
    | otherwise = tint Literal label <> Inline (paint Comment (" (" <> url <> ")"))
  image url title label = C.str "[image: " <> C.link url title label <> C.str "]"
  code = Inline . paint Literal
  rawInline _ = C.str

instance C.IsBlock Inline Blocks where
  paragraph (Inline []) = mempty
  paragraph (Inline chars) = Blocks [Flow chars, Gap]
  plain (Inline []) = mempty
  plain (Inline chars) = Blocks [Flow chars]
  thematicBreak = Blocks [Flow (paint Comment "───"), Gap]
  blockQuote blocks = Blocks [Indent "> " (trim blocks), Gap]
  codeBlock info source = Blocks [Code (shellBlock info) (codeStyles info source), Gap]
  heading level content = let Inline chars = tint (Heading level) content in Blocks [Flow chars,Gap]
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
    where unInline (Inline chars)=chars

-- CommonMark handles incomplete input too, so streaming callers keep ownership
-- of the raw source and may simply render each new accumulated chunk.
renderMarkdown :: Int -> T.Text -> Styled
renderMarkdown requested source = intercalate [('\n',Plain)] $ render width $ trim $
  either (const (Blocks [Pre (paint Plain source)])) id (runIdentity (C.commonmarkWith (pipeTableSpec <> C.defaultSyntaxSpec) "" source))
  where width = max 1 requested

paint :: Style -> T.Text -> Styled
paint style = map (,style) . T.unpack

tint :: Style -> Inline -> Inline
tint style (Inline chars) = Inline [(c,if old == Plain then style else old) | (c,old) <- chars]

textOf :: Styled -> T.Text
textOf = T.pack . map fst

columns :: Styled -> Int
columns chars = let text = textOf chars in displayColumn text (T.length text)

trim :: Blocks -> Blocks
trim (Blocks blocks) = Blocks (reverse (dropWhile gap (reverse blocks)))
  where gap Gap = True; gap _ = False

render :: Int -> Blocks -> [Styled]
render width (Blocks blocks) = concatMap block blocks
  where
    block (Flow chars) = concatMap (wrapWords width) (rows chars)
    block (Code shell chars) =
      let margin=if width>=8 then 2 else 0
          panelWidth=width-margin
          padding=if panelWidth>=3 then 1 else 0
          inner=max 1 (panelWidth-2*padding)
          base=CodeStyle shell Plain
          line xs=paint Plain (T.replicate margin " ") ++ paint base (T.replicate padding " ") ++
            [(c,CodeStyle shell style) | (c,style)<-xs] ++ paint base (T.replicate (max 0 (panelWidth-padding-columns xs)) " ")
          content=concatMap (wrapExact inner) (rows (stripFinalNewline (expandTabs chars)))
      in map line ([]:content++[[]])
    block (Table aligns header body)=renderTable width aligns header body
    block (Pre chars) = concatMap (wrapExact width) (rows (stripFinalNewline (expandTabs chars)))
    block Gap = [[]]
    block (Indent prefix content)
      | indent >= width = concatMap (wrapExact width) (attach (render width content))
      | otherwise = attach (render (width-indent) content)
      where
        indent = T.length prefix
        attach [] = [paint Comment (T.stripEnd prefix)]
        attach (first:rest) = (paint Comment prefix ++ first) : map (paint Plain (T.replicate indent " ") ++) rest

renderTable :: Int -> [ColAlignment] -> [Styled] -> [[Styled]] -> [Styled]
renderTable width aligns header body
  | count==0 = []
  | width < count*4+1 = concat [concat [wrapWords width (h++paint Comment ": "++value) | (h,value)<-zip header row] ++ [[]] | row<-body]
  | otherwise = [rule '┌' '┬' '┐'] ++ rowLines True header ++ [rule '├' '┼' '┤'] ++ concatMap (rowLines False) body ++ [rule '└' '┴' '┘']
  where
    count=length header
    budget=width-3*count-1
    natural=[maximum (1:[columns cell | row<-header:body,cell<-take 1 (drop i row)]) | i<-[0..count-1]]
    shrink widths | sum widths<=budget = widths
                  | otherwise = let biggest=maximum widths; (before,after)=break (==biggest) widths
                               in shrink (before++[biggest-1]++drop 1 after)
    sizes=shrink natural
    rule a b c=paint Comment (T.singleton a<>T.intercalate (T.singleton b) [T.replicate (n+2) "─" | n<-sizes]<>T.singleton c)
    rowLines isHeader cells=
      let wrapped=zipWith wrapWords sizes (take count (cells++repeat []))
          height=maximum (1:map length wrapped)
          line j=paint Comment "│"++concat [paint Plain " "++pad alignment n (if isHeader then [(c,Heading 2) | (c,_)<-part] else part)++paint Comment " │"
            | (n,alignment,parts)<-zip3 sizes (aligns++repeat DefaultAlignedCol) wrapped, let part=case drop j parts of x:_->x; _->[]]
      in map line [0..height-1]
    pad alignment n chars=paint Plain (T.replicate left " ")++chars++paint Plain (T.replicate (extra-left) " ")
      where extra=max 0 (n-columns chars)
            left=case alignment of RightAlignedCol->extra; CenterAlignedCol->extra `div` 2; _->0

rows :: Styled -> [Styled]
rows chars = let (line,rest) = break ((== '\n') . fst) chars in line : case rest of [] -> []; _:more -> rows more

stripFinalNewline :: Styled -> Styled
stripFinalNewline chars = case reverse chars of ('\n',_):rest -> reverse rest; _ -> chars

wrapWords :: Int -> Styled -> [Styled]
wrapWords width = go [] . wordsStyled
  where
    go current [] = [current]
    go [] (word:rest)
      | columns word > width = let parts = wrapExact width word in init parts ++ go (last parts) rest
      | otherwise = go word rest
    go current remaining@(word:rest)
      | columns current + 1 + columns word <= width = go (current ++ [(' ',Plain)] ++ word) rest
      | otherwise = current : go [] remaining
    wordsStyled chars = case dropWhile (isSpace . fst) chars of
      [] -> []
      remaining -> let (word,rest) = break (isSpace . fst) remaining in word : wordsStyled rest

wrapExact :: Int -> Styled -> [Styled]
wrapExact width chars = go (textOf chars) chars
  where
    go _ [] = [[]]
    go text remaining =
      let offset = max (nextCharacter text 0) (columnOffset text width)
          (part,rest) = splitAt offset remaining
          (marks,after) = span (\(c,_) -> displayColumn (T.singleton c) 1 == 0) rest
          row = part ++ marks
      in row : [line | not (null after), line <- go (T.drop (length row) text) after]

expandTabs :: Styled -> Styled
expandTabs chars = go 0 (graphemes (textOf chars)) chars
  where
    go _ [] _=[]
    go col (g:gs) remaining = expanded ++ go next gs rest
      where
        (part,rest)=splitAt (T.length g) remaining
        style=case part of (_,s):_->s; _->Plain
        count=8-col `mod` 8
        expanded=if g=="\t" then replicate count (' ',style) else part
        next | g=="\n"=0
             | g=="\t"=col+count
             | otherwise=col+clusterWidth g

shellBlock :: T.Text -> Bool
shellBlock info = case T.words (T.toLower info) of
  language:_ -> language `elem` ["sh","bash","shell","shellsession","console","terminal","zsh","fish","powershell","pwsh","ps1","cmd","bat","batch","dos"]
  [] -> False

codeStyles :: T.Text -> T.Text -> Styled
codeStyles info source = case T.words info of
  language:_ -> case S.lookupSyntax language S.defaultSyntaxMap of
    Just syntax -> case listToMaybe (S.sExtensions syntax) of
      Just pattern' -> highlightFor (T.unpack (T.replace "*" "code" (T.pack pattern'))) source
      Nothing -> plain
    Nothing -> highlightFor ("code." ++ T.unpack language) source
  [] -> plain
  where plain = paint Plain source
