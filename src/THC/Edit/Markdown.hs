{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Markdown (renderMarkdown) where

import qualified Commonmark as C
import Commonmark.Entity (lookupEntity)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Skylighting as S
import THC.Edit.Buffer (columnOffset, displayColumn)
import THC.Edit.Syntax (Style(..), highlightFor)

type Styled = [(Char,Style)]
newtype Inline = Inline Styled deriving (Show, Semigroup, Monoid)
newtype Blocks = Blocks [Block] deriving (Show, Semigroup, Monoid)
data Block = Flow Styled | Pre Styled | Indent T.Text Blocks | Gap deriving Show

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
  codeBlock info source = Blocks [Pre (codeStyles info source), Gap]
  heading _ content = let Inline chars = tint Keyword content in Blocks [Flow chars,Gap]
  rawBlock _ source = Blocks [Pre (paint Plain source),Gap]
  referenceLinkDefinition _ _ = mempty
  list kind spacing items = Blocks (concat (zipWith item [first..] items) ++ [Gap])
    where
      first = case kind of C.OrderedList n _ _ -> n; _ -> 1
      item n blocks = [Indent (prefix n) (trim blocks)] ++ [Gap | spacing == C.LooseList]
      prefix n = case kind of
        C.BulletList _ -> "• "
        C.OrderedList _ _ delimiter -> T.pack (show n) <> if delimiter == C.Period then ". " else ") "

-- CommonMark handles incomplete input too, so streaming callers keep ownership
-- of the raw source and may simply render each new accumulated chunk.
renderMarkdown :: Int -> T.Text -> Styled
renderMarkdown requested source = intercalate [('\n',Plain)] $ render width $ trim $
  either (const (Blocks [Pre (paint Plain source)])) id (C.commonmark "" source)
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
    block (Pre chars) = concatMap (wrapExact width) (rows (stripFinalNewline (expandTabs chars)))
    block Gap = [[]]
    block (Indent prefix content)
      | indent >= width = concatMap (wrapExact width) (attach (render width content))
      | otherwise = attach (render (width-indent) content)
      where
        indent = T.length prefix
        attach [] = [paint Comment (T.stripEnd prefix)]
        attach (first:rest) = (paint Comment prefix ++ first) : map (paint Plain (T.replicate indent " ") ++) rest

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
      let offset = max 1 (columnOffset text width)
          (part,rest) = splitAt offset remaining
          (marks,after) = span (\(c,_) -> displayColumn (T.singleton c) 1 == 0) rest
          row = part ++ marks
      in row : [line | not (null after), line <- go (T.drop (length row) text) after]

expandTabs :: Styled -> Styled
expandTabs = go 0
  where
    go _ [] = []
    go _ (('\n',style):rest) = ('\n',style) : go 0 rest
    go col (('\t',style):rest) = let count = 8-col `mod` 8 in replicate count (' ',style) ++ go (col+count) rest
    go col (char@(c,_):rest) = char : go (col + displayColumn (T.singleton c) 1) rest

codeStyles :: T.Text -> T.Text -> Styled
codeStyles info source = case T.words info of
  language:_ -> case S.lookupSyntax language S.defaultSyntaxMap of
    Just syntax -> case listToMaybe (S.sExtensions syntax) of
      Just pattern' -> highlightFor (T.unpack (T.replace "*" "code" (T.pack pattern'))) source
      Nothing -> plain
    Nothing -> highlightFor ("code." ++ T.unpack language) source
  [] -> plain
  where plain = paint Plain source
