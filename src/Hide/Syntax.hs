{-# LANGUAGE OverloadedStrings #-}
module Hide.Syntax (Style(..), highlight, highlightFor, bubbleTile, linkSpans) where

import Data.List (intercalate)
import qualified Data.List as List
import Data.Char (chr)
import Data.Word (Word32)
import qualified Data.Text as T
import qualified Skylighting as S
import System.FilePath (takeFileName)

data Style = LinkStyle T.Text Style | Plain | Heading Int | CodeStyle Bool Style | ProseStyle Style | Keyword | Comment | Literal | Number | Constructor | Pragma | BubbleStyle Bool Style | BubbleText Int Bool Style | TerminalStyle Word32 Word32 Word32 deriving (Eq,Show)

highlight :: T.Text -> [(Char,Style)]
highlight = highlightFor "Main.hs"

-- Language rules come entirely from Skylighting's maintained KDE definitions.
-- Only their token categories are mapped to the editor's palette here.
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

-- Top-left/right, bottom-left/right, single-row caps, left/right tails.
bubbleTile :: Bool -> Int -> Char
bubbleTile graphical n
  | n<0 || n>7 = ' '
  | graphical = chr (0xe000+n)
  | otherwise = "▟▙▜▛▐▌◥◤" !! n

-- Metadata is collected by Markdown/conversation layout, never by the renderer.
linkSpans :: [(Char,Style)] -> [(Int,Int,T.Text)]
linkSpans = reverse . snd . List.foldl' collect (0,[])
  where
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
