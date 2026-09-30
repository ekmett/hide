{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Syntax (Style(..), highlight, highlightFor) where

import Data.List (intercalate)
import qualified Data.Text as T
import qualified Skylighting as S
import System.FilePath (takeFileName)

data Style = Plain | Keyword | Comment | Literal | Number | Constructor | Pragma deriving (Eq,Show)

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
    paint (token,text) = map (,style token) (T.unpack text)
    style token = case token of
      S.KeywordTok -> Keyword; S.ControlFlowTok -> Keyword; S.ImportTok -> Keyword
      S.CommentTok -> Comment; S.DocumentationTok -> Comment; S.AnnotationTok -> Comment; S.CommentVarTok -> Comment
      S.CharTok -> Literal; S.SpecialCharTok -> Literal; S.StringTok -> Literal
      S.VerbatimStringTok -> Literal; S.SpecialStringTok -> Literal
      S.DecValTok -> Number; S.BaseNTok -> Number; S.FloatTok -> Number
      S.DataTypeTok -> Constructor; S.ConstantTok -> Constructor
      S.PreprocessorTok -> Pragma; S.ExtensionTok -> Pragma
      _ -> Plain
