module THC.Edit.Syntax where
import Data.Char (isAlpha, isAlphaNum, isDigit, isUpper)
import qualified Data.Text as T

data Style = Plain | Keyword | Comment | Literal | Number | Constructor | Pragma deriving (Eq,Show)

-- Stateful scan includes nested comments; styling never changes source text.
highlight :: T.Text -> [(Char,Style)]
highlight = scan . T.unpack
  where
    paint st = map (,st)
    scan [] = []
    scan ('{':'-':'#':xs) = paint Pragma "{-#" ++ block 1 Pragma xs
    scan ('{':'-':xs) = paint Comment "{-" ++ block 1 Comment xs
    scan ('-':'-':xs) | case xs of [] -> True; x:_ -> not (symbol x) =
      let (a,b) = break (=='\n') xs in paint Comment ("--"++a) ++ scan b
    scan ('"':xs) = ('"',Literal) : quoted '"' xs
    scan ('\'':c:'\'':xs) = paint Literal ['\'',c,'\''] ++ scan xs
    scan ('\'':'\\':c:'\'':xs) = paint Literal ['\'','\\',c,'\''] ++ scan xs
    scan (c:cs)
      | isAlpha c || c == '_' =
          let (a,b) = span (\x -> isAlphaNum x || x `elem` "_'") cs
              token = c:a
              style | token `elem` keywords = Keyword
                    | isUpper c = Constructor
                    | otherwise = Plain
          in paint style token ++ scan b
      | isDigit c = let (a,b) = span (\x -> isAlphaNum x || x `elem` "._") cs
                    in paint Number (c:a) ++ scan b
      | otherwise = (c,Plain) : scan cs
    block :: Int -> Style -> String -> [(Char,Style)]
    block _ _ [] = []
    block n st ('{':'-':xs) = paint st "{-" ++ block (n+1) st xs
    block n st ('-':'}':xs) = paint st "-}" ++ (if n == 1 then scan xs else block (n-1) st xs)
    block n st (c:cs) = (c,st) : block n st cs
    quoted _ [] = []
    quoted q ('\\':c:cs) = paint Literal ['\\',c] ++ quoted q cs
    quoted q (c:cs) = (c,Literal) : (if c == q then scan cs else quoted q cs)
    symbol c = c `elem` "!#$%&*+./<=>?@\\^|-~:"
    keywords = words "as case class data default deriving do else family forall foreign hiding if import in infix infixl infixr instance let mdo module newtype of pattern qualified role safe then type unsafe where stock anyclass via"
