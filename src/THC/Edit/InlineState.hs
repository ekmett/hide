{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.InlineState where
import Control.Monad (unless)
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Vector as V
import THC.Edit.Buffer
import THC.Edit.InlineTypes

-- Only bounded proposal rows live here. Source trees/history remain in Buffer.
data InlineOption = InlineOption
  { optionProposal :: Proposal
  -- Matched prefix removed from the original normalized insertText. Add this
  -- and already accepted text when reporting cumulative partial acceptance.
  , optionPrefixLength :: Int, optionFirstRow :: Int, optionLastRow :: Int
  , optionRows :: V.Vector [(T.Text,Bool)] } deriving (Eq,Show)
data InlineView = InlineView
  { inlineWindow :: Int, inlineBuffer :: Int, inlineRevision :: Int
  , inlineSelection :: Selection, inlineGeneration :: Int
  , inlineOptions :: [InlineOption], inlineIndex :: Int } deriving (Eq,Show)

selectedOption :: InlineView -> Maybe InlineOption
selectedOption v
  | inlineIndex v<0=Nothing
  | otherwise=case drop (inlineIndex v) (inlineOptions v) of a:_->Just a; _->Nothing

prepareOption :: Buffer -> Proposal -> Either T.Text InlineOption
prepareOption b input=do
  let a=proposalStart input; z=proposalEnd input; replacement=T.replace "\r\n" "\n" (proposalText input)
  unless (a>=0 && z>=a && z<=bufferLength b && z-a<=131072 && T.length replacement<=131072 && not (T.any (=='\0') replacement)) (Left "Completion range or text is invalid.")
  let old=bufferSlice b a (z-a)
      prefix=maybe 0 (T.length . (\(x,_,_)->x)) (T.commonPrefixes old replacement)
      oldTail=T.drop prefix old; newTail=T.drop prefix replacement
      suffix=maybe 0 (T.length . (\(x,_,_)->x)) (T.commonPrefixes (T.reverse oldTail) (T.reverse newTail))
      p=input {proposalStart=a+prefix,proposalEnd=z-suffix,proposalText=T.take (T.length newTail-suffix) newTail}
      (first,col)=bufferLineColumn b (proposalStart p)
      (lastRow,endCol)=bufferLineColumn b (proposalEnd p)
      before=T.take col (bufferLineAt b first)
      after=T.drop endCol (bufferLineAt b lastRow)
      parts=T.splitOn "\n" (proposalText p)
      rows=case parts of
        []->[[(before,False),(after,False)]]
        [one]->[[(before,False),(one,True),(after,False)]]
        firstPart:rest->[(before,False),(firstPart,True)]:
          [[(line,True)] | line<-init rest]++[[(last rest,True),(after,False)]]
  unless (proposalStart p/=proposalEnd p || not (T.null (proposalText p))) (Left "Completion makes no change.")
  pure (InlineOption p prefix first lastRow (V.fromList rows))

-- Accept a lexical word and its leading whitespace. A replacement consumes
-- the corresponding original word; remaining replacement text stays proposed.
proposalWord :: Buffer -> Proposal -> (Int,Int,T.Text,Maybe Proposal)
proposalWord b p
  | T.null (proposalText p)=(a,z,"",Nothing)
  | otherwise=let n=nextWord (proposalText p)
                  taken=T.take n (proposalText p)
                  end=if a==z then z else min z (a+nextWord (bufferSlice b a (z-a)))
                  rest=T.drop n (proposalText p)
                  next=if T.null rest then Nothing else Just p {proposalStart=a+n,proposalEnd=z+n-(end-a),proposalText=rest}
              in (a,if T.null rest then z else end,taken,next)
  where a=proposalStart p; z=proposalEnd p

-- Keep a leading indentation/newline with its next lexical word. Punctuation
-- advances by a grapheme, so emoji sequences and combining marks stay intact.
nextWord :: T.Text -> Int
nextWord text=T.length spaces+case T.uncons rest of
  Nothing->0
  Just (c,_) | wordChar c -> T.length (T.takeWhile (\x->wordChar x || combining x) rest)
             | otherwise -> nextCharacter rest 0
  where (spaces,rest)=T.span isSpace text
