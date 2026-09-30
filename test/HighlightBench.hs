{-# LANGUAGE OverloadedStrings #-}
-- cabal exec -- ghc -O2 -package thc-edit test/HighlightBench.hs -o /tmp/thc-highlight-bench
-- /tmp/thc-highlight-bench
import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import THC.Edit.Buffer
import THC.Edit.Model
import THC.Edit.Render (snapshot)
import THC.Edit.Syntax

main :: IO ()
main = do
  let source=T.unlines ("module Example where" : concat [["value"<>T.pack (show n)<>" :: Int", "value"<>T.pack (show n)<>" = "<>T.pack (show n)<>" -- comment"] | n<-[1..500::Int]])
      forceTokens text=evaluate (length (filter ((/=Plain) . snd) (highlight text)))
  _ <- forceTokens source
  start <- getCPUTime
  forM_ [1..20::Int] $ \n -> forceTokens (source<>T.pack (show n))
  end <- getCPUTime
  printf "1001-line Haskell tokenization: %.2f ms/edit\n" (fromIntegral (end-start) / 1e9 / 20 :: Double)
  let desktop=fst (runCommand SplitVertical (addDocument Nothing (newBuffer source) (initialDesktop (100,32))))
      states=take 100 (iterate (fst . handleEvent (V.EvKey V.KDown [])) desktop)
  _ <- evaluate (T.length (snapshot desktop))
  drawn <- getCPUTime
  forM_ states $ \d -> evaluate (T.length (snapshot d))
  done <- getCPUTime
  printf "Cached split-view cursor redraw: %.2f ms/frame\n" (fromIntegral (done-drawn) / 1e9 / 100 :: Double)
