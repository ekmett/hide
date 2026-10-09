-- |
-- Module      : BufferViewCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : GHC2021

module BufferViewCheck (checks) where

import Control.Monad (unless,forM_)
import Hide.BufferView

checks :: IO ()
checks=do
  let check name ok=unless ok (error name)
      p=buildViewProjection 30 [(5,2,3),(20,3,1)]
      both n=ViewRow (Just n) (Just n) 0
  check "context view folds unchanged regions with two context lines"
    (map (viewRowAt OnlyChangesView p) [0..18]==
      [ViewRow Nothing Nothing 3]++map both [3..11]++[ViewRow Nothing Nothing 6]++map both [18..25])
  check "context keeps trailing omission" (viewRowAt OnlyChangesView p 19==ViewRow Nothing Nothing 4)
  check "side by side aligns unequal hunks"
    (map (viewRowAt SideBySideView p) [5..7]==
      [ViewRow (Just 5) (Just 7) 0,ViewRow (Just 6) (Just 8) 0,ViewRow Nothing (Just 9) 0])
  check "side by side deletion padding"
    (map (viewRowAt SideBySideView p) [18..20]==
      [ViewRow (Just 20) (Just 23) 0,ViewRow (Just 21) Nothing 0,ViewRow (Just 22) Nothing 0])
  check "only changes copy excludes hidden rows"
    (viewChangeRanges OnlyChangesView p UnifiedSide==[(3,12),(18,26)])
  check "adjacent context ranges merge"
    (viewChangeRanges OnlyChangesView (buildViewProjection 15 [(3,1,1),(7,1,1)]) UnifiedSide==[(1,11)])
  check "unchanged buffer has empty focused diff" (viewRowCount OnlyChangesView (buildViewProjection 100 [])==0)
  forM_ [ChangesView,OnlyChangesView,SideBySideView] $ \mode ->
    forM_ [0..viewRowCount mode p-1] $ \row -> do
      let entry=viewRowAt mode p row
      forM_ (viewLeftRow entry) $ \full -> check "left projection roundtrip" (viewRowForChange mode p OriginalSide full==row)
      forM_ (viewRightRow entry) $ \full -> check "right projection roundtrip" (viewRowForChange mode p CurrentSide full==row)
  putStrLn "buffer view checks passed"
