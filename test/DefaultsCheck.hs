{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : DefaultsCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module DefaultsCheck (checks) where
import Control.Monad (unless,forM_)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Text as T
import Hide.BufferView
import Hide.Defaults
import Hide.Model (ChatSubmit(..))
import qualified Hide.Model as Model

checks :: IO ()
checks=do
  let check label ok=unless ok (error label)
      parse=parseEither parseDefaults
      failed=either (const True) (const False)
  check "empty defaults preserve every fallback" (parse (object [])==Right (Defaults Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing))
  let chosen=parse (object ["backend" .= ("web"::T.Text),"scale" .= (1.5::Double),"screenMode" .= (259::Int),"columns" .= (101::Int),"rows" .= (37::Int),"appearance" .= ("dark"::T.Text),"wordStar" .= True,"blinkCursor" .= False,"crtFilter" .= True,"pixelateUnicode" .= False,"materialIcons" .= True])
  check "typed startup defaults preserve supplied values" (chosen==Right (Defaults (Just "web") (Just 1.5) (Just 259) (Just 101) (Just 37) (Just "dark") (Just True) (Just False) (Just True) (Just False) (Just True) Nothing Nothing Nothing Nothing Nothing Nothing))
  check "wide titles default is typed" (fmap defaultWideSectionTitles (parse (object ["wideSectionTitles" .= True]))==Right (Just True))
  check "wide titles reject non-Boolean values" (failed (parse (object ["wideSectionTitles" .= ("true"::T.Text)])))
  check "haptic feedback starts disabled" (not (Model.hapticFeedback (Model.initialDesktop (80,25))))
  forM_ [False,True] $ \enabled ->
    check "haptic feedback default is Boolean" (fmap defaultHapticFeedback (parse (object ["hapticFeedback" .= enabled]))==Right (Just enabled))
  check "haptic feedback rejects non-Boolean values" (failed (parse (object ["hapticFeedback" .= ("true"::T.Text)])))
  check "Mac key symbols default is typed" (fmap defaultMacKeySymbols (parse (object ["macKeySymbols" .= True]))==Right (Just True))
  check "buffer view default is typed" (fmap defaultView (parse (object ["bufferView" .= ("only-changes"::T.Text)]))==Right (Just OnlyChangesView))
  check "invalid buffer view is rejected" (failed (parse (object ["bufferView" .= ("original"::T.Text)])))
  forM_ [("query",QuerySubmit),("steer",SteerSubmit)] $ \(name,action) ->
    check "chat submit default is typed" (fmap defaultChatSubmit (parse (object ["chatSubmit" .= (name::T.Text)]))==Right (Just action))
  forM_ [String "send",String "Steer",Bool True,Number 1] $ \value ->
    check "invalid chat action is rejected" (failed (parse (object ["chatSubmit" .= value])))
  forM_ [object ["backend" .= ("unknown"::T.Text)],object ["scale" .= (0::Int)],object ["scale" .= (9::Int)],object ["screenMode" .= (80::Int)],object ["columns" .= (39::Int)],object ["rows" .= (257::Int)],object ["appearance" .= ("blue"::T.Text)],object ["wordStar" .= ("true"::T.Text)],object ["blinkCursor" .= (1::Int)],object ["permission" .= True],toJSON ([]::[Int])] $ \value ->
    check "invalid or misspelled defaults are rejected" (failed (parse value))
  putStrLn "editor defaults checks passed"
