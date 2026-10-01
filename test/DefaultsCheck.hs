{-# LANGUAGE OverloadedStrings #-}
module DefaultsCheck (checks) where
import Control.Monad (unless,forM_)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Text as T
import THC.Edit.Defaults

checks :: IO ()
checks=do
  let check label ok=unless ok (error label)
      parse=parseEither parseDefaults
      failed=either (const True) (const False)
  check "empty defaults preserve every fallback" (parse (object [])==Right (Defaults Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing))
  let chosen=parse (object ["backend" .= ("web"::T.Text),"scale" .= (1.5::Double),"screenMode" .= (259::Int),"columns" .= (101::Int),"rows" .= (37::Int),"appearance" .= ("dark"::T.Text),"wordStar" .= True,"blinkCursor" .= False,"crtFilter" .= True,"pixelateUnicode" .= False,"materialIcons" .= True])
  check "typed startup defaults preserve supplied values" (chosen==Right (Defaults (Just "web") (Just 1.5) (Just 259) (Just 101) (Just 37) (Just "dark") (Just True) (Just False) (Just True) (Just False) (Just True) Nothing))
  forM_ [object ["backend" .= ("unknown"::T.Text)],object ["scale" .= (0::Int)],object ["scale" .= (9::Int)],object ["screenMode" .= (80::Int)],object ["columns" .= (39::Int)],object ["rows" .= (257::Int)],object ["appearance" .= ("blue"::T.Text)],object ["wordStar" .= ("true"::T.Text)],object ["blinkCursor" .= (1::Int)],object ["permission" .= True],toJSON ([]::[Int])] $ \value ->
    check "invalid or misspelled defaults are rejected" (failed (parse value))
  putStrLn "editor defaults checks passed"
