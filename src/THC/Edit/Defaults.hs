{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Defaults (Defaults(..), parseDefaults) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import THC.Edit.Frontend (chooseBackend, chooseScale)

data Defaults = Defaults
  { defaultBackend :: Maybe String, defaultScale :: Maybe Double
  , defaultScreenMode :: Maybe Int, defaultColumns :: Maybe Int, defaultRows :: Maybe Int
  , defaultAppearance :: Maybe String, defaultWordStar :: Maybe Bool
  , defaultBlinkCursor :: Maybe Bool, defaultCRT :: Maybe Bool
  , defaultPixelateUnicode :: Maybe Bool, defaultMaterialIcons :: Maybe Bool
  } deriving (Eq,Show)

parseDefaults :: Value -> Parser Defaults
parseDefaults=withObject "editor.defaults" $ \o -> do
  unless (all (`elem` ["backend","scale","screenMode","columns","rows","appearance","wordStar","blinkCursor","crtFilter","pixelateUnicode","materialIcons"]) (KM.keys o)) (fail "Unknown editor default")
  backend<-o .:? "backend"
  either fail (const (pure ())) (chooseBackend backend [])
  scale<-o .:? "scale"
  either fail (const (pure ())) (chooseScale (show <$> scale) [])
  mode<-o .:? "screenMode"
  unless (maybe True (`elem` [3,259]) mode) (fail "screenMode must be 3 or 259")
  columns<-o .:? "columns"
  rows<-o .:? "rows"
  unless (maybe True (\n -> n>=40 && n<=512) columns && maybe True (\n -> n>=12 && n<=256) rows) (fail "Use 40..512 columns and 12..256 rows")
  appearance<-o .:? "appearance"
  unless (maybe True (`elem` ["light","dark","system"]) appearance) (fail "appearance must be light, dark or system")
  Defaults backend scale mode columns rows appearance <$> o .:? "wordStar" <*> o .:? "blinkCursor" <*> o .:? "crtFilter" <*> o .:? "pixelateUnicode" <*> o .:? "materialIcons"
