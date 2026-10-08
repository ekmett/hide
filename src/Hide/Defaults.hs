{-# LANGUAGE OverloadedStrings #-}
-- | Strict parsing of optional startup defaults.
--
-- Absent fields remain Nothing so the caller can apply CLI, environment and
-- configuration precedence. Parsing reuses frontend validators for backend/scale
-- and bounds modes, dimensions and enumerated preferences. It does not select a
-- concrete runtime default or silently accept unknown fields.
module Hide.Defaults (Defaults(..), parseDefaults) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import Hide.BufferView
import Hide.Model (ChatSubmit,parseChatSubmit)
import Hide.Frontend (chooseBackend, chooseScale)

-- | Optional configuration overrides, before runtime precedence is resolved.
data Defaults = Defaults
  { defaultBackend :: Maybe String, defaultScale :: Maybe Double
  , defaultScreenMode :: Maybe Int, defaultColumns :: Maybe Int, defaultRows :: Maybe Int
  , defaultAppearance :: Maybe String, defaultWordStar :: Maybe Bool
  , defaultBlinkCursor :: Maybe Bool, defaultCRT :: Maybe Bool
  , defaultPixelateUnicode :: Maybe Bool, defaultMaterialIcons :: Maybe Bool, defaultStreamerMode :: Maybe Bool, defaultView :: Maybe BufferView, defaultChatSubmit :: Maybe ChatSubmit, defaultWideSectionTitles :: Maybe Bool, defaultMacKeySymbols :: Maybe Bool
  , defaultHapticFeedback :: Maybe Bool
  } deriving (Eq,Show)

-- | Validate a defaults object; omission is not a concrete default value.
parseDefaults :: Value -> Parser Defaults
parseDefaults=withObject "editor.defaults" $ \o -> do
  unless (all (`elem` ["backend","scale","screenMode","columns","rows","appearance","wordStar","blinkCursor","crtFilter","pixelateUnicode","materialIcons","streamerMode","bufferView","chatSubmit","macKeySymbols","wideSectionTitles","hapticFeedback"]) (KM.keys o)) (fail "Unknown editor default")
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
  viewName<-o .:? "bufferView"
  view<-traverse (maybe (fail "bufferView must be current, changes, only-changes, side-by-side or markdown") pure . parseBufferView) viewName
  submitName<-o .:? "chatSubmit"
  submit<-traverse (maybe (fail "chatSubmit must be query or steer") pure . parseChatSubmit) submitName
  Defaults backend scale mode columns rows appearance <$> o .:? "wordStar" <*> o .:? "blinkCursor" <*> o .:? "crtFilter" <*> o .:? "pixelateUnicode" <*> o .:? "materialIcons" <*> o .:? "streamerMode" <*> pure view <*> pure submit <*> o .:? "wideSectionTitles" <*> o .:? "macKeySymbols" <*> o .:? "hapticFeedback"
