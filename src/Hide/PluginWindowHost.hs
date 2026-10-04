{-# LANGUAGE OverloadedStrings #-}
-- | Closed prepared-window adoption shared by existing menu/sidebar owners.
-- Workers retain preparation and cancellation; this adapter observes only exact
-- scope/instance metadata and never runs extension callbacks or scans text.
module Hide.PluginWindowHost (adoptWindowUpdate, tickPluginWindows, retireClosedWindow) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (filterM)
import Hide.Buffer (Selection(..),contentLength,contentLineCount)
import qualified Hide.Plugin.Menu as P
import qualified Hide.Plugin.Window as W
import Hide.Model

-- | Adopt only a human publication after its calling owner revalidates the
-- originating command and captured target. Plugin text has no guest grant.
adoptWindowUpdate :: P.MenuOrigin -> W.WindowUpdate -> Desktop -> IO Desktop
adoptWindowUpdate origin update desktop
  | origin/=P.HumanMenu || dialog desktop/=Nothing || questionActive desktop || activeAutocomplete desktop=pure desktop {status="Plugin window publication is protected."}
  | M.size (pluginWindows desktop)>=256 && not present=pure desktop {status="Plugin window budget reached."}
  | otherwise=do
      accepted<-W.admitWindowUpdate present update
      pure $ case accepted of
        Nothing->desktop {status="Plugin window publication expired."}
        Just (reference,prepared)
          | present->desktop {pluginWindows=M.insert reference prepared (pluginWindows desktop),
              windows=map (clamp reference prepared) (windows desktop)}
          | otherwise->addPluginWindow reference prepared desktop
  where present=M.member (W.updateWindowRef update) (pluginWindows desktop)

-- Scalar scope checks are bounded by the 256-view admission limit. Retirement
-- leaves a selectable read-only snapshot; no stale plugin request can revive it.
tickPluginWindows :: Desktop -> IO Desktop
tickPluginWindows desktop=do
  retired<-filterM (fmap not . W.windowRefCurrent) (M.keys (pluginWindows desktop))
  pure desktop {retiredPluginWindows=S.fromList retired}

clamp :: W.WindowRef -> W.PreparedWindow -> Window -> Window
clamp reference prepared window
  | windowContent window/=PluginContent reference=window
  | otherwise=window {selection=Selection (limit (anchor selected)) (limit (caret selected)),
      scrollRow=min (scrollRow window) (max 0 (contentLineCount text-1))}
  where
    selected=selection window
    text=W.preparedWindowText prepared
    limit=max 0 . min (contentLength text)

-- | A close effect is harmless unless the exact view is already absent. The
-- core close operation owns geometry removal; this retires only its capability.
retireClosedWindow :: W.WindowRef -> Desktop -> IO Desktop
retireClosedWindow reference desktop=do
  if M.member reference (pluginWindows desktop) then pure () else W.retireWindowRef reference
  pure desktop
