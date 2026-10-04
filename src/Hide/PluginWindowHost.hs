{-# LANGUAGE OverloadedStrings #-}
-- | Closed prepared-window adoption shared by existing menu/sidebar owners.
-- Workers retain preparation and cancellation; this adapter observes only exact
-- scope/instance metadata and never runs extension callbacks or scans text.
module Hide.PluginWindowHost (adoptWindowUpdate) where

import qualified Data.Map.Strict as M
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
          | present->desktop {pluginWindows=M.insert reference prepared (pluginWindows desktop)}
          | otherwise->addPluginWindow reference prepared desktop
  where present=M.member (W.updateWindowRef update) (pluginWindows desktop)
