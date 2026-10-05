{-# LANGUAGE OverloadedStrings #-}
-- | Closed prepared-window adoption shared by existing menu/sidebar owners.
-- Workers retain preparation and cancellation; this adapter observes only exact
-- scope/instance metadata and never runs extension callbacks or scans text.
module Hide.PluginWindowHost (adoptWindowUpdate, replaceWindowUpdate, tickPluginWindows, retireClosedWindow) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (filterM)
import Hide.Buffer (Selection(..),contentLength,contentLineCount)
import qualified Hide.Plugin.Menu as P
import qualified Hide.Plugin.Window as W
import Hide.Model

-- | Adopt only a human publication after its calling owner revalidates the
-- originating command and captured target. Opening remains modal-protected; an
-- exact installed refresh changes only its content/local geometry, never focus
-- or modal input. Plugin text has no guest grant.
adoptWindowUpdate :: P.MenuOrigin -> W.WindowUpdate -> Desktop -> IO Desktop
adoptWindowUpdate origin update desktop
  | origin/=P.HumanMenu || not present && (dialog desktop/=Nothing || questionActive desktop || activeAutocomplete desktop)=pure desktop {status="Plugin window publication is protected."}
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

-- | Replace an owner-held output slot with a fresh content lifetime. Exact live
-- old identity and a fresh admitted opening are required; labels are irrelevant.
-- Installed geometry/numbering/focus and unrelated modal input are preserved.
-- Absence/retirement follows ordinary protected opening, never resurrecting old
-- content. Old queued refreshes cannot update the fresh replacement.
replaceWindowUpdate :: P.MenuOrigin -> W.WindowRef -> W.WindowUpdate -> Desktop -> IO Desktop
replaceWindowUpdate origin old update desktop
  | origin/=P.HumanMenu=pure desktop {status="Plugin window publication is protected."}
  | W.updateWindowRef update==old || M.member (W.updateWindowRef update) (pluginWindows desktop)=
      pure desktop {status="Plugin window replacement requires a fresh instance."}
  | not (M.member old (pluginWindows desktop)) || not (any ((==PluginContent old) . windowContent) (windows desktop))=
      adoptWindowUpdate origin update desktop
  | otherwise=do
      live<-W.windowRefCurrent old
      if not live then adoptWindowUpdate origin update desktop else do
        accepted<-W.admitWindowUpdate False update
        case accepted of
          Nothing->pure desktop {status="Plugin window publication expired."}
          Just (reference,prepared)->do
            W.retireWindowRef old
            pure desktop {pluginWindows=M.insert reference prepared (M.delete old (pluginWindows desktop)),
              retiredPluginWindows=S.delete old (retiredPluginWindows desktop),windows=map (replace reference) (windows desktop)}
  where
    replace reference w | windowContent w==PluginContent old=w {windowContent=PluginContent reference,
      selection=Selection 0 0,scrollRow=0,scrollColumn=0}
    replace _ w=w

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
