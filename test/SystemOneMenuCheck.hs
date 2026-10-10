{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : SystemOneMenuCheck
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
module SystemOneMenuCheck (checks) where

import Control.Concurrent (yield)
import Control.Monad (unless)
import qualified Data.Text as T
import System.Timeout (timeout)
import Hide.DocumentationHost (withDocsCommands)
import Hide.GuestAccess (guestKeyboardAllowed)
import Hide.MenuCommands
import Hide.Model hiding (menus)
import qualified Hide.Plugin.Menu as Menu
import Hide.Plugin.SystemOne
import Hide.SidebarCommands
import Hide.SystemOne
import Hide.SystemOneBrowser
import Hide.SystemOneMenu
import Hide.Warden
import Hide.WardenMenu
import Hide.WardenRuntime

checks :: IO ()
checks=withSystemOne $ \owner->withSystemOneBrowser $ \browsers->
  withSidebarCommands $ \sidebar->withDocsCommands $ \docs->withMenuCommands docs $ \menus->
  withSystemOneMenu menus sidebar owner (Just provider) browsers $
  withWarden (systemOneServices owner) defaultWardenSettings (const (pure (Right []))) $ \warden->
  withWardenMenu menus sidebar warden $ do
    metadata<-Menu.menuSnapshot (menuContributions menus)
    entry<-case filter ((=="hide.system-one.supplier") . Menu.menuName . Menu.menuReference) metadata of
      [value]->pure value
      _->fail "Missing supplier menu"
    let reference=Menu.menuReference entry
        initial=(initialDesktop (100,40)) {contributedMenus=metadata,menusActive=True}
        services=systemOneServices owner
        fallback _ _=fail "Unexpected effect from supplier menu"
        core=sidebarEffects sidebar fallback
        tick desktop=tickMenus menus core desktop >>= tickSidebar sidebar core
        open desktop=snd <$> menuEffects menus core desktop [InvokeMenu reference Menu.HumanMenu Nothing]
        formReady desktop=pure $ case dialog desktop of
          Just dg | PluginChoiceForm{}<-purpose dg->True
          _->False
        form desktop=case dialog desktop of
          Just dg | PluginChoiceForm ref revision<-purpose dg->(ref,revision)
          _->error "Missing supplier form"
        selected=maybe False (const True) <$> currentDecisionSupplier services
    (_,denied)<-menuEffects menus core initial [InvokeMenu reference Menu.AgentMenu Nothing]
    check "agents cannot open supplier controls" (dialog denied==Nothing)
    check "denied selection leaves supplier disabled" . not =<< selected
    offered<-open initial >>= await tick formReady
    check "supplier form is human controlled" (not (guestKeyboardAllowed offered))
    check "opening choices neither selects nor loads a provider" . not =<< selected
    let (old,revision)=form offered
    (_,cancelled)<-core offered {dialog=Nothing} [RetireInputForm old]
    (_,replayed)<-core cancelled [SubmitChoiceForm old revision 1 Menu.HumanMenu]
    settled<-tick replayed
    check "cancelled supplier form cannot select through a retained reference" . not =<< selected
    fresh<-open settled >>= await tick formReady
    let (current,currentRevision)=form fresh
    (_,pending)<-core fresh [SubmitChoiceForm current currentRevision 1 Menu.HumanMenu]
    chosen<-await tick (const selected) pending
    supplier<-currentDecisionSupplier services
    check "exact submitted form selects configured supplier" (fmap (supplierLabel . decisionSupplierDescription) supplier==Just "Configured fixture")
    off<-open chosen >>= await tick formReady
    let (offRef,offRevision)=form off
    (_,stopping)<-core off [SubmitChoiceForm offRef offRevision 0 Menu.HumanMenu]
    finished<-await tick (const (not <$> selected)) stopping
    wardenReference<-case filter ((=="hide.warden.mode") . Menu.menuName . Menu.menuReference) metadata of
      [value]->pure (Menu.menuReference value)
      _->fail "Missing Warden menu"
    (_,wardenDenied)<-menuEffects menus core finished [InvokeMenu wardenReference Menu.AgentMenu Nothing]
    check "agents cannot open Warden controls" (dialog wardenDenied==Nothing)
    (_,wardenOpening)<-menuEffects menus core wardenDenied [InvokeMenu wardenReference Menu.HumanMenu Nothing]
    wardenForm<-await tick formReady wardenOpening
    check "Warden mode is human controlled" (not (guestKeyboardAllowed wardenForm))
    let (wardenRef,wardenRevision)=form wardenForm
    (_,wardenPending)<-core wardenForm [SubmitChoiceForm wardenRef wardenRevision 1 Menu.HumanMenu]
    _<-await tick (const ((==WardenObserve) . wardenMode <$> getWardenSettings warden)) wardenPending
    pure ()
  where
    -- Selection must not acquire weights or start a network request.
    provider=DecisionProvider (SupplierDescription "Configured fixture" InProcess (ReportedModel "fixture") Nothing 0)
      (\_ _->fail "Opening or selecting the supplier acquired its driver")

await :: (Desktop -> IO Desktop) -> (Desktop -> IO Bool) -> Desktop -> IO Desktop
await tick ready initial=timeout 5000000 (go initial) >>= maybe (fail "Supplier form did not complete") pure
  where
    go desktop=do
      next<-tick desktop
      done<-ready next
      if done then pure next else
        if "failed" `T.isInfixOf` status next then fail (T.unpack (status next))
        else yield >> go next

check :: String -> Bool -> IO ()
check label condition=unless condition (fail label)
