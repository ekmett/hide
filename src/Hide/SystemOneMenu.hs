{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.SystemOneMenu
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Human supplier selection through the ordinary menu and form owners. Opening
-- a form captures an offer, not inference authority. Its worker prepares only a
-- closed choice; the UI owner revalidates and adopts that choice on submission.
module Hide.SystemOneMenu (withSystemOneMenu) where

import Control.Exception (bracket)
import Data.Aeson (Value(Null))
import Hide.Plugin.Command
import Hide.Plugin.SystemOne
import qualified Hide.Plugin.Form as Form
import qualified Hide.Plugin.Menu as Menu
import Hide.MenuCommands (MenuHost,menuSidebarCapabilities)
import Hide.SidebarCommands
import Hide.SystemOne
import Hide.SystemOneBrowser

-- | Scope a human-only Tools entry. Advertising a browser does not select it;
-- canceling the form has no effect. Retained forms die with this command scope.
withSystemOneMenu :: MenuHost -> SidebarHost -> SystemOne -> Maybe DecisionProvider
  -> SystemOneBrowser -> IO a -> IO a
withSystemOneMenu menus sidebar owner configured browsers use=withRegistry $ \registry->do
  select<-registerCommand registry (CommandDef "hide.system-one.select" "Select decision supplier" hidden hidden $ \ctx (offer,value)->
    pure $ if sidebarOrigin ctx/=Menu.HumanMenu then Left (CommandRejected "Supplier selection requires the human.") else
      case value of
        "off"->Right (SidebarSystemOne (ConfiguredSystemOne owner Nothing))
        "configured" | Just provider<-configured->Right (SidebarSystemOne (ConfiguredSystemOne owner (Just provider)))
        "browser" | Just captured<-offer->Right (SidebarSystemOne (BrowserSystemOne owner captured))
        _->Left (InvalidArguments "Select an available supplier.")) >>= required
  open<-registerCommand registry (CommandDef "hide.system-one.supplier" "System One supplier…" hidden hidden $ \ctx ()->
    if sidebarOrigin ctx/=Menu.HumanMenu then pure (Left (CommandRejected "Supplier selection requires the human.")) else do
      offer<-captureBrowserOffer browsers
      current<-currentDecisionSupplier (systemOneServices owner)
      let choices=[("off","Off")]
            ++[("configured",supplierLabel (decisionProviderDescription provider)) | Just provider<-[configured]]
            ++[("browser",browserOfferLabel browser<>" (this browser)") | Just browser<-[offer]]
          selected=case fmap (supplierLocation . decisionSupplierDescription) current of
            Just BrowserWorker{} | Just _<-offer->"browser"
            Just _ | Just _<-configured->"configured"
            _->"off"
      prepared<-Form.prepareForm Form.ReadableForm
        (Form.ChoiceFormSpec "System One supplier" "Only the selected supplier receives decision context." choices selected "Select")
        (Form.formAction registry select ((,) offer) (\_ reply->pure reply))
      pure (SidebarForm <$> prepared)) >>= required
  let publisher=menuSidebarCapabilities menus sidebar
      definition=Menu.MenuDef "hide.system-one.supplier" "tools" "inference" 0 "System One supplier…" "" False
        (Menu.menuAction registry open (const (Right ())) (\_ reply->pure reply))
  bracket (Menu.publishMenu publisher definition >>= required) (Menu.withdrawMenu publisher) (const use)
  where
    hidden=Codec Null (const (Left "Supplier choices are captured by the human form.")) (const Null)
    required :: Show e => Either e a -> IO a
    required=either (ioError . userError . show) pure
