{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.WardenMenu
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Human-only Warden settings and reviewed advice. Mode choices retain their
-- captured settings epoch; advice prepares an unsent draft on the menu worker.
-- Global TOML supplies startup defaults. Menu changes apply to this session.
module Hide.WardenMenu (parseWardenSettings,withWardenMenu) where

import Control.Exception (bracket,evaluate)
import Control.Monad (unless)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Buffer (newBuffer,prepareBuffer)
import Hide.Plugin.Command
import qualified Hide.Plugin.Form as Form
import qualified Hide.Plugin.Menu as Menu
import Hide.MenuCommands (MenuHost,menuSidebarCapabilities)
import Hide.SidebarCommands
import Hide.Warden
import Hide.WardenRuntime

-- | Only global settings select supervision. Project text cannot disable it or
-- choose a different inference destination; the common supplier stays separate.
parseWardenSettings :: Value -> Either Text WardenSettings
parseWardenSettings value=either (Left . T.pack) Right (parseEither parser value)
  where
    parser=withObject "Warden" $ \o->do
      unless (all (`elem` ["mode","budgetMs","threshold"]) (KM.keys o)) (fail "Unknown Warden setting.")
      mode<-o .:? "mode" .!= ("off"::Text) >>= \name->case name of
        "off"->pure WardenOff
        "observe"->pure WardenObserve
        "enforce"->pure WardenEnforce
        _->fail "Warden mode must be off, observe or enforce."
      budget<-o .:? "budgetMs" .!= wardenBudgetMs defaultWardenSettings
      threshold<-o .:? "threshold" .!= wardenThreshold defaultWardenSettings
      unless (budget>=1 && budget<=30000) (fail "Warden budget must be 1..30000 ms.")
      unless (not (isNaN threshold || isInfinite threshold) && threshold>=0 && threshold<=1) (fail "Invalid Warden probability threshold.")
      pure (WardenSettings mode budget threshold)

-- | Scope both human declarations to the existing menu/form owners. Mode
-- controls load no supplier. Explicit advice review prepares an unsent draft
-- from the captured conversation receipt; its owner validates adoption.
withWardenMenu :: MenuHost -> SidebarHost -> WardenRuntime -> IO a -> IO a
withWardenMenu menus sidebar owner use=withRegistry $ \registry->do
  select<-registerCommand registry (CommandDef "hide.warden.select" "Set ACP Warden mode" hidden hidden $ \ctx (reference,name)->
    pure $ if sidebarOrigin ctx/=Menu.HumanMenu then Left (CommandRejected "Warden settings require the human.") else
      case name of
        "off"->Right (SidebarWarden reference WardenOff)
        "observe"->Right (SidebarWarden reference WardenObserve)
        "enforce"->Right (SidebarWarden reference WardenEnforce)
        _->Left (InvalidArguments "Select a Warden mode.")) >>= required
  open<-registerCommand registry (CommandDef "hide.warden.mode" "ACP Warden…" hidden hidden $ \ctx ()->
    if sidebarOrigin ctx/=Menu.HumanMenu then pure (Left (CommandRejected "Warden settings require the human.")) else do
      (reference,current)<-captureWardenSettings owner
      let selected=case wardenMode current of WardenOff->"off"; WardenObserve->"observe"; WardenEnforce->"enforce"
      prepared<-Form.prepareForm Form.ReadableForm
        (Form.ChoiceFormSpec "ACP Warden" "This session. Uses the selected System One supplier."
          [("off","Off"),("observe","Observe actions"),("enforce","Hold actions that fail judgment")] selected "Select")
        (Form.formAction registry select ((,) reference) (\_ reply->pure reply))
      pure (SidebarForm <$> prepared)) >>= required
  review<-registerCommand registry (CommandDef "hide.warden.advice" "Review Warden advice" hidden hidden $ \ctx ()->
    if sidebarOrigin ctx/=Menu.HumanMenu then pure (Left (CommandRejected "Reviewing Warden advice requires the human.")) else
      case sidebarWardenAdvice ctx of
        Left err->pure (Left (CommandRejected err))
        Right (target,binding)->do
          advice<-prepareWardenAdvice binding
          case advice of
            Left err->pure (Left (CommandRejected err))
            Right receipt->do
              let prepared=newBuffer (wardenAdviceText receipt)
              _<-evaluate (prepareBuffer prepared)
              pure (Right (SidebarWardenAdvice target receipt prepared))) >>= required
  let publisher=menuSidebarCapabilities menus sidebar
      definition=Menu.MenuDef "hide.warden.mode" "options" "agents" 0 "ACP Warden…" "" False
        (Menu.menuAction registry open (const (Right ())) (\_ reply->pure reply))
      adviceDefinition=Menu.MenuDef "hide.warden.advice" "tools" "agents" 8 "Review Warden advice" "" False
        (Menu.menuAction registry review (const (Right ())) (\_ reply->pure reply))
  bracket (Menu.publishMenu publisher definition >>= required) (Menu.withdrawMenu publisher) $ \_->
    bracket (Menu.publishMenu publisher adviceDefinition >>= required) (Menu.withdrawMenu publisher) (const use)
  where
    hidden=Codec Null (const (Left "Host-captured human Warden action only.")) (const Null)
    required :: Show e => Either e a -> IO a
    required=either (ioError . userError . show) pure
