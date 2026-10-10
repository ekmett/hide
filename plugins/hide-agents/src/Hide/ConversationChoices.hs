{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.ConversationChoices
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- First-party conversation dropdowns retain one advertised configuration across
-- category and value selection. The ordinary menu worker prepares forms; their
-- host owns popup geometry, input provenance and exact submission lifetimes.
module Hide.ConversationChoices (withConversationChoices) where

import Control.Exception (bracket)
import Data.Aeson (Value(Null))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Plugin.Agent
import Hide.Plugin.AgentDirectory
import Hide.Plugin.Command
import qualified Hide.Plugin.Form as Form
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Sidebar as Sidebar
import Hide.AgentConfigurationForms

-- | Scope the title-bar/Tools contribution and both finite-choice actions.
-- The original receipt and choices survive the submenu unchanged. Every action
-- independently requires the host's human origin, including both form levels.
withConversationChoices :: Sidebar.Sidebar c r -> Menu.MenuPublisher c r
  -> AgentDirectory settings completion -> (c -> Either Text AgentId)
  -> (DirectoryRequest settings completion -> r) -> IO a -> IO a
withConversationChoices forms menus directory selected inject use=withRegistry $ \registry->do
  let command name title handler=registerCommand registry (CommandDef name title hidden hidden handler)
        >>= either (ioError . userError . show) pure
      human ctx=Sidebar.sidebarOrigin forms ctx==Menu.HumanMenu
      denied=pure (Left (CommandRejected "Conversation settings require the human."))
  apply<-command "hide.agents.configure" "Apply conversation setting" (\ctx (receipt,option,value)->
    if human ctx then pure (Right (inject (ConfigureAgent receipt option value))) else denied)
  choose<-command "hide.agents.choose-setting" "Conversation setting" (\ctx (receipt,choices,ident)->
    if not (human ctx) then denied else case find ((==ident).configId) choices of
      Nothing->pure (Left (InvalidArguments "This conversation setting is unavailable."))
      Just choice->fmap (Sidebar.popupFormReply forms) <$> prepareAgentChoice True registry apply receipt choice)
  open<-command "hide.agents.model" "Conversation model…" (\ctx ()->
    if not (human ctx) then denied else case selected ctx of
      Left err->pure (Left (CommandRejected err))
      Right who->do
        captured<-captureChoices directory who
        case captured of
          Left err->pure (Left (CommandRejected err))
          Right (_,[])->pure (Left (CommandRejected "The provider has not advertised model settings."))
          Right (receipt,choices@(first:_))->do
            let labels=[(configId choice,(if configCategory choice=="model" then "Model" else "Reasoning effort")<>"  "<>T.take 128 (configCurrent choice)<>" ►") | choice<-choices]
            prepared<-Form.prepareForm Form.ReadableForm (choiceSpec "Conversation setting" labels (configId first))
              (Form.formAction registry choose (\ident->(receipt,choices,ident)) (\_ reply->pure reply))
            pure (Sidebar.popupFormReply forms <$> prepared))
  bracket (Menu.publishMenu menus (Menu.MenuDef "hide.agents.model" "tools" "conversation" (-1)
      "Conversation model…" "" False (Menu.menuAction registry open (const (Right ())) (\_ reply->pure reply)))
      >>= either (ioError . userError . show) pure)
    (Menu.withdrawMenu menus) (const use)
  where hidden=Codec Null (const (Left "Conversation choices are host-captured.")) (const Null)
