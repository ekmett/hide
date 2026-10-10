{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.ConversationMenus
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- First-party New/Resume declarations use the ordinary menu worker and host
-- form lifetime. The plugin owns labels and scalar validation; the host owns
-- the captured human receipt, provider retirement/acquisition and recovery.
module Hide.ConversationMenus (withConversationMenus) where

import Control.Exception (bracket,evaluate)
import Data.Aeson (Value(Null))
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Plugin.Command
import Hide.Plugin.ConversationSession
import qualified Hide.Plugin.Form as Form
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Sidebar as Sidebar

-- | Scope exact command/menu registrations around the session. Invocation and
-- form validation run on host workers. Resume prefill never enters menu metadata
-- or a readable form. Closing the scope retires retained form actions as well.
withConversationMenus :: Sidebar.Sidebar c r -> Menu.MenuPublisher c r
  -> (c -> Either Text (ConversationTarget receipt))
  -> (ConversationRequest receipt -> r) -> IO a -> IO a
withConversationMenus forms menus capture inject use=withRegistry $ \registry->do
  let command name title run=registerCommand registry (CommandDef name title hidden hidden run)
        >>= either (ioError . userError . show) pure
      target ctx
        | Sidebar.sidebarOrigin forms ctx/=Menu.HumanMenu=Left "Conversation session actions require the human."
        | otherwise=capture ctx
      resumeTarget ctx=do
        captured<-target ctx
        if conversationPrimary captured then Right captured else Left "Switch to Primary to resume a conversation."
  new<-command "hide.agents.new" "New conversation" (\ctx ()->pure $
    either (Left . CommandRejected) (Right . inject . NewConversation . conversationReceipt) (target ctx))
  load<-command "hide.agents.load" "Resume conversation" (\ctx (receipt,raw)->
    if Sidebar.sidebarOrigin forms ctx/=Menu.HumanMenu then pure (Left (CommandRejected "Conversation session actions require the human.")) else do
      let sid=T.strip raw
      if T.null sid then pure (Left (InvalidArguments "Enter a Session ID.")) else do
        let copied=T.copy sid
        _<-evaluate (T.length copied)
        pure (Right (inject (ResumeConversation receipt copied))))
  resume<-command "hide.agents.resume" "Resume session…" (\ctx ()->case resumeTarget ctx of
    Left err->pure (Left (CommandRejected err))
    Right captured->do
      prepared<-Form.prepareForm Form.PrivateForm
        (Form.InputFormSpec "Resume conversation" "Session ID" (conversationResumeId captured) "Resume")
        (Form.formAction registry load (\sid->(conversationReceipt captured,sid)) (\_ reply->pure reply))
      pure (Sidebar.formReply forms <$> prepared))
  let definitions=
        [Menu.MenuDef "hide.agents.resume" "tools" "conversation" 0 "Resume session…" "" False
          (Menu.menuAction registry resume (const (Right ())) (\_ reply->pure reply))
        ,Menu.MenuDef "hide.agents.new" "tools" "conversation" 1 "New conversation" "Ctrl+Shift+N" False
          (Menu.menuAction registry new (const (Right ())) (\_ reply->pure reply))]
      scope definition action=bracket
        (Menu.publishMenu menus definition >>= either (ioError . userError . show) pure)
        (Menu.withdrawMenu menus) (const action)
  foldr scope use definitions
  where hidden=Codec Null (const (Left "Conversation session arguments are host-captured.")) (const Null)
