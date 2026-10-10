{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.ConversationMenus
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- First-party conversation declarations use the ordinary menu worker and host
-- form lifetime. The plugin owns labels and scalar validation; the host owns
-- captured human receipts, provider retirement/acquisition, clipboard and recovery.
-- Cancel is a fixed host action: it never queues a delayed plugin callback.
module Hide.ConversationMenus (withConversationMenus) where

import Control.Exception (bracket,evaluate)
import Data.Aeson (Value(Null), ToJSON, encode, eitherDecodeStrict')
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hide.Plugin.Command
import Hide.Plugin.ConversationSession
import Hide.Plugin.Provider (ProviderLaunch(..))
import qualified Hide.Plugin.Form as Form
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Sidebar as Sidebar

-- | Scope exact command/menu registrations around the session. Invocation and
-- form validation run on host workers. Resume prefill never enters menu metadata
-- or a readable form. Closing the scope retires retained form actions as well.
withConversationMenus :: Sidebar.Sidebar c r -> Menu.MenuPublisher c r
  -> (c -> Either Text (ConversationTarget receipt))
  -> (c -> Either Text (ConversationOperationTarget receipt))
  -> (ConversationRequest receipt -> r) -> Menu.MenuAction c r -> IO a -> IO a
withConversationMenus forms menus capture captureOperation inject cancel use=withRegistry $ \registry->do
  let command name title run=registerCommand registry (CommandDef name title hidden hidden run)
        >>= either (ioError . userError . show) pure
      human ctx
        | Sidebar.sidebarOrigin forms ctx/=Menu.HumanMenu=Left "Conversation session actions require the human."
        | otherwise=Right ()
      target ctx=human ctx >> capture ctx
      operation ctx=human ctx >> captureOperation ctx
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
  open<-command "hide.agents.open" "Conversation" (\ctx ()->pure $
    either (Left . CommandRejected) (Right . inject . OpenConversation . operationReceipt) (operation ctx))
  copy<-command "hide.agents.copy" "Copy raw conversation" (\ctx ()->pure $
    either (Left . CommandRejected) (Right . inject . CopyRawConversation . operationReceipt) (operation ctx))
  configure<-command "hide.agents.configure" "Configure provider" (\ctx (receipt,fields)->pure $ do
    either (Left . CommandRejected) Right (human ctx)
    launch<-either (Left . InvalidArguments . T.pack) Right (parseProvider fields)
    Right (inject (ConfigureConversation receipt launch)))
  provider<-command "hide.agents.provider" "Agents…" (\ctx ()->case operation ctx of
    Left err->pure (Left (CommandRejected err))
    Right captured | not (operationConfigurable captured)->pure (Left (CommandRejected "Cancel the current reply before changing providers."))
    Right captured->do
      let launch=operationProvider captured
      prepared<-Form.prepareForm Form.PrivateForm
        (Form.InputsFormSpec "Agents"
          [Form.InputField "executable" "Executable" (T.pack (executable launch))
          ,Form.InputField "arguments" "Arguments (JSON array)" (jsonText (arguments launch))
          ,Form.InputField "environment" "Environment (JSON object)" (jsonText (M.fromList (environment launch)))] "OK")
        (Form.inputsFormAction registry configure (Right . (,) (operationReceipt captured)) (\_ reply->pure reply))
      pure (Sidebar.formReply forms <$> prepared))
  editContext<-command "hide.agents.edit-context" "Edit agent context" (\ctx (receipt,scope)->pure $ do
    either (Left . CommandRejected) Right (human ctx)
    selected<-case scope of
      "global"->Right GlobalContext
      "project"->Right ProjectContext
      _->Left (InvalidArguments "Choose Global or Project context.")
    Right (inject (OpenConversationContext receipt selected)))
  context<-command "hide.agents.context" "Agent Context…" (\ctx ()->case operation ctx of
    Left err->pure (Left (CommandRejected err))
    Right captured->do
      prepared<-Form.prepareForm Form.ReadableForm
        (Form.ChoiceFormSpec "Agent Context"
          "Scope (edit [editor.agent] in TOML)"
          [("global","Global"),("project","Project")] "project" "Edit")
        (Form.formAction registry editContext ((,) (operationReceipt captured)) (\_ reply->pure reply))
      pure (Sidebar.formReply forms <$> prepared))
  let declaration name slot order title key action=Menu.MenuDef name slot "conversation" order title key False action
      invokeAction cmd=Menu.menuAction registry cmd (const (Right ())) (\_ reply->pure reply)
      definitions=
        [declaration "hide.agents.open" "tools" (-2) "Conversation" "Ctrl+Shift+C" (invokeAction open)
        ,declaration "hide.agents.cancel" "tools" (-1) "Cancel reply" "" cancel
        ,declaration "hide.agents.resume" "tools" 0 "Resume session…" "" (invokeAction resume)
        ,declaration "hide.agents.new" "tools" 1 "New conversation" "Ctrl+Shift+N" (invokeAction new)
        ,declaration "hide.agents.copy" "tools" 2 "Copy raw conversation" "" (invokeAction copy)
        ,declaration "hide.agents.provider" "options" 0 "Agents…" "" (invokeAction provider)
        ,declaration "hide.agents.context" "options" 1 "Agent Context…" "" (invokeAction context)]
      scope definition action=bracket
        (Menu.publishMenu menus definition >>= either (ioError . userError . show) pure)
        (Menu.withdrawMenu menus) (const action)
  foldr scope use definitions
  where hidden=Codec Null (const (Left "Conversation session arguments are host-captured.")) (const Null)

-- Parsing belongs to the form worker. Values remain private, and argv is never
-- interpreted by a shell. The form owner checks the exact named field set.
parseProvider :: M.Map Text Text -> Either String ProviderLaunch
parseProvider fields=do
  argv<-eitherDecodeStrict' (TE.encodeUtf8 (fields M.! "arguments"))
  env<-eitherDecodeStrict' (TE.encodeUtf8 (fields M.! "environment"))
  let launch=ProviderLaunch (T.unpack (T.strip (fields M.! "executable"))) argv (M.toList env)
  if null (executable launch) then Left "Enter an executable."
  else if any (elem '\0') (executable launch:arguments launch++concatMap (\(k,v)->[k,v]) (environment launch))
    then Left "NUL bytes are not valid in process arguments."
  else if any (\(key,_)->null key || '=' `elem` key) (environment launch)
    then Left "Invalid environment variable name."
  else Right launch

jsonText :: ToJSON a => a -> Text
jsonText=TE.decodeUtf8 . BL.toStrict . encode
