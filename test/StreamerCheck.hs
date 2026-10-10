{-# LANGUAGE OverloadedStrings #-}
module StreamerCheck (checks) where
import Control.Monad (unless)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EditorFixture (agentSettingsReply)
import Hide.Conversation
import qualified Hide.Consoles as C
import Hide.Model
import Hide.Render (snapshot)

checks :: IO ()
checks=do
  let check label good=unless good (error label)
      values=[Input "Executable" "provider-program" 0,Input "Environment (JSON object)" "PRIVATE_TOKEN_VALUE" 0]
      original=(initialDesktop (80,25)) {dialog=Just (Dialog "Agents" (AgentDialog "configure") values 0 ["OK","Cancel"] [])}
      hidden=original {streamerMode=True}
  check "streamer off displays actual values" ("PRIVATE_TOKEN_VALUE" `T.isInfixOf` snapshot original)
  check "streamer hides secret values but retains public settings" (not ("PRIVATE_TOKEN_VALUE" `T.isInfixOf` snapshot hidden) && "provider-program" `T.isInfixOf` snapshot hidden)
  check "redaction leaves stored settings intact" (dialog hidden==dialog original)
  check "disabling streamer restores visible values" (snapshot (hidden {streamerMode=False})==snapshot original)
  let statusOnly=(initialDesktop (80,25)) {status="Session private-session-key",streamerMode=True}
  check "streamer mode covers session status" (not ("private-session-key" `T.isInfixOf` snapshot statusOnly))
  retained<-C.withConsoles $ \consoles -> withConversation Nothing Nothing Nothing consoles $ \runtime -> do
    let state=(initialDesktop (80,25)) {agentSettings=[AgentSetting "model" "Model" "model" "model-public" [("model-public","Public model")],AgentSetting "token" "API token" "private" "private-value" [("private-value","private-choice")]]}
    finish<-agentSettingsReply runtime state
    result<-finish
    let text=TE.decodeUtf8 (BL.toStrict (encode result))
    check "public agent settings readable and secrets redacted" ("model-public" `T.isInfixOf` text && not ("private-value" `T.isInfixOf` text) && not ("private-choice" `T.isInfixOf` text))
    agentSettingsReply runtime state
  retired<-retained
  check "retired conversation settings cannot read configuration" (case retired of Left _->True; Right _->False)
  putStrLn "streamer and public agent settings checks passed"
