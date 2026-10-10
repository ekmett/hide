{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.AgentConfigurationForms
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Advertised model/effort preparations shared by sidebar and conversation
-- choices. Actions retain their original receipt; presentation never recaptures
-- whichever provider later occupies the selected view.
module Hide.AgentConfigurationForms (captureChoices,prepareAgentChoice,choiceSpec) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Plugin.Agent
import Hide.Plugin.AgentDirectory
import Hide.Plugin.Command
import qualified Hide.Plugin.Form as Form

-- | Copy the already public finite model/effort metadata on its calling worker.
-- Forcing the small receipt and copied catalogue detaches lazy provider selectors.
captureChoices :: AgentDirectory settings completion -> AgentId -> IO (Either Text (settings,[ConfigChoice]))
captureChoices directory who=do
  captured<-directorySettings directory who
  case captured of
    Left err->pure (Left err)
    Right (receipt,choices)->do
      ready<-evaluate receipt
      let copied=[ConfigChoice (T.copy (configId choice)) (T.copy (configCategory choice)) (T.copy (configCurrent choice))
            [(T.copy ident,T.copy label) | (ident,label)<-configValues choice]
            | choice<-choices,configCategory choice `elem` ["model","thought_level"]]
      _<-evaluate (force [(configId choice,configCategory choice,configCurrent choice,configValues choice) | choice<-copied])
      pure (Right (ready,copied))

-- | Shared finite-choice validation and scalar label sanitization. IDs retain
-- provider values exactly; a missing initial value selects the first advertised ID.
choiceSpec :: Text -> [(Text,Text)] -> Text -> Form.FormSpec
choiceSpec title options current=Form.ChoiceFormSpec title "Provider choices"
  [(ident,T.take 256 (T.map (\c->if c<' ' || c=='\DEL' then ' ' else c) label)) | (ident,label)<-options]
  (if current `elem` map fst options then current else maybe "" fst (listToMaybe options)) "Apply"

-- | Prepare the same captured configuration action for a modal or a popup.
-- Popup labels retain the existing checkmark and indentation convention.
prepareAgentChoice :: Bool -> Registry c -> Command c (settings,Text,Text) r
  -> settings -> ConfigChoice -> IO (Either CommandError (Form.PreparedForm c r))
prepareAgentChoice checked registry command receipt choice=Form.prepareForm Form.ReadableForm
  (choiceSpec "Agent setting" labels (configCurrent choice))
  (Form.formAction registry command (\value->(receipt,configId choice,value)) (\_ reply->pure reply))
  where labels=[(ident,if checked then (if ident==configCurrent choice then "✓ " else "  ")<>label else label)
               | (ident,label)<-configValues choice]
