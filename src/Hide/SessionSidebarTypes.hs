{-# LANGUAGE OverloadedStrings #-}
-- | Module      : Hide.SessionSidebarTypes
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Closed human view selection. Editor session identity is distinct from private
-- provider keys; no launch arguments, document or callback crosses this boundary.
module Hide.SessionSidebarTypes (SessionSidebarRequest(..)) where

import Data.Text (Text)

-- | Select the captured live window in this exact editor session. The session
-- owner rechecks registration lifetime, current identity and modal availability.
data SessionSidebarRequest
  = SelectSessionWindow !Text !Int
  | SwitchSession !Text !Int -- Target and requesting display lifetime.
  | SessionDeleted !Text
  deriving (Eq,Show)
