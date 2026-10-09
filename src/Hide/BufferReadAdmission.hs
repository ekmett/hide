{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.BufferReadAdmission
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Host-owned callback receipt for one granted capture. Policy owns minting;
-- this lightweight adapter keeps receipt expiry independent of UI model types.
module Hide.BufferReadAdmission
  ( ReadAdmission, withReadAdmission, readReference, resolveReadReference ) where

import Control.Exception (bracket)
import Data.IORef
import Data.Text (Text)
import Hide.Plugin.BufferHost

-- The closed check is the owning Permissions session latch, not a second scope.
data ReadAdmission = ReadAdmission BufferNamespace (IO Bool) (IORef Bool)

withReadAdmission :: BufferNamespace -> IO Bool -> (ReadAdmission -> IO a) -> IO a
withReadAdmission namespace closed=bracket (ReadAdmission namespace closed <$> newIORef True)
  (\(ReadAdmission _ _ live)->writeIORef live False)

readReference :: ReadAdmission -> Int -> IO (Either Text BufferRef)
readReference receipt@(ReadAdmission namespace _ _) ident=(bufferReference namespace ident <$) <$> check receipt

resolveReadReference :: ReadAdmission -> BufferRef -> IO (Either Text Int)
resolveReadReference receipt@(ReadAdmission namespace _ _) reference=do
  accepted<-check receipt
  pure (accepted >> maybe (Left "Buffer reference belongs to another editor session") Right (referenceId namespace reference))

check :: ReadAdmission -> IO (Either Text ())
check (ReadAdmission _ closed live)=do
  activeNow<-readIORef live
  stopped<-closed
  pure $ if activeNow && not stopped then Right () else Left "Buffer read admission expired"
