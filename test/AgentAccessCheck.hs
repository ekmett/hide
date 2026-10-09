{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : AgentAccessCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module AgentAccessCheck (checks) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless)
import qualified Data.Set as S
import qualified Data.Text as T
import Hide.AgentAccess
import Hide.AgentHub (AgentId(..))

checks :: IO ()
checks = do
  access <- newAgentAccess
  let first=AgentId "agent-1"
      second=AgentId "agent-2"
      check label condition=unless condition (error label)
  tokens <- mapConcurrently (const (grantAgentAccess access first)) [1..64::Int]
  other <- grantAgentAccess access second
  check "grants use distinct opaque 192-bit OS-random tokens"
    (S.size (S.fromList (other:tokens))==65 && all (\token -> T.length token==48 && T.all (`elem` ("0123456789abcdef"::String)) token) (other:tokens))
  resolved <- mapM (resolveAgentAccess access) tokens
  check "every concurrent grant resolves only to its host-bound identity" (all (==Just first) resolved)
  unknown <- mapM (resolveAgentAccess access) ["", "agent-1", T.replicate 48 "0", T.replicate 1024 "a"]
  check "agent IDs and invalid or ungranted strings are not capabilities" (all (==Nothing) unknown)
  revokeAgentAccess access first
  revoked <- mapM (resolveAgentAccess access) tokens
  preserved <- resolveAgentAccess access other
  check "revocation removes all tokens for one identity and preserves others" (all (==Nothing) revoked && preserved==Just second)
  revokeAgentAccess access first
  replacement <- grantAgentAccess access first
  check "regrant never reuses a revoked token" (replacement `notElem` tokens)
  fresh <- newAgentAccess
  afterRestart <- resolveAgentAccess fresh replacement
  check "a fresh registry never restores capabilities" (afterRestart==Nothing)
  putStrLn "agent access checks passed"
