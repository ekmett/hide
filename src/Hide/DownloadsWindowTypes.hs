-- |
-- Module      : Hide.DownloadsWindowTypes
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : GHC2021
--
-- Closed first-party Downloads action. The WindowRef owns presentation and
-- the numeric ID owns the job; no row label, index or mutable payload is retained.
module Hide.DownloadsWindowTypes (DownloadCancelRequest(..)) where
import Hide.Plugin.Window (WindowRef)
data DownloadCancelRequest = DownloadCancelRequest !WindowRef !Int deriving (Eq,Show)
