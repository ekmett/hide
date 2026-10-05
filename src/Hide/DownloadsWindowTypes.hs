-- SPDX-License-Identifier: BSD-3-Clause
-- | Closed first-party Downloads action. The WindowRef owns presentation and
-- the numeric ID owns the job; no row label, index or mutable payload is retained.
module Hide.DownloadsWindowTypes (DownloadCancelRequest(..)) where
import Hide.Plugin.Window (WindowRef)
data DownloadCancelRequest = DownloadCancelRequest !WindowRef !Int deriving (Eq,Show)
