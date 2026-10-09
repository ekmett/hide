-- |
-- Module      : Hide.FrameTiming
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : GHC2021
--
-- Outstanding native redraw demands, identified by the input sequence echoed
-- by a presented frame. Times use the frontend's monotonic nanosecond clock;
-- clocks on different machines are never compared. Retiring a coalesced prefix
-- returns its oldest demand once. A no-op receipt retires without drawing.
module Hide.FrameTiming
  (FrameTiming, emptyFrameTiming, requestFrame, settleFrame) where

import qualified Data.Map.Strict as M
import Data.Word (Word64)

-- | Input queues bound the number of outstanding demands; no screen payload lives here.
data FrameTiming = FrameTiming !Int !(M.Map Int Word64)

-- | No outstanding draw demand.
emptyFrameTiming :: FrameTiming
emptyFrameTiming=FrameTiming 1 M.empty

-- | Register an accepted input before transport. Identifiers are never reused.
requestFrame :: Word64 -> FrameTiming -> (Int,FrameTiming)
requestFrame time (FrameTiming next pending)
  | next==maxBound=error "Frame demand sequence exhausted"
  | otherwise=(next,FrameTiming (next+1) (M.insert next time pending))

-- | Retire every demand represented by this snapshot. Duplicate receipts return
-- 'Nothing'; later input remains pending. The caller discards the returned time
-- for an acknowledged input that made no visible change.
settleFrame :: Int -> FrameTiming -> (Maybe Word64,FrameTiming)
settleFrame serial (FrameTiming next pending)=
  let (finished,waiting)=M.spanAntitone (<=serial) pending
  in (snd <$> M.lookupMin finished,FrameTiming next waiting)
