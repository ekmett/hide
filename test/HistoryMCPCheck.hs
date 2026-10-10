-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : HistoryMCPCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module HistoryMCPCheck (checks) where
import SourceWindowFixture (sourceFixtureBuffer)
import Control.Monad (unless, forM_)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hide.Files (FileState(..))
import Hide.HistoryMCP
import Hide.Model
import Hide.Buffer
checks :: IO ()
checks=do
  let check label ok=unless ok (error label)
      base=addDocument Nothing (newBuffer "old\nline") (initialDesktop (80,25))
      edited=insertText "new " base
      bid=maybe 0 sourceFixtureBuffer (activeWindow edited)
      version=revision (documentBuffer (buffers edited M.! bid))
      call desktop name values=do (next,finish)<-historyTool desktop name (object (["bufferId" .= bid]++values)); reply<-finish; pure (next,reply)
      text=TE.decodeUtf8 . BL.toStrict . encode
  (same,preview)<-call edited "editor_history" []
  check "history preview is read-only" (same==edited)
  check "history exposes diff content" (case preview of Right value -> all (`T.isInfixOf` text value) ["-new old","+old","undoCount"]; _ -> False)
  (unchanged,stale)<-call edited "editor_undo" ["revision" .= (version-1)]
  check "stale history cannot mutate buffer" (unchanged==edited && either (const True) (const False) stale)
  (unchanged2,short)<-call edited "editor_undo" ["revision" .= version,"steps" .= (2::Int)]
  check "too many undo steps fail atomically" (unchanged2==edited && either (const True) (const False) short)
  (undone,result)<-call edited "editor_undo" ["revision" .= version]
  check "undo restores saved contents and dirty flag" (activeText undone=="old\nline" && not (dirty (documentBuffer (buffers undone M.! bid))) && either (const False) (const True) result)
  (redone,_)<-call undone "editor_undo" ["revision" .= (version+1),"direction" .= ("redo"::T.Text)]
  check "redo restores edit" (activeText redone==activeText edited)
  let other=addDocument Nothing (newBuffer "other") edited
  (background,_)<-call other "editor_undo" ["revision" .= version]
  check "background history preserves active window" (fmap windowId (activeWindow background)==fmap windowId (activeWindow other) && contents (documentBuffer (buffers background M.! bid))=="old\nline")
  let binary=addDocument Nothing (replaceSelection (Selection 1 2) "\255" (newByteBuffer (BS.pack [0,1,2]))) (initialDesktop (80,25))
  (_,hex)<-call binary "editor_history" []
  check "binary history previews hex bytes" (case hex of Right value -> "ff" `T.isInfixOf` text value && "bytes" `T.isInfixOf` text value; _ -> False)
  let privateBuffer=(newBuffer "private live text")
        {saved=error "history admission forced private baseline",undoStack=error "history admission forced private undo",redoStack=error "history admission forced private redo"}
      private=edited {buffers=M.adjust (\doc->doc {documentBuffer=privateBuffer,
        documentFile=Just (FileState "/project/thc.toml" Nothing)}) bid (buffers edited)}
  forM_ ["undo","redo"::T.Text] $ \direction->do
    (_,hidden)<-call private "editor_history" ["direction" .= direction]
    (_,refused)<-call private "editor_undo" ["direction" .= direction,"revision" .= (0::Int)]
    check "private history previews and mutations are refused before inspecting history"
      (either (const True) (const False) hidden && either (const True) (const False) refused)
  putStrLn "history MCP checks passed"
