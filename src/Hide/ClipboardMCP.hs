{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.ClipboardMCP
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Agent writes to the editor clipboard and attached frontend clipboard queue.
--
-- The tool never reads the user's system clipboard. A serial marks a new export
-- for the frontend, whose browser/terminal permissions can still reject the copy;
-- a successful tool reply acknowledges queueing, not OS acceptance.
module Hide.ClipboardMCP (clipboardTools, clipboardTool) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Hide.Model

-- | Schema for the bounded text-only clipboard write operation.
clipboardTools :: [Value]
clipboardTools=[object
  ["name" .= ("clipboard_write"::T.Text)
  ,"description" .= ("Copy supplied text to the editor clipboard and queue it for the attached frontend's system clipboard. Does not read existing clipboard contents. Browser/terminal permissions may prevent the system copy; queued is not confirmation of OS acceptance. Maximum 1 MiB UTF-8."::T.Text)
  ,"inputSchema" .= object ["type" .= ("object"::T.Text),"required" .= ["text"::T.Text],"additionalProperties" .= False,
    "properties" .= object ["text" .= object ["type" .= ("string"::T.Text)]]]
  ,"annotations" .= object ["readOnlyHint" .= False,"destructiveHint" .= False,"openWorldHint" .= False]]]

-- | Validate text, replace the editor clipboard and increment the export serial.
-- Reject NUL and payloads exceeding one MiB of UTF-8.
clipboardTool :: Desktop -> Value -> IO (Desktop,IO (Either T.Text Value))
clipboardTool d args=case parseEither parse args of
  Left err -> pure (d,pure (Left (T.pack err)))
  Right text -> let (serial,_)=clipboardExport d in pure
    (d {clipboard=text,clipboardCode=Nothing,clipboardExport=(serial+1,Just text)},pure (Right (object ["editorClipboardWritten" .= True,"frontendCopyQueued" .= True,"bytes" .= BS.length (TE.encodeUtf8 text)])))
  where
    parse=withObject "clipboard_write" $ \o -> do
      unless (KM.keys o==["text"]) (fail "Supply only text")
      text<-o .: "text"
      unless (BS.length (TE.encodeUtf8 text)<=1048576) (fail "Clipboard write exceeds 1 MiB")
      unless (not (T.any (=='\0') text)) (fail "Clipboard text cannot contain NUL")
      pure text
