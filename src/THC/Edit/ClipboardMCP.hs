{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.ClipboardMCP (clipboardTools, clipboardTool) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import THC.Edit.Model

clipboardTools :: [Value]
clipboardTools=[object
  ["name" .= ("clipboard_write"::T.Text)
  ,"description" .= ("Copy supplied text to the editor clipboard and queue it for the attached frontend's system clipboard. Does not read existing clipboard contents. Browser/terminal permissions may prevent the system copy; queued is not confirmation of OS acceptance. Maximum 1 MiB UTF-8."::T.Text)
  ,"inputSchema" .= object ["type" .= ("object"::T.Text),"required" .= ["text"::T.Text],"additionalProperties" .= False,
    "properties" .= object ["text" .= object ["type" .= ("string"::T.Text)]]]
  ,"annotations" .= object ["readOnlyHint" .= False,"destructiveHint" .= False,"openWorldHint" .= False]]]

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
