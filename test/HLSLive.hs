{-# LANGUAGE OverloadedStrings #-}
-- Run against an installed HLS with the project's supported GHC first in PATH.
-- Compile: cabal exec -- ghc -threaded -isrc test/HLSLive.hs -o /tmp/thc-hls-live
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import THC.Edit.LSP

main :: IO ()
main = bracket temporary removePathForcibly $ \root -> do
  let file = root </> "Live.hs"
      good = "module Live where\n\nanswer :: Int\nanswer = 42\n\nuse :: Int\nuse = answer\n"
      bad = good <> "\nbroken :: Int\nbroken = True\n"
      params line column = object ["textDocument" .= object ["uri" .= fileUri file], "position" .= object ["line" .= (line :: Int), "character" .= (column :: Int)]]
  writeFile (root </> "hie.yaml") "cradle:\n  direct:\n    arguments:\n      - Live.hs\n"
  T.writeFile file bad
  bracket (startClient root) stopClient $ \client -> do
    syncDocuments client [(file,0,bad)]
    broken <- await client "initial error diagnostics" $ \event -> case event of
      Diagnostics path version diagnostics | path == file && version == Just 0 && nonempty diagnostics -> Just (object ["version" .= version, "diagnostics" .= diagnostics])
      _ -> Nothing
    report "didOpen diagnostics" broken
    syncDocuments client [(file,1,good)]
    cleared <- await client "cleared diagnostics" $ \event -> case event of
      Diagnostics path version diagnostics | path == file && version == Just 1 && not (nonempty diagnostics) -> Just (object ["version" .= version, "diagnostics" .= diagnostics])
      _ -> Nothing
    report "didChange cleared diagnostics" cleared
    hover <- rpc client "textDocument/hover" (params 6 8)
    check "hover contains Int" ("Int" `T.isInfixOf` T.pack (BL.unpack (encode hover)))
    report "hover" hover
    definition <- rpc client "textDocument/definition" (params 6 8)
    check "definition points into fixture" (fileUri file `T.isInfixOf` T.pack (BL.unpack (encode definition)))
    report "definition" definition
    completion <- rpc client "textDocument/completion" (params 6 12)
    check "completion contains answer" ("answer" `T.isInfixOf` T.pack (BL.unpack (encode completion)))
    report "completion" (object ["containsAnswer" .= True])
    renamed <- rpc client "textDocument/rename" (object ["textDocument" .= object ["uri" .= fileUri file], "position" .= object ["line" .= (6 :: Int), "character" .= (8 :: Int)], "newName" .= ("renamedAnswer" :: T.Text)])
    check "rename returns workspace edits" ("renamedAnswer" `T.isInfixOf` T.pack (BL.unpack (encode renamed)))
    report "rename workspace edit (including protocol versions)" renamed
    syncDocuments client []
  putStrLn "Live HLS checks passed"
  where
    check label ok = unless ok (error label)
    temporary = do
      base <- getTemporaryDirectory
      (path, file) <- openTempFile base "thc-hls-live"
      hClose file
      removeFile path
      createDirectory path
      canonicalizePath path

nonempty :: Value -> Bool
nonempty value = case fromJSON value :: Result [Value] of Success items -> not (null items); _ -> False

report :: String -> Value -> IO ()
report label value = T.putStrLn (T.pack label <> ": " <> TE.decodeUtf8 (BL.toStrict (encode value)))

rpc :: Client -> T.Text -> Value -> IO Value
rpc client method params = do
  ident <- request client method params
  response <- await client (T.unpack method) $ \event -> case event of Response actual value | actual == ident -> Just value; _ -> Nothing
  case get "error" response :: Maybe Value of
    Just failure -> error (T.unpack method ++ ": " ++ BL.unpack (encode failure))
    Nothing -> maybe (error "Missing JSON RPC result") pure (get "result" response)

get :: FromJSON a => T.Text -> Value -> Maybe a
get key = parseMaybe (withObject "object" (.: Key.fromText key))

await :: Client -> String -> (Event -> Maybe a) -> IO a
await client label select = do
  result <- timeout 60000000 loop
  maybe (error ("Timed out: " ++ label)) pure result
  where
    loop = do
      events <- pollEvents client
      case [message | ServerError message <- events] of message:_ -> error (T.unpack message); [] -> pure ()
      case [value | event <- events, Just value <- [select event]] of
        value:_ -> pure value
        [] -> threadDelay 20000 >> loop
