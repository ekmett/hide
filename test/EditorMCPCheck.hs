{-# LANGUAGE OverloadedStrings #-}
module EditorMCPCheck (checks) where
import Control.Monad (unless)
import Control.Exception (bracket, try, IOException)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (openBinaryTempFile, hClose, hSeek, SeekMode(AbsoluteSeek))
import qualified Data.ByteString as BS
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import THC.Edit.EditorMCP
import THC.Edit.Buffer
import THC.Edit.Model

checks :: IO ()
checks = do
  let check name ok=unless ok (error name)
      original=addDocument Nothing (newBuffer "old\nsecond") (initialDesktop (80,25))
      win=fromJust (activeWindow original)
      edited=original {buffers=M.adjust (\doc->doc {documentBuffer=replaceBuffer False "unsaved λ\nsecond" (documentBuffer doc)}) (bufferId win) (buffers original)}
      rpc method params=object ["jsonrpc" .= ("2.0"::T.Text),"id" .= (7::Int),"method" .= (method::T.Text),"params" .= params]
      call name args=editorResponse edited (rpc "tools/call" (object ["name" .= (name::T.Text),"arguments" .= args]))
      text=TE.decodeUtf8 . BL.toStrict . encode
      names=editorResponse edited (rpc "tools/list" (object []))
      content=call "read_buffer" (object ["bufferId" .= bufferId win,"startLine" .= (1::Int),"lineCount" .= (1::Int)])
  check "MCP lists live windows and buffers" (all (\name->maybe False (T.isInfixOf name . text) names) ["list_windows","list_buffers","read_buffer","read_selection"])
  check "MCP returns unsaved Unicode text" (maybe False (T.isInfixOf "unsaved λ" . text) content && not (maybe False (T.isInfixOf "second" . text) content))
  check "MCP does not fabricate saved paths for untitled buffers" (maybe False (T.isInfixOf "Untitled" . text) (call "list_windows" (object [])))
  check "MCP missing buffer is a tool error" (maybe False (T.isInfixOf "Buffer not found" . text) (call "read_buffer" (object ["bufferId" .= (999::Int)])))
  let pastEnd=call "read_buffer" (object ["startLine" .= (maxBound::Int),"lineCount" .= (1000::Int)])
  check "MCP extreme line offsets cannot overflow" (maybe False (T.isInfixOf "lineCount\\\":0" . text) pastEnd)
  check "MCP bounds reads" (maybe False (T.isInfixOf "lineCount" . text) (call "read_buffer" (object ["lineCount" .= (1001::Int)])))
  check "MCP notifications receive no response" (editorResponse edited (object ["jsonrpc" .= ("2.0"::T.Text),"method" .= ("notifications/initialized"::T.Text)])==Nothing)
  let initialized=editorResponse edited (rpc "initialize" (object ["protocolVersion" .= ("2025-11-25"::T.Text)]))
  check "MCP initialize advertises tools" (maybe False (T.isInfixOf "capabilities" . text) initialized)
  check "MCP invalid request is protocol error" (case editorResponse edited Null of Just (Object fields)->KM.member "error" fields; _->False)
  let decoded=content >>= parseMaybe (withObject "reply" (.: "result")) :: Maybe Value
  check "MCP tool result envelope exists" (decoded/=Nothing)
  temp<-getTemporaryDirectory
  bracket (openBinaryTempFile temp "thc-mcp-line") (\(path,h)->hClose h >> removeFile path) $ \(_,h) -> do
    BS.hPut h "first\nsecond\n"
    hSeek h AbsoluteSeek 0
    Just (first,rest)<-readMCPLine h BS.empty
    Just (second,_)<-readMCPLine h rest
    check "MCP reader preserves following messages" (first=="first" && second=="second")
    hSeek h AbsoluteSeek 0
    BS.hPut h (BS.replicate 1048577 120)
    hSeek h AbsoluteSeek 0
    tooLarge<-try (readMCPLine h BS.empty) :: IO (Either IOException (Maybe (BS.ByteString,BS.ByteString)))
    check "MCP rejects oversized unterminated input" (case tooLarge of Left _->True; _->False)
  putStrLn "editor MCP checks passed"
