{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.TerminalTools
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Terminal wire declarations and presentation. The typed service owns fresh
-- admission and shared process lifetimes; arguments supply no policy authority.
module Hide.TerminalTools (tools) where

import Data.Aeson
import Data.Aeson.Types (Parser,parseEither)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Hide.Plugin.Command
import Hide.Plugin.Terminal
import Hide.Plugin.Tool

-- | Five complete public tools. Optional absence refuses rather than falling
-- back to a private interpreter. Every service call admits its own permission.
tools :: [Tool (Maybe TerminalServices)]
tools=
  [tool "terminal_list" "hide.terminal.list" True
    "List shared Ghostty terminals, buffer IDs, and exit codes."
    (input [] [] (const (pure ()))) listing (\services ()->terminalList services)
  ,tool "terminal_start" "hide.terminal.start" False
    "Start an executable and argument array in a shared Ghostty terminal; no implicit shell. cwd defaults to the project. Returns terminalId and opens its window."
    (input ["command"] [("command",str),("args",strings),("cwd",str),("outputByteLimit",integer)] $ \o->do
      request<-TerminalLaunch <$> o .: "command" <*> o .:? "args" .!= [] <*> o .:? "cwd" <*> o .:? "outputByteLimit" .!= 1048576
      if T.null (terminalCommand request) || any (T.any (=='\0')) (terminalCommand request:terminalArguments request) ||
          maybe False (elem '\0') (terminalDirectory request) || terminalOutputByteLimit request<0 || terminalOutputByteLimit request>16777216
        then fail "Invalid command, arguments, working directory or output limit (0..16 MiB)." else pure request)
    opened terminalStart
  ,tool "terminal_output" "hide.terminal.output" True
    "Read retained terminal output (at most 128 KiB per call) and exit code. Offset and limit count bytes; offset is relative to the retained tail. truncated indicates dropped earlier bytes."
    (input ["terminalId"] [("terminalId",str),("offset",integer),("limit",integer)] $ \o->do
      ident<-TerminalId <$> o .: "terminalId"
      offset<-o .:? "offset" .!= 0; limit<-o .:? "limit" .!= 32768
      if offset<0 || limit<1 || limit>131072 then fail "Use offset>=0 and limit 1..131072." else pure (ident,offset,limit))
    page (\services (ident,offset,limit)->terminalOutput services ident offset limit)
  ,tool "terminal_input" "hide.terminal.input" False
    "Write UTF-8 input to a shared terminal. Input may execute commands or send control characters."
    (input ["terminalId","text"] [("terminalId",str),("text",str)] $ \o->do
      ident<-TerminalId <$> o .: "terminalId"; text<-o .: "text"
      if BS.length (TE.encodeUtf8 text)>65536 then fail "Terminal input is limited to 64 KiB." else pure (ident,text))
    accepted (\services (ident,text)->terminalInput services ident text)
  ,tool "terminal_stop" "hide.terminal.stop" False
    "Terminate a shared terminal process, retaining its output window."
    (input ["terminalId"] [("terminalId",str)] (fmap TerminalId . (.: "terminalId"))) accepted terminalStop]
  where
    tool name identity readonly description arguments result action=Tool name (ToolHints readonly (not readonly) (not readonly))
      (CommandDef identity description arguments result $ \context value->case context of
        Nothing->pure (Left (CommandRejected "Terminal services are unavailable."))
        Just services->fmap (either (Left . CommandRejected) Right) (action services value))
    str=object ["type" .= ("string"::T.Text)]
    integer=object ["type" .= ("integer"::T.Text)]
    boolean=object ["type" .= ("boolean"::T.Text)]
    exitCode=object ["type" .= (["integer","null"]::[T.Text])]
    strings=object ["type" .= ("array"::T.Text),"items" .= str]
    objectSchema properties=object ["type" .= ("object"::T.Text),"properties" .= object [K.fromText key .= value | (key,value)<-properties],
      "required" .= map fst properties,"additionalProperties" .= False]
    output properties encodeResult=Codec (objectSchema properties) (const (Left "Terminal results are host values.")) encodeResult
    opened=output [("terminalId",str),("bufferId",integer)] (\result->object ["terminalId" .= terminalIdText (openedTerminal result),"bufferId" .= openedTerminalBuffer result])
    listing=output [("available",boolean),("terminals",object ["type" .= ("array"::T.Text),"items" .=
      objectSchema [("terminalId",str),("bufferId",integer),("exitCode",exitCode)]])] (\result->object ["available" .= terminalsAvailable result,"terminals" .=
      [object ["terminalId" .= terminalIdText (summaryTerminal item),"bufferId" .= summaryTerminalBuffer item,"exitCode" .= summaryTerminalExitCode item] | item<-terminalEntries result]])
    page=output [("terminalId",str),("offset",integer),("retainedBytes",integer),("truncated",boolean),("exitCode",exitCode),("text",str)] (\result->object ["terminalId" .= terminalIdText (pageTerminal result),"offset" .= terminalPageOffset result,
      "retainedBytes" .= terminalRetainedBytes result,"truncated" .= terminalTruncated result,"exitCode" .= terminalExitCode result,
      "text" .= TE.decodeUtf8With lenientDecode (terminalPageBytes result)])
    accepted=output [("accepted",boolean)] (\()->object ["accepted" .= True])

input :: [T.Text] -> [(T.Text,Value)] -> (Object -> Parser a) -> Codec a
input required fields parse=Codec schema (either (Left . T.pack) Right . parseEither parseArguments) (const Null)
  where
    schema=object ["type" .= ("object"::T.Text),"properties" .= object [K.fromText key .= value | (key,value)<-fields],
      "required" .= required,"additionalProperties" .= False]
    parseArguments=withObject "terminal arguments" $ \o->
      if any ((`notElem` map (K.fromText . fst) fields)) (KM.keys o) then fail "Unknown terminal argument." else parse o
