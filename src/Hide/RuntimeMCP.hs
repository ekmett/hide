{-# LANGUAGE OverloadedStrings #-}
-- | Build/run and terminal tools backed by the editor's shared runtime services.
--
-- Agent commands use the same build-job and console owners as menus and ACP
-- terminal requests. Captured builds occupy the single shared job slot; terminals
-- have separate IDs and bounded retained output. Executables and argv are passed
-- directly, while terminal input may itself execute shell commands.
module Hide.RuntimeMCP (runtimeTools, runtimeTool, runtimeToolNames) where

import Data.Aeson
import Data.Aeson.Types (parseEither,parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (canonicalizePath)
import Hide.Model
import Hide.SessionServices
import qualified Hide.Consoles as C
import qualified Hide.Terminal as Term
import qualified Hide.Build as B
import qualified Hide.BuildJobs as Jobs

runtimeToolNames :: [T.Text]
runtimeToolNames=["build_status","build_output","build_start","build_stop","terminal_list","terminal_start","terminal_output","terminal_input","terminal_stop"]

-- | Schemas for build lifecycle and terminal creation, input, output and stopping.
runtimeTools :: [Value]
runtimeTools=
  [ tool "build_status" "Read active build and last completion: exact jobId, semantic windowId when open, retained output character count/truncation and exit code. build_output reads its combined stdout/stderr." True [] []
  , tool "build_output" "Read combined captured output for the exact jobId from build_status. Character offset is relative to the retained tail; default/maximum limit 32768 Unicode characters. Closing the output window keeps the last job readable; a newer job expires its ID." True ["jobId"] [("jobId",str),("offset",integer),("limit",integer)]
  , tool "build_start" "Compile, make or run the selected project using THC/GHC. Requires saved source buffers. Overrides apply only to this job. Captures output by default; terminal=true runs in a shared Ghostty terminal." False ["action"] [("action",enum ["compile","make","run"]),("toolchain",enum ["THC","GHC"]),("target",str),("arguments",strings),("terminal",boolean)]
  , tool "build_stop" "Cancel pending editor build preparation and stop the active captured build/run; retain captured output and completion status." False [] []
  , tool "terminal_list" "List shared Ghostty terminals, buffer IDs, and exit codes." True [] []
  , tool "terminal_start" "Start an executable and argument array in a shared Ghostty terminal; no implicit shell. cwd defaults to the project. Returns terminalId and opens its window." False ["command"] [("command",str),("args",strings),("cwd",str),("outputByteLimit",integer)]
  , tool "terminal_output" "Read retained terminal output (at most 128 KiB per call) and exit code. Offset is relative to the retained tail; truncated indicates dropped earlier bytes." True ["terminalId"] [("terminalId",str),("offset",integer),("limit",integer)]
  , tool "terminal_input" "Write UTF-8 input to a shared terminal. Input may execute commands or send control characters." False ["terminalId","text"] [("terminalId",str),("text",str)]
  , tool "terminal_stop" "Terminate a shared terminal process, retaining its output window." False ["terminalId"] [("terminalId",str)]
  ]
  where
    str=object ["type" .= ("string"::T.Text)]
    integer=object ["type" .= ("integer"::T.Text)]
    boolean=object ["type" .= ("boolean"::T.Text)]
    strings=object ["type" .= ("array"::T.Text),"items" .= str]
    enum values=object ["type" .= ("string"::T.Text),"enum" .= (values::[T.Text])]
    tool :: T.Text -> T.Text -> Bool -> [T.Text] -> [(T.Text,Value)] -> Value
    tool name description readOnly required props=object ["name" .= name,"description" .= description,
      "inputSchema" .= object ["type" .= ("object"::T.Text),"properties" .= object [K.fromText key .= value | (key,value)<-props],"required" .= required,"additionalProperties" .= False],
      "annotations" .= object ["readOnlyHint" .= readOnly,"destructiveHint" .= not readOnly,"openWorldHint" .= not readOnly]]

-- | Dispatch against the session-owned build and console services.
-- Builds require saved source buffers; launch planning can perform synchronous IO.
runtimeTool :: SessionServices -> Desktop -> T.Text -> Value -> IO (Desktop,IO (Either T.Text Value))
runtimeTool runtime d name args=case parseEither (withObject "arguments" pure) args of
  Left err -> done d (Left (T.pack err))
  Right o -> case name of
    "build_status" -> Jobs.buildJobStatus jobs d >>= done d . Right
    "build_output" -> case parseEither (\_ -> (,,) <$> o .: "jobId" <*> o .:? "offset" .!= 0 <*> o .:? "limit" .!= 32768) args of
      Left err -> bad err
      Right (ident,offset,limit) -> (d,) <$> Jobs.buildJobOutput jobs ident offset limit
    "build_stop" -> do
      updated<-stopSessionBuild runtime d
      Jobs.buildJobStatus jobs updated >>= done updated . Right
    "build_start" -> case parseEither (\_ -> (,,,,) <$> o .: "action" <*> o .:? "toolchain" <*> o .:? "target" <*> o .:? "arguments" <*> o .:? "terminal" .!= False) args of
      Left err -> bad err
      Right (action,toolchain,target,arguments,terminal) -> do
        state<-Jobs.buildJobStatus jobs d
        if parseMaybe (withObject "status" (.: "active")) state==Just True then failWith "A build/run is already active."
        else if any (\doc -> documentLabel doc==Nothing && documentModified doc) (M.elems (buffers d)) then failWith "Save modified source buffers before building files on disk."
        else case lookup (action::T.Text) [("compile",B.Compile),("make",B.Make),("run",B.Run)] of
          Nothing -> failWith "Expected compile, make or run."
          Just task | terminal && task/=B.Run -> failWith "terminal=true is only valid for run."
          Just task -> do
            root<-B.resolveBuildRoot d
            config<-B.loadBuildConfig directory root
            let selected=case (toolchain::Maybe T.Text) of Nothing -> Right (B.buildToolchain config); Just "THC" -> Right B.THC; Just "GHC" -> Right B.GHC; _ -> Left "Expected THC or GHC."
            case selected of
              Left err -> failWith err
              Right compiler -> do
                let updated=config {B.buildToolchain=compiler,B.buildExecutable=if compiler==B.buildToolchain config then B.buildExecutable config else if compiler==B.THC then "thc" else "ghc",B.buildTarget=maybe (B.buildTarget config) id target,B.buildArguments=maybe (B.buildArguments config) id arguments}
                plan<-B.buildPlan task updated root (B.buildSource d)
                case plan of
                  Left err -> failWith err
                  Right [(command,argv)] | terminal -> start (Term.TerminalConfig command argv [] root 80 24) (1024*1024)
                  Right _ | terminal -> failWith "This run needs a compile step; use captured mode."
                  Right commands -> do
                    next<-Jobs.startBuildJob jobs (T.pack (show task)) root commands d
                    Jobs.buildJobStatus jobs next >>= done next . Right
    "terminal_list" -> do
      entries<-C.listConsoles consoles
      done d (Right (object ["available" .= Term.terminalAvailable,"terminals" .= [object ["terminalId" .= ident,"bufferId" .= bid,"exitCode" .= code] | (ident,bid,code)<-entries]]))
    "terminal_start" -> case parseEither (\_ -> (,,,) <$> o .: "command" <*> o .:? "args" .!= [] <*> o .:? "cwd" <*> o .:? "outputByteLimit" .!= (1024*1024)) args of
      Left err -> bad err
      Right (command,argv,_,limit) | null command || any (elem '\0') (command:argv) || limit<0 || limit>16*1024*1024 -> failWith "Invalid command, arguments or output limit (0..16 MiB)."
      Right (_,_,Just wanted,_) | '\0' `elem` wanted -> failWith "Invalid working directory."
      Right (command,argv,wanted,limit) -> do
        root<-maybe (B.resolveBuildRoot d) canonicalizePath wanted
        start (Term.TerminalConfig command argv [] root 80 24) limit
    "terminal_output" -> case parseEither (\_ -> (,,) <$> o .: "terminalId" <*> o .:? "offset" .!= 0 <*> o .:? "limit" .!= 32768) args of
      Left err -> bad err
      Right (_,offset,limit) | offset<0 || limit<1 || limit>131072 -> failWith "Use offset>=0 and limit 1..131072."
      Right (ident,offset,limit) -> do
        result<-C.consoleOutput consoles ident
        done d $ fmap (\(bytes,truncated,code) -> object ["terminalId" .= ident,"offset" .= offset,"retainedBytes" .= BS.length bytes,"truncated" .= truncated,"exitCode" .= code,"text" .= TE.decodeUtf8With lenientDecode (BS.take limit (BS.drop offset bytes))]) result
    "terminal_input" -> case parseEither (\_ -> (,) <$> o .: "terminalId" <*> o .: "text") args of
      Left err -> bad err
      Right (ident,text) | BS.length (TE.encodeUtf8 text)>65536 -> failWith "Terminal input is limited to 64 KiB."
                        | otherwise -> C.inputConsole consoles ident (TE.encodeUtf8 text) >>= done d . fmap (const (object ["accepted" .= True]))
    "terminal_stop" -> case parseEither (\_ -> o .: "terminalId") args of
      Left err -> bad err
      Right ident -> C.killConsole consoles ident >>= done d . fmap (const (object ["accepted" .= True]))
    _ -> failWith "Unknown runtime tool."
  where
    directory=sessionDirectory runtime
    consoles=sessionConsoles runtime
    jobs=sessionBuildJobs runtime
    done updated result=pure (updated,pure result)
    failWith=done d . Left
    bad=failWith . T.pack
    start config limit=do
      result<-C.startConsole consoles config limit d
      case result of
        Left err -> failWith err
        Right (ident,updated) -> done updated (Right (object ["terminalId" .= ident,"bufferId" .= fmap bufferId (activeWindow updated)]))
