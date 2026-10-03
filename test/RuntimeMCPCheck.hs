{-# LANGUAGE OverloadedStrings #-}
module RuntimeMCPCheck (checks) where
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import qualified Data.Aeson.Key as K
import System.Directory (findExecutable,getTemporaryDirectory)
import Hide.Conversation
import Hide.RuntimeMCP
import Hide.Terminal (terminalAvailable)
import qualified Hide.BuildJobs as Jobs
import Hide.Model
import Hide.Buffer
checks :: IO ()
checks=withConversation $ \runtime -> do
  let d=initialDesktop (80,25)
      call desktop name args=do (updated,finish)<-runtimeTool runtime desktop name args; result<-finish; pure (updated,result)
      check label ok=unless ok (error label)
      field key value=parseMaybe (withObject "reply" (.: K.fromText key)) value
      core desktop _=pure (False,desktop)
  (_,listing)<-call d "terminal_list" (object [])
  check "terminal capability explicitly reported" (case listing of Right value -> field "available" value==Just terminalAvailable; _ -> False)
  (_,invalid)<-call d "terminal_output" (object ["terminalId" .= ("missing"::T.Text),"limit" .= (maxBound::Int)])
  check "terminal reads bounded" (either (const True) (const False) invalid)
  let dirty=insertText "x" (addDocument Nothing (newBuffer "") d)
  (unchanged,refused)<-call dirty "build_start" (object ["action" .= ("make"::T.Text)])
  check "build refuses unsaved source without UI dialog" (unchanged==dirty && either (const True) (const False) refused)
  python<-findExecutable "python3"
  case python of
    Nothing -> pure ()
    Just executable -> do
      root<-getTemporaryDirectory
      let (_,_,jobs)=conversationServices runtime
      started<-Jobs.startBuildJob jobs "Test job" root [(executable,["-c","print('mcp job'); raise SystemExit(7)"])] d
      done<-awaitJob jobs started (100::Int)
      status<-Jobs.buildJobStatus jobs
      check "job retains exact nonzero exit status" (field "active" status==Just False && field "exitCode" status==Just (7::Int))
      check "job output is a live buffer" ("mcp job" `T.isInfixOf` activeText done)
      if not terminalAvailable then pure () else do
        (terminal,response)<-call d "terminal_start" (object ["command" .= executable,"args" .= (["-u","-c","print('ready',flush=True); print(input(),flush=True)"]::[String]),"cwd" .= root])
        ident<-case response of Right value -> maybe (error "missing terminalId") pure (field "terminalId" value :: Maybe T.Text); Left err -> error (T.unpack err)
        (_,written)<-call terminal "terminal_input" (object ["terminalId" .= ident,"text" .= ("mcp input\n"::T.Text)])
        check "terminal accepts input" (either (const False) (const True) written)
        output<-awaitTerminal call terminal ident (100::Int)
        check "shared terminal produces captured output and exit" (field "exitCode" output==Just (0::Int) && maybe False (T.isInfixOf "mcp input") (field "text" output))
  _<-tickConversation runtime d
  _<-conversationEffects runtime core d []
  putStrLn "runtime MCP checks passed"
  where
    awaitJob jobs d count | count<=0=error "MCP job timed out"
                         | otherwise=do
      updated<-Jobs.tickBuildJobs jobs d
      status<-Jobs.buildJobStatus jobs
      if parseMaybe (withObject "job" (.: "active")) status==Just False then pure updated else threadDelay 20000 >> awaitJob jobs updated (count-1)
    awaitTerminal call d ident count | count<=0=error "MCP terminal timed out"
                                    | otherwise=do
      (_,reply)<-call d "terminal_output" (object ["terminalId" .= ident])
      case reply of
        Right value | Just (Just (_::Int))<-parseMaybe (withObject "output" (.: "exitCode")) value -> pure value
        _ -> threadDelay 20000 >> awaitTerminal call d ident (count-1)
