{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module PluginCommandCheck (checks) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (AsyncException(UserInterrupt), evaluate, throwIO, try)
import Control.Monad (unless,forM_)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import System.IO.Error (tryIOError)
import qualified Hide.Plugin.Tool as Tool
import Data.IORef
import qualified Data.Text as T
import System.Timeout (timeout)
import Hide.Plugin.Command

checks :: IO ()
checks=do
  withRegistry $ \registry->do
    let integer=Codec (object ["type" .= ("integer"::T.Text)]) (either (Left . T.pack) Right . parseEither parseJSON) toJSON
        definition name action=CommandDef name "Arithmetic" integer integer action
        increment=definition "example.increment" (\() n->pure (Right (n+1::Int)))
        check label condition=unless condition (error label)
        right= either (error . show) id
        stale (Left StaleCommand{})=True
        stale _=False
    command<-right <$> registerCommand registry increment
    duplicate<-registerCommand registry increment
    check "duplicate registration cannot replace a live handler" (case duplicate of Left DuplicateCommand{}->True; _->False)
    typed<-invoke registry command () 4
    wire<-invokeJSON registry (commandRef command) () (toJSON (4::Int))
    check "typed and wire calls execute the registered handler" (typed==Right 5 && wire==Right (toJSON (5::Int)))
    bad<-invokeJSON registry (commandRef command) () (String "not an integer")
    check "wire arguments are decoded before handler execution" (case bad of Left InvalidArguments{}->True; _->False)
    pending<-right <$> resolveCommand registry "example.increment"
    _<-retireCommand registry pending
    replacement<-right <$> registerCommand registry (definition "example.increment" (\() n->pure (Right (n+100::Int))))
    oldTyped<-invoke registry command () 4
    oldWire<-invokeJSON registry pending () (toJSON (4::Int))
    fresh<-invoke registry replacement () 4
    check "replacement cannot redirect an old typed handle or queued wire call" (stale oldTyped && stale oldWire && fresh==Right 104)
    cross<-withRegistry $ \other->invoke other replacement () 4
    check "handles cannot cross registries" (stale cross)
    nested<-right <$> registerCommand registry (definition "example.nested" (\() n->invoke registry replacement () n))
    nestedReply<-timeout 1000000 (invoke registry nested () 4)
    check "handlers run outside the registry lock" (nestedReply==Just (Right 104))
    entered<-newEmptyMVar
    release<-newEmptyMVar
    result<-newEmptyMVar
    slow<-right <$> registerCommand registry (definition "example.slow" (\() n->putMVar entered () >> takeMVar release >> pure (Right n)))
    _<-forkIO (invoke registry slow () 7 >>= putMVar result)
    began<-timeout 1000000 (takeMVar entered)
    check "slow command begins" (began==Just ())
    retired<-timeout 1000000 (retireCommand registry (commandRef slow))
    check "retirement does not wait for admitted work" (retired==Just (Right ()))
    putMVar release ()
    finished<-timeout 1000000 (takeMVar result)
    refused<-invoke registry slow () 7
    check "admitted work can finish but retirement refuses later calls" (finished==Just (Right 7) && stale refused)
    faulty<-right <$> registerCommand registry (definition "example.faulty" (\() _->evaluate (error "broken handler")))
    failure<-invoke registry faulty () 0
    check "synchronous handler failure becomes a command error" (case failure of Left CommandFailed{}->True; _->False)
    encoder<-right <$> registerCommand registry (increment {commandName="example.encoder",commandOutput=integer {codecEncode= \_->object ["answer" .= (error "broken encoder"::Int)]}})
    encoded<-invokeJSON registry (commandRef encoder) () (toJSON (4::Int))
    check "wire encoding failure is contained before reply publication" (case encoded of Left CommandFailed{}->True; _->False)
    decoder<-right <$> registerCommand registry (increment {commandName="example.decoder",commandInput=integer {codecDecode= \_->Left (error "broken decoder error")}})
    decoded<-invokeJSON registry (commandRef decoder) () (toJSON (4::Int))
    check "lazy codec error is contained before reply publication" (case decoded of Left CommandFailed{}->True; _->False)
    denial<-right <$> registerCommand registry (definition "example.denial" (\() _->pure (Left (CommandRejected (error "broken denial")))))
    denied<-invoke registry denial () 0
    check "lazy handler denial is contained before reply publication" (case denied of Left CommandFailed{}->True; _->False)
    cancelled<-right <$> registerCommand registry (definition "example.cancelled" (\() _->throwIO UserInterrupt))
    interruption<-try (invoke registry cancelled () 0)
    check "asynchronous cancellation is not turned into a command reply" (interruption==Left UserInterrupt)
  escaped<-newIORef (pure (Right Null))
  withRegistry $ \registry->do
    let passthrough=Codec Null Right id
    command<-either (error . show) id <$> registerCommand registry (CommandDef "example.echo" "Echo" passthrough passthrough (\() value->pure (Right value)))
    writeIORef escaped (invoke registry command () Null)
  closed<-readIORef escaped >>= id
  unless (closed==Left RegistryClosed) (error "closing registry rejects escaped handles")
  toolChecks
  putStrLn "plugin command checks passed"

-- The MCP adapter must retain the registry's lifetime and the host's context.
-- All state belongs to this invocation; no files, processes or cleanup setup.
toolChecks :: IO ()
toolChecks=do
  calls<-newIORef (0::Int)
  let check label ok=unless ok (error label)
      toolSchema=object ["type" .= ("object"::T.Text),"additionalProperties" .= False,
        "required" .= (["n"]::[T.Text]),"properties" .= object ["n" .= object ["type" .= ("integer"::T.Text),"minimum" .= (0::Int),"maximum" .= (10::Int)]]]
      decodeInput=withObject "integer request" $ \fields->do
        unless (KM.keys fields==["n"]) (fail "Unexpected field")
        n<-fields .: "n"
        unless (n>=0 && n<=10) (fail "Out of range")
        pure (n::Int)
      input=Codec toolSchema (either (Left . T.pack) Right . parseEither decodeInput) (\n->object ["n" .= n])
      output=Codec (object ["type" .= ("object"::T.Text)]) Right id
      definition name action=CommandDef name "Add the authenticated context" input output action
      run (context::Int) n=modifyIORef' calls (+1) >> pure (Right (object ["answer" .= (context+n)]))
      increment=Tool.Tool "increment" True (definition "example.tool.increment" run)
      second=Tool.Tool "second" True (definition "example.tool.second" run)
      arguments=object ["n" .= (4::Int)]
  escaped<-Tool.withTools [] [increment,second] $ \tools->do
    check "tool discovery contains exactly the composed set" (length (Tool.toolDefinitions tools)==2 && Tool.hasTool tools "increment" && Tool.hasTool tools "second" && not (Tool.hasTool tools "missing"))
    actual<-Tool.callTool tools 10 "increment" arguments
    other<-Tool.callTool tools 20 "second" arguments
    check "tool codecs use the host context" (actual==Right (object ["answer" .= (14::Int)]) && other==Right (object ["answer" .= (24::Int)]))
    before<-readIORef calls
    missing<-Tool.callTool tools 10 "missing" arguments
    spoof<-Tool.callTool tools 10 "increment" (object ["n" .= (4::Int),"context" .= (100::Int)])
    bounds<-Tool.callTool tools 10 "increment" (object ["n" .= (11::Int)])
    oversized<-Tool.callTool tools 10 "increment" (String (T.replicate (1024*1024+1) "x"))
    after<-readIORef calls
    check "unknown, spoofed, out-of-range and oversized calls cannot reach a handler" (all isLeft [missing,spoof,bounds,oversized] && before==after)
    pure tools
  beforeClose<-readIORef calls
  closed<-Tool.callTool escaped 10 "increment" arguments
  afterClose<-readIORef calls
  check "leaving tool scope revokes retained calls" (isLeft closed && beforeClose==afterClose)
  forM_ [(["increment"],[increment]),([],[increment,increment]),([],[increment,Tool.Tool "another" True (definition "example.tool.increment" run)])] $ \(reserved,declarations)->do
    reached<-newIORef False
    rejected<-tryIOError (Tool.withTools reserved declarations (\_->writeIORef reached True))
    used<-readIORef reached
    check "reserved names, overlapping names and command IDs fail before exposure" (isLeft rejected && not used)
  let malformed=Tool.Tool "malformed" True ((definition "example.tool.malformed" run)
        {commandInput=input {codecSchema=object ["type" .= ("object"::T.Text)]}})
  invalid<-tryIOError (Tool.withTools [] [malformed] (\_->error "invalid schema exposed"))
  check "an input schema must declare its strict fields" (isLeft invalid)
  let huge=Tool.Tool "large" False (definition "example.tool.large" (\(_::Int) _->pure (Right (object ["text" .= T.replicate (4*1024*1024+1) "x"]))))
  large<-Tool.withTools [] [huge] $ \tools->Tool.callTool tools 0 "large" arguments
  check "oversized result fails without a truncated success" (isLeft large)
