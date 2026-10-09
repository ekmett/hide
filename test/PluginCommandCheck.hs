{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : PluginCommandCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module PluginCommandCheck (checks) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (AsyncException(UserInterrupt), evaluate, throwIO, try)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
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
  putStrLn "plugin command checks passed"
