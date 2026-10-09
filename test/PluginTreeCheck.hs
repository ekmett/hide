{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : PluginTreeCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module PluginTreeCheck (checks) where
import Control.Monad (unless)
import Data.Aeson (Value(Null))
import qualified Data.Text as T
import Hide.Plugin.Command
import Hide.Plugin.Tree

checks :: IO ()
checks=withRegistry $ \registry->do
  let check label ok=unless ok (error label)
      ident=either (error . show) id . nodeId
      root=NodeDef (NodeInfo (ident "root") "Independent" "" True Nothing) Nothing []
      child=NodeDef (NodeInfo (ident "child") "Child" "" False Nothing) Nothing []
      handler _ _=pure (Right (NodePage [child] Nothing))
      right=either (error . show) id
  provider<-right <$> registerTree registry "example.sidebar" root handler
  check "provider wire scope is opaque bounded lowercase hexadecimal"
    (let identity=treeIdentity (treeReference provider) in T.length identity==48 && T.all (`elem` ("0123456789abcdef"::String)) identity)
  duplicate<-registerTree registry "example.sidebar" root handler
  check "duplicate tree provider registration fails" (case duplicate of Left DuplicateCommand{}->True; _->False)
  loaded<-right <$> loadChildren provider () (ChildRequest (ident "root") Nothing)
  check "independent provider prepares ordinary bounded child metadata" (map (infoLabel . nodeInfo) (pageNodes loaded)==["Child"])
  _<-retireTree provider
  stale<-loadChildren provider () (ChildRequest (ident "root") Nothing)
  check "retired provider refuses queued loading" (case stale of Left StaleCommand{}->True; _->False)
  replacement<-right <$> registerTree registry "example.sidebar" root handler
  check "provider name reuse retains a new scoped identity"
    (treeReference replacement/=treeReference provider && treeIdentity (treeReference replacement)/=treeIdentity (treeReference provider))
  _<-withRegistry $ \other->do
    independent<-right <$> registerTree other "example.sidebar" root handler
    check "another registry cannot reuse the same provider identity"
      (treeReference independent/=treeReference replacement && treeIdentity (treeReference independent)/=treeIdentity (treeReference replacement))
  overflow<-right <$> registerTree registry "example.overflow" root (\_ _->pure (Right (NodePage (repeat child) Nothing)))
  refused<-loadChildren overflow () (ChildRequest (ident "root") Nothing)
  check "an infinite child page is rejected at its fixed bound" (case refused of Left CommandRejected{}->True; _->False)
  duplicates<-right <$> registerTree registry "example.duplicates" root (\_ _->pure (Right (NodePage [child,child] Nothing)))
  bad<-loadChildren duplicates () (ChildRequest (ident "root") Nothing)
  check "repeated child identities are refused" (case bad of Left CommandRejected{}->True; _->False)
  let codec=Codec Null (const (Right ())) (const Null)
  action<-right <$> registerCommand registry (CommandDef "example.action" "Action" codec codec (\_ ()->pure (Right ())))
  let captured=treeAction registry action () (\_ ()->pure ("executed"::String))
  reply<-invokeTreeAction captured ()
  check "typed node action uses the shared command registry" (reply==Right "executed")
  _<-retireCommand registry (commandRef action)
  expired<-invokeTreeAction captured ()
  check "retired action retains its original command lifetime" (case expired of Left StaleCommand{}->True; _->False)
  putStrLn "typed tree provider checks passed"
