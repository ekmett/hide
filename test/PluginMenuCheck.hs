{-# LANGUAGE OverloadedStrings #-}
module PluginMenuCheck (checks) where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar
import Control.Exception (ErrorCall,try)
import Control.Monad (unless,void)
import Data.Aeson
import Data.IORef
import System.Timeout (timeout)
import Hide.Plugin.Command
import Hide.Plugin.Menu
import Hide.Plugin.ConversationSession
import qualified Hide.Plugin.Form as Form
import qualified Hide.Plugin.Sidebar as Sidebar
import qualified Hide.ConversationMenus as ConversationMenus

data SessionReply = SessionForm (Form.PreparedForm (MenuOrigin,Int) SessionReply)
  | SessionRequest (ConversationRequest Int)

checks :: IO ()
checks=do
  escaped<-newIORef (pure True)
  withRegistry $ \registry->withMenus ["help"] $ \menus->do
    let codec=Codec Null Right id
        definition=CommandDef "example.independent" "Independent extension" codec codec (\() value->pure (Right value))
        right :: Show e => Either e a -> a
        right=either (error . show) id
        check label condition=unless condition (error label)
    command<-right <$> registerCommand registry definition
    let contribution ident group order=MenuDef ident "help" group order ident "" False (menuAction registry command (const (Right (String ident))) (\_ -> pure))
    first<-right <$> contributeMenu menus (contribution "example.z" "contents" 10)
    _<-right <$> contributeMenu menus (contribution "example.a" "contents" 10)
    before<-right <$> contributeMenu menus (contribution "example.before" "about" 100)
    duplicate<-contributeMenu menus (contribution "example.z" "about" 0)
    check "duplicate contribution cannot replace a live action" (duplicate==Left (DuplicateMenu "example.z"))
    unknownSlot<-contributeMenu menus ((contribution "example.unknown-slot" "contents" 0) {contributionSlot="missing"})
    check "unknown named slot fails instead of disappearing" (unknownSlot==Left (UnknownSlot "missing"))
    snapshot<-menuSnapshot menus
    check "named groups, order and ID determine entry order" (map (menuName . menuReference) snapshot==["example.before","example.a","example.z"])
    reply<-invokeMenu menus first ()
    check "independent declaration invokes retained typed arguments" (reply==Right (String "example.z"))
    _<-retireMenu menus first
    replacement<-right <$> contributeMenu menus (contribution "example.z" "contents" 10)
    old<-invokeMenu menus first ()
    current<-menuCurrent menus first
    check "retired contribution cannot redirect queued calls or late adoption" (old==Left (StaleMenu "example.z") && not current && menuGeneration first/=menuGeneration replacement)
    cross<-withMenus ["help"] $ \other->do
      separate<-right <$> contributeMenu other (contribution "example.z" "contents" 10)
      check "same ID and numeric generation in new scope have distinct wire epoch" (menuGeneration separate==menuGeneration first && menuEpoch separate/=menuEpoch first)
      invokeMenu other first ()
    check "contribution refs cannot cross session scopes" (case cross of Left (StaleMenu "example.z")->True; _->False)
    unknown<-invokeMenu menus first () -- remove replacement to exercise missing entry
    check "old lifetime remains stale while replacement exists" (unknown==Left (StaleMenu "example.z"))
    _<-retireMenu menus replacement
    missing<-invokeMenu menus replacement ()
    check "unknown entry fails explicitly" (missing==Left (UnknownMenu "example.z"))
    entered<-newEmptyMVar
    release<-newEmptyMVar
    slow<-right <$> registerCommand registry (CommandDef "example.slow" "Slow" codec codec (\() value->putMVar entered () >> takeMVar release >> pure (Right value)))
    pending<-right <$> contributeMenu menus (MenuDef "example.slow" "help" "contents" 20 "Slow" "" False (menuAction registry slow (const (Right Null)) (\_ -> pure)))
    worker<-async (invokeMenu menus pending ())
    began<-timeout 1000000 (takeMVar entered)
    check "slow handler admitted" (began==Just ())
    retired<-timeout 1000000 (retireMenu menus pending)
    check "menu handler does not hold contribution lock" (retired==Just (Right ()))
    putMVar release ()
    finished<-wait worker
    live<-menuCurrent menus pending
    check "retired in-flight result is ineligible for adoption" (finished==Right Null && not live)
    _<-retireCommand registry (commandRef command)
    unavailable<-menuSnapshot menus
    check "command retirement withdraws every dependent contribution" (null unavailable)
    staleCommand<-invokeMenu menus before ()
    check "retired command is refused by menu admission" (case staleCommand of Left (MenuCommandError StaleCommand{})->True; _->False)
    fresh<-right <$> registerCommand registry definition
    alive<-right <$> contributeMenu menus (MenuDef "example.escape" "help" "contents" 0 "Escape" "" False (menuAction registry fresh (const (Right Null)) (\_ -> pure)))
    withMenus ["help"] $ \prepared->do
      _<-right <$> contributeMenu prepared (MenuDef "example.lazy-metadata" "help" "contents" 0 "Lazy" "" (error "metadata was not prepared") (menuAction registry fresh (const (Right Null)) (\_ -> pure)))
      snapshotResult<-try (menuSnapshot prepared) :: IO (Either ErrorCall [MenuItem])
      check "metadata is fully evaluated before publication on its owner" (case snapshotResult of Left _->True; _->False)
    writeIORef escaped (menuCurrent menus alive)
  closed<-readIORef escaped >>= id
  unless (not closed) (error "closing menu scope invalidates escaped refs")
  withRegistry $ \registry->withMenus ["context.messages"] $ \menus->do
    calls<-newIORef (0::Int)
    let codec=Codec Null Right id
        definition=CommandDef "example.projected" "Projected" codec codec (\_ value->modifyIORef' calls (+1) >> pure (Right value))
        right :: Show e => Either e a -> a
        right=either (error . show) id
    command<-right <$> registerCommand registry definition
    reference<-right <$> contributeMenu menus (MenuDef "example.projected" "context.messages" "source" 0 "Projected" "" False
      (menuAction registry command (\context->Right (Number (fromIntegral (context::Int)))) (\_ -> pure)))
    projected<-invokeMenu menus reference 7
    unless (projected==Right (Number 7)) (error "typed menu arguments must come from captured host context")
    refused<-right <$> contributeMenu menus (MenuDef "example.refused-projection" "context.messages" "source" 1 "Refused" "" False
      (menuAction registry command (const (Left "No captured location")) (\_ -> pure)))
    refusal<-invokeMenu menus refused 8
    invoked<-readIORef calls
    unless (refusal==Left (MenuCommandError (CommandRejected "No captured location")) && invoked==1)
      (error "failed argument projection must not invoke the typed handler")
    withMenus ["context.messages"] $ \mapped->do
      let original=MenuDef "example.mapped" "context.messages" "source" 0 "Mapped" "" False
            (menuAction registry command (\context->Right (Number (fromIntegral context))) (\_ ->pure))
      identity<-right <$> contributeMenu mapped (mapMenu id (\_ ->pure) original)
      identically<-invokeMenu mapped identity 7
      unless (identically==projected) (error "menu identity mapping changed invocation")
      composed<-right <$> contributeMenu mapped ((mapMenu (+3) (\_ ->pure) (mapMenu (*2) (\_ ->pure) original)) {menuId="example.composed"})
      composition<-invokeMenu mapped composed 2
      unless (composition==Right (Number 10)) (error "menu context maps do not compose")
      _<-retireCommand registry (commandRef command)
      current<-menuCurrent mapped composed
      unless (not current) (error "mapped menu lost its original command lifetime")
  withMenus ["tools"] $ \menus->do
    let forms=Sidebar.Sidebar fst (const "") SessionForm (\_->pure ()) (\_->pure ()) (\_ _->pure ())
        publisher=MenuPublisher (contributeMenu menus) (void . retireMenu menus)
        captured (_,receipt)=Right (ConversationTarget receipt True "private-resume-id")
        check label condition=unless condition (error label)
    ConversationMenus.withConversationMenus forms publisher captured SessionRequest $ do
      metadata<-menuSnapshot menus
      let reference name=case [menuReference item | item<-metadata,menuName (menuReference item)==name] of
            ref:_->ref; _->error "Missing declared session menu"
      resumed<-async (invokeMenu menus (reference "hide.agents.resume") (HumanMenu,1)) >>= wait
      case resumed of
        Right (SessionForm prepared)->do
          let initial=case Form.formSpec prepared of Form.InputFormSpec _ _ value _->Just value; _->Nothing
          check "plugin Resume declares a private form and keeps the remembered ID"
            (Form.formDisclosure (Form.formReference prepared)==Form.PrivateForm && initial==Just "private-resume-id")
          invalid<-async (Form.invokeFormAction prepared (HumanMenu,2) (Form.TextValue "   ")) >>= wait
          check "plugin Resume rejects whitespace-only input on its worker" (case invalid of Left InvalidArguments{}->True; _->False)
          loaded<-async (Form.invokeFormAction prepared (HumanMenu,2) (Form.TextValue "  another-id  ")) >>= wait
          check "Resume trims input and retains the original receipt across a changed context"
            (case loaded of Right (SessionRequest (ResumeConversation 1 "another-id"))->True; _->False)
        _->error "Declared Resume did not prepare its form"
      refused<-async (invokeMenu menus (reference "hide.agents.new") (AgentMenu,1)) >>= wait
      check "plugin New requires the human independently of menu metadata" (case refused of Left (MenuCommandError CommandRejected{})->True; _->False)
  putStrLn "plugin menu checks passed"
