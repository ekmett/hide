-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
-- |
-- Module      : Hide.Plugin.Menu
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : ExistentialQuantification, OverloadedStrings
--
-- Session-scoped menu contributions to named slots and groups.
--
-- Snapshots contain only bounded metadata and opaque registration identities.
-- Typed arguments are retained with their command, never recovered from later
-- focus. Invocation and result preparation run on the caller's worker. Hosts
-- check currentness again before adopting replies and supply their own context,
-- policy and presentation adapter; this module has no mutable desktop capability.
module Hide.Plugin.Menu
  ( Menus, MenuRef, MenuItem(..), MenuDef(..), MenuAction, MenuError(..), MenuOrigin(..), MenuPublisher(..)
  , withMenus, menuAction, mapMenu, contributeMenu, retireMenu, menuSnapshot, menuMetadata
  , menuName, menuEpoch, menuGeneration, menuCurrent, invokeMenu
  ) where

import Control.Concurrent.MVar
import Control.DeepSeq (force)
import Control.Exception (bracket, evaluate)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (Unique, newUnique)
import Hide.Plugin.Identity (randomIdentity)
import Hide.Plugin.Command

-- | Origin is supplied by host dispatch, never frontend JSON.
data MenuOrigin = HumanMenu | AgentMenu deriving (Eq,Show)

-- | Exact entry lifetime. Names may be reused, identities never are.
data MenuRef = MenuRef Unique Text Integer Text deriving Eq
instance Show MenuRef where
  show reference="MenuRef "++show (menuName reference)++" "++show (menuGeneration reference)
menuName :: MenuRef -> Text
menuName (MenuRef _ _ _ name)=name
menuEpoch :: MenuRef -> Text
menuEpoch (MenuRef _ epoch _ _)=epoch
menuGeneration :: MenuRef -> Integer
menuGeneration (MenuRef _ _ generation _)=generation

-- | Pure prepared frontend metadata. Named groups sort lexically, then order and
-- entry ID break ties. Availability is a prepared host fact, not plugin IO.
data MenuItem = MenuItem
  { menuReference :: MenuRef, menuSlot :: Text, menuGroup :: Text
  , menuOrder :: Int, menuTitle :: Text, menuKey :: Text, menuAgentAllowed :: Bool
  } deriving (Eq,Show)

-- | Host-chosen argument projection and reply adapter, evaluated with typed
-- command work off the UI lock. Projection reads only captured immutable context.
data MenuAction context reply = forall c a b. MenuAction
  (Registry c) (Command c a b) (context -> c) (c -> Either Text a) (context -> b -> IO reply)
menuAction :: Registry context -> Command context a b -> (context -> Either Text a) -> (context -> b -> IO reply) -> MenuAction context reply
menuAction registry command=MenuAction registry command id

-- | Ordered session publication supplied by the host. Calls may block their
-- registration worker; never publish or withdraw beneath the UI lock. Scope
-- owners withdraw exact refs before their registry closes. Closing the host
-- rejects waiting and retained calls without redirecting a reused name.
data MenuPublisher c r = MenuPublisher
  { publishMenu :: MenuDef c r -> IO (Either MenuError MenuRef)
  , withdrawMenu :: MenuRef -> IO ()
  }

data MenuDef context reply = MenuDef
  { menuId :: Text, contributionSlot :: Text, contributionGroup :: Text
  , contributionOrder :: Int, contributionTitle :: Text, contributionKey :: Text
  , contributionAgentAllowed :: Bool, contributionAction :: MenuAction context reply
  }

-- | Select immutable invocation context and adapt a worker reply while retaining
-- the exact command and registration lifetime. Both maps run on the invoking
-- worker, never during painting/adoption. Identity and composition obey:
-- @mapMenu id (\_ -> pure) d ≡ d@ (observationally), and successive context maps
-- compose without registering or invoking another command.
mapMenu :: (c -> d) -> (c -> s -> IO r) -> MenuDef d s -> MenuDef c r
mapMenu project prepare definition=definition {contributionAction=case contributionAction definition of
  MenuAction registry command capture arguments reply->MenuAction registry command (capture . project) arguments
    (\context value->reply (project context) value >>= prepare context)}
data MenuError = MenusClosed | InvalidMenu Text | DuplicateMenu Text | UnknownMenu Text
  | UnknownSlot Text | MenuLimit
  | StaleMenu Text | MenuCommandError CommandError deriving (Eq,Show)
data Entry context reply = Entry MenuItem (MenuAction context reply)
data State context reply = State Bool Integer (M.Map Text (Entry context reply))
data Menus context reply = Menus Unique Text [Text] (MVar (State context reply))

-- | Closing removes all contributions and refuses escaped references.
withMenus :: [Text] -> (Menus context reply -> IO a) -> IO a
withMenus slots=bracket (Menus <$> newUnique <*> (T.pack <$> randomIdentity) <*> pure slots <*> newMVar (State False 0 M.empty)) close
  where close (Menus _ _ _ state)=modifyMVar_ state $ \(State _ generation _)->pure (State True generation M.empty)

-- | Registration checks the retained command lifetime, without invoking it.
-- Withdrawal of its command also makes this entry unavailable.
contributeMenu :: Menus context reply -> MenuDef context reply -> IO (Either MenuError MenuRef)
contributeMenu (Menus owner epoch slots state) definition
  | contributionSlot definition `notElem` slots=pure (Left (UnknownSlot (contributionSlot definition)))
  | not (valid definition)=pure (Left (InvalidMenu name))
  | otherwise=do
      live<-actionCurrent (contributionAction definition)
      if not live then pure (Left (StaleMenu name)) else modifyMVar state $ \current@(State closed generation entries)->
        if closed then pure (current,Left MenusClosed)
        else if M.size entries>=256 then pure (current,Left MenuLimit)
        else if M.member name entries then pure (current,Left (DuplicateMenu name))
        else let next=generation+1
                 reference=MenuRef owner epoch next name
                 item=MenuItem reference (contributionSlot definition) (contributionGroup definition)
                   (contributionOrder definition) (contributionTitle definition) (contributionKey definition) (contributionAgentAllowed definition)
             in pure (State False next (M.insert name (Entry item (contributionAction definition)) entries),Right reference)
  where
    name=menuId definition
    valid d=validCommandName (menuId d) && all bounded [menuId d,contributionSlot d,contributionGroup d,contributionTitle d] &&
      T.length (contributionKey d)<=32 && T.all (>= ' ') (contributionKey d)
    bounded value=not (T.null value) && T.length value<=128 && T.all (>= ' ') value

entry :: Menus context reply -> MenuRef -> IO (Either MenuError (Entry context reply))
entry (Menus owner epoch _ state) (MenuRef ident registeredEpoch generation name)=withMVar state $ \(State closed _ entries)->pure $
  if closed then Left MenusClosed else if ident/=owner || registeredEpoch/=epoch then Left (StaleMenu name) else
  case M.lookup name entries of
    Nothing->Left (UnknownMenu name)
    Just item@(Entry metadata _) | menuGeneration (menuReference metadata)==generation -> Right item
    _ -> Left (StaleMenu name)

-- | Retire an exact contribution. Queued calls and late replies retain this
-- lifetime and cannot adopt into a replacement entry with the same name.
retireMenu :: Menus context reply -> MenuRef -> IO (Either MenuError ())
retireMenu menus@(Menus _ _ _ state) reference=do
  found<-entry menus reference
  case found of
    Left err->pure (Left err)
    Right _->modifyMVar state $ \current@(State closed generation entries)->
      case M.lookup (menuName reference) entries of
        Just (Entry item _) | not closed && menuReference item==reference ->
          pure (State False generation (M.delete (menuName reference) entries),Right ())
        _->pure (current,Left (StaleMenu (menuName reference)))

actionCurrent :: MenuAction context reply -> IO Bool
actionCurrent (MenuAction registry command _ _ _)=commandCurrent registry (commandRef command)

-- | Currentness checks never call extension code. Use immediately before host
-- adoption as well as admission; already-running command IO may finish retired.
menuCurrent :: Menus context reply -> MenuRef -> IO Bool
menuCurrent menus reference=entry menus reference >>= either (const (pure False)) (\(Entry _ action)->actionCurrent action)

-- | Prepare one exact live contribution for publication outside the session
-- lock. An unrelated unprepared registration cannot poison this delta.
menuMetadata :: Menus context reply -> MenuRef -> IO (Either MenuError MenuItem)
menuMetadata menus reference=do
  found<-entry menus reference
  case found of
    Left err->pure (Left err)
    Right (Entry item action)->do
      live<-actionCurrent action
      if not live then pure (Left (StaleMenu (menuName reference))) else Right <$> prepareMetadata item

prepareMetadata :: MenuItem -> IO MenuItem
prepareMetadata item=do
  let ref=menuReference item
  _<-evaluate (force (menuName ref,menuEpoch ref,menuGeneration ref,menuSlot item,menuGroup item,menuOrder item,menuTitle item,menuKey item,menuAgentAllowed item))
  pure item

-- | Snapshot live metadata in deterministic slot/group/order/ID order. Preparing
-- snapshots belongs to the registration owner, never the per-frame render path.
menuSnapshot :: Menus context reply -> IO [MenuItem]
menuSnapshot (Menus _ _ _ state)=do
  entries<-withMVar state $ \(State _ _ items)->pure (M.elems items)
  alive<-mapM (\(Entry item action)->do live<-actionCurrent action; pure [item | live]) entries
  let metadata=concat alive
  -- Force only the bounded frontend fields on the registration owner. A lazy
  -- extension metadata expression must never migrate into painting/admission.
  prepared<-mapM prepareMetadata metadata
  pure (sortOn (\item->(menuSlot item,menuGroup item,menuOrder item,menuName (menuReference item))) prepared)

-- | Project captured typed arguments and invoke on the caller's worker. The host owns reply
-- preparation failures/cancellation and must check menuCurrent before adoption.
invokeMenu :: Menus context reply -> MenuRef -> context -> IO (Either MenuError reply)
invokeMenu menus reference context=do
  found<-entry menus reference
  case found of
    Left err->pure (Left err)
    Right (Entry _ (MenuAction registry command capture arguments prepare))->do
      let captured=capture context
      result<-case arguments captured of
        Left err->pure (Left (CommandRejected err))
        Right value->invoke registry command captured value
      case result of
        Left err->pure (Left (MenuCommandError err))
        Right value->Right <$> prepare context value
