{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
-- | Session-scoped menu contributions to named slots and groups.
--
-- Snapshots contain only bounded metadata and opaque registration identities.
-- Typed arguments are retained with their command, never recovered from later
-- focus. Invocation and result preparation run on the caller's worker. Hosts
-- check currentness again before adopting replies and supply their own context,
-- policy and presentation adapter; this module has no mutable desktop capability.
module Hide.Plugin.Menu
  ( Menus, MenuRef, MenuItem(..), MenuDef(..), MenuAction, MenuError(..)
  , withMenus, menuAction, contributeMenu, retireMenu, menuSnapshot
  , menuName, menuGeneration, menuCurrent, invokeMenu
  ) where

import Control.Concurrent.MVar
import Control.Exception (bracket)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (Unique, newUnique)
import Hide.Plugin.Command

-- | Exact entry lifetime. Names may be reused, identities never are.
data MenuRef = MenuRef Unique Integer Text deriving Eq
instance Show MenuRef where
  show reference="MenuRef "++show (menuName reference)++" "++show (menuGeneration reference)
menuName :: MenuRef -> Text
menuName (MenuRef _ _ name)=name
menuGeneration :: MenuRef -> Integer
menuGeneration (MenuRef _ generation _)=generation

-- | Pure prepared frontend metadata. Named groups sort lexically, then order and
-- entry ID break ties. Availability is a prepared host fact, not plugin IO.
data MenuItem = MenuItem
  { menuReference :: MenuRef, menuSlot :: Text, menuGroup :: Text
  , menuOrder :: Int, menuTitle :: Text, menuKey :: Text, menuGuestAllowed :: Bool
  } deriving (Eq,Show)

-- | Host-chosen typed reply adapter, evaluated with command work off the UI lock.
data MenuAction context reply = forall a b. MenuAction
  (Registry context) (Command context a b) a (b -> IO reply)
menuAction :: Registry context -> Command context a b -> a -> (b -> IO reply) -> MenuAction context reply
menuAction=MenuAction

data MenuDef context reply = MenuDef
  { menuId :: Text, contributionSlot :: Text, contributionGroup :: Text
  , contributionOrder :: Int, contributionTitle :: Text, contributionKey :: Text
  , contributionGuestAllowed :: Bool, contributionAction :: MenuAction context reply
  }
data MenuError = MenusClosed | InvalidMenu Text | DuplicateMenu Text | UnknownMenu Text
  | StaleMenu Text | MenuCommandError CommandError deriving (Eq,Show)
data Entry context reply = Entry MenuItem (MenuAction context reply)
data State context reply = State Bool Integer (M.Map Text (Entry context reply))
data Menus context reply = Menus Unique (MVar (State context reply))

-- | Closing removes all contributions and refuses escaped references.
withMenus :: (Menus context reply -> IO a) -> IO a
withMenus=bracket (Menus <$> newUnique <*> newMVar (State False 0 M.empty)) close
  where close (Menus _ state)=modifyMVar_ state $ \(State _ generation _)->pure (State True generation M.empty)

-- | Registration checks the retained command lifetime, without invoking it.
-- Withdrawal of its command also makes this entry unavailable.
contributeMenu :: Menus context reply -> MenuDef context reply -> IO (Either MenuError MenuRef)
contributeMenu (Menus owner state) definition
  | not (valid definition)=pure (Left (InvalidMenu name))
  | otherwise=do
      live<-actionCurrent (contributionAction definition)
      if not live then pure (Left (StaleMenu name)) else modifyMVar state $ \current@(State closed generation entries)->
        if closed then pure (current,Left MenusClosed)
        else if M.member name entries then pure (current,Left (DuplicateMenu name))
        else let next=generation+1
                 reference=MenuRef owner next name
                 item=MenuItem reference (contributionSlot definition) (contributionGroup definition)
                   (contributionOrder definition) (contributionTitle definition) (contributionKey definition) (contributionGuestAllowed definition)
             in pure (State False next (M.insert name (Entry item (contributionAction definition)) entries),Right reference)
  where
    name=menuId definition
    valid d=all bounded [menuId d,contributionSlot d,contributionGroup d,contributionTitle d] &&
      T.length (contributionKey d)<=32 && T.all (>= ' ') (contributionKey d)
    bounded value=not (T.null value) && T.length value<=128 && T.all (>= ' ') value

entry :: Menus context reply -> MenuRef -> IO (Either MenuError (Entry context reply))
entry (Menus owner state) (MenuRef ident generation name)=withMVar state $ \(State closed _ entries)->pure $
  if closed then Left MenusClosed else if ident/=owner then Left (StaleMenu name) else
  case M.lookup name entries of
    Nothing->Left (UnknownMenu name)
    Just item@(Entry metadata _) | menuGeneration (menuReference metadata)==generation -> Right item
    _ -> Left (StaleMenu name)

-- | Retire an exact contribution. Queued calls and late replies retain this
-- lifetime and cannot adopt into a replacement entry with the same name.
retireMenu :: Menus context reply -> MenuRef -> IO (Either MenuError ())
retireMenu menus@(Menus _ state) reference=do
  found<-entry menus reference
  case found of
    Left err->pure (Left err)
    Right _->modifyMVar state $ \current@(State closed generation entries)->
      case M.lookup (menuName reference) entries of
        Just (Entry item _) | not closed && menuReference item==reference ->
          pure (State False generation (M.delete (menuName reference) entries),Right ())
        _->pure (current,Left (StaleMenu (menuName reference)))

actionCurrent :: MenuAction context reply -> IO Bool
actionCurrent (MenuAction registry command _ _)=commandCurrent registry (commandRef command)

-- | Currentness checks never call extension code. Use immediately before host
-- adoption as well as admission; already-running command IO may finish retired.
menuCurrent :: Menus context reply -> MenuRef -> IO Bool
menuCurrent menus reference=entry menus reference >>= either (const (pure False)) (\(Entry _ action)->actionCurrent action)

-- | Snapshot live metadata in deterministic slot/group/order/ID order. Preparing
-- snapshots belongs to the registration owner, never the per-frame render path.
menuSnapshot :: Menus context reply -> IO [MenuItem]
menuSnapshot (Menus _ state)=do
  entries<-withMVar state $ \(State _ _ items)->pure (M.elems items)
  alive<-mapM (\(Entry item action)->do live<-actionCurrent action; pure [item | live]) entries
  pure (sortOn (\item->(menuSlot item,menuGroup item,menuOrder item,menuName (menuReference item))) (concat alive))

-- | Invoke retained typed arguments on the caller's worker. The host owns reply
-- preparation failures/cancellation and must check menuCurrent before adoption.
invokeMenu :: Menus context reply -> MenuRef -> context -> IO (Either MenuError reply)
invokeMenu menus reference context=do
  found<-entry menus reference
  case found of
    Left err->pure (Left err)
    Right (Entry _ (MenuAction registry command arguments prepare))->do
      result<-invoke registry command context arguments
      case result of
        Left err->pure (Left (MenuCommandError err))
        Right value->Right <$> prepare value
