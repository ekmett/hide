{-# LANGUAGE OverloadedStrings #-}
-- | Session-owned live menu worker and Help contribution.
--
-- Only immutable context and exact registrations cross into the worker. Typed
-- command handlers, docs IO and Markdown preparation run there. Tick checks the
-- contribution/command lifetime and host modal/layout state before installing a
-- prepared document. No extension callback executes during admission or adoption.
module Hide.MenuCommands
  ( MenuHost, withMenuCommands, menuContributions, menuAgentReferences, retireMenuFromHost, menuEffects, tickMenus
  ) where

import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, writeTQueue, tryReadTQueue)
import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Exception (bracket, displayException, mask)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.IORef
import Data.List (find)
import qualified Data.Text as T
import Paths_hide (getDataFileName)
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory)
import Hide.DocsMCP (DocsCommands, readDocs)
import Hide.Documentation
import Hide.Links (LinkResult, applyLink, prepareMarkdown)
import Hide.Model hiding (menus)
import qualified Hide.Model as Model
import Hide.Plugin.Command
import qualified Hide.Plugin.Menu as Plugin

data Pending = Pending Plugin.MenuRef Plugin.MenuContext (Async (Either Plugin.MenuError LinkResult))
data MenuHost = MenuHost (Plugin.Menus Plugin.MenuContext LinkResult) [Plugin.MenuRef] (TQueue Plugin.MenuRef) (IORef (Maybe Pending))

-- | Keep the registry independent of frontend attachments. The host may add
-- linked extension declarations to menuContributions before taking its snapshot.
-- Once published, registrations/withdrawals belong to the session owner; use
-- retireMenuFromHost. Closing cancels and joins the worker before either registry
-- scope closes, so shutdown cannot race adoption or resurrect a document.
withMenuCommands :: DocsCommands -> (MenuHost -> IO a) -> IO a
withMenuCommands docs use=withRegistry $ \registry->Plugin.withMenus [T.toLower title | (title,_,_)<-Model.menus] $ \menus->do
  command<-either (ioError . userError . show) pure =<< registerCommand registry (helpCommand docs)
  reference<-either (ioError . userError . show) pure =<< Plugin.contributeMenu menus
    (Plugin.MenuDef "hide.help.contents" "help" "contents" 0 "Contents" "F1" True
      (Plugin.menuAction registry command () (\context (path,text)->prepareMarkdown (Plugin.invocationColumns context) path "" text)))
  bracket (MenuHost menus [reference] <$> newTQueueIO <*> newIORef Nothing) close use
  where close (MenuHost _ _ _ ref)=readIORef ref >>= mapM_ (\(Pending _ _ worker)->cancel worker)

menuContributions :: MenuHost -> Plugin.Menus Plugin.MenuContext LinkResult
menuContributions (MenuHost menus _ _ _)=menus

-- | Exact first-party refs allowed by host policy. Contribution metadata can
-- further restrict these; it cannot grant agent authority to new registrations.
menuAgentReferences :: MenuHost -> [Plugin.MenuRef]
menuAgentReferences (MenuHost _ permitted _ _)=permitted

-- | Queue retirement to the serialized session owner. Published contributions
-- must use this route, not retire the underlying menu/command from another
-- thread. The owner withdraws metadata before admitting/adopting further work.
retireMenuFromHost :: MenuHost -> Plugin.MenuRef -> IO ()
retireMenuFromHost (MenuHost _ _ withdrawals _)=atomically . writeTQueue withdrawals

withdrawMenus :: MenuHost -> Desktop -> IO Desktop
withdrawMenus host@(MenuHost menus _ withdrawals _) d=do
  pending<-atomically (tryReadTQueue withdrawals)
  case pending of
    Nothing->pure d
    Just reference->do
      _<-Plugin.retireMenu menus reference
      withdrawMenus host d {contributedMenus=filter ((/=reference) . Plugin.menuReference) (contributedMenus d),
        agentMenuRefs=filter (/=reference) (agentMenuRefs d)}

helpCommand :: DocsCommands -> CommandDef Plugin.MenuContext () (FilePath,T.Text)
helpCommand docs=CommandDef "hide.help.contents" "Help contents" unit output $ \_ ()->do
  path<-getDataFileName "README.md" >>= canonicalizePath
  let resolve _=pure (takeDirectory path)
      readAll start pieces=case readArguments "editor" "README.md" start 500 of
        Left err->pure (Left (CommandRejected err))
        Right arguments->do
          loaded<-readDocs docs resolve arguments
          case loaded of
            Left err->pure (Left err)
            Right page
              | pageTruncated page->pure (Left (CommandRejected "Help page exceeds the documentation read budget."))
              | pageHasMore page->readAll (start+pageCount page) (pageText page:pieces)
              | otherwise->pure (Right (path,T.intercalate "\n" (reverse (pageText page:pieces))))
  readAll 1 []
  where
    unit=Codec (object ["type" .= ("object"::T.Text),"additionalProperties" .= False])
      (\value->if value==object [] then Right () else Left "Help contents takes no arguments.") (const (object []))
    output=Codec (object ["type" .= ("object"::T.Text),"required" .= (["path","text"]::[T.Text]),
        "properties" .= object ["path" .= object ["type" .= ("string"::T.Text)],"text" .= object ["type" .= ("string"::T.Text)]]])
      (either (Left . T.pack) Right . parseEither (withObject "help contents" $ \o->(,) <$> o .: "path" <*> o .: "text"))
      (\(path,text)->object ["path" .= path,"text" .= text])

columns :: Desktop -> Int
columns d=max 20 (min 76 (fst (screenSize d)-treeWidthOf d-4))

-- | Admission performs only host policy/lifetime checks and starts one worker.
-- Busy calls fail explicitly rather than replacing another prepared result.
menuEffects :: MenuHost -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
menuEffects host@(MenuHost menus permitted _ ref) _ original [InvokeMenu reference origin]=mask $ \restore->do
  d<-withdrawMenus host original
  pending<-readIORef ref
  live<-Plugin.menuCurrent menus reference
  let item=find ((==reference) . Plugin.menuReference) (contributedMenus d)
      allowed=dialog d==Nothing && maybe False (\entry->origin==Plugin.HumanMenu || reference `elem` permitted && Plugin.menuAgentAllowed entry) item
  if not live || not allowed then pure (False,d {status="Menu action is stale or unavailable."}) else case pending of
    Just _->pure (False,d {status="A menu action is already running."})
    Nothing->do
      let context=Plugin.MenuContext (columns d) origin
      worker<-async (restore (Plugin.invokeMenu menus reference context))
      writeIORef ref (Just (Pending reference context worker))
      pure (False,d {status="Opening document…"})
menuEffects _ core d requests=core d requests

-- | Retirement also refuses late adoption. Modal/layout changes discard the
-- prepared result; no plugin code or whole-buffer comparison is needed.
tickMenus :: MenuHost -> Desktop -> IO Desktop
tickMenus host@(MenuHost menus _ _ ref) original=do
  d<-withdrawMenus host original
  pending<-readIORef ref
  case pending of
    Nothing->pure d
    Just (Pending reference context worker)->do
      completed<-poll worker
      case completed of
        Nothing->pure d
        Just result->do
          writeIORef ref Nothing
          live<-Plugin.menuCurrent menus reference
          let current=dialog d==Nothing && columns d==Plugin.invocationColumns context &&
                any ((==reference) . Plugin.menuReference) (contributedMenus d)
          pure $ if not live || not current then d {status="Menu result expired; invoke it again."} else case result of
            Left err->d {status="Menu action failed: "<>T.pack (displayException err)}
            Right (Left err)->d {status="Menu action failed: "<>T.pack (show err)}
            Right (Right prepared)->fst (applyLink prepared d)
