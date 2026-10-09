{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.ProjectBrowser
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Asynchronous read-only browser for an existing Cabal plan.
--
-- A request token owns the loading dialog. Completed reads are adopted only while
-- that dialog, project root and privacy paths still match; navigation uses a
-- bounded cached snapshot. Browsing never configures or builds the project.
module Hide.ProjectBrowser (withProjectBrowser, projectBrowserEffects, tickProjectBrowser) where

import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Exception (bracket, mask)
import Control.Monad (foldM)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.IORef
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Error (tryIOError)
import Hide.Build (resolveBuildRoot)
import Hide.CabalPlan (cabalPlan)
import Hide.Model

data Cached = Cached Int FilePath [FilePath] Value
data Browser = Browser Int (Maybe (Async (FilePath,[FilePath],Value))) (Maybe Cached)
type ProjectBrowserState = IORef Browser

-- | Scope the outstanding plan read and cancel it on exit.
withProjectBrowser :: (ProjectBrowserState -> IO a) -> IO a
withProjectBrowser=bracket (newIORef (Browser 0 Nothing Nothing)) close
  where close ref=readIORef ref >>= \(Browser _ worker _)->mapM_ cancel worker

-- | Handle project-browser effects and delegate all others to the supplied interpreter.
projectBrowserEffects :: ProjectBrowserState -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
projectBrowserEffects ref fallback=foldM step . (False,)
  where
    step result@(True,_) _=pure result
    step (_,desktop) (ProjectRequest action)=(False,) <$> request action desktop
    step (_,desktop) effect=fallback desktop [effect]
    request LoadProject desktop=mask $ \restore -> do
      Browser token previous _<-readIORef ref
      mapM_ cancel previous
      worker<-async $ restore $ do
        root<-resolveBuildRoot desktop
        value<-cabalPlan desktop root
        pure (root,guestPrivatePaths desktop,value)
      let next=token+1
      writeIORef ref (Browser next (Just worker) Nothing)
      pure desktop {dialog=Just (Dialog "Cabal project" (ProjectLoading next) [] 0 ["Cancel"] ["Reading the existing Cabal plan..."]),menu=Nothing,drag=Nothing,dragOriginal=Nothing}
    request action desktop=do
      Browser _ _ cached<-readIORef ref
      case cached of
        Just (Cached token root private value) | token==actionToken action -> do
          current<-tryIOError (resolveBuildRoot desktop)
          if current/=Right root || private/=guestPrivatePaths desktop
            then pure desktop {status="Project changed; reopen the project browser."}
            else pure $ case action of
              ProjectPage _ page -> chooser token page value desktop
              ProjectDetails _ index -> case drop (max 0 index) (components value) of
                selected:_ | index>=0 -> (addReadOnly "Cabal component" (details root value selected) desktop) {status="Cabal plan snapshot; reopen Project browser to choose another component."}
                _ -> desktop {status="Component no longer available; refresh the project browser."}
        _ -> pure desktop {status="Project browser expired; reopen it."}
    actionToken (ProjectPage token _)=token
    actionToken (ProjectDetails token _)=token
    actionToken LoadProject = -1

-- | Adopt a completed read only into its own still-current loading dialog.
-- A dismissed dialog or changed root/privacy policy invalidates the result.
tickProjectBrowser :: ProjectBrowserState -> Desktop -> IO Desktop
tickProjectBrowser ref desktop=do
  Browser token worker cached<-readIORef ref
  case worker of
    Nothing -> pure desktop
    Just running | (purpose <$> dialog desktop)/=Just (ProjectLoading token) -> do
      cancel running
      writeIORef ref (Browser token Nothing cached)
      pure desktop
    Just running -> poll running >>= \result->case result of
      Nothing -> pure desktop
      Just completed -> do
        writeIORef ref (Browser token Nothing Nothing)
        case completed of
          Left _ -> pure (message "Cabal project" ["Could not read the Cabal plan.","No build was started."] desktop)
          Right (root,private,value) -> do
            current<-tryIOError (resolveBuildRoot desktop)
            if current/=Right root || private/=guestPrivatePaths desktop
              then pure desktop {dialog=Nothing,status="Project changed; reopen the project browser."}
              else do
                writeIORef ref (Browser token Nothing (Just (Cached token root private value)))
                pure (chooser token 0 value desktop)

valueAt :: FromJSON a => Key -> Value -> Maybe a
valueAt key=parseMaybe (withObject "plan field" (.: key))
textAt :: Key -> Value -> Text
textAt key=fromMaybe "unknown" . valueAt key
listAt :: Key -> Value -> [Value]
listAt key=fromMaybe [] . valueAt key
components :: Value -> [Value]
components=listAt "localComponents"
unitFor :: Value -> Value -> Value
unitFor plan component=fromMaybe Null (find (\unit->valueAt "id" unit==(valueAt "unitId" component::Maybe Text) && valueAt "id" unit/=(Nothing::Maybe Text)) (listAt "units" plan))

chooser :: Int -> Int -> Value -> Desktop -> Desktop
chooser token requested value desktop
  | textAt "status" value/="available"=message "Cabal project" ["Cabal plan: "<>textAt "status" value<>".","Build with Cabal, then reopen Project browser.","Only dist-newstyle/cache/plan.json is read.","No build was started."] desktop
  | null entries=message "Cabal project" (summary value++["No local components are present in this plan."]) desktop
  | otherwise=desktop {dialog=Just (Dialog "Cabal project" (ProjectChoices token page)
      [ListBox ("Components — page "<>number (page+1)<>"/"<>number pages) labels 0] 0 ["Details","Prev","Next","Refresh","Cancel"] (summary value)),menu=Nothing}
  where
    entries=components value
    pages=max 1 ((length entries+31) `div` 32)
    page=max 0 (min (pages-1) requested)
    labels=[textAt "package" (unitFor value entry)<>" / "<>textAt "component" entry | entry<-take 32 (drop (page*32) entries)]

summary :: Value -> [Text]
summary value=
  [T.take 54 (textAt "compilerId" value<>" · "<>freshness<>" · "<>number (length (components value))<>" components")
  ,if valueAt "graphComplete" value==Just True then "Existing plan snapshot; freshness is not guaranteed." else "Incomplete plan snapshot: some units were omitted."]
  where freshness=maybe "unknown" (textAt "status") (valueAt "freshness" value)

number :: Int -> Text
number=T.pack . show

details :: FilePath -> Value -> Value -> Text
details root plan component=bounded $ T.unlines $
  ["Package: "<>textAt "package" unit<>" "<>textAt "version" unit
  ,"Component: "<>name,"Unit: "<>textAt "id" unit,"Project: "<>T.pack root
  ,"Source root: "<>textAt "sourceRoot" unit,"Compiler: "<>textAt "compilerId" plan
  ,"Plan modified: "<>textAt "modifiedAt" plan]++summary plan++
  ["","Library dependencies:" ]++dependencies "depends"++["","Build-tool dependencies:"]++dependencies "exeDepends"++
  ["","An unresolved unit may be absent from this bounded plan.","This view does not build, configure, or change the run target."]
  where
    unit=unitFor plan component
    name=textAt "component" component
    source=fromMaybe unit (find ((==Just name) . valueAt "name") (listAt "components" unit))
    dependencies key=case valueAt key source :: Maybe [Text] of
      Nothing -> ["  unknown (not reported)"]
      Just [] | key=="depends",valueAt "dependenciesKnown" source/=Just True -> ["  unknown (not reported)"]
              | otherwise -> ["  (none reported)"]
      Just names -> map dependency names
    dependency ident=case find ((==Just ident) . valueAt "id") (listAt "units" plan) of
      Nothing -> "  "<>ident<>" [unresolved]"
      Just entry -> "  "<>ident<>" — "<>textAt "package" entry<>" "<>textAt "version" entry
    bounded text | T.length text<=131072=text
                 | otherwise=T.take 131000 text<>"\n[Details truncated at 128 Ki characters.]\n"
