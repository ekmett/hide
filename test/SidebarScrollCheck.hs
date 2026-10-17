-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : SidebarScrollCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- A long Files tree pushes later sections (Sessions, Agents, the package) below
-- the viewport. The sidebar must say so, offer a way to reach them by mouse and
-- keyboard, and keep its scroll offset valid when rows disappear.

module SidebarScrollCheck (checks) where

import Control.Monad (unless)
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as Lazy
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Hide.Bindings (BindingPlatform(..))
import Hide.Commands (platformBindings)
import Hide.Plugin.Command
import qualified Hide.Plugin.Tree as P
import Hide.Render (snapshot)
import Hide.Sidebar
import Hide.Model

check :: String -> Bool -> IO ()
check label ok=unless ok (error ("sidebar scroll check failed: "<>label))

ident :: T.Text -> P.NodeId
ident=either (error . T.unpack) id . P.nodeId

-- | Files (expanded, @n@ directories) followed by three collapsed sections.
fixture :: Int -> (Int,Int) -> IO Desktop
fixture n size=withRegistry $ \registry->do
  let register name label resource nodes=do
        let info=P.NodeInfo (ident name) label "" True resource
        provider<-P.registerTree registry ("test.scroll."<>name) (P.NodeDef info Nothing [])
          (\() _->pure (Right (P.NodePage nodes Nothing))) >>= either (error.show) pure
        pure (provider,info)
      directories=[P.NodeDef (P.NodeInfo (ident (T.pack (show i))) (T.pack ("dir"<>show i)) "📁" False (Just ("/w/dir"<>show i))) Nothing [] | i<-[1..n]]
  (files,filesInfo)<-register "files" "Files" (Just "/w") directories
  (sessions,sessionsInfo)<-register "sessions" "Sessions" Nothing []
  (agents,agentsInfo)<-register "agents" "Agents" Nothing []
  (package,packageInfo)<-register "package" "longtree" (Just "/w/longtree.cabal") []
  let roots=foldl (\tree (provider,info)->addRoot (P.treeReference provider) info Nothing [] tree) (emptySidebar "/w" 30 False)
        [(files,filesInfo),(sessions,sessionsInfo),(agents,agentsInfo),(package,packageInfo)]
      key=NodeKey (P.treeReference files) (P.infoId filesInfo)
      (loading,pending)=requestChildren (nodeHit key (treeNodes roots M.! key)) Nothing roots
      request=maybe (error "files request missing") id pending
  page<-P.loadChildren files () (P.ChildRequest (P.infoId filesInfo) Nothing) >>= either (error.show) pure
  loaded<-either (error.T.unpack) pure (adoptPage request [(P.nodeInfo node,Nothing,[]) | node<-P.pageNodes page] Nothing loading)
  prepared<-prepareProjection loaded
  pure (installSidebar (adoptProjection prepared loaded) (initialDesktop size))

treeOf :: Desktop -> Sidebar
treeOf d=maybe (error "no sidebar") id (sideTree d)

mouse :: V.Button -> Int -> Int -> Desktop -> Desktop
mouse button x y=fst . handleEvent (V.EvMouseDown x y button [])

press :: V.Key -> [V.Modifier] -> Desktop -> Desktop
press key mods=fst . handleEvent (V.EvKey key mods)

visible :: T.Text -> Desktop -> Bool
visible text d=text `T.isInfixOf` snapshot d

checks :: IO ()
checks=do
  d<-fixture 60 (100,30)
  let tree=treeOf d
      rows=treeContentRows d
      limit=treeScrollLimit d tree
      focused=d {sideTree=Just tree {treeFocused=True},keyBindings=either (error . T.unpack) id (platformBindings [] TerminalPlatform M.empty)}
  check "unfocused overflow is visible" (visible "↓ 3 more sections" d && visible "▲" d && visible "▼" d)
  let jumped=mouse V.BLeft 5 1 d
  check "cue reaches later sections without stealing focus" (visible "▶ longtree" jumped && not (visible "more section" jumped) && not (treeFocused (treeOf jumped)))
  check "unfocused scrollbar pages" (treeScroll (treeOf (mouse V.BLeft 28 (2+rows-2) d))==rows)
  let down=press V.KDown [V.MCtrl] focused
      again=press V.KDown [V.MCtrl] down
  check "section keys reach the next header and scroll it into view" (treeSelected (treeOf down)==61 && treeSelected (treeOf again)==62 && visible "▶ Sessions" down)
  check "previous section selects its header" (treeSelected (treeOf (press V.KUp [V.MCtrl] again))==61)
  let atEnd=press V.KEnd [] focused
  check "End and Home reach the edges" (treeSelected (treeOf atEnd)==63 && treeScroll (treeOf atEnd)==limit && treeSelected (treeOf (press V.KHome [] atEnd))==0)
  let collapsed=press V.KLeft [] (atEnd {sideTree=Just (treeOf atEnd) {treeSelected=0}})
  check "collapse clamps the viewport" (treeScroll (treeOf collapsed)==0 && visible "longtree" collapsed)
  let grown=fst (handleEvent (V.EvResize 100 80) atEnd)
  check "resize clamps the viewport" (treeScroll (treeOf grown)==0)
  short<-fixture 3 (100,30)
  check "short trees need no overflow controls" (not (visible "more section" short) && not (visible "▲" short))
  -- The cue may inspect root metadata, but never needs the hidden child payloads.
  let lazyRows=Lazy.mapWithKey (\address row->if length address>1 && maybe False (>=rows) (M.lookupIndex address (treeRows tree)) then error "forced a hidden child row" else row) (treeRows tree)
  check "section cue does not traverse hidden child payloads" (visible "↓ 3 more sections" d {sideTree=Just tree {treeRows=lazyRows,treeScroll=0}})
