{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : SidebarFixture
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module SidebarFixture (sidebarFixture) where
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Hide.Plugin.Command
import qualified Hide.Plugin.Tree as P
import Hide.Sidebar
import Hide.Model

-- Frozen prepared geometry for renderer/privacy checks only. No action is
-- invoked from this snapshot; live routing/lifetimes use SidebarCheck's host.
sidebarFixture :: FilePath -> [(T.Text,FilePath)] -> Desktop -> IO Desktop
sidebarFixture root entries d=withRegistry $ \registry->do
  let ident text=either (error . T.unpack) id (P.nodeId text)
      info=P.NodeInfo (ident "root") "Files" "" True (Just root)
      nodes=[P.NodeDef (P.NodeInfo (ident (T.pack (show i))) name "📄" False (Just path)) Nothing [] | (i,(name,path))<-zip [0::Int ..] entries]
  provider<-P.registerTree registry "test.tree.fixture" (P.NodeDef info Nothing []) (\() _->pure (Right (P.NodePage nodes Nothing))) >>= either (error.show) pure
  let tree=addRoot (P.treeReference provider) info Nothing [] (emptySidebar root 30 False)
      key=NodeKey (P.treeReference provider) (P.infoId info)
      (loading,pending)=requestChildren (nodeHit key (treeNodes tree M.! key)) Nothing tree
      request=maybe (error "sidebar fixture child request missing") id pending
  page<-P.loadChildren provider () (P.ChildRequest (P.infoId info) Nothing) >>= either (error.show) pure
  loaded<-either (error.T.unpack) pure (adoptPage request [(P.nodeInfo node,Nothing,[]) | node<-P.pageNodes page] Nothing loading)
  prepared<-prepareProjection loaded
  pure (installSidebar (adoptProjection prepared loaded) d)
