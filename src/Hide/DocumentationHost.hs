{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.DocumentationHost
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Session-scoped documentation capabilities. The host captures only selected
-- path metadata before returning worker actions; filesystem discovery never
-- holds the UI lock. Retirement applies to read, list and search alike, and
-- Help uses the same read command as the linked plugin.
module Hide.DocumentationHost (DocsCommands, withDocsCommands, docsServices, captureDocsContext, readDocs) where

import Control.Exception (evaluate)
import Control.Monad (unless, when)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Paths_hide (getDataFileName)
import System.Directory
import System.FilePath (takeDirectory)
import qualified Hide.Build as Build
import Hide.Model (Desktop)
import Hide.Documentation
import Hide.Plugin.Documentation
import Hide.Plugin.Command

data DocsCommands = DocsCommands (Registry DocsContext)
  (Command DocsContext ReadArguments ReadPage)
  (Command DocsContext ListArguments Value)
  (Command DocsContext SearchArguments Value)

-- | All registrations retire with the editor session. Already admitted work
-- may finish; deferred invocations cannot resolve a replacement by name.
withDocsCommands :: (DocsCommands -> IO a) -> IO a
withDocsCommands use=withRegistry $ \registry->do
  readRef<-registerCommand registry readCommand >>= registered
  listRef<-registerCommand registry listCommand >>= registered
  searchRef<-registerCommand registry searchCommand >>= registered
  use (DocsCommands registry readRef listRef searchRef)
  where registered=either (ioError . userError . show) pure

-- | Native Help and plugin reads share this exact registration.
readDocs :: DocsCommands -> DocsContext -> ReadArguments -> IO (Either CommandError ReadPage)
readDocs (DocsCommands registry command _ _)=invoke registry command

-- | Closed capabilities expose bounded documentation operations, never corpus
-- roots or arbitrary paths. Invoke on the caller's worker after policy admission.
docsServices :: DocsCommands -> DocsContext -> DocsServices
docsServices commands@(DocsCommands registry _ listRef searchRef) resolve=DocsServices
  { docsRead=readDocs commands resolve
  , docsList=invoke registry listRef resolve
  , docsSearch=invoke registry searchRef resolve
  }

-- | Capture source selection before resolving configuration or directories, without
-- retaining the Desktop, its buffers or undo histories in a worker closure.
captureDocsContext :: Desktop -> IO DocsContext
captureDocsContext desktop=do
  start<-evaluate (Build.buildStartDirectory desktop)
  pure (corpusRoot start)

corpusRoot :: FilePath -> Text -> IO FilePath
corpusRoot _ "editor"=getDataFileName "README.md" >>= canonicalizePath . takeDirectory
corpusRoot start _=do
  directory<-getXdgDirectory XdgConfig "thc-edit"
  project<-Build.resolveBuildRootFrom start
  config<-Build.loadBuildConfig directory project
  let configured=T.unpack (T.strip (Build.buildTHCRoot config))
  when (null configured) (ioError (userError "Compiler docs are unavailable: set THC_ROOT or the build target's THC root."))
  root<-canonicalizePath configured
  exists<-doesDirectoryExist root
  unless exists (ioError (userError "Configured THC root does not exist."))
  pure root
