{-# LANGUAGE OverloadedStrings #-}
-- | Session-owned registration and MCP adaptation for offline documentation.
--
-- The initial call captures a corpus resolver and returns a deferred action.
-- Resolution, codecs and reads run after the desktop lock is released. Registering
-- a command does not expose a tool: App retains the explicit permission wrapper.
module Hide.DocsMCP (docsTools, docsToolNames, DocsCommands, withDocsCommands, docsTool, readDocs) where

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
import Hide.Plugin.Command

data DocsCommands = DocsCommands (Registry DocsContext) (Command DocsContext ReadArguments ReadPage)

-- | Keep the registration alive for the editor session; deferred calls after
-- shutdown fail admission rather than using a replacement command by name.
withDocsCommands :: (DocsCommands -> IO a) -> IO a
withDocsCommands use=withRegistry $ \registry->do
  registered<-registerCommand registry readCommand
  case registered of
    Left err->ioError (userError (show err))
    Right command->use (DocsCommands registry command)

-- | Typed session reader for first-party consumers. The immutable resolver is
-- captured by the caller; filesystem work runs on that caller's worker.
readDocs :: DocsCommands -> DocsContext -> ReadArguments -> IO (Either CommandError ReadPage)
readDocs (DocsCommands registry command)=invoke registry command

-- | Capture a request without filesystem IO. Run the continuation on a worker.
docsTool :: DocsCommands -> Desktop -> Text -> Value -> IO (Desktop,IO (Either Text Value))
docsTool (DocsCommands registry reference) desktop name arguments=pure (desktop,
  if name=="docs_read" then do
    reply<-invokeJSON registry (commandRef reference) (corpusRoot desktop) arguments
    pure (either (Left . commandError) Right reply)
  else prepareDocs (corpusRoot desktop) name arguments)
  where
    commandError (CommandRejected message)=message
    commandError (InvalidArguments message)=message
    commandError (CommandFailed message)=message
    commandError err=T.pack (show err)

corpusRoot :: Desktop -> Text -> IO FilePath
corpusRoot _ "editor"=getDataFileName "README.md" >>= canonicalizePath . takeDirectory
corpusRoot desktop _=do
  directory<-getXdgDirectory XdgConfig "thc-edit"
  project<-Build.resolveBuildRoot desktop
  config<-Build.loadBuildConfig directory project
  let configured=T.unpack (T.strip (Build.buildTHCRoot config))
  when (null configured) (ioError (userError "Compiler docs are unavailable: set THC_ROOT or the build target's THC root."))
  root<-canonicalizePath configured
  exists<-doesDirectoryExist root
  unless exists (ioError (userError "Configured THC root does not exist."))
  pure root
