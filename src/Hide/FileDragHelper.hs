{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Optional saved-file drag delegation. The existing Files action worker owns
-- this blocking operation; cancellation terminates the helper's process group.
-- Human admission and canonical privacy checks belong to the caller, before this
-- operation. No helper is needed for ordinary editing.
module Hide.FileDragHelper (runFileDragHelper) where

import Control.Exception (bracket,mask,onException)
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (canonicalizePath,doesFileExist,findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath (isAbsolute)
import System.IO.Error (tryIOError)
import System.Process
import Hide.Process (processCleanup)

-- | Run on a session-owned worker with an authorized canonical absolute file.
-- This exports its disk contents without saving a buffer or modifying the source.
-- The helper's window owns the gesture and cancellation. Exit success means the
-- helper closed, not that a receiving application accepted the file. Missing,
-- disabled or failing helpers return an error; asynchronous cancellation escapes.
runFileDragHelper :: FilePath -> IO (Either Text ())
runFileDragHelper path=do
  result<-tryIOError $ do
    resolved<-canonicalizePath path
    unless (isAbsolute path && resolved==path)
      (ioError (userError "File drag target changed; choose the file again."))
    exists<-doesFileExist path
    unless exists (ioError (userError "File drag needs an existing saved file."))
    configured<-lookupEnv "THC_EDIT_FILE_DRAG_HELPER"
    helper<-case configured of
      Just ""->pure (Left "File drag helper is disabled.")
      Just name->maybe (Left "Configured file drag helper is unavailable.") Right <$> findExecutable name
      Nothing->discover ["ripdrag","dragon-drop"]
    case helper of
      Left err->pure (Left err)
      Right executable->do
        code<-run executable path
        pure $ case code of
          ExitSuccess->Right ()
          ExitFailure value->Left ("File drag helper failed (exit "<>T.pack (show value)<>").")
  pure (either (Left . T.pack . show) id result)
  where
    discover []=pure (Left "File drag helper unavailable. Install ripdrag or dragon-drop, or set THC_EDIT_FILE_DRAG_HELPER.")
    discover (name:names)=findExecutable name >>= maybe (discover names) (pure . Right)

run :: FilePath -> FilePath -> IO ExitCode
run executable path=mask $ \restore->bracket
  (do
    -- dragon has no -- terminator. A canonical absolute path cannot be an option.
    (_,_,_,child)<-createProcess (proc executable ["--and-exit",path])
      {std_in=NoStream,std_out=NoStream,std_err=NoStream,close_fds=True,create_group=True}
    stop<-processCleanup child `onException` terminateProcess child
    pure (child,stop))
  snd
  (\(child,_)->restore (waitForProcess child))
