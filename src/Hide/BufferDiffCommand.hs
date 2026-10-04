{-# LANGUAGE OverloadedStrings #-}
-- | The live typed strict-diff command. Its context contains only a host-bound
-- editor, opaque target and exact read version. Permission waits, preparation,
-- editable approval and adoption belong to the existing Permissions owner;
-- command codecs and final JSON run on the invoking worker.
module Hide.BufferDiffCommand
  ( BufferDiffCommands, withBufferDiffCommands, bufferDiffCommand, bufferDiffTool ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Text as T
import Hide.Model (Desktop)
import Hide.WorkspaceFilesMCP (capturePatchRequest)
import Hide.Plugin.BufferHost (editorReference)
import qualified Hide.Plugin.Buffer as P
import Hide.Plugin.Command

data BufferDiffCommands = BufferDiffCommands
  (Registry (P.BufferEditor,P.BufferRef,P.ContentVersion))
  (Command (P.BufferEditor,P.BufferRef,P.ContentVersion) T.Text P.DiffResult)

-- | Session registration; retired command handles cannot start later requests.
-- The Permissions service owns cancellation of already accepted requests.
withBufferDiffCommands :: (BufferDiffCommands -> IO a) -> IO a
withBufferDiffCommands use=withRegistry $ \registry->do
  command<-registerCommand registry definition >>= either (ioError . userError . show) pure
  use (BufferDiffCommands registry command)

-- | Call on a worker. Typed handlers need no Desktop or reusable approval.
bufferDiffCommand :: BufferDiffCommands -> P.BufferEditor -> P.BufferRef -> P.ContentVersion -> T.Text -> IO (Either T.Text P.DiffResult)
bufferDiffCommand (BufferDiffCommands registry command) editor reference version patch=
  fmap (either (Left . message) Right) (invoke registry command (editor,reference,version) patch)
  where
    message (CommandRejected err)=err
    message (CommandFailed err)=err
    message err=T.pack (show err)

definition :: CommandDef (P.BufferEditor,P.BufferRef,P.ContentVersion) T.Text P.DiffResult
definition=CommandDef "hide.buffer.apply-diff" "Apply exact buffer diff" input output $ \(editor,reference,version) patch->
  fmap (either (Left . CommandRejected) Right) (P.applyBufferDiff editor reference version patch)
  where
    input=Codec (object ["type" .= ("string"::T.Text),"maxLength" .= (1048576::Int)])
      (either (Left . T.pack) Right . parseEither parseJSON) toJSON
    output=Codec (object ["type" .= ("object"::T.Text)]) (const (Left "Diff results are host-issued")) (resultJSON Nothing)

resultJSON :: Maybe Int -> P.DiffResult -> Value
resultJSON ident result=object (["revision" .= P.diffRevision result,"saved" .= False,
  "appliedDiff" .= P.appliedDiff result,"userModified" .= P.userModified result]++["bufferId" .= bid | Just bid<-[ident]])

-- | Locked wire dispatch captures the exact version after checking numeric
-- revision, retaining no Desktop. Invoke the returned continuation once, on the
-- reply worker: the same typed service owns its entire approval/correction ticket.
bufferDiffTool :: BufferDiffCommands -> P.BufferEditor -> Desktop -> T.Text -> Value -> IO (Desktop,IO (Either T.Text Value))
bufferDiffTool commands editor desktop _ args=do
  captured<-capturePatchRequest desktop args
  case captured of
    Left err->pure (desktop,pure (Left err))
    Right (ident,version,patch)->do
      let reference=editorReference editor ident
      pure (desktop,do
        outcome<-bufferDiffCommand commands editor reference version patch
        case outcome of
          Left err->pure (Left err)
          Right result->Right <$> evaluate (force (resultJSON (Just ident) result)))
