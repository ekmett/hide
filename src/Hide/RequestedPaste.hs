-- SPDX-License-Identifier: BSD-3-Clause
-- | Ephemeral requested clipboard reads owned by one serialized frontend session.
-- A matching receipt is consumed before input application. Leaving its immutable
-- input target retires it permanently, even if focus later returns. Nothing here
-- retains a buffer, compares text/Undo, runs a plugin or persists a capability.
module Hide.RequestedPaste
  ( RequestedPaste, newRequestedPaste, requestPaste, refreshRequestedPaste
  , cancelRequestedPaste, applyRequestedPaste ) where

import Control.Exception (evaluate)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import System.Mem.StableName (StableName,makeStableName)
import Hide.Buffer (Selection,Buffer)
import Hide.BufferView (BufferView)
import Hide.Model
import Hide.Plugin.BufferHost (ContentVersion,captureVersion)
import qualified Hide.Plugin.Editor as Editor
import Hide.RemoteEndpoint (randomIdentity)

-- Owner metadata is bounded; immutable payloads are represented only by identities.
data Owner = SourceOwner !Int !Int !BufferView !(Maybe ReviewSelection) !Bool !Bool
  | EditorOwner !Int !Editor.EditorMount !Bool | QuestionOwner !Int !Int | AutocompleteOwner !Int
  deriving Eq
data Target = DialogTarget !(StableName Dialog)
  | BufferTarget !Owner !Selection !ContentVersion | TerminalTarget !Int !Text
  deriving Eq
newtype RequestedPaste = RequestedPaste (IORef (Maybe (Text,Target)))

-- | Allocate a single pending slot for the live host/connection lifetime.
newRequestedPaste :: IO RequestedPaste
newRequestedPaste=RequestedPaste <$> newIORef Nothing

-- | Supersede an older request and capture only the actual focused input owner.
-- The nonce uses the existing session identity generator; it is not an authority
-- grant. The owning human transport performs all capture/consumption under its lock.
requestPaste :: RequestedPaste -> Desktop -> IO (Maybe Text)
requestPaste (RequestedPaste ref) d=do
  target<-captureTarget d
  case target of
    Nothing->writeIORef ref Nothing >> pure Nothing
    Just value->do
      token<-T.pack <$> randomIdentity
      writeIORef ref (Just (token,value))
      pure (Just token)

-- | Validate only while a read is pending, after each owner transition. Expiration
-- is irreversible: an away/back or close/reopen sequence cannot revive a receipt.
refreshRequestedPaste :: RequestedPaste -> Desktop -> IO ()
refreshRequestedPaste (RequestedPaste ref) d=do
  pending<-readIORef ref
  case pending of
    Nothing->pure ()
    Just (_,expected)->do
      current<-captureTarget d
      if current==Just expected then pure () else writeIORef ref Nothing

-- | Disconnect/reattach and teardown retire reads; none survive recovery.
cancelRequestedPaste :: RequestedPaste -> IO ()
cancelRequestedPaste (RequestedPaste ref)=writeIORef ref Nothing

-- | Consume a matching receipt at most once before applying ordinary paste input.
-- An older or duplicate token cannot consume a newer request. The caller has
-- already attributed this reply to the human transport, never to a guest batch.
applyRequestedPaste :: RequestedPaste -> Text -> Text -> Desktop -> IO (Desktop,[Effect])
applyRequestedPaste requested@(RequestedPaste ref) token text d=do
  refreshRequestedPaste requested d
  pending<-readIORef ref
  case pending of
    Just (expected,_) | token==expected->do
      writeIORef ref Nothing
      pure (handleEvent (V.EvPaste (TE.encodeUtf8 text)) d)
    _->pure (d,[])

captureTarget :: Desktop -> IO (Maybe Target)
captureTarget d
  | Just dg<-dialog d = case drop (focus dg) (fields dg) of
      SelectedInput{}:_->named dg
      Input{}:_->named dg
      TextArea _ True _ _ _ _:_->named dg
      _->pure Nothing
  | menu d/=Nothing || contextMenu d/=Nothing = pure Nothing
  | Just w<-activeWindow d,windowFocused d w =
      if activeAutocomplete d then
        if autocompleteFocused d then buffered (AutocompleteOwner (windowId w)) (autocompleteSelection d) (autocompleteDraft d) else pure Nothing
      else if questionActive d then case chatQuestion d of
        Just q->buffered (QuestionOwner (windowId w) (questionToken q)) (questionSelection q) (questionBuffer q)
        _->pure Nothing
      else case activeEditorMount d of
        Just mount->buffered (EditorOwner (windowId w) mount (composerFocused d)) (composerSelection d) (composerBuffer d)
        Nothing->case activeTerminal d of
          Just terminal->pure (Just (TerminalTarget (windowId w) terminal))
          _->case (bufferId w,activeDocument d) of
            (Just bid,Just doc) | documentLabel doc==Nothing,commandEnabled d Paste ->
              buffered (SourceOwner (windowId w) bid (bufferView w) (reviewSelection w) (windowHexLow w) (windowHexAscii w)) (selection w) (documentBuffer doc)
            _->pure Nothing
  | otherwise=pure Nothing
  where
    named dg=Just . DialogTarget <$> (evaluate dg >>= makeStableName)
    buffered :: Owner -> Selection -> Buffer -> IO (Maybe Target)
    buffered owner sel b=Just . BufferTarget owner sel <$> captureVersion b
