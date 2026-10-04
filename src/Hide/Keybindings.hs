{-# LANGUAGE OverloadedStrings #-}
-- | Session-owned binding reload and effective-map inspection workers.
--
-- Configuration IO, validation and inspection-buffer preparation run off the
-- interaction thread. Tick adopts a complete result; failed reloads retain the
-- previous maps. Closing the host cancels its outstanding worker.
module Hide.Keybindings (Keybindings, withKeybindings, keybindingEffects, tickKeybindings) where

import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.DeepSeq (force)
import Control.Exception (bracket, displayException, evaluate, mask)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Bindings
import Hide.BufferView (BufferView(CurrentView))
import Hide.Buffer (Buffer, newBuffer, prepareBuffer)
import Hide.Commands (configuredBindings)
import Hide.MCPPermissions (readKeybindingsFor)
import Hide.Model

data Result = Reloaded FilePath (M.Map (BindingPlatform,BindingContext) (Bindings Command)) | Inspected Text Buffer Int
newtype Keybindings = Keybindings (IORef (Maybe (Async (Either Text Result))))

-- | Scope the session's reload/inspection worker, independent of attachments.
withKeybindings :: (Keybindings -> IO a) -> IO a
withKeybindings = bracket (Keybindings <$> newIORef Nothing) close
  where close (Keybindings ref)=readIORef ref >>= mapM_ cancel

-- | Start one bounded binding task; ordinary effects continue through the host.
keybindingEffects :: Keybindings -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
keybindingEffects (Keybindings ref) core desktop effects=case effects of
  [ReloadKeyBindings directory] -> start $ do
    loaded<-readKeybindingsFor directory
    case loaded >>= configuredBindings of
      Left err -> pure (Left err)
      Right maps -> do
        _<-evaluate (force (map bindingEntries (M.elems maps)))
        pure (Right (Reloaded directory maps))
  [InspectKeyBindings context bindings] -> start $ do
    let title="Keybindings: "<>maybe "unavailable" (\(platform,owner)->platformName platform<>"/"<>contextName owner) context
        entries=maybe [] bindingEntries bindings
        text=T.unlines (title:"":if null entries then ["No configurable map is active in this view."] else
          [name<>" = "<>if null chords then "[]" else T.intercalate ", " chords | (name,chords)<-entries])
        buffer=newBuffer text
    _<-evaluate (prepareBuffer buffer)
    width<-evaluate (measureDocumentWidth text)
    pure (Right (Inspected title buffer width))
  _ -> core desktop effects
  where
    start task=mask $ \restore->do
      running<-readIORef ref
      case running of
        Just _ -> pure (False,desktop {status="A bindings operation is already running."})
        Nothing -> do
          worker<-async (restore task)
          writeIORef ref (Just worker)
          pure (False,desktop)

-- | Adopt only completed work. Reloads cannot replace a different project's map.
tickKeybindings :: Keybindings -> Desktop -> IO Desktop
tickKeybindings (Keybindings ref) desktop=do
  pending<-readIORef ref
  result<-maybe (pure Nothing) poll pending
  case result of
    Nothing -> pure desktop
    Just completed -> do
      writeIORef ref Nothing
      pure $ case completed of
        Left err -> desktop {status="Keybindings: "<>T.pack (displayException err)}
        Right (Left err) -> desktop {status="Keybindings: "<>err}
        Right (Right (Reloaded directory maps))
          | startingDirectory desktop==directory -> desktop {keyBindings=maps,status="Keybindings reloaded."}
          | otherwise -> desktop {status="Working directory changed; reload bindings again."}
        Right (Right (Inspected title buffer width)) ->
          let added=modifyActive (\window->window {bufferView=CurrentView,reviewSelection=Nothing}) (addDocument Nothing buffer desktop)
          in added {buffers=M.adjust (\doc->doc {documentLabel=Just title,documentCursorVisible=False,documentWidth=width}) (nextId desktop) (buffers added)}
