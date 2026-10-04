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
import Hide.Commands (configuredBindings, contributedBindingCommands)
import Hide.MCPPermissions (readKeybindingsFor)
import Hide.Model

type Configuration = M.Map Text (M.Map Text (M.Map Text [Text]))
type Catalogue = [(Text,Command)]
data Result = Compiled Bool FilePath Catalogue Configuration (M.Map (BindingPlatform,BindingContext) (Bindings Command)) | Inspected Text Buffer Int
data Keybindings = Keybindings (IORef (Maybe (Async (Either Text Result)))) (IORef Configuration) (IORef (Maybe Catalogue))

-- | Scope the session's binding worker, independent of attachments. The host
-- validates the initial configuration against its live catalogue before input.
-- Previously validated configured IDs stay inert after their registration retires.
withKeybindings :: Configuration -> (Keybindings -> IO a) -> IO a
withKeybindings configuration = bracket (Keybindings <$> newIORef Nothing <*> newIORef configuration <*> newIORef Nothing) close
  where close (Keybindings ref _ _)=readIORef ref >>= mapM_ cancel

-- Retired configured identities remain chord owners, never implicit defaults or
-- new authority. This catalogue is computed on the binding worker, not in input.
prepare :: Bool -> FilePath -> Catalogue -> Configuration -> Configuration -> IO (Either Text Result)
prepare reload directory catalogue known configuration=case configuredBindings available configuration of
  Left err->pure (Left err)
  Right maps->do
    _<-evaluate (force (map bindingEntries (M.elems maps)))
    pure (Right (Compiled reload directory catalogue configuration maps))
  where
    names=M.keys (M.unions [commands | contexts<-M.elems known,commands<-M.elems contexts])
    available=catalogue++[(name,Disabled name) | name<-names,name `notElem` map fst catalogue]

-- | Start one bounded binding task; ordinary effects continue through the host.
keybindingEffects :: Keybindings -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
keybindingEffects (Keybindings ref configurationRef observed) core desktop effects=case effects of
  [ReloadKeyBindings directory] -> do
    known<-readIORef configurationRef
    catalogue<-evaluate (contributedBindingCommands desktop)
    start (Just catalogue) $ do
      loaded<-readKeybindingsFor directory
      case loaded of
        Left err->pure (Left err)
        Right configuration->prepare True directory catalogue known configuration
  [InspectKeyBindings context bindings] -> start Nothing $ do
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
    start catalogue task=mask $ \restore->do
      running<-readIORef ref
      case running of
        Just _ -> pure (False,desktop {status="A bindings operation is already running."})
        Nothing -> do
          worker<-async (restore task)
          writeIORef ref (Just worker)
          mapM_ (writeIORef observed . Just) catalogue
          pure (False,desktop)

-- | Adopt only completed work. Reloads cannot replace a different project's map.
tickKeybindings :: Keybindings -> Desktop -> IO Desktop
tickKeybindings (Keybindings ref configurationRef observed) desktop=do
  pending<-readIORef ref
  result<-maybe (pure Nothing) poll pending
  next<-case result of
    Nothing->pure desktop
    Just completed->do
      writeIORef ref Nothing
      case completed of
        Left err->pure desktop {status="Keybindings: "<>T.pack (displayException err)}
        Right (Left err)->pure desktop {status="Keybindings: "<>err}
        Right (Right (Compiled reload directory catalogue configuration maps))
          | startingDirectory desktop/=directory -> pure desktop {status="Working directory changed; reload bindings again."}
          | otherwise->do
              writeIORef configurationRef configuration
              pure $ if contributedBindingCommands desktop/=catalogue then desktop else
                desktop {keyBindings=maps,status=if reload then "Keybindings reloaded." else status desktop}
        Right (Right (Inspected title buffer width))->pure $
          let added=modifyActive (\window->window {bufferView=CurrentView,reviewSelection=Nothing}) (addDocument Nothing buffer desktop)
          in added {buffers=M.adjust (\doc->doc {documentLabel=Just title,documentCursorVisible=False,documentWidth=width}) (nextId desktop) (buffers added)}
  -- The only comparison is this bounded list of exact refs/actor flags. Captured
  -- maps/configuration are immutable; callbacks and compilation stay off-thread.
  mask $ \restore->do
    running<-readIORef ref
    previous<-readIORef observed
    catalogue<-evaluate (contributedBindingCommands next)
    if maybe False (const True) running || previous==Just catalogue then pure next else do
      configuration<-readIORef configurationRef
      worker<-async (restore (prepare False (startingDirectory next) catalogue configuration configuration))
      writeIORef ref (Just worker)
      writeIORef observed (Just catalogue)
      pure next
