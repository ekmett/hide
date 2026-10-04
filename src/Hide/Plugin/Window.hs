{-# LANGUAGE OverloadedStrings #-}
-- | Prepared read-only plugin content, independent of source documents.
--
-- Preparation belongs to a command/reply worker. The opaque instance identity
-- is fresh for each open request; the host owns geometry, focus and selection.
-- Text uses the existing measured tree internally without publishing a BufferRef,
-- saved baseline, Undo history or editable source document.
module Hide.Plugin.Window
  ( WindowRef, WindowScope, WindowUpdate, withWindowScope, openTextWindow, refreshTextWindow
  , updateWindowRef, admitWindowUpdate, windowRefCurrent, retireWindowRef
  , PreparedWindow, prepareTextWindow, prepareMarkdownWindow, prepareRecoverableTextWindow
  , preparedWindowTitle, preparedWindowText, preparedWindowRows, preparedWindowRecovery
  ) where

import Control.Exception (evaluate, bracket)
import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (Unique, newUnique, hashUnique)
import qualified Data.Vector as V
import Hide.Buffer (BufferContent, bufferContent, newBuffer, prepareBuffer)
import Hide.Markdown (renderMarkdown)
import Hide.Plugin.Command (validCommandName)
import Hide.Syntax (Style(..))

-- | Exact content instance. A closed/reopened view cannot reuse this identity.
data WindowRef = WindowRef Unique WindowScope (TVar (Integer,Bool))
instance Eq WindowRef where
  WindowRef a _ _==WindowRef b _ _=a==b
instance Ord WindowRef where
  compare (WindowRef a _ _) (WindowRef b _ _)=compare a b
instance Show WindowRef where
  show (WindowRef ident _ _)="WindowRef "++show (hashUnique ident)

-- | Fully prepared immutable text and styled display rows. Equality observes
-- the unique prepared identity only, never text or styled payloads.
data PreparedWindow = PreparedWindow !Unique !Text !BufferContent !(V.Vector [(Char,Style)]) !(Maybe (Text,Int))
preparedWindowRef :: PreparedWindow -> Unique
preparedWindowRef (PreparedWindow ident _ _ _ _)=ident
preparedWindowTitle :: PreparedWindow -> Text
preparedWindowTitle (PreparedWindow _ title _ _ _)=title
preparedWindowText :: PreparedWindow -> BufferContent
preparedWindowText (PreparedWindow _ _ text _ _)=text
preparedWindowRows :: PreparedWindow -> V.Vector [(Char,Style)]
preparedWindowRows (PreparedWindow _ _ _ rows _)=rows

-- | Explicit durable type/version. Ordinary prepared views are transient: their
-- text is never checkpointed implicitly. The host restores durable text as an
-- inert unavailable view; it does not invoke a plugin from the recovery parser.
preparedWindowRecovery :: PreparedWindow -> Maybe (Text,Int)
preparedWindowRecovery (PreparedWindow _ _ _ _ recovery)=recovery

-- | Prepare text whose title and content may be written to private recovery.
-- Use only non-secret state declared durable by the view's owner. Type IDs are
-- namespaced command-style names; positive versions describe the stored format.
prepareRecoverableTextWindow :: Text -> Int -> Text -> Text -> IO (Either Text PreparedWindow)
prepareRecoverableTextWindow kind version title text
  | not (validCommandName kind) || T.length kind>128 || version<=0=pure (Left "Invalid durable plugin window type/version.")
  | otherwise=do
      PreparedWindow ident caption measured rows _<-prepareTextWindow title text
      pure (Right (PreparedWindow ident caption measured rows (Just (kind,version))))

instance Eq PreparedWindow where
  a==b=preparedWindowRef a==preparedWindowRef b
instance Show PreparedWindow where
  show prepared="PreparedWindow "++show (hashUnique (preparedWindowRef prepared))

-- | Prepare ordinary selectable text on the calling worker.
prepareTextWindow :: Text -> Text -> IO PreparedWindow
prepareTextWindow title text=prepare title [(c,Plain) | c<-T.unpack text]

-- | Prepare CommonMark at a requested cell width on the calling worker.
-- Copy addresses the laid-out semantic text, excluding host chrome.
prepareMarkdownWindow :: Int -> Text -> Text -> IO PreparedWindow
prepareMarkdownWindow columns title text=prepare title (renderMarkdown columns text)

prepare :: Text -> [(Char,Style)] -> IO PreparedWindow
prepare title styled=do
  ident<-newUnique
  let rows=V.fromList (split styled)
  _<-evaluate (V.foldl' (\n row->foldl' (\m (c,s)->c `seq` s `seq` m+1) n row) (0::Int) rows)
  let measured=newBuffer (T.pack (map fst styled))
  _<-evaluate (prepareBuffer measured)
  evaluate (PreparedWindow ident (T.take 8192 (T.map safe title)) (bufferContent measured) rows Nothing)
  where
    safe c | c<' ' || c=='\DEL'=' '
           | otherwise=c
    split chars=case break ((=='\n').fst) chars of
      (line,[])->[line]
      (line,_:rest)->line:split rest

-- | A publication lifetime. Closing revokes every escaped instance without
-- invoking callbacks. Existing command/sidebar workers own preparation and join
-- their work before this scope closes; no work is spawned by the window API.
newtype WindowScope = WindowScope (TVar Bool)
withWindowScope :: (WindowScope -> IO a) -> IO a
withWindowScope=bracket (WindowScope <$> newTVarIO True) (\(WindowScope live)->atomically (writeTVar live False))

-- | One complete worker-prepared publication. It carries no desktop callback.
data WindowUpdate = WindowUpdate !WindowRef !Integer !Bool !PreparedWindow
updateWindowRef :: WindowUpdate -> WindowRef
updateWindowRef (WindowUpdate reference _ _ _)=reference

-- | Create an exact pending instance in a live publication scope. Reusing the
-- same prepared text creates independent instances; a duplicated reply does not.
openTextWindow :: WindowScope -> PreparedWindow -> IO (Maybe WindowUpdate)
openTextWindow scope@(WindowScope live) prepared=do
  current<-readTVarIO live
  if not current then pure Nothing else do
    reference<-WindowRef <$> newUnique <*> pure scope <*> newTVarIO (1,False)
    pure (Just (WindowUpdate reference 1 True prepared))

-- | Prepare a complete refresh for an exact adopted instance. Issuing a later
-- revision invalidates older queued publications. Host geometry and selection
-- are retained; refresh never opens a missing or closed window.
refreshTextWindow :: WindowRef -> PreparedWindow -> IO (Maybe WindowUpdate)
refreshTextWindow reference@(WindowRef _ (WindowScope scope) state) prepared=atomically $ do
  live<-readTVar scope
  (revision,opened)<-readTVar state
  if not live || revision<=0 || not opened then pure Nothing else do
    let next=revision+1
    writeTVar state (next,opened)
    pure (Just (WindowUpdate reference next False prepared))

-- | Host lifetime check; observes scalar scope/instance state only.
windowRefCurrent :: WindowRef -> IO Bool
windowRefCurrent (WindowRef _ (WindowScope scope) state)=atomically $ do
  live<-readTVar scope
  (revision,_)<-readTVar state
  pure (live && revision>0)

-- | Host adoption primitive. The supplied flag records whether the exact view
-- is already installed; false cannot turn a refresh into an open operation.
-- A missing refresh retires its instance. Caller must check actor/modal policy
-- before invoking this function; this primitive grants no desktop capability.
admitWindowUpdate :: Bool -> WindowUpdate -> IO (Maybe (WindowRef,PreparedWindow))
admitWindowUpdate present (WindowUpdate reference@(WindowRef _ (WindowScope scope) state) revision opening prepared)=atomically $ do
  live<-readTVar scope
  (latest,opened)<-readTVar state
  if not live || latest<=0 then pure Nothing
  else if not opening && not present then writeTVar state (0,False) >> pure Nothing
  else if latest/=revision || opening && (opened || present) || not opening && not opened then pure Nothing
  else writeTVar state (latest,True) >> pure (Just (reference,prepared))

-- | Host close invalidates the exact instance. Idempotent and callback-free.
retireWindowRef :: WindowRef -> IO ()
retireWindowRef (WindowRef _ _ state)=atomically (writeTVar state (0,False))
