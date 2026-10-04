{-# LANGUAGE OverloadedStrings #-}
-- | Prepared read-only plugin content, independent of source documents.
--
-- Preparation belongs to a command/reply worker. The opaque instance identity
-- is fresh for each open request; the host owns geometry, focus and selection.
-- Text uses the existing measured tree internally without publishing a BufferRef,
-- saved baseline, Undo history or editable source document.
module Hide.Plugin.Window
  ( WindowRef, WindowScope, WindowUpdate, withWindowScope, openTextWindow, refreshTextWindow
  , updateWindowRef, admitWindowUpdate, windowRefCurrent
  , PreparedWindow, prepareTextWindow, prepareMarkdownWindow
  , preparedWindowTitle, preparedWindowText, preparedWindowRows
  ) where

import Control.Exception (evaluate, bracket)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (Unique, newUnique, hashUnique)
import qualified Data.Vector as V
import Hide.Buffer (BufferContent, bufferContent, newBuffer, prepareBuffer)
import Hide.Markdown (renderMarkdown)
import Hide.Syntax (Style(..))

-- | Exact content instance. A closed/reopened view cannot reuse this identity.
data WindowRef = WindowRef Unique WindowScope (IORef (Integer,Bool))
instance Eq WindowRef where
  WindowRef a _ _==WindowRef b _ _=a==b
instance Ord WindowRef where
  compare (WindowRef a _ _) (WindowRef b _ _)=compare a b
instance Show WindowRef where
  show (WindowRef ident _ _)="WindowRef "++show (hashUnique ident)

-- | Fully prepared immutable text and styled display rows. Equality observes
-- the unique prepared identity only, never text or styled payloads.
data PreparedWindow = PreparedWindow
  { preparedWindowRef :: !Unique, preparedWindowTitle :: !Text
  , preparedWindowText :: !BufferContent, preparedWindowRows :: !(V.Vector [(Char,Style)]) }

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
  pure (PreparedWindow ident (T.take 8192 (T.map safe title)) (bufferContent measured) rows)
  where
    safe c | c<' ' || c=='\DEL'=' '
           | otherwise=c
    split chars=case break ((=='\n').fst) chars of
      (line,[])->[line]
      (line,_:rest)->line:split rest

-- | A publication lifetime. Closing revokes every escaped instance without
-- invoking callbacks. Existing command/sidebar workers own preparation and join
-- their work before this scope closes; no work is spawned by the window API.
newtype WindowScope = WindowScope (IORef Bool)
withWindowScope :: (WindowScope -> IO a) -> IO a
withWindowScope=bracket (WindowScope <$> newIORef True) (\(WindowScope live)->writeIORef live False)

-- | One complete worker-prepared publication. It carries no desktop callback.
data WindowUpdate = WindowUpdate !WindowRef !Integer !Bool !PreparedWindow
updateWindowRef :: WindowUpdate -> WindowRef
updateWindowRef (WindowUpdate reference _ _ _)=reference

-- | Create an exact pending instance in a live publication scope. Reusing the
-- same prepared text creates independent instances; a duplicated reply does not.
openTextWindow :: WindowScope -> PreparedWindow -> IO (Maybe WindowUpdate)
openTextWindow scope@(WindowScope live) prepared=do
  current<-readIORef live
  if not current then pure Nothing else do
    reference<-WindowRef <$> newUnique <*> pure scope <*> newIORef (1,False)
    pure (Just (WindowUpdate reference 1 True prepared))

-- | Prepare a complete refresh for an exact adopted instance. Issuing a later
-- revision invalidates older queued publications. Host geometry and selection
-- are retained; refresh never opens a missing or closed window.
refreshTextWindow :: WindowRef -> PreparedWindow -> IO (Maybe WindowUpdate)
refreshTextWindow reference@(WindowRef _ _ state) prepared=do
  live<-windowRefCurrent reference
  if not live then pure Nothing else atomicModifyIORef' state $ \(revision,opened)->
    if not opened then ((revision,opened),Nothing) else
      let next=revision+1 in ((next,opened),Just (WindowUpdate reference next False prepared))

-- | Host lifetime check; observes scalar scope/instance state only.
windowRefCurrent :: WindowRef -> IO Bool
windowRefCurrent (WindowRef _ (WindowScope scope) state)=do
  live<-readIORef scope
  (revision,_)<-readIORef state
  pure (live && revision>0)

-- | Host adoption primitive. The supplied flag records whether the exact view
-- is already installed; false cannot turn a refresh into an open operation.
-- A missing refresh retires its instance. Caller must check actor/modal policy
-- before invoking this function; this primitive grants no desktop capability.
admitWindowUpdate :: Bool -> WindowUpdate -> IO (Maybe (WindowRef,PreparedWindow))
admitWindowUpdate present (WindowUpdate reference@(WindowRef _ _ state) revision opening prepared)=do
  live<-windowRefCurrent reference
  if not live then pure Nothing else atomicModifyIORef' state $ \(latest,opened)->
    if not opening && not present then ((0,False),Nothing)
    else if latest/=revision || opening && (opened || present) || not opening && not opened then ((latest,opened),Nothing)
    else ((latest,True),Just (reference,prepared))
