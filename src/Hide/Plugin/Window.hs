{-# LANGUAGE OverloadedStrings #-}
-- | Prepared read-only plugin content, independent of source documents.
--
-- Preparation belongs to a command/reply worker. The opaque instance identity
-- is fresh for each open request; the host owns geometry, focus and selection.
-- Text uses the existing measured tree internally without publishing a BufferRef,
-- saved baseline, Undo history or editable source document.
module Hide.Plugin.Window
  ( WindowRef, PreparedWindow, prepareTextWindow, prepareMarkdownWindow
  , preparedWindowRef, preparedWindowTitle, preparedWindowText, preparedWindowRows
  ) where

import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (Unique, newUnique, hashUnique)
import qualified Data.Vector as V
import Hide.Buffer (BufferContent, bufferContent, newBuffer, prepareBuffer)
import Hide.Markdown (renderMarkdown)
import Hide.Syntax (Style(..))

-- | Exact content instance. A closed/reopened view cannot reuse this identity.
newtype WindowRef = WindowRef Unique deriving (Eq,Ord)
instance Show WindowRef where
  show (WindowRef ident)="WindowRef "++show (hashUnique ident)

-- | Fully prepared immutable text and styled display rows. Equality observes
-- the unique prepared identity only, never text or styled payloads.
data PreparedWindow = PreparedWindow
  { preparedWindowRef :: !WindowRef, preparedWindowTitle :: !Text
  , preparedWindowText :: !BufferContent, preparedWindowRows :: !(V.Vector [(Char,Style)]) }

instance Eq PreparedWindow where
  a==b=preparedWindowRef a==preparedWindowRef b
instance Show PreparedWindow where
  show prepared="PreparedWindow "++show (preparedWindowRef prepared)

-- | Prepare ordinary selectable text on the calling worker.
prepareTextWindow :: Text -> Text -> IO PreparedWindow
prepareTextWindow title text=prepare title [(c,Plain) | c<-T.unpack text]

-- | Prepare CommonMark at a requested cell width on the calling worker.
-- Copy addresses the laid-out semantic text, excluding host chrome.
prepareMarkdownWindow :: Int -> Text -> Text -> IO PreparedWindow
prepareMarkdownWindow columns title text=prepare title (renderMarkdown columns text)

prepare :: Text -> [(Char,Style)] -> IO PreparedWindow
prepare title styled=do
  ident<-WindowRef <$> newUnique
  let rows=V.fromList (split styled)
  _<-evaluate (V.foldl' (\n row->foldl (\m (c,s)->c `seq` s `seq` m+1) n row) (0::Int) rows)
  let measured=newBuffer (T.pack (map fst styled))
  _<-evaluate (prepareBuffer measured)
  pure (PreparedWindow ident (T.take 8192 (T.map safe title)) (bufferContent measured) rows)
  where
    safe c | c<' ' || c=='\DEL'=' '
           | otherwise=c
    split chars=case break ((=='\n').fst) chars of
      (line,[])->[line]
      (line,_:rest)->line:split rest
