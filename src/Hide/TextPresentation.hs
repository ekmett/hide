-- | One scoped worker prepares wide-title layout outside input/render locks.
-- Captures retain immutable source content only, never Desktop or Undo. Adoption
-- checks bounded exact window/content/version/width targets. Retired or resized
-- targets use ordinary geometry until matching preparation completes.
module Hide.TextPresentation (TextPresentation,withTextPresentation,tickTextPresentation,prepareTextPresentations) where

import Control.Concurrent.Async (Async,async,cancel,poll)
import Control.Exception (bracket,mask,evaluate)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Hide.Buffer (BufferContent,bufferContent)
import Hide.Model
import qualified Hide.Plugin.Window as W
import Hide.Syntax (Style)
import Hide.TextLayout (prepareTextLayout)

type Target = (Int,PresentationTarget,Int)
data StyledPayload = DocumentStyles ![(Char,Style)] | PluginStyles !(V.Vector [(Char,Style)])
data Capture = Capture !Target !BufferContent !StyledPayload
data Pending = Pending [Target] (Async [(Int,WindowPresentation)])
data TextPresentation = TextPresentation (IORef (Maybe Pending)) (IORef [Target])

-- | Own and join the single presentation worker at session shutdown.
withTextPresentation :: (TextPresentation -> IO a) -> IO a
withTextPresentation=bracket (TextPresentation <$> newIORef Nothing <*> newIORef []) close
  where close (TextPresentation pending _)=readIORef pending >>= mapM_ (\(Pending _ worker)->cancel worker)

-- | Poll/adopt complete snapshots and enqueue only when scalar targets change.
-- Preparation failure retains ordinary text; no input operation waits for it.
tickTextPresentation :: TextPresentation -> Desktop -> IO Desktop
tickTextPresentation (TextPresentation pending observed) desktop=mask $ \restore->do
  let targets=metadata desktop
  running<-readIORef pending
  completed<-case running of Nothing->pure Nothing; Just (Pending _ worker)->poll worker
  ready<-case (running,completed) of
    (Just (Pending captured _),Just result)->do
      writeIORef pending Nothing
      pure $ case result of
        Right prepared | captured==targets->desktop {windowPresentations=M.fromList prepared}
        _->desktop
    _->pure desktop
  let retained=M.filterWithKey (\ident (WindowPresentation target width _)->(ident,target,width) `elem` targets) (windowPresentations ready)
      shown=ready {windowPresentations=retained}
  previous<-readIORef observed
  active<-readIORef pending
  case active of
    Just _->pure shown
    Nothing | previous==targets->pure shown
    Nothing | null targets->writeIORef observed [] >> pure shown
    Nothing->do
      let captured=captures shown targets
      mapM_ evaluate captured
      worker<-async (restore (mapM prepare captured))
      writeIORef pending (Just (Pending targets worker))
      writeIORef observed targets
      pure shown

prepare :: Capture -> IO (Int,WindowPresentation)
prepare (Capture (ident,target,width) text styled)=do
  layout<-prepareTextLayout True width text (case styled of DocumentStyles chars->indexedHighlightRows chars; PluginStyles rows->rows)
  pure (ident,WindowPresentation target width layout)

-- | Prepare deterministic snapshots using the same owner, outside an input lock.
prepareTextPresentations :: Desktop -> IO Desktop
prepareTextPresentations desktop=do
  prepared<-mapM prepare (captures desktop (metadata desktop))
  pure desktop {windowPresentations=M.fromList prepared}

metadata :: Desktop -> [Target]
metadata desktop
  | not (wideSectionTitles desktop)=[]
  | otherwise=[(windowId window,target,max 1 (width (bounds window)-2))
      | window<-windows desktop,Just target<-[windowPresentationTarget desktop window]]

captures :: Desktop -> [Target] -> [Capture]
captures desktop=map capture
  where
    capture target@(_,DocumentPresentation bid _,_)=case M.lookup bid (buffers desktop) of
      Just document->Capture target (bufferContent (documentBuffer document))
        (DocumentStyles (documentHighlight document))
      Nothing->error "Text presentation capture lost a document."
    capture target@(_,PluginPresentation _ prepared,_)=Capture target (W.preparedWindowText prepared) (PluginStyles (W.preparedWindowRows prepared))
