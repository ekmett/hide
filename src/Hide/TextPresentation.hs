{-# LANGUAGE OverloadedStrings #-}
-- | One scoped worker prepares styled layouts and Markdown previews outside input/render locks.
-- Captures retain immutable source content only, never Desktop or Undo. Adoption
-- checks bounded exact window/content/version/width targets. Retired or resized
-- styled targets use ordinary geometry until matching preparation completes;
-- Markdown previews remain a read-only pending/error surface.
module Hide.TextPresentation (TextPresentation,withTextPresentation,tickTextPresentation,prepareTextPresentations) where

import Control.Concurrent.Async (Async,async,cancel,poll)
import Control.Exception (bracket,mask,evaluate)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Hide.Buffer (BufferContent,bufferContent,contentSlice,contentLength,newBuffer,prepareBuffer)
import Hide.Markdown (renderMarkdown)
import Hide.Syntax (linkSpans)
import qualified Data.Text as T
import Hide.Model
import qualified Hide.Plugin.Window as W
import Hide.Syntax (Style)
import Hide.TextLayout (prepareTextLayout)

type Target = (Int,PresentationTarget,Int,Bool)
data StyledPayload = MarkdownSource | DocumentStyles ![(Char,Style)] | PluginStyles !(V.Vector [(Char,Style)])
data Capture = Capture !Target !BufferContent !StyledPayload
data Pending = Pending [Target] (Async [(Int,WindowPresentation)])
data TextPresentation = TextPresentation (IORef (Maybe Pending)) (IORef [Target])

-- | Own and join the single presentation worker at session shutdown.
withTextPresentation :: (TextPresentation -> IO a) -> IO a
withTextPresentation=bracket (TextPresentation <$> newIORef Nothing <*> newIORef []) close
  where close (TextPresentation pending _)=readIORef pending >>= mapM_ (\(Pending _ worker)->cancel worker)

-- | Poll/adopt complete snapshots and enqueue only when scalar targets change.
-- Preparation failure retains ordinary text or a bounded Markdown error state;
-- no input operation waits for it.
tickTextPresentation :: TextPresentation -> Desktop -> IO Desktop
tickTextPresentation (TextPresentation pending observed) desktop=mask $ \restore->do
  let targets=metadata desktop
  running<-readIORef pending
  completed<-case running of Nothing->pure Nothing; Just (Pending _ worker)->poll worker
  ready<-case (running,completed) of
    (Just (Pending captured _),Just result)->do
      writeIORef pending Nothing
      pure $ case result of
        Right prepared | captured==targets->reprojectWindowPresentations desktop desktop {windowPresentations=M.fromList prepared}
        Left _ | captured==targets->desktop {windowPresentations=M.fromList [(ident,MarkdownWindowFailure target width wide) | (ident,target@MarkdownPresentation{},width,wide)<-captured],status="Markdown view preparation failed."}
        _->desktop
    _->pure desktop
  let retained=M.filterWithKey (\ident prepared->let (target,width,wide)=presentationMetadata prepared in (ident,target,width,wide) `elem` targets) (windowPresentations ready)
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
prepare (Capture (ident,target,width,wide) text MarkdownSource)=do
  let chars=renderMarkdown width (contentSlice text 0 (contentLength text))
      rendered=T.pack (map fst chars)
  let measured=newBuffer rendered
  _<-evaluate (prepareBuffer measured)
  let content=bufferContent measured
      links=linkSpans chars
  layout<-prepareTextLayout wide width content (indexedHighlightRows chars)
  _<-evaluate (sum [a+z+T.length url | (a,z,url)<-links])
  pure (ident,MarkdownWindowPresentation target width wide layout content links)
prepare (Capture (ident,target,width,wide) text styled)=do
  layout<-prepareTextLayout wide width text (case styled of DocumentStyles chars->indexedHighlightRows chars; PluginStyles rows->rows; MarkdownSource->error "Unprepared Markdown source")
  pure (ident,WindowPresentation target width wide layout)

-- | Prepare deterministic snapshots using the same owner, outside an input lock.
prepareTextPresentations :: Desktop -> IO Desktop
prepareTextPresentations desktop=do
  prepared<-mapM prepare (captures desktop (metadata desktop))
  pure (reprojectWindowPresentations desktop desktop {windowPresentations=M.fromList prepared})

metadata :: Desktop -> [Target]
metadata desktop=[(windowId window,target,max 1 (width (bounds window)-2),wideSectionTitles desktop)
      | window<-windows desktop,windowPresentationNeeded desktop window,Just target<-[windowPresentationTarget desktop window]]

captures :: Desktop -> [Target] -> [Capture]
captures desktop=map capture
  where
    capture target@(_,MarkdownPresentation bid _,_,_)=case M.lookup bid (buffers desktop) of
      Just document->Capture target (bufferContent (documentBuffer document)) MarkdownSource
      Nothing->error "Markdown presentation capture lost a document."
    capture target@(_,DocumentPresentation bid _,_,_)=case M.lookup bid (buffers desktop) of
      Just document->Capture target (bufferContent (documentBuffer document))
        (DocumentStyles (documentHighlight document))
      Nothing->error "Text presentation capture lost a document."
    capture target@(_,PluginPresentation _ prepared,_,_)=Capture target (W.preparedWindowText prepared) (case W.preparedWindowRows prepared of W.StyledRows rows->PluginStyles rows; W.PlainRows _->DocumentStyles [])
