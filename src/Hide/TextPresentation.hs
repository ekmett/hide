{-# LANGUAGE OverloadedStrings #-}
-- | One scoped worker prepares styled layouts and Markdown previews outside input/render locks.
-- Captures retain immutable source content only, never Desktop or Undo. Adoption
-- checks bounded exact window/content/version/width targets. Retired or resized
-- styled targets use ordinary geometry until matching preparation completes;
-- Markdown previews remain a read-only pending/error surface.
module Hide.TextPresentation (TextPresentation,withTextPresentation,tickTextPresentation,prepareTextPresentations,BodyRequest(..),BodyResult(..)) where

import Control.Concurrent.Async (Async,asyncWithUnmask,cancel,poll)
import Control.Exception (bracket,mask,evaluate)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Hide.Buffer (BufferContent,bufferContent,contentSlice,contentLength,newBuffer,prepareBuffer)
import Hide.Markdown (renderMarkdown)
import Hide.Syntax (linkSpans,styleLayoutMetadata,StyledText,StyledRow,styledContents)
import qualified Data.Text as T
import Hide.Model
import Hide.ConversationBody (BodyRequest(..),BodyResult(..),BodyKey,prepareConversationBody)
import qualified Hide.Plugin.Window as W
import Hide.TextLayout (prepareTextLayout)

type Target = (Int,PresentationTarget,Int,Bool)
data StyledPayload = MarkdownSource | DocumentStyles !StyledText | PluginStyles !(V.Vector StyledRow)
data Capture = Capture !Target !BufferContent !StyledPayload
data Pending = Pending [Target] [BodyKey] (Async ([(Int,WindowPresentation)],[BodyResult]))
data TextPresentation = TextPresentation (IORef (Maybe Pending)) (IORef ([Target],[BodyKey]))

-- | Own and join the single presentation worker at session shutdown.
withTextPresentation :: (TextPresentation -> IO a) -> IO a
withTextPresentation=bracket (TextPresentation <$> newIORef Nothing <*> newIORef ([],[])) close
  where close (TextPresentation pending _)=readIORef pending >>= mapM_ (\(Pending _ _ worker)->cancel worker)

-- | Poll/adopt complete snapshots and enqueue only when scalar targets change.
-- Preparation failure retains ordinary text or a bounded Markdown error state;
-- no input operation waits for it.
tickTextPresentation :: TextPresentation -> [BodyRequest] -> Desktop -> IO (Desktop,[BodyResult])
tickTextPresentation (TextPresentation pending observed) bodies desktop=mask $ \_->do
  let targets=metadata desktop
      keys=[key | BodyRequest key _<-bodies]
  mapM_ evaluate keys
  running<-readIORef pending
  completed<-case running of Nothing->pure Nothing; Just (Pending _ _ worker)->poll worker
  (ready,bodyResults)<-case (running,completed) of
    (Just (Pending captured bodyKeys _),Just result)->do
      writeIORef pending Nothing
      pure $ case result of
        Right (prepared,finished)->
          (reprojectWindowPresentations desktop desktop {windowPresentations=M.union
            (M.fromList [(ident,presentation) | (ident,presentation)<-prepared,
              let (target,width,wide)=presentationMetadata presentation,(ident,target,width,wide) `elem` targets])
            (windowPresentations desktop)},finished)
        Left _->(desktop {windowPresentations=M.union
          (M.fromList [(ident,MarkdownWindowFailure target width wide) | entry@(ident,target@MarkdownPresentation{},width,wide)<-captured,entry `elem` targets])
          (windowPresentations desktop),status="Text view preparation failed."},
          [BodyResult key (Left "Conversation body preparation failed.") | key<-bodyKeys])
    _->pure (desktop,[])
  let retained=M.filterWithKey (\ident prepared->let (target,width,wide)=presentationMetadata prepared
        in (ident,target,width,wide) `elem` targets || any (\window->windowId window==ident && maybe False (const True) (conversationTargetFor ready window)) (windows ready)) (windowPresentations ready)
      shown=ready {windowPresentations=retained}
  previous<-readIORef observed
  active<-readIORef pending
  case active of
    Just _->pure (shown,bodyResults)
    Nothing | previous==(targets,keys)->pure (shown,bodyResults)
    Nothing | null targets && null bodies->writeIORef observed ([],[]) >> pure (shown,bodyResults)
    Nothing->do
      let workTargets=filter (`notElem` fst previous) targets
          captured=captures shown workTargets
          changed=[request | request@(BodyRequest key _)<-bodies,key `notElem` snd previous]
      let workKeys=[key | BodyRequest key _<-changed]
      mapM_ evaluate workKeys
      mapM_ evaluate captured
      mapM_ evaluate changed
      worker<-asyncWithUnmask $ \unmask->unmask ((,) <$> mapM prepare captured <*> mapM prepareConversationBody changed)
      writeIORef pending (Just (Pending workTargets workKeys worker))
      writeIORef observed (targets,keys)
      pure (shown,bodyResults)

prepare :: Capture -> IO (Int,WindowPresentation)
prepare (Capture (ident,target,width,wide) text MarkdownSource)=do
  let chars=renderMarkdown width (contentSlice text 0 (contentLength text))
      rendered=styledContents chars
  let measured=newBuffer rendered
  _<-evaluate (prepareBuffer measured)
  let content=bufferContent measured
      links=linkSpans chars
  layout<-prepareTextLayout wide width content (indexedHighlightRows chars)
  _<-evaluate (sum [a+z+T.length url | (a,z,url)<-links])
  pure (ident,MarkdownWindowPresentation target width wide layout content links)
prepare (Capture (ident,target,width,False) _ (DocumentStyles chars))
  | not (any (styleLayoutMetadata . snd) chars)=pure (ident,WindowPresentationUnneeded target width False)
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
      | window<-windows desktop,conversationTargetFor desktop window==Nothing,Just target<-[windowPresentationTarget desktop window],
        windowPresentationNeeded desktop window || case target of DocumentPresentation{}->True; _->False]

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
    capture target@(_,PluginPresentation _ prepared,_,_)=Capture target (W.preparedWindowText prepared) (case W.preparedWindowRows prepared of W.StyledRows rows->PluginStyles rows; W.PlainRows _->DocumentStyles []; W.RowsDetails{}->DocumentStyles [])
