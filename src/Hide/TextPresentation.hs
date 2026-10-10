{-# LANGUAGE OverloadedStrings #-}
-- | One scoped worker prepares styled layouts and Markdown previews outside input/render locks.
-- Captures retain immutable source content only, never Desktop or Undo. Adoption
-- checks bounded exact window/content/version/width targets. Retired or resized
-- styled targets use ordinary geometry until matching preparation completes;
-- Markdown previews remain a read-only pending/error surface.
module Hide.TextPresentation (TextPresentation,withTextPresentation,textPresentationEffects,tickTextPresentation,prepareTextPresentations,BodyRequest(..),BodyResult(..)) where

import Control.Concurrent.Async (Async,asyncWithUnmask,cancel,poll)
import Control.Exception (bracket,mask,evaluate)
import Control.Monad (foldM)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Hide.Buffer (BufferContent,bufferContent,contentSlice,contentLength,newBuffer,prepareBuffer)
import Hide.Markdown (renderMarkdown)
import Hide.Syntax (linkSpans,styleLayoutMetadata,StyledText,StyledRow,styledContents)
import qualified Data.Text as T
import Hide.Model
import Hide.ConversationBody (BodyRequest(..),BodyResult(..),BodyKey,ConversationCopy(..),prepareConversationCopy,copyReference,copyTarget,copyProvider,copySerial,capturedSourceProvider,logicalBodyProvider,prepareConversationBody)
import qualified Hide.Plugin.Window as W
import Hide.TextLayout (prepareTextLayout)

type Target = (Int,PresentationTarget,Int,Bool)
data StyledPayload = MarkdownSource | DocumentStyles !StyledText | PluginStyles !(V.Vector StyledRow)
data Capture = Capture !Target !BufferContent !StyledPayload
data Pending = Pending [Target] [BodyKey] (Async ([(Int,WindowPresentation)],[BodyResult],Maybe (ConversationCopy,T.Text)))
data TextPresentation = TextPresentation (IORef (Maybe Pending)) (IORef ([Target],[BodyKey])) (IORef (Maybe ConversationCopy))

-- | Own and join the single presentation worker at session shutdown.
withTextPresentation :: (TextPresentation -> IO a) -> IO a
withTextPresentation=bracket (TextPresentation <$> newIORef Nothing <*> newIORef ([],[]) <*> newIORef Nothing) close
  where close (TextPresentation pending _ _)=readIORef pending >>= mapM_ (\(Pending _ _ worker)->cancel worker)

-- | The existing serial presentation owner accepts one latest closed human
-- copy intent. No formatting/owner lookup runs on input; stale intent serials
-- and frame/provider retirement prevent eventual clipboard publication.
textPresentationEffects :: TextPresentation -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
textPresentationEffects (TextPresentation _ _ requested) fallback original=foldM step (False,original)
  where
    step state@(True,_) _=pure state
    step (_,desktop) (CopyConversation copy)
      | copyCurrent desktop copy=writeIORef requested (Just copy) >> pure (False,desktop)
      | otherwise=pure (False,desktop)
    step (_,desktop) (AgentAction "copy" [])
      | Just view<-M.lookup (conversationTarget desktop) (conversationViews desktop)
      , Just reference<-conversationBodyRef view
      , let serial=fst (clipboardExport desktop)+1
      , Just copy<-case conversationSource view of
          Just source->Just (ConversationTranscriptCopy reference (conversationTarget desktop) source serial)
          Nothing->(\logical->ConversationLogicalCopy reference (conversationTarget desktop) logical serial) <$> conversationLogical view =do
          let updated=desktop {clipboardExport=(serial,Nothing),status="Preparing conversation copy."}
          if copyCurrent updated copy then writeIORef requested (Just copy) >> pure (False,updated)
          else pure (False,desktop)
    step (_,desktop) effect=fallback desktop [effect]

copyCurrent :: Desktop -> ConversationCopy -> Bool
copyCurrent desktop copy=
  fst (clipboardExport desktop)==copySerial copy && any ((==PluginContent (copyReference copy)).windowContent) (windows desktop) &&
  case M.lookup (copyTarget copy) (conversationViews desktop) of
    Just view | conversationBodyRef view==Just (copyReference copy)->case copy of
      ConversationTranscriptCopy{}->maybe False ((==copyProvider copy).capturedSourceProvider) (conversationSource view)
      _->maybe False ((==copyProvider copy).logicalBodyProvider) (conversationLogical view) &&
        maybe True ((==copyProvider copy).capturedSourceProvider) (conversationSource view)
    _->False

-- | Poll/adopt complete snapshots and enqueue only when scalar targets change.
-- Preparation failure retains ordinary text or a bounded Markdown error state;
-- no input operation waits for it.
tickTextPresentation :: TextPresentation -> [BodyRequest] -> Desktop -> IO (Desktop,[BodyResult])
tickTextPresentation (TextPresentation pending observed requested) bodies desktop=mask $ \_->do
  let targets=metadata desktop
      keys=[key | BodyRequest key _<-bodies]
  mapM_ evaluate keys
  running<-readIORef pending
  completed<-case running of Nothing->pure Nothing; Just (Pending _ _ worker)->poll worker
  (ready,bodyResults)<-case (running,completed) of
    (Just (Pending captured bodyKeys _),Just result)->do
      writeIORef pending Nothing
      pure $ case result of
        Right (prepared,finished,copied)->
          (publishCopy copied (reprojectWindowPresentations desktop desktop {windowPresentations=M.union
            (M.fromList [(ident,presentation) | (ident,presentation)<-prepared,
              let (target,width,wide)=presentationMetadata presentation,(ident,target,width,wide) `elem` targets])
            (windowPresentations desktop)}),finished)
        Left _->(desktop {windowPresentations=M.union
          (M.fromList [(ident,MarkdownWindowFailure target width wide) | entry@(ident,target@MarkdownPresentation{},width,wide)<-captured,entry `elem` targets])
          (windowPresentations desktop),status="Text view preparation failed."},
          [BodyResult key (Left "Conversation body preparation failed.") | key<-bodyKeys])
    _->pure (desktop,[])
  let retained=M.filterWithKey (\ident prepared->let (target,width,wide)=presentationMetadata prepared
        in (ident,target,width,wide) `elem` targets || any (\window->windowId window==ident && maybe False (const True) (conversationTargetFor ready window)) (windows ready)) (windowPresentations ready)
      shown=ready {windowPresentations=retained}
  previous<-readIORef observed
  waiting<-readIORef requested
  let wanted=case waiting of Just copy | copyCurrent shown copy->Just copy; _->Nothing
  if waiting/=wanted then writeIORef requested wanted else pure ()
  active<-readIORef pending
  case active of
    Just _->pure (shown,bodyResults)
    Nothing | previous==(targets,keys) && wanted==Nothing->pure (shown,bodyResults)
    Nothing | null targets && null bodies && wanted==Nothing->writeIORef observed ([],[]) >> pure (shown,bodyResults)
    Nothing->do
      let workTargets=filter (`notElem` fst previous) targets
          captured=captures shown workTargets
          changed=[request | request@(BodyRequest key _)<-bodies,key `notElem` snd previous]
      let workKeys=[key | BodyRequest key _<-changed]
      mapM_ evaluate workKeys
      mapM_ evaluate captured
      mapM_ evaluate changed
      worker<-asyncWithUnmask $ \unmask->unmask ((,,) <$> mapM prepare captured <*> mapM prepareConversationBody changed <*> traverse prepareCopy wanted)
      writeIORef requested Nothing
      writeIORef pending (Just (Pending workTargets workKeys worker))
      writeIORef observed (targets,keys)
      pure (shown,bodyResults)

publishCopy :: Maybe (ConversationCopy,T.Text) -> Desktop -> Desktop
publishCopy (Just (copy,text)) desktop
  | copyCurrent desktop copy=desktop {clipboard=text,clipboardCode=Nothing,clipboardExport=(copySerial copy,Just text),status="Conversation text copied."}
publishCopy _ desktop=desktop

prepareCopy :: ConversationCopy -> IO (ConversationCopy,T.Text)
prepareCopy copy=do
  text<-prepareConversationCopy copy
  pure (copy,text)

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
  layout<-prepareTextLayout wide width text (case styled of DocumentStyles chars->indexedHighlightRows chars; PluginStyles rows->rows)
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
