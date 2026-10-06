{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- |
-- Module      : Hide.Plugin.Window
-- Copyright   : (c) Edward Kmett
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings, BangPatterns
--
-- Prepared read-only plugin content, independent of source documents.
--
-- Preparation belongs to a command/reply worker. The opaque instance identity
-- is fresh for each open request; the host owns geometry, focus and selection.
-- Text uses the existing measured tree internally without publishing a BufferRef,
-- saved baseline, Undo history or editable source document.
module Hide.Plugin.Window
  ( WindowRef, WindowScope, WindowUpdate, withWindowScope, openTextWindow, refreshTextWindow
  , updateWindowRef, admitWindowUpdate, windowRefCurrent, windowScopeCurrent, retireWindowRef
  , EditorWindowUpdate, openEditorWindow, editorWindowBody, editorWindowEditor, admitEditorWindowUpdate
  , PreparedWindow, prepareTextWindow, prepareMarkdownWindow, prepareStyledTextWindow, prepareSemanticTextWindow, prepareRecoverableTextWindow
  , WindowDisclosure(..), TextCopy(..), MessageAttribution(..), TextSemantics(..), preparedWindowSemantics, preparedWindowDisclosure, preparedWindowMessages, copyPreparedSelection
  , WindowRow(..), prepareRowsWindow, prepareRecoverableRowsWindow
  , WindowRows(..), preparedWindowTitle, preparedWindowText, preparedWindowRows, preparedWindowWidth, preparedWindowHasSections, preparedWindowNeedsLayout, preparedWindowRecovery
  ) where

import Control.Exception (evaluate, bracket)
import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (Unique, newUnique, hashUnique)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Hide.Plugin.Tree (NodeId)
import Hide.Plugin.Menu (MenuRef)
import Data.List (nub,foldl',groupBy)
import Hide.Buffer (contentLength,contentSlice,BufferContent, bufferContent, newBuffer, prepareBuffer)
import Hide.Unicode (sourceTextWidth)
import Hide.Markdown (renderMarkdown)
import Hide.Plugin.Command (validCommandName)
import qualified Hide.Plugin.EditorHost as E
import Hide.Syntax (Style(..),SourceRow,plainSourceRow,sourceRowText,sourceRowRanges,sourceRangeCharEnd,sectionTitle,styleLayoutMetadata)

-- | Exact content instance. A closed/reopened view cannot reuse this identity.
data WindowRef = WindowRef Unique WindowScope !WindowDisclosure (TVar (Integer,Bool))
instance Eq WindowRef where
  WindowRef a _ _ _==WindowRef b _ _ _=a==b
instance Ord WindowRef where
  compare (WindowRef a _ _ _) (WindowRef b _ _ _)=compare a b
instance Show WindowRef where
  show (WindowRef ident _ _ _)="WindowRef "++show (hashUnique ident)

-- | Fully prepared immutable text and styled display rows. Equality observes
-- the unique prepared identity only, never text or styled payloads.
-- | One stable selectable row and its already-prepared plain Details snapshot.
-- IDs are local to the containing WindowRef; labels never identify actions.
data WindowRow = WindowRow !NodeId !Text !PreparedWindow
data WindowRows = PlainRows !(V.Vector SourceRow) | StyledRows !(V.Vector [(Char,Style)])
  | RowsDetails !(V.Vector WindowRow) !(M.Map NodeId Int) ![MenuRef]
-- | Observation only. Readability grants no input, shell or command authority.
-- The declaration is frozen by each WindowRef; changing it needs a fresh open.
data WindowDisclosure = PrivateWindow | ReadableWindow deriving (Eq,Show)

-- | Message attribution belongs to copied text, never provider identity.
data MessageAttribution = NoAttribution | UserBotAttribution deriving (Eq,Show)
data TextCopy = CopyText | CopyMessages !MessageAttribution deriving (Eq,Show)

-- | Immutable scalar intervals over exactly the prepared text. Masks describe
-- the distinct guest, human-streamer and recovery projections. No field grants
-- authority or contains a callback; owners sanitize data before preparation.
data TextSemantics = TextSemantics
  { textCopy :: !TextCopy, textLinkBase :: !(Maybe FilePath)
  , textLinks :: !(V.Vector (Int,Int,Text))
  , textShellBlocks :: !(V.Vector (Int,Int,Text,Text))
  , textDisclosure :: !WindowDisclosure
  , textGuestHidden :: !(V.Vector (Int,Int))
  , textStreamerHidden :: !(V.Vector (Int,Int))
  , textRecoveryHidden :: !(V.Vector (Int,Int))
  }

data PreparedWindow = PreparedWindow !Unique !Text !BufferContent !WindowRows !Int !(Maybe (Text,Int)) !Bool !Bool !(Maybe (TextSemantics,V.Vector (Int,Int,Int,Bool)))
preparedWindowRef :: PreparedWindow -> Unique
preparedWindowRef (PreparedWindow ident _ _ _ _ _ _ _ _)=ident
preparedWindowTitle :: PreparedWindow -> Text
preparedWindowTitle (PreparedWindow _ title _ _ _ _ _ _ _)=title
preparedWindowText :: PreparedWindow -> BufferContent
preparedWindowText (PreparedWindow _ _ text _ _ _ _ _ _)=text
preparedWindowRows :: PreparedWindow -> WindowRows
preparedWindowRows (PreparedWindow _ _ _ rows _ _ _ _ _)=rows

-- | Worker-cached natural cell extent; querying it never scans content. Semantic
-- heading/script layouts override this extent with their own prepared width.
preparedWindowWidth :: PreparedWindow -> Int
preparedWindowWidth (PreparedWindow _ _ _ _ width _ _ _ _)=width

-- | Cached heading presence, forced during preparation; input never scans rows.
preparedWindowHasSections :: PreparedWindow -> Bool
preparedWindowHasSections (PreparedWindow _ _ _ _ _ _ sections _ _)=sections

-- | Cached semantic layout admission, forced by the preparation worker. Input
-- observes only these scalar flags; it never scans styled rows. Script geometry
-- is independent of the wide-heading preference.
preparedWindowNeedsLayout :: Bool -> PreparedWindow -> Bool
preparedWindowNeedsLayout wide (PreparedWindow _ _ _ _ _ _ sections scripts _)=scripts || wide && sections

-- | Explicit durable type/version. Ordinary prepared views are transient: their
-- text is never checkpointed implicitly. The host restores durable text as an
-- inert unavailable view; it does not invoke a plugin from the recovery parser.
preparedWindowRecovery :: PreparedWindow -> Maybe (Text,Int)
preparedWindowRecovery (PreparedWindow _ _ _ _ _ recovery _ _ _)=recovery

-- | /O(1)/. Absent metadata retains ordinary private text/copy behavior.
preparedWindowSemantics :: PreparedWindow -> Maybe TextSemantics
preparedWindowSemantics (PreparedWindow _ _ _ _ _ _ _ _ semantics)=fst <$> semantics

-- | /O(1)/. Cached passive message intervals include the original styled
-- newline decisions. StyledRows alone intentionally excludes newline furniture.
preparedWindowMessages :: PreparedWindow -> V.Vector (Int,Int,Int,Bool)
preparedWindowMessages (PreparedWindow _ _ _ _ _ _ _ _ semantics)=maybe V.empty snd semantics

-- | Copy in the same logical scalar space as selection. Message copy excludes
-- furniture, preserving decorated newlines and optional multi-message attribution.
copyPreparedSelection :: PreparedWindow -> Int -> Int -> Text
copyPreparedSelection prepared start end=case maybe CopyText textCopy (preparedWindowSemantics prepared) of
  CopyText->contentSlice source first (lastOffset-first)
  CopyMessages attribution->T.intercalate "\n\n" (map render groups)
    where
      cells=[(ident,outgoing,contentSlice source a (z-a))
        | (begin,finish,ident,outgoing)<-V.toList (preparedWindowMessages prepared)
        , let a=max first begin,let z=min lastOffset finish,a<z]
      groups=groupBy (\(a,_,_) (b,_,_)->a==b) cells
      render group@((_,outgoing,_):_)=
        (if attribution==UserBotAttribution && length groups>1 then if outgoing then "User: " else "Bot: " else "")<>
        T.concat [text | (_,_,text)<-group]
      render []=""
  where
    source=preparedWindowText prepared
    first=max 0 (min (contentLength source) start)
    lastOffset=max first (min (contentLength source) end)

messageIntervals :: [(Char,Style)] -> V.Vector (Int,Int,Int,Bool)
messageIntervals=V.fromList . go 0 Nothing []
  where
    go !_ current done []=reverse (maybe done (:done) current)
    go !offset current done ((_,style):rest)=case style of
      BubbleText ident outgoing _->case current of
        Just (a,_,previous,sent) | previous==ident && sent==outgoing->
          go (offset+1) (Just (a,offset+1,ident,outgoing)) done rest
        _->go (offset+1) (Just (offset,offset+1,ident,outgoing)) (maybe done (:done) current) rest
      _->go (offset+1) Nothing (maybe done (:done) current) rest

-- | /O(1)/. Explicit immutable observation declaration, private by default.
preparedWindowDisclosure :: PreparedWindow -> WindowDisclosure
preparedWindowDisclosure=maybe PrivateWindow textDisclosure . preparedWindowSemantics

-- | Prepare styled text and validate every semantic interval on its worker.
-- @0 <= start <= end <= contentLength preparedWindowText@ holds for every
-- returned range. Strings and vectors are forced here; adoption only reads refs.
prepareSemanticTextWindow :: Text -> [(Char,Style)] -> TextSemantics -> IO (Either Text PreparedWindow)
prepareSemanticTextWindow title styled semantics=do
  PreparedWindow ident caption text rows width recovery sections scripts _<-prepareStyledTextWindow title styled
  let valid (a,z)=a>=0 && a<=z && z<=contentLength text
      ranges=V.map (\(a,z,_)->(a,z)) (textLinks semantics) V.++
        V.map (\(a,z,_,_)->(a,z)) (textShellBlocks semantics) V.++
        textGuestHidden semantics V.++ textStreamerHidden semantics V.++ textRecoveryHidden semantics
  if not (V.all valid ranges) then pure (Left "Invalid prepared window semantic range.") else do
    _<-evaluate (maybe 0 (foldl' (\n c->c `seq` n+1) 0) (textLinkBase semantics)+
      V.foldl' (\n (a,z,url)->n+a+z+T.length url) 0 (textLinks semantics)+
      V.foldl' (\n (a,z,dialect,body)->n+a+z+T.length dialect+T.length body) 0 (textShellBlocks semantics)+
      V.foldl' (\n (a,z)->n+a+z) 0 ranges)
    -- Eligibility is immutable worker-prepared metadata; menu/launch admission
    -- must not rescan raw code bodies while holding the Desktop owner.
    let eligible=semantics {textShellBlocks=V.filter (\(_,_,_,body)->not (T.null (T.strip body))) (textShellBlocks semantics)}
        messages=case textCopy semantics of CopyText->V.empty; CopyMessages{}->messageIntervals styled
    _<-evaluate eligible
    _<-evaluate (V.foldl' (\n (a,z,message,outgoing)->outgoing `seq` n+a+z+message) 0 messages)
    result<-evaluate (PreparedWindow ident caption text rows width recovery sections scripts (Just (eligible,messages)))
    pure (Right result)

-- | Prepare text whose title and content may be written to private recovery.
-- Use only non-secret state declared durable by the view's owner. Type IDs are
-- namespaced command-style names; positive versions describe the stored format.
prepareRecoverableTextWindow :: Text -> Int -> Text -> Text -> IO (Either Text PreparedWindow)
prepareRecoverableTextWindow kind version title text
  | not (validCommandName kind) || T.length kind>128 || version<=0=pure (Left "Invalid durable plugin window type/version.")
  | otherwise=do
      PreparedWindow ident caption measured rows width _ sections scripts semantics<-prepareTextWindow title text
      pure (Right (PreparedWindow ident caption measured rows width (Just (kind,version)) sections scripts semantics))

-- | Prepare a fixed selectable list above one readonly Details pane on a worker.
-- Refresh preserves the selected ID if present; host geometry, draft selection
-- and scroll are independent of publication identity. Explicit menu references
-- scope row actions to this prepared view; readonly lists pass @[]@. Up to 16
-- distinct refs are allowed; they grant no execution or agent authority.
-- Details must be ordinary prepared text, never nested rows or styled layout.
-- At most 64 unique rows with control-free captions of at most 240 characters
-- and Details of at most 16384 characters are accepted. The fallback is a label
-- summary, never a duplicate Details body.
prepareRowsWindow :: Text -> [MenuRef] -> [WindowRow] -> IO (Either Text PreparedWindow)
prepareRowsWindow title references entries
  | length (take 17 references)>16 || length (nub references)/=length references=pure (Left "Window rows require at most 16 unique menu references.")
  | length (take 65 entries)>64=pure (Left "Too many window rows.")
  | otherwise=case validate entries M.empty 0 of
      Left err->pure (Left err)
      Right index->do
        -- Force captions, IDs and shape before the host can receive this page.
        _<-evaluate (M.size index)
        mapM_ evaluate references
        let rows=V.fromList entries
        _<-evaluate (V.foldl' (\n (WindowRow ident caption detail)->ident `seq` detail `seq` n+T.length caption) 0 rows)
        PreparedWindow ident caption text _ width recovery sections scripts semantics<-prepareTextWindow title
          (T.intercalate "\n" [label | WindowRow _ label _<-entries])
        pure (Right (PreparedWindow ident caption text (RowsDetails rows index references) width recovery sections scripts semantics))
  where
    validate [] index _=Right index
    validate (WindowRow ident caption detail:rest) index n
      | M.member ident index=Left "Duplicate window row ID."
      | T.length caption>240 || T.any (\c->c<' ' || c=='\DEL') caption=Left "Invalid window row caption."
      | contentLength (preparedWindowText detail)>16384=Left "Window row Details exceed 16384 characters."
      | PlainRows{}<-preparedWindowRows detail=validate rest (M.insert ident n index) (n+1)
      | otherwise=Left "Window row Details require plain prepared text."

-- | Durable rows restore only their private inert label summary; IDs, actions
-- and Details are not checkpointed or reconnected by recovery.
prepareRecoverableRowsWindow :: Text -> Int -> Text -> [MenuRef] -> [WindowRow] -> IO (Either Text PreparedWindow)
prepareRecoverableRowsWindow kind version title references entries
  | not (validCommandName kind) || T.length kind>128 || version<=0=pure (Left "Invalid durable plugin window type/version.")
  | otherwise=do
      result<-prepareRowsWindow title references entries
      pure $ case result of
        Left err->Left err
        Right (PreparedWindow ident caption text rows width _ sections scripts semantics)->
          Right (PreparedWindow ident caption text rows width (Just (kind,version)) sections scripts semantics)

instance Eq PreparedWindow where
  a==b=preparedWindowRef a==preparedWindowRef b
instance Show PreparedWindow where
  show prepared="PreparedWindow "++show (hashUnique (preparedWindowRef prepared))

-- | Prepare ordinary selectable text on the calling worker.
prepareTextWindow :: Text -> Text -> IO PreparedWindow
prepareTextWindow title text=do
  ident<-newUnique
  let measured=newBuffer text
      rows=V.fromList (map plainSourceRow (T.splitOn "\n" text))
      -- plainSourceRow counts UTF8 characters. Force that metadata and the cell
      -- extent here once, never while rendering a long line or using a scrollbar.
      width=V.foldl' (\longest row->let count=V.foldl' (\_ range->sourceRangeCharEnd range) 0 (sourceRowRanges row)
        in count `seq` max longest (sourceTextWidth (sourceRowText row))) 0 rows
  _<-evaluate width
  _<-evaluate (prepareBuffer measured)
  evaluate (PreparedWindow ident (safeTitle title) (bufferContent measured) (PlainRows rows) width Nothing False False Nothing)

-- | Prepare CommonMark at a requested cell width on the calling worker.
-- Copy addresses the laid-out semantic text, excluding host chrome.
prepareMarkdownWindow :: Int -> Text -> Text -> IO PreparedWindow
prepareMarkdownWindow columns title text=prepareStyledTextWindow title (renderMarkdown columns text)

-- | Prepare explicit semantic styled text on the calling worker. Source/copy
-- remains exactly the supplied characters; script hints do not insert markers.
-- This is the same row owner used by ordinary text and CommonMark preparation.
prepareStyledTextWindow :: Text -> [(Char,Style)] -> IO PreparedWindow
prepareStyledTextWindow title styled=do
  ident<-newUnique
  let rows=V.fromList (split styled)
  _<-evaluate (V.foldl' (\n row->foldl' (\m (c,s)->c `seq` s `seq` m+1) n row) (0::Int) rows)
  let source=T.pack (map fst styled)
      measured=newBuffer source
  _<-evaluate (prepareBuffer measured)
  let width=maximum (0:map sourceTextWidth (T.splitOn "\n" source))
  _<-evaluate width
  evaluate (PreparedWindow ident (safeTitle title) (bufferContent measured) (StyledRows rows) width Nothing (any (sectionTitle . snd) styled) (any (styleLayoutMetadata . snd) styled) Nothing)
  where
    split chars=case break ((=='\n').fst) chars of
      (line,[])->[line]
      (line,_:rest)->line:split rest

safeTitle :: Text -> Text
safeTitle=T.take 8192 . T.map (\c->if c<' ' || c=='\DEL' then ' ' else c)

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
    reference<-WindowRef <$> newUnique <*> pure scope <*> pure (preparedWindowDisclosure prepared) <*> newTVarIO (1,False)
    pure (Just (WindowUpdate reference 1 True prepared))

-- | Prepare a complete refresh for an exact adopted instance. Issuing a later
-- revision invalidates older queued publications. Host geometry and selection
-- are retained; refresh never opens a missing or closed window.
refreshTextWindow :: WindowRef -> PreparedWindow -> IO (Maybe WindowUpdate)
refreshTextWindow reference@(WindowRef _ (WindowScope scope) disclosure state) prepared=atomically $ do
  live<-readTVar scope
  (revision,opened)<-readTVar state
  if not live || revision<=0 || not opened || preparedWindowDisclosure prepared/=disclosure then pure Nothing else do
    let next=revision+1
    writeTVar state (next,opened)
    pure (Just (WindowUpdate reference next False prepared))

-- | Host lifetime check; observes scalar scope/instance state only.
windowRefCurrent :: WindowRef -> IO Bool
windowRefCurrent (WindowRef _ (WindowScope scope) _ state)=atomically $ do
  live<-readTVar scope
  (revision,_)<-readTVar state
  pure (live && revision>0)

-- | /O(1)/. Publication scope liveness independently of a closed frame. Typed
-- owners retain one draft binding until this scope or its registration retires;
-- closing a mount must not lose the receipt needed to release its hidden state.
windowScopeCurrent :: WindowRef -> IO Bool
windowScopeCurrent (WindowRef _ (WindowScope scope) _ _)=readTVarIO scope

-- | Host adoption primitive. The supplied flag records whether the exact view
-- is already installed; false cannot turn a refresh into an open operation.
-- A missing refresh retires its instance. Caller must check actor/modal policy
-- before invoking this function; this primitive grants no desktop capability.
admitWindowUpdate :: Bool -> WindowUpdate -> IO (Maybe (WindowRef,PreparedWindow))
admitWindowUpdate present (WindowUpdate reference@(WindowRef _ (WindowScope scope) disclosure state) revision opening prepared)=atomically $ do
  live<-readTVar scope
  (latest,opened)<-readTVar state
  if not live || latest<=0 || preparedWindowDisclosure prepared/=disclosure then pure Nothing
  else if not opening && not present then writeTVar state (0,False) >> pure Nothing
  else if latest/=revision || opening && (opened || present) || not opening && not opened then pure Nothing
  else writeTVar state (latest,True) >> pure (Just (reference,prepared))

-- | Host close invalidates the exact instance. Idempotent and callback-free.
retireWindowRef :: WindowRef -> IO ()
retireWindowRef (WindowRef _ _ _ state)=atomically (writeTVar state (0,False))

-- | A joint readonly body and typed editor attachment. Only this reply carrier
-- is parameterized; body content never retains an extension callback. It is an
-- opening, not an ordinary body refresh or an action replacement.
data EditorWindowUpdate c r = EditorWindowUpdate !WindowUpdate !(E.PreparedEditor c r)

-- | Prepare an exact body/attachment pair on the existing reply worker. The
-- mount is consumed only during joint host adoption, never while constructing a
-- reply. Publishing the same pair twice cannot create two editable frames.
openEditorWindow :: WindowScope -> PreparedWindow -> E.PreparedEditor c r -> IO (Maybe (EditorWindowUpdate c r))
openEditorWindow scope body editor=fmap (`EditorWindowUpdate` editor) <$> openTextWindow scope body

-- | /O(1)/. Readonly publication paired to this exact attachment.
editorWindowBody :: EditorWindowUpdate c r -> WindowUpdate
editorWindowBody (EditorWindowUpdate body _)=body

-- | /O(1)/. Callable binding retained only by the typed reply owner, never by
-- Desktop or prepared readonly content. Its initial Buffer transfers once.
editorWindowEditor :: EditorWindowUpdate c r -> E.PreparedEditor c r
editorWindowEditor (EditorWindowUpdate _ editor)=editor

-- | Host joint opening after current actor/geometry/budget and exact draft-owner
-- checks. One widget has one live attachment; another owner or a second frame
-- must be refused before this operation, without stealing/resetting its draft.
-- Check every
-- body condition before attempting the mount claim; after that claim succeeds,
-- the only remaining operation commits the body in the same transaction.
-- Returning Nothing cannot leave either lifetime partly adopted. Command
-- retirement can leave an inert view, but submission rechecks registration.
admitEditorWindowUpdate :: EditorWindowUpdate c r -> IO (Maybe (WindowRef,PreparedWindow,E.PreparedEditor c r))
admitEditorWindowUpdate (EditorWindowUpdate (WindowUpdate reference@(WindowRef _ (WindowScope scope) disclosure state) revision opening prepared) editor)=do
  current<-E.editorCurrent editor
  if not current then pure Nothing else atomically $ do
    live<-readTVar scope
    (latest,opened)<-readTVar state
    if not live || latest<=0 || latest/=revision || not opening || opened || preparedWindowDisclosure prepared/=disclosure then pure Nothing else do
      claimed<-E.claimEditorMount (E.editorMount editor)
      if not claimed then pure Nothing else do
        writeTVar state (latest,True)
        pure (Just (reference,prepared,editor))
