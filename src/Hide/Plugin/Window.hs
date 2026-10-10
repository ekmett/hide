-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- |
-- Module      : Hide.Plugin.Window
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
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
  ( WindowRef, WindowScope, WindowUpdate, withWindowScope, openWindow, refreshWindow
  , windowRefIdentity, updateWindowRef, admitWindowUpdate, windowRefCurrent, windowScopeCurrent, retireWindowRef
  , EditorWindowUpdate, openEditorWindow, editorWindowBody, editorWindowEditor, admitEditorWindowUpdate
  , PreparedWindow, prepareImageWindow, preparedWindowImage, retirePreparedImage, prepareTextWindow, prepareMarkdownWindow, prepareStyledTextWindow, prepareStyledRowsWindow, prepareSemanticTextWindow, prepareSemanticRowsWindow, prepareRecoverableTextWindow
  , WindowDisclosure(..), TextCopy(..), MessageAttribution(..), TextSemantics(..), preparedWindowSemantics, preparedWindowDisclosure, preparedWindowMessages, copyPreparedSelection
  , WindowRow(..), prepareRowsWindow, prepareRecoverableRowsWindow
  , WindowRows(..), preparedWindowTitle, preparedWindowText, preparedWindowRows, preparedWindowWidth, preparedWindowHasSections, preparedWindowNeedsLayout, preparedWindowRecovery
  ) where

import Control.Exception (evaluate, bracket)
import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Hide.Plugin.Canvas (PreparedImage, prepareImage, imageFormat, imageWidth, imageHeight)
import Data.Unique (Unique, newUnique, hashUnique)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Hide.Plugin.Tree (NodeId)
import Hide.Plugin.Menu (MenuRef)
import Data.List (nub,groupBy)
import qualified Data.List as L (foldl')
import Hide.Buffer (contentLength,contentSlice,BufferContent, bufferContent, newBuffer, prepareBuffer)
import Hide.Unicode (sourceTextWidth)
import Hide.Markdown (renderMarkdownRows)
import Hide.Plugin.Command (validCommandName)
import qualified Hide.Plugin.EditorHost as E
import Hide.Syntax (Style(..),StyledText,StyledRow(..),Sigils(..),styledRows,sigilsText,sigilsLength,sigilsStyles,SourceRow,plainSourceRow,sourceRowText,sourceRowRanges,sourceRangeCharEnd,sectionTitle,styleLayoutMetadata)

-- | Exact content instance. A closed/reopened view cannot reuse this identity.
data WindowRef = WindowRef Unique WindowScope !WindowDisclosure (TVar (Integer,Bool))
instance Eq WindowRef where
  WindowRef a _ _ _==WindowRef b _ _ _=a==b
instance Ord WindowRef where
  compare (WindowRef a _ _ _) (WindowRef b _ _ _)=compare a b
instance Show WindowRef where
  show (WindowRef ident _ _ _)="WindowRef "++show (hashUnique ident)

-- | /O(1)/. Public instance receipt, never a credential or permission grant.
-- Closing/reopening a view allocates a fresh identity even with the same pixels.
windowRefIdentity :: WindowRef -> Text
windowRefIdentity (WindowRef ident _ _ _)=T.pack (show (hashUnique ident))

-- | Fully prepared immutable text and styled display rows. Equality observes
-- the unique prepared identity only, never text or styled payloads.
-- | One stable selectable row and its already-prepared plain Details snapshot.
-- IDs are local to the containing WindowRef; labels never identify actions.
data WindowRow = WindowRow !NodeId !Text !PreparedWindow
data WindowRows = PlainRows !(V.Vector SourceRow) | StyledRows !(V.Vector StyledRow)
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

data PreparedWindow = PreparedWindow !Unique !Text !BufferContent !WindowRows !Int !(Maybe (Text,Int)) !Bool !Bool !(Maybe (TextSemantics,V.Vector (Int,Int,Int,Bool))) !(Maybe PreparedImage)
preparedWindowRef :: PreparedWindow -> Unique
preparedWindowRef (PreparedWindow ident _ _ _ _ _ _ _ _ _)=ident
preparedWindowTitle :: PreparedWindow -> Text
preparedWindowTitle (PreparedWindow _ title _ _ _ _ _ _ _ _)=title
preparedWindowText :: PreparedWindow -> BufferContent
preparedWindowText (PreparedWindow _ _ text _ _ _ _ _ _ _)=text
preparedWindowRows :: PreparedWindow -> WindowRows
preparedWindowRows (PreparedWindow _ _ _ rows _ _ _ _ _ _)=rows

-- | Worker-cached natural cell extent; querying it never scans content. Semantic
-- heading/script layouts override this extent with their own prepared width.
preparedWindowWidth :: PreparedWindow -> Int
preparedWindowWidth (PreparedWindow _ _ _ _ width _ _ _ _ _)=width

-- | Cached heading presence, forced during preparation; input never scans rows.
preparedWindowHasSections :: PreparedWindow -> Bool
preparedWindowHasSections (PreparedWindow _ _ _ _ _ _ sections _ _ _)=sections

-- | Cached semantic layout admission, forced by the preparation worker. Input
-- observes only these scalar flags; it never scans styled rows. Script geometry
-- is independent of the wide-heading preference.
preparedWindowNeedsLayout :: Bool -> PreparedWindow -> Bool
preparedWindowNeedsLayout wide (PreparedWindow _ _ _ _ _ _ sections scripts _ _)=scripts || wide && sections

-- | Explicit durable type/version. Ordinary prepared views are transient: their
-- text is never checkpointed implicitly. The host restores durable text as an
-- inert unavailable view; it does not invoke a plugin from the recovery parser.
preparedWindowRecovery :: PreparedWindow -> Maybe (Text,Int)
preparedWindowRecovery (PreparedWindow _ _ _ _ _ recovery _ _ _ _)=recovery

-- | /O(1)/. Absent metadata retains ordinary private text/copy behavior.
preparedWindowSemantics :: PreparedWindow -> Maybe TextSemantics
preparedWindowSemantics (PreparedWindow _ _ _ _ _ _ _ _ semantics _)=fst <$> semantics

-- | /O(1)/. Cached passive message intervals include the original styled
-- newline decisions. StyledRows alone intentionally excludes newline furniture.
preparedWindowMessages :: PreparedWindow -> V.Vector (Int,Int,Int,Bool)
preparedWindowMessages (PreparedWindow _ _ _ _ _ _ _ _ semantics _)=maybe V.empty snd semantics

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

messageIntervals :: [StyledRow] -> V.Vector (Int,Int,Int,Bool)
messageIntervals rows=V.fromList (reverse (L.foldl' merge [] (reverse pieces)))
  where
    (_,pieces)=L.foldl' row (0,[]) rows
    row (offset,found) (StyledRow sigils newline messages)=
      let size=sigilsLength sigils
          shifted=[(offset+a,offset+z,ident,outgoing) | (a,z,ident,outgoing)<-V.toList messages]
          ending=case newline of Just (BubbleText ident outgoing _)->[(offset+size,offset+size+1,ident,outgoing)]; _->[]
      in (offset+size+maybe 0 (const 1) newline,reverse ending++reverse shifted++found)
    merge ((a,z,ident,outgoing):rest) (b,end,other,sent)
      | z==b && ident==other && outgoing==sent=(a,end,ident,outgoing):rest
    merge done piece=piece:done

-- | /O(1)/. Explicit immutable observation declaration, private by default.
preparedWindowDisclosure :: PreparedWindow -> WindowDisclosure
preparedWindowDisclosure=maybe PrivateWindow textDisclosure . preparedWindowSemantics

-- | Worker-only bounded PNG/JPEG preparation. The returned window is read-only
-- and independently disclosed; opening uses the existing scoped publication.
-- Recovery keeps only its inert description, never source bytes or pixels.
-- Decode work never runs during adoption or rendering.
prepareImageWindow :: Text -> WindowDisclosure -> Maybe FilePath -> BS.ByteString -> IO (Either Text PreparedWindow)
prepareImageWindow title disclosure origin encoded=do
  decoded<-prepareImage encoded
  case decoded of
    Left err->pure (Left err)
    Right image->do
      let prefix=safeTitle title<>"\n"<>imageFormat image<>" "<>T.pack (show (imageWidth image))<>" × "<>T.pack (show (imageHeight image))<>
            "\nF: Fit   1: 100%   +/− or wheel: zoom\nArrows or drag: pan\n"
          fallback=prefix<>"Open externally"
          links=case origin of Nothing->V.empty; Just _->V.singleton (T.length prefix,T.length fallback,"")
      PreparedWindow ident caption text rows width _ sections scripts _ _<-prepareTextWindow title fallback
      let semantics=TextSemantics CopyText origin links V.empty disclosure V.empty V.empty V.empty
      pure (Right (PreparedWindow ident caption text rows width (Just ("hide.image",1)) sections scripts (Just (semantics,V.empty)) (Just image)))

-- | O(1). The immutable resource, absent for text or retired image windows.
preparedWindowImage :: PreparedWindow -> Maybe PreparedImage
preparedWindowImage (PreparedWindow _ _ _ _ _ _ _ _ _ image)=image

-- | O(1). Drop image bytes while preserving the inert text snapshot and identity.
-- Retirement cannot resurrect this resource through a stale refresh.
retirePreparedImage :: PreparedWindow -> PreparedWindow
retirePreparedImage prepared | Nothing<-preparedWindowImage prepared=prepared
retirePreparedImage (PreparedWindow ident title text rows width recovery sections scripts semantics _)=
  PreparedWindow ident title text rows width recovery sections scripts semantics Nothing

-- | Prepare styled text and validate every semantic interval on its worker.
-- @0 <= start <= end <= contentLength preparedWindowText@ holds for every
-- returned range. Strings and vectors are forced here; adoption only reads refs.
prepareSemanticTextWindow :: Text -> StyledText -> TextSemantics -> IO (Either Text PreparedWindow)
prepareSemanticTextWindow title styled=prepareSemanticRowsWindow title (styledRows styled)

-- | Prepare already-finalized demanded rows without segmenting them again.
-- Semantic intervals address their exact original scalar text, including the
-- retained styled newline decisions.
prepareSemanticRowsWindow :: Text -> [StyledRow] -> TextSemantics -> IO (Either Text PreparedWindow)
prepareSemanticRowsWindow title styled semantics=do
  PreparedWindow ident caption text rows width recovery sections scripts _ _<-prepareStyledRowsWindow title styled
  let valid (a,z)=a>=0 && a<=z && z<=contentLength text
      ranges=V.map (\(a,z,_)->(a,z)) (textLinks semantics) V.++
        V.map (\(a,z,_,_)->(a,z)) (textShellBlocks semantics) V.++
        textGuestHidden semantics V.++ textStreamerHidden semantics V.++ textRecoveryHidden semantics
  if not (V.all valid ranges) then pure (Left "Invalid prepared window semantic range.") else do
    _<-evaluate (maybe 0 (L.foldl' (\n c->c `seq` n+1) 0) (textLinkBase semantics)+
      V.foldl' (\n (a,z,url)->n+a+z+T.length url) 0 (textLinks semantics)+
      V.foldl' (\n (a,z,dialect,body)->n+a+z+T.length dialect+T.length body) 0 (textShellBlocks semantics)+
      V.foldl' (\n (a,z)->n+a+z) 0 ranges)
    -- Eligibility is immutable worker-prepared metadata; menu/launch admission
    -- must not rescan raw code bodies while holding the Desktop owner.
    let eligible=semantics {textShellBlocks=V.filter (\(_,_,_,body)->not (T.null (T.strip body))) (textShellBlocks semantics)}
        messages=case textCopy semantics of CopyText->V.empty; CopyMessages{}->messageIntervals styled
    _<-evaluate eligible
    _<-evaluate (V.foldl' (\n (a,z,message,outgoing)->outgoing `seq` n+a+z+message) 0 messages)
    result<-evaluate (PreparedWindow ident caption text rows width recovery sections scripts (Just (eligible,messages)) Nothing)
    pure (Right result)

-- | Prepare text whose title and content may be written to private recovery.
-- Use only non-secret state declared durable by the view's owner. Type IDs are
-- namespaced command-style names; positive versions describe the stored format.
-- Readability is an explicit observation grant; recovery restores inert private
-- text and never reconnects the provider or restores input authority.
prepareRecoverableTextWindow :: Text -> Int -> WindowDisclosure -> Text -> Text -> IO (Either Text PreparedWindow)
prepareRecoverableTextWindow kind version disclosure title text
  | not (validCommandName kind) || T.length kind>128 || version<=0=pure (Left "Invalid durable plugin window type/version.")
  | otherwise=do
      PreparedWindow ident caption measured rows width _ sections scripts _ _<-prepareTextWindow title text
      let semantics=TextSemantics CopyText Nothing V.empty V.empty disclosure V.empty V.empty V.empty
      pure (Right (PreparedWindow ident caption measured rows width (Just (kind,version)) sections scripts (Just (semantics,V.empty)) Nothing))

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
  | any (\(WindowRow _ _ detail)->preparedWindowImage detail/=Nothing) entries=pure (Left "Image windows cannot be row details.")
  | otherwise=case validate entries M.empty 0 of
      Left err->pure (Left err)
      Right index->do
        -- Force captions, IDs and shape before the host can receive this page.
        _<-evaluate (M.size index)
        mapM_ evaluate references
        let rows=V.fromList entries
        _<-evaluate (V.foldl' (\n (WindowRow ident caption detail)->ident `seq` detail `seq` n+T.length caption) 0 rows)
        PreparedWindow ident caption text _ width recovery sections scripts semantics _<-prepareTextWindow title
          (T.intercalate "\n" [label | WindowRow _ label _<-entries])
        pure (Right (PreparedWindow ident caption text (RowsDetails rows index references) width recovery sections scripts semantics Nothing))
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
        Right (PreparedWindow ident caption text rows width _ sections scripts semantics _)->
          Right (PreparedWindow ident caption text rows width (Just (kind,version)) sections scripts semantics Nothing)

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
  evaluate (PreparedWindow ident (safeTitle title) (bufferContent measured) (PlainRows rows) width Nothing False False Nothing Nothing)

-- | Prepare CommonMark at a requested cell width on the calling worker.
-- Copy addresses the laid-out semantic text, excluding host chrome.
prepareMarkdownWindow :: Int -> Text -> Text -> IO PreparedWindow
prepareMarkdownWindow columns title text=prepareStyledRowsWindow title (renderMarkdownRows columns text)

-- | Prepare explicit semantic styled text on the calling worker. Source/copy
-- remains exactly the supplied characters; script hints do not insert markers.
-- This is the same row owner used by ordinary text and CommonMark preparation.
prepareStyledTextWindow :: Text -> StyledText -> IO PreparedWindow
prepareStyledTextWindow title=prepareStyledRowsWindow title . styledRows

-- | Prepare exactly the supplied finalized rows. The caller owns the demand
-- boundary: a conversation supplies its visible slice, never one strict Sigils
-- chain for the complete history. Ordinary public text windows may supply all
-- their rows. Original text and newline metadata remain the copy projection.
prepareStyledRowsWindow :: Text -> [StyledRow] -> IO PreparedWindow
prepareStyledRowsWindow title styled=do
  ident<-newUnique
  let rows=V.fromList styled
      textOfRow (StyledRow sigils newline _)=sigilsText sigils<>maybe "" (const "\n") newline
      source=T.concat (map textOfRow styled)
      measured=newBuffer source
      styles (StyledRow sigils newline _)=sigilsStyles sigils++maybe [] pure newline
      metadata predicate=any (any predicate . styles) styled
      width=V.foldl' (\longest (StyledRow sigils _ _)->max longest (extent 0 sigils)) 0 rows
      extent !col Nil=col
      extent !col (ConsChars text _ rest)=extent (col+T.length text) rest
      extent !col (ConsSigil _ _ advance rest)=extent (col+advance) rest
  _<-evaluate (V.foldl' (\n (StyledRow sigils newline _)->n+sigilsLength sigils+maybe 0 (\style->style `seq` 1) newline) 0 rows)
  _<-evaluate (prepareBuffer measured)
  _<-evaluate width
  evaluate (PreparedWindow ident (safeTitle title) (bufferContent measured) (StyledRows rows) width Nothing (metadata sectionTitle) (metadata styleLayoutMetadata) Nothing Nothing)

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
openWindow :: WindowScope -> PreparedWindow -> IO (Maybe WindowUpdate)
openWindow scope@(WindowScope live) prepared=do
  current<-readTVarIO live
  if not current then pure Nothing else do
    reference<-WindowRef <$> newUnique <*> pure scope <*> pure (preparedWindowDisclosure prepared) <*> newTVarIO (1,False)
    pure (Just (WindowUpdate reference 1 True prepared))

-- | Prepare a complete refresh for an exact adopted instance. Issuing a later
-- revision invalidates older queued publications. Host geometry and selection
-- are retained; refresh never opens a missing or closed window.
refreshWindow :: WindowRef -> PreparedWindow -> IO (Maybe WindowUpdate)
refreshWindow reference@(WindowRef _ (WindowScope scope) disclosure state) prepared=atomically $ do
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
openEditorWindow _ body _ | preparedWindowImage body/=Nothing=pure Nothing
openEditorWindow scope body editor=fmap (`EditorWindowUpdate` editor) <$> openWindow scope body

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
