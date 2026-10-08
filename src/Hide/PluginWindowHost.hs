{-# LANGUAGE OverloadedStrings #-}
-- | Closed prepared-window adoption shared by existing menu/sidebar owners.
-- Workers retain preparation and cancellation; this adapter observes only exact
-- scope/instance metadata and never runs extension callbacks or scans text.
module Hide.PluginWindowHost (adoptWindowUpdate, replaceWindowUpdate, tickPluginWindows, retireClosedWindow, adoptEditorWindowUpdate, applyEditorUpdate, installEditorDraft) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Hide.Plugin.Canvas (imageResourceId,imageRGBA,fitCanvasView)
import Control.Monad (filterM)
import qualified Data.List as L (foldl')
import Hide.Buffer (Buffer,Selection(..),contentLength,contentLineCount)
import qualified Hide.Plugin.EditorHost as E
import Hide.Plugin.BufferHost (versionCurrent)
import qualified Hide.Plugin.Menu as P
import qualified Hide.Plugin.Window as W
import Hide.Model

-- | Adopt only a human publication after its calling owner revalidates the
-- originating command and captured target. Opening remains modal-protected; an
-- exact installed refresh changes only its content/local geometry, never focus
-- or modal input. Plugin text has no guest grant.
adoptWindowUpdate :: P.MenuOrigin -> W.WindowUpdate -> Desktop -> IO Desktop
adoptWindowUpdate origin update desktop
  | origin/=P.HumanMenu || not present && (dialog desktop/=Nothing || questionActive desktop || activeAutocomplete desktop)=pure desktop {status="Plugin window publication is protected."}
  | M.size (pluginWindows desktop)>=256 && not present=pure desktop {status="Plugin window budget reached."}
  | otherwise=do
      accepted<-W.admitWindowUpdate present update
      case accepted of
        Nothing->pure desktop {status="Plugin window publication expired."}
        Just (reference,prepared)
          | not (imageBudget (M.insert reference prepared (pluginWindows desktop)))->do
              if present then pure () else W.retireWindowRef reference
              pure desktop {status="Image window budget reached (64 windows / 64 MiB decoded)."}
          | present->pure desktop {pluginWindows=M.insert reference prepared (pluginWindows desktop),
              windows=map (clamp reference prepared) (windows desktop),
              drag=if (M.lookup reference (pluginWindows desktop) >>= W.preparedWindowImage)==W.preparedWindowImage prepared
                then drag desktop else cancelImagePan reference desktop}
          | otherwise->pure (addPluginWindow reference prepared desktop)
  where present=M.member (W.updateWindowRef update) (pluginWindows desktop)

-- | Replace an owner-held output slot with a fresh content lifetime. Exact live
-- old identity and a fresh admitted opening are required; labels are irrelevant.
-- Installed geometry/numbering/focus and unrelated modal input are preserved.
-- Absence/retirement follows ordinary protected opening, never resurrecting old
-- content. Old queued refreshes cannot update the fresh replacement.
replaceWindowUpdate :: P.MenuOrigin -> W.WindowRef -> W.WindowUpdate -> Desktop -> IO Desktop
replaceWindowUpdate origin old update desktop
  | origin/=P.HumanMenu=pure desktop {status="Plugin window publication is protected."}
  | W.updateWindowRef update==old || M.member (W.updateWindowRef update) (pluginWindows desktop)=
      pure desktop {status="Plugin window replacement requires a fresh instance."}
  | not (M.member old (pluginWindows desktop)) || not (any ((==PluginContent old) . windowContent) (windows desktop))=
      adoptWindowUpdate origin update desktop
  | otherwise=do
      live<-W.windowRefCurrent old
      if not live then adoptWindowUpdate origin update desktop else do
        accepted<-W.admitWindowUpdate False update
        case accepted of
          Nothing->pure desktop {status="Plugin window publication expired."}
          Just (reference,prepared) | not (imageBudget (M.insert reference prepared (M.delete old (pluginWindows desktop))))->do
            W.retireWindowRef reference
            pure desktop {status="Image window budget reached (64 windows / 64 MiB decoded)."}
          Just (reference,prepared)->do
            W.retireWindowRef old
            mapM_ E.retireEditorMount [mount | w<-windows desktop,windowContent w==PluginContent old,Just mount<-[windowEditorMount w]]
            pure desktop {pluginWindows=M.insert reference prepared (M.delete old (pluginWindows desktop)),
              retiredPluginWindows=S.delete old (retiredPluginWindows desktop),windows=map (replace reference prepared) (windows desktop),
              drag=cancelImagePan old desktop}
  where
    replace reference prepared w | windowContent w==PluginContent old=w {windowContent=PluginContent reference,windowEditorMount=Nothing,
      selection=Selection 0 0,scrollRow=0,scrollColumn=0,rowsInteraction=initialRowsInteraction prepared,imageViewport=fitCanvasView}
    replace _ _ w=w

-- A captured gesture belongs to the admitted image. A same-resource refresh
-- keeps it; changing the resource or replacing its instance cancels it.
cancelImagePan :: W.WindowRef -> Desktop -> Maybe Drag
cancelImagePan reference desktop=case drag desktop of
  Just (ImagePanning wid _ _ _) | any (\w->windowId w==wid && windowContent w==PluginContent reference) (windows desktop)->Nothing
  captured->captured

-- Scalar scope checks are bounded by the 256-view admission limit. Retirement
-- leaves a selectable read-only snapshot; no stale plugin request can revive it.
tickPluginWindows :: Desktop -> IO Desktop
tickPluginWindows desktop=do
  retired<-filterM (fmap not . W.windowRefCurrent) (M.keys (pluginWindows desktop))
  let dead=S.fromList retired
      newlyRetired=S.difference dead (retiredPluginWindows desktop)
      released=L.foldl' (\installed reference->M.adjust W.retirePreparedImage reference installed)
        (pluginWindows desktop) (S.toList newlyRetired)
  pure desktop {retiredPluginWindows=dead,pluginWindows=released}

-- The prepared resource accessor and ByteString length are constant-time;
-- shared immutable resources count once, while every image instance counts.
imageBudget :: M.Map W.WindowRef W.PreparedWindow -> Bool
imageBudget prepared=length images<=64 && sum (map (BS.length . imageRGBA) (M.elems unique))<=67108864
  where
    images=[image | body<-M.elems prepared,Just image<-[W.preparedWindowImage body]]
    unique=M.fromList [(imageResourceId image,image) | image<-images]

clamp :: W.WindowRef -> W.PreparedWindow -> Window -> Window
clamp reference prepared window
  | windowContent window/=PluginContent reference=window
  | otherwise=next {selection=Selection (limit (anchor selected)) (limit (caret selected)),
      scrollRow=min (scrollRow next) (max 0 (contentLineCount text-1)),scrollColumn=min (scrollColumn next) (W.preparedWindowWidth detail)}
  where
    interaction=case (W.preparedWindowRows prepared,rowsInteraction window) of
      (W.RowsDetails _ index _,Just old@(RowsInteraction ident _)) | M.member ident index->Just old
      _->initialRowsInteraction prepared
    changed=fmap (\(RowsInteraction ident _)->ident) interaction/=fmap (\(RowsInteraction ident _)->ident) (rowsInteraction window)
    next=if changed then window {rowsInteraction=interaction,selection=Selection 0 0,scrollRow=0,scrollColumn=0} else window {rowsInteraction=interaction}
    detail=case (W.preparedWindowRows prepared,interaction) of
      (W.RowsDetails rows index _,Just (RowsInteraction ident _))->case M.lookup ident index >>= (rows V.!?) of
        Just (W.WindowRow _ _ value)->value; _->prepared
      _->prepared
    selected=selection next
    text=W.preparedWindowText detail
    limit=max 0 . min (contentLength text)

-- | A close effect is harmless unless the exact view is already absent. The
-- core close operation owns geometry removal; this retires only its capability.
retireClosedWindow :: W.WindowRef -> Desktop -> IO Desktop
retireClosedWindow reference desktop=do
  if M.member reference (pluginWindows desktop) then pure () else W.retireWindowRef reference
  pure desktop

-- | One frame per draft and one owning callable binding. A retained previous
-- mount proves remount ownership; labels or a matching draft alone never do.
adoptEditorWindowUpdate :: P.MenuOrigin -> Maybe E.EditorMount -> W.EditorWindowUpdate c r -> Desktop -> IO (Bool,Desktop)
adoptEditorWindowUpdate origin previous update d
  | origin/=P.HumanMenu || dialog d/=Nothing || questionActive d || activeAutocomplete d=
      pure (False,d {status="Editor window publication is protected."})
  | M.size (pluginWindows d)>=256 || M.member reference (pluginWindows d)=
      pure (False,d {status="Editor window requires a fresh available frame."})
  | any ((==Just draft).fmap E.mountDraft.windowEditorMount) (windows d)=
      pure (False,d {status="Editor draft already has a visible frame."})
  | Just retained<-M.lookup draft (editorDrafts d),editorDraftMount retained/=previous || previous==Nothing=
      pure (False,d {status="Editor draft belongs to another owner."})
  | M.notMember draft (editorDrafts d),Nothing<-E.editorInitialBuffer editor=
      pure (False,d {status="Editor draft has no initial state."})
  | otherwise=do
      admitted<-W.admitEditorWindowUpdate update
      pure $ case admitted of
        Nothing->(False,d {status="Editor window publication expired."})
        Just (ref,body,_)->let opened=addPluginWindow ref body (installEditorDraft mount (E.editorInitialBuffer editor) d)
          in (True,modifyActive (\w->w {windowEditorMount=Just mount}) opened)
  where
    editor=W.editorWindowEditor update
    mount=E.editorMount editor
    draft=E.mountDraft mount
    reference=W.updateWindowRef (W.editorWindowBody update)

-- Transfer a preparation/recovery seed exactly once; remounts preserve Buffer,
-- Undo, selection and focus. The callable owner retains no duplicate seed.
installEditorDraft :: E.EditorMount -> Maybe Buffer -> Desktop -> Desktop
installEditorDraft mount seed d=d {editorDrafts=M.alter install ref (editorDrafts d)}
  where
    ref=E.mountDraft mount
    install (Just old)=Just old {editorDraftMount=Just mount}
    install Nothing=fmap (\b->EditorDraft b (Selection 0 0) True (Just mount)) seed

-- | Match the submitted job as well as its hidden draft's immutable version.
-- Stale completion is consumed once but cannot clear a replacement or another
-- action's draft. This is the only Buffer write in prepared result adoption.
applyEditorUpdate :: E.DraftSubmission -> E.EditorUpdate -> Desktop -> IO Desktop
applyEditorUpdate submitted update d
  | E.updateSubmission update/=submitted=pure d {status="Editor result does not match its submission."}
  | otherwise=case M.lookup (E.submissionDraft submitted) (editorDrafts d) of
      Nothing->pure d
      Just draft->do
        current<-versionCurrent (E.submissionVersion submitted) (editorDraftBuffer draft)
        consumed<-E.consumeEditorUpdate update
        pure $ if not current || not consumed then d else d {editorDrafts=M.insert (E.submissionDraft submitted)
          draft {editorDraftBuffer=E.updateReplacement update,editorDraftSelection=Selection 0 0} (editorDrafts d)}
