{-# LANGUAGE OverloadedStrings #-}
module InlineRenderCheck (checks) where

import SourceWindowFixture (sourceFixtureBuffer)
import EditorFixture (installAutocompleteFixture)
import Control.Exception (evaluate)
import Control.Monad (forM_,unless)
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Graphics.Vty.Span (SpanOp(..))
import Hide.Buffer
import Hide.Files (FileState(..))
import Hide.InlineState
import Hide.InlineTypes
import Hide.Sidebar
import Hide.Model
import Hide.Render
import qualified Hide.Plugin.Editor as E
import qualified Hide.Plugin.Window as W
import Hide.Syntax
import Hide.Unicode (displayOpsForPic)

checks :: IO ()
checks=W.withWindowScope $ \scope->do
  ref<-E.newDraftRef
  chatBody<-W.prepareTextWindow "Chat" ""
  let source="before\nlet old tail\nfollowing\nlast"
      b=newBuffer source
      raw=addDocument (Just (FileState "Preview.hs" Nothing)) b (initialDesktop (80,25))
      base=modifyActive (\win->win {selection=Selection 11 11,bounds=Rect 0 1 70 18}) raw
      w=fromJust (activeWindow base)
      colored=base {buffers=M.adjust (\doc->doc {documentSourceRows=Just (Vec.fromList
        [prepareSourceRow row [(T.singleton c,if c=='l' then Keyword else Literal) | c<-T.unpack row] | row<-T.lines source])}) (sourceFixtureBuffer w) (buffers base)}
      preview option=colored {inlinePreview=Just (InlineView (windowId w) (sourceFixtureBuffer w) (revision b) (selection w) (inlineEpoch colored) [option] 0)}
      rows d=let win=fromJust (activeWindow d); Rect x y width' _=bounds win
             in map (\line->let content=T.drop (columnOffset line (x+1)) line in T.stripEnd (T.take (columnOffset content (width'-2)) content)) (take 8 (drop (y+1) (T.lines (snapshot d))))
  option<-either (error . T.unpack) pure (prepareOption b (Proposal 11 14 "new\nsecond" Nothing))
  let shown=preview option
  let acceptRect=head [rect | (rect,_,Right (V.EvKey (V.KChar '\t') []))<-statusItemRects shown]
      (clicked,clickEffects)=handleEvent (V.EvMouseDown (left acceptRect+1) (top acceptRect) V.BLeft []) shown
  check "clicking inline Accept dispatches against the displayed proposal"
    (clickEffects==[AutocompleteAction "accept" []] && inlinePreview clicked/=Nothing)
  check "multiline inline proposal preserves context and shifts following source rows"
    (take 5 (rows shown)==["before","let new","second tail","following","last"])
  check "inline proposal leaves the source and ordinary caret untouched"
    (contents (documentBuffer (fromJust (activeDocument shown)))==source &&
      V.picCursor (renderDesktop shown)==V.picCursor (renderDesktop colored))
  check "changed text is gray and unchanged source keeps syntax colors"
    (hasColor "new" (V.RGBColor 170 170 170) shown && hasColor "secon" (V.RGBColor 170 170 170) shown &&
      hasColor "l" (V.RGBColor 255 255 255) shown && hasColor "et " (V.RGBColor 85 255 85) shown &&
      hasColor "d tai" (V.RGBColor 85 255 85) shown)
  forM_ [shown {inlineEpoch=inlineEpoch shown+1},
         modifyActive (\win->win {selection=Selection 12 12}) shown,
         shown {buffers=M.adjust (\doc->doc {documentBuffer=replaceSelection (Selection 0 0) "edited " b}) (sourceFixtureBuffer w) (buffers shown)},
         shown {menu=Just (0,0)},shown {problemsFocused=True}] $ \stale->
    check "stale or obstructed inline preview is not rendered" (snapshot stale==snapshot stale {inlinePreview=Nothing})
  let split=shown {windows=[w {bounds=Rect 0 1 35 18},w {windowId=windowId w+1,bounds=Rect 36 1 35 18}]}
      splitRow=T.lines (snapshot split)!!3
  check "only the active split displays the preview"
    ("let new" `T.isInfixOf` T.take 35 splitRow && "let old tail" `T.isInfixOf` T.drop 36 splitRow)
  let scrolled=modifyActive (\win->win {scrollRow=2,scrollColumn=2}) shown
  check "preview viewport cropping maps back to the proper source rows"
    (take 3 (rows scrolled)==["cond tail","llowing","st"])
  deletion<-either (error . T.unpack) pure (prepareOption b (Proposal 7 20 "" Nothing))
  check "multiline deletion maps subsequent rows without blank phantom lines"
    (take 3 (rows (preview deletion))==["before","following","last"])
  tabs<-either (error . T.unpack) pure (prepareOption b (Proposal 11 14 "x\t😀" Nothing))
  check "tabs use the whole projected row column and Unicode remains intact"
    (rows (preview tabs)!!1=="let x   😀 tail")
  let opaqueHistory=shown {buffers=M.adjust (\doc->doc {documentBuffer=b
        {undoStack=error "inline rendering forced Undo history",redoStack=error "inline rendering forced Redo history"}}) (sourceFixtureBuffer w) (buffers shown)}
  check "inline rendering does not inspect edit history" (rows opaqueHistory==rows shown)
  -- Identity capture must not inspect any option text or list tail.
  let opaque=shown {inlinePreview=fmap (\v->v {inlineOptions=error "render key forced inline proposals"}) (inlinePreview shown)}
  key<-renderKey opaque
  same<-renderKey opaque
  check "inline redraw identity never forces proposals" =<< evaluate (key==same)
  forM_ [opaque {inlinePreview=Nothing},opaque {inlineEpoch=inlineEpoch opaque+1},
         opaque {inlinePreview=fmap (\v->v {inlineIndex=inlineIndex v+1}) (inlinePreview opaque)}] $ \changed->do
    next<-renderKey changed
    check "inline identity, dismissal and validation epoch invalidate redraw" (key/=next)
  hintBase<-installAutocompleteFixture scope "completion trace" colored
  let hint=hintBase {autocompleteACPEnabled=True,autocompleteDraft=newBuffer "    hint",autocompleteSelection=Selection 2 2,
        autocompleteFocused=True,inlinePreview=Nothing,
        conversationViews=M.singleton "" (ConversationView (InertBody chatBody) "Chat" ref Nothing Nothing FollowEnd 0 0 Nothing Nothing Nothing Nothing),
        editorDrafts=M.singleton ref (EditorDraft (newBuffer "CHAT_ONLY") (Selection 0 0) True Nothing)}
      hintRect=autocompleteComposerRect hint (fromJust (activeWindow hint))
  check "ACP hint composer retains plain indentation and does not render the chat draft"
    ("    hint" `T.isInfixOf` snapshot hint && not ("CHAT_ONLY" `T.isInfixOf` snapshot hint) &&
      V.picCursor (renderDesktop hint)==V.Cursor (left hintRect+2) (top hintRect))
  check "disabling ACP hides the independent hint composer"
    (not ("hint" `T.isInfixOf` snapshot hint {autocompleteACPEnabled=False}))
  hintKey<-renderKey hint
  hintSame<-renderKey hint
  check "unchanged hint draft keeps redraw identity" (hintKey==hintSame)
  forM_ [hint {autocompleteWindow=Nothing},hint {autocompleteDraft=newBuffer "other hint"},hint {autocompleteSelection=Selection 3 3},
         hint {autocompleteFocused=False},hint {autocompleteACPEnabled=False}] $ \changed->do
    next<-renderKey changed
    check "hint contents, selection, focus and availability invalidate redraw" (hintKey/=next)
  autocomplete<-installAutocompleteFixture scope "Completion transcript" colored
  let acpId=nextId colored
      terminal=addDocument Nothing (newBuffer "Terminal output") autocomplete
      terminalId=nextId autocomplete
      docked=layoutBottomWindows terminal
        {buffers=M.adjust (\doc->doc {documentLabel=Just "Terminal 1"}) terminalId (buffers terminal),
         dockedTerminals=M.fromList [(ident,(Rect 0 1 30 8,Nothing)) | ident<-[acpId,terminalId]],
         bottomTerminal=Just acpId,problemsVisible=True}
      sourceFocused=focusWindow (windowId w) docked
      borders d=let Rect x y _ h=problemsRect d; picture=T.lines (snapshot d)
                in [T.index (picture!!rowIndex) x | rowIndex<-[y,y+1,y+h-1]]
  forM_ [acpId,terminalId] $ \ident->do
    let unfocused=sourceFocused {bottomTerminal=Just ident}
        focused=focusWindow ident unfocused
    check "unfocused docked window tab strip and body use single borders" (borders unfocused=="┌│└")
    check "focused docked window tab strip and body use double borders" (borders focused=="╔║╚")
  let messages=sourceFocused {bottomTerminal=Nothing}
  check "unfocused Messages tab strip and body use single borders" (borders messages=="┌│└")
  check "focused Messages tab strip and body use double borders" (borders messages {problemsFocused=True}=="╔║╚")
  let sourceTree=sourceFocused {sideTree=Just (emptySidebar "/project" 20 True)}
  check "tree focus leaves selected docked tab with single borders"
    (borders (focusWindow acpId sourceFocused) {sideTree=sideTree sourceTree}=="┌│└")
  putStrLn "Inline render checks passed"

check :: String -> Bool -> IO ()
check name ok=unless ok (error name)

hasColor :: T.Text -> V.Color -> Desktop -> Bool
hasColor needle color d=or
  [needle `T.isInfixOf` TL.toStrict text && V.attrForeColor a==V.SetTo color
  | line<-toList (displayOpsForPic (renderDesktop d) (screenSize d)),
    TextSpan {textSpanAttr=a,textSpanText=text}<-toList line]
