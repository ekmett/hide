{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
-- | Compose the desktop into the common Vty character grid.
--
-- Painting consumes model geometry and prepared document rows. Layers implement
-- windows, menus, dialogs, shadows and privacy masks before grapheme-aware
-- flattening. Native, browser and terminal frontends consume the same result.
--
-- The redraw gate is a separate explicit metadata projection. Immutable payloads
-- are compared by stable identity, including hidden documents needed by native
-- menus; adding a model field must not silently introduce a text/history scan.
module Hide.Render (renderDesktop, renderCellRows, renderCellRowsAndCanvas, renderCursor, snapshot, snapshotHtml, RenderKey, renderKey) where

import Control.Exception (evaluate)
import Hide.ConversationBody (logicalBodyIdentity)
import Data.Unique (Unique)
import Data.IORef
import System.Mem.StableName (StableName, makeStableName, eqStableName)
import Data.List (find, group, sort)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Image.Internal as I
import qualified Hide.TextLayout as TextLayout
import Hide.Unicode (Script(..),scriptTerminalText,CellSpan(..),CellLayer(..),cellRowsAndOwnership,cellDisplayOps)
import Graphics.Vty.Span (SpanOp(..))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Map.Strict as M
import qualified Data.Vector as Vec
import Data.Foldable (toList)
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe)
import System.FilePath (takeFileName, (</>))
import Data.Bits ((.&.), shiftR)
import Data.Time (formatTime, defaultTimeLocale)
import Hide.Hex
import Hide.Buffer
import Hide.BufferView
import Hide.Unicode (clusterWidth, textImage, wideTextImage, displayClusters, terminalProjection)
import Hide.GuestAccess (streamerReadableAt)
import qualified Hide.Plugin.Menu as Plugin
import qualified Hide.Plugin.Editor as Editor
import Hide.Sidebar
import qualified Hide.Plugin.Tree as Tree
import qualified Hide.Plugin.Window as PluginWindow
import Hide.Plugin.Canvas
import Hide.Frontend (modeHeight)
import Hide.Model
import Hide.InlineState
import Hide.InlineTypes (proposalEnd)
import Hide.Syntax
import Hide.Files (FileState(..))
import Hide.Browser (Entry(..))

-- The draw gate compares small UI state and immutable payload identities, never
-- source lines, Undo history, transcript cells or diagnostic bodies. Keeping all
-- documents also invalidates native menus when a hidden buffer changes.
data RenderIdentity = forall a. RenderIdentity (StableName a)
instance Eq RenderIdentity where
  RenderIdentity a == RenderIdentity b = eqStableName a b

-- HARD RULE: never compare whole Desktops, directly or through a sanitized
-- Desktop/Document/Buffer carrier. This explicit metadata projection cannot
-- acquire source contents or Undo history when those model records grow.
data RenderState = RenderState
  { keyScreenSize :: (Int,Int)
  , keyWindows :: [Window]
  , keyRetiredPluginWindows :: [PluginWindow.WindowRef]
  , keyNextId :: Int
  , keyMenu :: Maybe (Int,Int)
  , keyDrag :: Maybe Drag
  , keyWordStar :: Bool
  , keyPrefix :: Maybe Char
  , keyStatus :: Text
  , keyBlockStart :: Maybe (Int,Int)
  , keyLastFind :: Text
  , keyBranchStatus :: Text
  , keyNativeMac :: Bool
  , keyWideSectionTitles :: Bool, keyWindowPresentations :: [(Int,WindowPresentation)]
  , keyMacKeySymbols :: Bool
  , keyVideoMode :: Maybe Int
  , keyHoverTarget :: Maybe (Int,Int,Int)
  , keyTypeHint :: Text
  , keyButtonHover :: Maybe Int
  , keyButtonPressed :: Maybe Int
  , keyContextMenu :: Maybe (Rect,Int)
  , keyContributedMenus :: [Plugin.MenuItem]
  , keyAgentMenuRefs :: [Plugin.MenuRef]
  , keyMenusActive :: Bool
  , keyContextTarget :: Maybe ContextTarget
  , keyDiagnosticsGeneration :: Integer
  , keyProblemsVisible :: Bool
  , keyProblemsSelected :: Int
  , keyProblemsScroll :: Int
  , keyProblemsFocused :: Bool
  , keyDragOriginal :: Maybe [(Int,Rect,Maybe Rect)]
  , keyBranchAdded :: Int
  , keyBranchDeleted :: Int
  , keyBranchRoot :: Maybe FilePath
  , keyMessagesNumber :: Maybe Int
  , keyAgentSteering :: Bool
  , keyAgentReplying :: Bool
  , keyAgentQueued :: Int
  , keyBlinkCursor :: Bool
  , keyCrtFilter :: Bool
  , keyPixelateUnicode :: Bool
  , keyMaterialIcons :: Bool
  , keyDefaultDirectory :: Maybe FilePath
  , keyStatusHover :: Maybe Int
  , keyHeldModifiers :: [V.Modifier]
  , keyProblemsPreferredHeight :: Int
  , keyAgentContextUsage :: Maybe (Integer,Integer)
  , keyAgentSettings :: [AgentSetting]
  , keyBrowserFrontend :: Bool
  , keyAppearance :: Appearance
  , keySystemDark :: Bool
  , keyChildAgentSettings :: [AgentSetting]
  , keyChildAgentSteering :: Bool
  , keyChildAgentContextUsage :: Maybe (Integer,Integer)
  , keyConversationTarget :: Text
  , keyStreamerMode :: Bool
  , keyToolchain :: Maybe Toolchain
  , keyDefaultBufferView :: BufferView
  , keyChatSubmit :: ChatSubmit
  , keyInlineEpoch :: Int
  , keyAutocompleteACPEnabled :: Bool
  , keyAutocompleteSelection :: Selection
  , keyAutocompleteFocused :: Bool
  , keyDockedTerminals :: M.Map Int (Rect,Maybe Rect)
  , keyBottomTerminal :: Maybe Int
  } deriving Eq

data DocumentKey = DocumentKey (Maybe (FilePath,Bool)) (Maybe Text) Int Bool (Maybe FilePath) Bool deriving Eq
data ViewKey = ViewKey (Maybe PluginWindow.WindowRef) !(Maybe Unique) Text Editor.DraftRef (Maybe Editor.EditorMount) (Maybe Int) !BodyAnchor !Int !Int !(Maybe BodySelection) deriving Eq
data DraftKey = DraftKey Selection Bool (Maybe Editor.EditorMount) deriving Eq
data QuestionKey = QuestionKey Int (Maybe Int) Selection Bool deriving Eq
data FieldKey = InputKey Text Int | SelectedInputKey Text Selection | ComboBoxKey Text Int (Maybe Int) | CheckBoxKey Text Bool | RadioKey Text Int
  | ListBoxKey Text Int | FileListKey Int | ReadOnlyKey Text
  | TextAreaKey Text Bool Selection Int Int deriving Eq
data DialogKey = DialogKey Text Int [Text] [FieldKey] deriving Eq
data SidebarKey = SidebarKey FilePath Int Int Int Bool Integer deriving Eq
-- | Comparable UI metadata and immutable payload identities, without a Desktop payload.
data RenderKey = RenderKey RenderState (M.Map Int DocumentKey) (M.Map Text ViewKey) (M.Map Editor.DraftRef DraftKey)
  (Maybe QuestionKey) (Maybe DialogKey) (Maybe SidebarKey) Bool (Int,Bool) Int [RenderIdentity] deriving Eq

-- | Capture a conservative redraw key from explicit metadata and immutable
-- payload identities. No content hashing or structural Buffer equality runs.
renderKey :: Desktop -> IO RenderKey
renderKey original = do
  identities<-newIORef []
  let payload value = do
        evaluated<-evaluate value
        identity<-makeStableName evaluated
        modifyIORef' identities (RenderIdentity identity:)
      present=maybe False (const True)
      file value=do
        mapM_ payload (diskBytes value)
        pure (filePath value,present (diskBytes value))
      document value=do
        payload (documentBuffer value)
        f<-traverse file (documentFile value)
        payload (documentHighlight value)
        mapM_ payload (documentSourceRows value)
        payload (documentLinks value)
        payload (documentShellBlocks value)
        pure (DocumentKey f (documentLabel value) (documentWidth value)
          (documentCursorVisible value) (documentSuggestedName value) (present (documentSourceRows value)))
      view value=do
        case conversationBody value of
          InstalledBody _ receipt->mapM_ (\(BodyControlReceipt prepared _ _ _ _)->payload prepared) receipt
          InertBody prepared->payload prepared
        logicalIdentity<-traverse (evaluate . logicalBodyIdentity) (conversationLogical value)
        pure (ViewKey (conversationBodyRef value) logicalIdentity (conversationName value) (conversationDraftRef value)
          (conversationEditor value) (conversationEditorFrame value) (conversationAnchor value)
          (conversationRowShift value) (conversationScrollColumn value) (conversationReplySelection value))
      draft value=do
        payload (editorDraftBuffer value)
        pure (DraftKey (editorDraftSelection value) (editorDraftFocused value) (editorDraftMount value))
      question value=do
        payload (questionText value)
        payload (questionChoices value)
        payload (questionBuffer value)
        pure (QuestionKey (questionToken value) (questionChoice value) (questionSelection value) (questionFocused value))
      field value=case value of
        Input caption text cursor -> payload text >> pure (InputKey caption cursor)
        SelectedInput caption text sel -> payload text >> pure (SelectedInputKey caption sel)
        ComboBox caption choices choice preview -> payload choices >> pure (ComboBoxKey caption choice preview)
        CheckBox caption checked -> pure (CheckBoxKey caption checked)
        Radio caption choices choice -> payload choices >> pure (RadioKey caption choice)
        ListBox caption choices choice -> payload choices >> pure (ListBoxKey caption choice)
        FileList entries choice -> payload entries >> pure (FileListKey choice)
        ReadOnly caption text -> payload text >> pure (ReadOnlyKey caption)
        TextArea caption editable text selection firstRow column ->
          payload text >> pure (TextAreaKey caption editable selection firstRow column)
      dialogKey value=do
        payload (purpose value)
        fields'<-mapM field (fields value)
        payload (body value)
        pure (DialogKey (dialogTitle value) (focus value) (buttons value) fields')
      sidebar value=do
        payload (treeRows value)
        payload (treeBadges value)
        pure (SidebarKey (treeRoot value) (treeSelected value) (treeScroll value) (treeWidth value) (treeFocused value) (treeRevision value))
  mapM_ payload (pluginWindows original)
  documents<-mapM document (buffers original)
  drafts<-mapM draft (editorDrafts original)
  mapM_ payload (inlinePreview original)
  payload (autocompleteDraft original)
  views<-mapM view (conversationViews original)
  question'<-traverse question (chatQuestion original)
  dialog'<-traverse dialogKey (dialog original)
  tree<-traverse sidebar (sideTree original)
  payload (diagnostics original)
  payload (buildDiagnostics original)
  mapM_ payload (gitReview original)
  payload (clipboard original)
  mapM_ payload (snd (clipboardExport original))
  payload (contextKind original)
  mapM_ payload (keyBindings original)
  payload (guestPrivatePaths original)
  names<-readIORef identities
  let state=RenderState
        { keyScreenSize=screenSize original
        , keyWindows=windows original
        , keyRetiredPluginWindows=toList (retiredPluginWindows original)
        , keyNextId=nextId original
        , keyMenu=menu original
        , keyDrag=drag original
        , keyWordStar=wordStar original
        , keyPrefix=prefix original
        , keyStatus=status original
        , keyBlockStart=blockStart original
        , keyLastFind=lastFind original
        , keyBranchStatus=branchStatus original
        , keyNativeMac=nativeMac original
        , keyWideSectionTitles=wideSectionTitles original
        , keyWindowPresentations=M.toList (windowPresentations original)
        , keyMacKeySymbols=macKeySymbols original
        , keyVideoMode=videoMode original
        , keyHoverTarget=hoverTarget original
        , keyTypeHint=typeHint original
        , keyButtonHover=buttonHover original
        , keyButtonPressed=buttonPressed original
        , keyContextMenu=contextMenu original
        , keyContributedMenus=contributedMenus original
        , keyAgentMenuRefs=agentMenuRefs original
        , keyMenusActive=menusActive original
        , keyContextTarget=contextTarget original
        , keyDiagnosticsGeneration=diagnosticsGeneration original
        , keyProblemsVisible=problemsVisible original
        , keyProblemsSelected=problemsSelected original
        , keyProblemsScroll=problemsScroll original
        , keyProblemsFocused=problemsFocused original
        , keyDragOriginal=dragOriginal original
        , keyBranchAdded=branchAdded original
        , keyBranchDeleted=branchDeleted original
        , keyBranchRoot=branchRoot original
        , keyMessagesNumber=messagesNumber original
        , keyAgentSteering=agentSteering original
        , keyAgentReplying=agentReplying original
        , keyAgentQueued=agentQueued original
        , keyBlinkCursor=blinkCursor original
        , keyCrtFilter=crtFilter original
        , keyPixelateUnicode=pixelateUnicode original
        , keyMaterialIcons=materialIcons original
        , keyDefaultDirectory=defaultDirectory original
        , keyStatusHover=statusHover original
        , keyHeldModifiers=heldModifiers original
        , keyProblemsPreferredHeight=problemsPreferredHeight original
        , keyAgentContextUsage=agentContextUsage original
        , keyAgentSettings=agentSettings original
        , keyBrowserFrontend=browserFrontend original
        , keyAppearance=appearance original
        , keySystemDark=systemDark original
        , keyChildAgentSettings=childAgentSettings original
        , keyChildAgentSteering=childAgentSteering original
        , keyChildAgentContextUsage=childAgentContextUsage original
        , keyConversationTarget=conversationTarget original
        , keyStreamerMode=streamerMode original
        , keyToolchain=toolchain original
        , keyDefaultBufferView=defaultBufferView original
        , keyChatSubmit=chatSubmit original
        , keyInlineEpoch=inlineEpoch original
        , keyAutocompleteACPEnabled=autocompleteACPEnabled original
        , keyAutocompleteSelection=autocompleteSelection original
        , keyAutocompleteFocused=autocompleteFocused original
        , keyDockedTerminals=dockedTerminals original
        , keyBottomTerminal=bottomTerminal original
        }
  pure (RenderKey state documents views drafts question' dialog' tree
    (present (gitReview original)) (fst (clipboardExport original),present (snd (clipboardExport original))) (fst (pendingFileExport original)) names)

blue, gray, black, white, yellow, cyan, green, red :: V.Color
blue=V.RGBColor 0 0 170; gray=V.RGBColor 170 170 170; black=V.RGBColor 0 0 0
white=V.RGBColor 255 255 255; yellow=V.RGBColor 255 255 85; cyan=V.RGBColor 85 255 255
green=V.RGBColor 0 170 0; red=V.RGBColor 170 0 0
scrollCyan :: V.Color
scrollCyan=V.RGBColor 0 170 170
attr :: V.Color -> V.Color -> V.Attr
attr fg bg = V.defAttr `V.withForeColor` fg `V.withBackColor` bg
paper, edit, selected, shadow :: V.Attr
paper=attr black gray; edit=attr yellow blue; selected=attr black green; shadow=attr gray black

label :: V.Attr -> Text -> V.Image
label a = textImage a . T.map (\c -> if c<' ' || c=='\DEL' then '·' else c)
row :: V.Attr -> Int -> Text -> V.Image
row a w t = V.cropRight (max 0 w) (label a t V.<|> V.charFill a ' ' (max 0 w) 1)
place :: Int -> Int -> V.Image -> V.Image
place x y = V.translate (max 0 x) (max 0 y)
buttonShadow :: V.Color -> Rect -> [V.Image]
buttonShadow bg (Rect x y w _) =
  [place (x+w) y (V.char (attr black bg) '▄'),
   place (x+1) (y+1) (V.charFill (attr black bg) '▀' w 1)]

box :: V.Attr -> Bool -> Int -> Int -> V.Image
box a double w h
  | w<2 || h<2 = V.charFill a ' ' (max 0 w) (max 0 h)
  | otherwise = V.vertCat [line tl hz tr, V.vertCat (replicate (h-2) (V.char a vt V.<|> V.charFill a ' ' (w-2) 1 V.<|> V.char a vt)),line bl hz br]
  where (tl,tr,bl,br,hz,vt)=if double then ('╔','╗','╚','╝','═','║') else ('┌','┐','└','┘','─','│')
        line l m r=V.char a l V.<|> V.charFill a m (w-2) 1 V.<|> V.char a r

-- | Compatibility picture for text-mode Vty callers. Display frontends and
-- captures consume renderCellRows directly, avoiding this image projection.
renderDesktop :: Desktop -> V.Picture
renderDesktop d = (V.picForImage (V.vertCat (map (V.horizCat . map image . Vec.toList) (Vec.toList (renderCellRows d))))) {V.picCursor=cursor}
  where
    (_,cursor)=renderScene d
    image (CellText a text)=textImage a text
    image (CellScript a text natural _)=textImage a (scriptTerminalText natural text)
    image (CellGlyph a text full start shown)
      | start/=0 || shown/=full=V.charFill a ' ' shown 1
      | otherwise=(if full/=clusterWidth text then wideTextImage else textImage) a text

-- | The one composed visible grid consumed by GPU and frame transports. Shadows
-- update tile colors in place; no text/image reconstruction or payload equality.
renderCellRows :: Desktop -> Vec.Vector (Vec.Vector CellSpan)
renderCellRows=fst . renderCellRowsAndCanvas

-- | Cells and image ownership share one composition. Only audience-permitted,
-- live window resources enter the returned scene; occluded windows keep their
-- resource identity while owning no cells. No pixel payload is traversed here.
renderCellRowsAndCanvas :: Desktop -> (Vec.Vector (Vec.Vector CellSpan),CanvasScene)
renderCellRowsAndCanvas d=
  let surfaces=desktopCanvases d
      (cells,mask)=cellRowsAndOwnership (fst (renderSceneWith surfaces d)) (screenSize d)
  in (cells,CanvasScene surfaces mask)

-- Bounded by admission (64 image windows). The clipped viewport is also the
-- semantic rectangle; the target retains the original uncut host transform.
desktopCanvases :: Desktop -> [CanvasSurface]
desktopCanvases d=zipWith surface [1..64] eligible
  where
    (columns,rows)=screenSize d
    eligible=[(w,image,prepared,viewport,clipped) | w<-windows d,windowVisible d w,
      Just image<-[windowImage d w],Just prepared<-[windowPluginText d w],
      not (streamerMode d) || not (privatePreparedWindow d prepared),
      let Rect x y width height=pluginTextRect d w; viewport=(x,y,width,height)
          left=max 0 x; top=max 0 y; right=min columns (x+width); bottom=min rows (y+height)
          clipped=(left,top,right-left,bottom-top),right>left,bottom>top]
    surface slot (w,image,prepared,viewport,clipped)=CanvasSurface (windowId w) slot image clipped
      (canvasImageTarget (modeHeight (fromMaybe 3 (videoMode d))) viewport (imageViewport w) image)
      (T.take 256 (PluginWindow.preparedWindowTitle prepared))
      ("PNG "<>T.pack (show (imageWidth image))<>" × "<>T.pack (show (imageHeight image))<>
       ". F: fit; 1: actual size; plus/minus or wheel: zoom; arrows or drag: pan.")

-- | Cursor from the same scene, without projecting its cell grid into an image.
renderCursor :: Desktop -> V.Cursor
renderCursor=snd . renderScene

renderScene :: Desktop -> ([CellLayer],V.Cursor)
renderScene=renderSceneWith []

renderSceneWith :: [CanvasSurface] -> Desktop -> ([CellLayer],V.Cursor)
renderSceneWith canvases d=(privacyLayers++layers,visibleCursor)
  where
    (sw,sh)=screenSize d
    privacyLayers
      | not (streamerMode d) = []
      | otherwise = [CellMask (attr gray blue) [(x,y,n)
          | y<-[0..sh-1],(x,n)<-hiddenRuns 0 [streamerReadableAt d x y | x<-[0..sw-1]]]]
    hiddenRuns _ []=[]
    hiddenRuns x cells=
      let (shown,rest)=span id cells
          (hidden,tailCells)=span not rest
          start=x+length shown
      in [(start,length hidden) | not (null hidden)]++hiddenRuns (start+length hidden) tailCells
    visibleCursor | dialog d==Nothing,maybe False (\w->maybe False (const True) (windowImage d w) && windowFocused d w) (activeWindow d)=V.NoCursor
                  | otherwise=case cursor of
                    V.Cursor x y | streamerMode d && not (streamerReadableAt d x y) -> V.NoCursor
                    _ -> cursor
    layers = case dialog d of
      Nothing -> withMenu
      Just dg -> images (dialogLayers d dg) ++ halo (dialogRect d dg) ++ withMenu
    withMenu = case contextMenu d of
      Just popup@(r,_) -> images (contextLayers d popup) ++ (if dialog d==Nothing then halo r else []) ++ base
      Nothing -> withMainMenu
    withMainMenu = case menu d of
      Nothing -> base
      Just m@(i,_) -> images (menuLayers d m) ++ (if dialog d==Nothing then halo (menuRect d i) else []) ++ base
    images=map CellImage
    halo (Rect x y w h)=[CellHalo shadow [(x+w,y+1,2,max 0 (h-1)),(x+2,y+h,w,1)]]
    base = images [place 0 0 menuBar, place 0 (sh-1) statusBar]
      ++ bottomLayers canvases d
      ++ images (maybe [] (treeLayers d) (sideTree d))
      ++ foldr stackWindow (images [V.charFill (attr blue gray) '░' sw sh]) (floatingWindows d)
    stackWindow w below = windowLayers canvases d (windowFocused d w) w ++
      (if fmap windowId (activeWindow d)==Just (windowId w) && dialog d==Nothing && contextMenu d==Nothing && menu d==Nothing then halo (bounds w) else []) ++ below
    menuBar = V.cropRight sw (V.char paper ' ' V.<|> V.horizCat
      [V.char normal ' ' V.<|> label (attr red bg) (T.take 1 title) V.<|> label normal (T.drop 1 title<>" ")
       | (i,(title,_,_))<-zip [0..] menus,let bg=if fmap fst (menu d)==Just i then green else gray,let normal=attr black bg]
      V.<|> V.charFill paper ' ' sw 1)
    statusBar = V.cropRight (left (toolchainBadgeRect d)) (V.horizCat
      [keyLegendOn (if statusHover d==Just i && action/=Nothing then green else gray) text
      | (i,(text,action))<-zip [0..] (statusHints d)] V.<|> V.charFill paper ' ' sw 1) V.<|> toolchainImage V.<|> badgeImage
    toolchainImage = label (attr black (if statusHover d==Just (length (statusHints d)) then green else gray)) (toolchainBadgeText d)
    badge = if activeConversation d then "" else gitBadgeText d
    badgeImage = if T.null badge then V.emptyImage else label paper (" │ "<>gitBranchText d<>" ")
      V.<|> label (attr (V.RGBColor 0 85 0) gray) ("+"<>gitCountText (branchAdded d)) V.<|> label paper " "
      V.<|> label (attr red gray) ("-"<>gitCountText (branchDeleted d)) V.<|> label paper " "
    cursor = case dialog d of
      Just dg -> case drop (focus dg) (zip (fieldRects d dg) (fields dg)) of
        (Rect x y w _,Input _ value p):_ -> let offset=max 0 (displayColumn value p-w+1)
                                         in V.Cursor (x+displayColumn value p-offset) (y+1)
        (Rect x y w _,SelectedInput _ value sel):_ -> let p=caret sel; offset=max 0 (displayColumn value p-w+1)
                                                   in V.Cursor (x+displayColumn value p-offset) (y+1)
        (rect,f@(TextArea _ True b sel sr sc)):_ ->
          let area=textAreaRect rect f; (line,col)=bufferLineColumn b (caret sel)
              cx=left area+displayColumn (bufferLineAt b line) col-sc; cy=top area+line-sr
          in if inside area cx cy && cy<top (dialogRect d dg)+height (dialogRect d dg)-3 then V.Cursor cx cy else V.NoCursor
        _ -> V.NoCursor
      Nothing | menu d/=Nothing || contextMenu d/=Nothing || problemsFocused d || maybe False treeFocused (sideTree d) -> V.NoCursor
      Nothing | activeMarkdown d,Just w<-activeWindow d,Nothing<-windowMarkdown d w -> V.NoCursor
      Nothing -> case (activeWindow d,activeDocument d) of
        (Just w,_) | questionActive d,Just (rect,q,projected)<-questionInputGeometry d w,questionChoice q==Nothing -> let
          text=contents (questionBuffer q)
          delta=displayColumn text (caret (questionSelection q))-displayColumn text (questionInputStart (projectedQuestionWidth projected) q)
          cx=left rect+delta
          cy=top rect
          body=pluginTextRect d w
          in if inside body cx cy then V.Cursor cx cy else V.NoCursor
        (Just w,_) | activeAutocomplete d && autocompleteFocused d -> let
          b=autocompleteDraft d; (r,c)=bufferLineColumn b (caret (autocompleteSelection d)); (sr,sc)=autocompleteComposerScroll d w
          rect=autocompleteComposerRect d w
          in if height rect>0 && width rect>0 then V.Cursor (left rect+displayColumn (bufferLineAt b r) c-sc) (top rect+r-sr) else V.NoCursor
        (Just w,_) | composerActive d,Just draft<-windowEditorDraft d w -> let
          b=editorDraftBuffer draft; (r,c)=bufferLineColumn b (caret (editorDraftSelection draft)); (sr,sc)=composerScroll d w
          rect=composerRect d w; (marker,line)=if windowEditorCode w then composerLine b r else (0,bufferLineAt b r)
          in if height rect>0 && width rect>0 then V.Cursor (left rect+displayColumn line (max 0 (c-marker))-sc) (top rect+r-sr) else V.NoCursor
        (_,Just doc) | not (documentCursorVisible doc) -> V.NoCursor
        (Just original,Just doc) -> let { w=displayWindow original; b=documentBuffer doc; (r,c)=windowCaretCell d doc w; x=left (bounds w)+1+c-scrollColumn w; y=top (bounds w)+1+r-scrollRow w;
                                                  liveRow=not (windowChangeView b w) || viewRightRow (viewRowAt (bufferView w) (bufferViewProjection b) r)/=Nothing }
                            in if liveRow && inside (Rect (left (bounds w)+1) (top (bounds w)+1) (width (bounds w)-2) (windowContentRows d doc w)) x y then V.Cursor x y else V.NoCursor
        _ -> V.NoCursor

-- The static body owns all question offsets; bounded live rows use exactly
-- that body's current layout and clip, without rebuilding transcript Markdown.
questionLayers :: Desktop -> Window -> [V.Image]
questionLayers d w=case windowPluginText d w of
  Nothing->[]
  Just prepared->
    [place (left (bounds w)+1) (top (bounds w)+1+row-scrollRow w)
      (V.cropRight columns (V.translateX (column-scrollColumn w)
        (styledImage (darkAppearance d) (const False) Nothing False (Selection 0 0) offset chars)))
    | (offset,chars)<-questionOverlayRows d w
    , let (row,column)=windowTextPosition d w (PluginWindow.preparedWindowText prepared) offset
    , row>=scrollRow w,row<scrollRow w+pluginBodyRows d w]
    where columns=max 0 (width (bounds w)-2)

-- The same host buttons and border geometry are shared by source and plugin text.
hostWindowFrame :: Desktop -> Bool -> Window -> V.Attr -> [V.Image]
hostWindowFrame d active w frame=
  (if active then [place (x+2) y (label frame "[" V.<|> label (attr (V.RGBColor 85 255 85) blue) (if videoMode d==Nothing then "x" else "■") V.<|> label frame "]"),
   place (x+ww-6) y (label frame "[" V.<|> label (attr cyan blue) "↑" V.<|> label frame "]")] else [])
  ++[place (x+ww-7-T.length number) y (label frame number),place x y (box frame (active && not moving) ww hh)]
  where
    Rect x y ww hh=bounds w
    number=T.pack (show (windowNumber w))
    moving=case drag d of Just (Moving wid _ _)->wid==windowId w; Just (Resizing wid _ _)->wid==windowId w; _->False

-- The host paints one input owner identically over source and prepared bodies.
composerLayers :: Desktop -> Bool -> Window -> [V.Image]
composerLayers d active w
  | not (windowHasEditor d w) && not hintComposer = []
  | otherwise = [place (left rect) (top rect) inputImage] ++ thoughtEdges
  where
    attached=windowEditorDraft d w
    hintComposer=autocompletePane d w
    rect=if hintComposer then autocompleteComposerRect d w else composerRect d w
    draft=if hintComposer then autocompleteDraft d else maybe emptyEditorBuffer editorDraftBuffer attached
    draftSelection=if hintComposer then autocompleteSelection d else maybe (Selection 0 0) editorDraftSelection attached
    draftFocused=if hintComposer then autocompleteFocused d else maybe False editorDraftFocused attached
    (sr,sc)=if hintComposer then autocompleteComposerScroll d w else composerScroll d w
    draftLine n=if not hintComposer && windowEditorCode w then composerLine draft n else (0,bufferLineAt draft n)
    thoughtEdges
      | width rect<=0 || height rect<=0 = []
      | otherwise = [place (left rect-1) (top rect) (edgeImage True),
                     place (left rect+width rect) (top rect) (edgeImage False),
                     place (left rect+width rect+1) (top rect) (label (attr scrollCyan blue) "•.")]
    edgeImage leftSide=V.vertCat
      [V.char (if corner then attr scrollCyan blue else attr black scrollCyan)
        (if corner then bubbleTile (videoMode d/=Nothing) shape else ' ')
      | n<-[0..height rect-1], let corner=n==0 || n==height rect-1,
        let shape=if height rect==1 then if leftSide then 4 else 5
                  else (if n==0 then 0 else 2)+(if leftSide then 0 else 1)]
    inputImage=V.vertCat [V.cropRight (width rect)
      (styledSourceImage (darkAppearance d) (Just fill) (active && draftFocused) draftSelection (bufferLineOffset draft n+marker) sc (width rect) (plainSourceRow line) V.<|> V.charFill fill ' ' (width rect) 1)
      | n<-[sr..sr+height rect-1],let (marker,line)=draftLine n,
        let fill=if marker>0 then attr yellow (if darkAppearance d then black else blue) else attr black scrollCyan]

pluginWindowLayers :: Maybe CanvasSurface -> Desktop -> Bool -> Window -> PluginWindow.PreparedWindow -> [CellLayer]
pluginWindowLayers canvas d active w prepared=
  map CellImage (questionLayers d w++composerLayers d active w)++imageBody++map CellImage ((if active then positionBadge++[windowScrollbarImage d True w,windowScrollbarImage d False w] else [])
    ++(place (x+column) y (label frame title):hostWindowFrame d active w frame)
    ++[place (x+1) (y+1) (V.charFill edit ' ' (max 0 (ww-2)) (max 0 (hh-2)))])
  where
    imageBody=case canvas of
      Nothing->bodyLayers
      Just surface->let Rect cx cy cw ch=pluginTextRect d w
        in [CellCanvas (canvasSlot surface) (bodyLayers++[CellImage (place cx cy (V.charFill edit ' ' cw ch))])]
    Rect x y ww hh=bounds w
    moving=case drag d of Just (Moving wid _ _) -> wid==windowId w; Just (Resizing wid _ _) -> wid==windowId w; _ -> False
    frame=attr (if moving then cyan else if active then white else gray) blue
    text=PluginWindow.preparedWindowText prepared
    rows=PluginWindow.preparedWindowRows prepared
    paintSelection=conversationPaintSelection d w
    title=" "<>T.take (columnOffset (windowTitle d w) (max 0 (ww-17))) (windowTitle d w)<>" "
    column=max 6 ((ww-keyLabelWidth title) `div` 2)
    positionBadge=[place (x+2) (y+hh-1) (label frame (T.take (max 0 (ww-4)) (conversationPositionText d)))
      | conversationTargetFor d w/=Nothing]
    selectable style=case fmap PluginWindow.textCopy (PluginWindow.preparedWindowSemantics prepared) of
      Just PluginWindow.CopyMessages{}->case style of BubbleText{}->True; _->False
      _->True
    bodyLayers | PluginWindow.RowsDetails listed index _<-rows =
      let (listRect,detailRect)=rowsWindowRects d w
          chosen=case rowsInteraction w of Just (RowsInteraction ident _)->fromMaybe 0 (M.lookup ident index); _->0
          offset=rowsListOffset d w chosen
          detailsFocused=case rowsInteraction w of Just (RowsInteraction _ focused)->focused; _->False
          detail=fromMaybe prepared (windowPluginText d w)
          detailText=PluginWindow.preparedWindowText detail
          listImages=[place (left listRect) (top listRect+n-offset)
            (V.cropRight (width listRect) (label (if active && n==chosen then attr blue white else edit) caption V.<|>V.charFill edit ' ' (width listRect) 1))
            | n<-[offset..offset+height listRect-1],Just (PluginWindow.WindowRow _ caption _)<-[listed Vec.!? n]]
          detailLayers=case PluginWindow.preparedWindowRows detail of
            PluginWindow.PlainRows plain->plainLayers detailRect detailText plain (active && detailsFocused)
            _->[]
      in detailLayers++map CellImage (listImages++[place (x+1) (top detailRect-1) (label frame " Details " V.<|>V.charFill frame '─' (max 0 (ww-11)) 1)])
      | Just layout<-windowPresentation d w =
      [styledLayoutRow (darkAppearance d) selectable active (selection w) paintSelection
        (Rect (x+1) (y+1+n-scrollRow w) (max 0 (ww-2)) 1) (scrollColumn w) row
      | n<-[scrollRow w..scrollRow w+pluginBodyRows d w-1],Just row<-[TextLayout.layoutRows layout Vec.!? n]]
      | PluginWindow.PlainRows plain<-rows =plainLayers (pluginTextRect d w) text plain active
      | otherwise=[CellImage (place (x+1) (y+1) body)]
    plainLayers rect content plain focused=
      [sourceCellRow (darkAppearance d) Nothing focused (selection w) (contentLineOffset content n)
        (Rect (left rect) (top rect+n-scrollRow w) (width rect) 1) (scrollColumn w) row
      | n<-[scrollRow w..scrollRow w+height rect-1],Just row<-[plain Vec.!? n]]
    body=V.vertCat [line n | n<-[scrollRow w..scrollRow w+pluginBodyRows d w-1]]
    line n=V.cropRight (max 0 (ww-2)) (lineImage V.<|> V.charFill edit ' ' (max 0 (ww-2)) 1)
      where
        start=contentLineOffset text n
        lineImage=case rows of
          PluginWindow.PlainRows _->V.emptyImage
          PluginWindow.RowsDetails{}->V.emptyImage
          PluginWindow.StyledRows styled->V.translateX (negate (scrollColumn w))
            (styledSigilsImage (darkAppearance d) selectable Nothing active (selection w) paintSelection start (case styled Vec.!? n of Just (StyledRow sigils _ _)->sigils; _->Nil))

-- Shared frame chrome uses the same semantic geometry as pointer dispatch.
windowScrollbarImage :: Desktop -> Bool -> Window -> V.Image
windowScrollbarImage d vertical w=case windowScrollbar d vertical w of
  Nothing->V.emptyImage
  Just (Rect sx sy bw bh,limit)->
    let len=if vertical then bh else bw
        thumb=scrollbarThumb len limit (windowScrollPosition d vertical w)
        cell n=V.char (if n==0 || n==len-1 then attr blue scrollCyan else attr scrollCyan blue)
          (if n==0 then if vertical then '▲' else '◄' else if n==len-1 then if vertical then '▼' else '►' else if n==thumb then '█' else '░')
    in place sx sy ((if vertical then V.vertCat else V.horizCat) [cell n | n<-[0..len-1]])

windowLayers :: [CanvasSurface] -> Desktop -> Bool -> Window -> [CellLayer]
windowLayers canvases d active w | PluginContent reference<-windowContent w = case M.lookup reference (pluginWindows d) of
  Nothing->[]
  Just prepared->pluginWindowLayers (find ((==windowId w).canvasId) canvases) d active w prepared
windowLayers _ d active original =
  map CellImage ([place x (y+1+issueRow issue-scrollRow w) (label (attr (if diagnosticSeverity issue==1 then V.RGBColor 255 85 85 else yellow) blue) "▶")
    | bufferView w/=MarkdownView,not (byteMode (documentBuffer doc)), issue<-diagnostics d, Just (diagnosticPath issue)==fmap filePath (documentFile doc), issueRow issue>=scrollRow w, issueRow issue<scrollRow w+hh-2]
  ++ (if active then [place (x+windowPositionColumn doc) (y+hh-1) (label frame (T.take (max 0 (ww-windowPositionColumn doc-2)) (windowPositionText d doc w))),windowScrollbarImage d True w,windowScrollbarImage d False w] else [])
  ++ [place (x+6) y (label frame "[" V.<|> label (attr cyan blue) " " V.<|> label frame "]") | active,terminalWindow d w,not (windowPinned d w)]
  ++ composerLayers d active w
  ++ hexDividerLayers
  ++ reviewDividerLayers
  ++ [place (x+titleColumn) y titleImage])
  ++ documentLayers
  ++ map CellImage (hostWindowFrame d active w frame)
  where
    w=displayWindow original
    Rect x y ww hh=bounds w
    doc=fromMaybe (newDocument (newBuffer "") Nothing) (windowDocument (buffers d) w)
    b=documentBuffer doc
    issueRow issue
      | windowChangeView b w = viewRowForChange (bufferView w) (bufferViewProjection b) CurrentSide
          (fst (changeLineColumn b (liveToChangeOffset b (bufferLineOffset b (diagnosticRow issue)))))
      | otherwise = sourceDisplayRow (diagnosticRow issue)
    file=maybe (maybe ("NONAME"<>maybe "" (T.pack . show) (bufferId w)<>".HS") T.pack (documentSuggestedName doc)) (T.pack . takeFileName . filePath) (documentFile doc)
    -- Measured line changes are shared by all views; never diff text while drawing.
    -- Docs: docs/editing.md (unsaved change counts in each buffer title).
    name=fromMaybe file (documentLabel doc)<>(if dirty b then " *" else "")
    (added,deleted)=bufferLineChanges b
    badge=if documentLabel doc==Nothing && (added/=0 || deleted/=0)
      then [("+"<>T.pack (show added),attr (V.RGBColor 85 255 85) background),
            (" ",frame),("-"<>T.pack (show deleted),attr (V.RGBColor 255 85 85) background)] else []
    (titleStart,titleEnd)
      | byteMode b = (max 6 (1+hexColumn 0-scrollColumn w),min (ww-8-T.length number) (hexAsciiColumn (windowHexBytes w)-scrollColumn w))
      | otherwise = (if terminalWindow d w then 10 else 6,ww-8-T.length number)
    titleWidth=max 0 (titleEnd-titleStart)
    badgeWidth=sum [T.length text | (text,_)<-badge]
    shownBadge=if badgeWidth+3<=titleWidth then badge else []
    reserved=if null shownBadge then 0 else badgeWidth+1
    clippedName=T.take (columnOffset name (max 0 (titleWidth-reserved-2))) name
    titleImage=label frame (" "<>clippedName<>(if null shownBadge then "" else " "))
      V.<|> V.horizCat [label color text | (text,color)<-shownBadge] V.<|> label frame " "
    titleColumn=titleStart+max 0 ((titleWidth-V.imageWidth titleImage) `div` 2)
    number=T.pack (show (windowNumber w))
    moving=case drag d of Just (Moving wid _ _) -> wid==windowId w; Just (Resizing wid _ _) -> wid==windowId w; _ -> False
    helpWindow=documentLabel doc==Just "Haskell Help"
    background=if helpWindow && not (darkAppearance d) then scrollCyan else blue
    base=if helpWindow then attr (if darkAppearance d then white else black) background else edit
    frame=attr (if moving then cyan else if active then white else gray) background
    -- Embedded DAP sources retain language tokens despite their read-only label.
    -- Docs: docs/site/screenshots/debug-step.png (docs/running.md).
    useStyles=case documentLabel doc of
      Nothing -> True
      Just title -> title=="Haskell Help" || any (`T.isPrefixOf` title) ["Terminal ","Source "]
    styledLines=splitStyled (documentHighlight doc)
    hexDividerLayers =
      [place (x+1+column) y (V.vertCat [V.char frame (if active && not moving then '╤' else '┬'),
        V.charFill frame '│' 1 contentHeight,V.char frame (if active && not moving then '╧' else '┴')])
      | byteMode b, divider<-hexDividers (windowHexBytes w), let column=divider-scrollColumn w, column>=0, column<contentWidth]
    contentWidth=max 0 (ww-2); contentHeight=windowContentRows d doc w
    -- Docs: tools/docs-screenshots.hs markdown-view -> docs/site/screenshots/markdown-view.png (docs/editing.md).
    documentLayers | bufferView w==MarkdownView,Nothing<-windowMarkdown d w =
      [CellImage (place (x+1) (y+1) (label base (case M.lookup (windowId w) (windowPresentations d) of
          Just failed@MarkdownWindowFailure{} | let (target,columns,wide)=presentationMetadata failed,Just target==windowPresentationTarget d w,columns==max 1 (ww-2),wide==wideSectionTitles d -> "Markdown view preparation failed."
          _->"Preparing Markdown view..."))),CellImage (place (x+1) (y+1) (V.charFill base ' ' contentWidth contentHeight))]
      | Just layout<-windowPresentation d w =
      [styledLayoutRow (darkAppearance d) selectable active (selection w) Nothing
        (Rect (x+1) (y+1+n-scrollRow w) contentWidth 1) (scrollColumn w) row
      | n<-[scrollRow w..scrollRow w+contentHeight-1],Just row<-[TextLayout.layoutRows layout Vec.!? n]]
      ++[CellImage (place (x+1) (y+1) (V.charFill base ' ' contentWidth contentHeight))]
      | bufferView w==CurrentView,syntaxDocument doc,Nothing<-inlineOption =
      [sourceCellRow (darkAppearance d) (lineColor n) active (selection w) (bufferLineOffset b n)
        (Rect (x+1) (y+1+n-scrollRow w) contentWidth 1) (scrollColumn w) (sourceRow n)
      | n<-[scrollRow w..scrollRow w+contentHeight-1]]
      ++[CellImage (place (x+1) (y+1) (V.charFill base ' ' contentWidth contentHeight))]
      | otherwise=[CellImage (place (x+1) (y+1) documentImage)]
    documentImage=V.vertCat [(if windowChangeView b w then renderReview else renderPreview) n
      | n<-[scrollRow w..scrollRow w+contentHeight-1]]
    -- Prepared rows are a bounded overlay, not a replacement Buffer. Preserve
    -- source colors on the original chunks and map the remaining viewport back
    -- to source rows; neither source contents nor Undo history is traversed.
    inlineOption=case inlinePreview d of
      Just view | active && inlineMatches d view -> selectedOption view
      _ -> Nothing
    sourceDisplayRow n=case inlineOption of
      Just option | n>optionLastRow option -> n+inlineRowDelta option
                  | n>=optionFirstRow option -> optionFirstRow option
      _ -> n
    inlineRowDelta option=Vec.length (optionRows option)-(optionLastRow option-optionFirstRow option+1)
    renderPreview n=case inlineOption of
      Just option | n>=optionFirstRow option ->
        case optionRows option Vec.!? (n-optionFirstRow option) of
          Just chunks -> V.cropRight contentWidth (V.translateX (negate (scrollColumn w))
            (styledImage (darkAppearance d) (const True) Nothing False (Selection 0 0) 0
              (concat [inlineChunk option (n-optionFirstRow option) i text changed
                | (i,(text,changed))<-zip [0::Int ..] chunks])) V.<|> V.charFill base ' ' contentWidth 1)
          Nothing -> renderLine (n-inlineRowDelta option)
      _ -> renderLine n
    inlineChunk _ _ _ text True=styledText (TerminalStyle 0xaaaaaa 0x0000aa 0) text
    inlineChunk option rowIndex chunkIndex text False=highlight offset (runs text ranges)
      where
        offset | rowIndex==0 && chunkIndex==0 = bufferLineOffset b (optionFirstRow option)
               | otherwise = proposalEnd (optionProposal option)
        (sourceRow,sourceColumn)=bufferLineColumn b offset
        ranges=case documentSourceRows doc >>= (Vec.!? sourceRow) of
          Just row->[(sourceRangeCharEnd range-max sourceColumn (sourceRangeCharStart range),sourceRangeStyle range)
            | range<-Vec.toList (sourceRowRanges row),sourceRangeCharEnd range>sourceColumn]
          Nothing->[]
        runs remaining _ | T.null remaining=[]
        runs remaining []=styledText Plain remaining
        runs remaining ((count,style):rest)=let (part,after)=T.splitAt count remaining
                                           in styledText style part++runs after rest
        highlight _ []=[]
        highlight position ((part,style):rest)=
          let n=T.length part
              a=max 0 (min n (lo-position)); z=max a (min n (hi-position))
              (before,tailText)=T.splitAt a part; (chosen,after)=T.splitAt (z-a) tailText
          in (if active then styledText style before++styledText (TerminalStyle 0x0000aa 0xaaaaaa 0) chosen++styledText style after
              else styledText style part)++highlight (position+n) rest
        (lo,hi)=ordered (selection w)
    -- Docs: docs/editing.md (buffer views). Shared compact projections skip
    -- unchanged subtrees; only these visible rows request source text.
    projection=bufferViewProjection b
    reviewDividerLayers
      | not (windowChangeView b w) || bufferView w/=SideBySideView = []
      | otherwise = [place (x+column) (y+1) (V.vertCat
          [V.charFill frame '│' 1 contentHeight,V.char frame (if active && not moving then '╧' else '┴')])]
          ++ [place (x+column) y (V.char frame (if active && not moving then '╤' else '┬'))
             | column<titleColumn || column>=titleColumn+V.imageWidth titleImage]
      where column=1+fst (reviewPaneWidths w)
    renderReview n
      | viewOmittedRows entry>0 = row (attr gray blue) contentWidth
          ("  ⋯ "<>T.pack (show (viewOmittedRows entry))<>" unchanged lines ⋯")
      | bufferView w==OnlyChangesView,viewRowCount OnlyChangesView projection==0,n==0 =
          row (attr gray blue) contentWidth "  No unsaved changes."
      -- Docs: docs/site/screenshots/side-by-side.png (docs/editing.md);
      -- regenerate with tools/docs-screenshots.hs after changing this layout.
      | bufferView w==SideBySideView =
          reviewLine OriginalSide leftWidth (viewLeftRow entry) (viewRightRow entry/=Nothing)
          V.<|> V.char frame '│'
          V.<|> reviewLine CurrentSide rightWidth (viewRightRow entry) (viewLeftRow entry/=Nothing)
      | otherwise = reviewLine UnifiedSide contentWidth (viewRightRow entry) False
      where
        entry=viewRowAt (bufferView w) projection n
        (leftWidth,rightWidth)=reviewPaneWidths w
    reviewLine side columns Nothing opposite=V.charFill
      (if opposite then attr gray (if side==OriginalSide then V.RGBColor 0 85 0 else V.RGBColor 85 0 0) else base) ' ' columns 1
    reviewLine side columns (Just fullRow) _=case bufferChangeRows b fullRow 1 of
      (kind,liveRow,_):_ ->
        let plain=plainSourceRow (changeLineAt b fullRow)
            tokens=case liveRow of
              Just n | syntaxDocument doc -> maybe plain (\rows->fromMaybe plain (rows Vec.!? n)) (documentSourceRows doc)
              _ -> plain
            color=case kind of
              OriginalLine -> Nothing
              AddedLine -> Just (attr (V.RGBColor 85 255 85) blue)
              DeletedLine -> Just (attr (V.RGBColor 255 85 85) blue)
            selectedReview=windowReviewSelection b w >>= \chosen ->
              if reviewSide chosen==side then Just (reviewRange chosen) else Nothing
            (sel,start,canSelect)=case selectedReview of
              Just chosen -> (chosen,changeLineOffset b fullRow,active)
              Nothing -> case liveRow of
                Just n | side/=OriginalSide -> (selection w,bufferLineOffset b n,active && windowReviewSelection b w==Nothing)
                _ -> (Selection 0 0,0,False)
        in V.cropRight columns (styledSourceImage (darkAppearance d) color canSelect sel start (scrollColumn w) columns tokens
          V.<|> V.charFill base ' ' columns 1)
      _ -> V.charFill base ' ' columns 1
    selectable _ = True
    renderLine n | byteMode b && n>=documentRows doc w = V.charFill base ' ' contentWidth 1
    renderLine n | byteMode b = V.cropRight contentWidth (V.translateX (negate (scrollColumn w)) (V.horizCat
      [V.char (if active && maybe False highlighted offset then selected else if maybe False (\i -> let byte=T.index bytes (i-n*count) in byte<' ' || byte>'~') offset then attr gray blue else edit) ch | (ch,offset)<-hexRowChunk count (n*count) bytes]) V.<|> V.charFill base ' ' contentWidth 1)
      where
        count=windowHexBytes w
        bytes=bufferSlice b (n*count) count
        highlighted offset = offset==caret (selection w) || let (a,z)=ordered (selection w) in offset>=a && offset<z
    renderLine n=V.cropRight contentWidth (lineImage V.<|> V.charFill base ' ' contentWidth 1)
      where
        plain=bufferLineAt b n
        lineImage
          | syntaxDocument doc = styledSourceImage (darkAppearance d) (lineColor n) active (selection w) (bufferLineOffset b n) (scrollColumn w) contentWidth
              (sourceRow n)
          | otherwise = V.translateX (negate (scrollColumn w)) (styledImage (darkAppearance d) selectable (lineColor n) active (selection w) (bufferLineOffset b n)
              (if useStyles && not (null (documentHighlight doc)) then fromMaybe [] (atMay styledLines n) else styledText Plain plain))

    sourceLines=contentSourceLinesFrom (bufferContent b) (max 0 (scrollRow w))
    sourceRow n=let line=if n>=max 0 (scrollRow w)
                          then fromMaybe (contentSourceLineAt (bufferContent b) n) (atMay sourceLines (n-max 0 (scrollRow w)))
                          else contentSourceLineAt (bufferContent b) n
                in maybe (plainSourceLine line) (attachSourceLine line) (documentSourceRows doc >>= (Vec.!? n))

    lineColor n = case documentLabel doc of
      Just "Git diff" -> Just (diffLineAttr (bufferLineAt b n))
      Just _ | useStyles -> Nothing
      Just _ -> Just (attr yellow blue)
      Nothing -> Nothing

diffLineAttr :: Text -> V.Attr
diffLineAttr line=attr (if "+" `T.isPrefixOf` line then V.RGBColor 85 255 85 else if "-" `T.isPrefixOf` line then V.RGBColor 255 85 85 else if "@@" `T.isPrefixOf` line then cyan else yellow) blue

atMay :: [a] -> Int -> Maybe a
atMay xs n = case drop n xs of a:_ -> Just a; [] -> Nothing

splitStyled :: StyledText -> [StyledText]
splitStyled=map fst . splitStyledText

-- Source runs retain original UTF8 slices. Selection cuts ordinary runs at
-- character boundaries; exceptional graphemes keep one complete source target
-- and the advance already resolved by sourceSigils, including tab stops.
styledSourceImage :: Bool -> Maybe V.Attr -> Bool -> Selection -> Int -> Int -> Int -> SourceRow -> V.Image
styledSourceImage dark override active sel start left columns row=
  V.translateX (column-left) (V.horizCat spans)
  where
    (column,spans)=sourceCellSpans image dark override active sel start left columns row
    image (CellText paint text)=I.HorizText paint (TL.fromStrict text) (T.length text) (T.length text)
    image (CellGlyph paint text advance _ _)=I.HorizText paint (TL.fromStrict text) advance (T.length text)
    image (CellScript _ _ _ _)=V.emptyImage -- Source Sigils never contain scripted presentation.

-- Source images, direct source rows and plain plugin rows share paint and selection cuts.
-- The compositor owns clipping; glyphs retain their complete semantic identity.
sourceCellRow :: Bool -> Maybe V.Attr -> Bool -> Selection -> Int -> Rect -> Int -> SourceRow -> CellLayer
sourceCellRow dark override active sel start (Rect x y columns _) left row=
  CellRow (x+column-left) y x (x+columns) (Vec.fromList spans)
  where (column,spans)=sourceCellSpans id dark override active sel start left columns row

-- Project at emission so image callers do not retain an intermediate span list.
{-# INLINE sourceCellSpans #-}
sourceCellSpans :: (CellSpan -> a) -> Bool -> Maybe V.Attr -> Bool -> Selection -> Int -> Int -> Int -> SourceRow -> (Int,[a])
sourceCellSpans project dark override active sel start left columns row=(column,draw (start+char) sigils)
  where
    (char,column,sigils)=sourceSigilsWindow left columns row
    (lo,hi)=ordered sel
    color style=maybe normal (\paint->paint {V.attrStyle=V.attrStyle normal}) override
      where normal=syntaxAttr dark style
    selected paint=paint `V.withForeColor` blue `V.withBackColor` gray
    ordinary paint text=[project (CellText paint text) | not (T.null text)]
    draw _ Nil=[]
    draw offset (ConsChars text style rest)=
      let n=T.length text
          paint=color style
          a=max 0 (min n (lo-offset)); z=max a (min n (hi-offset))
          (before,tailText)=T.splitAt a text
          (chosen,after)=T.splitAt (z-a) tailText
          highlighted=ordinary paint before++ordinary (selected paint) chosen++ordinary paint after
      in (if active && z>a then highlighted else ordinary paint text)++draw (offset+n) rest
    draw offset (ConsSigil glyph style advance rest)=
      let original=graphemeText glyph
          n=T.length original
          paint=if active && offset<hi && offset+n>lo then selected (color style) else color style
          shown | T.any (\c->c<' ' || c=='\DEL') original=T.map (\c->if c<' ' || c=='\DEL' then '·' else c) original
                | otherwise=graphemeDisplayText glyph
          occupied | advance<=0=[]
                   | original=="\t"=ordinary paint (T.replicate advance " ")
                   | otherwise=[project (CellGlyph paint shown advance 0 advance)]
      in occupied++draw (offset+n) rest

styledImage :: Bool -> (Style -> Bool) -> Maybe V.Attr -> Bool -> Selection -> Int -> StyledText -> V.Image
styledImage dark selectable override active sel start styled=case styledRows styled of
  StyledRow sigils _ _:_->styledSigilsImage dark selectable override active sel Nothing start sigils
  []->V.emptyImage

-- Consume finalized borrowed rows directly; no pack/unpack or second glyph pass.
styledSigilsImage :: Bool -> (Style -> Bool) -> Maybe V.Attr -> Bool -> Selection -> Maybe [(Int,Int)] -> Int -> Sigils -> V.Image
styledSigilsImage dark selectable override active sel logicalSelection start=V.horizCat . draw start
  where
    (lo,hi)=ordered sel
    selectedRanges=fromMaybe [(lo,hi)] logicalSelection
    intersects a z=any (\(first,lastOffset)->a<lastOffset && first<z) selectedRanges
    color style=maybe normal (\paint->paint {V.attrStyle=V.attrStyle normal}) override
      where normal=syntaxAttr dark style
    selected paint=paint `V.withForeColor` blue `V.withBackColor` gray
    image paint text advance=[I.HorizText paint (TL.fromStrict text) advance (T.length text) | not (T.null text)]
    draw _ Nil=[]
    draw offset (ConsChars text style rest)=
      let n=T.length text; paint=color style
          a=max 0 (min n (lo-offset)); z=max a (min n (hi-offset))
          (before,tailText)=T.splitAt a text; (chosen,after)=T.splitAt (z-a) tailText
          pieces | active && selectable style,Just ranges<-logicalSelection=
              concat [image (if intersects (offset+first) (offset+lastOffset) then selected paint else paint) value (lastOffset-first)
                | (first,lastOffset,value)<-selectionRunPieces n (\value->max 0 (min n (value-offset))) ranges text]
                 | active && selectable style && z>a=
            image paint before a++image (selected paint) chosen (z-a)++image paint after (n-z)
                 | otherwise=image paint text n
      in pieces++draw (offset+n) rest
    draw offset (ConsSigil glyph style advance rest)=
      let original=graphemeText glyph; n=T.length original
          paint=if active && selectable style && intersects offset (offset+n) then selected (color style) else color style
          shown | original=="\r"=""
                | original=="\t"=T.replicate advance " "
                | graphemeOverflow glyph=graphemeDisplayText glyph
                | T.any (\c->c<' ' || c=='\DEL') original=T.map (\c->if c<' ' || c=='\DEL' then '·' else c) original
                | otherwise=graphemeDisplayText glyph
      in image paint shown advance++draw (offset+n) rest

-- Prepared semantic rows enter the common grid directly. Only the cached
-- horizontal glyph slice is visited; padding is a separate baseline layer.
-- Script metadata never passes through a Vty attribute or image round trip.
styledLayoutRow :: Bool -> (Style -> Bool) -> Bool -> Selection -> Maybe [(Int,Int)] -> Rect -> Int -> TextLayout.LayoutRow -> CellLayer
styledLayoutRow dark selectable active sel logicalSelection (Rect x y columns _) left row=
  CellRow (x+origin-left) y x (x+columns) (Vec.fromList (concatMap spans (Vec.toList visible)))
  where
    visible=TextLayout.layoutVisibleGlyphs left columns row
    origin=maybe left TextLayout.layoutColumn (visible Vec.!? 0)
    (lo,hi)=ordered sel
    selectedRanges=fromMaybe [(lo,hi)] logicalSelection
    spans glyph
      | step>0=if step==1 && TextLayout.layoutScript glyph==Nothing then ordinary else characters 0 text
      | otherwise=[case TextLayout.layoutScript glyph of
          Just script->CellScript (paintAt a z) text (TextLayout.layoutNatural glyph) script
          Nothing->CellGlyph (paintAt a z) text (TextLayout.layoutAdvance glyph) 0 (TextLayout.layoutAdvance glyph)]
      where
        text=TextLayout.layoutDisplayText glyph
        style=TextLayout.layoutStyle glyph
        normal=syntaxAttr dark style
        a=TextLayout.layoutStart glyph; z=TextLayout.layoutEnd glyph
        step=TextLayout.layoutRunStep glyph
        n=if step>0 then TextLayout.layoutAdvance glyph `div` step else 0
        selected paint=paint `V.withForeColor` blue `V.withBackColor` gray
        paintAt first lastOffset=if active && selectable style && any (\(lo,hi)->first<hi && lastOffset>lo) selectedRanges then selected normal else normal
        cut value | z<=a=0
                  | otherwise=max 0 (min n (((value-a)*n+z-a-1) `div` (z-a)))
        first=cut lo; lastOffset=max first (cut hi)
        (before,tailText)=T.splitAt first text
        (chosen,after)=T.splitAt (lastOffset-first) tailText
        piece paint value=[CellText paint value | not (T.null value)]
        ordinary | active && selectable style,Just ranges<-logicalSelection=
            concat [piece (paintAt (a+begin*(z-a) `div` n) (a+(end*(z-a)+n-1) `div` n)) value
              | (begin,end,value)<-selectionRunPieces n cut ranges text]
                 | active && selectable style && lastOffset>first=
            piece normal before++piece (selected normal) chosen++piece normal after
                 | otherwise=piece normal text
        characters index remaining=case T.uncons remaining of
          Nothing->[]
          Just (_,rest)->
            let char=T.take 1 remaining
                first=a+index*(z-a) `div` n
                lastOffset=a+((index+1)*(z-a)+n-1) `div` n
                paint=paintAt first lastOffset
                cell=case TextLayout.layoutScript glyph of
                  Just script->CellScript paint char 1 script
                  Nothing->CellGlyph paint char step 0 step
            in cell:characters (index+1) rest

-- Split only at bounded selected paint endpoints, borrowing each run slice.
-- Ordinary/source callers retain their single-Selection path above.
selectionRunPieces :: Int -> (Int -> Int) -> [(Int,Int)] -> Text -> [(Int,Int,Text)]
selectionRunPieces count cut ranges text=go 0 text (drop 1 endpoints)
  where
    endpoints=map head (group (sort (0:count:concat [[cut a,cut z] | (a,z)<-ranges])))
    go _ _ []=[]
    go start remaining (end:rest)=
      let (part,after)=T.splitAt (end-start) remaining
      in (start,end,part):go end after rest

-- One paint owner for ordinary and prepared semantic glyphs.
syntaxAttr :: Bool -> Style -> V.Attr
syntaxAttr dark style=paint style
  where
    paint style=let (base,bold,italic)=fontTraits style in
      foldl V.withStyle (baseAttr base) ([V.bold | bold]++[V.italic | italic])
    baseAttr (ScriptStyle _ style)=paint style
    baseAttr (SectionStyle _ style)=paint style
    baseAttr (ProseStyle (SectionStyle _ style))=paint (ProseStyle style)
    baseAttr (BubbleStyle outgoing (SectionStyle _ style))=paint (BubbleStyle outgoing style)
    baseAttr (LinkStyle _ style)=V.withStyle (paint style) V.underline
    baseAttr (ProseStyle (LinkStyle _ style))=V.withStyle (paint (ProseStyle style)) V.underline
    baseAttr (BubbleStyle outgoing (LinkStyle _ style))=V.withStyle (paint (BubbleStyle outgoing style)) V.underline
    baseAttr (ProseStyle (CodeStyle shell style))=paint (CodeStyle shell style)
    baseAttr (ProseStyle style) | dark = V.withForeColor (paint style) (case style of Plain->white; _->foreground style)
    baseAttr (ProseStyle style)=attr (case style of Heading 1->white; Heading 2->blue; Heading _->V.RGBColor 170 0 170; Keyword->blue; Literal->V.RGBColor 0 85 0; Comment->V.RGBColor 85 85 85; _->black) scrollCyan
    baseAttr (CodeStyle shell style)
      | dark = attr (foreground style) black
      | shell = attr (lightForeground style) gray
      | otherwise = attr (foreground style) blue
    baseAttr (BubbleStyle _ (CodeStyle shell style))=paint (CodeStyle shell style)
    baseAttr (BubbleText _ outgoing style)=paint (BubbleStyle outgoing style)
    baseAttr (BubbleStyle outgoing style)=attr bubbleForeground (if outgoing then scrollCyan else gray)
      where bubbleForeground | outgoing = black
                       | otherwise = case style of
                           Heading 1 -> blue; Heading _ -> V.RGBColor 170 0 170; Keyword -> blue; Comment -> V.RGBColor 85 85 85
                           Literal -> V.RGBColor 0 85 0; Number -> V.RGBColor 170 0 170
                           Constructor -> blue; Pragma -> V.RGBColor 85 85 85; _ -> black
    baseAttr (TerminalStyle fg bg flags)=foldl V.withStyle (attr (rgb fg) (rgb bg)) [style | (bit,style)<-[(1,V.bold),(2,V.italic),(4,V.underline),(8,V.strikethrough),(16,V.dim)], flags .&. bit /= 0]
      where rgb value=V.RGBColor (fromIntegral (value `shiftR` 16 .&. 255)) (fromIntegral (value `shiftR` 8 .&. 255)) (fromIntegral (value .&. 255))
    baseAttr style=attr (foreground style) blue
    foreground style=case style of Heading 1->white; Heading 2->cyan; Heading _->V.RGBColor 85 255 85; Plain->yellow; Keyword->white; Comment->cyan; Literal->V.RGBColor 85 255 85; Number->V.RGBColor 255 85 255; Constructor->yellow; _->gray
    lightForeground style=case style of Keyword->blue; Comment->V.RGBColor 85 85 85; Literal->V.RGBColor 0 85 0; Number->V.RGBColor 170 0 170; _->black

treeLayers :: Desktop -> Sidebar -> [V.Image]
treeLayers d tree =
  [place (w-5) 1 (label frame "[" V.<|> label (attr cyan blue) "←" V.<|> label frame "]")]
  ++ [place (w-1) y (V.char frame '│')
     | y<-[1..h], not (any (\win -> inside (bounds win) (w-1) y) (windows d))]
  ++ [place (w-2) 2 (V.vertCat [scrollCell n | n<-[0..visible-1]]) | treeFocused tree, visible>=3]
  ++ [place 1 2 (V.vertCat (map line listing)),place 0 1 (V.charFill frame ' ' (max 0 (w-1)) h)]
  where
    w=treeWidth tree; h=max 0 (snd (screenSize d)-2-problemsHeight d); visible=treeContentRows d
    frame=attr white blue
    listing=visibleRows (treeScroll tree) visible tree
    line (i,node)=V.cropRight listWidth (leading
      V.<|> label a (" "<>shownName) V.<|> counts V.<|> V.charFill a ' ' listWidth 1)
      where
        info=rowInfo node
        listWidth=max 0 (w-if treeFocused tree then 3 else 2)
        chosen=treeFocused tree && i==treeSelected tree
        changes=if Tree.infoBranch info then Nothing else Tree.infoResource info >>= (\path->case M.lookup path (treeBadges tree) of Just (True,added,deleted)->Just (added,deleted); _->Nothing)
        bg=if chosen then green else blue
        a=if changes/=Nothing then attr (V.RGBColor 255 85 85) bg else if chosen then selected else edit
        leading=label (if chosen then selected else frame) (rowPrefix node) V.<|> label iconColor marker
        available=max 0 (listWidth-V.imageWidth leading-1)
        badge=case changes of
          Just (added,deleted) | added/=0 || deleted/=0 ->
            label a " " V.<|> label (attr (V.RGBColor 85 255 85) bg) ("+"<>T.pack (show added))
            V.<|> label a " " V.<|> label (attr (V.RGBColor 255 85 85) bg) ("-"<>T.pack (show deleted))
          _ -> V.emptyImage
        counts=if V.imageWidth badge<available then badge else V.emptyImage
        shownName=T.take (columnOffset (Tree.infoLabel info) (available-V.imageWidth counts)) (Tree.infoLabel info)
        iconColor=if chosen then selected else attr (if Tree.infoBranch info then yellow else white) blue
        marker | Tree.infoIcon info=="📁", materialIcons d = if rowExpanded node then "\xf0770" else "\xf024b"
               | Tree.infoIcon info=="📁" = if rowExpanded node then "📂" else "📁"
               | Tree.infoIcon info=="" = if Tree.infoBranch info then if rowExpanded node then "▼" else "▶" else ""
               | otherwise = Tree.infoIcon info
    thumb=scrollbarThumb visible (treeScrollLimit d tree) (treeScroll tree)
    scrollCell n=V.char (if n==0 || n==visible-1 then attr blue scrollCyan else attr scrollCyan blue)
      (if n==0 then '▲' else if n==visible-1 then '▼' else if n==thumb then '█' else '░')

keyLegendOn :: V.Color -> Text -> V.Image
keyLegendOn bg text = V.horizCat [label (attr (if shortcut token then red else black) bg) token | token <- T.groupBy (\a b -> isSpace a == isSpace b) text]
  where shortcut t = t `elem` ["Tab","Enter","Esc","↑↓→←","↵"] || any (`T.isPrefixOf` t) ["F1","F2","F3","F5","F6","Ctrl+","Alt+","Shift+","Cmd+","⌘","⌥","⇧","⌃"]

menuLayers :: Desktop -> (Int,Int) -> [V.Image]
menuLayers d (i,j) = [place x y contents']
  where
    Rect x y w _=menuRect d i
    items=menuItemsFor d i
    contents'=V.vertCat [border '┌' '┐',V.vertCat (zipWith item [0..] items),border '└' '┘']
    border a b=V.char paper a V.<|> V.charFill paper '─' (max 0 (w-2)) 1 V.<|> V.char paper b
    item _ (MenuItem "" _ (Disabled _)) = V.char paper '├' V.<|> V.charFill paper '─' (max 0 (w-2)) 1 V.<|> V.char paper '┤'
    item n entry@(MenuItem title _ cmd) = V.char paper '│'  V.<|> V.cropRight (w-2) content V.<|> V.char paper '│'
      where
        key = menuShortcut d entry
        disabled = not (menuCommandAvailable d cmd)
        bg = if n == j then green else gray
        a = attr (if disabled then V.RGBColor 85 85 85 else black) bg
        hot = attr red bg
        pos = fromMaybe 0 (T.findIndex ((==menuMnemonic entry) . toLower) title)
        name = label a (T.take pos title) V.<|> label (if disabled then a else hot) (T.take 1 (T.drop pos title)) V.<|> label a (T.drop (pos+1) title)
        radio = case cmd of SetBufferView mode -> if defaultBufferView d==mode then "(●) " else "( ) "; _ -> ""
        content = menuRow (w-2) a (if disabled then a else hot) (label (attr black bg) radio V.<|> name) key

-- Keep shortcuts at the right edge in cell coordinates, cropping a long title
-- before its key. Both popup kinds use the same key color and wide-glyph path.
menuRow :: Int -> V.Attr -> V.Attr -> V.Image -> Text -> V.Image
menuRow w a hot name key = V.cropRight w $
  label a " " V.<|> shownName V.<|> V.charFill a ' ' gap 1 V.<|> shownKey V.<|> label a " "
  where
    shownKey=V.cropRight (max 0 (w-3)) (label hot key)
    shownName=V.cropRight (max 0 (w-3-V.imageWidth shownKey)) name
    gap=max 1 (w-2-V.imageWidth shownName-V.imageWidth shownKey)

bottomLayers :: [CanvasSurface] -> Desktop -> [CellLayer]
bottomLayers canvases d
  | not (bottomVisible d) || height r<2 = []
  | M.null (dockedTerminals d) = map CellImage (problemsLayers d)
  | otherwise = map CellImage (tabs++controls++[place 0 y (label frame (cornerLeft<>T.replicate (max 0 (width r-2)) horizontal<>cornerRight))])++content
  where
    r=problemsRect d; y=top r
    pane=bottomTerminal d >>= (\ident -> find ((==ident).windowId) (windows d))
    focused=maybe (problemsFocused d) (windowFocused d) pane
    (cornerLeft,horizontal,cornerRight)=if focused then ("╔","═","╗") else ("┌","─","┐")
    frame=attr (if focused then white else gray) blue
    tabs=[place (left tab) y (label (if ident==bottomTerminal d then attr black cyan else frame) title)
         | (tab,ident,title)<-bottomTabs d]
    controls=case bottomTerminal d of
      Just _ -> [place (width r-9) y (label frame "[" V.<|> label (attr cyan blue) "P" V.<|> label frame "]"),
                 place (width r-5) y (label frame "[" V.<|> label (attr (V.RGBColor 85 255 85) blue) "x" V.<|> label frame "]")]
      Nothing -> [place (width r-5) y (label frame "[" V.<|> label (attr cyan blue) "↓" V.<|> label frame "]")]
    content=case pane of
      Just w -> windowLayers canvases d focused w
      Nothing -> map CellImage (problemsLayers d)

problemsLayers :: Desktop -> [V.Image]
problemsLayers d
  | not (problemsVisible d) || h<2 = []
  | otherwise = [place (x+max 1 ((w-10) `div` 2)) y (label frame " Messages "),place (x+w-7-T.length number) y (label frame number)]
      ++ [place (x+w-5) y (label frame "[" V.<|> label (attr cyan scrollCyan) "↓" V.<|> label frame "]") | problemsFocused d]
      ++ [place (x+1) (y+1+i) (row (if problemsFocused d && index==problemsSelected d then attr white blue else bodyColor) (w-2) (format issue))
         | (i,(index,issue))<-zip [0..] (take (h-2) (drop (problemsScroll d) (zip [0..] (diagnostics d))))]
      ++ [place (x+1) (y+1) (row bodyColor (w-2) " No messages reported.") | null (diagnostics d)]
      ++ [place x y (box frame (problemsFocused d) w h)]
  where
    Rect x y w h=problemsRect d
    frame=attr (if problemsFocused d then white else blue) scrollCyan
    bodyColor=attr black scrollCyan
    number=maybe "" (T.pack . show) (messagesNumber d)
    format issue=" "<>(case diagnosticSeverity issue of 1 -> "Error "; 2 -> "Warning "; 3 -> "Info "; _ -> "Hint ")<>T.pack (takeFileName (diagnosticPath issue))<>":"<>T.pack (show (diagnosticRow issue+1))<>":"<>T.pack (show (diagnosticColumn issue+1))<>" "<>T.unwords (T.words (diagnosticMessage issue))

contextLayers :: Desktop -> (Rect,Int) -> [V.Image]
contextLayers d (r@(Rect x y w h),chosen) =
  [place (x+1) (y+i-contextOffset r chosen+1) (item i title cmd) | (i,(title,cmd))<-take (max 0 (h-2)) (drop (contextOffset r chosen) (zip [0..] (contextItemsFor d)))]
  ++ [place x y (box paper False w h)]
  where
    item i title cmd=menuRow (w-2) a (if disabled then a else attr red bg) (label a title) (menuShortcut d (MenuItem title "" cmd))
      where
        disabled=not (contextTargetCurrent d && commandEnabled d cmd)
        bg=if i==chosen then green else gray
        a=attr (if disabled then V.RGBColor 85 85 85 else black) bg

-- Dialog frames, fields, buttons and shadows appear in docs/site/screenshots/*.png.
-- Refresh those artifacts with tools/docs-screenshots.hs after visual changes.
dialogLayers :: Desktop -> Dialog -> [V.Image]
dialogLayers d dg =
  comboLayers ++ [place (x+max 1 ((w-T.length title) `div` 2)) y (label (attr white gray) title)]
  ++ [place (left r) (top r) (row (if chosen==mode then selected else paper) (width r) (if mode then " Replace " else " Find "))
      | Searching chosen _<-[purpose dg],(r,mode)<-searchTabRects d dg]
  ++ [place (left r) (top r) (label (attr white gray) "[x]") | approvalDialog dg,let r=dialogCloseRect d dg]
  ++ [place (bx+if pushed i then 1 else 0) by (V.cropRight bw (buttonImage i name)) | (i,(Rect bx by bw _,name))<-zip [0..] (zip (buttonRects d dg) (buttons dg))]
  ++ concat [buttonShadow gray r | (i,r)<-zip [0..] (buttonRects d dg), not (pushed i)]
  ++ concat [fieldLayer i r f | (i,(r,f))<-zip [0..] (zip (fieldRects d dg) (fields dg))]
  ++ [place (x+3) (y+2+i) (row paper (w-6) line) | (i,line)<-zip [0..] (body dg),y+2+i<y+h-3]
  ++ [place x y (box (attr white gray) True w h)]
  where
    Rect x y w h=dialogRect d dg
    comboLayers=case openComboBox dg of
      Nothing -> []
      Just (i,_,choices,_,preview) ->
        let Rect cx cy cw ch=comboBoxRect d dg i choices
        in [place (cx+1) (cy+1+n) (row (if n==preview then selected else paper) (cw-2) (" "<>choice)) | (n,choice)<-zip [0..] choices]
           ++ [place cx cy (box paper False cw ch)]
    title=" "<>dialogTitle dg<>" "
    pushed i=buttonPressed d==Just i && buttonHover d==Just i
    buttonImage i name = label normal "  " V.<|> label normal (T.take pos name)
      V.<|> label (attr white bg) (T.take 1 (T.drop pos name)) V.<|> label normal (T.drop (pos+1) name)
      V.<|> label normal "  "
      where
        bg | pushed i = V.RGBColor 0 85 0
           | buttonHover d==Just i = V.RGBColor 85 255 85
           | otherwise = green
        normal=attr (if focus dg==length (fields dg)+i then white else black) bg
        mnemonic=fromMaybe Nothing (atMay (buttonMnemonics dg) i)
        pos=fromMaybe (T.length name) (mnemonic >>= \c -> T.findIndex ((==c) . toLower) name)
    fieldLayer i rect@(Rect fx fy fw fh) field
      | fy>=y+h-3 = []
      | otherwise = [place fx (max (y+2) fy) (V.cropBottom (max 0 (y+h-3-max (y+2) fy)) (V.translateY (min 0 (fy-y-2)) image))]
      where
        a=if focus dg==i then selected else paper
        inputColor=case purpose dg of Opening{} -> attr white blue; ChangingDirectory{} -> attr white blue; _ -> attr black scrollCyan
        image=case field of
          Input name value p -> let offset=if focus dg==i then max 0 (displayColumn value p-fw+1) else 0
                               in V.vertCat [row paper fw name,V.cropRight fw (V.translateX (negate offset) (label inputColor value) V.<|> V.charFill inputColor ' ' fw 1)]
          SelectedInput name value sel ->
            let offset=if focus dg==i then max 0 (displayColumn value (caret sel)-fw+1) else 0
                (a,z)=ordered sel
                text=label inputColor (T.take a value) V.<|> label (if focus dg==i then selected else inputColor) (T.take (z-a) (T.drop a value)) V.<|> label inputColor (T.drop z value)
            in V.vertCat [row paper fw name,V.cropRight fw (V.translateX (negate offset) text V.<|> V.charFill inputColor ' ' fw 1)]
          ComboBox name choices chosen _ -> V.vertCat [row paper fw name,
            row inputColor (fw-2) (fromMaybe "" (atMay choices chosen)) V.<|> label (attr blue scrollCyan) " ▼"]
          ReadOnly name value -> let lw=min 18 (fw `div` 3) in row a lw (name<>":") V.<|> row paper (fw-lw) value
          TextArea name editable b sel sr sc ->
            let area=textAreaRect rect field
                pane=attr white blue
                line n=V.cropRight (width area)
                  (styledSourceImage False (Just (if editable then diffLineAttr (bufferLineAt b n) else pane))
                    (focus dg==i && editable) sel (bufferLineOffset b n) sc (width area) (plainSourceRow (bufferLineAt b n))
                  V.<|> V.charFill pane ' ' (width area) 1)
                thumb=sr*max 0 (height area-1) `div` max 1 (bufferLineCount b-height area)
                content=V.vertCat [line n V.<|> label (attr blue scrollCyan) (if n-sr==thumb then "■" else "│") | n<-[sr..sr+height area-1]]
                position=T.pack (show (sr+1))<>"/"<>T.pack (show (bufferLineCount b))<>"  ← → ↑ ↓ PgUp PgDn"
            in if editable then V.vertCat [row paper fw (name<>"  (editable)"),content,row paper fw position]
               else (row paper (left area-fx) (name<>":") V.<-> V.charFill paper ' ' (left area-fx) (max 0 (fh-1))) V.<|> V.vertCat [content,row paper (fw-(left area-fx)) position]
          CheckBox name checked -> row a fw ((if checked then "[X] " else "[ ] ")<>name)
          Radio name values chosen -> V.vertCat (row paper fw name:[row (if focus dg==i && n==chosen then selected else paper) fw ((if n==chosen then "(●) " else "( ) ")<>v) | (n,v)<-zip [0..] values])
          FileList entries chosen ->
            let cw=max 1 ((fw-3) `div` 2); page=(max 0 chosen `div` 16)*16
                listColor=attr black scrollCyan
                borderColor=attr blue scrollCyan
                item idx=case drop idx entries of
                  e:_ -> row (if idx==chosen then attr (if focus dg==i then white else black) green else listColor) cw (" "<>entryName e<>(if entryDirectory e then "/" else ""))
                  _ -> row listColor cw ""
                bar=label borderColor "┌" V.<|> V.charFill borderColor '─' cw 1 V.<|> label borderColor "┬" V.<|> V.charFill borderColor '─' cw 1 V.<|> label borderColor "┐"
                line r=label borderColor "│" V.<|> item (page+r) V.<|> label borderColor "│" V.<|> item (page+8+r) V.<|> label borderColor "│"
                path=case purpose dg of Opening base pattern _ -> T.pack (base </> T.unpack pattern); ChangingDirectory base _ -> T.pack base; _ -> ""
                details=case drop chosen entries of
                  entry:_ | chosen>=0 ->
                    let size=if entryDirectory entry then "<DIR>" else maybe "?" (T.pack . show) (entryBytes entry)<>" bytes"
                        stamp=maybe "" (T.pack . formatTime defaultTimeLocale "%b %e, %Y %H:%M") (entryModified entry)
                        suffix="  "<>size<>"  "<>stamp
                    in T.take (columnOffset (entryName entry) (max 0 (fw-T.length suffix))) (entryName entry)<>suffix
                  _ -> ""
            in V.vertCat ([row paper fw (case purpose dg of ChangingDirectory{} -> "Directories"; _ -> "Files"),bar] ++ [line r | r<-[0..7]] ++ [label borderColor "└" V.<|> V.charFill borderColor '─' cw 1 V.<|> label borderColor "┴" V.<|> V.charFill borderColor '─' cw 1 V.<|> label borderColor "┘",row (attr scrollCyan blue) fw path,row (attr scrollCyan blue) fw details])
          ListBox name values chosen -> V.vertCat (row paper fw name:[row (if n==chosen then a else paper) fw (" "<>v) | (n,v)<-take 4 (drop (max 0 (chosen-3)) (zip [0..] values))])

-- | Render a colorless character-grid snapshot for inspection and tests.
snapshot :: Desktop -> Text
snapshot d = T.unlines [T.concat (map plain (toList ops)) | ops<-toList (cellDisplayOps (renderCellRows d))]
  where plain (TextSpan _ n _ t)=terminalProjection n (TL.toStrict t)
        plain (Skip n)=T.replicate n " "
        plain (RowEnd n)=T.replicate n " "

-- | Render a standalone HTML view of the current grid and its colors.
snapshotHtml :: Desktop -> Text
snapshotHtml d = "<!doctype html><meta charset='utf-8'><title>Haskell</title><style>body{background:#111;margin:24px;display:grid;place-content:center;min-height:90vh}pre{background:#0000aa;font:min(20px,calc((100vw - 48px)/48))/1.066667 'Courier New',monospace;margin:0;box-shadow:0 0 0 2px #333;white-space:pre}span{font-weight:normal}</style><pre>" <> T.intercalate "\n" rows <> "</pre>"
  where
    rows=[T.concat (map spanHtml (Vec.toList spans)) | spans<-Vec.toList (renderCellRows d)]
    spanHtml (CellText a text)=colored a (body (T.length text) text)
    spanHtml (CellGlyph a text full start shown)=colored a
      (if start/=0 || shown/=full then T.replicate shown " " else body full text)
    spanHtml (CellScript a text natural script)=colored a
      ("<span style='font-weight:inherit;display:inline-block;position:relative;width:1ch;height:1em;vertical-align:bottom'><span style='font-weight:inherit;position:absolute;left:0;top:"<>
       (if script==Superscript then "0" else "0.5em")<>";width:"<>T.pack (show natural)<>"ch;line-height:1em;transform:scale(0.5);transform-origin:top left'>"<>escape text<>"</span></span>")
    colored a contents="<span style='color:"<>color (V.attrForeColor a)<>";background:"<>color (V.attrBackColor a)<>
      (if V.styleMask a .&. V.bold/=0 then ";font-weight:bold" else "")<>
      (if V.styleMask a .&. V.italic/=0 then ";font-style:italic" else "")<>decoration a<>"'>"<>contents<>"</span>"
    body advance text | [(g,2)]<-displayClusters advance text,clusterWidth g<2 = "<span style='display:inline-block;width:2ch'><span style='display:inline-block;transform:scaleX(2);transform-origin:left'>"<>escape g<>"</span></span>"
                      | otherwise=escape text
    decoration a=case [name | (flag,name)<-[(V.underline,"underline"),(V.strikethrough,"line-through")],V.styleMask a .&. flag/=0] of
      []->""
      styles->";text-decoration:"<>T.unwords styles
    color (V.SetTo (V.RGBColor r g b))="rgb("<>T.intercalate "," (map (T.pack.show) [r,g,b])<>")"
    color _="#aaa"
    escape=T.concatMap (\c -> case c of '&'->"&amp;"; '<'->"&lt;"; '>'->"&gt;"; _->T.singleton c)
