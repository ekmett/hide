{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Render (renderDesktop, snapshot, snapshotHtml) where

import qualified Graphics.Vty as V
import THC.Edit.Unicode (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Map.Strict as M
import Data.Foldable (toList)
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe)
import System.FilePath (takeFileName, (</>))
import Data.Bits ((.&.), shiftR)
import Data.Time (formatTime, defaultTimeLocale)
import THC.Edit.Hex
import THC.Edit.Buffer
import THC.Edit.Unicode (graphemes, clusterWidth, textImage, flattenPicture)
import THC.Edit.Model
import THC.Edit.Syntax
import THC.Edit.Files (filePath)
import THC.Edit.Browser (Entry(..))

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

renderDesktop :: Desktop -> V.Picture
renderDesktop d = flattenPicture (screenSize d) ((V.picForLayers layers) {V.picCursor=cursor})
  where
    (sw,sh)=screenSize d
    layers = case dialog d of
      Nothing -> withMenu
      Just dg -> dialogLayers d dg ++ [castShadow (screenSize d) (dialogRect d dg) withMenu] ++ withMenu
    withMenu = case contextMenu d of
      Just popup@(r,_) -> contextLayers d popup ++ [castShadow (screenSize d) r base] ++ base
      Nothing -> withMainMenu
    withMainMenu = case menu d of
      Nothing -> base
      Just m@(i,_) -> menuLayers d m ++ [castShadow (screenSize d) (menuRect d i) base] ++ base
    base = [place 0 0 menuBar, place 0 (sh-1) statusBar]
      ++ problemsLayers d
      ++ maybe [] (treeLayers d) (sideTree d)
      ++ foldr stackWindow [V.charFill (attr blue gray) '░' sw sh] (windows d)
    stackWindow w below = windowLayers d (windowFocused d w) w ++ [castShadow (screenSize d) (bounds w) below] ++ below
    menuBar = V.cropRight sw (V.char paper ' ' V.<|> V.horizCat
      [V.char normal ' ' V.<|> label (attr red bg) (T.take 1 title) V.<|> label normal (T.drop 1 title<>" ")
       | (i,(title,_,_))<-zip [0..] menus,let bg=if fmap fst (menu d)==Just i then green else gray,let normal=attr black bg]
      V.<|> V.charFill paper ' ' sw 1)
    statusBar = V.cropRight (max 0 (sw-T.length badge)) (V.horizCat
      [keyLegendOn (if statusHover d==Just i && action/=Nothing then green else gray) text
      | (i,(text,action))<-zip [0..] (statusItems d)] V.<|> V.charFill paper ' ' sw 1) V.<|> badgeImage
    badge = if activeConversation d then "" else gitBadgeText d
    badgeImage = if T.null badge then V.emptyImage else label paper (" │ "<>gitBranchText d<>" ")
      V.<|> label (attr green gray) ("+"<>gitCountText (branchAdded d)) V.<|> label paper " "
      V.<|> label (attr red gray) ("-"<>gitCountText (branchDeleted d)) V.<|> label paper " "
    cursor = case dialog d of
      Just dg -> case drop (focus dg) (zip (fieldRects d dg) (fields dg)) of
        (Rect x y w _,Input _ value p):_ -> let offset=max 0 (displayColumn value p-w+1)
                                         in V.Cursor (x+displayColumn value p-offset) (y+1)
        _ -> V.NoCursor
      Nothing | menu d/=Nothing || contextMenu d/=Nothing || problemsFocused d || maybe False treeFocused (sideTree d) -> V.NoCursor
      Nothing -> case (activeWindow d,activeDocument d) of
        (Just w,_) | composerActive d -> let
          b=composerBuffer d; (r,c)=bufferLineColumn b (caret (composerSelection d)); (sr,sc)=composerScroll d w
          rect=composerRect d w
          in if height rect>0 && width rect>0 then V.Cursor (left rect+displayColumn (bufferLineAt b r) c-sc) (top rect+r-sr) else V.NoCursor
        (_,Just doc) | not (documentCursorVisible doc) -> V.NoCursor
        (Just w,Just doc) -> let { (r,c)=windowCursorCell (documentBuffer doc) w; x=left (bounds w)+1+c-scrollColumn w; y=top (bounds w)+1+r-scrollRow w }
                            in if inside (Rect (left (bounds w)+1) (top (bounds w)+1) (width (bounds w)-2) (windowContentRows d doc w)) x y then V.Cursor x y else V.NoCursor
        _ -> V.NoCursor

-- A DOS shadow changes the underlying cell attributes, preserving its glyph.
-- Flatten only the layers below the popup, so stacked popups shadow correctly.
castShadow :: (Int,Int) -> Rect -> [V.Image] -> V.Image
castShadow size (Rect x y w h) below =
  place (x+2) (y+1) (V.crop w h (V.translate (negate (x+2)) (negate (y+1)) dimmed))
  where
    dimmed = V.vertCat [V.horizCat (map dim (toList spans)) | spans <- toList (displayOpsForPic (flattenPicture size (V.picForLayers below)) size)]
    dim TextSpan{textSpanText=t} = label shadow (TL.toStrict t)
    dim (Skip n) = V.charFill shadow ' ' n 1
    dim (RowEnd n) = V.charFill shadow ' ' n 1

windowLayers :: Desktop -> Bool -> Window -> [V.Image]
windowLayers d active w =
  [place x (y+1+diagnosticRow issue-scrollRow w) (label (attr (if diagnosticSeverity issue==1 then V.RGBColor 255 85 85 else yellow) blue) "▶")
    | not (byteMode (documentBuffer doc)), issue<-diagnostics d, Just (diagnosticPath issue)==fmap filePath (documentFile doc), diagnosticRow issue>=scrollRow w, diagnosticRow issue<scrollRow w+hh-2]
  ++ (if active then
    [place (x+2) y (label frame "[" V.<|> label (attr (V.RGBColor 85 255 85) blue) (if videoMode d==Nothing then "x" else "■") V.<|> label frame "]"),place (x+ww-6) y (label frame "[" V.<|> label (attr cyan blue) "↑" V.<|> label frame "]")
    ,place (x+windowPositionColumn doc) (y+hh-1) (label frame (T.take (max 0 (ww-windowPositionColumn doc-2)) (windowPositionText d doc w)))
    ,scrollbarImage True,scrollbarImage False] else [])
  ++ composerLayers
  ++ hexDividerLayers
  ++ [place (x+ww-7-T.length number) y (label frame number)
  ,place (x+titleColumn) y (label frame shownTitle)
  ,place (x+1) (y+1) documentImage
  ,place x y (box frame (active && not moving) ww hh)]
  where
    Rect x y ww hh=bounds w
    doc=fromMaybe (newDocument (newBuffer "") Nothing) (M.lookup (bufferId w) (buffers d))
    b=documentBuffer doc; t=contents b
    file=maybe ("NONAME"<>T.pack (show (bufferId w))<>".HS") (T.pack . takeFileName . filePath) (documentFile doc)
    title=" "<>(if documentLabel doc==Just "Conversation" then conversationTitle d else fromMaybe file (documentLabel doc))<>(if dirty b then " * " else " ")
    (titleColumn,shownTitle)
      | byteMode b = let start=max 6 (1+hexColumn 0-scrollColumn w)
                         end=min (ww-8-T.length number) (hexAsciiColumn (windowHexBytes w)-scrollColumn w)
                         clipped=T.take (columnOffset title (max 0 (end-start))) title
                     in (start+max 0 ((end-start-displayColumn clipped (T.length clipped)) `div` 2),clipped)
      | otherwise = let clipped=T.take (columnOffset title (max 0 (ww-17-T.length number))) title
                    in (max 6 ((ww-displayColumn clipped (T.length clipped)) `div` 2),clipped)
    number=T.pack (show (windowNumber w))
    moving=case drag d of Just (Moving wid _ _) -> wid==windowId w; Just (Resizing wid _ _) -> wid==windowId w; _ -> False
    frame=attr (if moving then cyan else if active then white else gray) blue
    styledLines=splitStyled (if documentLabel doc /= Nothing && documentLabel doc /= Just "Conversation" && not (maybe False (T.isPrefixOf "Terminal ") (documentLabel doc)) then [(ch,Plain) | ch<-T.unpack t] else documentHighlight doc)
    scrollbarImage vertical =
      let Rect sx sy bw bh=scrollbarRect d vertical doc w
          len=if vertical then bh else bw
          thumb=scrollbarThumb len (scrollbarLimit d vertical doc w) (if vertical then scrollRow w else scrollColumn w)
          cell n=V.char (if n==0 || n==len-1 then attr blue scrollCyan else attr scrollCyan blue)
            (if n==0 then if vertical then '▲' else '◄' else if n==len-1 then if vertical then '▼' else '►' else if n==thumb then '█' else '░')
      in place sx sy ((if vertical then V.vertCat else V.horizCat) [cell n | n<-[0..len-1]])
    composerLayers
      | documentLabel doc/=Just "Conversation" = []
      | otherwise = [place (left rect) (top rect) inputImage] ++ thoughtEdges
      where
        rect=composerRect d w; draft=composerBuffer d; (sr,sc)=composerScroll d w
        thoughtEdges
          | width rect<=0 || height rect<=0 = []
          | otherwise = [place (left rect-1) (top rect) (edgeImage True),
                         place (left rect+width rect) (top rect) (edgeImage False),
                         place (left rect+width rect+1) (top rect) (label (attr scrollCyan blue) "o.")]
        edgeImage leftSide=V.vertCat
          [V.char (if corner then attr scrollCyan blue else attr black scrollCyan)
            (if corner then bubbleTile (videoMode d/=Nothing) shape else ' ')
          | n<-[0..height rect-1], let corner=n==0 || n==height rect-1,
            let shape=if height rect==1 then if leftSide then 4 else 5
                      else (if n==0 then 0 else 2)+(if leftSide then 0 else 1)]
        inputImage=V.vertCat [V.cropRight (width rect) (V.translateX (negate sc)
          (styledImage (const True) Nothing (active && composerFocused d) (composerSelection d) (bufferLineOffset draft n) [(c,BubbleStyle True Plain) | c<-T.unpack (bufferLineAt draft n)]) V.<|> V.charFill (attr black scrollCyan) ' ' (width rect) 1)
          | n<-[sr..sr+height rect-1]]
    hexDividerLayers =
      [place (x+1+column) y (V.vertCat [V.char frame (if active && not moving then '╤' else '┬'),
        V.charFill frame '│' 1 contentHeight,V.char frame (if active && not moving then '╧' else '┴')])
      | byteMode b, divider<-hexDividers (windowHexBytes w), let column=divider-scrollColumn w, column>=0, column<contentWidth]
    contentWidth=max 0 (ww-2); contentHeight=windowContentRows d doc w
    documentImage=V.vertCat [renderLine n | n<-[scrollRow w..scrollRow w+contentHeight-1]]
    selectable style | documentLabel doc==Just "Conversation" = case style of BubbleText{} -> True; _ -> False
                     | otherwise = True
    renderLine n | byteMode b && n>=documentRows doc w = V.charFill edit ' ' contentWidth 1
    renderLine n | byteMode b = V.cropRight contentWidth (V.translateX (negate (scrollColumn w)) (V.horizCat
      [V.char (if active && maybe False highlighted offset then selected else if ch=='.' && maybe False (\i -> T.index bytes (i-n*count)/='.') offset then attr gray blue else edit) ch | (ch,offset)<-hexRow count n t]) V.<|> V.charFill edit ' ' contentWidth 1)
      where
        count=windowHexBytes w
        bytes=T.take count (T.drop (n*count) t)
        highlighted offset = offset==caret (selection w) || let (a,z)=ordered (selection w) in offset>=a && offset<z
    renderLine n=V.cropRight contentWidth (V.translateX (negate (scrollColumn w)) (styledImage selectable (lineColor n) active (selection w) (bufferLineOffset b n) (fromMaybe [] (atMay styledLines n))) V.<|> V.charFill edit ' ' contentWidth 1)

    lineColor n = case documentLabel doc of
      Just "Git diff" -> let line=bufferLineAt b n in Just (attr (if "+" `T.isPrefixOf` line then V.RGBColor 85 255 85 else if "-" `T.isPrefixOf` line then V.RGBColor 255 85 85 else if "@@" `T.isPrefixOf` line then cyan else yellow) blue)
      Just "Conversation" -> Nothing
      Just name | "Terminal " `T.isPrefixOf` name -> Nothing
      Just _ -> Just (attr yellow blue)
      Nothing -> Nothing

atMay :: [a] -> Int -> Maybe a
atMay xs n = case drop n xs of a:_ -> Just a; [] -> Nothing

splitStyled :: [(Char,Style)] -> [[(Char,Style)]]
splitStyled []=[[]]
splitStyled xs=let (a,b)=break ((=='\n').fst) xs in a:case b of []->[]; _:rest->splitStyled rest

styledImage :: (Style -> Bool) -> Maybe V.Attr -> Bool -> Selection -> Int -> [(Char,Style)] -> V.Image
styledImage selectable override active sel start chars = V.horizCat (expand 0 start (graphemes (T.pack (map fst chars))) chars)
  where
    (lo,hi)=ordered sel
    expand _ _ [] _=[]
    expand col offset (g:gs) styled = image : expand (col+width) (offset+T.length g) gs (drop (T.length g) styled)
      where
        style=case styled of (_,s):_->s; _->Plain
        a=if active && selectable style && offset<hi && offset+T.length g>lo then attr blue gray else fromMaybe (syntaxAttr style) override
        text | g=="\r"=""
             | g=="\t"=T.replicate (8-col `mod` 8) " "
             | otherwise=T.map (\c -> if c<' ' || c=='\DEL' then '·' else c) g
        width=sum (map clusterWidth (graphemes text))
        image=label a text
    syntaxAttr (BubbleText _ outgoing style)=syntaxAttr (BubbleStyle outgoing style)
    syntaxAttr (BubbleStyle outgoing style)=attr foreground (if outgoing then scrollCyan else gray)
      where foreground | outgoing = black
                       | otherwise = case style of
                           Keyword -> blue; Comment -> V.RGBColor 85 85 85
                           Literal -> V.RGBColor 0 85 0; Number -> V.RGBColor 170 0 170
                           Constructor -> blue; Pragma -> V.RGBColor 85 85 85; _ -> black
    syntaxAttr (TerminalStyle fg bg flags)=foldl V.withStyle (attr (rgb fg) (rgb bg)) [style | (bit,style)<-[(1,V.bold),(2,V.italic),(4,V.underline),(8,V.strikethrough),(16,V.dim)], flags .&. bit /= 0]
      where rgb value=V.RGBColor (fromIntegral (value `shiftR` 16 .&. 255)) (fromIntegral (value `shiftR` 8 .&. 255)) (fromIntegral (value .&. 255))
    syntaxAttr style=attr (case style of Plain->yellow; Keyword->white; Comment->cyan; Literal->V.RGBColor 85 255 85; Number->V.RGBColor 255 85 255; Constructor->yellow; Pragma->gray) blue

treeLayers :: Desktop -> Sidebar -> [V.Image]
treeLayers d tree =
  [place (max 2 ((w-11) `div` 2)) 1 (label frame " Files "),
   place (w-5) 1 (label frame "[" V.<|> label (attr cyan blue) "←" V.<|> label frame "]")]
  ++ [place (w-1) y (V.char frame '│')
     | y<-[1..h], not (any (\win -> inside (bounds win) (w-1) y) (windows d))]
  ++ [place (w-2) 2 (V.vertCat [scrollCell n | n<-[0..visible-1]]) | treeFocused tree, visible>=3]
  ++ [place 1 2 (V.vertCat (map line listing)),place 0 1 (V.charFill frame ' ' (max 0 (w-1)) h)]
  where
    w=treeWidth tree; h=max 0 (snd (screenSize d)-2-problemsHeight d); visible=treeContentRows d
    frame=attr white blue
    listing=take visible (drop (treeScroll tree) (zip [0..] (treeRows tree)))
    line (i,node)=V.cropRight listWidth (label a (T.replicate (2*nodeDepth node) " ")
      V.<|> label iconColor marker V.<|> label a (" "<>nodeName node) V.<|> V.charFill a ' ' listWidth 1)
      where
        listWidth=max 0 (w-if treeFocused tree then 3 else 2)
        chosen=treeFocused tree && i==treeSelected tree
        changed=any (\doc -> dirty (documentBuffer doc) && maybe False ((==nodePath node).filePath) (documentFile doc)) (M.elems (buffers d))
        a=if changed then attr (V.RGBColor 255 85 85) (if chosen then green else blue) else if chosen then selected else edit
        iconColor=if chosen then selected else attr (if nodeDirectory node then yellow else white) blue
        marker | nodeDirectory node = if nodeExpanded node then "📂" else "📁"
               | otherwise = "📄"
    thumb=scrollbarThumb visible (treeScrollLimit d tree) (treeScroll tree)
    scrollCell n=V.char (if n==0 || n==visible-1 then attr blue scrollCyan else attr scrollCyan blue)
      (if n==0 then '▲' else if n==visible-1 then '▼' else if n==thumb then '█' else '░')

keyLegendOn :: V.Color -> Text -> V.Image
keyLegendOn bg text = V.horizCat [label (attr (if shortcut token then red else black) bg) token | token <- T.groupBy (\a b -> isSpace a == isSpace b) text]
  where shortcut t = t `elem` ["Tab","Enter","Esc","↑↓→←","↵"] || any (`T.isPrefixOf` t) ["F1","F2","F3","F5","F6","Ctrl+","Alt+","Shift+","Cmd+"]

menuLayers :: Desktop -> (Int,Int) -> [V.Image]
menuLayers d (i,j) = [place x y contents']
  where
    Rect x y w _=menuRect d i
    items=menuItems i
    contents'=V.vertCat [border '┌' '┐',V.vertCat (zipWith item [0..] items),border '└' '┘']
    border a b=V.char paper a V.<|> V.charFill paper '─' (max 0 (w-2)) 1 V.<|> V.char paper b
    item n entry@(MenuItem title _ cmd) = V.char paper '│' V.<|> V.cropRight (w-2) content V.<|> V.char paper '│'
      where
        key = menuShortcut d entry
        disabled = not (commandEnabled d cmd)
        bg = if n == j then green else gray
        a = attr (if disabled then V.RGBColor 85 85 85 else black) bg
        hot = attr red bg
        pos = fromMaybe 0 (T.findIndex ((==menuMnemonic entry) . toLower) title)
        name = label a (T.take pos title) V.<|> label (if disabled then a else hot) (T.take 1 (T.drop pos title)) V.<|> label a (T.drop (pos+1) title)
        content = label a " " V.<|> name V.<|> label a (T.replicate (max 1 (w-4-T.length title-T.length key)) " ") V.<|> label (if disabled then a else hot) key V.<|> label a " "

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
  [place (x+1) (y+i-contextOffset r chosen+1) (row (attr (if commandEnabled d cmd then black else V.RGBColor 85 85 85) (if i==chosen then green else gray)) (w-2) (" "<>title)) | (i,(title,cmd))<-take (max 0 (h-2)) (drop (contextOffset r chosen) (zip [0..] (contextItems (contextKind d))))]
  ++ [place x y (box paper False w h)]

dialogLayers :: Desktop -> Dialog -> [V.Image]
dialogLayers d dg =
  [place (x+max 1 ((w-T.length title) `div` 2)) y (label (attr white gray) title)]
  ++ [place (bx+if pushed i then 1 else 0) by (V.cropRight bw (buttonImage i name)) | (i,(Rect bx by bw _,name))<-zip [0..] (zip (buttonRects d dg) (buttons dg))]
  ++ concat [buttonShadow gray r | (i,r)<-zip [0..] (buttonRects d dg), not (pushed i)]
  ++ concat [fieldLayer i r f | (i,(r,f))<-zip [0..] (zip (fieldRects d dg) (fields dg))]
  ++ [place (x+3) (y+2+i) (row paper (w-6) line) | (i,line)<-zip [0..] (body dg),y+2+i<y+h-3]
  ++ [place x y (box (attr white gray) True w h)]
  where
    Rect x y w h=dialogRect d dg
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
    fieldLayer i (Rect fx fy fw _) field
      | fy>=y+h-3 = []
      | otherwise = [place fx (max (y+2) fy) (V.cropBottom (max 0 (y+h-3-max (y+2) fy)) (V.translateY (min 0 (fy-y-2)) image))]
      where
        a=if focus dg==i then selected else paper
        inputColor=case purpose dg of Opening{} -> attr white blue; ChangingDirectory{} -> attr white blue; _ -> attr black scrollCyan
        image=case field of
          Input name value p -> let offset=if focus dg==i then max 0 (displayColumn value p-fw+1) else 0
                               in V.vertCat [row paper fw name,V.cropRight fw (V.translateX (negate offset) (label inputColor value) V.<|> V.charFill inputColor ' ' fw 1)]
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

snapshot :: Desktop -> Text
snapshot d = T.unlines [T.concat (map plain (toList ops)) | ops<-toList (displayOpsForPic (renderDesktop d) (screenSize d))]
  where plain TextSpan{textSpanText=t}=TL.toStrict t
        plain (Skip n)=T.replicate n " "
        plain (RowEnd n)=T.replicate n " "

-- Headless preview uses the actual Vty output spans, not a second UI renderer.
snapshotHtml :: Desktop -> Text
snapshotHtml d = "<!doctype html><meta charset='utf-8'><title>Turbo Haskell</title><style>body{background:#111;margin:24px;display:grid;place-content:center;min-height:90vh}pre{background:#0000aa;font:min(20px,calc((100vw - 48px)/48))/1.066667 'Courier New',monospace;margin:0;box-shadow:0 0 0 2px #333;white-space:pre}span{font-weight:normal}</style><pre>" <> T.intercalate "\n" rows <> "</pre>"
  where
    rows=[T.concat (map spanHtml (toList ops)) | ops<-toList (displayOpsForPic (renderDesktop d) (screenSize d))]
    spanHtml TextSpan{textSpanAttr=a,textSpanText=t}="<span style='color:"<>color (V.attrForeColor a)<>";background:"<>color (V.attrBackColor a)<>"'>"<>escape (TL.toStrict t)<>"</span>"
    spanHtml (Skip n)=T.replicate n " "
    spanHtml (RowEnd n)=T.replicate n " "
    color (V.SetTo (V.RGBColor r g b))="rgb("<>T.intercalate "," (map (T.pack.show) [r,g,b])<>")"
    color _="#aaa"
    escape=T.concatMap (\c -> case c of '&'->"&amp;"; '<'->"&lt;"; '>'->"&gt;"; _->T.singleton c)
