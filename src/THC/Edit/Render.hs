{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Render (renderDesktop, snapshot, snapshotHtml) where

import qualified Graphics.Vty as V
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Map.Strict as M
import Data.Foldable (toList)
import Data.List (groupBy)
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe)
import System.FilePath (takeFileName)
import THC.Edit.Buffer
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
label = V.text'
row :: V.Attr -> Int -> Text -> V.Image
row a w t = V.cropRight (max 0 w) (label a t V.<|> V.charFill a ' ' (max 0 w) 1)
place :: Int -> Int -> V.Image -> V.Image
place x y = V.translate (max 0 x) (max 0 y)
box :: V.Attr -> Bool -> Int -> Int -> V.Image
box a double w h
  | w<2 || h<2 = V.charFill a ' ' (max 0 w) (max 0 h)
  | otherwise = V.vertCat [line tl hz tr, V.vertCat (replicate (h-2) (V.char a vt V.<|> V.charFill a ' ' (w-2) 1 V.<|> V.char a vt)),line bl hz br]
  where (tl,tr,bl,br,hz,vt)=if double then ('╔','╗','╚','╝','═','║') else ('┌','┐','└','┘','─','│')
        line l m r=V.char a l V.<|> V.charFill a m (w-2) 1 V.<|> V.char a r

renderDesktop :: Desktop -> V.Picture
renderDesktop d = (V.picForLayers layers) {V.picCursor=cursor}
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
    statusBar = V.cropRight (max 0 (sw-T.length badge)) (keyLegend statusText V.<|> V.charFill paper ' ' sw 1) V.<|> badgeImage
    statusText
      | Just _ <- dragOriginal d = " ↑↓→← Move  Shift+↑↓→← Resize  ↵ Done  Esc Cancel"
      | Just text <- menuHelp d = " "<>text
      | Just c <- prefix d = " Ctrl+"<>T.singleton c<>"-  (Esc cancels)"
      | dialog d/=Nothing = " Tab Next  Enter Select  Esc Cancel"
      | not (T.null (typeHint d)) = " "<>typeHint d
      | not (T.null (status d)) = " F1 Help | "<>status d
      | otherwise = " F1 Help  F2 Save  F3 Open  F5 Zoom  F6 Next  F10 Menu"
    badge = gitBadgeText d
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
        (Just w,Just doc) -> let { (r,c)=bufferLineColumn (documentBuffer doc) (caret (selection w)); x=left (bounds w)+1+displayColumn (bufferLineAt (documentBuffer doc) r) c-scrollColumn w; y=top (bounds w)+1+r-scrollRow w }
                            in if inside (Rect (left (bounds w)+1) (top (bounds w)+1) (width (bounds w)-2) (height (bounds w)-2)) x y then V.Cursor x y else V.NoCursor
        _ -> V.NoCursor

-- A DOS shadow changes the underlying cell attributes, preserving its glyph.
-- Flatten only the layers below the popup, so stacked popups shadow correctly.
castShadow :: (Int,Int) -> Rect -> [V.Image] -> V.Image
castShadow size (Rect x y w h) below =
  place (x+2) (y+1) (V.crop w h (V.translate (negate (x+2)) (negate (y+1)) dimmed))
  where
    dimmed = V.vertCat [V.horizCat (map dim (toList spans)) | spans <- toList (displayOpsForPic (V.picForLayers below) size)]
    dim TextSpan{textSpanText=t} = label shadow (TL.toStrict t)
    dim (Skip n) = V.charFill shadow ' ' n 1
    dim (RowEnd n) = V.charFill shadow ' ' n 1

windowLayers :: Desktop -> Bool -> Window -> [V.Image]
windowLayers d active w =
  [place x (y+1+diagnosticRow issue-scrollRow w) (label (attr (if diagnosticSeverity issue==1 then V.RGBColor 255 85 85 else yellow) blue) "▶")
    | issue<-diagnostics d, Just (diagnosticPath issue)==fmap filePath (documentFile doc), diagnosticRow issue>=scrollRow w, diagnosticRow issue<scrollRow w+hh-2]
  ++ (if active then
    [place (x+2) y (label frame "[" V.<|> label (attr (V.RGBColor 85 255 85) blue) "■" V.<|> label frame "]"),place (x+ww-6) y (label frame "[" V.<|> label (attr cyan blue) "↑" V.<|> label frame "]")
    ,place (x+2) (y+hh-1) (label frame (T.take (max 0 (ww-4)) (windowPositionText doc w)))
    ,place (x+ww-2) (y+hh-1) (label frame "◢")
    ,scrollbarImage True,scrollbarImage False] else [])
  ++ [place (x+ww-7-T.length number) y (label frame number)
  ,place (x+max 6 ((ww-T.length title) `div` 2)) y (label frame (T.take (max 0 (ww-17-T.length number)) title))
  ,place (x+1) (y+1) textImage
  ,place x y (box frame (active && not moving) ww hh)]
  where
    Rect x y ww hh=bounds w
    doc=fromMaybe (newDocument (newBuffer "") Nothing) (M.lookup (bufferId w) (buffers d))
    b=documentBuffer doc; t=contents b
    file=maybe ("NONAME"<>T.pack (show (bufferId w))<>".HS") (T.pack . takeFileName . filePath) (documentFile doc)
    title=" "<>fromMaybe file (documentLabel doc)<>(if dirty b then " * " else " ")
    number=T.pack (show (windowNumber w))
    moving=case drag d of Just (Moving wid _ _) -> wid==windowId w; Just (Resizing wid _ _) -> wid==windowId w; _ -> False
    frame=attr (if moving then cyan else if active then white else gray) blue
    styledLines=splitStyled (if documentLabel doc /= Nothing then [(ch,Plain) | ch<-T.unpack t] else documentHighlight doc)
    scrollbarImage vertical =
      let Rect sx sy bw bh=scrollbarRect vertical doc w
          len=if vertical then bh else bw
          thumb=scrollbarThumb len (scrollbarLimit vertical doc w) (if vertical then scrollRow w else scrollColumn w)
          cell n=V.char (if n==0 || n==len-1 then attr blue scrollCyan else attr scrollCyan blue)
            (if n==0 then if vertical then '▲' else '◄' else if n==len-1 then if vertical then '▼' else '►' else if n==thumb then '█' else '░')
      in place sx sy ((if vertical then V.vertCat else V.horizCat) [cell n | n<-[0..len-1]])
    contentWidth=max 0 (ww-2); contentHeight=max 0 (hh-2)
    textImage=V.vertCat [renderLine n | n<-[scrollRow w..scrollRow w+contentHeight-1]]
    renderLine n=V.cropRight contentWidth (V.translateX (negate (scrollColumn w)) (styledImage (lineColor n) active (selection w) (bufferLineOffset b n) (fromMaybe [] (atMay styledLines n))) V.<|> V.charFill edit ' ' contentWidth 1)

    lineColor n = case documentLabel doc of
      Just "Git diff" -> let line=bufferLineAt b n in Just (attr (if "+" `T.isPrefixOf` line then V.RGBColor 85 255 85 else if "-" `T.isPrefixOf` line then V.RGBColor 255 85 85 else if "@@" `T.isPrefixOf` line then cyan else yellow) blue)
      Just _ -> Just (attr yellow blue)
      Nothing -> Nothing

atMay :: [a] -> Int -> Maybe a
atMay xs n = case drop n xs of a:_ -> Just a; [] -> Nothing

splitStyled :: [(Char,Style)] -> [[(Char,Style)]]
splitStyled []=[[]]
splitStyled xs=let (a,b)=break ((=='\n').fst) xs in a:case b of []->[]; _:rest->splitStyled rest

styledImage :: Maybe V.Attr -> Bool -> Selection -> Int -> [(Char,Style)] -> V.Image
styledImage override active sel start chars = V.horizCat [label a (T.pack (map snd group)) | group@((a,_):_) <- groupBy (\a b -> fst a==fst b) (expand 0 start chars)]
  where
    (lo,hi)=ordered sel
    expand _ _ []=[]
    expand col offset ((c,style):rest)
      | c=='\r' = expand col (offset+1) rest
      | c=='\t' = replicate (8-col `mod` 8) (a,' ') ++ expand (col+8-col `mod` 8) (offset+1) rest
      | c<' ' || c=='\DEL' = (a,'·'):expand (col+1) (offset+1) rest
      | otherwise = (a,c):expand (col+V.safeWcwidth c) (offset+1) rest
      where a=if active && offset>=lo && offset<hi then attr blue gray else fromMaybe (syntaxAttr style) override
    syntaxAttr style=attr (case style of Plain->yellow; Keyword->white; Comment->cyan; Literal->V.RGBColor 85 255 85; Number->V.RGBColor 255 85 255; Constructor->yellow; Pragma->gray) blue

treeLayers :: Desktop -> Sidebar -> [V.Image]
treeLayers d tree = [place 0 1 image]
  where
    w=treeWidth tree; h=max 0 (snd (screenSize d)-2)
    visible=max 0 (h-3)
    listing=take visible (drop (treeScroll tree) (zip [0..] (treeRows tree)))
    title=row paper (w-1) (" Files"<>T.replicate (max 0 (w-10)) " "<>"[×]") V.<|> label paper "│"
    root=row paper (w-1) (T.pack (treeRoot tree)) V.<|> label paper "│"
    line (i,node)=row (if treeFocused tree && i==treeSelected tree then selected else edit) (w-1) (T.replicate (2*nodeDepth node) " "<>(if nodeDirectory node then if nodeExpanded node then "[-] " else "[+] " else "    ")<>nodeName node) V.<|> label paper "│"
    blank=row edit (w-1) "" V.<|> label paper "│"
    image=V.crop w h (V.vertCat ([title,root] ++ map line listing ++ replicate (max 0 (h-2-length listing)) blank))

keyLegend :: Text -> V.Image
keyLegend text = V.horizCat [label (if shortcut token then attr red gray else paper) token | token <- T.groupBy (\a b -> isSpace a == isSpace b) text]
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
      ++ [place (x+w-5) y (label frame "[×]") | problemsFocused d]
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
contextLayers d (Rect x y w h,chosen) =
  [place (x+1) (y+i+1) (row (if i==chosen then selected else paper) (w-2) (" "<>title)) | (i,(title,_))<-zip [0..] (contextItems (contextKind d))]
  ++ [place x y (box paper False w h)]

dialogLayers :: Desktop -> Dialog -> [V.Image]
dialogLayers d dg =
  [place (x+max 1 ((w-T.length title) `div` 2)) y (label (attr white gray) title)]
  ++ [place bx by (V.cropRight bw (buttonImage i name)) | (i,(Rect bx by bw _,name))<-zip [0..] (zip (buttonRects d dg) (buttons dg))]
  ++ [place (bx+1) (by+1) (V.charFill (attr black black) ' ' bw 1) | Rect bx by bw _<-buttonRects d dg]
  ++ concat [fieldLayer i r f | (i,(r,f))<-zip [0..] (zip (fieldRects d dg) (fields dg))]
  ++ [place (x+3) (y+2+i) (row paper (w-6) line) | (i,line)<-zip [0..] (body dg),y+2+i<y+h-3]
  ++ [place x y (box (attr white gray) True w h)]
  where
    Rect x y w h=dialogRect d dg
    title=" "<>dialogTitle dg<>" "
    pushed i=buttonPressed d==Just i && buttonHover d==Just i
    buttonImage i name = label normal (if pushed i then "   " else "  ") V.<|> label normal (T.take pos name)
      V.<|> label (attr white bg) (T.take 1 (T.drop pos name)) V.<|> label normal (T.drop (pos+1) name)
      V.<|> label normal (if pushed i then " " else "  ")
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
        image=case field of
          Input name value p -> let offset=if focus dg==i then max 0 (displayColumn value p-fw+1) else 0
                               in V.vertCat [row paper fw name,V.cropRight fw (V.translateX (negate offset) (label (attr black (V.RGBColor 0 170 170)) value) V.<|> V.charFill (attr black (V.RGBColor 0 170 170)) ' ' fw 1)]
          CheckBox name checked -> row a fw ((if checked then "[X] " else "[ ] ")<>name)
          Radio name values chosen -> V.vertCat (row paper fw name:[row (if focus dg==i && n==chosen then selected else paper) fw ((if n==chosen then "(●) " else "( ) ")<>v) | (n,v)<-zip [0..] values])
          FileList entries chosen ->
            let cw=max 1 ((fw-3) `div` 2); page=(max 0 chosen `div` 16)*16
                item idx=case drop idx entries of
                  e:_ -> row (if idx==chosen then selected else paper) cw (" "<>entryName e<>(if entryDirectory e then "/" else ""))
                  _ -> row paper cw ""
                bar=label paper "┌" V.<|> V.charFill paper '─' cw 1 V.<|> label paper "┬" V.<|> V.charFill paper '─' cw 1 V.<|> label paper "┐"
                line r=label paper "│" V.<|> item (page+r) V.<|> label paper "│" V.<|> item (page+8+r) V.<|> label paper "│"
            in V.vertCat ([row paper fw "Files",bar] ++ [line r | r<-[0..7]] ++ [label paper "└" V.<|> V.charFill paper '─' cw 1 V.<|> label paper "┴" V.<|> V.charFill paper '─' cw 1 V.<|> label paper "┘"])
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
