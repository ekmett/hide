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
import Data.Maybe (fromMaybe)
import System.FilePath (takeFileName)
import THC.Edit.Buffer
import THC.Edit.Model
import THC.Edit.Syntax
import THC.Edit.Files (filePath)

blue, gray, black, white, yellow, cyan, green, red :: V.Color
blue=V.RGBColor 0 0 170; gray=V.RGBColor 170 170 170; black=V.RGBColor 0 0 0
white=V.RGBColor 255 255 255; yellow=V.RGBColor 255 255 85; cyan=V.RGBColor 85 255 255
green=V.RGBColor 0 170 0; red=V.RGBColor 170 0 0
attr :: V.Color -> V.Color -> V.Attr
attr fg bg = V.defAttr `V.withForeColor` fg `V.withBackColor` bg
paper, edit, selected, shadow :: V.Attr
paper=attr black gray; edit=attr yellow blue; selected=attr black green; shadow=attr black black

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
    layers = maybe [] (dialogLayers d) (dialog d) ++ maybe [] (menuLayers d) (menu d)
      ++ [place 0 0 menuBar, place 0 (sh-1) statusBar]
      ++ concat [windowLayers d (i==0) w | (i,w)<-zip [0::Int ..] (windows d)]
      ++ [V.charFill (attr gray blue) '░' sw sh]
    menuBar = V.cropRight sw (V.char paper ' ' V.<|> V.horizCat [V.char paper ' ' V.<|> label (attr red gray) (T.take 1 title) V.<|> label paper (T.drop 1 title<>" ") | (title,_,_)<-menus] V.<|> V.charFill paper ' ' sw 1)
    statusBar = row paper sw (case prefix d of
      Just c -> " Ctrl+"<>T.singleton c<>"-  (Esc cancels)"
      Nothing -> if dialog d/=Nothing then " Tab Next  Enter Select  Esc Cancel"
                 else if not (T.null (status d)) then " F1 Help | "<>status d
                 else " F1 Help  F2 Save  F3 Open  F5 Zoom  F6 Next  F10 Menu")
    cursor = case dialog d of
      Just dg -> case drop (focus dg) (zip (fieldRects d dg) (fields dg)) of
        (Rect x y w _,Input _ value p):_ -> let offset=max 0 (displayColumn value p-w+1)
                                         in V.Cursor (x+displayColumn value p-offset) (y+1)
        _ -> V.NoCursor
      Nothing | menu d/=Nothing -> V.NoCursor
      Nothing -> case (activeWindow d,activeDocument d) of
        (Just w,Just doc) -> let { (r,c)=lineColumn (contents (documentBuffer doc)) (caret (selection w)); x=left (bounds w)+1+displayColumn (lineAt (contents (documentBuffer doc)) r) c-scrollColumn w; y=top (bounds w)+1+r-scrollRow w }
                            in if inside (Rect (left (bounds w)+1) (top (bounds w)+1) (width (bounds w)-2) (height (bounds w)-2)) x y then V.Cursor x y else V.NoCursor
        _ -> V.NoCursor

windowLayers :: Desktop -> Bool -> Window -> [V.Image]
windowLayers d active w =
  [place (x+2) y (label frame "[■]"),place (x+ww-6) y (label frame "[↕]")
  ,place (x+max 6 ((ww-T.length title) `div` 2)) y (label frame (T.take (max 0 (ww-14)) title))
  ,place (x+2) (y+hh-1) (label frame (" "<>T.pack (show (r+1))<>":"<>T.pack (show (c+1))<>" "))
  ,place (x+ww-2) (y+hh-1) (label frame "◢")
  ,place (x+ww-1) (y+1) (V.vertCat [V.char frame (if n==0 then '▲' else if n==hh-3 then '▼' else if n==thumb then '█' else '░') | n<-[0..hh-3]])
  ,place (x+1) (y+1) textImage
  ,place x y (box frame active ww hh)]
  where
    Rect x y ww hh=bounds w
    doc=fromMaybe (Document (newBuffer "") Nothing) (M.lookup (bufferId w) (buffers d))
    b=documentBuffer doc; t=contents b
    file=maybe ("NONAME"<>T.pack (show (bufferId w))<>".HS") (T.pack . takeFileName . filePath) (documentFile doc)
    title=" "<>file<>(if dirty b then " * " else " ")
    frame=attr (if active then white else gray) blue
    (r,c)=lineColumn t (caret (selection w))
    styledLines=splitStyled (highlight t)
    thumb=1+scrollRow w*max 1 (hh-5) `div` max 1 (length styledLines-1)
    contentWidth=max 0 (ww-2); contentHeight=max 0 (hh-2)
    textImage=V.vertCat [renderLine n | n<-[scrollRow w..scrollRow w+contentHeight-1]]
    renderLine n=V.cropRight contentWidth (V.translateX (negate (scrollColumn w)) (styledImage active (selection w) (lineOffset t n) (fromMaybe [] (atMay styledLines n))) V.<|> V.charFill edit ' ' contentWidth 1)

atMay :: [a] -> Int -> Maybe a
atMay xs n = case drop n xs of a:_ -> Just a; [] -> Nothing

splitStyled :: [(Char,Style)] -> [[(Char,Style)]]
splitStyled []=[[]]
splitStyled xs=let (a,b)=break ((=='\n').fst) xs in a:case b of []->[]; _:rest->splitStyled rest

styledImage :: Bool -> Selection -> Int -> [(Char,Style)] -> V.Image
styledImage active sel start chars = V.horizCat [label a (T.pack (map snd group)) | group@((a,_):_) <- groupBy (\a b -> fst a==fst b) (expand 0 start chars)]
  where
    (lo,hi)=ordered sel
    expand _ _ []=[]
    expand col offset ((c,style):rest)
      | c=='\r' = expand col (offset+1) rest
      | c=='\t' = replicate (8-col `mod` 8) (a,' ') ++ expand (col+8-col `mod` 8) (offset+1) rest
      | c<' ' || c=='\DEL' = (a,'·'):expand (col+1) (offset+1) rest
      | otherwise = (a,c):expand (col+V.safeWcwidth c) (offset+1) rest
      where a=if active && offset>=lo && offset<hi then attr blue gray else syntaxAttr style
    syntaxAttr style=attr (case style of Plain->yellow; Keyword->white; Comment->cyan; Literal->V.RGBColor 85 255 85; Number->V.RGBColor 255 85 255; Constructor->yellow; Pragma->gray) blue

menuLayers :: Desktop -> (Int,Int) -> [V.Image]
menuLayers d (i,j) = [place x y contents',place (x+2) (y+1) (V.charFill shadow ' ' w h)]
  where
    Rect x y w h=menuRect d i
    items=menuItems i
    contents'=V.vertCat [border '┌' '┐',V.vertCat (zipWith item [0..] items),border '└' '┘']
    border a b=V.char paper a V.<|> V.charFill paper '─' (max 0 (w-2)) 1 V.<|> V.char paper b
    item n (MenuItem title key cmd)=V.char paper '│' V.<|> row a (w-2) (" "<>title<>T.replicate (max 1 (w-4-T.length title-T.length key)) " "<>key<>" ") V.<|> V.char paper '│'
      where a=case cmd of Disabled _ -> attr (V.RGBColor 85 85 85) gray; _->if n==j then selected else paper

dialogLayers :: Desktop -> Dialog -> [V.Image]
dialogLayers d dg =
  [place (x+max 1 ((w-T.length title) `div` 2)) y (label paper title)]
  ++ [place bx by (row a bw ("[ "<>name<>" ]")) | (i,(Rect bx by bw _,name))<-zip [0..] (zip (buttonRects d dg) (buttons dg)),let a=if focus dg==length (fields dg)+i then attr white green else attr black (V.RGBColor 0 170 170)]
  ++ concat [fieldLayer i r f | (i,(r,f))<-zip [0..] (zip (fieldRects d dg) (fields dg))]
  ++ [place (x+3) (y+2+i) (row paper (w-6) line) | (i,line)<-zip [0..] (body dg),y+2+i<y+h-3]
  ++ [place x y (box paper True w h),place (x+2) (y+1) (V.charFill shadow ' ' w h)]
  where
    Rect x y w h=dialogRect d dg
    title=" "<>dialogTitle dg<>" "
    fieldLayer i (Rect fx fy fw _) field
      | fy>=y+h-3 = []
      | otherwise = [place fx fy (V.cropBottom (max 0 (y+h-3-fy)) image)]
      where
        a=if focus dg==i then selected else paper
        image=case field of
          Input name value p -> let offset=if focus dg==i then max 0 (displayColumn value p-fw+1) else 0
                               in V.vertCat [row paper fw name,V.cropRight fw (V.translateX (negate offset) (label (attr black (V.RGBColor 0 170 170)) value) V.<|> V.charFill (attr black (V.RGBColor 0 170 170)) ' ' fw 1)]
          CheckBox name checked -> row a fw ((if checked then "[X] " else "[ ] ")<>name)
          Radio name values chosen -> V.vertCat (row paper fw name:[row (if focus dg==i && n==chosen then selected else paper) fw ((if n==chosen then "(●) " else "( ) ")<>v) | (n,v)<-zip [0..] values])
          ListBox name values chosen -> V.vertCat (row paper fw name:[row (if n==chosen then a else paper) fw (" "<>v) | (n,v)<-take 4 (drop (max 0 (chosen-3)) (zip [0..] values))])

snapshot :: Desktop -> Text
snapshot d = T.unlines [T.concat (map plain (toList ops)) | ops<-toList (displayOpsForPic (renderDesktop d) (screenSize d))]
  where plain TextSpan{textSpanText=t}=TL.toStrict t
        plain (Skip n)=T.replicate n " "
        plain (RowEnd n)=T.replicate n " "

-- Headless preview uses the actual Vty output spans, not a second UI renderer.
snapshotHtml :: Desktop -> Text
snapshotHtml d = "<!doctype html><meta charset='utf-8'><title>Turbo Haskell</title><style>body{background:#111;margin:24px;display:grid;place-content:center;min-height:90vh}pre{font:20px/1.15 'Courier New',monospace;margin:0;box-shadow:0 0 0 2px #333;white-space:pre}span{font-weight:normal}</style><pre>" <> T.intercalate "\n" rows <> "</pre>"
  where
    rows=[T.concat (map spanHtml (toList ops)) | ops<-toList (displayOpsForPic (renderDesktop d) (screenSize d))]
    spanHtml TextSpan{textSpanAttr=a,textSpanText=t}="<span style='color:"<>color (V.attrForeColor a)<>";background:"<>color (V.attrBackColor a)<>"'>"<>escape (TL.toStrict t)<>"</span>"
    spanHtml (Skip n)=T.replicate n " "
    spanHtml (RowEnd n)=T.replicate n " "
    color (V.SetTo (V.RGBColor r g b))="rgb("<>T.intercalate "," (map (T.pack.show) [r,g,b])<>")"
    color _="#aaa"
    escape=T.concatMap (\c -> case c of '&'->"&amp;"; '<'->"&lt;"; '>'->"&gt;"; _->T.singleton c)
