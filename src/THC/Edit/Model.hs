{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Model where

import qualified Graphics.Vty as V
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (find, findIndex)
import Data.Char (toLower, isPrint)
import Text.Read (readMaybe)
import THC.Edit.Buffer
import THC.Edit.Files (FileState(..))

-- The same rectangles drive drawing and mouse dispatch.
data Rect = Rect { left :: Int, top :: Int, width :: Int, height :: Int } deriving (Eq,Show)
inside :: Rect -> Int -> Int -> Bool
inside (Rect x y w h) a b = a >= x && a < x+w && b >= y && b < y+h

data Document = Document { documentBuffer :: Buffer, documentFile :: Maybe FileState } deriving (Eq,Show)
data Window = Window
  { windowId :: Int, bufferId :: Int, bounds :: Rect, selection :: Selection
  , scrollRow :: Int, scrollColumn :: Int, restoredBounds :: Maybe Rect
  } deriving (Eq,Show)
data Command = New | Open | Save | SaveAs | Close | Quit | Undo | Redo | Cut | Copy | Paste
  | Find | FindNext | Replace | GoTo | SelectAll | Zoom | NextWindow | Cascade | Tile
  | SplitVertical | SplitHorizontal | About | Help | EditorOptions | Gallery
  | Disabled Text deriving (Eq,Show)
data Effect = ReadPath FilePath | SaveDocument Int (Maybe FilePath) (Maybe Command) | Exit deriving (Eq,Show)
data Field = Input Text Text Int | CheckBox Text Bool | Radio Text [Text] Int | ListBox Text [Text] Int deriving (Eq,Show)
data Purpose = Opening | Saving Int (Maybe Command) | Finding | Replacing | GoingTo
  | Confirm Command | Information | Settings | Widgets deriving (Eq,Show)
data Dialog = Dialog
  { dialogTitle :: Text, purpose :: Purpose, fields :: [Field], focus :: Int
  , buttons :: [Text], body :: [Text]
  } deriving (Eq,Show)
data Drag = Moving Int Int Int | Resizing Int Int Int | Selecting Int deriving (Eq,Show)
data Desktop = Desktop
  { screenSize :: (Int,Int), windows :: [Window], buffers :: M.Map Int Document
  , nextId :: Int, menu :: Maybe (Int,Int), dialog :: Maybe Dialog, drag :: Maybe Drag
  , clipboard :: Text, wordStar :: Bool, prefix :: Maybe Char, status :: Text
  , lastFind :: Text
  } deriving (Eq,Show)

data MenuItem = MenuItem Text Text Command deriving (Eq,Show)
menus :: [(Text,Char,[MenuItem])]
menus =
  [("File",'f',[mi "New" "" New, mi "Open..." "F3" Open, mi "Save" "F2" Save, mi "Save as..." "" SaveAs, mi "Close" "Alt+F3" Close, mi "Exit" "Alt+X" Quit])
  ,("Edit",'e',[mi "Undo" "Ctrl+Z" Undo, mi "Redo" "Ctrl+Y" Redo, mi "Cut" "Shift+Del" Cut, mi "Copy" "Ctrl+Ins" Copy, mi "Paste" "Shift+Ins" Paste, mi "Select all" "Ctrl+A" SelectAll])
  ,("Search",'s',[mi "Find..." "Ctrl+F" Find, mi "Replace..." "Ctrl+R" Replace, mi "Search again" "Ctrl+L" FindNext, mi "Go to line..." "Ctrl+G" GoTo])
  ,("Run",'r',[off "Run" "Ctrl+F9" "THC execution is not connected yet."])
  ,("Compile",'c',[off "Compile" "Alt+F9" "THC compilation is not connected yet.",off "Make" "F9" "Cabal project integration is a later milestone."])
  ,("Debug",'d',[off "Inspect type..." "" "HLS is not connected yet.",off "Go to definition" "" "HLS is not connected yet."])
  ,("Tools",'t',[mi "Widget gallery..." "" Gallery,off "Project browser..." "" "Cabal component browsing is not connected yet."])
  ,("Options",'o',[mi "Editor..." "" EditorOptions])
  ,("Window",'w',[mi "Tile" "" Tile,mi "Cascade" "" Cascade,mi "Split vertically" "" SplitVertical,mi "Split horizontally" "" SplitHorizontal,mi "Zoom" "F5" Zoom,mi "Next" "F6" NextWindow,mi "Close" "Alt+F3" Close])
  ,("Help",'h',[mi "Contents" "F1" Help,mi "About Turbo Haskell..." "" About])]
  where mi = MenuItem
        off title key reason = mi title key (Disabled reason)

menuPositions :: [(Int,Int)]
menuPositions = zip starts widths
  where widths = [T.length title+2 | (title,_,_) <- menus]
        starts = scanl (+) 1 widths

menuItems :: Int -> [MenuItem]
menuItems i = let (_,_,xs) = menus !! (i `mod` length menus) in xs
menuRect :: Desktop -> Int -> Rect
menuRect d i = Rect (min x (max 0 (sw-w))) 1 w (length (menuItems i)+2)
  where x = fst (menuPositions !! i)
        sw = fst (screenSize d)
        w = min sw (maximum [T.length t + T.length key + 5 | MenuItem t key _ <- menuItems i])

initialDesktop :: (Int,Int) -> Desktop
initialDesktop size = Desktop size [] M.empty 1 Nothing Nothing Nothing "" False Nothing "640K ought to be enough for any thunk." ""

activeWindow :: Desktop -> Maybe Window
activeWindow = listToMaybe . windows
activeDocument :: Desktop -> Maybe Document
activeDocument d = activeWindow d >>= (\w -> M.lookup (bufferId w) (buffers d))

fitRect :: (Int,Int) -> Rect -> Rect
fitRect (sw,sh) (Rect x y w h) = Rect (max 0 (min x (sw-w'))) (max 1 (min y (sh-1-h'))) w' h'
  where w' = max 1 (min sw (max 16 w)); h' = max 1 (min (max 1 (sh-2)) (max 5 h))

addDocument :: Maybe FileState -> Buffer -> Desktop -> Desktop
addDocument file b d = d { windows = w : windows d, buffers = M.insert i (Document b file) (buffers d), nextId = i+1 }
  where
    i = nextId d
    offset = length (windows d) `mod` 5
    (sw,sh) = screenSize d
    w = Window i i (fitRect (screenSize d) (Rect offset (1+offset) (sw-offset) (sh-2-offset))) (Selection 0 0) 0 0 Nothing

focusWindow :: Int -> Desktop -> Desktop
focusWindow i d = d { windows = filter ((==i) . windowId) (windows d) ++ filter ((/=i) . windowId) (windows d) }

modifyActive :: (Window -> Window) -> Desktop -> Desktop
modifyActive f d = d { windows = case windows d of [] -> []; w:ws -> f w : ws }

ensureVisible :: Desktop -> Desktop
ensureVisible d = case (activeWindow d, activeDocument d) of
  (Just w, Just doc) -> modifyActive (const w { scrollRow = max 0 row', scrollColumn = max 0 col' }) d
    where
      t = contents (documentBuffer doc)
      (row,col) = lineColumn t (caret (selection w))
      dc = displayColumn (lineAt t row) col
      rows = max 1 (height (bounds w)-2); cols = max 1 (width (bounds w)-2)
      row' = if row < scrollRow w then row else if row >= scrollRow w+rows then row-rows+1 else scrollRow w
      col' = if dc < scrollColumn w then dc else if dc >= scrollColumn w+cols then dc-cols+1 else scrollColumn w
  _ -> d

-- Map other view positions through the changed character interval.
editActive :: (Selection -> Buffer -> Buffer) -> Maybe Int -> Desktop -> Desktop
editActive f cursor d = case (activeWindow d, activeDocument d) of
  (Just active, Just doc) -> ensureVisible d { buffers = M.insert bid doc {documentBuffer = changed} (buffers d), windows = map adjust (windows d) }
    where
      bid = bufferId active
      original = documentBuffer doc
      changed = f (selection active) original
      old = contents original; new = contents changed
      common = maybe 0 (T.length . (\(a,_,_) -> a)) (T.commonPrefixes old new)
      suffix = maybe 0 (T.length . (\(a,_,_) -> a)) (T.commonPrefixes (T.reverse (T.drop common old)) (T.reverse (T.drop common new)))
      oldEnd = T.length old-suffix; newEnd = T.length new-suffix
      rebase p | p <= common = p
               | p >= oldEnd = p + newEnd-oldEnd
               | otherwise = newEnd
      adjust w | bufferId w /= bid = w
               | windowId w == windowId active = w {selection = Selection target target}
               | otherwise = w {selection = let Selection a c = selection w in Selection (rebase a) (rebase c)}
      target = max 0 (min (T.length new) (fromMaybe newEnd cursor))
  _ -> d

insertText :: Text -> Desktop -> Desktop
insertText text d = case activeWindow d of
  Nothing -> insertText text (addDocument Nothing (newBuffer "") d)
  Just w -> editActive (\s -> replaceSelection s text) (Just (fst (ordered (selection w)) + T.length text)) d

moveTo :: Bool -> Int -> Desktop -> Desktop
moveTo extend pos d = ensureVisible (modifyActive update d)
  where
    len = maybe 0 (T.length . contents . documentBuffer) (activeDocument d)
    p = max 0 (min len pos)
    update w = w {selection = Selection (if extend then anchor (selection w) else p) p}

message :: Text -> [Text] -> Desktop -> Desktop
message title lines' d = d {dialog = Just (Dialog title Information [] 0 ["OK"] lines'), menu = Nothing, drag = Nothing}

prompt :: Text -> Purpose -> [Field] -> Desktop -> Desktop
prompt title p fs d = d {dialog = Just (Dialog title p fs 0 ["OK","Cancel"] []), menu = Nothing, drag = Nothing}

runCommand :: Command -> Desktop -> (Desktop,[Effect])
runCommand cmd source = go cmd (source {menu = Nothing, prefix = Nothing, drag = Nothing})
  where
    go New d = (addDocument Nothing (newBuffer "") d,[])
    go Open d = (prompt "Open" Opening [Input "Name" "" 0] d,[])
    go Save d = saveRequest Nothing d
    go SaveAs d = case activeWindow d of
      Nothing -> (d,[])
      Just w -> (prompt "Save file as" (Saving (bufferId w) Nothing) [Input "Name" (currentPath d) (T.length (currentPath d))] d,[])
    go Quit d = case find (dirty . documentBuffer . snd) (M.toList (buffers d)) of
      Nothing -> (d,[Exit])
      Just (bid,_) -> let focused = maybe d (\w -> focusWindow (windowId w) d) (find ((==bid) . bufferId) (windows d))
                     in confirm Quit focused
    go Close d = case (activeWindow d, activeDocument d) of
      (Just w, Just doc) | dirty (documentBuffer doc) && length (filter ((==bufferId w) . bufferId) (windows d)) == 1 -> confirm Close d
      _ -> (closeActive d,[])
    go Undo d = (editActive (const undo) Nothing d,[])
    go Redo d = (editActive (const redo) Nothing d,[])
    go Copy d = (d {clipboard = selected d, status = "Block copied."},[])
    go Cut d = (insertText "" d {clipboard = selected d},[])
    go Paste d = (insertText (clipboard d) d,[])
    go SelectAll d = (modifyActive (\w -> w {selection = Selection 0 (T.length (activeText d))}) d,[])
    go Find d = (prompt "Find" Finding [Input "Text to find" (lastFind d) (T.length (lastFind d))] d,[])
    go Replace d = (prompt "Replace" Replacing [Input "Text to find" (lastFind d) (T.length (lastFind d)),Input "Replace with" "" 0] d,[])
    go FindNext d = (findText (lastFind d) d,[])
    go GoTo d = (prompt "Go to line" GoingTo [Input "Line number" "1" 1] d,[])
    go Zoom d = (modifyActive zoom d,[]) where
      zoom w = case restoredBounds w of
        Just r -> w {bounds = fitRect (screenSize d) r, restoredBounds = Nothing}
        Nothing -> w {bounds = let (sw,sh) = screenSize d in Rect 0 1 sw (sh-2), restoredBounds = Just (bounds w)}
    go NextWindow d = (d {windows = case windows d of [] -> []; w:ws -> ws++[w]},[])
    go Cascade d = (d {windows = zipWith cascade [0..] (windows d)},[]) where
      (sw,sh) = screenSize d
      cascade i w = w {bounds = fitRect (screenSize d) (Rect (i `mod` 6) (1+i `mod` 6) (sw-6) (sh-8)), restoredBounds = Nothing}
    go Tile d = (tileWindows False d,[])
    go SplitVertical d = splitWindow True d
    go SplitHorizontal d = splitWindow False d
    go About d = (message "About Turbo Haskell" ["Turbo Haskell  0.1", "Copyright (c) 2026 Edward Kmett", "", "Now with fewer assignment statements.", "Destination: Weak Head Normal Form", "", "An affectionate Borland-era homage.", "Written in Haskell. Naturally."] d,[])
    go Help d = (prompt "Turbo Help" Information [ListBox "Keyboard reference" helpLines 0] d,[])
    go EditorOptions d = (prompt "Editor options" Settings [Radio "Key bindings" ["Modern","WordStar"] (if wordStar d then 1 else 0)] d,[])
    go Gallery d = (prompt "Turbo widget laboratory" Widgets [Input "Unit name" "Prelude" 7,CheckBox "Enable excessive laziness" True,Radio "Evaluation" ["Normal order","Weak head normal form"] 1,ListBox "Installed abstractions" ["Functor","Applicative","Monad","Comonad","A monad is a monoid..."] 0] d,[])
    go (Disabled reason) d = (d {status = reason},[])
    confirm action d = (d {dialog = Just (Dialog "Save changes?" (Confirm action) [] 0 ["Save","Discard","Cancel"] ["Save changes to " <> documentTitle d <> "?"])},[])
    selected d = case (activeWindow d,activeDocument d) of (Just w,Just doc) -> selectedText (selection w) (documentBuffer doc); _ -> ""

activeText :: Desktop -> Text
activeText = maybe "" (contents . documentBuffer) . activeDocument
currentPath :: Desktop -> Text
currentPath d = maybe "" (T.pack . filePath) (activeDocument d >>= documentFile)
documentTitle :: Desktop -> Text
documentTitle d = if T.null (currentPath d) then "NONAME.HS" else currentPath d

saveRequest :: Maybe Command -> Desktop -> (Desktop,[Effect])
saveRequest after d = case (activeWindow d,activeDocument d) of
  (Just w,Just doc) -> case documentFile doc of
    Nothing -> (prompt "Save file as" (Saving (bufferId w) after) [Input "Name" "" 0] d,[])
    Just _ -> (d,[SaveDocument (bufferId w) Nothing after])
  _ -> (d,[])

closeActive :: Desktop -> Desktop
closeActive d = case windows d of
  [] -> d
  w:ws -> d {windows = ws, buffers = if any ((==bufferId w) . bufferId) ws then buffers d else M.delete (bufferId w) (buffers d)}

tileWindows :: Bool -> Desktop -> Desktop
tileWindows vertical d = d {windows = zipWith place [0..] (windows d)}
  where
    n = max 1 (length (windows d)); (sw,sh) = screenSize d; extent = if vertical then sw else sh-2
    place i w = w {bounds = if vertical then Rect start 1 size (sh-2) else Rect 0 (1+start) sw size, restoredBounds = Nothing}
      where start = i*extent `div` n; size = (i+1)*extent `div` n-start

splitWindow :: Bool -> Desktop -> (Desktop,[Effect])
splitWindow vertical d = case activeWindow d of
  Nothing -> (d,[])
  Just w -> (tileWindows vertical d {windows = w {windowId = nextId d} : windows d, nextId = nextId d+1},[])

findText :: Text -> Desktop -> Desktop
findText needle d | T.null needle = d {status = "Enter search text first."}
findText needle d = case activeWindow d of
  Nothing -> d
  Just w -> case searchFrom (snd (ordered (selection w))) of
    Nothing -> d {lastFind = needle,status = "Search text not found."}
    Just p -> ensureVisible (modifyActive (\v -> v {selection = Selection p (p+T.length needle)}) d {lastFind = needle,status = "Search match."})
  where
    t = activeText d
    locate start source = let (before,after) = T.breakOn needle source in if T.null after then Nothing else Just (start+T.length before)
    searchFrom p = case locate p (T.drop p t) of Just x -> Just x; Nothing -> locate 0 (T.take p t)

helpLines :: [Text]
helpLines = ["F1 Help   F2 Save   F3 Open   F5 Zoom", "F6 Next window   F10 Menu   Alt+X Exit", "Alt+F3 Close   Ctrl+Z Undo   Ctrl+Y Redo", "Shift+arrows Select   Ctrl+arrows Words", "Ctrl+C/X/V Copy/Cut/Paste (internal clipboard)", "Ctrl+F Find   Ctrl+R Replace   Ctrl+L Next", "Ctrl+G Go to line   Ctrl+A Select all", "Mouse: title drag, bottom-right resize", "Window menu: tile, cascade, shared splits", "", "WordStar (Options > Editor):", "Ctrl+E/S/D/X Up/Left/Right/Down", "Ctrl+A/F Word left/right   Ctrl+Y Delete line", "Ctrl+K B/K Block start/end   C/V Copy/Move", "Ctrl+K Y Delete block   S Save   D Close", "Ctrl+Q S/D Line start/end   R/C File top/end", "Ctrl+Q F Find   Ctrl+Q A Replace", "Escape cancels a command prefix.", "", "HLS and Cabal browsing are not connected yet."]

fieldHeight :: Field -> Int
fieldHeight Input{} = 3
fieldHeight CheckBox{} = 2
fieldHeight (Radio _ xs _) = length xs+2
fieldHeight ListBox{} = 6

dialogRect :: Desktop -> Dialog -> Rect
dialogRect d dg = Rect ((sw-w) `div` 2) (max 1 ((sh-h) `div` 2)) w h
  where
    (sw,sh) = screenSize d
    w = min sw 62
    h = min (sh-2) (max 7 (5+length (body dg)+sum (map fieldHeight (fields dg))))

fieldRects :: Desktop -> Dialog -> [Rect]
fieldRects d dg = zipWith make starts (fields dg)
  where
    Rect x y w _ = dialogRect d dg
    starts = scanl (+) (y+2+length (body dg)) (map fieldHeight (fields dg))
    make row f = Rect (x+3) row (max 1 (w-6)) (fieldHeight f)

buttonRects :: Desktop -> Dialog -> [Rect]
buttonRects d dg = zipWith (\bx label -> Rect bx (y+h-2) (T.length label+4) 1) starts (buttons dg)
  where
    Rect x y w h = dialogRect d dg
    widths = map ((+4) . T.length) (buttons dg)
    total = sum widths+2*(length widths-1)
    starts = scanl (\a b -> a+b+2) (x+max 1 ((w-total) `div` 2)) widths

handleEvent :: V.Event -> Desktop -> (Desktop,[Effect])
handleEvent (V.EvResize sw sh) d = (d {screenSize = (sw,sh),windows = map (\w -> w {bounds = fitRect (sw,sh) (bounds w)}) (windows d),drag = Nothing,menu = Nothing},[])
handleEvent ev d | Just dg <- dialog d = dialogEvent ev dg d
handleEvent ev d | Just m <- menu d = menuEvent ev m d
handleEvent (V.EvMouseUp _ _ _) d = (d {drag = Nothing},[])
handleEvent (V.EvMouseDown x y button mods) d = mouseEvent x y button mods d
handleEvent (V.EvPaste bytes) d = case TE.decodeUtf8' bytes of
  Left _ -> (message "Paste failed" ["The pasted text is not valid UTF-8."] d,[])
  Right t -> (insertText (T.filter (\c -> isPrint c || c `elem` ['\n','\r','\t']) t) d,[])
handleEvent (V.EvKey key mods) d = keyEvent key mods d
handleEvent _ d = (d,[])

menuEvent :: V.Event -> (Int,Int) -> Desktop -> (Desktop,[Effect])
menuEvent ev (i,j) d = case ev of
  V.EvKey V.KEsc _ -> (d {menu = Nothing},[])
  V.EvKey V.KLeft _ -> choose (i-1) 0
  V.EvKey V.KRight _ -> choose (i+1) 0
  V.EvKey V.KUp _ -> choose i (j-1)
  V.EvKey V.KDown _ -> choose i (j+1)
  V.EvKey V.KEnter _ -> invoke j
  V.EvKey (V.KChar c) _ -> case findIndex (\(MenuItem t _ _) -> toLower c == toLower (T.head t)) (menuItems i) of
    Just k -> invoke k
    _ -> (d,[])
  V.EvMouseDown x 0 V.BLeft _ -> case menuAt x of Just k -> choose k 0; _ -> (d {menu=Nothing},[])
  V.EvMouseDown x y V.BLeft _ -> let r = menuRect d i in if inside r x y && y>top r && y<top r+height r-1 then invoke (y-top r-1) else (d {menu=Nothing},[])
  _ -> (d,[])
  where
    choose a b = let a' = a `mod` length menus in (d {menu = Just (a',b `mod` length (menuItems a'))},[])
    invoke k = let MenuItem _ _ command = menuItems i !! k in runCommand command d

menuAt :: Int -> Maybe Int
menuAt x = findIndex (\(start,w) -> x >= start && x < start+w) menuPositions

mouseEvent :: Int -> Int -> V.Button -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
mouseEvent x y V.BLeft _ d | Just capture <- drag d = (case capture of
  Moving i dx dy -> mapWindow i (\w -> w {bounds = fitRect (screenSize d) (bounds w) {left=x-dx,top=y-dy},restoredBounds=Nothing}) d
  Resizing i dx dy -> mapWindow i (\w -> w {bounds = fitRect (screenSize d) (bounds w) {width=x-left (bounds w)+dx,height=y-top (bounds w)+dy},restoredBounds=Nothing}) d
  Selecting i -> selectAt True x y (focusWindow i d),[])
mouseEvent x 0 V.BLeft _ d = (d {menu = (\i -> (i,0)) <$> menuAt x},[])
mouseEvent x y button mods d = case find (\w -> inside (bounds w) x y) (windows d) of
  Nothing -> (d,[])
  Just w -> let focused = focusWindow (windowId w) d; Rect l t ww hh = bounds w in case button of
    V.BScrollUp -> (modifyActive (\v -> v {scrollRow=max 0 (scrollRow v-3)}) focused,[])
    V.BScrollDown -> (modifyActive (\v -> v {scrollRow=min (max 0 (length (textLines (activeText focused))-1)) (scrollRow v+3)}) focused,[])
    V.BLeft
      | y==t && x>=l+2 && x<=l+4 -> runCommand Close focused
      | y==t && x>=l+ww-5 -> runCommand Zoom focused
      | y==t -> (focused {drag=Just (Moving (windowId w) (x-l) (y-t))},[])
      | x==l+ww-1 && y==t+hh-1 -> (focused {drag=Just (Resizing (windowId w) 1 1)},[])
      | x==l+ww-1 -> let { total = max 0 (length (textLines (activeText focused))-1); row = max 0 (min total ((y-t-1)*total `div` max 1 (hh-3))) }
                    in (modifyActive (\v -> v {scrollRow=row}) focused,[])
      | y==t+hh-1 -> (modifyActive (\v -> v {scrollColumn=max 0 ((x-l-1)*8)}) focused,[])
      | otherwise -> (selectAt (V.MShift `elem` mods) x y focused {drag=Just (Selecting (windowId w))},[])
    _ -> (focused,[])

mapWindow :: Int -> (Window -> Window) -> Desktop -> Desktop
mapWindow i f d = d {windows = map (\w -> if windowId w==i then f w else w) (windows d)}

selectAt :: Bool -> Int -> Int -> Desktop -> Desktop
selectAt extend x y d = case activeWindow d of
  Nothing -> d
  Just w -> moveTo extend pos d where
    t = activeText d
    row = max 0 (min (length (textLines t)-1) (y-top (bounds w)-1+scrollRow w))
    col = max 0 (x-left (bounds w)-1+scrollColumn w)
    pos = lineOffset t row + columnOffset (lineAt t row) col

keyEvent :: V.Key -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
keyEvent key mods d
  | key==V.KEsc = (d {prefix=Nothing},[])
  | Just p <- prefix d, V.KChar c <- key = starPrefix p (toLower c) d {prefix=Nothing}
  | V.MAlt `elem` mods, V.KChar c <- key, Just i <- findIndex (\(_,mn,_) -> mn==toLower c) menus = (d {menu=Just (i,0)},[])
  | V.MAlt `elem` mods, key==V.KChar 'x' = runCommand Quit d
  | V.MAlt `elem` mods, key==V.KFun 3 = runCommand Close d
  | key==V.KFun 1 = runCommand Help d
  | key==V.KFun 2 = runCommand Save d
  | key==V.KFun 3 = runCommand Open d
  | key==V.KFun 5 = runCommand Zoom d
  | key==V.KFun 6 = runCommand NextWindow d
  | key==V.KFun 10 = (d {menu=Just (0,0)},[])
  | key==V.KIns && V.MCtrl `elem` mods = runCommand Copy d
  | key==V.KIns && V.MShift `elem` mods = runCommand Paste d
  | key==V.KDel && V.MShift `elem` mods = runCommand Cut d
  | ctrl, wordStar d, V.KChar c <- key = starKey (toLower c) d
  | ctrl, V.KChar c <- key, Just cmd <- lookup (toLower c) [('s',Save),('o',Open),('n',New),('z',Undo),('y',Redo),('c',Copy),('x',Cut),('v',Paste),('a',SelectAll),('f',Find),('r',Replace),('g',GoTo),('l',FindNext),('q',Quit)] = runCommand cmd d
  | otherwise = (editorKey key mods d,[])
  where ctrl = V.MCtrl `elem` mods

editorKey :: V.Key -> [V.Modifier] -> Desktop -> Desktop
editorKey key mods d = case key of
  V.KLeft -> move (if ctrl then wordLeft t p else previousCharacter t p)
  V.KRight -> move (if ctrl then wordRight t p else nextCharacter t p)
  V.KUp -> vertical (-1)
  V.KDown -> vertical 1
  V.KPageUp -> vertical (negate page)
  V.KPageDown -> vertical page
  V.KHome -> move (if ctrl then 0 else start)
  V.KEnd -> move (if ctrl then T.length t else start+T.length (lineAt t row))
  V.KBS -> erase (if ctrl then wordLeft t p else previousCharacter t p) p
  V.KDel -> erase p (if ctrl then wordRight t p else nextCharacter t p)
  V.KEnter -> insertText (if "\r\n" `T.isInfixOf` t then "\r\n" else "\n") d
  V.KChar '\t' -> insertText "  " d
  V.KChar c | null mods || mods==[V.MShift], isPrint c -> insertText (T.singleton c) d
  _ -> d
  where
    t = activeText d
    sel = maybe (Selection 0 0) selection (activeWindow d); p = caret sel
    (row,col) = lineColumn t p; start = lineOffset t row
    ctrl = V.MCtrl `elem` mods; shift = V.MShift `elem` mods
    page = maybe 10 (\w -> max 1 (height (bounds w)-3)) (activeWindow d)
    move = (\q -> moveTo shift q d)
    vertical delta = let r = max 0 (min (length (textLines t)-1) (row+delta))
                    in move (lineOffset t r+columnOffset (lineAt t r) (displayColumn (lineAt t row) col))
    erase a z = let s = if anchor sel/=caret sel then sel else Selection a z
                in editActive (\_ -> replaceSelection s "") (Just (fst (ordered s))) d

starKey :: Char -> Desktop -> (Desktop,[Effect])
starKey c d = case lookup c [('e',V.KUp),('s',V.KLeft),('d',V.KRight),('x',V.KDown)] of
  Just k -> (editorKey k [] d,[])
  Nothing -> case c of
    'k' -> (d {prefix=Just 'k'},[])
    'q' -> (d {prefix=Just 'q'},[])
    'a' -> (editorKey V.KLeft [V.MCtrl] d,[])
    'f' -> (editorKey V.KRight [V.MCtrl] d,[])
    'y' -> let t=activeText d; p=maybe 0 (caret . selection) (activeWindow d); row=fst (lineColumn t p); a=lineOffset t row; z=min (T.length t) (lineOffset t (row+1))
           in (editActive (\_ -> replaceSelection (Selection a z) "") (Just a) d,[])
    'z' -> runCommand Undo d
    _ -> (d,[])

starPrefix :: Char -> Char -> Desktop -> (Desktop,[Effect])
starPrefix 'k' c d = case c of
  'b' -> (modifyActive (\w -> w {selection=Selection (caret (selection w)) (caret (selection w))}) d,[])
  'k' -> (d,[])
  'c' -> runCommand Copy d
  'v' -> runCommand Cut d
  'y' -> (insertText "" d,[])
  's' -> runCommand Save d
  'd' -> runCommand Close d
  _ -> (d {status="Unknown Ctrl+K command."},[])
starPrefix 'q' c d = case c of
  's' -> (editorKey V.KHome [] d,[])
  'd' -> (editorKey V.KEnd [] d,[])
  'r' -> (moveTo False 0 d,[])
  'c' -> (moveTo False (T.length (activeText d)) d,[])
  'f' -> runCommand Find d
  'a' -> runCommand Replace d
  _ -> (d {status="Unknown Ctrl+Q command."},[])
starPrefix _ _ d = (d,[])

dialogEvent :: V.Event -> Dialog -> Desktop -> (Desktop,[Effect])
dialogEvent ev dg d = case ev of
  V.EvKey V.KEsc _ -> (d {dialog=Nothing},[])
  V.EvKey (V.KChar '\t') mods -> setFocus (focus dg + if V.MShift `elem` mods then -1 else 1)
  V.EvKey V.KBackTab _ -> setFocus (focus dg-1)
  V.EvKey V.KEnter _ -> submitDialog (if focus dg>=count then focus dg-count else 0) dg d
  V.EvKey k mods | focus dg<count -> updateField (fieldKey k mods)
  V.EvKey (V.KChar ' ') _ -> submitDialog (focus dg-count) dg d
  V.EvKey V.KLeft _ -> setFocus (focus dg-1)
  V.EvKey V.KRight _ -> setFocus (focus dg+1)
  V.EvPaste bytes | focus dg<count -> case TE.decodeUtf8' bytes of
    Right text -> updateField (\f -> case f of Input label value pos -> let clean=T.filter isPrint text in Input label (T.take pos value<>clean<>T.drop pos value) (pos+T.length clean); _ -> f)
    Left _ -> (d,[])
  V.EvMouseDown x y V.BLeft _ -> case findIndex (\r -> inside r x y) (buttonRects d dg) of
    Just i -> submitDialog i dg d
    Nothing -> case findIndex (\r -> inside r x y && y < top (dialogRect d dg)+height (dialogRect d dg)-3) (fieldRects d dg) of
      Nothing -> (d,[])
      Just i -> let Rect l t _ _ = fieldRects d dg !! i
                    click (Input label value _) = Input label value (columnOffset value (max 0 (x-l)))
                    click (CheckBox label b) = CheckBox label (not b)
                    click (Radio label xs _) = Radio label xs (max 0 (min (length xs-1) (y-t-1)))
                    click (ListBox label xs selected) = ListBox label xs (max 0 (min (length xs-1) (max 0 (selected-3)+y-t-1)))
                in updateDialog dg {focus=i,fields=replaceAt i (click (fields dg !! i)) (fields dg)}
  V.EvMouseDown _ _ V.BScrollDown _ -> updateField (fieldKey V.KDown [])
  V.EvMouseDown _ _ V.BScrollUp _ -> updateField (fieldKey V.KUp [])
  _ -> (d,[])
  where
    count=length (fields dg)
    setFocus i = updateDialog dg {focus=i `mod` (count+length (buttons dg))}
    updateDialog new = (d {dialog=Just new},[])
    updateField f | focus dg<count = updateDialog dg {fields=replaceAt (focus dg) (f (fields dg !! focus dg)) (fields dg)}
                  | otherwise = (d,[])

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i+1) xs

fieldKey :: V.Key -> [V.Modifier] -> Field -> Field
fieldKey key mods field = case field of
  Input label value pos -> let set s p = Input label s (max 0 (min (T.length s) p)) in case key of
    V.KLeft -> set value (previousCharacter value pos)
    V.KRight -> set value (nextCharacter value pos)
    V.KHome -> set value 0
    V.KEnd -> set value (T.length value)
    V.KBS -> let p=previousCharacter value pos in set (T.take p value<>T.drop pos value) p
    V.KDel -> set (T.take pos value<>T.drop (nextCharacter value pos) value) pos
    V.KChar 'u' | V.MCtrl `elem` mods -> set "" 0
    V.KChar c | (null mods || mods==[V.MShift]) && isPrint c -> set (T.take pos value<>T.singleton c<>T.drop pos value) (pos+1)
    _ -> field
  CheckBox label value | key==V.KChar ' ' -> CheckBox label (not value)
  Radio label values chosen -> Radio label values (choose values chosen)
  ListBox label values chosen -> ListBox label values (choose values chosen)
  _ -> field
  where choose xs n = max 0 (min (length xs-1) (n + case key of V.KUp -> -1; V.KDown -> 1; V.KLeft -> -1; V.KRight -> 1; V.KChar ' ' -> 1; _ -> 0))

submitDialog :: Int -> Dialog -> Desktop -> (Desktop,[Effect])
submitDialog button dg original
  | button<0 || button>=length (buttons dg) = (original,[])
  | buttons dg !! button == "Cancel" = (d,[])
  | otherwise = case purpose dg of
    Opening -> if T.null first then (original,[]) else (d,[ReadPath (T.unpack first)])
    Saving bid after -> if T.null first then (original,[]) else (d,[SaveDocument bid (Just (T.unpack first)) after])
    Finding -> (findText first d,[])
    Replacing -> let found = findText first d
                 in if T.null first || status found=="Search text not found." then (found,[])
                    else (insertText second found,[])
    GoingTo -> case readMaybe (T.unpack first) of
      Just n | n>0 -> (moveTo False (lineOffset (activeText d) (n-1)) d,[])
      _ -> (original {status="Enter a positive line number."},[])
    Confirm cmd | button==0 -> saveRequest (Just cmd) d
                | button==1 -> runCommand cmd (markClean d)
                | otherwise -> (d,[])
    Settings -> (d {wordStar=any (\f -> case f of Radio _ _ 1 -> True; _ -> False) (fields dg),status="Editor options updated."},[])
    Widgets -> (d {status="Widget test complete. Laziness remains enabled."},[])
    Information -> (d,[])
  where
    d=original {dialog=Nothing}
    values=[value | Input _ value _ <- fields dg]
    first=fromMaybe "" (listToMaybe values); second=fromMaybe "" (listToMaybe (drop 1 values))
    markClean s = case activeWindow s of
      Nothing -> s
      Just w -> s {buffers=M.adjust (\doc -> doc {documentBuffer=(documentBuffer doc) {saved=contents (documentBuffer doc)}}) (bufferId w) (buffers s)}
