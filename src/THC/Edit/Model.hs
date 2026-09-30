{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Model where

import qualified Graphics.Vty as V
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as M
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (find, findIndex, sortOn, mapAccumL)
import Data.Char (toLower, isPrint, isAlphaNum, chr, ord, toUpper)
import Text.Read (readMaybe)
import System.FilePath ((</>), takeDirectory)
import THC.Edit.Browser (Entry(..))
import THC.Edit.Git (GitReview)
import THC.Edit.Syntax (Style, highlightFor)
import THC.Edit.Buffer
import THC.Edit.Files (FileState(..))

-- The same rectangles drive drawing and mouse dispatch.
data Rect = Rect { left :: Int, top :: Int, width :: Int, height :: Int } deriving (Eq,Show)
inside :: Rect -> Int -> Int -> Bool
inside (Rect x y w h) a b = a >= x && a < x+w && b >= y && b < y+h

data Document = Document { documentBuffer :: Buffer, documentFile :: Maybe FileState, documentLabel :: Maybe Text, documentHighlight :: [(Char,Style)], documentWidth :: Int, documentCursorVisible :: Bool } deriving (Eq,Show)
-- Shared by split views; cursor movement and repaint reuse the lazy token cache.
newDocument :: Buffer -> Maybe FileState -> Document
newDocument b file = restyle (Document b file Nothing [] 0 True)

-- ponytail: retokenize the buffer after edits; use an incremental engine if large-file latency warrants it.
restyle :: Document -> Document
restyle doc = doc {documentHighlight=highlightFor (maybe "Main.hs" filePath (documentFile doc)) text,
  documentWidth=maximum (0:[displayColumn line (T.length line) | raw<-textLines text,let line=T.dropWhileEnd (=='\r') raw])}
  where text=contents (documentBuffer doc)

data Window = Window
  { windowId :: Int, bufferId :: Int, bounds :: Rect, selection :: Selection
  , scrollRow :: Int, scrollColumn :: Int, restoredBounds :: Maybe Rect
  , windowNumber :: Int
  } deriving (Eq,Show)
data Command = New | Open | Save | SaveAs | Close | Quit | Undo | Redo | Cut | Copy | Paste
  | Find | FindNext | Replace | GoTo | SelectAll | Zoom | NextWindow | Cascade | Tile
  | SplitVertical | SplitHorizontal | About | Help | EditorOptions | Gallery
  | InspectType | Definition | Complete | Problems | NextMessage | PreviousMessage | RestartHLS | RenameSymbol
  | ToggleTree | GitDiff | GitCommit | GitFetch | GitPull | GitMerge | ReviewDisk
  | RunTarget | RunOptions | OpenTerminal | StopTerminal
  | AgentOptions | Conversation | AgentPrompt | AgentCancel | AgentResume | AgentCopyRaw | AgentNew
  | Disabled Text deriving (Eq,Show)
data ConflictAction = CompareDisk | ReloadDisk | KeepBuffer | SaveConflictAs deriving (Eq,Show)
data Conflict = Conflict { conflictBuffer :: Int, conflictRevision :: Int, conflictBaseline :: FileState, conflictDisk :: Maybe ByteString } deriving (Eq,Show)
data GitAction = FetchRemote | PullRemote | MergeBranch Text deriving (Eq,Show)
data ContextKind = SourceContext | GitContext deriving (Eq,Show)
data LanguageAction = TypeInfo | FindDefinition | Completions | ShowProblems | RestartLanguage | RenameAt Text deriving (Eq,Show)
data Completion = Completion Text [(Int,Int,Text)] deriving (Eq,Show)
data Effect = LanguageRequest LanguageAction | RunGit GitAction | ReadMergeBranches | JumpTo FilePath Int Int | ReadPath FilePath | BrowsePath FilePath Text | OpenChoice FilePath Text Text | ReadTree FilePath | ExpandTree Int | ReadHelp | RefreshGit FilePath | ReadGitDiff | AskGitCommit | WriteGitCommit Text | SaveDocument Int (Maybe FilePath) (Maybe Command) | ReviewExternal | ResolveConflict Conflict ConflictAction | AgentAction Text [Text] | SetScreenMode Int | Exit deriving (Eq,Show)
data Field = Input Text Text Int | CheckBox Text Bool | Radio Text [Text] Int | ListBox Text [Text] Int | FileList [Entry] Int deriving (Eq,Show)
data Purpose = Opening FilePath Text [Entry] | Committing | Saving Int (Maybe Command) | Finding | Replacing | GoingTo | Renaming
  | Completing Int Int Int [Completion] | Locations [(FilePath,Int,Int)] | Merging [Text]
  | DiskConflict Conflict | AgentDialog Text
  | Confirm Command | Information | Settings | Widgets deriving (Eq,Show)
data Dialog = Dialog
  { dialogTitle :: Text, purpose :: Purpose, fields :: [Field], focus :: Int
  , buttons :: [Text], body :: [Text]
  } deriving (Eq,Show)
data TreeRow = TreeRow { nodeName :: Text, nodePath :: FilePath, nodeDepth :: Int, nodeDirectory :: Bool, nodeExpanded :: Bool } deriving (Eq,Show)
data Diagnostic = Diagnostic
  { diagnosticPath :: FilePath, diagnosticVersion :: Maybe Int, diagnosticRow :: Int
  , diagnosticColumn :: Int, diagnosticSeverity :: Int, diagnosticMessage :: Text
  } deriving (Eq,Show)
data Sidebar = Sidebar { treeRoot :: FilePath, treeRows :: [TreeRow], treeSelected :: Int, treeScroll :: Int, treeWidth :: Int, treeFocused :: Bool } deriving (Eq,Show)
data Drag = DockSizing | Moving Int Int Int | Resizing Int Int Int | Selecting Int | Scrolling Int Bool deriving (Eq,Show)
data Desktop = Desktop
  { screenSize :: (Int,Int), windows :: [Window], buffers :: M.Map Int Document
  , nextId :: Int, menu :: Maybe (Int,Int), dialog :: Maybe Dialog, drag :: Maybe Drag
  , clipboard :: Text, wordStar :: Bool, prefix :: Maybe Char, status :: Text
  , blockStart :: Maybe (Int,Int), lastFind :: Text, sideTree :: Maybe Sidebar, branchStatus :: Text, nativeMac :: Bool, gitReview :: Maybe GitReview, videoMode :: Maybe Int
  , hoverTarget :: Maybe (Int,Int,Int), typeHint :: Text
  , buttonHover :: Maybe Int, buttonPressed :: Maybe Int, contextMenu :: Maybe (Rect,Int)
  , diagnostics :: [Diagnostic], problemsVisible :: Bool, problemsSelected :: Int, problemsScroll :: Int, problemsFocused :: Bool
  , dragOriginal :: Maybe (Int,Rect,Maybe Rect)
  , branchAdded :: Int, branchDeleted :: Int, branchRoot :: Maybe FilePath, contextKind :: ContextKind
  , messagesNumber :: Maybe Int
  , blinkCursor :: Bool
  } deriving (Eq,Show)

data MenuItem = MenuItem Text Text Command deriving (Eq,Show)
menus :: [(Text,Char,[MenuItem])]
menus =
  [("File",'f',[mi "New" "" New, mi "Open..." "F3" Open, mi "Save" "F2" Save, mi "Save as..." "" SaveAs, mi "Disk changes..." "" ReviewDisk, mi "Close" "Alt+F3" Close, mi "Exit" "Alt+X" Quit])
  ,("Edit",'e',[mi "Undo" "Ctrl+Z" Undo, mi "Redo" "Ctrl+Y" Redo, mi "Cut" "Shift+Del" Cut, mi "Copy" "Ctrl+Ins" Copy, mi "Paste" "Shift+Ins" Paste, mi "Select all" "Ctrl+A" SelectAll,mi "Complete identifier..." "Ctrl+Space" Complete])
  ,("Search",'s',[mi "Find..." "Ctrl+F" Find, mi "Replace..." "Ctrl+R" Replace, mi "Search again" "Ctrl+L" FindNext, mi "Go to line..." "Ctrl+G" GoTo,mi "Go to definition" "F12" Definition])
  ,("Run",'r',[mi "Run" "Ctrl+F9" RunTarget,mi "Target..." "" RunOptions,mi "Terminal" "" OpenTerminal,mi "Stop terminal" "" StopTerminal])
  ,("Compile",'c',[off "Compile" "Alt+F9" "THC compilation is not connected yet.",off "Make" "F9" "Cabal project integration is a later milestone."])
  ,("Debug",'d',[off "Inspect..." "" "THC Truffle debugging is not connected yet."])
  ,("Tools",'t',[mi "File tree" "Ctrl+B" ToggleTree,mi "Git diff..." "" GitDiff,mi "Approve changes..." "" GitCommit,mi "Inspect type" "Shift+F1" InspectType,mi "Messages" "" Problems,mi "Go to next" "Alt+F8" NextMessage,mi "Go to previous" "Alt+F7" PreviousMessage,mi "Restart language server" "" RestartHLS,mi "Conversation" "" Conversation,mi "Prompt..." "" AgentPrompt,mi "Cancel reply" "" AgentCancel,mi "Resume session..." "" AgentResume,mi "New session" "" AgentNew,mi "Copy raw conversation" "" AgentCopyRaw,mi "Widget gallery..." "" Gallery,off "Project browser..." "" "Cabal component browsing is a later milestone."])
  ,("Options",'o',[mi "Preferences..." "" EditorOptions,mi "Agents..." "" AgentOptions])
  ,("Window",'w',[mi "Tile" "" Tile,mi "Cascade" "" Cascade,mi "Split vertically" "" SplitVertical,mi "Split horizontally" "" SplitHorizontal,mi "Zoom" "F5" Zoom,mi "Next" "F6" NextWindow,mi "Close" "Alt+F3" Close])
  ,("Help",'h',[mi "Contents" "F1" Help,mi "About Turbo Haskell..." "" About])]
  where mi = MenuItem
        off title key reason = mi title key (Disabled reason)

menuMnemonic :: MenuItem -> Char
menuMnemonic (MenuItem title _ cmd) = case cmd of
  ToggleTree -> 'f'; GitDiff -> 'g'; GitCommit -> 'a'
  Problems -> 'm'; NextMessage -> 'n'; PreviousMessage -> 'p'
  SaveAs -> 'a'; Quit -> 'x'; Cut -> 't'; SelectAll -> 'a'
  SplitVertical -> 'v'; SplitHorizontal -> 'h'
  Close -> 'l'
  _ -> toLower (T.head title)

menuShortcut :: Desktop -> MenuItem -> Text
menuShortcut d (MenuItem _ key cmd)
  | nativeMac d = fromMaybe key (lookup cmd [(New,"Cmd+N"),(Open,"Cmd+O"),(Save,"Cmd+S"),(SaveAs,"Cmd+Shift+S"),(Close,"Cmd+W"),(Quit,"Cmd+Q"),(Undo,"Cmd+Z"),(Redo,"Cmd+Shift+Z"),(Copy,"Cmd+C"),(Cut,"Cmd+X"),(Paste,"Cmd+V"),(SelectAll,"Cmd+A"),(Find,"Cmd+F"),(FindNext,"Cmd+G")])
  | otherwise = key

commandDescription :: Command -> Text
commandDescription cmd = case cmd of
  New -> "Create a new source buffer."; Open -> "Browse directories and open a file."
  Save -> "Save the active file."; SaveAs -> "Save the active buffer under a new filename."
  ReviewDisk -> "Review an external change without discarding unsaved text."
  RunTarget -> "Run the selected Cabal executable through thc run."
  RunOptions -> "Choose the Cabal executable and THC installation."
  OpenTerminal -> "Open a project shell in a terminal window."
  StopTerminal -> "Stop the selected terminal process."
  AgentOptions -> "Configure agents and their executable commands."
  Conversation -> "Show the agent conversation."
  AgentPrompt -> "Send a prompt to the selected agent."
  AgentCancel -> "Cancel the active agent reply."
  AgentResume -> "Resume an agent session."
  AgentNew -> "Start a new agent session."
  AgentCopyRaw -> "Copy the raw conversation text."
  Close -> "Close this window; ask before discarding unsaved changes."
  Quit -> "Exit the editor; ask before discarding unsaved changes."
  Undo -> "Undo the last edit."; Redo -> "Redo the last undone edit."
  Cut -> "Cut the selected text."; Copy -> "Copy the selected text."; Paste -> "Insert clipboard text."
  SelectAll -> "Select all text in this buffer."
  Find -> "Find text in the active buffer."; FindNext -> "Find the next occurrence of the last search."
  Replace -> "Find text and replace the next match."; GoTo -> "Move to a line number."
  Zoom -> "Toggle between full workspace and the previous window size."
  NextWindow -> "Activate the next editor window."; Cascade -> "Arrange windows in an overlapping stack."
  Tile -> "Arrange windows in horizontal rows."
  SplitVertical -> "Create a side-by-side view of the same buffer."
  SplitHorizontal -> "Create a view of the same buffer above or below."
  About -> "Show information about Turbo Haskell."; Help -> "Open the read-only help document."
  EditorOptions -> "Change key bindings, cursor appearance, and graphical screen mode."; Gallery -> "Try the available dialog controls."
  InspectType -> "Ask HLS for type information at the cursor."
  Definition -> "Go to the symbol's definition using HLS."
  Complete -> "Choose an identifier completion from HLS."
  Problems -> "Show or hide Messages; select a diagnostic to visit it."
  NextMessage -> "Go to the next diagnostic, opening its file if needed."
  PreviousMessage -> "Go to the previous diagnostic, opening its file if needed."
  RestartHLS -> "Restart the Haskell language server."
  RenameSymbol -> "Rename this symbol with HLS; review and save the changed buffers."
  ToggleTree -> "Show or hide the file tree."
  GitDiff -> "Review saved Git changes, including untracked files."
  GitCommit -> "Approve the reviewed saved changes and enter a commit message."
  GitFetch -> "Fetch remote changes without changing the working tree."
  GitPull -> "Pull remote changes with a fast-forward-only update."
  GitMerge -> "Choose a branch to merge into the current branch."
  Disabled reason -> reason

menuHelp :: Desktop -> Maybe Text
menuHelp d = case contextMenu d of
  Just (_,i) -> commandDescription . snd <$> listToMaybe (drop i (contextItems (contextKind d)))
  Nothing -> do
    (i,j)<-menu d
    MenuItem _ _ cmd<-listToMaybe (drop j (menuItems i))
    pure (commandDescription cmd)

menuPositions :: [(Int,Int)]
menuPositions = zip starts widths
  where widths = [T.length title+2 | (title,_,_) <- menus]
        starts = scanl (+) 1 widths

menuItems :: Int -> [MenuItem]
menuItems i = let (_,_,xs) = menus !! (i `mod` length menus) in xs

commandEnabled :: Desktop -> Command -> Bool
commandEnabled _ Disabled{} = False
commandEnabled d cmd | cmd `elem` [NextMessage,PreviousMessage] = not (null (diagnostics d))
commandEnabled _ _ = True
menuRect :: Desktop -> Int -> Rect
menuRect d i = Rect (min x (max 0 (sw-w))) 1 w (length (menuItems i)+2)
  where x = fst (menuPositions !! i)
        sw = fst (screenSize d)
        w = min sw (maximum [T.length t + T.length (menuShortcut d entry) + 5 | entry@(MenuItem t _ _) <- menuItems i])

initialDesktop :: (Int,Int) -> Desktop
initialDesktop size = Desktop size [] M.empty 1 Nothing Nothing Nothing "" False Nothing "" Nothing "" Nothing "" False Nothing Nothing Nothing "" Nothing Nothing Nothing [] False 0 0 False Nothing 0 0 Nothing SourceContext Nothing True

activeWindow :: Desktop -> Maybe Window
activeWindow = listToMaybe . windows
activeDocument :: Desktop -> Maybe Document
activeDocument d = activeWindow d >>= (\w -> M.lookup (bufferId w) (buffers d))

fitRect :: (Int,Int) -> Rect -> Rect
fitRect (sw,sh) (Rect x y w h) = Rect (max 0 (min x (sw-w'))) (max 1 (min y (sh-1-h'))) w' h'
  where w' = max 1 (min sw (max 16 w)); h' = max 1 (min (max 1 (sh-2)) (max 5 h))

addDocument :: Maybe FileState -> Buffer -> Desktop -> Desktop
addDocument file b d = d { windows = w : windows d, buffers = M.insert i (newDocument b file) (buffers d), nextId = i+1, problemsFocused=False, sideTree=fmap (\tree -> tree {treeFocused=False}) (sideTree d) }
  where
    i = nextId d
    offset = length (windows d) `mod` 5
    (sw,sh) = screenSize d
    w = Window i i (fitWindow d (Rect offset (1+offset) (sw-offset) (sh-2-offset))) (Selection 0 0) 0 0 Nothing (nextWindowNumber d)

nextWindowNumber :: Desktop -> Int
nextWindowNumber d = choose 1
  where used=map windowNumber (windows d)++maybe [] pure (messagesNumber d)
        choose n=if n `elem` used then choose (n+1) else n

activateWindowNumber :: Int -> Desktop -> Desktop
activateWindowNumber number d
  | Just number==messagesNumber d, problemsVisible d = ready {problemsFocused=True,sideTree=fmap (\tree -> tree {treeFocused=False}) (sideTree d)}
  | Just w<-find ((==number) . windowNumber) (windows d) = focusWindow (windowId w) ready
  | otherwise = d
  where ready=d {menu=Nothing,contextMenu=Nothing,drag=Nothing,dragOriginal=Nothing,prefix=Nothing}

windowFocused :: Desktop -> Window -> Bool
windowFocused d w = not (problemsFocused d) && not (maybe False treeFocused (sideTree d)) &&
  fmap windowId (activeWindow d)==Just (windowId w)

focusWindow :: Int -> Desktop -> Desktop
focusWindow i d = d { problemsFocused=False, sideTree=fmap (\tree -> tree {treeFocused=False}) (sideTree d), windows = filter ((==i) . windowId) (windows d) ++ filter ((/=i) . windowId) (windows d) }

modifyActive :: (Window -> Window) -> Desktop -> Desktop
modifyActive f d = d { windows = case windows d of [] -> []; w:ws -> f w : ws }

ensureVisible :: Desktop -> Desktop
ensureVisible d = case (activeWindow d, activeDocument d) of
  (Just w, Just doc) -> modifyActive (const w { scrollRow = max 0 row', scrollColumn = max 0 col' }) d
    where
      b = documentBuffer doc
      (row,col) = bufferLineColumn b (caret (selection w))
      dc = displayColumn (bufferLineAt b row) col
      rows = max 1 (height (bounds w)-2); cols = max 1 (width (bounds w)-2)
      row' = if row < scrollRow w then row else if row >= scrollRow w+rows then row-rows+1 else scrollRow w
      col' = if dc < scrollColumn w then dc else if dc >= scrollColumn w+cols then dc-cols+1 else scrollColumn w
  _ -> d

-- Map other view positions through the changed character interval.
editActive :: (Selection -> Buffer -> Buffer) -> Maybe Int -> Desktop -> Desktop
editActive _ _ d | maybe False treeFocused (sideTree d) || problemsFocused d = d
editActive f cursor d = case (activeWindow d, activeDocument d) of
  (Just _, Just doc) | documentLabel doc /= Nothing -> d {status="This window is read-only."}
  (Just active, Just doc)
    | revision changed==revision original -> maybe d (\p -> moveTo False p d) cursor
    | otherwise -> ensureVisible d { buffers = M.insert bid (restyle doc {documentBuffer = changed}) (buffers d), windows = map adjust (windows d) }
    where
      bid = bufferId active
      original = documentBuffer doc
      changed = f (selection active) original
      new = contents changed
      (common,oldEnd,inserted) = fromMaybe (0,0,0) (lastChange changed)
      newEnd = common+inserted
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
    len = maybe 0 (bufferLength . documentBuffer) (activeDocument d)
    p = max 0 (min len pos)
    update w = w {selection = Selection (if extend then anchor (selection w) else p) p}

wrapMessage :: T.Text -> [T.Text]
wrapMessage text | T.null text=[]
wrapMessage text=T.take 54 text:wrapMessage (T.drop 54 text)

message :: Text -> [Text] -> Desktop -> Desktop
message title lines' d = d {dialog = Just (Dialog title Information [] 0 ["OK"] lines'), menu = Nothing, drag = Nothing,dragOriginal=Nothing}

prompt :: Text -> Purpose -> [Field] -> Desktop -> Desktop
prompt title p fs d = d {dialog = Just (Dialog title p fs 0 ["OK","Cancel"] []), menu = Nothing, drag = Nothing,dragOriginal=Nothing}

runCommand :: Command -> Desktop -> (Desktop,[Effect])
runCommand cmd source = go cmd (source {menu = Nothing, contextMenu=Nothing, buttonHover=Nothing, buttonPressed=Nothing, prefix = Nothing, drag = Nothing,dragOriginal=Nothing})
  where
    go New d = (addDocument Nothing (newBuffer "") d,[])
    go Open d = (d,[BrowsePath (startingDirectory d) "*.hs"])
    go Save d = saveRequest Nothing d
    go ReviewDisk d = (d,[ReviewExternal])
    go RunTarget d = (d,[AgentAction "run" []])
    go RunOptions d = (d,[AgentAction "run-options" []])
    go OpenTerminal d = (d,[AgentAction "terminal" []])
    go StopTerminal d = (d,[AgentAction "terminal-stop" []])
    go AgentOptions d = (d,[AgentAction "options" []])
    go Conversation d = (d,[AgentAction "show" []])
    go AgentPrompt d = (d,[AgentAction "prompt" []])
    go AgentCancel d = (d,[AgentAction "cancel" []])
    go AgentResume d = (d,[AgentAction "resume" []])
    go AgentNew d = (d,[AgentAction "new" []])
    go AgentCopyRaw d = (d,[AgentAction "copy" []])
    go SaveAs d = case activeWindow d of
      Nothing -> (d,[])
      Just _ | maybe False ((/=Nothing) . documentLabel) (activeDocument d) -> (d {status="This window is read-only."},[])
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
    go Paste d | Just ident<-activeTerminal d = (d,[AgentAction "terminal-input" [ident,clipboard d]])
    go Paste d = (insertText (clipboard d) d,[])
    go SelectAll d = (modifyActive (\w -> w {selection = Selection 0 (T.length (activeText d))}) d,[])
    go Find d = (prompt "Find" Finding [Input "Text to find" (lastFind d) (T.length (lastFind d))] d,[])
    go Replace d = (prompt "Replace" Replacing [Input "Text to find" (lastFind d) (T.length (lastFind d)),Input "Replace with" "" 0] d,[])
    go FindNext d = (findText (lastFind d) d,[])
    go GoTo d = (prompt "Go to line" GoingTo [Input "Line number" "1" 1] d,[])
    go Zoom d = (modifyActive zoom d,[]) where
      zoom w = case restoredBounds w of
        Just r -> w {bounds = fitWindow d r, restoredBounds = Nothing}
        Nothing -> w {bounds = let (sw,sh) = screenSize d in Rect (treeWidthOf d) 1 (sw-treeWidthOf d) (sh-2-problemsHeight d), restoredBounds = Just (bounds w)}
    go NextWindow d = (d {windows = case windows d of [] -> []; w:ws -> ws++[w]},[])
    go Cascade d = (d {windows = zipWith cascade [0..] (windows d)},[]) where
      (sw,sh) = screenSize d
      cascade i w = w {bounds = fitWindow d (Rect (treeWidthOf d+i `mod` 6) (1+i `mod` 6) (sw-treeWidthOf d-6) (sh-8)), restoredBounds = Nothing}
    go Tile d = (tileWindows False d,[])
    go SplitVertical d = splitWindow True d
    go SplitHorizontal d = splitWindow False d
    go About d = (message "About Turbo Haskell" ["Turbo Haskell  0.1", "Copyright (c) 2026 Edward Kmett", "", "Haskell source editor"] d,[])
    go InspectType d = (d,[LanguageRequest TypeInfo])
    go RenameSymbol d = (prompt "Rename symbol" Renaming [Input "New name" "" 0] d,[])
    go Definition d = (d,[LanguageRequest FindDefinition])
    go Complete d = (d,[LanguageRequest Completions])
    go Problems d = ((setProblemsVisible (not (problemsVisible d)) d) {problemsFocused=not (problemsVisible d)},[LanguageRequest ShowProblems])
    go NextMessage d = navigateMessage 1 d
    go PreviousMessage d = navigateMessage (-1) d
    go RestartHLS d = (d,[LanguageRequest RestartLanguage])
    go Help d = (d,[ReadHelp])
    go ToggleTree d = case sideTree d of Just _ -> (setTree Nothing d,[]); Nothing -> (d,[ReadTree (startingDirectory d)])
    go GitDiff d = (d,[ReadGitDiff])
    go GitCommit d = (d,[AskGitCommit])
    go GitFetch d = (d,[RunGit FetchRemote])
    go GitPull d = (d,[RunGit PullRemote])
    go GitMerge d = (d,[ReadMergeBranches])
    go EditorOptions d = (prompt "Preferences" Settings
      ([Radio "Key bindings" ["Modern","WordStar"] (if wordStar d then 1 else 0)] ++
       [Radio "Screen size" ["Mode 3 (80x25)","Mode 259 (80x50)"] (if mode == 259 then 1 else 0) | Just mode <- [videoMode d]] ++
       [CheckBox "Blinking cursor" (blinkCursor d)]) d,[])
    go Gallery d = (prompt "Dialog controls" Widgets [Input "Module name" "Main" 4,CheckBox "Auto indent" True,Radio "Tab width" ["4 columns","8 columns"] 1,ListBox "Source files" ["Main.hs","Types.hs","Parser.hs","Syntax.hs","Eval.hs"] 0] d,[])
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
  (Just _,Just doc) | documentLabel doc /= Nothing -> (d {status="This window is read-only."},[])
  (Just w,Just doc) -> case documentFile doc of
    Nothing -> (prompt "Save file as" (Saving (bufferId w) after) [Input "Name" "" 0] d,[])
    Just _ -> (d,[SaveDocument (bufferId w) Nothing after])
  _ -> (d,[])

closeActive :: Desktop -> Desktop
closeActive d = case windows d of
  [] -> d
  w:ws -> d {windows = ws, buffers = if any ((==bufferId w) . bufferId) ws then buffers d else M.delete (bufferId w) (buffers d)}

tileWindows :: Bool -> Desktop -> Desktop
tileWindows vertical d
  | extent `div` n < (if vertical then 16 else 5) = d {status="Not enough room to tile; enlarge the terminal."}
  | otherwise = d {windows = zipWith place [0..] (windows d)}
  where
    n = max 1 (length (windows d)); (sw,sh) = screenSize d; areaWidth=sw-treeWidthOf d; areaHeight=sh-2-problemsHeight d; extent = if vertical then areaWidth else areaHeight
    place i w = w {bounds = if vertical then Rect (treeWidthOf d+start) 1 size areaHeight else Rect (treeWidthOf d) (1+start) areaWidth size, restoredBounds = Nothing}
      where start = i*extent `div` n; size = (i+1)*extent `div` n-start

splitWindow :: Bool -> Desktop -> (Desktop,[Effect])
splitWindow vertical d = case activeWindow d of
  Nothing -> (d,[])
  Just _ | (if vertical then (fst (screenSize d)-treeWidthOf d) `div` (length (windows d)+1) < 16 else (snd (screenSize d)-2-problemsHeight d) `div` (length (windows d)+1) < 5) -> (d {status="Not enough room to split; enlarge the terminal."},[])
  Just w -> (tileWindows vertical d {windows = w {windowId = nextId d,windowNumber=nextWindowNumber d} : windows d, nextId = nextId d+1},[])

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
    searchFrom p = case locate p (T.drop p t) of Just x -> Just x; Nothing -> locate 0 t

helpLines :: [Text]
helpLines = ["F1 Help   F2 Save   F3 Open   F5 Zoom", "F6 Next window   F10 Menu   Alt+X Exit", "Alt+F3 Close   Ctrl+Z Undo   Ctrl+Y Redo", "Shift+arrows Select   Ctrl+arrows Words", "Ctrl+C/X/V Copy/Cut/Paste (internal clipboard)", "Ctrl+F Find   Ctrl+R Replace   Ctrl+L Next", "Ctrl+G Go to line   Ctrl+A Select all", "Mouse: title drag, bottom-right resize", "Window menu: tile, cascade, shared splits", "", "WordStar (Options > Editor):", "Ctrl+E/S/D/X Up/Left/Right/Down", "Ctrl+A/F Word left/right   Ctrl+Y Delete line", "Ctrl+K B/K Block start/end   C/V Copy/Cut", "Ctrl+K Y Delete block   S Save   D Close", "Ctrl+Q S/D Line start/end   R/C File top/end", "Ctrl+Q F Find   Ctrl+Q A Replace", "Escape cancels a command prefix.", "", "HLS and Cabal browsing are not connected yet."]

fieldHeight :: Field -> Int
fieldHeight Input{} = 3
fieldHeight CheckBox{} = 2
fieldHeight (Radio _ xs _) = length xs+2
fieldHeight ListBox{} = 6
fieldHeight FileList{} = 13

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
    make row f = Rect (x+3) (row-offset) (max 1 (w-6)) (fieldHeight f)
    offset = case drop (focus dg) (zip starts (fields dg)) of
      (row,f):_ -> max 0 (min (row-y-2) (row+fieldHeight f-(top (dialogRect d dg)+height (dialogRect d dg)-3)))
      _ -> 0

buttonRects :: Desktop -> Dialog -> [Rect]
buttonRects d dg = zipWith (\bx label -> Rect bx (y+h-3) (T.length label+4) 1) starts (buttons dg)
  where
    Rect x y w h = dialogRect d dg
    widths = map ((+4) . T.length) (buttons dg)
    total = sum widths+2*(length widths-1)
    starts = scanl (\a b -> a+b+2) (x+max 1 ((w-total) `div` 2)) widths

buttonMnemonics :: Dialog -> [Maybe Char]
buttonMnemonics dg = snd (mapAccumL choose [] (buttons dg))
  where
    choose used name = let chosen=find (\c -> isAlphaNum c && c `notElem` used) (T.unpack (T.toLower name))
                      in (maybe used (:used) chosen,chosen)

-- A screen-mode change scales the desktop layout to use the new row count.
resizeScreenMode :: (Int,Int) -> Desktop -> Desktop
resizeScreenMode (sw,sh) d = ensureVisible resized {windows=map stretch (windows d)}
  where
    resized = fst (handleEvent (V.EvResize sw sh) d)
    (oldW,oldH) = screenSize d
    oldTree = treeWidthOf d
    newTree = treeWidthOf resized
    x n = newTree + (n-oldTree) * (sw-newTree) `div` max 1 (oldW-oldTree)
    y n = 1 + (n-1) * (sh-2) `div` max 1 (oldH-2)
    stretchRect (Rect l t w h) = fitWindow resized (Rect (x l) (y t) (x (l+w)-x l) (y (t+h)-y t))
    stretch w = w {bounds=stretchRect (bounds w),restoredBounds=fmap stretchRect (restoredBounds w)}

handleEvent :: V.Event -> Desktop -> (Desktop,[Effect])
handleEvent event d = dispatchEvent event (case event of
  V.EvKey{} -> d {hoverTarget=Nothing,typeHint="",buttonHover=Nothing,buttonPressed=Nothing}
  V.EvMouseDown{} -> d {hoverTarget=Nothing,typeHint=""}
  V.EvPaste{} -> d {hoverTarget=Nothing,typeHint=""}
  _ -> d)

dispatchEvent :: V.Event -> Desktop -> (Desktop,[Effect])
dispatchEvent (V.EvResize sw sh) d = let resized=d {screenSize=(max 1 sw,max 3 sh),sideTree=fmap (\t -> t {treeWidth=min (treeWidth t) (max 0 (sw-16))}) (sideTree d)} in (resized {windows=map (\w -> w {bounds=fitWindow resized (bounds w)}) (windows d),drag=Nothing,dragOriginal=Nothing,menu=Nothing,contextMenu=Nothing,buttonHover=Nothing,buttonPressed=Nothing},[])
dispatchEvent (V.EvKey (V.KChar c) mods) d | dialog d==Nothing, V.MAlt `elem` mods, c>='1', c<='9' = (activateWindowNumber (fromEnum c-fromEnum '0') d,[])
dispatchEvent (V.EvKey (V.KFun key) mods) d | dialog d==Nothing, V.MAlt `elem` mods, key `elem` [7,8] = runCommand (if key==8 then NextMessage else PreviousMessage) d
dispatchEvent (V.EvKey (V.KFun 9) [V.MCtrl]) d | dialog d==Nothing = runCommand RunTarget d
dispatchEvent ev d | Just dg <- dialog d = dialogEvent ev dg d
dispatchEvent ev d | Just popup <- contextMenu d = contextEvent ev popup d
dispatchEvent ev d | Just m <- menu d = menuEvent ev m d
dispatchEvent (V.EvKey key mods) d | Just _ <- dragOriginal d = dragKey key mods d
dispatchEvent (V.EvKey key mods) d | problemsVisible d && problemsFocused d = problemsKey key mods d
dispatchEvent ev d | Just ident<-activeTerminal d,Just text<-terminalInput ev = (d,[AgentAction "terminal-input" [ident,text]])
dispatchEvent (V.EvMouseUp _ _ _) d = (d {drag = Nothing,dragOriginal=Nothing},[])
dispatchEvent (V.EvMouseDown x y button mods) d = mouseEvent x y button mods d
dispatchEvent (V.EvPaste bytes) d = case TE.decodeUtf8' bytes of
  Left _ -> (message "Paste failed" ["The pasted text is not valid UTF-8."] d,[])
  Right t -> (insertText (T.filter (\c -> isPrint c || c `elem` ['\n','\r','\t']) t) d,[])
dispatchEvent (V.EvKey key mods) d | Just tree <- sideTree d, treeFocused tree = treeKey key mods tree d
dispatchEvent (V.EvKey key mods) d = keyEvent key mods d
dispatchEvent _ d = (d,[])

activeTerminal :: Desktop -> Maybe Text
activeTerminal d = do
  w<-activeWindow d
  if not (windowFocused d w) then Nothing else activeDocument d >>= documentLabel >>= T.stripPrefix "Terminal "

-- Window/menu shortcuts stay with the editor; ordinary keys go to the PTY.
terminalInput :: V.Event -> Maybe Text
terminalInput (V.EvPaste bytes)=either (const Nothing) Just (TE.decodeUtf8' bytes)
terminalInput (V.EvKey key mods)
  | V.MAlt `elem` mods || V.MMeta `elem` mods = Nothing
  | key `elem` [V.KFun 5,V.KFun 6,V.KFun 10] = Nothing
  | otherwise = case key of
      V.KChar c | V.MCtrl `elem` mods, c==' ' -> Just "\0"
                | V.MCtrl `elem` mods, let upper=toUpper c,upper>='@' && upper<='_' -> Just (T.singleton (chr (ord upper-64)))
                | V.MCtrl `notElem` mods -> Just (T.singleton c)
      V.KEnter -> Just "\r"
      V.KBS -> Just "\DEL"
      V.KEsc -> Just "\ESC"
      V.KBackTab -> Just "\ESC[Z"
      V.KUp -> arrow "A"; V.KDown -> arrow "B"; V.KRight -> arrow "C"; V.KLeft -> arrow "D"
      V.KHome -> arrow "H"; V.KEnd -> arrow "F"
      V.KDel -> Just "\ESC[3~"; V.KIns -> Just "\ESC[2~"
      V.KPageUp -> Just "\ESC[5~"; V.KPageDown -> Just "\ESC[6~"
      V.KFun n -> lookup n [(1,"\ESCOP"),(2,"\ESCOQ"),(3,"\ESCOR"),(4,"\ESCOS"),(7,"\ESC[18~"),(8,"\ESC[19~"),(9,"\ESC[20~"),(11,"\ESC[23~"),(12,"\ESC[24~")]
      _ -> Nothing
  where arrow suffix=Just ("\ESC["<>(if V.MCtrl `elem` mods then "1;5" else if V.MShift `elem` mods then "1;2" else "")<>suffix)
terminalInput _=Nothing

menuEvent :: V.Event -> (Int,Int) -> Desktop -> (Desktop,[Effect])
menuEvent ev (i,j) d = case ev of
  V.EvKey V.KEsc _ -> (d {menu = Nothing},[])
  V.EvKey V.KLeft _ -> choose (i-1) 0
  V.EvKey V.KRight _ -> choose (i+1) 0
  V.EvKey V.KUp _ -> choose i (j-1)
  V.EvKey V.KDown _ -> choose i (j+1)
  V.EvKey V.KEnter _ -> invoke j
  V.EvKey (V.KChar c) _ -> case findIndex (\item -> toLower c == menuMnemonic item) (menuItems i) of
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

contextItems :: ContextKind -> [(Text,Command)]
contextItems SourceContext = [("Rename symbol...",RenameSymbol),("Go to definition",Definition),("Inspect type",InspectType),("Complete identifier",Complete)]
contextItems GitContext = [("Pull",GitPull),("Fetch",GitFetch),("Merge...",GitMerge)]

openContext :: ContextKind -> Int -> Int -> Desktop -> Desktop
openContext kind x y d = d {contextKind=kind,contextMenu=Just (popup,0),drag=Nothing,dragOriginal=Nothing,menu=Nothing}
  where
    (sw,sh)=screenSize d
    h=length (contextItems kind)+2
    popup=Rect (max 0 (min x (sw-24))) (max 1 (min y (sh-h-1))) (min sw 24) h

gitCountText :: Int -> Text
gitCountText n = if n<0 then "?" else T.pack (show n)

gitBranchText :: Desktop -> Text
gitBranchText d
  | T.length name<=available = name
  | available==0 = ""
  | otherwise = T.take (available-1) name<>"…"
  where
    name=branchStatus d
    sw=fst (screenSize d)
    counts=" +"<>gitCountText (branchAdded d)<>" -"<>gitCountText (branchDeleted d)<>" "
    available=max 0 (min (sw `div` 2) (sw-T.length counts-3))

gitBadgeText :: Desktop -> Text
gitBadgeText d = if T.null (branchStatus d) then "" else " │ "<>gitBranchText d<>" +"<>gitCountText (branchAdded d)<>" -"<>gitCountText (branchDeleted d)<>" "

gitBadgeRect :: Desktop -> Rect
gitBadgeRect d = let (sw,sh)=screenSize d; len=T.length (gitBadgeText d) in Rect (max 0 (sw-len)) (sh-1) (min sw len) 1

contextEvent :: V.Event -> (Rect,Int) -> Desktop -> (Desktop,[Effect])
contextEvent ev (r,chosen) d = case ev of
  V.EvKey V.KEsc _ -> close
  V.EvKey V.KUp _ -> choose (chosen-1)
  V.EvKey V.KDown _ -> choose (chosen+1)
  V.EvKey V.KEnter _ -> invoke chosen
  V.EvMouseDown x y V.BLeft _
    | inside r x y && y>top r && y<top r+height r-1 -> invoke (y-top r-1)
    | otherwise -> close
  V.EvMouseDown x y V.BRight mods -> mouseEvent x y V.BRight mods d {contextMenu=Nothing}
  _ -> (d,[])
  where
    close=(d {contextMenu=Nothing},[])
    choose i=(d {contextMenu=Just (r,i `mod` length items)},[])
    invoke i=case drop i items of (_,cmd):_ -> runCommand cmd d; _ -> close
    items=contextItems (contextKind d)

-- SDL supplies click counts; terminal clicks retain the same selection and Enter path.
handleDoubleClick :: Int -> Int -> Desktop -> (Desktop,[Effect])
handleDoubleClick x y d = case dialog d of
  Just dg | Just (i,chosen) <- fileEntryAt x y d dg ->
    let fs=fields dg
        select (FileList entries _) = FileList entries chosen
        select f=f
        selected=dg {focus=i,fields=replaceAt i (select (fs !! i)) fs}
    in submitDialog 0 selected d {dialog=Just selected}
  _ -> handleEvent (V.EvMouseDown x y V.BLeft []) d

fileEntryAt :: Int -> Int -> Desktop -> Dialog -> Maybe (Int,Int)
fileEntryAt x y d dg = listToMaybe
  [(i,chosen) | (i,(r,FileList entries selected))<-zip [0..] (zip (fieldRects d dg) (fields dg))
  , inside r x y, y>=top r+2, y<top r+10
  , y>=top (dialogRect d dg)+2, y<top (dialogRect d dg)+height (dialogRect d dg)-3
  , let cw=max 1 ((width r-3) `div` 2)
        column=x-left r
  , column>=1, column<=2*cw+1, column/=cw+1
  , let chosen=(max 0 selected `div` 16)*16+y-top r-2+(if column>cw+1 then 8 else 0)
  , chosen<length entries]

mouseEvent :: Int -> Int -> V.Button -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
mouseEvent x y V.BLeft _ d | Just capture <- drag d = (case capture of
  DockSizing -> resizeTree x d
  Moving i dx dy -> mapWindow i (\w -> w {bounds = fitWindow d (bounds w) {left=x-dx,top=y-dy},restoredBounds=Nothing}) d
  Resizing i dx dy -> mapWindow i (\w -> w {bounds = fitWindow d (bounds w) {width=x-left (bounds w)+dx,height=y-top (bounds w)+dy},restoredBounds=Nothing}) d
  Scrolling i vertical -> scrollTrack vertical x y (focusWindow i d)
  Selecting i -> selectAt True x y (focusWindow i d),[])
mouseEvent x 0 V.BLeft _ d = (d {menu = (\i -> (i,0)) <$> menuAt x},[])
mouseEvent x y V.BRight _ d | inside (gitBadgeRect d) x y = (openContext GitContext x y d,[])
mouseEvent x y button _ d | problemsVisible d, inside (problemsRect d) x y = problemsMouse x y button d
mouseEvent x y button _ d | Just tree <- sideTree d, x < treeWidth tree = treeMouse x y button tree d {problemsFocused=False}
mouseEvent x y button mods d = case find (\w -> inside (bounds w) x y) (windows d) of
  Nothing -> (d,[])
  Just w -> let focused = focusWindow (windowId w) d {sideTree=fmap (\sidebar -> sidebar {treeFocused=False}) (sideTree d)}; Rect l t ww hh = bounds w in case button of
    V.BScrollUp -> (changeScroll True (-3) focused,[])
    V.BScrollDown -> (changeScroll True 3 focused,[])
    V.BRight | x>l && x<l+ww-1 && y>t && y<t+hh-1,
               maybe False ((==Nothing) . documentLabel) (activeDocument focused) ->
      (openContext SourceContext x y (selectAt False x y focused),[])
    V.BLeft
      | not (windowFocused d w), x==l || x==l+ww-1 || y==t || y==t+hh-1 -> (focused,[])
      | y==t && x>=l+2 && x<=l+4 -> runCommand Close focused
      | y==t && x>=l+ww-6 && x<l+ww-3 -> runCommand Zoom focused
      | y==t -> (focused {drag=Just (Moving (windowId w) (x-l) (y-t)),dragOriginal=Just (windowId w,bounds w,restoredBounds w)},[])
      | x>=l+ww-2 && y==t+hh-1 -> (focused {drag=Just (Resizing (windowId w) (l+ww-x) (t+hh-y)),dragOriginal=Just (windowId w,bounds w,restoredBounds w)},[])
      | Just doc<-activeDocument focused, inside (scrollbarRect True doc w) x y -> (scrollClick True x y focused,[])
      | Just doc<-activeDocument focused, inside (scrollbarRect False doc w) x y -> (scrollClick False x y focused,[])
      | y==t+hh-1 -> (focused,[])
      | otherwise -> (selectAt (V.MShift `elem` mods) x y focused {drag=Just (Selecting (windowId w))},[])
    _ -> (focused,[])

mapWindow :: Int -> (Window -> Window) -> Desktop -> Desktop
mapWindow i f d = d {windows = map (\w -> if windowId w==i then f w else w) (windows d)}

dragKey :: V.Key -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
dragKey key mods d = case dragOriginal d of
  Nothing -> (d,[])
  Just (wid,original,restored) -> case key of
    V.KEsc -> (mapWindow wid (\w -> w {bounds=original,restoredBounds=restored}) done,[])
    V.KEnter -> (done,[])
    _ | Just (dx,dy)<-lookup key [(V.KLeft,(-1,0)),(V.KRight,(1,0)),(V.KUp,(0,-1)),(V.KDown,(0,1))] ->
      (mapWindow wid (\w -> let { r=bounds w; changed=if V.MShift `elem` mods then r {width=width r+dx,height=height r+dy} else r {left=left r+dx,top=top r+dy} }
                           in w {bounds=fitWindow d changed,restoredBounds=Nothing}) d,[])
      | otherwise -> (d,[])
  where done=d {drag=Nothing,dragOriginal=Nothing}

windowPositionText :: Document -> Window -> Text
windowPositionText doc w = let (r,c)=bufferLineColumn (documentBuffer doc) (caret (selection w))
  in " "<>T.pack (show (r+1))<>":"<>T.pack (show (c+1))<>" "

scrollbarRect :: Bool -> Document -> Window -> Rect
scrollbarRect vertical doc w
  | vertical = Rect (x+ww-1) (y+1) 1 (max 0 (hh-2))
  | otherwise = let start=2+T.length (windowPositionText doc w) in Rect (x+start) (y+hh-1) (max 0 (ww-start-2)) 1
  where Rect x y ww hh=bounds w

scrollbarLimit :: Bool -> Document -> Window -> Int
scrollbarLimit vertical doc w = max 0 (if vertical then bufferLineCount (documentBuffer doc)-max 1 (height (bounds w)-2)
  else documentWidth doc-max 1 (width (bounds w)-2)+1)

scrollbarThumb :: Int -> Int -> Int -> Int
scrollbarThumb len limit position = 1+min limit (max 0 position)*max 0 (len-3) `div` max 1 limit

changeScroll :: Bool -> Int -> Desktop -> Desktop
changeScroll vertical delta d = case (activeWindow d,activeDocument d) of
  (Just w,Just doc) -> let value=max 0 (min (scrollbarLimit vertical doc w) ((if vertical then scrollRow w else scrollColumn w)+delta))
    in modifyActive (\v -> if vertical then v {scrollRow=value} else v {scrollColumn=value}) d
  _ -> d

scrollClick :: Bool -> Int -> Int -> Desktop -> Desktop
scrollClick vertical x y d = case (activeWindow d,activeDocument d) of
  (Just w,Just doc) ->
    let r=scrollbarRect vertical doc w
        len=if vertical then height r else width r
        offset=if vertical then y-top r else x-left r
        thumb=scrollbarThumb len (scrollbarLimit vertical doc w) (if vertical then scrollRow w else scrollColumn w)
        page=max 1 ((if vertical then height else width) (bounds w)-2)
    in if offset==0 then changeScroll vertical (-1) d
       else if offset==len-1 then changeScroll vertical 1 d
       else if offset==thumb then d {drag=Just (Scrolling (windowId w) vertical)}
       else changeScroll vertical (if offset<thumb then negate page else page) d
  _ -> d

scrollTrack :: Bool -> Int -> Int -> Desktop -> Desktop
scrollTrack vertical x y d = case (activeWindow d,activeDocument d) of
  (Just w,Just doc) -> let { r=scrollbarRect vertical doc w
                         ; len=if vertical then height r else width r
                         ; offset=if vertical then y-top r else x-left r
                         ; value=max 0 (min (scrollbarLimit vertical doc w) ((offset-1)*scrollbarLimit vertical doc w `div` max 1 (len-3))) }
                     in modifyActive (\v -> if vertical then v {scrollRow=value} else v {scrollColumn=value}) d
  _ -> d

selectAt :: Bool -> Int -> Int -> Desktop -> Desktop
selectAt extend x y d = case activeWindow d of
  Nothing -> d
  Just w -> moveTo extend pos d where
    b = maybe (newBuffer "") documentBuffer (activeDocument d)
    row = max 0 (min (bufferLineCount b-1) (y-top (bounds w)-1+scrollRow w))
    col = max 0 (x-left (bounds w)-1+scrollColumn w)
    pos = bufferLineOffset b row + columnOffset (bufferLineAt b row) col

keyEvent :: V.Key -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
keyEvent key mods d
  | key==V.KEsc = (d {prefix=Nothing},[])
  | Just p <- prefix d, V.KChar c <- key = starPrefix p (toLower c) d {prefix=Nothing}
  | V.MAlt `elem` mods, V.KChar c <- key, Just i <- findIndex (\(_,mn,_) -> mn==toLower c) menus = (d {menu=Just (i,0)},[])
  | V.MAlt `elem` mods, key==V.KChar 'x' = runCommand Quit d
  | V.MAlt `elem` mods, key==V.KFun 3 = runCommand Close d
  | key==V.KFun 1, V.MShift `elem` mods = runCommand InspectType d
  | key==V.KFun 12 = runCommand Definition d
  | ctrl, key==V.KChar ' ' = runCommand Complete d
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
  | ctrl, V.KChar c <- key, Just cmd <- lookup (toLower c) [('b',ToggleTree),('s',Save),('o',Open),('n',New),('z',Undo),('y',Redo),('c',Copy),('x',Cut),('v',Paste),('a',SelectAll),('f',Find),('r',Replace),('g',GoTo),('l',FindNext),('q',Quit)] = runCommand cmd d
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
  V.KEnd -> move (if ctrl then T.length t else start+T.length (bufferLineAt b row))
  V.KBS -> erase (if ctrl then wordLeft t p else previousCharacter t p) p
  V.KDel -> erase p (if ctrl then wordRight t p else nextCharacter t p)
  V.KEnter -> insertText (if "\r\n" `T.isInfixOf` t then "\r\n" else "\n") d
  V.KChar '\t' -> insertText "  " d
  V.KChar c | null mods || mods==[V.MShift], isPrint c -> insertText (T.singleton c) d
  _ -> d
  where
    b = maybe (newBuffer "") documentBuffer (activeDocument d)
    t = contents b
    sel = maybe (Selection 0 0) selection (activeWindow d); p = caret sel
    (row,col) = bufferLineColumn b p; start = bufferLineOffset b row
    ctrl = V.MCtrl `elem` mods; shift = V.MShift `elem` mods
    page = maybe 10 (\w -> max 1 (height (bounds w)-3)) (activeWindow d)
    move = (\q -> moveTo shift q d)
    vertical delta = let r = max 0 (min (bufferLineCount b-1) (row+delta))
                    in move (bufferLineOffset b r+columnOffset (bufferLineAt b r) (displayColumn (bufferLineAt b row) col))
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
  'b' -> (d {blockStart=(\w -> (bufferId w,caret (selection w))) <$> activeWindow d},[])
  'k' -> (case (blockStart d,activeWindow d) of
    (Just (bid,p),Just w) | bid==bufferId w -> modifyActive (\v -> v {selection=Selection (min p (T.length (activeText d))) (caret (selection v))}) d
    _ -> d,[])
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
  V.EvKey V.KEsc _ -> (d {dialog=Nothing,buttonHover=Nothing,buttonPressed=Nothing},[])
  V.EvKey (V.KChar c) mods | V.MCtrl `elem` mods || V.MAlt `elem` mods,
    Just i<-findIndex (==Just (toLower c)) (buttonMnemonics dg) -> submitDialog i dg d
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
    Just i -> (d {buttonHover=Just i,buttonPressed=Just i},[])
    Nothing -> case findIndex (\r -> inside r x y && y >= top (dialogRect d dg)+2 && y < top (dialogRect d dg)+height (dialogRect d dg)-3) (fieldRects d dg) of
      Nothing -> (d,[])
      Just i -> let Rect l t _ _ = fieldRects d dg !! i
                    click (FileList xs selected) = FileList xs (max 0 (min (length xs-1) ((max 0 selected `div` 16)*16+max 0 (y-t-2)+if x-l >= width (fieldRects d dg !! i) `div` 2 then 8 else 0)))
                    click (Input label value pos) = Input label value (columnOffset value (max 0 (x-l)+max 0 (displayColumn value pos-width (fieldRects d dg !! i)+1)))
                    click (CheckBox label b) = CheckBox label (not b)
                    click (Radio label xs _) = Radio label xs (max 0 (min (length xs-1) (y-t-1)))
                    click (ListBox label xs selected) = ListBox label xs (max 0 (min (length xs-1) (max 0 (selected-3)+y-t-1)))
                in updateDialog dg {focus=i,fields=replaceAt i (click (fields dg !! i)) (fields dg)}
  V.EvMouseDown _ _ V.BScrollDown _ -> updateField (fieldKey V.KDown [])
  V.EvMouseDown _ _ V.BScrollUp _ -> updateField (fieldKey V.KUp [])
  V.EvMouseUp x y button | button==Nothing || button==Just V.BLeft ->
    let released=d {buttonPressed=Nothing}
    in case buttonPressed d of
      Just i | Just i==findIndex (\r -> inside r x y) (buttonRects d dg) -> submitDialog i dg released
      _ -> (released {buttonHover=Nothing},[])
  _ -> (d,[])
  where
    count=length (fields dg)
    setFocus i = updateDialog dg {focus=i `mod` (count+length (buttons dg))}
    updateDialog new = (d {dialog=Just new},[])
    updateField f | focus dg<count =
                    let old=fields dg !! focus dg
                        changed=f old
                        updated=replaceAt (focus dg) changed (fields dg)
                        clear (FileList entries _) = FileList entries (-1)
                        clear field = field
                        typed = case (old,changed) of (Input _ a _,Input _ b _) -> a/=b; _ -> False
                    in updateDialog dg {fields=if typed then map clear updated else updated}
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
  FileList values chosen -> FileList values (max 0 (min (length values-1) (case key of V.KLeft -> chosen-8; V.KRight -> chosen+8; V.KPageUp -> chosen-16; V.KPageDown -> chosen+16; V.KHome -> 0; V.KEnd -> length values-1; _ -> choose values chosen)))
  ListBox label values chosen -> ListBox label values (choose values chosen)
  _ -> field
  where choose xs n = max 0 (min (length xs-1) (n + case key of V.KUp -> -1; V.KDown -> 1; V.KLeft -> -1; V.KRight -> 1; V.KChar ' ' -> 1; _ -> 0))

submitDialog :: Int -> Dialog -> Desktop -> (Desktop,[Effect])
submitDialog button dg original
  | button<0 || button>=length (buttons dg) = (original,[])
  | buttons dg !! button == "Cancel" = (d,[])
  | otherwise = case purpose dg of
    Opening base pattern entries
      | focus dg `elem` [1,length (fields dg)], FileList _ selectedFile:_ <- drop 1 (fields dg), selectedFile>=0, entry:_ <- drop selectedFile entries ->
          if entryDirectory entry then (original,[BrowsePath (base </> T.unpack (entryName entry)) pattern]) else (d,[ReadPath (base </> T.unpack (entryName entry))])
      | otherwise -> (original,[OpenChoice base first pattern])
    Completing bid version pos choices -> case (activeWindow d,activeDocument d,drop selected choices) of
      (Just w,Just doc,Completion _ edits:_) | bufferId w==bid, revision (documentBuffer doc)==version, caret (selection w)==pos ->
        (applyCompletion edits d,[])
      _ -> (d {status="Completion expired; request it again."},[])
    Locations places -> case drop selected places of
      (path,row,col):_ -> (d,[JumpTo path row col])
      _ -> (d,[])
    Merging branches -> case drop selected branches of
      branch:_ -> (d,[RunGit (MergeBranch branch)])
      _ -> (d,[])
    Committing -> if T.null (T.strip first) then (original {status="Enter a commit message."},[]) else (original,[WriteGitCommit first])
    Renaming -> if T.null (T.strip first) then (original {status="Enter a new name."},[]) else (d,[LanguageRequest (RenameAt (T.strip first))])
    Saving bid after -> if T.null first then (original,[]) else (d,[SaveDocument bid (Just (T.unpack first)) after])
    DiskConflict conflict -> (d,[ResolveConflict conflict ([CompareDisk,ReloadDisk,KeepBuffer,SaveConflictAs] !! button)])
    AgentDialog action -> (d,[AgentAction action (T.pack (show button) : values ++
      [if value then "true" else "false" | CheckBox _ value <- fields dg] ++
      [T.pack (show i) | ListBox _ _ i <- fields dg])])
    Finding -> (findText first d,[])
    Replacing -> let found = findText first d
                 in if T.null first || status found=="Search text not found." then (found,[])
                    else (insertText second found,[])
    GoingTo -> case readMaybe (T.unpack first) of
      Just n | n>0 -> (moveTo False (lineOffset (activeText d) (n-1)) d,[])
      _ -> (original {status="Enter a positive line number."},[])
    Confirm cmd | button==0 -> saveRequest (Just cmd) d
                | button==1 -> case cmd of
                    Close -> (closeActive d,[])
                    Quit -> runCommand Quit (discardActive d)
                    _ -> (d,[])
                | otherwise -> (d,[])
    Settings -> (d {wordStar=any (\f -> case f of Radio "Key bindings" _ 1 -> True; _ -> False) (fields dg),
      blinkCursor=fromMaybe (blinkCursor d) (listToMaybe [value | CheckBox "Blinking cursor" value<-fields dg]),status="Preferences updated."},
      [SetScreenMode mode | Radio "Screen size" _ chosen <- fields dg,
       let mode = if chosen == 1 then 259 else 3, Just mode /= videoMode d])
    Widgets -> (d {status="Dialog test complete."},[])
    Information -> (d,[])
  where
    d=original {dialog=Nothing,buttonHover=Nothing,buttonPressed=Nothing}
    selected=fromMaybe 0 (listToMaybe [i | ListBox _ _ i <- fields dg])
    values=[value | Input _ value _ <- fields dg]
    first=fromMaybe "" (listToMaybe values); second=fromMaybe "" (listToMaybe (drop 1 values))
    discardActive s = case activeWindow s of
      Nothing -> s
      Just w -> s {windows=filter ((/=bufferId w) . bufferId) (windows s), buffers=M.delete (bufferId w) (buffers s)}


startingDirectory :: Desktop -> FilePath
startingDirectory d = maybe (maybe "." treeRoot (sideTree d)) (takeDirectory . filePath) (activeDocument d >>= documentFile)

treeWidthOf :: Desktop -> Int
treeWidthOf = maybe 0 treeWidth . sideTree

problemsHeight :: Desktop -> Int
problemsHeight d = if problemsVisible d then min 8 (max 0 (snd (screenSize d)-7)) else 0

problemsRect :: Desktop -> Rect
problemsRect d = let (sw,sh)=screenSize d; h=problemsHeight d in Rect 0 (sh-h-1) sw h

setProblemsVisible :: Bool -> Desktop -> Desktop
setProblemsVisible visible d = ensureVisible next {windows=map resize (windows d)}
  where
    next=d {problemsVisible=visible,problemsFocused=False,drag=Nothing,dragOriginal=Nothing,
      messagesNumber=if visible then Just (fromMaybe (nextWindowNumber d) (messagesNumber d)) else Nothing}
    oldHeight=max 1 (snd (screenSize d)-2-problemsHeight d)
    newHeight=max 1 (snd (screenSize d)-2-problemsHeight next)
    resize w=let r=bounds w in w {bounds=fitWindow next r {top=1+(top r-1)*newHeight `div` oldHeight,height=height r*newHeight `div` oldHeight},restoredBounds=Nothing}

chooseProblem :: Int -> Desktop -> Desktop
chooseProblem index d = d {problemsSelected=chosen,problemsScroll=max 0 (min chosen (max (problemsScroll d) (chosen-visible+1)))}
  where chosen=max 0 (min (length (diagnostics d)-1) index); visible=max 1 (problemsHeight d-2)

jumpProblem :: Desktop -> (Desktop,[Effect])
jumpProblem d = case drop (problemsSelected d) (diagnostics d) of
  problem:_ -> (d,[JumpTo (diagnosticPath problem) (diagnosticRow problem) (diagnosticColumn problem)])
  _ -> (d,[])

navigateMessage :: Int -> Desktop -> (Desktop,[Effect])
navigateMessage delta d
  | null (diagnostics d) = (d {status="No messages."},[])
  | otherwise = jumpProblem (chooseProblem target (if problemsVisible d then d else setProblemsVisible True d))
  where
    index=max 0 (min (length (diagnostics d)-1) (problemsSelected d))
    current=diagnostics d !! index
    atCurrent=case (activeWindow d,activeDocument d) of
      (Just w,Just doc) -> fmap filePath (documentFile doc)==Just (diagnosticPath current) &&
        fst (bufferLineColumn (documentBuffer doc) (caret (selection w)))==diagnosticRow current
      _ -> False
    target=if atCurrent then (index+delta) `mod` length (diagnostics d) else index

problemsMouse :: Int -> Int -> V.Button -> Desktop -> (Desktop,[Effect])
problemsMouse x y button d = case button of
  V.BLeft | problemsFocused d && y==top r && x>=width r-5 -> (setProblemsVisible False d,[])
          | y>top r && y<top r+height r-1 && selected<length (diagnostics d) -> jumpProblem (chooseProblem selected focused)
          | otherwise -> (focused,[])
  V.BScrollUp -> (chooseProblem (problemsSelected d-3) focused,[])
  V.BScrollDown -> (chooseProblem (problemsSelected d+3) focused,[])
  _ -> (d,[])
  where r=problemsRect d; selected=problemsScroll d+y-top r-1; focused=d {problemsFocused=True}

problemsKey :: V.Key -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
problemsKey key mods d = case key of
  V.KUp -> move (-1)
  V.KDown -> move 1
  V.KPageUp -> move (negate (max 1 (problemsHeight d-2)))
  V.KPageDown -> move (max 1 (problemsHeight d-2))
  V.KEnter -> jumpProblem d
  V.KEsc -> leave
  V.KChar '\t' -> leave
  V.KFun 6 -> leave
  V.KChar _ | null mods || mods==[V.MShift] -> (d,[])
  V.KBS -> (d,[])
  V.KDel -> (d,[])
  _ -> keyEvent key mods d
  where move delta=(chooseProblem (problemsSelected d+delta) d,[])
        leave=(d {problemsFocused=False},[])

fitWindow :: Desktop -> Rect -> Rect
fitWindow d r = let offset=treeWidthOf d; (sw,sh)=screenSize d; fitted=fitRect (max 1 (sw-offset),sh-problemsHeight d) r {left=left r-offset} in fitted {left=left fitted+offset}

setTree :: Maybe Sidebar -> Desktop -> Desktop
setTree tree d = next {windows=map move (windows d)}
  where
    next=d {sideTree=tree,drag=Nothing,dragOriginal=Nothing}
    old=treeWidthOf d; new=treeWidthOf next
    available=max 1 (fst (screenSize d)-old); target=max 1 (fst (screenSize d)-new)
    move w = w {bounds=let r=bounds w in fitWindow next r {left=new+(left r-old)*target `div` available,width=width r*target `div` available},restoredBounds=Nothing}

resizeTree :: Int -> Desktop -> Desktop
resizeTree x d = case sideTree d of
  Nothing -> d
  Just tree -> (setTree (Just tree {treeWidth=max 16 (min (fst (screenSize d)-20) (x+1))}) d) {drag=Just DockSizing}

installTree :: FilePath -> [Entry] -> Desktop -> Desktop
installTree root entries d = setTree (Just (Sidebar root (nodes root 0 entries) 0 0 (min 24 (max 0 (fst (screenSize d)-20))) True)) d {problemsFocused=False}

nodes :: FilePath -> Int -> [Entry] -> [TreeRow]
nodes base depth entries = [TreeRow (entryName e) (base </> T.unpack (entryName e)) depth (entryDirectory e) False | e<-entries,entryName e/=".."]

expandTree :: Int -> [Entry] -> Desktop -> Desktop
expandTree i entries d = d {sideTree=fmap expand (sideTree d)}
  where expand t = case drop i (treeRows t) of
          node:rest -> t {treeRows=take i (treeRows t) ++ [node {nodeExpanded=True}] ++ nodes (nodePath node) (nodeDepth node+1) entries ++ rest}
          _ -> t

activateTree :: Bool -> Int -> Desktop -> (Desktop,[Effect])
activateTree forceOpen i d = case sideTree d of
  Just tree | i>=0, node:rest <- drop i (treeRows tree) ->
    let selected=d {sideTree=Just tree {treeSelected=i,treeFocused=True}} in
    if nodeDirectory node then
      if nodeExpanded node && not forceOpen then (selected {sideTree=Just tree {treeSelected=i,treeFocused=True,treeRows=take i (treeRows tree) ++ [node {nodeExpanded=False}] ++ dropWhile ((>nodeDepth node) . nodeDepth) rest}},[])
      else if nodeExpanded node then (selected,[]) else (selected,[ExpandTree i])
    else (selected {sideTree=Just tree {treeSelected=i,treeFocused=False}},[ReadPath (nodePath node)])
  _ -> (d,[])

treeKey :: V.Key -> [V.Modifier] -> Sidebar -> Desktop -> (Desktop,[Effect])
treeKey key mods tree d = case key of
  V.KUp -> move (-1)
  V.KDown -> move 1
  V.KPageUp -> move (-10)
  V.KPageDown -> move 10
  V.KEnter -> activateTree False (treeSelected tree) d
  V.KRight -> activateTree True (treeSelected tree) d
  V.KLeft -> case drop (treeSelected tree) (treeRows tree) of
    node:_ | nodeExpanded node -> activateTree False (treeSelected tree) d
    node:_ -> let ancestors=[i | (i,n)<-zip [0..] (take (treeSelected tree) (treeRows tree)),nodeDepth n < nodeDepth node] in moveToNode (if null ancestors then 0 else last ancestors)
    _ -> (d,[])
  V.KEsc -> leave
  V.KChar '\t' -> leave
  V.KFun 6 -> leave
  V.KChar _ | null mods || mods == [V.MShift] -> (d,[])
  V.KBS -> (d,[])
  V.KDel -> (d,[])
  _ -> keyEvent key mods d
  where
    move delta=moveToNode (treeSelected tree+delta)
    moveToNode i = let chosen=max 0 (min (length (treeRows tree)-1) i); visible=max 1 (snd (screenSize d)-5); scroll=max 0 (min chosen (max (treeScroll tree) (chosen-visible+1))) in (d {sideTree=Just tree {treeSelected=chosen,treeScroll=scroll}},[])
    leave=(d {sideTree=Just tree {treeFocused=False}},[])

treeMouse :: Int -> Int -> V.Button -> Sidebar -> Desktop -> (Desktop,[Effect])
treeMouse x y button tree d = case button of
  V.BLeft | x==treeWidth tree-1 -> (d {drag=Just DockSizing},[])
          | y==1 && x>=treeWidth tree-5 -> (setTree Nothing d,[])
          | y>=3 && y<snd (screenSize d)-2 -> activateTree False (treeScroll tree+y-3) d
  V.BScrollUp -> (d {sideTree=Just tree {treeScroll=max 0 (treeScroll tree-3)}},[])
  V.BScrollDown -> (d {sideTree=Just tree {treeScroll=min (max 0 (length (treeRows tree)-1)) (treeScroll tree+3)}},[])
  _ -> (d,[])

openBrowser :: FilePath -> Text -> [Entry] -> Desktop -> Desktop
openBrowser base pattern entries d = d {dialog=Just (Dialog "Open a file" (Opening base pattern entries) [Input "Name" pattern (T.length pattern),FileList entries 0] 1 ["Open","Cancel"] []),menu=Nothing,drag=Nothing,dragOriginal=Nothing}

addHelp :: Text -> Desktop -> Desktop
addHelp text d = addReadOnly "Turbo Haskell Help" text d

addReadOnly :: Text -> Text -> Desktop -> Desktop
addReadOnly title text d = case [(bid,w) | (bid,doc)<-M.toList (buffers d),documentLabel doc==Just title,w<-windows d,bufferId w==bid] of
  (bid,w):_ -> focusWindow (windowId w) d {buffers=M.adjust (\doc -> restyle doc {documentBuffer=newBuffer text}) bid (buffers d)}
  [] -> let new=addDocument Nothing (newBuffer text) d in new {buffers=M.adjust (\doc -> doc {documentLabel=Just title}) (nextId d) (buffers new)}

-- Hit testing uses the same cell geometry as selection, including tabs and wide glyphs.
hoverAt :: Int -> Int -> Desktop -> (Desktop,[Effect])
hoverAt x y d = (d {hoverTarget=target,typeHint=if target==hoverTarget d then typeHint d else "",buttonHover=hovered,contextMenu=popup},[])
  where
    hovered = dialog d >>= \dg -> findIndex (\r -> inside r x y) (buttonRects d dg)
    popup = fmap (\(r,i) -> (r,if inside r x y && y>top r && y<top r+height r-1 then y-top r-1 else i)) (contextMenu d)
    target | dialog d/=Nothing || menu d/=Nothing || contextMenu d/=Nothing || drag d/=Nothing = Nothing
           | problemsVisible d && inside (problemsRect d) x y = Nothing
           | x<treeWidthOf d = Nothing
           | otherwise = do
        w <- find (\w -> inside (bounds w) x y) (windows d)
        let Rect l t ww hh=bounds w
        if x<=l || x>=l+ww-1 || y<=t || y>=t+hh-1 then Nothing else do
          doc <- M.lookup (bufferId w) (buffers d)
          if documentLabel doc/=Nothing then Nothing else do
            let b=documentBuffer doc
                row=y-t-1+scrollRow w
                col=x-l-1+scrollColumn w
                line=bufferLineAt b row
                offset=columnOffset line col
            if row>=bufferLineCount b || col>=displayColumn line (T.length line) then Nothing
              else Just (bufferId w,revision (documentBuffer doc),bufferLineOffset b row+offset)

-- A completion (including imports) is a single undoable transaction.
applyCompletion :: [(Int,Int,Text)] -> Desktop -> Desktop
applyCompletion edits d
  | null edits || not valid = d {status="Invalid completion edits; buffer unchanged."}
  | otherwise = editActive (\_ -> replaceSelection (Selection 0 (T.length original)) changed) (Just cursor) d
  where
    original=activeText d
    ascending=sortOn (\(a,z,_) -> (a,z)) edits
    valid=all (\(a,z,_) -> a>=0 && z>=a && z<=T.length original) ascending &&
      and [z<=a' && a/=a' | ((a,z,_),(a',_,_))<-zip ascending (drop 1 ascending)]
    changed=foldr (\(a,z,text) rest -> T.take a rest<>text<>T.drop z rest) original ascending
    cursor=case edits of
      (a,_,text):_ -> a+T.length text+sum [T.length t-(z'-a') | (a',z',t)<-drop 1 edits,z'<=a]
      _ -> 0
