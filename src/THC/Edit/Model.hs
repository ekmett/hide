{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.Model where

import qualified Data.Bifunctor as Bifunctor
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as M
import qualified Data.Vector as Vec
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (find, findIndex, sortOn, mapAccumL, groupBy)
import Data.Char (toLower, isAlphaNum, chr, ord, toUpper, isHexDigit, digitToInt)
import Text.Read (readMaybe)
import System.FilePath ((</>), takeDirectory, isAbsolute, equalFilePath, splitDirectories, joinPath, normalise)
import THC.Edit.Browser (Entry(..))
import THC.Edit.Git (GitReview)
import THC.Edit.Syntax (Style(..), highlightFor)
import THC.Edit.Hex
import THC.Edit.Unicode (textInputChar)
import THC.Edit.Buffer
import THC.Edit.Files (FileState(..))

-- The same rectangles drive drawing and mouse dispatch.
data Rect = Rect { left :: Int, top :: Int, width :: Int, height :: Int } deriving (Eq,Show)
inside :: Rect -> Int -> Int -> Bool
inside (Rect x y w h) a b = a >= x && a < x+w && b >= y && b < y+h

data Document = Document { documentBuffer :: Buffer, documentFile :: Maybe FileState, documentLabel :: Maybe Text, documentHighlight :: [(Char,Style)], documentWidth :: Int, documentCursorVisible :: Bool, documentSuggestedName :: Maybe FilePath, documentSourceRows :: Maybe (Vec.Vector [(Char,Style)]) } deriving (Eq,Show)
-- Source colors are populated by the session worker, never forced by input or drawing.
newDocument :: Buffer -> Maybe FileState -> Document
newDocument b file = restyle (Document b file Nothing [] 0 True Nothing Nothing)

restyle :: Document -> Document
restyle doc = doc {documentHighlight=[],documentSourceRows=Nothing,
  documentWidth=if byteMode (documentBuffer doc) then hexWidth 16 else documentWidth doc}

syntaxDocument :: Document -> Bool
syntaxDocument doc = not (byteMode (documentBuffer doc)) &&
  maybe True (T.isPrefixOf "Source ") (documentLabel doc)

documentSyntaxPath :: Document -> FilePath
documentSyntaxPath doc = maybe (fromMaybe "Main.hs" (documentSuggestedName doc)) filePath (documentFile doc)

-- | Explicit synchronous highlighting for snapshots and deterministic pure callers.
-- Interactive rendering uses the worker-populated row index or plain text.
highlightDocument :: Document -> Document
highlightDocument doc
  | not (syntaxDocument doc) = doc
  | otherwise = doc {documentHighlight=tokens,documentSourceRows=Just (indexedHighlightRows tokens),documentWidth=measureDocumentWidth text}
  where text=contents (documentBuffer doc)
        tokens=highlightFor (documentSyntaxPath doc) text

indexedHighlightRows :: [(Char,Style)] -> Vec.Vector [(Char,Style)]
indexedHighlightRows = Vec.fromList . rows
  where rows []=[[]]
        rows xs=let (line,rest)=break ((=='\n').fst) xs in line:case rest of []->[]; _:more->rows more

measureDocumentWidth :: Text -> Int
measureDocumentWidth text=maximum (0:[displayColumn line (T.length line) | raw<-textLines text,let line=T.dropWhileEnd (=='\r') raw])

data Window = Window
  { windowId :: Int, bufferId :: Int, bounds :: Rect, selection :: Selection
  , scrollRow :: Int, scrollColumn :: Int, restoredBounds :: Maybe Rect
  , windowHexLow :: Bool, windowHexAscii :: Bool
  , windowNumber :: Int
  } deriving (Eq,Show)
data Command = New | Open | Download | ChangeDir | Save | SaveAs | Close | Quit | Undo | Redo | Cut | Copy | Paste
  | Find | FindNext | FindPrevious | Replace | GoTo | SelectAll | Zoom | NextWindow | Cascade | Tile
  | SplitVertical | SplitHorizontal | ToggleTerminalPin | About | Help | EditorOptions | Gallery
  | InspectType | Definition | Complete | Problems | NextMessage | PreviousMessage | RestartHLS | RenameSymbol | CodeActions
  | ProjectBrowser | ToggleTree | GitDiff | GitCommit | GitFetch | GitPull | GitMerge | ReviewDisk
  | CompileTarget | MakeTarget | StopBuild | RunTarget | RunOptions | OpenTerminal | StopTerminal
  | AgentChoose Text | AgentSet Text Text
  | AgentDirectory | AgentOptions | AgentPermissions | AgentGuidance | Conversation | AgentCancel | AgentResume | AgentCopyRaw | AgentNew
  | ToggleHex | GoToMessage | CopyAllMessages
  | ToolchainOptions | SelectToolchain Toolchain | SelectCompiler Text
  | DebugCommand Text
  | Disabled Text deriving (Eq,Show)
data ConflictAction = CompareDisk | ReloadDisk | KeepBuffer | SaveConflictAs deriving (Eq,Show)
data Conflict = Conflict { conflictBuffer :: Int, conflictRevision :: Int, conflictBaseline :: FileState, conflictDisk :: Maybe ByteString } deriving (Eq,Show)
data GitAction = FetchRemote | PullRemote | MergeBranch Text deriving (Eq,Show)
data Toolchain = THC | GHC deriving (Eq,Show)
data ContextKind = ToolchainContext [(Text,Command)] | SourceContext | GitContext | MessagesContext | AgentContext [(Text,Command)] deriving (Eq,Show)
data LanguageAction = TypeInfo | FindDefinition | Completions | ShowProblems | RestartLanguage | RenameAt Text | RequestCodeActions | ApplyCodeAction Int Int Text deriving (Eq,Show)
data Completion = Completion Text [(Int,Int,Text)] deriving (Eq,Show)
data ProjectAction = LoadProject | ProjectPage Int Int | ProjectDetails Int Int deriving (Eq,Show)
data Effect = ProjectRequest ProjectAction | DownloadDocument Int | ReadBrowserClipboard | WriteBrowserClipboard Text | LanguageRequest LanguageAction | RunGit GitAction | ReadMergeBranches | JumpTo FilePath Int Int | ReadPath FilePath | BrowsePath FilePath Text | BrowseDirectories FilePath | ChangeDirectory FilePath | OpenChoice FilePath Text Text | ReadTree FilePath | ExpandTree Int | ReadHelp | RefreshGit FilePath | ReadGitDiff | AskGitCommit | WriteGitCommit Text | SaveDocument Int (Maybe FilePath) (Maybe Command) | ReviewExternal | ResolveConflict Conflict ConflictAction | AgentAction Text [Text] | PermissionAction Text [Text] | DebugAction Text [Text] | SetScreenMode Int | Exit deriving (Eq,Show)
data Field = Input Text Text Int | CheckBox Text Bool | Radio Text [Text] Int | ListBox Text [Text] Int | FileList [Entry] Int
  | ReadOnly Text Text
  | TextArea Text Bool Buffer Selection Int Int deriving (Eq,Show)
data Purpose = Opening FilePath Text [Entry] | ChangingDirectory FilePath [Entry] | Committing | Saving Int (Maybe Command) | Searching Bool Text | GoingTo | Renaming
  | ProjectLoading Int | ProjectChoices Int Int
  | CodeActionChoices Int Int [Text]
  | Completing Int Int Int [Completion] | Locations [(FilePath,Int,Int)] | Merging [Text]
  | DiskConflict Conflict | AgentDialog Text | PermissionDialog Text | DebugDialog Text
  | DiscardDraft | Confirm Command | Information | Settings | Widgets deriving (Eq,Show)
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
data Drag = DockSizing | MessagesSizing | TreeScrolling | Moving Int Int Int | Resizing Int Int Int | EdgeSizing Int Bool Bool Int | Selecting Int | Scrolling Int Bool deriving (Eq,Show)
data AgentSetting = AgentSetting { settingId :: Text, settingName :: Text, settingCategory :: Text, settingCurrent :: Text, settingChoices :: [(Text,Text)] } deriving (Eq,Show)
data Appearance = LightMode | DarkMode | SystemMode deriving (Eq,Show,Enum,Bounded)

darkAppearance :: Desktop -> Bool
darkAppearance d = case appearance d of LightMode -> False; DarkMode -> True; SystemMode -> systemDark d

data ChatQuestion = ChatQuestion
  { questionToken :: Int, questionText :: Text, questionChoices :: [Text]
  , questionChoice :: Maybe Int, questionBuffer :: Buffer, questionSelection :: Selection
  , questionFocused :: Bool
  } deriving (Eq,Show)

-- Transcript documents remain in the normal buffer store when another agent is
-- selected; only the shared conversation window and composer change target.
data ConversationView = ConversationView
  { conversationBufferId :: Int, conversationName :: Text
  , conversationDraft :: Buffer, conversationDraftSelection :: Selection
  , conversationScroll :: (Int,Int), conversationReplySelection :: Selection
  } deriving (Eq,Show)

data Desktop = Desktop
  { screenSize :: (Int,Int), windows :: [Window], buffers :: M.Map Int Document
  , nextId :: Int, menu :: Maybe (Int,Int), dialog :: Maybe Dialog, drag :: Maybe Drag
  , clipboard :: Text, wordStar :: Bool, prefix :: Maybe Char, status :: Text
  , blockStart :: Maybe (Int,Int), lastFind :: Text, sideTree :: Maybe Sidebar, branchStatus :: Text, nativeMac :: Bool, gitReview :: Maybe GitReview, videoMode :: Maybe Int
  , hoverTarget :: Maybe (Int,Int,Int), typeHint :: Text
  , buttonHover :: Maybe Int, buttonPressed :: Maybe Int, contextMenu :: Maybe (Rect,Int)
  , diagnostics :: [Diagnostic], problemsVisible :: Bool, problemsSelected :: Int, problemsScroll :: Int, problemsFocused :: Bool
  , dragOriginal :: Maybe [(Int,Rect,Maybe Rect)]
  , branchAdded :: Int, branchDeleted :: Int, branchRoot :: Maybe FilePath, contextKind :: ContextKind
  , messagesNumber :: Maybe Int
  , composerBuffer :: Buffer, composerSelection :: Selection, composerFocused :: Bool, agentSteering :: Bool, agentReplying :: Bool, agentQueued :: Int
  , blinkCursor :: Bool, crtFilter :: Bool, pixelateUnicode :: Bool, materialIcons :: Bool, defaultDirectory :: Maybe FilePath, statusHover :: Maybe Int, heldModifiers :: [V.Modifier], problemsPreferredHeight :: Int, agentContextUsage :: Maybe (Integer,Integer), agentSettings :: [AgentSetting], browserFrontend :: Bool, appearance :: Appearance, systemDark :: Bool, buildDiagnostics :: [Diagnostic]
  , chatQuestion :: Maybe ChatQuestion, chatActions :: [(Int,Int,Text,[Text])], chatInputOffset :: Maybe Int
  , childAgentSettings :: [AgentSetting], childAgentSteering :: Bool, childAgentContextUsage :: Maybe (Integer,Integer)
  , conversationTarget :: Text, conversationViews :: M.Map Text ConversationView
  , streamerMode :: Bool, clipboardExport :: (Int,Maybe Text), guestPrivatePaths :: [FilePath]
  , toolchain :: Maybe Toolchain
  , dockedTerminals :: M.Map Int (Rect,Maybe Rect), bottomTerminal :: Maybe Int
  } deriving (Eq,Show)

data MenuItem = MenuItem Text Text Command deriving (Eq,Show)
menus :: [(Text,Char,[MenuItem])]
-- Docs: docs/site/screenshots/{file-menu,debug-menu}.png (docs/editing.md, docs/running.md).
-- Refresh the matching cropped popup after menu changes.
menus =
  [("File",'f',[mi "New" "" New, mi "Open..." "F3" Open, mi "Save" "F2" Save, mi "Save as..." "" SaveAs, mi "Disk changes..." "" ReviewDisk, mi "Close" "Alt+F3" Close, mi "Change dir..." "" ChangeDir, mi "Terminal" "" OpenTerminal, mi "Exit" "Alt+X" Quit])
  ,("Edit",'e',[mi "Undo" "Ctrl+Z" Undo, mi "Redo" "Ctrl+Shift+Z" Redo, mi "Cut" "Ctrl+X" Cut, mi "Copy" "Ctrl+C" Copy, mi "Paste" "Ctrl+V" Paste, mi "Select all" "Ctrl+A" SelectAll,mi "Text / hex mode" "" ToggleHex,mi "Complete identifier..." "Ctrl+Space" Complete])
  ,("Search",'s',[mi "Find..." "Ctrl+F" Find, mi "Replace..." "Ctrl+H" Replace, mi "Find next" "Ctrl+L" FindNext, mi "Find previous" "Ctrl+Shift+L" FindPrevious, mi "Go to line..." "Ctrl+G" GoTo,mi "Go to definition" "F12" Definition])
  ,("Run",'r',[mi "Run" "Ctrl+F9" RunTarget,mi "Target..." "" RunOptions,mi "Stop build/run" "" StopBuild,mi "Stop terminal" "" StopTerminal])
  ,("Compile",'c',[mi "Compile" "Alt+F9" CompileTarget,mi "Make" "F9" MakeTarget,mi "Target..." "" RunOptions,mi "Stop build" "" StopBuild])
  ,("Debug",'d',[mi "Attach..." "" (DebugCommand "attach"),mi "Launch..." "" (DebugCommand "launch"),
      mi "Toggle breakpoint" "Ctrl+F8" (DebugCommand "breakpoint"),mi "Breakpoints..." "" (DebugCommand "breakpoints"),
      mi "Continue" "F4" (DebugCommand "continue"),mi "Pause" "" (DebugCommand "pause"),
      mi "Trace into" "F7" (DebugCommand "stepIn"),mi "Step over" "F8" (DebugCommand "next"),mi "Step out" "Ctrl+F7" (DebugCommand "stepOut"),
      mi "Threads..." "" (DebugCommand "threads"),mi "Call stack..." "" (DebugCommand "stack"),mi "Scopes..." "" (DebugCommand "scopes"),
      mi "Exceptions..." "" (DebugCommand "exceptions"),mi "Exception details" "" (DebugCommand "exception-info"),mi "Output" "" (DebugCommand "output"),mi "Disconnect" "" (DebugCommand "disconnect")])
  ,("Tools",'t',[mi "File tree" "Ctrl+B" ToggleTree,mi "Git diff..." "" GitDiff,mi "Approve changes..." "" GitCommit,mi "Inspect type" "Shift+F1" InspectType,mi "Code actions..." "" CodeActions,mi "Messages" "" Problems,mi "Go to next" "Alt+F8" NextMessage,mi "Go to previous" "Alt+F7" PreviousMessage,mi "Restart language server" "" RestartHLS,mi "Conversation" "Ctrl+Shift+C" Conversation,mi "Agents..." "" AgentDirectory,mi "Conversation model..." "" (AgentChoose ""),mi "Cancel reply" "" AgentCancel,mi "Resume session..." "" AgentResume,mi "New conversation" "Ctrl+Shift+N" AgentNew,mi "Copy raw conversation" "" AgentCopyRaw,mi "Widget gallery..." "" Gallery,mi "Project browser..." "" ProjectBrowser])
  ,("Options",'o',[mi "Preferences..." "" EditorOptions,mi "Agents..." "" AgentOptions,mi "Agent Permissions" "" AgentPermissions,mi "Agent Context..." "" AgentGuidance])
  ,("Window",'w',[mi "Agents..." "" AgentDirectory,mi "Tile" "" Tile,mi "Cascade" "" Cascade,mi "Split vertically" "" SplitVertical,mi "Split horizontally" "" SplitHorizontal,mi "Zoom" "F5" Zoom,mi "Pin / unpin terminal" "" ToggleTerminalPin,mi "Next" "F6" NextWindow,mi "Close" "Alt+F3" Close])
  ,("Help",'h',[mi "Contents" "F1" Help,mi "About Turbo Haskell..." "" About])]
  where mi = MenuItem

menuMnemonic :: MenuItem -> Char
menuMnemonic (MenuItem title _ cmd) = case cmd of
  ProjectBrowser -> 'b'
  ToggleTree -> 'f'; GitDiff -> 'g'; GitCommit -> 'a'
  Problems -> 'm'; NextMessage -> 'n'; PreviousMessage -> 'p'
  SaveAs -> 'a'; Quit -> 'x'; Cut -> 't'; SelectAll -> 'a'
  SplitVertical -> 'v'; SplitHorizontal -> 'h'
  Close -> 'l'; Download -> 'w'
  _ -> toLower (T.head title)

menuShortcut :: Desktop -> MenuItem -> Text
menuShortcut d (MenuItem _ key cmd)
  | nativeMac d = fromMaybe key (lookup cmd [(New,"Cmd+N"),(Open,"Cmd+O"),(Save,"Cmd+S"),(SaveAs,"Cmd+Shift+S"),(Close,"Cmd+W"),(Quit,"Cmd+Q"),(Undo,"Cmd+Z"),(Redo,"Cmd+Shift+Z"),(Copy,"Cmd+C"),(Cut,"Cmd+X"),(Paste,"Cmd+V"),(SelectAll,"Cmd+A"),(Find,"Cmd+F"),(Replace,"Cmd+Option+F"),(FindNext,"Cmd+G"),(FindPrevious,"Cmd+Shift+G"),(Conversation,"Cmd+Shift+C"),(AgentNew,"Cmd+Shift+N")])
  | otherwise = key

commandDescription :: Command -> Text
commandDescription cmd = case cmd of
  New -> "Create a new source buffer."; Open -> "Browse directories and open a file."
  ChangeDir -> "Choose a new default directory."
  Save -> "Save the active file."; SaveAs -> "Save the active buffer under a new filename."
  GoToMessage -> "Jump to the selected message in its source file."
  CopyAllMessages -> "Copy all messages with their source locations."
  ProjectBrowser -> "Browse local Cabal components and their dependency graph without starting a build."
  ToggleHex -> "Switch between UTF-8 text and editable hexadecimal bytes."
  ReviewDisk -> "Review an external change without discarding unsaved text."
  CompileTarget -> "Compile the current source or selected project target."
  MakeTarget -> "Build the selected project target."
  StopBuild -> "Stop the current build or captured run."
  RunTarget -> "Run the selected target with THC or GHC."
  ToolchainOptions -> "Choose THC or GHC for compile, build, run and debugging."
  SelectToolchain choice -> "Use saved "<>T.pack (show choice)<>" settings."
  SelectCompiler command -> if command=="ghc" then "Use the Cabal project compiler, or GHC on PATH for standalone files." else "Use the selected installed GHC for build, run and debugging."
  RunOptions -> "Choose THC or GHC, the executable and project target."
  OpenTerminal -> "Open a project shell in a terminal window."
  StopTerminal -> "Stop the selected terminal process."
  ToggleTerminalPin -> "Pin the terminal in the bottom panel or restore its floating window."
  DebugCommand action -> case action of
    "attach" -> "Attach to a loopback Debug Adapter Protocol endpoint."
    "breakpoint" -> "Toggle a breakpoint at the current source line."
    "breakpoints" -> "Inspect breakpoint verification and remove breakpoints."
    "scopes" -> "Inspect scopes; expand variables explicitly without evaluation."
    "exception-info" -> "Inspect the stopped exception, its cause and stack."
    "disconnect" -> "Disconnect the debugger; stop editor-owned programs."
    _ -> "Debugger: " <> action
  AgentChoose _ -> "Choose the conversation model or reasoning effort."
  AgentSet _ _ -> "Apply this choice to the conversation."
  AgentDirectory -> "Inspect agents, their history and workspaces."
  AgentOptions -> "Configure agents and their executable commands."
  AgentPermissions -> "Set each agent tool to Enable, Prompt, or Disable."
  AgentGuidance -> "Edit global or project context supplied to the conversation agent."
  Conversation -> "Show the agent conversation."
  AgentCancel -> "Cancel the active agent reply."
  AgentResume -> "Resume an agent session."
  AgentNew -> "Start a new agent session."
  AgentCopyRaw -> "Copy the raw conversation text."
  Download -> "Download the current buffer, including unsaved changes."
  FindPrevious -> "Find the previous match."
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
  CodeActions -> "List HLS quick fixes and refactorings for the selected source range."
  RenameSymbol -> "Rename this symbol with HLS; review and save the changes."
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
    MenuItem _ _ cmd<-listToMaybe (drop j (menuItemsFor d i))
    pure (commandDescription cmd)

-- Labels, hit rectangles and actions share one source, including modal hints.
statusItems :: Desktop -> [(Text,Maybe (Either Command V.Event))]
statusItems d=statusHints d++[(toolchainBadgeText d,if dialog d==Nothing then Just (Left ToolchainOptions) else Nothing) | not (T.null (toolchainBadgeText d))]

statusHints :: Desktop -> [(Text,Maybe (Either Command V.Event))]
statusHints d
  | dragOriginal d/=Nothing = [(" ↑↓→← Move  Shift+↑↓→← Resize",Nothing),key "  ↵ Done" V.KEnter [],key "  Esc Cancel" V.KEsc []]
  | Just text<-menuHelp d = [command " F1 Help" Help,(" | "<>text,Nothing)]
  | Just c<-prefix d = [(" Ctrl+"<>T.singleton c<>"- ",Nothing),key " Esc Cancel" V.KEsc []]
  | Just dg<-dialog d, approvalDialog dg = [key " Tab Next" (V.KChar '\t') [],key "  Alt+A Allow" (V.KChar 'a') [V.MAlt],key "  Alt+D Deny" (V.KChar 'd') [V.MAlt],key "  Esc Deny" V.KEsc []]
  | Just dg<-dialog d, searching dg = [key " Ctrl+Tab Find/Replace" (V.KChar '\t') [V.MCtrl],key "  Tab Next" (V.KChar '\t') [],key "  Enter Apply" V.KEnter [],key "  Esc Cancel" V.KEsc []]
  | dialog d/=Nothing = [key " Tab Next" (V.KChar '\t') [],key "  Enter Select" V.KEnter [],key "  Esc Cancel" V.KEsc []]
  | problemsVisible d && problemsFocused d = [key " Enter Source" V.KEnter [],command (if nativeMac d then "  Cmd+C Copy" else "  Ctrl+C Copy") Copy,command "  Copy all" CopyAllMessages]
  | questionActive d = [key " Enter Answer" V.KEnter [],key "  Tab Choices" (V.KChar '\t') [],key "  Esc Cancel" V.KEsc []]
  | activeConversation d =
      [key (" Enter "<>(if agentReplying d then "Queue query" else "Query")) V.KEnter [],key "  Shift+Enter Newline" V.KEnter [V.MShift]] ++
      [key "  Ctrl+Enter Steer" V.KEnter [V.MCtrl] | conversationSteering d, agentReplying d] ++ [key "  Esc Cancel" V.KEsc [] | agentReplying d]
  | not (T.null (typeHint d)) = [(" "<>typeHint d,Nothing)]
  | not (T.null (status d)) = [command " F1 Help" Help,(" | "<>status d,Nothing)]
  | otherwise = [command " F1 Help" Help,command "  F2 Save" Save,command "  F3 Open" Open,
      command "  Alt+F9 Compile" CompileTarget,command "  F9 Make" MakeTarget,command "  Ctrl+F9 Run" RunTarget]
  where command label cmd=(label,Just (Left cmd)); key label k mods=(label,Just (Right (V.EvKey k mods)))

statusItemRects :: Desktop -> [(Rect,Int,Either Command V.Event)]
statusItemRects d = [(Rect x (snd (screenSize d)-1) (min (T.length text) (limit-x)) 1,i,action)
  | (i,(x,(text,Just action)))<-zip [0..] (zip starts items), x<limit] ++
  [(toolchainBadgeRect d,length items,Left ToolchainOptions) | dialog d==Nothing, not (T.null (toolchainBadgeText d))]
  where
    items=statusHints d; starts=scanl (+) 0 (map (T.length . fst) items)
    limit=left (toolchainBadgeRect d)

menuPositions :: [(Int,Int)]
menuPositions = zip starts widths
  where widths = [T.length title+2 | (title,_,_) <- menus]
        starts = scanl (+) 1 widths

menuItems :: Int -> [MenuItem]
menuItems i = let (_,_,xs) = menus !! (i `mod` length menus) in xs

menuItemsFor :: Desktop -> Int -> [MenuItem]
menuItemsFor d i
  | browserFrontend d && i==0 = take 4 items ++ [MenuItem "Download" "" Download] ++ drop 4 items
  | otherwise = items
  where items=menuItems i

commandEnabled :: Desktop -> Command -> Bool
commandEnabled d cmd | dialogCommandAllowed cmd d = True
commandEnabled d Download = browserFrontend d && maybe False ((==Nothing) . documentLabel) (activeDocument d)
commandEnabled _ Disabled{} = False
commandEnabled d ToggleTerminalPin = maybe False (terminalWindow d) (activeWindow d)
commandEnabled d cmd | cmd `elem` [Zoom,SplitVertical,SplitHorizontal], maybe False (windowPinned d) (activeWindow d) = False
commandEnabled d (AgentChoose _) = not (null (conversationSettings d))
commandEnabled d (AgentSet _ _) = not (agentReplying d) && not (null (conversationSettings d))
commandEnabled d cmd | cmd `elem` [GoToMessage,CopyAllMessages,NextMessage,PreviousMessage] = not (null (diagnostics d))
commandEnabled d Copy | problemsVisible d && problemsFocused d = not (null (diagnostics d))
commandEnabled d cmd | problemsVisible d && problemsFocused d, cmd `elem` [Undo,Redo,Cut,Paste,SelectAll] = False
commandEnabled _ _ = True
menuRect :: Desktop -> Int -> Rect
menuRect d i = Rect (min x (max 0 (sw-w))) 1 w (length (menuItemsFor d i)+2)
  where x = fst (menuPositions !! i)
        sw = fst (screenSize d)
        w = min sw (maximum [T.length t + T.length (menuShortcut d entry) + 5 | entry@(MenuItem t _ _) <- menuItemsFor d i])

initialDesktop :: (Int,Int) -> Desktop
initialDesktop size = Desktop size [] M.empty 1 Nothing Nothing Nothing "" False Nothing "" Nothing "" Nothing "" False Nothing Nothing Nothing "" Nothing Nothing Nothing [] False 0 0 False Nothing 0 0 Nothing SourceContext Nothing (newBuffer "") (Selection 0 0) True False False 0 True False False False Nothing Nothing [] 8 Nothing [] False SystemMode True [] Nothing [] Nothing [] False Nothing "" M.empty False (0,Nothing) [] Nothing M.empty Nothing

activeWindow :: Desktop -> Maybe Window
activeWindow d = listToMaybe (filter (windowVisible d) (windows d))
activeDocument :: Desktop -> Maybe Document
activeDocument d = activeWindow d >>= (\w -> M.lookup (bufferId w) (buffers d))

applicationTitle :: FilePath -> Desktop -> Text
applicationTitle cwd d = case activeDocument d of
  Nothing -> "th"
  Just doc | streamerMode d, Just file<-documentFile doc, any (equalFilePath (filePath file)) (guestPrivatePaths d) -> "th [private]"
  Just doc -> "th "<>fromMaybe name (documentLabel doc)
    where
      root=fromMaybe (maybe cwd treeRoot (sideTree d)) (defaultDirectory d)
      name=case documentFile doc of
        Just file -> T.pack (relative (filePath file))
        Nothing -> T.pack (fromMaybe ("NONAME"++maybe "" (show . bufferId) (activeWindow d)++".HS") (documentSuggestedName doc))
      relative path | not (isAbsolute path) = path
                    | otherwise = joinPath (stripCommon (splitDirectories (normalise root)) (splitDirectories (normalise path)))
      stripCommon (a:as) (b:bs) | a==b = stripCommon as bs
      stripCommon as bs = replicate (length as) ".."++bs

fitRect :: (Int,Int) -> Rect -> Rect
fitRect (sw,sh) (Rect x y w h) = Rect (max 0 (min x (sw-w'))) (max 1 (min y (sh-1-h'))) w' h'
  where w' = max 1 (min sw (max 16 w)); h' = max 1 (min (max 1 (sh-2)) (max 5 h))

addDocument :: Maybe FileState -> Buffer -> Desktop -> Desktop
addDocument file b d = d { windows = w : windows d, buffers = M.insert i (newDocument b file) (buffers d), nextId = i+1, problemsFocused=False, sideTree=fmap (\tree -> tree {treeFocused=False}) (sideTree d) }
  where
    i = nextId d
    offset = length (windows d) `mod` 5
    (sw,sh) = screenSize d
    w = Window i i (fitWindow d (Rect offset (1+offset) (sw-offset) (sh-2-offset))) (Selection 0 0) 0 0 Nothing False False (nextWindowNumber d)

nextWindowNumber :: Desktop -> Int
nextWindowNumber d = choose 1
  where used=map windowNumber (windows d)++maybe [] pure (messagesNumber d)
        choose n=if n `elem` used then choose (n+1) else n

activateWindowNumber :: Int -> Desktop -> Desktop
activateWindowNumber number d
  | Just number==messagesNumber d, problemsVisible d = ready {bottomTerminal=Nothing,problemsFocused=True,sideTree=fmap (\tree -> tree {treeFocused=False}) (sideTree d)}
  | Just w<-find ((==number) . windowNumber) (windows d) = focusWindow (windowId w) ready
  | otherwise = d
  where ready=d {menu=Nothing,contextMenu=Nothing,drag=Nothing,dragOriginal=Nothing,prefix=Nothing}

windowFocused :: Desktop -> Window -> Bool
windowFocused d w = not (problemsFocused d) && not (maybe False treeFocused (sideTree d)) &&
  fmap windowId (activeWindow d)==Just (windowId w)

focusWindow :: Int -> Desktop -> Desktop
focusWindow i d = d { bottomTerminal=if M.member i (dockedTerminals d) then Just i else bottomTerminal d, problemsFocused=False, sideTree=fmap (\tree -> tree {treeFocused=False}) (sideTree d), windows = filter ((==i) . windowId) (windows d) ++ filter ((/=i) . windowId) (windows d) }

cycleEditorWindow :: Bool -> Desktop -> Desktop
cycleEditorWindow backwards d = case orderedWindows of
  [] -> d
  ws@(w:rest) -> let rotated=if backwards then last ws:init ws else rest++[w]
                in case rotated of
                  next:_ -> focusWindow (windowId next) d {windows=rotated,menu=Nothing,contextMenu=Nothing}
                  [] -> d
  where
    orderedWindows=case activeWindow d of
      Nothing -> windows d
      Just active -> let (before,after)=break ((==windowId active).windowId) (windows d) in after++before

cycleUIFocus :: Bool -> Desktop -> Desktop
cycleUIFocus backwards d = case targets of
  [] -> d
  _ -> let index=fromMaybe 0 (findIndex (==current) targets)
           target=targets !! ((index+if backwards then -1 else 1) `mod` length targets)
           ready=d {menu=Nothing,contextMenu=Nothing,problemsFocused=False,sideTree=fmap (\tree -> tree {treeFocused=False}) (sideTree d)}
       in case target of
         0 -> ready {menu=Just (0,0)}
         -1 -> ready {sideTree=fmap (\tree -> tree {treeFocused=True}) (sideTree ready)}
         -2 -> ready {bottomTerminal=Nothing,problemsFocused=True}
         ident -> focusWindow ident ready
  where
    targets=[0]++[-1 | sideTree d/=Nothing]++map windowId (sortOn windowNumber (windows d))++[-2 | problemsVisible d]
    current | menu d/=Nothing = 0
            | maybe False treeFocused (sideTree d) = -1
            | problemsFocused d = -2
            | otherwise = maybe 0 windowId (activeWindow d)

modifyActive :: (Window -> Window) -> Desktop -> Desktop
modifyActive f d = maybe d (\w -> mapWindow (windowId w) f d) (activeWindow d)

ensureVisible :: Desktop -> Desktop
ensureVisible d = case (activeWindow d, activeDocument d) of
  (Just w, Just doc) -> modifyActive (const w { scrollRow = max 0 row', scrollColumn = max 0 col' }) d
    where
      b = documentBuffer doc
      (row,dc) = windowCursorCell b w
      rows = max 1 (windowContentRows d doc w); cols = max 1 (width (bounds w)-2)
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
      (common,oldEnd,inserted) = fromMaybe (0,0,0) (lastChange changed)
      newEnd = common+inserted
      rebase p | byteMode original/=byteMode changed = min (bufferLength changed) (modeOffset original p)
               | p <= common = p
               | p >= oldEnd = p + newEnd-oldEnd
               | otherwise = newEnd
      adjust w | bufferId w /= bid = w
               | windowId w == windowId active = w {selection = Selection target target,windowHexLow=False}
               | otherwise = w {selection = let Selection a c = selection w in Selection (rebase a) (rebase c)}
      target = max 0 (min (bufferLength changed) (fromMaybe newEnd cursor))
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
    update w = w {windowHexLow=False,selection = Selection (if extend then anchor (selection w) else p) p}

wrapMessage :: T.Text -> [T.Text]
wrapMessage text | T.null text=[]
wrapMessage text=T.take 54 text:wrapMessage (T.drop 54 text)

message :: Text -> [Text] -> Desktop -> Desktop
message title lines' d = d {dialog = Just (Dialog title Information [] 0 ["OK"] lines'), menu = Nothing, drag = Nothing,dragOriginal=Nothing}

prompt :: Text -> Purpose -> [Field] -> Desktop -> Desktop
prompt title p fs d = d {dialog = Just (Dialog title p fs 0 ["OK","Cancel"] []), menu = Nothing, drag = Nothing,dragOriginal=Nothing}

runCommand :: Command -> Desktop -> (Desktop,[Effect])
runCommand cmd source | browserFrontend source, cmd `elem` [Copy,Cut,CopyAllMessages] =
  let (next,requests)=runCommand cmd source {browserFrontend=False}
  in (next {browserFrontend=True},requests++[WriteBrowserClipboard (clipboard next)])
runCommand Paste source | browserFrontend source = (source {menu=Nothing,contextMenu=Nothing},[ReadBrowserClipboard])
runCommand cmd source | dialogCommandAllowed cmd source = case cmd of
  Find -> (searchPrompt False source,[])
  Replace -> (searchPrompt True source,[])
  _ -> handleEvent (V.EvKey (V.KChar (fromMaybe 'a' (lookup cmd [(Copy,'c'),(Cut,'x'),(Paste,'v'),(SelectAll,'a'),(Undo,'z'),(Redo,'y')]))) [V.MCtrl]) source
runCommand cmd source | problemsVisible source && problemsFocused source, cmd `elem` [Undo,Redo,Cut,Paste,SelectAll] = (source {menu=Nothing,contextMenu=Nothing},[])
runCommand Copy source | activeConversation source, Just w<-activeWindow source, anchor (selection w)/=caret (selection w) =
  (source {clipboard=conversationSelection source,status="Conversation text copied.",menu=Nothing,contextMenu=Nothing},[])
runCommand cmd source | questionActive source, cmd `elem` [Undo,Redo,Copy,Cut,Paste,SelectAll] = (questionEdit (composerCommand cmd) source,[])
runCommand cmd source | not (problemsFocused source), composerActive source, cmd `elem` [Undo,Redo,Copy,Cut,Paste,SelectAll] = (composerCommand cmd source,[])
runCommand cmd source = Bifunctor.first (clampHexScroll source) $ go cmd (source {menu = Nothing, contextMenu=Nothing, buttonHover=Nothing, buttonPressed=Nothing, prefix = Nothing, drag = Nothing,dragOriginal=Nothing})
  where
    go Download d = (d,[DownloadDocument (bufferId w) | commandEnabled d Download, Just w<-[activeWindow d]])
    go New d = (addDocument Nothing (newBuffer "") d,[])
    go Open d = (d,[BrowsePath (startingDirectory d) "*.hs"])
    go ChangeDir d = (d,[BrowseDirectories (startingDirectory d)])
    go Save d = saveRequest Nothing d
    go ReviewDisk d = (d,[ReviewExternal])
    go (DebugCommand action) d = (d,[DebugAction action []])
    go CompileTarget d = (d,[AgentAction "compile" []])
    go MakeTarget d = (d,[AgentAction "make" []])
    go StopBuild d = (d,[AgentAction "build-stop" []])
    go RunTarget d = (d,[AgentAction "run" []])
    go ToolchainOptions d | dialog d/=Nothing = (d,[])
    go ToolchainOptions d =
      let Rect x y _ _=toolchainBadgeRect d
          opened=openContext (ToolchainContext [("THC",SelectToolchain THC),("GHC Automatic",SelectCompiler "ghc"),("Target settings...",RunOptions)]) x y d
      in (opened {contextMenu=fmap (\(r,_) -> (r,if toolchain d==Just GHC then 1 else 0)) (contextMenu opened)},[AgentAction "toolchain" []])
    go (SelectToolchain choice) d = (d,[AgentAction "toolchain" [T.pack (show choice)]])
    go (SelectCompiler command) d = (d,[AgentAction "toolchain" ["GHC",command]])
    go RunOptions d = (d,[AgentAction "run-options" []])
    go OpenTerminal d = (d,[AgentAction "terminal" []])
    go StopTerminal d = (d,[AgentAction "terminal-stop" []])
    go AgentDirectory d = (d,[AgentAction "directory" []])
    go AgentOptions d = (d,[AgentAction "options" []])
    go AgentPermissions d = (d,[PermissionAction "show" []])
    go AgentGuidance d = (d,[AgentAction "context" []])
    go Conversation d = case find (\w -> maybe False ((==Just "Conversation").documentLabel) (M.lookup (bufferId w) (buffers d))) (windows d) of
      Just w -> (focusWindow (windowId w) d {composerFocused=True},[AgentAction "focus" []])
      Nothing -> (d,[AgentAction "show" []])
    go AgentCancel d = (d,[AgentAction "cancel" []])
    go AgentResume d = (d,[AgentAction "resume" []])
    go AgentNew d = (selectConversationView "" "Primary" d,[AgentAction "new" []])
    go (AgentChoose category) d = (openAgentChoices category d,[])
    go (AgentSet ident value) d = (d,[AgentAction "set-config" [ident,value]])
    go AgentCopyRaw d = (d,[AgentAction "copy" []])
    go SaveAs d = case activeWindow d of
      Nothing -> (d,[])
      Just _ | maybe False ((/=Nothing) . documentLabel) (activeDocument d) -> (d {status="This window is read-only."},[])
      Just w -> (prompt "Save file as" (Saving (bufferId w) Nothing) [Input "Name" (currentPath d) (T.length (currentPath d))] d,[])
    go Quit d = case find (dirty . documentBuffer . snd) (M.toList (buffers d)) of
      Nothing | conversationHasDraft d -> (d {dialog=Just (Dialog "Unsent query" DiscardDraft [] 0 ["Discard","Cancel"] ["Discard the unsent conversation query?"])},[])
              | otherwise -> (d,[Exit])
      Just (bid,_) -> let focused = maybe d (\w -> focusWindow (windowId w) d) (find ((==bid) . bufferId) (windows d))
                     in confirm Quit focused
    go Close d = case (activeWindow d, activeDocument d) of
      (Just w, Just doc) | dirty (documentBuffer doc) && length (filter ((==bufferId w) . bufferId) (windows d)) == 1 -> confirm Close d
      _ -> (closeActive d,[])
    go ToggleHex d = (toggleHex d,[])
    go Undo d = (editActive (const undo) Nothing d,[])
    go Redo d = (editActive (const redo) Nothing d,[])
    go GoToMessage d = jumpProblem d
    go CopyAllMessages d = copyMessages (diagnostics d) d
    go Copy d | problemsVisible d && problemsFocused d = copyMessages (take 1 (drop (problemsSelected d) (diagnostics d))) d
    go Copy d | activeHex d = (d {clipboard=T.unwords (map (hexNumber 2 . ord) (T.unpack (selected d))),status="Hex bytes copied."},[])
    go Copy d = (d {clipboard = selected d, status = "Block copied."},[])
    go Cut d | activeHex d = let copied=fst (go Copy d) in (insertText "" copied,[])
    go Cut d = (insertText "" d {clipboard = selected d},[])
    go Paste d | Just ident<-activeTerminal d = (d,[AgentAction "terminal-input" [ident,clipboard d]])
    go Paste d | activeHex d = (pasteHex (clipboard d) d,[])
    go Paste d = (insertText (clipboard d) d,[])
    go SelectAll d = (modifyActive (\w -> w {selection = Selection 0 (maybe 0 (bufferLength . documentBuffer) (activeDocument d))}) d,[])
    go action d | activeHex d, action `elem` [Find,Replace,FindNext] = (d {status="Text search is unavailable in hex mode."},[])
    go GoTo d | activeHex d = (prompt "Go to byte" GoingTo [Input "Byte offset (decimal)" "0" 1] d,[])
    go Find d = (searchPrompt False d,[])
    go Replace d = (searchPrompt True d,[])
    go FindPrevious d = (findPrevious d,[])
    go FindNext d = (findText (lastFind d) d,[])
    go GoTo d = (prompt "Go to line" GoingTo [Input "Line number" "1" 1] d,[])
    go ToggleTerminalPin d = (maybe d (\w -> setTerminalPinned (not (windowPinned d w)) (windowId w) d) (activeWindow d),[])
    go action d | action `elem` [Zoom,SplitVertical,SplitHorizontal], maybe False (windowPinned d) (activeWindow d) = (d {status="Unpin the terminal before changing its window geometry."},[])
    go Zoom d = (modifyActive zoom d,[]) where
      zoom w = case restoredBounds w of
        Just r -> w {bounds = fitWindow d r, restoredBounds = Nothing}
        Nothing -> w {bounds = let (sw,sh) = screenSize d in Rect (treeWidthOf d) 1 (sw-treeWidthOf d) (sh-2-problemsHeight d), restoredBounds = Just (bounds w)}
    go NextWindow d = (cycleEditorWindow False d,[])
    go Cascade d = (replaceFloating (zipWith cascade [0..] (floatingWindows d)) d,[]) where
      (sw,sh) = screenSize d
      cascade i w = w {bounds = fitWindow d (Rect (treeWidthOf d+i `mod` 6) (1+i `mod` 6) (sw-treeWidthOf d-6) (sh-8)), restoredBounds = Nothing}
    go Tile d = (tileWindows False d,[])
    -- Docs: docs/site/screenshots/split.png (docs/editing.md) shows shared split views.
    go SplitVertical d = splitWindow True d
    go SplitHorizontal d = splitWindow False d
    go About d = (message "About Turbo Haskell" ["Turbo Haskell  0.1", "Copyright (c) 2026 Edward Kmett", "", "Haskell source editor"] d,[])
    go InspectType d = (d,[LanguageRequest TypeInfo])
    go CodeActions d = (d,[LanguageRequest RequestCodeActions])
    go RenameSymbol d = (prompt "Rename symbol" Renaming [Input "New name" "" 0] d,[])
    go Definition d = (d,[LanguageRequest FindDefinition])
    go Complete d = (d,[LanguageRequest Completions])
    go Problems d = ((setProblemsVisible (not (messagesDisplayed d)) d) {problemsFocused=not (messagesDisplayed d)},[LanguageRequest ShowProblems])
    go NextMessage d = navigateMessage 1 d
    go PreviousMessage d = navigateMessage (-1) d
    go RestartHLS d = (d,[LanguageRequest RestartLanguage])
    go Help d = (d,[ReadHelp])
    go ToggleTree d = case sideTree d of Just _ -> (setTree Nothing d,[]); Nothing -> (d,[ReadTree (startingDirectory d)])
    go GitDiff d = (d,[ReadGitDiff])
    go GitCommit d = (d,[AskGitCommit])
    go GitFetch d = (d,[RunGit FetchRemote])
    go GitPull d = (d,[RunGit PullRemote])
    go ProjectBrowser d = (d,[ProjectRequest LoadProject])
    go GitMerge d = (d,[ReadMergeBranches])
    -- Docs: docs/site/screenshots/preferences.png (docs/display.md); refresh with the controls.
    go EditorOptions d = (prompt "Preferences" Settings
      ([Radio "Key bindings" ["Modern","WordStar"] (if wordStar d then 1 else 0)] ++
       [Radio "Screen size" ["Mode 3 (80x25)","Mode 259 (80x50)"] (if mode == 259 then 1 else 0) | Just mode <- [videoMode d]] ++
       [Radio "Appearance" ["Light","Dark","System"] (fromEnum (appearance d)),CheckBox "Blinking cursor" (blinkCursor d),CheckBox "Streamer mode" (streamerMode d)] ++
       [field | videoMode d/=Nothing,field<-[CheckBox "CRT filter" (crtFilter d),CheckBox "Pixelate Unicode" (pixelateUnicode d)]]) d,[])
    go Gallery d = (prompt "Dialog controls" Widgets [Input "Module name" "Main" 4,CheckBox "Auto indent" True,Radio "Tab width" ["4 columns","8 columns"] 1,ListBox "Source files" ["Main.hs","Types.hs","Parser.hs","Syntax.hs","Eval.hs"] 0] d,[])
    go (Disabled reason) d = (d {status = reason},[])
    confirm action d = (d {dialog = Just (Dialog "Save changes?" (Confirm action) [] 0 ["Save","Discard","Cancel"] (["Save changes to:"] ++ wrapMessage (documentTitle d <> "?")))},[])
    selected d | activeConversation d = conversationSelection d
    selected d = case (activeWindow d,activeDocument d) of (Just w,Just doc) -> selectedText (selection w) (documentBuffer doc); _ -> ""

activeText :: Desktop -> Text
activeText = maybe "" (contents . documentBuffer) . activeDocument
currentPath :: Desktop -> Text
currentPath d = maybe "" (\doc -> T.pack (maybe (fromMaybe "" (documentSuggestedName doc)) filePath (documentFile doc))) (activeDocument d)
documentTitle :: Desktop -> Text
documentTitle d = if T.null (currentPath d) then "NONAME.HS" else currentPath d

saveRequest :: Maybe Command -> Desktop -> (Desktop,[Effect])
saveRequest after d = case (activeWindow d,activeDocument d) of
  (Just _,Just doc) | documentLabel doc /= Nothing -> (d {status="This window is read-only."},[])
  (Just w,Just doc) -> case documentFile doc of
    Nothing -> (prompt "Save file as" (Saving (bufferId w) after) [Input "Name" (currentPath d) (T.length (currentPath d))] d,[])
    Just _ -> (d,[SaveDocument (bufferId w) Nothing after])
  _ -> (d,[])

closeActive :: Desktop -> Desktop
closeActive d = case activeWindow d of
  Nothing -> d
  Just w -> layoutProblems d (normalizeBottom (rememberConversationView d)
    {windows=ws,dockedTerminals=M.delete (windowId w) (dockedTerminals d),
     buffers=if any ((==bufferId w) . bufferId) ws || maybe False ((==Just "Conversation").documentLabel) (M.lookup (bufferId w) (buffers d)) then buffers d else M.delete (bufferId w) (buffers d)})
    where ws=filter ((/=windowId w).windowId) (windows d)

tileWindows :: Bool -> Desktop -> Desktop
tileWindows vertical d
  | extent `div` n < (if vertical then 16 else 5) = d {status="Not enough room to tile; enlarge the terminal."}
  | otherwise = replaceFloating (zipWith place [0..] (floatingWindows d)) d
  where
    n = max 1 (length (floatingWindows d)); (sw,sh) = screenSize d; areaWidth=sw-treeWidthOf d; areaHeight=sh-2-problemsHeight d; extent = if vertical then areaWidth else areaHeight
    place i w = w {bounds = if vertical then Rect (treeWidthOf d+start) 1 size areaHeight else Rect (treeWidthOf d) (1+start) areaWidth size, restoredBounds = Nothing}
      where start = i*extent `div` n; size = (i+1)*extent `div` n-start

splitWindow :: Bool -> Desktop -> (Desktop,[Effect])
splitWindow vertical d = case activeWindow d of
  Nothing -> (d,[])
  Just w | windowPinned d w -> (d {status="Unpin the terminal before splitting."},[])
  Just _ | (if vertical then (fst (screenSize d)-treeWidthOf d) `div` (length (floatingWindows d)+1) < 16 else (snd (screenSize d)-2-problemsHeight d) `div` (length (floatingWindows d)+1) < 5) -> (d {status="Not enough room to split; enlarge the terminal."},[])
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

findPrevious :: Desktop -> Desktop
findPrevious d | T.null (lastFind d) = d {status="Enter search text first."}
findPrevious d = case (activeWindow d,activeDocument d) of
  (Just w,Just doc) ->
    let b=documentBuffer doc
        limit=fst (ordered (selection w))
        -- Include the rest of an overlapping match whose start precedes the
        -- selection. breakOnEnd searches once instead of dropping every prefix.
        before=locate (bufferSlice b 0 (max 0 (limit+size-1)))
        found=case before of Just p -> Just p; Nothing -> locate (contents b)
    in case found of
      Nothing -> d {status="Search text not found."}
      Just p -> ensureVisible (modifyActive (\v -> v {selection=Selection p (p+size)}) d {status="Search match."})
  _ -> d
  where
    needle=lastFind d
    size=T.length needle
    locate text=let (prefix,_)=T.breakOnEnd needle text in
      if T.null prefix then Nothing else Just (T.length prefix-size)

helpLines :: [Text]
helpLines = ["F1 Help   F2 Save   F3 Open   F5 Zoom", "F6 Next window   F10 Menu   Alt+X Exit", "Alt+F3 Close   Ctrl+Z Undo   Ctrl+Shift+Z Redo", "Shift+arrows Select   Ctrl+arrows Words", "Ctrl+C/X/V Copy/Cut/Paste (internal clipboard)", "Ctrl+F Find   Ctrl+H Replace   Ctrl+L Next", "Ctrl+G Go to line   Ctrl+A Select all", "Mouse: title drag, bottom-right resize", "Window menu: tile, cascade, shared splits", "", "WordStar (Options > Editor):", "Ctrl+E/S/D/X Up/Left/Right/Down", "Ctrl+A/F Word left/right   Ctrl+Y Delete line", "Ctrl+K B/K Block start/end   C/V Copy/Cut", "Ctrl+K Y Delete block   S Save   D Close", "Ctrl+Q S/D Line start/end   R/C File top/end", "Ctrl+Q F Find   Ctrl+Q A Replace", "Escape cancels a command prefix.", "", "Tools: HLS code actions and Cabal project browser."]

searching :: Dialog -> Bool
searching dg=case purpose dg of Searching{} -> True; _ -> False

-- One dialog owns both texts. The inactive replacement is retained in its purpose.
-- doc-artifact: tools/docs-screenshots.hs find-replace -> docs/site/screenshots/find-replace.png
searchPrompt :: Bool -> Desktop -> Desktop
searchPrompt replacing d = d {dialog=Just updated,menu=Nothing,contextMenu=Nothing}
  where
    initial=Dialog "Find and Replace" (Searching False "") [Input "Text to find" (lastFind d) (T.length (lastFind d))] 0 ["Find next","Cancel"] []
    previous=case dialog d of Just dg | searching dg -> dg; _ -> initial
    oldMode=case purpose previous of Searching mode _ -> mode; _ -> False
    replacement=case fields previous of
      _:Input _ value _:_->value
      _ -> case purpose previous of Searching _ value -> value; _ -> ""
    findField=case fields previous of f:_->f; _ -> Input "Text to find" "" 0
    updated | oldMode==replacing = previous
            | otherwise=previous {purpose=Searching replacing replacement,
                fields=findField:[Input "Replace with" replacement (T.length replacement) | replacing],
                focus=if replacing then 1 else 0,buttons=[if replacing then "Replace" else "Find next","Cancel"]}

searchTabRects :: Desktop -> Dialog -> [(Rect,Bool)]
searchTabRects d dg=let Rect x y _ _=dialogRect d dg in [(Rect (x+3) (y+2) 10 1,False),(Rect (x+14) (y+2) 12 1,True)]

dialogCommandAllowed :: Command -> Desktop -> Bool
dialogCommandAllowed cmd d=case dialog d of
  Just dg | searching dg,cmd `elem` [Find,Replace] -> True
          | f:_<-drop (focus dg) (fields dg),editableArea f -> cmd `elem` [Copy,Cut,Paste,SelectAll,Undo,Redo]
  _ -> False

fieldHeight :: Field -> Int
fieldHeight Input{} = 3
fieldHeight CheckBox{} = 2
fieldHeight (Radio _ xs _) = length xs+2
fieldHeight ListBox{} = 6
fieldHeight FileList{} = 13
fieldHeight ReadOnly{} = 1
fieldHeight (TextArea _ _ b _ _ _) = min 10 (bufferLineCount b+2)

-- Keep the appearance controls visible together in the standard 80x25 mode.
dialogFieldHeight :: Dialog -> Field -> Int
dialogFieldHeight dg CheckBox{} | purpose dg==Settings = 1
dialogFieldHeight _ field = fieldHeight field

-- Shared content geometry keeps drawing, focus scrolling and hit testing aligned.
-- Preferences: input/screen controls left, appearance controls right; narrow displays stack.
dialogFieldLayout :: Int -> Int -> Dialog -> [Rect]
dialogFieldLayout w available dg
  | purpose dg==Settings && w>=54 = column 3 cw before ++ column (5+cw) cw after
  | otherwise = column 3 (max 1 (w-6)) (fields dg)
  where
    (before,after)=break (\f -> case f of Radio "Appearance" _ _ -> True; _ -> False) (fields dg)
    cw=(w-8) `div` 2
    column x fw fs=zipWith (\y f -> Rect x y fw (fieldRows f))
      (scanl (+) (2+length (body dg)+if searching dg then 2 else 0) (map fieldRows fs)) fs
    fieldRows (TextArea _ True _ _ _ _) = max 4 (available-5-length (body dg)-sum [dialogFieldHeight dg f | f<-fields dg,not (editableArea f)])
    fieldRows f = dialogFieldHeight dg f

dialogRect :: Desktop -> Dialog -> Rect
dialogRect d dg = Rect ((sw-w) `div` 2) (max 1 ((sh-h) `div` 2)) w h
  where
    (sw,sh) = screenSize d
    w = if approvalDialog dg then min sw (min (max 20 (sw-4)) 110) else min sw 62
    h = min (sh-2) (max (if searching dg then 13 else 7) (3+maximum (2+length (body dg):[top r+height r | r<-dialogFieldLayout w (sh-2) dg])))

fieldRects :: Desktop -> Dialog -> [Rect]
fieldRects d dg = [r {left=x+left r,top=y+top r-offset} | r<-layout]
  where
    Rect x y w h = dialogRect d dg
    layout=dialogFieldLayout w h dg
    offset = case drop (focus dg) layout of
      r:_ -> max 0 (min (top r-2) (top r+height r-(h-3)))
      _ -> 0

approvalDialog :: Dialog -> Bool
approvalDialog dg = case purpose dg of PermissionDialog action -> "approve:" `T.isPrefixOf` action; _ -> False

editableArea :: Field -> Bool
editableArea (TextArea _ editable _ _ _ _) = editable
editableArea _ = False

-- The text area uses the ordinary buffer/editor operations in a private view;
-- it never enters the desktop buffer map, recovery checkpoint or agent inventory.
textAreaRect :: Rect -> Field -> Rect
textAreaRect (Rect x y w h) (TextArea _ editable _ _ _ _) =
  if editable then Rect x (y+1) (max 1 (w-1)) (max 1 (h-2))
  else Rect (x+labelWidth) y (max 1 (w-labelWidth-1)) (max 1 (h-1))
  where labelWidth=min 18 (w `div` 3)
textAreaRect r _ = r

textAreaEdit :: Rect -> (Desktop -> Desktop) -> Field -> Field
textAreaEdit rect edit field@(TextArea name True b sel row col) =
  case (activeDocument changed,activeWindow changed) of
    (Just doc,Just w) -> TextArea name True (documentBuffer doc) (selection w) (scrollRow w) (scrollColumn w)
    _ -> field
  where
    area=textAreaRect rect field
    base=addDocument Nothing b (initialDesktop (width area+2,height area+4))
    view=modifyActive (\w -> w {bounds=Rect 0 1 (width area+2) (height area+2),selection=sel,scrollRow=row,scrollColumn=col}) base
    changed=edit view
textAreaEdit _ _ field = field

dialogCloseRect :: Desktop -> Dialog -> Rect
dialogCloseRect d dg = let Rect x y w _=dialogRect d dg in Rect (x+w-5) y 3 1

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
resizeScreenMode (sw,sh) d = layoutBottomWindows (ensureVisible (clampHexScroll d resized {windows=map stretch (windows d)}))
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
handleEvent (V.EvMouseDown x y V.BLeft _) d | y==snd (screenSize d)-1 =
  case find (\(rect,_,_)->inside rect x y) (statusItemRects d) of
    Just (_,_,Left cmd) -> runCommand cmd d
    Just (_,_,Right event) -> handleEvent event (if activeConversation d then d {composerFocused=True} else d)
    Nothing -> (d,[])
handleEvent event d = Bifunctor.first (layoutComposer d . clampHexScroll d) $ dispatchEvent event (case event of
  V.EvKey{} -> d {hoverTarget=Nothing,typeHint="",buttonHover=Nothing,buttonPressed=Nothing,statusHover=Nothing}
  V.EvMouseDown{} -> d {hoverTarget=Nothing,typeHint=""}
  V.EvPaste{} -> d {hoverTarget=Nothing,typeHint=""}
  _ -> d)

dispatchEvent :: V.Event -> Desktop -> (Desktop,[Effect])
dispatchEvent (V.EvResize sw sh) d =
  (layoutBottomWindows resized {windows=map resize (windows d),drag=Nothing,dragOriginal=Nothing,
    menu=Nothing,contextMenu=Nothing,buttonHover=Nothing,buttonPressed=Nothing},[])
  where
    resized=d {screenSize=(max 1 sw,max 3 sh),
      sideTree=fmap (\t -> t {treeWidth=min (treeWidth t) (max 0 (sw-16))}) (sideTree d)}
    -- Attachment is to the usable desktop: Messages owns the bottom strip.
    oldRight=fst (screenSize d); newRight=fst (screenSize resized)
    oldBottom=top (problemsRect d); newBottom=top (problemsRect resized)
    stretch r=fitWindow resized r
      {width=if left r+width r==oldRight then newRight-left r else width r,
       height=if top r+height r==oldBottom then newBottom-top r else height r}
    resize w=w {bounds=stretch (bounds w),restoredBounds=fmap stretch (restoredBounds w)}
dispatchEvent (V.EvKey key mods) d | key `elem` [V.KChar '\t',V.KBackTab], V.MAlt `elem` mods =
  case dialog d of
    Just dg -> dialogEvent (V.EvKey (V.KChar '\t') [V.MShift | backwards]) dg d
    Nothing -> (cycleUIFocus backwards d,[])
  where backwards=key==V.KBackTab || V.MShift `elem` mods
dispatchEvent (V.EvKey key mods) d | dialog d==Nothing, key `elem` [V.KChar '\t',V.KBackTab], V.MCtrl `elem` mods =
  (cycleEditorWindow (key==V.KBackTab || V.MShift `elem` mods) d,[])
dispatchEvent (V.EvKey (V.KChar c) mods) d | dialog d==Nothing, V.MAlt `elem` mods, c>='1', c<='9' = (activateWindowNumber (fromEnum c-fromEnum '0') d,[])
dispatchEvent (V.EvKey (V.KFun key) mods) d | dialog d==Nothing, V.MAlt `elem` mods, key `elem` [7,8] = runCommand (if key==8 then NextMessage else PreviousMessage) d
dispatchEvent (V.EvKey (V.KFun 9) []) d | dialog d==Nothing = runCommand MakeTarget d
dispatchEvent (V.EvKey (V.KFun 9) [V.MAlt]) d | dialog d==Nothing = runCommand CompileTarget d
dispatchEvent (V.EvKey (V.KFun 9) [V.MCtrl]) d | dialog d==Nothing = runCommand RunTarget d
dispatchEvent ev d | Just dg <- dialog d = dialogEvent ev dg d
dispatchEvent ev d | Just popup <- contextMenu d = contextEvent ev popup d
dispatchEvent ev d | Just m <- menu d = menuEvent ev m d
dispatchEvent (V.EvKey key mods) d | Just _ <- dragOriginal d = dragKey key mods d
dispatchEvent (V.EvKey key mods) d | problemsVisible d && problemsFocused d = problemsKey key mods d
dispatchEvent (V.EvKey (V.KFun key) mods) d
  | Just action <- lookup (key,mods) [((4,[]),"continue"),((7,[]),"stepIn"),((8,[]),"next"),((7,[V.MCtrl]),"stepOut"),((8,[V.MCtrl]),"breakpoint")] = runCommand (DebugCommand action) d
dispatchEvent ev d | questionActive d, Just result<-questionEvent ev d = result
dispatchEvent ev d | activeConversation d, Just result<-composerEvent ev d = result
dispatchEvent ev d | Just ident<-activeTerminal d,Just text<-terminalInput ev = (d,[AgentAction "terminal-input" [ident,text]])
dispatchEvent (V.EvMouseUp _ _ _) d = (d {drag = Nothing,dragOriginal=Nothing},[])
dispatchEvent (V.EvMouseDown x y button mods) d = mouseEvent x y button mods d
dispatchEvent (V.EvPaste bytes) d | activeHex d = (either (const (d {status="Paste hexadecimal text."})) (`pasteHex` d) (TE.decodeUtf8' bytes),[])
dispatchEvent (V.EvPaste bytes) d = case TE.decodeUtf8' bytes of
  Left _ -> (message "Paste failed" ["The pasted text is not valid UTF-8."] d,[])
  Right t -> (insertText (T.filter (\c -> textInputChar c || c `elem` ['\n','\r','\t']) t) d,[])
dispatchEvent (V.EvKey key mods) d | Just tree <- sideTree d, treeFocused tree = treeKey key mods tree d
dispatchEvent (V.EvKey key mods) d = keyEvent key mods d
dispatchEvent _ d = (d,[])

conversationDocument :: Text -> Desktop -> Maybe (Int,Document)
conversationDocument target d = case M.lookup target (conversationViews d) of
  Just view -> (conversationBufferId view,) <$> M.lookup (conversationBufferId view) (buffers d)
  Nothing | T.null target -> find (\(bid,doc)->documentLabel doc==Just "Conversation" && all ((/=bid).conversationBufferId) (M.elems (conversationViews d))) (M.toList (buffers d))
          | otherwise -> Nothing

rememberConversationView :: Desktop -> Desktop
rememberConversationView d = case conversationDocument (conversationTarget d) d of
  Nothing -> d
  Just (bid,_) ->
    let old=M.lookup (conversationTarget d) (conversationViews d)
        win=find ((==bid).bufferId) (windows d)
        view=ConversationView bid (maybe "Primary" conversationName old) (composerBuffer d) (composerSelection d)
          (maybe (maybe (0,0) conversationScroll old) (\w->(scrollRow w,scrollColumn w)) win)
          (maybe (maybe (Selection 0 0) conversationReplySelection old) selection win)
    in d {conversationViews=M.insert (conversationTarget d) view (conversationViews d)}

addConversationDocument :: Desktop -> Desktop
addConversationDocument d=let added=addDocument Nothing (newBuffer "") d in
  added {buffers=M.adjust (\doc->doc {documentLabel=Just "Conversation",documentCursorVisible=False}) (nextId d) (buffers added)}

selectConversationView :: Text -> Text -> Desktop -> Desktop
selectConversationView target name original =
  let saved=rememberConversationView original
      existingWindow=find (\w->maybe False ((==Just "Conversation").documentLabel) (M.lookup (bufferId w) (buffers saved))) (windows saved)
      (bid,prepared0)=case conversationDocument target saved of
        Just (ident,_) -> (ident,saved)
        Nothing -> let added=addConversationDocument saved in (nextId saved,added)
      prepared=case existingWindow of
        Nothing | not (any ((==bid).bufferId) (windows prepared0)) ->
          let added=addConversationDocument prepared0
          in added {buffers=M.delete (nextId prepared0) (buffers added),windows=case windows added of
            new:rest -> new {bufferId=bid}:rest
            [] -> []}
        _ -> prepared0
      old=M.lookup target (conversationViews prepared)
      view=maybe (ConversationView bid name (newBuffer "") (Selection 0 0) (0,0) (Selection 0 0)) (\v->v {conversationName=name}) old
      adjusted w=w {bufferId=bid,scrollRow=fst (conversationScroll view),scrollColumn=snd (conversationScroll view),selection=conversationReplySelection view}
      oldWindows=case existingWindow of
        Just _ -> windows saved
        Nothing -> windows prepared
      views=map (\w->if maybe False ((==Just "Conversation").documentLabel) (M.lookup (bufferId w) (buffers prepared)) then adjusted w else w) oldWindows
      result=prepared {windows=views,childAgentSettings=[],childAgentSteering=False,childAgentContextUsage=Nothing,conversationTarget=target,conversationViews=M.insert target view (conversationViews prepared),
        composerBuffer=conversationDraft view,composerSelection=conversationDraftSelection view,composerFocused=True,
        chatActions=[],chatInputOffset=Nothing,contextMenu=Nothing,menu=Nothing}
  in maybe result (\w->focusWindow (windowId w) result) (find ((==bid).bufferId) views)

conversationHasDraft :: Desktop -> Bool
conversationHasDraft d=not (T.null (contents (composerBuffer d))) ||
  any (\(target,view)->target/=conversationTarget d && not (T.null (contents (conversationDraft view)))) (M.toList (conversationViews d))

activeConversation :: Desktop -> Bool
activeConversation d = maybe False (windowFocused d) (activeWindow d) && maybe False ((==Just "Conversation").documentLabel) (activeDocument d)

-- Rendered cells carry message identity only for text, never bubble furniture.
conversationSelection :: Desktop -> Text
conversationSelection d = case (activeWindow d,activeDocument d) of
  (Just w,Just doc) | documentLabel doc==Just "Conversation" ->
    let (a,z)=ordered (selection w)
        cells=[(ident,outgoing,c) | (c,BubbleText ident outgoing _)<-take (z-a) (drop a (documentHighlight doc))]
        groups=groupBy (\(i,_,_) (j,_,_) -> i==j) cells
        render group@((_,outgoing,_):_)=
          (if length groups>1 && T.null (conversationTarget d) then if outgoing then "User: " else "Bot: " else "")<>
          T.pack [c | (_,_,c)<-group]
        render []=""
    in T.intercalate "\n\n" (map render groups)
  _ -> ""

clearReplySelection :: Desktop -> Desktop
clearReplySelection d = if activeConversation d then modifyActive (\w -> w {selection=Selection 0 0}) d else d

composerActive :: Desktop -> Bool
composerActive d = activeConversation d && composerFocused d && not (questionActive d)

-- Preserve the last visible reply when the draft grows; browsing older replies
-- keeps its position. Every viewport calculation uses the same draft height.
layoutComposer :: Desktop -> Desktop -> Desktop
layoutComposer before after = after {windows=map adjust (windows after)}
  where
    adjust w | Just doc<-M.lookup (bufferId w) (buffers after), documentLabel doc==Just "Conversation",
               height (composerRect before w)/=height (composerRect after w) =
      let oldLimit=scrollbarLimit before True doc w
          newLimit=scrollbarLimit after True doc w
      in w {scrollRow=if scrollRow w>=oldLimit then newLimit else min newLimit (scrollRow w)}
    adjust w=w

composerRect :: Desktop -> Window -> Rect
composerRect d w = Rect (x+ww-4-columns) (y+hh-1-rows) columns rows
  where
    Rect x y ww hh=bounds w
    draft=composerBuffer d
    rows=min (min 12 (bufferLineCount draft)) (max 0 (hh-6))
    columns=min (max 0 (ww-6)) (max 12 (longest+1))
    longest=maximum (0:[displayColumn line (T.length line) | line<-textLines (contents draft)])

composerSubmit :: [V.Modifier] -> Desktop -> (Desktop,[Effect])
composerSubmit mods d
  | V.MCtrl `elem` mods = (d,[AgentAction "steer-draft" []])
  | V.MShift `elem` mods = (composerInsert "\n" d,[])
  | otherwise = (d,[AgentAction "send-draft" []])

windowContentRows :: Desktop -> Document -> Window -> Int
windowContentRows d doc w = max 0 (height (bounds w)-2-if documentLabel doc==Just "Conversation" then height (composerRect d w)+1 else 0)

composerScroll :: Desktop -> Window -> (Int,Int)
composerScroll d w = (max 0 (r-height rect+1),max 0 (displayColumn (bufferLineAt b r) c-width rect+1))
  where b=composerBuffer d; (r,c)=bufferLineColumn b (caret (composerSelection d)); rect=composerRect d w

composerClick :: Int -> Int -> [V.Modifier] -> Window -> Desktop -> Desktop
composerClick x y mods w d = clearReplySelection d {chatQuestion=fmap (\q->q {questionFocused=False}) (chatQuestion d),composerFocused=True,composerSelection=Selection (if V.MShift `elem` mods then anchor (composerSelection d) else p) p}
  where
    Rect l t _ _=composerRect d w; (sr,sc)=composerScroll d w; b=composerBuffer d
    r=min (bufferLineCount b-1) (max 0 (y-t+sr))
    p=bufferLineOffset b r+columnOffset (bufferLineAt b r) (max 0 (x-l+sc))

composerInsert :: Text -> Desktop -> Desktop
composerInsert text d = clearReplySelection d {composerBuffer=replaceSelection sel text (composerBuffer d),composerSelection=Selection p p,composerFocused=True}
  where sel=composerSelection d; p=fst (ordered sel)+T.length text

composerCommand :: Command -> Desktop -> Desktop
composerCommand cmd d = case cmd of
  Copy -> d {clipboard=selectedText sel b}
  Cut -> composerInsert "" d {clipboard=selectedText sel b}
  Paste -> composerInsert (clipboard d) d
  SelectAll -> clearReplySelection d {composerSelection=Selection 0 (bufferLength b)}
  Undo -> history undo
  Redo -> history redo
  _ -> d
  where
    b=composerBuffer d; sel=composerSelection d
    history f=let changed=f b; p=min (bufferLength changed) (caret sel)
              in d {composerBuffer=changed,composerSelection=Selection p p}

composerEvent :: V.Event -> Desktop -> Maybe (Desktop,[Effect])
composerEvent (V.EvPaste bytes) d = Just (either (const d) (\text -> composerInsert (T.filter (\c -> textInputChar c || c `elem` ['\n','\r','\t']) text) d) (TE.decodeUtf8' bytes),[])
composerEvent (V.EvKey key mods) d
  | key==V.KEsc, composerFocused d, agentReplying d = Just (d,[AgentAction "cancel" []])
  | key==V.KChar '\t', null mods = Just (d {composerFocused=not (composerFocused d)},[])
  | key==V.KEnter, composerFocused d, all (`elem` [V.MCtrl,V.MShift]) mods = Just (composerSubmit mods d)
  | V.KChar c<-key, textInputChar c, null mods || mods==[V.MShift] = done (composerInsert (T.singleton c) d)
  | not (composerFocused d) || V.MAlt `elem` mods || V.MMeta `elem` mods = Nothing
  | ctrl, V.KChar c<-key, toLower c=='z', V.MShift `elem` mods = Just (runCommand Redo d)
  | ctrl, V.KChar c<-key, Just cmd<-lookup (toLower c) [('c',Copy),('x',Cut),('v',Paste),('a',SelectAll),('z',Undo),('y',Redo)] = Just (runCommand cmd d)
  | otherwise = case key of
      V.KLeft -> move (if ctrl then wordLeft text p else previousCharacter text p)
      V.KRight -> move (if ctrl then wordRight text p else nextCharacter text p)
      V.KUp -> vertical (-1)
      V.KDown -> vertical 1
      V.KHome -> move (if ctrl then 0 else bufferLineOffset b r)
      V.KEnd -> move (if ctrl then bufferLength b else bufferLineOffset b r+T.length (bufferLineAt b r))
      V.KBS -> erase (if ctrl then wordLeft text p else previousCharacter text p) p
      V.KDel -> erase p (if ctrl then wordRight text p else nextCharacter text p)
      _ -> Nothing
  where
    done next=Just (next,[])
    b=composerBuffer d; text=contents b; sel=composerSelection d; p=caret sel
    (r,column)=bufferLineColumn b p; ctrl=V.MCtrl `elem` mods
    move n=let q=max 0 (min (bufferLength b) n) in done d {composerSelection=Selection (if V.MShift `elem` mods then anchor sel else q) q}
    vertical delta=let row=max 0 (min (bufferLineCount b-1) (r+delta))
                   in move (bufferLineOffset b row+columnOffset (bufferLineAt b row) (displayColumn (bufferLineAt b r) column))
    erase a z=done (composerInsert "" d {composerSelection=if anchor sel/=p then sel else Selection a z})
composerEvent _ _ = Nothing

-- Inline questions have their own editing state; the ordinary draft is never
-- borrowed or cleared while a tool waits for a human response.
questionActive :: Desktop -> Bool
questionActive d=T.null (conversationTarget d) && activeConversation d && maybe False questionFocused (chatQuestion d)

questionEdit :: (Desktop -> Desktop) -> Desktop -> Desktop
questionEdit edit d=case chatQuestion d of
  Nothing -> d
  Just q -> let temporary=d {chatQuestion=Nothing,composerBuffer=questionBuffer q,composerSelection=questionSelection q,composerFocused=True}
                changed=edit temporary
                bounded=T.take 4096 (T.map (\c->if c `elem` ['\n','\r','\t'] then ' ' else c) (contents (composerBuffer changed)))
                b=if bounded==contents (composerBuffer changed) then composerBuffer changed else newBuffer bounded
                bound n=max 0 (min (T.length bounded) n)
                sel=composerSelection changed
            in d {clipboard=clipboard changed,chatQuestion=Just q {questionChoice=Nothing,questionFocused=True,questionBuffer=b,
                questionSelection=Selection (bound (anchor sel)) (bound (caret sel))}}

questionInputStart :: Int -> ChatQuestion -> Int
questionInputStart width q=columnOffset text (max 0 (displayColumn text (caret (questionSelection q))-max 1 (width-9)+1))
  where text=contents (questionBuffer q)

questionVisibleInput :: Int -> ChatQuestion -> Text
questionVisibleInput width q=T.take (columnOffset suffix (max 1 (width-8))) suffix
  where suffix=T.drop (questionInputStart width q) (contents (questionBuffer q))

questionEvent :: V.Event -> Desktop -> Maybe (Desktop,[Effect])
questionEvent event d=case chatQuestion d of
  Nothing -> Nothing
  Just q -> case event of
    V.EvKey V.KEsc [] -> send "question-cancel" q
    V.EvKey V.KEnter [] -> send "question-submit" q
    V.EvKey V.KUp [] -> choose (-1) q
    V.EvKey V.KDown [] -> choose 1 q
    V.EvKey (V.KChar '\t') [] -> choose 1 q
    V.EvKey V.KBackTab [] -> choose (-1) q
    V.EvMouseDown{} -> Nothing
    V.EvMouseUp{} -> Nothing
    _ -> let temporary=d {chatQuestion=Nothing,composerBuffer=questionBuffer q,composerSelection=questionSelection q,composerFocused=True}
         in case composerEvent event temporary of
           Just (_,effects) | not (null effects) -> Just (d,[])
           Just _ -> Just (questionEdit (\state->maybe state fst (composerEvent event state)) d,[])
           Nothing -> Nothing
  where
    send action q=Just (d,[AgentAction action [T.pack (show (questionToken q))]])
    choose delta q=let count=length (questionChoices q)+1
                       index=(maybe 0 (+1) (questionChoice q)+delta+count) `mod` count
                   in Just (d {chatQuestion=Just q {questionChoice=if index==0 then Nothing else Just (index-1)}},[])

conversationClick :: Int -> Int -> Window -> Desktop -> Maybe Effect
conversationClick x y w d=do
  doc<-M.lookup (bufferId w) (buffers d)
  let row=y-top (bounds w)-1+scrollRow w
      b=documentBuffer doc
      column=columnOffset (bufferLineAt b row) (x-left (bounds w)-1+scrollColumn w)
      offset=bufferLineOffset b row+column
  if y<=top (bounds w) || y>=top (composerRect d w) || x<=left (bounds w) || x>=left (bounds w)+width (bounds w)-1 then Nothing
  else case find (\(a,z,_,_)->offset>=a && offset<z) (chatActions d) of
    Just (start,_,action,values) -> Just (AgentAction action (values++[T.pack (show (max 0 (offset-start-7))) | action=="question-input"]))
    Nothing -> Nothing

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
  V.EvKey (V.KChar c) _ -> case findIndex (\item -> toLower c == menuMnemonic item) (menuItemsFor d i) of
    Just k -> invoke k
    _ -> (d,[])
  V.EvMouseDown x 0 V.BLeft _ -> case menuAt x of Just k -> choose k 0; _ -> (d {menu=Nothing},[])
  V.EvMouseDown x y V.BLeft _ -> let r = menuRect d i in if inside r x y && y>top r && y<top r+height r-1 then invoke (y-top r-1) else (d {menu=Nothing},[])
  _ -> (d,[])
  where
    choose a b = let a' = a `mod` length menus in (d {menu = Just (a',b `mod` length (menuItemsFor d a'))},[])
    invoke k = let MenuItem _ _ command = menuItemsFor d i !! k in runCommand command d

menuAt :: Int -> Maybe Int
menuAt x = findIndex (\(start,w) -> x >= start && x < start+w) menuPositions

contextItems :: ContextKind -> [(Text,Command)]
contextItems (ToolchainContext items) = items
contextItems SourceContext = [("Rename symbol...",RenameSymbol),("Code actions...",CodeActions),("Go to definition",Definition),("Inspect type",InspectType),("Complete identifier",Complete)]
contextItems MessagesContext = [("Go to source",GoToMessage),("Copy message",Copy),("Copy all messages",CopyAllMessages),("Hide Messages",Problems)]
contextItems (AgentContext items) = items
contextItems GitContext = [("Pull",GitPull),("Fetch",GitFetch),("Merge...",GitMerge)]

conversationSettings :: Desktop -> [AgentSetting]
conversationSettings d=if T.null (conversationTarget d) then agentSettings d else childAgentSettings d
conversationSteering :: Desktop -> Bool
conversationSteering d=if T.null (conversationTarget d) then agentSteering d else childAgentSteering d
conversationContextUsage :: Desktop -> Maybe (Integer,Integer)
conversationContextUsage d=if T.null (conversationTarget d) then agentContextUsage d else childAgentContextUsage d

conversationTitle :: Desktop -> Text
conversationTitle d = prefix<>case [settingCurrent option | option<-settings,settingCategory option=="model"] of
  model:_ -> model<>case [settingCurrent option | option<-settings,settingCategory option=="thought_level"] of
    effort:_ -> " ("<>effort<>") ▼"
    [] -> " ▼"
  [] -> if T.null target then if null settings then "Conversation" else "Conversation ▼" else if null settings then name else "Settings ▼"
  where
    target=conversationTarget d
    settings=conversationSettings d
    name=maybe "Agent conversation" conversationName (M.lookup target (conversationViews d))
    prefix=if T.null target || null settings then "" else name<>" · "

agentTitleRect :: Desktop -> Window -> Rect
agentTitleRect d w = Rect (x+max 6 ((ww-T.length title) `div` 2)) y (max 0 (min (T.length title) (ww-17-count))) 1
  where
    Rect x y ww _=bounds w
    title=" "<>conversationTitle d<>" "
    count=T.length (T.pack (show (windowNumber w)))

openAgentChoices :: Text -> Desktop -> Desktop
openAgentChoices category d
  | null items = d {status="The provider has not advertised model settings."}
  | otherwise = openContext (AgentContext items) x (y+1) d
  where
    Rect x y _ _=maybe (Rect 1 1 0 0) (agentTitleRect d) (activeWindow d)
    items | T.null category = [(settingName option<>"  "<>settingCurrent option<>" ►",AgentChoose (settingId option)) | option<-conversationSettings d]
          | otherwise = [(if value==settingCurrent option then "✓ "<>name else "  "<>name,AgentSet category value)
                        | option<-conversationSettings d,settingId option==category,(value,name)<-settingChoices option]

contextOffset :: Rect -> Int -> Int
contextOffset r chosen = let count=max 1 (height r-2) in chosen `div` count*count

openContext :: ContextKind -> Int -> Int -> Desktop -> Desktop
openContext kind x y d = d {contextKind=kind,contextMenu=Just (popup,0),drag=Nothing,dragOriginal=Nothing,menu=Nothing}
  where
    (sw,sh)=screenSize d
    items=contextItems kind
    h=max 3 (min (sh-2) (length items+2))
    w=min sw (max 24 (maximum (0:map (T.length . fst) items)+4))
    popup=Rect (max 0 (min x (sw-w))) (max 1 (min y (sh-h-1))) w h

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

-- Docs: docs/site/screenshots/toolchain.png (docs/running.md), including the popup.
toolchainBadgeText :: Desktop -> Text
toolchainBadgeText d | fst (screenSize d)<20 = ""
                     | otherwise = " "<>T.pack (show (fromMaybe THC (toolchain d)))<>" ▼ "

toolchainBadgeRect :: Desktop -> Rect
toolchainBadgeRect d = Rect (max 0 (sw-gitWidth-len)) (sh-1) len 1
  where
    (sw,sh)=screenSize d
    gitWidth=if activeConversation d then 0 else T.length (gitBadgeText d)
    len=T.length (toolchainBadgeText d)

gitBadgeRect :: Desktop -> Rect
gitBadgeRect d = let (sw,sh)=screenSize d; len=T.length (gitBadgeText d) in Rect (max 0 (sw-len)) (sh-1) (min sw len) 1

contextEvent :: V.Event -> (Rect,Int) -> Desktop -> (Desktop,[Effect])
contextEvent ev (r,chosen) d = case ev of
  V.EvKey V.KEsc _ -> close
  V.EvKey V.KUp _ -> choose (chosen-1)
  V.EvKey V.KDown _ -> choose (chosen+1)
  V.EvKey V.KEnter _ -> invoke chosen
  V.EvKey V.KPageDown _ -> choose (chosen+max 1 (height r-2))
  V.EvKey V.KPageUp _ -> choose (chosen-max 1 (height r-2))
  V.EvMouseDown x y V.BLeft _
    | inside r x y && y>top r && y<top r+height r-1 -> invoke (contextOffset r chosen+y-top r-1)
    | otherwise -> close
  V.EvMouseDown x y V.BRight mods -> mouseEvent x y V.BRight mods d {contextMenu=Nothing}
  _ -> (d,[])
  where
    close=(d {contextMenu=Nothing},[])
    choose i=(d {contextMenu=Just (r,i `mod` length items)},[])
    invoke i=case drop i items of (_,cmd):_ | commandEnabled d cmd -> runCommand cmd d; _ -> close
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
  Nothing | menu d==Nothing, contextMenu d==Nothing, messagesDisplayed d,
            let r=problemsRect d, inside r x y, y>top r, y<top r+height r-1,
            problemsScroll d+y-top r-1<length (diagnostics d) ->
    jumpProblem (fst (problemsMouse x y V.BLeft d))
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
  MessagesSizing -> resizeProblems y d
  TreeScrolling -> case sideTree d of
    Just tree -> scrollTreeTo ((y-3)*treeScrollLimit d tree `div` max 1 (treeContentRows d-3)) tree d
    Nothing -> d
  Moving i dx dy -> moveWindow i (x-dx) (y-dy) d
  Resizing i dx dy -> case find ((==i).windowId) (windows d) of
    Just w -> resizeWindowBounds i (bounds w) {width=x-left (bounds w)+dx,height=y-top (bounds w)+dy} d
    Nothing -> d
  EdgeSizing i vertical leading offset -> resizeWindowEdge i vertical leading ((if vertical then y else x)+offset) d
  Scrolling i vertical -> scrollTrack vertical x y (focusWindow i d)
  Selecting i -> selectAt True x y (focusWindow i d),[])
mouseEvent x 0 V.BLeft _ d = (d {menu = (\i -> (i,0)) <$> menuAt x},[])
mouseEvent x y V.BRight _ d | inside (gitBadgeRect d) x y = (openContext GitContext x y d,[])
mouseEvent x y button mods d | bottomVisible d, inside (problemsRect d) x y = bottomMouse x y button mods d
mouseEvent x y button _ d | Just tree <- sideTree d, x < treeWidth tree = treeMouse x y button tree d {problemsFocused=False}
mouseEvent x y button mods d = windowMouse x y button mods d

windowMouse :: Int -> Int -> V.Button -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
windowMouse x y button mods d = case find (\w -> windowVisible d w && inside (bounds w) x y) (windows d) of
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
      | y==t && terminalWindow focused w && x>=l+6 && x<=l+8 -> runCommand ToggleTerminalPin focused
      | windowPinned d w && (x==l || x==l+ww-1 || y==t || y==t+hh-1) -> (focused,[])
      | y==t && x>=l+ww-6 && x<l+ww-3 -> runCommand Zoom focused
      | y==t, activeConversation focused, not (null (conversationSettings focused)), inside (agentTitleRect focused w) x y -> runCommand (AgentChoose "") focused
      | y==t && (x==l || x==l+ww-1) -> (beginWindowDrag (EdgeSizing (windowId w) True True 0) focused,[])
      | y==t -> (beginWindowDrag (Moving (windowId w) (x-l) (y-t)) focused,[])
      | x>=l+ww-2 && y==t+hh-1 -> (beginWindowDrag (Resizing (windowId w) (l+ww-x) (t+hh-y)) focused,[])
      | Just doc<-activeDocument focused, inside (scrollbarRect focused True doc w) x y -> (scrollClick True x y focused,[])
      | Just doc<-activeDocument focused, inside (scrollbarRect focused False doc w) x y -> (scrollClick False x y focused,[])
      | x==l -> (beginWindowDrag (EdgeSizing (windowId w) False True 0) focused,[])
      | x==l+ww-1 -> (beginWindowDrag (EdgeSizing (windowId w) False False 1) focused,[])
      | y==t+hh-1 -> (beginWindowDrag (EdgeSizing (windowId w) True False 1) focused,[])
      | activeConversation focused, Just action<-conversationClick x y w focused -> (focused {drag=Nothing},[action])
      | activeConversation focused, inside (composerRect focused w) x y -> (composerClick x y mods w focused,[])
      | activeConversation focused, y>=top (composerRect focused w) -> (focused,[])
      | otherwise -> (selectAt (V.MShift `elem` mods) x y focused {drag=Just (Selecting (windowId w)),composerFocused=if activeConversation focused then True else composerFocused focused},[])
    _ -> (focused,[])

mapWindow :: Int -> (Window -> Window) -> Desktop -> Desktop
mapWindow i f d = d {windows = map (\w -> if windowId w==i then f w else w) (windows d)}

beginWindowDrag :: Drag -> Desktop -> Desktop
beginWindowDrag capture d = d {drag=Just capture,
  dragOriginal=Just [(windowId w,bounds w,restoredBounds w) | w<-windows d]}

dragKey :: V.Key -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
dragKey key mods d = case dragOriginal d of
  Just originals@((wid,_,_):_) -> case key of
    V.KEsc -> (done {windows=map restore (windows d)},[])
      where restore w=case find (\(i,_,_)->i==windowId w) originals of
              Just (_,r,savedBounds) -> w {bounds=r,restoredBounds=savedBounds}
              Nothing -> w
    V.KEnter -> (done,[])
    _ | Just (dx,dy)<-lookup key [(V.KLeft,(-1,0)),(V.KRight,(1,0)),(V.KUp,(0,-1)),(V.KDown,(0,1))],
        Just w<-find ((==wid).windowId) (windows d) ->
      let r=bounds w in
      (if V.MShift `elem` mods
        then resizeWindowBounds wid r {width=width r+dx,height=height r+dy} d
        else moveWindow wid (left r+dx) (top r+dy) d,[])
      | otherwise -> (d,[])
  _ -> (d,[])
  where done=d {drag=Nothing,dragOriginal=Nothing}

-- Walk both moving edges against the original contacts on each axis. Doing
-- the axes independently keeps a diagonal title drag from losing its neighbors
-- after the first axis moves. Corner resizing remains the way to detach.
moveWindow :: Int -> Int -> Int -> Desktop -> Desktop
moveWindow wid _ _ d | M.member wid (dockedTerminals d) = d
moveWindow wid x y d = case find ((==wid).windowId) (windows d) of
  Nothing -> d
  Just source ->
    let r=bounds source
        target=fitMovingWindow d r {left=x,top=y}
        views=floatingWindows d
        along axis delta area=snd (moveEdges axis (True,True) delta area (wid,r) views)
        horizontal=along False (left target-left r) (treeWidthOf d,fst (screenSize d))
        vertical=along True (top target-top r) (1,top (problemsRect d))
        combine a b=let ar=bounds a; br=bounds b; rectangle=ar {top=top br,height=height br}
                    in a {bounds=rectangle,restoredBounds=if restoredBounds b==Nothing then Nothing else restoredBounds a}
    in replaceFloating (zipWith combine horizontal vertical) d

-- A diagonal corner gesture deliberately breaks contacts. A single moving edge
-- uses the same rule whether it came from a frame, corner or Shift-arrow drag.
resizeWindowBounds :: Int -> Rect -> Desktop -> Desktop
resizeWindowBounds wid _ d | M.member wid (dockedTerminals d) = d
resizeWindowBounds wid requested d = case find ((==wid).windowId) (windows d) of
  Just w | top old==top requested && height old==height requested ->
             resizeWindowEdge wid False False (left requested+width requested) d
         | left old==left requested && width old==width requested ->
             resizeWindowEdge wid True False (top requested+height requested) d
         | otherwise -> mapWindow wid (\v -> v {bounds=fitWindow d requested,restoredBounds=Nothing}) d
    where old=bounds w
  Nothing -> d

axisBounds :: Bool -> Rect -> (Int,Int)
axisBounds vertical r = if vertical then (top r,top r+height r) else (left r,left r+width r)

resizeWindowEdge :: Int -> Bool -> Bool -> Int -> Desktop -> Desktop
resizeWindowEdge wid _ _ _ d | M.member wid (dockedTerminals d) = d
resizeWindowEdge wid vertical leading position d = case find ((==wid).windowId) (windows d) of
  Nothing -> d
  Just source -> let (_,moved)=resizeEdge vertical leading position area (wid,bounds source) (floatingWindows d)
                 in replaceFloating moved d
  where area=if vertical then (1,top (problemsRect d)) else (treeWidthOf d,fst (screenSize d))

-- Docks seed the same contact walk with a rectangle outside the window list.
-- The returned edge can stop short of the pointer to preserve every minimum size.
resizeEdge :: Bool -> Bool -> Int -> (Int,Int) -> (Int,Rect) -> [Window] -> (Int,[Window])
resizeEdge vertical leading position area source views = (oldEdge+delta,moved)
  where
    (sourceLow,sourceHigh)=axisBounds vertical (snd source)
    oldEdge=if leading then sourceLow else sourceHigh
    (delta,moved)=moveEdges vertical (leading,not leading) (position-oldEdge) area source views

moveEdges :: Bool -> (Bool,Bool) -> Int -> (Int,Int) -> (Int,Rect) -> [Window] -> (Int,[Window])
moveEdges _ _ 0 _ _ views = (0,views)
moveEdges vertical (low,high) requested (desktopLow,desktopHigh) source views = (delta,map apply views)
  where
    minimumSize=min (if vertical then 5 else 16) (desktopHigh-desktopLow)
    seed=(source,low,high)
    planned=propagate [seed] [seed]
    -- ponytail: list scans are cubic in window count; index touching edges only
    -- if desktops with hundreds of windows make this visible during dragging.
    propagate seen []=seen
    propagate seen ((moving,lowMoves,highMoves):pending)=propagate (seen++neighbors) (pending++neighbors)
      where
        r=snd moving
        (lo,hi)=axisBounds vertical r
        (start,end)=axisBounds (not vertical) r
        neighbors=[((windowId w,other),moveLow,moveHigh) | w<-views,
          not (any (\((ident,_),_,_)->ident==windowId w) seen),
          let other=bounds w
              (a,b)=axisBounds vertical other
              (s,e)=axisBounds (not vertical) other
              equal=start==s && end==e,
          start<=s && e<=end,
          (moveLow,moveHigh)<-if lowMoves && b==lo then [(not equal && a>desktopLow,True)]
            else if highMoves && a==hi then [(True,not equal && b<desktopHigh)] else []]
    limits ((_,r),moveLow,moveHigh)
      | moveLow && moveHigh = (desktopLow+minimumSize-hi,desktopHigh-minimumSize-lo)
      | moveLow = (desktopLow-lo,hi-lo-minimumSize)
      | otherwise = (minimumSize-(hi-lo),desktopHigh-hi)
      where (lo,hi)=axisBounds vertical r
    allowed=[limits entry | entry@((ident,_),_,_)<-planned,any ((==ident).windowId) views]
    delta=foldl (\n (a,b)->max a (min b n)) requested allowed
    apply w=case find (\((ident,_),_,_)->ident==windowId w) planned of
      Nothing -> w
      Just (_,moveLow,moveHigh) ->
        let r=bounds w
            (lo,hi)=axisBounds vertical r
            a=if moveLow then max desktopLow (lo+delta) else lo
            b=if moveHigh then min desktopHigh (hi+delta) else hi
        in w {bounds=if vertical then r {top=a,height=b-a} else r {left=a,width=b-a},restoredBounds=Nothing}

windowPositionText :: Desktop -> Document -> Window -> Text
windowPositionText d doc _ | documentLabel doc==Just "Conversation" = " "<>case conversationContextUsage d of
  Just (used,size) | used>=0 && size>0 -> T.pack (show (used*100 `div` size))<>"% · "<>formatTokenCount used<>"/"<>formatTokenCount size<>" "
  _ -> "-- "
windowPositionText _ doc w | byteMode (documentBuffer doc) = " HEX "<>hexNumber 8 (caret (selection w))<>" "<>(if windowHexAscii w then "ASCII" else "HEX")<>" "
windowPositionText _ doc w = let (r,c)=bufferLineColumn (documentBuffer doc) (caret (selection w))
  in " "<>T.pack (show (r+1))<>":"<>T.pack (show (c+1))<>" "

formatTokenCount :: Integer -> Text
formatTokenCount value
  | value<1000 = T.pack (show (max 0 value))
  | otherwise = compact 1000 ["k","M","G","T","P","E"]
  where
    compact unit (suffix:rest)
      | rounded>=1000, not (null rest) = compact (unit*1000) rest
      | value<10*unit = T.pack (show (tenths `div` 10))<>
          (if tenths `mod` 10==0 then "" else "."<>T.pack (show (tenths `mod` 10)))<>suffix
      | otherwise = T.pack (show rounded)<>suffix
      where rounded=(value+unit `div` 2) `div` unit
            tenths=(value*10+unit `div` 2) `div` unit
    compact _ []=T.pack (show value)

windowPositionColumn :: Document -> Int
windowPositionColumn doc = if byteMode (documentBuffer doc) then 10 else 2

scrollbarRect :: Desktop -> Bool -> Document -> Window -> Rect
scrollbarRect d vertical doc w
  | vertical = Rect (x+ww-1) (y+1) 1 (windowContentRows d doc w)
  | byteMode (documentBuffer doc) && scrollbarLimit d False doc w==0 = Rect x (y+hh-1) 0 1
  | otherwise = let start=windowPositionColumn doc+T.length (windowPositionText d doc w) in Rect (x+start) (y+hh-1) (max 0 (ww-start-2)) 1
  where Rect x y ww hh=bounds w

scrollbarLimit :: Desktop -> Bool -> Document -> Window -> Int
scrollbarLimit d vertical doc w = max 0 (if vertical then documentRows doc w-max 1 (windowContentRows d doc w)
  else windowDocumentWidth doc w-max 1 (width (bounds w)-2)+(if byteMode (documentBuffer doc) then 0 else 1))

-- Each split chooses its own layout. Keep the byte viewport and a visible caret
-- anchored when resizing or docking Files changes the number of bytes per row.
clampHexScroll :: Desktop -> Desktop -> Desktop
clampHexScroll before d = d {windows=map clamp (windows d)}
  where
    clamp w | Just doc<-M.lookup (bufferId w) (buffers d), byteMode (documentBuffer doc) =
      let old=fromMaybe w (find ((==windowId w).windowId) (windows before))
          changed=windowHexBytes old/=windowHexBytes w
          row=if changed then scrollRow old*windowHexBytes old `div` windowHexBytes w else scrollRow w
          oldCursor=caret (selection old) `div` windowHexBytes old
          visible=oldCursor>=scrollRow old && oldCursor<scrollRow old+height (bounds old)-2
          cursor=caret (selection w) `div` windowHexBytes w
          anchored=if changed && visible then max (cursor-height (bounds w)+3) (min cursor row) else row
      in w {scrollRow=max 0 (min (scrollbarLimit d True doc w) anchored),
            scrollColumn=max 0 (min (scrollbarLimit d False doc w) (if changed then 0 else scrollColumn w))}
    clamp w = w

scrollbarThumb :: Int -> Int -> Int -> Int
scrollbarThumb len limit position = 1+min limit (max 0 position)*max 0 (len-3) `div` max 1 limit

changeScroll :: Bool -> Int -> Desktop -> Desktop
changeScroll vertical delta d = case (activeWindow d,activeDocument d) of
  (Just w,Just doc) -> let value=max 0 (min (scrollbarLimit d vertical doc w) ((if vertical then scrollRow w else scrollColumn w)+delta))
    in modifyActive (\v -> if vertical then v {scrollRow=value} else v {scrollColumn=value}) d
  _ -> d

scrollClick :: Bool -> Int -> Int -> Desktop -> Desktop
scrollClick vertical x y d = case (activeWindow d,activeDocument d) of
  (Just w,Just doc) ->
    let r=scrollbarRect d vertical doc w
        len=if vertical then height r else width r
        offset=if vertical then y-top r else x-left r
        thumb=scrollbarThumb len (scrollbarLimit d vertical doc w) (if vertical then scrollRow w else scrollColumn w)
        page=max 1 ((if vertical then height else width) (bounds w)-2)
    in if offset==0 then changeScroll vertical (-1) d
       else if offset==len-1 then changeScroll vertical 1 d
       else if offset==thumb then d {drag=Just (Scrolling (windowId w) vertical)}
       else changeScroll vertical (if offset<thumb then negate page else page) d
  _ -> d

scrollTrack :: Bool -> Int -> Int -> Desktop -> Desktop
scrollTrack vertical x y d = case (activeWindow d,activeDocument d) of
  (Just w,Just doc) -> let { r=scrollbarRect d vertical doc w
                         ; len=if vertical then height r else width r
                         ; offset=if vertical then y-top r else x-left r
                         ; value=max 0 (min (scrollbarLimit d vertical doc w) ((offset-1)*scrollbarLimit d vertical doc w `div` max 1 (len-3))) }
                     in modifyActive (\v -> if vertical then v {scrollRow=value} else v {scrollColumn=value}) d
  _ -> d

selectAt :: Bool -> Int -> Int -> Desktop -> Desktop
selectAt extend x y d = case activeWindow d of
  Nothing -> d
  Just w | activeHex d -> let { col=max 0 (x-left (bounds w)-1+scrollColumn w)
                             ; row=max 0 (y-top (bounds w)-1+scrollRow w)
                             ; (offset,ascii,low)=hexHit (windowHexBytes w) col }
                            in modifyActive (\v -> v {windowHexAscii=ascii,windowHexLow=low}) (moveTo extend (row*windowHexBytes w+offset) d)
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
  | ctrl, V.MShift `elem` mods, V.KChar c<-key, Just cmd<-lookup (toLower c) [('z',Redo),('l',FindPrevious),('c',Conversation),('n',AgentNew)] = runCommand cmd d
  | ctrl, wordStar d, not (activeHex d), V.KChar c <- key = starKey (toLower c) d
  | ctrl, V.KChar c <- key, Just cmd <- lookup (toLower c) [('b',ToggleTree),('s',Save),('o',Open),('n',New),('z',Undo),('y',Redo),('c',Copy),('x',Cut),('v',Paste),('a',SelectAll),('f',Find),('h',Replace),('r',Replace),('g',GoTo),('l',FindNext),('q',Quit)] = runCommand cmd d
  | otherwise = (editorKey key mods d,[])
  where ctrl = V.MCtrl `elem` mods

editorKey :: V.Key -> [V.Modifier] -> Desktop -> Desktop
editorKey key mods d | activeHex d = hexKey key mods d
editorKey key mods d = case key of
  V.KLeft -> move (if ctrl then wordLeft t p else bufferPreviousCharacter b p)
  V.KRight -> move (if ctrl then wordRight t p else bufferNextCharacter b p)
  V.KUp -> vertical (-1)
  V.KDown -> vertical 1
  V.KPageUp -> vertical (negate page)
  V.KPageDown -> vertical page
  V.KHome -> move (if ctrl then 0 else start)
  V.KEnd -> move (if ctrl then bufferLength b else start+T.length (bufferLineAt b row))
  V.KBS -> erase (if ctrl then wordLeft t p else bufferPreviousCharacter b p) p
  V.KDel -> erase p (if ctrl then wordRight t p else bufferNextCharacter b p)
  V.KEnter -> insertText (bufferNewline b) d
  V.KChar '\t' -> insertText "  " d
  V.KChar c | null mods || mods==[V.MShift], textInputChar c -> insertText (T.singleton c) d
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
    'y' -> let b=maybe (newBuffer "") documentBuffer (activeDocument d); p=maybe 0 (caret . selection) (activeWindow d); row=fst (bufferLineColumn b p); a=bufferLineOffset b row; z=bufferLineOffset b (row+1)
           in (editActive (\_ -> replaceSelection (Selection a z) "") (Just a) d,[])
    'z' -> runCommand Undo d
    _ -> (d,[])

starPrefix :: Char -> Char -> Desktop -> (Desktop,[Effect])
starPrefix 'k' c d = case c of
  'b' -> (d {blockStart=(\w -> (bufferId w,caret (selection w))) <$> activeWindow d},[])
  'k' -> (case (blockStart d,activeWindow d) of
    (Just (bid,p),Just w) | bid==bufferId w -> modifyActive (\v -> v {selection=Selection (min p (maybe 0 (bufferLength . documentBuffer) (activeDocument d))) (caret (selection v))}) d
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
  'c' -> (moveTo False (maybe 0 (bufferLength . documentBuffer) (activeDocument d)) d,[])
  'f' -> runCommand Find d
  'a' -> runCommand Replace d
  _ -> (d {status="Unknown Ctrl+Q command."},[])
starPrefix _ _ d = (d,[])

dialogEvent :: V.Event -> Dialog -> Desktop -> (Desktop,[Effect])
dialogEvent ev dg d = case ev of
  V.EvKey (V.KChar c) mods | searching dg,V.MCtrl `elem` mods,toLower c `elem` ['f','h','r'] -> runCommand (if toLower c=='f' then Find else Replace) d
  V.EvKey (V.KChar '\t') mods | Searching mode _<-purpose dg,V.MCtrl `elem` mods -> (searchPrompt (not mode) d,[])
  V.EvMouseDown x y V.BLeft _ | searching dg,Just (_,mode)<-find (\(r,_)->inside r x y) (searchTabRects d dg) -> (searchPrompt mode d,[])
  V.EvKey (V.KFun 3) mods | V.MAlt `elem` mods, PermissionDialog{}<-purpose dg -> dialogEvent (V.EvKey V.KEsc []) dg d
  V.EvKey V.KEsc _ | PermissionDialog action<-purpose dg -> (d {dialog=Nothing,buttonHover=Nothing,buttonPressed=Nothing},[PermissionAction action ["1"]])
  V.EvKey V.KEsc _ -> (d {dialog=Nothing,buttonHover=Nothing,buttonPressed=Nothing},[])
  V.EvKey (V.KChar c) mods | (V.MCtrl `elem` mods && not areaFocused && not (approvalDialog dg)) || V.MAlt `elem` mods,
    Just i<-findIndex (==Just (toLower c)) (buttonMnemonics dg) -> submitDialog i dg d
  V.EvKey (V.KChar '\t') mods -> setFocus (focus dg + if V.MShift `elem` mods then -1 else 1)
  V.EvKey V.KBackTab _ -> setFocus (focus dg-1)
  V.EvKey key mods | areaFocused -> areaKey key mods
  V.EvKey V.KEnter _ | approvalDialog dg, focus dg<count -> (d,[])
  V.EvKey V.KEnter _ -> submitDialog (if focus dg>=count then focus dg-count else 0) dg d
  V.EvKey k mods | focus dg<count -> updateField (fieldKey k mods)
  V.EvKey (V.KChar ' ') _ -> submitDialog (focus dg-count) dg d
  V.EvKey V.KLeft _ -> setFocus (focus dg-1)
  V.EvKey V.KRight _ -> setFocus (focus dg+1)
  V.EvPaste bytes | areaFocused -> case TE.decodeUtf8' bytes of
    Right text -> updateField (textAreaEdit focusedRect (insertText (T.filter (\c -> textInputChar c || c=='\n' || c=='\r' || c=='\t') text)))
    Left _ -> (d,[])
  V.EvPaste bytes | focus dg<count -> case TE.decodeUtf8' bytes of
    Right text -> updateField (\f -> case f of Input label value pos -> let clean=T.filter textInputChar text in Input label (T.take pos value<>clean<>T.drop pos value) (pos+T.length clean); _ -> f)
    Left _ -> (d,[])
  V.EvMouseDown x y V.BLeft _ | approvalDialog dg, inside (dialogCloseRect d dg) x y -> submitDialog 1 dg d
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
                    click f@(TextArea name editable b sel row col) = let area=textAreaRect (fieldRects d dg !! i) f
                                                                 in if x==left area+width area && y>=top area && y<top area+height area then
                                                                   TextArea name editable b sel ((y-top area)*max 0 (bufferLineCount b-height area) `div` max 1 (height area-1)) col
                                                                 else if inside area x y then
                                                                   let line=max 0 (min (bufferLineCount b-1) (row+y-top area))
                                                                       pos=bufferLineOffset b line+columnOffset (bufferLineAt b line) (col+x-left area)
                                                                   in TextArea name editable b (Selection pos pos) row col
                                                                   else f
                    click f = f
                in updateDialog dg {focus=i,fields=replaceAt i (click (fields dg !! i)) (fields dg)}
  V.EvMouseDown x y V.BScrollDown _ -> wheel x y 3
  V.EvMouseDown x y V.BScrollUp _ -> wheel x y (-3)
  V.EvMouseUp x y button | button==Nothing || button==Just V.BLeft ->
    let released=d {buttonPressed=Nothing}
    in case buttonPressed d of
      Just i | Just i==findIndex (\r -> inside r x y) (buttonRects d dg) -> submitDialog i dg released
      _ -> (released {buttonHover=Nothing},[])
  _ -> (d,[])
  where
    count=length (fields dg)
    areaFocused=case drop (focus dg) (fields dg) of TextArea{}:_ -> True; _ -> False
    focusedRect=fromMaybe (Rect 0 0 1 1) (listToMaybe (drop (focus dg) (fieldRects d dg)))
    areaKey (V.KChar c) mods | V.MCtrl `elem` mods, c `elem` ['c','x','v'], f@(TextArea _ True b sel _ _)<-fields dg !! focus dg =
      let copied=selectedText sel b
          edited=case c of 'x' -> textAreaEdit focusedRect (insertText "") f; 'v' -> textAreaEdit focusedRect (insertText (clipboard d)) f; _ -> f
      in (d {clipboard=if c=='v' then clipboard d else copied,dialog=Just dg {fields=replaceAt (focus dg) edited (fields dg)}},[])
    areaKey key mods = updateField $ \f -> if editableArea f
      then textAreaEdit focusedRect (case key of
        V.KChar c | V.MCtrl `elem` mods, Just cmd<-lookup (toLower c) [('z',if V.MShift `elem` mods then Redo else Undo),('y',Redo),('a',SelectAll)] -> fst . runCommand cmd
        _ -> editorKey key mods) f
      else clampArea focusedRect (fieldKey key mods f)
    clampArea rect f@(TextArea name editable b sel row col) = TextArea name editable b sel (min row (max 0 (bufferLineCount b-height (textAreaRect rect f)))) col
    clampArea _ f = f
    wheel x y delta = case findIndex (\r -> inside r x y && y<top (dialogRect d dg)+height (dialogRect d dg)-3) (fieldRects d dg) of
      Just i | f@TextArea{}<-fields dg !! i -> updateDialog dg {focus=i,fields=replaceAt i (clampArea (fieldRects d dg !! i) (scrollTextArea delta f)) (fields dg)}
      _ -> updateField (fieldKey (if delta>0 then V.KDown else V.KUp) [])
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

scrollTextArea :: Int -> Field -> Field
scrollTextArea delta (TextArea name editable b sel row col) = TextArea name editable b sel (max 0 (min (bufferLineCount b-1) (row+delta))) col
scrollTextArea _ f = f

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
    V.KChar c | (null mods || mods==[V.MShift]) && textInputChar c -> set (T.take pos value<>T.singleton c<>T.drop pos value) (pos+1)
    _ -> field
  TextArea name editable b sel row col -> case key of
    V.KLeft -> TextArea name editable b sel row (max 0 (col-1))
    V.KRight -> TextArea name editable b sel row (min (bufferLength b) (col+1))
    V.KHome -> TextArea name editable b sel 0 0
    V.KEnd -> TextArea name editable b sel (max 0 (bufferLineCount b-1)) col
    _ -> scrollTextArea (case key of V.KUp -> -1; V.KDown -> 1; V.KPageUp -> -8; V.KPageDown -> 8; _ -> 0) field
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
    ChangingDirectory base entries
      | focus dg==1, FileList _ index:_<-drop 1 (fields dg), index>=0, entry:_<-drop index entries ->
          (original,[BrowseDirectories (base </> T.unpack (entryName entry))])
      | otherwise -> (original,[if button==1 then BrowseDirectories chosenPath else ChangeDirectory chosenPath])
      where chosenPath=if isAbsolute (T.unpack first) then T.unpack first else base </> T.unpack first
    ProjectLoading _ -> (d,[])
    ProjectChoices token page -> (d,[ProjectRequest (case button of
      0 -> ProjectDetails token (page*32+selected)
      1 -> ProjectPage token (page-1)
      2 -> ProjectPage token (page+1)
      _ -> LoadProject)])
    CodeActionChoices bid version choices -> case drop selected choices of
      ident:_ -> (d,[LanguageRequest (ApplyCodeAction bid version ident)])
      _ -> (d,[])
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
    DebugDialog action -> (d,[DebugAction action (T.pack (show button) : values ++
      [if value then "true" else "false" | CheckBox _ value <- fields dg] ++
      [T.pack (show i) | ListBox _ _ i <- fields dg])])
    PermissionDialog action -> (if button==0 && approvalDialog dg then original else d,[PermissionAction action (T.pack (show button) : values ++
      [contents b | TextArea _ True b _ _ _ <- fields dg] ++
      [T.pack (show i) | Radio _ _ i <- fields dg] ++
      [T.pack (show i) | ListBox _ _ i <- fields dg])])
    AgentDialog action -> (d,[AgentAction action (T.pack (show button) : values ++
      [if value then "true" else "false" | CheckBox _ value <- fields dg] ++
      [T.pack (show i) | Radio _ _ i <- fields dg] ++
      [T.pack (show i) | ListBox _ _ i <- fields dg])])
    Searching False _ -> (findText first d,[])
    Searching True _ -> let found = findText first d
                 in if T.null first || status found=="Search text not found." then (found,[])
                    else (insertText second found,[])
    GoingTo | activeHex d -> case readMaybe (T.unpack first) of
      Just n | n>=0 -> (moveTo False n d,[])
      _ -> (original {status="Enter a nonnegative byte offset."},[])
    GoingTo -> case readMaybe (T.unpack first) of
      Just n | n>0 -> (moveTo False (maybe 0 (\doc->bufferLineOffset (documentBuffer doc) (n-1)) (activeDocument d)) d,[])
      _ -> (original {status="Enter a positive line number."},[])
    DiscardDraft | button==0 -> runCommand Quit d {composerBuffer=newBuffer "",composerSelection=Selection 0 0,conversationViews=M.map (\view->view {conversationDraft=newBuffer "",conversationDraftSelection=Selection 0 0}) (conversationViews d)}
                 | otherwise -> (d,[])
    Confirm cmd | button==0 -> saveRequest (Just cmd) d
                | button==1 -> case cmd of
                    Close -> (closeActive d,[])
                    Quit -> runCommand Quit (discardActive d)
                    _ -> (d,[])
                | otherwise -> (d,[])
    Settings -> (d {wordStar=any (\f -> case f of Radio "Key bindings" _ 1 -> True; _ -> False) (fields dg),
      appearance=fromMaybe (appearance d) (listToMaybe [toEnum (max 0 (min 2 value)) | Radio "Appearance" _ value<-fields dg]),
      streamerMode=fromMaybe (streamerMode d) (listToMaybe [value | CheckBox "Streamer mode" value<-fields dg]),
      blinkCursor=fromMaybe (blinkCursor d) (listToMaybe [value | CheckBox "Blinking cursor" value<-fields dg]),
      pixelateUnicode=fromMaybe (pixelateUnicode d) (listToMaybe [value | CheckBox "Pixelate Unicode" value<-fields dg]),
      crtFilter=fromMaybe (crtFilter d) (listToMaybe [value | CheckBox "CRT filter" value<-fields dg]),status="Preferences updated."},
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
startingDirectory d = fromMaybe (maybe (maybe "." treeRoot (sideTree d)) (takeDirectory . filePath) (activeDocument d >>= documentFile)) (defaultDirectory d)

treeWidthOf :: Desktop -> Int
-- The dock's right frame is also the editor area's left frame.
treeWidthOf = maybe 0 (max 0 . subtract 1 . treeWidth) . sideTree

-- Pinning changes only view placement. Console processes remain owned by Consoles.
terminalWindow :: Desktop -> Window -> Bool
terminalWindow d w = maybe False terminal (M.lookup (bufferId w) (buffers d) >>= documentLabel)
  where terminal label=any (`T.isPrefixOf` label) ["Terminal ","Ended Terminal "]

windowPinned :: Desktop -> Window -> Bool
windowPinned d w = M.member (windowId w) (dockedTerminals d)

floatingWindows :: Desktop -> [Window]
floatingWindows d = filter (not . windowPinned d) (windows d)

windowVisible :: Desktop -> Window -> Bool
windowVisible d w = not (windowPinned d w) || bottomTerminal d==Just (windowId w)

bottomVisible :: Desktop -> Bool
bottomVisible d = problemsVisible d || not (M.null (dockedTerminals d))

messagesDisplayed :: Desktop -> Bool
messagesDisplayed d = problemsVisible d && bottomTerminal d==Nothing

replaceFloating :: [Window] -> Desktop -> Desktop
replaceFloating views d = d {windows=map (\w -> fromMaybe w (find ((==windowId w).windowId) views)) (windows d)}

layoutBottomWindows :: Desktop -> Desktop
layoutBottomWindows d = d {windows=map (\w -> if windowPinned d w then w {bounds=problemsRect d,restoredBounds=Nothing} else w) (windows d)}

normalizeBottom :: Desktop -> Desktop
normalizeBottom d = d {bottomTerminal=chosen,problemsFocused=problemsFocused d && chosen==Nothing && problemsVisible d}
  where chosen=case bottomTerminal d of
          Just ident | M.member ident (dockedTerminals d) -> Just ident
          _ | problemsVisible d -> Nothing
            | otherwise -> listToMaybe (M.keys (dockedTerminals d))

setTerminalPinned :: Bool -> Int -> Desktop -> Desktop
setTerminalPinned pinned ident d = case find ((==ident).windowId) (windows d) of
  Just w | terminalWindow d w, pinned, not (windowPinned d w) ->
    focusWindow ident (layoutProblems d d {dockedTerminals=M.insert ident (bounds w,restoredBounds w) (dockedTerminals d),bottomTerminal=Just ident,drag=Nothing,dragOriginal=Nothing})
  Just _ | not pinned, Just (rectangle,saved)<-M.lookup ident (dockedTerminals d) ->
    let next=layoutProblems d (normalizeBottom d {dockedTerminals=M.delete ident (dockedTerminals d),drag=Nothing,dragOriginal=Nothing})
    in focusWindow ident (mapWindow ident (\w -> w {bounds=fitWindow next rectangle,restoredBounds=fmap (fitWindow next) saved}) next)
  _ -> d

-- Tab positions are shared by drawing and hit testing. When crowded, scroll the
-- strip to include the selected tab; Alt-number/F6 still reaches every window.
bottomTabs :: Desktop -> [(Rect,Maybe Int,Text)]
bottomTabs d = placeTabs 1 visible
  where
    available=max 0 (fst (screenSize d)-12)
    tabs=[(Nothing,"Messages "<>maybe "" (T.pack.show) (messagesNumber d)) | problemsVisible d]++
      [(Just (windowId w),"Terminal "<>T.pack (show (windowNumber w))) | w<-sortOn windowNumber (windows d),windowPinned d w]
    selected=fromMaybe 0 (findIndex ((==bottomTerminal d).fst) tabs)
    tabWidth (_,name)=min available (T.length name+2)
    prefix=take (selected+1) tabs
    skip=length prefix-length (takeFitting (reverse prefix))
    takeFitting=go 0
      where go _ []=[]
            go used (t:ts) | used+tabWidth t<=available = t:go (used+tabWidth t) ts
                           | otherwise = []
    visible=drop skip tabs
    placeTabs _ []=[]
    placeTabs x ((ident,name):rest)
      | x>available = []
      | otherwise = let n=min (available-x+1) (T.length name+2)
                    in (Rect x (top (problemsRect d)) n 1,ident,T.take n (" "<>name<>" ")):placeTabs (x+n) rest

bottomMouse :: Int -> Int -> V.Button -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
bottomMouse x y button mods d
  | M.null (dockedTerminals d) = problemsMouse x y button d
  | y==top r, button==V.BLeft = case () of
      _ | Just (_,ident,_)<-find (\(tab,_,_)->inside tab x y) (bottomTabs d) ->
            (case ident of Just wid -> focusWindow wid d; Nothing -> d {bottomTerminal=Nothing,problemsFocused=True,sideTree=fmap (\t->t {treeFocused=False}) (sideTree d)},[])
        | Just wid<-bottomTerminal d, x>=width r-9, x<width r-6 -> (setTerminalPinned False wid d,[])
        | Just wid<-bottomTerminal d, x>=width r-5 -> runCommand Close (focusWindow wid d)
        | messagesDisplayed d, x>=width r-5 -> (setProblemsVisible False d,[])
        | otherwise -> (d {drag=Just MessagesSizing},[])
  | messagesDisplayed d = problemsMouse x y button d
  | otherwise = windowMouse x y button mods d
  where r=problemsRect d

problemsHeight :: Desktop -> Int
problemsHeight d = if bottomVisible d then min (max 3 (problemsPreferredHeight d)) (max 0 (snd (screenSize d)-7)) else 0

problemsRect :: Desktop -> Rect
problemsRect d = let (sw,sh)=screenSize d; h=problemsHeight d in Rect 0 (sh-h-1) sw h

setProblemsVisible :: Bool -> Desktop -> Desktop
setProblemsVisible visible d = layoutProblems d next
  where next=normalizeBottom d {problemsVisible=visible,bottomTerminal=if visible then Nothing else bottomTerminal d,problemsFocused=False,drag=Nothing,dragOriginal=Nothing,
          messagesNumber=if visible then Just (fromMaybe (nextWindowNumber d) (messagesNumber d)) else Nothing}

resizeProblems :: Int -> Desktop -> Desktop
resizeProblems y d = clampHexScroll d (ensureVisible fitted)
  where
    sh=snd (screenSize d)
    requested=sh-max 3 (min (sh-7) (sh-y-1))-1
    (edge,moved)=resizeEdge True True requested (1,sh-1) (-2,problemsRect d) (floatingWindows d)
    next=d {problemsPreferredHeight=sh-edge-1,drag=Just MessagesSizing}
    fitted=layoutBottomWindows (replaceFloating (map (fitDockWindow next) moved) next) {
      sideTree=fmap (\t -> t {treeScroll=min (treeScroll t) (treeScrollLimit next t)}) (sideTree next)}

layoutProblems :: Desktop -> Desktop -> Desktop
layoutProblems before after = clampHexScroll before (ensureVisible fitted)
  where
    oldEdge=top (problemsRect before); newEdge=top (problemsRect after)
    fitted=layoutBottomWindows after {windows=map resize (windows after),
      sideTree=fmap (\t -> t {treeScroll=min (treeScroll t) (treeScrollLimit after t)}) (sideTree after)}
    resize w
      | windowPinned after w || oldEdge==newEdge = w
      | bottom==oldEdge || bottom>newEdge =
          let y=if top r<=1 then 1 else max 1 (newEdge-height r)
          in w {bounds=fitWindow after r {top=y,height=newEdge-y},restoredBounds=Nothing}
      | otherwise = w
      where r=bounds w; bottom=top r+height r

copyMessages :: [Diagnostic] -> Desktop -> (Desktop,[Effect])
copyMessages [] d = (d {status="No messages to copy."},[])
copyMessages issues d = (d {clipboard=T.intercalate "\n\n" (map format issues),status="Messages copied."},[])
  where format issue=(case diagnosticSeverity issue of 1 -> "Error "; 2 -> "Warning "; 3 -> "Info "; _ -> "Hint ")<>
          T.pack (diagnosticPath issue)<>":"<>T.pack (show (diagnosticRow issue+1))<>":"<>T.pack (show (diagnosticColumn issue+1))<>" "<>diagnosticMessage issue

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
          | y==top r -> (focused {drag=Just MessagesSizing},[])
          | y>top r && y<top r+height r-1 && selected<length (diagnostics d) -> (chooseProblem selected focused,[])
          | otherwise -> (focused,[])
  V.BRight -> (openContext MessagesContext x y (if y>top r && y<top r+height r-1 && selected<length (diagnostics d) then chooseProblem selected focused else focused),[])
  V.BScrollUp -> (chooseProblem (problemsSelected d-3) focused,[])
  V.BScrollDown -> (chooseProblem (problemsSelected d+3) focused,[])
  _ -> (d,[])
  where r=problemsRect d; selected=problemsScroll d+y-top r-1; focused=d {bottomTerminal=Nothing,problemsFocused=True,sideTree=fmap (\t -> t {treeFocused=False}) (sideTree d)}

problemsKey :: V.Key -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
problemsKey key mods d
  | V.MCtrl `elem` mods, key `elem` [V.KChar 'c',V.KChar 'C',V.KIns] = runCommand Copy d
  | otherwise = case key of
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

-- Make room for the requested position before enforcing minimum dimensions.
fitMovingWindow :: Desktop -> Rect -> Rect
fitMovingWindow d r = fitWindow d (r
  {width=min (width r) (fst (screenSize d)-left r),
   height=min (height r) (top (problemsRect d)-top r)})

setTree :: Maybe Sidebar -> Desktop -> Desktop
setTree tree d = layoutBottomWindows (clampHexScroll d next {windows=map move (windows d)})
  where
    next=d {sideTree=tree,drag=Nothing,dragOriginal=Nothing}
    old=treeWidthOf d; new=treeWidthOf next
    sw=fst (screenSize d); delta=new-old
    move w | windowPinned d w = w
    move w = w {bounds=fitWindow next r {left=x,width=right-x},restoredBounds=Nothing}
      where r=bounds w
            x=left r+delta
            right=if left r+width r>=sw then sw else min sw (x+width r)

-- Detached windows stay put unless the growing dock reaches their rectangle.
fitDockWindow :: Desktop -> Window -> Window
fitDockWindow d w
  | windowPinned d w = w {bounds=problemsRect d}
  | fitted==bounds w = w
  | otherwise = w {bounds=fitted,restoredBounds=Nothing}
  where fitted=fitWindow d (bounds w)

resizeTree :: Int -> Desktop -> Desktop
resizeTree x d = case sideTree d of
  Nothing -> d
  Just tree -> layoutBottomWindows (clampHexScroll d (replaceFloating (map (fitDockWindow next) moved) next))
    where
      sw=fst (screenSize d)
      source=Rect 0 1 (treeWidthOf d) (top (problemsRect d)-1)
      requested=max 16 (min (sw-20) (x+1))-1
      (edge,moved)=resizeEdge False False requested (0,sw) (-1,source) (floatingWindows d)
      next=d {sideTree=Just tree {treeWidth=edge+1},drag=Just DockSizing,dragOriginal=Nothing}

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
    moveToNode i = let chosen=max 0 (min (length (treeRows tree)-1) i); visible=max 1 (treeContentRows d); scroll=max 0 (min chosen (max (treeScroll tree) (chosen-visible+1))) in (d {sideTree=Just tree {treeSelected=chosen,treeScroll=scroll}},[])
    leave=(d {sideTree=Just tree {treeFocused=False}},[])

treeContentRows :: Desktop -> Int
treeContentRows d = max 0 (snd (screenSize d)-4-problemsHeight d)

treeScrollLimit :: Desktop -> Sidebar -> Int
treeScrollLimit d tree = max 0 (length (treeRows tree)-treeContentRows d)

scrollTreeTo :: Int -> Sidebar -> Desktop -> Desktop
scrollTreeTo position tree d = d {sideTree=Just tree {treeScroll=max 0 (min (treeScrollLimit d tree) position)}}

treeMouse :: Int -> Int -> V.Button -> Sidebar -> Desktop -> (Desktop,[Effect])
treeMouse x y button tree d = case button of
  V.BLeft | y==1 && x>=treeWidth tree-5 && x<treeWidth tree-1 -> (setTree Nothing d,[])
          | x==treeWidth tree-2 && y>=2 && y<sh-2 && treeFocused tree && treeContentRows d>=3 ->
              let offset=y-2; len=treeContentRows d; thumb=scrollbarThumb len (treeScrollLimit d tree) (treeScroll tree)
                  step | offset==0 = -1 | offset==len-1 = 1 | offset<thumb = negate len | otherwise = len
              in if offset==thumb then (d {drag=Just TreeScrolling},[]) else (scrollTreeTo (treeScroll tree+step) tree d,[])
          | x==treeWidth tree-1 -> (d {drag=Just DockSizing},[])
          | x>0 && y>=2 && y<sh-2 -> activateTree False (treeScroll tree+y-2) d
          | otherwise -> (d {sideTree=Just tree {treeFocused=True},problemsFocused=False},[])
  V.BScrollUp -> (scrollTreeTo (treeScroll tree-3) tree d,[])
  V.BScrollDown -> (scrollTreeTo (treeScroll tree+3) tree d,[])
  _ -> (d,[])
  where sh=snd (screenSize d)-problemsHeight d

openDirectoryBrowser :: FilePath -> [Entry] -> Desktop -> Desktop
openDirectoryBrowser base entries d = d {dialog=Just (Dialog "Change directory" (ChangingDirectory base dirs)
  [Input "Directory" (T.pack base) (length base),FileList dirs 0] 1 ["OK","Browse","Cancel"] []),menu=Nothing,drag=Nothing,dragOriginal=Nothing}
  where dirs=filter entryDirectory entries

openBrowser :: FilePath -> Text -> [Entry] -> Desktop -> Desktop
openBrowser base pattern entries d = d {dialog=Just (Dialog "Open a file" (Opening base pattern entries) [Input "Name" pattern (T.length pattern),FileList entries 0] 1 ["Open","Cancel"] []),menu=Nothing,drag=Nothing,dragOriginal=Nothing}

addHelpStyled :: [(Char,Style)] -> Desktop -> Desktop
addHelpStyled chars d = let opened=addHelp (T.pack (map fst chars)) d
                       in case activeWindow opened of
                         Nothing -> opened
                         Just w -> opened {buffers=M.adjust (\doc -> doc {documentHighlight=[(c,ProseStyle style) | (c,style)<-chars]}) (bufferId w) (buffers opened)}

addHelp :: Text -> Desktop -> Desktop
addHelp text d = addReadOnly "Turbo Haskell Help" text d

addReadOnly :: Text -> Text -> Desktop -> Desktop
addReadOnly title text d = case [(bid,w) | (bid,doc)<-M.toList (buffers d),documentLabel doc==Just title,w<-windows d,bufferId w==bid] of
  (bid,w):_ -> focusWindow (windowId w) d {buffers=M.adjust (\doc -> restyle doc {documentBuffer=newBuffer text}) bid (buffers d)}
  [] -> let new=addDocument Nothing (newBuffer text) d in new {buffers=M.adjust (\doc -> doc {documentLabel=Just title}) (nextId d) (buffers new)}

-- Hit testing uses the same cell geometry as selection, including tabs and wide glyphs.
hoverAt :: Int -> Int -> Desktop -> (Desktop,[Effect])
hoverAt x y d = (d {hoverTarget=target,typeHint=fromMaybe (if target==hoverTarget d && typeHint d `notElem` ["Unpin window","Dock window at bottom"] then typeHint d else "") pinHint,buttonHover=hovered,contextMenu=popup,statusHover=highlight},[])
  where
    pinHint | Just _<-bottomTerminal d, y==top (problemsRect d), x>=fst (screenSize d)-9, x<fst (screenSize d)-6 = Just "Unpin window"
            | Just w<-find (\w->windowVisible d w && terminalWindow d w && not (windowPinned d w) && y==top (bounds w) && x>=left (bounds w)+6 && x<=left (bounds w)+8) (windows d), windowFocused d w = Just "Dock window at bottom"
            | otherwise = Nothing
    highlight = (\(_,i,_)->i) <$> find (\(rect,_,_)->inside rect x y) (statusItemRects d)
    hovered = dialog d >>= \dg -> findIndex (\r -> inside r x y) (buttonRects d dg)
    popup = fmap (\(r,i) -> (r,if inside r x y && y>top r && y<top r+height r-1 then contextOffset r i+y-top r-1 else i)) (contextMenu d)
    target | dialog d/=Nothing || menu d/=Nothing || contextMenu d/=Nothing || drag d/=Nothing = Nothing
           | bottomVisible d && inside (problemsRect d) x y = Nothing
           | x<treeWidthOf d = Nothing
           | otherwise = do
        w <- find (\w -> windowVisible d w && inside (bounds w) x y) (windows d)
        let Rect l t ww hh=bounds w
        if x<=l || x>=l+ww-1 || y<=t || y>=t+hh-1 then Nothing else do
          doc <- M.lookup (bufferId w) (buffers d)
          if documentLabel doc/=Nothing || not (textBuffer (documentBuffer doc)) then Nothing else do
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
  | activeHex d = d {status="Text completion is unavailable in hex mode."}
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

activeHex :: Desktop -> Bool
activeHex = maybe False (byteMode . documentBuffer) . activeDocument

windowHexBytes :: Window -> Int
windowHexBytes = hexBytesPerRow . subtract 2 . width . bounds

windowDocumentWidth :: Document -> Window -> Int
windowDocumentWidth doc w | byteMode (documentBuffer doc) = hexWidth (windowHexBytes w)
                          | documentSourceRows doc/=Nothing = documentWidth doc
                          | otherwise = maximum (documentWidth doc:caretColumn:
                              [displayColumn line (T.length line) | n<-[scrollRow w..min (bufferLineCount b-1) (scrollRow w+max 0 (height (bounds w)-2))],let line=bufferLineAt b n])
  where b=documentBuffer doc
        (_,caretColumn)=windowCursorCell b w

documentRows :: Document -> Window -> Int
documentRows doc w | byteMode b = bufferLength b `div` windowHexBytes w+1
                 | otherwise = bufferLineCount b
  where b=documentBuffer doc

windowCursorCell :: Buffer -> Window -> (Int,Int)
windowCursorCell b w
  | byteMode b = (p `div` windowHexBytes w, if windowHexAscii w then hexAsciiColumn (windowHexBytes w)+p `mod` windowHexBytes w else hexColumn (p `mod` windowHexBytes w)+if windowHexLow w then 1 else 0)
  | otherwise = let (row,col)=bufferLineColumn b p in (row,displayColumn (bufferLineAt b row) col)
  where p=caret (selection w)

toggleHex :: Desktop -> Desktop
toggleHex d = case (activeWindow d,activeDocument d) of
  (Just w,Just doc) | documentLabel doc==Nothing -> case toggleByteMode b of
    Left err -> d {status=err}
    Right changed -> let position=modeOffset b p
                    in (editActive (\_ _ -> changed) (Just position) d)
                       {status=if byteMode changed then "Hex mode: type hex pairs; Tab switches ASCII; Insert adds a zero byte." else "Text mode."}
    where b=documentBuffer doc; p=caret (selection w)
  _ -> d

pasteHex :: Text -> Desktop -> Desktop
pasteHex text d = case parseHex text of
  Left err -> d {status=err}
  Right bytes -> insertText bytes d

hexKey :: V.Key -> [V.Modifier] -> Desktop -> Desktop
hexKey key mods d = case (activeWindow d,activeDocument d) of
  (Just w,Just doc) -> let
      b=documentBuffer doc; sel=selection w; p=caret sel; size=bufferLength b
      shift=V.MShift `elem` mods; ctrl=V.MCtrl `elem` mods
      move n=moveTo shift n d
      erase a z=let range=if anchor sel/=p then sel else Selection a z
                in editActive (\_ -> replaceSelection range "") (Just (fst (ordered range))) d
      write value low=ensureVisible $ modifyActive (\v -> v {windowHexLow=low}) $
        editActive (\_ -> replaceSelection (if anchor sel/=p then sel else Selection p (min size (p+1))) (T.singleton (chr value)))
          (Just (if low then fst (ordered sel) else fst (ordered sel)+1)) d
      start=fst (ordered sel)
      old=if start<size then maybe 0 (ord.fst) (T.uncons (bufferSlice b start 1)) else 0
      count=windowHexBytes w
      page=max 1 (height (bounds w)-3)*count
    in case key of
      V.KLeft -> move (p-1)
      V.KRight -> move (p+1)
      V.KUp -> move (p-count)
      V.KDown -> move (p+count)
      V.KPageUp -> move (p-page)
      V.KPageDown -> move (p+page)
      V.KHome -> move (if ctrl then 0 else p-p `mod` count)
      V.KEnd -> move (if ctrl then size else min size (p-p `mod` count+count-1))
      V.KBS -> erase (max 0 (p-1)) p
      V.KDel -> erase p (min size (p+1))
      V.KIns -> editActive (\_ -> replaceSelection (Selection p p) "\0") (Just p) d
      V.KChar '\t' -> ensureVisible (modifyActive (\v -> v {windowHexAscii=not (windowHexAscii v),windowHexLow=False}) d)
      V.KChar c | null mods || mods==[V.MShift]
        , windowHexAscii w, c>=' ', c<='~' -> write (ord c) False
        | null mods || mods==[V.MShift]
        , not (windowHexAscii w), isHexDigit c ->
          if windowHexLow w then write (old `div` 16*16+digitToInt c) False
          else write (16*digitToInt c+old `mod` 16) True
      _ -> d
  _ -> d

modeOffset :: Buffer -> Int -> Int
modeOffset b p
  | byteMode b = T.length (TE.decodeUtf8With (\_ _ -> Nothing) (BS.take p (bufferBytes b)))
  | otherwise = BS.length (TE.encodeUtf8 (T.take p (contents b)))
