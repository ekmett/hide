{-# LANGUAGE OverloadedStrings #-}
-- | Shared desktop state, geometry and pure input transitions.
--
-- Documents own buffers; windows refer to documents by ID and keep independent
-- selection, scroll and review state. The same rectangles drive painting and hit
-- testing. Commands and input return ordered effects for the session interpreter,
-- so filesystem and protocol services can remain outside the pure transition.
--
-- Docking and neighboring-window geometry are handled here, as are modal focus,
-- gesture ownership and source/chat editing. Derived equality exists for tests
-- and explicit data operations; never use Desktop, Document or Buffer equality
-- as an interaction or redraw gate. Cached text work belongs to its worker.
module Hide.Model (module Hide.Model,questionChoiceLines,QuestionProjection(..),ConversationBody(..),BodyControlReceipt(..),HostBodyControls(..),BodyPoint(..),BodyAnchor(..),BodySelection(..),BodyDemand(..),BodyViewport(..),BodyRow(..)) where

import qualified Hide.TextLayout as TextLayout
import qualified Data.Bifunctor as Bifunctor
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as M
import qualified Data.Vector as Vec
import qualified Hide.Plugin.Window as PluginWindow
import qualified Hide.Plugin.Canvas as Canvas
import Hide.ConversationBody (CapturedConversationSource,LogicalBody,ConversationCopy(..),BodyPoint(..),BodyAnchor(..),BodySelection(..),BodyDemand(..),BodyViewport(..),BodyRow(..),logicalBodyItemIndex,logicalBodyItems,logicalItemRecord,Record(..),viewportPoint,viewportOffset,questionChoiceLines,QuestionProjection(..),ConversationBody(..),BodyControlReceipt(..),HostBodyControls(..))
import qualified Hide.Privacy as Privacy
import Control.Applicative ((<|>))
import Hide.Sidebar
import Hide.DebuggerSidebarTypes
import Hide.DownloadsWindowTypes
import qualified Hide.Plugin.Form as Form
import qualified Hide.Plugin.Editor as Editor
import Hide.AgentSidebarTypes
import Hide.SessionSidebarTypes
import qualified Hide.Plugin.Tree as Tree
import Hide.Plugin.Command (CommandRef)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import Data.List (find, findIndex, sortOn, mapAccumL, groupBy, nub)
import Data.Char (toLower, isAlphaNum, chr, ord, toUpper, isHexDigit, digitToInt)
import Text.Read (readMaybe)
import System.FilePath ((</>), takeDirectory, takeFileName, takeExtension, isAbsolute, splitDirectories, joinPath, normalise)
import Hide.Browser (Entry(..))
import Hide.Git (GitReview)
import Hide.Syntax (Style(..), StyledText, StyledRow, styledText, styledContents, styledRows, splitStyledText, SourceRow, prepareSourceRow, highlightFor, linkSpans, styleLayoutMetadata)
import Hide.Frontend (modeHeight)
import Hide.Hex
import Hide.Unicode (textInputChar)
import Hide.InlineState
import qualified Data.Set as S
import Hide.Buffer
import Hide.BufferView
import qualified Hide.Bindings as Bindings
import qualified Hide.Plugin.Menu as Plugin
import Hide.Files (FileState(..))

-- | A zero-based character-cell rectangle with exclusive right and bottom edges.
data Rect = Rect { left :: Int, top :: Int, width :: Int, height :: Int } deriving (Eq,Show)
inside :: Rect -> Int -> Int -> Bool
inside (Rect x y w h) a b = a >= x && a < x+w && b >= y && b < y+h

-- | Shared buffer and prepared presentation metadata; split windows reference its ID.
-- documentOrigin retains canonical privacy provenance for generated source. It
-- does not authorize saving, filesystem access or debugger source operations.
data Document = Document { documentBuffer :: Buffer, documentFile :: Maybe FileState, documentLabel :: Maybe Text, documentHighlight :: StyledText, documentHasLayoutMetadata :: !Bool, documentWidth :: Int, documentCursorVisible :: Bool, documentSuggestedName :: Maybe FilePath, documentSourceRows :: Maybe (Vec.Vector SourceRow), documentShellBlocks :: [(Int,Int,Text,Text)], documentLinks :: [(Int,Int,Text)], documentMarkdownPath :: Maybe FilePath, documentOrigin :: Maybe FilePath } deriving (Eq,Show)
-- Source colors are populated by the session worker, never forced by input or drawing.
newDocument :: Buffer -> Maybe FileState -> Document
newDocument b file = restyle (Document b file Nothing [] False 0 True Nothing Nothing [] [] Nothing Nothing)

restyle :: Document -> Document
restyle doc = doc {documentHighlight=[],documentHasLayoutMetadata=False,documentSourceRows=Nothing,documentShellBlocks=[],documentLinks=[],
  documentWidth=if byteMode (documentBuffer doc) then hexWidth 16 else documentWidth doc}

-- | Install prepared styling and its cached layout admission together. The
-- owner replaces content/version before installing new styles; input and layout
-- admission read this flag without traversing the styled payload.
setDocumentHighlight :: StyledText -> Document -> Document
setDocumentHighlight styled doc=doc {documentHighlight=styled,documentHasLayoutMetadata=any (styleLayoutMetadata . snd) styled}

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
  | otherwise = doc {documentHighlight=[],documentHasLayoutMetadata=False,documentSourceRows=Just (sourceHighlightRows text tokens),documentWidth=measureDocumentWidth text}
  where text=contents (documentBuffer doc)
        tokens=highlightFor (documentSyntaxPath doc) text

indexedHighlightRows :: StyledText -> Vec.Vector StyledRow
indexedHighlightRows=Vec.fromList . styledRows

-- | Preserve original source Text while retaining borrowed tokenizer run ranges.
-- Prepare on the highlighting worker; Markdown keeps its semantic styled rows.
sourceHighlightRows :: Text -> StyledText -> Vec.Vector SourceRow
sourceHighlightRows text tokens=Vec.imap (\i row->prepareSourceRow row (fromMaybe [] (styles Vec.!? i))) (Vec.fromList (T.splitOn "\n" text))
  where styles=Vec.fromList (map fst (splitStyledText tokens))

measureDocumentWidth :: Text -> Int
measureDocumentWidth text=maximum (0:[displayColumn line (T.length line) | raw<-textLines text,let line=T.dropWhileEnd (=='\r') raw])

data ReviewSelection = ReviewSelection
  { reviewRevision :: Int, reviewCounts :: (Int,Int), reviewSide :: ReviewSide, reviewRange :: Selection
  } deriving (Eq,Show)

-- | Content identity is separate from host chrome; plugin views have no buffer ID.
data WindowContent = SourceContent !Int | PluginContent !PluginWindow.WindowRef deriving (Eq,Show)
data Window = Window
  { windowId :: Int, windowContent :: WindowContent, bounds :: Rect, selection :: Selection
  , scrollRow :: Int, scrollColumn :: Int, restoredBounds :: Maybe Rect
  , windowHexLow :: Bool, windowHexAscii :: Bool
  , windowNumber :: Int, bufferView :: BufferView, reviewSelection :: Maybe ReviewSelection, reviewSplit :: Int, markdownInteraction :: Maybe MarkdownInteraction, rowsInteraction :: Maybe RowsInteraction, sourceWidthHint :: Maybe SourceWidthHint, windowEditorMount :: Maybe Editor.EditorMount, imageViewport :: Canvas.CanvasView
  } deriving (Eq,Show)
-- Only a thumb estimate, never source identity or proof of EOF. A matching
-- revision/range retains a discovered extent when scrolling left; every actual
-- scroll independently queries live source, including equal-revision replacement.
data SourceWidthHint = SourceWidthHint !Int !Int !Int !Int !Int deriving (Eq,Show)

-- | Only the selected stable row and pane focus; Details uses Window selection/scroll.
data RowsInteraction = RowsInteraction !Tree.NodeId !Bool deriving (Eq,Show)

-- | Bounded source/payload identity for a prepared presentation snapshot.
-- Plugin prepared values compare only their unique identity.
data MarkdownInteraction = MarkdownInteraction !Selection !Int !Int !(Maybe (Int,Int)) deriving (Eq,Show)

-- Source selection and scroll remain authoritative while preview interaction is separate.
data PresentationTarget = DocumentPresentation !Int !Int | MarkdownPresentation !Int !Int
  | PluginPresentation !PluginWindow.WindowRef !PluginWindow.PreparedWindow deriving (Eq,Show)
data WindowPresentation = WindowPresentation !PresentationTarget !Int !Bool !TextLayout.TextLayout
  | MarkdownWindowPresentation !PresentationTarget !Int !Bool !TextLayout.TextLayout !BufferContent ![(Int,Int,Text)]
  | MarkdownWindowFailure !PresentationTarget !Int !Bool
  | WindowPresentationUnneeded !PresentationTarget !Int !Bool
-- Payloads share the layout's immutable identity; never compare rendered text/links.
instance Eq WindowPresentation where
  a==b=presentationMetadata a==presentationMetadata b && presentationLayout a==presentationLayout b
instance Show WindowPresentation where
  show p="WindowPresentation "++show (presentationMetadata p,presentationLayout p)
presentationMetadata :: WindowPresentation -> (PresentationTarget,Int,Bool)
presentationMetadata (WindowPresentation target columns wide _)=(target,columns,wide)
presentationMetadata (MarkdownWindowPresentation target columns wide _ _ _)=(target,columns,wide)
presentationMetadata (MarkdownWindowFailure target columns wide)=(target,columns,wide)
presentationMetadata (WindowPresentationUnneeded target columns wide)=(target,columns,wide)
presentationLayout :: WindowPresentation -> Maybe TextLayout.TextLayout
presentationLayout (WindowPresentation _ _ _ layout)=Just layout
presentationLayout (MarkdownWindowPresentation _ _ _ layout _ _)=Just layout
presentationLayout MarkdownWindowFailure{}=Nothing
presentationLayout WindowPresentationUnneeded{}=Nothing

-- | Read the displayed selection/viewport without changing source interaction.
displayWindow :: Window -> Window
displayWindow w | bufferView w==MarkdownView,Just (MarkdownInteraction selected row column _)<-markdownInteraction w =
  w {selection=selected,scrollRow=row,scrollColumn=column}
displayWindow w=w
modifyDisplayedWindow :: (Window -> Window) -> Window -> Window
modifyDisplayedWindow f w | bufferView w==MarkdownView =
  let shown=f (displayWindow w) in w {markdownInteraction=Just (MarkdownInteraction (selection shown) (scrollRow shown) (scrollColumn shown) (case markdownInteraction w of Just (MarkdownInteraction _ _ _ stamp)->stamp; Nothing->Nothing))}
modifyDisplayedWindow f w=f w
activeMarkdown :: Desktop -> Bool
activeMarkdown d=maybe False ((==MarkdownView).bufferView) (activeWindow d)
markdownDocument :: Document -> Bool
markdownDocument doc=documentLabel doc==Nothing && textBuffer (documentBuffer doc) &&
  maybe False ((`elem` [".md",".markdown"]) . map toLower . takeExtension) (fmap filePath (documentFile doc) `orName` documentSuggestedName doc)
  where orName (Just value) _=Just value; orName Nothing value=value


data Command = New | Open | Download | ChangeDir | Save | SaveAs | Close | Quit | Undo | Redo | Cut | Copy | Paste
  | Find | FindNext | FindPrevious | Replace | GoTo | SelectAll | Zoom | NextWindow | PreviousWindow | Cascade | Tile
  | OpenLink LinkOrigin Text | SplitVertical | SplitHorizontal | ToggleTerminalPin | About | Help | EditorOptions | ChatInputOptions | Gallery
  | InspectType | Definition | Complete | Problems | NextMessage | PreviousMessage | RestartHLS | RenameSymbol | CodeActions
  | ProjectBrowser | ToggleTree | GitDiff | GitCommit | GitFetch | GitPull | GitMerge | ReviewDisk
  | CompileTarget | MakeTarget | StopBuild | RunTarget | RunOptions | OpenTerminal | StopTerminal
  | AgentChoose Text | AgentSet Text Text
  | EnvironmentOptions | AgentDirectory | AgentOptions | AgentPermissions | AgentGuidance | Conversation | AgentCancel | AgentResume | AgentCopyRaw | AgentNew
  | ExecuteShellBlock ShellOrigin (Int,Int,Text,Text)
  | SetBufferView BufferView | SetDefaultBufferView BufferView | RevertChange Int Int (Int,Int) Int
  | ToggleHex | GoToMessage | CopyAllMessages | CopyLocation | SubmitChat ChatSubmit
  | ReloadBindings | InspectBindings
  | CursorLeft Bool | CursorRight Bool | CursorUp Bool | CursorDown Bool
  | CursorRowStart Bool | CursorRowEnd Bool | CursorDocumentStart Bool | CursorDocumentEnd Bool | CursorPageUp Bool | CursorPageDown Bool
  | CursorWordLeft Bool | CursorWordRight Bool | DeleteWordBackward | DeleteWordForward
  | DialogFocusNext | DialogFocusPrevious | DialogAccept | DialogCancel
  | DeleteBackward | DeleteForward | DeleteLine | DeleteSelection
  | WordStarBlockPrefix | WordStarQuickPrefix | MarkBlockStart | MarkBlockEnd
  | SidebarMove Int | SidebarActivate | SidebarExpand | SidebarCollapse | FocusSource | MessagesMove Int | MessagesPage Int
  | ToolchainOptions | SelectToolchain Toolchain | SelectCompiler Text
  | DebugCommand Text | AutocompleteCommand Text
  | TreeCommand [Tree.TreeHit] CommandRef
  | RegisteredMenu Plugin.MenuRef Bool
  | Disabled Text deriving (Eq,Show)
-- | Host-captured shell metadata in the exact source or prepared-window lifetime.
data LinkOrigin = SourceLink !(Maybe FilePath) | WindowLink !PluginWindow.WindowRef !PluginWindow.PreparedWindow !(Maybe FilePath) deriving (Eq,Show)

data ShellOrigin = SourceShell !Int | WindowShell !PluginWindow.WindowRef !PluginWindow.PreparedWindow deriving (Eq,Show)

data ConflictAction = CompareDisk | ReloadDisk | KeepBuffer | SaveConflictAs deriving (Eq,Show)
data Conflict = Conflict { conflictBuffer :: Int, conflictRevision :: Int, conflictBaseline :: FileState, conflictDisk :: Maybe ByteString } deriving (Eq,Show)
data GitAction = FetchRemote | PullRemote | MergeBranch Text deriving (Eq,Show)
data Toolchain = THC | GHC deriving (Eq,Show)
data ContextKind = TreeContext [Tree.TreeHit] [(Text,Command)] | ToolchainContext [(Text,Command)] | LinkContext Command | ShellContext Command | ChangeContext Command | WindowRowsContext | SourceContext | GitContext | MessagesContext | AgentContext [(Text,Command)] deriving (Eq,Show)
-- | Bounded hit target retained while a context popup is open. Messages use a
-- projection generation and optional frozen source location, never message text. The
-- source target keeps a copied expression of at most 4096 characters, never a
-- Buffer or Undo payload. Source edits/reloads and read-only source replacement
-- advance revision; admission additionally captures exact ContentVersion.
-- Reconcile reload guarantees old+1, and checked edits/Git reload derive their
-- replacement from the original buffer.
data ContextTarget = WindowRowTarget !PluginWindow.WindowRef !Tree.NodeId | SidebarTarget [Tree.TreeHit] | SourceTarget
  { sourceTargetWindow :: !Int, sourceTargetBuffer :: !Int, sourceTargetRevision :: !Int
  , sourceTargetSelection :: !Selection, sourceTargetRow :: !Int
  , sourceTargetExpression :: !(Maybe Text), sourceTargetFile :: !(Maybe FilePath) } | ConversationTarget Text | MessagesTarget !Integer !Int !(Maybe (FilePath,Int,Int)) | UnavailableMessagesTarget | UnavailableSourceTarget deriving (Eq,Show)

data LanguageAction = TypeInfo | FindDefinition | Completions | ShowProblems | RestartLanguage | RenameAt Text | RequestCodeActions | ApplyCodeAction Int Int Text deriving (Eq,Show)
data Completion = Completion Text [(Int,Int,Text)] deriving (Eq,Show)
data ProjectAction = LoadProject | ProjectPage Int Int | ProjectDetails Int Int deriving (Eq,Show)
-- | Existing build operation selected by the admitted host command.
data BuildAction = Compile | Make | Run | Test | Benchmark deriving (Eq,Show)
-- | Captured component identity and provider/snapshot scope. The manifest stamp
-- is checked on the preparation worker; final owner checks use only metadata.
data PackageBuildTarget = PackageBuildTarget
  { packageBuildProvider :: !Tree.TreeRef, packageBuildVersion :: !Int
  , packageBuildScope :: !(FilePath,[FilePath]), packageBuildRoot :: !FilePath
  , packageBuildManifest :: !FilePath, packageBuildStamp :: !(Maybe (UTCTime,Integer))
  , packageBuildName :: !Text } deriving (Eq,Show)

-- | Ordered requests for the host interpreter, produced alongside a new desktop.
data Effect = CopyConversation !ConversationCopy | ExecuteShellBlockAction !ShellOrigin !(Int,Int,Text,Text) | SubmitEditor !Editor.EditorMount !Editor.EditorSlot !Plugin.MenuOrigin | RetireEditorMount !Editor.EditorMount | PackageDebugAction !PackageBuildTarget !(Either Text FilePath) | AdoptPreparedDebug !PackageBuildTarget | PackageBuildAction !BuildAction !PackageBuildTarget | AdoptPreparedBuild !(Maybe PackageBuildTarget) | DownloadCancelAction !DownloadCancelRequest | SubmitInputForm !Form.FormRef !Form.FormValue !Plugin.MenuOrigin | SubmitChoiceForm !Form.FormRef !Integer !Int !Plugin.MenuOrigin | RetireInputForm !Form.FormRef | SessionSidebarAction !SessionSidebarRequest | DebugSourceAction !DebugSourceRequest | RetirePluginWindow !PluginWindow.WindowRef | DebugSidebarAction !DebugSidebarRequest | AgentSidebarAction !AgentSidebarRequest | ReloadKeyBindings FilePath | InspectKeyBindings (Maybe (Bindings.BindingPlatform,Bindings.BindingContext)) (Maybe (Bindings.Bindings Command)) | FollowLink !LinkOrigin Text | FollowTreeLink [Tree.TreeHit] FilePath Text | EnvironmentAction Text [Text] | AutocompleteAction Text [Text] | SaveWideSectionTitles Bool | SaveMacKeySymbols Bool | SaveChatSubmit ChatSubmit | SaveBufferViewDefault BufferView | ProjectRequest ProjectAction | DownloadDocument Int | ReadBrowserClipboard | WriteBrowserClipboard Text | LanguageRequest LanguageAction | RunGit GitAction | ReadMergeBranches | JumpTo FilePath Int Int | ReadPath FilePath | OpenFile !Plugin.MenuOrigin !FilePath | OpenFileBytes !Text !ByteString | BrowsePath FilePath Text | BrowseDirectories FilePath | ChangeDirectory FilePath | OpenChoice !Plugin.MenuOrigin FilePath Text Text | ReadTree FilePath | RefreshRenamedPath FilePath FilePath | RefreshTree FilePath [Entry] | LoadTree TreeRequest Plugin.MenuOrigin | InvokeTree [Tree.TreeHit] CommandRef Plugin.MenuOrigin | ReadHelp | InvokeMenu Plugin.MenuRef Plugin.MenuOrigin (Maybe ContextTarget) | RefreshGit FilePath | ReadGitDiff | AskGitCommit | WriteGitCommit Text | SaveDocument Int (Maybe FilePath) (Maybe Command) | ReviewExternal | ResolveConflict Conflict ConflictAction | ServiceAction Text [Text] | AgentAction Text [Text] | PermissionAction Text [Text] | DebugAction Text [Text] | SetScreenMode Int | Exit deriving (Eq,Show)
data Field = Input Text Text Int | SelectedInput Text Text Selection | ComboBox Text [Text] Int (Maybe Int) | CheckBox Text Bool | Radio Text [Text] Int | ListBox Text [Text] Int | FileList [Entry] Int
  | ReadOnly Text Text
  | TextArea Text Bool Buffer Selection Int Int deriving (Eq,Show)
data Purpose = Opening FilePath Text [Entry] | ChangingDirectory FilePath [Entry] | Committing | Saving Int (Maybe Command) | Searching Bool Text | GoingTo | Renaming
  | ProjectLoading Int | ProjectChoices Int Int
  | CodeActionChoices Int Int [Text]
  | Completing Int Int Int [Completion] | Locations [(FilePath,Int,Int)] | Merging [Text]
  | PluginInputForm !Form.FormRef | PluginInputsForm !Form.FormRef ![Text]
  | PluginChoiceForm !Form.FormRef !Integer
  | EnvironmentDialog Text | AutocompleteDialog Text | DiskConflict Conflict | ServiceDialog Text | AgentDialog Text | PermissionDialog Text | DebugDialog Text
  | DebugSourceWatchDialog !Int !Int !(Maybe FilePath) !Bool
  | DebuggerWatchDialog !Int !(Maybe FilePath) !Bool
  | DiscardDraft | Confirm Command | Information | Settings | ChatInputSettings | Widgets deriving (Eq,Show)
data Dialog = Dialog
  { dialogTitle :: Text, purpose :: Purpose, fields :: [Field], focus :: Int
  , buttons :: [Text], body :: [Text]
  } deriving (Eq,Show)
data Diagnostic = Diagnostic
  { diagnosticPath :: FilePath, diagnosticVersion :: Maybe Int, diagnosticRow :: Int
  , diagnosticColumn :: Int, diagnosticSeverity :: Int, diagnosticMessage :: Text
  } deriving (Eq,Show)
data Drag = FollowingLink Int Int Int LinkOrigin Text | ImagePanning Int Int Int Canvas.CanvasView | ReviewSizing Int | DockSizing | MessagesSizing | TreeScrolling | Moving Int Int Int | Resizing Int Int Int | EdgeSizing Int Bool Bool Int | Selecting Int | Scrolling Int Bool deriving (Eq,Show)
data AgentSetting = AgentSetting { settingId :: Text, settingName :: Text, settingCategory :: Text, settingCurrent :: Text, settingChoices :: [(Text,Text)] } deriving (Eq,Show)
-- | The human-selected composer action for Enter; Ctrl+Enter uses the other action.
data ChatSubmit = QuerySubmit | SteerSubmit deriving (Eq,Show,Enum,Bounded)
chatSubmitName :: ChatSubmit -> Text
chatSubmitName QuerySubmit="query"
chatSubmitName SteerSubmit="steer"
parseChatSubmit :: Text -> Maybe ChatSubmit
parseChatSubmit name=lookup name [("query",QuerySubmit),("steer",SteerSubmit)]

data Appearance = LightMode | DarkMode | SystemMode deriving (Eq,Show,Enum,Bounded)

darkAppearance :: Desktop -> Bool
darkAppearance d = case appearance d of LightMode -> False; DarkMode -> True; SystemMode -> systemDark d

data ChatQuestion = ChatQuestion
  { questionToken :: Int, questionText :: Text, questionChoices :: [Text]
  , questionChoice :: Maybe Int, questionBuffer :: Buffer, questionSelection :: Selection
  , questionFocused :: Bool
  } deriving (Eq,Show)

-- One retained body and draft per target. Installed content lives only in the
-- plugin window map; closed bodies retain an inert immutable snapshot.
data ConversationView = ConversationView
  { conversationBody :: ConversationBody, conversationName :: Text
  , conversationDraftRef :: Editor.DraftRef, conversationEditor :: Maybe Editor.EditorMount, conversationEditorFrame :: Maybe Int
  , conversationAnchor :: !BodyAnchor, conversationRowShift :: !Int, conversationScrollColumn :: !Int
  , conversationReplySelection :: !(Maybe BodySelection)
  , conversationLogical :: Maybe LogicalBody
  , conversationCaretIntent :: Maybe ConversationCaretIntent
  , conversationSource :: Maybe CapturedConversationSource
  } deriving (Eq,Show)

-- Finite input intent resolved through the next exact viewport receipt.
data ConversationCaretIntent
  = EdgeCaret !Bool !(Maybe BodyPoint)
  | RowCaret !Int !(Maybe BodyPoint)
  deriving (Eq,Show)

-- One editable state per opaque widget, including hidden conversation targets.
-- The last mount is an ownership receipt retained after frame close; it is not
-- itself proof of current input admission. Callable bindings live in IO owners.
data EditorDraft = EditorDraft
  { editorDraftBuffer :: Buffer, editorDraftSelection :: Selection
  , editorDraftFocused :: Bool, editorDraftMount :: Maybe Editor.EditorMount
  } deriving (Eq,Show)

-- Shared editing selects the actual owner; temporary question/hint projections
-- never copy their Buffer into another Desktop field or allocate a fake draft.
data EditingInput = MountedInput | HintInput | QuestionInput deriving (Eq,Show)

-- One transient export offer. Frontends retire only the serial they consumed;
-- recovery never persists file bytes or an armed native gesture.
data FileExport = ExportFileCopy !Text !ByteString !Rect ![Integer] deriving (Eq,Show)

-- | Session UI state and references to immutable document payloads.
-- This record is not a cheap equality key; use the dedicated rendering projection.
data Desktop = Desktop
  { screenSize :: (Int,Int), windows :: [Window], buffers :: M.Map Int Document
  , pluginWindows :: M.Map PluginWindow.WindowRef PluginWindow.PreparedWindow
  , retiredPluginWindows :: S.Set PluginWindow.WindowRef
  , nextId :: Int, menu :: Maybe (Int,Int), dialog :: Maybe Dialog, drag :: Maybe Drag
  , clipboard :: Text, clipboardCode :: Maybe Text, wordStar :: Bool, prefix :: Maybe Char, status :: Text
  , blockStart :: Maybe (Int,Int), lastFind :: Text, sideTree :: Maybe Sidebar, branchStatus :: Text, nativeMac :: Bool, gitReview :: Maybe GitReview, videoMode :: Maybe Int
  , hoverTarget :: Maybe (Int,Int,Int), typeHint :: Text
  , buttonHover :: Maybe Int, buttonPressed :: Maybe Int, contextMenu :: Maybe (Rect,Int)
  , diagnostics :: [Diagnostic], problemsVisible :: Bool, problemsSelected :: Int, problemsScroll :: Int, problemsFocused :: Bool
  , dragOriginal :: Maybe [(Int,Rect,Maybe Rect)]
  , branchAdded :: Int, branchDeleted :: Int, branchRoot :: Maybe FilePath, contextKind :: ContextKind
  , messagesNumber :: Maybe Int
  , editorDrafts :: M.Map Editor.DraftRef EditorDraft, editingInput :: EditingInput, agentSteering :: Bool, agentReplying :: Bool, agentQueued :: Int
  , blinkCursor :: Bool, crtFilter :: Bool, pixelateUnicode :: Bool, materialIcons :: Bool, defaultDirectory :: Maybe FilePath, statusHover :: Maybe Int, heldModifiers :: [V.Modifier], problemsPreferredHeight :: Int, agentContextUsage :: Maybe (Integer,Integer), agentSettings :: [AgentSetting], browserFrontend :: Bool, appearance :: Appearance, systemDark :: Bool, buildDiagnostics :: [Diagnostic]
  , chatQuestion :: Maybe ChatQuestion
  , childAgentSettings :: [AgentSetting], childAgentSteering :: Bool, childAgentContextUsage :: Maybe (Integer,Integer)
  , conversationTarget :: Text, conversationViews :: M.Map Text ConversationView
  , streamerMode :: Bool, clipboardExport :: (Int,Maybe Text), guestPrivatePaths :: [FilePath]
  , toolchain :: Maybe Toolchain
  , defaultBufferView :: BufferView, chatSubmit :: ChatSubmit
  , inlinePreview :: Maybe InlineView, inlineEpoch :: Int
  , dockedTerminals :: M.Map Int (Rect,Maybe Rect), bottomTerminal :: Maybe Int
  , autocompleteACPEnabled :: Bool, autocompleteDraft :: Buffer
  , autocompleteSelection :: Selection, autocompleteFocused :: Bool, macKeySymbols :: Bool
  , keyBindings :: M.Map (Bindings.BindingPlatform,Bindings.BindingContext) (Bindings.Bindings Command)
  , contributedMenus :: [Plugin.MenuItem], agentMenuRefs :: [Plugin.MenuRef], menusActive :: Bool
  , contextTarget :: Maybe ContextTarget
  , wideSectionTitles :: !Bool, windowPresentations :: M.Map Int WindowPresentation
  , diagnosticsGeneration :: !Integer
  , pendingFileExport :: (Int,Maybe FileExport)
  } deriving (Eq,Show)

data MenuItem = MenuItem Text Text Command deriving (Eq,Show)
menus :: [(Text,Char,[MenuItem])]
-- Docs: docs/site/screenshots/{file-menu,debug-menu,window-views-menu}.png (docs/editing.md, docs/running.md).
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
  ,("Tools",'t',[mi "File tree" "Ctrl+B" ToggleTree,mi "Git diff..." "" GitDiff,mi "Approve changes..." "" GitCommit,mi "Inspect type" "Shift+F1" InspectType,mi "Code actions..." "" CodeActions,mi "Messages" "" Problems,mi "Go to next" "Alt+F8" NextMessage,mi "Go to previous" "Alt+F7" PreviousMessage,mi "Restart language server" "" RestartHLS,mi "Conversation" "Ctrl+Shift+C" Conversation,mi "Agents..." "" AgentDirectory,mi "Conversation model..." "" (AgentChoose ""),mi "Cancel reply" "" AgentCancel,mi "Resume session..." "" AgentResume,mi "New conversation" "Ctrl+Shift+N" AgentNew,mi "Copy raw conversation" "" AgentCopyRaw,mi "Widget gallery..." "" Gallery,mi "Project browser..." "" ProjectBrowser,mi "Downloads..." "" (DebugCommand "downloads")])
  ,("Options",'o',[mi "Preferences..." "" EditorOptions,mi "Environment..." "" EnvironmentOptions,mi "Chat input..." "" ChatInputOptions,mi "Autocomplete..." "" (AutocompleteCommand "settings"),mi "Agents..." "" AgentOptions,mi "Agent Permissions" "" AgentPermissions,mi "Agent Context..." "" AgentGuidance,mi "Reload keybindings" "" ReloadBindings,mi "Inspect keybindings" "" InspectBindings])
  ,("Window",'w',[mi "Agents..." "" AgentDirectory,mi "Tile" "" Tile,mi "Cascade" "" Cascade,mi "Split vertically" "" SplitVertical,mi "Split horizontally" "" SplitHorizontal,mi "Zoom" "F5" Zoom,mi "Pin / unpin terminal" "" ToggleTerminalPin,mi "Next" "F6" NextWindow,mi "Close" "Alt+F3" Close,mi "" "" (Disabled ""),mi "Current" "" (SetBufferView CurrentView),mi "Changes" "" (SetBufferView ChangesView),mi "Only Changes" "" (SetBufferView OnlyChangesView),mi "Side by Side" "" (SetBufferView SideBySideView),mi "Markdown" "" (SetBufferView MarkdownView)])
  ,("Help",'h',[mi "Contents" "F1" Help,mi "About Haskell..." "" About])]
  where mi = MenuItem

menuMnemonic :: MenuItem -> Char
menuMnemonic (MenuItem title _ cmd) = case cmd of
  ProjectBrowser -> 'b'
  ToggleTree -> 'f'; GitDiff -> 'g'; GitCommit -> 'a'
  Problems -> 'm'; NextMessage -> 'n'; PreviousMessage -> 'p'
  SaveAs -> 'a'; Quit -> 'x'; Cut -> 't'; SelectAll -> 'a'
  SplitVertical -> 'v'; SplitHorizontal -> 'h'
  Close -> 'l'; Download -> 'w'
  _ -> if T.null title then '\0' else toLower (T.head title)

-- Display labels never change terminal input bindings. Command belongs to the
-- native menu; terminal users may independently opt into Mac modifier symbols.
keyLabel :: Desktop -> Text -> Text
keyLabel d text
  | nativeMac d || videoMode d==Nothing && macKeySymbols d =
      foldl' (\s (from,to) -> T.replace from to s) text
        [("Cmd+Alt+Shift+","⌥⇧⌘"),("Cmd+Alt+","⌥⌘"),("Cmd+Shift+","⇧⌘"),("Cmd+Option+","⌥⌘"),("Cmd+","⌘"),
         ("Option+","⌥"),("Alt+","⌥"),("Shift+","⇧"),("Ctrl+","⌃")]
  | otherwise = text

keyLabelWidth :: Text -> Int
keyLabelWidth text = displayColumn text (T.length text)

menuShortcut :: Desktop -> MenuItem -> Text
menuShortcut d (MenuItem _ key cmd)
  | Just _<-effectiveBindings d = keyLabel d (fromMaybe "" (listToMaybe (commandBindingKeys d cmd)))
  | nativeMac d = keyLabel d $ fromMaybe key (lookup cmd [(New,"Cmd+N"),(Open,"Cmd+O"),(Save,"Cmd+S"),(SaveAs,"Cmd+Shift+S"),(Close,"Cmd+W"),(Quit,"Cmd+Q"),(Undo,"Cmd+Z"),(Redo,"Cmd+Shift+Z"),(Copy,"Cmd+C"),(Cut,"Cmd+X"),(Paste,"Cmd+V"),(SelectAll,"Cmd+A"),(Find,"Cmd+F"),(Replace,"Cmd+Option+F"),(FindNext,"Cmd+G"),(FindPrevious,"Cmd+Shift+G"),(Conversation,"Cmd+Shift+C"),(AgentNew,"Cmd+Shift+N")])
  | otherwise = keyLabel d key

-- | Labels and native accelerators share the effective command identity. The
-- live registered Help action uses the configurable built-in Help key entry.
commandBindingKeys :: Desktop -> Command -> [Text]
commandBindingKeys d cmd
  | dialog d/=Nothing, not (dialogCommandAllowed cmd d) = []
  | wordStarPrefixOwner d, Just _<-table Bindings.WordStarKeys =
      maybe (keys Bindings.WordStarKeys shortcutCommand++sequences Bindings.WordStarBlockKeys WordStarBlockPrefix++sequences Bindings.WordStarQuickKeys WordStarQuickPrefix)
        (\context->sequences context (if context==Bindings.WordStarBlockKeys then WordStarBlockPrefix else WordStarQuickPrefix)) (wordStarPrefixContext d)
  | otherwise = maybe [] (\bindings->Bindings.bindingKeys bindings shortcutCommand) (effectiveBindings d)
  where table context=M.lookup (bindingPlatform d,context) (keyBindings d)
        keys context action=maybe [] (\bindings->Bindings.bindingKeys bindings action) (table context)
        sequences context starter=[first<>" "<>second | first<-keys Bindings.WordStarKeys starter,second<-keys context shortcutCommand]
        shortcutCommand=case cmd of
          RegisteredMenu ref _ | Plugin.menuName ref=="hide.help.contents" -> Help
                               | Plugin.menuName ref=="hide.messages.go-to" -> GoToMessage
                               | Plugin.menuName ref=="hide.debug.toggle-breakpoint" -> DebugCommand "breakpoint"
          _ -> cmd

commandDescription :: Command -> Text
commandDescription cmd = case cmd of
  WordStarBlockPrefix -> "Begin a WordStar block command."
  WordStarQuickPrefix -> "Begin a WordStar quick command."
  MarkBlockStart -> "Mark the current source caret as the block start."
  MarkBlockEnd -> "Select from the marked source block start to the current caret."
  DeleteSelection -> "Delete the current source selection."
  DialogAccept -> "Accept the focused dialog control or submit its current button."
  DialogCancel -> "Revert an open dropdown or cancel the current dialog."
  DialogFocusNext -> "Focus the next dialog control, committing an open dropdown preview."
  DialogFocusPrevious -> "Focus the previous dialog control, committing an open dropdown preview."
  CursorLeft False -> "Move to the previous character."
  CursorRight False -> "Move to the next character."
  CursorLeft True -> "Extend selection to the previous character."
  CursorRight True -> "Extend selection to the next character."
  CursorUp False -> "Move to the previous displayed row."
  CursorDown False -> "Move to the next displayed row."
  CursorUp True -> "Extend selection to the previous displayed row."
  CursorDown True -> "Extend selection to the next displayed row."
  CursorRowStart False -> "Move to the start of the displayed row."
  CursorRowStart True -> "Extend selection to the start of the displayed row."
  CursorRowEnd False -> "Move to the end of the displayed row."
  CursorRowEnd True -> "Extend selection to the end of the displayed row."
  CursorDocumentStart False -> "Move to the start of the displayed document."
  CursorDocumentStart True -> "Extend selection to the start of the displayed document."
  CursorDocumentEnd False -> "Move to the end of the displayed document."
  CursorDocumentEnd True -> "Extend selection to the end of the displayed document."
  CursorPageUp False -> "Move to the previous page."
  CursorPageUp True -> "Extend selection to the previous page."
  CursorPageDown False -> "Move to the next page."
  CursorPageDown True -> "Extend selection to the next page."
  CursorWordLeft False -> "Move left by the source word or current view step."
  CursorWordRight False -> "Move right by the source word or current view step."
  CursorWordLeft True -> "Extend selection left by the source word or current view step."
  CursorWordRight True -> "Extend selection right by the source word or current view step."
  DeleteWordBackward -> "Delete the selected range or previous source word (one hex byte)."
  DeleteWordForward -> "Delete the selected range or next source word (one hex byte)."
  DeleteBackward -> "Delete the selected range or previous character."
  DeleteForward -> "Delete the selected range or next character."
  DeleteLine -> "Delete the current source line."
  ReloadBindings -> "Reload and validate bindings from global and project configuration."
  InspectBindings -> "Show every effective command binding for the focused profile and context."
  TreeCommand _ _ -> "Run the captured sidebar action."
  SidebarMove _ -> "Move the selected sidebar row."
  SidebarActivate -> "Open the selected file or toggle its directory."
  SidebarExpand -> "Expand the selected directory or open its file."
  SidebarCollapse -> "Collapse the selected directory or select its parent."
  FocusSource -> "Return focus to the source window."
  MessagesMove _ -> "Move the selected message."
  MessagesPage _ -> "Move through messages by one visible page."
  AutocompleteCommand _ -> "Configure inline code suggestions and sign in to Copilot."
  New -> "Create a new source buffer."; Open -> "Browse directories and open a file."
  ChangeDir -> "Choose a new default directory."
  Save -> "Save the active file."; SaveAs -> "Save the active buffer under a new filename."
  GoToMessage -> "Jump to the selected message in its source file."
  CopyAllMessages -> "Copy all messages with their source locations."
  CopyLocation -> "Copy this file’s path, line and column as plain text."
  SubmitChat _ -> "Submit the draft to the selected agent."
  ProjectBrowser -> "Browse local Cabal components and their dependency graph without starting a build."
  ExecuteShellBlock{} -> "Run this shell code block in a new terminal."
  SetBufferView _ -> "Change this window’s buffer view. Click the radio or press Space to choose the default for new buffers."
  SetDefaultBufferView _ -> "Choose the default view for newly opened buffers."
  ChatInputOptions -> "Choose whether Enter queries or steers the selected agent; Ctrl+Enter uses the other action."
  RevertChange{} -> "Restore this changed block from the last load or save."
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
  EnvironmentOptions -> "Inspect and change the environment inherited by new processes."
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
  NextWindow -> "Activate the next editor window."; PreviousWindow -> "Activate the previous editor window."; Cascade -> "Arrange windows in an overlapping stack."
  Tile -> "Arrange windows in horizontal rows."
  SplitVertical -> "Create a side-by-side view of the same buffer."
  SplitHorizontal -> "Create a view of the same buffer above or below."
  OpenLink _ target -> "Open "<>target
  About -> "Show information about Haskell."; Help -> "Open the read-only help document."
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
  RegisteredMenu reference _ | Plugin.menuName reference=="hide.help.contents" -> "Open the read-only help document."
                            | Plugin.menuName reference=="hide.messages.go-to" -> "Go to the captured diagnostic source location."
                            | otherwise -> "Run the selected extension action."
  Disabled reason -> reason

menuHelp :: Desktop -> Maybe Text
menuHelp d = case contextMenu d of
  Just (_,i) -> commandDescription . snd <$> listToMaybe (drop i (contextItemsFor d))
  Nothing -> do
    (i,j)<-menu d
    MenuItem _ _ cmd<-listToMaybe (drop j (menuItemsFor d i))
    pure (commandDescription cmd)

-- Labels, hit rectangles and actions share one source, including modal hints.
statusItems :: Desktop -> [(Text,Maybe (Either Command V.Event))]
statusItems d=statusHints d++[(toolchainBadgeText d,if dialog d==Nothing then Just (Left ToolchainOptions) else Nothing) | not (T.null (toolchainBadgeText d))]

statusHints :: Desktop -> [(Text,Maybe (Either Command V.Event))]
statusHints d = [(if action==Nothing then text else keyLabel d text,action) | (text,action)<-statusHintsRaw d]

statusHintsRaw :: Desktop -> [(Text,Maybe (Either Command V.Event))]
statusHintsRaw d
  | dragOriginal d/=Nothing = [(keyLabel d " ↑↓→← Move  Shift+↑↓→← Resize",Nothing),key "  ↵ Done" V.KEnter [],key "  Esc Cancel" V.KEsc []]
  | Just text<-menuHelp d = [command " F1" "Help" Help,(" | "<>text,Nothing)]
  | Just c<-prefix d, wordStarPrefixOwner d = [(prefixHint c,Nothing),key " Esc Cancel" V.KEsc []]
  | Just dg<-dialog d, approvalDialog dg = [command " Tab" "Next" DialogFocusNext,key "  Alt+A Allow" (V.KChar 'a') [V.MAlt],key "  Alt+D Deny" (V.KChar 'd') [V.MAlt],command "  Esc" "Deny" DialogCancel]
  | Just dg<-dialog d, searching dg = [key " Ctrl+Tab Find/Replace" (V.KChar '\t') [V.MCtrl],command "  Tab" "Next" DialogFocusNext,command "  Enter" "Apply" DialogAccept,command "  Esc" "Cancel" DialogCancel] ++
      [command (if nativeMac d then "  Cmd+Alt+F" else "  Ctrl+H") "Replace" Replace]
  | dialog d/=Nothing = [command " Tab" "Next" DialogFocusNext,command "  Enter" "Select" DialogAccept,command "  Esc" "Cancel" DialogCancel] ++
      [command ((if nativeMac d then "  Cmd+" else "  Ctrl+")<>keyName) label cmd | (keyName,label,cmd)<-[("C","Copy",Copy),("V","Paste",Paste)],dialogCommandAllowed cmd d]
  | problemsVisible d && problemsFocused d = [command " Enter" "Source" GoToMessage,command (if nativeMac d then "  Cmd+C" else "  Ctrl+C") "Copy" Copy,command " " "Copy all" CopyAllMessages]
  | Just v<-inlinePreview d,inlineMatches d v = [key " Tab Accept" (V.KChar '\t') [],key "  Alt+Right Word" V.KRight [V.MAlt],key (if nativeMac d then "  Cmd+[ Previous" else "  Alt+[ Previous") (V.KChar '[') [V.MAlt],key (if nativeMac d then "  Cmd+] Next" else "  Alt+] Next") (V.KChar ']') [V.MAlt],key "  Esc Dismiss" V.KEsc []]
  | activeAutocomplete d = [key " Enter Send hint" V.KEnter [],key "  Shift+Enter Newline" V.KEnter [V.MShift],key "  Tab Transcript / hint" (V.KChar '\t') []]
  | questionActive d = [key " Enter Answer" V.KEnter [],key "  Tab Choices" (V.KChar '\t') [],key "  Esc Cancel" V.KEsc []]
  | composerActive d, activeConversation d, composerInCode d =
      [key " Enter Newline" V.KEnter []] ++
      [("  "<>submitHint action,Just (Left (SubmitChat action))) | action<-[QuerySubmit,SteerSubmit]] ++
      [key "  Esc Cancel" V.KEsc [] | agentReplying d]
  | composerActive d,not (activeConversation d),Just mount<-activeEditorMount d =
      [key (" Enter "<>Editor.editorDefaultLabel (Editor.mountSpec mount)) V.KEnter [],
       key ("  Ctrl+Enter "<>Editor.editorAlternateLabel (Editor.mountSpec mount)) V.KEnter [V.MCtrl],
       key "  Shift+Enter Newline" V.KEnter [V.MShift]]
  | activeConversation d =
      [key (" Enter "<>submitLabel False) V.KEnter [],key ("  Ctrl+Enter "<>submitLabel True) V.KEnter [V.MCtrl],
       key "  Shift+Enter Newline" V.KEnter [V.MShift]] ++ [key "  Esc Cancel" V.KEsc [] | agentReplying d]
  | not (T.null (typeHint d)) = [(" "<>typeHint d,Nothing)]
  | not (T.null (status d)) = [command " F1" "Help" Help,(" | "<>status d,Nothing)]
  | Just _<-activePluginWindow d = [command " Ctrl+C" "Copy" Copy,command "  Alt+F3" "Close" Close,(" | Read-only plugin text",Nothing)]
  | otherwise = [command " F1" "Help" Help,command "  F2" "Save" Save,command "  F3" "Open" Open,
      command "  Alt+F9" "Compile" CompileTarget,command "  F9" "Make" MakeTarget,command "  Ctrl+F9" "Run" RunTarget]
  where prefixHint c=case M.lookup (bindingPlatform d,Bindings.WordStarKeys) (keyBindings d) of
          Just bindings -> " "<>fromMaybe "" (listToMaybe (Bindings.bindingKeys bindings (if c=='k' then WordStarBlockPrefix else WordStarQuickPrefix)))<>" … "
          Nothing -> " Ctrl+"<>T.singleton c<>"- "
        command shortcut caption cmd=(effective shortcut caption cmd,Just (Left cmd))
        effective _ caption cmd | Just _<-effectiveBindings d =
          " "<>maybe "" (<>" ") (listToMaybe (commandBindingKeys d cmd))<>caption
        effective shortcut caption _=shortcut<>" "<>caption
        key label k mods=(label,Just (Right (V.EvKey k mods)))
        submitHint action=(if action/=chatSubmit d then "Ctrl+Enter " else "")<>(if action==SteerSubmit then "Steer" else if agentReplying d then "Queue query" else "Query")
        submitLabel opposite=if composerQuery opposite d then if agentReplying d then "Queue query" else "Query" else "Steer"

statusItemRects :: Desktop -> [(Rect,Int,Either Command V.Event)]
statusItemRects d = [(Rect x (snd (screenSize d)-1) (min (keyLabelWidth text) (limit-x)) 1,i,action)
  | (i,(x,(text,Just action)))<-zip [0..] (zip starts items), x<limit] ++
  [(toolchainBadgeRect d,length items,Left ToolchainOptions) | dialog d==Nothing, not (T.null (toolchainBadgeText d))]
  where
    items=statusHints d; starts=scanl (+) 0 (map (keyLabelWidth . fst) items)
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
  where
    original=menuItems i
    slot=let (title,_,_)=menus !! (i `mod` length menus) in T.toLower title
    additions=[MenuItem (Plugin.menuTitle item) (Plugin.menuKey item) (contributionCommand d item) | item<-contributedMenus d,Plugin.menuSlot item==slot]
    helpEntry=[item | item@(MenuItem _ _ (RegisteredMenu ref _))<-additions,Plugin.menuName ref=="hide.help.contents"]
    sourceEntry=find ((=="hide.debug.toggle-breakpoint") . Plugin.menuName . Plugin.menuReference) (contributedMenus d)
    replaceSource (MenuItem title key (DebugCommand "breakpoint"))=case sourceEntry of
      Just item->MenuItem title key (contributionCommand d item)
      Nothing->MenuItem title key (if menusActive d then Disabled "Breakpoint command is unavailable." else DebugCommand "breakpoint")
    replaceSource item=item
    items=map replaceSource $ case helpEntry of
      first:_ -> [if cmd==Help then first else item | item@(MenuItem _ _ cmd)<-original]++[item | item<-additions,item/=first]
      [] -> original++additions

-- | Agent enablement is intersected with exact refs granted by the host policy;
-- an extension's metadata cannot grant authority or change protected controls.
contributionCommand :: Desktop -> Plugin.MenuItem -> Command
contributionCommand d item=RegisteredMenu reference (Plugin.menuAgentAllowed item && reference `elem` agentMenuRefs d)
  where reference=Plugin.menuReference item

-- | Resolve a transported entry against its exact prepared lifetime. Unknown
-- names and retired generations never rediscover the currently focused action.
contributedCommand :: Text -> Text -> Integer -> Desktop -> Maybe Command
contributedCommand name epoch generation d=do
  item<-find (\item->Plugin.menuName (Plugin.menuReference item)==name && Plugin.menuEpoch (Plugin.menuReference item)==epoch && Plugin.menuGeneration (Plugin.menuReference item)==generation) (contributedMenus d)
  pure (contributionCommand d item)

commandEnabled :: Desktop -> Command -> Bool
commandEnabled d cmd | cmd `elem` [DialogFocusNext,DialogFocusPrevious,DialogAccept,DialogCancel] = dialogCommandAllowed cmd d
commandEnabled d cmd | dialogCommandAllowed cmd d = True
commandEnabled d cmd | activeMarkdown d, markdownSourceCommand cmd = False
commandEnabled d cmd | activePluginWindow d/=Nothing, sourceOnlyCommand cmd,not ((composerActive d || questionActive d) && cmd `elem` [Undo,Redo,Cut,Paste]) = False
commandEnabled d Download = browserFrontend d && maybe False ((==Nothing) . documentLabel) (activeDocument d)
commandEnabled d GoToMessage | menusActive d = maybe False (commandEnabled d . contributionCommand d) (find ((=="hide.messages.go-to") . Plugin.menuName . Plugin.menuReference) (contributedMenus d))
commandEnabled d (DebugCommand "breakpoint") | menusActive d = maybe False (commandEnabled d . contributionCommand d) (find ((=="hide.debug.toggle-breakpoint") . Plugin.menuName . Plugin.menuReference) (contributedMenus d))
commandEnabled d Help | menusActive d = any ((=="hide.help.contents") . Plugin.menuName . Plugin.menuReference) (contributedMenus d)
commandEnabled d (TreeCommand trace _) = dialog d==Nothing && maybe False (hitCurrent trace) (sideTree d)
commandEnabled d (RegisteredMenu reference _) = dialog d==Nothing && case find ((==reference) . Plugin.menuReference) (contributedMenus d) of
  Nothing -> False
  Just item | Plugin.menuSlot item=="context.source" -> case sourceInvocationTarget d of
    Just target@SourceTarget{} -> contextTargetCurrent d {contextTarget=Just target} && maybe False (textBuffer . documentBuffer) (activeDocument d)
    _ -> False
  Just item | Plugin.menuSlot item=="context.window-rows" -> reference `elem` windowRowMenuRefs d && maybe False (\target->contextTargetCurrent d {contextTarget=Just target}) (rowInvocationTarget d)
  Just item | Plugin.menuSlot item=="context.messages" -> case messageInvocationTarget d of
    Just target@(MessagesTarget _ _ location) -> contextTargetCurrent d {contextTarget=Just target} &&
      (Plugin.menuName reference/="hide.messages.go-to" || location/=Nothing)
    _ -> False
  Just _ -> True
commandEnabled d cmd | cmd `elem` [WordStarBlockPrefix,WordStarQuickPrefix] = wordStarPrefixOwner d
commandEnabled d cmd | cmd `elem` [MarkBlockStart,MarkBlockEnd] = sourceNavigationOwner d && not (activeHex d) && not (activeMarkdown d) && maybe False ((==Nothing) . documentLabel) (activeDocument d)
commandEnabled d cmd | sourceKeyCommand cmd = sourceNavigationOwner d &&
  (not (activeMarkdown d) || maybe False (isJust . windowMarkdown d) (activeWindow d)) &&
  (not (horizontalMutation cmd) || maybe False ((==Nothing) . documentLabel) (activeDocument d) && not (activeMarkdown d)) &&
  (cmd `notElem` [DeleteLine,DeleteSelection] || not (activeHex d))
commandEnabled _ Disabled{} = False
commandEnabled d SidebarMove{} = maybe False treeFocused (sideTree d)
commandEnabled d cmd | cmd `elem` [SidebarActivate,SidebarExpand,SidebarCollapse] = maybe False treeFocused (sideTree d)
commandEnabled d MessagesMove{} = problemsFocused d
commandEnabled d MessagesPage{} = problemsFocused d
commandEnabled d (ExecuteShellBlock origin block) = shellBlockCurrent d origin block
commandEnabled d (SetBufferView MarkdownView) = maybe False markdownDocument (activeDocument d)
commandEnabled d (SetBufferView _) = maybe False (\doc -> documentLabel doc==Nothing && textBuffer (documentBuffer doc)) (activeDocument d)
commandEnabled d (RevertChange bid version counts _) = case activeDocument d of
  Just doc -> documentLabel doc==Nothing && textBuffer (documentBuffer doc) && (activeWindow d >>= bufferId)==Just bid && revision (documentBuffer doc)==version && bufferLineChanges (documentBuffer doc)==counts
  _ -> False
commandEnabled d ToggleTerminalPin = maybe False (terminalWindow d) (activeWindow d)
commandEnabled d cmd | cmd `elem` [Zoom,SplitVertical,SplitHorizontal], maybe False (windowPinned d) (activeWindow d) = False
commandEnabled d CopyLocation = maybe False (\doc -> documentFile doc/=Nothing && not (byteMode (documentBuffer doc))) (activeDocument d)
commandEnabled d (AgentChoose _) = not (null (conversationSettings d))
commandEnabled d (AgentSet _ _) = not (agentReplying d) && not (null (conversationSettings d))
commandEnabled d cmd | cmd `elem` [GoToMessage,CopyAllMessages,NextMessage,PreviousMessage] = not (null (diagnostics d))
commandEnabled d Copy | problemsVisible d && problemsFocused d = case messageInvocationTarget d of
  Just (MessagesTarget _ _ (Just _)) -> True
  _ -> False
commandEnabled d cmd | problemsVisible d && problemsFocused d, cmd `elem` [Undo,Redo,Cut,Paste,SelectAll] = False
commandEnabled _ _ = True
-- | Actions requiring editable source identity never act on plugin text.
markdownSourceCommand :: Command -> Bool
markdownSourceCommand cmd=sourceOnlyCommand cmd && cmd `notElem` [Save,SaveAs,Download,SplitVertical,SplitHorizontal] || case cmd of
  RevertChange{}->True
  ExecuteShellBlock{}->True
  _->False

sourceOnlyCommand :: Command -> Bool
sourceOnlyCommand cmd=horizontalMutation cmd || cmd `elem` [Save,SaveAs,Download,Undo,Redo,Cut,Paste,Find,FindNext,FindPrevious,Replace,GoTo,
  InspectType,Definition,Complete,RenameSymbol,CodeActions,ToggleHex,SplitVertical,SplitHorizontal]

-- | Shared current-state gate for menu invocations and frontend hints. Queued
-- events must check this again when consumed; a painted enabled state is a hint.
menuCommandAvailable :: Desktop -> Command -> Bool
menuCommandAvailable d cmd = commandEnabled d cmd && canInvoke
  where
    canInvoke | dialogCommandAllowed cmd d = True
              | cmd==Paste = not (maybe False treeFocused (sideTree d)) || dialog d/=Nothing
              | otherwise = dialog d==Nothing && (activeWindow d/=Nothing ||
                  (problemsVisible d && problemsFocused d && cmd==Copy) ||
                  (case cmd of RegisteredMenu{}->True; TreeCommand{}->True; _->False) || cmd `elem` [ReloadBindings,InspectBindings,New,Open,ChangeDir,Quit,Help,About,Gallery,EditorOptions,EnvironmentOptions,ChatInputOptions,ProjectBrowser,RunTarget,RunOptions,CompileTarget,MakeTarget,StopBuild,OpenTerminal,StopTerminal,AgentDirectory,AgentOptions,AgentPermissions,AgentGuidance,Conversation,AgentCancel,AgentResume,AgentNew,AgentCopyRaw,ToggleTree,GitDiff,GitCommit,GitFetch,GitPull,GitMerge,Problems,NextMessage,PreviousMessage,ToolchainOptions,AutocompleteCommand "settings",DebugCommand "attach",DebugCommand "launch",DebugCommand "downloads"])

menuRect :: Desktop -> Int -> Rect
menuRect d i = Rect (min x (max 0 (sw-w))) 1 w (length (menuItemsFor d i)+2)
  where x = fst (menuPositions !! i)
        sw = fst (screenSize d)
        w = min sw (maximum [keyLabelWidth t + keyLabelWidth (menuShortcut d entry) + 5 + (case command of SetBufferView _ -> 4; _ -> 0) | entry@(MenuItem t _ command) <- menuItemsFor d i])

initialDesktop :: (Int,Int) -> Desktop
initialDesktop size = Desktop size [] M.empty M.empty S.empty 1 Nothing Nothing Nothing "" Nothing False Nothing "" Nothing "" Nothing "" False Nothing Nothing Nothing "" Nothing Nothing Nothing [] False 0 0 False Nothing 0 0 Nothing SourceContext Nothing M.empty MountedInput False False 0 True False False False Nothing Nothing [] 8 Nothing [] False SystemMode True [] Nothing [] False Nothing "" M.empty False (0,Nothing) [] Nothing CurrentView QuerySubmit Nothing 0 M.empty Nothing False (newBuffer "") (Selection 0 0) True False M.empty [] [] False Nothing False M.empty 0 (0,Nothing)

activeWindow :: Desktop -> Maybe Window
activeWindow d = listToMaybe (filter (windowVisible d) (windows d))
activeDocument :: Desktop -> Maybe Document
activeDocument d = activeWindow d >>= (\w -> windowDocument (buffers d) w)

-- | Source identity is absent for plugin-owned content.
bufferId :: Window -> Maybe Int
bufferId w=case windowContent w of SourceContent bid->Just bid; PluginContent _->Nothing

-- | Checked lookup shared by source-only owners.
windowDocument :: M.Map Int Document -> Window -> Maybe Document
windowDocument documents w=bufferId w >>= (`M.lookup` documents)

-- | Adopt worker-prepared content without creating an editable document.
addPluginWindow :: PluginWindow.WindowRef -> PluginWindow.PreparedWindow -> Desktop -> Desktop
addPluginWindow reference prepared d=d {windows=w:windows d,pluginWindows=M.insert reference prepared (pluginWindows d),nextId=i+1,
  problemsFocused=False,sideTree=fmap (\tree->tree {treeFocused=False}) (sideTree d)}
  where
    i=nextId d
    (sw,sh)=screenSize d
    column=fromMaybe 0 (listToMaybe [conversationScrollColumn view | view<-M.elems (conversationViews d),conversationBodyRef view==Just reference])
    w=Window i (PluginContent reference) (fitWindow d (Rect 0 1 sw (sh-2))) (Selection 0 0) 0 column Nothing False False
      (nextWindowNumber d) CurrentView Nothing 50 Nothing (initialRowsInteraction prepared) Nothing Nothing Canvas.fitCanvasView

-- | Current Details text for rows, or the original plain/styled view. This reads
-- only the selected NodeId and the prepared ordinal index, never content.
windowPluginText :: Desktop -> Window -> Maybe PluginWindow.PreparedWindow
windowPluginText d w=do
  PluginContent reference<-pure (windowContent w)
  prepared<-M.lookup reference (pluginWindows d)
  pure $ case (PluginWindow.preparedWindowRows prepared,rowsInteraction w) of
    (PluginWindow.RowsDetails rows index _,Just (RowsInteraction ident _))->
      case M.lookup ident index >>= (rows Vec.!?) of Just (PluginWindow.WindowRow _ _ detail)->detail; _->prepared
    _->prepared

activePluginWindow :: Desktop -> Maybe PluginWindow.PreparedWindow
activePluginWindow d=activeWindow d >>= windowPluginText d

initialRowsInteraction :: PluginWindow.PreparedWindow -> Maybe RowsInteraction
initialRowsInteraction prepared=case PluginWindow.preparedWindowRows prepared of
  PluginWindow.RowsDetails rows _ _->case rows Vec.!? 0 of Just (PluginWindow.WindowRow ident _ _)->Just (RowsInteraction ident False); _->Nothing
  _->Nothing

-- | Shared fixed list/Details geometry. Both painting and pointer input use it.
rowsWindowRects :: Desktop -> Window -> (Rect,Rect)
rowsWindowRects d w=(Rect (x+1) (y+1) inner listHeight,Rect (x+1) (y+listHeight+2) inner (max 0 (bodyHeight-listHeight-1)))
  where Rect x y ww hh=bounds w
        inner=max 0 (ww-2)
        bodyHeight=pluginBodyRows d w
        listHeight=min 8 (max 1 (bodyHeight `div` 3))

pluginTextRect :: Desktop -> Window -> Rect
pluginTextRect d w=case rowsInteraction w of
  Just _->snd (rowsWindowRects d w)
  _->let Rect x y ww _=bounds w in Rect (x+1) (y+1) (max 0 (ww-2)) (pluginBodyRows d w)

-- | O(log n). Only a current installed image participates in content input.
windowImage :: Desktop -> Window -> Maybe Canvas.PreparedImage
windowImage d w=do
  PluginContent reference<-pure (windowContent w)
  if reference `S.member` retiredPluginWindows d then Nothing else
    M.lookup reference (pluginWindows d) >>= PluginWindow.preparedWindowImage

-- | The source-image zoom at the current viewport. Fit is resolved using the
-- same cell-to-logical-pixel conversion consumed by rendering and capture.
imageZoom :: Desktop -> Window -> Canvas.PreparedImage -> Double
imageZoom d w image=targetWidth*8/fromIntegral (Canvas.imageWidth image)
  where
    Rect x y width height=pluginTextRect d w
    (_,_,targetWidth,_)=Canvas.canvasImageTarget (modeHeight (fromMaybe 3 (videoMode d))) (x,y,width,height) (imageViewport w) image

imageKey :: V.Key -> [V.Modifier] -> Desktop -> Maybe Desktop
imageKey key mods d=do
  w<-activeWindow d
  image<-windowImage d w
  if not (windowFocused d w) || not (null mods || key==V.KChar '+' && mods==[V.MShift]) then Nothing else do
    let Canvas.CanvasView chosen dx dy=imageViewport w
        pan x y=Canvas.CanvasView chosen (max (-300000) (min 300000 (dx+x))) (max (-300000) (min 300000 (dy+y)))
        zoom factor=Canvas.CanvasView (Just (max (1/64) (min 64 (imageZoom d w image*factor)))) dx dy
    view<-case key of
      V.KChar 'f'->Just Canvas.fitCanvasView
      V.KChar '1'->Just (Canvas.CanvasView (Just 1) 0 0)
      V.KChar '+'->Just (zoom 1.25)
      V.KChar '='->Just (zoom 1.25)
      V.KChar '-'->Just (zoom 0.8)
      V.KLeft->Just (pan 32 0)
      V.KRight->Just (pan (-32) 0)
      V.KUp->Just (pan 0 32)
      V.KDown->Just (pan 0 (-32))
      _->Nothing
    pure (modifyActive (\current->current {imageViewport=view}) d)

-- The same reserved body extent feeds paint, hit maps and scrollbar limits.
pluginBodyRows :: Desktop -> Window -> Int
pluginBodyRows d w=max 0 (height (bounds w)-2-if windowHasEditor d w then height (composerRect d w)+1 else 0)

windowRows :: Desktop -> Window -> Maybe (Vec.Vector PluginWindow.WindowRow,M.Map Tree.NodeId Int,Tree.NodeId,Bool)
windowRows d w=do
  PluginContent reference<-pure (windowContent w)
  prepared<-M.lookup reference (pluginWindows d)
  PluginWindow.RowsDetails rows index _<-pure (PluginWindow.preparedWindowRows prepared)
  RowsInteraction ident details<-rowsInteraction w
  pure (rows,index,ident,details)

rowsListOffset :: Desktop -> Window -> Int -> Int
rowsListOffset d w chosen=max 0 (chosen-height (fst (rowsWindowRects d w))+1)

selectWindowRow :: Int -> Desktop -> Desktop
selectWindowRow requested d=case activeWindow d of
  Just w | Just (rows,_,ident,_)<-windowRows d w,
    Just (PluginWindow.WindowRow next _ _)<-rows Vec.!? max 0 (min (Vec.length rows-1) requested)->
      modifyActive (\v->if next==ident then v {rowsInteraction=Just (RowsInteraction next False)} else
        v {rowsInteraction=Just (RowsInteraction next False),selection=Selection 0 0,scrollRow=0,scrollColumn=0}) d
  _->d

moveWindowRow :: Int -> Desktop -> Desktop
moveWindowRow delta d=case activeWindow d of
  Just w | Just (_,index,ident,_)<-windowRows d w,Just chosen<-M.lookup ident index->selectWindowRow (chosen+delta) d
  _->d

-- | A declaration never overrides the current protected-origin policy. The
-- origin remains host metadata, never a resource ID or frontend field.
privatePreparedWindow :: Desktop -> PluginWindow.PreparedWindow -> Bool
privatePreparedWindow d prepared=PluginWindow.preparedWindowDisclosure prepared/=PluginWindow.ReadableWindow ||
  maybe False (Privacy.protectedFilePath (guestPrivatePaths d)) (PluginWindow.preparedWindowSemantics prepared >>= PluginWindow.textLinkBase)

-- | Shared document authority classification. Titles, buffer reads and screen
-- masks use the same canonical path and host-owned document-role rules.
privateDocument :: Desktop -> Document -> Bool
privateDocument d doc=maybe False privateLabel (documentLabel doc) || maybe False (privatePath . filePath) (documentFile doc) || maybe False privatePath (documentOrigin doc)
  where
    privatePath=Privacy.protectedFilePath (guestPrivatePaths d)
    privateLabel "Git diff"=True
    privateLabel label=maybe False (privatePath . T.unpack) (T.stripPrefix "Disk changes: " label)

applicationTitle :: FilePath -> Desktop -> Text
applicationTitle _ d | Just _<-activePluginWindow d,Just w<-activeWindow d = "th "<>windowTitle d w
applicationTitle cwd d = case activeDocument d of
  Nothing -> "th"
  Just doc | streamerMode d, privateDocument d doc -> "th [private]"
  Just doc -> "th "<>fromMaybe name (documentLabel doc)
    where
      root=fromMaybe (maybe cwd treeRoot (sideTree d)) (defaultDirectory d)
      name=case documentFile doc of
        Just file -> T.pack (relative (filePath file))
        Nothing -> T.pack (fromMaybe ("NONAME"++maybe "" (maybe "" show . bufferId) (activeWindow d)++".HS") (documentSuggestedName doc))
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
    w = Window i (SourceContent i) (fitWindow d (Rect offset (1+offset) (sw-offset) (sh-2-offset))) (Selection 0 0) 0 0 Nothing False False (nextWindowNumber d) (if byteMode b || (defaultBufferView d==MarkdownView && not (markdownDocument (newDocument b file))) then CurrentView else defaultBufferView d) Nothing 50 Nothing Nothing Nothing Nothing Canvas.fitCanvasView

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

-- | Small native window metadata, including open docked tabs. Buffers without
-- views are absent; deriving names never reads content, baselines or histories.
editorWindowEntries :: Desktop -> [(Int,Text,Bool,Bool)]
editorWindowEntries d=[(windowId w,safeTitle (windowTitle d w),selected==Just (windowId w),enabled) | w<-sortOn windowNumber (windows d)]
  where
    enabled=dialog d==Nothing && not (questionActive d)
    selected=if problemsFocused d || maybe False treeFocused (sideTree d) then Nothing else windowId <$> activeWindow d
    safeTitle=T.take 8192 . T.map (\c->if c<' ' || c=='\DEL' then '·' else c)

-- | Host title projection never reads content or histories.
windowTitle :: Desktop -> Window -> Text
windowTitle d w | Just target<-conversationTargetFor d w = if target==conversationTarget d then conversationTitle d else maybe "Conversation" conversationName (M.lookup target (conversationViews d))
windowTitle d w=case windowContent w of
  PluginContent reference | streamerMode d->"Private plugin window"
                          | otherwise->(if reference `S.member` retiredPluginWindows d then "Unavailable: " else "")<>maybe "Unavailable plugin window" PluginWindow.preparedWindowTitle (M.lookup reference (pluginWindows d))
  SourceContent bid->case M.lookup bid (buffers d) of
    Nothing->"Unavailable source"
    Just doc | streamerMode d,privateDocument d doc->"Private buffer"
             | otherwise->fromMaybe (maybe (maybe ("NONAME"<>T.pack (show bid)<>".HS") T.pack (documentSuggestedName doc))
                 (T.pack . takeFileName . filePath) (documentFile doc)) (documentLabel doc)

-- | Dock targets are human view selection, not an escape from a modal control.
editorWindowAvailable :: Desktop -> Int -> Bool
editorWindowAvailable d ident=dialog d==Nothing && not (questionActive d) && any ((==ident).windowId) (windows d)

activateEditorWindow :: Int -> Desktop -> Desktop
activateEditorWindow ident d
  | editorWindowAvailable d ident = focusWindow ident d {menu=Nothing,contextMenu=Nothing,drag=Nothing,dragOriginal=Nothing,prefix=Nothing}
  | otherwise = d

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
ensureVisible d | activeMarkdown d,Just w<-activeWindow d = markdownMoveTo True (caret (selection (displayWindow w))) d
ensureVisible d = case (activeWindow d, activeDocument d) of
  (Just w, Just doc) -> modifyActive (const (rememberSourceWidth d doc w { scrollRow = max 0 row', scrollColumn = max 0 col' })) d
    where
      b = documentBuffer doc
      (row,dc) = windowCaretCell d doc w
      rows = max 1 (windowContentRows d doc w); cols = max 1 (if bufferView w==SideBySideView then snd (reviewPaneWidths w) else width (bounds w)-2)
      paneOffset=if bufferView w==SideBySideView then fst (reviewPaneWidths w)+1 else 0
      textColumn=dc-paneOffset
      row' = if row < scrollRow w then row else if row >= scrollRow w+rows then row-rows+1 else scrollRow w
      col' = if textColumn < scrollColumn w then textColumn else if textColumn >= scrollColumn w+cols then textColumn-cols+1 else scrollColumn w
  _ -> d

-- A layout change follows only caret axes that were visible beforehand.
-- Manual scrollbar browsing must survive Messages appearing or being resized.
ensureVisibleAfterLayout :: Desktop -> Desktop -> Desktop
ensureVisibleAfterLayout _ after | activeMarkdown after=after {windows=map (clampMarkdownInteraction after) (windows after)}
ensureVisibleAfterLayout before after = case (activeWindow after,activeDocument after) of
  (Just current,Just doc) | Just previous<-find ((==windowId current).windowId) (windows before) ->
    let (row,column)=windowCaretCell before doc previous
        textColumn=column-if bufferView previous==SideBySideView then fst (reviewPaneWidths previous)+1 else 0
        columns=max 1 (if bufferView previous==SideBySideView then snd (reviewPaneWidths previous) else width (bounds previous)-2)
        rowVisible=row>=scrollRow previous && row<scrollRow previous+windowContentRows before doc previous
        columnVisible=textColumn>=scrollColumn previous && textColumn<scrollColumn previous+columns
    in modifyActive (\shown -> shown
      {scrollRow=if rowVisible then scrollRow shown else min (scrollbarLimit after True doc current) (scrollRow current),
       scrollColumn=if columnVisible then scrollColumn shown else min (scrollbarLimit after False doc current {sourceWidthHint=Nothing}) (scrollColumn current)}) (ensureVisible after)
  _ -> after

-- Map other view positions through the changed character interval.
editActive :: (Selection -> Buffer -> Buffer) -> Maybe Int -> Desktop -> Desktop
editActive _ _ d | activeMarkdown d=d {status="Markdown view is read-only. Switch to Current to edit."}
editActive _ _ d | maybe False treeFocused (sideTree d) || problemsFocused d = d
editActive f cursor d = case (activeWindow d, activeDocument d) of
  (Just _, Just doc) | documentLabel doc /= Nothing -> d {status="This window is read-only."}
  (Just active, Just doc)
    | Just _<-bufferId active, revision changed==revision original -> maybe (modifyActive (\w -> w {reviewSelection=Nothing}) d) (\p -> moveTo False p d) cursor
    | Just bid<-bufferId active -> clampReviewWindows (ensureVisible d { buffers = M.insert bid (restyle doc {documentBuffer = changed}) (buffers d), windows = map adjust (windows d) })
    where
      original = documentBuffer doc
      changed = f (selection active) original
      (common,oldEnd,inserted) = fromMaybe (0,0,0) (lastChange changed)
      newEnd = common+inserted
      rebase p | byteMode original/=byteMode changed = min (bufferLength changed) (modeOffset original p)
               | p <= common = p
               | p >= oldEnd = p + newEnd-oldEnd
               | otherwise = newEnd
      adjust w | bufferId w/=bufferId active = w
               | windowId w == windowId active = w {selection = Selection target target,windowHexLow=False,reviewSelection=Nothing,bufferView=if byteMode changed then CurrentView else bufferView w}
               | otherwise = w {selection = let Selection a c = selection w in Selection (rebase a) (rebase c),reviewSelection=Nothing,bufferView=if byteMode changed then CurrentView else bufferView w}
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
    raw = max 0 (min len pos)
    p = case (activeWindow d,activeDocument d) of
      (Just w,Just doc) -> visibleReviewPosition (documentBuffer doc) w raw
      _ -> raw
    update w = w {windowHexLow=False,reviewSelection=Nothing,selection = Selection (if extend then anchor (selection w) else p) p}

-- Review selections use display offsets; editable selections always use live text.
windowReviewSelection :: Buffer -> Window -> Maybe ReviewSelection
windowReviewSelection b w = case reviewSelection w of
  Just selected | windowChangeView b w,
    reviewRevision selected==revision b, reviewCounts selected==bufferLineChanges b,
    let (a,z)=ordered (reviewRange selected), a>=0, z<=changeLength b -> Just selected
  _ -> Nothing

windowReviewRange :: Buffer -> Window -> Maybe Selection
windowReviewRange b w=reviewRange <$> windowReviewSelection b w

windowChangeView :: Buffer -> Window -> Bool
windowChangeView b w=bufferView w `elem` [ChangesView,OnlyChangesView,SideBySideView] && not (byteMode b)

reviewVisibleRanges :: Buffer -> Window -> ReviewSelection -> [(Int,Int)]
reviewVisibleRanges b w selected=
  [(max a start,min z end) | (first,last')<-viewChangeRanges (bufferView w) (bufferViewProjection b) (reviewSide selected),
    let start=changeLineOffset b first, let end=changeLineOffset b last',max a start<min z end]
  where (a,z)=ordered (reviewRange selected)

windowSelectedText :: Buffer -> Window -> Text
windowSelectedText b w=case windowReviewSelection b w of
  Nothing -> selectedText (selection w) b
  Just selected | reviewSide selected==UnifiedSide -> T.concat [changeSlice b a (z-a) | (a,z)<-reviewVisibleRanges b w selected]
  Just selected -> T.concat [rawSlice a z | (a,z)<-reviewVisibleRanges b w selected]
  where
    rawSlice a z=T.concat [T.take (min (T.length raw) (z-start)-max 0 (a-start)) (T.drop (max 0 (a-start)) raw)
      | (row,(_,_,raw))<-zip [first..] (bufferChangeRows b first (last'-first+1)),let start=changeLineOffset b row,start<z]
      where first=fst (changeLineColumn b a); last'=fst (changeLineColumn b z)

cutReviewSelection :: Desktop -> Desktop
cutReviewSelection d=case (activeWindow d,activeDocument d) of
  (Just w,Just doc) | Just selected<-windowReviewSelection b w ->
    if reviewSide selected==OriginalSide then copied else case intervals of
      [] -> copied
      (start,_):_ -> let { end=snd (last intervals); replacement=preserve start intervals end }
                    in editActive (\_ -> replaceSelection (Selection start end) replacement) (Just start) copied
    where
      b=documentBuffer doc
      copied=(copyClipboard (syntaxDocument doc && not (byteMode b)) (windowSelectedText b w) d) {status="Block copied."}
      intervals=[(a,z) | selected'<-maybe [] pure (windowReviewSelection b w), (first,last')<-reviewVisibleRanges b w selected',
        let a=changeToLiveOffset b first,let z=changeToLiveOffset b last',a<z]
      preserve pos [] end=bufferSlice b pos (end-pos)
      preserve pos ((a,z):rest) end=bufferSlice b pos (a-pos)<>preserve z rest end
  _ -> insertText "" (copyClipboard (maybe False syntaxDocument (activeDocument d)) (maybe "" (\doc -> maybe "" (windowSelectedText (documentBuffer doc)) (activeWindow d)) (activeDocument d)) d)

setBufferView :: BufferView -> Desktop -> Desktop
setBufferView mode d
  | mode==MarkdownView && commandEnabled d (SetBufferView mode)=modifyActive (\w->w {bufferView=mode,reviewSelection=Nothing,markdownInteraction=Just (fromMaybe (MarkdownInteraction (Selection 0 0) 0 0 Nothing) (markdownInteraction w))}) d
  | activeMarkdown d && mode==CurrentView=modifyActive (\w->w {bufferView=CurrentView,reviewSelection=Nothing}) d
  | activeMarkdown d=setBufferView mode (setBufferView CurrentView d)
  | not (commandEnabled d (SetBufferView mode))=d
  | otherwise=case (activeWindow d,activeDocument d) of
      (Just w,Just doc) -> ensureVisible (modifyActive update d)
        where
          update v=let changed=v {bufferView=mode,reviewSelection=Nothing,scrollRow=newRow}
                       target=visibleReviewPosition b changed (caret (selection v))
                   in if target==caret (selection v) then changed else changed {selection=Selection target target}
          b=documentBuffer doc
          projection=bufferViewProjection b
          currentTop=fst (changeLineColumn b (liveToChangeOffset b (bufferLineOffset b (scrollRow w))))
          oldFull | bufferView w==CurrentView=maybe currentTop fst (changeHunkAt b currentTop)
                  | otherwise=fromMaybe (fst (changeLineColumn b (liveToChangeOffset b (caret (selection w))))) (viewRightRow (viewRowAt (bufferView w) projection (scrollRow w)))
          newRow | mode==CurrentView=fst (bufferLineColumn b (changeToLiveOffset b (changeLineOffset b oldFull)))
                 | otherwise=viewRowForChange mode projection CurrentSide oldFull
      _ -> d

-- | Retire incompatible per-window views when a source path/content is adopted.
-- Source interaction and Undo stay authoritative; only presentation metadata is
-- discarded. Byte replacement also normalizes review modes, as direct edits do.
normalizeDocumentViews :: Int -> Desktop -> Desktop
normalizeDocumentViews bid d=case M.lookup bid (buffers d) of
  Nothing->d
  Just doc->let incompatible w=bufferId w==Just bid &&
                 (byteMode (documentBuffer doc) || (bufferView w==MarkdownView && not (markdownDocument doc)))
                retired=[windowId w | w<-windows d,incompatible w]
                normalize w | incompatible w=w {bufferView=CurrentView,reviewSelection=Nothing,markdownInteraction=Nothing}
                            | otherwise=w
            in d {windows=map normalize (windows d),windowPresentations=M.filterWithKey (\ident _->ident `notElem` retired) (windowPresentations d)}

clampReviewWindows :: Desktop -> Desktop
clampReviewWindows d=d {windows=map clamp (windows d)}
  where
    clamp w | Just doc<-windowDocument (buffers d) w,windowChangeView (documentBuffer doc) w =
      w {reviewSelection=windowReviewSelection (documentBuffer doc) w,
         scrollRow=max 0 (min (scrollbarLimit d True doc w) (scrollRow w))}
    clamp w=w

-- Arrow navigation crosses hidden unchanged ranges without placing a caret on a gap.
visibleReviewPosition :: Buffer -> Window -> Int -> Int
visibleReviewPosition b w pos
  | bufferView w/=OnlyChangesView || byteMode b=pos
  | viewOmittedRows entry==0=pos
  | otherwise=fromMaybe pos (candidate direction `orElse` candidate (negate direction))
  where
    projection=bufferViewProjection b
    full=fst (changeLineColumn b (liveToChangeOffset b pos))
    row=viewRowForChange OnlyChangesView projection CurrentSide full
    entry=viewRowAt OnlyChangesView projection row
    direction=if pos<caret (selection w) then -1 else 1
    candidate delta=do
      source<-viewRightRow (viewRowAt OnlyChangesView projection (row+delta))
      let offset=changeLineOffset b source+(if delta<0 then T.length (changeLineAt b source) else 0)
      pure (changeToLiveOffset b offset)
    orElse (Just value) _=Just value
    orElse Nothing other=other

-- A separator or padded alignment cell carries no selectable text.
reviewHit :: Int -> Int -> Buffer -> Window -> Maybe (ReviewSide,Int,Int)
reviewHit x y b w=do
  let row=max 0 (min (viewRowCount (bufferView w) projection-1) (y-top (bounds w)-1+scrollRow w))
      (leftWidth,_)=reviewPaneWidths w
      local=x-left (bounds w)-1
      side=if bufferView w/=SideBySideView then UnifiedSide else if local<leftWidth then OriginalSide else CurrentSide
      col=max 0 (local-(if bufferView w==SideBySideView && side==CurrentSide then leftWidth+1 else 0)+scrollColumn w)
      entry=viewRowAt (bufferView w) projection row
  fullRow<-if side==OriginalSide then viewLeftRow entry else viewRightRow entry
  pure (side,fullRow,changeLineOffset b fullRow+columnOffset (changeLineAt b fullRow) col)
  where projection=bufferViewProjection b

-- Docs: tools/docs-screenshots.hs documentation-links -> docs/site/screenshots/documentation-links.png (docs/editing.md).
linkAt :: Int -> Int -> Desktop -> Maybe Command
linkAt x y d | activeMarkdown d=do
  original<-activeWindow d
  (_,text,links)<-windowMarkdown d original
  let w=displayWindow original; Rect l t ww hh=bounds w; row=y-t-1+scrollRow w; col=x-l-1+scrollColumn w
      offset=windowTextOffset d w text row col
  if x<=l || x>=l+ww-1 || y<=t || y>=t+hh-1 || row>=windowTextRows d w text || not (windowTextContainsColumn d w text row col) then Nothing else do
    (_,_,url)<-find (\(a,z,_)->offset>=a && offset<z) links
    doc<-activeDocument d
    pure (OpenLink (SourceLink (filePath <$> documentFile doc)) url)
linkAt x y d | Just w<-activeWindow d,PluginContent reference<-windowContent w=do
  prepared<-windowPluginText d w
  semantics<-PluginWindow.preparedWindowSemantics prepared
  let rect=pluginTextRect d w
      row=y-top rect+scrollRow w
      col=x-left rect+scrollColumn w
      position=windowTextOffset d w (PluginWindow.preparedWindowText prepared) row col
  if not (inside rect x y) || reference `S.member` retiredPluginWindows d ||
      PluginWindow.preparedWindowImage prepared/=Nothing && (videoMode d/=Nothing || browserFrontend d) then Nothing else do
    (_,_,url)<-Vec.find (\(start,end,_)->position>=start && position<end) (PluginWindow.textLinks semantics)
    pure (OpenLink (WindowLink reference prepared (PluginWindow.textLinkBase semantics)) url)
linkAt x y d=do
  w<-activeWindow d
  doc<-activeDocument d
  if null (documentLinks doc) then Nothing else pure ()
  let Rect l t ww _=bounds w
      row=y-t-1+scrollRow w
      col=x-l-1+scrollColumn w
      b=documentBuffer doc
      position=windowTextOffset d w (bufferContent b) row col
  if x<=l || x>=l+ww-1 || y<=t || y>=t+1+windowContentRows d doc w || row>=windowTextRows d w (bufferContent b) || not (windowTextContainsColumn d w (bufferContent b) row col)
    then Nothing else do
      (_,_,url)<-find (\(start,end,_)->position>=start && position<end) (documentLinks doc)
      pure (OpenLink (SourceLink (documentMarkdownPath doc)) url)

-- A link action carries a passive immutable origin plus exact publication
-- identity; the existing link worker resolves its base and validates on adoption.
linkOriginPath :: LinkOrigin -> Maybe FilePath
linkOriginPath (SourceLink path)=path
linkOriginPath (WindowLink _ _ path)=path

linkOriginCurrent :: Desktop -> LinkOrigin -> Bool
linkOriginCurrent _ SourceLink{}=True
linkOriginCurrent d (WindowLink reference prepared _)=reference `S.notMember` retiredPluginWindows d &&
  M.lookup reference (pluginWindows d)==Just prepared && any ((==PluginContent reference).windowContent) (windows d)

-- Docs: tools/docs-screenshots.hs shell-block-menu -> docs/site/screenshots/shell-block-menu.png (docs/conversations.md).
shellBlockAt :: Int -> Int -> Desktop -> Maybe Command
shellBlockAt _ _ d | activeMarkdown d=Nothing
shellBlockAt x y d | Just w<-activeWindow d,PluginContent reference<-windowContent w=do
  prepared<-windowPluginText d w
  semantics<-PluginWindow.preparedWindowSemantics prepared
  let rect=pluginTextRect d w
      row=y-top rect+scrollRow w
      col=x-left rect+scrollColumn w
      position=windowTextOffset d w (PluginWindow.preparedWindowText prepared) row col
  if not (inside rect x y) || reference `S.member` retiredPluginWindows d then Nothing else do
    block<-find (\(start,end,_,_)->position>=start && position<end) (Vec.toList (PluginWindow.textShellBlocks semantics))
    pure (ExecuteShellBlock (WindowShell reference prepared) block)
shellBlockAt x y d=do
  w<-activeWindow d
  doc<-activeDocument d
  let b=documentBuffer doc
      row=y-top (bounds w)-1+scrollRow w
      col=x-left (bounds w)-1+scrollColumn w
      position=windowTextOffset d w (bufferContent b) row col
  if byteMode b || windowChangeView b w || row>=windowTextRows d w (bufferContent b) then Nothing else do
    block<-find (\(start,end,_,raw)->position>=start && position<end && not (T.null (T.strip raw))) (documentShellBlocks doc)
    bid<-bufferId w
    pure (ExecuteShellBlock (SourceShell bid) block)

-- Window identity proves all immutable block strings; no body-text comparison
-- belongs to owner admission. Source documents retain their established check.
shellBlockCurrent :: Desktop -> ShellOrigin -> (Int,Int,Text,Text) -> Bool
shellBlockCurrent d (SourceShell bid) block=maybe False (elem block.documentShellBlocks) (M.lookup bid (buffers d))
shellBlockCurrent d (WindowShell reference prepared) (start,end,_,_)=
  reference `S.notMember` retiredPluginWindows d && M.lookup reference (pluginWindows d)==Just prepared &&
  any ((==PluginContent reference).windowContent) (windows d) &&
  maybe False (Vec.any (\(a,z,_,_)->a==start && z==end).PluginWindow.textShellBlocks) (PluginWindow.preparedWindowSemantics prepared)

reviewContext :: Int -> Int -> Desktop -> ContextKind
reviewContext x y d=case (activeWindow d,activeDocument d) of
  (Just w,Just doc) | windowChangeView b w,
    let entry=viewRowAt (bufferView w) (bufferViewProjection b) (y-top (bounds w)-1+scrollRow w),
    Just row<-case reviewHit x y b w of Just (_,r,_)->Just r; Nothing->case viewLeftRow entry of Just r->Just r; Nothing->viewRightRow entry,
    Just _<-changeHunkAt b row, Just bid<-bufferId w -> ChangeContext (RevertChange bid (revision b) (bufferLineChanges b) row)
    where b=documentBuffer doc
  _ -> SourceContext

wrapMessage :: T.Text -> [T.Text]
wrapMessage text | T.null text=[]
wrapMessage text=T.take 54 text:wrapMessage (T.drop 54 text)

message :: Text -> [Text] -> Desktop -> Desktop
message title lines' d = d {dialog = Just (Dialog title Information [] 0 ["OK"] lines'), menu = Nothing, drag = Nothing,dragOriginal=Nothing}

prompt :: Text -> Purpose -> [Field] -> Desktop -> Desktop
prompt title p fs d = d {dialog = Just (Dialog title p fs 0 ["OK","Cancel"] []), menu = Nothing, drag = Nothing,dragOriginal=Nothing}

-- | Apply a semantic editor command and return any required host effects.
runCommand :: Command -> Desktop -> (Desktop,[Effect])
runCommand cmd source | dialog source==Nothing,menu source==Nothing,contextMenu source==Nothing,not (questionActive source),
  Just key<-lookup cmd [(CursorLeft False,V.KLeft),(CursorRight False,V.KRight),(CursorUp False,V.KUp),(CursorDown False,V.KDown)],
  Just next<-imageKey key [] source = (next,[])
runCommand cmd source | cmd `elem` [WordStarBlockPrefix,WordStarQuickPrefix], not (commandEnabled source cmd) = (source,[])
runCommand cmd source | sourceKeyCommand cmd, not (commandEnabled source cmd) =
  (if horizontalMutation cmd && sourceNavigationOwner source && not (activeMarkdown source) &&
      maybe False ((/=Nothing) . documentLabel) (activeDocument source)
    then source {status="This window is read-only."} else source,[])
runCommand cmd source | dialog source==Nothing, activeMarkdown source, markdownSourceCommand cmd = (source {status="Markdown view is read-only. Switch to Current to edit."},[])
runCommand cmd source | dialog source==Nothing, Just _<-activePluginWindow source, sourceOnlyCommand cmd,not ((composerActive source || questionActive source) && cmd `elem` [Undo,Redo,Cut,Paste]) = (source {status="This plugin window is read-only."},[])
runCommand cmd source | browserFrontend source, cmd `elem` [Copy,Cut,CopyAllMessages,CopyLocation] =
  let (next,requests)=runCommand cmd source {browserFrontend=False}
  in (next {browserFrontend=True},requests++[WriteBrowserClipboard (clipboard next) | not (any deferredCopy requests)])
  where deferredCopy CopyConversation{}=True; deferredCopy _=False
runCommand Paste source | browserFrontend source = (source {menu=Nothing,contextMenu=Nothing,prefix=Nothing},[ReadBrowserClipboard])
runCommand cmd source | dialogCommandAllowed cmd source = applyDialogCommand cmd source
runCommand cmd source | problemsVisible source && problemsFocused source, cmd `elem` [Undo,Redo,Cut,Paste,SelectAll] = (source {menu=Nothing,contextMenu=Nothing},[])
runCommand Copy source | dialog source==Nothing,activeConversation source,
  not (questionActive source || activeAutocomplete source && autocompleteFocused source),Just window<-activeWindow source,
  Just target<-conversationTargetFor source window,Just view<-M.lookup target (conversationViews source),
  Just logical<-conversationLogical view,Just chosen@(BodySelection a z)<-conversationCopySelection source target window,a/=z,
  Just reference<-conversationBodyRef view =
    let serial=fst (clipboardExport source)+1
    in (source {clipboardExport=(serial,Nothing),status="Preparing conversation copy.",menu=Nothing,contextMenu=Nothing},
      [CopyConversation (ConversationCopy reference target logical chosen serial)])
runCommand cmd source | activeAutocomplete source, autocompleteFocused source, cmd `elem` [Undo,Redo,Copy,Cut,Paste,SelectAll] = (autocompleteEdit (composerCommandWith False cmd) source,[])
runCommand cmd source | questionActive source, cmd `elem` [Undo,Redo,Copy,Cut,Paste,SelectAll] = (questionCommand cmd source,[])
runCommand cmd source | not (problemsFocused source), composerActive source, cmd `elem` [Undo,Redo,Copy,Cut,Paste,SelectAll] = (composerCommand cmd source,[])
runCommand Copy source | dialog source==Nothing,activeMarkdown source,maybe False (windowFocused source) (activeWindow source) =
  let text=case activeWindow source >>= windowMarkdown source of
        Just (_,content,_)->let (a,z)=ordered (selection (displayWindow (fromMaybe (error "Missing Markdown window") (activeWindow source)))) in contentSlice content a (z-a)
        Nothing->""
  in (copyClipboard False text source {menu=Nothing,contextMenu=Nothing},[])
runCommand SelectAll source | dialog source==Nothing,activeMarkdown source,maybe False (windowFocused source) (activeWindow source) =
  (modifyActive (modifyDisplayedWindow (\w->w {selection=Selection 0 (maybe 0 (\(_,text,_)->contentLength text) (windowMarkdown source w))})) source,[])
runCommand Copy source | dialog source==Nothing,Just view<-activePluginWindow source, Just w<-activeWindow source =
  let (a,b)=ordered (selection w)
  in (copyClipboard False (PluginWindow.copyPreparedSelection view a b) source {menu=Nothing,contextMenu=Nothing},[])
runCommand SelectAll source | dialog source==Nothing,Just view<-activePluginWindow source =
  (modifyActive (\w->w {selection=Selection 0 (contentLength (PluginWindow.preparedWindowText view))}) source,[])
runCommand cmd source = Bifunctor.first (clampHexScroll source) $ go cmd (source {menu = Nothing, contextMenu=Nothing, buttonHover=Nothing, buttonPressed=Nothing, prefix = Nothing, drag = Nothing,dragOriginal=Nothing})
  where
    go DialogAccept d = applyDialogCommand DialogAccept d
    go DialogCancel d = applyDialogCommand DialogCancel d
    go DialogFocusNext d = applyDialogCommand DialogFocusNext d
    go DialogFocusPrevious d = applyDialogCommand DialogFocusPrevious d
    go WordStarBlockPrefix d = (d {prefix=Just 'k'},[])
    go WordStarQuickPrefix d = (d {prefix=Just 'q'},[])
    go MarkBlockStart d = (d {blockStart=(\w -> (,caret (selection w)) <$> bufferId w) =<< activeWindow d},[])
    go MarkBlockEnd d = (case (blockStart d,activeWindow d) of
      (Just (bid,p),Just w) | Just bid==bufferId w -> modifyActive (\v -> v {selection=Selection (min p (maybe 0 (bufferLength . documentBuffer) (activeDocument d))) (caret (selection v))}) d
      _ -> d,[])
    go DeleteSelection d = (insertText "" d,[])
    go ReloadBindings d = (d {status="Reloading keybindings..."},[ReloadKeyBindings (startingDirectory d)])
    go InspectBindings d = (d,[InspectKeyBindings ((,) (bindingPlatform d) <$> bindingContext d) (effectiveBindings d)])
    go (SidebarMove delta) d = case sideTree d of
      Just tree | treeFocused tree -> moveTree delta tree d
      _ -> (d,[])
    go SidebarActivate d = maybe (d,[]) (\tree->activateTree False (treeSelected tree) d) (sideTree d)
    go SidebarExpand d = maybe (d,[]) (\tree->activateTree True (treeSelected tree) d) (sideTree d)
    go SidebarCollapse d = maybe (d,[]) (\tree->collapseTree tree d) (sideTree d)
    go (MessagesPage delta) d = (chooseProblem (problemsSelected d+delta*max 1 (problemsHeight d-2)) d,[])
    go (MessagesMove delta) d = (chooseProblem (problemsSelected d+delta) d,[])
    go FocusSource d = (d {problemsFocused=False,sideTree=fmap (\tree->tree {treeFocused=False}) (sideTree d)},[])
    go (SubmitChat action) d = submitEditorSlot (if action==QuerySubmit then Editor.DefaultEditor else Editor.AlternateEditor) (setComposerInput (composerBuffer d) (composerSelection d) True d)
    go CopyLocation d = case (activeWindow d,activeDocument d) of
      (Just w,Just doc) | Just file<-documentFile doc ->
        let (r,col)=bufferLineColumn (documentBuffer doc) (caret (selection w))
            location=T.pack (filePath file)<>":"<>T.pack (show (r+1))<>":"<>T.pack (show (col+1))
        in ((copyClipboard False location d) {status="Location copied."},[])
      _ -> (d,[])
    go Download d = (d,[DownloadDocument bid | commandEnabled d Download, Just w<-[activeWindow d], Just bid<-[bufferId w]])
    go New d = (addDocument Nothing (newBuffer "") d,[])
    go Open d = (d,[BrowsePath (startingDirectory d) "*.hs"])
    go ChangeDir d = (d,[BrowseDirectories (startingDirectory d)])
    go Save d = saveRequest Nothing d
    go ReviewDisk d = (d,[ReviewExternal])
    go (DebugCommand "breakpoint") d | menusActive source = case find ((=="hide.debug.toggle-breakpoint") . Plugin.menuName . Plugin.menuReference) (contributedMenus source) of
      Just item->go (contributionCommand source item) d
      Nothing->(d {status="Breakpoint command is unavailable."},[])
    go (DebugCommand action) d = (d,[DebugAction action []])
    go CompileTarget d = (d,[ServiceAction "compile" []])
    go MakeTarget d = (d,[ServiceAction "make" []])
    go StopBuild d = (d,[ServiceAction "build-stop" []])
    go RunTarget d = (d,[ServiceAction "run" []])
    go ToolchainOptions d | dialog d/=Nothing = (d,[])
    go ToolchainOptions d =
      let Rect x y _ _=toolchainBadgeRect d
          opened=openContext (ToolchainContext [("THC",SelectToolchain THC),("GHC Automatic",SelectCompiler "ghc"),("Target settings...",RunOptions)]) x y d
      in (opened {contextMenu=fmap (\(r,_) -> (r,if toolchain d==Just GHC then 1 else 0)) (contextMenu opened)},[ServiceAction "toolchain" []])
    go (SelectToolchain choice) d = (d,[ServiceAction "toolchain" [T.pack (show choice)]])
    go (SelectCompiler command) d = (d,[ServiceAction "toolchain" ["GHC",command]])
    go RunOptions d = (d,[ServiceAction "run-options" []])
    go OpenTerminal d = (d,[ServiceAction "terminal" []])
    go StopTerminal d = (d,[ServiceAction "terminal-stop" []])
    go AgentDirectory d = (d,[AgentAction "directory" []])
    go AgentOptions d = (d,[AgentAction "options" []])
    go AgentPermissions d = (d,[PermissionAction "show" []])
    go EnvironmentOptions d = (d,[EnvironmentAction "show" []])
    go AgentGuidance d = (d,[AgentAction "context" []])
    go Conversation d = case find (\w->conversationTargetFor d w==Just (conversationTarget d)) (windows d) of
      Just w -> let focused=focusWindow (windowId w) d in (setComposerInput (composerBuffer focused) (composerSelection focused) True focused,[AgentAction "focus" []])
      Nothing -> (d,[AgentAction "show" []])
    go AgentCancel d = (d,[AgentAction "cancel" []])
    go AgentResume d = (d,[AgentAction "resume" []])
    go AgentNew d = (d,[AgentAction "new" []])
    go (AgentChoose category) d = (openAgentChoices category d,[])
    go (AgentSet ident value) d = (d,[AgentAction "set-config" [ident,value]])
    go AgentCopyRaw d = (d,[AgentAction "copy" []])
    go SaveAs d = case activeWindow d of
      Nothing -> (d,[])
      Just _ | maybe False ((/=Nothing) . documentLabel) (activeDocument d) -> (d {status="This window is read-only."},[])
      Just w | Just bid<-bufferId w -> (prompt "Save file as" (Saving bid Nothing) [Input "Name" (currentPath d) (T.length (currentPath d))] d,[])
      _ -> (d,[])
    go Quit d = case find (dirty . documentBuffer . snd) (M.toList (buffers d)) of
      Nothing | conversationHasDraft d -> (d {dialog=Just (Dialog "Unsent query" DiscardDraft [] 0 ["Discard","Cancel"] ["Discard the unsent conversation query?"])},[])
              | otherwise -> (d,[Exit])
      Just (bid,_) -> let focused = maybe d (\w -> focusWindow (windowId w) d) (find ((==Just bid) . bufferId) (windows d))
                     in confirm Quit focused
    go Close d = case (activeWindow d, activeDocument d) of
      (Just w,Nothing) | PluginContent reference<-windowContent w ->(closeActive d,map RetirePluginWindow (nub (reference:closingConversationBodies d w))++closingEditors d w)
      (Just w, Just doc) | dirty (documentBuffer doc) && length (filter ((==bufferId w) . bufferId) (windows d)) == 1 -> confirm Close d
      (Just w,_) -> (closeActive d,closingEditors d w)
      _ -> (d,[])
    go command@(ExecuteShellBlock origin block) d
      | commandEnabled d command = (d,[ExecuteShellBlockAction origin block])
      | otherwise = (d {status="The shell code block is no longer current."},[])
    go (SetBufferView mode) d = (setBufferView mode d,[])
    go (SetDefaultBufferView mode) d = (d {defaultBufferView=mode,status="Default buffer view updated."},[SaveBufferViewDefault mode])
    go command@(RevertChange _ _ _ row) d
      | commandEnabled d command = (editActive (\_ -> revertChangeHunk row) Nothing d,[])
      | otherwise = (d {status="The change is no longer current."},[])
    go ToggleHex d = (toggleHex d,[])
    go (CursorLeft extend) d = (horizontalMove False extend d,[])
    go (CursorRight extend) d = (horizontalMove True extend d,[])
    go (CursorUp extend) d = (verticalMove (-1) extend d,[])
    go (CursorDown extend) d = (verticalMove 1 extend d,[])
    go (CursorRowStart extend) d = (rowEdge False extend d,[])
    go (CursorRowEnd extend) d = (rowEdge True extend d,[])
    go (CursorDocumentStart extend) d = (documentEdge False extend d,[])
    go (CursorDocumentEnd extend) d = (documentEdge True extend d,[])
    go (CursorPageUp extend) d = (pageMove False extend d,[])
    go (CursorPageDown extend) d = (pageMove True extend d,[])
    go (CursorWordLeft extend) d = (wordMove False extend d,[])
    go (CursorWordRight extend) d = (wordMove True extend d,[])
    go DeleteWordBackward d = (deleteWord False d,[])
    go DeleteWordForward d = (deleteWord True d,[])
    go DeleteBackward d = (deleteAdjacent False d,[])
    go DeleteForward d = (deleteAdjacent True d,[])
    go DeleteLine d = (deleteSourceLine d,[])
    go Undo d = (editActive (const undo) Nothing d,[])
    go Redo d = (editActive (const redo) Nothing d,[])
    go GoToMessage d | menusActive d = case find ((=="hide.messages.go-to") . Plugin.menuName . Plugin.menuReference) (contributedMenus d) of
      Just item -> go (contributionCommand d item) d
      Nothing -> (d {status="Source navigation is unavailable."},[])
    go GoToMessage d = jumpProblem d
    go CopyAllMessages d = copyMessages (diagnostics d) d
    go Copy d | problemsVisible d && problemsFocused d = copyMessages (take 1 (drop (problemsSelected d) (diagnostics d))) d
    go Copy d | activeHex d = ((copyClipboard False (T.unwords (map (hexNumber 2 . ord) (T.unpack (selected d)))) d) {status="Hex bytes copied."},[])
    go Copy d = ((copyClipboard (maybe False syntaxDocument (activeDocument d)) (selected d) d) {status="Block copied."},[])
    go Cut d | activeHex d = let copied=fst (go Copy d) in (insertText "" copied,[])
    go Cut d = (cutReviewSelection d,[])
    go Paste d | Just ident<-activeTerminal d = (d,[ServiceAction "terminal-input" [ident,clipboard d]])
    go Paste d | activeHex d = (pasteHex (clipboard d) d,[])
    go Paste d = (insertText (clipboard d) d,[])
    go SelectAll d = (modifyActive (\w -> case activeDocument d of
      Just doc -> let { b=documentBuffer doc; side=maybe (if bufferView w==SideBySideView then CurrentSide else UnifiedSide) reviewSide (windowReviewSelection b w) }
                  in w {selection=if side==OriginalSide then selection w else Selection 0 (bufferLength b),
                        reviewSelection=if windowChangeView b w then Just (ReviewSelection (revision b) (bufferLineChanges b) side (Selection 0 (changeLength b))) else Nothing}
      Nothing -> w) d,[])
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
    go PreviousWindow d = (cycleEditorWindow True d,[])
    go Cascade d = (replaceFloating (zipWith cascade [0..] (floatingWindows d)) d,[]) where
      (sw,sh) = screenSize d
      cascade i w = w {bounds = fitWindow d (Rect (treeWidthOf d+i `mod` 6) (1+i `mod` 6) (sw-treeWidthOf d-6) (sh-8)), restoredBounds = Nothing}
    go Tile d = (tileWindows False d,[])
    -- Docs: docs/site/screenshots/split.png (docs/editing.md) shows shared split views.
    go SplitVertical d = splitWindow True d
    go SplitHorizontal d = splitWindow False d
    go About d = (message "About Haskell" ["Haskell  0.1", "Copyright (c) 2026 Edward Kmett", "", "Haskell source editor"] d,[])
    go InspectType d = (d,[LanguageRequest TypeInfo])
    go CodeActions d = (d,[LanguageRequest RequestCodeActions])
    go RenameSymbol d = (prompt "Rename symbol" Renaming [Input "New name" "" 0] d,[])
    go Definition d = (d,[LanguageRequest FindDefinition])
    go Complete d = (d,[LanguageRequest Completions])
    go Problems d = ((setProblemsVisible (not (messagesDisplayed d)) d) {problemsFocused=not (messagesDisplayed d)},[LanguageRequest ShowProblems])
    go NextMessage d = navigateMessage 1 d
    go PreviousMessage d = navigateMessage (-1) d
    go RestartHLS d = (d,[LanguageRequest RestartLanguage])
    go (OpenLink origin target) d = case contextTarget d of
      Just (SidebarTarget trace) | contextTargetCurrent d,SourceLink (Just path)<-origin->(d,[FollowTreeLink trace path target])
      _ | linkOriginCurrent d origin->(d,[FollowLink origin target])
      _->(d {status="Link body expired."},[])
    go Help d = case find ((=="hide.help.contents") . Plugin.menuName . Plugin.menuReference) (contributedMenus d) of
      Just item -> go (contributionCommand d item) d
      Nothing | menusActive d -> (d {status="Help command is unavailable."},[])
              | otherwise -> (d,[ReadHelp])
    go (TreeCommand trace action) d
      | commandEnabled d (TreeCommand trace action) = (d,[InvokeTree trace action Plugin.HumanMenu])
      | otherwise = (d,[])
    go (RegisteredMenu reference _) d
      | commandEnabled source (RegisteredMenu reference False) = (d,[InvokeMenu reference Plugin.HumanMenu target])
      | otherwise = (d {status="Menu action is unavailable."},[])
      where target=case find ((==reference) . Plugin.menuReference) (contributedMenus source) of
              Just item | Plugin.menuSlot item=="context.source" -> sourceInvocationTarget source
              Just item | Plugin.menuSlot item=="context.window-rows" -> rowInvocationTarget source
              Just item | Plugin.menuSlot item=="context.messages" -> messageInvocationTarget source
              _ -> Nothing
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
       [Radio "Appearance" ["Light","Dark","System"] (fromEnum (appearance d)),CheckBox "Wide section titles" (wideSectionTitles d),CheckBox "Blinking cursor" (blinkCursor d),CheckBox "Streamer mode" (streamerMode d)] ++
       [CheckBox "Mac key symbols" (macKeySymbols d) | videoMode d==Nothing] ++
       [field | videoMode d/=Nothing,field<-[CheckBox "CRT filter" (crtFilter d),CheckBox "Pixelate Unicode" (pixelateUnicode d)]]) d,[])
    go (AutocompleteCommand action) d = (d,[AutocompleteAction action []])
    go ChatInputOptions d = (prompt "Chat input" ChatInputSettings
      [Radio "Enter action" ["Query","Steer"] (fromEnum (chatSubmit d))] d,[])
    go Gallery d = (prompt "Dialog controls" Widgets [Input "Module name" "Main" 4,CheckBox "Auto indent" True,Radio "Tab width" ["4 columns","8 columns"] 1,ListBox "Source files" ["Main.hs","Types.hs","Parser.hs","Syntax.hs","Eval.hs"] 0] d,[])
    go (Disabled reason) d = (d {status = reason},[])
    confirm action d = (d {dialog = Just (Dialog "Save changes?" (Confirm action) [] 0 ["Save","Discard","Cancel"] (["Save changes to:"] ++ wrapMessage (documentTitle d <> "?")))},[])
    selected d | activeConversation d = conversationSelection d
    selected d = case (activeWindow d,activeDocument d) of (Just w,Just doc) -> windowSelectedText (documentBuffer doc) w; _ -> ""

activeText :: Desktop -> Text
activeText d=case activeDocument d of
  Just doc->contents (documentBuffer doc)
  Nothing->maybe "" (\prepared->contentSlice (PluginWindow.preparedWindowText prepared) 0 (contentLength (PluginWindow.preparedWindowText prepared))) (activePluginWindow d)
currentPath :: Desktop -> Text
currentPath d = maybe "" (\doc -> T.pack (maybe (fromMaybe "" (documentSuggestedName doc)) filePath (documentFile doc))) (activeDocument d)
documentTitle :: Desktop -> Text
documentTitle d = if T.null (currentPath d) then "NONAME.HS" else currentPath d

saveRequest :: Maybe Command -> Desktop -> (Desktop,[Effect])
saveRequest after d = case (activeWindow d,activeDocument d) of
  (Just _,Just doc) | documentLabel doc /= Nothing -> (d {status="This window is read-only."},[])
  (Just w,Just doc) | Just bid<-bufferId w -> case documentFile doc of
    Nothing -> (prompt "Save file as" (Saving bid after) [Input "Name" (currentPath d) (T.length (currentPath d))] d,[])
    Just _ -> (d,[SaveDocument bid Nothing after])
  _ -> (d,[])

closeActive :: Desktop -> Desktop
closeActive d = case activeWindow d of
  Nothing -> d
  Just w -> layoutProblems d (normalizeBottom saved
    {windows=ws,dockedTerminals=M.delete (windowId w) (dockedTerminals d),
     buffers=case bufferId w of
       Just bid | not (any ((==Just bid) . bufferId) ws)->M.delete bid (buffers d)
       _->buffers d,
     retiredPluginWindows=foldr S.delete (retiredPluginWindows d) references,
     pluginWindows=foldr M.delete (pluginWindows d) references,
     conversationViews=M.map retire (conversationViews saved)})
    where
      saved=rememberConversationView d
      ws=filter ((/=windowId w).windowId) (windows d)
      references=closingConversationBodies d w++case windowContent w of PluginContent reference->[reference]; _->[]
      retire view | conversationEditorFrame view==Just (windowId w) || maybe False ((==windowContent w).PluginContent) (conversationBodyRef view)=view
        {conversationBody=case conversationBody view of
          InstalledBody reference _->maybe (conversationBody view) InertBody (M.lookup reference (pluginWindows d))
          inert->inert,
         conversationEditor=Nothing,conversationEditorFrame=Nothing}
      retire view=view

closingConversationBodies :: Desktop -> Window -> [PluginWindow.WindowRef]
closingConversationBodies d w=[reference | view<-M.elems (conversationViews d),conversationEditorFrame view==Just (windowId w),Just reference<-[conversationBodyRef view]]

-- Hidden target mounts share this host frame and close with it. Draft state
-- remains in its owning map; reopening obtains fresh frame attachment identity.
closingEditors :: Desktop -> Window -> [Effect]
closingEditors d w=map RetireEditorMount (nub (maybe [] pure (windowEditorMount w)++
  [mount | view<-M.elems (conversationViews d),conversationEditorFrame view==Just (windowId w),Just mount<-[conversationEditor view]]))

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
  Just w -> (tileWindows vertical d {windows = w {windowId = nextId d,windowNumber=nextWindowNumber d,windowEditorMount=Nothing} : windows d, nextId = nextId d+1},[])

findText :: Text -> Desktop -> Desktop
findText needle d | T.null needle = d {status = "Enter search text first."}
findText needle d = case activeWindow d of
  Nothing -> d
  Just w -> case searchFrom (snd (ordered (selection w))) of
    Nothing -> d {lastFind = needle,status = "Search text not found."}
    Just p -> ensureVisible (modifyActive (\v -> v {selection = Selection p (p+T.length needle),reviewSelection=Nothing}) d {lastFind = needle,status = "Search match."})
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
      Just p -> ensureVisible (modifyActive (\v -> v {selection=Selection p (p+size),reviewSelection=Nothing}) d {status="Search match."})
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
  Just dg | cmd==DialogCancel -> True
          | cmd==DialogAccept -> case openComboBox dg of
              Just _ -> True
              Nothing | TextArea _ editable b _ _ _:_<-drop (focus dg) (fields dg) -> editable && not (byteMode b)
                      | approvalDialog dg,focus dg<length (fields dg) -> False
                      | otherwise -> not (null (fields dg) && null (buttons dg))
          | cmd `elem` [DialogFocusNext,DialogFocusPrevious] -> not (null (fields dg) && null (buttons dg))
          | Just _<-openComboBox dg -> False
          | searching dg,cmd `elem` [Find,Replace] -> True
          | Input{}:_<-drop (focus dg) (fields dg) -> cmd `elem` map snd dialogInputKeys
          | SelectedInput{}:_<-drop (focus dg) (fields dg) -> cmd `elem` [Copy,Cut,Paste,SelectAll]
          | f:_<-drop (focus dg) (fields dg),editableArea f -> cmd `elem` [Copy,Cut,Paste,SelectAll,Undo,Redo]
  _ -> False

-- | Modal controls and field editing/search share this catalogue.
-- Semantic Accept/Cancel retain each focused control and permission owner.
dialogBindingCommands :: [Command]
dialogBindingCommands=[DialogFocusNext,DialogFocusPrevious,DialogAccept,DialogCancel,Copy,Cut,Paste,SelectAll,Undo,Redo,Find,Replace]++map snd dialogInputKeys

-- | Caret-only Input defaults ignore modifiers, retaining the original field law.
dialogInputKeys :: [(V.Key,Command)]
dialogInputKeys=[(V.KLeft,CursorLeft False),(V.KRight,CursorRight False),
  (V.KHome,CursorRowStart False),(V.KEnd,CursorRowEnd False),
  (V.KBS,DeleteBackward),(V.KDel,DeleteForward)]

-- | An open dropdown retains input ownership even if another field has focus.
dialogInputOwner :: Desktop -> Bool
dialogInputOwner d=case dialog d of
  Just dg | Nothing<-openComboBox dg,Input{}:_<-drop (focus dg) (fields dg) -> True
  _ -> False

dialogReserved :: V.Key -> [V.Modifier] -> Bool
dialogReserved key mods=key==V.KChar 'u' && V.MCtrl `elem` mods ||
  (V.MAlt `elem` mods && V.MMeta `notElem` mods && not (dialogFocusChord key mods) && case key of V.KChar _->True; _->False)

-- | Chords owned by configurable dialog focus. Control/Command Tab retains
-- its existing search-page and focus aliases, outside this bounded catalogue.
dialogFocusChord :: V.Key -> [V.Modifier] -> Bool
dialogFocusChord key mods=key `elem` [V.KChar '\t',V.KBackTab] &&
  not (any (`elem` mods) [V.MCtrl,V.MMeta])

-- | Chords whose physical defaults belong to the prepared modal map.
-- Ordinary text and fixed button mnemonics remain with their field owner.
dialogControlChord :: V.Key -> [V.Modifier] -> Bool
dialogControlChord key mods=dialogFocusChord key mods || key `elem` [V.KEnter,V.KEsc]

-- | Apply the current Enter owner directly. Dropdowns commit/open, editable
-- multiline fields insert a newline, and protected approval fields never submit.
-- Only the existing button owner can make an acceptance decision.
acceptDialog :: Desktop -> (Desktop,[Effect])
acceptDialog d=case dialog d of
  Just dg | Just (i,name,choices,_,preview)<-openComboBox dg ->
    (d {dialog=Just dg {fields=replaceAt i (ComboBox name choices preview Nothing) (fields dg)},buttonHover=Nothing,buttonPressed=Nothing},[])
  Just dg | field@(TextArea _ True b _ _ _):_<-drop (focus dg) (fields dg),not (byteMode b) ->
    let rect=fromMaybe (Rect 0 0 1 1) (listToMaybe (drop (focus dg) (fieldRects d dg)))
    in (d {dialog=Just dg {fields=replaceAt (focus dg) (textAreaEdit rect (insertText (bufferNewline b)) field) (fields dg)}},[])
  Just dg | TextArea{}:_<-drop (focus dg) (fields dg) -> (d,[])
  Just dg | ComboBox name choices chosen Nothing:_<-drop (focus dg) (fields dg) ->
    (d {dialog=Just dg {fields=replaceAt (focus dg) (ComboBox name choices chosen (Just chosen)) (fields dg)}},[])
  Just dg | approvalDialog dg,focus dg<length (fields dg) -> (d,[])
          | otherwise -> submitDialog (if focus dg>=length (fields dg) then focus dg-length (fields dg) else 0) dg d
  Nothing -> (d,[])

-- | Apply the existing lossless dropdown/permission cancellation directly.
cancelDialog :: Desktop -> (Desktop,[Effect])
cancelDialog d=case dialog d of
  Just dg | Just (i,name,choices,chosen,_)<-openComboBox dg ->
    (d {dialog=Just dg {fields=replaceAt i (ComboBox name choices chosen Nothing) (fields dg)},buttonHover=Nothing,buttonPressed=Nothing},[])
  Just dg -> (d {dialog=Nothing,buttonHover=Nothing,buttonPressed=Nothing},case purpose dg of
    PermissionDialog action -> [PermissionAction action ["1"]]
    DebugDialog action | "hdb-accept:" `T.isPrefixOf` action -> [DebugAction action ["1"]]
    _ -> [])
  Nothing -> (d,[])

-- | Advance within the current modal controls. An open dropdown commits its
-- preview before moving; focus alone never submits or retires a typed form.
moveDialogFocus :: Int -> Desktop -> (Desktop,[Effect])
moveDialogFocus delta d=case dialog d of
  Just dg | total<-length (fields dg)+length (buttons dg),total>0 ->
    case openComboBox dg of
      Just (i,name,choices,_,preview) ->
        (d {dialog=Just dg {fields=replaceAt i (ComboBox name choices preview Nothing) (fields dg),
                           focus=(i+delta) `mod` total},buttonHover=Nothing,buttonPressed=Nothing},[])
      Nothing -> (d {dialog=Just dg {focus=(focus dg+delta) `mod` total}},[])
  _ -> (d,[])

-- | Apply a permitted action to its dialog field without replaying a key through
-- configurable dispatch. The text-area adapter owns its isolated buffer/undo.
applyDialogCommand :: Command -> Desktop -> (Desktop,[Effect])
applyDialogCommand cmd d
  | not (dialogCommandAllowed cmd d) = (d,[])
  | cmd==DialogAccept = acceptDialog d
  | cmd==DialogCancel = cancelDialog d
  | cmd==DialogFocusNext = moveDialogFocus 1 d
  | cmd==DialogFocusPrevious = moveDialogFocus (-1) d
  | cmd==Find = (searchPrompt False d,[])
  | cmd==Replace = (searchPrompt True d,[])
  | Just dg<-dialog d,field@Input{}:_<-drop (focus dg) (fields dg) =
      (d {dialog=Just (replaceDialogField (inputCommand cmd field) dg)},[])
  | Just dg<-dialog d,field@(SelectedInput caption value sel):_<-drop (focus dg) (fields dg) =
      let (a,z)=ordered sel
          copied=T.take (z-a) (T.drop a value)
          edited=case cmd of
            Cut -> replaceInputSelection "" field
            Paste -> replaceInputSelection (T.filter textInputChar (clipboard d)) field
            SelectAll -> SelectedInput caption value (Selection 0 (T.length value))
            _ -> field
          updated=if cmd `elem` [Copy,Cut] then copyClipboard False copied d else d
      in (updated {dialog=Just dg {fields=replaceAt (focus dg) edited (fields dg)}},[])
  | Just dg<-dialog d,field@(TextArea _ True b sel _ _):_<-drop (focus dg) (fields dg) =
      let rect=fromMaybe (Rect 0 0 1 1) (listToMaybe (drop (focus dg) (fieldRects d dg)))
          copied=selectedText sel b
          edited=case cmd of
            Cut -> textAreaEdit rect (insertText "") field
            Paste -> textAreaEdit rect (insertText (clipboard d)) field
            Copy -> field
            _ -> textAreaEdit rect (fst . runCommand cmd) field
          updated=if cmd `elem` [Copy,Cut] then copyClipboard False copied d else d
      in (updated {dialog=Just dg {fields=replaceAt (focus dg) edited (fields dg)}},[])
  | otherwise = (d,[])

-- | Project only prepared chords and bounded contribution/focus metadata.
-- Inert runtime ownership keeps its chord with an empty target, so a frontend
-- cannot queue a raw key that redirects to a later replacement registration.
focusedBindingChords :: Desktop -> [(Text,Text)]
focusedBindingChords d
  | not (bindingInputAvailable d) = []
  | Just bindings<-effectiveBindings d, dialog d==Nothing = Bindings.bindingChordsWith target bindings
  | Just bindings<-effectiveBindings d = Bindings.bindingChordsWhere (`dialogCommandAllowed` d) bindings
  | otherwise = []
  where
    current=M.fromList [(Plugin.menuName (Plugin.menuReference item),contributionCommand d item) | item<-contributedMenus d]
    target name action=case action of
      RegisteredMenu{} | M.lookup name current/=Just action -> ""
      Disabled{} -> ""
      _ -> name

fieldHeight :: Field -> Int
fieldHeight Input{} = 3
fieldHeight SelectedInput{} = 3
fieldHeight ComboBox{} = 3
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
  | purpose dg==AutocompleteDialog "save" && w>=54 = column 3 cw (take 5 (fields dg)) ++ column (5+cw) cw (drop 5 (fields dg))
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

-- | Resize the desktop and adjust windows using their edge attachment rules.
resizeScreenMode :: (Int,Int) -> Desktop -> Desktop
resizeScreenMode (sw,sh) d = layoutBottomWindows (ensureVisibleAfterLayout d (clampHexScroll d resized {windows=map stretch (windows d)}))
  where
    resized = fst (handleEvent (V.EvResize sw sh) d)
    (oldW,oldH) = screenSize d
    oldTree = treeWidthOf d
    newTree = treeWidthOf resized
    x n = newTree + (n-oldTree) * (sw-newTree) `div` max 1 (oldW-oldTree)
    y n = 1 + (n-1) * (sh-2) `div` max 1 (oldH-2)
    stretchRect (Rect l t w h) = fitWindow resized (Rect (x l) (y t) (x (l+w)-x l) (y (t+h)-y t))
    stretch w = w {bounds=stretchRect (bounds w),restoredBounds=fmap stretchRect (restoredBounds w)}

-- | Dispatch a Vty event through modal, gesture and focused-view handling.
-- The caller must interpret returned effects in order.
handleEvent :: V.Event -> Desktop -> (Desktop,[Effect])
-- Fresh status clicks dispatch before invalidating a proposal. Captured drags
-- keep their original owner and unmodified coordinates, even outside its window.
handleEvent event@(V.EvMouseDown _ y V.BLeft _) d | drag d==Nothing, y==snd (screenSize d)-1 = handleEventCore event d
handleEvent event d=case inlineEvent event d of
  Just result->result
  Nothing->handleEventCore event (case event of
    V.EvKey{}->clearInline d
    V.EvPaste{}->clearInline d
    V.EvMouseDown{}->clearInline d
    _->d)

clearInline :: Desktop -> Desktop
clearInline d=d {inlinePreview=Nothing,inlineEpoch=inlineEpoch d+1}

inlineEligible :: Desktop -> Bool
inlineEligible d=dialog d==Nothing && menu d==Nothing && contextMenu d==Nothing && not (problemsFocused d) &&
  not (maybe False treeFocused (sideTree d)) && case (activeWindow d,activeDocument d) of
    (Just w,Just doc)->documentLabel doc==Nothing && textBuffer (documentBuffer doc) && bufferView w==CurrentView
    _->False

inlineMatches :: Desktop -> InlineView -> Bool
inlineMatches d v=inlineEligible d && inlineEpoch d==inlineGeneration v && case (activeWindow d,activeDocument d) of
  (Just w,Just doc)->windowId w==inlineWindow v && bufferId w==Just (inlineBuffer v) && revision (documentBuffer doc)==inlineRevision v && selection w==inlineSelection v
  _->False

inlineEvent :: V.Event -> Desktop -> Maybe (Desktop,[Effect])
inlineEvent (V.EvKey (V.KChar '\\') mods) d | mods==[if nativeMac d then V.MMeta else V.MAlt], inlineEligible d = Just (clearInline d,[AutocompleteAction "propose" []])
inlineEvent (V.EvKey key mods) d | Just v<-inlinePreview d,inlineMatches d v = case (key,map (\modifier->if nativeMac d && modifier==V.MMeta then V.MAlt else modifier) mods) of
  (V.KChar '[',[V.MAlt])->Just (cycleOption v (-1))
  (V.KChar ']',[V.MAlt])->Just (cycleOption v 1)
  (V.KChar '\t',[])->Just (d,[AutocompleteAction "accept" []])
  (V.KRight,[V.MAlt])->Just (d,[AutocompleteAction "word" []])
  (V.KEsc,[])->Just (clearInline d,[])
  _->Nothing
  where cycleOption v delta
          | next>=0 && next<length (inlineOptions v)=(d {inlinePreview=Just v {inlineIndex=next}},[AutocompleteAction "shown" []])
          | otherwise=(clearInline d,[AutocompleteAction (if delta>0 then "alternate-next" else "alternate-previous") []])
          where next=inlineIndex v+delta
inlineEvent _ _=Nothing

handleEventCore :: V.Event -> Desktop -> (Desktop,[Effect])
handleEventCore (V.EvMouseDown x y V.BLeft _) d | drag d==Nothing, y==snd (screenSize d)-1 =
  case find (\(rect,_,_)->inside rect x y) (statusItemRects d) of
    Just (_,_,Left cmd) -> runCommand cmd d
    Just (_,_,Right event) -> handleEvent event (if activeConversation d then (setComposerInput (composerBuffer d) (composerSelection d) True d) else d)
    Nothing -> (d,[])
handleEventCore event d = Bifunctor.first (layoutComposer d . clampReviewWindows . clampHexScroll d) $ dispatchEvent event (case event of
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
-- A compiled map owns named commands in the focused profile and context. Missing
-- chords fall back to local text/movement, never to removed command defaults.
dispatchEvent ev d | prefix d/=Nothing, not (wordStarPrefixOwner d) = dispatchEvent ev d {prefix=Nothing}
dispatchEvent (V.EvKey key mods) d
  | prefix d/=Nothing, terminalContextReserved d key mods = dispatchEvent (V.EvKey key mods) d {prefix=Nothing}
  | bindingInputAvailable d, not (terminalContextReserved d key mods), Just bindings<-effectiveBindings d =
      case Bindings.bindingAction bindings key mods of
        Just cmd | dialog d/=Nothing, not (dialogCommandAllowed cmd d) -> unboundKey key mods d
                 | sourceKeyCommand cmd || commandEnabled d cmd -> runCommand cmd d {prefix=Nothing}
                 | otherwise -> (d {prefix=Nothing},[])
        Nothing -> unboundKey key mods d
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
dispatchEvent ev d | activeAutocomplete d, Just result<-autocompleteEvent ev d = result
dispatchEvent ev d | questionActive d, Just result<-questionEvent ev d = result
dispatchEvent (V.EvKey key mods) d | Just next<-imageKey key mods d = (next,[])
dispatchEvent ev d | activeEditorMount d/=Nothing,maybe False (windowFocused d) (activeWindow d),Just result<-composerEvent ev d = result
dispatchEvent ev d | Just ident<-activeTerminal d,Just text<-terminalInput ev = (d,[ServiceAction "terminal-input" [ident,text]])
dispatchEvent (V.EvMouseUp x y button) d | Just (FollowingLink _ a b origin target)<-drag d,
  button==Nothing || button==Just V.BLeft =
    (d {drag=Nothing,dragOriginal=Nothing},[FollowLink origin target | x==a && y==b,linkOriginCurrent d origin])
dispatchEvent (V.EvMouseUp _ _ _) d = (d {drag = Nothing,dragOriginal=Nothing},[])
dispatchEvent (V.EvMouseDown x y button mods) d = mouseEvent x y button mods d
dispatchEvent ev@V.EvPaste{} d | prefix d/=Nothing = dispatchEvent ev d {prefix=Nothing}
dispatchEvent V.EvPaste{} d | activeMarkdown d = (d {status="Markdown view is read-only."},[])
dispatchEvent (V.EvPaste bytes) d | activeHex d = (either (const (d {status="Paste hexadecimal text."})) (`pasteHex` d) (TE.decodeUtf8' bytes),[])
dispatchEvent (V.EvPaste bytes) d = case TE.decodeUtf8' bytes of
  Left _ -> (message "Paste failed" ["The pasted text is not valid UTF-8."] d,[])
  Right t -> (insertText (T.filter (\c -> textInputChar c || c `elem` ['\n','\r','\t']) t) d,[])
dispatchEvent (V.EvKey key mods) d | Just tree <- sideTree d, treeFocused tree = treeKey key mods tree d
dispatchEvent (V.EvKey key mods) d | Just _<-activePluginWindow d = (pluginKey key mods d,[])
dispatchEvent (V.EvKey key mods) d = keyEvent key mods d
dispatchEvent _ d = (d,[])

-- | Exact attached editor for the currently focused frame. Metadata and draft
-- lookup are shared by input, rendering and requested-paste capture.
activeEditorMount :: Desktop -> Maybe Editor.EditorMount
activeEditorMount d=do
  w<-activeWindow d
  mount<-windowEditorMount w
  draft<-windowEditorDraft d w
  if editorDraftMount draft==Just mount then Just mount else Nothing

-- | Per-frame input state. Inactive windows never borrow another frame's draft.
windowEditorDraft :: Desktop -> Window -> Maybe EditorDraft
windowEditorDraft d w=do
  mount<-windowEditorMount w
  draft<-M.lookup (Editor.mountDraft mount) (editorDrafts d)
  if editorDraftMount draft==Just mount then Just draft else Nothing

windowHasEditor :: Desktop -> Window -> Bool
windowHasEditor d w=maybe False (const True) (windowEditorDraft d w)

windowEditorCode :: Window -> Bool
windowEditorCode=maybe False (Editor.editorCodeInput . Editor.mountSpec) . windowEditorMount

composerDraftRef :: Desktop -> Maybe Editor.DraftRef
composerDraftRef d=case activeEditorMount d of
  Just mount->Just (Editor.mountDraft mount)
  Nothing->conversationDraftRef <$> M.lookup (conversationTarget d) (conversationViews d)

-- These selectors read one actual input owner. An unmounted initial Desktop
-- has no editable draft; its empty display value is not a retained input owner.
composerBuffer :: Desktop -> Buffer
composerBuffer d=case editingInput d of
  HintInput->autocompleteDraft d
  QuestionInput->maybe emptyEditorBuffer questionBuffer (chatQuestion d)
  MountedInput->maybe emptyEditorBuffer editorDraftBuffer (composerDraftRef d >>= (`M.lookup` editorDrafts d))

emptyEditorBuffer :: Buffer
emptyEditorBuffer=newBuffer ""

composerSelection :: Desktop -> Selection
composerSelection d=case editingInput d of
  HintInput->autocompleteSelection d
  QuestionInput->maybe (Selection 0 0) questionSelection (chatQuestion d)
  MountedInput->maybe (Selection 0 0) editorDraftSelection (composerDraftRef d >>= (`M.lookup` editorDrafts d))

composerFocused :: Desktop -> Bool
composerFocused d=case editingInput d of
  HintInput->autocompleteFocused d
  QuestionInput->maybe False questionFocused (chatQuestion d)
  MountedInput->maybe False editorDraftFocused (composerDraftRef d >>= (`M.lookup` editorDrafts d))

-- | Pure update of the selected real input owner. Missing mounts never create
-- identities or input state. Argument evaluation stays as lazy as ordinary edits.
setComposerInput :: Buffer -> Selection -> Bool -> Desktop -> Desktop
setComposerInput text selected focused d=case editingInput d of
  HintInput->d {autocompleteDraft=text,autocompleteSelection=selected,autocompleteFocused=focused}
  QuestionInput->d {chatQuestion=fmap (\q->q {questionBuffer=text,questionSelection=selected,questionFocused=focused}) (chatQuestion d)}
  MountedInput->case composerDraftRef d of
    Nothing->d
    Just ref->d {editorDrafts=M.adjust (\draft->draft {editorDraftBuffer=text,editorDraftSelection=selected,editorDraftFocused=focused}) ref (editorDrafts d)}

-- | Resolve the one immutable target body without requiring a live action scope.
conversationBodySnapshot :: Text -> Desktop -> Maybe PluginWindow.PreparedWindow
conversationBodySnapshot target d=do
  view<-M.lookup target (conversationViews d)
  case conversationBody view of
    InstalledBody reference _->M.lookup reference (pluginWindows d)
    InertBody prepared->Just prepared

-- | /O(log targets)/. The logical transcript belongs to the retained target,
-- independent of visibility, viewport presentation or callable window lifetime.
conversationLogicalBody :: Text -> Desktop -> Maybe LogicalBody
conversationLogicalBody target desktop=conversationLogical =<< M.lookup target (conversationViews desktop)

-- | Installed ownership is distinct from callable liveness or visibility.
conversationBodyRef :: ConversationView -> Maybe PluginWindow.WindowRef
conversationBodyRef view=case conversationBody view of InstalledBody reference _->Just reference; InertBody{}->Nothing

-- | Exact frame/body association; labels never identify a conversation.
conversationTargetFor :: Desktop -> Window -> Maybe Text
conversationTargetFor d w=do
  PluginContent reference<-pure (windowContent w)
  fst <$> find ((==Just reference).conversationBodyRef.snd) (M.toList (conversationViews d))

-- | Host controls are usable only for their exact installed prepared snapshot.
-- A retired/inert publication remains readable but cannot recover these actions.
windowConversationControls :: Desktop -> Window -> Maybe HostBodyControls
windowConversationControls d w=do
  target<-conversationTargetFor d w
  view<-M.lookup target (conversationViews d)
  InstalledBody reference (Just (BodyControlReceipt captured _ _ _ controls))<-pure (conversationBody view)
  current<-M.lookup reference (pluginWindows d)
  if current==captured && reference `S.notMember` retiredPluginWindows d then Just controls else Nothing

-- | Only the current bounded prepared receipt maps paint to logical points.
bodyViewportFor :: Desktop -> Text -> Maybe BodyViewport
bodyViewportFor d target=do
  view<-M.lookup target (conversationViews d)
  InstalledBody reference (Just (BodyControlReceipt captured _ _ _ controls))<-pure (conversationBody view)
  current<-M.lookup reference (pluginWindows d)
  if current==captured then hostBodyViewport controls else Nothing

projectConversationSelection :: ConversationView -> Maybe BodyViewport -> Selection
projectConversationSelection view captured=case (conversationReplySelection view,captured) of
  (Just (BodySelection a z),Just viewport)->Selection (clipped viewport a) (clipped viewport z)
  _->Selection 0 0
  where
    -- Copy retains the exact logical endpoints. Keyboard movement can use the
    -- nearest currently visible endpoint instead of fabricating paint offset0
    -- when its extension anchor is offscreen. Exact paint ranges are separate.
    clipped viewport point=fromMaybe (boundary viewport point) (viewportOffset viewport point)
    boundary viewport point=case [(bodyRowPaintStart row,first) | row<-Vec.toList (viewportRows viewport),Just first<-[bodyRowPoint row],first>=point] of
      (offset,_):_->offset
      []->maybe 0 bodyRowPaintEnd (lastMaybe (Vec.toList (viewportRows viewport)))
    lastMaybe []=Nothing
    lastMaybe rows=Just (last rows)

-- | Bounded paint intervals selected in logical source space. Repeated table
-- header mappings may produce disjoint intervals; furniture has no interval.
-- Render uses these endpoints to cut ordinary runs and whole-glyph overlap for
-- exceptional glyphs. Nothing leaves ordinary/plugin Selection unchanged.
conversationPaintSelection :: Desktop -> Window -> Maybe [(Int,Int)]
conversationPaintSelection d window=do
  target<-conversationTargetFor d window
  view<-M.lookup target (conversationViews d)
  _<-conversationLogical view
  pure $ case (conversationReplySelection view,bodyViewportFor d target) of
    (Just (BodySelection anchor caret),Just viewport)->concatMap (ranges (min anchor caret) (max anchor caret)) (Vec.toList (viewportRows viewport))
    _->[]
  where
    ranges first end row=case bodyRowPoint row of
      Nothing->[]
      Just point->[(bodyRowPaintStart row+a+(start-lo)*(z-a) `div` (hi-lo),
          bodyRowPaintStart row+a+((finish-lo)*(z-a)+hi-lo-1) `div` (hi-lo))
        | (a,z,lo,hi)<-Vec.toList (bodyRowRanges row),hi>lo,
          let low=scalarPoint point lo,let high=scalarPoint point hi,
          first<high,end>low,
          let start=if sameSource first point then max lo (scalar first) else lo,
          let finish=if sameSource end point then min hi (scalar end) else hi,start<finish]
    scalarPoint (BodyPoint ident block _) value=BodyPoint ident block value
    scalarPoint (QuestionPoint token block _) value=QuestionPoint token block value
    sameSource (BodyPoint ident block _) (BodyPoint other next _)=ident==other && block==next
    sameSource (QuestionPoint token block _) (QuestionPoint other next _)=token==other && block==next
    sameSource _ _=False
    scalar (BodyPoint _ _ value)=value
    scalar (QuestionPoint _ _ value)=value

-- Pending edge/row carets use the already-demanded viewport receipt; source
-- parsing is never needed on input. Their optional anchor preserves Shift range.
settleConversationCaret :: Maybe TextLayout.TextLayout -> Maybe BodyViewport -> ConversationView -> ConversationView
settleConversationCaret layout captured view=case (conversationCaretIntent view,captured) of
  (Just (EdgeCaret end extended),Just viewport) | not end || viewportAtEnd viewport->case points viewport of
    []->view {conversationCaretIntent=Nothing}
    entries->let (point,finish)=if end then last entries else head entries
                 chosen=if end then ending point finish else point
             in selected extended chosen
  (Just (RowCaret column extended),Just viewport)->case viewportPoint viewport offset <|> firstPoint viewport of
    Just chosen->selected extended chosen
    Nothing->view {conversationCaretIntent=Nothing}
    where offset=maybe (maybe 0 bodyRowPaintStart (viewportRows viewport Vec.!? viewportDemandRow viewport))
            (\ready->TextLayout.layoutOffset ready (viewportDemandRow viewport) column) layout
  _->view
  where
    selected extended chosen=view {conversationCaretIntent=Nothing,conversationReplySelection=Just (BodySelection (fromMaybe chosen extended) chosen)}
    points viewport=[(point,bodyRowLogicalEnd row) | row<-Vec.toList (viewportRows viewport),Just point<-[bodyRowPoint row]]
    firstPoint viewport=listToMaybe [point | row<-drop (viewportDemandRow viewport) (Vec.toList (viewportRows viewport)),Just point<-[bodyRowPoint row]]
    ending (BodyPoint ident block _) scalar=BodyPoint ident block scalar
    ending (QuestionPoint token block _) scalar=QuestionPoint token block scalar

conversationEdge :: Bool -> Bool -> Desktop -> Desktop
conversationEdge end extend d=case activeWindow d of
  Just window | Just target<-conversationTargetFor d window,Just view<-M.lookup target (conversationViews d),
    Just logical<-conversationLogical view,Just first<-firstPoint target window logical->
      let requested=if end then FollowEnd else At first
          extended=if extend then case conversationCopySelection d target window of Just (BodySelection anchor _)->Just anchor; _->Nothing else Nothing
          previous=bodyViewportFor d target
          ready=case previous of
            Just viewport | end && viewportAtEnd viewport->Just viewport
            Just viewport | not end,Just actual<-viewportPoint viewport 0,actual==first->Just viewport
            _->Nothing
          next=settleConversationCaret (windowPresentation d window) ready view {conversationAnchor=requested,conversationRowShift=0,conversationCaretIntent=Just (EdgeCaret end extended)}
      in (modifyActive (\w->w {selection=projectConversationSelection next previous}) d) {conversationViews=M.insert target next (conversationViews d)}
  _->d
  where
    firstPoint target window logical=case logicalBodyItems logical Vec.!? 0 of
      Just item->Just (BodyPoint (recordId (logicalItemRecord item)) 0 0)
      Nothing | T.null target,Just question<-chatQuestion d,Just controls<-windowConversationControls d window,
        hostBodyQuestionToken controls==Just (questionToken question)->Just (QuestionPoint (questionToken question) 0 0)
      _->Nothing

rememberConversationView :: Desktop -> Desktop
rememberConversationView d=case M.lookup (conversationTarget d) (conversationViews d) of
  Just old | Just reference<-conversationBodyRef old ->
    let win=find ((==PluginContent reference).windowContent) (windows d)
        view=old {conversationScrollColumn=maybe (conversationScrollColumn old) scrollColumn win}
    in d {conversationViews=M.insert (conversationTarget d) view (conversationViews d)}
  _->d

-- An explicit IO show first installs/open-admits the target body. Pure selection
-- switches only already-owned refs, retaining hidden bodies and draft histories.
selectConversationView :: Text -> Text -> Desktop -> Desktop
selectConversationView target name original=case M.lookup target (conversationViews saved) of
  Just view | Just reference<-conversationBodyRef view ->
    let existing=find (\w->conversationTargetFor saved w/=Nothing) (windows saved)
        adjusted w=w {windowContent=PluginContent reference,windowEditorMount=conversationEditor view,
          scrollRow=maybe 0 viewportScroll (bodyViewportFor saved target),
          scrollColumn=conversationScrollColumn view,selection=projectConversationSelection view (bodyViewportFor saved target)}
        frames=case existing of
          Just old->map (\w->if windowId w==windowId old then adjusted w else w) (windows saved)
          Nothing->windows saved
        frame=windowId <$> find ((==PluginContent reference).windowContent) frames
        retained=M.map (\v->if conversationEditor v/=Nothing then v {conversationEditorFrame=frame} else v) (conversationViews saved)
        result=saved {windows=frames,childAgentSettings=[],childAgentSteering=False,childAgentContextUsage=Nothing,
          conversationTarget=target,conversationViews=M.insert target view {conversationName=name,conversationEditorFrame=frame} retained,
          editorDrafts=M.adjust (\draft->draft {editorDraftFocused=True}) (conversationDraftRef view) (editorDrafts saved),
          contextMenu=Nothing,menu=Nothing}
    in maybe result (\ident->focusWindow ident result) frame
  _->original
  where saved=rememberConversationView original

conversationHasDraft :: Desktop -> Bool
conversationHasDraft=any ((>0) . bufferLength . editorDraftBuffer) . M.elems . editorDrafts

activeConversation :: Desktop -> Bool
activeConversation d=maybe False (\w->windowFocused d w && conversationTargetFor d w/=Nothing) (activeWindow d)

conversationCopySelection :: Desktop -> Text -> Window -> Maybe BodySelection
conversationCopySelection d target window=do
  view<-M.lookup target (conversationViews d)
  conversationReplySelection view <|> do
    viewport<-bodyViewportFor d target
    BodySelection <$> viewportPoint viewport (anchor (selection window)) <*> viewportPoint viewport (caret (selection window))

conversationSelection :: Desktop -> Text
conversationSelection d=case activeWindow d of
  Just w | conversationTargetFor d w/=Nothing,Just prepared<-windowPluginText d w ->
    let (a,z)=ordered (selection w) in PluginWindow.copyPreparedSelection prepared a z
  _->""

clearReplySelection :: Desktop -> Desktop
clearReplySelection d = case activeWindow d >>= conversationTargetFor d of
  Just target->(modifyActive (\w->w {selection=Selection 0 0}) d) {conversationViews=M.adjust (\view->view {conversationReplySelection=Nothing}) target (conversationViews d)}
  _->d

composerActive :: Desktop -> Bool
composerActive d = activeEditorMount d/=Nothing && maybe False (windowFocused d) (activeWindow d) && composerFocused d && not (questionActive d)

-- Preserve the last visible reply when the draft grows; browsing older replies
-- keeps its position. Every viewport calculation uses the same draft height.
layoutComposer :: Desktop -> Desktop -> Desktop
layoutComposer before after = after {windows=map adjust (windows after)}
  where
    adjust w | Just doc<-windowDocument (buffers after) w,
               let reserved state | windowHasEditor state w = height (composerRect state w)
                                  | autocompletePane state w = height (autocompleteComposerRect state w)
                                  | otherwise = 0,
               reserved before/=reserved after =
      let oldLimit=scrollbarLimit before True doc w
          newLimit=scrollbarLimit after True doc w
      in w {scrollRow=if scrollRow w>=oldLimit then newLimit else min newLimit (scrollRow w)}
    adjust w=w

composerRect :: Desktop -> Window -> Rect
composerRect d w = Rect (x+ww-4-columns) (y+hh-1-rows) columns rows
  where
    Rect x y ww hh=bounds w
    draft=maybe emptyEditorBuffer editorDraftBuffer (windowEditorDraft d w)
    rows=min (min 12 (bufferLineCount draft)) (max 0 (hh-6))
    columns=draftColumns (windowEditorCode w) (max 0 (ww-6)) draft

-- Measure only up to the window cap. One measured seek streams borrowed rows;
-- once a row fills the bubble, neither its suffix nor later rows are needed.
-- Use the source renderer's width rules, including one-cell control placeholders.
draftColumns :: Bool -> Int -> Buffer -> Int
draftColumns code limit b
  | limit<=12=max 0 limit
  | otherwise=1+go 11 (contentSourceLinesFrom (bufferContent b) 0)
  where
    bound=limit-1
    go widest _ | widest>=bound=widest
    go widest []=widest
    go widest (line:rest)=
      let start=if code && sourceLineLength line>=4 && sourceLineSlice line 0 4=="    " then 4 else 0
      in go (max widest (sourceLineSuffixWidth line start bound)) rest

-- The transcript adapter chooses Query/Steer; generic editors use their fixed
-- default/alternate slots. Labels never select a command registration.
composerCodeInput :: Desktop -> Bool
composerCodeInput d=editingInput d==MountedInput && maybe False (Editor.editorCodeInput . Editor.mountSpec) (activeEditorMount d)

composerSubmit :: [V.Modifier] -> Desktop -> (Desktop,[Effect])
composerSubmit mods d
  | V.MShift `elem` mods || composerInCode d && V.MCtrl `notElem` mods = (composerInsert (if composerInCode d then "\n    " else "\n") d,[])
  | otherwise=submitEditorSlot slot d
  where slot | activeConversation d=if composerQuery (V.MCtrl `elem` mods) d then Editor.DefaultEditor else Editor.AlternateEditor
             | V.MCtrl `elem` mods=Editor.AlternateEditor
             | otherwise=Editor.DefaultEditor

submitEditorSlot :: Editor.EditorSlot -> Desktop -> (Desktop,[Effect])
submitEditorSlot slot d=case activeEditorMount d of
  Just mount->(d,[SubmitEditor mount slot Plugin.HumanMenu])
  Nothing->(d,[])

composerQuery :: Bool -> Desktop -> Bool
composerQuery opposite d=(chatSubmit d==QuerySubmit)/=opposite

windowContentRows :: Desktop -> Document -> Window -> Int
windowContentRows d _ w = max 0 (height (bounds w)-2-reserved)
  where reserved | windowHasEditor d w = height (composerRect d w)+1
                 | autocompletePane d w = height (autocompleteComposerRect d w)+1
                 | otherwise = 0

composerScroll :: Desktop -> Window -> (Int,Int)
composerScroll d w = (max 0 (r-height rect+1),max 0 (displayColumn line (max 0 (c-marker))-width rect+1))
  where draft=windowEditorDraft d w
        b=maybe emptyEditorBuffer editorDraftBuffer draft
        selected=maybe (Selection 0 0) editorDraftSelection draft
        (r,c)=bufferLineColumn b (caret selected)
        (marker,line)=if windowEditorCode w then composerLine b r else (0,bufferLineAt b r)
        rect=composerRect d w

composerClick :: Int -> Int -> [V.Modifier] -> Window -> Desktop -> Desktop
composerClick x y mods w d = clearReplySelection (setComposerInput (composerBuffer d) (Selection (if V.MShift `elem` mods then anchor (composerSelection d) else p) p) (True) (d {chatQuestion=fmap (\q->q {questionFocused=False}) (chatQuestion d)}))
  where
    Rect l t _ _=composerRect d w; (sr,sc)=composerScroll d w; b=composerBuffer d
    r=min (bufferLineCount b-1) (max 0 (y-t+sr))
    p=bufferLineOffset b r+marker+columnOffset line (max 0 (x-l+sc))
    (marker,line)=if windowEditorCode w then composerLine b r else (0,bufferLineAt b r)

composerInsert :: Text -> Desktop -> Desktop
composerInsert text d = clearReplySelection (setComposerInput (replaceSelection sel text (composerBuffer d)) (Selection p p) True d)
  where sel=composerSelection d; p=fst (ordered sel)+T.length text

-- Clipboard provenance is ephemeral and only applies when the pasted bytes
-- still match the source copy. Plain copies explicitly clear it.
copyClipboard :: Bool -> Text -> Desktop -> Desktop
copyClipboard code text d=d {clipboard=text,clipboardCode=if code && not (T.null text) then Just text else Nothing,
  clipboardExport=(fst (clipboardExport d)+1,Nothing)}

-- Indented Markdown keeps the draft itself as the sole editable/recoverable
-- state. A row-local marker needs no whole-draft parsing during rendering.
composerLine :: Buffer -> Int -> (Int,Text)
composerLine b row=let line=bufferLineAt b row in if "    " `T.isPrefixOf` line then (4,T.drop 4 line) else (0,line)

composerInCode :: Desktop -> Bool
composerInCode d=composerCodeInput d && fst (composerLine b (fst (bufferLineColumn b (caret (composerSelection d)))))>0
  where b=composerBuffer d

composerBlock :: Text -> Desktop -> Desktop
composerBlock text d
  | T.null text=d
  | otherwise=let changed=composerInsert replacement d in setComposerInput (composerBuffer changed) (Selection p p) (composerFocused changed) changed
  where
    b=composerBuffer d; (a,z)=ordered (composerSelection d)
    (row,col)=bufferLineColumn b a
    before=if col>0 then "\n\n" else if row>0 && not (T.null (T.strip (bufferLineAt b (row-1)))) then "\n" else ""
    parts=T.splitOn "\n" text
    lines'=if "\n" `T.isSuffixOf` text then init parts else parts
    code=T.intercalate "\n" (map ("    "<>) lines')
    (_,endCol)=bufferLineColumn b z
    after=if z<bufferLength b && endCol<T.length (bufferLineAt b (fst (bufferLineColumn b z))) then "\n\n" else "\n"
    replacement=before<>code<>after
    p=a+T.length before+T.length code

-- Send code as fenced Markdown so a preceding list cannot reinterpret the
-- draft's row-local indentation as a list continuation. Keep raw draft text
-- separate for undo, asynchronous acceptance and rejected-steering restoration.
composerMarkdown :: Text -> Text
composerMarkdown = T.intercalate "\n" . rows Nothing . T.splitOn "\n"
  where
    rows _ []=[]
    rows (Just (marker,count)) (line:rest)=line:rows (if closes marker count line then Nothing else Just (marker,count)) rest
    rows Nothing lines'@(line:rest)
      | Just fence<-opening line=line:rows (Just fence) rest
      | "    " `T.isPrefixOf` line=
          let (code,after)=span (T.isPrefixOf "    ") lines'
              body=map (T.drop 4) code
              longest=maximum (0:map (snd . T.foldl' runs (0,0)) body)
              fence=T.replicate (max 3 (longest+1)) "`"
          in fence:body++fence:rows Nothing after
      | otherwise=line:rows Nothing rest
    opening line=case T.uncons (T.dropWhile (==' ') line) of
      Just (marker,_) | marker `elem` ['`','~'], T.length line-T.length (T.dropWhile (==' ') line)<=3,
        let count=T.length (T.takeWhile (==marker) (T.dropWhile (==' ') line)),count>=3 -> Just (marker,count)
      _ -> Nothing
    closes marker count line=let stripped=T.strip line in T.length stripped>=count && T.all (==marker) stripped
    runs (current,longest) char=let next=if char=='`' then current+1 else 0 in (next,max longest next)

composerPaste :: Text -> Desktop -> Desktop
composerPaste text d
  | composerInCode d=composerInsert (T.replace "\n" "\n    " text) d
  | clipboardCode d==Just text=composerBlock text d
  | otherwise=composerInsert text d {clipboardCode=Nothing}

composerTyped :: Text -> Desktop -> Desktop
composerTyped text d
  | text==" ", anchor sel==caret sel, column==1, ">" `T.isPrefixOf` line =
      let block=composerBlock (T.drop 1 line<>"\n") (setComposerInput (composerBuffer d) (Selection start (start+T.length line)) (composerFocused d) (d))
          prefix=if row>0 && not (T.null (T.strip (bufferLineAt b (row-1)))) then 1 else 0
          pos=start+prefix+4
      in (setComposerInput (composerBuffer block) (Selection pos pos) (composerFocused block) (block))
  | otherwise=composerInsert text d
  where b=composerBuffer d; sel=composerSelection d; (row,column)=bufferLineColumn b (caret sel)
        start=bufferLineOffset b row; line=bufferLineAt b row

composerCopied :: Desktop -> (Bool,Text)
composerCopied d=(not (null pieces) && all (\(code,_) -> code) pieces,T.concat (map snd pieces))
  where
    b=composerBuffer d; (a,z)=ordered (composerSelection d)
    first=fst (bufferLineColumn b a); last'=fst (bufferLineColumn b z)
    pieces=[(marker>0,bufferSlice b (max a (start+marker)) (max 0 (min z end-max a (start+marker))))
      | row<-[first..last'],let start=bufferLineOffset b row,let end=if row+1<bufferLineCount b then bufferLineOffset b (row+1) else bufferLength b,
        start<z,end>a,let (marker,_)=composerLine b row]

composerCommand :: Command -> Desktop -> Desktop
composerCommand command d=composerCommandWith (composerCodeInput d) command d

composerCommandWith :: Bool -> Command -> Desktop -> Desktop
composerCommandWith code cmd d = case cmd of
  Copy -> copied
  Cut -> composerInsert "" copied
  Paste -> if code then composerPaste (clipboard d) d else composerInsert (clipboard d) d
  SelectAll -> clearReplySelection (setComposerInput (composerBuffer d) (Selection 0 (bufferLength b)) (composerFocused d) (d))
  Undo -> history undo
  Redo -> history redo
  _ -> d
  where
    b=composerBuffer d; sel=composerSelection d
    (isCode,text)=if code then composerCopied d else (False,selectedText sel b)
    copied=copyClipboard isCode text d
    history f=let changed=f b; bounded=min (bufferLength changed) (caret sel)
                  row=fst (bufferLineColumn changed bounded)
                  p=if code then max bounded (bufferLineOffset changed row+fst (composerLine changed row)) else bounded
              in clearReplySelection (setComposerInput changed (Selection p p) (composerFocused d) d)

composerEvent :: V.Event -> Desktop -> Maybe (Desktop,[Effect])
composerEvent event d=composerEventWith (composerCodeInput d) event d

composerEventWith :: Bool -> V.Event -> Desktop -> Maybe (Desktop,[Effect])
composerEventWith code (V.EvPaste bytes) d = Just (either (const d) (\text -> (if code then composerPaste else composerInsert) (T.filter (\c -> textInputChar c || c `elem` ['\n','\r','\t']) text) d) (TE.decodeUtf8' bytes),[])
composerEventWith code (V.EvKey key mods) d
  | key==V.KEsc, activeConversation d, composerFocused d, agentReplying d = Just (d,[AgentAction "cancel" []])
  | key==V.KChar '\t', null mods = Just ((setComposerInput (composerBuffer d) (composerSelection d) (not (composerFocused d)) d),[])
  | key==V.KEnter, composerFocused d, all (`elem` [V.MCtrl,V.MShift]) mods = Just (if code || editingInput d==MountedInput then composerSubmit mods d else if V.MShift `elem` mods then (composerInsert "\n" d,[]) else (d,[]))
  | V.KChar c<-key, textInputChar c, null mods || mods==[V.MShift] = done ((if code then composerTyped else composerInsert) (T.singleton c) d)
  | not (composerFocused d) || V.MAlt `elem` mods || V.MMeta `elem` mods = Nothing
  | effectiveBindings d==Nothing, ctrl, V.KChar c<-key, toLower c=='z', V.MShift `elem` mods = Just (if code then runCommand Redo d else (composerCommandWith False Redo d,[]))
  | effectiveBindings d==Nothing, ctrl, V.KChar c<-key, Just cmd<-lookup (toLower c) [('c',Copy),('x',Cut),('v',Paste),('a',SelectAll),('z',Undo),('y',Redo)] = Just (if code then runCommand cmd d else (composerCommandWith False cmd d,[]))
  | otherwise = case key of
      V.KLeft | marker>0 && p==start+marker -> move (if r>0 then bufferLineOffset b r-1 else p)
      V.KLeft -> move (if ctrl then bufferWordLeft b p else bufferPreviousCharacter b p)
      V.KRight -> move (if ctrl then bufferWordRight b p else bufferNextCharacter b p)
      V.KUp -> vertical (-1)
      V.KDown | marker>0, r+1==bufferLineCount b ->
        done (composerInsert "\n" (setComposerInput (composerBuffer d) (Selection (bufferLength b) (bufferLength b)) (composerFocused d) (d)))
      V.KDown -> vertical 1
      V.KHome -> move (if ctrl then 0 else start+marker)
      V.KEnd -> move (if ctrl then bufferLength b else bufferLineOffset b r+T.length (bufferLineAt b r))
      V.KBS | anchor sel==p, marker>0, p==start+marker -> erase start p
      V.KBS | anchor sel==p, marker==0, column==0, r>0, fst (composerLine b (r-1))>0 -> move (start-1)
      V.KBS -> erase (if ctrl then bufferWordLeft b p else bufferPreviousCharacter b p) p
      V.KDel | anchor sel==p, marker>0, p==start+T.length (bufferLineAt b r), r+1<bufferLineCount b, T.null (bufferLineAt b (r+1)) -> move (bufferLineOffset b (r+1))
      V.KDel -> erase p (if ctrl then bufferWordRight b p else bufferNextCharacter b p)
      _ -> Nothing
  where
    done next=Just (clearReplySelection next,[])
    b=composerBuffer d; sel=composerSelection d; p=caret sel
    (r,column)=bufferLineColumn b p; ctrl=V.MCtrl `elem` mods
    start=bufferLineOffset b r; (marker,line)=if code then composerLine b r else (0,bufferLineAt b r)
    move n=let bounded=max 0 (min (bufferLength b) n); row=fst (bufferLineColumn b bounded)
               q=if code then max bounded (bufferLineOffset b row+fst (composerLine b row)) else bounded
           in done (setComposerInput (composerBuffer d) (Selection (if V.MShift `elem` mods then anchor sel else q) q) (composerFocused d) (d))
    vertical delta=let row=max 0 (min (bufferLineCount b-1) (r+delta))
                       (prefix,target)=if code then composerLine b row else (0,bufferLineAt b row)
                   in move (bufferLineOffset b row+prefix+columnOffset target (displayColumn line (max 0 (column-marker))))
    erase a z=done (composerInsert "" (setComposerInput (composerBuffer d) (if anchor sel/=p then sel else Selection a z) (composerFocused d) (d)))
composerEventWith _ _ _ = Nothing

-- The Autocomplete hint is independent human input. Reuse editing operations
-- through a temporary projection, then copy back only its dedicated state.
autocompletePane :: Desktop -> Window -> Bool
autocompletePane d w=autocompleteACPEnabled d && maybe False ((==Just "Autocomplete") . documentLabel) (windowDocument (buffers d) w)

activeAutocomplete :: Desktop -> Bool
activeAutocomplete d=maybe False (autocompletePane d) (activeWindow d)

autocompleteProjection :: Desktop -> Desktop
autocompleteProjection d=d {editingInput=HintInput,chatQuestion=Nothing,agentReplying=False}

autocompleteEdit :: (Desktop -> Desktop) -> Desktop -> Desktop
autocompleteEdit edit d=let changed=edit (autocompleteProjection d) in d
  {autocompleteDraft=composerBuffer changed,autocompleteSelection=composerSelection changed,
   autocompleteFocused=composerFocused changed,clipboard=clipboard changed,clipboardCode=clipboardCode changed,
   clipboardExport=clipboardExport changed}

autocompleteEvent :: V.Event -> Desktop -> Maybe (Desktop,[Effect])
autocompleteEvent (V.EvKey V.KEnter mods) d
  | autocompleteFocused d, all (`elem` [V.MCtrl,V.MShift]) mods, V.MShift `notElem` mods =
      Just $ if T.null (T.strip text) then (d {status="Type a hint for autocomplete."},[])
        else (d {autocompleteDraft=newBuffer "",autocompleteSelection=Selection 0 0},[AutocompleteAction "hint" [text]])
  where text=contents (autocompleteDraft d)
autocompleteEvent (V.EvKey V.KEsc []) d=Just (d {autocompleteFocused=False},[])
autocompleteEvent event d=case composerEventWith False event (autocompleteProjection d) of
  Just (changed,[]) -> Just (autocompleteEdit (const changed) d,[])
  Just _ -> Just (d,[])
  Nothing -> Nothing

autocompleteComposerRect :: Desktop -> Window -> Rect
autocompleteComposerRect d w=Rect (x+ww-4-columns) (y+hh-1-rows) columns rows
  where
    Rect x y ww hh=bounds w
    b=autocompleteDraft d
    rows=min (min 12 (bufferLineCount b)) (max 0 (hh-6))
    columns=draftColumns False (max 0 (ww-6)) b

autocompleteComposerScroll :: Desktop -> Window -> (Int,Int)
autocompleteComposerScroll d w=(max 0 (r-height rect+1),max 0 (displayColumn (bufferLineAt b r) c-width rect+1))
  where b=autocompleteDraft d; (r,c)=bufferLineColumn b (caret (autocompleteSelection d)); rect=autocompleteComposerRect d w

autocompleteClick :: Int -> Int -> [V.Modifier] -> Window -> Desktop -> Desktop
autocompleteClick x y mods w d=d {autocompleteFocused=True,
  autocompleteSelection=Selection (if V.MShift `elem` mods then anchor (autocompleteSelection d) else p) p}
  where
    Rect l t _ _=autocompleteComposerRect d w
    (sr,sc)=autocompleteComposerScroll d w
    b=autocompleteDraft d
    row=min (bufferLineCount b-1) (max 0 (y-t+sr))
    p=bufferLineOffset b row+columnOffset (bufferLineAt b row) (max 0 (x-l+sc))

-- Inline questions have their own editing state; the ordinary draft is never
-- borrowed or cleared while a tool waits for a human response.
questionActive :: Desktop -> Bool
questionActive d=T.null (conversationTarget d) && activeConversation d && maybe False questionFocused (chatQuestion d) &&
  case (chatQuestion d,activeWindow d >>= windowConversationControls d) of
    (Just q,Just controls)->hostBodyQuestionToken controls==Just (questionToken q)
    _->False

questionEdit :: (Desktop -> Desktop) -> Desktop -> Desktop
questionEdit edit d=case chatQuestion d of
  Nothing -> d
  Just q -> let temporary=d {editingInput=QuestionInput,chatQuestion=Just q {questionFocused=True}}
                changed=edit temporary
                b=composerBuffer changed
                bound n=max 0 (min (bufferLength b) n)
                sel=composerSelection changed
            in d {clipboard=clipboard changed,clipboardCode=clipboardCode changed,clipboardExport=clipboardExport changed,chatQuestion=Just q {questionChoice=Nothing,questionFocused=True,questionBuffer=b,
                questionSelection=Selection (bound (anchor sel)) (bound (caret sel))}}

-- Only insertion normalizes input. Empty creation and these bounded edits keep
-- all history states single-line and at most 4096 characters, so navigation and
-- Undo never need to scan or repair the existing draft.
questionInsert :: Text -> Desktop -> Desktop
questionInsert text=questionEdit $ \d->
  let b=composerBuffer d; size=bufferLength b
      (rawA,rawZ)=ordered (composerSelection d)
      a=max 0 (min size rawA); z=max 0 (min size rawZ)
      inserted=T.map (\c->if c `elem` ['\n','\r','\t'] then ' ' else c) (T.take (4096-a) text)
      n=T.length inserted
      tailStart=min size (z+max 0 (4096-a-n))
      -- Preserve take 4096 (prefix<>inserted<>suffix), deleting the actual old
      -- tail rather than the characters immediately after the insertion.
      edits | tailStart<=z=[(a,size,inserted)]
            | otherwise=[(a,z,inserted),(tailStart,size,"")]
      changed | tailStart<size=either (const b) id (replaceRanges edits b)
              | otherwise=replaceSelection (Selection a z) inserted b
  in (setComposerInput (changed) (Selection (a+n) (a+n)) (composerFocused d) (d))

questionCommand :: Command -> Desktop -> Desktop
questionCommand Paste d=questionInsert (clipboard d) d
questionCommand cmd d=questionEdit (composerCommandWith False cmd) d

questionInputStart :: Int -> ChatQuestion -> Int
questionInputStart width q=columnOffset text (max 0 (displayColumn text (caret (questionSelection q))-max 1 (width-9)+1))
  where text=contents (questionBuffer q)

questionVisibleInput :: Int -> ChatQuestion -> Text
questionVisibleInput width q=T.take (columnOffset suffix (max 1 (width-8))) suffix
  where suffix=T.drop (questionInputStart width q) (contents (questionBuffer q))

-- | A question projection is usable only in its actual primary source view.
-- Hidden/child views and restyled replacement bodies cannot borrow its offsets.
windowQuestion :: Desktop -> Window -> Maybe (ChatQuestion,QuestionProjection)
windowQuestion d w=do
  q<-chatQuestion d
  target<-conversationTargetFor d w
  controls<-windowConversationControls d w
  projected<-hostBodyQuestion controls
  if T.null target && T.null (conversationTarget d) &&
      questionToken q==projectedQuestionToken projected &&
      projectedQuestionWidth projected==max 1 (width (bounds w)-2)
    then Just (q,projected) else Nothing

-- | Only bounded question rows are projected; transcript/Markdown is untouched.
questionOverlayRows :: Desktop -> Window -> [(Int,StyledText)]
questionOverlayRows d w=case windowQuestion d w of
  Nothing->[]
  Just (q,projected)->
    [(offset,styledText Keyword line)
    | (index,(starts,text))<-zip [0..] (zip (projectedQuestionChoices projected) (questionChoices q))
    , questionChoice q==Just index
    , (offset,line)<-zip starts (T.splitOn "\n" (questionChoiceLines columns True text))]
    ++[(offset-7,styledText (if questionChoice q==Nothing then Literal else Plain)
      ("Other: "<>shown<>T.replicate (max 1 (columns-7-displayColumn shown (T.length shown))) " ")) | offset<-maybe [] pure (projectedQuestionInput projected)]
    where columns=projectedQuestionWidth projected; shown=questionVisibleInput columns q

-- | The live input starts at this body coordinate. Its width is also used for
-- clipping, reverse hits and the cursor, including narrow and panned windows.
questionInputGeometry :: Desktop -> Window -> Maybe (Rect,ChatQuestion,QuestionProjection)
questionInputGeometry d w=do
  (q,projected)<-windowQuestion d w
  prepared<-windowPluginText d w
  offset<-projectedQuestionInput projected
  let (row,column)=windowTextPosition d w (PluginWindow.preparedWindowText prepared) offset
  pure (Rect (left (bounds w)+1+column-scrollColumn w) (top (bounds w)+1+row-scrollRow w)
    (max 1 (projectedQuestionWidth projected-8)) 1,q,projected)

-- | Reveal a newly focused choice/input without reflowing transcript history.
-- The owner calls this only for a new interaction, never an idle/manual scroll.
ensureQuestionVisible :: Desktop -> Desktop
ensureQuestionVisible d=case chatQuestion d of
  Just q | questionFocused q,T.null (conversationTarget d),activeConversation d->
    let visible=any (\window->case windowQuestion d window of
          Just (_,projection)->case questionChoice q of
            Just index->maybe False (not . null) (listToMaybe (drop index (projectedQuestionChoices projection)))
            Nothing->projectedQuestionInput projection/=Nothing
          _->False) (windows d)
        changed=if visible then d else d {conversationViews=M.adjust (\view->view {
          conversationAnchor=At (QuestionPoint (questionToken q) (questionBlock q) 0),
          -- A deliberate control reveal requests nearby prompt context. Keep
          -- room for Other and Submit; ordinary resize still has zero shift.
          conversationRowShift=negate (max 0 (maybe 1 (pluginBodyRows d) (activeWindow d)-(if questionChoice q==Nothing then 2 else 1)))}) "" (conversationViews d)}
        revealedWindows=map reveal (windows changed)
        revealedAnchor=case [(w,old) | w<-revealedWindows,old<-windows changed,windowId w==windowId old,scrollRow w/=scrollRow old,
                    conversationTargetFor changed w==Just ""] of
          (w,_):_->bodyViewportFor changed "" >>= \viewport->conversationScrollPoint changed w viewport (scrollRow w)
          _->Nothing
    in changed {windows=revealedWindows,conversationViews=case revealedAnchor of
      Just point->M.adjust (\view->view {conversationAnchor=At point,conversationRowShift=0}) "" (conversationViews changed)
      Nothing->conversationViews changed}
  _->d
  where
    -- Negative slots name fixed controls, independently of prompt parsing.
    questionBlock q=maybe (-1) (\index->(-3)-index) (questionChoice q)
    reveal w=case windowQuestion d w of
      Just (q,projected) | questionFocused q,Just prepared<-windowPluginText d w ->
        let offset=case questionChoice q of
              Just index->(listToMaybe =<< listToMaybe (drop index (projectedQuestionChoices projected))) <|> projectedQuestionInput projected
              Nothing->projectedQuestionInput projected
            row=maybe (scrollRow w) (fst . windowTextPosition d w (PluginWindow.preparedWindowText prepared)) offset
            rows=max 1 (pluginBodyRows d w)
            previous=scrollRow w
            revealed=if row<previous then row else if row>=previous+rows then row-rows+1 else previous
            -- Include Submit when it fits, but never scroll Other out of view.
            followed=if questionChoice q==Nothing && rows>1 && row+1>=revealed+rows then row-rows+2 else revealed
        in w {scrollRow=max 0 followed}
      _->w

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
    V.EvPaste bytes -> Just (either (const (questionEdit id d))
      (\text->questionInsert (T.filter (\c->textInputChar c || c `elem` ['\n','\r','\t']) text) d) (TE.decodeUtf8' bytes),[])
    V.EvKey (V.KChar c) mods | textInputChar c, null mods || mods==[V.MShift] -> Just (questionInsert (T.singleton c) d,[])
    V.EvKey V.KEnter mods | V.MShift `elem` mods, all (`elem` [V.MCtrl,V.MShift]) mods -> Just (questionInsert " " d,[])
    V.EvKey (V.KChar c) mods | effectiveBindings d==Nothing, toLower c=='v', V.MCtrl `elem` mods,
      V.MAlt `notElem` mods, V.MMeta `notElem` mods -> Just (questionCommand Paste d,[])
    _ -> let temporary=d {editingInput=QuestionInput,chatQuestion=Just q {questionFocused=True}}
         in case composerEventWith False event temporary of
           Just (_,effects) | not (null effects) -> Just (d,[])
           Just _ -> Just (questionEdit (\state->maybe state fst (composerEventWith False event state)) d,[])
           Nothing -> Nothing
  where
    send action q=Just (d,[AgentAction action [T.pack (show (questionToken q))]])
    choose delta q=let count=length (questionChoices q)+1
                       index=(maybe 0 (+1) (questionChoice q)+delta+count) `mod` count
                   in Just (d {chatQuestion=Just q {questionChoice=if index==0 then Nothing else Just (index-1)}},[])

conversationClick :: Int -> Int -> Window -> Desktop -> Maybe Effect
conversationClick x y w d=do
  prepared<-windowPluginText d w
  controls<-windowConversationControls d w
  let rect=pluginTextRect d w
      offset=windowTextOffset d w (PluginWindow.preparedWindowText prepared)
        (y-top rect+scrollRow w) (x-left rect+scrollColumn w)
  if not (inside rect x y) then Nothing
  else case find (\(a,z,_,_)->offset>=a && offset<z) (hostBodyActions controls) of
    Just (_,_,action,_) | "question-" `T.isPrefixOf` action,Nothing<-windowQuestion d w -> Nothing
    Just (_,_,"question-input",values) -> do
      (inputRect,q,projected)<-questionInputGeometry d w
      let input=columnOffset (questionVisibleInput (projectedQuestionWidth projected) q) (max 0 (x-left inputRect))
      pure (AgentAction "question-input" (values++[T.pack (show input)]))
    Just (_,_,action,values)->Just (AgentAction action values)
    Nothing->Nothing

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
  V.EvKey (V.KChar ' ') _ -> case menuItemsFor d i !! j of
    MenuItem _ _ (SetBufferView mode) -> runCommand (SetDefaultBufferView mode) d
    _ -> invoke j
  V.EvKey (V.KChar c) _ -> case findIndex (\item -> toLower c == menuMnemonic item) (menuItemsFor d i) of
    Just k -> invoke k
    _ -> (d,[])
  V.EvMouseDown x 0 V.BLeft _ -> case menuAt x of Just k -> choose k 0; _ -> (d {menu=Nothing},[])
  V.EvMouseDown x y V.BLeft _ -> let r = menuRect d i in if inside r x y && y>top r && y<top r+height r-1 then let k=y-top r-1 in case menuItemsFor d i !! k of
      MenuItem _ _ (SetBufferView mode) | x<=left r+4 -> runCommand (SetDefaultBufferView mode) d
      _ -> invoke k
    else (d {menu=Nothing},[])
  _ -> (d,[])
  where
    choose a b = let a' = a `mod` length menus in (d {menu = Just (a',b `mod` length (menuItemsFor d a'))},[])
    invoke k = let MenuItem _ _ command = menuItemsFor d i !! k in if menuCommandAvailable d command then runCommand command d else (d {menu=Nothing},[])

menuAt :: Int -> Maybe Int
menuAt x = findIndex (\(start,w) -> x >= start && x < start+w) menuPositions

contextItems :: ContextKind -> [(Text,Command)]
contextItems (TreeContext _ items) = items
contextItems (ToolchainContext items) = items
contextItems (LinkContext command) = [("Open",command)]
contextItems (ShellContext command) = [("Execute in terminal",command)]
contextItems (ChangeContext command) = ("Revert this change",command):contextItems SourceContext
contextItems WindowRowsContext = []
contextItems SourceContext = [("Copy Location",CopyLocation),("Rename symbol...",RenameSymbol),("Code actions...",CodeActions),("Go to definition",Definition),("Inspect type",InspectType),("Complete identifier",Complete)]
contextItems MessagesContext = [("Go to source",GoToMessage),("Copy message",Copy),("Copy all messages",CopyAllMessages),("Hide Messages",Problems)]
contextItems (AgentContext items) = items
contextItems GitContext = [("Pull",GitPull),("Fetch",GitFetch),("Merge...",GitMerge)]

-- | Compose prepared context contributions with the existing session actions.
contextItemsFor :: Desktop -> [(Text,Command)]
contextItemsFor d | contextKind d==MessagesContext = map replace base++extras
  where
    base=contextItems MessagesContext
    additions=[(Plugin.menuTitle item,contributionCommand d item) | item<-contributedMenus d,Plugin.menuSlot item=="context.messages"]
    sourceEntry=find (\(_,command)->case command of RegisteredMenu ref _->Plugin.menuName ref=="hide.messages.go-to"; _->False) additions
    replace entry@(_,GoToMessage)=maybe entry id sourceEntry
    replace entry=entry
    extras=[entry | entry@(_,command)<-additions,case command of RegisteredMenu ref _->Plugin.menuName ref/="hide.messages.go-to"; _->True]
contextItemsFor d | sourceContextKind (contextKind d) =
  contextItems (contextKind d)++[(Plugin.menuTitle item,contributionCommand d item) | item<-contributedMenus d,Plugin.menuSlot item=="context.source"]
contextItemsFor d | contextKind d==WindowRowsContext = [(Plugin.menuTitle item,contributionCommand d item) | item<-contributedMenus d,Plugin.menuSlot item=="context.window-rows",Plugin.menuReference item `elem` windowRowMenuRefs d]
contextItemsFor d=contextItems (contextKind d)

sourceContextDocument :: Document -> Bool
sourceContextDocument doc=maybe True (T.isPrefixOf "Source ") (documentLabel doc)

sourceContextKind :: ContextKind -> Bool
sourceContextKind SourceContext=True
sourceContextKind ChangeContext{}=True
sourceContextKind _=False

sourceInvocationTarget :: Desktop -> Maybe ContextTarget
sourceInvocationTarget d
  | contextMenu d/=Nothing, sourceContextKind (contextKind d) = contextTarget d
  | otherwise = captureContextTarget SourceContext d

-- Only an open Messages popup retains its original capture. Direct key/menu
-- invocation captures the live owner, never a leftover dismissed popup target.
messageInvocationTarget :: Desktop -> Maybe ContextTarget
messageInvocationTarget d
  | contextMenu d/=Nothing && contextKind d==MessagesContext = contextTarget d
  | otherwise = captureContextTarget MessagesContext d

-- Prepared metadata alone scopes row menu visibility. An unrelated list has
-- no attached refs; labels, durable type and NodeId spelling confer no actions.
windowRowMenuRefs :: Desktop -> [Plugin.MenuRef]
windowRowMenuRefs d=windowRowMenuRefsFor (rowInvocationTarget d) d

windowRowMenuRefsFor :: Maybe ContextTarget -> Desktop -> [Plugin.MenuRef]
windowRowMenuRefsFor (Just (WindowRowTarget reference _)) d=case M.lookup reference (pluginWindows d) of
  Just prepared | PluginWindow.RowsDetails _ _ references<-PluginWindow.preparedWindowRows prepared->references
  _->[]
windowRowMenuRefsFor _ _=[]

-- An open rows popup keeps its captured job; direct invocation takes current ID.
rowInvocationTarget :: Desktop -> Maybe ContextTarget
rowInvocationTarget d
  | contextMenu d/=Nothing && contextKind d==WindowRowsContext=contextTarget d
  | otherwise=captureContextTarget WindowRowsContext d

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
openContext kind x y d = d {contextKind=kind,contextTarget=captureContextTarget kind d,contextMenu=Just (popup,0),drag=Nothing,dragOriginal=Nothing,menu=Nothing}
  where
    (sw,sh)=screenSize d
    items=contextItemsFor d {contextKind=kind}
    h=max 3 (min (sh-2) (length items+2))
    w=min sw (max 24 (maximum (0:[keyLabelWidth title+keyLabelWidth (menuShortcut d (MenuItem title "" cmd))+5 | (title,cmd)<-items])))
    popup=Rect (max 0 (min x (sw-w))) (max 1 (min y (sh-h-1))) w h

-- Source actions are admitted only at their captured view/caret/revision.
-- Focus or edits while a popup is open refuse it instead of redirecting it.
captureContextTarget :: ContextKind -> Desktop -> Maybe ContextTarget
captureContextTarget kind d = case kind of
  WindowRowsContext -> do
    w<-activeWindow d
    PluginContent reference<-pure (windowContent w)
    (_,_,ident,_)<-windowRows d w
    pure (WindowRowTarget reference ident)
  TreeContext trace _ -> Just (SidebarTarget trace)
  SourceContext -> source
  ChangeContext{} -> source
  AgentContext{} -> Just (ConversationTarget (conversationTarget d))
  MessagesContext -> Just $ if messagesOwner d then
    MessagesTarget (diagnosticsGeneration d) (problemsSelected d) (case drop (problemsSelected d) (diagnostics d) of
      problem:_ -> let path=diagnosticPath problem; row=diagnosticRow problem; column=diagnosticColumn problem
                   in path `seq` row `seq` column `seq` Just (path,row,column)
      _ -> Nothing)
    else UnavailableMessagesTarget
  _ -> Nothing -- These actions already carry arguments or have session scope.
  where
    source=Just $ case (activeWindow d,activeDocument d) of
      (Just w,Just doc) | bufferView w/=MarkdownView, sourceContextDocument doc, windowFocused d w, Just bid<-bufferId w ->
        let b=documentBuffer doc
            selected=selection w
            row=1+fst (bufferLineColumn b (caret selected))
        in SourceTarget (windowId w) bid (revision b) selected row
          (if byteMode b then Nothing else sourceExpression b selected) (filePath <$> documentFile doc)
      _ -> UnavailableSourceTarget

-- | Frozen bounded expression. Selection overflow is unavailable rather than a
-- truncated executable expression. Identifier capture touches at most 8194 chars;
-- forcing its small Text before publication does not retain the Buffer/Undo.
sourceExpression :: Buffer -> Selection -> Maybe Text
sourceExpression b selected
  | a<z = if z-a>limit then Nothing else ready (bufferSlice b a (z-a))
  | otherwise =
      let start=max 0 (a-limit-1)
          nearby=bufferSlice b start (2*limit+2)
          offset=a-start
          before=T.takeWhileEnd wordChar (T.take offset nearby)
          after=T.takeWhile wordChar (T.drop offset nearby)
          expression=before<>after
      in if T.length expression>limit || T.null expression then Nothing else ready expression
  where
    (a,z)=ordered selected
    limit=4096
    ready text=let frozen=T.copy text in T.length frozen `seq` Just frozen

messagesOwner :: Desktop -> Bool
messagesOwner d=messagesDisplayed d && problemsFocused d && not (maybe False treeFocused (sideTree d))

contextTargetCurrent :: Desktop -> Bool
contextTargetCurrent d = case contextTarget d of
  Just (WindowRowTarget reference ident) -> reference `S.notMember` retiredPluginWindows d &&
    any ((==PluginContent reference).windowContent) (windows d) && case M.lookup reference (pluginWindows d) of
      Just prepared | PluginWindow.RowsDetails _ index _<-PluginWindow.preparedWindowRows prepared->M.member ident index
      _->False
  Just (SidebarTarget trace) -> maybe False (\tree->treeFocused tree && hitCurrent trace tree) (sideTree d)
  Nothing -> True
  Just (ConversationTarget target) -> conversationTarget d==target
  Just target@SourceTarget{} -> case (activeWindow d,activeDocument d) of
    (Just window,Just doc) -> bufferView window/=MarkdownView && sourceContextDocument doc && windowFocused d window &&
      windowId window==sourceTargetWindow target && bufferId window==Just (sourceTargetBuffer target) &&
      revision (documentBuffer doc)==sourceTargetRevision target && selection window==sourceTargetSelection target &&
      fmap filePath (documentFile doc)==sourceTargetFile target
    _ -> False
  Just (MessagesTarget generation index _) -> messagesOwner d && diagnosticsGeneration d==generation && problemsSelected d==index
  Just UnavailableMessagesTarget -> False
  Just UnavailableSourceTarget -> False

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
    invoke i=case drop i items of (_,cmd):_ | contextTargetCurrent d && commandEnabled d cmd -> runCommand cmd d; _ -> close
    items=contextItemsFor d

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

-- A coalesced wheel burst retains distance but renders only its final state.
wheelEvent :: Int -> Int -> Int -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
wheelEvent x y steps mods d = go (abs steps) d []
  where
    event=V.EvMouseDown x y (if steps>0 then V.BScrollUp else V.BScrollDown) mods
    go 0 current pending=(current,concat (reverse pending))
    go n current pending=let (next,requests)=handleEvent event current in go (n-1) next (requests:pending)

mouseEvent :: Int -> Int -> V.Button -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
mouseEvent x y V.BLeft _ d | Just capture <- drag d = (case capture of
  FollowingLink i a b origin target
    | x==a && y==b -> d {drag=Just (FollowingLink i a b origin target)}
    | otherwise -> selectAt True x y (focusWindow i d) {drag=Just (Selecting i)}
  ImagePanning wid startX startY (Canvas.CanvasView zoom dx dy) -> mapWindow wid (\w->w {imageViewport=Canvas.CanvasView zoom
    (max (-300000) (min 300000 (dx+fromIntegral (x-startX)*8)))
    (max (-300000) (min 300000 (dy+fromIntegral ((y-startY)*modeHeight (fromMaybe 3 (videoMode d))))))}) d
  ReviewSizing wid -> case find ((==wid).windowId) (windows d) of
    Just w -> mapWindow wid (\v -> v {reviewSplit=max 0 (min 100 ((x-left (bounds w)-1)*100 `div` max 1 (width (bounds w)-3)))}) d
    Nothing -> d
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
    V.BScrollUp | Just _<-windowImage focused w,inside (pluginTextRect focused w) x y -> (fromMaybe focused (imageKey (V.KChar '+') [] focused),[])
    V.BScrollDown | Just _<-windowImage focused w,inside (pluginTextRect focused w) x y -> (fromMaybe focused (imageKey (V.KChar '-') [] focused),[])
    V.BScrollUp | Just _<-windowRows focused w,inside (fst (rowsWindowRects focused w)) x y -> (moveWindowRow (-3) focused,[])
    V.BScrollUp -> (changeScroll True (-3) focused,[])
    V.BScrollDown | Just _<-windowRows focused w,inside (fst (rowsWindowRects focused w)) x y -> (moveWindowRow 3 focused,[])
    V.BScrollDown -> (changeScroll True 3 focused,[])
    V.BRight | Just _<-windowRows focused w,inside (fst (rowsWindowRects focused w)) x y ->
      (openContext WindowRowsContext x y (selectAt False x y focused),[])
    V.BRight | Just command<-linkAt x y focused -> (openContext (LinkContext command) x (y+1) focused,[])
    V.BRight | x>l && x<l+ww-1 && y>t && y<t+hh-1,
               Just command<-shellBlockAt x y focused -> (openContext (ShellContext command) x y focused,[])
    V.BRight | x>l && x<l+ww-1 && y>t && y<t+hh-1,
               not (activeMarkdown focused), maybe False sourceContextDocument (activeDocument focused) ->
      let pointed=selectAt False x y focused
          (a,z)=ordered (selection w)
          hit=maybe (-1) (caret . selection) (activeWindow pointed)
          captured=if bufferView w==CurrentView && a<z && hit>=a && hit<z then focused else pointed
          opened=openContext (reviewContext x y focused) x y captured
          row=maybe 1 (\doc->1+fst (bufferLineColumn (documentBuffer doc) hit)) (activeDocument captured)
          freeze target@SourceTarget{}=target {sourceTargetRow=row}
          freeze target=target
      in (opened {contextTarget=fmap freeze (contextTarget opened)},[])
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
      | Just (rect,_)<-windowScrollbar focused True w, inside rect x y -> (scrollClick True x y focused,[])
      | Just (rect,_)<-windowScrollbar focused False w, inside rect x y -> (scrollClick False x y focused,[])
      | x==l -> (beginWindowDrag (EdgeSizing (windowId w) False True 0) focused,[])
      | x==l+ww-1 -> (beginWindowDrag (EdgeSizing (windowId w) False False 1) focused,[])
      | y==t+hh-1 -> (beginWindowDrag (EdgeSizing (windowId w) True False 1) focused,[])
      | Just _<-windowImage focused w,videoMode focused/=Nothing || browserFrontend focused,inside (pluginTextRect focused w) x y -> (focused {drag=Just (ImagePanning (windowId w) x y (imageViewport w))},[])
      | bufferView w==SideBySideView, x==left (bounds w)+1+fst (reviewPaneWidths w) -> (focused {drag=Just (ReviewSizing (windowId w))},[])
      | activeAutocomplete focused, inside (autocompleteComposerRect focused w) x y -> (autocompleteClick x y mods w focused,[])
      | activeAutocomplete focused, y>=top (autocompleteComposerRect focused w) -> (focused,[])
      | Just (OpenLink origin target)<-linkAt x y focused, null mods ->
          (selectAt False x y focused {drag=Just (FollowingLink (windowId w) x y origin target)},[])
      | activeConversation focused, Just action<-conversationClick x y w focused -> (focused {drag=Nothing},[action])
      | windowHasEditor focused w, inside (composerRect focused w) x y -> (composerClick x y mods w focused,[])
      | windowHasEditor focused w, y>=top (composerRect focused w) -> (focused,[])
      | otherwise -> (selectAt (V.MShift `elem` mods) x y (setComposerInput (composerBuffer focused) (composerSelection focused) (if activeConversation focused then True else composerFocused focused) (focused {drag=Just (Selecting (windowId w)), autocompleteFocused=if activeAutocomplete focused then False else autocompleteFocused focused})),[])
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

conversationPositionText :: Desktop -> Text
conversationPositionText d = " "<>case conversationContextUsage d of
  Just (used,size) | used>=0 && size>0 -> T.pack (show (used*100 `div` size))<>"% · "<>formatTokenCount used<>"/"<>formatTokenCount size<>" "
  _ -> "-- "
windowPositionText :: Desktop -> Document -> Window -> Text
windowPositionText d _ w | bufferView w==MarkdownView = case windowMarkdown d w of
  Nothing->" Markdown "
  Just (layout,_,_)->let (r,c)=TextLayout.layoutPosition layout (caret (selection (displayWindow w))) in " Markdown "<>T.pack (show (r+1))<>":"<>T.pack (show (c+1))<>" "
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
scrollbarLimit d vertical doc w | bufferView w==MarkdownView=markdownScrollLimit d vertical w
scrollbarLimit d vertical doc w = max 0 (if vertical then (case windowPresentation d w of Just layout->Vec.length (TextLayout.layoutRows layout); Nothing->documentRows doc w)-max 1 (windowContentRows d doc w)
  else (case windowPresentation d w of Just layout->TextLayout.layoutWidth layout; Nothing->windowDocumentWidth doc w)-max 1 (if bufferView w==SideBySideView then min (fst (reviewPaneWidths w)) (snd (reviewPaneWidths w)) else width (bounds w)-2)+(if byteMode (documentBuffer doc) then 0 else 1))

-- | Frame scrollbar geometry and limits for the actual semantic view. Plain
-- plugin extents are prepared/cached by their worker; no content is scanned here.
windowScrollbar :: Desktop -> Bool -> Window -> Maybe (Rect,Int)
windowScrollbar d _ original | Just _<-windowImage d original=Nothing
windowScrollbar d vertical original=case windowContent original of
  SourceContent _->do
    doc<-windowDocument (buffers d) original
    pure (scrollbarRect d vertical doc w,scrollbarLimit d vertical doc w)
  PluginContent _->do
    prepared<-windowPluginText d w
    let Rect x y ww hh= bounds w
        detail=pluginTextRect d w
        rect=if vertical then Rect (x+ww-1) (top detail) 1 (height detail)
          else let start=2+if conversationTargetFor d w/=Nothing then T.length (conversationPositionText d) else 0
               in Rect (x+start) (y+hh-1) (max 0 (ww-start-2)) 1
        extent=if vertical then windowTextRows d w (PluginWindow.preparedWindowText prepared)
          else maybe (PluginWindow.preparedWindowWidth prepared) TextLayout.layoutWidth (windowPresentation d w)
        viewport=if vertical then max 1 (height detail) else max 1 (ww-2)
    pure (rect,case (vertical,conversationTargetFor d w) of
      (True,Just target) | Just captured<-bodyViewportFor d target->snd (viewportProgress captured)
      _->max 0 (extent-viewport+(if vertical then 0 else 1)))
  where w=displayWindow original

-- Conversation frame scrolling is logical; its paint rows cover only the
-- current viewport. The scalar progress receipt is prepared outside input.
windowScrollPosition :: Desktop -> Bool -> Window -> Int
windowScrollPosition d True window | Just target<-conversationTargetFor d window,
  Just captured<-bodyViewportFor d target=case M.lookup target (conversationViews d) of
    Just view | conversationAnchor view==FollowEnd->snd (viewportProgress captured)
    _->fst (viewportProgress captured)
windowScrollPosition _ vertical window=if vertical then scrollRow (displayWindow window) else scrollColumn (displayWindow window)

-- Each split chooses its own layout. Keep the byte viewport and a visible caret
-- anchored when resizing or docking Files changes the number of bytes per row.
clampHexScroll :: Desktop -> Desktop -> Desktop
clampHexScroll before d = d {windows=map clamp (windows d)}
  where
    clamp w | Just doc<-windowDocument (buffers d) w, byteMode (documentBuffer doc) =
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
changeScroll True delta d | Just window<-activeWindow d,Just target<-conversationTargetFor d window =
  d {conversationViews=M.adjust (\view->view {conversationRowShift=conversationRowShift view+delta,conversationCaretIntent=Nothing}) target (conversationViews d)}
changeScroll False delta d | Just window<-activeWindow d,Just target<-conversationTargetFor d window,
  Just (_,limit)<-windowScrollbar d False window=
    let column=max 0 (min limit (scrollColumn window+delta))
    in (modifyActive (\w->w {scrollColumn=column}) d) {conversationViews=M.adjust (\view->view {conversationScrollColumn=column}) target (conversationViews d)}
changeScroll vertical delta d | activeMarkdown d,Just w<-activeWindow d =
  modifyActive (modifyDisplayedWindow (\shown->if vertical then shown {scrollRow=max 0 (min (markdownScrollLimit d True w) (scrollRow shown+delta))}
    else shown {scrollColumn=max 0 (min (markdownScrollLimit d False w) (scrollColumn shown+delta))})) d
changeScroll vertical delta d | Just w<-activeWindow d,PluginContent{}<-windowContent w,Just (_,limit)<-windowScrollbar d vertical w =
  modifyActive (\shown->if vertical then shown {scrollRow=max 0 (min limit (scrollRow shown+delta))}
    else shown {scrollColumn=max 0 (min limit (scrollColumn shown+delta))}) d
changeScroll vertical delta d = case (activeWindow d,activeDocument d) of
  (Just w,Just doc) ->
    let requested=max 0 ((if vertical then scrollRow w else scrollColumn w)+delta)
        prospective=if vertical then w {scrollRow=requested} else w {scrollColumn=requested}
        -- Hints affect only the drawn thumb. Never cap a prospective seek by
        -- an old receipt: replacement may have the same numeric revision.
        value=min (scrollbarLimit d vertical doc prospective {sourceWidthHint=Nothing}) requested
        changed=if vertical then w {scrollRow=value} else w {scrollColumn=value}
    in modifyActive (const (rememberSourceWidth d doc changed)) d
  _ -> d

scrollClick :: Bool -> Int -> Int -> Desktop -> Desktop
scrollClick vertical x y d = case activeWindow d of
  Just original | Just (r,limit)<-windowScrollbar d vertical original ->
    let w=displayWindow original
        len=if vertical then height r else width r
        offset=if vertical then y-top r else x-left r
        thumb=scrollbarThumb len limit (windowScrollPosition d vertical w)
        page=max 1 (case windowContent w of PluginContent _->(if vertical then height else width) (pluginTextRect d w); _->(if vertical then height else width) (bounds w)-2)
    in if offset==0 then changeScroll vertical (-1) d
       else if offset==len-1 then changeScroll vertical 1 d
       else if offset==thumb then d {drag=Just (Scrolling (windowId w) vertical)}
       else changeScroll vertical (if offset<thumb then negate page else page) d
  _ -> d

scrollTrack :: Bool -> Int -> Int -> Desktop -> Desktop
scrollTrack True x y d | Just window<-activeWindow d,Just target<-conversationTargetFor d window,
  Just logical<-conversationLogicalBody target d,Just (area,_)<-windowScrollbar d True window=
    let slots=logicalBodyItems logical
        fraction=max 0 (min 1000 ((y-top area-1)*1000 `div` max 1 (height area-3)))
        scaled=Vec.length slots*fraction
        index=min (Vec.length slots-1) (scaled `div` 1000)
        point=if fraction>=1000 then FollowEnd else WithinItem (recordId (logicalItemRecord (slots Vec.! index))) (scaled `mod` 1000)
    in if Vec.null slots then d else d {conversationViews=M.adjust (\view->view {conversationAnchor=point,conversationRowShift=0,conversationCaretIntent=Nothing}) target (conversationViews d)}
scrollTrack vertical x y d = case activeWindow d of
  Just original | Just (r,limit)<-windowScrollbar d vertical original -> let { w=displayWindow original
                         ; len=if vertical then height r else width r
                         ; offset=if vertical then y-top r else x-left r
                         ; liveLimit=case (not vertical && offset>=len-2,activeDocument d) of
                             (True,Just doc)->scrollbarLimit d False doc w {sourceWidthHint=Nothing}
                             _->limit
                         ; value=max 0 (min liveLimit ((offset-1)*liveLimit `div` max 1 (len-3))) }
                     in changeScroll vertical (value-(if vertical then scrollRow w else scrollColumn w)) d
  _ -> d

selectAt :: Bool -> Int -> Int -> Desktop -> Desktop
selectAt extend x y d | activeMarkdown d,Just original<-activeWindow d,Just (_,text,_)<-windowMarkdown d original =
  let w=displayWindow original; row=y-top (bounds w)-1+scrollRow w; col=x-left (bounds w)-1+scrollColumn w
  in markdownMoveTo extend (windowTextOffset d w text row col) d
selectAt _ _ _ d | activeMarkdown d=d
selectAt _ x y d | Just w<-activeWindow d,Just (_,index,ident,_)<-windowRows d w,
  inside (fst (rowsWindowRects d w)) x y = selectWindowRow (rowsListOffset d w (fromMaybe 0 (M.lookup ident index))+y-top (fst (rowsWindowRects d w))) d
selectAt extend x y d | Just view<-activePluginWindow d,Just w<-activeWindow d =
  let text=PluginWindow.preparedWindowText view
      row=max 0 (min (windowTextRows d w text-1) (y-top (pluginTextRect d w)+scrollRow w))
      col=max 0 (x-left (pluginTextRect d w)+scrollColumn w)
      pos=windowTextOffset d w text row col
      focused=modifyActive (\v->v {rowsInteraction=fmap (\(RowsInteraction ident _)->RowsInteraction ident True) (rowsInteraction v)}) d
  in pluginMoveTo extend pos focused
selectAt extend x y d = case activeWindow d of
  Nothing -> d
  Just w | activeHex d -> let { col=max 0 (x-left (bounds w)-1+scrollColumn w)
                             ; row=max 0 (y-top (bounds w)-1+scrollRow w)
                             ; (offset,ascii,low)=hexHit (windowHexBytes w) col }
                            in modifyActive (\v -> v {windowHexAscii=ascii,windowHexLow=low}) (moveTo extend (row*windowHexBytes w+offset) d)
  Just w | Just doc<-activeDocument d, let b=documentBuffer doc, windowChangeView b w -> case reviewHit x y b w of
    Nothing -> d
    Just (side,_,pos) -> let { start=if extend then maybe (liveToChangeOffset b (anchor (selection w))) (anchor . reviewRange) (windowReviewSelection b w >>= \r -> if reviewSide r==side then Just r else Nothing) else pos
                           ; live=if side==OriginalSide then selection w else Selection (changeToLiveOffset b start) (changeToLiveOffset b pos) }
                         in modifyActive (\v -> v {selection=live,reviewSelection=Just (ReviewSelection (revision b) (bufferLineChanges b) side (Selection start pos))}) d
  Just w -> moveTo extend pos d where
    b = maybe (newBuffer "") documentBuffer (activeDocument d)
    row = max 0 (min (windowTextRows d w (bufferContent b)-1) (y-top (bounds w)-1+scrollRow w))
    col = max 0 (x-left (bounds w)-1+scrollColumn w)
    pos = case activeWindow d of Just w->windowTextOffset d w (bufferContent b) row col; Nothing->0

-- | Read-only semantic text navigation; printable keys cannot edit a source behind it.
pluginKey :: V.Key -> [V.Modifier] -> Desktop -> Desktop
pluginKey key mods d | Just w<-activeWindow d,Just (rows,index,ident,details)<-windowRows d w =
  if key==V.KChar '\t' || key==V.KBackTab then
    modifyActive (\v->v {rowsInteraction=Just (RowsInteraction ident (not details))}) d
  else if not details then selectWindowRow (listChoice key (Vec.length rows) (fromMaybe 0 (M.lookup ident index))) d
  else pluginDetailsKey key mods d
pluginKey key mods d=pluginDetailsKey key mods d

pluginDetailsKey :: V.Key -> [V.Modifier] -> Desktop -> Desktop
pluginDetailsKey key mods d | Just view<-activePluginWindow d,Just w<-activeWindow d = readOnlyTextKey (PluginWindow.preparedWindowText view) w (\extend target->pluginMoveTo extend target d) key mods d
pluginDetailsKey _ _ d=d

readOnlyTextKey :: BufferContent -> Window -> (Bool -> Int -> Desktop) -> V.Key -> [V.Modifier] -> Desktop -> Desktop
readOnlyTextKey text w moveToText key mods d=
  let pos=caret (selection w)
      extend=V.MShift `elem` mods
      move target=moveToText extend target
  in case key of
    V.KLeft->move (horizontalTextOffset False text pos)
    V.KRight->move (horizontalTextOffset True text pos)
    V.KUp->verticalMove (-1) extend d
    V.KDown->verticalMove 1 extend d
    V.KPageUp->pageMove False extend d
    V.KPageDown->pageMove True extend d
    V.KHome->if V.MCtrl `elem` mods then documentEdge False extend d else rowEdge False extend d
    V.KEnd->if V.MCtrl `elem` mods then documentEdge True extend d else rowEdge True extend d
    _->d

-- Map only the currently prepared rows when local visibility changes.
conversationScrollPoint :: Desktop -> Window -> BodyViewport -> Int -> Maybe BodyPoint
conversationScrollPoint d w viewport row=do
  prepared<-windowPluginText d w
  viewportPoint viewport (windowTextOffset d w (PluginWindow.preparedWindowText prepared) row 0) <|>
    listToMaybe [point | entry<-drop row (Vec.toList (viewportRows viewport)),Just point<-[bodyRowPoint entry]]

pluginMoveTo :: Bool -> Int -> Desktop -> Desktop
pluginMoveTo extend requested d | Just view<-activePluginWindow d, Just w<-activeWindow d =
  let text=PluginWindow.preparedWindowText view
      pos=max 0 (min (contentLength text) requested)
      (row,col)=windowTextPosition d w text pos
      update w=w {selection=Selection (if extend then anchor (selection w) else pos) pos,
      scrollRow=max 0 (min row (max (scrollRow w) (row-height (pluginTextRect d w)+1))),
      scrollColumn=max 0 (min col (max (scrollColumn w) (col-width (bounds w)+3)))}
      movedWindow=update w
      moved=modifyActive update d
  in case conversationTargetFor d w of
    Just target | Just viewport<-bodyViewportFor d target,Just point<-viewportPoint viewport pos->
      let original=do view<-M.lookup target (conversationViews d); BodySelection first _<-conversationReplySelection view; pure first
          chosen=BodySelection (if extend then fromMaybe point original else point) point
      in moved {conversationViews=M.adjust (\v->v {conversationReplySelection=Just chosen,conversationCaretIntent=Nothing,
        conversationScrollColumn=scrollColumn movedWindow,
        conversationAnchor=if scrollRow movedWindow/=scrollRow w then maybe (conversationAnchor v) At (conversationScrollPoint d w viewport (scrollRow movedWindow)) else conversationAnchor v}) target (conversationViews moved)}
    _->moved
pluginMoveTo _ _ d=d

-- | Ready Markdown content shares exactly the target used for paint and hits.
windowMarkdown :: Desktop -> Window -> Maybe (TextLayout.TextLayout,BufferContent,[(Int,Int,Text)])
windowMarkdown d w=do
  layout<-windowPresentation d w
  MarkdownWindowPresentation _ _ _ _ text links<-M.lookup (windowId w) (windowPresentations d)
  pure (layout,text,links)

markdownScrollLimit :: Desktop -> Bool -> Window -> Int
markdownScrollLimit d vertical w=case windowMarkdown d w of
  Nothing->0
  Just (layout,_,_)->max 0 (if vertical then Vec.length (TextLayout.layoutRows layout)-max 1 (height (bounds w)-2)
    else TextLayout.layoutWidth layout-max 1 (width (bounds w)-2))

clampMarkdownInteraction :: Desktop -> Window -> Window
clampMarkdownInteraction _ w | bufferView w/=MarkdownView=w
clampMarkdownInteraction d w=case windowMarkdown d w of
  Nothing->w
  Just (_,text,_)->case M.lookup (windowId w) (windowPresentations d) of
    Just prepared | (MarkdownPresentation _ version,columns,_)<-presentationMetadata prepared ->
      let stamp=Just (version,columns)
          currentStamp=case markdownInteraction w of Just (MarkdownInteraction _ _ _ old)->old; Nothing->Nothing
          shown=displayWindow w
          Selection a c=if currentStamp==stamp then selection shown else Selection 0 0
          end=contentLength text
      in w {markdownInteraction=Just (MarkdownInteraction (Selection (max 0 (min end a)) (max 0 (min end c)))
        (min (markdownScrollLimit d True w) (scrollRow shown)) (min (markdownScrollLimit d False w) (scrollColumn shown)) stamp)}
    _->w

markdownKey :: V.Key -> [V.Modifier] -> Desktop -> Desktop
markdownKey key mods d=case activeWindow d of
  Just original | Just (_,text,_)<-windowMarkdown d original->readOnlyTextKey text (displayWindow original) (\extend target->markdownMoveTo extend target d) key mods d
  _->d

markdownMoveTo :: Bool -> Int -> Desktop -> Desktop
markdownMoveTo extend requested d=case activeWindow d of
  Just original | Just (_,text,_)<-windowMarkdown d original ->
    let pos=max 0 (min (contentLength text) requested); (row,col)=windowTextPosition d original text pos
    in modifyActive (modifyDisplayedWindow (\w->w {selection=Selection (if extend then anchor (selection w) else pos) pos,
      scrollRow=max 0 (min row (max (scrollRow w) (row-height (bounds w)+3))),
      scrollColumn=max 0 (min col (max (scrollColumn w) (col-width (bounds w)+3)))})) d
  _->d

-- Measured line offsets avoid scanning preceding text on cursor movement.
contentPosition :: BufferContent -> Int -> (Int,Int)
contentPosition text pos=(row,sourceLineDisplayColumn (contentSourceLineAt text row) (pos-contentLineOffset text row))
  where
    row=findRow 0 (max 0 (contentLineCount text-1))
    findRow low high | low>=high=low
                     | contentLineOffset text middle<=pos=findRow middle high
                     | otherwise=findRow low (middle-1)
      where middle=(low+high+1) `div` 2

-- | Capture only identity/version metadata; styled payloads stay on the worker.
windowPresentationTarget :: Desktop -> Window -> Maybe PresentationTarget
windowPresentationTarget d w=case windowContent w of
  PluginContent reference->do
    prepared<-M.lookup reference (pluginWindows d)
    if PluginWindow.preparedWindowNeedsLayout True prepared then Just (PluginPresentation reference prepared) else Nothing
  SourceContent bid->do
    doc<-M.lookup bid (buffers d)
    if bufferView w==MarkdownView && markdownDocument doc then Just (MarkdownPresentation bid (revision (documentBuffer doc)))
    else if bufferView w/=CurrentView || byteMode (documentBuffer doc) || syntaxDocument doc || null (documentHighlight doc) ||
       not (documentLabel doc `elem` [Just "Haskell Help"] || documentMarkdownPath doc/=Nothing) then Nothing
    else Just (DocumentPresentation bid (revision (documentBuffer doc)))

-- | A layout is usable only for this exact payload, width and live preference.
-- Pending styled views use ordinary geometry; Markdown stays a read-only
-- placeholder until a matching derived presentation is adopted.
windowPresentation :: Desktop -> Window -> Maybe TextLayout.TextLayout
windowPresentation d w | Just target<-conversationTargetFor d w=do
  view<-M.lookup target (conversationViews d)
  InstalledBody reference (Just (BodyControlReceipt captured columns wide layout _))<-pure (conversationBody view)
  current<-M.lookup reference (pluginWindows d)
  if current==captured && columns==max 1 (width (bounds w)-2) && wide==wideSectionTitles d then layout else Nothing
windowPresentation d w=do
  prepared<-M.lookup (windowId w) (windowPresentations d)
  current<-windowPresentationTarget d w
  let (target,columns,wide)=presentationMetadata prepared
  if target==current && wide==wideSectionTitles d && columns==max 1 (width (bounds w)-2) then presentationLayout prepared else Nothing

-- Cached admission stays separate from payload identity, so preference changes
-- can still reproject the existing semantic viewport anchor.
windowPresentationNeeded :: Desktop -> Window -> Bool
windowPresentationNeeded d w=bufferView w==MarkdownView || wideSectionTitles d || case windowContent w of
  PluginContent reference->maybe False (PluginWindow.preparedWindowNeedsLayout False) (M.lookup reference (pluginWindows d))
  SourceContent bid->maybe False documentHasLayoutMetadata (M.lookup bid (buffers d))

windowTextPosition :: Desktop -> Window -> BufferContent -> Int -> (Int,Int)
windowTextPosition d w text pos=maybe (contentPosition text pos) (\layout->TextLayout.layoutPosition layout pos) (windowPresentation d w)

windowTextOffset :: Desktop -> Window -> BufferContent -> Int -> Int -> Int
windowTextOffset d w text row column=case windowPresentation d w of
  Just layout->TextLayout.layoutOffset layout row column
  Nothing->let line=max 0 (min (contentLineCount text-1) row)
           in contentLineOffset text line+sourceLineColumnOffset (contentSourceLineAt text line) column

-- | Preserve the semantic viewport top when prepared geometry adopts or a
-- preference disables it. Only measured source maps and scalar identities are
-- compared; manual browsing never becomes an implicit caret-follow operation.
reprojectWindowPresentations :: Desktop -> Desktop -> Desktop
reprojectWindowPresentations before after=after {windows=map reposition (windows after)}
  where
    reposition w
      | bufferView w==MarkdownView = clampMarkdownInteraction after w
      | Just previous<-find ((==windowId w).windowId) (windows before)
      , windowPresentationTarget before previous==windowPresentationTarget after w
      , windowPresentation before previous/=windowPresentation after w
      , Just text<-case windowContent w of
          SourceContent bid->bufferContent . documentBuffer <$> M.lookup bid (buffers after)
          PluginContent reference->PluginWindow.preparedWindowText <$> M.lookup reference (pluginWindows after)
      = let offset=windowTextOffset before previous text (scrollRow previous) (scrollColumn previous)
            (row,column)=windowTextPosition after w text offset
        in w {scrollRow=max 0 row,scrollColumn=max 0 column}
      | otherwise=w

windowTextRows :: Desktop -> Window -> BufferContent -> Int
windowTextRows d w text=maybe (contentLineCount text) (Vec.length . TextLayout.layoutRows) (windowPresentation d w)

windowCaretCell :: Desktop -> Document -> Window -> (Int,Int)
windowCaretCell d doc original=let w=displayWindow original in case windowPresentation d w of
  Just layout->TextLayout.layoutPosition layout (caret (selection w))
  Nothing->windowCursorCell (documentBuffer doc) w

-- Link eligibility needs only the demanded column, never an exact whole-row
-- width. A source seek reaches real EOF if the column lies past its contents.
windowTextContainsColumn :: Desktop -> Window -> BufferContent -> Int -> Int -> Bool
windowTextContainsColumn d w text row column=column>=0 && case windowPresentation d w of
  Just layout->column<maybe 0 TextLayout.layoutRowWidth (TextLayout.layoutRows layout Vec.!? row)
  Nothing->column<fst (sourceLineExtentThrough (contentSourceLineAt text row) column)

-- | Choose the current focused input owner using only small focus metadata.
-- Captured gestures, popups and human question/completion controls retain priority.
bindingPlatform :: Desktop -> Bindings.BindingPlatform
bindingPlatform d | nativeMac d = Bindings.MacPlatform
                  | videoMode d/=Nothing = Bindings.GraphicalPlatform
                  | otherwise = Bindings.TerminalPlatform

bindingContext :: Desktop -> Maybe Bindings.BindingContext
bindingContext d
  | dialog d/=Nothing = Just Bindings.DialogKeys
  | problemsFocused d = Just Bindings.MessagesKeys
  | maybe False treeFocused (sideTree d) = Just Bindings.SidebarKeys
  | composerActive d || activeConversation d = Just Bindings.ConversationKeys
  | activeTerminal d/=Nothing = Just Bindings.TerminalKeys
  | Just label<-activeDocument d >>= documentLabel,
    "Debugger " `T.isPrefixOf` label || "Source " `T.isPrefixOf` label = Just Bindings.DebuggerKeys
  | Just _<-activeDocument d = Just (if wordStar d && not (activeHex d) then fromMaybe Bindings.WordStarKeys (wordStarPrefixContext d) else Bindings.SourceKeys)
  | Nothing<-activeDocument d = Just Bindings.SourceKeys
  | otherwise = Nothing

-- Only the current ordinary WordStar source owner admits finite prefix steps.
wordStarPrefixOwner :: Desktop -> Bool
wordStarPrefixOwner d=wordStar d && not (activeHex d) && not (activeMarkdown d) &&
  dialog d==Nothing && not (questionActive d) && not (activeAutocomplete d) && not (problemsFocused d) &&
  not (maybe False treeFocused (sideTree d)) && not (activeConversation d) && activeTerminal d==Nothing &&
  maybe False (windowFocused d) (activeWindow d) && maybe False ((==Nothing) . documentLabel) (activeDocument d)

wordStarPrefixContext :: Desktop -> Maybe Bindings.BindingContext
wordStarPrefixContext d | wordStarPrefixOwner d = case prefix d of Just 'k'->Just Bindings.WordStarBlockKeys; Just 'q'->Just Bindings.WordStarQuickKeys; _->Nothing
                       | otherwise = Nothing

effectiveBindings :: Desktop -> Maybe (Bindings.Bindings Command)
effectiveBindings d = bindingContext d >>= \context->M.lookup (bindingPlatform d,context) (keyBindings d)

terminalContextReserved :: Desktop -> V.Key -> [V.Modifier] -> Bool
terminalContextReserved d key mods =
  (terminalSourceReserved key mods && not (bindingContext d==Just Bindings.DialogKeys &&
    (dialogControlChord key mods || dialogInputOwner d && key `elem` map fst dialogInputKeys))) ||
  (bindingContext d `elem` [Just Bindings.WordStarKeys,Just Bindings.WordStarBlockKeys,Just Bindings.WordStarQuickKeys] && wordStarReserved key mods) ||
  (bindingContext d==Just Bindings.DialogKeys && (dialogReserved key mods || windowCycleChord key mods)) ||
  (bindingContext d==Just Bindings.ConversationKeys && key==V.KEnter)

bindingInputAvailable :: Desktop -> Bool
bindingInputAvailable d=case dialog d of
  Just _ -> any (`dialogCommandAllowed` d) dialogBindingCommands
  Nothing -> menu d==Nothing && contextMenu d==Nothing && drag d==Nothing &&
    dragOriginal d==Nothing && (prefix d==Nothing || isJust (wordStarPrefixContext d)) && not (questionActive d) && not (activeAutocomplete d)

-- | Plain movement/text is handled by its owner. No removed chord falls through
-- into a hardcoded named command. PTY fallback retains every ordinary control key.
unboundKey :: V.Key -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
unboundKey key mods d
  | Just _<-effectiveBindings d, Just _<-wordStarPrefixContext d =
      (d {prefix=Nothing,status="Unknown WordStar prefix command."},[])
  | Just _<-effectiveBindings d,dialog d==Nothing,windowCycleChord key mods = (d,[])
  | Just _<-effectiveBindings d,dialog d/=Nothing,dialogControlChord key mods = (d,[])
  | Just _<-effectiveBindings d,dialogInputOwner d, key `elem` map fst dialogInputKeys,
    not (bindingPlatform d==Bindings.TerminalPlatform && V.MMeta `elem` mods) = (d,[])
  | Just _<-effectiveBindings d, sourceNavigationOwner d,
    not (bindingPlatform d==Bindings.TerminalPlatform && V.MMeta `elem` mods),
    key `elem` [V.KUp,V.KDown,V.KHome,V.KEnd,V.KPageUp,V.KPageDown,V.KLeft,V.KRight,V.KBS,V.KDel] = (d,[])
unboundKey key mods d | dialog d==Nothing, Just next<-imageKey key mods d = (next,[])
unboundKey key mods d | activeMarkdown d, dialog d==Nothing,maybe False (windowFocused d) (activeWindow d) = (markdownKey key mods d,[])
unboundKey key mods d | dialog d==Nothing,activeEditorMount d/=Nothing,maybe False (windowFocused d) (activeWindow d),
  Just result<-composerEvent (V.EvKey key mods) d = result
unboundKey key mods d | Just _<-activePluginWindow d, dialog d==Nothing,maybe False (windowFocused d) (activeWindow d) = (pluginKey key mods d,[])
unboundKey key mods d = case bindingContext d of
  Just Bindings.DialogKeys | Just dg<-dialog d -> dialogEvent (V.EvKey key mods) dg d
  Just Bindings.SidebarKeys -> (d,[])
  Just Bindings.MessagesKeys -> (d,[])
  Just Bindings.ConversationKeys -> fromMaybe (editorKey key mods d,[]) (composerEvent (V.EvKey key mods) d)
  Just Bindings.TerminalKeys -> (d,maybe [] (\text->[ServiceAction "terminal-input" [fromMaybe "" (activeTerminal d),text]]) (terminalInput (V.EvKey key mods)))
  Just Bindings.WordStarKeys | wordStarReserved key mods -> keyEvent key mods d
  _ -> (editorKey key mods d,[])

-- | The earlier Ctrl+Alt+X Quit owner remains outside compiled WordStar maps.
wordStarReserved :: V.Key -> [V.Modifier] -> Bool
wordStarReserved (V.KChar c) mods=V.MCtrl `elem` mods && V.MMeta `notElem` mods &&
  (toLower c=='x' && V.MAlt `elem` mods)
wordStarReserved _ _=False

-- | Control Tab aliases belong to window commands outside a modal. Alt and Meta
-- retain their earlier focus/platform owners.
windowCycleChord :: V.Key -> [V.Modifier] -> Bool
windowCycleChord key mods=key `elem` [V.KChar '\t',V.KBackTab] &&
  V.MCtrl `elem` mods && V.MAlt `notElem` mods && V.MMeta `notElem` mods

terminalSourceReserved :: V.Key -> [V.Modifier] -> Bool
terminalSourceReserved key mods=(key==V.KChar ']' && V.MCtrl `elem` mods) || key==V.KEsc || key==V.KFun 10 ||
  (key `elem` [V.KChar '\t',V.KBackTab] && not (windowCycleChord key mods)) ||
  (V.MAlt `elem` mods && V.MMeta `notElem` mods && case key of
    V.KChar c->c `elem` ['1'..'9'] || toLower c `elem` [mn | (_,mn,_)<-menus] || c `elem` ['\\','[',']']
    V.KRight->True
    _->False)

-- | Policy inspects exactly the same resolved action as human key dispatch.
boundKeyCommand :: V.Key -> [V.Modifier] -> Desktop -> Maybe Command
boundKeyCommand key mods d
  | bindingInputAvailable d, not (terminalContextReserved d key mods),
    Just cmd<-effectiveBindings d >>= \bindings->Bindings.bindingAction bindings key mods,
    dialog d==Nothing || dialogCommandAllowed cmd d = Just cmd
  | dialog d==Nothing, messagesDisplayed d, problemsFocused d = problemsKeyCommand key mods
  | otherwise = Nothing

keyEvent :: V.Key -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
keyEvent key mods d
  | key==V.KEsc = (d {prefix=Nothing},[])
  | wordStarPrefixOwner d, effectiveBindings d==Nothing, Just p <- prefix d, V.KChar c <- key, all (`elem` [V.MCtrl,V.MShift]) mods = starPrefix p (toLower c) d {prefix=Nothing}
  | V.MAlt `elem` mods, V.MMeta `notElem` mods, V.KChar c <- key, Just i <- findIndex (\(_,mn,_) -> mn==toLower c) menus = (d {menu=Just (i,0)},[])
  | V.MAlt `elem` mods, V.MMeta `notElem` mods, key==V.KChar 'x' = runCommand Quit d
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
  | not (activeMarkdown d), ctrl, wordStar d, not (activeHex d), V.KChar c <- key = starKey (toLower c) d
  | ctrl, V.KChar c <- key, Just cmd <- lookup (toLower c) [('b',ToggleTree),('s',Save),('o',Open),('n',New),('z',Undo),('y',Redo),('c',Copy),('x',Cut),('v',Paste),('a',SelectAll),('f',Find),('h',Replace),('r',Replace),('g',GoTo),('l',FindNext),('q',Quit)] = runCommand cmd d
  | otherwise = (editorKey key mods d,[])
  where ctrl = V.MCtrl `elem` mods

-- | Finite source operations resolve semantic positions, never replay key events.
sourceKeyCommand :: Command -> Bool
sourceKeyCommand CursorLeft{}=True
sourceKeyCommand CursorRight{}=True
sourceKeyCommand CursorUp{}=True
sourceKeyCommand CursorDown{}=True
sourceKeyCommand CursorRowStart{}=True
sourceKeyCommand CursorRowEnd{}=True
sourceKeyCommand CursorDocumentStart{}=True
sourceKeyCommand CursorDocumentEnd{}=True
sourceKeyCommand CursorPageUp{}=True
sourceKeyCommand CursorPageDown{}=True
sourceKeyCommand CursorWordLeft{}=True
sourceKeyCommand CursorWordRight{}=True
sourceKeyCommand MarkBlockStart=True
sourceKeyCommand MarkBlockEnd=True
sourceKeyCommand cmd=horizontalMutation cmd

horizontalMutation :: Command -> Bool
horizontalMutation cmd=cmd `elem` [DeleteBackward,DeleteForward,DeleteLine,DeleteSelection,DeleteWordBackward,DeleteWordForward]

-- A global remap cannot edit or move a source behind another focused input owner.
sourceNavigationOwner :: Desktop -> Bool
sourceNavigationOwner d=dialog d==Nothing && not (questionActive d) && not (activeAutocomplete d) &&
  (bindingContext d `elem` [Just Bindings.SourceKeys,Just Bindings.WordStarKeys,Just Bindings.WordStarBlockKeys,Just Bindings.WordStarQuickKeys,Just Bindings.DebuggerKeys] ||
    activeConversation d && not (composerActive d)) &&
  maybe False (windowFocused d) (activeWindow d)

-- | Preserve each window's existing displayed selection and source coordinate owner.
horizontalMove :: Bool -> Bool -> Desktop -> Desktop
horizontalMove forward extend d
  | Just w<-activeWindow d,Just (_,_,_,False)<-windowRows d w=moveWindowRow (if forward then 1 else -1) d
  | activeMarkdown d = case activeWindow d of
      Just w | Just (_,text,_)<-windowMarkdown d w ->
        markdownMoveTo extend (horizontalTextOffset forward text (caret (selection (displayWindow w)))) d
      _->d
  | Just view<-activePluginWindow d, Just w<-activeWindow d =
      pluginMoveTo extend (horizontalTextOffset forward (PluginWindow.preparedWindowText view) (caret (selection w))) d
  | Just w<-activeWindow d, Just doc<-activeDocument d =
      let b=documentBuffer doc; p=caret (selection w)
          next | byteMode b=p+if forward then 1 else -1
               | forward=bufferNextCharacter b p
               | otherwise=bufferPreviousCharacter b p
      in moveTo extend next d
  | otherwise=d

-- | Return a source character offset at the existing grapheme/line boundary.
horizontalTextOffset :: Bool -> BufferContent -> Int -> Int
horizontalTextOffset forward text pos
  | forward, pos>=start+T.length line=min (contentLength text) (pos+1)
  | forward=start+nextCharacter line (pos-start)
  | pos==start=max 0 (pos-1)
  | otherwise=start+previousCharacter line (pos-start)
  where row=fst (contentPosition text pos); start=contentLineOffset text row; line=contentLineAt text row

-- | Preserve each owner's existing row geometry, selection and visibility.
verticalMove :: Int -> Bool -> Desktop -> Desktop
verticalMove delta extend d
  | Just w<-activeWindow d,Just (_,_,_,False)<-windowRows d w=moveWindowRow delta d
  | activeMarkdown d = case activeWindow d of
      Just w | Just (_,text,_)<-windowMarkdown d w ->
        markdownMoveTo extend (verticalTextOffset d (displayWindow w) text (caret (selection (displayWindow w))) delta) d
      _->d
  | Just view<-activePluginWindow d,Just w<-activeWindow d,Just target<-conversationTargetFor d w,
    Just captured<-bodyViewportFor d target,Just retained<-M.lookup target (conversationViews d),
    let text=PluginWindow.preparedWindowText view,
    let (row,column)=windowTextPosition d w text (caret (selection w)),
    row+delta<scrollRow w || row+delta>=scrollRow w+max 1 (pluginBodyRows d w),
    Just point<-viewportPoint captured (caret (selection w))=
      let (shift,previousAnchor)=case conversationCaretIntent retained of
            Just (RowCaret _ original) | conversationAnchor retained==At point->(conversationRowShift retained+delta,original)
            _->(delta,Nothing)
          extended=if extend then previousAnchor <|> (case conversationReplySelection retained of Just (BodySelection first _)->Just first; _->Just point) else Nothing
          requested=retained {conversationAnchor=At point,conversationRowShift=shift,conversationCaretIntent=Just (RowCaret column extended)}
      in d {conversationViews=M.insert target requested (conversationViews d)}
  | Just view<-activePluginWindow d, Just w<-activeWindow d =
      pluginMoveTo extend (verticalTextOffset d w (PluginWindow.preparedWindowText view) (caret (selection w)) delta) d
  | Just w<-activeWindow d, Just doc<-activeDocument d =
      let b=documentBuffer doc; text=bufferContent b; p=caret (selection w)
          (row,col)=bufferLineColumn b p
          next | byteMode b=p+delta*windowHexBytes w
               | Just layout<-windowPresentation d w =
                   let (r,c)=TextLayout.layoutPosition layout p
                   in TextLayout.layoutOffset layout (max 0 (min (Vec.length (TextLayout.layoutRows layout)-1) (r+delta))) c
               | otherwise=let r=max 0 (min (bufferLineCount b-1) (row+delta))
                   in bufferLineOffset b r+sourceLineColumnOffset (contentSourceLineAt text r)
                     (sourceLineDisplayColumn (contentSourceLineAt text row) col)
      in moveTo extend next d
  | otherwise=d

-- | Resolve a displayed row using the same prepared layout as paint and hits.
verticalTextOffset :: Desktop -> Window -> BufferContent -> Int -> Int -> Int
verticalTextOffset d w text pos delta=windowTextOffset d w text next col
  where
    (row,col)=windowTextPosition d w text pos
    next=max 0 (min (windowTextRows d w text-1) (row+delta))

-- | Resolve row edges in the current displayed geometry; hex End keeps its last-byte convention.
rowEdge :: Bool -> Bool -> Desktop -> Desktop
rowEdge end extend d
  | Just w<-activeWindow d,Just (rows,_,_,False)<-windowRows d w=selectWindowRow (if end then Vec.length rows-1 else 0) d
  | activeMarkdown d = case activeWindow d of
      Just w | Just (_,text,_)<-windowMarkdown d w ->
        markdownMoveTo extend (textEdge (displayWindow w) text) d
      _->d
  | Just view<-activePluginWindow d, Just w<-activeWindow d =
      pluginMoveTo extend (textEdge w (PluginWindow.preparedWindowText view)) d
  | Just w<-activeWindow d, Just doc<-activeDocument d =
      let b=documentBuffer doc; p=caret (selection w); row=fst (bufferLineColumn b p)
          start=bufferLineOffset b row
          target | byteMode b=let start=p-p `mod` windowHexBytes w
                             in if end then min (bufferLength b) (start+windowHexBytes w-1) else start
                 | Just layout<-windowPresentation d w =
                     TextLayout.layoutOffset layout (fst (TextLayout.layoutPosition layout p)) column
                 | end=start+sourceLineLength (contentSourceLineAt (bufferContent b) row)
                 | otherwise=start
      in moveTo extend target d
  | otherwise=d
  where
    column=if end then maxBound else 0
    textEdge w text=windowTextOffset d w text (fst (windowTextPosition d w text (caret (selection w)))) column

-- | Document edges use measured lengths in their owning source or prepared text.
documentEdge :: Bool -> Bool -> Desktop -> Desktop
documentEdge end extend d
  | activeConversation d=conversationEdge end extend d
  | Just w<-activeWindow d,Just (rows,_,_,False)<-windowRows d w=selectWindowRow (if end then Vec.length rows-1 else 0) d
  | activeMarkdown d = case activeWindow d of
      Just w | Just (_,text,_)<-windowMarkdown d w ->markdownMoveTo extend (edge (contentLength text)) d
      _->d
  | Just view<-activePluginWindow d =pluginMoveTo extend (edge (contentLength (PluginWindow.preparedWindowText view))) d
  | Just doc<-activeDocument d =moveTo extend (edge (bufferLength (documentBuffer doc))) d
  | otherwise=d
  where edge size=if end then size else 0

-- | Source/hex pages reserve three chrome rows; prepared read-only text reserves two.
pageMove :: Bool -> Bool -> Desktop -> Desktop
pageMove forward extend d=verticalMove (if forward then page else negate page) extend d
  where page=maybe 10 (\w->case windowRows d w of Just (_,_,_,details)->max 1 (height (if details then snd (rowsWindowRects d w) else fst (rowsWindowRects d w))); _->max 1 (height (bounds w)-if activeMarkdown d || isJust (activePluginWindow d) then 2 else 3)) (activeWindow d)

-- | Use measured scalar word boundaries in source; preserve byte/grapheme steps in other views.
wordMove :: Bool -> Bool -> Desktop -> Desktop
wordMove forward extend d
  | activeMarkdown d || isJust (activePluginWindow d) || activeHex d=horizontalMove forward extend d
  | Just w<-activeWindow d,Just doc<-activeDocument d =
      let b=documentBuffer doc; p=caret (selection w)
      in moveTo extend (if forward then bufferWordRight b p else bufferWordLeft b p) d
  | otherwise=d

-- | Delete the selected range first, otherwise one measured source word or hex byte.
deleteWord :: Bool -> Desktop -> Desktop
deleteWord forward d
  | activeHex d=deleteAdjacent forward d
  | Just w<-activeWindow d,Just doc<-activeDocument d =
      let b=documentBuffer doc; sel=selection w; p=caret sel
          target=if forward then bufferWordRight b p else bufferWordLeft b p
          range=if anchor sel/=p then sel else Selection p target
      in editActive (\_ -> replaceSelection range "") (Just (fst (ordered range))) d
  | otherwise=d

-- | Delete a selected range first, otherwise one existing character (or hex byte).
deleteAdjacent :: Bool -> Desktop -> Desktop
deleteAdjacent forward d=case (activeWindow d,activeDocument d) of
  (Just w,Just doc)->let
      b=documentBuffer doc; sel=selection w; p=caret sel
      previous=if byteMode b then max 0 (p-1) else bufferPreviousCharacter b p
      next=if byteMode b then min (bufferLength b) (p+1) else bufferNextCharacter b p
      range=if anchor sel/=p then sel else if forward then Selection p next else Selection previous p
    in editActive (\_ -> replaceSelection range "") (Just (fst (ordered range))) d
  _->d

-- | WordStar line deletion keeps its measured range and single Undo owner.
deleteSourceLine :: Desktop -> Desktop
deleteSourceLine d=case (activeWindow d,activeDocument d) of
  (Just w,Just doc)->let
      b=documentBuffer doc; row=fst (bufferLineColumn b (caret (selection w)))
      start=bufferLineOffset b row; end=bufferLineOffset b (row+1)
    in editActive (\_ -> replaceSelection (Selection start end) "") (Just start) d
  _->d

editorKey :: V.Key -> [V.Modifier] -> Desktop -> Desktop
editorKey key mods d | activeMarkdown d=markdownKey key mods d
editorKey key mods d | activeHex d = hexKey key mods d
editorKey key mods d = case key of
  V.KLeft | ctrl -> wordMove False shift d
          | otherwise -> horizontalMove False shift d
  V.KRight | ctrl -> wordMove True shift d
           | otherwise -> horizontalMove True shift d
  V.KUp -> verticalMove (-1) shift d
  V.KDown -> verticalMove 1 shift d
  V.KPageUp -> pageMove False shift d
  V.KPageDown -> pageMove True shift d
  V.KHome -> if ctrl then documentEdge False shift d else rowEdge False shift d
  V.KEnd -> if ctrl then documentEdge True shift d else rowEdge True shift d
  V.KBS | ctrl -> deleteWord False d
        | otherwise -> deleteAdjacent False d
  V.KDel | ctrl -> deleteWord True d
         | otherwise -> deleteAdjacent True d
  V.KEnter -> insertText (bufferNewline b) d
  V.KChar '\t' -> insertText "  " d
  V.KChar c | null mods || mods==[V.MShift], textInputChar c -> insertText (T.singleton c) d
  _ -> d
  where
    b = maybe (newBuffer "") documentBuffer (activeDocument d)
    ctrl = V.MCtrl `elem` mods; shift = V.MShift `elem` mods

starKey :: Char -> Desktop -> (Desktop,[Effect])
starKey c d = case c of
    'e' -> runCommand (CursorUp False) d
    'x' -> runCommand (CursorDown False) d
    's' -> runCommand (CursorLeft False) d
    'd' -> runCommand (CursorRight False) d
    'k' -> (d {prefix=Just 'k'},[])
    'q' -> (d {prefix=Just 'q'},[])
    'a' -> (wordMove False False d,[])
    'f' -> (wordMove True False d,[])
    'y' -> runCommand DeleteLine d
    'z' -> runCommand Undo d
    _ -> (d,[])

starPrefix :: Char -> Char -> Desktop -> (Desktop,[Effect])
starPrefix 'k' c d = case c of
  'b' -> runCommand MarkBlockStart d
  'k' -> runCommand MarkBlockEnd d
  'c' -> runCommand Copy d
  'v' -> runCommand Cut d
  'y' -> runCommand DeleteSelection d
  's' -> runCommand Save d
  'd' -> runCommand Close d
  _ -> (d {status="Unknown Ctrl+K command."},[])
starPrefix 'q' c d = case c of
  's' -> (rowEdge False False d,[])
  'd' -> (rowEdge True False d,[])
  'r' -> (documentEdge False False d,[])
  'c' -> (documentEdge True False d,[])
  'f' -> runCommand Find d
  'a' -> runCommand Replace d
  _ -> (d {status="Unknown Ctrl+Q command."},[])
starPrefix _ _ d = (d,[])

-- Preview is separate from the committed choice so Escape is lossless.
openComboBox :: Dialog -> Maybe (Int,Text,[Text],Int,Int)
openComboBox dg = listToMaybe [(i,name,choices,chosen,preview) | (i,ComboBox name choices chosen (Just preview))<-zip [0..] (fields dg)]

comboBoxRect :: Desktop -> Dialog -> Int -> [Text] -> Rect
comboBoxRect d dg i choices = Rect x popupY w popupHeight
  where
    Rect x y w _=fieldRects d dg !! i
    popupHeight=length choices+2
    popupY=if y+2+popupHeight<=snd (screenSize d)-1 then y+2 else max 1 (y-popupHeight)

comboBoxEvent :: V.Event -> Dialog -> Desktop -> Int -> Text -> [Text] -> Int -> Int -> (Desktop,[Effect])
comboBoxEvent ev dg d i name choices chosen preview = case ev of
  V.EvKey V.KEsc _ -> finish chosen
  V.EvKey V.KEnter _ -> finish preview
  V.EvKey (V.KChar ' ') _ -> finish preview
  V.EvKey (V.KChar '\t') mods -> advance (if V.MShift `elem` mods then -1 else 1)
  V.EvKey V.KBackTab _ -> advance (-1)
  V.EvKey V.KUp _ -> highlight (preview-1)
  V.EvKey V.KDown _ -> highlight (preview+1)
  V.EvKey V.KHome _ -> highlight 0
  V.EvKey V.KEnd _ -> highlight (length choices-1)
  V.EvMouseDown x y V.BLeft _ -> finish (if inside rect x y && y>top rect && y<top rect+height rect-1 then y-top rect-1 else chosen)
  V.EvMouseDown _ _ V.BScrollUp _ -> highlight (preview-1)
  V.EvMouseDown _ _ V.BScrollDown _ -> highlight (preview+1)
  _ -> (d,[])
  where
    rect=comboBoxRect d dg i choices
    update selection opened=(d {dialog=Just dg {fields=replaceAt i (ComboBox name choices selection opened) (fields dg)},buttonHover=Nothing,buttonPressed=Nothing},[])
    finish selection=update selection Nothing
    highlight selection=update chosen (Just (max 0 (min (length choices-1) selection)))
    advance delta=moveDialogFocus delta d

dialogEvent :: V.Event -> Dialog -> Desktop -> (Desktop,[Effect])
dialogEvent ev dg d
  | not prepared, nativeMac d, V.EvKey (V.KChar 'f') mods<-ev, V.MMeta `elem` mods, V.MAlt `elem` mods,
    dialogCommandAllowed Replace d = runCommand Replace d
  | not prepared, V.EvKey (V.KChar c) mods<-platformEvent, V.MCtrl `elem` mods,
    SelectedInput{}:_<-drop (focus dg) (fields dg), Just command<-lookup (toLower c) [('a',SelectAll),('c',Copy),('x',Cut),('v',Paste)] = applyDialogCommand command d
  | Just (i,name,choices,chosen,preview)<-openComboBox dg = comboBoxEvent platformEvent dg d i name choices chosen preview
  | otherwise = case platformEvent of
  V.EvKey key mods | DebugDialog action<-purpose dg,"hdb-accept:" `T.isPrefixOf` action,
    key==V.KEsc || key==V.KFun 3 && V.MAlt `elem` mods -> cancelDialog d
  V.EvKey (V.KChar c) mods | not prepared,searching dg,V.MCtrl `elem` mods,toLower c `elem` ['f','h','r'] -> runCommand (if toLower c=='f' then Find else Replace) d
  V.EvKey (V.KChar '\t') mods | Searching mode _<-purpose dg,V.MCtrl `elem` mods -> (searchPrompt (not mode) d,[])
  V.EvMouseDown x y V.BLeft _ | searching dg,Just (_,mode)<-find (\(r,_)->inside r x y) (searchTabRects d dg) -> (searchPrompt mode d,[])
  V.EvKey (V.KFun 3) mods | V.MAlt `elem` mods, PermissionDialog{}<-purpose dg -> dialogEvent (V.EvKey V.KEsc []) dg d
  V.EvKey V.KEsc _ -> cancelDialog d
  V.EvKey (V.KChar c) mods | not commandModifier, (V.MCtrl `elem` mods && not areaFocused && not (approvalDialog dg)) || V.MAlt `elem` mods,
    Just i<-findIndex (==Just (toLower c)) (buttonMnemonics dg) -> submitDialog i dg d
  V.EvKey (V.KChar '\t') mods -> setFocus (focus dg + if V.MShift `elem` mods then -1 else 1)
  V.EvKey V.KBackTab _ -> setFocus (focus dg-1)
  V.EvKey V.KEnter _ -> acceptDialog d
  V.EvKey key mods | areaFocused -> areaKey key mods
  V.EvKey key mods | ComboBox name choices chosen Nothing:_<-drop (focus dg) (fields dg),
    key==V.KChar ' ' || key==V.KDown && V.MAlt `elem` mods -> updateField (const (ComboBox name choices chosen (Just chosen)))
  V.EvKey k mods | focus dg<count -> updateField (fieldKey k mods)
  V.EvKey (V.KChar ' ') _ -> submitDialog (focus dg-count) dg d
  V.EvKey V.KLeft _ -> setFocus (focus dg-1)
  V.EvKey V.KRight _ -> setFocus (focus dg+1)
  V.EvPaste bytes | areaFocused -> case TE.decodeUtf8' bytes of
    Right text -> updateField (textAreaEdit focusedRect (insertText (T.filter (\c -> textInputChar c || c=='\n' || c=='\r' || c=='\t') text)))
    Left _ -> (d,[])
  V.EvPaste bytes | focus dg<count -> case TE.decodeUtf8' bytes of
    Right text -> updateField (\f -> case f of Input label value pos -> let clean=T.filter textInputChar text in Input label (T.take pos value<>clean<>T.drop pos value) (pos+T.length clean); SelectedInput{} -> replaceInputSelection (T.filter textInputChar text) f; _ -> f)
    Left _ -> (d,[])
  V.EvMouseDown x y V.BLeft _ | approvalDialog dg, inside (dialogCloseRect d dg) x y -> submitDialog 1 dg d
  V.EvMouseDown x y V.BLeft _ -> case findIndex (\r -> inside r x y) (buttonRects d dg) of
    Just i -> (d {buttonHover=Just i,buttonPressed=Just i},[])
    Nothing -> case findIndex (\r -> inside r x y && y >= top (dialogRect d dg)+2 && y < top (dialogRect d dg)+height (dialogRect d dg)-3) (fieldRects d dg) of
      Nothing -> (d,[])
      Just i -> let Rect l t _ _ = fieldRects d dg !! i
                    click (FileList xs selected) = FileList xs (max 0 (min (length xs-1) ((max 0 selected `div` 16)*16+max 0 (y-t-2)+if x-l >= width (fieldRects d dg !! i) `div` 2 then 8 else 0)))
                    click (Input label value pos) = Input label value (columnOffset value (max 0 (x-l)+max 0 (displayColumn value pos-width (fieldRects d dg !! i)+1)))
                    click (SelectedInput label value sel) = let pos=columnOffset value (max 0 (x-l)+max 0 (displayColumn value (caret sel)-width (fieldRects d dg !! i)+1)) in SelectedInput label value (Selection pos pos)
                    click (ComboBox name choices chosen _) = ComboBox name choices chosen (Just chosen)
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
    commandModifier=case ev of V.EvKey _ mods->V.MMeta `elem` mods; _->False
    prepared=M.member (bindingPlatform d,Bindings.DialogKeys) (keyBindings d)
    platformEvent=case ev of
      V.EvKey key mods | nativeMac d -> V.EvKey key [if modifier==V.MMeta then V.MCtrl else modifier | modifier<-mods]
      _ -> ev
    count=length (fields dg)
    areaFocused=case drop (focus dg) (fields dg) of TextArea{}:_ -> True; _ -> False
    focusedRect=fromMaybe (Rect 0 0 1 1) (listToMaybe (drop (focus dg) (fieldRects d dg)))
    areaKey (V.KChar c) mods | not prepared, V.MCtrl `elem` mods, c `elem` ['c','x','v'], f@(TextArea _ True b sel _ _)<-fields dg !! focus dg =
      let copied=selectedText sel b
          edited=case c of 'x' -> textAreaEdit focusedRect (insertText "") f; 'v' -> textAreaEdit focusedRect (insertText (clipboard d)) f; _ -> f
          updated=if c=='v' then d else copyClipboard False copied d
      in (updated {dialog=Just dg {fields=replaceAt (focus dg) edited (fields dg)}},[])
    areaKey key mods = updateField $ \f -> if editableArea f
      then textAreaEdit focusedRect (case key of
        V.KChar c | not prepared, V.MCtrl `elem` mods, Just cmd<-lookup (toLower c) [('z',if V.MShift `elem` mods then Redo else Undo),('y',Redo),('a',SelectAll)] -> fst . runCommand cmd
        _ -> editorKey key mods) f
      else clampArea focusedRect (fieldKey key mods f)
    clampArea rect f@(TextArea name editable b sel row col) = TextArea name editable b sel (min row (max 0 (bufferLineCount b-height (textAreaRect rect f)))) col
    clampArea _ f = f
    wheel x y delta = case findIndex (\r -> inside r x y && y<top (dialogRect d dg)+height (dialogRect d dg)-3) (fieldRects d dg) of
      Just i | f@TextArea{}<-fields dg !! i -> updateDialog dg {focus=i,fields=replaceAt i (clampArea (fieldRects d dg !! i) (scrollTextArea delta f)) (fields dg)}
      _ -> updateField (fieldKey (if delta>0 then V.KDown else V.KUp) [])
    setFocus i = moveDialogFocus (i-focus dg) d
    updateDialog new = (d {dialog=Just new},[])
    updateField f | focus dg<count = updateDialog (replaceDialogField (f (fields dg !! focus dg)) dg)
                  | otherwise = (d,[])

-- | Changed entry text invalidates the file dialog's old chosen filename.
-- Caret movement alone preserves it, through both raw and configured editing.
replaceDialogField :: Field -> Dialog -> Dialog
replaceDialogField changed dg=case drop (focus dg) (fields dg) of
  old:_ -> let updated=replaceAt (focus dg) changed (fields dg)
               clear (FileList entries _) = FileList entries (-1)
               clear field = field
               typed=case (old,changed) of (Input _ a _,Input _ b _) -> a/=b; (SelectedInput _ a _,SelectedInput _ b _) -> a/=b; _ -> False
           in dg {fields=if typed then map clear updated else updated}
  _ -> dg

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i+1) xs

scrollTextArea :: Int -> Field -> Field
scrollTextArea delta (TextArea name editable b sel row col) = TextArea name editable b sel (max 0 (min (bufferLineCount b-1) (row+delta))) col
scrollTextArea _ f = f

-- | Replace the selected single-line range. Ordinary Input editing keeps its
-- existing caret-only contract; this field is reusable by selected-value dialogs.
replaceInputSelection :: Text -> Field -> Field
replaceInputSelection text (SelectedInput caption value sel)=
  let (a,z)=ordered sel
      pos=a+T.length text
  in SelectedInput caption (T.take a value<>text<>T.drop z value) (Selection pos pos)
replaceInputSelection _ field=field

-- | Apply the six existing caret-only operations directly, without key replay.
-- Source scalar positions and complete grapheme deletion match raw Input editing.
inputCommand :: Command -> Field -> Field
inputCommand cmd field@(Input label value pos)=
  let set s p=Input label s (max 0 (min (T.length s) p))
  in case cmd of
    CursorLeft False -> set value (previousCharacter value pos)
    CursorRight False -> set value (nextCharacter value pos)
    CursorRowStart False -> set value 0
    CursorRowEnd False -> set value (T.length value)
    DeleteBackward -> let p=previousCharacter value pos in set (T.take p value<>T.drop pos value) p
    DeleteForward -> set (T.take pos value<>T.drop (nextCharacter value pos) value) pos
    _ -> field
inputCommand _ field=field

fieldKey :: V.Key -> [V.Modifier] -> Field -> Field
fieldKey key mods field = case field of
  Input{} | Just cmd<-lookup key dialogInputKeys -> inputCommand cmd field
  Input label value pos -> let set s p = Input label s (max 0 (min (T.length s) p)) in case key of
    V.KChar 'u' | V.MCtrl `elem` mods -> set "" 0
    V.KChar c | (null mods || mods==[V.MShift]) && textInputChar c -> set (T.take pos value<>T.singleton c<>T.drop pos value) (pos+1)
    _ -> field
  SelectedInput label value sel ->
    let pos=caret sel
        (a,z)=ordered sel
        move target=SelectedInput label value (Selection (if V.MShift `elem` mods then anchor sel else target) target)
        erase=replaceInputSelection "" field
    in case key of
      V.KLeft -> move (if V.MShift `notElem` mods && a/=z then a else previousCharacter value pos)
      V.KRight -> move (if V.MShift `notElem` mods && a/=z then z else nextCharacter value pos)
      V.KHome -> move 0
      V.KEnd -> move (T.length value)
      V.KBS | a/=z -> erase
            | otherwise -> replaceInputSelection "" (SelectedInput label value (Selection (previousCharacter value pos) pos))
      V.KDel | a/=z -> erase
             | otherwise -> replaceInputSelection "" (SelectedInput label value (Selection pos (nextCharacter value pos)))
      V.KChar 'u' | V.MCtrl `elem` mods -> SelectedInput label "" (Selection 0 0)
      V.KChar c | (null mods || mods==[V.MShift]) && textInputChar c -> replaceInputSelection (T.singleton c) field
      _ -> field
  TextArea name editable b sel row col -> case key of
    V.KLeft -> TextArea name editable b sel row (max 0 (col-1))
    V.KRight -> TextArea name editable b sel row (min (bufferLength b) (col+1))
    V.KHome -> TextArea name editable b sel 0 0
    V.KEnd -> TextArea name editable b sel (max 0 (bufferLineCount b-1)) col
    _ -> scrollTextArea (case key of V.KUp -> -1; V.KDown -> 1; V.KPageUp -> -8; V.KPageDown -> 8; _ -> 0) field
  CheckBox label value | key==V.KChar ' ' -> CheckBox label (not value)
  Radio label values chosen -> Radio label values (step values chosen)
  FileList values chosen -> FileList values (max 0 (min (length values-1) (case key of V.KLeft -> chosen-8; V.KRight -> chosen+8; V.KPageUp -> chosen-16; V.KPageDown -> chosen+16; V.KHome -> 0; V.KEnd -> length values-1; _ -> step values chosen)))
  ListBox label values chosen -> ListBox label values (choose values chosen)
  _ -> field
  where choose xs n = listChoice key (length xs) n
        step xs n=max 0 (min (length xs-1) (n+case key of V.KUp -> -1; V.KDown -> 1; V.KLeft -> -1; V.KRight -> 1; V.KChar ' ' -> 1; _ -> 0))

-- | Shared bounded chosen-row navigation for modal lists and fixed row windows.
listChoice :: V.Key -> Int -> Int -> Int
listChoice key count chosen=max 0 (min (count-1) (case key of
  V.KHome->0; V.KEnd->count-1; V.KPageUp->chosen-8; V.KPageDown->chosen+8
  V.KUp->chosen-1; V.KDown->chosen+1; V.KLeft->chosen-1; V.KRight->chosen+1; V.KChar ' '->chosen+1; _->chosen))

submitDialog :: Int -> Dialog -> Desktop -> (Desktop,[Effect])
submitDialog button dg original
  | button<0 || button>=length (buttons dg) = (original,[])
  | PluginInputForm ref<-purpose dg = if button==0 then (original,[SubmitInputForm ref (Form.TextValue first) Plugin.HumanMenu]) else (d,[RetireInputForm ref])
  | PluginInputsForm ref ids<-purpose dg = if button/=0 then (d,[RetireInputForm ref]) else
      case traverse namedInput (fields dg) of
        Just inputs | length inputs==length ids->(original,[SubmitInputForm ref (Form.InputValues (M.fromList (zip ids inputs))) Plugin.HumanMenu])
        _->(original {status="Input form fields do not match its schema."},[])
  | PluginChoiceForm ref version<-purpose dg = if button==0 then (original,[SubmitChoiceForm ref version selected Plugin.HumanMenu]) else (d,[RetireInputForm ref])
  | buttons dg !! button == "Cancel" = (d,[])
  | otherwise = case purpose dg of
    Opening base pattern entries
      | focus dg `elem` [1,length (fields dg)], FileList _ selectedFile:_ <- drop 1 (fields dg), selectedFile>=0, entry:_ <- drop selectedFile entries ->
          if entryDirectory entry then (original,[BrowsePath (base </> T.unpack (entryName entry)) pattern]) else (d,[OpenFile Plugin.HumanMenu (base </> T.unpack (entryName entry))])
      | otherwise -> (original,[OpenChoice Plugin.HumanMenu base first pattern])
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
      (Just w,Just doc,Completion _ edits:_) | bufferId w==Just (bid), revision (documentBuffer doc)==version, caret (selection w)==pos ->
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
    DebuggerWatchDialog ident _ _ -> (d,[DebugAction ("watch-edit:"<>T.pack (show ident)) (T.pack (show button):values)])
    DebugSourceWatchDialog ident _ _ _ -> (d,[DebugAction ("source-watch:"<>T.pack (show ident)) (T.pack (show button):values)])
    DebugDialog action -> (d,[DebugAction action (T.pack (show button) : values ++
      [if value then "true" else "false" | CheckBox _ value <- fields dg] ++
      [T.pack (show i) | ListBox _ _ i <- fields dg])])
    PermissionDialog action -> (original,[PermissionAction action (T.pack (show button) : values ++
      [contents b | TextArea _ True b _ _ _ <- fields dg] ++
      [T.pack (show i) | Radio _ _ i <- fields dg] ++
      [T.pack (show i) | ListBox _ _ i <- fields dg])])
    EnvironmentDialog action -> (d,[EnvironmentAction action (T.pack (show button):values++[if value then "true" else "false" | CheckBox _ value<-fields dg]++concat [take 1 (drop i choices) | ListBox _ choices i<-fields dg])])
    ServiceDialog action -> (d,[ServiceAction action (T.pack (show button) : values ++
      [if value then "true" else "false" | CheckBox _ value <- fields dg] ++
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
    DiscardDraft | button==0 -> runCommand Quit d {editorDrafts=M.map (\draft->draft {editorDraftBuffer=newBuffer "",editorDraftSelection=Selection 0 0}) (editorDrafts d)}
                 | otherwise -> (d,[])
    Confirm cmd | button==0 -> saveRequest (Just cmd) d
                | button==1 -> case cmd of
                    Close -> case activeWindow d of
                      Just w -> (closeActive d,closingEditors d w)
                      Nothing -> (d,[])
                    Quit -> runCommand Quit (discardActive d)
                    _ -> (d,[])
                | otherwise -> (d,[])
    AutocompleteDialog action -> (d,[AutocompleteAction action (T.pack (show button):values++[if enabled then "true" else "false" | CheckBox _ enabled<-fields dg])])
    ChatInputSettings -> let chosen=if any (\f -> case f of Radio "Enter action" _ 1 -> True; _ -> False) (fields dg) then SteerSubmit else QuerySubmit
                         in (d {chatSubmit=chosen},[SaveChatSubmit chosen])
    Settings -> (reprojectWindowPresentations d d {wideSectionTitles=fromMaybe (wideSectionTitles d) (listToMaybe [value | CheckBox "Wide section titles" value<-fields dg]),macKeySymbols=fromMaybe (macKeySymbols d) (listToMaybe [value | CheckBox "Mac key symbols" value<-fields dg]),wordStar=any (\f -> case f of Radio "Key bindings" _ 1 -> True; _ -> False) (fields dg),
      appearance=fromMaybe (appearance d) (listToMaybe [toEnum (max 0 (min 2 value)) | Radio "Appearance" _ value<-fields dg]),
      streamerMode=fromMaybe (streamerMode d) (listToMaybe [value | CheckBox "Streamer mode" value<-fields dg]),
      blinkCursor=fromMaybe (blinkCursor d) (listToMaybe [value | CheckBox "Blinking cursor" value<-fields dg]),
      pixelateUnicode=fromMaybe (pixelateUnicode d) (listToMaybe [value | CheckBox "Pixelate Unicode" value<-fields dg]),
      crtFilter=fromMaybe (crtFilter d) (listToMaybe [value | CheckBox "CRT filter" value<-fields dg]),status="Preferences updated."},
      [SaveWideSectionTitles value | CheckBox "Wide section titles" value<-fields dg,value/=wideSectionTitles d] ++
      [SaveMacKeySymbols value | CheckBox "Mac key symbols" value<-fields dg,value/=macKeySymbols d] ++
      [SetScreenMode mode | Radio "Screen size" _ chosen <- fields dg,
       let mode = if chosen == 1 then 259 else 3, Just mode /= videoMode d])
    Widgets -> (d {status="Dialog test complete."},[])
    Information -> (d,[])
  where
    d=original {dialog=Nothing,buttonHover=Nothing,buttonPressed=Nothing}
    selected=fromMaybe 0 (listToMaybe [i | ListBox _ _ i <- fields dg])
    namedInput (Input _ text _)=Just text
    namedInput (SelectedInput _ text _)=Just text
    namedInput _=Nothing
    values=concatMap fieldValue (fields dg)
    fieldValue (Input _ value _)=[value]
    fieldValue (SelectedInput _ value _)=[value]
    fieldValue (ComboBox _ choices chosen _)=take 1 (drop chosen choices)
    fieldValue _=[]
    first=fromMaybe "" (listToMaybe values); second=fromMaybe "" (listToMaybe (drop 1 values))
    discardActive s = case activeWindow s of
      Nothing -> s
      Just w | Just bid<-bufferId w -> s {windows=filter ((/=Just bid) . bufferId) (windows s), buffers=M.delete bid (buffers s)}
      _ -> s


startingDirectory :: Desktop -> FilePath
startingDirectory d = fromMaybe (maybe (maybe "." treeRoot (sideTree d)) (takeDirectory . filePath) (activeDocument d >>= documentFile)) (defaultDirectory d)

treeWidthOf :: Desktop -> Int
-- The dock's right frame is also the editor area's left frame.
treeWidthOf = maybe 0 (max 0 . subtract 1 . treeWidth) . sideTree

-- Pinning changes only view placement. Console processes remain owned by Consoles.
terminalWindow :: Desktop -> Window -> Bool
terminalWindow d w = maybe False terminal (windowDocument (buffers d) w >>= documentLabel)
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
      [(Just (windowId w),(if (documentLabel =<< windowDocument (buffers d) w)==Just "Autocomplete" then "Autocomplete " else "Terminal ")<>T.pack (show (windowNumber w))) | w<-sortOn windowNumber (windows d),windowPinned d w]
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
resizeProblems y d = clampHexScroll d (ensureVisibleAfterLayout d fitted)
  where
    sh=snd (screenSize d)
    requested=sh-max 3 (min (sh-7) (sh-y-1))-1
    (edge,moved)=resizeEdge True True requested (1,sh-1) (-2,problemsRect d) (floatingWindows d)
    next=d {problemsPreferredHeight=sh-edge-1,drag=Just MessagesSizing}
    fitted=layoutBottomWindows (replaceFloating (map (fitDockWindow next) moved) next) {
      sideTree=fmap (\t -> t {treeScroll=min (treeScroll t) (treeScrollLimit next t)}) (sideTree next)}

layoutProblems :: Desktop -> Desktop -> Desktop
layoutProblems before after = clampHexScroll before (ensureVisibleAfterLayout before fitted)
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
copyMessages issues d = ((copyClipboard False (T.intercalate "\n\n" (map format issues)) d) {status="Messages copied."},[])
  where format issue=(case diagnosticSeverity issue of 1 -> "Error "; 2 -> "Warning "; 3 -> "Info "; _ -> "Hint ")<>
          T.pack (diagnosticPath issue)<>":"<>T.pack (show (diagnosticRow issue+1))<>":"<>T.pack (show (diagnosticColumn issue+1))<>" "<>diagnosticMessage issue

-- | Publish a replacement diagnostic projection with a fresh popup lifetime.
-- Even an equal-looking refresh invalidates captured actions; no payload equality
-- or message scanning is needed to check their currentness.
setDiagnostics :: [Diagnostic] -> Desktop -> Desktop
setDiagnostics values d=d {diagnostics=values,diagnosticsGeneration=diagnosticsGeneration d+1}

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

-- Shared by input dispatch and permission admission, including fallback keys.
problemsKeyCommand :: V.Key -> [V.Modifier] -> Maybe Command
problemsKeyCommand key mods
  | V.MCtrl `elem` mods, key `elem` [V.KChar 'c',V.KChar 'C',V.KIns] = Just Copy
  | otherwise = Nothing

problemsKey :: V.Key -> [V.Modifier] -> Desktop -> (Desktop,[Effect])
problemsKey key mods d
  | Just command<-problemsKeyCommand key mods = runCommand command d
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

-- | Mount host-prepared sidebar metadata without changing dock geometry.
installSidebar :: Sidebar -> Desktop -> Desktop
installSidebar tree d=setTree (Just tree) d {problemsFocused=False}

activateTree :: Bool -> Int -> Desktop -> (Desktop,[Effect])
activateTree forceOpen index d=case sideTree d of
  Just tree | Just row<-rowAt index tree->
    let chosen=(case rowAction row of ActivateNode->dismissRecoveryAnchors tree; _->tree) {treeSelected=index,treeFocused=True}
        selected=d {sideTree=Just chosen,problemsFocused=False}
        expand cursor=let (next,request)=requestChildren (currentHit row chosen) cursor chosen
                      in (selected {sideTree=Just next},maybe [] (\value->[LoadTree value Plugin.HumanMenu]) request)
    in case rowAction row of
      WaitForLoad->(selected,[])
      RetryLoad->expand Nothing
      LoadNext cursor->expand (Just cursor)
      ActivateNode | Tree.infoBranch (rowInfo row)->case M.lookup (keyOf (rowHit row)) (treeNodes chosen) of
        Just node | stateExpanded node && not forceOpen->(selected {sideTree=Just (collapseAt index (maybe chosen (`dismissRecoveryBranch` chosen) (Tree.infoResource (rowInfo row))))},[])
                  | stateExpanded node->(selected,[])
                  | Loaded{}<-stateLoad node->(selected {sideTree=Just chosen {treeNodes=M.adjust (\value->value {stateExpanded=True}) (keyOf (rowHit row)) (treeNodes chosen),treeRevision=treeRevision chosen+1}},[])
        _->expand Nothing
      ActivateNode | Just action<-rowCommand row->
        let trace=hitTrace (keyOf (rowHit row)) chosen
        in if hitCurrent (rowHit row:drop 1 trace) chosen
          then (selected,[InvokeTree (rowHit row:drop 1 trace) action Plugin.HumanMenu]) else (selected,[])
      _->(selected,[])
  _->(d,[])
  where currentHit row tree=maybe (rowHit row) (nodeHit (keyOf (rowHit row))) (M.lookup (keyOf (rowHit row)) (treeNodes tree))

treeKey :: V.Key -> [V.Modifier] -> Sidebar -> Desktop -> (Desktop,[Effect])
treeKey key mods tree d = case key of
  V.KUp -> move (-1)
  V.KDown -> move 1
  V.KPageUp -> move (-10)
  V.KPageDown -> move 10
  V.KHome -> selectTreeRow 0 tree d
  V.KEnd -> selectTreeRow (M.size (treeRows tree)-1) tree d
  V.KEnter -> activateTree False (treeSelected tree) d
  V.KRight -> activateTree True (treeSelected tree) d
  V.KLeft -> collapseTree tree d
  V.KEsc -> leave
  V.KChar '\t' -> leave
  V.KFun 6 -> leave
  V.KChar _ | null mods || mods == [V.MShift] -> (d,[])
  V.KBS -> (d,[])
  V.KDel -> (d,[])
  _ -> keyEvent key mods d
  where
    move delta=moveTree delta tree d
    leave=(d {sideTree=Just tree {treeFocused=False}},[])

moveTree :: Int -> Sidebar -> Desktop -> (Desktop,[Effect])
moveTree delta tree d = selectTreeRow (treeSelected tree+delta) tree d

selectTreeRow :: Int -> Sidebar -> Desktop -> (Desktop,[Effect])
selectTreeRow i tree d = (d {sideTree=Just intent {treeSelected=chosen,treeScroll=scroll}},[])
  where intent=case rowAt chosen tree of
          Just row | ActivateNode<-rowAction row->dismissRecoveryAnchors tree
          _->tree
        chosen=max 0 (min (M.size (treeRows tree)-1) i)
        visible=max 1 (treeContentRows d)
        scroll=max 0 (min chosen (max (treeScroll tree) (chosen-visible+1)))

collapseTree :: Sidebar -> Desktop -> (Desktop,[Effect])
collapseTree tree d=case rowAt (treeSelected tree) tree of
  Just row | rowExpanded row->let chosen=dismissRecoveryAnchors tree
    in (d {sideTree=Just (collapseAt (treeSelected tree) (maybe chosen (`dismissRecoveryBranch` chosen) (Tree.infoResource (rowInfo row))))},[])
  Just row | Just node<-M.lookup (keyOf (rowHit row)) (treeNodes tree), Just parent<-stateParent node->
    let ancestor=M.lookup parent (treeNodes tree) >>= (\value->M.lookupIndex (stateAddress value) (treeRows tree))
    in maybe (d,[]) (\index->selectTreeRow index tree d) ancestor
  _->(d,[])

treeContentRows :: Desktop -> Int
treeContentRows d = max 0 (snd (screenSize d)-4-problemsHeight d)

treeScrollLimit :: Desktop -> Sidebar -> Int
treeScrollLimit d tree = max 0 (M.size (treeRows tree)-treeContentRows d)

scrollTreeTo :: Int -> Sidebar -> Desktop -> Desktop
scrollTreeTo position tree d = d {sideTree=Just (dismissRecoveryScroll tree) {treeScroll=max 0 (min (treeScrollLimit d tree) position)}}

treeMouse :: Int -> Int -> V.Button -> Sidebar -> Desktop -> (Desktop,[Effect])
treeMouse x y button tree d = case button of
  V.BRight | y>=2, y<sh-2, Just row<-rowAt (treeScroll tree+y-2) tree, not (null (rowActions row))->
    let trace=rowHit row:drop 1 (hitTrace (keyOf (rowHit row)) tree)
        actions=[(title,case target of Tree.RegisteredAction action->TreeCommand trace action; Tree.ResourceLink path targetText->OpenLink (SourceLink (Just path)) targetText) | (title,target)<-rowActions row]
    in (openContext (TreeContext trace actions) x (y+1) d {sideTree=Just tree {treeFocused=True},problemsFocused=False},[])
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

-- | Preserve the open-dialog form when reporting filesystem failures.
browserError :: T.Text -> Desktop -> Desktop
browserError err d = case dialog d of
  Just dg -> d {dialog=Just dg {body=take 1 (body dg) ++ [T.take 54 err]},status=err}
  Nothing -> message "Cannot open directory" (wrapMessage err) d

openBrowser :: FilePath -> Text -> [Entry] -> Desktop -> Desktop
openBrowser base pattern entries d = d {dialog=Just (Dialog "Open a file" (Opening base pattern entries) [Input "Name" pattern (T.length pattern),FileList entries 0] 1 ["Open","Cancel"] []),menu=Nothing,drag=Nothing,dragOriginal=Nothing}

addHelpStyled :: StyledText -> Desktop -> Desktop
addHelpStyled chars d = let opened=addHelp (styledContents chars) d
                       in case activeWindow opened of
                         Nothing -> opened
                         Just w | Just bid<-bufferId w -> opened {buffers=M.adjust (\doc -> (setDocumentHighlight [(c,ProseStyle style) | (c,style)<-chars] doc) {documentLinks=linkSpans chars}) bid (buffers opened)}
                         _ -> opened

addHelp :: Text -> Desktop -> Desktop
addHelp text d = addReadOnly "Haskell Help" text d

addReadOnly :: Text -> Text -> Desktop -> Desktop
addReadOnly title text = addReadOnlyBuffer title (newBuffer text)

-- | Adopt an already measured read-only buffer. Replacements advance the source
-- revision; the prepared immutable geometry is independent of that revision.
addReadOnlyBuffer :: Text -> Buffer -> Desktop -> Desktop
addReadOnlyBuffer title prepared d = case [(bid,w) | (bid,doc)<-M.toList (buffers d),documentLabel doc==Just title,w<-windows d,bufferId w==Just bid] of
  (bid,w):_ -> focusWindow (windowId w) d {buffers=M.adjust (\doc -> restyle doc {documentBuffer=prepared {revision=revision (documentBuffer doc)+1}}) bid (buffers d)}
  [] -> let new=modifyActive (\w -> w {bufferView=CurrentView,reviewSelection=Nothing}) (addDocument Nothing prepared d) in new {buffers=M.adjust (\doc -> doc {documentLabel=Just title}) (nextId d) (buffers new)}

-- Hit testing uses the same cell geometry as selection, including tabs and wide glyphs.
hoverAt :: Int -> Int -> Desktop -> (Desktop,[Effect])
hoverAt x y d | Just dg<-dialog d, Just (i,name,choices,chosen,_)<-openComboBox dg =
  let rect=comboBoxRect d dg i choices
  in if inside rect x y && y>top rect && y<top rect+height rect-1
     then (d {dialog=Just dg {fields=replaceAt i (ComboBox name choices chosen (Just (y-top rect-1))) (fields dg)}},[])
     else (d,[])
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
          bid <- bufferId w
          doc <- windowDocument (buffers d) w
          if documentLabel doc/=Nothing || not (textBuffer (documentBuffer doc)) then Nothing else do
            let b=documentBuffer doc
                visualRow=y-t-1+scrollRow w
                normalRow=visualRow
                normalColumn=x-l-1+scrollColumn w
            if windowChangeView b w then do
              (_,row,projected)<-reviewHit x y b w
              case bufferChangeRows b row 1 of
                (DeletedLine,_,_):_ -> Nothing
                _ | projected<changeLineOffset b row+T.length (changeLineAt b row) -> Just (bid,revision b,changeToLiveOffset b projected)
                _ -> Nothing
            else let line=contentSourceLineAt (bufferContent b) normalRow; offset=sourceLineColumnOffset line normalColumn
                 in if normalRow>=bufferLineCount b || offset>=sourceLineLength line then Nothing
                    else Just (bid,revision b,bufferLineOffset b normalRow+offset)

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

reviewPaneWidths :: Window -> (Int,Int)
reviewPaneWidths w = (leftWidth,available-leftWidth)
  where available=max 0 (width (bounds w)-3)
        minimumWidth=min 4 (available `div` 2)
        leftWidth=max minimumWidth (min (available-minimumWidth) (available*reviewSplit w `div` 100))

windowHexBytes :: Window -> Int
windowHexBytes = hexBytesPerRow . subtract 2 . width . bounds

windowDocumentWidth :: Document -> Window -> Int
windowDocumentWidth doc w | byteMode (documentBuffer doc) = hexWidth (windowHexBytes w)
                          | windowChangeView b w = maximum (documentWidth doc:[displayColumn line (T.length line) | visual<-[scrollRow w..min (documentRows doc w-1) (scrollRow w+max 0 (height (bounds w)-2))],
                              let entry=viewRowAt (bufferView w) (bufferViewProjection b) visual, row<-maybe [] pure (viewLeftRow entry)++maybe [] pure (viewRightRow entry),let line=changeLineAt b row])
                          | sourceScrollbarDocument doc w = fromMaybe (fst (sourceWindowExtent doc w)) (sourceWidthEstimate doc w)
                          | documentSourceRows doc/=Nothing = documentWidth doc
                          | otherwise = maximum (documentWidth doc:caretColumn:
                              [sourceLineWidth line | line<-take (max 0 (height (bounds w)-2)+1) (contentSourceLinesFrom (bufferContent b) (scrollRow w))])
  where b=documentBuffer doc
        (_,caretColumn)=windowCursorCell b w

-- Ordinary Current source geometry borrows only the demanded row prefixes.
-- Review, prepared layouts, byte buffers and semantic documents keep their own
-- extents. A prospective seek can discover EOF and refine the estimate at once.
sourceScrollbarDocument :: Document -> Window -> Bool
sourceScrollbarDocument doc w=bufferView w==CurrentView && syntaxDocument doc

sourceWidthEstimate :: Document -> Window -> Maybe Int
sourceWidthEstimate doc w=do
  bid<-bufferId w
  SourceWidthHint ident version row count extent<-sourceWidthHint w
  if bid==ident && revision (documentBuffer doc)==version && row==scrollRow w && count==sourceWidthRows w
    then Just extent else Nothing

sourceWidthRows :: Window -> Int
sourceWidthRows w=max 0 (height (bounds w)-2)+1

rememberSourceWidth :: Desktop -> Document -> Window -> Window
rememberSourceWidth d doc w
  | sourceScrollbarDocument doc w,Nothing<-windowPresentation d w,Just bid<-bufferId w,
    (extent,True)<-sourceWindowExtent doc w =
      w {sourceWidthHint=Just (SourceWidthHint bid (revision (documentBuffer doc)) (scrollRow w) (sourceWidthRows w) extent)}
  | otherwise=w

sourceWindowExtent :: Document -> Window -> (Int,Bool)
sourceWindowExtent doc w=foldl' combine (0,True)
  [sourceLineExtentThrough line (scrollColumn w+max 1 (width (bounds w)-2)) |
    line<-take (sourceWidthRows w)
      (contentSourceLinesFrom (bufferContent (documentBuffer doc)) (scrollRow w))]
  where combine (extent,exact) (next,known)=(max extent next,exact && known)

documentRows :: Document -> Window -> Int
documentRows doc w | byteMode b = bufferLength b `div` windowHexBytes w+1
                 | windowChangeView b w = viewRowCount (bufferView w) (bufferViewProjection b)
                 | otherwise = bufferLineCount b
  where b=documentBuffer doc

windowCursorCell :: Buffer -> Window -> (Int,Int)
windowCursorCell b w
  | byteMode b = (p `div` windowHexBytes w, if windowHexAscii w then hexAsciiColumn (windowHexBytes w)+p `mod` windowHexBytes w else hexColumn (p `mod` windowHexBytes w)+if windowHexLow w then 1 else 0)
  | windowChangeView b w = let { (row,col)=changeLineColumn b (liveToChangeOffset b p)
                                ; visual=viewRowForChange (bufferView w) (bufferViewProjection b) CurrentSide row
                                ; pane=if bufferView w==SideBySideView then fst (reviewPaneWidths w)+1 else 0 }
                            in (visual,pane+displayColumn (changeLineAt b row) col)
  | otherwise = let (row,col)=bufferLineColumn b p in (row,sourceLineDisplayColumn (contentSourceLineAt (bufferContent b) row) col)
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
    in case key of
      V.KLeft -> move (p-1)
      V.KRight -> move (p+1)
      V.KUp -> verticalMove (-1) shift d
      V.KDown -> verticalMove 1 shift d
      V.KPageUp -> pageMove False shift d
      V.KPageDown -> pageMove True shift d
      V.KHome -> if ctrl then documentEdge False shift d else rowEdge False shift d
      V.KEnd -> if ctrl then documentEdge True shift d else rowEdge True shift d
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

-- | Bounded sidebar identity for a frontend-owned export gesture. No row text,
-- buffer contents or host paths enter this receipt.
fileExportView :: Desktop -> [Integer]
fileExportView d = map fromIntegral [fst (pendingFileExport d),fst (screenSize d),snd (screenSize d),problemsHeight d,
  fromEnum (dialog d/=Nothing),fromEnum (menu d/=Nothing),fromEnum (contextMenu d/=Nothing)] ++ case sideTree d of
    Nothing -> []
    Just tree -> [treeEpoch tree,treeRevision tree] ++ map fromIntegral
      [treeWidth tree,treeScroll tree,treeSelected tree,fromEnum (treeFocused tree)]
