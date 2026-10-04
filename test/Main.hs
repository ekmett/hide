{-# LANGUAGE CPP, OverloadedStrings #-}
module Main where
#ifdef WITH_WEB
import qualified WebCheck
#endif
#ifdef WITH_FONT
import qualified FontCheck
#endif
#ifdef WITH_PROTOCOL
import qualified ProtocolCheck
#endif
#ifdef WITH_REMOTE
import qualified RecoveryCheck
import qualified RemoteCheck
import qualified RemoteWindowCheck
import qualified RemoteTerminalCheck
import qualified GuestAccessCheck
import qualified StreamerCheck
import qualified ClipboardMCPCheck
import qualified TypedBufferDiffsCheck
import qualified TypedBufferReadsCheck
import qualified BufferReadsCheck
import qualified WorkerDiffCheck
import qualified BufferEditsCheck
import qualified PluginBufferCheck
import qualified MenuCommandsCheck
import qualified MenuContextCheck
import qualified PluginMenuCheck
import qualified SidebarCheck
import qualified PluginTreeCheck
import qualified PluginCommandCheck
import qualified DocsMCPCheck
import qualified EnvironmentCheck
import qualified ControlMCPCheck
import qualified DefaultsCheck
import qualified MCPPermissionsCheck
import qualified EditorMCPCheck
import qualified HistoryMCPCheck
import qualified RuntimeMCPCheck
import qualified ScreenCaptureCheck
import qualified WorkspaceMCPCheck
import qualified WorkspaceFilesMCPCheck
import qualified TestsMCPCheck
#endif
import qualified BindingsCheck
import qualified HintComposerCheck
import qualified AutocompleteCheck
import qualified InlineCheck
import qualified InlineRenderCheck
import qualified AutocompleteACPCheck
import qualified CopilotCheck
import qualified HighlightingCheck
import qualified AgentSidebarCheck
import qualified AgentIntegrationCheck
import qualified AgentAccessCheck
import qualified AgentRuntimeCheck
import qualified AgentHubCheck
import qualified AgentACPCheck
import qualified AgentMCPCheck
import qualified AgentWorkspaceCheck
import qualified UnicodeCheck
import qualified HexCheck
import qualified DAPCheck
import qualified DebuggerSidebarCheck
import qualified DebuggerCheck
import qualified CompletionCheck
import qualified DownloadsCheck
import qualified HdbAcquisitionCheck
import qualified DebuggerAcquisitionCheck
import qualified CompilersCheck
import qualified BuildCheck
import qualified PackageSidebarCheck
import qualified PackagePathsCheck
import qualified PackageSourcesCheck
import qualified ProjectBrowserCheck
import qualified RunCheck
import qualified ConversationCheck
import qualified AgentFilesCheck
import qualified TerminalCheck
import qualified ConsolesCheck
import Control.Monad (unless)
import qualified Data.Text as T
import Hide.App (demoDesktop)
import qualified BufferViewCheck
import qualified BufferTreeCheck
import qualified LSPCheck
import qualified ToolingCheck
import qualified DialogMouseCheck
import qualified GitOperationsCheck
import qualified GitCheck
import qualified HelpCheck
import qualified BrowserCheck
import Hide.Browser (Entry(..))
import qualified PluginWindowsCheck
import qualified WindowCheck
import qualified FilesCheck
import qualified ExternalCheck
import qualified ReconcileCheck
import qualified ACPCheck
import qualified LinksCheck
import qualified MarkdownCheck
import Hide.Render
import Hide.Buffer
import Hide.Syntax
import Hide.Files (FileState(..))
import Hide.Sidebar
import Hide.Model
import qualified Graphics.Vty as V
import qualified Data.Map.Strict as M

check :: String -> Bool -> IO ()
check name ok = unless ok (error name)

main :: IO ()
main = do
  BindingsCheck.checks
  HintComposerCheck.checks
  AutocompleteCheck.checks
  InlineCheck.checks
  InlineRenderCheck.checks
  AutocompleteACPCheck.checks
  CopilotCheck.checks
  HighlightingCheck.checks
  AgentSidebarCheck.checks
  AgentIntegrationCheck.checks
  AgentAccessCheck.checks
  AgentRuntimeCheck.checks
  AgentHubCheck.checks
  AgentACPCheck.checks
  AgentMCPCheck.checks
  AgentWorkspaceCheck.checks
#ifdef WITH_REMOTE
  RemoteCheck.checks
  RemoteWindowCheck.checks
  RemoteTerminalCheck.checks
  GuestAccessCheck.checks
  StreamerCheck.checks
  ClipboardMCPCheck.checks
  TypedBufferDiffsCheck.checks
  TypedBufferReadsCheck.checks
  BufferReadsCheck.checks
  WorkerDiffCheck.checks
  BufferEditsCheck.checks
  PluginBufferCheck.checks
  PluginTreeCheck.checks
  SidebarCheck.checks
  PluginCommandCheck.checks
  PluginMenuCheck.checks
  MenuCommandsCheck.checks
  MenuContextCheck.checks
  DocsMCPCheck.checks
  EnvironmentCheck.checks
  ControlMCPCheck.checks
  DefaultsCheck.checks
  MCPPermissionsCheck.checks
  EditorMCPCheck.checks
  HistoryMCPCheck.checks
  RuntimeMCPCheck.checks
  ScreenCaptureCheck.checks
  WorkspaceMCPCheck.checks
  WorkspaceFilesMCPCheck.checks
  TestsMCPCheck.checks
#endif
#ifdef WITH_PROTOCOL
  ProtocolCheck.checks
#endif
#ifdef WITH_WEB
  WebCheck.checks
#endif
  HexCheck.checks
  DAPCheck.checks
  DebuggerCheck.checks
  DebuggerSidebarCheck.checks
  CompletionCheck.checks
  DownloadsCheck.checks
  HdbAcquisitionCheck.checks
  DebuggerAcquisitionCheck.checks
  CompilersCheck.checks
  BuildCheck.checks
  RunCheck.checks
  ConversationCheck.checks
  AgentFilesCheck.checks
  TerminalCheck.checks
  ConsolesCheck.checks
  let b = newBuffer "hello\nworld"
      edited = replaceSelection (Selection 0 5) "λ" b
  check "selection replacement" (contents edited == "λ\nworld")
  check "undo restores original" (contents (undo edited) == "hello\nworld")
  check "redo restores replacement" (contents (redo (undo edited)) == "λ\nworld")
  check "savepoint dirty tracking" (not (dirty (undo edited)) && dirty edited)
  check "paste is one undo action" (contents (undo (replaceSelection (Selection 5 5) "\na\nb" b)) == contents b)
  check "trailing newline position" (lineColumn "a\n" 2 == (1,0))
  check "tab and wide display columns" (displayColumn "\t界x" 2 == 10)
  check "click inside wide glyph" (columnOffset "\t界x" 9 == 1)
  check "click after wide glyph" (columnOffset "\t界x" 10 == 2)
  check "combining sequence moves together" (nextCharacter "e\x0301x" 0 == 2)
  check "nested comment stays a comment" (all ((== Comment) . snd) (highlight "{- x {- y -} z -}"))
  check "apostrophe identifiers" (map snd (highlight "foldl' x") == replicate 6 Plain ++ [Plain,Plain])
  check "highlight preserves all characters" (T.pack (map fst (highlight "x = \"hi\" -- ok\n")) == "x = \"hi\" -- ok\n")
  check "Python uses maintained language definition" (all ((== Keyword) . snd) (take 3 (highlightFor "test.py" "def f():\n    return 42\n")))
  mapM_ (\(path,source) -> let tokens=highlightFor path source in
    check ("maintained syntax for "++path) (any ((/=Plain) . snd) tokens && T.pack (map fst tokens)==source))
    [("test.c","int main(void) { return 42; }\n"),("test.cpp","class Thing { public: int value = 42; };\n"),
     ("test.cabal","name: example\nversion: 0.1\nlibrary\n  build-depends: base\n"),("cabal.project","packages: .\n")]
  check "unknown extension remains plain" (all ((== Plain) . snd) (highlightFor "notes.unknown" "module x = 42"))
  mapM_ (\text -> check "tokenizer preserves source positions" (T.pack (map fst (highlight text)) == text))
    ["", "\n", "\n\n", "module Main where\r\n\tmain = print \"λ界\"\r\n", "x = '\\x03bb'", "x = [1..10] -- unfinished", "{- open comment\n"]
  let python=highlightDocument $ newDocument (newBuffer "def f():\n    return 42\n") (Just (FileState "test.py" Nothing))
      renamed=highlightDocument $ restyle python {documentFile=Just (FileState "notes.unknown" Nothing)}
  check "document filename chooses syntax" (take 3 (map snd (documentHighlight python)) == replicate 3 Keyword)
  check "renaming refreshes syntax" (all ((== Plain) . snd) (documentHighlight renamed))
  check "CRLF input is highlighted, not just preserved" (take 6 (map snd (highlight "module Main where\r\n")) == replicate 6 Keyword)
  let d = initialDesktop (80,25)
      key k ms s = fst (handleEvent (V.EvKey k ms) s)
      n = fst (runCommand New d)
      modal = fst (runCommand About n)
      clicked = fst (handleEvent (V.EvMouseDown 20 10 V.BLeft []) modal)
      typed = key (V.KChar 'x') [] modal
  let typedKeyword=insertText "module" n
      undoneKeyword=fst (runCommand Undo typedKeyword)
  check "edits invalidate syntax without tokenizing on input" (fmap documentHighlight (activeDocument typedKeyword) == Just [] && fmap documentSourceRows (activeDocument typedKeyword)==Just Nothing)
  check "undo refreshes syntax cache" (fmap documentHighlight (activeDocument undoneKeyword) == Just [])
  check "modal blocks text" (buffers typed == buffers modal)
  check "modal blocks underlying focus" (map windowId (windows clicked) == map windowId (windows modal))
  check "escape restores editor focus" (dialog (key V.KEsc [] modal) == Nothing)
  let changed = key (V.KChar 'x') [] n
      split = fst (runCommand SplitVertical changed)
  check "split shares buffer" (length (windows split) == 2 && M.size (buffers split) == 1)
  let shared = key (V.KChar 'y') [] split
  check "split edits same buffer" (maybe False ((== "xy") . contents . documentBuffer) (activeDocument shared))
  let quitting = fst (runCommand Quit shared)
  check "dirty quit asks" (dialog quitting /= Nothing)
  check "cancel quit retains text" (buffers (key V.KEsc [] quitting) == buffers shared)
  let menuState = key (V.KFun 10) [] d
  check "F10 activates menu" (menu menuState /= Nothing)
  let small = fst (handleEvent (V.EvResize 30 10) split)
  check "resize keeps frames inside desktop" (all (\w -> let Rect x y width height = bounds w in x >= 0 && y >= 1 && x+width <= 30 && y+height <= 9) (windows small))
  check "snapshot keeps 25 rows" (length (T.lines (snapshot d)) == 25)
  check "dialog labels cannot inject terminal controls" (not (T.any (<' ') (T.filter (/='\n') (snapshot (message "Provider" ["\ESC[31m"] d)))))
  check "snapshot has menu" ("File" `T.isInfixOf` snapshot d)
  let dragging=case activeWindow demoDesktop of Just w -> demoDesktop {drag=Just (Moving (windowId w) 0 0)}; Nothing -> demoDesktop
  check "moving frame becomes cyan and single line" ("┌" `T.isInfixOf` snapshot dragging && "color:rgb(85,255,255);background:rgb(0,0,170)" `T.isInfixOf` snapshotHtml dragging)
  check "source text survives zero horizontal scroll" ("factorial" `T.isInfixOf` snapshot demoDesktop)
  check "CRLF moves as one newline" (nextCharacter "a\r\nb" 1 == 3 && previousCharacter "a\r\nb" 3 == 1)
  check "word-left crosses punctuation" (wordLeft "foo.bar" 4 == 3)
  let twoDirty = insertText "second" (fst (runCommand New shared))
      asked = fst (runCommand Quit twoDirty)
      discarded = case dialog asked of Just dg -> fst (submitDialog 1 dg asked); Nothing -> asked
      cancelled = key V.KEsc [] discarded
  check "discard then cancel cannot bless unsaved text" (all (dirty . documentBuffer) (M.elems (buffers cancelled)))
  let star = key (V.KChar 'b') [] (key (V.KChar 'k') [V.MCtrl] changed {wordStar=True})
      starMoved = key (V.KChar 's') [V.MCtrl] star
      starBlock = key (V.KChar 'k') [] (key (V.KChar 'k') [V.MCtrl] starMoved)
  check "WordStar block marker survives movement" (maybe False ((== (0,1)) . ordered . selection) (activeWindow starBlock))
  let repeated = addDocument Nothing (newBuffer "aaaa") d
      repSplit = fst (runCommand SplitVertical repeated)
      positioned = repSplit {windows=case windows repSplit of w:v:rest -> w:v {selection=Selection 2 2}:rest; ws -> ws}
      inserted = insertText "a" positioned
  check "repeated text rebase uses actual edit" (map (caret . selection) (windows inserted) == [1,3])
  let undone = fst (runCommand Undo inserted)
  check "undo rebases other split cursor" (map (caret . selection) (windows undone) == [0,2])
  let resizeClick = fst (handleEvent (V.EvMouseDown 78 23 V.BLeft []) n)
  check "visible resize grip captures drag" (case drag resizeClick of Just Resizing{} -> True; _ -> False)
  let abc = moveTo False 1 (addDocument Nothing (newBuffer "abc") d)
  check "undo without history preserves cursor" (fmap selection (activeWindow (fst (runCommand Undo abc)))==fmap selection (activeWindow abc))
  check "wrapped search spans old cursor" (maybe False ((== (0,3)) . ordered . selection) (activeWindow (findText "abc" abc)))
  check "control placeholders use one column" (displayColumn "a\SOHb" 2 == 2 && columnOffset "a\SOHb" 2 == 2)
  let crowded = iterate (fst . runCommand New) d !! 6
      tiled = fst (runCommand Tile crowded)
  check "tile refuses unusably short windows" (map bounds (windows tiled) == map bounds (windows crowded))
  let gallery = fst (runCommand Gallery (initialDesktop (80,12)))
      scrolled = iterate (key (V.KChar '\t') []) gallery !! 3
      borderClick = case dialog scrolled of
        Just dg -> let Rect x y _ _ = dialogRect scrolled dg in fst (handleEvent (V.EvMouseDown (x+4) y V.BLeft []) scrolled)
        Nothing -> scrolled
  check "clipped dialog fields cannot receive border clicks" (dialog borderClick == dialog scrolled)
  let shadowBase=addDocument Nothing (newBuffer (T.unlines (replicate 24 (T.replicate 78 "x")))) d
      shadowMenu=shadowBase {menu=Just (0,0)}
      Rect mx my mw _=menuRect shadowMenu 0
      shadowLine=T.lines (snapshot shadowMenu) !! (my+1)
      normalLine=T.lines (snapshot shadowBase) !! (my+1)
  check "menu shadow retains underlying characters" (T.take 2 (T.drop (mx+mw) shadowLine) == T.take 2 (T.drop (mx+mw) normalLine))
  check "shadow text is gray on black" ("color:rgb(170,170,170);background:rgb(0,0,0)'>xx" `T.isInfixOf` snapshotHtml shadowMenu)
  let focusedTree=installSidebar (emptySidebar "/tmp" 24 True) demoDesktop
      pastedTree=fst (handleEvent (V.EvPaste "oops") focusedTree)
      undoneTree=fst (runCommand Undo focusedTree)
  check "tree focus blocks background paste" (buffers pastedTree == buffers focusedTree)
  check "tree focus blocks background undo" (buffers undoneTree == buffers focusedTree)
  UnicodeCheck.checks
#ifdef WITH_REMOTE
  RecoveryCheck.checks
#endif
  BufferViewCheck.checks
  BufferTreeCheck.checks
  LSPCheck.checks
  ToolingCheck.checks
  DialogMouseCheck.checks
  GitOperationsCheck.checks
  GitCheck.checks
  PluginWindowsCheck.checks
  WindowCheck.checks
#ifdef WITH_FONT
  FontCheck.checks
#endif
  let fileMenu = n {menu=Just (0,0)}
  check "File Exit mnemonic is X" (snd (handleEvent (V.EvKey (V.KChar 'x') []) fileMenu) == [Exit])
  check "File Save as mnemonic is A" (case dialog (key (V.KChar 'a') [] fileMenu) of Just dg -> dialogTitle dg == "Save file as"; _ -> False)
  check "status shortcut is red" ("color:rgb(170,0,0);background:rgb(170,170,170)'>F1" `T.isInfixOf` snapshotHtml d)
  let entries = [Entry "src" True Nothing Nothing, Entry "Main.hs" False (Just 12) (Just (read "1992-10-30 08:00:00"))]
      browsing = openBrowser "/tmp" "*.hs" entries d
      chosen = browsing {dialog=fmap (\dg -> dg {focus=1,fields=[Input "Name" "*.hs" 4,FileList entries 1]}) (dialog browsing)}
  check "file browser selection is white on green" ("color:rgb(255,255,255);background:rgb(0,170,0)'> Main.hs" `T.isInfixOf` snapshotHtml chosen)
  check "file browser shows path size and local timestamp" (all (`T.isInfixOf` snapshot chosen) ["/tmp/*.hs","Main.hs","12 bytes","Oct 30, 1992 08:00"])
  check "browser enter opens selected file" (snd (handleEvent (V.EvKey V.KEnter []) chosen) == [ReadPath "/tmp/Main.hs"])
  let naming=browsing {dialog=fmap (\dg -> dg {focus=0}) (dialog browsing)}
      erased=key (V.KChar 'u') [V.MCtrl] naming
      named=foldl (\state ch -> key (V.KChar ch) [] state) erased ("Other.hs" :: String)
      openButton=iterate (key (V.KChar '\t') []) named !! 2
  check "keyboard Open honors typed filename" (snd (handleEvent (V.EvKey V.KEnter []) openButton) == [OpenChoice "/tmp" "Other.hs" "*.hs"])
  let docked = installSidebar (emptySidebar "/tmp" 24 True) n
  check "tree reserves editor space" (all ((>=treeWidthOf docked) . left . bounds) (windows docked))
  check "closing tree returns full editor width" (map bounds (windows (fst (runCommand ToggleTree docked))) == map bounds (windows n))
  let help = addHelp "Documentation" n
  check "help text cannot be edited" (activeText (insertText "x" help) == "Documentation")
  HelpCheck.checks
  BrowserCheck.checks
  PackageSourcesCheck.checks
  PackagePathsCheck.checks
  PackageSidebarCheck.checks
  ProjectBrowserCheck.checks
  FilesCheck.checks
  ExternalCheck.checks
  ReconcileCheck.checks
  ACPCheck.checks
  LinksCheck.checks
  MarkdownCheck.checks
  putStrLn "editor checks passed"
