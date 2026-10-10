-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE CPP, OverloadedStrings #-}
-- | Module      : Main
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : CPP, OverloadedStrings
--
-- Existing checks run serially because fixtures change process environment
-- and working directory. Optional JUnit output records actual case outcomes.
module Main (main) where
#ifdef WITH_WEB
import qualified WebCheck
import qualified RemoteWebCheck
#endif
#ifdef WITH_FONT
import qualified FontCheck
#endif
#ifdef WITH_PROTOCOL
import qualified RequestedPasteCheck
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
import qualified AccessibilityCheck
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
import qualified SessionSidebarCheck
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
import qualified DebuggerSourcePolicyCheck
import qualified DebuggerWatchesCheck
import qualified DebuggerSidebarCheck
import qualified DebuggerCheck
import qualified CompletionCheck
import qualified DownloadsCheck
import qualified HdbAcquisitionCheck
#ifndef mingw32_HOST_OS
import qualified DebuggerAcquisitionCheck
#endif
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
import Control.Monad (unless, when)
import System.FilePath ((</>), normalise)
import AllocationProfile (AllocationProfile(..), withinBudget)
import Data.Proxy (Proxy(..))
import Test.Tasty (defaultMainWithIngredients, includingOptions, askOption, withResource, inOrderTestGroup, TestTree)
import Test.Tasty.Options (OptionDescription(..))
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.Basic (listingTests, consoleTestReporter)
import Test.Tasty.Runners.AntXML (antXMLRunner)
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
import qualified MarkdownViewCheck
import qualified WideTextCheck
import qualified TextStyleCheck
import qualified PluginFormCheck
import qualified PluginWindowsCheck
import qualified WindowCheck
import qualified FilesCheck
import qualified FileExportCheck
import qualified FileDragHelperCheck
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
import qualified Hide.Plugin.Menu as PluginMenu
import Hide.Model
import qualified Graphics.Vty as V
import qualified Data.Map.Strict as M
import Data.Foldable (toList)

check :: String -> Bool -> IO ()
check name ok = unless ok (error name)

main :: IO ()
main = defaultMainWithIngredients
  [includingOptions [Option (Proxy :: Proxy AllocationProfile)]
  , listingTests, composeReporters consoleTestReporter antXMLRunner] tests

-- Exclude overlapping ownership of process-wide environment and cwd.
-- Groups remain independent; no group requires a predecessor to have run.
tests :: TestTree
tests = askOption $ \profile->withResource
  (when (profile==Instrumented) (putStrLn "Instrumented allocation profile: counters are measured; optimized limits are enforced by the separate normal run."))
  (const (pure ())) $ \_->inOrderTestGroup (if profile==Instrumented then "editor-instrumented" else "editor")
  [ testCase "FileExport" FileExportCheck.checks
  , testCase "FileDragHelper" FileDragHelperCheck.checks
  , testCase "Bindings" BindingsCheck.checks
  , testCase "HintComposer" HintComposerCheck.checks
  , testCase "Autocomplete" AutocompleteCheck.checks
  , testCase "Inline" InlineCheck.checks
  , testCase "InlineRender" InlineRenderCheck.checks
  , testCase "AutocompleteACP" AutocompleteACPCheck.checks
  , testCase "Copilot" CopilotCheck.checks
  , testCase "Highlighting" (HighlightingCheck.checks profile)
  , testCase "AgentSidebar" AgentSidebarCheck.checks
  , testCase "SessionSidebar" SessionSidebarCheck.checks
  , testCase "AgentIntegration" AgentIntegrationCheck.checks
  , testCase "AgentAccess" AgentAccessCheck.checks
  , testCase "AgentRuntime" AgentRuntimeCheck.checks
  , testCase "AgentHub" AgentHubCheck.checks
  , testCase "AgentACP" AgentACPCheck.checks
  , testCase "AgentMCP" AgentMCPCheck.checks
  , testCase "AgentWorkspace" AgentWorkspaceCheck.checks
#ifdef WITH_REMOTE
  , testCase "Remote" RemoteCheck.checks
  , testCase "Remote.parent-open" RemoteCheck.parentOpenChecks
  , testCase "RemoteWindow" RemoteWindowCheck.checks
  , testCase "RemoteTerminal" (RemoteTerminalCheck.checks profile)
  , testCase "GuestAccess" GuestAccessCheck.checks
  , testCase "Streamer" StreamerCheck.checks
  , testCase "ClipboardMCP" ClipboardMCPCheck.checks
  , testCase "TypedBufferDiffs" TypedBufferDiffsCheck.checks
  , testCase "TypedBufferReads" (TypedBufferReadsCheck.checks profile)
  , testCase "BufferReads" BufferReadsCheck.checks
  , testCase "WorkerDiff" (WorkerDiffCheck.checks profile)
  , testCase "BufferEdits" BufferEditsCheck.checks
  , testCase "PluginBuffer" PluginBufferCheck.checks
  , testCase "Accessibility" AccessibilityCheck.checks
  , testCase "PluginTree" PluginTreeCheck.checks
  , testCase "Sidebar" SidebarCheck.checks
  , testCase "PluginCommand" PluginCommandCheck.checks
  , testCase "PluginMenu" PluginMenuCheck.checks
  , testCase "MenuCommands" MenuCommandsCheck.checks
  , testCase "MenuContext" MenuContextCheck.checks
  , testCase "DocsMCP" DocsMCPCheck.checks
  , testCase "Environment" EnvironmentCheck.checks
  , testCase "ControlMCP" ControlMCPCheck.checks
  , testCase "Defaults" DefaultsCheck.checks
  , testCase "MCPPermissions" MCPPermissionsCheck.checks
  , testCase "EditorMCP" (EditorMCPCheck.checks profile)
  , testCase "HistoryMCP" HistoryMCPCheck.checks
  , testCase "RuntimeMCP" RuntimeMCPCheck.checks
  , testCase "ScreenCapture" ScreenCaptureCheck.checks
  , testCase "WorkspaceMCP" WorkspaceMCPCheck.checks
  , testCase "WorkspaceFilesMCP" WorkspaceFilesMCPCheck.checks
  , testCase "TestsMCP" TestsMCPCheck.checks
#endif
#ifdef WITH_PROTOCOL
  , testCase "RequestedPaste" RequestedPasteCheck.checks
  , testCase "Protocol" (ProtocolCheck.checks profile)
#endif
#ifdef WITH_WEB
  , testCase "Web" WebCheck.checks
  , testCase "RemoteWeb" RemoteWebCheck.checks
#endif
  , testCase "Hex" HexCheck.checks
  , testCase "DAP" DAPCheck.checks
  , testCase "Debugger" DebuggerCheck.checks
  , testCase "DebuggerWatches" DebuggerWatchesCheck.checks
  , testCase "DebuggerSidebar" DebuggerSidebarCheck.checks
  , testCase "DebuggerSourcePolicy" DebuggerSourcePolicyCheck.checks
  , testCase "Completion" CompletionCheck.checks
  , testCase "Downloads" DownloadsCheck.checks
  , testCase "HdbAcquisition" HdbAcquisitionCheck.checks
#ifndef mingw32_HOST_OS
  -- Official hdb bindists are POSIX-only; there is no Windows acquisition check.
  , testCase "DebuggerAcquisition" DebuggerAcquisitionCheck.checks
#endif
  , testCase "Compilers" CompilersCheck.checks
  , testCase "Build" (BuildCheck.checks profile)
  , testCase "Run" RunCheck.checks
  , testCase "Conversation.drafts" ConversationCheck.draftReceiptChecks
  , testCase "Conversation" (ConversationCheck.checks profile)
  , testCase "AgentFiles" AgentFilesCheck.checks
  , testCase "Terminal" TerminalCheck.checks
  , testCase "Consoles" ConsolesCheck.checks
  , testCase "Main.model" modelChecks
  , testCase "Unicode" (UnicodeCheck.checks profile)
#ifdef WITH_REMOTE
  , testCase "Recovery" RecoveryCheck.checks
#endif
  , testCase "BufferView" BufferViewCheck.checks
  , testCase "BufferTree" (BufferTreeCheck.checks profile)
  , testCase "LSP" (LSPCheck.checks profile)
  , testCase "Tooling.allocations" (ToolingCheck.allocationChecks profile)
  , testCase "Tooling" ToolingCheck.checks
  , testCase "Tooling.startup" ToolingCheck.startupChecks
  , testCase "DialogMouse" (DialogMouseCheck.checks profile)
  , testCase "GitOperations" GitOperationsCheck.checks
  , testCase "Git" GitCheck.checks
  , testCase "MarkdownView" MarkdownViewCheck.checks
  , testCase "WideText" WideTextCheck.checks
  , testCase "TextStyle" TextStyleCheck.checks
  , testCase "PluginForm" PluginFormCheck.checks
  , testCase "PluginWindows" PluginWindowsCheck.checks
  , testCase "PluginWindows.rows" PluginWindowsCheck.rowsChecks
  , testCase "Window" WindowCheck.checks
#ifdef WITH_FONT
  , testCase "Font" FontCheck.checks
#endif
  , testCase "Main.browser" browserChecks
  , testCase "Help" HelpCheck.checks
  , testCase "Browser" BrowserCheck.checks
  , testCase "PackageSources" PackageSourcesCheck.checks
  , testCase "PackagePaths" PackagePathsCheck.checks
  , testCase "PackageSidebar" PackageSidebarCheck.checks
  , testCase "ProjectBrowser" ProjectBrowserCheck.checks
  , testCase "Files" FilesCheck.checks
  , testCase "External" ExternalCheck.checks
  , testCase "Reconcile" ReconcileCheck.checks
  , testCase "ACP" ACPCheck.checks
  , testCase "Links" LinksCheck.checks
  , testCase "Markdown" MarkdownCheck.checks
  , testCase "Markdown.performance" MarkdownCheck.performanceChecks
  ]

modelChecks :: IO ()
modelChecks = do
  check "normal allocation profile keeps strict optimized limits" (withinBudget Normal 9 10 && not (withinBudget Normal 10 10))
  check "instrumented allocation profile separates numeric limits" (withinBudget Instrumented 10 10 && withinBudget Instrumented 20 10)
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
  let scalarStyles=concatMap (\(text,style)->replicate (T.length text) style)
  check "nested comment stays a comment" (all ((== Comment) . snd) (highlight "{- x {- y -} z -}"))
  check "apostrophe identifiers" (scalarStyles (highlight "foldl' x") == replicate 6 Plain ++ [Plain,Plain])
  check "highlight preserves all characters" (styledContents (highlight "x = \"hi\" -- ok\n") == "x = \"hi\" -- ok\n")
  check "Python uses maintained language definition" (all ((== Keyword) . snd) (fst (splitStyledAt 3 (highlightFor "test.py" "def f():\n    return 42\n"))))
  mapM_ (\(path,source) -> let tokens=highlightFor path source in
    check ("maintained syntax for "++path) (any ((/=Plain) . snd) tokens && styledContents tokens==source))
    [("test.c","int main(void) { return 42; }\n"),("test.cpp","class Thing { public: int value = 42; };\n"),
     ("test.cabal","name: example\nversion: 0.1\nlibrary\n  build-depends: base\n"),("cabal.project","packages: .\n")]
  check "unknown extension remains plain" (all ((== Plain) . snd) (highlightFor "notes.unknown" "module x = 42"))
  mapM_ (\text -> check "tokenizer preserves source positions" (styledContents (highlight text) == text))
    ["", "\n", "\n\n", "module Main where\r\n\tmain = print \"λ界\"\r\n", "x = '\\x03bb'", "x = [1..10] -- unfinished", "{- open comment\n"]
  let python=highlightDocument $ newDocument (newBuffer "def f():\n    return 42\n") (Just (FileState "test.py" Nothing))
      renamed=highlightDocument $ restyle python {documentFile=Just (FileState "notes.unknown" Nothing)}
  let sourceStyles doc=maybe [] (concatMap (\row->sourceStylesAt row 0) . toList) (documentSourceRows doc)
  check "document filename chooses syntax" (take 3 (sourceStyles python) == replicate 3 Keyword)
  check "renaming refreshes syntax" (sourceStyles renamed == replicate (T.length "def f():    return 42") Plain)
  check "CRLF input is highlighted, not just preserved" (take 6 (scalarStyles (highlight "module Main where\r\n")) == replicate 6 Keyword)
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

browserChecks :: IO ()
browserChecks = do
  let d=initialDesktop (80,25)
      n=fst (runCommand New d)
      key k ms s=fst (handleEvent (V.EvKey k ms) s)
      base=normalise "/tmp"
      fileMenu = n {menu=Just (0,0)}
  check "File Exit mnemonic is X" (snd (handleEvent (V.EvKey (V.KChar 'x') []) fileMenu) == [Exit])
  check "File Save as mnemonic is A" (case dialog (key (V.KChar 'a') [] fileMenu) of Just dg -> dialogTitle dg == "Save file as"; _ -> False)
  check "status shortcut is red" ("color:rgb(170,0,0);background:rgb(170,170,170)'>F1" `T.isInfixOf` snapshotHtml d)
  let entries = [Entry "src" True Nothing Nothing, Entry "Main.hs" False (Just 12) (Just (read "1992-10-30 08:00:00"))]
      browsing = openBrowser base "*.hs" entries d
      chosen = browsing {dialog=fmap (\dg -> dg {focus=1,fields=[Input "Name" "*.hs" 4,FileList entries 1]}) (dialog browsing)}
  check "file browser selection is white on green" ("color:rgb(255,255,255);background:rgb(0,170,0)'> Main.hs" `T.isInfixOf` snapshotHtml chosen)
  check "file browser shows path size and local timestamp" (all (`T.isInfixOf` snapshot chosen) [T.pack (base </> "*.hs"),"Main.hs","12 bytes","Oct 30, 1992 08:00"])
  check "browser enter opens selected file" (snd (handleEvent (V.EvKey V.KEnter []) chosen) == [OpenFile PluginMenu.HumanMenu (base </> "Main.hs")])
  let naming=browsing {dialog=fmap (\dg -> dg {focus=0}) (dialog browsing)}
      erased=key (V.KChar 'u') [V.MCtrl] naming
      named=foldl (\state ch -> key (V.KChar ch) [] state) erased ("Other.hs" :: String)
      openButton=iterate (key (V.KChar '\t') []) named !! 2
  check "keyboard Open honors typed filename" (snd (handleEvent (V.EvKey V.KEnter []) openButton) == [OpenChoice PluginMenu.HumanMenu base "Other.hs" "*.hs"])
  let docked = installSidebar (emptySidebar base 24 True) n
  check "tree reserves editor space" (all ((>=treeWidthOf docked) . left . bounds) (windows docked))
  check "closing tree returns full editor width" (map bounds (windows (fst (runCommand ToggleTree docked))) == map bounds (windows n))
  let help = addHelp "Documentation" n
  check "help text cannot be edited" (activeText (insertText "x" help) == "Documentation")
