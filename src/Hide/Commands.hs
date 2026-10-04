{-# LANGUAGE OverloadedStrings #-}
-- | Stable public identities for built-in actions, independent of menu placement.
--
-- Canonical names are intended for bindings and extension routing. Frozen legacy
-- spellings keep existing remote clients usable; never derive new public names
-- from Show or parse arbitrary constructor expressions. This catalog is the first
-- step toward registration, not a dynamic plugin registry or authority boundary.
module Hide.Commands (BuiltinCommand(..), builtinCommands, commandIdentifier, commandAliases, terminalSourceBindings) where

import Data.Text (Text)
import Control.Monad (unless, forM_)
import qualified Graphics.Vty as V
import Data.List (find)
import qualified Data.Map.Strict as M
import Hide.Bindings (Bindings, compileBindings, readChord)
import Hide.Model (Command(..), terminalSourceReserved)
import Hide.BufferView (BufferView(..))

-- | One action and its fixed canonical/legacy wire identities.
data BuiltinCommand = BuiltinCommand
  { builtinIdentifier :: Text, builtinLegacyName :: Text, builtinAction :: Command }

-- | Canonical identity for an explicitly registered built-in action.
-- Arbitrary parameterized actions and separators have no implicit public name.
commandIdentifier :: Command -> Maybe Text
commandIdentifier command = builtinIdentifier <$> find ((==command) . builtinAction) builtinCommands

-- | Preferred name first, then the frozen compatibility alias.
commandAliases :: Command -> [Text]
commandAliases command = case find ((==command) . builtinAction) builtinCommands of
  Just entry -> [builtinIdentifier entry,builtinLegacyName entry]
  Nothing -> []

builtinCommands :: [BuiltinCommand]
builtinCommands =
  [BuiltinCommand "hide.file.new" "New" (New)
  ,BuiltinCommand "hide.file.open" "Open" (Open)
  ,BuiltinCommand "hide.file.download" "Download" (Download)
  ,BuiltinCommand "hide.file.save" "Save" (Save)
  ,BuiltinCommand "hide.file.save-as" "SaveAs" (SaveAs)
  ,BuiltinCommand "hide.file.review-disk" "ReviewDisk" (ReviewDisk)
  ,BuiltinCommand "hide.window.close" "Close" (Close)
  ,BuiltinCommand "hide.file.change-directory" "ChangeDir" (ChangeDir)
  ,BuiltinCommand "hide.terminal.open" "OpenTerminal" (OpenTerminal)
  ,BuiltinCommand "hide.app.quit" "Quit" (Quit)
  ,BuiltinCommand "hide.edit.undo" "Undo" (Undo)
  ,BuiltinCommand "hide.edit.redo" "Redo" (Redo)
  ,BuiltinCommand "hide.edit.cut" "Cut" (Cut)
  ,BuiltinCommand "hide.edit.copy" "Copy" (Copy)
  ,BuiltinCommand "hide.edit.paste" "Paste" (Paste)
  ,BuiltinCommand "hide.edit.select-all" "SelectAll" (SelectAll)
  ,BuiltinCommand "hide.edit.toggle-hex" "ToggleHex" (ToggleHex)
  ,BuiltinCommand "hide.language.complete" "Complete" (Complete)
  ,BuiltinCommand "hide.search.find" "Find" (Find)
  ,BuiltinCommand "hide.search.replace" "Replace" (Replace)
  ,BuiltinCommand "hide.search.next" "FindNext" (FindNext)
  ,BuiltinCommand "hide.search.previous" "FindPrevious" (FindPrevious)
  ,BuiltinCommand "hide.search.go-to" "GoTo" (GoTo)
  ,BuiltinCommand "hide.language.definition" "Definition" (Definition)
  ,BuiltinCommand "hide.run.start" "RunTarget" (RunTarget)
  ,BuiltinCommand "hide.run.options" "RunOptions" (RunOptions)
  ,BuiltinCommand "hide.build.stop" "StopBuild" (StopBuild)
  ,BuiltinCommand "hide.terminal.stop" "StopTerminal" (StopTerminal)
  ,BuiltinCommand "hide.build.compile" "CompileTarget" (CompileTarget)
  ,BuiltinCommand "hide.build.make" "MakeTarget" (MakeTarget)
  ,BuiltinCommand "hide.debug.attach" "DebugCommand \"attach\"" (DebugCommand "attach")
  ,BuiltinCommand "hide.debug.launch" "DebugCommand \"launch\"" (DebugCommand "launch")
  ,BuiltinCommand "hide.debug.toggle-breakpoint" "DebugCommand \"breakpoint\"" (DebugCommand "breakpoint")
  ,BuiltinCommand "hide.debug.breakpoints" "DebugCommand \"breakpoints\"" (DebugCommand "breakpoints")
  ,BuiltinCommand "hide.debug.continue" "DebugCommand \"continue\"" (DebugCommand "continue")
  ,BuiltinCommand "hide.debug.pause" "DebugCommand \"pause\"" (DebugCommand "pause")
  ,BuiltinCommand "hide.debug.step-into" "DebugCommand \"stepIn\"" (DebugCommand "stepIn")
  ,BuiltinCommand "hide.debug.step-over" "DebugCommand \"next\"" (DebugCommand "next")
  ,BuiltinCommand "hide.debug.step-out" "DebugCommand \"stepOut\"" (DebugCommand "stepOut")
  ,BuiltinCommand "hide.debug.threads" "DebugCommand \"threads\"" (DebugCommand "threads")
  ,BuiltinCommand "hide.debug.stack" "DebugCommand \"stack\"" (DebugCommand "stack")
  ,BuiltinCommand "hide.debug.scopes" "DebugCommand \"scopes\"" (DebugCommand "scopes")
  ,BuiltinCommand "hide.debug.exceptions" "DebugCommand \"exceptions\"" (DebugCommand "exceptions")
  ,BuiltinCommand "hide.debug.exception-info" "DebugCommand \"exception-info\"" (DebugCommand "exception-info")
  ,BuiltinCommand "hide.debug.output" "DebugCommand \"output\"" (DebugCommand "output")
  ,BuiltinCommand "hide.debug.disconnect" "DebugCommand \"disconnect\"" (DebugCommand "disconnect")
  ,BuiltinCommand "hide.debug.downloads" "DebugCommand \"downloads\"" (DebugCommand "downloads")
  ,BuiltinCommand "hide.view.files" "ToggleTree" (ToggleTree)
  ,BuiltinCommand "hide.git.diff" "GitDiff" (GitDiff)
  ,BuiltinCommand "hide.git.commit" "GitCommit" (GitCommit)
  ,BuiltinCommand "hide.git.fetch" "GitFetch" (GitFetch)
  ,BuiltinCommand "hide.git.pull" "GitPull" (GitPull)
  ,BuiltinCommand "hide.git.merge" "GitMerge" (GitMerge)
  ,BuiltinCommand "hide.language.inspect-type" "InspectType" (InspectType)
  ,BuiltinCommand "hide.language.code-actions" "CodeActions" (CodeActions)
  ,BuiltinCommand "hide.language.rename" "RenameSymbol" (RenameSymbol)
  ,BuiltinCommand "hide.view.messages" "Problems" (Problems)
  ,BuiltinCommand "hide.messages.next" "NextMessage" (NextMessage)
  ,BuiltinCommand "hide.messages.previous" "PreviousMessage" (PreviousMessage)
  ,BuiltinCommand "hide.language.restart" "RestartHLS" (RestartHLS)
  ,BuiltinCommand "hide.agents.conversation" "Conversation" (Conversation)
  ,BuiltinCommand "hide.agents.directory" "AgentDirectory" (AgentDirectory)
  ,BuiltinCommand "hide.agents.model" "AgentChoose \"\"" (AgentChoose "")
  ,BuiltinCommand "hide.agents.cancel" "AgentCancel" (AgentCancel)
  ,BuiltinCommand "hide.agents.resume" "AgentResume" (AgentResume)
  ,BuiltinCommand "hide.agents.new" "AgentNew" (AgentNew)
  ,BuiltinCommand "hide.agents.copy-raw" "AgentCopyRaw" (AgentCopyRaw)
  ,BuiltinCommand "hide.help.gallery" "Gallery" (Gallery)
  ,BuiltinCommand "hide.project.browse" "ProjectBrowser" (ProjectBrowser)
  ,BuiltinCommand "hide.options.preferences" "EditorOptions" (EditorOptions)
  ,BuiltinCommand "hide.options.environment" "EnvironmentOptions" (EnvironmentOptions)
  ,BuiltinCommand "hide.options.chat-input" "ChatInputOptions" (ChatInputOptions)
  ,BuiltinCommand "hide.options.autocomplete" "AutocompleteCommand \"settings\"" (AutocompleteCommand "settings")
  ,BuiltinCommand "hide.options.agents" "AgentOptions" (AgentOptions)
  ,BuiltinCommand "hide.options.agent-permissions" "AgentPermissions" (AgentPermissions)
  ,BuiltinCommand "hide.options.agent-context" "AgentGuidance" (AgentGuidance)
  ,BuiltinCommand "hide.window.tile" "Tile" (Tile)
  ,BuiltinCommand "hide.window.cascade" "Cascade" (Cascade)
  ,BuiltinCommand "hide.window.split-vertical" "SplitVertical" (SplitVertical)
  ,BuiltinCommand "hide.window.split-horizontal" "SplitHorizontal" (SplitHorizontal)
  ,BuiltinCommand "hide.window.zoom" "Zoom" (Zoom)
  ,BuiltinCommand "hide.window.pin-terminal" "ToggleTerminalPin" (ToggleTerminalPin)
  ,BuiltinCommand "hide.window.next" "NextWindow" (NextWindow)
  ,BuiltinCommand "hide.view.current" "SetBufferView CurrentView" (SetBufferView CurrentView)
  ,BuiltinCommand "hide.view.changes" "SetBufferView ChangesView" (SetBufferView ChangesView)
  ,BuiltinCommand "hide.view.only-changes" "SetBufferView OnlyChangesView" (SetBufferView OnlyChangesView)
  ,BuiltinCommand "hide.view.side-by-side" "SetBufferView SideBySideView" (SetBufferView SideBySideView)
  ,BuiltinCommand "hide.help.contents" "Help" (Help)
  ,BuiltinCommand "hide.help.about" "About" (About)
  ]

-- | Prepare standard terminal source bindings. Actions with no default chord can
-- still be bound by their canonical identifier. Other input contexts keep their
-- own bindings until migrated; this table never owns terminal-process input.
terminalSourceBindings :: M.Map Text [Text] -> Either Text (Bindings Command)
terminalSourceBindings overrides=do
  forM_ (concat (M.elems overrides)) $ \raw->do
    (key,mods)<-readChord raw
    unless (not (terminalSourceReserved key mods) && case key of V.KChar _->any (`elem` mods) [V.MCtrl,V.MAlt]; _->True)
      (Left ("Reserved source key: "<>raw))
  compileBindings [(builtinIdentifier entry,builtinAction entry,maybe [] id (lookup (builtinAction entry) defaults)) | entry<-builtinCommands] overrides
  where
    defaults=
      [(New,["Ctrl+N"]),(Open,["F3","Ctrl+O"]),(Save,["F2","Ctrl+S"])
      ,(Close,["Alt+F3"]),(Quit,["Alt+X","Ctrl+Q"])
      ,(Undo,["Ctrl+Z"]),(Redo,["Ctrl+Shift+Z","Ctrl+Y"])
      ,(Copy,["Ctrl+C","Ctrl+Insert"]),(Cut,["Ctrl+X","Shift+Delete"])
      ,(Paste,["Ctrl+V","Shift+Insert"]),(SelectAll,["Ctrl+A"])
      ,(Find,["Ctrl+F"]),(Replace,["Ctrl+H","Ctrl+R"])
      ,(FindNext,["Ctrl+L"]),(FindPrevious,["Ctrl+Shift+L"]),(GoTo,["Ctrl+G"])
      ,(Help,["F1"]),(InspectType,["Shift+F1"]),(Definition,["F12"])
      ,(Complete,["Ctrl+Space"]),(Zoom,["F5"]),(NextWindow,["F6"])
      ,(ToggleTree,["Ctrl+B"]),(Conversation,["Ctrl+Shift+C"]),(AgentNew,["Ctrl+Shift+N"])
      ,(NextMessage,["Alt+F8"]),(PreviousMessage,["Alt+F7"])
      ,(MakeTarget,["F9"]),(CompileTarget,["Alt+F9"]),(RunTarget,["Ctrl+F9"])
      ,(DebugCommand "continue",["F4"]),(DebugCommand "stepIn",["F7"])
      ,(DebugCommand "next",["F8"]),(DebugCommand "stepOut",["Ctrl+F7"])
      ,(DebugCommand "breakpoint",["Ctrl+F8"])]
