{-# LANGUAGE OverloadedStrings #-}
-- | Stable public identities for built-in actions, independent of menu placement.
--
-- Canonical names are intended for bindings and extension routing. Never derive
-- public names from Show or parse arbitrary constructor expressions. This catalog
-- is the first step toward registration, not a dynamic plugin registry or
-- authority boundary.
module Hide.Commands (BuiltinCommand(..), builtinCommands, commandIdentifier, terminalSourceBindings) where

import Data.Text (Text)
import Control.Monad (unless, forM_)
import qualified Graphics.Vty as V
import Data.List (find)
import qualified Data.Map.Strict as M
import Hide.Bindings (Bindings, compileBindings, readChord)
import Hide.Model (Command(..), terminalSourceReserved)
import Hide.BufferView (BufferView(..))

-- | One action and its public identity.
data BuiltinCommand = BuiltinCommand
  { builtinIdentifier :: Text, builtinAction :: Command }

-- | Canonical identity for an explicitly registered built-in action.
-- Arbitrary parameterized actions and separators have no implicit public name.
commandIdentifier :: Command -> Maybe Text
commandIdentifier command = builtinIdentifier <$> find ((==command) . builtinAction) builtinCommands

builtinCommands :: [BuiltinCommand]
builtinCommands =
  [BuiltinCommand "hide.file.new" (New)
  ,BuiltinCommand "hide.file.open" (Open)
  ,BuiltinCommand "hide.file.download" (Download)
  ,BuiltinCommand "hide.file.save" (Save)
  ,BuiltinCommand "hide.file.save-as" (SaveAs)
  ,BuiltinCommand "hide.file.review-disk" (ReviewDisk)
  ,BuiltinCommand "hide.window.close" (Close)
  ,BuiltinCommand "hide.file.change-directory" (ChangeDir)
  ,BuiltinCommand "hide.terminal.open" (OpenTerminal)
  ,BuiltinCommand "hide.app.quit" (Quit)
  ,BuiltinCommand "hide.edit.undo" (Undo)
  ,BuiltinCommand "hide.edit.redo" (Redo)
  ,BuiltinCommand "hide.edit.cut" (Cut)
  ,BuiltinCommand "hide.edit.copy" (Copy)
  ,BuiltinCommand "hide.edit.paste" (Paste)
  ,BuiltinCommand "hide.edit.select-all" (SelectAll)
  ,BuiltinCommand "hide.edit.toggle-hex" (ToggleHex)
  ,BuiltinCommand "hide.language.complete" (Complete)
  ,BuiltinCommand "hide.search.find" (Find)
  ,BuiltinCommand "hide.search.replace" (Replace)
  ,BuiltinCommand "hide.search.next" (FindNext)
  ,BuiltinCommand "hide.search.previous" (FindPrevious)
  ,BuiltinCommand "hide.search.go-to" (GoTo)
  ,BuiltinCommand "hide.language.definition" (Definition)
  ,BuiltinCommand "hide.run.start" (RunTarget)
  ,BuiltinCommand "hide.run.options" (RunOptions)
  ,BuiltinCommand "hide.build.stop" (StopBuild)
  ,BuiltinCommand "hide.terminal.stop" (StopTerminal)
  ,BuiltinCommand "hide.build.compile" (CompileTarget)
  ,BuiltinCommand "hide.build.make" (MakeTarget)
  ,BuiltinCommand "hide.debug.attach" (DebugCommand "attach")
  ,BuiltinCommand "hide.debug.launch" (DebugCommand "launch")
  ,BuiltinCommand "hide.debug.toggle-breakpoint" (DebugCommand "breakpoint")
  ,BuiltinCommand "hide.debug.breakpoints" (DebugCommand "breakpoints")
  ,BuiltinCommand "hide.debug.continue" (DebugCommand "continue")
  ,BuiltinCommand "hide.debug.pause" (DebugCommand "pause")
  ,BuiltinCommand "hide.debug.step-into" (DebugCommand "stepIn")
  ,BuiltinCommand "hide.debug.step-over" (DebugCommand "next")
  ,BuiltinCommand "hide.debug.step-out" (DebugCommand "stepOut")
  ,BuiltinCommand "hide.debug.threads" (DebugCommand "threads")
  ,BuiltinCommand "hide.debug.stack" (DebugCommand "stack")
  ,BuiltinCommand "hide.debug.scopes" (DebugCommand "scopes")
  ,BuiltinCommand "hide.debug.exceptions" (DebugCommand "exceptions")
  ,BuiltinCommand "hide.debug.exception-info" (DebugCommand "exception-info")
  ,BuiltinCommand "hide.debug.output" (DebugCommand "output")
  ,BuiltinCommand "hide.debug.disconnect" (DebugCommand "disconnect")
  ,BuiltinCommand "hide.debug.downloads" (DebugCommand "downloads")
  ,BuiltinCommand "hide.view.files" (ToggleTree)
  ,BuiltinCommand "hide.git.diff" (GitDiff)
  ,BuiltinCommand "hide.git.commit" (GitCommit)
  ,BuiltinCommand "hide.git.fetch" (GitFetch)
  ,BuiltinCommand "hide.git.pull" (GitPull)
  ,BuiltinCommand "hide.git.merge" (GitMerge)
  ,BuiltinCommand "hide.language.inspect-type" (InspectType)
  ,BuiltinCommand "hide.language.code-actions" (CodeActions)
  ,BuiltinCommand "hide.language.rename" (RenameSymbol)
  ,BuiltinCommand "hide.view.messages" (Problems)
  ,BuiltinCommand "hide.messages.next" (NextMessage)
  ,BuiltinCommand "hide.messages.previous" (PreviousMessage)
  ,BuiltinCommand "hide.language.restart" (RestartHLS)
  ,BuiltinCommand "hide.agents.conversation" (Conversation)
  ,BuiltinCommand "hide.agents.directory" (AgentDirectory)
  ,BuiltinCommand "hide.agents.model" (AgentChoose "")
  ,BuiltinCommand "hide.agents.cancel" (AgentCancel)
  ,BuiltinCommand "hide.agents.resume" (AgentResume)
  ,BuiltinCommand "hide.agents.new" (AgentNew)
  ,BuiltinCommand "hide.agents.copy-raw" (AgentCopyRaw)
  ,BuiltinCommand "hide.help.gallery" (Gallery)
  ,BuiltinCommand "hide.project.browse" (ProjectBrowser)
  ,BuiltinCommand "hide.options.preferences" (EditorOptions)
  ,BuiltinCommand "hide.options.environment" (EnvironmentOptions)
  ,BuiltinCommand "hide.options.chat-input" (ChatInputOptions)
  ,BuiltinCommand "hide.options.autocomplete" (AutocompleteCommand "settings")
  ,BuiltinCommand "hide.options.agents" (AgentOptions)
  ,BuiltinCommand "hide.options.agent-permissions" (AgentPermissions)
  ,BuiltinCommand "hide.options.agent-context" (AgentGuidance)
  ,BuiltinCommand "hide.window.tile" (Tile)
  ,BuiltinCommand "hide.window.cascade" (Cascade)
  ,BuiltinCommand "hide.window.split-vertical" (SplitVertical)
  ,BuiltinCommand "hide.window.split-horizontal" (SplitHorizontal)
  ,BuiltinCommand "hide.window.zoom" (Zoom)
  ,BuiltinCommand "hide.window.pin-terminal" (ToggleTerminalPin)
  ,BuiltinCommand "hide.window.next" (NextWindow)
  ,BuiltinCommand "hide.view.current" (SetBufferView CurrentView)
  ,BuiltinCommand "hide.view.changes" (SetBufferView ChangesView)
  ,BuiltinCommand "hide.view.only-changes" (SetBufferView OnlyChangesView)
  ,BuiltinCommand "hide.view.side-by-side" (SetBufferView SideBySideView)
  ,BuiltinCommand "hide.help.contents" (Help)
  ,BuiltinCommand "hide.help.about" (About)
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
