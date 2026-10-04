{-# LANGUAGE OverloadedStrings #-}
-- | Stable public identities for built-in actions, independent of menu placement.
--
-- Canonical names are intended for bindings and extension routing. Never derive
-- public names from Show or parse arbitrary constructor expressions. This catalog
-- is the first step toward registration, not a dynamic plugin registry or
-- authority boundary.
module Hide.Commands (BuiltinCommand(..), builtinCommands, commandIdentifier, platformBindings, configuredBindings) where

import Data.Text (Text)
import Control.Monad (unless, forM_)
import qualified Graphics.Vty as V
import Data.List (find)
import qualified Data.Map.Strict as M
import Hide.Bindings (BindingPlatform(..), bindingPlatforms, platformName, BindingContext(..), bindingContexts, contextName, Bindings, compileBindings, readChord)
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
  ,BuiltinCommand "hide.bindings.reload" ReloadBindings
  ,BuiltinCommand "hide.bindings.inspect" InspectBindings
  ,BuiltinCommand "hide.source.copy-location" CopyLocation
  ,BuiltinCommand "hide.messages.copy-all" CopyAllMessages
  ,BuiltinCommand "hide.messages.go-to" GoToMessage
  ,BuiltinCommand "hide.sidebar.up" (SidebarMove (-1))
  ,BuiltinCommand "hide.sidebar.down" (SidebarMove 1)
  ,BuiltinCommand "hide.sidebar.page-up" (SidebarMove (-10))
  ,BuiltinCommand "hide.sidebar.page-down" (SidebarMove 10)
  ,BuiltinCommand "hide.sidebar.activate" SidebarActivate
  ,BuiltinCommand "hide.sidebar.expand" SidebarExpand
  ,BuiltinCommand "hide.sidebar.collapse" SidebarCollapse
  ,BuiltinCommand "hide.focus.source" FocusSource
  ,BuiltinCommand "hide.messages.up" (MessagesMove (-1))
  ,BuiltinCommand "hide.messages.down" (MessagesMove 1)
  ,BuiltinCommand "hide.messages.page-up" (MessagesPage (-1))
  ,BuiltinCommand "hide.messages.page-down" (MessagesPage 1)
  ,BuiltinCommand "hide.help.contents" (Help)
  ,BuiltinCommand "hide.help.about" (About)
  ]

-- | Compile every terminal context outside the interaction path. Explicit global
-- entries apply to all owners; a context entry replaces the same global command.
-- Plain PTY control characters cannot be assigned to editor commands.
configuredBindings :: M.Map Text (M.Map Text (M.Map Text [Text])) -> Either Text (M.Map (BindingPlatform,BindingContext) (Bindings Command))
configuredBindings configuration=do
  unless (all (`elem` map platformName bindingPlatforms) (M.keys configuration)) (Left "Unknown keybinding platform")
  M.unions <$> traverse (\platform->platformBindings platform (M.findWithDefault M.empty (platformName platform) configuration)) bindingPlatforms

platformBindings :: BindingPlatform -> M.Map Text (M.Map Text [Text]) -> Either Text (M.Map (BindingPlatform,BindingContext) (Bindings Command))
platformBindings platform configuration=do
  unless (all (`elem` ("global":map contextName bindingContexts)) (M.keys configuration))
    (Left ("Unknown "<>platformName platform<>" keybinding context"))
  forM_ (concat (M.elems (M.findWithDefault M.empty "global" configuration))) readChord
  M.fromList <$> traverse prepare bindingContexts
  where
    prepare context=do
      let global=M.findWithDefault M.empty "global" configuration
          inherited=if context==TerminalKeys then fmap (filter (not . processControlChord)) global else global
          overrides=M.union (M.findWithDefault M.empty (contextName context) configuration) inherited
      forM_ (concat (M.elems overrides)) $ \raw->do
        (key,mods)<-readChord raw
        let character=case key of V.KChar _->not (any (`elem` mods) [V.MCtrl,V.MAlt,V.MMeta]); _->False
            contextReserved=case context of
              SidebarKeys -> character
              MessagesKeys -> character
              ConversationKeys -> character || key==V.KEnter
              TerminalKeys -> character || processControl key mods
              _ -> character || key==V.KEnter
        let platformReserved = (platform==TerminalPlatform && V.MMeta `elem` mods) ||
              (platform==MacPlatform && ((V.MAlt `elem` mods && V.MCtrl `notElem` mods && V.MMeta `notElem` mods && case key of V.KChar _->True; _->False) || (key `elem` map V.KChar "h\\[]" && V.MMeta `elem` mods))) ||
              (platform/=TerminalPlatform && key `elem` map V.KChar "0+=-" && any (`elem` mods) [V.MCtrl,V.MAlt])
        unless (not (terminalSourceReserved key mods || contextReserved || platformReserved))
          (Left ("Reserved "<>contextName context<>" key: "<>raw))
      compiled<-either (Left . (("Keybinding context "<>contextName context<>": ")<>)) Right $ compileBindings [(builtinIdentifier entry,builtinAction entry,keys context (builtinAction entry)) | entry<-builtinCommands] overrides
      pure ((platform,context),compiled)
    processControl key mods=case key of
      V.KChar _ -> V.MCtrl `elem` mods && V.MAlt `notElem` mods && V.MMeta `notElem` mods
      _ -> False
    processControlChord raw=case readChord raw of Right (key,mods)->processControl key mods; _->False
    keys context action=maybe [] id (lookup action (if platform==MacPlatform then macDefaults context else defaultsFor context))
    macDefaults context = [(action,chords++maybe [] id (lookup action (defaultsFor context))) | (action,chords)<-macCommands] ++ filter (\(action,_)->action `notElem` map fst macCommands) (defaultsFor context)
    macCommands =
      [(New,["Cmd+N"]),(Open,["Cmd+O"]),(Save,["Cmd+S"]),(SaveAs,["Cmd+Shift+S"]),(Close,["Cmd+W"]),(Quit,["Cmd+Q"])
      ,(Undo,["Cmd+Z"]),(Redo,["Cmd+Shift+Z"]),(Copy,["Cmd+C"]),(Cut,["Cmd+X"]),(Paste,["Cmd+V"]),(SelectAll,["Cmd+A"])
      ,(Find,["Cmd+F"]),(Replace,["Cmd+Alt+F"]),(FindNext,["Cmd+G"]),(FindPrevious,["Cmd+Shift+G"])
      ,(EditorOptions,["Cmd+,"]),(Conversation,["Cmd+Shift+C"]),(AgentNew,["Cmd+Shift+N"])]
    defaultsFor TerminalKeys=[(action,filter (/="Ctrl+Q") chords) | (action,chords)<-defaults,action `elem` [Close,Quit,Zoom,NextWindow,NextMessage,PreviousMessage,MakeTarget,CompileTarget,RunTarget] || case action of DebugCommand _->True; _->False]
    defaultsFor SidebarKeys=filter ((/=NextWindow).fst) defaults ++
      [(SidebarMove (-1),["Up"]),(SidebarMove 1,["Down"]),(SidebarMove (-10),["PageUp"]),(SidebarMove 10,["PageDown"]),
       (SidebarActivate,["Enter"]),(SidebarExpand,["Right"]),(SidebarCollapse,["Left"]),(FocusSource,["F6"])]
    defaultsFor MessagesKeys=filter (\(action,_)->action/=NextWindow && case action of DebugCommand _->False; _->True) defaults ++
      [(MessagesMove (-1),["Up"]),(MessagesMove 1,["Down"]),(MessagesPage (-1),["PageUp"]),(MessagesPage 1,["PageDown"]),(GoToMessage,["Enter"]),(FocusSource,["F6"])]
    defaultsFor ConversationKeys=filter (\(action,_)->action `notElem` [Copy,Cut,Paste,SelectAll,Redo,Conversation]) defaults ++
      [(Copy,["Ctrl+C","Ctrl+Shift+C","Ctrl+Insert"]),(Cut,["Ctrl+X","Ctrl+Shift+X","Shift+Delete"]),
       (Paste,["Ctrl+V","Ctrl+Shift+V","Shift+Insert"]),(SelectAll,["Ctrl+A","Ctrl+Shift+A"]),
       (Redo,["Ctrl+Y","Ctrl+Shift+Y","Ctrl+Shift+Z"])]
    defaultsFor _=defaults
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
