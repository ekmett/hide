-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.Commands
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Stable public identities for built-in actions, independent of menu placement.
--
-- Canonical names are intended for bindings and extension routing. Never derive
-- public names from Show or parse arbitrary constructor expressions. This catalog
-- is the first step toward registration, not a dynamic plugin registry or
-- authority boundary.
module Hide.Commands (BuiltinCommand(..), builtinCommands, commandIdentifier, contributedBindingCommands, platformBindings, configuredBindings) where

import Data.Text (Text)
import Control.Monad (unless, forM_)
import qualified Graphics.Vty as V
import Data.List (find,nub,subsequences)
import qualified Data.Map.Strict as M
import qualified Hide.Bindings
import Hide.Bindings (BindingPlatform(..), bindingPlatforms, platformName, BindingContext(..), bindingContexts, contextName, Bindings, compileBindings, readChord)
import Hide.Model (Desktop(..), Command(..), terminalSourceReserved, windowCycleChord, wordStarReserved, dialogBindingCommands, dialogInputKeys, dialogReserved, dialogFocusChord, dialogControlChord)
import qualified Hide.Plugin.Menu as Plugin
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
  [BuiltinCommand "hide.dialog.accept" DialogAccept
  ,BuiltinCommand "hide.dialog.cancel" DialogCancel
  ,BuiltinCommand "hide.dialog.focus-next" DialogFocusNext
  ,BuiltinCommand "hide.dialog.focus-previous" DialogFocusPrevious
  ,BuiltinCommand "hide.wordstar.block-prefix" WordStarBlockPrefix
  ,BuiltinCommand "hide.wordstar.quick-prefix" WordStarQuickPrefix
  ,BuiltinCommand "hide.selection.block-start" MarkBlockStart
  ,BuiltinCommand "hide.selection.block-end" MarkBlockEnd
  ,BuiltinCommand "hide.edit.delete-selection" DeleteSelection
  ,BuiltinCommand "hide.file.new" (New)
  ,BuiltinCommand "hide.file.open" (Open)
  ,BuiltinCommand "hide.file.download" (Download)
  ,BuiltinCommand "hide.file.export-buffer" (ExportBuffer)
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
  ,BuiltinCommand "hide.cursor.left" (CursorLeft False)
  ,BuiltinCommand "hide.cursor.right" (CursorRight False)
  ,BuiltinCommand "hide.selection.left" (CursorLeft True)
  ,BuiltinCommand "hide.selection.right" (CursorRight True)
  ,BuiltinCommand "hide.cursor.up" (CursorUp False)
  ,BuiltinCommand "hide.cursor.down" (CursorDown False)
  ,BuiltinCommand "hide.selection.up" (CursorUp True)
  ,BuiltinCommand "hide.selection.down" (CursorDown True)
  ,BuiltinCommand "hide.cursor.row-start" (CursorRowStart False)
  ,BuiltinCommand "hide.selection.row-start" (CursorRowStart True)
  ,BuiltinCommand "hide.cursor.row-end" (CursorRowEnd False)
  ,BuiltinCommand "hide.selection.row-end" (CursorRowEnd True)
  ,BuiltinCommand "hide.cursor.document-start" (CursorDocumentStart False)
  ,BuiltinCommand "hide.selection.document-start" (CursorDocumentStart True)
  ,BuiltinCommand "hide.cursor.document-end" (CursorDocumentEnd False)
  ,BuiltinCommand "hide.selection.document-end" (CursorDocumentEnd True)
  ,BuiltinCommand "hide.cursor.page-up" (CursorPageUp False)
  ,BuiltinCommand "hide.selection.page-up" (CursorPageUp True)
  ,BuiltinCommand "hide.cursor.page-down" (CursorPageDown False)
  ,BuiltinCommand "hide.selection.page-down" (CursorPageDown True)
  ,BuiltinCommand "hide.cursor.word-left" (CursorWordLeft False)
  ,BuiltinCommand "hide.cursor.word-right" (CursorWordRight False)
  ,BuiltinCommand "hide.selection.word-left" (CursorWordLeft True)
  ,BuiltinCommand "hide.selection.word-right" (CursorWordRight True)
  ,BuiltinCommand "hide.edit.delete-word-backward" DeleteWordBackward
  ,BuiltinCommand "hide.edit.delete-word-forward" DeleteWordForward
  ,BuiltinCommand "hide.edit.delete-backward" DeleteBackward
  ,BuiltinCommand "hide.edit.delete-forward" DeleteForward
  ,BuiltinCommand "hide.edit.delete-line" DeleteLine
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
  ,BuiltinCommand "hide.debug.assist" (DebugCommand "assist")
  ,BuiltinCommand "hide.debug.assist-pause" (DebugCommand "assist-pause")
  ,BuiltinCommand "hide.debug.assist-stop" (DebugCommand "assist-stop")
  ,BuiltinCommand "hide.debug.assist-reveal" (DebugCommand "assist-reveal")
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
  ,BuiltinCommand "hide.agents.model" AgentChoose
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
  ,BuiltinCommand "hide.window.previous" (PreviousWindow)
  ,BuiltinCommand "hide.view.current" (SetBufferView CurrentView)
  ,BuiltinCommand "hide.view.changes" (SetBufferView ChangesView)
  ,BuiltinCommand "hide.view.only-changes" (SetBufferView OnlyChangesView)
  ,BuiltinCommand "hide.view.side-by-side" (SetBufferView SideBySideView)
  ,BuiltinCommand "hide.view.markdown" (SetBufferView MarkdownView)
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

-- | Small exact contribution identities admitted by the existing menu host.
-- Built-in identities retain their canonical route, including Help/navigation.
-- Evaluating the catalogue forces its bounded spine and actor flags, so a worker
-- retains only exact refs and names. No callback or document payload is inspected.
contributedBindingCommands :: Desktop -> [(Text,Command)]
contributedBindingCommands d=foldr entry [] (contributedMenus d)
  where
    entry item rest
      | name `elem` map builtinIdentifier builtinCommands = rest
      | otherwise = rest `seq` allowed `seq` (name,RegisteredMenu reference allowed):rest
      where
        reference=Plugin.menuReference item
        name=Plugin.menuName reference
        allowed=Plugin.menuAgentAllowed item && reference `elem` agentMenuRefs d

-- | Compile every platform context outside the interaction path. Explicit global
-- entries apply to all owners; a context entry replaces the same global command.
-- Plain PTY control characters cannot be assigned to editor commands.
configuredBindings :: [(Text,Command)] -> M.Map Text (M.Map Text (M.Map Text [Text])) -> Either Text (M.Map (BindingPlatform,BindingContext) (Bindings Command))
configuredBindings catalogue configuration=do
  unless (all (`elem` map platformName bindingPlatforms) (M.keys configuration)) (Left "Unknown keybinding platform")
  M.unions <$> traverse (\platform->platformBindings catalogue platform (M.findWithDefault M.empty (platformName platform) configuration)) bindingPlatforms

platformBindings :: [(Text,Command)] -> BindingPlatform -> M.Map Text (M.Map Text [Text]) -> Either Text (M.Map (BindingPlatform,BindingContext) (Bindings Command))
platformBindings catalogue platform configuration=do
  unless (all (`elem` ("global":map contextName bindingContexts)) (M.keys configuration))
    (Left ("Unknown "<>platformName platform<>" keybinding context"))
  forM_ (concat (M.elems (M.findWithDefault M.empty "global" configuration))) readChord
  M.fromList <$> traverse prepare bindingContexts
  where
    prepare context=do
      let global=M.findWithDefault M.empty "global" configuration
          inherited=case context of
            TerminalKeys -> fmap (filter (not . processControlChord)) global
            DialogKeys -> fmap (filter (not . dialogChord)) $ M.filterWithKey (\name _->name `elem` [builtinIdentifier entry | entry<-builtinCommands,builtinAction entry `elem` dialogBindingCommands]) global
            owner | owner `elem` [WordStarKeys,WordStarBlockKeys,WordStarQuickKeys] -> fmap (filter (not . wordStarChord)) global
            _ -> global
          overrides=M.union (M.findWithDefault M.empty (contextName context) configuration) inherited
      forM_ [(name,raw) | (name,chords)<-M.toList overrides,raw<-chords] $ \(name,raw)->do
        (key,mods)<-readChord raw
        let character=case key of V.KChar _->not (any (`elem` mods) [V.MCtrl,V.MAlt,V.MMeta]) && not (context==DialogKeys && dialogFocusChord key mods); _->False
            contextReserved=case context of
              SidebarKeys -> character
              MessagesKeys -> character
              ConversationKeys -> character || key==V.KEnter
              TerminalKeys -> character || processControl key mods
              DialogKeys -> character
              WordStarBlockKeys -> False
              WordStarQuickKeys -> False
              _ -> character || key==V.KEnter
        let platformReserved = (platform==TerminalPlatform && V.MMeta `elem` mods) ||
              (platform==MacPlatform && ((V.MAlt `elem` mods && V.MCtrl `notElem` mods && V.MMeta `notElem` mods && not (context==DialogKeys && dialogFocusChord key mods) && case key of V.KChar _->True; _->False) || (key `elem` map V.KChar "h\\[]" && V.MMeta `elem` mods))) ||
              (platform/=TerminalPlatform && key `elem` map V.KChar "0+=-" && any (`elem` mods) [V.MCtrl,V.MAlt])
        unless (not (terminalSourceReserved key mods && not (context==DialogKeys && (dialogControlChord key mods || name `elem` inputNames && key `elem` map fst dialogInputKeys)) || contextReserved || platformReserved || context `elem` [WordStarKeys,WordStarBlockKeys,WordStarQuickKeys] && wordStarReserved key mods || context==DialogKeys && (dialogReserved key mods || windowCycleChord key mods)))
          (Left ("Reserved "<>contextName context<>" key: "<>raw))
      compiled<-either (Left . (("Keybinding context "<>contextName context<>": ")<>)) Right $ compileBindings ([(builtinIdentifier entry,builtinAction entry,keys context (builtinAction entry)) | entry<-builtinCommands,context/=DialogKeys || builtinAction entry `elem` dialogBindingCommands]++[(name,action,[]) | (name,action)<-catalogue,context/=DialogKeys,name `notElem` map builtinIdentifier builtinCommands]) overrides
      pure ((platform,context),compiled)
    -- Non-character source controls are prepared in this same table so an
    -- explicit prefix override/unbinding cannot reach a second fallback map.
    secondStrokes entries=[(action,maybe [] (\letter->[modifier<>letter | modifier<-["","Shift+","Ctrl+","Ctrl+Shift+"]]) (lookup action entries)++maybe [] id (lookup action controls)) | action<-nub (map fst entries++map fst controls)]
      where controls=[(action,filter nonCharacter chords) | (action,chords)<-defaultsFor WordStarKeys,any nonCharacter chords]
            nonCharacter raw=case readChord raw of Right (key,mods) | windowCycleChord key mods->True; Right (V.KChar _,_)->False; Right _->True; _->False
    processControl key mods=case key of
      V.KChar _ -> V.MCtrl `elem` mods && V.MAlt `notElem` mods && V.MMeta `notElem` mods && not (windowCycleChord key mods)
      _ -> False
    inputNames=[builtinIdentifier entry | entry<-builtinCommands,builtinAction entry `elem` map snd dialogInputKeys]
    dialogChord raw=case readChord raw of Right (key,mods)->dialogReserved key mods; _->False
    wordStarChord raw=case readChord raw of Right (key,mods)->wordStarReserved key mods; _->False
    processControlChord raw=case readChord raw of Right (key,mods)->processControl key mods; _->False
    keys context action=maybe [] id (lookup action (if platform==MacPlatform then macDefaults context else defaultsFor context))
    macDefaults context | context `elem` [WordStarBlockKeys,WordStarQuickKeys] = defaultsFor context
    macDefaults context = [(action,chords++maybe [] id (lookup action (defaultsFor context))) | (action,chords)<-macCommands,context/=DialogKeys || action `elem` dialogBindingCommands] ++ filter (\(action,_)->action `notElem` map fst macCommands) (defaultsFor context)
    macCommands =
      [(New,["Cmd+N"]),(Open,["Cmd+O"]),(Save,["Cmd+S"]),(SaveAs,["Cmd+Shift+S"]),(Close,["Cmd+W"]),(Quit,["Cmd+Q"])
      ,(Undo,["Cmd+Z"]),(Redo,["Cmd+Shift+Z"]),(Copy,["Cmd+C"]),(Cut,["Cmd+X"]),(Paste,["Cmd+V"]),(SelectAll,["Cmd+A"])
      ,(Find,["Cmd+F"]),(Replace,["Cmd+Alt+F"]),(FindNext,["Cmd+G"]),(FindPrevious,["Cmd+Shift+G"])
      ,(EditorOptions,["Cmd+,"]),(Conversation,["Cmd+Shift+C"]),(AgentNew,["Cmd+Shift+N"])]
    defaultsFor DialogKeys=[(DialogAccept,controlAliases V.KEnter),(DialogCancel,controlAliases V.KEsc),(DialogFocusNext,["Tab","Alt+Tab"]),(DialogFocusPrevious,["Shift+Tab","Alt+Shift+Tab"]),(Copy,["Ctrl+C","Ctrl+Shift+C"]),(Cut,["Ctrl+X","Ctrl+Shift+X"]),(Paste,["Ctrl+V","Ctrl+Shift+V"]),
      (SelectAll,["Ctrl+A","Ctrl+Shift+A"]),(Undo,["Ctrl+Z"]),(Redo,["Ctrl+Y","Ctrl+Shift+Z"]),
      (Find,["Ctrl+F"]),(Replace,["Ctrl+H","Ctrl+R"])]++[(cmd,controlAliases key) | (key,cmd)<-dialogInputKeys]
    defaultsFor WordStarBlockKeys=secondStrokes [(MarkBlockStart,"B"),(MarkBlockEnd,"K"),(Copy,"C"),(Cut,"V"),(DeleteSelection,"Y"),(Save,"S"),(Close,"D")]
    defaultsFor WordStarQuickKeys=secondStrokes [(CursorRowStart False,"S"),(CursorRowEnd False,"D"),(CursorDocumentStart False,"R"),(CursorDocumentEnd False,"C"),(Find,"F"),(Replace,"A")]
    defaultsFor WordStarKeys=[(action,filter named chords) | (action,chords)<-defaults] ++
      [(action,chords++maybe [] id (lookup action [(CursorLeft False,["Ctrl+S","Ctrl+Shift+S"]),(CursorRight False,["Ctrl+D","Ctrl+Shift+D"]),(CursorUp False,["Ctrl+E","Ctrl+Shift+E"]),(CursorDown False,["Ctrl+X","Ctrl+Shift+X"]),
        (CursorWordLeft False,["Ctrl+A","Ctrl+Shift+A","Ctrl+Alt+A","Ctrl+Alt+Shift+A"]),(CursorWordRight False,["Ctrl+F","Ctrl+Shift+F"])])) | (action,chords)<-navigationDefaults] ++
      [(DeleteLine,["Ctrl+Y","Ctrl+Shift+Y","Ctrl+Alt+Y","Ctrl+Alt+Shift+Y"]),(WordStarBlockPrefix,["Ctrl+K","Ctrl+Shift+K"]),(WordStarQuickPrefix,["Ctrl+Q","Ctrl+Shift+Q"])]
      where named raw=case readChord raw of
              Right (key,mods) | windowCycleChord key mods -> True
              Right (key,mods) | wordStarReserved key mods -> False
              Right (V.KChar c,mods) | V.MCtrl `elem` mods ->
                c==' ' || c=='z' || V.MShift `elem` mods && c `elem` ("lcn"::String)
              _ -> True
    defaultsFor TerminalKeys=[(action,filter (/="Ctrl+Q") chords) | (action,chords)<-defaults,action `elem` [Close,Quit,Zoom,NextWindow,PreviousWindow,NextMessage,PreviousMessage,MakeTarget,CompileTarget,RunTarget] || case action of DebugCommand _->True; _->False]
    defaultsFor SidebarKeys=withoutWindowF6 defaults ++
      [(SidebarMove (-1),["Up"]),(SidebarMove 1,["Down"]),(SidebarMove (-10),["PageUp"]),(SidebarMove 10,["PageDown"]),
       (SidebarActivate,["Enter"]),(SidebarExpand,["Right"]),(SidebarCollapse,["Left"]),(FocusSource,["F6"])]
    defaultsFor MessagesKeys=withoutWindowF6 (filter (\(action,_)->case action of DebugCommand _->False; _->True) defaults) ++
      [(MessagesMove (-1),["Up"]),(MessagesMove 1,["Down"]),(MessagesPage (-1),["PageUp"]),(MessagesPage 1,["PageDown"]),(GoToMessage,["Enter"]),(FocusSource,["F6"])]
    defaultsFor ConversationKeys=filter (\(action,_)->action `notElem` [Copy,Cut,Paste,SelectAll,Redo,Conversation]) defaults ++
      [(Copy,["Ctrl+C","Ctrl+Shift+C","Ctrl+Insert"]),(Cut,["Ctrl+X","Ctrl+Shift+X","Shift+Delete"]),
       (Paste,["Ctrl+V","Ctrl+Shift+V","Shift+Insert"]),(SelectAll,["Ctrl+A","Ctrl+Shift+A"]),
       (Redo,["Ctrl+Y","Ctrl+Shift+Y","Ctrl+Shift+Z"])]
    defaultsFor SourceKeys=defaults++navigationDefaults
    defaultsFor DebuggerKeys=defaults++navigationDefaults
    withoutWindowF6=map (\(action,chords)->(action,if action==NextWindow then filter (/="F6") chords else chords))
    navigationDefaults=horizontalDefaults++edgeDefaults++wordDefaults++
      [(CursorUp False,verticalAliases V.KUp False),(CursorDown False,verticalAliases V.KDown False),
       (CursorUp True,verticalAliases V.KUp True),(CursorDown True,verticalAliases V.KDown True)]
    edgeDefaults=[(CursorRowStart False,keyAliases V.KHome False False),
      (CursorRowStart True,keyAliases V.KHome True False),
      (CursorRowEnd False,keyAliases V.KEnd False False),
      (CursorRowEnd True,keyAliases V.KEnd True False),
      (CursorDocumentStart False,keyAliases V.KHome False True),
      (CursorDocumentStart True,keyAliases V.KHome True True),
      (CursorDocumentEnd False,keyAliases V.KEnd False True),
      (CursorDocumentEnd True,keyAliases V.KEnd True True),
      (CursorPageUp False,verticalAliases V.KPageUp False),
      (CursorPageUp True,verticalAliases V.KPageUp True),
      (CursorPageDown False,verticalAliases V.KPageDown False),
      (CursorPageDown True,verticalAliases V.KPageDown True)]
    wordDefaults=[(CursorWordLeft False,keyAliases V.KLeft False True),(CursorWordRight False,keyAliases V.KRight False True),
      (CursorWordLeft True,keyAliases V.KLeft True True),(CursorWordRight True,keyAliases V.KRight True True),
      (DeleteWordBackward,keyAliases V.KBS False True++keyAliases V.KBS True True),
      (DeleteWordForward,keyAliases V.KDel False True++keyAliases V.KDel True True)]
    controlAliases key=[name | mods<-subsequences [V.MCtrl,V.MShift,V.MAlt,V.MMeta],
      platform/=TerminalPlatform || V.MMeta `notElem` mods,Just name<-[Hide.Bindings.chordName key mods]]
    verticalAliases key shift=aliases key shift++keyAliases key shift True
    horizontalDefaults=
      [(CursorLeft False,aliases V.KLeft False),(CursorRight False,aliases V.KRight False),
       (CursorLeft True,aliases V.KLeft True),(CursorRight True,aliases V.KRight True),
       (DeleteBackward,aliases V.KBS False++aliases V.KBS True),
       (DeleteForward,aliases V.KDel False++filter (/="Shift+Delete") (aliases V.KDel True))]
    aliases key shift=keyAliases key shift False
    keyAliases key shift ctrl=[name | mods<-[[V.MCtrl | ctrl]++[V.MShift | shift]++extra | extra<-[[],[V.MAlt],[V.MMeta],[V.MMeta,V.MAlt]]],
      platform/=TerminalPlatform || V.MMeta `notElem` mods,not (terminalSourceReserved key mods),Just name<-[Hide.Bindings.chordName key mods]]
    defaults=
      [(New,["Ctrl+N"]),(Open,["F3","Ctrl+O"]),(Save,["F2","Ctrl+S"])
      ,(Close,["Alt+F3"]),(Quit,["Alt+X","Ctrl+Q"])
      ,(Undo,["Ctrl+Z"]),(Redo,["Ctrl+Shift+Z","Ctrl+Y"])
      ,(Copy,["Ctrl+C","Ctrl+Insert"]),(Cut,["Ctrl+X","Shift+Delete"])
      ,(Paste,["Ctrl+V","Shift+Insert"]),(SelectAll,["Ctrl+A"])
      ,(Find,["Ctrl+F"]),(Replace,["Ctrl+H","Ctrl+R"])
      ,(FindNext,["Ctrl+L"]),(FindPrevious,["Ctrl+Shift+L"]),(GoTo,["Ctrl+G"])
      ,(Help,["F1"]),(InspectType,["Shift+F1"]),(Definition,["F12"])
      ,(Complete,["Ctrl+Space"]),(Zoom,["F5"]),(NextWindow,["F6","Ctrl+Tab"]),(PreviousWindow,["Ctrl+Shift+Tab"])
      ,(ToggleTree,["Ctrl+B"]),(Conversation,["Ctrl+Shift+C"]),(AgentNew,["Ctrl+Shift+N"])
      ,(NextMessage,["Alt+F8"]),(PreviousMessage,["Alt+F7"])
      ,(MakeTarget,["F9"]),(CompileTarget,["Alt+F9"]),(RunTarget,["Ctrl+F9"])
      ,(DebugCommand "continue",["F4"]),(DebugCommand "stepIn",["F7"])
      ,(DebugCommand "next",["F8"]),(DebugCommand "stepOut",["Ctrl+F7"])
      ,(DebugCommand "breakpoint",["Ctrl+F8"])]
