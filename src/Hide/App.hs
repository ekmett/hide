-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE CPP, OverloadedStrings #-}
-- |
-- Module      : Hide.App
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : CPP, OverloadedStrings
--
-- Command-line entry point and composition root for editor sessions.
--
-- Interactive frontends attach to a persistent session; snapshots and the MCP
-- bridge take separate startup paths. Nested resource scopes own language tools,
-- conversations, debugging, reconciliation and highlighting. Effect handlers form
-- a delegation chain, while the session tick adopts their completed work.
-- Permission checks wrap agent-facing dispatch rather than individual frontends.
module Hide.App (main, demoDesktop, applyEffects) where

import Hide.Sidebar
import Hide.PackageSidebar
import qualified Hide.Plugin.Session as Plugin
import qualified Hide.AgentDirectoryHost as AgentDirectory
import Hide.SessionSidebar
import Hide.SidebarCommands
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Hide.DocumentationHost
import qualified Hide.Plugin.Services as PluginServices
import qualified Hide.Plugin.Request as PluginRequest
import Hide.GuestAccess (validateGuestEffects)
import Hide.Autocomplete
import Hide.BufferView
import Hide.Defaults
import qualified Hide.Plugin.Menu as PluginMenu
import Hide.TextPresentation
import qualified Hide.Plugin.EditorHost as Editor
import qualified Hide.Plugin.Form as Form
import Hide.PluginWindowHost (tickPluginWindows,retireClosedWindow)
import Hide.MenuCommands
import Hide.Keybindings
import Hide.Commands (configuredBindings, contributedBindingCommands)
import Hide.SystemOne (withSystemOne,selectDecisionProvider,systemOneServices)
import Hide.SystemOneConfig (loadSystemOneProvider)
import Hide.SystemOneBrowser (withSystemOneBrowser)
import Hide.SystemOneMenu (withSystemOneMenu)
import Hide.MCPPermissions
import Hide.ClipboardMCP
import Hide.Links (followLink)
import Hide.Environment
import Hide.ControlMCP
import Control.Exception (evaluate, finally, catch, AsyncException(UserInterrupt), Exception, throwIO)
import Control.Concurrent (threadDelay)
#ifndef mingw32_HOST_OS
import Control.Concurrent (myThreadId,throwTo)
import Control.Exception (bracket)
import System.Posix.Signals (installHandler, Handler(Catch), sigTERM, sigHUP)
#endif
import Data.Aeson (Value(..), object, (.=), withObject, (.:), (.:?), (.!=))
import Hide.Protocol (WirePacket(..))
import Data.Aeson.Types (parseEither, parseMaybe, Parser)
import qualified Hide.Font as Font
import Hide.ScreenCapture (capture, screenTool)
import Hide.TestsMCP
import Hide.WorkspaceFilesMCP
import Hide.HistoryMCP
import Hide.RuntimeMCP
import qualified Hide.Build as Build
import Hide.WorkspaceMCP
import Hide.ProjectBrowser
import qualified Hide.AgentRuntime as AR
import qualified Hide.AgentHub as AH
import Hide.AgentAccess (resolveAgentAccess,resolveActiveAgentAccess)
import Hide.AgentServicesHost (agentServices)
import qualified Hide.Plugin.Tool as PluginTool
import Hide.BufferRequest (bufferRequestServices)
import Hide.EditorMCP (runEditorMCP, openEditorFiles, editorResponseOnly, rpcError, editorResponseWith, debugTools, builtinTools, builtinTool)
import Hide.RemoteEndpoint (sessionEndpoint)
import Hide.Session
import Hide.Completion (bashCompletion)
import Hide.RemoteTerminal (runRemoteTerminal)
import Text.Read (readMaybe)
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad (foldM, when, unless)
import System.IO (hPutStrLn, stderr, hFlush, stdout, stdin, hIsTerminalDevice)
import Hide.Debugger
import Hide.DebuggerSidebar
import Hide.Conversation
import Hide.SessionServices hiding (sessionDirectory)
import qualified Hide.LSP as L
import Hide.Tooling
import Hide.GitOperations
import qualified Data.Map.Strict as M
import Data.List (find, isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Graphics.Vty as V
import System.Console.GetOpt
import System.Directory (XdgDirectory(..), getXdgDirectory, canonicalizePath, doesDirectoryExist, doesFileExist, getCurrentDirectory, getHomeDirectory, setCurrentDirectory, listDirectory)
import System.FilePath ((</>), takeDirectory, takeExtension, equalFilePath)
import Control.Exception (try, IOException)
import Paths_hide (getDataFileName)
import Hide.Browser
import Hide.Git
import System.Environment (getArgs, lookupEnv, setEnv)
import Hide.Frontend
import Hide.Remote
import Hide.RemoteWindow (runRemoteWindow)
import Hide.RemoteWeb (runRemoteWeb)
import System.Exit (die)
import System.Timeout (timeout)
import Hide.Buffer
import Hide.Highlighting (withHighlighting,tickHighlighting)
import Hide.Model
import Hide.Render
import Hide.Files
import Hide.Reconcile

data Option = Daemon | NewSession | Sessions | MCPBridge String | Resume (Maybe String) | SSH String | RemoteSession String | RemoteDaemon String | RequireCheckpoint | Use Backend | Scale String | Size String | Mode String | ColorMode String | Demo | CRT | NoCRT | MaterialIcons | ClassicIcons | WordStar | StandardKeys | CursorBlink Bool | Pixelate Bool | Streamer Bool | Snapshot | Html | Scene String | Usage deriving Eq
options :: [OptDescr Option]
options = [Option [] ["appearance"] (ReqArg ColorMode "light|dark|system") "Document/terminal colors (default THC_EDIT_APPEARANCE or system)"
          ,Option [] ["metal"] (NoArg (Use Metal)) "Open a Metal window"
          ,Option [] ["vulkan"] (NoArg (Use Vulkan)) "Open a Vulkan window"
          ,Option [] ["remote"] (NoArg (Use Remote)) "Serve the remote editing protocol on stdin/stdout"
          ,Option [] ["ssh"] (ReqArg SSH "HOST") "Connect to a remote hide (also HOST:PATH)"
          ,Option [] ["daemon"] (NoArg Daemon) "Start a session without a display; print its ID when ready"
          ,Option [] ["new-session"] (NoArg NewSession) "Start a separate editor even inside an embedded terminal"
          ,Option [] ["sessions"] (NoArg Sessions) "List running, recoverable and remote sessions"
          ,Option [] ["resume"] (OptArg Resume "ID") "Resume an unfinished editor session (choose if several exist)"
          ,Option [] ["remote-session"] (ReqArg RemoteSession "ID") "Reattach to an existing remote session"
          ,Option [] ["web"] (NoArg (Use Web)) "Open the local WebGL browser frontend"
          ,Option [] ["window"] (NoArg (Use Auto)) "Open a window using the platform backend"
          ,Option [] ["terminal"] (NoArg (Use Terminal)) "Use the terminal (override THC_EDIT_BACKEND)"
          ,Option [] ["crt"] (NoArg CRT) "Enable CRT scanlines and vignetting (window only)"
          ,Option [] ["streamer"] (NoArg (Streamer True)) "Hide sensitive fields and session keys on screen"
          ,Option [] ["no-streamer"] (NoArg (Streamer False)) "Show sensitive fields on the human display"
          ,Option [] ["no-crt"] (NoArg NoCRT) "Disable CRT filtering"
          ,Option [] ["classic-icons"] (NoArg ClassicIcons) "Use standard folder icons"
          ,Option [] ["standard-keys"] (NoArg StandardKeys) "Use standard editing keys"
          ,Option [] ["blink-cursor"] (NoArg (CursorBlink True)) "Blink the editing cursor"
          ,Option [] ["no-blink-cursor"] (NoArg (CursorBlink False)) "Keep the editing cursor steady"
          ,Option [] ["pixelate-unicode"] (NoArg (Pixelate True)) "Pixelate Unicode glyphs"
          ,Option [] ["no-pixelate-unicode"] (NoArg (Pixelate False)) "Render Unicode glyphs smoothly"
          ,Option [] ["material-icons"] (NoArg MaterialIcons) "Use Material folder icons (terminal requires a compatible Nerd Font)"
          ,Option [] ["scale"] (ReqArg Scale "FACTOR") "Window pixel scale, 1 to 8 in 1/8 steps (default THC_EDIT_SCALE or display density)"
          ,Option [] ["mode"] (ReqArg Mode "NUMBER") "Window screen mode: 3 (80x25), 259 (80x50); default 3"
          ,Option [] ["vga50"] (NoArg (Mode "259")) "Alias for --mode 259 (window only)"
          ,Option [] ["size"] (ReqArg Size "COLSxROWS") "Initial character dimensions (override --mode dimensions)"
          ,Option [] ["demo"] (NoArg Demo) "Open a sample Haskell buffer"
          ,Option [] ["wordstar"] (NoArg WordStar) "Use WordStar editing keys"
          ,Option [] ["snapshot"] (NoArg Snapshot) "Print an 80x25 text snapshot and exit"
          ,Option [] ["snapshot-html"] (NoArg Html) "Print an HTML preview of actual Vty output and exit"
          ,Option [] ["scene"] (ReqArg Scene "desktop|menu|about|gallery|split|open|tree|help|diff|preferences") "Preview scene (with --demo)"
          ,Option ['h'] ["help"] (NoArg Usage) "Show help"]

-- | Compose linked plugins, parse launch options and enter a frontend, session
-- daemon, snapshot or MCP bridge. The executable selects the plugin list.
main :: [Plugin.Plugin] -> IO ()
main plugins = do
  args<-getArgs
  case args of
    "--bash-completion":request -> bashCompletion options request >>= mapM_ putStrLn
    _ -> runEditor plugins args

runEditor :: [Plugin.Plugin] -> [String] -> IO ()
runEditor plugins args = do
  presenter<-case [value | plugin<-plugins,Just value<-[Plugin.pluginConversation plugin]] of
    []->pure Nothing
    [value]->pure (Just value)
    _->die "More than one plugin contributes conversation presentation."
  primaryInput<-case [value | plugin<-plugins,Just value<-[Plugin.pluginPrimaryInput plugin]] of
    []->pure Nothing
    [value]->pure (Just value)
    _->die "More than one plugin contributes primary conversation input."
  childInput<-case [value | plugin<-plugins,Just value<-[Plugin.pluginChildInput plugin]] of
    []->pure Nothing
    [value]->pure (Just value)
    _->die "More than one plugin contributes child conversation input."
  agentProvider<-case [value | plugin<-plugins,Just value<-[Plugin.pluginAgentProvider plugin]] of
    []->pure Nothing
    [value]->pure (Just value)
    _->die "More than one plugin contributes an agent provider."
  completionProvider<-case [value | plugin<-plugins,Just value<-[Plugin.pluginCompletionProvider plugin]] of
    []->pure Nothing
    [value]->pure (Just value)
    _->die "More than one plugin contributes an ACP completion provider."
  completionInput<-case [value | plugin<-plugins,Just value<-[Plugin.pluginCompletionInput plugin]] of
    []->pure Nothing
    [value]->pure (Just value)
    _->die "More than one plugin contributes completion input."
  parentSession<-lookupEnv "THC_EDIT_SESSION"
  backendEnvironment<-lookupEnv "THC_EDIT_BACKEND"
  scaleEnvironment<-lookupEnv "THC_EDIT_SCALE"
  appearanceEnvironment<-lookupEnv "THC_EDIT_APPEARANCE"
  terminalColors<-lookupEnv "COLORFGBG"
  let (flags,paths,errors)=getOpt Permute (options++[Option [] ["mcp-editor"] (ReqArg MCPBridge "ID") "Internal editor introspection bridge",Option [] ["remote-daemon"] (ReqArg RemoteDaemon "ID") "Internal remote session process",Option [] ["require-checkpoint"] (NoArg RequireCheckpoint) "Internal recovery precondition"]) (resumeArguments args)
  if not (null errors) then die (concat errors)
  else if Usage `elem` flags then putStr (usageInfo "Usage: hide [OPTIONS] [--] [FILE.hs ...]\n\nHaskell source editor.\nF2 Save, F3 Open, F10 Menu, Alt+X Exit.\n" options)
  else if [ident | MCPBridge ident<-flags]/=[] then case flags of
    [MCPBridge ident] | null paths -> runEditorMCP ident
    _ -> die "--mcp-editor accepts only a session ID."
  else if Sessions `elem` flags then
    if flags==[Sessions] && null paths then printSessions else die "--sessions does not accept other options or paths."
  else if null flags && not (null paths) && all ((==Nothing) . parseRemoteTarget) paths && parentSession/=Nothing then
    openEditorFiles (fromMaybe "" parentSession) paths `catch` (\(err::IOException)->
      die ("Parent editor: "++show err++"\nUse --new-session to start a separate editor."))
  else do
    when (Daemon `elem` flags && (Use Remote `elem` flags || any isDaemon flags || Snapshot `elem` flags || Html `elem` flags)) $
      die "--daemon cannot be combined with --remote, --remote-daemon, or snapshots."
    configBase<-case paths of
      path:_ | not (any isSSH flags), parseRemoteTarget path==Nothing -> expandRemoteHome path
      _ -> getCurrentDirectory
    loadEnvironment configBase >>= either (die . T.unpack) pure
    defaultsJSON<-readEditorDefaultsFor configBase >>= either (die . T.unpack) pure
    defaults<-either die pure (parseEither parseDefaults defaultsJSON)
    keys<-readKeybindingsFor configBase >>= either (die . T.unpack) pure
    let backendDefault=backendEnvironment <|> defaultBackend defaults
        scaleDefault=scaleEnvironment <|> (show <$> defaultScale defaults)
        appearanceDefault=appearanceEnvironment <|> defaultAppearance defaults
        flagBool yes no fallback=fromMaybe fallback (lastMaybe [value | flag<-flags, Just value<-[if flag==yes then Just True else if flag==no then Just False else Nothing]])
    daemon <- case [sid | RemoteDaemon sid<-flags] of []->pure Nothing; [sid]->pure (Just sid); _->die "Specify --remote-daemon once."
    when (RequireCheckpoint `elem` flags && daemon==Nothing) (die "--require-checkpoint requires --remote-daemon.")
    resume <- case [ident | Resume ident<-flags] of
      [] -> pure Nothing
      [ident] -> do
        when (not (null paths) || any isSSH flags || any isSession flags || daemon/=Nothing || Use Remote `elem` flags || Snapshot `elem` flags || Html `elem` flags) (die "--resume cannot be combined with paths, --ssh, --remote, or snapshots.")
        Just <$> chooseSession ident
      _ -> die "Specify --resume only once."
    let serving=Use Remote `elem` flags || daemon/=Nothing || (Daemon `notElem` flags && null [b | Use b<-flags] && backendDefault==Just "remote")
    when (resume/=Nothing && serving) (die "--resume requires a display backend, not --remote.")
    target <- if serving then pure Nothing else case ([host | SSH host<-flags],paths) of
      ([],[path]) | Just remote<-parseRemoteTarget path -> pure (Just remote)
      ([],_) | any (maybe False (const True) . parseRemoteTarget) paths -> die "Open one remote project at a time; do not mix local and remote paths."
             | otherwise -> pure Nothing
      ([host],[]) -> pure (Just (host,"."))
      ([host],[path]) -> pure (Just (host,path))
      _ -> die "Specify one SSH host and one remote file or project path."
    backend <- if daemon/=Nothing then pure Remote else if Daemon `elem` flags then pure Terminal else either die pure (chooseBackend backendDefault [b | Use b<-flags])
    when (target/=Nothing && (Snapshot `elem` flags || Html `elem` flags)) (die "Snapshots require local paths.")
    when (length [sid | RemoteSession sid<-flags]>1) (die "Specify --remote-session once.")
    when (target==Nothing && any isSession flags) (die "--remote-session requires HOST:PATH or --ssh HOST.")
    when (serving && (Snapshot `elem` flags || Html `elem` flags || any isSSH flags)) (die "--remote cannot be combined with snapshots or --ssh.")
    colorMode <- case [s | ColorMode s<-flags] of
      [] -> parseAppearance (maybe "system" id appearanceDefault)
      [s] -> parseAppearance s
      _ -> die "Specify --appearance only once."
    screenMode <- case [s | Mode s <- flags] of
      [] -> pure (fromMaybe 3 (defaultScreenMode defaults))
      [s] -> either die pure (parseScreenMode s)
      _ -> die "Specify --mode or --vga50 only once."
    when (Daemon `notElem` flags && backend == Terminal && any isMode flags && Snapshot `notElem` flags && Html `notElem` flags) $
      die "--mode/--vga50 requires --window, --metal or --vulkan; terminal size is controlled by your terminal."
    scale <- either die pure (chooseScale scaleDefault [s | Scale s <- flags])
    dimensions <- case [s | Size s <- flags] of
      [] -> let (cols,rows)=modeSize screenMode in pure (if any isMode flags then (cols,rows) else (fromMaybe cols (defaultColumns defaults),fromMaybe rows (defaultRows defaults)))
      [s] -> either die pure (parseWindowSize s)
      _ -> die "Specify --size only once."
    if backend==Remote && daemon==Nothing then runRemoteRelay (remoteArguments flags paths)
    else if daemon==Nothing && Snapshot `notElem` flags && Html `notElem` flags then do
#ifndef WITH_WINDOW
      when (backend `elem` [Auto,Metal,Vulkan]) (die "Graphical support is not built. Rebuild with -fwindow.")
#endif
#ifndef WITH_WEB
      when (backend==Web) (die "Browser support is not built. Rebuild with -fweb.")
#endif
      record <- case resume of
        Just saved -> pure saved
        Nothing -> do
          let host=fmap fst target
              arguments=remoteArguments (filter (not . isSession) flags) (maybe paths (pure . snd) target)
          fresh<-newSessionRecord host arguments
          pure $ case [ident | RemoteSession ident<-flags] of
            [ident] -> fresh {sessionId=ident}
            _ -> fresh
      wasInterrupted<-newIORef False
      displayedSession<-newIORef (sessionId record)
      let reattach=resume/=Nothing || any isSession flags
          attach=case sessionHost record of
            Nothing -> withLocalPeer (sessionId record) reattach (sessionArguments record)
            Just host -> withSSHSession host (sessionId record) reattach (sessionArguments record)
          interruptedDisplay peer = display peer `catch` (\err -> case err of
            UserInterrupt -> do
              writeIORef wasInterrupted True
              suspendSession peer
            other -> throwIO (other :: AsyncException))
          display peer | Daemon `elem` flags = awaitSessionReady peer
          display peer = do
            peerSend peer (JsonPacket (object ["type" .= ("frontend"::T.Text),"mode" .= (if backend==Terminal then Nothing else Just screenMode)]))
            case backend of
              Terminal -> runRemoteTerminal peer
              Web -> runRemoteWeb scale (maybe "" id (sessionHost record)) peer
              _ -> runRemoteWindow backend scale dimensions screenMode (maybe "" id (sessionHost record)) peer
          report = do
            ident<-readIORef displayedSession
            saved<-loadSession ident
            detached<-readIORef wasInterrupted
            when (saved/=Nothing || detached) $ putStrLn ("Session: "++ident++"\nResume: hide --resume "++ident) >> hFlush stdout
      -- A local session starts beside the project that created it. Reattachment
      -- needs only its endpoint, so a removed/renamed working directory is fine.
      (withDetachSignals (do
          attach (\peer->interruptedDisplay peer `finally` (peerSession peer >>= writeIORef displayedSession))
          stopped <- readIORef wasInterrupted
          when (Daemon `elem` flags && not stopped) (awaitSessionDetached record))
        `catch` (\err -> writeIORef wasInterrupted True >> interrupted err)
        `catch` (\FrontendDetached -> writeIORef wasInterrupted True)) `finally` report
    else withSidebarCommands $ \sidebarHost -> do
        cwd<-getCurrentDirectory >>= canonicalizePath
        initialKeymap<-if Snapshot `elem` flags || Html `elem` flags then either (die . T.unpack) pure (configuredBindings [] keys) else pure M.empty
        let initial=if Demo `elem` flags then addDocument Nothing (newBuffer (activeText demoDesktop)) (initialDesktop dimensions) else initialDesktop dimensions
            configured=(fst (handleEvent (uncurry V.EvResize dimensions) initial)) {launchDirectory=cwd,keyBindings=initialKeymap,wideSectionTitles=fromMaybe False (defaultWideSectionTitles defaults),macKeySymbols=fromMaybe False (defaultMacKeySymbols defaults),hapticFeedback=fromMaybe False (defaultHapticFeedback defaults),defaultBufferView=fromMaybe CurrentView (defaultView defaults),chatSubmit=fromMaybe QuerySubmit (defaultChatSubmit defaults),appearance=colorMode,systemDark=maybe True (not . (`elem` ["7","15"]) . reverse . takeWhile (/=';') . reverse) terminalColors,wordStar=flagBool WordStar StandardKeys (fromMaybe (wordStar initial) (defaultWordStar defaults)),crtFilter=flagBool CRT NoCRT (fromMaybe (crtFilter initial) (defaultCRT defaults)),materialIcons=flagBool MaterialIcons ClassicIcons (fromMaybe (materialIcons initial) (defaultMaterialIcons defaults)),blinkCursor=fromMaybe (fromMaybe (blinkCursor initial) (defaultBlinkCursor defaults)) (lastMaybe [value | CursorBlink value<-flags]),pixelateUnicode=fromMaybe (fromMaybe (pixelateUnicode initial) (defaultPixelateUnicode defaults)) (lastMaybe [value | Pixelate value<-flags]),streamerMode=fromMaybe (fromMaybe False (defaultStreamerMode defaults)) (lastMaybe [value | Streamer value<-flags]),videoMode=if backend == Terminal then Nothing else Just screenMode}
        localPaths<-if daemon/=Nothing then mapM expandRemoteHome paths else pure paths
        loaded<-foldM (\current path->snd <$> sidebarEffects sidebarHost applyEffects current [OpenFile PluginMenu.HumanMenu path] >>= awaitFileOpening sidebarHost) configured localPaths
        base<-packageDirectory cwd
        (_,browsing)<-if sideTree loaded/=Nothing || Demo `elem` flags || Snapshot `elem` flags || Html `elem` flags then pure (False,loaded)
          else applyEffects loaded [if null paths then ReadPath base else ReadTree base]
        let focused=browsing {sideTree=fmap (\tree -> tree {treeFocused=null (windows browsing)}) (sideTree browsing)}
        (_,withGit)<-applyEffects focused [RefreshGit (startingDirectory focused)]
        staged<-foldM stageScene withGit [scene | Scene scene<-flags]
        configPath<-permissionConfigPath
        localConfigPath<-projectConfigPath configBase
        agentDirectory<-getXdgDirectory XdgConfig "thc-edit"
        endpoints<-maybe (pure []) (\sid -> do endpoint<-sessionEndpoint sid; pure [takeDirectory endpoint]) daemon
        sessionStore<-sessionStoreDirectory
        privatePaths<-mapM canonicalizePath ([configPath,localConfigPath,sessionStore,agentDirectory </> "agents.json",agentDirectory </> "agent-session.json"]++endpoints)
        protectedDesktop<-initializeSidebar sidebarHost staged {guestPrivatePaths=privatePaths}
        if Html `elem` flags then prepareTextPresentations protectedDesktop {buffers=M.map highlightDocument (buffers protectedDesktop)} >>= TIO.putStr . snapshotHtml
        else if Snapshot `elem` flags then prepareTextPresentations protectedDesktop {buffers=M.map highlightDocument (buffers protectedDesktop)} >>= TIO.putStr . snapshot
        else do
          mapM_ (setEnv "THC_EDIT_SESSION") daemon
          font<-Font.loadFont
          let specs=builtinTools++debugTools++toolingTools++workspaceTools++fileTools++testsTools++historyTools++runtimeTools++gitTools++controlTools++clipboardTools++[screenTool]
          let names definitions=[name | spec<-definitions,Just name<-[parseMaybe (withObject "tool" (.: "name")) spec]]
              declarations=concatMap Plugin.pluginTools plugins
          systemOneJSON<-readSystemOne >>= either (die . T.unpack) pure
          systemOneProvider<-loadSystemOneProvider systemOneJSON >>= either (die . T.unpack) pure
          withSystemOne $ \systemOne -> withSystemOneBrowser $ \systemOneBrowser -> do
            _<-selectDecisionProvider systemOne systemOneProvider >>= either (die . show) pure
            PluginTool.withTools (names specs) [tool | Plugin.EditorTool tool<-declarations] $ \editorToolset ->
              PluginTool.withTools (names (specs++PluginTool.toolDefinitions editorToolset)) [tool | Plugin.RequestTool tool<-declarations] $ \requestToolset ->
              PluginTool.withTools (names (specs++PluginTool.toolDefinitions editorToolset++PluginTool.toolDefinitions requestToolset)) [tool | Plugin.CoordinationTool tool<-declarations] $ \agentToolset -> withPermissions (specs++PluginTool.toolDefinitions editorToolset++PluginTool.toolDefinitions requestToolset++PluginTool.toolDefinitions agentToolset) $ \permissions -> withDocsCommands $ \docsCommands -> withEnvironmentCommands $ \environmentCommands -> withSessionSidebar sidebarHost daemon protectedDesktop $ \sessionSidebar -> withSessionServices $ \services -> withConversationAt agentProvider presenter primaryInput childInput (sessionConsoles services) (startingDirectory protectedDesktop) $ \conversation -> withConversationMenuCommands docsCommands conversation $ \menuHost -> withSystemOneMenu menuHost sidebarHost systemOne systemOneProvider systemOneBrowser $ withDebuggerConsoles (sessionConsoles services) $ \debugger -> withDownloadsCommands menuHost debugger $ withDebuggerSidebar sidebarHost debugger $ \debugSidebar -> withTooling L.startClient $ \tooling -> withGitOperations (buildTerminalLaunchPending services) $ \gitOperations -> withReconciliation $ \reconciliation -> withProjectBrowser $ \projectBrowser -> withHighlighting $ \highlighting -> withAutocomplete completionProvider [tool | Plugin.CompletionTool tool<-declarations] completionInput (startingDirectory protectedDesktop) $ \autocomplete -> withPackageSidebar sidebarHost protectedDesktop $ \packageSidebar -> Plugin.withPlugins plugins (Plugin.Session (sidebarCapabilities sidebarHost) (AgentDirectory.agentDirectory (AR.agentHub (conversationAgents conversation)) autocomplete) SidebarAgent (menuSidebarCapabilities menuHost sidebarHost) sidebarConversation SidebarConversation sidebarConversationOperation PluginMenu.cancelConversationAction sidebarSelectedAgent (systemOneServices systemOne)) $ do
              contributions<-PluginMenu.menuSnapshot (menuContributions menuHost)
              let agentTools=PluginTool.toolDefinitions agentToolset
                  editorSpecs=specs++PluginTool.toolDefinitions editorToolset++PluginTool.toolDefinitions requestToolset
                  liveBase=protectedDesktop {contributedMenus=contributions,agentMenuRefs=menuAgentReferences menuHost,menusActive=True}
              keymap<-either (die . T.unpack) pure (configuredBindings (contributedBindingCommands liveBase) keys)
              let liveDesktop=liveBase {keyBindings=keymap}
              withKeybindings keys (contributedBindingCommands liveBase) $ \keybindings -> withTextPresentation $ \textPresentation -> do
                exiting<-newIORef False
                let runtimeEffects=textPresentationEffects textPresentation $ sidebarEffects sidebarHost (packageBuildEffects packageSidebar (sessionSidebarEffects sessionSidebar (menuEffects menuHost (keybindingEffects keybindings (autocompleteEffects autocomplete (projectBrowserEffects projectBrowser (gitOperationEffects gitOperations (debuggerEffects debugger (conversationEffects conversation (sessionEffects services (reconciliationEffects reconciliation (toolingEffects tooling applyEffects))))))))))))
                    core d pending=foldM step (False,d) pending
                      where
                        step result@(True,_) _=pure result
                        step (_,current) (SetScreenMode mode)=pure (False,(resizeScreenMode (modeSize mode) current) {videoMode=Just mode})
                        step (_,current) effect=do
                          result@(quit,_)<-runtimeEffects current [effect]
                          when quit (writeIORef exiting True)
                          pure result
                    guestCore d pending=validateGuestEffects d pending >> core d pending
                    effects d pending=do
                      writeIORef exiting False
                      (quit,updated)<-policyEffects permissions core d pending
                      approvedExit<-readIORef exiting
                      pure (quit || approvedExit,updated)
                    prepareBodies desktop=do
                      requests<-conversationBodyRequests conversation desktop
                      (prepared,completed)<-tickTextPresentation textPresentation requests desktop
                      adoptConversationBodies conversation completed prepared
                    tick d=tickProjectBrowser projectBrowser d >>= tickGitOperations gitOperations applyEffects >>= tickTooling tooling applyEffects >>= tickReconciliation reconciliation (sidebarEffects sidebarHost applyEffects) >>= tickSessionServices services >>= tickConversation conversation >>= tickBuildPreparation services runtimeEffects >>= tickDebugger debugger >>= tickPreparedDebug debugger runtimeEffects >>= tickPermissions permissions >>= tickHighlighting highlighting >>= tickAutocomplete autocomplete >>= tickKeybindings keybindings >>= tickMenus menuHost runtimeEffects >>= tickDebuggerSidebar debugSidebar sidebarHost debugger >>= tickPackageSidebar packageSidebar sidebarHost >>= tickSessionSidebar sessionSidebar sidebarHost >>= tickSidebar sidebarHost runtimeEffects >>= tickPluginWindows >>= prepareBodies
                    inspectTool d name parameters
                      | PluginTool.hasTool editorToolset name = do
                          context<-captureDocsContext d
                          directory<-evaluate (startingDirectory d)
                          settings<-captureAgentSettings conversation d
                          pure (d,PluginTool.callTool editorToolset
                            (PluginServices.EditorServices (docsServices docsCommands context)
                              (environmentServices environmentCommands directory) settings) name parameters)
                      | name `elem` ["list_windows","read_selection"] = pure (d,pure (builtinTool d name parameters))
                      | name `elem` toolingToolNames = toolingTool tooling guestCore d name parameters
                      | name `elem` workspaceToolNames = workspaceTool guestCore d name parameters
                      | name `elem` fileToolNames = fileTool guestCore d name parameters
                      | name `elem` testsToolNames = testsTool services d name parameters
                      | name `elem` historyToolNames = historyTool d name parameters
                      | name `elem` runtimeToolNames = runtimeTool services d name parameters
                      | name `elem` gitToolNames = gitTool gitOperations d name parameters
                      | name=="clipboard_write" = clipboardTool d parameters
                      | name `elem` controlToolNames = controlTool guestCore d name parameters
                      | name=="editor_screen" = pure (d,case parseEither (withObject "screen" (\o -> o .:? "image" .!= False)) parameters of
                          Left err -> pure (Left (T.pack err))
                          Right image -> capture font d image)
                      | otherwise = debuggerTool debugger d name parameters
                    inspect d token request=do
                      writeIORef exiting False
                      let agents=conversationAgents conversation
                          hub=AR.agentHub agents
                          reject=pure (d,pure (Just (rpcError (fromMaybe Null (parseMaybe (withObject "request" (.: "id")) request)) (-32600) "Invalid or inactive agent connection.")))
                      let permitted questionBinding callback current name parameters
                            | PluginTool.hasTool requestToolset name = do
                                context<-bufferRequestServices (bufferReader permissions currentCaller)
                                  (bufferEditor permissions currentCaller) (windowReader permissions currentCaller) current name parameters
                                terminals<-terminalServices permissions (sessionConsoles services) currentCaller (Build.buildStartDirectory current)
                                pure (current,either (pure . Left)
                                  (\services'->PluginTool.callTool requestToolset
                                    services' {PluginRequest.requestQuestions=fmap (\bound->questionServices conversation bound permissions currentCaller) questionBinding,
                                      PluginRequest.requestTerminals=Just terminals}
                                    name parameters) context)
                            | name=="editor_input" = permissionBuildInputAs currentCaller permissions
                                (\admission admittedDesktop admittedTool admittedArgs->withBuildAdmission services admission (controlTool guestCore admittedDesktop admittedTool admittedArgs)) current name parameters
                            | otherwise = permissionCallAs currentCaller permissions callback current name parameters
                          currentCaller=case token of
                            Nothing->pure (Right ())
                            Just secret->fmap (() <$) (resolveActiveAgentAccess (AR.agentAccess agents) hub secret)
                      response<-case token of
                        Nothing -> editorResponseWith editorSpecs (permitted Nothing inspectTool) d request
                        Just secret | secret==autocompleteToken autocomplete -> editorResponseOnly (autocompleteTools autocomplete) (\current name parameters -> pure (current,autocompleteTool autocomplete name parameters)) d request
                        Just secret -> do
                          bound<-resolveAgentAccess (AR.agentAccess agents) secret
                          case bound of
                            Nothing -> reject
                            Just ident -> do
                              active<-AH.statusAgent hub (AH.Agent ident) ident
                              case active >>= maybe (Left "Unknown workspace.") Right . parseMaybe (withObject "agent" (.: "cwd")) of
                                Left _ -> reject
                                Right root -> do
                                  questionCaller<-captureQuestionCaller conversation ident
                                  let dispatch current name parameters
                                        | PluginTool.hasTool agentToolset name = pure (current,PluginTool.callTool agentToolset (agentServices hub (AH.Agent ident) root) name parameters)
                                        | otherwise = inspectTool current name parameters
                                      -- Worktree agents reach this endpoint only for
                                      -- coordination. Their editor tools use their own session.
                                      visible=if ident==AR.primaryAgent agents then editorSpecs++agentTools else agentTools
                                  editorResponseOnly visible (permitted (either (const Nothing) Just questionCaller) dispatch) d request
                      let (updated,finish)=response
                      quit<-readIORef exiting
                      pure (quit,updated,finish)
                case daemon of
                  Just sid -> do
                    let startOwned=do
                          -- Recovery eligibility can change after the relay spawns
                          -- us. This callback runs under the daemon lifetime lock.
                          when (RequireCheckpoint `elem` flags) $ do
                            present<-checkpointPath sid >>= doesFileExist
                            unless present (ioError (userError "Saved session was deleted before recovery started."))
                          AR.activateAgentCheckpoint (conversationAgents conversation)
                    runRemoteDaemonWithStartup (Just systemOneBrowser) startOwned (awaitPermissionWork permissions) sid scale effects tick inspect liveDesktop
                  Nothing -> die "Missing session process identity."
  where
    lastMaybe []=Nothing
    lastMaybe values=Just (last values)
    parseAppearance s = maybe (die "Appearance must be light, dark, or system.") pure (lookup s [("light",LightMode),("dark",DarkMode),("system",SystemMode)])
    isDaemon RemoteDaemon{} = True
    isDaemon _ = False
    isMode Mode{} = True
    isMode _ = False
    isSession RemoteSession{} = True
    isSession _ = False
    isSSH SSH{} = True
    isSSH _ = False

-- The connected notification follows both the assets handshake and catalog write.
-- Do not print a resumable ID before the peer has completed those steps.
awaitSessionReady :: RemotePeer -> IO ()
awaitSessionReady peer = do
  result<-timeout 75000000 (loop False)
  maybe (die "Session did not become ready within 75 seconds.") pure result
  where
    loop assets=peerReceive peer >>= \packet -> case packet of
      Nothing -> die "Session ended before becoming ready."
      Just (JsonPacket value) -> case parseEither readiness value of
        Right (kind,connected,detail)
          | kind=="assets" -> loop True
          | kind=="connection" && connected && assets -> pure ()
          | kind `elem` ["closed","error"] -> die ("Session startup failed: "++T.unpack detail)
        _ -> loop assets
      _ -> loop assets
    readiness=withObject "session startup" $ \o -> (,,) <$> o .:? "type" .!= (""::T.Text)
      <*> o .:? "connected" .!= False <*> o .:? "message" .!= ("Session closed"::T.Text)

-- Socket close and the daemon's writer cleanup run on different threads. Wait
-- for the read-only status to confirm local detach before a rapid next resume.
awaitSessionDetached :: SessionRecord -> IO ()
awaitSessionDetached record | sessionHost record/=Nothing = pure ()
awaitSessionDetached record = do
  result<-timeout 5000000 loop
  maybe (die "Session started, but local detach could not be confirmed within 5 seconds.") pure result
  where
    loop=do
      activity<-sessionActivity record
      case activity >>= either (const Nothing) Just . parseEither
        (withObject "session activity" (\o -> o .:? "attached" .!= True)) of
        Just False -> pure ()
        _ -> threadDelay 20000 >> loop

printSessions :: IO ()
printSessions = do
  records<-listSessions
  if null records then putStrLn "No unfinished editor sessions."
  else mapM_ printRecord records
  where
    printRecord record=do
      state<-sessionState record
      activity<-if state=="running" then sessionActivity record else pure Nothing
      putStrLn (sessionId record++"  "++state++activityLabel activity++"  "++
        maybe "local" id (sessionHost record)++"  "++sessionDirectory record)
    activityLabel Nothing=""
    activityLabel (Just value)=case parseEither activityFields value of
      Left _ -> ""
      Right (attached,replying,queued,waiting,unsaved) -> concat
        [if attached then " attached" else " detached", if replying then " replying" else "",
         if queued>0 then " queued="++show queued else "", if waiting then " waiting" else "",
         if unsaved then " unsaved" else ""]
    activityFields :: Value -> Parser (Bool,Bool,Int,Bool,Bool)
    activityFields=withObject "session activity" $ \o -> (,,,,) <$> o .:? "attached" .!= False
      <*> o .:? "agentReplying" .!= False <*> o .:? "agentQueued" .!= 0
      <*> o .:? "waiting" .!= False <*> o .:? "dirty" .!= False

-- GetOpt optional arguments normally require '='. Also accept --resume ID.
resumeArguments :: [String] -> [String]
resumeArguments ("--":rest)="--":rest
resumeArguments ("--resume":ident:rest) | not ("-" `isPrefixOf` ident) = ("--resume="++ident):resumeArguments rest
resumeArguments (arg:rest)=arg:resumeArguments rest
resumeArguments []=[]

chooseSession :: Maybe String -> IO SessionRecord
chooseSession wanted = do
  sessions<-listSessions
  case wanted of
    Just ident -> case filter (isPrefixOf ident . sessionId) sessions of
      [record] | not (null ident) -> pure record
      [] -> die ("No unfinished session matches "++ident++".")
      _ -> die "Session ID is ambiguous; use a longer ID."
    Nothing -> case sessions of
      [] -> die "No unfinished editor sessions."
      [record] -> pure record
      _ -> do
        putStrLn "Unfinished editor sessions:"
        mapM_ (\(n,record) -> putStrLn (show n++") "++sessionId record++"  "++maybe "local" id (sessionHost record)++"  "++sessionDirectory record)) (zip [1::Int ..] sessions)
        interactive<-hIsTerminalDevice stdin
        if not interactive then die "Choose one with --resume ID."
        else do
          putStr "Resume session number (empty to cancel): "; hFlush stdout
          answer<-getLine
          case readMaybe answer of
            Just n | n>=1, record:_<-drop (n-1) sessions -> pure record
            _ -> die "No session selected."

interrupted :: AsyncException -> IO ()
interrupted UserInterrupt=pure ()
interrupted other=throwIO other

-- Unlike detachment, a launcher interrupt stops the session after checkpointing.
-- This travels through the active peer, so local and SSH sessions behave alike.
suspendSession :: RemotePeer -> IO ()
suspendSession peer = do
  result <- timeout 15000000 $ do
    peerSend peer (JsonPacket (object ["type" .= ("suspend"::T.Text)]))
    wait
  case result of
    Just True -> pure ()
    _ -> hPutStrLn stderr "Could not confirm daemon shutdown; the session may still be running."
  where
    wait = peerReceive peer >>= \packet -> case packet of
      Just (JsonPacket value) | Just (("closed" :: T.Text),True) <- parseMaybe
        (withObject "shutdown" (\o -> (,) <$> o .: "type" <*> o .:? "resumable" .!= False)) value -> pure True
      Nothing -> pure False
      _ -> wait

data FrontendDetached = FrontendDetached deriving Show
instance Exception FrontendDetached

withDetachSignals :: IO a -> IO a
#ifdef mingw32_HOST_OS
withDetachSignals = id
#else
withDetachSignals action = do
  thread<-myThreadId
  let install signal=installHandler signal (Catch (throwTo thread FrontendDetached)) Nothing
      restore signal handler=installHandler signal handler Nothing >> pure ()
  bracket (install sigTERM) (restore sigTERM) $ \_ ->
    bracket (install sigHUP) (restore sigHUP) (const action)
#endif

-- Rebuild arguments rather than reinterpreting their shell spelling. Paths after
-- -- are passed verbatim to the remote process, including spaces and metacharacters.
remoteArguments :: [Option] -> [FilePath] -> [String]
remoteArguments flags paths = concatMap option flags++["--"]++paths
  where
    option (Scale s)=["--scale",s]
    option (Size s)=["--size",s]
    option (Mode s)=["--mode",s]
    option (ColorMode s)=["--appearance",s]
    option (RemoteSession s)=["--remote-session",s]
    option (Scene s)=["--scene",s]
    option Demo=["--demo"]
    option CRT=["--crt"]
    option NoCRT=["--no-crt"]
    option ClassicIcons=["--classic-icons"]
    option StandardKeys=["--standard-keys"]
    option (CursorBlink yes)=[if yes then "--blink-cursor" else "--no-blink-cursor"]
    option (Streamer yes)=[if yes then "--streamer" else "--no-streamer"]
    option (Pixelate yes)=[if yes then "--pixelate-unicode" else "--no-pixelate-unicode"]
    option MaterialIcons=["--material-icons"]
    option WordStar=["--wordstar"]
    option _=[]

expandRemoteHome :: FilePath -> IO FilePath
expandRemoteHome "~" = getHomeDirectory
expandRemoteHome ('~':'/':path) = (</> path) <$> getHomeDirectory
expandRemoteHome path = pure path

stageScene :: Desktop -> String -> IO Desktop
stageScene d scene = case lookup scene [("open",Open),("tree",ToggleTree),("help",Help),("diff",GitDiff)] of
  Just cmd -> snd <$> uncurry applyEffects (runCommand cmd d)
  Nothing -> pure (setScene d scene)

setScene :: Desktop -> String -> Desktop
setScene d scene=case scene of
  "desktop" -> d
  "menu" -> d {menu=Just (0,1)}
  "about" -> fst (runCommand About d)
  "gallery" -> fst (runCommand Gallery d)
  "preferences" -> fst (runCommand EditorOptions d)
  "split" -> fst (runCommand SplitHorizontal d)
  _ -> message "Unknown preview scene" [T.pack scene] d

-- | A deterministic sample desktop for previews and rendering checks.
demoDesktop :: Desktop
demoDesktop = addDocument Nothing (newBuffer sample) (initialDesktop (80,25))
  where sample=T.unlines ["module Main where","", "factorial :: Integer -> Integer", "factorial n = product [1 .. n]", "", "main :: IO ()", "main = do", "  putStrLn \"Enter a number:\"", "  input <- getLine", "  print (factorial (read input))"]

-- | Interpret raw buffer reads, help, browser and save effects in order.
-- Ordinary file presentation is owned by SidebarCommands; ReadPath deliberately
-- stays synchronous for source navigation and explicit buffer services.
-- The Bool result requests exit; runtime-specific handlers delegate unhandled
-- effects here. This interpreter can perform blocking filesystem work.
applyEffects :: Desktop -> [Effect] -> IO (Bool,Desktop)
applyEffects = foldM apply . (False,)
  where
    apply state@(True,_) _=pure state
    apply (_,d) CopyConversation{}=pure (False,d {status="Conversation copy requires its presentation owner."})
    apply (_,d) SubmitEditor{}=pure (False,d {status="Editor submission requires its callable owner."})
    apply (_,d) (RetireEditorMount mount)=Editor.retireEditorMount mount >> pure (False,d)
    apply (_,d) (RetirePluginWindow reference)=(False,) <$> retireClosedWindow reference d
    -- Reload/inspection belong to the session worker, not this blocking file
    -- interpreter used by standalone drivers and snapshots.
    apply (_,d) SubmitPopupChoiceForm{}=pure (False,d {status="Popup choices require their form owner."})
    apply (_,d) SubmitChoiceForm{}=pure (False,d {status="Choice form submission requires its sidebar owner."})
    apply (_,d) SubmitInputForm{}=pure (False,d {status="Input form submission requires its sidebar owner."})
    apply (_,d) (RetireInputForm reference)=Form.retireForm reference >> pure (False,d)
    apply (_,d) ReloadKeyBindings{}=pure (False,d {status="Binding reload requires a running session."})
    apply (_,d) InspectKeyBindings{}=pure (False,d {status="Binding inspection requires a running session."})
    apply (_,d) (EnvironmentAction action args)= (False,) <$> environmentAction action args d
    apply (_,d) AutocompleteAction{}=pure (False,d {status="Autocomplete is unavailable in this preview."})
    apply (_,d) LanguageRequest{}=pure (False,d {status="Language tools are unavailable in this preview."})
    apply (_,d) JumpTo{}=pure (False,d)
    apply (_,d) RunGit{}=pure (False,d {status="Git operations are unavailable in this preview."})
    apply (_,d) (ProjectRequest _)=pure (False,d {status="Project browser is unavailable in this preview."})
    apply (_,d) ReadMergeBranches=pure (False,d {status="Git operations are unavailable in this preview."})
    apply (_,d) ReviewExternal=pure (False,d {status="Disk change monitoring is unavailable in this preview."})
    apply (_,d) ResolveConflict{}=pure (False,d {status="Disk change monitoring is unavailable in this preview."})
    apply (_,d) DownloadCancelAction{}=pure (False,d {status="Downloads cancellation requires its running owner."})
    apply (_,d) DebugAction{}=pure (False,d {status="Debugger unavailable in this preview."})
    apply (_,d) PermissionAction{}=pure (False,d {status="Agent permissions are unavailable in this preview."})
    apply (_,d) ExecuteShellBlockAction{}=pure (False,d {status="Session services are unavailable in this preview."})
    apply (_,d) TerminalMouseInput{}=pure (False,d {status="Session services are unavailable in this preview."})
    apply (_,d) ServiceAction{}=pure (False,d {status="Session services are unavailable in this preview."})
    apply (_,d) AgentAction{}=pure (False,d {status="Agents are unavailable in this preview."})
    apply (_,d) ConversationSessionAction{}=pure (False,d {status="Conversation sessions require their running owner."})
    apply (_,d) PackageDebugAction{}=pure (False,d {status="Package debug requires its running owner."})
    apply (_,d) AdoptPreparedDebug{}=pure (False,d {status="Debug preparation requires its running owner."})
    apply (_,d) PackageBuildAction{}=pure (False,d {status="Package build requires its running owner."})
    apply (_,d) AdoptPreparedBuild{}=pure (False,d {status="Build preparation requires its running owner."})
    apply (_,d) DownloadDocument{}=pure (False,d)
    apply (_,d) ExportBufferDocument{}=pure (False,d {status="Buffer export requires its running owner."})
    apply (_,d) ReadBrowserClipboard=pure (False,d)
    apply (_,d) WriteBrowserClipboard{}=pure (False,d)
    apply (_,d) Exit=pure (True,d)
    apply (_,d) (SaveWideSectionTitles chosen)=do
      result<-writeEditorDefaults (object ["wideSectionTitles" .= chosen])
      pure (False,d {status=either ("Section titles changed for this session; could not save: "<>) (const "Wide section title preference saved.") result})
    apply (_,d) (SaveMacKeySymbols chosen)=do
      result<-writeEditorDefaults (object ["macKeySymbols" .= chosen])
      pure (False,d {status=either ("Key labels changed for this session; could not save: "<>) (const "Mac key symbol preference saved.") result})
    apply (_,d) (SaveChatSubmit chosen)=do
      result<-writeEditorDefaults (object ["chatSubmit" .= chatSubmitName chosen])
      pure (False,d {status=either ("Chat input changed for this session; could not save: "<>) (const "Chat input default saved.") result})
    apply (_,d) (SaveBufferViewDefault mode)=do
      result<-writeEditorDefaults (object ["bufferView" .= bufferViewName mode])
      pure (False,d {status=either ("Default view changed for this session; could not save: "<>) (const "Default buffer view saved.") result})
    apply (_,d) SetScreenMode{}=pure (False,d {status="Screen modes are available in a graphical window."})
    apply (_,d) (ReadPath path)
      | Just window<-find (\w -> fmap filePath (windowDocument (buffers d) w >>= documentFile)==Just path) (windows d) =
          pure (False,focusWindow (windowId window) d)
    apply (_,d) (ReadPath path)=do
      directory<-doesDirectoryExist path
      if directory then do
        (_,browsed)<-apply (False,d) (ReadTree path)
        package<-packageFile path
        maybe (pure (False,browsed)) (\file -> apply (False,browsed) (ReadPath file)) package
      else do
        result<-loadFile path
        let opened=case result of
              Left err->message "Cannot open file" (wrapMessage (T.pack err)) d
              Right (file,b)->case find (\(_,doc)->fmap filePath (documentFile doc)==Just (filePath file)) (M.toList (buffers d)) of
                Just (bid,_)->maybe d (\w->focusWindow (windowId w) d) (find ((==Just bid) . bufferId) (windows d))
                Nothing->addDocument (Just file) b d
        apply (False,opened) (RefreshGit (startingDirectory opened))
    apply (_,d) (BrowsePath path pattern)=do
      result<-readDirectory path pattern
      pure (False,case result of Left err -> browserError (T.pack err) d; Right (base,entries) -> openBrowser base pattern entries d)
    apply (_,d) (BrowseDirectories path)=do
      result<-readDirectory path "*"
      pure (False,case result of Left err -> browserError (T.pack err) d; Right (base,entries) -> openDirectoryBrowser base entries d)
    apply (_,d) (ChangeDirectory path)=do
      result<-readDirectory path "*"
      case result of
        Left err -> pure (False,browserError (T.pack err) d)
        Right (base,_) -> do
          changed<-try (setCurrentDirectory base) :: IO (Either IOException ())
          case changed of
            Left err -> pure (False,browserError (T.pack (show err)) d)
            Right () -> apply (False,installSidebar (sidebarDirectory base d) d {defaultDirectory=Just base,dialog=Nothing,status="Directory changed."}) (RefreshGit base)
    apply (_,d) OpenFile{}=pure (False,d {status="File opening requires its presentation owner."})
    apply (_,d) OpenFileBytes{}=pure (False,d {status="Dropped file opening requires its presentation owner."})
    apply (_,d) OpenChoice{}=pure (False,d {status="File opening requires its presentation owner."})
    apply (_,d) (ReadTree path)=do
      root<-canonicalizePath path
      pure (False,installSidebar (sidebarDirectory root d) d)
    apply (_,d) DebugSourceAction{}=pure (False,d {status="Debugger source actions are unavailable in this preview."})
    apply (_,d) DebugSidebarAction{}=pure (False,d {status="Debugger sidebar is unavailable in this preview."})
    apply (_,d) SessionSidebarAction{}=pure (False,d {status="Session selection is unavailable in this preview."})
    apply (_,d) AgentSidebarAction{}=pure (False,d {status="Agent navigation is unavailable in this preview."})
    apply (_,d) LoadTree{}=pure (False,d {status="Sidebar provider host is unavailable in this preview."})
    apply (_,d) InvokeTree{}=pure (False,d {status="Sidebar provider host is unavailable in this preview."})
    apply (_,d) RefreshTree{}=pure (False,d)
    apply (_,d) RefreshRenamedPath{}=pure (False,d)
    apply (_,d) InvokeMenu{}=pure (False,d {status="Registered menu actions are unavailable in this preview."})
    apply (_,d) ReadHelp=do
      path<-getDataFileName "README.md"
      (opened,_)<-followLink False d (Just path) ""
      pure (False,opened)
    apply (_,d) (FollowTreeLink trace path target)
      | maybe False (hitCurrent trace) (sideTree d)=do
          (opened,_)<-followLink False d (Just path) target
          pure (False,opened)
      | otherwise=pure (False,d {status="Sidebar link target expired."})
    apply (_,d) (FollowLink origin target)
      | not (linkOriginCurrent d origin)=pure (False,d {status="Link body expired."})
      | otherwise=do
          (opened,_)<-followLink False d (linkOriginPath origin) target
          pure (False,opened)
    apply (_,d) (RefreshGit path)=do
      repo<-repositoryStatus path
      pure (False,d {branchStatus=maybe "" (\r -> repoBranch r <> if repoDirty r then "*" else "") repo,branchAdded=maybe 0 repoAdded repo,branchDeleted=maybe 0 repoDeleted repo,branchRoot=fmap repoRoot repo,gitReview=case gitReview d of Just review | fmap repoRoot repo == Just (reviewRoot review) -> Just review; _ -> Nothing})
    apply (_,d) ReadGitDiff=do
      reviewed<-reviewRepository (gitDirectory d)
      case reviewed of
        Left err -> pure (False,message "Cannot review changes" (wrapMessage err) d)
        Right review -> apply (False,(addReadOnly "Git diff" (reviewText review) d) {gitReview=Just review,status="Review saved changes; Tools > Approve changes commits them."}) (RefreshGit (reviewRoot review))
    -- Docs: docs/site/screenshots/git-commit.png (docs/git.md); refresh if approval changes.
    apply (_,d) AskGitCommit
      | any documentModified (M.elems (buffers d)) = pure (False,message "Unsaved changes" ["Save changed buffers before approving a commit."] d)
      | Just review <- gitReview d = pure (False,d {dialog=Just (Dialog "Approve changes" Committing [Input "Commit message" "" 0] 0 ["Commit","Cancel"] ["Commit all reviewed saved changes in:",T.pack (reviewRoot review)]),menu=Nothing})
      | otherwise = apply (False,d) ReadGitDiff
    apply (_,d) (WriteGitCommit text)=case gitReview d of
      Nothing -> pure (False,message "Review required" ["Open Tools > Git diff before approving changes."] d)
      Just review -> do
        result<-commitReview review text
        case result of
          Left err -> pure (False,d {status=err,dialog=fmap (\dg -> dg {body=wrapMessage err}) (dialog d)})
          Right summary -> do
            (_,updated)<-apply (False,d {dialog=Nothing}) ReadGitDiff
            pure (False,updated {gitReview=Nothing,status=T.takeWhile (/='\n') summary})
    apply (_,d) (SaveDocument bid target after)=case M.lookup bid (buffers d) of
      Nothing->pure (False,d)
      Just doc->do
        result<-case target of
          Nothing->case documentFile doc of
            Nothing->pure (Left "Choose a filename with Save as.")
            Just file->saveFile file (documentBuffer doc)
          Just path->do
            -- loadFile resolves symbolic links and reports encoding/permission errors.
            loaded<-loadFile path
            case loaded of
              Left err->pure (Left err)
              Right (file,_)->case documentFile doc of
                Just old | filePath old==filePath file->saveFile old (documentBuffer doc)
                _ | diskBytes file/=Nothing->pure (Left "Save as will not overwrite an existing file. Open it first or choose a new name.")
                  | otherwise->saveFile file (documentBuffer doc)
        case result of
          Left err->pure (False,message "Cannot save file" (wrapMessage (T.pack err)) d)
          Right file->do
            let b=documentBuffer doc
                clean=restyle doc {documentFile=Just file,documentBuffer=markSaved b}
                -- Transfer the saved path's privacy once, including source
                -- and generated views, so protection survives closing this draft.
                protect other | documentPrivate clean,any (maybe False (equalFilePath (filePath file))) [filePath <$> documentFile other,documentOrigin other]=other {documentPrivate=True}
                              | otherwise=other
                updated=clampReviewWindows (normalizeDocumentViews bid d {buffers=M.map protect (M.insert bid clean (buffers d)),status="File saved."})
            (_,refreshed)<-apply (False,updated) (RefreshGit (takeDirectory (filePath file)))
            case after of
              Nothing->pure (False,refreshed)
              Just cmd->uncurry applyEffects (runCommand cmd refreshed)

gitDirectory :: Desktop -> FilePath
gitDirectory d = case activeDocument d >>= documentFile of
  Just file -> takeDirectory (filePath file)
  Nothing -> maybe (startingDirectory d) reviewRoot (gitReview d)

-- Start with the nearest enclosing Cabal package, without invoking a build.
packageDirectory :: FilePath -> IO FilePath
packageDirectory start = search start
  where
    search path = do
      entries<-either (const []) id <$> (try (listDirectory path) :: IO (Either IOException [FilePath]))
      if any ((==".cabal") . takeExtension) entries then pure path
        else if takeDirectory path==path then pure start else search (takeDirectory path)

-- Selecting another Files directory preserves the other ordinary provider roots.
sidebarDirectory :: FilePath -> Desktop -> Sidebar
sidebarDirectory path d=case sideTree d of
  Just tree->tree {treeRoot=path,treeFocused=True}
  Nothing->emptySidebar path (min 24 (max 0 (fst (screenSize d)-20))) True
