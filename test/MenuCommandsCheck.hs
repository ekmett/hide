{-# LANGUAGE CPP, OverloadedStrings #-}
module MenuCommandsCheck (checks) where
import Control.Concurrent (threadDelay)
import Control.Exception (bracket,bracket_,finally)
import Control.Concurrent.MVar
import System.Directory (getCurrentDirectory,getTemporaryDirectory,createDirectory,removeFile,removePathForcibly)
import System.Environment (lookupEnv,setEnv,unsetEnv)
import Control.Monad (unless)
import Data.Aeson
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Data.List (findIndex)
import System.Timeout (timeout)
import Hide.Commands (configuredBindings, contributedBindingCommands)
import qualified Hide.Bindings as Bindings
import qualified Data.Map.Strict as M
import Hide.Buffer (contents)
import Hide.DocsMCP
import System.FilePath ((</>))
import System.IO (openTempFile,hClose)
import qualified Data.Text.IO as TIO
import Hide.Keybindings
import Hide.MenuCommands
import Hide.Model
import Hide.Plugin.Command (withRegistry,registerCommand,CommandDef(..),Codec(..))
import qualified Hide.Plugin.Menu as Plugin
import Hide.Links (prepareMarkdown)
import MenuExtension
#if defined(WITH_WEB) || defined(WITH_REMOTE)
import Data.Aeson.Types (parseEither)
import Hide.GuestAccess (beginGuestInput)
import Hide.Protocol
import Hide.Frontend (decodeKey)
import Hide.RemoteTerminal (terminalEventInput)
import Hide.RemoteWindow
import Hide.Window (nativeCommands, nativeCommandsFor, nativeMenuEventFor, nativeMenuShortcut)
#endif

checks :: IO ()
checks=bracket (lookupEnv "hide_datadir") (maybe (unsetEnv "hide_datadir") (setEnv "hide_datadir")) $ \_->do
  getCurrentDirectory >>= setEnv "hide_datadir"
  runChecks

runChecks :: IO ()
runChecks=withDocsCommands $ \docs->withRegistry $ \registry->withMenuCommands docs $ \host->do
  runtimeLifecycleChecks docs
  extension<-either (error . show) pure =<< registerExtension registry (menuContributions host)
    (\context text->fmap PreparedDocument (prepareMarkdown (invocationColumns context) "/tmp/README.md" "" text))
  metadata<-Plugin.menuSnapshot (menuContributions host)
  let initial=(initialDesktop (80,25)) {contributedMenus=metadata,agentMenuRefs=menuAgentReferences host,menusActive=True}
      helpRef=case [Plugin.menuReference entry | entry<-metadata,Plugin.menuName (Plugin.menuReference entry)=="hide.help.contents"] of ref:_->ref; _->error "missing help contribution"
      check label condition=unless condition (error label)
      waitDoc desktop=do
        updated<-tickMenus host (\current _->pure (False,current)) desktop
        case activeDocument updated of
          Just _->pure updated
          _ | "failed" `T.isInfixOf` status updated || "expired" `T.isInfixOf` status updated->error (T.unpack (status updated))
            | otherwise->threadDelay 5000 >> waitDoc updated
      run desktop effects=do
        (_,queued)<-menuEffects host (\_ _->error "registered action missed menu worker") desktop effects
        timeout 5000000 (waitDoc queued) >>= maybe (error "menu worker did not complete") pure
  -- Configuration reaches an independently typed contribution, not a built-in alias.
  bracket temporary removePathForcibly $ \directory->do
    previous<-lookupEnv "XDG_CONFIG_HOME"
    bracket_ (setEnv "XDG_CONFIG_HOME" directory) (maybe (unsetEnv "XDG_CONFIG_HOME") (setEnv "XDG_CONFIG_HOME") previous) $ withKeybindings M.empty (contributedBindingCommands initial) $ \bindings->do
      TIO.writeFile (directory </> "thc.toml") "[editor.keybindings.terminal.source]\n\"example.manual\" = [\"Ctrl+Shift+J\"]\n"
      let base=initial {defaultDirectory=Just directory,keyBindings=either (error . show) id (configuredBindings (contributedBindingCommands initial) M.empty)}
          settle current=do
            next<-tickKeybindings bindings current
            if status next=="Reloading keybindings..." then threadDelay 1000 >> settle next else pure next
      started<-tickKeybindings bindings base
      let (reload,requests)=runCommand ReloadBindings started
      (_,pending)<-keybindingEffects bindings (\desktop _->pure (False,desktop)) reload requests
      loaded<-timeout 5000000 (settle pending) >>= maybe (error "runtime binding reload did not finish") pure
      let key=V.EvKey (V.KChar 'j') [V.MCtrl,V.MShift]
          (chosen,actions)=handleEvent key loaded
      check "TOML binds an actual contributed typed command" (actions==[InvokeMenu extension Plugin.HumanMenu Nothing])
      opened<-run chosen actions
      check "bound typed callback installs independent extension document"
        (maybe False (T.isInfixOf "Independent extension" . contents . documentBuffer) (activeDocument opened))
      let modal=prompt "Edit" Information [Input "Text" "draft" 5] loaded
      check "contributed remap cannot escape modal editing" (null (snd (handleEvent key modal)))
#if defined(WITH_WEB) || defined(WITH_REMOTE)
      guestKey<-either error pure (parseEither parseInput (object ["type" .= ("key"::T.Text),"key" .= ("j"::T.Text),"mods" .= (["ctrl","shift"]::[T.Text])]))
      check "contributed remap cannot grant guest authority" (case applyGuestInput guestKey (beginGuestInput loaded) of Left _->True; _->False)
#endif
      TIO.writeFile (directory </> "thc.toml") "[editor.keybindings.terminal.source]\n\"example.not-published\" = [\"Ctrl+Shift+J\"]\n"
      let (invalid,invalidRequests)=runCommand ReloadBindings loaded
      (_,invalidPending)<-keybindingEffects bindings (\desktop _->pure (False,desktop)) invalid invalidRequests
      retained<-timeout 5000000 (settle invalidPending) >>= maybe (error "invalid runtime reload did not finish") pure
      check "unpublished ID fails reload and retains validated runtime chord"
        ("Unknown" `T.isInfixOf` status retained && snd (handleEvent key retained)==actions)
      TIO.writeFile (directory </> "thc.toml") "[editor.keybindings.terminal.source]\n\"example.manual\" = []\n"
      let (unbinding,unbindingRequests)=runCommand ReloadBindings loaded
      (_,unbindingPending)<-keybindingEffects bindings (\desktop _->pure (False,desktop)) unbinding unbindingRequests
      unbound<-timeout 5000000 (settle unbindingPending) >>= maybe (error "runtime unbinding reload did not finish") pure
      let boundItem=case filter ((==extension) . Plugin.menuReference) metadata of item:_->item; _->error "missing bound extension"
      check "explicit empty list removes contributed chord and effective label"
        (null (snd (handleEvent key unbound)) && menuShortcut unbound (MenuItem "Extension" "F12" (contributionCommand unbound boundItem))=="")
  let (fromF1,effects)=handleEvent (V.EvKey (V.KFun 1) []) initial
  check "F1 resolves live contributed Help lifetime" (effects==[InvokeMenu helpRef Plugin.HumanMenu Nothing])
  help<-run fromF1 effects
  check "registered help prepares styled document with relative links" (maybe False (\doc->documentMarkdownPath doc/=Nothing && not (null (documentLinks doc)) && documentLabel doc==Just "Haskell Help") (activeDocument help))
  let helpIndex=case findIndex (\(title,_,_)->title=="Help") menus of Just index->index; _->error "missing help slot"
      popup=initial {menu=Just (helpIndex,0)}
      (chosen,popupEffects)=handleEvent (V.EvKey V.KEnter []) popup
  check "Help popup invokes same exact contribution as F1" (popupEffects==effects)
  _<-run chosen popupEffects
#if defined(WITH_WEB) || defined(WITH_REMOTE)
  packet<-either error pure (parseEither parseInput (object ["type" .= ("menu"::T.Text),"command" .= Plugin.menuName extension,"registry" .= Plugin.menuEpoch extension,"generation" .= Plugin.menuGeneration extension]))
  let transport=object (frameMetadata "/tmp" initial++["menuCommands" .= map fst protocolMenuCommands])
  frame<-either error pure (parseRemoteFrame transport (replicate 25 (toJSON ([]::[Value]))))
  let extensionIndex=case findIndex ((==extension) . Plugin.menuReference) metadata of Just index->index; _->error "missing extension snapshot"
      nativeToken=length nativeCommands+extensionIndex
  nativePacket<-maybe (error "native contribution failed to resolve") pure (remoteNativeMenuInput frame 19 [11,nativeToken,19])
  check "native contribution transports exact ID and generation" (nativePacket==object ["type" .= ("menu"::T.Text),"command" .= Plugin.menuName extension,"registry" .= Plugin.menuEpoch extension,"generation" .= Plugin.menuGeneration extension])
  check "native old menu incarnation refuses extension event" (remoteNativeMenuInput frame 20 [11,nativeToken,19]==Nothing)
  check "local native catalogue retains exact registered action" (nativeMenuEventFor (nativeCommandsFor initial) 19 [11,nativeToken,19]==Just (contributionCommand initial (metadata !! extensionIndex)))
  check "native Help row carries its registered token and extension joins Help menu" (case lookup "Help" (remoteMenuLayout frame) of Just rows->any (\(title,_,token)->title=="Extension manual" && token==nativeToken) rows && any (\(title,_,token)->title=="Contents" && token>=length nativeCommands) rows; _->False)
  let profiles=either (error . show) id (configuredBindings [] (M.singleton "macos" (M.singleton "source" (M.singleton "hide.help.contents" ["Cmd+Shift+J"]))))
      configured=initial {nativeMac=True,videoMode=Just 3,keyBindings=profiles}
      helpCommand=case [contributionCommand configured item | item<-metadata,Plugin.menuReference item==helpRef] of command:_->command; _->error "missing live help command"
      configuredTransport=object (frameMetadata "/tmp" configured++["menuCommands" .= map fst protocolMenuCommands])
  configuredFrame<-either error pure (parseRemoteFrame configuredTransport (replicate 25 (toJSON ([]::[Value]))))
  check "configured keyboard Help invokes current registry lifetime" (snd (handleEvent (V.EvKey (V.KChar 'j') [V.MMeta,V.MShift]) configured)==effects)
  check "local and remote native Help keep effective shortcut on the registered token" (nativeMenuShortcut configured helpCommand==("j",9) && case lookup "Help" (remoteMenuLayout configuredFrame) of Just rows->any (\(title,shortcut,token)->title=="Contents" && shortcut==("j",9) && token>=length nativeCommands) rows; _->False)
  let modal=prompt "Edit" Information [Input "Text" "draft" 5] configured
  modalFrame<-either error pure (parseRemoteFrame (object (frameMetadata "/tmp" modal)) (replicate 25 (toJSON ([]::[Value]))))
  check "registered native Help has no source accelerator through a modal" (nativeMenuShortcut modal helpCommand==("",0) && all (\(_,rows)->all (\(_,shortcut,_)->shortcut==("",0)) rows) (remoteMenuLayout modalFrame))
  nativeInput<-either error pure (parseEither parseInput nativePacket)
  check "native emitted packet reaches same actual host invocation" (snd (applyInput nativeInput initial)==[InvokeMenu extension Plugin.HumanMenu Nothing])
  let (invoked,extensionEffects)=applyInput packet initial
  check "extension declaration reaches actual transported route" (extensionEffects==[InvokeMenu extension Plugin.HumanMenu Nothing])
  opened<-run invoked extensionEffects
  check "transported extension installs independent Markdown and links" (maybe False (T.isInfixOf "Independent extension" . contents . documentBuffer) (activeDocument opened) && maybe False (not . null . documentLinks) (activeDocument opened))
  check "extension metadata cannot grant agent invocation" (case applyGuestInput packet (beginGuestInput initial) of Left _->True; _->False)
  check "contributed Help has no generationless static menu route" (case parseEither parseInput (object ["type" .= ("menu"::T.Text),"command" .= ("hide.help.contents"::T.Text)]) of Left _->True; _->False)
  helpPacket<-either error pure (parseEither parseInput (object ["type" .= ("menu"::T.Text),"command" .= Plugin.menuName helpRef,"registry" .= Plugin.menuEpoch helpRef,"generation" .= Plugin.menuGeneration helpRef]))
  withMenuCommands docs $ \nextHost->do
    nextMetadata<-Plugin.menuSnapshot (menuContributions nextHost)
    let nextDesktop=initial {contributedMenus=nextMetadata,agentMenuRefs=menuAgentReferences nextHost}
    check "prior scope same name/generation cannot target a new session registry" (null (snd (applyInput helpPacket nextDesktop)))
  check "host-permitted help preserves agent origin" (case applyGuestInput helpPacket (beginGuestInput initial) of Right (_,requests)->requests==[InvokeMenu helpRef Plugin.AgentMenu Nothing]; _->False)
  stalePacket<-either error pure (parseEither parseInput (object ["type" .= ("menu"::T.Text),"command" .= Plugin.menuName extension,"registry" .= Plugin.menuEpoch extension,"generation" .= (Plugin.menuGeneration extension+100)]))
  let oversized=object ["type" .= ("menu"::T.Text),"command" .= Plugin.menuName extension,"registry" .= Plugin.menuEpoch extension,"generation" .= (9007199254740992::Integer)]
  check "contributed wire generations are bounded" (case parseEither parseInput oversized of Left _->True; _->False)
  check "stale transported generation cannot invoke extension" (null (snd (applyInput stalePacket initial)))
#endif
  -- Retire through the host owner while an admitted result is outstanding.
  (_,pending)<-menuEffects host (\_ _->error "missing host") initial [InvokeMenu extension Plugin.HumanMenu Nothing]
  requestMenuRetirement host extension
  threadDelay 100000
  refused<-tickMenus host (\current _->pure (False,current)) pending
  check "ordered host retirement precedes late reply adoption" (activeDocument refused==Nothing && all ((/=extension) . Plugin.menuReference) (contributedMenus refused))
  (_,queuedOld)<-menuEffects host (\_ _->error "missing host") refused [InvokeMenu extension Plugin.HumanMenu Nothing]
  check "queued retired action has no document and explicit refusal" (activeDocument queuedOld==Nothing && "stale" `T.isInfixOf` status queuedOld)
  requestMenuRetirement host helpRef
  withdrawnHelp<-tickMenus host (\current _->pure (False,current)) initial
  check "retired Help fallback row paints unavailable" (not (menuCommandAvailable withdrawnHelp Help))
  check "F1 cannot bypass retired Help through preview fallback" (null (snd (handleEvent (V.EvKey (V.KFun 1) []) withdrawnHelp)))
  entered<-newEmptyMVar
  finished<-newEmptyMVar
  blocked<-newEmptyMVar
  withMenuCommands docs $ \closingHost->do
    let unit=Codec Null (const (Right ())) (const Null)
        textCodec=Codec Null (\value->case value of String valueText->Right valueText; _->Left "Expected text") String
        slow=CommandDef "example.cancel-on-close" "Closing" unit textCodec
          (\_ ()->(putMVar entered () >> takeMVar blocked >> pure (Right "# Closed")) `finally` putMVar finished ())
    command<-either (error . show) pure =<< registerCommand registry slow
    reference<-either (error . show) pure =<< Plugin.contributeMenu (menuContributions closingHost)
      (Plugin.MenuDef "example.cancel-on-close" "help" "extensions" 20 "Closing" "" False
        (Plugin.menuAction registry command (const (Right ())) (\context valueText->fmap PreparedDocument (prepareMarkdown (invocationColumns context) "/tmp/README.md" "" valueText))))
    closingMetadata<-Plugin.menuSnapshot (menuContributions closingHost)
    let closingDesktop=initial {contributedMenus=closingMetadata,agentMenuRefs=menuAgentReferences closingHost}
    _<-menuEffects closingHost (\_ _->error "missing closing worker") closingDesktop [InvokeMenu reference Plugin.HumanMenu Nothing]
    began<-timeout 1000000 (takeMVar entered)
    check "closing worker starts" (began==Just ())
  joined<-tryTakeMVar finished
  check "session scope joins cancelled worker before registry closure" (joined==Just ())
  putStrLn "live menu command checks passed"

-- Exact refs remain the keyboard owner across retirement and replacement. These
-- checks use independently typed commands and the real host publication boundary.
runtimeLifecycleChecks :: DocsCommands -> IO ()
runtimeLifecycleChecks docs=bracket temporary removePathForcibly $ \directory->withMenuCommands docs $ \host->withRegistry $ \registry->do
  original<-either (error . show) pure =<< registerExtension registry (menuContributions host)
    (\context text->fmap PreparedDocument (prepareMarkdown (invocationColumns context) "/tmp/README.md" "" text))
  metadata<-Plugin.menuSnapshot (menuContributions host)
  let check label condition=unless condition (error label)
      config=M.fromList [("terminal",M.singleton "source" (M.fromList [("example.manual",["Ctrl+Shift+J"]),("hide.focus.source",["Ctrl+Shift+U"])])),
        ("graphical",M.singleton "source" (M.singleton "example.manual" ["Ctrl+Shift+J"])),
        ("macos",M.singleton "source" (M.singleton "example.manual" ["Cmd+Shift+J"]))]
      base=(initialDesktop (80,25)) {defaultDirectory=Just directory,contributedMenus=metadata,menusActive=True,agentMenuRefs=menuAgentReferences host}
      maps=either (error . show) id (configuredBindings (contributedBindingCommands base) config)
      bound=base {keyBindings=maps}
      key=V.EvKey (V.KChar 'j') [V.MCtrl,V.MShift]
      command d=effectiveBindings d >>= \bindings->Bindings.bindingAction bindings (V.KChar 'j') [V.MCtrl,V.MShift]
      wait bindings expected current=do
        next<-tickKeybindings bindings current
        if command next==Just expected then pure next else threadDelay 1000 >> wait bindings expected next
      bounded bindings expected current=timeout 5000000 (wait bindings expected current) >>= maybe (error "runtime catalogue refresh did not settle") pure
  check "catalogue capture cannot inspect document payloads"
    (contributedBindingCommands base {buffers=error "catalogue forced buffers"}==contributedBindingCommands base)
#if defined(WITH_WEB) || defined(WITH_REMOTE)
  let frameFor d=either error id (parseRemoteFrame (object (frameMetadata directory d)) (replicate 25 (toJSON ([]::[Value]))))
      packet reference=object ["type" .= ("menu"::T.Text),"command" .= Plugin.menuName reference,"registry" .= Plugin.menuEpoch reference,"generation" .= Plugin.menuGeneration reference]
      oldFrame=frameFor bound
      mac=bound {nativeMac=True,videoMode=Just 3}
      graphical=bound {videoMode=Just 3}
      originalCommand=RegisteredMenu original False
      label d=menuShortcut d (MenuItem "Extension manual" "F12" originalCommand)
  check "terminal and graphical labels use effective contributed map"
    (label bound=="Ctrl+Shift+J" && label graphical=="Ctrl+Shift+J")
  check "Mac label and native accelerator use the same contributed map"
    (label mac=="⇧⌘J" && nativeMenuShortcut mac originalCommand==("j",9))
  check "terminal and native decoded key packets retain exact contribution"
    (remoteBindingInput oldFrame key Nothing==Just (packet original) && (decodeKey (fromEnum 'j') 9 >>= \input->remoteBindingInput (frameFor mac) input Nothing)==Just (packet original))
  check "Mac contribution metadata publishes effective label"
    (case filter ((=="example.manual") . contributionId) (remoteContributions (frameFor mac)) of item:_->contributionKey item==label mac; _->False)
  check "remote Cocoa menu keeps configured contribution accelerator"
    (case lookup "Help" (remoteMenuLayout (frameFor mac)) of Just rows->any (\(title,shortcut,_)->title=="Extension manual" && shortcut==("j",9)) rows; _->False)
  let modal=prompt "Edit" Information [Input "Text" "draft" 5] mac
  check "modal published map cannot route contributed key"
    ((decodeKey (fromEnum 'j') 9 >>= \input->remoteBindingInput (frameFor modal) input Nothing)==Nothing && nativeMenuShortcut modal originalCommand==("",0))
  let builtinKey=V.EvKey (V.KChar 'u') [V.MCtrl,V.MShift]
  check "non-menu builtin chord keeps its raw transport route"
    (lookup "Ctrl+Shift+U" (remoteBindings oldFrame)==Just "hide.focus.source" && remoteBindingInput oldFrame builtinKey (terminalEventInput builtinKey)==terminalEventInput builtinKey)
  oldInput<-either error pure (parseEither parseInput (packet original))
#endif
  withKeybindings config (contributedBindingCommands base) $ \bindings->do
    retired<-retireMenuFromHost host original bound
    check "retired chord refuses before worker refresh" (null (snd (handleEvent key retired)))
    inert<-bounded bindings (Disabled "example.manual") retired
    check "retirement preserves inert ownership without invoking a default" (null (snd (handleEvent key inert)))
    withRegistry $ \replacementRegistry->do
      replacement<-either (error . show) pure =<< registerExtension replacementRegistry (menuContributions host)
        (\context text->fmap PreparedDocument (prepareMarkdown (invocationColumns context) "/tmp/README.md" "" text))
      publication<-publishMenuFromHost host replacement
      check "replacement publishes through existing host" (publication==Right ())
      published<-tickMenus host (\current _->pure (False,current)) retired
#if defined(WITH_WEB) || defined(WITH_REMOTE)
      let pendingFrame=frameFor published
          pendingKey=remoteBindingInput pendingFrame key (terminalEventInput key)
      check "actual pending publication frame consumes inert contributed chord" (pendingKey==Nothing)
      check "pending frame projects explicit inert ownership" (lookup "Ctrl+Shift+J" (remoteBindings pendingFrame)==Just "")
      check "actual retired old-ref frame consumes missing contribution chord" (remoteBindingInput (frameFor retired) key (terminalEventInput key)==Nothing)
      check "actual retired inert-map frame consumes missing contribution chord" (remoteBindingInput (frameFor inert) key (terminalEventInput key)==Nothing)
#endif
      replaced<-bounded bindings (RegisteredMenu replacement False) published
      let (chosen,actions)=handleEvent key replaced
      check "replacement refresh routes current exact registration" (actions==[InvokeMenu replacement Plugin.HumanMenu Nothing] && replacement/=original)
#if defined(WITH_WEB) || defined(WITH_REMOTE)
      check "queued old-frame shortcut cannot redirect to replacement" (null (snd (applyInput oldInput replaced)))
      let disabledOldFrame=oldFrame {remoteMenus=replicate (length (remoteMenus oldFrame)) False}
      check "disabled old-frame shortcut retains its stamp rather than raw-key fallback" (remoteBindingInput disabledOldFrame key Nothing==Just (packet original))
      check "replacement frame shortcut stamps new registration" (remoteBindingInput (frameFor replaced) key Nothing==Just (packet replacement))
#endif
      (_,queued)<-menuEffects host (\_ _->error "bound replacement missed host") chosen actions
      let document current=do
            next<-tickMenus host (\current _->pure (False,current)) current
            if activeDocument next/=Nothing then pure next else threadDelay 1000 >> document next
      opened<-timeout 5000000 (document queued) >>= maybe (error "bound replacement callback did not finish") pure
      check "replacement chord invokes actual typed callback" (maybe False (T.isInfixOf "Independent extension" . contents . documentBuffer) (activeDocument opened))

temporary :: IO FilePath
temporary=do
  directory<-getTemporaryDirectory
  (path,handle)<-openTempFile directory "hide-runtime-bindings"
  hClose handle
  removeFile path
  createDirectory path
  pure path
