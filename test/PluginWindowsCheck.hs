{-# LANGUAGE CPP, OverloadedStrings #-}
module PluginWindowsCheck (checks) where
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Control.Monad (unless)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.Timeout (timeout)
import Hide.Buffer (newBuffer,contents,Selection(..))
import Hide.Commands (configuredBindings,contributedBindingCommands)
import Hide.DocsMCP
import Hide.GuestAccess (guestKeyboardAllowed,pointerAllowedAt,readableAt)
import Hide.MenuCommands
import qualified Hide.Plugin.Window as W
import Hide.PluginWindowHost (adoptWindowUpdate)
import Hide.Model
import Hide.Plugin.Command (withRegistry)
import qualified Hide.Plugin.Menu as P
import Hide.Render (snapshot,renderKey)
import WindowExtension
#ifdef WITH_PROTOCOL
import Data.Aeson (object,(.=))
import Data.Aeson.Types (parseEither)
import Hide.Protocol hiding (Paste)
import Hide.RemoteWindow
import Hide.RemoteTerminal (terminalEventInput)
#endif

checks :: IO ()
checks=W.withWindowScope $ \scope->withDocsCommands $ \docs->withRegistry $ \registry->withMenuCommands docs $ \host->do
  let check label condition=unless condition (fail label)
      core _ _=error "plugin menu missed typed owner"
  reference<-either (error . show) pure =<< registerNotes registry (menuContributions host) scope (pure ()) PreparedWindow
  catalogue<-P.menuSnapshot (menuContributions host)
  let source=addDocument Nothing (newBuffer "background source") (initialDesktop (80,25))
      initial=source {contributedMenus=catalogue,menusActive=True,agentMenuRefs=menuAgentReferences host}
      bindings=either (error . show) id (configuredBindings (contributedBindingCommands initial) M.empty)
      (chosen,requests)=runCommand (RegisteredMenu reference False) initial {keyBindings=bindings}
      settle desktop=do
        next<-tickMenus host desktop
        if activePluginWindow next/=Nothing then pure next else threadDelay 1000 >> settle next
  (_,queued)<-menuEffects host core chosen requests
  opened<-timeout 5000000 (settle queued) >>= maybe (fail "plugin window did not open") pure
  check "typed extension opens a window without a source document"
    (length (windows opened)==2 && M.size (buffers opened)==1 && activeDocument opened==Nothing && maybe False ((==Nothing) . bufferId) (activeWindow opened))
  check "shared frame renders host title and prepared plugin text"
    (all (`T.isInfixOf` snapshot opened) ["Plugin notes","Independent text","Second row"])
  let (selected,_)=handleEvent (V.EvKey (V.KChar 'a') [V.MCtrl]) opened
      (copied,_)=handleEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) selected
  check "effective source-context copy selects semantic plugin text"
    (clipboard copied=="Independent text\nSecond row")
  let attempted=foldl (\desktop action->fst (runCommand action desktop)) selected [Save,SaveAs,Cut,Paste,Undo,Redo,Replace,Complete]
  check "plugin commands cannot edit background source"
    (fmap (contents . documentBuffer) (M.lookup 1 (buffers attempted))==Just "background source" && M.size (buffers attempted)==1 && dialog attempted==Nothing)
  check "guest content/controls remain private without explicit semantics grant"
    (not (guestKeyboardAllowed opened) && not (pointerAllowedAt opened 3 3) && not (readableAt opened 3 3))
  let modal=prompt "Details" Information [TextArea "Draft" True (newBuffer "modal draft") (Selection 0 5) 0 0] opened
      (modalCopy,_)=runCommand Copy modal
  check "modal clipboard uses its field instead of background plugin text" (clipboard modalCopy=="modal")
#ifdef WITH_PROTOCOL
  let packet value=either error id (parseEither parseInput value)
      input event=maybe (error "missing terminal packet") packet (terminalEventInput event)
      native value=maybe (error "missing native packet") packet value
      wireSelected=fst (applyInput (input (V.EvKey (V.KChar 'a') [V.MCtrl])) opened)
      wireCopy=fst (applyInput (input (V.EvKey (V.KChar 'c') [V.MCtrl])) wireSelected)
      mac=opened {nativeMac=True}
      macSelected=fst (applyInput (native (nativeKeyInput (fromEnum 'a') 8)) mac)
      macCopy=fst (applyInput (native (nativeKeyInput (fromEnum 'c') 8)) macSelected)
      browserCopy=fst (applyInput (BrowserCommand Copy) selected {browserFrontend=True})
  frame<-either fail pure (parseRemoteFrame (object (frameMetadata "/tmp" opened)) (frameRows opened))
  check "actual remote frame includes plugin title/window and parsed cells" (remoteTitle frame=="th Plugin notes" && length (remoteWindows frame)==2)
  check "native, terminal and browser clipboard routes agree on plugin text"
    (all ((==clipboard copied) . clipboard) [wireCopy,macCopy,browserCopy])
#endif
  let private=opened {streamerMode=True}
  check "private plugin title is masked in application and Dock metadata"
    (applicationTitle "/tmp" private=="th Private plugin window" && all (\(_,title,_,_)->title/="Plugin notes") (editorWindowEntries private))
#ifdef WITH_PROTOCOL
  privateFrame<-either fail pure (parseRemoteFrame (object (frameMetadata "/tmp" private)) (frameRows private))
  check "streamer frame metadata cannot leak private plugin title"
    (remoteTitle privateFrame=="th Private plugin window" && not ("Plugin notes" `T.isInfixOf` T.pack (show (remoteWindows privateFrame))))
#endif
  key<-renderKey opened
  wrapped<-renderKey opened {pluginWindows=M.map id (pluginWindows opened)}
  check "prepared payload wrapper retains shallow render identity" =<< evaluate (key==wrapped)
  let (closed,_)=runCommand Close opened
  check "closing plugin view preserves source and retires its content"
    (length (windows closed)==1 && M.null (pluginWindows closed) && activeDocument closed/=Nothing && M.size (buffers closed)==1)
  prepared<-W.prepareTextWindow "Updated notes" "a\x0301\nUpdated text and a longer selected text"
  let reference=case windowContent <$> activeWindow opened of Just (PluginContent ref)->ref; _->error "missing plugin ref"
  refresh<-W.refreshTextWindow reference prepared >>= maybe (fail "live refresh refused") pure
  refreshed<-adoptWindowUpdate P.HumanMenu refresh selected
  check "refresh preserves host geometry and semantic selection"
    (fmap (\w->(windowId w,bounds w,selection w)) (activeWindow refreshed)==fmap (\w->(windowId w,bounds w,selection w)) (activeWindow selected) && "Updated text" `T.isInfixOf` snapshot refreshed)
  let (start,_)=runCommand SelectAll refreshed
      caretStart=start {windows=map (\w->w {selection=Selection 0 0}) (windows start)}
      (advanced,_)=handleEvent (V.EvKey V.KRight []) caretStart
  check "plugin navigation respects combining character boundaries" (fmap (caret . selection) (activeWindow advanced)==Just 2)
  stale<-W.refreshTextWindow reference prepared >>= maybe (fail "prepare queued refresh") pure
  afterClosed<-adoptWindowUpdate P.HumanMenu stale closed
  check "queued refresh cannot resurrect a closed instance" (windows afterClosed==windows closed && M.null (pluginWindows afterClosed))
  actorPreparedDummy<-W.prepareTextWindow "Duplicate" "text"
  originalUpdate<-W.openTextWindow scope actorPreparedDummy >>= maybe (fail "prepare duplicate open") pure
  originalOpened<-adoptWindowUpdate P.HumanMenu originalUpdate source
  duplicate<-adoptWindowUpdate P.HumanMenu originalUpdate originalOpened
  check "duplicate open reply cannot create another window" (length (windows duplicate)==length (windows originalOpened))
  actorPrepared<-W.prepareTextWindow "Private notes" "private"
  actorUpdate<-W.openTextWindow scope actorPrepared >>= maybe (fail "prepare actor update") pure
  denied<-adoptWindowUpdate P.AgentMenu actorUpdate source
  modalDenied<-adoptWindowUpdate P.HumanMenu actorUpdate modal
  check "shared host adapter preserves agent and modal protection" (windows denied==windows source && windows modalDenied==windows modal)
  retired<-W.withWindowScope $ \short->W.openTextWindow short actorPrepared >>= maybe (fail "prepare retired update") pure
  retiredResult<-adoptWindowUpdate P.HumanMenu retired source
  check "retired scope refuses escaped publication" (windows retiredResult==windows source)
  putStrLn "plugin window checks passed"
