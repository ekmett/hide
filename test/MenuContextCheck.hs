{-# LANGUAGE CPP, OverloadedStrings #-}
module MenuContextCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async,wait,poll)
import Control.Exception (bracket,evaluate,try,ErrorCall,IOException)
import Control.Monad (unless,replicateM_,foldM)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.List (findIndex)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.Directory
import System.FilePath ((</>))
import System.IO
import System.Timeout (timeout)
import qualified Hide.Buffer as B
import Hide.Buffer (newBuffer,contents,caret,bufferContent,contentLineOffset)
import Hide.Commands (configuredBindings)
import Hide.DocsMCP
import Hide.Files (FileState(..))
import Hide.GuestAccess (beginGuestInput,validateGuestEffects)
import Hide.MenuCommands
import Hide.Model
import Hide.Plugin.Command
import qualified Hide.Plugin.Menu as Plugin
import Hide.Links (prepareMarkdown)
import Hide.Protocol
import Hide.Render (snapshot)
import Hide.RemoteWindow
import Hide.Window (nativeCommands,nativeCommandsFor,nativeMenuEventFor)
import MenuExtension

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->withDocsCommands $ \docs->withRegistry $ \registry->withMenuCommands docs $ \host->do
  let check label ok=unless ok (error label)
      right :: Show e => Either e a -> a
      right=either (error . show) id
      path=root </> "Main.hs"
      problem=Diagnostic path Nothing 1 2 1 "problem"
      pollUntil predicate original=do
        let loop d=do next<-tickMenus host d; if predicate next then pure next else threadDelay 1000 >> loop next
        timeout 5000000 (loop original) >>= maybe (error "context worker did not complete") pure
      run d requests=do
        (_,pending)<-menuEffects host (\_ _->error "context action missed scoped worker") d requests
        pollUntil (\next->status next/=status pending || not (problemsFocused next)) pending
  writeFile path "disk\n😀text\n"
  metadata<-Plugin.menuSnapshot (menuContributions host)
  let source=insertText "unsaved " (addDocument (Just (FileState path Nothing)) (newBuffer "memory\n😀source\n") (initialDesktop (80,25)))
      opaque=source {buffers=M.map (\doc->doc {documentBuffer=(documentBuffer doc) {B.undoStack=error "navigation retained Undo",B.redoStack=error "navigation retained Redo"}}) (buffers source)}
      sourceId=maybe (error "missing source") bufferId (activeWindow source)
      pane=(setDiagnostics [problem] (setProblemsVisible True opaque)) {problemsFocused=True,contributedMenus=metadata,menusActive=True,agentMenuRefs=menuAgentReferences host}
      popup=openContext MessagesContext 8 5 pane
      (chosen,requests)=handleEvent (V.EvKey V.KEnter []) popup
      reference=case [Plugin.menuReference item | item<-metadata,Plugin.menuName (Plugin.menuReference item)=="hide.messages.go-to"] of ref:_->ref; _->error "missing Messages contribution"
      target=contextTarget popup
      packet ref=object ["type" .= ("menu"::T.Text),"command" .= Plugin.menuName ref,"registry" .= Plugin.menuEpoch ref,"generation" .= Plugin.menuGeneration ref]
  check "Messages popup composes the actual registered source action" (requests==[InvokeMenu reference Plugin.HumanMenu target])
  check "Go to source key action resolves the same registry identity" (snd (runCommand GoToMessage pane)==requests)
  check "unselected/empty projection retains popup but disables source navigation"
    (let empty=openContext MessagesContext 8 5 (setDiagnostics [] pane) in contextTargetCurrent empty && not (commandEnabled empty (RegisteredMenu reference False)))
  let profiles=right (configuredBindings [] (M.singleton "macos" (M.singleton "messages" (M.singleton "hide.messages.go-to" ["Cmd+Shift+J"]))))
      configured=pane {nativeMac=True,videoMode=Just 3,keyBindings=profiles}
  check "registered Messages action uses its effective canonical binding label"
    (commandBindingKeys configured (RegisteredMenu reference False)==["Cmd+Shift+J"])
  let configuredPopup=openContext MessagesContext 8 5 configured
      contextRow d n=case contextMenu d of
        Just (rect,_) -> T.lines (snapshot d) !! (top rect+1+n)
        _ -> error "missing configured popup"
      removed=configured {keyBindings=right (configuredBindings [] (M.singleton "macos" (M.singleton "messages" (M.singleton "hide.messages.go-to" []))))}
  check "registered context row paints its effective Mac shortcut"
    ("⇧⌘J" `T.isInfixOf` contextRow configuredPopup 0)
  check "registered context row clears an explicitly unbound shortcut"
    (not ("⇧⌘J" `T.isInfixOf` contextRow (openContext MessagesContext 8 5 removed) 0))
  check "remapped Messages key resolves the same captured contributed action"
    (snd (handleEvent (V.EvKey (V.KChar 'j') [V.MMeta,V.MShift]) configured)==requests)
  let invalidSelection=openContext MessagesContext 8 5 pane {problemsSelected=10}
  check "projection-only actions remain usable without a selected message"
    (contextTargetCurrent invalidSelection && commandEnabled invalidSelection CopyAllMessages && commandEnabled invalidSelection Problems && not (commandEnabled invalidSelection Copy) && not (commandEnabled invalidSelection (RegisteredMenu reference False)))
  input<-either error pure (parseEither parseInput (packet reference))
  let mouseInput=case contextMenu popup of Just (rect,_)->Mouse "down" (left rect+2) (top rect+1) 0 1 []; _->error "missing popup"
  check "browser canvas context click reaches the same captured typed route" (snd (applyInput mouseInput popup)==requests)
  check "browser exact contribution route freezes the host diagnostic target" (snd (applyInput input popup)==requests)
  let transport=object (frameMetadata root popup++["menuCommands" .= map fst protocolMenuCommands])
  frame<-either error pure (parseRemoteFrame transport (replicate 25 (toJSON ([]::[Value]))))
  let index=maybe (error "missing native context token") id (findIndex ((==reference) . Plugin.menuReference) metadata)
      token=length nativeCommands+index
  nativePacket<-maybe (error "context contribution missing native transport") pure (remoteNativeMenuInput frame 31 [11,token,31])
  check "native transport preserves exact context registration" (nativePacket==packet reference && nativeMenuEventFor (nativeCommandsFor popup) 31 [11,token,31]==Just (RegisteredMenu reference True))
  check "native context lifetime refuses prior catalogue events" (remoteNativeMenuInput frame 32 [11,token,31]==Nothing)
  navigated<-run chosen requests
  let expected=contentLineOffset (bufferContent (documentBuffer (buffers pane M.! sourceId))) 1+1
  check "navigation focuses dirty existing source without reading disk baseline"
    (not (problemsFocused navigated) && fmap bufferId (activeWindow navigated)==Just sourceId && activeText navigated==activeText source && maybe False ((==expected).caret.selection) (activeWindow navigated))
  -- A replacement at the same numeric revision must not redirect a prepared read.
  (_,pending)<-menuEffects host (\_ _->error "missing context worker") chosen requests
  let replaced=pending {buffers=M.adjust (\doc->doc {documentBuffer=(newBuffer "replacement\nsource") {B.revision=B.revision (documentBuffer doc)}}) sourceId (buffers pending)}
  refused<-pollUntil (T.isInfixOf "changed" . status) replaced
  check "prepared navigation refuses changed open buffer identity" (problemsFocused refused && activeText refused=="replacement\nsource")
  (_,pendingGeneration)<-menuEffects host (\_ _->error "missing context worker") chosen requests
  expired<-pollUntil (T.isInfixOf "expired" . status) (setDiagnostics [problem] pendingGeneration)
  check "diagnostic refresh refuses late navigation adoption" (problemsFocused expired)
  (_,pendingFocus)<-menuEffects host (\_ _->error "missing context worker") chosen requests
  lostFocus<-pollUntil (T.isInfixOf "expired" . status) pendingFocus {problemsFocused=False}
  check "changed Messages input owner refuses late navigation adoption" (fmap bufferId (activeWindow lostFocus)==Just sourceId)
  -- An unopened source is fully prepared off-lock, then installed as an editable
  -- document with the same established Files loader/decoder.
  let unopened=(setDiagnostics [problem] (setProblemsVisible True (initialDesktop (80,25)))) {problemsFocused=True,contributedMenus=metadata,menusActive=True}
      (newChosen,newRequests)=handleEvent (V.EvKey V.KEnter []) (openContext MessagesContext 8 5 unopened)
  let publicAgentPane=newChosen {agentMenuRefs=menuAgentReferences host}
      agentRequests=case applyGuestInput input (beginGuestInput publicAgentPane) of
        Right (_,effects)->effects
        Left err->error (T.unpack err)
  check "host-permitted diagnostic source preserves attributed agent origin"
    (agentRequests==[InvokeMenu reference Plugin.AgentMenu (contextTarget newChosen)])
  validateGuestEffects publicAgentPane agentRequests
  agentOpened<-run publicAgentPane agentRequests
  check "agent diagnostic navigation retains public source access" (activeText agentOpened=="disk\n😀text\n")
  let privatePane=publicAgentPane {guestPrivatePaths=[path]}
  denied<-try (validateGuestEffects privatePane agentRequests) :: IO (Either IOException ())
  check "resolved captured agent target is checked before dispatch" (case denied of Left _->True; _->False)
  workerDenied<-run privatePane agentRequests
  check "navigation worker rechecks immutable path authority before loading"
    (problemsFocused workerDenied && T.isInfixOf "protected" (status workerDenied) && activeText workerDenied==activeText privatePane)
  (_,beforeProtection)<-menuEffects host (\_ _->error "missing context worker") publicAgentPane agentRequests
  afterProtection<-pollUntil (T.isInfixOf "now protected" . status) beforeProtection {guestPrivatePaths=[path]}
  check "prepared canonical source is rechecked against current policy at adoption"
    (problemsFocused afterProtection && activeText afterProtection==activeText publicAgentPane)
  let openAgentRequests=[InvokeMenu reference Plugin.AgentMenu target]
  (_,openPending)<-menuEffects host (\_ _->error "missing context worker") chosen openAgentRequests
  protectedOpen<-pollUntil (T.isInfixOf "changed" . status) openPending {buffers=M.adjust (\doc->doc {documentLabel=Just "Agent request"}) sourceId (buffers openPending)}
  check "open source that becomes private refuses delayed agent focus" (problemsFocused protectedOpen)
  let alias=root </> "alias.hs"
  createFileLink path alias
  let aliasPane=(setDiagnostics [problem {diagnosticPath=alias}] privatePane)
      (aliasChosen,_) = handleEvent (V.EvKey V.KEnter []) (openContext MessagesContext 8 5 aliasPane)
      aliasTarget=contextTarget (openContext MessagesContext 8 5 aliasPane)
      aliasRequests=[InvokeMenu reference Plugin.AgentMenu aliasTarget]
  aliasDenied<-try (validateGuestEffects aliasChosen aliasRequests) :: IO (Either IOException ())
  check "effect validation resolves symbolic links before agent path policy" (case aliasDenied of Left _->True; _->False)
  aliasWorkerDenied<-run aliasChosen aliasRequests
  check "worker canonicalization cannot redirect agent navigation to a protected file"
    (problemsFocused aliasWorkerDenied && T.isInfixOf "protected" (status aliasWorkerDenied))
  humanPrivate<-run privatePane newRequests
  check "human navigation retains normal access to protected source" (activeText humanPrivate=="disk\n😀text\n")
  diskOpened<-run newChosen newRequests
  check "unopened diagnostic navigation loads and positions editable source"
    (activeText diskOpened=="disk\n😀text\n" && maybe False ((==Nothing).documentLabel) (activeDocument diskOpened) && maybe False ((==6).caret.selection) (activeWindow diskOpened))
  let farPane=setDiagnostics [problem {diagnosticRow=1000000}] unopened
      (farChosen,farRequests)=handleEvent (V.EvKey V.KEnter []) (openContext MessagesContext 8 5 farPane)
  farOpened<-run farChosen farRequests
  check "beyond-EOF diagnostics clamp prepared caret and viewport to live source"
    (maybe False (\window->caret (selection window)==B.bufferLength (documentBuffer (buffers farOpened M.! bufferId window)) && scrollRow window<B.bufferLineCount (documentBuffer (buffers farOpened M.! bufferId window))) (activeWindow farOpened))
  let longPath=root </> "Wide.hs"
  writeFile longPath (replicate 200 'x'++"\n")
  let longPane=setDiagnostics [problem {diagnosticPath=longPath,diagnosticRow=0,diagnosticColumn=180}] unopened
      (longChosen,longRequests)=handleEvent (V.EvKey V.KEnter []) (openContext MessagesContext 8 5 longPane)
  longOpened<-run longChosen longRequests
  check "far-column diagnostic keeps the prepared caret visible"
    (maybe False (\window->caret (selection window)==180 && scrollColumn window>0 && 180>=scrollColumn window && 180<scrollColumn window+width (bounds window)-2) (activeWindow longOpened))
  (_,opening)<-menuEffects host (\_ _->error "missing context worker") newChosen newRequests
  let intervening=(addDocument (Just (FileState path Nothing)) (newBuffer "newly opened unsaved") opening) {problemsFocused=True}
  protected<-pollUntil (T.isInfixOf "changed" . status) intervening
  check "disk result never replaces a newly opened buffer" (activeText protected=="newly opened unsaved")
  -- A separately declared extension joins a live popup after startup via an
  -- immutable ordered delta; there is no core constructor addition.
  extension<-right <$> registerContextExtension "example.context" registry (menuContributions host)
    (\context text->PreparedDocument <$> prepareMarkdown (invocationColumns context) path "" text)
  prepared<-publishMenuFromHost host extension
  check "runtime publication prepares exact bounded metadata" (prepared==Right ())
  published<-tickMenus host popup
  check "runtime publication closes positional popup and preserves other registrations"
    (contextMenu published==Nothing && menu published==Nothing && all (`elem` map Plugin.menuReference (contributedMenus published)) [reference,extension])
  let contextPopup=openContext MessagesContext 8 5 published
      extensionIndex=maybe (error "missing contributed context row") id (findIndex (\(_,command)->case command of RegisteredMenu ref _->ref==extension; _->False) (contextItemsFor contextPopup))
      extensionPopup=contextPopup {contextMenu=fmap (\(rect,_)->(rect,extensionIndex)) (contextMenu contextPopup)}
      (extensionChosen,extensionRequests)=handleEvent (V.EvKey V.KEnter []) extensionPopup
  check "separate context declaration invokes through actual popup dispatch" (extensionRequests==[InvokeMenu extension Plugin.HumanMenu (contextTarget contextPopup)])
  extensionInput<-either error pure (parseEither parseInput (packet extension))
  check "live context extension reaches transported browser route" (snd (applyInput extensionInput contextPopup)==extensionRequests)
  extended<-run extensionChosen extensionRequests
  check "live context extension executes independent typed handler" (maybe False (T.isInfixOf "Context extension" . contents . documentBuffer) (activeDocument extended))
  -- Interleaved publication/withdrawal uses deltas, never a stale whole snapshot.
  second<-right <$> registerExtension registry (menuContributions host)
    (\context text->PreparedDocument <$> prepareMarkdown (invocationColumns context) path "" text)
  _<-publishMenuFromHost host second
  requestMenuRetirement host extension
  ordered<-tickMenus host contextPopup
  check "ordered deltas cannot clobber intervening registrations"
    (second `elem` map Plugin.menuReference (contributedMenus ordered) && extension `notElem` map Plugin.menuReference (contributedMenus ordered))
  check "withdrawn context packet refuses queued admission" (null (snd (applyInput extensionInput ordered)))
  -- Lazy extension metadata is forced on the publication caller, never owner tick.
  let unit=Codec Null (const (Right ())) (const Null)
  lazyCommand<-right <$> registerCommand registry (CommandDef "example.lazy" "Lazy" unit unit (\_ ()->pure (Right ())))
  lazyRef<-right <$> Plugin.contributeMenu (menuContributions host)
    (Plugin.MenuDef "example.lazy" "context.messages" "extensions" 40 "Lazy" "" (error "metadata leaked onto owner")
      (Plugin.menuAction registry lazyCommand (const (Right ())) (\_ ()->error "lazy action must not execute")))
  publication<-async (try (publishMenuFromHost host lazyRef) :: IO (Either ErrorCall (Either Plugin.MenuError ())))
  failed<-wait publication
  check "publication caller evaluates lazy metadata before enqueue" (case failed of Left _->True; _->False)
  unchanged<-tickMenus host ordered
  check "owner tick receives no failed lazy publication" (lazyRef `notElem` map Plugin.menuReference (contributedMenus unchanged))
  -- A bounded queue backpressures workers and a bounded drain leaves later
  -- deltas for another tick. Direct owner retirement never waits for that queue.
  replacement<-right <$> registerContextExtension "example.context-next" registry (menuContributions host)
    (\context text->PreparedDocument <$> prepareMarkdown (invocationColumns context) path "" text)
  replicateM_ 16 (publishMenuFromHost host second >>= either (error . show) pure)
  _<-publishMenuFromHost host replacement
  limited<-tickMenus host unchanged
  check "one owner tick bounds publication drain to sixteen deltas"
    (replacement `notElem` map Plugin.menuReference (contributedMenus limited))
  later<-tickMenus host limited
  check "later tick adopts remaining live publication"
    (replacement `elem` map Plugin.menuReference (contributedMenus later))
  replicateM_ 256 (publishMenuFromHost host second >>= either (error . show) pure)
  blocked<-async (publishMenuFromHost host second)
  threadDelay 1000
  backpressure<-poll blocked
  check "full bounded publication queue backpressures registration worker" (case backpressure of Nothing->True; _->False)
  direct<-timeout 1000000 (retireMenuFromHost host replacement later)
  retired<-maybe (error "owner retirement blocked on full publication queue") pure direct
  check "direct owner withdrawal needs no publication capacity"
    (replacement `notElem` map Plugin.menuReference (contributedMenus retired))
  resumed<-tickMenus host retired
  released<-timeout 1000000 (wait blocked)
  check "bounded owner drain releases waiting producer" (released==Just (Right ()))
  _<-foldM (\current _->tickMenus host current) resumed [1..17::Int]
  -- Existing queued work also checks the source contribution lifetime at adoption.
  (_,late)<-menuEffects host (\_ _->error "missing context worker") chosen requests
  requestMenuRetirement host reference
  withdrawn<-pollUntil (T.isInfixOf "expired" . status) late
  check "context contribution retirement refuses late navigation and paint"
    (problemsFocused withdrawn && not (commandEnabled withdrawn GoToMessage))
  _<-evaluate (diagnosticsGeneration unchanged)
  putStrLn "live context menu checks passed"
  where
    temporary=do base<-getTemporaryDirectory; (path,h)<-openTempFile base "hide-menu-context"; hClose h; removeFile path; createDirectory path; canonicalizePath path
