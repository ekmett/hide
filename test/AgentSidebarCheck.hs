-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : AgentSidebarCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module AgentSidebarCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
import Data.IORef
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO (openTempFile,hClose)
import System.Timeout (timeout)
import GHC.Stack (HasCallStack,callStack,prettyCallStack)
import Hide.Autocomplete (withAutocomplete,autocompleteEffects,tickAutocomplete)
import Hide.MCPPermissions (readAutocompleteFor)
import qualified Hide.AgentUI
import qualified Hide.Plugin.Session as Plugin
import qualified Hide.AgentDirectoryHost as AgentDirectory
import qualified Hide.Plugin.AgentDirectory as Directory
import Hide.AgentSidebarTypes
import qualified Hide.AgentHub as AH
import qualified Hide.AgentRuntime as AR
import Hide.App (applyEffects)
import Hide.MenuCommands
import Hide.DocumentationHost (withDocsCommands)
import Hide.Commands (configuredBindings)
import Hide.Buffer (Selection(..),newBuffer,contents)
import Hide.Plugin.Command (withRegistry,registerCommand,CommandDef(..),Codec(..),CommandError(..))
import qualified Hide.Plugin.Sidebar as PluginSidebar
import qualified FormExtension
import Hide.Plugin.BufferHost (captureVersion,versionCurrent)
import qualified Hide.Plugin.Form as Form
import qualified Hide.Plugin.ConversationSession as ConversationSession
import qualified Hide.Plugin.Menu as Menu
import Hide.GuestAccess (readableAt,streamerReadableAt,guestKeyboardAllowed,guestEffectsAllowed)
import qualified Hide.AgentTranscript as AgentTranscript
import Hide.Conversation
import qualified Hide.Consoles as C
import Hide.Model
import Hide.Sidebar
import Hide.SidebarCommands
import Hide.Session
import qualified Hide.Plugin.Tree as P
import qualified AgentIntegrationCheck

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->
  environment "XDG_CONFIG_HOME" (Just (root </> "config")) $
  environment "XDG_DATA_HOME" (Just (root </> "data")) $ do
    let config=root </> "config" </> "thc-edit"
        script=root </> "provider.py"
    createDirectoryIfMissing True config
    writeFile script AgentIntegrationCheck.fixture
    BL.writeFile (config </> "agents.json") (encode (object ["executable" .= ("python3"::T.Text),"arguments" .= [script]]))
    writeFile (root </> "thc.toml") (unlines ["[editor.autocomplete]","provider = 'acp'","executable = 'python3'","arguments = '"++show [script]++"'","debug = false"])
    record<-newSessionRecord Nothing ["--",root]
    rememberSession record
    environment "THC_EDIT_SESSION" (Just (sessionId record)) $ withSidebarCommands $ \host->C.withConsoles $ \consoles -> withConversationAt (Just AgentTranscript.presentConversation) (Plugin.pluginPrimaryInput Hide.AgentUI.plugin) (Plugin.pluginChildInput Hide.AgentUI.plugin) consoles root $ \conversation->
      withDocsCommands $ \docs->withConversationMenuCommands docs conversation $ \menuHost->
      withAutocomplete (Plugin.pluginCompletionProvider Hide.AgentUI.plugin) [tool | Plugin.CompletionTool tool<-Plugin.pluginTools Hide.AgentUI.plugin] (Plugin.pluginCompletionInput Hide.AgentUI.plugin) root $ \autocomplete->Plugin.withPlugins [Hide.AgentUI.plugin] (Plugin.Session (sidebarCapabilities host) (AgentDirectory.agentDirectory (AR.agentHub (conversationAgents conversation)) autocomplete) SidebarAgent (menuSidebarCapabilities menuHost host) sidebarConversation SidebarConversation sidebarSelectedAgent) $ do
        createRequests<-newIORef []
        agentRequests<-newIORef []
        sessionRequests<-newIORef []
        let core desktop effects=do
              modifyIORef' agentRequests (++[request | AgentSidebarAction request<-effects])
              modifyIORef' createRequests (++[(workspace,name,task) | AgentSidebarAction (CreateAgent workspace name task)<-effects])
              changed<-autocompleteEffects autocomplete (conversationEffects conversation applyEffects) desktop effects
              modifyIORef' sessionRequests (++[request | ConversationSessionAction request<-effects])
              pure changed
            runtime=sidebarEffects host (menuEffects menuHost core)
            tick d=tickConversation conversation d >>= tickAutocomplete autocomplete >>= tickMenus menuHost runtime >>= tickSidebar host runtime
            act (d,effects)=snd <$> runtime d effects
            -- Refusal is synchronous at the form owner. A later settings reply
            -- may update the HUD; it cannot change whether this action dispatched.
            refuseReplay d effects=do
              before<-readIORef agentRequests
              refused<-act (d,effects)
              after<-readIORef agentRequests
              ensure "consumed settings form is refused on submission" ("expired" `T.isInfixOf` status refused)
              ensure "consumed settings form dispatches no agent action" (before==after)
              pure refused
            hub=AR.agentHub (conversationAgents conversation)
            primary=AR.primaryAgent (conversationAgents conversation)
        initial<-initializeSidebar host (installSidebar (emptySidebar root 26 True) (modifyActive (\w->w {selection=Selection 2 4}) ((addDocument Nothing (newBuffer "source payload") (initialDesktop (100,35))) {defaultDirectory=Just root})))
        published<-await tick (has "Agents") initial
        ensure "agent plugin publishes session actions and choices through the public menu boundary"
          (all (`elem` map (Menu.menuName . Menu.menuReference) (contributedMenus published))
            ["hide.agents.new","hide.agents.resume","hide.agents.model"])
        let sessionEntry name desktop=case [entry | i<-[0..length menus-1],entry@(MenuItem _ _ (RegisteredMenu reference _))<-menuItemsFor desktop i,Menu.menuName reference==name] of
              [entry]->entry; _->error "Missing or duplicated conversation menu entry"
            boundSessions=published {keyBindings=either (error . show) id (configuredBindings [] M.empty)}
            newSessionEntry=sessionEntry "hide.agents.new" boundSessions
            nativeSessions=published {nativeMac=True,videoMode=Just 3,keyBindings=M.empty}
        ensure "registered New retains its configured command key identity"
          (case newSessionEntry of MenuItem _ _ command->not (null (commandBindingKeys boundSessions AgentNew)) && commandBindingKeys boundSessions command==commandBindingKeys boundSessions AgentNew)
        ensure "native session menu gives New its accelerator and leaves Resume unbound"
          (menuShortcut nativeSessions (sessionEntry "hide.agents.new" nativeSessions)=="⇧⌘N" && menuShortcut nativeSessions (sessionEntry "hide.agents.resume" nativeSessions)=="")
        expanded<-act (activateTree True (index "Agents" published) published) >>= await tick (has "Primary")
        ensure "disconnected agents offer no unadvertised model actions"
          ("Model" `notElem` map fst (contextItemsFor (popupFor "Primary  idle" expanded)))
        completionReady<-await tick (has "ACP completion  configured") expanded
        revealed<-act (activateTree False (index "ACP completion  configured" completionReady) completionReady) >>= await tick hasCompletionChat
        ensure "completion tree activation reveals the existing transcript" (hasCompletionChat revealed)
        let closed=fst (runCommand Close revealed)
        ensure "closing completion transcript leaves its provider node" (not (hasCompletionChat closed) && has "ACP completion" closed)
        choicesDialog<-act (chooseMenu "ACP completion  configured" closed) >>= await tick (maybe False completionPurpose . dialog)
        ensure "advertised completion choices remain readable but human-controlled"
          (not (guestKeyboardAllowed choicesDialog) && case dialog choicesDialog of
            Just dg->readableAt choicesDialog (left (dialogRect choicesDialog dg)+2) (top (dialogRect choicesDialog dg)+2)
            _->False)
        let choose=fmapDialog (\dg->dg {fields=[ListBox "Provider choices" ["Small","Large"] 1]}) choicesDialog
        acceptedChoice<-act (handleEvent (V.EvKey V.KEnter []) choose)
        let modelSaved _=readAutocompleteFor root >>= pure . (\value->case value of Right configValue->field "model" configValue==Just ("large"::T.Text); _->False)
        configured<-awaitIO tick modelSaved acceptedChoice
        ensure "completion model settings remain separate from Primary" (null (agentSettings configured))
        let staleSubmission=handleEvent (V.EvKey V.KEnter []) choose
        stale<-refuseReplay configured (snd staleSubmission)
        initialRename<-act (chooseMenu "Primary  idle" stale) >>= await tick (maybe False ((=="Rename agent").dialogTitle) . dialog)
        sourceVersion<-captureVersion (maybe (error "Missing source") documentBuffer (activeDocument initialRename))
        let originalSelection=selection <$> activeWindow initialRename
            formRef desktop=case dialog desktop of
              Just dg->case purpose dg of
                PluginInputForm reference->reference
                PluginInputsForm reference _->reference
                PluginChoiceForm reference _->reference
                _->error "Missing typed form"
              _->error "Missing typed form"
            captured=formRef initialRename
        ensure "typed form is private to guest input and capture" (not (guestKeyboardAllowed initialRename) &&
          not (readableAt initialRename (left (dialogRect initialRename (maybe (error "dialog") id (dialog initialRename)))) (top (dialogRect initialRename (maybe (error "dialog") id (dialog initialRename))))) &&
          not (streamerReadableAt initialRename 2 34))
        cancelled<-act (handleEvent (V.EvKey V.KEsc []) initialRename)
        reopened<-act (chooseMenu "Primary  idle" cancelled) >>= await tick (maybe False ((=="Rename agent").dialogTitle) . dialog)
        ensure "reopened form has a fresh lifetime" (captured/=formRef reopened)
        refusedOld<-act (reopened,snd (handleEvent (V.EvKey V.KEnter []) initialRename))
        ensure "old form submission leaves reopened draft intact" (inputValue refusedOld==Just ("Primary",Selection 0 7))
        deniedAgent<-act (refusedOld,[SubmitInputForm (formRef refusedOld) (Form.TextValue "Agent edit") Menu.AgentMenu])
        ensure "agent-origin form submission leaves human draft intact" (inputValue deniedAgent==Just ("Primary",Selection 0 7))
        let deniedForm=deniedAgent
        ensure "rename opens with current name selected" (inputValue deniedForm==Just ("Primary",Selection 0 7))
        let focusMaps=either (error . show) id (configuredBindings [] (M.singleton "terminal" (M.singleton "dialog" (M.fromList [("hide.dialog.focus-next",["F13"]),("hide.dialog.focus-previous",["F14"])]))))
            navigable=deniedForm {keyBindings=focusMaps}
        focusedForm<-act (handleEvent (V.EvKey (V.KFun 13) []) navigable)
        ensure "remapped form focus retains exact form and draft" (formRef focusedForm==formRef deniedForm && inputValue focusedForm==Just ("Primary",Selection 0 7) && maybe False ((==1).focus) (dialog focusedForm))
        returnedForm<-act (handleEvent (V.EvKey (V.KFun 14) []) focusedForm)
        ensure "form focus never claims submission" (formRef returnedForm==formRef deniedForm && maybe False ((==0).focus) (dialog returnedForm))
        let renamed=returnedForm
        let typedName=fst (handleEvent (V.EvKey (V.KChar 'N') []) renamed)
        ensure "typing replaces rename selection" (inputValue typedName==Just ("N",Selection 1 1))
        let pasted=fst (handleEvent (V.EvPaste (TE.encodeUtf8 "界x")) typedName)
            backed=fst (handleEvent (V.EvKey V.KBS []) pasted)
            moved=fst (handleEvent (V.EvKey V.KLeft [V.MShift]) backed)
        ensure "rename paste backspace and selection use the real dialog owner" (inputValue moved==Just ("N界",Selection 2 1))
        update<-Form.refreshForm (formRef moved) (Form.InputFormSpec "Updated rename" "Label" "RESET" "Apply") >>= right >>= maybe (fail "No refresh") pure
        publishFormRefreshFromHost host update
        let resized=fst (handleEvent (V.EvResize 90 30) moved)
        refreshedForm<-await tick (maybe False ((=="Updated rename").dialogTitle) . dialog) resized
        ensure "metadata refresh and resize preserve newer draft selection" (inputValue refreshedForm==Just ("N界",Selection 2 1))
        let typed=fst (runCommand Paste refreshedForm {clipboard=""})
        ensure "selected rename range remains editable after refresh" (inputValue typed==Just ("N",Selection 1 1))
        -- Capture the target, then reorder metadata before applying the dialog.
        let caps=AH.Capabilities False False False []
            driver=AH.AgentDriver root "private-peer-key" caps (const (pure (Right caps))) (const (pure (Right Null))) (pure ()) (pure ()) (const (pure (Left "unsupported")))
        peer<-AH.registerAgent hub "A peer" root driver >>= right
        let primaryNamed name _=do
              current<-AH.statusAgent hub AH.Human primary >>= right
              pure (field "name" current==Just (name::T.Text))
        submitted<-act (handleEvent (V.EvKey V.KEnter []) typed) >>= awaitIO tick (primaryNamed "N")
        unchanged<-versionCurrent sourceVersion (maybe (error "Missing source") documentBuffer (activeDocument submitted))
        ensure "rename preserves source content and selection" (unchanged && (selection <$> activeWindow submitted)==originalSelection)
        primaryStatus<-AH.statusAgent hub AH.Human primary >>= right
        peerStatus<-AH.statusAgent hub AH.Human peer >>= right
        ensure "rename targets captured ID after directory changes" (field "name" primaryStatus==Just ("N"::T.Text) && field "name" peerStatus==Just ("A peer"::T.Text))
        _<-AH.renameAgent hub AH.Human primary "Fresh" >>= right
        replayed<-act (submitted,snd (handleEvent (V.EvKey V.KEnter []) typed))
        replayStatus<-AH.statusAgent hub AH.Human primary >>= right
        ensure "consumed rename form cannot replay its captured submission" (field "name" replayStatus==Just ("Fresh"::T.Text))
        _<-AH.renameAgent hub AH.Human primary "N" >>= right
        gate<-newEmptyMVar
        started<-newEmptyMVar
        (pending,reference)<-withRegistry $ \registry->do
          prepared<-FormExtension.prepareForm registry (Form.InputFormSpec "Delayed rename" "Name" "N" "Rename")
            (\_ value->putMVar started () >> takeMVar gate >> pure (Right (SidebarAgent (RenameAgentTo primary value)))) >>= right
          let codec=Codec Null (const (Left "Typed only")) (const Null)
          opening<-registerCommand registry (CommandDef "test.form.open" "Open form" codec codec (\_ ()->pure (Right (SidebarForm prepared)))) >>= right
          ident<-right (P.nodeId "delayed-form")
          provider<-P.registerTree registry "test.form.root"
            (P.NodeDef (P.NodeInfo ident "Form fixture" "" False Nothing) (Just (P.treeAction registry opening () (\_ reply->pure reply))) [])
            (\_ _->pure (Right (P.NodePage [] Nothing))) >>= right
          publishTreeFromHost host provider
          visible<-await tick (has "Form fixture") replayed
          opened<-act (activateTree False (index "Form fixture" visible) visible) >>= await tick (maybe False ((=="Delayed rename").dialogTitle) . dialog)
          running<-act (handleEvent (V.EvKey V.KEnter []) opened)
          entered<-timeout 10000000 (takeMVar started)
          ensure "typed form action runs outside the owner" (entered==Just ())
          pure (running,formRef opened)
        -- A newer modal must survive a reply from the retired registration.
        let newer=Dialog "Newer dialog" Information [] 0 ["Cancel"] []
        putMVar gate ()
        expired<-await tick (T.isInfixOf "expired" . status) pending {dialog=Just newer}
        lateStatus<-AH.statusAgent hub AH.Human primary >>= right
        ensure "retired form cannot rename or replace a newer dialog" (field "name" lateStatus==Just ("N"::T.Text) && fmap dialogTitle (dialog expired)==Just "Newer dialog")
        Form.retireForm reference
        refreshed<-await tick (has "N  idle") expired {dialog=Nothing}
        let foreground=activeWindow refreshed
        hidden<-await tick (has "A peer") refreshed
        ensure "background refresh preserves input owner" (fmap windowId (activeWindow hidden)==fmap windowId foreground)
        childView<-act (activateTree False (index "A peer  idle" hidden) hidden) >>= await tick ((==AH.agentIdText peer).conversationTarget)
        ensure "captured child navigation opens existing attributed view" (conversationTarget childView==AH.agentIdText peer)
        primaryView<-act (childView,[AgentSidebarAction (ShowAgent primary)])
        ensure "Primary navigation restores Primary target" (T.null (conversationTarget primaryView))
        ensure "agent input cannot manufacture human sidebar controls" (not (guestEffectsAllowed [AgentSidebarAction (CreateAgent root "Guest" "Task")]))
        connecting<-act (primaryView,[AgentAction "send" ["0","Count files","false","false","false"]])
        connected<-await tick (\d->not (agentReplying d) && has "N  idle" d && any ((=="model").settingId) (agentSettings d) &&
          "Model" `elem` map fst (contextItemsFor (popupFor "N  idle" d))) connecting
        primaryChoices<-act (chooseMenuAt 1 "N  idle" connected) >>= await tick (maybe False agentChoicePurpose . dialog)
        let selectLarge=fmapDialog (\dg->dg {fields=[ListBox "Provider choices" ["Small","Large"] 1],focus=1}) primaryChoices
        changedLabels<-Form.refreshForm (formRef selectLarge) (Form.ChoiceFormSpec "Agent setting" "Provider choices" [("large","L"),("small","S")] "small" "Apply") >>= right >>= maybe (fail "Missing choice refresh") pure
        publishFormRefreshFromHost host changedLabels
        selectedLarge<-await tick (\d->case dialog d of Just dg | [ListBox _ labels selected]<-fields dg->labels==["L","S"] && selected==0 && focus dg==1; _->False) selectLarge
        ensure "choice form keeps its exact lifetime and refuses guest submission"
          (formRef selectedLarge==formRef primaryChoices && not (guestEffectsAllowed [SubmitChoiceForm (formRef selectedLarge) 0 0 Menu.AgentMenu]))
        staleIndex<-act (selectedLarge,snd (handleEvent (V.EvKey V.KEnter []) selectLarge))
        ensure "old numeric submission cannot choose a different ID after reorder"
          (formRef staleIndex==formRef selectedLarge && any (\option->settingId option=="model" && settingCurrent option=="small") (agentSettings staleIndex))
        let capturedPrimary=handleEvent (V.EvKey V.KEnter []) staleIndex
        primaryUpdated<-act capturedPrimary >>= await tick (any (\option->settingId option=="model" && settingCurrent option=="large") . agentSettings)
        stalePrimary<-refuseReplay primaryUpdated (snd capturedPrimary)
        ensure "Primary model change retains its exact conversation" (T.null (conversationTarget stalePrimary))
        -- Real named form remains private, preserves drafts and captures workspace.
        newDialog<-act (chooseMenu "Agents" stalePrimary) >>= await tick (maybe False ((=="New agent").dialogTitle) . dialog)
        let fill=fmapDialog (\dg->dg {fields=[SelectedInput "Name" "Child" (Selection 1 3),SelectedInput "Task" "Count files" (Selection 2 5)],focus=1}) newDialog
            malformed=fmapDialog (\dg->dg {fields=fields dg++[CheckBox "Extra" False]}) fill
            wrongKind=fmapDialog (\dg->dg {fields=[CheckBox "Name" False,Input "Task" "Count files" 11]}) fill
        ensure "named form projection refuses malformed field count and kind"
          (null (snd (handleEvent (V.EvKey V.KEnter []) malformed)) && null (snd (handleEvent (V.EvKey V.KEnter []) wrongKind)))
        ensure "New agent inputs are private and human controlled" (not (guestKeyboardAllowed fill) && case dialog fill of
          Just dg->not (readableAt fill (left (dialogRect fill dg)+2) (top (dialogRect fill dg)+2))
          _->False)
        updateInputs<-Form.refreshForm (formRef fill)
          (Form.InputsFormSpec "New agent" [Form.InputField "name" "Agent name" "RESET",Form.InputField "task" "Initial task" "RESET"] "Create") >>= right >>= maybe (fail "Missing input refresh") pure
        publishFormRefreshFromHost host updateInputs
        refreshedInputs<-await tick (\desktop->case dialog desktop of
          Just dg->fields dg==[SelectedInput "Agent name" "Child" (Selection 1 3),SelectedInput "Initial task" "Count files" (Selection 2 5)] && focus dg==1
          _->False) fill
        let closedSubmission=snd (handleEvent (V.EvKey V.KEnter []) refreshedInputs)
        closedForm<-act (handleEvent (V.EvKey V.KEsc []) refreshedInputs)
        refusedCreation<-act (closedForm,closedSubmission)
        attempted<-readIORef createRequests
        ensure "closed New agent form cannot commit a spawn" (null attempted && dialog refusedCreation==Nothing)
        staleDialog<-act (chooseMenu "Agents" refusedCreation) >>= await tick (maybe False ((=="New agent").dialogTitle) . dialog)
        let staleInputs=fmapDialog (\dg->dg {fields=[Input "Name" "Stale child" 11,Input "Task" "Count files" 11]}) staleDialog
            other=root </> "other"
        createDirectoryIfMissing True other
        staleStarting<-act (handleEvent (V.EvKey V.KEnter []) staleInputs)
        let staleAdopted _=any (\(_,name,_)->name=="Stale child") <$> readIORef createRequests
        refusedWorkspace<-awaitIO tick staleAdopted staleStarting {defaultDirectory=Just other}
        afterStale<-AH.listAgents hub AH.Human >>= right
        ensure "changed workspace refuses the captured spawn"
          ("workspace changed" `T.isInfixOf` status refusedWorkspace &&
           not (any (\entry->field "name" entry==Just ("Stale child"::T.Text)) (maybe [] id (field "agents" afterStale :: Maybe [Value]))))
        -- A Files directory is not the editor's captured workspace.
        relocated<-act (refusedWorkspace {defaultDirectory=Just root},[ReadTree other]) >>= await tick (\desktop->treeRoot (tree desktop)==other && has "Agents" desktop)
        reopenedCreate<-act (chooseMenu "Agents" relocated) >>= await tick (maybe False ((=="New agent").dialogTitle) . dialog)
        let childInputs=fmapDialog (\dg->dg {fields=[Input "Name" "Child" 5,Input "Task" "Count files" 11]}) reopenedCreate
            createSubmission=handleEvent (V.EvKey V.KEnter []) childInputs
        starting<-act createSubmission
        let createdAndQueued desktop=do
              directoryNow<-AH.listAgents hub AH.Human >>= right
              case [AH.AgentId who | entry<-maybe [] id (field "agents" directoryNow :: Maybe [Value]),field "name" entry==Just ("Child"::T.Text),Just who<-[field "id" entry]] of
                who:_->do
                  history<-AH.historyAgent hub AH.Human who 0 100 >>= right
                  pure (has "Child" desktop && length [() | event<-AH.historyEvents history,AH.historyKind event=="message_queued"]==1)
                _->pure False
        createdDesktop<-awaitIO tick createdAndQueued starting
        creationReplay<-act (createdDesktop,snd createSubmission)
        requests<-readIORef createRequests
        ensure "New agent form commits the captured values once" ([(workspace,task) | (workspace,"Child",task)<-requests]==[(root,"Count files")])
        directory<-AH.listAgents hub AH.Human >>= right
        let children=[entry | entry<-maybe [] id (field "agents" directory :: Maybe [Value]),field "name" entry==Just ("Child"::T.Text)]
        ensure "New Agent enqueues its task without selecting its view" (length children==1 && all (\entry->field "cwd" entry==Just (T.pack root)) children && T.null (conversationTarget creationReplay))
        ensure "background completion never steals focus" (fmap windowId (activeWindow createdDesktop)==fmap windowId (activeWindow primaryView))
        childId<-case children of [entry] | Just who<-field "id" entry->pure (AH.AgentId who); _->error "Missing created child"
        let childReady desktop=has "Child  idle" desktop &&
              "Model" `elem` map fst (contextItemsFor (popupFor "Child  idle" desktop))
        idleChild<-await tick childReady createdDesktop
        childChoices<-act (chooseMenuAt 1 "Child  idle" idleChild) >>= await tick (maybe False agentChoicePurpose . dialog)
        let selectedChild=fmapDialog (\dg->dg {fields=[ListBox "Provider choices" ["Small","Large"] 1]}) childChoices
            capturedChild=handleEvent (V.EvKey V.KEnter []) selectedChild
            childLarge _=fmap (\current->case current of Right (_,options)->any (\option->AH.configId option=="model" && AH.configCurrent option=="large") options; _->False) (AH.agentConfiguration hub childId)
        childUpdated<-act capturedChild >>= awaitIO tick childLarge
        refusedChild<-refuseReplay childUpdated (snd capturedChild)
        ensure "sidebar child setting never selects that conversation" (T.null (conversationTarget refusedChild))
        ensure "child setting does not change Primary settings" (any (\option->settingId option=="model" && settingCurrent option=="large") (agentSettings refusedChild))
        let popupRef desktop=case contextKind desktop of FormChoicesContext owner _ _->Just owner; _->Nothing
            hasPopup desktop=contextMenu desktop/=Nothing && popupRef desktop/=Nothing
            nextPopup previous desktop=hasPopup desktop && popupRef desktop/=popupRef previous
            selectedPopup selected desktop=handleEvent (V.EvKey V.KEnter []) desktop {contextMenu=fmap (\(rect,_)->(rect,selected)) (contextMenu desktop)}
        childTitle<-act (refusedChild,[AgentSidebarAction (ShowAgent childId)]) >>= await tick (not . null . childAgentSettings)
        _<-captureConversationChoices conversation childTitle {buffers=error "choice capture forced buffers"} >>= right
        childCategories<-act (runCommand AgentChoose childTitle) >>= await tick hasPopup
        childValues<-act (selectedPopup 0 childCategories) >>= await tick (nextPopup childCategories)
        ensure "child title uses the public two-level popup and its advertised checkmark"
          (dialog childValues==Nothing && any ((=="✓ Large").fst) (contextItemsFor childValues) && not (guestKeyboardAllowed childValues))
        let capturedPopup=selectedPopup 0 childValues
        beforePopupCancel<-length <$> readIORef agentRequests
        closedPopup<-act (handleEvent (V.EvKey V.KEsc []) childValues)
        refusedPopup<-act (closedPopup,snd capturedPopup)
        afterPopupCancel<-length <$> readIORef agentRequests
        ensure "Escape retires the popup before a saved numeric submission can claim it"
          (beforePopupCancel==afterPopupCancel && contextMenu refusedPopup==Nothing)
        categoriesAgain<-act (runCommand AgentChoose refusedPopup) >>= await tick hasPopup
        valuesAgain<-act (selectedPopup 0 categoriesAgain) >>= await tick (nextPopup categoriesAgain)
        let (submittedPopup,popupSubmission)=selectedPopup 0 valuesAgain
            sourceWindow=case activeWindow initial of Just window->windowId window; _->error "Missing source window"
        beforeFocusChange<-length <$> readIORef agentRequests
        focusRefused<-act (focusWindow sourceWindow submittedPopup,popupSubmission)
        afterFocusChange<-length <$> readIORef agentRequests
        ensure "changed focus refuses the popup without claiming its form"
          (beforeFocusChange==afterFocusChange)
        expiredPopup<-await tick (\desktop->contextMenu desktop==Nothing && case contextKind desktop of SourceContext->True; _->False) focusRefused
        selectedChildAgain<-act (expiredPopup,[AgentSidebarAction (ShowAgent childId)])
        freshCategories<-act (runCommand AgentChoose selectedChildAgain) >>= await tick hasPopup
        freshValues<-act (selectedPopup 0 freshCategories) >>= await tick (nextPopup freshCategories)
        let freshSubmission=selectedPopup 0 freshValues
        childSmall<-act freshSubmission >>= awaitIO tick (\_->fmap (\current->case current of Right (_,options)->any ((=="small").AH.configCurrent) options; _->False) (AH.agentConfiguration hub childId))
        ensure "a fresh child popup configures its captured child"
          (conversationTarget childSmall==AH.agentIdText childId && not (guestEffectsAllowed (snd freshSubmission)))
        replayedPopup<-refuseReplay childSmall (snd freshSubmission)
        -- An independently named public contribution uses the same popup owner.
        -- The long list exercises its existing viewport rather than provider IO.
        afterPaging<-withRegistry $ \registry->do
          selections<-newIORef []
          gateChoice<-newIORef Nothing
          latestForm<-newIORef Nothing
          let codec=Codec Null (const (Left "Typed only")) (const Null)
              directoryApi=AgentDirectory.agentDirectory hub autocomplete
          select<-registerCommand registry (CommandDef "test.choice.select" "Select choice" codec codec (\ctx (who,value)->
            if sidebarOrigin ctx/=Menu.HumanMenu then pure (Left (CommandRejected "Human only")) else do
              modifyIORef' selections (++[(who,value)])
              pure (Right (SidebarAgent (RenameAgentTo who "Child"))))) >>= right
          opening<-registerCommand registry (CommandDef "test.choice.popup" "Public choices" codec codec (\ctx ()->do
            who<-right (sidebarSelectedAgent ctx)
            readIORef gateChoice >>= mapM_ (\(entered,release)->putMVar entered () >> takeMVar release)
            -- This worker read may observe newer metadata. The host's earlier
            -- target still governs opening; the plugin cannot replace it.
            _<-Directory.directorySettings directoryApi who >>= right
            prepared<-Form.prepareForm Form.ReadableForm
              (Form.ChoiceFormSpec "Public choices" "Values" [(T.pack (show n),"Model "<>T.pack (show n)) | n<-[0..29::Int]] "0" "Select")
              (Form.formAction registry select (\value->(who,value)) (\_ reply->pure reply)) >>= right
            writeIORef latestForm (Just prepared)
            pure (Right (PluginSidebar.popupFormReply (sidebarCapabilities host) prepared)))) >>= right
          bracket (Menu.publishMenu (menuSidebarCapabilities menuHost host)
            (Menu.MenuDef "test.choice.popup" "tools" "test" 0 "Public choices" "" False
              (Menu.menuAction registry opening (const (Right ())) (\_ reply->pure reply))) >>= right)
            (Menu.withdrawMenu (menuSidebarCapabilities menuHost host)) $ \popupMenu->do
              visible<-await tick (any ((==popupMenu).Menu.menuReference) . contributedMenus) replayedPopup
              pagedPopup<-act (runCommand (RegisteredMenu popupMenu False) visible {screenSize=(90,12)}) >>= await tick hasPopup
              let key code desktop=fst (handleEvent (V.EvKey code []) desktop)
                  page=key V.KPageDown pagedPopup
                  firstPage=key V.KPageUp page
                  row25=iterate (key V.KDown) firstPage !! 25
                  numericSelection=handleEvent (V.EvKey V.KEnter []) row25
              ensure "PageDown advances the popup selection and PageUp restores it"
                (maybe False ((>0).snd) (contextMenu page) && fmap snd (contextMenu firstPage)==Just 0)
              ensure "public popup paging keeps geometry bounded and emits the installed numeric receipt"
                (case contextMenu row25 of
                  Just (rect,25)->left rect>=0 && top rect>=0 && left rect+width rect<=90 && top rect+height rect<=12 &&
                    case snd numericSelection of [SubmitPopupChoiceForm owner revision 25 Menu.HumanMenu]->Just owner==popupRef row25 && revision==1; _->False
                  _->False)
              beforePaged<-length <$> readIORef agentRequests
              chosen<-act numericSelection >>= awaitIO tick (\_->do
                selected<-readIORef selections
                dispatched<-drop beforePaged <$> readIORef agentRequests
                pure (selected==[(childId,"25")] && RenameAgentTo childId "Child" `elem` dispatched))
              -- Delay only the declared command, then change the host receipt
              -- before its newer worker metadata is returned for adoption.
              entered<-newEmptyMVar
              release<-newEmptyMVar
              writeIORef gateChoice (Just (entered,release))
              writeIORef latestForm Nothing
              pendingChoice<-act (runCommand (RegisteredMenu popupMenu False) chosen)
              reached<-timeout 10000000 (takeMVar entered)
              ensure "public popup command runs on the menu worker" (reached==Just ())
              (oldReceipt,_)<-AH.agentConfiguration hub childId >>= right
              _<-AH.configureAgentAt hub oldReceipt "model" "small" >>= right
              putMVar release ()
              rejected<-awaitIO tick (\_->readIORef latestForm >>= maybe (pure False) (fmap not . Form.formCurrent)) pendingChoice
              ensure "newer worker metadata cannot replace the original popup receipt" (contextMenu rejected==Nothing)
              pure rejected {screenSize=screenSize replayedPopup}
        primaryAfterPopup<-act (afterPaging,[AgentSidebarAction (ShowAgent primary)])
        (subagent,_)<-AH.spawnAgentWithTask hub (AH.Agent primary) (AH.SpawnSpec "Nested" "Count files" root AH.Shared AH.Fresh Nothing Nothing) >>= right
        parentReady<-await tick (\d->case [row | (_,row)<-visibleRows 0 32768 (tree d),P.infoLabel (rowInfo row)=="N  idle"] of row:_->P.infoBranch (rowInfo row); _->False) primaryAfterPopup
        nested<-act (activateTree True (index "N  idle" parentReady) parentReady) >>= await tick (has "Nested")
        let nestedRows=[row | (_,row)<-visibleRows 0 32768 (tree nested),T.isPrefixOf "Nested" (P.infoLabel (rowInfo row))]
        ensure "agent parent IDs determine the shared hierarchy" (case nestedRows of
          [row] | Just node<-M.lookup (keyOf (rowHit row)) (treeNodes (tree nested)),Just (NodeKey _ nodeId)<-stateParent node->P.nodeIdText nodeId==AH.agentIdText primary && rowDepth row==2
          _->False)
        ensure "rename paste replaces selected text" (inputValue (fst (handleEvent (V.EvPaste "Replacement") renamed))==Just ("Replacement",Selection 11 11))
        ensure "rename backspace deletes selected text" (inputValue (fst (handleEvent (V.EvKey V.KBS []) renamed))==Just ("",Selection 0 0))
        ensure "rename Left collapses selection to its start" (inputValue (fst (handleEvent (V.EvKey V.KLeft []) renamed))==Just ("Primary",Selection 0 0))
        ensure "rename Shift Left extends existing selection" (inputValue (fst (handleEvent (V.EvKey V.KLeft [V.MShift]) typed))==Just ("N",Selection 1 0))
        _<-AH.endAgent hub AH.Human subagent
        let sessionRequestAfter count _=((>count).length) <$> readIORef sessionRequests
            resumeForm desktop=case dialog desktop of
              Just dg | PluginInputForm{}<-purpose dg,dialogTitle dg=="Resume conversation"->True
              _->False
        capturedSession<-captureConversationSession conversation nested >>= right
        ensure "session capture never inspects source payloads"
          =<< fmap (either (const False) (const True)) (captureConversationSession conversation nested {buffers=error "session capture forced buffers"})
        let menuContext=sidebarInvocationContext Menu.HumanMenu nested {buffers=error "menu context forced buffers"}
        ensure "menu form context retains only shallow host input" (null (sidebarPrivatePaths menuContext) && null (sidebarRenameFiles menuContext))
        childFocus<-act (nested,[AgentSidebarAction (ShowAgent peer)])
        let childDraft=setComposerInput (newBuffer "unsent child draft") (Selection 3 8) True childFocus
        beforeNew<-length <$> readIORef sessionRequests
        restarting<-act (runCommand AgentNew childDraft) >>= awaitIO tick (sessionRequestAfter beforeNew)
        let childAgain=selectConversationView (AH.agentIdText peer) "A peer" restarting
        ensure "plugin New selects Primary and preserves the independent child draft"
          (T.null (conversationTarget restarting) && contents (composerBuffer childAgain)=="unsent child draft" && composerSelection childAgain==Selection 3 8)
        staleSession<-act (restarting,[ConversationSessionAction (ConversationSession.NewConversation (ConversationSession.conversationReceipt capturedSession))])
        ensure "old session receipt cannot replace a newer provider acquisition"
          ("changed or is busy" `T.isInfixOf` status staleSession && not (guestEffectsAllowed [ConversationSessionAction (ConversationSession.NewConversation (ConversationSession.conversationReceipt capturedSession))]))
        readySession<-await tick (\desktop->not (null (agentSettings desktop)) && not (agentReplying desktop)) staleSession
        offered<-act (runCommand AgentResume readySession) >>= await tick resumeForm
        let resumeReference=case dialog offered of Just dg | PluginInputForm formOwner<-purpose dg->formOwner; _->error "Missing Resume form"
        ensure "Resume uses the ordinary private form with the remembered ID"
          (Form.formDisclosure resumeReference==Form.PrivateForm && inputValue offered==Just ("private-main-key",Selection 0 16) && not (guestKeyboardAllowed offered))
        let pendingResume=snd (runCommand DialogAccept offered)
        beforeCancel<-length <$> readIORef sessionRequests
        cancelledResume<-act (runCommand DialogCancel offered)
        replayedResume<-act (cancelledResume,pendingResume)
        afterCancel<-length <$> readIORef sessionRequests
        ensure "cancelled Resume cannot submit its retained form action"
          (afterCancel==beforeCancel && dialog replayedResume==Nothing)
        putStrLn "agent sidebar checks passed"
  where
    ensure label ok=unless ok (error label)
    right value=either (error.show) pure value
    field key value=parseMaybe (withObject "field" (.: key)) value
    has text d=any (T.isPrefixOf text . P.infoLabel . rowInfo . snd) (visibleRows 0 32768 (tree d))
    index text d=case [i | (i,row)<-visibleRows 0 32768 (tree d),P.infoLabel (rowInfo row)==text] of i:_->i; _->error "missing agent row"
    tree=maybe (error "missing sidebar") id . sideTree
    popupFor label d=
      let target=index label d
          y=2+target-treeScroll (tree d)
      in fst (handleEvent (V.EvMouseDown 5 y V.BRight []) d)
    chooseMenu=chooseMenuAt 0
    chooseMenuAt selected label d=
      let popup=popupFor label d
          selectedPopup=popup {contextMenu=fmap (\(rectangle,_)->(rectangle,selected)) (contextMenu popup)}
      in handleEvent (V.EvKey V.KEnter []) selectedPopup
    hasCompletionChat d=any (\w->maybe False ((==windowContent w).PluginContent) (autocompleteWindow d)) (windows d)
    agentChoicePurpose dg=case purpose dg of PluginChoiceForm{}->dialogTitle dg=="Agent setting"; _->False
    completionPurpose dg=case purpose dg of PluginChoiceForm{}->dialogTitle dg=="Completion setting"; _->False
    inputValue d=case dialog d of
      Just dg | SelectedInput _ value sel:_<-fields dg->Just (value,sel)
      _->Nothing
    fmapDialog f d=d {dialog=fmap f (dialog d)}
    await :: HasCallStack => (Desktop -> IO Desktop) -> (Desktop -> Bool) -> Desktop -> IO Desktop
    await tick done=awaitIO tick (pure . done)
    awaitIO :: HasCallStack => (Desktop -> IO Desktop) -> (Desktop -> IO Bool) -> Desktop -> IO Desktop
    awaitIO tick done initial=do
      observed<-newIORef initial
      result<-timeout 10000000 (go observed initial)
      case result of
        Just ready->pure ready
        Nothing->do
          lastSeen<-readIORef observed
          error ("Agent sidebar timeout\n"++observation lastSeen++prettyCallStack callStack)
      where
        go observed d=do
          next<-tick d
          writeIORef observed next
          ready<-done next
          if ready then pure next else threadDelay 10000 >> go observed next
    -- Format only on failure; retain no sequence of desktop snapshots.
    observation d=unlines
      [ "Last status: "++show (T.take 256 (status d))
      , "Last dialog: "++show (fmap (\dg->(T.take 128 (dialogTitle dg),dialogOwner dg)) (dialog d))
      , "Conversation target: "++show (T.take 128 (conversationTarget d))
      , "Sidebar (revision, projection, focused): "++show (fmap (\sidebar->(treeRevision sidebar,treeProjectionRevision sidebar,treeFocused sidebar)) (sideTree d))
      ]
    dialogOwner dg=case purpose dg of
      PluginInputForm reference->"input "++show reference
      PluginInputsForm reference _->"inputs "++show reference
      PluginChoiceForm reference revision->"choice "++show (reference,revision)
      AgentDialog action->"agent "++show (T.take 64 action)
      _->"other"

temporary :: IO FilePath
temporary=do root<-getTemporaryDirectory; (path,h)<-openTempFile root "hide-agent-sidebar"; hClose h; removeFile path; createDirectory path; canonicalizePath path

environment :: String -> Maybe String -> IO a -> IO a
environment key value action=bracket (lookupEnv key <* set value) set (const action)
  where set=maybe (unsetEnv key) (setEnv key)
