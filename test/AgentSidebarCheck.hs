{-# LANGUAGE OverloadedStrings #-}
module AgentSidebarCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO (openTempFile,hClose)
import System.Timeout (timeout)
import Hide.Autocomplete (withAutocomplete,autocompleteEffects,tickAutocomplete)
import Hide.MCPPermissions (readAutocompleteFor)
import Hide.AgentSidebar
import Hide.AgentSidebarTypes
import qualified Hide.AgentHub as AH
import qualified Hide.AgentRuntime as AR
import Hide.App (applyEffects)
import Hide.Buffer (Selection(..))
import Hide.Conversation
import Hide.Model
import Hide.Sidebar
import Hide.SidebarCommands
import Hide.Session
import Hide.GuestAccess (guestEffectsAllowed)
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
    environment "THC_EDIT_SESSION" (Just (sessionId record)) $ withSidebarCommands $ \host->withConversationAt root $ \conversation->
      withAutocomplete root $ \autocomplete->withAgentSidebar host (conversationAgents conversation) autocomplete $ \agents->do
        let core=autocompleteEffects autocomplete (conversationEffects conversation applyEffects)
            tick d=tickConversation conversation d >>= tickAutocomplete autocomplete >>= tickAgentSidebar agents host >>= tickSidebar host core
            act (d,effects)=snd <$> sidebarEffects host core d effects
            hub=AR.agentHub (conversationAgents conversation)
            primary=AR.primaryAgent (conversationAgents conversation)
        initial<-initializeSidebar host (installSidebar (emptySidebar root 26 True) ((initialDesktop (100,35)) {defaultDirectory=Just root}))
        published<-await tick (has "Agents") initial
        expanded<-act (activateTree True (index "Agents" published) published) >>= await tick (has "Primary")
        ensure "disconnected agents offer no unadvertised model actions"
          ("Model" `notElem` map fst (contextItemsFor (popupFor "Primary  idle" expanded)))
        completionReady<-await tick (has "ACP completion  configured") expanded
        revealed<-act (activateTree False (index "ACP completion  configured" completionReady) completionReady) >>= await tick hasCompletionChat
        ensure "completion tree activation reveals the existing transcript" (hasCompletionChat revealed)
        let closed=fst (runCommand Close revealed)
        ensure "closing completion transcript leaves its provider node" (not (hasCompletionChat closed) && has "ACP completion" closed)
        choicesDialog<-act (chooseMenu "ACP completion  configured" closed) >>= await tick (maybe False completionPurpose . dialog)
        let choose=fmapDialog (\dg->dg {fields=[ListBox "Provider choices" ["Small","Large"] 1]}) choicesDialog
        acceptedChoice<-act (handleEvent (V.EvKey V.KEnter []) choose)
        let modelSaved _=readAutocompleteFor root >>= pure . (\value->case value of Right configValue->field "model" configValue==Just ("large"::T.Text); _->False)
        configured<-awaitIO tick modelSaved acceptedChoice
        ensure "completion model settings remain separate from Primary" (null (agentSettings configured))
        let staleSubmission=handleEvent (V.EvKey V.KEnter []) choose
        stale<-act (configured,snd staleSubmission) >>= await tick (T.isInfixOf "expired" . status)
        renamed<-act (chooseMenu "Primary  idle" stale) >>= await tick (maybe False ((=="Rename agent").dialogTitle) . dialog)
        ensure "rename opens with current name selected" (inputValue renamed==Just ("Primary",Selection 0 7))
        let typed=fst (handleEvent (V.EvKey (V.KChar 'N') []) renamed)
        ensure "typing replaces rename selection" (inputValue typed==Just ("N",Selection 1 1))
        -- Capture the target, then reorder metadata before applying the dialog.
        let caps=AH.Capabilities False False False []
            driver=AH.AgentDriver root "private-peer-key" caps (const (pure (Right caps))) (const (pure (Right Null))) (pure ()) (pure ()) (const (pure (Left "unsupported")))
        peer<-AH.registerAgent hub "A peer" root driver >>= right
        submitted<-act (handleEvent (V.EvKey V.KEnter []) typed)
        primaryStatus<-AH.statusAgent hub AH.Human primary >>= right
        peerStatus<-AH.statusAgent hub AH.Human peer >>= right
        ensure "rename targets captured ID after directory changes" (field "name" primaryStatus==Just ("N"::T.Text) && field "name" peerStatus==Just ("A peer"::T.Text))
        refreshed<-await tick (has "N  idle") submitted
        let foreground=activeWindow refreshed
        hidden<-await tick (has "A peer") refreshed
        ensure "background refresh preserves input owner" (fmap windowId (activeWindow hidden)==fmap windowId foreground)
        childView<-act (activateTree False (index "A peer  idle" hidden) hidden) >>= await tick ((==AH.agentIdText peer).conversationTarget)
        ensure "captured child navigation opens existing attributed view" (conversationTarget childView==AH.agentIdText peer)
        primaryView<-act (childView,[AgentSidebarAction (ShowAgent primary)])
        ensure "Primary navigation restores Primary target" (T.null (conversationTarget primaryView))
        ensure "agent input cannot manufacture human sidebar controls" (not (guestEffectsAllowed [AgentSidebarAction (RenameAgent primary)]))
        connecting<-act (primaryView,[AgentAction "send" ["0","Count files","false","false","false"]])
        connected<-await tick (\d->not (agentReplying d) && has "N  idle" d && any ((=="model").settingId) (agentSettings d) &&
          "Model" `elem` map fst (contextItemsFor (popupFor "N  idle" d))) connecting
        primaryChoices<-act (chooseMenuAt 1 "N  idle" connected) >>= await tick (maybe False agentChoicePurpose . dialog)
        let selectLarge=fmapDialog (\dg->dg {fields=[ListBox "Provider choices" ["Small","Large"] 1]}) primaryChoices
            capturedPrimary=handleEvent (V.EvKey V.KEnter []) selectLarge
        primaryUpdated<-act capturedPrimary >>= await tick (any (\option->settingId option=="model" && settingCurrent option=="large") . agentSettings)
        stalePrimary<-act (primaryUpdated,snd capturedPrimary) >>= await tick (T.isInfixOf "expired" . status)
        ensure "Primary model change retains its exact conversation" (T.null (conversationTarget stalePrimary))
        -- Real New Agent dialog submits through the existing runtime/ACP owner.
        newDialog<-act (chooseMenu "Agents" stalePrimary) >>= await tick (maybe False ((=="New agent").dialogTitle) . dialog)
        let fill=fmapDialog (\dg->dg {fields=[Input "Name" "Child" 5,Input "Task" "Count files" 11]}) newDialog
        starting<-act (handleEvent (V.EvKey V.KEnter []) fill)
        let createdAndQueued desktop=do
              directoryNow<-AH.listAgents hub AH.Human >>= right
              case [AH.AgentId who | entry<-maybe [] id (field "agents" directoryNow :: Maybe [Value]),field "name" entry==Just ("Child"::T.Text),Just who<-[field "id" entry]] of
                who:_->do
                  history<-AH.historyAgent hub AH.Human who 0 100 >>= right
                  pure (has "Child" desktop && any (\event->field "kind" event==Just ("message_queued"::T.Text)) (maybe [] id (field "events" history :: Maybe [Value])))
                _->pure False
        started<-awaitIO tick createdAndQueued starting
        directory<-AH.listAgents hub AH.Human >>= right
        let children=[entry | entry<-maybe [] id (field "agents" directory :: Maybe [Value]),field "name" entry==Just ("Child"::T.Text)]
        ensure "New Agent enqueues its task without selecting its view" (length children==1 && T.null (conversationTarget started))
        ensure "background completion never steals focus" (fmap windowId (activeWindow started)==fmap windowId (activeWindow primaryView))
        childId<-case children of [entry] | Just who<-field "id" entry->pure (AH.AgentId who); _->error "Missing created child"
        let childReady desktop=do
              current<-AH.agentConfiguration hub childId
              pure (has "Child  idle" desktop && case current of Right (_,options)->any ((=="model").AH.configId) options; _->False)
        idleChild<-awaitIO tick childReady started
        childChoices<-act (chooseMenuAt 1 "Child  idle" idleChild) >>= await tick (maybe False agentChoicePurpose . dialog)
        let selectedChild=fmapDialog (\dg->dg {fields=[ListBox "Provider choices" ["Small","Large"] 1]}) childChoices
            capturedChild=handleEvent (V.EvKey V.KEnter []) selectedChild
            childLarge _=fmap (\current->case current of Right (_,options)->any (\option->AH.configId option=="model" && AH.configCurrent option=="large") options; _->False) (AH.agentConfiguration hub childId)
        childUpdated<-act capturedChild >>= awaitIO tick childLarge
        retiredControl<-await tick ((=="Child settings updated.").status) childUpdated
        refusedChild<-act (retiredControl,snd capturedChild) >>= await tick (T.isInfixOf "expired" . status)
        ensure "sidebar child setting never selects that conversation" (T.null (conversationTarget refusedChild))
        ensure "child setting does not change Primary settings" (any (\option->settingId option=="model" && settingCurrent option=="large") (agentSettings refusedChild))
        (subagent,_)<-AH.spawnAgentWithTask hub (AH.Agent primary) (AH.SpawnSpec "Nested" "Count files" root AH.Shared AH.Fresh Nothing Nothing) >>= right
        parentReady<-await tick (\d->case [row | (_,row)<-visibleRows 0 32768 (tree d),P.infoLabel (rowInfo row)=="N  idle"] of row:_->P.infoBranch (rowInfo row); _->False) refusedChild
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
    hasCompletionChat d=any ((==Just "Autocomplete").documentLabel) (M.elems (buffers d))
    agentChoicePurpose dg=case purpose dg of AgentChoiceDialog{}->True; _->False
    completionPurpose dg=case purpose dg of CompletionChoiceDialog{}->True; _->False
    inputValue d=case dialog d of
      Just dg | SelectedInput _ value sel:_<-fields dg->Just (value,sel)
      _->Nothing
    fmapDialog f d=d {dialog=fmap f (dialog d)}
    await tick done=awaitIO tick (pure . done)
    awaitIO tick done initial=timeout 10000000 (go initial) >>= maybe (error "Agent sidebar timeout") pure
      where go d=do next<-tick d; ready<-done next; if ready then pure next else threadDelay 10000 >> go next

temporary :: IO FilePath
temporary=do root<-getTemporaryDirectory; (path,h)<-openTempFile root "hide-agent-sidebar"; hClose h; removeFile path; createDirectory path; canonicalizePath path

environment :: String -> Maybe String -> IO a -> IO a
environment key value action=bracket (lookupEnv key <* set value) set (const action)
  where set=maybe (unsetEnv key) (setEnv key)
