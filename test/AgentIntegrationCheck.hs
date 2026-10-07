{-# LANGUAGE OverloadedStrings #-}
module AgentIntegrationCheck (checks, fixture) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket,onException)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO (hClose,openTempFile)
import System.Timeout (timeout)
import GHC.Stack (HasCallStack,callStack,prettyCallStack)
import Hide.Conversation
import Hide.TextPresentation (TextPresentation,withTextPresentation,textPresentationEffects,tickTextPresentation)
import qualified Hide.Consoles as C
import qualified Hide.AgentRuntime as AR
import qualified Hide.AgentHub as AH
import Hide.GuestAccess (guestKeyboardAllowed,guestCommandAllowed,sanitizedBuffer,sanitizedPreparedContent,guestTransitionAllowed)
import qualified Data.Map.Strict as M
import Data.IORef
import Hide.Buffer (contents,newBuffer,contentSlice,contentLength,Selection(..))
import Hide.AgentSidebarTypes (AgentSidebarRequest(ShowAgent))
import Hide.Recovery (writeCheckpoint,readCheckpoint,checkpointKey)
import Hide.BufferReadCommand (withBufferReadCommands,readPage,readWindowCommand)
import Hide.BufferReads (windowReadTarget,captureWindow)
import qualified Hide.Font as Font
import Hide.ScreenCapture (capture)
import Hide.Session
import Hide.Model
import qualified Hide.Plugin.Window as W

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root ->
  environment "XDG_CONFIG_HOME" (Just (root </> "config")) $
  environment "XDG_DATA_HOME" (Just (root </> "data")) $
  environment "THC_EDIT_SESSION" Nothing $ do
    let config=root </> "config" </> "thc-edit"
        script=root </> "provider.py"
    createDirectoryIfMissing True config
    font<-Font.loadFont
    writeFile script fixture
    BL.writeFile (config </> "agents.json") (encode (object ["executable" .= ("python3"::T.Text),"arguments" .= [script]]))
    record<-newSessionRecord Nothing ["--",root]
    rememberSession record
    environment "THC_EDIT_SESSION" (Just (sessionId record)) $ C.withConsoles $ \consoles -> withConversationAt consoles root $ \conversation -> withTextPresentation $ \presentation->do
      let agents=conversationAgents conversation
          hub=AR.agentHub agents
          primary=AR.primaryAgent agents
          initial=(initialDesktop (80,25)) {defaultDirectory=Just root}
          apply d effects=snd <$> conversationEffects conversation (\x _->pure (False,x)) d effects
          ui action values d=apply d [AgentAction action values]
          select ident d=apply d [AgentSidebarAction (ShowAgent ident)]
          submit action d=let (prepared,effects)=runCommand (SubmitChat action) d in apply prepared effects
          draft text selected d=setComposerInput (newBuffer text) selected True d
          send text d=submit QuerySubmit (draft text (Selection (T.length text) (T.length text)) d)
          tickUntil :: HasCallStack => (Desktop -> IO Bool) -> Desktop -> IO Desktop
          tickUntil=testUntil "provider/body"
          testUntil :: HasCallStack => String -> (Desktop -> IO Bool) -> Desktop -> IO Desktop
          testUntil label test d=do
            answer<-timeout 5000000 (loop d)
            maybe (error ("Agent conversation integration timed out: "++label++"\n"++prettyCallStack callStack)) pure answer
            where loop current=do next<-tickBody presentation conversation current; ok<-test next; if ok then pure next else threadDelay 10000 >> loop next
      shown<-ui "show" [] initial
      connected<-send "Hello" shown >>= tickUntil (pure . (\d->status d=="Agent: end_turn" && "Hello" `T.isInfixOf` activeText d))
      servers<-AR.primaryServers agents
      let tokens=[value | server<-servers, entry<-maybe [] id (field "env" server :: Maybe [Value]),Just value<-[field "value" entry :: Maybe T.Text]]
      let publicSafe desktop=do
            let readable=T.concat ([body | ident<-M.keys (buffers desktop),Just body<-[sanitizedBuffer desktop ident]]++readableBodies desktop)
                private="private-main-key":tokens
            screen<-capture font desktop False
            ensure "primary transcript never exposes private keys through buffer reads" (all (not . (`T.isInfixOf` readable)) private)
            ensure "primary transcript never exposes private keys through screen capture" (all (not . (`T.isInfixOf` T.pack (show screen))) private)
      publicSafe connected
      split<-send "split-private" connected >>= tickUntil (\desktop->publicSafe desktop >> pure (status desktop=="Agent: end_turn" && "split-private" `T.isInfixOf` activeText desktop))
      publicSafe split
      nested<-send "nested-private" split >>= tickUntil (\desktop->publicSafe desktop >> pure (status desktop=="Agent: end_turn" && "nested-private" `T.isInfixOf` activeText desktop))
      expanded<-snd <$> conversationEffects conversation (\x _->pure (False,x)) nested [AgentAction "copy" []]
      ensure "raw tool and plan details never retain bearer values" (all (not . (`T.isInfixOf` clipboard expanded)) ("private-main-key":tokens))
      peerCancels<-newIORef (0::Int)
      peer<-AH.registerAgent hub "Peer" root (AH.AgentDriver root "private-peer-key" (AH.Capabilities False False False [])
        (\_ ->pure (Right (AH.Capabilities False False False []))) (\_ ->pure (Right Null)) (modifyIORef' peerCancels (+1)) (pure ()) (\_ ->pure (Left "unsupported"))) >>= right
      ticket<-AH.sendAgent hub (AH.Agent peer) primary "Count files" >>= right
      finished<-tickUntil (\d->do result<-AH.waitAgent hub AH.Human primary ticket 0 >>= right; pure (field "status" result==Just ("completed"::T.Text) && "Count files" `T.isInfixOf` activeText d)) connected
      result<-AH.waitAgent hub AH.Human primary ticket 0 >>= right
      ensure "peer message reaches the primary provider and returns text" (maybe False (T.isInfixOf "Count files") (field "result" result >>= field "text"))
      ensure "peer output cannot publish private provider references" (not ("private-main-key" `T.isInfixOf` T.pack (show result)))
      ensure "peer result cannot publish bearer tokens" (not (null tokens) && all (not . (`T.isInfixOf` T.pack (show result))) tokens)
      copied<-snd <$> conversationEffects conversation (\x _ ->pure (False,x)) finished [AgentAction "copy" []]
      ensure "peer transcript attributes the sender" (("Agent "<>AH.agentIdText peer) `T.isInfixOf` clipboard copied && "not the human user seat" `T.isInfixOf` clipboard copied)
      child<-AH.spawnAgent hub AH.Human (AH.SpawnSpec "Worker" "Permission" root AH.Shared AH.Fresh Nothing Nothing) >>= right
      _<-AH.sendAgent hub AH.Human child "permission" >>= right
      approval<-tickUntil (pure . maybe False (T.isPrefixOf "Agent permission:" . dialogTitle) . dialog) finished
      ensure "agents cannot approve their own UI" (not (guestKeyboardAllowed approval) && not (guestCommandAllowed AgentDirectory))
      _<-AH.cancelAgent hub AH.Human child >>= right
      cleared<-tickUntil (pure . maybe True (not . T.isPrefixOf "Agent permission:" . dialogTitle) . dialog) approval
      ensure "cancel removes stale human approval" (dialog cleared==Nothing)
      peerTicket<-AH.sendAgent hub (AH.Agent primary) peer "Peer attribution fixture" >>= right
      _<-AH.waitAgent hub AH.Human peer peerTicket 1000 >>= right
      let primaryDrafted=draft "primary unsent" (Selection 3 7) cleared
          primaryText=bodyText "" primaryDrafted
      primaryCanonical<-canonicalWindowText primaryDrafted
      earlierSource<-maybe (fail "Primary source capture missing before output") pure
        (M.lookup "" (conversationViews connected) >>= conversationSource)
      sourceKey<-checkpointKey primaryDrafted
      earlierKey<-checkpointKey primaryDrafted {conversationViews=M.adjust
        (\view->view {conversationSource=Just earlierSource}) "" (conversationViews primaryDrafted)}
      unpaintedKey<-checkpointKey primaryDrafted {conversationViews=M.adjust
        (\view->view {conversationLogical=error "Checkpoint key forced adopted logical source"}) "" (conversationViews primaryDrafted)}
      ensure "received source identity invalidates checkpoints without inspecting painted catalogues"
        (sourceKey/=earlierKey && sourceKey==unpaintedKey)
      directoryView<-ui "directory" [] primaryDrafted
      directory<-AH.listAgents hub AH.Human >>= right
      ensure "registered peer remains listed in the directory" (any ((==Just (AH.agentIdText peer)) . field "id") (maybe [] id (field "agents" directory :: Maybe [Value])))
      history<-select peer directoryView {dialog=Nothing} >>= tickUntil (pure . T.isInfixOf ("Agent "<>AH.agentIdText primary<>" (peer message)") . activeText)
      ensure "child opens in the existing protected conversation composer" (activeConversation history && not (guestKeyboardAllowed history))
      let historyText=activeText history
      ensure "history preserves peer identity instead of assigning the human seat" (("Agent "<>AH.agentIdText primary<>" (peer message)") `T.isInfixOf` historyText)
      childTransitions<-mapM (\changed->guestTransitionAllowed history changed []) [history {childAgentSteering=True},history {childAgentContextUsage=Just (1,2)},history {childAgentSettings=agentSettings connected}]
      ensure "agent input cannot manufacture selected-child authority or usage" (not (or childTransitions))
      let childDrafted=draft "child unsent" (Selection 2 5) history
      primaryAgain<-ui "show" [] childDrafted
      ensure "switching restores primary draft caret and transcript" (T.null (conversationTarget primaryAgain) && contents (composerBuffer primaryAgain)=="primary unsent" && composerSelection primaryAgain==Selection 3 7 && activeText primaryAgain==primaryText)
      childAgain<-select peer primaryAgain
      ensure "switching restores child draft caret" (contents (composerBuffer childAgain)=="child unsent" && composerSelection childAgain==Selection 2 5)
      let childTool ident state details=object
            ["toolCallId" .= (ident::T.Text),"title" .= ident,"status" .= (state::T.Text),"rawInput" .= (details::T.Text)]
          actions name desktop=[values | Just w<-[activeWindow desktop],Just controls<-[windowConversationControls desktop w],
            (_,_,action,values)<-hostBodyActions controls,action==name]
          childGroupText=activeText
      AH.recordAgentEvent hub peer "tool" (childTool "child-first" "pending" "first arguments")
      AH.recordAgentEvent hub peer "tool" (childTool "child-second" "pending" "second arguments")
      groupedChild<-tickUntil (pure . (\d->length (actions "toggle-tool-run" d)==1 && "2 tool calls" `T.isInfixOf` activeText d)) childAgain
      ensure "child tools form one collapsed run" (length (actions "toggle-tool-run" groupedChild)==1 &&
        null (actions "toggle-activity" groupedChild) && "2 tool calls" `T.isInfixOf` childGroupText groupedChild)
      let groupValues=case actions "toggle-tool-run" groupedChild of values:_->values; _->error "Missing child run action"
      openChildRun<-ui "toggle-tool-run" groupValues groupedChild >>= tickUntil (pure . (\d->length (actions "toggle-activity" d)==2))
      let firstCall=case actions "toggle-activity" openChildRun of values:_->values; _->error "Missing child call action"
      openChildCall<-ui "toggle-activity" firstCall openChildRun >>= tickUntil (pure . T.isInfixOf "first arguments" . activeText)
      ensure "child call exposes its raw arguments" ("first arguments" `T.isInfixOf` childGroupText openChildCall)
      AH.recordAgentEvent hub peer "tool" (object ["toolCallId" .= ("child-first"::T.Text),"status" .= ("completed"::T.Text),"rawOutput" .= ("first result"::T.Text)])
      AH.recordAgentEvent hub peer "tool" (childTool "child-third" "in_progress" "third arguments")
      streamedChild<-tickUntil (pure . (\d->length (actions "toggle-activity" d)==3 && all (`T.isInfixOf` activeText d) ["3 tool calls","2 running","first result"])) openChildCall
      ensure "child updates count calls rather than events and preserve the open run" (actions "toggle-tool-run" streamedChild==[groupValues] &&
        length (actions "toggle-activity" streamedChild)==3 && "3 tool calls" `T.isInfixOf` childGroupText streamedChild &&
        "2 running" `T.isInfixOf` childGroupText streamedChild)
      ensure "child refresh preserves open call input and all streamed raw output" (all (`T.isInfixOf` childGroupText streamedChild) ["first arguments","first result"])
      switchedPrimary<-ui "show" [] streamedChild
      switchedChild<-select peer switchedPrimary >>= tickUntil (pure . (\d->length (actions "toggle-activity" d)==3 && "first result" `T.isInfixOf` activeText d))
      ensure "child run and call expansion survive switching through Primary" (childGroupText switchedChild==childGroupText streamedChild &&
        actions "toggle-tool-run" switchedChild==[groupValues] && length (actions "toggle-activity" switchedChild)==3)
      collapsedChild<-ui "toggle-tool-run" groupValues switchedChild >>= tickUntil (pure . (\d->null (actions "toggle-activity" d) && not ("first arguments" `T.isInfixOf` activeText d)))
      copiedChild<-ui "copy" [] collapsedChild
      ensure "collapsed child run retains all raw call updates for copying" (all (`T.isInfixOf` clipboard copiedChild)
        ["first arguments","first result","second arguments","third arguments"])
      reopenedChild<-ui "toggle-tool-run" groupValues collapsedChild >>= tickUntil (pure . (\d->length (actions "toggle-activity" d)==3 && "first arguments" `T.isInfixOf` activeText d))
      ensure "child per-call expansion survives collapsing its run" (childGroupText reopenedChild==childGroupText streamedChild)
      AH.recordAgentEvent hub peer "output" (object ["text" .= ("Reply between child tool runs"::T.Text)])
      AH.recordAgentEvent hub peer "tool" (childTool "child-isolated" "completed" "isolated arguments")
      separatedChild<-tickUntil (pure . (\d->length (actions "toggle-activity" d)==4 && "Reply between child tool runs" `T.isInfixOf` activeText d)) reopenedChild
      ensure "child reply ends a tool run and leaves the next isolated call visible" (actions "toggle-tool-run" separatedChild==[groupValues] &&
        length (actions "toggle-activity" separatedChild)==4 && "Reply between child tool runs" `T.isInfixOf` childGroupText separatedChild)
      let largeTool=object ["title" .= ("Large tool"::T.Text),"payload" .= T.replicate 600000 "x"]
      AH.recordAgentEvent hub peer "tool" largeTool
      AH.recordAgentEvent hub peer "tool" largeTool
      AH.recordAgentEvent hub peer "output" (object ["text" .= ("latest reply after paged tools"::T.Text)])
      paged<-tickUntil (pure . T.isInfixOf "latest reply after paged tools" . activeText) childAgain
      ensure "live child reaches newest reply past history byte cap" ("latest reply after paged tools" `T.isInfixOf` activeText paged)
      _<-AH.cancelAgent hub AH.Human primary >>= right
      afterPrimaryCancel<-tickBody presentation conversation childAgain
      ensure "primary mailbox cancellation does not cancel selected child" . (==0) =<< readIORef peerCancels
      ensure "primary cancellation preserves child target and draft" (conversationTarget afterPrimaryCancel==AH.agentIdText peer && contents (composerBuffer afterPrimaryCancel)=="child unsent")
      let primaryMetadata=childAgain {agentReplying=True,agentSteering=True,agentContextUsage=Just (42,84),agentSettings=[AgentSetting "model" "Model" "model" "primary-model" [("primary-model","Primary")]]}
      ensure "child capabilities do not inherit primary steering or model settings" (not (conversationSteering primaryMetadata) && not (commandEnabled primaryMetadata (AgentChoose "")) && not (commandEnabled primaryMetadata (AgentSet "model" "primary-model")))
      ensure "child view does not show primary context usage" (conversationContextUsage primaryMetadata==Nothing)
      (_,settingsReply)<-chatTool conversation primaryMetadata "agent_settings" (object [])
      settingsInfo<-settingsReply >>= right
      ensure "settings snapshot identifies primary scope and does not mix child busy state" (field "scope" settingsInfo==Just ("primary"::T.Text) && field "replying" settingsInfo==Just False)
      cancellingPeer<-ui "cancel" [] afterPrimaryCancel
      _<-tickUntil (\_->(==1) <$> readIORef peerCancels) cancellingPeer
      settingPeer<-ui "set-config" ["model","invented"] childAgain
      rejectedPeer<-tickUntil (pure . T.isInfixOf "Select a connected child agent" . status) settingPeer
      ensure "child setters cannot reconfigure the primary" (conversationTarget rejectedPeer==AH.agentIdText peer)
      liveChild<-AH.spawnAgent hub (AH.Agent primary) (AH.SpawnSpec "Live child" "Inspect source" root AH.Shared AH.Fresh Nothing Nothing) >>= right
      parentTicket<-AH.sendAgent hub (AH.Agent primary) liveChild "parent instruction" >>= right
      _<-AH.waitAgent hub AH.Human liveChild parentTicket 3000 >>= right
      refreshedDirectory<-ui "directory" [] childAgain
      listedNow<-AH.listAgents hub AH.Human >>= right
      ensure "live child remains listed in the directory" (any ((==Just (AH.agentIdText liveChild)) . field "id") (maybe [] id (field "agents" listedNow::Maybe [Value])))
      liveView<-select liveChild refreshedDirectory {dialog=Nothing} >>= tickUntil (pure . T.isInfixOf "controlling parent" . activeText)
      ensure "parent-owned child shows controlling-parent attribution" ("controlling parent" `T.isInfixOf` activeText liveView)
      ensure "live child title/dropdown use advertised child model choices" ("small" `T.isInfixOf` conversationTitle liveView && commandEnabled liveView (AgentChoose ""))
      changedChild<-ui "set-config" ["model","large"] liveView
      configuredChild<-tickUntil (pure . (\d->not (agentReplying d) && any ((=="large").settingCurrent) (childAgentSettings d))) changedChild
      ensure "child configuration does not replace primary provider settings" (agentSettings configuredChild==agentSettings liveView && "large" `T.isInfixOf` conversationTitle configuredChild)
      waitingChild<-submit QuerySubmit (draft "steer-wait" (Selection 10 10) configuredChild)
      steerReady<-tickUntil (pure . (==Just (120,1000)) . childAgentContextUsage) waitingChild
      ensure "child reports its own context usage" (conversationContextUsage steerReady==Just (120,1000))
      steeringChild<-submit SteerSubmit (draft "human direction" (Selection 15 15) steerReady)
      ensure "pending child steering keeps the draft until acknowledged" (contents (composerBuffer steeringChild)=="human direction")
      steeredChild<-tickUntil (pure . (\d->not (agentReplying d) && T.null (contents (composerBuffer d)) && "human direction" `T.isInfixOf` activeText d)) steeringChild
      ensure "child steering retains human peer attribution" ("Human (peer message)" `T.isInfixOf` activeText steeredChild && "human direction" `T.isInfixOf` activeText steeredChild)
      humanSent<-submit QuerySubmit (draft "human followup" (Selection 14 14) steeredChild)
      replied<-tickUntil (pure . (\d->"human followup" `T.isInfixOf` activeText d && T.null (contents (composerBuffer d)))) humanSent
      ensure "accepted human message clears only its own composer" (T.null (contents (composerBuffer replied)) && maybe False ((=="primary unsent").contents.editorDraftBuffer) (M.lookup "" (conversationViews replied) >>= \view->M.lookup (conversationDraftRef view) (editorDrafts replied)))
      ensure "child transcript carries no provider keys" (not ("private-main-key" `T.isInfixOf` activeText replied))
      firstQueued<-submit QuerySubmit (draft "permission" (Selection 10 10) replied)
      secondQueued<-submit QuerySubmit (draft "must-not-replay" (Selection 15 15) firstQueued)
      primaryWhileQueued<-ui "show" [] secondQueued
      configuredPrimary<-ui "configure" ["0","python3",TE.decodeUtf8 (BL.toStrict (encode [script])),"{}"] primaryWhileQueued
      childAfterPrimaryConfig<-select liveChild configuredPrimary
      awaiting<-tickUntil (pure . maybe False (T.isPrefixOf "Agent permission:" . dialogTitle) . dialog) childAfterPrimaryConfig
      ensure "human can queue a followup while child is running" (agentQueued awaiting>=1)
      _<-AH.cancelAgent hub AH.Human primary >>= right
      childStillWaiting<-tickBody presentation conversation awaiting
      ensure "primary cancellation leaves child approval pending" (maybe False (T.isPrefixOf "Agent permission:" . dialogTitle) (dialog childStillWaiting))
      cancelledChild<-ui "cancel" [] childStillWaiting
      afterChildCancel<-tickUntil (\desktop->do entry<-AH.statusAgent hub AH.Human liveChild >>= right; pure (field "status" entry==Just ("idle"::T.Text) && field "queued" entry==Just (0::Int) && dialog desktop==Nothing)) cancelledChild
      privateConfig<-submit QuerySubmit (draft "private-configuration" (Selection 21 21) afterChildCancel)
      redactedConfig<-tickUntil (pure . (\d->not (agentReplying d) && null (childAgentSettings d))) privateConfig
      let private="private-main-key":tokens
      publicDirectory<-AH.listAgents hub AH.Human >>= right
      publicCapture<-capture font redactedConfig True >>= right
      ensure "child capability keys never reach directory/title or screen text/image input"
        (all (not . (`T.isInfixOf` T.pack (show publicDirectory))) private && all (not . (`T.isInfixOf` T.pack (show (agentSettings redactedConfig)))) private && all (not . (`T.isInfixOf` T.pack (show publicCapture))) private)
      let finalDraft=draft "recover child draft" (Selection 4 9) redactedConfig
          recovery=root </> "conversation-views.checkpoint"
      writeCheckpoint recovery finalDraft >>= right
      recovered<-readCheckpoint recovery initial >>= right
      ensure "selected child and active draft survive recovery" (conversationTarget recovered==AH.agentIdText liveChild && contents (composerBuffer recovered)=="recover child draft" && composerSelection recovered==Selection 4 9)
      primaryProgress<-newIORef (0,False,"not ticked"::T.Text)
      let awaitPrimary desktop=do
            text<-canonicalWindowText desktop
            writeIORef primaryProgress (T.length text,not (T.null (activeText desktop)),status desktop)
            pure (not (T.null (activeText desktop)) && text==primaryCanonical)
          awaitRecovered label desktop=testUntil label awaitPrimary desktop
            `onException` (readIORef primaryProgress >>= \progress->putStrLn
              ("Primary recovery progress: expected scalars="++show (T.length primaryCanonical)++", actual="++show progress))
      restoredPrimary<-ui "show" [] recovered >>= awaitRecovered "hidden primary recovery"
      restoredCanonical<-canonicalWindowText restoredPrimary
      ensure "hidden primary transcript and draft survive recovery" (restoredCanonical==primaryCanonical && contents (composerBuffer restoredPrimary)=="primary unsent")
      ensure "hidden unsent drafts still prevent quiet Exit" (conversationHasDraft recovered && not (null (conversationViews recovered)))
      recoveredShown<-ui "show" [] recovered >>= awaitRecovered "repeated primary recovery"
      shownCanonical<-canonicalWindowText recoveredShown
      ensure "show after switching does not lose primary transcript" (shownCanonical==primaryCanonical)
      caller<-captureQuestionCaller conversation primary >>= right
      (questionView,_)<-chatToolAs conversation (Just caller) recoveredShown "ask_user" (object ["question" .= ("Choose privately"::T.Text)])
      let privateAnswer=questionView {chatQuestion=fmap (\q->q {questionBuffer=newBuffer "unsent secret answer",questionSelection=Selection 20 20}) (chatQuestion questionView)}
      paintedAnswer<-tickUntil (pure . (\d->"Choose privately" `T.isInfixOf` activeText d &&
        maybe False (maybe False (const True) . windowQuestion d) (activeWindow d))) privateAnswer
      hiddenQuestion<-select liveChild paintedAnswer
      ensure "hidden primary question answer never becomes readable transcript" (all (not . T.isInfixOf "unsent secret answer") (readableBodies hiddenQuestion))
      writeCheckpoint recovery hiddenQuestion >>= right
      questionBytes<-BL.readFile recovery
      questionCheckpoint<-readCheckpoint recovery initial >>= right
      ensure "hidden transient answer is omitted from recovery"
        (not ("unsent secret answer" `T.isInfixOf` TE.decodeUtf8 (BL.toStrict questionBytes)) &&
         all (not . T.isInfixOf "unsent secret answer" . (\target->bodyText target questionCheckpoint)) (M.keys (conversationViews questionCheckpoint)))
      returnedQuestion<-ui "show" [] hiddenQuestion
      ensure "switching preserves the live primary answer without sending it" (maybe False ((=="unsent secret answer").contents.questionBuffer) (chatQuestion returnedQuestion))
      _<-AH.endAgent hub AH.Human primary >>= right
      _<-tickBody presentation conversation finished
      invalid<-AH.statusAgent hub (AH.Agent primary) primary
      ensure "ended primary loses orchestration authority" (case invalid of Left _->True; _->False)
    recoveryRecord<-newSessionRecord Nothing ["--",root]
    rememberSession recoveryRecord
    let recoveredPath=root </> "newer-child.checkpoint"
        fakeDriver=AH.AgentDriver root "private-recovery-child" (AH.Capabilities False False False [])
          (\_ ->pure (Right (AH.Capabilities False False False []))) (\_ ->pure (Right Null)) (pure ()) (pure ()) (\_ ->pure (Left "unsupported"))
    recoveredChild<-environment "THC_EDIT_SESSION" (Just (sessionId recoveryRecord)) $ C.withConsoles $ \consoles -> withConversationAt consoles root $ \conversation -> withTextPresentation $ \presentation->do
      let agents=conversationAgents conversation
          hub=AR.agentHub agents
      child<-AH.registerAgent hub "Recovered child" root fakeDriver >>= right
      AH.recordAgentEvent hub child "output" (object ["text" .= ("older Hub history"::T.Text)])
      (_,selected)<-conversationEffects conversation (\d _->pure (False,d)) (initialDesktop (80,25)) [AgentSidebarAction (ShowAgent child)]
      let awaitOlder current=do
            next<-tickBody presentation conversation current
            if "older Hub history" `T.isInfixOf` activeText next then pure next else threadDelay 10000 >> awaitOlder next
      older<-timeout 5000000 (awaitOlder selected) >>= maybe (fail "Original child body preparation timed out") pure
      let newer=(setComposerInput (newBuffer "recovered draft") (Selection 0 0) True older) {agentReplying=True,agentQueued=5}
      writeCheckpoint recoveredPath newer >>= right
      -- The independent durable source enters through the real schema5 trust
      -- boundary. A painted refresh cannot create logical transcript authority.
      encoded<-BL.readFile recoveredPath
      value<-either error pure (eitherDecode encoded)
      let text="newer Desktop child transcript"::T.Text
          item=object ["id" .= (0::Int),"revision" .= (0::Int),"content" .=
            object ["kind" .= ("reply"::T.Text),"role" .= ("Agent"::T.Text),"markdown" .= text]]
          replaceView (Object fields)
            | KM.lookup "target" fields==Just (toJSON (AH.agentIdText child))=
                Object (KM.insert "body" (object ["items" .= [item]])
                  (KM.insert "anchor" Null
                    (KM.insert "replySelection" (toJSON (toJSON (0::Int,0::Int,0::Int),toJSON (0::Int,0::Int,T.length text))) fields)))
          replaceView other=other
          input=case value of
            Object fields | Just (Array entries)<-KM.lookup "conversationViews" fields->
              Object (KM.insert "conversationViews" (Array (fmap replaceView entries)) fields)
            _->error "Missing fixture conversation views"
      BL.writeFile recoveredPath (encode input)
      acquired<-readCheckpoint recoveredPath (initialDesktop (80,25)) >>= right
      writeCheckpoint recoveredPath acquired >>= right
      AR.activateAgentCheckpoint agents
      AR.checkpointAgents agents >>= right
      pure child
    environment "THC_EDIT_SESSION" (Just (sessionId recoveryRecord)) $ C.withConsoles $ \consoles -> withConversationAt consoles root $ \conversation -> withTextPresentation $ \presentation->do
      recovered<-readCheckpoint recoveredPath (initialDesktop (80,25)) >>= right
      retained<-tickBody presentation conversation recovered
      ensure "stale recovered Hub history cannot overwrite newer logical transcript"
        ("newer Desktop child transcript" `T.isInfixOf` activeText retained && not ("older Hub history" `T.isInfixOf` activeText retained) && contents (composerBuffer retained)=="recovered draft")
      ensure "recovered child projects current status while retaining text" (not (agentReplying retained) && agentQueued retained==0)
      let (copyRequested,copyEffects)=runCommand Copy retained
      ensure "recovered selection uses the actual logical copy worker" (case copyEffects of [CopyConversation{}]->True; _->False)
      (_,copying)<-textPresentationEffects presentation
        (conversationEffects conversation (\d _->pure (False,d))) copyRequested copyEffects
      let awaitCopy current=do
            next<-tickBody presentation conversation current
            if clipboard next=="newer Desktop child transcript" then pure next else threadDelay 10000 >> awaitCopy next
      copied<-timeout 5000000 (awaitCopy copying) >>= maybe (fail "Recovered logical copy timed out") pure
      ensure "recovered transcript remains copyable before reconnect" (clipboard copied=="newer Desktop child transcript")
      let hub=AR.agentHub (conversationAgents conversation)
      AH.updateExternalAgent hub recoveredChild fakeDriver >>= right
      AH.recordAgentEvent hub recoveredChild "output" (object ["text" .= ("new live child output"::T.Text)])
      let awaitLive current=do
            next<-tickBody presentation conversation current
            if "new live child output" `T.isInfixOf` activeText next then pure next else threadDelay 10000 >> awaitLive next
      refreshed<-timeout 5000000 (awaitLive retained) >>= maybe (fail "Live recovered body preparation timed out") pure
      ensure "live output replaces frozen recovered presentation" ("new live child output" `T.isInfixOf` activeText refreshed)
      ensure "live recovery refresh retains the existing frame geometry"
        (map (\w->(windowId w,bounds w)) (windows refreshed)==map (\w->(windowId w,bounds w)) (windows retained))
      let closed=fst (runCommand Close refreshed)
      AH.recordAgentEvent hub recoveredChild "output" (object ["text" .= ("output after close"::T.Text)])
      stayedClosed<-tickBody presentation conversation closed
      ensure "live child output cannot reopen or focus its closed conversation"
        (map windowId (windows stayedClosed)==map windowId (windows closed) &&
          fmap windowId (activeWindow stayedClosed)==fmap windowId (activeWindow closed))
    corrupt<-newSessionRecord Nothing ["--",root]
    rememberSession corrupt
    checkpoint<-(++".agents.json") <$> checkpointPath (sessionId corrupt)
    writeFile checkpoint "{incomplete"
    environment "THC_EDIT_SESSION" (Just (sessionId corrupt)) $ C.withConsoles $ \consoles -> withConversationAt consoles root $ \conversation -> withTextPresentation $ \presentation->do
      noticed<-tickBody presentation conversation (initialDesktop (80,25))
      ensure "agent checkpoint failures reach the status line" ("checkpoint" `T.isInfixOf` status noticed && "retained" `T.isInfixOf` status noticed)
      consumed<-tickBody presentation conversation noticed {status="ordinary status"}
      ensure "agent runtime notices are shown only once" (status consumed=="ordinary status")
  where
    right=either (error . T.unpack) pure
    ensure label ok=unless ok (error label)

-- Run the same existing protocol and presentation owners as the application's
-- final body phase; tests wait for adopted text rather than provider status.
tickBody :: TextPresentation -> ConversationState -> Desktop -> IO Desktop
tickBody owner conversation current=do
  protocol<-tickConversation conversation current
  requests<-conversationBodyRequests conversation protocol
  (prepared,completed)<-tickTextPresentation owner requests protocol
  adoptConversationBodies conversation completed prepared

-- Public read_window acquisition checks exact logical source independently of
-- width, bubble furniture and the asynchronously adopted bounded viewport.
canonicalWindowText :: Desktop -> IO T.Text
canonicalWindowText desktop=withBufferReadCommands $ \commands->do
  window<-maybe (fail "No canonical conversation frame") pure (activeWindow desktop)
  target<-either (fail . T.unpack) pure (windowReadTarget desktop (windowId window))
  page<-either (fail . T.unpack) pure (readPage 1 1000 0)
  value<-readWindowCommand commands (captureWindow desktop target) page >>= either (fail . T.unpack) pure
  case (field "text" value,field "truncated" value,field "lineCount" value::Maybe Int,field "totalLines" value) of
    (Just text,Just False,Just count,Just total) | count==total->pure text
    _->fail "Canonical recovery fixture requires one complete read page"

bodyText :: T.Text -> Desktop -> T.Text
bodyText target desktop=maybe "" (\prepared->let text=W.preparedWindowText prepared in contentSlice text 0 (contentLength text))
  (conversationBodySnapshot target desktop)

readableBodies :: Desktop -> [T.Text]
readableBodies desktop=[contentSlice text 0 (contentLength text) | target<-M.keys (conversationViews desktop),
  Just prepared<-[conversationBodySnapshot target desktop],Just (_,text)<-[sanitizedPreparedContent prepared]]

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "field" (.:key))
environment :: String -> Maybe String -> IO a -> IO a
environment key value action=bracket (lookupEnv key <* put value) put (const action)
  where put=maybe (unsetEnv key) (setEnv key)
temporary :: IO FilePath
temporary=do root<-getTemporaryDirectory; (path,h)<-openTempFile root "agent-integration"; hClose h; removeFile path; createDirectory path; canonicalizePath path
fixture :: String
fixture=unlines
  ["import sys,json,time"
  ,"tokens=[]; pending=None; chosen={'model':'small','effort':'low'}"
  ,"def options(): return [{'id':'model','name':'Model','category':'model','type':'select','currentValue':chosen['model'],'options':[{'value':'small','name':'Small'},{'value':'large','name':'Large'}]},{'id':'effort','name':'Effort','category':'thought_level','type':'select','currentValue':chosen['effort'],'options':[{'value':'low','name':'Low'},{'value':'high','name':'High'}]}]"
  ,"def send(x): print(json.dumps(x),flush=True)"
  ,"for line in sys.stdin:"
  ," r=json.loads(line); m=r.get('method'); p=r.get('params',{}); v={}"
  ," if m=='initialize': v={'protocolVersion':1,'_meta':{'steering':{'supported':True}},'agentCapabilities':{'loadSession':True}}"
  ," elif m in ('session/new','session/load'):"
  ,"  tokens=[e['value'] for s in p.get('mcpServers',[]) for e in s.get('env',[]) if e['name']=='THC_EDIT_MCP_TOKEN']"
  ,"  v={'sessionId':'private-main-key','configOptions':options()}"
  ," elif m=='session/set_config_option': chosen[p['configId']]=p['value']; v={'configOptions':options()}"
  ," elif m=='_session/steering':"
  ,"  v={'outcome':'injected'}"
  ,"  if pending is not None: send({'jsonrpc':'2.0','id':pending,'result':{'stopReason':'end_turn'}}); pending=None"
  ," elif m=='session/prompt':"
  ,"  text=\"\\n\".join(block['text'] for block in p['prompt'])+' private-main-key '+str(tokens)"
  ,"  if any(block['text'].strip().endswith('steer-wait') for block in p['prompt']):"
  ,"   pending=r['id']; send({'jsonrpc':'2.0','method':'session/update','params':{'sessionId':'private-main-key','update':{'sessionUpdate':'usage_update','used':120,'size':1000}}}); continue"
  ,"  if 'private-configuration' in text:"
  ,"   cfg=options(); cfg[0]['currentValue']='private-main-key'; cfg[1]['options'][0]['name']=str(tokens); send({'jsonrpc':'2.0','method':'session/update','params':{'sessionId':'private-main-key','update':{'sessionUpdate':'config_option_update','configOptions':cfg}}})"
  ,"  if 'split-private' in text:"
  ,"   for kind in ['agent_message_chunk','user_message_chunk']:"
  ,"    for secret in ['private-main-key']+tokens:"
  ,"     for part in [secret[:8],secret[8:17],secret[17:]+' ']:"
  ,"      send({'jsonrpc':'2.0','method':'session/update','params':{'sessionId':'private-main-key','update':{'sessionUpdate':kind,'content':{'type':'text','text':part}}}}); send({'jsonrpc':'2.0','id':999,'result':{}}); time.sleep(0.03)"
  ,"   send({'jsonrpc':'2.0','id':r['id'],'result':{'stopReason':'end_turn'}}); continue"
  ,"  if 'nested-private' in text:"
  ,"   for kind in ['tool_call','plan']: send({'jsonrpc':'2.0','method':'session/update','params':{'sessionId':'private-main-key','update':{'sessionUpdate':kind,'toolCallId':'nested','title':str(tokens),'rawInput':{'nested':[{'private-main-key':tokens}]}}}})"
  ,"  if any(block['text'].strip().endswith('permission') for block in p['prompt']):"
  ,"   pending=r['id']; send({'jsonrpc':'2.0','id':'permit','method':'session/request_permission','params':{'sessionId':'private-main-key','toolCall':{'title':'Write file'},'options':[{'optionId':'yes','name':'Allow','kind':'allow_once'},{'optionId':'no','name':'Reject','kind':'reject_once'}]}}); continue"
  ,"  send({'jsonrpc':'2.0','method':'session/update','params':{'sessionId':'private-main-key','update':{'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':text}}}})"
  ,"  v={'stopReason':'end_turn'}"
  ," elif m=='session/cancel' and pending is not None:"
  ,"  send({'jsonrpc':'2.0','id':pending,'result':{'stopReason':'cancelled'}}); pending=None"
  ," if m and 'id' in r: send({'jsonrpc':'2.0','id':r['id'],'result':v})"]
