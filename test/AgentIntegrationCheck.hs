{-# LANGUAGE OverloadedStrings #-}
module AgentIntegrationCheck (checks, fixture) where

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO (hClose,openTempFile)
import System.Timeout (timeout)
import Hide.Conversation
import qualified Hide.AgentRuntime as AR
import qualified Hide.AgentHub as AH
import Hide.GuestAccess (guestKeyboardAllowed,guestCommandAllowed,sanitizedBuffer,guestTransitionAllowed)
import qualified Data.Map.Strict as M
import Data.List (findIndex)
import Data.IORef
import Hide.Buffer (contents,newBuffer,Selection(..),snapshotBuffer)
import Hide.Recovery (writeCheckpoint,readCheckpoint)
import qualified Hide.Font as Font
import Hide.ScreenCapture (capture)
import Hide.Session
import Hide.Model

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
    environment "THC_EDIT_SESSION" (Just (sessionId record)) $ withConversationAt root $ \conversation -> do
      let agents=conversationAgents conversation
          hub=AR.agentHub agents
          primary=AR.primaryAgent agents
          initial=(initialDesktop (80,25)) {defaultDirectory=Just root}
          send text d=snd <$> conversationEffects conversation (\x _->pure (False,x)) d [AgentAction "send" ["0",text,"false","false","false"]]
          tickUntil test d=do
            answer<-timeout 5000000 (loop d)
            maybe (error "Agent conversation integration timed out") pure answer
            where loop current=do next<-tickConversation conversation current; ok<-test next; if ok then pure next else threadDelay 10000 >> loop next
      connected<-send "Hello" initial >>= tickUntil (pure . (=="Agent: end_turn") . status)
      servers<-AR.primaryServers agents
      let tokens=[value | server<-servers, entry<-maybe [] id (field "env" server :: Maybe [Value]),Just value<-[field "value" entry :: Maybe T.Text]]
      let publicSafe desktop=do
            let readable=T.concat [body | ident<-M.keys (buffers desktop),Just body<-[sanitizedBuffer desktop ident]]
                private="private-main-key":tokens
            screen<-capture font desktop False
            ensure "primary transcript never exposes private keys through buffer reads" (all (not . (`T.isInfixOf` readable)) private)
            ensure "primary transcript never exposes private keys through screen capture" (all (not . (`T.isInfixOf` T.pack (show screen))) private)
      publicSafe connected
      split<-send "split-private" connected >>= tickUntil (\desktop->publicSafe desktop >> pure (status desktop=="Agent: end_turn"))
      publicSafe split
      nested<-send "nested-private" split >>= tickUntil (\desktop->publicSafe desktop >> pure (status desktop=="Agent: end_turn"))
      expanded<-snd <$> conversationEffects conversation (\x _->pure (False,x)) nested [AgentAction "copy" []]
      ensure "raw tool and plan details never retain bearer values" (all (not . (`T.isInfixOf` clipboard expanded)) ("private-main-key":tokens))
      peerCancels<-newIORef (0::Int)
      peer<-AH.registerAgent hub "Peer" root (AH.AgentDriver root "private-peer-key" (AH.Capabilities False False False [])
        (\_ ->pure (Right (AH.Capabilities False False False []))) (\_ ->pure (Right Null)) (modifyIORef' peerCancels (+1)) (pure ()) (\_ ->pure (Left "unsupported"))) >>= right
      ticket<-AH.sendAgent hub (AH.Agent peer) primary "Count files" >>= right
      finished<-tickUntil (\_ ->do result<-AH.waitAgent hub AH.Human primary ticket 0 >>= right; pure (field "status" result==Just ("completed"::T.Text))) connected
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
      let primaryDrafted=cleared {composerBuffer=newBuffer "primary unsent",composerSelection=Selection 3 7}
          primaryText=maybe "" (contents.documentBuffer.snd) (conversationDocument "" primaryDrafted)
          ui action values d=snd <$> conversationEffects conversation (\x _->pure (False,x)) d [AgentAction action values]
      directoryView<-ui "directory" [] primaryDrafted
      directory<-AH.listAgents hub AH.Human >>= right
      let listed=maybe [] id (field "agents" directory :: Maybe [Value])
          index=maybe (error "Peer absent from directory") id (findIndex ((==Just (AH.agentIdText peer)) . field "id") listed)
      history<-snd <$> conversationEffects conversation (\x _->pure (False,x)) directoryView {dialog=Nothing} [AgentAction "directory-select" ["0",T.pack (show index)]]
      ensure "child opens in the existing protected conversation composer" (activeConversation history && not (guestKeyboardAllowed history))
      let historyText=activeText history
      ensure "history preserves peer identity instead of assigning the human seat" (("Agent "<>AH.agentIdText primary<>" (peer message)") `T.isInfixOf` historyText)
      ensure "agent input cannot manufacture selected-child authority or usage" (all (\changed->not (guestTransitionAllowed history changed [])) [history {childAgentSteering=True},history {childAgentContextUsage=Just (1,2)},history {childAgentSettings=agentSettings connected}])
      let childDrafted=history {composerBuffer=newBuffer "child unsent",composerSelection=Selection 2 5}
      primaryAgain<-ui "show" [] childDrafted
      ensure "switching restores primary draft caret and transcript" (T.null (conversationTarget primaryAgain) && contents (composerBuffer primaryAgain)=="primary unsent" && composerSelection primaryAgain==Selection 3 7 && activeText primaryAgain==primaryText)
      childAgain<-ui "directory-select" ["0",T.pack (show index)] primaryAgain
      ensure "switching restores child draft caret" (contents (composerBuffer childAgain)=="child unsent" && composerSelection childAgain==Selection 2 5)
      let childTool ident state details=object
            ["toolCallId" .= (ident::T.Text),"title" .= ident,"status" .= (state::T.Text),"rawInput" .= (details::T.Text)]
          actions name desktop=[values | (_,_,action,values)<-chatActions desktop,action==name]
          childGroupText=activeText
      AH.recordAgentEvent hub peer "tool" (childTool "child-first" "pending" "first arguments")
      AH.recordAgentEvent hub peer "tool" (childTool "child-second" "pending" "second arguments")
      groupedChild<-tickConversation conversation childAgain
      ensure "child tools form one collapsed run" (length (actions "toggle-tool-run" groupedChild)==1 &&
        null (actions "toggle-activity" groupedChild) && "2 tool calls" `T.isInfixOf` childGroupText groupedChild)
      let groupValues=case actions "toggle-tool-run" groupedChild of values:_->values; _->error "Missing child run action"
      openChildRun<-ui "toggle-tool-run" groupValues groupedChild
      let firstCall=case actions "toggle-activity" openChildRun of values:_->values; _->error "Missing child call action"
      openChildCall<-ui "toggle-activity" firstCall openChildRun
      ensure "child call exposes its raw arguments" ("first arguments" `T.isInfixOf` childGroupText openChildCall)
      AH.recordAgentEvent hub peer "tool" (object ["toolCallId" .= ("child-first"::T.Text),"status" .= ("completed"::T.Text),"rawOutput" .= ("first result"::T.Text)])
      AH.recordAgentEvent hub peer "tool" (childTool "child-third" "in_progress" "third arguments")
      streamedChild<-tickConversation conversation openChildCall
      ensure "child updates count calls rather than events and preserve the open run" (actions "toggle-tool-run" streamedChild==[groupValues] &&
        length (actions "toggle-activity" streamedChild)==3 && "3 tool calls" `T.isInfixOf` childGroupText streamedChild &&
        "2 running" `T.isInfixOf` childGroupText streamedChild)
      ensure "child refresh preserves open call input and all streamed raw output" (all (`T.isInfixOf` childGroupText streamedChild) ["first arguments","first result"])
      switchedPrimary<-ui "show" [] streamedChild
      switchedChild<-ui "directory-select" ["0",T.pack (show index)] switchedPrimary
      ensure "child run and call expansion survive switching through Primary" (childGroupText switchedChild==childGroupText streamedChild &&
        actions "toggle-tool-run" switchedChild==[groupValues] && length (actions "toggle-activity" switchedChild)==3)
      collapsedChild<-ui "toggle-tool-run" groupValues switchedChild
      copiedChild<-ui "copy" [] collapsedChild
      ensure "collapsed child run retains all raw call updates for copying" (all (`T.isInfixOf` clipboard copiedChild)
        ["first arguments","first result","second arguments","third arguments"])
      reopenedChild<-ui "toggle-tool-run" groupValues collapsedChild
      ensure "child per-call expansion survives collapsing its run" (childGroupText reopenedChild==childGroupText streamedChild)
      AH.recordAgentEvent hub peer "output" (object ["text" .= ("Reply between child tool runs"::T.Text)])
      AH.recordAgentEvent hub peer "tool" (childTool "child-isolated" "completed" "isolated arguments")
      separatedChild<-tickConversation conversation reopenedChild
      ensure "child reply ends a tool run and leaves the next isolated call visible" (actions "toggle-tool-run" separatedChild==[groupValues] &&
        length (actions "toggle-activity" separatedChild)==4 && "Reply between child tool runs" `T.isInfixOf` childGroupText separatedChild)
      let largeTool=object ["title" .= ("Large tool"::T.Text),"payload" .= T.replicate 600000 "x"]
      AH.recordAgentEvent hub peer "tool" largeTool
      AH.recordAgentEvent hub peer "tool" largeTool
      AH.recordAgentEvent hub peer "output" (object ["text" .= ("latest reply after paged tools"::T.Text)])
      paged<-tickConversation conversation childAgain
      ensure "live child reaches newest reply past history byte cap" ("latest reply after paged tools" `T.isInfixOf` activeText paged)
      _<-AH.cancelAgent hub AH.Human primary >>= right
      afterPrimaryCancel<-tickConversation conversation childAgain
      ensure "primary mailbox cancellation does not cancel selected child" . (==0) =<< readIORef peerCancels
      ensure "primary cancellation preserves child target and draft" (conversationTarget afterPrimaryCancel==AH.agentIdText peer && contents (composerBuffer afterPrimaryCancel)=="child unsent")
      let primaryMetadata=childAgain {agentReplying=True,agentSteering=True,agentContextUsage=Just (42,84),agentSettings=[AgentSetting "model" "Model" "model" "primary-model" [("primary-model","Primary")]]}
      ensure "child capabilities do not inherit primary steering or model settings" (not (conversationSteering primaryMetadata) && not (commandEnabled primaryMetadata (AgentChoose "")) && not (commandEnabled primaryMetadata (AgentSet "model" "primary-model")))
      ensure "child view does not show primary context usage" (case (activeDocument primaryMetadata,activeWindow primaryMetadata) of (Just doc,Just win)->windowPositionText primaryMetadata doc win==" -- "; _->False)
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
      let liveIndex=maybe (error "Live child absent") id (findIndex ((==Just (AH.agentIdText liveChild)) . field "id") (maybe [] id (field "agents" listedNow::Maybe [Value])))
      liveView<-ui "directory-select" ["0",T.pack (show liveIndex)] refreshedDirectory {dialog=Nothing}
      ensure "parent-owned child shows controlling-parent attribution" ("controlling parent" `T.isInfixOf` activeText liveView)
      ensure "live child title/dropdown use advertised child model choices" ("small" `T.isInfixOf` conversationTitle liveView && commandEnabled liveView (AgentChoose ""))
      changedChild<-ui "set-config" ["model","large"] liveView
      configuredChild<-tickUntil (pure . (\d->not (agentReplying d) && any ((=="large").settingCurrent) (childAgentSettings d))) changedChild
      ensure "child configuration does not replace primary provider settings" (agentSettings configuredChild==agentSettings liveView && "large" `T.isInfixOf` conversationTitle configuredChild)
      waitingChild<-ui "send-draft" [] configuredChild {composerBuffer=newBuffer "steer-wait"}
      steerReady<-tickUntil (pure . (==Just (120,1000)) . childAgentContextUsage) waitingChild
      ensure "child reports its own context usage" (conversationContextUsage steerReady==Just (120,1000))
      steeringChild<-ui "steer-draft" [] steerReady {composerBuffer=newBuffer "human direction"}
      ensure "pending child steering keeps the draft until acknowledged" (contents (composerBuffer steeringChild)=="human direction")
      steeredChild<-tickUntil (pure . (\d->not (agentReplying d) && T.null (contents (composerBuffer d)))) steeringChild
      ensure "child steering retains human peer attribution" ("Human (peer message)" `T.isInfixOf` activeText steeredChild && "human direction" `T.isInfixOf` activeText steeredChild)
      humanSent<-ui "send-draft" [] steeredChild {composerBuffer=newBuffer "human followup",composerSelection=Selection 14 14}
      replied<-tickUntil (pure . T.isInfixOf "Human (peer message)" . activeText) humanSent
      ensure "accepted human message clears only its own composer" (T.null (contents (composerBuffer replied)) && maybe False ((=="primary unsent").contents.conversationDraft) (M.lookup "" (conversationViews replied)))
      ensure "child transcript carries no provider keys" (not ("private-main-key" `T.isInfixOf` activeText replied))
      firstQueued<-ui "send-draft" [] replied {composerBuffer=newBuffer "permission"}
      secondQueued<-ui "send-draft" [] firstQueued {composerBuffer=newBuffer "must-not-replay"}
      awaiting<-tickUntil (pure . maybe False (T.isPrefixOf "Agent permission:" . dialogTitle) . dialog) secondQueued
      ensure "human can queue a followup while child is running" (agentQueued awaiting>=1)
      _<-AH.cancelAgent hub AH.Human primary >>= right
      childStillWaiting<-tickConversation conversation awaiting
      ensure "primary cancellation leaves child approval pending" (maybe False (T.isPrefixOf "Agent permission:" . dialogTitle) (dialog childStillWaiting))
      cancelledChild<-ui "cancel" [] childStillWaiting
      afterChildCancel<-tickUntil (\desktop->do entry<-AH.statusAgent hub AH.Human liveChild >>= right; pure (field "status" entry==Just ("idle"::T.Text) && field "queued" entry==Just (0::Int) && dialog desktop==Nothing)) cancelledChild
      privateConfig<-ui "send-draft" [] afterChildCancel {composerBuffer=newBuffer "private-configuration"}
      redactedConfig<-tickUntil (pure . (\d->not (agentReplying d) && null (childAgentSettings d))) privateConfig
      let private="private-main-key":tokens
      publicDirectory<-AH.listAgents hub AH.Human >>= right
      publicCapture<-capture font redactedConfig True >>= right
      ensure "child capability keys never reach directory/title or screen text/image input"
        (all (not . (`T.isInfixOf` T.pack (show publicDirectory))) private && all (not . (`T.isInfixOf` T.pack (show (agentSettings redactedConfig)))) private && all (not . (`T.isInfixOf` T.pack (show publicCapture))) private)
      let finalDraft=redactedConfig {composerBuffer=newBuffer "recover child draft",composerSelection=Selection 4 9}
          recovery=root </> "conversation-views.checkpoint"
      writeCheckpoint recovery finalDraft >>= right
      recovered<-readCheckpoint recovery initial >>= right
      ensure "selected child and active draft survive recovery" (conversationTarget recovered==AH.agentIdText liveChild && snapshotBuffer (composerBuffer recovered)==snapshotBuffer (composerBuffer finalDraft) && composerSelection recovered==Selection 4 9)
      let restoredPrimary=selectConversationView "" "Primary" recovered
      ensure "hidden primary transcript and draft survive recovery" (activeText restoredPrimary==primaryText && contents (composerBuffer restoredPrimary)=="primary unsent")
      ensure "hidden unsent drafts still prevent quiet Exit" (conversationHasDraft recovered && not (null (conversationViews recovered)))
      recoveredShown<-ui "show" [] recovered
      ensure "show after switching does not lose primary transcript" (activeText recoveredShown==primaryText)
      caller<-captureQuestionCaller conversation primary >>= right
      (questionView,_)<-chatToolAs conversation (Just caller) recoveredShown "ask_user" (object ["question" .= ("Choose privately"::T.Text)])
      let privateAnswer=questionView {chatQuestion=fmap (\q->q {questionBuffer=newBuffer "unsent secret answer",questionSelection=Selection 20 20}) (chatQuestion questionView)}
      paintedAnswer<-tickConversation conversation privateAnswer
      hiddenQuestion<-ui "directory-select" ["0",T.pack (show liveIndex)] paintedAnswer
      ensure "hidden primary question answer never becomes readable transcript" (all (maybe True (not . T.isInfixOf "unsent secret answer") . sanitizedBuffer hiddenQuestion) (M.keys (buffers hiddenQuestion)))
      writeCheckpoint recovery hiddenQuestion >>= right
      questionCheckpoint<-readCheckpoint recovery initial >>= right
      ensure "hidden transient answer is omitted from recovery" (all (not . T.isInfixOf "unsent secret answer" . contents . documentBuffer) (M.elems (buffers questionCheckpoint)))
      returnedQuestion<-ui "show" [] hiddenQuestion
      ensure "switching preserves the live primary answer without sending it" (maybe False ((=="unsent secret answer").contents.questionBuffer) (chatQuestion returnedQuestion))
      _<-AH.endAgent hub AH.Human primary >>= right
      _<-tickConversation conversation finished
      invalid<-AH.statusAgent hub (AH.Agent primary) primary
      ensure "ended primary loses orchestration authority" (case invalid of Left _->True; _->False)
    recoveryRecord<-newSessionRecord Nothing ["--",root]
    rememberSession recoveryRecord
    let recoveredPath=root </> "newer-child.checkpoint"
        fakeDriver=AH.AgentDriver root "private-recovery-child" (AH.Capabilities False False False [])
          (\_ ->pure (Right (AH.Capabilities False False False []))) (\_ ->pure (Right Null)) (pure ()) (pure ()) (\_ ->pure (Left "unsupported"))
    recoveredChild<-environment "THC_EDIT_SESSION" (Just (sessionId recoveryRecord)) $ withConversationAt root $ \conversation -> do
      let agents=conversationAgents conversation
          hub=AR.agentHub agents
      child<-AH.registerAgent hub "Recovered child" root fakeDriver >>= right
      AH.recordAgentEvent hub child "output" (object ["text" .= ("older Hub history"::T.Text)])
      let selected=selectConversationView (AH.agentIdText child) "Recovered child" (selectConversationView "" "Primary" (initialDesktop (80,25)))
          bid=maybe (error "missing child view") sourceFixtureBuffer (activeWindow selected)
          newer=selected {buffers=M.adjust (\doc->doc {documentBuffer=newBuffer "newer Desktop child transcript"}) bid (buffers selected),composerBuffer=newBuffer "recovered draft",agentReplying=True,agentQueued=5}
      writeCheckpoint recoveredPath newer >>= right
      AR.activateAgentCheckpoint agents
      AR.checkpointAgents agents >>= right
      pure child
    environment "THC_EDIT_SESSION" (Just (sessionId recoveryRecord)) $ withConversationAt root $ \conversation -> do
      recovered<-readCheckpoint recoveredPath (initialDesktop (80,25)) >>= right
      retained<-tickConversation conversation recovered
      ensure "stale recovered Hub history cannot overwrite newer Desktop transcript" (activeText retained=="newer Desktop child transcript" && contents (composerBuffer retained)=="recovered draft")
      ensure "recovered child projects current status while retaining text" (not (agentReplying retained) && agentQueued retained==0)
      (_,copied)<-conversationEffects conversation (\d _->pure (False,d)) retained [AgentAction "copy" []]
      ensure "recovered transcript remains copyable before reconnect" (clipboard copied=="newer Desktop child transcript")
      let hub=AR.agentHub (conversationAgents conversation)
      AH.updateExternalAgent hub recoveredChild fakeDriver >>= right
      AH.recordAgentEvent hub recoveredChild "output" (object ["text" .= ("new live child output"::T.Text)])
      refreshed<-tickConversation conversation retained
      ensure "live output replaces frozen recovered presentation" ("new live child output" `T.isInfixOf` activeText refreshed)
    corrupt<-newSessionRecord Nothing ["--",root]
    rememberSession corrupt
    checkpoint<-(++".agents.json") <$> checkpointPath (sessionId corrupt)
    writeFile checkpoint "{incomplete"
    environment "THC_EDIT_SESSION" (Just (sessionId corrupt)) $ withConversationAt root $ \conversation -> do
      noticed<-tickConversation conversation (initialDesktop (80,25))
      ensure "agent checkpoint failures reach the status line" ("checkpoint" `T.isInfixOf` status noticed && "retained" `T.isInfixOf` status noticed)
      consumed<-tickConversation conversation noticed {status="ordinary status"}
      ensure "agent runtime notices are shown only once" (status consumed=="ordinary status")
  where
    right=either (error . T.unpack) pure
    ensure label ok=unless ok (error label)

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
