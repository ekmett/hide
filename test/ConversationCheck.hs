{-# LANGUAGE OverloadedStrings #-}
module ConversationCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless, when, forM_)
import Data.Aeson hiding (Number)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime, minutesToTimeZone, addUTCTime)
import Data.List (findIndex)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import THC.Edit.Render (snapshot, snapshotHtml)
import THC.Edit.Buffer
import THC.Edit.Conversation
import THC.Edit.Files
import THC.Edit.Model hiding (prompt)
import THC.Edit.Syntax (Style(..))
import THC.Edit.Terminal (terminalAvailable)

checks :: IO ()
checks = bracket temporary removePathForcibly $ \root ->
  bracket (lookupEnv "XDG_CONFIG_HOME" <* setEnv "XDG_CONFIG_HOME" (root </> "config")) restore $ \_ -> do
    let server=root </> "provider.py"
        logPath=root </> "messages.jsonl"
        source=root </> "Source.hs"
        secondSource=root </> "Second.hs"
        settings=root </> "config" </> "thc-edit"
        environment support=object ["THC_LOG" .= logPath,"THC_SOURCE" .= source,"THC_SECOND" .= secondSource,"THC_RESUME" .= support]
        send runtime action values desktop=snd <$> conversationEffects runtime fallback desktop [AgentAction action values]
        prompt runtime text=send runtime "send" ["0",text,"false","false","false"]
        await runtime label predicate desktop=do
          result<-timeout 8000000 (loop desktop)
          maybe (error ("Conversation timeout: "++label)) pure result
          where loop d=do
                  next<-tickConversation runtime d
                  if predicate next then pure next else threadDelay 10000 >> loop next
        done runtime=await runtime "prompt completion" ((=="Agent: end_turn").status)
        modal runtime=await runtime "approval dialog" (maybe False isApproval . dialog)
        actDialog runtime button desktop=case dialog desktop of
          Nothing -> error "Expected conversation approval"
          Just dg -> let (changed,effects)=submitDialog button dg desktop in snd <$> conversationEffects runtime fallback changed effects
        escape runtime desktop=tickConversation runtime (fst (handleEvent (V.EvKey V.KEsc []) desktop))
        configure runtime support=send runtime "configure" ["0","python3",json [server],json (environment support)]
        logged=readMessages logPath
        response ident=do entries<-logged; pure (findResponse ident entries)
        sourceDocument desktop=case [doc | doc<-M.elems (buffers desktop),fmap filePath (documentFile doc)==Just source] of
          doc:_ -> doc
          [] -> error "Source document missing"
        focusSource desktop=case [w | w<-windows desktop,Just doc<-[M.lookup (bufferId w) (buffers desktop)],fmap filePath (documentFile doc)==Just source] of
          w:_ -> focusWindow (windowId w) desktop
          [] -> error "Source window missing"
    check "token counts use compact rounded SI units"
      (map formatTokenCount [0,999,1000,1234,9999,12345,148000,999500,1234567,2400000000]
        ==["0","999","1k","1.2k","10k","12k","148k","1M","1.2M","2.4G"])
    do
      let noon=UTCTime (fromGregorian 2026 9 30) (secondsToDiffTime (16*3600))
          zone=minutesToTimeZone (-240)
      check "timestamps appear only after five-minute gaps, in local time"
        (pauseLabel Nothing noon zone==Nothing && pauseLabel (Just noon) (addUTCTime 299 noon) zone==Nothing &&
         pauseLabel (Just noon) (addUTCTime 300 noon) zone==Just "Sep 30, 12:05")
      check "timestamps are centered and clipped to narrow windows"
        (T.pack (map fst (renderTimestamp 20 "12:05"))=="       12:05" && length (renderTimestamp 3 "12:05")==3)
      let tag ident=map (\(c,s) -> (c,case s of BubbleText _ out base -> BubbleText ident out base; _ -> s))
          cells=renderReply False 54 True "one"++[('\n',Plain),('\n',Plain)]++renderTimestamp 54 "12:05"++[('\n',Plain)]++tag 1 (renderReply False 54 False "two")
          base=addReadOnly "Conversation" (T.pack (map fst cells)) (initialDesktop (60,18))
          chat=base {buffers=M.map (\doc -> doc {documentHighlight=cells}) (buffers base),composerFocused=True,composerBuffer=newBuffer "draft",composerSelection=Selection 2 2}
          positions ident=[i | (i,(_,BubbleText j _ _))<-zip [0..] cells,j==ident]
          a=head (positions 0); z=last (positions 1)+1
          selectedReply lo hi=modifyActive (\w -> w {selection=Selection lo hi}) chat
          copiedReply lo hi=clipboard (fst (runCommand Copy (selectedReply lo hi)))
      check "single-bubble copies omit speaker names and decoration"
        (copiedReply a (a+3)=="one" && copiedReply (a+1) (a+3)=="ne")
      check "cross-bubble copies label speakers and omit timestamps and furniture"
        (copiedReply 0 (length cells)=="User: one\n\nBot: two" && copiedReply z a=="User: one\n\nBot: two")
      let w=fromMaybe (error "conversation window") (activeWindow chat)
          b=fromMaybe (newBuffer "") (documentBuffer <$> activeDocument chat)
          clickAt p state=let (row,col)=bufferLineColumn b p in fst (handleEvent (V.EvMouseDown (left (bounds w)+1+col) (top (bounds w)+1+row) V.BLeft []) state)
          dragging=clickAt z (clickAt a chat)
          released=fst (handleEvent (V.EvMouseUp 0 0 (Just V.BLeft)) dragging)
          keyCopied=fst (handleEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) released)
      check "dragging across bubbles preserves the draft caret and copies only message text"
        (composerFocused released && composerSelection released==Selection 2 2 && clipboard keyCopied=="User: one\n\nBot: two")
    let reply width outgoing=T.pack . map fst . renderReply False width outgoing
    check "short bubbles occupy one row with outward tails"
      (reply 30 True "hello"==T.replicate 22 " "<>"▐hello▛◤" && reply 30 False "hello"=="◥▜hello▌")
    check "outgoing bubble is black on VGA cyan"
      (('h',BubbleText 0 True Plain) `elem` renderReply False 30 True "hello")
    check "agent prose and code retain their styles inside bubbles"
      (('h',BubbleText 0 False Plain) `elem` renderReply False 30 False "hello" && ('4',BubbleText 0 False Number) `elem` renderReply False 30 False "```haskell\nx = 42\n```")
    forM_ [1,2,5,6,8,30,80] $ \width -> forM_ [False,True] $ \outgoing -> do
      let rendered=reply width outgoing "Wide 界 words é and more words"
      check "bubbles wrap within the window width"
        (all (\row -> displayColumn row (T.length row)<=width || width==1 && displayColumn row (T.length row)==2) (T.lines rendered))
      check "bubble wrapping retains combining marks" (not ("\ń" `T.isInfixOf` rendered))
    writeFile server providerScript
    BS.writeFile source "disk original\n"
    BS.writeFile secondSource "second original\n"
    (secondFile,secondBuffer)<-loadFile secondSource >>= either error pure
    (file,b)<-loadFile source >>= either error pure
    let desktop=insertText "unsaved " (addDocument (Just file) b (addDocument (Just secondFile) secondBuffer (initialDesktop (90,28))) {sideTree=Just (Sidebar root [] 0 0 20 False)})
    withConversation $ \runtime -> do
      configured<-configure runtime ("yes"::T.Text) desktop
      check "configuration saved to isolated XDG directory" =<< doesFileExist (settings </> "agents.json")
      streamed<-prompt runtime "stream" configured >>= done runtime
      entries<-logged
      let initParams=[params | entry<-entries,field "method" entry==Just ("initialize"::T.Text),Just params<-[field "params" entry]]
      check "initialize advertises actual terminal capability" (case initParams of p:_ -> (field "clientCapabilities" p >>= field "terminal")==Just terminalAvailable; [] -> False)
      check "context footer uses latest provider usage and capacity"
        (agentContextUsage streamed==Just (148000,400000) && "37% · 148k/400k" `T.isInfixOf` snapshot streamed)
      check "new session handshake" (any ((==Just ("session/new"::T.Text)).field "method") entries)
      check "conversation has an inline composer" ("Shift+Enter Newline" `T.isInfixOf` snapshot streamed)
      check "conversation renders streamed Markdown" ("Hello" `T.isInfixOf` conversationText streamed && not ("**bold" `T.isInfixOf` conversationText streamed))
      check "conversation omits speaker headings and session banner once chatting"
        (not (any (`elem` T.lines (conversationText streamed)) ["You","Agent"]) && not ("Session:" `T.isInfixOf` conversationText streamed))
      check "conversation preserves Markdown styling" (any ((==BubbleText 1 False Keyword).snd) (conversationHighlight streamed))
      check "tool update merges pending record" (T.count "[completed] Local tool" (conversationText streamed)==1 && not ("[pending] Local tool" `T.isInfixOf` conversationText streamed))
      check "provider settings appear in the title" (conversationTitle streamed=="fixture-model (high) ▼")
      let conversationWindow=fromMaybe (error "conversation window") (activeWindow streamed)
          titleRect=agentTitleRect streamed conversationWindow
          titleMenu=fst (handleEvent (V.EvMouseDown (left titleRect) (top titleRect) V.BLeft []) streamed)
          modelMenu=fst (handleEvent (V.EvKey V.KEnter []) titleMenu)
          chosen=fst (handleEvent (V.EvKey V.KDown []) modelMenu)
          (changing,changeEffects)=handleEvent (V.EvKey V.KEnter []) chosen
      let many=streamed {screenSize=(90,12),agentSettings=[AgentSetting "model" "Model" "model" "0" [(T.pack (show n),"Model "<>T.pack (show n)) | n<-[0..29::Int]]]}
          paged=foldl (\d _ -> fst (handleEvent (V.EvKey V.KDown []) d)) (openAgentChoices "model" many) [1..25::Int]
      check "long provider menus remain on screen and select by absolute index"
        (maybe False (\(r,_) -> top r+height r<12) (contextMenu paged) && snd (handleEvent (V.EvKey V.KEnter []) paged)==[AgentAction "set-config" ["model","25"]])
      check "title click opens settings without moving the window" (contextMenu titleMenu/=Nothing && drag titleMenu==Nothing)
      check "model selection waits for confirmation" (conversationTitle changing==conversationTitle streamed && changeEffects==[AgentAction "set-config" ["model","fixture-other"]])
      changed<-snd <$> conversationEffects runtime fallback changing changeEffects
      updated<-await runtime "model selection" ((=="Conversation settings updated.").status) changed
      check "model acknowledgement updates title and choices" (conversationTitle updated=="fixture-other (high) ▼")
      effortPending<-send runtime "set-config" ["reasoning_effort","ultra"] updated
      effortChanged<-await runtime "effort selection" ((=="Conversation settings updated.").status) effortPending
      check "effort acknowledgement updates title" (conversationTitle effortChanged=="fixture-other (ultra) ▼")
      unavailable<-send runtime "set-config" ["model","not-advertised"] effortChanged
      check "unadvertised options are rejected locally" (status unavailable=="This conversation setting is unavailable." && agentSettings unavailable==agentSettings effortChanged)
      copied<-send runtime "copy" [] effortChanged
      check "copy retains raw Markdown" ("**bold text**" `T.isInfixOf` clipboard copied)
      savedSession<-BS.readFile (settings </> "agent-session.json")
      check "session ID persisted" ((decodeStrict' savedSession >>= field "sessionId")==Just ("fixture-session"::T.Text))
      permission<-prompt runtime "permission" copied >>= modal runtime
      check "permission is not answered before user choice" . (==Nothing) =<< response "permission-1"
      reviewed<-actDialog runtime 1 permission >>= tickConversation runtime
      check "Review opens a read-only request without answering" (dialog reviewed==Nothing && maybe False ((==Just "Agent request").documentLabel) (activeDocument reviewed))
      check "reviewed permission remains unanswered" . (==Nothing) =<< response "permission-1"
      returned<-send runtime "show" [] reviewed >>= modal runtime
      let chooseAllow=returned {dialog=fmap (\dg -> dg {fields=map chooseAllowOption (fields dg)}) (dialog returned)}
      allowed<-actDialog runtime 0 chooseAllow >>= done runtime
      answer<-response "permission-1"
      check "explicit option ID returned" ((answer >>= field "result" >>= field "outcome" >>= field "optionId")==Just ("allow"::T.Text))
      deniedDialog<-prompt runtime "permission" allowed >>= modal runtime
      denied<-escape runtime deniedDialog >>= done runtime
      deniedAnswer<-response "permission-2"
      check "Escape cancels permission" ((deniedAnswer >>= field "result" >>= field "outcome" >>= field "outcome")==Just ("cancelled"::T.Text))
      proposed<-prompt runtime "write" (focusSource denied) >>= modal runtime
      readAnswer<-response "read-3"
      check "ACP read returns unsaved editor contents" ((readAnswer >>= field "result" >>= field "content")==Just ("unsaved disk original\n"::T.Text))
      check "agent write waits for approval" . (=="disk original\n") =<< BS.readFile source
      editReview<-actDialog runtime 1 proposed >>= tickConversation runtime
      check "write Review leaves disk unchanged" . (=="disk original\n") =<< BS.readFile source
      editReturned<-send runtime "show" [] editReview >>= modal runtime
      written<-actDialog runtime 0 editReturned >>= done runtime
      check "approved ACP write saves disk" . (=="agent saved\n") =<< BS.readFile source
      check "approved write uses ordinary Undo" (contents (undo (documentBuffer (sourceDocument written)))=="unsaved disk original\n")
      check "approved write marks buffer saved" (not (dirty (documentBuffer (sourceDocument written))))
      staleDialog<-prompt runtime "write" (focusSource written) >>= modal runtime
      let changed=insertText "later " (focusSource staleDialog)
      rejected<-actDialog runtime 0 changed
      stale<-done runtime rejected
      staleAnswer<-response "write-4"
      check "stale revision rejects agent write" (maybe False hasError staleAnswer && "later " `T.isInfixOf` contents (documentBuffer (sourceDocument stale)))
      check "stale rejection preserves disk" . (=="agent saved\n") =<< BS.readFile source
      twoProposal<-prompt runtime "write-two" (stale {dialog=Nothing}) >>= modal runtime
      let changeSecond doc | fmap filePath (documentFile doc)==Just secondSource = doc {documentBuffer=replaceSelection (Selection 0 0) "user change " (documentBuffer doc)}
                           | otherwise = doc
          otherEdited=twoProposal {buffers=M.map changeSecond (buffers twoProposal)}
      secondProposal<-actDialog runtime 0 otherEdited >>= modal runtime
      twoRejected<-actDialog runtime 0 secondProposal >>= done runtime
      secondAnswer<-response "two-write-b"
      check "approving one file cannot bless stale reads of another" (maybe False hasError secondAnswer)
      check "other file disk survives stale cross-file write" . (=="second original\n") =<< BS.readFile secondSource
      check "other unsaved buffer survives stale cross-file write" (any (\doc -> fmap filePath (documentFile doc)==Just secondSource && contents (documentBuffer doc)=="user change second original\n") (M.elems (buffers twoRejected)))
      let unblocked=twoRejected {dialog=Nothing}
      waiting<-prompt runtime "wait" unblocked >>= await runtime "streamed waiting update" (T.isInfixOf "waiting for cancellation" . conversationText)
      cancelling<-send runtime "cancel" [] waiting
      cancelled<-await runtime "cancel response" ((=="Agent: cancelled").status) cancelling
      check "session cancellation notification sent" . any ((==Just ("session/cancel"::T.Text)).field "method") =<< logged
      let pasteDraft text=fst . handleEvent (V.EvPaste (TE.encodeUtf8 text))
          press key mods=fst . handleEvent (V.EvKey key mods)
          draft=pasteDraft "λ" cancelled
          multiline=pasteDraft "next" (press V.KEnter [V.MShift] draft)
          selected=press (V.KChar 'a') [V.MCtrl] multiline
          copiedDraft=press (V.KChar 'c') [V.MCtrl] selected
          window=fromMaybe (error "conversation window") (activeWindow multiline)
          clickStatus needle desktop=case [r | (r,i,_)<-statusItemRects desktop,needle `T.isInfixOf` fst (statusItems desktop !! i)] of
            r:_ -> handleEvent (V.EvMouseDown (left r) (top r) V.BLeft []) desktop
            [] -> error ("missing status action: "++T.unpack needle)
          applyEvent event desktop=let (next,effects)=handleEvent event desktop in snd <$> conversationEffects runtime fallback next effects
      check "composer is a compact thought bubble without buttons or divider"
        (height (composerRect cancelled window)==1 && height (composerRect multiline window)==2 && width (composerRect multiline window)==12 &&
         "o." `T.isInfixOf` snapshot multiline && not (" Query " `T.isInfixOf` T.intercalate "\n" (init (T.lines (snapshot multiline)))))
      let sized text=composerRect (cancelled {composerBuffer=newBuffer text}) window
          edge r=left r+width r
          available=width (bounds window)-6
          wide=T.replicate 10 "界"<>"\nshort"
          wideDesktop=cancelled {composerBuffer=newBuffer wide,composerSelection=Selection 0 0,composerFocused=True}
          wideRect=composerRect wideDesktop window
          clicked=fst (handleEvent (V.EvMouseDown (left wideRect+6) (top wideRect) V.BLeft []) wideDesktop)
      check "draft width follows the longest display line and keeps its right edge fixed"
        (width (sized wide)==21 && width (sized "\t123456789")==18 &&
         edge (sized wide)==edge (sized "") && edge (sized wide)==left (bounds window)+width (bounds window)-4 &&
         width (sized (T.replicate 200 "x"))==available && width (sized "tiny")==12)
      check "clicking a right-aligned draft locates the Unicode caret"
        (caret (composerSelection clicked)==3)
      let tall=foldl (\d _ -> press V.KEnter [V.MShift] d) multiline [1..15::Int]
          shrunk=press (V.KChar 'z') [V.MCtrl] (press (V.KChar 'z') [V.MCtrl] multiline)
      check "thought bubble caps at twelve rows and scrolls to the caret"
        (height (composerRect tall window)==12 && fst (composerScroll tall window)>0)
      check "undoing newlines shrinks the thought bubble"
        (height (composerRect shrunk window)==1)
      check "composer supports Unicode, newline, and clipboard without changing transcript"
        (contents (composerBuffer multiline)=="λ\nnext" && clipboard copiedDraft=="λ\nnext" && conversationText multiline==conversationText cancelled)
      check "status newline works even with an empty draft"
        (contents (composerBuffer (fst (clickStatus "Newline" cancelled)))=="\n" && null (snd (clickStatus "Newline" cancelled)))
      check "status steering sends the advertised action"
        (snd (clickStatus "Steer" (multiline {agentSteering=True}))==[AgentAction "steer-draft" []])
      submitted<-uncurry (conversationEffects runtime fallback) (clickStatus "Query" (pasteDraft "stream" cancelled)) >>= done runtime . snd
      check "status Query posts draft into transcript and clears input" (T.null (contents (composerBuffer submitted)) && not (agentReplying submitted))
      busyDraft<-prompt runtime "wait" submitted >>= await runtime "composer busy" agentReplying
      preserved<-tickConversation runtime (pasteDraft "stream" busyDraft)
      queued<-applyEvent (V.EvKey V.KEnter []) preserved
      check "Enter queues a query while replying and retains input during ticks"
        (contents (composerBuffer preserved)=="stream" && agentQueued queued==1 && T.null (contents (composerBuffer queued)) && "Enter Queue query" `T.isInfixOf` snapshot queued)
      drained<-uncurry (conversationEffects runtime fallback) (clickStatus "Cancel" queued) >>= done runtime . snd
      check "Cancel stops current response then queued query runs" (agentQueued drained==0 && not (agentReplying drained) && "Enter Query" `T.isInfixOf` snapshot drained)
      steeringWait<-prompt runtime "wait" drained >>= await runtime "steering active" agentReplying
      check "steering hint requires negotiated support" (agentSteering steeringWait && "Ctrl+Enter Steer" `T.isInfixOf` snapshot steeringWait)
      refused<-applyEvent (V.EvKey V.KEnter [V.MCtrl]) (pasteDraft "direction" steeringWait {agentSteering=False})
      check "unsupported steering retains draft" (contents (composerBuffer refused)=="direction")
      steered<-applyEvent (V.EvKey V.KEnter [V.MCtrl]) (pasteDraft "direction" steeringWait) >>= await runtime "steering delivered" (not . agentReplying)
      check "steering uses adapter extension and clears submitted draft" . any ((==Just ("_session/steering"::T.Text)).field "method") =<< logged
      check "steering does not edit source or retain sent draft" (T.null (contents (composerBuffer steered)) && documentBuffer (sourceDocument steered)==documentBuffer (sourceDocument cancelled))
      disconnected<-prompt runtime "disconnect" steered >>= await runtime "provider EOF" ((=="Agent disconnected.").status)
      reconnected<-prompt runtime "stream" disconnected >>= done runtime
      resumed<-send runtime "load" ["0","saved-id"] reconnected >>= await runtime "resume session" ((=="Session saved-id").status)
      check "new session clears stale context usage" (agentContextUsage resumed==Nothing && " -- " `T.isInfixOf` snapshot resumed)
      check "conversation header follows resumed session" ("Session: saved-id" `T.isInfixOf` conversationText resumed)
      check "capability selects session/load" . any ((==Just ("session/load"::T.Text)).field "method") =<< logged
      when terminalAvailable $ do
        pendingTerminal<-prompt runtime "terminal" resumed >>= modal runtime
        check "terminal requires explicit execution approval" (maybe False ((=="Run agent command").dialogTitle) (dialog pendingTerminal))
        completedTerminal<-actDialog runtime 0 pendingTerminal >>= done runtime
        output<-response "terminal-output-1"
        check "ACP terminal output and truncation use real backend" ((output >>= field "result" >>= field "output")==Just ("23456789"::T.Text) && (output >>= field "result" >>= field "truncated")==Just True)
        exit<-response "terminal-wait-1"
        check "ACP waits for real exit status" ((exit >>= field "result" >>= field "exitCode")==Just (9::Int))
        released<-response "terminal-after-release-1"
        check "released terminal cannot be reused" (maybe False hasError released)
        killDialog<-prompt runtime "terminal-kill" completedTerminal >>= modal runtime
        killed<-actDialog runtime 0 killDialog >>= done runtime
        killedExit<-response "terminal-wait-2"
        check "ACP terminal kill completes pending wait" ((killedExit >>= field "result" >>= field "exitCode")==Just (137::Int))
        rejectedTerminal<-prompt runtime "terminal-reject" killed >>= modal runtime
        _<-escape runtime rejectedTerminal >>= done runtime
        refused<-response "terminal-create-3"
        check "rejected terminal request gets error" (maybe False hasError refused)
        check "rejected terminal command never executes" . not =<< doesFileExist (root </> "should-not-exist")
      -- Reconfiguration stops the old provider; the next handshake lacks resume support.
      current<-tickConversation runtime resumed
      unsupported<-configure runtime ("no"::T.Text) current
      countBefore<-length . filter ((==Just ("session/load"::T.Text)).field "method") <$> logged
      gated<-send runtime "load" ["0","forbidden-id"] unsupported >>= await runtime "capability refusal" ((=="This provider cannot resume sessions.").status)
      countAfter<-length . filter ((==Just ("session/load"::T.Text)).field "method") <$> logged
      check "resume is capability-gated" (countBefore==countAfter)
      _<-send runtime "options" [] gated
      pure ()
    -- A new runtime reads the saved provider configuration, without reconfiguring it.
    withConversation $ \runtime -> do
      restored<-send runtime "new" [] desktop >>= await runtime "persisted provider configuration" ((=="Session fixture-session").status)
      _<-send runtime "resume" [] restored
      pure ()
  where
    restore=maybe (unsetEnv "XDG_CONFIG_HOME") (setEnv "XDG_CONFIG_HOME")
    fallback desktop _=pure (False,desktop)
    chooseAllowOption (ListBox label options _) = ListBox label options (fromMaybe (error "Allow choice missing") (findIndex (=="Allow once") options))
    chooseAllowOption other = other
    isApproval dg=case purpose dg of AgentDialog action -> "approval:" `T.isPrefixOf` action; _ -> False

field :: FromJSON a => T.Text -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.: K.fromText key))
json :: ToJSON a => a -> T.Text
json=TE.decodeUtf8 . BL.toStrict . encode
hasError :: Value -> Bool
hasError value=case field "error" value :: Maybe Value of Just _ -> True; _ -> False
findResponse :: T.Text -> [Value] -> Maybe Value
findResponse ident entries=case [entry | entry<-entries,field "id" entry==Just ident,field "method" entry==(Nothing::Maybe T.Text)] of entry:_ -> Just entry; [] -> Nothing
readMessages :: FilePath -> IO [Value]
readMessages path=do
  exists<-doesFileExist path
  if exists then mapMaybe decodeStrict' . B8.lines <$> BS.readFile path else pure []
conversationText :: Desktop -> T.Text
conversationText=T.pack . map fst . conversationHighlight
conversationHighlight :: Desktop -> [(Char,Style)]
conversationHighlight desktop=concat [documentHighlight doc | doc<-M.elems (buffers desktop),documentLabel doc==Just "Conversation"]
check :: String -> Bool -> IO ()
check label success=unless success (error label)
temporary :: IO FilePath
temporary=do
  root<-getTemporaryDirectory
  (path,handle)<-openTempFile root "thc-conversation-check"
  hClose handle
  removeFile path
  createDirectory path
  canonicalizePath path

providerScript :: String
providerScript=unlines
  [ "import json,os,sys"
  , "log=open(os.environ['THC_LOG'],'a',buffering=1)"
  , "sid='fixture-session'; prompt=None; serial=0; terminal_serial=0; scenario=''"
  , "model='fixture-model'; effort='high'"
  , "def settings(): return [{'id':'model','name':'Model','category':'model','type':'select','currentValue':model,'options':[{'value':m,'name':m} for m in ['fixture-model','fixture-other']]},{'id':'reasoning_effort','name':'Reasoning effort','category':'thought_level','type':'select','currentValue':effort,'options':[{'value':e,'name':e} for e in ['high','ultra']]}]"
  , "def send(value):"
  , "  value['jsonrpc']='2.0'; print(json.dumps(value,ensure_ascii=False),flush=True)"
  , "def reply(ident,result): send({'id':ident,'result':result})"
  , "def call(ident,method,params): send({'id':ident,'method':method,'params':dict(params,sessionId=sid)})"
  , "def update(value): send({'method':'session/update','params':{'sessionId':sid,'update':value}})"
  , "def finish(reason='end_turn'):"
  , "  global prompt"
  , "  if prompt is not None: reply(prompt,{'stopReason':reason}); prompt=None"
  , "for line in sys.stdin:"
  , "  msg=json.loads(line); log.write(json.dumps(msg,ensure_ascii=False)+'\\n')"
  , "  method=msg.get('method'); ident=msg.get('id'); params=msg.get('params',{})"
  , "  if method=='initialize': reply(ident,{'protocolVersion':1,'agentCapabilities':{'loadSession':os.environ['THC_RESUME']=='yes'},'_meta':{'steering':{'supported':os.environ['THC_RESUME']=='yes'}}})"
  , "  elif method=='session/new': sid='fixture-session'; reply(ident,{'sessionId':sid,'configOptions':settings()})"
  , "  elif method=='session/load': sid=params['sessionId']; reply(ident,{})"
  , "  elif method=='session/set_config_option':"
  , "    if params['configId']=='model': model=params['value']"
  , "    else: effort=params['value']"
  , "    reply(ident,{'configOptions':settings()})"
  , "  elif method=='session/cancel': finish('cancelled')"
  , "  elif method=='_session/steering': reply(ident,{'outcome':'injected'}); finish()"
  , "  elif method=='session/prompt':"
  , "    prompt=ident; scenario=params['prompt'][0]['text'].splitlines()[0]"
  , "    if scenario=='stream':"
  , "      update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'# Hello\\n\\n**bold'}})"
  , "      update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':' text**\\n'}})"
  , "      update({'sessionUpdate':'tool_call','toolCallId':'fixture-tool','title':'Local tool','status':'pending'})"
  , "      update({'sessionUpdate':'tool_call_update','toolCallId':'fixture-tool','status':'completed'})"
  , "      update({'sessionUpdate':'usage_update','used':300000,'size':400000})"
  , "      update({'sessionUpdate':'usage_update','used':148000,'size':400000})"
  , "      update({'sessionUpdate':'usage_update','used':-1,'size':0})"
  , "      finish()"
  , "    elif scenario=='permission':"
  , "      serial+=1; call('permission-'+str(serial),'session/request_permission',{'toolCall':{'title':'Fixture action'},'options':[{'optionId':'allow','name':'Allow once','kind':'allow_once'},{'optionId':'deny','name':'Reject','kind':'reject_once'}]})"
  , "    elif scenario=='write':"
  , "      serial+=1; call('read-'+str(serial),'fs/read_text_file',{'path':os.environ['THC_SOURCE']})"
  , "    elif scenario=='write-two': call('two-read-a','fs/read_text_file',{'path':os.environ['THC_SOURCE']})"
  , "    elif scenario=='wait': update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'waiting for cancellation'}})"
  , "    elif scenario=='disconnect': sys.exit(0)"
  , "    elif scenario.startswith('terminal'):"
  , "      terminal_serial+=1"
  , "      command=\"printf 'λ0123456789'; sleep 0.1; exit 9\" if scenario=='terminal' else ('touch should-not-exist' if scenario=='terminal-reject' else 'sleep 30')"
  , "      call('terminal-create-'+str(terminal_serial),'terminal/create',{'command':'/bin/sh','args':['-c',command],'outputByteLimit':8})"
  , "  elif method is None and isinstance(ident,str):"
  , "    if ident=='two-read-a': call('two-read-b','fs/read_text_file',{'path':os.environ['THC_SECOND']})"
  , "    elif ident=='two-read-b': call('two-write-a','fs/write_text_file',{'path':os.environ['THC_SOURCE'],'content':'first approved\\n'})"
  , "    elif ident=='two-write-a': call('two-write-b','fs/write_text_file',{'path':os.environ['THC_SECOND'],'content':'should be rejected\\n'})"
  , "    elif ident=='two-write-b': finish()"
  , "    elif ident.startswith('permission-') or ident.startswith('write-'): finish()"
  , "    elif ident.startswith('read-'): call('write-'+str(serial),'fs/write_text_file',{'path':os.environ['THC_SOURCE'],'content':'agent saved\\n'})"
  , "    elif ident.startswith('terminal-create-'):"
  , "      if 'error' in msg: finish()"
  , "      else:"
  , "        tid=msg['result']['terminalId']; call('terminal-wait-'+str(terminal_serial),'terminal/wait_for_exit',{'terminalId':tid})"
  , "        if scenario=='terminal-kill': call('terminal-kill-'+str(terminal_serial),'terminal/kill',{'terminalId':tid})"
  , "    elif ident.startswith('terminal-wait-'): call('terminal-output-'+str(terminal_serial),'terminal/output',{'terminalId':tid})"
  , "    elif ident.startswith('terminal-output-'): call('terminal-release-'+str(terminal_serial),'terminal/release',{'terminalId':tid})"
  , "    elif ident.startswith('terminal-release-'): call('terminal-after-release-'+str(terminal_serial),'terminal/output',{'terminalId':tid})"
  , "    elif ident.startswith('terminal-after-release-'): finish()"
  ]
