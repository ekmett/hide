{-# LANGUAGE CPP, OverloadedStrings #-}
module ConversationCheck (checks) where

import Control.Concurrent (threadDelay)

import Control.Concurrent.Async (Async, withAsync, cancel, poll, wait)
import Control.Exception (bracket, evaluate)
import Control.Monad (unless, when, forM_, foldM)
import Data.Aeson hiding (Number)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime, minutesToTimeZone, addUTCTime)
import Data.List (findIndex)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, hFlush, openTempFile)
#ifndef mingw32_HOST_OS
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, isEmptyMVar)
import System.Process (withCreateProcess, proc, CreateProcess(..), StdStream(..), waitForProcess)
import System.Exit (ExitCode(..))
import System.Posix.Files (createNamedPipe)
import qualified System.Posix.IO as Posix
#endif
import System.Info (os)
import System.Timeout (timeout)
import THC.Edit.Render (snapshot, snapshotHtml)
import THC.Edit.Buffer
import qualified THC.Edit.App as App
import THC.Edit.GuestAccess (guestCommandAllowed, protectedBuffer)
import THC.Edit.Conversation
import qualified THC.Edit.AgentHub as AH
import qualified THC.Edit.AgentRuntime as AR
import System.Mem.StableName (makeStableName)
import THC.Edit.Files
import THC.Edit.Model hiding (prompt)
import THC.Edit.Markdown (renderMarkdown)
import THC.Edit.Syntax (Style(..))
import THC.Edit.Terminal (terminalAvailable)
import THC.Edit.Session (checkpointPath)

checks :: IO ()
checks = bracket temporary removePathForcibly $ \root ->
  bracket (lookupEnv "XDG_CONFIG_HOME" <* setEnv "XDG_CONFIG_HOME" (root </> "config")) restore $ \_ ->
  bracket (lookupEnv "XDG_DATA_HOME" <* setEnv "XDG_DATA_HOME" (root </> "data")) (restoreEnvironment "XDG_DATA_HOME") $ \_ ->
  bracket (lookupEnv "THC_EDIT_SESSION" <* unsetEnv "THC_EDIT_SESSION") (restoreEnvironment "THC_EDIT_SESSION") $ \_ -> do
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
      (('h',BubbleText 0 False Plain) `elem` renderReply False 30 False "hello" && ('4',BubbleText 0 False (CodeStyle False Number)) `elem` renderReply False 30 False "```haskell\nx = 42\n```")
    forM_ [False,True] $ \outgoing -> do
      let sourceText="```haskell\nx = 42\n```"
          cells=renderReply False 40 outgoing sourceText
          firstRow=takeWhile ((/='\n').fst) cells
          codeCell (_,BubbleText _ _ (CodeStyle _ _))=True
          codeCell _=False
          copied=[c | (c,BubbleText _ _ _)<-cells]
      check "leading code panels leave the bubble top edge clear"
        (not (any codeCell firstRow) && any codeCell cells)
      check "leading code panel spacing is decoration, not copied text"
        (copied==map fst (renderMarkdown 35 sourceText))
    forM_ [1,2,5,6,8,30,80] $ \width -> forM_ [False,True] $ \outgoing -> do
      let rendered=reply width outgoing "Wide 界 words é and more words"
      check "bubbles wrap within the window width"
        (all (\row -> displayColumn row (T.length row)<=width || width==1 && displayColumn row (T.length row)==2) (T.lines rendered))
      check "bubble wrapping retains combining marks" (not ("\ń" `T.isInfixOf` rendered))
    let permissionBase=initialDesktop (80,25)
        permissionMenu=[title | ("Options",_,items)<-menus,MenuItem title _ AgentPermissions<-items]
        chooser=Dialog "Agent Permissions" (PermissionDialog "settings") [ListBox "Tool" ["tool"<>T.pack (show n) | n<-[0..39::Int]] 0] 0 ["Edit","Close"] []
        chooseLast=foldl (\d _->fst (handleEvent (V.EvKey V.KDown []) d)) permissionBase {dialog=Just chooser} [1..39::Int]
        approval=Dialog "Allow tool?" (PermissionDialog "approve:fixture") [] 0 ["Allow once","Deny"] ["Tool: editor_file"]
    check "Options menu uses exact Agent Permissions label" (permissionMenu==["Agent Permissions"] && snd (runCommand AgentPermissions permissionBase)==[PermissionAction "show" []])
    check "permission tool chooser paginates all entries" ("tool39" `T.isInfixOf` snapshot chooseLast && snd (handleEvent (V.EvKey V.KEnter []) chooseLast)==[PermissionAction "settings" ["0","39"]])
    check "Escape explicitly denies permission requests" (snd (handleEvent (V.EvKey V.KEsc []) permissionBase {dialog=Just approval})==[PermissionAction "approve:fixture" ["1"]])
    let modeDialog=Dialog "Agent Permissions" (PermissionDialog "set:editor_file") [Radio "Permission" ["Enable","Prompt","Disable"] 2] 0 ["Save","Back"] []
    check "permission mode submission includes selected radio" (snd (submitDialog 0 modeDialog permissionBase {dialog=Just modeDialog})==[PermissionAction "set:editor_file" ["0","2"]])
    let draftBase=addReadOnly "Conversation" "" (initialDesktop (90,30))
        savedDraft=draftBase {composerBuffer=newBuffer "existing draft",composerSelection=Selection 4 4}
        isLeft (Left _)=True
        isLeft _=False
        clickAction runtime action desktop=case [(a,values) | (a,_,name,values)<-chatActions desktop,name==action] of
          (offset,_):_ -> do
            let win=fromMaybe (error "question window") (activeWindow desktop)
                buffer=maybe (newBuffer "") documentBuffer (activeDocument desktop)
                (row,column)=bufferLineColumn buffer offset
                (changed,effects)=handleEvent (V.EvMouseDown (left (bounds win)+1+column) (top (bounds win)+1+row-scrollRow win) V.BLeft []) desktop
            next<-snd <$> conversationEffects runtime fallback changed effects
            tickConversation runtime next
          _ -> error ("Missing inline action "++T.unpack action)
    withConversation $ \runtime -> do
      let longReply=T.unwords (replicate 90 "window-width")
          rawShell="printf '%s\\n' 'literal λ'\n\tprintf 'tail  '  \n"
          question=longReply<>"\n\n```sh\n"<>rawShell<>"```"
      (initial,_)<-chatTool runtime (initialDesktop (80,25)) "ask_user" (object ["question" .= question])
      forM_ [150,32,120] $ \columns -> do
        let resized=modifyActive (\w->w {bounds=Rect 0 1 columns 23}) initial {screenSize=(columns,25)}
        (_,reflowed)<-conversationEffects runtime fallback resized []
        let doc=fromMaybe (error "missing reflowed conversation") (activeDocument reflowed)
            text=contents (documentBuffer doc)
            bubbleRows=[T.pack [c | (c,BubbleText _ _ _) <- row] | row<-splitStyled (documentHighlight doc)]
            nonempty=filter (not . T.null) bubbleRows
            blockRows=[(i,c) | (i,(c,BubbleText _ _ (CodeStyle True _)))<-zip [0..] (documentHighlight doc)]
            blocks=documentShellBlocks doc
        check "chat reflows to the resized window before the next timer tick"
          (maximum (0:map T.length nonempty)>columns-22 && all ((<=columns-2).T.length) nonempty)
        check "reflow preserves the whole shell source and maps its decorated cells"
          (map (\(_,_,dialect,raw)->(dialect,raw)) blocks==[("sh",rawShell)] &&
           all (\(i,_)->any (\(start,end,_,_)->i>=start && i<end) blocks) blockRows)
        let selected=modifyActive (\w->w {selection=Selection 0 (T.length text)}) reflowed {composerFocused=False}
            copied=fst (runCommand Copy selected)
        check "copy after chat reflow still excludes bubble furniture"
          ("window-width" `T.isInfixOf` clipboard copied && not ("┌" `T.isInfixOf` clipboard copied) && not ("```" `T.isInfixOf` clipboard copied))
        stable<-tickConversation runtime reflowed
        check "timer tick keeps immediately reflowed layout stable" ((documentBuffer <$> activeDocument stable)==Just (documentBuffer doc))
    forM_ [32,120,150] $ \columns -> do
      let outgoing=renderReply False columns True (T.unwords (replicate 90 "window-width"))
          incoming=renderReply False columns False (T.unwords (replicate 90 "window-width"))
          firstRow=takeWhile ((/='\n').fst)
      check "wide user bubbles anchor on the right and replies on the left"
        (length (firstRow outgoing)==columns && maybe False (\(_,style)->case style of BubbleText _ False _->False; _->True) (listToMaybe incoming) &&
         maximum (map (length . filter (\(_,style)->case style of BubbleText{}->True; _->False)) (splitStyled incoming))>columns-20)
    withConversation $ \runtime -> do
      agents <- send runtime "directory" [] savedDraft
      check "Agents directory exposes an explicit reconnect action"
        (maybe False (elem "Reconnect" . buttons) (dialog agents))
      let recovered=addReadOnly "Conversation" "Recovered user and agent transcript" savedDraft
      idle<-tickConversation runtime recovered
      resized<-tickConversation runtime idle {screenSize=(100,35)}
      check "idle fresh conversation runtime preserves recovered transcript and draft"
        (buffers resized==buffers recovered && composerBuffer resized==composerBuffer recovered && composerSelection resized==composerSelection recovered)
      shown<-send runtime "show" [] resized
      check "opening a recovered conversation preserves its transcript and draft"
        (buffers shown==buffers recovered && composerBuffer shown==composerBuffer recovered && composerSelection shown==composerSelection recovered && composerFocused shown)
    withConversation $ \runtime -> do
      (asked,answer)<-chatTool runtime savedDraft "ask_user" (object ["question" .= ("Pick a direction"::T.Text),"choices" .= (["Left","Right"]::[T.Text])])
      check "ask_user renders inline choices and custom entry without a modal" (dialog asked==Nothing && chatQuestion asked/=Nothing && all (`T.isInfixOf` conversationText asked) ["Pick a direction","Left","Right","Other:","Submit answer","Cancel"])
      check "question footer describes answer actions" ("Enter Answer" `T.isInfixOf` snapshot asked && not ("Session: not connected" `T.isInfixOf` conversationText asked))
      check "ask_user preserves the existing draft and caret" (composerBuffer asked==composerBuffer savedDraft && composerSelection asked==composerSelection savedDraft)
      (duplicate,refused)<-chatTool runtime asked "ask_user" (object ["question" .= ("Another?"::T.Text)])
      check "only one human question can wait" . (&& (duplicate==asked)) . isLeft =<< refused
      withAsync answer $ \pendingAnswer -> do
        threadDelay 10000
        pendingResult<-poll pendingAnswer
        check "question registration returns while the human answer waits" (case pendingResult of Nothing->True; _->False)
        selected<-clickAction runtime "question-choice" asked
        check "choice click waits for explicit submit" (maybe False ((==Just 0).questionChoice) (chatQuestion selected))
        submitted<-clickAction runtime "question-submit" selected
        choiceReply<-wait pendingAnswer
        check "choice response reaches waiting tool" (case choiceReply of Right value->field "answer" value==Just ("Left"::T.Text) && field "custom" value==Just False; _->False)
        check "answer removes the inline form and preserves draft" (chatQuestion submitted==Nothing && composerBuffer submitted==composerBuffer savedDraft && composerSelection submitted==composerSelection savedDraft)
      (custom,customReply)<-chatTool runtime savedDraft "ask_user" (object ["question" .= ("Your answer?"::T.Text)])
      let typed=foldl (\desktop c->fst (handleEvent (V.EvKey (V.KChar c) []) desktop)) custom ("custom λ"::String)
          (sending,effects)=handleEvent (V.EvKey V.KEnter []) typed
      sent<-snd <$> conversationEffects runtime fallback sending effects
      result<-customReply
      check "free text answers preserve Unicode and the ordinary draft" (case result of Right value->field "answer" value==Just ("custom λ"::T.Text) && composerBuffer sent==composerBuffer savedDraft; _->False)
      (cancelledQuestion,cancelledReply)<-chatTool runtime savedDraft "ask_user" (object ["question" .= ("Cancel me"::T.Text)])
      cancelledDesktop<-clickAction runtime "question-cancel" cancelledQuestion
      check "inline Cancel resolves the pending request" . (&& (chatQuestion cancelledDesktop==Nothing)) . isLeft =<< cancelledReply
      (disconnected,disconnectedReply)<-chatTool runtime savedDraft "ask_user" (object ["question" .= ("Requester leaves"::T.Text)])
      withAsync disconnectedReply $ \worker->threadDelay 10000 >> cancel worker
      cleaned<-tickConversation runtime disconnected
      check "requester cancellation clears the pending question on tick" (chatQuestion cleaned==Nothing)
      let narrow=addReadOnly "Conversation" "" (initialDesktop (40,12))
      (manyChoices,_)<-chatTool runtime narrow "ask_user" (object ["question" .= ("Choose"::T.Text),"choices" .= (T.replicate 100 "z":["Option "<>T.pack (show n) | n<-[1..11::Int]])])
      visibleChoice<-tickConversation runtime (fst (handleEvent (V.EvKey V.KDown []) manyChoices))
      let active=fromMaybe (error "question window") (activeWindow visibleChoice)
          document=fromMaybe (error "question document") (activeDocument visibleChoice)
          choiceRows=[fst (bufferLineColumn (documentBuffer document) a) | (a,_,action,values)<-chatActions visibleChoice,action=="question-choice",last values=="0"]
      check "keyboard choices stay visible in a small conversation window" (case choiceRows of row:_->row>=scrollRow active && row<scrollRow active+windowContentRows visibleChoice document active; _->False)
      check "long choice labels wrap without being discarded" (T.count "z" (conversationText visibleChoice)==100)
      smallCancelled<-clickAction runtime "question-cancel" =<< tickConversation runtime (fst (handleEvent (V.EvKey V.KUp []) visibleChoice))
      check "small-window custom input leaves Cancel reachable" (chatQuestion smallCancelled==Nothing)
      (_,invalid)<-chatTool runtime savedDraft "ask_user" (object ["question" .= ("Unsupported"::T.Text),"allowMultiple" .= True])
      check "unsupported multi-select is explicit" . isLeft =<< invalid
    closingReply<-withConversation $ \runtime->snd <$> chatTool runtime savedDraft "ask_user" (object ["question" .= ("Session closes"::T.Text)])
    check "session shutdown resolves a waiting question" . isLeft =<< closingReply
    BS.writeFile server (TE.encodeUtf8 (T.pack providerScript))
    BS.writeFile source "disk original\n"
    BS.writeFile secondSource "second original\n"
    (secondFile,secondBuffer)<-loadFile secondSource >>= either error pure
    (file,b)<-loadFile source >>= either error pure
    let desktop=insertText "unsaved " (addDocument (Just file) b (addDocument (Just secondFile) secondBuffer (initialDesktop (90,28))) {sideTree=Just (Sidebar root [] 0 0 20 False)})
#ifndef mingw32_HOST_OS
    -- Hold an actual filesystem read open while the provider sends more work.
    -- The desktop must keep ticking, and a later write cannot bless user edits
    -- made while its request was waiting behind that read.
    let pipe=root </> "slow-source"
    createNamedPipe pipe 0o600
    withHeldRead server pipe $ \opened release writer -> withConversation $ \runtime -> do
        configured<-configure runtime ("yes"::T.Text) desktop
        started<-prompt runtime "slow-files" configured
        responsive<-await runtime "provider update behind held file read"
          (T.isInfixOf "file requests sent" . conversationText) started
        waitForReader opened
        let edited=insertText "during read " (focusSource responsive)
        composing<-send runtime "show" [] edited
        let typed=fst (handleEvent (V.EvPaste "draft stays responsive") composing {composerFocused=True})
        ticked<-timeout 1000000 (tickConversation runtime typed)
        progressed<-maybe (error "Desktop tick blocked on ACP file read") pure ticked
        check "desktop input progresses while ACP filesystem read is held"
          (contents (composerBuffer progressed)=="draft stays responsive" && "during read " `T.isInfixOf` contents (documentBuffer (sourceDocument progressed)))
        check "file results are not delivered out of request order" . (==Nothing) =<< response "slow-write"
        check "held read has not been released" =<< isEmptyMVar release
        putMVar release ()
        wait writer
        completed<-await runtime "held file prompt completion" ((=="Agent: end_turn").status) progressed
        readResult<-response "slow-read"
        writeResult<-response "slow-write"
        check "held read replies after release" ((readResult >>= field "result" >>= field "content")==Just ("held read\n"::T.Text))
        check "captured write rejects intervening buffer edits without approval"
          (maybe False hasError writeResult && dialog completed==Nothing)
        check "stale asynchronous write leaves disk intact" . (=="disk original\n") =<< BS.readFile source
    forM_ ["slow-replaced","slow-private"] $ \scenario -> do
      withHeldRead server pipe $ \held releaseCapture writer -> withConversation $ \runtime -> do
          configured<-configure runtime ("yes"::T.Text) desktop
          started<-prompt runtime scenario configured
          waiting<-await runtime "prepared read behind FIFO head" (T.isInfixOf "guarded requests sent" . conversationText) started
          waitForReader held
          let guarded=if scenario=="slow-private" then waiting {guestPrivatePaths=source:guestPrivatePaths waiting}
                else waiting {buffers=M.map (\doc->if fmap filePath (documentFile doc)==Just source
                  then doc {documentBuffer=(newBuffer "same revision replacement") {revision=revision (documentBuffer doc)}} else doc) (buffers waiting)}
          check "prepared response stays queued until FIFO head completes" . (==Nothing) =<< response (scenario<>"-source")
          putMVar releaseCapture ()
          wait writer
          completed<-await runtime "prepared response revalidation" ((=="Agent: end_turn").status) guarded
          answer<-response (scenario<>"-source")
          check "prepared reads reject equal-revision replacement or newly private source"
            (maybe False hasError answer && dialog completed==Nothing)
    cancelOpened<-newEmptyMVar
    cancelRelease<-newEmptyMVar
    withAsync (bracket (Posix.openFd pipe Posix.ReadWrite Posix.defaultFileFlags >>= \fd -> Posix.setFdOption fd Posix.CloseOnExec True >> Posix.fdToHandle fd) hClose $ \_ -> do
      putMVar cancelOpened ()
      takeMVar cancelRelease) $ \writer -> do
        closed<-timeout 3000000 $ withConversation $ \runtime -> do
          takeMVar cancelOpened
          configured<-configure runtime ("yes"::T.Text) desktop
          started<-prompt runtime "slow-cancel" configured
          ready<-await runtime "held cancellation request" (T.isInfixOf "cancel file sent" . conversationText) started
          result<-timeout 1000000 (send runtime "cancel" [] ready)
          cancelled<-maybe (error "Cancellation joined blocked ACP read") pure result
          _<-await runtime "held read cancellation acknowledgement" ((=="Agent: cancelled").status) cancelled
          answer<-timeout 1000000 $ let loop=do value<-response "cancel-read"; maybe (threadDelay 1000 >> loop) pure value in loop
          check "cancellation responds to outstanding capture without approval" (maybe False hasError answer)
          shutdown<-prompt runtime "slow-shutdown" cancelled
          _<-await runtime "held shutdown request" (T.isInfixOf "shutdown file sent" . conversationText) shutdown
          pure ()
        check "conversation shutdown owns and stops cancelled file workers" (closed==Just ())
        putMVar cancelRelease ()
        wait writer
    let runSettings=settings </> "run.json"
    createNamedPipe runSettings 0o600
    settingsOpened<-newEmptyMVar
    settingsRelease<-newEmptyMVar
    withAsync (bracket (Posix.openFd runSettings Posix.ReadWrite Posix.defaultFileFlags >>= \fd -> Posix.setFdOption fd Posix.CloseOnExec True >> Posix.fdToHandle fd) hClose $ \handle -> do
      putMVar settingsOpened ()
      takeMVar settingsRelease
      BS.hPut handle "{\"toolchain\":\"GHC\"}") $ \writer -> withConversation $ \runtime -> do
        takeMVar settingsOpened
        ready<-timeout 1000000 (tickConversation runtime desktop)
        responsive<-maybe (error "Idle tick blocked on build settings read") pure ready
        next<-timeout 1000000 (tickConversation runtime responsive)
        check "repeated ticks do not join or duplicate blocked settings discovery" (maybe False (const True) next)
        putMVar settingsRelease ()
        wait writer
        _<-await runtime "asynchronous persisted toolchain" ((==Just GHC).toolchain) responsive
        pure ()
    removeFile runSettings
    -- Context reads happen before enqueueing a prompt. Holding one must leave
    -- the draft editable, and cancelling it must never send the stale prompt.
    withConversation $ \runtime -> do
      configured<-configure runtime ("yes"::T.Text) desktop
      connected<-prompt runtime "stream" configured >>= done runtime
      let contextPath=root </> "thc.toml"
      createNamedPipe contextPath 0o600
      contextOpened<-newEmptyMVar
      contextRelease<-newEmptyMVar
      withAsync (bracket (Posix.openFd contextPath Posix.ReadWrite Posix.defaultFileFlags >>= \fd -> Posix.setFdOption fd Posix.CloseOnExec True >> Posix.fdToHandle fd) hClose $ \handle -> do
        putMVar contextOpened ()
        takeMVar contextRelease
        BS.hPut handle "[editor.agent]\ncontext = 'late guidance'\n") $ \writer -> do
          takeMVar contextOpened
          fast<-timeout 1000000 (send runtime "send-draft" [] connected {composerBuffer=newBuffer "cancel before send"})
          preparing<-maybe (error "Context preparation blocked send/input") pure fast
          check "draft remains while context is preparing" (contents (composerBuffer preparing)=="cancel before send")
          next<-timeout 1000000 (tickConversation runtime preparing)
          responsive<-maybe (error "Context preparation blocked tick") pure next
          let edited=responsive {composerBuffer=newBuffer "newer human draft"}
          cancelled<-send runtime "cancel" [] edited
          check "cancelling context preparation preserves newer draft" (contents (composerBuffer cancelled)=="newer human draft")
          putMVar contextRelease ()
          wait writer
          removeFile contextPath
          settled<-foldM (\state _ -> threadDelay 1000 >> tickConversation runtime state) cancelled [1..20::Int]
          entries<-logged
          check "cancelled context result never sends a prompt"
            (not (any (T.isInfixOf "cancel before send" . json) entries) && contents (composerBuffer settled)=="newer human draft")
#endif
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
      check "tool activity starts collapsed without raw arguments" (not ("rawInput" `T.isInfixOf` conversationText streamed) && "▸" `T.isInfixOf` conversationText streamed)
      let activityAction=case [values | (_,_,name,values)<-chatActions streamed,name=="toggle-activity"] of values:_->values; _->error "missing activity action"
      expanded<-clickAction runtime "toggle-activity" streamed
      check "expanded activity retains original request and response JSON" (all (`T.isInfixOf` conversationText expanded) ["rawInput","rawOutput","original argument","exact response"])
      let selectedActivity=modifyActive (\w->w {selection=Selection 0 (maybe 0 (bufferLength.documentBuffer) (activeDocument expanded))}) expanded
          selectedTextOnly=clipboard (fst (runCommand Copy selectedActivity))
      check "conversation copies omit activity chevrons and raw JSON" (not ("rawInput" `T.isInfixOf` selectedTextOnly) && not ("▾" `T.isInfixOf` selectedTextOnly) && "Hello" `T.isInfixOf` selectedTextOnly)
      collapsed<-send runtime "toggle-activity" activityAction expanded
      check "activity collapses without changing prose" (conversationText collapsed==conversationText streamed)
      grouped<-prompt runtime "tool-run" collapsed >>= done runtime
      let groupActions d=[values | (_,_,name,values)<-chatActions d,name=="toggle-tool-run"]
          singleActions d=[values | (_,_,name,values)<-chatActions d,name=="toggle-activity"]
      check "consecutive calls collapse to one double chevron with visible failures"
        (length (groupActions grouped)==1 && "▸▸ 3 tool calls · 1 failed" `T.isInfixOf` conversationText grouped &&
         length (singleActions grouped)==2 && not ("run argument" `T.isInfixOf` conversationText grouped))
      openedGroup<-clickAction runtime "toggle-tool-run" grouped
      check "expanding a run reveals tightly stacked individual calls"
        (length (singleActions openedGroup)==5 && "▾▾ 3 tool calls" `T.isInfixOf` conversationText openedGroup &&
         "[completed] Read files\n  ▸ [completed] Run tests\n  ▸ [failed] Check output" `T.isInfixOf` conversationText openedGroup)
      let firstRunCall=case drop 1 (singleActions openedGroup) of values:_->values; _->error "missing grouped call"
          runAction d=case groupActions d of values:_->values; _->error "missing run toggle"
      detailGroup<-send runtime "toggle-activity" firstRunCall openedGroup
      check "group members still expose exact request and reply details"
        (all (`T.isInfixOf` conversationText detailGroup) ["run argument","run result"])
      foldedGroup<-send runtime "toggle-tool-run" (runAction detailGroup) detailGroup
      check "folding a run hides every member and its expanded JSON"
        (conversationText foldedGroup==conversationText grouped)
      reopenedGroup<-send runtime "toggle-tool-run" (runAction foldedGroup) foldedGroup
      check "unfolding restores individual detail state"
        (conversationText reopenedGroup==conversationText detailGroup)
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
         "•." `T.isInfixOf` snapshot multiline && not (" Query " `T.isInfixOf` T.intercalate "\n" (init (T.lines (snapshot multiline)))))
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
        (snd (clickStatus "Steer" (multiline {agentSteering=True,agentReplying=True}))==[AgentAction "steer-draft" []])
      submitted<-uncurry (conversationEffects runtime fallback) (clickStatus "Query" (pasteDraft "stream" cancelled)) >>= done runtime . snd
      check "status Query posts draft into transcript and clears input" (T.null (contents (composerBuffer submitted)) && not (agentReplying submitted))
      busyDraft<-prompt runtime "wait" submitted >>= await runtime "composer busy" ((=="Agent is replying...").status)
      preserved<-tickConversation runtime (pasteDraft "stream" busyDraft)
      queued<-applyEvent (V.EvKey V.KEnter []) preserved
      check "Enter queues a query while replying and retains input during ticks"
        (contents (composerBuffer preserved)=="stream" && agentQueued queued==1 && T.null (contents (composerBuffer queued)) && "Enter Queue query" `T.isInfixOf` snapshot queued)
      drained<-uncurry (conversationEffects runtime fallback) (clickStatus "Cancel" queued) >>= done runtime . snd
      check "Cancel stops current response then queued query runs" (agentQueued drained==0 && not (agentReplying drained) && "Enter Query" `T.isInfixOf` snapshot drained)
      steeringWait<-prompt runtime "wait" drained >>= await runtime "steering active" ((=="Agent is replying...").status)
      check "steering hint requires negotiated support" (agentSteering steeringWait && "Ctrl+Enter Steer" `T.isInfixOf` snapshot steeringWait)
      refused<-applyEvent (V.EvKey V.KEnter [V.MCtrl]) (pasteDraft "direction" steeringWait {agentSteering=False})
      check "unsupported steering retains draft" (contents (composerBuffer refused)=="direction")
      steered<-applyEvent (V.EvKey V.KEnter [V.MCtrl]) (pasteDraft "direction" steeringWait) >>= await runtime "steering delivered" (not . agentReplying)
      check "steering uses adapter extension and clears submitted draft" . any ((==Just ("_session/steering"::T.Text)).field "method") =<< logged
      check "steering does not edit source or retain sent draft" (T.null (contents (composerBuffer steered)) && documentBuffer (sourceDocument steered)==documentBuffer (sourceDocument cancelled))
      idleRace<-prompt runtime "wait" steered >>= await runtime "idle race active" ((=="Agent is replying...").status)
      rejectedSteer<-send runtime "steer-draft" [] idleRace {composerBuffer=newBuffer "idle-race"} >>= await runtime "idle race response" (not . agentReplying)
      check "primary idle race leaves steering draft unsent" (contents (composerBuffer rejectedSteer)=="idle-race")
      legacyWait<-prompt runtime "wait" rejectedSteer >>= await runtime "legacy steer active" ((=="Agent is replying...").status)
      legacy<-send runtime "steer-draft" [] legacyWait {composerBuffer=newBuffer "legacy-steer"} >>= await runtime "legacy steering retires provider" (T.isInfixOf "provider stopped" . status)
      check "primary legacy detached steering stops safely and retains the draft" (contents (composerBuffer legacy)=="legacy-steer" && not (agentSteering legacy))
      restarted<-prompt runtime "stream" legacy >>= done runtime
      disconnected<-prompt runtime "disconnect" restarted >>= await runtime "provider EOF" ((=="Agent disconnected.").status)
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
        let terminalOutput=output >>= field "result" >>= field "output"
            -- ConPTY emits rendered VT updates, including cursor controls.
            outputMatches=if os=="mingw32" then maybe False ((==8).BS.length.TE.encodeUtf8) terminalOutput
              else terminalOutput==Just ("23456789"::T.Text)
        check "ACP terminal output and truncation use real backend" (outputMatches && (output >>= field "result" >>= field "truncated")==Just True)
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
    -- User guidance reaches ACP as context, without changing the visible query.
    createDirectoryIfMissing True (root </> "config/thc")
    writeFile (root </> "config/thc/config.toml") "[editor.agent]\ncontext = 'Global guidance marker'\n"
    writeFile (root </> "thc.toml") "[editor.agent]\ncontext = 'Project guidance marker'\n"
    withConversation $ \runtime -> do
      configured<-configure runtime ("yes"::T.Text) desktop
      guided<-prompt runtime "stream" configured >>= done runtime
      entries<-logged
      let prompts=[params | entry<-entries,field "method" entry==Just ("session/prompt"::T.Text),Just params<-[field "params" entry::Maybe Value]]
          sent=json (last prompts)
      check "ACP receives global and project guidance with skill discovery"
        (all (`T.isInfixOf` sent) ["Global guidance marker","Project guidance marker","docs/agent-skills.md"])
      check "guidance is not repeated as a user chat bubble" (not ("Global guidance marker" `T.isInfixOf` conversationText guided))
      (_,public)<-chatTool runtime guided "agent_settings" (object [])
      info<-public
      check "public agent settings expose effective context" (case info of Right value->"Project guidance marker" `T.isInfixOf` json value; _->False)
      again<-prompt runtime "stream" guided >>= done runtime
      repeated<-logged
      let latestPrompt=last [params | entry<-repeated,field "method" entry==Just ("session/prompt"::T.Text),Just params<-[field "params" entry::Maybe Value]]
      check "unchanged guidance does not consume context again" (not ("Global guidance marker" `T.isInfixOf` json latestPrompt))
      let otherProject=root </> "other-project"
      createDirectory otherProject
      writeFile (otherProject </> "thc.toml") "[editor.agent]\ncontext = 'Not this conversation'\n"
      scope<-send runtime "context" [] again {sideTree=fmap (\tree->tree {treeRoot=otherProject}) (sideTree again)}
      check "context UI is a human-only command" (not (guestCommandAllowed AgentGuidance))
      case dialog scope of
        Just dg -> do
          let (next,effects)=submitDialog 0 dg scope
          (_,opened)<-conversationEffects runtime App.applyEffects next effects
          check "context UI opens protected project config for normal editing"
            (fmap (fmap filePath . documentFile) (activeDocument opened)==Just (Just (root </> "thc.toml")) && maybe False (protectedBuffer opened . bufferId) (activeWindow opened))
        Nothing -> error "Missing Agent Context scope chooser"
      writeFile (root </> "thc.toml") "[editor.agent]\ncontext = 'Updated project guidance'\n"
      updated<-prompt runtime "stream" guided >>= done runtime
      messages<-logged
      let latest=last [params | entry<-messages,field "method" entry==Just ("session/prompt"::T.Text),Just params<-[field "params" entry::Maybe Value]]
      check "saved project context reaches the next query without reconnecting" ("Updated project guidance" `T.isInfixOf` json latest)
      waiting<-prompt runtime "wait" updated >>= await runtime "context steering wait" ((=="Agent is replying...").status)
      writeFile (root </> "thc.toml") "[editor.agent]\ncontext = 'Steering guidance marker'\n"
      steered<-send runtime "steer-draft" [] waiting {composerBuffer=newBuffer "direction"} >>= await runtime "context steering completion" (not . agentReplying)
      steeringLog<-logged
      let lastSteer=last [params | entry<-steeringLog,field "method" entry==Just ("_session/steering"::T.Text),Just params<-[field "params" entry::Maybe Value]]
      check "primary steering requests host-owned idle handling" ((field "_meta" lastSteer >>= field "steering" >>= field "idleBehavior")==Just ("promptRequired"::T.Text))
      check "steering receives saved context updates" ("Steering guidance marker" `T.isInfixOf` json lastSteer)
      rejectionWait<-prompt runtime "wait" steered >>= await runtime "rejected context steering wait" ((=="Agent is replying...").status)
      writeFile (root </> "thc.toml") "[editor.agent]\ncontext = 'Retry context marker'\n"
      pendingSteer<-send runtime "steer-draft" [] rejectionWait {composerBuffer=newBuffer "reject-context"}
      let childWaiting=(selectConversationView "fixture-child" "Child" pendingSteer) {composerBuffer=newBuffer "child draft",composerSelection=Selection 2 4}
      let settleHidden d=do
            next<-tickConversation runtime d
            (_,answer)<-chatTool runtime next "agent_settings" (object [])
            settingsResult<-answer
            if either (const False) ((==Just False).field "replying") settingsResult
              then pure next else threadDelay 10000 >> settleHidden next
      hiddenRestored<-timeout 8000000 (settleHidden childWaiting) >>= maybe (error "Hidden primary steering did not settle") pure
      check "failed primary steering preserves selected child draft" (conversationTarget hiddenRestored=="fixture-child" && contents (composerBuffer hiddenRestored)=="child draft" && composerSelection hiddenRestored==Selection 2 4)
      rejectedSteer<-send runtime "show" [] hiddenRestored
      check "switching back restores rejected primary steering draft" (contents (composerBuffer rejectedSteer)=="reject-context")
      afterRejection<-prompt runtime "stream" rejectedSteer >>= done runtime
      retryLog<-logged
      let retryPrompt=last [params | entry<-retryLog,field "method" entry==Just ("session/prompt"::T.Text),Just params<-[field "params" entry::Maybe Value]]
      check "rejected steering does not mark new context delivered" ("Retry context marker" `T.isInfixOf` json retryPrompt)
      writeFile (root </> "thc.toml") "[broken\n"
      failed<-prompt runtime "stream" afterRejection >>= await runtime "invalid context preparation" (\d -> not (agentReplying d) && "TOML" `T.isInfixOf` status d)
      check "invalid context preserves an existing draft without sending" (not (agentReplying failed) && contents (composerBuffer failed)==contents (composerBuffer afterRejection))
      writeFile (root </> "thc.toml") "[editor.agent]\ncontext = ''\n"
      _<-prompt runtime "stream" failed >>= done runtime
      pure ()
    -- A new runtime reads the saved provider configuration, without reconfiguring it.
    withConversation $ \runtime -> do
      restored<-send runtime "new" [] desktop >>= await runtime "persisted provider configuration" ((=="Session fixture-session").status)
      _<-send runtime "resume" [] restored
      pure ()
    let editorSessions=[(replicate 48 'a',"resume-a"::T.Text,root),(replicate 48 'b',"resume-b",root </> "other-project")]
        withEditor ident action=bracket (lookupEnv "THC_EDIT_SESSION" <* setEnv "THC_EDIT_SESSION" ident) (restoreEnvironment "THC_EDIT_SESSION") (const action)
        resumeId d=case [value | Just dg<-[dialog d],Input "Session ID" value _<-fields dg] of value:_->Just value; _->Nothing
    forM_ editorSessions $ \(ident,providerId,providerRoot) -> withEditor ident $ withConversation $ \runtime -> do
      offered<-send runtime "resume" [] desktop
      check "new editor session does not inherit another session resume ID" (resumeId offered==Just "")
      configured<-configure runtime ("yes"::T.Text) desktop {sideTree=fmap (\tree->tree {treeRoot=providerRoot}) (sideTree desktop)}
      _<-send runtime "load" ["0",providerId] configured >>= await runtime "per-editor resume record" ((==("Session "<>providerId)).status)
      sidecar<-(++".agent.json") <$> checkpointPath ident
      saved<-decodeStrict' <$> BS.readFile sidecar
      check "provider resume record is saved beside its editor checkpoint"
        ((saved >>= field "sessionId")==Just providerId && (saved >>= field "cwd")==Just providerRoot)
    withConversation $ \runtime -> do
      _<-send runtime "configure" ["0","not-the-saved-provider", "[]", "{}"] desktop
      pure ()
    beforeRecovery<-logged
    forM_ editorSessions $ \(ident,providerId,_) -> withEditor ident $ withConversation $ \runtime -> do
      let recovered=addReadOnly "Conversation" "Retained conversation after a daemon crash" savedDraft
      idle<-tickConversation runtime recovered
      offered<-send runtime "resume" [] idle
      check "recovered editor selects its own provider resume ID" (resumeId offered==Just providerId)
      check "reading resume metadata leaves recovered transcript and draft intact"
        (buffers offered==buffers recovered && composerBuffer offered==composerBuffer recovered)
    afterRecovery<-logged
    check "recovery never starts a provider or sends a prompt automatically" (afterRecovery==beforeRecovery)
    forM_ editorSessions $ \(ident,providerId,providerRoot) -> withEditor ident $ withConversation $ \runtime -> do
      unrelated<-send runtime "load" ["0","unrelated-session-id"] desktop
      check "an unrelated resume ID does not select another saved provider"
        (maybe False ((=="Cannot start agent").dialogTitle) (dialog unrelated))
      loaded<-send runtime "load" ["0",providerId] desktop >>= await runtime "saved provider and project" ((==("Session "<>providerId)).status)
      entries<-logged
      let loads=[params | entry<-entries,field "method" entry==Just ("session/load"::T.Text),Just params<-[field "params" entry::Maybe Value]]
      check "explicit resume selects saved provider and working directory"
        (field "cwd" (last loads)==Just providerRoot)
      configured<-send runtime "options" [] loaded
      check "saved provider selection is local to the resumed editor"
        (case dialog configured of Just dg->Input "Executable" "python3" 7 `elem` fields dg; _->False)
    globalProvider<-decodeStrict' <$> BS.readFile (settings </> "agents.json")
    check "resuming a saved provider does not rewrite global configuration"
      ((globalProvider >>= field "executable")==Just ("not-the-saved-provider"::T.Text))
    withConversation $ \runtime -> do
      configured<-configure runtime ("yes"::T.Text) desktop
      answered<-prompt runtime ("wide\n"<>T.unwords (replicate 90 "user-width")) configured >>= done runtime
      forM_ [150,36,120] $ \columns -> do
        let resized=fst (handleEvent (V.EvResize columns 30) answered)
            (zoomed,effects)=runCommand Zoom resized
        (_,effected)<-conversationEffects runtime fallback zoomed effects
        shown<-tickConversation runtime effected
        let doc=fromMaybe (error "missing resized live conversation") (activeDocument shown)
            win=fromMaybe (error "missing resized live window") (activeWindow shown)
            available=width (bounds win)-2
            rows=splitStyled (documentHighlight doc)
            sent row=any (\(_,style)->case style of BubbleText _ True _->True; _->False) row
            received row=any (\(_,style)->case style of BubbleText _ False _->True; _->False) row
            columnsOf row=let text=T.pack (map fst row) in displayColumn text (T.length text)
        check "resize then Zoom anchors live user bubbles at the right window edge"
          (not (null (filter sent rows)) && all ((==available).columnsOf) (filter sent rows))
        check "resize then Zoom lets long live replies span the available window"
          (maximum (0:map columnsOf (filter received rows))>available-16 && all ((<=available).columnsOf) rows)
        check "live reflow preserves exact shell payload"
          (map (\(_,_,language,raw)->(language,raw)) (documentShellBlocks doc)==[("sh","printf 'live λ'\n")])

      let hub=AR.agentHub (conversationAgents runtime)
          caps=AH.Capabilities False False False []
          driver=AH.AgentDriver root "layout-fixture" caps (const (pure (Right caps)))
            (const (pure (Right Null))) (pure ()) (pure ()) (const (pure (Right Null)))
      child<-AH.registerAgent hub "Layout child" root driver >>= either (error . T.unpack) pure
      ticket<-AH.sendAgent hub AH.Human child (T.unwords (replicate 90 "child-user")) >>= either (error . T.unpack) pure
      _<-AH.waitAgent hub AH.Human child ticket 2000 >>= either (error . T.unpack) pure
      AH.recordAgentEvent hub child "output" (object ["text" .= (T.unwords (replicate 90 ("child-reply"::T.Text))<>"\n\n```sh\nprintf 'child λ'\n```" :: T.Text)])
      childView<-tickConversation runtime (selectConversationView (AH.agentIdText child) "Layout child" answered)
      let (primaryId,_)=fromMaybe (error "primary view missing") (conversationDocument "" childView)
          (childId,_)=fromMaybe (error "child view missing") (conversationDocument (AH.agentIdText child) childView)
          baseWindow=fromMaybe (error "child window missing") (activeWindow childView)
          paired primaryColumns childColumns=childView
            { conversationTarget=""
            , windows=[baseWindow {bufferId=childId,bounds=(bounds baseWindow) {width=childColumns}},
                baseWindow {windowId=windowId baseWindow+100,bufferId=primaryId,bounds=(bounds baseWindow) {width=primaryColumns}}] }
          bufferIdentity bid d=makeStableName =<< evaluate (documentBuffer (fromMaybe (error "chat buffer missing") (M.lookup bid (buffers d))))
      forM_ [(148,62),(43,126)] $ \(primaryColumns,childColumns) -> do
        shown<-tickConversation runtime (paired primaryColumns childColumns)
        forM_ [(primaryId,primaryColumns,"printf 'live λ'\n"),(childId,childColumns,"printf 'child λ'\n")] $ \(bid,columns,body) -> do
          let doc=fromMaybe (error "visible chat missing") (M.lookup bid (buffers shown))
              rows=splitStyled (documentHighlight doc)
              sent row=any (\(_,style)->case style of BubbleText _ True _->True; _->False) row
              received row=any (\(_,style)->case style of BubbleText _ False _->True; _->False) row
              columnsOf row=let text=T.pack (map fst row) in displayColumn text (T.length text)
          check "simultaneously visible chats anchor user bubbles to their own right edge"
            (not (null (filter sent rows)) && all ((==columns-2).columnsOf) (filter sent rows))
          check "inactive child and primary replies reflow at their own window width"
            (maximum (0:map columnsOf (filter received rows))>columns-18 && all ((<=columns-2).columnsOf) rows)
          check "each resized target retains its own exact executable shell body"
            (map (\(_,_,_,raw)->raw) (documentShellBlocks doc)==[body])
        before<-mapM (\bid->bufferIdentity bid shown) [primaryId,childId]
        unchanged<-tickConversation runtime shown
        after<-mapM (\bid->bufferIdentity bid unchanged) [primaryId,childId]
        check "unchanged visible widths preserve rendered buffer identities" (before==after)

  where
    restore=maybe (unsetEnv "XDG_CONFIG_HOME") (setEnv "XDG_CONFIG_HOME")
    restoreEnvironment name=maybe (unsetEnv name) (setEnv name)
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

#ifndef mingw32_HOST_OS
-- A child process can block opening the write end without blocking a GHC
-- capability. Its acknowledgement proves that the capture reader has opened;
-- only the explicit gate releases the payload and EOF.
withHeldRead :: FilePath -> FilePath -> (MVar () -> MVar () -> Async () -> IO a) -> IO a
withHeldRead server pipe action =
  withCreateProcess (proc "python3" [server,"held-pipe-writer",pipe])
    {std_in=CreatePipe,std_out=CreatePipe} $ \input output _ child ->
      case (input,output) of
        (Just gate,Just ready) -> do
          opened<-newEmptyMVar
          release<-newEmptyMVar
          withAsync (do
            marker<-B8.hGetLine ready
            check "held read child acknowledges actual reader" (marker=="reader-ready")
            putMVar opened ()
            takeMVar release
            BS.hPut gate "!"
            hFlush gate
            code<-waitForProcess child
            check "held read child exits successfully" (code==ExitSuccess)) $ action opened release
        _ -> error "Held read child pipes missing"

waitForReader :: MVar () -> IO ()
waitForReader ready = do
  result<-timeout 3000000 (takeMVar ready)
  check "capture reader opens held FIFO before payload release" (result==Just ())
#endif

providerScript :: String
providerScript=unlines
  [ "import json,os,sys"
  , "if len(sys.argv)==3 and sys.argv[1]=='held-pipe-writer':"
  , "  with open(sys.argv[2],'wb',buffering=0) as output:"
  , "    print('reader-ready',flush=True)"
  , "    assert sys.stdin.buffer.read(1)==b'!'"
  , "    output.write(b'held read\\n')"
  , "  sys.exit(0)"
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
  , "  elif method=='_session/steering': reply(ident,{'outcome':'failed' if params['prompt'][0]['text']=='reject-context' else 'promptRequired' if params['prompt'][0]['text']=='idle-race' else 'startedNewTurn' if params['prompt'][0]['text']=='legacy-steer' else 'injected'}); finish()"
  , "  elif method=='session/prompt':"
  , "    prompt=ident; scenario=params['prompt'][0]['text'].splitlines()[0]"
  , "    if scenario=='stream':"
  , "      update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'# Hello\\n\\n**bold'}})"
  , "      update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':' text**\\n'}})"
  , "      update({'sessionUpdate':'tool_call','toolCallId':'fixture-tool','title':'Local tool','status':'pending','rawInput':{'argument':'original argument'}})"
  , "      update({'sessionUpdate':'tool_call_update','toolCallId':'fixture-tool','status':'completed','rawOutput':{'answer':'exact response'}})"
  , "      update({'sessionUpdate':'usage_update','used':300000,'size':400000})"
  , "      update({'sessionUpdate':'usage_update','used':148000,'size':400000})"
  , "      update({'sessionUpdate':'usage_update','used':-1,'size':0})"
  , "      finish()"
  , "    elif scenario=='wide':"
  , "      update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':('reply-width '*90)+'\\n\\n```sh\\nprintf \\\'live λ\\\'\\n```'}})"
  , "      finish()"
  , "    elif scenario=='tool-run':"
  , "      for n,title in enumerate(['Read files','Run tests','Check output']):"
  , "        update({'sessionUpdate':'tool_call','toolCallId':'run-'+str(n),'title':title,'status':'pending','rawInput':'run argument'})"
  , "        update({'sessionUpdate':'tool_call_update','toolCallId':'run-'+str(n),'status':'failed' if n==2 else 'completed','rawOutput':'run result'})"
  , "      update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'Between tool runs.'}})"
  , "      update({'sessionUpdate':'tool_call','toolCallId':'isolated','title':'Isolated call','status':'completed'})"
  , "      finish()"
  , "    elif scenario=='permission':"
  , "      serial+=1; call('permission-'+str(serial),'session/request_permission',{'toolCall':{'title':'Fixture action'},'options':[{'optionId':'allow','name':'Allow once','kind':'allow_once'},{'optionId':'deny','name':'Reject','kind':'reject_once'}]})"
  , "    elif scenario=='write':"
  , "      serial+=1; call('read-'+str(serial),'fs/read_text_file',{'path':os.environ['THC_SOURCE']})"
  , "    elif scenario=='slow-shutdown':"
  , "      call('shutdown-read','fs/read_text_file',{'path':os.path.join(os.path.dirname(os.environ['THC_SOURCE']),'slow-source')})"
  , "      update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'shutdown file sent'}})"
  , "    elif scenario=='slow-cancel':"
  , "      call('cancel-read','fs/read_text_file',{'path':os.path.join(os.path.dirname(os.environ['THC_SOURCE']),'slow-source')})"
  , "      update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'cancel file sent'}})"
  , "    elif scenario=='slow-files':"
  , "      call('slow-read','fs/read_text_file',{'path':os.path.join(os.path.dirname(os.environ['THC_SOURCE']),'slow-source')})"
  , "      call('slow-write','fs/write_text_file',{'path':os.environ['THC_SOURCE'],'content':'must not overwrite'})"
  , "      update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'file requests sent'}})"
  , "    elif scenario in ['slow-replaced','slow-private']:"
  , "      call(scenario+'-head','fs/read_text_file',{'path':os.path.join(os.path.dirname(os.environ['THC_SOURCE']),'slow-source')})"
  , "      call(scenario+'-source','fs/read_text_file',{'path':os.environ['THC_SOURCE'],'line':1,'limit':1})"
  , "      update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'guarded requests sent'}})"
  , "    elif scenario=='write-two': call('two-read-a','fs/read_text_file',{'path':os.environ['THC_SOURCE']})"
  , "    elif scenario=='wait': update({'sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'waiting for cancellation'}})"
  , "    elif scenario=='disconnect': sys.exit(0)"
  , "    elif scenario.startswith('terminal'):"
  , "      terminal_serial+=1"
  , "      command=\"printf 'λ0123456789'; sleep 0.1; exit 9\" if scenario=='terminal' else ('touch should-not-exist' if scenario=='terminal-reject' else 'sleep 30')"
  , "      executable='/bin/sh'"
  , "      if os.name=='nt':"
  , "        executable=sys.executable"
  , "        command=\"import sys,time;print('0123456789',end='',flush=True);time.sleep(0.1);sys.exit(9)\" if scenario=='terminal' else (\"open('should-not-exist','w').close()\" if scenario=='terminal-reject' else 'import time;time.sleep(30)')"
  , "      call('terminal-create-'+str(terminal_serial),'terminal/create',{'command':executable,'args':['-c',command],'outputByteLimit':8})"
  , "  elif method is None and isinstance(ident,str):"
  , "    if ident=='slow-write' or ident in ['slow-replaced-source','slow-private-source']: finish()"
  , "    elif ident=='two-read-a': call('two-read-b','fs/read_text_file',{'path':os.environ['THC_SECOND']})"
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

splitStyled :: [(Char,Style)] -> [[(Char,Style)]]
splitStyled cells=case break ((=='\n').fst) cells of
  (row,[]) -> [row]
  (row,_:rest) -> row:splitStyled rest
