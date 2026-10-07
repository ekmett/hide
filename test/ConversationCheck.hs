{-# LANGUAGE CPP, OverloadedStrings #-}
module ConversationCheck (checks, composerCodeChecks, draftReceiptChecks, questionInsertionChecks) where

import EditorFixture (withEditorFixture,withEditorBodyFixture,sameBufferVersions)
import qualified Hide.Plugin.Window as W
import qualified Data.Vector as Vec
import Hide.TextPresentation (withTextPresentation,textPresentationEffects,tickTextPresentation,TextPresentation)
import qualified Hide.Conversation as Conversation
import qualified Hide.Plugin.Menu as HideMenu
import qualified Hide.Plugin.Editor as Editor
import Hide.AgentSidebarTypes (AgentSidebarRequest(..))
import MCPPermissionsCheck (settledTool,settleDialog)
import SourceWindowFixture (sourceFixtureBuffer)
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
import qualified Data.Set as S
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime, minutesToTimeZone, addUTCTime)
import Data.List (find,findIndex,mapAccumL)
import Data.IORef (newIORef,writeIORef,readIORef)
import GHC.Conc (getAllocationCounter)
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
import Hide.Render (snapshot, snapshotHtml, renderCursor)
import Hide.Font (loadFont)
import qualified Hide.ScreenCapture as ScreenCapture
import Hide.Buffer
import qualified Hide.App as App
import Hide.GuestAccess (guestCommandAllowed, protectedBuffer, readableAt)
import Hide.Conversation
import Hide.SessionServices
import qualified Hide.BuildJobs as Jobs
import qualified Hide.Build as Build
import qualified Hide.Terminal as Terminal
import qualified Hide.Consoles as C
import qualified Hide.AgentHub as AH
import qualified Hide.AgentRuntime as AR
import Hide.Files
import Hide.Plugin.BufferHost (captureVersion,versionCurrent)
import Hide.Sidebar
import qualified Hide.MCPPermissions as Permissions
import Hide.Model hiding (prompt)
import Hide.Commands (configuredBindings)
import Hide.Markdown (renderMarkdown)
import Hide.Syntax (Style(..),StyledText,StyledRow(..),Sigils(..),styledText,styledContents,styledLength,splitStyledText,graphemeText)
import Hide.Terminal (terminalAvailable)
import Hide.Session (checkpointPath)
import Hide.Recovery (writeCheckpoint,readCheckpoint)

draftBuffer :: Buffer -> Desktop -> Desktop
draftBuffer b d=setComposerInput b (composerSelection d) (composerFocused d) d

draftAt :: Buffer -> Selection -> Desktop -> Desktop
draftAt b selected d=setComposerInput b selected True d

sameDraftRoot :: Desktop -> Desktop -> IO Bool
sameDraftRoot before after=captureVersion (composerBuffer before) >>= \version->versionCurrent version (composerBuffer after)

-- Input checks attach the authenticated token to their already-admitted
-- immutable body; setting ChatQuestion alone grants no editing authority.
questionOwner :: Int -> Desktop -> Desktop
questionOwner token desktop=desktop {conversationViews=M.adjust authorize "" (conversationViews desktop)}
  where
    body=fromMaybe (error "Missing inline question fixture body") (conversationBodySnapshot "" desktop)
    columns=maybe 1 (\window->max 1 (width (bounds window)-2)) (activeWindow desktop)
    authorize view=case conversationBody view of
      InstalledBody reference _->view {conversationBody=InstalledBody reference
        (Just (BodyControlReceipt body columns (wideSectionTitles desktop) Nothing (HostBodyControls (Just token) Nothing Nothing [])))}
      _->error "Missing inline question fixture lifetime"

editorEffect :: ChatSubmit -> Desktop -> Effect
editorEffect action d=SubmitEditor (fromMaybe (error "Missing editor mount") (activeEditorMount d))
  (if action==QuerySubmit then Editor.DefaultEditor else Editor.AlternateEditor) HideMenu.HumanMenu

submit :: ConversationState -> ChatSubmit -> Desktop -> IO Desktop
submit runtime action d=let (next,effects)=runCommand (SubmitChat action) d in
  snd <$> conversationEffects runtime (\value _->pure (False,value)) next effects

openChild :: ConversationState -> AH.AgentId -> Desktop -> IO Desktop
openChild runtime ident d=snd <$> conversationEffects runtime (\value _->pure (False,value)) d [AgentSidebarAction (ShowAgent ident)]

-- Draft code is ordinary Markdown carried by the existing Buffer. Exercise
-- user edits and submitted/copy payloads, without depending on bubble artwork.
composerCodeChecks :: IO ()
composerCodeChecks=do
  questionInsertionChecks
  withEditorFixture "" (initialDesktop (100,35)) $ \primary->
    withEditorFixture "child" primary $ \fixtures->do
      let check label ok=unless ok (error label)
          press key mods=fst . handleEvent (V.EvKey key mods)
          typeText text desktop=foldl (\d c->press (V.KChar c) [] d) desktop (T.unpack text)
          paste text=fst . handleEvent (V.EvPaste (TE.encodeUtf8 text))
          chat=selectConversationView "" "Primary" fixtures
          toChat source=selectConversationView "" "Primary" source {conversationViews=conversationViews chat,editorDrafts=editorDrafts chat,pluginWindows=M.union (pluginWindows chat) (pluginWindows source),windows=windows source++windows chat}
          block=typeText "> " chat
          code=typeText "x = 1" block
          extended=paste "  y = 2\nz = 3" (press V.KEnter [] code)
          normal=typeText "Then explain it." (press V.KDown [] extended)
          text d=contents (composerBuffer d)
          isCodeChar (_,CodeStyle _ _)=True
          isCodeChar _=False
      check "greater-than-space creates code with a normal exit line" (composerInCode block && bufferLineCount (composerBuffer block)==2)
      let codeWindow=maybe (error "missing conversation window") id (activeWindow code)
          codeRect=composerRect code codeWindow
          clickedCode=fst (handleEvent (V.EvMouseDown (left codeRect+1) (top codeRect) V.BLeft []) code)
      check "clicking visible code edits its text rather than its hidden indentation"
        (text (typeText "Z" clickedCode)=="    xZ = 1\n")
      check "code creation is one undoable edit" (text (fst (runCommand Undo block))==">" && text (fst (runCommand Redo (fst (runCommand Undo block))))==text block)
      check "Enter extends code while Down leaves it for normal prose"
        ("    x = 1\n      y = 2\n    z = 3\nThen explain it."==text normal &&
         not (composerInCode normal) && any isCodeChar (renderMarkdown 80 (text normal)))
      let listCode=paste "line = 1" (typeText "> " (press V.KEnter [V.MShift] (typeText "- Inspect this:" chat)))
          fenceCode="    before\n    ```\n    after\n"
      check "submission preserves code after a list and embedded fence characters"
        (any isCodeChar (renderMarkdown 80 (composerMarkdown (text listCode))) &&
         "```" `T.isInfixOf` T.concat [text | pair@(text,_)<-renderMarkdown 80 (composerMarkdown fenceCode),isCodeChar pair] &&
         composerMarkdown "```hs\n    original indentation\n```\n"=="```hs\n    original indentation\n```\n")
      check "Control Enter keeps the configured opposite submit action inside code"
        (snd (handleEvent (V.EvKey V.KEnter [V.MCtrl]) extended)==[editorEffect SteerSubmit extended] &&
         snd (handleEvent (V.EvKey V.KEnter [V.MCtrl]) extended {chatSubmit=SteerSubmit})==[editorEffect QuerySubmit extended])
      let maps=either (error . show) id (configuredBindings [] M.empty)
          clickHint action desktop=case [r | (r,_,Left command)<-statusItemRects desktop,command==SubmitChat action] of
            r:_->snd (handleEvent (V.EvMouseDown (left r) (top r) V.BLeft []) desktop)
            []->error "missing code submission status action"
      forM_ [QuerySubmit,SteerSubmit] $ \chosen->forM_ [False,True] $ \replying->do
        let desktop=extended {keyBindings=maps,chatSubmit=chosen,agentReplying=replying}
            caption action=(if action==chosen then "" else "Ctrl+Enter ")<>
              (if action==SteerSubmit then "Steer" else if replying then "Queue query" else "Query")
        check "code status preserves both fixed submission captions with compiled bindings"
          (all (\action->any (\(label,target)->T.strip label==caption action && target==Just (Left (SubmitChat action))) (statusHints desktop)) [QuerySubmit,SteerSubmit])
        check "code status clicks keep query and steer ownership with compiled bindings"
          (clickHint QuerySubmit desktop==[editorEffect QuerySubmit extended] && clickHint SteerSubmit desktop==[editorEffect SteerSubmit extended] &&
           snd (handleEvent (V.EvKey V.KEnter [V.MCtrl]) desktop)==[editorEffect (if chosen==QuerySubmit then SteerSubmit else QuerySubmit) desktop])
      let copied=fst (runCommand Copy (fst (runCommand SelectAll extended)))
          unwrapped=press V.KBS [] (press V.KHome [] code)
          backIn=press V.KBS [] (press V.KDown [] code)
      check "composer copy removes only Markdown markers and keeps code indentation" (clipboard copied=="x = 1\n  y = 2\nz = 3\n")
      check "backspace at code start unwraps and backspace from the exit line reenters" (text unwrapped=="x = 1\n" && composerInCode backIn && text backIn==text code)
      let sourceText="main = 1\n  helper = 2\n"
          source=fst (runCommand SelectAll (addDocument (Just (FileState "/project/Main.hs" Nothing)) (newBuffer sourceText) (initialDesktop (100,35))))
          sourceCopy=fst (runCommand Copy source)
          sourceCut=fst (runCommand Cut source)
          pasted=fst (runCommand Paste (toChat sourceCopy))
          nativePaste=paste sourceText (toChat sourceCopy)
          browserPaste=runCommand Paste (toChat sourceCopy) {browserFrontend=True}
          external=paste "plain replacement" (toChat sourceCopy)
          prosePaste=paste sourceText (typeText "Please inspect:" (toChat sourceCopy))
      check "copied and cut source paste as code through internal or native clipboard"
        (composerInCode pasted && text pasted==text nativePaste && activeText sourceCut=="" &&
         composerInCode (fst (runCommand Paste (toChat sourceCut))) &&
         clipboard sourceCopy==sourceText && "      helper = 2" `T.isInfixOf` text pasted)
      check "source paste is one undo step and separates existing prose"
        (T.null (text (fst (runCommand Undo pasted))) && "Please inspect:\n\n    main" `T.isPrefixOf` text prosePaste)
      check "browser paste still requests its clipboard and external text does not inherit code formatting"
        (snd browserPaste==[ReadBrowserClipboard] && text external=="plain replacement" && not (composerInCode external))
      let location=fst (runCommand CopyLocation (modifyActive (\w->w {selection=Selection 11 11}) (copyClipboard True "/project/Main.hs:2:3" sourceCopy)))
          locationPaste=fst (runCommand Paste (toChat location))
      check "Copy Location is one-based plain text even after copying source code"
        (clipboard location=="/project/Main.hs:2:3" && text locationPaste==clipboard location && not (composerInCode locationPaste) && lookup "Copy Location" (contextItems SourceContext)==Just CopyLocation)
      let child=selectConversationView "child" "Child" chat
          childCode=typeText "child = 1" (typeText "> " child)
          returned=selectConversationView "child" "Child" (selectConversationView "" "Primary" childCode)
          question=(questionOwner 1 chat) {chatQuestion=Just (ChatQuestion 1 "Answer?" [] Nothing (newBuffer "") (Selection 0 0) True),clipboard=sourceText,clipboardCode=Just sourceText}
          answered=typeText "> " question
          questionPaste=fst (runCommand Paste question)
      check "child code drafts survive switching conversations" (text returned==text childCode && composerInCode returned)
      check "inline question input stays plain text" (fmap (contents.questionBuffer) (chatQuestion answered)==Just "> " && fmap (contents.questionBuffer) (chatQuestion questionPaste)==Just (T.map (\c->if c=='\n' then ' ' else c) sourceText))
      let ignoredQuestion=fst (handleEvent (V.EvKey V.KEnter [V.MCtrl]) answered)
      check "inline question Control Enter preserves answer and separate draft"
        (chatQuestion ignoredQuestion==chatQuestion answered)
      unchanged<-captureVersion (composerBuffer answered)
      current<-versionCurrent unchanged (composerBuffer ignoredQuestion)
      check "inline question leaves the separate draft root unchanged" current
      putStrLn "composer code checks passed"


-- Normalization and the cap belong to the inserted fragment, with one Undo.
questionInsertionChecks :: IO ()
questionInsertionChecks=do
  withEditorFixture "" (initialDesktop (100,35)) $ \prepared->do
    let ordinary=newBuffer "independent draft"
        chat=questionOwner 1 (draftBuffer ordinary prepared)
        question text sel=chat {chatQuestion=Just (ChatQuestion 1 "Answer?" [] Nothing (newBuffer text) sel True)}
        answer d=maybe (error "Missing inline answer") questionBuffer (chatQuestion d)
        pasted text d=fst (runCommand Paste d {clipboard=text})
        textOf=contents.answer
        expected source sel inserted=let (a,z)=ordered sel in
          T.take 4096 (T.map (\c->if c `elem` ['\n','\r','\t'] then ' ' else c) (T.take a source<>inserted<>T.drop z source))
        checkEdit source sel inserted result=do
          let wanted=expected source sel inserted
              undone=fst (runCommand Undo result)
              redone=fst (runCommand Redo undone)
          check "inline insertion keeps normalization and cap result" (textOf result==wanted)
          check "inline normalized/capped insertion is one Undo step" (textOf undone==source && textOf (fst (runCommand Undo undone))==source)
          check "inline insertion Redo restores the bounded answer" (textOf redone==wanted)
          receipt<-captureVersion ordinary
          current<-versionCurrent receipt (composerBuffer result)
          check "inline insertion does not borrow the ordinary draft" current
    let before="before"
        inserted="\nnext\tpart\r"
        initial=question before (Selection 6 6)
    checkEdit before (Selection 6 6) inserted (pasted inserted initial)
    checkEdit before (Selection 6 6) inserted (fst (handleEvent (V.EvPaste (TE.encodeUtf8 inserted)) initial))
    checkEdit before (Selection 6 6) inserted (fst (handleEvent (V.EvKey (V.KChar 'v') [V.MCtrl]) initial {clipboard=inserted}))
    checkEdit before (Selection 6 6) "\n" (fst (handleEvent (V.EvKey V.KEnter [V.MShift]) initial))
    let nonuniform=T.take 4096 (T.concat (replicate 500 "012α界e\x301\&😀XYZ"))
    forM_ [(Selection 0 0,"front"),(Selection 17 17,T.replicate 30 "λ界"),
           (Selection 31 8,"selected λ界\ttext"),(Selection 25 25,T.replicate 5000 "界"),
           (Selection 4096 4096,"full end")] $ \(sel,addition)->
      checkEdit nonuniform sel addition (pasted addition (question nonuniform sel))
    let large=(newBuffer (T.replicate 4096 "界")) {saved=error "question selection forced baseline",undoStack=error "question selection forced Undo"}
        focused=chat {chatQuestion=Just (ChatQuestion 1 "Answer?" [] Nothing large (Selection 0 0) True)}
    _<-evaluate (prepareBuffer large)
    _<-evaluate (questionActive focused)
    beforeAllocation<-getAllocationCounter
    selected<-evaluate (maybe (-1) (caret.questionSelection) (chatQuestion (fst (runCommand SelectAll focused))))
    afterAllocation<-getAllocationCounter
    check "inline selection does not normalize the existing answer" (selected==4096 && beforeAllocation-afterAllocation<8000)
    let first=pasted inserted initial
        second=pasted "more" first
    check "successive inline edits retain earlier Undo" (textOf (fst (runCommand Undo second))==textOf first && textOf (fst (runCommand Undo (fst (runCommand Undo second))))==before)
    putStrLn "inline question insertion checks passed"

-- Acceptance consumes only the submitted immutable draft, including hidden views.
-- Gates use the same ACP process and held-read fixture as the other owner checks.
draftReceiptChecks :: IO ()
#ifdef mingw32_HOST_OS
draftReceiptChecks=pure ()
#else
draftReceiptChecks=withTextPresentation $ \presentation->
  let tickConversation=tickPresented presentation
  in bracket temporary removePathForcibly $ \root->
  bracket (lookupEnv "XDG_CONFIG_HOME" <* setEnv "XDG_CONFIG_HOME" (root </> "config")) (restoreDraftEnvironment "XDG_CONFIG_HOME") $ \_->
  bracket (lookupEnv "XDG_DATA_HOME" <* setEnv "XDG_DATA_HOME" (root </> "data")) (restoreDraftEnvironment "XDG_DATA_HOME") $ \_->
  bracket (lookupEnv "THC_EDIT_SESSION" <* unsetEnv "THC_EDIT_SESSION") (restoreDraftEnvironment "THC_EDIT_SESSION") $ \_->do
    let server=root </> "provider.py"
        context=root </> "thc.toml"
        gate=root </> "steer-gate"
        source=root </> "Source.hs"
        environment=object ["THC_LOG" .= (root </> "messages.jsonl"),"THC_SOURCE" .= source,"THC_SECOND" .= source,"THC_RESUME" .= ("yes"::T.Text)]
        send runtime action values desktop=snd <$> conversationEffects runtime (\d _->pure (False,d)) desktop [AgentAction action values]
        configure runtime d=send runtime "configure" ["0","python3",json [server],json environment] d >>= send runtime "show" []
        prompt runtime text=send runtime "send" ["0",text,"false","false","false"]
        await runtime label predicate desktop=do
          observed<-newIORef (status desktop,agentReplying desktop,agentQueued desktop)
          let loop d=do
                next<-tickConversation runtime d
                writeIORef observed (status next,agentReplying next,agentQueued next)
                accepted<-predicate next
                if accepted then pure next else threadDelay 10000 >> loop next
          result<-timeout 8000000 (loop desktop)
          case result of
            Just accepted->pure accepted
            Nothing->do
              lastState<-readIORef observed
              messages<-readMessages (root </> "messages.jsonl")
              error ("Draft receipt timeout: "++label++"; state="++show lastState++"; provider="++show (map (field "method" :: Value -> Maybe T.Text) (drop (max 0 (length messages-6)) messages)))
        primaryDone runtime label=await runtime (label++" primary acceptance") $ \d->do
          (_,reply)<-chatTool runtime d "agent_settings" (object [])
          value<-reply >>= either (error.T.unpack) pure
          pure (field "replying" value==Just False)
        fresh original=do
          replacement<-evaluate (newBuffer (T.copy (contents original)))
          old<-captureVersion original; new<-captureVersion replacement
          check "draft fixture replaces equal text at equal revision" (old/=new && revision original==revision replacement)
          pure replacement
    BS.writeFile server (TE.encodeUtf8 (T.pack providerScript))
    BS.writeFile source "main=1\n"
    let base=(addDocument (Just (FileState source Nothing)) (newBuffer "main=1\n") (initialDesktop (100,35))) {sideTree=Just (emptySidebar root 20 False)}
    -- Submission identity is captured before the initial provider handshake.
    bracket (lookupEnv "THC_CONNECT_GATE" <* setEnv "THC_CONNECT_GATE" gate) (restoreDraftEnvironment "THC_CONNECT_GATE") $ \_->
      C.withConsoles $ \consoles -> withConversationAt consoles root $ \runtime->do
        configured<-configure runtime base
        original<-evaluate (newBuffer "stream")
        createNamedPipe gate 0o600
        withHeldRead server gate "connected" $ \opened release writer->do
          submitted<-submit runtime QuerySubmit (draftBuffer original configured)
          let awaitConnection d=do
                openedNow<-not <$> isEmptyMVar opened
                if openedNow then pure d else tickConversation runtime d >>= \next->threadDelay 1000 >> awaitConnection next
          connecting<-timeout 3000000 (awaitConnection submitted) >>= maybe (error "Initial editor connection did not reach its gate") pure
          waitForReader opened
          followup<-evaluate (newBuffer "stream\nconnecting followup")
          queuedConnecting<-submit runtime QuerySubmit (draftBuffer followup connecting)
          check "input while initializing joins the original provider queue" (agentQueued queuedConnecting==1)
          newer<-fresh original
          putMVar release (); wait writer
          accepted<-primaryDone runtime "connecting replacement" (draftBuffer newer queuedConnecting)
          connectedMessages<-readMessages (root </> "messages.jsonl")
          let sent=[params | entry<-connectedMessages,field "method" entry==Just ("session/prompt"::T.Text),Just params<-[field "params" entry::Maybe Value]]
              sentText params=case (field "prompt" params::Maybe [Value]) of Just (first:_)->field "text" first; _->Nothing
          check "initial connection accepts queued input in order" (map sentText sent==[Just ("stream"::T.Text),Just "stream\nconnecting followup"])
          check "connecting submission cannot clear a same-text new draft" (contents (composerBuffer accepted)=="stream")
        removeFile gate
    writeFile context "[editor.agent]\ncontext='receipt guidance'\n"
    forM_ [(False,False,False),(False,True,False),(False,True,True),(True,False,False),(True,True,False)] $ \(steer,replaced,requeue)->C.withConsoles $ \consoles -> withConversationAt consoles root $ \runtime->do
      let scenario=if steer then (if replaced then "steer replacement" else "steer unchanged") else if requeue then "query requeue" else if replaced then "query replacement" else "query unchanged"
      configured<-configure runtime base
      connected<-prompt runtime "stream" configured >>= primaryDone runtime (scenario++" connect")
      untouched<-evaluate (newBuffer "stream")
      independent<-prompt runtime "stream" (draftBuffer untouched connected) >>= primaryDone runtime (scenario++" independent")
      check "independent prompt does not consume the composer" (contents (composerBuffer independent)=="stream")
      active<-if steer then prompt runtime "wait" independent >>= await runtime "active primary turn" (pure . (=="Agent is replying...") . status) else pure independent
      original<-evaluate (newBuffer (if steer then "direction" else "stream"))
      -- Preparation may finish off-thread, but only a later serialized tick adopts it.
      beforeSubmission<-readMessages (root </> "messages.jsonl")
      submittedReceipt<-captureVersion original
      submitted<-submit runtime (if steer then SteerSubmit else QuerySubmit) (draftBuffer original active)
      sameDraft<-versionCurrent submittedReceipt (composerBuffer submitted)
      check "submitted draft remains pending before owner adoption" (agentReplying submitted && sameDraft)
      current<-if replaced then fresh original else pure original
      staged<-if requeue then do
        queuedIntent<-submit runtime QuerySubmit (draftBuffer current submitted)
        pendingCurrent<-captureVersion current >>= \version->versionCurrent version (composerBuffer queuedIntent)
        check "same-text replacement is a new queued query before adoption" (agentQueued queuedIntent==1 && pendingCurrent)
        queued<-await runtime "replacement queued acceptance" (pure . (==0) . bufferLength . composerBuffer) queuedIntent
        newest<-fresh original
        pure (draftBuffer newest queued)
        else if not steer && not replaced then do
          duplicate<-submit runtime QuerySubmit submitted
          samePending<-versionCurrent submittedReceipt (composerBuffer duplicate)
          check "unchanged pending draft is submitted once" (agentQueued duplicate==0 && samePending)
          pure duplicate
        else pure (draftBuffer current submitted)
      let sourceFrame=fromMaybe (error "Missing receipt source frame") (listToMaybe [w | w<-windows staged,Just doc<-[windowDocument (buffers staged) w],fmap filePath (documentFile doc)==Just source])
          hidden=if steer then focusWindow (windowId sourceFrame) staged else staged
      settled<-primaryDone runtime scenario hidden
      let restored=if steer then selectConversationView "" "Primary" settled else settled
      when requeue $ do
        afterSubmission<-readMessages (root </> "messages.jsonl")
        let prompts values=[params | entry<-values,field "method" entry==Just ("session/prompt"::T.Text),Just params<-[field "params" entry::Maybe Value]]
            added=drop (length (prompts beforeSubmission)) (prompts afterSubmission)
            firstText params=case (field "prompt" params::Maybe [Value]) of Just (first:_)->field "text" first; _->Nothing
        check "replacement query reaches the provider once in submission order" (length added==2 && all ((==Just ("stream"::T.Text)).firstText) added)
      check "acceptance preserves a replacement draft and clears only the submitted one"
        (if replaced then contents (composerBuffer restored)==contents original else bufferLength (composerBuffer restored)==0)
    bracket (lookupEnv "THC_EDIT_SESSION" <* setEnv "THC_EDIT_SESSION" (replicate 48 'c')) (restoreDraftEnvironment "THC_EDIT_SESSION") $ \_->
      bracket (lookupEnv "THC_STEER_GATE" <* setEnv "THC_STEER_GATE" gate) (restoreDraftEnvironment "THC_STEER_GATE") $ \_->
        forM_ [False,True] $ \replaced->C.withConsoles $ \consoles -> withConversationAt consoles root $ \runtime->do
          _<-configure runtime base
          let hub=AR.agentHub (conversationAgents runtime)
          ident<-AH.spawnAgent hub AH.Human (AH.SpawnSpec "Receipt child" "wait" root AH.Shared AH.Fresh Nothing Nothing) >>= either (error.T.unpack) pure
          _<-AH.sendAgent hub AH.Human ident "wait" >>= either (error.T.unpack) pure
          let running=do value<-AH.statusAgent hub AH.Human ident >>= either (error.T.unpack) pure
                         if field "status" value==Just ("running"::T.Text) then pure () else threadDelay 10000 >> running
          timeout 3000000 running >>= maybe (error "Receipt child did not start") pure
          original<-evaluate (newBuffer "child direction")
          let target=AH.agentIdText ident
          childView<-openChild runtime ident base
          let selected=draftBuffer original childView
          createNamedPipe gate 0o600
          withHeldRead server gate "accepted" $ \opened release writer->do
            submitted<-submit runtime SteerSubmit selected
            waitForReader opened
            current<-if replaced then fresh original else pure original
            hidden<-send runtime "show" [] (draftBuffer current submitted)
            putMVar release (); wait writer
            let accepted d=do
                  value<-AH.statusAgent hub AH.Human ident >>= either (error.T.unpack) pure
                  pure (field "status" value==Just ("idle"::T.Text))
            settled<-await runtime "child acceptance" accepted hidden
            restored<-await runtime "child control completion" (pure . not . agentReplying) (selectConversationView target "Receipt child" settled)
            check "child acceptance preserves a replacement draft and clears only its submitted draft"
              (if replaced then contents (composerBuffer restored)==contents original else bufferLength (composerBuffer restored)==0)
            when replaced $ do
              let (requested,effects)=runCommand AgentNew restored
              restarted<-snd <$> conversationEffects runtime (\value _->pure (False,value)) requested effects
              let childAgain=selectConversationView target "Receipt child" restarted
              retained<-sameDraftRoot restored childAgain
              check "New conversation selects the real primary editor and retains the child draft"
                (T.null (conversationTarget restarted) && activeConversation restarted && activeEditorMount restarted/=Nothing &&
                 retained && composerSelection childAgain==composerSelection restored)
          removeFile gate
    putStrLn "draft receipt checks passed"
  where restoreDraftEnvironment name=maybe (unsetEnv name) (setEnv name)
#endif

checks :: IO ()
checks = (draftReceiptChecks >> composerCodeChecks >>) $ withTextPresentation $ \presentation->
  let tickConversation=tickPresented presentation
  in bracket temporary removePathForcibly $ \root ->
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
        questionTool runtime desktop args=do
          caller<-captureQuestionCaller runtime (AR.primaryAgent (conversationAgents runtime))
          case caller of
            Left err->pure (desktop,pure (Left err))
            Right context->do
              (asked,reply)<-chatToolAs runtime (Just context) desktop "ask_user" args
              shown<-case chatQuestion asked of
                Just q | chatQuestion desktop==Nothing->await runtime "prepared question body" (\d->maybe False ((==questionToken q).questionToken.fst) (activeWindow d >>= windowQuestion d)) asked
                _->pure asked
              pure (shown,reply)
        questionId reply=reply >>= either (error . T.unpack) (maybe (error "Missing questionId") pure . (field "questionId" :: Value -> Maybe Int))
        questionPoll runtime desktop ident=questionTool runtime desktop (object ["questionId" .= ident]) >>= snd
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
        focusSource desktop=case [w | w<-windows desktop,Just bid<-[bufferId w],Just doc<-[M.lookup bid (buffers desktop)],fmap filePath (documentFile doc)==Just source] of
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
        (styledContents (renderTimestamp 20 "12:05")=="       12:05" && styledLength (renderTimestamp 3 "12:05")==3)
      let tag ident=map (\(c,s) -> (c,case s of BubbleText _ out base -> BubbleText ident out base; _ -> s))
          cells=renderReply False 54 True "one"++styledText Plain "\n\n"++renderTimestamp 54 "12:05"++styledText Plain "\n"++tag 1 (renderReply False 54 False "two")
      preparedCopy<-W.prepareSemanticTextWindow "Conversation" cells
        (W.TextSemantics (W.CopyMessages W.UserBotAttribution) Nothing Vec.empty Vec.empty W.ReadableWindow Vec.empty Vec.empty Vec.empty) >>= either (error.T.unpack) pure
      withEditorBodyFixture "" preparedCopy (initialDesktop (60,18)) $ \base->do
        let chat=draftAt (newBuffer "draft") (Selection 2 2) base
            positions ident=[(a,z) | (a,z,BubbleText j _ _)<-styleRanges cells,j==ident]
            a=fst (head (positions 0)); z=snd (last (positions 1))
            selectedReply lo hi=modifyActive (\w -> w {selection=Selection lo hi})
              (setComposerInput (composerBuffer chat) (composerSelection chat) False chat)
            copiedReply lo hi=clipboard (fst (runCommand Copy (selectedReply lo hi)))
        check "single-bubble copies omit speaker names and decoration"
          (copiedReply a (a+3)=="one" && copiedReply (a+1) (a+3)=="ne")
        check "cross-bubble copies label speakers and omit timestamps and furniture"
          (copiedReply 0 (styledLength cells)=="User: one\n\nBot: two" && copiedReply z a=="User: one\n\nBot: two")
        let w=fromMaybe (error "conversation window") (activeWindow chat)
            b=W.preparedWindowText preparedCopy
            clickAt p state=let (row,col)=windowTextPosition chat w b p in fst (handleEvent (V.EvMouseDown (left (bounds w)+1+col) (top (bounds w)+1+row) V.BLeft []) state)
            dragging=clickAt z (clickAt a chat)
            released=fst (handleEvent (V.EvMouseUp 0 0 (Just V.BLeft)) dragging)
            keyCopied=fst (handleEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) released)
        preservedDraft<-sameDraftRoot chat released
        check "dragging across bubbles focuses body copy and preserves the draft caret"
          (not (composerFocused released) && preservedDraft && composerSelection released==Selection 2 2 && clipboard keyCopied=="User: one\n\nBot: two")
    let reply width outgoing=styledContents . renderReply False width outgoing
    check "short bubbles occupy one row with outward tails"
      (reply 30 True "hello"==T.replicate 22 " "<>"▐hello▛◤" && reply 30 False "hello"=="◥▜hello▌")
    check "outgoing bubble is black on VGA cyan"
      (hasStyledChar 'h' (BubbleText 0 True Plain) (renderReply False 30 True "hello"))
    check "agent prose and code retain their styles inside bubbles"
      (hasStyledChar 'h' (BubbleText 0 False Plain) (renderReply False 30 False "hello") && hasStyledChar '4' (BubbleText 0 False (CodeStyle False Number)) (renderReply False 30 False "```haskell\nx = 42\n```"))
    forM_ [False,True] $ \outgoing -> do
      let sourceText="```haskell\nx = 42\n```"
          cells=renderReply False 40 outgoing sourceText
          firstRow=head (splitStyled cells)
          codeCell (_,BubbleText _ _ (CodeStyle _ _))=True
          codeCell _=False
          copied=T.concat [text | (text,BubbleText _ _ _)<-cells]
      check "leading code panels leave the bubble top edge clear"
        (not (any codeCell firstRow) && any codeCell cells)
      check "leading code panel spacing is decoration, not copied text"
        (copied==styledContents (renderMarkdown 35 sourceText))
    forM_ [False,True] $ \graphical -> forM_ [False,True] $ \outgoing -> forM_ [8,40,100] $ \width ->
      forM_ ["```haskell\nx = 42\n```","Before\n\n```sh\nprintf hello\n```"] $ \sourceText -> do
        let cells=renderReply graphical width outgoing sourceText
            lastRow=last (splitStyled cells)
            codeCell (_,BubbleText _ _ (CodeStyle _ _))=True
            codeCell _=False
        check "trailing code panels leave the bubble bottom edge clear"
          (not (any codeCell lastRow) && any codeCell cells)
        check "code panel margins leave copied contents unchanged"
          (T.concat [text | (text,BubbleText _ _ _)<-cells]==styledContents (renderMarkdown (width-5) sourceText))
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
    let draftBase=initialDesktop (90,30)
        isLeft (Left _)=True
        isLeft _=False
        clickAction runtime=clickActionBeforeTick runtime True
        clickActionBeforeTick runtime settle action desktop=case [(a,values) | (a,_,name,values)<-conversationActions desktop,name==action] of
          (offset,_):_ -> do
            let win=fromMaybe (error "question window") (activeWindow desktop)
                body=fromMaybe (error "question body") (windowPluginText desktop win)
                (row,column)=windowTextPosition desktop win (W.preparedWindowText body) offset
                (changed,effects)=handleEvent (V.EvMouseDown (left (bounds win)+1+column) (top (bounds win)+1+row-scrollRow win) V.BLeft []) desktop
            next<-snd <$> conversationEffects runtime fallback changed effects
            if settle then case conversationBodySnapshot (conversationTarget desktop) desktop of
              Just previous | action `elem` ["toggle-activity","toggle-tool-run"]->await runtime "expanded body" (\d->conversationBodySnapshot (conversationTarget desktop) d/=Just previous) next
              _->tickConversation runtime next
            else pure next
          _ -> error ("Missing inline action "++T.unpack action)
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
      let longReply=T.unwords (replicate 90 "window-width")
          rawShell="printf '%s\\n' 'literal λ'\n\tprintf 'tail  '  \n"
          question=longReply<>"\n\n```sh\n"<>rawShell<>"```"
      (initial,_)<-questionTool runtime (initialDesktop (80,25)) (object ["question" .= question])
      forM_ [150,32,120] $ \columns -> do
        let resized=modifyActive (\w->w {bounds=Rect 0 1 columns 23}) initial {screenSize=(columns,25)}
        (_,pending)<-conversationEffects runtime fallback resized []
        reflowed<-await runtime "resized question body" (bodyAtWidth "") pending
        let prepared=targetBody "" reflowed
            bubbleRows=[T.concat [text | (text,BubbleText _ _ _) <- row] | row<-splitStyled (bodyHighlight prepared)]
            nonempty=filter (not . T.null) bubbleRows
            blockRows=[(a,z) | (a,z,BubbleText _ _ (CodeStyle True _))<-styleRanges (bodyHighlight prepared)]
            blocks=bodyShellBlocks prepared
        check "background chat preparation adopts the resized window width"
          (maximum (0:map T.length nonempty)>columns-22 && all ((<=columns-2).T.length) nonempty)
        check "reflow preserves the whole shell source and maps its decorated cells"
          (map (\(_,_,dialect,raw)->(dialect,raw)) blocks==[("sh",rawShell)] &&
           all (\(a,z)->any (\(start,end,_,_)->a>=start && z<=end) blocks) blockRows)
        let browsing=(setComposerInput (composerBuffer reflowed) (composerSelection reflowed) False reflowed) {chatQuestion=fmap (\q->q {questionFocused=False}) (chatQuestion reflowed)}
            home=fst (runCommand (CursorDocumentStart False) browsing)
            caretReady d=maybe False ((==Nothing).conversationCaretIntent) (M.lookup "" (conversationViews d))
        first<-await runtime "logical transcript Home" caretReady home
        selected<-await runtime "logical transcript Shift End" caretReady (fst (runCommand (CursorDocumentEnd True) first))
        let (pendingCopy,copyEffects)=runCommand Copy selected
        (_,capturedCopy)<-textPresentationEffects presentation fallback pendingCopy copyEffects
        copied<-await runtime "logical transcript copy" (T.isInfixOf "window-width".clipboard) capturedCopy
        check "copy after chat reflow still excludes bubble furniture"
          ("window-width" `T.isInfixOf` clipboard copied && not ("┌" `T.isInfixOf` clipboard copied) && not ("```" `T.isInfixOf` clipboard copied))
        stable<-tickConversation runtime reflowed
        check "timer tick keeps adopted body identity stable" (conversationBodySnapshot "" stable==Just prepared)
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
      let sourceText="# Alpha界Beta Gamma界Delta Epsilon Zeta Eta Theta\n\n"<>T.unwords (replicate 80 "following")
          base=(initialDesktop (32,18)) {wideSectionTitles=True}
          caretReady d=maybe False ((==Nothing).conversationCaretIntent) (M.lookup "" (conversationViews d))
          retained d=fromMaybe (error "wide heading target") (M.lookup "" (conversationViews d))
          geometry d=case (activeWindow d,bodyViewportFor d "") of
            (Just w,Just viewport) | Just body<-windowPluginText d w->
              windowTextRows d w (W.preparedWindowText body)==Vec.length (viewportRows viewport)
            _->False
      (asked,_)<-questionTool runtime base (object ["question" .= sourceText])
      let browsing=(setComposerInput (composerBuffer asked) (composerSelection asked) False asked) {chatQuestion=fmap (\q->q {questionFocused=False}) (chatQuestion asked)}
      first<-await runtime "wide heading Home" caretReady (fst (runCommand (CursorDocumentStart False) browsing))
      let previous=conversationBodySnapshot "" first
      scrolled<-await runtime "wide heading next physical row" (\d->conversationRowShift (retained d)==0 && conversationBodySnapshot "" d/=previous) (changeScroll True 1 first)
      check "conversation wide headings budget physical rows and retain the next logical row"
        (geometry first && geometry scrolled && case conversationAnchor (retained scrolled) of
          At (QuestionPoint _ 0 scalar)->scalar>0
          _->False)
      let selected=fst (runCommand (CursorDown True) (fst (runCommand (CursorRowStart False) scrolled)))
          logicalSelection=conversationReplySelection (retained selected)
          resized=modifyActive (\w->w {bounds=(bounds w) {width=40}}) selected {screenSize=(40,18)}
      reflowed<-await runtime "wide heading anchored resize" (bodyAtWidth "") resized
      check "wide heading resize preserves logical selection and mapped paint"
        (geometry reflowed && conversationReplySelection (retained reflowed)==logicalSelection &&
          maybe False (not . null) (activeWindow reflowed >>= conversationPaintSelection reflowed))
      let draftCopy=draftAt (newBuffer "draft-copy") (Selection 0 10) reflowed
          (copiedDraft,draftEffects)=runCommand Copy draftCopy
          answerCopy=reflowed {chatQuestion=fmap (\q->q {questionFocused=True,questionChoice=Nothing,
            questionBuffer=newBuffer "answer-copy",questionSelection=Selection 0 11}) (chatQuestion reflowed)}
          (copiedAnswer,answerEffects)=runCommand Copy answerCopy
      check "retained transcript selection does not steal draft or question copy"
        (clipboard copiedDraft=="draft-copy" && null draftEffects && clipboard copiedAnswer=="answer-copy" && null answerEffects)
      let pageOnce=fst (runCommand (CursorPageDown True) reflowed)
          pageTwice=fst (runCommand (CursorPageDown True) pageOnce)
      check "pending page movement retains repeated input"
        (conversationRowShift (retained pageOnce)>0 && conversationRowShift (retained pageTwice)==2*conversationRowShift (retained pageOnce))
      paged<-await runtime "wide heading Shift Page Down" caretReady pageTwice
      check "page movement preserves an offscreen logical extension anchor"
        (case (logicalSelection,conversationReplySelection (retained paged)) of
          (Just (BodySelection a _),Just (BodySelection b z))->a==b && z>b
          _->False)
    forM_ [32,120,150] $ \columns -> do
      let outgoing=renderReply False columns True (T.unwords (replicate 90 "window-width"))
          incoming=renderReply False columns False (T.unwords (replicate 90 "window-width"))
          firstRow=head . splitStyled
      check "wide user bubbles anchor on the right and replies on the left"
        (styledLength (firstRow outgoing)==columns && maybe False (\(_,style)->case style of BubbleText _ False _->False; _->True) (listToMaybe incoming) &&
         maximum (map (styledLength . filter (\(_,style)->case style of BubbleText{}->True; _->False)) (splitStyled incoming))>columns-20)
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
      preparedDraft<-send runtime "show" [] draftBase
      let savedDraft=draftAt (newBuffer "existing draft") (Selection 4 4) preparedDraft
      agents <- send runtime "directory" [] savedDraft
      check "Agents directory exposes an explicit reconnect action"
        (maybe False (elem "Reconnect" . buttons) (dialog agents))
      recovered<-recoveredBody "Recovered user and agent transcript" savedDraft
      idle<-tickConversation runtime recovered
      resized<-tickConversation runtime idle {screenSize=(100,35)}
      sourceSame<-sameBufferVersions recovered resized
      draftSame<-sameDraftRoot recovered resized
      check "idle fresh conversation runtime preserves recovered transcript and draft"
        (sourceSame && draftSame && conversationText resized=="Recovered user and agent transcript" && composerSelection resized==composerSelection recovered)
      shown<-send runtime "show" [] resized
      shownSources<-sameBufferVersions recovered shown
      shownDraft<-sameDraftRoot recovered shown
      check "opening a recovered conversation preserves its transcript and draft"
        (shownSources && shownDraft && conversationText shown=="Recovered user and agent transcript" && map windowId (windows shown)==map windowId (windows recovered) && composerSelection shown==composerSelection recovered && composerFocused shown)
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
      preparedDraft<-send runtime "show" [] draftBase
      let savedDraft=draftAt (newBuffer "existing draft") (Selection 4 4) preparedDraft
      (asked,reply)<-questionTool runtime savedDraft (object ["question" .= ("Question presentation identity"::T.Text),"choices" .= (["First","Second"]::[T.Text])])
      _<-questionId reply
      let q=fromMaybe (error "Missing presentation question") (chatQuestion asked)
          answer=(newBuffer "kept answer") {saved=error "Question presentation forced saved answer",
            undoStack=error "Question presentation forced answer Undo",redoStack=error "Question presentation forced answer Redo"}
          poisoned=asked {chatQuestion=Just q {questionBuffer=answer}}
          questionBody d=targetBody "" d
      first<-tickConversation runtime poisoned
      let originalIdentity=questionBody first
      unchanged<-tickConversation runtime first
      let unchangedIdentity=questionBody unchanged
      check "unchanged question redraw never compares retained answer history"
        (originalIdentity==unchangedIdentity && map scrollRow (windows first)==map scrollRow (windows unchanged))
      let replacement=newBuffer "fresh answer"
          replaced=unchanged {chatQuestion=Just q {questionBuffer=replacement}}
      redrawn<-tickConversation runtime replaced
      let replacementIdentity=questionBody redrawn
      check "equal-revision answer replacement paints live input without rebuilding history"
        (revision answer==revision replacement && originalIdentity==replacementIdentity &&
          "fresh answer" `T.isInfixOf` snapshot redrawn && not ("kept answer" `T.isInfixOf` snapshot redrawn))
      selected<-clickAction runtime "question-choice" redrawn
      let selectedIdentity=questionBody selected
      check "direct choice click paints its marker without rebuilding history"
        (selectedIdentity==originalIdentity && "(*) First" `T.isInfixOf` snapshot selected)
      custom<-clickAction runtime "question-input" selected
      let entered=custom {chatQuestion=fmap (\value->value {questionBuffer=newBuffer "界λ",questionSelection=Selection 2 2}) (chatQuestion custom)}
      live<-tickConversation runtime entered
      let win=fromMaybe (error "Missing projected window") (activeWindow live)
      case questionInputGeometry live win of
        Nothing->error "Missing live question geometry"
        Just (rect,_,_)->do
          let (clicked,effects)=handleEvent (V.EvMouseDown (left rect+2) (top rect) V.BLeft []) live
          hit<-snd <$> conversationEffects runtime fallback clicked effects
          check "live wide answer shares cursor, hit and private cell geometry"
            (renderCursor live==V.Cursor (left rect+3) (top rect) &&
              maybe False ((==Selection 1 1).questionSelection) (chatQuestion hit) &&
              "界λ" `T.isInfixOf` snapshot live && all (\x->not (readableAt live x (top rect))) [left rect..left rect+2])
          let marked=live {chatQuestion=fmap (\value->value {questionBuffer=newBuffer (T.replicate 33 "\x301"<>"private"),questionSelection=Selection 0 0}) (chatQuestion live)}
          font<-loadFont
          captured<-ScreenCapture.capture font marked False >>= either (error . T.unpack) pure
          let capturedText=do
                blocks<-field "content" captured
                block<-listToMaybe blocks
                encoded<-field "text" block
                metadata<-decodeStrict' (TE.encodeUtf8 encoded)
                field "text" metadata
          let guardCell text=T.index (T.lines text!!top rect) (left rect-1)
          check "capture hides leading answer fragments joined to the label guard cell"
            (guardCell (snapshot marked {streamerMode=True})=='�' &&
              not (readableAt marked (left rect-1) (top rect)) &&
              maybe False (\text->guardCell text==' ' && not ("private" `T.isInfixOf` text)) capturedText)
          let inputRow=top rect-top (bounds win)-1+scrollRow win
              oneRow=ensureQuestionVisible (modifyActive (\w->w {bounds=(bounds w) {height=4},scrollRow=inputRow+1}) live)
              shortWindow=fromMaybe (error "Missing one-row question window") (activeWindow oneRow)
          check "one-row body reveals the actual answer before Submit"
            (pluginBodyRows oneRow shortWindow==1 &&
              case questionInputGeometry oneRow shortWindow of
                Just (input,_,_)->top input==top (bounds shortWindow)+1
                _->False)
          let spacer=top (bounds win)+1+pluginBodyRows live win
              (_,outside)=handleEvent (V.EvMouseDown (left rect) spacer V.BLeft []) live
          check "question controls do not hit the reserved composer spacer"
            (not (any (\effect->case effect of AgentAction action _->"question-" `T.isPrefixOf` action; _->False) outside))
      replacementBody<-W.prepareTextWindow "Replacement" "different immutable body"
      let reference=fromMaybe (error "missing body ref") (M.lookup "" (conversationViews live) >>= conversationBodyRef)
          restyled=live {pluginWindows=M.insert reference replacementBody (pluginWindows live)}
      check "restyled body cannot reuse question projection or accept stale input"
        (case questionInputGeometry live win of
          Just (rect,_,_)->case (windowQuestion restyled win,conversationClick (left rect) (top rect) win restyled) of
            (Nothing,Nothing)->True
            _->False
          _->False)
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
      preparedDraft<-send runtime "show" [] draftBase
      let savedDraft=draftAt (newBuffer "existing draft") (Selection 4 4) preparedDraft
      (_,anonymous)<-chatTool runtime savedDraft "ask_user" (object ["question" .= ("Anonymous question"::T.Text)])
      refused<-timeout 100000 anonymous
      check "anonymous ask_user cannot acquire a private answer" (case refused of Just (Left _)->True; _->False)
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
      preparedDraft<-send runtime "show" [] draftBase
      let savedDraft=draftAt (newBuffer "existing draft") (Selection 4 4) preparedDraft
      (asked,answer)<-questionTool runtime savedDraft (object ["question" .= ("Pick a direction"::T.Text),"choices" .= (["Left","Right"]::[T.Text])])
      check "ask_user renders inline choices and custom entry without a modal" (dialog asked==Nothing && chatQuestion asked/=Nothing && all (`T.isInfixOf` conversationText asked) ["Pick a direction","Left","Right","Other:","Submit answer","Cancel"])
      check "question footer describes answer actions" ("Enter Answer" `T.isInfixOf` snapshot asked && not ("Session: not connected" `T.isInfixOf` conversationText asked))
      draftKept<-sameDraftRoot savedDraft asked
      check "ask_user preserves the existing draft and caret" (draftKept && composerSelection asked==composerSelection savedDraft)
      (duplicate,refused)<-questionTool runtime asked (object ["question" .= ("Another?"::T.Text)])
      check "only one human question can wait" . (&& (fmap questionToken (chatQuestion duplicate)==fmap questionToken (chatQuestion asked))) . isLeft =<< refused
      created<-timeout 100000 answer
      ident<-case created of Just (Right value)->maybe (error "Missing immediate questionId") pure (field "questionId" value :: Maybe Int); _->error "ask_user did not return pending immediately"
      check "question creation is immediately pending" (case created of Just (Right value)->field "status" value==Just ("pending"::T.Text); _->False)
      (_,anonymousPoll)<-chatTool runtime asked "ask_user" (object ["questionId" .= ident])
      check "anonymous callers cannot retrieve authenticated questions" . isLeft =<< anonymousPoll
      foreignCaller<-captureQuestionCaller runtime (AH.AgentId "unrelated-agent")
      check "another actor cannot acquire a private question caller receipt" (case foreignCaller of Left _->True; _->False)
      selected<-clickAction runtime "question-choice" asked
      check "choice click waits for explicit submit" (maybe False ((==Just 0).questionChoice) (chatQuestion selected))
      pending<-questionPoll runtime selected ident
      check "pending result excludes choice and draft" (pending==Right (object ["questionId" .= ident,"status" .= ("pending"::T.Text)]))
      (_,mixed)<-questionTool runtime selected (object ["questionId" .= ident,"question" .= ("replacement"::T.Text)])
      check "question retrieval cannot also replace the question" . isLeft =<< mixed
      submitted<-clickAction runtime "question-submit" selected
      choiceReply<-questionPoll runtime submitted ident
      check "choice response is retained for the requesting actor" (case choiceReply of Right value->field "status" value==Just ("answered"::T.Text) && field "answer" value==Just ("Left"::T.Text) && field "custom" value==Just False; _->False)
      repeated<-questionPoll runtime submitted ident
      check "polling is stable and does not duplicate the transcript" (repeated==choiceReply && T.count "Pick a direction" (conversationText submitted)==1)
      draftAfterAnswer<-sameDraftRoot savedDraft submitted
      check "answer removes the inline form and preserves draft" (chatQuestion submitted==Nothing && draftAfterAnswer && composerSelection submitted==composerSelection savedDraft)
      (custom,customReply)<-questionTool runtime savedDraft (object ["question" .= ("Your answer?"::T.Text)])
      customId<-questionId customReply
      let typed=foldl (\desktop c->fst (handleEvent (V.EvKey (V.KChar c) []) desktop)) custom ("custom λ"::String)
          (sending,effects)=handleEvent (V.EvKey V.KEnter []) typed
      stillPending<-questionPoll runtime typed customId
      check "typed draft stays private until submitted" (stillPending==Right (object ["questionId" .= customId,"status" .= ("pending"::T.Text)]))
      sent<-snd <$> conversationEffects runtime fallback sending effects
      result<-questionPoll runtime sent customId
      draftAfterText<-sameDraftRoot savedDraft sent
      check "free text answers preserve Unicode and the ordinary draft" (case result of Right value->field "answer" value==Just ("custom λ"::T.Text) && draftAfterText; _->False)
      (cancelledQuestion,cancelledReply)<-questionTool runtime savedDraft (object ["question" .= ("Cancel me"::T.Text)])
      cancelId<-questionId cancelledReply
      cancelledDesktop<-clickAction runtime "question-cancel" cancelledQuestion
      cancelledResult<-questionPoll runtime cancelledDesktop cancelId
      check "inline Cancel stores an explicit terminal result" (chatQuestion cancelledDesktop==Nothing && case cancelledResult of Right value->field "status" value==Just ("cancelled"::T.Text) && (field "answer" value :: Maybe T.Text)==Nothing; _->False)
      (disconnected,disconnectedReply)<-questionTool runtime savedDraft (object ["question" .= ("Requester leaves"::T.Text)])
      disconnectedId<-questionId disconnectedReply
      cleaned<-send runtime "cancel" [] disconnected
      ended<-questionPoll runtime cleaned disconnectedId
      check "explicit conversation cancellation retires the pending question" (chatQuestion cleaned==Nothing && case ended of Right value->field "status" value==Just ("cancelled"::T.Text); _->False)
      forM_ [1..65::Int] $ \_->do
        (fresh,reply)<-questionTool runtime savedDraft (object ["question" .= ("Bounded result"::T.Text)])
        _<-questionId reply
        _<-clickAction runtime "question-cancel" fresh
        pure ()
      expired<-questionPoll runtime cleaned ident
      check "old terminal results explicitly expire" (case expired of Left message->"expired" `T.isInfixOf` message; _->False)
      let narrow=initialDesktop (40,12)
      (manyChoices,_)<-questionTool runtime narrow (object ["question" .= ("Choose"::T.Text),"choices" .= (T.replicate 100 "z":["Option "<>T.pack (show n) | n<-[1..11::Int]])])
      visibleChoice<-tickConversation runtime (fst (handleEvent (V.EvKey V.KDown []) manyChoices))
      let active=fromMaybe (error "question window") (activeWindow visibleChoice)
          body=targetBody "" visibleChoice
          choiceRows=[fst (windowTextPosition visibleChoice active (W.preparedWindowText body) a) | (a,_,action,values)<-conversationActions visibleChoice,action=="question-choice",last values=="0"]
      check "keyboard choices stay visible in a small conversation window" (case choiceRows of row:_->row>=scrollRow active && row<scrollRow active+pluginBodyRows visibleChoice active; _->False)
      check "long choice labels wrap without being discarded" (T.count "z" (conversationText visibleChoice)==100)
      customInput<-tickConversation runtime (fst (handleEvent (V.EvKey V.KUp []) visibleChoice))
      let browsing=modifyActive (\w->w {scrollRow=0}) customInput
      retainedScroll<-tickConversation runtime browsing
      check "idle question overlay preserves manual history scrolling"
        (maybe False ((==0).scrollRow) (activeWindow retainedScroll))
      typedInput<-tickConversation runtime (fst (handleEvent (V.EvKey (V.KChar 'x') []) retainedScroll))
      let inputWindow=fromMaybe (error "question input window") (activeWindow typedInput)
          inputBody=targetBody "" typedInput
          inputSame=body==inputBody
      check "typing reveals an already-focused answer without reflowing history"
        (inputSame && case questionInputGeometry typedInput inputWindow of
          Just (rect,_,_)->top rect>top (bounds inputWindow) && top rect<top (bounds inputWindow)+1+pluginBodyRows typedInput inputWindow
          _->False)
      smallCancelled<-clickAction runtime "question-cancel" typedInput
      check "small-window custom input leaves Cancel reachable" (chatQuestion smallCancelled==Nothing)
      (_,invalid)<-questionTool runtime savedDraft (object ["question" .= ("Unsupported"::T.Text),"allowMultiple" .= True])
      check "unsupported multi-select is explicit" . isLeft =<< invalid
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime->Permissions.withPermissionsAt (root </> "question-permissions.toml") chatTools $ \permissions->do
      preparedDraft<-send runtime "show" [] draftBase
      let savedDraft=draftAt (newBuffer "existing draft") (Selection 4 4) preparedDraft
      bound<-captureQuestionCaller runtime (AR.primaryAgent (conversationAgents runtime)) >>= either (error . T.unpack) pure
      let primary=AR.primaryAgent (conversationAgents runtime)
          actor=fmap (() <$) (AH.statusAgent (AR.agentHub (conversationAgents runtime)) (AH.Agent primary) primary)
          operation=chatToolAs runtime (Just bound)
          call=settledTool permissions (Permissions.permissionCallAs actor permissions operation)
          approve desktop=case dialog desktop of
            Just dg->let (next,effects)=submitDialog 0 dg desktop in snd <$> Permissions.policyEffects permissions fallback next effects >>= settleDialog permissions dg
            Nothing->error "Missing question permission review"
      (review,creation)<-call savedDraft "ask_user" (object ["question" .= ("One permission per question"::T.Text)])
      check "question creation retains ordinary permission approval" (dialog review/=Nothing && chatQuestion review==Nothing)
      withAsync creation $ \request->do
        admitted<-approve review
        ident<-questionId (wait request)
        (unchanged,pending)<-call admitted "ask_user" (object ["questionId" .= ident])
        result<-timeout 100000 pending
        check "owned pending retrieval does not create another human approval"
          (dialog unchanged==Nothing && chatQuestion unchanged==chatQuestion admitted && case result of Just (Right value)->field "status" value==Just ("pending"::T.Text); _->False)
        writeFile (root </> "question-permissions.toml") "[editor.mcp.permissions]\nask_user = 'disable'\n"
        (_,disabled)<-call admitted "ask_user" (object ["questionId" .= ident])
        check "Disable still refuses owned question retrieval" . isLeft =<< disabled
        _<-send runtime "cancel" [] admitted
        pure ()
      writeFile (root </> "question-permissions.toml") ""
      (review,creation)<-call savedDraft "ask_user" (object ["question" .= ("Revoked caller"::T.Text)])
      _<-AH.endAgent (AR.agentHub (conversationAgents runtime)) AH.Human primary
      withAsync creation $ \request->do
        refused<-approve review
        result<-wait request
        check "deferred approval cannot resurrect an ended question requester" (chatQuestion refused==Nothing && isLeft result)
    (closedRuntime,closedId)<-C.withConsoles $ \consoles -> withConversation consoles $ \runtime->do
      preparedDraft<-send runtime "show" [] draftBase
      let savedDraft=draftAt (newBuffer "existing draft") (Selection 4 4) preparedDraft
      (_,reply)<-questionTool runtime savedDraft (object ["question" .= ("Session closes"::T.Text)])
      ident<-questionId reply
      pure (runtime,ident)
    check "session shutdown refuses further question retrieval" . isLeft =<< questionPoll closedRuntime draftBase closedId
    BS.writeFile server (TE.encodeUtf8 (T.pack providerScript))
    BS.writeFile source "disk original\n"
    BS.writeFile secondSource "second original\n"
    (secondFile,secondBuffer)<-loadFile secondSource >>= either error pure
    (file,b)<-loadFile source >>= either error pure
    let desktop=insertText "unsaved " (addDocument (Just file) b (addDocument (Just secondFile) secondBuffer (initialDesktop (90,28))) {sideTree=Just (emptySidebar root 20 False)})
    -- Services belong to the editor session, not its mounted ACP provider.
    -- Use the same real peer/approval route as the terminal checks below.
    when (terminalAvailable && os/="mingw32") $ withSessionServices $ \services->do
      let consoles=sessionConsoles services
          jobs=sessionBuildJobs services
          base=(initialDesktop (90,28)) {defaultDirectory=Just root}
          serviceCore=sessionEffects services App.applyEffects
          awaitService label pump predicate current=timeout 8000000 (loop current) >>= maybe (error ("Session ownership: "++label)) pure
            where loop d=do
                    next<-pump d
                    ready<-predicate next
                    if ready then pure next else threadDelay 10000 >> loop next
      sharedJob<-Jobs.startBuildJob jobs "Session fixture" root
        [("python3",["-u","-c","import time; print('shared captured output',flush=True); time.sleep(30)"])] base
      (sharedId,sharedDesktop)<-C.startConsole consoles
        (Terminal.TerminalConfig "python3" ["-u","-c","import time; print('shared terminal output',flush=True); time.sleep(30)"] [] root 80 24)
        65536 sharedJob >>= either (error.T.unpack) pure
      warm<-awaitService "shared output" (tickSessionServices services) (\_->do
        (out,_)<-Jobs.buildJobStdout jobs
        terminal<-C.consoleOutput consoles sharedId
        pure ("shared captured output" `T.isInfixOf` out && either (const False) (\(bytes,_,_)->"shared terminal output" `BS.isInfixOf` bytes) terminal)) sharedDesktop
      before<-Jobs.buildJobStatus jobs warm
      jobId<-maybe (error "Missing shared job ID") pure (field "jobId" before :: Maybe T.Text)
      (providerId,retired)<-withConversationAt consoles root $ \runtime->do
        configured<-configure runtime ("yes"::T.Text) warm
        approval<-prompt runtime "terminal-hold" configured >>= modal runtime
        accepted<-actDialog runtime 0 approval
        let pump d=tickSessionServices services d >>= tickConversation runtime
        owned<-awaitService "approved ACP terminal" pump (\_->do
          entries<-C.listConsoles consoles
          pure (any (\(ident,_,_)->ident/=sharedId) entries)) accepted
        entries<-C.listConsoles consoles
        providerId<-case [ident | (ident,_,_)<-entries,ident/=sharedId] of
          [ident]->pure ident
          _->error "Expected one ACP-owned terminal"
        pure (providerId,owned)
      entries<-C.listConsoles consoles
      after<-Jobs.buildJobStatus jobs retired
      check "provider retirement releases only its own terminal and preserves shared job"
        (all (\(ident,_,_)->ident/=providerId) entries && any (\(ident,_,code)->ident==sharedId && code==Nothing) entries &&
         field "jobId" after==Just jobId && field "active" after==Just True)
      let closeView d ident=fst (runCommand Close (focusWindow ident d))
          outputWindows=[windowId w | w<-windows retired,case windowContent w of PluginContent{}->True; _->False]
          terminalWindows=[windowId w | w<-windows retired,Just doc<-[windowDocument (buffers retired) w],documentLabel doc==Just ("Terminal "<>sharedId)]
          closed=foldl closeView retired (outputWindows++terminalWindows)
      stayedClosed<-tickSessionServices services closed
      output<-Jobs.buildJobStdout jobs
      terminal<-C.consoleOutput consoles sharedId
      check "closed shared views retain exact output without reopening"
        (all (`notElem` map windowId (windows stayedClosed)) (outputWindows++terminalWindows) &&
         "shared captured output" `T.isInfixOf` fst output && either (const False) (\(bytes,_,_)->"shared terminal output" `BS.isInfixOf` bytes) terminal)
      stopping<-stopSessionBuild services stayedClosed
      stopped<-awaitService "shared job Stop" (tickSessionServices services)
        (\d->(==Just False) . field "active" <$> Jobs.buildJobStatus jobs d) stopping
      let compiler=root </> "session-compiler"
      writeFile compiler "#!/bin/sh\nprintf 'build without conversation\\n'\n"
      permissions<-getPermissions compiler
      setPermissions compiler permissions {executable=True}
      _<-persist (sessionDirectory services </> "run.json")
        (Build.buildConfigValue root (Build.BuildConfig THC compiler "" "" "" [])) >>= either (error.T.unpack) pure
      queued<-snd <$> serviceCore stopped [ServiceAction "make" []]
      completed<-awaitService "build without Conversation" (\d->tickSessionServices services d >>= tickBuildPreparation services serviceCore)
        (\d->do facts<-Jobs.buildJobStatus jobs d; (text,_)<-Jobs.buildJobStdout jobs
                pure (field "jobId" facts/=Just jobId && field "active" facts==Just False && "build without conversation" `T.isInfixOf` text)) queued
      check "session build runs after provider retirement" (dialog completed==Nothing)
      _<-C.releaseConsole consoles sharedId
      removeFile (sessionDirectory services </> "run.json")
      pure ()
    -- A submitted answer resumes the original idle provider through its existing
    -- query owner. A replacement with the same provider session ID cannot replay it.
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
      configured<-configure runtime ("yes"::T.Text) desktop
      connected<-prompt runtime "stream" configured >>= done runtime
      let liveDraft=draftAt (newBuffer "newer independent draft") (Selection 6 6) connected
      (asked,creation)<-questionTool runtime liveDraft (object ["question" .= ("Resume the idle provider?"::T.Text),"choices" .= (["async-answer-resume-marker"]::[T.Text])])
      ident<-questionId creation
      submitted<-clickAction runtime "question-submit" =<< clickAction runtime "question-choice" asked
      resumed<-done runtime submitted
      messages<-logged
      let answerPrompts=[params | entry<-messages,field "method" entry==Just ("session/prompt"::T.Text),Just params<-[field "params" entry::Maybe Value],"async-answer-resume-marker" `T.isInfixOf` json params]
      check "explicit answer wakes the original idle provider once with question attribution"
        (length answerPrompts==1 && all (T.isInfixOf ("question "<>T.pack (show ident)).json) answerPrompts && contents (composerBuffer resumed)=="newer independent draft")
      result<-questionPoll runtime resumed ident
      check "delivered answer stays available without another transcript record"
        (case result of Right value->field "answer" value==Just ("async-answer-resume-marker"::T.Text) && T.count "Resume the idle provider?" (conversationText resumed)==1; _->False)
      (held,reply)<-questionTool runtime resumed (object ["question" .= ("Do not replay"::T.Text),"choices" .= (["async-retired-answer-marker"]::[T.Text])])
      oldId<-questionId reply
      originalCaller<-captureQuestionCaller runtime (AR.primaryAgent (conversationAgents runtime)) >>= either (error . T.unpack) pure
      queued<-clickActionBeforeTick runtime False "question-submit" =<< clickAction runtime "question-choice" held
      replacement<-send runtime "new" [] queued >>= await runtime "replacement provider" ((=="Session fixture-session").status)
      (_,oldCreation)<-chatToolAs runtime (Just originalCaller) replacement "ask_user" (object ["question" .= ("Old queued request"::T.Text)])
      check "old queued admission cannot bind to a replacement provider" . isLeft =<< oldCreation
      stale<-questionPoll runtime replacement oldId
      check "same session label on a new connection cannot retrieve an old answer"
        (case stale of Left message->"expired" `T.isInfixOf` message; _->False)
      settled<-foldM (\value _->threadDelay 1000 >> tickConversation runtime value) replacement [1..20::Int]
      entries<-logged
      check "retired queued answer never prompts the replacement provider"
        (not (any (T.isInfixOf "async-retired-answer-marker".json) entries) && agentQueued settled==0)
      (pending,_)<-questionTool runtime settled (object ["question" .= ("Pending lifetime"::T.Text)])
      retired<-send runtime "new" [] pending >>= await runtime "pending owner retirement" ((=="Session fixture-session").status)
      check "provider retirement removes an unanswered question without creating an answer" (chatQuestion retired==Nothing)
#ifndef mingw32_HOST_OS
    -- Hold an actual filesystem read open while the provider sends more work.
    -- The desktop must keep ticking, and a later write cannot bless user edits
    -- made while its request was waiting behind that read.
    let pipe=root </> "slow-source"
    createNamedPipe pipe 0o600
    withHeldRead server pipe "held read\n" $ \opened release writer -> C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
        configured<-configure runtime ("yes"::T.Text) desktop
        started<-prompt runtime "slow-files" configured
        responsive<-await runtime "provider update behind held file read"
          (T.isInfixOf "file requests sent" . conversationText) started
        waitForReader opened
        let edited=insertText "during read " (focusSource responsive)
        composing<-send runtime "show" [] edited
        let typed=fst (handleEvent (V.EvPaste "draft stays responsive") (setComposerInput (composerBuffer composing) (composerSelection composing) True composing))
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
      withHeldRead server pipe "held read\n" $ \held releaseCapture writer -> C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
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
        closed<-timeout 3000000 $ C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
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
    withHeldRead server runSettings "{\"toolchain\":\"GHC\"}" $ \opened release writer -> withSessionServices $ \runtime -> do
        ready<-timeout 1000000 (tickSessionServices runtime desktop)
        responsive<-maybe (error "Idle tick blocked on build settings read") pure ready
        -- The child acknowledges an actual reader, never just its own writer.
        -- Keep polling if discovery reached the FIFO before the child opened it.
        let reader current=do
              next<-tickSessionServices runtime current
              pending<-isEmptyMVar opened
              if pending then threadDelay 10000 >> reader next else pure next
        held<-timeout 3000000 (reader responsive) >>= maybe (error "Settings reader did not acquire held FIFO") pure
        waitForReader opened
        next<-timeout 1000000 (tickSessionServices runtime held)
        check "repeated ticks do not join or duplicate blocked settings discovery" (maybe False (const True) next)
        putMVar release ()
        wait writer
        let loop current=do
              next<-tickSessionServices runtime current
              if toolchain next==Just GHC then pure next else threadDelay 10000 >> loop next
        _<-timeout 8000000 (loop held) >>= maybe (error "asynchronous persisted toolchain timed out") pure
        pure ()
    removeFile runSettings
    -- Context reads happen before enqueueing a prompt. Holding one must leave
    -- the draft editable, and cancelling it must never send the stale prompt.
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
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
          fast<-timeout 1000000 (submit runtime QuerySubmit (draftBuffer (newBuffer "cancel before send") connected))
          preparing<-maybe (error "Context preparation blocked send/input") pure fast
          check "draft remains while context is preparing" (contents (composerBuffer preparing)=="cancel before send")
          next<-timeout 1000000 (tickConversation runtime preparing)
          responsive<-maybe (error "Context preparation blocked tick") pure next
          let edited=draftBuffer (newBuffer "newer human draft") responsive
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
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
      configured<-configure runtime ("yes"::T.Text) desktop
      check "configuration saved to isolated XDG directory" =<< doesFileExist (settings </> "agents.json")
      streamed<-prompt runtime "stream" configured >>= done runtime >>= await runtime "adopted streamed body" (\d->"[completed] Local tool" `T.isInfixOf` conversationText d && "Hello" `T.isInfixOf` conversationText d)
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
      check "conversation preserves Markdown styling" (any ((==BubbleText 1 False (BoldStyle Keyword)).snd) (conversationHighlight streamed))
      check "tool update merges pending record" (T.count "[completed] Local tool" (conversationText streamed)==1 && not ("[pending] Local tool" `T.isInfixOf` conversationText streamed))
      check "tool activity starts collapsed without raw arguments" (not ("rawInput" `T.isInfixOf` conversationText streamed) && "▸" `T.isInfixOf` conversationText streamed)
      let activityAction=case [values | (_,_,name,values)<-conversationActions streamed,name=="toggle-activity"] of values:_->values; _->error "missing activity action"
      expanded<-clickAction runtime "toggle-activity" streamed
      check "expanded activity retains original request and response JSON" (all (`T.isInfixOf` conversationText expanded) ["rawInput","rawOutput","original argument","exact response"])
      let selectedActivity=modifyActive (\w->w {selection=Selection 0 (maybe 0 (contentLength.W.preparedWindowText) (conversationBodySnapshot "" expanded))}) expanded
          selectedTextOnly=clipboard (fst (runCommand Copy selectedActivity))
      check "conversation copies omit activity chevrons and raw JSON" (not ("rawInput" `T.isInfixOf` selectedTextOnly) && not ("▾" `T.isInfixOf` selectedTextOnly) && "Hello" `T.isInfixOf` selectedTextOnly)
      collapsed<-send runtime "toggle-activity" activityAction expanded >>= await runtime "adopted expansion" (\d->conversationBodySnapshot "" d/=conversationBodySnapshot "" expanded)
      check "activity collapses without changing prose" (conversationText collapsed==conversationText streamed)
      grouped<-prompt runtime "tool-run" collapsed >>= done runtime >>= await runtime "adopted tool run" (T.isInfixOf "▸▸ 3 tool calls · 1 failed".conversationText)
      let groupActions d=[values | (_,_,name,values)<-conversationActions d,name=="toggle-tool-run"]
          singleActions d=[values | (_,_,name,values)<-conversationActions d,name=="toggle-activity"]
      check "consecutive calls collapse to one double chevron with visible failures"
        (length (groupActions grouped)==1 && "▸▸ 3 tool calls · 1 failed" `T.isInfixOf` conversationText grouped &&
         length (singleActions grouped)==2 && not ("run argument" `T.isInfixOf` conversationText grouped))
      openedGroup<-clickAction runtime "toggle-tool-run" grouped
      check "expanding a run reveals tightly stacked individual calls"
        (length (singleActions openedGroup)==5 && "▾▾ 3 tool calls" `T.isInfixOf` conversationText openedGroup &&
         "[completed] Read files\n  ▸ [completed] Run tests\n  ▸ [failed] Check output" `T.isInfixOf` conversationText openedGroup)
      let firstRunCall=case drop 1 (singleActions openedGroup) of values:_->values; _->error "missing grouped call"
          runAction d=case groupActions d of values:_->values; _->error "missing run toggle"
      detailGroup<-send runtime "toggle-activity" firstRunCall openedGroup >>= await runtime "adopted expansion" (\d->conversationBodySnapshot "" d/=conversationBodySnapshot "" openedGroup)
      check "group members still expose exact request and reply details"
        (all (`T.isInfixOf` conversationText detailGroup) ["run argument","run result"])
      foldedGroup<-send runtime "toggle-tool-run" (runAction detailGroup) detailGroup >>= await runtime "adopted expansion" (\d->conversationBodySnapshot "" d/=conversationBodySnapshot "" detailGroup)
      check "folding a run hides every member and its expanded JSON"
        (conversationText foldedGroup==conversationText grouped)
      reopenedGroup<-send runtime "toggle-tool-run" (runAction foldedGroup) foldedGroup >>= await runtime "adopted expansion" (\d->conversationBodySnapshot "" d/=conversationBodySnapshot "" foldedGroup)
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
      let sized text=composerRect (draftBuffer (newBuffer text) cancelled) window
          edge r=left r+width r
          available=width (bounds window)-6
          wide=T.replicate 10 "界"<>"\nshort"
          wideDesktop=draftAt (newBuffer wide) (Selection 0 0) cancelled
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
      forM_ [False,True] $ \replying -> forM_ [False,True] $ \supported -> do
        let visible=multiline {agentSteering=supported,agentReplying=replying}
        check "steering shortcut stays visible while idle or replying"
          ("Ctrl+Enter Steer" `T.isInfixOf` snapshot visible)
      let (options,_) = runCommand ChatInputOptions multiline
          optionsDialog=maybe (error "missing chat input settings") id (dialog options)
          selectedOptions=optionsDialog {fields=[Radio "Enter action" ["Query","Steer"] 1]}
          (selectedInput,saveInput)=submitDialog 0 selectedOptions options
          (cancelledInput,cancelEffects)=submitDialog 1 selectedOptions options
      check "Options chat input selects and persists the human default"
        (chatSubmit selectedInput==SteerSubmit && saveInput==[SaveChatSubmit SteerSubmit] && dialog selectedInput==Nothing &&
         chatSubmit cancelledInput==QuerySubmit && null cancelEffects && any (\(_,_,items)->any (\(MenuItem label _ command)->label=="Chat input..." && command==ChatInputOptions) items) menus)
      withEditorFixture "child-fixture" multiline $ \fixtures->do
        forM_ [QuerySubmit,SteerSubmit] $ \submitChoice -> forM_ [False,True] $ \replying -> forM_ ["","child-fixture"] $ \target -> do
          let chatConfigured=(if T.null target then selectConversationView "" "Primary" fixtures else selectConversationView target "Child" fixtures) {chatSubmit=submitChoice,agentReplying=replying,agentSteering=False,childAgentSteering=False}
              queryMods=if submitChoice==QuerySubmit then [] else [V.MCtrl]
              steerMods=if submitChoice==SteerSubmit then [] else [V.MCtrl]
              queryHint=(if submitChoice==QuerySubmit then "Enter " else "Ctrl+Enter ")<>(if replying then "Queue query" else "Query")
              steerHint=(if submitChoice==SteerSubmit then "Enter " else "Ctrl+Enter ")<>"Steer"
          check "both shortcut labels follow the primary or child composer default"
            (queryHint `T.isInfixOf` snapshot chatConfigured && steerHint `T.isInfixOf` snapshot chatConfigured)
          check "Enter and Ctrl+Enter invoke the advertised opposite actions"
            (snd (handleEvent (V.EvKey V.KEnter queryMods) chatConfigured)==[editorEffect QuerySubmit chatConfigured] &&
             snd (handleEvent (V.EvKey V.KEnter steerMods) chatConfigured)==[editorEffect SteerSubmit chatConfigured] &&
             snd (clickStatus (if replying then "Queue query" else "Query") chatConfigured)==[editorEffect QuerySubmit chatConfigured] && snd (clickStatus "Steer" chatConfigured)==[editorEffect SteerSubmit chatConfigured])
          forM_ [[V.MShift],[V.MCtrl,V.MShift]] $ \mods -> do
            let (newline,effects)=handleEvent (V.EvKey V.KEnter mods) chatConfigured
            check "Shift+Enter always inserts newline without sending" (null effects && bufferLength (composerBuffer newline)==bufferLength (composerBuffer chatConfigured)+1)
      check "status steering sends the advertised action"
        (snd (clickStatus "Steer" (multiline {agentSteering=True,agentReplying=True}))==[editorEffect SteerSubmit multiline])
      submitted<-uncurry (conversationEffects runtime fallback) (clickStatus "Query" (pasteDraft "stream" cancelled)) >>= done runtime . snd
      check "status Query posts draft into transcript and clears input" (T.null (contents (composerBuffer submitted)) && not (agentReplying submitted))
      let codeDraft=press V.KDown [] (pasteDraft "value = 42" (press (V.KChar ' ') [] (press (V.KChar '>') [] submitted)))
      codeSubmitted<-applyEvent (V.EvKey V.KEnter []) codeDraft >>= await runtime "code prompt completion" ((=="Agent: end_turn").status)
      codeMessages<-logged
      let codePrompts=[params | entry<-codeMessages,field "method" entry==Just ("session/prompt"::T.Text),Just params<-[field "params" entry::Maybe Value]]
          sentText=case reverse codePrompts of
            params:_->case field "prompt" params :: Maybe [Value] of
              Just (first:_)->field "text" first
              _->Nothing
            _->Nothing
      check "submitting a code draft sends code Markdown and clears the accepted raw draft"
        (sentText==Just (composerMarkdown (contents (composerBuffer codeDraft))) && T.null (contents (composerBuffer codeSubmitted)))
      busyDraft<-prompt runtime "wait" codeSubmitted >>= await runtime "composer busy" ((=="Agent is replying...").status)
      preserved<-tickConversation runtime (pasteDraft "stream" busyDraft)
      queued<-applyEvent (V.EvKey V.KEnter []) preserved >>= await runtime "busy query acceptance" (\d->agentQueued d==1 && bufferLength (composerBuffer d)==0)
      check "Enter queues a query while replying and retains input during ticks"
        (contents (composerBuffer preserved)=="stream" && agentQueued queued==1 && T.null (contents (composerBuffer queued)) && "Enter Queue query" `T.isInfixOf` snapshot queued)
      drained<-uncurry (conversationEffects runtime fallback) (clickStatus "Cancel" queued) >>= done runtime . snd
      check "Cancel stops current response then queued query runs" (agentQueued drained==0 && not (agentReplying drained) && "Enter Query" `T.isInfixOf` snapshot drained)
      idleDefault<-applyEvent (V.EvKey V.KEnter []) ((pasteDraft "idle direction" drained) {chatSubmit=SteerSubmit,agentSteering=True})
      check "default steering preserves idle draft and names Query" (contents (composerBuffer idleDefault)=="idle direction" && "Query" `T.isInfixOf` status idleDefault)
      steeringWait<-prompt runtime "wait" drained >>= await runtime "steering active" ((=="Agent is replying...").status)
      check "negotiated steering is available while replying" (agentSteering steeringWait && "Ctrl+Enter Steer" `T.isInfixOf` snapshot steeringWait)
      refused<-applyEvent (V.EvKey V.KEnter [V.MCtrl]) (pasteDraft "direction" steeringWait {agentSteering=False})
      check "unsupported steering retains draft" (contents (composerBuffer refused)=="direction")
      refusedDefault<-applyEvent (V.EvKey V.KEnter []) ((pasteDraft "direction" steeringWait) {chatSubmit=SteerSubmit,agentSteering=False})
      check "default steering preserves unsupported draft" (contents (composerBuffer refusedDefault)=="direction")
      steered<-applyEvent (V.EvKey V.KEnter []) ((pasteDraft "direction" steeringWait) {chatSubmit=SteerSubmit}) >>= await runtime "steering delivered" (not . agentReplying)
      check "steering uses adapter extension and clears submitted draft" . any ((==Just ("_session/steering"::T.Text)).field "method") =<< logged
      sourceReceipt<-captureVersion (documentBuffer (sourceDocument cancelled))
      sameSource<-versionCurrent sourceReceipt (documentBuffer (sourceDocument steered))
      check "steering does not edit source or retain sent draft" (T.null (contents (composerBuffer steered)) && sameSource)
      idleRace<-prompt runtime "wait" steered >>= await runtime "idle race active" ((=="Agent is replying...").status)
      rejectedSteer<-submit runtime SteerSubmit (draftBuffer (newBuffer "idle-race") idleRace) >>= await runtime "idle race response" (not . agentReplying)
      check "primary idle race leaves steering draft unsent" (contents (composerBuffer rejectedSteer)=="idle-race")
      legacyWait<-prompt runtime "wait" rejectedSteer >>= await runtime "legacy steer active" ((=="Agent is replying...").status)
      legacy<-submit runtime SteerSubmit (draftBuffer (newBuffer "legacy-steer") legacyWait) >>= await runtime "legacy steering retires provider" (T.isInfixOf "provider stopped" . status)
      check "primary legacy detached steering stops safely and retains the draft" (contents (composerBuffer legacy)=="legacy-steer" && not (agentSteering legacy))
      restarted<-prompt runtime "stream" legacy >>= done runtime
      disconnected<-prompt runtime "disconnect" restarted >>= await runtime "provider EOF" ((=="Agent disconnected.").status)
      reconnected<-prompt runtime "stream" disconnected >>= done runtime
      resumed<-send runtime "load" ["0","saved-id"] reconnected >>= await runtime "resume session" (\d->status d=="Session saved-id" && "Session: saved-id" `T.isInfixOf` conversationText d)
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
      shownBeforeClose<-send runtime "show" [] gated
      pendingClosed<-prompt runtime "wide" shownBeforeClose
      let (closing,closeEffects)=runCommand Close pendingClosed
          oldPaint=conversationBodySnapshot "" closing
          oldSource=M.lookup "" (conversationViews closing) >>= conversationSource
          received current=do
            next<-Conversation.tickConversation runtime current
            if status next=="Agent: end_turn" then pure next else threadDelay 10000 >> received next
      closed<-snd <$> conversationEffects runtime fallback closing closeEffects
      receivedClosed<-timeout 8000000 (received closed) >>= maybe (error "Closed conversation source timed out") pure
      let closedSource=M.lookup "" (conversationViews receivedClosed) >>= conversationSource
          checkpoint=root </> "closed-conversation.checkpoint"
          expected=T.replicate 90 "reply-width "<>"\n\n```sh\nprintf 'live λ'\n```"
          replies value=do
            views<-field "conversationViews" value :: Maybe [Value]
            primary<-find ((==Just (""::T.Text)).field "target") views
            body<-field "body" primary
            items<-field "items" body :: Maybe [Value]
            pure (mapMaybe (\item->field "content" item >>= field "markdown") items :: [T.Text])
      check "closed output advances source without presenting or reopening its body"
        (closedSource/=Nothing && closedSource/=oldSource && conversationBodySnapshot "" receivedClosed==oldPaint &&
         not (any ((==Just "").conversationTargetFor receivedClosed) (windows receivedClosed)))
      writeCheckpoint checkpoint receivedClosed >>= either (error.T.unpack) pure
      saved<-decodeStrict' <$> BS.readFile checkpoint
      restoredClosed<-readCheckpoint checkpoint desktop >>= either (error.T.unpack) pure
      writeCheckpoint checkpoint restoredClosed >>= either (error.T.unpack) pure
      restoredSource<-decodeStrict' <$> BS.readFile checkpoint
      check "checkpoint preserves latest closed canonical source rather than stale painted rows"
        (maybe False (elem expected) (saved >>= replies) && maybe False (elem expected) (restoredSource >>= replies) &&
         not (any ((==Just "").conversationTargetFor restoredClosed) (windows restoredClosed)))
    -- User guidance reaches ACP as context, without changing the visible query.
    createDirectoryIfMissing True (root </> "config/thc")
    writeFile (root </> "config/thc/config.toml") "[editor.agent]\ncontext = 'Global guidance marker'\n"
    writeFile (root </> "thc.toml") "[editor.agent]\ncontext = 'Project guidance marker'\n"
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
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
            (fmap (fmap filePath . documentFile) (activeDocument opened)==Just (Just (root </> "thc.toml")) && maybe False (protectedBuffer opened . sourceFixtureBuffer) (activeWindow opened))
        Nothing -> error "Missing Agent Context scope chooser"
      writeFile (root </> "thc.toml") "[editor.agent]\ncontext = 'Updated project guidance'\n"
      updated<-prompt runtime "stream" guided >>= done runtime
      messages<-logged
      let latest=last [params | entry<-messages,field "method" entry==Just ("session/prompt"::T.Text),Just params<-[field "params" entry::Maybe Value]]
      check "saved project context reaches the next query without reconnecting" ("Updated project guidance" `T.isInfixOf` json latest)
      waiting<-prompt runtime "wait" updated >>= await runtime "context steering wait" ((=="Agent is replying...").status)
      writeFile (root </> "thc.toml") "[editor.agent]\ncontext = 'Steering guidance marker'\n"
      steered<-submit runtime SteerSubmit (draftBuffer (newBuffer "direction") waiting) >>= await runtime "context steering completion" (not . agentReplying)
      steeringLog<-logged
      let lastSteer=last [params | entry<-steeringLog,field "method" entry==Just ("_session/steering"::T.Text),Just params<-[field "params" entry::Maybe Value]]
      check "primary steering requests host-owned idle handling" ((field "_meta" lastSteer >>= field "steering" >>= field "idleBehavior")==Just ("promptRequired"::T.Text))
      check "steering receives saved context updates" ("Steering guidance marker" `T.isInfixOf` json lastSteer)
      rejectionWait<-prompt runtime "wait" steered >>= await runtime "rejected context steering wait" ((=="Agent is replying...").status)
      writeFile (root </> "thc.toml") "[editor.agent]\ncontext = 'Retry context marker'\n"
      pendingSteer<-submit runtime SteerSubmit (draftBuffer (newBuffer "reject-context") rejectionWait)
      rejectedSteer<-withEditorFixture "fixture-child" pendingSteer $ \childFixture->do
        let childWaiting=draftAt (newBuffer "child draft") (Selection 2 4) childFixture
        let settleHidden d=do
              next<-tickConversation runtime d
              (_,answer)<-chatTool runtime next "agent_settings" (object [])
              settingsResult<-answer
              if either (const False) ((==Just False).field "replying") settingsResult
                then pure next else threadDelay 10000 >> settleHidden next
        hiddenRestored<-timeout 8000000 (settleHidden childWaiting) >>= maybe (error "Hidden primary steering did not settle") pure
        check "failed primary steering preserves selected child draft" (conversationTarget hiddenRestored=="fixture-child" && contents (composerBuffer hiddenRestored)=="child draft" && composerSelection hiddenRestored==Selection 2 4)
        restoredPrimary<-send runtime "show" [] hiddenRestored
        check "switching back restores rejected primary steering draft" (contents (composerBuffer restoredPrimary)=="reject-context")
        pure restoredPrimary
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
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
      restored<-send runtime "new" [] desktop >>= await runtime "persisted provider configuration" ((=="Session fixture-session").status)
      _<-send runtime "resume" [] restored
      pure ()
    let editorSessions=[(replicate 48 'a',"resume-a"::T.Text,root),(replicate 48 'b',"resume-b",root </> "other-project")]
        withEditor ident action=bracket (lookupEnv "THC_EDIT_SESSION" <* setEnv "THC_EDIT_SESSION" ident) (restoreEnvironment "THC_EDIT_SESSION") (const action)
        resumeId d=case [value | Just dg<-[dialog d],Input "Session ID" value _<-fields dg] of value:_->Just value; _->Nothing
    forM_ editorSessions $ \(ident,providerId,providerRoot) -> withEditor ident $ C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
      offered<-send runtime "resume" [] desktop
      check "new editor session does not inherit another session resume ID" (resumeId offered==Just "")
      configured<-configure runtime ("yes"::T.Text) desktop {sideTree=fmap (\tree->tree {treeRoot=providerRoot}) (sideTree desktop)}
      _<-send runtime "load" ["0",providerId] configured >>= await runtime "per-editor resume record" ((==("Session "<>providerId)).status)
      sidecar<-(++".agent.json") <$> checkpointPath ident
      saved<-decodeStrict' <$> BS.readFile sidecar
      check "provider resume record is saved beside its editor checkpoint"
        ((saved >>= field "sessionId")==Just providerId && (saved >>= field "cwd")==Just providerRoot)
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
      _<-send runtime "configure" ["0","not-the-saved-provider", "[]", "{}"] desktop
      pure ()
    beforeRecovery<-logged
    forM_ editorSessions $ \(ident,providerId,_) -> withEditor ident $ C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
      preparedDraft<-send runtime "show" [] draftBase
      let savedDraft=draftAt (newBuffer "existing draft") (Selection 4 4) preparedDraft
      recovered<-recoveredBody "Retained conversation after a daemon crash" savedDraft
      idle<-tickConversation runtime recovered
      offered<-send runtime "resume" [] idle
      check "recovered editor selects its own provider resume ID" (resumeId offered==Just providerId)
      resumedSources<-sameBufferVersions recovered offered
      resumedDraft<-sameDraftRoot recovered offered
      check "reading resume metadata leaves recovered transcript and draft intact" (resumedSources && resumedDraft)
    afterRecovery<-logged
    check "recovery never starts a provider or sends a prompt automatically" (afterRecovery==beforeRecovery)
    forM_ editorSessions $ \(ident,providerId,providerRoot) -> withEditor ident $ C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
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
    C.withConsoles $ \consoles -> withConversation consoles $ \runtime -> do
      configured<-configure runtime ("yes"::T.Text) desktop
      answered<-prompt runtime ("wide\n"<>T.unwords (replicate 90 "user-width")) configured >>= done runtime >>= await runtime "adopted wide reply" (T.isInfixOf "live λ".conversationText)
      forM_ [150,36,120] $ \columns -> do
        let resized=fst (handleEvent (V.EvResize columns 30) answered)
            (zoomed,effects)=runCommand Zoom resized
        (_,effected)<-conversationEffects runtime fallback zoomed effects
        shown<-await runtime "resized live body" (bodyAtWidth "") effected
        let body=targetBody "" shown
            win=fromMaybe (error "missing resized live window") (activeWindow shown)
            available=width (bounds win)-2
            rows=splitStyled (bodyHighlight body)
            sent row=any (\(_,style)->case style of BubbleText _ True _->True; _->False) row
            received row=any (\(_,style)->case style of BubbleText _ False _->True; _->False) row
            columnsOf row=let text=styledContents row in displayColumn text (T.length text)
        check "live conversation publication replaces immutable body identity"
          (conversationBodySnapshot "" shown/=conversationBodySnapshot "" answered)
        check "resize then Zoom anchors live user bubbles at the right window edge"
          (not (null (filter sent rows)) && all ((==available).columnsOf) (filter sent rows))
        check "resize then Zoom lets long live replies span the available window"
          (maximum (0:map columnsOf (filter received rows))>available-16 && all ((<=available).columnsOf) rows)
        check "live reflow preserves exact shell payload"
          (map (\(_,_,language,raw)->(language,raw)) (bodyShellBlocks body)==[("sh","printf 'live λ'\n")])

      let hub=AR.agentHub (conversationAgents runtime)
          caps=AH.Capabilities False False False []
          driver=AH.AgentDriver root "layout-fixture" caps (const (pure (Right caps)))
            (const (pure (Right Null))) (pure ()) (pure ()) (const (pure (Right Null)))
      child<-AH.registerAgent hub "Layout child" root driver >>= either (error . T.unpack) pure
      ticket<-AH.sendAgent hub AH.Human child (T.unwords (replicate 90 "child-user")) >>= either (error . T.unpack) pure
      _<-AH.waitAgent hub AH.Human child ticket 2000 >>= either (error . T.unpack) pure
      AH.recordAgentEvent hub child "output" (object ["text" .= (T.unwords (replicate 90 ("child-reply"::T.Text))<>"\n\n```sh\nprintf 'child λ'\n```" :: T.Text)])
      childView<-openChild runtime child answered >>= await runtime "adopted child layout" (\d->"child-reply" `T.isInfixOf` conversationText d && bodyAtWidth (AH.agentIdText child) d)
      let primaryRef=fromMaybe (error "primary view missing") (M.lookup "" (conversationViews childView) >>= conversationBodyRef)
          childRef=fromMaybe (error "child view missing") (M.lookup (AH.agentIdText child) (conversationViews childView) >>= conversationBodyRef)
          baseWindow=fromMaybe (error "child window missing") (activeWindow childView)
          paired primaryColumns childColumns=childView
            { conversationTarget=""
            , windows=[baseWindow {windowContent=PluginContent childRef,bounds=(bounds baseWindow) {width=childColumns}},
                baseWindow {windowId=windowId baseWindow+100,windowContent=PluginContent primaryRef,windowEditorMount=M.lookup "" (conversationViews childView) >>= conversationEditor,bounds=(bounds baseWindow) {width=primaryColumns}}] }

      forM_ [(148,62),(43,126)] $ \(primaryColumns,childColumns) -> do
        shown<-await runtime "paired body widths" (\d->bodyAtWidth "" d && bodyAtWidth (AH.agentIdText child) d) (paired primaryColumns childColumns)
        forM_ [("",primaryColumns,"printf 'live λ'\n"),(AH.agentIdText child,childColumns,"printf 'child λ'\n")] $ \(target,columns,raw) -> do
          let body=targetBody target shown
              rows=splitStyled (bodyHighlight body)
              sent row=any (\(_,style)->case style of BubbleText _ True _->True; _->False) row
              received row=any (\(_,style)->case style of BubbleText _ False _->True; _->False) row
              columnsOf row=let text=styledContents row in displayColumn text (T.length text)
          check "simultaneously visible chats anchor user bubbles to their own right edge"
            (not (null (filter sent rows)) && all ((==columns-2).columnsOf) (filter sent rows))
          check "inactive child and primary replies reflow at their own window width"
            (maximum (0:map columnsOf (filter received rows))>columns-18 && all ((<=columns-2).columnsOf) rows)
          check "each resized target retains its own exact executable shell body"
            (map (\(_,_,_,source)->source) (bodyShellBlocks body)==[raw])
        let before=map (`conversationBodySnapshot` shown) ["",AH.agentIdText child]
        unchanged<-tickConversation runtime shown
        let after=map (`conversationBodySnapshot` unchanged) ["",AH.agentIdText child]
        check "unchanged visible widths preserve prepared body identities" (before==after)

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
conversationText desktop=maybe "" (\body->let text=W.preparedWindowText body in contentSlice text 0 (contentLength text)) (conversationBodySnapshot (conversationTarget desktop) desktop)
conversationHighlight :: Desktop -> StyledText
conversationHighlight desktop=maybe [] bodyHighlight (conversationBodySnapshot (conversationTarget desktop) desktop)

bodyHighlight :: W.PreparedWindow -> StyledText
bodyHighlight body=case W.preparedWindowRows body of
  W.StyledRows rows->concat [runs sigils++maybe [] (\style->styledText style "\n") newline | StyledRow sigils newline _<-Vec.toList rows]
  _->styledText Plain (contentSlice (W.preparedWindowText body) 0 (contentLength (W.preparedWindowText body)))
  where
    runs Nil=[]
    runs (ConsChars text style rest)=(text,style):runs rest
    runs (ConsSigil glyph style _ rest)=(graphemeText glyph,style):runs rest

targetBody :: T.Text -> Desktop -> W.PreparedWindow
targetBody target desktop=fromMaybe (error "Missing prepared conversation body") (conversationBodySnapshot target desktop)

bodyShellBlocks :: W.PreparedWindow -> [(Int,Int,T.Text,T.Text)]
bodyShellBlocks=maybe [] (Vec.toList.W.textShellBlocks) . W.preparedWindowSemantics

bodyAtWidth :: T.Text -> Desktop -> Bool
bodyAtWidth target desktop=case M.lookup target (conversationViews desktop) of
  Just view | InstalledBody _ (Just (BodyControlReceipt _ columns _ _ _))<-conversationBody view->
    maybe False (\window->columns==max 1 (width (bounds window)-2)) (find ((==Just target).conversationTargetFor desktop) (windows desktop))
  _->False

-- Simulate inert restored installed text using the public retired publication
-- law; explicit Show must replace this exact visible frame in place.
recoveredBody :: T.Text -> Desktop -> IO Desktop
recoveredBody text desktop=do
  body<-W.prepareSemanticTextWindow "Conversation" (styledText Plain text)
    (W.TextSemantics W.CopyText Nothing Vec.empty Vec.empty W.ReadableWindow Vec.empty Vec.empty Vec.empty) >>= either (error.T.unpack) pure
  let reference=fromMaybe (error "Missing restored ref") (M.lookup "" (conversationViews desktop) >>= conversationBodyRef)
  W.retireWindowRef reference
  pure desktop {pluginWindows=M.insert reference body (pluginWindows desktop),
    retiredPluginWindows=S.insert reference (retiredPluginWindows desktop),
    conversationViews=M.adjust (\view->view {conversationBody=InstalledBody reference Nothing}) "" (conversationViews desktop)}

conversationActions :: Desktop -> [(Int,Int,T.Text,[T.Text])]
conversationActions desktop=maybe [] hostBodyActions (activeWindow desktop >>= windowConversationControls desktop)

-- Pump the same scoped presentation owner as App; protocol readiness alone is
-- deliberately insufficient for assertions about adopted transcript bodies.
tickPresented :: TextPresentation -> ConversationState -> Desktop -> IO Desktop
tickPresented presentation runtime desktop=do
  updated<-Conversation.tickConversation runtime desktop
  requests<-conversationBodyRequests runtime updated
  (prepared,results)<-tickTextPresentation presentation requests updated
  adoptConversationBodies runtime results prepared
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
withHeldRead :: FilePath -> FilePath -> String -> (MVar () -> MVar () -> Async () -> IO a) -> IO a
withHeldRead server pipe payload action =
  withCreateProcess (proc "python3" [server,"held-pipe-writer",pipe,payload])
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
  , "if len(sys.argv)==4 and sys.argv[1]=='held-pipe-writer':"
  , "  with open(sys.argv[2],'wb',buffering=0) as output:"
  , "    print('reader-ready',flush=True)"
  , "    assert sys.stdin.buffer.read(1)==b'!'"
  , "    output.write(sys.argv[3].encode('utf-8'))"
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
  , "  if method=='initialize':"
  , "    if os.environ.get('THC_CONNECT_GATE'): open(os.environ['THC_CONNECT_GATE'],'rb').read()"
  , "    reply(ident,{'protocolVersion':1,'agentCapabilities':{'loadSession':os.environ['THC_RESUME']=='yes'},'_meta':{'steering':{'supported':os.environ['THC_RESUME']=='yes'}}})"
  , "  elif method=='session/new': sid='fixture-session'; reply(ident,{'sessionId':sid,'configOptions':settings()})"
  , "  elif method=='session/load': sid=params['sessionId']; reply(ident,{})"
  , "  elif method=='session/set_config_option':"
  , "    if params['configId']=='model': model=params['value']"
  , "    else: effort=params['value']"
  , "    reply(ident,{'configOptions':settings()})"
  , "  elif method=='session/cancel': finish('cancelled')"
  , "  elif method=='_session/steering':"
  , "    if os.environ.get('THC_STEER_GATE'): open(os.environ['THC_STEER_GATE'],'rb').read()"
  , "    reply(ident,{'outcome':'failed' if params['prompt'][0]['text']=='reject-context' else 'promptRequired' if params['prompt'][0]['text']=='idle-race' else 'startedNewTurn' if params['prompt'][0]['text']=='legacy-steer' else 'injected'}); finish()"
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
  , "    elif scenario.startswith('Human answer to ask_user question '): finish()"
  , "    elif scenario=='```': finish()"
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

splitStyled :: StyledText -> [StyledText]
splitStyled=map fst . splitStyledText

styleRanges :: StyledText -> [(Int,Int,Style)]
styleRanges=snd . mapAccumL (\offset (text,style)->let end=offset+T.length text in (end,(offset,end,style))) 0

hasStyledChar :: Char -> Style -> StyledText -> Bool
hasStyledChar wanted style=any (\(text,actual)->actual==style && T.any (==wanted) text)
