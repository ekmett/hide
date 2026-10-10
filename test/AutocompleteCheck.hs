{-# LANGUAGE OverloadedStrings #-}
module AutocompleteCheck (checks) where

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad (unless,void)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import AutocompleteACPCheck (fixture)
import Hide.Autocomplete
import qualified Hide.AgentUI as AgentUI
import qualified Hide.Plugin.Session as Plugin
import qualified Hide.Plugin.Command as Command
import Hide.Plugin.Completion (HintServices(..))
import Hide.Plugin.Input (InputDeclaration(..))
import qualified Graphics.Vty as V
import Hide.AgentSidebarTypes
import Hide.Buffer
import Hide.Model
import qualified Hide.Plugin.Window as W
import qualified Hide.Plugin.Editor as E
import qualified Hide.Plugin.Menu as Menu
import Hide.PluginWindowHost (adoptWindowUpdate,retireClosedWindow)
import Hide.GuestAccess (readableAt,pointerAllowedAt)

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->do
  acknowledged<-newEmptyMVar
  let script=root </> "provider.py"
      logPath=root </> "requests.jsonl"
      project=root </> "thc.toml"
      source="value = old\nnext = untouched\n"
      desktop=addDocument Nothing (newBuffer source) (initialDesktop (100,30))
      send runtime action args d=snd <$> autocompleteEffects runtime (\state _->pure (False,state)) d [AutocompleteAction action args]
      hasTranscript d=any (\w->maybe False ((==windowContent w).PluginContent) (autocompleteWindow d)) (windows d)
      logs=mapMaybe decodeStrict' . BS.lines <$> BS.readFile logPath
      prompts=do
        entries<-logs
        pure [context | entry<-entries,field "method" entry==Just ("session/prompt"::T.Text),
          Just params<-[field "params" entry],Just blocks<-[field "prompt" params::Maybe [Value]],
          block<-drop (max 0 (length blocks-1)) blocks,Just text<-[field "text" block],
          Just context<-[decodeStrict' (TE.encodeUtf8 text)]]
      awaitPrompt count=awaitIO "provider prompt" $ do
        values<-prompts
        pure (if length values>=count then Just (last values) else Nothing)
      submit runtime context replacement=do
        let ident=required "requestId" context::T.Text
        result<-autocompleteTool runtime "submit_completion" (object ["requestId" .= ident,"proposals" .=
          [object ["startLine" .= (0::Int),"endLine" .= (1::Int),"text" .= (replacement::T.Text)]]])
        check "runtime private route accepts a bounded proposal" (either (const False) (const True) result)
        writeFile (root </> T.unpack ident) "complete"
        pure ident
      waitRetired runtime ident=awaitIO "completion retirement" $ do
        result<-autocompleteTool runtime "read_completion_context" (object ["requestId" .= ident])
        pure (case result of Left _->Just (); _->Nothing)
      save runtime visible d=send runtime "save" ["0","acp","python3",T.pack (show [script]),"","","copilot-language-server","[\"--stdio\"]",if visible then "true" else "false"] d
  writeFile script fixture
  writeFile logPath ""
  writeFile project (unlines ["[editor.autocomplete]","provider = \"acp\"","executable = \"python3\"","arguments = '"++show [script]++"'","debug = false",
    "[agent]","executable = \"must-not-start-main-conversation-provider\""])
  withEnv "XDG_CONFIG_HOME" (Just (root </> "config")) $
    withEnv "THC_EDIT_SESSION" Nothing $
    withEnv "LOG" (Just logPath) $
    withEnv "SECRET" (Just "runtime-autocomplete-secret") $
    withAutocomplete (observeHint acknowledged <$> Plugin.pluginCompletionInput AgentUI.plugin) root $ \runtime->do
      configured<-awaitDesktop runtime "project ACP configuration" autocompleteACPEnabled desktop
      check "completion chat is hidden by default" (not (hasTranscript configured))
      check "provider is lazy before any request" . null =<< logs
      requested<-send runtime "propose" [] configured
      first<-awaitPrompt 1
      -- The running child retains its launch environment after parent changes.
      setEnv "SECRET" "replacement-autocomplete-secret"
      ident<-submit runtime first "value = new\n"
      waitRetired runtime ident
      preview<-awaitDesktop runtime "inline preview" (isJust.inlinePreview) requested
      check "proposal does not mutate the source" (activeText preview==source && not (hasTranscript preview))
      accepted<-send runtime "accept" [] preview
      check "accept applies the proposed change" (activeText accepted=="value = new\nnext = untouched\n" && inlinePreview accepted==Nothing)
      check "accept is exactly one undoable edit" (activeText (fst (runCommand Undo accepted))==source && maybe False ((==1).length.undoStack.documentBuffer) (activeDocument accepted))
      next<-send runtime "propose" [] (clearInline accepted)
      second<-awaitPrompt 2
      staleIdent<-submit runtime second "value = stale\n"
      waitRetired runtime staleIdent
      let edited=insertText "human" next
      discarded<-tickAutocomplete runtime edited
      check "queued result cannot overwrite a newer buffer" (inlinePreview discarded==Nothing && activeText discarded==activeText edited)
      sameVersionRequest<-send runtime "propose" [] (clearInline discarded)
      third<-awaitPrompt 3
      sameVersionId<-submit runtime third "value = stale again\n"
      waitRetired runtime sameVersionId
      let changedIdentity=sameVersionRequest {buffers=M.adjust
            (\doc->doc {documentBuffer=(newBuffer "replacement with reused revision\n") {revision=revision (documentBuffer doc)}})
            (maybe (error "Missing source window") sourceFixtureBuffer (activeWindow sameVersionRequest)) (buffers sameVersionRequest)}
      rejectedIdentity<-tickAutocomplete runtime changedIdentity
      check "buffer identity rejects stale results even with the same revision and caret"
        (inlinePreview rejectedIdentity==Nothing && activeText rejectedIdentity=="replacement with reused revision\n")
      -- Eighty surrounding rows exceed the prompt budget, but each row and the
      -- caret line fit. Context must select whole lines and retain the caret.
      let large=T.unlines (replicate 81 (T.replicate 300 "x"))
          wide=moveTo False (40*301+5) (addDocument Nothing (newBuffer large) rejectedIdentity)
      wideRequest<-send runtime "propose" [] wide
      context<-awaitPrompt 4
      let firstLine=required "firstLine" context::Int
          endLine=required "endLine" context::Int
          requestId=required "requestId" context::T.Text
      check "bounded context still includes the caret line" (firstLine<=40 && endLine>40)
      abstain<-autocompleteTool runtime "submit_completion" (object ["requestId" .= requestId,"proposals" .= ([]::[Value])])
      check "large source context remains valid" (either (const False) (const True) abstain)
      writeFile (root </> T.unpack requestId) "complete"
      waitRetired runtime requestId
      settled<-tickAutocomplete runtime wideRequest
      openingTranscript<-save runtime True settled >>= awaitDesktop runtime "debug pane toggle on" hasTranscript
      shown<-awaitDesktop runtime "prepared completion activity" (\d->case autocompleteWindow d >>= (`M.lookup` pluginWindows d) of
        Just body->let text=W.preparedWindowText body in "[reply]" `T.isInfixOf` contentSlice text 0 (contentLength text)
        Nothing->False) openingTranscript
      check "debug pane preserves focused source" (activeText shown==large)
      check "completion transcript allocates no source document" (M.keys (buffers shown)==M.keys (buffers settled))
      let oldRef=maybe (error "Missing completion reference") id (autocompleteWindow shown)
          traceWindow=case [w | w<-windows shown,windowContent w==PluginContent oldRef] of w:_->w; _->error "Missing completion frame"
          traceBody=pluginWindows shown M.! oldRef
          focused=setComposerInput (newBuffer "retained human hint") (Selection 0 0) False (focusWindow (windowId traceWindow) shown)
          hintRef=maybe (error "Missing completion editor") E.mountDraft (activeEditorMount focused)
          retainedHint d=maybe "" (contents . editorDraftBuffer) (M.lookup hintRef (editorDrafts d))
          selected=fst (runCommand SelectAll focused)
          copied=fst (runCommand Copy selected)
      check "published completion body preserves upstream secret redaction"
        (all (not . (`T.isInfixOf` contentSlice (W.preparedWindowText traceBody) 0 (contentLength (W.preparedWindowText traceBody)))) ["private-completion","runtime-autocomplete-secret","replacement-autocomplete-secret"])
      check "completion output uses plugin copy without hint text" (clipboard copied==W.copyPreparedSelection traceBody 0 (contentLength (W.preparedWindowText traceBody)))
      check "completion trace remains readable but has no guest input authority"
        (readableAt focused (left (bounds traceWindow)+2) (top (bounds traceWindow)+2) && not (pointerAllowedAt focused (left (bounds traceWindow)+2) (top (bounds traceWindow)+2)))
      late<-W.refreshWindow oldRef traceBody >>= maybe (error "Missing pending trace refresh") pure
      let closed=fst (runCommand Close focused)
      retired<-retireClosedWindow oldRef closed
      stale<-adoptWindowUpdate Menu.HumanMenu late retired
      check "late completion publication cannot reopen a closed frame" (not (hasTranscript stale) && retainedHint stale=="retained human hint")
      -- A completion after frame close keeps the warm provider and the frame shut.
      continued<-send runtime "propose" [] stale
      fifth<-awaitPrompt 5
      let fifthId=required "requestId" fifth::T.Text
      fifthSubmitted<-autocompleteTool runtime "submit_completion" (object ["requestId" .= fifthId,"proposals" .= ([]::[Value])])
      check "warm provider can abstain" (either (const False) (const True) fifthSubmitted)
      writeFile (root </> T.unpack fifthId) "complete"
      waitRetired runtime fifthId
      warm<-tickAutocomplete runtime continued
      check "new trace output leaves a closed completion frame closed" (not (hasTranscript warm))
      staleChoices<-timeout 5000000 (completionChoices runtime (CompletionTarget (-1) Nothing) "model")
      check "expired choice query resolves without a dialog publication" (case staleChoices of Just (Left _)->True; _->False)
      warmEntries<-logs
      check "revealing completion chat preserves the existing provider instance"
        (length [() | entry<-warmEntries,field "method" entry==Just ("session/new"::T.Text)]==1)
      target<-awaitIO "completion target for reopening" $ fmap (\(CompletionSummary current _)->current) <$> completionSummary runtime
      opening<-snd <$> autocompleteEffects runtime (\state _->pure (False,state)) warm [AgentSidebarAction (ShowCompletion target)]
      reopened<-awaitDesktop runtime "reopened completion view" hasTranscript opening
      check "reopening gets a new frame identity and retains the hint"
        (autocompleteWindow reopened/=Just oldRef && retainedHint reopened=="retained human hint")
      let floatingTrace=case [windowId w | w<-windows reopened,Just ref<-[autocompleteWindow reopened],windowContent w==PluginContent ref] of
            traceId:_->setTerminalPinned False traceId reopened; []->error "reopened completion frame"
          sourceFocused=case (activeWindow warm,activeWindow floatingTrace) of
            (Just sourceWindow,Just trace)->groupWindows (windowId sourceWindow) (windowId trace) floatingTrace
            _->error "source and completion frames"
      hidden<-save runtime False sourceFocused >>= awaitDesktop runtime "debug pane toggle off" (not.hasTranscript)
      check "debug toggle keeps the source and retires completion tab membership"
        (activeText hidden==large && null (windowTabs hidden))
      let shortSource=addDocument Nothing (newBuffer "x\n") hidden
          configure settingTarget option value state=snd <$> autocompleteEffects runtime (\d _->pure (False,d)) state
            [AgentSidebarAction (ConfigureCompletion settingTarget option value)]
      sixthRequest<-send runtime "propose" [] shortSource
      sixth<-awaitPrompt 6
      sixthId<-submit runtime sixth "first proposal\n"
      waitRetired runtime sixthId
      firstPreview<-awaitDesktop runtime "preview before settings change" (isJust.inlinePreview) sixthRequest
      oldTarget<-awaitIO "connected completion target" $ fmap (\(CompletionSummary current _)->current) <$> completionSummary runtime
      changedSetting<-configure oldTarget "model-id" "model-b" firstPreview
      configuredPreview<-awaitDesktop runtime "accepted setting invalidates preview" ((==Nothing).inlinePreview) changedSetting
      seventhRequest<-send runtime "propose" [] configuredPreview
      seventh<-awaitPrompt 7
      seventhId<-submit runtime seventh "newer proposal\n"
      waitRetired runtime seventhId
      newerPreview<-awaitDesktop runtime "new preview after settings change" (isJust.inlinePreview) seventhRequest
      currentTarget<-awaitIO "completion target after settings change" $ fmap (\(CompletionSummary current _)->current) <$> completionSummary runtime
      rejectedSetting<-configure oldTarget "effort-id" "high" newerPreview
      -- The reply to this queued request proves the owner handled the preceding
      -- rejected setting; a shared notice cannot identify that completion. The
      -- shared provider advertises one effort option and two model-category
      -- options (one used by the credential-redaction check).
      choicesBarrier<-timeout 5000000 (completionChoices runtime currentTarget "thought_level")
      check ("current completion choices follow rejected setting: " ++ show choicesBarrier ++ "; captured " ++ show currentTarget) (case choicesBarrier of
        Just (Right (replyTarget,_,_,_))->replyTarget==currentTarget
        _->False)
      preserved<-tickAutocomplete runtime rejectedSetting
      check "expired setting leaves a newer inline proposal intact"
        (inlinePreview preserved==inlinePreview newerPreview && activeText preserved=="x\n")
      entries<-logs
      check "autocomplete configuration is independent of the main agent" (length [() | entry<-entries,field "method" entry==Just ("initialize"::T.Text)]==1)
      -- The real plugin's command and this provider turn own acknowledgement.
      -- Each gate is named by its ACP request; no later case clears an old gate.
      visibleHints<-save runtime True preserved >>= awaitDesktop runtime "hint pane visible" hasTranscript
      let hintWindow state=case [w | Just ref<-[autocompleteWindow state],w<-windows state,windowContent w==PluginContent ref] of
            w:_->w
            _->error "Missing hint window"
          focusHint state=focusWindow (windowId (hintWindow state)) state
          draft text state=setComposerInput (newBuffer text) (Selection (T.length text) (T.length text)) True (focusHint state)
          sendInput state=let (nextState,effects)=handleEvent (V.EvKey V.KEnter []) state
            in snd <$> autocompleteEffects runtime (\d _->pure (False,d)) nextState effects
          hintReady :: T.Text -> IO FilePath
          hintReady hintText=do
            gateId<-awaitIO "hint request" $ do
              requestEntries<-logs
              pure $ case [rpcId | entry<-requestEntries,field "method" entry==Just ("session/prompt"::T.Text),
                    Just params<-[field "params" entry],Just blocks<-[field "prompt" params::Maybe [Value]],
                    block<-blocks,Just text<-[field "text" block],Just hintContext<-[decodeStrict' (TE.encodeUtf8 text)],
                    field "intent" hintContext==Just ("hint"::T.Text),field "message" hintContext==Just hintText,
                    Just rpcId<-[field "id" entry::Maybe Int]] of
                rpcId:_->Just ("hint-"++show rpcId)
                []->Nothing
            awaitIO "hint provider arrival" $ do
              ready<-doesFileExist (root </> (gateId++".ready"))
              pure (if ready then Just () else Nothing)
            pure gateId
          releaseHint gateId=writeFile (root </> gateId) "complete"
      pendingHint<-sendInput (draft "send this exact version" visibleHints)
      hintId<-hintReady "send this exact version"
      check "submitted hint stays editable during provider delivery" (contents (composerBuffer pendingHint)=="send this exact version")
      duplicateHint<-sendInput pendingHint
      check "repeated Enter does not enqueue the same immutable hint" (status duplicateHint=="Completion hint is already pending.")
      releaseHint hintId
      -- Clearing this exact draft is the owning adoption result, independent of
      -- mutable HUD messages or later transcript refreshes.
      clearedHint<-awaitDesktop runtime "accepted hint clear" ((==0).bufferLength.composerBuffer) duplicateHint
      services<-timeout 5000000 (takeMVar acknowledged) >>= maybe (error "Plugin hint command did not acknowledge") pure
      expired<-timeout 5000000 (sendHint services "must not reach the retired invocation")
      check "retained hint service rejects after its command completes" (case expired of Just (Left _)->True; _->False)
      check "success clears only the submitted hint" (activeText preserved=="x\n" && bufferLength (composerBuffer clearedHint)==0)
      -- Input bounds are enforced before the real plugin can call its service.
      let oversizedText=T.replicate 16385 "x"
      oversizedHint<-sendInput (draft oversizedText clearedHint)
      boundTarget<-awaitIO "completion target for input bound" $ fmap (\(CompletionSummary current _)->current) <$> completionSummary runtime
      boundBarrier<-timeout 5000000 (completionChoices runtime boundTarget "thought_level")
      check "input bound rejection completes before the owner barrier" (case boundBarrier of
        Just (Right (replyTarget,_,_,_))->replyTarget==boundTarget
        _->False)
      boundedHint<-tickAutocomplete runtime oversizedHint
      boundedPrompts<-prompts
      check "oversized input stays in its draft and never reaches the provider"
        (bufferLength (composerBuffer boundedHint)==16385 && all ((/=Just oversizedText) . field "message") boundedPrompts)
      -- Retire one queued request while the owner stays live. Its FIFO reply
      -- proves the skipped request was handled before inspecting provider input.
      heldHint<-sendInput (draft "hold before close" boundedHint)
      heldId<-hintReady "hold before close"
      queuedHint<-sendInput (draft "retired queued hint" heldHint)
      check "later hint is queued before its mount closes"
        (status queuedHint=="Sending completion hint; draft kept until delivered.")
      let (closingHint,retireEffects)=runCommand Close queuedHint
      retiredHint<-snd <$> autocompleteEffects runtime (\d _->pure (False,d)) closingHint retireEffects
      liveTarget<-awaitIO "completion target before retired hint barrier" $ fmap (\(CompletionSummary current _)->current) <$> completionSummary runtime
      releaseHint heldId
      retiredBarrier<-timeout 5000000 (completionChoices runtime liveTarget "thought_level")
      check "live owner handles queued hint retirement before replying" (case retiredBarrier of
        Just (Right (replyTarget,_,_,_))->replyTarget==liveTarget
        _->False)
      receivedPrompts<-prompts
      check "retired queued hint never reaches the provider"
        (all ((/=Just ("retired queued hint"::T.Text)) . field "message") receivedPrompts)
      retiredSettled<-tickAutocomplete runtime retiredHint
      reopeningHint<-snd <$> autocompleteEffects runtime (\d _->pure (False,d)) retiredSettled
        [AgentSidebarAction (ShowCompletion liveTarget)]
      reopenedHint<-awaitDesktop runtime "hint pane after queued retirement" hasTranscript reopeningHint
      -- While an identified provider turn is held, queue pressure is a direct
      -- synchronous refusal. No status polling or provider timing establishes it.
      blockedHint<-sendInput (draft "hold provider" reopenedHint)
      blockedId<-hintReady "hold provider"
      let fill n state
            | n>80=error "Hint queue did not remain bounded"
            | otherwise=do
                let text="queued hint "<>T.pack (show n)
                nextState<-sendInput (draft text state)
                if status nextState=="Autocomplete is busy; draft kept."
                  then check "queue pressure keeps the unsubmitted hint" (contents (composerBuffer nextState)==text) >> pure nextState
                  else fill (n+1) nextState
      fullHints<-fill (1::Int) blockedHint
      let (closedHints,closeEffects)=runCommand Close fullHints
      _<-snd <$> autocompleteEffects runtime (\d _->pure (False,d)) closedHints closeEffects
      releaseHint blockedId
  closed<-withAutocomplete (Plugin.pluginCompletionInput AgentUI.plugin) root pure
  closedChoices<-timeout 5000000 (completionChoices closed (CompletionTarget 0 Nothing) "model")
  check "choice requests refuse after owner shutdown" (case closedChoices of Just (Left _)->True; _->False)
  putStrLn "Autocomplete runtime checks passed"

-- Observe the real command's result without changing its validation, provider
-- call or reply adapter. The runtime adoption above proves that it has returned.
observeHint :: MVar HintServices -> InputDeclaration HintServices -> InputDeclaration HintServices
observeHint acknowledged (InputDeclaration spec limit definition arguments result)=
  InputDeclaration spec limit (definition {Command.commandRun= \services input->do
    reply<-Command.commandRun definition services input
    case reply of Right _->void (tryPutMVar acknowledged services); Left _->pure ()
    pure reply}) arguments result

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "fixture" (.:key))
required :: FromJSON a => Key -> Value -> a
required key=maybe (error "Missing runtime fixture field") id . field key
check :: String -> Bool -> IO ()
check label ok=unless ok (error ("Autocomplete runtime: "++label))
awaitIO :: String -> IO (Maybe a) -> IO a
awaitIO label ready=timeout 5000000 loop >>= maybe (error ("Autocomplete runtime timed out: "++label)) pure
  where loop=ready >>= maybe (threadDelay 1000 >> loop) pure
awaitDesktop :: Autocomplete -> String -> (Desktop->Bool) -> Desktop -> IO Desktop
awaitDesktop runtime label predicate initial=timeout 5000000 (loop initial) >>= maybe (error ("Autocomplete runtime timed out: "++label)) pure
  where loop state=do
          updated<-tickAutocomplete runtime state
          if predicate updated then pure updated else threadDelay 1000 >> loop updated
withEnv :: String -> Maybe String -> IO a -> IO a
withEnv name value action=bracket (lookupEnv name <* set value) set (const action)
  where set=maybe (unsetEnv name) (setEnv name)
temporary :: IO FilePath
temporary=do
  base<-getTemporaryDirectory
  (path,handle)<-openTempFile base "thc-autocomplete-runtime"
  hClose handle
  removeFile path
  createDirectory path
  canonicalizePath path
