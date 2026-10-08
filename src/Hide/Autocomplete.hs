{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | Background ownership of persistent completion providers and preview adoption.
--
-- Requests capture immutable source context. A worker forces input/history and
-- prepares bounded previews; the tick checks generation, caret/view state and
-- stable source identity before installation. The provider is opened lazily and
-- kept warm across completions; explicit acceptance uses ordinary buffer edits.
module Hide.Autocomplete
  ( Autocomplete, withAutocomplete, autocompleteEffects, tickAutocomplete
  , autocompleteToken, autocompleteTool, completionSummary, completionChoices ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, race)
import Control.Concurrent.STM
import Control.Exception (IOException, try, evaluate, finally)
import Control.Monad (forever, forM_, void, when, foldM)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import Data.Maybe (fromMaybe, mapMaybe, isNothing)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import qualified Graphics.Vty as V
import System.FilePath ((</>), isAbsolute)
import System.Mem.StableName
import Hide.Buffer
import Hide.Model hiding (Settings, Save)
import Hide.InlineState
import Hide.InlineTypes
import qualified Hide.AutocompleteConfig as C
import qualified Hide.AutocompleteACP as A
import qualified Hide.Copilot as P
import Hide.MCPPermissions (readAutocompleteFor, writeAutocompleteFor)
import Hide.EditorMCP (editorServersFor)
import Hide.RemoteEndpoint (randomIdentity)
import Hide.AgentSidebarTypes
import qualified Hide.AgentHub as Hub
import Control.DeepSeq (force)
import qualified Hide.Plugin.Window as W
import qualified Hide.Plugin.Menu as Menu
import Hide.PluginWindowHost (adoptWindowUpdate)

-- Snapshot identity guards adoption even when undo returns an old revision.
data Snapshot = Snapshot InlineView Buffer (StableName Buffer) FilePath
type CompletionChoices = (CompletionTarget,T.Text,[(T.Text,T.Text)],T.Text)
data Job = Request Snapshot T.Text Int | Settings | Save Value | Feedback CompletionFeedback Proposal
  | SignIn | FinishSignIn | SignOut | Hint T.Text
  | Reveal CompletionTarget | Choices CompletionTarget T.Text (TMVar (Either T.Text CompletionChoices)) | Configure CompletionTarget T.Text T.Text
data Reply = Ready Snapshot [InlineOption] | Notice T.Text | ShowSettings Value
  | ShowSignIn T.Text | Configured Bool Bool | Transcript W.PreparedWindow
  | RevealTranscript CompletionTarget
  | CompletionConfigured CompletionTarget Int T.Text

data Provider = Provider
  { complete :: CompletionInput -> IO [Proposal]
  , feedback :: CompletionFeedback -> Proposal -> IO ()
  , hint :: T.Text -> IO ()
  , signIn :: IO CopilotSignIn, finishSignIn :: CopilotSignIn -> IO (), signOut :: IO () }

data Autocomplete = Autocomplete
  { autocompleteToken :: T.Text, jobs :: TBQueue Job, replies :: TQueue Reply
  , generation :: TVar Int, connection :: IORef (Maybe A.ACPCompletion)
  , requested :: IORef (Maybe Snapshot), displayed :: IORef (Maybe InlineView)
  , hold :: IORef (Maybe (Integer,Int,Bool)), debugVisible :: IORef Bool
  , transcriptScope :: W.WindowScope, debugBody :: IORef W.PreparedWindow
  , transcriptReady :: IORef (Maybe W.PreparedWindow)
  , summary :: IORef (Maybe CompletionSummary), settingsEpoch :: IORef Int
  , choiceClosed :: TVar Bool }

-- | Read cheap prepared ACP metadata. This never starts a provider or retains
-- its transcript, source snapshots or private connection identity.
completionSummary :: Autocomplete -> IO (Maybe CompletionSummary)
completionSummary=readIORef . summary

withAutocomplete :: FilePath -> (Autocomplete -> IO a) -> IO a
withAutocomplete root use=W.withWindowScope $ \scope->do
  empty<-prepareTranscript ""
  token<-T.pack <$> randomIdentity
  runtime<-Autocomplete token <$> newTBQueueIO 64 <*> newTQueueIO <*> newTVarIO 0 <*> newIORef Nothing
    <*> newIORef Nothing <*> newIORef Nothing <*> newIORef Nothing <*> newIORef False <*> pure scope <*> newIORef empty <*> newIORef Nothing <*> newIORef Nothing <*> newIORef 0 <*> newTVarIO False
  servers<-editorServersFor (Just token)
  withAsync (owner runtime servers `finally` closeChoices runtime) $ \_ ->
    withAsync (traceLoop runtime) (const (use runtime)) `finally` closeChoices runtime
  where
    owner runtime servers=forever $ do
      loaded<-readAutocompleteFor root
      let values=either (const (object [])) id loaded
      case loaded >>= C.parseCompletionConfig of
        Left problem->emit runtime (Notice problem)
        Right _->pure ()
      let cfg=either (const (error "default autocomplete configuration")) id (C.parseCompletionConfig (object []))
      loop runtime servers values (either (const cfg) id (C.parseCompletionConfig values)) Nothing
    loop runtime servers values cfg active=do
      epoch<-atomicModifyIORef' (settingsEpoch runtime) (\previous->(previous+1,previous+1))
      settings<-newIORef values
      emit runtime (Configured (C.debug cfg) (C.provider cfg=="acp"))
      let refresh=do
            preparedSummary<-if C.provider cfg/="acp" then pure Nothing else do
              configuration<-readIORef (connection runtime) >>= maybe (pure Nothing) A.completionConfiguration
              pure (Just (CompletionSummary (CompletionTarget epoch (fmap fst configuration)) (if isNothing configuration then "configured" else "connected")))
            writeIORef (summary runtime) preparedSummary
          currentTarget=do
            configuration<-readIORef (connection runtime) >>= maybe (pure Nothing) A.completionConfiguration
            pure (CompletionTarget epoch (fmap fst configuration))
          targetCurrent expected=(==expected) <$> currentTarget
      refresh
      auth<-newIORef Nothing
      let run current job=case job of
            Settings->readIORef settings >>= emit runtime . ShowSettings >> pure True
            Save updated->case C.parseCompletionConfig updated of
              Left problem->emit runtime (Notice problem) >> pure True
              Right selected->writeAutocompleteFor root updated >>= \result->case result of
                Left problem->emit runtime (Notice problem) >> pure True
                Right ()->do
                  previous<-C.parseCompletionConfig <$> readIORef settings
                  case previous of
                    Right old | selected {C.debug=C.debug old}==old->do
                      writeIORef settings updated
                      emit runtime (Configured (C.debug selected) (C.provider selected=="acp"))
                      pure True
                    _->pure False
            Feedback action proposal->forM_ current (\p->void (try (feedback p action proposal) :: IO (Either IOException ()))) >> pure True
            Request snap intent serial->do
              valid<-((==serial) <$> readTVarIO (generation runtime))
              when valid $ case current of
                Nothing->emit runtime (Notice "Choose ACP or Copilot in Options > Autocomplete.")
                Just p->do
                  result<-race (atomically (readTVar (generation runtime) >>= check . (/=serial))) $ try $ do
                    input<-prepareInput snap intent serial
                    options<-complete p input
                    let Snapshot _ source _ _=snap
                        prepared=mapMaybe (either (const Nothing) Just . prepareOption source) (take 8 options)
                    -- Evaluate all bounded preview rows on this worker.
                    _<-evaluate (length (show prepared))
                    pure prepared
                  case result of
                    Right (Right options)->emit runtime (Ready snap options)
                    Right (Left (_::IOException))->emit runtime (Notice "Autocomplete failed. Check the provider executable, configuration and authentication.")
                    Left ()->pure ()
              pure True
            SignIn->do
              forM_ current $ \p->try (signIn p) >>= \result->case result of
                Left (_::IOException)->emit runtime (Notice "Copilot sign-in could not start.")
                Right challenge->writeIORef auth (Just challenge) >> emit runtime (ShowSignIn (signInCode challenge))
              pure True
            FinishSignIn->do
              challenge<-atomicModifyIORef' auth (\old->(Nothing,old))
              forM_ current $ \p->forM_ challenge $ \c->try (finishSignIn p c) >>= \result->
                emit runtime (Notice (either (const "Copilot sign-in failed.") (const "Copilot sign-in completed.") (result :: Either IOException ())))
              pure True
            Hint text->do
              forM_ current $ \p->try (hint p text) >>= \result->case result of
                Left (_::IOException)->emit runtime (Notice "Completion agent hint failed.")
                Right ()->pure ()
              pure True
            Reveal expected->do
              valid<-targetCurrent expected
              emit runtime (if valid && C.provider cfg=="acp" then RevealTranscript expected else Notice "Completion view expired.")
              pure True
            Choices expected category answer->do
              result<-try $ do
                valid<-targetCurrent expected
                session<-readIORef (connection runtime)
                case session of
                  Just provider | valid && C.provider cfg=="acp"->do
                    A.discoverACPConfiguration provider
                    configuration<-A.completionConfiguration provider
                    case configuration of
                      Just (receipt,choices) | [choice]<-[choice | choice<-choices,Hub.configCategory choice==category]->do
                        let options=Hub.configValues choice
                        _<-evaluate (force options)
                        refresh
                        pure (Right (CompletionTarget epoch (Just receipt),Hub.configId choice,options,Hub.configCurrent choice))
                      _->pure (Left "The completion provider has not advertised these choices.")
                  _->pure (Left "Completion choices expired; reopen them.")
              finishChoices answer (either (const (Left "Cannot read completion provider choices.")) id (result :: Either IOException (Either T.Text CompletionChoices)))
              pure True
            Configure expected option value->do
              valid<-targetCurrent expected
              session<-readIORef (connection runtime)
              case (session,expected) of
                (Just provider,CompletionTarget _ (Just receipt)) | valid->do
                  configuration<-A.completionConfiguration provider
                  result<-A.configureACPAt provider receipt option value
                  case result of
                    Left problem->emit runtime (Notice problem)
                    Right ()->do
                      previous<-readIORef settings
                      let category=case configuration of Just (_,choices)->[Hub.configCategory choice | choice<-choices,Hub.configId choice==option]; _->[]
                          key=if category==["model"] then "model" else "effort"
                          updated=case previous of Object fields->Object (KM.insert key (String value) fields); _->previous
                      writeIORef settings updated
                      serial<-atomically $ do
                        modifyTVar' (generation runtime) (+1)
                        readTVar (generation runtime)
                      saved<-writeAutocompleteFor root updated
                      refresh
                      accepted<-currentTarget
                      emit runtime (CompletionConfigured accepted serial
                        (either ("Completion setting changed, but could not be saved: "<>) (const "Completion setting updated.") saved))
                _->emit runtime (Notice "Completion setting expired.")
              pure True
            SignOut->do
              writeIORef auth Nothing
              forM_ current $ \p->try (signOut p) >>= \result->emit runtime (Notice (either (const "Copilot sign-out failed.") (const "Copilot signed out.") (result :: Either IOException ())))
              pure True
          go current=do
            job<-atomically (readTBQueue (jobs runtime))
            let needsProvider=case job of Request{}->True; SignIn->True; FinishSignIn->True; SignOut->True; Hint{}->True; Choices{}->True; _->False
            if needsProvider && maybe True (const False) current && C.provider cfg/="off"
              then do
                opened<-try (withProvider runtime servers cfg (\p->run (Just p) job >>= \again->refresh >> when again (go (Just p))))
                case opened of
                  Left (_::IOException)->do
                    case job of Choices _ _ answer->finishChoices answer (Left "Cannot start autocomplete provider. Check Options > Autocomplete."); _->pure ()
                    emit runtime (Notice "Cannot start autocomplete provider. Check Options > Autocomplete.") >> refresh >> go Nothing
                  Right ()->pure ()
              else run current job >>= \again->refresh >> when again (go current)
      go active
    withProvider runtime servers cfg action
      | C.provider cfg=="acp"=A.withACPCompletion (C.acpLaunch cfg) root servers (C.model cfg) (C.effort cfg) $ \session->do
          writeIORef (connection runtime) (Just session)
          action (Provider (A.completeACP session) (A.feedbackACP session) (A.hintACP session) unavailable (const unavailable) unavailable)
            `finally` writeIORef (connection runtime) Nothing
      | otherwise=P.withCopilot (C.copilotLaunch cfg) root $ \session->
          action (Provider (P.completeCopilot session) (P.feedbackCopilot session) (const unavailable) (P.signInCopilot session) (P.finishSignInCopilot session) (P.signOutCopilot session))
    unavailable=ioError (userError "Authentication is available only for Copilot")

-- | Request advertised choices on the existing provider owner. This waits only
-- on its result cell, outside the desktop owner. Rejection, provider failure and
-- shutdown each resolve once; cancellation leaves only its bounded queued request.
-- A live incarnation is never rebound to another receipt. Initial connection
-- startup may require reopening if it changes the captured target first.
completionChoices :: Autocomplete -> CompletionTarget -> T.Text -> IO (Either T.Text CompletionChoices)
completionChoices runtime target category=do
  answer<-newEmptyTMVarIO
  accepted<-atomically $ do
    closed<-readTVar (choiceClosed runtime)
    full<-isFullTBQueue (jobs runtime)
    if closed || full then pure False else writeTBQueue (jobs runtime) (Choices target category answer) >> pure True
  if not accepted then pure (Left "Completion choices are unavailable or busy.") else atomically $
    readTMVar answer `orElse` (readTVar (choiceClosed runtime) >>= check >> pure (Left "Completion owner closed."))
finishChoices :: TMVar (Either T.Text CompletionChoices) -> Either T.Text CompletionChoices -> IO ()
finishChoices answer result=atomically (void (tryPutTMVar answer result))
closeChoices :: Autocomplete -> IO ()
closeChoices runtime=atomically (writeTVar (choiceClosed runtime) True)

emit :: Autocomplete -> Reply -> IO ()
emit runtime=atomically . writeTQueue (replies runtime)

enqueue :: Autocomplete -> Job -> IO ()
enqueue runtime job=atomically $ do
  full<-isFullTBQueue (jobs runtime)
  if full then writeTQueue (replies runtime) (Notice "Autocomplete is busy.") else writeTBQueue (jobs runtime) job

autocompleteTool :: Autocomplete -> T.Text -> Value -> IO (Either T.Text Value)
autocompleteTool runtime name args=readIORef (connection runtime) >>= maybe (pure (Left "No active completion request.")) (\c->A.callCompletionTool c name args)

snapshot :: Desktop -> IO (Maybe Snapshot)
snapshot d | inlineEligible d,Just w<-activeWindow d,Just doc<-activeDocument d,Just bid<-bufferId w=do
  let source=documentBuffer doc
      path=documentSyntaxPath doc
      absolute=if isAbsolute path then path else startingDirectory d </> path
      view=InlineView (windowId w) bid (revision source) (selection w) (inlineEpoch d) [] 0
  identity<-makeStableName =<< evaluate source
  pure (Just (Snapshot view source identity absolute))
snapshot _=pure Nothing

snapshotCurrent :: Desktop -> Snapshot -> IO Bool
snapshotCurrent d (Snapshot v _ identity _)
  | inlineMatches d v,Just doc<-activeDocument d=(==identity) <$> (makeStableName =<< evaluate (documentBuffer doc))
  | otherwise=pure False

prepareInput :: Snapshot -> T.Text -> Int -> IO CompletionInput
prepareInput (Snapshot v b _ path) intent serial=do
  let offset=caret (inlineSelection v)
      (row,_)=bufferLineColumn b offset
      (first,nearby)=context 40 row
      history=recent 3 b
      input=CompletionInput (T.pack (show serial)) intent path (contents b) (revision b) offset first nearby (toJSON history)
  _<-evaluate (T.length (inputText input)+sum (map T.length nearby)+length (show history))
  pure input
  where
    context :: Int -> Int -> (Int,[T.Text])
    context radius row=
      let first=max 0 (row-radius)
          rows=[bufferLineAt b n | n<-[first..min (bufferLineCount b-1) (row+radius)]]
      in if radius>0 && sum (map T.length rows)>16384 then context (radius-1) row else (first,rows)
    recent :: Int -> Buffer -> [Value]
    recent 0 _=[]
    recent n after=case undoStack after of
      []->[]
      _->let before=undo after
             (a,z,inserted)=fromMaybe (0,0,0) (lastChange after)
         in object ["startOffset" .= a,"oldText" .= bufferSlice before a (min 2048 (z-a)),
                    "newText" .= bufferSlice after a (min 2048 inserted)]:recent (n-1) before

request :: Autocomplete -> T.Text -> Desktop -> IO Desktop
request runtime intent d=do
  captured<-snapshot d
  forM_ captured $ \snap->do
    serial<-atomically $ do n<-readTVar (generation runtime); writeTVar (generation runtime) (n+1); pure (n+1)
    writeIORef (requested runtime) (Just snap)
    enqueue runtime (Request snap intent serial)
  pure d {status=if maybe False (const True) captured then "Requesting autocomplete..." else status d}

autocompleteEffects :: Autocomplete -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
autocompleteEffects runtime fallback d effects=foldM step (False,d) effects
  where
    step result@(True,_) _=pure result
    step (_,state) (AgentSidebarAction closedRequest) | Just job<-completionJob closedRequest=do
      enqueue runtime job
      pure (False,state)
    step (_,state) (AutocompleteAction action args)=do
      next<-case action of
        "propose"->request runtime action state
        "alternate-next"->request runtime action state
        "alternate-previous"->request runtime action state
        "hint"->mapM_ (enqueue runtime . Hint . T.take 16384) (take 1 args) >> pure state
        "settings"->enqueue runtime Settings >> pure state
        "save"->case args of
          button:backend:exe:arguments:model:effort:cp:cpArgs:debug:_ | button `elem` ["0","2","3"]->do
            enqueue runtime (Save (object ["provider" .= T.toLower backend,"executable" .= exe,"arguments" .= arguments,"model" .= model,"effort" .= effort,"copilotExecutable" .= cp,"copilotArguments" .= cpArgs,"debug" .= (debug=="true")]))
            when (button=="2") (enqueue runtime SignIn)
            when (button=="3") (enqueue runtime SignOut)
            atomically (modifyTVar' (generation runtime) (+1))
            pure (clearInline state)
          _->pure state
        "signin"->when (take 1 args==["0"]) (enqueue runtime FinishSignIn) >> pure state
        "shown"->do
          prior<-readIORef (displayed runtime)
          forM_ (prior >>= selectedOption) (enqueue runtime . Feedback Ignored . optionProposal)
          report Shown state
          writeIORef (displayed runtime) (inlinePreview state)
          pure state
        "accept"->accept False state
        "word"->accept True state
        _->pure state
      pure (False,next)
    step (_,state) effect=fallback state [effect]
    completionJob closedRequest=case closedRequest of
      ShowCompletion target->Just (Reveal target)
      ConfigureCompletion target option value->Just (Configure target option value)
      _->Nothing
    report outcome state=forM_ (inlinePreview state >>= selectedOption) (enqueue runtime . Feedback outcome . optionProposal)
    accept word state=case inlinePreview state of
      Just view | inlineMatches state view,Just opt<-selectedOption view,Just doc<-activeDocument state->do
        let p=optionProposal opt
            source=documentBuffer doc
            (a,z,text,rest)=if word then proposalWord source p else (proposalStart p,proposalEnd p,proposalText p,Nothing)
            changed=editActive (\_ -> replaceSelection (Selection a z) text) (Just (a+T.length text)) (clearInline state)
            outcome=case rest of Nothing->Accepted; Just _->PartiallyAccepted (optionPrefixLength opt+T.length text)
        enqueue runtime (Feedback outcome p)
        let next=case (rest,activeWindow changed,activeDocument changed) of
              (Just remaining,Just w,Just document)->case prepareOption (documentBuffer document) remaining of
                Right option->changed {inlinePreview=Just view {inlineRevision=revision (documentBuffer document),inlineSelection=selection w,
                  inlineGeneration=inlineEpoch changed,inlineIndex=0,inlineOptions=[option {optionPrefixLength=optionPrefixLength opt+T.length text+optionPrefixLength option}]}}
                Left _->changed
              _->changed
        writeIORef (displayed runtime) (inlinePreview next)
        writeIORef (requested runtime) Nothing
        pure next
      _->pure state

-- The UI drains already-prepared results and compares only scalar view state
-- and stable source identities; source text/history is never compared here.
tickAutocomplete :: Autocomplete -> Desktop -> IO Desktop
tickAutocomplete runtime original=do
  pending<-readIORef (requested runtime)
  forM_ pending $ \snap->snapshotCurrent original snap >>= \valid->when (not valid) $ do
    atomically (modifyTVar' (generation runtime) (+1))
    writeIORef (requested runtime) Nothing
  old<-readIORef (displayed runtime)
  forM_ old $ \view->when (not (inlineMatches original view) || maybe True (const False) (inlinePreview original)) $ do
    forM_ (selectedOption view) (enqueue runtime . Feedback Ignored . optionProposal)
    writeIORef (displayed runtime) Nothing
  drained<-drain original
  fresh<-atomicModifyIORef' (transcriptReady runtime) (\pendingTrace->(Nothing,pendingTrace))
  updated<-maybe (pure drained) (adopt drained . Transcript) fresh
  held<-readIORef (hold runtime)
  now<-toInteger <$> getMonotonicTimeNSec
  case (heldModifiers updated,held) of
    ([V.MAlt],Nothing)->writeIORef (hold runtime) (Just (now,inlineEpoch updated,False)) >> pure updated
    ([V.MAlt],Just (start,epoch,fired))
      | epoch/=inlineEpoch updated->writeIORef (hold runtime) (Just (start,epoch,True)) >> pure updated
      | not fired && now-start>=1000000000 && inlineEligible updated->do
          writeIORef (hold runtime) (Just (start,epoch,True))
          request runtime "propose" updated
      | otherwise->pure updated
    (mods,_) | V.MAlt `elem` mods->writeIORef (hold runtime) (Just (now,inlineEpoch updated,True)) >> pure updated
    _->writeIORef (hold runtime) Nothing >> pure updated
  where
    drain state=atomically (tryReadTQueue (replies runtime)) >>= maybe (pure state) (\reply->adopt state reply >>= drain)
    adopt state reply=case reply of
      Ready snap@(Snapshot view _ _ _) options->do
        valid<-snapshotCurrent state snap
        when valid (writeIORef (requested runtime) Nothing)
        if not valid then do
          forM_ options (enqueue runtime . Feedback Ignored . optionProposal)
          pure state
        else do
          let shown=view {inlineOptions=options}
          writeIORef (displayed runtime) (if null options then Nothing else Just shown)
          forM_ (selectedOption shown) (enqueue runtime . Feedback Shown . optionProposal)
          pure state {inlinePreview=if null options then Nothing else Just shown,status=if null options then "No autocomplete proposal." else ""}
      RevealTranscript expected->do
        current<-readIORef (summary runtime)
        if maybe True (\(CompletionSummary target _)->target/=expected) current || not (isNothing (dialog state)) then pure state {status="Completion view expired."} else do
          writeIORef (debugVisible runtime) True
          body<-readIORef (debugBody runtime)
          shown<-showTranscript runtime True body state
          pure (maybe shown (\w->focusWindow (windowId w) shown) (transcriptWindow shown))
      CompletionConfigured expected serial text->do
        current<-readIORef (summary runtime)
        valid<-((==serial) <$> readTVarIO (generation runtime))
        let accepted=valid && maybe False (\(CompletionSummary target _)->target==expected) current
        pure (if accepted then (clearInline state) {status=text} else state {status=text})
      Notice text->pure state {status=text}
      ShowSettings values->pure (settingsDialog values state)
      ShowSignIn code->pure state {dialog=Just (Dialog "Copilot sign in" (AutocompleteDialog "signin") [] 0 ["Continue","Cancel"] ["Enter this device code when asked:",code,"Continue opens the provider's sign-in flow."])}
      Configured visible acp->do
        previous<-readIORef (debugVisible runtime)
        writeIORef (debugVisible runtime) visible
        body<-readIORef (debugBody runtime)
        let configured=state {autocompleteACPEnabled=acp}
        if previous==visible then pure configured else showTranscript runtime visible body configured
      Transcript body->do
        writeIORef (debugBody runtime) body
        visible<-readIORef (debugVisible runtime)
        if visible then updateTranscript body state else pure state

settingsDialog :: Value -> Desktop -> Desktop
settingsDialog values d=d {dialog=Just (Dialog "Autocomplete" (AutocompleteDialog "save") fields' 0 ["OK","Cancel","Sign in","Sign out"] ["Separate from the main conversation."])}
  where
    value key fallback=case values of
      Object o->case KM.lookup key o of Just (String x)->x; _->fallback
      _->fallback
    input key label fallback=let text=value key fallback in Input label text (T.length text)
    visible=case values of Object o->KM.lookup "debug" o==Just (Bool True); _->False
    fields'=[ComboBox "Provider" ["Off","ACP","Copilot"] (case value "provider" "off" of "acp"->1; "copilot"->2; _->0) Nothing,input "executable" "ACP executable" "codex-acp",
      input "arguments" "ACP arguments (JSON)" "[]",input "model" "Model" "",input "effort" "Effort" "",
      input "copilotExecutable" "Copilot executable" "copilot-language-server",input "copilotArguments" "Copilot arguments (JSON)" "[\"--stdio\"]",
      CheckBox "Show completion chat" visible]

-- Preparation and bounded coalescing stay on the existing trace worker. The
-- host receives immutable rows, never a Buffer/Undo root or an opening callback.
traceLoop :: Autocomplete -> IO ()
traceLoop runtime=go ""
  where
    go old=do
      threadDelay 100000
      messages<-readIORef (connection runtime) >>= maybe (pure []) A.pollACPCompletionTranscript
      if null messages then go old else do
        let text=T.takeEnd 65536 (old<>T.intercalate "\n" messages<>"\n")
        body<-prepareTranscript text
        writeIORef (transcriptReady runtime) (Just body)
        go text

prepareTranscript :: T.Text -> IO W.PreparedWindow
prepareTranscript text=W.prepareRecoverableTextWindow "hide.autocomplete.transcript" 1 W.ReadableWindow "Autocomplete" text
  >>= either (ioError . userError . T.unpack) pure

transcriptWindow :: Desktop -> Maybe Window
transcriptWindow d=case [w | Just reference<-[autocompleteWindow d],w<-windows d,windowContent w==PluginContent reference] of
  w:_->Just w
  _->Nothing

-- A trace publication only refreshes its installed frame. Closing the window
-- leaves the latest prepared body with this owner and never implicitly reopens.
updateTranscript :: W.PreparedWindow -> Desktop -> IO Desktop
updateTranscript body d=case transcriptWindow d of
  Just w | PluginContent reference<-windowContent w->do
    update<-W.refreshWindow reference body
    maybe (pure d) (\publication->adoptWindowUpdate Menu.HumanMenu publication d) update
  _->pure d

showTranscript :: Autocomplete -> Bool -> W.PreparedWindow -> Desktop -> IO Desktop
showTranscript _ False _ d=do
  mapM_ W.retireWindowRef (autocompleteWindow d)
  pure $ case transcriptWindow d of
    Nothing->d {autocompleteWindow=Nothing}
    Just w->layoutProblems d (normalizeBottom d
      {windows=filter ((/=windowId w).windowId) (windows d),
       pluginWindows=maybe (pluginWindows d) (`M.delete` pluginWindows d) (autocompleteWindow d),
       retiredPluginWindows=maybe (retiredPluginWindows d) (`S.delete` retiredPluginWindows d) (autocompleteWindow d),
       autocompleteWindow=Nothing,dockedTerminals=M.delete (windowId w) (dockedTerminals d)})
showTranscript runtime True body d=case transcriptWindow d of
  Just _->updateTranscript body d
  Nothing->do
    update<-W.openWindow (transcriptScope runtime) body
    case update of
      Nothing->pure d
      Just publication->do
        added<-adoptWindowUpdate Menu.HumanMenu publication d
        let reference=W.updateWindowRef publication
        case [w | w<-windows added,windowContent w==PluginContent reference] of
          w:_->do
            let ident=windowId w
                rectangle=bounds w
                docked=layoutProblems d added {autocompleteWindow=Just reference,
                  dockedTerminals=M.insert ident (rectangle,Nothing) (dockedTerminals added),bottomTerminal=Just ident}
            pure (maybe docked (\previous->focusWindow (windowId previous) docked) (activeWindow d))
          _->W.retireWindowRef reference >> pure added
