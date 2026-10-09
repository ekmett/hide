{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : AutocompleteCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module AutocompleteCheck (checks) where

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
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
import Hide.AgentSidebarTypes
import Hide.Buffer
import Hide.Model
import qualified Hide.Plugin.Window as W
import qualified Hide.Plugin.Menu as Menu
import Hide.PluginWindowHost (adoptWindowUpdate,retireClosedWindow)
import Hide.GuestAccess (readableAt,pointerAllowedAt)

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->do
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
    withAutocomplete root $ \runtime->do
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
          focused=(focusWindow (windowId traceWindow) shown) {autocompleteFocused=False,autocompleteDraft=newBuffer "retained human hint"}
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
      check "late completion publication cannot reopen a closed frame" (not (hasTranscript stale) && contents (autocompleteDraft stale)=="retained human hint")
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
        (autocompleteWindow reopened/=Just oldRef && contents (autocompleteDraft reopened)=="retained human hint")
      let sourceFocused=maybe reopened (\w->focusWindow (windowId w) reopened) (activeWindow warm)
      hidden<-save runtime False sourceFocused >>= awaitDesktop runtime "debug pane toggle off" (not.hasTranscript)
      check "debug toggle keeps the source" (activeText hidden==large)
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
      configuredPreview<-awaitDesktop runtime "accepted setting invalidates preview" (\d->status d=="Completion setting updated." && inlinePreview d==Nothing) changedSetting
      seventhRequest<-send runtime "propose" [] configuredPreview
      seventh<-awaitPrompt 7
      seventhId<-submit runtime seventh "newer proposal\n"
      waitRetired runtime seventhId
      newerPreview<-awaitDesktop runtime "new preview after settings change" (isJust.inlinePreview) seventhRequest
      rejectedSetting<-configure oldTarget "effort-id" "high" newerPreview
      preserved<-awaitDesktop runtime "expired completion setting" (\d->status d=="Completion setting expired.") rejectedSetting
      check "expired setting leaves a newer inline proposal intact"
        (inlinePreview preserved==inlinePreview newerPreview && activeText preserved=="x\n")
      entries<-logs
      check "autocomplete configuration is independent of the main agent" (length [() | entry<-entries,field "method" entry==Just ("initialize"::T.Text)]==1)
  closed<-withAutocomplete root pure
  closedChoices<-timeout 5000000 (completionChoices closed (CompletionTarget 0 Nothing) "model")
  check "choice requests refuse after owner shutdown" (case closedChoices of Just (Left _)->True; _->False)
  putStrLn "Autocomplete runtime checks passed"

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
