{-# LANGUAGE OverloadedStrings #-}
module AutocompleteCheck (checks) where

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
import Hide.Buffer
import Hide.Model

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->do
  let script=root </> "provider.py"
      logPath=root </> "requests.jsonl"
      project=root </> "thc.toml"
      source="value = old\nnext = untouched\n"
      desktop=addDocument Nothing (newBuffer source) (initialDesktop (100,30))
      send runtime action args d=snd <$> autocompleteEffects runtime (\state _->pure (False,state)) d [AutocompleteAction action args]
      hasTranscript d=any ((==Just "Autocomplete").documentLabel) (M.elems (buffers d))
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
            (maybe (error "Missing source window") bufferId (activeWindow sameVersionRequest)) (buffers sameVersionRequest)}
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
      shown<-save runtime True settled >>= awaitDesktop runtime "debug pane toggle on" hasTranscript
      check "debug pane preserves focused source" (activeText shown==large)
      hidden<-save runtime False shown >>= awaitDesktop runtime "debug pane toggle off" (not.hasTranscript)
      check "debug toggle keeps the source" (activeText hidden==large)
      entries<-logs
      check "autocomplete configuration is independent of the main agent" (length [() | entry<-entries,field "method" entry==Just ("initialize"::T.Text)]==1)
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
