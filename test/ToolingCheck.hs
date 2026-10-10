-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- | Module      : ToolingCheck
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
module ToolingCheck (checks, allocationChecks, diagnosticCacheChecks, startupChecks, workspaceEditChecks, commandChecks) where
import AllocationProfile (AllocationProfile, withinBudget)
import SourceWindowFixture (sourceFixtureBuffer)
import Control.Monad (unless, when, forM_, replicateM, foldM, void)
import Control.Concurrent (threadDelay, newEmptyMVar, readMVar, putMVar, tryPutMVar, tryReadMVar)
import Control.Concurrent.Async (withAsync, poll, wait, cancel)
import Data.Aeson.Types (parseMaybe)
import Control.Exception (bracket, evaluate, finally, uninterruptibleMask_)
import System.Mem.StableName (makeStableName)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef', atomicModifyIORef')
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (findIndex)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath ((</>))
import System.IO (openTempFile, hClose)
import System.Timeout (timeout)
import GHC.Conc (getAllocationCounter)
import GHC.Clock (getMonotonicTimeNSec)
import Hide.Buffer
import Hide.BufferView (BufferView(..))
import qualified Hide.App as App
import Hide.Files (FileState(..), loadFile, saveFile)
import Hide.Model
import qualified Hide.LSP as L
import Hide.Tooling

-- | Keep measured JumpTo behavior independently selectable from HLS integration.
allocationChecks :: AllocationProfile -> IO ()
allocationChecks profile = do
  let check name ok=unless ok (error name)
  -- Exercise JumpTo itself without splitting preceding rows. The flat oracle
  -- and initial buffer preparation are outside the measured interaction.
  let jumpPath="/measured-jump.hs"
      original=newBuffer (T.replicate 50000 "a😀b\r\n"<>"last\r")
      edited=replaceSelection (Selection 0 1) "λ\n" original
  withTooling L.startClient $ \tooling->forM_ [original,edited,undo edited] $ \buffer->do
    let text=contents buffer
        positions=[(-1,-1),(0,2),(49999,2),(49999,3),(50000,99),(99999,99)]
    _<-evaluate (T.length text)
    _<-evaluate (prepareBuffer buffer)
    let opaque=buffer {undoStack=error "JumpTo forced Undo",redoStack=error "JumpTo forced Redo"}
        opened=addDocument (Just (FileState jumpPath Nothing)) opaque (initialDesktop (80,25))
    forM_ positions $ \(row,column)->do
      expected<-evaluate (L.positionOffset text (row,column))
      beforeJump<-getAllocationCounter
      (_,moved)<-toolingEffects tooling App.applyEffects opened [JumpTo jumpPath row column]
      check "JumpTo uses measured UTF16 positions on loaded/edited/Undo source"
        (fmap (caret.selection) (activeWindow moved)==Just expected &&
         fmap windowId (activeWindow moved)==fmap windowId (activeWindow opened) &&
         fmap bufferView (activeWindow moved)==Just CurrentView)
      afterJump<-getAllocationCounter
      check "JumpTo does not traverse or split preceding document rows" (withinBudget profile (beforeJump-afterJump) (1024*1024))

checks :: IO ()
checks = do
  let check name ok=unless ok (error name)
      pos n=object ["line" .= (0::Int),"character" .= (n::Int)]
      range a z=object ["start" .= pos a,"end" .= pos z]
      edit a z text=object ["range" .= range a z,"newText" .= (text::String)]
      item=object ["label" .= ("foobar"::String),"textEdit" .= edit 0 3 "foobar"]
      ds=addDocument Nothing (newBuffer "foo x") (initialDesktop (80,25))
  check "completion parses replacement" (completionItems "foo x" 3 (toJSON [item])==[Completion "foobar" [(0,3,"foobar")]])
  check "completion rejects invalid UTF16 range" (null (completionItems "😀x" 1 (toJSON [object ["label" .= ("bad"::String),"textEdit" .= edit 1 2 "bad"]])))
  let applied=applyCompletion [(0,3,"foobar"),(5,5,"!")] ds
  check "completion applies simultaneous edits" (activeText applied=="foobar x!")
  check "completion is one undo" (activeText (fst (runCommand Undo applied))=="foo x")
  check "completion rejects overlap" (buffers (applyCompletion [(0,3,"a"),(1,2,"b")] ds)==buffers ds)
  check "completion rejects coincident insertion ambiguity" (buffers (applyCompletion [(0,0,"a"),(0,0,"b")] ds)==buffers ds)
  check "workspace edit rejects resource operations" (case workspaceEdits (object ["documentChanges" .= [object ["kind" .= ("delete"::String),"uri" .= ("file:///tmp/A.hs"::String)]]]) of Left _ -> True; _ -> False)
  check "workspace edit handles versioned document" (workspaceEdits (object ["documentChanges" .= [object ["textDocument" .= object ["uri" .= ("file:///tmp/A.hs"::String),"version" .= (7::Int)],"edits" .= [edit 0 1 "a"]]]])==Right [("/tmp/A.hs",Just 7,[edit 0 1 "a"])])
  check "hover markdown fits status" (hoverText (object ["contents" .= object ["kind" .= ("markdown"::String),"value" .= ("```haskell\nfoo :: Int\n```\n"::String)]])=="foo :: Int")
  let hover text=hoverText (object ["contents" .= (text::T.Text)])
  check "hover types use Unicode syntax" (hover "f :: forall a. Eq a => a->a"=="f :: ∀ a. Eq a ⇒ a→a")
  check "hover keeps identifiers and quoted arrows" (hover "forallValue :: Proxy \"->\" -> a"=="forallValue :: Proxy \"->\" → a")
  check "unversioned diagnostics cannot masquerade as current edits" (not (diagnosticsCurrent Nothing [1]) && diagnosticsCurrent Nothing [0] && not (diagnosticsCurrent (Just 0) [1]))
  check "split buffer count unaffected" (M.size (buffers applied)==1)
  bracket temporary removePathForcibly $ \root -> do
    let server=root </> "fake-hls"
        source=root </> "Main.hs"
        text="foo = 1\n"
        desktop=addDocument (Just (FileState source Nothing)) (newBuffer text) (initialDesktop (80,25))
        core d _=pure (False,d)
    writeUtf8 server "#!/usr/bin/env python3\nimport time\ntime.sleep(30)\n"
    writeUtf8 source (T.unpack text)
    writeUtf8 (root </> "Bad.hs") "\NUL"
    withTooling (launchServer server) $ \tooling -> do
      let binary=desktop {buffers=M.map (\doc -> restyle doc {documentBuffer=newByteBuffer "a\0b"}) (buffers desktop)}
      (_,blocked)<-toolingEffects tooling core binary [LanguageRequest TypeInfo]
      check "hex buffers do not reach HLS text requests" (buffers blocked==buffers binary && status blocked=="Open a saved Haskell source file first.")
      began<-timeout 1000000 (toolingEffects tooling core desktop [LanguageRequest (RenameAt "bar")])
      (_,preparing)<-maybe (error "rename preparation blocked dispatch") pure began
      check "rename preparation returns before disk result" (status preparing `elem` ["Starting HLS...","Preparing rename..."])
      let await d = do
            next<-tickTooling tooling core d
            if "Cannot prepare rename:" `T.isPrefixOf` status next then pure next else threadDelay 10000 >> await next
      failed<-timeout 2000000 (await preparing)
      check "snapshot errors surface and preserve buffers" (maybe False (\d -> buffers d==buffers desktop) failed)
      (_,again)<-toolingEffects tooling core desktop [LanguageRequest (RenameAt "bar")]
      let changed=insertText "x" again
      stale<-tickTooling tooling core changed
      check "editing while preparing cancels rename" (status stale=="Rename target changed; request it again." && buffers stale==buffers changed)
    entered<-newEmptyMVar; release<-newEmptyMVar
    let holdSnapshot _ _=putMVar entered () >> readMVar release >> pure M.empty
    withToolingUsing (const (pure root)) (launchServer server) holdSnapshot loadFile $ \tooling -> do
      (_,requested)<-toolingEffects tooling core desktop [LanguageRequest (RenameAt "bar")]
      let awaitPreparation current=do
            next<-tickTooling tooling core current
            started<-tryReadMVar entered
            if started==Just () then pure next else threadDelay 1000 >> awaitPreparation next
      beforeSaveAs<-timeout 2000000 (awaitPreparation requested) >>= maybe (error "Save As fixture never entered snapshot preparation") pure
      check "Save As fixture holds an active preparation" (status beforeSaveAs=="Preparing rename...")
      let moved=beforeSaveAs {buffers=M.map (\doc -> doc {documentFile=Just (FileState (root </> "Elsewhere.hs") Nothing)}) (buffers beforeSaveAs)}
      renamedPath<-tickTooling tooling core moved
      check "Save As while preparing cancels original target" (status renamedPath=="Rename target changed; request it again." && buffers renamedPath==buffers moved)
  check "HLS exposes code action discovery and checked application"
    (all (`elem` toolingToolNames) ["lsp_code_actions","lsp_apply_code_action"])
  workspaceEditChecks
  diagnosticCacheChecks
  mcpChecks
  codeActionChecks
  commandChecks
  putStrLn "tooling checks passed"
  where
    temporary = do
      base<-getTemporaryDirectory
      (path,h)<-openTempFile base "thc-tooling-check"
      hClose h
      removeFile path
      createDirectory path
      canonicalizePath path

-- Gates cover the owning filesystem/process startup boundaries. Dispatch and
-- ordinary editing must finish while either operation is held indefinitely.
startupChecks :: IO ()
startupChecks = bracket temporary removePathForcibly $ \root -> do
  let source=root </> "Main.hs"; other=root </> "Other.hs"; server=root </> "fake-hls"
      base=addDocument (Just (FileState source Nothing)) (newBuffer "foo = 1\n") (initialDesktop (80,25))
      bid=maybe (error "missing source") sourceFixtureBuffer (activeWindow base)
      desktop=addDocument (Just (FileState other Nothing)) (newBuffer "bar = 2\n") base
      arguments=object ["bufferId" .= bid,"line" .= (1::Int),"column" .= (1::Int)]
      core d _=pure (False,d)
      check label ok=unless ok (error label)
      bounded label action=timeout 1000000 action >>= maybe (error label) pure
      -- The 30s query deadline owns external completion (initialize has 20s).
      -- Leave it time to return its error, while every UI tick stays bounded.
      pump label tooling d predicate=timeout 31000000 (loop d) >>= maybe (error (label++": receipt did not complete within the HLS request lifetime")) pure
        where loop state=do
                next<-bounded (label++": UI tick blocked") (tickTooling tooling core state)
                ready<-predicate next
                if ready then pure next else threadDelay 1000 >> loop next
      finish label tooling d answer=withAsync answer $ \worker -> do
        next<-pump label tooling d (const (maybe False (const True) <$> poll worker))
        (next,) <$> wait worker
      isLeft (Left _)=True; isLeft _=False
      field key=parseMaybe (withObject "result" (.: key))
  writeUtf8 source "foo = 1\n"; writeUtf8 other "bar = 2\n"
  writeUtf8 server mcpServer
  discovered<-newEmptyMVar; discoveryGate<-newEmptyMVar
  started<-newEmptyMVar; startupGate<-newEmptyMVar
  rootsRead<-newIORef (0::Int); launches<-newIORef (0::Int)
  let discover _=do
        modifyIORef' rootsRead (+1)
        void (tryPutMVar discovered ())
        readMVar discoveryGate
        pure root
      launch path=do
        modifyIORef' launches (+1)
        putMVar started ()
        readMVar startupGate
        launchServer server path
  withToolingUsing discover launch (\_ _->pure M.empty) loadFile $ \tooling -> do
    (queued,answer)<-bounded "cold HLS dispatch blocked" (toolingTool tooling core desktop "lsp_hover" arguments)
    bounded "discovery was not started" (readMVar discovered)
    ticked<-bounded "held discovery blocked tick" (tickTooling tooling core queued)
    check "input progresses during root discovery" (activeText (insertText "x" ticked)=="xbar = 2\n")
    withAsync answer $ \waiting->do
      check "request remains queued during discovery" . maybe True (const False) =<< poll waiting
      putMVar discoveryGate ()
      processPending<-pump "launcher entry after discovery" tooling ticked (const (maybe False (const True) <$> tryReadMVar started))
      _<-bounded "held process startup blocked tick" (tickTooling tooling core processPending)
      -- Ordinary UI requests and saved notifications share the same startup.
      (_,human)<-bounded "human operation blocked startup" (toolingEffects tooling core base [LanguageRequest TypeInfo])
      check "human startup request is accepted" (status human=="Starting HLS...")
      (_,saved)<-toolingEffects tooling core desktop [SaveDocument bid Nothing Nothing]
      putMVar startupGate ()
      ready<-pump "initial hover reply after startup" tooling saved (const (maybe False (const True) <$> poll waiting))
      reply<-wait waiting
      let raw=either (const Nothing) (field "result") reply :: Maybe Value
      check "deferred request observes didOpen text" ((raw >>= field "text")==Just ("foo = 1\n"::T.Text))
      _<-pump "didSave receipt" tooling ready (const (doesFileExist (root </> "saved.marker")))
      check "same root launches only one HLS" . (==1) =<< readIORef launches
      check "one coalesced discovery per path" . (==2) =<< readIORef rootsRead
  -- Never dispatch a captured request after editing, equal-revision reload,
  -- Save As, closing the buffer, or marking its source private.
  forM_ [ insertText "x" base
        , base {buffers=M.adjust (\doc->restyle doc {documentBuffer=newBuffer "different\n"}) bid (buffers base)}
        , base {buffers=M.adjust (\doc->doc {documentFile=Just (FileState other Nothing)}) bid (buffers base)}
        , base {buffers=M.empty,windows=[]}
        , base {guestPrivatePaths=[source]}] $ \changed->do
    gate<-newEmptyMVar
    withToolingUsing (\_->readMVar gate >> pure root) (launchServer server) (\_ _->pure M.empty) loadFile $ \tooling->do
      (_,answer)<-toolingTool tooling core base "lsp_hover" arguments
      stale<-tickTooling tooling core changed
      reply<-bounded "stale deferred request did not fail" answer
      check "stale startup cannot dispatch or mutate" (isLeft reply && buffers stale==buffers changed)
  -- Overflow, waiter cancellation and restart all complete explicitly.
  gate<-newEmptyMVar; cancelled<-newEmptyMVar
  withToolingUsing (\_->readMVar gate `finally` void (tryPutMVar cancelled ()) >> pure root) (launchServer server) (\_ _->pure M.empty) loadFile $ \tooling->do
    requests<-replicateM 32 (snd <$> toolingTool tooling core base "lsp_hover" arguments)
    (_,overflow)<-toolingTool tooling core base "lsp_hover" arguments
    check "startup queue overflow is explicit" . isLeft =<< bounded "overflow blocked" overflow
    first:rest<-pure requests
    withAsync first $ \waiting->threadDelay 1000 >> cancel waiting
    check "cancelled startup waiter completes" . isLeft =<< bounded "cancelled waiter was stranded" first
    _<-tickTooling tooling core base
    (_,replacement)<-toolingTool tooling core base "lsp_hover" arguments
    restarted<-bounded "restart blocked on startup cancellation" (toolingEffects tooling core base [LanguageRequest RestartLanguage])
    check "restart cancels queued requests" . all isLeft =<< mapM (bounded "restart stranded waiter") (rest++[replacement])
    check "restart keeps desktop intact" (buffers (snd restarted)==buffers base)
  bounded "shutdown failed to join discovery worker" (readMVar cancelled)
  -- A canceled launcher may be inside an uninterruptible process operation.
  -- Its root remains reserved until the result can be closed by its owner.
  launched<-newEmptyMVar; release<-newEmptyMVar; calls<-newIORef (0::Int)
  let heldLaunch path=do
        count<-atomicModifyIORef' calls (\n->(n+1,n))
        client<-launchServer server path
        when (count==0) (putMVar launched () >> uninterruptibleMask_ (readMVar release))
        pure client
  withToolingUsing (const (pure root)) heldLaunch (\_ _->pure M.empty) loadFile $ \tooling->flip finally (void (tryPutMVar release ())) $ do
    (_,firstReply)<-toolingTool tooling core base "lsp_hover" arguments
    pending<-pump "held launcher entry" tooling base (const (maybe False (const True) <$> tryReadMVar launched))
    (_,restarted)<-bounded "restart waited for held process operation" (toolingEffects tooling core pending [LanguageRequest RestartLanguage])
    check "restart completes original process-start waiter" . isLeft =<< bounded "process-start waiter stranded" firstReply
    (_,nextReply)<-toolingTool tooling core restarted "lsp_hover" arguments
    held<-foldM (\d _->tickTooling tooling core d <* threadDelay 1000) restarted [1..10::Int]
    check "retiring startup reserves its root" . (==1) =<< readIORef calls
    putMVar release ()
    (_,reply)<-finish "hover after launcher retirement" tooling held nextReply
    check "reopen resumes after startup cleanup" (not (isLeft reply))
    check "one replacement starts after old root retires" . (==2) =<< readIORef calls
  entered<-newEmptyMVar; releaseDiscovery<-newEmptyMVar; discoveries<-newIORef (0::Int)
  let heldDiscovery _=do
        modifyIORef' discoveries (+1)
        void (tryPutMVar entered ())
        uninterruptibleMask_ (readMVar releaseDiscovery)
        pure root
  withToolingUsing heldDiscovery (launchServer server) (\_ _->pure M.empty) loadFile $ \tooling->flip finally (void (tryPutMVar releaseDiscovery ())) $ do
    (_,originalReply)<-toolingTool tooling core base "lsp_hover" arguments
    bounded "root discovery did not enter" (readMVar entered)
    forM_ [1..3::Int] $ \_->do
      _<-bounded "restart blocked on held root discovery" (toolingEffects tooling core base [LanguageRequest RestartLanguage])
      _<-tickTooling tooling core base
      pure ()
    check "repeated restart keeps discovery worker bounded" . (==1) =<< readIORef discoveries
    check "restarted discovery completes old waiter" . isLeft =<< bounded "old root waiter stranded" originalReply
    (_,answer)<-toolingTool tooling core base "lsp_hover" arguments
    putMVar releaseDiscovery ()
    (_,reply)<-finish "hover after discovery retirement" tooling base answer
    check "root discovery resumes after retirement" (not (isLeft reply))
    check "only one replacement discovery runs" . (==2) =<< readIORef discoveries
  -- Canceling a snapshot does not release its slot while an uninterruptible
  -- filesystem operation still owns the old preparation worker.
  preparingEntered<-newEmptyMVar; releasePreparation<-newEmptyMVar; preparations<-newIORef (0::Int)
  let heldSnapshot _ _=do
        modifyIORef' preparations (+1)
        void (tryPutMVar preparingEntered ())
        uninterruptibleMask_ (readMVar releasePreparation)
        pure M.empty
  withToolingUsing (const (pure root)) (launchServer server) heldSnapshot loadFile $ \tooling->flip finally (void (tryPutMVar releasePreparation ())) $ do
    (_,warmReply)<-toolingTool tooling core base "lsp_hover" arguments
    _<-finish "hover before snapshot cancellation" tooling base warmReply
    (_,first)<-toolingEffects tooling core base [LanguageRequest (RenameAt "first")]
    bounded "edit snapshot did not enter" (readMVar preparingEntered)
    (_,replaced)<-bounded "rename replacement blocked cancellation" (toolingEffects tooling core first [LanguageRequest (RenameAt "replacement")])
    check "rename explicitly rejects a retiring preparation" ("still stopping" `T.isInfixOf` status replaced)
    forM_ [1..3::Int] $ \_->do
      (_,restarted)<-bounded "restart blocked on snapshot cancellation" (toolingEffects tooling core base [LanguageRequest RestartLanguage])
      (_,queued)<-toolingEffects tooling core restarted [LanguageRequest (RenameAt "replacement")]
      _<-pump "rename rejection during snapshot retirement" tooling queued (pure . T.isInfixOf "still stopping" . status)
      pure ()
    let renameArgs=object ["bufferId" .= bid,"revision" .= (0::Int),"line" .= (1::Int),"column" .= (1::Int),"newName" .= ("replacement"::T.Text)]
    (_,blockedReply)<-toolingTool tooling core base "lsp_rename" renameArgs
    check "MCP rename rejects a retiring preparation" . isLeft =<< bounded "retiring snapshot stranded MCP" blockedReply
    check "restart and rename retain one preparation worker" . (==1) =<< readIORef preparations
    putMVar releasePreparation ()
    let resume state=do
          ticked<-tickTooling tooling core state
          (_,requested)<-toolingEffects tooling core ticked [LanguageRequest (RenameAt "resumed")]
          if status requested=="Preparing rename..." then pure requested else threadDelay 1000 >> resume requested
    ready<-bounded "rename did not resume after preparation cleanup" (resume base)
    _<-pump "replacement snapshot entry" tooling ready (const ((==2) <$> readIORef preparations))
    check "one replacement snapshot starts after cleanup" . (==2) =<< readIORef preparations
  -- A process failure is cached until explicit restart, avoiding a spawn loop.
  attempts<-newIORef (0::Int)
  withToolingUsing (const (pure root)) (\_->modifyIORef' attempts (+1) >> ioError (userError "held startup failed")) (\_ _->pure M.empty) loadFile $ \tooling->do
    (_,answer)<-toolingTool tooling core base "lsp_hover" arguments
    (failed,reply)<-finish "failed launch reply" tooling base answer
    check "startup failure reaches original waiter" (isLeft reply)
    _<-foldM (\d _->tickTooling tooling core d) failed [1..10::Int]
    check "startup failure is not retried every tick" . (==1) =<< readIORef attempts
  putStrLn "HLS startup checks passed"
  where
    temporary=do
      base<-getTemporaryDirectory
      (path,h)<-openTempFile base "thc-startup-check"
      hClose h; removeFile path; createDirectory path; canonicalizePath path

-- The mock observes didOpen/didChange before each request, and can hold rename
-- replies while the buffer, disk or waiting client changes independently.
mcpChecks :: IO ()
mcpChecks = bracket temporary removePathForcibly $ \root -> do
  let source=root </> "Main.hs"
      other=root </> "Util.hs"
      server=root </> "fake-hls"
      disk="foo = 1\n" :: T.Text
      base=addDocument (Just (FileState source Nothing)) (newBuffer disk) (initialDesktop (80,25))
      live=insertText "😀 " base
      bid=maybe (error "no source window") sourceFixtureBuffer (activeWindow live)
      rev=revision . documentBuffer . (M.! bid) . buffers
      args d extra=object (["bufferId" .= bid,"line" .= (1::Int),"column" .= (3::Int),"revision" .= rev d]++extra)
      core d _=pure (False,d)
      check label ok=unless ok (error label)
      field key=parseMaybe (withObject "value" (.: key))
      isLeft (Left _)=True; isLeft _=False
      awaitFile name=timeout 3000000 (loop (doesFileExist (root </> name))) >>= maybe (error ("Missing mock marker "++name)) pure
      loop test=test >>= \done -> unless done (threadDelay 1000 >> loop test)
      finish tooling d answer=withAsync answer $ \worker -> do
        let pump state=do
              next<-tickTooling tooling core state
              result<-poll worker
              case result of
                Just _ -> (next,) <$> wait worker
                Nothing -> threadDelay 1000 >> pump next
        timeout 3000000 (pump d) >>= maybe (error "HLS tool reply timed out") pure
      held tooling d name=do
        (unchanged,answer)<-toolingTool tooling core d "lsp_rename" (args d ["newName" .= (name::T.Text)])
        let pump state=do
              next<-tickTooling tooling core state
              exists<-doesFileExist (root </> T.unpack name<>".requested")
              if exists then pure next else threadDelay 1000 >> pump next
        ready<-timeout 3000000 (pump unchanged) >>= maybe (error "Rename never reached HLS") pure
        pure (ready,answer)
      textAt path d=contents . documentBuffer <$> findDocument path d
      findDocument path d=case [doc | doc<-M.elems (buffers d),fmap filePath (documentFile doc)==Just path] of doc:_ -> Just doc; _ -> Nothing
  writeUtf8 source (T.unpack disk)
  writeUtf8 other "foo = 2\n"
  writeUtf8 server mcpServer
  withTooling (launchServer server) $ \tooling -> do
    forM_ [("lsp_hover","textDocument/hover"),("lsp_definition","textDocument/definition"),("lsp_type_definition","textDocument/typeDefinition"),("lsp_references","textDocument/references"),("lsp_document_symbols","textDocument/documentSymbol")] $ \(name,method) -> do
      let parameters=if name=="lsp_document_symbols" then object ["bufferId" .= bid] else args live ["includeDeclaration" .= False | name=="lsp_references"]
      (unchanged,answer)<-toolingTool tooling core live name parameters
      (updated,result)<-finish tooling unchanged answer
      let raw=either (const Nothing) (field "result") result :: Maybe Value
      check "HLS tool uses requested method" ((raw >>= field "method")==Just (method::T.Text))
      check "HLS tools synchronize unsaved Unicode source" ((raw >>= field "text")==Just ("😀 foo = 1\n"::T.Text))
      unless (name/="lsp_references") $ check "references forwards declaration preference" ((raw >>= field "context")==Just (object ["includeDeclaration" .= False]))
      check "HLS reads do not create dialogs or change buffers" (dialog updated==Nothing && buffers updated==buffers live)
      if name=="lsp_document_symbols" then check "symbols has no cursor position" ((raw >>= field "position") == Just Null)
      else check "one-based codepoint position converts to UTF16" ((raw >>= field "position")==Just (object ["line" .= (0::Int),"character" .= (3::Int)]))
    forM_ [object ["bufferId" .= bid,"line" .= (0::Int),"column" .= (1::Int)],args live ["column" .= (99::Int)],args live ["revision" .= (999::Int)]] $ \bad -> do
      (_,answer)<-toolingTool tooling core live "lsp_hover" bad
      result<-answer
      check "HLS rejects invalid range or stale revision" (isLeft result)
    (_,missingRevision)<-toolingTool tooling core live "lsp_rename" (object ["bufferId" .= bid,"line" .= (1::Int),"column" .= (3::Int),"newName" .= ("bar"::T.Text)])
    missing<-missingRevision
    check "rename requires revision" (isLeft missing)
    let binary=live {buffers=M.adjust (\doc -> doc {documentBuffer=newByteBuffer "x"}) bid (buffers live)}
        untitled=live {buffers=M.adjust (\doc -> doc {documentFile=Nothing}) bid (buffers live)}
    forM_ [binary,untitled] $ \bad -> do
      (_,answer)<-toolingTool tooling core bad "lsp_hover" (args bad [])
      answer >>= check "HLS rejects binary and untitled buffers" . isLeft
    (unchanged,rename)<-toolingTool tooling core live "lsp_rename" (args live ["newName" .= ("bar"::T.Text)])
    (renamed,reply)<-finish tooling unchanged rename
    check "rename reports success without a dialog" (not (isLeft reply) && dialog renamed==Nothing)
    check "rename updates open and previously closed buffers" (textAt source renamed==Just "😀 bar = 1\n" && textAt other renamed==Just "bar = 2\n")
    onDisk<-readUtf8 source; otherDisk<-readUtf8 other
    check "rename never saves files" (onDisk==T.unpack disk && otherDisk=="foo = 2\n")
    (waitingOther,otherReply)<-held tooling renamed "hold_other"
    let editedOther=waitingOther {buffers=M.map (\doc -> if fmap filePath (documentFile doc)==Just other then doc {documentBuffer=replaceSelection (Selection 0 0) "x" (documentBuffer doc)} else doc) (buffers waitingOther)}
    writeUtf8 (root </> "hold_other.release") ""
    (afterOther,staleOther)<-finish tooling editedOther otherReply
    check "rename rejects changes in another edited buffer atomically" (isLeft staleOther && buffers afterOther==buffers editedOther && dialog afterOther==Nothing)
    (waiting,late)<-held tooling live "hold_stale"
    let edited=insertText "x" waiting
    writeUtf8 (root </> "hold_stale.release") ""
    (after,stale)<-finish tooling edited late
    check "late rename cannot overwrite a changed target" (isLeft stale && buffers after==buffers edited && dialog after==Nothing)
    (waitingDisk,diskReply)<-held tooling live "hold_disk"
    writeUtf8 other "changed on disk\n"
    writeUtf8 (root </> "hold_disk.release") ""
    (afterDisk,staleDisk)<-finish tooling waitingDisk diskReply
    check "rename rejects intervening closed-file changes atomically" (isLeft staleDisk && buffers afterDisk==buffers live && dialog afterDisk==Nothing)
    writeUtf8 other "foo = 2\n"
    (waitingCancel,cancelled)<-held tooling live "hold_cancel"
    _<-timeout 1000 cancelled
    writeUtf8 (root </> "hold_cancel.release") ""
    awaitFile "hold_cancel.replied"
    (barrierDesktop,barrier)<-toolingTool tooling core waitingCancel "lsp_hover" (args waitingCancel [])
    (afterCancel,barrierReply)<-finish tooling barrierDesktop barrier
    check "post-cancel response barrier completes" (not (isLeft barrierReply))
    cancellation<-cancelled
    check "cancelled rename cannot apply a later reply" (isLeft cancellation && buffers afterCancel==buffers live && dialog afterCancel==Nothing)
    (_,errorReply)<-toolingTool tooling core live "lsp_rename" (args live ["newName" .= ("error"::T.Text)])
    (afterError,hlsError)<-finish tooling live errorReply
    check "HLS errors return without dialogs or edits" (isLeft hlsError && buffers afterError==buffers live && dialog afterError==Nothing)
    _<-replicateM 32 (toolingTool tooling core live "lsp_hover" (args live []))
    (_,saturated)<-toolingTool tooling core live "lsp_hover" (args live [])
    saturated >>= check "outstanding HLS tool calls are bounded" . isLeft
  where
    temporary=do
      base<-getTemporaryDirectory
      (path,h)<-openTempFile base "thc-hls-tools"
      hClose h; removeFile path; createDirectory path
      canonicalizePath path

-- Hold an actual closed-file read after obtaining its bytes. The current
-- desktop can change independently until the prepared all-file patch adopts.
workspaceEditChecks :: IO ()
workspaceEditChecks = bracket temporary removePathForcibly $ \root->do
  let source=root </> "Main.hs"; other=root </> "Util.hs"; extra=root </> "Extra.hs"; server=root </> "fake-hls"
      initial=addDocument (Just (FileState source Nothing)) (newBuffer "😀 foo = 1\n") (initialDesktop (80,25))
      sourceId=maybe (error "missing source") sourceFixtureBuffer (activeWindow initial)
      withOther=addDocument (Just (FileState other Nothing)) (newBuffer "foo = 2\n") initial
      otherId=maybe (error "missing other") sourceFixtureBuffer (activeWindow withOther)
      base=addDocument Nothing (newBuffer "scratch\n") withOther
      scratchId=maybe (error "missing scratch") sourceFixtureBuffer (activeWindow base)
      snapshot d _=pure (M.union (M.fromList [(filePath file,(revision (documentBuffer doc),contents (documentBuffer doc))) | doc<-M.elems (buffers d),Just file<-[documentFile doc]]) (M.singleton extra (-1,"third = 3\n")))
      args=object ["bufferId" .= sourceId,"revision" .= (0::Int),"line" .= (1::Int),"column" .= (3::Int)]
      core d _=pure (False,d)
      check label ok=unless ok (error label)
      bounded label action=timeout 2000000 action >>= maybe (error label) pure
      field key=parseMaybe (withObject "result" (.:key))
      right=either (error . T.unpack) pure
      isLeft (Left _)=True; isLeft _=False
      pump tooling d predicate=bounded "workspace edit did not progress" (loop d)
        where loop state=do
                next<-tickTooling tooling core state
                ready<-predicate next
                if ready then pure next else threadDelay 1000 >> loop next
      finish tooling d answer=withAsync answer $ \waiting->do
        updated<-pump tooling d (const (maybe False (const True) <$> poll waiting))
        (updated,) <$> wait waiting
      begin tooling current=do
        (listing,listed)<-toolingTool tooling core current "lsp_code_actions" args
        (_,offered)<-finish tooling listing listed
        value<-right offered
        let identifiers=[key | item<-maybe [] id (field "actions" value),field "title" item==Just ("Fix source"::T.Text),Just key<-[field "actionId" item]]
        key<-case identifiers of one:_->pure (one::T.Text); _->error "No prepared action"
        toolingTool tooling core current "lsp_apply_code_action" (object ["bufferId" .= sourceId,"revision" .= (0::Int),"actionId" .= key])
      withHeld action=do
        entered<-newEmptyMVar; release<-newEmptyMVar
        let readHeld path=do
              loaded<-loadFile path
              when (path==extra) (void (tryPutMVar entered ()) >> uninterruptibleMask_ (readMVar release))
              pure loaded
        withToolingUsing (const (pure root)) (launchServer server) snapshot readHeld $ \tooling->flip finally (void (tryPutMVar release ())) $ do
          (queued,answer)<-begin tooling base
          held<-pump tooling queued (const (maybe False (const True) <$> tryReadMVar entered))
          ticked<-bounded "closed-file preparation blocked tick" (tickTooling tooling core held)
          check "input progresses while workspace read is held" (activeText (insertText "typed " ticked)=="typed scratch\n")
          action tooling ticked answer release
      editOther f d=d {buffers=M.adjust (\doc->restyle doc {documentBuffer=f (documentBuffer doc)}) otherId (buffers d)}
      textAt bid d=contents (documentBuffer (buffers d M.! bid))
  writeUtf8 source "😀 foo = 1\n"; writeUtf8 other "foo = 2\n"; writeUtf8 extra "third = 3\n"
  writeUtf8 server mcpServer
  withHeld $ \tooling held answer release->do
    let oldLength=bufferLength (documentBuffer (buffers held M.! sourceId))
        moved=(insertText "typed " held) {windows=map (\w->if sourceFixtureBuffer w==sourceId then w {selection=Selection oldLength oldLength,bounds=Rect 2 3 25 8} else w) (windows held)}
    putMVar release ()
    (adopted,reply)<-finish tooling moved answer
    check "prepared batch succeeds" (not (isLeft reply) && textAt sourceId adopted=="😀 fixed = 1\n" && textAt otherId adopted=="fixed = 2\n")
    check "adoption preserves unrelated input" (textAt scratchId adopted=="typed scratch\n")
    let sourceWindows=[w | w<-windows adopted,sourceFixtureBuffer w==sourceId]
    check "adoption preserves moved windows and rebases current selection" (case sourceWindows of w:_->bounds w==Rect 2 3 25 8 && selection w==Selection (oldLength+2) (oldLength+2); _->False)
    check "each affected live buffer gets one undo step" (contents (undo (documentBuffer (buffers adopted M.! sourceId)))==textAt sourceId base && contents (undo (documentBuffer (buffers adopted M.! otherId)))==textAt otherId base)
    check "closed target becomes an unsaved buffer" (any (\doc->fmap filePath (documentFile doc)==Just extra && contents (documentBuffer doc)=="fixed = 3\n" && dirty (documentBuffer doc)) (M.elems (buffers adopted)))
  forM_ [ ("edit",editOther (replaceSelection (Selection 0 0) "x"))
        , ("equal-revision reload",editOther (const (newBuffer "reload\n")))
        , ("save baseline",editOther markSaved)
        , ("file baseline",\d->d {buffers=M.adjust (\doc->doc {documentFile=Just (FileState other (Just "new baseline"))}) otherId (buffers d)})
        , ("Save As",\d->d {buffers=M.adjust (\doc->doc {documentFile=Just (FileState (root </> "Moved.hs") Nothing)}) otherId (buffers d)})
        , ("close",\d->d {buffers=M.delete otherId (buffers d),windows=filter ((/=otherId) . sourceFixtureBuffer) (windows d)})
        , ("open closed target",addDocument (Just (FileState extra Nothing)) (newBuffer "new unsaved contents\n"))
        , ("private target",\d->d {guestPrivatePaths=[extra]})] $ \(label,change)->
    withHeld $ \tooling held answer release->do
      let changed=change held
      putMVar release ()
      (rejected,reply)<-finish tooling changed answer
      check ("workspace adoption rejects "++label++" atomically") (isLeft reply && buffers rejected==buffers changed)
  withHeld $ \tooling held answer release->do
    -- The checked read already completed. A subsequent external write must
    -- remain protected by the FileState baseline when the user later saves.
    writeUtf8 extra "external update\n"
    putMVar release ()
    (adopted,reply)<-finish tooling held answer
    check "prepared closed-file edit retains its checked bytes" (not (isLeft reply))
    let targets=[(file,documentBuffer doc) | doc<-M.elems (buffers adopted),Just file<-[documentFile doc],filePath file==extra]
    target<-case targets of one:_->pure one; _->error "Missing prepared closed-file buffer"
    result<-uncurry saveFile target
    check "save refuses a post-read external disk change" (isLeft result)
    check "failed save preserves external bytes" . (=="external update\n") =<< readUtf8 extra
    writeUtf8 extra "third = 3\n"
  withHeld $ \tooling held answer release->do
    withAsync answer $ \waiting->threadDelay 1000 >> cancel waiting
    putMVar release ()
    (unchanged,reply)<-finish tooling held answer
    check "cancelled workspace adoption changes no buffers" (isLeft reply && buffers unchanged==buffers held)
  withHeld $ \tooling held answer release->do
    (_,restarted)<-bounded "restart blocked on workspace read" (toolingEffects tooling core held [LanguageRequest RestartLanguage])
    putMVar release ()
    reply<-bounded "restart stranded workspace waiter" answer
    check "restart cancels prepared workspace edit" (isLeft reply && buffers restarted==buffers held)
  entered<-newEmptyMVar; release<-newEmptyMVar; readCount<-newIORef (0::Int)
  let heldRead path=do
        loaded<-loadFile path
        when (path==extra) $ do
          modifyIORef' readCount (+1)
          void (tryPutMVar entered ())
          uninterruptibleMask_ (readMVar release)
        pure loaded
  withToolingUsing (const (pure root)) (launchServer server) snapshot heldRead $ \tooling->flip finally (void (tryPutMVar release ())) $ do
    (queued,original)<-begin tooling base
    _<-pump tooling queued (const (maybe False (const True) <$> tryReadMVar entered))
    (_,restarted)<-bounded "restart waited for uninterruptible workspace read" (toolingEffects tooling core base [LanguageRequest RestartLanguage])
    check "restart completes original workspace waiter" . isLeft =<< bounded "old workspace waiter stranded" original
    forM_ [1..3::Int] $ \_->do
      (pending,answer)<-begin tooling restarted
      _<-tickTooling tooling core pending
      threadDelay 1000
      check "retiring workspace read reserves the sole worker" . (==1) =<< readIORef readCount
      _<-bounded "repeated restart blocked workspace retirement" (toolingEffects tooling core pending [LanguageRequest RestartLanguage])
      check "queued workspace cancellation is explicit" . isLeft =<< bounded "queued workspace waiter stranded" answer
    putMVar release ()
    (pending,answer)<-begin tooling base
    (_,reply)<-finish tooling pending answer
    check "workspace preparation resumes after retirement" (not (isLeft reply))
    check "only one replacement workspace read starts" . (==2) =<< readIORef readCount
  putStrLn "workspace edit checks passed"
  where
    temporary=do
      base<-getTemporaryDirectory
      (path,h)<-openTempFile base "thc-workspace-edit"
      hClose h; removeFile path; createDirectory path; canonicalizePath path

-- Real stdio fixtures keep handles, resolution, and edits on the normal pump.
codeActionChecks :: IO ()
codeActionChecks = bracket temporary removePathForcibly $ \root -> do
  let source=root </> "Main.hs"; other=root </> "Util.hs"; private=root </> "Secret.hs"
      server=root </> "fake-hls"
      live=addDocument (Just (FileState source Nothing)) (newBuffer "😀 foo = 1\n") (initialDesktop (80,25))
      bid=maybe (error "missing source") sourceFixtureBuffer (activeWindow live)
      rev d=revision (documentBuffer (buffers d M.! bid))
      core d _=pure (False,d)
      args d=object ["bufferId" .= bid,"revision" .= rev d,"line" .= (1::Int),"column" .= (3::Int),"endLine" .= (1::Int),"endColumn" .= (6::Int)]
      applyArgs d key=object ["bufferId" .= bid,"revision" .= rev d,"actionId" .= key]
      field key=parseMaybe (withObject "value" (.:key))
      right=either (error . T.unpack) pure
      check label condition=unless condition (error label)
      isLeft (Left _)=True; isLeft _=False
      finish tooling d answer=withAsync answer $ \worker -> do
        let pump current=do
              next<-tickTooling tooling core current
              result<-poll worker
              case result of Just _->(next,) <$> wait worker; _->threadDelay 1000 >> pump next
        timeout 4000000 (pump d) >>= maybe (error "Code action response timed out") pure
      list tooling d=do
        (pending,answer)<-toolingTool tooling core d "lsp_code_actions" (args d)
        (_,result)<-finish tooling pending answer
        right result
      action title result=case [v | v<-maybe [] id (field "actions" result),field "title" v==Just (title::T.Text)] of
        value:_->value
        _->error ("Missing action "++T.unpack title)
      ident title result=maybe (error "Missing action ID") id (field "actionId" (action title result)::Maybe T.Text)
      apply tooling d key=do
        (pending,answer)<-toolingTool tooling core d "lsp_apply_code_action" (applyArgs d key)
        finish tooling pending answer
      awaitFile tooling d path=do
        let loop current=do
              next<-tickTooling tooling core current
              exists<-doesFileExist path
              if exists then pure next else threadDelay 1000 >> loop next
        timeout 3000000 (loop d) >>= maybe (error "Resolve never reached server") pure
  writeUtf8 source "😀 foo = 1\n"; writeUtf8 other "foo = 2\n"; writeUtf8 private "secret = 3\n"
  writeUtf8 server mcpServer
  advance <- newIORef 0
  let clock = (+) <$> (toInteger <$> getMonotonicTimeNSec) <*> readIORef advance
  withToolingClock clock (launchServer server) $ \tooling -> do
    (waiting,answer)<-toolingTool tooling core live "lsp_hover" (args live)
    _<-finish tooling waiting answer
    offered<-list tooling live
    requested<-eitherDecodeStrict' <$> BS.readFile (root </> "actions-request.json") >>= either error pure
    check "code actions include current intersecting diagnostics"
      (maybe False ((==1).length) (field "context" requested >>= field "diagnostics" :: Maybe [Value]))
    check "code action range uses UTF16"
      ((field "range" requested >>= field "start")==Just (object ["line" .= (0::Int),"character" .= (3::Int)]) &&
       (field "range" requested >>= field "end")==Just (object ["line" .= (0::Int),"character" .= (6::Int)]))
    forM_ ["Command only","Edit and command","Disabled"] $ \title->do
      check "unsupported and server-disabled actions remain visible with reasons" (maybe False (const True) (field "disabledReason" (action title offered)::Maybe T.Text))
      (unchanged,result)<-apply tooling live (ident title offered)
      check "disabled action cannot partially apply edits" (isLeft result && buffers unchanged==buffers live)
    (fixed,done)<-apply tooling live (ident "Fix source" offered)
    check "literal action edits both open and closed buffers" (not (isLeft done) && activeText fixed=="fixed = 2\n" && any ((=="😀 fixed = 1\n").contents.documentBuffer) (M.elems (buffers fixed)))
    (_,replayed)<-apply tooling live (ident "Fix source" offered)
    check "action handles are single use" (isLeft replayed)
    check "code actions never save files" . (=="😀 foo = 1\n") =<< readUtf8 source
    check "closed-file action changes also remain unsaved" . (=="foo = 2\n") =<< readUtf8 other
    stale<-list tooling live
    (untouched,badRevision)<-apply tooling (insertText "x" live) (ident "Fix source" stale)
    check "action rejects a changed source even when caller supplies its new revision" (isLeft badRevision && activeText untouched==activeText (insertText "x" live))
    diskActions<-list tooling live
    writeUtf8 other "changed on disk\n"
    (unchangedDisk,badDisk)<-apply tooling live (ident "Fix source" diskActions)
    check "changed closed file rejects the entire action" (isLeft badDisk && buffers unchangedDisk==buffers live)
    writeUtf8 other "foo = 2\n"
    let openedOther=addDocument (Just (FileState other Nothing)) (newBuffer "foo = 2\n") live
    openActions<-list tooling openedOther
    let editedOther=insertText "changed " openedOther
    (unchangedOther,badOther)<-apply tooling editedOther (ident "Fix source" openActions)
    check "changed other buffer rejects the entire action" (isLeft badOther && buffers unchangedOther==buffers editedOther)
    resolvedActions<-list tooling live
    (resolved,resolvedReply)<-apply tooling live (ident "Resolve" resolvedActions)
    check "advertised resolver produces checked text edits" (not (isLeft resolvedReply) && any ((=="😀 fixed = 1\n").contents.documentBuffer) (M.elems (buffers resolved)))
    commandActions<-list tooling live
    (noCommand,commandReply)<-apply tooling live (ident "Resolve command" commandActions)
    check "resolver adding a command applies none of its edits" (isLeft commandReply && buffers noCommand==buffers live)
    let nested=root </> "nested"
        nestedSource=nested </> "Other.hs"
    createDirectory nested
    writeUtf8 nestedSource "other = 3\n"
    -- A parent marker makes the nested source share its root until a nearer
    -- marker appears. Prime ownership first, then require fresh snapshots.
    writeUtf8 (root </> "hie.yaml") "cradle: {direct: {arguments: []}}\n"
    let nestedOpen=addDocument (Just (FileState nestedSource Nothing)) (newBuffer "other = 3\n") live
    _<-tickTooling tooling core nestedOpen
    forM_ ["hie.yaml","cabal.project","stack.yaml","nested.cabal",".git"] $ \markerName->do
      writeUtf8 (nested </> markerName) "boundary\n"
      forM_ [live,nestedOpen] $ \view->do
        nestedActions<-list tooling view
        (unchanged,nestedReply)<-apply tooling view (ident "Nested project" nestedActions)
        check "independent nested project is excluded whether its file is open or closed"
          (isLeft nestedReply && buffers unchanged==buffers view)
      removeFile (nested </> markerName)
    createDirectory (nested </> ".git")
    nestedDirectoryActions<-list tooling live
    (_,nestedDirectoryReply)<-apply tooling live (ident "Nested project" nestedDirectoryActions)
    check "nested Git directory is a project boundary" (isLeft nestedDirectoryReply)
    removeDirectory (nested </> ".git")
    let protected=live {guestPrivatePaths=[private]}
    privateActions<-list tooling protected
    (privateUnchanged,privateReply)<-apply tooling protected (ident "Private file" privateActions)
    check "actions cannot change a protected closed file" (isLeft privateReply && buffers privateUnchanged==buffers protected)
    check "protected file stays unchanged" . (=="secret = 3\n") =<< readUtf8 private
    cancellationActions<-list tooling live
    (pending,cancelled)<-toolingTool tooling core live "lsp_apply_code_action" (applyArgs live (ident "Hold resolve" cancellationActions))
    ready<-awaitFile tooling pending (root </> "resolve.requested")
    _<-timeout 1000 cancelled
    writeUtf8 (root </> "resolve.release") ""
    _<-awaitFile tooling ready (root </> "resolve.replied")
    (barrier,barrierReply)<-toolingTool tooling core ready "lsp_hover" (args ready)
    (afterCancel,_)<-finish tooling barrier barrierReply
    check "cancelled resolve never applies a late edit" (buffers afterCancel==buffers live)
    expired<-list tooling live
    writeUtf8 (root </> "many-actions") ""
    many<-list tooling live
    check "action cache and response are bounded" ((length <$> (field "actions" many::Maybe [Value]))==Just 128 && field "truncated" many==Just True)
    (_,expiredReply)<-apply tooling live (ident "Fix source" expired)
    check "new list expires previous handles" (isLeft expiredReply)
    removeFile (root </> "many-actions")
    (_,uiStart)<-toolingEffects tooling core live [LanguageRequest RequestCodeActions]
    let awaitDialog d=do
          next<-tickTooling tooling core d
          case dialog next of Just dg | CodeActionChoices{}<-purpose dg ->pure next; _->threadDelay 1000 >> awaitDialog next
    ui<-timeout 3000000 (awaitDialog uiStart) >>= maybe (error "No code-action chooser") pure
    dg<-maybe (error "Missing code-action chooser") pure (dialog ui)
    let (chosen,effects)=submitDialog 0 dg ui
    (_,uiPending)<-toolingEffects tooling core chosen effects
    let awaitApplied current=do
          next<-tickTooling tooling core current
          if rev next /= rev live then pure next else threadDelay 1000 >> awaitApplied next
    uiApplied<-timeout 3000000 (awaitApplied uiPending) >>= maybe (error "Human code action never adopted its prepared edit") pure
    check "human chooser applies the same checked action" (buffers uiApplied/=buffers live && dialog uiApplied==Nothing)
    forM_ ["resolve.requested","resolve.replied","resolve.release"] (removeFile . (root </>))
    (_,timeoutStart)<-toolingEffects tooling core live [LanguageRequest RequestCodeActions]
    timeoutChoices<-timeout 3000000 (awaitDialog timeoutStart) >>= maybe (error "No timeout chooser") pure
    timeoutDialog<-maybe (error "Missing timeout chooser") pure (dialog timeoutChoices)
    timeoutIndex<-case fields timeoutDialog of
      [ListBox _ labels _]->maybe (error "Missing held action") pure (findIndex (=="Hold resolve") labels)
      _->error "Unexpected action chooser"
    let selected=timeoutDialog {fields=[case fieldValue of ListBox title labels _->ListBox title labels timeoutIndex; otherField->otherField | fieldValue<-fields timeoutDialog]}
        (timeoutChosen,timeoutEffects)=submitDialog 0 selected timeoutChoices
    (_,resolving)<-toolingEffects tooling core timeoutChosen timeoutEffects
    requestedTimeout<-awaitFile tooling resolving (root </> "resolve.requested")
    modifyIORef' advance (+31000000000)
    timedOut<-tickTooling tooling core requestedTimeout
    check "human sees the resolver timeout with unchanged buffers"
      (status timedOut=="HLS request timed out" && buffers timedOut==buffers live)
    writeUtf8 (root </> "resolve.release") ""
    timeoutReplied<-awaitFile tooling timedOut (root </> "resolve.replied")
    (timeoutBarrier,timeoutBarrierReply)<-toolingTool tooling core timeoutReplied "lsp_hover" (args timeoutReplied)
    (afterTimeout,_)<-finish tooling timeoutBarrier timeoutBarrierReply
    check "timed-out human resolve never applies a late edit" (buffers afterTimeout==buffers live)
    writeUtf8 (root </> "no-resolve") ""
  withTooling (launchServer server) $ \tooling -> do
    unsupported<-list tooling live
    check "unadvertised resolution stays disabled" (maybe False (const True) (field "disabledReason" (action "Resolve" unsupported)::Maybe T.Text))
  where
    temporary=do
      base<-getTemporaryDirectory
      (path,h)<-openTempFile base "thc-code-actions"
      hClose h; removeFile path; createDirectory path
      canonicalizePath path

-- An unchanged tick must retain the already parsed/sorted projection, while all
-- source ownership/version changes and newly published batches invalidate it.
diagnosticCacheChecks :: IO ()
diagnosticCacheChecks = bracket temporary removePathForcibly $ \root -> do
  let source=root </> "Main.hs"; other=root </> "Other.hs"; server=root </> "fake-hls"
      base=addDocument (Just (FileState source Nothing)) (newBuffer "foo = 1\n") (initialDesktop (80,25))
      bid=maybe (error "no source") sourceFixtureBuffer (activeWindow base)
      core d _=pure (False,d)
      check name ok=unless ok (error name)
      identity d=makeStableName =<< evaluate (diagnostics d)
      await tooling label predicate d=do
        latest<-newIORef d
        let loop current=do
              next<-tickTooling tooling core current
              writeIORef latest next
              if predicate next then pure next else threadDelay 1000 >> loop next
        result<-timeout 3000000 (loop d)
        case result of
          Just next->pure next
          Nothing->do lastView<-readIORef latest; error ("Diagnostic update timed out: "++label++"; "++show (status lastView,diagnostics lastView))
      diagnostic description=object ["range" .= object ["start" .= object ["line" .= (0::Int),"character" .= (0::Int)]],"severity" .= (1::Int),"message" .= (description::T.Text)]
      batch path version messages=object ["uri" .= L.fileUri path,"version" .= (version::Maybe Int),"diagnostics" .= map diagnostic messages]
      publish tooling d entries predicate=do
        BS.writeFile (root </> "diagnostics.next") (BL.toStrict (encode entries))
        renameFile (root </> "diagnostics.next") (root </> "diagnostics.json")
        (_,pending)<-toolingEffects tooling core d {problemsFocused=False} [LanguageRequest TypeInfo]
        await tooling (show entries) predicate pending {problemsFocused=problemsFocused d}
  writeUtf8 source "foo = 1\n"; writeUtf8 other "other = 2\n"
  writeUtf8 (root </> "hie.yaml") "cradle: {direct: {arguments: []}}\n"
  writeUtf8 server mcpServer
  permission<-getPermissions server; setPermissions server permission {executable=True}
  withTooling (launchServer server) $ \tooling -> do
    ready<-await tooling "initial" (not.null.diagnostics) base
    expected<-identity ready
    unchanged<-foldM (\current _->do next<-tickTooling tooling core current; actual<-identity next; check "idle diagnostic ticks reuse the parsed sorted list" (actual==expected); check "idle diagnostic ticks preserve captured action generation" (diagnosticsGeneration next==diagnosticsGeneration ready); pure next) ready [1..40::Int]
    changed<-tickTooling tooling core (insertText "x" unchanged)
    check "editing invalidates versioned diagnostics" (null (diagnostics changed))
    restored<-tickTooling tooling core unchanged
    check "returning to the original source version restores current diagnostics" (length (diagnostics restored)==1)
    restoredIdentity<-identity restored
    let savedView=restored {buffers=M.adjust (\doc->doc {documentBuffer=markSaved (documentBuffer doc)}) bid (buffers restored)}
    savedView'<-tickTooling tooling core savedView
    savedIdentity<-identity savedView'
    check "saving rebases the diagnostics projection key" (savedIdentity/=restoredIdentity && diagnostics savedView'==diagnostics restored)
    let reloaded=savedView' {buffers=M.adjust (\doc->doc {documentBuffer=newBuffer "different = 3\n"}) bid (buffers savedView')}
    reloaded'<-tickTooling tooling core reloaded
    reloadIdentity<-identity reloaded'
    check "equal-revision reload invalidates the cached source identity" (reloadIdentity/=savedIdentity)
    let captured=openContext MessagesContext 5 5 restored {problemsFocused=True}
    updated<-publish tooling captured [batch source (Just 0) ["updated"]] ((==["updated"]).map diagnosticMessage.diagnostics)
    check "real HLS diagnostic replacement invalidates an open Messages action"
      (diagnosticsGeneration updated>diagnosticsGeneration captured && not (contextTargetCurrent updated))
    cleared<-publish tooling updated [batch source (Just 0) []] (null.diagnostics)
    let opened=addDocument (Just (FileState other Nothing)) (newBuffer "other = 2\n") cleared
    opened'<-await tooling "opened" (any ((==other).diagnosticPath).diagnostics) opened
    both<-publish tooling (focusWindow bid opened') [batch source (Just 0) ["main"],batch other (Just 0) ["other"]] ((==2).length.diagnostics)
    let otherId=maybe (error "no second source") windowId (activeWindow opened')
    closed<-tickTooling tooling core (closeActive (focusWindow otherId both))
    check "closing a source removes its versioned diagnostics" (map diagnosticPath (diagnostics closed)==[source])
    let moved=closed {buffers=M.adjust (\doc->doc {documentFile=Just (FileState other Nothing)}) bid (buffers closed)}
    moved'<-tickTooling tooling core moved
    check "Save As remaps diagnostics to the current source path" (map diagnosticPath (diagnostics moved')==[other])
    let build=Diagnostic "build" Nothing 0 0 2 "compiler warning"
    compiled<-tickTooling tooling core moved' {buildDiagnostics=[build]}
    check "new build diagnostics invalidate the combined projection" (build `elem` diagnostics compiled)
    (_,restart)<-toolingEffects tooling core compiled [LanguageRequest RestartLanguage]
    fresh<-tickTooling tooling core restart {buffers=M.empty,windows=[],buildDiagnostics=[]}
    check "language restart clears the diagnostic projection" (null (diagnostics fresh))
  putStrLn "diagnostics cache checks passed"
  where
    temporary=do
      base<-getTemporaryDirectory; (path,h)<-openTempFile base "thc-diagnostics-cache"
      hClose h; removeFile path; createDirectory path; canonicalizePath path

mcpServer :: String
mcpServer = unlines
  [ "#!/usr/bin/env python3"
  , "import sys,json,pathlib,time"
  , "docs={}"
  , "def send(value):"
  , " body=json.dumps(value).encode(); sys.stdout.buffer.write(('Content-Length: %d\\r\\n\\r\\n'%len(body)).encode()+body); sys.stdout.buffer.flush()"
  , "while True:"
  , " line=sys.stdin.buffer.readline()"
  , " if not line: break"
  , " n=int(line.split(b':')[1]); sys.stdin.buffer.readline(); msg=json.loads(sys.stdin.buffer.read(n)); method=msg.get('method'); params=msg.get('params') or {}"
  , " if method=='exit': break"
  , " if method=='textDocument/didSave': pathlib.Path('saved.marker').write_bytes(docs[params['textDocument']['uri']].encode('utf-8'))"
  , " if method=='textDocument/didOpen': docs[params['textDocument']['uri']]=params['textDocument']['text']"
  , " if method=='textDocument/didChange': docs[params['textDocument']['uri']]=params['contentChanges'][0]['text']"
  , " if method=='textDocument/didOpen': send(dict(method='textDocument/publishDiagnostics',params=dict(uri=params['textDocument']['uri'],version=params['textDocument']['version'],diagnostics=[dict(range=dict(start=dict(line=0,character=3),end=dict(line=0,character=6)),message='fix me',severity=1)])))"

  , " if pathlib.Path('diagnostics.json').exists():"
  , "  batches=json.loads(pathlib.Path('diagnostics.json').read_text()); pathlib.Path('diagnostics.json').unlink()"
  , "  for batch in batches: send(dict(method='textDocument/publishDiagnostics',params=batch))"
  , " if 'id' not in msg: continue"
  , " result={}"
  , " if method=='initialize': result=dict(capabilities=dict(codeActionProvider=dict(resolveProvider=not pathlib.Path('no-resolve').exists())))"
  , " if method=='codeAction/resolve':"
  , "  data=params['data']; uri=data['uri']; edit=data['edit']"
  , "  if data['kind']=='hold':"
  , "   pathlib.Path('resolve.requested').touch()"
  , "   while not pathlib.Path('resolve.release').exists(): time.sleep(.001)"
  , "  result=dict(title=params['title'],edit=edit)"
  , "  if data['kind']=='command': result['command']=dict(title='unsafe',command='unsafe')"

  , " if method.startswith('textDocument/'):"
  , "  uri=params['textDocument']['uri']; text=docs[uri]"
  , "  if method=='textDocument/rename':"
  , "   name=params['newName']; pathlib.Path(name+'.requested').touch()"
  , "   if name=='error': send(dict(jsonrpc='2.0',id=msg['id'],error=dict(code=-32603,message='fixture rename error'))); continue"
  , "   if name.startswith('hold_'):"
  , "    while not pathlib.Path(name+'.release').exists(): time.sleep(.001)"
  , "   def edit(start,end): return dict(range=dict(start=dict(line=0,character=start),end=dict(line=0,character=end)),newText=name)"
  , "   result=dict(changes={uri:[edit(3,6)],pathlib.Path('Util.hs').resolve().as_uri():[edit(0,3)]})"
  , "  elif method=='textDocument/codeAction':"
  , "   pathlib.Path('actions-request.json').write_text(json.dumps(params))"
  , "   def change(a,z): return dict(range=dict(start=dict(line=0,character=a),end=dict(line=0,character=z)),newText='fixed')"
  , "   edit=dict(changes={uri:[change(3,6)],pathlib.Path('Util.hs').resolve().as_uri():[change(0,3)]})"
  , "   if pathlib.Path('Extra.hs').exists(): edit['changes'][pathlib.Path('Extra.hs').resolve().as_uri()]=[change(0,5)]"
  , "   command=dict(title='Run command',command='unsafe')"
  , "   result=[dict(title='Fix source',kind='quickfix',isPreferred=True,edit=edit),dict(title='Command only',command='unsafe'),dict(title='Edit and command',edit=edit,command=command),dict(title='Disabled',disabled=dict(reason='Not applicable'),edit=edit),dict(title='Private file',edit=dict(changes={pathlib.Path('Secret.hs').resolve().as_uri():[change(0,6)]})),dict(title='Nested project',edit=dict(changes={pathlib.Path('nested/Other.hs').resolve().as_uri():[change(0,5)]}))]"
  , "   result += [dict(title=title,data=dict(uri=uri,edit=edit,kind=kind)) for title,kind in [('Resolve','normal'),('Resolve command','command'),('Hold resolve','hold')]]"
  , "   if pathlib.Path('many-actions').exists(): result=[dict(title='Action '+str(n),edit=edit) for n in range(140)]"
  , "  else: result=dict(method=method,text=text,position=params.get('position'),contents=text,context=params.get('context'))"
  , " send(dict(jsonrpc='2.0',id=msg['id'],result=result))"
  , " if method=='textDocument/rename': pathlib.Path(params['newName']+'.replied').touch()"
  , " if method=='codeAction/resolve': pathlib.Path('resolve.replied').touch()"
  ]

commandChecks :: IO ()
commandChecks = bracket temporary removePathForcibly $ \root -> do
  let source=root </> "Main.hs"
      secret=root </> "Secret.hs"
      server=root </> "fake-command-hls"
      base=(addDocument (Just (FileState source Nothing)) (newBuffer "foo = 1\n") (initialDesktop (80,25))) {guestPrivatePaths=[secret]}
      bid=maybe (error "missing source") sourceFixtureBuffer (activeWindow base)
      version d=revision (documentBuffer (buffers d M.! bid))
      args d=object ["bufferId" .= bid,"revision" .= version d,"line" .= (1::Int),"column" .= (1::Int)]
      core d _=pure (False,d)
      check label ok=unless ok (error label)
      field key=parseMaybe (withObject "value" (.:key))
      isLeft (Left _)=True; isLeft _=False
      right=either (error . T.unpack) pure
      finish tooling d answer=withAsync answer $ \worker->do
        let pump current=do
              next<-tickTooling tooling core current
              result<-poll worker
              case result of Just _->(next,) <$> wait worker; Nothing->threadDelay 1000 >> pump next
        timeout 5000000 (pump d) >>= maybe (error "Command test timed out") pure
      list tooling d=do
        (next,answer)<-toolingTool tooling core d "lsp_code_actions" (args d)
        (_,result)<-finish tooling next answer
        right result
      action :: T.Text -> Value -> T.Text
      action title value=case [key | item<-fromMaybeList (field "actions" value),field "title" item==Just title,Just key<-[field "actionId" item]] of key:_->key; _->error "Missing command action"
      begin tooling d title listing=toolingTool tooling core d "lsp_apply_code_action" (object ["bufferId" .= bid,"revision" .= version d,"actionId" .= action title listing])
      apply tooling d title=do
        listing<-list tooling d
        (next,answer)<-begin tooling d title listing
        finish tooling next answer
      marker tooling d name=do
        let loop current=do
              next<-tickTooling tooling core current
              exists<-doesFileExist (root </> name)
              if exists then pure next else threadDelay 1000 >> loop next
        timeout 5000000 (loop d) >>= maybe (error ("Missing command marker "++name)) pure
      cleanMarkers=forM_ ["held","release","late-replied","command-finished"] $ \name->do
        exists<-doesFileExist (root </> name)
        when exists (removeFile (root </> name))
  writeUtf8 source "foo = 1\n"
  writeUtf8 secret "secret = 2\n"
  writeUtf8 (root </> "hie.yaml") "cradle: {direct: {arguments: []}}\n"
  createDirectory (root </> "nested")
  writeUtf8 (root </> "nested" </> "hie.yaml") "cradle: {direct: {arguments: []}}\n"
  writeUtf8 (root </> "nested" </> "Other.hs") "other = 3\n"
  writeUtf8 server commandServer
  withTooling (launchServer server) $ \tooling->do
    (normal,reply)<-apply tooling base "normal"
    result<-right reply
    check "offered legacy command applies owned edits" (activeText normal=="bar = 1\n" && field "succeeded" result==Just True && field "appliedBatches" result==Just (1::Int))
    -- A terminal response releases command ownership without losing the
    -- initialized server, diagnostics, and synchronized documents.
    beforeNext<-BS.readFile (root </> "last-executor")
    (nextAction,nextAnswer)<-apply tooling normal "normal"
    _<-right nextAnswer
    afterNext<-BS.readFile (root </> "last-executor")
    check "successful commands reuse the initialized HLS process" (afterNext==beforeNext && activeText nextAction=="bar = 1\n")
    check "command does not save disk" . (=="foo = 1\n") =<< readUtf8 source
    check "command edits retain undo" (activeText (fst (runCommand Undo normal))=="foo = 1\n")
    (twice,twiceReply)<-apply tooling base "twice"
    twiceResult<-right twiceReply
    check ("second owned batch uses updated revision baseline: "++show (activeText twice,twiceResult))
      (activeText twice=="baz = 1\n" && field "appliedBatches" twiceResult==Just (2::Int))
    forM_ [("pipeline","baz = 1\n",True,True,2),("pipeline-drain","baz"<>T.replicate (256*1024) "x"<>" = 1\n",True,True,2),("pipeline-failed","baz = 1\n",False,False,2),("pipeline-invalid","bar = 1\n",False,True,1)] $ \(title,expected,success,commandSucceeded,batches)->do
      (pipelined,pipelineReply)<-apply tooling base title
      pipelineResult<-right pipelineReply
      when (title=="pipeline-drain") $ do
        -- The server has stopped reading while didChange fills its pipe.
        -- Completion must return promptly and retain queued replies meanwhile.
        threadDelay 30000
        writeUtf8 (root </> "pipeline-drain-release") ""
      check "pipelined command response follows all edit adoptions" (activeText pipelined==expected && field "succeeded" pipelineResult==Just success && field "commandSucceeded" pipelineResult==Just commandSucceeded && field "appliedBatches" pipelineResult==Just (batches::Int) && field "partial" pipelineResult==Just (not success))
      do
        _<-marker tooling pipelined (T.unpack title<>"-acks.json")
        observed<-eitherDecodeStrict' <$> BS.readFile (root </> T.unpack title<>"-acks.json") >>= either error pure
        check "server receives pipelined edit acknowledgments" (map (field "applied") (observed::[Value])==[Just True,Just (title/="pipeline-invalid")])
    forM_ ["rejected","private","version","resource","nested"] $ \title->do
      (unchanged,answer)<-apply tooling base title
      refused<-right answer
      check ("command rejects invalid edit: "++T.unpack title) (buffers unchanged==buffers base && field "succeeded" refused==Just False && field "commandSucceeded" refused==Just True && field "applied" refused==Just False)
    forM_ [("failed","bar = 1\n"),("literal-failed","lit = 1\n")] $ \(title,expected)->do
      (partial,answer)<-apply tooling base title
      resultValue<-right answer
      check "failed command preserves and reports preceding edit" (activeText partial==expected && field "partial" resultValue==Just True && field "commandSucceeded" resultValue==Just False && field "appliedBatches" resultValue==Just (1::Int))
      when (title=="literal-failed") $ check "server sees adopted literal edit before command starts" . (=="lit = 1\n") =<< readUtf8 (root </> "execute-seen")
    cleanMarkers
    listing<-list tooling base
    (held,answer)<-begin tooling base "held" listing
    ready<-marker tooling held "held"
    (_,second)<-begin tooling ready "normal" listing
    check "one command per HLS session" . isLeft =<< second
    let changed=insertText "x" ready
    writeUtf8 (root </> "release") ""
    (stale,staleReply)<-finish tooling changed answer
    staleResult<-right staleReply
    check "source changed during command prevents server edits" (buffers stale==buffers changed && field "succeeded" staleResult==Just False)
    cleanMarkers
    cancelListing<-list tooling base
    (cancelStart,cancelReply)<-begin tooling base "held-after" cancelListing
    applied<-marker tooling cancelStart "held"
    check "held command already applied first batch" (activeText applied=="bar = 1\n")
    writeUtf8 (root </> "release") ""
    let awaitFinished=do
          finished<-doesFileExist (root </> "command-finished")
          unless finished (threadDelay 1000 >> awaitFinished)
    timeout 3000000 awaitFinished >>= maybe (error "Command did not finish before cancellation") pure
    -- The response is already in flight, but the desktop has not polled it.
    _<-timeout 1000 cancelReply
    cancelled<-tickTooling tooling core applied
    check "late completion cannot overwrite cancellation status" (not ("HLS command completed" `T.isPrefixOf` status cancelled))
    cancelledResult<-right =<< cancelReply
    check "cancellation reports retained partial edits" (activeText cancelled=="bar = 1\n" && field "partial" cancelledResult==Just True && field "appliedBatches" cancelledResult==Just (1::Int))
    writeUtf8 (root </> "release") ""
    newListing<-list tooling cancelled
    (_,oldReply)<-begin tooling cancelled "normal" cancelListing
    check "a fresh action list invalidates older IDs" . isLeft =<< oldReply
    (restarted,nextReply)<-begin tooling cancelled "normal" newListing
    (fresh,done)<-finish tooling restarted nextReply
    _<-right done
    check "next command runs after cancellation settles" (activeText fresh=="bar = 1\n")
    starts<-lines <$> readUtf8 (root </> "started")
    check "success, failure and settled cancellation retain one HLS process" (length starts==1)
    cleanMarkers
    cancelableListing<-list tooling fresh
    (cancelable,cancelableReply)<-begin tooling fresh "cancelable" cancelableListing
    cancelableReady<-marker tooling cancelable "held"
    _<-timeout 1000 cancelableReply
    cancelableDone<-marker tooling cancelableReady "cancel-acknowledged"
    (afterCancel,afterCancelReply)<-apply tooling cancelableDone "normal"
    _<-right afterCancelReply
    check "cancellation settles without applying subsequent edits" (activeText cancelableDone=="bar = 1\n" && activeText afterCancel=="bar = 1\n")
    cleanMarkers
    hangingListing<-list tooling afterCancel
    (hanging,hangingReply)<-begin tooling afterCancel "hang" hangingListing
    hung<-marker tooling hanging "held"
    _<-timeout 1000 hangingReply
    let awaitRestart current=do
          next<-tickTooling tooling core current
          if "HLS did not finish cancellation" `T.isPrefixOf` status next then pure next
            else threadDelay 1000 >> awaitRestart next
    retired<-timeout 4000000 (awaitRestart hung) >>= maybe (error "Unresponsive cancellation was not retired") pure
    (recovered,recoveredReply)<-apply tooling retired "normal"
    _<-right recoveredReply
    recoveredStarts<-lines <$> readUtf8 (root </> "started")
    check "only unresponsive cancellation restarts HLS" (length recoveredStarts==2 && activeText recovered=="bar = 1\n")
  where
    fromMaybeList Nothing=[]
    fromMaybeList (Just xs)=xs
    temporary=do
      base<-getTemporaryDirectory
      (path,h)<-openTempFile base "thc-command-check"
      hClose h;removeFile path;createDirectory path
      canonicalizePath path

commandServer :: String
commandServer = unlines
  ["#!/usr/bin/env python3"
  ,"import sys,json,pathlib,time"
  ,"root=pathlib.Path.cwd()"
  ,"with open('started','a') as f:f.write('server\\n')"
  ,"generation=len(pathlib.Path('started').read_text().splitlines())"
  ,"docs={}; serial=0; deferred=[]"
  ,"def send(value):"
  ," body=json.dumps(dict(jsonrpc='2.0',**value)).encode();sys.stdout.buffer.write(('Content-Length: %d\\r\\n\\r\\n'%len(body)).encode()+body);sys.stdout.buffer.flush()"
  ,"def recv():"
  ," line=sys.stdin.buffer.readline()"
  ," if not line: raise EOFError()"
  ," n=int(line.split(b':')[1]);sys.stdin.buffer.readline();x=json.loads(sys.stdin.buffer.read(n));p=x.get('params',{});m=x.get('method')"
  ," if m=='textDocument/didOpen': docs[p['textDocument']['uri']]=p['textDocument']['text']"
  ," if m=='textDocument/didChange': docs[p['textDocument']['uri']]=p['contentChanges'][0]['text']"
  ," return x"
  ,"def edit(uri,text='bar',version=None,resource=False,wait=True):"
  ," global serial"
  ," serial+=1;i='edit-'+str(serial)"
  ," edits=[dict(range=dict(start=dict(line=0,character=0),end=dict(line=0,character=3)),newText=text)]"
  ," body=dict(changes={uri:edits}) if version is None else dict(documentChanges=[dict(textDocument=dict(uri=uri,version=version),edits=edits)])"
  ," if resource:body=dict(documentChanges=[dict(kind='delete',uri=uri)])"
  ," send(dict(id=i,method='workspace/applyEdit',params=dict(edit=body)))"
  ," if not wait:return i"
  ," while True:"
  ,"  r=recv()"
  ,"  if r.get('id')==i:"
  ,"   pathlib.Path('last-edit.json').write_text(json.dumps(r['result']));return r['result']"
  ,"  if 'method' in r and 'id' in r: deferred.append(r)"
  ,"def hold():"
  ," pathlib.Path('held').touch()"
  ," while not pathlib.Path('release').exists():time.sleep(.002)"
  ,"while True:"
  ," x=deferred.pop(0) if deferred else recv();m=x.get('method');p=x.get('params',{});i=x.get('id')"
  ," if m=='exit':break"
  ," if m=='initialize':send(dict(id=i,result=dict(capabilities=dict(codeActionProvider=dict(resolveProvider=True),executeCommandProvider=dict(commands=['fixture'])))));continue"
  ," if m=='shutdown':send(dict(id=i,result=None));continue"
  ," if m=='textDocument/codeAction':"
  ,"  uri=p['textDocument']['uri'];actions=[dict(title=mode,command='fixture',arguments=[mode,uri]) for mode in ['normal','hang','cancelable','twice','pipeline','pipeline-drain','pipeline-failed','pipeline-invalid','rejected','failed','held','held-after','private','version','resource','nested']]"
  ,"  literal=dict(changes={uri:[dict(range=dict(start=dict(line=0,character=0),end=dict(line=0,character=3)),newText='lit')]})"
  ,"  actions += [dict(title='literal-failed',edit=literal,command=dict(title='fail',command='fixture',arguments=['fail-only',uri]))]"
  ,"  send(dict(id=i,result=actions));continue"
  ," if m=='workspace/executeCommand':"
  ,"  mode,uri=p['arguments']"
  ,"  pathlib.Path('execute-seen').write_bytes(docs[uri].encode('utf-8'))"
  ,"  pathlib.Path('last-executor').write_text(str(generation))"
  ,"  pathlib.Path('execute.json').write_text(json.dumps(p))"
  ,"  if mode.startswith('pipeline'):"
  ,"   first='bar'+'x'*(256*1024) if mode=='pipeline-drain' else 'bar'"
  ,"   ids=[edit(uri,first,wait=False),edit(uri,'baz',version=99 if mode=='pipeline-invalid' else None,wait=False)]"
  ,"   if mode=='pipeline-failed':send(dict(id=i,error=dict(code=-32603,message='pipeline failure')))"
  ,"   else:send(dict(id=i,result=None))"
  ,"   if mode=='pipeline-drain':"
  ,"    while not pathlib.Path('pipeline-drain-release').exists():time.sleep(.001)"
  ,"   replies=[]"
  ,"   while len(replies)<2:"
  ,"    response=recv()"
  ,"    if response.get('id') in ids:replies.append(response['result'])"
  ,"    elif response.get('method')=='shutdown':send(dict(id=response['id'],result=None));break"
  ,"   if len(replies)==2:"
  ,"    path=pathlib.Path(mode+'-acks.next');path.write_text(json.dumps(replies));path.replace(mode+'-acks.json')"
  ,"   continue"
  ,"  if mode=='cancelable':"
  ,"   pathlib.Path('held').touch()"
  ,"   while True:"
  ,"    cancellation=recv()"
  ,"    if cancellation.get('method')=='$/cancelRequest':break"
  ,"   assert cancellation['params']['id']==i"
  ,"   assert edit(uri,'BAD')['applied']==False"
  ,"   send(dict(id=i,error=dict(code=-32800,message='cancelled')))"
  ,"   pathlib.Path('cancel-acknowledged').touch()"
  ,"   continue"
  ,"  if mode=='hang':"
  ,"   pathlib.Path('held').touch()"
  ,"   while True:time.sleep(.01)"
  ,"  if mode=='held':hold()"
  ,"  if mode!='fail-only':"
  ,"   target=(root.parent/'Outside.hs').as_uri() if mode=='rejected' else (root/'Secret.hs').as_uri() if mode=='private' else (root/'nested'/'Other.hs').as_uri() if mode=='nested' else uri"
  ,"   edit(target,version=99 if mode=='version' else None,resource=mode=='resource')"
  ,"  if mode=='twice':edit(uri,'baz')"
  ,"  if mode=='held-after':hold()"
  ,"  if mode in ['failed','fail-only']:send(dict(id=i,error=dict(code=-32603,message='fixture command failed')))"
  ,"  else:send(dict(id=i,result=None))"
  ,"  pathlib.Path('command-finished').touch()"
  ,"  continue"
  ," if i is not None:send(dict(id=i,result={}))"]

launchServer :: FilePath -> FilePath -> IO L.Client
launchServer server = L.startClientWith "python3" ["-X", "utf8", server, "--lsp"]

writeUtf8 :: FilePath -> String -> IO ()
writeUtf8 path = BS.writeFile path . TE.encodeUtf8 . T.pack

readUtf8 :: FilePath -> IO String
readUtf8 path = T.unpack . TE.decodeUtf8 <$> BS.readFile path
