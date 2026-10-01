{-# LANGUAGE OverloadedStrings #-}
module ToolingCheck (checks) where
import Control.Monad (unless, forM_, replicateM)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, poll, wait)
import Data.Aeson.Types (parseMaybe)
import Control.Exception (bracket)
import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (openTempFile, hClose)
import System.Timeout (timeout)
import THC.Edit.Buffer
import THC.Edit.Files (FileState(..))
import THC.Edit.Model
import THC.Edit.Tooling

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
    writeFile server "#!/usr/bin/env python3\nimport time\ntime.sleep(30)\n"
    permissions<-getPermissions server
    setPermissions server permissions {executable=True}
    writeFile source (T.unpack text)
    writeFile (root </> "Bad.hs") "\NUL"
    bracket (lookupEnv "THC_EDIT_HLS") (maybe (unsetEnv "THC_EDIT_HLS") (setEnv "THC_EDIT_HLS")) $ \_ -> do
      setEnv "THC_EDIT_HLS" server
      withTooling $ \tooling -> do
        let binary=desktop {buffers=M.map (\doc -> restyle doc {documentBuffer=newByteBuffer "a\0b"}) (buffers desktop)}
        (_,blocked)<-toolingEffects tooling core binary [LanguageRequest TypeInfo]
        check "hex buffers do not reach HLS text requests" (buffers blocked==buffers binary && status blocked=="Open a saved Haskell source file first.")
        began<-timeout 1000000 (toolingEffects tooling core desktop [LanguageRequest (RenameAt "bar")])
        (_,preparing)<-maybe (error "rename preparation blocked dispatch") pure began
        check "rename preparation returns before disk result" (status preparing=="Preparing rename...")
        let await d = do
              next<-tickTooling tooling core d
              if "Cannot prepare rename:" `T.isPrefixOf` status next then pure next else threadDelay 10000 >> await next
        failed<-timeout 2000000 (await preparing)
        check "snapshot errors surface and preserve buffers" (maybe False (\d -> buffers d==buffers desktop) failed)
        (_,again)<-toolingEffects tooling core desktop [LanguageRequest (RenameAt "bar")]
        let changed=insertText "x" again
        stale<-tickTooling tooling core changed
        check "editing while preparing cancels rename" (status stale=="Rename target changed; request it again." && buffers stale==buffers changed)
        (_,beforeSaveAs)<-toolingEffects tooling core desktop [LanguageRequest (RenameAt "bar")]
        let moved=beforeSaveAs {buffers=M.map (\doc -> doc {documentFile=Just (FileState (root </> "Elsewhere.hs") Nothing)}) (buffers beforeSaveAs)}
        renamedPath<-tickTooling tooling core moved
        check "Save As while preparing cancels original target" (status renamedPath=="Rename target changed; request it again." && buffers renamedPath==buffers moved)
  mcpChecks
  putStrLn "tooling checks passed"
  where
    temporary = do
      base<-getTemporaryDirectory
      (path,h)<-openTempFile base "thc-tooling-check"
      hClose h
      removeFile path
      createDirectory path
      canonicalizePath path

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
      bid=maybe (error "no source window") bufferId (activeWindow live)
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
  writeFile source (T.unpack disk)
  writeFile other "foo = 2\n"
  writeFile server mcpServer
  permissions<-getPermissions server
  setPermissions server permissions {executable=True}
  bracket (lookupEnv "THC_EDIT_HLS") (maybe (unsetEnv "THC_EDIT_HLS") (setEnv "THC_EDIT_HLS")) $ \_ -> do
    setEnv "THC_EDIT_HLS" server
    withTooling $ \tooling -> do
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
      onDisk<-readFile source; otherDisk<-readFile other
      check "rename never saves files" (onDisk==T.unpack disk && otherDisk=="foo = 2\n")
      (waitingOther,otherReply)<-held tooling renamed "hold_other"
      let editedOther=waitingOther {buffers=M.map (\doc -> if fmap filePath (documentFile doc)==Just other then doc {documentBuffer=replaceSelection (Selection 0 0) "x" (documentBuffer doc)} else doc) (buffers waitingOther)}
      writeFile (root </> "hold_other.release") ""
      (afterOther,staleOther)<-finish tooling editedOther otherReply
      check "rename rejects changes in another edited buffer atomically" (isLeft staleOther && buffers afterOther==buffers editedOther && dialog afterOther==Nothing)
      (waiting,late)<-held tooling live "hold_stale"
      let edited=insertText "x" waiting
      writeFile (root </> "hold_stale.release") ""
      (after,stale)<-finish tooling edited late
      check "late rename cannot overwrite a changed target" (isLeft stale && buffers after==buffers edited && dialog after==Nothing)
      (waitingDisk,diskReply)<-held tooling live "hold_disk"
      writeFile other "changed on disk\n"
      writeFile (root </> "hold_disk.release") ""
      (afterDisk,staleDisk)<-finish tooling waitingDisk diskReply
      check "rename rejects intervening closed-file changes atomically" (isLeft staleDisk && buffers afterDisk==buffers live && dialog afterDisk==Nothing)
      writeFile other "foo = 2\n"
      (waitingCancel,cancelled)<-held tooling live "hold_cancel"
      _<-timeout 1000 cancelled
      writeFile (root </> "hold_cancel.release") ""
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
  , " if method=='textDocument/didOpen': docs[params['textDocument']['uri']]=params['textDocument']['text']"
  , " if method=='textDocument/didChange': docs[params['textDocument']['uri']]=params['contentChanges'][0]['text']"
  , " if 'id' not in msg: continue"
  , " result={}"
  , " if method.startswith('textDocument/'):"
  , "  uri=params['textDocument']['uri']; text=docs[uri]"
  , "  if method=='textDocument/rename':"
  , "   name=params['newName']; pathlib.Path(name+'.requested').touch()"
  , "   if name=='error': send(dict(jsonrpc='2.0',id=msg['id'],error=dict(code=-32603,message='fixture rename error'))); continue"
  , "   if name.startswith('hold_'):"
  , "    while not pathlib.Path(name+'.release').exists(): time.sleep(.001)"
  , "   def edit(start,end): return dict(range=dict(start=dict(line=0,character=start),end=dict(line=0,character=end)),newText=name)"
  , "   result=dict(changes={uri:[edit(3,6)],pathlib.Path('Util.hs').resolve().as_uri():[edit(0,3)]})"
  , "  else: result=dict(method=method,text=text,position=params.get('position'),contents=text,context=params.get('context'))"
  , " send(dict(jsonrpc='2.0',id=msg['id'],result=result))"
  , " if method=='textDocument/rename': pathlib.Path(params['newName']+'.replied').touch()"
  ]
