{-# LANGUAGE CPP, OverloadedStrings #-}
module RunCheck (checks) where

import SourceWindowFixture (sourceFixtureBuffer)
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (unless, when, forM_, foldM)
import Data.Aeson
import Data.IORef
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile, withBinaryFile, IOMode(..), Handle, hFlush)
#ifndef mingw32_HOST_OS
import System.Posix.IO (openFd, closeFd, OpenMode(ReadWrite), OpenFileFlags(nonBlock), defaultFileFlags)
#endif
import System.Process (callProcess)
import System.Timeout (timeout)
import System.Info (os)
import qualified Hide.Consoles as C
import Hide.Markdown (renderMarkdownWithShellBlocks)
import qualified Hide.Build as B
import Hide.Buffer
import Hide.Debugger
import Hide.Conversation (withConversationAt, conversationAgents)
import Hide.SessionServices
import Hide.RuntimeMCP (runtimeTool)
import Hide.GitOperations
import Hide.GuestAccess (guestEffectsAllowed, validateGuestEffects)
import Hide.MCPPermissions (withPermissionsAt, permissionBuildInputAs, policyEffects, tickPermissions, awaitPermissionWork)
import Hide.ControlMCP (controlTool, controlTools)
import Hide.AgentAccess (grantAgentAccess, revokeAgentAccess, resolveActiveAgentAccess)
import qualified Hide.AgentRuntime as AR
import MCPPermissionsCheck (settledTool, settleDialog)
import qualified Hide.BuildJobs as Jobs
import Hide.Files (FileState(..))
import Hide.Sidebar
import Hide.App (applyEffects)
import Hide.Model
import Hide.Terminal (terminalAvailable)

checks :: IO ()
checks = do
  shellBlockChecks
  compilerMenuChecks
  keyboardChecks
  buildPreparationChecks
  admittedBuildChecks
  bracket temporary removePathForcibly $ \root ->
    withEnv "XDG_CONFIG_HOME" (Just (root </> "config")) $
    withEnv "THC_ROOT" Nothing $ do
      let command=root </> "fake compiler command"
          record=root </> "arguments.json"
          compilerRoot=T.pack (root </> "compiler root")
          runtimePath=T.pack (root </> "runtime with spaces")
          target="exe:target with spaces;$(touch should-not-exist)"
          desktop=(initialDesktop (80,25)) {sideTree=Just (emptySidebar root 20 False)}
          core runtime=sessionEffects runtime (\state _ -> pure (False,state))
          tick runtime d=tickBuildPreparation runtime (core runtime) d >>= tickSessionServices runtime
          awaitDialog runtime d=timeout 5000000 (loop d) >>= maybe (error "Build target preparation timed out") pure
            where loop current=do
                    next<-tick runtime current
                    if dialog next/=Nothing then pure next else threadDelay 1000 >> loop next
          send runtime action values d=do
            next<-snd <$> core runtime d [ServiceAction action values]
            if action=="run-options" then awaitDialog runtime next else pure next
          awaitRun runtime d=do
            result<-timeout 5000000 (loop d)
            maybe (error "Run fixture timed out") pure result
            where loop state=do
                    updated<-tick runtime state
                    exists<-doesFileExist record
                    value<-if exists then decodeStrict' <$> BS.readFile record else pure Nothing
                    let output=any (T.isInfixOf "fixture run" . contents . documentBuffer) (M.elems (buffers updated))
                    case value of
                      Just arguments | output -> pure (updated,arguments)
                      _ -> threadDelay 10000 >> loop updated
          awaitToolchain runtime selected d=do
            result<-timeout 5000000 (loop d)
            maybe (error "Toolchain refresh timed out") pure result
            where loop state=do
                    updated<-tick runtime state
                    if toolchain updated==Just selected then pure updated else threadDelay 10000 >> loop updated
          save runtime values d=send runtime "run-config" ("0":T.pack command:values) d
      writeFile command $ unlines
        [ "#!/usr/bin/env python3"
        , "import json,os,sys"
        , "print('fixture run',flush=True)"
        , "with open('arguments.json','w') as f: json.dump({'cwd':os.getcwd(),'args':sys.argv[1:]},f)"
        ]
      permissions<-getPermissions command
      setPermissions command permissions {executable=True}
      withSessionServices $ \runtime -> do
        loading<-tickSessionServices runtime desktop {toolchain=Just GHC}
        check "pending settings refresh preserves the selected toolchain" (toolchain loading==Just GHC)
      withSessionServices $ \runtime -> do
        initial<-tickSessionServices runtime desktop
        check "status starts with persisted toolchain" (toolchain initial==Just THC)
        ghc<-send runtime "toolchain" ["GHC"] initial
        stored<-B.loadBuildConfig (root </> "config/thc-edit") root
        check "status selector saves GHC and compiler together" (toolchain ghc==Just GHC && B.buildToolchain stored==GHC && B.buildExecutable stored=="ghc")
        withSessionServices $ \other -> do
          stale<-awaitToolchain other GHC desktop
          _<-send runtime "toolchain" ["THC"] ghc
          refreshed<-awaitToolchain other THC stale
          check "another session refreshes global toolchain choice" (toolchain refreshed==Just THC)
          _<-send runtime "toolchain" ["GHC"] refreshed
          pure ()
        reloaded<-awaitToolchain runtime GHC desktop
        check "status restores saved GHC choice" (toolchain reloaded==Just GHC)
        _<-send runtime "toolchain" ["THC"] ghc
        options<-send runtime "run-options" [] desktop
        check "Run options default to thc with optional empty settings" (case dialog options of
          Just dg -> [value | Input _ value _<-fields dg]==["thc","","","","[]"]
          Nothing -> False)
        configured<-save runtime [target,compilerRoot,runtimePath] options {dialog=Nothing}
        check "Run configuration saved under isolated XDG" =<< doesFileExist (root </> "config" </> "thc-edit" </> "run.json")
        switched<-send runtime "toolchain" ["GHC"] configured
        restored<-send runtime "toolchain" ["THC"] switched
        let elsewhere=restored {sideTree=Just (emptySidebar (root </> "another-project") 20 False)}
        _<-send runtime "toolchain" ["THC"] elsewhere
        preserved<-B.loadBuildConfig (root </> "config/thc-edit") root
        check "switching preserves custom compiler and root-scoped target"
          (B.buildExecutable preserved==command && B.buildTarget preserved==target && B.buildTHCRoot preserved==compilerRoot && B.buildRuntime preserved==runtimePath)
        reopened<-send runtime "run-options" [] configured
        check "Run options preserve literal configured arguments" (case dialog reopened of
          Just dg -> [value | Input _ value _<-fields dg]==[T.pack command,target,compilerRoot,runtimePath,"[]"]
          Nothing -> False)
        let ghcDialog=case dialog reopened of
              Just dg -> dg {fields=[case item of ListBox name choices _ -> ListBox name choices 1; _ -> item | item<-fields dg]}
              Nothing -> error "missing target dialog"
            (_,submitted)=submitDialog 0 ghcDialog reopened
        check "dialog passes toolchain after text inputs" (case submitted of
          [ServiceAction "run-config" values] -> last values=="1" && values !! 5=="[]"
          _ -> False)
        _<-sessionEffects runtime (\state _ -> pure (False,state)) configured submitted
        withDebugger $ \debugger -> do
          (_,refusedDebug)<-debuggerEffects debugger (\state _ -> pure (False,state)) configured
            [DebugAction "launch-config" ["0","","4711"]]
          check "GHC debug refuses silently replacing custom compiler"
            ("custom compiler requires an explicit Adapter config" `T.isInfixOf` status refusedDebug)
        _<-send runtime "toolchain" ["THC"] configured
        let dirtyDesktop=insertText "unsaved source" (addDocument Nothing (newBuffer "") configured)
        refused<-send runtime "run" [] dirtyDesktop >>= awaitDialog runtime
        check "Run rejects dirty source buffers" (maybe False ((=="Save before running").dialogTitle) (dialog refused))
        check "dirty rejection starts no executable" . not =<< doesFileExist record
        when terminalAvailable $ do
          started<-send runtime "run" [] configured
          (shown,arguments)<-awaitRun runtime started
          check "Run uses shared terminal window" (any (maybe False (T.isPrefixOf "Terminal ") . documentLabel) (M.elems (buffers shown)))
          check "Run preserves argv and project cwd without a shell" (field "cwd" arguments==Just root && field "args" arguments==Just
            (["run",target,"--project-dir",T.pack root,"--thc-root",compilerRoot,"--runtime",runtimePath]::[T.Text]))
          check "literal target never executes shell substitution" . not =<< doesFileExist (root </> "should-not-exist")
          removeFile record
          blank<-save runtime ["","",""] shown
          blankStarted<-send runtime "run" [] blank
          (_,blankArguments)<-awaitRun runtime blankStarted
          check "blank target and optional roots omit CLI flags" (field "args" blankArguments==Just (["run","--project-dir",T.pack root]::[T.Text]))
      -- Persisted Run configuration must work in a fresh runtime, independently of ACP.
      withSessionServices $ \runtime -> do
        restored<-send runtime "run-options" [] desktop
        check "Run configuration survives runtime restart" (case dialog restored of
          Just dg -> case fields dg of Input _ savedCommand _: _ -> savedCommand==T.pack command; _ -> False
          Nothing -> False)

-- The existing Run owner must return while filesystem preparation is held,
-- and only its captured immutable intent may cross the execution gate later.
buildPreparationChecks :: IO ()
buildPreparationChecks=when (os/="mingw32") $ bracket temporary removePathForcibly $ \root ->
  withEnv "XDG_CONFIG_HOME" (Just (root </> "config")) $ do
    let directory=root </> "config/thc-edit"
        path=directory </> "run.json"
        command=root </> "compiler"
        marker=root </> "executed"
        file=root </> "Main.hs"
        base=(addDocument (Just (FileState file Nothing)) ((newBuffer "source") {undoStack=error "build preparation retained Undo"}) (initialDesktop (80,25)))
          {defaultDirectory=Just root}
        config=B.BuildConfig THC command "exe:captured" "" "" ["literal argument"]
        bytes=encode (B.buildConfigValue root config)
        raw runtime action d=snd <$> sessionEffects runtime (\state _->pure (False,state)) d [ServiceAction action []]
        core runtime=sessionEffects runtime (\state _->pure (False,state))
        await label runtime predicate d=timeout 5000000 (loop d) >>= maybe (error ("Build preparation: "++label)) pure
          where loop current=do
                  next<-tickBuildPreparation runtime (core runtime) current
                  ready<-predicate next
                  if ready then pure next else threadDelay 1000 >> loop next
        freshOptions runtime d=timeout 5000000 (loop d) >>= maybe (error "fresh options after retired preparation") pure
          where loop current=do
                  next<-tickBuildPreparation runtime (core runtime) current
                  queued<-raw runtime "run-options" next
                  shown<-tickBuildPreparation runtime (core runtime) queued
                  if dialog shown/=Nothing then pure shown else threadDelay 1000 >> loop shown
    createDirectoryIfMissing True directory
    writeFile command ("#!/bin/sh\nprintf 'executed' > '"++marker++"'\nprintf 'prepared run\n'\nIFS= read -r value\n")
    perms<-getPermissions command
    setPermissions command perms {executable=True}
    check "prepared build adoption is host-only" (not (guestEffectsAllowed [AdoptPreparedBuild Nothing]))
    (_,unsupported)<-applyEffects base [AdoptPreparedBuild Nothing]
    check "unowned prepared-build adoption fails closed"
      (M.keys (buffers unsupported)==M.keys (buffers base) && fmap windowId (activeWindow unsupported)==fmap windowId (activeWindow base))

    -- The writer handshake proves that the real read is waiting for EOF. The
    -- compiler cannot run while the UI remains responsive to a source edit.
    callProcess "mkfifo" [path]
    opened<-newEmptyMVar
    release<-newEmptyMVar
    withAsync (withSettingsWriter path $ \handle->do
      BL.hPut handle (bytes<>BL.replicate 1048577 32); hFlush handle; putMVar opened (); takeMVar release) $ \writer ->
      withSessionServices $ \runtime -> do
        fast<-timeout 500000 (raw runtime "make" base)
        pending<-maybe (error "build settings read blocked the owner") pure fast
        reader<-timeout 5000000 (takeMVar opened)
        check "held build worker reaches settings read" (reader==Just ())
        let changed=insertText "new" pending
        _<-tickBuildPreparation runtime (core runtime) changed
        putMVar release ()
        wait writer
        removeFile path
        BL.writeFile path bytes
        shown<-freshOptions runtime changed
        let jobs=sessionBuildJobs runtime
        result<-Jobs.buildJobStatus jobs shown
        check "stale source preparation cannot start or replace output" (field "outputAvailable" result/=Just True)
        check "stale source preparation does not execute" . not =<< doesFileExist marker

    withSessionServices $ \runtime -> do
      queued<-raw runtime "run-options" base
      let modal=prompt "Unrelated" Information [SelectedInput "Name" "draft" (Selection 0 5)] queued
      retained<-foldM (\d _->threadDelay 1000 >> tickBuildPreparation runtime (core runtime) d) modal [1..30::Int]
      check "ready build options preserve a later modal" (dialog retained==dialog modal)
      shown<-await "modal-deferred options" runtime (pure . maybe False ((==ServiceDialog "run-config") . purpose) . dialog) retained {dialog=Nothing}
      check "deferred options use the captured compiler" (case dialog shown of
        Just dg->case fields dg of Input _ selected _:_->selected==T.pack command; _->False
        _->False)

    withSessionServices $ \runtime -> do
      pending<-raw runtime "make" base
      _<-snd <$> core runtime pending [ServiceAction "run-config" ["0",T.pack command,"exe:new","","","[]","0"]]
      _<-freshOptions runtime pending
      check "settings Save retires an earlier captured build" . not =<< doesFileExist marker

    -- Refusing the delayed gate consumes the ready intent. A later permissive
    -- tick cannot resurrect it; Stop also retires a not-yet-adopted intent.
    withSessionServices $ \runtime -> do
      pending<-raw runtime "make" base
      gated<-newIORef False
      let refuse state effects=do
            when (AdoptPreparedBuild Nothing `elem` effects) (writeIORef gated True)
            pure (False,state)
          loop current=do
            next<-tickBuildPreparation runtime refuse current
            seen<-readIORef gated
            if seen then pure next else threadDelay 1000 >> loop next
      refused<-timeout 5000000 (loop pending) >>= maybe (error "build result did not reach the refusing gate") pure
      _<-freshOptions runtime refused
      check "refused ready build is not retried behind the gate" . not =<< doesFileExist marker
      waiting<-raw runtime "make" base
      (stopped,answer)<-runtimeTool runtime waiting "build_stop" (object [])
      accepted<-answer
      check "shared tool Stop accepts pending owner cancellation" (either (const False) (const True) accepted)
      _<-freshOptions runtime stopped
      check "Stop before adoption starts no process" . not =<< doesFileExist marker

    when terminalAvailable $ withSessionServices $ \runtime -> do
      pending<-raw runtime "run" base
      launching<-await "terminal launch reservation" runtime (const (buildTerminalLaunchPending runtime)) pending
      withGitOperations (buildTerminalLaunchPending runtime) $ \git -> do
        (_,denied)<-gitTool git launching "git_pull" (object [])
        check "mutating Git refuses an unadopted terminal launch" . either (const True) (const False) =<< denied
        (_,readable)<-gitTool git launching "git_operation_status" (object [])
        check "Git status remains readable during terminal launch" . either (const False) (const True) =<< readable
      stopped<-raw runtime "build-stop" launching
      _<-await "abandoned terminal cleanup" runtime (const (not <$> buildTerminalLaunchPending runtime)) stopped
      let consoles=sessionConsoles runtime
      check "abandoned prepared terminal cannot become a visible console" . null =<< C.listConsoles consoles

      exists<-doesFileExist marker
      when exists (removeFile marker)
      again<-raw runtime "run" base
      started<-await "second terminal launch reservation" runtime (const (buildTerminalLaunchPending runtime)) again
      let waitMarker=do
            present<-doesFileExist marker
            if present then pure () else threadDelay 1000 >> waitMarker
      processStarted<-timeout 5000000 waitMarker
      check "terminal worker actually starts the approved process" (processStarted==Just ())
      let edited=insertText "new" started
      changed<-snd <$> core runtime edited [ServiceAction "run-config" ["0",T.pack command,"exe:next","","","[]","0"]]
      shown<-await "edited running terminal adoption" runtime (const (not . null <$> C.listConsoles consoles)) changed
      check "post-launch editing and settings Save preserve program adoption"
        (contents (documentBuffer (buffers shown M.! 1))=="newsource")

keyboardChecks :: IO ()
keyboardChecks = do
  let source=addDocument Nothing (newBuffer "source text") (initialDesktop (80,25))
      sourceWindow=maybe (error "source window missing") id (activeWindow source)
      terminal=addReadOnly "Terminal fixture" "terminal text" source
      before=activeText terminal
      expected text=[ServiceAction "terminal-input" ["fixture",text]]
      cases=[(V.EvKey V.KEnter [],"\r"),(V.EvKey (V.KChar 'c') [V.MCtrl],"\ETX")
            ,(V.EvPaste (TE.encodeUtf8 "λ\ntext"),"λ\ntext"),(V.EvKey V.KUp [],"\ESC[A")
            ,(V.EvKey V.KLeft [V.MCtrl],"\ESC[1;5D"),(V.EvKey V.KRight [V.MShift],"\ESC[1;2C")]
  forM_ cases $ \(event,text) -> do
    let (updated,effects)=handleEvent event terminal
    check "terminal key routes to PTY effect" (effects==expected text)
    check "terminal input does not edit display buffer" (activeText updated==before)
  let (menuOpen,menuEffects)=handleEvent (V.EvKey (V.KFun 10) []) terminal
      (cycled,cycleEffects)=handleEvent (V.EvKey (V.KFun 6) []) terminal
      digit=toEnum (fromEnum '0'+windowNumber sourceWindow)
      (numbered,numberEffects)=handleEvent (V.EvKey (V.KChar digit) [V.MAlt]) terminal
  check "F10 still opens editor menus from terminal" (menu menuOpen==Just (0,0) && null menuEffects)
  check "F6 still cycles windows from terminal" (fmap windowId (activeWindow cycled)==Just (windowId sourceWindow) && null cycleEffects)
  check "Alt-number still activates numbered source window" (fmap windowId (activeWindow numbered)==Just (windowId sourceWindow) && null numberEffects)
  check "native menu Paste targets terminal input" (snd (runCommand Paste terminal {clipboard="paste\n"})==expected "paste\n")
  check "Ctrl-F9 remains editor Run shortcut" (snd (handleEvent (V.EvKey (V.KFun 9) [V.MCtrl]) terminal)==[ServiceAction "run" []])

field :: FromJSON a => T.Text -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.: K.fromText key))
check :: String -> Bool -> IO ()
check label ok=unless ok (error label)
withEnv :: String -> Maybe String -> IO a -> IO a
withEnv name value action=bracket (lookupEnv name <* set value) set (const action)
  where set=maybe (unsetEnv name) (setEnv name)
-- Keep FIFO EOF held without a blocking open on the sole capability. Writes
-- larger than its capacity prove the real configuration reader consumed them.
withSettingsWriter :: FilePath -> (Handle -> IO a) -> IO a
#ifdef mingw32_HOST_OS
withSettingsWriter path=withBinaryFile path WriteMode
#else
withSettingsWriter path action=bracket (openFd path ReadWrite defaultFileFlags {nonBlock=True}) closeFd $
  \_->withBinaryFile path WriteMode action
#endif

temporary :: IO FilePath
temporary=do
  root<-getTemporaryDirectory
  (path,handle)<-openTempFile root "thc-run-check"
  hClose handle
  removeFile path
  createDirectory path
  canonicalizePath path

compilerMenuChecks :: IO ()
compilerMenuChecks=bracket temporary removePathForcibly $ \root -> do
  let bin=root </> "bin"
      selected=root </> "compiler with spaces"
      settings=root </> "config/thc-edit"
      path=settings </> "run.json"
      started=root </> "started"
      release=root </> "release"
      done=root </> "done"
      desktop=(initialDesktop (80,25)) {sideTree=Just (emptySidebar root 20 False)}
      core d _=pure (False,d)
      open runtime d=let (shown,effects)=runCommand ToolchainOptions d in
        snd <$> sessionEffects runtime core shown effects
      await runtime label predicate d=timeout 5000000 (loop d) >>= maybe (error label) pure
        where loop current=do
                next<-tickSessionServices runtime current
                if predicate next then pure next else threadDelay 1000 >> loop next
      waitFile file=timeout 5000000 loop >>= check "compiler menu fixture starts" . (==Just ())
        where loop=do exists<-doesFileExist file; if exists then pure () else threadDelay 1000 >> loop
      entries=contextItems . contextKind
      choose runtime label d=case [command | (name,command)<-entries d,label `T.isInfixOf` name] of
        command:_ -> let (next,effects)=runCommand command d in snd <$> sessionEffects runtime core next effects
        [] -> error "missing compiler choice"
      saved=object ["toolchain" .= ("GHC"::T.Text),"command" .= ("ghc"::T.Text),"cwd" .= root,
        "target" .= ("exe:kept"::T.Text),"arguments" .= ["literal argument"::T.Text],"custom" .= True,
        "toolchains" .= object ["THC" .= object ["toolchain" .= ("THC"::T.Text),"command" .= ("/saved/thc"::T.Text),"custom" .= ("retain"::T.Text)]]]
  check "opening toolchain menu starts asynchronous catalogue discovery"
    (snd (runCommand ToolchainOptions desktop)==[ServiceAction "toolchain" []])
  createDirectory bin
  createDirectoryIfMissing True settings
  writeFile selected ""
  BS.writeFile path (BL.toStrict (encode saved))
  python<-findExecutable "python3" >>= maybe (error "python3 required") pure
  let executable=bin </> "ghcup"
  writeFile executable $ unlines
    ["#!"++python,"import os,sys,time", "a=sys.argv[1:]", "assert a[0]=='--offline'",
     "if a[1]=='list':", " open(os.environ['MENU_STARTED'],'w').close()",
     " while not os.path.exists(os.environ['MENU_RELEASE']): time.sleep(.001)",
     " print('ghc 9.8.2 installed')", "else:", " open(os.environ['MENU_DONE'],'w').close()", " print(os.environ['MENU_COMPILER'])"]
  perms<-getPermissions executable
  setPermissions executable perms {executable=True}
  withEnv "XDG_CONFIG_HOME" (Just (root </> "config")) $ withEnv "PATH" (Just bin) $
    withEnv "MENU_STARTED" (Just started) $ withEnv "MENU_RELEASE" (Just release) $
    withEnv "MENU_DONE" (Just done) $ withEnv "MENU_COMPILER" (Just selected) $ do
      withSessionServices $ \runtime -> do
        fast<-timeout 500000 (open runtime desktop)
        shown<-maybe (error "compiler discovery blocked UI") pure fast
        check "Automatic is immediately available while discovering" (any (T.isInfixOf "Automatic" . fst) (entries shown))
        waitFile started
        writeFile release "release"
        populated<-await runtime "installed compiler menu" (any (T.isInfixOf "9.8.2" . fst) . entries) shown
        chosen<-choose runtime "9.8.2" populated
        config<-B.loadBuildConfig settings root
        value<-decodeStrict' <$> BS.readFile path
        check "installed selection keeps target arguments and custom fields" (B.buildExecutable config==selected && B.buildTarget config=="exe:kept" && B.buildArguments config==["literal argument"] && (value >>= field "custom")==Just True)
        reopened<-open runtime chosen
        automatic<-choose runtime "Automatic" reopened
        restored<-B.loadBuildConfig settings root
        check "Automatic restores project compiler selection without losing target" (B.buildExecutable restored=="ghc" && B.buildTarget restored=="exe:kept")
        (_,edited)<-sessionEffects runtime core automatic [ServiceAction "run-config" ["0","ghc","exe:edited","","","[]","1"]]
        afterEdit<-decodeStrict' <$> BS.readFile path
        check "editing target retains unknown saved fields" ((afterEdit >>= field "custom")==Just True)
        _<-choose runtime "THC" =<< open runtime edited
        other<-decodeStrict' <$> BS.readFile path
        check "compiler menu preserves the other backend" ((other >>= field "command")==Just ("/saved/thc"::T.Text) && (other >>= field "custom")==Just ("retain"::T.Text))
      removeFile started
      removeFile release
      removeFile done
      closed<-timeout 3000000 $ withSessionServices $ \runtime -> do
        opened<-open runtime desktop
        waitFile started
        let dismissed=fst (handleEvent (V.EvKey V.KEsc []) opened)
        writeFile release "release"
        waitFile done
        -- The completed worker may be collected on either side of this tick;
        -- neither that result nor later ticks can restore the dismissed popup.
        final<-foldM (\d _ -> threadDelay 1000 >> tickSessionServices runtime d) dismissed [1..100::Int]
        check "discovery does not reopen a dismissed popup" (contextMenu final==Nothing)
      check "closed-menu worker cleanup is bounded" (closed==Just ())
      removeFile started
      removeFile release
      cleanup<-timeout 3000000 $ withSessionServices $ \runtime -> do
        _<-open runtime desktop
        waitFile started
      check "conversation shutdown cancels pending discovery" (cleanup==Just ())

shellBlockChecks :: IO ()
shellBlockChecks = when (terminalAvailable && os/="mingw32") $ bracket temporary removePathForcibly $ \root ->
  withEnv "XDG_CONFIG_HOME" (Just (root </> "config")) $ withSessionServices $ \runtime -> do
    expectedRoot<-canonicalizePath root
    let body="printf '%s\\n' 'literal ; $(touch should-not-exist) λ'\npwd -P\nprintf 'ready\\n'\nIFS= read -r value\nprintf 'echo:%s\\n' \"$value\"\nprintf 'stderr-visible\\n' >&2\n"
        (styled,blocks)=renderMarkdownWithShellBlocks 30 ("intro\n\n```sh\n"<>body<>"```\n\nafter")
        base=(initialDesktop (80,25)) {sideTree=Just (emptySidebar root 20 False)}
        bid=nextId base
        help=addHelpStyled styled base
        desktop=help {buffers=M.adjust (\doc->doc {documentShellBlocks=blocks}) bid (buffers help)}
        chosen=case blocks of block:_->block; _->error "missing shell block"
        core d _=pure (False,d)
        execute d block=let (updated,effects)=runCommand (ExecuteShellBlock (SourceShell bid) block) d
                       in snd <$> sessionEffects runtime core updated effects
        consoles=sessionConsoles runtime
        await label predicate d=do
          result<-timeout 5000000 (loop d)
          maybe (error ("shell block timed out: "++label)) pure result
          where loop current=do
                  updated<-tickSessionServices runtime current
                  entries<-C.listConsoles consoles
                  ready<-predicate entries
                  if ready then pure updated else threadDelay 10000 >> loop updated
        outputHas tid needle = do
          output<-C.consoleOutput consoles tid
          pure (case output of Right (bytes,_,_)->TE.encodeUtf8 needle `BS.isInfixOf` bytes; _->False)
    queued<-execute desktop chosen
    opened<-await "visible interactive terminal" (\entries->case entries of (tid,_,_):_->outputHas tid "ready"; _->pure False) queued
    entries<-C.listConsoles consoles
    let (tid,terminalBid,_)=head entries
    check "explicit Markdown execution opens and focuses terminal" (fmap sourceFixtureBuffer (activeWindow opened)==Just terminalBid)
    check "whole shell body preserves literal arguments" =<< outputHas tid "literal ; $(touch should-not-exist) λ"
    check "shell block runs in selected project cwd" =<< outputHas tid (T.pack expectedRoot)
    check "quoted command substitution remains literal" . not =<< doesFileExist (root </> "should-not-exist")
    _<-C.inputConsole consoles tid (TE.encodeUtf8 "typed λ\n")
    finished<-await "stdin and exit" (\rows->pure (any (\(ident,_,code)->ident==tid && code==Just 0) rows)) opened
    check "shell block terminal accepts stdin and captures stdout" =<< outputHas tid "echo:typed λ"
    check "shell block terminal captures stderr" =<< outputHas tid "stderr-visible"
    let stale=finished {buffers=M.adjust (\doc->doc {documentShellBlocks=[]}) bid (buffers finished)}
    _<-execute stale chosen
    check "stale shell action cannot launch another process" . (==1) . length =<< C.listConsoles consoles
    let empty=(0,1,"sh","")
        emptyDesktop=finished {buffers=M.adjust (\doc->doc {documentShellBlocks=[empty]}) bid (buffers finished)}
    rejected<-execute emptyDesktop empty
    check "empty shell block reports error" (maybe False ((=="Cannot execute shell block").dialogTitle) (dialog rejected))

-- The real input path captures a delayed build before the same permission owner
-- rechecks its caller/policy. Adoption ticks are withheld until the state changes.
admittedBuildChecks :: IO ()
admittedBuildChecks=when (os/="mingw32") $ bracket temporary removePathForcibly $ \root ->
  withEnv "XDG_CONFIG_HOME" (Just (root </> "config")) $
  withEnv "THC_EDIT_SESSION" Nothing $ withEnv "THC_ROOT" Nothing $ do
    let directory=root </> "config/thc-edit"
        configPath=directory </> "run.json"
        policyPath=root </> "policy.toml"
        command=root </> "fixture compiler"
        marker=root </> "launches"
        base=(addDocument (Just (FileState (root </> "Main.hs") Nothing)) (newBuffer "source") (initialDesktop (80,25))) {defaultDirectory=Just root}
        bytes=encode (B.buildConfigValue root (B.BuildConfig THC command "exe:captured" "" "" []))
        input events=object ["events" .= events]
        key=object ["type" .= ("key"::T.Text),"key" .= ("F9"::T.Text)]
        core runtime=sessionEffects runtime (\d _->pure (False,d))
        tick runtime permissions d=tickBuildPreparation runtime (core runtime) d >>= tickPermissions permissions
        policy mode=writeFile policyPath ("[editor.mcp.permissions]\neditor_input = '"++mode++"'\n")
        clearMarker=doesFileExist marker >>= \exists->when exists (removeFile marker)
        freshOptions runtime permissions d=timeout 5000000 (loop d) >>= maybe (error "admitted build did not retire") pure
          where loop current=do
                  next<-tick runtime permissions current
                  queued<-snd <$> core runtime next [ServiceAction "run-options" []]
                  shown<-tick runtime permissions queued
                  if maybe False ((==ServiceDialog "run-config").purpose) (dialog shown)
                    then pure shown else threadDelay 1000 >> loop shown
        noLaunch label=check label . not =<< doesFileExist marker
        noJob runtime d label=do
          let jobs=sessionBuildJobs runtime
          facts<-Jobs.buildJobStatus jobs d
          check label (field "active" facts==Just False && field "outputAvailable" facts/=Just True)
        launchCount=do
          found<-doesFileExist marker
          if found then BS.length <$> BS.readFile marker else pure 0
        awaitLaunch runtime permissions d=timeout 5000000 (loop d) >>= maybe (error "admitted build never launched") pure
          where loop current=do
                  next<-tick runtime permissions current
                  count<-launchCount
                  if count>0 then pure next else threadDelay 1000 >> loop next
        initiate runtime permissions caller apply=permissionBuildInputAs caller permissions
          (\admission d name args->withBuildAdmission runtime admission (controlTool apply d name args))
        guest runtime d fx=validateGuestEffects d fx >> core runtime d fx
        attributed runtime=do
          let agents=conversationAgents runtime
          token<-grantAgentAccess (AR.agentAccess agents) (AR.primaryAgent agents)
          let caller=fmap (() <$) (resolveActiveAgentAccess (AR.agentAccess agents) (AR.agentHub agents) token)
          live<-caller
          check "build fixture has a live Primary credential" (live==Right ())
          pure caller
        admit runtime permissions caller d=do
          (shown,reply)<-settledTool permissions (initiate runtime permissions caller (guest runtime)) d "editor_input" (input [key])
          accepted<-case dialog shown of
            Just dg | PermissionDialog action<-purpose dg,"approve:" `T.isPrefixOf` action->do
              let (next,fx)=submitDialog 0 dg shown
              (_,approved)<-policyEffects permissions (core runtime) next fx
              settleDialog permissions dg approved
            _->pure shown
          result<-reply
          check "real editor_input accepted one build event" (case result of Right value->field "appliedEvents" value==Just (1::Int); _->False)
          pure accepted
        -- Preparation can complete in the background, but only the serialized
        -- owner tick may adopt it. Apply revocation/policy/Stop before that tick.
        beforeAdoption runtime permissions caller after=admit runtime permissions caller base >>= after
        -- Consume the initial wire-policy wake. A subsequent fresh-check wake
        -- then proves a result is ready without invoking the launch callback.
        freshPolicy runtime permissions d=do
          STM.atomically ((awaitPermissionWork permissions >> pure ()) `STM.orElse` pure ())
          timeout 5000000 (loop d) >>= maybe (error "deferred build policy did not complete") pure
          where loop current=do
                  next<-tick runtime permissions current
                  ready<-STM.atomically ((awaitPermissionWork permissions >> pure True) `STM.orElse` pure False)
                  if ready then tickPermissions permissions next else threadDelay 1000 >> loop next
    createDirectoryIfMissing True directory
    BL.writeFile configPath bytes
    writeFile command ("#!/bin/sh\nprintf x >> '"++marker++"'\nprintf 'captured build\n'\n")
    perms<-getPermissions command
    setPermissions command perms {executable=True}

    -- All four refusals begin as actually admitted, attributed input. They must
    -- retire before a fresh human options request can occupy the same slot.
    forM_ ["revoked","disabled","needs-approval","stopped"] $ \reason->do
      policy "enable"
      clearMarker
      withSessionServices $ \runtime->withConversationAt (sessionConsoles runtime) root $ \conversation->withPermissionsAt policyPath controlTools $ \permissions->do
        caller<-attributed conversation
        pending<-beforeAdoption runtime permissions caller $ \d->case reason of
          "revoked"->do
            let agents=conversationAgents conversation
            revokeAgentAccess (AR.agentAccess agents) (AR.primaryAgent agents)
            pure d
          "disabled"->policy "disable" >> pure d
          "needs-approval"->policy "prompt" >> pure d
          _->stopSessionBuild runtime d
        shown<-freshOptions runtime permissions pending
        noJob runtime shown ("admitted build has no job: "++reason)
        noLaunch ("admitted build refusal: "++reason)

    policy "prompt"
    clearMarker
    withSessionServices $ \runtime->withConversationAt (sessionConsoles runtime) root $ \conversation->withPermissionsAt policyPath controlTools $ \permissions->do
      caller<-attributed conversation
      pending<-beforeAdoption runtime permissions caller pure
      launched<-awaitLaunch runtime permissions pending
      check "approved input final policy does not ask again" (dialog launched==Nothing)
      _<-freshOptions runtime permissions launched
      check "approved admitted intent executes once" . (==1) =<< launchCount

    policy "enable"
    clearMarker
    withSessionServices $ \runtime->withConversationAt (sessionConsoles runtime) root $ \conversation->withPermissionsAt policyPath controlTools $ \permissions->do
      caller<-attributed conversation
      pending<-admit runtime permissions caller base
      allowed<-freshPolicy runtime permissions pending
      noLaunch "fresh policy result alone does not launch"
      let modal=prompt "Unrelated" Information [SelectedInput "Name" "draft" (Selection 0 5)] allowed
      heldModal<-tickBuildPreparation runtime (core runtime) modal
      policy "disable"
      _<-tick runtime permissions heldModal
      shown<-freshOptions runtime permissions heldModal {dialog=Nothing}
      noJob runtime shown "modal-deferred refusal has no job"
      noLaunch "modal-deferred build obtains fresh external policy"

    -- The later real input event fails after the first event captured an intent.
    -- Failure of the original callback must retire that intent, not just its RPC.
    policy "enable"
    clearMarker
    withSessionServices $ \runtime->withConversationAt (sessionConsoles runtime) root $ \conversation->withPermissionsAt policyPath controlTools $ \permissions->do
      caller<-attributed conversation
      seen<-newIORef (0::Int)
      let failing d fx=do
            validateGuestEffects d fx
            when (ServiceAction "make" [] `elem` fx) $ do
              n<-atomicModifyIORef' seen (\n->(n+1,n+1))
              when (n==2) (ioError (userError "fixture-only later input failure"))
            core runtime d fx
      (failed,reply)<-settledTool permissions (initiate runtime permissions caller failing) base "editor_input" (input [key,key])
      check "later input exception fails the original call" . either (const True) (const False) =<< reply
      shown<-freshOptions runtime permissions failed
      noJob runtime shown "failed input callback has no job"
      noLaunch "failed input callback cannot leave an executable intent"

    -- Anonymous inspection retains session policy; human input bypasses the
    -- editor_input receipt but continues through the original execution gates.
    policy "enable"
    clearMarker
    withSessionServices $ \runtime->withConversationAt (sessionConsoles runtime) root $ \conversation->withPermissionsAt policyPath controlTools $ \permissions->do
      pending<-admit runtime permissions (pure (Right ())) base
      _<-awaitLaunch runtime permissions pending
      check "anonymous input uses the same admitted lifecycle" . (==1) =<< launchCount
    policy "disable"
    clearMarker
    withSessionServices $ \runtime->withConversationAt (sessionConsoles runtime) root $ \conversation->withPermissionsAt policyPath controlTools $ \permissions->do
      pending<-snd <$> core runtime base [ServiceAction "make" []]
      _<-awaitLaunch runtime permissions pending
      check "human build remains independent of editor_input policy" . (==1) =<< launchCount
