{-# LANGUAGE OverloadedStrings #-}
module RunCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless, when, forM_, foldM)
import Data.Aeson
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
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import System.Info (os)
import qualified Hide.Consoles as C
import Hide.Markdown (renderMarkdownWithShellBlocks)
import qualified Hide.Build as B
import Hide.Buffer
import Hide.Debugger
import Hide.Conversation
import Hide.Sidebar
import Hide.Model
import Hide.Terminal (terminalAvailable)

checks :: IO ()
checks = do
  shellBlockChecks
  compilerMenuChecks
  keyboardChecks
  bracket temporary removePathForcibly $ \root ->
    withEnv "XDG_CONFIG_HOME" (Just (root </> "config")) $
    withEnv "THC_ROOT" Nothing $ do
      let command=root </> "fake compiler command"
          record=root </> "arguments.json"
          compilerRoot=T.pack (root </> "compiler root")
          runtimePath=T.pack (root </> "runtime with spaces")
          target="exe:target with spaces;$(touch should-not-exist)"
          desktop=(initialDesktop (80,25)) {sideTree=Just (emptySidebar root 20 False)}
          send runtime action values d=snd <$> conversationEffects runtime (\state _ -> pure (False,state)) d [AgentAction action values]
          awaitRun runtime d=do
            result<-timeout 5000000 (loop d)
            maybe (error "Run fixture timed out") pure result
            where loop state=do
                    updated<-tickConversation runtime state
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
                    updated<-tickConversation runtime state
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
      withConversation $ \runtime -> do
        loading<-tickConversation runtime desktop {toolchain=Just GHC}
        check "pending settings refresh preserves the selected toolchain" (toolchain loading==Just GHC)
      withConversation $ \runtime -> do
        initial<-tickConversation runtime desktop
        check "status starts with persisted toolchain" (toolchain initial==Just THC)
        ghc<-send runtime "toolchain" ["GHC"] initial
        stored<-B.loadBuildConfig (root </> "config/thc-edit") root
        check "status selector saves GHC and compiler together" (toolchain ghc==Just GHC && B.buildToolchain stored==GHC && B.buildExecutable stored=="ghc")
        withConversation $ \other -> do
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
          [AgentAction "run-config" values] -> last values=="1" && values !! 5=="[]"
          _ -> False)
        _<-conversationEffects runtime (\state _ -> pure (False,state)) configured submitted
        withDebugger $ \debugger -> do
          (_,refusedDebug)<-debuggerEffects debugger (\state _ -> pure (False,state)) configured
            [DebugAction "launch-config" ["0","","4711"]]
          check "GHC debug refuses silently replacing custom compiler"
            ("custom compiler requires an explicit Adapter config" `T.isInfixOf` status refusedDebug)
        _<-send runtime "toolchain" ["THC"] configured
        let dirtyDesktop=insertText "unsaved source" (addDocument Nothing (newBuffer "") configured)
        refused<-send runtime "run" [] dirtyDesktop
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
      withConversation $ \runtime -> do
        restored<-send runtime "run-options" [] desktop
        check "Run configuration survives runtime restart" (case dialog restored of
          Just dg -> case fields dg of Input _ savedCommand _: _ -> savedCommand==T.pack command; _ -> False
          Nothing -> False)

keyboardChecks :: IO ()
keyboardChecks = do
  let source=addDocument Nothing (newBuffer "source text") (initialDesktop (80,25))
      sourceWindow=maybe (error "source window missing") id (activeWindow source)
      terminal=addReadOnly "Terminal fixture" "terminal text" source
      before=activeText terminal
      expected text=[AgentAction "terminal-input" ["fixture",text]]
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
  check "Ctrl-F9 remains editor Run shortcut" (snd (handleEvent (V.EvKey (V.KFun 9) [V.MCtrl]) terminal)==[AgentAction "run" []])

field :: FromJSON a => T.Text -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.: K.fromText key))
check :: String -> Bool -> IO ()
check label ok=unless ok (error label)
withEnv :: String -> Maybe String -> IO a -> IO a
withEnv name value action=bracket (lookupEnv name <* set value) set (const action)
  where set=maybe (unsetEnv name) (setEnv name)
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
        snd <$> conversationEffects runtime core shown effects
      await runtime label predicate d=timeout 5000000 (loop d) >>= maybe (error label) pure
        where loop current=do
                next<-tickConversation runtime current
                if predicate next then pure next else threadDelay 1000 >> loop next
      waitFile file=timeout 5000000 loop >>= check "compiler menu fixture starts" . (==Just ())
        where loop=do exists<-doesFileExist file; if exists then pure () else threadDelay 1000 >> loop
      entries=contextItems . contextKind
      choose runtime label d=case [command | (name,command)<-entries d,label `T.isInfixOf` name] of
        command:_ -> let (next,effects)=runCommand command d in snd <$> conversationEffects runtime core next effects
        [] -> error "missing compiler choice"
      saved=object ["toolchain" .= ("GHC"::T.Text),"command" .= ("ghc"::T.Text),"cwd" .= root,
        "target" .= ("exe:kept"::T.Text),"arguments" .= ["literal argument"::T.Text],"custom" .= True,
        "toolchains" .= object ["THC" .= object ["toolchain" .= ("THC"::T.Text),"command" .= ("/saved/thc"::T.Text),"custom" .= ("retain"::T.Text)]]]
  check "opening toolchain menu starts asynchronous catalogue discovery"
    (snd (runCommand ToolchainOptions desktop)==[AgentAction "toolchain" []])
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
      withConversation $ \runtime -> do
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
        (_,edited)<-conversationEffects runtime core automatic [AgentAction "run-config" ["0","ghc","exe:edited","","","[]","1"]]
        afterEdit<-decodeStrict' <$> BS.readFile path
        check "editing target retains unknown saved fields" ((afterEdit >>= field "custom")==Just True)
        _<-choose runtime "THC" =<< open runtime edited
        other<-decodeStrict' <$> BS.readFile path
        check "compiler menu preserves the other backend" ((other >>= field "command")==Just ("/saved/thc"::T.Text) && (other >>= field "custom")==Just ("retain"::T.Text))
      removeFile started
      removeFile release
      removeFile done
      closed<-timeout 3000000 $ withConversation $ \runtime -> do
        opened<-open runtime desktop
        waitFile started
        let dismissed=fst (handleEvent (V.EvKey V.KEsc []) opened)
        writeFile release "release"
        waitFile done
        -- The completed worker may be collected on either side of this tick;
        -- neither that result nor later ticks can restore the dismissed popup.
        final<-foldM (\d _ -> threadDelay 1000 >> tickConversation runtime d) dismissed [1..100::Int]
        check "discovery does not reopen a dismissed popup" (contextMenu final==Nothing)
      check "closed-menu worker cleanup is bounded" (closed==Just ())
      removeFile started
      removeFile release
      cleanup<-timeout 3000000 $ withConversation $ \runtime -> do
        _<-open runtime desktop
        waitFile started
      check "conversation shutdown cancels pending discovery" (cleanup==Just ())

shellBlockChecks :: IO ()
shellBlockChecks = when (terminalAvailable && os/="mingw32") $ bracket temporary removePathForcibly $ \root ->
  withEnv "XDG_CONFIG_HOME" (Just (root </> "config")) $ withConversationAt root $ \runtime -> do
    expectedRoot<-canonicalizePath root
    let body="printf '%s\\n' 'literal ; $(touch should-not-exist) λ'\npwd -P\nprintf 'ready\\n'\nIFS= read -r value\nprintf 'echo:%s\\n' \"$value\"\nprintf 'stderr-visible\\n' >&2\n"
        (styled,blocks)=renderMarkdownWithShellBlocks 30 ("intro\n\n```sh\n"<>body<>"```\n\nafter")
        base=(initialDesktop (80,25)) {sideTree=Just (emptySidebar root 20 False)}
        bid=nextId base
        help=addHelpStyled styled base
        desktop=help {buffers=M.adjust (\doc->doc {documentShellBlocks=blocks}) bid (buffers help)}
        chosen=case blocks of block:_->block; _->error "missing shell block"
        core d _=pure (False,d)
        execute d block=let (updated,effects)=runCommand (ExecuteShellBlock bid block) d
                       in snd <$> conversationEffects runtime core updated effects
        (_,consoles,_)=conversationServices runtime
        await label predicate d=do
          result<-timeout 5000000 (loop d)
          maybe (error ("shell block timed out: "++label)) pure result
          where loop current=do
                  updated<-tickConversation runtime current
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
    check "explicit Markdown execution opens and focuses terminal" (fmap bufferId (activeWindow opened)==Just terminalBid)
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
