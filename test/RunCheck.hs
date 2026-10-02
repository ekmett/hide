{-# LANGUAGE OverloadedStrings #-}
module RunCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless, when, forM_)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import qualified THC.Edit.Build as B
import THC.Edit.Buffer
import THC.Edit.Debugger
import THC.Edit.Conversation
import THC.Edit.Model
import THC.Edit.Terminal (terminalAvailable)

checks :: IO ()
checks = do
  keyboardChecks
  bracket temporary removePathForcibly $ \root ->
    withEnv "XDG_CONFIG_HOME" (Just (root </> "config")) $
    withEnv "THC_ROOT" Nothing $ do
      let command=root </> "fake compiler command"
          record=root </> "arguments.json"
          compilerRoot=T.pack (root </> "compiler root")
          runtimePath=T.pack (root </> "runtime with spaces")
          target="exe:target with spaces;$(touch should-not-exist)"
          desktop=(initialDesktop (80,25)) {sideTree=Just (Sidebar root [] 0 0 20 False)}
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
        initial<-tickConversation runtime desktop
        check "status starts with persisted toolchain" (toolchain initial==Just THC)
        ghc<-send runtime "toolchain" ["GHC"] initial
        stored<-B.loadBuildConfig (root </> "config/thc-edit") root
        check "status selector saves GHC and compiler together" (toolchain ghc==Just GHC && B.buildToolchain stored==GHC && B.buildExecutable stored=="ghc")
        withConversation $ \other -> do
          stale<-tickConversation other desktop
          _<-send runtime "toolchain" ["THC"] ghc
          refreshed<-tickConversation other stale
          check "another session refreshes global toolchain choice" (toolchain refreshed==Just THC)
          _<-send runtime "toolchain" ["GHC"] refreshed
          pure ()
        reloaded<-tickConversation runtime desktop
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
        let elsewhere=restored {sideTree=Just (Sidebar (root </> "another-project") [] 0 0 20 False)}
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
