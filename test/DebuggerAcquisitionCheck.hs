{-# LANGUAGE OverloadedStrings #-}
module DebuggerAcquisitionCheck (checks) where

import Control.Concurrent
import Control.Exception (bracket,finally,getMaskingState,MaskingState(Unmasked))
import Control.Monad (unless,forM_,void)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding
import qualified Graphics.Vty as V
import qualified Network.Socket as S
import System.Directory
import System.Environment
import System.FilePath
import System.Info (os)
import System.Timeout
import THC.Edit.Buffer
import THC.Edit.Build
import THC.Edit.Debugger
import THC.Edit.Downloads
import THC.Edit.Files
import THC.Edit.GuestAccess
import THC.Edit.HdbAcquisition
import THC.Edit.Model
import THC.Edit.RemoteEndpoint (randomIdentity)

checks :: IO ()
checks | os=="mingw32"=pure () -- Official hdb bindists are currently POSIX only.
      | otherwise=do
  temp<-getTemporaryDirectory
  ident<-randomIdentity
  repository<-getCurrentDirectory
  python<-findExecutable "python3" >>= maybe (error "hdb UI fixture requires Python") pure
  let directory=temp </> "hdb-ui-"++take 12 ident
  bracket (createDirectory directory >> canonicalizePath directory) removePathForcibly $ \root->do
    let configRoot=root </> "config"; config=configRoot </> "thc-edit"
        commands=root </> "commands"; ghc=commands </> "ghc-9.14.1"
        library=root </> "lib"; cache=root </> "cache"; source=root </> "Main.hs"
        logFile=root </> "dap.log"; adapter=cache </> "bin/hdb-9.14.1"
        settings=BuildConfig GHC ghc "" "" "" ["original argument"]
    mapM_ (createDirectoryIfMissing True) [config,commands,library]
    TIO.writeFile source "main = putStrLn \"hello\"\n"
    executableFile ghc ("#!/bin/sh\ncase \"$1\" in\n--numeric-version) echo 9.14.1;;\n--print-libdir) printf '%s\\n' "<>quote library<>";;\nesac\n")
    BL.writeFile (config </> "run.json") (encode (buildConfigValue root settings))
    withEnvironment [("XDG_CONFIG_HOME",configRoot),("THC_CACHE_HOME",cache),("PATH",commands)] $ do
      let asset=either (error.T.unpack) id (selectHdbAsset "aarch64-apple-darwin" "9.14.1")
          plan compiler=HdbPlan compiler library cache asset
          base=(addDocument (Just (FileState source Nothing)) (newBuffer "main = putStrLn \"hello\"\n") (initialDesktop (80,25))) {defaultDirectory=Just root,toolchain=Just GHC}
          core d _=pure (False,d)
          offer d=case dialog d of
            Just dg | DebugDialog name<-purpose dg->"hdb-accept:" `T.isPrefixOf` name
            _->False
          send runtime action args d=snd <$> debuggerEffects runtime core d [DebugAction action args]
          submit runtime button d=case dialog d of
            Nothing->error "missing acquisition dialog"
            Just dg->let (next,effects)=submitDialog button dg d in snd <$> debuggerEffects runtime core next effects
          waitFor runtime label predicate d=timeout 5000000 (loop d) >>= maybe (error ("hdb UI timed out: "++label)) pure
            where loop state=do
                    next<-tickDebugger runtime core state
                    done<-predicate next
                    if done then pure next else threadDelay 1000 >> loop next
          has text d=pure (maybe False (any (\control->case control of ListBox _ labels _->any (T.isInfixOf text) labels; _->False).fields) (dialog d))
          install report gate=do
            _<-report (DownloadProgress "Downloading" 7 (Just 20))
            _<-takeMVar gate
            createDirectoryIfMissing True (takeDirectory adapter)
            executableFile adapter ("#!/bin/sh\nexec "<>quote python<>" "<>quote (repository </> "test/dap-session.py")<>" "<>quote logFile<>" server-basic\n")
            pure (Right adapter)
          port=bracket (S.socket S.AF_INET S.Stream S.defaultProtocol) S.close $ \socket->do
            S.bind socket (S.SockAddrInet 0 (S.tupleToHostAddress (127,0,0,1)))
            S.SockAddrInet number _<-S.getSocketName socket
            pure (T.pack (show (fromIntegral number::Int)))
          start runtime=do p<-port; send runtime "launch-config" ["0","",p] base >>= waitFor runtime "offer" (pure.offer)
          reset=do
            exists<-doesPathExist cache
            if exists then removePathForcibly cache else pure ()
            TIO.writeFile logFile ""
      calls<-newIORef (0::Int)
      withDebuggerHdb (pure 0) (pure . Right . plan) (\_ _->modifyIORef' calls (+1)>>pure (Left "unexpected acquisition")) $ \runtime->do
        opened<-start runtime
        untouched<-readIORef calls
        exists<-doesPathExist cache
        check "offer performs no download or install" (untouched==0 && not exists)
        check "agent cannot read or activate download offer" (guestModalBlocked opened && not (guestKeyAllowed opened V.KEnter []) && not (cellReadable (cellAccess opened 20 10)))
        let (closed,effects)=handleEvent (V.EvKey V.KEsc []) opened
        declined<-snd <$> debuggerEffects runtime core closed effects
        check "escape declines and closes offer" (dialog declined==Nothing)
        _<-case dialog opened of Just dg->submit runtime 0 declined {dialog=Just dg}; _->error "missing offer"
        count<-readIORef calls
        check "old declined offer cannot acquire" (count==0)
      -- Fork ownership is registered while masked, but compiler probes remain
      -- interruptible and runtime shutdown joins their cleanup.
      preparationState<-newEmptyMVar
      preparationStopped<-newEmptyMVar
      preparationGate<-newEmptyMVar
      withDebuggerHdb (pure 0)
        (\compiler->(do state<-getMaskingState
                        putMVar preparationState state
                        takeMVar preparationGate
                        pure (Right (plan compiler))) `finally` putMVar preparationStopped ())
        (\_ _->pure (Left "unexpected acquisition")) $ \runtime->do
          p<-port
          _<-send runtime "launch-config" ["0","",p] base
          state<-timeout 1000000 (takeMVar preparationState) >>= maybe (error "preparation did not start") pure
          check "preparation body restores unmasked execution" (state==Unmasked)
      stopped<-tryTakeMVar preparationStopped
      check "runtime close joins pending preparation cleanup" (stopped==Just ())
      -- Replacing a still-running preparation retires its worker before the new
      -- request starts, without treating that old context as the new request.
      preparations<-newIORef (0::Int)
      entered<-newEmptyMVar
      held<-newEmptyMVar
      let prepare compiler=do
            number<-atomicModifyIORef' preparations (\n->(n+1,n))
            if number==0 then putMVar entered () >> takeMVar held else pure ()
            pure (Right (plan compiler))
      withDebuggerHdb (pure 0) prepare (\_ _->pure (Left "unexpected acquisition")) $ \runtime->do
        p<-port
        pending<-send runtime "launch-config" ["0","",p] base
        timeout 1000000 (readMVar entered) >>= maybe (error "preparation did not start") pure
        let replacement=addDocument (Just (FileState (root </> "Other.hs") Nothing)) (newBuffer "main = pure ()") pending
        TIO.writeFile (root </> "Other.hs") "main = pure ()"
        newer<-send runtime "launch-config" ["0","",p] replacement
        _<-waitFor runtime "replacement preparation" (pure.offer) newer
        count<-readIORef preparations
        check "retiring preparation cannot invalidate newer request" (count==2)
      forM_ ["cancel","toolchain","project","edit","reload","settings","stop","newer","success"] $ \mode->do
        reset
        gate<-newEmptyMVar
        finished<-newEmptyMVar
        writeIORef calls 0
        withDebuggerHdb (pure 0) (pure . Right . plan)
          (\_ report->(modifyIORef' calls (+1)>>install report gate) `finally` void (tryPutMVar finished ())) $ \runtime->do
            opened<-start runtime
            accepted<-submit runtime 0 opened
            progress<-waitFor runtime "progress" (has "Downloading") accepted
            count<-readIORef calls
            check "accept starts exactly one background transfer" (count==1)
            responsive<-timeout 1000000 (tickDebugger runtime core progress)
            check "held transfer never blocks editor tick" (maybe False (const True) responsive)
            changed<-case mode of
              "cancel"->send runtime "downloads" ["0","0"] progress
              "toolchain"->pure progress {toolchain=Just THC}
              "project"->pure progress {defaultDirectory=Just library}
              "edit"->pure (insertText "x" progress)
              "reload"->pure progress {buffers=M.map (\doc->doc {documentBuffer=newBuffer "main = print (1::Int)\n"}) (buffers progress)}
              "settings"->snd <$> debuggerEffects runtime core progress [AgentAction "toolchain" ["GHC","ghc-9.12.4"]]
              "stop"->send runtime "disconnect" [] progress
              "newer"->send runtime "launch" [] progress
              _->pure progress
            observed<-tickDebugger runtime core changed
            if mode=="cancel" then do
              _<-waitFor runtime "cancelled" (has "Cancelled") observed
              cleaned<-timeout 1000000 (readMVar finished)
              check "cancel joins transfer cleanup off UI" (cleaned==Just ())
            else do
              putMVar gate ()
              if mode=="success" then do
                _<-waitFor runtime "original launch" (\_->T.isInfixOf "\"command\": \"launch\"" <$> TIO.readFile logFile) observed
                entries<-mapM (either error pure.eitherDecodeStrict'.encodeText) . T.lines =<< TIO.readFile logFile
                let launches=[args | entry<-entries,Just request<-[field "request" entry],field "command" request==Just ("launch"::T.Text),Just args<-[field "arguments" request]]
                check "success continues captured launch arguments" (any (\args->field "projectRoot" args==Just root && field "entryFile" args==Just ("Main.hs"::String) && field "entryArgs" args==Just (["original argument"]::[String])) launches)
                completed<-send runtime "downloads" [] observed
                noCancel<-send runtime "downloads" ["0","0"] completed
                check "finished transfer never claims to be cancelling" (status noCancel=="This download has already finished.")
              else do
                _<-timeout 1000000 (readMVar finished) >>= maybe (error "download did not finish") pure
                -- Observe both completion and possible queued continuation.
                final<-foldTicks runtime core 20 observed
                logText<-TIO.readFile logFile
                check ("stale "++mode++" never launches") (T.null logText && status final=="Debugger installed; the original launch is no longer current.")
  putStrLn "Debugger acquisition checks passed"
  where
    check label okay=unless okay (error label)
    field key=parseMaybe (withObject "field" (.: key))
    quote value="'"<>T.replace "'" "'\\''" (T.pack value)<>"'"
    executableFile path text=do TIO.writeFile path text; permissions<-getPermissions path;setPermissions path permissions {executable=True}
    encodeText=Data.Text.Encoding.encodeUtf8
    foldTicks :: Debugger -> Core -> Int -> Desktop -> IO Desktop
    foldTicks _ _ 0 d=pure d
    foldTicks runtime core n d=tickDebugger runtime core d >>= \next->threadDelay 1000>>foldTicks runtime core (n-1) next

withEnvironment :: [(String,String)] -> IO a -> IO a
withEnvironment entries action=bracket (mapM (\(name,_)->(name,) <$> lookupEnv name) entries)
  (mapM_ (\(name,value)->maybe (unsetEnv name) (setEnv name) value))
  (\_->mapM_ (uncurry setEnv) entries>>action)
