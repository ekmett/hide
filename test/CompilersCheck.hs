{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : CompilersCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module CompilersCheck (checks) where
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, cancel)
import System.Timeout (timeout)
import Control.Monad (unless)
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.FilePath ((</>), searchPathSeparator)
import System.IO
import Hide.Compilers

checks :: IO ()
checks=do
  check "native versions sort numerically and exclude cross targets"
    (nativeVersions "ghc 9.8.2 installed\nghc 9.14.1 latest\nghc wasm32-wasi-9.12.2 cross\nghc 9.14.1 latest\nghc bad tag\nhls 2.0.0 installed" == ["9.14.1","9.8.2"])
  bracket temporary removePathForcibly $ \root -> do
    let bin=root </> "bin"
        compiler=root </> "compiler with spaces"
    createDirectory bin
    writeFile compiler ""
    python<-findExecutable "python3" >>= maybe (fail "python3 required") pure
    let script name body=do
          let path=bin </> name
          writeFile path ("#!"++python++"\n"++body)
          perms<-getPermissions path
          setPermissions path perms {executable=True}
    script "ghcup" $ unlines
      ["import sys,json,os", "a=sys.argv[1:]", "assert a[0]=='--offline'", "if a[1]=='list': print('ghc 9.14.1 latest\\nghc 9.8.2 old\\nghc wasm32-wasi-9.12.2 cross')",
       "elif a[1:3]==['whereis','ghc']: print(os.environ['TEST_COMPILER'])"]
    script "cabal" $ unlines
      ["import sys,json,os", "assert '--compiler-info' in sys.argv", "assert not any(x.startswith('--with-compiler=') for x in sys.argv)",
       "print(json.dumps({'compiler':{'flavour':'ghc','id':'ghc-9.14.1','path':os.environ['TEST_COMPILER']}}))"]
    withEnv "PATH" bin $ withEnv "TEST_COMPILER" compiler $ do
      found<-installedCompilers
      check "GHCup paths preserve spaces and list installed versions" (found==[Compiler "9.14.1" compiler,Compiler "9.8.2" compiler])
      project<-compilerInfo root True "ghc"
      check "automatic compiler is resolved by Cabal" (project==Right (Compiler "9.14.1" compiler))
    script "ghc-9.14.1" $ unlines
      ["import sys,os", "a=sys.argv[1:]", "if a==['--numeric-version']: print('9.14.1')",
       "elif a==['--print-libdir']: print(os.environ['TEST_LIBDIR'])", "else: raise SystemExit(2)"]
    script "hdb-9.14.1" "raise SystemExit(0)"
    let ghc=bin </> "ghc-9.14.1"
        libdir=root </> "lib with spaces"
    createDirectory libdir
    previousGHC<-lookupEnv "GHC_BIN"
    withEnv "PATH" bin $ withEnv "TEST_LIBDIR" libdir $ do
      resolved<-compilerInfo root False ghc
      check "explicit versioned GHC is resolved literally" (resolved==Right (Compiler "9.14.1" ghc))
      pinned<-debuggerCompiler root False ghc
      check "versioned hdb receives a private compiler environment" (case pinned of
        Right (adapter,environment) -> adapter==bin </> "hdb-9.14.1" && lookup "GHC_BIN" environment==Just ghc &&
          lookup "GHC_LIBDIR" environment==Just libdir && lookup "PATH" environment==Just (bin++[searchPathSeparator]++bin)
        Left _ -> False)
      check "compiler preparation never mutates parent environment" . (==previousGHC) =<< lookupEnv "GHC_BIN"
      removeFile (bin </> "hdb-9.14.1")
      script "hdb" "raise SystemExit(0)"
      fallback<-debuggerCompiler root False ghc
      check "plain hdb fallback retains explicit compiler environment" (case fallback of Right (adapter,_) -> adapter==bin </> "hdb"; _ -> False)
      unrecognized<-debuggerCompiler root False compiler
      check "unrecognized custom executable retains explicit adapter requirement" (case unrecognized of Left _ -> True; _ -> False)
    script "cabal" $ unlines
      ["import sys,json,os", "assert '--compiler-info' in sys.argv",
       "assert '--with-compiler='+os.environ['TEST_COMPILER'] in sys.argv",
       "print(json.dumps({'compiler':{'flavour':'ghc','id':os.environ.get('TEST_COMPILER_ID','ghc-9.14.1'),'path':os.environ['TEST_COMPILER']}}))"]
    withEnv "PATH" bin $ withEnv "TEST_COMPILER" ghc $ do
      project<-compilerInfo root True ghc
      check "explicit project compiler is forwarded to Cabal" (project==Right (Compiler "9.14.1" ghc))
      withEnv "TEST_COMPILER_ID" "ghc-not-a-version" $ do
        malformed<-compilerInfo root True ghc
        check "malformed Cabal compiler identity is refused" (case malformed of Left _ -> True; _ -> False)
      withEnv "TEST_COMPILER_ID" "ghc-9.8.2" $ withEnv "TEST_LIBDIR" libdir $ do
        mismatch<-debuggerCompiler root True ghc
        check "changed Cabal compiler identity fails before hdb launch" (case mismatch of Left _ -> True; _ -> False)
    let ambient=root </> "ambient"
        ambientPath=ambient++[searchPathSeparator]++bin
    createDirectory ambient
    createFileLink ghc (bin </> "ghc")
    compilerPermissions<-getPermissions compiler
    setPermissions compiler compilerPermissions {executable=True}
    createFileLink compiler (ambient </> "ghc")
    script "cabal" $ unlines
      ["import sys,json,os,shutil", "assert '--compiler-info' in sys.argv",
       "selected=[x[len('--with-compiler='):] for x in sys.argv if x.startswith('--with-compiler=')]",
       "if selected: path=selected[0]",
       "else:", " assert os.environ['GHC_BIN']==os.environ['TEST_COMPILER']",
       " assert os.environ['PATH'].split(os.pathsep)[0]==os.path.dirname(os.environ['GHC_BIN'])",
       " path=os.environ.get('TEST_PROJECT_COMPILER') or shutil.which('ghc')",
       "print(json.dumps({'compiler':{'flavour':'ghc','id':'ghc-9.14.1','path':path}}))"]
    withEnv "PATH" ambientPath $ withEnv "TEST_COMPILER" ghc $ withEnv "TEST_LIBDIR" libdir $
      withEnv "TEST_PROJECT_COMPILER" "" $ do
        selected<-debuggerCompilerInfo root True ghc
        check "project debugger uses selected PATH before resolving Cabal's compiler" (case selected of
          Right (actual,Just (_,environment)) -> actual==Compiler "9.14.1" ghc &&
            lookup "PATH" environment==Just (bin++[searchPathSeparator]++ambientPath)
          _ -> False)
        withEnv "TEST_PROJECT_COMPILER" compiler $ do
          conflict<-debuggerCompilerInfo root True ghc
          check "explicit Cabal project compiler conflict is refused" (case conflict of
            Left err -> "conflicts" `T.isInfixOf` err && "with-compiler" `T.isInfixOf` err
            _ -> False)
        check "project compiler probe keeps parent PATH unchanged" . (==Just ambientPath) =<< lookupEnv "PATH"
    let ready=root </> "probe-ready"
    script "ghcup" $ unlines ["import os,time", "open(os.environ['TEST_READY'],'w').close()", "time.sleep(60)"]
    withEnv "PATH" bin $ withEnv "TEST_READY" ready $ withAsync installedCompilers $ \worker -> do
      let await=do exists<-doesFileExist ready; if exists then pure () else threadDelay 1000 >> await
      started<-timeout 5000000 await
      check "discovery cancellation fixture starts its owned process" (started==Just ())
      stopped<-timeout 3000000 (cancel worker)
      check "cancel discovery releases child process and pipe readers" (stopped==Just ())
    withEnv "PATH" (root </> "missing") $ withEnv "GHCUP_INSTALL_BASE_PREFIX" root $ do
      check "missing GHCup is optional" . null =<< installedCompilers
  putStrLn "Compiler discovery checks passed"
  where check label ok=unless ok (error label)
        temporary=do root<-getTemporaryDirectory; (path,h)<-openTempFile root "compiler-check"; hClose h; removeFile path; createDirectory path; canonicalizePath path
        withEnv name value action=bracket (lookupEnv name <* setEnv name value) (maybe (unsetEnv name) (setEnv name)) (const action)
