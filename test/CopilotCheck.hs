{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : CopilotCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module CopilotCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async,cancel,waitCatch)
import Control.Exception (IOException,bracket,try,displayException)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))
import System.IO (openTempFile,hClose)
import System.Timeout (timeout)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import qualified Hide.ACP as ACP
import Hide.Copilot
import Hide.InlineTypes

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->do
  let server=root </> "fake-copilot.py"
      source=root </> "space λ.py"
      launch=ACP.Launch "python3" [server,"--stdio"] []
      input text version=CompletionInput "fixture" "propose" source text version (T.length text) 0 [] Null
  writeFile server fakeServer
  withCopilot launch root $ \client->do
    alternatives<-completeCopilot client (input "a😀" 1)
    check "all valid alternatives survive with Unicode offsets" (map proposalText alternatives==["😀\nnext","other"] && all (\p->proposalStart p==2 && proposalEnd p==2) alternatives)
    first<-case alternatives of item:_->pure item; []->error "Missing Copilot alternative"
    feedbackCopilot client Shown first
    feedbackCopilot client (PartiallyAccepted 2) first
    feedbackCopilot client Accepted first
    feedbackCopilot client Ignored first
    _<-completeCopilot client ((input "a😀" 1) {inputIntent="alternate-next"})
    signin<-signInCopilot client
    check "device flow code and command are returned without signing in" (signInCode signin=="TEST-CODE")
    beforeFinish<-decode <$> BL.readFile (root </> "state.json")
    check "signIn does not finish device flow automatically" (maybe False ((/=Just True) . field "finished") beforeFinish)
    finishSignInCopilot client signin
    signOutCopilot client
    _<-completeCopilot client (input "a😀b\n" 2)
    statuses<-pollCopilotMessages client
    check "server status is sanitized" (not (null statuses) && all (not . T.isInfixOf "secret") statuses)
    held<-async (completeCopilot client (input "hold" 3))
    bounded "completion reached server" (waitFile (root </> "held"))
    cancel held
    _<-waitCatch held
    after<-completeCopilot client (input "after" 4)
    check "late cancelled reply cannot become a later completion" (map proposalText after==["😀\nnext","other"])
    _<-completeCopilot client ((input "other file" 1) {inputPath=root </> "other.py"})
    state<-decode <$> BL.readFile (root </> "state.json")
    case state of
      Nothing->error "Copilot fixture state missing"
      Just value->do
        check "initialize then initialized precede document traffic" (field "initialized" value==Just True)
        check "incremental sync uses previous UTF16 endpoint" (field "firstChangeEnd" value==Just (object ["line" .= (0::Int),"character" .= (3::Int)]))
        check "unchanged source is not reopened or changed for explicit alternatives"
          (field "opens" value==Just (2::Int) && field "changes" value==Just (3::Int) && field "explicit" value==Just (1::Int))
        check "document switches close the previous file" (field "closes" value==Just (1::Int))
        check "cancelled requests send cancellation and leave client reusable" (field "cancels" value==Just (1::Int))
        check "shown feedback preserves opaque item" (field "shown" value==Just ("opaque-item"::T.Text))
        check "partial feedback converts normalized CRLF and astral text to cumulative UTF16" (field "partial" value==Just (4::Int))
        check "accepted feedback invokes server command" (field "accepted" value==Just True)
        check "device flow finishes only on explicit call" (field "finished" value==Just True && field "signedOut" value==Just True)
  check "process exits with owning scope" =<< notRunning (root </> "pid")
  -- Malformed protocol is rejected without including any server payload.
  writeFile server "import sys\nsys.stdout.write('Content-Length: 999999999\\r\\n\\r\\nsecret');sys.stdout.flush()\n"
  failed<-try (withCopilot launch root (const (pure ())))::IO (Either IOException ())
  check "oversized frames fail without raw payload leakage" (case failed of Left e->not ("secret" `T.isInfixOf` T.pack (displayException e)); _->False)
  writeFile server "import time,os\nopen('pid','w').write(str(os.getpid()))\nopen('initializing','w').close()\ntime.sleep(30)\n"
  blocked<-async (withCopilot launch root (const (pure ())))
  bounded "initialization process starts" (waitFile (root </> "initializing"))
  bounded "initialization cancellation joins process workers" (cancel blocked)
  check "cancelled initialization leaves no process behind" =<< notRunning (root </> "pid")
  putStrLn "Copilot checks passed"

notRunning :: FilePath -> IO Bool
notRunning file=do
  pid<-readFile file
  (code,_,_)<-readProcessWithExitCode "python3"
    ["-c","import os,sys\ntry: os.kill(int(sys.argv[1]),0)\nexcept ProcessLookupError: sys.exit(0)\nsys.exit(1)",pid] ""
  pure (code==ExitSuccess)

field :: FromJSON a => T.Text -> Value -> Maybe a
field name=parseMaybe (withObject "fixture" (\o->o .: K.fromText name))

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)

bounded :: String -> IO a -> IO a
bounded label action=timeout 4000000 action >>= maybe (error label) pure

waitFile :: FilePath -> IO ()
waitFile path=doesFileExist path >>= \yes->unless yes (threadDelay 1000 >> waitFile path)

temporary :: IO FilePath
temporary=do
  base<-getTemporaryDirectory
  (path,h)<-openTempFile base "thc-copilot-check"
  hClose h
  removeFile path
  createDirectory path
  pure path

fakeServer :: String
fakeServer=unlines
  [ "import sys,json,os"
  , "state={'initialized':False,'closes':0,'cancels':0,'opens':0,'changes':0,'explicit':0}; document=None; version=None"
  , "open('pid','w').write(str(os.getpid()))"
  , "def send(v):"
  , " b=json.dumps(v,ensure_ascii=False).encode();sys.stdout.buffer.write(('Content-Length: %d\\r\\n\\r\\n'%len(b)).encode()+b);sys.stdout.buffer.flush()"
  , "def save():"
  , " with open('state.json','w') as f:json.dump(state,f)"
  , "while True:"
  , " headers={}"
  , " while True:"
  , "  line=sys.stdin.buffer.readline()"
  , "  if not line:sys.exit(0)"
  , "  if line==b'\\r\\n':break"
  , "  k,v=line.decode().split(':',1);headers[k.lower()]=v.strip()"
  , " v=json.loads(sys.stdin.buffer.read(int(headers['content-length'])))"
  , " method=v.get('method');p=v.get('params',{});result=None"
  , " if method=='initialize':result={'capabilities':{'textDocumentSync':2}}"
  , " elif method=='initialized':state['initialized']=True"
  , " elif method=='textDocument/didOpen':"
  , "  assert state['initialized'];state['opens']+=1;document=p['textDocument']['text'];version=p['textDocument']['version']"
  , " elif method=='textDocument/didChange':"
  , "  change=p['contentChanges'][0];assert 'range' in change"
  , "  rows=document.split('\\n');assert change['range']['start']=={'line':0,'character':0};assert change['range']['end']=={'line':len(rows)-1,'character':len(rows[-1].encode('utf-16-le'))//2};state['changes']+=1"
  , "  state.setdefault('firstChangeEnd',change['range']['end']);document=change['text'];version=p['textDocument']['version']"
  , " elif method=='textDocument/didClose':state['closes']+=1"
  , " elif method=='textDocument/inlineCompletion':"
  , "  assert p['textDocument']['version']==version;state['explicit']+=int(p['context']['triggerKind']==1)"
  , "  if document=='hold':"
  , "   open('held','w').close();continue"
  , "  pos=p['position'];item={'insertText':'😀\\r\\nnext','range':{'start':pos,'end':pos},'command':{'command':'github.copilot.didAcceptCompletionItem','arguments':['opaque-item']},'id':'opaque-item'}"
  , "  result={'items':[item,{'insertText':'other'},{'insertText':'bad','range':{'start':{'line':0,'character':2},'end':{'line':-1,'character':0}}},{'insertText':{'kind':'snippet','value':'bad'}}]}"
  , " elif method=='$/cancelRequest':state['cancels']+=1;send({'jsonrpc':'2.0','id':p['id'],'result':{'items':[{'insertText':'stale'}]}})"
  , " elif method=='textDocument/didShowCompletion':state['shown']=p['item']['id']"
  , " elif method=='textDocument/didPartiallyAcceptCompletion':state['partial']=p['acceptedLength']"
  , " elif method=='signIn':result={'userCode':'TEST-CODE','command':{'command':'github.copilot.finishDeviceFlow','arguments':[],'title':'Sign in'}}"
  , " elif method=='signOut':"
  , "  state['signedOut']=True;send({'jsonrpc':'2.0','method':'didChangeStatus','params':{'kind':'Error','message':'secret-token-must-not-escape'}})"
  , " elif method=='workspace/executeCommand':"
  , "  if p['command']=='github.copilot.finishDeviceFlow':state['finished']=True"
  , "  if p['command']=='github.copilot.didAcceptCompletionItem':state['accepted']=p['arguments']==['opaque-item']"
  , " save()"
  , " if 'id' in v:send({'jsonrpc':'2.0','id':v['id'],'result':result})"
  ]
