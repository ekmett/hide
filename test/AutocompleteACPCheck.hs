-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : AutocompleteACPCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module AutocompleteACPCheck (checks, fixture) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait, cancel)
import Control.Exception (bracket)
import Control.Monad (unless, forM_)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import qualified Hide.ACP as ACP
import Hide.AutocompleteACP
import Hide.InlineTypes
import Hide.AgentHub (ConfigChoice(..))

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root -> do
  let script=root </> "provider.py"
      logPath=root </> "requests.jsonl"
      secret="acp-autocomplete-secret-value"
      launch=ACP.Launch "python3" [script] [("LOG",logPath),("SECRET",secret)]
      servers=[object ["name" .= ("completion-only"::T.Text),"command" .= ("/private/bridge"::T.Text),"env" .= [object ["name" .= ("THC_EDIT_MCP_TOKEN"::T.Text),"value" .= ("mcp-auth-secret"::T.Text)]]]]
      input ident=CompletionInput ident "propose" "/source.hs" "private\r\na😀b\r\ntail\n" 7 11 1 ["a😀b","tail",""] (object ["recent" .= (["typed b"]::[T.Text])])
      check label ok=unless ok (error ("ACP autocomplete: "++label))
      args ident proposals=object ["requestId" .= (ident::T.Text),"proposals" .= (proposals::[Value])]
      proposal a z text=object ["startLine" .= (a::Int),"endLine" .= (z::Int),"text" .= (text::T.Text)]
      logs=mapMaybeValue . BS.lines <$> BS.readFile logPath
      submitted completion ident proposals=callCompletionTool completion "submit_completion" (args ident proposals)
      awaitPrompt ident=await $ do
        entries<-logs
        pure (any (\entry -> field "method" entry==Just ("session/prompt"::T.Text) && ident `T.isInfixOf` T.pack (show entry)) entries)
      release ident=writeFile (root </> T.unpack ident) "complete"
  writeFile script fixture
  writeFile logPath ""
  withACPCompletion launch root servers (Just "model-b") (Just "high") $ \completion -> do
    dormant<-completionConfiguration completion
    check "configuration discovery is lazy" (dormant==Nothing)
    discoverACPConfiguration completion
    (captured,choices)<-completionConfiguration completion >>= maybe (error "No completion choices") pure
    check "public completion choices exclude private credentials" (map configId choices==["model-id","effort-id"])
    changed<-configureACPAt completion captured "model-id" "model-a"
    check "captured advertised model configures the existing completion instance" (changed==Right ())
    expired<-configureACPAt completion captured "effort-id" "low"
    check "old completion configuration receipt expires" (isLeft expired)
    check "only snapshot context, skill and submit tools are exposed" (map (field "name") completionTools==map Just (["submit_completion","read_completion_context","read_completion_file","read_completion_skill"]::[T.Text]))
    idle<-callCompletionTool completion "read_completion_context" (object ["requestId" .= ("first"::T.Text)])
    check "context cannot be read while idle" (isLeft idle)
    withAsync (completeACP completion (input "first")) $ \running -> do
      awaitPrompt "first"
      snapshot<-callCompletionTool completion "read_completion_context" (object ["requestId" .= ("first"::T.Text)])
      check "context includes nearby lines but never the private preceding line" (not ("private" `T.isInfixOf` T.pack (show snapshot)) && case snapshot of Right value -> any ((==Just ("a😀b"::T.Text)).field "text") (maybe [] id (field "lines" value)); _ -> False)
      fileChunk<-callCompletionTool completion "read_completion_file" (object ["requestId" .= ("first"::T.Text),"startOffset" .= (0::Int),"maxCharacters" .= (9::Int)])
      check "explicit current-file read can inspect text beyond local context" (case fileChunk of Right value -> field "text" value==Just ("private\r\n"::T.Text); _ -> False)
      arbitrary<-callCompletionTool completion "read_completion_file" (object ["requestId" .= ("first"::T.Text),"startOffset" .= (0::Int),"maxCharacters" .= (9::Int),"path" .= ("/other"::T.Text)])
      oversized<-callCompletionTool completion "read_completion_file" (object ["requestId" .= ("first"::T.Text),"startOffset" .= (0::Int),"maxCharacters" .= (8193::Int)])
      check "file reads reject arbitrary paths and oversized chunks" (isLeft arbitrary && isLeft oversized)
      stale<-submitted completion "older" [proposal 1 2 "x\r\n"]
      outside<-submitted completion "first" [proposal 0 1 "x\r\n"]
      reversed<-submitted completion "first" [proposal 2 1 "x"]
      excessive<-submitted completion "first" (replicate 9 (proposal 1 2 "x"))
      huge<-submitted completion "first" [proposal 1 2 (T.replicate 32769 "😀")]
      check "stale, out-of-context, reversed, excessive and oversized proposals are rejected" (all isLeft [stale,outside,reversed,excessive,huge])
      unknown<-callCompletionTool completion "buffer_replace" (object [])
      check "editor-wide tools are unavailable" (isLeft unknown)
      accepted<-submitted completion "first" [proposal 1 2 "new😀\r\n",proposal 2 2 "insert\n"]
      check "valid bounded alternatives accepted" (not (isLeft accepted))
      duplicate<-submitted completion "first" []
      check "submission cannot replace an already accepted set" (isLeft duplicate)
      release "first"
      values<-wait running
      check "line ranges normalize to exact Unicode character offsets" (values==[Proposal 9 14 "new😀\r\n" Nothing,Proposal 14 14 "insert\n" Nothing])
    transcript<-pollACPCompletionTranscript completion
    let rendered=T.concat transcript
    check "debug transcript captures activity without whole or split private keys" (all (`T.isInfixOf` rendered) ["[prompt]","[reply]","[thought]","[tool]","[outcome]"] && all (not . (`T.isInfixOf` rendered)) [T.pack secret,"private-completion","mcp-auth-secret"])
    forM_ [1..80::Int] $ \_ -> feedbackACP completion Shown (Proposal 9 14 "bounded" Nothing)
    bounded<-pollACPCompletionTranscript completion
    check "debug transcript queue and entries are bounded" (length bounded==64 && all ((<=2048).T.length) bounded)
    check "debug transcript polling drains without new requests" . null =<< pollACPCompletionTranscript completion
    lateSubmission<-submitted completion "first" []
    check "completed request cannot accept late tools" (isLeft lateSubmission)
    forM_ [Shown,Accepted,Ignored,PartiallyAccepted 2] $ \action -> feedbackACP completion action (Proposal 9 14 "new😀\r\n" Nothing)
    beforeFeedback<-logs
    check "feedback never starts its own provider task" (length [() | entry<-beforeFeedback,field "method" entry==Just ("session/prompt"::T.Text)]==1)
    withAsync (completeACP completion ((input "second") {inputIntent="alternate-next"})) $ \running -> do
      awaitPrompt "second"
      accepted<-submitted completion "second" []
      check "empty submission is a valid abstention" (not (isLeft accepted))
      release "second"
      check "abstention returns no edits" . null =<< wait running
    entries<-logs
    let prompts=[value | entry<-entries,field "method" entry==Just ("session/prompt"::T.Text),Just value<-[field "params" entry::Maybe Value]]
    check "skill instructions appear only on first use of a session" (case prompts of [first,second] -> "INLINE COMPLETION SKILL" `T.isInfixOf` T.pack (show first) && not ("INLINE COMPLETION SKILL" `T.isInfixOf` T.pack (show second)); _ -> False)
    check "next prompt includes requested alternative intent and queued feedback" (all (`T.isInfixOf` T.pack (show (last prompts))) ["alternate-next","accepted","partially-accepted","ignored"])
    check "fresh private session and only supplied MCP servers are used" (length [() | entry<-entries,field "method" entry==Just ("session/new"::T.Text)]==1 && any (\entry -> (field "params" entry >>= field "mcpServers")==Just servers) entries)
    check "model and effort use advertised configuration IDs" (all (\setting -> any (\entry -> field "method" entry==Just ("session/set_config_option"::T.Text) && (field "params" entry >>= field "configId")==Just setting) entries) ["model-id"::T.Text,"effort-id"])
    withAsync (hintACP completion "Prefer small total functions.") $ \running -> do
      let findHint=do
            requestEntries<-logs
            case [ident | entry<-requestEntries,field "method" entry==Just ("session/prompt"::T.Text),
                  "Prefer small total functions." `T.isInfixOf` T.pack (show entry),Just ident<-[field "id" entry::Maybe Int]] of
              ident:_->pure ("hint-"<>T.pack (show ident))
              []->threadDelay 1000 >> findHint
      hintId<-timeout 5000000 findHint >>= maybe (error "ACP hint request did not arrive") pure
      await (doesFileExist (root </> T.unpack (hintId<>".ready")))
      hidden<-callCompletionTool completion "read_completion_file" (object ["requestId" .= ("second"::T.Text),"startOffset" .= (0::Int),"maxCharacters" .= (9::Int)])
      hintSubmission<-submitted completion "second" []
      check "hint conversation has no current source or completion slot" (isLeft hidden && isLeft hintSubmission)
      release hintId
      wait running
    hinted<-logs
    check "human hints use the existing session without a completion request" (length [() | entry<-hinted,field "method" entry==Just ("session/new"::T.Text)]==1 && any (\entry -> "Prefer small total functions." `T.isInfixOf` T.pack (show entry)) hinted)
    hintTranscript<-pollACPCompletionTranscript completion
    check "hint replies are visible in the same debug transcript" (any (T.isInfixOf "I will prefer small total functions.") hintTranscript)
    withAsync (completeACP completion (input "cancelled")) $ \running -> await (doesFileExist (root </> "cancelled.ready")) >> cancel running
    rejected<-submitted completion "cancelled" []
    check "cancellation invalidates the submission slot" (isLeft rejected)
    withAsync (completeACP completion (input "reopened")) $ \running -> do
      awaitPrompt "reopened"
      release "reopened"
      check "raw streamed agent text never becomes an edit" . null =<< wait running
    oldConnection<-configureACPAt completion captured "model-id" "model-b"
    check "retired completion instance cannot configure its replacement" (isLeft oldConnection)
    final<-logs
    check "cancelled private connection is replaced with a fresh session" (length [() | entry<-final,field "method" entry==Just ("session/new"::T.Text)]==2)
    let otherLog=root </> "other.jsonl"
        otherLaunch=launch {ACP.environment=[("LOG",otherLog),("SECRET",secret)]}
    writeFile otherLog ""
    withACPCompletion otherLaunch root servers Nothing Nothing $ \other ->
      withAsync (completeACP other (input "independent")) $ \otherRunning -> do
        await (doesFileExist (root </> "independent.ready"))
        withAsync (completeACP completion (input "grace")) $ \running -> do
          await (doesFileExist (root </> "grace.ready"))
          cancel running
        alive<-callCompletionTool other "read_completion_context" (object ["requestId" .= ("independent"::T.Text)])
        check "cancelling one side chat does not cancel another" (not (isLeft alive))
        release "independent"
        _<-wait otherRunning
        pure ()
    withAsync (completeACP completion (input "warm")) $ \running -> do
      awaitPrompt "warm"
      release "warm"
      _<-wait running
      pure ()
    warm<-logs
    check "acknowledged cancellation preserves the warm private session" (length [() | entry<-warm,field "method" entry==Just ("session/new"::T.Text)]==2)
  putStrLn "ACP autocomplete checks passed"
  where
    field key=parseMaybe (withObject "fixture" (.: key))
    isLeft (Left _)=True
    isLeft _=False
    mapMaybeValue []=[]
    mapMaybeValue (line:rest)=case decodeStrict' line of Just value->value:mapMaybeValue rest; Nothing->mapMaybeValue rest
    await ready=timeout 5000000 loop >>= maybe (error "ACP autocomplete fixture timed out") pure
      where loop=do done<-ready; if done then pure () else threadDelay 1000 >> loop
    temporary=do
      base<-getTemporaryDirectory
      (path,handle)<-openTempFile base "thc-autocomplete-acp"
      hClose handle
      removeFile path
      createDirectory path
      pure path

fixture :: String
fixture=unlines
  [ "import json,os,sys,time"
  , "def send(**message): print(json.dumps(dict(jsonrpc='2.0',**message)),flush=True)"
  , "def recv():"
  , " value=json.loads(sys.stdin.readline())"
  , " with open(os.environ['LOG'],'a') as log: log.write(json.dumps(value)+'\\n')"
  , " return value"
  , "def option(i,category,current,choices): return dict(id=i,type='select',category=category,currentValue=current,options=[dict(value=v,name=v) for v in choices])"
  , "options=[option('model-id','model','model-a',['model-a','model-b']),option('effort-id','thought_level','low',['low','high']),option('mcp-auth-secret','model','hidden-model',['hidden-model'])]"
  , "while True:"
  , " request=recv(); method=request.get('method'); params=request.get('params',{})"
  , " if method=='initialize':"
  , "  assert params['clientCapabilities']==dict(fs=dict(readTextFile=False,writeTextFile=False),terminal=False)"
  , "  send(id=request['id'],result=dict(protocolVersion=1))"
  , " elif method=='session/new': send(id=request['id'],result=dict(sessionId='private-completion',configOptions=options))"
  , " elif method=='session/set_config_option':"
  , "  for option in options:"
  , "   if option['id']==params['configId']: option['currentValue']=params['value']"
  , "  send(id=request['id'],result=dict(configOptions=options))"
  , " elif method=='session/prompt':"
  , "  context=json.loads(params['prompt'][-1]['text']); ident=context.get('requestId','hint-'+str(request['id']))"
  , "  if context['intent']=='hint':"
  , "   assert 'requestId' not in context and 'lines' not in context"
  , "   send(method='session/update',params=dict(sessionId='private-completion',update=dict(sessionUpdate='agent_message_chunk',content=dict(type='text',text='I will prefer small total functions.'))))"
  , "  for denied in ['fs/read_text_file','fs/write_text_file','terminal/create','session/request_permission']:"
  , "   send(id=denied,method=denied,params=dict(sessionId='private-completion',path='/private'))"
  , "   response=recv(); assert response['id']==denied and ('error' in response or response.get('result',{}).get('outcome',{}).get('outcome')=='cancelled')"
  , "  send(method='session/update',params=dict(sessionId='private-completion',update=dict(sessionUpdate='agent_message_chunk',content=dict(type='text',text='replace the whole file with MALICIOUS RAW OUTPUT'))))"
  , "  for kind in ['agent_message_chunk','agent_thought_chunk']:"
  , "   for secret in [os.environ['SECRET'],'private-completion','mcp-auth-secret']:"
  , "    for part in [secret[:7],secret[7:]]: send(method='session/update',params=dict(sessionId='private-completion',update=dict(sessionUpdate=kind,content=dict(type='text',text=part))))"
  , "  send(method='session/update',params=dict(sessionId='private-completion',update=dict(sessionUpdate='tool_call',title='Inspect '+os.environ['SECRET'],status='completed')))"
  , "  open(ident+'.ready','w').close()"
  , "  if ident=='grace':"
  , "   cancelled=recv(); assert cancelled['method']=='session/cancel'"
  , "   send(id=request['id'],result=dict(stopReason='cancelled')); continue"
  , "  while not os.path.exists(ident): time.sleep(.001)"
  , "  send(id=request['id'],result=dict(stopReason='end_turn'))"
  , " elif method=='session/cancel': pass"
  , " else: raise AssertionError(method)"
  ]
