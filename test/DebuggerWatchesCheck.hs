{-# LANGUAGE OverloadedStrings #-}
module DebuggerWatchesCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll,wait,withAsync)
import Control.Exception (bracket)
import Control.Monad (unless,foldM,forM_,when)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import System.Directory (canonicalizePath,doesFileExist,removeFile)
import System.FilePath (takeDirectory)
import System.Process (ProcessHandle)
import System.Timeout (timeout)
import qualified DebuggerCheck as Fixture
import Hide.Buffer (Selection(..))
import Hide.Debugger
import Hide.DebuggerSidebar
import Hide.DebuggerSidebarTypes
import Hide.Model
import Hide.Sidebar
import Hide.SidebarCommands
import qualified Hide.Plugin.Tree as P
import qualified Hide.Plugin.Menu as Menu

checks :: IO ()
checks=mapM_ session ["pages","pages-edit","pages-remove","pages-resume","pages-frame","pages-retire","pages-oversized","child","child-invalidated","child-error","child-edit","child-remove","child-resume","child-frame","child-retire","child-modal","child-policy","normal","edit","remove","resume","frame","retire","modal","policy"] >> putStrLn "Debugger watch execution checks passed"

session :: String -> IO ()
session scenario=bracket (Fixture.fixture (if take 5 scenario=="child" || take 5 scenario=="pages" then "watches-"<>scenario else if scenario=="policy" then "watches-private" else "watches")) cleanup $ \(port,path,_)->withSidebarCommands $ \host->withDebugger $ \runtime->withDebuggerSidebar host runtime $ \provider->do
  origin<-canonicalizePath (path<>".hs")
  putStrLn ("watch scenario "<>scenario)
  let fallback d _=pure (False,d)
      core=debuggerEffects runtime fallback
      effects=sidebarEffects host core
      tick d=tickDebugger runtime d >>= tickDebuggerSidebar provider host runtime >>= tickSidebar host core
      rows d=maybe [] (M.elems.treeRows) (sideTree d)
      labels=map (P.infoLabel.rowInfo).rows
      has value=any (T.isInfixOf value).labels
      await label predicate=awaitIO label (pure.predicate)
      awaitIO label predicate d=timeout 5000000 (loop d) >>= maybe (fail (label<>" timed out")) pure
        where
          loop current=do
            next<-tick current
            ready<-predicate next
            if ready then pure next else do
              threadDelay 1000 >> loop next
      row title d=case [r | r<-rows d,title `T.isPrefixOf` P.infoLabel (rowInfo r)] of [value]->value; _->error ("missing watch row "<>T.unpack title)
      invoke action value d=case lookup action (rowActions value) of
        Just (P.RegisteredAction reference)->do
          let trace=maybe [] (hitTrace (keyOf (rowHit value))) (sideTree d)
              (next,outbox)=runCommand (TreeCommand trace reference) d
          snd <$> effects next outbox
        _->fail ("missing watch action "<>T.unpack action)
      save expression d=case dialog d of
        Just dg->let filled=dg {fields=[SelectedInput "Expression" expression (Selection 0 (T.length expression))]}
                     (next,outbox)=submitDialog 0 filled d {dialog=Just filled}
                 in snd <$> core next outbox
        _->fail "missing watch editor"
      add expression d=invoke "Add watch…" (row "Watches" d) d >>= await "watch dialog" ((/=Nothing).dialog) >>= save expression >>= await "watch row" (has expression)
      expand title d=case [i | (i,r)<-zip [0..] (rows d),title `T.isPrefixOf` P.infoLabel (rowInfo r)] of
        i:_->let (next,outbox)=activateTree True i d {sideTree=fmap (\tree->tree {treeFocused=True}) (sideTree d)} in snd <$> effects next outbox
        _->fail ("missing expandable watch "<>T.unpack title)
      requests=do logLines<-T.lines <$> TIO.readFile path
                  pure [request | line<-logLines,Just body<-[decodeStrict' (TE.encodeUtf8 line)],Just request<-[field "request" body]] :: IO [Value]
      executing=filter ((==Just ("evaluate"::T.Text)).field "command") <$> requests
      rejectInspect target extra d=do
        (_,current)<-debuggerTool runtime d "debug_status" (object [])
        value<-current >>= either (fail.T.unpack) pure
        let gen=maybe (error "missing generation") id (field "generation" value :: Maybe Int)
        (_,finish)<-debuggerTool runtime d "debug_inspect" (object (["generation" .= gen,"request" .= (target::T.Text)]++extra))
        outcome<-finish
        check "read-only MCP cannot execute or force a watch" (either (const True) (const False) outcome)
      entries=do (_,selected,values)<-debuggerWatches runtime; pure (selected,values)
      release d=writeFile (path<>".release") "release" >> barrier d
      barrier d=do
        (_,current)<-debuggerTool runtime d "debug_status" (object [])
        value<-current >>= either (fail.T.unpack) pure
        (_,finish)<-debuggerTool runtime d "debug_inspect" (object ["generation" .= maybe (0::Int) id (field "generation" value),"request" .= ("threads"::T.Text)])
        withAsync finish $ \reply->do
          let loop current=do
                next<-tick current
                result<-poll reply
                if maybe False (const True) result then pure next else threadDelay 1000 >> loop next
          drained<-timeout 5000000 (loop d) >>= maybe (fail "ordered watch response barrier timed out") pure
          -- Continue can expire this receipt before its ordered response.
          -- The response handler still resolves it only after consuming the reply.
          outcome<-wait reply
          case outcome of
            Right _->pure ()
            Left "Debugger inspection expired; refresh debug_status."->pure ()
            Left err->fail (T.unpack err)
          pure drained
  mounted<-initializeSidebar host (initialDesktop (90,30)) {sideTree=Just (emptySidebar (takeDirectory path) 28 False)} >>= await "Watches provider" (has "Watches")
  (_,attached)<-core mounted [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
  stopped<-await "stopped selected frame" (T.isPrefixOf "Stopped in ".status) attached
  empty<-expand "Watches" stopped >>= await "empty watches" (has "No watches")
  let childScenario=take 5 scenario=="child"
      expression=if scenario=="normal" then "counter + 1" else if childScenario || take 5 scenario=="pages" then "record" else "delay"
  created<-add expression empty
  quiet<-foldM (\d _->tick d) created [1..20::Int]
  check "passive stop/tree/tick never evaluates" . null =<< executing
  let initialRow=row expression quiet
      agentRef=case lookup "Evaluate watch" (rowActions initialRow) of Just (P.RegisteredAction ref)->ref; _->error "missing evaluate action"
      agentTrace=maybe [] (hitTrace (keyOf (rowHit initialRow))) (sideTree quiet)
  (_,agentRejected)<-effects quiet [InvokeTree agentTrace agentRef Menu.AgentMenu]
  check "agent origin does not gain executing sidebar authority" ("stale, protected or busy" `T.isInfixOf` status agentRejected)
  check "agent attempt did not evaluate" . null =<< executing
  evaluated<-invoke "Evaluate watch" (row expression quiet) quiet >>= await "explicit request" (T.isPrefixOf "Evaluating watch".status)
  if take 5 scenario=="pages" then do
    loaded<-await "pageable watch" (has "record = Record") evaluated
    expanded<-expand "record" loaded
    if scenario=="pages-oversized" then do
      failed<-await "oversized child snapshot is refused" (has "exceeds 1 MiB") expanded
      check "oversized snapshot does not publish a silently truncated page" (not (has "item0 =" failed))
    else do
      let hasWatchMore d=any (\r->case rowAction r of LoadNext{}->rowHit r==rowHit (row "record" d); _->False) (rows d)
      first<-await "first watch page and More" (\d->has "item127 =" d && hasWatchMore d) expanded
      let childRows=filter (T.isPrefixOf "item".P.infoLabel.rowInfo).rows
          watchMore d=[i | (i,r)<-zip [0..] (rows d),LoadNext{}<-[rowAction r],rowHit r==rowHit (row "record" d)]
          more d=case watchMore d of
            [i]->pure (activateTree True i d)
            _->fail "missing Watch More row"
      check "first watch page contains exactly128 children" (length (childRows first)==128 && not (has "item128 =" first))
      (firstSelected,firstValues)<-entries
      let [(firstKey,firstEntry)]=M.toList firstValues
          firstReceipt=maybe (error "missing first page frame") id firstSelected
      (_,unpublished)<-core first [DebugSidebarAction (ForceDebugWatchChild firstKey (watchRevision firstEntry) firstReceipt 980 128 1 971)]
      check "retained snapshot tail does not grant unpublished child authority" ("expired" `T.isInfixOf` status unpublished)
      (loading,outbox)<-more first
      second<-snd <$> effects loading outbox >>= await "second watch page" (\d->has "item255 =" d && hasWatchMore d)
      check "second page adds exactly128 children" (length (childRows second)==256 && not (has "item256 =" second))
      (loadingLast,oldMore)<-more second
      lastPage<-snd <$> effects loadingLast oldMore >>= await "last watch page" (\d->has "item259 =" d && not (hasWatchMore d))
      check "final page preserves all260 children" (length (childRows lastPage)==260)
      sent<-requests
      let parentReads=[r | r<-sent,field "command" r==Just ("variables"::T.Text),(field "arguments" r >>= field "variablesReference")==Just (980::Int)]
      check "More pages do not resend DAP variables" (length parentReads==1)
      (selected,values)<-entries
      let [(key,entry)]=M.toList values
          receipt@(WatchFrame _ epoch _ _ _)=maybe (error "missing page frame") id selected
          request=DebugPageRequest epoch (DebugWatchVariables key (watchRevision entry) receipt 980) 256
          force=ForceDebugWatchChild key (watchRevision entry) receipt 980 128 1 971
      forM_ [ForceDebugWatchChild key (watchRevision entry) receipt 980 0 1 971,
        ForceDebugWatchChild key (watchRevision entry) receipt 980 128 0 971,
        ForceDebugWatchChild key (watchRevision entry) receipt 980 129 1 971] $ \guessed->do
        (_,refused)<-core lastPage [DebugSidebarAction guessed]
        check "later child Force requires exact page offset and position" ("expired" `T.isInfixOf` status refused)
      changed<-case scenario of
        "pages"->invoke "Force lazy child" (row "item129 =" lastPage) lastPage >>= await "later-page child forced" (has "Child forced; evaluate watch")
        "pages-edit"->snd <$> core lastPage [DebugSidebarAction (EditDebugWatch key (watchRevision entry))] >>= save "replacement"
        "pages-remove"->snd <$> core lastPage [DebugSidebarAction (RemoveDebugWatch key (watchRevision entry))]
        "pages-resume"->snd <$> core lastPage [DebugAction "continue" []]
        "pages-retire"->retireTreeFromHost host (case receipt of WatchFrame owner _ _ _ _->owner) lastPage
        _->snd <$> core lastPage [DebugAction "stack" []] >>= await "page frame chooser" ((/=Nothing).dialog) >>= \d->case dialog d of
          Just dg->let chosen=dg {fields=map (\field->case field of ListBox title values _->ListBox title values 1; _->field) (fields dg)}
                       (next,outbox)=submitDialog 0 chosen d
                   in snd <$> core next outbox
          _->fail "missing page frame chooser"
      (_,oldForce)<-core changed [DebugSidebarAction force]
      _<-effects oldForce oldMore
      -- Poll the actual owner refusal, rather than infer retirement from a quiet
      -- UI before a delayed request has reached the debugger mailbox.
      withAsync (debuggerSidebarRead runtime request) $ \reply->do
        _<-awaitIO "stale page receipt resolves" (\_->maybe False (const True) <$> poll reply) changed
        result<-wait reply
        check "old stopped/watch/provider receipt cannot read cached children" (either (const True) (const False) result)
      after<-requests
      check "retained More/Force cannot revive a retired snapshot" (length [r | r<-after,field "command" r==Just ("variables"::T.Text)]==length [r | r<-sent,field "command" r==Just ("variables"::T.Text)]+if scenario=="pages" then 1 else 0)
      check "later-page Force sends only the captured lazy reference" (length [r | r<-after,field "command" r==Just ("variables"::T.Text),(field "arguments" r >>= field "variablesReference")==Just (971::Int)]==if scenario=="pages" then 1 else 0)
  else if childScenario then do
    loaded<-await "nonlazy parent watch" (has "record = Record") evaluated
    expanded<-expand "record" loaded >>= await "nested lazy child" (has "nested =")
    let child=row "nested =" expanded
    check "lazy child stays inert but offers explicit Force" (not (P.infoBranch (rowInfo child)) && any ((=="Force lazy child").fst) (rowActions child))
    before<-requests
    idle<-foldM (\d _->tick d) expanded [1..20::Int]
    check "passive child display never forces" . (==length before) . length =<< requests
    rejectInspect "variables" ["variablesReference" .= (971::Int)] idle
    let childRef=case lookup "Force lazy child" (rowActions child) of Just (P.RegisteredAction ref)->ref; _->error "missing child force"
        childTrace=maybe [] (hitTrace (keyOf (rowHit child))) (sideTree idle)
    (_,refused)<-effects idle [InvokeTree childTrace childRef Menu.AgentMenu]
    afterAgent<-requests
    check "agent cannot force a lazy child" (length before==length afterAgent && "stale, protected or busy" `T.isInfixOf` status refused)
    (selected,values)<-entries
    let [(key,entry)]=M.toList values
        receipt=maybe (error "missing child frame") id selected
        command=ForceDebugWatchChild key (watchRevision entry) receipt 980 0 1 971
    forM_ [ForceDebugWatchChild key (watchRevision entry) receipt 980 0 (-1) 971,
      ForceDebugWatchChild key (watchRevision entry) receipt 980 0 128 971,
      ForceDebugWatchChild key (watchRevision entry) receipt 980 0 0 971,
      ForceDebugWatchChild key (watchRevision entry) receipt 999 0 1 971,
      ForceDebugWatchChild key (watchRevision entry) receipt 980 0 1 972] $ \guessed->do
      (_,rejected)<-core refused [DebugSidebarAction guessed]
      check "child Force requires exact cached page and position" ("expired" `T.isInfixOf` status rejected)
    forcing<-invoke "Force lazy child" child refused
    pending<-awaitIO "real child request reaches adapter" (\_->any (\request->(field "arguments" request >>= field "variablesReference")==Just (971::Int)) <$> requests) forcing
    let changing action=case action of
          "edit"->snd <$> core pending [DebugSidebarAction (EditDebugWatch key (watchRevision entry))] >>= save "replacement"
          "remove"->snd <$> core pending [DebugSidebarAction (RemoveDebugWatch key (watchRevision entry))]
          "resume"->snd <$> core pending [DebugAction "continue" []]
          "frame"->snd <$> core pending [DebugAction "stack" []] >>= await "child frame chooser" ((/=Nothing).dialog) >>= \d->case dialog d of
            Just dg->let chosen=dg {fields=map (\field->case field of ListBox title values _->ListBox title values 1; _->field) (fields dg)}
                         (next,outbox)=submitDialog 0 chosen d
                     in snd <$> core next outbox
            _->fail "missing child frame chooser"
          "retire"->retireTreeFromHost host (case receipt of WatchFrame owner _ _ _ _->owner) pending
          "policy"->pure pending {guestPrivatePaths=[origin]}
          _->pure pending {dialog=Just (Dialog "Human child modal" (DebuggerWatchDialog 999 Nothing False) [Input "Expression" "" 0] 0 ["Cancel"] [])}
    outcome<-if scenario=="child" then await "forced child refresh marker" (has "Child forced; evaluate watch") pending
      else if scenario=="child-error" then await "child local error" (has "error: fixture refused") pending
      else if scenario=="child-invalidated" then barrier pending
      else changing (drop 6 scenario) >>= release
    finished<-await "retired child projection" (not.has "nested =") outcome
    sent<-requests
    let childRequests=[request | request<-sent,(field "arguments" request >>= field "variablesReference")==Just (971::Int)]
    check "captured child forces exactly once" (length childRequests==1)
    check "child forcing retires the old displayed subtree" (not (has "nested =" finished))
    (_,oldOwner)<-core finished [DebugSidebarAction command]
    afterOwner<-requests
    check "old stopped child receipt cannot force twice" (length sent==length afterOwner && ("expired" `T.isInfixOf` status oldOwner || scenario=="child-modal" && "dialog owns input" `T.isInfixOf` status oldOwner))
    _<-effects finished [InvokeTree childTrace childRef Menu.HumanMenu]
    afterOld<-requests
    check "retained child action cannot force twice" (length sent==length afterOld)
    rejectInspect "variables" ["variablesReference" .= (971::Int)] finished
    rejectInspect "variables" ["variablesReference" .= (972::Int)] finished
    (_,after)<-entries
    check "Force never republishes a requested or replacement handle" (all (\value->case watchValue value of WatchResult _ _ ref _ _->ref/=971 && ref/=972; _->True) (M.elems after))
    when (scenario=="child" || scenario=="child-invalidated") $ do
      refreshed<-invoke "Evaluate watch" (row "record" finished) finished >>= await "explicit parent refresh" (has "record = Record")
      _<-expand "record" refreshed >>= await "fresh parent children" (has "nested =")
      pure ()
    when (scenario=="child-policy") $ do
      private<-await "private child outcome" (has "Private watch") finished
      publicAgain<-foldM (\d _->tick d) private {guestPrivatePaths=[]} [1..20::Int]
      check "child outcome privacy remains sticky" (has "Private watch" publicAgain && not (has "record" publicAgain))
  else if scenario=="normal" then do
    result<-await "scalar watch reply" (has "counter + 1 = 42") evaluated
    sent<-executing
    check "exact captured selected frame and watch context reach DAP" (length sent==1 && case sent of [request]->((field "arguments" request :: Maybe Value) >>= field "frameId")==Just (11::Int); _->False)
    rejectInspect "evaluate" [] result
    lazyWatch<-add "lazy" result
    lazyRequested<-invoke "Evaluate watch" (row "lazy" lazyWatch) lazyWatch
    lazyResult<-await "lazy watch reply" (has "lazy = <thunk>") lazyRequested
    let lazyRow=row "lazy" lazyResult
    check "lazy watch has no passive branch" (not (P.infoBranch (rowInfo lazyRow)) && any ((=="Force lazy watch").fst) (rowActions lazyRow))
    rejectInspect "variables" ["variablesReference" .= (970::Int)] lazyResult
    (selected,values)<-entries
    let [(key,entry)]=[(key,value) | (key,value)<-M.toList values,watchExpression value=="lazy"]
        receipt=maybe (error "missing stopped watch frame") id selected
    (_,guessed)<-core lazyResult [DebugSidebarAction (ForceDebugWatch key (watchRevision entry) receipt 971)]
    check "guessed lazy references are refused" ("expired" `T.isInfixOf` status guessed)
    forced<-invoke "Force lazy watch" (row "lazy" guessed) guessed >>= await "forced watch result" (has "Forced; expand")
    (_,oldForce)<-core forced [DebugSidebarAction (ForceDebugWatch key (watchRevision entry) receipt 970)]
    check "previous stopped lazy receipt cannot force twice" ("expired" `T.isInfixOf` status oldForce)
    children<-expand "lazy" oldForce >>= await "forced bounded cached children" (has "counter = 42")
    check "nested lazy values remain inert" (all (not.P.infoBranch.rowInfo) [value | value<-rows children,"nested =" `T.isPrefixOf` P.infoLabel (rowInfo value)])
    rejectInspect "variables" ["variablesReference" .= (970::Int)] children
    record<-add "record" children
    loaded<-invoke "Evaluate watch" (row "record" record) record >>= await "nonlazy watch result" (has "record = Record")
    expanded<-expand "record" loaded >>= await "ordinary known child page" (has "counter = 42")
    bad<-add "unsupported" expanded
    failed<-invoke "Evaluate watch" (row "unsupported" bad) bad >>= await "unsupported evaluation error" (has "error: fixture refused")
    count<-length <$> executing
    idle<-foldM (\d _->tick d) failed [1..30::Int]
    check "unsupported evaluation is local and never retried" . (==count) . length =<< executing
    oversized<-add "oversized" idle
    bounded<-invoke "Evaluate watch" (row "oversized" oversized) oversized >>= await "bounded watch error" (\d->any (\title->"oversized" `T.isPrefixOf` title && "error:" `T.isInfixOf` title) (labels d))
    check "watch result labels remain bounded" (all ((<=256).T.length) (labels bounded))
  else do
    -- A deterministic peer holds the executing reply until another request.
    pending<-await "pending watch receipt" (has "[evaluating") evaluated
    (selected,values)<-entries
    let [(key,entry)]=M.toList values
        receipt=maybe (error "missing pending receipt") id selected
    (_,busy)<-core pending [DebugSidebarAction (EvaluateDebugWatch key (watchRevision entry) receipt)]
    check "one executing request owns the slot" ("busy" `T.isInfixOf` status busy)
    changed<-case scenario of
      "edit"->snd <$> core pending [DebugSidebarAction (EditDebugWatch key (watchRevision entry))] >>= save "replacement"
      "remove"->snd <$> core pending [DebugSidebarAction (RemoveDebugWatch key (watchRevision entry))]
      "resume"->snd <$> core pending [DebugAction "continue" []]
      "frame"->snd <$> core pending [DebugAction "stack" []] >>= await "frame chooser" ((/=Nothing).dialog) >>= \d->case dialog d of
        Just dg->let chosen=dg {fields=map (\field->case field of ListBox title values _->ListBox title values 1; _->field) (fields dg)}
                     (next,outbox)=submitDialog 0 chosen d
                 in snd <$> core next outbox
        _->fail "missing frame chooser"
      "policy"->pure pending {guestPrivatePaths=[origin]}
      "retire"->retireTreeFromHost host (case receipt of WatchFrame owner _ _ _ _->owner) pending
      _->pure pending {dialog=Just (Dialog "modal" (DebuggerWatchDialog 999 Nothing False) [Input "Expression" "" 0] 0 ["Cancel"] [])}
    settled<-if scenario=="resume" then foldM (\d _->threadDelay 1000 >> tick d) changed [1..80::Int] else release changed
    (_,after)<-entries
    if scenario=="policy" then do
      check "canonical frame origin is rechecked before publication" (any (\entry->watchPrivate entry && case watchValue entry of WatchResult _ _ _ _ resultOrigin->resultOrigin==Just origin; _->False) (M.elems after) && has "Private watch" settled && not (has "STALE" settled))
      publicAgain<-foldM (\d _->tick d) settled {guestPrivatePaths=[]} [1..20::Int]
      check "captured result privacy remains sticky" (has "Private watch" publicAgain && not (has "delay" publicAgain))
    else check ("late watch reply refused after "<>scenario) (not (has "STALE" settled) && all (\entry->case watchValue entry of WatchResult{}->False; _->True) (M.elems after))
    if scenario=="retire" then do
      before<-length <$> executing
      (_,refused)<-core settled [DebugSidebarAction (EvaluateDebugWatch key (watchRevision entry) receipt)]
      afterCount<-length <$> executing
      check "retired provider also refuses queued execution admission" (before==afterCount && "expired" `T.isInfixOf` status refused)
    else pure ()
  pure ()

cleanup :: (String,FilePath,ProcessHandle) -> IO ()
cleanup fixture@(_,path,_)=do
  Fixture.cleanup fixture
  exists<-doesFileExist (path<>".release")
  when exists (removeFile (path<>".release"))

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.:key))
check :: String -> Bool -> IO ()
check label condition=unless condition (fail label)
