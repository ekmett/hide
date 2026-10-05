{-# LANGUAGE OverloadedStrings #-}
module DebuggerWatchesCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless,foldM,when)
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
checks=mapM_ session ["normal","edit","remove","resume","frame","retire","modal","policy"] >> putStrLn "Debugger watch execution checks passed"

session :: String -> IO ()
session scenario=bracket (Fixture.fixture (if scenario=="policy" then "watches-private" else "watches")) cleanup $ \(port,path,_)->withSidebarCommands $ \host->withDebugger $ \runtime->withDebuggerSidebar host runtime $ \provider->do
  origin<-canonicalizePath (path<>".hs")
  putStrLn ("watch scenario "<>scenario)
  let fallback d _=pure (False,d)
      core=debuggerEffects runtime fallback
      effects=sidebarEffects host core
      tick d=tickDebugger runtime d >>= tickDebuggerSidebar provider host runtime >>= tickSidebar host core
      rows d=maybe [] (M.elems.treeRows) (sideTree d)
      labels=map (P.infoLabel.rowInfo).rows
      has value=any (T.isInfixOf value).labels
      await label predicate d=timeout 5000000 (loop d) >>= maybe (fail (label<>" timed out")) pure
        where
          loop current=do
            next<-tick current
            if predicate next then pure next else do
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
      release d=do
        writeFile (path<>".release") "release"
        (_,current)<-debuggerTool runtime d "debug_status" (object [])
        value<-current >>= either (fail.T.unpack) pure
        (_,_)<-debuggerTool runtime d "debug_inspect" (object ["generation" .= maybe (0::Int) id (field "generation" value),"request" .= ("threads"::T.Text)])
        foldM (\desktop _->threadDelay 1000 >> tick desktop) d [1..80::Int]
  mounted<-initializeSidebar host (initialDesktop (90,30)) {sideTree=Just (emptySidebar (takeDirectory path) 28 False)} >>= await "Watches provider" (has "Watches")
  (_,attached)<-core mounted [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
  stopped<-await "stopped selected frame" (T.isPrefixOf "Stopped in ".status) attached
  empty<-expand "Watches" stopped >>= await "empty watches" (has "No watches")
  created<-add (if scenario=="normal" then "counter + 1" else "delay") empty
  quiet<-foldM (\d _->tick d) created [1..20::Int]
  check "passive stop/tree/tick never evaluates" . null =<< executing
  let initialRow=row (if scenario=="normal" then "counter + 1" else "delay") quiet
      agentRef=case lookup "Evaluate watch" (rowActions initialRow) of Just (P.RegisteredAction ref)->ref; _->error "missing evaluate action"
      agentTrace=maybe [] (hitTrace (keyOf (rowHit initialRow))) (sideTree quiet)
  (_,agentRejected)<-effects quiet [InvokeTree agentTrace agentRef Menu.AgentMenu]
  check "agent origin does not gain executing sidebar authority" ("stale, protected or busy" `T.isInfixOf` status agentRejected)
  check "agent attempt did not evaluate" . null =<< executing
  evaluated<-invoke "Evaluate watch" (row (if scenario=="normal" then "counter + 1" else "delay") quiet) quiet >>= await "explicit request" (T.isPrefixOf "Evaluating watch".status)
  if scenario=="normal" then do
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
