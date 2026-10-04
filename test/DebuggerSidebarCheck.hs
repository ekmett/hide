{-# LANGUAGE OverloadedStrings #-}
module DebuggerSidebarCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync,poll)
import qualified Control.Concurrent.Async as Async
import Control.Exception (bracket)
import Control.Monad (unless,foldM)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding
import System.FilePath (takeDirectory)
import System.Timeout (timeout)
import qualified DebuggerCheck as Fixture
import Hide.Buffer (Selection(..))
import Hide.Debugger
import Hide.DebuggerSidebar
import Hide.DebuggerSidebarTypes
import Hide.Model
import Hide.Sidebar
import Hide.SidebarCommands
import Hide.Render (snapshot)
import Hide.GuestAccess (guestModalBlocked,readableAt,streamerReadableAt)
import qualified Hide.Plugin.Tree as P
import Data.Aeson
import Data.Aeson.Types (parseMaybe)

checks :: IO ()
checks=watchManagement >> mapM_ session ["sidebar","sidebar-exit"] >> putStrLn "Debugger sidebar checks passed"

-- The persistent provider uses the same captured command route as stopped frames.
-- A watch catalogue can be managed without a live adapter, and never evaluates.
watchManagement :: IO ()
watchManagement=withSidebarCommands $ \host->withDebugger $ \runtime->withDebuggerSidebar host runtime $ \provider->do
  let fallback d _=pure (False,d)
      core=debuggerEffects runtime fallback
      effects=sidebarEffects host core
      tick d=tickDebugger runtime fallback d >>= tickDebuggerSidebar provider host runtime >>= tickSidebar host core
      rows d=maybe [] (M.elems . treeRows) (sideTree d)
      labels=map (P.infoLabel . rowInfo) . rows
      has text=any (T.isInfixOf text) . labels
      wait label predicate d=timeout 5000000 (loop d) >>= maybe (error (label<>" timed out")) pure
        where loop current=do next<-tick current; if predicate next then pure next else threadDelay 1000 >> loop next
      row title d=one (T.unpack title) [value | value<-rows d,title `T.isPrefixOf` P.infoLabel (rowInfo value)]
      command title value d=case lookup title (rowActions value) of
        Just (P.RegisteredAction ref)->TreeCommand (rowHit value:drop 1 (maybe [] (hitTrace (keyOf (rowHit value))) (sideTree d))) ref
        _->error ("missing captured watch action "<>T.unpack title)
      invoke cmd d=let (next,outbox)=runCommand cmd d in snd <$> effects next outbox
      action title value d=invoke (command title value d) d
      save text d=case dialog d of
        Just dg->let updated=dg {fields=[SelectedInput "Expression" text (Selection 0 (T.length text))]}
                     (next,outbox)=submitDialog 0 updated d {dialog=Just updated}
                 in snd <$> core next outbox
        _->error "missing watch editor"
      add text d=action "Add watch…" (row "Watches" d) d >>= wait "Add watch dialog" ((/=Nothing).dialog) >>= save text
      expand d=case [index | (index,value)<-zip [0..] (rows d),P.infoLabel (rowInfo value)=="Watches"] of
        index:_->let (next,outbox)=activateTree True index d {sideTree=fmap (\tree->tree {treeFocused=True}) (sideTree d)} in snd <$> effects next outbox
        _->error "missing persistent Watches root"
  initial<-initializeSidebar host (initialDesktop (80,25)) {sideTree=Just (emptySidebar "/tmp" 28 False)}
  mounted<-wait "persistent Watches provider" (has "Watches") initial
  empty<-expand mounted >>= wait "empty Watches" (has "No watches")
  added<-add "counter + 1" empty >>= wait "added watch row" (has "counter + 1")
  (revision,entries)<-debuggerWatches runtime
  let [(ident,entry)]=M.toList entries
      selected=row "counter + 1" added
      stale=command "Remove watch" selected added
  check "watch has monotonic identity and bounded owner metadata" (ident>0 && revision>0 && watchExpression entry=="counter + 1" && watchRevision entry==0)
  editing<-invoke (TreeCommand (rowHit selected:drop 1 (maybe [] (hitTrace (keyOf (rowHit selected))) (sideTree added))) (maybe (error "missing watch edit") id (rowCommand selected))) added >>= wait "Edit watch dialog" ((/=Nothing).dialog)
  check "watch editor is protected human input" (guestModalBlocked editing && case fields <$> dialog editing of Just [SelectedInput _ text _]->text=="counter + 1"; _->False)
  updated<-save "counter + 2" editing >>= wait "updated watch row" (has "counter + 2")
  (_,edited)<-debuggerWatches runtime
  check "edit retains watch identity and increments its revision" (M.keys edited==[ident] && maybe False ((==1).watchRevision) (M.lookup ident edited))
  staleAction<-invoke stale updated >>= wait "stale captured action refusal" ((/="Running sidebar action…").status)
  (_,afterStale)<-debuggerWatches runtime
  check "previous watch row cannot remove edited expression" (M.member ident afterStale)
  removed<-action "Remove watch" (row "counter + 2" staleAction) staleAction >>= wait "removed watch row" (has "No watches")
  again<-add "second" removed >>= wait "second watch row" (has "second")
  (_,newEntries)<-debuggerWatches runtime
  check "removed watch IDs are never reused" (M.keys newEntries/= [ident])
  invalid<-add (T.replicate 4097 "x") again
  (_,afterInvalid)<-debuggerWatches runtime
  check "oversized expression is refused rather than truncated" (M.size afterInvalid==1 && "1–4096" `T.isInfixOf` status invalid)
  valid<-add (T.replicate 4096 "v") invalid >>= wait "maximum-length watch row" (has (T.replicate 128 "v"))
  (_,boundedEntries)<-debuggerWatches runtime
  check "maximum-length expression remains exact while its display is bounded" (any ((==4096).T.length.watchExpression) (M.elems boundedEntries) && all ((<=256).T.length) (labels valid))
  let private=valid {dialog=Just (Dialog "Edit watch" (DebuggerWatchDialog 99 (Just "/private/watch.hs") True)
        [SelectedInput "Expression" "private-expression" (Selection 0 18)] 0 ["Save","Cancel"] []),guestPrivatePaths=["/private"]}
      rect=case dialog private of Just dg->one "watch field rectangle" (fieldRects private dg); _->error "missing private watch"
  check "watch privacy survives its captured expression and canonical origin" (not (readableAt private (left rect+1) (top rect+1)) && not (streamerReadableAt private (left rect+1) (top rect+1)))
  _<-foldM (\d _->tick d) valid [1..20::Int]
  check "management does not create a stopped session or evaluation handles" . (==Nothing) =<< debuggerSidebarEpoch runtime

session :: String -> IO ()
session mode=bracket (Fixture.fixture mode) Fixture.cleanup $ \(port,logPath,_)->
  withSidebarCommands $ \host->withDebugger $ \runtime->withDebuggerSidebar host runtime $ \provider->do
    let fallback d _=pure (False,d)
        core=debuggerEffects runtime fallback
        effects=sidebarEffects host core
        tick d=tickDebugger runtime fallback d >>= tickDebuggerSidebar provider host runtime >>= tickSidebar host core
        waitIO label predicate d=timeout 5000000 (loop d) >>= maybe (error (label<>" timed out")) pure
          where loop current=do next<-tick current; done<-predicate next; if done then pure next else threadDelay 1000 >> loop next
        wait label predicate=waitIO label (pure . predicate)
        rows d=maybe [] (M.elems . treeRows) (sideTree d)
        labels=map (P.infoLabel . rowInfo) . rows
        has text=any (T.isInfixOf text) . labels
        expand title d=case [i | (i,row)<-zip [0..] (rows d),title==P.infoLabel (rowInfo row)] of
          i:_->let (next,outbox)=activateTree True i d {sideTree=fmap (\tree->tree {treeFocused=True}) (sideTree d)} in snd <$> effects next outbox
          _->error ("missing row "<>T.unpack title)
        requests=do values<-T.lines <$> TIO.readFile logPath
                    pure [request | value<-values,Just body<-[decodeStrictText value],Just request<-[field "request" body]] :: IO [Value]
        commands=map (maybe "" id . (field "command" :: Value -> Maybe T.Text)) <$> requests
        readPage d request=withAsync (debuggerSidebarRead runtime request) $ \worker->do
          next<-waitIO "sidebar page reply" (\_->maybe False (const True) <$> poll worker) d
          result<-Async.wait worker
          pure (next,result)
        selected d=do (_,finish)<-debuggerTool runtime fallback d "debug_status" (object []); value<-finish
                      pure (value >>= maybe (Left "no frame") Right . (field "frame" :: Value -> Maybe Value) >>= maybe (Left "no frame ID") Right . (field "id" :: Value -> Maybe Int))
        decodeStrictText=decodeStrict' . Data.Text.Encoding.encodeUtf8
    initial<-initializeSidebar host (initialDesktop (80,25)) {sideTree=Just (emptySidebar (takeDirectory logPath) 28 False)}
    mounted<-wait "Debug provider" (has "Debug") initial
    (_,prompted)<-core mounted [DebugSidebarAction AddDebugWatch]
    dg<-maybe (error "missing persistent watch prompt") pure (dialog prompted)
    let filled=dg {fields=[SelectedInput "Expression" "persistent" (Selection 0 10)]}
        (accepted,watchEffects)=submitDialog 0 filled prompted {dialog=Just filled}
    (_,withWatch)<-core accepted watchEffects
    (_,connected)<-core withWatch [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
    stopped<-wait "stopped debugger" (T.isInfixOf "Stopped in " . status) connected
    (_,retainedWatches)<-debuggerWatches runtime
    check "expressions survive session initialization without evaluation" (map watchExpression (M.elems retainedWatches)==["persistent"])
    root<-expand "Debug" stopped >>= wait "threads" (has "main λ")
    if mode=="sidebar-exit" then do
      worker<-expand "worker" root >>= wait "worker stack" (has "worker frame")
      requesting<-expand "worker frame  :2" worker
      expired<-wait "exited thread expires tree" (\d->not (has "worker" d) && not (has "STALE" d)) requesting
      check "exit preserves stopped state" . (/=Nothing) =<< debuggerSidebarEpoch runtime
      (_,closing)<-core expired [DebugAction "disconnect" []]
      _<-wait "disconnect" (T.isPrefixOf "Debugger disconnected" . status) closing
      pure ()
    else do
      stack<-expand "main λ" root >>= wait "stack" (has "sibling frame")
      first<-expand "entry λ  :2" stack
      second<-expand "sibling frame  :1" first
      prepared<-wait "interleaved frame scopes" (\d->has "Locals 11" d && has "Locals 12" d && maybe False (\tree->treeProjectionRevision tree==treeRevision tree) (sideTree d)) second
      let frameRow=one "frame row" [row | row<-rows prepared,"sibling frame" `T.isPrefixOf` P.infoLabel (rowInfo row)]
          trace=maybe [] (hitTrace (keyOf (rowHit frameRow))) (sideTree prepared)
          reference=one "frame action" [ref | (_,P.RegisteredAction ref)<-rowActions frameRow]
          captured=TreeCommand (rowHit frameRow:drop 1 trace) reference
          (selectedFrame,activation)=runCommand captured prepared
      (_,queued)<-effects selectedFrame activation
      navigated<-waitIO "captured frame activation" (fmap (==Right 12) . selected) queued
      scopes<-wait "interleaved frame scopes" (\d->has "Locals 11" d && has "Locals 12" d) navigated
      one<-expand "Locals 11" scopes
      two<-expand "Locals 12" one
      locals<-wait "both frame locals" (\d->has "counter211" d && has "counter212" d) two
      before<-commands
      _<-foldM (\d _->snapshot d `seq` tick d) locals [1..20::Int]
      after<-commands
      check "cached render/ticks do not issue DAP reads" (before==after)
      let lazyRows=[row | row<-rows locals,"lazy =" `T.isPrefixOf` P.infoLabel (rowInfo row)]
      check "lazy values have no passive expansion or action" (not (null lazyRows) && all (\row->not (P.infoBranch (rowInfo row)) && rowCommand row==Nothing) lazyRows)
      epoch<-maybe (error "missing stopped epoch") pure =<< debuggerSidebarEpoch runtime
      (replayed,_)<-readPage locals (DebugPageRequest epoch (DebugScopes 7 11) 0)
      (checked,lazyReply)<-readPage replayed (DebugPageRequest epoch (DebugVariables 7 11 900) 0)
      check "cached scope replay cannot weaken lazy reference policy" (case lazyReply of Left _->True; Right _->False)
      waiting<-expand "waiting = expand to wait" checked
      pending<-waitIO "pending variable read" (\_->any (\request->(field "arguments" request >>= field "variablesReference")==Just (910::Int)) <$> requests) waiting
      (_,resuming)<-core pending [DebugAction "continue" []]
      resumed<-wait "resume expires handles" (T.isPrefixOf "Running" . status) resuming
      drained<-foldM (\d _->threadDelay 1000 >> tick d) resumed [1..30::Int]
      let (late,oldAction)=runCommand captured drained
      (_,refused)<-effects late oldAction
      check "previous-stop frame action cannot select a source" (null oldAction || "stale" `T.isInfixOf` T.toLower (status refused) || "expired" `T.isInfixOf` T.toLower (status refused))
      check "late previous-stop locals cannot revive" (not (has "STALE" drained) && not (has "counter211" drained))
      check "inspection never evaluates lazy targets" . not . any (`elem` ["evaluate","setVariable"]) =<< commands
      (_,closing)<-core drained [DebugAction "disconnect" []]
      _<-wait "disconnect" (T.isPrefixOf "Debugger disconnected" . status) closing
      pure ()

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.:key))
one :: String -> [a] -> a
one label values=case values of value:_->value; _->error ("missing "<>label)

check :: String -> Bool -> IO ()
check name passed=unless passed (error name)
