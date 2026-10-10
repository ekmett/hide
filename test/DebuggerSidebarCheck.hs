-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : DebuggerSidebarCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

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
import Hide.Buffer (Selection(..),newBuffer)
import Hide.Files (FileState(..))
import Hide.Plugin.BufferHost (captureVersion)
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
checks=watchManagement >> session "sidebar" 11 >> session "sidebar" 12 >> session "sidebar-exit" 11 >> putStrLn "Debugger sidebar checks passed"

-- The persistent provider uses the same captured command route as stopped frames.
-- A watch catalogue can be managed without a live adapter, and never evaluates.
watchManagement :: IO ()
watchManagement=withSidebarCommands $ \host->withDebugger $ \runtime->withDebuggerSidebar host runtime $ \provider->do
  let fallback d _=pure (False,d)
      core=debuggerEffects runtime fallback
      effects=sidebarEffects host core
      tick d=tickDebugger runtime d >>= tickDebuggerSidebar provider host runtime >>= tickSidebar host core
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
  (revision,_,entries)<-debuggerWatches runtime
  let (ident,entry)=case M.toList entries of [watch]->watch; _->error "expected one sidebar debugger watch"
      selected=row "counter + 1" added
      stale=command "Remove watch" selected added
  check "watch has monotonic identity and bounded owner metadata" (ident>0 && revision>0 && watchExpression entry=="counter + 1" && watchRevision entry==0)
  editing<-invoke (TreeCommand (rowHit selected:drop 1 (maybe [] (hitTrace (keyOf (rowHit selected))) (sideTree added))) (maybe (error "missing watch edit") id (rowCommand selected))) added >>= wait "Edit watch dialog" ((/=Nothing).dialog)
  check "watch editor is protected human input" (guestModalBlocked editing && case fields <$> dialog editing of Just [SelectedInput _ text _]->text=="counter + 1"; _->False)
  updated<-save "counter + 2" editing >>= wait "updated watch row" (has "counter + 2")
  (_,_,edited)<-debuggerWatches runtime
  check "edit retains watch identity and increments its revision" (M.keys edited==[ident] && maybe False ((==1).watchRevision) (M.lookup ident edited))
  check "previous watch row receipt is expired" (case stale of TreeCommand trace _->maybe False (not . hitCurrent trace) (sideTree updated); _->False)
  staleAction<-invoke stale updated
  (_,_,afterStale)<-debuggerWatches runtime
  check "previous watch row cannot remove edited expression" (M.member ident afterStale)
  removed<-action "Remove watch" (row "counter + 2" staleAction) staleAction >>= wait "removed watch row" (has "No watches")
  again<-add "second" removed >>= wait "second watch row" (has "second")
  (_,_,newEntries)<-debuggerWatches runtime
  check "removed watch IDs are never reused" (M.keys newEntries/= [ident])
  invalid<-add (T.replicate 4097 "x") again
  (_,_,afterInvalid)<-debuggerWatches runtime
  check "oversized expression is refused rather than truncated" (M.size afterInvalid==1 && "1–4096" `T.isInfixOf` status invalid)
  valid<-add (T.replicate 4096 "v") invalid >>= wait "maximum-length watch row" (has (T.replicate 128 "v"))
  (_,_,boundedEntries)<-debuggerWatches runtime
  check "maximum-length expression remains exact while its display is bounded" (any ((==4096).T.length.watchExpression) (M.elems boundedEntries) && all ((<=256).T.length) (labels valid))
  let private=valid {dialog=Just (Dialog "Edit watch" (DebuggerWatchDialog 99 (Just "/private/watch.hs") True)
        [SelectedInput "Expression" "private-expression" (Selection 0 18)] 0 ["Save","Cancel"] []),guestPrivatePaths=["/private"]}
      rect=case dialog private of Just dg->one "watch field rectangle" (fieldRects private dg); _->error "missing private watch"
  check "watch privacy survives its captured expression and canonical origin" (not (readableAt private (left rect+1) (top rect+1)) && not (streamerReadableAt private (left rect+1) (top rect+1)))
  let privateSource=addDocument (Just (FileState "/private/watch.hs" Nothing)) (newBuffer "private-source-expression") valid
        {guestPrivatePaths=["/private"],sideTree=fmap (\tree->tree {treeFocused=False}) (sideTree valid)}
      window=maybe (error "private source window missing") id (activeWindow privateSource)
      doc=maybe (error "private source document missing") id (activeDocument privateSource)
      bid=maybe (error "private source ID missing") id (bufferId window)
  version<-captureVersion (documentBuffer doc)
  (_,sourceEditor)<-core privateSource [DebugSourceAction (DebugSourceRequest AddSourceWatch (windowId window) bid version (selection window)
    (Just "/private/watch.hs") (Just "/private/watch.hs") 1 (Just "private-source-expression") False)]
  storedPrivate<-save "private-source-expression" sourceEditor
  protected<-wait "private watch row" (has "Private watch") storedPrivate {guestPrivatePaths=[]}
  (_,_,privateEntries)<-debuggerWatches runtime
  check "captured source privacy remains decisive after origin becomes public" (all (not.T.isInfixOf "private-source-expression") (labels protected) && any (\watch->watchPrivate watch && watchOrigin watch==Just "/private/watch.hs") (M.elems privateEntries))
  _<-foldM (\d _->tick d) protected [1..20::Int]
  check "management does not create a stopped session or evaluation handles" . (==Nothing) =<< debuggerSidebarEpoch runtime

session :: String -> Int -> IO ()
session mode firstFrame=bracket (Fixture.fixture mode) Fixture.cleanup $ \(port,logPath,_)->
  withSidebarCommands $ \host->withDebugger $ \runtime->withDebuggerSidebar host runtime $ \provider->do
    let fallback d _=pure (False,d)
        core=debuggerEffects runtime fallback
        effects=sidebarEffects host core
        tick d=tickDebugger runtime d >>= tickDebuggerSidebar provider host runtime >>= tickSidebar host core
        waitIO label predicate d=timeout 5000000 (loop d) >>= maybe (error (label<>" timed out")) pure
          where loop current=do next<-tick current; done<-predicate next; if done then pure next else threadDelay 1000 >> loop next
        wait label predicate=waitIO label (pure . predicate)
        rows d=maybe [] (M.elems . treeRows) (sideTree d)
        labels=map (P.infoLabel . rowInfo) . rows
        has text=any (T.isInfixOf text) . labels
        expand title d=case [i | (i,row)<-zip [0..] (rows d),title==P.infoLabel (rowInfo row)] of
          i:_->let (next,outbox)=activateTree True i d {sideTree=fmap (\tree->tree {treeFocused=True}) (sideTree d)} in snd <$> effects next outbox
          _->error ("missing row "<>T.unpack title)
        records=do values<-T.lines <$> TIO.readFile logPath
                   pure [body | value<-values,Just body<-[decodeReplyText value]] :: IO [Value]
        requests=do values<-records; pure [request | body<-values,Just request<-[field "request" body]]
        reversedReplies command argument targets=do
          values<-records
          let sent=[request | body<-values,Just request<-[field "request" body],field "command" request==Just command,
                    (field "arguments" request >>= field argument) `elem` map Just targets]
              received=[field "arguments" request >>= field argument | request<-sent]
              releases=[release | body<-values,Just release<-[field "sidebarRelease" body]]
          pure $ case traverse (field "seq") sent :: Maybe [Int] of
            Just sequences | received==map Just targets->any (\release->field "command" release==Just command &&
              field "targets" release==Just (reverse targets) && field "requestSeqs" release==Just (reverse sequences)) releases
            _->False
        requested command argument target=any (\request->field "command" request==Just command &&
          (field "arguments" request >>= field argument)==Just target) <$> requests
        commands=map (maybe "" id . (field "command" :: Value -> Maybe T.Text)) <$> requests
        secondFrame=if firstFrame==11 then 12 else 11
        frameTitle :: Int -> T.Text
        frameTitle fid=if fid==11 then "entry λ  :2" else "sibling frame  :1"
        localsTitle fid="Locals "<>T.pack (show fid)
        below fid title d=case (sideTree d,[row | row<-rows d,P.infoLabel (rowInfo row)==title]) of
          (Just tree,[row])->let parent=one "owning frame" [value | value<-rows d,P.infoLabel (rowInfo value)==frameTitle fid]
                            in keyOf (rowHit parent) `elem` map keyOf (hitTrace (keyOf (rowHit row)) tree)
          _->False
        readPage d request=withAsync (debuggerSidebarRead runtime request) $ \worker->do
          next<-waitIO "sidebar page reply" (\_->maybe False (const True) <$> poll worker) d
          result<-Async.wait worker
          pure (next,result)
        debuggerState d=do (_,finish)<-debuggerTool runtime d "debug_status" (object []); finish
        disconnected d=either (const False) ((==Just False) . (field "active" :: Value -> Maybe Bool)) <$> debuggerState d
        selected d=do value<-debuggerState d
                      pure (value >>= maybe (Left "no frame") Right . (field "frame" :: Value -> Maybe Value) >>= maybe (Left "no frame ID") Right . (field "id" :: Value -> Maybe Int))
        decodeReplyText=decodeStrict' . Data.Text.Encoding.encodeUtf8
    initial<-initializeSidebar host (initialDesktop (80,25)) {sideTree=Just (emptySidebar (takeDirectory logPath) 28 False)}
    mounted<-wait "Debug provider" (has "Debug") initial
    (_,prompted)<-core mounted [DebugSidebarAction AddDebugWatch]
    dg<-maybe (error "missing persistent watch prompt") pure (dialog prompted)
    let filled=dg {fields=[SelectedInput "Expression" "persistent" (Selection 0 10)]}
        (accepted,watchEffects)=submitDialog 0 filled prompted {dialog=Just filled}
    (_,withWatch)<-core accepted watchEffects
    (_,connected)<-core withWatch [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
    stopped<-waitIO "initial stopped source publication" (\d->do
      epoch<-debuggerSidebarEpoch runtime
      pure (epoch/=Nothing && (activeDocument d >>= documentLabel)==Just "Source Generated.hs [9]")) connected
    (_,_,retainedWatches)<-debuggerWatches runtime
    check "expressions survive session initialization without evaluation" (map watchExpression (M.elems retainedWatches)==["persistent"])
    root<-expand "Debug" stopped >>= wait "threads" (has "main λ")
    if mode=="sidebar-exit" then do
      worker<-expand "worker" root >>= wait "worker stack" (has "worker frame")
      requesting<-expand "worker frame  :2" worker
      expired<-wait "exited thread expires tree" (\d->not (has "worker" d) && not (has "STALE" d)) requesting
      check "exit preserves stopped state" . (/=Nothing) =<< debuggerSidebarEpoch runtime
      (_,closing)<-core expired [DebugAction "disconnect" []]
      _<-waitIO "disconnected debugger owner" disconnected closing
      pure ()
    else do
      stack<-expand "main λ" root >>= wait "stack" (has "sibling frame")
      first<-expand (frameTitle firstFrame) stack
      -- Choose both arrival orders explicitly. The fixture holds either first
      -- request until its pair arrives, then releases replies in reverse order.
      firstPending<-waitIO "first frame scopes request" (\_->requested ("scopes"::T.Text) "frameId" firstFrame) first
      second<-expand (frameTitle secondFrame) firstPending
      prepared<-waitIO "interleaved frame scopes" (\d->do
        reordered<-reversedReplies ("scopes"::T.Text) "frameId" [firstFrame,secondFrame]
        pure (reordered && has "Locals 11" d && has "Locals 12" d &&
          maybe False (\tree->treeProjectionRevision tree==treeRevision tree) (sideTree d))) second
      check "reordered scopes retain their owning frame" (below 11 "Locals 11" prepared && below 12 "Locals 12" prepared)
      let frameRow=one "frame row" [row | row<-rows prepared,frameTitle secondFrame==P.infoLabel (rowInfo row)]
          trace=maybe [] (hitTrace (keyOf (rowHit frameRow))) (sideTree prepared)
          reference=one "frame action" [ref | (_,P.RegisteredAction ref)<-rowActions frameRow]
          captured=TreeCommand (rowHit frameRow:drop 1 trace) reference
          (_,activation)=runCommand captured prepared
      firstLocals<-expand (localsTitle firstFrame) prepared
      pendingFirst<-waitIO "first frame locals request" (\_->requested ("variables"::T.Text) "variablesReference" (200+firstFrame)) firstLocals
      (_,queued)<-effects pendingFirst activation
      navigated<-waitIO "captured frame activation" (fmap (==Right secondFrame) . selected) queued
      scopes<-wait "retained sibling scopes" (\d->has "Locals 11" d && has "Locals 12" d) navigated
      two<-expand (localsTitle secondFrame) scopes
      locals<-waitIO "both frame locals" (\d->do
        reordered<-reversedReplies ("variables"::T.Text) "variablesReference" [200+firstFrame,200+secondFrame]
        pure (reordered && has "counter211" d && has "counter212" d &&
          maybe False (\tree->treeProjectionRevision tree==treeRevision tree) (sideTree d))) two
      check "reordered locals retain their owning frame" (below 11 "counter211 = 211" locals && below 12 "counter212 = 212" locals)
      let moreRows title d=[i | (i,row)<-zip [0::Int ..] (rows d),LoadNext{}<-[rowAction row],
                    rowHit row==rowHit (one "locals root" [r | r<-rows d,P.infoLabel (rowInfo r)==title])]
          more title d=case moreRows title d of
            [i]->let (next,outbox)=activateTree True i d in snd <$> effects next outbox
            _->error ("missing locals More: "<>T.unpack title)
          count title=length.filter (T.isPrefixOf title.P.infoLabel.rowInfo).rows
      check "both frame locals expose first128 rows and continuation" (count "local211_" locals==125 && count "local212_" locals==125 && length (moreRows "Locals 11" locals)==1 && length (moreRows "Locals 12" locals)==1)
      epoch0<-maybe (error "missing stopped epoch") pure =<< debuggerSidebarEpoch runtime
      (unpublished,denied)<-readPage locals (DebugPageRequest epoch0 (DebugVariables 7 11 1211) 0)
      check "retained snapshot does not grant unpublished local references" (case denied of Left _->True; _->False)
      secondPage<-more "Locals 11" unpublished >>= wait "second locals page" (has "local211_255")
      check "second locals page adds128 rows" (count "local211_" secondPage==253 && not (has "local211_256" secondPage))
      let retainedMore=activateTree True (one "retained locals More" (moreRows "Locals 11" secondPage)) secondPage
      lastPage<-more "Locals 11" secondPage >>= wait "last locals page" (\d->has "local211_259" d && null (moreRows "Locals 11" d))
      check "last locals page adds4 rows; sibling frame remains expanded" (count "local211_" lastPage==257 && count "local212_" lastPage==125)
      inspected<-expand "local211_129 = expand" lastPage >>= wait "published later local expands" (has "child211 = later page")
      sent<-requests
      check "locals continuation reuses one unpaged response per frame" (all (\ref->[field "arguments" r :: Maybe Value | r<-sent,field "command" r==Just ("variables"::T.Text),(field "arguments" r >>= field "variablesReference")==Just ref]==[Just (object ["variablesReference" .= ref])]) [211::Int,212])
      check "delayed locals and paging do not change selected frame" . (==Right secondFrame) =<< selected inspected
      before<-commands
      _<-foldM (\d _->snapshot d `seq` tick d) inspected [1..20::Int]
      after<-commands
      check "cached render/ticks do not issue DAP reads" (before==after)
      let lazyRows=[row | row<-rows locals,"lazy =" `T.isPrefixOf` P.infoLabel (rowInfo row)]
      check "lazy values have no passive expansion or action" (not (null lazyRows) && all (\row->not (P.infoBranch (rowInfo row)) && rowCommand row==Nothing) lazyRows)
      epoch<-maybe (error "missing stopped epoch") pure =<< debuggerSidebarEpoch runtime
      (replayed,_)<-readPage inspected (DebugPageRequest epoch (DebugScopes 7 11) 0)
      (checked,lazyReply)<-readPage replayed (DebugPageRequest epoch (DebugVariables 7 11 900) 0)
      check "cached scope replay cannot weaken lazy reference policy" (case lazyReply of Left _->True; Right _->False)
      waiting<-expand "waiting = expand to wait" checked
      pending<-waitIO "pending variable read" (\_->any (\request->(field "arguments" request >>= field "variablesReference")==Just (910::Int)) <$> requests) waiting
      beforeResume<-length . filter (=="threads") <$> commands
      (_,resuming)<-core pending [DebugAction "continue" []]
      drained<-waitIO "resume consumes late locals and expires handles" (\d->do
        resumedEpoch<-debuggerSidebarEpoch runtime
        threads<-length . filter (=="threads") <$> commands
        -- The adapter sends thread-started after the delayed locals reply.
        -- Its resulting threads request proves those events reached the owner.
        pure (threads>beforeResume && resumedEpoch==Nothing && not (has "counter211" d) &&
          maybe False (\tree->treeProjectionRevision tree==treeRevision tree) (sideTree d))) resuming
      (_,oldPage)<-effects drained (snd retainedMore)
      check "retained continuation cannot restore resumed locals" (not (has "local211_" oldPage))
      let (late,oldAction)=runCommand captured oldPage
      (_,refused)<-effects late oldAction
      check "previous-stop frame action cannot select a source" (null oldAction || "stale" `T.isInfixOf` T.toLower (status refused) || "expired" `T.isInfixOf` T.toLower (status refused))
      check "late previous-stop locals cannot revive" (not (has "STALE" drained) && not (has "counter211" drained))
      check "inspection never evaluates lazy targets" . not . any (`elem` ["evaluate","setVariable"]) =<< commands
      (_,closing)<-core drained [DebugAction "disconnect" []]
      _<-waitIO "disconnected debugger owner" disconnected closing
      pure ()

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.:key))
one :: String -> [a] -> a
one label values=case values of value:_->value; _->error ("missing "<>label)

check :: String -> Bool -> IO ()
check name passed=unless passed (error name)
