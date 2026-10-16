-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications #-}
-- |
-- Module      : Hide.SymbolInspector
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Inspecting one Haskell symbol, for the human and for an agent.
--
-- Both start the same three read-only HLS requests (hover, definition,
-- references) on the existing "Hide.Tooling" workers, with the buffer revision
-- bound into every request. The answers become one 'Report' of
-- "Hide.SymbolInspect"; the human sees its documents on the rows window and the
-- agent tool @inspect_symbol@ receives the same prepared documents as JSON.
--
-- A request is /reserved/ before it starts and only a still-reserved request can
-- commit its report. Each tick revokes reservations and reports whose buffer
-- changed revision, closed or became private, so a late answer is never
-- published and old pages resolve to 'Gone'. Agent pages are opaque random
-- tokens bound to their caller, report and expiry; every use rechecks all of
-- them against the live editor. Cancelling a waiting request cancels its HLS
-- requests through the tooling wait; closing the inspector refuses publication.
module Hide.SymbolInspector
  ( SymbolInspector, withSymbolInspector, withSymbolInspectorClock
  , symbolEffects, tickSymbolInspector
  , symbolTool, symbolToolName, symbolTools
  , liveReports, agentPageCount
  ) where

import Control.Concurrent.STM
import Control.Concurrent.Async (Async, async, cancel, poll, mapConcurrently)
import Control.Exception (SomeException, bracket, evaluate, finally)
import Control.Monad (foldM, forM, forM_, unless, when, void)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as K
import Data.Aeson.Types (Parser, parseEither)
import Data.IORef
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import Hide.Buffer
import Hide.Files (FileState(..))
import Hide.FileIO (withFileRead)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Set as S
import System.IO.Error (tryIOError)
import Hide.GuestAccess (protectedBuffer, protectedFilePath)
import Hide.InspectorWindow
import qualified Hide.LSP as L
import Hide.Model hiding (OpenFile)
import Hide.RemoteEndpoint (randomIdentity)
import Hide.Plugin.Inspect
import Hide.Plugin.Inspect.JSON
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Window as W
import Hide.PluginWindowHost (adoptWindowUpdate)
import Hide.SymbolInspect
import Hide.Tooling (Tooling, toolingTool, humanToolingTool, locations, projectRoot)
import System.Directory (canonicalizePath)
import System.FilePath (addTrailingPathSeparator, takeExtension)
import Data.List (isPrefixOf)

-- | One pending page of an agent's investigation. Its row numbers are local to
-- the exact 'JSONPage' stored here.
data AgentPage = AgentPage
  { pageOwner :: !Owner, pageReport :: !ReportId, pageSubject :: !Subject, pageView :: !ViewId, pageOffset :: !Int
  , pageJSON :: !JSONPage, pageParent :: !(Maybe Text), pageExpires :: !Integer }

data State = State
  { reports :: !(M.Map ReportId Report), generation :: !Integer, nextReport :: !Int
  , reserved :: !(M.Map ReportId (Int,Int))
  , human :: !(Maybe (ReportId,Async (Either Text W.WindowUpdate)))
  , pages :: ![(Text,AgentPage)] }

data PendingReply = PendingReply !ReportId !(Either Text Value) !(TMVar (Either Text Value))

data SymbolInspector = SymbolInspector
  { siTooling :: !Tooling, siInspector :: !Inspector, siState :: !(IORef State)
  , siRoot :: !(Root Reports), siClock :: IO Integer
  , siReplies :: !(TBQueue PendingReply), siClosed :: !(TVar Bool) }

-- Bounds. Nothing here grows with the size of a project or of a session.
maxReports, maxReserved, maxPages, maxSites, maxFiles :: Int
maxReports=32
maxReserved=8
maxPages=64
maxSites=200
maxFiles=24

pageLifetime :: Integer
pageLifetime=15*60*1000000000

-- | Publish the symbol views and the report root for this inspector's lifetime.
withSymbolInspector :: Tooling -> Inspector -> (SymbolInspector -> IO a) -> IO a
withSymbolInspector tooling inspector=withSymbolInspectorClock (toInteger <$> getMonotonicTimeNSec) tooling inspector

-- | As 'withSymbolInspector' with an injectable monotonic nanosecond clock, so
-- tests can expire pages without waiting.
withSymbolInspectorClock :: IO Integer -> Tooling -> Inspector -> (SymbolInspector -> IO a) -> IO a
withSymbolInspectorClock clock tooling inspector use=bracket acquire shutdown (\(si,_)->use si)
  where
    publisher=inspectorPublisher' inspector
    acquire=do
      refs<-publishSymbolViews publisher
      state<-newIORef (State M.empty 0 1 M.empty Nothing [])
      root<-publishRoot publisher (RootDef "hide.symbol.reports" (do
        State found revision _ _ _ _<-readIORef state
        pure (Just (Revision revision,Reports found)))) >>= either (ioError . userError . show) pure
      replies<-newTBQueueIO 64
      closed<-newTVarIO False
      pure (SymbolInspector tooling inspector state root clock replies closed,refs)
    shutdown (si,refs)=do
      atomically $ do
        writeTVar (siClosed si) True
        pending<-flushTBQueue (siReplies si)
        forM_ pending $ \(PendingReply _ _ reply)->void (tryPutTMVar reply (Left "Inspector closed."))
      State _ _ _ _ running _<-readIORef (siState si)
      forM_ running (cancel . snd)
      withdrawRoot publisher (siRoot si)
      mapM_ (withdrawViews publisher) refs

-- | The reports currently retained, for tests and diagnostics.
liveReports :: SymbolInspector -> IO [Report]
liveReports si=M.elems . reports <$> readIORef (siState si)

agentPageCount :: SymbolInspector -> IO Int
agentPageCount si=length . pages <$> readIORef (siState si)

-- Locating a symbol -----------------------------------------------------------

data Located = Located { locBuffer :: !Int, locRevision :: !Int, locPath :: !FilePath, locName :: !Text, locLine :: !Int, locColumn :: !Int }

-- | Resolve a 1-based position in an open, public Haskell buffer to the symbol
-- there. Every refusal is a plain sentence; none reveals buffer contents.
locateAt :: Desktop -> Int -> Maybe Int -> Int -> Int -> Either Text Located
locateAt d bid expected row col=do
  doc<-maybe (Left "Unknown bufferId") Right (M.lookup bid (buffers d))
  when (protectedBuffer d bid) (Left "This buffer is private to the user.")
  let b=documentBuffer doc
  unless (textBuffer b && documentLabel doc==Nothing) (Left "Inspect symbol requires a source text buffer.")
  file<-maybe (Left "Save this buffer with a Haskell filename first.") Right (documentFile doc)
  unless (takeExtension (filePath file) `elem` [".hs",".lhs"]) (Left "Inspect symbol requires a Haskell source file.")
  unless (maybe True (==revision b) expected) (Left "Buffer revision changed.")
  unless (row>=1 && row<=bufferLineCount b) (Left "Line is outside the buffer.")
  line<-maybe (Left "The source line exceeds the 4096-character inspection limit.") Right (boundedLine (bufferContent b) (row-1))
  unless (col>=1 && col<=T.length line+1) (Left "Column is outside the line.")
  (name,start)<-maybe (Left "No symbol at that position.") Right (symbolAt line (col-1))
  pure (Located bid (revision b) (filePath file) (T.take 256 name) row (start+1))

-- | The symbol under the human's caret, or at the start of a selection.
locateCaret :: Desktop -> Either Text Located
locateCaret d=case (activeWindow d,activeDocument d) of
  (Just window,Just doc) | Just bid<-bufferId window->
    let Selection anchor cursor=selection window
        (row,col)=bufferLineColumn (documentBuffer doc) (min anchor cursor)
    in locateAt d bid Nothing (row+1) (col+1)
  _->Left "Open a saved Haskell source file first."

-- What the worker may read, captured immutably under the desktop's serialization.
-- The roots are the editor's launch directory and the inspected file's project:
-- sites elsewhere (library sources, other projects) are counted, never shown.
data Env = Env { envProject :: !FilePath, envRoots :: ![FilePath], envPrivate :: ![FilePath], envOpen :: !(M.Map FilePath BufferContent) }

captureEnv :: Desktop -> FilePath -> IO Env
captureEnv d path=do
  project<-projectRoot path >>= canonicalizePath
  launch<-canonicalizePath (launchDirectory d)
  let open=M.fromList [(filePath f,bufferContent (documentBuffer doc)) | (bid,doc)<-M.toList (buffers d),documentLabel doc==Nothing,textBuffer (documentBuffer doc),
        not (protectedBuffer d bid),Just f<-[documentFile doc]]
  private<-evaluate (privateFilePaths d)
  _<-evaluate (length private)
  pure (Env project (nub [project,launch]) private open)

-- Requests and the report ------------------------------------------------------

noCore :: Desktop -> [Effect] -> IO (Bool,Desktop)
noCore d _=pure (False,d)

-- Start hover, definition and references. Each carries the buffer revision, so a
-- changed or private buffer is refused by the tooling worker as well.
issue :: Bool -> SymbolInspector -> Desktop -> Located -> IO (Desktop,[IO (Either Text Value)])
issue isHuman si d0 loc=foldM one (d0,[]) ["lsp_hover","lsp_definition","lsp_references"]
  where
    arguments=object ["bufferId" .= locBuffer loc,"revision" .= locRevision loc,"line" .= locLine loc,"column" .= locColumn loc]
    start=if isHuman then humanToolingTool else toolingTool
    one (d,waits) name=do
      (next,wait)<-start (siTooling si) noCore d name arguments
      pure (next,waits++[wait])

member :: Text -> Value -> Maybe Value
member key (Object obj)=K.lookup (Key.fromText key) obj
member _ _=Nothing

hoverOutcome :: Either Text Value -> Outcome Text
hoverOutcome (Left err)=Unavailable (T.take 300 err)
hoverOutcome (Right answer)=case member "text" answer of
  Just (String text) | not (T.null (T.strip text))->Available (T.copy (T.take 2000 text))
  _->Empty "HLS has no type information for this symbol."

-- Resolve raw HLS locations to admitted sites. A location is shown only when its
-- canonical path is inside the workspace and not protected; its line must be
-- readable now. Everything else is counted, never described.
siteOutcome :: Env -> Text -> Either Text Value -> IO (Outcome [Site],Int)
siteOutcome _ _ (Left err)=pure (Unavailable (T.take 300 err),0)
siteOutcome env noun (Right answer)=do
  let (raw,rest)=splitAt (maxSites*2) (locations (fromMaybe Null (member "result" answer)))
  canonical<-forM raw $ \(path,row,col16)->do
    file<-either (const Nothing) Just <$> tryIOError (canonicalizePath path)
    pure [(found,row,col16) | Just found<-[file],admitted env found,row>=0,col16>=0]
  let locations'=concat canonical
      wanted=M.fromListWith (++) [(file,[row]) | (file,row,_)<-locations']
  excerpts<-forM (take maxFiles (M.toAscList wanted)) $ \(file,rows)->do
    found<-case M.lookup file (envOpen env) of
      Just content->pure (M.fromList [(row,line) | row<-S.toList (S.fromList rows),Just line<-[boundedLine content row]])
      Nothing->either (const M.empty) id <$> readSourceLines file rows
    pure (file,found)
  let byFile=M.fromList excerpts
      found=nub (sortOn (\site->(sitePath site,siteLine site,siteColumn site))
        [Site file (relativePath (envProject env) file) (row+1) (1+L.positionOffset line (0,col16)) line
        | (file,row,col16)<-locations',Just lines'<-[M.lookup file byFile],Just line<-[M.lookup row lines']])
      kept=take maxSites found
      omitted=length raw-length kept+if null rest then 0 else 1
  pure (if null raw then (Empty ("No "<>noun<>" found."),0)
        else if null kept then (Empty ("No "<>noun<>" is available within the workspace, privacy and excerpt limits."),omitted)
        else (Available kept,omitted))

snippet :: Text -> Text
snippet=T.take 200 . T.strip

admitted :: Env -> FilePath -> Bool
admitted env file=not (protectedFilePath (envPrivate env) file) && any (\root->file==root || addTrailingPathSeparator root `isPrefixOf` file) (envRoots env)

-- Copy just the requested line, with a one-character overflow probe. No whole
-- long line, buffer contents or undo history is retained by an excerpt.
boundedLine :: BufferContent -> Int -> Maybe Text
boundedLine content row
  | row<0 || row>=contentLineCount content=Nothing
  | otherwise=let start=contentLineOffset content row
                  end=contentLineOffset content (row+1)
                  line=T.dropWhileEnd (\c->c=='\r' || c=='\n') (contentSlice content start (min 4098 (end-start)))
              in if T.length line>4096 then Nothing else Just (T.copy line)

-- Read only through the last requested line, up to 1 MiB per file. Unwanted
-- lines are discarded as chunks arrive; only requested complete UTF-8 lines of
-- at most 4096 code points survive. A budget cut never masquerades as EOF.
readSourceLines :: FilePath -> [Int] -> IO (Either Text (M.Map Int Text))
readSourceLines file rows=fmap (either (Left . T.pack . show) Right) $ tryIOError $
  withFileRead file $ \handle->loop handle (S.fromList (filter (>=0) rows)) 1048576 0 [] 0 M.empty
  where
    loop handle wanted remaining row parts count found
      | S.null wanted || remaining<=0=pure found
      | otherwise=do
          chunk<-BS.hGetSome handle (min 4096 remaining)
          if BS.null chunk then pure (finish wanted row parts count found)
            else consume handle wanted (remaining-BS.length chunk) row parts count found chunk
    consume handle wanted remaining row parts count found chunk=do
      let (prefix,suffix)=BS.break (==10) chunk
          nextCount=count+BS.length prefix
          nextParts=if S.member row wanted && nextCount<=16384 then prefix:parts else []
      if BS.null suffix then loop handle wanted remaining row nextParts nextCount found else do
        let nextFound=finish wanted row nextParts nextCount found
            nextWanted=S.delete row wanted
        if S.null nextWanted then pure nextFound
          else consume handle nextWanted remaining (row+1) [] 0 nextFound (BS.tail suffix)
    finish wanted row parts count found
      | not (S.member row wanted) || count>16384=found
      | otherwise=case TE.decodeUtf8' (BS.concat (reverse parts)) of
          Right raw | not (T.any (=='\NUL') raw),let line=T.dropWhileEnd (=='\r') raw,T.length line<=4096->M.insert row (T.copy line) found
          _->found

buildReport :: Env -> Owner -> ReportId -> Located -> [Either Text Value] -> IO Report
buildReport env owner rid loc answers=do
  let (hover,definitions,references)=case answers of
        [a,b,c]->(a,b,c)
        _->(Left "Missing answer.",Left "Missing answer.",Left "Missing answer.")
  (defs,omittedDefs)<-siteOutcome env "definition" definitions
  (refs,omittedRefs)<-siteOutcome env "reference" references
  let report=Report rid owner (locBuffer loc) (locRevision loc) (envProject env) (locPath loc) (locName loc) (locLine loc) (locColumn loc)
        (hoverOutcome hover) defs refs (max omittedDefs omittedRefs)
        (S.toList (S.fromList (locPath loc:[sitePath site | Available sites<-[defs,refs],site<-sites])))
  _<-evaluate (forceReport report)
  pure report

-- Force the bounded report directly, without constructing a serialization.
forceReport :: Report -> Int
forceReport report=sum (map length (reportFiles report))+T.length (reportSymbol report)+outcome T.length (reportHover report)
  +outcome (sum . map site) (reportDefinitions report)+outcome (sum . map site) (reportReferences report)
  where
    outcome f (Available value)=f value
    outcome _ (Empty text)=T.length text
    outcome _ (Unavailable text)=T.length text
    site value=length (sitePath value)+length (siteRelative value)+siteLine value+siteColumn value+T.length (siteText value)

-- State ------------------------------------------------------------------------

-- | Strict atomic transitions shared by UI and reply workers. Never run IO here.
updateState :: SymbolInspector -> (State -> State) -> IO ()
updateState si f=atomicModifyIORef' (siState si) (\old->publishState (f old) ())

-- Force the bounded page spine as well as the record. Otherwise an idle tick's
-- filter can retain the previous filter's tail for the entire page lifetime.
publishState :: State -> a -> (State,a)
publishState state result=length (pages state) `seq` (state,result)

-- A worker prepares its response, then the session owner admits it against the
-- live desktop. Cancellation closes the one-shot reply without blocking a tick.
publishReply :: SymbolInspector -> ReportId -> IO (Either Text Value) -> IO (Either Text Value)
publishReply si rid prepare=do
  value<-prepare
  case value of
    Left _->pure value
    Right _->do
      reply<-newEmptyTMVarIO
      accepted<-atomically $ do
        closed<-readTVar (siClosed si)
        if closed then pure False else writeTBQueue (siReplies si) (PendingReply rid value reply) >> pure True
      if not accepted then pure (Left "Inspector closed.") else
        atomically (takeTMVar reply) `finally` atomically (void (tryPutTMVar reply (Left "Inspection cancelled.")))

-- Paths are bounded by the excerpt budget and were canonicalized by the worker.
-- Cache ownership does not retain authority to disclose a newly private file.
reportCurrent :: Desktop -> Report -> Bool
reportCurrent d report=sourceCurrent d (reportBuffer report) (reportRevision report)
  && maybe False ((==Just (reportPath report)) . fmap filePath . documentFile) (M.lookup (reportBuffer report) (buffers d))
  && all (not . protectedFilePath private) (reportFiles report)
  where
    private=privateFilePaths d

-- Reserve an id for a request. A full table refuses instead of evicting.
reserve :: SymbolInspector -> Int -> Int -> IO (Maybe ReportId)
reserve si bid version=atomicModifyIORef' (siState si) $ \st->
  if M.size (reserved st)>=maxReserved then (st,Nothing)
  else let rid=ReportId (nextReport st) in (st {nextReport=nextReport st+1,reserved=M.insert rid (bid,version) (reserved st)},Just rid)

-- Only a reservation that no tick has revoked may become a report.
commit :: SymbolInspector -> Report -> IO Bool
commit si report=atomicModifyIORef' (siState si) $ \st->
  if M.notMember (reportId report) (reserved st) then (st,False)
  else let all'=M.insert (reportId report) report (reports st)
           dropped=M.keys all' `dropOldest` maxReports
           kept=foldr M.delete all' dropped
       in publishState (st {reserved=M.delete (reportId report) (reserved st),reports=kept,generation=generation st+1,
               pages=[page | page@(_,p)<-pages st,pageReport p `notElem` dropped]}) True
  where dropOldest keys limit=take (max 0 (length keys-limit)) keys

release :: SymbolInspector -> ReportId -> IO ()
release si rid=updateState si (\st->st {reserved=M.delete rid (reserved st)})

retireWhere :: SymbolInspector -> (Report -> Bool) -> IO ()
retireWhere si gone=updateState si $ \st->
  let (dead,alive)=M.partition gone (reports st)
  in if M.null dead then st else st {reports=alive,generation=generation st+1,pages=[page | page@(_,p)<-pages st,pageReport p `M.notMember` dead]}

-- | The buffer a report was made from, if it is still the same public revision.
sourceCurrent :: Desktop -> Int -> Int -> Bool
sourceCurrent d bid version=case M.lookup bid (buffers d) of
  Just doc->not (protectedBuffer d bid) && revision (documentBuffer doc)==version
  Nothing->False

reportSubject :: SymbolInspector -> ReportId -> IO (Maybe Subject)
reportSubject si rid=do
  root<-rootSubject (inspectorViews (siInspector si)) (siRoot si)
  pure $ do
    Subject here<-root
    top<-subjectAs @Reports (Subject here)
    case [Subject child | child<-follow reportsEdge top,reportId (hereValue child)==rid] of
      found:_->Just found
      []->Nothing

-- Human flow -------------------------------------------------------------------

-- | Intercept the human's Inspect symbol request; every other effect passes
-- through unchanged to the next handler.
symbolEffects :: SymbolInspector -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> [Effect] -> IO (Bool,Desktop)
symbolEffects si core d0 effects=foldM apply (False,d0) effects
  where
    apply state@(True,_) _=pure state
    apply (_,d) (LanguageRequest ExamineSymbol)=(False,) <$> startHuman si d
    apply (_,d) (LanguageRequest (GoToSite path row col expected))=(False,) <$> goToSite core d path row col expected
    apply (_,d) effect=core d [effect]

startHuman :: SymbolInspector -> Desktop -> IO Desktop
startHuman si d=case locateCaret d of
  Left err->pure d {status=err}
  Right loc->do
    previous<-atomicModifyIORef' (siState si) (\st->(st {human=Nothing},human st))
    forM_ previous (\(rid,worker)->cancel worker >> release si rid)
    reservation<-reserve si (locBuffer loc) (locRevision loc)
    case reservation of
      Nothing->pure d {status="Too many symbol inspections are running."}
      Just rid->do
        env<-captureEnv d (locPath loc)
        (started,waits)<-issue True si d loc
        worker<-async (finishHuman si env rid loc waits)
        updateState si (\st->st {human=Just (rid,worker)})
        pure started {status="Inspecting "<>locName loc<>"…"}

finishHuman :: SymbolInspector -> Env -> ReportId -> Located -> [IO (Either Text Value)] -> IO (Either Text W.WindowUpdate)
finishHuman si env rid loc waits=(do
  answers<-mapConcurrently id waits
  report<-buildReport env HumanOwner rid loc answers
  accepted<-commit si report
  if not accepted then pure (Left "The source changed before the inspection finished; inspect again.") else do
    subject<-reportSubject si rid
    case subject of
      Nothing->pure (Left "The inspection is no longer available.")
      Just found->inspectSubject (siInspector si) Menu.HumanMenu found Nothing) `finally` release si rid

-- | Adopt a finished inspection or revoke work whose source changed. This only
-- compares revisions and a bounded set of paths; no document is walked.
tickSymbolInspector :: SymbolInspector -> Desktop -> IO Desktop
tickSymbolInspector si d=do
  st<-readIORef (siState si)
  now<-siClock si
  let revoked=[rid | (rid,(bid,version))<-M.toList (reserved st),not (sourceCurrent d bid version)]
  unless (null revoked) (updateState si (\s->s {reserved=foldr M.delete (reserved s) revoked}))
  retireWhere si (not . reportCurrent d)
  updateState si (\s->s {pages=[page | page@(_,p)<-pages s,pageExpires p>now]})
  pending<-atomically (flushTBQueue (siReplies si))
  forM_ pending $ \(PendingReply rid answer reply)->do
    found<-M.lookup rid . reports <$> readIORef (siState si)
    let allowed=maybe False (reportCurrent d) found
    atomically (void (tryPutTMVar reply (if allowed then answer else Left expiredText)))
  case human st of
    Nothing->pure d
    Just (rid,worker)->do
      finished<-poll worker
      case finished of
        Nothing->pure d
        Just result->do
          updateState si (\s->s {human=Nothing})
          case result of
            Left (err::SomeException)->pure d {status="Symbol inspection failed: "<>T.take 200 (T.pack (show err))}
            Right (Left err)->pure d {status=err}
            Right (Right update)->do
              live<-M.member rid . reports <$> readIORef (siState si)
              if live then adoptWindowUpdate Menu.HumanMenu update d
                else W.retireWindowRef (W.updateWindowRef update) >> pure d {status="The source changed before the inspection finished; inspect again."}

-- | Jump to a verified source location. The line must still read as it did when
-- it was inspected, in the open buffer if the file is open and otherwise on disk.
goToSite :: (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> FilePath -> Int -> Int -> Text -> IO Desktop
goToSite core d path row col expected=do
  current<-currentLine d path row
  case current of
    Left err->pure d {status=err}
    Right line | line/=expected->pure d {status="The source changed since it was inspected; inspect again."}
               | otherwise->do
                   let (_,col16)=L.offsetPosition line (max 0 (col-1))
                   snd <$> core d [JumpTo path (row-1) col16]

-- The current text of a 1-based line: the open buffer when there is one.
currentLine :: Desktop -> FilePath -> Int -> IO (Either Text Text)
currentLine d path row=do
  canonical<-tryIOError (canonicalizePath path)
  case canonical of
    Right file | file==path,not (protectedFilePath (privateFilePaths d) file)->
      case [bufferContent (documentBuffer doc) | (bid,doc)<-M.toList (buffers d),fmap filePath (documentFile doc)==Just file,documentLabel doc==Nothing,not (protectedBuffer d bid)] of
        content:_->pure (maybe missing Right (boundedLine content (row-1)))
        []->do
          found<-readSourceLines file [row-1]
          pure (found >>= maybe missing Right . M.lookup (row-1))
    _->pure (Left "The source path changed or is protected; inspect again.")
  where missing=Left "That complete source line is unavailable within the excerpt limits; inspect again."

-- Agent tool -------------------------------------------------------------------

symbolToolName :: Text
symbolToolName="inspect_symbol"

symbolTools :: [Value]
symbolTools=[object
  ["name" .= symbolToolName
  ,"description" .= (("Investigate one Haskell symbol in an open buffer with the same semantic views the human sees: Summary (type), Definitions and References, built from read-only HLS hover/definition/references on live unsaved source. "
     <>"Actions: discover (what is available; optional bufferId,line,column check a position), inspect (bufferId,line,column,revision? -> a page), view (page,view -> another view of that page's object), "
     <>"follow (page,row -> the object or source location that row names; a source location is re-verified against current text), back (page -> the page it came from). "
     <>"Pages are opaque tokens bound to the caller, source revision and a 15 minute lifetime; row numbers are local to one page. Changing, closing or privatizing the buffer expires them. Locations are limited to the workspace.")::Text)
  ,"inputSchema" .= object ["type" .= ("object"::Text),"additionalProperties" .= False
     ,"properties" .= object
       ["action" .= object ["type" .= ("string"::Text),"enum" .= (["discover","inspect","view","follow","back"]::[Text])]
       ,"bufferId" .= object ["type" .= ("integer"::Text),"minimum" .= (0::Int)]
       ,"line" .= object ["type" .= ("integer"::Text),"minimum" .= (1::Int)]
       ,"column" .= object ["type" .= ("integer"::Text),"minimum" .= (1::Int)]
       ,"revision" .= object ["type" .= ("integer"::Text),"minimum" .= (0::Int)]
       ,"page" .= object ["type" .= ("string"::Text),"maxLength" .= (128::Int)]
       ,"row" .= object ["type" .= ("integer"::Text),"minimum" .= (0::Int)]
       ,"view" .= object ["type" .= ("string"::Text),"maxLength" .= (128::Int)]]
     ,"required" .= (["action"]::[Text])]
  ,"annotations" .= object ["readOnlyHint" .= True,"destructiveHint" .= False,"idempotentHint" .= False,"openWorldHint" .= False]]]

data Request = Discover (Maybe (Int,Int,Int)) | Inspect' Int Int Int (Maybe Int) | ViewOf Text Text | FollowRow Text Int | BackFrom Text

parseRequest :: Value -> Either Text Request
parseRequest=either (Left . T.pack) Right . parseEither (withObject "inspect_symbol arguments" $ \o->do
  unless (all ((`elem` ["action","bufferId","line","column","revision","page","row","view"]) . Key.toText) (K.keys o)) (fail "Unknown argument")
  action<-o .: "action" :: Parser Text
  case action of
    "discover"->do
      bid<-o .:? "bufferId"
      case bid of
        Nothing->pure (Discover Nothing)
        Just b->do row<-o .: "line"; col<-o .: "column"; pure (Discover (Just (b,row,col)))
    "inspect"->Inspect' <$> o .: "bufferId" <*> o .: "line" <*> o .: "column" <*> o .:? "revision"
    "view"->ViewOf <$> o .: "page" <*> o .: "view"
    "follow"->FollowRow <$> o .: "page" <*> o .: "row"
    "back"->BackFrom <$> o .: "page"
    _->fail "Unknown action")

-- | Run one @inspect_symbol@ action. Initiation happens under the desktop's
-- serialization; the returned wait runs outside it. @caller@ identifies the
-- agent; reports and pages never cross callers.
symbolTool :: SymbolInspector -> Maybe Text -> Desktop -> Text -> Value -> IO (Desktop,IO (Either Text Value))
symbolTool si caller d name arguments=do
  retireWhere si (not . reportCurrent d)
  startSymbolTool si caller d name arguments

startSymbolTool :: SymbolInspector -> Maybe Text -> Desktop -> Text -> Value -> IO (Desktop,IO (Either Text Value))
startSymbolTool si caller d _ arguments=case parseRequest arguments of
  Left err->refuse ("Invalid inspect_symbol arguments: "<>err)
  Right request->case request of
    Discover position->(d,) . pure <$> discover si owner d position
    Inspect' bid row col expected->case locateAt d bid expected row col of
      Left err->refuse err
      Right loc->do
        reservation<-reserve si (locBuffer loc) (locRevision loc)
        case reservation of
          Nothing->refuse "Too many symbol inspections are running."
          Just rid->do
            env<-captureEnv d (locPath loc)
            (started,waits)<-issue False si d loc
            pure (started,publishReply si rid (inspectAgent si owner env rid loc waits))
    ViewOf token view->use token (\page->case mkViewId view of
      Left _->pure (Left "Unknown view.")
      Right ident->render si owner (pageParent page) (pageReport page) (pageSubject page) (Just ident))
    FollowRow token row->use token (followAgent si owner d token row)
    BackFrom token->use token (\page->case pageParent page of
      Nothing->pure (Left "This page has no earlier page.")
      Just parent->do
        again<-lookupPage si owner parent
        case again of
          Left err->pure (Left err)
          Right earlier->do
            checked<-pageLive si d earlier
            case checked of
              Left err->pure (Left err)
              Right _->renderPage si owner (pageParent earlier) (pageReport earlier) (pageSubject earlier) (Just (pageView earlier)) (pageOffset earlier))
  where
    owner=AgentOwner (fromMaybe "local" caller)
    refuse err=pure (d,pure (Left err))
    use token action=pure (d,do
      found<-lookupPage si owner token
      case found of
        Left err->pure (Left err)
        Right page->do
          checked<-pageLive si d page
          case checked of
            Left err->pure (Left err)
            Right _->publishReply si (pageReport page) (action page))

-- A page must exist, belong to the caller and not have expired.
lookupPage :: SymbolInspector -> Owner -> Text -> IO (Either Text AgentPage)
lookupPage si owner token=do
  st<-readIORef (siState si)
  now<-siClock si
  pure $ case lookup token (pages st) of
    Nothing->Left "Unknown or expired page. Run inspect again."
    Just page | pageOwner page/=owner->Left "Unknown or expired page. Run inspect again."
              | pageExpires page<=now->Left "Unknown or expired page. Run inspect again."
              | otherwise->Right page

-- The page's report must still be live and its source the same public revision.
pageLive :: SymbolInspector -> Desktop -> AgentPage -> IO (Either Text Report)
pageLive si d page=do
  st<-readIORef (siState si)
  case M.lookup (pageReport page) (reports st) of
    Just report | reportOwner report==pageOwner page && reportCurrent d report->pure (Right report)
    Just report->do
      retireWhere si (\r->reportId r==reportId report)
      pure (Left expiredText)
    Nothing->pure (Left expiredText)

expiredText :: Text
expiredText="This inspection expired: its buffer changed, closed or became private. Run inspect again."

inspectAgent :: SymbolInspector -> Owner -> Env -> ReportId -> Located -> [IO (Either Text Value)] -> IO (Either Text Value)
inspectAgent si owner env rid loc waits=(do
  answers<-mapConcurrently id waits
  report<-buildReport env owner rid loc answers
  accepted<-commit si report
  if not accepted then pure (Left expiredText) else do
    subject<-reportSubject si rid
    case subject of
      Nothing->pure (Left expiredText)
      Just found->render si owner Nothing rid found Nothing) `finally` release si rid

-- Render one object as an agent page: the same prepared document the human sees.
render :: SymbolInspector -> Owner -> Maybe Text -> ReportId -> Subject -> Maybe ViewId -> IO (Either Text Value)
render si owner parent rid subject requested=renderPage si owner parent rid subject requested 0

renderPage :: SymbolInspector -> Owner -> Maybe Text -> ReportId -> Subject -> Maybe ViewId -> Int -> IO (Either Text Value)
renderPage si owner parent rid subject requested offset=do
  resolved<-resolve (inspectorViews (siInspector si)) subject
  case resolved of
    Gone->pure (Left "That object no longer exists; run inspect again.")
    RootRetired->pure (Left "That object's source is no longer available; run inspect again.")
    Fresh current->do
      available<-applicableViews (inspectorViews (siInspector si)) current
      let chosen=case requested of
            Just wanted | wanted `elem` map infoView available->Just wanted
            Just _->Nothing
            Nothing->infoView <$> safeHead available
      case chosen of
        Nothing->pure (Left "That view does not apply to this object.")
        Just view->do
          prepared<-openPage (inspectorViews (siInspector si)) subject view offset
          case prepared of
            Left err->pure (Left (T.pack (show err)))
            Right ready->do
              token<-T.pack <$> randomIdentity
              now<-siClock si
              let page=renderJSON ready
                  entry=AgentPage owner rid current view offset page parent (now+pageLifetime)
              atomicModifyIORef' (siState si) $ \st->case M.lookup rid (reports st) of
                Nothing->(st,Left expiredText)
                Just report->publishState (st {pages=take maxPages ((token,entry):pages st)}) (Right (pageValue token parent report available view page))

  where safeHead (x:_)=Just x
        safeHead []=Nothing

pageValue :: Text -> Maybe Text -> Report -> [ViewInfo] -> ViewId -> JSONPage -> Value
pageValue token parent report available view page=object
  ["page" .= token,"back" .= parent,"report" .= reportValue report
  ,"view" .= viewIdText view
  ,"views" .= [object ["id" .= viewIdText (infoView v),"title" .= infoTitle v,"current" .= (infoView v==view)] | v<-available]
  ,"document" .= (member' "document" (jsonValue page)),"hasMore" .= (member' "hasMore" (jsonValue page))]
  where member' k v=fromMaybe Null (member k v)

reportValue :: Report -> Value
reportValue report=object
  ["symbol" .= reportSymbol report,"path" .= reportPath report,"line" .= reportLine report,"column" .= reportColumn report
  ,"bufferId" .= reportBuffer report,"revision" .= reportRevision report
  ,"hover" .= outcomeValue (const Nothing) (reportHover report)
  ,"definitions" .= outcomeValue (Just . length) (reportDefinitions report)
  ,"references" .= outcomeValue (Just . length) (reportReferences report)
  ,"omittedAtLeast" .= reportOmitted report]
  where
    outcomeValue :: (a -> Maybe Int) -> Outcome a -> Value
    outcomeValue count outcome=case outcome of
      Available a->object (["status" .= ("available"::Text)]++["count" .= n | Just n<-[count a]])
      Empty why->object ["status" .= ("empty"::Text),"message" .= why]
      Unavailable why->object ["status" .= ("unavailable"::Text),"message" .= why]

-- Follow a row of an exact stored page. An inspector target becomes a new page; a
-- source location is verified against the current text before it is reported.
followAgent :: SymbolInspector -> Owner -> Desktop -> Text -> Int -> AgentPage -> IO (Either Text Value)
followAgent si owner d token row page=case jsonTarget (pageJSON page) row of
  Nothing->pure (Left "That row has no action on this page.")
  Just (Inspect subject)->
    if subjectRoot subject/=subjectRoot (pageSubject page) then pure (Left "That row belongs to another inspection.")
    else render si owner (Just token) (pageReport page) subject Nothing
  Just (PageAt subject view offset)->renderPage si owner (Just token) (pageReport page) subject (Just view) offset
  Just (OpenFile _)->pure (Left "File rows are not available through this tool.")
  Just (OpenAt path line column expected)->do
    st<-readIORef (siState si)
    env<-maybe (pure Nothing) (fmap Just . captureEnv d . reportPath) (M.lookup (pageReport page) (reports st))
    if maybe True (\known->not (admitted known path)) env then pure (Left "That location is outside the workspace or protected.") else do
      current<-currentLine d path line
      pure $ case current of
        Left err->Left err
        Right text | text/=expected->Left "The source changed since it was inspected; run inspect again."
                   | otherwise->Right (object
                       ["page" .= token,"location" .= object ["path" .= path,"line" .= line,"column" .= column,"text" .= snippet text,"verified" .= True]])

-- Discovery: what exists and what this caller has open. Never contacts HLS.
discover :: SymbolInspector -> Owner -> Desktop -> Maybe (Int,Int,Int) -> IO (Either Text Value)
discover si owner d position=do
  st<-readIORef (siState si)
  now<-siClock si
  let mine=[object ["page" .= token,"report" .= reportSymbol report,"view" .= viewIdText (pageView page),"secondsLeft" .= ((pageExpires page-now) `div` 1000000000)]
           | (token,page)<-pages st,pageOwner page==owner,Just report<-[M.lookup (pageReport page) (reports st)]]
      probe=case position of
        Nothing->Null
        Just (bid,row,col)->case locateAt d bid Nothing row col of
          Left err->object ["eligible" .= False,"reason" .= err]
          Right loc->object ["eligible" .= True,"symbol" .= locName loc,"line" .= locLine loc,"column" .= locColumn loc,"revision" .= locRevision loc]
  pure (Right (object
    ["tool" .= symbolToolName,"actions" .= (["discover","inspect","view","follow","back"]::[Text])
    ,"views" .= [object ["id" .= vid,"title" .= title,"object" .= ty] | (ty,vid,title)<-symbolViewCatalog]
    ,"limits" .= object ["pageLifetimeSeconds" .= (pageLifetime `div` 1000000000),"pages" .= maxPages,"reports" .= maxReports,"locations" .= maxSites]
    ,"position" .= probe,"pages" .= mine]))
