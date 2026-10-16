-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-x-partial -Wno-incomplete-uni-patterns #-}
-- | Module      : SymbolInspectorCheck
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Framework-free checks of symbol inspection. They drive the real "Hide.Tooling"
-- workers, "Hide.SymbolInspector", the human rows window and the registered tool
-- function against @test/inspector-lsp.mjs@, a deterministic FAKE language server.
-- Passing here is regression coverage; it is not a real-HLS result.
module SymbolInspectorCheck (checks) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, poll, waitCatch, withAsync)
import Control.Exception (finally, throwIO)
import Control.Monad (forM, unless, void)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as K
import Data.IORef
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Hide.Buffer
import qualified Hide.App as App
import Hide.Files (FileState(..))
import Hide.InspectorWindow
import qualified Hide.LSP as L
import Hide.Model
import qualified Hide.Plugin.Menu as Menu
import qualified Hide.Plugin.Tree as Tree
import qualified Hide.Plugin.Window as W
import Hide.SymbolInspect
import Hide.SymbolInspector
import Hide.Tooling
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)

libText, mainText, secretText :: Text
libText=T.unlines
  ["module Lib where","area :: Int -> Int","area r = r * r","boom :: Int","boom = 1","empty :: Int","empty = 2"
  ,"slow :: Int","slow = 3","elsewhere :: Int","elsewhere = 4"]
mainText=T.unlines
  ["module Main where","import Lib","","main :: IO ()"
  ,"main = print (area 2) >> print (area 3) >> print (boom + empty + slow + elsewhere)"]
secretText=T.unlines ["module Secret where","import Lib","secret :: Int","secret = area 1"]

-- | The 1-based position of the first occurrence of a word in Main.hs.
at :: Text -> (Int,Int)
at word=head [(row,1+T.length before) | (row,line)<-zip [1..] (T.lines mainText),let (before,rest)=T.breakOn word line,not (T.null rest)]

data World = World
  { tooling :: Tooling, symbols :: SymbolInspector, inspector :: Inspector
  , mainPath :: FilePath, libPath :: FilePath, secretPath :: FilePath
  , desktop :: IORef Desktop, clock :: IORef Integer, mainId :: Int }

-- Run the editor's tick loop until the action finishes, bounded.
driven :: World -> IO a -> IO a
driven world action=withAsync action $ \job->do
  let loop (0::Int)=cancel job >> error "symbol inspection check timed out"
      loop n=do
        current<-readIORef (desktop world)
        ticked<-tickTooling (tooling world) core current >>= tickSymbolInspector (symbols world)
        writeIORef (desktop world) ticked
        done<-poll job
        case done of
          Just (Right value)->pure value
          Just (Left err)->throwIO err
          Nothing->threadDelay 2000 >> loop (n-1)
  loop 6000

core :: Desktop -> [Effect] -> IO (Bool,Desktop)
core d _=pure (False,d)

-- One agent call. The caller is the agent's identity.
call :: World -> Text -> Value -> IO (Either Text Value)
call world caller arguments=do
  current<-readIORef (desktop world)
  (next,answer)<-symbolTool (symbols world) (Just caller) current symbolToolName arguments
  writeIORef (desktop world) next
  driven world answer

inspectArgs :: World -> (Int,Int) -> Value
inspectArgs world (row,col)=object ["action" .= ("inspect"::Text),"bufferId" .= mainId world,"line" .= row,"column" .= col]

field :: Text -> Value -> Value
field key (Object o)=maybe Null id (K.lookup (Key.fromText key) o)
field _ _=Null

path :: [Text] -> Value -> Value
path keys value=foldl (flip field) value keys

items :: Value -> [Value]
items page=case path ["document","items"] page of Array xs->V.toList xs; _->[]

labelsOf :: Value -> [Text]
labelsOf page=[t | item<-items page,String t<-[field "label" item]]

pageToken :: Value -> Text
pageToken page=case field "page" page of String t->t; _->error ("no page token in "++show page)

textOf :: Value -> Text
textOf (String t)=t
textOf _=""

rightOf :: Show e => String -> Either e a -> IO a
rightOf label (Left err)=error (label++": "++show err)
rightOf _ (Right value)=pure value

rowIndex :: Text -> Value -> Int
rowIndex wanted page=case [n | (n,item)<-zip [0..] (items page),textOf (field "label" item)==wanted || wanted `T.isPrefixOf` textOf (field "label" item)] of
  n:_->n
  []->error ("no row "++show wanted++" in "++show (labelsOf page))

checks :: IO ()
checks=do
  let check label ok=unless ok (error ("SymbolInspector: "++label))
  -- Symbol selection is pure and total.
  let sym=symbolAt
  check "an identifier under the caret is selected" (sym "foo bar" 1==Just ("foo",0))
  check "a caret just after a name selects it" (sym "foo bar" 3==Just ("foo",0))
  check "the character under the caret wins over the one before" (sym "foo bar" 4==Just ("bar",4))
  check "primes and underscores belong to names" (sym "x_1' y" 1==Just ("x_1'",0))
  check "operators are symbols" (sym "a <+> b" 3==Just ("<+>",2))
  check "numeric literals are not symbols" (sym "x = 42" 5==Nothing)
  check "comment markers and reserved syntax are not symbols" (sym "-- note" 1==Nothing && sym "x = 1" 2==Nothing && sym "a -> b" 3==Nothing && sym "a - b" 2==Just ("-",2))
  check "space between tokens is not a symbol" (sym "a  b" 2==Nothing && sym "" 0==Nothing)
  check "positions outside the text select nothing" (sym "abc" (-1)==Nothing && sym "abc" 9==Nothing)

  rootDirectory<-temporaryRoot
  flip finally (removePathForcibly rootDirectory) $ do
    let main'=rootDirectory </> "Main.hs"; lib=rootDirectory </> "Lib.hs"; secret=rootDirectory </> "Secret.hs"
        outsideDirectory=rootDirectory++"-outside"
        outside=outsideDirectory </> "Outside.hs"
    createDirectory outsideDirectory
    flip finally (removePathForcibly outsideDirectory) $ do
      writeFile (rootDirectory </> "fixture.cabal") "name: fixture\n"
      TIO.writeFile main' mainText
      TIO.writeFile lib libText
      TIO.writeFile secret secretText
      TIO.writeFile outside "x = 1\n"
      script<-makeAbsolute "test/inspector-lsp.mjs"
      let launch=L.startClientWith "node" [script,"--lsp","--outside",outside]
      time<-newIORef (1000000000000::Integer)
      let initial=(addDocument (Just (FileState main' Nothing)) (newBuffer mainText) (initialDesktop (100,30))) {launchDirectory=rootDirectory}
      state<-newIORef initial
      withTooling launch $ \tool->withInspector $ \insp->withSymbolInspectorClock (readIORef time) tool insp $ \si->do
        let world=World tool si insp main' lib secret state time (maybe 0 id (activeWindow initial >>= bufferId))
            (areaRow,areaCol)=at "area"
        agentChecks world check (areaRow,areaCol)
        publicationCheck world check (areaRow,areaCol)
        failureChecks world check
        revisionChecks world check (areaRow,areaCol)
        accessChecks world check (areaRow,areaCol) outside
        cancellationChecks world check
        humanChecks world check (areaRow,areaCol)
      -- HLS that cannot start is reported, not hidden.
      broken<-newIORef initial
      withTooling (\_->ioError (userError "no HLS here")) $ \tool->withInspector $ \insp->withSymbolInspectorClock (readIORef time) tool insp $ \si->do
        let world=World tool si insp main' lib secret broken time (maybe 0 id (activeWindow initial >>= bufferId))
        answer<-call world "agent" (inspectArgs world (at "area"))
        page<-rightOf "unavailable HLS still describes the symbol" answer
        check "an unavailable HLS is stated on every relation" (all (\key->field "status" (path ["report",key] page)==String "unavailable") ["hover","definitions","references"])
        check "the unavailable message is visible" ("no HLS here" `T.isInfixOf` textOf (path ["report","hover","message"] page))
  putStrLn "symbol inspector checks passed (fake language server; not a real-HLS result)"

temporaryRoot :: IO FilePath
temporaryRoot=do
  base<-getTemporaryDirectory
  (file,handle)<-openTempFile base "symbol-inspector"
  hClose handle
  removeFile file
  createDirectory file
  canonicalizePath file

-- The tool end to end: discovery, inspection, views, follow, back.
agentChecks :: World -> (String -> Bool -> IO ()) -> (Int,Int) -> IO ()
agentChecks world check position=do
  discovered<-call world "agent" (object ["action" .= ("discover"::Text),"bufferId" .= mainId world,"line" .= fst position,"column" .= snd position]) >>= rightOf "discover"
  check "discovery lists the actions and views" (field "actions" discovered==toJSON (["discover","inspect","view","follow","back"]::[Text]) && length (case field "views" discovered of Array xs->V.toList xs; _->[])==4)
  check ("discovery says whether a position is eligible without contacting HLS "++show discovered) (path ["position","eligible"] discovered==Bool True && path ["position","symbol"] discovered==String "area")
  summary<-call world "agent" (inspectArgs world position) >>= rightOf "inspect"
  check "the report names the symbol and its source" (path ["report","symbol"] summary==String "area" && textOf (path ["report","path"] summary)/="" && path ["view"] summary==String "hide.symbol.summary")
  check "hover text is the type the server returned" ("area :: Int → Int" `T.isInfixOf` T.unwords (map (textOf . field "detail") (items summary)))
  check "all three relations are available" (all (\key->field "status" (path ["report",key] summary)==String "available") ["hover","definitions","references"])
  check "views are discoverable from a page" (map (textOf . field "id") (case field "views" summary of Array xs->V.toList xs; _->[])==["hide.symbol.summary","hide.symbol.definitions","hide.symbol.references"])
  definitions<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken summary,"view" .= ("hide.symbol.definitions"::Text)]) >>= rightOf "definitions view"
  check "the definition is the line in Lib.hs" (labelsOf definitions==["Lib.hs:2:1"] && any (T.isInfixOf "area :: Int -> Int" . textOf . field "detail") (items definitions))
  check "a view switch keeps the previous parent (it is not history)" (field "back" definitions==Null)
  site<-call world "agent" (object ["action" .= ("follow"::Text),"page" .= pageToken definitions,"row" .= (0::Int)]) >>= rightOf "follow definition"
  check "following a definition opens the site's own view" (path ["view"] site==String "hide.site.source" && head (labelsOf site)=="Open in editor" && field "back" site==String (pageToken definitions))
  located<-call world "agent" (object ["action" .= ("follow"::Text),"page" .= pageToken site,"row" .= (0::Int)]) >>= rightOf "follow open-source"
  check "the source location is verified against the current text" (path ["location","verified"] located==Bool True && path ["location","line"] located==Number 2 && "area ::" `T.isPrefixOf` textOf (path ["location","text"] located) && ("Lib.hs" `T.isSuffixOf` textOf (path ["location","path"] located)))
  back<-call world "agent" (object ["action" .= ("back"::Text),"page" .= pageToken site]) >>= rightOf "back"
  check "back restores the definitions view, not the summary" (path ["view"] back==String "hide.symbol.definitions" && labelsOf back==labelsOf definitions)
  references<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken back,"view" .= ("hide.symbol.references"::Text)]) >>= rightOf "references"
  check "references include uses in Main.hs and Lib.hs" (length (labelsOf references)>=5 && any ("Main.hs:5:" `T.isPrefixOf`) (labelsOf references) && any ("Lib.hs:3:1" ==) (labelsOf references))
  reference<-call world "agent" (object ["action" .= ("follow"::Text),"page" .= pageToken references,"row" .= rowIndex "Main.hs:5:" references]) >>= rightOf "follow reference"
  check "a reference site says it is a reference" ("Reference: " `T.isInfixOf` textOf (path ["document","title"] reference))
  listed<-call world "agent" (object ["action" .= ("discover"::Text)]) >>= rightOf "discover pages"
  check "discovery lists this caller's pages" (length (case field "pages" listed of Array xs->V.toList xs; _->[])>=5)
  other<-call world "other-agent" (object ["action" .= ("discover"::Text)]) >>= rightOf "discover pages of another caller"
  check "another caller sees none of them" (field "pages" other==toJSON ([]::[Value]))

-- The response is already prepared when privacy changes, but has not been
-- admitted by the owner tick. Cached payloads must not bypass that last check.
publicationCheck :: World -> (String -> Bool -> IO ()) -> (Int,Int) -> IO ()
publicationCheck world check position=do
  before<-readIORef (desktop world)
  count<-agentPageCount (symbols world)
  (next,answer)<-symbolTool (symbols world) (Just "agent") before symbolToolName (inspectArgs world position)
  writeIORef (desktop world) next
  withAsync answer $ \pending->do
    let prepare=do
          current<-readIORef (desktop world)
          ticked<-tickTooling (tooling world) core current
          writeIORef (desktop world) ticked
          pages<-agentPageCount (symbols world)
          if pages>count then pure () else threadDelay 1000 >> prepare
    ready<-timeout 5000000 prepare
    check "response preparation finishes without owner admission" (ready==Just ())
    modifyIORef' (desktop world) (\d->d {guestPrivatePaths=[secretPath world]})
    result<-driven world (waitCatch pending)
    check "the publication gate rejects a newly private referenced file"
      (case result of Right (Left err)->"expired" `T.isInfixOf` err; _->False)
  writeIORef (desktop world) before

failureChecks :: World -> (String -> Bool -> IO ()) -> IO ()
failureChecks world check=do
  let probe word=call world "agent" (inspectArgs world (at word))
  boom<-probe "boom" >>= rightOf "boom"
  check "an HLS error is stated per relation, with the server's message" (all (\key->field "status" (path ["report",key] boom)==String "unavailable" && "boom exploded" `T.isInfixOf` textOf (path ["report",key,"message"] boom)) ["hover","definitions","references"])
  empty<-probe "empty" >>= rightOf "empty"
  check "an empty answer is stated, not an error" (field "status" (path ["report","definitions"] empty)==String "empty" && "No definition found" `T.isInfixOf` textOf (path ["report","definitions","message"] empty) && field "status" (path ["report","hover"] empty)==String "empty")
  emptyDefinitions<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken empty,"view" .= ("hide.symbol.definitions"::Text)]) >>= rightOf "empty definitions view"
  check "an empty relation renders one explanatory row with no action" (labelsOf emptyDefinitions==["Definition not available"] && field "action" (head (items emptyDefinitions))==Null)
  nothing<-call world "agent" (object ["action" .= ("inspect"::Text),"bufferId" .= mainId world,"line" .= (3::Int),"column" .= (1::Int)])
  check "no symbol at the position is refused plainly" (nothing==Left "No symbol at that position.")
  outsideLine<-call world "agent" (object ["action" .= ("inspect"::Text),"bufferId" .= mainId world,"line" .= (99::Int),"column" .= (1::Int)])
  check "a line outside the buffer is refused" (outsideLine==Left "Line is outside the buffer.")
  unknown<-call world "agent" (object ["action" .= ("inspect"::Text),"bufferId" .= (77::Int),"line" .= (1::Int),"column" .= (1::Int)])
  check "an unknown buffer is refused" (unknown==Left "Unknown bufferId")
  malformed<-call world "agent" (object ["action" .= ("explode"::Text)])
  extra<-call world "agent" (object ["action" .= ("discover"::Text),"unexpected" .= True])
  check "unknown actions and extra arguments are refused" (isLeft malformed && isLeft extra)
  where
    isLeft (Left _)=True
    isLeft _=False

-- Revisions: a late answer is never published and old pages expire.
revisionChecks :: World -> (String -> Bool -> IO ()) -> (Int,Int) -> IO ()
revisionChecks world check position=do
  page<-call world "agent" (inspectArgs world position) >>= rightOf "inspect before edit"
  before<-readIORef (desktop world)
  check "the report records the revision it was made at" (path ["report","revision"] page==Number (fromIntegral (maybe 0 (revision . documentBuffer) (activeDocument before))))
  stale<-call world "agent" (object ["action" .= ("inspect"::Text),"bufferId" .= mainId world,"line" .= fst position,"column" .= snd position,"revision" .= (999::Int)])
  check "a wrong revision argument is refused" (stale==Left "Buffer revision changed.")
  modifyIORef' (desktop world) (insertText "x")
  edited<-readIORef (desktop world)
  -- Before any tick the live desktop alone decides.
  answer<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken page,"view" .= ("hide.symbol.references"::Text)])
  check "a page of an edited buffer is expired at once" (either (T.isInfixOf "expired") (const False) answer)
  _<-tickSymbolInspector (symbols world) edited
  answerAfter<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken page,"view" .= ("hide.symbol.references"::Text)])
  check "and stays expired after the tick retires it" (either ("Run inspect again." `T.isInfixOf`) (const False) answerAfter)
  reports<-liveReports (symbols world)
  check "the stale report is retired" (null reports)
  pages<-agentPageCount (symbols world)
  check "its pages are retired with it" (pages==0)
  -- Pending work is revoked by the tick when its source changes.
  writeIORef (desktop world) before
  current<-readIORef (desktop world)
  (started,answerLater)<-symbolTool (symbols world) (Just "agent") current symbolToolName (object ["action" .= ("inspect"::Text),"bufferId" .= mainId world,"line" .= fst (at "slow"),"column" .= snd (at "slow")])
  writeIORef (desktop world) (insertText "y" started)
  job<-async answerLater
  changed<-readIORef (desktop world)
  _<-tickSymbolInspector (symbols world) changed
  cancel job
  void (waitCatch job)
  check "work for a changed buffer cannot commit" . null =<< liveReports (symbols world)
  writeIORef (desktop world) before

-- Access: private buffers, protected paths, forged and expired tokens, stale sites.
accessChecks :: World -> (String -> Bool -> IO ()) -> (Int,Int) -> FilePath -> IO ()
accessChecks world check position outside=do
  before<-readIORef (desktop world)
  -- Private from the start.
  writeIORef (desktop world) before {guestPrivatePaths=[mainPath world]}
  private<-call world "agent" (inspectArgs world position)
  check "a private buffer cannot be inspected" (private==Left "This buffer is private to the user.")
  discoveredPrivate<-call world "agent" (object ["action" .= ("discover"::Text),"bufferId" .= mainId world,"line" .= fst position,"column" .= snd position]) >>= rightOf "discover private"
  check "discovery reports a private buffer as ineligible" (path ["position","eligible"] discoveredPrivate==Bool False)
  writeIORef (desktop world) before
  -- Becoming private revokes an existing inspection.
  page<-call world "agent" (inspectArgs world position) >>= rightOf "inspect public"
  definitions<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken page,"view" .= ("hide.symbol.definitions"::Text)]) >>= rightOf "definitions"
  writeIORef (desktop world) before {guestPrivatePaths=[mainPath world]}
  revoked<-call world "agent" (object ["action" .= ("follow"::Text),"page" .= pageToken definitions,"row" .= (0::Int)])
  check "making the buffer private revokes follow-up actions" (isExpired revoked)
  revokedView<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken page,"view" .= ("hide.symbol.summary"::Text)])
  check "and every other action on that inspection" (isExpired revokedView)
  writeIORef (desktop world) before
  -- A reference file becoming private also revokes the cached excerpts.
  public<-call world "agent" (inspectArgs world position) >>= rightOf "public referenced file"
  publicRefs<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken public,"view" .= ("hide.symbol.references"::Text)]) >>= rightOf "public references"
  writeIORef (desktop world) before {guestPrivatePaths=[secretPath world]}
  revokedReference<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken publicRefs,"view" .= ("hide.symbol.references"::Text)])
  check "privatizing a referenced file revokes cached text" (isExpired revokedReference || isUnknown revokedReference)
  writeIORef (desktop world) before
  -- Tokens: forged, truncated, cross-caller, expired.
  fresh<-call world "agent" (inspectArgs world position) >>= rightOf "inspect for tokens"
  forged<-call world "agent" (object ["action" .= ("view"::Text),"page" .= ("0123456789abcdef"::Text),"view" .= ("hide.symbol.summary"::Text)])
  check "a forged token is refused" (isUnknown forged)
  stolen<-call world "intruder" (object ["action" .= ("view"::Text),"page" .= pageToken fresh,"view" .= ("hide.symbol.summary"::Text)])
  check "another caller's token is refused" (isUnknown stolen)
  noAction<-call world "agent" (object ["action" .= ("follow"::Text),"page" .= pageToken fresh,"row" .= (0::Int)])
  check "a row without an action is refused" (noAction==Left "That row has no action on this page.")
  farRow<-call world "agent" (object ["action" .= ("follow"::Text),"page" .= pageToken fresh,"row" .= (900::Int)])
  check "a row beyond the page is refused" (farRow==Left "That row has no action on this page.")
  badView<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken fresh,"view" .= ("hide.nothing"::Text)])
  check "a view that does not apply is refused" (badView==Left "That view does not apply to this object.")
  modifyIORef' (clock world) (+ (16*60*1000000000))
  expired<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken fresh,"view" .= ("hide.symbol.summary"::Text)])
  check "an expired page is refused" (isUnknown expired)
  -- A site whose line changed since inspection must not be reported.
  again<-call world "agent" (inspectArgs world position) >>= rightOf "inspect for site"
  defs<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken again,"view" .= ("hide.symbol.definitions"::Text)]) >>= rightOf "definitions for site"
  site<-call world "agent" (object ["action" .= ("follow"::Text),"page" .= pageToken defs,"row" .= (0::Int)]) >>= rightOf "site"
  TIO.writeFile (libPath world) (T.replace "area :: Int -> Int" "  area :: Int -> Int" libText)
  changedSite<-call world "agent" (object ["action" .= ("follow"::Text),"page" .= pageToken site,"row" .= (0::Int)])
  check "a location whose line changed is refused, not retargeted" (changedSite==Left "The source changed since it was inspected; run inspect again.")
  TIO.writeFile (libPath world) libText
  -- Locations outside the workspace or in protected files are counted, never shown.
  writeIORef (desktop world) before {guestPrivatePaths=[secretPath world]}
  refs<-call world "agent" (inspectArgs world position) >>= rightOf "inspect with protected neighbour"
  refsView<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken refs,"view" .= ("hide.symbol.references"::Text)]) >>= rightOf "references with protected neighbour"
  check "a protected file's reference is omitted" (not (any ("Secret.hs" `T.isInfixOf`) (labelsOf refsView)) && not ("Secret.hs" `isInfixOf` show refsView) && path ["report","omittedAtLeast"] refs==Number 1)
  writeIORef (desktop world) before
  let (elseRow,elseCol)=at "elsewhere"
  away<-call world "agent" (inspectArgs world (elseRow,elseCol)) >>= rightOf "inspect elsewhere"
  awayView<-call world "agent" (object ["action" .= ("view"::Text),"page" .= pageToken away,"view" .= ("hide.symbol.definitions"::Text)]) >>= rightOf "elsewhere definitions"
  check "a location outside the workspace is omitted" (not ("Outside.hs" `isInfixOf` show awayView) && not (any (T.isInfixOf (T.pack outside)) (labelsOf awayView)) && path ["report","omittedAtLeast"] away>=Number 1)
  where
    isExpired (Left err)="expired" `T.isInfixOf` err
    isExpired _=False
    isUnknown (Left err)="Unknown or expired page" `T.isInfixOf` err
    isUnknown _=False

-- Cancellation: a waiting request cancels its HLS requests and frees its slot.
cancellationChecks :: World -> (String -> Bool -> IO ()) -> IO ()
cancellationChecks world check=do
  let (row,col)=at "slow"
      args=object ["action" .= ("inspect"::Text),"bufferId" .= mainId world,"line" .= row,"column" .= col]
  current<-readIORef (desktop world)
  started<-forM [1..8::Int] $ \_->do
    d<-readIORef (desktop world)
    (next,waiting)<-symbolTool (symbols world) (Just "agent") d symbolToolName args
    writeIORef (desktop world) next
    async waiting
  d<-readIORef (desktop world)
  (_,refused)<-symbolTool (symbols world) (Just "agent") d symbolToolName args
  answer<-timeout 2000000 refused
  check "too many concurrent inspections are refused explicitly" (answer==Just (Left "Too many symbol inspections are running."))
  mapM_ cancel started
  mapM_ waitCatch started
  _<-driven world (pure ())
  freed<-call world "agent" (inspectArgs world (at "area"))
  check "cancelling the waits frees their slots" (either (const False) (const True) freed)
  check "cancelled work leaves no report behind" . (all ((/="slow") . reportSymbol)) =<< liveReports (symbols world)
  writeIORef (desktop world) current

-- Human flow: the same semantic views on the rows window, with Back and jumps.
humanChecks :: World -> (String -> Bool -> IO ()) -> (Int,Int) -> IO ()
humanChecks world check (row,col)=do
  original<-readIORef (desktop world)
  let human=Menu.HumanMenu
      Just mainWindow=activeWindow original
      place line column d=let b=maybe (error "no document") documentBuffer (activeDocument d)
                              offset=bufferLineOffset b (line-1)+column-1
                          in d {windows=map (\w->if windowId w==windowId mainWindow then w {selection=Selection offset offset} else w) (windows d)}
  -- A caret on whitespace says so.
  (_,blank)<-symbolEffects (symbols world) (toolingEffects (tooling world) App.applyEffects) (place 3 1 original) [LanguageRequest ExamineSymbol]
  check "a caret away from any symbol is reported" (status blank=="No symbol at that position.")
  -- The real thing, on a caret in the middle of `area`.
  (_,started)<-symbolEffects (symbols world) (toolingEffects (tooling world) App.applyEffects) (place row (col+2) original) [LanguageRequest ExamineSymbol]
  check "starting an inspection says what it inspects" ("Inspecting area" `T.isPrefixOf` status started)
  writeIORef (desktop world) started
  let before=M.size (pluginWindows started)
  adopted<-driven world (waitFor world (\d->M.size (pluginWindows d)>before))
  let Just (summaryRef,summaryWindow)=newestWindow adopted
  check ("the summary page is rendered on the rows window "++show (W.preparedWindowTitle summaryWindow,rowsOf summaryWindow)) ("area — Main.hs:5:15" `T.isInfixOf` W.preparedWindowTitle summaryWindow)
  check "the type is shown in the Type row's details" (any (\(label,detail)->label=="Type" && "area :: Int → Int" `T.isInfixOf` detail) (rowsOf summaryWindow))
  let go ref window label=do
        followed<-followInspectorAction (inspector world) human ref (nodeFor label window)
        pure followed
      open label ref window=do
        followed<-go ref window label
        case followed of
          Right (Opened update)->do
            admitted<-W.admitWindowUpdate False update
            maybe (error "not admitted") pure admitted
          Right Jump{}->error "unexpected jump"
          Left err->error (T.unpack err)
  (definitionsRef,definitionsWindow)<-open "→ Definitions view" summaryRef summaryWindow
  check "the definitions view lists the same site the agent sees" (take 1 (map fst (rowsOf definitionsWindow))==["Lib.hs:2:1"])
  (siteRef,siteWindow)<-open "Lib.hs:2:1" definitionsRef definitionsWindow
  check "a site page offers to open the location" ("Open in editor" `elem` map fst (rowsOf siteWindow) && "← Back" `elem` map fst (rowsOf siteWindow))
  (backRef,backWindow)<-open "← Back" siteRef siteWindow
  check "Back restores the definitions view the user had chosen" (map fst (rowsOf backWindow)==map fst (rowsOf definitionsWindow) && "← Back" `notElem` map fst (rowsOf backWindow))
  (referencesRef,referencesWindow)<-open "→ References view" backRef backWindow
  check "references list every use" (length (filter ("Main.hs" `T.isPrefixOf`) (map fst (rowsOf referencesWindow)))==2)
  (referenceRef,referenceWindow)<-open "Main.hs:5:15" referencesRef referencesWindow
  (siteAgain,siteWindowAgain)<-open "← Back" referenceRef referenceWindow
  void (pure (siteAgain,siteWindowAgain))
  -- Agents never drive the human's windows.
  refused<-followInspectorAction (inspector world) Menu.AgentMenu siteRef (nodeFor "Open in editor" siteWindow)
  check "the human's pages refuse an agent origin" (isLeft refused)
  -- Open in editor jumps, but only while the line still reads as inspected.
  jump<-followInspectorAction (inspector world) human siteRef (nodeFor "Open in editor" siteWindow)
  case jump of
    Right (Jump file line column expected)->do
      d0<-readIORef (desktop world)
      (_,jumped)<-symbolEffects (symbols world) (toolingEffects (tooling world) App.applyEffects) d0 [LanguageRequest (GoToSite file line column expected)]
      let Just doc=activeDocument jumped
          Just win=activeWindow jumped
      check "the jump opens the definition's file" (fmap filePath (documentFile doc)==Just (libPath world))
      check "the caret lands on the definition's first character" (caret (selection win)==bufferLineOffset (documentBuffer doc) 1)
      TIO.writeFile (libPath world) (T.replace "area :: Int -> Int" "areaX :: Int -> Int" libText)
      (_,refusedJump)<-symbolEffects (symbols world) (toolingEffects (tooling world) App.applyEffects) d0 [LanguageRequest (GoToSite file line column expected)]
      check "a location whose line changed refuses to jump" (status refusedJump=="The source changed since it was inspected; inspect again." && activeDocument refusedJump==activeDocument d0)
      TIO.writeFile (libPath world) libText
    _->error "SymbolInspector: Open in editor must produce a jump"
  -- Editing the source retires the human's inspection too.
  edited<-readIORef (desktop world)
  writeIORef (desktop world) edited {buffers=M.adjust (\doc->doc {documentBuffer=replaceSelection (Selection 0 0) "z" (documentBuffer doc)}) (mainId world) (buffers edited)}
  _<-driven world (pure ())
  gone<-followInspectorAction (inspector world) human siteRef (nodeFor "Open in editor" siteWindow)
  check "after an edit the human's old pages no longer resolve" (isLeft gone)
  check "no report outlives its source revision" . null =<< liveReports (symbols world)
  writeIORef (desktop world) original
  where
    isLeft (Left _)=True
    isLeft _=False

newestWindow :: Desktop -> Maybe (W.WindowRef,W.PreparedWindow)
newestWindow d=case M.toList (pluginWindows d) of
  []->Nothing
  windows'->Just (last windows')

waitFor :: World -> (Desktop -> Bool) -> IO Desktop
waitFor world ready=loop (3000::Int)
  where loop 0=error "SymbolInspector: window never adopted"
        loop n=do
          d<-readIORef (desktop world)
          if ready d then pure d else threadDelay 2000 >> loop (n-1)

rowsOf :: W.PreparedWindow -> [(Text,Text)]
rowsOf prepared=case W.preparedWindowRows prepared of
  W.RowsDetails rows _ _->[(caption,let content=W.preparedWindowText detail in contentSlice content 0 (contentLength content)) | W.WindowRow _ caption detail<-V.toList rows]
  _->error "expected a rows window"

nodeFor :: Text -> W.PreparedWindow -> Tree.NodeId
nodeFor wanted window=case W.preparedWindowRows window of
  W.RowsDetails rows _ _->case [ident | W.WindowRow ident label _<-V.toList rows,label==wanted] of
    ident:_->ident
    []->error ("missing row "++show wanted++" in "++show (map fst (rowsOf window)))
  _->error "expected a rows window"
