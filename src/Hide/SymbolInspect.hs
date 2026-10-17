-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.SymbolInspect
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Semantic descriptions of one inspected Haskell symbol.
--
-- A 'Report' records what the language server answered for one symbol at one
-- buffer revision: its hover text, its definitions and its references. Every
-- location is a 'Site' that carries the text of its line when observed.
-- Everything here is pure and presentation-free. "Hide.SymbolInspector"
-- queries HLS on the existing tooling workers and fills a 'Report'; the rows
-- window and the agent tool then render the /same/ documents built here.
--
-- Reports live under a single 'Reports' root. A 'ReportId' is a serial that is
-- never reused, and sites are reached by keyed edges, so a retired report or a
-- site that is no longer present resolves to 'Gone' instead of to other data.
module Hide.SymbolInspect
  ( Owner(..), ReportId(..), Outcome(..), Site(..), Report(..), Reports(..)
  , Role(..), siteRole, siteLabel, relativePath
  , reportsEdge, definitionsEdge, referencesEdge
  , symbolAt, siteKey
  , symbolContributions, publishSymbolViews, symbolViewCatalog
  ) where

import Data.Char (isAlphaNum, isDigit)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Plugin.Inspect
import System.FilePath (makeRelative, addTrailingPathSeparator)

-- | Who asked. Reports are never shown to the other kind of owner.
data Owner = HumanOwner | AgentOwner !Text deriving (Eq,Ord,Show)

-- | Monotonic and never reused within a session.
newtype ReportId = ReportId Int deriving (Eq,Ord,Show)

-- | What one language-server request produced. A missing answer and a failure
-- are different things and are described differently.
data Outcome a = Available !a | Empty !Text | Unavailable !Text deriving (Eq,Show)

-- | A source position: its absolute path, the path relative to the report's
-- project root for display, a 1-based line, a 1-based column in Unicode code
-- points and the exact line text observed when the report was built.
data Site = Site { sitePath :: !FilePath, siteRelative :: !FilePath, siteLine :: !Int, siteColumn :: !Int, siteText :: !Text }
  deriving (Eq,Ord,Show)

data Report = Report
  { reportId :: !ReportId, reportOwner :: !Owner
  , reportBuffer :: !Int, reportRevision :: !Int
  , reportRoot :: !FilePath, reportPath :: !FilePath
  , reportSymbol :: !Text, reportLine :: !Int, reportColumn :: !Int
  , reportHover :: !(Outcome Text)
  , reportDefinitions :: !(Outcome [Site]), reportReferences :: !(Outcome [Site])
  , reportOmitted :: !Int, reportFiles :: ![FilePath] }
  deriving (Eq,Show)

-- | The live reports of one editor session.
newtype Reports = Reports (M.Map ReportId Report)

data Role = DefinitionSite | ReferenceSite | PlainSite deriving (Eq,Show)

siteKey :: Site -> Key
siteKey site=Key (sitePath site,siteLine site,siteColumn site)

reportsEdge :: Edge Reports Report
reportsEdge=required (edge "hide.symbol.edge.reports" "Symbol reports" observed (\report->Key (case reportId report of ReportId n->n)))
  where observed f (Reports reports)=Reports <$> traverse f reports

definitionsEdge :: Edge Report Site
definitionsEdge=required (edge "hide.symbol.edge.definitions" "Definitions" observed siteKey)
  where observed f report=case reportDefinitions report of
          Available sites->(\kept->report {reportDefinitions=Available kept}) <$> traverse f sites
          _->pure report

referencesEdge :: Edge Report Site
referencesEdge=required (edge "hide.symbol.edge.references" "References" observed siteKey)
  where observed f report=case reportReferences report of
          Available sites->(\kept->report {reportReferences=Available kept}) <$> traverse f sites
          _->pure report

required :: Either Text a -> a
required=either (error . T.unpack) id

-- | Which relation led to this site, from its retained path.
siteRole :: Here Site -> Role
siteRole here=case reverse (snd (herePath here)) of
  Step ident _:_ | ident==edgeIdent definitionsEdge->DefinitionSite
                 | ident==edgeIdent referencesEdge->ReferenceSite
  _->PlainSite

relativePath :: FilePath -> FilePath -> FilePath
relativePath root path
  | addTrailingPathSeparator root `isPrefixOf` path=makeRelative root path
  | otherwise=path

siteLabel :: Site -> Text
siteLabel site=T.pack (siteRelative site)<>":"<>T.pack (show (siteLine site))<>":"<>T.pack (show (siteColumn site))

-- | The Haskell identifier or operator at a 0-based offset, with the offset of
-- its first character. A caret just past the end of a name selects that name.
-- Digits begin literals, not names. Selecting prefers the character under the
-- caret over the one before it.
symbolAt :: Text -> Int -> Maybe (Text,Int)
symbolAt text offset
  | offset<0 || offset>T.length text=Nothing
  | otherwise=do
      anchor<-case [n | n<-[offset,offset-1],Just _<-[kindAt n]] of
        n:_->Just n
        []->Nothing
      kind<-kindAt anchor
      let same c=classify c==Just kind
          start=anchor-T.length (T.takeWhileEnd same (T.take anchor text))
          name=T.takeWhile same (T.drop start text)
      if (isDigit (T.head name) && kind==Identifier) || reserved name then Nothing else Just (name,start)
  where
    kindAt n | n<0 || n>=T.length text=Nothing
             | otherwise=classify (T.index text n)

-- | Dashes alone open a line comment, and `|` or `=` alone are syntax, not names.
reserved :: Text -> Bool
reserved name=(T.length name>=2 && T.all (=='-') name) || name `elem` ["=","|","->","<-","=>","::","\\","~","@"]

data Kind = Identifier | Operator deriving (Eq,Show)

classify :: Char -> Maybe Kind
classify c | isAlphaNum c || c=='_' || c=='\''=Just Identifier
           | c `elem` ("!#$%&*+/<=>?@\\^|-~:" :: String)=Just Operator
           | otherwise=Nothing

-- Views ----------------------------------------------------------------------

countWord :: Int -> Text -> Text
countWord n noun=T.pack (show n)<>" "<>noun<>(if n==1 then "" else "s")

outcomeNote :: Outcome a -> Maybe Text
outcomeNote (Available _)=Nothing
outcomeNote (Empty why)=Just why
outcomeNote (Unavailable why)=Just ("Unavailable: "<>why)

omittedNote :: Report -> Text
omittedNote report
  | reportOmitted report==0=""
  | otherwise="\nAt least "<>countWord (reportOmitted report) "location"<>" outside the workspace, protected by policy or beyond the excerpt limits are not shown."

siteCount :: Outcome [Site] -> Text
siteCount (Available sites)=T.pack (show (length sites))
siteCount (Empty _)="none"
siteCount (Unavailable _)="unavailable"

summary :: Here Report -> Doc
summary here=(List (reportSymbol report<>" — "<>T.pack (relativePath (reportRoot report) (reportPath report))<>":"<>T.pack (show (reportLine report))<>":"<>T.pack (show (reportColumn report)))
  [ Item "Symbol" (T.intercalate "\n"
      [ "Name: "<>reportSymbol report
      , "Location: "<>T.pack (relativePath (reportRoot report) (reportPath report))<>":"<>T.pack (show (reportLine report))<>":"<>T.pack (show (reportColumn report))
      , "Source revision: "<>T.pack (show (reportRevision report)) ]) Nothing
  , Item "Type" (case reportHover report of
      Available hover->hover
      other->maybe "" id (outcomeNote other)) Nothing
  , Item ("Definitions: "<>siteCount (reportDefinitions report)) (maybe "Switch to the Definitions view to follow them." id (outcomeNote (reportDefinitions report))) Nothing
  , Item ("References: "<>siteCount (reportReferences report)) (maybe "Switch to the References view to follow them." id (outcomeNote (reportReferences report))<>omittedNote report) Nothing ]
  )
  where report=hereValue here

relation :: Text -> Edge Report Site -> (Report -> Outcome [Site]) -> Here Report -> Doc
relation noun edgeOf outcomeOf here=(List (reportSymbol report<>" — "<>heading) rows)
  where
    report=hereValue here
    heading=case outcomeOf report of
      Available sites->countWord (length sites) noun
      _->"no "<>noun<>"s"
    rows=case outcomeOf report of
      Available _->[Item (siteLabel site) (T.take 200 (T.strip (siteText site))) (Just (Inspect (Subject child)))
                   | child<-follow edgeOf here,let site=hereValue child]
                   ++[Item "Omitted" (omittedNote report) Nothing | reportOmitted report>0]
      other->[Item (noun'<>" not available") (maybe "" id (outcomeNote other)<>omittedNote report) Nothing]
    noun'=T.toUpper (T.take 1 noun)<>T.drop 1 noun

-- | One site, with a row that opens it in the editor and one that quotes it.
siteSource :: Here Site -> Doc
siteSource here=(List (role<>": "<>siteLabel site)
  [ Item "Open in editor" ("Jump to line "<>T.pack (show (siteLine site))<>", column "<>T.pack (show (siteColumn site))<>"\n"<>siteText site)
      (Just (OpenAt (sitePath site) (siteLine site) (siteColumn site) (siteText site)))
  , Item ("Source line "<>T.pack (show (siteLine site))) (siteText site) Nothing ])
  where
    site=hereValue here
    role=case siteRole here of
      DefinitionSite->"Definition"
      ReferenceSite->"Reference"
      PlainSite->"Location"

reportViews :: Either Text [ViewDef Report]
reportViews=sequence
  [ viewDef "hide.symbol.summary" "Summary" 0 summary
  , viewDef "hide.symbol.definitions" "Definitions" 10 (relation "definition" definitionsEdge reportDefinitions)
  , viewDef "hide.symbol.references" "References" 20 (relation "reference" referencesEdge reportReferences) ]

siteViews :: Either Text [ViewDef Site]
siteViews=sequence [viewDef "hide.site.source" "Source" 0 siteSource]

-- | Every view this module attaches: object type, view id and title.
symbolViewCatalog :: [(Text,Text,Text)]
symbolViewCatalog=[("report","hide.symbol.summary","Summary"),("report","hide.symbol.definitions","Definitions")
  ,("report","hide.symbol.references","References"),("site","hide.site.source","Source")]

-- | Views for reports and sites. Two independent contributions, each for one
-- exact type, exactly like "Hide.PackageInspect".
symbolContributions :: Either Text [Contribution]
symbolContributions=do
  report<-reportViews
  site<-siteViews
  pure [attach "hide.symbol.views" report,attach "hide.site.views" site]

publishSymbolViews :: InspectPublisher -> IO [ContributionRef]
publishSymbolViews publisher=case symbolContributions of
  Left err->ioError (userError (T.unpack err))
  Right contributions->mapM (\c->publishViews publisher c >>= either (ioError . userError . show) pure) contributions
