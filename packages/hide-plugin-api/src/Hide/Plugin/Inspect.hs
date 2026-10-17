-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE ExistentialQuantification, GADTs, RankNTypes, ScopedTypeVariables, TypeApplications, OverloadedStrings #-}
-- |
-- Module      : Hide.Plugin.Inspect
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : ExistentialQuantification, GADTs, RankNTypes, ScopedTypeVariables, TypeApplications
--
-- Typed object inspectors in the style of Glamorous Toolkit's Phlow.
--
-- A plugin /attaches/ views to an existing Haskell type without modifying it.
-- A view is a pure function from an object, with its provenance, to a semantic
-- 'Doc'. Rendering belongs to the host: nothing here mentions windows, colours
-- or geometry. Observation and identity are separate:
--
-- * __Pure observation__: an 'Edge' is a named 'Observe' (any van Laarhoven
--   getter, lens, traversal or fold) plus an injective key. It cannot perform IO.
-- * __Identity__: a 'Subject' retains its type ('Typeable'), a 'RootId', the
--   'Revision' at which it was observed and a typed path of keyed 'Step's. The
--   path, not a list index or label, identifies the object across refreshes.
--
-- Discovery is exact: views are indexed by 'SomeTypeRep' and recovered with
-- 'eqTypeRep'. There are no unsafe casts and no string dispatch. Registration
-- is scoped; withdrawing or closing refuses later use. The module never forces
-- an object's payload for type lookup. Applicability predicates may inspect
-- the value on a worker. Every 'Doc' is bounded and
-- forced by 'prepareDoc' on the caller's worker before a host may adopt it.
module Hide.Plugin.Inspect
  ( -- * Pure observations
    Observe, observe
  , Key(..), keyType
  , EdgeId, mkEdgeId, edgeIdText
  , Edge, edgeIdent, edgeTitle, edgeObserve, edgeKey, edge, checkEdge
    -- * Typed navigation
  , Step(..), Nav, navSteps, navResolve, step
  , RootId, rootIdText, Revision(..)
  , Here, hereValue, hereRoot, hereRevision, herePath, rootHere, detached, follow
  , Subject(..), subjectAs, subjectIdentity, subjectRevision, subjectType, subjectRoot
    -- * Semantic documents
  , Doc(..), Block(..), Item(..), Target(..)
  , PreparedDoc, preparedDoc, preparedHasMore, prepareDoc, preparePage, openPage
  , maxItems, maxDetail
    -- * Views
  , ViewId, mkViewId, viewIdText
  , ViewDef(..), viewDef
    -- * Registration and discovery
  , Contribution, attach, ContributionRef, contributionName
  , Inspectors, withInspectors
  , RootDef(..), Root, rootIdent
  , InspectPublisher(..), inspectPublisher
  , InspectError(..)
  , ViewInfo(..), viewsFor, applicableViews, openView
  , Resolution(..), resolve, currentRevision, rootSubject
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeAsyncException, SomeException, bracket, evaluate, fromException, throwIO, try)
import qualified Control.Category as C
import Data.Functor.Const (Const(..))
import Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import Data.Monoid (Endo(..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (Unique, newUnique)
import Type.Reflection (SomeTypeRep(..), Typeable, eqTypeRep, typeRep, (:~~:)(HRefl))
import Hide.Plugin.Command (validCommandName)
import Hide.Plugin.Identity (randomIdentity)

-- | A pure, total observation of zero or more @a@ inside an @s@. Any @lens@ or
-- @microlens@ getter, lens, traversal or fold instantiates it; no optics package
-- is required. It cannot sequence effects. 'Const' is the only functor used.
type Observe s a = (a -> Const (Endo [a]) a) -> s -> Const (Endo [a]) s

-- | /O(n)/ in the observed elements, in traversal order.
--
-- @observe (\\f -> fmap id . f) = ...@ — for a lens @l@, @observe l s = [s ^. l]@.
observe :: Observe s a -> s -> [a]
observe o s=appEndo (getConst (o (\a->Const (Endo (a:))) s)) []

-- | A typed, ordered element identity. Comparison first orders by type, then by
-- value, so keys of different types never alias.
data Key = forall k. (Typeable k, Ord k) => Key k

-- | The key's type; useful for diagnostics, never for dispatch.
keyType :: Key -> SomeTypeRep
keyType (Key (_ :: k))=SomeTypeRep (typeRep @k)

instance Eq Key where
  a==b=compare a b==EQ
instance Ord Key where
  compare (Key (x :: p)) (Key (y :: q))=case eqTypeRep (typeRep @p) (typeRep @q) of
    Just HRefl->compare x y
    Nothing->compare (SomeTypeRep (typeRep @p)) (SomeTypeRep (typeRep @q))
instance Show Key where
  show key="Key<"++show (keyType key)++">"

-- | Namespaced lowercase ASCII, following command names: @hide.package.components@.
newtype EdgeId = EdgeId Text deriving (Eq,Ord,Show)
mkEdgeId :: Text -> Either Text EdgeId
mkEdgeId name | validCommandName name=Right (EdgeId name)
              | otherwise=Left "Invalid inspector edge ID."
edgeIdText :: EdgeId -> Text
edgeIdText (EdgeId name)=name

-- | A named pure edge from @s@ to @a@.
--
-- Law: 'edgeKey' is injective over @'observe' edgeObserve s@ for every @s@
-- (distinct observed elements have distinct keys). 'checkEdge' tests it. Keys
-- should also be stable across revisions for the "same" element; that is the
-- author's responsibility and the reason keys are not list positions.
data Edge s a = Edge
  { edgeIdent :: !EdgeId, edgeTitle :: !Text
  , edgeObserve :: Observe s a, edgeKey :: a -> Key }

-- | Validate the identifier and title.
edge :: Text -> Text -> Observe s a -> (a -> Key) -> Either Text (Edge s a)
edge name title o key=do
  ident<-mkEdgeId name
  if T.null title || T.length title>128 || T.any (< ' ') title then Left "Invalid inspector edge title."
    else Right (Edge ident title o key)

-- | Does the key function distinguish every element observed in this value?
checkEdge :: Edge s a -> s -> Bool
checkEdge e s=let keys=map (edgeKey e) (observe (edgeObserve e) s) in S.size (S.fromList keys)==length keys

-- | One keyed hop in a path.
data Step = Step !EdgeId !Key deriving (Eq,Ord,Show)

-- | A typed path from a root value. Identity is its steps; 'navResolve' follows
-- them again against a possibly newer root value. @Nav@ is a 'C.Category':
--
-- @navSteps (a C.. b) = navSteps b ++ navSteps a@ and @navSteps C.id = []@.
data Nav s a = Nav ![Step] (s -> Maybe a)

navSteps :: Nav s a -> [Step]
navSteps (Nav steps _)=steps
-- | Re-resolve the path. 'Nothing' means an element with that key is gone.
navResolve :: Nav s a -> s -> Maybe a
navResolve (Nav _ resolve')=resolve'

instance C.Category Nav where
  id=Nav [] Just
  Nav later g . Nav earlier f=Nav (earlier++later) (\s->f s>>=g)

-- | The hop to the element with this key. Lookup is by key, never by index, so
-- reordering the source cannot retarget it.
step :: Edge s a -> Key -> Nav s a
step e key=Nav [Step (edgeIdent e) key] (\s->find (\a->edgeKey e a==key) (observe (edgeObserve e) s))

-- | Opaque owner scope for one registered root. Minted by the registry from
-- OS entropy; never a path, a label or a reusable name.
newtype RootId = RootId Text deriving (Eq,Ord,Show)
rootIdText :: RootId -> Text
rootIdText (RootId text)=text

-- | A freshness stamp supplied by a root's owner. Only equality is meaningful;
-- payloads are never compared to decide whether something changed.
newtype Revision = Revision Integer deriving (Eq,Ord,Show)

-- | An object together with how it was reached. The root type is existential:
-- a view is written once for @a@ and works wherever @a@ occurs.
data Here a = forall r. Typeable r => Here !RootId !Revision !(Nav r a) a

hereValue :: Here a -> a
hereValue (Here _ _ _ value)=value
hereRoot :: Here a -> RootId
hereRoot (Here root _ _ _)=root
hereRevision :: Here a -> Revision
hereRevision (Here _ revision _ _)=revision
-- | Root and path: the object's retained identity.
herePath :: Here a -> (RootId,[Step])
herePath (Here root _ nav _)=(root,navSteps nav)

-- | The root object itself. It resolves only if a root with this ID is registered.
rootHere :: Typeable r => RootId -> Revision -> r -> Here r
rootHere root revision value=Here root revision C.id value

-- | An object with no root, for building views of literals and in tests. Its
-- root never resolves: re-resolution answers 'RootRetired'.
detached :: Typeable r => r -> Here r
detached=rootHere (RootId "detached") (Revision 0)

-- | One 'Here' per element observed through the edge, each with its key
-- appended to the path.
follow :: Edge a b -> Here a -> [Here b]
follow e (Here root revision nav value)=
  [Here root revision (step e (edgeKey e b) C.. nav) b | b<-observe (edgeObserve e) value]

-- | A type-erased 'Here' that still carries its 'Typeable' witness. Equality is
-- identity: same root and path, regardless of revision or payload.
data Subject = forall a. Typeable a => Subject (Here a)

subjectIdentity :: Subject -> (RootId,[Step])
subjectIdentity (Subject here)=herePath here
subjectRevision :: Subject -> Revision
subjectRevision (Subject here)=hereRevision here
subjectRoot :: Subject -> RootId
subjectRoot (Subject here)=hereRoot here
subjectType :: Subject -> SomeTypeRep
subjectType (Subject (_ :: Here a))=SomeTypeRep (typeRep @a)

-- | Recover a typed object when the subject is exactly a @b@. The comparison is
-- by type representation; a different type is simply Nothing.
subjectAs :: forall b. Typeable b => Subject -> Maybe (Here b)
subjectAs (Subject (here :: Here a))=case eqTypeRep (typeRep @a) (typeRep @b) of
  Just HRefl->Just here
  Nothing->Nothing

instance Eq Subject where
  a==b=subjectIdentity a==subjectIdentity b
instance Ord Subject where
  compare a b=compare (subjectIdentity a) (subjectIdentity b)
instance Show Subject where
  show subject="Subject "++show (subjectType subject)++" "++show (subjectIdentity subject)

-- | Semantic description of what to show. The host chooses the surface:
-- 'Text' becomes a read-only text window, 'List' a selectable list with Details.
-- The item spine is deliberately lazy: preparing a page inspects only its
-- prefix and one lookahead cell. Object payloads in Here are also deliberately
-- lazy, so type discovery never evaluates them.
data Doc = Text [Block] | List !Text [Item] deriving (Eq,Show)
data Block = Heading !Text | Prose !Text | Facts [(Text,Text)] deriving (Eq,Show)
-- | A list row: a caption, preformatted Details and an optional navigation target.
data Item = Item { itemLabel :: !Text, itemDetail :: !Text, itemTarget :: !(Maybe Target) } deriving (Eq,Show)
-- | Navigation inside the inspector, or a file for the host to open through
-- its own admission. A file path is a request, never authority.
--
-- 'OpenAt' names a 1-based line, a 1-based column in Unicode code points and the
-- text of that line when the object was observed. The host jumps only if that
-- line still reads the same, so a location can go stale but can never silently
-- select unrelated text.
data Target = Inspect !Subject | PageAt !Subject !ViewId !Int | OpenFile !FilePath | OpenAt !FilePath !Int !Int !Text deriving (Eq,Show)

-- | A document that 'prepareDoc' bounded, sanitized and forced.
data PreparedDoc = PreparedDoc !Doc !Bool
preparedDoc :: PreparedDoc -> Doc
preparedDoc (PreparedDoc doc _)=doc
-- | Whether another list page exists. No discarded tail is counted.
preparedHasMore :: PreparedDoc -> Bool
preparedHasMore (PreparedDoc _ more)=more

-- | Leave room for navigation and view controls in a 64-row window.
maxItems, maxDetail :: Int
maxItems=48
maxDetail=16384

-- | Bound and force only the displayed document, with one list-cell lookahead.
-- Captions are single-line, details retain newlines. Source validation text is
-- exact and must already fit the source-line limit; it is never sanitized.
prepareDoc :: Doc -> IO (Either InspectError PreparedDoc)
prepareDoc doc=do
  outcome<-contained $ do
    prepared<-evaluate (bound doc)
    _<-evaluate (weigh prepared)
    pure prepared
  pure (either (Left . ViewFailed) Right outcome)
  where
    bound (Text blocks)=PreparedDoc (Text (map block (take 48 blocks))) False
    bound (List title items)=let (kept,rest)=splitAt maxItems items
      in PreparedDoc (List (line 240 title) (map item kept)) (not (null rest))
    block (Heading text)=Heading (line 240 text)
    block (Prose text)=Prose (multi maxDetail text)
    block (Facts facts)=Facts [(line 128 k,line 4096 v) | (k,v)<-take 128 facts]
    item (Item label detail target)=Item (line 240 label) (multi maxDetail detail) (fmap pathOk target)
    pathOk (OpenFile path)=OpenFile (take 32768 (filter (/='\NUL') path))
    pathOk target@(OpenAt path _ _ expected)
      | length (take 32769 path)>32768 || T.length expected>4096=error "Source location exceeds inspector bounds."
      | otherwise=target
    pathOk other=other
    line n=T.copy . T.map blank . T.take n
    multi n=T.copy . T.map (\c->if c=='\n' then c else blank c) . T.take n
    blank c=if c<' ' || c=='\DEL' then ' ' else c
    weigh (PreparedDoc d _)=case d of
      Text blocks->sum (map weighBlock blocks)
      List title items->T.length title+sum [T.length l+T.length x+weighTarget t | Item l x t<-items]
    weighBlock (Heading t)=T.length t
    weighBlock (Prose t)=T.length t
    weighBlock (Facts fs)=sum [T.length k+T.length v | (k,v)<-fs]
    weighTarget (Just (OpenFile path))=length path
    weighTarget (Just (OpenAt path row column expected))=length path+row+column+T.length expected
    weighTarget (Just (Inspect subject))=weighSubject subject
    weighTarget (Just (PageAt subject view offset))=weighSubject subject+T.length (viewIdText view)+offset
    weighTarget Nothing=0
    weighSubject subject=let (root,steps)=subjectIdentity subject in T.length (rootIdText root)+sum (map keyDepth steps)
    keyDepth (Step ident key)=T.length (edgeIdText ident)+(case key of Key k->compare k k `seq` 1)

-- | Prepare one page, adding typed continuation targets for this exact revision.
-- Seeking costs /O(offset)/ list cells; preparation never walks the suffix.
preparePage :: Subject -> ViewId -> Int -> Doc -> IO (Either InspectError PreparedDoc)
preparePage subject view offset doc
  | offset<0=pure (Left (ViewFailed "Invalid inspector page."))
  | otherwise=do
      ready<-prepareDoc (case doc of List title items->List title (drop offset items); other->other)
      pure $ fmap (\(PreparedDoc body more)->PreparedDoc (case body of
        List title items->List title (items++
          [Item "← Previous page" "Show the preceding entries." (Just (PageAt subject view (max 0 (offset-maxItems)))) | offset>0]++
          [Item "More…" "Show the next entries." (Just (PageAt subject view (offset+maxItems))) | more])
        other->other) more) ready

-- | Resolve a current view. A continuation refuses a changed root revision, so
-- a list edit cannot silently change the meaning of the requested page.
openPage :: Inspectors -> Subject -> ViewId -> Int -> IO (Either InspectError PreparedDoc)
openPage inspectors subject wanted offset=do
  current<-resolve inspectors subject
  case current of
    Fresh fresh | offset>0 && subjectRevision fresh/=subjectRevision subject->pure (Left (ViewFailed "The source changed; reopen the inspector."))
    Fresh fresh->do
      available<-applicableViews inspectors fresh
      if wanted `notElem` map infoView available then pure (Left (UnknownView (viewIdText wanted))) else do
        body<-openView inspectors fresh wanted
        either (pure . Left) (preparePage fresh wanted offset) body
    _->pure (Left (ViewFailed "The inspected object is no longer available."))

-- | Run IO while retaining asynchronous cancellation semantics.
contained :: IO a -> IO (Either Text a)
contained action=do
  result<-try action
  case result of
    Right value->pure (Right value)
    Left exception | Just (_ :: SomeAsyncException)<-fromException exception->throwIO exception
                   | otherwise->pure (Left (T.pack (show (exception :: SomeException))))

-- | Namespaced like commands: @hide.package.components@.
newtype ViewId = ViewId Text deriving (Eq,Ord,Show)
mkViewId :: Text -> Either Text ViewId
mkViewId name | validCommandName name=Right (ViewId name)
              | otherwise=Left "Invalid inspector view ID."
viewIdText :: ViewId -> Text
viewIdText (ViewId name)=name

-- | A view of @a@. 'viewApplies' is a cheap predicate evaluated only on a
-- worker; 'viewBody' must be pure. Order breaks ties by 'ViewId'.
data ViewDef a = ViewDef
  { viewIdent :: !ViewId, viewTitle :: !Text, viewOrder :: !Int
  , viewApplies :: a -> Bool, viewBody :: Here a -> Doc }

-- | An always-applicable view with a validated ID and title.
viewDef :: Text -> Text -> Int -> (Here a -> Doc) -> Either Text (ViewDef a)
viewDef name title order body=do
  ident<-mkViewId name
  if T.null title || T.length title>128 || T.any (< ' ') title then Left "Invalid inspector view title."
    else Right (ViewDef ident title order (const True) body)

-- | Views for one type, attached by a named contribution.
data Contribution = forall a. Typeable a => Contribution Text [ViewDef a]

-- | @attach name views@: @name@ follows command-name grammar.
attach :: Typeable a => Text -> [ViewDef a] -> Contribution
attach=Contribution

data ContributionRef = ContributionRef Unique Integer Text deriving Eq
instance Show ContributionRef where
  show (ContributionRef _ generation name)="ContributionRef "++show name++" "++show generation
contributionName :: ContributionRef -> Text
contributionName (ContributionRef _ _ name)=name

-- | The current value of a root and its revision, or 'Nothing' when the owner
-- no longer has it. Must be cheap and must not force the value.
data RootDef r = RootDef { rootDefName :: Text, rootRead :: IO (Maybe (Revision,r)) }

-- | A registered root handle.
data Root r = Root RootId (IO (Maybe (Revision,r)))
rootIdent :: Root r -> RootId
rootIdent (Root ident _)=ident

data SomeRoot = forall r. Typeable r => SomeRoot (IO (Maybe (Revision,r)))
data Entry = forall a. Typeable a => Entry Integer Text (ViewDef a)
-- Closed flag, generation, views by exact type, contribution generations, used view IDs, roots.
data State = State !Bool !Integer !(M.Map SomeTypeRep [Entry]) !(M.Map Text Integer) !(S.Set ViewId) !(M.Map RootId SomeRoot)
closedState :: State -> Bool
closedState (State closed _ _ _ _ _)=closed
byType :: State -> M.Map SomeTypeRep [Entry]
byType (State _ _ index _ _ _)=index
roots :: State -> M.Map RootId SomeRoot
roots (State _ _ _ _ _ rs)=rs
data Inspectors = Inspectors Unique (MVar State)

-- | Scope registrations. Closing refuses discovery and later publication.
withInspectors :: (Inspectors -> IO a) -> IO a
withInspectors=bracket acquire close
  where
    acquire=Inspectors <$> newUnique <*> newMVar (State False 0 M.empty M.empty S.empty M.empty)
    close (Inspectors _ state)=modifyMVar_ state (\(State _ generation _ _ _ _)->pure (State True generation M.empty M.empty S.empty M.empty))

data InspectError = InspectorsClosed | InvalidContribution Text | DuplicateContribution Text
  | DuplicateView Text | InspectLimit | InvalidRoot Text | UnknownView Text | StaleContribution Text
  | ViewFailed Text
  deriving (Eq,Show)

-- | Publish views for one type. A view ID may appear once in the registry.
publishViews' :: Inspectors -> Contribution -> IO (Either InspectError ContributionRef)
publishViews' (Inspectors owner state) (Contribution name views)
  | not (validCommandName name)=pure (Left (InvalidContribution name))
  | null views || length (take 65 views)>64=pure (Left InspectLimit)
  | S.size ids/=length views=pure (Left (DuplicateView "duplicate view in contribution"))
  | otherwise=modifyMVar state $ \current@(State closed generation index known used rs)->
      if closed then pure (current,Left InspectorsClosed)
      else if M.member name known then pure (current,Left (DuplicateContribution name))
      else if M.size known>=256 then pure (current,Left InspectLimit)
      else case S.toList (S.intersection ids used) of
        ViewId clash:_->pure (current,Left (DuplicateView clash))
        []->let next=generation+1
                entries=[Entry next name view | view<-views]
            in pure (State False next (M.insertWith (flip (++)) key entries index) (M.insert name next known) (S.union ids used) rs
                    ,Right (ContributionRef owner next name))
  where
    ids=S.fromList (map viewIdent views)
    key=viewsKey views
    viewsKey :: forall a. Typeable a => [ViewDef a] -> SomeTypeRep
    viewsKey _=SomeTypeRep (typeRep @a)

withdrawViews' :: Inspectors -> ContributionRef -> IO ()
withdrawViews' (Inspectors owner state) (ContributionRef ident generation name)=
  modifyMVar_ state $ \current@(State closed gen index known used rs)->
    if ident/=owner || closed || M.lookup name known/=Just generation then pure current else do
      let mine (Entry g n _)=g==generation && n==name
          freed=S.fromList [viewIdent view | entries<-M.elems index,Entry g n view<-entries,g==generation && n==name]
          index'=M.filter (not . null) (M.map (filter (not . mine)) index)
      pure (State False gen index' (M.delete name known) (S.difference used freed) rs)

-- | Register a root and mint its fresh identity.
registerRoot :: Typeable r => Inspectors -> RootDef r -> IO (Either InspectError (Root r))
registerRoot (Inspectors _ state) (RootDef name reader)
  | not (validCommandName name)=pure (Left (InvalidRoot name))
  | otherwise=do
      scope<-T.pack <$> randomIdentity
      let ident=RootId (name<>"#"<>scope)
      modifyMVar state $ \current@(State closed generation index known used rs)->
        if closed then pure (current,Left InspectorsClosed)
        else if M.size rs>=256 then pure (current,Left InspectLimit)
        else pure (State False generation index known used (M.insert ident (SomeRoot reader) rs),Right (Root ident reader))

withdrawRoot' :: Inspectors -> RootId -> IO ()
withdrawRoot' (Inspectors _ state) ident=modifyMVar_ state (\(State closed generation index known used rs)->pure (State closed generation index known used (M.delete ident rs)))

-- | Capabilities handed to plugins through the session. Both publications block
-- only on the registry's short lock; neither calls plugin code.
data InspectPublisher = InspectPublisher
  { publishViews :: Contribution -> IO (Either InspectError ContributionRef)
  , withdrawViews :: ContributionRef -> IO ()
  , publishRoot :: forall r. Typeable r => RootDef r -> IO (Either InspectError (Root r))
  , withdrawRoot :: forall r. Root r -> IO ()
  }

inspectPublisher :: Inspectors -> InspectPublisher
inspectPublisher inspectors=InspectPublisher (publishViews' inspectors) (withdrawViews' inspectors)
  (\def->registerRoot inspectors def) (\(Root ident _)->withdrawRoot' inspectors ident)

-- | The root's present subject, or 'Nothing' if the root is withdrawn or empty.
rootSubject :: Typeable r => Inspectors -> Root r -> IO (Maybe Subject)
rootSubject (Inspectors _ state) (Root ident reader)=do
  live<-withMVar state (\s->pure (not (closedState s) && M.member ident (roots s)))
  if not live then pure Nothing else fmap (\(revision,value)->Subject (rootHere ident revision value)) <$> reader

-- | Discovery metadata. Reading it never evaluates the object.
data ViewInfo = ViewInfo { infoView :: !ViewId, infoTitle :: !Text, infoOrder :: !Int } deriving (Eq,Show)

-- | Every view attached to exactly this type, ordered by @(order, ID)@. It does
-- not evaluate the subject or any 'viewApplies'.
viewsFor :: Inspectors -> Subject -> IO [ViewInfo]
viewsFor (Inspectors _ state) (Subject (_ :: Here a))=withMVar state $ \s->pure $
  if closedState s then [] else sortOn (\i->(infoOrder i,infoView i))
    [ViewInfo (viewIdent view) (viewTitle view) (viewOrder view)
    | Entry _ _ view<-M.findWithDefault [] (SomeTypeRep (typeRep @a)) (byType s)]

-- | As 'viewsFor', keeping views whose predicate holds. A predicate that throws
-- is treated as not applicable. Worker only: it may inspect the payload.
applicableViews :: Inspectors -> Subject -> IO [ViewInfo]
applicableViews (Inspectors _ state) (Subject (here :: Here a))=do
  entries<-withMVar state $ \s->pure (if closedState s then [] else M.findWithDefault [] (SomeTypeRep (typeRep @a)) (byType s))
  applicable<-mapM (\(Entry _ _ (view :: ViewDef b))->case eqTypeRep (typeRep @a) (typeRep @b) of
      Just HRefl->do
        outcome<-contained (evaluate (viewApplies view (hereValue here)))
        pure [ViewInfo (viewIdent view) (viewTitle view) (viewOrder view) | Right True<-[outcome]]
      Nothing->pure []) entries
  pure (sortOn (\i->(infoOrder i,infoView i)) (concat applicable))

-- | Build one named view of this subject's exact type. The view body is pure;
-- it is evaluated here to weak head normal form and failures are contained.
openView :: Inspectors -> Subject -> ViewId -> IO (Either InspectError Doc)
openView (Inspectors _ state) (Subject (here :: Here a)) ident=do
  entries<-withMVar state $ \s->pure (if closedState s then Nothing else Just (M.findWithDefault [] (SomeTypeRep (typeRep @a)) (byType s)))
  case entries of
    Nothing->pure (Left InspectorsClosed)
    Just found->case filter (\(Entry _ _ view)->viewIdent view==ident) found of
      Entry _ _ (view :: ViewDef b):_->case eqTypeRep (typeRep @a) (typeRep @b) of
        Just HRefl->either (Left . ViewFailed) Right <$> contained (evaluate (viewBody view here))
        Nothing->pure (Left (UnknownView (viewIdText ident)))
      []->pure (Left (UnknownView (viewIdText ident)))

-- | Outcome of following a subject's path against its root's current value.
data Resolution = Fresh Subject | Gone | RootRetired

-- | Re-resolve through the typed path. The path is followed by key, so a
-- reorder keeps the same element and a deletion gives 'Gone', never another
-- object. A root whose type differs from the path's root type is also 'Gone'.
resolve :: Inspectors -> Subject -> IO Resolution
resolve (Inspectors _ state) (Subject (Here ident _ (nav :: Nav r a) _))=do
  found<-withMVar state $ \s->pure (if closedState s then Nothing else M.lookup ident (roots s))
  case found of
    Nothing->pure RootRetired
    Just (SomeRoot (reader :: IO (Maybe (Revision,r'))))->case eqTypeRep (typeRep @r) (typeRep @r') of
      Nothing->pure Gone
      Just HRefl->do
        current<-reader
        pure $ case current of
          Nothing->RootRetired
          Just (revision,value)->maybe Gone (\a->Fresh (Subject (Here ident revision nav a))) (navResolve nav value)

-- | /O(1)/ besides the owner's cheap read: the root's current revision, without
-- forcing its value. Hosts compare this scalar to decide whether to re-resolve.
currentRevision :: Inspectors -> Subject -> IO (Maybe Revision)
currentRevision (Inspectors _ state) subject=do
  found<-withMVar state $ \s->pure (if closedState s then Nothing else M.lookup (subjectRoot subject) (roots s))
  case found of
    Nothing->pure Nothing
    Just (SomeRoot reader)->fmap fst <$> reader
