{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.ConversationBody
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Immutable transcript preparation inputs and exact host body receipts.
-- This module owns no provider, Desktop, editable Buffer, callbacks or workers.
-- TextPresentation's existing serial worker will consume these closed requests;
-- Conversation retains task/control authority and adopts their exact results.
module Hide.ConversationBody
  ( BodyItemId(..), Record(..), RecordContent(..), BodyPoint(..), BodyAnchor(..), BodySelection(..), BodyDemand(..), BodyViewport(..), BodyRow(..), viewportPoint, viewportOffset
  , LogicalBody, LogicalItem, logicalBodyIdentity, logicalBodyProvider, logicalBodyTranscriptIdentity, logicalBodyRead, validateLogicalPoint, clampPoint, logicalBodyItems, logicalBodyItemIndex, logicalItemRecord, logicalItemMarkdown, logicalItemBlocks, prepareLogicalBody, restoreLogicalBody, restoreLogicalViewport
  , ConversationCopy(..), logicalBodyCopy
  , ToolExpansion(..), QuestionSchema(..), QuestionProjection(..)
  , BodyProvider(..), BodyKey(..), bodyOwnerMatches, BodyInput(..)
  , CapturedConversationSource, captureConversationSource, capturedSourceIdentity, prepareCapturedSource
  , BodyRequest(..), BodyResult(..), PreparedBody(..), HostBodyControls(..)
  , BodyControlReceipt(..), ConversationBody(..)
  , prepareConversationBody, renderReply, renderReplyWithShellBlocks, renderTimestamp, questionChoiceLines
  ) where

import Control.Exception (evaluate)
import Data.Aeson (Value,FromJSON,ToJSON,encode,withObject,(.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Set as S
import System.FilePath ((</>))
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Data.List (mapAccumL,find)
import qualified Data.List as List
import Data.Unique (Unique,newUnique,hashUnique)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Hide.Buffer (BufferContent,newBuffer,bufferContent,columnOffset,displayColumn,lineColumn)
import Hide.Markdown (Markdown,MarkdownBlock,parseMarkdown,markdownBlocks,markdownBlockText,markdownBlockLinks,markdownBlockShell,markdownIntrinsicWidth,renderMarkdownBlock,renderMarkdownWithShellBlocks)
import Hide.Syntax (Style(..),StyledText,StyledRow(..),MappedStyledRow(..),Sigils(..),styledText,styledContents,styledLength,splitStyledText,styledRows,sigilsColumn,sigilsLength,mapSigilsStyle,bubbleTile)
import Data.Text (Text)
import Data.Set (Set)
import System.Mem.StableName (StableName,makeStableName)
import qualified Hide.ACP as A
import qualified Hide.AgentHub as AH
import qualified Hide.Plugin.Window as W
import Hide.TextLayout (TextLayout,prepareTextLayout)

-- Item identity is allocated by the transcript/event owner, never rendering.
-- It survives chunk append/tool update and grants no provider/input authority.
newtype BodyItemId = BodyItemId Int deriving (Eq,Ord,Show)
data Record = Record
  { recordId :: !BodyItemId, recordRevision :: !Int, recordContent :: !RecordContent
  } deriving (Eq,Show)
data RecordContent = Reply Text Text | Activity Text Value [Value] | Pause Text deriving (Eq,Show)

-- Positions belong to the target's logical catalogue, never a painted row.
-- Missing item IDs cannot be redirected to an unrelated surviving item.
data BodyPoint = BodyPoint !BodyItemId !Int !Int | QuestionPoint !Int !Int !Int deriving (Eq,Show)
instance Ord BodyPoint where
  compare a b=compare (order a) (order b)
    where
      order (BodyPoint (BodyItemId ident) block scalar)=(0::Int,ident,0::Int,block,scalar)
      order (QuestionPoint token block scalar)
        | block>=0=(1,token,0,block,scalar)
        | block<=(-3)=(1,token,1,(-3)-block,scalar)
        | block==(-1)=(1,token,2,0,scalar)
        | otherwise=(1,token,3,0,scalar)
data BodyAnchor = At !BodyPoint | FollowEnd | WithinItem !BodyItemId !Int deriving (Eq,Show)
data BodySelection = BodySelection !BodyPoint !BodyPoint deriving (Eq,Show)

-- A signed row request is relative to the containing logical row, never an
-- accumulated rendered height. Adoption replaces it with the resulting anchor.
data BodyDemand = BodyDemand !BodyAnchor !Int !Int deriving (Eq,Show)
data BodyRow = BodyRow
  { bodyRowPoint :: !(Maybe BodyPoint), bodyRowLogicalEnd :: !Int
  , bodyRowPaintStart :: !Int, bodyRowPaintEnd :: !Int
  , bodyRowRanges :: !(V.Vector (Int,Int,Int,Int))
  } deriving Show
data BodyViewport = BodyViewport
  { viewportRows :: !(V.Vector BodyRow), viewportAnchor :: !BodyAnchor
  , viewportScroll :: !Int, viewportDemandRow :: !Int, viewportAtEnd :: !Bool, viewportProgress :: !(Int,Int)
  } deriving Show

-- | /O(demanded rows + row spans)/. Convert only through the authoritative
-- bounded receipt; furniture uses its nearest surviving logical boundary.
viewportPoint :: BodyViewport -> Int -> Maybe BodyPoint
viewportPoint viewport offset=do
  row<-find (\r->offset>=bodyRowPaintStart r && offset<=bodyRowPaintEnd r) (V.toList (viewportRows viewport))
  point<-bodyRowPoint row
  let paint=max 0 (offset-bodyRowPaintStart row)
      spans=V.toList (bodyRowRanges row)
      scalar=case find (\(a,z,_,_)->paint>=a && paint<z) spans of
        Just (a,z,start,end)->start+(paint-a)*(end-start) `div` max 1 (z-a)
        Nothing->case takeWhile (\(a,_,_,_)->a<=paint) spans of
          []->pointScalar point
          before->let (_,_,_,end)=last before in end
  pure (withScalar scalar point)

-- | /O(demanded rows + spans)/. Tables may revisit a header's canonical range;
-- choose its first visible occurrence, never assume globally monotone offsets.
viewportOffset :: BodyViewport -> BodyPoint -> Maybe Int
viewportOffset viewport point=do
  row<-find matches rows <|> find ends (reverse rows)
  let scalar=pointScalar point
      offset=case find (\(_,_,a,z)->scalar>=a && scalar<z) (V.toList (bodyRowRanges row)) of
        Just (start,end,a,z)->start+(scalar-a)*(end-start) `div` max 1 (z-a)
        Nothing->if scalar<=maybe 0 pointScalar (bodyRowPoint row) then 0 else bodyRowPaintEnd row-bodyRowPaintStart row
  pure (bodyRowPaintStart row+offset)
  where
    rows=V.toList (viewportRows viewport)
    matches row=case bodyRowPoint row of
      Just first->sameBlock first point && pointScalar point>=pointScalar first && pointScalar point<bodyRowLogicalEnd row
      Nothing->False
    ends row=case bodyRowPoint row of
      Just first->sameBlock first point && pointScalar point==bodyRowLogicalEnd row
      Nothing->False
pointScalar :: BodyPoint -> Int
pointScalar (BodyPoint _ _ scalar)=scalar
pointScalar (QuestionPoint _ _ scalar)=scalar
withScalar :: Int -> BodyPoint -> BodyPoint
withScalar scalar (BodyPoint item block _)=BodyPoint item block scalar
withScalar scalar (QuestionPoint token block _)=QuestionPoint token block scalar
sameBlock :: BodyPoint -> BodyPoint -> Bool
sameBlock (BodyPoint item block _) (BodyPoint other next _)=item==other && block==next
sameBlock (QuestionPoint token block _) (QuestionPoint other next _)=token==other && block==next
sameBlock _ _=False

-- The record and its exact payload identity are strict metadata. Markdown is
-- lazy: looking up an item or constructing this vector parses no prior items.
data LogicalParsed = LogicalParsed !Markdown !(V.Vector MarkdownBlock) Int
data LogicalItem = LogicalItem !Record !(StableName Record) (Maybe LogicalParsed) !(Int,Bool) (V.Vector [MappedStyledRow])
-- One width's lazy block rows belong to this immutable item. Replacing width
-- drops the old derived roots without parsing or laying out untouched items.
data LogicalBody = LogicalBody
  { logicalIdentity :: !Unique, logicalTarget :: !Text
  , logicalProvider :: !BodyProvider, logicalTranscript :: !(StableName [Record])
  , logicalSession :: !(Maybe Text), logicalQuestion :: !(Maybe QuestionSchema)
  , logicalWidth :: !(Int,Bool), logicalItems :: !(V.Vector LogicalItem)
  }
instance Eq LogicalBody where a==b=logicalIdentity a==logicalIdentity b
instance Show LogicalBody where show body="LogicalBody "++show (hashUnique (logicalIdentity body))

-- | /O(1)/. Width and viewport changes preserve this exact identity. A changed
-- catalogue receives a fresh worker-issued identity, never a payload comparison.
logicalBodyIdentity :: LogicalBody -> Unique
logicalBodyIdentity=logicalIdentity
logicalBodyProvider :: LogicalBody -> BodyProvider
logicalBodyProvider=logicalProvider
logicalBodyTranscriptIdentity :: LogicalBody -> StableName [Record]
logicalBodyTranscriptIdentity=logicalTranscript

-- | Validate only the named item/block on a worker. Recovery never accepts a
-- transient question point or redirects a missing source to another item.
validateLogicalPoint :: LogicalBody -> BodyPoint -> Either Text ()
validateLogicalPoint _ QuestionPoint{}=Left "Transient question positions cannot be recovered."
validateLogicalPoint body (BodyPoint ident block scalar)=do
  index<-maybe (Left "Conversation position names a missing item.") Right (logicalBodyItemIndex ident body)
  let item=logicalItems body V.! index
      source=case recordContent (logicalItemRecord item) of
        Reply _ _->markdownBlockText <$> (logicalItemBlocks item V.!? block)
        Activity label value history | block==0->Just (activityTitle label value<>"\n"<>T.intercalate "\n" (map jsonText history))
        Pause label | block==0->Just label
        _->Nothing
  text<-maybe (Left "Conversation position names a missing block.") Right source
  if scalar>=0 && scalar<=T.length text then Right () else Left "Conversation position is outside its logical block."

-- Source replacement may shorten an existing item. Normalize only its named
-- endpoints on the worker; missing identities are never redirected elsewhere.
clampPoint :: LogicalBody -> BodyPoint -> Maybe BodyPoint
clampPoint body (BodyPoint ident block scalar)=do
  index<-logicalBodyItemIndex ident body
  let texts=itemBlockTexts (logicalItems body V.! index)
      selected=max 0 (min (max 0 (V.length texts-1)) block)
      size=maybe 0 T.length (texts V.!? selected)
  pure (BodyPoint ident selected (max 0 (min size (if selected==block then scalar else maxBound))))
clampPoint body point@(QuestionPoint token _ _)=case logicalQuestion body of
  Just (QuestionSchema current _ _) | token==current->Just point
  _->Nothing

-- | /O(1)/. Borrow ordered items. Their parser owners remain lazy.
logicalBodyItems :: LogicalBody -> V.Vector LogicalItem
logicalBodyItems=logicalItems
logicalItemRecord :: LogicalItem -> Record
logicalItemRecord (LogicalItem record _ _ _ _)=record
logicalItemMarkdown :: LogicalItem -> Maybe Markdown
logicalItemMarkdown (LogicalItem _ _ parsed _ _)=case parsed of
  Nothing->Nothing
  Just (LogicalParsed markdown _ _)->Just markdown

-- | Parse only this item's immutable source on the preparation/read worker.
-- Block metadata is cached, so a selected block is indexed without replaying
-- preceding blocks or laying out their rows.
logicalItemBlocks :: LogicalItem -> V.Vector MarkdownBlock
logicalItemBlocks (LogicalItem _ _ parsed _ _)=case parsed of
  Nothing->V.empty
  Just (LogicalParsed _ blocks _)->blocks
logicalItemWidth :: LogicalItem -> Int
logicalItemWidth (LogicalItem _ _ parsed _ _)=case parsed of Nothing->1; Just (LogicalParsed _ _ intrinsic)->intrinsic

-- | /O(log items)/. Insertion/event IDs are strictly ordered within one target.
-- Missing IDs return Nothing, never a neighboring item's source position.
logicalBodyItemIndex :: BodyItemId -> LogicalBody -> Maybe Int
logicalBodyItemIndex wanted body=search 0 (V.length items)
  where
    items=logicalItems body
    search low high
      | low>=high=Nothing
      | ident==wanted=Just middle
      | ident<wanted=search (middle+1) high
      | otherwise=search low middle
      where
        middle=low+(high-low) `div` 2
        ident=recordId (logicalItemRecord (items V.! middle))

-- | Update immutable logical ownership on the existing presentation worker.
-- Reuse requires exact provider, item and immutable record identities; numeric
-- revisions or equal text alone cannot prove the cached parser still applies.
prepareLogicalBody :: BodyKey -> BodyInput -> Maybe LogicalBody -> IO LogicalBody
prepareLogicalBody key input previous=do
  body<-captureLogicalBody (bodyTarget key) (bodyProvider key) (bodyTranscript key)
    (bodySession input) (bodyQuestion input) (bodyRecords input) previous
  withLogicalWidth (bodyWide key) (max 1 (bodyColumns key-5)) body

-- Latest received immutable source is durable before any viewport job finishes.
-- Its identity is independent of layout, and capturing it parses no item.
data CapturedConversationSource = CapturedConversationSource !Unique !Text !BodyProvider !(StableName [Record]) ![Record]
instance Eq CapturedConversationSource where
  CapturedConversationSource a _ _ _ _==CapturedConversationSource b _ _ _ _=a==b
instance Show CapturedConversationSource where
  show (CapturedConversationSource ident _ _ _ _)="CapturedConversationSource "++show (hashUnique ident)

capturedSourceIdentity :: CapturedConversationSource -> Unique
capturedSourceIdentity (CapturedConversationSource ident _ _ _ _)=ident

captureConversationSource :: Text -> BodyProvider -> [Record] -> Maybe CapturedConversationSource -> IO CapturedConversationSource
captureConversationSource target provider records previous=do
  root<-makeStableName =<< evaluate records
  case previous of
    Just source@(CapturedConversationSource _ sameTarget sameProvider sameRoot _) | target==sameTarget && provider==sameProvider && root==sameRoot->pure source
    _->do
      ident<-newUnique
      evaluate (CapturedConversationSource ident target provider root records)

-- Checkpoint work uses the same lazy parser catalogue as presentation. This
-- omits transient session/question state and never requests painted rows.
prepareCapturedSource :: CapturedConversationSource -> Maybe LogicalBody -> IO LogicalBody
prepareCapturedSource (CapturedConversationSource _ target provider root records) previous=
  captureLogicalBody target provider root Nothing Nothing records previous

captureLogicalBody :: Text -> BodyProvider -> StableName [Record] -> Maybe Text -> Maybe QuestionSchema -> [Record] -> Maybe LogicalBody -> IO LogicalBody
captureLogicalBody target provider root session question records previous=case previous of
    Just body | logicalTarget body==target && logicalProvider body==provider &&
      logicalTranscript body==root && questionTokenOf (logicalQuestion body)==questionTokenOf question->pure body
    _->do
      identity<-newUnique
      let old=case previous of
            Just body | logicalTarget body==target && logicalProvider body==provider->M.fromList
              [(recordId (logicalItemRecord item),item) | item<-V.toList (logicalItems body)]
            _->M.empty
      items<-V.fromList <$> mapM (capture old) records
      evaluate (LogicalBody identity target provider root session question (0,False) items)
  where
    questionTokenOf Nothing=Nothing
    questionTokenOf (Just (QuestionSchema token _ _))=Just token
    capture old record=do
      payload<-makeStableName =<< evaluate record
      case M.lookup (recordId record) old of
        Just item@(LogicalItem _ same _ _ _) | same==payload->pure item
        _->evaluate (LogicalItem record payload (case recordContent record of
          Reply _ text->let parsed=parseMarkdown text in Just (LogicalParsed parsed (V.fromList (markdownBlocks parsed)) (markdownIntrinsicWidth parsed))
          _->Nothing) (0,False) V.empty)

withLogicalWidth :: Bool -> Int -> LogicalBody -> IO LogicalBody
withLogicalWidth wide columns body
  | logicalWidth body==(columns,wide)=pure body
  | otherwise=do
      items<-V.mapM resize (logicalItems body)
      evaluate body {logicalWidth=(columns,wide),logicalItems=items}
  where
    resize item@(LogicalItem record payload parsed width _)
      | width==(columns,wide)=pure item
      | otherwise=evaluate (LogicalItem record payload parsed (columns,wide) (case parsed of
          Nothing->V.empty
          Just (LogicalParsed _ blocks _)->V.map (renderMarkdownBlock wide columns) blocks))

logicalItemRows :: Bool -> Int -> LogicalItem -> Int -> [MappedStyledRow]
logicalItemRows wide columns item@(LogicalItem _ _ _ width rows) block
  | width==(columns,wide)=fromMaybe [] (rows V.!? block)
  | otherwise=maybe [] (renderMarkdownBlock wide columns) (logicalItemBlocks item V.!? block)

-- | Project full canonical logical text on a read worker, independently of
-- folding and viewport rows. True means privacy redaction was applied. Session
-- headers and question choices/input retain their existing exclusions; no live
-- answer, approval or input/action capability belongs to this catalogue.
logicalBodyRead :: LogicalBody -> IO (Bool,BufferContent)
logicalBodyRead body=do
  text<-evaluate (T.intercalate "\n\n" (records++question))
  content<-evaluate (bufferContent (newBuffer text))
  pure (redacted,content)
  where
    records
      | V.null (logicalItems body),maybe True (const False) (logicalQuestion body)=
          [if T.null (logicalTarget body) then hide ("Session: "<>fromMaybe "not connected" (logicalSession body))<>"\n" else "No messages yet.\n"]
      | otherwise=map itemText (V.toList (logicalItems body))
    itemText item=case recordContent (logicalItemRecord item) of
      Reply _ text->maybe (canonical (parseMarkdown text)) canonical (logicalItemMarkdown item)
      Pause label->label
      -- Both live provider producers scrub this immutable activity history
      -- before insertion; use exactly the existing expanded-display payload.
      Activity ident value history->
        activityTitle ident value<>"\n"<>T.intercalate "\n" (map jsonText history)
    question=case logicalQuestion body of
      Nothing->[]
      Just (QuestionSchema _ prompt choices)->
        [canonical (parseMarkdown prompt)<>"\n"<>T.intercalate "\n" (map hide choices)<>"\nOther:  \n[Submit answer]  [Cancel]"]
    canonical=T.concat . map markdownBlockText . markdownBlocks
    hide=T.map (\c->if c=='\n' || c=='\r' then c else ' ')
    redacted=case logicalQuestion body of
      Just _->True
      Nothing->V.null (logicalItems body) && T.null (logicalTarget body)

-- A human copy intent owns its immutable source/selection. Later streaming
-- append may replace the catalogue but not this capture. Delivery checks the
-- frame/provider lifetime and the shared clipboard intent serial.
data ConversationCopy = ConversationCopy !W.WindowRef !Text !LogicalBody !BodySelection !Int
  deriving (Eq,Show)

-- | Read only selected canonical blocks on the presentation worker. Furniture
-- and soft wraps are absent; hard breaks/block separators belong to the parser.
-- As in prepared message copy, one bubble is speaker-free and cross-bubble
-- primary copy carries attribution. Live answers/choice controls are absent.
logicalBodyCopy :: LogicalBody -> BodySelection -> IO Text
logicalBodyCopy body (BodySelection anchor caret)=do
  result<-evaluate (T.intercalate "\n\n" (map render messages))
  _<-evaluate (T.length result)
  pure result
  where
    first=min anchor caret
    lastPoint=max anchor caret
    items=logicalBodyItems body
    lower=case first of BodyPoint ident _ _->logicalBodyItemIndex ident body; QuestionPoint{}->Just (V.length items)
    upper=case lastPoint of BodyPoint ident _ _->logicalBodyItemIndex ident body; QuestionPoint{}->Just (V.length items-1)
    selected=case (lower,upper) of
      (Just a,Just z) | a<=z->V.toList (V.slice a (z-a+1) items)
      _->[]
    messages=[(role=="You",text) | item<-selected,Reply role _<-[recordContent (logicalItemRecord item)],
      let ident=recordId (logicalItemRecord item),let text=part (BodyPoint ident) (logicalItemBlocks item),not (T.null text)]++question
    question=case logicalQuestion body of
      Just (QuestionSchema token prompt _) | first<=QuestionPoint token maxBound maxBound,lastPoint>=QuestionPoint token 0 0->
        let text=part (QuestionPoint token) (V.fromList (markdownBlocks (parseMarkdown prompt)))
        in [(False,text) | not (T.null text)]
      _->[]
    part point blocks
      | V.null blocks=""
      | otherwise=T.concat [slice block (markdownBlockText (blocks V.! block)) | block<-[startBlock..endBlock]]
      where
        end=max 0 (V.length blocks-1)
        bound source fallback=case source of
          BodyPoint ident block scalar | point 0 0==BodyPoint ident 0 0->(max 0 (min end block),scalar)
          QuestionPoint token block scalar | block>=0,point 0 0==QuestionPoint token 0 0->(max 0 (min end block),scalar)
          _->fallback
        (startBlock,startScalar)=bound first (0,0)
        (endBlock,endScalar)=bound lastPoint (end,maxBound)
        slice block text=let begin=if block==startBlock then max 0 startScalar else 0
                             count=if block==endBlock then max 0 (endScalar-begin) else maxBound
                         in T.take count (T.drop begin text)
    render (outgoing,text)=(if T.null (logicalTarget body) && length messages>1 then if outgoing then "User: " else "Bot: " else "")<>text

-- One existing UI expansion owner covers individual activities and grouped runs.
-- Transcript updates contain data only and cannot overwrite this interaction.
data ToolExpansion = ActivityExpansion !Text | RunExpansion !Text deriving (Eq,Ord)

-- The authenticated question token owns immutable prompt/choices. No live
-- answer, selection, focus or Undo can enter a background body capture.
data QuestionSchema = QuestionSchema !Int !Text ![Text]

-- Fixed furniture intervals address precisely one prepared body's scalar space.
-- Live answer paint/hit/caret resolves them through that body's current layout.
data QuestionProjection = QuestionProjection
  { projectedQuestionToken :: !Int, projectedQuestionWidth :: !Int
  , projectedQuestionInput :: !(Maybe Int), projectedQuestionChoices :: [[Int]]
  } deriving (Eq,Show)

-- Captured incarnation, never a reusable provider/session display label.
data BodyProvider
  = PrimaryBodyProvider !(StableName A.Launch) !(Maybe (StableName A.Client,Maybe Text))
  | ChildBodyProvider !AH.AgentConfigRef
  | RecoveredBodyProvider !Unique
  deriving Eq

-- Desired work includes the transcript root. Completion admission deliberately
-- separates its freshness from exact owner/schema/presentation correctness.
data BodyKey = BodyKey
  { bodyWindow :: !W.WindowRef, bodyTarget :: !Text, bodyProvider :: !BodyProvider
  , bodyTranscript :: !(StableName [Record]), bodyQuestionToken :: !(Maybe Int)
  , bodyColumns :: !Int, bodyGraphical :: !Bool, bodyWide :: !Bool, bodyDemand :: !BodyDemand
  , bodyExpansion :: !(StableName (Set (Text,ToolExpansion)))
  } deriving Eq

-- | A completed stream snapshot may be shown while its successor is preparing.
-- Only the transcript-root field is ignored. The existing single serial job
-- prevents out-of-order adoption; changed owner/question/expansion never passes.
bodyOwnerMatches :: BodyKey -> BodyKey -> Bool
bodyOwnerMatches a b=bodyWindow a==bodyWindow b && bodyTarget a==bodyTarget b &&
  bodyProvider a==bodyProvider b && bodyQuestionToken a==bodyQuestionToken b &&
  bodyColumns a==bodyColumns b && bodyGraphical a==bodyGraphical b &&
  bodyWide a==bodyWide b && bodyDemand a==bodyDemand b && bodyExpansion a==bodyExpansion b

-- Immutable roots captured by the owner; all rendering/string/semantic walks
-- belong to preparation, not adoption. This contains no Conversation State.
data BodyInput = BodyInput
  { bodyTitle :: !Text, bodyProject :: !FilePath, bodySession :: !(Maybe Text)
  , bodyRecords :: ![Record], bodyQuestion :: !(Maybe QuestionSchema)
  , bodyExpandedTools :: !(Set (Text,ToolExpansion)), bodyPreviousLogical :: !(Maybe LogicalBody)
  , bodySelection :: !(Maybe BodySelection)
  }
data BodyRequest = BodyRequest !BodyKey !BodyInput
data BodyResult = BodyResult !BodyKey !(Either Text PreparedBody)
data PreparedBody = PreparedBody !W.PreparedWindow !(Maybe TextLayout) !HostBodyControls !LogicalBody
  !(Maybe (BodySelection,Maybe BodySelection))

-- Private host-minted regions; public TextSemantics cannot install controls.
data HostBodyControls = HostBodyControls
  { hostBodyQuestionToken :: !(Maybe Int), hostBodyViewport :: !(Maybe BodyViewport)
  , hostBodyQuestion :: !(Maybe QuestionProjection)
  , hostBodyActions :: [(Int,Int,Text,[Text])]
  }
-- The layout belongs to the immutable body, including while its frame is hidden.
data BodyControlReceipt = BodyControlReceipt !W.PreparedWindow !Int !Bool !(Maybe TextLayout) !HostBodyControls

-- Installed ownership does not imply a callable lifetime. Retired/recovered
-- installed text remains in pluginWindows exactly once, without host controls.
-- Close transfers that one immutable payload to InertBody before removing the
-- map entry. Hidden installed targets retain their entry; no Document mirror.
data ConversationBody
  = InstalledBody !W.WindowRef !(Maybe BodyControlReceipt)
  | InertBody !W.PreparedWindow
instance Eq ConversationBody where
  InstalledBody a x==InstalledBody b y=a==b && identity x==identity y
    where identity Nothing=Nothing; identity (Just (BodyControlReceipt prepared _ _ _ _))=Just prepared
  InertBody a==InertBody b=a==b
  _==_=False
instance Show ConversationBody where
  show (InstalledBody reference _)="InstalledBody "++show reference
  show (InertBody prepared)="InertBody "++show prepared

-- Rendering owns no callable lifetime. Recovery uses the same block producer
-- without allocating a fake WindowRef or recreating host question/shell actions.
data BodyDisplay = BodyDisplay
  { displayTarget :: !Text, displayColumns :: !Int, displayGraphical :: !Bool
  , displayWide :: !Bool, displayDemand :: !BodyDemand
  }

-- | Restore inert logical source on the recovery worker. IDs/revisions and
-- bounds are validated by the codec before this constructor; no live provider,
-- session, question, command or clipboard authority survives restoration.
restoreLogicalBody :: Text -> [Record] -> IO LogicalBody
restoreLogicalBody target records=do
  identity<-newUnique
  root<-makeStableName =<< evaluate records
  items<-V.fromList <$> mapM restore records
  evaluate (LogicalBody identity target (RecoveredBodyProvider identity) root Nothing Nothing (0,False) items)
  where
    restore record=do
      payload<-makeStableName =<< evaluate record
      evaluate (LogicalItem record payload (case recordContent record of
        Reply _ text->let parsed=parseMarkdown text in Just (LogicalParsed parsed (V.fromList (markdownBlocks parsed)) (markdownIntrinsicWidth parsed))
        _->Nothing) (0,False) V.empty)

-- | Prepare an inert bounded recovery viewport on its worker. Only passive
-- copy/style semantics are installed; links, shell and host control actions are
-- deliberately absent. The durable source remains this logical catalogue.
restoreLogicalViewport :: Bool -> Bool -> Int -> Int -> BodyAnchor -> LogicalBody -> IO (Either Text W.PreparedWindow)
restoreLogicalViewport graphical wide columns height anchor source=do
  logical<-withLogicalWidth wide (max 1 (columns-5)) source
  let display=BodyDisplay (logicalTarget logical) (max 1 columns) graphical wide (BodyDemand anchor 0 (max 1 height))
      input=BodyInput "Conversation" "" Nothing (map logicalItemRecord (V.toList (logicalItems logical))) Nothing S.empty (Just logical) Nothing
  fmap (\(body,_,_)->body) <$> prepareRenderedBody True display input logical

-- A row is derived only while its block is demanded. Logical ranges are
-- retained by the bounded receipt; styled payloads transfer into PreparedWindow.
data PendingRow = PendingRow !(Maybe BodyPoint) !MappedStyledRow
  ![(Int,Int,Text,[Text])] ![(Int,Int,Text)] !(Maybe (Text,Text))

-- | Prepare the containing block and forward viewport only. No previous
-- transcript item is parsed or laid out to locate an ordinary anchored view.
-- FollowEnd may scan the final demanded block's rows; it never scans history.
prepareConversationBody :: BodyRequest -> IO BodyResult
prepareConversationBody (BodyRequest key input)=do
  logical<-prepareLogicalBody key input (bodyPreviousLogical input)
  let display=BodyDisplay (bodyTarget key) (bodyColumns key) (bodyGraphical key) (bodyWide key) (bodyDemand key)
  result<-prepareRenderedBody (case bodyProvider key of RecoveredBodyProvider{}->True; _->False) display input logical
  let sourceChanged=case bodyPreviousLogical input of
        Just previous->logicalTranscript previous/=logicalTranscript logical || logicalProvider previous/=logicalProvider logical
        Nothing->True
  normalized<-if sourceChanged then traverse (normalize logical) (bodySelection input) else pure Nothing
  pure (BodyResult key (fmap (\(body,layout,controls)->PreparedBody body layout controls logical normalized) result))
  where
    normalize logical original@(BodySelection a z)=do
      let normalized=BodySelection <$> clampPoint logical a <*> clampPoint logical z
      case normalized of Just chosen->evaluate chosen >> pure (); Nothing->pure ()
      pure (original,normalized)

prepareRenderedBody :: Bool -> BodyDisplay -> BodyInput -> LogicalBody -> IO (Either Text (W.PreparedWindow,Maybe TextLayout,HostBodyControls))
prepareRenderedBody inert key input logical=case demandedRows key input logical of
    Left message->pure (Left message)
    Right (pending,scroll,atEnd,demandRow)->do
      let rows=[StyledRow sigils (if null rest then Nothing else Just Plain) messages
            | (PendingRow _ mapped _ _ _,rest)<-withTail pending,let StyledRow sigils _ messages=mappedStyledRow mapped]
          (_,receipts)=mapAccumL receipt 0 (zip pending rows)
          actions=[(bodyRowPaintStart row+a,bodyRowPaintStart row+z,name,values)
            | (row,PendingRow _ _ spans _ _)<-zip receipts pending,(a,z,name,values)<-spans]
          links=concat [projectRanges row mapped (a,z) (\lo hi->(lo,hi,url))
            | (row,PendingRow _ mapped _ ranges _)<-zip receipts pending,(a,z,url)<-ranges]
          shells=[(bodyRowPaintStart first,bodyRowPaintEnd (fst (last grouped)),dialect,source)
            | grouped<-List.groupBy sameShellBlock (zip receipts pending)
            , (first,PendingRow _ _ _ _ (Just (dialect,source))):_<-[grouped]]
          questionToken=case bodyQuestion input of Just (QuestionSchema token _ _)->Just token; Nothing->Nothing
          projected=do
            QuestionSchema token _ choices<-bodyQuestion input
            let starts index=[a | (a,_,name,values)<-actions,name=="question-choice",values==[T.pack (show token),T.pack (show index)]]
                other=case [a+7 | (a,_,name,_)<-actions,name=="question-input"] of first:_->Just first; _->Nothing
            if other==Nothing && all (null . starts) [0..length choices-1] then Nothing
              else Just (QuestionProjection token (displayColumns key) other [starts index | index<-[0..length choices-1]])
          guestHidden=[(if name=="question-input" then a+6 else a,z)
            | (a,z,name,_)<-actions,name `elem` ["question-input","question-choice"]]
          sessionHidden=[(bodyRowPaintStart row,bodyRowPaintEnd row)
            | (row,PendingRow Nothing _ _ _ _)<-zip receipts pending,V.null (logicalBodyItems logical),T.null (displayTarget key)]
          questionHidden=[(bodyRowPaintStart row,bodyRowPaintEnd row)
            | row<-receipts,Just QuestionPoint{}<-[bodyRowPoint row]]
          semantics=W.TextSemantics (W.CopyMessages (if T.null (displayTarget key) then W.UserBotAttribution else W.NoAttribution))
            (if inert then Nothing else Just (bodyProject input </> "conversation.md")) (if inert then V.empty else V.fromList links) (if inert then V.empty else V.fromList shells) W.ReadableWindow
            (V.fromList (sessionHidden++guestHidden)) (V.fromList sessionHidden) (V.fromList (sessionHidden++questionHidden))
          BodyDemand requested _ _=displayDemand key
          anchor=case requested of FollowEnd | let BodyDemand _ delta _=displayDemand key,delta>=0->FollowEnd; _->maybe requested At (bodyRowPoint =<< listAt scroll receipts)
          progress@(position,limit)=logicalProgress logical anchor
          viewport=BodyViewport (V.fromList receipts) anchor scroll demandRow atEnd progress
          controls=if inert then HostBodyControls Nothing (Just viewport) Nothing [] else HostBodyControls questionToken (Just viewport) projected actions
      prepared<-W.prepareSemanticRowsWindow (bodyTitle input) rows semantics
      case prepared of
        Left message->pure (Left message)
        Right body->do
          -- TextLayout remains in viewport paint coordinates. Its same glyph
          -- extents drive paint/privacy; the bounded receipt maps to logical IDs.
          layout<-case W.preparedWindowRows body of
            W.StyledRows styled->Just <$> prepareTextLayout (displayWide key) (displayColumns key) (W.preparedWindowText body) styled
            _->pure Nothing
          _<-evaluate position
          _<-evaluate limit
          _<-evaluate (sum [a+z+T.length name+sum (map T.length values) | (a,z,name,values)<-actions])
          _<-evaluate (V.foldl' (\n row->maybe () (\point->point `seq` ()) (bodyRowPoint row) `seq` n+bodyRowPaintStart row+bodyRowPaintEnd row+bodyRowLogicalEnd row+
            V.foldl' (\m (a,z,lo,hi)->m+a+z+lo+hi) 0 (bodyRowRanges row)) 0 (viewportRows viewport))
          evaluate (Right (body,layout,controls))
  where
    receipt offset (PendingRow point mapped _ _ _,StyledRow sigils newline _)=
      let end=offset+sigilsLength sigils
      in (end+maybe 0 (const 1) newline,BodyRow point (mappedRowEnd mapped) offset end (mappedSourceRanges mapped))
    projectRanges row mapped (a,z) build=
      [build (bodyRowPaintStart row+lo+(max a start-start)*(hi-lo) `div` max 1 (end-start))
        (bodyRowPaintStart row+lo+((min z end-start)*(hi-lo)+end-start-1) `div` max 1 (end-start))
      | (lo,hi,start,end)<-V.toList (mappedSourceRanges mapped),end>start,a<end,z>start]
    -- The parser owns the payload; contiguous demanded rows of its exact
    -- block share one visible action span without comparing source text.
    sameShellBlock (_,PendingRow (Just a) _ _ _ (Just _)) (_,PendingRow (Just z) _ _ _ (Just _))=sameBlock a z
    sameShellBlock _ _=False
    listAt index rows=case drop index rows of first:_->Just first; _->Nothing

-- Locate by item/block metadata, not accumulated prior row heights. A missing
-- anchor is refused rather than redirected to a different surviving item.
demandedRows :: BodyDisplay -> BodyInput -> LogicalBody -> Either Text ([PendingRow],Int,Bool,Int)
demandedRows key input logical=case anchor of
  WithinItem ident fraction->do
    point<-withinItemPoint logical ident fraction
    demandedRows key {displayDemand=BodyDemand (At point) delta requested} input logical
  FollowEnd->let rows=ending (budget+max 0 (-delta))
                 scroll=max 0 (min (max 0 (length rows-height)) (length rows-height+delta))
             in Right (take (scroll+budget) rows,scroll,scroll+height>=length rows,scroll)
  At point->do
    (index,block,rows)<-locate point
    let (prior,selectedPairs)=break (\(row,rest)->contains (anchorPoint index block point) row || null rest && endsAt (anchorPoint index block point) row) (withTail rows)
        -- Source replacement may shorten a surviving item's selected block.
        -- Clamp within that item, never redirect an evicted ID to its neighbor.
        selected=if null selectedPairs then lastRows 1 rows else map fst selectedPairs
        before=if null selectedPairs then take (max 0 (length prior-1)) (map fst prior) else map fst prior
        needed=max 0 (-delta)
        nearby=lastRows needed before
        prefix=preceding (needed-length nearby) index block++nearby
        combined=prefix++selected++following index block
        start=max 0 (length prefix+delta)
        remaining=drop start combined
        shown=take (height+2) remaining
        ended=length (take (height+1) remaining)<=height
    if null selected then Left "The anchored conversation position is no longer present."
      -- Ordinary anchors retain the requested top row even near EOF. Only
      -- explicit FollowEnd fills backwards from history. A forward movement
      -- past EOF clamps to the final row already demanded from this anchor.
      else if null shown then Right (lastRows 1 (take start combined),0,True,0)
      else Right (shown,0,ended,0)
  where
    BodyDemand anchor delta requested=displayDemand key
    height=max 1 requested
    budget=height+2
    items=logicalBodyItems logical
    count=V.length items
    questionRows=case bodyQuestion input of Nothing->[]; Just schema->questionPendingRows key schema
    locate (BodyPoint ident block _)=do
      index<-maybe (Left "The anchored conversation item is no longer present.") Right (logicalBodyItemIndex ident logical)
      let item=items V.! index; selectedBlock=min (blockCount item-1) block
      if block<0 then Left "The anchored conversation block is no longer present."
        else Right (index,selectedBlock,rowsAt index selectedBlock)
    locate (QuestionPoint token block _)=case bodyQuestion input of
      Just (QuestionSchema current _ _) | token==current->Right (count,block,filter (sameQuestionBlock block) questionRows)
      _->Left "The anchored question is no longer present."
    anchorPoint _ block (BodyPoint ident actual scalar)=BodyPoint ident block (if block==actual then scalar else maxBound)
    anchorPoint _ _ point=point
    sameQuestionBlock block (PendingRow (Just (QuestionPoint _ actual _)) _ _ _ _)=actual==block
    sameQuestionBlock _ _=False
    following index block
      | index>=count=dropWhile (sameQuestionBlock block) (dropWhile (not . sameQuestionBlock block) questionRows)
      | otherwise=let item=items V.! index
                  in concat [rowsAt index next | next<-[block+1..blockCount item-1]]++
                    itemSeparator index++itemsFrom (nextItem index)++questionRows
    ending needed
      | not (null questionRows)=let tailRows=lastRows needed questionRows in takeEnd (needed-length tailRows) (count-1)++tailRows
      | count==0=[PendingRow Nothing (plainMapped Comment (if T.null (displayTarget key) then "Session: "<>fromMaybe "not connected" (bodySession input) else "No messages yet.")) [] [] Nothing]
      | otherwise=takeEnd needed (count-1)
    takeEnd needed index
      | needed<=0 || index<0=[]
      | otherwise=let item=items V.! index; rows=lastBlocks needed index (blockCount item-1)
                      previous=case collapsedGroup index of Just (first,_,_)->first-1; Nothing->index-1
                      separator=if needed>length rows && previous>=0 then itemSeparator previous else []
                  in takeEnd (needed-length rows-length separator) previous++separator++rows
    lastBlocks needed index block
      | needed<=0 || block<0=[]
      | otherwise=let rows=lastRows needed (rowsAt index block)
                  in lastBlocks (needed-length rows) index (block-1)++rows
    preceding needed index block
      | needed<=0=[]
      | index>=count=let earlier=lastRows needed (takeWhile (not . sameQuestionBlock block) questionRows)
                    in takeEnd (needed-length earlier) (count-1)++earlier
      | otherwise=let prior=lastBlocks needed index (block-1)
                      previous=case collapsedGroup index of Just (first,_,_)->first-1; Nothing->index-1
                  in takeEnd (needed-length prior) previous++prior
    blankRow=PendingRow Nothing (MappedStyledRow (StyledRow Nil Nothing V.empty) 0 0 V.empty) [] [] Nothing
    itemsFrom index
      | index>=count=[]
      | otherwise=concat [rowsAt index block | block<-[0..blockCount (items V.! index)-1]]++
          itemSeparator index++itemsFrom (nextItem index)
    itemSeparator index
      | nextItem index>=count=[]
      | Just (_,lastIndex,_)<-group index,nextItem index<=lastIndex=[]
      | sameSpeaker (recordContent (logicalItemRecord (items V.! index))) (recordContent (logicalItemRecord (items V.! nextItem index)))=[]
      | otherwise=[blankRow]
    sameSpeaker (Reply a _) (Reply b _)=a==b
    sameSpeaker _ _=False
    nextItem index=case collapsedGroup index of Just (_,lastIndex,_)->lastIndex+1; Nothing->index+1
    group index
      | not (tool index)=Nothing
      | otherwise=let first=back index; lastIndex=forward index
                  in if lastIndex<=first then Nothing else case recordContent (logicalItemRecord (items V.! first)) of
                    Activity ident _ _->Just (first,lastIndex,ident)
                    _->Nothing
    tool index=index>=0 && index<count && isToolRecord (recordContent (logicalItemRecord (items V.! index)))
    back index | tool (index-1)=back (index-1) | otherwise=index
    forward index | tool (index+1)=forward (index+1) | otherwise=index
    collapsedGroup index=do
      found@(_,_,ident)<-group index
      if S.member (displayTarget key,RunExpansion ident) (bodyExpandedTools input) then Nothing else Just found
    rowsAt index block=case collapsedGroup index of
      Just (first,lastIndex,ident)->[groupHeading (Just (BodyPoint (recordId (logicalItemRecord (items V.! index))) 0 0)) False first lastIndex ident]
      Nothing->case group index of
        Just (first,lastIndex,ident)->
          let padding=min 2 (max 0 (displayColumns key-1))
              rows=indentMember padding (blockRows key {displayColumns=max 1 (displayColumns key-padding)} input (items V.! index) block)
          in if index==first && block==0 then groupHeading Nothing True first lastIndex ident:rows else rows
        _->blockRows key input (items V.! index) block
    indentMember 0 rows=rows
    indentMember padding (PendingRow point mapped actions links shell:rest)=
      let StyledRow sigils newline messages=mappedStyledRow mapped
          shift (a,z,lo,hi)=(a+padding,z+padding,lo,hi)
          decorated=mapped {mappedStyledRow=StyledRow (ConsChars (T.replicate padding " ") Plain sigils) newline
            (V.map (\(a,z,ident,outgoing)->(a+padding,z+padding,ident,outgoing)) messages),
            mappedSourceRanges=V.map shift (mappedSourceRanges mapped)}
      in PendingRow point decorated [(a+padding,z+padding,name,values) | (a,z,name,values)<-actions] links shell:rest
    indentMember _ []=[]
    groupHeading point expanded first lastIndex ident=
      let calls=[(label,value) | index<-[first..lastIndex],Activity label value _<-[recordContent (logicalItemRecord (items V.! index))]]
          running=length [() | (_,value)<-calls,field "status" value `elem` [Just ("pending"::Text),Just "in_progress"]]
          failed=length [() | (_,value)<-calls,field "status" value==Just ("failed"::Text)]
          labels=[T.pack (show running)<>" running" | running>0]++[T.pack (show failed)<>" failed" | failed>0]
          title=T.intercalate " · " (T.pack (show (lastIndex-first+1))<>" tool calls":labels++
            [T.intercalate ", " [fromMaybe label (field "title" value) | (label,value)<-take 3 calls]])
          text=clipCells (displayColumns key) ((if expanded then "▾▾ " else "▸▸ ")<>title)
      in PendingRow point (plainMapped Pragma text) (actionRanges text (Just ("toggle-tool-run",[ident]))) [] Nothing
    contains point (PendingRow (Just first) row actions _ _)=sameBlock first point &&
      (any (\(_,_,name,_)->name=="toggle-tool-run") actions ||
        pointScalar point>=mappedRowStart row && pointScalar point<mappedRowEnd row)
    contains _ _=False
    endsAt point (PendingRow (Just first) row _ _ _)=sameBlock first point && pointScalar point==mappedRowEnd row
    endsAt _ _=False

-- Canonical point resolution is worker-only and visits the selected item's
-- parsed blocks. Earlier items' payloads/row heights are never demanded.
itemBlockTexts :: LogicalItem -> V.Vector Text
itemBlockTexts item=case recordContent (logicalItemRecord item) of
  Reply _ _->V.map markdownBlockText (logicalItemBlocks item)
  Pause label->V.singleton label
  Activity ident value history->V.singleton (activityTitle ident value<>"\n"<>T.intercalate "\n" (map jsonText history))

withinItemPoint :: LogicalBody -> BodyItemId -> Int -> Either Text BodyPoint
withinItemPoint logical ident fraction
  | fraction<0 || fraction>1000=Left "Conversation seek fraction is outside its range."
  | otherwise=do
      index<-maybe (Left "The sought conversation item is no longer present.") Right (logicalBodyItemIndex ident logical)
      let sizes=V.map T.length (itemBlockTexts (logicalItems logical V.! index))
          scalar=fromInteger (toInteger (V.sum sizes)*toInteger fraction `div` 1000)
          locate block remaining
            | block>=V.length sizes=BodyPoint ident (max 0 (V.length sizes-1)) (if V.null sizes then 0 else V.last sizes)
            | remaining<sizes V.! block || block==V.length sizes-1=BodyPoint ident block remaining
            | otherwise=locate (block+1) (remaining-sizes V.! block)
      pure (locate 0 scalar)

-- The thumb estimates equal item slots, refined by canonical progress within
-- the current item. It is never a measured sum of earlier wrapped row heights.
logicalProgress :: LogicalBody -> BodyAnchor -> (Int,Int)
logicalProgress logical anchor=(position,limit)
  where
    items=logicalItems logical
    limit=max 1 ((V.length items+maybe 0 (const 1) (logicalQuestion logical))*1000)
    position=case anchor of
      FollowEnd->limit
      WithinItem ident fraction->maybe 0 (\index->index*1000+max 0 (min 1000 fraction)) (logicalBodyItemIndex ident logical)
      At QuestionPoint{}->V.length items*1000
      At (BodyPoint ident block scalar)->case logicalBodyItemIndex ident logical of
        Nothing->0
        Just index->let sizes=V.map T.length (itemBlockTexts (items V.! index))
                        total=V.sum sizes
                        offset=V.sum (V.take (max 0 block) sizes)+max 0 scalar
                    in index*1000+fromInteger (min 1000 (toInteger offset*1000 `div` toInteger (max 1 total)))

blockCount :: LogicalItem -> Int
blockCount item=max 1 (V.length (logicalItemBlocks item))

blockRows :: BodyDisplay -> BodyInput -> LogicalItem -> Int -> [PendingRow]
blockRows key input item block=case recordContent record of
  Reply role _ | Just source<-logicalItemBlocks item V.!? block->
    let columns=displayColumns key; available=max 1 (columns-5)
        cap=min available (logicalItemWidth item)
        rows=logicalItemRows (displayWide key) available item block
        decorate index (row:rest)=
          let mapped=if columns<6 then recolorMapped ident outgoing row else bubbleMapped (displayWide key) (displayGraphical key) columns cap ident outgoing (block==0 && index==0) (block==blockCount item-1 && null rest) row
          in PendingRow (Just (BodyPoint (recordId record) block (mappedRowStart row))) mapped []
               (markdownBlockLinks source) (markdownBlockShell source):decorate (index+1) rest
        decorate _ []=[]
        outgoing=role=="You"
        BodyItemId ident=recordId record
    in decorate (0::Int) rows
  Pause label->[PendingRow (Just (BodyPoint (recordId record) 0 0)) (plainMapped Comment label) [] [] Nothing]
  Activity ident value history->
    let expanded=S.member (displayTarget key,ActivityExpansion ident) (bodyExpandedTools input)
        title=activityTitle ident value
        shown=clipCells (max 0 (displayColumns key-2)) title
        heading=(if expanded then "▾ " else "▸ ")<>shown
        header=(plainMapped Pragma heading) {mappedRowEnd=T.length title,
          mappedSourceRanges=V.singleton (2,T.length heading,0,T.length shown)}
        row offset text=PendingRow (Just (BodyPoint (recordId record) 0 offset))
          ((plainMapped Plain text) {mappedRowStart=offset,mappedRowEnd=offset+T.length text,
            mappedSourceRanges=V.singleton (0,T.length text,offset,offset+T.length text)}) [] [] Nothing
        (_,details)=mapAccumL (\offset text->(offset+T.length text+1,row offset text))
          (T.length title+1) (T.splitOn "\n" (T.intercalate "\n" (map jsonText history)))
    in PendingRow (Just (BodyPoint (recordId record) 0 0)) header (actionRanges heading (Just ("toggle-activity",[ident]))) [] Nothing:
      [detail | expanded,detail<-details]
  _->[]
  where record=logicalItemRecord item

questionPendingRows :: BodyDisplay -> QuestionSchema -> [PendingRow]
questionPendingRows key (QuestionSchema token prompt choices)=promptRows++choiceRows++otherRows
  where
    columns=displayColumns key
    parsed=parseMarkdown prompt
    blocks=markdownBlocks parsed
    promptRows=concat [
      [PendingRow (Just (QuestionPoint token number (mappedRowStart row)))
        (if columns<6 then recolorMapped (-2) False row else bubbleMapped (displayWide key) (displayGraphical key) columns (min (max 1 (columns-5)) (markdownIntrinsicWidth parsed)) (-2) False (number==0 && index==0) (number==length blocks-1 && null rest) row) [] (markdownBlockLinks block) (markdownBlockShell block)
      | (index,(row,rest))<-zip [0::Int ..] (withTail (renderMarkdownBlock (displayWide key) (max 1 (columns-5)) block))]
      | (number,block)<-zip [0..] blocks]
    choiceRows=concat [[make (-3-index) offset text (Just ("question-choice",[shownToken,T.pack (show index)]))
      | (offset,text)<-snd (mapAccumL (\offset text->(offset+T.length text+1,(offset,text))) 0 (T.splitOn "\n" (questionChoiceLines columns False label)))]
      | (index,label)<-zip [0..] choices]
    otherBlock=(-1)
    otherRows=[make otherBlock 0 ("Other: "<>T.replicate (max 1 (columns-8)+1) " ") (Just ("question-input",[shownToken])),
      submitRow]
    submitRow=PendingRow (Just (QuestionPoint token (-2) 0))
      (plainMapped Plain "[Submit answer]  [Cancel]")
      [(0,15,"question-submit",[shownToken]),(17,25,"question-cancel",[shownToken])] [] Nothing
    shownToken=T.pack (show token)
    make block offset text action=let row=plainMapped Plain text
      in PendingRow (Just (QuestionPoint token block offset)) (row {mappedRowStart=offset,mappedRowEnd=offset+T.length text,
        mappedSourceRanges=V.singleton (0,T.length text,offset,offset+T.length text)}) (actionRanges text action) [] Nothing

actionRanges :: Text -> Maybe (Text,[Text]) -> [(Int,Int,Text,[Text])]
actionRanges text=maybe [] (\(name,values)->[(0,T.length text,name,values)])

plainMapped :: Style -> Text -> MappedStyledRow
plainMapped style text=MappedStyledRow (case styledRows (styledText style text) of first:_->first; []->StyledRow Nil Nothing V.empty)
  0 (T.length text) (V.singleton (0,T.length text,0,T.length text))
recolorMapped :: Int -> Bool -> MappedStyledRow -> MappedStyledRow
recolorMapped ident outgoing row=let StyledRow sigils newline _=mappedStyledRow row
  in row {mappedStyledRow=StyledRow (mapSigilsStyle (BubbleText ident outgoing) sigils) newline (V.map (\(a,z,_,_)->(a,z,ident,outgoing)) (mappedSourceRanges row))}

-- Bubble furniture changes paint ranges only; logical endpoints remain exactly
-- the parser's block coordinates. No whole-message row maximum/last scan occurs.
bubbleMapped :: Bool -> Bool -> Int -> Int -> Int -> Bool -> Bool -> Bool -> MappedStyledRow -> MappedStyledRow
bubbleMapped wide graphical columns cap ident outgoing first lastRow row=
  let StyledRow sigils newline _=mappedStyledRow row
      natural=sigilsColumn wide 0 sigils
      contentWidth=max natural cap
      background=BubbleStyle outgoing Plain
      edge=TerminalStyle (if outgoing then 0x00aaaa else 0xaaaaaa) 0x0000aa 0
      tile number=styledText edge (T.singleton (bubbleTile graphical number))
      side leftSide | first && lastRow=tile (if leftSide then if outgoing then 4 else 2 else if outgoing then 3 else 5)
                    | first && (if outgoing then leftSide else not leftSide)=tile (if outgoing then 0 else 1)
                    | lastRow=tile (if leftSide then 2 else 3)
                    | otherwise=styledText background " "
      tailCell=if first then tile (if outgoing then 7 else 6) else styledText Plain " "
      prefix=if outgoing then styledText Plain (T.replicate (max 0 (columns-contentWidth-3)) " ")++side True else tailCell++side True
      suffix=styledText background (T.replicate (max 0 (contentWidth-natural)) " ")++side False++if outgoing then tailCell else []
      fromRuns runs=case styledRows runs of StyledRow value _ _:_->value; _->Nil
      before=fromRuns prefix
      shifted=V.map (\(a,z,lo,hi)->(a+sigilsLength before,z+sigilsLength before,lo,hi)) (mappedSourceRanges row)
      paint=appendSigils before (appendSigils (mapSigilsStyle (BubbleText ident outgoing) sigils) (fromRuns suffix))
  in row {mappedStyledRow=StyledRow paint newline (V.map (\(a,z,_,_)->(a,z,ident,outgoing)) shifted),mappedSourceRanges=shifted}
  where
    appendSigils Nil rest=rest
    appendSigils (ConsChars text style rest) next=ConsChars text style (appendSigils rest next)
    appendSigils (ConsSigil glyph style advance rest) next=ConsSigil glyph style advance (appendSigils rest next)

withTail :: [a] -> [(a,[a])]
withTail []=[]
withTail (first:rest)=(first,rest):withTail rest
lastRows :: Int -> [a] -> [a]
lastRows count=toList . List.foldl' (\rows row->let next=rows Seq.|> row in if Seq.length next>count then Seq.drop 1 next else next) Seq.empty


clipCells :: Int -> Text -> Text
clipCells count text=T.take (columnOffset text (max 0 count)) text

isToolRecord :: RecordContent -> Bool
isToolRecord (Activity _ value _)=field "status" value `elem`
  [Just ("pending"::Text),Just "in_progress",Just "completed",Just "failed"]
isToolRecord _=False

activityTitle :: Text -> Value -> Text
activityTitle ident value=T.unwords (T.words ("["<>fromMaybe "activity" (field "status" value)<>"] "<>fromMaybe ident (field "title" value)))

field :: FromJSON a => Text -> Value -> Maybe a
field name=parseMaybe (withObject "object" (.: K.fromText name))
jsonText :: ToJSON a => a -> Text
jsonText=TE.decodeUtf8 . BL.toStrict . encode

renderTimestamp :: Int -> Text -> StyledText
renderTimestamp width label = styledText Comment (T.replicate (max 0 ((width-T.length label) `div` 2)) " "<>T.take (max 0 width) label)

-- | The same scalar-wrapped labels prepare the static body and live paint.
questionChoiceLines :: Int -> Bool -> Text -> Text
questionChoiceLines columns selected text=T.intercalate "\n" (zipWith (<>)
  ((if selected then "(*) " else "( ) "):repeat "    ") (wrap text))
  where
    wrap remaining
      | T.null remaining=[]
      | otherwise=let count=max 1 (columnOffset remaining (max 1 (columns-4)))
                  in T.take count remaining:wrap (T.drop count remaining)

-- | Lay out Markdown as styled response-bubble characters for the chosen frontend.
renderReply :: Bool -> Int -> Bool -> Text -> StyledText
renderReply graphical width outgoing = fst . renderReplyWithShellBlocks graphical width outgoing

renderReplyWithShellBlocks :: Bool -> Int -> Bool -> Text -> (StyledText,[(Int,Int,Text,Text)])
renderReplyWithShellBlocks graphical requested outgoing text
  | width<6 = (recolor markdown,blocks)
  | otherwise = (concat rendered,[(position blockStart,position blockEnd,dialect,body) | (blockStart,blockEnd,dialect,body)<-blocks])
  where
    width=max 1 requested
    (markdown,blocks)=renderMarkdownWithShellBlocks (if width<6 then width else width-5) text
    contentRows=splitRows (recolor markdown)
    rendered=zipWith renderLine [0::Int ..] rows
    -- Every source cell survives bubble decoration; only row prefixes change.
    position offset =
      let (row,column)=lineColumn (styledContents markdown) offset
          renderedRow=row+if leadingCode then 1 else 0
          prefix=if outgoing then max 0 (width-bubbleWidth-3)+1 else 2
      in sum (map styledLength (take renderedRow rendered)) + (if renderedRow>0 then 1 else 0) + prefix + column
    codeRow=any (\(_,style)->case style of BubbleText _ _ (CodeStyle _ _)->True; _->False)
    leadingCode=case contentRows of first:_->codeRow first; []->False
    trailingCode=case reverse contentRows of lastRow:_->codeRow lastRow; []->False
    rows=[[] | leadingCode]++contentRows++[[] | trailingCode]
    columns chars=let t=styledContents chars in displayColumn t (T.length t)
    bubbleWidth=maximum (0:map columns rows)
    background=BubbleStyle outgoing Plain
    edge=TerminalStyle (if outgoing then 0x00aaaa else 0xaaaaaa) 0x0000aa 0
    recolor=map (\(c,style)->(c,BubbleText 0 outgoing style))
    spaces style n=styledText style (T.replicate (max 0 n) " ")
    tile n=(T.singleton (bubbleTile graphical n),edge)
    side first lastRow leftSide
      | first && lastRow = if leftSide then if outgoing then tile 4 else tile 2
                                            else if outgoing then tile 3 else tile 5
      | first = if leftSide then if outgoing then tile 0 else (" ",background)
                             else if outgoing then (" ",background) else tile 1
      | lastRow = tile (if leftSide then 2 else 3)
      | otherwise = (" ",background)
    lastIndex=length rows-1
    -- Margins belong to the bubble, never to copied text or shell-block spans.
    renderLine i chars =
      [("\n",if (leadingCode && i==1) || (trailingCode && i==lastIndex) then background else BubbleText 0 outgoing Plain) | i>0] ++ line i chars
    line i chars =
      let first=i==0; lastRow=i==lastIndex
          body=side first lastRow True:chars++spaces background (bubbleWidth-columns chars)++[side first lastRow False]
          tailCell=if first then tile (if outgoing then 7 else 6) else (" ",Plain)
      in if outgoing then spaces Plain (width-bubbleWidth-3)++body++[tailCell]
                     else tailCell:body
    splitRows=map fst . splitStyledText
