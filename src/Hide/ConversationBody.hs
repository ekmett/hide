{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Immutable transcript preparation inputs and exact host body receipts.
-- This module owns no provider, Desktop, editable Buffer, callbacks or workers.
-- TextPresentation's existing serial worker will consume these closed requests;
-- Conversation retains task/control authority and adopts their exact results.
module Hide.ConversationBody
  ( BodyItemId(..), Record(..), RecordContent(..), BodyPoint(..), BodyAnchor(..), BodySelection(..)
  , LogicalBody, LogicalItem, logicalBodyIdentity, logicalBodyRead, logicalBodyItems, logicalBodyItemIndex, logicalItemRecord, logicalItemMarkdown, logicalItemBlocks, prepareLogicalBody
  , ToolExpansion(..), QuestionSchema(..), QuestionProjection(..)
  , BodyProvider(..), BodyKey(..), bodyOwnerMatches, BodyInput(..)
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
import Data.List (mapAccumL,foldl')
import Data.Unique (Unique,newUnique,hashUnique)
import qualified Data.Map.Strict as M
import Hide.Buffer (BufferContent,newBuffer,bufferContent,columnOffset,displayColumn,lineColumn)
import Hide.Markdown (Markdown,MarkdownBlock,parseMarkdown,markdownBlocks,markdownBlockText,renderMarkdownWithShellBlocks)
import Hide.Syntax (Style(..),bubbleTile,linkSpans)
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
data BodyPoint = BodyPoint !BodyItemId !Int !Int deriving (Eq,Ord,Show)
data BodyAnchor = At !BodyPoint | FollowEnd deriving (Eq,Show)
data BodySelection = BodySelection !BodyPoint !BodyPoint deriving (Eq,Show)

-- The record and its exact payload identity are strict metadata. Markdown is
-- lazy: looking up an item or constructing this vector parses no prior items.
data LogicalParsed = LogicalParsed !Markdown !(V.Vector MarkdownBlock)
data LogicalItem = LogicalItem !Record !(StableName Record) (Maybe LogicalParsed)
data LogicalBody = LogicalBody
  { logicalIdentity :: !Unique, logicalTarget :: !Text
  , logicalProvider :: !BodyProvider, logicalTranscript :: !(StableName [Record])
  , logicalSession :: !(Maybe Text), logicalQuestion :: !(Maybe QuestionSchema)
  , logicalItems :: !(V.Vector LogicalItem)
  }
instance Eq LogicalBody where a==b=logicalIdentity a==logicalIdentity b
instance Show LogicalBody where show body="LogicalBody "++show (hashUnique (logicalIdentity body))

-- | /O(1)/. Width and viewport changes preserve this exact identity. A changed
-- catalogue receives a fresh worker-issued identity, never a payload comparison.
logicalBodyIdentity :: LogicalBody -> Unique
logicalBodyIdentity=logicalIdentity

-- | /O(1)/. Borrow ordered items. Their parser owners remain lazy.
logicalBodyItems :: LogicalBody -> V.Vector LogicalItem
logicalBodyItems=logicalItems
logicalItemRecord :: LogicalItem -> Record
logicalItemRecord (LogicalItem record _ _)=record
logicalItemMarkdown :: LogicalItem -> Maybe Markdown
logicalItemMarkdown (LogicalItem _ _ parsed)=case parsed of
  Nothing->Nothing
  Just (LogicalParsed markdown _)->Just markdown

-- | Parse only this item's immutable source on the preparation/read worker.
-- Block metadata is cached, so a selected block is indexed without replaying
-- preceding blocks or laying out their rows.
logicalItemBlocks :: LogicalItem -> V.Vector MarkdownBlock
logicalItemBlocks (LogicalItem _ _ parsed)=case parsed of
  Nothing->V.empty
  Just (LogicalParsed _ blocks)->blocks

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
prepareLogicalBody key input previous=case previous of
  Just body | logicalProvider body==bodyProvider key &&
    logicalTranscript body==bodyTranscript key && questionTokenOf (logicalQuestion body)==bodyQuestionToken key->pure body
  _->do
    identity<-newUnique
    let old=case previous of
          Just body | logicalProvider body==bodyProvider key->M.fromList
            [(recordId (logicalItemRecord item),item) | item<-V.toList (logicalItems body)]
          _->M.empty
    items<-V.fromList <$> mapM (capture old) (bodyRecords input)
    evaluate (LogicalBody identity (bodyTarget key) (bodyProvider key) (bodyTranscript key)
      (bodySession input) (bodyQuestion input) items)
  where
    questionTokenOf Nothing=Nothing
    questionTokenOf (Just (QuestionSchema token _ _))=Just token
    capture old record=do
      payload<-makeStableName =<< evaluate record
      case M.lookup (recordId record) old of
        Just item@(LogicalItem _ same _) | same==payload->pure item
        _->evaluate (LogicalItem record payload (case recordContent record of
          Reply _ text->let parsed=parseMarkdown text in Just (LogicalParsed parsed (V.fromList (markdownBlocks parsed)))
          _->Nothing))

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
      Activity ident value history->
        T.unwords (T.words ("["<>fromMaybe "activity" (field "status" value)<>"] "<>fromMaybe ident (field "title" value)))<>
        "\n"<>T.intercalate "\n" (map jsonText history)
    question=case logicalQuestion body of
      Nothing->[]
      Just (QuestionSchema _ prompt choices)->
        [canonical (parseMarkdown prompt)<>"\n"<>T.intercalate "\n" (map hide choices)<>"\nOther:  \n[Submit answer]  [Cancel]"]
    canonical=T.concat . map markdownBlockText . markdownBlocks
    hide=T.map (\c->if c=='\n' || c=='\r' then c else ' ')
    redacted=case logicalQuestion body of
      Just _->True
      Nothing->V.null (logicalItems body) && T.null (logicalTarget body)

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
  , projectedQuestionInput :: !Int, projectedQuestionChoices :: [[Int]]
  } deriving (Eq,Show)

-- Captured incarnation, never a reusable provider/session display label.
data BodyProvider
  = PrimaryBodyProvider !(StableName A.Launch) !(Maybe (StableName A.Client,Maybe Text))
  | ChildBodyProvider !AH.AgentConfigRef
  deriving Eq

-- Desired work includes the transcript root. Completion admission deliberately
-- separates its freshness from exact owner/schema/presentation correctness.
data BodyKey = BodyKey
  { bodyWindow :: !W.WindowRef, bodyTarget :: !Text, bodyProvider :: !BodyProvider
  , bodyTranscript :: !(StableName [Record]), bodyQuestionToken :: !(Maybe Int)
  , bodyColumns :: !Int, bodyGraphical :: !Bool, bodyWide :: !Bool
  , bodyExpansion :: !(StableName (Set (Text,ToolExpansion)))
  } deriving Eq

-- | A completed stream snapshot may be shown while its successor is preparing.
-- Only the transcript-root field is ignored. The existing single serial job
-- prevents out-of-order adoption; changed owner/question/expansion never passes.
bodyOwnerMatches :: BodyKey -> BodyKey -> Bool
bodyOwnerMatches a b=bodyWindow a==bodyWindow b && bodyTarget a==bodyTarget b &&
  bodyProvider a==bodyProvider b && bodyQuestionToken a==bodyQuestionToken b &&
  bodyColumns a==bodyColumns b && bodyGraphical a==bodyGraphical b &&
  bodyWide a==bodyWide b && bodyExpansion a==bodyExpansion b

-- Immutable roots captured by the owner; all rendering/string/semantic walks
-- belong to preparation, not adoption. This contains no Conversation State.
data BodyInput = BodyInput
  { bodyTitle :: !Text, bodyProject :: !FilePath, bodySession :: !(Maybe Text)
  , bodyRecords :: ![Record], bodyQuestion :: !(Maybe QuestionSchema)
  , bodyExpandedTools :: !(Set (Text,ToolExpansion)), bodyPreviousLogical :: !(Maybe LogicalBody)
  }
data BodyRequest = BodyRequest !BodyKey !BodyInput
data BodyResult = BodyResult !BodyKey !(Either Text PreparedBody)
data PreparedBody = PreparedBody !W.PreparedWindow !(Maybe TextLayout) !HostBodyControls !LogicalBody

-- Private host-minted regions; public TextSemantics cannot install controls.
data HostBodyControls = HostBodyControls
  { hostBodyQuestion :: !(Maybe QuestionProjection)
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

-- | Render and force one immutable transcript on the presentation worker.
-- Live question answers never enter this capture.
prepareConversationBody :: BodyRequest -> IO BodyResult
prepareConversationBody (BodyRequest key input)=do
  logical<-prepareLogicalBody key input (bodyPreviousLogical input)
  prepared<-W.prepareSemanticTextWindow (bodyTitle input) styled semantics
  case prepared of
    Left message->pure (BodyResult key (Left message))
    Right body->do
      layout<-if bodyWide key || W.preparedWindowNeedsLayout False body
        then case W.preparedWindowRows body of
          W.StyledRows rows->Just <$> prepareTextLayout (bodyWide key) columns (W.preparedWindowText body) rows
          _->pure Nothing
        else pure Nothing
      _<-evaluate (sum [a+z+T.length action+sum (map T.length values) | (a,z,action,values)<-actions])
      _<-evaluate (maybe 0 (sum . concat . projectedQuestionChoices) projection)
      snapshot<-evaluate (PreparedBody body layout (HostBodyControls projection actions) logical)
      evaluate (BodyResult key (Right snapshot))
  where
    target=bodyTarget key
    columns=bodyColumns key
    records=[(ident,recordContent record) | record<-bodyRecords input,let BodyItemId ident=recordId record]
    header=if T.null target then "Session: "<>fromMaybe "not connected" (bodySession input)<>"\n" else "No messages yet.\n"
    chunks=if null records then [(plain Comment header,Nothing,[]) | maybe True (const False) (bodyQuestion input)] else renderRecords columns records
    questions=maybe [] (renderQuestion columns (-2)) (bodyQuestion input)
    allChunks=chunks++[(plain Plain "\n\n",Nothing,[]) | not (null chunks) && not (null questions)]++questions
    styled=concatMap (\(cells,_,_)->cells) allChunks
    (_,shellBlocks)=foldl' (\(offset,found) (cells,_,blocks)->(offset+length cells,found++[(offset+a,offset+z,dialect,body) | (a,z,dialect,body)<-blocks])) (0,[]) allChunks
    (_,actions)=foldl' (\(offset,found) (cells,action,_)->(offset+length cells,found++maybe [] (\(name,values)->[(offset,offset+length cells,name,values)]) action)) (0,[]) allChunks
    projection=do
      QuestionSchema token _ choices<-bodyQuestion input
      first<-case [a+7 | (a,_,action,_)<-actions,action=="question-input"] of a:_->Just a; _->Nothing
      let starts index label=case [a | (a,_,action,values)<-actions,action=="question-choice",values==[T.pack (show token),T.pack (show index)]] of
            a:_->snd (mapAccumL (\offset line->(offset+T.length line+1,offset)) a (T.splitOn "\n" (questionChoiceLines columns False label)))
            _->[]
      pure (QuestionProjection token columns first [starts index label | (index,label)<-zip [0::Int ..] choices])
    sessionHidden=if "Session: " `T.isPrefixOf` header && null records && null questions then [(0,T.length (T.takeWhile (/='\n') header))] else []
    guestHidden=sessionHidden++[(if action=="question-input" then a+6 else a,z) | (a,z,action,_)<-actions,action `elem` ["question-input","question-choice"]]
    questionStart=sum [length cells | (cells,_,_)<-chunks]+if null chunks || null questions then 0 else 2
    recoveryHidden=sessionHidden++[(questionStart,length styled) | not (null questions)]
    semantics=W.TextSemantics (W.CopyMessages (if T.null target then W.UserBotAttribution else W.NoAttribution)) (Just (bodyProject input </> "conversation.md"))
      (V.fromList (linkSpans styled)) (V.fromList shellBlocks) W.ReadableWindow
      (V.fromList guestHidden) (V.fromList sessionHidden) (V.fromList recoveryHidden)
    plain style=map (,style).T.unpack
    renderRecords _ []=[]
    renderRecords width rows@(record:rest)
      | (calls@(_:_:_),after)<-span (isToolRecord.snd) rows = renderRun width calls++continue (last calls) after
      | otherwise = renderRecord width record++continue record rest
      where
        continue _ []=[]
        continue previous remaining@(next:_)=
          [(plain Plain (if sameSpeaker (snd previous) (snd next) then "\n" else "\n\n"),Nothing,[])]++renderRecords width remaining
    renderRun width calls=case calls of
      (_,Activity ident _ _):_ ->
        let expanded=S.member (target,RunExpansion ident) (bodyExpandedTools input)
            titles=[fromMaybe label (field "title" value) | (_,Activity label value _)<-calls]
            running=length [() | (_,Activity _ value _)<-calls,field "status" value `elem` [Just ("pending"::Text),Just "in_progress"]]
            failed=length [() | (_,Activity _ value _)<-calls,field "status" value==Just ("failed"::Text)]
            count n label=[T.pack (show n)<>label | n>0]
            summary=T.intercalate " · " (T.pack (show (length calls))<>" tool calls":count running " running"++count failed " failed"++[T.intercalate ", " (take 3 titles)])
            heading=clipCells width ((if expanded then "▾▾ " else "▸▸ ")<>T.unwords (T.words summary))
        in [(plain Pragma heading,Just ("toggle-tool-run",[ident]),[])]++
           (if expanded then concatMap (\call->(plain Plain "\n  ",Nothing,[]):renderRecord (max 1 (width-2)) call) calls else [])
      _ -> []
    sameSpeaker (Reply a _) (Reply b _)=a==b
    sameSpeaker _ _=False
    renderRecord width (messageId,record)=case record of
      Pause label -> [(renderTimestamp width label,Nothing,[])]
      Reply role text -> [replyChunk width messageId (role=="You") text]
      Activity ident value history ->
        let expanded=S.member (target,ActivityExpansion ident) (bodyExpandedTools input)
            title=T.unwords (T.words ("["<>fromMaybe "activity" (field "status" value)<>"] "<>fromMaybe ident (field "title" value)))
            heading=(if expanded then "▾ " else "▸ ")<>clipCells (max 1 (width-2)) title
        in [(plain Pragma heading,Just ("toggle-activity",[ident]),[])]++
          [(plain Plain ("\n"<>T.intercalate "\n" (map jsonText history)),Nothing,[]) | expanded]
    renderQuestion width messageId (QuestionSchema tokenId prompt choices)=
      [replyChunk width messageId False (prompt)]++
      concat [[(plain Plain "\n",Nothing,[]),(plain Plain
        (questionChoiceLines width False text),Just ("question-choice",[token,T.pack (show index)]),[])] | (index,text)<-zip [0::Int ..] (choices)]++
      [(plain Plain "\n",Nothing,[]),(plain Plain
        ("Other: "<>T.replicate (max 1 (width-8)+1) " "),Just ("question-input",[token]),[]),
       (plain Plain "\n",Nothing,[]),(plain Keyword "[Submit answer]",Just ("question-submit",[token]),[]),
       (plain Plain "  ",Nothing,[]),(plain Comment "[Cancel]",Just ("question-cancel",[token]),[])]
      where token=T.pack (show (tokenId))

    replyChunk width messageId outgoing text =
      let (cells,blocks)=renderReplyWithShellBlocks (bodyGraphical key) width outgoing text
      in (map (\(c,style)->(c,case style of BubbleText _ sent base->BubbleText messageId sent base; _->style)) cells,Nothing,blocks)


clipCells :: Int -> Text -> Text
clipCells count text=T.take (columnOffset text (max 0 count)) text

isToolRecord :: RecordContent -> Bool
isToolRecord (Activity _ value _)=field "status" value `elem`
  [Just ("pending"::Text),Just "in_progress",Just "completed",Just "failed"]
isToolRecord _=False

field :: FromJSON a => Text -> Value -> Maybe a
field name=parseMaybe (withObject "object" (.: K.fromText name))
jsonText :: ToJSON a => a -> Text
jsonText=TE.decodeUtf8 . BL.toStrict . encode

renderTimestamp :: Int -> Text -> [(Char,Style)]
renderTimestamp width label = map (,Comment) (T.unpack (T.replicate (max 0 ((width-T.length label) `div` 2)) " "<>T.take (max 0 width) label))

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
renderReply :: Bool -> Int -> Bool -> Text -> [(Char,Style)]
renderReply graphical width outgoing = fst . renderReplyWithShellBlocks graphical width outgoing

renderReplyWithShellBlocks :: Bool -> Int -> Bool -> Text -> ([(Char,Style)],[(Int,Int,Text,Text)])
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
      let (row,column)=lineColumn (T.pack (map fst markdown)) offset
          renderedRow=row+if leadingCode then 1 else 0
          prefix=if outgoing then max 0 (width-bubbleWidth-3)+1 else 2
      in sum (map length (take renderedRow rendered)) + (if renderedRow>0 then 1 else 0) + prefix + column
    codeRow=any (\(_,style)->case style of BubbleText _ _ (CodeStyle _ _)->True; _->False)
    leadingCode=case contentRows of first:_->codeRow first; []->False
    trailingCode=case reverse contentRows of lastRow:_->codeRow lastRow; []->False
    rows=[[] | leadingCode]++contentRows++[[] | trailingCode]
    columns chars=let t=T.pack (map fst chars) in displayColumn t (T.length t)
    bubbleWidth=maximum (0:map columns rows)
    background=BubbleStyle outgoing Plain
    edge=TerminalStyle (if outgoing then 0x00aaaa else 0xaaaaaa) 0x0000aa 0
    recolor=map (\(c,style)->(c,BubbleText 0 outgoing style))
    spaces style n=replicate (max 0 n) (' ',style)
    tile n=(bubbleTile graphical n,edge)
    side first lastRow leftSide
      | first && lastRow = if leftSide then if outgoing then tile 4 else tile 2
                                            else if outgoing then tile 3 else tile 5
      | first = if leftSide then if outgoing then tile 0 else (' ',background)
                             else if outgoing then (' ',background) else tile 1
      | lastRow = tile (if leftSide then 2 else 3)
      | otherwise = (' ',background)
    lastIndex=length rows-1
    -- Margins belong to the bubble, never to copied text or shell-block spans.
    renderLine i chars =
      [('\n',if (leadingCode && i==1) || (trailingCode && i==lastIndex) then background else BubbleText 0 outgoing Plain) | i>0] ++ line i chars
    line i chars =
      let first=i==0; lastRow=i==lastIndex
          body=side first lastRow True:chars++spaces background (bubbleWidth-columns chars)++[side first lastRow False]
          tailCell=if first then tile (if outgoing then 7 else 6) else (' ',Plain)
      in if outgoing then spaces Plain (width-bubbleWidth-3)++body++[tailCell]
                     else tailCell:body
    splitRows chars=case break ((=='\n').fst) chars of
      (row,[]) -> [row]
      (row,_:rest) -> row:splitRows rest
