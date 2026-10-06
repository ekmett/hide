-- SPDX-License-Identifier: BSD-3-Clause
-- | Immutable transcript preparation inputs and exact host body receipts.
-- This module owns no provider, Desktop, editable Buffer, callbacks or workers.
-- TextPresentation's existing serial worker will consume these closed requests;
-- Conversation retains task/control authority and adopts their exact results.
module Hide.ConversationBody
  ( Record(..), QuestionSchema(..), QuestionProjection(..)
  , BodyProvider(..), BodyKey(..), bodyOwnerMatches, BodyInput(..)
  , BodyRequest(..), BodyResult(..), PreparedBody(..), HostBodyControls(..)
  , BodyControlReceipt(..), ConversationBody(..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Set (Set)
import System.Mem.StableName (StableName)
import qualified Hide.ACP as A
import qualified Hide.AgentHub as AH
import qualified Hide.Plugin.Window as W
import Hide.TextLayout (TextLayout)

-- Records retain immutable transcript data only; copy identities are local to
-- one rendering and do not identify a provider or grant input authority.
data Record = Reply Text Text | Activity Text Value [Value] Bool | Pause Text deriving (Eq,Show)

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
  = PrimaryBodyProvider !(StableName A.Launch) !(Maybe (StableName A.Client,Text))
  | ChildBodyProvider !AH.AgentConfigRef
  deriving Eq

-- Desired work includes the transcript root. Completion admission deliberately
-- separates its freshness from exact owner/schema/presentation correctness.
data BodyKey = BodyKey
  { bodyWindow :: !W.WindowRef, bodyTarget :: !Text, bodyProvider :: !BodyProvider
  , bodyTranscript :: !(StableName [Record]), bodyQuestionToken :: !(Maybe Int)
  , bodyColumns :: !Int, bodyGraphical :: !Bool, bodyWide :: !Bool
  , bodyExpansion :: !(StableName (Set (Text,Text)))
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
  , bodyExpandedTools :: !(Set (Text,Text))
  }
data BodyRequest = BodyRequest !BodyKey !BodyInput
data BodyResult = BodyResult !BodyKey !(Either Text PreparedBody)
data PreparedBody = PreparedBody !W.PreparedWindow !(Maybe TextLayout) !HostBodyControls

-- Private host-minted regions; public TextSemantics cannot install controls.
data HostBodyControls = HostBodyControls
  { hostBodyQuestion :: !(Maybe QuestionProjection)
  , hostBodyActions :: [(Int,Int,Text,[Text])]
  }
data BodyControlReceipt = BodyControlReceipt !W.PreparedWindow !HostBodyControls

-- Installed ownership does not imply a callable lifetime. Retired/recovered
-- installed text remains in pluginWindows exactly once, without host controls.
-- Close transfers that one immutable payload to InertBody before removing the
-- map entry. Hidden installed targets retain their entry; no Document mirror.
data ConversationBody
  = InstalledBody !W.WindowRef !(Maybe BodyControlReceipt)
  | InertBody !W.PreparedWindow
instance Eq ConversationBody where
  InstalledBody a x==InstalledBody b y=a==b && identity x==identity y
    where identity Nothing=Nothing; identity (Just (BodyControlReceipt prepared _))=Just prepared
  InertBody a==InertBody b=a==b
  _==_=False
instance Show ConversationBody where
  show (InstalledBody reference _)="InstalledBody "++show reference
  show (InertBody prepared)="InertBody "++show prepared
