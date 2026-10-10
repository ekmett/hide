-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.Agent
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : Haskell2010
--
-- Provider-neutral driver contract. The editor owns authenticated actors,
-- identity, capacity, workspaces and message tickets. A provider owns one
-- connection and translates its protocol into these events and operations.
-- No editor, frontend or transport implementation is a dependency of this API.
module Hide.Plugin.Agent
  ( AgentId(..)
  , Actor(..)
  , Context(..)
  , Workspace(..)
  , SpawnSpec(..)
  , ConfigChoice(..)
  , Capabilities(..)
  , PrivateSource(..)
  , StartRequest(..)
  , HubMessage(..)
  , AgentDriver(..)
  , DriverEvent(..)
  , StartProvider
  , ProviderKind(..), ProviderEndpoint(..), ProviderHost(..), StartAgentProvider
  , ProviderPermission(..), ProviderFiles(..), ProviderTerminals(..)
  , ProviderTerminal(..), ProviderTerminalOutput(..), ProviderContent(..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Hide.Plugin.Provider

-- | Identity assigned by the host bridge, never decoded from tool arguments.
newtype AgentId = AgentId { agentIdText :: Text } deriving (Eq,Ord,Show)
-- | Host-authenticated author identity; never decode this from an agent tool argument.
data Actor = Human | Agent AgentId deriving (Eq,Show)
-- | Start afresh or fork the identified provider session, when advertised.
data Context = Fresh | Fork AgentId deriving (Eq,Show)
-- | Workspace request. The host resolves and acquires it before provider startup.
data Workspace = Shared | Worktree { workspaceRef :: Maybe Text, workspaceBranch :: Maybe Text, workspaceName :: Maybe Text } deriving (Eq,Show)
-- | Host-validated launch intent; provider code cannot change its workspace authority.
data SpawnSpec = SpawnSpec
  { spawnName :: Text, spawnTask :: Text, spawnDirectory :: FilePath
  , spawnWorkspace :: Workspace, spawnContext :: Context, spawnModel :: Maybe Text, spawnEffort :: Maybe Text
  } deriving (Eq,Show)
-- | One advertised finite setting. IDs remain provider-owned, labels are public.
data ConfigChoice = ConfigChoice
  { configId :: Text, configCategory :: Text, configCurrent :: Text, configValues :: [(Text,Text)]
  } deriving (Eq,Show)
-- | Explicit provider capabilities; absence never implies protocol support.
data Capabilities = Capabilities { supportsFork :: Bool, supportsResume :: Bool, supportsSteering :: Bool, configChoices :: [ConfigChoice] }
  deriving (Eq,Show)
-- | Only the trusted launcher and protected persistence see provider session keys.
data PrivateSource = PrivateSource { sourceAgent :: AgentId, sourceSessionKey :: Text } deriving (Eq,Show)
-- | One admitted launch bound to its host identity and optional private source.
data StartRequest = StartRequest
  { startAgent :: AgentId, startOwner :: Actor, startSpec :: SpawnSpec, startSource :: Maybe PrivateSource, startResume :: Maybe Text }
  deriving (Eq,Show)
-- | Attributed delivery. A parent in a child's user seat is still an agent.
data HubMessage = HubMessage
  { messageTicket :: Int, messageAuthor :: Actor, messageText :: Text, messageIsUserSeat :: Bool }
  deriving (Eq,Show)
-- | Provider lifetime and delivery boundary. The host serializes ordinary
-- delivery/configuration; cancellation, steering and stop may arrive concurrently.
-- Stop is idempotent and joins owned workers: after it returns, no event or
-- permission callback may run. Cancel settles the current delivery without replay.
-- Failure must not imply that a prompt/configuration operation was unapplied.
-- Private session keys are for protected recovery, never public descriptions.
data AgentDriver = AgentDriver
  { driverDirectory :: FilePath, driverSessionKey :: Text, driverCapabilities :: Capabilities
  , driverConfigure :: [(Text,Text)] -> IO (Either Text Capabilities)
  , driverDeliver :: ProviderTurnId -> HubMessage -> [Text] -> ProviderSubmission -> IO (Either Text ProviderTurn)
  , driverCancel :: IO (), driverStop :: IO ()
  , driverSteer :: HubMessage -> [Text] -> ProviderSubmission -> IO (Either Text Value) }
-- Deliver returns only after the original submission commits; its returned turn
-- owns terminal completion and exact cancellation. The Hub's worker awaits that
-- result; Primary's host acknowledges its captured draft at send admission.
-- Extra text blocks preserve mutable editor context/skill attribution separately
-- from the authenticated message. Steering uses the same one-shot send boundary.
-- | Bounded, redacted public projection or a typed lifecycle/configuration event.
data DriverEvent = ProviderUpdate Text Value | ProviderCapabilities Capabilities | ProviderUsage Integer Integer | ProviderClosed
  deriving (Eq,Show)
-- | Acquire one connection. Failure or an asynchronous exception releases every
-- partially acquired resource; success transfers ownership to 'driverStop'.
-- Opening/forking/resuming a session does not itself submit the task as a prompt.
type StartProvider = StartRequest -> (DriverEvent -> IO ()) -> IO (Either Text AgentDriver)

-- | The two existing provider owners. Children have no native file/terminal bridge.
data ProviderKind = PrimaryProvider | ChildProvider deriving (Eq,Show)
-- | Host-selected private stdio endpoint; never decode caller authority from it.
data ProviderEndpoint = ProviderEndpoint
  { endpointName :: !Text, endpointLaunch :: !ProviderLaunch } deriving (Eq,Show)
-- | Bounded, already-scrubbed human permission presentation. Result is one
-- offered option ID or cancellation; the host still owns provenance and Review.
data ProviderPermission = ProviderPermission
  { permissionTitle :: !Text, permissionDetails :: !Text
  , permissionOptions :: ![(Text,Text,Text)] } deriving (Eq,Show)
-- | Existing host file capture/write owner, scoped to one acquisition.
data ProviderFiles = ProviderFiles
  { readProviderFile :: FilePath -> Int -> Maybe Int -> IO (ProviderReply Text)
  , writeProviderFile :: FilePath -> Text -> IO (ProviderReply ()) }
-- | Native create includes environment and full retained output, unlike MCP's
-- paged terminal tools. These values carry no process or editor handle.
data ProviderTerminal = ProviderTerminal
  { providerTerminalLaunch :: !ProviderLaunch, providerTerminalDirectory :: !FilePath
  , providerTerminalLimit :: !Int } deriving (Eq,Show)
data ProviderTerminalOutput = ProviderTerminalOutput
  { providerTerminalBytes :: !ByteString, providerTerminalTruncated :: !Bool
  , providerTerminalExit :: !(Maybe Int) } deriving (Eq,Show)
-- | Existing session Consoles owner, restricted to this provider's exact IDs.
data ProviderTerminals = ProviderTerminals
  { createProviderTerminal :: ProviderTerminal -> IO (ProviderReply Text)
  , readProviderTerminal :: Text -> IO (ProviderReply ProviderTerminalOutput)
  , waitProviderTerminal :: Text -> IO (ProviderReply Int)
  , killProviderTerminal :: Text -> IO (ProviderReply ())
  , releaseProviderTerminal :: Text -> IO (ProviderReply ()) }
-- | Private primary transcript updates, separate from bounded public Hub events.
-- Protocol IDs and human-author constructors are absent. Tool/plan payloads are
-- scrubbed on the adapter worker before publication to the original host owner.
data ProviderContent = ProviderMessage !Text !Text | ProviderTool !Value | ProviderPlan !Value | ProviderTurnBoundary !ProviderTurnId
  deriving (Eq,Show)
-- | Host-issued callbacks bound before acquisition. Native capabilities are
-- advertised only when their actual host services are supplied.
data ProviderHost = ProviderHost
  { providerPermission :: ProviderPermission -> IO (ProviderReply (Maybe Text))
  , providerFiles :: !(Maybe ProviderFiles), providerTerminals :: !(Maybe ProviderTerminals)
  , providerContent :: !(Maybe (Maybe ProviderTurnId -> ProviderContent -> IO ())) }
-- | Linked implementation selected once, consumed by both Primary and children.
-- Negotiation/process/protocol ownership transfers to driverStop on success.
type StartAgentProvider = ProviderKind -> ProviderIdentity -> ProviderLaunch -> [ProviderEndpoint] -> Text -> ProviderHost -> StartProvider
