{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.AgentHub
  ( AgentHub, AgentId(..), Actor(..), HubLimits(..), SpawnSpec(..), Context(..), Workspace(..)
  , AgentDriver(..), DriverEvent(..), StartProvider, StartRequest(..), PrivateSource(..), HubMessage(..)
  , Capabilities(..), ConfigChoice(..), parseCapabilities
  , newAgentHub, newAgentHubWithLimits, closeAgentHub, spawnAgent, registerAgent, updateExternalAgent, setExternalAgentBusy, renameAgent
  , listAgents, statusAgent, sendAgent, waitAgent, cancelAgent, endAgent
  , historyAgent, searchAgentHistory, recordAgentEvent, snapshotHub, restoreHub, restoreHubWithLimits
  ) where

import Control.Concurrent (forkIOWithUnmask)
import Control.Concurrent.STM
import Control.Exception (SomeException, SomeAsyncException, fromException, throwIO, try, mask, onException, finally)
import Control.Monad (unless, void, forM_)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe, parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isControl)
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Sequence as Q
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (isAbsolute)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import qualified Data.Text.Encoding as TE

-- These identities are supplied by the host bridge, never decoded from tool arguments.
newtype AgentId = AgentId { agentIdText :: Text } deriving (Eq,Ord,Show)
data Actor = Human | Agent AgentId deriving (Eq,Show)
data HubLimits = HubLimits { totalActiveAgents :: Int, directSubagents :: Int } deriving (Eq,Show)
data Context = Fresh | Fork AgentId deriving (Eq,Show)
data Workspace = Shared | Worktree { workspaceRef :: Maybe Text, workspaceBranch :: Maybe Text, workspaceName :: Maybe Text } deriving (Eq,Show)
data SpawnSpec = SpawnSpec
  { spawnName :: Text, spawnTask :: Text, spawnDirectory :: FilePath
  , spawnWorkspace :: Workspace, spawnContext :: Context, spawnModel :: Maybe Text, spawnEffort :: Maybe Text
  } deriving (Eq,Show)
data ConfigChoice = ConfigChoice
  { configId :: Text, configCategory :: Text, configCurrent :: Text, configValues :: [(Text,Text)]
  } deriving (Eq,Show)
data Capabilities = Capabilities { supportsFork :: Bool, supportsResume :: Bool, configChoices :: [ConfigChoice] }
  deriving (Eq,Show)
-- Only the trusted launcher and protected persistence see provider session keys.
data PrivateSource = PrivateSource { sourceAgent :: AgentId, sourceSessionKey :: Text } deriving (Eq,Show)
data StartRequest = StartRequest
  { startAgent :: AgentId, startOwner :: Actor, startSpec :: SpawnSpec, startSource :: Maybe PrivateSource }
  deriving (Eq,Show)
data HubMessage = HubMessage
  { messageTicket :: Int, messageAuthor :: Actor, messageText :: Text, messageIsUserSeat :: Bool }
  deriving (Eq,Show)
data AgentDriver = AgentDriver
  { driverDirectory :: FilePath, driverSessionKey :: Text, driverCapabilities :: Capabilities
  , driverConfigure :: [(Text,Text)] -> IO (Either Text Capabilities)
  , driverDeliver :: HubMessage -> IO (Either Text Value)
  , driverCancel :: IO (), driverStop :: IO () }
data DriverEvent = ProviderUpdate Text Value | ProviderClosed
  deriving (Eq,Show)
type StartProvider = StartRequest -> (DriverEvent -> IO ()) -> IO (Either Text AgentDriver)
data Phase = Starting | Idle | Running | Cancelling | Ended | Failed | Recovered deriving (Eq,Show)
data HistoryEvent = HistoryEvent Int Text Actor Value deriving (Eq,Show)
data Entry = Entry
  { entryId :: AgentId, entryParent :: Maybe AgentId, entrySpec :: SpawnSpec, entryPhase :: Phase
  , entryDriver :: Maybe AgentDriver, entryKey :: Maybe Text, entryCaps :: Capabilities
  , entryQueue :: Q.Seq HubMessage, entryCurrent :: Maybe HubMessage
  , entryResults :: M.Map Int (Either Text Value), entryNextTicket :: Int
  , entryHistory :: Q.Seq HistoryEvent, entryHistoryBytes :: Int, entryNextEvent :: Int
  , entryDropped :: Int, entryExternal :: Bool, entryCancelPending :: Bool }
data HubState = HubState { hubEntries :: M.Map AgentId Entry, hubNextId :: Int, hubClosed :: Bool, hubLastLimits :: HubLimits }
data AgentHub = AgentHub (FilePath -> IO (Either Text HubLimits)) StartProvider (TVar HubState)

emptyCaps :: Capabilities
emptyCaps=Capabilities False False []

-- ACP v1 config IDs and values remain provider-owned. No hard-coded model or
-- reasoning menu is offered. fork={} is the experimental session/fork marker.
parseCapabilities :: Value -> Value -> Capabilities
parseCapabilities initialized session=Capabilities (marker "fork") (marker "resume" || field "loadSession" caps==Just True) choices
  where
    caps=fromMaybe Null (field "agentCapabilities" initialized)
    marker name=case field "sessionCapabilities" caps >>= field name of Just (Object _)->True; _->False
    choices=take 128 (mapMaybe choice (fromMaybe [] (field "configOptions" session)))
    choice value=do
      ident<-field "id" value; category<-field "category" value
      current<-field "currentValue" value
      unless (field "type" value==Just ("select"::Text) && category `elem` ["model","thought_level"] && validSmall ident && validSmall current) Nothing
      let options=take 512 (concatMap option (fromMaybe [] (field "options" value)))
      unless (not (null options)) Nothing
      pure (ConfigChoice ident category current options)
    option value=case (field "value" value,field "name" value) of
      (Just ident,Just label) | validSmall ident && validSmall label -> [(ident,label)]
      _->concatMap (\group->case (field "value" group,field "name" group) of
           (Just ident,Just label) | validSmall ident && validSmall label -> [(ident,label)]; _->[]) (fromMaybe [] (field "options" value))
    validSmall t=not (T.null t) && T.length t<=4096

field :: FromJSON a => Key -> Value -> Maybe a
field key= parseMaybe (withObject "field" (.: key))

newAgentHub :: HubLimits -> StartProvider -> IO AgentHub
newAgentHub limits launcher = case checkedLimits limits of
  Left err -> ioError (userError (T.unpack err))
  Right checked -> do
    hub@(AgentHub _ _ ref) <- newAgentHubWithLimits (const (pure (Right checked))) launcher
    atomically $ modifyTVar' ref $ \state -> state {hubLastLimits = checked}
    pure hub

newAgentHubWithLimits :: (FilePath -> IO (Either Text HubLimits)) -> StartProvider -> IO AgentHub
newAgentHubWithLimits limits launcher=AgentHub limits launcher <$> newTVarIO (HubState M.empty 1 False (HubLimits 8 4))

checkedLimits :: HubLimits -> Either Text HubLimits
checkedLimits limits=if totalActiveAgents limits>=1 && totalActiveAgents limits<=128 && directSubagents limits>=0 && directSubagents limits<=128 then Right limits else Left "Invalid configured agent limits."

active :: Entry -> Bool
active entry=entryPhase entry `elem` [Starting,Idle,Running,Cancelling]

validActor :: Actor -> HubState -> Either Text ()
validActor Human _=Right ()
validActor (Agent ident) state=case M.lookup ident (hubEntries state) of
  Just entry | active entry->Right ()
  _->Left "The calling agent is not active."

validateName :: Text -> Either Text Text
validateName raw=let name=T.strip raw in
  if T.null name || T.length name>80 || T.any isControl name then Left "Agent names must contain 1–80 characters without control characters." else Right name

nameFree :: Maybe AgentId -> Text -> HubState -> Bool
nameFree excluded name state=all (\entry->Just (entryId entry)==excluded || T.toCaseFold (spawnName (entrySpec entry))/=T.toCaseFold name) (M.elems (hubEntries state))

validateSpec :: SpawnSpec -> Either Text SpawnSpec
validateSpec spec=do
  name<-validateName (spawnName spec)
  unless (not (T.null (T.strip (spawnTask spec))) && T.length (spawnTask spec)<=65536 && not (T.any (=='\0') (spawnTask spec))) (Left "Agent task must contain 1–65536 characters without NUL.")
  unless (isAbsolute (spawnDirectory spec) && length (spawnDirectory spec)<=32768 && '\0' `notElem` spawnDirectory spec) (Left "Agent working directory must be absolute.")
  unless (all (maybe True (\x->not (T.null x) && T.length x<=4096)) [spawnModel spec,spawnEffort spec]) (Left "Invalid model or effort identifier.")
  case spawnWorkspace spec of
    Worktree ref branch nameHint->unless (all (maybe True (\x->not (T.null x) && T.length x<=4096 && not (T.any isControl x))) [ref,branch,nameHint]) (Left "Invalid worktree option.")
    _->pure ()
  pure spec {spawnName=name}

reserve :: AgentHub -> HubLimits -> Actor -> SpawnSpec -> Bool -> STM (Either Text (Entry,Maybe PrivateSource))
reserve (AgentHub _ _ ref) limits actor raw external=do
  state<-readTVar ref
  case prepare state of
    Left err->pure (Left err)
    Right (spec,source)->do
      let ident=AgentId ("agent-"<>T.pack (show (hubNextId state)))
          entry=appendEvent "created" actor (object ["name" .= spawnName spec,"task" .= spawnTask spec])
            (Entry ident (case actor of Human->Nothing; Agent parent->Just parent) spec Starting Nothing Nothing emptyCaps Q.empty Nothing M.empty 1 Q.empty 0 1 0 external False)
      writeTVar ref state {hubEntries=M.insert ident entry (hubEntries state),hubNextId=hubNextId state+1,hubLastLimits=limits}
      pure (Right (entry,source))
  where
    prepare state=do
      validActor actor state
      unless (not (hubClosed state)) (Left "Agent directory is closed.")
      unless (M.size (hubEntries state)<1024) (Left "Agent directory capacity reached.")
      spec<-validateSpec raw
      unless (nameFree Nothing (spawnName spec) state) (Left "An agent already has that name.")
      unless (length (filter active (M.elems (hubEntries state)))<totalActiveAgents limits) (Left "Total active-agent limit reached.")
      case actor of
        Agent parent->unless (length [() | entry<-M.elems (hubEntries state),entryParent entry==Just parent,active entry]<directSubagents limits) (Left "Direct sub-agent limit reached.")
        Human->pure ()
      source<-case spawnContext spec of
        Fresh->pure Nothing
        Fork ident->case M.lookup ident (hubEntries state) of
          Just entry | active entry && supportsFork (entryCaps entry),Just key<-entryKey entry->do
            unless (actor==Human || actor==Agent ident) (Left "An agent may only fork its own provider session.")
            pure (Just (PrivateSource ident key))
          _->Left "The source provider does not advertise live session forking."
      pure (spec,source)

spawnAgent :: AgentHub -> Actor -> SpawnSpec -> IO (Either Text AgentId)
spawnAgent hub@(AgentHub readLimits launcher _) actor spec=mask $ \restore->do
  limits<-restore (safeCall (readLimits (spawnDirectory spec)))
  reserved<-case limits >>= checkedLimits of Left err->pure (Left err); Right effective->atomically (reserve hub effective actor spec False)
  case reserved of
    Left err->pure (Left err)
    Right (entry,source)->do
      let ident=entryId entry
          abort=failStart hub ident "Agent startup cancelled."
      started<-restore (safeCall (launcher (StartRequest ident actor (entrySpec entry) source) (recordDriverEvent hub ident))) `onException` abort
      case started of
        Left err->failStart hub ident err >> pure (Left err)
        Right driver->do
          configured<-restore (configureDriver (entrySpec entry) driver) `onException` (stopQuiet driver >> abort)
          case configured of
            Left err->stopQuiet driver >> failStart hub ident err >> pure (Left err)
            Right configuredDriver->do
              accepted<-installDriver hub ident configuredDriver
              if not accepted then stopQuiet driver >> pure (Left "Agent ended during startup.") else pure (Right ident)

registerAgent :: AgentHub -> Text -> FilePath -> AgentDriver -> IO (Either Text AgentId)
registerAgent hub@(AgentHub readLimits _ _) name directory driver=mask $ \restore ->do
  limits<-restore (safeCall (readLimits directory))
  reserved<-case limits >>= checkedLimits of Left err->pure (Left err); Right effective->atomically (reserve hub effective Human (SpawnSpec name "Human-driven conversation" directory Shared Fresh Nothing Nothing) True)
  case reserved of
    Left err->pure (Left err)
    Right (entry,_)->do
      accepted<-installDriver hub (entryId entry) driver
      if accepted then pure (Right (entryId entry)) else pure (Left "Agent directory closed during registration.")

-- The primary conversation retains its public identity across provider reconnects.
-- These host-only hooks are deliberately absent from tool argument decoders.
updateExternalAgent :: AgentHub -> AgentId -> AgentDriver -> IO (Either Text ())
updateExternalAgent hub@(AgentHub readLimits _ ref) ident driver=mask $ \restore->do
  loaded<-restore (safeCall (readLimits (driverDirectory driver)))
  result<-atomically $ do
    state<-readTVar ref
    case loaded >>= checkedLimits of
      Left err->pure (Left err)
      Right limits->case M.lookup ident (hubEntries state) of
        Just entry | entryExternal entry && (active entry || entryPhase entry==Recovered),not (hubClosed state)->do
          let reviving=entryPhase entry==Recovered
          if reviving && length (filter active (M.elems (hubEntries state)))>=totalActiveAgents limits then pure (Left "Total active-agent limit reached.") else do
            let next=appendEvent "provider_updated" Human Null entry {entryDriver=Just driver,entryKey=if T.null (driverSessionKey driver) then Nothing else Just (driverSessionKey driver),entryCaps=driverCapabilities driver,entrySpec=(entrySpec entry) {spawnDirectory=driverDirectory driver},entryPhase=if reviving then Idle else entryPhase entry}
            writeTVar ref state {hubEntries=M.insert ident next (hubEntries state),hubLastLimits=limits}
            pure (Right reviving)
        _->pure (Left "Unknown or ended external agent.")
  case result of
    Left err->pure (Left err)
    Right reviving->do
      if reviving then void (forkIOWithUnmask (\unmask->unmask (worker hub ident))) else pure ()
      pure (Right ())

setExternalAgentBusy :: AgentHub -> AgentId -> Bool -> IO ()
setExternalAgentBusy (AgentHub _ _ ref) ident busy=atomically $ modifyTVar' ref $ \state->state {hubEntries=M.adjust
  (\entry->if entryExternal entry && entryPhase entry `elem` [Idle,Running] && entryCurrent entry==Nothing then entry {entryPhase=if busy then Running else Idle} else entry) ident (hubEntries state)}

configureDriver :: SpawnSpec -> AgentDriver -> IO (Either Text AgentDriver)
configureDriver spec driver
  | not (isAbsolute (driverDirectory driver)) || length (driverDirectory driver)>32768 || '\0' `elem` driverDirectory driver=pure (Left "Provider returned an invalid working directory.")
  | T.null (driverSessionKey driver) || T.length (driverSessionKey driver)>65536=pure (Left "Provider returned an invalid session identity.")
  | otherwise=case traverse choose [("model",spawnModel spec),("thought_level",spawnEffort spec)] of
      Left err->pure (Left err)
      Right settings->fmap (\caps -> driver {driverCapabilities=caps}) <$> safeCall (driverConfigure driver (concat settings))
  where choose (_,Nothing)=Right []
        choose (category,Just value)=case [configId choice | choice<-configChoices (driverCapabilities driver),configCategory choice==category,value `elem` map fst (configValues choice)] of
          [ident]->Right [(ident,value)]
          _->Left "Requested model or effort is not uniquely advertised by this provider."

installDriver :: AgentHub -> AgentId -> AgentDriver -> IO Bool
installDriver hub@(AgentHub _ _ ref) ident driver=do
  accepted<-atomically $ do
    state<-readTVar ref
    case M.lookup ident (hubEntries state) of
      Just entry | entryPhase entry==Starting && not (hubClosed state)->do
        let next=appendEvent "ready" Human Null entry {entryDriver=Just driver,entryKey=if T.null (driverSessionKey driver) then Nothing else Just (driverSessionKey driver),entryCaps=driverCapabilities driver,entryPhase=Idle,entrySpec=(entrySpec entry) {spawnDirectory=driverDirectory driver}}
        writeTVar ref state {hubEntries=M.insert ident next (hubEntries state)}
        pure True
      _->pure False
  if accepted then void (forkIOWithUnmask (\unmask->unmask (worker hub ident))) >> pure True else pure False

failStart :: AgentHub -> AgentId -> Text -> IO ()
failStart (AgentHub _ _ ref) ident reason=atomically $ modifyTVar' ref $ \state->state {hubEntries=M.adjust
  (\entry->if entryPhase entry==Starting then appendEvent "failed" Human (String (boundedError reason)) entry {entryPhase=Failed} else entry) ident (hubEntries state)}

-- Each agent has a single prompt worker. The shared STM reservation protects
-- limits even when startup is slow or concurrent; provider IO never holds it.
worker :: AgentHub -> AgentId -> IO ()
worker hub@(AgentHub _ _ ref) ident = do
  next <- atomically $ do
    state <- readTVar ref
    case M.lookup ident (hubEntries state) of
      Nothing -> pure Nothing
      Just entry
        | not (active entry) -> pure Nothing
        | entryPhase entry == Cancelling -> retry
        -- The primary conversation owns its prompt until its host releases it.
        | entryExternal entry && entryPhase entry == Running -> retry
        | otherwise -> case Q.viewl (entryQueue entry) of
            Q.EmptyL -> retry
            message Q.:< rest -> do
              let updated = appendEvent "message_started" (messageAuthor message)
                    (object ["ticket" .= messageTicket message])
                    entry {entryQueue = rest, entryCurrent = Just message, entryPhase = Running}
              writeTVar ref state {hubEntries = M.insert ident updated (hubEntries state)}
              pure ((message,) <$> entryDriver entry)
  case next of
    Nothing -> pure ()
    Just (message, driver) -> do
      result <- safeCall (driverDeliver driver message)
      atomically $ modifyTVar' ref $ \state -> state
        {hubEntries = M.adjust (finish message result) ident (hubEntries state)}
      worker hub ident
  where
    finish message result entry
      | not (active entry)=entry
      | otherwise=let completed=if entryPhase entry==Cancelling then Left "Agent prompt cancelled." else fmap boundedValue result
                      results=M.insert (messageTicket message) (either (Left . boundedError) Right completed) (entryResults entry)
                  in appendEvent "message_finished" (Agent ident) (ticketValue (messageTicket message) completed)
                    entry {entryPhase=if entryCancelPending entry then Cancelling else Idle,entryCurrent=Nothing,entryResults=trimResults results}

sendAgent :: AgentHub -> Actor -> AgentId -> Text -> IO (Either Text Int)
sendAgent (AgentHub _ _ ref) actor ident body=atomically $ do
  state<-readTVar ref
  case prepare state of
    Left err->pure (Left err)
    Right entry->do
      let ticket=entryNextTicket entry
          owner=case entryParent entry of Nothing->Human; Just parent->Agent parent
          message=HubMessage ticket actor body (actor==owner)
          next=appendEvent "message_queued" actor (object ["ticket" .= ticket,"text" .= body,"userSeat" .= messageIsUserSeat message])
            entry {entryQueue=entryQueue entry Q.|> message,entryNextTicket=ticket+1}
      writeTVar ref state {hubEntries=M.insert ident next (hubEntries state)}
      pure (Right ticket)
  where prepare state=do
          validActor actor state
          entry<-maybe (Left "Unknown agent.") Right (M.lookup ident (hubEntries state))
          unless (entryPhase entry `elem` [Idle,Running]) (Left "Agent is not accepting messages.")
          unless (Q.length (entryQueue entry)<32) (Left "Agent message queue is full.")
          unless (not (T.null (T.strip body)) && T.length body<=65536 && not (T.any (=='\0') body)) (Left "Message must contain 1–65536 characters without NUL.")
          pure entry

waitAgent :: AgentHub -> Actor -> AgentId -> Int -> Int -> IO (Either Text Value)
waitAgent (AgentHub _ _ ref) actor ident ticket milliseconds
  | milliseconds<0 || milliseconds>60000=pure (Left "Wait timeout must be 0–60000 milliseconds.")
  | otherwise=do
      result<-if milliseconds==0 then atomically (inspect False) >>= pure . Just else timeout (milliseconds*1000) (atomically (inspect True))
      pure (fromMaybe (Right (object ["status" .= ("running"::Text),"ticket" .= ticket])) result)
  where
    inspect blocking=do
      state<-readTVar ref
      case validActor actor state >> maybe (Left "Unknown agent.") Right (M.lookup ident (hubEntries state)) of
        Left err->pure (Left err)
        Right entry->case M.lookup ticket (entryResults entry) of
          Just result->pure (Right (ticketValue ticket result))
          Nothing | ticket<1 || ticket>=entryNextTicket entry->pure (Left "Unknown message ticket.")
                  | any ((==ticket).messageTicket) (toList (entryQueue entry)) || maybe False ((==ticket).messageTicket) (entryCurrent entry)->
                      if blocking then retry else pure (Right (object ["status" .= ("running"::Text),"ticket" .= ticket]))
                  | otherwise->pure (Left "Message result is no longer retained; inspect history.")

renameAgent :: AgentHub -> Actor -> AgentId -> Text -> IO (Either Text ())
renameAgent (AgentHub _ _ ref) actor ident raw=atomically $ do
  state<-readTVar ref
  case do validActor actor state
          name<-validateName raw
          entry<-maybe (Left "Unknown agent.") Right (M.lookup ident (hubEntries state))
          unless (nameFree (Just ident) name state) (Left "An agent already has that name.")
          pure (entry,name) of
    Left err->pure (Left err)
    Right (entry,name)->do
      let next=appendEvent "renamed" actor (object ["previousName" .= spawnName (entrySpec entry),"name" .= name]) entry {entrySpec=(entrySpec entry) {spawnName=name}}
      writeTVar ref state {hubEntries=M.insert ident next (hubEntries state)}
      pure (Right ())

-- Human permission can authorize a tool call, but an agent can never use this
-- API to become its parent or terminate an ancestor's user seat.
controls :: Actor -> AgentId -> HubState -> Bool
controls Human _ _=True
controls (Agent caller) target state=walk target
  where walk ident=case M.lookup ident (hubEntries state) >>= entryParent of
          Nothing->False
          Just parent->parent==caller || walk parent

cancelAgent :: AgentHub -> Actor -> AgentId -> IO (Either Text ())
cancelAgent (AgentHub _ _ ref) actor ident = mask $ \restore -> do
  result <- atomically $ do
    state <- readTVar ref
    case authorized state of
      Left err -> pure (Left err)
      Right entry
        | entryPhase entry == Cancelling -> pure (Right Nothing)
        | otherwise -> do
            let next = cancelPending "Agent prompt cancelled." entry
                  {entryPhase = Cancelling, entryCancelPending = True}
            writeTVar ref state
              {hubEntries = M.insert ident (appendEvent "cancelled" actor Null next) (hubEntries state)}
            pure (Right (Just (mapM_ driverCancel (entryDriver entry))))
  case result of
    Left err -> pure (Left err)
    Right Nothing -> pure (Right ())
    Right (Just stop) -> restore (safeCall (stop >> pure (Right ()))) `finally` settled
  where
    authorized state = do
      validActor actor state
      unless (controls actor ident state) (Left "Only the human or an ancestor can cancel this agent.")
      entry <- maybe (Left "Unknown agent.") Right (M.lookup ident (hubEntries state))
      unless (entryPhase entry `elem` [Idle, Running, Cancelling]) (Left "Agent is not running.")
      pure entry
    -- Both cancellation IO and the current delivery must finish before reuse.
    settled = atomically $ modifyTVar' ref $ \state -> state
      {hubEntries = M.adjust release ident (hubEntries state)}
    release entry
      | entryPhase entry == Cancelling = entry
          {entryCancelPending = False, entryPhase = if entryCurrent entry == Nothing then Idle else Cancelling}
      | otherwise = entry

endAgent :: AgentHub -> Actor -> AgentId -> IO (Either Text ())
endAgent (AgentHub _ _ ref) actor ident=do
  result<-atomically $ do
    state<-readTVar ref
    case validActor actor state >> (unless (M.member ident (hubEntries state)) (Left "Unknown agent.")) >>
         unless (controls actor ident state) (Left "Only the human or an ancestor can end this agent.") of
      Left err->pure (Left err)
      Right ()->do
        let ending entry=entryId entry==ident || controls (Agent ident) (entryId entry) state
            drivers=[driver | entry<-M.elems (hubEntries state),ending entry,Just driver<-[entryDriver entry]]
            stop entry | ending entry=appendEvent "ended" actor Null (cancelPending "Agent session ended." entry) {entryPhase=Ended,entryCurrent=Nothing,entryDriver=Nothing}
                       | otherwise=entry
        writeTVar ref state {hubEntries=M.map stop (hubEntries state)}
        pure (Right drivers)
  case result of Left err->pure (Left err); Right drivers->mapM_ stopQuiet drivers >> pure (Right ())

closeAgentHub :: AgentHub -> IO ()
closeAgentHub hub@(AgentHub _ _ ref)=do
  atomically (modifyTVar' ref (\state->state {hubClosed=True}))
  state<-readTVarIO ref
  forM_ [entryId entry | entry<-M.elems (hubEntries state),entryParent entry==Nothing] (\ident->void (endAgent hub Human ident))

cancelPending :: Text -> Entry -> Entry
cancelPending reason entry=entry {entryQueue=Q.empty,entryResults=trimResults (foldr (\message->M.insert (messageTicket message) (Left reason))
  (entryResults entry) (toList (entryQueue entry)++maybe [] pure (entryCurrent entry)))}

trimResults :: M.Map Int (Either Text Value) -> M.Map Int (Either Text Value)
trimResults entries=trim (sum (map size (M.elems entries))) entries
  where size :: Either Text Value -> Int
        size=fromIntegral . BL.length . encode . either String id
        trim bytes results | M.size results<=256 && bytes<=4*1024*1024=results
                           | otherwise=case M.minView results of Nothing->results; Just (value,rest)->trim (bytes-size value) rest

ticketValue :: Int -> Either Text Value -> Value
ticketValue ticket result=object (["ticket" .= ticket,"status" .= (either (\reason->if reason `elem` ["Agent prompt cancelled.","Agent session ended."] then "cancelled" else "failed") (const "completed") result::Text)]++either (\err->["error" .= boundedError err]) (\value->["result" .= boundedValue value]) result)

listAgents :: AgentHub -> Actor -> IO (Either Text Value)
listAgents (AgentHub _ _ ref) actor=atomically $ do
  state<-readTVar ref
  let limits=hubLastLimits state
  pure $ validActor actor state >> Right (object ["agents" .= map (entryValue state) (M.elems (hubEntries state)),"totalActiveLimit" .= totalActiveAgents limits,"directSubagentLimit" .= directSubagents limits])
statusAgent :: AgentHub -> Actor -> AgentId -> IO (Either Text Value)
statusAgent (AgentHub _ _ ref) actor ident=atomically $ do
  state<-readTVar ref
  pure $ validActor actor state >> maybe (Left "Unknown agent.") (Right . entryValue state) (M.lookup ident (hubEntries state))
entryValue :: HubState -> Entry -> Value
entryValue state entry=object ["id" .= agentIdText (entryId entry),"name" .= spawnName spec,"task" .= T.take 2048 (spawnTask spec),"taskTruncated" .= (T.length (spawnTask spec)>2048),
  "parentId" .= fmap agentIdText (entryParent entry),"parentName" .= (entryParent entry >>= (fmap (spawnName.entrySpec) . (`M.lookup` hubEntries state))),
  "workspace" .= workspaceValue (spawnWorkspace spec),"status" .= T.toLower (T.pack (show (entryPhase entry))),"cwd" .= spawnDirectory spec,"queued" .= Q.length (entryQueue entry),
  "currentTicket" .= fmap messageTicket (entryCurrent entry),"capabilities" .= capabilitiesValue (entryCaps entry),
  "reconnectable" .= (entryPhase entry==Recovered && entryKey entry/=Nothing && supportsResume (entryCaps entry)),"historyDropped" .= entryDropped entry,"nextEvent" .= entryNextEvent entry,"humanSeat" .= (entryParent entry==Nothing)]
  where spec=entrySpec entry
workspaceValue :: Workspace -> Value
workspaceValue Shared=object ["mode" .= ("shared"::Text)]
workspaceValue (Worktree ref branch name)=object ["mode" .= ("worktree"::Text),"ref" .= ref,"branch" .= branch,"name" .= name]

capabilitiesValue :: Capabilities -> Value
capabilitiesValue caps=object ["fork" .= supportsFork caps,"resume" .= supportsResume caps,"configOptions" .=
  [object ["id" .= configId choice,"type" .= ("select"::Text),"category" .= configCategory choice,"currentValue" .= configCurrent choice,
    "options" .= [object ["value" .= value,"name" .= label] | (value,label)<-configValues choice]] | choice<-configChoices caps]]
actorValue :: Actor -> Value
actorValue Human=object ["kind" .= ("human"::Text)]
actorValue (Agent ident)=object ["kind" .= ("agent"::Text),"id" .= agentIdText ident]
eventValue :: HistoryEvent -> Value
eventValue (HistoryEvent index kind author value)=object ["index" .= index,"kind" .= kind,"author" .= actorValue author,"detail" .= value]

-- Lifecycle is a typed host event, never inferred from provider display text.
recordDriverEvent :: AgentHub -> AgentId -> DriverEvent -> IO ()
recordDriverEvent hub ident (ProviderUpdate kind detail) = recordAgentEvent hub ident kind detail
recordDriverEvent (AgentHub _ _ ref) ident ProviderClosed = atomically $
  modifyTVar' ref $ \state -> state {hubEntries = M.adjust disconnected ident (hubEntries state)}
  where
    disconnected entry
      | active entry = appendEvent "provider_closed" (Agent ident) Null
          (cancelPending "Agent provider disconnected." entry)
            {entryPhase = Failed, entryCurrent = Nothing, entryCancelPending = False}
      | otherwise = entry

recordAgentEvent :: AgentHub -> AgentId -> Text -> Value -> IO ()
recordAgentEvent (AgentHub _ _ ref) ident kind value=atomically $ modifyTVar' ref $ \state->state {hubEntries=M.adjust
  (\entry->if active entry then appendEvent (T.take 128 kind) (Agent ident) value entry else entry) ident (hubEntries state)}
appendEvent :: Text -> Actor -> Value -> Entry -> Entry
appendEvent kind author value entry=trim entry {entryHistory=entryHistory entry Q.|> event,entryHistoryBytes=entryHistoryBytes entry+size,entryNextEvent=entryNextEvent entry+1}
  where event=HistoryEvent (entryNextEvent entry) kind author (boundedValue value)
        size=eventSize event
        trim current | Q.length (entryHistory current)>1024 || entryHistoryBytes current>4*1024*1024=case Q.viewl (entryHistory current) of
                old Q.:< rest->trim current {entryHistory=rest,entryHistoryBytes=entryHistoryBytes current-eventSize old,entryDropped=entryDropped current+1}
                _->current
                     | otherwise=current

eventSize :: HistoryEvent -> Int
eventSize=fromIntegral . BL.length . encode . eventValue
boundedValue :: Value -> Value
boundedValue value=if BL.length (BL.take ((1024*1024-4096)+1) (encode value))<=1024*1024-4096 then value else object ["truncated" .= True,"message" .= ("Event exceeds the 1 MiB history record limit."::Text)]
boundedError :: Text -> Text
boundedError=T.take 4096

historyAgent :: AgentHub -> Actor -> AgentId -> Int -> Int -> IO (Either Text Value)
historyAgent hub actor ident after count=readHistory hub actor ident after count Nothing
searchAgentHistory :: AgentHub -> Actor -> AgentId -> Text -> Int -> Int -> IO (Either Text Value)
searchAgentHistory hub actor ident needle after count
  | T.null needle || T.length needle>4096=pure (Left "History search requires 1–4096 literal characters.")
  | otherwise=readHistory hub actor ident after count (Just (T.toCaseFold needle))
readHistory :: AgentHub -> Actor -> AgentId -> Int -> Int -> Maybe Text -> IO (Either Text Value)
readHistory (AgentHub _ _ ref) actor ident after count needle
  | after<0 || count<1 || count>100=pure (Left "History offset must be nonnegative and count must be 1–100.")
  | otherwise=atomically $ do
      state<-readTVar ref
      pure $ do
        validActor actor state
        entry<-maybe (Left "Unknown agent.") Right (M.lookup ident (hubEntries state))
        let matches=[event | event@(HistoryEvent index _ _ _)<-toList (entryHistory entry),index>after,maybe True (\query->query `T.isInfixOf` T.toCaseFold (jsonText (eventValue event))) needle]
            chosen=takeBytes (1024*1024) (take count matches)
        pure (object ["events" .= map eventValue chosen,"dropped" .= entryDropped entry,"hasMore" .= (length matches>length chosen),
          "nextAfter" .= (case reverse chosen of HistoryEvent index _ _ _:_->index; _->after)])

takeBytes :: Int -> [HistoryEvent] -> [HistoryEvent]
takeBytes _ []=[]
takeBytes remaining (event:rest) | eventSize event<=remaining=event:takeBytes (remaining-eventSize event) rest
                                 | otherwise=[]

jsonText :: Value -> Text
jsonText=TE.decodeUtf8 . BL.toStrict . encode

safeCall :: IO (Either Text a) -> IO (Either Text a)
safeCall action=do
  result<-try action
  case result of
    Right value->pure value
    Left (err::SomeException)->case fromException err::Maybe SomeAsyncException of
      Just async->throwIO async
      Nothing->pure (Left "Agent provider operation failed.")
stopQuiet :: AgentDriver -> IO ()
stopQuiet driver=void (safeCall (driverStop driver >> pure (Right ())))

-- The Value returned here contains private resume keys. Store it only beside
-- the private session checkpoint; never return it through a public tool.
snapshotHub :: AgentHub -> IO Value
snapshotHub (AgentHub _ _ ref)=do
  state<-readTVarIO ref
  pure (object ["schemaVersion" .= (1::Int),"nextId" .= hubNextId state,"agents" .= map persistEntry (M.elems (hubEntries state))])
  where persistEntry entry=object ["id" .= agentIdText (entryId entry),"parent" .= fmap agentIdText (entryParent entry),"name" .= spawnName (entrySpec entry),
          "workspace" .= workspaceValue (spawnWorkspace (entrySpec entry)),"task" .= spawnTask (entrySpec entry),"cwd" .= spawnDirectory (entrySpec entry),"model" .= spawnModel (entrySpec entry),"effort" .= spawnEffort (entrySpec entry),
          "ended" .= (entryPhase entry==Ended),"sessionKey" .= entryKey entry,"capabilities" .= capabilitiesValue (entryCaps entry),"external" .= entryExternal entry,
          "nextTicket" .= entryNextTicket entry,"nextEvent" .= entryNextEvent entry,"dropped" .= entryDropped entry,"history" .= map eventValue (toList (entryHistory entry))]

restoreHub :: HubLimits -> StartProvider -> Value -> IO (Either Text AgentHub)
restoreHub limits _ _ | Left err<-checkedLimits limits=pure (Left err)
restoreHub limits launcher value=case parseEither persisted value of
  Left _->pure (Left "Invalid agent directory checkpoint.")
  Right state->do
    hub@(AgentHub _ _ ref)<-newAgentHub limits launcher
    atomically (writeTVar ref state)
    pure (Right hub)
  where
    persisted=withObject "agent directory" $ \o->do
      version<-o .: "schemaVersion"; unless (version==(1::Int)) (fail "Version")
      next<-o .: "nextId"; unless (next>0 && next<=1000000000) (fail "ID")
      values<-o .: "agents"; unless (length values<=1024) (fail "Count")
      entries<-mapM entryParser values
      let directory=M.fromList [(entryId entry,entry) | entry<-entries]
          validParent entry=case entryParent entry of Nothing->True; Just parent->idNumber parent<idNumber (entryId entry) && M.member parent directory
      unless (M.size directory==length entries && all validParent entries && all ((<next).idNumber.entryId) entries &&
        M.size (M.fromList [(T.toCaseFold (spawnName (entrySpec entry)),()) | entry<-entries])==length entries) (fail "Duplicate or invalid identity")
      pure (HubState directory next False limits)
    entryParser=withObject "agent" $ \o->do
      ident<-o .: "id" >>= parseId
      parent<-o .: "parent" >>= traverse parseId
      spec<-SpawnSpec <$> o .: "name" <*> o .: "task" <*> o .: "cwd" <*> (o .: "workspace" >>= workspaceParser) <*> pure Fresh <*> o .: "model" <*> o .: "effort"
      checked<-either (fail.T.unpack) pure (validateSpec spec)
      key<-o .: "sessionKey"; unless (maybe True (\t->not (T.null t) && T.length t<=65536) key) (fail "Key")
      external<-o .: "external"
      ended<-o .: "ended"
      capsValue<-o .: "capabilities"
      let caps=parseCapabilities (object ["agentCapabilities" .= object ["loadSession" .= (field "resume" capsValue==Just True),"sessionCapabilities" .= object ["fork" .= (if field "fork" capsValue==Just True then object [] else Null)] ]]) capsValue
      ticket<-o .: "nextTicket"; index<-o .: "nextEvent"; dropped<-o .: "dropped"
      unless (ticket>0 && ticket<=1000000000 && index>0 && index<=1000000000 && dropped>=0 && dropped<index) (fail "Counters")
      history<-o .: "history" >>= mapM eventParser
      unless (length history<=1024 && sum (map eventSize history)<=4*1024*1024 && strictlyIncreasing [n | HistoryEvent n _ _ _<-history] && all (\(HistoryEvent n _ _ _)->n>0 && n<index) history) (fail "History")
      pure (Entry ident parent checked (if ended then Ended else Recovered) Nothing key caps Q.empty Nothing M.empty ticket (Q.fromList history) (sum (map eventSize history)) index dropped external False)
    eventParser=withObject "event" $ \o->HistoryEvent <$> o .: "index" <*> (o .: "kind" >>= shortText) <*> (o .: "author" >>= actorParser) <*> o .: "detail"
    actorParser=withObject "actor" $ \o->do
      kind<-o .: "kind"::Parser Text
      case kind of "human"->pure Human; "agent"->Agent <$> (o .: "id" >>= parseId); _->fail "Actor"
    parseId text=case T.stripPrefix "agent-" text >>= readMaybe . T.unpack of
      Just n | n>0 && n<=1000000000 && text=="agent-"<>T.pack (show (n::Int))->pure (AgentId text)
      _->fail "ID"
    idNumber (AgentId text)=fromMaybe 0 (readMaybe (T.unpack (T.drop 6 text))::Maybe Int)
    workspaceParser=withObject "workspace" $ \o->do
      mode<-o .: "mode"::Parser Text
      case mode of "shared"->pure Shared; "worktree"->Worktree <$> o .: "ref" <*> o .: "branch" <*> o .: "name"; _->fail "Workspace"
    shortText text=if T.length text<=128 then pure text else fail "Text"
    strictlyIncreasing xs=and (zipWith (<) xs (drop 1 xs))

-- Preserve the live policy reader after restoration as well as at first launch.
restoreHubWithLimits :: (FilePath -> IO (Either Text HubLimits)) -> StartProvider -> Value -> IO (Either Text AgentHub)
restoreHubWithLimits reader launcher value=do
  restored<-restoreHub (HubLimits 8 4) launcher value
  case restored of
    Left err->pure (Left err)
    Right (AgentHub _ _ ref)->do
      state<-readTVarIO ref
      let directory=case [spawnDirectory (entrySpec entry) | entry<-M.elems (hubEntries state),entryParent entry==Nothing] of path:_->path; _->"."
      loaded<-safeCall (reader directory)
      case loaded >>= checkedLimits of
        Left err->pure (Left err)
        Right limits->do
          atomically (modifyTVar' ref (\current->current {hubLastLimits=limits}))
          pure (Right (AgentHub reader launcher ref))
