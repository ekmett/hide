{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
-- | Typed commands with explicit wire codecs and revocable registrations.
--
-- Registration, lookup and admission hold a short registry lock. Codecs and
-- handlers run after it is released; nested invocation cannot hold that lock
-- hostage. Retirement rejects future admission, including deferred calls, while
-- already admitted work may finish. A registration is not an MCP exposure or an
-- authorization grant: the host supplies context and checks caller policy.
module Hide.Plugin.Command
  ( Registry, Command, CommandRef, Codec(..), CommandDef(..), CommandInfo(..)
  , CommandError(..), withRegistry, registerCommand, retireCommand, commandRef
  , resolveCommand, registeredCommands, commandCurrent, invoke, invokeJSON
  ) where

import Control.Concurrent.MVar
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate, SomeException, SomeAsyncException, bracket, fromException, throwIO, try)
import Data.Aeson (Value)
import Data.Char (isAsciiLower)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (Unique, newUnique)
import GHC.Generics (Generic)

-- | External schema and conversions are explicit, not inferred from FromJSON.
data Codec a = Codec
  { codecSchema :: Value, codecDecode :: Value -> Either Text a, codecEncode :: a -> Value }

-- | The host's context contains only capabilities granted to this command set.
-- Handlers perform IO on the caller's worker, never under the registry lock.
data CommandDef context a b = CommandDef
  { commandName :: Text, commandTitle :: Text
  , commandInput :: Codec a, commandOutput :: Codec b
  , commandRun :: context -> a -> IO (Either CommandError b)
  }

-- | Discoverable metadata; reading it neither invokes nor authorizes a command.
data CommandInfo = CommandInfo
  { registeredName :: Text, registeredTitle :: Text
  , inputSchema :: Value, outputSchema :: Value
  } deriving (Eq,Show)

data CommandError = RegistryClosed | InvalidCommandName Text | DuplicateCommand Text
  | UnknownCommand Text | StaleCommand Text | InvalidArguments Text
  | CommandRejected Text | CommandFailed Text
  deriving (Eq,Show,Generic)

instance NFData CommandError

-- | Opaque registration identity suitable for delayed wire invocations.
data CommandRef = CommandRef Unique Integer Text
-- The typed definition cannot be paired with an invented registration externally.
data Command context a b = Command CommandRef (CommandDef context a b)
data Entry context = forall a b. Entry Integer (CommandDef context a b)
data State context = State Bool Integer (M.Map Text (Entry context))
data Registry context = Registry Unique (MVar (State context))

-- | Scope registrations. Closing rejects new calls, without cancelling work
-- which already passed admission; task supervision belongs to the host.
withRegistry :: (Registry context -> IO a) -> IO a
withRegistry = bracket acquire close
  where
    acquire=Registry <$> newUnique <*> newMVar (State False 0 M.empty)
    close (Registry _ state)=modifyMVar_ state $ \(State _ generation _)->pure (State True generation M.empty)

-- | Register a unique namespaced command; a live name is never replaced.
registerCommand :: Registry context -> CommandDef context a b -> IO (Either CommandError (Command context a b))
registerCommand (Registry ident state) definition
  | not (validName name)=pure (Left (InvalidCommandName name))
  | otherwise=modifyMVar state $ \current@(State closed generation entries)->
      if closed then pure (current,Left RegistryClosed)
      else if M.member name entries then pure (current,Left (DuplicateCommand name))
      else let next=generation+1
               reference=CommandRef ident next name
           in pure (State False next (M.insert name (Entry next definition) entries),Right (Command reference definition))
  where name=commandName definition

validName :: Text -> Bool
validName name=T.length name<=128 && length segments>=2 && all validSegment segments
  where
    segments=T.splitOn "." name
    validSegment segment=case T.uncons segment of
      Just (first,rest)->isAsciiLower first && T.all (\c->isAsciiLower c || (c>='0' && c<='9') || c=='-') rest
      Nothing->False

-- | Capture the exact registration represented by a typed handle.
commandRef :: Command context a b -> CommandRef
commandRef (Command reference _)=reference

-- | Remove discovery and future admission without waiting for admitted work.
retireCommand :: Registry context -> CommandRef -> IO (Either CommandError ())
retireCommand (Registry ident state) reference@(CommandRef _ _ name)=modifyMVar state $ \current@(State closed generation entries)->
  case currentEntry ident closed entries reference of
    Left err->pure (current,Left err)
    Right _->pure (State False generation (M.delete name entries),Right ())

-- | Capture a generation, not just a name, before queueing or awaiting approval.
resolveCommand :: Registry context -> Text -> IO (Either CommandError CommandRef)
resolveCommand (Registry ident state) name=withMVar state $ \(State closed _ entries)->pure $
  if closed then Left RegistryClosed else case M.lookup name entries of
    Nothing->Left (UnknownCommand name)
    Just (Entry generation _)->Right (CommandRef ident generation name)

-- | Snapshot live schemas/titles. A closed registry has no discoverable commands.
registeredCommands :: Registry context -> IO [CommandInfo]
registeredCommands (Registry _ state)=withMVar state $ \(State _ _ entries)->pure
  [CommandInfo (commandName definition) (commandTitle definition) (codecSchema (commandInput definition)) (codecSchema (commandOutput definition)) | Entry _ definition<-M.elems entries]

currentEntry :: Unique -> Bool -> M.Map Text (Entry context) -> CommandRef -> Either CommandError (Entry context)
currentEntry ident closed entries (CommandRef owner generation name)
  | closed=Left RegistryClosed
  | owner/=ident=Left (StaleCommand name)
  | otherwise=case M.lookup name entries of
      Just entry@(Entry current _) | current==generation -> Right entry
      _ -> Left (StaleCommand name)

admit :: Registry context -> CommandRef -> IO (Either CommandError (Entry context))
admit (Registry ident state) reference=withMVar state $ \(State closed _ entries)->pure (currentEntry ident closed entries reference)

-- | Check exact registration liveness without running codecs or handlers. Hosts
-- use this again before adopting a delayed reply; retirement cannot resurrect UI.
commandCurrent :: Registry context -> CommandRef -> IO Bool
commandCurrent registry reference=either (const False) (const True) <$> admit registry reference

-- | Invoke a typed handle only while that exact registration is live.
-- Successful typed values remain lazy: callers own their evaluation, including
-- exceptions discovered later. Handler IO and fully evaluated errors are contained.
-- Use invokeJSON for a fully evaluated wire reply.
invoke :: Registry context -> Command context a b -> context -> a -> IO (Either CommandError b)
invoke registry (Command reference definition) context arguments=do
  admitted<-admit registry reference
  case admitted of
    Left err->pure (Left err)
    Right _->runHandler (commandRun definition context arguments)

-- | Resolve and validate explicit wire arguments after admission, outside locks.
-- A deferred call must retain its original reference rather than resolve by name
-- again: replacement of a retired command must not redirect approved work.
invokeJSON :: Registry context -> CommandRef -> context -> Value -> IO (Either CommandError Value)
invokeJSON registry reference context arguments=do
  admitted<-admit registry reference
  case admitted of
    Left err->pure (Left err)
    Right (Entry _ definition)->runHandler $ case codecDecode (commandInput definition) arguments of
      Left err->pure (Left (InvalidArguments err))
      Right parsed->do
        result<-commandRun definition context parsed
        case result of
          Left err->pure (Left err)
          Right value->Right <$> evaluate (force (codecEncode (commandOutput definition) value))

-- Async cancellation retains its exception semantics; synchronous failures have
-- an explicit result. Expected denials should return CommandRejected instead.
runHandler :: IO (Either CommandError a) -> IO (Either CommandError a)
runHandler action=do
  result<-try $ do
    reply<-action
    case reply of
      Left err->Left <$> evaluate (force err)
      Right value->pure (Right value)
  case result of
    Left exception | Just (_ :: SomeAsyncException)<-fromException exception -> throwIO exception
                   | otherwise -> pure (Left (CommandFailed (T.pack (show (exception :: SomeException)))))
    Right value->pure value
