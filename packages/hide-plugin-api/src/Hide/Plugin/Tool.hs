{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.Tool
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : ExistentialQuantification
--
-- Explicit MCP exposure of scoped typed commands. The command registry owns
-- exact invocation identity and retirement; this adapter adds wire metadata and
-- bounded arguments/results. Permission decisions remain in the host.
module Hide.Plugin.Tool
  ( Tool(..), ToolHints(..), mapToolContext, Tools, withTools, toolDefinitions, hasTool, callTool ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (foldM,unless)
import Data.Aeson (Value(..),object,encode,(.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower)
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Hide.Plugin.Command

-- | Independent MCP annotations. A local question can change state without
-- being destructive or reaching outside the editor. Read-only configures the
-- host's default policy; none of these hints authorizes execution.
data ToolHints = ToolHints
  { readOnlyHint :: !Bool
  , destructiveHint :: !Bool
  , openWorldHint :: !Bool
  } deriving (Eq,Show)

-- | MCP name, explicit policy hints and the typed command to expose.
-- Input schemas are strict objects; output schemas describe objects. Codecs
-- must enforce their stated fields and bounds.
-- Wire arguments have a 1 MiB ceiling and results 4 MiB, matching the host
-- transport. The host supplies an admitted context or a service that owns admission
-- for every call (see 'Hide.Plugin.Session.PluginTool'); arguments cannot supply
-- or replace it.
data Tool c = forall a b. Tool Text ToolHints (CommandDef c a b)

-- | Project a host-granted context without changing tool names, schemas,
-- exposure or registration lifetime. The projection runs only when the command
-- handler executes; metadata discovery never evaluates it.
--
-- @mapToolContext id tool = tool@
--
-- @mapToolContext f (mapToolContext g tool) = mapToolContext (g . f) tool@
mapToolContext :: (d -> c) -> Tool c -> Tool d
mapToolContext project (Tool name hints definition)=Tool name hints
  (CommandDef (commandName definition) (commandTitle definition)
    (commandInput definition) (commandOutput definition)
    (\context->commandRun definition (project context)))

data Tools c = Tools !(Registry c) !(M.Map Text (Value,CommandRef))

-- | Scope an immutable tool set around a session. Reserved host names and
-- overlapping names fail before use, including duplicate command identities.
-- Disjoint declarations compose by concatenation. Metadata discovery does not
-- call a handler. Leaving the scope retires every captured invocation; a later
-- tool with the same name cannot receive an earlier set's call.
--
-- Registration and invocation run outside the UI owner. There is no fallback
-- interpreter and no API to mutate the set while an approval is pending.
withTools :: [Text] -> [Tool c] -> (Tools c -> IO a) -> IO a
withTools reserved declarations use=withRegistry $ \registry->do
  entries<-foldM (register registry) M.empty declarations
  use (Tools registry entries)
  where
    register registry entries (Tool name hints definition)=do
      let input=codecSchema (commandInput definition)
          output=codecSchema (commandOutput definition)
          description=commandTitle definition
          metadata=object ["name" .= name,"description" .= description,"inputSchema" .= input,
            "outputSchema" .= output,"annotations" .= object
              ["readOnlyHint" .= readOnlyHint hints,"destructiveHint" .= destructiveHint hints,"openWorldHint" .= openWorldHint hints]]
      unless (validName name) (failTool "Invalid tool name.")
      unless (name `notElem` reserved && M.notMember name entries) (failTool ("Duplicate or reserved tool name: "<>name))
      unless (not (T.null description) && T.length description<=2048) (failTool "Tool description exceeds its bounds.")
      unless (strictInput input && objectSchema output && within 65536 metadata) (failTool "Tool schemas must be bounded objects; input must declare strict fields.")
      prepared<-evaluate (force metadata)
      registered<-registerCommand registry definition >>= either (failTool . T.pack . show) pure
      pure (M.insert name (prepared,commandRef registered) entries)
    failTool=ioError . userError . T.unpack

-- | Prepared metadata for exactly the declared tools, in stable name order.
toolDefinitions :: Tools c -> [Value]
toolDefinitions (Tools _ entries)=map fst (M.elems entries)

-- | Membership in this exact immutable set, never a fallback admission.
hasTool :: Tools c -> Text -> Bool
hasTool (Tools _ entries) name=M.member name entries

-- | Invoke the captured registration on the caller's worker, after host policy
-- and actor admission, or through an explicitly self-admitting service context.
-- Closing the set rejects retained calls. Command codecs
-- validate typed arguments; their errors and handler failures remain explicit.
-- An oversized reply fails rather than returning a truncated success.
callTool :: Tools c -> c -> Text -> Value -> IO (Either Text Value)
callTool (Tools registry entries) context name arguments=case M.lookup name entries of
  Nothing->pure (Left ("Unknown tool: "<>name))
  Just (_,reference)
    | not (within inputLimit arguments)->pure (Left "Tool arguments exceed 1 MiB.")
    | otherwise->do
        reply<-invokeJSON registry reference context arguments
        pure $ case reply of
          Right value | not (isObject value)->Left "Tool result must be an object."
                      | not (within outputLimit value)->Left "Tool result exceeds 4 MiB."
                      | otherwise->Right value
          Left err->Left (case err of
            InvalidArguments detail->detail
            CommandRejected detail->detail
            CommandFailed detail->detail
            _->T.pack (show err))

inputLimit, outputLimit :: Int64
inputLimit=1024*1024
outputLimit=4*1024*1024

within :: Int64 -> Value -> Bool
within limit value=BL.length (BL.take (limit+1) (encode value))<=limit

validName :: Text -> Bool
validName name=T.length name<=128 && case T.uncons name of
  Just (first,rest)->isAsciiLower first && T.all (\c->isAsciiLower c || c>='0' && c<='9' || c=='_') rest
  Nothing->False

isObject :: Value -> Bool
isObject (Object _)=True
isObject _=False

objectSchema :: Value -> Bool
objectSchema (Object fields)=KM.lookup "type" fields==Just (String "object")
objectSchema _=False

strictInput :: Value -> Bool
strictInput (Object fields)=case (KM.lookup "type" fields,KM.lookup "additionalProperties" fields,KM.lookup "properties" fields,KM.lookup "required" fields) of
  (Just (String "object"),Just (Bool False),Just (Object properties),Just (Array required))->
    let names=[name | String name<-V.toList required]
    in length names==V.length required && length names==S.size (S.fromList names) &&
       all (\name->KM.member (K.fromText name) properties) names
  _->False
strictInput _=False
