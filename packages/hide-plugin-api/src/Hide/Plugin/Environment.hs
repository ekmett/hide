{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.Plugin.Environment
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Checked requests for the editor environment used by future subprocesses.
-- The host owns workspace/configuration paths, redaction, protected-name policy
-- and mutation. Arguments grant no filesystem, process or human authority.
module Hide.Plugin.Environment
  ( EnvironmentServices(..)
  , GetArguments
  , getArguments
  , getNames
  , SetArguments
  , setArguments
  , setScope
  , setEntries
  , EnvironmentScope(..)
  , scopeText
  , getInput
  , getOutput
  , setInput
  , setOutput
  ) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser,parseEither)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAsciiLower,isAsciiUpper,isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Plugin.Command (Codec(..),CommandError)

-- | Session-scoped operations supplied after host permission admission. Calls
-- execute on the caller's worker and reject retired registrations. The host binds
-- the workspace, redacts reads and checks protected/sensitive names before any
-- write; these callbacks expose no switch to human authority. Changes affect only
-- future subprocesses, and retaining this record cannot prolong its owner's scope.
data EnvironmentServices = EnvironmentServices
  { environmentGet :: GetArguments -> IO (Either CommandError Value)
  , environmentSet :: SetArguments -> IO (Either CommandError Value)
  }

-- | Checked optional name selection. Nothing reads the host's current catalogue;
-- an explicit empty list requests no values. Selection never disables redaction.
data GetArguments = GetArguments (Maybe [Text])

-- | Validate the same selection as the wire request, rejecting empty names,
-- NUL and equals signs. No additional character or count limit is imposed.
--
-- @getNames <$> getArguments names = Right names@ for valid @names@.
getArguments :: Maybe [Text] -> Either Text GetArguments
getArguments names=do
  unless (maybe True (all (\name->not (T.null name) && not (T.any (`elem` ['\0','=']) name))) names)
    (Left "Invalid environment name")
  pure (GetArguments names)

-- | Selected names, or Nothing for the host's current catalogue.
getNames :: GetArguments -> Maybe [Text]
getNames (GetArguments names)=names

-- | Existing overlay scopes. Project entries override global entries; session
-- changes require no persistence. The host resolves all configuration paths.
data EnvironmentScope = SessionEnvironment | ProjectEnvironment | GlobalEnvironment
  deriving (Eq,Show)

-- | Wire spelling of an overlay scope.
scopeText :: EnvironmentScope -> Text
scopeText SessionEnvironment="session"
scopeText ProjectEnvironment="project"
scopeText GlobalEnvironment="global"

-- | Syntactically checked changes. Values are strings without NUL; Nothing
-- represents an explicit unset. Protected and sensitive names remain a separate
-- host authority check, applied to the complete request before mutation.
data SetArguments = SetArguments EnvironmentScope [(Text,Maybe Text)]

-- | Validate an entire change object and scope without performing IO. Null and
-- false both mean unset, preserving the configuration/wire request semantics.
--
-- @setScope <$> setArguments (scopeText scope) changes = Right scope@
-- for syntactically valid @changes@.
setArguments :: Text -> Value -> Either Text SetArguments
setArguments scope changes=do
  entries<-case changes of
    Object values->traverse entry (KM.toList values)
    _->Left "Environment must be an object of names with string values or null (unset)."
  selected<-case scope of
    "session"->Right SessionEnvironment
    "project"->Right ProjectEnvironment
    "global"->Right GlobalEnvironment
    _->Left "Scope must be session, project or global."
  pure (SetArguments selected entries)
  where
    entry (key,value)=do
      let name=K.toText key
          letter c=isAsciiLower c || isAsciiUpper c || c=='_'
      unless (not (T.null name) && letter (T.head name) && T.all (\c->letter c || isDigit c) name)
        (Left "Use environment names containing ASCII letters, digits and underscores, beginning with a letter or underscore.")
      content<-case value of
        String text | not (T.any (=='\0') text)->Right (Just text)
        Null->Right Nothing
        Bool False->Right Nothing
        _->Left "Use a string without NUL, or null/false to unset a variable."
      pure (name,content)

-- | Captured overlay scope, never a caller-supplied configuration path.
setScope :: SetArguments -> EnvironmentScope
setScope (SetArguments scope _)=scope

-- | Validated changes in object traversal order. Read-only projection cannot
-- manufacture unchecked names/values or select a human policy path.
setEntries :: SetArguments -> [(Text,Maybe Text)]
setEntries (SetArguments _ entries)=entries

-- | Strict request codec. Omitted/null names selects the current catalogue;
-- unknown fields are rejected. Explicit [] remains an empty selection.
getInput :: Codec GetArguments
getInput=Codec (objectSchema [] [("names",array string)]) (decodeValue parseArguments) encodeArguments
  where
    parseArguments=withObject "environment arguments" $ \o->do
      only ["names"] o
      names<-o .:? "names"
      checked (getArguments names)
    encodeArguments (GetArguments names)=object ["names" .= selected | Just selected<-[names]]

-- | Concrete redacted read reply: values are strings or null for absent names,
-- with the existing @appliesTo@ marker. Encoding preserves the host's reply.
getOutput :: Codec Value
getOutput=valueOutput (objectSchema ["values","appliesTo"]
  [("values",object ["type" .= ("object"::Text),"additionalProperties" .= nullableString]),
   ("appliesTo",constant "new processes")]) $ withObject "environment read reply" $ \o->do
    only ["values","appliesTo"] o
    entries<-o .: "values" :: Parser Object
    mapM_ validValue (KM.elems entries)
    marker<-o .: "appliesTo"
    unless (marker==("new processes"::Text)) (fail "Invalid environment appliesTo marker.")
  where
    validValue (String _)=pure ()
    validValue Null=pure ()
    validValue _=fail "Environment reply values must be strings or null."

-- | Strict change codec. Scope defaults to session; values is required. Unknown
-- fields, invalid scope/name/value and NUL are rejected before host execution.
setInput :: Codec SetArguments
setInput=Codec (objectSchema ["values"]
  [("values",object ["type" .= ("object"::Text),"additionalProperties" .= changeValue]),
   ("scope",object ["type" .= ("string"::Text),"enum" .= (["session","project","global"]::[Text])])])
  (decodeValue parseArguments) encodeArguments
  where
    parseArguments=withObject "environment arguments" $ \o->do
      only ["values","scope"] o
      scope<-o .:? "scope" .!= "session"
      values<-o .: "values"
      checked (setArguments scope values)
    encodeArguments (SetArguments scope entries)=object
      ["scope" .= scopeText scope,"values" .= object [K.fromText name .= maybe Null String value | (name,value)<-entries]]

-- | Concrete change reply with its applied scope and existing future-process
-- marker. This acknowledges host execution, never grants additional authority.
setOutput :: Codec Value
setOutput=valueOutput (objectSchema ["scope","appliesTo"]
  [("scope",object ["type" .= ("string"::Text),"enum" .= (["session","project","global"]::[Text])]),
   ("appliesTo",constant "new processes; existing processes unchanged")]) $ withObject "environment change reply" $ \o->do
    only ["scope","appliesTo"] o
    scope<-o .: "scope" :: Parser Text
    unless (scope `elem` ["session","project","global"]) (fail "Scope must be session, project or global.")
    marker<-o .: "appliesTo"
    unless (marker==("new processes; existing processes unchanged"::Text)) (fail "Invalid environment appliesTo marker.")

checked :: Either Text a -> Parser a
checked=either (fail . T.unpack) pure

decodeValue :: (Value -> Parser a) -> Value -> Either Text a
decodeValue parser=either (Left . T.pack) Right . parseEither parser

only :: [Key] -> Object -> Parser ()
only allowed o=unless (all (`elem` allowed) (KM.keys o)) (fail "Unknown argument")

valueOutput :: Value -> (Value -> Parser ()) -> Codec Value
valueOutput schema validate=Codec schema (\value->decodeValue validate value >> pure value) id

objectSchema :: [Text] -> [(Key,Value)] -> Value
objectSchema required properties=object ["type" .= ("object"::Text),"required" .= required,
  "additionalProperties" .= False,"properties" .= Object (KM.fromList properties)]

string, nullableString, changeValue :: Value
string=object ["type" .= ("string"::Text)]
nullableString=object ["anyOf" .= [string,object ["type" .= ("null"::Text)]]]
changeValue=object ["anyOf" .= [nullableString,object ["type" .= ("boolean"::Text),"enum" .= [False]]]]

array :: Value -> Value
array value=object ["type" .= ("array"::Text),"items" .= value]

constant :: Text -> Value
constant value=object ["type" .= ("string"::Text),"enum" .= [value]]
