-- | Host adapters for immutable plugin reads and conservative version checks.
--
-- The host applies its session/privacy policy before capture. No authority is
-- granted by these adapters. Identity capture/checks evaluate only the buffer
-- constructor; they never compare contents, baselines or history. Keep checks
-- under the owning session lock when adopting delayed work.
module Hide.Plugin.BufferHost
  ( BufferRef, BufferNamespace, newBufferNamespace, bufferReference, referenceId
  , ContentVersion, captureRead, captureVersion, versionCurrent ) where

import Control.Exception (evaluate)
import Data.Unique (Unique,newUnique)
import System.Mem.StableName (StableName, makeStableName)
import Hide.Buffer (Buffer, BufferContent, bufferContent, revision)

-- | One live editor session namespace. Numeric buffer IDs are allocated once by
-- the owning session; a new daemon/recovery lifetime receives a fresh namespace.
newtype BufferNamespace = BufferNamespace Unique
-- | Logical document instance in one session, distinct from a content version.
-- Reload/edit retains the instance; close and reopen allocates another ID.
data BufferRef = BufferRef Unique Int deriving Eq

newBufferNamespace :: IO BufferNamespace
newBufferNamespace = BufferNamespace <$> newUnique

bufferReference :: BufferNamespace -> Int -> BufferRef
bufferReference (BufferNamespace owner) = BufferRef owner

referenceId :: BufferNamespace -> BufferRef -> Maybe Int
referenceId (BufferNamespace owner) (BufferRef actual ident)
  | owner==actual = Just ident
  | otherwise = Nothing

-- | Host-issued identity of an immutable buffer value and its edit revision.
-- Equal numeric revisions do not establish equality. Stable names do not retain
-- the buffer or its Undo; conservative replacement requires a fresh capture.
data ContentVersion = ContentVersion !Int !(StableName Buffer) deriving Eq

-- | Capture only the measured live tree and representation, without flattening.
-- Captured reads remain usable after the editable buffer is closed or replaced.
captureRead :: Buffer -> BufferContent
captureRead = bufferContent

-- | Capture after host policy checks, from the same buffer as the read image.
captureVersion :: Buffer -> IO ContentVersion
captureVersion b=do
  evaluated<-evaluate b
  identity<-makeStableName evaluated
  pure (ContentVersion (revision evaluated) identity)

-- | Check current immutable identity and revision without payload traversal.
-- The caller also revalidates target existence and current authority.
versionCurrent :: ContentVersion -> Buffer -> IO Bool
versionCurrent expected b=(==expected) <$> captureVersion b
