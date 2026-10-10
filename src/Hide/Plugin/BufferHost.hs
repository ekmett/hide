-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- |
-- Module      : Hide.Plugin.BufferHost
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : GHC2021
--
-- Host adapters for immutable plugin reads and conservative version checks.
--
-- The host applies its session/privacy policy before capture. No authority is
-- granted by these adapters. Identity capture/checks evaluate only the buffer
-- constructor; they never compare contents, baselines or history. Keep checks
-- under the owning session lock when adopting delayed work.
module Hide.Plugin.BufferHost
  ( BufferRef, BufferNamespace, newBufferNamespace, bufferReference, referenceId
  , ContentVersion, captureRead, captureVersion, versionCurrent
  , BufferReader, newBufferReader, readerReference, requestCapture, requestListing
  , ListedBuffer(..)
  , BufferDiff(..), BufferEditor, newBufferEditor, editorReference, requestDiffs, requestDiff, DiffResult(..)
  , CapturedRead(..), BufferMetadata(..) ) where

import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
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
  -- Strict application also removes an HPC tick thunk at this call site.
  -- Name the evaluated buffer constructor, never an instrumentation wrapper.
  identity<-makeStableName $! evaluated
  -- Force the small token before returning: a deferred revision selector
  -- would retain the whole buffer (including Undo) despite the stable name.
  evaluate (ContentVersion (revision evaluated) identity)

-- | Check current immutable identity and revision without payload traversal.
-- The caller also revalidates target existence and current authority.
versionCurrent :: ContentVersion -> Buffer -> IO Bool
versionCurrent expected b=(==expected) <$> captureVersion b

-- | A session-bound ability to request fresh policy decisions. It contains no
-- cached approval or Human context; the host supplies the fixed actor binding.
data BufferReader = BufferReader BufferNamespace (BufferRef -> IO (Either Text CapturedRead))
  (IO (Either Text [ListedBuffer]))

-- | Host-only assembly of a reader over its bounded request transport.
newBufferReader :: BufferNamespace -> (BufferRef -> IO (Either Text CapturedRead))
  -> IO (Either Text [ListedBuffer]) -> BufferReader
newBufferReader = BufferReader

-- | Host wire adapter: references alone confer no capture authority.
readerReference :: BufferReader -> Int -> BufferRef
readerReference (BufferReader namespace _ _) = bufferReference namespace

requestCapture :: BufferReader -> BufferRef -> IO (Either Text CapturedRead)
requestCapture (BufferReader _ request _) = request

-- | Request a complete metadata listing from the same session/actor.
-- References confer no capture or edit authority.
requestListing :: BufferReader -> IO (Either Text [ListedBuffer])
requestListing (BufferReader _ _ request) = request

-- | One listed document. The host preserves metadata privacy and retains only
-- narrow dirty-comparison inputs; there is no content version or source image.
data ListedBuffer = ListedBuffer
  { listedRef :: !BufferRef, listedMetadata :: !BufferMetadata, listedBinary :: !Bool }

-- | Metadata from the same admitted source as the immutable image. The modified
-- flag may require full encoding comparison after a representation switch;
-- evaluate it on a worker. Its thunk contains only those narrow text inputs.
data BufferMetadata = BufferMetadata
  { bufferIdentifier :: !Int, displayName :: !Text, path :: !(Maybe FilePath)
  , modified :: Bool, editRevision :: !Int }

-- | Already granted content. Closing a reader or source cannot recall it.
-- Conversation redaction remains deferred for the reply worker.
data CapturedRead = CapturedRead
  { capturedRef :: !BufferRef, capturedVersion :: !ContentVersion
  , capturedContent :: BufferContent, capturedRedacted :: Bool
  , capturedMetadata :: !BufferMetadata }

-- | One strict unified diff against the exact version from an admitted read.
-- The reference names a session-owned target; construction grants no authority.
data BufferDiff = BufferDiff !BufferRef !ContentVersion !Text

-- | Session/actor-bound atomic diff admission; no cached approval or Human call.
data BufferEditor = BufferEditor BufferNamespace ([BufferDiff] -> IO (Either Text [DiffResult]))

-- | Host-only assembly over the fixed admission transport. The callback owns
-- current policy, actor and target validation; constructing a handle grants none.
newBufferEditor :: BufferNamespace -> ([BufferDiff] -> IO (Either Text [DiffResult])) -> BufferEditor
newBufferEditor = BufferEditor

-- | Host wire adapter: a reference alone confers no edit authority.
editorReference :: BufferEditor -> Int -> BufferRef
editorReference (BufferEditor namespace _) = bufferReference namespace

-- | Submit on a worker; waiting for approval must remain outside session locks.
-- The host validates the entire batch and returns results in input order.
requestDiffs :: BufferEditor -> [BufferDiff] -> IO (Either Text [DiffResult])
requestDiffs (BufferEditor _ request) = request

-- | Singleton convenience over the same batch admission callback. A malformed
-- host result cannot silently select a result from an empty or larger response.
requestDiff :: BufferEditor -> BufferRef -> ContentVersion -> Text -> IO (Either Text DiffResult)
requestDiff editor target version patch=do
  result<-requestDiffs editor [BufferDiff target version patch]
  pure $ result >>= \results->case results of
    [applied]->Right applied
    _->Left (T.pack "Diff host returned an invalid singleton result")

-- | Exact applied review and resulting revision. Success never saves the buffer.
data DiffResult = DiffResult { diffRevision :: !Int, appliedDiff :: !Text, userModified :: !Bool }
