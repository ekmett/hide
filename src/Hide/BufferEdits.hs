-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.BufferEdits
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Host-owned worker preparation and atomic adoption of buffer edits.
--
-- This is the shared owner extracted from the HLS workspace edit path.
-- Preparation builds/forces immutable replacements on the owning worker;
-- adoption under the session lock checks all targets and current privacy, then
-- splices prepared values with ordinary Undo and live-selection rebasing.
-- Callers own canonical project admission, task cancellation/order and replies.
-- These operations do not grant authority or provide the public plugin service.
module Hide.BufferEdits (PreparedEdit, prepareEdit, prepareBufferEdit, commitEdits) where

import Control.Exception (evaluate)
import Control.Monad (forM)
import qualified Data.Map.Strict as M
import Data.List (sortOn, mapAccumL)
import qualified Data.Text as T
import System.Mem.StableName (StableName, makeStableName)
import Hide.Buffer
import Hide.Files (FileState, filePath)
import Hide.GuestAccess (protectedPath, protectedBuffer)
import Hide.Model
import Hide.Plugin.BufferHost (ContentVersion, captureVersion, versionCurrent)

-- | Worker-prepared replacement and live-selection spans with target identities.
-- Construction is restricted to preparation. Unlike a read image, this owned
-- edit deliberately retains the checked file baseline and ordinary Undo state
-- which will be installed. It has no structural Eq/Show instance.
data PreparedEdit = PreparedEdit (Maybe FilePath) (Maybe (Int,ContentVersion,Maybe (StableName FileState))) (Maybe FileState) Buffer (M.Map Int (Int,Int,Int)) Bool

-- | Prepare character edits as one ordinary Undo step on the owner's worker.
-- The optional ID names an existing buffer; Nothing is a checked closed-file
-- baseline which will open as an unsaved document. The host has already admitted
-- the canonical path/project and source baseline. Full tree, content and span
-- evaluation completes here, before publication. This operation never saves.
prepareEdit :: Maybe Int -> FileState -> Buffer -> [(Int,Int,T.Text)] -> IO (Either T.Text PreparedEdit)
prepareEdit target file = prepareTarget target (Just file)

-- | Prepare an existing logical document, including an untitled buffer. File
-- identity (or its absence) is part of the checked target; no fake path is used.
prepareBufferEdit :: Int -> Maybe FileState -> Buffer -> [(Int,Int,T.Text)] -> IO (Either T.Text PreparedEdit)
prepareBufferEdit bid = prepareTarget (Just bid)

prepareTarget :: Maybe Int -> Maybe FileState -> Buffer -> [(Int,Int,T.Text)] -> IO (Either T.Text PreparedEdit)
prepareTarget target file b edits=do
  original<-case target of
    Nothing->pure Nothing
    Just bid->do
      version<-captureVersion b
      baseline<-traverse (\f->makeStableName =<< evaluate f) file
      pure (Just (bid,version,baseline))
  let sorted=sortOn (\(a,z,_)->(a,z)) edits
  case replaceRanges sorted b of
    Left err->pure (Left err)
    Right updated->do
      let (_,spans)=mapAccumL (\shift (a,z,text)->let n=T.length text in (shift+n-(z-a),(a,(z,n,shift)))) 0 sorted
          ranges=M.fromDistinctAscList spans
      _<-evaluate (prepareBuffer updated)
      _<-evaluate (T.length (contents updated))
      _<-evaluate (M.foldlWithKey' (\() a (z,n,shift)->a `seq` z `seq` n `seq` shift `seq` ()) () ranges)
      pure (Right (PreparedEdit (filePath <$> file) original file updated ranges (revision updated/=revision b)))

-- | Atomically validate all prepared targets, then splice their worker values.
-- The caller holds the session lock and rechecks the owning task's admission
-- before calling. Missing/replaced/ambiguous/private targets reject every edit.
-- Current selections are rebased while unrelated navigation is preserved.
-- This is one session's in-memory operation, with one ordinary Undo per changed
-- buffer; filesystem saving and coordinated workspace Undo are separate.
commitEdits :: [PreparedEdit] -> Desktop -> IO (Either T.Text (Desktop,[(Int,FileState,Buffer)]))
commitEdits patches d
  | M.size (M.fromList [(path,()) | path<-paths])/=length paths ||
    M.size (M.fromList [(bid,()) | bid<-identifiers])/=length identifiers =
      pure (Left "Duplicate prepared edit targets; no files changed.")
  | otherwise = do
    let byPath=M.fromListWith (++) [(filePath file,[(bid,doc)]) | (bid,doc)<-M.toList (buffers d),Just file<-[documentFile doc]]
    checks<-forM patches $ \(PreparedEdit path original _ _ _ _)->do
      let opened=maybe [] (\p->M.findWithDefault [] p byPath) path
          private=maybe False (protectedPath d) path || any (protectedBuffer d . fst) opened
      if private then pure False else case original of
        Nothing->pure (null opened)
        Just (bid,oldBuffer,oldFile)->case M.lookup bid (buffers d) of
          Nothing->pure False
          Just doc | protectedBuffer d bid->pure False
                   | maybe False (const (map fst opened/=[bid])) path->pure False
                   | (filePath <$> documentFile doc)/=path->pure False
                   | otherwise->do
                       bufferCurrent<-versionCurrent oldBuffer (documentBuffer doc)
                       fileIdentity<-traverse (\f->makeStableName =<< evaluate f) (documentFile doc)
                       pure (bufferCurrent && fileIdentity==oldFile)
    pure $ if not (and checks) then Left "An edit target changed or became private; no files changed."
      else let (updated,rebases,changed)=foldl' apply (d,M.empty,[]) patches
               rebased=updated {windows=map (\w->case bufferId w >>= (`M.lookup` rebases) of
                 Nothing->w
                 Just ranges->w {selection=let Selection a c=selection w in Selection (rebase ranges a) (rebase ranges c)}) (windows updated)}
           in Right (rebased,reverse changed)
  where
    paths=[path | PreparedEdit (Just path) _ _ _ _ _<-patches]
    identifiers=[bid | PreparedEdit _ (Just (bid,_,_)) _ _ _ _<-patches]
    apply (desktop,rebases,changed) (PreparedEdit _ original file b ranges modified)=
      let bid=maybe (nextId desktop) (\(ident,_,_)->ident) original
          updated=case original of
            Nothing->addDocument file b desktop
            Just _->desktop {buffers=M.adjust (\doc->restyle doc {documentBuffer=b}) bid (buffers desktop)}
      in (updated,M.insert bid ranges rebases,case file of Just f | modified || maybe True (const False) original -> (bid,f,b):changed; _ -> changed)
    rebase edits offset=case M.lookupLE offset edits of
      Nothing->offset
      Just (a,(z,n,shift)) | offset<z->a+shift+n
                         | otherwise->offset+shift+n-(z-a)
