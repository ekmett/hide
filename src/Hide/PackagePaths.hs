{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.PackagePaths
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Candidate paths for unconfigured Cabal components. This is a worker service:
-- declarations keep their conditions, and a file's existence does not establish
-- that its branch is selected. No directory walk, Cabal invocation or plan read
-- is needed. Canonical workspace/privacy checks precede source-file probes.
module Hide.PackagePaths
  ( SourceCandidate(..), CandidatePath(..), resolveSources
  ) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (canonicalizePath,doesFileExist)
import System.FilePath ((</>),(<.>),isAbsolute,makeRelative,normalise,splitDirectories)
import System.IO.Error (tryIOError)
import Hide.GuestAccess (protectedFilePath)
import Distribution.PackageDescription (Condition(..),ConfVar,CondTree(..),CondBranch(..))
import Distribution.Types.Condition (cAnd,cOr,cNot)
import Hide.PackageSources

-- | One declared source with the conditions under which it may be used.
data SourceCandidate = SourceCandidate
  { candidateSource :: !Source, candidateCondition :: !(Condition ConfVar)
  , candidatePaths :: [CandidatePath] } deriving (Eq,Show)
-- | A declared path and its branch condition. Only an admitted existing file
-- receives an openable canonical path; missing/generated sources stay visible.
data CandidatePath = CandidatePath
  { declaredPath :: !FilePath, pathCondition :: !(Condition ConfVar)
  , existingPath :: !(Maybe FilePath) } deriving (Eq,Show)

-- | Resolve bounded candidates inside a canonical workspace. The second argument
-- holds private paths; package root is independent of the workspace root.
resolveSources :: FilePath -> [FilePath] -> FilePath -> ComponentSources -> IO (Either Text [SourceCandidate])
resolveSources workspace private package component=do
  checked<-tryIOError $ do
    root<-canonicalizePath workspace
    base<-canonicalizePath package
    if not (within root base) || protectedFilePath private base
      then pure (Left "Package source root is outside the public workspace.")
      else case declarations 0 (Lit True) (sourceTree component) of
        Left err->pure (Left err)
        Right groups->case candidates groups of
          Left err->pure (Left err)
          Right values->do
            -- Resolve each spelling once even when components/branches share it.
            let paths=M.fromList [(path,()) | (_,_,entries)<-values,(path,_)<-entries]
            known<-traverseWithPath (probe root base) paths
            pure (Right [SourceCandidate source guard
              [CandidatePath path condition (M.findWithDefault Nothing path known) | (path,condition)<-entries]
              | (source,guard,entries)<-values])
  pure (either (const (Left "Could not inspect package source paths.")) id checked)
  where
    declarations :: Int -> Condition ConfVar -> CondTree ConfVar a SourceGroup -> Either Text [(Condition ConfVar,SourceGroup)]
    declarations depth guard tree
      | depth>64=Left "Package condition nesting exceeds 64 levels."
      | otherwise=do
          children<-mapM (branch depth guard) (condTreeComponents tree)
          let values=(guard,condTreeData tree):concat children
          if length (take 4097 values)>4096 then Left "Package has more than 4096 conditional groups." else Right values
    branch depth guard (CondBranch condition yes no)=do
      a<-declarations (depth+1) (cAnd guard condition) yes
      b<-maybe (Right []) (declarations (depth+1) (cAnd guard (cNot condition))) no
      pure (a++b)
    candidates groups=
      let dirs accessor=[(path,guard) | (guard,group)<-groups,path<-accessor group]
          withDefault values=values++[(".",cNot (foldr cOr (Lit False) (map snd values)))]
          haskell=withDefault (dirs sourceDirectories)
          headers=withDefault (dirs sourceIncludeDirectories)
          modulePaths name extensions=[T.unpack (T.replace "." "/" name)<.>extension | extension<-extensions]
          sourcePaths source=case source of
            ModuleSource name _->under haskell (modulePaths name ["hs","lhs","hsc","chs","x","y","ly"])
            SignatureSource name->under haskell (modulePaths name ["hsig","lhsig"])
            DriverSource name->under haskell (modulePaths name ["hs","lhs","hsc","chs","x","y","ly"])
            MainSource path->under haskell [path]
            PackageFileSource path->[(path,Lit True)]
            IncludeSource path _->under headers [path]
            VirtualSource _->[]
          under locations names=[(directory </> name,guard) | (directory,guard)<-locations,name<-names,guard/=Lit False]
          entries=[(source,guard,[(normalise path,cAnd guard condition) | (path,condition)<-sourcePaths source,cAnd guard condition/=Lit False])
                  | (guard,group)<-groups,source<-sourceEntries group,guard/=Lit False]
          -- Bound all probes before starting IO; never silently discard sources.
          cost=length (take 8193 (concatMap third entries))
      in if length (take 8193 entries)>8192 || cost>8192
           then Left "Package source candidates exceed the 8192-path inspection budget."
           else Right entries
    third (_,_,value)=value
    traverseWithPath run=M.traverseWithKey (\path _->run path)
    probe root base raw=do
      let path=normalise (base </> raw)
      if length raw>32768 || any (< ' ') raw || protectedFilePath private path
        then pure Nothing else do
          checked<-tryIOError $ do
            canonical<-canonicalizePath path
            if not (within root canonical) || protectedFilePath private canonical then pure Nothing else do
              exists<-doesFileExist canonical
              pure (if exists then Just canonical else Nothing)
          pure (either (const Nothing) id checked)
    within root path=let relative=makeRelative root path in not (isAbsolute relative) && ".." `notElem` splitDirectories relative
