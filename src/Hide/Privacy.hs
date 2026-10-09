-- |
-- Module      : Hide.Privacy
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : GHC2021
--
-- Canonical authority-path policy shared by UI projections and service workers.
-- Callers resolve filesystem paths before admission; pure rendering never does IO.
module Hide.Privacy (protectedFilePath, protectedFilePathParent, pathContains) where

import Data.Char (toLower)
import System.FilePath (takeFileName,makeRelative,isAbsolute,splitDirectories)

-- | Project authority files and registered authority roots are private. A root
-- includes descendants, never similarly prefixed siblings. This is editor policy,
-- not an operating-system sandbox or a classifier for arbitrary secret text.
protectedFilePath :: [FilePath] -> FilePath -> Bool
protectedFilePath roots path=map toLower (takeFileName path)=="thc.toml" || any (`pathContains` path) roots

-- | Component-wise containment of canonical absolute paths.
pathContains :: FilePath -> FilePath -> Bool
pathContains root path=let relative=makeRelative root path in not (isAbsolute relative) && ".." `notElem` splitDirectories relative

-- | Protect canonical paths and ancestors containing private authority stores.
protectedFilePathParent :: [FilePath] -> FilePath -> Bool
protectedFilePathParent roots path=protectedFilePath roots path || any (pathContains path) roots
