-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- |
-- Module      : Hide.Completion
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : GHC2021
--
-- Bash completion from existing option descriptors and directory entries.
--
-- Completion reads GetOpt metadata without evaluating option constructors or
-- executing shell text. Fixed argument choices come from supported options;
-- filesystem candidates are sorted and deduplicated; candidates containing line
-- breaks are discarded.
module Hide.Completion (bashCompletion) where

import Data.List (isPrefixOf, nub, sort)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..))
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeFileName, pathSeparator)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

-- | Complete from the current-word index followed by shell words including
-- the executable. Malformed input returns no candidates.
bashCompletion :: [OptDescr a] -> [String] -> IO [String]
bashCompletion descriptors (index:words')
  | Just n<-readMaybe index, n>0, current:_<-drop n words' =
      sort . nub . filter (all (`notElem` "\r\n")) <$> complete (take (n-1) (drop 1 words')) current
  where
    entries=[(name,argument) | Option shorts longs argument _<-descriptors,
      name<-map (\c -> ['-',c]) shorts++map ("--"++) longs]
    names=map fst entries
    complete ("--":_) current = paths current
    complete (word:rest) current = case lookup word entries of
      Just argument@(ReqArg _ _) -> case rest of
        [] -> pure (values word argument current)
        _:more -> complete more current
      Just argument@(OptArg _ _) -> case rest of
        [] | not ("-" `isPrefixOf` current) -> pure (values word argument current)
        next:more | not ("-" `isPrefixOf` next) -> complete more current
        _ -> complete rest current
      _ -> complete rest current
    complete [] current
      | (name,'=':value)<-break (=='=') current, Just argument<-lookup name entries =
          pure (map ((name++"=")++) (values name argument value))
      | "-" `isPrefixOf` current = pure (filter (isPrefixOf current) names)
      | otherwise = ((if null current then names else [])++) <$> paths current
    values name argument prefix = filter (isPrefixOf prefix) choices
      where
        label=case argument of ReqArg _ text -> text; OptArg _ text -> text; NoArg _ -> ""
        choices | name=="--mode" = ["3","259","0x03","0x103"]
                | '|' `elem` label = splitChoices label
                | otherwise = []
    splitChoices text=case break (=='|') text of
      (part,[]) -> [part]
      (part,_:rest) -> part:splitChoices rest
bashCompletion _ _ = pure []

paths :: FilePath -> IO [FilePath]
paths word = catchIOError enumerate (const (pure []))
  where
    name=takeFileName word
    prefix=take (length word-length name) word
    directory=if null prefix then "." else prefix
    enumerate = do
      entries<-filter (isPrefixOf name) <$> listDirectory directory
      mapM (\entry -> do
        folder<-doesDirectoryExist (directory </> entry)
        pure (prefix++entry++[pathSeparator | folder])) entries
