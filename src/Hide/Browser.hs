module Hide.Browser (Entry(..), readDirectory, packageFile, matchPattern) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List
import System.Directory (canonicalizePath, doesDirectoryExist, getFileSize, getModificationTime, listDirectory)
import System.FilePath ((</>), takeDirectory, takeFileName, dropTrailingPathSeparator)
import Data.Time (LocalTime, getCurrentTimeZone, utcToLocalTime)
import System.IO.Error (tryIOError)

data Entry = Entry { entryName :: Text, entryDirectory :: Bool, entryBytes :: Maybe Integer, entryModified :: Maybe LocalTime }
  deriving (Eq, Show)

readDirectory :: FilePath -> Text -> IO (Either String (FilePath, [Entry]))
readDirectory directory patternText = fmap (either (Left . show) Right) $ tryIOError $ do
  resolved <- canonicalizePath directory
  names <- listDirectory resolved
  zone <- getCurrentTimeZone
  entries <- mapM (describe zone resolved) names
  let parent = [Entry (T.pack "..") True Nothing Nothing | takeDirectory resolved /= resolved]
      visible entry = entryDirectory entry || matchPattern patternText (entryName entry)
      order entry = (not (entryDirectory entry), T.toCaseFold (entryName entry), entryName entry)
  pure (resolved, parent ++ List.sortOn order (filter visible entries))
  where
    describe zone base name = do
      let path = base </> name
      isDirectory <- doesDirectoryExist path
      size <- if isDirectory then pure Nothing else either (const Nothing) Just <$> tryIOError (getFileSize path)
      modified <- either (const Nothing) (Just . utcToLocalTime zone) <$> tryIOError (getModificationTime path)
      pure (Entry (T.pack name) isDirectory size modified)

matchPattern :: Text -> Text -> Bool
matchPattern patternText name = last (List.foldl' step initial patternChars)
  where
    chars = T.unpack name
    patternChars = if T.null patternText then "*" else T.unpack patternText
    initial = True : replicate (length chars) False
    -- One row of wildcard dynamic programming avoids exponential star backtracking.
    step previous '*' = scanl1 (||) previous
    step previous p = False : zipWith (\matched c -> matched && (p == '?' || p == c)) previous chars

-- Prefer the directory's namesake package; otherwise use the sorted first file.
packageFile :: FilePath -> IO (Maybe FilePath)
packageFile directory = do
  listing <- readDirectory directory (T.pack "*.cabal")
  pure $ case listing of
    Left _ -> Nothing
    Right (base,entries) ->
      let files=[T.unpack (entryName entry) | entry<-entries,not (entryDirectory entry)]
          namesake=takeFileName (dropTrailingPathSeparator base) ++ ".cabal"
      in case if namesake `elem` files then [namesake] else files of
        name:_ -> Just (base </> name)
        [] -> Nothing
