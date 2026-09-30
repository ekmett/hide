module THC.Edit.Browser (Entry(..), readDirectory, matchPattern) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List
import System.Directory (canonicalizePath, doesDirectoryExist, getFileSize, listDirectory)
import System.FilePath ((</>), takeDirectory)
import System.IO.Error (tryIOError)

data Entry = Entry { entryName :: Text, entryDirectory :: Bool, entryBytes :: Maybe Integer }
  deriving (Eq, Show)

readDirectory :: FilePath -> Text -> IO (Either String (FilePath, [Entry]))
readDirectory directory patternText = fmap (either (Left . show) Right) $ tryIOError $ do
  resolved <- canonicalizePath directory
  names <- listDirectory resolved
  entries <- mapM (describe resolved) names
  let parent = [Entry (T.pack "..") True Nothing | takeDirectory resolved /= resolved]
      visible entry = entryDirectory entry || matchPattern patternText (entryName entry)
      order entry = (not (entryDirectory entry), T.toCaseFold (entryName entry), entryName entry)
  pure (resolved, parent ++ List.sortOn order (filter visible entries))
  where
    describe base name = do
      let path = base </> name
      isDirectory <- doesDirectoryExist path
      size <- if isDirectory then pure Nothing else either (const Nothing) Just <$> tryIOError (getFileSize path)
      pure (Entry (T.pack name) isDirectory size)

matchPattern :: Text -> Text -> Bool
matchPattern patternText name = last (List.foldl' step initial patternChars)
  where
    chars = T.unpack name
    patternChars = if T.null patternText then "*" else T.unpack patternText
    initial = True : replicate (length chars) False
    -- One row of wildcard dynamic programming avoids exponential star backtracking.
    step previous '*' = scanl1 (||) previous
    step previous p = False : zipWith (\matched c -> matched && (p == '?' || p == c)) previous chars
