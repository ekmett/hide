module THC.Edit.Browser (Entry(..), readDirectory, matchPattern) where

import Data.Text (Text)

data Entry = Entry { entryName :: Text, entryDirectory :: Bool, entryBytes :: Maybe Integer }
  deriving (Eq, Show)

readDirectory :: FilePath -> Text -> IO (Either String (FilePath, [Entry]))
readDirectory _ _ = pure (Left "not implemented")

matchPattern :: Text -> Text -> Bool
matchPattern _ _ = False
