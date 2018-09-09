module TarArchive ( entries
                  , listEntries
                  , filterEntriesByName
                  , listEntryPaths
                  , listEntryContents
                  ) where

import           Prelude hiding (read)
import           Codec.Archive.Tar
import           Control.Exception (throw)
import qualified Data.ByteString.Lazy as BL

entries :: FilePath -> IO [String]
entries path =
  map entryPath <$> listEntries <$> BL.readFile path

listEntries :: BL.ByteString -> [Entry]
listEntries bs = foldEntries (:) [] throw $ read bs

filterEntriesByName :: (FilePath -> Bool) -> [Entry] -> [Entry]
filterEntriesByName f = filter (f . entryPath)

listEntryPaths = map entryPath . listEntries

listEntryContents :: [Entry] -> [BL.ByteString]
listEntryContents = map extractContent
  where
    extractContent entry = case entryContent entry of
                             NormalFile content _ -> content
