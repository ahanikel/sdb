module TarArchive (entries, listEntries) where

import           Prelude hiding (read)
import           Codec.Archive.Tar
import           Control.Exception (throw)
import qualified Data.ByteString.Lazy as BL

entries :: FilePath -> IO ()
entries path = do
  entries <- listEntries <$> BL.readFile path
  mapM_ (putStrLn . entryPath) entries

listEntries :: BL.ByteString -> [Entry]
listEntries bs = foldEntries (:) [] throw $ read bs

listEntryPaths = map entryPath . listEntries

