module TarArchive (listEntries) where

import           Prelude hiding (read)
import           Codec.Archive.Tar
import           Control.Exception (throw)
import qualified Data.ByteString.Lazy as BL

listEntries :: BL.ByteString -> [Entry]
listEntries bs = foldEntries (:) [] throw $ read bs

listEntryPaths = map entryPath . listEntries

