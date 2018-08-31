module Store ( TarFile(..)
             , readTarFiles
             , tarFilter
             , youngestOnly
             ) where

import Control.Exception
import Data.List
import Data.Maybe
import Data.Ord
import Data.Word
import System.Directory
import Text.Regex

data TarFile = TarFile { tName       :: String
                       , tNumber     :: Word64
                       , tGeneration :: Char
                       }
  deriving (Show)

readTarFiles :: FilePath -> Bool -> IO [TarFile]
readTarFiles dir showAll = do
  tarFiles <- tarFilter <$> listDirectory dir
  return $ if showAll
           then tarFiles
           else youngestOnly tarFiles

tarFilter :: [String] -> [TarFile]
tarFilter = catMaybes . map matchTarFile
  where
    matchTarFile :: FilePath -> Maybe TarFile
    matchTarFile f = do
      [num,gen] <- matchRegex tarRegex f
      return $ TarFile f (read num) (head gen)

    tarRegex :: Regex
    tarRegex = mkRegex "^data([0-9]{5})([a-z])\\.tar$"
  
youngestOnly :: [TarFile] -> [TarFile]
youngestOnly = map last . groupBy (\a b -> tNumber a == tNumber b) . sortBy (comparing tName)
