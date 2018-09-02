{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Graph (graph) where

import Display
import ParseUtil
import Segment
import TarArchive

import           Data.Attoparsec.ByteString
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Char            (chr)
import           Data.Hash.CRC32.GZip (calc_crc32)
import qualified Data.List            as L
import           Data.Word
import           Text.Printf

graph :: FilePath -> IO ()
graph path = do
  entries         <- listEntries <$> BL.readFile path
  let graphEntries = filterEntriesByName (".gph" `L.isSuffixOf`) entries
      graphContent = listEntryContents graphEntries
  mapM_ (putStrLn . either id display . parseGraph . BL.toStrict) graphContent

data GraphEntry = GraphEntry { gphReference :: Reference
                             , gphReferences :: [Reference]
                             }
  deriving (Show)

instance Display GraphEntry where
  display GraphEntry {..} = L.intercalate "\n" $ do
    ref <- gphReferences
    return $ (display gphReference) ++ " " ++ (display ref)

instance Display [GraphEntry] where
  display = L.intercalate "\n" . map display

data GraphFooter = GraphFooter { gfChecksum :: Word32
                               , gfCount    :: Int
                               , gfSize     :: Int
                               , gfMagic    :: Word32
                               }
  deriving (Show)

parseGraph :: BS.ByteString -> Either String [GraphEntry]
parseGraph bs = do
  let graphMagic           = 0x0a30470a
      footerSize           = 16
      keySize              = 20
      valueSize            = 16

      footerChecksumOffset =  0
      footerCountOffset    =  4
      footerSizeOffset     =  8
      footerMagicOffset    = 12

      entryMsbOffset       =  0
      entryLsbOffset       =  8
      entryCountOffset     = 16

      n                    = BS.length bs

  ensure (n >= footerSize)
         "Invalid data"

  let footer               = BS.drop (n - footerSize) bs
  GraphFooter {..}        <- parseOnly parseGraphFooter footer

  ensure (gfMagic == graphMagic)
         "Invalid magic"

  ensure (gfSize >= footerSize)
         "Invalid size"

  ensure (gfCount >= 0)
         "Invalid entry count"

  let entries = BS.drop (n - gfSize) $ BS.take (n - footerSize) bs

  ensure (calc_crc32 (map (chr . fromIntegral) $ BS.unpack entries) == gfChecksum)
         "invalid checksum"

  parseOnly (count gfCount parseGraphEntry) entries

parseGraphFooter :: Parser GraphFooter
parseGraphFooter = do
  gfChecksum <- parseBigEndianUInt32
  gfCount    <- fromIntegral <$> parseBigEndianUInt32
  gfSize     <- fromIntegral <$> parseBigEndianUInt32
  gfMagic    <- parseBigEndianUInt32
  return GraphFooter {..}

parseGraphEntry :: Parser GraphEntry
parseGraphEntry = do
  rMsb <- parseBigEndianUInt64
  rLsb <- parseBigEndianUInt64
  rCount <- fromIntegral <$> parseBigEndianUInt32
  let gphReference = Reference rMsb rLsb
  gphReferences <- count rCount parseReference
  return GraphEntry {..}
