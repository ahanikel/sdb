{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Index (index) where

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

index :: FilePath -> IO ()
index path = do
  entries         <- listEntries <$> BL.readFile path
  let indexEntries = filterEntriesByName (".idx" `L.isSuffixOf`) entries
      indexContent = listEntryContents indexEntries
  mapM_ (putStrLn . either id display . parseIndex . BL.toStrict) indexContent

data IndexEntry = IndexEntry { ixMsb            :: Word64
                             , ixLsb            :: Word64
                             , ixPosition       :: Int
                             , ixSize           :: Int
                             , ixGeneration     :: Int
                             , ixFullGeneration :: Int
                             , ixCompacted      :: Bool
                             }
  deriving (Show)

instance Display IndexEntry where
  display IndexEntry {..} =
    printf "%s %s %x %d %d %d %s" (display $ segmentType ixSegmentId)
                                  ixSegmentId
                                  ixPosition
                                  ixSize
                                  ixGeneration
                                  ixFullGeneration
                                  (show ixCompacted)
    where
      ixSegmentId = segmentIdFromMsbLsb ixMsb ixLsb

instance Display [IndexEntry] where
  display = L.intercalate "\n" . map display

data IndexFooter = IndexFooter { ixfChecksum :: Word32
                               , ixfCount    :: Int
                               , ixfSize     :: Int
                               , ixfMagic    :: Word32
                               }
  deriving (Show)

parseIndex :: BS.ByteString -> Either String [IndexEntry]
parseIndex bs = do

  ensure (len >= 4)
         "invalid data"

  case magic of
    0x0a304b0a -> parseIndexV1 bs
    0x0a314b0a -> parseIndexV2 bs

  where
    len   = BS.length bs
    magic = fst $ readBigEndianUInt32 $ BS.unpack $ BS.drop (len - 4) bs

parseIndexV1 :: BS.ByteString -> Either String [IndexEntry]
parseIndexV1 bs = do
  let indexMagic            = 0x0a304b0a
      footerSize            = 16
      indexEntrySize        = 28

      footerChecksumOffset  =  0
      footerCountOffset     =  4
      footerSizeOffset      =  8
      footerMagicOffset     = 12

      entryMsbOffset        =  0
      entryLsbOffset        =  8
      entryPositionOffset   = 16
      entrySizeOffset       = 20
      entryGenerationOffset = 24

      n                     = BS.length bs

  ensure (n >= footerSize)
         "invalid data"

  let footer                = BS.drop (n - footerSize) bs
  IndexFooter {..}         <- parseOnly parseIndexFooter footer

  ensure (ixfMagic == indexMagic)
         (printf "invalid magic %08x" ixfMagic)

  ensure (ixfSize >= ixfCount * indexEntrySize + footerSize)
         "invalid count or size"

  ensure (n >= ixfCount * indexEntrySize + footerSize)
         "invalid count or data size"

  let entries = BS.take (ixfCount * indexEntrySize)
                $ BS.drop (n - footerSize - ixfCount * indexEntrySize)
                bs

  ensure (calc_crc32 (map (chr . fromIntegral) $ BS.unpack entries) == ixfChecksum)
         "invalid checksum"

  parseOnly (count ixfCount parseIndexEntryV1) entries

parseIndexV2 :: BS.ByteString -> Either String [IndexEntry]
parseIndexV2 bs = do
  let indexMagic                = 0x0a314b0a
      footerSize                = 16
      indexEntrySize            = 33

      footerChecksumOffset      =  0
      footerCountOffset         =  4
      footerSizeOffset          =  8
      footerMagicOffset         = 12

      entryMsbOffset            =  0
      entryLsbOffset            =  8
      entryPositionOffset       = 16
      entrySizeOffset           = 20
      entryGenerationOffset     = 24
      entryFullGenerationOffset = 28
      entryCompacted            = 32

      n                         = BS.length bs

  ensure (n >= footerSize)
         "invalid data"

  let footer                = BS.drop (n - footerSize) bs
  IndexFooter {..}         <- parseOnly parseIndexFooter footer

  ensure (ixfMagic == indexMagic)
         (printf "invalid magic %08x" ixfMagic)

  ensure (ixfSize >= ixfCount * indexEntrySize + footerSize)
         "invalid count or size"

  ensure (n >= ixfCount * indexEntrySize + footerSize)
         "invalid count or data size"

  let entries = BS.take (ixfCount * indexEntrySize)
                $ BS.drop (n - footerSize - ixfCount * indexEntrySize)
                bs

  ensure (calc_crc32 (map (chr . fromIntegral) $ BS.unpack entries) == ixfChecksum)
         "invalid checksum"

  parseOnly (count ixfCount parseIndexEntryV2) entries

parseIndexEntryV1 :: Parser IndexEntry
parseIndexEntryV1 = do
  ixMsb               <- parseBigEndianUInt64
  ixLsb               <- parseBigEndianUInt64
  ixPosition          <- fromIntegral <$> parseBigEndianUInt32
  ixSize              <- fromIntegral <$> parseBigEndianUInt32
  ixGeneration        <- fromIntegral <$> parseBigEndianUInt32
  let ixFullGeneration = ixGeneration
      ixCompacted      = True
  return IndexEntry {..}

parseIndexEntryV2 :: Parser IndexEntry
parseIndexEntryV2 = do
  ixMsb               <- parseBigEndianUInt64
  ixLsb               <- parseBigEndianUInt64
  ixPosition          <- fromIntegral <$> parseBigEndianUInt32
  ixSize              <- fromIntegral <$> parseBigEndianUInt32
  ixGeneration        <- fromIntegral <$> parseBigEndianUInt32
  ixFullGeneration    <- fromIntegral <$> parseBigEndianUInt32
  ixCompacted         <- (/= 0)       <$> anyWord8
  return IndexEntry {..}

parseIndexFooter :: Parser IndexFooter
parseIndexFooter = do
  ixfChecksum <- parseBigEndianUInt32
  ixfCount    <- fromIntegral <$> parseBigEndianUInt32
  ixfSize     <- fromIntegral <$> parseBigEndianUInt32
  ixfMagic    <- parseBigEndianUInt32
  return IndexFooter {..}
