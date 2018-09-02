module Index where

import Display
import ParseUtil

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as U8
import Data.Char
import Data.Hash.CRC32.GZip (calc_crc32)
import Data.Word
import Text.Printf

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
  display idx = undefined

parseIndex :: BS.ByteString -> Either String [IndexEntry]
parseIndex bs =
  case magic of
    0x0a304b0a -> parseIndexV1 bytes
    0x0a314b0a -> parseIndexV2 bytes
  where
    magic = readBigEndianUInt32 bytes
    bytes = BS.unpack bs

parseIndexV1 :: [Word8] -> Either String [IndexEntry]
parseIndexV1 bytes = do
  let n = length bytes
      indexMagic = 0x0a304b0a
      footerSize = 16
      indexEntrySize = 28
      footerChecksumOffset = 0
      footerCountOffset = 4
      footerSizeOffset = 8
      footerMagicOffset = 12
      entryMsbOffset = 0
      entryLsbOffset = 8
      entryPositionOffset = 16
      entrySizeOffset = 20
      entryGenerationOffset = 24
      footer = drop (n - footerSize) bytes
      checksum = readBigEndianUInt32 $ drop footerChecksumOffset footer
      count = fromIntegral $ readBigEndianUInt32 $ drop footerCountOffset footer
      size = fromIntegral $ readBigEndianUInt32 $ drop footerSizeOffset footer
      magic = readBigEndianUInt32 $ drop footerMagicOffset footer
  ensure (magic == indexMagic)
         (printf "invalid magic %08x" magic)
  ensure (size < count * indexEntrySize + footerSize)
         "invalid count or size"
  ensure (n < count * indexEntrySize + footerSize)
         "invalid count or data size"
  let entries = take (count * indexEntrySize)
                $ drop (n - footerSize - count * indexEntrySize)
                bytes
  ensure (calc_crc32 (map (chr . fromIntegral) entries) == checksum)
         "invalid checksum"
  where
    ensure True  _            = Right []
    ensure False errorMessage = Left errorMessage
  
parseIndexV2 = undefined
