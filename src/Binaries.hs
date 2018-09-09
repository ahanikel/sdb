{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Binaries (binaries) where

import Display
import ParseUtil
import SegmentTypes
import Segment
import TarArchive

import           Data.Attoparsec.ByteString
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as U8
import           Data.Char            (chr)
import           Data.Hash.CRC32.GZip (calc_crc32)
import qualified Data.List            as L
import           Data.Word
import           Text.Printf

binaries :: FilePath -> IO [String]
binaries path = do
  entries          <- listEntries <$> BL.readFile path
  let binaryEntries = filterEntriesByName (".brf" `L.isSuffixOf`) entries
      binaryContent = listEntryContents binaryEntries
  return $ concat $ map (either (: []) (map display . bGenerations) . parseBinaries . BL.toStrict) binaryContent

newtype Binaries = Binaries { bGenerations :: [Generation] }

data Generation = Generation { gGeneration     :: Int
                             , gFullGeneration :: Int
                             , gCompacted      :: Bool
                             , gSegments       :: [SegmentRef]
                             }

instance Display Generation where
  display Generation {..} = unlines
    [ printf "%d %d %s %s %s" gGeneration
                              gFullGeneration
                              (show gCompacted)
                              (display $ srReference sr)
                              (display r)
    | sr <- gSegments
    , r  <- srReferences sr
    ]

data SegmentRef = SegmentRef { srReference     :: Reference
                             , srReferences    :: [BinaryReference]
                             }
instance Display SegmentRef where
  display SegmentRef {..} = unlines $ map ((display srReference ++) . display) srReferences

newtype BinaryReference = BinaryReference { brReference :: String }

instance Display BinaryReference where
  display = brReference

data BinariesFooter = BinariesFooter { bfChecksum :: Word32
                                     , bfCount    :: Int
                                     , bfSize     :: Int
                                     , bfMagic    :: Word32
                                     }
  deriving (Show)

parseBinaries :: BS.ByteString -> Either String Binaries
parseBinaries bs = do
  let magicV1 = 0x0a30420a
      magicV2 = 0x0a31420a

  let len   = BS.length bs
      magic = fst $ readBigEndianUInt32 $ BS.unpack $ BS.drop (len - 4) bs

  ensure (len >= 4)
         "invalid data"

  let parseGeneration = case magic of
                          _ | magic == magicV1 -> parseGenerationV1
                          _ | magic == magicV2 -> parseGenerationV2
                          _ | otherwise        -> undefined

  let binariesFooterSize     = 16
      binariesGenerationSize =  8
      binariesSegmentSize    = 20
      binariesReferenceSize  =  4

      n = BS.length bs

  ensure (n >= binariesFooterSize)
         "Invalid data"

  let footer                 = BS.drop (n - binariesFooterSize) bs
  BinariesFooter {..}       <- parseOnly parseBinariesFooter footer

  ensure (bfMagic == magic)
         "Invalid magic"

  ensure (bfSize >= binariesFooterSize)
         "Invalid size"

  ensure (bfCount >= 0)
         "Invalid count"

  let entries = BS.drop (n - bfSize) $ BS.take (n - binariesFooterSize) bs

  ensure (calc_crc32 (map (chr . fromIntegral) $ BS.unpack entries) == bfChecksum)
         "Invalid checksum"

  bGenerations <- parseOnly (count bfCount parseGeneration) entries
  return Binaries {..}

parseGenerationV1 :: Parser Generation
parseGenerationV1 = do
  gGeneration        <- fromIntegral <$> parseBigEndianUInt32
  let gFullGeneration = gGeneration
      gCompacted      = True
  segmentsCount      <- fromIntegral <$> parseBigEndianUInt32
  gSegments          <- count segmentsCount parseSegmentRef
  return Generation {..}

parseGenerationV2 :: Parser Generation
parseGenerationV2 = do
  gGeneration        <- fromIntegral <$> parseBigEndianUInt32
  gFullGeneration    <- fromIntegral <$> parseBigEndianUInt32
  gCompacted         <- (/= 0)       <$> anyWord8
  segmentsCount      <- fromIntegral <$> parseBigEndianUInt32
  gSegments          <- count segmentsCount parseSegmentRef
  return Generation {..}

parseSegmentRef :: Parser SegmentRef
parseSegmentRef = do
  srReference  <- parseReference
  refCount     <- fromIntegral <$> parseBigEndianUInt32
  srReferences <- count refCount parseBinaryReference
  return SegmentRef {..}

parseBinaryReference :: Parser BinaryReference
parseBinaryReference = do
  len <- fromIntegral <$> parseBigEndianUInt32
  brReference <- U8.toString <$> Data.Attoparsec.ByteString.take len
  return BinaryReference {..}

parseBinariesFooter :: Parser BinariesFooter
parseBinariesFooter = do
  bfChecksum <- parseBigEndianUInt32
  bfCount    <- fromIntegral <$> parseBigEndianUInt32
  bfSize     <- fromIntegral <$> parseBigEndianUInt32
  bfMagic    <- parseBigEndianUInt32
  return BinariesFooter {..}
