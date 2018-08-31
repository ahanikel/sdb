{-# LANGUAGE RecordWildCards #-}

module Segment where

import Prelude hiding (take)
import Codec.Archive.Tar
import Control.Monad
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BS8
import Data.Word
import TarArchive
import Text.Regex

data Segment = Segment { segVersion :: Word8
                       , segGeneration :: Word32
                       , segFullGeneration :: Word32
                       , segCompacted :: Bool
                       , segReferences :: [Reference]
                       , segRecords :: [Record]
                       }

data Reference = Reference { refMsb :: Word64
                           , refLsb :: Word64
                           }

data Record = Record { recNumber :: Word32
                     , recType :: RecordType
                     , recOffset :: Word32
                     }

data RecordType = RecTypeMapLeaf
                | RecTypeMapBranch
                | RecTypeListBucket
                | RecTypeList
                | RecTypeValue
                | RecTypeBlock
                | RecTypeTemplate
                | RecTypeNode
                | RecTypeBlobID

recordTypeFromWord8 :: Word8 -> RecordType
recordTypeFromWord8 = (recTypes !!) . fromIntegral
  where
    recTypes = [ RecTypeMapLeaf
               , RecTypeMapBranch
               , RecTypeListBucket
               , RecTypeList
               , RecTypeValue
               , RecTypeBlock
               , RecTypeTemplate
               , RecTypeNode
               , RecTypeBlobID
               ]

segmentIds :: [Entry] -> [String]
segmentIds = map segmentIdFromEntry . segmentEntries

segmentEntries :: [Entry] -> [String]
segmentEntries = filter isAnySegment . map entryPath

isAnySegment = toBoolean . matchRegex segmentRegex
  where
    toBoolean    = maybe False (const True)
    segmentRegex = mkRegex "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\.[0-9a-f]{8}$"

segmentIdFromEntry = normalizeSegmentId . entryNameToSegmentId

entryNameToSegmentId = Prelude.takeWhile (/= '.')

normalizeSegmentId = filter (not . (`elem` ['-', ' ']))

isSegment s = normalizeSegmentId s == segmentIdFromEntry s

parseSegment :: Word8 -> Parser Segment
parseSegment 12 = do
  magic <- string $ BS8.fromString "0aK"
  segVersion <- anyWord8
  _ <- take 6
  segGeneration <- parseBigEndianUInt32
  let segFullGeneration = segGeneration
  let segCompacted = True
  nReferences <- parseBigEndianUInt32
  nRecords <- parseBigEndianUInt32
  segReferences <- count (fromIntegral nReferences) parseReference
  segRecords <- count (fromIntegral nRecords) parseRecord
  return Segment {..}

segmentVersion :: BL.ByteString -> Word8
segmentVersion = BL.head . BL.drop 3

parseBigEndianUInt32 :: Parser Word32
parseBigEndianUInt32 = do
  bytes <- count 4 anyWord8
  return $ sum [x * 2^y | (x,y) <- zip (map fromIntegral bytes) [24,16,8,0]]

parseBigEndianUInt64 :: Parser Word64
parseBigEndianUInt64 = do
  uints <- count 2 parseBigEndianUInt32
  return $ sum [x * 2^y | (x,y) <- zip (map fromIntegral uints) [32, 0]]

parseReference :: Parser Reference
parseReference = do
  refMsb <- parseBigEndianUInt64
  refLsb <- parseBigEndianUInt64
  return Reference {..}

parseRecord :: Parser Record
parseRecord = do
  recNumber <- parseBigEndianUInt32
  recType   <- recordTypeFromWord8 <$> anyWord8
  recOffset <- parseBigEndianUInt32
  return Record {..}
