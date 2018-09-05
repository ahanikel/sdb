{-# LANGUAGE RecordWildCards #-}

module Segment ( segments
               , segment
               , segmentType
               , SegmentType(..)
               , segmentIdFromMsbLsb
               , Reference(..)
               , parseReference
               ) where

import           Display
import           ParseUtil
import           Record
import           TarArchive

import           Prelude                    hiding ( take )
import           Codec.Archive.Tar
import           Control.Monad
import           Data.Attoparsec.ByteString
import           Data.Bits
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.UTF8       as BS8
import           Data.List                  ( intercalate )
import           Data.Word
import           Numeric                    ( showHex )
import           Text.Printf                ( printf )
import           Text.Regex                 ( mkRegex
                                            , matchRegex
                                            )

segments :: FilePath -> IO ()
segments path = do
  ids <- segmentIds <$> listEntries <$> BL.readFile path
  mapM_ (putStrLn . displayId) ids
  where
    displayId id = (display $ segmentType id) ++ " " ++ id

segment :: FilePath -> SegmentId -> IO ()
segment path segmentId = do
  segments   <- segmentIdEntryPairs <$> segmentEntries <$> listEntries <$> BL.readFile path
  let segment = findSegment segmentId segments
  either putStrLn (putStrLn . display) segment
  
segmentType :: SegmentId -> SegmentType
segmentType id = if isBulkSegmentId id
                 then SegmentTypeBulk
                 else SegmentTypeData

data SegmentType = SegmentTypeBulk
                 | SegmentTypeData
  deriving (Show)

instance Display SegmentType where
  display SegmentTypeBulk = "bulk"
  display SegmentTypeData = "data"

data Segment = Segment { segVersion        :: Word8
                       , segGeneration     :: Word32
                       , segFullGeneration :: Word32
                       , segCompacted      :: Bool
                       , segReferences     :: [Reference]
                       , segRecords        :: [RecordRef]
                       }
  deriving (Show)

instance Display Segment where
  display Segment {..} =
    intercalate "\n" $ [ "version "        ++ show segVersion
                       , "generation "     ++ show segGeneration
                       , "fullGeneration " ++ show segFullGeneration
                       , "compacted "      ++ show segCompacted
                       ]
                       ++ map (("reference " ++) . display) segReferences
                       ++ map (("record " ++)    . display) segRecords

data Reference = Reference { refMsb :: Word64
                           , refLsb :: Word64
                           }
  deriving (Show)

instance Display Reference where
  display Reference {..} = printf "%016x%016x" refMsb refLsb

data RecordRef = RecordRef { recNumber :: Word32
                           , recType   :: RecordType
                           , recOffset :: Word32
                           }
  deriving (Show)

instance Display RecordRef where
  display RecordRef {..} = intercalate " "
                        [ show recNumber
                        , show recType
                        , showHex recOffset ""
                        ]

type    SegmentId    = String

newtype SegmentEntry = SegmentEntry { seEntry :: Entry }

segmentIds :: [Entry] -> [SegmentId]
segmentIds = map segmentIdFromEntryName . segmentEntryNames . segmentEntries

segmentEntryNames :: [SegmentEntry] -> [String]
segmentEntryNames = map (entryPath . seEntry)

segmentEntries :: [Entry] -> [SegmentEntry]
segmentEntries = map SegmentEntry . filter (isAnySegment . entryPath)

segmentIdEntryPairs :: [SegmentEntry] -> [(SegmentId, SegmentEntry)]
segmentIdEntryPairs entries = do
  entry <- entries
  return (segmentIdFromEntryName $ entryPath $ seEntry entry, entry)

findSegment :: SegmentId -> [(SegmentId, SegmentEntry)] -> Either String Segment
findSegment segmentId segments = do
  SegmentEntry entry    <- maybe (Left "Segment not found")
                                 Right
                                 (lookup segmentId segments)
  let content            = entryContent entry
      NormalFile bytes _ = content
      segVer             = segmentVersion bytes
  parseOnly (parseSegment segVer) $ BL.toStrict bytes

isAnySegment = toBoolean . matchRegex segmentRegex
  where
    toBoolean    = maybe False (const True)
    segmentRegex = mkRegex "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\.[0-9a-f]{8}$"

segmentIdFromEntryName = normalizeSegmentId . entryNameToSegmentId

segmentIdFromMsbLsb msb lsb = printf "%016x%016x" msb lsb

entryNameToSegmentId = Prelude.takeWhile (/= '.')

normalizeSegmentId = filter (not . (`elem` ['-', ' ']))

isSegment s = normalizeSegmentId s == segmentIdFromEntryName s

isBulkSegmentId :: SegmentId -> Bool
isBulkSegmentId segId = segId !! 16 == 'b'

segmentVersion :: BL.ByteString -> Word8
segmentVersion = BL.head . BL.drop 3

parseSegment :: Word8 -> Parser Segment
parseSegment 12 = do
  magic                 <- string $ BS8.fromString "0aK"
  segVersion            <- anyWord8
  _                     <- take 6
  segGeneration         <- parseBigEndianUInt32
  let segFullGeneration  = segGeneration
      segCompacted       = True
  nReferences           <- parseBigEndianUInt32
  nRecords              <- parseBigEndianUInt32
  _                     <- take 10   -- header size is 32 and we're at 22 so far
  segReferences         <- count (fromIntegral nReferences) parseReference
  segRecords            <- count (fromIntegral nRecords) parseRecordRef
  return Segment {..}

parseSegment 13 = do
  magic                 <- string $ BS8.fromString "0aK"
  segVersion            <- anyWord8
  segFullGeneration'    <- parseBigEndianUInt32
  let segFullGeneration  = segFullGeneration' .&. 0x7fffffff
      segCompacted       = segFullGeneration' .&. 0x80000000 /= 0
  _                     <- take 2
  segGeneration         <- parseBigEndianUInt32
  nReferences           <- parseBigEndianUInt32
  nRecords              <- parseBigEndianUInt32
  _                     <- take 10   -- header size is 32 and we're at 22 so far
  segReferences         <- count (fromIntegral nReferences) parseReference
  segRecords            <- count (fromIntegral nRecords) parseRecordRef
  return Segment {..}

parseReference :: Parser Reference
parseReference = do
  refMsb <- parseBigEndianUInt64
  refLsb <- parseBigEndianUInt64
  return Reference {..}

parseRecordRef :: Parser RecordRef
parseRecordRef = do
  recNumber <- parseBigEndianUInt32
  recType   <- recordTypeFromWord8 <$> anyWord8
  recOffset <- parseBigEndianUInt32
  return RecordRef {..}
