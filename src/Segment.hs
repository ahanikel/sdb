{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Segment ( segments
               , segment
               , record
               , segmentType
               , segmentIdFromMsbLsb
               , parseReference
               ) where

import           SegmentTypes
import           RecordTypes
import           Display
import           ParseUtil
import           Record
import           Store
import           TarArchive

import           Prelude                    hiding ( take )
import qualified Codec.Archive.Tar          as Tar
import           Control.Monad              ( msum
                                            , (<=<)
                                            , MonadPlus(..)
                                            , mplus
                                            )
import           Data.Attoparsec.ByteString.Lazy
import           Data.Bits
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.UTF8       as BS8
import           Data.Int                   ( Int64 )
import           Data.List                  ( intercalate )
import           Data.Word
import           Numeric                    ( showHex )
import           Text.Printf                ( printf )
import           Text.Regex                 ( mkRegex
                                            , matchRegex
                                            )

segments :: FilePath -> IO [SegmentId]
segments path = segmentIds <$> listEntries <$> BL.readFile path

segment :: SegmentId -> IO (Either String Segment)
segment segmentId = do
  paths <- map tName <$> reverse <$> readTarFiles "." True
  untilFound (flip segment' segmentId) paths (Left "no paths")
  where
    untilFound :: (c -> IO (Either a b)) -> [c] -> Either a b -> IO (Either a b)
    untilFound f []     res       = return res
    untilFound f _      (Right b) = return $ Right b
    untilFound f (c:cs) (Left a)  = f c >>= untilFound f cs
 
segment' :: FilePath -> SegmentId -> IO (Either String Segment)
segment' path segmentId = do
  segments   <- segmentIdEntryPairs <$> segmentEntries <$> listEntries <$> BL.readFile path
  return $ findSegment segmentId segments

segmentType :: SegmentId -> SegmentType
segmentType id = if isBulkSegmentId id
                 then SegmentTypeBulk
                 else SegmentTypeData

segmentIds :: [Tar.Entry] -> [SegmentId]
segmentIds = map segmentIdFromEntryName . segmentEntryNames . segmentEntries

segmentEntryNames :: [SegmentEntry] -> [String]
segmentEntryNames = map (Tar.entryPath . seEntry)

segmentEntries :: [Tar.Entry] -> [SegmentEntry]
segmentEntries = map SegmentEntry . filter (isAnySegment . Tar.entryPath)

segmentIdEntryPairs :: [SegmentEntry] -> [(SegmentId, SegmentEntry)]
segmentIdEntryPairs entries = do
  entry <- entries
  return (segmentIdFromEntryName $ Tar.entryPath $ seEntry entry, entry)

findSegment :: SegmentId -> [(SegmentId, SegmentEntry)] -> Either String Segment
findSegment segmentId segments = do
  SegmentEntry entry    <- maybe (Left "Segment not found")
                                 Right
                                 (lookup segmentId segments)
  let content                = Tar.entryContent entry
      Tar.NormalFile bytes _ = content
      segVer                 = segmentVersion bytes
  toEither $ parse (parseSegment segmentId segVer bytes) bytes

toEither :: Result r  -> Either String r
toEither (Fail _ _ e) = Left e
toEither (Done _   r) = Right r

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

parseSegment :: SegmentId -> Word8 -> BL.ByteString -> Parser Segment
parseSegment segSegmentId 12 segBytes = do
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

parseSegment segSegmentId 13 segBytes = do
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

parseRecordRef :: Parser SegmentRecordRef
parseRecordRef = do
  recNumber <- parseBigEndianUInt32
  recType   <- recordTypeFromWord8 <$> anyWord8
  recOffset <- parseBigEndianUInt32
  return SegmentRecordRef {..}

recordFromSegment :: Segment -> Int -> Either String Record
recordFromSegment seg@Segment {..} recNo = toEither $ parse (parseRecord seg $ recType rec) bytes
  where
    rec           = segRecords !! recNo
    ofsNormalised = normaliseOffset (BL.length segBytes) (recOffset rec)
    bytes         = BL.drop ofsNormalised segBytes

normaliseOffset :: Int64 -> Word32 -> Int64
normaliseOffset segmentLength recOffset =
  segmentLength - 256 * 1024 + (fromIntegral recOffset)

record :: SegmentId -> String -> IO (Either String Record)
record segmentId recordIndex = do
  eSegment <- segment segmentId
  return $ do
    segment <- eSegment
    recordFromSegment segment $ read recordIndex
