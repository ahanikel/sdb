{-# LANGUAGE RecordWildCards      #-}

module Record (parseRecord) where

import           SegmentTypes
import           RecordTypes
import           Display
import           ParseUtil

import           Data.Attoparsec.ByteString.Lazy
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           Prelude hiding (take)

parseRecord :: Segment -> RecordType -> Parser Record
parseRecord seg RecTypeBlock = BlockRecord <$> takeByteString
parseRecord seg RecTypeValue = do
  first <- anyWord8

  case first of
    _ | first .&. 0x80 == 0 -> do
          let rvlValueType = SmallValue
              rvlLength    = fromIntegral first
          rvlData         <- take $ fromIntegral rvlLength
          return ValueRecord {..}

    _ | first .&. 0xC0 == 0x80 -> do
          second          <- anyWord8
          let rvlValueType = MediumValue
              rvlLength    = fromIntegral $ first .&. 0x3F * 2^8 + second
          rvlData         <- take $ fromIntegral rvlLength
          return ValueRecord {..}

    _ | first .&. 0xE0 == 0xC0 -> do
          let rvlValueType = LongValue
              first'       = first .&. 0x1F
          lenBytes        <- count 7 anyWord8
          let rvlLength    = foldl (\s n -> shift s 8 + fromIntegral n)
                                   0
                                   (first' : lenBytes)
          rvlData         <- takeByteString -- we could convert this directly to a ref
          return ValueRecord {..}

    _ | first .&. 0xF0 == 0xE0 -> do
          second          <- anyWord8
          let rvlValueType = ExternalValue
              rvlLength    = fromIntegral $ first .&. 0x0F * 2^8 + second
          rvlData         <- take $ fromIntegral rvlLength -- we could convert this directly to a string
          return ValueRecord {..}

parseRecord seg RecTypeList = do
  rlsLength <- parseBigEndianUInt32
  rlsBucket <- parseRecordRef seg
  return ListRecord {..}

parseRecord seg RecTypeListBucket = do
  rbrLength <- parseBigEndianUInt16
  rbrRecs <- count (fromIntegral rbrLength) (parseRecordRef seg)
  return BucketRecord {..}

parseRecord seg RecTypeMapLeaf = do
  levelAndSize <- parseBigEndianUInt32
  let rmlLevel  = fromIntegral $ levelAndSize `shift` (- mapSizeBits)
      rmlSize   = levelAndSize .&. (2 ^ mapSizeBits - 1)
      size      = fromIntegral rmlSize
  rmlHashes    <- count size parseBigEndianUInt32
  rmlPairs     <- count size $ do
                    key   <- parseRecordRef seg
                    value <- parseRecordRef seg
                    return (key, value)
  return MapLeafRecord {..}

parseRecord seg RecTypeMapBranch = do
  levelAndSize <- parseBigEndianUInt32
  let rmbLevel  = fromIntegral $ levelAndSize `shift` (- mapSizeBits)
      rmbSize   = levelAndSize .&. (2 ^ mapSizeBits - 1)
      size      = fromIntegral rmbSize
  rmbBitmap    <- parseBigEndianUInt32
  rmbRecs      <- count size (parseRecordRef seg)
  return MapBranchRecord {..}

parseRecord seg RecTypeTemplate = do
  head              <- parseBigEndianUInt32
  let hasPrimaryType = head .&. 2^31 /= 0
      hasMixins      = head .&. 2^30 /= 0
      numMixins      = if hasMixins
                       then fromIntegral $ head .&. (2^30 - 1) `shift` (-19)
                       else 0
      rtpHasChildren = head .&. 2^19 /= 0
      hasMoreThanOne = head .&. 2^18 /= 0
      numProperties  = fromIntegral $ head .&. (2^18 - 1)
  rtpPrimaryId      <- if hasPrimaryType
                       then Just <$> parseRecordRef seg
                       else return Nothing
  rtpMixinIds       <- count numMixins (parseRecordRef seg)
  rtpChildName      <- if rtpHasChildren && not hasMoreThanOne
                       then Just <$> parseRecordRef seg
                       else return Nothing
  rtpPropNamesId    <- if numProperties > 0
                       then Just <$> parseRecordRef seg
                       else return Nothing
  rtpPropTypes      <- map fromIntegral <$> count numProperties anyWord8
  return TemplateRecord {..}

parseRecord seg RecTypeNode = do
  rnoStableId <- parseRecordRef seg
  rnoTemplate <- parseRecordRef seg
  rnoPropValues <- parseRecordRef seg
  rnoChildNodes <- parseRecordRef seg
  return NodeRecord {..}

parseRecordRef :: Segment -> Parser RecordRef
parseRecordRef seg = do
  idxSegment <- parseBigEndianUInt16
  let refSegment = if idxSegment == 0
                   then segSegmentId seg
                   else display $ head $ drop (fromIntegral idxSegment - 1) $ segReferences seg
  refRecord  <- parseBigEndianUInt32
  return RecordRef {..}

mapLevelBits = 4
mapSizeBits  = 32 - mapLevelBits
