{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE FlexibleInstances    #-}

module SegmentTypes ( SegmentId(..)
                    , SegmentType(..)
                    , Segment(..)
                    , SegmentEntry(..)
                    , SegmentRecordRef(..)
                    , Reference(..)
                    ) where

import           Display
import           RecordTypes

import qualified Codec.Archive.Tar          as Tar
import qualified Data.ByteString.Lazy       as BL
import           Data.List                  ( intercalate )
import           Data.Word
import           Numeric                    ( showHex )
import           Text.Printf                ( printf )

data SegmentType = SegmentTypeBulk
                 | SegmentTypeData
  deriving (Show)

instance Display SegmentType where
  display SegmentTypeBulk = "bulk"
  display SegmentTypeData = "data"

data Segment = Segment { segSegmentId      :: SegmentId
                       , segVersion        :: Word8
                       , segGeneration     :: Word32
                       , segFullGeneration :: Word32
                       , segCompacted      :: Bool
                       , segReferences     :: [Reference]
                       , segRecords        :: [SegmentRecordRef]
                       , segBytes          :: BL.ByteString
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

data SegmentRecordRef = SegmentRecordRef { recNumber :: Word32
                                         , recType   :: RecordType
                                         , recOffset :: Word32
                                         }
  deriving (Show)

instance Display SegmentRecordRef where
  display SegmentRecordRef {..} = intercalate " "
                                  [ show recNumber
                                  , show recType
                                  , showHex recOffset ""
                                  ]

type    SegmentId    = String

newtype SegmentEntry = SegmentEntry { seEntry :: Tar.Entry }

instance Display SegmentId where
  display id = (display $ segmentType id) ++ " " ++ id

segmentType :: SegmentId -> SegmentType
segmentType id = if isBulkSegmentId id
                 then SegmentTypeBulk
                 else SegmentTypeData

isBulkSegmentId :: SegmentId -> Bool
isBulkSegmentId segId = segId !! 16 == 'b'
