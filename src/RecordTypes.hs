module RecordTypes ( RecordType(..)
                   , Record(..)
                   , ValueType(..)
                   , recordTypeFromWord8
                   , RecordRef(..)
                   ) where

import           Display
import           ParseUtil

import qualified Data.ByteString as BS
import           Data.Word

data RecordType = RecTypeMapLeaf
                | RecTypeMapBranch
                | RecTypeListBucket
                | RecTypeList
                | RecTypeValue
                | RecTypeBlock
                | RecTypeTemplate
                | RecTypeNode
                | RecTypeBlobID
  deriving (Show)

instance Display RecordType where
  display = show

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

data ValueType = SmallValue
               | MediumValue
               | LongValue
               | ExternalValue
  deriving (Show, Read)

data RecordRef = RecordRef { refSegment :: String
                           , refRecord  :: Word32
                           }
  deriving (Show, Read)

data Record = BlockRecord         { rblData      :: BS.ByteString
                                  }
            | ValueRecord         { rvlValueType :: ValueType
                                  , rvlLength    :: Word64
                                  , rvlData      :: BS.ByteString
                                  }
            | ListRecord          { rlsLength    :: Word32
                                  , rlsBucket    :: RecordRef
                                  }
            | BucketRecord        { rbrLength    :: Word16
                                  , rbrRecs      :: [RecordRef]
                                  }
            | MapLeafRecord       { rmlLevel     :: Word8
                                  , rmlSize      :: Word32
                                  , rmlHashes    :: [Word32]
                                  , rmlPairs     :: [(RecordRef, RecordRef)]
                                  }
            | MapBranchRecord     { rmbLevel     :: Word8
                                  , rmbSize      :: Word32
                                  , rmbBitmap    :: Word32
                                  , rmbRecs      :: [RecordRef]
                                  }
            | TemplateRecord      { rtpPrimaryId    :: Maybe RecordRef
                                  , rtpMixinIds     :: [RecordRef]
                                  , rtpHasChildren  :: Bool
                                  , rtpChildName    :: Maybe RecordRef
                                  , rtpPropNamesId  :: Maybe RecordRef
                                  , rtpPropTypes    :: [Int]
                                  }
            | NodeRecord          { rnoStableId     :: RecordRef -- to its ancestor or to itself
                                  , rnoTemplate     :: RecordRef -- to a template record
                                  , rnoPropValues   :: RecordRef -- to a list record
                                  , rnoChildNodes   :: RecordRef -- to a map record
                                  }
  deriving (Show, Read)

instance Display Record where
  display = show
