module Record (RecordType(..), recordTypeFromWord8) where

import Data.Word
import Display

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
