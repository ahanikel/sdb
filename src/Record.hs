module Record (RecordType(..), recordTypeFromWord8) where

import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           Display

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
               | LargeValue
               | ExternalValue

type Reference = String

data Record = BlockRecord         { recData      :: BL.ByteString
                                  }
            | ValueRecord         { recValueType :: ValueType
                                  , recLength    :: Word64
                                  , recData      :: BL.ByteString
                                  }
            | ListRecord          { recLength    :: Word64
                                  , recBucket    :: Reference
                                  }
            | BucketRecord        { recLength    :: Word64
                                  , recRecs      :: [Reference]
                                  }
            | MapLeafRecord       { recLength    :: Word64
                                  , recPairs     :: [(String, Reference)]
                                  }
            | MapBranchRecord     { recLength    :: Word64
                                  , recRecs      :: [Reference]
                                  }
            | TemplateZeroRecord  { recProps     :: [(Reference, Reference)]
                                  }
            | TemplateOneRecord   { recProps     :: [(Reference, Reference)]
                                  , recChildren  :: [Reference]
                                  }
            | TemplateManyRecord  { recProps     :: [(Reference, Reference)]
                                  , recChildren  :: [Reference]
                                  }
            | NodeRecord          { recTemplate  :: Reference
                                  , recValues    :: Reference -- to a list record
                                  , recTree      :: Reference -- to a map record
                                  }
