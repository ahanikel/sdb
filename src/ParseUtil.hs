module ParseUtil where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import Data.Bits
import Data.Word

parseBigEndianUInt32 :: Parser Word32
parseBigEndianUInt32 = do
  bytes <- count 4 anyWord8
  return $ sum [x * 2^y | (x,y) <- zip (map fromIntegral bytes) [24, 16, 8, 0]]

parseBigEndianUInt64 :: Parser Word64
parseBigEndianUInt64 = do
  uints <- count 2 parseBigEndianUInt32
  return $ sum [x * 2^y | (x,y) <- zip (map fromIntegral uints) [32, 0]]

parseBigEndianUInt16 :: Parser Word16
parseBigEndianUInt16 = do
  msb <- fromIntegral <$> anyWord8
  lsb <- fromIntegral <$> anyWord8
  return $ msb `shift` 8 .|. lsb

readBigEndianUInt32 :: [Word8] -> (Word32, [Word8])
readBigEndianUInt32 bs =
  ( sum [x * 2^y | (x,y) <- zip (map fromIntegral $ Prelude.take 4 bs) [24, 16, 8, 0]]
  , Prelude.drop 4 bs
  )

readBigEndianUInt64 :: [Word8] -> (Word64, [Word8])
readBigEndianUInt64 bs =
  ( sum [x * 2^y | (x,y) <- zip (map fromIntegral $ Prelude.take 8 bs) [56, 48, 40, 32, 24, 16, 8, 0]]
  , Prelude.drop 8 bs
  )

ensure :: Bool -> String -> Either String [a]
ensure True  _            = Right []
ensure False errorMessage = Left errorMessage
