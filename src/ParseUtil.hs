module ParseUtil where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import Data.Word

parseBigEndianUInt32 :: Parser Word32
parseBigEndianUInt32 = do
  bytes <- count 4 anyWord8
  return $ sum [x * 2^y | (x,y) <- zip (map fromIntegral bytes) [24, 16, 8, 0]]

parseBigEndianUInt64 :: Parser Word64
parseBigEndianUInt64 = do
  uints <- count 2 parseBigEndianUInt32
  return $ sum [x * 2^y | (x,y) <- zip (map fromIntegral uints) [32, 0]]

readBigEndianUInt32 :: [Word8] -> Word32
readBigEndianUInt32 bs =
  sum [x * 2^y | (x,y) <- zip (map fromIntegral $ Prelude.take 4 bs) [24, 16, 8, 0]]

readBigEndianUInt64 :: [Word8] -> Word64
readBigEndianUInt64 bs =
  sum [x * 2^y | (x,y) <- zip (map fromIntegral $ Prelude.take 8 bs) [56, 48, 40, 32, 24, 16, 8, 0]]
