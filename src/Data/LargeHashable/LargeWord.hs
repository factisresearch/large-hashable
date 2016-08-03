{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.LargeHashable.LargeWord
    ( Word128(..), Word256(..)
    , fromWord8, toWord8
    , bsToW64, w64ToBs
    , bsToW128, w128ToBs
    , bsToW256, w256ToBs
    , xorW128, xorW256
    )
where

-- keep imports in alphabetic order (in Emacs, use "M-x sort-lines")
import Data.Bits
import Data.Data (Data)
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import qualified Data.ByteString as BS

data Word128
    = Word128
    { w128_first :: !Word64
    , w128_second :: !Word64
    }
    deriving (Show, Read, Eq, Ord, Typeable, Generic, Data)

data Word256
    = Word256
    { w256_first :: !Word128
    , w256_second :: !Word128
    }
    deriving (Show, Read, Eq, Ord, Typeable, Generic, Data)

fromWord8 :: Integral a => Word8 -> a
fromWord8 = fromIntegral

toWord8 :: (Show a, Integral a) => a -> Word8
toWord8 i
    | i > 255 || i < 0 = error ("toWord8: not in range " ++ show i)
    | otherwise = fromIntegral i

bsToW256 :: BS.ByteString -> Word256
bsToW256 bs = Word256 first128 next128
    where
      first128 = bsToW128 bs
      next128 = bsToW128 (BS.drop 16 bs)

w256ToBs :: Word256 -> BS.ByteString
w256ToBs (Word256 first128 next128) =
    w128ToBs first128 `BS.append` w128ToBs next128

bsToW128 :: BS.ByteString -> Word128
bsToW128 bs = Word128 first64 next64
    where
      first64 = bsToW64 bs
      next64 = bsToW64 (BS.drop 8 bs)

w128ToBs :: Word128 -> BS.ByteString
w128ToBs (Word128 first64 next64) =
    w64ToBs first64 `BS.append` w64ToBs next64

w64ToBs :: Word64 -> BS.ByteString
w64ToBs w64 =
    BS.pack
    [ fromIntegral (w64 `shiftR` 56 .&. 255)
    , fromIntegral (w64 `shiftR` 48 .&. 255)
    , fromIntegral (w64 `shiftR` 40 .&. 255)
    , fromIntegral (w64 `shiftR` 32 .&. 255)
    , fromIntegral (w64 `shiftR` 24 .&. 255)
    , fromIntegral (w64 `shiftR` 16 .&. 255)
    , fromIntegral (w64 `shiftR` 8 .&. 255)
    , fromIntegral (w64 .&. 255)
    ]

bsToW64 :: BS.ByteString -> Word64
bsToW64 = BS.foldl' (\x w8 -> (x `shiftL` 8) + fromIntegral w8) 0 . BS.take 8

xorW128 :: Word128 -> Word128 -> Word128
xorW128 (Word128 w11 w12) (Word128 w21 w22) = Word128 (w11 `xor` w21) (w12 `xor` w22)

xorW256 :: Word256 -> Word256 -> Word256
xorW256 (Word256 w11 w12) (Word256 w21 w22) = Word256 (w11 `xorW128` w21) (w12 `xorW128` w22)
