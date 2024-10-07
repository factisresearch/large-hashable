{-# LANGUAGE CPP #-}
module Data.LargeHashable.Endianness (

    Endianness(..), systemEndianness

) where

-- taken from the cpu package (last release 2013)

-- | represent the CPU endianness
--
-- Big endian system stores bytes with the MSB as the first byte.
-- Little endian system stores bytes with the LSB as the first byte.
--
-- middle endian is purposely avoided.
data Endianness = LittleEndian
                | BigEndian
                deriving (Show,Eq)

-- | return the system endianness
systemEndianness :: Endianness
#ifdef WORDS_BIGENDIAN
systemEndianness = BigEndian
#else
systemEndianness = LittleEndian
#endif
