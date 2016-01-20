module Data.LargeHashable.Serial (serialLargeHash) where

import Data.LargeHashable.Intern
import Data.LargeHashable.Class
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

serialLargeHash :: Serial a => HashAlgorithm -> a -> Hash
serialLargeHash algo a = runLH algo $ serialize a

instance MonadPut LH where
    flush = return ()

    putWord8 = updateHash
    putWord16host = updateHash
    putWord32host = updateHash
    putWord64host = updateHash
    putWordhost = updateHash

    putWord16le = updateHashle
    putWord32le = updateHashle
    putWord64le = updateHashle

    putWord16be = updateHashbe
    putWord32be = updateHashbe
    putWord64be = updateHashbe

    putByteString = B.foldl (\m w -> m >> updateHash w) (return ())
    putLazyByteString = BL.foldl (\m w -> m >> updateHash w) (return ())

updateHashbe :: (Bits a, Integral a, LargeHashable a) => a -> LH ()
updateHashbe w = mapM_ updateHash $ splitInto8 w

updateHashle :: (Bits a, Integral a, LargeHashable a) => a -> LH ()
updateHashle w = let words = splitInto8 w
                   in updateHash (last words) >> mapM_ updateHash (init words)

-- | Warning: Only works with unsigned Numbers
splitInto8 :: (Bits a, Integral a) => a -> [Word8]
splitInto8 i
    | isSigned i = error "splitInto8 is only meant to be called with Words!"
    | i == 0     = []
    | otherwise  = fromIntegral (i .&. 0xff) : splitInto8 (shift i (-8))
