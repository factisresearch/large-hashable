{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LargeHashable.Benchmarks.Serial (serialLargeHash) where

import Data.LargeHashable.Intern
import Data.LargeHashable.Class
import Data.Bytes.Put
import Data.Bytes.Serial

serialLargeHash :: Serial a => HashAlgorithm h -> a -> h
serialLargeHash algo a = runLH algo $ serialize a

instance MonadPut LH where
    flush = return ()

    putWord8 = updateHash
    putWord16host = updateHash
    putWord32host = updateHash
    putWord64host = updateHash
    putWordhost = updateHash

    putWord16le = updateHash
    putWord32le = updateHash
    putWord64le = updateHash

    putWord16be = updateHash
    putWord32be = updateHash
    putWord64be = updateHash

    putByteString = updateHash
    putLazyByteString = updateHash
