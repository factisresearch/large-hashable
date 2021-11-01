-- | An implementation of 'HashAlgorithm' for MD5 (https://www.ietf.org/rfc/rfc1321.txt).
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CApiFFI #-}
module Data.LargeHashable.MD5 (

    MD5Hash(..), md5HashAlgorithm, runMD5

) where

-- keep imports in alphabetic order (in Emacs, use "M-x sort-lines")
import Data.ByteArray
import Data.LargeHashable.Intern
import Data.LargeHashable.LargeWord
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import Crypto.Hash (Context, Digest)
import Crypto.Hash.IO (hashInternalUpdate, hashMutableInit, hashMutableFinalize)
import qualified Crypto.Hash.Algorithms as Algorithms

newtype MD5Hash = MD5Hash { unMD5Hash :: Word128 }
    deriving (Eq, Ord)

instance Show MD5Hash where
    show (MD5Hash w) =
        BSC.unpack (Base16.encode (w128ToBs w))

data Ctx a = Ctx { rawCtx :: Ptr (Context a), buf :: Ptr Word8 }

withCtx' :: Algorithms.HashAlgorithm a => a -> (Ctx a -> IO ()) -> IO (Digest a)
withCtx' alg f =
    -- Allocate a scratch buffer to use for updating individual fields
    allocaBytes 64 $ \(buf :: Ptr Word8) -> do
    ctx <- hashMutableInit
    withByteArray ctx $ \rawCtx ->
        f (Ctx rawCtx buf)
    hashMutableFinalize ctx

withCtx :: (Ctx Algorithms.MD5 -> IO ()) -> IO MD5Hash
withCtx f = do
    rawDigest <- withCtx' Algorithms.MD5 f
    withByteArray rawDigest $ \(resPtr :: Ptr Word64) -> do
        w1 <- peek resPtr
        w2 <- peekByteOff resPtr (sizeOf w1)
        return (MD5Hash (Word128 w1 w2))

md5HashAlgorithm :: HashAlgorithm MD5Hash
md5HashAlgorithm =
    HashAlgorithm
    { ha_run = run
    , ha_xor = xorMD5
    , ha_updateHash = updateHash
    }
    where
      xorMD5 (MD5Hash h1) (MD5Hash h2) = MD5Hash (h1 `xorW128` h2)
      updateHash updates (MD5Hash h) =
          let f = hu_updateULong updates
          in do f (w128_first h)
                f (w128_second h)
      run f =
          withCtx $ \(Ctx ctxPtr buf) ->
              let updatePtr ptr len = hashInternalUpdate ctxPtr ptr (fromIntegral len)
                  updatePrimitive :: Storable w => w -> IO ()
                  updatePrimitive w = do
                    poke (castPtr buf) w
                    updatePtr buf (sizeOf w)
                  !updates =
                      HashUpdates
                      { hu_updatePtr = updatePtr
                      , hu_updateUChar = updatePrimitive
                      , hu_updateUShort = updatePrimitive
                      , hu_updateUInt = updatePrimitive
                      , hu_updateULong = updatePrimitive
                      }
              in f updates

runMD5 :: LH () -> MD5Hash
runMD5 = runLH md5HashAlgorithm
