-- | An implementation of 'HashAlgorithm' for MD5 (https://www.ietf.org/rfc/rfc1321.txt).
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CApiFFI #-}
module Data.LargeHashable.MD5 (

    MD5Hash(..), md5HashAlgorithm, runMD5

) where

-- keep imports in alphabetic order (in Emacs, use "M-x sort-lines")
import Data.LargeHashable.Intern
import Data.LargeHashable.LargeWord
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC

newtype MD5Hash = MD5Hash { unMD5Hash :: Word128 }
    deriving (Eq, Ord)

instance Show MD5Hash where
    show (MD5Hash w) =
        BSC.unpack (Base16.encode (w128ToBs w))

foreign import capi unsafe "md5.h md5_init"
    c_md5_init :: Ptr RawCtx -> IO ()

foreign import capi unsafe "md5.h md5_update"
    c_md5_update :: Ptr RawCtx -> Ptr Word8 -> Int -> IO ()

foreign import capi unsafe "md5.h md5_update_uchar"
    c_md5_update_uchar :: Ptr RawCtx -> Word8 -> IO ()

foreign import capi unsafe "md5.h md5_update_ushort"
    c_md5_update_ushort :: Ptr RawCtx -> Word16 -> IO ()

foreign import capi unsafe "md5.h md5_update_uint"
    c_md5_update_uint :: Ptr RawCtx -> Word32 -> IO ()

foreign import capi unsafe "md5.h md5_update_ulong"
    c_md5_update_ulong :: Ptr RawCtx -> Word64 -> IO ()

foreign import capi unsafe "md5.h md5_finalize"
    c_md5_finalize :: Ptr RawCtx -> Ptr Word8 -> IO ()

{-# INLINE digestSize #-}
digestSize :: Int
digestSize = 16

{-# INLINE sizeCtx #-}
sizeCtx :: Int
sizeCtx = 96

data RawCtx -- phantom type argument

newtype Ctx = Ctx { _unCtx :: Ptr RawCtx }

withCtx :: (Ctx -> IO ()) -> IO MD5Hash
withCtx f =
    allocaBytes sizeCtx $ \(ptr :: Ptr RawCtx) ->
    do c_md5_init ptr
       f (Ctx ptr)
       allocaBytes digestSize $ \(resPtr :: Ptr Word8) ->
           do c_md5_finalize ptr resPtr
              let first = castPtr resPtr :: Ptr Word64
              w1 <- peek first
              let second = castPtr (plusPtr resPtr (sizeOf w1)) :: Ptr Word64
              w2 <- peek second
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
          withCtx $ \(Ctx ctxPtr) ->
              let !updates =
                      HashUpdates
                      { hu_updatePtr = c_md5_update ctxPtr
                      , hu_updateUChar = c_md5_update_uchar ctxPtr
                      , hu_updateUShort = c_md5_update_ushort ctxPtr
                      , hu_updateUInt = c_md5_update_uint ctxPtr
                      , hu_updateULong = c_md5_update_ulong ctxPtr
                      }
              in f updates

runMD5 :: LH () -> MD5Hash
runMD5 = runLH md5HashAlgorithm
