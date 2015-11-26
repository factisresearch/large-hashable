-- | An implementation of 'HashAlgorithm' for MD5 (https://www.ietf.org/rfc/rfc1321.txt).
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.LargeHashable.MD5 (

    md5HashAlgorithm, runMD5

) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Data.Word
import qualified Data.ByteString.Internal as BS

import Data.LargeHashable.Intern

foreign import ccall unsafe "md5.h md5_init"
    c_md5_init :: Ptr RawCtx -> IO ()

foreign import ccall unsafe "md5.h md5_update"
    c_md5_update :: Ptr RawCtx -> Ptr Word8 -> Int -> IO ()

foreign import ccall unsafe "md5.h md5_update_ulong"
    c_md5_update_ulong :: Ptr RawCtx -> CULong -> IO ()

foreign import ccall unsafe "md5.h md5_finalize"
    c_md5_finalize :: Ptr RawCtx -> Ptr Word8 -> IO ()

{-# INLINE digestSize #-}
digestSize :: Int
digestSize = 16

{-# INLINE sizeCtx #-}
sizeCtx :: Int
sizeCtx = 96

data RawCtx -- phantom type argument

newtype Ctx = Ctx { _unCtx :: Ptr RawCtx }

withCtx :: (Ctx -> IO ()) -> IO Hash
withCtx f =
    allocaBytes sizeCtx $ \(ptr :: Ptr RawCtx) ->
    do c_md5_init ptr
       f (Ctx ptr)
       resPtr <- mallocForeignPtrBytes digestSize
       withForeignPtr resPtr $ \resPtr' ->
           c_md5_finalize ptr resPtr'
       let bs = BS.fromForeignPtr resPtr 0 digestSize
       return (Hash bs)

md5HashAlgorithm :: HashAlgorithm
md5HashAlgorithm =
    HashAlgorithm run
    where
      run f =
          withCtx $ \(Ctx ctxPtr) ->
              let !updates =
                      HashUpdates
                          (c_md5_update ctxPtr)
                          (c_md5_update_ulong ctxPtr)
              in f updates

runMD5 :: LH () -> Hash
runMD5 = runLH md5HashAlgorithm