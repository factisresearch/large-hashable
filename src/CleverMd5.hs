{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CleverMd5 where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Internal as TI
import qualified Data.Text.Array as TA
import qualified Data.Text.Foreign as TF
import System.IO.Unsafe (unsafePerformIO)

data HashUpdates
    = HashUpdates
    { hu_updatePtr :: !(Ptr Word8 -> Int -> IO ())
    , hu_updateULong :: !(CULong -> IO ())
    }

data HashAlgorithm result
    = HashAlgorithm
    { ha_run :: (HashUpdates -> IO ()) -> IO result
    }

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

data Hash = Hash !Word64 !Word64
            deriving (Show, Eq)

data RawCtx -- phantom type argument

newtype Ctx = Ctx { unCtx :: Ptr RawCtx }

withCtx :: (Ctx -> IO ()) -> IO Hash
withCtx f =
    allocaBytes sizeCtx $ \(ptr :: Ptr RawCtx) ->
    do c_md5_init ptr
       f (Ctx ptr)
       allocaBytes digestSize $ \(resPtr :: Ptr Word64) ->
           do c_md5_finalize ptr (castPtr resPtr)
              fst <- peek resPtr
              snd <- peekElemOff resPtr 8
              return (Hash fst snd)

md5Algorithm :: HashAlgorithm Hash
md5Algorithm =
    HashAlgorithm run
    where
      run f =
          withCtx $ \(Ctx ctxPtr) ->
              let !updates =
                      HashUpdates (c_md5_update ctxPtr)
                          (c_md5_update_ulong ctxPtr)
              in f updates

runMD5 :: LH () -> Hash
runMD5 = runLH md5Algorithm

newtype LH a = LH { unLH :: ReaderT HashUpdates IO a }
    deriving (Functor, Applicative, Monad)

getUpdates :: LH HashUpdates
getUpdates = LH ask

ioInLH :: IO a -> LH a
ioInLH = LH . liftIO

{-# NOINLINE runLH #-}
runLH :: HashAlgorithm r -> LH () -> r
runLH alg (LH lh) =
    unsafePerformIO $
    ha_run alg $
    runReaderT lh

class LargeHashable a where
    updateHash :: a -> LH ()

largeHash :: LargeHashable a => a -> Hash
largeHash x = runMD5 (updateHash x)

{-# INLINE updateHashText #-}
updateHashText :: T.Text -> LH ()
updateHashText !t = do
    updates <- getUpdates
    ioInLH $ do
        hu_updateULong updates (fromIntegral (T.length t))
        TF.useAsPtr t $ \valPtr units ->
            hu_updatePtr updates (castPtr valPtr) (fromIntegral (2 * units))
        return ()

instance LargeHashable T.Text where
    updateHash = updateHashText

prop_appendTextOk :: T.Text -> T.Text -> Bool
prop_appendTextOk t1 t2 =
    runMD5 (updateHash (t1 `T.append` t2)) /=
    runMD5 (updateHash t1 >> updateHash t2)

{-# INLINE updateHashInt #-}
updateHashInt :: Int -> LH ()
updateHashInt !i = do
    updates <- getUpdates
    ioInLH $ hu_updateULong updates (fromIntegral i)

instance LargeHashable Int where
    updateHash = updateHashInt

{-# INLINE updateHashList #-}
updateHashList :: LargeHashable a => [a] -> LH ()
updateHashList = loop 0
    where
      loop !i [] =
          updateHashInt i
      loop !i (x:xs) = do
          updateHash x
          loop (i + 1) xs

instance LargeHashable a => LargeHashable [a] where
    updateHash = updateHashList

prop_appendListOk :: [Int] -> [Int] -> Bool
prop_appendListOk l1 l2 =
    runMD5 (updateHash (l1 ++ l2)) /=
    runMD5 (updateHash l1 >> updateHash l2)
