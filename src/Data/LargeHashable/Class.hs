{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
module Data.LargeHashable.Class (

    LargeHashable(..), largeHash

) where

import Data.LargeHashable.Intern
import Data.Char (ord)
import Foreign.C.Types
import Foreign.Ptr
import Data.Word
import Data.Int
import Data.Bits
import Data.Ratio
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

class LargeHashable a where
    updateHash :: a -> LH ()
    default updateHash :: (LargeHashable' (Rep a), Generic a) => a -> LH ()
    updateHash = updateHash' . from

largeHash :: LargeHashable a => HashAlgorithm -> a -> Hash
largeHash algo x = runLH algo (updateHash x)

{-# INLINE updateHashText #-}
updateHashText :: T.Text -> LH ()
updateHashText !t = do
    updates <- hashUpdates
    ioInLH $ do
        hu_updateULong updates (fromIntegral (T.length t))
        TF.useAsPtr t $ \valPtr units ->
            hu_updatePtr updates (castPtr valPtr) (fromIntegral (2 * units))
        return ()

instance LargeHashable T.Text where
    updateHash = updateHashText

{-# INLINE updateHashByteString #-}
updateHashByteString :: B.ByteString -> LH ()
updateHashByteString !b = do
    updates <- hashUpdates
    ioInLH $ do
        ptr <- B.useAsCString b return
        let length = B.length b
        hu_updateULong updates (fromIntegral length)
        hu_updatePtr updates (castPtr ptr)  length

instance LargeHashable B.ByteString where
    updateHash = updateHashByteString

instance LargeHashable BL.ByteString where
    -- TODO: Optimize
    updateHash = BL.foldl (\m w -> m >> updateHash w) (return ())

{-# INLINE updateHashBoundedIntegral #-}
-- Note: This only works if a's bounds are smaller or
--       equal to CULong's bounds
updateHashBoundedIntegral :: (Bounded a, Integral a) => a -> LH ()
updateHashBoundedIntegral !i = do
    updates <- hashUpdates
    ioInLH $ hu_updateULong updates (fromIntegral i)

instance LargeHashable Int where
    updateHash = updateHashBoundedIntegral

instance LargeHashable Int8 where
    updateHash = updateHashBoundedIntegral

instance LargeHashable Int16 where
    updateHash = updateHashBoundedIntegral

instance LargeHashable Int32 where
    updateHash = updateHashBoundedIntegral

instance LargeHashable Int64 where
    updateHash = updateHashBoundedIntegral

instance LargeHashable Word where
    updateHash = updateHashBoundedIntegral

instance LargeHashable Word8 where
    updateHash = updateHashBoundedIntegral

instance LargeHashable Word16 where
    updateHash = updateHashBoundedIntegral

instance LargeHashable Word32 where
    updateHash = updateHashBoundedIntegral

instance LargeHashable Word64 where
    updateHash = updateHash . CULong

instance LargeHashable Char where
    -- TODO: ord can't be used for 100% of unicode
    --       characters since it uses int.
    --       We could remove this limitation through
    --       a dependency to utf8-light.
    updateHash = updateHash . ord

instance LargeHashable CULong where
    updateHash l = hashUpdates >>= \hu -> ioInLH $ hu_updateULong hu l

{-# INLINE updateHashInteger #-}
updateHashInteger :: Integer -> LH ()
updateHashInteger !i
    | i == 0 = updateHash (0 :: CULong)
    | i > 0  = do
        updateHash (fromIntegral (i .&. 0xffffffffffffffff) :: CULong)
        updateHashInteger (shift i (-64))
    | otherwise = do
        updateHash (0 :: CULong) -- prepend 0 to show it is negative
        updateHashInteger (abs i)

instance LargeHashable Integer where
    updateHash = updateHashInteger

{-# INLINE updateHashBool #-}
updateHashBool :: Bool -> LH ()
updateHashBool !True  = updateHash (1 :: CULong)
updateHashBool !False = updateHash (0 :: CULong)

instance LargeHashable Bool where
    updateHash = updateHashBool

{-# INLINE updateHashList #-}
updateHashList :: LargeHashable a => [a] -> LH ()
updateHashList = loop 0
    where
      loop :: LargeHashable a => Int -> [a] -> LH ()
      loop !i [] =
          updateHash i
      loop !i (x:xs) = do
          updateHash x
          loop (i + 1) xs

instance LargeHashable a => LargeHashable [a] where
    updateHash = updateHashList

{-# INLINE updateHashTuple #-}
updateHashTuple :: (LargeHashable a, LargeHashable b) => (a, b) -> LH ()
updateHashTuple (!a, !b) = do
    updateHash a
    updateHash b

instance (LargeHashable a, LargeHashable b) => LargeHashable (a, b) where
    updateHash = updateHashTuple

{-# INLINE updateHashMaybe #-}
updateHashMaybe :: LargeHashable a => Maybe a -> LH ()
updateHashMaybe !Nothing   = updateHash (0 :: CULong)
updateHashMaybe !(Just !x) = do
    updateHash (1 :: CULong)
    updateHash x

instance LargeHashable a => LargeHashable (Maybe a) where
    updateHash = updateHashMaybe

instance LargeHashable () where
    updateHash () = updateHash (0 :: CULong)

instance LargeHashable Ordering where
    updateHash EQ = updateHash (0  :: CULong)
    updateHash GT = updateHash (-1 :: CULong)
    updateHash LT = updateHash (1  :: CULong)

instance (Integral a, LargeHashable a) => LargeHashable (Ratio a) where
    updateHash !i = do
        updateHash $ numerator i
        updateHash $ denominator i

class LargeHashable' f where
    updateHash' :: f p -> LH ()

instance LargeHashable' V1 where
    updateHash' = undefined

instance LargeHashable' U1 where
    updateHash' _ = updateHash ()

instance (LargeHashable' f, LargeHashable' g) => LargeHashable' (f :+: g) where
    updateHash' (L1 x) = do
        updateHash (0 :: CULong) -- is left
        updateHash' x
    updateHash' (R1 x) = do
        updateHash (1 :: CULong) -- is right
        updateHash' x

instance (LargeHashable' f, LargeHashable' g) => LargeHashable' (f :*: g) where
    updateHash' (x :*: y) = updateHash' x >> updateHash' y

instance LargeHashable c => LargeHashable' (K1 i c) where
    updateHash' (K1 x) = updateHash x

-- ignore meta-info (for now)
instance (LargeHashable' f) => LargeHashable' (M1 i t f) where
      updateHash' (M1 x) = updateHash' x
