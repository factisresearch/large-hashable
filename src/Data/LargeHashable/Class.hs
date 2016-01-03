{-# LANGUAGE BangPatterns #-}
module Data.LargeHashable.Class (

    LargeHashable(..), largeHash

) where

import Data.LargeHashable.Intern
import Data.Char (ord)
import Foreign.Ptr
import Foreign.Marshal.Array
import Data.Word
import Data.Int
import Data.Bits
import Data.Ratio
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF

class LargeHashable a where
    updateHash :: a -> LH ()

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

{-# INLINE updateHashBoundedIntegral #-}
-- Note: This only works if a's bounds are smaller or
--       equal to CULong's bounds
-- Eval: Omit a compile time warning if that is not the case?
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
    updateHash = updateHashBoundedIntegral

instance LargeHashable Char where
    -- TODO: ord can't be used for 100% of unicode
    --       characters since it uses int.
    --       We could remove this limitation trough
    --       a dependency to utf8-light.
    updateHash = updateHash . ord

{-# INLINE updateHashInteger #-}
updateHashInteger :: Integer -> LH ()
updateHashInteger !i = do
    updates <- hashUpdates
    array <- ioInLH . newArray $ splitted
    ioInLH $ hu_updatePtr updates array (length splitted)
    where split :: Integer -> [Word8]
          split 0 = []
          split i = fromIntegral (i .&. 0xff) : split (shift i (-8))
          splitted = split i

instance LargeHashable Integer where
    updateHash = updateHashInteger

{-# INLINE updateHashBool #-}
updateHashBool :: Bool -> LH ()
updateHashBool !True  = updateHash (1 :: Int)
updateHashBool !False = updateHash (0 :: Int)

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
    updateHash (2 :: Int) -- TODO: Necessary?
    updateHash a
    updateHash b

instance (LargeHashable a, LargeHashable b) => LargeHashable (a, b) where
    updateHash = updateHashTuple

{-# INLINE updateHashMaybe #-}
updateHashMaybe :: LargeHashable a => Maybe a -> LH ()
updateHashMaybe !Nothing   = updateHash (0 :: Int)
updateHashMaybe !(Just !x) = do
    updateHash (1 :: Int)
    updateHash x

instance LargeHashable a => LargeHashable (Maybe a) where
    updateHash = updateHashMaybe

instance LargeHashable () where
    updateHash () = updateHash (0 :: Int)

instance LargeHashable Ordering where
    updateHash EQ = updateHash (0  :: Int)
    updateHash GT = updateHash (-1 :: Int)
    updateHash LT = updateHash (1  :: Int)

instance (Integral a, LargeHashable a) => LargeHashable (Ratio a) where
    updateHash !i = do
        updateHash $ numerator i
        updateHash $ denominator i
