-- | This module defines the central type class `LargeHashable` of this package.
{-# LANGUAGE BangPatterns #-}
module Data.LargeHashable.Class (

    LargeHashable(..), largeHash

) where

import Data.LargeHashable.Intern
import Foreign.Ptr
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF

-- | A type class for computing large hashes (i.e. MD5, SHA256, ...) from
-- haskell values.
--
-- Important: when implementing `LargeHashable` for your custom datatype,
-- make sure to hash all information present in the datatype. Otherwise,
-- unncessary hash collisions arise. A rule of thumb: hash all
-- information that you would also need for serializing/deserializing
-- values of your datatype.
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

{-# INLINE updateHashInt #-}
updateHashInt :: Int -> LH ()
updateHashInt !i = do
    updates <- hashUpdates
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
