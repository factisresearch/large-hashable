{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Data.LargeHashable.Tests.Class where

import Test.Framework

import Data.LargeHashable
import Data.LargeHashable.MD5
import Data.LargeHashable.Tests.Helper
import qualified Data.Text as T
import Data.ByteString (ByteString ())

prop_appendTextOk :: T.Text -> T.Text -> Bool
prop_appendTextOk t1 t2 =
    runMD5 (updateHash (t1 `T.append` t2)) /=
    runMD5 (updateHash t1 >> updateHash t2)

prop_appendListOk :: [Int] -> [Int] -> Bool
prop_appendListOk l1 l2 =
    runMD5 (updateHash (l1 ++ l2)) /=
    runMD5 (updateHash l1 >> updateHash l2)

-- of course we can't fully prove uniqueness using
-- properties and there is a small chance of collisions
generic_uniquenessProp :: (Eq a, LargeHashable a)  => a -> a -> Bool
generic_uniquenessProp a b = (a == b) == (largeHash md5HashAlgorithm a == largeHash md5HashAlgorithm b)

prop_integerUniqueness :: Integer -> Integer -> Bool
prop_integerUniqueness = generic_uniquenessProp

prop_genericUniqueness :: TestA -> TestA -> Bool
prop_genericUniqueness = generic_uniquenessProp

prop_intUniqueness :: Int -> Int -> Bool
prop_intUniqueness = generic_uniquenessProp

prop_wordUniqueness :: Word -> Word -> Bool
prop_wordUniqueness = generic_uniquenessProp

prop_bytestringUniqueness :: ByteString -> ByteString -> Bool
prop_bytestringUniqueness = generic_uniquenessProp
