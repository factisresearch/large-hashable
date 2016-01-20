module Data.LargeHashable.Tests.Class where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.LargeHashable
import Data.LargeHashable.MD5
import Data.LargeHashable.Tests.Helper
import qualified Data.Text as T

appendProperties :: TestTree
appendProperties = testGroup "Append Tests"
    [ QC.testProperty "bound updateHash /= appending for Text" prop_appendTextOk
    , QC.testProperty "bound updateHash /= appending for [Int]" prop_appendListOk
    ]

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
uniquenessProperties :: TestTree
uniquenessProperties = testGroup "Properties to test uniqueness"
    [ QC.testProperty "Integer hashes are unique" prop_integerUniqueness
    , QC.testProperty "Hashes by generic-derived instances are unique" prop_genericUniqueness
    , QC.testProperty "Hashes of Ints are unique" prop_intUniqueness
    , QC.testProperty "Hashes of Words are unique" prop_wordUniqueness
    ]
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
