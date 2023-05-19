{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Data.LargeHashable.Tests.Class where

-- keep imports in alphabetic order (in Emacs, use "M-x sort-lines")
import Data.Fixed
import Data.Hashable
import Data.Int
import Data.LargeHashable
import Data.LargeHashable.Tests.Helper
import Data.Map (Map ())
import Data.Ratio
import Data.Set (Set ())
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.TAI
import Data.Time.LocalTime
import Data.Word
import Test.Framework hiding (Fixed (..))
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as Map
import qualified Data.Scientific as Sci
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Strict.Tuple as Tuple
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

-- of course we can't fully prove uniqueness using
-- properties and there is a small chance of collisions
generic_uniquenessProp :: (Eq a, LargeHashable a)  => a -> a -> Bool
generic_uniquenessProp a b = (a == b) == (largeHash md5HashAlgorithm a == largeHash md5HashAlgorithm b)

prop_integerUniqueness :: Integer -> Integer -> Bool
prop_integerUniqueness = generic_uniquenessProp

test_hashInteger :: IO ()
test_hashInteger =
    do subAssert $ assertHashes 0
       subAssert $ assertHashes (toInteger (maxBound::Int))
       subAssert $ assertHashes (toInteger (minBound::Int))
    where
      assertHashes :: Integer -> IO ()
      assertHashes n =
          do let list = [(n-100)..(n+100)]
                 hashes = map (largeHash md5HashAlgorithm) list
                 hashesSet = Set.fromList hashes
             assertEqual (length list) (Set.size hashesSet)

test_hashBool :: IO ()
test_hashBool =
    assertBool (largeHash md5HashAlgorithm True /= largeHash md5HashAlgorithm False)

prop_genericUniqueness :: TestA -> TestA -> Bool
prop_genericUniqueness = generic_uniquenessProp

prop_intUniqueness :: Int -> Int -> Bool
prop_intUniqueness = generic_uniquenessProp

prop_int8Uniqueness :: Int8 -> Int8 -> Bool
prop_int8Uniqueness = generic_uniquenessProp

prop_int16Uniqueness :: Int16 -> Int16 -> Bool
prop_int16Uniqueness = generic_uniquenessProp

prop_int32Uniqueness :: Int32 -> Int32 -> Bool
prop_int32Uniqueness = generic_uniquenessProp

prop_int64Uniqueness :: Int64 -> Int64 -> Bool
prop_int64Uniqueness = generic_uniquenessProp

prop_wordUniqueness :: Word -> Word -> Bool
prop_wordUniqueness = generic_uniquenessProp

prop_word8Uniqueness :: Word8 -> Word8 -> Bool
prop_word8Uniqueness = generic_uniquenessProp

prop_word16Uniqueness :: Word16 -> Word16 -> Bool
prop_word16Uniqueness = generic_uniquenessProp

prop_word32Uniqueness :: Word32 -> Word32 -> Bool
prop_word32Uniqueness = generic_uniquenessProp

prop_word64Uniqueness :: Word64 -> Word64 -> Bool
prop_word64Uniqueness = generic_uniquenessProp

prop_charUniqueness :: Char -> Char -> Bool
prop_charUniqueness = generic_uniquenessProp

prop_bytestringUniqueness :: B.ByteString -> B.ByteString -> Bool
prop_bytestringUniqueness = generic_uniquenessProp

prop_appendByteStringOk :: B.ByteString -> B.ByteString -> Bool
prop_appendByteStringOk b1 b2 =
    runMD5 (updateHash (b1 `B.append` b2)) /=
    runMD5 (updateHash b1 >> updateHash b2)

test_irrelevantByteStringChunking :: IO ()
test_irrelevantByteStringChunking = do
    assertEqual (largeHash md5HashAlgorithm (BL.fromChunks ["foo", "ba", "r"]))
                (largeHash md5HashAlgorithm (BL.fromChunks ["foob", "ar"]))

prop_lazyBytestringUniqueness :: BL.ByteString -> BL.ByteString -> Bool
prop_lazyBytestringUniqueness = generic_uniquenessProp

prop_appendLazyByteStringOk :: BL.ByteString -> BL.ByteString -> Bool
prop_appendLazyByteStringOk b1 b2 =
    runMD5 (updateHash (b1 `BL.append` b2)) /=
    runMD5 (updateHash b1 >> updateHash b2)

prop_shortBytestringUniqueness :: BS.ShortByteString -> BS.ShortByteString -> Bool
prop_shortBytestringUniqueness = generic_uniquenessProp

prop_textUniqueness :: T.Text -> T.Text -> Bool
prop_textUniqueness = generic_uniquenessProp

prop_appendTextOk :: T.Text -> T.Text -> Bool
prop_appendTextOk t1 t2 =
    runMD5 (updateHash (t1 `T.append` t2)) /=
    runMD5 (updateHash t1 >> updateHash t2)

test_irrelevantTextChunking :: IO ()
test_irrelevantTextChunking = do
    assertEqual (largeHash md5HashAlgorithm (TL.fromChunks ["don't", " pa", "nic"]))
                (largeHash md5HashAlgorithm (TL.fromChunks ["don", "'t p", "an", "ic"]))

prop_lazyTextUniqueness :: TL.Text -> TL.Text -> Bool
prop_lazyTextUniqueness = generic_uniquenessProp

prop_appendLazyTextOk :: TL.Text -> TL.Text -> Bool
prop_appendLazyTextOk t1 t2 =
    runMD5 (updateHash (t1 `TL.append` t2)) /=
    runMD5 (updateHash t1 >> updateHash t2)

prop_listUniqueness :: [Bool] -> [Bool] -> Bool
prop_listUniqueness = generic_uniquenessProp

prop_appendListOk :: [Int] -> [Int] -> Bool
prop_appendListOk l1 l2 =
    runMD5 (updateHash (l1 ++ l2)) /=
    runMD5 (updateHash l1 >> updateHash l2)

prop_DoubleUniqueness :: Double -> Double -> Bool
prop_DoubleUniqueness = generic_uniquenessProp

prop_FloatUniqueness :: Float -> Float -> Bool
prop_FloatUniqueness = generic_uniquenessProp

prop_setUniqueness :: Set Int -> Set Int -> Bool
prop_setUniqueness = generic_uniquenessProp

test_hashSet :: IO ()
test_hashSet =
    do let s1 = Set.fromList [1::Int, 2, 3]
           s2 = Set.fromList [3::Int, 2, 1]
           s3 = Set.fromList [2::Int, 1, 3]
       assertEqual (largeHash md5HashAlgorithm s1) (largeHash md5HashAlgorithm s2)
       assertEqual (largeHash md5HashAlgorithm s1) (largeHash md5HashAlgorithm s3)
       assertEqual (largeHash md5HashAlgorithm s2) (largeHash md5HashAlgorithm s3)

prop_unionSetOk :: Set Int -> Set Int -> Bool
prop_unionSetOk s1 s2 =
    runMD5 (updateHash (s1 `Set.union` s2)) /= runMD5 (updateHash s1 >> updateHash s2)

prop_intSetUniqueness :: IS.IntSet -> IS.IntSet -> Bool
prop_intSetUniqueness = generic_uniquenessProp

test_hashIntSet :: IO ()
test_hashIntSet =
    do let s1 = IS.fromList [1::Int, 2, 3]
           s2 = IS.fromList [3::Int, 2, 1]
           s3 = IS.fromList [2::Int, 1, 3]
       assertEqual (largeHash md5HashAlgorithm s1) (largeHash md5HashAlgorithm s2)
       assertEqual (largeHash md5HashAlgorithm s1) (largeHash md5HashAlgorithm s3)
       assertEqual (largeHash md5HashAlgorithm s2) (largeHash md5HashAlgorithm s3)

prop_unionIntSetOk :: IS.IntSet -> IS.IntSet -> Bool
prop_unionIntSetOk s1 s2 =
    runMD5 (updateHash (s1 `IS.union` s2)) /=
    runMD5 (updateHash s1 >> updateHash s2)

prop_hashSetUniqueness :: HS.HashSet Int -> HS.HashSet Int -> Bool
prop_hashSetUniqueness = generic_uniquenessProp

data ConstHash
    = ConstHash Int
      deriving (Eq)

instance Hashable ConstHash where
    hashWithSalt _ _ = 0

instance LargeHashable ConstHash where
    updateHash (ConstHash i) = updateHash i

test_hashHashSet :: IO ()
test_hashHashSet =
    do let s1 = HS.fromList [ConstHash 1, ConstHash 2, ConstHash 3]
           s2 = HS.fromList [ConstHash 3, ConstHash 2, ConstHash 1]
           s3 = HS.fromList [ConstHash 2, ConstHash 1, ConstHash 3]
       assertEqual (largeHash md5HashAlgorithm s1) (largeHash md5HashAlgorithm s2)
       assertEqual (largeHash md5HashAlgorithm s1) (largeHash md5HashAlgorithm s3)
       assertEqual (largeHash md5HashAlgorithm s2) (largeHash md5HashAlgorithm s3)

prop_unionHashSetOk :: HS.HashSet Int -> HS.HashSet Int -> Bool
prop_unionHashSetOk s1 s2 =
    runMD5 (updateHash (s1 `HS.union` s2)) /=
    runMD5 (updateHash s1 >> updateHash s2)

prop_mapUniqueness :: Map Int String -> Map Int String -> Bool
prop_mapUniqueness = generic_uniquenessProp

test_hashMap :: IO ()
test_hashMap =
    do let m1 = Map.fromList [(1::Int, "1"::String), (2, "2"), (3, "3")]
           m2 = Set.fromList [(3::Int, "3"::String), (2, "2"), (1, "1")]
           m3 = Set.fromList [(2::Int, "2"::String), (1, "1"), (3, "3")]
       assertEqual (largeHash md5HashAlgorithm m1) (largeHash md5HashAlgorithm m2)
       assertEqual (largeHash md5HashAlgorithm m1) (largeHash md5HashAlgorithm m3)
       assertEqual (largeHash md5HashAlgorithm m2) (largeHash md5HashAlgorithm m3)

prop_unionMapOk :: Map Int Bool -> Map Int Bool -> Bool
prop_unionMapOk m1 m2 =
    runMD5 (updateHash (m1 `Map.union` m2)) /= runMD5 (updateHash m1 >> updateHash m2)

prop_intMapUniqueness :: IM.IntMap String -> IM.IntMap String -> Bool
prop_intMapUniqueness = generic_uniquenessProp

test_hashIntMap :: IO ()
test_hashIntMap =
    do let m1 = IM.fromList [(1::Int, "1"::String), (2, "2"), (3, "3")]
           m2 = IM.fromList [(3::Int, "3"::String), (2, "2"), (1, "1")]
           m3 = IM.fromList [(2::Int, "2"::String), (1, "1"), (3, "3")]
       assertEqual (largeHash md5HashAlgorithm m1) (largeHash md5HashAlgorithm m2)
       assertEqual (largeHash md5HashAlgorithm m1) (largeHash md5HashAlgorithm m3)
       assertEqual (largeHash md5HashAlgorithm m2) (largeHash md5HashAlgorithm m3)

prop_unionIntMapOk :: IM.IntMap Bool -> IM.IntMap Bool -> Bool
prop_unionIntMapOk m1 m2 =
    runMD5 (updateHash (m1 `IM.union` m2)) /= runMD5 (updateHash m1 >> updateHash m2)

prop_hashMapUniqueness :: HM.HashMap Int Bool -> HM.HashMap Int Bool -> Bool
prop_hashMapUniqueness = generic_uniquenessProp

test_hashHashMap :: IO ()
test_hashHashMap =
    do let val = "foo" :: String
           m1 = HM.fromList [(ConstHash 1, val), (ConstHash 2, val), (ConstHash 3, val)]
           m2 = HM.fromList [(ConstHash 3, val), (ConstHash 2, val), (ConstHash 1, val)]
           m3 = HM.fromList [(ConstHash 2, val), (ConstHash 1, val), (ConstHash 3, val)]
       assertEqual (largeHash md5HashAlgorithm m1) (largeHash md5HashAlgorithm m2)
       assertEqual (largeHash md5HashAlgorithm m1) (largeHash md5HashAlgorithm m3)
       assertEqual (largeHash md5HashAlgorithm m2) (largeHash md5HashAlgorithm m3)

prop_unionHashMapOk :: HM.HashMap Int Bool -> HM.HashMap Int Bool -> Bool
prop_unionHashMapOk m1 m2 =
    runMD5 (updateHash (m1 `HM.union` m2)) /= runMD5 (updateHash m1 >> updateHash m2)

prop_hashMapLazyUniqueness :: HML.HashMap Int Bool -> HML.HashMap Int Bool -> Bool
prop_hashMapLazyUniqueness = generic_uniquenessProp

test_hashHashMapLazy :: IO ()
test_hashHashMapLazy =
    do let val = "foo" :: String
           m1 = HML.fromList [(ConstHash 1, val), (ConstHash 2, val), (ConstHash 3, val)]
           m2 = HML.fromList [(ConstHash 3, val), (ConstHash 2, val), (ConstHash 1, val)]
           m3 = HML.fromList [(ConstHash 2, val), (ConstHash 1, val), (ConstHash 3, val)]
       assertEqual (largeHash md5HashAlgorithm m1) (largeHash md5HashAlgorithm m2)
       assertEqual (largeHash md5HashAlgorithm m1) (largeHash md5HashAlgorithm m3)
       assertEqual (largeHash md5HashAlgorithm m2) (largeHash md5HashAlgorithm m3)

prop_hashPairUniqueness :: (Int, Int) -> (Int, Int) -> Bool
prop_hashPairUniqueness = generic_uniquenessProp

prop_hashTripleUniqueness :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
prop_hashTripleUniqueness = generic_uniquenessProp

prop_hashQuadrupleUniqueness :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
prop_hashQuadrupleUniqueness = generic_uniquenessProp

prop_hashQuintupleUniqueness :: (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int) -> Bool
prop_hashQuintupleUniqueness = generic_uniquenessProp

prop_hashStrictPairUniqueness :: Tuple.Pair Int Int -> Tuple.Pair Int Int -> Bool
prop_hashStrictPairUniqueness = generic_uniquenessProp

prop_hashMaybeUniqueness :: Maybe Int -> Maybe Int -> Bool
prop_hashMaybeUniqueness = generic_uniquenessProp

prop_hashEitherUniqueness :: Either Int Bool -> Either Int Bool -> Bool
prop_hashEitherUniqueness = generic_uniquenessProp

prop_hashOrderingUniqueness :: Ordering -> Ordering -> Bool
prop_hashOrderingUniqueness = generic_uniquenessProp

prop_ratioUniqueness :: Ratio Int -> Ratio Int -> Bool
prop_ratioUniqueness = generic_uniquenessProp

test_ratio :: IO ()
test_ratio =
    do let r1 = (1::Int) % 2
           r2 = (2::Int) % 4
       assertEqual (largeHash md5HashAlgorithm r1) (largeHash md5HashAlgorithm r2)

prop_dayUniqueness :: Day -> Day -> Bool
prop_dayUniqueness = generic_uniquenessProp

prop_diffTimeUniqueness :: DiffTime -> DiffTime -> Bool
prop_diffTimeUniqueness = generic_uniquenessProp

prop_utcTimeUniqueness :: UTCTime -> UTCTime -> Bool
prop_utcTimeUniqueness = generic_uniquenessProp

prop_absoluteTimeUniqueness :: AbsoluteTime -> AbsoluteTime -> Bool
prop_absoluteTimeUniqueness = generic_uniquenessProp

prop_nominalDiffTimeUniqueness :: NominalDiffTime -> NominalDiffTime -> Bool
prop_nominalDiffTimeUniqueness = generic_uniquenessProp

prop_timeZoneUniqueness :: TimeZone -> TimeZone -> Bool
prop_timeZoneUniqueness = generic_uniquenessProp

prop_timeOfDayUniqueness :: TimeOfDay -> TimeOfDay -> Bool
prop_timeOfDayUniqueness = generic_uniquenessProp

prop_localTimeUniqueness :: LocalTime -> LocalTime -> Bool
prop_localTimeUniqueness = generic_uniquenessProp

prop_fixedUniqueness :: Fixed E1 -> Fixed E1 -> Bool
prop_fixedUniqueness = generic_uniquenessProp

prop_scientificUniqueness :: Sci.Scientific -> Sci.Scientific -> Bool
prop_scientificUniqueness = generic_uniquenessProp

prop_aesonValueUniqueness :: J.Value -> J.Value -> Bool
prop_aesonValueUniqueness = generic_uniquenessProp

prop_vectorUniqueness :: V.Vector Int -> V.Vector Int -> Bool
prop_vectorUniqueness = generic_uniquenessProp

prop_appendVectorOk :: V.Vector Int -> V.Vector Int -> Bool
prop_appendVectorOk v1 v2 =
    runMD5 (updateHash (v1 V.++ v2)) /=
    runMD5 (updateHash v1 >> updateHash v2)

prop_seqUniqueness :: Seq.Seq Int -> Seq.Seq Int -> Bool
prop_seqUniqueness = generic_uniquenessProp

prop_appendSeqOk :: Seq.Seq Int -> Seq.Seq Int -> Bool
prop_appendSeqOk s1 s2 =
    runMD5 (updateHash (s1 Seq.>< s2)) /=
    runMD5 (updateHash s1 >> updateHash s2)

-- regression test for #25
test_textHash :: IO ()
test_textHash = do
  let t1 = T.pack "abcdefgh"
      t2 = T.pack "abcdxxxx"
  assertEqual
    (largeHash md5HashAlgorithm (T.take 4 t1))
    (largeHash md5HashAlgorithm (T.take 4 t2))
  assertNotEqual
    (largeHash md5HashAlgorithm (T.take 5 t1))
    (largeHash md5HashAlgorithm (T.take 5 t2))
