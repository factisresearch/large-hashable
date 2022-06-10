{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.LargeHashable.Tests.Helper where

-- keep imports in alphabetic order (in Emacs, use "M-x sort-lines")
import Control.Monad
import Data.Bytes.Serial
import Data.Hashable
import Data.LargeHashable
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.TAI
import Data.Time.LocalTime
import GHC.Generics
import Test.QuickCheck
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Scientific as Sci
import qualified Data.Strict.Tuple as Tuple
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

#if !MIN_VERSION_QuickCheck(2,8,2)
-- keep imports in alphabetic order (in Emacs, use "M-x sort-lines")
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
#endif

instance Arbitrary T.Text where
    arbitrary = fmap T.pack arbitrary
    shrink = map T.pack . shrink . T.unpack

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack arbitrary
    shrink = map B.pack . shrink . B.unpack

instance Arbitrary BL.ByteString where
    arbitrary = fmap BL.fromChunks arbitrary
    shrink = map BL.fromChunks . shrink . BL.toChunks

instance Arbitrary TL.Text where
    arbitrary = fmap TL.fromChunks arbitrary
    shrink = map TL.fromChunks . shrink . TL.toChunks

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = liftM V.fromList arbitrary
    shrink = map V.fromList . shrink . V.toList

instance Arbitrary Sci.Scientific where
    arbitrary =
        liftM2 Sci.scientific arbitrary arbitrary
    shrink = shrinkRealFrac

#if !MIN_VERSION_aeson(2,0,0)
instance Arbitrary J.Value where
    arbitrary = sized arbitraryJsonValue
        where
          arbitraryJsonValue n =
              if n <= 0
              then oneof simpleGens
              else frequency $
                       (map (\g -> (1, g)) simpleGens) ++
                       [(3, arbitraryObject (n `div` 2))
                       ,(3, arbitraryArray (n `div` 2))]
          arbitraryObject n =
              do l <- arbitraryListOfJsonValues n
                 elems <- forM l $ \v ->
                     do k <-
                            elements
                                ["key", "foo", "bar", "baz", "spam", "egg", "chicken", "dog"
                                ,"any", "what", "santa", "mark", "wardrobe", "baseball"]
                        return (k, v)
                 return (J.Object (HM.fromList elems))
          arbitraryArray n =
              do l <- arbitraryListOfJsonValues n
                 return (J.Array (V.fromList l))
          arbitraryListOfJsonValues n =
              do size <- elements [(0::Int)..10]
                 forM [1..(size::Int)] $ \_ -> arbitraryJsonValue n
          simpleGens =
            [liftM J.String arbitrary
            ,liftM J.Number arbitrary
            ,liftM J.Bool arbitrary
            ,return J.Null]
    shrink v =
        case v of
          J.Object obj ->
              map J.Object (shrink obj)
          J.Array arr ->
              map J.Array (shrink arr)
          J.String t ->
              map J.String (shrink t)
          J.Number n ->
              map J.Number (shrink n)
          J.Bool _ -> []
          J.Null -> []
#endif

instance (Eq k, Hashable k, Arbitrary k, Arbitrary a) => Arbitrary (HML.HashMap k a) where
    arbitrary = fmap HML.fromList arbitrary
    shrink = map HML.fromList . shrink . HML.toList

instance (Eq a, Hashable a, Arbitrary a) => Arbitrary (HS.HashSet a) where
    arbitrary = fmap HS.fromList arbitrary
    shrink = map HS.fromList . shrink . HS.toList

#if !MIN_VERSION_QuickCheck(2,8,2)
instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (M.Map k a) where
    arbitrary = fmap M.fromList arbitrary
    shrink = map M.fromList . shrink . M.toList

instance Arbitrary a => Arbitrary (IM.IntMap a) where
    arbitrary = fmap IM.fromList arbitrary
    shrink = map IM.fromList . shrink . IM.toList

instance (Ord a, Arbitrary a) => Arbitrary (S.Set a) where
    arbitrary = fmap S.fromList arbitrary
    shrink = map S.fromList . shrink . S.toList

instance Arbitrary IS.IntSet where
    arbitrary = fmap IS.fromList arbitrary
    shrink = map IS.fromList . shrink . IS.toList

#endif

data TestA
    = TestA
    { age :: Int
    , wealth :: Integer
    , isStudent :: Bool
    , name :: T.Text
    , initials :: [Char]
    , legLength :: (Int, Int)
    } deriving (Generic, Eq, Show)

instance LargeHashable TestA
instance Serial TestA

instance Arbitrary TestA where
    arbitrary =
        TestA <$> arbitrary <*> arbitrary <*> arbitrary <*>
              arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BS.ShortByteString where
    arbitrary = liftM BS.pack arbitrary
    shrink x =
        map BS.pack (shrink (BS.unpack x))

instance Arbitrary Day where
    arbitrary =
        do n <- arbitrary
           return (ModifiedJulianDay (n `mod` 1000))
    shrink (ModifiedJulianDay n) =
        map ModifiedJulianDay (shrink n)

instance Arbitrary DiffTime where
    arbitrary =
        liftM picosecondsToDiffTime arbitrary
    shrink delta =
        let n = toRational delta
        in map fromRational (shrink n)

instance Arbitrary UTCTime where
    arbitrary =
        do day <- arbitrary
           picos <- arbitrary
           return (UTCTime day (picosecondsToDiffTime (picos `mod` 86401)))

instance Arbitrary AbsoluteTime where
    arbitrary =
        do diff <- arbitrary
           return (addAbsoluteTime diff taiEpoch)

instance Arbitrary NominalDiffTime where
    arbitrary =
        do r <- arbitrary
           return (fromRational r)

instance Arbitrary TimeZone where
    arbitrary =
        do n <- arbitrary
           s <- arbitrary
           b <- arbitrary
           return (TimeZone (n `mod` (60*24)) b s)

instance Arbitrary TimeOfDay where
    arbitrary =
        do h <- arbitrary
           m <- arbitrary
           s <- arbitrary
           return (TimeOfDay (h `mod` 24) (m `mod` 60) (fromInteger (s `mod` 61)))

instance Arbitrary LocalTime where
    arbitrary =
        do d <- arbitrary
           t <- arbitrary
           return (LocalTime d t)

instance Arbitrary ZonedTime where
    arbitrary =
        do t <- arbitrary
           z <- arbitrary
           return (ZonedTime t z)

instance Arbitrary UniversalTime where
    arbitrary =
        do r <- arbitrary
           return (ModJulianDate r)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Tuple.Pair a b) where
    arbitrary =
        do x <- arbitrary
           y <- arbitrary
           return (x Tuple.:!: y)

instance Arbitrary Word128 where
    arbitrary = Word128 <$> arbitrary <*> arbitrary

instance Arbitrary Word256 where
    arbitrary = Word256 <$> arbitrary <*> arbitrary
