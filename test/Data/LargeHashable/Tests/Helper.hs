{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.LargeHashable.Tests.Helper where

import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Data.LargeHashable
import Data.Bytes.Serial

#if !MIN_VERSION_QuickCheck(2,8,2)
import Data.Hashable

import qualified Data.Set as S
import qualified Data.HashSet as HS
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as HM
import qualified Data.IntMap as IM
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

#if !MIN_VERSION_QuickCheck(2,8,2)
instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (M.Map k a) where
    arbitrary = fmap M.fromList arbitrary
    shrink = map M.fromList . shrink . M.toList

instance (Eq k, Hashable k, Arbitrary k, Arbitrary a) => Arbitrary (HM.HashMap k a) where
    arbitrary = fmap HM.fromList arbitrary
    shrink = map HM.fromList . shrink . HM.toList

instance Arbitrary a => Arbitrary (IM.IntMap a) where
    arbitrary = fmap IM.fromList arbitrary
    shrink = map IM.fromList . shrink . IM.toList

instance (Ord a, Arbitrary a) => Arbitrary (S.Set a) where
    arbitrary = fmap S.fromList arbitrary
    shrink = map S.fromList . shrink . S.toList

instance (Eq a, Hashable a, Arbitrary a) => Arbitrary (HS.HashSet a) where
    arbitrary = fmap HS.fromList arbitrary
    shrink = map HS.fromList . shrink . HS.toList

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
    arbitrary = TestA <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
