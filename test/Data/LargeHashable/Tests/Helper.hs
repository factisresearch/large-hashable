{-# LANGUAGE DeriveGeneric #-}
module Data.LargeHashable.Tests.Helper where

import Control.Monad
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Data.LargeHashable
import Data.Bytes.Serial

instance Arbitrary T.Text where
    arbitrary = liftM T.pack arbitrary
    shrink t =
        map T.pack (shrink (T.unpack t))

instance Arbitrary B.ByteString where
    arbitrary = liftM B.pack arbitrary
    shrink t =
        map B.pack (shrink (B.unpack t))

instance Arbitrary BL.ByteString where
    arbitrary = liftM BL.fromChunks arbitrary
    shrink t =
        map BL.fromChunks (shrink (BL.toChunks t))

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
