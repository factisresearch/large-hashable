{-# LANGUAGE DeriveGeneric #-}
module Data.LargeHashable.Tests.Helper where

import Control.Monad
import Test.QuickCheck
import qualified Data.Text as T
import GHC.Generics
import Data.LargeHashable

instance Arbitrary T.Text where
    arbitrary = liftM T.pack arbitrary
    shrink t =
        map T.pack (shrink (T.unpack t))

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

instance Arbitrary TestA where
    arbitrary = TestA <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
