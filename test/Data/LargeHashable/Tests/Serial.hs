{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Data.LargeHashable.Tests.Serial where

import Test.Framework

import Data.LargeHashable
import Data.LargeHashable.Tests.Helper
import Data.Bytes.Serial

generic_uniquenessProp :: (Eq a, Serial a)  => a -> a -> Bool
generic_uniquenessProp a b = (a == b) == (serialLargeHash md5HashAlgorithm a == serialLargeHash md5HashAlgorithm b)

prop_genericUniqueness :: TestA -> TestA -> Bool
prop_genericUniqueness = generic_uniquenessProp
