{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Data.LargeHashable.Tests.TH where

import Test.Framework

import Data.LargeHashable

-- | Simple test data structure that embodies most
--   of the diferrent features of a type 
--   the TH deriver can encounter.
data BlaFoo a
    = Foo
    | Bar Int a
    | Baz a a a
    deriving (Eq, Show)

$(deriveLargeHashable ''BlaFoo)

instance Arbitrary a => Arbitrary (BlaFoo a) where
    arbitrary = oneof [ pure Foo
                      , Bar <$> arbitrary <*> arbitrary
                      , Baz <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

-- | Simple property that tries to find hash collisions.
prop_thDerivedHashUnique :: BlaFoo Char -> BlaFoo Char -> Bool
prop_thDerivedHashUnique x y = (x == y) == (largeHash md5HashAlgorithm x == largeHash md5HashAlgorithm y)
    
newtype Fool = Fool { unFool :: Bool }

$(deriveLargeHashable ''Fool)

-- | Simple sanity check for TH derived instances for newtypes.
test_newtypeTHHashSane :: IO ()
test_newtypeTHHashSane = assertNotEqual (largeHash md5HashAlgorithm (Fool True))
                                        (largeHash md5HashAlgorithm (Fool False))
