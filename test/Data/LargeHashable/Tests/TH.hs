{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -ddump-splices #-}
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

data HigherKinded t = HigherKinded (t String)

instance LargeHashable' BlaFoo where
    updateHash' = updateHash

instance LargeHashable' t => LargeHashable (HigherKinded t) where
    updateHash (HigherKinded x) =
        updateHash' x

test_higherKinded :: IO ()
test_higherKinded =
    assertNotEqual
        (largeHash md5HashAlgorithm (HigherKinded (Bar 42 "Stefan")))
        (largeHash md5HashAlgorithm (HigherKinded (Bar 5 "Stefan")))

data GadtNoArgs where
    GadtNoArgsA :: Int -> Char -> GadtNoArgs
    GadtNoArgsB :: Integer -> GadtNoArgs

$(deriveLargeHashable ''GadtNoArgs)

test_gadtNoArgs :: IO ()
test_gadtNoArgs =
    assertNotEqual
        (largeHash md5HashAlgorithm (GadtNoArgsA 1 'a'))
        (largeHash md5HashAlgorithm (GadtNoArgsB 1))

data GadtOneArg a where
    GadtOneArgA :: Int -> Char -> GadtOneArg Integer
    GadtOneArgB :: Integer -> GadtOneArg Char

$(deriveLargeHashable ''GadtOneArg)

test_gadtOneArg :: IO ()
test_gadtOneArg =
    assertNotEqual
        (largeHash md5HashAlgorithm (GadtOneArgA 1 'a'))
        (largeHash md5HashAlgorithm (GadtOneArgB 1))


data GadtMultipleArgs a b c where
    GadtMultipleArgsA :: a -> b -> GadtMultipleArgs a b Int
    GadtMultipleArgsB :: Int -> GadtMultipleArgs Char String Integer

$(deriveLargeHashable ''GadtMultipleArgs)

test_gadtMultipleArgs :: IO ()
test_gadtMultipleArgs =
    assertNotEqual
        (largeHash md5HashAlgorithm (GadtMultipleArgsA (1::Int) 'a'))
        (largeHash md5HashAlgorithm (GadtMultipleArgsB (1::Int)))
