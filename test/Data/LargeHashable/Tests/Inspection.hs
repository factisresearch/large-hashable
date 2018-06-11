{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}
module Data.LargeHashable.Tests.Inspection where

import Test.Framework hiding ((===), Failure, Success)
import Test.Inspection

import Data.LargeHashable
import Data.LargeHashable.Tests.Helper
import Data.LargeHashable.Class
import GHC.Generics

genericUpdateHashTestA :: TestA -> LH ()
genericUpdateHashTestA = genericUpdateHash

test_genericProductGetsOptimized :: IO ()
test_genericProductGetsOptimized =
    case $(inspectTest (hasNoGenerics 'genericUpdateHashTestA)) of
      Success _ -> return ()
      Failure e -> assertFailure e

data SumTest
    = A Int
    | B Char
    | C Integer
    | D (Either Int Char)
    deriving (Generic)

genericUpdateHashSum :: SumTest -> LH ()
genericUpdateHashSum = genericUpdateHash

test_genericSumGetsOptimized :: IO ()
test_genericSumGetsOptimized =
    case $(inspectTest (hasNoGenerics 'genericUpdateHashSum)) of
      Success _ -> return ()
      Failure e -> assertFailure e

data SopTest
    = A2 Char Int Bool
    | B2 SopTest Int Bool SopTest
    | C2 SopTest SopTest
    | D2 (Either Int Char) SopTest
    deriving (Generic)

genericUpdateHashSop :: SumTest -> LH ()
genericUpdateHashSop = genericUpdateHash

test_genericSumOfProductsGetsOptimized :: IO ()
test_genericSumOfProductsGetsOptimized =
    case $(inspectTest (hasNoGenerics 'genericUpdateHashSop)) of
      Success _ -> return ()
      Failure e -> assertFailure e
