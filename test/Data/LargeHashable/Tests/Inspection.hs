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

$(deriveLargeHashable ''SumTest)

genericUpdateHashSum :: SumTest -> LH ()
genericUpdateHashSum = genericUpdateHash

thUpdateHashSum :: SumTest -> LH ()
thUpdateHashSum = updateHash

test_genericSumGetsOptimized :: IO ()
test_genericSumGetsOptimized =
    case $(inspectTest (hasNoGenerics 'genericUpdateHashSum)) of
      Success _ -> return ()
      Failure e -> assertFailure e

test_genericSumEqTH :: IO ()
test_genericSumEqTH =
    unitTestPending' "This currently doesn't hold" $
    case $(inspectTest ('genericUpdateHashSum === 'thUpdateHashSum)) of
      Success _ -> return ()
      Failure e -> assertFailure e

data SopTest
    = A2 Char Int Bool
    | B2 Int Bool
    | C2 (Char, Int) (Maybe Char)
    | D2 (Either Int Char) (Char, Int, Integer)
    deriving (Generic)

$(deriveLargeHashable ''SopTest)

genericUpdateHashSop :: SopTest -> LH ()
genericUpdateHashSop = genericUpdateHash

thUpdateHashSop :: SopTest -> LH ()
thUpdateHashSop = updateHash

test_genericSumOfProductsGetsOptimized :: IO ()
test_genericSumOfProductsGetsOptimized =
    unitTestPending' "This currently doesn't hold" $
    case $(inspectTest (hasNoGenerics 'genericUpdateHashSop)) of
      Success _ -> return ()
      Failure e -> assertFailure e

test_genericSopEqTH :: IO ()
test_genericSopEqTH =
    unitTestPending' "This currently doesn't hold" $
    case $(inspectTest ('genericUpdateHashSop === 'thUpdateHashSop)) of
      Success _ -> return ()
      Failure e -> assertFailure e

data UnitTest = UnitTest deriving (Generic)

$(deriveLargeHashable ''UnitTest)

genericUpdateHashUnitType :: UnitTest -> LH ()
genericUpdateHashUnitType = genericUpdateHash

thUpdateHashUnitType :: UnitTest -> LH ()
thUpdateHashUnitType = updateHash

unitTypeReturn :: UnitTest -> LH ()
unitTypeReturn UnitTest = return ()

test_genericUnitHashIsNoop :: IO ()
test_genericUnitHashIsNoop =
    case $(inspectTest ('genericUpdateHashUnitType === 'unitTypeReturn)) of
      Success _ -> return ()
      Failure e -> assertFailure e

test_thUnitHashIsNoop :: IO ()
test_thUnitHashIsNoop =
    case $(inspectTest ('thUpdateHashUnitType === 'unitTypeReturn)) of
      Success _ -> return ()
      Failure e -> assertFailure e

updateHashHaskellUnit :: () -> LH ()
updateHashHaskellUnit () =
    -- I have to do this twice for inlining to work
    do updateHash ()
       updateHash ()

haskellUnitReturn :: () -> LH ()
haskellUnitReturn () = return ()

test_haskellUnitHashIsNoop :: IO ()
test_haskellUnitHashIsNoop =
    case $(inspectTest ('updateHashHaskellUnit === 'haskellUnitReturn)) of
      Success _ -> return ()
      Failure e -> assertFailure e
