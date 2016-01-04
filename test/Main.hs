module Main where

import Test.Tasty
import Data.LargeHashable.Tests.Class

allTests :: TestTree
allTests = testGroup "All Tests" [ appendProperties ]

main :: IO ()
main = defaultMain allTests
