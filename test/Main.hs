{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework

-- In Emacs sort block with M-x sort-lines
import {-@ HTF_TESTS @-} Data.LargeHashable.Tests.Class
import {-@ HTF_TESTS @-} Data.LargeHashable.Tests.TH

allTests :: [TestSuite]
allTests = htf_importedTests

main :: IO ()
main = htfMain allTests
