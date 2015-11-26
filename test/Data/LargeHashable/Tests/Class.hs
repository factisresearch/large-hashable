{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Data.LargeHashable.Tests.Class (
    htf_thisModulesTests
) where

import Test.Framework

import Data.LargeHashable
import Data.LargeHashable.MD5
import Data.LargeHashable.Tests.Helper ()
import qualified Data.Text as T

prop_appendTextOk :: T.Text -> T.Text -> Bool
prop_appendTextOk t1 t2 =
    runMD5 (updateHash (t1 `T.append` t2)) /=
    runMD5 (updateHash t1 >> updateHash t2)

prop_appendListOk :: [Int] -> [Int] -> Bool
prop_appendListOk l1 l2 =
    runMD5 (updateHash (l1 ++ l2)) /=
    runMD5 (updateHash l1 >> updateHash l2)
