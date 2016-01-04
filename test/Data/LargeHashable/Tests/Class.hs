module Data.LargeHashable.Tests.Class where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.LargeHashable
import Data.LargeHashable.MD5
import Data.LargeHashable.Tests.Helper ()
import qualified Data.Text as T

appendProperties :: TestTree
appendProperties = testGroup "Append Tests"
    [ QC.testProperty "Appending Text is okay" prop_appendTextOk
    , QC.testProperty "Appending Lists is okay" prop_appendListOk
    ]

prop_appendTextOk :: T.Text -> T.Text -> Bool
prop_appendTextOk t1 t2 =
    runMD5 (updateHash (t1 `T.append` t2)) /=
    runMD5 (updateHash t1 >> updateHash t2)

prop_appendListOk :: [Int] -> [Int] -> Bool
prop_appendListOk l1 l2 =
    runMD5 (updateHash (l1 ++ l2)) /=
    runMD5 (updateHash l1 >> updateHash l2)
