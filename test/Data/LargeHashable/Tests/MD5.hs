{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Data.LargeHashable.Tests.MD5 where

import Data.LargeHashable.LargeWord
import Data.LargeHashable.MD5
import Data.LargeHashable.Tests.Helper ()
import Test.Framework

prop_readShowMD5Hash :: Word128 -> Bool
prop_readShowMD5Hash w =
    let h = MD5Hash w
    in h == read (show h)
