{-# LANGUAGE  OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Data.LargeHashable.Tests.Stable where

import Data.LargeHashable
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.Framework

test_strictTextBackwardsCompat :: IO ()
test_strictTextBackwardsCompat = do
    -- we check that largeHashStable returns the same hash as for text version 1.*
    assertEqual (read "cc1ea58b3c2ec61d19c4ce222bc9e55d")
        (largeHashStable md5HashAlgorithm (T.pack "Stefan"))

test_lazyTextBackwardsCompat :: IO ()
test_lazyTextBackwardsCompat = do
    -- we check that largeHashStable returns the same hash as for text version 1.*
    assertEqual (read "cc1ea58b3c2ec61d19c4ce222bc9e55d")
        (largeHashStable md5HashAlgorithm (TL.pack "Stefan"))

test_lazyTextStableChunking :: IO ()
test_lazyTextStableChunking = do
    assertEqual (largeHashStable md5HashAlgorithm (TL.fromChunks ["foo", "ba", "r"]))
                (largeHashStable md5HashAlgorithm (TL.fromChunks ["foob", "ar"]))
