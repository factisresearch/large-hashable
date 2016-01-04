module Data.LargeHashable.Tests.Helper where

import Control.Monad
import Test.QuickCheck
import qualified Data.Text as T

instance Arbitrary T.Text where
    arbitrary = liftM T.pack arbitrary
    shrink t =
        map T.pack (shrink (T.unpack t))
