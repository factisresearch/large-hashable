-- | This is the top-level module of LargeHashable, a library
--   for efficiently hashing any Haskell data type using a
--   hash algorithm like MD5, SHA256 etc.
--
--   Normal users shoud import this module.
module Data.LargeHashable (
   LargeHashable(..)
 , LargeHashable'(..)
 , LH
 , HashAlgorithm
 , largeHash
 , deriveLargeHashable
 , deriveLargeHashableNoCtx
 , deriveLargeHashableCtx
 , deriveLargeHashableCustomCtx
 , MD5Hash(..)
 , md5HashAlgorithm
 , runMD5
 , module Data.LargeHashable.LargeWord
) where

import Data.LargeHashable.Class
import Data.LargeHashable.Intern
import Data.LargeHashable.LargeWord
import Data.LargeHashable.MD5
import Data.LargeHashable.TH
